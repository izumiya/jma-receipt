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
      *
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGI01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院業務
      *  コンポーネント名  : 入退院登録（Ｉ０１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02.08.09    ひさなが 　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    02.12.10  収納明細削除処理追加
      *  01.00.02    NACL-太田    02.12.26  入院科の初期表示を修正
      *  01.00.03    NACL-太田    03.01.24  食事の集計方法修正
      *  01.00.04    NACL-太田    03.02.03  退院時診療行為チェック追加
      *  01.00.05    NACL-太田    03.02.05  自院歴作成データを考慮する
      *  01.00.06    NACL-太田    03.03.05  異動取消機能を追加
      *                                     入院歴の表示を修正
      *  01.00.07    NACL-太田    03.03.06  通算日数を関数より取得する
      *                                     よう修正
      *  01.00.08    NACL-太田    03.03.07  排他制御対応
      *  01.00.09    NACL-太田    03.03.12  選定療養費１８０日越えの
      *                                     日数算定方法修正
      *  01.00.10    NACL-太田    03.03.12  カルテ印刷機能追加
      *  01.00.11    NACL-太田    03.03.14  転科・転棟・転室時、入院料
      *                                     のみの変更も可とする
      *  01.00.12    NACL-太田    03.03.18  入院カスタマイズ帳票対応
      *  01.00.13    NACL-太田    03.04.02  負担金計算サブ引数修正
      *  01.00.14    NACL-太田    03.04.08  患者登録への遷移を追加
      *  01.00.15    NACL-太田    03.04.09  保険組合せの取得方法修正
      *  01.00.16    NACL-太田    03.05.08  入院歴表示の自院歴退院日の
      *                                     編集を修正（入院中対応）
      *  01.00.17    NACL-太田    03.05.16  入院カルテに保険を編集する
      *  01.00.18    NACL-太田    03.05.19  退院時診療行為チェック追加
      *  01.00.19    NACL-太田    03.06.05  入院歴の修正を日付項目以外は
      *                                     いつでも変更可とする
      *  01.00.20    NACL-小豆沢  03.06.23  診療所老人医療管理料の転室、
      *                                     退院取消、異動取消の正式対応
      *  01.00.21    NACL-太田    03.06.24  診療所老人医療管理料（３０日
      *                                     以内再入院）の考慮を追加
      *  01.00.22    NACL-小豆沢  03.06.24  診療所老人医療管理料（３０日
      *                                     以内再入院）の考慮を追加
      *  01.00.23    NACL-小豆沢  03.06.24  特定集中治療室管理料の転室対応
      *  01.00.24    NACL-太田    03.07.01  出力先プリンタサブ追加
      *  01.00.25    NACL-小豆沢  03.07.08  特定集中治療室管理料の退院取消対応
      *  01.00.26    NACL-小豆沢  03.07.09  特定集中治療室管理料の異動取消対応
      *  01.00.27    NACL-小豆沢  03.07.10  退院取消時に診療録管理体制加算を
      *                                     算定しない
      *  01.00.28    NACL-小豆沢  03.07.10  退院取消時の通算日数算定を修正
      *  01.00.29    NACL-太田    03.07.10  排他制御処理修正
      *  01.00.30    NACL-小豆沢  03.07.10  異動取消時に診療会計を削除しないよう
      *                                     変更
      *  01.00.31    NACL-小豆沢  03.07.11  診療録管理体制加算の特別チェック削除
      *  01.00.32    NACL-小豆沢  03.09.24  特別入院基本料算定時の乳幼児加算対応
      *  01.00.33    NACL-太田    03.09.24  小児入院医療管理料での入院時に年齢
      *                                     をチェックし１５才以上であれば特定入
      *                                     院料コンボに表示をしない
      *  01.00.34    NACL-小豆沢  03.09.24  小児入院医療管理料３の算定対応追加
      *  01.00.35    NACL-多々納  03.09.30  業務権限判定追加
      *  01.00.36    NACL-小豆沢  03.10.03  一類感染症患者入院医療管理の算定対応
      *                                     追加
      *  01.00.37    NACL-小豆沢  03.10.03  老人性痴呆疾患治療病棟入院料の算定
      *                                     対応追加
      *  01.00.38    NACL-太田    03.10.09  入院日変更時に室料差額を初期化しない
      *  01.00.39    NACL-小豆沢  03.10.24  労災（自賠）入院時に老人点数無効
      *  01.00.40    NACL-小豆沢  03.10.27  労災（自賠）入院時に選定入院なし
      *  01.00.41    NACL-太田    03.11.14  更新日付編集対応
      *  01.00.42    NACL-小豆沢  03.11.26  老人患者の判定条件追加
      *  01.00.43    NACL-小豆沢  03.12.01  広範囲熱傷特定集中治療室管理料の
      *                                     算定対応追加（算定上限日数経過後の
      *                                     一般入院料算定はコメントとする）
      *  01.00.44    NACL-太田    04.01.28  病室変更時に室料差額表示が
      *                                     変わらない不具合を修正
      *  01.00.45    NACL-小豆沢  04.03.10  老人性痴呆疾患治療病棟入院料２の
      *                                     算定対応追加
      *  01.00.46    NACL-小豆沢  04.03.11  ハイケアユニット入院医療管理料の
      *                                     算定対応追加
      *  01.00.47    NACL-太田    04.03.11  転科・転棟・転室時の項目変更
      *                                     チェックを削除
      *  01.00.48    NACL-太田    04.03.12  算定入院料の表示を追加
      *  01.00.49    NACL-小豆沢  04.03.15  広範囲熱傷特定集中治療室管理料の
      *                                     算定対応再追加
      *  01.00.50    NACL-小豆沢  04.03.15  亜急性期入院医療管理料の算定対応追加
      *  01.00.51    NACL-小豆沢  04.03.16  １５歳未満の患者は選定入院対象外
      *  01.00.52    NACL-太田    04.08.02  定期請求中は退院登録は不可とする
      *  01.00.53    NACL-太田    04.10.14  退院事由追加
      *  01.00.54    NACL-小豆沢  05.07.19  自費１０項目対応
      *  01.00.55    NACL-小豆沢  06.01.13  退院日以降の診療会計を残す処理
      *                                     を可能とする区分を追加
      *  02.07.00    NACL-太田    06.02.15  入院オーダー確認画面遷移制御追加
      *平成１８年４月改定対応
      *  02.08.01    NACL-小豆沢  06/03/16  入院料老人コード置き換えは療養病棟
      *                                     または療養病床のみを対象とする
      *              NACL-小豆沢  06/03/17  特定入院料対応
      *                                     脳卒中ケアユニット入院医療管理料
      *                                     精神科救急入院料
      *                                     精神科急性期治療病棟入院料１
      *                                     精神科急性期治療病棟入院料２
      *                                     看護補助加算の変更
      *                                     異動処理時の食事退避処理変更
      *  02.09.01    NACL-小豆沢  06/05/16  老人特定入院基本料を特定入院料
      *                                     より外す
      *平成１８年７月改定対応
      *  03.00.01    NACL-小豆沢  06/06/19  療養病棟、療養病床改定対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *
      *    画面非表示
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    入退院登録共通領域
           COPY    "I0COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "I0SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-ERR                     PIC 9(01).
           03  FLG-DBRC                    PIC 9(01).
           03  FLG-ENTRYCHK                PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-HKNCOMBICHK             PIC 9(01).
           03  FLG-SYORIPTN                PIC 9(01).
           03  FLG-TAISYO                  PIC 9(01).
           03  FLG-UKETUKEGAITO            PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-ORCSDAY                 PIC 9(01).
           03  FLG-JYUHKNCOMBISET          PIC 9(01).
           03  FLG-NYUKSN                  PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-TENKA-SRYKA             PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-ZI-SAISIN               PIC 9(01).
           03  FLG-SYUNOU                  PIC 9(01).
           03  FLG-SYUMEI                  PIC 9(01).
           03  FLG-PTTEIKIRRK              PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-SRYACCT                 PIC 9(01).
           03  FLG-SYUARI                  PIC 9(01).
           03  FLG-RJN                     PIC 9(01).
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-NYUINACCTARI            PIC 9(01).
           03  FLG-WKPTTEIKIRRK            PIC 9(01).
           03  FLG-HKNCOMBIGET             PIC 9(01).
           03  FLG-RRKCHK                  PIC 9(01).
           03  FLG-CMB-EDIT                PIC 9(01).
           03  FLG-NYUREKILST-ROW          PIC 9(01).
           03  FLG-SHONUM                  PIC 9(01).
           03  FLG-TAIINRRK                PIC 9(01).
           03  FLG-TEIKI-CHK               PIC 9(01).
           03  FLG-TAINDOUJITU             PIC 9(01).
           03  FLG-TAINTUSANMINUS          PIC 9(01).
           03  FLG-TARGET                  PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX3                        PIC 9(05).
           03  IDX4                        PIC 9(05).
           03  IDX5                        PIC 9(05).
           03  IDX6                        PIC 9(05).
           03  IDX7                        PIC 9(05).
           03  IDX8                        PIC 9(05).
           03  IDX9                        PIC 9(05).
           03  IDX10                       PIC 9(05).
           03  IDX11                       PIC 9(05).
           03  IDX12                       PIC 9(05).
           03  IDX13                       PIC 9(05).
           03  IDX14                       PIC 9(05).
           03  IDX15                       PIC 9(05).
           03  IDX16                       PIC 9(05).
           03  IDX-CMB                     PIC 9(05).
           03  IDX-DAY                     PIC 9(02).
           03  IDX-SKJ-TAIHI               PIC 9(02).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-TUKISU          PIC 9(05).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD     PIC X(08).
           03  SYS-TIME    PIC X(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-I0IDCD                  PIC X(04).
           03  WRK-I0ID-FLG                PIC X(02).
           03  WRK-I0IDMSG                 PIC X(80). 
           03  WRK-MOTOPG                  PIC X(08).
           03  WRK-YMD                     PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY                 PIC 9(04).
               05  WRK-SMM                 PIC 9(02).
               05  WRK-SDD                 PIC 9(02).
           03  WRK-HENYMDG                 PIC X(09).
           03  WRK-KANACHK-MAE-INPUT       PIC X(400).
           03  WRK-KANACHK-OUT-INPUT       PIC X(400).
      *    パス名領域
           03  WRK-DBPATH                  PIC X(20).
           03  WRK-CALC-YMD1.
               05  WRK-CALC-YY1            PIC 9(04).
               05  WRK-CALC-MM1            PIC 9(02).
               05  WRK-CALC-DD1            PIC 9(02).
      *
           03  WRK-CALC-YMD2.
               05  WRK-CALC-YY2            PIC 9(04).
               05  WRK-CALC-MM2            PIC 9(02).
               05  WRK-CALC-DD2            PIC 9(02).
           03  WRK-AGE-YY                  PIC S9(04).
           03  WRK-AGE-MM                  PIC S9(02).
           03  WRK-AGE-G.
               05  WRK-AGE-Z               PIC ---9.
               05  WRK-AGE-MEI             PIC X(04).
           03  WRK-BIRTHDAY                PIC X(08).
           03  WRK-COMBO.
               05  WRK-CMB-CD              PIC X(05).
               05  WRK-CMB-IDX             PIC 9(03).
               05  WRK-CMB-ITM             PIC X(100).
               05  WRK-CMB-ITM2            PIC X(100).
               05  WRK-CMB-ITM3            PIC X(100).
               05  WRK-CMB-ITM4            PIC X(100).
           03  WRK-CNT                     PIC 9(05).
           03  WRK-NUM-EDIT.
               05  WRK-99                  PIC 9(02).
               05  WRK-99999               PIC 9(05).
               05  WRK-999999              PIC 9(06).
               05  WRK-ZZ9                 PIC ZZ9.
               05  WRK-ZZZ9                PIC ZZZ9.
               05  WRK-ZZZZ9               PIC ZZZZ9.
               05  WRK-ZZZZZ9              PIC ZZZZZ9.
           03  WRK-SAGAKU.
               05  WRK-SAGAKU-NUM          PIC ZZZZ9.
               05  WRK-SAGAKU-EN           PIC X(02).
           03  WRK-STR1                    PIC X(20).
           03  WRK-STR2                    PIC X(20).
           03  WRK-IDX1                    PIC 9(05).
           03  WRK-IDX2                    PIC 9(05).
           03  WRK-BTUJYOHO-TBL.
               05  WRK-BTUJYOHO-CNT        PIC 9(05).
               05  WRK-BTUJYOHO-OCC        OCCURS  50
                                           INDEXED BY  WRK-BTUNUM-IDX.
                   07  WRK-BTUJY-BTUNUM    PIC X(02).
                   07  WRK-BTUJY-BRM-SAGAKU
                                           PIC X(02).
                   07  WRK-BTUJY-BRM-KA    PIC X(02).
           03  WRK-BRM-SAGAKU-CD           PIC X(02).
           03  WRK-BTU-KA-CD               PIC X(02).
           03  WRK-BRM-KA-CD               PIC X(02).
           03  WRK-HKNCOMBI                PIC X(100).
           03  WRK-FTNRATE                 PIC X(05).
           03  WRK-HKNKBN-MEI              PIC X(02).
           03  WRK-CREYMD                  PIC X(08).
           03  WRK-JTIKBN                  PIC X(01).
           03  WRK-RRKNUM                  PIC 9(03).
           03  WRK-RRKEDANUM               PIC 9(03).
           03  WRK-KJNYMD                  PIC X(08).
           03  WRK-KIHON-KJNYMD            PIC X(08).
           03  WRK-UPDKBN                  PIC X(3).
           03  WRK-TENNYUYMD               PIC X(08).
           03  WRK-TENSTUYMD               PIC X(08).
           03  WRK-BRMNUM                  PIC X(06).
           03  WRK-BRMNUM-IN               PIC X(06).
           03  WRK-BRMNUM-EDT              PIC X(06).
           03  WRK-BRMNUM-OT               PIC X(06).
           03  WRK-NYUINKA-KEY             PIC X(02).
           03  WRK-BTUNUM-KEY              PIC X(02).
           03  WRK-BRMNUM-KEY              PIC X(08).
           03  WRK-HKNCOMBI-KEY            PIC X(04).
           03  WRK-TOKUTEI-KEY.
               05  WRK-TOKUTEI-KBN-KEY     PIC X(01).
               05  WRK-TOKUTEI-NYUIN-KEY   PIC X(02).
      *
           03  WRK-REKI-BYOMEI-EDIT-AREA.
               05  WRK-REKI-BYOMEI-EDIT-OCC
                                           OCCURS 100
                                           INDEXED BY  WRK-RRKBYO-IDX.
                   07  WRK-REKI-BYO-ZITA-FLG   PIC 9(01).
                   07  WRK-REKI-BYO-NYUINKA    PIC X(02).
                   07  WRK-REKI-BYO-NYUINYMD   PIC X(08).
                   07  WRK-REKI-BYO-TAIINYMD   PIC X(08).
                   07  WRK-REKI-BYO-RENNUM     PIC 9(02).
                   07  WRK-REKI-BYO-SRYYMD     PIC X(08).
                   07  WRK-REKI-BYO-BYOMEI     PIC X(40).
                   07  WRK-REKI-BYO-TENKIKBN   PIC X(01).
           03  WRK-WIDGET-B07-STT              PIC S9(9)   BINARY.
           03  WRK-WIDGET-B08-STT              PIC S9(9)   BINARY.
           03  WRK-WIDGET-STT                  PIC S9(9)   BINARY.
           03  WRK-STYMD                       PIC X(08).
           03  WRK-EDYMD                       PIC X(08).
           03  WRK-NISSU                       PIC 9(05).
           03  WRK-NISSU1                      PIC 9(05).
           03  WRK-NISSU2                      PIC 9(05).
           03  WRK-TSUSAN                      PIC 9(05).
           03  WRK-NYUINNISSU                  PIC 9(05).
           03  WRK-SANTEISTYMD                 PIC X(08).
           03  WRK-SENTEI-STYMD                PIC X(08).
           03  WRK-BIRTHDAY-15                 PIC X(08).
           03  WRK-BIRTHDAY-15R   REDEFINES    WRK-BIRTHDAY-15.
               05  WRK-BIRTHDAY-15Y            PIC 9(04).
               05  WRK-BIRTHDAY-15MD           PIC 9(04).
           03  WRK-REKI-ZIIN-TBL.
               05  WRK-REKI-ZIIN-CNT           PIC 9(05).
               05  WRK-REKI-ZIIN-OCC           OCCURS  100.
                   07  WRK-REKI-ZIIN-NYUINYMD  PIC X(08).
                   07  WRK-REKI-ZIIN-TAIINYMD  PIC X(08).
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-KEISANYMD               PIC X(08).
           03  WRK-HKNCOMBINUM             PIC X(04).
           03  WRK-HKNCOMBINUM9   REDEFINES  WRK-HKNCOMBINUM
                                           PIC 9(04).
           03  WRK-SHONUM                  PIC 9(03).
           03  WRK-SRYKAMEI                PIC X(30).
           03  WRK-TENKA-SRYKA-TBL.
               05  WRK-TENKA-SRYKA-CNT     PIC 9(05).
               05  WRK-TENKA-SRYKA         PIC X(02)
                                           OCCURS 50.
           03  WRK-SAGAKU-KBN              PIC X(02).
           03  WRK-SAGAKU-KBN9  REDEFINES  WRK-SAGAKU-KBN
                                           PIC 9(02).
           03  WRK-KANRICD                 PIC X(04).
           03  WRK-KBNCD                   PIC X(08).
           03  WRK-TOKNYUIN-NM             PIC X(60).
           03  WRK-TOKNYUIN-KBN            PIC X(01).
           03  WRK-TOKNYUIN-CD             PIC X(02).
           03  WRK-TAIHI-SRYKA             PIC X(02).
           03  WRK-NOW-SKYSTYMD            PIC X(08).
           03  WRK-SKYSTYM                 PIC X(06).
           03  WRK-SYU-SUM-IDX             PIC 9(05).
           03  WRK-TAIINUPDCNT             PIC 9(05).
      *
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-IDOYMD                  PIC X(08).
           03  WRK-NYUINYMD                PIC X(08).
           03  WRK-MAE-SKYEDYMD            PIC X(08).
           03  WRK-SKYSTYMD                PIC X(08).
           03  WRK-SKYEDYMD                PIC X(08).
           03  WRK-SKYMONEY                PIC  S9(07).
      *    医療機関設定の入院料加算チェック用
           03  WRK-NYUKHNKSN-SRYCD         PIC X(09)  OCCURS 20.
           03  WRK-NYUKSNKBN               PIC X(03).
           03  WRK-BTU-NYUKSNKBN           PIC X(03).
           03  WRK-KSN-NYUKSNKBN           PIC X(03).
           03  WRK-STYUKYMD                PIC X(08).
           03  WRK-TAIINYMD.
               05  WRK-TAIINYM             PIC X(06).
               05  WRK-TAIINDD             PIC 9(02).
           03  WRK-MCP-PUTTYPE             PIC X(08).
           03  WRK-ORCHCN01-SYORIKBN       PIC X(01).
           03  WRK-ORCHCN01-PRTKBN         PIC X(01).
           03  WRK-AGECHK-SRYYMD           PIC X(08).
           03  WRK-NACCT-SRYYM             PIC X(08).
           03  WRK-PRINT-PG                PIC X(20).
           03  WRK-ORCSHKN-HKNCOMBINUM     PIC X(04).
           03  WRK-TOKUBETSU-NYUIN         PIC X(01).
      *    プレイルーム、保育士加算加算退避領域
           03  WRK-BTU-HIKSISUKBN          PIC X(01).
           03  WRK-BTU-KNGTEK-KBN          PIC X(01).
           03  WRK-BTU-DRTEK-KBN           PIC X(01).
      *    会計作成用退避領域
           03  WRK-TAIHI-HKN               PIC 9(03).
           03  WRK-TAIHI-SHOKUJI           PIC 9(03).
           03  IDX-SHOKUJIX                PIC X(02).
           03  IDX-SHOKUJI9   REDEFINES    IDX-SHOKUJIX
                                           PIC 9(02).
           03  IDX-SKJ                     PIC 9(02).
           03  WRK-NYUINCHU-TAIINYMD       PIC X(08).
           03  WRK-ORCHCN01-HKNCOMBINUM    PIC 9(04).
           03  WRK-STIDX                   PIC 9(02).
           03  WRK-EDIDX                   PIC 9(02).
           03  WRK-MIN-YMD.
               05  WRK-MIN-YM              PIC X(06).
               05  WRK-MIN-DD              PIC 9(02).
      *    特定入院料の会計作成後に一般の入院料で会計を作成するための
      *    領域
           03  IPN-NYUKAIKEI-SAKUSEI-FLG   PIC X(01).
           03  IPN-SANTEISTYMD             PIC X(08).
           03  WRK-IPN-HANTEI-YMD1.
               05  WRK-IPN-HANTEI-YM1.
                   07  WRK-IPN-HANTEI-Y1   PIC 9(04).
                   07  WRK-IPN-HANTEI-M1   PIC 9(02).
               05  WRK-IPN-HANTEI-D1       PIC 9(02).
           03  WRK-IPN-HANTEI-YMD2         PIC X(08).
      *    特定入院料の会計作成用に診療コードを退避する領域
           03  WRK-TOKUTEI-SRYCD1          PIC X(09).
           03  WRK-TOKUTEI-SRYCD2          PIC X(09).
           03  WRK-SRYCD                   PIC X(09).
           03  WRK-SHOKAI-NYUINYMD-W       PIC X(10).
           03  WRK-CHK-SHONUM              PIC 9(03).
           03  WRK-I01-I04-PROCKBN         PIC X(01).
           03  WRK-FTN-NYUINYMD            PIC X(08).
           03  WRK-FTN-TAIINYMD            PIC X(08).
           03  WRK-FTN-DJKBN               PIC X(01).
           03  WRK-FTN-RRKNUM              PIC 9(03).
           03  WRK-NYUREKILST-SEL          PIC 9(05).
      *    転科／転棟／転室時に食事会計を退避する項目（最大２カ月分）
           03  WRK-TENROOM-SKJ-SRYYMX.
               05  WRK-TENROOM-SKJ-SRYYY9  PIC 9(04).
               05  WRK-TENROOM-SKJ-SRYMM9  PIC 9(02).
           03  WRK-TENROOM-SKJ-TABLE.
               05  WRK-TENROOM-SKJ-TBL     OCCURS  2.
                   07  WRK-TENROOM-SKJ-SRYYM
                                           PIC X(06).
                   07  WRK-TENROOM-SKJ-DD  PIC 9(02).
                   07  WRK-TENROOM-SKJ     PIC 9(03)  OCCURS  31.
                   07  WRK-TENROOM-SKJ18   PIC 9(03)  OCCURS  31.
                   07  WRK-TENROOM-SKJ19   PIC 9(03)  OCCURS  31.
                   07  WRK-TENROOM-SKJ20   PIC 9(03)  OCCURS  31.
           03  WRK-NAI-NYUINYMD.
               05  WRK-NAI-NYUINYM         PIC X(06).
               05  WRK-NAI-NYUINDD         PIC 9(02).
           03  WRK-SYUKA-G.
               05  WRK-SYUKA-KBN           PIC X(02).
               05  WRK-SYUKA-YMD           PIC X(08).
           03  WRK-CHK-YMD                 PIC X(08).
      *
      *    患者年齢算出用エリア
       01  WRK-NENREI-AREA.
           03  WRK-NENREI                  PIC 9(03).
           03  WRK-YMDS1.
               05  WRK-YYS1                PIC 9(04).
               05  WRK-MMS1                PIC 9(02).
               05  WRK-DDS1                PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2                PIC 9(04).
               05  WRK-MMS2                PIC 9(02).
               05  WRK-DDS2                PIC 9(02).
      *
      *    診療所老人医療管理料（転科、転棟）計算エリア
       01  TN-IDX                          PIC 9(02).
       01  WK-TOKUTEI-DAY                  PIC 9(04).
       01  WK-TOKUTEI-DAY2                 PIC 9(04).
      *
       01  WRK-ROW.
           03  WRK-NYUREKILST-ROW          PIC S9(9)   BINARY.
      *
       01  CONST-AREA.
           03  CONST-H180401               PIC X(08)   VALUE "20060401".
           03  CONST-H180701               PIC X(08)   VALUE "20060701".
           03  CONST-NYUREKILST-MAX        PIC 9(05)   VALUE   100.
           03  CONST-TOKNYUIN-MAX          PIC 9(05)   VALUE   30.
           03  CONST-JOB-SHELLID           PIC X(08)   VALUE   "TEIKI1".
           03  CONST-JOB-JOBID             PIC 9(07)   VALUE   1.
      *
      *    スパ領域 ワーク
           COPY    "COMMON-SPA"      REPLACING
                               //SPA-// BY //WRK-SPA-//.
      *
      *    入院履歴ワーク
       01  WKPTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING
                               //PTNYUINRRK-// BY //WKPTNYUINRRK-//.
      *
      *    収納ワーク
       01  SUMSYUNOU-REC.
         02 SUMSYUNOU-CNT     PIC 9(05).
         02 SUMSYUNOU-OCC     OCCURS 13.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-// BY //SUMSYU-//.
         02 SUMSYUDAY-OCC     OCCURS 13.
           COPY  "CPSYUDAY.INC"    REPLACING  //SDAY-//
                                                       BY //SUMSDAY-//.
      *    収納合計ワーク
       01  TTLSYUNOU-REC.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-// BY //TTLSYU-//.
      *
      *    退院日の同日再入院用ワーク領域
       01  WRK-DOU-AREA.
           03  WRK-DOU-NAI-I01-NYUINYMD      PIC X(08).
           03  WRK-DOU-PTNYUINRRK-TENNYUYMD  PIC X(08).
           03  WRK-DOU-TENNYUYMD             PIC X(08).
           03  FLG-JIINRRK                   PIC 9(01).
       01  WRK-PTNYUINRRK-REC.
           COPY  "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                            BY  //WRK-PTNYUINRRK-//.
       01  LASTRRK-REC.
           COPY  "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                            BY  //LASTRRK-//.
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    （診療科入院科情報）
           COPY    "CPSK1005.INC".
      *    （職員情報）
           COPY    "CPSK1010.INC".
      *
      *    帳票プログラム名
           COPY    "CPSK1035.INC".
      *    （医療機関情報）
           COPY    "CPSK5000.INC".
      *    （病棟情報）
           COPY    "CPSK5001.INC".
      *    （病室情報）
           COPY    "CPSK5002.INC".
      *    （室料差額）
           COPY    "CPSK5113.INC".
      *    （特定入院料−病棟）
           COPY    "CPSK5110.INC".
      *    （特定入院料−病室）
           COPY    "CPSK5111.INC".
      *    
           COPY    "CPSK5013.INC".
      *    
           COPY    "CPSK9000.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    患者他院歴付加情報
       01  PTTAINFUKA-REC.
           COPY    "CPPTTAINFUKA.INC".
      *
      *    受付マスタ
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *
      *    収納マスター
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細マスター
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
       01  WKJYURRK-REC.
           COPY    "CPJYURRK.INC"      REPLACING
                                       //JYURRK-// BY //WKJYURRK-//.
      *    老人一般コード変換
        01  SRYCDCHG-REC.
            COPY    "CPSRYCDCHG.INC".
      *
      *    患者定期請求履歴
        01 PTTEIKIRRK-REC.
           COPY     "CPPTTEIKIRRK.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    入院料加算チェック
       01  NYUKSNCHK-REC.
           COPY    "CPNYUKSNCHK.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    選定療養費
       01  SENTEICDCHG-REC.
           COPY    "CPSENTEICDCHG.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    同日入院チェックサブ
           COPY    "CPORCSDNTCHK.INC".
      *
       01  LNKNRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-// 
                                       BY         //LNKNRRK-//.
      *
      *    入院登録サブ
           COPY    "CPI0SUB01.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    数値号変換サブ
           COPY    "CPORCSNUM.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    初期加算算定サブ
           COPY    "CPORCSSYOKIKSN.INC".
      *
      *    入院会計作成サブ
           COPY    "CPORCSNYUACCT.INC".
      *
      *    負担金計算サブ
       01  LNK-ORCS02-REC.
           COPY  "CPORCS02.INC".
       01  LNK-SYUNOU-REC-T.
         02 LNK-SYUNOU-REC     OCCURS 10.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-// BY //LNK-SYU-//.
       01  LNK-SYUDAY-REC-T.
         02 LNK-SYUDAY-REC     OCCURS 10.
           COPY  "CPSYUDAY.INC"  REPLACING  //SDAY-// BY //LNK-SDAY-//.
      *
      *    通算日数取得サブ
           COPY "CPORCSTUSAN.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    入院カルテ
           COPY    "CPORCHCN01.INC".
      *
      *    保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    保険組合せ編集サブ
           COPY    "CPORCSHKNSET.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    日報キー取得サブ
           COPY "CPORCSDAILYKEY.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    定期請求ジョブチェック
           COPY    "CPORCSTEIKIJOB.INC".
      *
      *    主科情報アクセス用パラメタ
       01  ORCSSYUACCAREA.
           COPY    "CPORCSSYUACC.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
      *
        LINKAGE                     SECTION.
      *
      *     テーブルアクセス
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "I01.INC".
           COPY    "I02.INC".
           COPY    "I03.INC".
           COPY    "I04.INC".
           COPY    "I05.INC".
           COPY    "I06.INC".
           COPY    "I0ERR.INC".
           COPY    "I0ID1.INC".
           COPY    "I0ID2.INC".
      *
      *
       PROCEDURE                   DIVISION    USING
                                               MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA.
      *****************************************************************
      *    主    処    理
      *****************************************************************
      *
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-I0FREE.
           MOVE    SPA-WORK        TO  SPA-I0KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD.
           MOVE    ZERO            TO  FLG-END.
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
           IF  FLG-END   =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF.
      *
           MOVE    SPA-I0KYOUTU    TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON.
           MOVE    SPA-I0FREE      TO  SPA-FREE
           .
      *
       000-PROC-SEC-EXT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                   SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "I0ERR" )
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF 
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
      *
               IF    ( SPA-I0IDCD  NOT =   SPACE )
                   PERFORM 520-I0IDSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
      *
               IF    ( SPA-I0IDCD2 NOT =   SPACE )
                   PERFORM 520-I0IDSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
      *
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "I01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *
               IF      WRK-MOTOPG(1:3)     =   "P97"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-I01
                   MOVE    SPA-WORK        TO  SPA-I0KYOUTU
                   MOVE    1               TO  SPA-GMN-I01-CUR
               END-IF
      *
               IF      WRK-MOTOPG(1:3)     =   "P97"
                   PERFORM 330-P97-SET-SEC
                   GO  TO  300-SCREEN-EXT
               END-IF
           END-IF
      *
           EVALUATE    SPA-MOTOPG
      *
      *    確認画面
           WHEN    "I0ID1"
               PERFORM 330-I0ID1-SET-SEC
      *
               GO  TO      300-SCREEN-EXT
      *
      *
      *    確認画面２
           WHEN    "I0ID2"
               PERFORM 330-I0ID2-SET-SEC
      *
               GO  TO      300-SCREEN-EXT
      *
      *    入院歴表示画面
           WHEN    "I02"
               PERFORM 330-I02-SET-SEC
               GO  TO      300-SCREEN-EXT
      *
      *    入院歴作成画面
           WHEN    "I03"
               PERFORM 330-I03-SET-SEC
               GO  TO      300-SCREEN-EXT
      *
      *    請求確認画面
           WHEN    "I04"
               IF    ( SPA-I01-I04-BTN =   "B12" )
      *            画面を更新後の内容で再表示
                   PERFORM 4101-PTNUM-CHK-SEC
               ELSE
                   IF    ( SPA-I01-I04-PROCKBN =   "1" )
      *                カーソルを退院日に設定
                       MOVE    11          TO  SPA-GMN-I01-CUR
                   ELSE
      *                カーソルを選択番号に設定
                       MOVE    100         TO  SPA-GMN-I01-CUR
                   END-IF
               END-IF
      *
               GO  TO      300-SCREEN-EXT
      *
      *    収納登録・入院会計照会画面
           WHEN    "S02"
           WHEN    "I41"
      *
      *        退避しておいたＳＰＡの内容を元に戻す
               INITIALIZE                  I0SUB01-LNK
               MOVE    SPA-TERMID      TO  I0SUB01-TERMID
               MOVE    "I"             TO  I0SUB01-KBN
               CALL    "ORCGI0SUB01"   USING   I0SUB01-LNK
      *
               MOVE    I0SUB01-SCRSPA-AREA
                                       TO  SPA-I0FREE
               MOVE    I0SUB01-COMMON  TO  SPA-I0KYOUTU
      *
               GO  TO  300-SCREEN-EXT
      *    患者登録画面
           WHEN    "P02"
      *        退避しておいたＳＰＡの内容を元に戻す
               INITIALIZE                  I0SUB01-LNK
               MOVE    SPA-TERMID      TO  I0SUB01-TERMID
               MOVE    "I"             TO  I0SUB01-KBN
               CALL    "ORCGI0SUB01"   USING   I0SUB01-LNK
      *
               MOVE    I0SUB01-SCRSPA-AREA
                                       TO  SPA-I0FREE
               MOVE    I0SUB01-COMMON  TO  SPA-I0KYOUTU
      *
               PERFORM 330-P02-SET-SEC
      *
               GO  TO  300-SCREEN-EXT
      *
      *    入院患者照会画面
           WHEN    "I20"
      *    診療行為画面
           WHEN    "K02N"
      *
               PERFORM 310-SPASET-SEC
               MOVE    SPA-MOTOPG      TO  SPA-I0-MOTOPG
      *
               MOVE    SPA-PTID        TO  SPA-NAI-I01-PTID
               MOVE    SPA-PTNUM       TO  SPA-GMN-I01-PTNUM
               PERFORM 4101-PTINF-SELECT-SEC
               IF    ( SPA-ERRCD   =   SPACE )
      *            照会として表示
                   MOVE    SPA-GMN-I01-B07-STT
                                   TO  WRK-WIDGET-B07-STT
                   MOVE    SPA-GMN-I01-B08-STT
                                   TO  WRK-WIDGET-B08-STT
                   MOVE    "04"        TO  WRK-CMB-CD
                   PERFORM 4103-CMB-SYORIKBN-SRH-SEC
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SYORIKBN
                   PERFORM 4201-WIDGET-INSENSITIVE-SEC
                   MOVE    WRK-WIDGET-B07-STT
                                   TO  SPA-GMN-I01-B07-STT
                   MOVE    WRK-WIDGET-B08-STT
                                   TO  SPA-GMN-I01-B08-STT
               END-IF
      *
               INITIALIZE                  SPA-ERRCD
                                           SPA-I0IDCD
                                           SPA-I0IDCD2
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
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
      *    入外区分
           MOVE    "1"                 TO  SPA-NYUGAIKBN
      *
      *
           INITIALIZE              I01
                                   SPA-I0FREE
                                   SPA-I0KYOUTU
      *
           INITIALIZE                      SYS-9000-REC
           MOVE    "9000"              TO  SYS-9000-KANRICD
           MOVE    "*"                 TO  SYS-9000-KBNCD
           MOVE    SPA-SYSYMD          TO  ORC-DBYMD
           MOVE    SYS-9000-REC        TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           MOVE    MCPDATA-REC         TO  SYS-9000-REC
      *
           MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           MOVE    SYS-9000-NYU-ORDER-FLG
                                       TO  SPA-GMN-I01-ORDERKBN
      *
      *    コンボボックス初期化
           MOVE    SPA-SYSYMD      TO  WRK-KJNYMD
           PERFORM 310-CMB-SYOKI-SEC
      *
      *    処理区分コンボ（全て表示）
           PERFORM 310-CMB-SYORIKBN-SET1-SEC
      *
      *    画面初期化
           PERFORM 420-CLEAR-SEC
      *
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    コンボボックス初期処理
      *****************************************************************
       310-CMB-SYOKI-SEC               SECTION.
      *
      *    INITIALIZE              SPA-GMN-I01-CMB-DAT-AREA
      *
      *    室料差額コンボ
           PERFORM 310-CMB-SAGAKU-SET-SEC
      *
      *    入院科コンボ
           PERFORM 310-CMB-NYUINKA-SET-SEC
      *
      *    退院事由コンボ
           PERFORM 310-CMB-TAIINCD-SET-SEC
      *
      *    初回コンボ
           PERFORM 310-CMB-SHOKAI-SET-SEC
      *
      *    担当医コンボ
           PERFORM 310-CMB-DR-SET-SEC
      *
      *    定期請求コンボ
           PERFORM 310-CMB-SEIKYU-SET-SEC
      *
      *    検索時患者表示コンボ
           PERFORM 310-CMB-DISPKBN-SET-SEC
      *
           .
       310-CMB-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    処理区分コンボ初期処理（全て表示）
      *****************************************************************
       310-CMB-SYORIKBN-SET1-SEC       SECTION.
      *
           INITIALIZE              SPA-GMN-I01-CMB-SYORIKBN
      *
           MOVE    11              TO  SPA-GMN-I01-SYORIKBN-CNT
      *
           MOVE    "01 入院登録"   TO  SPA-GMN-I01-SYORIKBN-ITM (01)
           MOVE    "02 退院登録"   TO  SPA-GMN-I01-SYORIKBN-ITM (02)
           MOVE    "03 変更"       TO  SPA-GMN-I01-SYORIKBN-ITM (03)
           MOVE    "04 照会"       TO  SPA-GMN-I01-SYORIKBN-ITM (04)
           MOVE    "05 入院取消"   TO  SPA-GMN-I01-SYORIKBN-ITM (05)
           MOVE    "06 入院取消（会計含む）"
                                   TO  SPA-GMN-I01-SYORIKBN-ITM (06)
           MOVE    "07 退院取消"   TO  SPA-GMN-I01-SYORIKBN-ITM (07)
           MOVE    "08 転科　転棟　転室"
                                   TO  SPA-GMN-I01-SYORIKBN-ITM (08)
           MOVE    "09 異動取消"   TO  SPA-GMN-I01-SYORIKBN-ITM (09)
           MOVE    "10 退院再計算" TO  SPA-GMN-I01-SYORIKBN-ITM (10)
           MOVE    "11 退院登録（診療保存）"
                                   TO  SPA-GMN-I01-SYORIKBN-ITM (11)
      *
      *
           .
       310-CMB-SYORIKBN-SET1-EXT.
           EXIT.
      *****************************************************************
      *    処理区分コンボ初期処理（初入院）
      *****************************************************************
       310-CMB-SYORIKBN-SET2-SEC       SECTION.
      *
           INITIALIZE              SPA-GMN-I01-CMB-SYORIKBN
      *
           MOVE    1               TO  SPA-GMN-I01-SYORIKBN-CNT
           MOVE    "01 入院登録"   TO  SPA-GMN-I01-SYORIKBN-ITM (01)
      *
      *    退院履歴チェック
           PERFORM 3101-TAIINRRK-CHK-SEC
      *
           .
       310-CMB-SYORIKBN-SET2-EXT.
           EXIT.
      *****************************************************************
      *    処理区分コンボ初期処理（入院中）
      *****************************************************************
       310-CMB-SYORIKBN-SET3-SEC       SECTION.
      *
           INITIALIZE              SPA-GMN-I01-CMB-SYORIKBN
      *
           MOVE    7               TO  SPA-GMN-I01-SYORIKBN-CNT
      *
           MOVE    "02 退院登録"   TO  SPA-GMN-I01-SYORIKBN-ITM (01)
           MOVE    "03 変更"       TO  SPA-GMN-I01-SYORIKBN-ITM (02)
           MOVE    "04 照会"       TO  SPA-GMN-I01-SYORIKBN-ITM (03)
           MOVE    "05 入院取消"   TO  SPA-GMN-I01-SYORIKBN-ITM (04)
           MOVE    "06 入院取消（会計含む）"
                                   TO  SPA-GMN-I01-SYORIKBN-ITM (05)
           MOVE    "08 転科　転棟　転室"
                                   TO  SPA-GMN-I01-SYORIKBN-ITM (06)
           MOVE    "11 退院登録（診療保存）"
                                   TO  SPA-GMN-I01-SYORIKBN-ITM (07)
      *
      *    退院履歴チェック
           PERFORM 3101-TAIINRRK-CHK-SEC
      *
           .
       310-CMB-SYORIKBN-SET3-EXT.
           EXIT.
      *****************************************************************
      *    処理区分コンボ初期処理（入院歴あり−退院済）
      *****************************************************************
       310-CMB-SYORIKBN-SET4-SEC       SECTION.
      *
           INITIALIZE              SPA-GMN-I01-CMB-SYORIKBN
      *
           MOVE    3               TO  SPA-GMN-I01-SYORIKBN-CNT
      *
           MOVE    "01 入院登録"   TO  SPA-GMN-I01-SYORIKBN-ITM (01)
           MOVE    "04 照会"       TO  SPA-GMN-I01-SYORIKBN-ITM (02)
           MOVE    "07 退院取消"   TO  SPA-GMN-I01-SYORIKBN-ITM (03)
      *
      *    退院履歴チェック
           PERFORM 3101-TAIINRRK-CHK-SEC
      *
           .
       310-CMB-SYORIKBN-SET4-EXT.
           EXIT.
      *****************************************************************
      *    処理区分コンボ初期処理（前回入院歴作成）
      *****************************************************************
       310-CMB-SYORIKBN-SET5-SEC       SECTION.
      *
           INITIALIZE              SPA-GMN-I01-CMB-SYORIKBN
      *
           MOVE    2               TO  SPA-GMN-I01-SYORIKBN-CNT
      *
           MOVE    "01 入院登録"   TO  SPA-GMN-I01-SYORIKBN-ITM (01)
           MOVE    "04 照会"       TO  SPA-GMN-I01-SYORIKBN-ITM (02)
      *
      *    退院履歴チェック
           PERFORM 3101-TAIINRRK-CHK-SEC
      *
      *
           .
       310-CMB-SYORIKBN-SET5-EXT.
           EXIT.
      *****************************************************************
      *    処理区分コンボ初期処理（入院中）
      *****************************************************************
       310-CMB-SYORIKBN-SET6-SEC       SECTION.
      *
           INITIALIZE              SPA-GMN-I01-CMB-SYORIKBN
      *
           MOVE    8               TO  SPA-GMN-I01-SYORIKBN-CNT
      *
           MOVE    "02 退院登録"   TO  SPA-GMN-I01-SYORIKBN-ITM (01)
           MOVE    "03 変更"       TO  SPA-GMN-I01-SYORIKBN-ITM (02)
           MOVE    "04 照会"       TO  SPA-GMN-I01-SYORIKBN-ITM (03)
           MOVE    "05 入院取消"   TO  SPA-GMN-I01-SYORIKBN-ITM (04)
           MOVE    "06 入院取消（会計含む）"
                                   TO  SPA-GMN-I01-SYORIKBN-ITM (05)
           MOVE    "08 転科　転棟　転室"
                                   TO  SPA-GMN-I01-SYORIKBN-ITM (06)
           MOVE    "09 異動取消"   TO  SPA-GMN-I01-SYORIKBN-ITM (07)
           MOVE    "11 退院登録（診療保存）"
                                   TO  SPA-GMN-I01-SYORIKBN-ITM (08)
      *
      *    退院履歴チェック
           PERFORM 3101-TAIINRRK-CHK-SEC
      *
           .
       310-CMB-SYORIKBN-SET6-EXT.
           EXIT.
      *****************************************************************
      *    退院履歴チェック処理
      *****************************************************************
       3101-TAIINRRK-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TAIINRRK
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-I01-NYUREKILST-CNT )
                    OR   ( FLG-TAIINRRK    NOT =   ZERO )
      *
               IF    ( SPA-NAI-I01-TZITA-FLG (IDX0)      =   ZERO )
                AND  ( SPA-NAI-I01-TTAIINYMD (IDX0)  NOT =   SPACE )
                   MOVE    1           TO  FLG-TAIINRRK
               END-IF
      *
           END-PERFORM
      *
           IF    ( FLG-TAIINRRK    NOT =   ZERO )
               COMPUTE SPA-GMN-I01-SYORIKBN-CNT
                   =   SPA-GMN-I01-SYORIKBN-CNT
                   +   1
               MOVE    "10 退院再計算" TO  SPA-GMN-I01-SYORIKBN-ITM
                                            (SPA-GMN-I01-SYORIKBN-CNT)
           END-IF
      *
           .
       3101-TAIINRRK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    室料差額コンボ初期処理
      *****************************************************************
       310-CMB-SAGAKU-SET-SEC          SECTION.
      *
           INITIALIZE              SPA-GMN-I01-CMB-SAGAKU
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5113"          TO  SYS-KANRICD
           MOVE    WRK-KJNYMD      TO  ORC-DBYMD
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
      *
      *    システム管理マスタ検索処理
           MOVE    "SYSKANRI-KEY2" TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1            >   30 )
                   OR    ( FLG-DBRC    NOT =   ZERO )
      *
               MOVE    MCPDATA-REC TO  SYS-5113-REC
      *
               MOVE    IDX1    TO  SPA-GMN-I01-SAGAKU-CNT
               MOVE    SYS-5113-KBNCD (1 : 2)
                           TO  SPA-GMN-I01-SAGAKU-ITM  (IDX1)(1 : 2)
               MOVE    SYS-5113-BRM-SAGAKU-NM
                           TO  WRK-99999
               MOVE    WRK-99999
                               TO  WRK-SAGAKU-NUM
               MOVE    "円"    TO  WRK-SAGAKU-EN
               MOVE    WRK-SAGAKU
                           TO  SPA-GMN-I01-SAGAKU-ITM  (IDX1)(4 : )
      *
      *        システム管理マスタ読込処理
               MOVE    "SYSKANRI-KEY2" TO  WRK-DBPATH
               PERFORM 900-TBL-READ-SEC
      *
           END-PERFORM
      *
      *    システム管理マスタクローズ処理
           MOVE    "SYSKANRI-KEY2" TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
       310-CMB-SAGAKU-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院科コンボ初期処理
      *****************************************************************
       310-CMB-NYUINKA-SET-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-I01-CMB-NYUINKA
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1005"          TO  SYS-KANRICD
           MOVE    WRK-KJNYMD      TO  ORC-DBYMD
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
      *
      *    システム管理マスタ検索処理
           MOVE    "SYSKANRI-KEY2" TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1            >   99 )
                   OR    ( FLG-DBRC    NOT =   ZERO )
      *
               MOVE    MCPDATA-REC TO  SYS-1005-REC
      *
               MOVE    IDX1    TO  SPA-GMN-I01-NYUINKA-CNT
               MOVE    SYS-1005-KBNCD (1 : 2)
                           TO  SPA-GMN-I01-NYUINKA-ITM (IDX1)(1 : 2)
               MOVE    SYS-1005-SRYKANAME
                           TO  SPA-GMN-I01-NYUINKA-ITM (IDX1)(4 : )
      *
      *        システム管理マスタ読込処理
               MOVE    "SYSKANRI-KEY2" TO  WRK-DBPATH
               PERFORM 900-TBL-READ-SEC
      *
           END-PERFORM
      *
      *    システム管理マスタクローズ処理
           MOVE    "SYSKANRI-KEY2" TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
       310-CMB-NYUINKA-SET-EXT.
           EXIT.
      *****************************************************************
      *    退院事由コンボ初期処理
      *****************************************************************
       310-CMB-TAIINCD-SET-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-I01-CMB-TAIINCD
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5013"          TO  SYS-KANRICD
           MOVE    WRK-KJNYMD      TO  ORC-DBYMD
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
      *
      *    システム管理マスタ検索処理
           MOVE    "SYSKANRI-KEY2" TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1            >   50 )
                   OR    ( FLG-DBRC    NOT =   ZERO )
      *
               MOVE    MCPDATA-REC TO  SYS-5013-REC
      *
               MOVE    IDX1    TO  SPA-GMN-I01-TAIINCD-CNT
               MOVE    SYS-5013-KBNCD (1 : 2)
                           TO  SPA-GMN-I01-TAIINCD-ITM (IDX1)(1 : 2)
               MOVE    SYS-5013-NAME
                           TO  SPA-GMN-I01-TAIINCD-ITM (IDX1)(4 : )
      *
      *        システム管理マスタ読込処理
               MOVE    "SYSKANRI-KEY2" TO  WRK-DBPATH
               PERFORM 900-TBL-READ-SEC
      *
           END-PERFORM
      *
      *    システム管理マスタクローズ処理
           MOVE    "SYSKANRI-KEY2" TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
       310-CMB-TAIINCD-SET-EXT.
           EXIT.
      *****************************************************************
      *    初回コンボ初期処理
      *****************************************************************
       310-CMB-SHOKAI-SET-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-I01-CMB-SHOKAI
      *
           MOVE    2                   TO  SPA-GMN-I01-SHOKAI-CNT
      *
           MOVE    "1 初回"            TO  SPA-GMN-I01-SHOKAI-ITM (01)
           MOVE    "2 継続"            TO  SPA-GMN-I01-SHOKAI-ITM (02)
      *
           .
       310-CMB-SHOKAI-SET-EXT.
           EXIT.
      *****************************************************************
      *    担当医コンボ初期処理
      *****************************************************************
       310-CMB-DR-SET-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-I01-CMB-DR
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1010"          TO  SYS-KANRICD
           MOVE    "1"             TO  SYS-KBNCD
           MOVE    WRK-KJNYMD      TO  ORC-DBYMD
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
      *
      *    システム管理マスタ検索処理
           MOVE    "SYSKANRI-KEY"  TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1            >   50 )
                   OR    ( FLG-DBRC    NOT =   ZERO )
      *
               MOVE    MCPDATA-REC TO  SYS-1010-REC
      *
               MOVE    IDX1    TO  SPA-GMN-I01-DR-CNT (1)
               MOVE    SYS-1010-KBNCD (2 : 4)
                           TO  SPA-GMN-I01-DR-ITM(1 IDX1)(1 : 4)
               MOVE    SYS-1010-NAME
                           TO  SPA-GMN-I01-DR-ITM(1 IDX1)(6 : )
      *
      *        システム管理マスタ読込処理
               MOVE    "SYSKANRI-KEY"  TO  WRK-DBPATH
               PERFORM 900-TBL-READ-SEC
      *
           END-PERFORM
      *
      *    システム管理マスタクローズ処理
           MOVE    "SYSKANRI-KEY"  TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           MOVE    SPA-GMN-I01-CMB-DR-OCC (1)
                                       TO  SPA-GMN-I01-CMB-DR-OCC (2)
                                           SPA-GMN-I01-CMB-DR-OCC (3)
      *
           .
       310-CMB-DR-SET-EXT.
           EXIT.
      *****************************************************************
      *    定期請求コンボ初期処理
      *****************************************************************
       310-CMB-SEIKYU-SET-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-I01-CMB-SEIKYU
      *
           MOVE    2               TO  SPA-GMN-I01-SEIKYU-CNT
      *
           MOVE    "1 医療機関での設定"
                                   TO  SPA-GMN-I01-SEIKYU-ITM (01)
           MOVE    "2 月末時のみ請求  "
                                   TO  SPA-GMN-I01-SEIKYU-ITM (02)
      *
      *
           .
       310-CMB-SEIKYU-SET-EXT.
           EXIT.
      *****************************************************************
      *    検索時患者表示コンボ初期処理
      *****************************************************************
       310-CMB-DISPKBN-SET-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-I01-CMB-DISPKBN
      *
           MOVE    2               TO  SPA-GMN-I01-DISPKBN-CNT
      *
           MOVE    "1 表示可  "    TO  SPA-GMN-I01-DISPKBN-ITM (1)
           MOVE    "2 表示不可"    TO  SPA-GMN-I01-DISPKBN-ITM (2)
      *
      *
           .
       310-CMB-DISPKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    氏名検索画面（Ｐ９７）からの戻り値設定処理
      *****************************************************************
       330-P97-SET-SEC                 SECTION.
      *
           MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU
      *
      *    人が選ばれなかった場合、
      *    同一の人が選択された場合は、画面内容の変更は行わない
           IF    ( WRK-SPA-PTID    =   ZERO )
             OR  ( WRK-SPA-PTID    =   SPA-NAI-I01-PTID )
               GO  TO  330-P97-SET-EXT
           END-IF
      *
      *    画面初期化
           PERFORM 420-CLEAR-SEC
      *
           MOVE    WRK-SPA-PTID        TO  SPA-NAI-I01-PTID 
           MOVE    WRK-SPA-PTNUM       TO  SPA-GMN-I01-PTNUM
      *
           PERFORM 4101-PTINF-SELECT-SEC
      *
           .
       330-P97-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者登録画面（Ｐ０２）からの戻り値設定処理
      *****************************************************************
       330-P02-SET-SEC                 SECTION.
      *
           IF    ( SPA-ERRMSG(1:2)     =   "NO" )
      *        戻る
               IF      SPA-PTNUM(1:1)     =   "*"
      *            画面初期化
                   PERFORM 420-CLEAR-SEC
               END-IF
               GO  TO  330-P02-SET-EXT
           END-IF
      *
           IF    ( SPA-PTID            =   ZERO )
      *        画面初期化
               PERFORM 420-CLEAR-SEC
               GO  TO  330-P02-SET-EXT
           END-IF
      *
           MOVE    SPA-PTID            TO  SPA-NAI-I01-PTID
           MOVE    SPA-PTNUM           TO  SPA-GMN-I01-PTNUM
      *
           PERFORM 4101-PTINF-SELECT-SEC
      *
           .
       330-P02-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（Ｉ０ＩＤ１）からの戻り値設定処理
      *****************************************************************
       330-I0ID1-SET-SEC               SECTION.
      *
           MOVE    SPA-I0ID1-FLG   TO  WRK-I0ID-FLG
           MOVE    SPA-I0IDCD      TO  WRK-I0IDCD
      *
           MOVE    SPACE           TO  SPA-I0ID1-FLG
           MOVE    SPACE           TO  SPA-I0IDCD
      *
           IF    ( WRK-I0ID-FLG    =   "OK" )
               MOVE    SPACE       TO  WRK-UPDKBN
               EVALUATE    WRK-I0IDCD
               WHEN    "3009"
                   MOVE    1       TO  FLG-TAINTUSANMINUS
                   MOVE    "INS"   TO  WRK-UPDKBN
                   PERFORM 4801-PTNYUINRRK-UPD-SEC
                   IF    ( SPA-ERRCD       NOT =   SPACE )
                       GO  TO  330-I0ID1-SET-EXT
                   END-IF
               WHEN    "3010"
                   MOVE    "INS"   TO  WRK-UPDKBN
                   PERFORM 4801-PTNYUINRRK-UPD-SEC
                   IF    ( SPA-ERRCD       NOT =   SPACE )
                       GO  TO  330-I0ID1-SET-EXT
                   END-IF
      *        入院取消
               WHEN "3001"
               WHEN "3006"
      *        入院取消（会計含む）
               WHEN "3002"
               WHEN "3007"
                   MOVE    "DEL"   TO  WRK-UPDKBN
                   PERFORM 4801-PTNYUINRRK-UPD-SEC
                   IF    ( SPA-ERRCD       NOT =   SPACE )
                       GO  TO  330-I0ID1-SET-EXT
                   END-IF
      *        退院取消
               WHEN "3003"
                   MOVE    "UPD"   TO  WRK-UPDKBN
                   PERFORM 4801-PTNYUINRRK-UPD-SEC
                   IF    ( SPA-ERRCD       NOT =   SPACE )
                       GO  TO  330-I0ID1-SET-EXT
                   END-IF
      *        退院登録
               WHEN "3008"
                   MOVE    ZERO    TO  FLG-TEIKI-CHK
                   PERFORM 480-TOUROKU-SEC
                   IF    ( SPA-ERRCD       NOT =   SPACE )
                       GO  TO  330-I0ID1-SET-EXT
                   END-IF
               WHEN "3004"
               WHEN "3005"
               WHEN "3011"
                   IF    ( WRK-I0IDCD      =   "3005" )
      *
                       MOVE    1   TO  SPA-NAI-I01-TORIKESI-FLG
      *
      *                診療会計テーブルチェック処理
                       PERFORM 490-SRYACCT-CHK-SEC
                       IF    ( SPA-I0IDCD  NOT =   SPACE )
                         OR  ( SPA-ERRCD   NOT =   SPACE )
                           GO  TO  330-I0ID1-SET-EXT
                       END-IF
                   END-IF
      *
      *            請求開始日取得処理
                   PERFORM 4801-SKYSTYMD-GET-SEC
      *            請求確認画面遷移処理
                   MOVE    "JOIN"      TO  WRK-MCP-PUTTYPE
                   PERFORM 490-TAIIN-SYORI-SEC
                   IF    ( SPA-ERRCD   NOT =   SPACE )
                       GO  TO  330-I0ID1-SET-EXT
                   END-IF
      *
               END-EVALUATE
      *
           ELSE
               EVALUATE    WRK-I0IDCD
      *        入院取消
               WHEN "3001"
               WHEN "3006"
      *        入院取消（会計含む）
               WHEN "3002"
               WHEN "3007"
      *        退院取消
               WHEN "3003"
                   MOVE    02      TO  SPA-GMN-I01-CUR
      *        退院登録
               WHEN "3004"
               WHEN "3011"
                   MOVE    11      TO  SPA-GMN-I01-CUR
               END-EVALUATE
           END-IF
      *
           .
       330-I0ID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（Ｉ０ＩＤ２）からの戻り値設定処理
      *****************************************************************
       330-I0ID2-SET-SEC               SECTION.
      *
           MOVE    SPA-I0ID2-FLG   TO  WRK-I0ID-FLG
           MOVE    SPA-I0IDCD2     TO  WRK-I0IDCD
      *
           MOVE    SPACE           TO  SPA-I0ID2-FLG
           MOVE    SPACE           TO  SPA-I0IDCD2
      *
           IF    ( WRK-I0ID-FLG    =   "OK" )
               MOVE    SPACE       TO  WRK-UPDKBN
               EVALUATE    WRK-I0IDCD
               WHEN "3101"
                   MOVE    "DEL"   TO  WRK-UPDKBN
                   PERFORM 4801-PTNYUINRRK-UPD-SEC
                   IF    ( SPA-ERRCD       NOT =   SPACE )
                       GO  TO  330-I0ID2-SET-EXT
                   END-IF
      *
               END-EVALUATE
           END-IF
      *
           .
       330-I0ID2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院歴表示画面から戻った場合の処理
      *****************************************************************
       330-I02-SET-SEC                 SECTION.
      *
           IF    ( SPA-NAI-I01-PTRRKNUM    =   SPA-I01-I02-PTRRKNUM )
            AND  ( SPA-I01-I02-TAIINYMD    NOT =   SPACE )
               MOVE    SPA-I01-I02-TAIINCD TO  WRK-CMB-CD
               PERFORM 4103-CMB-TAIINCD-SRH-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-TAIINCD
           END-IF
      *
           .
       330-I02-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院歴作成画面から戻った場合の処理
      *****************************************************************
       330-I03-SET-SEC                 SECTION.
      *
      *    戻るボタンで戻ってきた場合は何もしない
           IF    ( SPA-I01-I03-BTN =   "B01" )
               GO  TO  330-I03-SET-EXT
           END-IF
      *
      *    入院歴リストを再編集する
           PERFORM 4101-NYUREKI-EDIT-SEC
      *
      *    最新の入院歴を取得する
           PERFORM 4101-PTNYUINRRK-SEL1-SEC
      *
           IF  (   SPA-I01-I03-SYORIKBN        =   "2" OR  "3" )
               IF    (   FLG-PTNYUINRRK        =   ZERO )
                   IF    ( PTNYUINRRK-JTIKBN   NOT =   "5" AND "6" )
                    AND  ( PTNYUINRRK-TAIINYMD =   "99999999" )
                       MOVE    1       TO  SPA-GMN-I01-NYUREKILST-SEL
                                           SPA-GMN-I01-SELNUM
                       GO  TO  330-I03-SET-EXT
                   END-IF
               END-IF
           END-IF
      *
           IF  (   SPA-I01-I03-SYORIKBN-CD     =   "01" )
      *        入院歴なし（初入院）
               IF  (   FLG-PTNYUINRRK  NOT =   ZERO )
                   PERFORM 310-CMB-SYORIKBN-SET2-SEC
                   MOVE    SPA-GMN-I01-SYORIKBN-ITM (01)
                                       TO  SPA-GMN-I01-SYORIKBN
               ELSE
      *            歴作成データ有り
                   IF    ( PTNYUINRRK-JTIKBN   =   "5" OR "6" )
                       PERFORM 310-CMB-SYORIKBN-SET5-SEC
                       MOVE    SPA-GMN-I01-SYORIKBN-ITM (01)
                                       TO  SPA-GMN-I01-SYORIKBN
                   ELSE
                       PERFORM 310-CMB-SYORIKBN-SET4-SEC
                       MOVE    SPA-GMN-I01-SYORIKBN-ITM (01)
                                       TO  SPA-GMN-I01-SYORIKBN
                   END-IF
               END-IF
           ELSE
      *        入院歴なし（初入院）
               IF  (   FLG-PTNYUINRRK  NOT =   ZERO )
                   MOVE    SPA-SYSYMD  TO  WRK-KJNYMD
                   PERFORM 310-CMB-SYOKI-SEC
                   PERFORM 4201-INPUT-AREA-CLEAR-SEC
                   PERFORM 310-CMB-SYORIKBN-SET2-SEC
                   MOVE    SPACE       TO  SPA-GMN-I01-SYORIKBN
               ELSE
      *            歴作成データ有り
                   IF    ( PTNYUINRRK-JTIKBN   =   "5" OR "6" )
                       PERFORM 4101-INPUT-AREA-EDIT2-SEC
                       PERFORM 4201-WIDGET-INSENSITIVE-SEC
                       PERFORM 310-CMB-SYORIKBN-SET5-SEC
                       MOVE    SPA-GMN-I01-SYORIKBN-ITM (02)
                                       TO  SPA-GMN-I01-SYORIKBN
                   ELSE
                       PERFORM 4101-INPUT-AREA-EDIT-SEC
                       PERFORM 4201-WIDGET-INSENSITIVE-SEC
                       PERFORM 310-CMB-SYORIKBN-SET4-SEC
                       MOVE    SPA-GMN-I01-SYORIKBN-ITM (02)
                                       TO  SPA-GMN-I01-SYORIKBN
                   END-IF
               END-IF
           END-IF
      *
           MOVE    WIDGET-NORMAL       TO  SPA-GMN-I01-B07-STT
           MOVE    WIDGET-NORMAL       TO  SPA-GMN-I01-B08-STT
      *
           IF    ( SPA-GMN-I01-NYUREKILST-CNT  >   ZERO )
               MOVE    1           TO  SPA-GMN-I01-NYUREKILST-SEL
                                       SPA-GMN-I01-SELNUM
           ELSE
               MOVE   ZERO         TO  SPA-GMN-I01-NYUREKILST-SEL
                                       SPA-GMN-I01-SELNUM
           END-IF
      *
           .
       330-I03-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                     SECTION.
      *
           IF      ( MCP-WIDGET        =   "B06"
                                       OR  "B07"
                                       OR  "B11"
                                       OR  "B12"
                                       OR  "B23"
                                                 )
      *        患者番号チェック
               PERFORM 4102-PTNUM-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 420-CLEAR-SEC
                   GO  TO  200-GMNSENI-EXT
               END-IF
      *
           END-IF
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 210-BACK
           WHEN    "CLICKED"       ALSO    "B02"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 420-CLEAR-SEC
           WHEN    "CLICKED"       ALSO    "B03"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 420-MAEPTINF-SET-SEC
           WHEN    "CLICKED"       ALSO    "B05"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 480-KANJATOROKU-SEC
           WHEN    "CLICKED"       ALSO    "B06"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 480-NYUINREKI-SEC
           WHEN    "CLICKED"       ALSO    "B07"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 480-NYUINRRK-SAKUSEI-SEC
           WHEN    "CLICKED"       ALSO    "B08"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 480-NYUINRRK-SYUSEI-SEC
           WHEN    "CLICKED"       ALSO    "B09"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 480-KENSAKU-SEC
           WHEN    "CLICKED"       ALSO    "B12"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               MOVE    1                   TO  FLG-TEIKI-CHK
               PERFORM 480-TOUROKU-SEC
           WHEN    "CLICKED"       ALSO    "B11"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 480-KARUTE-PRT-SEC
           WHEN    "CLICKED"       ALSO    "B23"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 480-ZOKUSI-PRT-SEC
           WHEN    "CLICKED"       ALSO    "B20"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 480-S02-SENI-SEC
           WHEN    "CLICKED"       ALSO    "B21"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 480-NYUINKAIKEI-SEC
           WHEN    "CLICKED"       ALSO    "B24"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 480-I06-SENI-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           IF      ( MCP-WIDGET    NOT =   "PTNUM" )
      *        患者番号チェック
               PERFORM 4102-PTNUM-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 420-CLEAR-SEC
                   GO  TO  200-ENTRY-EXT
               END-IF
      *
           END-IF
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    患者番号チェック
           WHEN    "ACTIVATE"      ALSO    "PTNUM"
               PERFORM 4101-PTNUM-CHK-SEC
      *    処理区分チェック
           WHEN    "ACTIVATE"      ALSO    "SYORIKBN"
               PERFORM 4101-SYORIKBN-CHK-SEC
           WHEN    OTHER
      *
      *        入力個所のセット
               PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC          SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    入院歴リスト
           WHEN        ANY         ALSO    "NYUREKILST"
               PERFORM 4101-NYUREKILST-SEL-SEC
           WHEN        OTHER
               MOVE    ZERO        TO      SPA-GMN-I01-CUR
      *
           END-EVALUATE
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    処理区分チェック
           PERFORM 4102-SYORIKBN-CHK-SEC
           IF    ( SPA-ERRCD NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
      *    入院退院登録画面で登録した入院歴のみチェックする
           IF    ( SPA-NAI-I01-NYURRK-FLG   = ZERO )
      *        基本チェック処理
               PERFORM 4102-KIHON-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                 OR  ( FLG-END     NOT =   ZERO  )
                   GO  TO  410-INPUT-CHK-EXT
               END-IF
      *
           END-IF
      *
      *    選択番号チェック処理
           PERFORM 4101-SELNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
      *    異動日
           MOVE    I01-IDOYMD          TO  SPA-GMN-I01-IDOYMD
      *    病室番号
           MOVE    I01-BRMNUM          TO  SPA-GMN-I01-BRMNUM
      *    病棟名称
           MOVE    I01-BTUNAME         TO  SPA-GMN-I01-BTUNAME
      *    室料差額
           MOVE    I01-SAGAKU          TO  SPA-GMN-I01-SAGAKU
      *    入院日
           MOVE    I01-NYUINYMD        TO  SPA-GMN-I01-NYUINYMD
      *    入院科
           MOVE    I01-NYUINKA         TO  SPA-GMN-I01-NYUINKA
      *    初回
           MOVE    I01-SHOKAI          TO  SPA-GMN-I01-SHOKAI
      *    初回番号
           MOVE    I01-SHONUM          TO  SPA-GMN-I01-SHONUM
      *    初回入院日
           MOVE    I01-LBL-SHONYUINYMD TO  SPA-GMN-I01-LBL-SHONYUINYMD
      *    退院日
           MOVE    I01-TAIINYMD        TO  SPA-GMN-I01-TAIINYMD
      *    退院事由
           MOVE    I01-TAIINCD         TO  SPA-GMN-I01-TAIINCD
      *    担当医
           MOVE    I01-DR (1)          TO  SPA-GMN-I01-DR (1)
           MOVE    I01-DR (2)          TO  SPA-GMN-I01-DR (2)
           MOVE    I01-DR (3)          TO  SPA-GMN-I01-DR (3)
      *    保険組み合わせ
           MOVE    I01-HKNCOMBI        TO  SPA-GMN-I01-HKNCOMBI
      *    特定入院
           MOVE    I01-TOKNYUIN        TO  SPA-GMN-I01-TOKNYUIN
      *    前受け金
      *    MOVE    I01-MAEUKE          TO  SPA-GMN-I01-MAEUKE
      *    定期請求
           MOVE    I01-SEIKYU          TO  SPA-GMN-I01-SEIKYU
      *    検索時患者表示
           MOVE    I01-DISPKBN         TO  SPA-GMN-I01-DISPKBN
      *    選択番号
           MOVE    I01-SELNUM          TO  SPA-GMN-I01-SELNUM
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
           EVALUATE    SPA-GMN-I01-SYORIKBN-CD
           WHEN    "01"
      *
      *        入院日チェック処理
               PERFORM 4101-NYUINYMD-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
               MOVE    SPA-NAI-I01-NYUINYMD
                                   TO  WRK-KIHON-KJNYMD
           WHEN    "08"
      *
      *        異動日チェック処理
               PERFORM 4101-IDOYMD-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
               MOVE    SPA-NAI-I01-IDOYMD
                                   TO  WRK-KIHON-KJNYMD
           WHEN    OTHER
               MOVE    SPA-NAI-I01-MAEIDOYMD
                                   TO  WRK-KIHON-KJNYMD
           END-EVALUATE
      *
      *    退院日チェック処理
           PERFORM 4101-TAIINYMD-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    病室番号チェック処理
           IF    ( SPA-GMN-I01-BRMNUM  NOT =   SPA-NAI-I01-BRMNUM-MAE )
            OR   ( SPA-GMN-I01-BTUNAME-CNT =   ZERO )
               PERFORM 4101-BRMNUM-CHK-SEC
               MOVE    SPA-GMN-I01-BRMNUM
                                       TO  SPA-NAI-I01-BRMNUM-MAE
               MOVE    SPA-ERRCD       TO  SPA-NAI-I01-BRMNUM-ERR
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
           ELSE
               IF    ( SPA-NAI-I01-BRMNUM-ERR
                                   NOT =   SPACE )
                   MOVE    SPA-NAI-I01-BRMNUM-ERR
                                       TO  SPA-ERRCD
                   MOVE    4           TO  SPA-GMN-I01-CUR
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
           END-IF
      *
      *    病棟名称チェック処理
           IF    ( SPA-GMN-I01-BTUNAME
                                       NOT =   SPA-NAI-I01-BTUNAME-MAE )
               PERFORM 4101-BTUNAME-CHK-SEC
               MOVE    SPA-GMN-I01-BTUNAME
                                       TO  SPA-NAI-I01-BTUNAME-MAE
               MOVE    SPA-ERRCD       TO  SPA-NAI-I01-BTUNAME-ERR
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
           ELSE
               IF    ( SPA-NAI-I01-BTUNAME-ERR
                                   NOT =   SPACE )
                   MOVE    SPA-NAI-I01-BTUNAME-ERR
                                       TO  SPA-ERRCD
                   MOVE    5           TO  SPA-GMN-I01-CUR
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
           END-IF
      *
      *    PERFORM 4101-BTUNAME-CHK-SEC
      *    IF    ( SPA-ERRCD   NOT =   SPACE )
      *        GO  TO  4102-KIHON-CHK-EXT
      *    END-IF
      *
      *    室料差額チェック処理
           PERFORM 4101-SAGAKU-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    入院科チェック処理
           PERFORM 4101-NYUINKA-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    初回チェック処理
           PERFORM 4101-SHOKAI-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    初回番号チェック処理
           PERFORM 4101-SHONUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    退院事由チェック処理
           PERFORM 4101-TAIINCD-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    担当医チェック処理
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   3   )
      *
               PERFORM 4101-DR-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
      *
           END-PERFORM
      *
      *    保険組み合わせチェック処理
           PERFORM 4101-HKNCOMBI-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    特定入院チェック処理
           PERFORM 4101-TOKNYUIN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    前受け金チェック処理
      *    PERFORM 4101-MAEUKE-CHK-SEC
      *    IF    ( SPA-ERRCD   NOT =   SPACE )
      *        GO  TO  4102-KIHON-CHK-EXT
      *    END-IF
      *
      *    定期請求チェック処理
           PERFORM 4101-SEIKYU-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    検索時患者表示チェック処理
           PERFORM 4101-DISPKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       4101-PTNUM-CHK-SEC              SECTION.
      *
           PERFORM  420-CLEAR-SEC
      *
           MOVE    1               TO  SPA-GMN-I01-CUR
      *
           MOVE    SPACE           TO  SPA-GMN-I01-ERRCD (1)
      *
           INITIALIZE                  SPA-NAI-I01-PTID
                                       SPA-GMN-I01-NAME
                                       SPA-NAI-I01-KANANAME
                                       SPA-GMN-I01-SEX
                                       SPA-GMN-I01-BIRTHDAY
                                       SPA-NAI-I01-BIRTHDAY
                                       SPA-GMN-I01-AGE
                                       SPA-KYOTUKEY
      *
           MOVE    I01-PTNUM       TO  SPA-GMN-I01-PTNUM
      *
      *    患者番号が入力されていること
           IF    ( SPA-GMN-I01-PTNUM   =   SPACE )
               MOVE    "1001"      TO  SPA-ERRCD
               MOVE    SPA-ERRCD   TO  SPA-GMN-I01-ERRCD (1)
               GO  TO  4101-PTNUM-CHK-EXT
           END-IF
      *
      *    ＊のとき、患者新規登録へ
           IF    ( SPA-GMN-I01-PTNUM(1:1)  =   "*" )
               MOVE    "*"                 TO  SPA-PTNUM
               PERFORM 481-KANJATOROKU-SEC
               GO  TO      4101-PTNUM-CHK-EXT
           END-IF
      *
      *    患者番号変換サブ呼び出し
           INITIALIZE                  ORCSPTNUMAREA
           MOVE    SPA-GMN-I01-PTNUM   TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
           MOVE    SPTNUM-PTNUM        TO  SPA-GMN-I01-PTNUM
           EVALUATE    SPTNUM-RC
               WHEN    00
                   MOVE    SPTNUM-PTNUM        TO  SPA-GMN-I01-PTNUM
                   MOVE    SPTNUM-PTID         TO  SPA-NAI-I01-PTID
               WHEN   10
                   MOVE    "0002"              TO  SPA-ERRCD
                   MOVE    SPA-ERRCD           TO  SPA-GMN-I01-ERRCD (1)
                   GO  TO  4101-PTNUM-CHK-EXT
               WHEN    20
      *            氏名検索へ
                   MOVE    SPACE               TO  LINKAREA
                   MOVE    SPA-KYOUTU          TO  LNK-COMMON
                   MOVE    SPA-I0FREE          TO  LNK-SCRSPA
      *
                   MOVE    "I01"               TO  SPA-MOTOPG
                   MOVE    "P97"               TO  SPA-SAKIPG
                   MOVE    ZERO                TO  SPA-PTID
      *            患者氏名セット
                   MOVE    SPA-GMN-I01-PTNUM   TO  SPA-NAME
                   MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
                   MOVE    "CHANGE"            TO  MCP-PUTTYPE
                   MOVE    "P97"               TO  MCP-WINDOW
                   PERFORM 900-PUT-WINDOW
                   MOVE    1                   TO  FLG-END
                   GO      TO     4101-PTNUM-CHK-EXT
               WHEN    OTHER
                   MOVE    "0002"              TO  SPA-ERRCD
                   MOVE    SPA-ERRCD           TO  SPA-GMN-I01-ERRCD (1)
                   GO  TO  4101-PTNUM-CHK-EXT
           END-EVALUATE
      *
      *    患者マスタ検索
           INITIALIZE  PTINF-REC
           MOVE    SPA-HOSPID                  TO  PTINF-HOSPID
           MOVE    SPA-NAI-I01-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC                   TO  MCPDATA-REC
      *
           MOVE    "PTINF-KEY"                 TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        =   ZERO )
      *
      *        排他制御処理
               PERFORM 990-LOCK-IN-SEC
               IF  (   SPA-ERRCD   NOT =   SPACE )
                   MOVE    SPA-ERRCD           TO  SPA-GMN-I01-ERRCD (1)
                   GO  TO  4101-PTNUM-CHK-EXT
               END-IF
      *
      *        患者情報設定
               PERFORM 4101-PTINF-SET-SEC
      *        IF    ( SPA-NAI-I01-NYURRK-FLG        =   ZERO )
      *            IF    ( SPA-GMN-I01-HKNCOMBI-CNT   >   ZERO )
      *                CONTINUE
      *            ELSE
      *                MOVE    "1021"      TO  SPA-ERRCD
      *                MOVE    1           TO  SPA-GMN-I01-CUR
      *            END-IF
      *        END-IF
      *
           ELSE
               MOVE    "0002"                  TO  SPA-ERRCD
               MOVE    SPA-ERRCD               TO  SPA-GMN-I01-ERRCD (1)
           END-IF
      *
      *    患者マスタクローズ
           MOVE    "PTINF-KEY"                 TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
      *
           .
      *
       4101-PTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴マスタ検索処理（入力領域表示用）
      *****************************************************************
       4101-PTNYUINRRK-SEL1-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNYUINRRK
           INITIALIZE  PTNYUINRRK-REC
           MOVE    ZERO            TO  SPA-NAI-I01-PTRRKNUM
                                       SPA-NAI-I01-PTRRKEDANUM
      *
           MOVE    SPA-HOSPID      TO  PTNYUINRRK-HOSPID
           MOVE    SPA-NAI-I01-PTID
                                   TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
      *
      *    入院履歴検索処理
           MOVE    "PTNYUINRRK-KEY3"
                                   TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        =   ZERO )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
               MOVE    PTNYUINRRK-RRKNUM
                                       TO  SPA-NAI-I01-PTRRKNUM
               MOVE    PTNYUINRRK-RRKEDANUM
                                       TO  SPA-NAI-I01-PTRRKEDANUM
           ELSE
               MOVE    1           TO  FLG-PTNYUINRRK
           END-IF
      *
      *    入院履歴クローズ
           MOVE    "PTNYUINRRK-KEY3"
                                   TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
       4101-PTNYUINRRK-SEL1-EXT.
           EXIT.
      *****************************************************************
      *    入力領域編集処理
      *****************************************************************
       4101-INPUT-AREA-EDIT-SEC        SECTION.
      *
           IF    ( PTNYUINRRK-TAIINYMD =   "99999999" )
               MOVE    PTNYUINRRK-TENNYUYMD
                                   TO  WRK-KJNYMD
               PERFORM 310-CMB-SYOKI-SEC
           ELSE
               MOVE    PTNYUINRRK-TAIINYMD
                                   TO  WRK-KJNYMD
               PERFORM 310-CMB-SYOKI-SEC
           END-IF
      *
      *    入力領域初期化
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
           MOVE    ZERO            TO  SPA-NAI-I01-NYURRK-FLG
      *
      *    同日再入院フラグ
           PERFORM 800-ORCSDNTCHK-SEC
           MOVE    SDNTCHK-DJKBN   TO  SPA-NAI-I01-DJKBN
           MOVE    SDNTCHK-STYMD   TO  SPA-NAI-I01-DJSTYMD
      *
      *    前回異動日
           MOVE    PTNYUINRRK-TENNYUYMD
                                   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
      *
           IF      ( PTNYUINRRK-JTIKBN     = "2" OR "3" OR "4" OR "7" )
            AND    ( PTNYUINRRK-TAIINYMD   = "99999999" )
               MOVE    WRK-HENYMDG TO  SPA-GMN-I01-MAEIDOYMD
           END-IF
      *
           MOVE    WRK-SYMD        TO  SPA-NAI-I01-MAEIDOYMD
      *
      *    保険組み合わせコンボ設定処理
           MOVE    PTNYUINRRK-TENNYUYMD
                                   TO  WRK-KJNYMD
           PERFORM 4101-CMB-HKNCOMBI-SET-SEC
      *
      *    病室番号
           MOVE   PTNYUINRRK-BRMNUM ( 3 : 6 )
                                   TO  WRK-BRMNUM-IN
           PERFORM 4101-BRMNUM-LEFT-EDIT-SEC
           MOVE   WRK-BRMNUM-OT    TO  SPA-GMN-I01-BRMNUM
                                       SPA-NAI-I01-BRMNUM-MAE
                                       SPA-NAI-I01-BRMNUM-MOT
      *
      *    病室情報より病棟コンボ編集
           MOVE    PTNYUINRRK-TENNYUYMD
                                   TO  WRK-KJNYMD
           PERFORM 4101-BRMNUM-SEL-SEC
      *
      *    病棟番号
           MOVE    PTNYUINRRK-BTUNUM
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-BTUNAME-SRH-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-BTUNAME
                                       SPA-NAI-I01-BTUNAME-MAE
                                       SPA-NAI-I01-BTUNAME-MOT
      *
      *    入院科
           MOVE    PTNYUINRRK-NYUINKA
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-NYUINKA-SRH-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-NYUINKA
           MOVE    PTNYUINRRK-NYUINKA
                                   TO  SPA-NAI-I01-NYUINKA-MOT
      *
      *    保険組合せ番号
           MOVE    PTNYUINRRK-HKNCOMBINUM
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-HKNCOMBI-SRH-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-HKNCOMBI
           MOVE    WRK-CMB-ITM2    TO  SPA-NAI-I01-FTNRATE
           MOVE    PTNYUINRRK-HKNCOMBINUM
                                   TO  SPA-NAI-I01-HKNCOMBI-MOT
      *
      *    入院日
           MOVE    PTNYUINRRK-NYUINYMD
                                   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-I01-NYUINYMD
           MOVE    WRK-SYMD        TO  SPA-NAI-I01-NYUINYMD
                                       SPA-NAI-I01-NYUINYMD-MAE
      *
      *    退院日
           IF    ( PTNYUINRRK-TAIINYMD     =   "99999999" )
               MOVE    SPACE           TO  SPA-GMN-I01-TAIINYMD
                                           SPA-NAI-I01-TAIINYMD
           ELSE
               MOVE    PTNYUINRRK-TAIINYMD
                                   TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-I01-TAIINYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-I01-TAIINYMD
      *
      *        退院事由
               MOVE    PTNYUINRRK-TAIINCD  TO  WRK-CMB-CD
               PERFORM 4103-CMB-TAIINCD-SRH-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-TAIINCD
           END-IF
      *
      *
      *    特定入院料コンボ設定処理
           MOVE    PTNYUINRRK-TENNYUYMD
                                   TO  WRK-KJNYMD
           PERFORM 4101-CMB-TOKNYUIN-SET-SEC
      *
      *    特定入院料
           MOVE    SPACE           TO  WRK-CMB-CD
           MOVE    PTNYUINRRK-TOKUTEI-KBN
                                   TO  WRK-CMB-CD (1 : 1)
           MOVE    PTNYUINRRK-TOKUTEI-NYUIN
                                   TO  WRK-CMB-CD (2 : 2)
      *
           PERFORM 4103-CMB-TOKNYUIN-SRH2-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-TOKNYUIN
           MOVE    WRK-CMB-ITM2    TO  SPA-NAI-I01-TOKNYUIN-ORG
                                       SPA-NAI-I01-TOKNYUINMOT
      *
      *    算定入院料
           IF    ( SPA-GMN-I01-TOKNYUIN    =   SPACE )
               MOVE    SPA-NAI-I01-KHNRYO-NM
                                       TO  SPA-GMN-I01-SNTNYUIN
           ELSE
               MOVE    SPA-GMN-I01-TOKNYUIN-NM
                                       TO  SPA-GMN-I01-SNTNYUIN
           END-IF
      *
      *    室料差額
           MOVE    PTNYUINRRK-RM-SAGAKU
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-SAGAKU-SRH-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-SAGAKU
      *
      *    定期請求区分
           MOVE    PTNYUINRRK-TEIKI-SEIKYUKBN
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-SEIKYU-SRH-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-SEIKYU
      *
      *    検索表示区分
           MOVE    PTNYUINRRK-KENSAKU-DISPKBN
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-DISPKBN-SRH-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-DISPKBN
      *
      *    初回継続区分
           MOVE  PTNYUINRRK-SHOKAIKBN
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-SHOKAI-SRH-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-SHOKAI
      *
      *    初回番号
           MOVE    PTNYUINRRK-SHONUM
                                   TO  SPA-GMN-I01-SHONUM
      *    初回入院日
           PERFORM 4101-SHONYUINYMD-EDIT-SEC
      *
      *    担当医１
           MOVE    PTNYUINRRK-DRCD1 (2 : 4)
                                   TO  WRK-CMB-CD
           MOVE    1               TO  IDX1
           PERFORM 4103-CMB-DR-SRH-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-DR (1)
      *
      *    担当医２
           MOVE    PTNYUINRRK-DRCD2 (2 : 4) 
                                   TO  WRK-CMB-CD
           MOVE    2               TO  IDX1
           PERFORM 4103-CMB-DR-SRH-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-DR (2)
      *
      *    担当医３
           MOVE    PTNYUINRRK-DRCD3 (2 : 4)
                                   TO  WRK-CMB-CD
           MOVE    3               TO  IDX1
           PERFORM 4103-CMB-DR-SRH-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-DR (3)
       .
      *
       4101-INPUT-AREA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    初回入院日編集処理
      *****************************************************************
       4101-SHONYUINYMD-EDIT-SEC       SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-I01-NYUREKILST-CNT )
               IF    ( SPA-GMN-I01-SHONUM
                                   =   SPA-GMN-I01-TSHONUM (IDX1))
                   IF    ( SPA-GMN-I01-TSHOKAI (IDX1)  =   "1" )
                       STRING "入院日 "
                              SPA-GMN-I01-TNYUINYMD (IDX1)
                       DELIMITED   BY  SIZE
                       INTO   SPA-GMN-I01-LBL-SHONYUINYMD
                       END-STRING
                       MOVE    SPA-GMN-I01-NYUREKILST-CNT
                                   TO  IDX1
                   END-IF
               END-IF
      *
           END-PERFORM
       .
      *
       4101-SHONYUINYMD-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入力領域編集処理（入院歴を編集）
      *****************************************************************
       4101-INPUT-AREA-EDIT2-SEC       SECTION.
      *
      *    入力領域初期化
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
           MOVE    1               TO  SPA-NAI-I01-NYURRK-FLG
      *
      *    同日再入院フラグ
           PERFORM 800-ORCSDNTCHK-SEC
           MOVE    SDNTCHK-DJKBN   TO  SPA-NAI-I01-DJKBN
           MOVE    SDNTCHK-STYMD   TO  SPA-NAI-I01-DJSTYMD
      *
      *    病棟名称
           MOVE    PTNYUINRRK-BTUNAME
                                   TO  SPA-GMN-I01-BTUNAME
      *
      *    入院日
           MOVE    PTNYUINRRK-NYUINYMD
                                   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-I01-NYUINYMD
           MOVE    WRK-SYMD        TO  SPA-NAI-I01-NYUINYMD
                                       SPA-NAI-I01-NYUINYMD-MAE
      *
      *    退院日
           IF    ( PTNYUINRRK-TAIINYMD     =   "99999999" )
               MOVE    SPACE           TO  SPA-GMN-I01-TAIINYMD
                                           SPA-NAI-I01-TAIINYMD
           ELSE
               MOVE    PTNYUINRRK-TAIINYMD
                                   TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-I01-TAIINYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-I01-TAIINYMD
      *
      *        退院事由
               MOVE    PTNYUINRRK-TAIINCD  TO  WRK-CMB-CD
               PERFORM 4103-CMB-TAIINCD-SRH-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-TAIINCD
           END-IF
      *
      *    初回継続区分
           MOVE  PTNYUINRRK-SHOKAIKBN
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-SHOKAI-SRH-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-SHOKAI
      *
      *    初回番号
           MOVE    PTNYUINRRK-SHONUM
                                   TO  SPA-GMN-I01-SHONUM
      *    初回入院日
           PERFORM 4101-SHONYUINYMD-EDIT-SEC
      *
       .
      *
       4101-INPUT-AREA-EDIT2-EXT.
           EXIT.
      *****************************************************************
      *    病室番号左詰処理
      *****************************************************************
       4101-BRMNUM-LEFT-EDIT-SEC       SECTION.
      *
           IF    ( WRK-BRMNUM-IN (1 : 1)   =   SPACE )
               MOVE    SPACE           TO  WRK-STR1
               MOVE    1               TO  WRK-IDX1
               UNSTRING    WRK-BRMNUM-IN
                   DELIMITED   BY  ALL SPACE
                   INTO    WRK-STR1
                   WITH    POINTER WRK-IDX1
               END-UNSTRING
               MOVE    WRK-BRMNUM-IN (WRK-IDX1 : )
                                       TO  WRK-BRMNUM-EDT
           ELSE
               MOVE    WRK-BRMNUM-IN   TO  WRK-BRMNUM-EDT
           END-IF
      *
           PERFORM VARYING WRK-IDX1    FROM    6   BY  -1
               UNTIL     ( WRK-IDX1    <   1 )
                 OR      ( WRK-BRMNUM-EDT (WRK-IDX1 : 1)
                                   NOT =   SPACE )
               CONTINUE
           END-PERFORM
      *
           IF    ( WRK-IDX1    <   1 )
               MOVE    WRK-BRMNUM-EDT  TO  WRK-BRMNUM-OT
               GO  TO  4101-BRMNUM-LEFT-EDIT-EXT
           END-IF
      *
      *    病室番号が数字の場合前ゼロをとる
           IF    ( WRK-BRMNUM-EDT ( 1 : WRK-IDX1 )
                                       IS  NUMERIC )
               IF    ( WRK-BRMNUM-EDT ( 1 : 1 )
                                       =   ZERO )
                   MOVE    1               TO  WRK-IDX2
                   MOVE    SPACE           TO  WRK-STR1
                   UNSTRING    WRK-BRMNUM-EDT
                       DELIMITED BY  ALL ZERO
                   INTO    WRK-STR1
                   WITH    POINTER WRK-IDX2
                   END-UNSTRING
                   MOVE    WRK-BRMNUM-EDT (WRK-IDX2 : )
                                           TO  WRK-BRMNUM-OT
               ELSE
                   MOVE    WRK-BRMNUM-EDT  TO  WRK-BRMNUM-OT
               END-IF
           ELSE
                   MOVE    WRK-BRMNUM-EDT  TO  WRK-BRMNUM-OT
           END-IF
      *
           .
       4101-BRMNUM-LEFT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴編集処理
      *****************************************************************
       4101-NYUREKI-EDIT-SEC       SECTION.
      *
      *    患者履歴マスタ検索
           PERFORM 4101-PTNYUINRRK-SEL2-SEC
      *
      *    主病名編集処理
           PERFORM 4101-SYUBYOMEI-EDIT-SEC
      *
      *    日数編集処理
           PERFORM 4101-NISSU-EDIT-SEC
      *
           .
       4101-NYUREKI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴編集処理（入院歴表示用）
      *****************************************************************
       4101-PTNYUINRRK-SEL2-SEC        SECTION.
      *
      *
           INITIALIZE  PTNYUINRRK-REC
                       SPA-GMN-I01-NYUREKILST-AREA
                       SPA-NAI-I01-NYUREKILST-AREA
                       WRK-REKI-BYOMEI-EDIT-AREA
      *
           MOVE    1               TO  FLG-NYUREKILST-ROW
      *
           MOVE    SPA-HOSPID      TO  PTNYUINRRK-HOSPID
           MOVE    SPA-NAI-I01-PTID
                                   TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
      *
      *    入院履歴検索処理
           MOVE    "PTNYUINRRK-KEY2"
                                   TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1            >   CONST-NYUREKILST-MAX   )
               OR        ( FLG-DBRC    NOT =   ZERO )
      *
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
               MOVE    IDX1        TO  SPA-GMN-I01-NYUREKILST-CNT
      *
      *
      *        入院歴リスト編集
               IF    ( PTNYUINRRK-JTIKBN   =   "5" OR "6" )
                   PERFORM 4101-NYUREKILST-EDIT2-SEC
               ELSE
                   PERFORM 4101-NYUREKILST-EDIT-SEC
               END-IF
      *
      *        入院履歴検索処理
               MOVE    "PTNYUINRRK-KEY2"
                                   TO  WRK-DBPATH
               PERFORM 900-TBL-READ-SEC
      *
           END-PERFORM
      *
      *    入院履歴クローズ
           MOVE    "PTNYUINRRK-KEY2"
                                   TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
       4101-PTNYUINRRK-SEL2-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報検索処理
      *****************************************************************
       4101-PTINF-SELECT-SEC              SECTION.
      *
      *    患者マスタ検索
           INITIALIZE  PTINF-REC
           MOVE    SPA-HOSPID          TO  PTINF-HOSPID
           MOVE    SPA-NAI-I01-PTID    TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *
           MOVE    "PTINF-KEY"         TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        =   ZERO )
      *
      *        排他制御処理
               PERFORM 990-LOCK-IN-SEC
               IF  (   SPA-ERRCD   NOT =   SPACE )
                   MOVE    SPA-ERRCD   TO  SPA-GMN-I01-ERRCD (1)
                   GO  TO  4101-PTINF-SELECT-EXT
               END-IF
      *
      *        患者情報設定
               PERFORM 4101-PTINF-SET-SEC
           END-IF
      *
      *    患者マスタクローズ
           MOVE    "PTINF-KEY"         TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
      *
           .
       4101-PTINF-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    患者情報設定処理
      *****************************************************************
       4101-PTINF-SET-SEC              SECTION.
      *
           MOVE    MCPDATA-REC     TO  PTINF-REC
      *    患者ＩＤ
           MOVE    PTINF-PTID      TO  SPA-NAI-I01-PTID
      *    患者氏名
           MOVE    PTINF-NAME      TO  SPA-GMN-I01-NAME
      *    患者カナ氏名
           MOVE    PTINF-KANANAME  TO  SPA-NAI-I01-KANANAME
      *    患者性別
           EVALUATE    PTINF-SEX
           WHEN    "1"
               MOVE    "男"        TO  SPA-GMN-I01-SEX
           WHEN    "2"
               MOVE    "女"        TO  SPA-GMN-I01-SEX
           END-EVALUATE
      *    患者生年月日
           MOVE    PTINF-BIRTHDAY  TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-I01-BIRTHDAY
           MOVE    PTINF-BIRTHDAY  TO  SPA-NAI-I01-BIRTHDAY
      *    患者年齢
           MOVE    PTINF-BIRTHDAY  TO  WRK-BIRTHDAY
           PERFORM 4101-AGE-CALC-SEC
      *
           MOVE    WRK-AGE-G       TO  SPA-GMN-I01-AGE
      *
           MOVE    WIDGET-INSENSITIVE
                                       TO  SPA-GMN-I01-IDOYMD-STT
      *
      *
      *    入院履歴編集処理
           PERFORM 4101-NYUREKI-EDIT-SEC
      *
           MOVE    ZERO                TO  FLG-SYORIPTN
      *
      *    入院履歴マスタ検索処理（入力領域表示用）
           PERFORM 4101-PTNYUINRRK-SEL1-SEC
      *    入院歴なし（初入院）
           IF  (   FLG-PTNYUINRRK  NOT =   ZERO )
               MOVE    1               TO  FLG-SYORIPTN
           ELSE
      *
               EVALUATE    TRUE
      *        歴作成データあり
               WHEN    PTNYUINRRK-JTIKBN   =   "5"
               WHEN    PTNYUINRRK-JTIKBN   =   "6"
                   MOVE    2           TO  FLG-SYORIPTN
      *        入院中
               WHEN    PTNYUINRRK-TAIINYMD =   "99999999"
                   MOVE    3           TO  FLG-SYORIPTN
      *        退院済み
               WHEN    OTHER
                   MOVE    4           TO  FLG-SYORIPTN
               END-EVALUATE
           END-IF
      *
           EVALUATE    FLG-SYORIPTN
           WHEN    1
               MOVE    3           TO  SPA-GMN-I01-CUR
               MOVE    SPA-SYSYMD  TO  WRK-KJNYMD
               PERFORM 310-CMB-SYOKI-SEC
               PERFORM 4201-INPUT-AREA-CLEAR-SEC
               PERFORM 4101-WIDGET-NYUIN-STT-SEC
      *        保険組み合わせコンボ設定処理
               MOVE    SPA-SYSYMD      TO  WRK-KJNYMD
               PERFORM 4101-CMB-HKNCOMBI-SET-SEC
      *        受付マスタより入院科等を確定する
               MOVE    SPA-SYSYMD  TO  WRK-KJNYMD
               PERFORM 4101-UKETUKE-SEL-SEC
      *        受診履歴検索処理
               MOVE    SPA-SYSYMD  TO  WRK-KJNYMD
               PERFORM 4101-JYURRK-SEL-SEC
               PERFORM 310-CMB-SYORIKBN-SET2-SEC
      *        デフォルトの処理区分に入院登録を設定
               MOVE    SPA-GMN-I01-SYORIKBN-ITM (01)
                                   TO  SPA-GMN-I01-SYORIKBN
           WHEN    2
               MOVE    2           TO  SPA-GMN-I01-CUR
      *
               PERFORM 4101-INPUT-AREA-EDIT2-SEC
               PERFORM 4201-WIDGET-INSENSITIVE-SEC
               MOVE    WIDGET-NORMAL
                                   TO  SPA-GMN-I01-B07-STT
               MOVE    WIDGET-NORMAL
                                   TO  SPA-GMN-I01-B08-STT
               PERFORM 310-CMB-SYORIKBN-SET5-SEC
      *        デフォルトの処理区分に照会を設定
               MOVE    SPA-GMN-I01-SYORIKBN-ITM (02)
                                   TO  SPA-GMN-I01-SYORIKBN
      *
      *
           WHEN    3
               MOVE    2               TO  SPA-GMN-I01-CUR
               PERFORM 4101-INPUT-AREA-EDIT-SEC
               IF    ( PTNYUINRRK-RRKEDANUM    >   1   )
                   PERFORM 310-CMB-SYORIKBN-SET6-SEC
               ELSE
                   PERFORM 310-CMB-SYORIKBN-SET3-SEC
               END-IF
      *
               PERFORM 4101-WIDGET-HENKOU-STT-SEC
      *        デフォルトの処理区分に変更を設定
               MOVE    SPA-GMN-I01-SYORIKBN-ITM (02)
                                   TO  SPA-GMN-I01-SYORIKBN
           WHEN    4
               MOVE    2               TO  SPA-GMN-I01-CUR
               PERFORM 4101-INPUT-AREA-EDIT-SEC
               PERFORM 4201-WIDGET-INSENSITIVE-SEC
               MOVE    WIDGET-NORMAL
                                   TO  SPA-GMN-I01-B07-STT
               MOVE    WIDGET-NORMAL
                                   TO  SPA-GMN-I01-B08-STT
      *
               PERFORM 310-CMB-SYORIKBN-SET4-SEC
      *        デフォルトの処理区分に照会を設定
               MOVE    SPA-GMN-I01-SYORIKBN-ITM (02)
                                   TO  SPA-GMN-I01-SYORIKBN
      *
           END-EVALUATE
      *
           IF    ( SPA-GMN-I01-NYUREKILST-CNT  >   ZERO )
               MOVE    1           TO  SPA-GMN-I01-NYUREKILST-SEL
                                       SPA-GMN-I01-SELNUM
           END-IF
      *
           MOVE    SPA-GMN-I01-PTNUM   TO  SPA-LAST-PTNUM
           MOVE    SPA-NAI-I01-PTID    TO  SPA-LAST-PTID
      *
           .
      *
       4101-PTINF-SET-EXT.
           EXIT.
      *****************************************************************
      *    受付マスタ検索処理
      *****************************************************************
       4101-UKETUKE-SEL-SEC            SECTION.
      
      *
      *    受付マスタ検索
           MOVE    SPACE               TO  UKETUKE-REC
           INITIALIZE                  UKETUKE-REC
           MOVE    SPA-HOSPID          TO  UKE-HOSPID
           MOVE    WRK-KJNYMD          TO  UKE-UKEYMD
           MOVE    SPA-NAI-I01-PTID    TO  UKE-PTID
      *
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "UKETUKE-KEY2"      TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
      *
           MOVE    ZERO                TO  FLG-UKETUKEGAITO   
      *
           PERFORM UNTIL ( FLG-DBRC    NOT =   ZERO )
               OR        ( FLG-UKETUKEGAITO
                                       NOT =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  UKETUKE-REC
      *
      *        会計のまだの最初の受付を対象とする
               IF    ( UKE-KAIKEITIME  =   ZERO )
                   IF    ( SPA-GMN-I01-NYUINKA =   SPACE   )
                       MOVE    1               TO  FLG-UKETUKEGAITO
                       MOVE    UKE-SRYKA       TO  WRK-CMB-CD
                       PERFORM 4103-CMB-NYUINKA-SRH-SEC
                       MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-NYUINKA
                   END-IF
      *
                   IF    ( SPA-GMN-I01-DR (1)  =   SPACE   )
                       MOVE    UKE-DRCD (2 : 4)
                                               TO  WRK-CMB-CD
                       MOVE    1               TO  IDX1
                       PERFORM 4103-CMB-DR-SRH-SEC
                       MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-DR (1)
                   END-IF
               END-IF
      *
               MOVE    "UKETUKE-KEY2"          TO  WRK-DBPATH
               PERFORM 900-TBL-READ-SEC
           END-PERFORM
      *
           MOVE    "UKETUKE-KEY2"      TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
      *
       4101-UKETUKE-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院歴リスト編集処理
      *****************************************************************
       4101-NYUREKILST-EDIT-SEC        SECTION.
      *
      *    自院他院フラグ
           MOVE    ZERO            TO  SPA-NAI-I01-TZITA-FLG (IDX1)
      *
      *
      *    システム管理マスタ検索処理
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5000"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SPA-SYSYMD      TO  ORC-DBYMD
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY"  TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        =   ZERO )
               MOVE    MCPDATA-REC TO  SYS-5000-REC
           ELSE
               INITIALIZE              SYS-5000-REC
           END-IF
      *
      *    システム管理マスタクローズ
           MOVE    "SYSKANRI-KEY"  TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF    ( SYS-5000-REKI-DISPKBN   =   "2" )
               MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
               MOVE    "PTNYUINRRK-KEY4"   TO  WRK-DBPATH
               PERFORM 900-TBL-SELECT-SEC
               IF    ( FLG-DBRC        =   ZERO )
                   MOVE    MCPDATA-REC TO  PTNYUINRRK-REC

               END-IF
      *
      *        システム管理マスタクローズ
               MOVE    "PTNYUINRRK-KEY4"
                                       TO  WRK-DBPATH
               PERFORM 900-TBL-CLOSE-SEC
           END-IF
      *
      *    同日再入院フラグ
           PERFORM 800-ORCSDNTCHK-SEC
           MOVE    SDNTCHK-DJKBN   TO  SPA-NAI-I01-TDJKBN  (IDX1)
           MOVE    SDNTCHK-STYMD   TO  SPA-NAI-I01-TDJSTYMD(IDX1)
      *
      *    入院歴
           MOVE    PTNYUINRRK-RRKNUM
                                   TO  SPA-NAI-I01-TRRKNUM (IDX1)
      *    選択番号
           MOVE    IDX1            TO  SPA-GMN-I01-TSELNUM (IDX1)
      *
      *    初回区分
           MOVE    PTNYUINRRK-SHOKAIKBN
                                   TO  SPA-GMN-I01-TSHOKAI (IDX1)
      *
      *    初回番号
           MOVE    PTNYUINRRK-SHONUM
                                   TO  SPA-GMN-I01-TSHONUM (IDX1)
      *
      *    病棟
           INITIALIZE  SYSKANRI-REC
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    PTNYUINRRK-BTUNUM
                                   TO  SYS-KBNCD
           MOVE    PTNYUINRRK-TENNYUYMD
                                   TO  ORC-DBYMD
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY"  TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    =   ZERO )
               MOVE    MCPDATA-REC TO  SYS-5001-REC
               MOVE    SYS-5001-BTU-NAME
                                   TO  SPA-GMN-I01-TBTUNAME (IDX1)
           END-IF
           MOVE    "SYSKANRI-KEY"  TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
      *    病室番号
           IF    ( PTNYUINRRK-BRMNUM (3 : 6)
                                   IS  NUMERIC )
               MOVE    PTNYUINRRK-BRMNUM (3 : 6)
                                   TO   WRK-999999
               MOVE    WRK-999999
                                   TO   WRK-ZZZZZ9
               MOVE    WRK-ZZZZZ9
                                   TO  SPA-GMN-I01-TBRMNUM (IDX1)
           ELSE
               MOVE    PTNYUINRRK-BRMNUM ( 3 : 6 )
                                   TO  SPA-GMN-I01-TBRMNUM (IDX1)
           END-IF
      *
      *    入院日
           MOVE    PTNYUINRRK-NYUINYMD
                                   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-I01-TNYUINYMD (IDX1)
           MOVE    PTNYUINRRK-NYUINYMD
                                   TO  SPA-NAI-I01-TNYUINYMD (IDX1)
      *
      *    退院日
           IF    ( PTNYUINRRK-TAIINYMD   =   "99999999" )
               MOVE    SPACE   TO  SPA-GMN-I01-TTAIINYMD (IDX1)
                                   SPA-NAI-I01-TTAIINYMD (IDX1)
           ELSE
               MOVE    PTNYUINRRK-TAIINYMD
                                   TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG TO  SPA-GMN-I01-TTAIINYMD (IDX1)
               MOVE    PTNYUINRRK-TAIINYMD
                                   TO  SPA-NAI-I01-TTAIINYMD (IDX1)
           END-IF
      *
      *    診療科
           INITIALIZE  SYSKANRI-REC
           MOVE    "1005"          TO  SYS-KANRICD
           MOVE    PTNYUINRRK-NYUINKA
                                   TO  SYS-KBNCD
           MOVE    PTNYUINRRK-TENNYUYMD
                                   TO  ORC-DBYMD
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY"  TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    =   ZERO )
               MOVE    MCPDATA-REC TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME
                                   TO  SPA-GMN-I01-TNYUINKA (IDX1)
           END-IF
           MOVE    "SYSKANRI-KEY"  TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
      *------------
           MOVE    ZERO            TO  WRK-REKI-BYO-ZITA-FLG (IDX1)
           MOVE    PTNYUINRRK-NYUINYMD
                                   TO  WRK-REKI-BYO-NYUINYMD  (IDX1)
      *
           IF    ( PTNYUINRRK-TAIINYMD   =   "99999999" )
               MOVE    "99999999"
                                   TO  WRK-REKI-BYO-TAIINYMD (IDX1)
           ELSE
               MOVE    PTNYUINRRK-TAIINYMD
                                   TO  WRK-REKI-BYO-TAIINYMD (IDX1)
           END-IF
      *
           MOVE    PTNYUINRRK-NYUINKA
                                   TO  WRK-REKI-BYO-NYUINKA (IDX1)
      *
           .
      *
       4101-NYUREKILST-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院歴リスト編集処理(入院歴を編集)
      *****************************************************************
       4101-NYUREKILST-EDIT2-SEC       SECTION.
      *
      *    自院他院フラグ
           IF    ( PTNYUINRRK-JTIKBN   =   "5" )
               MOVE    1           TO  SPA-NAI-I01-TZITA-FLG (IDX1)
           ELSE
               MOVE    2           TO  SPA-NAI-I01-TZITA-FLG (IDX1)
           END-IF
      *
      *    同日再入院フラグ
           PERFORM 800-ORCSDNTCHK-SEC
           MOVE    SDNTCHK-DJKBN   TO  SPA-NAI-I01-TDJKBN  (IDX1)
           MOVE    SDNTCHK-STYMD   TO  SPA-NAI-I01-TDJSTYMD(IDX1)
      *
      *    入院歴
           MOVE    PTNYUINRRK-RRKNUM
                                   TO  SPA-NAI-I01-TRRKNUM (IDX1)
      *    選択番号
           MOVE    IDX1            TO  SPA-GMN-I01-TSELNUM (IDX1)
      *
      *    初回区分
           MOVE    PTNYUINRRK-SHOKAIKBN
                                   TO  SPA-GMN-I01-TSHOKAI (IDX1)
      *
      *    初回番号
           MOVE    PTNYUINRRK-SHONUM
                                   TO  SPA-GMN-I01-TSHONUM (IDX1)
      *
      *    病棟
           MOVE    PTNYUINRRK-BTUNAME
                                   TO  SPA-GMN-I01-TBTUNAME (IDX1)
      *
      *    病室番号（自院歴／他院歴）を表示
           EVALUATE    PTNYUINRRK-JTIKBN
           WHEN    "5"
               MOVE    "他院歴"    TO  SPA-GMN-I01-TBRMNUM (IDX1)
           WHEN    "6"
               MOVE    "自院歴"    TO  SPA-GMN-I01-TBRMNUM (IDX1)
           END-EVALUATE
      *
      *    入院日
           MOVE    PTNYUINRRK-NYUINYMD
                                   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-I01-TNYUINYMD (IDX1)
           MOVE    PTNYUINRRK-NYUINYMD
                                   TO  SPA-NAI-I01-TNYUINYMD (IDX1)
      *
      *    退院日（最新の情報を取得）
           IF    ( PTNYUINRRK-TAIINYMD     =   "99999999" )
               MOVE    SPACE       TO  SPA-GMN-I01-TTAIINYMD (IDX1)
                                       SPA-NAI-I01-TTAIINYMD (IDX1)
           ELSE
               MOVE    PTNYUINRRK-TAIINYMD
                                   TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG TO  SPA-GMN-I01-TTAIINYMD (IDX1)
      *
               MOVE    PTNYUINRRK-TAIINYMD
                                   TO  SPA-NAI-I01-TTAIINYMD (IDX1)
           END-IF
      *
           IF    ( PTNYUINRRK-JTIKBN       =   "6" )
            AND  ( PTNYUINRRK-NYUINCHUKBN  =   "1" )
               MOVE    "*"         TO  SPA-GMN-I01-TTAIINYMD (IDX1)
                                                             (10:)
           END-IF
      *
      *    通算対象日数
           MOVE    PTNYUINRRK-TAISYONISSU
                                   TO  SPA-NAI-I01-TNYURRKTAISYO (IDX1)
      *
      *    病名
           INITIALIZE  PTTAINFUKA-REC
           MOVE    SPA-HOSPID      TO  PTTAINFUKA-HOSPID
           MOVE    PTNYUINRRK-PTID
                                   TO  PTTAINFUKA-PTID
           MOVE    PTNYUINRRK-RRKNUM
                                   TO  PTTAINFUKA-RRKNUM
           MOVE    PTTAINFUKA-REC  TO  MCPDATA-REC
           MOVE    "PTTAINFUKA-KEY"
                                   TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        =   ZERO )
               MOVE    MCPDATA-REC TO  PTTAINFUKA-REC
               MOVE    PTTAINFUKA-BYOMEI (1)
                                   TO  WRK-REKI-BYO-BYOMEI (IDX1)
           END-IF
      *
           MOVE    "PTTAINFUKA-KEY"
                                   TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           MOVE    1               TO  WRK-REKI-BYO-ZITA-FLG (IDX1)
           MOVE    SPACE           TO  WRK-REKI-BYO-NYUINYMD  (IDX1)
                                       WRK-REKI-BYO-TAIINYMD (IDX1)
                                       WRK-REKI-BYO-TAIINYMD  (IDX1)
                                       WRK-REKI-BYO-NYUINKA (IDX1)
           .
      *
       4101-NYUREKILST-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    主病名編集処理
      *****************************************************************
       4101-SYUBYOMEI-EDIT-SEC        SECTION.
      *
      *    患者病名マスタ検索
           INITIALIZE  PTBYOMEI-REC
           MOVE    SPA-HOSPID          TO  PTBYO-HOSPID
           MOVE    SPA-NAI-I01-PTID    TO  PTBYO-PTID
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "PTBYOMEI-KEY3"     TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
      *
           PERFORM UNTIL ( FLG-DBRC    NOT =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC
      *
      *        入院病名のみを対象とする
               IF    ( PTBYO-NYUGAIKBN =   "1" )
      *            対象の主病名をワークエリアに退避する
                   PERFORM 4101-SYUBYOMEI-TAIHI-SEC
               END-IF
      *
      *        患者病名マスタ読み込み
               MOVE    "PTBYOMEI-KEY3" TO  WRK-DBPATH
               PERFORM 900-TBL-READ-SEC
      *
           END-PERFORM
      *
      *    患者病名マスタクローズ
           MOVE    "PTBYOMEI-KEY3"     TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
      *    入院歴リストの主病名の編集を行う
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I01-NYUREKILST-CNT )
      *
               IF    ( WRK-REKI-BYO-BYOMEI (IDX1)  NOT =   SPACE )
                   MOVE    WRK-REKI-BYO-BYOMEI (IDX1)
                                       TO  SPA-GMN-I01-TBYOMEI (IDX1)
                   EVALUATE    WRK-REKI-BYO-TENKIKBN (IDX1)
                   WHEN "1"
                       MOVE    "治ゆ"  TO  SPA-GMN-I01-TTENKI (IDX1)
                   WHEN "2"
                       MOVE    "死亡"  TO  SPA-GMN-I01-TTENKI (IDX1)
                   WHEN "3"
                       MOVE    "中止"  TO  SPA-GMN-I01-TTENKI (IDX1)
                   WHEN "4"
                       MOVE    "移行"  TO  SPA-GMN-I01-TTENKI (IDX1)
                   END-EVALUATE
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4101-SYUBYOMEI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    主病名退避処理
      *****************************************************************
       4101-SYUBYOMEI-TAIHI-SEC        SECTION.
      *
      *    入院時の診療科で、入院期間中に診療開始日がある
      *    主病名で、日付が最も古いものを取得する
           SET     WRK-RRKBYO-IDX  TO  1
           SEARCH  WRK-REKI-BYOMEI-EDIT-OCC
               VARYING WRK-RRKBYO-IDX
           WHEN  ( WRK-RRKBYO-IDX  >  SPA-GMN-I01-NYUREKILST-CNT )
               CONTINUE
           WHEN  ( WRK-REKI-BYO-ZITA-FLG (WRK-RRKBYO-IDX)
                                   =  ZERO             )
             AND ( WRK-REKI-BYO-NYUINYMD (WRK-RRKBYO-IDX)
                                  <=   PTBYO-SRYYMD    )
             AND ( WRK-REKI-BYO-TAIINYMD (WRK-RRKBYO-IDX)
                                  >=   PTBYO-SRYYMD    )
             AND ( WRK-REKI-BYO-NYUINKA  (WRK-RRKBYO-IDX)
                                   =   PTBYO-SRYKA     )
             AND ( PTBYO-SYUBYOFLG =   "1"             )
      *
               MOVE    ZERO        TO  FLG-TAISYO
               EVALUATE    TRUE
               WHEN    ( WRK-REKI-BYO-SRYYMD (WRK-RRKBYO-IDX)
                                   = SPACE )
                   MOVE    1       TO  FLG-TAISYO
               WHEN    ( WRK-REKI-BYO-SRYYMD (WRK-RRKBYO-IDX)
                                   >   PTBYO-SRYYMD    )
                   MOVE    1       TO  FLG-TAISYO
               WHEN    ( WRK-REKI-BYO-SRYYMD (WRK-RRKBYO-IDX)
                                   =   PTBYO-SRYYMD    )
      *            同一日の場合は連番の小さい方を取得
                   IF  ( WRK-REKI-BYO-RENNUM (WRK-RRKBYO-IDX)
                                   >   PTBYO-RENNUM )
                       MOVE    1       TO  FLG-TAISYO
                   END-IF
      *
               END-EVALUATE
      *
      *
               IF    ( FLG-TAISYO  =   "1" )
                   MOVE    PTBYO-RENNUM
                       TO  WRK-REKI-BYO-RENNUM (WRK-RRKBYO-IDX)
                   MOVE    PTBYO-SRYYMD
                       TO  WRK-REKI-BYO-SRYYMD (WRK-RRKBYO-IDX)
                   MOVE    PTBYO-BYOMEI
                       TO  WRK-REKI-BYO-BYOMEI (WRK-RRKBYO-IDX)
                   MOVE    PTBYO-TENKIKBN
                       TO  WRK-REKI-BYO-TENKIKBN (WRK-RRKBYO-IDX)
               END-IF
      *
           END-SEARCH
           .
      *
       4101-SYUBYOMEI-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    日数編集処理処理
      *****************************************************************
       4101-NISSU-EDIT-SEC             SECTION.
      *
           PERFORM VARYING IDX1
                   FROM    SPA-GMN-I01-NYUREKILST-CNT
                   BY      -1
                   UNTIL ( IDX1    <   1 )
      *
               IF    ( SPA-GMN-I01-TSHOKAI (IDX1)  =   "1" )
                   MOVE    ZERO    TO  WRK-TSUSAN
                   MOVE    SPA-GMN-I01-TSHONUM (IDX1)
                                   TO  WRK-CHK-SHONUM
                   PERFORM VARYING IDX2    FROM    IDX1    BY  -1
                           UNTIL ( IDX2    <   1 )
      *
                       PERFORM 4101-NISSU-EDIT2-SEC
      *
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           IF    ( SPA-GMN-I01-NYUREKILST-CNT  <= ZERO )
               GO  TO  4101-NISSU-EDIT-EXT
           END-IF
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPID          TO  PTNYUINRRK-HOSPID
           MOVE    SPA-NAI-I01-PTID
                                       TO  PTNYUINRRK-PTID
           MOVE    SPA-NAI-I01-TNYUINYMD (SPA-GMN-I01-NYUREKILST-CNT)
                                       TO  PTNYUINRRK-NYUINYMD
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "PTNYUINRRK-KEY31"  TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-DBRC    NOT =   ZERO )
      *
               PERFORM 800-ORCSDNTCHK-SEC
      *
               MOVE    ZERO            TO  WRK-TSUSAN
               MOVE    PTNYUINRRK-SHONUM
                                       TO  WRK-CHK-SHONUM
      *
               PERFORM UNTIL ( PTNYUINRRK-SHONUM
                                           NOT =   WRK-CHK-SHONUM )
                        OR   ( FLG-DBRC    NOT =   ZERO )
      *
                   IF    ( PTNYUINRRK-JTIKBN   =   "5" )
                           COMPUTE WRK-TSUSAN
                               =   WRK-TSUSAN
                               +   PTNYUINRRK-TAISYONISSU
                   ELSE
                       IF    ( SDNTCHK-STYMD   NOT =   ZERO )
                           MOVE    SDNTCHK-STYMD
                                       TO  WRK-STYMD
                           MOVE    PTNYUINRRK-TAIINYMD
                                       TO  WRK-EDYMD
                           PERFORM 5002-KIKAN-CALC-SEC
                           IF    ( FLG-ORCSDAY =   ZERO )
                               COMPUTE WRK-TSUSAN
                                   =   WRK-TSUSAN
                                   +   WRK-NISSU2
                           END-IF
                       END-IF
                   END-IF
      *
                   MOVE    "PTNYUINRRK-KEY31"  TO  WRK-DBPATH
                   PERFORM 900-TBL-READ-SEC
                   IF    ( FLG-DBRC        =   ZERO )
                       MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
                   ELSE
                       INITIALIZE                  PTNYUINRRK-REC
                   END-IF
      *
               END-PERFORM
      *
               PERFORM VARYING IDX2    
                       FROM    SPA-GMN-I01-NYUREKILST-CNT
                       BY      -1
                       UNTIL ( IDX2    <   1 )
      *
                   PERFORM 4101-NISSU-EDIT2-SEC
      *
               END-PERFORM
      *
           END-PERFORM
      *
           MOVE    "PTNYUINRRK-KEY31"  TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
       4101-NISSU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    日数編集処理処理（対象チェック）
      *****************************************************************
       4101-NISSU-EDIT2-SEC            SECTION.
      *
           IF    ( WRK-CHK-SHONUM  NOT =   SPA-GMN-I01-TSHONUM (IDX2) )
               GO  TO  4101-NISSU-EDIT2-EXT
           END-IF
      *
           IF    ( IDX2    =   1 )
             AND ( SPA-NAI-I01-TNYUINYMD (IDX2)
                               =   SPACE )
               GO  TO  4101-NISSU-EDIT2-EXT
           END-IF
      *
           MOVE    SPA-NAI-I01-TNYUINYMD (IDX2)
                           TO  WRK-STYMD
           MOVE    SPA-NAI-I01-TTAIINYMD (IDX2)
                           TO  WRK-EDYMD
           PERFORM 5002-KIKAN-CALC-SEC
           IF    ( FLG-ORCSDAY =   ZERO )
               IF    ( SPA-NAI-I01-TZITA-FLG (IDX2)
                               =   ZERO )
                   MOVE    WRK-NISSU2
                               TO  WRK-NISSU
      *
                   IF    ( SPA-NAI-I01-TDJKBN (IDX2)   =   SPACE )
                       COMPUTE WRK-TSUSAN
                           =   WRK-TSUSAN  +   WRK-NISSU2
                   ELSE
                       COMPUTE WRK-TSUSAN
                           =   WRK-TSUSAN  +   WRK-NISSU2  -   1
                   END-IF
      *
               ELSE
                   MOVE    SPA-NAI-I01-TNYURRKTAISYO (IDX2)
                               TO  WRK-NISSU
                   COMPUTE WRK-TSUSAN
                       =   WRK-TSUSAN
                       +   SPA-NAI-I01-TNYURRKTAISYO (IDX2)
      *
               END-IF
               MOVE    WRK-NISSU   TO  WRK-ZZZZ9
               MOVE    WRK-ZZZZ9   TO  SPA-GMN-I01-TNISSU (IDX2)
               MOVE    WRK-TSUSAN  TO  WRK-ZZZZ9
               MOVE    WRK-ZZZZ9   TO  SPA-GMN-I01-TTUSAN (IDX2)
           END-IF
      *
           .
       4101-NISSU-EDIT2-EXT.
           EXIT.
      *****************************************************************
      *    年齢算出処理
      *****************************************************************
       4101-AGE-CALC-SEC               SECTION.
      *
           IF  (   WRK-BIRTHDAY    <   SPA-SYSYMD )
               MOVE    WRK-BIRTHDAY    TO  WRK-CALC-YMD1
               MOVE    SPA-SYSYMD      TO  WRK-CALC-YMD2
           ELSE
               MOVE    SPA-SYSYMD      TO  WRK-CALC-YMD1
               MOVE    WRK-BIRTHDAY    TO  WRK-CALC-YMD2
           END-IF
      *
           COMPUTE WRK-AGE-YY          =   WRK-CALC-YY2
                                       -   WRK-CALC-YY1
           IF    ( WRK-CALC-YMD1(5:4)  >   WRK-CALC-YMD2(5:4) )
               COMPUTE WRK-AGE-YY      =   WRK-AGE-YY
                                       -   1
               COMPUTE WRK-AGE-MM      =  (WRK-CALC-MM2    +   12 )
                                       -   WRK-CALC-MM1
           ELSE
               COMPUTE WRK-AGE-MM      =   WRK-CALC-MM2
                                       -   WRK-CALC-MM1
           END-IF
      *
           IF    ( WRK-CALC-DD1        >   WRK-CALC-DD2 )
               COMPUTE WRK-AGE-MM      =   WRK-AGE-MM
                                       -   1
           END-IF
      *
           IF  (   WRK-BIRTHDAY    <   SPA-SYSYMD )
               CONTINUE
           ELSE
               COMPUTE WRK-AGE-YY  =   WRK-AGE-YY
                                   *   -1
               COMPUTE WRK-AGE-MM  =   WRK-AGE-MM
                                   *   -1
           END-IF
      *
           IF    ( WRK-AGE-YY          =   ZERO )
               MOVE    WRK-AGE-MM      TO  WRK-AGE-Z
               MOVE    "ヶ月"          TO  WRK-AGE-MEI
           ELSE
               MOVE    WRK-AGE-YY      TO  WRK-AGE-Z
               MOVE    "才"            TO  WRK-AGE-MEI
           END-IF
      *
           .
       4101-AGE-CALC-EXT.
           EXIT.
      *****************************************************************
      *    保険組み合わせコンボ設定処理
      *****************************************************************
       4101-CMB-HKNCOMBI-SET-SEC       SECTION.
      *
           INITIALIZE                  SPA-GMN-I01-CMB-HKNCOMBI
                                       SPA-KYOTUKEY
                                       SPA-KYOTU-SET
                                       ORCSHKNSETAREA
      *
           MOVE    WRK-KJNYMD          TO  SPA-SRYYMD
           MOVE    SPA-GMN-I01-NYUINKA-CD
                                       TO  SPA-SRYKA
           MOVE    "1"                 TO  SPA-NYUGAIKBN
           MOVE    SPA-NAI-I01-PTID    TO  SPA-PTID
           MOVE    SPA-NAI-I01-BIRTHDAY
                                       TO  SPA-BIRTHDAY
           MOVE    SPA-GMN-I01-HKNCOMBI-CD
                                       TO  SPA-HKNCOMBINUM
           MOVE    "1"                 TO  ORCSHKN-KBN
      *
           CALL    "ORCSHKNSET"        USING   SPA-AREA
                                               ORCSHKNSETAREA
           IF    ( ORCSHKN-ERRCD   NOT =   ZERO )
               GO  TO  4101-CMB-HKNCOMBI-SET-EXT
           END-IF
      *
           IF    ( ORCSHKN-HKN-MAX     >   30 )
               MOVE    30              TO  SPA-GMN-I01-HKNCOMBI-CNT
           ELSE
               MOVE    ORCSHKN-HKN-MAX TO  SPA-GMN-I01-HKNCOMBI-CNT
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-I01-HKNCOMBI-CNT )

               MOVE    ORCSHKN-GMN-HKNCOMBINUM (IDX1)
                                       TO  SPA-GMN-I01-HKNCOMBI-ITM
                                                       (IDX1)(1 : 4)
               MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX1)
                                       TO  SPA-GMN-I01-HKNCOMBI-ITM
                                                       (IDX1)(6 : )
               MOVE    ORCSHKN-GMN-FTNRATEMEI  (IDX1)
                                       TO  SPA-GMN-I01-HKNCOMBI-ITM2
                                                       (IDX1)
               MOVE    ORCSHKN-NAI-RSI-HKNKBN (IDX1)
                                       TO  SPA-GMN-I01-HKNCOMBI-ITM3
                                                       (IDX1)
      *
           END-PERFORM
      *
           IF    ( ORCSHKN-IDX    >=   1 )
            AND  ( ORCSHKN-IDX    <=   SPA-GMN-I01-HKNCOMBI-CNT )
               MOVE    SPA-GMN-I01-HKNCOMBI-ITM (ORCSHKN-IDX)
                                       TO  SPA-GMN-I01-HKNCOMBI
               MOVE    SPA-GMN-I01-HKNCOMBI-ITM2 (ORCSHKN-IDX)
                                       TO  SPA-NAI-I01-FTNRATE
               MOVE    SPA-GMN-I01-HKNCOMBI-ITM3 (ORCSHKN-IDX)
                                       TO  SPA-NAI-I01-RSI-HKNKBN
           END-IF
      *
           .
       4101-CMB-HKNCOMBI-SET-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴検索処理
      *****************************************************************
       4101-JYURRK-SEL-SEC           SECTION.
      *
           MOVE    SPACE           TO  WKJYURRK-REC
      *
      *    受診履歴検索
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPID      TO  JYURRK-HOSPID
           MOVE    SPA-NAI-I01-PTID
                                   TO  JYURRK-PTID
           MOVE    WRK-KJNYMD      TO  JYURRK-SRYYMD
           MOVE    JYURRK-REC      TO  MCPDATA-REC
           MOVE    "JYURRK-KEY10"  TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
      *
           PERFORM
               UNTIL ( FLG-DBRC        NOT =   ZERO )
                OR   ( WKJYURRK-REC    NOT =   SPACE )
      *
               MOVE    MCPDATA-REC TO  JYURRK-REC  
      *
      *        最後の受診履歴を対象とする（診療日以前のもの）
               IF    ((SPA-GMN-I01-NYUINKA     NOT =   SPACE  )
                  AND (SPA-GMN-I01-NYUINKA-CD      =   JYURRK-SRYKA ) )
                OR    ( SPA-GMN-I01-NYUINKA        =   SPACE        )
                   MOVE    JYURRK-REC  TO  WKJYURRK-REC
               END-IF
      *
      *        受診履歴読込
               MOVE    "JYURRK-KEY10"      TO  WRK-DBPATH
               PERFORM 900-TBL-READ-SEC
      *
           END-PERFORM
      *
      *    受診履歴クローズ
           MOVE    "JYURRK-KEY10"      TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF    ( WKJYURRK-REC        =   SPACE )
               GO  TO  4101-JYURRK-SEL-EXT
           END-IF
      *
           MOVE    WKJYURRK-REC        TO  JYURRK-REC 
      *
      *    入院科
           IF    ( SPA-GMN-I01-NYUINKA =   SPACE )
               MOVE    JYURRK-SRYKA    TO  WRK-CMB-CD
               PERFORM 4103-CMB-NYUINKA-SRH-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-NYUINKA
           END-IF
      *
      *    主治医
           IF    ( SPA-GMN-I01-DR (1)  =   SPACE )
               MOVE    JYURRK-DRCD (2 : 4) 
                                       TO  WRK-CMB-CD
               MOVE    1               TO  IDX1
               PERFORM 4103-CMB-DR-SRH-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-DR (1)
           END-IF
      *
           .
       4101-JYURRK-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理区分チェック処理
      *****************************************************************
       4101-SYORIKBN-CHK-SEC           SECTION.
      *
           MOVE    2               TO  SPA-GMN-I01-CUR
      *
           MOVE    SPACE           TO  SPA-GMN-I01-ERRCD (2)
      *
           MOVE    I01-SYORIKBN    TO  SPA-GMN-I01-SYORIKBN
      *
           IF    ( SPA-GMN-I01-SYORIKBN    =   SPACE )
               MOVE    "1002"      TO  SPA-ERRCD
               MOVE    SPA-ERRCD   TO  SPA-GMN-I01-ERRCD (2)
               GO  TO  4101-SYORIKBN-CHK-EXT
           END-IF
      *
      *    処理区分サーチ
           MOVE    SPA-GMN-I01-SYORIKBN-CD
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-SYORIKBN-SRH-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SYORIKBN
           ELSE
               MOVE    "0012"      TO  SPA-ERRCD
               MOVE    SPA-ERRCD           TO  SPA-GMN-I01-ERRCD (2)
               MOVE    2           TO  SPA-GMN-I01-CUR
               GO  TO  4101-SYORIKBN-CHK-EXT
           END-IF
      *
           MOVE    SPACE           TO  SPA-GMN-I01-IDOYMD
                                       SPA-NAI-I01-IDOYMD
                                       SPA-NAI-I01-IDOYMD-MAE
      *
      *    入院歴ボタンの状態を退避
           MOVE    SPA-GMN-I01-B07-STT
                                   TO  WRK-WIDGET-B07-STT
           MOVE    SPA-GMN-I01-B08-STT
                                   TO  WRK-WIDGET-B08-STT
      *
      *    コラムリストの選択位置を退避
           MOVE    SPA-GMN-I01-NYUREKILST-SEL
                                   TO  WRK-NYUREKILST-SEL
      *
           INITIALIZE              SPA-GMN-I01-INPUT-AREA
                                   SPA-NAI-I01-INPUT-AREA
      *
      *    入院履歴マスタ検索処理（入力領域表示用）
           PERFORM 4101-PTNYUINRRK-SEL1-SEC
           IF    ( FLG-PTNYUINRRK  =   ZERO  )
      *        入力領域編集処理
               IF    ( PTNYUINRRK-JTIKBN   =   "5" OR "6" )
                   PERFORM 4101-INPUT-AREA-EDIT2-SEC
               ELSE
                   PERFORM 4101-INPUT-AREA-EDIT-SEC
               END-IF
           END-IF
      *
           IF    ( FLG-PTNYUINRRK  NOT =   ZERO  )
            OR   ( SPA-GMN-I01-SYORIKBN-CD =   "01" )    
               PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *        保険組み合わせコンボ設定処理
               MOVE    SPA-SYSYMD      TO  WRK-KJNYMD
               PERFORM 4101-CMB-HKNCOMBI-SET-SEC
           END-IF
      *
      *    処理区分応じて画面編集の内容を変更する
           EVALUATE    SPA-GMN-I01-SYORIKBN-CD
      *    入院登録
           WHEN    "01"
      *
      *        コンボボックス初期処理
               MOVE    SPA-SYSYMD      TO  WRK-KJNYMD
               PERFORM 310-CMB-SYOKI-SEC
      *
      *        受付マスタより入院科等を確定する
               MOVE    SPA-SYSYMD      TO  WRK-KJNYMD
               PERFORM 4101-UKETUKE-SEL-SEC
      *        受診履歴検索処理
               MOVE    SPA-SYSYMD      TO  WRK-KJNYMD
               PERFORM 4101-JYURRK-SEL-SEC
               PERFORM 4101-WIDGET-NYUIN-STT-SEC
      *    退院登録
           WHEN    "02"
           WHEN    "11"
      *
      *        退院日にシステム日付か前回異動日で日付の新しい方を表示
               IF    ( SPA-SYSYMD      >   SPA-NAI-I01-MAEIDOYMD )
                   MOVE    SPA-SYSYMD      TO  WRK-YMD
               ELSE
                   MOVE    SPA-NAI-I01-MAEIDOYMD
                                           TO  WRK-YMD
               END-IF
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-I01-TAIINYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-I01-TAIINYMD
      *        コンボボックス初期処理
               MOVE    SPA-NAI-I01-TAIINYMD
                                       TO  WRK-KJNYMD
               PERFORM 310-CMB-SYOKI-SEC
      *
               PERFORM 4101-WIDGET-TAIIN-STT-SEC
      *
      *
      *    変更
           WHEN    "03"
               PERFORM 4101-WIDGET-HENKOU-STT-SEC
      *    照会
           WHEN    "04"
               PERFORM 4201-WIDGET-INSENSITIVE-SEC
      *    転科　転棟　転室
           WHEN    "08"
      *
      *        異動日にシステム日付か前回異動日で日付の新しい方を表示
               IF    ( SPA-SYSYMD      >   SPA-NAI-I01-MAEIDOYMD )
                   MOVE    SPA-SYSYMD      TO  WRK-YMD
               ELSE
                   MOVE    SPA-NAI-I01-MAEIDOYMD
                                           TO  WRK-YMD
               END-IF
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-I01-IDOYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-I01-IDOYMD
                                           SPA-NAI-I01-IDOYMD-MAE
      *
      *        コンボボックス初期処理
               MOVE    SPA-NAI-I01-IDOYMD
                                       TO  WRK-KJNYMD
               PERFORM 310-CMB-SYOKI-SEC
      *
      *        保険組み合わせコンボ設定処理
               MOVE    SPA-NAI-I01-IDOYMD
                                       TO  WRK-KJNYMD
               PERFORM 4101-CMB-HKNCOMBI-SET-SEC
      *
      *        受付マスタより入院科等を確定する
               MOVE    SPA-SYSYMD      TO  WRK-KJNYMD
               PERFORM 4101-UKETUKE-SEL-SEC
      *        受診履歴検索処理
               MOVE    SPA-NAI-I01-IDOYMD
                                       TO  WRK-KJNYMD
               PERFORM 4101-JYURRK-SEL-SEC
      *
               IF    ( SPA-NAI-I01-NYURRK-FLG   = ZERO )
                   PERFORM 4102-KIHON-CHK-SEC
               END-IF
      *
               PERFORM 4101-WIDGET-TENKA-STT-SEC
      *
      *    照会
           WHEN    "10"
               PERFORM 4201-WIDGET-INSENSITIVE-SEC
               MOVE    WIDGET-NORMAL   TO  SPA-GMN-I01-B12-STT
      *    その他
           WHEN    OTHER
               PERFORM 4201-WIDGET-INSENSITIVE-SEC
               MOVE    WIDGET-NORMAL   TO  SPA-GMN-I01-B12-STT
           END-EVALUATE
      *
      *    入院歴ボタンの状態を元にもどす
           MOVE    WRK-WIDGET-B07-STT  TO  SPA-GMN-I01-B07-STT
           MOVE    WRK-WIDGET-B08-STT  TO  SPA-GMN-I01-B08-STT
      *
           IF    ( SPA-GMN-I01-SYORIKBN-CD     =   "10" )
               MOVE    WRK-NYUREKILST-SEL
                                       TO  SPA-GMN-I01-NYUREKILST-SEL
                                           SPA-GMN-I01-SELNUM
           ELSE
               IF    ( SPA-GMN-I01-NYUREKILST-CNT  >   ZERO )
                   MOVE    1           TO  SPA-GMN-I01-NYUREKILST-SEL
                                           SPA-GMN-I01-SELNUM
               END-IF
           END-IF
      *
      *    保険組合せが存在していること
           IF    ( SPA-GMN-I01-SYORIKBN-CD     =   "01" )
               IF    ( SPA-GMN-I01-HKNCOMBI-CNT   >   ZERO )
                   CONTINUE
               ELSE
                   MOVE    "1021"      TO  SPA-ERRCD
                   MOVE    1           TO  SPA-GMN-I01-CUR
                   GO  TO  4101-SYORIKBN-CHK-EXT
               END-IF
           END-IF
      *
           EVALUATE SPA-GMN-I01-SYORIKBN-CD
           WHEN    "04"
               MOVE    2               TO  SPA-GMN-I01-CUR
           WHEN    "10"
               MOVE    100             TO  SPA-GMN-I01-CUR
           WHEN    OTHER
               MOVE    4               TO  SPA-GMN-I01-CUR
           END-EVALUATE
      *
           .
      *
       4101-SYORIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院登録時入力可否設定処理
      *****************************************************************
       4101-WIDGET-NYUIN-STT-SEC       SECTION.
      *
           PERFORM 4201-WIDGET-STATE-CLEAR-SEC
      *
           MOVE    WIDGET-INSENSITIVE
                                   TO
                                       SPA-GMN-I01-IDOYMD-STT
                                       SPA-GMN-I01-TAIINYMD-STT
                                       SPA-GMN-I01-TAIINCD-STT
                                       SPA-GMN-I01-TAIINCD-ITM-STT
                                       SPA-GMN-I01-SHONUM-STT
      *
           .
      *
       4101-WIDGET-NYUIN-STT-EXT.
           EXIT.
      *****************************************************************
      *    退院登録時入力可否設定処理
      *****************************************************************
       4101-WIDGET-TAIIN-STT-SEC       SECTION.
      *
           PERFORM 4201-WIDGET-STATE-CLEAR-SEC
      *
           MOVE    WIDGET-INSENSITIVE
                                   TO
                                       SPA-GMN-I01-B07-STT
      *                                SPA-GMN-I01-B08-STT
                                       SPA-GMN-I01-IDOYMD-STT
                                       SPA-GMN-I01-BRMNUM-STT
                                       SPA-GMN-I01-BTUNAME-STT
                                       SPA-GMN-I01-BTUNAME-ITM-STT
                                       SPA-GMN-I01-SAGAKU-STT
                                       SPA-GMN-I01-SAGAKU-ITM-STT
                                       SPA-GMN-I01-NYUINYMD-STT
                                       SPA-GMN-I01-NYUINKA-STT
                                       SPA-GMN-I01-NYUINKA-ITM-STT
                                       SPA-GMN-I01-SHOKAI-STT
                                       SPA-GMN-I01-SHOKAI-ITM-STT
                                       SPA-GMN-I01-SHONUM-STT
                                       SPA-GMN-I01-DR-STT (1)
                                       SPA-GMN-I01-DR-ITM-STT (1)
                                       SPA-GMN-I01-DR-STT (2)
                                       SPA-GMN-I01-DR-ITM-STT (2)
                                       SPA-GMN-I01-DR-STT (3)
                                       SPA-GMN-I01-DR-ITM-STT (3)
                                       SPA-GMN-I01-HKNCOMBI-STT
                                       SPA-GMN-I01-HKNCOMBI-ITM-STT
                                       SPA-GMN-I01-TOKNYUIN-STT
                                       SPA-GMN-I01-TOKNYUIN-ITM-STT
                                       SPA-GMN-I01-SNTNYUIN-STT
                                       SPA-GMN-I01-MAEUKE-STT
                                       SPA-GMN-I01-SEIKYU-STT
                                       SPA-GMN-I01-SEIKYU-ITM-STT
                                       SPA-GMN-I01-DISPKBN-STT
                                       SPA-GMN-I01-DISPKBN-ITM-STT
      *
           .
      *
       4101-WIDGET-TAIIN-STT-EXT.
           EXIT.
      *****************************************************************
      *    変更時入力可否設定処理
      *****************************************************************
       4101-WIDGET-HENKOU-STT-SEC       SECTION.
      *
           PERFORM 4201-WIDGET-STATE-CLEAR-SEC
      *
           MOVE    WIDGET-INSENSITIVE
                                   TO  SPA-GMN-I01-B07-STT
      *                                SPA-GMN-I01-B08-STT
                                       SPA-GMN-I01-IDOYMD-STT
                                       SPA-GMN-I01-BRMNUM-STT
                                       SPA-GMN-I01-BTUNAME-STT
                                       SPA-GMN-I01-BTUNAME-ITM-STT
                                       SPA-GMN-I01-SAGAKU-STT
                                       SPA-GMN-I01-SAGAKU-ITM-STT
                                       SPA-GMN-I01-NYUINYMD-STT
                                       SPA-GMN-I01-NYUINKA-STT
                                       SPA-GMN-I01-NYUINKA-ITM-STT
                                       SPA-GMN-I01-SHOKAI-STT
                                       SPA-GMN-I01-SHOKAI-ITM-STT
                                       SPA-GMN-I01-SHONUM-STT
                                       SPA-GMN-I01-TAIINYMD-STT
                                       SPA-GMN-I01-TAIINCD-STT
                                       SPA-GMN-I01-TAIINCD-ITM-STT
                                       SPA-GMN-I01-HKNCOMBI-STT
                                       SPA-GMN-I01-HKNCOMBI-ITM-STT
                                       SPA-GMN-I01-TOKNYUIN-STT
                                       SPA-GMN-I01-TOKNYUIN-ITM-STT
                                       SPA-GMN-I01-SNTNYUIN-STT
      *
           .
      *
       4101-WIDGET-TAIIN-STT-EXT.
           EXIT.
      *****************************************************************
      *    転科・転棟・転室時入力可否設定処理
      *****************************************************************
       4101-WIDGET-TENKA-STT-SEC       SECTION.
      *
           PERFORM 4201-WIDGET-STATE-CLEAR-SEC
      *
           MOVE    WIDGET-INSENSITIVE
                                   TO
                                       SPA-GMN-I01-B07-STT
      *                                SPA-GMN-I01-B08-STT
                                       SPA-GMN-I01-NYUINYMD-STT
                                       SPA-GMN-I01-SHOKAI-STT
                                       SPA-GMN-I01-SHOKAI-ITM-STT
                                       SPA-GMN-I01-SHONUM-STT
                                       SPA-GMN-I01-TAIINYMD-STT
                                       SPA-GMN-I01-TAIINCD-STT
                                       SPA-GMN-I01-TAIINCD-ITM-STT
                                       SPA-GMN-I01-MAEUKE-STT
                                       SPA-GMN-I01-SEIKYU-STT
                                       SPA-GMN-I01-SEIKYU-ITM-STT
                                       SPA-GMN-I01-DISPKBN-STT
                                       SPA-GMN-I01-DISPKBN-ITM-STT
      *
           .
      *
       4101-WIDGET-TENKA-STT-EXT.
           EXIT.
      *****************************************************************
      *    異動日チェック処理
      *****************************************************************
       4101-IDOYMD-CHK-SEC             SECTION.
      *
           MOVE    SPA-NAI-I01-IDOYMD
                                       TO  WRK-IDOYMD
      *
           IF    ( SPA-GMN-I01-IDOYMD  =   SPACE )
               IF    ( SPA-SYSYMD      >   SPA-NAI-I01-MAEIDOYMD )
                   MOVE    SPA-SYSYMD      TO  WRK-YMD
               ELSE
                   MOVE    SPA-NAI-I01-MAEIDOYMD
                                           TO  WRK-YMD
               END-IF
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-I01-IDOYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-I01-IDOYMD
           ELSE
               MOVE    SPA-GMN-I01-IDOYMD
                                       TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
      *
      *            エラーコードを設定（後続の処理は続行）
                   MOVE    "0013"      TO  SPA-ERRCD
                   MOVE    3           TO  SPA-GMN-I01-CUR
                   MOVE    WRK-IDOYMD  TO  WRK-YMD
                   PERFORM 5002-HIZUKE-CHK-SEC
               END-IF
      *
               MOVE    WRK-HENYMDG     TO  SPA-GMN-I01-IDOYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-I01-IDOYMD
           END-IF
      *
           IF    ( SPA-NAI-I01-IDOYMD  NOT =   SPA-NAI-I01-IDOYMD-MAE )
            OR   ( WRK-MCP-WIDGET          =   "SYORIKBN" )
               MOVE    SPA-NAI-I01-IDOYMD  TO  SPA-NAI-I01-IDOYMD-MAE
      *        コンボ初期処理
               MOVE    SPA-NAI-I01-IDOYMD
                                   TO  WRK-KJNYMD
               PERFORM 310-CMB-SYOKI-SEC
      *
      *        保険組み合わせコンボ初期処理
               PERFORM 4101-CMB-HKNCOMBI-SET-SEC
      *        受付マスタ検索処理
               MOVE    SPA-NAI-I01-IDOYMD
                                   TO  WRK-KJNYMD
               PERFORM 4101-UKETUKE-SEL-SEC
      *        受診履歴検索処理
               MOVE    SPA-NAI-I01-IDOYMD
                                   TO  WRK-KJNYMD
               PERFORM 4101-JYURRK-SEL-SEC
      *
      *        病室番号チェック
               MOVE    SPA-NAI-I01-IDOYMD
                                   TO  WRK-KIHON-KJNYMD
      *
               PERFORM 4101-BRMNUM-CHK-SEC
               MOVE    SPA-GMN-I01-BRMNUM
                                       TO  SPA-NAI-I01-BRMNUM-MAE
               MOVE    SPA-ERRCD       TO  SPA-NAI-I01-BRMNUM-ERR
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4101-IDOYMD-CHK-EXT
               END-IF
      *
               MOVE    4               TO   SPA-GMN-I01-CUR
      *
           END-IF
      
      *
           .
      *
       4101-IDOYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院日チェック処理
      *****************************************************************
       4101-NYUINYMD-CHK-SEC           SECTION.
      *
           MOVE    SPA-NAI-I01-NYUINYMD    TO  WRK-NYUINYMD
      *
           IF    ( SPA-GMN-I01-NYUINYMD    =   SPACE )
      *
      *        入力がない場合、システム日付を設定
               MOVE    SPA-SYSYMD  TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG TO  SPA-GMN-I01-NYUINYMD
               MOVE    WRK-SYMD    TO  SPA-NAI-I01-NYUINYMD
           ELSE
               MOVE    SPA-GMN-I01-NYUINYMD
                                   TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
      *            エラーコードを設定（後続の処理は続行）
                   MOVE    "0017"      TO  SPA-ERRCD
                   MOVE    7           TO  SPA-GMN-I01-CUR
                   MOVE    WRK-NYUINYMD
                                       TO  WRK-YMD
                   PERFORM 5002-HIZUKE-CHK-SEC
               END-IF
               MOVE    WRK-HENYMDG     TO  SPA-GMN-I01-NYUINYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-I01-NYUINYMD
           END-IF
      *
           IF    (  SPA-NAI-I01-NYUINYMD
                                   NOT =   SPA-NAI-I01-NYUINYMD-MAE )
               MOVE    SPA-NAI-I01-NYUINYMD
                                       TO  SPA-NAI-I01-NYUINYMD-MAE
      *        コンボ初期処理
               MOVE    SPA-NAI-I01-NYUINYMD
                                   TO  WRK-KJNYMD
               PERFORM 310-CMB-SYOKI-SEC
      *
               MOVE    SPA-NAI-I01-NYUINYMD
                                   TO  WRK-KJNYMD
      *        保険組み合わせコンボ初期処理
               PERFORM 4101-CMB-HKNCOMBI-SET-SEC
      *        受付マスタ検索処理
               MOVE    SPA-NAI-I01-NYUINYMD
                                   TO  WRK-KJNYMD
               PERFORM 4101-UKETUKE-SEL-SEC
      *        受診履歴検索処理
               MOVE    SPA-NAI-I01-NYUINYMD
                                   TO  WRK-KJNYMD
               PERFORM 4101-JYURRK-SEL-SEC
      *
               MOVE    SPA-NAI-I01-NYUINYMD
                                   TO  WRK-KIHON-KJNYMD
      *        病室番号チェック
               PERFORM 4101-BRMNUM-CHK-SEC
               MOVE    SPA-GMN-I01-BRMNUM
                                       TO  SPA-NAI-I01-BRMNUM-MAE
               MOVE    SPA-ERRCD       TO  SPA-NAI-I01-BRMNUM-ERR
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4101-NYUINYMD-CHK-EXT
               END-IF
      *
               MOVE    8              TO   SPA-GMN-I01-CUR
      *
           END-IF
      *
           .
      *
       4101-NYUINYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院日チェック処理
      *****************************************************************
       4101-TAIINYMD-CHK-SEC           SECTION.
      *
           MOVE    SPACE           TO  SPA-NAI-I01-TAIINYMD
      *
           IF    ( SPA-GMN-I01-TAIINYMD    =   SPACE )
               GO  TO  4101-TAIINYMD-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I01-TAIINYMD
                                   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    "0021"      TO  SPA-ERRCD
               MOVE    11          TO  SPA-GMN-I01-CUR
               GO  TO  4101-TAIINYMD-CHK-EXT
           END-IF
      *
           MOVE    WRK-HENYMDG     TO  SPA-GMN-I01-TAIINYMD
           MOVE    WRK-SYMD        TO  SPA-NAI-I01-TAIINYMD
      *
           IF    ( WRK-MCP-WIDGET  =   "TAIINYMD" )
               MOVE    SPA-NAI-I01-TAIINYMD
                                   TO  WRK-KJNYMD
               PERFORM 310-CMB-SYOKI-SEC
           END-IF
      *
           .
      *
       4101-TAIINYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院事由チェック処理
      *****************************************************************
       4101-TAIINCD-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-I01-TAIINCD =   SPACE )
               GO  TO  4101-TAIINCD-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I01-TAIINCD-CD
                                       TO  WRK-CMB-CD
           PERFORM 4103-CMB-TAIINCD-SRH-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    "0028"          TO  SPA-ERRCD
               MOVE    20              TO  SPA-GMN-I01-CUR
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-TAIINCD
           END-IF
      *
           .
      *
       4101-TAIINCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病室番号チェック処理
      *****************************************************************
       4101-BRMNUM-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-I01-BRMNUM  =   SPACE )
               INITIALIZE  SPA-GMN-I01-CMB-BTUNAME
                           SPA-GMN-I01-BTUNAME
               GO  TO  4101-BRMNUM-CHK-EXT
           END-IF
      *
      *    全角半角混在チェック
           MOVE    SPA-GMN-I01-BRMNUM
                                   TO  WRK-KANACHK-MAE-INPUT
      *
           PERFORM     800-ENRTRY-CHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  SPA-GMN-I01-BRMNUM
           IF  ( FLG-ENTRYCHK      NOT =   1 )
               MOVE    SPACE           TO  SPA-GMN-I01-BTUNAME
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    4               TO  SPA-GMN-I01-CUR
               GO  TO  4101-BRMNUM-CHK-EXT
           END-IF
      *
      *    左詰編集を行う
           MOVE    SPA-GMN-I01-BRMNUM  TO  WRK-BRMNUM-IN
           PERFORM 4101-BRMNUM-LEFT-EDIT-SEC
           MOVE    WRK-BRMNUM-OT       TO  SPA-GMN-I01-BRMNUM
      *
      *    間に空白文字が混入していないかチェック
           MOVE    SPACE           TO  WRK-STR1
                                       WRK-STR2
           UNSTRING    SPA-GMN-I01-BRMNUM
               DELIMITED   BY  ALL SPACE
               INTO    WRK-STR1
                       WRK-STR2
           END-UNSTRING
      *
           IF    ( WRK-STR2        NOT =   SPACE )
               MOVE    SPACE           TO  SPA-GMN-I01-BTUNAME
               MOVE    "0014"          TO  SPA-ERRCD
               MOVE    4               TO  SPA-GMN-I01-CUR
               GO  TO  4101-BRMNUM-CHK-EXT
           END-IF
      *
      *    病室番号存在チェック
           MOVE    WRK-KIHON-KJNYMD    TO  WRK-KJNYMD
           PERFORM 4101-BRMNUM-SEL-SEC
      *
           EVALUATE    SPA-GMN-I01-BTUNAME-CNT
      *    該当の病室が存在しなかった場合
           WHEN    0
               MOVE    SPACE           TO  SPA-GMN-I01-BTUNAME
               MOVE    "0004"          TO  SPA-ERRCD
               MOVE    4               TO  SPA-GMN-I01-CUR
               GO  TO  4101-BRMNUM-CHK-EXT
      *    該当の病室が１件存在した場合
           WHEN    1
               MOVE    SPA-GMN-I01-BTUNAME-ITM (1)
                                       TO  SPA-GMN-I01-BTUNAME
               MOVE    SPA-GMN-I01-BTUNAME-ITM2 (1)
                                       TO  WRK-BRM-SAGAKU-CD
               MOVE    SPA-GMN-I01-BTUNAME-ITM3 (1)
                                       TO  WRK-BTU-KA-CD
               MOVE    SPA-GMN-I01-BTUNAME-ITM4 (1)
                                       TO  WRK-BRM-KA-CD
               MOVE    6               TO  SPA-GMN-I01-CUR
      *    該当の病室が複数件存在した場合
           WHEN    OTHER
               IF    ( SPA-GMN-I01-BTUNAME =   SPACE )
                   MOVE    SPA-GMN-I01-BTUNAME-ITM (1)
                                       TO  SPA-GMN-I01-BTUNAME
                   MOVE    SPA-GMN-I01-BTUNAME-ITM2 (1)
                                       TO  WRK-BRM-SAGAKU-CD
                   MOVE    SPA-GMN-I01-BTUNAME-ITM3 (1)
                                       TO  WRK-BTU-KA-CD
                   MOVE    SPA-GMN-I01-BTUNAME-ITM4 (1)
                                       TO  WRK-BRM-KA-CD
               ELSE
                   MOVE   SPA-GMN-I01-BTUNAME-CD
                                       TO  WRK-CMB-CD
                   PERFORM 4103-CMB-BTUNAME-SRH-SEC
                   IF    ( WRK-CMB-ITM =   SPACE )
                       MOVE    SPA-GMN-I01-BTUNAME-ITM (1)
                                       TO  SPA-GMN-I01-BTUNAME
                       MOVE    SPA-GMN-I01-BTUNAME-ITM2 (1)
                                       TO  WRK-BRM-SAGAKU-CD
                       MOVE    SPA-GMN-I01-BTUNAME-ITM3 (1)
                                       TO  WRK-BTU-KA-CD
                       MOVE    SPA-GMN-I01-BTUNAME-ITM4 (1)
                                       TO  WRK-BRM-KA-CD
                   ELSE
                      MOVE WRK-CMB-ITM TO  SPA-GMN-I01-BTUNAME
                      MOVE WRK-CMB-ITM2
                                       TO  WRK-BRM-SAGAKU-CD
                      MOVE WRK-CMB-ITM3
                                       TO  WRK-BTU-KA-CD
                      MOVE WRK-CMB-ITM4
                                       TO  WRK-BRM-KA-CD
                   END-IF
               END-IF
           END-EVALUATE
      *
      *    室料差額の初期値を設定
           IF    ( SPA-GMN-I01-BRMNUM      =  SPA-NAI-I01-BRMNUM-MAE )
               MOVE    SPA-GMN-I01-SAGAKU-CD
                                       TO  WRK-CMB-CD
               PERFORM 4103-CMB-SAGAKU-SRH-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-SAGAKU
           ELSE
               MOVE    WRK-BRM-SAGAKU-CD   TO  WRK-CMB-CD
               PERFORM 4103-CMB-SAGAKU-SRH-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-I01-SAGAKU
           END-IF
      *
      *    診療科の初期値を設定
           IF    ( SPA-GMN-I01-BRMNUM      =  SPA-NAI-I01-BRMNUM-MAE )
               MOVE    SPA-GMN-I01-NYUINKA-CD
                                           TO  WRK-CMB-CD
               PERFORM 4103-CMB-NYUINKA-SRH-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-I01-NYUINKA
           ELSE
               IF    ( WRK-BRM-KA-CD       NOT =   SPACE )
                   MOVE    WRK-BRM-KA-CD   TO  WRK-CMB-CD
                   PERFORM 4103-CMB-NYUINKA-SRH-SEC
                   MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-NYUINKA
               ELSE
                   IF    ( WRK-BTU-KA-CD   NOT =   SPACE )
                       MOVE    WRK-BTU-KA-CD   TO  WRK-CMB-CD
                       PERFORM 4103-CMB-NYUINKA-SRH-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-NYUINKA
                   END-IF
               END-IF
           END-IF
      *
      *    特定入院料コンボ設定処理
           MOVE    WRK-KIHON-KJNYMD    TO  WRK-KJNYMD
           PERFORM 4101-CMB-TOKNYUIN-SET-SEC
      *
      *    特定入院料の初期値を設定
           MOVE    WRK-KIHON-KJNYMD    TO  WRK-KJNYMD
           PERFORM 4101-TOKNYUIN-SYOKI-SEC
      *
           .
      *
       4101-BRMNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病室検索処理
      *****************************************************************
       4101-BRMNUM-SEL-SEC             SECTION.
      *
           INITIALIZE              SYSKANRI-REC
                                   WRK-BTUJYOHO-TBL
      *
           MOVE    "5002"          TO  SYS-KANRICD
           MOVE    WRK-KJNYMD
                                   TO  ORC-DBYMD
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
      *
      *    システム管理マスタ検索
           MOVE    "SYSKANRI-KEY2" TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
      *
           PERFORM UNTIL ( FLG-DBRC       NOT =   ZERO )
                   OR    ( WRK-BTUJYOHO-CNT  >= 50  )
      *
               MOVE    MCPDATA-REC TO  SYS-5002-REC
      *
               IF    ( SYS-5002-BRM-NUM
                                               =   SPA-GMN-I01-BRMNUM )
                   COMPUTE WRK-BTUJYOHO-CNT    
                       =   WRK-BTUJYOHO-CNT    +   1
                   MOVE    SYS-5002-KBNCD (1 : 2)
                                               TO  WRK-BTUJY-BTUNUM
                                                   (WRK-BTUJYOHO-CNT)
                   MOVE    SYS-5002-BRM-SAGAKU
                                               TO  WRK-BTUJY-BRM-SAGAKU
                                                   (WRK-BTUJYOHO-CNT)
                   MOVE    SYS-5002-BRM-KA
                                               TO  WRK-BTUJY-BRM-KA
                                                   (WRK-BTUJYOHO-CNT)
               END-IF
      *
      *        システム管理マスタ読み込み
               MOVE    "SYSKANRI-KEY2" TO  WRK-DBPATH
               PERFORM 900-TBL-READ-SEC
      *
           END-PERFORM
      *
      *    システム管理マスタクローズ
           MOVE    "SYSKANRI-KEY2" TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
      *    病棟名検索処理
           PERFORM 4101-BTUNAME-SEL-SEC
      *
           .
       4101-BRMNUM-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    病棟名称設定処理
      *****************************************************************
       4101-BTUNAME-SEL-SEC            SECTION.
      *
      *
           INITIALIZE              SYSKANRI-REC
                                   SPA-GMN-I01-CMB-BTUNAME
      *
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    WRK-KJNYMD      TO  ORC-DBYMD
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
      *
      *    システム管理マスタ検索
           MOVE    "SYSKANRI-KEY2" TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
      *
           PERFORM UNTIL ( FLG-DBRC                NOT = ZERO )
                   OR    ( SPA-GMN-I01-BTUNAME-CNT    >= 50  )
      *
               MOVE    MCPDATA-REC TO  SYS-5001-REC
      *
               SET WRK-BTUNUM-IDX  TO  1
               SEARCH  WRK-BTUJYOHO-OCC    VARYING WRK-BTUNUM-IDX
               WHEN    WRK-BTUJY-BTUNUM (WRK-BTUNUM-IDX)
                                   =   SYS-5001-BTU-NUM
                   COMPUTE SPA-GMN-I01-BTUNAME-CNT
                       =   SPA-GMN-I01-BTUNAME-CNT +   1
      *
                   MOVE    SYS-5001-BTU-NUM
                               TO  SPA-GMN-I01-BTUNAME-ITM
                                    (SPA-GMN-I01-BTUNAME-CNT) (1 : 2)
                   MOVE    SYS-5001-BTU-NAME
                               TO  SPA-GMN-I01-BTUNAME-ITM
                                    (SPA-GMN-I01-BTUNAME-CNT) (4 : )
                   MOVE    WRK-BTUJY-BRM-SAGAKU (WRK-BTUNUM-IDX)
                               TO  SPA-GMN-I01-BTUNAME-ITM2
                                    (SPA-GMN-I01-BTUNAME-CNT)
                   MOVE    SYS-5001-BTU-KA
                               TO  SPA-GMN-I01-BTUNAME-ITM3
                                    (SPA-GMN-I01-BTUNAME-CNT)
                   MOVE    WRK-BTUJY-BRM-KA (WRK-BTUNUM-IDX)
                               TO  SPA-GMN-I01-BTUNAME-ITM4
                                    (SPA-GMN-I01-BTUNAME-CNT)
               WHEN   WRK-BTUJY-BTUNUM (WRK-BTUNUM-IDX)
                                   =   SPACE
                   CONTINUE
               END-SEARCH
      *
      *        システム管理マスタ読み込み
               MOVE    "SYSKANRI-KEY2" TO  WRK-DBPATH
               PERFORM 900-TBL-READ-SEC
      *
           END-PERFORM
      *
      *    システム管理マスタクローズ
           MOVE    "SYSKANRI-KEY2" TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
       4101-BTUNAME-SEL-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料コンボ設定処理
      *****************************************************************
       4101-CMB-TOKNYUIN-SET-SEC       SECTION.
      *
           INITIALIZE  SPA-GMN-I01-CMB-TOKNYUIN
                       SPA-NAI-I01-KHNRYO-G
                       SPA-NAI-I01-BTUSBT
                       SPA-NAI-I01-BTUKHNKBNCD
                       SPA-NAI-I01-BRMKHNKBNCD
                       SPA-NAI-I01-SELNYUKHN
      *
           MOVE    ZERO            TO  FLG-CMB-EDIT
      *    病棟情報からコンボ項目を設定
           MOVE    "5001"          TO  WRK-KANRICD
           MOVE    SPA-GMN-I01-BTUNAME-CD
                                   TO  WRK-KBNCD
           PERFORM 4102-SYSKANRI-SEL-SEC
           IF    ( FLG-SYSKANRI    =   ZERO )
               MOVE    MCPDATA-REC TO  SYS-5001-REC
      *
               MOVE    SYS-5001-BTU-SBT    TO  SPA-NAI-I01-BTUSBT
               MOVE    SYS-5001-KHNKBNCD   TO  SPA-NAI-I01-BTUKHNKBNCD
      *
      *        病棟の入院基本料を退避する
               IF    ( SYS-5001-BTU-KHNSRYCD
                               NOT =   SPACE )
                   MOVE    SYS-5001-BTU-KHNSRYCD
                                   TO  SPA-NAI-I01-KHNRYO-CD
                   MOVE    SYS-5001-BTU-KHNSRYCD
                                   TO  WRK-SRYCD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( FLG-TENSU   =   ZERO )
                       MOVE    TNS-NAME    TO   SPA-NAI-I01-KHNRYO-NM
                   END-IF
               END-IF
      *
               IF    ( WRK-KJNYMD         >=   CONST-H180701 )
                AND  ( SYS-5001-BTU-SBT    =   "04" OR "09" )
      *
                   MOVE    1               TO  SPA-NAI-I01-SELNYUKHN
      *
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    "5111"          TO  SYS-KANRICD
                   MOVE    WRK-KJNYMD      TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
                   PERFORM 900-SYSKANRI-KEY7-SEL-SEC
                   MOVE    SYSKANRI-REC    TO  SYS-5111-REC
                   PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
      *
                       MOVE    ZERO        TO  FLG-TARGET
                       IF    ( SYS-5001-BTU-SBT            =   "04" )
                        AND  ( SYS-5111-BRM-NYUINRYOKBN    =   "05" )
                           MOVE    1       TO  FLG-TARGET
                       END-IF
      *
                       IF    ( SYS-5001-BTU-SBT            =   "09" )
                        AND  ( SYS-5111-BRM-NYUINRYOKBN    =   "06" )
                           MOVE    1       TO  FLG-TARGET
                       END-IF
      *
                       IF    ( FLG-TARGET  =   1 )
      *
                           MOVE    SYS-5111-BRM-TOKUTEINYUIN-NM
                                                   TO  WRK-TOKNYUIN-NM
                           MOVE    "3"             TO  WRK-TOKNYUIN-KBN
                           MOVE    SYS-5111-KBNCD
                                                   TO  WRK-TOKNYUIN-CD
                           PERFORM 4101-CMB-TOKNYUIN-DAT-SET-SEC
                       END-IF
      *
                       PERFORM 900-SYSKANRI-KEY7-FET-SEC
                       MOVE    SYSKANRI-REC    TO  SYS-5111-REC
      *
                   END-PERFORM
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key7"              TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
      *
               IF    ( SYS-5001-BTU-TOKTEI-NYUIN
                               NOT =   SPACE )
                   MOVE    "5110"      TO  WRK-KANRICD
                   IF    ( SYS-5001-BTU-TOKTEI-NYUIN
                                       =   "01" OR "02" OR "14" )
                       PERFORM 41011-PT-AGE-SEC
                       IF    ( WRK-NENREI  <   15 )
                           MOVE    1   TO  FLG-CMB-EDIT
                       END-IF
                   ELSE
                       MOVE        1   TO  FLG-CMB-EDIT
                   END-IF
      *
                   IF    ( FLG-CMB-EDIT    =   1 )
                       MOVE    SYS-5001-BTU-TOKTEI-NYUIN
                                       TO  WRK-KBNCD
                       PERFORM 4102-SYSKANRI-SEL-SEC
                       IF    ( FLG-SYSKANRI    =   ZERO )
                           MOVE    MCPDATA-REC
                                       TO  SYS-5110-REC
                           MOVE    SYS-5110-BTU-TOKUTEINYUIN-NM
                                       TO  WRK-TOKNYUIN-NM
                           MOVE    "2"     TO  WRK-TOKNYUIN-KBN
                           MOVE    SYS-5110-KBNCD
                                       TO  WRK-TOKNYUIN-CD
                           PERFORM 4101-CMB-TOKNYUIN-DAT-SET-SEC
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    病室情報からコンボ項目を設定
           MOVE    "5002"          TO  WRK-KANRICD
           MOVE    SPA-GMN-I01-BTUNAME-CD
                                   TO  WRK-KBNCD (1 : 2)
           MOVE    SPA-GMN-I01-BRMNUM
                                   TO  WRK-BRMNUM
           PERFORM 4800-BRMNUM-EDIT-SEC
           MOVE    WRK-BRMNUM-EDT  TO  WRK-KBNCD (3 : )
           PERFORM 4102-SYSKANRI-SEL-SEC
           IF    ( FLG-SYSKANRI    =   ZERO )
               MOVE    MCPDATA-REC TO  SYS-5002-REC
      *
               MOVE    SYS-5002-KHNKBNCD   TO  SPA-NAI-I01-BRMKHNKBNCD
      *
               IF    ( SYS-5002-BRM-TOKTEI-NYUIN
                               NOT =   SPACE )
                   MOVE    "5111"      TO  WRK-KANRICD
                   MOVE    SYS-5002-BRM-TOKTEI-NYUIN
                                       TO  WRK-KBNCD
                   PERFORM 4102-SYSKANRI-SEL-SEC
                   IF    ( FLG-SYSKANRI    =   ZERO )
                       MOVE    MCPDATA-REC
                                       TO  SYS-5111-REC
                       MOVE    SYS-5111-BRM-TOKUTEINYUIN-NM
                                       TO  WRK-TOKNYUIN-NM
                       MOVE    "3"     TO  WRK-TOKNYUIN-KBN
                       MOVE    SYS-5111-KBNCD
                                       TO  WRK-TOKNYUIN-CD
                       PERFORM 4101-CMB-TOKNYUIN-DAT-SET-SEC
                   END-IF
      *
      *            病室の特定入院料が、診療所老人医療管理料の場合、
      *            診療所老人医療管理料（３０日以内再入院）もコンボへ
      *            追加する
                   IF    ( SYS-5002-BRM-TOKTEI-NYUIN   =   "02" )
                       MOVE    "5111"      TO  WRK-KANRICD
                       MOVE    "23"        TO  WRK-KBNCD
                       PERFORM 4102-SYSKANRI-SEL-SEC
                       IF    ( FLG-SYSKANRI    =   ZERO )
                           MOVE    MCPDATA-REC
                                       TO  SYS-5111-REC
                           MOVE    SYS-5111-BRM-TOKUTEINYUIN-NM
                                       TO  WRK-TOKNYUIN-NM
                           MOVE    "3"     TO  WRK-TOKNYUIN-KBN
                           MOVE    SYS-5111-KBNCD
                                       TO  WRK-TOKNYUIN-CD
                           PERFORM 4101-CMB-TOKNYUIN-DAT-SET-SEC
                       END-IF
      *
                   END-IF
      *
               END-IF
           END-IF
      *
      *    老人の場合、老人一般病棟特定入院基本料をコンボに設定
           MOVE    WRK-KJNYMD  TO  WRK-AGECHK-SRYYMD
           PERFORM 4801-RJN-TAISYO-CHK-SEC
           IF    ( FLG-RJN         =   1 )
               MOVE    "5111"      TO  WRK-KANRICD
               MOVE    "22"        TO  WRK-KBNCD
               PERFORM 4102-SYSKANRI-SEL-SEC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   MOVE    MCPDATA-REC     TO  SYS-5111-REC
                   MOVE    SYS-5111-BRM-TOKUTEINYUIN-NM
                                           TO  WRK-TOKNYUIN-NM
                   MOVE    "3"             TO  WRK-TOKNYUIN-KBN
                   MOVE    SYS-5111-KBNCD
                                           TO  WRK-TOKNYUIN-CD
                   PERFORM 4101-CMB-TOKNYUIN-DAT-SET-SEC
               END-IF
      *
               MOVE    "5111"      TO  WRK-KANRICD
               MOVE    "24"        TO  WRK-KBNCD
               PERFORM 4102-SYSKANRI-SEL-SEC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   MOVE    MCPDATA-REC     TO  SYS-5111-REC
                   MOVE    SYS-5111-BRM-TOKUTEINYUIN-NM
                                           TO  WRK-TOKNYUIN-NM
                   MOVE    "3"             TO  WRK-TOKNYUIN-KBN
                   MOVE    SYS-5111-KBNCD
                                           TO  WRK-TOKNYUIN-CD
                   PERFORM 4101-CMB-TOKNYUIN-DAT-SET-SEC
               END-IF
      *
           END-IF
      *
      *    短期滞在手術基本料をコンボに設定
           MOVE    "5111"      TO  WRK-KANRICD
           MOVE    "21"        TO  WRK-KBNCD
           PERFORM 4102-SYSKANRI-SEL-SEC
           IF    ( FLG-SYSKANRI    =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-5111-REC
               MOVE    SYS-5111-BRM-TOKUTEINYUIN-NM
                                       TO  WRK-TOKNYUIN-NM
               MOVE    "3"             TO  WRK-TOKNYUIN-KBN
               MOVE    SYS-5111-KBNCD
                                       TO  WRK-TOKNYUIN-CD
               PERFORM 4101-CMB-TOKNYUIN-DAT-SET-SEC
           END-IF
      *
      *
           .
       4101-CMB-TOKNYUIN-SET-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料コンボ設定処理
      *****************************************************************
       41011-PT-AGE-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-NENREI
      *
           MOVE    SPA-NAI-I01-BIRTHDAY
                                       TO  WRK-YMDS1
           MOVE    WRK-KJNYMD          TO  WRK-YMDS2
           COMPUTE WRK-NENREI      =   WRK-YYS2
                                   -   WRK-YYS1
           IF    ( WRK-YMDS1(5:4)     >   WRK-YMDS2(5:4) )
                COMPUTE WRK-NENREI =   WRK-NENREI  -   1
           END-IF
      *
           .
       41011-PT-AGE-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       4102-SYSKANRI-SEL-SEC       SECTION.
      *
           INITIALIZE              SYSKANRI-REC
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    WRK-KANRICD     TO  SYS-KANRICD
           MOVE    WRK-KBNCD       TO  SYS-KBNCD
           MOVE    WRK-KJNYMD      TO  ORC-DBYMD
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY10"
                                   TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    =   ZERO )
               CONTINUE
           ELSE
               MOVE    1          TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "SYSKANRI-KEY10"
                                   TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
       4102-SYSKANRI-SEL-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料コンボデータセット処理
      *****************************************************************
       4101-CMB-TOKNYUIN-DAT-SET-SEC   SECTION.
      *
           IF    ( WRK-TOKNYUIN-NM     =   SPACE )
               GO  TO  4101-CMB-TOKNYUIN-DAT-SET-EXT
           END-IF
      *
           IF    ( SPA-GMN-I01-TOKNYUIN-CNT   >=   CONST-TOKNYUIN-MAX )
               CONTINUE
           ELSE
               COMPUTE SPA-GMN-I01-TOKNYUIN-CNT
                   =   SPA-GMN-I01-TOKNYUIN-CNT    +   1
               MOVE    SPA-GMN-I01-TOKNYUIN-CNT
                                       TO  IDX10
               MOVE    IDX10           TO  WRK-99
               MOVE    WRK-99          TO  SPA-GMN-I01-TOKNYUIN-ITM
                                                       (IDX10) (1 : 2)
               MOVE    WRK-TOKNYUIN-NM TO  SPA-GMN-I01-TOKNYUIN-ITM
                                                       (IDX10) (4 : )
               MOVE    WRK-TOKNYUIN-KBN
                                       TO  SPA-GMN-I01-TOKNYUIN-ITM2
                                                       (IDX10) (1 : 1)
               MOVE    WRK-TOKNYUIN-CD
                                       TO  SPA-GMN-I01-TOKNYUIN-ITM2
                                                       (IDX10) (2 : 2)
           END-IF
      *
           .
       4101-CMB-TOKNYUIN-DAT-SET-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料初期値設定処理
      *****************************************************************
       4101-TOKNYUIN-SYOKI-SEC         SECTION.
      *
           INITIALIZE  SPA-GMN-I01-TOKNYUIN
                       SPA-GMN-I01-SNTNYUIN
      *
           PERFORM VARYING IDX10
                   FROM    SPA-GMN-I01-TOKNYUIN-CNT    BY  -1
                   UNTIL ( IDX10   <   1 )
                    OR   ( SPA-GMN-I01-TOKNYUIN NOT =   SPACE )
      *
               IF    ( SPA-GMN-I01-TOKNYUIN-ITM2 (IDX10) (1 : 1)
                                   =   "1" OR  "2" OR  "3" )
                   IF    ( SPA-GMN-I01-TOKNYUIN-ITM2 (IDX10)
                                   >=  "321" )
                       CONTINUE
                   ELSE
                       MOVE    SPA-GMN-I01-TOKNYUIN-ITM (IDX10)
                                       TO  SPA-GMN-I01-TOKNYUIN
                       MOVE    SPA-GMN-I01-TOKNYUIN-ITM2 (IDX10)
                                       TO  SPA-NAI-I01-TOKNYUIN-ORG
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-GMN-I01-TOKNYUIN            =   SPACE )
               IF    ( SPA-NAI-I01-BRMKHNKBNCD NOT =   SPACE )
                   MOVE    SPA-NAI-I01-BRMKHNKBNCD
                                       TO  WRK-CMB-CD
                   PERFORM 4103-CMB-TOKNYUIN-SRH2-SEC
                   MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-TOKNYUIN
                   MOVE    WRK-CMB-ITM2    TO  SPA-NAI-I01-TOKNYUIN-ORG
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-TOKNYUIN            =   SPACE )
               IF    ( SPA-NAI-I01-BTUKHNKBNCD NOT =   SPACE )
                   MOVE    SPA-NAI-I01-BTUKHNKBNCD
                                       TO  WRK-CMB-CD
                   PERFORM 4103-CMB-TOKNYUIN-SRH2-SEC
                   MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-TOKNYUIN
                   MOVE    WRK-CMB-ITM2    TO  SPA-NAI-I01-TOKNYUIN-ORG
               END-IF
           END-IF
      *
      *    算定入院料についても初期値を設定
           IF    ( SPA-GMN-I01-TOKNYUIN    =   SPACE )
               MOVE    SPA-NAI-I01-KHNRYO-NM
                                       TO  SPA-GMN-I01-SNTNYUIN
           ELSE
               MOVE    SPA-GMN-I01-TOKNYUIN-NM
                                       TO  SPA-GMN-I01-SNTNYUIN
           END-IF
      *
           .
       4101-TOKNYUIN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    病棟名称チェック処理
      *****************************************************************
       4101-BTUNAME-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-I01-BTUNAME =   SPACE )
               GO  TO  4101-BTUNAME-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I01-BTUNAME-CD
                                       TO  WRK-CMB-CD
           PERFORM 4103-CMB-BTUNAME-SRH-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-BTUNAME
      *        室料差額のデフォルト値を設定
               MOVE    WRK-CMB-ITM2
                                   TO  WRK-CMB-CD
               PERFORM 4103-CMB-SAGAKU-SRH-SEC
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SAGAKU
      *
      *        診療科の初期値を設定
               IF    ( WRK-CMB-ITM4    NOT =   SPACE )
                   MOVE    WRK-CMB-ITM4        TO  WRK-CMB-CD
                   PERFORM 4103-CMB-NYUINKA-SRH-SEC
                   MOVE    WRK-CMB-ITM         TO  SPA-GMN-I01-NYUINKA
               ELSE
                   IF    ( WRK-CMB-ITM3    NOT =   SPACE )
                       MOVE    WRK-CMB-ITM3    TO  WRK-CMB-CD
                       PERFORM 4103-CMB-NYUINKA-SRH-SEC
                       MOVE    WRK-CMB-ITM     TO  SPA-GMN-I01-NYUINKA
                   END-IF
               END-IF
           ELSE
               MOVE    "0015"      TO  SPA-ERRCD
               MOVE    5           TO  SPA-GMN-I01-CUR
               GO  TO  4101-BTUNAME-CHK-EXT
           END-IF
      *
      *    特定入院料コンボ設定処理
           MOVE    WRK-KIHON-KJNYMD    TO  WRK-KJNYMD
           PERFORM 4101-CMB-TOKNYUIN-SET-SEC
      *
      *    特定入院料の初期値を設定
           MOVE    WRK-KIHON-KJNYMD    TO  WRK-KJNYMD
           PERFORM 4101-TOKNYUIN-SYOKI-SEC
      *
           .
      *
       4101-BTUNAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    室料差額チェック処理
      *****************************************************************
       4101-SAGAKU-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-I01-SAGAKU  =   SPACE )
               GO  TO  4101-SAGAKU-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I01-SAGAKU-CD
                                       TO  WRK-CMB-CD
           PERFORM 4103-CMB-SAGAKU-SRH-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SAGAKU
           ELSE
               MOVE    "0016"      TO  SPA-ERRCD
               MOVE    6           TO  SPA-GMN-I01-CUR
               GO  TO  4101-SAGAKU-CHK-EXT
           END-IF
      *
           .
      *
       4101-SAGAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院科チェック処理
      *****************************************************************
       4101-NYUINKA-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-I01-NYUINKA =   SPACE )
               GO  TO  4101-NYUINKA-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I01-NYUINKA-CD
                                       TO  WRK-CMB-CD
           PERFORM 4103-CMB-NYUINKA-SRH-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-NYUINKA
           ELSE
               MOVE    "0018"      TO  SPA-ERRCD
               MOVE    8           TO  SPA-GMN-I01-CUR
               GO  TO  4101-NYUINKA-CHK-EXT
           END-IF
      *
           .
      *
       4101-NYUINKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初回チェック処理
      *****************************************************************
       4101-SHOKAI-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-I01-SHOKAI  =   SPACE )
               GO  TO  4101-SHOKAI-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I01-SHOKAI-CD
                                       TO  WRK-CMB-CD
           PERFORM 4103-CMB-SHOKAI-SRH-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SHOKAI
           ELSE
               MOVE    "0019"      TO  SPA-ERRCD
               MOVE    9           TO  SPA-GMN-I01-CUR
               GO  TO  4101-SHOKAI-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   "01" )
               IF    ( SPA-GMN-I01-SHOKAI-CD   =   "2")
                   MOVE    WIDGET-NORMAL
                                           TO  SPA-GMN-I01-SHONUM-STT
               ELSE
                   MOVE    WIDGET-INSENSITIVE
                                           TO  SPA-GMN-I01-SHONUM-STT
                   MOVE    ZERO            TO  SPA-GMN-I01-SHONUM
                   MOVE    SPACE       TO  SPA-GMN-I01-LBL-SHONYUINYMD
               END-IF
           END-IF
      *
           .
      *
       4101-SHOKAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初回番号チェック処理
      *****************************************************************
       4101-SHONUM-CHK-SEC             SECTION.
      *
           MOVE    SPACE       TO      SPA-GMN-I01-LBL-SHONYUINYMD
                                       WRK-SHOKAI-NYUINYMD-W
           MOVE    ZERO        TO      FLG-SHONUM
      *
           IF    ( SPA-GMN-I01-SHONUM   =   ZERO )
               GO  TO  4101-SHONUM-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-I01-NYUREKILST-CNT )
                    OR   ( FLG-SHONUM      =   1 )
      *
               IF    ( SPA-GMN-I01-TSHONUM (IDX1)
                                   =   SPA-GMN-I01-SHONUM )
                 AND ( SPA-GMN-I01-TSHOKAI (IDX1)  =   "1" )
      *
                   MOVE      SPA-GMN-I01-TNYUINYMD (IDX1)
                                   TO  WRK-SHOKAI-NYUINYMD-W
                   MOVE      1     TO  FLG-SHONUM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( FLG-SHONUM      =   ZERO )
               INITIALIZE                  WKPTNYUINRRK-REC
               MOVE    SPA-HOSPID      TO  WKPTNYUINRRK-HOSPID
               MOVE    SPA-NAI-I01-PTID
                                       TO  WKPTNYUINRRK-PTID
               MOVE    SPA-GMN-I01-SHONUM
                                       TO  WKPTNYUINRRK-SHONUM
               MOVE    WKPTNYUINRRK-REC
                                       TO  MCPDATA-REC
               MOVE    "PTNYUINRRK-KEY30"
                                       TO  WRK-DBPATH
               PERFORM 900-TBL-SELECT-SEC
               IF    ( FLG-DBRC        =   ZERO )
                   MOVE    MCPDATA-REC TO  WKPTNYUINRRK-REC
                   MOVE    WKPTNYUINRRK-NYUINYMD
                                       TO  WRK-YMD
                   PERFORM 5002-HIZUKE-CHK-SEC
                   MOVE    WRK-HENYMDG TO  WRK-SHOKAI-NYUINYMD-W
      *
                   MOVE      1       TO  FLG-SHONUM
               END-IF
      *
               MOVE    "PTNYUINRRK-KEY30"
                                       TO  WRK-DBPATH
               PERFORM 900-TBL-CLOSE-SEC
      *
           END-IF
      *
           IF    ( FLG-SHONUM      =   1 )
               STRING "入院日 "
                      WRK-SHOKAI-NYUINYMD-W
               DELIMITED   BY  SIZE
               INTO   SPA-GMN-I01-LBL-SHONYUINYMD
               END-STRING
           ELSE
               MOVE    10          TO      SPA-GMN-I01-CUR
               MOVE    "0005"      TO      SPA-ERRCD
               GO  TO  4101-SHONUM-CHK-EXT
           END-IF
      *
           .
       4101-SHONUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    担当医チェック処理
      *****************************************************************
       4101-DR-CHK-SEC                 SECTION.
      *
           IF    ( SPA-GMN-I01-DR (IDX1)  =   SPACE )
               GO  TO  4101-DR-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I01-DR-CD (IDX1)
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-DR-SRH-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-DR (IDX1)
           ELSE
               MOVE    "0022"      TO  SPA-ERRCD
               COMPUTE SPA-GMN-I01-CUR =   IDX1    +   11
               GO  TO  4101-DR-CHK-EXT
           END-IF
      *
           .
      *
       4101-DR-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合わせチェック処理
      *****************************************************************
       4101-HKNCOMBI-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-I01-SYORIKBN-CD    =   "01" OR  "08")
               CONTINUE
           ELSE
               GO  TO  4101-HKNCOMBI-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-I01-HKNCOMBI-CNT   >   ZERO )
               CONTINUE
           ELSE
               MOVE    SPACE       TO  SPA-GMN-I01-HKNCOMBI
               MOVE    "1021"      TO  SPA-ERRCD
               MOVE    15          TO  SPA-GMN-I01-CUR
               GO  TO  4101-HKNCOMBI-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-I01-HKNCOMBI
                                       =   SPACE )
               GO  TO  4101-HKNCOMBI-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I01-HKNCOMBI-CD
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-HKNCOMBI-SRH-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-HKNCOMBI
               MOVE    WRK-CMB-ITM2
                                   TO  SPA-NAI-I01-FTNRATE
               MOVE    WRK-CMB-ITM3
                                   TO  SPA-NAI-I01-RSI-HKNKBN
           ELSE
               MOVE    "0023"      TO  SPA-ERRCD
               MOVE    15          TO  SPA-GMN-I01-CUR
               GO  TO  4101-HKNCOMBI-CHK-EXT
           END-IF
      *
           .
      *
       4101-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特定入院チェック処理（算定入院料の設定も行う）
      *****************************************************************
       4101-TOKNYUIN-CHK-SEC           SECTION.
      *
           MOVE    SPACE           TO  SPA-NAI-I01-TOKNYUIN-ORG
                                       SPA-GMN-I01-SNTNYUIN
      *
           IF    ( SPA-GMN-I01-TOKNYUIN
                                       =   SPACE )
               MOVE    SPA-NAI-I01-KHNRYO-NM
                                   TO  SPA-GMN-I01-SNTNYUIN
               GO  TO  4101-TOKNYUIN-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I01-TOKNYUIN-CD
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-TOKNYUIN-SRH-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-TOKNYUIN
               MOVE    WRK-CMB-ITM2
                                   TO  SPA-NAI-I01-TOKNYUIN-ORG
               MOVE    SPA-GMN-I01-TOKNYUIN-NM
                                   TO  SPA-GMN-I01-SNTNYUIN
           ELSE
               MOVE    "0024"      TO  SPA-ERRCD
               MOVE    16          TO  SPA-GMN-I01-CUR
               GO  TO  4101-TOKNYUIN-CHK-EXT
           END-IF
      *
           .
      *
       4101-TOKNYUIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    前受け金チェック処理
      *****************************************************************
       4101-MAEUKE-CHK-SEC             SECTION.
      *
      *
           .
      *
       4101-MAEUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    定期請求チェック処理
      *****************************************************************
       4101-SEIKYU-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-I01-SEIKYU
                                       =   SPACE )
               GO  TO  4101-SEIKYU-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I01-SEIKYU-CD
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-SEIKYU-SRH-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-SEIKYU
           ELSE
               MOVE    "0025"      TO  SPA-ERRCD
               MOVE    18          TO  SPA-GMN-I01-CUR
               GO  TO  4101-SEIKYU-CHK-EXT
           END-IF
      *
           .
      *
       4101-SEIKYU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検索時患者表示チェック処理
      *****************************************************************
       4101-DISPKBN-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-I01-DISPKBN
                                       =   SPACE )
               GO  TO  4101-DISPKBN-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I01-DISPKBN-CD
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-DISPKBN-SRH-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I01-DISPKBN
           ELSE
               MOVE    "0026"      TO  SPA-ERRCD
               MOVE    19          TO  SPA-GMN-I01-CUR
               GO  TO  4101-DISPKBN-CHK-EXT
           END-IF
      *
           .
      *
       4101-DISPKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4101-SELNUM-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-I01-SELNUM  =   ZERO )
               GO  TO  4101-SELNUM-CHK-EXT
           END-IF
      *
      *    入力された番号が存在するかチェック
           IF    ( SPA-GMN-I01-SELNUM  >   SPA-GMN-I01-NYUREKILST-CNT )
               MOVE    "0027"      TO  SPA-ERRCD
               MOVE    100         TO  SPA-GMN-I01-CUR
               GO  TO  4101-SELNUM-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I01-SELNUM
                                   TO  SPA-GMN-I01-NYUREKILST-SEL
      *
           .
      *
       4101-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院歴リスト選択処理
      *****************************************************************
       4101-NYUREKILST-SEL-SEC         SECTION.
      *
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL     ( IDX1    >   SPA-GMN-I01-NYUREKILST-CNT )
      *
               IF    ( I01-NYUREKILST-SELECT (IDX1)    =   "T"  )
                   MOVE    IDX1        TO  SPA-GMN-I01-NYUREKILST-SEL
                   MOVE    SPA-GMN-I01-TSELNUM (IDX1)
                                       TO  I01-SELNUM
                   MOVE    SPA-GMN-I01-NYUREKILST-CNT
                                       TO  IDX1
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4101-NYUREKILST-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       4102-PTNUM-CHK-SEC              SECTION.
      *
      *    患者番号がエラーでないこと
           IF    ( SPA-GMN-I01-ERRCD (1)   NOT =   SPACE )
               MOVE    SPA-GMN-I01-ERRCD (1)
                                   TO  SPA-ERRCD
               MOVE    1           TO  SPA-GMN-I01-CUR
               GO  TO  4102-PTNUM-CHK-EXT
           END-IF
      *
      *    患者番号が入力されていること
           IF    ( SPA-GMN-I01-PTNUM           =   SPACE )
               MOVE    "1001"      TO  SPA-ERRCD
               MOVE    1           TO  SPA-GMN-I01-CUR
               GO  TO  4102-PTNUM-CHK-EXT
           END-IF
      *
           .
      *
       4102-PTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処理区分チェック処理
      *****************************************************************
       4102-SYORIKBN-CHK-SEC              SECTION.
      *
      *    処理区分がエラーでないこと
           IF    ( SPA-GMN-I01-ERRCD (2)   NOT =   SPACE )
               MOVE    SPA-GMN-I01-ERRCD (2)
                                   TO  SPA-ERRCD
               MOVE    2           TO  SPA-GMN-I01-CUR
               GO  TO  4102-SYORIKBN-CHK-EXT
           END-IF
      *
      *    処理区分が入力されていること
           IF    ( SPA-GMN-I01-SYORIKBN    =   SPACE )
               MOVE    "1002"      TO  SPA-ERRCD
               MOVE    2           TO  SPA-GMN-I01-CUR
               GO  TO  4102-SYORIKBN-CHK-EXT
           END-IF
      *
      *    処理区分が照会の場合、カーソルは常に処理区分におく
           IF    ( SPA-GMN-I01-SYORIKBN    =   "4" )
               MOVE    2           TO  SPA-GMN-I01-CUR
               GO  TO  4102-SYORIKBN-CHK-EXT
           END-IF
      *
           .
      *
       4102-SYORIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処理区分サーチ処理
      *****************************************************************
       4103-CMB-SYORIKBN-SRH-SEC   SECTION.
      *
           SET     CMB-IDX01       TO  1
           SEARCH  SPA-GMN-I01-SYORIKBN-ITM
                   VARYING CMB-IDX01
           AT  END
               INITIALIZE          WRK-CMB-ITM
                                   WRK-CMB-IDX
           WHEN    SPA-GMN-I01-SYORIKBN-ITM (CMB-IDX01) (1 : 2)
                                   =   WRK-CMB-CD
               SET WRK-CMB-IDX     TO  CMB-IDX01
               MOVE    SPA-GMN-I01-SYORIKBN-ITM (CMB-IDX01)
                                   TO  WRK-CMB-ITM
           WHEN    SPA-GMN-I01-SYORIKBN-ITM (CMB-IDX01)
                                   =   SPACE
               INITIALIZE          WRK-CMB-ITM
                                   WRK-CMB-IDX
           END-SEARCH
      *
           .
      *
       4103-CMB-SYORIKBN-SRH-EXT.
           EXIT.
      *****************************************************************
      *    病棟名サーチ処理
      *****************************************************************
       4103-CMB-BTUNAME-SRH-SEC    SECTION.
      *
           SET     CMB-IDX02       TO  1
           SEARCH  SPA-GMN-I01-BTUNAME-ITM-OCC
                   VARYING CMB-IDX02
           AT  END
               INITIALIZE          WRK-CMB-ITM
                                   WRK-CMB-ITM2
                                   WRK-CMB-ITM3
                                   WRK-CMB-ITM4
                                   WRK-CMB-IDX
           WHEN    SPA-GMN-I01-BTUNAME-ITM (CMB-IDX02) (1 : 2)
                                   =   WRK-CMB-CD
               SET WRK-CMB-IDX     TO  CMB-IDX02
               MOVE    SPA-GMN-I01-BTUNAME-ITM (CMB-IDX02)
                                   TO  WRK-CMB-ITM
               MOVE    SPA-GMN-I01-BTUNAME-ITM2 (CMB-IDX02)
                                   TO  WRK-CMB-ITM2
               MOVE    SPA-GMN-I01-BTUNAME-ITM3 (CMB-IDX02)
                                   TO  WRK-CMB-ITM3
               MOVE    SPA-GMN-I01-BTUNAME-ITM4 (CMB-IDX02)
                                   TO  WRK-CMB-ITM4
           WHEN    SPA-GMN-I01-BTUNAME-ITM (CMB-IDX02)
                                   =   SPACE
               INITIALIZE          WRK-CMB-ITM
                                   WRK-CMB-ITM2
                                   WRK-CMB-ITM3
                                   WRK-CMB-ITM4
                                   WRK-CMB-IDX
           END-SEARCH
      *
           .
      *
       4103-CMB-BTUNAME-SRH-EXT.
           EXIT.
      *****************************************************************
      *    退院事由サーチ処理
      *****************************************************************
       4103-CMB-TAIINCD-SRH-SEC    SECTION.
      *
           INITIALIZE              WRK-CMB-ITM
                                   WRK-CMB-IDX
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I01-TAIINCD-CNT )
                    OR   ( WRK-CMB-ITM     NOT =   SPACE )
      *
               IF    ( SPA-GMN-I01-TAIINCD-ITM (IDX-CMB) (1:2)
                                               =   WRK-CMB-CD )
                   MOVE    SPA-GMN-I01-TAIINCD-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4103-CMB-TAIINCD-SRH-EXT.
           EXIT.
      *****************************************************************
      *    室料差額サーチ処理
      *****************************************************************
       4103-CMB-SAGAKU-SRH-SEC     SECTION.
      *
           SET     CMB-IDX03       TO  1
           SEARCH  SPA-GMN-I01-SAGAKU-ITM
                   VARYING CMB-IDX03
           AT  END
               INITIALIZE          WRK-CMB-ITM
                                   WRK-CMB-IDX
           WHEN    SPA-GMN-I01-SAGAKU-ITM (CMB-IDX03) (1 : 2)
                                   =   WRK-CMB-CD
               SET WRK-CMB-IDX     TO  CMB-IDX03
               MOVE    SPA-GMN-I01-SAGAKU-ITM (CMB-IDX03)
                                   TO  WRK-CMB-ITM
           WHEN    SPA-GMN-I01-SAGAKU-ITM (CMB-IDX03)
                                   =   SPACE
               INITIALIZE          WRK-CMB-ITM
                                   WRK-CMB-IDX
           END-SEARCH
      *
           .
      *
       4103-CMB-SAGAKU-SRH-EXT.
           EXIT.
      *****************************************************************
      *    入院科サーチ処理
      *****************************************************************
       4103-CMB-NYUINKA-SRH-SEC    SECTION.
      *
           SET     CMB-IDX04       TO  1
           SEARCH  SPA-GMN-I01-NYUINKA-ITM
                   VARYING CMB-IDX04
           AT  END
               INITIALIZE          WRK-CMB-ITM
                                   WRK-CMB-IDX
           WHEN    SPA-GMN-I01-NYUINKA-ITM (CMB-IDX04) (1 : 2)
                                   =   WRK-CMB-CD
               SET WRK-CMB-IDX     TO  CMB-IDX04
               MOVE    SPA-GMN-I01-NYUINKA-ITM (CMB-IDX04)
                                   TO  WRK-CMB-ITM
           WHEN    SPA-GMN-I01-NYUINKA-ITM (CMB-IDX04)
                                   =   SPACE
               INITIALIZE          WRK-CMB-ITM
                                   WRK-CMB-IDX
           END-SEARCH
      *
           .
      *
       4103-CMB-NYUINKA-SRH-EXT.
           EXIT.
      *****************************************************************
      *    初回サーチ処理
      *****************************************************************
       4103-CMB-SHOKAI-SRH-SEC     SECTION.
      *
           SET     CMB-IDX05       TO  1
           SEARCH  SPA-GMN-I01-SHOKAI-ITM
                   VARYING CMB-IDX05
           AT  END
               INITIALIZE          WRK-CMB-ITM
                                   WRK-CMB-IDX
           WHEN    SPA-GMN-I01-SHOKAI-ITM (CMB-IDX05) (1 : 1)
                                   =   WRK-CMB-CD
               SET WRK-CMB-IDX     TO  CMB-IDX05
               MOVE    SPA-GMN-I01-SHOKAI-ITM (CMB-IDX05)
                                   TO  WRK-CMB-ITM
           WHEN    SPA-GMN-I01-SHOKAI-ITM (CMB-IDX05)
                                   =   SPACE
               INITIALIZE          WRK-CMB-ITM
                                   WRK-CMB-IDX
           END-SEARCH
      *
           .
      *
       4103-CMB-SHOKAI-SRH-EXT.
           EXIT.
      *****************************************************************
      *    担当医サーチ処理
      *****************************************************************
       4103-CMB-DR-SRH-SEC         SECTION.
      *
           INITIALIZE              WRK-CMB-ITM
                                   WRK-CMB-IDX
      *
           MOVE    SPA-GMN-I01-DR-CNT (IDX1)
                                   TO  WRK-CNT
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL  (IDX2    >   WRK-CNT )
                   OR     (WRK-CMB-ITM NOT =   SPACE )
                   OR     (SPA-GMN-I01-DR-ITM (IDX1 IDX2)
                                           =   SPACE )
               IF    ( SPA-GMN-I01-DR-ITM (IDX1 IDX2) (1 : 4)
                                   =   WRK-CMB-CD  )
                   MOVE    IDX2    TO  WRK-CMB-IDX
                   MOVE    SPA-GMN-I01-DR-ITM (IDX1 IDX2)
                                   TO  WRK-CMB-ITM
               END-IF
           END-PERFORM
      *
           .
      *
       4103-CMB-DR-SRH-EXT.
           EXIT.
      *****************************************************************
      *    保険組み合わせサーチ処理
      *****************************************************************
       4103-CMB-HKNCOMBI-SRH-SEC   SECTION.
      *
           SET     CMB-IDX07       TO  1
           SEARCH  SPA-GMN-I01-HKNCOMBI-ITM-OCC
                   VARYING CMB-IDX07
           AT  END
               INITIALIZE          WRK-CMB-ITM
                                   WRK-CMB-ITM2
                                   WRK-CMB-ITM3
                                   WRK-CMB-IDX
           WHEN    SPA-GMN-I01-HKNCOMBI-ITM (CMB-IDX07) (1 : 4)
                                   =   WRK-CMB-CD
               SET WRK-CMB-IDX     TO  CMB-IDX07
               MOVE    SPA-GMN-I01-HKNCOMBI-ITM (CMB-IDX07)
                                   TO  WRK-CMB-ITM
               MOVE    SPA-GMN-I01-HKNCOMBI-ITM2 (CMB-IDX07)
                                   TO  WRK-CMB-ITM2
               MOVE    SPA-GMN-I01-HKNCOMBI-ITM3 (CMB-IDX07)
                                   TO  WRK-CMB-ITM3
           WHEN    SPA-GMN-I01-HKNCOMBI-ITM (CMB-IDX07)
                                   =   SPACE
               INITIALIZE          WRK-CMB-ITM
                                   WRK-CMB-ITM2
                                   WRK-CMB-ITM3
                                   WRK-CMB-IDX
           END-SEARCH
      *
           .
      *
       4103-CMB-HKNCOMBI-SRH-EXT.
           EXIT.
      *****************************************************************
      *    特定入院サーチ処理
      *    コンボに表示されている番号でサーチ
      *****************************************************************
       4103-CMB-TOKNYUIN-SRH-SEC   SECTION.
      *
           SET     CMB-IDX08       TO  1
           SEARCH  SPA-GMN-I01-TOKNYUIN-ITM-OCC
                   VARYING CMB-IDX08
           AT  END
               INITIALIZE          WRK-CMB-ITM
                                   WRK-CMB-ITM2
                                   WRK-CMB-IDX
           WHEN    SPA-GMN-I01-TOKNYUIN-ITM (CMB-IDX08) (1 : 2)
                                   =   WRK-CMB-CD
               SET WRK-CMB-IDX     TO  CMB-IDX08
               MOVE    SPA-GMN-I01-TOKNYUIN-ITM (CMB-IDX08)
                                   TO  WRK-CMB-ITM
               MOVE    SPA-GMN-I01-TOKNYUIN-ITM2 (CMB-IDX08)
                                   TO  WRK-CMB-ITM2
           WHEN    SPA-GMN-I01-TOKNYUIN-ITM (CMB-IDX08)
                                   =   SPACE
               INITIALIZE          WRK-CMB-ITM
                                   WRK-CMB-ITM2
                                   WRK-CMB-IDX
           END-SEARCH
      *
           .
      *
       4103-CMB-TOKNYUIN-SRH-EXT.
           EXIT.
      *****************************************************************
      *    特定入院サーチ処理２
      *    実際のコードでサーチ
      *****************************************************************
       4103-CMB-TOKNYUIN-SRH2-SEC  SECTION.
      *
           SET     CMB-IDX08       TO  1
           SEARCH  SPA-GMN-I01-TOKNYUIN-ITM-OCC
                   VARYING CMB-IDX08
           AT  END
               INITIALIZE          WRK-CMB-ITM
                                   WRK-CMB-ITM2
                                   WRK-CMB-IDX
           WHEN    SPA-GMN-I01-TOKNYUIN-ITM2 (CMB-IDX08)
                                   =   WRK-CMB-CD
               SET WRK-CMB-IDX     TO  CMB-IDX08
               MOVE    SPA-GMN-I01-TOKNYUIN-ITM (CMB-IDX08)
                                   TO  WRK-CMB-ITM
               MOVE    SPA-GMN-I01-TOKNYUIN-ITM2 (CMB-IDX08)
                                   TO  WRK-CMB-ITM2
           WHEN    SPA-GMN-I01-TOKNYUIN-ITM2 (CMB-IDX08)
                                   =   SPACE
               INITIALIZE          WRK-CMB-ITM
                                   WRK-CMB-ITM2
                                   WRK-CMB-IDX
           END-SEARCH
      *
           .
      *
       4103-CMB-TOKNYUIN-SRH2-EXT.
           EXIT.
      *****************************************************************
      *    定期請求サーチ処理
      *****************************************************************
       4103-CMB-SEIKYU-SRH-SEC         SECTION.
      *
           SET     CMB-IDX09       TO  1
           SEARCH  SPA-GMN-I01-SEIKYU-ITM
                   VARYING CMB-IDX09
           AT  END
               INITIALIZE          WRK-CMB-ITM
                                   WRK-CMB-IDX
           WHEN    SPA-GMN-I01-SEIKYU-ITM (CMB-IDX09) (1 : 1)
                                   =   WRK-CMB-CD
               SET WRK-CMB-IDX     TO  CMB-IDX09
               MOVE    SPA-GMN-I01-SEIKYU-ITM (CMB-IDX09)
                                   TO  WRK-CMB-ITM
           WHEN    SPA-GMN-I01-SEIKYU-ITM (CMB-IDX09)
                                   =   SPACE
               INITIALIZE          WRK-CMB-ITM
                                   WRK-CMB-IDX
           END-SEARCH
      *
           .
      *
       4103-CMB-SEIKYU-SRH-EXT.
           EXIT.
      *****************************************************************
      *    検索時患者表示サーチ処理
      *****************************************************************
       4103-CMB-DISPKBN-SRH-SEC    SECTION.
      *
           SET     CMB-IDX10       TO  1
           SEARCH  SPA-GMN-I01-DISPKBN-ITM
                   VARYING CMB-IDX10
           AT  END
               INITIALIZE          WRK-CMB-ITM
                                   WRK-CMB-IDX
           WHEN    SPA-GMN-I01-DISPKBN-ITM (CMB-IDX10) (1 : 1)
                                   =   WRK-CMB-CD
               SET WRK-CMB-IDX     TO  CMB-IDX10
               MOVE    SPA-GMN-I01-DISPKBN-ITM (CMB-IDX10)
                                   TO  WRK-CMB-ITM
           WHEN    SPA-GMN-I01-DISPKBN-ITM (CMB-IDX10)
                                   =   SPACE
               INITIALIZE          WRK-CMB-ITM
                                   WRK-CMB-IDX
           END-SEARCH
      *
           .
      *
       4103-CMB-DISPKBN-SRH-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           EVALUATE    SPA-I0-MOTOPG
           WHEN    "I20"
           WHEN    "K02N"
               MOVE    SPA-I0-MOTOPG   TO  SPA-SAKIPG
           WHEN    OTHER
               MOVE    "M01"           TO  SPA-SAKIPG
           END-EVALUATE
      *
           MOVE    "I01"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  SPA-WORK 
           INITIALIZE                      SPA-KYOTUKEY
                                           SPA-KYOTU-SET
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
      *
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    クリア処理
      *****************************************************************
       420-CLEAR-SEC                   SECTION.
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           INITIALIZE    SPA-GMN-I01-HEAD-AREA
                         SPA-NAI-I01-HEAD-AREA
                         SPA-GMN-I01-NYUREKILST-AREA
                         SPA-NAI-I01-NYUREKILST-AREA
                         SPA-GMN-I01-ERRCD-TBL
                         SPA-GMN-I01-CMB-BTUNAME
                         SPA-GMN-I01-CMB-HKNCOMBI
      *
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
           PERFORM 310-CMB-SYORIKBN-SET1-SEC
      *
           PERFORM 4201-WIDGET-STATE-CLEAR-SEC
      *
           MOVE    1                   TO  SPA-GMN-I01-CUR
      *
      *
           .
      *
       420-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回患者設定処理
      *****************************************************************
       420-MAEPTINF-SET-SEC            SECTION.
      *
      *    前回患者が設定されているばあは前回患者を表示する
           IF    ( SPA-LAST-PTNUM  NOT =   SPACE )
               MOVE    SPA-LAST-PTID   TO  SPA-NAI-I01-PTID
               MOVE    SPA-LAST-PTNUM  TO  SPA-GMN-I01-PTNUM
               PERFORM 4101-PTINF-SELECT-SEC 
           END-IF
      *
           .
      *
       420-MAEPTINF-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力領域初期化処理
      *****************************************************************
       4201-INPUT-AREA-CLEAR-SEC       SECTION.
      *
           INITIALIZE              SPA-GMN-I01-INPUT-AREA
                                   SPA-NAI-I01-INPUT-AREA
                                   SPA-GMN-I01-CMB-BTUNAME
                                   SPA-GMN-I01-CMB-HKNCOMBI
                                   SPA-GMN-I01-CMB-TOKNYUIN
      *
           MOVE    SPA-GMN-I01-SHOKAI-ITM (01)
                                       TO  SPA-GMN-I01-SHOKAI
           MOVE    SPA-GMN-I01-SEIKYU-ITM (01)
                                   TO  SPA-GMN-I01-SEIKYU
           MOVE    SPA-GMN-I01-DISPKBN-ITM (1)
                                   TO  SPA-GMN-I01-DISPKBN
      *
      *    入院日にシステム日付を表示
           MOVE    SPA-SYSYMD      TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-I01-NYUINYMD
           MOVE    WRK-SYMD        TO  SPA-NAI-I01-NYUINYMD
                                       SPA-NAI-I01-NYUINYMD-MAE
      *
           MOVE    ZERO            TO  SPA-GMN-I01-NYUREKILST-SEL
      *
           .
      *
       4201-INPUT-AREA-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面状態初期化処理
      *****************************************************************
       4201-WIDGET-STATE-CLEAR-SEC     SECTION.
      *
           MOVE    WIDGET-NORMAL   TO  SPA-GMN-I01-B07-STT
                                       SPA-GMN-I01-B08-STT
                                       SPA-GMN-I01-B12-STT
                                       SPA-GMN-I01-IDOYMD-STT
                                       SPA-GMN-I01-BRMNUM-STT
                                       SPA-GMN-I01-BTUNAME-STT
                                       SPA-GMN-I01-BTUNAME-ITM-STT
                                       SPA-GMN-I01-SAGAKU-STT
                                       SPA-GMN-I01-SAGAKU-ITM-STT
                                       SPA-GMN-I01-NYUINYMD-STT
                                       SPA-GMN-I01-NYUINKA-STT
                                       SPA-GMN-I01-NYUINKA-ITM-STT
                                       SPA-GMN-I01-SHOKAI-STT
                                       SPA-GMN-I01-SHOKAI-ITM-STT
                                       SPA-GMN-I01-SHONUM-STT
                                       SPA-GMN-I01-TAIINYMD-STT
                                       SPA-GMN-I01-TAIINCD-STT
                                       SPA-GMN-I01-TAIINCD-ITM-STT
                                       SPA-GMN-I01-DR-STT (1)
                                       SPA-GMN-I01-DR-ITM-STT (1)
                                       SPA-GMN-I01-DR-STT (2)
                                       SPA-GMN-I01-DR-ITM-STT (2)
                                       SPA-GMN-I01-DR-STT (3)
                                       SPA-GMN-I01-DR-ITM-STT (3)
                                       SPA-GMN-I01-HKNCOMBI-STT
                                       SPA-GMN-I01-HKNCOMBI-ITM-STT
                                       SPA-GMN-I01-TOKNYUIN-STT
                                       SPA-GMN-I01-TOKNYUIN-ITM-STT
                                       SPA-GMN-I01-SNTNYUIN-STT
                                       SPA-GMN-I01-MAEUKE-STT
                                       SPA-GMN-I01-SEIKYU-STT
                                       SPA-GMN-I01-SEIKYU-ITM-STT
                                       SPA-GMN-I01-DISPKBN-STT
                                       SPA-GMN-I01-DISPKBN-ITM-STT
      *
           .
      *
       4201-WIDGET-STATE-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    画面状態非活性処理
      *****************************************************************
       4201-WIDGET-INSENSITIVE-SEC     SECTION.
      *
           MOVE    WIDGET-INSENSITIVE
                                   TO  SPA-GMN-I01-B07-STT
                                       SPA-GMN-I01-B08-STT
                                       SPA-GMN-I01-B12-STT
                                       SPA-GMN-I01-IDOYMD-STT
                                       SPA-GMN-I01-BRMNUM-STT
                                       SPA-GMN-I01-BTUNAME-STT
                                       SPA-GMN-I01-BTUNAME-ITM-STT
                                       SPA-GMN-I01-SAGAKU-STT
                                       SPA-GMN-I01-SAGAKU-ITM-STT
                                       SPA-GMN-I01-NYUINYMD-STT
                                       SPA-GMN-I01-NYUINKA-STT
                                       SPA-GMN-I01-NYUINKA-ITM-STT
                                       SPA-GMN-I01-SHOKAI-STT
                                       SPA-GMN-I01-SHOKAI-ITM-STT
                                       SPA-GMN-I01-SHONUM-STT
                                       SPA-GMN-I01-TAIINYMD-STT
                                       SPA-GMN-I01-TAIINCD-STT
                                       SPA-GMN-I01-TAIINCD-ITM-STT
                                       SPA-GMN-I01-DR-STT (1)
                                       SPA-GMN-I01-DR-ITM-STT (1)
                                       SPA-GMN-I01-DR-STT (2)
                                       SPA-GMN-I01-DR-ITM-STT (2)
                                       SPA-GMN-I01-DR-STT (3)
                                       SPA-GMN-I01-DR-ITM-STT (3)
                                       SPA-GMN-I01-HKNCOMBI-STT
                                       SPA-GMN-I01-HKNCOMBI-ITM-STT
                                       SPA-GMN-I01-TOKNYUIN-STT
                                       SPA-GMN-I01-TOKNYUIN-ITM-STT
                                       SPA-GMN-I01-SNTNYUIN-STT
                                       SPA-GMN-I01-MAEUKE-STT
                                       SPA-GMN-I01-SEIKYU-STT
                                       SPA-GMN-I01-SEIKYU-ITM-STT
                                       SPA-GMN-I01-DISPKBN-STT
                                       SPA-GMN-I01-DISPKBN-ITM-STT
      *
           .
       4201-WIDGET-INSENSITIVE-EXT.
           EXIT.
      *****************************************************************
      *    患者登録へ
      *****************************************************************
       480-KANJATOROKU-SEC             SECTION.
      *
           MOVE    SPA-NAI-I01-PTID    TO  SPA-PTID
           MOVE    SPA-GMN-I01-PTNUM   TO  SPA-PTNUM
           PERFORM 481-KANJATOROKU-SEC
      *
          .
       480-KANJATOROKU-EXT.
           EXIT.
      *****************************************************************
      *    患者登録へ
      *****************************************************************
       481-KANJATOROKU-SEC             SECTION.
      *
      *    患者登録の権限がない時、エラー
           IF      SPA-GSRAUTH-12      =   "1"
               CONTINUE
           ELSE
               MOVE    "9300"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-I01-CUR
               GO  TO  481-KANJATOROKU-EXT
           END-IF
      *
      *    ＳＰＡの内容を退避
           INITIALIZE                  I0SUB01-LNK
           MOVE    SPA-TERMID          TO  I0SUB01-TERMID
           MOVE    "O"                 TO  I0SUB01-KBN
           MOVE    SPA-I0FREE          TO  I0SUB01-SCRSPA-AREA
           MOVE    SPA-I0KYOUTU        TO  I0SUB01-COMMON
      *
           CALL    "ORCGI0SUB01"       USING   I0SUB01-LNK
      *
           MOVE    "I01"               TO  SPA-MOTOPG
           MOVE    "I01"               TO  SPA-MOTOPG2
           MOVE    "P02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      SPA-WORK
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "P02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
          .
       481-KANJATOROKU-EXT.
           EXIT.
      *****************************************************************
      *    入院歴画面遷移処理
      *****************************************************************
       480-NYUINREKI-SEC               SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    選択番号未入力チェック
           IF    ( SPA-GMN-I01-SELNUM  =   ZERO )
               MOVE    "1019"      TO  SPA-ERRCD
               MOVE    100         TO  SPA-GMN-I01-CUR
               GO  TO  480-NYUINREKI-EXT
           END-IF
      *
      *    選択番号チェック（基本チェック）
           PERFORM 4101-SELNUM-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-NYUINREKI-EXT
           END-IF
      *
           MOVE    SPA-NAI-I01-PTID
                                   TO  SPA-I01-I02-PTID
           MOVE    SPA-NAI-I01-TRRKNUM (SPA-GMN-I01-SELNUM)
                                   TO  SPA-I01-I02-PTRRKNUM
      *
           MOVE    SPACE           TO  SPA-MOTOPG
      *
           MOVE    SPA-I0KYOUTU    TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-I0FREE      TO  SPA-FREE
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGI02"      USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-I0FREE
           MOVE    SPA-WORK        TO  SPA-I0KYOUTU
      *
           IF    ( SPA-NAI-I01-TZITA-FLG (SPA-GMN-I01-SELNUM)
                                   =   ZERO  )
               MOVE    "NYUGAI"        TO  MCP-WIDGET
           ELSE
               MOVE    "B01"           TO  MCP-WIDGET
           END-IF
      *
           MOVE    "I01"           TO  SPA-MOTOPG
           MOVE    "I02"           TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "I02"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE  1             TO  FLG-END
      *
           .
       480-NYUINREKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院歴作成画面遷移処理（作成処理）
      *****************************************************************
       480-NYUINRRK-SAKUSEI-SEC        SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    "1"                 TO  SPA-I01-I03-SYORIKBN
           MOVE    SPA-NAI-I01-PTID    TO  SPA-I01-I03-PTID
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPID          TO  PTNYUINRRK-HOSPID
           MOVE    SPA-NAI-I01-PTID    TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "PTNYUINRRK-KEY9"   TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "PTNYUINRRK-KEY9"   TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF    ( FLG-PTNYUINRRK  NOT =   ZERO )
               COMPUTE     SPA-I01-I03-PTRRKNUM    =   1
               COMPUTE     SPA-I01-I03-PTRRKEDANUM =   1
           ELSE
               COMPUTE     SPA-I01-I03-PTRRKNUM    =   PTNYUINRRK-RRKNUM
                                                   +   1
               COMPUTE     SPA-I01-I03-PTRRKEDANUM =   1
           END-IF
      *
           MOVE    SPA-GMN-I01-SYORIKBN-CD
                                   TO  SPA-I01-I03-SYORIKBN-CD
      *
           MOVE    SPA-GMN-I01-NYUREKILST-SEL
                                   TO  IDX1
           IF    ( IDX1            >   ZERO )
            AND  ( IDX1            <=  SPA-GMN-I01-NYUREKILST-CNT )
               MOVE    SPA-GMN-I01-TSHONUM (IDX1)
                                   TO  SPA-I01-I03-SHONUM
           ELSE
               MOVE    ZERO        TO  SPA-I01-I03-SHONUM
           END-IF
      *
           MOVE    ZERO            TO  SPA-I01-I03-TAIINYMD
           IF    ( SPA-I01-I03-SHONUM      NOT =   ZERO )
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL     ( IDX2    >    SPA-GMN-I01-NYUREKILST-CNT )
                   IF    ( SPA-I01-I03-SHONUM
                                   =   SPA-GMN-I01-TSHONUM (IDX2) )
                       MOVE    SPA-NAI-I01-TTAIINYMD (IDX2)
                                   TO  SPA-I01-I03-TAIINYMD
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    SPACE           TO  SPA-MOTOPG
           MOVE    SPA-I0KYOUTU    TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-I0FREE      TO  SPA-FREE
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGI03"      USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-I0FREE
           MOVE    SPA-WORK        TO  SPA-I0KYOUTU
      *
           MOVE    "SHOKAI"        TO  MCP-WIDGET
      *
           MOVE    "I01"           TO  SPA-MOTOPG
           MOVE    "I03"           TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "I03"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE  1             TO  FLG-END
      *
           .
       480-NYUINRRK-SAKUSEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院歴作成画面遷移処理（修正処理）
      *****************************************************************
       480-NYUINRRK-SYUSEI-SEC         SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    歴が選択されていること
           IF    ( SPA-GMN-I01-NYUREKILST-SEL >   ZERO )
            AND  ( SPA-GMN-I01-NYUREKILST-SEL
                              <=   SPA-GMN-I01-NYUREKILST-CNT )
               CONTINUE
           ELSE
               MOVE    "1023"          TO  SPA-ERRCD
               GO  TO  480-NYUINRRK-SYUSEI-EXT
           END-IF
      *
           MOVE    SPA-GMN-I01-NYUREKILST-SEL
                                       TO  IDX1
      *
      *    歴作成データが選択されていること
           IF    ( SPA-NAI-I01-TZITA-FLG (IDX1)    =   ZERO )
               MOVE    "1023"          TO  SPA-ERRCD
               GO  TO  480-NYUINRRK-SYUSEI-EXT
           END-IF
      *
      *    歴作成データが最新であること
           MOVE    ZERO                TO  FLG-RRKCHK
           COMPUTE IDX3    =   IDX1    -   1
           PERFORM VARYING IDX2    FROM    IDX3    BY  -1
                   UNTIL ( IDX2    <   1 )
                   OR    ( FLG-RRKCHK      NOT =   ZERO )
               IF    ( SPA-GMN-I01-TSHONUM (IDX1)
                                   =   SPA-GMN-I01-TSHONUM (IDX2) )
                   MOVE    1       TO  FLG-RRKCHK
                   MOVE    1       TO  IDX2
               END-IF
           END-PERFORM
      *
           IF    ( FLG-RRKCHK       =   ZERO )
               MOVE    "2"             TO  SPA-I01-I03-SYORIKBN
           ELSE
               MOVE    "3"             TO  SPA-I01-I03-SYORIKBN
           END-IF
      *
           MOVE    SPA-NAI-I01-PTID
                                   TO  SPA-I01-I03-PTID
           MOVE    SPA-NAI-I01-TRRKNUM (IDX1)
                                   TO  SPA-I01-I03-PTRRKNUM
           MOVE    1               TO  SPA-I01-I03-PTRRKEDANUM
           MOVE    SPA-GMN-I01-SYORIKBN-CD
                                   TO  SPA-I01-I03-SYORIKBN-CD
           MOVE    ZERO            TO  SPA-I01-I03-TAIINYMD
           IF    ( SPA-GMN-I01-TSHOKAI (IDX1)  NOT =   "1" )
               COMPUTE IDX3    =   IDX1    +   1
               PERFORM VARYING IDX2    FROM    IDX3    BY  1
                   UNTIL     ( IDX2    >    SPA-GMN-I01-NYUREKILST-CNT )
                   IF    ( SPA-GMN-I01-TSHONUM (IDX1)
                                   =   SPA-GMN-I01-TSHONUM (IDX2) )
                       MOVE    SPA-NAI-I01-TTAIINYMD (IDX2)
                                   TO  SPA-I01-I03-TAIINYMD
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    SPACE           TO  SPA-MOTOPG
           MOVE    SPA-I0KYOUTU    TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-I0FREE      TO  SPA-FREE
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGI03"      USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-I0FREE
           MOVE    SPA-WORK        TO  SPA-I0KYOUTU
      *
           MOVE    "COMMENT"       TO  MCP-WIDGET
      *
           MOVE    "I01"           TO  SPA-MOTOPG
           MOVE    "I03"           TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "I03"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE  1             TO  FLG-END
      *
           .
       480-NYUINRRK-SYUSEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名検索処理
      *****************************************************************
       480-KENSAKU-SEC                 SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-I0FREE          TO  LNK-SCRSPA
      *
           MOVE    ZERO                TO  SPA-PTID
      *
           MOVE    "I01"               TO  SPA-MOTOPG
           MOVE    "P97"               TO  SPA-SAKIPG
      *    氏名をクリアする
           MOVE    SPACE               TO  SPA-NAME
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "P97"               TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
           .
      *
       480-KENSAKU-EXT.
           EXIT.
      *****************************************************************
      *    カルテ発行処理
      *****************************************************************
       480-KARUTE-PRT-SEC              SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-KARUTE-PRT-EXT
           END-IF
      *
           MOVE    "2"                 TO  WRK-ORCHCN01-SYORIKBN
           MOVE    "1"                 TO  WRK-ORCHCN01-PRTKBN
      *
      *    入院カルテ編集サブ呼出
           PERFORM 4801-ORCHCN01-CALL-SEC
      *
           .
       480-KARUTE-PRT-EXT.
           EXIT.
      *****************************************************************
      *    続紙発行処理
      *****************************************************************
       480-ZOKUSI-PRT-SEC              SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-ZOKUSI-PRT-EXT
           END-IF
      *
           MOVE    "1"                 TO  WRK-ORCHCN01-SYORIKBN
           MOVE    "2"                 TO  WRK-ORCHCN01-PRTKBN
      *
      *    入院カルテ編集サブ呼出
           PERFORM 4801-ORCHCN01-CALL-SEC
      *
           .
       480-ZOKUSI-PRT-EXT.
           EXIT.
      *****************************************************************
      *    入院診療禄サブ呼出処理
      *****************************************************************
       4801-ORCHCN01-CALL-SEC          SECTION.
      *
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
           INITIALIZE                      WRK-SPA-KYOTUKEY
                                           WRK-SPA-KYOTU-SET
                                           ORCHCN01AREA
      *
           MOVE    WRK-ORCHCN01-SYORIKBN
                                       TO  ORCHCN01-SYORIKBN
           MOVE    WRK-ORCHCN01-PRTKBN
                                       TO  ORCHCN01-PRTKBN
           MOVE    SPA-NAI-I01-PTID    TO  WRK-SPA-PTID
           MOVE    SPA-GMN-I01-PTNUM   TO  WRK-SPA-PTNUM
           MOVE    SPA-GMN-I01-NYUINKA-CD
                                       TO  WRK-SPA-SRYKA
      *    カルテ編集用保険組合せ決定処理
           PERFORM 4801-HKNCOMBI-KETTEI-SEC
      *    MOVE    SPA-GMN-I01-HKNCOMBI-CD
      *                                TO  WRK-SPA-HKNCOMBINUM
           IF    ( WRK-ORCHCN01-HKNCOMBINUM    =   ZERO )
               MOVE    SPACE           TO  WRK-SPA-HKNCOMBINUM
           ELSE
               MOVE    WRK-ORCHCN01-HKNCOMBINUM
                                       TO  WRK-SPA-HKNCOMBINUM
           END-IF
      *
           MOVE    ZERO                TO  IDX2
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   3 )
               IF    ( SPA-GMN-I01-DR-CD (IDX1)
                                   NOT =   SPACE )
                   COMPUTE IDX2    =   IDX2    +   1
                   MOVE    "1"         TO  ORCHCN01-DR (IDX2) (1 : 1)
                   MOVE    SPA-GMN-I01-DR-CD (IDX1)
                                       TO  ORCHCN01-DR (IDX2) (2 : )
               END-IF
           END-PERFORM
      *
           MOVE    SPA-NAI-I01-NYUINYMD
                                       TO  ORCHCN01-NYUINYMD
      *
      *    DISPLAY "ORCHCN01-SYORIKBN  = " ORCHCN01-SYORIKBN
      *    DISPLAY "ORCHCN01-PRTKBN    = " ORCHCN01-PRTKBN
      *    DISPLAY "SPA-PTID           = " SPA-PTID
      *    DISPLAY "SPA-PTNUM          = " SPA-PTNUM
      *    DISPLAY "SPA-SRYKA          = " SPA-SRYKA
      *    DISPLAY "SPA-HKNCOMBINUM    = " SPA-HKNCOMBINUM
      *    DISPLAY "ORCHCN01-DR (1)    = " ORCHCN01-DR (1)
      *    DISPLAY "ORCHCN01-DR (2)    = " ORCHCN01-DR (2)
      *    DISPLAY "ORCHCN01-DR (3)    = " ORCHCN01-DR (3)
      *    DISPLAY "ORCHCN01-NYUINYMD  = " ORCHCN01-NYUINYMD
      *
      *    帳票プログラム名取得
           PERFORM 4801-PRINT-PG-EDIT-SEC
      *
           CALL    WRK-PRINT-PG    USING   WRK-SPA-AREA
                                           ORCHCN01AREA
      *
           .
       4801-ORCHCN01-CALL-EXT.
           EXIT.
      *****************************************************************
      *   カルテ編集用保険組合せ決定処理
      *****************************************************************
       4801-HKNCOMBI-KETTEI-SEC        SECTION.
      *
           INITIALIZE                  WRK-ORCHCN01-HKNCOMBINUM
      *
           EVALUATE    SPA-GMN-I01-SYORIKBN-CD
           WHEN    "01"
               MOVE    SPA-GMN-I01-HKNCOMBI-CD
                                   TO  WRK-ORCHCN01-HKNCOMBINUM
           WHEN    "08"
               IF    ( SPA-GMN-I01-HKNCOMBI-CD
                               NOT =   SPA-NAI-I01-HKNCOMBI-MOT )
                   MOVE    SPA-GMN-I01-HKNCOMBI-CD
                                   TO  WRK-ORCHCN01-HKNCOMBINUM
               ELSE
                   PERFORM 4801-NYUACCT-HKNCOMBI-GET-SEC
               END-IF
           WHEN    OTHER
                   PERFORM 4801-NYUACCT-HKNCOMBI-GET-SEC
           END-EVALUATE
      *
           .
       4801-HKNCOMBI-KETTEI-EXT.
           EXIT.
      *****************************************************************
      *   入院会計より保険組合せ取得処理
      *****************************************************************
       4801-NYUACCT-HKNCOMBI-GET-SEC   SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           MOVE    ZERO                TO  FLG-HKNCOMBIGET
      *
           IF    ( SPA-NAI-I01-NYUINYMD    <   SPA-SYSYMD )
               MOVE    SPA-SYSYMD      TO  WRK-MIN-YMD
           ELSE
               MOVE    SPA-NAI-I01-NYUINYMD
                                       TO  WRK-MIN-YMD
           END-IF
      *
           INITIALIZE  NYUINACCT-REC
           MOVE    SPA-HOSPID          TO  NACCT-HOSPID
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    SPA-NAI-I01-PTID    TO  NACCT-PTID
           MOVE    WRK-MIN-YM          TO  NACCT-SRYYM
           MOVE    "5"                 TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "NYUINACCT-KEY19"   TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        NOT =   ZERO )
               MOVE    1               TO  FLG-NYUINACCT
           ELSE
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-NYUINACCT   =   1 )
                   OR    ( FLG-HKNCOMBIGET =   1 )
                   OR    ( NACCT-SRYYM <   SPA-NAI-I01-NYUINYMD (1 : 6))
      *
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY7-AREA
               MOVE    "71"                TO  LNK-DAY7-IRAI
               MOVE    NACCT-SRYYM         TO  LNK-DAY7-YM
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
               MOVE    LNK-DAY7-KOYOMI(7:2)
                                           TO  WRK-STIDX
      *
               MOVE    1                   TO  WRK-EDIDX
      *
               PERFORM VARYING IDX-DAY   FROM    WRK-STIDX   BY  -1
                   UNTIL ( IDX-DAY           <   WRK-EDIDX )
                    OR   ( FLG-HKNCOMBIGET =   1 )
      *
                   IF    ( WRK-MIN-YM      =   NACCT-SRYYM )
                       IF    ( WRK-MIN-DD >=  IDX-DAY)
                        AND  ( NACCT-DAY (IDX-DAY)   NOT =   ZERO )
                           MOVE    NACCT-DAY (IDX-DAY)
                                           TO  WRK-ORCHCN01-HKNCOMBINUM
                           MOVE    1       TO  FLG-HKNCOMBIGET
                       END-IF
                   ELSE
                       MOVE    SPA-NAI-I01-NYUINYMD
                                           TO  WRK-NAI-NYUINYMD
                       IF    ( WRK-NAI-NYUINYM
                                           =   NACCT-SRYYM )
                        AND  ( WRK-NAI-NYUINDD
                                           >   IDX-DAY)
                           MOVE    1       TO  IDX-DAY
                       ELSE
                           IF    ( NACCT-DAY (IDX-DAY)   NOT =   ZERO )
                               MOVE    NACCT-DAY (IDX-DAY)
                                           TO  WRK-ORCHCN01-HKNCOMBINUM
                               MOVE    1   TO  FLG-HKNCOMBIGET
                           END-IF
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
               MOVE    "NYUINACCT-KEY19"   TO  WRK-DBPATH
               PERFORM 900-TBL-READ-SEC
               IF    ( FLG-DBRC        NOT =   ZERO )
                   MOVE    1           TO  FLG-NYUINACCT
               ELSE
                   MOVE    MCPDATA-REC TO  NYUINACCT-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "NYUINACCT-KEY19"   TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
       4801-NYUACCT-HKNCOMBI-GET-EXT.
           EXIT.
      *****************************************************************
      *   帳票プログラム名編集処理
      *****************************************************************
       4801-PRINT-PG-EDIT-SEC         SECTION.
      *
           INITIALIZE                      WRK-PRINT-PG
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000001"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "1"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           WRK-SPA-AREA
                                           SYS-1035-REC
           IF    ( ORCSPRTNM-RC        =   ZERO )
              MOVE    ORCSPRTNM-PRTPG  TO  WRK-PRINT-PG
           ELSE
               MOVE    "ORCHCN01"      TO  WRK-PRINT-PG
           END-IF
      *
      *
           .
       4801-PRINT-PG-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院会計照会遷移処理
      *****************************************************************
       480-NYUINKAIKEI-SEC             SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    入院会計照会へ引き継ぐ情報を設定
           PERFORM 4801-SENIJI-PARA-SET-SEC
      *
      *    ＳＰＡの内容を退避
           INITIALIZE                  I0SUB01-LNK
           MOVE    SPA-TERMID          TO  I0SUB01-TERMID
           MOVE    "O"                 TO  I0SUB01-KBN
           MOVE    SPA-I0FREE          TO  I0SUB01-SCRSPA-AREA
           MOVE    SPA-I0KYOUTU        TO  I0SUB01-COMMON
      *
           CALL    "ORCGI0SUB01"       USING   I0SUB01-LNK
      *
      *    入院会計照会へ遷移する
           MOVE    "I01"               TO  SPA-MOTOPG
           MOVE    "I41"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-I0KYOUTU        TO  SPA-WORK
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-I0FREE          TO  LNK-SCRSPA
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "I41"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       480-NYUINKAIKEI-EXT.
           EXIT.
      *****************************************************************
      *    収納登録遷移処理
      *****************************************************************
       480-S02-SENI-SEC                SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    収納登録へ引き継ぐ情報を設定
           PERFORM 4801-SENIJI-PARA-SET-SEC
      *
      *    ＳＰＡの内容を退避
           INITIALIZE                  I0SUB01-LNK
           MOVE    SPA-TERMID          TO  I0SUB01-TERMID
           MOVE    "O"                 TO  I0SUB01-KBN
           MOVE    SPA-I0FREE          TO  I0SUB01-SCRSPA-AREA
           MOVE    SPA-I0KYOUTU        TO  I0SUB01-COMMON
      *
           CALL    "ORCGI0SUB01"       USING   I0SUB01-LNK
      *
      *    収納登録へ遷移する
           MOVE    "I01"               TO  SPA-MOTOPG
           MOVE    "S02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-I0KYOUTU        TO  SPA-WORK
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-I0FREE          TO  LNK-SCRSPA
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "S02"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       480-S02-SENI-EXT.
           EXIT.
      *****************************************************************
      *    オーダー変更画面遷移処理
      *****************************************************************
       480-I06-SENI-SEC                SECTION.
      *
      *    収納登録へ遷移する
           MOVE    "I01"               TO  SPA-MOTOPG
           MOVE    "I06"               TO  SPA-SAKIPG
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "I06"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       480-I06-SENI-EXT.
           EXIT.
      *****************************************************************
      *    他業務引継情報設定処理
      *****************************************************************
       4801-SENIJI-PARA-SET-SEC        SECTION.
      *
           INITIALIZE                  SPA-KYOTUKEY
                                       SPA-KYOTU-SET
      *
      *    入院科を編集
           MOVE    SPACE           TO  WRK-SRYKAMEI
      *
           IF    ( SPA-GMN-I01-NYUINKA NOT =   SPACE )
               MOVE    SPA-GMN-I01-NYUINKA-CD
                                   TO  WRK-CMB-CD
               PERFORM 4103-CMB-NYUINKA-SRH-SEC
               MOVE    WRK-CMB-ITM
                                   TO  WRK-SRYKAMEI
           END-IF
      *
      *    システム日付を診療日付として会計に引き継ぐ
           MOVE    SPA-SYSYMD      TO  SPA-SRYYMD
           MOVE    SPA-SYSYMDWH    TO  SPA-SRYYMDW
      *
      *    入院科が入力されていた場合は会計に引き継ぐ
      *    （患者情報が入力されていた場合に限る）
           IF    ( WRK-SRYKAMEI        NOT =   SPACE )
            AND  ( SPA-NAI-I01-PTID    NOT =   ZERO )
               MOVE    SPA-GMN-I01-NYUINKA-CD
                                   TO  SPA-SRYKA
               MOVE    WRK-SRYKAMEI
                                   TO  SPA-SRYKAMEI
           END-IF
      *
      *    患者情報が入力されていた場合は会計に引継ぐ
           IF    ( SPA-NAI-I01-PTID    NOT =   ZERO )
               MOVE    SPA-NAI-I01-PTID
                                   TO  SPA-PTID
               MOVE    SPA-GMN-I01-PTNUM
                                   TO  SPA-PTNUM
               MOVE    SPA-NAI-I01-KANANAME
                                   TO  SPA-KANANAME
               MOVE    SPA-GMN-I01-NAME
                                   TO  SPA-NAME
               MOVE    SPA-GMN-I01-SEX
                                   TO  SPA-SEX
               MOVE    SPA-GMN-I01-BIRTHDAY
                                   TO  SPA-BIRTHDAYW
               MOVE    SPA-NAI-I01-BIRTHDAY
                                   TO  SPA-BIRTHDAY
           END-IF
      *
           .
       4801-SENIJI-PARA-SET-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       480-TOUROKU-SEC                 SECTION.
      *
           IF    ( SPA-GMN-I01-SYORIKBN-CD  = "02" OR "10"  OR "11")
            AND  ( FLG-TEIKI-CHK       =   1 )
      *
      *        定期請求処理中チェック
               PERFORM 4801-TEIKI-JOB-CHK-SEC
               IF    ( SPA-I0IDCD      NOT =   SPACE )
                   GO  TO  480-TOUROKU-EXT
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-SYORIKBN-CD     =   "10" )
      *
               PERFORM 4101-GMN-SPA-SET-SEC
           ELSE
      *
               PERFORM 410-INPUT-CHK-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  480-TOUROKU-EXT
               END-IF
      *
               PERFORM 4801-KANREN-CHK-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  480-TOUROKU-EXT
               END-IF
           END-IF
      *
           PERFORM 4801-SYORI-KANREN-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-TOUROKU-EXT
           END-IF
      *
           MOVE    SPACE           TO  WRK-UPDKBN
           EVALUATE    SPA-GMN-I01-SYORIKBN-CD
      *    入院登録
           WHEN    "01"
      *
               IF    ( FLG-TAINDOUJITU =   1 )
                   MOVE    "3009"  TO  SPA-I0IDCD
                   GO  TO  480-TOUROKU-EXT
               END-IF
      *
               IF    ( FLG-TAINDOUJITU =   2 )
                   MOVE    "3010"  TO  SPA-I0IDCD
                   GO  TO  480-TOUROKU-EXT
               END-IF
      *
               MOVE    "INS"       TO  WRK-UPDKBN
      *
      *    退院登録
           WHEN    "02"
           WHEN    "11"
      *
               IF    ( FLG-SYUARI  NOT =   ZERO )
                   MOVE    "3005"  TO  SPA-I0IDCD
                   GO  TO  480-TOUROKU-EXT
               END-IF
      *
      *        診療会計テーブルチェック処理
               PERFORM 490-SRYACCT-CHK-SEC
               IF    ( SPA-I0IDCD  NOT =   SPACE )
                 OR  ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  480-TOUROKU-EXT
               END-IF
      *
      *        退院処理
               MOVE    "CHANGE"    TO  WRK-MCP-PUTTYPE
               PERFORM 490-TAIIN-SYORI-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  480-TOUROKU-EXT
               END-IF
               GO  TO  480-TOUROKU-EXT
      *    転科・転棟・転室
           WHEN    "08"
               MOVE    "INS"       TO  WRK-UPDKBN
      *    入院取消
           WHEN    "05"
               IF    ( SPA-NAI-I01-PTRRKEDANUM >   1 )
                   MOVE    "3006"      TO  SPA-I0IDCD
               ELSE
                   MOVE    "3001"      TO  SPA-I0IDCD
               END-IF
               GO  TO  480-TOUROKU-EXT
      *    入院取消（会計含む）
           WHEN    "06"
               IF    ( SPA-NAI-I01-PTRRKEDANUM >   1 )
                   MOVE    "3007"      TO  SPA-I0IDCD
               ELSE
                   MOVE    "3002"      TO  SPA-I0IDCD
               END-IF
               GO  TO  480-TOUROKU-EXT
      *    退院取消
           WHEN    "07"
               MOVE    "3003"      TO  SPA-I0IDCD
               GO  TO  480-TOUROKU-EXT
      *    異動取消
           WHEN    "09"
               MOVE    "DEL"       TO  WRK-UPDKBN
               MOVE    "3101"      TO  SPA-I0IDCD2
               GO  TO  480-TOUROKU-EXT
      *    退院再計算
           WHEN    "10"
               MOVE    "CHANGE"    TO  WRK-MCP-PUTTYPE
               PERFORM 490-TAIIN-SAIKEISAN-SYORI-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  480-TOUROKU-EXT
               END-IF
               GO  TO  480-TOUROKU-EXT
      *    その他
           WHEN    OTHER
               MOVE    "UPD"       TO  WRK-UPDKBN
           END-EVALUATE
      *
           PERFORM 4801-PTNYUINRRK-UPD-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-TOUROKU-EXT
           END-IF
      *
           .
      *
       480-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    患者履歴マスタ更新処理
      *****************************************************************
       4801-PTNYUINRRK-UPD-SEC     SECTION.
      *
      *    入院履歴データ編集
           PERFORM 4801-PTNYUINRRK-EDIT-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4801-PTNYUINRRK-UPD-EXT
           END-IF
      *
           EVALUATE WRK-UPDKBN
           WHEN    "INS"
      *        入院履歴マスタ追加
               MOVE    "PTNYUINRRK-KEY"
                                       TO  WRK-DBPATH
               MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
               PERFORM 900-TBL-INSERT-SEC
           WHEN    "UPD"
      *        入院履歴マスタ更新
               MOVE    "PTNYUINRRK-KEY"
                                       TO  WRK-DBPATH
               MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
               PERFORM 900-TBL-UPDATE-SEC
           WHEN    "DEL"
      *        入院履歴マスタ削除
               IF    ( SPA-GMN-I01-SYORIKBN-CD
                                       =   "05"  OR  "06" )
                   MOVE    "PTNYUINRRK-DEL1"
                                       TO  WRK-DBPATH
               ELSE
                   MOVE    "PTNYUINRRK-KEY"
                                       TO  WRK-DBPATH
               END-IF
               MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
               PERFORM 900-TBL-DELETE-SEC
           END-EVALUATE
      *
           IF    ( FLG-DBRC    NOT =   ZERO )
      *        入院歴を再編集
               PERFORM 4101-NYUREKI-EDIT-SEC
               MOVE    "4001"      TO  SPA-ERRCD
               GO  TO  4801-PTNYUINRRK-UPD-EXT
           END-IF
      *
      *    入院会計テーブルを編集する（入院時・転室時）
      *     IF    ( SPA-GMN-I01-SYORIKBN-CD =   "01"
      *                                     OR  "08" )
           IF      SPA-GMN-I01-SYORIKBN-CD =   "01"
               PERFORM 4801-NYUKAIKEI-EDIT-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  4801-PTNYUINRRK-UPD-EXT
               END-IF
           END-IF
      *
      *    入院会計テーブルを編集する（入院取消時）
           IF     SPA-GMN-I01-SYORIKBN-CD =   "05"  OR  "06"
               PERFORM 4801-NYUKAIKEI-ERASE-SEC
           END-IF
      *
      *    入院会計テーブルを編集する（退院取消時）
           IF     SPA-GMN-I01-SYORIKBN-CD =   "07"
               PERFORM 4801-TAIINKAIKEI-CANCEL-SEC
           END-IF
      *
      *    入院会計テーブルを編集する（転科／転棟／転室時）
           IF     SPA-GMN-I01-SYORIKBN-CD =   "08"
               PERFORM 4801-TENROOM-EDIT-SEC
               IF     IPN-NYUKAIKEI-SAKUSEI-FLG    =   "1"
                   MOVE    SPACE   TO   SPA-GMN-I01-TOKNYUIN-CD
                   MOVE    IPN-SANTEISTYMD   TO   WRK-SANTEISTYMD
                   MOVE    IPN-SANTEISTYMD   TO   SPA-NAI-I01-IDOYMD
                   PERFORM   4801-TENROOM-EDIT-SEC
               END-IF
           END-IF
      *
      *    入院会計テーブルを編集する（異動取消時）
           IF     SPA-GMN-I01-SYORIKBN-CD =   "09"
               PERFORM 4801-IDOKAIKEI-CANCEL-SEC
           END-IF
      *
      *    患者情報の入院歴番号を更新する
      *    主科情報も更新する
           PERFORM 4801-PTINF-UPD-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4801-PTNYUINRRK-UPD-EXT
           END-IF
      *
      *    前回患者情報を設定
           MOVE SPA-NAI-I01-PTID   TO  SPA-LAST-PTID
           MOVE SPA-GMN-I01-PTNUM  TO  SPA-LAST-PTNUM
      *
      *    画面を更新後の内容で再表示
           PERFORM 4101-PTNUM-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4801-PTNYUINRRK-UPD-EXT
           END-IF
           .
      *
       4801-PTNYUINRRK-UPD-EXT.
           EXIT.
      *****************************************************************
      *    定期請求処理中チェック
      *****************************************************************
       4801-TEIKI-JOB-CHK-SEC          SECTION.
      *
      *    ジョブ管理チェック処理
           INITIALIZE                  STEIKIJOB-AREA
           CALL    "ORCSTEIKIJOB"      USING   STEIKIJOB-AREA
           IF    ( STEIKIJOB-ERRCD NOT =   SPACE )
                   MOVE    "3008"      TO  SPA-I0IDCD
           END-IF
      *
           .
       4801-TEIKI-JOB-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       4801-KANREN-CHK-SEC         SECTION.
      *
      *    保険組合せが存在すること
      *    IF    ( SPA-GMN-I01-HKNCOMBI-CNT    >   ZERO )
      *        CONTINUE
      *    ELSE
      *        MOVE    "1021"      TO  SPA-ERRCD
      *        MOVE    1           TO  SPA-GMN-I01-CUR
      *        GO  TO  4801-KANREN-CHK-EXT
      *    END-IF
      *
      *    必須入力チェック
      *    病室番号
           IF    ( SPA-GMN-I01-BRMNUM      =   SPACE )
               MOVE    "1003"      TO  SPA-ERRCD
               MOVE    4           TO  SPA-GMN-I01-CUR
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *
      *    病棟名
           IF    ( SPA-GMN-I01-BTUNAME     =   SPACE )
               MOVE    "1004"      TO  SPA-ERRCD
               MOVE    5           TO  SPA-GMN-I01-CUR
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *
      *    入院日
           IF    ( SPA-GMN-I01-NYUINYMD    =   SPACE )
               MOVE    "1005"      TO  SPA-ERRCD
               MOVE    7           TO  SPA-GMN-I01-CUR
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *
      *    入院科
           IF    ( SPA-GMN-I01-NYUINKA     =   SPACE )
               MOVE    "1006"      TO  SPA-ERRCD
               MOVE    8           TO  SPA-GMN-I01-CUR
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *
      *    初回
           IF    ( SPA-GMN-I01-SHOKAI      =   SPACE )
               MOVE    "1007"      TO  SPA-ERRCD
               MOVE    9           TO  SPA-GMN-I01-CUR
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *
      *    初回履歴番号
           IF    ( SPA-GMN-I01-SHOKAI-CD   =   "2" )
               IF    ( SPA-GMN-I01-SHONUM  =   ZERO )
                   MOVE    "1022"      TO  SPA-ERRCD
                   MOVE    10          TO  SPA-GMN-I01-CUR
                   GO  TO  4801-KANREN-CHK-EXT
               END-IF
           END-IF
      *
      *    担当医
      ***  IF    ( SPA-GMN-I01-DR (1)         =   SPACE )
      *     AND  ( SPA-GMN-I01-DR (2)         =   SPACE )
      *     AND  ( SPA-GMN-I01-DR (3)         =   SPACE )
      *        MOVE    "1008"      TO  SPA-ERRCD
      *        MOVE    11          TO  SPA-GMN-I01-CUR
      *        GO  TO  4801-KANREN-CHK-EXT
      *    END-IF
      *
      *
      *    保険組合せ
           IF    ( SPA-GMN-I01-HKNCOMBI    =   SPACE )
            AND  ( SPA-GMN-I01-SYORIKBN-CD =   "01" OR "08" )
               MOVE    "1009"      TO  SPA-ERRCD
               MOVE    15          TO  SPA-GMN-I01-CUR
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *    労災保険（アフター）
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   "01" OR "08" )
             IF    ( SPA-NAI-I01-RSI-HKNKBN    =   "3" )
               MOVE    "1033"      TO  SPA-ERRCD
               MOVE    15          TO  SPA-GMN-I01-CUR
               GO  TO  4801-KANREN-CHK-EXT
             END-IF
           END-IF
      *
      *    定期請求
           IF    ( SPA-GMN-I01-SEIKYU      =   SPACE )
               MOVE    "1010"      TO  SPA-ERRCD
               MOVE    18          TO  SPA-GMN-I01-CUR
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *
      *    検索時患者表示
           IF    ( SPA-GMN-I01-DISPKBN     =   SPACE )
               MOVE    "1011"      TO  SPA-ERRCD
               MOVE    19          TO  SPA-GMN-I01-CUR
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *
      *    入院基本料チェック
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   "01" OR "08" )
               IF    ( SPA-GMN-I01-TOKNYUIN    =   SPACE )
                   INITIALIZE  SYS-5001-REC
                   MOVE    "5001"              TO  SYS-5001-KANRICD
                   MOVE    SPA-GMN-I01-BTUNAME-CD
                                               TO  SYS-5001-KBNCD
                   MOVE    WRK-KIHON-KJNYMD    TO  ORC-DBYMD
                   MOVE    SYS-5001-REC        TO  MCPDATA-REC
                   MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
                   PERFORM 900-TBL-SELECT-SEC
                   IF    ( FLG-DBRC    =   ZERO )
                       MOVE    MCPDATA-REC     TO  SYS-5001-REC
                   ELSE
                       INITIALIZE                  SYS-5001-REC
                   END-IF
      *
                   MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
                   PERFORM 900-TBL-CLOSE-SEC
      *
                   IF    ( SYS-5001-BTU-KHNSRYCD   =   SPACE )
                       MOVE    "1027"          TO  SPA-ERRCD
                       MOVE    16              TO  SPA-GMN-I01-CUR
                       GO  TO  4801-KANREN-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
           PERFORM 48011-H18-KAISEI-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *
           .
       4801-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処理別関連チェック処理
      *****************************************************************
       48011-H18-KAISEI-CHK-SEC        SECTION.
      *
           MOVE       ZERO         TO  WRK-CHK-YMD
           EVALUATE    SPA-GMN-I01-SYORIKBN-CD
      *    入院登録
           WHEN    "01"
               MOVE    SPA-NAI-I01-NYUINYMD
                                   TO  WRK-CHK-YMD
      *    退院登録
           WHEN    "02"
           WHEN    "10"
           WHEN    "11"
               MOVE    SPA-NAI-I01-TAIINYMD
                                   TO  WRK-CHK-YMD
      *    転科　転棟　転室
           WHEN    "08"
      *
               MOVE    SPA-NAI-I01-IDOYMD
                                   TO  WRK-CHK-YMD
           END-EVALUATE
      *
           INITIALIZE  SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    SPA-GMN-I01-BTUNAME-CD  TO  SYS-5001-KBNCD
           MOVE    WRK-CHK-YMD         TO  ORC-DBYMD
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-5001-REC
           ELSE
              INITIALIZE                   SYS-5001-REC
           END-IF
      *
           MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           INITIALIZE  SYS-5000-REC
           MOVE    "5000"              TO  SYS-5000-KANRICD
           MOVE    "*"                 TO  SYS-5000-KBNCD
           MOVE    WRK-CHK-YMD         TO  ORC-DBYMD
           MOVE    SYS-5000-REC        TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-5000-REC
           ELSE
              INITIALIZE                  SYS-5000-REC
           END-IF
      *
           MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF    ( WRK-CHK-YMD     >=  CONST-H180401 )
      *
               IF    ( SYS-5001-STYUKYMD NOT =  SPACE )
                AND  ( SYS-5001-STYUKYMD   >=  CONST-H180401 )
                   CONTINUE
               ELSE
                   MOVE    "1039"          TO  SPA-ERRCD
                   GO  TO  48011-H18-KAISEI-CHK-EXT
               END-IF
      *
               IF    ( SYS-5000-STYUKYMD NOT =  SPACE )
                 AND ( SYS-5000-STYUKYMD    >=  CONST-H180401 )
                   CONTINUE
               ELSE
                   MOVE    "1040"          TO  SPA-ERRCD
                   GO  TO  48011-H18-KAISEI-CHK-EXT
               END-IF
           END-IF
      *
           IF    ( WRK-CHK-YMD     >=  CONST-H180701 )
      *
               IF    ( SYS-5001-BTU-SBT     =   "04"   OR  "09" )
                   IF    ( SYS-5001-STYUKYMD NOT =  SPACE )
                    AND  ( SYS-5001-STYUKYMD   >=  CONST-H180701 )
                       CONTINUE
                   ELSE
                       MOVE    "1041"          TO  SPA-ERRCD
                       GO  TO  48011-H18-KAISEI-CHK-EXT
                   END-IF
               END-IF
      *
               IF    ( SPA-NAI-I01-MAEIDOYMD
                                           NOT =   SPACE )
                 AND ( SPA-NAI-I01-MAEIDOYMD
                                               <   CONST-H180701 )
                   IF    ( ( SPA-GMN-I01-SYORIKBN-CD
                                               =   "02" )
                       AND ( WRK-CHK-YMD  >=  CONST-H180701 ))
                     OR  ( ( SPA-GMN-I01-SYORIKBN-CD
                                               =   "08" )
                       AND ( WRK-CHK-YMD   >  CONST-H180701 ))
      *
                       INITIALIZE  SYS-5001-REC
                       MOVE    "5001"          TO  SYS-5001-KANRICD
                       MOVE    SPA-NAI-I01-BTUNAME-MOT (1:2)
                                               TO  SYS-5001-KBNCD
                       MOVE    SPA-NAI-I01-MAEIDOYMD
                                               TO  ORC-DBYMD
                       MOVE    SYS-5001-REC    TO  MCPDATA-REC
                       MOVE    "SYSKANRI-KEY"  TO  WRK-DBPATH
                       PERFORM 900-TBL-SELECT-SEC
                       IF    ( FLG-DBRC    =   ZERO )
                           MOVE    MCPDATA-REC TO  SYS-5001-REC
                       ELSE
                           INITIALIZE              SYS-5001-REC
                       END-IF
      *
                       MOVE    "SYSKANRI-KEY"  TO  WRK-DBPATH
                       PERFORM 900-TBL-CLOSE-SEC
      *
                       IF    ( SYS-5001-BTU-SBT
                                               =   "04"   OR  "09" )
                           MOVE    "1043"      TO  SPA-ERRCD
                       END-IF
      *
                   END-IF
               END-IF
      *
           END-IF
      *
           .
       48011-H18-KAISEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処理別関連チェック処理
      *****************************************************************
       4801-SYORI-KANREN-CHK-SEC       SECTION.
      *
      *    処理区分固有の関連チェック
           EVALUATE    SPA-GMN-I01-SYORIKBN-CD
      *    入院登録
           WHEN    "01"
               PERFORM 4801-NYUIN-TOUROKU-CHK-SEC
      *    退院登録
           WHEN    "02"
           WHEN    "11"
               PERFORM 4801-TAIIN-TOUROKU-CHK-SEC
      *    変更
           WHEN    "03"
               PERFORM 4801-HENKOU-CHK-SEC
      *    照会
           WHEN    "04"
               PERFORM 4801-SYOKAI-CHK-SEC
      *    入院取消
           WHEN    "05"
           WHEN    "06"
               PERFORM 4801-NYUIN-TORIKESI-CHK-SEC
      *    退院取消
           WHEN    "07"
               PERFORM 4801-TAIIN-TORIKESI-CHK-SEC
      *    転科　転棟　転室
           WHEN    "08"
               PERFORM 4801-TEKNA-CHK-SEC
      *    異動取消
           WHEN    "09"
               PERFORM 4801-IDO-TORIKESI-CHK-SEC
      *    退院再計算
           WHEN    "10"
               PERFORM 4801-TAIIN-SAIKEISAN-CHK-SEC
           END-EVALUATE
      *
           .
       4801-SYORI-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院登録時関連チェック
      *****************************************************************
       4801-NYUIN-TOUROKU-CHK-SEC      SECTION.
      *
      *    前回退院日≦入院日となっていること
      *    前歴が自（他）院歴の場合は前回退院日＜入院日となっていること
           IF    ( SPA-NAI-I01-TTAIINYMD (1) NOT =   SPACE )
      *
               EVALUATE    SPA-NAI-I01-TZITA-FLG (1)
               WHEN    ZERO
                   IF    ( SPA-NAI-I01-TTAIINYMD (1)
                                  <=    SPA-NAI-I01-NYUINYMD )
                       CONTINUE
                   ELSE
                       MOVE    "1018"      TO  SPA-ERRCD
                       MOVE    7           TO  SPA-GMN-I01-CUR
                       GO  TO  4801-NYUIN-TOUROKU-CHK-EXT
                   END-IF
      *
      *            同日再入院の場合継続入院となっていること
                   IF    ( SPA-NAI-I01-TTAIINYMD (1)
                                   =    SPA-NAI-I01-NYUINYMD )
                       IF    ( SPA-GMN-I01-SHOKAI-CD   =   "2" )
                         AND ( SPA-GMN-I01-SHONUM
                                   =   SPA-GMN-I01-TSHONUM (1)  )
                           CONTINUE
                       ELSE
                           MOVE    "1038"  TO  SPA-ERRCD
                           MOVE    10      TO  SPA-GMN-I01-CUR
                           GO  TO  4801-NYUIN-TOUROKU-CHK-EXT
                       END-IF
                   END-IF
               WHEN    1
                   IF    ( SPA-NAI-I01-TTAIINYMD (1)
                                  <=    SPA-NAI-I01-NYUINYMD )
                       IF    ( SPA-NAI-I01-TTAIINYMD (1)
                                   =    SPA-NAI-I01-NYUINYMD )
                           MOVE    SPA-NAI-I01-TNYUINYMD (1)
                                           TO  WRK-STYMD
                           MOVE    SPA-NAI-I01-TTAIINYMD (1)
                                           TO  WRK-EDYMD
                           PERFORM 5002-KIKAN-CALC-SEC
                           IF    ( SPA-NAI-I01-TNYURRKTAISYO (1)
                                           =   WRK-NISSU2 )
                               MOVE    1   TO  FLG-TAINDOUJITU
                           ELSE
                               MOVE    2   TO  FLG-TAINDOUJITU
                           END-IF
                       END-IF
                   ELSE
                       MOVE    "1018"      TO  SPA-ERRCD
                       MOVE    7           TO  SPA-GMN-I01-CUR
                       GO  TO  4801-NYUIN-TOUROKU-CHK-EXT
                   END-IF
               WHEN    OTHER
                   IF    ( SPA-NAI-I01-TTAIINYMD (1)
                                  <     SPA-NAI-I01-NYUINYMD )
                       CONTINUE
                   ELSE
                       MOVE    "1036"      TO  SPA-ERRCD
                       MOVE    7           TO  SPA-GMN-I01-CUR
                       GO  TO  4801-NYUIN-TOUROKU-CHK-EXT
                   END-IF
      *
      *            前回自院歴チェック
                   PERFORM 4801-ZENKAI-ZIINRRK-CHK-SEC
                   IF    ( SPA-ERRCD   NOT =   SPACE )
                       GO  TO  4801-NYUIN-TOUROKU-CHK-EXT
                   END-IF
               END-EVALUATE
           END-IF
      *
           .
       4801-NYUIN-TOUROKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    前回自院歴チェック
      *****************************************************************
       4801-ZENKAI-ZIINRRK-CHK-SEC     SECTION.
      *
      *    前回入院歴（自院分）をチェック
           MOVE    SPACE               TO  WRK-NYUINCHU-TAIINYMD
           MOVE    ZERO                TO  FLG-WKPTTEIKIRRK
      *
           INITIALIZE      WKPTNYUINRRK-REC
           MOVE    SPA-HOSPID          TO  WKPTNYUINRRK-HOSPID
           MOVE    SPA-NAI-I01-PTID    TO  WKPTNYUINRRK-PTID
      *
           MOVE    SPA-NAI-I01-NYUINYMD
                                       TO  WRK-YMD
           MOVE    "1"                 TO  WRK-ZOGENPTN
           MOVE    -1                  TO  WRK-ZOGEN
           PERFORM 5002-CALENDAR-SEC
           MOVE    WRK-KEISANYMD       TO  WKPTNYUINRRK-NYUINYMD
                                           WRK-NYUINCHU-TAIINYMD
      *
           MOVE    WKPTNYUINRRK-REC    TO  MCPDATA-REC
           MOVE    "PTNYUINRRK-KEY12"  TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        NOT =   ZERO )
               MOVE    1               TO  FLG-WKPTTEIKIRRK
           ELSE
               MOVE    MCPDATA-REC     TO  WKPTNYUINRRK-REC
           END-IF
      *
           MOVE    "PTNYUINRRK-KEY12"  TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF    ( FLG-WKPTTEIKIRRK    NOT =   ZERO )
               GO  TO  4801-ZENKAI-ZIINRRK-CHK-EXT
           END-IF
      *
      *    前歴の自院歴が「入院中」の場合
           IF    ( WKPTNYUINRRK-JTIKBN         =   "6" )
            AND  ( WKPTNYUINRRK-NYUINCHUKBN    =   "1" )
      *
      *        継続入院であること
               IF    ( SPA-GMN-I01-SHOKAI-CD   NOT =   "2" )
                   MOVE    "1031"      TO  SPA-ERRCD
                   MOVE    9           TO  SPA-GMN-I01-CUR
                   GO TO   4801-ZENKAI-ZIINRRK-CHK-EXT
               END-IF
      *
      *        初回番号が等しいこと
               IF    ( WKPTNYUINRRK-SHONUM NOT =   SPA-GMN-I01-SHONUM )
                   MOVE    "1031"      TO  SPA-ERRCD
                   MOVE    10          TO  SPA-GMN-I01-CUR
                   GO TO   4801-ZENKAI-ZIINRRK-CHK-EXT
               END-IF
      *
      *        自院歴の退院日の翌日が入院日となっていること
               IF    ( WKPTNYUINRRK-TAIINYMD
                                   NOT =   WRK-NYUINCHU-TAIINYMD )
                   MOVE    "1032"      TO  SPA-ERRCD
                   MOVE    7           TO  SPA-GMN-I01-CUR
                   GO TO   4801-ZENKAI-ZIINRRK-CHK-EXT
               END-IF
           END-IF
      *
           .
       4801-ZENKAI-ZIINRRK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院登録時関連チェック
      *****************************************************************
       4801-TAIIN-TOUROKU-CHK-SEC      SECTION.
      *
      *    退院日
           IF    ( SPA-GMN-I01-TAIINYMD    =   SPACE )
               MOVE    "1020"      TO  SPA-ERRCD
               MOVE    11          TO  SPA-GMN-I01-CUR
               GO  TO  4801-TAIIN-TOUROKU-CHK-EXT
           END-IF
      *
      *    前回異動日が表示されていない場合は入院日と退院日を比較
      *    前回異動日が表示されている場合は前回異動日と退院日を比較
           IF    ( SPA-GMN-I01-MAEIDOYMD  =   SPACE   )
               IF    ( SPA-NAI-I01-NYUINYMD
                                      <=   SPA-NAI-I01-TAIINYMD )
                   CONTINUE
               ELSE
                   MOVE    "1012"      TO  SPA-ERRCD
                   MOVE    11          TO  SPA-GMN-I01-CUR
                   GO  TO  4801-TAIIN-TOUROKU-CHK-EXT
               END-IF
           ELSE
               IF    ( SPA-NAI-I01-MAEIDOYMD
                                      <=   SPA-NAI-I01-TAIINYMD )
                   CONTINUE
               ELSE
                   MOVE    "1013"      TO  SPA-ERRCD
                   MOVE    11          TO  SPA-GMN-I01-CUR
                   GO  TO  4801-TAIIN-TOUROKU-CHK-EXT
               END-IF
           END-IF
      *
      *    同日再入院の場合、入院日≠退院日となっていること
           IF    ( SPA-NAI-I01-DJKBN   =   "1" OR "2" )
               IF    ( SPA-NAI-I01-NYUINYMD =   SPA-NAI-I01-TAIINYMD )
                   MOVE    "1037"      TO  SPA-ERRCD
                   MOVE    11          TO  SPA-GMN-I01-CUR
                   GO  TO  4801-TAIIN-TOUROKU-CHK-EXT
               END-IF
           END-IF
      *
      *    定期請求との関連チェック
      *    請求開始日取得処理
           PERFORM 4801-SKYSTYMD-GET-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4801-TAIIN-TOUROKU-CHK-EXT
           END-IF
      *
      *    前回定期請求日の翌日（あるいは入院日）
      *    〜算定開始日の翌月までの
      *    日付が入力されていること
           MOVE    WRK-NOW-SKYSTYMD
                                   TO  WRK-YMD
           MOVE    "2"             TO  WRK-ZOGENPTN
           MOVE    1               TO  WRK-ZOGEN
           PERFORM 5002-CALENDAR-SEC
      *
           IF    ( SPA-NAI-I01-TAIINYMD (1 : 6)
                                   <= WRK-KEISANYMD (1 : 6) )
               CONTINUE
           ELSE
               MOVE    "1025"      TO  SPA-ERRCD
               MOVE    11          TO  SPA-GMN-I01-CUR
               GO  TO  4801-TAIIN-TOUROKU-CHK-EXT
           END-IF
      *
      *    月途中の定期請求終了後...(1)、
      *    定期請求区分を「2 月末時のみ請求」に変更された場合、
      *    (1)で作成された収納が未入金であること
           PERFORM 4801-MAE-TEIKI-SKYEDYMD-GET-SEC
           IF    ( SPA-GMN-I01-SEIKYU-CD   =   "2" )
             AND ( WRK-MAE-SKYEDYMD    NOT =   SPACE )
               MOVE    WRK-MAE-SKYEDYMD
                                       TO  WRK-YMD
               MOVE    "1"             TO  WRK-ZOGENPTN
               MOVE    1               TO  WRK-ZOGEN
               PERFORM 5002-CALENDAR-SEC
               IF    ( WRK-KEISANYMD (7 : 2)   NOT =   "01" )
                   MOVE    ZERO                TO  FLG-SYUNOU
                   INITIALIZE  SYUNOU-REC
                   MOVE    SPA-HOSPID          TO  SYU-HOSPID
                   MOVE    SPA-NAI-I01-PTID    TO  SYU-PTID
                   MOVE    "1"                 TO  SYU-NYUGAIKBN
                   MOVE    WRK-NOW-SKYSTYMD    TO  SYU-SKYSTYMD
                   MOVE    SPA-NAI-I01-TAIINYMD
                                           TO  SYU-SKYEDYMD
                   MOVE    SYUNOU-REC          TO  MCPDATA-REC
                   MOVE    "SYUNOU-KEY5"       TO  WRK-DBPATH
                   PERFORM 900-TBL-SELECT-SEC
                   IF    ( FLG-DBRC        NOT =   ZERO )
                       MOVE    1               TO  FLG-SYUNOU
                   END-IF
      *
                   PERFORM UNTIL   ( FLG-SYUNOU    NOT =   ZERO )
                            OR     ( SPA-ERRCD     NOT =   SPACE )
                       MOVE    MCPDATA-REC     TO  SYUNOU-REC
                       IF    ( SYU-NYUIN-RRKNUM
                                           =   SPA-NAI-I01-PTRRKNUM )
                        AND  ( SYU-DENPJTIKBN
                                       NOT =   "3" )
      *                 AND  ( SYU-CREATEKBN   =   "2" )
                           MOVE    1           TO  FLG-SYUARI
                           IF    ( SYU-NYUKIN-TOTAL    >   ZERO )
                               MOVE    "1028"  TO  SPA-ERRCD
                           END-IF
                       END-IF
      *
                       MOVE    "SYUNOU-KEY5"   TO  WRK-DBPATH
                       PERFORM 900-TBL-READ-SEC
                       IF    ( FLG-DBRC    NOT =   ZERO )
                           MOVE    1           TO  FLG-SYUNOU
                       END-IF
      *
                   END-PERFORM
      *
                   MOVE    "SYUNOU-KEY5"   TO  WRK-DBPATH
                   PERFORM 900-TBL-CLOSE-SEC
      *
                   IF    ( SPA-ERRCD     NOT =   SPACE )
                       MOVE    11          TO  SPA-GMN-I01-CUR
                       GO  TO  4801-TAIIN-TOUROKU-CHK-EXT
                   END-IF
               END-IF
      *
           END-IF
      *
      *    退院月に会計テーブルが存在していること
           MOVE    SPA-NAI-I01-TAIINYMD (1 : 6)
                                       TO  WRK-NACCT-SRYYM
           PERFORM 4801-NYUINACCT-SEL-SEC
           IF    ( FLG-NYUINACCTARI    =   ZERO )
               MOVE    "1030"          TO  SPA-ERRCD
               GO  TO  4801-TAIIN-TOUROKU-CHK-EXT
           END-IF
      *
           .
       4801-TAIIN-TOUROKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求開始日取得処理
      *****************************************************************
       4801-SKYSTYMD-GET-SEC           SECTION.
      *
      *    前回定期請求終了日取得処理
           PERFORM 4801-MAE-TEIKI-SKYEDYMD-GET-SEC
           IF    ( WRK-MAE-SKYEDYMD    NOT =   SPACE )
               IF    ( WRK-MAE-SKYEDYMD
                                       >=  SPA-NAI-I01-TAIINYMD )
                   MOVE    "1026"      TO  SPA-ERRCD
                   MOVE    11          TO  SPA-GMN-I01-CUR
                   GO  TO  4801-SKYSTYMD-GET-EXT
               ELSE
                   MOVE    WRK-MAE-SKYEDYMD
                                       TO  WRK-YMD
                   MOVE    "1"         TO  WRK-ZOGENPTN
                   MOVE    1           TO  WRK-ZOGEN
                   PERFORM 5002-CALENDAR-SEC
                   MOVE    WRK-KEISANYMD
                                       TO  WRK-NOW-SKYSTYMD
                   IF    ( SPA-GMN-I01-SEIKYU-CD   =   "2" )
                       IF   ( SPA-NAI-I01-NYUINYMD (1 : 6)
                           <  WRK-NOW-SKYSTYMD     (1 : 6) )
                           MOVE    "01"    TO  WRK-NOW-SKYSTYMD (7 : 2)
                       ELSE
                           MOVE    SPA-NAI-I01-NYUINYMD
                                           TO  WRK-NOW-SKYSTYMD
      *
                           IF    ( SPA-NAI-I01-DJKBN   =   "1" )
                               MOVE    SPA-NAI-I01-NYUINYMD
                                               TO  WRK-YMD
                               MOVE    "1"     TO  WRK-ZOGENPTN
                               MOVE    1       TO  WRK-ZOGEN
                               PERFORM 5002-CALENDAR-SEC
                               MOVE    WRK-KEISANYMD
                                       TO  WRK-NOW-SKYSTYMD
                           END-IF
      *
                       END-IF
                   END-IF
               END-IF
           ELSE
               MOVE    SPA-NAI-I01-NYUINYMD
                                       TO  WRK-NOW-SKYSTYMD
               IF    ( SPA-NAI-I01-DJKBN   =   "1" )
                   MOVE    SPA-NAI-I01-NYUINYMD
                                       TO  WRK-YMD
                   MOVE    "1"         TO  WRK-ZOGENPTN
                   MOVE    1           TO  WRK-ZOGEN
                   PERFORM 5002-CALENDAR-SEC
                   MOVE    WRK-KEISANYMD
                                       TO  WRK-NOW-SKYSTYMD
               END-IF
           END-IF
      *
           .
       4801-SKYSTYMD-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    変更時関連チェック
      *****************************************************************
       4801-HENKOU-CHK-SEC             SECTION.
      *
      *
           .
       4801-HENKOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    照会時関連チェック
      *****************************************************************
       4801-SYOKAI-CHK-SEC             SECTION.
      *
      *
           .
       4801-SYOKAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院取消時関連チェック
      *****************************************************************
       4801-NYUIN-TORIKESI-CHK-SEC     SECTION.
      *
      *
           .
       4801-NYUIN-TORIKESI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院取消時関連チェック
      *****************************************************************
       4801-TAIIN-TORIKESI-CHK-SEC     SECTION.
      *
        
      *
           .
       4801-TAIIN-TORIKESI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    転科　転棟　転室時関連チェック
      *****************************************************************
       4801-TEKNA-CHK-SEC              SECTION.
      *
      *    異動日が入力されていること
           IF    ( SPA-GMN-I01-IDOYMD  =   SPACE )
               MOVE    "1014"      TO  SPA-ERRCD
               MOVE    3           TO  SPA-GMN-I01-CUR
               GO  TO  4801-TEKNA-CHK-EXT
           END-IF
      *
      *    前回異動日が表示されていない場合は入院日と異動日を比較
      *    前回異動日が表示されている場合は前回異動日と異動日を比較
           IF    ( SPA-GMN-I01-MAEIDOYMD  =   SPACE   )
               IF    ( SPA-NAI-I01-NYUINYMD
                                      <=   SPA-NAI-I01-IDOYMD )
                   CONTINUE
               ELSE
                   MOVE    "1015"      TO  SPA-ERRCD
                   MOVE    3           TO  SPA-GMN-I01-CUR
                   GO  TO  4801-TEKNA-CHK-EXT
               END-IF
           ELSE
               IF    ( SPA-NAI-I01-MAEIDOYMD
                                      <=   SPA-NAI-I01-IDOYMD )
                   CONTINUE
               ELSE
                   MOVE    "1016"      TO  SPA-ERRCD
                   MOVE    3           TO  SPA-GMN-I01-CUR
                   GO  TO  4801-TEKNA-CHK-EXT
               END-IF
           END-IF
      *
      *    *** 2004.03.11 削除 ***
      *    入院科または、病室番号または、病棟名または、特定入院料のいずれ
      *    かが変更されていること 
      *-   IF    ( SPA-GMN-I01-NYUINKA-CD  =   SPA-NAI-I01-NYUINKA-MOT )
      *-    AND  ( SPA-GMN-I01-BTUNAME     =   SPA-NAI-I01-BTUNAME-MOT )
      *-    AND  ( SPA-GMN-I01-BRMNUM      =   SPA-NAI-I01-BRMNUM-MOT  )
      *-    AND  ( SPA-GMN-I01-HKNCOMBI-CD =   SPA-NAI-I01-HKNCOMBI-MOT)
      *-    AND  ( SPA-NAI-I01-TOKNYUIN-ORG
      *-                                   =   SPA-NAI-I01-TOKNYUINMOT )
      *-           MOVE    "1017"      TO  SPA-ERRCD
      *-           MOVE    4           TO  SPA-GMN-I01-CUR
      *-           GO  TO  4801-TEKNA-CHK-EXT
      *-   END-IF
      *
      *    該当月の会計テーブルが作成済であること
           IF    ( SPA-NAI-I01-IDOYMD   =  CONST-H180701 )
            AND  ( SPA-NAI-I01-NYUINYMD <  CONST-H180701 )
            AND  ( SPA-NAI-I01-BTUSBT   =  "04"  OR "09" )
               MOVE    SPA-NAI-I01-IDOYMD
                                       TO  WRK-YMD
               MOVE    "2"             TO  WRK-ZOGENPTN
               MOVE    -1              TO  WRK-ZOGEN
               PERFORM 5002-CALENDAR-SEC
               MOVE    WRK-KEISANYMD   TO  WRK-NACCT-SRYYM
               PERFORM 4801-NYUINACCT-SEL-SEC
               IF    ( FLG-NYUINACCTARI    =   ZERO )
                   MOVE    "1042"      TO  SPA-ERRCD
                   GO  TO  4801-TEKNA-CHK-EXT
               END-IF
           ELSE
               MOVE    SPA-NAI-I01-IDOYMD (1 : 6)
                                       TO  WRK-NACCT-SRYYM
               PERFORM 4801-NYUINACCT-SEL-SEC
               IF    ( FLG-NYUINACCTARI    =   ZERO )
                   MOVE    "1029"          TO  SPA-ERRCD
                   GO  TO  4801-TEKNA-CHK-EXT
               END-IF
           END-IF
      *
           .
       4801-TEKNA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理
      *****************************************************************
       4801-NYUINACCT-SEL-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCTARI
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPID          TO  NACCT-HOSPID
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    SPA-NAI-I01-PTID    TO  NACCT-PTID
           MOVE    WRK-NACCT-SRYYM
                                       TO  NACCT-SRYYM
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "NYUINACCT-KEY7"    TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        NOT =   ZERO )
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           PERFORM UNTIL ( FLG-NYUINACCT       NOT =   ZERO )
                    OR   ( FLG-NYUINACCTARI    NOT =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
      *
               IF    ( NACCT-ZAISKBKBN     =    "1"    )
                   MOVE    1           TO  FLG-NYUINACCTARI
               END-IF
      *
               MOVE    "NYUINACCT-KEY7"    TO  WRK-DBPATH
               PERFORM 900-TBL-READ-SEC
               IF    ( FLG-DBRC    NOT =   ZERO )
                   MOVE     1          TO  FLG-NYUINACCT
               END-IF
      *
           END-PERFORM
      *
           MOVE    "NYUINACCT-KEY7"    TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
       4801-NYUINACCT-SEL-EXT.
           EXIT.
      *****************************************************************
      *    異動取消時関連チェック
      *****************************************************************
       4801-IDO-TORIKESI-CHK-SEC       SECTION.
      *
        
      *
           .
       4801-IDO-TORIKESI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院再計算関連チェック
      *****************************************************************
       4801-TAIIN-SAIKEISAN-CHK-SEC       SECTION.
      *
      *    歴が選択されていること
           IF    ( SPA-GMN-I01-NYUREKILST-SEL >   ZERO )
            AND  ( SPA-GMN-I01-NYUREKILST-SEL
                              <=   SPA-GMN-I01-NYUREKILST-CNT )
               CONTINUE
           ELSE
               MOVE    "1034"          TO  SPA-ERRCD
               GO  TO  4801-TAIIN-SAIKEISAN-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I01-NYUREKILST-SEL
                                       TO  IDX1
      *
      *    歴作成データが選択されていないこと
           IF    ( SPA-NAI-I01-TZITA-FLG (IDX1)    NOT =   ZERO )
               MOVE    "1035"          TO  SPA-ERRCD
               GO  TO  4801-TAIIN-SAIKEISAN-CHK-EXT
           END-IF
      *
      *    入院中の歴が選択されていないこと
           IF    ( SPA-NAI-I01-TTAIINYMD (IDX1)        =   SPACE )
               MOVE    "1034"          TO  SPA-ERRCD
               GO  TO  4801-TAIIN-SAIKEISAN-CHK-EXT
           END-IF
      *
           .
       4801-TAIIN-SAIKEISAN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴データ編集
      *****************************************************************
       4801-PTNYUINRRK-EDIT-SEC    SECTION.
      *
      *    入院履歴データ編集前処理
           PERFORM 4801-PTNYUINRRK-EDIT-MAE-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4801-PTNYUINRRK-EDIT-EXT
           END-IF
      *
           INITIALIZE              PTNYUINRRK-REC
      *
      *    医療機関ＩＤ
           MOVE    SPA-HOSPID      TO  PTNYUINRRK-HOSPID
      *
      *    患者ＩＤ
           MOVE    SPA-NAI-I01-PTID
                                   TO  PTNYUINRRK-PTID
      *
      *    入院履歴番号（入院１回で＋１ずつ採番）
           MOVE    WRK-RRKNUM      TO  PTNYUINRRK-RRKNUM
      *
      *    入院履歴枝番号（転科・転室・転棟時に採番）
           MOVE    WRK-RRKEDANUM   TO  PTNYUINRRK-RRKEDANUM
      *
           IF    ( WRK-UPDKBN      =   "DEL" )
               GO  TO  4801-PTNYUINRRK-EDIT-EXT
           END-IF
      *
      *    病棟番号
           MOVE    SPA-GMN-I01-BTUNAME-CD
                                   TO  PTNYUINRRK-BTUNUM
           MOVE    SPA-GMN-I01-BTUNAME-CD
                                   TO  PTNYUINRRK-BRMNUM (1 : 2)
      *
      *    病棟名称
      *    MOVE    SPA-GMN-I01-BTUNAME-NM
      *                            TO  PTNYUINRRK-BTUNAME
      *
      *    病室番号
           MOVE    SPA-GMN-I01-BRMNUM
                                   TO  WRK-BRMNUM
      *    病室番号が数字の場合、前ゼロ編集を行う
           PERFORM 4800-BRMNUM-EDIT-SEC
           MOVE    WRK-BRMNUM-EDT  TO  PTNYUINRRK-BRMNUM (3 : 6)
      *    入院科
           MOVE    SPA-GMN-I01-NYUINKA-CD
                                   TO  PTNYUINRRK-NYUINKA
      *    保険組合せ番号
           MOVE    SPA-GMN-I01-HKNCOMBI-CD
                                   TO  PTNYUINRRK-HKNCOMBINUM
      *    入院日
           MOVE    SPA-NAI-I01-NYUINYMD
                                   TO  PTNYUINRRK-NYUINYMD
      *    退院日、退院事由
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   "02"  OR  "11")
               MOVE    SPA-NAI-I01-TAIINYMD
                                   TO  PTNYUINRRK-TAIINYMD
               MOVE    SPA-GMN-I01-TAIINCD
                                   TO  PTNYUINRRK-TAIINCD
           ELSE
               MOVE    "99999999"  TO  PTNYUINRRK-TAIINYMD
               MOVE    SPACE       TO  PTNYUINRRK-TAIINCD
           END-IF
      *
      *    特定入院料区分
           MOVE    SPA-NAI-I01-TOKNYUIN-ORG-KBN
                                   TO  PTNYUINRRK-TOKUTEI-KBN
      *    特定入院料
           MOVE    SPA-NAI-I01-TOKNYUIN-ORG-CD
                                   TO  PTNYUINRRK-TOKUTEI-NYUIN
      *    室料差額
           MOVE    SPA-GMN-I01-SAGAKU-CD
                                   TO  PTNYUINRRK-RM-SAGAKU
      *    定期請求区分
           MOVE    SPA-GMN-I01-SEIKYU-CD
                                   TO  PTNYUINRRK-TEIKI-SEIKYUKBN
      *    検索表示区分
           MOVE    SPA-GMN-I01-DISPKBN-CD
                                   TO  PTNYUINRRK-KENSAKU-DISPKBN
      *    初回継続区分
           MOVE    SPA-GMN-I01-SHOKAI-CD
                                   TO  PTNYUINRRK-SHOKAIKBN
      *    初回番号
           MOVE    WRK-SHONUM
                                   TO  PTNYUINRRK-SHONUM
      *
      *    担当医１
           IF    ( SPA-GMN-I01-DR-CD (1)   NOT =   SPACE )
               MOVE    "1"         TO  PTNYUINRRK-DRCD1 ( 1 : 1 ) 
               MOVE    SPA-GMN-I01-DR-CD (1)
                                   TO  PTNYUINRRK-DRCD1 ( 2 : )
           END-IF
      *    担当医２
           IF    ( SPA-GMN-I01-DR-CD (2)   NOT =   SPACE )
               MOVE    "1"         TO  PTNYUINRRK-DRCD2 ( 1 : 1 ) 
               MOVE    SPA-GMN-I01-DR-CD (2)
                                   TO  PTNYUINRRK-DRCD2 ( 2 : )
           END-IF
      *    担当医３
           IF    ( SPA-GMN-I01-DR-CD (3)   NOT =   SPACE )
               MOVE    "1"         TO  PTNYUINRRK-DRCD3 ( 1 : 1 ) 
               MOVE    SPA-GMN-I01-DR-CD (3)
                                   TO  PTNYUINRRK-DRCD3 ( 2 : )
           END-IF
      *    状態区分
      *    （１：入院　２：転科　３：転棟　４：転室　５：他院歴
      *      ６：自院歴 ）
      *    転入日
      *    転出日
           EVALUATE    SPA-GMN-I01-SYORIKBN-CD
      *    入院登録
           WHEN    "01"
               MOVE    "1"         TO  PTNYUINRRK-JTIKBN
               MOVE    SPA-NAI-I01-NYUINYMD
                                   TO  PTNYUINRRK-TENNYUYMD
               MOVE    "99999999"  TO  PTNYUINRRK-TENSTUYMD
      *    退院登録
      *    WHEN    "02"
      *        MOVE    WRK-JTIKBN  TO  PTNYUINRRK-JTIKBN
      *        MOVE    WRK-TENNYUYMD
      *                            TO  PTNYUINRRK-TENNYUYMD
      *        MOVE    SPA-NAI-I01-TAIINYMD
      *                            TO  PTNYUINRRK-TENSTUYMD
      *    退院取消
           WHEN    "07"
               MOVE    WRK-JTIKBN  TO  PTNYUINRRK-JTIKBN
               MOVE    WRK-TENNYUYMD
                                   TO  PTNYUINRRK-TENNYUYMD
               MOVE    "99999999"  TO  PTNYUINRRK-TENSTUYMD
      *    転科　転棟　転室
           WHEN    "08"
      *
               EVALUATE    TRUE
      *        科が変わった場合
               WHEN    WRK-NYUINKA-KEY NOT =   PTNYUINRRK-NYUINKA
                   MOVE    "2"   TO  PTNYUINRRK-JTIKBN
      *        病棟が変わった場合
               WHEN    WRK-BTUNUM-KEY  NOT =   PTNYUINRRK-BTUNUM
                   MOVE    "3"   TO  PTNYUINRRK-JTIKBN
      *        病室が変わった場合
               WHEN    WRK-BRMNUM-KEY  NOT =   PTNYUINRRK-BRMNUM
                   MOVE    "4"   TO  PTNYUINRRK-JTIKBN
      *        入院料が変わったとき
               WHEN  ( WRK-HKNCOMBI-KEY
                                       NOT  =  PTNYUINRRK-HKNCOMBINUM )
               WHEN  ( WRK-TOKUTEI-KBN-KEY
                                   NOT =   PTNYUINRRK-TOKUTEI-KBN )
                 OR  ( WRK-TOKUTEI-NYUIN-KEY
                                   NOT =   PTNYUINRRK-TOKUTEI-NYUIN )
                   MOVE    "7"   TO  PTNYUINRRK-JTIKBN
               WHEN    OTHER
                   MOVE    "7"   TO  PTNYUINRRK-JTIKBN
               END-EVALUATE
      *
               MOVE    SPA-NAI-I01-IDOYMD
                                   TO  PTNYUINRRK-TENNYUYMD
               MOVE    "99999999"  TO  PTNYUINRRK-TENSTUYMD
      *    その他
           WHEN    OTHER
               MOVE    WRK-JTIKBN  TO  PTNYUINRRK-JTIKBN
               MOVE    WRK-TENNYUYMD
                                   TO  PTNYUINRRK-TENNYUYMD
               MOVE    WRK-TENSTUYMD
                                   TO  PTNYUINRRK-TENSTUYMD
           END-EVALUATE
      *
      *    入院中区分 ("0"固定)
           MOVE    "0"             TO  PTNYUINRRK-NYUINCHUKBN
      *
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   "01" )
               IF    ( SPA-NAI-I01-TTAIINYMD (1)   NOT =   SPACE )
                   IF    ( SPA-NAI-I01-TTAIINYMD (1)
                                   =   SPA-NAI-I01-NYUINYMD )
                       EVALUATE    SPA-NAI-I01-TZITA-FLG (1)
                       WHEN    ZERO
                           MOVE    "1" TO  PTNYUINRRK-DOUJITSUKBN
                       WHEN    1
                           MOVE    "2" TO  PTNYUINRRK-DOUJITSUKBN
                       END-EVALUATE
                   END-IF
               END-IF
           ELSE
                       MOVE    SPA-NAI-I01-DJKBN
                                   TO  PTNYUINRRK-DOUJITSUKBN
           END-IF
      *
           IF    ( WRK-UPDKBN      =   "UPD" )
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    WRK-CREYMD      TO  PTNYUINRRK-CREYMD
               MOVE    SMCNDATE-YMD    TO  PTNYUINRRK-UPYMD
               MOVE    SMCNDATE-HMS    TO  PTNYUINRRK-UPHMS
           ELSE
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD    TO  PTNYUINRRK-CREYMD
                                           PTNYUINRRK-UPYMD
               MOVE    SMCNDATE-HMS    TO  PTNYUINRRK-UPHMS
           END-IF
      *
           .
      *
       4801-PTNYUINRRK-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴データ編集
      *****************************************************************
       4801-PTNYUINRRK-EDIT-MAE-SEC    SECTION.
      *
           INITIALIZE                  WRK-SYUKA-G
      *
           MOVE    SPACE           TO  WRK-CREYMD
                                       WRK-TENNYUYMD
                                       WRK-TENSTUYMD
                                       WRK-NYUINKA-KEY
                                       WRK-BTUNUM-KEY
                                       WRK-BRMNUM-KEY
                                       WRK-TOKUTEI-KEY
           MOVE    ZERO            TO  WRK-SHONUM
      *
           MOVE    SPA-NAI-I01-PTRRKNUM
                                   TO  WRK-RRKNUM
           MOVE    SPA-NAI-I01-PTRRKEDANUM
                                   TO  WRK-RRKEDANUM
      *
      *    入院登録時、他院退院日との同一入院時で他院歴の
      *    通算日数を調整する場合
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   "01" )
            AND  ( FLG-TAINTUSANMINUS      =   1 )
               PERFORM 4801-TAINTUSANMINUS-UPD-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  4801-PTNYUINRRK-EDIT-MAE-EXT
               END-IF
           END-IF
      *
      *    入院登録で初回の場合、設定する初回番号を求める
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   "01" )
            AND  ( SPA-GMN-I01-SHOKAI-CD   =   "1"  )
               PERFORM 4801-SYOKAI-GET-SEC
           ELSE
               COMPUTE WRK-SHONUM      =   SPA-GMN-I01-SHONUM
           END-IF
      *
      *    退院取消処理時、履歴を遡って退院日を更新する
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   "07" )
      *
               PERFORM 4801-TAIINYMD-UPD-SEC
      *
      *        前回定期請求終了日取得処理
               PERFORM 4801-MAE-TEIKI-SKYEDYMD-GET-SEC
               IF    ( WRK-MAE-SKYEDYMD    =   SPACE )
                   MOVE    SPA-NAI-I01-NYUINYMD
                                       TO  WRK-SKYSTYMD
               ELSE
                   MOVE    WRK-MAE-SKYEDYMD
                                       TO  WRK-YMD
                   MOVE    "1"         TO  WRK-ZOGENPTN
                   MOVE    1           TO  WRK-ZOGEN
                   PERFORM 5002-CALENDAR-SEC
                   MOVE    WRK-KEISANYMD
                                       TO  WRK-SKYSTYMD
               END-IF
               MOVE    SPA-NAI-I01-TAIINYMD
                                       TO  WRK-SKYEDYMD
      *        収納データを更新する
               PERFORM 4801-SYUNOU-UPD-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  4801-PTNYUINRRK-EDIT-MAE-EXT
               END-IF
      *
           END-IF
      *
      *    入院取消時、転科歴をワークに退避する
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   "05"  OR  "06" )
               PERFORM 4801-TENKAREKI-TAIHI-SEC
      *
      *        -- 定期請求で作成された収納データを取消
      *        前回定期請求終了日取得処理
               PERFORM 4801-MAE-TEIKI-SKYEDYMD-GET-SEC
               IF    ( WRK-MAE-SKYEDYMD    =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    SPA-NAI-I01-NYUINYMD
                                       TO  WRK-SKYSTYMD
                   MOVE    WRK-MAE-SKYEDYMD
                                       TO  WRK-SKYEDYMD
      *
      *            患者定期請求履歴を更新する
                   PERFORM 4801-PTTEIKIRRK-UPD-SEC
                   IF    ( SPA-ERRCD       NOT =   SPACE )
                       GO  TO  4801-PTNYUINRRK-EDIT-MAE-EXT
                   END-IF
      *
      *            収納データを更新する
                   PERFORM 4801-SYUNOU-UPD-SEC
                   IF    ( SPA-ERRCD       NOT =   SPACE )
                       GO  TO  4801-PTNYUINRRK-EDIT-MAE-EXT
                   END-IF
      *
               END-IF
           END-IF
      *
      *    転科・転棟・転室の場合、転出元の歴の転出日に異動日を設定
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   "08" )
               PERFORM 4801-TENSYUTU-MOTO-UPD-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  4801-PTNYUINRRK-EDIT-MAE-EXT
               END-IF
           END-IF
      *
      *    異動取消の場合、異動前の歴の転出日にALL9を設定
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   "09" )
               PERFORM 4801-IDO-MAE-UPD-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  4801-PTNYUINRRK-EDIT-MAE-EXT
               END-IF
           END-IF
      *
           PERFORM 4801-RRKNUM-EDIT-SEC
      *
      *    入院履歴マスタ検索
           PERFORM 4801-PTNYUINRRK-SEL-SEC
           IF    ( FLG-PTNYUINRRK  =   ZERO )
      *        追加処理時、既にデータが存在する場合はエラーとする
               IF    ( WRK-UPDKBN  =   "INS" )
      *            入院歴を再編集
                   PERFORM 4101-NYUREKI-EDIT-SEC
                   MOVE    "4002"  TO  SPA-ERRCD
                   GO  TO  4801-PTNYUINRRK-EDIT-MAE-EXT
               ELSE
      *
      *            更新データ用に状態区分等を退避
                   MOVE    PTNYUINRRK-JTIKBN
                                   TO  WRK-JTIKBN
                   MOVE    PTNYUINRRK-TENNYUYMD
                                   TO  WRK-TENNYUYMD
                   MOVE    PTNYUINRRK-TENSTUYMD
                                   TO  WRK-TENSTUYMD
                   MOVE    PTNYUINRRK-CREYMD
                                   TO  WRK-CREYMD
      *
                   PERFORM 48011-EDIT-SYUKA-PARA-SEC
      *
               END-IF
           ELSE
      *        更新・削除処理時、対象データが存在しなければエラーとする
               IF    ( WRK-UPDKBN  =   "UPD" OR "DEL")
      *            入院歴を再編集
                   PERFORM 4101-NYUREKI-EDIT-SEC
                   MOVE    "4003"  TO  SPA-ERRCD
                   GO  TO  4801-PTNYUINRRK-EDIT-MAE-EXT
               END-IF
      *
               PERFORM 48011-EDIT-SYUKA-PARA-SEC
      *
           END-IF
      *
      *
           .
       4801-PTNYUINRRK-EDIT-MAE-EXT.
           EXIT.
      *****************************************************************
      *    主科決定サブパラメタ編集処理
      *****************************************************************
       48011-EDIT-SYUKA-PARA-SEC       SECTION.
      *
           EVALUATE    SPA-GMN-I01-SYORIKBN-CD
           WHEN    "01"
               MOVE    "04"                    TO  WRK-SYUKA-KBN
               MOVE    SPA-NAI-I01-NYUINYMD    TO  WRK-SYUKA-YMD
           WHEN    "02"
               MOVE    "05"                    TO  WRK-SYUKA-KBN
               MOVE    SPA-NAI-I01-TAIINYMD    TO  WRK-SYUKA-YMD
           WHEN    "03"
               MOVE    "04"                    TO  WRK-SYUKA-KBN
               MOVE    PTNYUINRRK-TENNYUYMD    TO  WRK-SYUKA-YMD
           WHEN    "05"
               MOVE    "04"                    TO  WRK-SYUKA-KBN
               MOVE    SPA-NAI-I01-NYUINYMD    TO  WRK-SYUKA-YMD
           WHEN    "06"
               MOVE    "05"                    TO  WRK-SYUKA-KBN
               MOVE    SPA-NAI-I01-NYUINYMD    TO  WRK-SYUKA-YMD
           WHEN    "07"
               MOVE    "04"                    TO  WRK-SYUKA-KBN
               MOVE    SPA-NAI-I01-TAIINYMD    TO  WRK-SYUKA-YMD
           WHEN    "08"
               MOVE    "04"                    TO  WRK-SYUKA-KBN
               MOVE    SPA-NAI-I01-IDOYMD      TO  WRK-SYUKA-YMD
           WHEN    "09"
               MOVE    "04"                    TO  WRK-SYUKA-KBN
               MOVE    PTNYUINRRK-TENNYUYMD    TO  WRK-SYUKA-YMD
           WHEN    "10"
               MOVE    "04"                    TO  WRK-SYUKA-KBN
               MOVE    SPA-NAI-I01-TAIINYMD    TO  WRK-SYUKA-YMD
           WHEN    "11"
               MOVE    "04"                    TO  WRK-SYUKA-KBN
               MOVE    SPA-NAI-I01-TAIINYMD    TO  WRK-SYUKA-YMD
           END-EVALUATE
      *
           .
       48011-EDIT-SYUKA-PARA-EXT.
           EXIT.
      *****************************************************************
      *    初回番号取得処理
      *****************************************************************
       4801-SYOKAI-GET-SEC         SECTION.
      *
      *    入院履歴マスタ検索
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPID          TO  PTNYUINRRK-HOSPID
           MOVE    SPA-NAI-I01-PTID    TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "PTNYUINRRK-KEY6"   TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    =   ZERO  )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "PTNYUINRRK-KEY6"   TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF    ( FLG-PTNYUINRRK  NOT =   ZERO )
               COMPUTE WRK-SHONUM  =   1
           ELSE
               COMPUTE WRK-SHONUM  =   PTNYUINRRK-SHONUM   +   1
           END-IF
      *
           .
       4801-SYOKAI-GET-EXT.
           EXIT.
      *****************************************************************
      *    退院日更新処理
      *****************************************************************
       4801-TAIINYMD-UPD-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-TAIINUPDCNT
      *
      *    入院履歴マスタ検索
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPID          TO  PTNYUINRRK-HOSPID
           MOVE    SPA-NAI-I01-PTID    TO  PTNYUINRRK-PTID
           MOVE    SPA-NAI-I01-PTRRKNUM
                                       TO  PTNYUINRRK-RRKNUM
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "PTNYUINRRK-KEY4"   TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
      *
      *    入院歴の退院日を更新する。
           PERFORM UNTIL ( FLG-DBRC    NOT =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
      *
               COMPUTE WRK-TAIINUPDCNT
                   =   WRK-TAIINUPDCNT +   1
      *
      *        最新歴は別途更新処理を行う
               IF    ( WRK-TAIINUPDCNT =   1 )
                   IF    ( SPA-NAI-I01-PTRRKNUM
                                       =   PTNYUINRRK-RRKNUM )
                    AND  ( SPA-NAI-I01-PTRRKEDANUM
                                       =   PTNYUINRRK-RRKEDANUM )
                       CONTINUE
                   ELSE
                       MOVE    "4004"  TO  SPA-ERRCD
                       GO  TO  4801-TAIINYMD-UPD-EXT
                   END-IF
               ELSE
                  MOVE    "99999999"
                                       TO  PTNYUINRRK-TAIINYMD
                  MOVE    SPACE        TO  PTNYUINRRK-TAIINCD
                  PERFORM 800-MCNDATE-SEC
                  MOVE    SMCNDATE-YMD        TO  PTNYUINRRK-UPYMD
                  MOVE    SMCNDATE-HMS        TO  PTNYUINRRK-UPHMS
      *
                   MOVE    "PTNYUINRRK-KEY"
                                       TO  WRK-DBPATH
                   MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
                   PERFORM 900-TBL-UPDATE-SEC
                   IF    ( FLG-DBRC    NOT =   ZERO )
                       MOVE    "4005"  TO  SPA-ERRCD
                       GO  TO  4801-TAIINYMD-UPD-EXT
                   END-IF
               END-IF
      *
               MOVE    "PTNYUINRRK-KEY4"   TO  WRK-DBPATH
               PERFORM 900-TBL-READ-SEC
      *
           END-PERFORM
      *
           MOVE    "PTNYUINRRK-KEY4"   TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF    ( WRK-TAIINUPDCNT     =   ZERO )
               MOVE    "4006"  TO  SPA-ERRCD
               GO  TO  4801-TAIINYMD-UPD-EXT
           END-IF
      *
      *
           .
       4801-TAIINYMD-UPD-EXT.
           EXIT.
      *****************************************************************
      *    収納データ更新処理
      *****************************************************************
       4801-SYUNOU-UPD-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
           INITIALIZE  SYUNOU-REC
      *
           MOVE    SPA-HOSPID          TO  SYU-HOSPID
           MOVE    SPA-NAI-I01-PTID    TO  SYU-PTID
           MOVE    "1"                 TO  SYU-NYUGAIKBN
           MOVE    WRK-SKYSTYMD        TO  SYU-SKYSTYMD
           MOVE    WRK-SKYEDYMD        TO  SYU-SKYEDYMD
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "SYUNOU-KEY5"       TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        NOT =   ZERO )
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
      *
               IF    ( SYU-NYUIN-RRKNUM    =   SPA-NAI-I01-PTRRKNUM )
                AND  ( SYU-DENPJTIKBN  NOT =   "3" )
      *
      *            収納明作成処理
                   PERFORM 4801-SYUMEI-SAKUSEI-SEC
                   IF    ( SPA-ERRCD   NOT =   SPACE )
                       GO  TO  4801-SYUNOU-UPD-EXT
                   END-IF
      *
      *            請求取消として収納データを更新
                   MOVE    ZERO            TO  SYU-SKYMONEY
                   MOVE    "3"             TO  SYU-DENPJTIKBN
      *
                   PERFORM 800-MCNDATE-SEC
                   MOVE    SMCNDATE-YMD    TO  SYU-UPYMD
                   MOVE    SMCNDATE-HMS    TO  SYU-UPHMS
      *
                   MOVE    SYUNOU-REC      TO  MCPDATA-REC
                   MOVE    "SYUNOU-KEY"    TO  WRK-DBPATH
                   PERFORM 900-TBL-UPDATE-SEC
                   IF    ( FLG-DBRC    NOT =   ZERO )
                       MOVE    "4007"      TO  SPA-ERRCD
                       GO  TO  4801-SYUNOU-UPD-EXT
                   END-IF
               END-IF
      *
               MOVE    "SYUNOU-KEY5"   TO  WRK-DBPATH
               PERFORM 900-TBL-READ-SEC
               IF    ( FLG-DBRC    NOT =   ZERO )
                   MOVE    1           TO  FLG-SYUNOU
               END-IF
      *
           END-PERFORM
      *
           MOVE    "SYUNOU-KEY5"   TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
      *
       4801-SYUNOU-UPD-EXT.
           EXIT.
      *****************************************************************
      *    前回定期請求終了日取得処理
      *****************************************************************
       4801-MAE-TEIKI-SKYEDYMD-GET-SEC SECTION.
      *
           MOVE    SPACE               TO  WRK-MAE-SKYEDYMD
      *
           INITIALIZE                      PTTEIKIRRK-REC
           MOVE    SPA-HOSPID          TO  PTTEIKIRRK-HOSPID
           MOVE    SPA-NAI-I01-PTID    TO  PTTEIKIRRK-PTID
           MOVE    SPA-NAI-I01-PTRRKNUM
                                       TO  PTTEIKIRRK-RRKNUM
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "PTTEIKIRRK-KEY7"   TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC            =   ZERO )
               MOVE    MCPDATA-REC     TO  PTTEIKIRRK-REC
               MOVE    PTTEIKIRRK-SKYEDYMD
                                       TO  WRK-MAE-SKYEDYMD
           END-IF
      *
           MOVE    "PTTEIKIRRK-KEY7"   TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
      *
       4801-MAE-TEIKI-SKYEDYMD-GET-EXT.
           EXIT.
      *****************************************************************
      *    収納明細作成処理
      *****************************************************************
       4801-SYUMEI-SAKUSEI-SEC         SECTION.
      *
           COMPUTE WRK-SKYMONEY    =   SYU-SKYMONEY        *   -1
      *
      *    伝票番号枝番を求める
           MOVE    ZERO                TO  FLG-SYUMEI
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPID          TO  SMEI-HOSPID
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "SYUMEI-KEY2"       TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    NOT =   ZERO )
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
      *
           PERFORM VARYING  IDX13  FROM    1   BY   1
               UNTIL ( FLG-SYUMEI  =   1)
      *
               MOVE    "SYUMEI-KEY2"   TO  WRK-DBPATH
               PERFORM 900-TBL-READ-SEC
               IF    ( FLG-DBRC    NOT =   ZERO )
                   MOVE    1               TO  FLG-SYUMEI
               END-IF
      *
           END-PERFORM
      *
           MOVE    "SYUMEI-KEY2"       TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           MOVE    SPACE               TO  SYUMEI-REC
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPID          TO  SMEI-HOSPID
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
      *伝票番号
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
      *伝票番号枝番
           MOVE    IDX13               TO  SMEI-DENPEDANUM
      *入金連番
           MOVE    ZERO                TO  SMEI-NYUKINRENNUM
      *診療科
           MOVE    SYU-SRYKA           TO  SMEI-SRYKA
      *伝票状態区分
           MOVE    SPACE               TO  SMEI-MEISAIJOUTAIKBN
      *請求発行日（発行日の西暦）
           MOVE    SPACE               TO  SMEI-SKYPRINTYMD
      *請求再発行日
           MOVE    SPACE               TO  SMEI-SKYREPRINTYMD
      *領収書発行日
           MOVE    SPACE               TO  SMEI-RYOSYUPRINTYMD
      *領収書再発行日
           MOVE    SPACE               TO  SMEI-RYOSYUREPRINTYMD
      *請求金額
           MOVE    WRK-SKYMONEY        TO  SMEI-SKYMONEY
      *
      *入返金区分
           MOVE    SPACE               TO  SMEI-NYUHEN-KBN
      *入返金額
           MOVE    ZERO                TO  SMEI-NYUHEN-MONEY
      *入返金日
           MOVE    SPA-SYSYMD          TO  SMEI-NYUHEN-YMD
      *入金方法
           MOVE    "00"                TO  SMEI-NYUKIN-HOHO
      *状態区分
           MOVE    "5"                 TO  SMEI-JOUTAIKBN
      *
      *    追加対象のデータが既に存在しないかチェック
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "SYUMEI-KEY"        TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC            =   ZERO )
               MOVE    "4008"          TO  SPA-ERRCD
               GO  TO  4801-SYUMEI-SAKUSEI-EXT
           END-IF
      *
           MOVE    "SYUMEI-KEY"        TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  SMEI-CREYMD
                                           SMEI-UPYMD
           MOVE    SMCNDATE-HMS        TO  SMEI-CREHMS
                                           SMEI-UPHMS
      *
           MOVE    SMEI-CREYMD         TO  SDAILYKEY-CREYMD
           MOVE    SMEI-CREHMS         TO  SDAILYKEY-CREHMS
           MOVE    SMEI-NYUHEN-YMD     TO  SDAILYKEY-BASEYMD
           PERFORM 800-DAILYKEY-SEC
           MOVE    SDAILYKEY-DAILYKEY  TO  SMEI-DAILYKEY
      *
      *    収納明細追加
           MOVE    "SYUMEI-KEY"        TO  WRK-DBPATH
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           PERFORM 900-TBL-INSERT-SEC
           IF    ( FLG-DBRC        NOT =   ZERO )
               MOVE    "4009"          TO  SPA-ERRCD
               GO  TO  4801-SYUMEI-SAKUSEI-EXT
           END-IF
      *
           .
       4801-SYUMEI-SAKUSEI-EXT.
           EXIT.
      *****************************************************************
      *    患者定期請求履歴更新処理
      *****************************************************************
       4801-PTTEIKIRRK-UPD-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-PTTEIKIRRK
      *
      *    患者請求履歴更新
           INITIALIZE  PTTEIKIRRK-REC
           MOVE    SPA-HOSPID          TO  PTTEIKIRRK-HOSPID
           MOVE    SPA-NAI-I01-PTID    TO  PTTEIKIRRK-PTID
           MOVE    WRK-SKYSTYMD        TO  PTTEIKIRRK-SKYSTYMD
           MOVE    WRK-SKYEDYMD        TO  PTTEIKIRRK-SKYEDYMD
           MOVE    SPA-NAI-I01-PTRRKNUM
                                       TO  PTTEIKIRRK-RRKNUM
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  PTTEIKIRRK-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTTEIKIRRK-UPHMS
      *
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "upd2"              TO  MCP-PATHNAME
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           IF    ( MCP-RC         NOT =   ZERO )
               MOVE    "4010"          TO  SPA-ERRCD
               GO  TO  4801-PTTEIKIRRK-UPD-EXT
           END-IF
      *
           .
       4801-PTTEIKIRRK-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    転科歴退避処理
      *****************************************************************
       4801-TENKAREKI-TAIHI-SEC    SECTION.
      *
           INITIALIZE  WRK-TENKA-SRYKA-TBL
      *
      *    入院履歴マスタ検索
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPID          TO  PTNYUINRRK-HOSPID
           MOVE    SPA-NAI-I01-PTID    TO  PTNYUINRRK-PTID
           MOVE    SPA-NAI-I01-PTRRKNUM
                                       TO  PTNYUINRRK-RRKNUM
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "PTNYUINRRK-KEY4"   TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
      *
           PERFORM UNTIL ( FLG-DBRC    NOT =   ZERO )
                   OR    ( WRK-TENKA-SRYKA-CNT
                                          >=  50 )
      *
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
      *
               MOVE    ZERO            TO  FLG-TENKA-SRYKA
               PERFORM VARYING IDX9    FROM    1   BY  1
                       UNTIL ( IDX9    >   50 )
                        OR   ( WRK-TENKA-SRYKA (IDX9) = SPACE )
                        OR   ( FLG-TENKA-SRYKA NOT    = ZERO  )
      *
                   IF    ( WRK-TENKA-SRYKA (IDX9)
                                       =   PTNYUINRRK-NYUINKA )
                       MOVE    1       TO  FLG-TENKA-SRYKA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( FLG-TENKA-SRYKA =   ZERO )
                   COMPUTE WRK-TENKA-SRYKA-CNT
                       =   WRK-TENKA-SRYKA-CNT +   1
                   MOVE    PTNYUINRRK-NYUINKA
                                       TO  WRK-TENKA-SRYKA
                                               (WRK-TENKA-SRYKA-CNT)
               END-IF
      *
               MOVE    "PTNYUINRRK-KEY4"   TO  WRK-DBPATH
               PERFORM 900-TBL-READ-SEC
      *
           END-PERFORM
      *
           MOVE    "PTNYUINRRK-KEY4"   TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
       4801-TENKAREKI-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    他院歴通算日数減算更新処理
      *****************************************************************
       4801-TAINTUSANMINUS-UPD-SEC  SECTION.
      *
           MOVE    SPA-NAI-I01-TRRKNUM (1)
                               TO      WRK-RRKNUM
           MOVE    1           TO      WRK-RRKEDANUM
           PERFORM 4801-PTNYUINRRK-SEL-SEC
           IF    ( FLG-PTNYUINRRK  NOT =   ZERO )
               MOVE    "4028"  TO      SPA-ERRCD
               GO  TO  4801-TAINTUSANMINUS-UPD-EXT
           END-IF
      *
           IF    ( PTNYUINRRK-JTIKBN   NOT =   "5" )
               MOVE    "4030"  TO  SPA-ERRCD
           END-IF
      *
           COMPUTE PTNYUINRRK-TAISYONISSU
               =   PTNYUINRRK-TAISYONISSU  -   1
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  PTNYUINRRK-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTNYUINRRK-UPHMS
      *
           MOVE    "PTNYUINRRK-KEY"
                               TO  WRK-DBPATH
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
           PERFORM 900-TBL-UPDATE-SEC
           IF    ( FLG-DBRC    NOT =   ZERO )
               MOVE    "4029"  TO  SPA-ERRCD
               GO  TO  4801-TAINTUSANMINUS-UPD-EXT
           END-IF
      *
           .
       4801-TAINTUSANMINUS-UPD-EXT.
           EXIT.
      *****************************************************************
      *    異動前歴更新処理
      *****************************************************************
       4801-IDO-MAE-UPD-SEC  SECTION.
      *
      *    入院履歴マスタ検索
           PERFORM 4802-PTNYUINRRK-SEL-SEC
           IF    ( FLG-PTNYUINRRK  NOT =   ZERO )
               MOVE    "4013"  TO  SPA-ERRCD
               GO  TO  4801-IDO-MAE-UPD-EXT
           END-IF
      *
           MOVE    ALL "9"     TO  PTNYUINRRK-TENSTUYMD
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  PTNYUINRRK-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTNYUINRRK-UPHMS
      *
           MOVE    "PTNYUINRRK-KEY"
                               TO  WRK-DBPATH
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
           PERFORM 900-TBL-UPDATE-SEC
           IF    ( FLG-DBRC    NOT =   ZERO )
               MOVE    "4014"  TO  SPA-ERRCD
               GO  TO  4801-IDO-MAE-UPD-EXT
           END-IF
      *
           .
       4801-IDO-MAE-UPD-EXT.
           EXIT.
      *****************************************************************
      *    転出元歴更新処理
      *****************************************************************
       4801-TENSYUTU-MOTO-UPD-SEC  SECTION.
      *
      *    入院履歴マスタ検索
           PERFORM 4801-PTNYUINRRK-SEL-SEC
           IF    ( FLG-PTNYUINRRK  NOT =   ZERO )
               MOVE    "4015"  TO  SPA-ERRCD
               GO  TO  4801-TENSYUTU-MOTO-UPD-EXT
           END-IF
      *
           MOVE    SPA-NAI-I01-IDOYMD
                               TO  PTNYUINRRK-TENSTUYMD
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  PTNYUINRRK-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTNYUINRRK-UPHMS
      *
           MOVE    "PTNYUINRRK-KEY"
                               TO  WRK-DBPATH
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
           PERFORM 900-TBL-UPDATE-SEC
           IF    ( FLG-DBRC    NOT =   ZERO )
               MOVE    "4016"  TO  SPA-ERRCD
               GO  TO  4801-TENSYUTU-MOTO-UPD-EXT
           END-IF
      *
      *    更新データ用に入院科等を退避
           MOVE    PTNYUINRRK-NYUINKA
                               TO  WRK-NYUINKA-KEY
           MOVE    PTNYUINRRK-BTUNUM
                               TO  WRK-BTUNUM-KEY
           MOVE    PTNYUINRRK-BRMNUM
                               TO  WRK-BRMNUM-KEY
           MOVE    PTNYUINRRK-TOKUTEI-KBN
                               TO  WRK-TOKUTEI-KBN-KEY
           MOVE    PTNYUINRRK-HKNCOMBINUM
                               TO  WRK-HKNCOMBI-KEY
           MOVE    PTNYUINRRK-TOKUTEI-NYUIN
                               TO  WRK-TOKUTEI-NYUIN-KEY
      *
           .
       4801-TENSYUTU-MOTO-UPD-EXT.
           EXIT.
      *****************************************************************
      *    入院歴番号採番処理
      *****************************************************************
       4801-RRKNUM-EDIT-SEC        SECTION.
      *
           EVALUATE    SPA-GMN-I01-SYORIKBN-CD
      *    入院
           WHEN    "01"
      *
               MOVE    ZERO                TO  FLG-PTNYUINRRK
               INITIALIZE  PTNYUINRRK-REC
               MOVE    SPA-HOSPID          TO  PTNYUINRRK-HOSPID
               MOVE    SPA-NAI-I01-PTID    TO  PTNYUINRRK-PTID
               MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
               MOVE    "PTNYUINRRK-KEY9"   TO  WRK-DBPATH
               PERFORM 900-TBL-SELECT-SEC
               IF    ( FLG-DBRC    =   ZERO )
                   MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
               ELSE
                   MOVE    1               TO  FLG-PTNYUINRRK
               END-IF
      *
               MOVE    "PTNYUINRRK-KEY9"   TO  WRK-DBPATH
               PERFORM 900-TBL-CLOSE-SEC
      *
               IF    ( FLG-PTNYUINRRK  NOT =   ZERO )
                   COMPUTE     WRK-RRKNUM      =   1
                   COMPUTE     WRK-RRKEDANUM   =   1
               ELSE
                   COMPUTE     WRK-RRKNUM      =   PTNYUINRRK-RRKNUM
                                               +   1
                   COMPUTE     WRK-RRKEDANUM   =   1
               END-IF
      *
      *    転科　転棟　転室
           WHEN    "08"
               COMPUTE     WRK-RRKEDANUM   =   WRK-RRKEDANUM   +   1
      *    その他
           WHEN    OTHER
               CONTINUE
           END-EVALUATE
      *
           .
       4801-RRKNUM-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       4801-PTNYUINRRK-SEL-SEC     SECTION.
      *
      *    入院履歴検索
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPID          TO  PTNYUINRRK-HOSPID
           MOVE    SPA-NAI-I01-PTID    TO  PTNYUINRRK-PTID
           MOVE    WRK-RRKNUM          TO  PTNYUINRRK-RRKNUM
           MOVE    WRK-RRKEDANUM       TO  PTNYUINRRK-RRKEDANUM
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "PTNYUINRRK-KEY"    TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    =   ZERO  )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
      *    入院履歴クローズ
           MOVE    "PTNYUINRRK-KEY"    TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
       4801-PTNYUINRRK-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       4802-PTNYUINRRK-SEL-SEC     SECTION.
      *
      *    入院履歴検索
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPID          TO  PTNYUINRRK-HOSPID
           MOVE    SPA-NAI-I01-PTID    TO  PTNYUINRRK-PTID
           MOVE    WRK-RRKNUM          TO  PTNYUINRRK-RRKNUM
           COMPUTE PTNYUINRRK-RRKEDANUM
               =   WRK-RRKEDANUM       -   1
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "PTNYUINRRK-KEY"    TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    =   ZERO  )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
      *    入院履歴クローズ
           MOVE    "PTNYUINRRK-KEY"    TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
       4801-PTNYUINRRK-SEL-EXT.
           EXIT.
      *****************************************************************
      *    病室番号編集処理
      *****************************************************************
       4800-BRMNUM-EDIT-SEC        SECTION.
      *
           MOVE    SPACE       TO  WRK-STR1
           MOVE    1           TO  WRK-IDX1
           STRING  WRK-BRMNUM
               DELIMITED   BY  SPACE
               INTO    WRK-STR1
               WITH    POINTER WRK-IDX1
           END-STRING
      *
           COMPUTE WRK-IDX1    =   WRK-IDX1    -   1
      *
           IF    ( WRK-IDX1    <   1 )
               MOVE    WRK-BRMNUM  TO  WRK-BRMNUM-EDT
               GO  TO  4800-BRMNUM-EDIT-EXT
           END-IF
      *
      *    病室番号が数字の場合ゼロ詰めを行う
           IF    ( WRK-BRMNUM ( 1 : WRK-IDX1 )
                                   IS  NUMERIC )
               COMPUTE WRK-IDX2    =   6   -   WRK-IDX1 + 1
               MOVE    ZERO    TO  WRK-BRMNUM-EDT
               MOVE    WRK-BRMNUM ( 1 : WRK-IDX1 )
                               TO  WRK-BRMNUM-EDT (WRK-IDX2 : WRK-IDX1)
           ELSE
               MOVE    WRK-BRMNUM
                               TO  WRK-BRMNUM-EDT
           END-IF
      *
           .
       4800-BRMNUM-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院　会計テーブル編集処理
      *****************************************************************
       4801-NYUKAIKEI-EDIT-SEC         SECTION.
      *
      *---------------退院日の同日入院処理ここから----------------------
           MOVE   SPACE                TO  WRK-DOU-TENNYUYMD
      *    同日再入院用に入院日（転入日）を退避
           MOVE   SPA-NAI-I01-NYUINYMD TO  WRK-DOU-NAI-I01-NYUINYMD
           MOVE   PTNYUINRRK-TENNYUYMD TO  WRK-DOU-PTNYUINRRK-TENNYUYMD
      *    入院履歴の退避
           MOVE   PTNYUINRRK-REC       TO  WRK-PTNYUINRRK-REC
      *    最終入院履歴が自院歴かを判定する
           MOVE    ZERO                TO  FLG-JIINRRK
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPID          TO  PTNYUINRRK-HOSPID
           MOVE    SPA-NAI-I01-PTID    TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "PTNYUINRRK-KEY9"   TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    =   ZERO  )
               MOVE    "PTNYUINRRK-KEY9"   TO  WRK-DBPATH
               PERFORM  900-TBL-READ-SEC
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
               IF      PTNYUINRRK-JTIKBN   NOT =   "5"  AND "6"
                   IF  PTNYUINRRK-TAIINYMD  = WRK-DOU-NAI-I01-NYUINYMD                   
                       MOVE   1        TO  FLG-JIINRRK
                   END-IF
               END-IF
           END-IF
      *    入院履歴クローズ
           MOVE    "PTNYUINRRK-KEY9"   TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *    入院履歴の復元
           MOVE    WRK-PTNYUINRRK-REC  TO  PTNYUINRRK-REC
      *    前歴が自院歴（同日退院分）の場合、入院会計作成日を１日ずらす
           IF     FLG-JIINRRK    =     1
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    SPA-NAI-I01-NYUINYMD
                                       TO  LNK-DAY6-KIJUN
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE    1               TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"       USING     STS-AREA-DAY
                                                 LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  SPA-NAI-I01-NYUINYMD
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    WRK-DOU-PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE    1               TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"       USING     STS-AREA-DAY
                                                 LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  WRK-DOU-TENNYUYMD
           END-IF
      *---------------退院日の同日入院処理ここまで----------------------
      *
           INITIALIZE      ORCSSYOKIKSNAREA
      *
      *    通算日数、算定開始日を求める
           PERFORM 4801-TSUSAN-SANTEI-GET-SEC
           IF    ( SPA-ERRCD    NOT =  SPACE )
               GO  TO  4801-NYUKAIKEI-EDIT-EXT
           END-IF
      *
           MOVE    WRK-TSUSAN      TO  SYOKIKSN-IN-TUSANNISSU
           MOVE    WRK-SANTEISTYMD
                                   TO  SYOKIKSN-IN-SANTEISTYMD
      *
           IF      SPA-GMN-I01-TOKNYUIN-CD    NOT =   SPACE
               PERFORM   4801-TOKTEI-NYUIN-SET-SEC
           ELSE
               IF  SYS-5001-BTU-KHNSRYCD      NOT =   SPACE
                   MOVE    SYS-5001-BTU-KHNSRYCD
                                       TO  SYOKIKSN-IN-KHNSRYCD
                   PERFORM   4801-RJNCD-CHG-SEC
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "4017"      TO  SPA-ERRCD
                       GO  TO  4801-NYUKAIKEI-EDIT-EXT
                   END-IF
               ELSE
                   MOVE    "4018"      TO  SPA-ERRCD
                   GO  TO  4801-NYUKAIKEI-EDIT-EXT
               END-IF
           END-IF
      *
      *    入院会計作成サブの呼出し
           INITIALIZE            ORCSNYUACCT-AREA
           MOVE     "1"     TO   LNK-SCNYUACCT-SYORI-KBN
           MOVE     "1"                TO  SPA-NYUGAIKBN
           MOVE     PTNYUINRRK-PTID    TO  SPA-PTID
           MOVE     PTNYUINRRK-NYUINKA TO  SPA-SRYKA
           MOVE    SYOKIKSN-IN-KHNSRYCD
                            TO   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                            TO   LNK-SCNYUACCT-STSRYYMD
           PERFORM   VARYING   IDX6   FROM   1   BY   1
               UNTIL (SYOKIKSN-OT-KGNSRYCD(IDX6)  =  SPACE) OR
                     (IDX6   >     7)
               MOVE   SYOKIKSN-OT-KGNSRYCD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOSTYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOEDYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX6)
           END-PERFORM
      *
      *    通算算定済日数
           MOVE   WRK-TSUSAN    TO  LNK-SCNYUACCT-NYUKHN-NISUU
      *
      *    特定入院で入院料が変更になるコードの対応
      *
      *    診療所老人医療管理料
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190774110"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190774110"  TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   SPA-NAI-I01-NYUINYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-NYUINYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    13                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *
                 MOVE   "190774210"  TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(2)
                 MOVE    "99999999"
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
           END-IF
      *    老人性痴呆疾患治療病棟入院料
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190739910"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU02      <=  90
                     ADD    1           TO  TN-IDX
                     MOVE   "190739910" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              老人性痴呆疾患治療病棟入院料の９０日以内の終日を再計算
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  90  - STUSAN-NISSU02
                     MOVE   SPA-NAI-I01-NYUINYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   SPA-NAI-I01-NYUINYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190740010"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     MOVE    SPA-NAI-I01-NYUINYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
           END-IF
      *
      *    老人性痴呆疾患治療病棟入院料２
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190813410"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU02      <=  90
                     ADD    1           TO  TN-IDX
                     MOVE   "190813410" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              老人性痴呆疾患治療病棟入院料の９０日以内の終日を再計算
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  90  - STUSAN-NISSU02
                     MOVE   SPA-NAI-I01-NYUINYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   SPA-NAI-I01-NYUINYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190813510"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     MOVE    SPA-NAI-I01-NYUINYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
           END-IF
      *
      *    特定入院料で７日まで、１４日まで、それ以降は一般入院料を算定する
      *    場合の診療コードを取得
           PERFORM    4801-TOKUTEI-NYUIN-7-14-SEC
      *
           IF    SYOKIKSN-IN-KHNSRYCD   =   WRK-TOKUTEI-SRYCD1
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   WRK-TOKUTEI-SRYCD1  
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   SPA-NAI-I01-NYUINYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-NYUINYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     6                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *
                 MOVE   WRK-TOKUTEI-SRYCD2
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(2)
                 MOVE    "99999999"
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
      *          一般入院料の算定開始日を算定
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-NYUINYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     14               TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          一般入院会計作成用のフラグ設定
                 MOVE    "1"      TO      IPN-NYUKAIKEI-SAKUSEI-FLG
           END-IF
      *
      *    広範囲熱傷特定集中治療室管理料（６０日上限）
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190024810"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190024810"  TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   SPA-NAI-I01-NYUINYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-NYUINYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    59                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *          一般入院料の算定開始日を算定
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-NYUINYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     60               TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          特定入院期間（６０日経過）した際の一般入院料作成の
      *          可否を判定
                 MOVE    SPA-NAI-I01-NYUINYMD
                                  TO      WRK-IPN-HANTEI-YMD1
                 IF      WRK-IPN-HANTEI-M1   =   12
                     ADD    1     TO      WRK-IPN-HANTEI-Y1
                     MOVE   1     TO      WRK-IPN-HANTEI-M1
                 ELSE
                     ADD    1     TO      WRK-IPN-HANTEI-M1
                 END-IF
      *          月末日を算定
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY7-AREA
                 MOVE    "71"              TO  LNK-DAY7-IRAI
                 MOVE   WRK-IPN-HANTEI-YM1 TO  LNK-DAY7-YM
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
                 MOVE    LNK-DAY7-KOYOMI   TO      WRK-IPN-HANTEI-YMD1
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                  TO      WRK-IPN-HANTEI-YMD2
      *          一般入院会計作成用のフラグ設定
                 IF      WRK-IPN-HANTEI-YMD1 > WRK-IPN-HANTEI-YMD2
                     MOVE    "1"   TO     IPN-NYUKAIKEI-SAKUSEI-FLG
                 END-IF
           END-IF
      *
      *    ハイケアユニット入院医療管理料（２１日を限度）
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190117310"
                 MOVE   SPACE       TO   IPN-NYUKAIKEI-SAKUSEI-FLG
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190117310"
                                    TO   LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   SPA-NAI-I01-NYUINYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-NYUINYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    20                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *          一般入院料の算定開始日を算定
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-NYUINYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     21               TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          一般入院会計作成用のフラグ設定
                 MOVE    "1"      TO      IPN-NYUKAIKEI-SAKUSEI-FLG
           END-IF
      *
      *    脳卒中ケアユニット入院医療管理料（１４日を限度）
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190120810"
                 MOVE   SPACE       TO   IPN-NYUKAIKEI-SAKUSEI-FLG
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190120810"
                                    TO   LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   SPA-NAI-I01-NYUINYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-NYUINYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    13                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *          一般入院料の算定開始日を算定
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-NYUINYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     14               TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          一般入院会計作成用のフラグ設定
                 MOVE    "1"      TO      IPN-NYUKAIKEI-SAKUSEI-FLG
           END-IF
      *
      *    精神科救急入院料
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190121010"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190121010" TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   SPA-NAI-I01-NYUINYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-NYUINYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    29                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *
                 MOVE   "190111410"    TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(2)
                 MOVE    "99999999"
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
           END-IF
      *
      *    精神科急性期治療病棟入院料１
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190121110"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190121110" TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   SPA-NAI-I01-NYUINYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-NYUINYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    29                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *
                 MOVE   "190066910"    TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(2)
                 MOVE    "99999999"
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
           END-IF
      *
      *    精神科急性期治療病棟入院料２
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190121210"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190121210" TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   SPA-NAI-I01-NYUINYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-NYUINYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    29                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *
                 MOVE   "190067010"    TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(2)
                 MOVE    "99999999"
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
           END-IF
      *
      *-------特定入院対応ここまで-----------------------------------
      *
      *    入院料加算診療コード取り込み
           PERFORM    4801-NYUKSN-GET-SEC
      *
      *    特定入院料区分のセット
           IF      SPA-GMN-I01-TOKNYUIN-CD    =   SPACE
               MOVE     "0"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
           ELSE
               MOVE     "1"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
      *        老人特定入院基本料及び平成１８年７月以降の療養病棟、
      *        療養病床の場合は特定入院料扱いから外す
               IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410" OR
                                                       "190799510" OR
                                                       "190121310" OR
                                                       "190121410" OR
                                                       "190121510" OR
                                                       "190121610" OR
                                                       "190121710" OR
                                                       "190121810" OR
                                                       "190122010" OR
                                                       "190122110" OR
                                                       "190122210" OR
                                                       "190122310" OR
                                                       "190122410" OR
                                                       "190121510"
                   MOVE  "0"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
               END-IF
           END-IF
      *    外泊
           MOVE   "099999911"   TO  LNK-SCNYUACCT-GAIHAKU-SRYCD
      *    室料差額
           MOVE   "099999912"   TO  LNK-SCNYUACCT-SAGAKU-SRYCD
           IF     PTNYUINRRK-RM-SAGAKU   =   SPACE
               MOVE    ZERO     TO  LNK-SCNYUACCT-SAGAKU-KBN
           ELSE
               MOVE   PTNYUINRRK-RM-SAGAKU 
                                TO  WRK-SAGAKU-KBN
               MOVE   WRK-SAGAKU-KBN9
                                TO  LNK-SCNYUACCT-SAGAKU-KBN
           END-IF
      *    食事区分
           MOVE   "099999913"   TO  LNK-SCNYUACCT-SHOKUJI-SRYCD
           MOVE   1             TO  LNK-SCNYUACCT-SHOKUJIKBN
      *    食堂加算
           PERFORM     4801-BTUSEL-SEC
           IF   FLG-SYSKANRI           =    ZERO
               IF   SYS-5001-BTU-SYOKUDOKBN       =    "2"
                    MOVE     "197000570"    TO
                                LNK-SCNYUACCT-SHOKUDO-SRYCD
                    MOVE    1   TO   LNK-SCNYUACCT-SHOKUDO-KBN
               END-IF
           END-IF
      *    保険組合せ
           MOVE   "099999914"   TO  LNK-SCNYUACCT-HKNCOMBINUM-SRYCD
           MOVE   PTNYUINRRK-HKNCOMBINUM
                                TO  WRK-HKNCOMBINUM
           MOVE   WRK-HKNCOMBINUM9 
                                TO  LNK-SCNYUACCT-HKNCOMBINUM
           MOVE   SPA-SRYKA     TO  WRK-TAIHI-SRYKA
           MOVE   "00"          TO  SPA-SRYKA
           PERFORM 4801-SENTEI-SET-SEC
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
               DISPLAY  "ORCGI01 ORCSNYUACCT ERR RC = "
                         LNK-SCNYUACCT-RC
           END-IF
      *
           IF     IPN-NYUKAIKEI-SAKUSEI-FLG    =   "1"
               MOVE    SPACE   TO   SPA-GMN-I01-TOKNYUIN-CD
               MOVE    IPN-SANTEISTYMD   TO   WRK-SANTEISTYMD
               MOVE    IPN-SANTEISTYMD   TO   SPA-NAI-I01-IDOYMD
               MOVE    IPN-SANTEISTYMD   TO   PTNYUINRRK-TENNYUYMD
               PERFORM   4801-TENROOM-EDIT-SEC
           END-IF
      *
           .
       4801-NYUKAIKEI-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定入院料（７日／１４日／それ以降）の診療コードセット
      *****************************************************************
       4801-TOKUTEI-NYUIN-7-14-SEC         SECTION.
      *
      *特定入院料で７日まで、１４日まで、それ以降は一般入院料を算定する
      *場合の診療コードを取得
      *
           MOVE      SPACE        TO    IPN-NYUKAIKEI-SAKUSEI-FLG
           MOVE      "999999999"  TO    WRK-TOKUTEI-SRYCD1
                                        WRK-TOKUTEI-SRYCD2
           EVALUATE   SYOKIKSN-IN-KHNSRYCD
      *    特定集中治療室管理料
               WHEN   "190024610"
                   MOVE    "190024610"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190074610"  TO  WRK-TOKUTEI-SRYCD2
      *    特定集中治療室管理料（重症者等比率基準未満）
               WHEN   "190116310"
                   MOVE    "190116310"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190116410"  TO  WRK-TOKUTEI-SRYCD2
      *    救命救急入院料１
               WHEN   "190024510"
                   MOVE    "190024510"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190074510"  TO  WRK-TOKUTEI-SRYCD2
      *    救命救急入院料１（高度）
               WHEN   "190110210"
                   MOVE    "190110210"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190110610"  TO  WRK-TOKUTEI-SRYCD2
      *    救命救急入院料１（Ａまたは新設）
               WHEN   "190110310"
                   MOVE    "190110310"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190110710"  TO  WRK-TOKUTEI-SRYCD2
      *    救命救急入院料２
               WHEN   "190024310"
                   MOVE    "190024310"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190024410"  TO  WRK-TOKUTEI-SRYCD2
      *    救命救急入院料２（高度）
               WHEN   "190110410"
                   MOVE    "190110410"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190110810"  TO  WRK-TOKUTEI-SRYCD2
      *    救命救急入院料２（Ａまたは新設）
               WHEN   "190110510"
                   MOVE    "190110510"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190110910"  TO  WRK-TOKUTEI-SRYCD2
      *    一類感染症患者入院医療管理料
               WHEN   "190075510"
                   MOVE    "190075510"  TO  WRK-TOKUTEI-SRYCD1
                   MOVE    "190075610"  TO  WRK-TOKUTEI-SRYCD2
           END-EVALUATE
           .
      *
       4801-TOKUTEI-NYUIN-7-14-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料加算の取り込み処理
      *****************************************************************
       4801-NYUKSN-GET-SEC         SECTION.
      *
           MOVE   ZERO             TO   IDX8
      *
      *医療機関加算（減算）情報
           INITIALIZE              SYSKANRI-REC
           MOVE    "5000"          TO  SYS-KANRICD
           EVALUATE   SPA-GMN-I01-SYORIKBN-CD
               WHEN   "01"
                   MOVE   SPA-NAI-I01-NYUINYMD
                                   TO  ORC-DBYMD
               WHEN   "07"
                   MOVE   SPA-NAI-I01-TAIINYMD
                                   TO  ORC-DBYMD
               WHEN   "08"
                   MOVE   SPA-NAI-I01-IDOYMD
                                   TO  ORC-DBYMD
               WHEN   "09"
                   MOVE   SPA-NAI-I01-MAEIDOYMD
                                   TO  ORC-DBYMD
               WHEN   OTHER
                   MOVE    SPA-SYSYMD
                                   TO  ORC-DBYMD
           END-EVALUATE
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
      *
      *    システム管理マスタの医療機関情報より取得
           MOVE    "SYSKANRI-KEY2" TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
      *
           IF   FLG-DBRC           =    ZERO
               MOVE    MCPDATA-REC TO   SYS-5000-REC
      *        入院料加算
               PERFORM   VARYING   IDX7   FROM   1  BY  1
                 UNTIL  (SYS-5000-KSNCD(IDX7)    =   SPACE)  OR
                        (IDX7                    >   20)
                    IF   SYS-5000-KSNKBN(IDX7)   =   "2"
                             ADD    1                TO  IDX8
                             MOVE   SYS-5000-KSNCD(IDX7)  TO
                                             WRK-NYUKHNKSN-SRYCD(IDX8)
                    END-IF
               END-PERFORM
      *        地域加算
               IF   SYS-5000-CHIIKIKSN    NOT =   SPACE
                    ADD    1        TO   IDX8
                    MOVE   SYS-5000-CHIIKIKSN   TO
                                             WRK-NYUKHNKSN-SRYCD(IDX8)
               END-IF
           END-IF
      *
      *    システム管理マスタクローズ
           MOVE    "SYSKANRI-KEY2" TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
      *病棟加算情報
           PERFORM     4801-BTUSEL-SEC
      *
           IF   FLG-SYSKANRI           =    ZERO
      *
      *        看護補助加算
               IF    SYS-5001-BTU-KNGHJOKSN   NOT =   SPACE
                   ADD      1          TO     IDX8
                   EVALUATE     SYS-5001-BTU-KNGHJOKSN
                       WHEN     "01"
                           MOVE  "190103070"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "02"
                           MOVE  "190103470"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "03"
                           MOVE  "190103770"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "04"
                           MOVE  "190103970"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "05"
                           MOVE  "190104070"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
      *                平成１８年４月改正対応
                       WHEN     "06"
                           MOVE  "190103770"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "07"
                           MOVE  "190103870"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "08"
                           MOVE  "190103970"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "09"
                           MOVE  "190104070"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                   END-EVALUATE
               END-IF
      *
      *        夜間勤務等看護加算
               IF    SYS-5001-BTU-YAKANKNGKSN   NOT =   SPACE
                   ADD      1          TO     IDX8
                   EVALUATE     SYS-5001-BTU-YAKANKNGKSN
                       WHEN     "01"
                           MOVE  "190109870"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "02"
                           MOVE  "190076070"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "03"
                           MOVE  "190076170"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "04"
                           MOVE  "190076370"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "05"
                           MOVE  "190076470"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                   END-EVALUATE
               END-IF
      *
      *        小児入院医療管理料加算（保育士１人以上）
               MOVE  SPACE       TO   WRK-BTU-HIKSISUKBN
               IF    SYS-5001-BTU-HIKSISUKBN   =   "2"
                   MOVE   "2"    TO   WRK-BTU-HIKSISUKBN
               END-IF
      *
      *        病棟入院料加算
               PERFORM   VARYING   IDX7   FROM   1  BY  1
                 UNTIL  (IDX7                    >   14)
                   EVALUATE   IDX7
                       WHEN   3
                       WHEN   4
                       WHEN   5
                           CONTINUE
      *                児童思春期精神科入院医療管理加算（２０才未満）
                       WHEN   6
                           IF      SYS-5001-KSNKBN(IDX7)   =   "2"
                               PERFORM   4801-PT-AGE-SEC
                               IF      WRK-NENREI          <      20
                                   ADD    1                TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                               WRK-NYUKHNKSN-SRYCD(IDX8)
                               END-IF
                           END-IF
      *                看護配置適合加算
                       WHEN   13
                           MOVE  SPACE       TO   WRK-BTU-KNGTEK-KBN
                           IF    SYS-5001-KNGTEK-KBN   =   "2"
                               MOVE   "2"    TO   WRK-BTU-KNGTEK-KBN
                           END-IF
      *                複数医師及び看護配置適合加算
                       WHEN   14
                           MOVE  SPACE       TO   WRK-BTU-DRTEK-KBN
                           IF    SYS-5001-DRTEK-KBN   =   "2"
                               MOVE   "2"    TO   WRK-BTU-DRTEK-KBN
                           END-IF
                       WHEN   OTHER
                           IF     SYS-5001-KSNKBN(IDX7)    =   "2"
                               ADD    1                TO  IDX8
                               MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                           END-IF
                   END-EVALUATE
               END-PERFORM
           END-IF
      *
      *病室情報
           PERFORM     4801-BRMSEL-SEC
      *
           IF   FLG-SYSKANRI       =    ZERO
      *        新生児入院医療管理加算
               IF   SYS-5002-SINSEIJI-KBN     =   "2"
                    ADD     1       TO     IDX8
                    MOVE   SYS-5002-SINSEIJI-CD  TO
                                   WRK-NYUKHNKSN-SRYCD(IDX8)
               END-IF
      *        診療所療養病床療養環境加算（１）
               IF   SYS-5002-SNR-RYOYO-KBN(1)     =   "2"
                    ADD     1       TO     IDX8
                    MOVE   SYS-5002-SNR-RYOYO-CD(1)  TO
                                   WRK-NYUKHNKSN-SRYCD(IDX8)
               END-IF
      *        診療所療養病床療養環境加算（２）
               IF   SYS-5002-SNR-RYOYO-KBN(2)     =   "2"
                    ADD     1       TO     IDX8
                    MOVE   SYS-5002-SNR-RYOYO-CD(2)  TO
                                   WRK-NYUKHNKSN-SRYCD(IDX8)
               END-IF
      *        放射線治療病室管理加算
               IF   SYS-5002-HOSYASEN-KBN     =   "2"
                    ADD     1       TO     IDX8
                    MOVE   SYS-5002-HOSYASEN-CD  TO
                                   WRK-NYUKHNKSN-SRYCD(IDX8)
               END-IF
      *        療養環境加算
               IF   SYS-5002-RYOYO-KBN     =   "2"
                    ADD     1       TO     IDX8
                    MOVE   SYS-5002-RYOYO-CD  TO
                                   WRK-NYUKHNKSN-SRYCD(IDX8)
               END-IF
      *        療養病棟療養環境加算１（平成１８年新設）
               IF   SYS-5002-BTU-RYOYO-H18-KBN     =   "2"
                    ADD     1       TO     IDX8
      *              MOVE   "190120210"  TO
                    MOVE   SYS-5002-BTU-RYOYO-H18-CD  TO
                                   WRK-NYUKHNKSN-SRYCD(IDX8)
               END-IF
      *        療養病棟療養環境加算１
               IF   SYS-5002-BTU-RYOYO-KBN(1)     =   "2"
                    ADD     1       TO     IDX8
                    MOVE   SYS-5002-BTU-RYOYO-CD(1)  TO
      *              MOVE   "190106070"  TO
                                   WRK-NYUKHNKSN-SRYCD(IDX8)
               END-IF
      *        療養病棟療養環境加算２
               IF   SYS-5002-BTU-RYOYO-KBN(2)     =   "2"
                    ADD     1       TO     IDX8
      *              MOVE   "190106170"  TO
                    MOVE   SYS-5002-BTU-RYOYO-CD(2)  TO
                                   WRK-NYUKHNKSN-SRYCD(IDX8)
               END-IF
      *        療養病棟療養環境加算３
               IF   SYS-5002-BTU-RYOYO-KBN(3)     =   "2"
                    ADD     1       TO     IDX8
      *              MOVE   "190106270"  TO
                    MOVE   SYS-5002-BTU-RYOYO-CD(3)  TO
                                   WRK-NYUKHNKSN-SRYCD(IDX8)
               END-IF
      *        重症者等療養環境特別加算（個室）
               IF   SYS-5002-JYUTOK1-KBN          =   "2"
                    ADD     1       TO     IDX8
                    MOVE   SYS-5002-JYUTOK1-CD  TO
                                   WRK-NYUKHNKSN-SRYCD(IDX8)
               END-IF
      *        重症者等療養環境特別加算（２人部屋）
               IF   SYS-5002-JYUTOK2-KBN          =   "2"
                    ADD     1       TO     IDX8
                    MOVE   SYS-5002-JYUTOK2-CD  TO
                                   WRK-NYUKHNKSN-SRYCD(IDX8)
               END-IF
      *        無菌治療室管理加算
               IF   SYS-5002-MUKIN-KBN            =   "2"
                    ADD     1       TO     IDX8
                    MOVE   SYS-5002-MUKIN-CD  TO
                                   WRK-NYUKHNKSN-SRYCD(IDX8)
               END-IF
           END-IF
      *
      *    入院料と入院料加算の併算定チェック
           MOVE    ZERO            TO     IDX9
           PERFORM    VARYING    IDX8   FROM   1   BY   1
                      UNTIL      (WRK-NYUKHNKSN-SRYCD(IDX8)  =  SPACE)
                         OR      (IDX8   >     20)
                         PERFORM   4801-NYUKSN-CHK-SEC
                         IF     FLG-NYUKSN       =   ZERO   
                             ADD    1                TO  IDX9
                             MOVE   WRK-NYUKHNKSN-SRYCD(IDX8)  TO
                                   LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX9)
                         END-IF
           END-PERFORM
      *
      *    小児入院医療管理料加算（保育士１人以上）
           IF    (WRK-BTU-HIKSISUKBN   =   "2")  AND
                 (IDX9    <   20 )
               ADD    1            TO  IDX9
               MOVE  "190111370"   TO
                                   LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX9)
           END-IF
      *
      *    看護配置適合加算
           IF    (WRK-BTU-KNGTEK-KBN   =   "2")  AND
                 (IDX9    <   20 )
               ADD    1            TO  IDX9
               MOVE  "190109670"   TO
                                   LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX9)
           END-IF
      *
      *    複数医師及び看護配置適合加算
           IF    (WRK-BTU-DRTEK-KBN   =   "2")  AND
                 (IDX9    <   20 )
               ADD    1            TO  IDX9
               MOVE  "190117070"   TO
                                   LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX9)
           END-IF
      *
           .
      *
       4801-NYUKSN-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料加算の算定可・不可チェック
      *****************************************************************
       4801-NYUKSN-CHK-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-NYUKSN
      *
      **     EVALUATE   WRK-NYUKHNKSN-SRYCD(IDX8)
      *        診療録管理体制加算
      **         WHEN   "190100370"
      **             IF   SPA-GMN-I01-SYORIKBN-CD   =  "07" OR "08"
      **                  MOVE   1        TO     FLG-NYUKSN
      **             END-IF
      *            自院での継続入院チェック
      **     END-EVALUATE
      *
      *    入院料加算チェックテーブルによる判定
           INITIALIZE  NYUKSNCHK-REC
      *
           PERFORM   48011-TENSU-SEL-SEC
           MOVE    WRK-BTU-NYUKSNKBN
                                   TO  NYUKSNCHK-NYUINKBN
           PERFORM   48012-TENSU-SEL-SEC
           MOVE    WRK-KSN-NYUKSNKBN
                                   TO  NYUKSNCHK-KSNKBN
           MOVE    SYOKIKSN-IN-SANTEISTYMD    TO  ORC-DBYMD
      *
           MOVE    "NYUKSNCHK-KEY" TO  WRK-DBPATH
           MOVE    NYUKSNCHK-REC   TO  MCPDATA-REC
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        =   ZERO )
               MOVE    MCPDATA-REC TO  NYUKSNCHK-REC
               IF    ( NYUKSNCHK-CHKKBN    =   "1" OR "2" )
                   CONTINUE
               ELSE
                   MOVE   1        TO     FLG-NYUKSN
               END-IF
           ELSE
               MOVE   1        TO     FLG-NYUKSN
           END-IF
      *
      *
      *    入院料加算チェッククローズ
           MOVE    "NYUKSNCHK-KEY" TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
      *    老人特定入院基本料の加算チェック漏れ対応
           IF   (WRK-BTU-NYUKSNKBN   =  "099")  AND
                (WRK-KSN-NYUKSNKBN   =  "407"   OR
                                        "408"   OR
                                        "409"   OR
                                        "410"   OR
                                        "411"   OR
                                        "412")
               MOVE   ZERO     TO     FLG-NYUKSN
           END-IF
           .
      *
       4801-NYUKSN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ検索処理（入院料）
      *****************************************************************
       48011-TENSU-SEL-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-NYUKSNKBN
           MOVE    SPACE           TO  WRK-TOKUBETSU-NYUIN
      *
           MOVE    ZERO            TO  FLG-TENSU
           INITIALIZE              TNS-TENSU-REC
      *
           IF      LNK-SCNYUACCT-NYUKHN-SRYCD(1) NOT = SPACE
               MOVE   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                   TO  TNS-SRYCD
           ELSE
               MOVE   LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                   TO  TNS-SRYCD
           END-IF
           MOVE    SYOKIKSN-IN-SANTEISTYMD    TO  ORC-DBYMD
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
      *
           MOVE    "TENSU-KEY"     TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
      *    点数マスタクローズ
           MOVE    "TENSU-KEY"     TO  ORC-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF    ( FLG-TENSU       NOT =   ZERO )
               GO  TO  48011-TENSU-SEL-EXT
           END-IF
      *
           MOVE    TNS-NYUKHNKSNKBN
                                   TO  WRK-BTU-NYUKSNKBN
      *
      *    特別入院料の判定
           EVALUATE   TNS-NYUKHNKBN
               WHEN   73
               WHEN   74
               WHEN   75
               WHEN   76
               WHEN   77
               WHEN   78
               WHEN   79
                   MOVE    "1"     TO  WRK-TOKUBETSU-NYUIN
           END-EVALUATE
           .
      *
       48011-TENSU-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ検索処理（入院料加算）
      *****************************************************************
       48012-TENSU-SEL-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-NYUKSNKBN
      *
           MOVE    ZERO            TO  FLG-TENSU
           INITIALIZE              TNS-TENSU-REC
      *
      *    特別入院料算定時に幼児加算と乳幼児加算の加算コード置き換え
           IF      WRK-TOKUBETSU-NYUIN   =   "1"
               EVALUATE   WRK-NYUKHNKSN-SRYCD(IDX8)
                   WHEN   "190100470"
                       MOVE  "190100570"  TO  WRK-NYUKHNKSN-SRYCD(IDX8)
                   WHEN   "190100770"
                       MOVE  "190100870"  TO  WRK-NYUKHNKSN-SRYCD(IDX8)
               END-EVALUATE
           END-IF
      *
           MOVE    WRK-NYUKHNKSN-SRYCD(IDX8)
                                   TO  TNS-SRYCD
           MOVE    SYOKIKSN-IN-SANTEISTYMD    TO  ORC-DBYMD
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
      *
           MOVE    "TENSU-KEY"     TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
      *    点数マスタクローズ
           MOVE    "TENSU-KEY"     TO  ORC-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF    ( FLG-TENSU       NOT =   ZERO )
               GO  TO  48012-TENSU-SEL-EXT
           END-IF
      *
           MOVE    TNS-NYUKHNKSNKBN
                                   TO  WRK-KSN-NYUKSNKBN
           .
      *
       48012-TENSU-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院患者の年齢算出
      *****************************************************************
       4801-PT-AGE-SEC         SECTION.
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPID          TO  PTINF-HOSPID
           MOVE    PTNYUINRRK-PTID     TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "PTINF-KEY"         TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    =   ZERO  )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           IF      FLG-PTINF           =   1
               MOVE    ZERO            TO  WRK-NENREI
           ELSE
               MOVE   PTINF-BIRTHDAY   TO  WRK-YMDS1
               MOVE   SYOKIKSN-IN-SANTEISTYMD
                                       TO  WRK-YMDS2
               MOVE   ZERO             TO  WRK-NENREI
               COMPUTE  WRK-NENREI     =   WRK-YYS2
                                       -   WRK-YYS1
               IF   WRK-YMDS1(5:4)     >   WRK-YMDS2(5:4)
                    ADD   -1           TO  WRK-NENREI
               END-IF
           END-IF
      *
           .
      *
       4801-PT-AGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院取消　会計テーブル編集処理
      *****************************************************************
       4801-NYUKAIKEI-ERASE-SEC         SECTION.
      *
           PERFORM     VARYING   IDX6   FROM   1   BY   1
                       UNTIL    (WRK-TENKA-SRYKA(IDX6)  =  SPACE)   OR
                                (IDX6       >   50)
                PERFORM  4801-NYUKAIKEI-ERASE-SUB-SEC
           END-PERFORM
      *
           .
       4801-NYUKAIKEI-ERASE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院取消　会計テーブル編集処理２
      *****************************************************************
       4801-NYUKAIKEI-ERASE-SUB-SEC         SECTION.
      *
      *---------------退院日の同日入院処理ここから----------------------
           MOVE   SPACE                TO  WRK-DOU-TENNYUYMD
      *    同日再入院用に入院日（転入日）を退避
           MOVE   SPA-NAI-I01-NYUINYMD TO  WRK-DOU-NAI-I01-NYUINYMD
      *    入院履歴の退避
           MOVE   PTNYUINRRK-REC       TO  WRK-PTNYUINRRK-REC
      *    最終入院履歴が自院歴かを判定する
           MOVE    ZERO                TO  FLG-JIINRRK
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPID          TO  PTNYUINRRK-HOSPID
           MOVE    SPA-NAI-I01-PTID    TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "PTNYUINRRK-KEY9"   TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    =   ZERO  )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
               IF      PTNYUINRRK-JTIKBN   NOT =   "5"  AND "6"
                   IF  PTNYUINRRK-TAIINYMD  = WRK-DOU-NAI-I01-NYUINYMD                   
                       MOVE   1        TO  FLG-JIINRRK
                   END-IF
               END-IF
           END-IF
      *    入院履歴クローズ
           MOVE    "PTNYUINRRK-KEY9"   TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *    入院履歴の復元
           MOVE    WRK-PTNYUINRRK-REC  TO  PTNYUINRRK-REC
      *    前歴が自院歴（同日退院分）の場合、入院会計作成日を１日ずらす
           IF     FLG-JIINRRK    =     1
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    SPA-NAI-I01-NYUINYMD
                                       TO  LNK-DAY6-KIJUN
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE    1               TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"       USING     STS-AREA-DAY
                                                 LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  SPA-NAI-I01-NYUINYMD
           END-IF
      *---------------退院日の同日入院処理ここまで----------------------
      *    入院会計作成サブの呼出し
           INITIALIZE            ORCSNYUACCT-AREA
           MOVE     SPA-GMN-I01-SYORIKBN-CD(2:1)
                                       TO   LNK-SCNYUACCT-SYORI-KBN
           MOVE     "1"                TO  SPA-NYUGAIKBN
           MOVE     PTNYUINRRK-PTID    TO  SPA-PTID
           MOVE     WRK-TENKA-SRYKA(IDX6)
                                       TO  SPA-SRYKA
           MOVE     SPA-NAI-I01-NYUINYMD
                                       TO   LNK-SCNYUACCT-STSRYYMD
      *    保険組合せ
           MOVE   PTNYUINRRK-HKNCOMBINUM
                                        TO  WRK-HKNCOMBINUM
           MOVE   WRK-HKNCOMBINUM9      TO  LNK-SCNYUACCT-HKNCOMBINUM
           MOVE   SPA-SRYKA     TO  WRK-TAIHI-SRYKA
           MOVE   "00"          TO  SPA-SRYKA
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
                  DISPLAY  "ORCGI01 ORCSNYUACCT ERR RC = "
                            LNK-SCNYUACCT-RC
           END-IF
      *
           .
       4801-NYUKAIKEI-ERASE-SUB-EXT.
           EXIT.
      *
      *****************************************************************
      *    退院取消　会計テーブル編集処理
      *****************************************************************
       4801-TAIINKAIKEI-CANCEL-SEC         SECTION.
      *
           MOVE      SPACE        TO    WRK-DOU-TENNYUYMD
           MOVE      SPACE        TO    IPN-NYUKAIKEI-SAKUSEI-FLG
      *
      *    特定集中治療室管理料算定時に退院日をＳＰＡ−異動日と転入日にセット
      *    以下の特定入院料も同様
      *      救命救急入院料
      *      一類感染症患者入院医療管理料
      *      広範囲熱傷特定集中治療室管理料
      *      ハイケアユニット入院医療管理料
      *      亜急性期入院医療管理料
      *      脳卒中ケアユニット入院医療管理料
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN  = "03" OR "04" OR "05" OR
                                                "06" OR "07" OR "08" OR
                                                "09" OR "13" OR "14" OR
                                                "16" OR "17" OR "18" OR
                                                "19"
                    MOVE   SPA-NAI-I01-TAIINYMD
                                       TO   PTNYUINRRK-TENNYUYMD
                                            SPA-NAI-I01-IDOYMD

               END-IF
           ELSE
               IF  PTNYUINRRK-TOKUTEI-KBN   =   SPACE
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    SPA-NAI-I01-TAIINYMD
                                               TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE     1                  TO  LNK-DAY6-ZOGEN
                   CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN     TO  PTNYUINRRK-TENNYUYMD
      *             MOVE   SPA-NAI-I01-TAIINYMD
      *                                 TO   PTNYUINRRK-TENNYUYMD
               END-IF
           END-IF
      *
      *    退院取消前の会計元データ作成
           INITIALIZE      ORCSSYOKIKSNAREA
      *
      *    入院会計削除前に会計データの食事区分と保険組合せを退避
           INITIALIZE                      NYUINACCT-REC
           MOVE    PTNYUINRRK-HOSPID   TO  NACCT-HOSPID
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    "00"                TO  NACCT-SRYKA
           MOVE   SPA-NAI-I01-TAIINYMD(1:6)
                                       TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "NYUINACCT-KEY16"   TO  WRK-DBPATH
           PERFORM   900-TBL-SELECT-SEC
           IF      FLG-DBRC            =   ZERO
               MOVE    ZERO        TO      WRK-TAIHI-SHOKUJI
               PERFORM UNTIL       FLG-DBRC     NOT =   ZERO
      *
                   MOVE     MCPDATA-REC   TO   NYUINACCT-REC
      *
      *            食事を退避
                   IF      NACCT-ZAISKBKBN     =   "4"
                       MOVE    SPA-NAI-I01-TAIINYMD(7:2)
                                         TO    IDX-SHOKUJIX
                       IF      NACCT-DAY(IDX-SHOKUJI9)  NOT =   ZERO
                            MOVE   NACCT-DAY(IDX-SHOKUJI9) TO
                                                  WRK-TAIHI-SHOKUJI
                       END-IF
                   END-IF
      *            保険を退避
                   IF      NACCT-ZAISKBKBN     =   "5"
                       MOVE   SPA-NAI-I01-TAIINYMD(7:2)
                                         TO    IDX-SHOKUJIX
                       IF      NACCT-DAY(IDX-SHOKUJI9)  NOT =   ZERO
                            MOVE   NACCT-DAY(IDX-SHOKUJI9) TO
                                                  WRK-TAIHI-HKN
                       END-IF
                   END-IF
      *
                   MOVE    "NYUINACCT-KEY16"   TO  WRK-DBPATH
                   PERFORM 900-TBL-READ-SEC
               END-PERFORM
           ELSE
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *    入院会計マスタクローズ
           MOVE    "NYUINACCT-KEY16"      TO  ORC-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
      *
      *    通算日数、算定開始日を求める
           PERFORM 4801-TSUSAN-SANTEI-GET-SEC
           IF    ( SPA-ERRCD    NOT =  SPACE )
               GO  TO  4801-TAIINKAIKEI-CANCEL-EXT
           END-IF
      *
           MOVE    WRK-TSUSAN      TO  SYOKIKSN-IN-TUSANNISSU
           MOVE    WRK-SANTEISTYMD
                                   TO  SYOKIKSN-IN-SANTEISTYMD
      *
      *    特定集中治療室管理料算定時に特定入院での算定日が１４日を
      *    経過していた場合は一般の入院料算定とする
      *    以下の特定入院料も同様
      *    救命救急入院料
      *    一類感染症患者入院医療管理料
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "03" OR "04" OR "05" OR
                                                 "06" OR "07" OR "08" OR
                                                 "09" OR "14" OR "16"
                   IF   STUSAN-NISSU06   >=   14
                       MOVE    SPACE   TO   SPA-GMN-I01-TOKNYUIN-CD
                   END-IF
               END-IF
           END-IF
      *
      *    広範囲熱傷特定集中治療室管理料算定時に特定入院での算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "13"
                   IF   STUSAN-NISSU06   >=   60
                       MOVE    SPACE   TO   SPA-GMN-I01-TOKNYUIN-CD
                   END-IF
               END-IF
           END-IF
      *
      *    ハイケアユニット入院医療管理料算定時に特定入院での算定日が２１日を
      *    経過していた場合は一般の入院料算定とする
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "17"
                   IF   STUSAN-NISSU06   >=   21
                       MOVE    SPACE   TO   SPA-GMN-I01-TOKNYUIN-CD
                   END-IF
               END-IF
           END-IF
      *
      *    亜急性期入院医療管理料算定時に特定入院での算定日が９０日を
      *    経過していた場合は一般の入院料算定とする
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "18"
                   IF   STUSAN-NISSU06   >=   90
                       MOVE    SPACE   TO   SPA-GMN-I01-TOKNYUIN-CD
                   END-IF
               END-IF
           END-IF
      *
      *    脳卒中ケアユニット入院医療管理料算定時に特定入院での算定日が１４日を
      *    経過していた場合は一般の入院料算定とする
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "19"
                   IF   STUSAN-NISSU06   >=   14
                       MOVE    SPACE   TO   SPA-GMN-I01-TOKNYUIN-CD
                   END-IF
               END-IF
           END-IF
      *
           PERFORM  4801-BTUSEL-SEC
           MOVE     SYS-5001-BTU-KHNSRYCD
                          TO    SYOKIKSN-IN-KHNSRYCD
      *
      *
           IF      SPA-GMN-I01-TOKNYUIN-CD    NOT =   SPACE
               PERFORM   4801-TOKTEI-NYUIN-SET-SEC
           ELSE
               IF  SYS-5001-BTU-KHNSRYCD      NOT =   SPACE
                   MOVE    SYS-5001-BTU-KHNSRYCD
                                       TO  SYOKIKSN-IN-KHNSRYCD
                   PERFORM   4801-RJNCD-CHG-SEC
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "4019"      TO  SPA-ERRCD
                       GO  TO  4801-TAIINKAIKEI-CANCEL-EXT
                   END-IF
               ELSE
                   MOVE    "4020"      TO  SPA-ERRCD
                   GO  TO  4801-TAIINKAIKEI-CANCEL-EXT
               END-IF
           END-IF
      *
      *
      *    入院会計作成サブの呼出し
           INITIALIZE                      ORCSNYUACCT-AREA
           MOVE     "7"                TO  LNK-SCNYUACCT-SYORI-KBN
           MOVE     "1"                TO  SPA-NYUGAIKBN
           MOVE     PTNYUINRRK-PTID    TO  SPA-PTID
           MOVE     PTNYUINRRK-NYUINKA TO  SPA-SRYKA
           MOVE     SPA-NAI-I01-NYUINYMD
                                       TO   LNK-SCNYUACCT-STSRYYMD
           MOVE     SPA-NAI-I01-TAIINYMD
                                       TO   LNK-SCNYUACCT-EDSRYYMD
           MOVE    SYOKIKSN-IN-KHNSRYCD
                            TO   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *退院日からの会計再作成に変更 ---------------------------------
      **     MOVE    SYOKIKSN-IN-SANTEISTYMD
      **                      TO   LNK-SCNYUACCT-STSRYYMD
           MOVE     SPA-NAI-I01-TAIINYMD
                                       TO   LNK-SCNYUACCT-STSRYYMD
      *--------------------------------------------------------------
      *
           PERFORM   VARYING   IDX6   FROM   1   BY   1
               UNTIL (SYOKIKSN-OT-KGNSRYCD(IDX6)  =  SPACE) OR
                     (IDX6   >     7)
               MOVE   SYOKIKSN-OT-KGNSRYCD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOSTYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOEDYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX6)
           END-PERFORM
      *
      *    通算算定済日数
           MOVE   WRK-TSUSAN    TO  LNK-SCNYUACCT-NYUKHN-NISUU
      *
      *    特定入院で入院料が変更になるコードの対応
      *    診療所老人医療管理料
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190774110"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190774110"  TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   SPA-NAI-I01-NYUINYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-NYUINYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    13                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *
                 MOVE   "190774210"  TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(2)
                 MOVE    "99999999"
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
           END-IF
      *    老人性痴呆疾患治療病棟入院料
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190739910"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190739910"  TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   SPA-NAI-I01-NYUINYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-NYUINYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    88                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *
                 MOVE   "190740010"  TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(2)
                 MOVE    "99999999"
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
           END-IF
      *
      *    老人性痴呆疾患治療病棟入院料２
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190813410"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190813410"  TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   SPA-NAI-I01-NYUINYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-NYUINYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    88                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *
                 MOVE   "190813510"  TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(2)
                 MOVE    "99999999"
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
           END-IF
      *
      *    特定入院料で７日まで、１４日まで、それ以降は一般入院料を算定する
      *    場合の診療コードを取得
           PERFORM    4801-TOKUTEI-NYUIN-7-14-SEC
      *
           MOVE      ZERO               TO  TN-IDX
           IF    SYOKIKSN-IN-KHNSRYCD   =   WRK-TOKUTEI-SRYCD1
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          特定集中治療室管理料の算定可能日数を計算
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                                         WK-TOKUTEI-DAY2
                 COMPUTE  WK-TOKUTEI-DAY  =  14  - STUSAN-NISSU06
                 IF       STUSAN-NISSU07  <  7
                     ADD     1        TO     TN-IDX
                     COMPUTE  WK-TOKUTEI-DAY2 =  6   -  STUSAN-NISSU07
                     MOVE   WRK-TOKUTEI-SRYCD1  TO
                                    LNK-SCNYUACCT-KAGEN-SRYCD(TN-IDX)
                     MOVE   SPA-NAI-I01-IDOYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   SPA-NAI-I01-IDOYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE   WK-TOKUTEI-DAY2    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX)
                 END-IF
      *
                 IF     WK-TOKUTEI-DAY   >=   1
                     ADD     1     TO   TN-IDX
                     MOVE   WRK-TOKUTEI-SRYCD2  TO 
                                    LNK-SCNYUACCT-KAGEN-SRYCD(TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     IF    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1) NOT = SPACE
                         MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                               TO  LNK-DAY6-KIJUN
                         MOVE    "1"           TO  LNK-DAY6-ZOGENPTN
                         MOVE     1            TO  LNK-DAY6-ZOGEN
                         CALL  "ORCSDAY"       USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                         MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     ELSE
                         MOVE  SPA-NAI-I01-IDOYMD   TO  LNK-DAY6-KIJUN
                         MOVE  SPA-NAI-I01-IDOYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     END-IF
                     MOVE    "99999999"        TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX)
                 END-IF
      *          一般入院料の算定開始日を算定
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-IDOYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          一般入院会計作成用のフラグ設定
                 MOVE    "1"      TO      IPN-NYUKAIKEI-SAKUSEI-FLG
           END-IF
      *
      *    広範囲熱傷特定集中治療室管理料
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190024810"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          広範囲熱傷特定集中治療室管理料の算定可能日数を計算
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                                         WK-TOKUTEI-DAY2
                 COMPUTE  WK-TOKUTEI-DAY  =  60  - STUSAN-NISSU06
                 IF       STUSAN-NISSU07  <  60
                     MOVE   "190024810"  TO
                                    LNK-SCNYUACCT-KAGEN-SRYCD(1)
                     MOVE   SPA-NAI-I01-IDOYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   SPA-NAI-I01-IDOYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
      *               MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                     COMPUTE  LNK-DAY6-ZOGEN = WK-TOKUTEI-DAY  -  1
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                 END-IF
      *          一般入院料の算定開始日を算定
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-IDOYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          特定入院期間（６０日経過）した際の一般入院料作成の
      *          可否を判定
                 MOVE    SPA-NAI-I01-IDOYMD
                                  TO      WRK-IPN-HANTEI-YMD1
                 IF      WRK-IPN-HANTEI-M1   =   12
                     ADD    1     TO      WRK-IPN-HANTEI-Y1
                     MOVE   1     TO      WRK-IPN-HANTEI-M1
                 ELSE
                     ADD    1     TO      WRK-IPN-HANTEI-M1
                 END-IF
      *          月末日を算定
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY7-AREA
                 MOVE    "71"              TO  LNK-DAY7-IRAI
                 MOVE   WRK-IPN-HANTEI-YM1 TO  LNK-DAY7-YM
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
                 MOVE    LNK-DAY7-KOYOMI   TO      WRK-IPN-HANTEI-YMD1
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                  TO      WRK-IPN-HANTEI-YMD2
      *          一般入院会計作成用のフラグ設定
                 IF      WRK-IPN-HANTEI-YMD1 > WRK-IPN-HANTEI-YMD2
                     MOVE    "1"   TO     IPN-NYUKAIKEI-SAKUSEI-FLG
                 END-IF
           END-IF
      *
      *    ハイケアユニット入院医療管理料（２１日を限度）
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190117310"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          ハイケアユニット入院医療管理料の算定可能日数を計算
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                 COMPUTE  WK-TOKUTEI-DAY  =  21  - STUSAN-NISSU06
      *
                 IF     WK-TOKUTEI-DAY   >=   1
                     MOVE   "190117310"  TO 
                                    LNK-SCNYUACCT-KAGEN-SRYCD(1)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE    SPA-NAI-I01-IDOYMD  TO
                                 LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                                 LNK-DAY6-KIJUN
                     MOVE    "1"           TO  LNK-DAY6-ZOGENPTN
                     MOVE     WK-TOKUTEI-DAY
                                           TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"       USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                 END-IF
      *          一般入院料の算定開始日を算定
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-IDOYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          一般入院会計作成用のフラグ設定
                 MOVE    "1"      TO      IPN-NYUKAIKEI-SAKUSEI-FLG
           END-IF
      *
      *    亜急性期入院医療管理料
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190117410"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          亜急性期入院医療管理料の算定可能日数を計算
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                                         WK-TOKUTEI-DAY2
                 COMPUTE  WK-TOKUTEI-DAY  =  90  - STUSAN-NISSU06
                 IF       STUSAN-NISSU07  <  90
                     MOVE   "190117410"  TO
                                    LNK-SCNYUACCT-KAGEN-SRYCD(1)
                     MOVE   SPA-NAI-I01-IDOYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   SPA-NAI-I01-IDOYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
      *               MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                     COMPUTE  LNK-DAY6-ZOGEN = WK-TOKUTEI-DAY  -  1
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                 END-IF
      *          一般入院料の算定開始日を算定
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-IDOYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          特定入院期間（６０日経過）した際の一般入院料作成の
      *          可否を判定
                 MOVE    SPA-NAI-I01-IDOYMD
                                  TO      WRK-IPN-HANTEI-YMD1
                 IF      WRK-IPN-HANTEI-M1   =   12
                     ADD    1     TO      WRK-IPN-HANTEI-Y1
                     MOVE   1     TO      WRK-IPN-HANTEI-M1
                 ELSE
                     ADD    1     TO      WRK-IPN-HANTEI-M1
                 END-IF
      *          月末日を算定
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY7-AREA
                 MOVE    "71"              TO  LNK-DAY7-IRAI
                 MOVE   WRK-IPN-HANTEI-YM1 TO  LNK-DAY7-YM
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
                 MOVE    LNK-DAY7-KOYOMI   TO      WRK-IPN-HANTEI-YMD1
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                  TO      WRK-IPN-HANTEI-YMD2
      *          一般入院会計作成用のフラグ設定
                 IF      WRK-IPN-HANTEI-YMD1 > WRK-IPN-HANTEI-YMD2
                     MOVE    "1"   TO     IPN-NYUKAIKEI-SAKUSEI-FLG
                 END-IF
           END-IF
      *
      *    脳卒中ケアユニット入院医療管理料（１４日を限度）
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190120810"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          脳卒中ケアユニット入院医療管理料の算定可能日数を計算
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                 COMPUTE  WK-TOKUTEI-DAY  =  14  - STUSAN-NISSU06
      *
                 IF     WK-TOKUTEI-DAY   >=   1
                     MOVE   "190120810"  TO 
                                    LNK-SCNYUACCT-KAGEN-SRYCD(1)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE    SPA-NAI-I01-IDOYMD  TO
                                 LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                                 LNK-DAY6-KIJUN
                     MOVE    "1"           TO  LNK-DAY6-ZOGENPTN
                     MOVE     WK-TOKUTEI-DAY
                                           TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"       USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                 END-IF
      *          一般入院料の算定開始日を算定
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-IDOYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          一般入院会計作成用のフラグ設定
                 MOVE    "1"      TO      IPN-NYUKAIKEI-SAKUSEI-FLG
           END-IF
      *
      *    精神科救急入院料
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190121010"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190121010" TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   SPA-NAI-I01-NYUINYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-NYUINYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    29                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *
                 MOVE   "190111410"    TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(2)
                 MOVE    "99999999"
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
           END-IF
      *
      *    精神科急性期治療病棟入院料１
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190121110"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190121110" TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   SPA-NAI-I01-NYUINYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-NYUINYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    29                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *
                 MOVE   "190069910"    TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(2)
                 MOVE    "99999999"
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
           END-IF
      *
      *    精神科急性期治療病棟入院料２
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190121210"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   "190121210" TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                 MOVE   SPA-NAI-I01-NYUINYMD
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-NYUINYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    29                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
      *
                 MOVE   "190067010"    TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE     1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(2)
                 MOVE    "99999999"
                                  TO LNK-SCNYUACCT-KAGEN-EDYUKYMD(2)
           END-IF
      *
      *-------特定入院対応ここまで-----------------------------------
      *
      *    入院料加算診療コード取り込み
           PERFORM    4801-NYUKSN-GET-SEC
      *
      *    特定入院料区分のセット
           IF      SPA-GMN-I01-TOKNYUIN-CD    =   SPACE
               MOVE     "0"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
           ELSE
               MOVE     "1"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
      *        老人特定入院基本料及び平成１８年７月以降の療養病棟、
      *        療養病床の場合は特定入院料扱いから外す
               IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410" OR
                                                       "190799510" OR
                                                       "190121310" OR
                                                       "190121410" OR
                                                       "190121510" OR
                                                       "190121610" OR
                                                       "190121710" OR
                                                       "190121810" OR
                                                       "190122010" OR
                                                       "190122110" OR
                                                       "190122210" OR
                                                       "190122310" OR
                                                       "190122410" OR
                                                       "190121510"
                   MOVE  "0"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
               END-IF
           END-IF
      *    外泊
           MOVE   "099999911"   TO  LNK-SCNYUACCT-GAIHAKU-SRYCD
      *    室料差額
           MOVE   "099999912"   TO  LNK-SCNYUACCT-SAGAKU-SRYCD
           IF     PTNYUINRRK-RM-SAGAKU   =   SPACE
               MOVE    ZERO     TO  LNK-SCNYUACCT-SAGAKU-KBN
           ELSE
               MOVE   PTNYUINRRK-RM-SAGAKU 
                                TO  WRK-SAGAKU-KBN
               MOVE   WRK-SAGAKU-KBN9
                                TO  LNK-SCNYUACCT-SAGAKU-KBN
           END-IF
      *    食事区分
           MOVE   "099999913"   TO  LNK-SCNYUACCT-SHOKUJI-SRYCD
           IF     WRK-TAIHI-SHOKUJI    NOT =   ZERO
                  MOVE   WRK-TAIHI-SHOKUJI
                                TO  LNK-SCNYUACCT-SHOKUJIKBN
           ELSE
                  MOVE   1      TO  LNK-SCNYUACCT-SHOKUJIKBN
           END-IF
      *    食堂加算
           PERFORM     4801-BTUSEL-SEC
           IF   FLG-SYSKANRI           =    ZERO
               IF   SYS-5001-BTU-SYOKUDOKBN       =    "2"
                    MOVE     "197000570"    TO
                                LNK-SCNYUACCT-SHOKUDO-SRYCD
                    MOVE    1   TO   LNK-SCNYUACCT-SHOKUDO-KBN
               END-IF
           END-IF
      *    保険組合せ
           MOVE   "099999914"   TO  LNK-SCNYUACCT-HKNCOMBINUM-SRYCD
           IF     WRK-TAIHI-HKN    NOT =   ZERO
                  MOVE   WRK-TAIHI-HKN
                                TO  LNK-SCNYUACCT-HKNCOMBINUM
           ELSE
                  MOVE   PTNYUINRRK-HKNCOMBINUM
                                TO  WRK-HKNCOMBINUM
                  MOVE   WRK-HKNCOMBINUM9 
                                TO  LNK-SCNYUACCT-HKNCOMBINUM
           END-IF
           MOVE   SPA-SRYKA     TO  WRK-TAIHI-SRYKA
           MOVE   "00"          TO  SPA-SRYKA
           PERFORM 4801-SENTEI-SET-SEC
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
                  DISPLAY  "ORCGI01 ORCSNYUACCT ERR RC = "
                            LNK-SCNYUACCT-RC
           END-IF
      *
           IF     IPN-NYUKAIKEI-SAKUSEI-FLG    =   "1"
               MOVE    SPACE   TO   SPA-GMN-I01-TOKNYUIN-CD
               MOVE    IPN-SANTEISTYMD   TO   WRK-SANTEISTYMD
               MOVE    IPN-SANTEISTYMD   TO   SPA-NAI-I01-IDOYMD
               MOVE    IPN-SANTEISTYMD   TO   PTNYUINRRK-TENNYUYMD
               PERFORM   4801-TENROOM-EDIT-SEC
           END-IF
      *
           .
       4801-TAIINKAIKEI-CANCEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    転科／転棟／転室時　会計テーブル編集処理
      *****************************************************************
       4801-TENROOM-EDIT-SEC         SECTION.
      *
           MOVE      SPACE        TO    WRK-DOU-TENNYUYMD
           MOVE      SPACE        TO    IPN-NYUKAIKEI-SAKUSEI-FLG
      *
           INITIALIZE      ORCSSYOKIKSNAREA
      *
      *    通算日数、算定開始日を求める
           PERFORM 4801-TSUSAN-SANTEI-GET-SEC
           IF    ( SPA-ERRCD    NOT =  SPACE )
               GO  TO  4801-TENROOM-EDIT-EXT
           END-IF
      *
           MOVE    WRK-TSUSAN      TO  SYOKIKSN-IN-TUSANNISSU
           MOVE    WRK-SANTEISTYMD
                                   TO  SYOKIKSN-IN-SANTEISTYMD
      *
      *    特定集中治療室管理料算定時に特定入院での算定日が１４日を
      *    経過していた場合は一般の入院料算定とする
      *    以下の特定入院料も同様
      *    救命救急入院料
      *    一類感染症患者入院医療管理料
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "03" OR "04" OR "05" OR
                                                 "06" OR "07" OR "08" OR
                                                 "09" OR "14" OR "16"
                   IF   STUSAN-NISSU06   >=   14
                       MOVE    SPACE   TO   SPA-GMN-I01-TOKNYUIN-CD
                   END-IF
               END-IF
           END-IF
      *
      *    広範囲熱傷特定集中治療室管理料算定時に特定入院での算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "13"
                   IF   STUSAN-NISSU06   >=   60
                       MOVE    SPACE   TO   SPA-GMN-I01-TOKNYUIN-CD
                   END-IF
               END-IF
           END-IF
      *
      *    ハイケアユニット入院医療管理料算定時に特定入院での算定日が２１日を
      *    経過していた場合は一般の入院料算定とする
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "17"
                   IF   STUSAN-NISSU06   >=   21
                       MOVE    SPACE   TO   SPA-GMN-I01-TOKNYUIN-CD
                   END-IF
               END-IF
           END-IF
      *
      *    亜急性期入院医療管理料算定時に特定入院での算定日が９０日を
      *    経過していた場合は一般の入院料算定とする
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "18"
                   IF   STUSAN-NISSU06   >=   90
                       MOVE    SPACE   TO   SPA-GMN-I01-TOKNYUIN-CD
                   END-IF
               END-IF
           END-IF
      *
      *    脳卒中ケアユニット入院医療管理料算定時に特定入院での算定日が１４日を
      *    経過していた場合は一般の入院料算定とする
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "19"
                   IF   STUSAN-NISSU06   >=   14
                       MOVE    SPACE   TO   SPA-GMN-I01-TOKNYUIN-CD
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-GMN-I01-TOKNYUIN-CD    NOT =   SPACE
               PERFORM   4801-TOKTEI-NYUIN-SET-SEC
           ELSE
               IF  SYS-5001-BTU-KHNSRYCD      NOT =   SPACE
                   MOVE    SYS-5001-BTU-KHNSRYCD
                                       TO  SYOKIKSN-IN-KHNSRYCD
                   PERFORM   4801-RJNCD-CHG-SEC
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "4021"      TO  SPA-ERRCD
                       GO  TO  4801-TENROOM-EDIT-EXT
                   END-IF
               ELSE
                   MOVE    "4022"      TO  SPA-ERRCD
                   GO  TO  4801-TENROOM-EDIT-EXT
               END-IF
           END-IF
      *
      *    入院会計作成前に会計データの食事区分を退避
           IF  SPA-NAI-I01-IDOYMD(1:6)  >= "200604"
      *      （異動月分）
             PERFORM   VARYING   IDX-SKJ-TAIHI  FROM  1 BY  1
               UNTIL   IDX-SKJ-TAIHI  >    3
               INITIALIZE                      NYUINACCT-REC
               MOVE    PTNYUINRRK-HOSPID   TO  NACCT-HOSPID
               MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    SPA-NAI-I01-IDOYMD(1:6)
                                           TO  NACCT-SRYYM
                                               WRK-TENROOM-SKJ-SRYYMX
      *
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM  910-NYUINACCT-SELECT-SEC
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM  910-NYUINACCT-READ-SEC
               IF      FLG-NYUINACCT        =   ZERO
                 MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *          食事を退避
                 MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(1)
                 MOVE    SPA-NAI-I01-IDOYMD(7:2)
                                         TO  IDX-SHOKUJIX
                 EVALUATE  IDX-SKJ-TAIHI
                     WHEN  1
                        PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   NACCT-DAY(IDX-SKJ)
                                         TO WRK-TENROOM-SKJ18(1 IDX-SKJ)
                        END-PERFORM
                     WHEN  2
                        PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   NACCT-DAY(IDX-SKJ)
                                         TO WRK-TENROOM-SKJ19(1 IDX-SKJ)
                        END-PERFORM
                     WHEN  3
                        PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   NACCT-DAY(IDX-SKJ)
                                         TO WRK-TENROOM-SKJ20(1 IDX-SKJ)
                        END-PERFORM
                 END-EVALUATE
               ELSE
                 MOVE    SPA-NAI-I01-IDOYMD(1:6)
                                         TO  WRK-TENROOM-SKJ-SRYYM(1)
                 MOVE    SPA-NAI-I01-IDOYMD(7:2)
                                         TO  IDX-SHOKUJIX
                 EVALUATE  IDX-SKJ-TAIHI
                     WHEN  1
                        PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   ZERO
                                         TO WRK-TENROOM-SKJ18(1 IDX-SKJ)
                        END-PERFORM
                     WHEN  2
                        PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   ZERO
                                         TO WRK-TENROOM-SKJ19(1 IDX-SKJ)
                        END-PERFORM
                     WHEN  3
                        PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   ZERO
                                         TO WRK-TENROOM-SKJ20(1 IDX-SKJ)
                        END-PERFORM
                 END-EVALUATE
                 MOVE    SPACE           TO  WRK-TENROOM-SKJ-SRYYM(1)
               END-IF
      *        入院会計マスタクローズ
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM 900-TBL-CLOSE2-SEC
             END-PERFORM
      *      （異動翌月分）
             PERFORM   VARYING   IDX-SKJ-TAIHI  FROM  1 BY  1
                UNTIL   IDX-SKJ-TAIHI  >    3
               INITIALIZE                      NYUINACCT-REC
               MOVE    PTNYUINRRK-HOSPID   TO  NACCT-HOSPID
               MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
               IF   IDX-SKJ-TAIHI     =    1
                   IF      WRK-TENROOM-SKJ-SRYMM9  =  12
                       ADD   1     TO   WRK-TENROOM-SKJ-SRYYY9
                       MOVE  1     TO   WRK-TENROOM-SKJ-SRYMM9
                   ELSE
                       ADD   1     TO   WRK-TENROOM-SKJ-SRYMM9
                   END-IF
               END-IF
               MOVE    WRK-TENROOM-SKJ-SRYYMX
                                           TO  NACCT-SRYYM
      *
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM  910-NYUINACCT-SELECT-SEC
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM  910-NYUINACCT-READ-SEC
               IF      FLG-NYUINACCT        =   ZERO
                 MOVE     MCPDATA-REC      TO  NYUINACCT-REC
      *          食事を退避
                 MOVE    NACCT-SRYYM       TO  WRK-TENROOM-SKJ-SRYYM(2)
                 EVALUATE  IDX-SKJ-TAIHI
                     WHEN  1
                       PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                            MOVE   NACCT-DAY(IDX-SKJ)
                                        TO  WRK-TENROOM-SKJ18(2 IDX-SKJ)
                       END-PERFORM
                     WHEN  2
                       PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                            MOVE   NACCT-DAY(IDX-SKJ)
                                        TO  WRK-TENROOM-SKJ19(2 IDX-SKJ)
                       END-PERFORM
                     WHEN  3
                       PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                            MOVE   NACCT-DAY(IDX-SKJ)
                                        TO  WRK-TENROOM-SKJ20(2 IDX-SKJ)
                       END-PERFORM
                 END-EVALUATE
               ELSE
                   MOVE    SPACE           TO  WRK-TENROOM-SKJ-SRYYM(2)
               END-IF
      *        入院会計マスタクローズ
               MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
               EVALUATE   IDX-SKJ-TAIHI
                   WHEN    1
                       MOVE    "key30"     TO  MCP-PATHNAME
                   WHEN    2
                       MOVE    "key31"     TO  MCP-PATHNAME
                   WHEN    3
                       MOVE    "key32"     TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM 900-TBL-CLOSE2-SEC
             END-PERFORM
           ELSE
      *      （異動月分）
             INITIALIZE                      NYUINACCT-REC
             MOVE    PTNYUINRRK-HOSPID   TO  NACCT-HOSPID
             MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
             MOVE    SPA-NAI-I01-IDOYMD(1:6)
                                         TO  NACCT-SRYYM
                                             WRK-TENROOM-SKJ-SRYYMX
      *
             MOVE    NYUINACCT-REC       TO  MCPDATA-REC
             MOVE    "NYUINACCT-KEY28"   TO  WRK-DBPATH
             PERFORM   900-TBL-SELECT-SEC
             IF      FLG-DBRC            =   ZERO
                 MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *          食事を退避
                 MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(1)
                 MOVE    SPA-NAI-I01-IDOYMD(7:2)
                                         TO  IDX-SHOKUJIX
                 PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                           UNTIL     IDX-SKJ   >   31
                      MOVE   NACCT-DAY(IDX-SKJ)
                                         TO  WRK-TENROOM-SKJ(1 IDX-SKJ)
                 END-PERFORM
             ELSE
                 MOVE    SPA-NAI-I01-IDOYMD(1:6)
                                         TO  WRK-TENROOM-SKJ-SRYYM(1)
                 MOVE    SPA-NAI-I01-IDOYMD(7:2)
                                         TO  IDX-SHOKUJIX
                 PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                           UNTIL     IDX-SKJ   >   31
                      MOVE   ZERO        TO  WRK-TENROOM-SKJ(1 IDX-SKJ)
                 END-PERFORM
             END-IF
      *      入院会計マスタクローズ
             MOVE    "NYUINACCT-KEY28"      TO  ORC-DBPATH
             PERFORM 900-TBL-CLOSE-SEC
      *      （異動翌月分）
             INITIALIZE                      NYUINACCT-REC
             MOVE    PTNYUINRRK-HOSPID   TO  NACCT-HOSPID
             MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
             IF      WRK-TENROOM-SKJ-SRYMM9  =  12
                 ADD   1     TO   WRK-TENROOM-SKJ-SRYYY9
                 MOVE  1     TO   WRK-TENROOM-SKJ-SRYMM9
             ELSE
                 ADD   1     TO   WRK-TENROOM-SKJ-SRYMM9
             END-IF
             MOVE    WRK-TENROOM-SKJ-SRYYMX
                                         TO  NACCT-SRYYM
      *
             MOVE    NYUINACCT-REC       TO  MCPDATA-REC
             MOVE    "NYUINACCT-KEY28"   TO  WRK-DBPATH
             PERFORM   900-TBL-SELECT-SEC
             IF      FLG-DBRC            =   ZERO
                 MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *          食事を退避
                 MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(2)
                 PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                           UNTIL     IDX-SKJ   >   31
                      MOVE   NACCT-DAY(IDX-SKJ)
                                         TO  WRK-TENROOM-SKJ(2 IDX-SKJ)
                 END-PERFORM
             ELSE
                 MOVE    SPACE           TO  WRK-TENROOM-SKJ-SRYYM(2)
             END-IF
      *      入院会計マスタクローズ
             MOVE    "NYUINACCT-KEY28"      TO  ORC-DBPATH
             PERFORM 900-TBL-CLOSE-SEC
           END-IF
      *
      *    入院会計作成サブの呼出し
           INITIALIZE                     ORCSNYUACCT-AREA
           MOVE    "8"                TO  LNK-SCNYUACCT-SYORI-KBN
           MOVE    "1"                TO  SPA-NYUGAIKBN
           MOVE    PTNYUINRRK-PTID    TO  SPA-PTID
           MOVE    PTNYUINRRK-NYUINKA TO  SPA-SRYKA
           MOVE    SYOKIKSN-IN-KHNSRYCD
                           TO  LNK-SCNYUACCT-NYUKHN-SRYCD(1)
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                           TO   LNK-SCNYUACCT-STSRYYMD
           MOVE    SPA-NAI-I01-IDOYMD
                           TO   LNK-SCNYUACCT-EDSRYYMD
           PERFORM   VARYING   IDX6   FROM   1   BY   1
               UNTIL (SYOKIKSN-OT-KGNSRYCD(IDX6)  =  SPACE) OR
                     (IDX6   >     7)
               MOVE   SYOKIKSN-OT-KGNSRYCD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOSTYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOEDYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX6)
           END-PERFORM
      *    通算算定済日数
           MOVE   WRK-TSUSAN    TO  LNK-SCNYUACCT-NYUKHN-NISUU
      *
      *    特定入院で入院料が変更になるコードの対応
      *    診療所老人医療管理料
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190774110"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU01      <=  14
                     ADD    1           TO  TN-IDX
                     MOVE   "190774110" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              診療所老人医療管理料の１４日以内の終日を再計算
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  14  - STUSAN-NISSU01
                     MOVE   SPA-NAI-I01-IDOYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   SPA-NAI-I01-IDOYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190774210"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     MOVE    SPA-NAI-I01-IDOYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
           END-IF
      *
      *    老人性痴呆疾患治療病棟入院料
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190739910"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU01      <=  90
                     ADD    1           TO  TN-IDX
                     MOVE   "190739910" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              老人性痴呆疾患治療病棟入院料の９０日以内の終日を再計算
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  90  - STUSAN-NISSU01
                     MOVE   SPA-NAI-I01-IDOYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   SPA-NAI-I01-IDOYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190740010"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     MOVE    SPA-NAI-I01-IDOYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
           END-IF
      *
      *    老人性痴呆疾患治療病棟入院料２
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190813410"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU01      <=  90
                     ADD    1           TO  TN-IDX
                     MOVE   "190813410" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              老人性痴呆疾患治療病棟入院料の９０日以内の終日を再計算
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  90  - STUSAN-NISSU01
                     MOVE   SPA-NAI-I01-IDOYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   SPA-NAI-I01-IDOYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190813510"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     MOVE    SPA-NAI-I01-IDOYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
           END-IF
      *
      *    特定入院料で７日まで、１４日まで、それ以降は一般入院料を算定する
      *    場合の診療コードを取得
           PERFORM    4801-TOKUTEI-NYUIN-7-14-SEC
      *
           MOVE      ZERO               TO  TN-IDX
           IF    SYOKIKSN-IN-KHNSRYCD   =   WRK-TOKUTEI-SRYCD1
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          特定集中治療室管理料の算定可能日数を計算
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                                         WK-TOKUTEI-DAY2
                 COMPUTE  WK-TOKUTEI-DAY  =  14  - STUSAN-NISSU06
                 IF       STUSAN-NISSU07  <  7
                     ADD     1        TO     TN-IDX
                     COMPUTE  WK-TOKUTEI-DAY2 =  6   -  STUSAN-NISSU07
                     MOVE   WRK-TOKUTEI-SRYCD1  TO
                                    LNK-SCNYUACCT-KAGEN-SRYCD(TN-IDX)
                     MOVE   SPA-NAI-I01-IDOYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   SPA-NAI-I01-IDOYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE   WK-TOKUTEI-DAY2    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX)
                 END-IF
      *
                 IF     WK-TOKUTEI-DAY   >=   1
                     ADD     1     TO   TN-IDX
                     MOVE   WRK-TOKUTEI-SRYCD2  TO 
                                    LNK-SCNYUACCT-KAGEN-SRYCD(TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     IF    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1) NOT = SPACE
                         MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                               TO  LNK-DAY6-KIJUN
                         MOVE    "1"           TO  LNK-DAY6-ZOGENPTN
                         MOVE     1            TO  LNK-DAY6-ZOGEN
                         CALL  "ORCSDAY"       USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                         MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     ELSE
                         MOVE  SPA-NAI-I01-IDOYMD   TO  LNK-DAY6-KIJUN
                         MOVE  SPA-NAI-I01-IDOYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     END-IF
                     MOVE    "99999999"        TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX)
                 END-IF
      *          一般入院料の算定開始日を算定
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-IDOYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          一般入院会計作成用のフラグ設定
                 MOVE    "1"      TO      IPN-NYUKAIKEI-SAKUSEI-FLG
           END-IF
      *
      *    広範囲熱傷特定集中治療室管理料
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190024810"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          広範囲熱傷特定集中治療室管理料の算定可能日数を計算
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                 COMPUTE  WK-TOKUTEI-DAY  =  60  - STUSAN-NISSU06
                 IF       STUSAN-NISSU07  <  60
                     MOVE   "190024810"  TO
                                    LNK-SCNYUACCT-KAGEN-SRYCD(1)
                     MOVE   SPA-NAI-I01-IDOYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   SPA-NAI-I01-IDOYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
      *               MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                     COMPUTE  LNK-DAY6-ZOGEN = WK-TOKUTEI-DAY  -  1
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                 END-IF
      *          一般入院料の算定開始日を算定
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-IDOYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          特定入院期間（６０日経過）した際の一般入院料作成の
      *          可否を判定
                 MOVE    SPA-NAI-I01-IDOYMD
                                  TO      WRK-IPN-HANTEI-YMD1
                 IF      WRK-IPN-HANTEI-M1   =   12
                     ADD    1     TO      WRK-IPN-HANTEI-Y1
                     MOVE   1     TO      WRK-IPN-HANTEI-M1
                 ELSE
                     ADD    1     TO      WRK-IPN-HANTEI-M1
                 END-IF
      *          月末日を算定
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY7-AREA
                 MOVE    "71"              TO  LNK-DAY7-IRAI
                 MOVE   WRK-IPN-HANTEI-YM1 TO  LNK-DAY7-YM
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
                 MOVE    LNK-DAY7-KOYOMI   TO      WRK-IPN-HANTEI-YMD1
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                  TO      WRK-IPN-HANTEI-YMD2
      *          一般入院会計作成用のフラグ設定
                 IF      WRK-IPN-HANTEI-YMD1 > WRK-IPN-HANTEI-YMD2
                     MOVE    "1"   TO     IPN-NYUKAIKEI-SAKUSEI-FLG
                 END-IF
           END-IF
      *
      *    ハイケアユニット入院医療管理料（２１日を限度）
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190117310"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          ハイケアユニット入院医療管理料の算定可能日数を計算
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                 COMPUTE  WK-TOKUTEI-DAY  =  21  - STUSAN-NISSU06
      *
                 IF     WK-TOKUTEI-DAY   >=   1
                     MOVE   "190117310"  TO 
                                    LNK-SCNYUACCT-KAGEN-SRYCD(1)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE    SPA-NAI-I01-IDOYMD  TO
                                 LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                                 LNK-DAY6-KIJUN
                     MOVE    "1"           TO  LNK-DAY6-ZOGENPTN
                     MOVE     WK-TOKUTEI-DAY
                                           TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"       USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                 END-IF
      *          一般入院料の算定開始日を算定
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-IDOYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          一般入院会計作成用のフラグ設定
                 MOVE    "1"      TO      IPN-NYUKAIKEI-SAKUSEI-FLG
           END-IF
      *
      *    亜急性期入院医療管理料（９０日を限度）
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190117410"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          亜急性期入院医療管理料の算定可能日数を計算
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                 COMPUTE  WK-TOKUTEI-DAY  =  90  - STUSAN-NISSU06
                 IF       STUSAN-NISSU07  <  90
                     MOVE   "190117410"  TO
                                    LNK-SCNYUACCT-KAGEN-SRYCD(1)
                     MOVE   SPA-NAI-I01-IDOYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   SPA-NAI-I01-IDOYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
      *               MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                     COMPUTE  LNK-DAY6-ZOGEN = WK-TOKUTEI-DAY  -  1
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                 END-IF
      *          一般入院料の算定開始日を算定
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-IDOYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          特定入院期間（６０日経過）した際の一般入院料作成の
      *          可否を判定
                 MOVE    SPA-NAI-I01-IDOYMD
                                  TO      WRK-IPN-HANTEI-YMD1
                 IF      WRK-IPN-HANTEI-M1   =   12
                     ADD    1     TO      WRK-IPN-HANTEI-Y1
                     MOVE   1     TO      WRK-IPN-HANTEI-M1
                 ELSE
                     ADD    1     TO      WRK-IPN-HANTEI-M1
                 END-IF
      *          月末日を算定
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY7-AREA
                 MOVE    "71"              TO  LNK-DAY7-IRAI
                 MOVE   WRK-IPN-HANTEI-YM1 TO  LNK-DAY7-YM
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
                 MOVE    LNK-DAY7-KOYOMI   TO      WRK-IPN-HANTEI-YMD1
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                  TO      WRK-IPN-HANTEI-YMD2
      *          一般入院会計作成用のフラグ設定
                 IF      WRK-IPN-HANTEI-YMD1 > WRK-IPN-HANTEI-YMD2
                     MOVE    "1"   TO     IPN-NYUKAIKEI-SAKUSEI-FLG
                 END-IF
           END-IF
      *
      *    脳卒中ケアユニット入院医療管理料（１４日を限度）
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190120810"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          脳卒中ケアユニット入院医療管理料の算定可能日数を計算
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                 COMPUTE  WK-TOKUTEI-DAY  =  14  - STUSAN-NISSU06
      *
                 IF     WK-TOKUTEI-DAY   >=   1
                     MOVE   "190120810"  TO 
                                    LNK-SCNYUACCT-KAGEN-SRYCD(1)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE    SPA-NAI-I01-IDOYMD  TO
                                 LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                                 LNK-DAY6-KIJUN
                     MOVE    "1"           TO  LNK-DAY6-ZOGENPTN
                     MOVE     WK-TOKUTEI-DAY
                                           TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"       USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                 END-IF
      *          一般入院料の算定開始日を算定
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-IDOYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          一般入院会計作成用のフラグ設定
                 MOVE    "1"      TO      IPN-NYUKAIKEI-SAKUSEI-FLG
           END-IF
      *
      *    精神科救急入院料
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190121010"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU01      <=  30
                     ADD    1           TO  TN-IDX
                     MOVE   "190121010" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              精神科救急入院料の３０日以内の終日を再計算
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  30  - STUSAN-NISSU01
                     MOVE   SPA-NAI-I01-IDOYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   SPA-NAI-I01-IDOYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190111410"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     MOVE    SPA-NAI-I01-IDOYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
           END-IF
      *
      *    精神科急性期治療病棟入院料１
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190121110"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU01      <=  30
                     ADD    1           TO  TN-IDX
                     MOVE   "190121110" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              精神科救急入院料の３０日以内の終日を再計算
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  30  - STUSAN-NISSU01
                     MOVE   SPA-NAI-I01-IDOYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   SPA-NAI-I01-IDOYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190066910"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     MOVE    SPA-NAI-I01-IDOYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
           END-IF
      *
      *    精神科急性期治療病棟入院料２
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190121210"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU01      <=  30
                     ADD    1           TO  TN-IDX
                     MOVE   "190121210" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              精神科救急入院料の３０日以内の終日を再計算
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  30  - STUSAN-NISSU01
                     MOVE   SPA-NAI-I01-IDOYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   SPA-NAI-I01-IDOYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190067010"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     MOVE    SPA-NAI-I01-IDOYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
           END-IF
      *
      *-------特定入院対応ここまで-----------------------------------
      *
      *    入院料加算診療コード取り込み
           PERFORM    4801-NYUKSN-GET-SEC
      *
      *    特定入院料区分のセット
           IF      SPA-GMN-I01-TOKNYUIN-CD    =   SPACE
               MOVE     "0"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
           ELSE
               MOVE     "1"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
      *        老人特定入院基本料及び平成１８年７月以降の療養病棟、
      *        療養病床の場合は特定入院料扱いから外す
               IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410" OR
                                                       "190799510" OR
                                                       "190121310" OR
                                                       "190121410" OR
                                                       "190121510" OR
                                                       "190121610" OR
                                                       "190121710" OR
                                                       "190121810" OR
                                                       "190122010" OR
                                                       "190122110" OR
                                                       "190122210" OR
                                                       "190122310" OR
                                                       "190122410" OR
                                                       "190121510"
                   MOVE  "0"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
               END-IF
           END-IF
      *    外泊
           MOVE   "099999911"   TO  LNK-SCNYUACCT-GAIHAKU-SRYCD
      *    室料差額
           MOVE   "099999912"   TO  LNK-SCNYUACCT-SAGAKU-SRYCD
           IF     PTNYUINRRK-RM-SAGAKU   =   SPACE
               MOVE    ZERO     TO  LNK-SCNYUACCT-SAGAKU-KBN
           ELSE
               MOVE   PTNYUINRRK-RM-SAGAKU 
                                TO  WRK-SAGAKU-KBN
               MOVE   WRK-SAGAKU-KBN9
                                TO  LNK-SCNYUACCT-SAGAKU-KBN
           END-IF
      *    食事区分
           MOVE   "099999913"   TO  LNK-SCNYUACCT-SHOKUJI-SRYCD
           IF     WRK-TAIHI-SHOKUJI    NOT =   ZERO
                  MOVE   WRK-TAIHI-SHOKUJI
                                TO  LNK-SCNYUACCT-SHOKUJIKBN
           ELSE
                  MOVE   1      TO  LNK-SCNYUACCT-SHOKUJIKBN
           END-IF
      *    食堂加算
           PERFORM     4801-BTUSEL-SEC
           IF   FLG-SYSKANRI           =    ZERO
               IF   SYS-5001-BTU-SYOKUDOKBN       =    "2"
                    MOVE     "197000570"    TO
                                LNK-SCNYUACCT-SHOKUDO-SRYCD
                    MOVE    1   TO   LNK-SCNYUACCT-SHOKUDO-KBN
               END-IF
           END-IF
      *    保険組合せ
           MOVE   "099999914"   TO  LNK-SCNYUACCT-HKNCOMBINUM-SRYCD
           MOVE   PTNYUINRRK-HKNCOMBINUM
                                TO  WRK-HKNCOMBINUM
           MOVE   WRK-HKNCOMBINUM9 
                                TO  LNK-SCNYUACCT-HKNCOMBINUM
      *    転科元入院科
           MOVE   "00"          TO  LNK-SCNYUACCT-MOTO-NYUINKA
      **     MOVE   SPA-GMN-I01-NYUINKA-MOT (1 : 2)
      **                          TO  LNK-SCNYUACCT-MOTO-NYUINKA
           MOVE   SPA-SRYKA     TO  WRK-TAIHI-SRYKA
           MOVE   "00"          TO  SPA-SRYKA
           PERFORM 4801-SENTEI-SET-SEC
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
               DISPLAY  "ORCGI01 ORCSNYUACCT ERR RC = "
                         LNK-SCNYUACCT-RC
           END-IF
      *
           IF     IPN-NYUKAIKEI-SAKUSEI-FLG    =   "1"
               MOVE    SPACE   TO   SPA-GMN-I01-TOKNYUIN-CD
               MOVE    IPN-SANTEISTYMD   TO   WRK-SANTEISTYMD
               MOVE    IPN-SANTEISTYMD   TO   SPA-NAI-I01-IDOYMD
               MOVE    IPN-SANTEISTYMD   TO   PTNYUINRRK-TENNYUYMD
           END-IF
      *
      *    入院会計作成後に退避データの食事区分を復元
           IF  SPA-NAI-I01-IDOYMD(1:6)  >= "200604"
      *       （異動月分）
               IF   WRK-TENROOM-SKJ-SRYYM(1)  NOT =   SPACE
                PERFORM    VARYING  IDX-SKJ-TAIHI  FROM  1  BY  1
                 UNTIL    IDX-SKJ-TAIHI   >   3
                 INITIALIZE                      NYUINACCT-REC
                 MOVE    PTNYUINRRK-HOSPID   TO  NACCT-HOSPID
                 MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
                 MOVE    WRK-TENROOM-SKJ-SRYYM(1)
                                             TO  NACCT-SRYYM
      *
                 EVALUATE   IDX-SKJ-TAIHI
                     WHEN    1
                         MOVE    "key30"     TO  MCP-PATHNAME
                     WHEN    2
                         MOVE    "key31"     TO  MCP-PATHNAME
                     WHEN    3
                         MOVE    "key32"     TO  MCP-PATHNAME
                 END-EVALUATE
                 PERFORM  910-NYUINACCT-SELECT-SEC
                 EVALUATE   IDX-SKJ-TAIHI
                     WHEN    1
                         MOVE    "key30"     TO  MCP-PATHNAME
                     WHEN    2
                         MOVE    "key31"     TO  MCP-PATHNAME
                     WHEN    3
                         MOVE    "key32"     TO  MCP-PATHNAME
                 END-EVALUATE
                 PERFORM  910-NYUINACCT-READ-SEC
                 IF      FLG-NYUINACCT        =   ZERO
                     MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *              食事を復元
                     MOVE    SPA-NAI-I01-IDOYMD(7:2)
                                         TO  IDX-SHOKUJIX
                     EVALUATE  IDX-SKJ-TAIHI
                         WHEN  1
                          PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY 1
                            UNTIL     IDX-SKJ   >   31
                               MOVE   WRK-TENROOM-SKJ18(1 IDX-SKJ)
                                             TO  NACCT-DAY(IDX-SKJ)
                          END-PERFORM
                         WHEN  2
                          PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY 1
                            UNTIL     IDX-SKJ   >   31
                               MOVE   WRK-TENROOM-SKJ19(1 IDX-SKJ)
                                             TO  NACCT-DAY(IDX-SKJ)
                          END-PERFORM
                         WHEN  3
                          PERFORM VARYING IDX-SKJ FROM IDX-SHOKUJI9 BY 1
                            UNTIL     IDX-SKJ   >   31
                               MOVE   WRK-TENROOM-SKJ20(1 IDX-SKJ)
                                             TO  NACCT-DAY(IDX-SKJ)
                          END-PERFORM
                     END-EVALUATE
      *              剤回数再セット
                     MOVE  ZERO              TO  NACCT-ZAIKAISU
                     PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                              UNTIL     IDX-SKJ   >   31
                         IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                             ADD   1        TO  NACCT-ZAIKAISU
                         END-IF
                     END-PERFORM   
                 END-IF
                 MOVE     NYUINACCT-REC      TO  MCPDATA-REC
                 MOVE     "DBUPDATE"         TO  MCP-FUNC
                 MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
                 MOVE     "key"              TO  MCP-PATH
                 CALL     "MONFUNC"          USING
                                             MCPAREA
                                             MCPDATA-REC
      *          入院会計マスタクローズ
                 MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                 EVALUATE   IDX-SKJ-TAIHI
                     WHEN    1
                         MOVE    "key30"     TO  MCP-PATHNAME
                     WHEN    2
                         MOVE    "key31"     TO  MCP-PATHNAME
                     WHEN    3
                         MOVE    "key32"     TO  MCP-PATHNAME
                 END-EVALUATE
                 PERFORM 900-TBL-CLOSE2-SEC
                END-PERFORM
               END-IF
      *
      *       （異動翌月分）
               IF   WRK-TENROOM-SKJ-SRYYM(2)  NOT =   SPACE
               PERFORM    VARYING  IDX-SKJ-TAIHI  FROM  1  BY  1
                 UNTIL    IDX-SKJ-TAIHI   >   3
                   INITIALIZE                      NYUINACCT-REC
                   MOVE    PTNYUINRRK-HOSPID   TO  NACCT-HOSPID
                   MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
                   MOVE    WRK-TENROOM-SKJ-SRYYM(2)
                                               TO  NACCT-SRYYM
      *
      *
                   EVALUATE   IDX-SKJ-TAIHI
                       WHEN    1
                           MOVE    "key30"     TO  MCP-PATHNAME
                       WHEN    2
                           MOVE    "key31"     TO  MCP-PATHNAME
                       WHEN    3
                           MOVE    "key32"     TO  MCP-PATHNAME
                   END-EVALUATE
                   PERFORM  910-NYUINACCT-SELECT-SEC
                   EVALUATE   IDX-SKJ-TAIHI
                       WHEN    1
                           MOVE    "key30"     TO  MCP-PATHNAME
                       WHEN    2
                           MOVE    "key31"     TO  MCP-PATHNAME
                       WHEN    3
                           MOVE    "key32"     TO  MCP-PATHNAME
                   END-EVALUATE
                   PERFORM  910-NYUINACCT-READ-SEC
                   IF      FLG-NYUINACCT        =   ZERO
                       MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *                食事を復元
                       EVALUATE  IDX-SKJ-TAIHI
                           WHEN  1
                             PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                UNTIL  IDX-SKJ   >   31
                                MOVE   WRK-TENROOM-SKJ18(2 IDX-SKJ)
                                               TO  NACCT-DAY(IDX-SKJ)
                             END-PERFORM
                           WHEN  2
                             PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                UNTIL  IDX-SKJ   >   31
                                MOVE   WRK-TENROOM-SKJ19(2 IDX-SKJ)
                                               TO  NACCT-DAY(IDX-SKJ)
                             END-PERFORM
                           WHEN  3
                             PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                UNTIL  IDX-SKJ   >   31
                                MOVE   WRK-TENROOM-SKJ20(2 IDX-SKJ)
                                               TO  NACCT-DAY(IDX-SKJ)
                             END-PERFORM
                       END-EVALUATE
      *                剤回数再セット
                       MOVE  ZERO              TO  NACCT-ZAIKAISU
                       PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                           IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                               ADD   1        TO  NACCT-ZAIKAISU
                           END-IF
                       END-PERFORM   
                   END-IF
                   MOVE     NYUINACCT-REC      TO  MCPDATA-REC
                   MOVE     "DBUPDATE"         TO  MCP-FUNC
                   MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
                   MOVE     "key"              TO  MCP-PATH
                   CALL     "MONFUNC"          USING
                                               MCPAREA
                                               MCPDATA-REC
      *            入院会計マスタクローズ
                   MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
                   EVALUATE   IDX-SKJ-TAIHI
                       WHEN    1
                           MOVE    "key30"     TO  MCP-PATHNAME
                       WHEN    2
                           MOVE    "key31"     TO  MCP-PATHNAME
                       WHEN    3
                           MOVE    "key32"     TO  MCP-PATHNAME
                   END-EVALUATE
                   PERFORM 900-TBL-CLOSE2-SEC
               END-PERFORM
               END-IF
           ELSE
      *       （異動月分）
               INITIALIZE                      NYUINACCT-REC
               MOVE    PTNYUINRRK-HOSPID   TO  NACCT-HOSPID
               MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    WRK-TENROOM-SKJ-SRYYM(1)
                                           TO  NACCT-SRYYM
      *
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "NYUINACCT-KEY28"   TO  WRK-DBPATH
               PERFORM   900-TBL-SELECT-SEC
               IF      FLG-DBRC            =   ZERO
                   MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *            食事を復元
                   MOVE    SPA-NAI-I01-IDOYMD(7:2)
                                       TO  IDX-SHOKUJIX
                   PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                             UNTIL     IDX-SKJ   >   31
                         MOVE   WRK-TENROOM-SKJ(1 IDX-SKJ)
                                           TO  NACCT-DAY(IDX-SKJ)
                   END-PERFORM
      *            剤回数再セット
                   MOVE  ZERO              TO  NACCT-ZAIKAISU
                   PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                            UNTIL     IDX-SKJ   >   31
                       IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                           ADD   1        TO  NACCT-ZAIKAISU
                       END-IF
                   END-PERFORM   
               END-IF
               MOVE     NYUINACCT-REC      TO  MCPDATA-REC
               MOVE     "DBUPDATE"         TO  MCP-FUNC
               MOVE     "NYUINACCT-KEY"    TO  ORC-DBPATH
               CALL     "ORCMCPSUB"        USING  MCPAREA
                                                  ORCMCPAREA
                                                  MCPDATA-REC 
      *        入院会計マスタクローズ
               MOVE    "NYUINACCT-KEY28"      TO  ORC-DBPATH
               PERFORM 900-TBL-CLOSE-SEC
      *
      *       （異動翌月分）
               IF   WRK-TENROOM-SKJ-SRYYM(2)  NOT =   SPACE
                   INITIALIZE                      NYUINACCT-REC
                   MOVE    PTNYUINRRK-HOSPID   TO  NACCT-HOSPID
                   MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
                   MOVE    WRK-TENROOM-SKJ-SRYYM(2)
                                               TO  NACCT-SRYYM
      *
                   MOVE    NYUINACCT-REC       TO  MCPDATA-REC
                   MOVE    "NYUINACCT-KEY28"   TO  WRK-DBPATH
                   PERFORM   900-TBL-SELECT-SEC
                   IF      FLG-DBRC            =   ZERO
                       MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *                食事を復元
                       PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                            MOVE   WRK-TENROOM-SKJ(2 IDX-SKJ)
                                               TO  NACCT-DAY(IDX-SKJ)
                       END-PERFORM
      *                剤回数再セット
                       MOVE  ZERO              TO  NACCT-ZAIKAISU
                       PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                                 UNTIL     IDX-SKJ   >   31
                           IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                               ADD   1        TO  NACCT-ZAIKAISU
                           END-IF
                       END-PERFORM   
                   END-IF
                   MOVE     NYUINACCT-REC      TO  MCPDATA-REC
                   MOVE     "DBUPDATE"         TO  MCP-FUNC
                   MOVE     "NYUINACCT-KEY"    TO  ORC-DBPATH
                   CALL     "ORCMCPSUB"        USING  MCPAREA
                                                      ORCMCPAREA
                                                      MCPDATA-REC 
      *            入院会計マスタクローズ
                   MOVE    "NYUINACCT-KEY28"      TO  ORC-DBPATH
                   PERFORM 900-TBL-CLOSE-SEC
               END-IF
           END-IF
      *
           .
       4801-TENROOM-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    異動取消時会計テーブル編集処理
      *****************************************************************
       4801-IDOKAIKEI-CANCEL-SEC       SECTION.
      *
           MOVE   SPACE                TO  WRK-DOU-TENNYUYMD
      *    入院会計作成サブの呼出し（異動分の会計削除）
           INITIALIZE            ORCSNYUACCT-AREA
           MOVE     "4"     TO   LNK-SCNYUACCT-SYORI-KBN
           MOVE     "1"                TO  SPA-NYUGAIKBN
           MOVE     PTNYUINRRK-PTID    TO  SPA-PTID
           MOVE     PTNYUINRRK-NYUINKA TO  SPA-SRYKA
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-I01-MAEIDOYMD
                                       TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                  TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  LNK-SCNYUACCT-EDSRYYMD
      *
      *    入院会計削除前に会計データの食事区分と保険組合せを退避
           INITIALIZE                      NYUINACCT-REC
           MOVE    PTNYUINRRK-HOSPID   TO  NACCT-HOSPID
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    "00"                TO  NACCT-SRYKA
           MOVE    LNK-SCNYUACCT-EDSRYYMD(1:6)
                                       TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "NYUINACCT-KEY16"   TO  WRK-DBPATH
           PERFORM   900-TBL-SELECT-SEC
           IF      FLG-DBRC            =   ZERO
               MOVE    ZERO        TO      WRK-TAIHI-SHOKUJI
               PERFORM UNTIL       FLG-DBRC     NOT =   ZERO
      *
                   MOVE     MCPDATA-REC   TO   NYUINACCT-REC
      *
      *            食事を退避
                   IF      NACCT-ZAISKBKBN     =   "4"
                       MOVE    LNK-SCNYUACCT-EDSRYYMD(7:2)
                                         TO    IDX-SHOKUJIX
                       IF      NACCT-DAY(IDX-SHOKUJI9)  NOT =   ZERO
                            MOVE   NACCT-DAY(IDX-SHOKUJI9) TO
                                                  WRK-TAIHI-SHOKUJI
                       END-IF
                   END-IF
      *            保険を退避
                   IF      NACCT-ZAISKBKBN     =   "5"
                       MOVE    LNK-SCNYUACCT-EDSRYYMD(7:2)
                                         TO    IDX-SHOKUJIX
                       IF      NACCT-DAY(IDX-SHOKUJI9)  NOT =   ZERO
                            MOVE   NACCT-DAY(IDX-SHOKUJI9) TO
                                                  WRK-TAIHI-HKN
                       END-IF
                   END-IF
      *
                   MOVE    "NYUINACCT-KEY16"   TO  WRK-DBPATH
                   PERFORM 900-TBL-READ-SEC
               END-PERFORM
           ELSE
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *    入院会計マスタクローズ
           MOVE    "NYUINACCT-KEY16"      TO  ORC-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
      *    入院会計作成前に会計データの食事区分を退避
      *    （異動月分）
           INITIALIZE                      NYUINACCT-REC
           MOVE    PTNYUINRRK-HOSPID   TO  NACCT-HOSPID
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    LNK-SCNYUACCT-EDSRYYMD(1:6)
                                       TO  NACCT-SRYYM
                                           WRK-TENROOM-SKJ-SRYYMX
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "NYUINACCT-KEY28"   TO  WRK-DBPATH
           PERFORM   900-TBL-SELECT-SEC
           IF      FLG-DBRC            =   ZERO
               MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *        食事を退避
               MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(1)
               MOVE    LNK-SCNYUACCT-EDSRYYMD(7:2)
                                       TO  IDX-SHOKUJIX
               MOVE    LNK-SCNYUACCT-EDSRYYMD(7:2)
                                       TO  WRK-TENROOM-SKJ-DD(1)
               PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                         UNTIL     IDX-SKJ   >   31
                    MOVE   NACCT-DAY(IDX-SKJ)
                                       TO  WRK-TENROOM-SKJ(1 IDX-SKJ)
               END-PERFORM
           END-IF
      *    入院会計マスタクローズ
           MOVE    "NYUINACCT-KEY28"      TO  ORC-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *    （異動翌月分）
           INITIALIZE                      NYUINACCT-REC
           MOVE    PTNYUINRRK-HOSPID   TO  NACCT-HOSPID
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           IF      WRK-TENROOM-SKJ-SRYMM9  =  12
               ADD   1     TO   WRK-TENROOM-SKJ-SRYYY9
               MOVE  1     TO   WRK-TENROOM-SKJ-SRYMM9
           ELSE
               ADD   1     TO   WRK-TENROOM-SKJ-SRYMM9
           END-IF
           MOVE    WRK-TENROOM-SKJ-SRYYMX
                                       TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "NYUINACCT-KEY28"   TO  WRK-DBPATH
           PERFORM   900-TBL-SELECT-SEC
           IF      FLG-DBRC            =   ZERO
               MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *        食事を退避
               MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(2)
               PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                         UNTIL     IDX-SKJ   >   31
                    MOVE   NACCT-DAY(IDX-SKJ)
                                       TO  WRK-TENROOM-SKJ(2 IDX-SKJ)
               END-PERFORM
           END-IF
      *    入院会計マスタクローズ
           MOVE    "NYUINACCT-KEY28"      TO  ORC-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
      *    保険組合せ
           MOVE   PTNYUINRRK-HKNCOMBINUM
                                       TO  WRK-HKNCOMBINUM
           MOVE   WRK-HKNCOMBINUM9     TO  LNK-SCNYUACCT-HKNCOMBINUM
           MOVE   SPA-SRYKA            TO  WRK-TAIHI-SRYKA
           MOVE   "00"                 TO  SPA-SRYKA
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
                  DISPLAY  "ORCGI01 ORCSNYUACCT ERR RC = "
                            LNK-SCNYUACCT-RC
           END-IF
      *
      *    入院会計作成サブの呼出し（異動前の会計作成）
      *
           MOVE    SPACE               TO  IPN-NYUKAIKEI-SAKUSEI-FLG
      *
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPID          TO  PTNYUINRRK-HOSPID
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "PTNYUINRRK-KEY9"   TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    =   ZERO  )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE              PTNYUINRRK-REC
           END-IF
      *
           MOVE    "PTNYUINRRK-KEY9"
                                   TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
      *    特定集中治療室管理料算定時に退院日をＳＰＡ−異動日と転入日にセット
      *    以下の特定入院料も同様
      *    救命救急入院料
      *    一類感染症患者入院医療管理料
      *    広範囲熱傷特定集中治療室管理料
      *    ハイケアユニット入院医療管理料
      *    亜急性期入院医療管理料
      *    脳卒中ケアユニット入院医療管理料
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN  = "03" OR "04" OR "05" OR
                                                "06" OR "07" OR "08" OR
                                                "09" OR "13" OR "14" OR
                                                "16" OR "17" OR "18" OR
                                                "19"
                    MOVE   SPA-NAI-I01-MAEIDOYMD
                                       TO   PTNYUINRRK-TENNYUYMD
                                            SPA-NAI-I01-IDOYMD

               END-IF
           END-IF
      *
           INITIALIZE      ORCSSYOKIKSNAREA
      *
      *    通算日数、算定開始日を求める
           MOVE    SPA-NAI-I01-MAEIDOYMD  TO   PTNYUINRRK-TENNYUYMD
           PERFORM 4801-TSUSAN-SANTEI-GET-SEC
           IF    ( SPA-ERRCD    NOT =  SPACE )
               GO  TO  4801-IDOKAIKEI-CANCEL-EXT
           END-IF
      *
           MOVE    WRK-TSUSAN      TO  SYOKIKSN-IN-TUSANNISSU
           MOVE    WRK-SANTEISTYMD
                                   TO  SYOKIKSN-IN-SANTEISTYMD
      *
           INITIALIZE              SYSKANRI-REC
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    SPA-NAI-I01-MAEIDOYMD
                                   TO  ORC-DBYMD
           MOVE    PTNYUINRRK-BTUNUM
                                   TO  SYS-KBNCD
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
      *
      *    システム管理マスタの病棟情報より取得
           MOVE    "SYSKANRI-KEY"  TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
      *
           IF   FLG-DBRC           =    ZERO
               MOVE    MCPDATA-REC TO   SYS-5001-REC
           END-IF
      *    システム管理クローズ
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF      PTNYUINRRK-TOKUTEI-NYUIN    NOT =   SPACE
               PERFORM   4801-TOKTEI-NYUIN-SET-SEC
           ELSE
               IF    ( SPA-NAI-I01-MAEIDOYMD   =  CONST-H180701 )
                AND  ( SPA-NAI-I01-BTUSBT       =  "04"  OR "09" )
                   CONTINUE
               ELSE
                   IF  SYS-5001-BTU-KHNSRYCD      NOT =   SPACE
                       MOVE    SYS-5001-BTU-KHNSRYCD
                                       TO  SYOKIKSN-IN-KHNSRYCD
                       PERFORM   4801-RJNCD-CHG-SEC
                       CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                       IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                           MOVE    "4023"      TO  SPA-ERRCD
                           GO  TO  4801-IDOKAIKEI-CANCEL-EXT
                       END-IF
                   ELSE
                       MOVE    "4024"      TO  SPA-ERRCD
                       GO  TO  4801-IDOKAIKEI-CANCEL-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    入院会計作成サブの呼出し
           INITIALIZE            ORCSNYUACCT-AREA
           MOVE     "1"     TO   LNK-SCNYUACCT-SYORI-KBN
           MOVE     "1"                TO  SPA-NYUGAIKBN
           MOVE     PTNYUINRRK-PTID    TO  SPA-PTID
           MOVE     PTNYUINRRK-NYUINKA TO  SPA-SRYKA
           MOVE    SYOKIKSN-IN-KHNSRYCD
                            TO   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                            TO   LNK-SCNYUACCT-STSRYYMD
           PERFORM   VARYING   IDX6   FROM   1   BY   1
               UNTIL (SYOKIKSN-OT-KGNSRYCD(IDX6)  =  SPACE) OR
                     (IDX6   >     7)
               MOVE   SYOKIKSN-OT-KGNSRYCD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOSTYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOEDYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX6)
           END-PERFORM
      *    通算算定済日数
           MOVE   WRK-TSUSAN    TO  LNK-SCNYUACCT-NYUKHN-NISUU
      *
      *    特定入院で入院料が変更になるコードの対応
      *    診療所老人医療管理料
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190774110"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU01      <=  14
                     ADD    1           TO  TN-IDX
                     MOVE   "190774110" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              診療所老人医療管理料の１４日以内の終日を再計算
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  14  - STUSAN-NISSU01
                     MOVE   SPA-NAI-I01-MAEIDOYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   SPA-NAI-I01-MAEIDOYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190774210"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     MOVE    SPA-NAI-I01-MAEIDOYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
           END-IF
      *
      *    老人性痴呆疾患治療病棟入院料
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190739910"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU01      <=  90
                     ADD    1           TO  TN-IDX
                     MOVE   "190739910" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              老人性痴呆疾患治療病棟入院料の９０日以内の終日を再計算
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  90  - STUSAN-NISSU01
                     MOVE   SPA-NAI-I01-MAEIDOYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   SPA-NAI-I01-MAEIDOYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190740010"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     MOVE    SPA-NAI-I01-MAEIDOYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
           END-IF
      *
      *    老人性痴呆疾患治療病棟入院料２
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190813410"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU01      <=  90
                     ADD    1           TO  TN-IDX
                     MOVE   "190813410" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              老人性痴呆疾患治療病棟入院料の９０日以内の終日を再計算
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  90  - STUSAN-NISSU01
                     MOVE   SPA-NAI-I01-MAEIDOYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   SPA-NAI-I01-MAEIDOYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190813510"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     MOVE    SPA-NAI-I01-MAEIDOYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
           END-IF
      *
      *    特定入院料で７日まで、１４日まで、それ以降は一般入院料を算定する
      *    場合の診療コードを取得
           PERFORM    4801-TOKUTEI-NYUIN-7-14-SEC
      *
           MOVE      ZERO               TO  TN-IDX
           IF    SYOKIKSN-IN-KHNSRYCD   =   WRK-TOKUTEI-SRYCD1
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          特定集中治療室管理料の算定可能日数を計算
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                                         WK-TOKUTEI-DAY2
                 COMPUTE  WK-TOKUTEI-DAY  =  14  - STUSAN-NISSU06
                 IF       STUSAN-NISSU07  <  7
                     ADD     1        TO     TN-IDX
                     COMPUTE  WK-TOKUTEI-DAY2 =  6   -  STUSAN-NISSU07
                     MOVE   WRK-TOKUTEI-SRYCD1  TO
                                    LNK-SCNYUACCT-KAGEN-SRYCD(TN-IDX)
                     MOVE   SPA-NAI-I01-IDOYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   SPA-NAI-I01-IDOYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE   WK-TOKUTEI-DAY2    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX)
                 END-IF
      *
                 IF     WK-TOKUTEI-DAY   >=   1
                     ADD     1     TO   TN-IDX
                     MOVE   WRK-TOKUTEI-SRYCD2  TO 
                                    LNK-SCNYUACCT-KAGEN-SRYCD(TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     IF    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1) NOT = SPACE
                         MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                               TO  LNK-DAY6-KIJUN
                         MOVE    "1"           TO  LNK-DAY6-ZOGENPTN
                         MOVE     1            TO  LNK-DAY6-ZOGEN
                         CALL  "ORCSDAY"       USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                         MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     ELSE
                         MOVE  SPA-NAI-I01-IDOYMD   TO  LNK-DAY6-KIJUN
                         MOVE  SPA-NAI-I01-IDOYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(TN-IDX)
                     END-IF
                     MOVE    "99999999"        TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(TN-IDX)
                 END-IF
      *          一般入院料の算定開始日を算定
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-IDOYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          一般入院会計作成用のフラグ設定
                 MOVE    "1"      TO      IPN-NYUKAIKEI-SAKUSEI-FLG
           END-IF
      *
      *    広範囲熱傷特定集中治療室管理料
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190024810"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          広範囲熱傷特定集中治療室管理料の算定可能日数を計算
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                 COMPUTE  WK-TOKUTEI-DAY  =  60  - STUSAN-NISSU06
                 IF       STUSAN-NISSU07  <  60
                     MOVE   "190024810"  TO
                                    LNK-SCNYUACCT-KAGEN-SRYCD(1)
                     MOVE   SPA-NAI-I01-IDOYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   SPA-NAI-I01-IDOYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
      *               MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                     COMPUTE  LNK-DAY6-ZOGEN = WK-TOKUTEI-DAY  -  1
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                 END-IF
      *          一般入院料の算定開始日を算定
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-IDOYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          特定入院期間（６０日経過）した際の一般入院料作成の
      *          可否を判定
                 MOVE    SPA-NAI-I01-IDOYMD
                                  TO      WRK-IPN-HANTEI-YMD1
                 IF      WRK-IPN-HANTEI-M1   =   12
                     ADD    1     TO      WRK-IPN-HANTEI-Y1
                     MOVE   1     TO      WRK-IPN-HANTEI-M1
                 ELSE
                     ADD    1     TO      WRK-IPN-HANTEI-M1
                 END-IF
      *          月末日を算定
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY7-AREA
                 MOVE    "71"              TO  LNK-DAY7-IRAI
                 MOVE   WRK-IPN-HANTEI-YM1 TO  LNK-DAY7-YM
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
                 MOVE    LNK-DAY7-KOYOMI   TO      WRK-IPN-HANTEI-YMD1
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                  TO      WRK-IPN-HANTEI-YMD2
      *          一般入院会計作成用のフラグ設定
                 IF      WRK-IPN-HANTEI-YMD1 > WRK-IPN-HANTEI-YMD2
                     MOVE    "1"   TO     IPN-NYUKAIKEI-SAKUSEI-FLG
                 END-IF
           END-IF
      *
      *    ハイケアユニット入院医療管理料（２１日を限度）
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190117310"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          ハイケアユニット入院医療管理料の算定可能日数を計算
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                 COMPUTE  WK-TOKUTEI-DAY  =  21  - STUSAN-NISSU06
      *
                 IF     WK-TOKUTEI-DAY   >=   1
                     MOVE   "190117310"  TO 
                                    LNK-SCNYUACCT-KAGEN-SRYCD(1)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE    SPA-NAI-I01-IDOYMD  TO
                                 LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                                 LNK-DAY6-KIJUN
                     MOVE    "1"           TO  LNK-DAY6-ZOGENPTN
                     MOVE     WK-TOKUTEI-DAY
                                           TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"       USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                 END-IF
      *          一般入院料の算定開始日を算定
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-IDOYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          一般入院会計作成用のフラグ設定
                 MOVE    "1"      TO      IPN-NYUKAIKEI-SAKUSEI-FLG
           END-IF
      *
      *    亜急性期入院医療管理料
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190117410"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          亜急性期入院医療管理料の算定可能日数を計算
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                 COMPUTE  WK-TOKUTEI-DAY  =  90  - STUSAN-NISSU06
                 IF       STUSAN-NISSU07  <  90
                     MOVE   "190117410"  TO
                                    LNK-SCNYUACCT-KAGEN-SRYCD(1)
                     MOVE   SPA-NAI-I01-IDOYMD   TO
                                  LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   SPA-NAI-I01-IDOYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
      *               MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                     COMPUTE  LNK-DAY6-ZOGEN = WK-TOKUTEI-DAY  -  1
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                 END-IF
      *          一般入院料の算定開始日を算定
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-IDOYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          特定入院期間（６０日経過）した際の一般入院料作成の
      *          可否を判定
                 MOVE    SPA-NAI-I01-IDOYMD
                                  TO      WRK-IPN-HANTEI-YMD1
                 IF      WRK-IPN-HANTEI-M1   =   12
                     ADD    1     TO      WRK-IPN-HANTEI-Y1
                     MOVE   1     TO      WRK-IPN-HANTEI-M1
                 ELSE
                     ADD    1     TO      WRK-IPN-HANTEI-M1
                 END-IF
      *          月末日を算定
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY7-AREA
                 MOVE    "71"              TO  LNK-DAY7-IRAI
                 MOVE   WRK-IPN-HANTEI-YM1 TO  LNK-DAY7-YM
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
                 MOVE    LNK-DAY7-KOYOMI   TO      WRK-IPN-HANTEI-YMD1
                 MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                  TO      WRK-IPN-HANTEI-YMD2
      *          一般入院会計作成用のフラグ設定
                 IF      WRK-IPN-HANTEI-YMD1 > WRK-IPN-HANTEI-YMD2
                     MOVE    "1"   TO     IPN-NYUKAIKEI-SAKUSEI-FLG
                 END-IF
           END-IF
      *
      *    脳卒中ケアユニット入院医療管理料（１４日を限度）
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190120810"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *          脳卒中ケアユニット入院医療管理料の算定可能日数を計算
                 MOVE     ZERO      TO   WK-TOKUTEI-DAY
                 COMPUTE  WK-TOKUTEI-DAY  =  14  - STUSAN-NISSU06
      *
                 IF     WK-TOKUTEI-DAY   >=   1
                     MOVE   "190120810"  TO 
                                    LNK-SCNYUACCT-KAGEN-SRYCD(1)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE    SPA-NAI-I01-IDOYMD  TO
                                 LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
                                 LNK-DAY6-KIJUN
                     MOVE    "1"           TO  LNK-DAY6-ZOGENPTN
                     MOVE     WK-TOKUTEI-DAY
                                           TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"       USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN   TO
                                  LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                 END-IF
      *          一般入院料の算定開始日を算定
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   SPA-NAI-I01-IDOYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE   WK-TOKUTEI-DAY     TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN
                                  TO       IPN-SANTEISTYMD
      *          一般入院会計作成用のフラグ設定
                 MOVE    "1"      TO      IPN-NYUKAIKEI-SAKUSEI-FLG
           END-IF
      *
      *    精神科救急入院料
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190121010"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU01      <=  30
                     ADD    1           TO  TN-IDX
                     MOVE   "190121010" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              精神科救急入院料の３０日以内の終日を再計算
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  30  - STUSAN-NISSU01
                     MOVE   SPA-NAI-I01-MAEIDOYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   SPA-NAI-I01-MAEIDOYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190111410"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     MOVE    SPA-NAI-I01-MAEIDOYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
           END-IF
      *
      *    精神科急性期治療病棟入院料１
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190121110"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU01      <=  30
                     ADD    1           TO  TN-IDX
                     MOVE   "190121110" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              精神科救急入院料の３０日以内の終日を再計算
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  30  - STUSAN-NISSU01
                     MOVE   SPA-NAI-I01-MAEIDOYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   SPA-NAI-I01-MAEIDOYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190066910"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     MOVE    SPA-NAI-I01-MAEIDOYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
           END-IF
      *
      *    精神科急性期治療病棟入院料２
           IF    SYOKIKSN-IN-KHNSRYCD   =   "190121210"
                 MOVE   SPACE       TO   SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                 MOVE   ZERO            TO  TN-IDX
                 IF     STUSAN-NISSU01      <=  30
                     ADD    1           TO  TN-IDX
                     MOVE   "190121210" TO   LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *              精神科救急入院料の３０日以内の終日を再計算
                     MOVE     ZERO      TO   WK-TOKUTEI-DAY
                     COMPUTE  WK-TOKUTEI-DAY  =  30  - STUSAN-NISSU01
                     MOVE   SPA-NAI-I01-MAEIDOYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   SPA-NAI-I01-MAEIDOYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
                 END-IF
      *
                 ADD    1   TO    TN-IDX
      *
                 MOVE   "190067010"  TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 IF      TN-IDX      =     2
                     MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE     1                TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 ELSE
                     MOVE    SPA-NAI-I01-MAEIDOYMD
                                       TO LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                 END-IF
                 MOVE    "99999999"    TO LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
           END-IF
      *
      *-------特定入院対応ここまで-----------------------------------
      *
      *    入院料加算診療コード取り込み
           PERFORM    4801-NYUKSN-GET-SEC
      *
      *    特定入院料区分のセット
           IF      PTNYUINRRK-TOKUTEI-NYUIN    =   SPACE
               MOVE     "0"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
           ELSE
               MOVE     "1"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
      *        老人特定入院基本料及び平成１８年７月以降の療養病棟、
      *        療養病床の場合は特定入院料扱いから外す
               IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410" OR
                                                       "190799510" OR
                                                       "190121310" OR
                                                       "190121410" OR
                                                       "190121510" OR
                                                       "190121610" OR
                                                       "190121710" OR
                                                       "190121810" OR
                                                       "190122010" OR
                                                       "190122110" OR
                                                       "190122210" OR
                                                       "190122310" OR
                                                       "190122410" OR
                                                       "190121510"
                   MOVE  "0"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
               END-IF
           END-IF
      *    外泊
           MOVE   "099999911"   TO  LNK-SCNYUACCT-GAIHAKU-SRYCD
      *    室料差額
           MOVE   "099999912"   TO  LNK-SCNYUACCT-SAGAKU-SRYCD
           IF     PTNYUINRRK-RM-SAGAKU   =   SPACE
               MOVE    ZERO     TO  LNK-SCNYUACCT-SAGAKU-KBN
           ELSE
               MOVE   PTNYUINRRK-RM-SAGAKU 
                                TO  WRK-SAGAKU-KBN
               MOVE   WRK-SAGAKU-KBN9
                                TO  LNK-SCNYUACCT-SAGAKU-KBN
           END-IF
      *    食事区分
           MOVE   "099999913"   TO  LNK-SCNYUACCT-SHOKUJI-SRYCD
           IF     WRK-TAIHI-SHOKUJI    NOT =   ZERO
                  MOVE   WRK-TAIHI-SHOKUJI
                                TO  LNK-SCNYUACCT-SHOKUJIKBN
           ELSE
                  MOVE   1      TO  LNK-SCNYUACCT-SHOKUJIKBN
           END-IF
      *    食堂加算
           PERFORM     4801-BTUSEL-SEC
           IF   FLG-SYSKANRI           =    ZERO
               IF   SYS-5001-BTU-SYOKUDOKBN       =    "2"
                    MOVE     "197000570"    TO
                                LNK-SCNYUACCT-SHOKUDO-SRYCD
                    MOVE    1   TO   LNK-SCNYUACCT-SHOKUDO-KBN
               END-IF
           END-IF
      *    保険組合せ
           MOVE   "099999914"   TO  LNK-SCNYUACCT-HKNCOMBINUM-SRYCD
           IF     WRK-TAIHI-HKN    NOT =   ZERO
                  MOVE   WRK-TAIHI-HKN
                                TO  LNK-SCNYUACCT-HKNCOMBINUM
           ELSE
                  MOVE   PTNYUINRRK-HKNCOMBINUM
                                TO  WRK-HKNCOMBINUM
                  MOVE   WRK-HKNCOMBINUM9 
                                TO  LNK-SCNYUACCT-HKNCOMBINUM
           END-IF
           MOVE   SPA-SRYKA     TO  WRK-TAIHI-SRYKA
           MOVE   "00"          TO  SPA-SRYKA
           PERFORM 4801-SENTEI-SET-SEC
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
               DISPLAY  "ORCGI01 ORCSNYUACCT ERR RC = "
                         LNK-SCNYUACCT-RC
           END-IF
      *
           IF     IPN-NYUKAIKEI-SAKUSEI-FLG    =   "1"
               MOVE    SPACE   TO   SPA-GMN-I01-TOKNYUIN-CD
               MOVE    IPN-SANTEISTYMD   TO   WRK-SANTEISTYMD
               MOVE    IPN-SANTEISTYMD   TO   SPA-NAI-I01-IDOYMD
               MOVE    IPN-SANTEISTYMD   TO   PTNYUINRRK-TENNYUYMD
               PERFORM   4801-TENROOM-EDIT-SEC
           END-IF
      *
      *    入院会計作成後に退避データの食事区分を復元
      *    （異動月分）
           INITIALIZE                      NYUINACCT-REC
           MOVE    PTNYUINRRK-HOSPID   TO  NACCT-HOSPID
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    WRK-TENROOM-SKJ-SRYYM(1)
                                       TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "NYUINACCT-KEY28"   TO  WRK-DBPATH
           PERFORM   900-TBL-SELECT-SEC
           IF      FLG-DBRC            =   ZERO
               MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *        食事を復元
               MOVE    WRK-TENROOM-SKJ-DD(1)
                                       TO  IDX-SHOKUJIX
               PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                         UNTIL     IDX-SKJ   >   31
                    MOVE   WRK-TENROOM-SKJ(1 IDX-SKJ)
                                       TO  NACCT-DAY(IDX-SKJ)
               END-PERFORM
           END-IF
           MOVE     NYUINACCT-REC      TO  MCPDATA-REC
           MOVE     "DBUPDATE"         TO  MCP-FUNC
           MOVE     "NYUINACCT-KEY"    TO  ORC-DBPATH
           CALL     "ORCMCPSUB"        USING  MCPAREA
                                              ORCMCPAREA
                                              MCPDATA-REC 
      *    入院会計マスタクローズ
           MOVE    "NYUINACCT-KEY28"      TO  ORC-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *    （異動翌月分）
           IF   WRK-TENROOM-SKJ-SRYYM(2)  NOT =   SPACE
               INITIALIZE                      NYUINACCT-REC
               MOVE    PTNYUINRRK-HOSPID   TO  NACCT-HOSPID
               MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    WRK-TENROOM-SKJ-SRYYM(2)
                                           TO  NACCT-SRYYM
      *
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "NYUINACCT-KEY28"   TO  WRK-DBPATH
               PERFORM   900-TBL-SELECT-SEC
               IF      FLG-DBRC            =   ZERO
                   MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *            食事を復元
                   PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                             UNTIL     IDX-SKJ   >   31
                        MOVE   WRK-TENROOM-SKJ(2 IDX-SKJ)
                                           TO  NACCT-DAY(IDX-SKJ)
                   END-PERFORM
               END-IF
               MOVE     NYUINACCT-REC      TO  MCPDATA-REC
               MOVE     "DBUPDATE"         TO  MCP-FUNC
               MOVE     "NYUINACCT-KEY"    TO  ORC-DBPATH
               CALL     "ORCMCPSUB"        USING  MCPAREA
                                                  ORCMCPAREA
                                                  MCPDATA-REC 
      *        入院会計マスタクローズ
               MOVE    "NYUINACCT-KEY28"      TO  ORC-DBPATH
               PERFORM 900-TBL-CLOSE-SEC
           END-IF
      *
           .
       4801-IDOKAIKEI-CANCEL-EXT.
           EXIT.
      *
      *****************************************************************
      *     選定療養費診療行為コード取得処理
      *****************************************************************
       4801-SENTEI-SET-SEC         SECTION.
      *
           INITIALIZE  SENTEICDCHG-REC
           MOVE    LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                   TO  SENCHG-IPNSRYCD
           MOVE    SENTEICDCHG-REC TO  MCPDATA-REC
           MOVE    "SENTEICDCHG-KEY2"
                                   TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    =   ZERO  )
               MOVE    MCPDATA-REC TO  SENTEICDCHG-REC
           ELSE
               INITIALIZE              SENTEICDCHG-REC
           END-IF
      *
           MOVE    "SENTEICDCHG-KEY2"
                                   TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
      **     MOVE    SENCHG-SENTEISRYCD
      **                             TO  LNK-SCNYUACCT-SENTEI-SRYCD
           IF      SENCHG-SENTEISRYCD    NOT =  SPACE
               MOVE    "099999916"   TO  LNK-SCNYUACCT-SENTEI-SRYCD
               MOVE    WRK-SENTEI-STYMD
                                     TO  LNK-SCNYUACCT-SENTEI-STYMD
           END-IF
      *    １５才未満の患者は選定入院対象外
           MOVE     PTINF-BIRTHDAY   TO  WRK-BIRTHDAY-15
           ADD      15               TO  WRK-BIRTHDAY-15Y
           IF       WRK-SENTEI-STYMD <=  WRK-BIRTHDAY-15
               MOVE   WRK-BIRTHDAY-15  TO  LNK-SCNYUACCT-SENTEI-STYMD
           END-IF
      *
      *
           .
       4801-SENTEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    通算日数取得処理
      *****************************************************************
       4801-TSUSAN-SANTEI-GET-SEC      SECTION.
      *
           INITIALIZE                  WRK-TSUSAN
                                       WRK-SANTEISTYMD
      *
           INITIALIZE                  ORCSTUSANAREA
           MOVE    SPA-HOSPID      TO  STUSAN-HOSPID
           MOVE    PTNYUINRRK-PTID TO  STUSAN-PTID
      *---------------退院日の同日入院処理ここから----------------------
           IF   WRK-DOU-TENNYUYMD    NOT =   SPACE
               MOVE    WRK-DOU-TENNYUYMD  TO  PTNYUINRRK-TENNYUYMD
           END-IF
      *---------------退院日の同日入院処理ここまで----------------------
           MOVE    PTNYUINRRK-TENNYUYMD
                                   TO  STUSAN-KJNYMD
      *    特定入院での算定済日数を取得用
      *    特定集中治療室管理料
      *    救命救急入院料
      *    一類感染症患者入院医療管理料
      *    広範囲熱傷特定集中治療室管理料
      *    ハイケアユニット入院医療管理料
      *    亜急性期入院医療管理料
      *    脳卒中ケアユニット入院医療管理料
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               EVALUATE   PTNYUINRRK-TOKUTEI-NYUIN
                   WHEN   "03"
                   WHEN   "04"
                   WHEN   "05"
                       MOVE   "190024510"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190074510"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190110210"  TO  STUSAN-TOKUTEI-SRYCD(3)
                       MOVE   "190110610"  TO  STUSAN-TOKUTEI-SRYCD(4)
                       MOVE   "190110310"  TO  STUSAN-TOKUTEI-SRYCD(5)
                       MOVE   "190110710"  TO  STUSAN-TOKUTEI-SRYCD(6)
                   WHEN   "06"
                   WHEN   "07"
                   WHEN   "08"
                       MOVE   "190024310"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190024410"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190110410"  TO  STUSAN-TOKUTEI-SRYCD(3)
                       MOVE   "190110810"  TO  STUSAN-TOKUTEI-SRYCD(4)
                       MOVE   "190110510"  TO  STUSAN-TOKUTEI-SRYCD(5)
                       MOVE   "190110910"  TO  STUSAN-TOKUTEI-SRYCD(6)
                   WHEN   "09"
                   WHEN   "16"
                       MOVE   "190024610"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190074610"  TO  STUSAN-TOKUTEI-SRYCD(2)
                       MOVE   "190116310"  TO  STUSAN-TOKUTEI-SRYCD(3)
                       MOVE   "190116410"  TO  STUSAN-TOKUTEI-SRYCD(4)
                   WHEN   "14"
                       MOVE   "190075510"  TO  STUSAN-TOKUTEI-SRYCD(1)
                       MOVE   "190075610"  TO  STUSAN-TOKUTEI-SRYCD(2)
                   WHEN   "13"
                       MOVE   "190024810"  TO  STUSAN-TOKUTEI-SRYCD(1)
                   WHEN   "17"
                       MOVE   "190117310"  TO  STUSAN-TOKUTEI-SRYCD(1)
                   WHEN   "18"
                       MOVE   "190117410"  TO  STUSAN-TOKUTEI-SRYCD(1)
                   WHEN   "19"
                       MOVE   "190120810"  TO  STUSAN-TOKUTEI-SRYCD(1)
               END-EVALUATE
           END-IF
      *    老人性痴呆疾患治療病棟入院料１
      *    老人性痴呆疾患治療病棟入院料２
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "2"
               EVALUATE   PTNYUINRRK-TOKUTEI-NYUIN
                   WHEN   "05"
                       MOVE   "190739910"  TO  STUSAN-TOKUTEI-SRYCD(1)
                   WHEN   "15"
                       MOVE   "190813410"  TO  STUSAN-TOKUTEI-SRYCD(1)
               END-EVALUATE
           END-IF
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
           IF    ( STUSAN-RC       NOT =   ZERO )
               MOVE    "4025"      TO  SPA-ERRCD
               GO  TO  4801-TSUSAN-SANTEI-GET-EXT
           END-IF
      *
           COMPUTE WRK-TSUSAN  =   STUSAN-NISSU03  -   1
      *---------------退院日の同日入院処理ここから----------------------
      *     IF   WRK-DOU-TENNYUYMD    NOT =   SPACE
      *         MOVE    WRK-DOU-TENNYUYMD  TO  PTNYUINRRK-TENNYUYMD
      *     END-IF
      *---------------退院日の同日入院処理ここまで----------------------
           MOVE    PTNYUINRRK-TENNYUYMD
                                   TO  WRK-SANTEISTYMD
      *
           MOVE    WRK-SANTEISTYMD
                                   TO  WRK-YMD
           MOVE    "1"             TO  WRK-ZOGENPTN
           IF    ( STUSAN-KJNYMD-CHK-FLG  =   ZERO )
               COMPUTE WRK-ZOGEN   =   180 -   STUSAN-NISSU04
           ELSE
               COMPUTE WRK-ZOGEN   =   180 - ( STUSAN-NISSU04  -   1 )
           END-IF
      *
           PERFORM 5002-CALENDAR-SEC
           MOVE    WRK-KEISANYMD   TO  WRK-SENTEI-STYMD
      *
           .
       4801-TSUSAN-SANTEI-GET-EXT.
           EXIT.
      ******************************************************************
      *    通算日数、算定開始日取得処理（入院登録時）
      *****************************************************************
      *- 4801-NYUTSUSAN-SANTEI-GET-SEC   SECTION.
      *-*
      *-     INITIALIZE                  WRK-TSUSAN
      *-                                 WRK-SANTEISTYMD
      *-*
      *-     PERFORM VARYING IDX1    FROM    1   BY  1
      *-         UNTIL     ( IDX1    >  CONST-NYUREKILST-MAX )
      *-         IF    ( SPA-NAI-I01-TZITA-FLG (IDX1)
      *-                                             =   2 )
      *-          AND  ( SPA-GMN-I01-TSHONUM (IDX1)
      *-                             =   PTNYUINRRK-SHONUM )
      *-             COMPUTE WRK-TSUSAN
      *-                 =   WRK-TSUSAN
      *-                 +   SPA-NAI-I01-TNYURRKTAISYO (IDX1)
      *-         END-IF
      *-*
      *-     END-PERFORM
      *-*
      *-*    歴をさかのぼり通算の対象となるデータを抽出する
      *-     INITIALIZE    WRK-REKI-ZIIN-TBL
      *-     PERFORM VARYING IDX1    FROM    1   BY  1
      *-         UNTIL     ( IDX1    >  CONST-NYUREKILST-MAX )
      *-         IF    ( SPA-NAI-I01-TZITA-FLG (IDX1)
      *-                                             =   ZERO )
      *-          AND  ( SPA-GMN-I01-TSHONUM (IDX1)
      *-                             =   PTNYUINRRK-SHONUM )
      *-*
      *-             COMPUTE WRK-REKI-ZIIN-CNT
      *-                 =   WRK-REKI-ZIIN-CNT   +   1
      *-*
      *-             MOVE    WRK-REKI-ZIIN-CNT   TO  IDX2
      *-             MOVE    SPA-NAI-I01-TNYUINYMD (IDX1)
      *-                             TO  WRK-REKI-ZIIN-NYUINYMD (IDX2)
      *-             MOVE    SPA-NAI-I01-TTAIINYMD (IDX1)
      *-                             TO  WRK-REKI-ZIIN-TAIINYMD (IDX2)
      *-*
      *-            IF    ( SPA-GMN-I01-TSHOKAI (IDX1)  =   "1" )
      *-                MOVE   CONST-NYUREKILST-MAX     TO  IDX1
      *-            END-IF
      *-*
      *-         END-IF
      *-     END-PERFORM
      *-*
      *-*    入院日が前回退院日と同日の場合は、翌日を算定開始日に設定する
      *-     IF    ( SPA-GMN-I01-NYUREKILST-CNT  >   ZERO )
      *-      AND  ( SPA-NAI-I01-TZITA-FLG (1)   =   ZERO )
      *-      AND  ( PTNYUINRRK-NYUINYMD
      *-                             =   SPA-NAI-I01-TTAIINYMD (1) )
      *-         MOVE    PTNYUINRRK-NYUINYMD
      *-                             TO WRK-YMD
      *-         MOVE    "1"         TO WRK-ZOGENPTN
      *-         MOVE    1           TO WRK-ZOGEN
      *-         PERFORM 5002-CALENDAR-SEC
      *-         MOVE    WRK-KEISANYMD
      *-                             TO  WRK-SANTEISTYMD
      *-     ELSE
      *-         MOVE    PTNYUINRRK-NYUINYMD
      *-                             TO  WRK-SANTEISTYMD
      *-     END-IF
      *-*
      *-*    今回が初回の場合、または過去に歴作成データしかない場合は、
      *-*    以下の処理は行わない
      *-     IF    ( PTNYUINRRK-SHOKAIKBN    =   "1" )
      *-       OR  ( WRK-REKI-ZIIN-CNT      <=  ZERO )
      *-     DISPLAY "************************"
      *-     DISPLAY "*** NYUTSUSAN-SANTEI ***"
      *-     DISPLAY "WRK-TSUSAN      = " WRK-TSUSAN
      *-     DISPLAY "WRK-SANTEISTYMD = " WRK-SANTEISTYMD
      *-     DISPLAY "************************"
      *-         GO  TO  4801-NYUTSUSAN-SANTEI-GET-EXT
      *-     END-IF
      *-*
      *-     PERFORM VARYING IDX1
      *-             FROM    WRK-REKI-ZIIN-CNT
      *-             BY      -1
      *-             UNTIL ( IDX1    <   1 )
      *-*
      *-         MOVE    WRK-REKI-ZIIN-NYUINYMD (IDX1)
      *-                         TO  WRK-STYMD
      *-         MOVE    WRK-REKI-ZIIN-TAIINYMD (IDX1)
      *-                         TO  WRK-EDYMD
      *-         PERFORM 5002-KIKAN-CALC-SEC
      *-         IF    ( FLG-ORCSDAY =   ZERO )
      *-             IF    ( WRK-REKI-ZIIN-CNT
      *-                                     >   IDX1 )
      *-              AND  ( WRK-REKI-ZIIN-NYUINYMD (IDX1)
      *-                 =   WRK-REKI-ZIIN-TAIINYMD (IDX1 + 1) )
      *-                 COMPUTE WRK-TSUSAN
      *-                     =   WRK-TSUSAN  +   WRK-NISSU2  -   1
      *-             ELSE
      *-                 COMPUTE WRK-TSUSAN
      *-                     =   WRK-TSUSAN  +   WRK-NISSU2
      *-             END-IF
      *-         END-IF
      *-*
      *-     END-PERFORM
      *-*
      *-     DISPLAY "*** NYUTSUSAN-SANTEI ***"
      *-     DISPLAY "WRK-TSUSAN = "  WRK-TSUSAN
      *-     DISPLAY "WRK-SANTEISTYMD = " WRK-SANTEISTYMD
      *-     DISPLAY "************************"
      *-*
      *-     .
      *- 4801-NYUTSUSAN-SANTEI-GET-EXT.
      *-     EXIT.
      *****************************************************************
      *    通算日数、算定開始日取得処理（転科・転棟・転室時）
      *****************************************************************
      *- 4801-IDOTSUSAN-SANTEI-GET-SEC   SECTION.
      *-*
      *-     INITIALIZE                  WRK-NYUINNISSU
      *-                                 WRK-TSUSAN
      *-                                 WRK-SANTEISTYMD
      *-*
      *-     PERFORM VARYING IDX1    FROM    1   BY  1
      *-         UNTIL     ( IDX1    >  CONST-NYUREKILST-MAX )
      *-         IF    ( SPA-NAI-I01-TZITA-FLG (IDX1)
      *-                                             =   2 )
      *-          AND  ( SPA-GMN-I01-TSHONUM (IDX1)
      *-                             =   PTNYUINRRK-SHONUM )
      *-             COMPUTE WRK-TSUSAN
      *-                 =   WRK-TSUSAN
      *-                 +   SPA-NAI-I01-TNYURRKTAISYO (IDX1)
      *-         END-IF
      *-*
      *-     END-PERFORM
      *-*
      *-*    入院日から（異動日当日を含まない）異動日までの日数を求める
      *-*    （入院日数とする）
      *-     MOVE    PTNYUINRRK-NYUINYMD
      *-                                 TO  WRK-STYMD
      *-     MOVE    PTNYUINRRK-TENNYUYMD
      *-                                 TO  WRK-EDYMD
      *-     PERFORM 5002-KIKAN-CALC-SEC
      *-     MOVE    WRK-NISSU1          TO  WRK-NYUINNISSU
      *-*
      *-*    歴をさかのぼり通算の対象となるデータを抽出する
      *-     INITIALIZE    WRK-REKI-ZIIN-TBL
      *-     PERFORM VARYING IDX1    FROM    2   BY  1
      *-         UNTIL     ( IDX1    >  CONST-NYUREKILST-MAX )
      *-         IF    ( SPA-NAI-I01-TZITA-FLG (IDX1)
      *-                                             =   ZERO )
      *-          AND  ( SPA-GMN-I01-TSHONUM (IDX1)
      *-                             =   PTNYUINRRK-SHONUM )
      *-*
      *-             COMPUTE WRK-REKI-ZIIN-CNT
      *-                 =   WRK-REKI-ZIIN-CNT   +   1
      *-*
      *-             MOVE    WRK-REKI-ZIIN-CNT   TO  IDX2
      *-             MOVE    SPA-NAI-I01-TNYUINYMD (IDX1)
      *-                             TO  WRK-REKI-ZIIN-NYUINYMD (IDX2)
      *-             MOVE    SPA-NAI-I01-TTAIINYMD (IDX1)
      *-                             TO  WRK-REKI-ZIIN-TAIINYMD (IDX2)
      *-*
      *-            IF    ( SPA-GMN-I01-TSHOKAI (IDX1)  =   "1" )
      *-                MOVE  CONST-NYUREKILST-MAX      TO  IDX1
      *-            END-IF
      *-*
      *-         END-IF
      *-     END-PERFORM
      *-*
      *-*    異動日が前回退院日と同日の場合は、翌日を算定開始日に設定する
      *-     IF    ( SPA-GMN-I01-NYUREKILST-CNT  >   1 )
      *-      AND  ( SPA-NAI-I01-TZITA-FLG (2)   =   ZERO )
      *-      AND  ( PTNYUINRRK-TENNYUYMD
      *-                             =   SPA-NAI-I01-TTAIINYMD (2) )
      *-         MOVE    PTNYUINRRK-TENNYUYMD
      *-                             TO WRK-YMD
      *-         MOVE    "1"         TO WRK-ZOGENPTN
      *-         MOVE    1           TO WRK-ZOGEN
      *-         PERFORM 5002-CALENDAR-SEC
      *-         MOVE    WRK-KEISANYMD
      *-                             TO  WRK-SANTEISTYMD
      *-     ELSE
      *-*        異動日が前回退院日と同日で無い場合は、異動日を算定開始日
      *-*        に設定する。
      *-*        通算日数は入院日と前回退院日が同日の場合は、
      *-*        歴を遡って求めた通算日数＋入院日数−１
      *-*        そうでない場合、通算日数＋入院日数を通算日数に設定する
      *-         MOVE    PTNYUINRRK-TENNYUYMD
      *-                             TO  WRK-SANTEISTYMD
      *-         IF    ( SPA-GMN-I01-NYUREKILST-CNT  >   1 )
      *-          AND  ( SPA-NAI-I01-TZITA-FLG (2)   =   ZERO )
      *-          AND  ( PTNYUINRRK-NYUINYMD
      *-                             =   SPA-NAI-I01-TTAIINYMD (2) )
      *-             COMPUTE WRK-TSUSAN
      *-                 =   WRK-TSUSAN  +   WRK-NYUINNISSU  -   1
      *-         ELSE
      *-             COMPUTE WRK-TSUSAN
      *-                 =   WRK-TSUSAN  +   WRK-NYUINNISSU
      *-         END-IF
      *-     END-IF
      *-*
      *-*    今回が初回の場合、または過去に歴作成データしかない場合は、
      *-*    以下の処理は行わない
      *-     IF    ( PTNYUINRRK-SHOKAIKBN    =   "1" )
      *-       OR  ( WRK-REKI-ZIIN-CNT      <=  ZERO )
      *-     DISPLAY "************************"
      *-     DISPLAY "*** IDOTSUSAN-SANTEI ***"
      *-     DISPLAY "WRK-TSUSAN      = " WRK-TSUSAN
      *-     DISPLAY "WRK-SANTEISTYMD = " WRK-SANTEISTYMD
      *-     DISPLAY "************************"
      *-         GO  TO  4801-IDOTSUSAN-SANTEI-GET-EXT
      *-     END-IF
      *-*
      *-     PERFORM VARYING IDX1
      *-             FROM    WRK-REKI-ZIIN-CNT
      *-             BY      -1
      *-             UNTIL ( IDX1    <   1 )
      *-*
      *-         MOVE    WRK-REKI-ZIIN-NYUINYMD (IDX1)
      *-                         TO  WRK-STYMD
      *-         MOVE    WRK-REKI-ZIIN-TAIINYMD (IDX1)
      *-                         TO  WRK-EDYMD
      *-         PERFORM 5002-KIKAN-CALC-SEC
      *-         IF    ( FLG-ORCSDAY =   ZERO )
      *-             IF    ( WRK-REKI-ZIIN-CNT
      *-                                     >   IDX1 )
      *-              AND  ( WRK-REKI-ZIIN-NYUINYMD (IDX1)
      *-                 =   WRK-REKI-ZIIN-TAIINYMD (IDX1 + 1) )
      *-                 COMPUTE WRK-TSUSAN
      *-                     =   WRK-TSUSAN  +   WRK-NISSU2  -   1
      *-             ELSE
      *-                 COMPUTE WRK-TSUSAN
      *-                     =   WRK-TSUSAN  +   WRK-NISSU2
      *-             END-IF
      *-         END-IF
      *-*
      *-     END-PERFORM
      *-*
      *-     DISPLAY "*** IDOTSUSAN-SANTEI ***"
      *-     DISPLAY "WRK-TSUSAN = "  WRK-TSUSAN
      *-     DISPLAY "WRK-SANTEISTYMD = " WRK-SANTEISTYMD
      *-     DISPLAY "************************"
      *-*
      *-     .
      *- 4801-IDOTSUSAN-SANTEI-GET-EXT.
      *-     EXIT.
      *****************************************************************
      *    病棟情報検索処理
      *****************************************************************
       4801-BTUSEL-SEC             SECTION.
      *
           INITIALIZE              SYSKANRI-REC
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    PTNYUINRRK-BTUNUM
                                   TO  SYS-KBNCD
           EVALUATE   SPA-GMN-I01-SYORIKBN-CD
               WHEN   "01"
                   MOVE   SPA-NAI-I01-NYUINYMD
                                   TO  ORC-DBYMD
               WHEN   "07"
                   MOVE   SPA-NAI-I01-TAIINYMD
                                   TO  ORC-DBYMD
               WHEN   "08"
                   MOVE   SPA-NAI-I01-IDOYMD
                                   TO  ORC-DBYMD
               WHEN   "09"
                   MOVE   SPA-NAI-I01-MAEIDOYMD
                                   TO  ORC-DBYMD
               WHEN   OTHER
                   MOVE    SPA-SYSYMD
                                   TO  ORC-DBYMD
           END-EVALUATE
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY10"
                                   TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    =   ZERO )
               MOVE    MCPDATA-REC
                                  TO  SYS-5001-REC
           ELSE
               MOVE    1          TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "SYSKANRI-KEY10"
                                   TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
       4801-BTUSEL-EXT.
           EXIT.
      *****************************************************************
      *    病室情報検索処理
      *****************************************************************
       4801-BRMSEL-SEC             SECTION.
      *
           INITIALIZE              SYSKANRI-REC
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "5002"          TO  SYS-KANRICD
           MOVE    PTNYUINRRK-BRMNUM
                                   TO  SYS-KBNCD
           EVALUATE   SPA-GMN-I01-SYORIKBN-CD
               WHEN   "01"
                   MOVE   SPA-NAI-I01-NYUINYMD
                                   TO  ORC-DBYMD
               WHEN   "07"
                   MOVE   SPA-NAI-I01-TAIINYMD
                                   TO  ORC-DBYMD
               WHEN   "08"
                   MOVE   SPA-NAI-I01-IDOYMD
                                   TO  ORC-DBYMD
               WHEN   "09"
                   MOVE   SPA-NAI-I01-MAEIDOYMD
                                   TO  ORC-DBYMD
               WHEN   OTHER
                   MOVE    SPA-SYSYMD
                                   TO  ORC-DBYMD
           END-EVALUATE
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY10"
                                   TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    =   ZERO )
               MOVE    MCPDATA-REC
                                  TO  SYS-5002-REC
           ELSE
               MOVE    1          TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "SYSKANRI-KEY10"
                                   TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
       4801-BRMSEL-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料セット処理
      *****************************************************************
       4801-TOKTEI-NYUIN-SET-SEC   SECTION.
      *    病棟
           IF  PTNYUINRRK-TOKUTEI-KBN   =   "1"
             EVALUATE    PTNYUINRRK-TOKUTEI-NYUIN
               WHEN    "01"
                       MOVE   "190075810"   TO  SYOKIKSN-IN-KHNSRYCD
             END-EVALUATE
           END-IF
      *    病棟
           IF  PTNYUINRRK-TOKUTEI-KBN   =   "2"
             EVALUATE    PTNYUINRRK-TOKUTEI-NYUIN
               WHEN    "01"
                       MOVE   "190111110"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "02"
                       MOVE   "190111210"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "03"
                       MOVE   "190055210"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "04"
                       MOVE   "190055310"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "05"
                       MOVE   "190739910"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "06"
                       MOVE   "190740110"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "07"
                       MOVE   "190055010"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "08"
                       MOVE   "190055110"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "09"
                       MOVE   "190028910"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "10"
                   IF  SYOKIKSN-IN-SANTEISTYMD(1:6) <= "200603"
                       MOVE   "190111410"   TO  SYOKIKSN-IN-KHNSRYCD
                   ELSE
                       MOVE   "190121010"   TO  SYOKIKSN-IN-KHNSRYCD
                   END-IF
               WHEN    "11"
                   IF  SYOKIKSN-IN-SANTEISTYMD(1:6) <= "200603"
                       MOVE   "190066910"   TO  SYOKIKSN-IN-KHNSRYCD
                   ELSE
                       MOVE   "190121110"   TO  SYOKIKSN-IN-KHNSRYCD
                   END-IF
               WHEN    "12"
                   IF  SYOKIKSN-IN-SANTEISTYMD(1:6) <= "200603"
                       MOVE   "190067010"   TO  SYOKIKSN-IN-KHNSRYCD
                   ELSE
                       MOVE   "190121210"   TO  SYOKIKSN-IN-KHNSRYCD
                   END-IF
               WHEN    "13"
                       MOVE   "190075910"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "14"
                       MOVE   "190075810"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "15"
                       MOVE   "190813410"   TO  SYOKIKSN-IN-KHNSRYCD
             END-EVALUATE
           END-IF
      *    病室
           IF  PTNYUINRRK-TOKUTEI-KBN   =   "3"
             EVALUATE    PTNYUINRRK-TOKUTEI-NYUIN
               WHEN    "01"
                       MOVE   "190075710"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "02"
                       MOVE   "190774110"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "03"
                       MOVE   "190024510"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "04"
                       MOVE   "190110210"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "05"
                       MOVE   "190110310"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "06"
                       MOVE   "190024310"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "07"
                       MOVE   "190110410"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "08"
                       MOVE   "190110510"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "09"
                       MOVE   "190024610"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "10"
                       MOVE   "190024710"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "11"
                       MOVE   "190066710"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "12"
                       MOVE   "190066810"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "13"
                       MOVE   "190024810"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "14"
                       MOVE   "190075510"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "15"
                       MOVE   "190788810"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "16"
                       MOVE   "190116310"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "17"
                       MOVE   "190117310"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "18"
                       MOVE   "190117410"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "19"
                       MOVE   "190120810"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "20"
                       MOVE   "190076710"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "21"
                       MOVE   "190076810"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "22"
                       MOVE   "190799410"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "23"
                       MOVE   "190808910"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "24"
                       MOVE   "190799510"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "25"
                       MOVE   "190121310"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "26"
                       MOVE   "190121410"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "27"
                       MOVE   "190121510"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "28"
                       MOVE   "190121610"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "29"
                       MOVE   "190121710"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "30"
                       MOVE   "190121810"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "31"
                       MOVE   "190122010"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "32"
                       MOVE   "190122110"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "33"
                       MOVE   "190122210"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "34"
                       MOVE   "190122310"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "35"
                       MOVE   "190122410"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "36"
                       MOVE   "190122510"   TO  SYOKIKSN-IN-KHNSRYCD
             END-EVALUATE
           END-IF
      *
           .
       4801-BTU-TOKTEI-NYUIN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料一般コードから老人コードへの置き換え処理
      *****************************************************************
       4801-RJNCD-CHG-SEC   SECTION.
      *
      *    老人対象患者チェック
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                                   TO  WRK-AGECHK-SRYYMD
           PERFORM 4801-RJN-TAISYO-CHK-SEC
           IF    ( FLG-RJN         =   1 )    AND
                 ( COMB-HKNNUM     NOT =   "971" )  AND
                 ( COMB-HKNNUM     NOT =   "973" )
      *
      *        平成１８年４月改定対応
      *        老人コード置き換えは療養病棟、療養病床のみ
               IF   SYOKIKSN-IN-SANTEISTYMD(1:6)  >=  "200604"
                   IF   SYOKIKSN-IN-KHNSRYCD  =  "190080610" OR
                                                 "190080710" OR
                                                 "190081310" OR
                                                 "190081410" OR
                                                 "190098210" OR
                                                 "190098310"
                       INITIALIZE              SRYCDCHG-REC
                       MOVE    SYOKIKSN-IN-KHNSRYCD
                                           TO   CHG-IPNSRYCD
                       MOVE    SRYCDCHG-REC    TO   MCPDATA-REC
                       MOVE    "SRYCDCHG-KEY2" TO   WRK-DBPATH
                       PERFORM 900-TBL-SELECT-SEC 
                       IF    ( FLG-DBRC    =   ZERO )
                           MOVE    MCPDATA-REC
                                               TO   SRYCDCHG-REC
                           IF   CHG-RJNSRYCD   NOT =   SPACE
                                MOVE   CHG-RJNSRYCD
                                               TO  SYOKIKSN-IN-KHNSRYCD
                           END-IF
                       END-IF
      *
                       MOVE    "SRYCDCHG-KEY2" TO   WRK-DBPATH
                       PERFORM 900-TBL-CLOSE-SEC
                   END-IF
               ELSE
                   INITIALIZE              SRYCDCHG-REC
                   MOVE    SYOKIKSN-IN-KHNSRYCD
                                           TO   CHG-IPNSRYCD
                   MOVE    SRYCDCHG-REC    TO   MCPDATA-REC
                   MOVE    "SRYCDCHG-KEY2" TO   WRK-DBPATH
                   PERFORM 900-TBL-SELECT-SEC 
                   IF    ( FLG-DBRC    =   ZERO )
                       MOVE    MCPDATA-REC
                                           TO   SRYCDCHG-REC
                       IF   CHG-RJNSRYCD   NOT =   SPACE
                            MOVE   CHG-RJNSRYCD
                                           TO  SYOKIKSN-IN-KHNSRYCD
                       END-IF
                   END-IF
      *
                   MOVE    "SRYCDCHG-KEY2" TO   WRK-DBPATH
                   PERFORM 900-TBL-CLOSE-SEC
               END-IF
           END-IF
      *
           .
       4801-RJNCD-CHG-EXT.
           EXIT.
      *****************************************************************
      *    老人対象患者チェック
      *****************************************************************
       4801-RJN-TAISYO-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-RJN
      *
      *    入院患者の老人保険判定
           MOVE    ZERO                TO   FLG-HKNCOMBI
           INITIALIZE              HKNCOMBI-REC
      *
           MOVE    SPA-HOSPID          TO   COMB-HOSPID
           MOVE    SPA-NAI-I01-PTID
                                       TO   COMB-PTID
           IF      SPA-GMN-I01-SYORIKBN-CD =   "07"  OR  "09"
               MOVE    WRK-TAIHI-HKN   TO   COMB-HKNCOMBINUM
           ELSE
               MOVE    SPA-GMN-I01-HKNCOMBI-CD
                                       TO   COMB-HKNCOMBINUM
           END-IF
           MOVE    HKNCOMBI-REC        TO   MCPDATA-REC
           MOVE    "HKNCOMBI-KEY"      TO   WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC 
           IF    ( FLG-DBRC    =   ZERO )
               MOVE    MCPDATA-REC
                                       TO   HKNCOMBI-REC
           ELSE
               MOVE    1               TO   FLG-HKNCOMBI
           END-IF
      *
           MOVE    "HKNCOMBI-KEY"      TO   WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
      *    入院患者の年齢計算
           INITIALIZE ORCSAGECHKAREA
           MOVE    SPA-HOSPID          TO  AGECHK-HOSPID
           MOVE    SPA-NAI-I01-PTID
                                       TO  AGECHK-PTID
           MOVE    WRK-AGECHK-SRYYMD
                                       TO  AGECHK-SRYYMD
           MOVE    SPA-NAI-I01-BIRTHDAY
                                       TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING
                                           ORCSAGECHKAREA
           IF    ( AGECHK-RC       =   ZERO )
               CONTINUE
           ELSE
               INITIALIZE  ORCSAGECHKAREA
           END-IF
      *
      *    老人対象患者判定
           IF    ((FLG-HKNCOMBI     =   ZERO)    AND
                  (COMB-KOH1HKNNUM  =   "027"))      OR
                 ( AGECHK-NENREI   >=    75)         OR
                 ((AGECHK-NENREI   >=    70)     AND
                  (PTINF-BIRTHDAY  <=   "19320930")) OR
                  (AGECHK-ROUJIN    =    1)
               MOVE    1            TO  FLG-RJN
           END-IF
      *
      *    患者公費情報から老人保健の登録チェック
           INITIALIZE                    PTKOHINF-REC
           MOVE    SPA-HOSPID       TO   PTKOH-HOSPID
           MOVE    SPA-NAI-I01-PTID TO   PTKOH-PTID
           MOVE    "027"            TO   PTKOH-KOHNUM
           MOVE    WRK-AGECHK-SRYYMD
                                    TO   PTKOH-TEKSTYMD
           MOVE    WRK-AGECHK-SRYYMD
                                    TO   PTKOH-TEKEDYMD
           MOVE    PTKOHINF-REC     TO   MCPDATA-REC
           MOVE    "PTKOHINF-KEY6"  TO   WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC 
           IF    ( FLG-DBRC    =   ZERO )
               MOVE    1            TO  FLG-RJN
           END-IF
           MOVE    "PTKOHINF-KEY6"  TO   WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
       4801-RJN-TAISYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者情報更新処理
      *****************************************************************
       4801-PTINF-UPD-SEC          SECTION.
      *
           INITIALIZE                      LASTRRK-REC
      *
           MOVE    ZERO                TO  FLG-ZI-SAISIN
      *    入院歴より最新の歴情報を取得
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPID          TO  PTNYUINRRK-HOSPID
           MOVE    SPA-NAI-I01-PTID    TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "PTNYUINRRK-KEY3"   TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
      *
           PERFORM UNTIL ( FLG-DBRC        NOT =   ZERO )
                   OR    ( FLG-ZI-SAISIN   NOT =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
      *
               IF    ( PTNYUINRRK-JTIKBN   =   "5" OR "6" )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-ZI-SAISIN
                   MOVE    PTNYUINRRK-REC
                                       TO  LASTRRK-REC
               END-IF
      *
               MOVE    "PTNYUINRRK-KEY3"   TO  WRK-DBPATH
               PERFORM 900-TBL-READ-SEC
           END-PERFORM
      *
           MOVE    "PTNYUINRRK-KEY3"   TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
      *    患者マスタ検索
           MOVE    ZERO                TO  FLG-PTINF
           INITIALIZE  PTINF-REC
           MOVE    SPA-HOSPID          TO  PTINF-HOSPID
           MOVE    SPA-NAI-I01-PTID    TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *
           MOVE    "PTINF-KEY"         TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF     ( FLG-DBRC           =  ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
      *    患者マスタクローズ
           MOVE    "PTINF-KEY"         TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF    ( FLG-PTINF           NOT =   ZERO )
               MOVE    "4026"          TO  SPA-ERRCD
               GO  TO  4801-PTINF-UPD-EXT
           END-IF
      *
      *    患者情報の入院歴番号を更新する
           IF    ( FLG-ZI-SAISIN       =   1 )
               MOVE   LASTRRK-RRKNUM   TO  PTINF-RRKNUM
               MOVE   LASTRRK-RRKEDANUM
                                       TO  PTINF-RRKEDANUM
           ELSE
               MOVE    ZERO            TO  PTINF-RRKNUM
                                           PTINF-RRKEDANUM
           END-IF
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  PTINF-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTINF-UPHMS
      *
      *    患者情報更新
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "PTINF-KEY"         TO  WRK-DBPATH
           PERFORM 900-TBL-UPDATE-SEC
           IF     ( FLG-DBRC       NOT =  ZERO )
               MOVE    "4027"          TO  SPA-ERRCD
               GO  TO  4801-PTINF-UPD-EXT
           END-IF
      *
      *    PERFORM 800-ORCSSYUKAMAIN-SEC
      *
           .
       4801-PTINF-UPD-EXT.
           EXIT.
      *****************************************************************
      *    診療会計テーブルチェック処理
      *****************************************************************
       490-SRYACCT-CHK-SEC             SECTION.
      *
           MOVE    SPA-NAI-I01-TAIINYMD
                                       TO  WRK-TAIINYMD
      *
           MOVE    ZERO                TO  FLG-SRYACCT
      *
           INITIALIZE                      SRYACCT-REC
      *
           MOVE    SPA-HOSPID          TO  ACCT-HOSPID
           MOVE    "1"                 TO  ACCT-NYUGAIKBN
           MOVE    SPA-NAI-I01-PTID    TO  ACCT-PTID
           MOVE    WRK-TAIINYM         TO  ACCT-SRYYM
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "SRYACCT-KEY15"     TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC 
           IF    ( FLG-DBRC            NOT =   ZERO )
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           PERFORM UNTIL ( FLG-SRYACCT     NOT =   ZERO )
                   OR    ( SPA-I0IDCD      NOT =   SPACE )
      *
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
      *
               IF    ( ACCT-SRYCDTOTAL =   120001110
                  OR                       120000410 )
                   CONTINUE
               ELSE
                   IF    ( WRK-TAIINYM     =   ACCT-SRYYM )
                       COMPUTE IDX14       =   WRK-TAIINDD +   1
                   ELSE
                       COMPUTE IDX14       =   1
                   END-IF
      *
                   PERFORM UNTIL ( IDX14               >   31 )
                           OR    ( SPA-I0IDCD      NOT =   SPACE )
      *
                       IF    ( ACCT-DAY (1 IDX14)      >   ZERO )
                           MOVE    "3004"          TO  SPA-I0IDCD
                           IF   SPA-GMN-I01-SYORIKBN-CD   =  "11"
                               MOVE  "3011"          TO  SPA-I0IDCD
                           END-IF
                       END-IF
      *
                       COMPUTE IDX14       =   IDX14   +   1
      *
                   END-PERFORM
               END-IF
      *
               IF    ( SPA-I0IDCD      =   SPACE )
                   MOVE    "SRYACCT-KEY15"     TO  WRK-DBPATH
                   PERFORM 900-TBL-READ-SEC 
                   IF    ( FLG-DBRC            NOT =   ZERO )
                       MOVE    1               TO  FLG-SRYACCT
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    "SRYACCT-KEY15"     TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC 
      *
           .
       490-SRYACCT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    負担金計算処理
      *****************************************************************
       490-FTNKIN-CALC-SEC             SECTION.
      *
      *    通算日数を求める
           MOVE    WRK-FTN-NYUINYMD
                                   TO  WRK-STYMD
      *
      *    請求開始日（入院日または前回定期請求終了日の翌日）を設定
           MOVE    WRK-NOW-SKYSTYMD
                                   TO  WRK-EDYMD
           PERFORM 5002-KIKAN-CALC-SEC
           IF    ( FLG-ORCSDAY     NOT =   ZERO )
               MOVE    "2008"      TO  SPA-ERRCD
               GO  TO  490-FTNKIN-CALC-EXT
           END-IF
      *
           MOVE    SPACE           TO  LNK-ORCS02-REC
                                       LNK-SYUNOU-REC-T
                                       LNK-SYUDAY-REC-T
      *
           INITIALIZE                  LNK-ORCS02-REC
                                       LNK-SYUNOU-REC-T
                                       LNK-SYUDAY-REC-T
      *
           MOVE    SPA-HOSPID      TO  LNK-S02-HOSPID
           MOVE    "1"             TO  LNK-S02-NYUGAIKBN
           MOVE    SPA-NAI-I01-PTID
                                   TO  LNK-S02-PTID
           MOVE    WRK-NISSU1      TO  LNK-S02-NYUIN-TSUSAN-NISSU
      *
      *    請求開始日（入院日（同日再入院の場合は入院日翌日）または
      *    前回定期請求終了日の翌日）を設定
           MOVE    WRK-NOW-SKYSTYMD
                                       TO  LNK-S02-SKYSTYMD
           MOVE    WRK-FTN-TAIINYMD
                                   TO  LNK-S02-SKYEDYMD
           MOVE    SPA-SYSYMD      TO  LNK-S02-SYSYMD
           MOVE    "1"             TO  LNK-S02-SYORI-KBN
           CALL    "ORCSNYUFTN"    USING
                                   SPA-AREA
                                   LNK-ORCS02-REC
                                   LNK-SYUNOU-REC-T
                                   LNK-SYUDAY-REC-T
           IF    ( LNK-S02-RC      NOT = ZERO )
               EVALUATE LNK-S02-RC
               WHEN    13
                   MOVE    "2012"      TO  SPA-ERRCD
                   GO  TO  490-FTNKIN-CALC-EXT
               WHEN    OTHER
                   MOVE    "2008"      TO  SPA-ERRCD
                   GO  TO  490-FTNKIN-CALC-EXT
               END-EVALUATE
           END-IF
      *
           PERFORM VARYING IDX11   FROM    1   BY  1
                   UNTIL ( IDX11   >   10 )
      *
               DISPLAY "LNK-SYU-SKYMONEY (" IDX11" ) = "
                        LNK-SYU-SKYMONEY ( IDX11 )
      *
           END-PERFORM
      *    収納データ退避処理
           PERFORM 490-SYUNOU-TAIHI-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  490-FTNKIN-CALC-EXT
           END-IF
      *
           .
       490-FTNKIN-CALC-EXT.
           EXIT.
      *****************************************************************
      *    収納データ退避処理
      *****************************************************************
       490-SYUNOU-TAIHI-SEC            SECTION.
      *
      *    保険組合せ取得
           PERFORM 490-HKNCOMBI-GET-SEC
      *
           INITIALIZE  SPA-I0-SYUNOU-TBL
           MOVE    LOW-VALUE       TO  WRK-SKYSTYM
           MOVE    SPACE           TO  SUMSYUNOU-REC
                                       TTLSYUNOU-REC
           INITIALIZE                  SUMSYUNOU-REC
                                       TTLSYUNOU-REC
                                       CNT-TUKISU
      *
           PERFORM VARYING IDX11   FROM    1   BY  1
                   UNTIL ( IDX11           >   10 )
                   OR    ( SUMSYUNOU-CNT    >=  13 )
                   OR    ( LNK-SYU-SKYSTYMD (IDX11)    =   SPACE )
      *
               IF    ( LNK-SYU-SKYSTYMD (IDX11) (1 : 6)
                                   NOT =   WRK-SKYSTYM )
      *
                   MOVE    LNK-SYU-SKYSTYMD (IDX11) (1 : 6)
                                   TO      WRK-SKYSTYM
                   COMPUTE SUMSYUNOU-CNT
                                       =   SUMSYUNOU-CNT +   1
                   MOVE    SUMSYUNOU-CNT
                                   TO  WRK-SYU-SUM-IDX
      *
                   MOVE    1       TO    SPA-I0-SYUNOU-TTLDAT-FLG
                                                 (WRK-SYU-SUM-IDX)
      *
                   MOVE    LNK-SYU-SKYSTYMD (IDX11)
                                   TO   WRK-YMD
                   PERFORM 5002-HIZUKE-CHK-SEC
                   MOVE    WRK-HENYMDG (1 : 6)
                           TO SPA-I0-SYUNOU-YM-W (WRK-SYU-SUM-IDX)
                   MOVE    WRK-SYMD    (1 : 6)
                           TO SPA-I0-SYUNOU-YM-S (WRK-SYU-SUM-IDX)
      *
                   MOVE    ZERO    TO    SPA-I0-SYUNOU-HKNCOMBI-SU
                                                 (WRK-SYU-SUM-IDX)
      *
                   COMPUTE CNT-TUKISU  =   CNT-TUKISU  +   1
      *
               END-IF
      *
               IF    ( SUMSYUNOU-CNT <   13 )
                   COMPUTE SUMSYUNOU-CNT    =   SUMSYUNOU-CNT +   1
      *
                   MOVE    0       TO    SPA-I0-SYUNOU-TTLDAT-FLG
                                                 (SUMSYUNOU-CNT)
      *
                   MOVE    SPA-I0-SYUNOU-YM-W (WRK-SYU-SUM-IDX)
                                       TO  SPA-I0-SYUNOU-YM-W
                                                 (SUMSYUNOU-CNT)
                   MOVE    SPA-I0-SYUNOU-YM-S (WRK-SYU-SUM-IDX)
                                       TO  SPA-I0-SYUNOU-YM-S
                                                 (SUMSYUNOU-CNT)
      *
      *            保険組み合わせ名称取得
                   MOVE    LNK-SYU-HKNCOMBINUM (IDX11)
                                           TO   WRK-ORCSHKN-HKNCOMBINUM
                   PERFORM 490-HKNCOMBI-EDIT-SEC
                   MOVE    WRK-HKNCOMBI
                                           TO  SPA-I0-SYUNOU-HKNCOMBI
                                                        (SUMSYUNOU-CNT)
      *
                   COMPUTE SPA-I0-SYUNOU-HKNCOMBI-SU (WRK-SYU-SUM-IDX)
                       =   SPA-I0-SYUNOU-HKNCOMBI-SU (WRK-SYU-SUM-IDX)
                       +   1
      *
                   MOVE    LNK-SYUNOU-REC (IDX11)
                                       TO  SUMSYUNOU-OCC (SUMSYUNOU-CNT)
                   MOVE    LNK-SYUDAY-REC (IDX11)
                                       TO  SUMSYUDAY-OCC (SUMSYUNOU-CNT)
      *            保険料
                   PERFORM VARYING     IDX12   FROM    1   BY  1
                           UNTIL       IDX12   >   12
                     COMPUTE SUMSYU-HKNTEN   (WRK-SYU-SUM-IDX IDX12)
                         =   SUMSYU-HKNTEN   (WRK-SYU-SUM-IDX IDX12)
                         +   LNK-SYU-HKNTEN (IDX11 IDX12)
                     COMPUTE TTLSYU-HKNTEN (IDX12)
                         =   TTLSYU-HKNTEN (IDX12)
                         +   LNK-SYU-HKNTEN (IDX11 IDX12)
      *
                     COMPUTE SUMSYU-TGMONEY   (WRK-SYU-SUM-IDX IDX12)
                         =   SUMSYU-TGMONEY   (WRK-SYU-SUM-IDX IDX12)
                         +   LNK-SYU-TGMONEY (IDX11 IDX12)
                     COMPUTE TTLSYU-TGMONEY  (IDX12)
                         =   TTLSYU-TGMONEY  (IDX12)
                         +   LNK-SYU-TGMONEY (IDX11 IDX12)
      *
                     COMPUTE SUMSYU-TGMONEY-TAX (WRK-SYU-SUM-IDX IDX12)
                         =   SUMSYU-TGMONEY-TAX (WRK-SYU-SUM-IDX IDX12)
                         +   LNK-SYU-TGMONEY-TAX (IDX11 IDX12)
                     COMPUTE TTLSYU-TGMONEY-TAX  (IDX12)
                         =   TTLSYU-TGMONEY-TAX  (IDX12)
                         +   LNK-SYU-TGMONEY-TAX (IDX11 IDX12)
                   END-PERFORM
      *
      *            保険適用金額
                   COMPUTE SUMSYU-HKNTEKMONEY   (WRK-SYU-SUM-IDX)
                       =   SUMSYU-HKNTEKMONEY   (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-HKNTEKMONEY (IDX11)
                   COMPUTE TTLSYU-HKNTEKMONEY
                       =   TTLSYU-HKNTEKMONEY
                       +   LNK-SYU-HKNTEKMONEY (IDX11)
      *
      *            老人一部負担金
                   COMPUTE SUMSYU-RJN-FTN    (WRK-SYU-SUM-IDX)
                       =   SUMSYU-RJN-FTN    (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-RJN-FTN  (IDX11)
                   COMPUTE TTLSYU-RJN-FTN
                       =   TTLSYU-RJN-FTN
                       +   LNK-SYU-RJN-FTN  (IDX11)
      *
      *            公費一部負担金
                   COMPUTE SUMSYU-KOH-FTN    (WRK-SYU-SUM-IDX)
                       =   SUMSYU-KOH-FTN    (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-KOH-FTN  (IDX11)
                   COMPUTE TTLSYU-KOH-FTN
                       =   TTLSYU-KOH-FTN
                       +   LNK-SYU-KOH-FTN  (IDX11)
      *
      *            自費
                   COMPUTE SUMSYU-JIHI-1   (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-1   (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-1 (IDX11)
                   COMPUTE TTLSYU-JIHI-1
                       =   TTLSYU-JIHI-1
                       +   LNK-SYU-JIHI-1 (IDX11)
      *
                   COMPUTE SUMSYU-JIHI-2   (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-2   (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-2 (IDX11)
                   COMPUTE TTLSYU-JIHI-2
                       =   TTLSYU-JIHI-2
                       +   LNK-SYU-JIHI-2 (IDX11)
      *
                   COMPUTE SUMSYU-JIHI-3   (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-3   (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-3 (IDX11)
                   COMPUTE TTLSYU-JIHI-3
                       =   TTLSYU-JIHI-3
                       +   LNK-SYU-JIHI-3 (IDX11)
      *
                   COMPUTE SUMSYU-JIHI-4   (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-4   (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-4 (IDX11)
                   COMPUTE TTLSYU-JIHI-4
                       =   TTLSYU-JIHI-4
                       +   LNK-SYU-JIHI-4 (IDX11)
      *
                   COMPUTE SUMSYU-JIHI-5   (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-5   (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-5 (IDX11)
                   COMPUTE TTLSYU-JIHI-5
                       =   TTLSYU-JIHI-5
                       +   LNK-SYU-JIHI-5 (IDX11)
      *
                   COMPUTE SUMSYU-JIHI-6   (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-6   (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-6 (IDX11)
                   COMPUTE TTLSYU-JIHI-6
                       =   TTLSYU-JIHI-6
                       +   LNK-SYU-JIHI-6 (IDX11)
      *
                   COMPUTE SUMSYU-JIHI-7   (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-7   (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-7 (IDX11)
                   COMPUTE TTLSYU-JIHI-7
                       =   TTLSYU-JIHI-7
                       +   LNK-SYU-JIHI-7 (IDX11)
      *
                   COMPUTE SUMSYU-JIHI-8   (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-8   (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-8 (IDX11)
                   COMPUTE TTLSYU-JIHI-8
                       =   TTLSYU-JIHI-8
                       +   LNK-SYU-JIHI-8 (IDX11)
      *
                   COMPUTE SUMSYU-JIHI-9   (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-9   (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-9 (IDX11)
                   COMPUTE TTLSYU-JIHI-9
                       =   TTLSYU-JIHI-9
                       +   LNK-SYU-JIHI-9 (IDX11)
      *
                   COMPUTE SUMSYU-JIHI-10  (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-10  (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-10 (IDX11)
                   COMPUTE TTLSYU-JIHI-10
                       =   TTLSYU-JIHI-10
                       +   LNK-SYU-JIHI-10 (IDX11)
      *
      *            自費計
                   COMPUTE SUMSYU-JIHI-TOTAL   (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-TOTAL   (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-TOTAL (IDX11)
                   COMPUTE TTLSYU-JIHI-TOTAL
                       =   TTLSYU-JIHI-TOTAL
                       +   LNK-SYU-JIHI-TOTAL (IDX11)
      *
      *            自費（消費税あり）
                   COMPUTE SUMSYU-JIHI-1-TAX    (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-1-TAX    (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-1-TAX  (IDX11)
                   COMPUTE TTLSYU-JIHI-1-TAX
                       =   TTLSYU-JIHI-1-TAX
                       +   LNK-SYU-JIHI-1-TAX  (IDX11)
      *
                   COMPUTE SUMSYU-JIHI-2-TAX    (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-2-TAX    (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-2-TAX  (IDX11)
                   COMPUTE TTLSYU-JIHI-2-TAX
                       =   TTLSYU-JIHI-2-TAX
                       +   LNK-SYU-JIHI-2-TAX  (IDX11)
      *
                   COMPUTE SUMSYU-JIHI-3-TAX    (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-3-TAX    (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-3-TAX  (IDX11)
                   COMPUTE TTLSYU-JIHI-3-TAX
                       =   TTLSYU-JIHI-3-TAX
                       +   LNK-SYU-JIHI-3-TAX  (IDX11)
      *
                   COMPUTE SUMSYU-JIHI-4-TAX    (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-4-TAX    (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-4-TAX  (IDX11)
                   COMPUTE TTLSYU-JIHI-4-TAX
                       =   TTLSYU-JIHI-4-TAX
                       +   LNK-SYU-JIHI-4-TAX  (IDX11)
      *
                   COMPUTE SUMSYU-JIHI-5-TAX    (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-5-TAX    (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-5-TAX  (IDX11)
                   COMPUTE TTLSYU-JIHI-5-TAX
                       =   TTLSYU-JIHI-5-TAX
                       +   LNK-SYU-JIHI-5-TAX  (IDX11)
      *
                   COMPUTE SUMSYU-JIHI-6-TAX    (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-6-TAX    (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-6-TAX  (IDX11)
                   COMPUTE TTLSYU-JIHI-6-TAX
                       =   TTLSYU-JIHI-6-TAX
                       +   LNK-SYU-JIHI-6-TAX  (IDX11)
      *
                   COMPUTE SUMSYU-JIHI-7-TAX    (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-7-TAX    (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-7-TAX  (IDX11)
                   COMPUTE TTLSYU-JIHI-7-TAX
                       =   TTLSYU-JIHI-7-TAX
                       +   LNK-SYU-JIHI-7-TAX  (IDX11)
      *
                   COMPUTE SUMSYU-JIHI-8-TAX    (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-8-TAX    (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-8-TAX  (IDX11)
                   COMPUTE TTLSYU-JIHI-8-TAX
                       =   TTLSYU-JIHI-8-TAX
                       +   LNK-SYU-JIHI-8-TAX  (IDX11)
      *
                   COMPUTE SUMSYU-JIHI-9-TAX    (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-9-TAX    (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-9-TAX  (IDX11)
                   COMPUTE TTLSYU-JIHI-9-TAX
                       =   TTLSYU-JIHI-9-TAX
                       +   LNK-SYU-JIHI-9-TAX  (IDX11)
      *
                   COMPUTE SUMSYU-JIHI-10-TAX    (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-10-TAX    (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-10-TAX  (IDX11)
                   COMPUTE TTLSYU-JIHI-10-TAX
                       =   TTLSYU-JIHI-10-TAX
                       +   LNK-SYU-JIHI-10-TAX  (IDX11)
      *
      *            自費計（消費税あり）
                   COMPUTE SUMSYU-JIHI-TOTAL-TAX (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-TOTAL-TAX (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-TOTAL-TAX (IDX11)
                   COMPUTE TTLSYU-JIHI-TOTAL-TAX
                       =   TTLSYU-JIHI-TOTAL-TAX
                       +   LNK-SYU-JIHI-TOTAL-TAX (IDX11)
      *
      *            自費消費税
                   COMPUTE SUMSYU-JIHI-TAX  (WRK-SYU-SUM-IDX)
                       =   SUMSYU-JIHI-TAX  (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-JIHI-TAX  (IDX11)
                   COMPUTE TTLSYU-JIHI-TAX
                       =   TTLSYU-JIHI-TAX
                       +   LNK-SYU-JIHI-TAX  (IDX11)
      *
      *            労災初診
                   COMPUTE SUMSYU-RSISHOSHIN-MONEY (WRK-SYU-SUM-IDX)
                       =   SUMSYU-RSISHOSHIN-MONEY (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-RSISHOSHIN-MONEY (IDX11)
                   COMPUTE TTLSYU-RSISHOSHIN-MONEY
                       =   TTLSYU-RSISHOSHIN-MONEY
                       +   LNK-SYU-RSISHOSHIN-MONEY (IDX11)
      *
      *            労災再診
                   COMPUTE SUMSYU-RSISAISHIN-MONEY (WRK-SYU-SUM-IDX)
                       =   SUMSYU-RSISAISHIN-MONEY (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-RSISAISHIN-MONEY (IDX11)
                   COMPUTE TTLSYU-RSISAISHIN-MONEY
                       =   TTLSYU-RSISAISHIN-MONEY
                       +   LNK-SYU-RSISAISHIN-MONEY (IDX11)
      *
      *            労災指導
                   COMPUTE SUMSYU-RSISDO-MONEY  (WRK-SYU-SUM-IDX)
                       =   SUMSYU-RSISDO-MONEY  (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-RSISDO-MONEY (IDX11)
                   COMPUTE TTLSYU-RSISDO-MONEY
                       =   TTLSYU-RSISDO-MONEY
                       +   LNK-SYU-RSISDO-MONEY (IDX11)
      *
      *            労災その他
                   COMPUTE SUMSYU-RSIETC-MONEY (WRK-SYU-SUM-IDX)
                       =   SUMSYU-RSIETC-MONEY (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-RSIETC-MONEY (IDX11)
                   COMPUTE TTLSYU-RSIETC-MONEY
                       =   TTLSYU-RSIETC-MONEY
                       +   LNK-SYU-RSIETC-MONEY (IDX11)
      *
      *            食事療養費（保険）
                   COMPUTE SUMSYU-RYOYOHI-SKJ (WRK-SYU-SUM-IDX)
                       =   SUMSYU-RYOYOHI-SKJ (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-RYOYOHI-SKJ (IDX11)
                   COMPUTE TTLSYU-RYOYOHI-SKJ
                       =   TTLSYU-RYOYOHI-SKJ
                       +   LNK-SYU-RYOYOHI-SKJ (IDX11)
      *
      *            食事負担額（保険：自己負担）
                   COMPUTE SUMSYU-SKYMONEY-SKJ (WRK-SYU-SUM-IDX)
                       =   SUMSYU-SKYMONEY-SKJ (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-SKYMONEY-SKJ (IDX11)
                   COMPUTE TTLSYU-SKYMONEY-SKJ
                       =   TTLSYU-SKYMONEY-SKJ
                       +   LNK-SYU-SKYMONEY-SKJ (IDX11)
      *
      *            食事負担額（保険：自己負担消費税）
                   COMPUTE SUMSYU-SKYMONEY-SKJ-TAX (WRK-SYU-SUM-IDX)
                       =   SUMSYU-SKYMONEY-SKJ-TAX (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-SKYMONEY-SKJ-TAX (IDX11)
                   COMPUTE TTLSYU-SKYMONEY-SKJ-TAX
                       =   TTLSYU-SKYMONEY-SKJ-TAX
                       +   LNK-SYU-SKYMONEY-SKJ-TAX (IDX11)
      *
      *            食事負担額（保険：自己負担合計）
                   COMPUTE SUMSYU-SKYMONEY-SKJ-KEI (WRK-SYU-SUM-IDX)
                       =   SUMSYU-SKYMONEY-SKJ-KEI (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-SKYMONEY-SKJ-KEI (IDX11)
                   COMPUTE TTLSYU-SKYMONEY-SKJ-KEI
                       =   TTLSYU-SKYMONEY-SKJ-KEI
                       +   LNK-SYU-SKYMONEY-SKJ-KEI (IDX11)
      *
      *            食事療養費（自費）
                   COMPUTE SUMSYU-RYOYOHI-SKJ-JIHI (WRK-SYU-SUM-IDX)
                       =   SUMSYU-RYOYOHI-SKJ-JIHI (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-RYOYOHI-SKJ-JIHI (IDX11)
                   COMPUTE TTLSYU-RYOYOHI-SKJ-JIHI
                       =   TTLSYU-RYOYOHI-SKJ-JIHI
                       +   LNK-SYU-RYOYOHI-SKJ-JIHI (IDX11)
      *
      *            食事負担額（自費：自己負担）
                   COMPUTE SUMSYU-SKYMONEY-SKJ-JIHI (WRK-SYU-SUM-IDX)
                       =   SUMSYU-SKYMONEY-SKJ-JIHI (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-SKYMONEY-SKJ-JIHI (IDX11)
                   COMPUTE TTLSYU-SKYMONEY-SKJ-JIHI
                       =   TTLSYU-SKYMONEY-SKJ-JIHI
                       +   LNK-SYU-SKYMONEY-SKJ-JIHI (IDX11)
      *
      *            食事負担額（自費：自己負担消費税）
                   COMPUTE SUMSYU-SKYMONEY-SKJ-JIHI-TAX
                                                    (WRK-SYU-SUM-IDX)
                       =   SUMSYU-SKYMONEY-SKJ-JIHI-TAX
                                                    (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-SKYMONEY-SKJ-JIHI-TAX (IDX11)
                   COMPUTE TTLSYU-SKYMONEY-SKJ-JIHI-TAX
                       =   TTLSYU-SKYMONEY-SKJ-JIHI-TAX
                       +   LNK-SYU-SKYMONEY-SKJ-JIHI-TAX (IDX11)
      *
      *            食事負担額（自費：自己負担合計）
                   COMPUTE SUMSYU-SKYMONEY-SKJ-JIHI-KEI
                                                    (WRK-SYU-SUM-IDX)
                       =   SUMSYU-SKYMONEY-SKJ-JIHI-KEI
                                                    (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-SKYMONEY-SKJ-JIHI-KEI (IDX11)
                   COMPUTE TTLSYU-SKYMONEY-SKJ-JIHI-KEI
                       =   TTLSYU-SKYMONEY-SKJ-JIHI-KEI
                       +   LNK-SYU-SKYMONEY-SKJ-JIHI-KEI (IDX11)
      *
      *            室料差額
                   COMPUTE SUMSYU-RMSAGAKU (WRK-SYU-SUM-IDX)
                       =   SUMSYU-RMSAGAKU (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-RMSAGAKU (IDX11)
                   COMPUTE TTLSYU-RMSAGAKU
                       =   TTLSYU-RMSAGAKU
                       +   LNK-SYU-RMSAGAKU (IDX11)
      *
      *            請求金額
                   COMPUTE SUMSYU-SKYMONEY (WRK-SYU-SUM-IDX)
                       =   SUMSYU-SKYMONEY (WRK-SYU-SUM-IDX)
                       +   LNK-SYU-SKYMONEY (IDX11)
                   COMPUTE TTLSYU-SKYMONEY
                       =   TTLSYU-SKYMONEY
                       +   LNK-SYU-SKYMONEY (IDX11)
      *
               END-IF
      *
           END-PERFORM
      *
      *    ゼロ件の場合はエラーとする
           IF    ( SUMSYUNOU-CNT    <=  ZERO )
               MOVE    "2008"      TO  SPA-ERRCD
               GO  TO  490-SYUNOU-TAIHI-EXT
           END-IF
      *
           COMPUTE SUMSYUNOU-CNT    =   SUMSYUNOU-CNT +   1
      *
      *    テーブル最大件数オーバー時はエラーとする
           IF    ( SUMSYUNOU-CNT    >   13 )
               MOVE    "2008"      TO  SPA-ERRCD
               GO  TO  490-SYUNOU-TAIHI-EXT
           END-IF
      *
      *
           MOVE    SUMSYUNOU-CNT    TO  SPA-I0-SYUNOU-CNT
      *
           MOVE    2               TO  SPA-I0-SYUNOU-TTLDAT-FLG
                                                   (SUMSYUNOU-CNT)
           MOVE    "999999"        TO  SPA-I0-SYUNOU-YM-S
                                                   (SUMSYUNOU-CNT)
           MOVE    "999999"        TO  SPA-I0-SYUNOU-YM-W
                                                   (SUMSYUNOU-CNT)
           MOVE    CNT-TUKISU      TO  SPA-I0-SYUNOU-TUKI-SU
                                                   (SUMSYUNOU-CNT)
           MOVE    TTLSYUNOU-REC   TO  SUMSYUNOU-OCC (SUMSYUNOU-CNT)
      *
           PERFORM VARYING IDX11   FROM    1   BY  1
                   UNTIL ( IDX11   >   SPA-I0-SYUNOU-CNT )
      *
               MOVE    SUMSYUNOU-OCC (IDX11)
                                   TO  SPA-I0-SYUNOU-DAT (IDX11)
               MOVE    SUMSYUDAY-OCC (IDX11)
                                   TO  SPA-I0-SYUDAY-DAT (IDX11)
      *
           END-PERFORM
      *
           .
       490-SYUNOU-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ取得処理
      *****************************************************************
       490-HKNCOMBI-GET-SEC            SECTION.
      *
           INITIALIZE                  SPA-KYOTUKEY
                                       SPA-KYOTU-SET
                                       ORCSHKNSETAREA
      *
           MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
           MOVE    "1"                 TO  SPA-NYUGAIKBN
           MOVE    SPA-NAI-I01-PTID    TO  SPA-PTID
           MOVE    SPA-NAI-I01-BIRTHDAY
                                       TO  SPA-BIRTHDAY
           MOVE    "2"                 TO  ORCSHKN-KBN
      *
           CALL    "ORCSHKNSET"        USING   SPA-AREA
                                               ORCSHKNSETAREA
           IF    ( ORCSHKN-ERRCD   NOT =   ZERO )
               INITIALIZE              ORCSHKNSETAREA
               GO  TO  490-HKNCOMBI-GET-EXT
           END-IF
      *
           .
       490-HKNCOMBI-GET-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       490-HKNCOMBI-EDIT-SEC           SECTION.
      *
           MOVE    SPACE                   TO  WRK-HKNCOMBI
           PERFORM VARYING IDX15   FROM 1  BY  1
                   UNTIL ( IDX15   >   ORCSHKN-HKN-MAX )
                     OR  ( IDX15   >   100 )
      *
               IF    ( ORCSHKN-GMN-HKNCOMBINUM (IDX15)
                                   =   WRK-ORCSHKN-HKNCOMBINUM )
                   MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX15)
                                           TO  WRK-HKNCOMBI
                   MOVE    ORCSHKN-HKN-MAX TO  IDX15
               END-IF
      *
           END-PERFORM
      *
           .
       490-HKNCOMBI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    請求確認画面遷移処理
      *****************************************************************
       490-SYUNOU-SUM-SEC              SECTION.
      *
      *
       490-SYUNOU-SUM-EXT.
           EXIT.
      *****************************************************************
      *    退院処理
      *****************************************************************
       490-TAIIN-SYORI-SEC             SECTION.
      *
      *    負担金計算処理
           MOVE    SPA-NAI-I01-NYUINYMD    TO  WRK-FTN-NYUINYMD
           MOVE    SPA-NAI-I01-TAIINYMD    TO  WRK-FTN-TAIINYMD
           MOVE    SPA-NAI-I01-DJKBN       TO  WRK-FTN-DJKBN
           PERFORM 490-FTNKIN-CALC-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  490-TAIIN-SYORI-EXT
           END-IF
      *
      *    請求確認画面遷移処理
           MOVE    "1"             TO  WRK-I01-I04-PROCKBN
           PERFORM 490-I04-SENI-SEC
           .
      *
       490-TAIIN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    退院処理
      *****************************************************************
       490-TAIIN-SAIKEISAN-SYORI-SEC   SECTION.
      *
      *    請求開始日取得処理
           MOVE    SPA-NAI-I01-TNYUINYMD (IDX1)
                                           TO  WRK-FTN-NYUINYMD
           MOVE    SPA-NAI-I01-TRRKNUM (IDX1)
                                           TO  WRK-FTN-RRKNUM
           MOVE    SPA-NAI-I01-TDJKBN (IDX1)
                                           TO  WRK-FTN-DJKBN
           PERFORM 4901-SKYSTYMD-GET-SEC
      *
      *    負担金計算処理
           MOVE    SPA-NAI-I01-TNYUINYMD (IDX1)
                                           TO  WRK-FTN-NYUINYMD
           MOVE    SPA-NAI-I01-TTAIINYMD (IDX1)
                                           TO  WRK-FTN-TAIINYMD
           PERFORM 490-FTNKIN-CALC-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  490-TAIIN-SAIKEISAN-SYORI-EXT
           END-IF
      *
      *    請求確認画面遷移処理
           MOVE    "2"             TO  WRK-I01-I04-PROCKBN
           PERFORM 490-I04-SENI-SEC
           .
      *
       490-TAIIN-SAIKEISAN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    請求確認画面遷移処理
      *****************************************************************
       490-I04-SENI-SEC                SECTION.
      *
           INITIALIZE  SPA-I01-I04-AREA
           MOVE    WRK-I01-I04-PROCKBN TO  SPA-I01-I04-PROCKBN
           IF    ( WRK-I01-I04-PROCKBN =   "1" )
               MOVE    SPA-NAI-I01-PTRRKNUM
                                           TO  SPA-I01-I04-RRKNUM
               MOVE    SPA-NAI-I01-PTRRKEDANUM
                                           TO  SPA-I01-I04-RRKEDANUM
               MOVE    SPA-NAI-I01-MAEIDOYMD
                                           TO  SPA-I01-I04-KJNYMD
               MOVE    SPA-GMN-I01-HKNCOMBI-CD
                                           TO  SPA-I01-I04-HKNCOMBI
               MOVE    SPA-GMN-I01-NYUINKA-CD
                                           TO  SPA-I01-I04-NYUINKA
               MOVE    SPA-NAI-I01-NYUINYMD
                                           TO  SPA-I01-I04-NYUINYMD
               MOVE    SPA-NAI-I01-TAIINYMD
                                           TO  SPA-I01-I04-TAIINYMD
               MOVE    SPA-GMN-I01-TAIINCD
                                           TO  SPA-I01-I04-TAIINCD
               MOVE    WRK-NOW-SKYSTYMD    TO  SPA-I01-I04-SKYSTYMD
               MOVE    SPA-NAI-I01-TAIINYMD
                                           TO  SPA-I01-I04-SKYEDYMD
               MOVE    SPA-NAI-I01-TORIKESI-FLG
                                           TO  SPA-I01-I04-TORIKESI-FLG
           ELSE
               PERFORM 4901-PTNYUINRRK-SEL-SEC
               MOVE    PTNYUINRRK-RRKNUM   TO  SPA-I01-I04-RRKNUM
               MOVE    PTNYUINRRK-RRKEDANUM
                                           TO  SPA-I01-I04-RRKEDANUM
               MOVE    PTNYUINRRK-TENNYUYMD
                                           TO  SPA-I01-I04-KJNYMD
               MOVE    PTNYUINRRK-HKNCOMBINUM
                                           TO  SPA-I01-I04-HKNCOMBI
               MOVE    PTNYUINRRK-NYUINKA  TO  SPA-I01-I04-NYUINKA
               MOVE    PTNYUINRRK-NYUINYMD TO  SPA-I01-I04-NYUINYMD
               MOVE    PTNYUINRRK-TAIINYMD TO  SPA-I01-I04-TAIINYMD
               MOVE    PTNYUINRRK-TAIINCD  TO  SPA-I01-I04-TAIINCD
      *
               MOVE    WRK-NOW-SKYSTYMD    TO  SPA-I01-I04-SKYSTYMD
      *
               MOVE    PTNYUINRRK-TAIINYMD TO  SPA-I01-I04-SKYEDYMD
           END-IF
      *
           MOVE    "I01"               TO  SPA-MOTOPG
           MOVE    "I04"               TO  SPA-SAKIPG
      *
           MOVE   WRK-MCP-PUTTYPE      TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
      *
       490-I04-SENI-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理（退院再計算処理）
      *****************************************************************
       4901-PTNYUINRRK-SEL-SEC         SECTION.
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPID          TO  PTNYUINRRK-HOSPID
           MOVE    SPA-NAI-I01-PTID    TO  PTNYUINRRK-PTID
           MOVE    SPA-NAI-I01-TRRKNUM (SPA-GMN-I01-NYUREKILST-SEL)
                                       TO  PTNYUINRRK-RRKNUM
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "PTNYUINRRK-KEY4"   TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC            =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           MOVE    "PTNYUINRRK-KEY4"   TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
           .
      *
       4901-PTNYUINRRK-SEL-EXT.
           EXIT.
      *****************************************************************
      *    請求開始日取得処理（退院再計算処理）
      *****************************************************************
       4901-SKYSTYMD-GET-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-NOW-SKYSTYMD
      *
           INITIALIZE                      PTTEIKIRRK-REC
           MOVE    SPA-HOSPID          TO  PTTEIKIRRK-HOSPID
           MOVE    SPA-NAI-I01-PTID    TO  PTTEIKIRRK-PTID
           MOVE    WRK-FTN-RRKNUM      TO  PTTEIKIRRK-RRKNUM
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "PTTEIKIRRK-KEY7"   TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC            =   ZERO )
               MOVE    MCPDATA-REC     TO  PTTEIKIRRK-REC
               MOVE    PTTEIKIRRK-SKYEDYMD
                                       TO  WRK-YMD
               MOVE    "1"             TO  WRK-ZOGENPTN
               MOVE    1               TO  WRK-ZOGEN
               PERFORM 5002-CALENDAR-SEC
               MOVE    WRK-KEISANYMD   TO  WRK-NOW-SKYSTYMD
           END-IF
      *
           MOVE    "PTTEIKIRRK-KEY7"   TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF    ( WRK-NOW-SKYSTYMD    =   SPACE )
               MOVE    WRK-FTN-NYUINYMD
                                       TO  WRK-NOW-SKYSTYMD
               IF    ( WRK-FTN-DJKBN   =   "1" )
                   MOVE    WRK-FTN-NYUINYMD
                                       TO  WRK-YMD
                   MOVE    "1"         TO  WRK-ZOGENPTN
                   MOVE    1           TO  WRK-ZOGEN
                   PERFORM 5002-CALENDAR-SEC
                   MOVE    WRK-KEISANYMD
                                       TO  WRK-NOW-SKYSTYMD
               END-IF
           END-IF
      *
           .
       4901-SKYSTYMD-GET-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-I0IDCD      NOT =   SPACE )
               PERFORM 520-I0IDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-I0IDCD2 NOT =   SPACE )
               PERFORM 520-I0ID2SET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "I01    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
      *    患者番号
           MOVE    SPA-GMN-I01-PTNUM       TO  I01-PTNUM
      *    患者氏名
           MOVE    SPA-GMN-I01-NAME        TO  I01-NAME
      *    患者性別
           MOVE    SPA-GMN-I01-SEX         TO  I01-SEX
      *    患者誕生日
           MOVE    SPA-GMN-I01-BIRTHDAY    TO  I01-BIRTHDAY
      *    患者年齢
           MOVE    SPA-GMN-I01-AGE         TO  I01-AGE
      *    処理区分
           MOVE    SPA-GMN-I01-SYORIKBN    TO  I01-SYORIKBN
           MOVE    SPA-GMN-I01-SYORIKBN-CNT
                                           TO  I01-SYORIKBN-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I01-SYORIKBN-CNT )
               MOVE    SPA-GMN-I01-SYORIKBN-ITM (IDX1)
                                           TO  I01-SYORIKBN-ITM (IDX1)
           END-PERFORM
      *
      *    異動日
           MOVE    SPA-GMN-I01-IDOYMD      TO  I01-IDOYMD
      *
      *    前回異動日
           MOVE    SPA-GMN-I01-MAEIDOYMD   TO  I01-MAEIDOYMD
      *
      *    病室番号
           MOVE    SPA-GMN-I01-BRMNUM      TO  I01-BRMNUM
      *    病棟名
           MOVE    SPA-GMN-I01-BTUNAME     TO  I01-BTUNAME
           MOVE    SPA-GMN-I01-BTUNAME-CNT TO  I01-BTUNAME-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I01-BTUNAME-CNT )
               MOVE    SPA-GMN-I01-BTUNAME-ITM (IDX1)
                                           TO  I01-BTUNAME-ITM (IDX1)
           END-PERFORM
      *
      *    室料差額
           MOVE    SPA-GMN-I01-SAGAKU      TO  I01-SAGAKU
           MOVE    SPA-GMN-I01-SAGAKU-CNT  TO  I01-SAGAKU-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I01-SAGAKU-CNT )
               MOVE    SPA-GMN-I01-SAGAKU-ITM (IDX1)
                                           TO  I01-SAGAKU-ITM (IDX1)
           END-PERFORM
      *
      *    入院日
           MOVE    SPA-GMN-I01-NYUINYMD    TO  I01-NYUINYMD
      *    入院科
           MOVE    SPA-GMN-I01-NYUINKA     TO  I01-NYUINKA
           MOVE    SPA-GMN-I01-NYUINKA-CNT TO  I01-NYUINKA-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I01-NYUINKA-CNT )
               MOVE    SPA-GMN-I01-NYUINKA-ITM (IDX1)
                                           TO  I01-NYUINKA-ITM (IDX1)
           END-PERFORM
      *
      *    初回
           MOVE    SPA-GMN-I01-SHOKAI      TO  I01-SHOKAI
           MOVE    SPA-GMN-I01-SHOKAI-CNT  TO  I01-SHOKAI-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I01-SHOKAI-CNT )
               MOVE    SPA-GMN-I01-SHOKAI-ITM (IDX1)
                                           TO  I01-SHOKAI-ITM (IDX1)
           END-PERFORM
      *
      *    初回番号
           MOVE    SPA-GMN-I01-SHONUM      TO  I01-SHONUM
      *
      *    初回入院日
           MOVE    SPA-GMN-I01-LBL-SHONYUINYMD
                                           TO  I01-LBL-SHONYUINYMD
      *
      *    退院日
           MOVE    SPA-GMN-I01-TAIINYMD    TO  I01-TAIINYMD
      *
      *    退院事由
           MOVE    SPA-GMN-I01-TAIINCD     TO  I01-TAIINCD
           MOVE    SPA-GMN-I01-TAIINCD-CNT TO  I01-TAIINCD-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I01-TAIINCD-CNT )
               MOVE    SPA-GMN-I01-TAIINCD-ITM (IDX1)
                                           TO  I01-TAIINCD-ITM (IDX1)
           END-PERFORM
      *
      *    担当医
           PERFORM VARYING IDX1   FROM     1   BY  1
               UNTIL     ( IDX1   >    3   )
               MOVE    SPA-GMN-I01-DR (IDX1)
                                           TO  I01-DR (IDX1)
               MOVE    SPA-GMN-I01-DR-CNT (IDX1)
                                           TO  I01-DR-CNT (IDX1)
                                               WRK-CNT
               PERFORM VARYING IDX2   FROM     1   BY  1
                   UNTIL     ( IDX2   >    WRK-CNT )
                   MOVE    SPA-GMN-I01-DR-ITM (IDX1 IDX2)
                                   TO  I01-DR-ITM (IDX1 IDX2)
               END-PERFORM
           END-PERFORM
      *
      *    保険組合わせ
           MOVE    SPA-GMN-I01-HKNCOMBI    TO  I01-HKNCOMBI
           MOVE    SPA-GMN-I01-HKNCOMBI-CNT
                                           TO  I01-HKNCOMBI-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I01-HKNCOMBI-CNT   )
               MOVE    SPA-GMN-I01-HKNCOMBI-ITM (IDX1)
                                           TO  I01-HKNCOMBI-ITM (IDX1)
           END-PERFORM
      *
           IF    ( SPA-NAI-I01-SELNYUKHN   =   ZERO )
               MOVE    "特定入院料"        TO  I01-LBL-TOKNYUIN
           ELSE
               MOVE    "入院料選択"        TO  I01-LBL-TOKNYUIN
           END-IF
      *
      *    特定入院料
           MOVE    SPA-GMN-I01-TOKNYUIN    TO  I01-TOKNYUIN
           MOVE    SPA-GMN-I01-TOKNYUIN-CNT
                                           TO  I01-TOKNYUIN-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I01-TOKNYUIN-CNT   )
               MOVE    SPA-GMN-I01-TOKNYUIN-ITM (IDX1)
                                           TO  I01-TOKNYUIN-ITM (IDX1)
           END-PERFORM
      *
      *    算定入院料
           MOVE    SPA-GMN-I01-SNTNYUIN    TO  I01-SNTNYUIN
      *
      *    前受け金
      *    MOVE    SPA-GMN-I01-MAEUKE      TO  I01-MAEUKE
      *
      *    定期請求
           MOVE    SPA-GMN-I01-SEIKYU      TO  I01-SEIKYU
           MOVE    SPA-GMN-I01-SEIKYU-CNT  TO  I01-SEIKYU-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I01-SEIKYU-CNT   )
               MOVE    SPA-GMN-I01-SEIKYU-ITM (IDX1)
                                           TO  I01-SEIKYU-ITM (IDX1)
           END-PERFORM
      *
      *    検索時患者表示
           MOVE    SPA-GMN-I01-DISPKBN     TO  I01-DISPKBN
           MOVE    SPA-GMN-I01-DISPKBN-CNT TO  I01-DISPKBN-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I01-DISPKBN-CNT   )
               MOVE    SPA-GMN-I01-DISPKBN-ITM (IDX1)
                                           TO  I01-DISPKBN-ITM (IDX1)
           END-PERFORM
      *
      *    選択番号
           MOVE    SPA-GMN-I01-SELNUM      TO  I01-SELNUM
      *
      *    入院歴リスト
           MOVE    SPA-GMN-I01-NYUREKILST-CNT
                                           TO  I01-NYUREKILST-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL ( IDX1    >   SPA-GMN-I01-NYUREKILST-CNT )
               MOVE    SPA-GMN-I01-TSELNUM (IDX1)
                                       TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9        TO  I01-TSELNUM (IDX1)
               IF    ( SPA-GMN-I01-TSHOKAI (IDX1)  =   "1" )
                   MOVE    "◎"        TO  I01-TSHOKAI (IDX1)
               ELSE
                   MOVE    SPACE       TO  I01-TSHOKAI (IDX1)
               END-IF
               MOVE    SPA-GMN-I01-TSHONUM (IDX1)
                                       TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9
                                       TO  I01-TSHONUM (IDX1)
               MOVE    SPA-GMN-I01-TBTUNAME (IDX1)
                                       TO  I01-TBTUNAME (IDX1)
               MOVE    SPA-GMN-I01-TBRMNUM (IDX1)
                                       TO  I01-TBRMNUM (IDX1)
               MOVE    SPA-GMN-I01-TNYUINYMD (IDX1)
                                       TO  I01-TNYUINYMD (IDX1)
               MOVE    SPA-GMN-I01-TTAIINYMD (IDX1)
                                       TO  I01-TTAIINYMD (IDX1)
               MOVE    SPA-GMN-I01-TNYUINKA (IDX1)
                                       TO  I01-TNYUINKA (IDX1)
               MOVE    SPA-GMN-I01-TBYOMEI (IDX1)
                                       TO  I01-TBYOMEI (IDX1)
               MOVE    SPA-GMN-I01-TTENKI (IDX1)
                                       TO  I01-TTENKI (IDX1)
               MOVE    SPA-GMN-I01-TNISSU (IDX1)
                                       TO  I01-TNISSU (IDX1)
               MOVE    SPA-GMN-I01-TTUSAN (IDX1)
                                       TO  I01-TTUSAN (IDX1)
      *
               IF    ( SPA-GMN-I01-NYUREKILST-SEL  =   IDX1)
                   MOVE    "T"         TO  I01-NYUREKILST-SELECT (IDX1)
               ELSE
                   MOVE    "F"         TO  I01-NYUREKILST-SELECT (IDX1)
               END-IF
      *
           END-PERFORM
      *
           EVALUATE    FLG-NYUREKILST-ROW
           WHEN    1
               MOVE    ZERO            TO  I01-NYUREKILST-ROW
               MOVE    ZERO            TO  I01-NYUREKILST-ROWATTR
           WHEN    OTHER
               MOVE    SPA-GMN-I01-NYUREKILST-SEL
                                       TO  I01-NYUREKILST-ROW
               MOVE    2               TO  I01-NYUREKILST-ROWATTR
           END-EVALUATE
      *
      *    項目状態編集
           PERFORM 500-GMNHEN-STATE-EDIT-SEC
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目状態編集処理
      *****************************************************************
       500-GMNHEN-STATE-EDIT-SEC       SECTION.
      *
      *    処理区分応じて画面編集の内容を変更する
           EVALUATE    SPA-GMN-I01-SYORIKBN-CD
      *    入院登録
           WHEN    "01"
               PERFORM 4101-WIDGET-NYUIN-STT-SEC
      *    退院登録
           WHEN    "02"
           WHEN    "11"
      *
               PERFORM 4101-WIDGET-TAIIN-STT-SEC
      *
      *    変更
           WHEN    "03"
               PERFORM 4101-WIDGET-HENKOU-STT-SEC
      *    照会
           WHEN    "04"
               PERFORM 4201-WIDGET-INSENSITIVE-SEC
      *    転科　転棟　転室
           WHEN    "08"
      *
               PERFORM 4101-WIDGET-TENKA-STT-SEC
      *
      *    退院再計算
           WHEN    "10"
               PERFORM 4201-WIDGET-INSENSITIVE-SEC
               MOVE    WIDGET-NORMAL   TO  SPA-GMN-I01-B12-STT
      *    その他
           WHEN    OTHER
               PERFORM 4201-WIDGET-INSENSITIVE-SEC
               MOVE    WIDGET-NORMAL   TO  SPA-GMN-I01-B12-STT
           END-EVALUATE
      *
           IF    ( SPA-NAI-I01-PTID    =   ZERO )
               PERFORM 4201-WIDGET-STATE-CLEAR-SEC
           END-IF
      *
           IF    ( SPA-GMN-I01-SYORIKBN-CD =   "01" )
            AND  ( SPA-GMN-I01-SHOKAI-CD   =   "2" )
               MOVE    WIDGET-NORMAL   TO  SPA-GMN-I01-SHONUM-STT
           END-IF
      *
      *    入院歴作成ボタン
           MOVE    SPA-GMN-I01-B07-STT
                                       TO  I01-B07-STT
      *    入院歴修正ボタン
           MOVE    SPA-GMN-I01-B08-STT
                                       TO  I01-B08-STT
      *    登録ボタン
           MOVE    SPA-GMN-I01-B12-STT
                                       TO  I01-B12-STT
      *
      *    入院オーダー遷移
           MOVE    WIDGET-INSENSITIVE  TO  I01-B24-STATE
           IF    ( SPA-GMN-I01-ORDERKBN    =   "1" )
               MOVE    "オーダー"      TO  I01-B24-LABEL
               MOVE    WIDGET-NORMAL   TO  I01-B24-STATE
           END-IF
      *
      *    異動日
           MOVE    SPA-GMN-I01-IDOYMD-STT
                                       TO  I01-IDOYMD-STT
      *    病室番号
           MOVE    SPA-GMN-I01-BRMNUM-STT
                                       TO  I01-BRMNUM-STT
      *    病棟名
           MOVE    SPA-GMN-I01-BTUNAME-STT
                                       TO  I01-BTUNAME-STT
           MOVE    SPA-GMN-I01-BTUNAME-ITM-STT
                                       TO  I01-BTUNAME-ITM-STT
      *    室料差額
           MOVE    SPA-GMN-I01-SAGAKU-STT
                                       TO  I01-SAGAKU-STT
           MOVE    SPA-GMN-I01-SAGAKU-ITM-STT
                                       TO  I01-SAGAKU-ITM-STT
      *    入院日
           MOVE    SPA-GMN-I01-NYUINYMD-STT
                                       TO  I01-NYUINYMD-STT
      *    入院科
           MOVE    SPA-GMN-I01-NYUINKA-STT
                                       TO  I01-NYUINKA-STT
           MOVE    SPA-GMN-I01-NYUINKA-ITM-STT
                                       TO  I01-NYUINKA-ITM-STT
      *    初回
           MOVE    SPA-GMN-I01-SHOKAI-STT
                                       TO  I01-SHOKAI-STT
           MOVE    SPA-GMN-I01-SHOKAI-ITM-STT
                                       TO  I01-SHOKAI-ITM-STT
      *    初回番号
           MOVE    SPA-GMN-I01-SHONUM-STT
                                       TO  I01-SHONUM-STT
      *    退院日
           MOVE    SPA-GMN-I01-TAIINYMD-STT
                                       TO  I01-TAIINYMD-STT
      *    退院事由
           MOVE    SPA-GMN-I01-TAIINCD-STT
                                       TO  I01-TAIINCD-STT
           MOVE    SPA-GMN-I01-TAIINCD-ITM-STT
                                       TO  I01-TAIINCD-ITM-STT
      *    担当医
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   3   )
               MOVE    SPA-GMN-I01-DR-STT (IDX1)
                                       TO  I01-DR-STT (IDX1)
               MOVE    SPA-GMN-I01-DR-ITM-STT (IDX1)
                                       TO  I01-DR-ITM-STT (IDX1)
           END-PERFORM
      *    保険組合わせ
           MOVE    SPA-GMN-I01-HKNCOMBI-STT
                                       TO  I01-HKNCOMBI-STT
           MOVE    SPA-GMN-I01-HKNCOMBI-ITM-STT
                                       TO  I01-HKNCOMBI-ITM-STT
      *    特定入院料
           MOVE    SPA-GMN-I01-TOKNYUIN-STT
                                       TO  I01-TOKNYUIN-STT
           MOVE    SPA-GMN-I01-TOKNYUIN-ITM-STT
                                       TO  I01-TOKNYUIN-ITM-STT
      *    算定入院料
           MOVE    SPA-GMN-I01-SNTNYUIN-STT
                                       TO  I01-SNTNYUIN-STT
      *    前受け金
      *    MOVE    SPA-GMN-I01-MAEUKE-STT
      *                                TO  I01-MAEUKE-STT
      *    定期請求
           MOVE    SPA-GMN-I01-SEIKYU-STT
                                       TO  I01-SEIKYU-STT
           MOVE    SPA-GMN-I01-SEIKYU-ITM-STT
                                       TO  I01-SEIKYU-ITM-STT
      *    検索時患者表示
           MOVE    SPA-GMN-I01-DISPKBN-STT
                                       TO  I01-DISPKBN-STT
           MOVE    SPA-GMN-I01-DISPKBN-ITM-STT
                                       TO  I01-DISPKBN-ITM-STT
      *業務処理権限設定
      *    入院会計照会
           IF      SPA-GSRAUTH-32      =   "1"
               MOVE    WIDGET-NORMAL       TO  I01-B21-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  I01-B21-STATE
           END-IF
      *    患者登録
           IF      SPA-GSRAUTH-12      =   "1"
               MOVE    WIDGET-NORMAL       TO  I01-B05-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  I01-B05-STATE
           END-IF
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC              SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-I01-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   1   )
                   MOVE    "PTNUM"         TO  MCP-WIDGET
                   GO  TO  5001-MAPCUR-EXT
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   2   )
                   MOVE    "SYORIKBN"      TO  MCP-WIDGET
                   GO  TO  5001-MAPCUR-EXT
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   3   )
               IF  ( SPA-GMN-I01-IDOYMD-STT    =   WIDGET-NORMAL )
                   MOVE    "IDOYMD"        TO  MCP-WIDGET
                   GO  TO  5001-MAPCUR-EXT
               ELSE
                   COMPUTE SPA-GMN-I01-CUR =   4
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   4   )
               IF  ( SPA-GMN-I01-BRMNUM-STT    =   WIDGET-NORMAL )
                   MOVE    "BRMNUM"        TO  MCP-WIDGET
                   GO  TO  5001-MAPCUR-EXT
               ELSE
                   COMPUTE SPA-GMN-I01-CUR =   5
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   5   )
               IF  ( SPA-GMN-I01-BTUNAME-STT   =   WIDGET-NORMAL )
                   MOVE    "BTUNAME"       TO  MCP-WIDGET
                   GO  TO  5001-MAPCUR-EXT
               ELSE
                   COMPUTE SPA-GMN-I01-CUR =   6
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   6   )
               IF  ( SPA-GMN-I01-SAGAKU-STT    =   WIDGET-NORMAL )
                   MOVE    "SAGAKU"        TO  MCP-WIDGET
                   GO  TO  5001-MAPCUR-EXT
               ELSE
                   COMPUTE SPA-GMN-I01-CUR =   7
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   7   )
               IF  ( SPA-GMN-I01-NYUINYMD-STT  =   WIDGET-NORMAL )
                   MOVE    "NYUINYMD"      TO  MCP-WIDGET
                   GO  TO  5001-MAPCUR-EXT
               ELSE
                   COMPUTE SPA-GMN-I01-CUR =   8
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   8   )
               IF  ( SPA-GMN-I01-NYUINKA-STT   =   WIDGET-NORMAL )
                   MOVE    "NYUINKA"       TO  MCP-WIDGET
                   GO  TO  5001-MAPCUR-EXT
               ELSE
                   COMPUTE SPA-GMN-I01-CUR =   9
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   9   )
               IF  ( SPA-GMN-I01-SHOKAI-STT    =   WIDGET-NORMAL )
                   MOVE    "SHOKAI"        TO  MCP-WIDGET
                   GO  TO  5001-MAPCUR-EXT
               ELSE
                   COMPUTE SPA-GMN-I01-CUR =   10
               END-IF
           END-IF
      *
      *    初回選択番号
           IF    ( SPA-GMN-I01-CUR =   10  )
               IF  ( SPA-GMN-I01-SHONUM-STT    =   WIDGET-NORMAL )
                   MOVE    "SHONUM"        TO  MCP-WIDGET
                   GO  TO  5001-MAPCUR-EXT
               ELSE
                   COMPUTE SPA-GMN-I01-CUR =   11
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   11  )
               IF  ( SPA-GMN-I01-TAIINYMD-STT  =   WIDGET-NORMAL )
                   MOVE    "TAIINYMD"      TO  MCP-WIDGET
                   GO  TO  5001-MAPCUR-EXT
               ELSE
                   COMPUTE SPA-GMN-I01-CUR =   20
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   20  )
               IF  ( SPA-GMN-I01-TAIINCD-STT   =   WIDGET-NORMAL )
                   MOVE    "TAIINCD"       TO  MCP-WIDGET
                   GO  TO  5001-MAPCUR-EXT
               ELSE
                   COMPUTE SPA-GMN-I01-CUR =   12
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   12  )
               IF  ( SPA-GMN-I01-DR-STT (1)    =   WIDGET-NORMAL )
                   MOVE    "DR1"           TO  MCP-WIDGET
                   GO  TO  5001-MAPCUR-EXT
               ELSE
                   COMPUTE SPA-GMN-I01-CUR =   13
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   13  )
               IF  ( SPA-GMN-I01-DR-STT (2)    =   WIDGET-NORMAL )
                   MOVE    "DR2"           TO  MCP-WIDGET
                   GO  TO  5001-MAPCUR-EXT
               ELSE
                   COMPUTE SPA-GMN-I01-CUR =   14
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   14  )
               IF  ( SPA-GMN-I01-DR-STT (3)    =   WIDGET-NORMAL )
                   MOVE    "DR3"           TO  MCP-WIDGET
                   GO  TO  5001-MAPCUR-EXT
               ELSE
                   COMPUTE SPA-GMN-I01-CUR =   15
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   15  )
               IF  ( SPA-GMN-I01-HKNCOMBI-STT  =   WIDGET-NORMAL )
                   MOVE    "HKNCOMBI"      TO  MCP-WIDGET
                   GO  TO  5001-MAPCUR-EXT
               ELSE
                   COMPUTE SPA-GMN-I01-CUR =   16
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   16  )
               IF  ( SPA-GMN-I01-TOKNYUIN-STT  =   WIDGET-NORMAL )
                   MOVE    "TOKNYUIN"      TO  MCP-WIDGET
                   GO  TO  5001-MAPCUR-EXT
               ELSE
                   COMPUTE SPA-GMN-I01-CUR =   17
               END-IF
           END-IF
      *
      *??? xx 暫定 xx 前受け金にはカーソルを飛ばさないようにする xx
           IF    ( SPA-GMN-I01-CUR =   17  )
               COMPUTE SPA-GMN-I01-CUR =   18
           END-IF
      *??? IF    ( SPA-GMN-I01-CUR =   17  )
      *???     IF  ( SPA-GMN-I01-MAEUKE-STT    =   WIDGET-NORMAL )
      *???         MOVE    "MAEUKE"        TO  MCP-WIDGET
      *???         GO  TO  5001-MAPCUR-EXT
      *???     ELSE
      *???         COMPUTE SPA-GMN-I01-CUR =   SPA-GMN-I01-CUR +   1
      *???     END-IF
      *??? END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   18  )
               IF  ( SPA-GMN-I01-SEIKYU-STT    =   WIDGET-NORMAL )
                   MOVE    "SEIKYU"        TO  MCP-WIDGET
                   GO  TO  5001-MAPCUR-EXT
               ELSE
                   COMPUTE SPA-GMN-I01-CUR =   19
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   19  )
               IF  ( SPA-GMN-I01-DISPKBN-STT   =   WIDGET-NORMAL )
                   MOVE    "DISPKBN"       TO  MCP-WIDGET
                   GO  TO  5001-MAPCUR-EXT
               ELSE
                   MOVE    212             TO  SPA-GMN-I01-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   100  )
               MOVE    "SELNUM"        TO  MCP-WIDGET
               GO  TO  5001-MAPCUR-EXT
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   206  )
               MOVE    "B06"           TO  MCP-WIDGET
               GO  TO  5001-MAPCUR-EXT
           END-IF
      *
           IF    ( SPA-GMN-I01-CUR =   212  )
               MOVE    "B12"           TO  MCP-WIDGET
               GO  TO  5001-MAPCUR-EXT
           END-IF
      *
           .
      *
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "IDOYMD"
               MOVE    004             TO  SPA-GMN-I01-CUR
           WHEN    "BRMNUM"
               MOVE    005             TO  SPA-GMN-I01-CUR
           WHEN    "BTUNAME"
               MOVE    006             TO  SPA-GMN-I01-CUR
           WHEN    "SAGAKU"
               MOVE    007             TO  SPA-GMN-I01-CUR
           WHEN    "NYUINYMD"
               MOVE    008             TO  SPA-GMN-I01-CUR
           WHEN    "NYUINKA"
               MOVE    009             TO  SPA-GMN-I01-CUR
           WHEN    "SHOKAI"
               MOVE    010             TO  SPA-GMN-I01-CUR
           WHEN    "SHONUM"
               MOVE    011             TO  SPA-GMN-I01-CUR
           WHEN    "TAIINYMD"
               MOVE    020             TO  SPA-GMN-I01-CUR
           WHEN    "DR1"
               MOVE    013             TO  SPA-GMN-I01-CUR
           WHEN    "DR2"
               MOVE    014             TO  SPA-GMN-I01-CUR
           WHEN    "DR3"
               MOVE    015             TO  SPA-GMN-I01-CUR
           WHEN    "HKNCOMBI"
               MOVE    016             TO  SPA-GMN-I01-CUR
           WHEN    "TOKNYUIN"
               MOVE    017             TO  SPA-GMN-I01-CUR
           WHEN    "MAEUKE"
               MOVE    018             TO  SPA-GMN-I01-CUR
           WHEN    "SEIKYU"
               MOVE    019             TO  SPA-GMN-I01-CUR
           WHEN    "DISPKBN"
               MOVE    212             TO  SPA-GMN-I01-CUR
           WHEN    "TAIINCD"
               MOVE    012             TO  SPA-GMN-I01-CUR
           WHEN    "SELNUM"
               IF    ( SPA-GMN-I01-SYORIKBN-CD =   "10" )
                   MOVE    212         TO  SPA-GMN-I01-CUR
               ELSE
                   MOVE    206         TO  SPA-GMN-I01-CUR
               END-IF
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "入力エラー"    TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "該当の患者は存在しません"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "半角文字でのみ入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "該当の病室は存在しません"
                                       TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "初歴を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "0011"
               MOVE    "患者番号入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0012"
               MOVE    "処理区分入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0013"
               MOVE    "異動日入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0014"
               MOVE    "病室番号入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0015"
               MOVE    "病棟名入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0016"
               MOVE    "室料差額入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0017"
               MOVE    "入院日入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0018"
               MOVE    "入院科入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0019"
               MOVE    "初回入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0020"
               MOVE    "初歴入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0021"
               MOVE    "退院日入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0022"
               MOVE    "担当医入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0023"
               MOVE    "保険組合せ入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0024"
               MOVE    "特定入院料入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0025"
               MOVE    "定期請求入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0026"
               MOVE    "検索時患者表示入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0027"
               MOVE    "選択番号入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "0028"
               MOVE    "退院事由入力エラー" 
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "患者番号を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1002"
               MOVE    "処理区分を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "1003"
               MOVE    "病室番号を入力して下さい"
                                       TO  SPA-ERRMSG
           WHEN    "1004"
               MOVE    "病棟名を入力して下さい"
                                       TO  SPA-ERRMSG
           WHEN    "1005"
               MOVE    "入院日を入力して下さい"
                                       TO  SPA-ERRMSG
           WHEN    "1006"
               MOVE    "入院科を入力して下さい"
                                       TO  SPA-ERRMSG
           WHEN    "1007"
               MOVE    "初回を入力して下さい"
                                       TO  SPA-ERRMSG
           WHEN    "1008"
               MOVE    "担当医を入力して下さい"
                                       TO  SPA-ERRMSG
           WHEN    "1009"
               MOVE    "保険組合せを入力して下さい"
                                       TO  SPA-ERRMSG
           WHEN    "1010"
               MOVE    "定期請求を入力して下さい"
                                       TO  SPA-ERRMSG
           WHEN    "1011"
               MOVE    "検索時患者表示を入力して下さい"
                                       TO  SPA-ERRMSG
           WHEN    "1012"
               MOVE    "退院日＜入院日となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1013"
               MOVE    "退院日＜前回異動日となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1014"
               MOVE    "異動日が入力されていません"
                                       TO  SPA-ERRMSG
           WHEN    "1015"
               MOVE    "異動日＜入院日となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1016"
               MOVE    "異動日＜前回異動日となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1017"
               MOVE    "入院科・病棟・病室・保険組合せ・特定入院"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "料のいずれかを変更してください"
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "1018"
               MOVE    "入院日＜前回退院日となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1019"
               MOVE    "入院歴を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "1020"
               MOVE    "退院日を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1021"
               MOVE    "保険組合せがありません。処理できません"
                                       TO  SPA-ERRMSG
           WHEN    "1022"
               MOVE    "継続元の歴番号を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1023"
               MOVE    "入院歴作成画面で作成した入院歴を選択して"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "ください"
                                       TO  SPA-ERRMSG (41 : )
      *    WHEN    "1024"
      *        MOVE    "選択歴より新しい歴が存在するため修正でき"
      *                                TO  SPA-ERRMSG (1 : 40)
      *        MOVE    "ません"        TO  SPA-ERRMSG (41 : )
           WHEN    "1025"
               MOVE    "定期請求が行われていないため、負担金計算"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "は行えません"  TO  SPA-ERRMSG (41 : )
           WHEN    "1026"
               MOVE    "定期請求済みの期間に該当するため、退院登"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "録は行えません" 
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "1027"
               MOVE    "入院料が指定されていません"
                                       TO  SPA-ERRMSG
           WHEN    "1028"
               MOVE    "定期請求が月末のみに変更されています。"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "月途中の入金（請求）を取消してください"
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "1029"
               MOVE    "異動月の会計テーブルが作成されていません"
                                       TO  SPA-ERRMSG
           WHEN    "1030"
               MOVE    "退院月の会計テーブルが作成されていません"
                                       TO  SPA-ERRMSG
           WHEN    "1031"
               MOVE    "前歴の自院歴が「入院中」です。前歴の継続"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "入院として登録してください"
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "1032"
               MOVE    "前歴の自院歴が「入院中」です。入院日を前"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "歴の退院日の翌日にしてください"
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "1033"
               MOVE    "労災保険（アフターケア）は入院できません"
                                       TO  SPA-ERRMSG
           WHEN    "1034"
               MOVE    "退院済の歴を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "1035"
               MOVE    "入院歴作成処理で作成した歴は処理の対象外"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "です"
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "1036"
               MOVE  "入院日≦前回退院日となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1037"
               MOVE    "同日再入院の場合、同日の退院登録は行えません"
                                       TO  SPA-ERRMSG
           WHEN    "1038"
               MOVE    "同日再入院は継続入院のみ可能です"
                                       TO  SPA-ERRMSG
           WHEN    "1039"
               MOVE    "病棟管理情報の設定を平成１８年４月１日よ"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "り新規に作成してください"
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "1040"
               MOVE    "医療機関情報−入院基本の設定を平成１８年"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE   "４月１日より新規に作成してください"
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "1041"
               MOVE    "病棟管理情報の設定を平成１８年７月１日よ"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "り新規に作成してください"
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "1042"
               MOVE    "６月の会計テーブルが作成されていません"
                                       TO  SPA-ERRMSG
           WHEN    "1043"
               MOVE    "異動日を７月１日にし、転科転棟転室処理を"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "行ってください"
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "2012"
               MOVE    "退院計算に必要な会計情報が作成されていません"
                                       TO  SPA-ERRMSG
           WHEN    "2008"
               MOVE    "負担金計算処理に失敗しました"
                                       TO  SPA-ERRMSG
           WHEN    "4001"  THRU    "4999"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
      *
           WHEN    "9300"
               MOVE    "患者登録の業務権限はありません。"
                                                TO  SPA-ERRMSG
               MOVE    "患者登録はできません。"
                                                TO  SPA-ERRMSG(33:)
           WHEN    "9999"
               MOVE    "他端末で使用中です。"
                                       TO  SPA-ERRMSG
               MOVE    SLOCK-MSG       TO  SPA-ERRMSG(21:)
           END-EVALUATE
      *
           MOVE    SPACE               TO  I0ERR
           MOVE    SPA-ERRCD           TO  I0ERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  I0ERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "I01"               TO  SPA-MOTOPG
           MOVE    "I0ERR"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "I0ERR"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-I0IDSET-SEC             SECTION.
      *
           EVALUATE    SPA-I0IDCD
           WHEN    "3001"
               MOVE    "入院情報を削除します（会計情報は残ります）"
                                       TO  WRK-I0IDMSG
           WHEN    "3002"
               MOVE    "入院情報を削除します"
                                       TO  WRK-I0IDMSG (1 : 20)
               MOVE    "（会計情報も併せて削除します）"
                                       TO  WRK-I0IDMSG (21 : )
           WHEN    "3003"
               MOVE    "退院取消を行います"
                                       TO  WRK-I0IDMSG
           WHEN    "3004"
               MOVE    SPACE           TO  WRK-I0IDMSG
               STRING  "退院後の診療日で入力された診療行為は"
                       "削除されます。確認してください"
               DELIMITED BY SIZE
               INTO  WRK-I0IDMSG
               END-STRING
           WHEN    "3005"
               MOVE    SPACE           TO  WRK-I0IDMSG
               STRING  "定期請求が月末のみに変更されています。"
                       "月途中で作成した請求データは取消します"
               DELIMITED BY SIZE
               INTO  WRK-I0IDMSG
               END-STRING
           WHEN    "3006"
               MOVE    SPACE           TO  WRK-I0IDMSG
               STRING  "入院情報（異動歴あり）を削除します"
                       "（会計情報は残ります）"
               DELIMITED BY SIZE
               INTO  WRK-I0IDMSG
               END-STRING
           WHEN    "3008"
               MOVE    SPACE           TO  WRK-I0IDMSG
               STRING  "定期請求処理中のため退院処理は行えません。"
                       "よろしいですか"
               DELIMITED BY SIZE
               INTO  WRK-I0IDMSG
               END-STRING
           WHEN    "3007"
               MOVE    SPACE           TO  WRK-I0IDMSG
               STRING  "入院情報（異動歴あり）を削除します"
                       "（会計情報も併せて削除します）"
               DELIMITED BY SIZE
               INTO  WRK-I0IDMSG
               END-STRING
           WHEN    "3009"
               MOVE    SPACE           TO  WRK-I0IDMSG
               STRING  "入院日が他院退院日と重複します。"
                       "他院歴の通算日数を減算しますか？"
               DELIMITED BY SIZE
               INTO  WRK-I0IDMSG
               END-STRING
           WHEN    "3010"
               MOVE    SPACE           TO  WRK-I0IDMSG
               STRING  "入院日が他院退院日と重複します。"
                       "よろしいですか？"
               DELIMITED BY SIZE
               INTO  WRK-I0IDMSG
               END-STRING
           WHEN    "3011"
               MOVE    SPACE           TO  WRK-I0IDMSG
               STRING  "退院後の診療日で入力された診療行為は"
                       "そのままの状態となります"
               DELIMITED BY SIZE
               INTO  WRK-I0IDMSG
               END-STRING
           END-EVALUATE
      *
      *
           MOVE    SPACE                TO  I0ID1
           MOVE    SPA-I0IDCD           TO  I0ID1-ID1CODE
           MOVE    WRK-I0IDMSG          TO  I0ID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "I01"                TO  SPA-MOTOPG
           MOVE    "I0ID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "I0ID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       520-I0IDSET-EXT.
           EXIT.
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-I0ID2SET-SEC            SECTION.
      *
           EVALUATE    SPA-I0IDCD2
           WHEN    "3101"
               MOVE    "転科（転棟・転室）歴を削除します"
                                       TO  WRK-I0IDMSG
           END-EVALUATE
      *
      *
           MOVE    SPACE               TO  I0ID2
           MOVE    SPA-I0IDCD2         TO  I0ID2-ID2CODE
           MOVE    WRK-I0IDMSG         TO  I0ID2-ID2MSG
      *
      *
           MOVE    SPA-GMN-I01-MAEIDOYMD
                                       TO  I0ID2-IDOYMD
           MOVE    SPA-GMN-I01-NYUINKA-NM
                                       TO  I0ID2-NYUINKA
      *
           MOVE    SPA-GMN-I01-BRMNUM  TO  I0ID2-BRM
      *
           MOVE    SPA-GMN-I01-BTUNAME-NM
                                       TO  I0ID2-BTU
      *
           MOVE    SPA-GMN-I01-HKNCOMBI-NM
                                       TO  I0ID2-HKNCOMBI
           MOVE    SPA-GMN-I01-SNTNYUIN
                                       TO  I0ID2-TOKNYUIN
      *
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "I01"                TO  SPA-MOTOPG
           MOVE    "I0ID2"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "I0ID2"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       520-I0ID2SET-EXT.
           EXIT.
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-HENYMDG
                                           WRK-SYMD
      *
           IF    ( WRK-YMD         =   SPACE )
               GO  TO  5002-HIZUKE-CHK-EXT
           END-IF
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF  ( SGDAY-RC              =   ZERO )
               MOVE    SGDAY-OTDAY     TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    期間算出処理
      *****************************************************************
       5002-KIKAN-CALC-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-ORCSDAY
                                           WRK-NISSU1
                                           WRK-NISSU2
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY5-AREA
           MOVE    "51"                TO  LNK-DAY5-IRAI
           MOVE    WRK-STYMD           TO  LNK-DAY5-START
           MOVE    WRK-EDYMD           TO  LNK-DAY5-END
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                           LNK-DAY5-AREA
           IF      ( STS-DAY-RC1   NOT =   ZERO )
               MOVE    1           TO  FLG-ORCSDAY
               GO  TO              5002-KIKAN-CALC-EXT
           END-IF
      *
           MOVE    LNK-DAY5-NISSU1     TO  WRK-NISSU1
           MOVE    LNK-DAY5-NISSU2     TO  WRK-NISSU2
      *
           .
       5002-KIKAN-CALC-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       5002-CALENDAR-SEC               SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       5002-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    全角半角混在チェック
      *****************************************************************
       800-ENRTRY-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-ENTRYCHK
      *
           IF  ( WRK-KANACHK-MAE-INPUT
                                   =   SPACE )
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           MOVE    SPACE           TO  ORCSKANACHKAREA
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-MAE-INPUT
                                   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-OUT-INPUT
           IF      ( KANACHK-RC    NOT =   ZERO )
               MOVE    1           TO  FLG-ENTRYCHK
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
      *    全角でない場合はエラーとする。
           IF      ( KANACHK-SYUBETU   NOT =   2 )
               MOVE    1           TO  FLG-ENTRYCHK
           END-IF
      *
           .
       800-ENRTRY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    同日再入院チェックサブ
      *****************************************************************
       800-ORCSDNTCHK-SEC              SECTION.
      *
           INITIALIZE                  SDNTCHK-AREA
           MOVE    PTNYUINRRK-REC      TO      LNKNRRK-REC
           CALL    "ORCSDNTCHK"        USING   SDNTCHK-AREA
                                               LNKNRRK-REC
      *
           .
       800-ORCSDNTCHK-EXT.
           EXIT.
      *****************************************************************
      *    日報キー取得サブ
      *****************************************************************
       800-DAILYKEY-SEC         SECTION.
      *
           CALL    "ORCSDAILYKEY"   USING   SDAILYKEYAREA
      *
           .
      *
       800-DAILYKEY-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    主科決定サブ
      *****************************************************************
       800-ORCSSYUKAMAIN-SEC           SECTION.
      *
           INITIALIZE                  ORCSSYUACCAREA
      *
           MOVE    PTINF-PTID          TO  SPA-PTID
           MOVE    "1"                 TO  SPA-NYUGAIKBN
           MOVE    PTINF-BIRTHDAY      TO  SPA-BIRTHDAY
      *
           INITIALIZE                      ORCSSYUACCAREA
           MOVE    WRK-SYUKA-KBN       TO  LNK-SYUACC-I-KBN
           MOVE    WRK-SYUKA-YMD (1:6) TO  LNK-SYUACC-I-SRYYM
           CALL    "ORCSSYUKAMAIN"     USING
                                       SPA-AREA
                                       ORCSSYUACCAREA
      *
           .
      *
       800-ORCSSYUKAMAIN-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
           INITIALIZE                  TNS-TENSU-REC
      *
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    WRK-KJNYMD          TO  ORC-DBYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      *
           MOVE    "TENSU-KEY"         TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
      *    点数マスタクローズ
           MOVE    "TENSU-KEY"         TO  ORC-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       900-TBL-SELECT-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-DBRC
           MOVE    WRK-DBPATH      TO  ORC-DBPATH
           PERFORM 910-DB-SELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    WRK-DBPATH  TO  ORC-DBPATH
               PERFORM 910-DB-READ-SEC
               IF    ( MCP-RC          =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    1       TO  FLG-DBRC
               END-IF
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       900-TBL-READ-SEC                SECTION.
      *
           MOVE    ZERO            TO  FLG-DBRC
           MOVE    WRK-DBPATH      TO  ORC-DBPATH
           PERFORM 910-DB-READ-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル更新処理
      *****************************************************************
       900-TBL-UPDATE-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-DBRC
           MOVE    WRK-DBPATH      TO  ORC-DBPATH
           PERFORM 910-DB-UPDATE-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    テーブル追加処理
      *****************************************************************
       900-TBL-INSERT-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-DBRC
           MOVE    WRK-DBPATH      TO  ORC-DBPATH
           PERFORM 910-DB-INSERT-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル削除処理
      *****************************************************************
       900-TBL-DELETE-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-DBRC
           MOVE    WRK-DBPATH      TO  ORC-DBPATH
           PERFORM 910-DB-DELETE-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-TBL-CLOSE-SEC               SECTION.
      *
           MOVE    WRK-DBPATH      TO  ORC-DBPATH
           PERFORM 910-DB-CLOSE-SEC
      *
           .
       900-TBL-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    テーブルクローズ処理（MONFUNC対応）
      *****************************************************************
       900-TBL-CLOSE2-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "MONFUNC"            USING
                                        MCPAREA
                                        MCPDATA-REC
      *
           .
       900-TBL-CLOSE2-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ検索処理(KEY7)
      *****************************************************************
       900-SYSKANRI-KEY7-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY7-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY7)
      *****************************************************************
       900-SYSKANRI-KEY7-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY7-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       910-DB-DELETE-SEC               SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
      *
       910-DB-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ更新処理
      *****************************************************************
       910-DB-UPDATE-SEC               SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
      *
       910-DB-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ追加処理
      *****************************************************************
       910-DB-INSERT-SEC               SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
      *
       910-DB-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DB-SELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DB-READ-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
      *
       910-DB-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DB-CLOSE-SEC                SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター検索
      *****************************************************************
       910-NYUINACCT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           CALL    "MONFUNC"            USING
                                        MCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター読込
      *****************************************************************
       910-NYUINACCT-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           CALL    "MONFUNC"            USING
                                        MCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  NYUINACCT-REC
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                     NYUINACCT-REC
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-NAI-I01-PTID    TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF    ( SLOCK-RETURN    NOT =   ZERO )
               MOVE    "9999"          TO  SPA-ERRCD
           END-IF
           .
       990-LOCK-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
      *    排他制御解除処理
           MOVE    2                   TO  SLOCK-MODE
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           .
       990-LOCK-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL   "ORCMCPSUB"   USING
                                MCPAREA
                                ORCMCPAREA
                                MCPDATA-REC.
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

