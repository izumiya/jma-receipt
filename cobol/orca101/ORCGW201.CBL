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
       PROGRAM-ID.                 ORCGW201.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 入院料加算設定登録
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/03/08    NACL−小豆沢  新規作成
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
      *
       WORKING-STORAGE             SECTION.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "W20SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-SELNUM          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    コンスタント値
       01  CONSTANT-AREA.
      *
      *
      *  システム管理（医療機関情報）設定加算
      *    総合入院体制加算
           03  CONS-KSN1                   PIC X(09) VALUE "190098870".
      *    地域医療支援病院入院診療加算
           03  CONS-KSN2                   PIC X(09) VALUE "190099870".
      *    診療録管理体制加算
           03  CONS-KSN3                   PIC X(09) VALUE "190100370".
      *    乳幼児加算（病院）
           03  CONS-KSN4                   PIC X(09) VALUE "190100470".
      *    乳幼児加算（病院）（特別入院基本料等）
           03  CONS-KSN5                   PIC X(09) VALUE "190100570".
      *    乳幼児加算（診療所）
           03  CONS-KSN6                   PIC X(09) VALUE "190100670".
      *    幼児加算（病院）
           03  CONS-KSN7                   PIC X(09) VALUE "190100770".
      *    幼児加算（病院）（特別入院基本料等）
           03  CONS-KSN8                   PIC X(09) VALUE "190100870".
      *    幼児加算（診療所）
           03  CONS-KSN9                   PIC X(09) VALUE "190100970".
      *    精神病棟入院時医学管理加算
           03  CONS-KSN10                  PIC X(09) VALUE "190107170".
      *    基幹型臨床研修病院入院診療加算
           03  CONS-KSN11                  PIC X(09) VALUE "190117170".
      *    協力型臨床研修病院入院診療加算
           03  CONS-KSN12                  PIC X(09) VALUE "190119910".
      *    離島加算
           03  CONS-KSN13                  PIC X(09) VALUE "190117270".
      *    医療安全対策加算１
           03  CONS-KSN14                  PIC X(09) VALUE "190120510".
      *    医療安全対策加算２
           03  CONS-KSN15                  PIC X(09) VALUE "190136910".
      *    感染防止対策加算
           03  CONS-KSN16                  PIC X(09) VALUE "190137070".
      *    栄養管理実施加算
           03  CONS-KSN17                  PIC X(09) VALUE "190120410".
      *    後発医薬品使用体制加算
           03  CONS-KSN18                  PIC X(09) VALUE "190138010".
      *    １級地地域加算
           03  CONS-KSN19                  PIC X(09) VALUE "190077070".
      *    ２級地地域加算
           03  CONS-KSN20                  PIC X(09) VALUE "190077170".
      *    ３級地地域加算
           03  CONS-KSN21                  PIC X(09) VALUE "190120010".
      *    ４級地地域加算
           03  CONS-KSN22                  PIC X(09) VALUE "190077270".
      *    ５級地地域加算
           03  CONS-KSN23                  PIC X(09) VALUE "190120110".
      *    ６級地地域加算
           03  CONS-KSN24                  PIC X(09) VALUE "190077370".
      *
      *  病院のシステム管理（病棟管理）設定加算
      *    看護補助加算１
           03  CONS-KSN25                  PIC X(09) VALUE "190103770".
      *    看護補助加算１（１３対１入院基本料）
           03  CONS-KSN26                  PIC X(09) VALUE "190103870".
      *    看護補助加算２
           03  CONS-KSN27                  PIC X(09) VALUE "190103970".
      *    看護補助加算３
           03  CONS-KSN28                  PIC X(09) VALUE "190104070".
      *    プレイルーム、保育士等加算
           03  CONS-KSN29                  PIC X(09) VALUE "190111370".
      *    難病患者等入院診療加算
           03  CONS-KSN30                  PIC X(09) VALUE "190101770".
      *    特殊疾患入院施設管理加算
           03  CONS-KSN31                  PIC X(09) VALUE "190101970".
      *    児童・思春期精神科入院医療管理加算
           03  CONS-KSN32                  PIC X(09) VALUE "190110170".
      *    精神科応急入院施設管理加算
           03  CONS-KSN33                  PIC X(09) VALUE "190106970".
      *    看護配置加算
           03  CONS-KSN34                  PIC X(09) VALUE "190102070".
      *    １５対１補助体制加算
           03  CONS-KSN35                  PIC X(09) VALUE "190135710".
      *    ２０対１補助体制加算
           03  CONS-KSN36                  PIC X(09) VALUE "190135810".
      *    ２５対１補助体制加算
           03  CONS-KSN37                  PIC X(09) VALUE "190127110".
      *    ５０対１補助体制加算
           03  CONS-KSN38                  PIC X(09) VALUE "190127210".
      *    ７５対１補助体制加算
           03  CONS-KSN39                  PIC X(09) VALUE "190127310".
      *    １００対１補助体制加算
           03  CONS-KSN40                  PIC X(09) VALUE "190127410".
      *    重症患者回復病棟加算
           03  CONS-KSN41                  PIC X(09) VALUE "190129870".
      *    急性期看護補助体制加算１
           03  CONS-KSN42                  PIC X(09) VALUE "190135910".
      *    急性期看護補助体制加算２
           03  CONS-KSN43                  PIC X(09) VALUE "190136010".
      *    精神科地域移行実施加算
           03  CONS-KSN44                  PIC X(09) VALUE "190127810".
      *    一般病棟看護必要度評価加算
           03  CONS-KSN45                  PIC X(09) VALUE "190131570".
      *    一般病棟看護必要度評価加算（専門病院）
           03  CONS-KSN46                  PIC X(09) VALUE "190134970".
      *    一般病棟看護必要度評価加算（特定機能病院）
           03  CONS-KSN47                  PIC X(09) VALUE "190134870".
      *    休日リハビリテーション提供体制加算
           03  CONS-KSN48                  PIC X(09) VALUE "190140870".
      *
      *  病院のシステム管理（病室管理）設定加算
      *    放射線治療病室管理加算
           03  CONS-KSN49                  PIC X(09) VALUE "190106670".
      *    療養環境加算
           03  CONS-KSN50                  PIC X(09) VALUE "190105570".
      *    療養病棟療養環境加算１
           03  CONS-KSN51                  PIC X(09) VALUE "190120210".
      *    療養病棟療養環境加算２
           03  CONS-KSN52                  PIC X(09) VALUE "190106070".
      *    療養病棟療養環境加算３
           03  CONS-KSN53                  PIC X(09) VALUE "190106170".
      *    療養病棟療養環境加算４
           03  CONS-KSN54                  PIC X(09) VALUE "190106270".
      *    重症者等療養環境特別加算（個室）
           03  CONS-KSN55                  PIC X(09) VALUE "190105870".
      *    重症者等療養環境特別加算（２人部屋）
           03  CONS-KSN56                  PIC X(09) VALUE "190105970".
      *    無菌治療室管理加算
           03  CONS-KSN57                  PIC X(09) VALUE "190106570".
      *
      *  診療所のシステム管理（病棟管理）設定加算
      *    難病患者等入院診療加算
           03  CONS-KSN58                  PIC X(09) VALUE "190101770".
      *    特殊疾患入院施設管理加算
           03  CONS-KSN59                  PIC X(09) VALUE "190101970".
      *    医師配置加算１
           03  CONS-KSN60                  PIC X(09) VALUE "190135570".
      *    医師配置加算２
           03  CONS-KSN61                  PIC X(09) VALUE "190117070".
      *    夜間緊急体制確保加算
           03  CONS-KSN62                  PIC X(09) VALUE "190126470".
      *    看護配置加算１
           03  CONS-KSN63                  PIC X(09) VALUE "190109670".
      *    看護配置加算２
           03  CONS-KSN64                  PIC X(09) VALUE "190126570".
      *    夜間看護配置加算１
           03  CONS-KSN65                  PIC X(09) VALUE "190126770".
      *    夜間看護配置加算２
           03  CONS-KSN66                  PIC X(09) VALUE "190126670".
      *
      *  診療所のシステム管理（病室管理）設定加算
      *    診療所療養病床療養環境加算１
           03  CONS-KSN67                  PIC X(09) VALUE "190106370".
      *    診療所療養病床療養環境加算２
           03  CONS-KSN68                  PIC X(09) VALUE "190106470".
      *    放射線治療病室管理加算
           03  CONS-KSN69                  PIC X(09) VALUE "190106670".
      *    無菌治療室管理加算
           03  CONS-KSN70                  PIC X(09) VALUE "190106570".
      *
      *    入退院登録時に算定する加算
      *    褥瘡患者管理加算
           03  CONS-KSN71                  PIC X(09) VALUE "190116990".
      *    有床診療所一般病床初期加算
           03  CONS-KSN72                  PIC X(09) VALUE "190135470".
      *    救急・在宅等支援療養病床初期加算（診療所）
           03  CONS-KSN73                  PIC X(09) VALUE "190135670".
      *    救急・在宅等支援療養病床初期加算（病院）
           03  CONS-KSN74                  PIC X(09) VALUE "190134270".
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WIDGET              PIC X(64).
           03  WRK-WIDMSG              PIC X(70).
      *
           03  WRK-SELNUM              PIC 9(03).
           03  WRK-KBNCD               PIC 9(08).
           03  WRK-ZZ9                 PIC ZZ9.
      *
      *    コラムリスト位置
           03  WRK-W201-ROW            PIC S9(09).
           03  FLG-GMN-INIT            PIC  9(01).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    病棟管理
           COPY  "CPSK5001.INC".
      *    点数マスタ
           COPY  "CPTENSU.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
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
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "W01.INC".
           COPY    "W02.INC".
           COPY    "W03.INC".
           COPY    "W04.INC".
           COPY    "W05.INC".
           COPY    "W06.INC".
           COPY    "W07.INC".
           COPY    "W08.INC".
           COPY    "W09.INC".
           COPY    "W10.INC".
           COPY    "W11.INC".
           COPY    "W111.INC".
           COPY    "W12.INC".
           COPY    "W121.INC".
           COPY    "W13.INC".
           COPY    "W14.INC".
           COPY    "W15.INC".
           COPY    "W16.INC".
           COPY    "W17.INC".
           COPY    "W18.INC".
           COPY    "W19.INC".
           COPY    "W20.INC".
           COPY    "W201.INC".
           COPY    "W21.INC".
           COPY    "W22.INC".
           COPY    "W23.INC".
           COPY    "W24.INC".
           COPY    "W25.INC".
           COPY    "W28.INC".
           COPY    "W29.INC".
           COPY    "W30.INC".
           COPY    "W31.INC".
           COPY    "W32.INC".
           COPY    "W321.INC".
           COPY    "W33.INC".
           COPY    "W34.INC".
           COPY    "W35.INC".
           COPY    "W60.INC".
           COPY    "W61.INC".
           COPY    "W62.INC".
           COPY    "W63.INC".
           COPY    "W64.INC".
           COPY    "W65.INC".
           COPY    "W66.INC".
           COPY    "W67.INC".
           COPY    "W68.INC".
           COPY    "W96.INC".
           COPY    "W98.INC".
           COPY    "W981.INC".
           COPY    "W97.INC".
           COPY    "W99.INC".
           COPY    "WERR.INC".
           COPY    "WID1.INC".
           COPY    "WID2.INC".
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W20FREE
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
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
           MOVE    SPA-W20FREE     TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "WERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
               MOVE   "NEW"                TO  MCP-PUTTYPE
               MOVE   "W201"               TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
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
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "WID1"
                   PERFORM 330-WID1-SET-SEC
               WHEN    OTHER
                       PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
      *            更新
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 2201-TOUROKU-SYORI-SEC
                   ELSE
                       MOVE    1           TO  SPA-W201-CUR
                   END-IF
               WHEN    "1003"
      *            削除
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 2301-DELETE-SYORI-SEC
                   ELSE
                       MOVE    1           TO  SPA-W201-CUR
                   END-IF
           END-EVALUATE
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD
      *
           .
       330-WID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPASET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-W201-AREA
      *
           MOVE    1                   TO  FLG-GMN-INIT
      *
           INITIALIZE                  SPA-W201-LIST-G
      *
      *    アップデート対象のデータを読み直す
           INITIALIZE                  SYSKANRI-REC
      *
           MOVE   "5001"               TO  SYS-KANRICD
           MOVE   SPA-GMN-W20-BTUNUM   TO  SYS-KBNCD
           MOVE   SPA-NAI-W20-YUKOSTYMD-OLD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF   ( FLG-SYSKANRI    NOT =   ZERO )
               MOVE    "9000"          TO  SPA-ERRCD
               GO  TO  310-SPASET-EXT
           END-IF
      *
           MOVE    SYSKANRI-REC        TO  SYS-5001-REC
      *
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL  (IDX             >=  10)
               ADD     1                   TO  IDX
               IF      SYS-5001-KSNCD2 (IDX)  NOT =   SPACE
                   MOVE    IDX             TO  SPA-W201-LIST-MAX
                   MOVE    IDX             TO  SPA-W201-TRENNUM (IDX)
                   MOVE    SYS-5001-KSNCD2 (IDX)
                                           TO  SPA-W201-TKSNCD2 (IDX)
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
                   MOVE    SYS-5001-KSNCD2 (IDX)
                                           TO  TNS-SRYCD
                   MOVE    SPA-NAI-W20-YUKOSTYMD
                                           TO  TNS-YUKOSTYMD
                   MOVE    SPA-NAI-W20-YUKOSTYMD
                                           TO  TNS-YUKOEDYMD
                   MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
                   MOVE    "tbl_tensu"     TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF      MCP-RC          =   ZERO
                       MOVE    "tbl_tensu" TO  MCP-TABLE
                       MOVE    "key"       TO  MCP-PATHNAME
                       PERFORM 900-TENSU-READ-SEC
                       IF    FLG-TENSU     =   ZERO
                           MOVE   MCPDATA-REC
                                           TO  TNS-TENSU-REC
                           MOVE   TNS-NAME TO  SPA-W201-TKSNNM2(IDX)
                       ELSE
                           MOVE   SPACE    TO  SPA-W201-TKSNNM2(IDX)
                       END-IF
                   ELSE
                       MOVE    1               TO  FLG-TENSU
                   END-IF
                   MOVE    "tbl_tensu"         TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 990-DBCLOSE-SEC
      *
               ELSE
                   MOVE    11             TO  IDX
               END-IF
           END-PERFORM
      *
           IF      SPA-W201-LIST-MAX   =   ZERO
               MOVE    2                   TO  SPA-W201-CUR
           ELSE
               MOVE    1                   TO  SPA-W201-CUR
           END-IF
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
      *    コラムリスト位置保存
           IF      FLG-GMN-INIT        =   1
               MOVE    ZERO                TO  WRK-W201-ROW
           ELSE
               MOVE    W201-MEILIST-ROW    TO  WRK-W201-ROW
           END-IF
      *
           MOVE    SPACE               TO  W201
      *    コラムリストの位置指定
           MOVE    ZERO                TO  W201-MEILIST-ROW
           IF      WRK-W201-ROW    NOT =   ZERO
               MOVE    WRK-W201-ROW    TO  W201-MEILIST-ROW 
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-W201-LIST-MAX
               MOVE    SPA-W201-TRENNUM (IDX)  TO  WRK-ZZ9
               MOVE    WRK-ZZ9             TO  W201-TRENNUM (IDX)
               MOVE    SPA-W201-TKSNCD2(IDX)
                                           TO  W201-TKSNCD2 (IDX)
               INITIALIZE                      TNS-TENSU-REC
               MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
               MOVE    SPA-W201-TKSNCD2(IDX)
                                           TO  TNS-SRYCD
               MOVE    SPA-NAI-W20-YUKOSTYMD
                                           TO  TNS-YUKOSTYMD
               MOVE    SPA-NAI-W20-YUKOSTYMD
                                           TO  TNS-YUKOEDYMD
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_tensu"     TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 900-TENSU-READ-SEC
                   IF    FLG-TENSU         =   ZERO
                       MOVE   MCPDATA-REC  TO  TNS-TENSU-REC
                           MOVE   TNS-NAME TO  SPA-W201-TKSNNM2(IDX)
                                               W201-TKSNNM2 (IDX)
                   ELSE
                       MOVE   SPACE        TO  SPA-W201-TKSNNM2(IDX)
                                               W201-TKSNNM2 (IDX)
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-TENSU
               END-IF
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
               IF      SPA-W201-SELNUM     =   SPA-W201-TRENNUM (IDX)
                   MOVE    "T"             TO  W201-MEILIST-SELECT
                                                               (IDX)
                   MOVE    IDX             TO  W201-MEILIST-ROW
                   MOVE    2               TO  W201-MEILIST-ROWATTR
               ELSE
                   MOVE    "F"             TO  W201-MEILIST-SELECT
                                                               (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-W201-LIST-MAX    TO  W201-MEILIST-COUNT
      *
           MOVE    SPA-W201-SELNUM      TO  W201-SELNUM
           MOVE    SPA-W201-KSNCD2      TO  W201-KSNCD2
           MOVE    SPA-W201-KSNNM2      TO  W201-KSNNM2
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-W201-CUR        =   ZERO
               EVALUATE    WRK-WIDGET
                   WHEN    "SELNUM"
                       MOVE    2                   TO  SPA-W201-CUR
                   WHEN    "KSNCD2"
                       MOVE    99                  TO  SPA-W201-CUR
               END-EVALUATE
           END-IF
      *
           EVALUATE    SPA-W201-CUR
               WHEN    1
                   MOVE    "SELNUM"            TO  MCP-WIDGET
               WHEN    2
                   MOVE    "KSNCD2"            TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 220-CLERA-SYORI-SEC
      *        削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 230-DELETE-SEC
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 220-TOUROKU-SEC
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
      *    入力個所のセット
           PERFORM 210-GMN-INPUT-SEC
      *
           EVALUATE    WRK-WIDGET
               WHEN    "MEILIST"
                   PERFORM 2001-MEILIST-SEC
               WHEN    OTHER
      *            入力チェック処理
                   PERFORM 220-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       210-GMN-INPUT-SEC               SECTION.
      *
           MOVE    ZERO                TO  SPA-W201-CUR
      *    入力項目の決定
           MOVE    MCP-WIDGET          TO  WRK-WIDGET
      *
           .
       210-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       220-INPUT-CHK-SEC               SECTION.
      *
      *    画面をＳＰＡへ編集
           PERFORM 2201-GMNSPA-SET-SEC
      *
           PERFORM 2002-SELNUM-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2202-KSNCD2-CHK-SEC
           END-IF
      *
           .
       220-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    リスト選択処理
      *****************************************************************
       2001-MEILIST-SEC               SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-W201-LIST-MAX
              IF      W201-MEILIST-SELECT (IDX)    =  "T"
                  IF       IDX                 =   SPA-W201-SELNUM
                      CONTINUE
                  ELSE
                      MOVE    IDX              TO  WRK-SELNUM
                  END-IF
              END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM          TO  SPA-W201-SELNUM
               MOVE    1                   TO  FLG-SELNUM
               PERFORM 2002-SELNUM-SEC
           END-IF
      *
           .
       2001-MEILIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号処理
      *****************************************************************
       2002-SELNUM-SEC               SECTION.
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-W201-SELNUM     TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN         =   "1"   )
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-W201-CUR
           ELSE
               MOVE    SNUM-OUTNUM         TO  SPA-W201-SELNUM
           END-IF
      *
           IF      (SPA-W201-SELNUM    >   ZERO)  AND
                   (SPA-ERRCD          =   SPACE) 
               MOVE    ZERO                TO  FLG-CHK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-W201-LIST-MAX ) OR
                                  (FLG-CHK =   1    )
                   IF      SPA-W201-TRENNUM (IDX)   =   SPA-W201-SELNUM
                       MOVE    1                   TO  FLG-CHK
                       IF      FLG-SELNUM          =   1
      *                    明細編集
                           PERFORM 20021-MEISAI-HENSYU-SEC
                       END-IF
                       MOVE    SPA-W201-SELNUM     TO
                                                   SPA-W201-MAE-SELNUM
                   END-IF
               END-PERFORM
               IF      FLG-CHK             =   ZERO
                   MOVE    "0001"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-W201-CUR
               END-IF
           END-IF
      *
           .
       2002-SELNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       20021-MEISAI-HENSYU-SEC               SECTION.
      *
           MOVE    SPA-W201-TRENNUM (IDX)   TO  SPA-W201-SELNUM
           MOVE    SPA-W201-TKSNCD2 (IDX)   TO  SPA-W201-KSNCD2
           MOVE    SPA-W201-TKSNNM2 (IDX)   TO  SPA-W201-KSNNM2
      *
           .
       20021-MEISAI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面項目→ＳＰＡ編集処理
      *****************************************************************
       2201-GMNSPA-SET-SEC               SECTION.
      *
           IF      W201-SELNUM         =   SPA-W201-MAE-SELNUM
               MOVE    ZERO                TO  FLG-SELNUM
           ELSE
               MOVE    1                   TO  FLG-SELNUM
           END-IF
           MOVE    W201-SELNUM          TO  SPA-W201-SELNUM
           MOVE    W201-KSNCD2          TO  SPA-W201-KSNCD2
           MOVE    W201-KSNNM2          TO  SPA-W201-KSNNM2
      *
           .
       2201-GMNSPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院料加算コードチェック処理
      *****************************************************************
       2202-KSNCD2-CHK-SEC            SECTION.
      *
           IF      SPA-W201-KSNCD2     =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                      TNS-TENSU-REC
               MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
               MOVE    SPA-W201-KSNCD2     TO  TNS-SRYCD
               MOVE    SPA-NAI-W20-YUKOSTYMD
                                           TO  TNS-YUKOSTYMD
               MOVE    SPA-NAI-W20-YUKOSTYMD
                                           TO  TNS-YUKOEDYMD
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_tensu"     TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 900-TENSU-READ-SEC
                   IF    FLG-TENSU         =   ZERO
                       MOVE   MCPDATA-REC  TO  TNS-TENSU-REC
                       MOVE   TNS-NAME     TO  SPA-W201-KSNNM2
                   ELSE
                       MOVE   SPACE        TO  SPA-W201-KSNNM2
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-TENSU
               END-IF
      *
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
               IF      FLG-TENSU           NOT =   ZERO
                   MOVE    "0002"          TO  SPA-ERRCD
                   MOVE    2               TO  SPA-W201-CUR
               END-IF
      *
               IF      FLG-TENSU           =   ZERO
                  IF   TNS-HOSPSRYKBN      =   ZERO
                      CONTINUE
                  ELSE
                      IF  (TNS-HOSPSRYKBN  =  1)   AND
                          (SPA-HOSPSBT     =  2)
                          MOVE    "0005"   TO  SPA-ERRCD
                          MOVE    2        TO  SPA-W201-CUR
                      END-IF
                      IF  (TNS-HOSPSRYKBN  =  2)   AND
                          (SPA-HOSPSBT     =  1)
                          MOVE    "0006"   TO  SPA-ERRCD
                          MOVE    2        TO  SPA-W201-CUR
                      END-IF
                  END-IF
               END-IF
      *
      *        システム管理（医療機関情報）で設定する加算
      *        でないことのチェック
               IF   SPA-ERRCD       =   SPACE
                    IF   SPA-W201-KSNCD2  =   CONS-KSN1   OR
                                              CONS-KSN2   OR
                                              CONS-KSN3   OR
                                              CONS-KSN4   OR
                                              CONS-KSN5   OR
                                              CONS-KSN6   OR
                                              CONS-KSN7   OR
                                              CONS-KSN8   OR
                                              CONS-KSN9   OR
                                              CONS-KSN10  OR
                                              CONS-KSN11  OR
                                              CONS-KSN12  OR
                                              CONS-KSN13  OR
                                              CONS-KSN14  OR
                                              CONS-KSN15  OR
                                              CONS-KSN16  OR
                                              CONS-KSN17  OR
                                              CONS-KSN18  OR
                                              CONS-KSN19  OR
                                              CONS-KSN20  OR
                                              CONS-KSN21  OR
                                              CONS-KSN22  OR
                                              CONS-KSN23  OR
                                              CONS-KSN24
                        MOVE    "0007"   TO   SPA-ERRCD
                        MOVE    2        TO   SPA-W201-CUR
                    END-IF
               END-IF
      *
      *        病院のシステム管理（病棟管理情報）で設定する加算
      *        でないことのチェック
               IF  (SPA-ERRCD       =   SPACE)   AND
                   (SPA-HOSPSBT     =       1)
                   IF   SPA-W201-KSNCD2  =    CONS-KSN25  OR
                                              CONS-KSN26  OR
                                              CONS-KSN27  OR
                                              CONS-KSN28  OR
                                              CONS-KSN29  OR
                                              CONS-KSN30  OR
                                              CONS-KSN31  OR
                                              CONS-KSN32  OR
                                              CONS-KSN33  OR
                                              CONS-KSN34  OR
                                              CONS-KSN35  OR
                                              CONS-KSN36  OR
                                              CONS-KSN37  OR
                                              CONS-KSN38  OR
                                              CONS-KSN39  OR
                                              CONS-KSN40  OR
                                              CONS-KSN41  OR
                                              CONS-KSN42  OR
                                              CONS-KSN43  OR
                                              CONS-KSN44  OR
                                              CONS-KSN45  OR
                                              CONS-KSN46  OR
                                              CONS-KSN47  OR
                                              CONS-KSN48
                        MOVE    "0008"   TO   SPA-ERRCD
                        MOVE    2        TO   SPA-W201-CUR
                   END-IF
               END-IF
      *
      *        病院のシステム管理（病室管理情報）で設定する加算
      *        でないことのチェック
               IF  (SPA-ERRCD       =   SPACE)   AND
                   (SPA-HOSPSBT     =       1)
                   IF   SPA-W201-KSNCD2  =    CONS-KSN49  OR
                                              CONS-KSN50  OR
                                              CONS-KSN51  OR
                                              CONS-KSN52  OR
                                              CONS-KSN53  OR
                                              CONS-KSN54  OR
                                              CONS-KSN55  OR
                                              CONS-KSN56  OR
                                              CONS-KSN57
                        MOVE    "0009"   TO   SPA-ERRCD
                        MOVE    2        TO   SPA-W201-CUR
                   END-IF
               END-IF
      *
      *        診療所のシステム管理（病棟管理情報）で設定する加算
      *        でないことのチェック
               IF  (SPA-ERRCD       =   SPACE)   AND
                   (SPA-HOSPSBT     =       2)
                   IF   SPA-W201-KSNCD2  =    CONS-KSN58  OR
                                              CONS-KSN59  OR
                                              CONS-KSN60  OR
                                              CONS-KSN61  OR
                                              CONS-KSN62  OR
                                              CONS-KSN63  OR
                                              CONS-KSN64  OR
                                              CONS-KSN65  OR
                                              CONS-KSN66
                        MOVE    "0008"   TO   SPA-ERRCD
                        MOVE    2        TO   SPA-W201-CUR
                   END-IF
               END-IF
      *
      *        診療所のシステム管理（病室管理情報）で設定する加算
      *        でないことのチェック
               IF  (SPA-ERRCD       =   SPACE)   AND
                   (SPA-HOSPSBT     =       2)
                   IF   SPA-W201-KSNCD2  =    CONS-KSN67  OR
                                              CONS-KSN68  OR
                                              CONS-KSN69  OR
                                              CONS-KSN70
                        MOVE    "0009"   TO   SPA-ERRCD
                        MOVE    2        TO   SPA-W201-CUR
                   END-IF
               END-IF
      *
      *        入退院登録画面で設定する加算でないことのチェック
               IF  SPA-ERRCD       =   SPACE
                   IF   SPA-W201-KSNCD2  =    CONS-KSN71  OR
                                              CONS-KSN72  OR
                                              CONS-KSN73  OR
                                              CONS-KSN74
                        MOVE    "0013"   TO   SPA-ERRCD
                        MOVE    2        TO   SPA-W201-CUR
                   END-IF
               END-IF
      *
           END-IF
      *
           .
       2202-KSNCD2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       220-CLERA-SYORI-SEC                   SECTION.
      *
           INITIALIZE                  SPA-W201-GMN-G
           MOVE    ZERO                TO  SPA-W201-MAE-SELNUM
      *
           IF      SPA-W201-LIST-MAX   =   ZERO
               MOVE    2                   TO  SPA-W201-CUR
           ELSE
               MOVE    1                   TO  SPA-W201-CUR
           END-IF
      *
           .
       220-CLERA-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    クリア処理
      *****************************************************************
       230-DELETE-SEC                   SECTION.
      *
           PERFORM 2002-SELNUM-SEC
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-W201-SELNUM     >   ZERO
                   MOVE    "1003"          TO  SPA-WIDCD
               ELSE
                   MOVE    "0004"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-W201-CUR
               END-IF
           END-IF
      *
           .
       230-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       2301-DELETE-SYORI-SEC                   SECTION.
      *
           IF    SPA-W201-SELNUM       NOT =   ZERO
               MOVE    SPACE           TO  SPA-W201-TKSNCD2
                                                 (SPA-W201-SELNUM)
           END-IF
      *
      *    アップデート対象のデータを読み直す
           INITIALIZE                      SYSKANRI-REC
      *
           MOVE   "5001"               TO  SYS-KANRICD
           MOVE   SPA-GMN-W20-BTUNUM   TO  SYS-KBNCD
           MOVE   SPA-NAI-W20-YUKOSTYMD-OLD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF   ( FLG-SYSKANRI         NOT =   ZERO )
               MOVE    "9001"          TO  SPA-ERRCD
               GO  TO  2201-TOUROKU-SYORI-EXT
           END-IF
      *
           MOVE    SYSKANRI-REC        TO  SYS-5001-REC
           MOVE    SPACE               TO  SYS-5001-KSN2
      *
           MOVE    ZERO                TO  IDX2
           PERFORM    VARYING   IDX   FROM   1  BY  1
             UNTIL   (IDX       >   10)
               IF   SPA-W201-TKSNCD2(IDX)   NOT =    SPACE
                  ADD     1            TO  IDX2
                  MOVE    SPA-W201-TKSNCD2(IDX)
                                       TO  SYS-5001-KSNCD2(IDX2)
               END-IF
           END-PERFORM
           MOVE    SYS-5001-REC        TO  SYSKANRI-REC
           MOVE    SYS-5001-KANRICD    TO  SYSUP-KANRICD
           MOVE    SYS-5001-KBNCD      TO  SYSUP-KBNCD
           MOVE    SPA-NAI-W20-YUKOSTYMD-OLD
                                       TO  SYSUP-STYUKYMD
           MOVE    SPA-NAI-W20-YUKOEDYMD-OLD
                                       TO  SYSUP-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYSUP-HOSPNUM
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD    TO  SYS-UPYMD
           MOVE    SMCNDATE-HMS    TO  SYS-UPHMS
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF   (  MCP-RC              NOT =   ZERO )
               MOVE    "9001"          TO  SPA-ERRCD
           END-IF
      *
           IF   SPA-ERRCD    =    SPACE
                PERFORM   310-SPASET-SEC
           END-IF
      *
           .
       2301-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       220-TOUROKU-SEC                   SECTION.
      *
      *    入力チェック処理
           PERFORM 220-INPUT-CHK-SEC
      *    更新前
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2201-ALL-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    "1001"          TO  SPA-WIDCD
           END-IF
      *
           .
       220-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新前チェック処理
      *****************************************************************
       2201-ALL-CHK-SEC                SECTION.
      *
           IF      SPA-W201-KSNCD2     =   SPACE
               MOVE    "0010"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-W201-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
             IF  SPA-W201-TKSNCD2(10)    NOT =  SPACE
                 MOVE    "0011"          TO  SPA-ERRCD
                 MOVE    2               TO  SPA-W201-CUR
             END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
             IF  SPA-W201-SELNUM   NOT =   ZERO
                 MOVE    "0014"          TO  SPA-ERRCD
                 MOVE    1               TO  SPA-W201-CUR
             END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
             IF (SPA-W201-KSNCD2       =   SPA-W201-TKSNCD2(1)) OR
                (SPA-W201-KSNCD2       =   SPA-W201-TKSNCD2(2)) OR
                (SPA-W201-KSNCD2       =   SPA-W201-TKSNCD2(3)) OR
                (SPA-W201-KSNCD2       =   SPA-W201-TKSNCD2(4)) OR
                (SPA-W201-KSNCD2       =   SPA-W201-TKSNCD2(5)) OR
                (SPA-W201-KSNCD2       =   SPA-W201-TKSNCD2(6)) OR
                (SPA-W201-KSNCD2       =   SPA-W201-TKSNCD2(7)) OR
                (SPA-W201-KSNCD2       =   SPA-W201-TKSNCD2(8)) OR
                (SPA-W201-KSNCD2       =   SPA-W201-TKSNCD2(9)) OR
                (SPA-W201-KSNCD2       =   SPA-W201-TKSNCD2(10))
                 MOVE    "0012"          TO  SPA-ERRCD
                 MOVE    2               TO  SPA-W201-CUR
             END-IF
           END-IF
           .
       2201-ALL-CHK-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       2201-TOUROKU-SYORI-SEC                   SECTION.
      *
           IF    SPA-W201-SELNUM       =   ZERO
                 PERFORM    VARYING    IDX   FROM  1   BY   1
                   UNTIL   (IDX     >   10)  OR
                           (SPA-W201-TKSNCD2(IDX)   =   SPACE)
                    CONTINUE
                 END-PERFORM
           END-IF
           MOVE     SPA-W201-KSNCD2    TO  SPA-W201-TKSNCD2(IDX)
           MOVE     IDX                TO  SPA-W201-TRENNUM(IDX)
           ADD      1                  TO  SPA-W201-LIST-MAX
      *
      *    アップデート対象のデータを読み直す
           INITIALIZE                      SYSKANRI-REC
      *
           MOVE   "5001"               TO  SYS-KANRICD
           MOVE   SPA-GMN-W20-BTUNUM   TO  SYS-KBNCD
           MOVE   SPA-NAI-W20-YUKOSTYMD-OLD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF   ( FLG-SYSKANRI         NOT =   ZERO )
               MOVE    "9001"          TO  SPA-ERRCD
               GO  TO  2201-TOUROKU-SYORI-EXT
           END-IF
      *
           MOVE    SYSKANRI-REC        TO  SYS-5001-REC
           PERFORM    VARYING   IDX   FROM   1  BY  1
             UNTIL   (IDX       >   10)  OR
                     (SPA-W201-TKSNCD2(IDX)  =  SPACE)
             MOVE    SPA-W201-TKSNCD2(IDX)
                                       TO  SYS-5001-KSNCD2(IDX)
           END-PERFORM
           MOVE    SYS-5001-REC        TO  SYSKANRI-REC
           MOVE    SYS-5001-KANRICD    TO  SYSUP-KANRICD
           MOVE    SYS-5001-KBNCD      TO  SYSUP-KBNCD
           MOVE    SPA-NAI-W20-YUKOSTYMD-OLD
                                       TO  SYSUP-STYUKYMD
           MOVE    SPA-NAI-W20-YUKOEDYMD-OLD
                                       TO  SYSUP-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYSUP-HOSPNUM
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD    TO  SYS-UPYMD
           MOVE    SMCNDATE-HMS    TO  SYS-UPHMS
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF   (  MCP-RC              NOT =   ZERO )
               MOVE    "9001"          TO  SPA-ERRCD
           END-IF
      *
           IF   SPA-ERRCD    =    SPACE
                PERFORM   220-CLERA-SYORI-SEC
           END-IF
           .
       2201-TOUROKU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ編集処理
      *****************************************************************
       22011-DBHENSYU-SEC                   SECTION.
      *
           .
       22011-DBHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W20"               TO  SPA-SAKIPG
           MOVE    "W201"              TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "W20"               TO  MCP-WINDOW
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
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
            PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-WIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W201"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択番号がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE   "入院料加算コードが点数マスタに存在しません。"
                                       TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "選択番号を入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE   "病院のみ算定可能な加算です。"
                                       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE   "診療所のみ算定可能な加算です。"
                                       TO  SPA-ERRMSG
               WHEN    "0007"
                MOVE "システム管理−医療機関情報で設定可能な加算です。"
                                       TO  SPA-ERRMSG
               WHEN    "0008"
                MOVE "システム管理−病棟管理情報で設定可能な加算です。"
                                       TO  SPA-ERRMSG
               WHEN    "0009"
                MOVE "システム管理−病室管理情報で設定可能な加算です。"
                                       TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "入院料加算コードが未設定です。"
                                       TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "入院料加算コードの設定は１０件が上限です。"
                                       TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "入院料加算コードが重複しています。"
                                       TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "入退院登録画面で設定可能な加算です。"
                                       TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "選択された加算は追加できません。"
                                       TO  SPA-ERRMSG
               WHEN    "9000"
                   MOVE    "病棟情報が取得できません。"
                                       TO  SPA-ERRMSG
               WHEN    "9001"
                   MOVE    "更新エラーです。"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W201"               TO  SPA-MOTOPG
           MOVE    "WERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "WERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   STRING  "入院料加算を追加します。"
                                                   DELIMITED  BY  SIZE
                           "よろしいですか？"      DELIMITED  BY  SIZE
                                                   INTO    WRK-WIDMSG
                   END-STRING
               WHEN    "1003"
                   STRING  "入院料加算を削除します。"
                                                   DELIMITED  BY  SIZE
                           "よろしいですか？"      DELIMITED  BY  SIZE
                                                   INTO    WRK-WIDMSG
                   END-STRING
          END-EVALUATE
      *
           MOVE    SPACE               TO  WID1
           MOVE    SPA-WIDCD           TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG          TO  WID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "W201"              TO  SPA-MOTOPG
           MOVE    "WID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "WID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
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
      *    管理マスター読込処理
      *****************************************************************
       900-SYSKANRI-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  920-DBFETCH-SEC
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込処理
      *****************************************************************
       900-TENSU-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-TENSU
           ELSE
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           .
       900-TENSU-READ-EXT.
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
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
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
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
