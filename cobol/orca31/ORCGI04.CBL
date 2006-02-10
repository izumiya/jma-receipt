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
       PROGRAM-ID.                 ORCGI04.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院業務
      *  コンポーネント名  : 請求確認画面
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/10/30    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    02.12.10  収納明細作成処理追加
      *  01.00.02    NACL-太田    03.01.23  食事の計算方法修正
      *  01.00.03    NACL-太田    03.01.24  保険組合せリストの表示を修正
      *  01.00.04    NACL-太田    03.02.05  自院歴作成データを考慮する
      *  01.00.05    NACL-太田    03.03.07  帳票発行コンボの初期表示修正
      *  01.00.06    NACL-太田    03.03.13  入院請求書複数枚一括印刷対応
      *  01.00.07    NACL-太田    03.03.18  入院カスタマイズ帳票対応
      *  01.00.08    NACL-太田    03.07.01  出力先プリンタサブ追加
      *  01.00.09    NACL-太田    03.11.14  更新日付編集対応
      *  01.00.10    NACL-太田    04.08.02  定期請求中は退院登録は不可
      *                                     とする
      *  01.00.11    NACL-太田    04.10.14  退院事由追加
      *  01.00.12    NACL-小豆沢  05.07.19  自費１０項目対応
      *  01.00.13    NACL-小豆沢  05/10/17  入金額ゼロでも入金方法を
      *                                     セットする
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
      *INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *    パラメタファイル名
      *
      *    画面色等
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
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SYUMEI          PIC 9(01).
           03  FLG-SYUDAY          PIC 9(01).
           03  FLG-PTTEIKIRRK      PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SRYKARRK        PIC 9(01).
           03  FLG-UKETUKE         PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-PARA            PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-SINKI           PIC 9(01).
      *
           03  FLG-SYOSIN-DUMMY    PIC 9(01).
      *
           03  FLG-TOKTEI          PIC 9(01).
           03  FLG-DBRC            PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-ENTRYCHK        PIC 9(01).
           03  FLG-ZI-SAISIN       PIC 9(01).
           03  FLG-NYUINKA         PIC 9(01).
           03  FLG-SKYPRT          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
           03  IDX1                PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDX3                PIC 9(05).
           03  IDX4                PIC 9(05).
           03  IDX5                PIC 9(05).
           03  IDX6                PIC 9(05).
           03  IDX-G               PIC 9(05).
           03  IDX-SYU             PIC 9(05).
           03  IDXY                PIC 9(05).
           03  IDXZ                PIC 9(05).
           03  IDX-LST             PIC 9(05).
           03  IDX-CMB             PIC 9(05).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD     PIC X(08).
           03  SYS-TIME    PIC X(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SKYMONEY               PIC  S9(07).
           03  WRK-YMD                    PIC X(10). 
           03  WRK-HENYMDG                PIC X(09).
           03  WRK-SYMD.
               05  WRK-SYY                 PIC 9(04).
               05  WRK-SMM                 PIC 9(02).
               05  WRK-SDD                 PIC 9(02).
      *
      *****03  WRK-Z9                      PIC Z,ZZZ,ZZZ.
           03  WRK-Z99                     PIC --,---,--9.
           03  WRK-Z9                      PIC --,---,---.
           03  WRK-Z92                     PIC --,---,---.
           03  WRK-ZZ9                     PIC ZZ9.
      *
      *    入力項目名
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-MOTOPG                  PIC X(08).
           03  WRK-KANACHK-MAE-INPUT       PIC X(400).
      *    パス名
           03  WRK-DBPATH                  PIC X(20).
           03  WRK-FTNRATE                 PIC X(05).
           03  WRK-I0IDMSG                 PIC X(80).
           03  WRK-KANRICD                 PIC X(04).
           03  WRK-KBNCD                   PIC X(08).
           03  WRK-KJNYMD                  PIC X(08).
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-KEISANYMD               PIC X(08).
           03  WRK-SEIKYU                  PIC S9(07).
           03  WRK-COMBO.
               05  WRK-CMB-CD              PIC X(10).
               05  WRK-CMB-IDX             PIC 9(03).
               05  WRK-CMB-ITM             PIC X(100).
               05  WRK-CMB-ITM2            PIC X(100).
           03  WRK-TAIINUPDCNT             PIC 9(05).
           03  WRK-HKNCOMBINUM             PIC X(04).
           03  WRK-HKNCOMBINUM9   REDEFINES  WRK-HKNCOMBINUM
                                           PIC 9(04).
           03  WRK-TAIHI-SRYKA             PIC X(02).
           03  WRK-ZI-SAISIN-RRKNUM        PIC 9(03).
           03  WRK-ZI-SAISIN-RRKEDANUM     PIC 9(03).
           03  WRK-LST-IDX                 PIC 9(05).
           03  WRK-SYU-SUM-IDX             PIC 9(05).
           03  WRK-IDX                     PIC 9(05).
           03  WRK-DENPNUM                 PIC 9(07).
           03  WRK-DENPNUM-X               PIC X(08).
           03  WRK-NYUINKA                 PIC X(02).
           03  WRK-BTUNUM                  PIC X(02).
           03  WRK-BRMNUM                  PIC X(08).
           03  WRK-DRCD                    PIC X(04).
           03  WRK-STIDX                   PIC 9(05).
           03  WRK-EDIDX                   PIC 9(05).
           03  WRK-SKY-CNT                 PIC 9(10).
           03  WRK-PRINT-PG                PIC X(20).
           03  WRK-ORG-PRINT-PG            PIC X(20).
      *
       01  CONST-AREA.
           03  CONST-JOB-SHELLID           PIC X(08)   VALUE   "TEIKI1".
           03  CONST-JOB-JOBID             PIC 9(07)   VALUE   1.
      *
      *    スパ領域 ワーク
           COPY    "COMMON-SPA"      REPLACING
                               //SPA-// BY //WRK-SPA-//.
      *
      *    患者入院履歴ワーク
       01  WKPTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC"
                                     REPLACING
                               //PTNYUINRRK-// BY //WKPTNYUINRRK-//.
      *    収納ワーク
       01  WKSYUNOU-REC.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-//
                                           BY //WKSYU-//.
      *
      *    日別収納ワーク
       01  WKSYUDAY-REC.
           COPY  "CPSYUDAY.INC"    REPLACING  //SDAY-//
                                           BY //WKSDAY-//.
      *
      *    収納明細ワーク
       01  WKSYUMEI-REC.
           COPY  "CPSYUMEI.INC"    REPLACING  //SMEI-//
                                           BY //WKSMEI-//.
      *
      *    定期請求履歴集計用ワーク
       01  SUMTEIKIRRK-REC.
         02    SUMTEIKIRRK-MAX         PIC 9(05).
         02    SUMTEIKIRRK-OCC     OCCURS 50.
           COPY  "CPTEIKIRRK.INC"  REPLACING  //TEIKIRRK-//
                                           BY //SUMTEIKIRRK-//.
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK0042.INC".
      *
      *    診療科コード
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1013.INC".
      *    ドクター
           COPY    "CPSK1010.INC".
      *
      *    帳票プログラム名
           COPY    "CPSK1035.INC".
      *    医療機関入院基本情報
           COPY    "CPSK5000.INC".
      *    出力先プリンタ名割り当て情報\
           COPY  "CPSK1031.INC".
      *    帳票プログラム選択情報
           COPY  "CPSK1032.INC".
      *
           COPY  "CPSK1041.INC".
      *
           COPY  "CPSK5013.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    収納マスター
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    日別収納マスター
       01  SYUDAY-REC.
           COPY    "CPSYUDAY.INC".
      *
      *    収納明細マスタ−
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    診療行為マスター
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療科履歴
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    受診内容
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    診療行為マスタワーク
       01  LNK-WKSRYACT-AREA.
           02  LNK-WKSRYACT-REC      OCCURS   30.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //LNK-WKSRY-//.
       01  LNK-WKSRYACT-MAX           PIC 9(04).
      *
      *    診療行為マスタワーク
       01  WRK-WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                    //WKSRY-// BY //WRK-WKSRY-//.
      *
      *    受診履歴
       01  TBL-JYURRK-AREA.
           02  TBL-JYURRK-REC        OCCURS   10.
           COPY    "CPJYURRK.INC"  REPLACING
                                     //JYURRK-// BY //TBL-JYURRK-//.
      *
      *    患者定期請求履歴
        01 PTTEIKIRRK-REC.
           COPY     "CPPTTEIKIRRK.INC".
      *
      *    定期請求履歴
        01 TEIKIRRK-REC.
           COPY     "CPTEIKIRRK.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    負担金額計算用パラメタ
           COPY    "CPORCS01.INC".
      *
      *    診療科算定履歴更新サブ
           COPY    "CPORCSKARRK.INC".
      *
      *    算定履歴更新サブ
           COPY    "CPORCSSANTEI.INC".
      *
      *    入院会計作成サブ
           COPY    "CPORCSNYUACCT.INC".
      *
      *    保険組合せ編集サブ
           COPY    "CPORCSHKNSET.INC".
      *
      *    帳票サブ < 請求書兼領収書 >
           COPY    "CPORCHCN03.INC".
      *
      *    帳票サブ < 退院証明書 >
       01  LNK-PTNYUINRRK.
           COPY    "CPPTNYUINRRK.INC"
               REPLACING   //PTNYUINRRK-//
               BY          //LNK-PTNYUINRRK-//.
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
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
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
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-I0FREE
           MOVE    SPA-WORK        TO  SPA-I0KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
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
           MOVE    SPA-I0KYOUTU    TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-I0FREE      TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    DISPLAY "*** ORCSNPL    START  ***"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "I0ERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO      TO      100-INIT-EXT
               END-IF
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
      *****MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   SPACE                TO  MCP-PUTTYPE.
           MOVE   "I04"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
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
       300-SCREEN-SEC             SECTION.
      *
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *
           END-IF
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
           WHEN    "I0ID1"
      *        確認画面より
               PERFORM 3001-I0ID1-SET-SEC
               MOVE   SPACE        TO  SPA-I0IDCD
               GO  TO      300-SCREEN-EXT
           END-EVALUATE
      *
           MOVE    SPACE           TO  SPA-I0IDCD
      *
           PERFORM                 310-SPA-SET-SEC
      *
           MOVE    1               TO  SPA-GMN-I04-CUR
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（Ｉ５ＩＤ１）ＯＫ処理
      *****************************************************************
       3001-I0ID1-SET-SEC         SECTION.
      *
           IF    ( SPA-I0ID1-FLG   =   "OK" )
               EVALUATE    SPA-I0IDCD
               WHEN    "3001"
                   PERFORM 452-TOUROKU-SEC
                   IF    ( SPA-ERRCD       NOT =   SPACE )
                       GO  TO  3001-I0ID1-SET-EXT
                   END-IF
               END-EVALUATE
           END-IF
           .
       3001-I0ID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                 SECTION.
      *
           MOVE    "1"             TO  SPA-NYUGAIKBN
      *
           MOVE    SPACE           TO  SPA-I04
           INITIALIZE                  SPA-I04
      *
      *    引継ぎ項目設定処理
           PERFORM 3100-SYOKI-SET-SEC
      *
      *    コンボボックス初期化
           PERFORM 3100-CMB-SYOKI-SEC
      *
      *    患者マスタ検索
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
      *    システム管理マスタ検索処理
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5000"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SPA-NAI-I04-TAIINYMD
                                   TO  ORC-DBYMD
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

           MOVE    SYS-5000-SKYPRTKBN
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-SKYPRTOUT-SRH-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-I04-SKYPRTOUT-ITM (1)
                                   TO  SPA-GMN-I04-SKYPRTOUT
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I04-SKYPRTOUT
           END-IF
      *
           IF    ( SPA-I01-I04-PROCKBN      =   "1" )
               IF    ( SYS-5000-TAIPRTKBN   =   "0" )
                   MOVE    SPA-GMN-I04-TAIPRTOUT-ITM (01)
                                       TO  SPA-GMN-I04-TAIPRTOUT
               ELSE
                   MOVE    SPA-GMN-I04-TAIPRTOUT-ITM (02)
                                       TO  SPA-GMN-I04-TAIPRTOUT
               END-IF
           ELSE
                   MOVE    SPA-GMN-I04-TAIPRTOUT-ITM (01)
                                       TO  SPA-GMN-I04-TAIPRTOUT
           END-IF
      *
      *    入金方法初期値
           MOVE    PTINF-NYUKIN-HOHO   TO  WRK-CMB-CD
           PERFORM 4103-CMB-NYKNHOHO-SRH-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    "01"            TO  WRK-CMB-CD
               PERFORM 4103-CMB-NYKNHOHO-SRH-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-I04-NYKNHOHO
               MOVE    WRK-CMB-ITM2    TO  SPA-NAI-I04-NYKNJYOTAI
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-I04-NYKNHOHO
               MOVE    WRK-CMB-ITM2    TO  SPA-NAI-I04-NYKNJYOTAI
           END-IF
      *
           DISPLAY "PTINF-NYUKIN-HOHO = " PTINF-NYUKIN-HOHO
           DISPLAY "SPA-NAI-I04-NYKNJYOTAI = " SPA-NAI-I04-NYKNJYOTAI
      *
      *    主治医氏名初期値
           MOVE    SPA-GMN-I01-DR-CD (1)
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-DR-SRH-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-I04-DR-ITM (1)
                                   TO  SPA-GMN-I04-DR
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I04-DR
           END-IF
      *
      *    発効日
           MOVE    SPA-SYSYMDWH        TO  SPA-GMN-I04-HAKYMD
           MOVE    SPA-SYSYMD          TO  SPA-NAI-I04-HAKYMD
      *
      *    請求データ内部領域を初期化
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-I0-SYUNOU-CNT )
                     OR  ( IDX1    >   15 )
               MOVE    SPA-I0-SYUNOU-DAT (IDX1)
                                       TO  WKSYUNOU-REC
               MOVE    WKSYU-SKYMONEY
                                       TO  SPA-NAI-I04-SEIKYU
                                                   (IDX1)
                                           SPA-NAI-I04-SEIKYU-KON
                                                   (IDX1)
               MOVE    ZERO                TO  SPA-NAI-I04-CHOSEI
                                                   (IDX1)
               MOVE    SPACE               TO  SPA-NAI-I04-CHOSEI-X
                                                   (IDX1)
      *        MOVE    WKSYU-SKYMONEY      TO  SPA-NAI-I04-NYUKIN
      *                                            (IDX1)
               IF    ( SPA-NAI-I04-NYKNJYOTAI  =   "1" )
                   MOVE    WKSYU-SKYMONEY      TO  SPA-NAI-I04-NYUKIN
                                                   (IDX1)
               ELSE
                   MOVE    ZERO                TO  SPA-NAI-I04-NYUKIN
                                                   (IDX1)
               END-IF
               MOVE    SPA-NAI-I04-NYUKIN (IDX1)
                                           TO  WRK-Z92
               MOVE    WRK-Z92             TO  SPA-NAI-I04-NYUKIN-X
                                                   (IDX1)
           END-PERFORM
      *
      *    未収データ取得処理
           PERFORM 3100-MISYU-DAT-EDIT-SEC
      *
      *    保険組み合わせリスト編集処理
           PERFORM 3100-HKNCOMBILST-EDIT-SEC
      *
           MOVE    SPA-NAI-I04-TSYUNOUIDX(1)
                                   TO  WRK-LST-IDX
      *    請求データ編集処理
           PERFORM 3100-SEIKYU-DAT-EDIT-SEC
      *
            .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    引継ぎ項目設定処理
      *****************************************************************
       3100-SYOKI-SET-SEC              SECTION.
      *
      *    患者ＩＤ
           MOVE    SPA-NAI-I01-PTID    TO  SPA-NAI-I04-PTID
      *
      *    入院履歴番号
           MOVE    SPA-I01-I04-RRKNUM
                                       TO  SPA-NAI-I04-PTRRKNUM
      *    入院履歴枝番号
           MOVE    SPA-I01-I04-RRKEDANUM
                                       TO  SPA-NAI-I04-PTRRKEDANUM
      *    患者番号
           MOVE    SPA-GMN-I01-PTNUM   TO  SPA-GMN-I04-PTNUM
      *    患者カナ氏名
           MOVE    SPA-NAI-I01-KANANAME
                                       TO  SPA-GMN-I04-KANANAME
      *    患者氏名
           MOVE    SPA-GMN-I01-NAME    TO  SPA-GMN-I04-NAME
      *    患者性別
           MOVE    SPA-GMN-I01-SEX     TO  SPA-GMN-I04-SEX
      *    患者誕生日
           MOVE    SPA-GMN-I01-BIRTHDAY
                                       TO  SPA-GMN-I04-BIRTHDAY
      *    患者年齢
           MOVE    SPA-GMN-I01-AGE     TO  SPA-GMN-I04-AGE
      *
      *    保険組み合わせ
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
           INITIALIZE                      WRK-SPA-KYOTUKEY
                                           WRK-SPA-KYOTU-SET
                                           ORCSHKNSETAREA
      *
           MOVE    SPA-I01-I04-KJNYMD  TO  WRK-SPA-SRYYMD
           MOVE    SPA-I01-I04-NYUINKA TO  WRK-SPA-SRYKA
           MOVE    "1"                 TO  WRK-SPA-NYUGAIKBN
           MOVE    SPA-NAI-I01-PTID    TO  WRK-SPA-PTID
           MOVE    SPA-NAI-I01-BIRTHDAY
                                       TO  WRK-SPA-BIRTHDAY
           MOVE    SPA-I01-I04-HKNCOMBI
                                       TO  WRK-SPA-HKNCOMBINUM
           MOVE    "1"                 TO  ORCSHKN-KBN
      *
           CALL    "ORCSHKNSET"        USING   WRK-SPA-AREA
                                               ORCSHKNSETAREA
           IF    ( ORCSHKN-ERRCD   NOT =   ZERO )
               INITIALIZE              ORCSHKNSETAREA
           END-IF
      *
           IF    ( ORCSHKN-IDX         >   ZERO )
            AND  ( ORCSHKN-IDX         <=  ORCSHKN-HKN-MAX )
               MOVE    ORCSHKN-GMN-HKNCOMBIMEI(ORCSHKN-IDX)
                                       TO  SPA-GMN-I04-HKNCOMBI
               MOVE    ORCSHKN-GMN-FTNRATEMEI (ORCSHKN-IDX)
                                       TO  SPA-GMN-I04-FTNRATE
           END-IF
      *
      *    入院科
           INITIALIZE                      SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-I01-I04-NYUINKA TO  SYS-1005-KBNCD
           MOVE    SPA-I01-I04-KJNYMD  TO  ORC-DBYMD
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
      *
      *    システム管理マスタ検索処理
           MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC            =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
           ELSE
               INITIALIZE                  SYS-1005-REC
           END-IF
      *
           MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           MOVE    SYS-1005-SRYKANAME  TO  SPA-GMN-I04-NYUINKA
      *
      *    入院日
           MOVE    SPA-I01-I04-NYUINYMD
                                       TO  SPA-NAI-I04-NYUINYMD
           MOVE    SPA-I01-I04-NYUINYMD
                                       TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-I04-NYUINYMD
      *
      *    退院日
           MOVE    SPA-I01-I04-TAIINYMD
                                       TO  SPA-NAI-I04-TAIINYMD
           MOVE    SPA-I01-I04-TAIINYMD
                                       TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-I04-TAIINYMD
      *
      *    退院事由
           INITIALIZE                      SYS-5013-REC
           MOVE    "5013"              TO  SYS-5013-KANRICD
           MOVE    SPA-I01-I04-TAIINCD TO  SYS-5013-KBNCD
           MOVE    SPA-I01-I04-TAIINYMD
                                       TO  ORC-DBYMD
           MOVE    SYS-5013-REC        TO  MCPDATA-REC
      *
           MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC            =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-5013-REC
           ELSE
               INITIALIZE                  SYS-5013-REC
           END-IF
      *
           MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           MOVE    SYS-5013-TANNAME    TO  SPA-GMN-I04-TAIINCD
      *
           MOVE    SPA-I01-I04-TAIINCD TO  SPA-NAI-I04-TAIINCD
      *
            .
       3100-SYOKI-SET-EXT.
           EXIT.
      *****************************************************************
      *    コンボボックス初期化
      *****************************************************************
       3100-CMB-SYOKI-SEC              SECTION.
      *
      *
           INITIALIZE                  SPA-GMN-I04-CMB-DAT-AREA
      *
      *    入金方法コンボ
           PERFORM 3100-CMB-NYKNHOHO-SET-SEC
      *
      *    請求書兼領収書発行コンボ
           PERFORM 3100-CMB-SKYPRTOUT-SET-SEC
      *
      *    退院証明書発行コンボ
           PERFORM 3100-CMB-TAIPRTOUT-SET-SEC
      *
      *    主治医氏名コンボ
           PERFORM 3100-CMB-DR-SET-SEC
      *
            .
       3100-CMB-SYOKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入金方法コンボ
      *****************************************************************
       3100-CMB-NYKNHOHO-SET-SEC       SECTION.
      *
      *    入金方法
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYS-1041-REC
           MOVE    "1041"              TO  SYS-1041-KANRICD
           MOVE    SYS-1041-REC        TO  MCPDATA-REC
           MOVE    SPA-SYSYMD          TO  ORC-DBYMD
           MOVE    "SYSKANRI-KEY2"     TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF  ( FLG-DBRC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-1041-REC
           ELSE
               INITIALIZE                  SYS-1041-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX0
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
                    OR   ( IDX0           >=   99 )
      *
               COMPUTE IDX0    =   IDX0    +   1
      *
               MOVE    SYS-1041-KBNCD
                                   TO  SPA-GMN-I04-NYKNHOHO-ITM (IDX0)
                                                                (1:2)
               MOVE    SYS-1041-NYKN-NAME
                                   TO  SPA-GMN-I04-NYKNHOHO-ITM (IDX0)
                                                                (4:)
               IF    ( SYS-1041-NYKN-JYOTAI-N  NOT =   SPACE )
                   MOVE    SYS-1041-NYKN-JYOTAI-N
                                   TO  SPA-GMN-I04-NYKNJYOTAI-ITM (IDX0)
               ELSE
                   MOVE    "2"     TO  SPA-GMN-I04-NYKNJYOTAI-ITM (IDX0)
               END-IF
      *
               MOVE    "SYSKANRI-KEY2" TO  WRK-DBPATH
               PERFORM 900-TBL-READ-SEC
               IF    ( FLG-DBRC    NOT =   ZERO )
                   MOVE    1           TO  FLG-SYSKANRI
               ELSE
                   MOVE    MCPDATA-REC TO  SYS-1041-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    IDX0                TO  SPA-GMN-I04-NYKNHOHO-CNT
      *
           MOVE    "SYSKANRI-KEY2"     TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
      *
            .
       3100-CMB-NYKNHOHO-SET-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書発行コンボ
      *****************************************************************
       3100-CMB-SKYPRTOUT-SET-SEC      SECTION.
      *
           MOVE    3               TO  SPA-GMN-I04-SKYPRTOUT-CNT
           MOVE    "0:発行なし"    TO  SPA-GMN-I04-SKYPRTOUT-ITM (01)
           MOVE    "1:発行あり"    TO  SPA-GMN-I04-SKYPRTOUT-ITM (02)
           MOVE    "2:発行あり（請求あり）"  
                                   TO  SPA-GMN-I04-SKYPRTOUT-ITM (03)
      *
            .
       3100-CMB-SKYPRTOUT-SET-EXT.
           EXIT.
      *****************************************************************
      *    退院証明書発行コンボ
      *****************************************************************
       3100-CMB-TAIPRTOUT-SET-SEC      SECTION.
      *
           MOVE    2               TO  SPA-GMN-I04-TAIPRTOUT-CNT
           MOVE    "0:発行なし"    TO  SPA-GMN-I04-TAIPRTOUT-ITM (01)
           MOVE    "1:発行あり"    TO  SPA-GMN-I04-TAIPRTOUT-ITM (02)
      *
            .
       3100-CMB-TAIPRTOUT-SET-EXT.
           EXIT.
      *****************************************************************
      *    主治医氏名コンボ
      *****************************************************************
       3100-CMB-DR-SET-SEC             SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1010"          TO  SYS-KANRICD
           MOVE    "1"             TO  SYS-KBNCD
           MOVE    SPA-NAI-I04-TAIINYMD
                                   TO  ORC-DBYMD
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
               MOVE    IDX1    TO  SPA-GMN-I04-DR-CNT
               MOVE    SYS-1010-KBNCD (2 : 4)
                           TO  SPA-GMN-I04-DR-ITM(IDX1)(1 : 4)
               MOVE    SYS-1010-NAME
                           TO  SPA-GMN-I04-DR-ITM(IDX1)(6 : )
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
            .
       3100-CMB-DR-SET-EXT.
           EXIT.
      *****************************************************************
      *    未収データ取得処理
      *****************************************************************
       3100-MISYU-DAT-EDIT-SEC         SECTION.
      *
           MOVE    ZERO                TO   SPA-GMN-I04-ZENMISYU-NYU
                                            SPA-GMN-I04-ZENMISYU-GAI
      *    入院分未収集計
           INITIALIZE                       SYUNOU-REC
           MOVE    SPA-HOSPID          TO   SYU-HOSPID
           MOVE    "1"                 TO   SYU-NYUGAIKBN
           MOVE    SPA-NAI-I04-PTID    TO   SYU-PTID
           MOVE    SYUNOU-REC          TO   MCPDATA-REC
           MOVE    "SYUNOU-KEY2"       TO   ORC-DBPATH
           PERFORM  910-DB-SELECT-SEC
           IF      MCP-RC              =    ZERO
               MOVE    "SYUNOU-KEY2"   TO   ORC-DBPATH
               PERFORM   910-DB-READ-SEC
               PERFORM   UNTIL  MCP-RC  NOT =  ZERO
                      MOVE    MCPDATA-REC     TO  SYUNOU-REC
                      IF    ( SPA-I01-I04-SKYSTYMD   <= SYU-SKYSTYMD )
                       AND  ( SPA-I01-I04-SKYEDYMD   >= SYU-SKYEDYMD )
                       AND  ( SPA-I01-I04-RRKNUM
                                                = SYU-NYUIN-RRKNUM )
                           CONTINUE
                      ELSE
                           IF  SYU-DENPPRTYMD  <=  SPA-NAI-I04-TAIINYMD
                               IF SYU-SKYMONEY  >   SYU-NYUKIN-TOTAL
                                  COMPUTE   SPA-GMN-I04-ZENMISYU-NYU   =
                                            SPA-GMN-I04-ZENMISYU-NYU   +
                                           (SYU-SKYMONEY   -
                                            SYU-NYUKIN-TOTAL)
                               END-IF
                           END-IF
                      END-IF
                      MOVE    "SYUNOU-KEY2"   TO   ORC-DBPATH
                      PERFORM   910-DB-READ-SEC
               END-PERFORM
           ELSE
               MOVE    ZERO            TO   SPA-GMN-I04-ZENMISYU-NYU
           END-IF
      *
           MOVE    "SYUNOU-KEY2"  TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
      *    外来分未収集計
           INITIALIZE                       SYUNOU-REC
           MOVE    SPA-HOSPID          TO   SYU-HOSPID
           MOVE    "2"                 TO   SYU-NYUGAIKBN
           MOVE    SPA-NAI-I04-PTID    TO   SYU-PTID
           MOVE    SYUNOU-REC          TO   MCPDATA-REC
           MOVE    "SYUNOU-KEY2"       TO   ORC-DBPATH
           PERFORM  910-DB-SELECT-SEC
           IF      MCP-RC              =    ZERO
               MOVE    "SYUNOU-KEY2"   TO   ORC-DBPATH
               PERFORM   910-DB-READ-SEC
               PERFORM   UNTIL  MCP-RC  NOT =  ZERO
                      MOVE    MCPDATA-REC     TO  SYUNOU-REC
                      IF      SYU-DENPPRTYMD  <=  SPA-NAI-I04-TAIINYMD
                        IF      SYU-SKYMONEY  >   SYU-NYUKIN-TOTAL
                              COMPUTE   SPA-GMN-I04-ZENMISYU-GAI   =
                                        SPA-GMN-I04-ZENMISYU-GAI   +
                                        (SYU-SKYMONEY   -
                                         SYU-NYUKIN-TOTAL)
                        END-IF
                      END-IF
                      MOVE    "SYUNOU-KEY2"   TO   ORC-DBPATH
                      PERFORM   910-DB-READ-SEC
               END-PERFORM
           ELSE
               MOVE    ZERO            TO   SPA-GMN-I04-ZENMISYU-GAI
           END-IF
      *
           MOVE    "SYUNOU-KEY2"  TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
            .
       3100-MISYU-DAT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    請求データ編集処理
      *****************************************************************
       3100-SEIKYU-DAT-EDIT-SEC        SECTION.
      *
           MOVE    SPACE       TO  SPA-GMN-I04-SEIKYU-AREA
           INITIALIZE              SPA-GMN-I04-SEIKYU-AREA
      *
           IF    ( SPA-I0-SYUNOU-TTLDAT-FLG  (WRK-LST-IDX) =   ZERO )
               MOVE    WIDGET-NORMAL
                               TO  SPA-GMN-I04-CHOSEI-STT
                                   SPA-GMN-I04-NYUKIN-STT
           ELSE
               MOVE    WIDGET-INSENSITIVE
                               TO  SPA-GMN-I04-CHOSEI-STT
                                   SPA-GMN-I04-NYUKIN-STT
           END-IF
      *
           MOVE    SPA-I0-SYUNOU-DAT (WRK-LST-IDX)
                                       TO  WKSYUNOU-REC
      *
      *
      *    保険料
           MOVE    ZERO                TO  SPA-GMN-I04-HKNRYO (12)
           PERFORM VARYING     IDX1     FROM    1   BY  1
                   UNTIL     ( IDX1     >   11 )
               MOVE    WKSYU-HKNTEN (IDX1)
                                      TO  SPA-GMN-I04-HKNRYO (IDX1)
               COMPUTE SPA-GMN-I04-HKNRYO (12)
                   =   SPA-GMN-I04-HKNRYO (12)
                   +   WKSYU-HKNTEN (IDX1)
      *    保険適用外
               COMPUTE SPA-GMN-I04-TGMONEY (IDX1)
                   =   WKSYU-TGMONEY (IDX1)
                   +   WKSYU-TGMONEY-TAX(IDX1)
           END-PERFORM
      *
      *    保険適用外
           COMPUTE SPA-GMN-I04-HKNGAI
                   =   WKSYU-TGMONEY (12)
                   +   WKSYU-TGMONEY-TAX (12)
      *
      *    保険適用金額
           MOVE    WKSYU-HKNTEKMONEY       TO  SPA-GMN-I04-HKNTEKI
      *    老人一部負担金
           MOVE    WKSYU-RJN-FTN           TO  SPA-GMN-I04-ROUFTN
           MOVE    WKSYU-KOH-FTN           TO  SPA-GMN-I04-KOHFTN
      *    自費１
           MOVE    WKSYU-JIHI-1            TO  SPA-GMN-I04-JIHI1
           MOVE    WKSYU-JIHI-2            TO  SPA-GMN-I04-JIHI2
           MOVE    WKSYU-JIHI-3            TO  SPA-GMN-I04-JIHI3
           MOVE    WKSYU-JIHI-4            TO  SPA-GMN-I04-JIHI4
           MOVE    WKSYU-JIHI-5            TO  SPA-GMN-I04-JIHI5
           MOVE    WKSYU-JIHI-6            TO  SPA-GMN-I04-JIHI6
           MOVE    WKSYU-JIHI-7            TO  SPA-GMN-I04-JIHI7
           MOVE    WKSYU-JIHI-8            TO  SPA-GMN-I04-JIHI8
           MOVE    WKSYU-JIHI-9            TO  SPA-GMN-I04-JIHI9
           MOVE    WKSYU-JIHI-10           TO  SPA-GMN-I04-JIHI10
      *    自費計
           MOVE    WKSYU-JIHI-TOTAL        TO  SPA-GMN-I04-JIHIKEI
      *    自費（消費税あり）
           MOVE    WKSYU-JIHI-1-TAX        TO  SPA-GMN-I04-JIHITAX1
           MOVE    WKSYU-JIHI-2-TAX        TO  SPA-GMN-I04-JIHITAX2
           MOVE    WKSYU-JIHI-3-TAX        TO  SPA-GMN-I04-JIHITAX3
           MOVE    WKSYU-JIHI-4-TAX        TO  SPA-GMN-I04-JIHITAX4
           MOVE    WKSYU-JIHI-5-TAX        TO  SPA-GMN-I04-JIHITAX5
           MOVE    WKSYU-JIHI-6-TAX        TO  SPA-GMN-I04-JIHITAX6
           MOVE    WKSYU-JIHI-7-TAX        TO  SPA-GMN-I04-JIHITAX7
           MOVE    WKSYU-JIHI-8-TAX        TO  SPA-GMN-I04-JIHITAX8
           MOVE    WKSYU-JIHI-9-TAX        TO  SPA-GMN-I04-JIHITAX9
           MOVE    WKSYU-JIHI-10-TAX       TO  SPA-GMN-I04-JIHITAX10
      *    自費計（消費税あり）
           MOVE    WKSYU-JIHI-TOTAL-TAX    TO  SPA-GMN-I04-JIHITAXKEI
      *    自費消費税
           MOVE    WKSYU-JIHI-TAX          TO  SPA-GMN-I04-JIHIZEI
      *    食事負担額
           COMPUTE SPA-GMN-I04-SKYMONEY-SKJ
               =   WKSYU-SKYMONEY-SKJ-KEI
               +   WKSYU-SKYMONEY-SKJ-JIHI-KEI
      *    食事療養費
           COMPUTE SPA-GMN-I04-RYOYOHI-SKJ
               =   WKSYU-RYOYOHI-SKJ
               +   WKSYU-RYOYOHI-SKJ-JIHI
      *
      *    一部負担金
           COMPUTE SPA-GMN-I04-ICHIFTN =   SPA-GMN-I04-ROUFTN
                                       +   SPA-GMN-I04-KOHFTN
                                       +   SPA-GMN-I04-SKYMONEY-SKJ
      *
      *    労災初診
           MOVE    WKSYU-RSISHOSHIN-MONEY  TO  SPA-GMN-I04-ROUSYOSIN
      *    労災再診
           MOVE    WKSYU-RSISAISHIN-MONEY  TO  SPA-GMN-I04-ROUSAISIN
      *    労災指導
           MOVE    WKSYU-RSISDO-MONEY      TO  SPA-GMN-I04-ROUSIDOU
      *    労災その他
           MOVE    WKSYU-RSIETC-MONEY      TO  SPA-GMN-I04-ROUETC
      *
      *    室料差額
           MOVE    WKSYU-RMSAGAKU          TO  SPA-GMN-I04-RMSAGAKU
      *
      *    調整金
           MOVE    SPA-NAI-I04-CHOSEI (WRK-LST-IDX)
                                           TO  SPA-GMN-I04-CHOSEI
           MOVE    SPA-NAI-I04-CHOSEI-X (WRK-LST-IDX)
                                           TO  SPA-GMN-I04-CHOSEI-X
      *    今回請求額
           MOVE    SPA-NAI-I04-SEIKYU (WRK-LST-IDX)
                                           TO  SPA-GMN-I04-SEIKYU
      *
      *    入金額
           MOVE    SPA-NAI-I04-NYUKIN (WRK-LST-IDX)
                                           TO  SPA-GMN-I04-NYUKIN
           MOVE    SPA-NAI-I04-NYUKIN-X (WRK-LST-IDX)
                                           TO  SPA-GMN-I04-NYUKIN-X
      *
      *
      *    合計請求額
           COMPUTE SPA-GMN-I04-SEIGOK  =   SPA-NAI-I04-SEIKYU
                                                (SPA-I0-SYUNOU-CNT)
                                       +   SPA-GMN-I04-ZENMISYU-GAI
                                       +   SPA-GMN-I04-ZENMISYU-NYU
      *    自費名称編集
           PERFORM 3100-JIHIMSG-HEN-SEC
      *
            .
       3100-SEIKYU-DAT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    自費名称編集処理
      *****************************************************************
       3100-JIHIMSG-HEN-SEC             SECTION.
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    ZERO                TO  SYS-1013-STYUKYMD
           MOVE    ALL "9"             TO  SYS-1013-EDYUKYMD
           MOVE    SPA-NAI-I04-TAIINYMD
                                       TO  ORC-DBYMD
      *    システム管理検索
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY2"     TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
      *
           PERFORM UNTIL ( FLG-DBRC    NOT =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  SYS-1013-REC
      *
               IF    ( SPA-NYUGAIKBN       =   SYS-1013-KBNCD(1:1) )
                   EVALUATE    SYS-1013-KBNCD(2:1)
                       WHEN    "1"
                           MOVE    SYS-1013-JIHINAME   
                                           TO  SPA-GMN-I04-JIHIMSG (1)
                       WHEN    "2"
                           MOVE    SYS-1013-JIHINAME
                                           TO  SPA-GMN-I04-JIHIMSG (2)
                       WHEN    "3"
                           MOVE    SYS-1013-JIHINAME
                                           TO  SPA-GMN-I04-JIHIMSG (3)
                       WHEN    "4"
                           MOVE    SYS-1013-JIHINAME
                                           TO  SPA-GMN-I04-JIHIMSG (4)
                       WHEN    "5"
                           MOVE    SYS-1013-JIHINAME
                                           TO  SPA-GMN-I04-JIHIMSG (5)
                       WHEN    "6"
                           MOVE    SYS-1013-JIHINAME
                                           TO  SPA-GMN-I04-JIHIMSG (6)
                       WHEN    "7"
                           MOVE    SYS-1013-JIHINAME
                                           TO  SPA-GMN-I04-JIHIMSG (7)
                       WHEN    "8"
                           MOVE    SYS-1013-JIHINAME
                                           TO  SPA-GMN-I04-JIHIMSG (8)
                       WHEN    "9"
                           MOVE    SYS-1013-JIHINAME
                                           TO  SPA-GMN-I04-JIHIMSG (9)
                       WHEN    "0"
                           MOVE    SYS-1013-JIHINAME
                                           TO  SPA-GMN-I04-JIHIMSG(10)
                   END-EVALUATE
               END-IF
      *
               MOVE    "SYSKANRI-KEY2"      TO  WRK-DBPATH
               PERFORM 900-TBL-READ-SEC
           END-PERFORM
      *
           MOVE    "SYSKANRI-KEY2"     TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
       3100-JIHIMSG-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組み合わせリスト編集処理
      *****************************************************************
       3100-HKNCOMBILST-EDIT-SEC       SECTION.
      *
           MOVE    SPACE           TO  SPA-GMN-I04-HKNCOMBILST
                                       SPA-NAI-I04-HKNCOMBILST
      *
           INITIALIZE                  SPA-GMN-I04-HKNCOMBILST
                                       SPA-NAI-I04-HKNCOMBILST
      *
           MOVE    ZERO            TO  IDX-LST
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-I0-SYUNOU-CNT )
               IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX1) =   1 )
                   IF    ( SPA-I0-SYUNOU-HKNCOMBI-SU (IDX1)    =   1 )
                       COMPUTE IDX2    =   IDX1    +   1
                       COMPUTE IDX-LST =   IDX-LST     +   1
                       MOVE  IDX2  TO  SPA-NAI-I04-TSYUNOUIDX  (IDX-LST)
                       STRING  SPA-I0-SYUNOU-YM-W (IDX2)
                               "　"
                               SPA-I0-SYUNOU-HKNCOMBI (IDX2) (1 : 44 )
                               DELIMITED BY  SIZE
                       INTO    SPA-GMN-I04-THKNCOMBI (IDX-LST)
                       END-STRING
                   ELSE
                       COMPUTE IDX-LST =   IDX-LST     +   1
                       MOVE  IDX1  TO  SPA-NAI-I04-TSYUNOUIDX  (IDX-LST)
      *
                       MOVE  SPA-I0-SYUNOU-YM-W (IDX1)
                                   TO  SPA-GMN-I04-THKNCOMBI (IDX-LST)
      *
                       COMPUTE WRK-STIDX   =   IDX1    +   1
                       COMPUTE WRK-EDIDX
                           =   IDX1
                           +   SPA-I0-SYUNOU-HKNCOMBI-SU (IDX1)
                       PERFORM VARYING IDX2    FROM WRK-STIDX  BY  1
                               UNTIL ( IDX2    >    WRK-EDIDX )
                           COMPUTE IDX-LST =   IDX-LST     +   1
                           MOVE    IDX2
                                   TO  SPA-NAI-I04-TSYUNOUIDX (IDX-LST)
                           STRING  "　"
                                   SPA-I0-SYUNOU-HKNCOMBI (IDX2)
                                                          (1 : 44 )
                                   DELIMITED BY  SIZE
                           INTO    SPA-GMN-I04-THKNCOMBI (IDX-LST)
                           END-STRING
                       END-PERFORM
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-I0-SYUNOU-TUKI-SU (SPA-I0-SYUNOU-CNT)
                                       >   1 )
               COMPUTE IDX-LST =   IDX-LST     +   1
               MOVE    "合計"          TO  SPA-GMN-I04-THKNCOMBI
                                                               (IDX-LST)
               MOVE    SPA-I0-SYUNOU-CNT
                                       TO  SPA-NAI-I04-TSYUNOUIDX
                                                               (IDX-LST)
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >    IDX-LST )
               MOVE    IDX1        TO  SPA-GMN-I04-HKNCOMBILST-CNT
                                       SPA-GMN-I04-TSELNUM (IDX1)
                                       SPA-NAI-I04-TSELNUM (IDX1)
           END-PERFORM
      *
           MOVE    1               TO  SPA-GMN-I04-HKNCOMBILST-SEL
                                       SPA-GMN-I04-SELNUM
      *
            .
       3100-HKNCOMBILST-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  I04
      *
      *    患者番号
           MOVE    SPA-GMN-I04-PTNUM   TO  I04-PTNUM
      *    カナ氏名
           MOVE    SPA-GMN-I04-KANANAME
                                       TO  I04-KANANAME
      *    氏名
           MOVE    SPA-GMN-I04-NAME    TO  I04-NAME
      *    性別
           MOVE    SPA-GMN-I04-SEX     TO  I04-SEX
      *    保険組み合わせ
           MOVE    SPA-GMN-I04-HKNCOMBI
                                       TO  I04-HKNCOMBI
      *    負担率
           MOVE    SPA-GMN-I04-FTNRATE TO  I04-FTNRATE
      *    生年月日
           MOVE    SPA-GMN-I04-BIRTHDAY
                                       TO  I04-BIRTHDAY
      *    入院科
           MOVE    SPA-GMN-I04-NYUINKA TO  I04-SRYKA
      *    年齢
           MOVE    SPA-GMN-I04-AGE     TO  I04-NENREI
      *
      *    発効日
           MOVE    SPA-GMN-I04-HAKYMD  TO  I04-HAKYMD
      *    入院日
           MOVE    SPA-GMN-I04-NYUINYMD
                                       TO  I04-NYUINYMD
      *    退院日
           MOVE    SPA-GMN-I04-TAIINYMD
                                       TO  I04-TAIINYMD
      *    退院事由
           MOVE    SPA-GMN-I04-TAIINCD
                                       TO  I04-TAIINCD
      *    診療料
           MOVE    SPA-GMN-I04-HKNRYO (01)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-SSUHKNTEN
      *    指導料
           MOVE    SPA-GMN-I04-HKNRYO (02)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-SDOHKNTEN
      *    在宅料
           MOVE    SPA-GMN-I04-HKNRYO (03)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-ZTKHKNTEN
      *    投薬料
           MOVE    SPA-GMN-I04-HKNRYO (04)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-TYKHKNTEN
      *    注射料
           MOVE    SPA-GMN-I04-HKNRYO (05)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-CSYHKNTEN
      *    処置料
           MOVE    SPA-GMN-I04-HKNRYO (06)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-SYCHKNTEN
      *    手術料
           MOVE    SPA-GMN-I04-HKNRYO (07)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-SJTHKNTEN
      *    検査料
           MOVE    SPA-GMN-I04-HKNRYO (08)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-KNSHKNTEN
      *    Ｘ線料
           MOVE    SPA-GMN-I04-HKNRYO (09)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-GZUHKNTEN
      *    その他
           MOVE    SPA-GMN-I04-HKNRYO (10)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-ETCHKNTEN
      *    入院料
           MOVE    SPA-GMN-I04-HKNRYO (11)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-NYUHKNTEN
      *    合計点数
           MOVE    SPA-GMN-I04-HKNRYO (12)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-TOTALHKNTEN
      *    保険適用外
      *    診療料
           MOVE    SPA-GMN-I04-TGMONEY (01)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-SUUTGMONEY
      *    指導料
           MOVE    SPA-GMN-I04-TGMONEY (02)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-SDOTGMONEY
      *    在宅料
           MOVE    SPA-GMN-I04-TGMONEY (03)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-ZTKTGMONEY
      *    投薬料
           MOVE    SPA-GMN-I04-TGMONEY (04)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-TYKTGMONEY
      *    注射料
           MOVE    SPA-GMN-I04-TGMONEY (05)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-CSYTGMONEY
      *    処置料
           MOVE    SPA-GMN-I04-TGMONEY (06)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-SYCTGMONEY
      *    手術料
           MOVE    SPA-GMN-I04-TGMONEY (07)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-SJTGMONEY
      *    検査料
           MOVE    SPA-GMN-I04-TGMONEY (08)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-KNSTGMONEY
      *    Ｘ線料
           MOVE    SPA-GMN-I04-TGMONEY (09)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-GZUTGMONEY
      *    その他
           MOVE    SPA-GMN-I04-TGMONEY (10)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-ETCTGMONEY
      *
      *    入院料
           MOVE    SPA-GMN-I04-TGMONEY (11)
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-NYUTGMONEY
      *
      *    自費
           MOVE    SPA-GMN-I04-JIHI1   TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHI1
           MOVE    SPA-GMN-I04-JIHI2   TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHI2
           MOVE    SPA-GMN-I04-JIHI3   TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHI3
           MOVE    SPA-GMN-I04-JIHI4   TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHI4
           MOVE    SPA-GMN-I04-JIHI5   TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHI5
           MOVE    SPA-GMN-I04-JIHI6   TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHI6
           MOVE    SPA-GMN-I04-JIHI7   TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHI7
           MOVE    SPA-GMN-I04-JIHI8   TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHI8
           MOVE    SPA-GMN-I04-JIHI9   TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHI9
           MOVE    SPA-GMN-I04-JIHI10  TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHI10
      *    自費計
           MOVE    SPA-GMN-I04-JIHIKEI TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHIGOK
      *    自費（消費税あり）
           MOVE    SPA-GMN-I04-JIHITAX1
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHITAX1
           MOVE    SPA-GMN-I04-JIHITAX2
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHITAX2
           MOVE    SPA-GMN-I04-JIHITAX3
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHITAX3
           MOVE    SPA-GMN-I04-JIHITAX4
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHITAX4
           MOVE    SPA-GMN-I04-JIHITAX5
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHITAX5
           MOVE    SPA-GMN-I04-JIHITAX6
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHITAX6
           MOVE    SPA-GMN-I04-JIHITAX7
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHITAX7
           MOVE    SPA-GMN-I04-JIHITAX8
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHITAX8
           MOVE    SPA-GMN-I04-JIHITAX9
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHITAX9
           MOVE    SPA-GMN-I04-JIHITAX10
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHITAX10
      *    自費計
           MOVE    SPA-GMN-I04-JIHITAXKEI  TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHITAXGOK
      *    自費税
           MOVE    SPA-GMN-I04-JIHIZEI     TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-JIHITAX
      *    食事療養費
           MOVE    SPA-GMN-I04-RYOYOHI-SKJ TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-RYOYOHI-SKJ
      *    食事負担額
           MOVE    SPA-GMN-I04-SKYMONEY-SKJ
                                           TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-SKYMONEY-SKJ
      *    老人一部負担金
           MOVE    SPA-GMN-I04-ROUFTN      TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-RJNFTN
      *    公費一部負担金
           MOVE    SPA-GMN-I04-KOHFTN      TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-KOHFTN
      *    一部負担金計
           MOVE    SPA-GMN-I04-ICHIFTN     TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-FTNTOTAL
      *    保険適用分
           MOVE    SPA-GMN-I04-HKNTEKI     TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-HKNTEKMONEY
      *    保険適用外
           MOVE    SPA-GMN-I04-HKNGAI      TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-HKNTEKGAI
      *    今回請求額
           MOVE    SPA-GMN-I04-SEIKYU  TO  WRK-Z99
           MOVE    WRK-Z99             TO  I04-SKYMONEY
      *
      *    前回未収（外来）
           MOVE    SPA-GMN-I04-ZENMISYU-GAI
                                       TO  WRK-Z92
           MOVE    WRK-Z92             TO  I04-MISYUZAN-GAI
      *
      *    前回未収（入院）
           MOVE    SPA-GMN-I04-ZENMISYU-NYU
                                       TO  WRK-Z92
           MOVE    WRK-Z92             TO  I04-MISYUZAN-NYU
      *    合計請求額
           MOVE    SPA-GMN-I04-SEIGOK  TO  WRK-Z99
           MOVE    WRK-Z99             TO  I04-GOKSKY
      *
      *
      *    自費名称
           MOVE    SPA-GMN-I04-JIHIMSG(1)
                                       TO  I04-JIHIMSG1
           MOVE    SPA-GMN-I04-JIHIMSG(2)
                                       TO  I04-JIHIMSG2
           MOVE    SPA-GMN-I04-JIHIMSG(3)
                                       TO  I04-JIHIMSG3
           MOVE    SPA-GMN-I04-JIHIMSG(4)
                                       TO  I04-JIHIMSG4
           MOVE    SPA-GMN-I04-JIHIMSG(5)
                                       TO  I04-JIHIMSG5
           MOVE    SPA-GMN-I04-JIHIMSG(6)
                                       TO  I04-JIHIMSG6
           MOVE    SPA-GMN-I04-JIHIMSG(7)
                                       TO  I04-JIHIMSG7
           MOVE    SPA-GMN-I04-JIHIMSG(8)
                                       TO  I04-JIHIMSG8
           MOVE    SPA-GMN-I04-JIHIMSG(9)
                                       TO  I04-JIHIMSG9
           MOVE    SPA-GMN-I04-JIHIMSG(10)
                                       TO  I04-JIHIMSG10
      *
      *    労災初診
           MOVE    SPA-GMN-I04-ROUSYOSIN
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-ROUSYOSIN
      *    労災再診
           MOVE    SPA-GMN-I04-ROUSAISIN
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-ROUSAISIN
      *    労災指導
           MOVE    SPA-GMN-I04-ROUSIDOU
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-ROUSIDOU
      *    労災その他
           MOVE    SPA-GMN-I04-ROUETC  TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-ROUETC
      *    室料差額
           MOVE    SPA-GMN-I04-RMSAGAKU
                                       TO  WRK-Z9
           MOVE    WRK-Z9              TO  I04-RMSAGAKU
      *    選択番号
           MOVE    SPA-GMN-I04-SELNUM  TO  I04-SELNUM
      *
      *    調整金
           MOVE    SPA-GMN-I04-CHOSEI-X
                                       TO  I04-CHOSEI
      *
      *    入金額
           MOVE    SPA-GMN-I04-NYUKIN-X
                                       TO  I04-NYUKIN
      *
      *    入金方法
           MOVE    SPA-GMN-I04-NYKNHOHO
                                       TO  I04-NYKNHOHO
           MOVE    SPA-GMN-I04-NYKNHOHO-CNT
                                       TO  I04-NYKNHOHO-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-I04-NYKNHOHO-CNT )
               MOVE    SPA-GMN-I04-NYKNHOHO-ITM (IDX1)
                                       TO  I04-NYKNHOHO-ITM (IDX1)
           END-PERFORM
      *
      *    請求書兼領収書発行
           MOVE    SPA-GMN-I04-SKYPRTOUT
                                       TO  I04-SKYPRTOUT
           MOVE    SPA-GMN-I04-SKYPRTOUT-CNT
                                       TO  I04-SKYPRTOUT-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-I04-SKYPRTOUT-CNT )
               MOVE    SPA-GMN-I04-SKYPRTOUT-ITM (IDX1)
                                       TO  I04-SKYPRTOUT-ITM (IDX1)
           END-PERFORM
      *
      *    退院証明書発行
           MOVE    SPA-GMN-I04-TAIPRTOUT
                                       TO  I04-TAIPRTOUT
           MOVE    SPA-GMN-I04-TAIPRTOUT-CNT
                                       TO  I04-TAIPRTOUT-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-I04-TAIPRTOUT-CNT )
               MOVE    SPA-GMN-I04-TAIPRTOUT-ITM (IDX1)
                                       TO  I04-TAIPRTOUT-ITM (IDX1)
           END-PERFORM
      *
      *    主治医氏名
           MOVE    SPA-GMN-I04-DR      TO  I04-DR
           MOVE    SPA-GMN-I04-DR-CNT  TO  I04-DR-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-I04-DR-CNT )
               MOVE    SPA-GMN-I04-DR-ITM (IDX1)
                                       TO  I04-DR-ITM (IDX1)
           END-PERFORM
      *
      *    保険組み合わせリスト
           MOVE    SPA-GMN-I04-HKNCOMBILST-CNT
                                       TO  I04-HKNCOMBILST-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-I04-HKNCOMBILST-CNT )
      *
               MOVE    SPA-NAI-I04-TSELNUM (IDX1)
                                       TO  I04-TSELNUM (IDX1)
      *
               MOVE    SPA-GMN-I04-THKNCOMBI (IDX1)
                                       TO  I04-THKNCOMBI (IDX1)
               IF    ( SPA-GMN-I04-HKNCOMBILST-SEL =   IDX1 )
                   MOVE    "T"         TO  I04-HKNCOMBILST-SELECT (IDX1)
               ELSE
                   MOVE    "F"         TO  I04-HKNCOMBILST-SELECT (IDX1)
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-I01-I04-PROCKBN =   "1" )
               MOVE    "black"             TO  I04-LBLTAIINKEISAN-STYLE
               MOVE    SPACE               TO  I04-LBLTAIINKEISAN
           ELSE
               MOVE    "red"               TO  I04-LBLTAIINKEISAN-STYLE
               MOVE    "［退院再計算］"    TO  I04-LBLTAIINKEISAN
           END-IF
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
           MOVE    SPA-GMN-I04-CHOSEI-STT
                                   TO  I04-CHOSEI-STT
           MOVE    SPA-GMN-I04-NYUKIN-STT
                                   TO  I04-NYUKIN-STT
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
      *    カーソル指定のない時、入力した項目の次ぎへ
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-I04-CUR     =   ZERO )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-I04-CUR
           WHEN    001
               IF    ( SPA-GMN-I04-NYUKIN-STT
                                   =   WIDGET-NORMAL )
                   MOVE    "NYUKIN"        TO  MCP-WIDGET
               ELSE
                   MOVE    "SKYPRTOUT"     TO  MCP-WIDGET
               END-IF
           WHEN    002
               MOVE    "SKYPRTOUT"     TO  MCP-WIDGET
           WHEN    003
               MOVE    "TAIPRTOUT"     TO  MCP-WIDGET
           WHEN    004
               MOVE    "DR"            TO  MCP-WIDGET
           WHEN    005
               MOVE    "NYKNHOHO"      TO  MCP-WIDGET
           WHEN    101
               MOVE    "SELNUM"        TO  MCP-WIDGET
           WHEN    102
               IF    ( SPA-GMN-I04-CHOSEI-STT
                                   =   WIDGET-NORMAL )
                   MOVE    "CHOSEI"    TO  MCP-WIDGET
               END-IF
           WHEN    212
               MOVE    "B12"           TO  MCP-WIDGET
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
           WHEN    "NYUKIN"
               MOVE    002             TO  SPA-GMN-I04-CUR
           WHEN    "SKYPRTOUT"
               MOVE    003             TO  SPA-GMN-I04-CUR
           WHEN    "TAIPRTOUT"
               MOVE    004             TO  SPA-GMN-I04-CUR
           WHEN    "DR"
               MOVE    212             TO  SPA-GMN-I04-CUR
           WHEN    "SELNUM"
               MOVE    001             TO  SPA-GMN-I04-CUR
           WHEN    "CHOSEI"
               MOVE    212             TO  SPA-GMN-I04-CUR
           WHEN    "NYKNHOHO"
               MOVE    002             TO  SPA-GMN-I04-CUR
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
           EVALUATE    MCP-EVENT     ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE    "B01"         TO  SPA-I01-I04-BTN
               MOVE    "CHANGE"      TO  MCP-PUTTYPE
               PERFORM 210-BACK
      *    調整
           WHEN    "CLICKED"       ALSO    "B02"
               MOVE    102           TO    SPA-GMN-I04-CUR
      *    選択
           WHEN    "CLICKED"       ALSO    "B03"
               MOVE    101           TO    SPA-GMN-I04-CUR
      *    コンボ初期化（テスト用）
           WHEN    "CLICKED"       ALSO    "B07"
               MOVE    SPA-GMN-I04-SKYPRTOUT-ITM (2)
                                     TO    SPA-GMN-I04-SKYPRTOUT
               MOVE    SPA-GMN-I04-TAIPRTOUT-ITM (2)
                                     TO    SPA-GMN-I04-TAIPRTOUT
      *    コンボ初期化（テスト用）
      *    WHEN    "CLICKED"       ALSO    "B08"
      *        MOVE    SPA-GMN-I04-SKYPRTOUT-ITM (1)
      *                              TO    SPA-GMN-I04-SKYPRTOUT
      *        MOVE    SPA-GMN-I04-TAIPRTOUT-ITM (1)
      *                              TO    SPA-GMN-I04-TAIPRTOUT
      *    登録
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 450-TOUROKU-SEC
           END-EVALUATE
          .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    入力個所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-I04-CUR
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    選択番号チェック
           WHEN    "ACTIVATE"      ALSO    "SELNUM"
               PERFORM 4100-SELNUM-CHK-SEC
           WHEN    OTHER
      *
      *        入力個所のセット
               PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
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
           WHEN        ANY         ALSO    "HKNCOMBILST"
               PERFORM 4101-HKNCOMBILST-SEL-SEC
           WHEN        OTHER
               MOVE    ZERO        TO      SPA-GMN-I04-CUR
           END-EVALUATE
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院歴リスト選択処理
      *****************************************************************
       4101-HKNCOMBILST-SEL-SEC        SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO TO   4101-HKNCOMBILST-SEL-EXT
           END-IF
      *
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL     ( IDX1    >   SPA-GMN-I04-HKNCOMBILST-CNT )
      *
               IF    ( I04-HKNCOMBILST-SELECT (IDX1)   =   "T"  )
                   MOVE    IDX1        TO  SPA-GMN-I04-HKNCOMBILST-SEL
                   MOVE    SPA-GMN-I04-TSELNUM (IDX1)
                                       TO  SPA-GMN-I04-SELNUM
      *
                   MOVE    SPA-NAI-I04-TSYUNOUIDX (SPA-GMN-I04-SELNUM)
                                       TO  WRK-LST-IDX
                   PERFORM 3100-SEIKYU-DAT-EDIT-SEC
      *
      *            調整金
                   MOVE    SPA-NAI-I04-CHOSEI-X (WRK-LST-IDX)
                                           TO  I04-CHOSEI
      *            入金額
                   MOVE    SPA-NAI-I04-NYUKIN-X (WRK-LST-IDX)
                                           TO  I04-NYUKIN
      *
                   MOVE    SPA-GMN-I04-HKNCOMBILST-CNT
                                           TO  IDX1
               END-IF
      *
           END-PERFORM
      *
           MOVE    1                   TO  SPA-GMN-I04-CUR
           .
      *
       4101-HKNCOMBILST-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
      *    調整金
           MOVE    I04-CHOSEI      TO  SPA-GMN-I04-CHOSEI-X
      *
      *    入金額
           MOVE    I04-NYUKIN      TO  SPA-GMN-I04-NYUKIN-X
      *
      *    入金方法
           MOVE    I04-NYKNHOHO    TO  SPA-GMN-I04-NYKNHOHO
      *
      *    請求書兼領収書発行
           MOVE    I04-SKYPRTOUT   TO  SPA-GMN-I04-SKYPRTOUT
      *
      *    退院証明書発行
           MOVE    I04-TAIPRTOUT   TO  SPA-GMN-I04-TAIPRTOUT
      *
      *    主治医氏名
           MOVE    I04-DR          TO  SPA-GMN-I04-DR
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェックと別画面処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    調整金チェック処理
           PERFORM 4100-CHOSEI-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    入金額チェック処理
           PERFORM 4100-NYUKIN-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    入金方法チェック処理
           PERFORM 4100-NYKNHOHO-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    請求書兼領収書発行チェック処理
           PERFORM 4100-SKYPRTOUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    退院証明書発行チェック処理
           PERFORM 4100-TAIPRTOUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    主治医氏名チェック処理
           PERFORM 4100-DR-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    調整金チェック処理
      *****************************************************************
       4100-CHOSEI-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-I04-HKNCOMBILST-SEL >   ZERO )
            AND  ( SPA-GMN-I04-HKNCOMBILST-SEL
                           <=  SPA-GMN-I04-HKNCOMBILST-CNT )
               CONTINUE
           ELSE
               GO  TO  4100-CHOSEI-CHK-EXT
           END-IF
      *
           MOVE    SPA-NAI-I04-TSYUNOUIDX
                                   (SPA-GMN-I04-HKNCOMBILST-SEL)
                                       TO  IDXZ
           IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDXZ)
                                   NOT =   ZERO )
               GO  TO  4100-CHOSEI-CHK-EXT
           END-IF
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN-I04-CHOSEI-X    TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-SEISUU         >   7     )   OR
                  (SNUM-SYOKBN         =   "1"   )
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    102                 TO  SPA-GMN-I04-CUR
           ELSE
               MOVE    SNUM-OUTNUM         TO  SPA-GMN-I04-CHOSEI
                                               SPA-NAI-I04-CHOSEI
                                                             (IDXZ)
               MOVE    SPA-GMN-I04-CHOSEI  TO  WRK-Z92
               MOVE    WRK-Z92             TO  SPA-GMN-I04-CHOSEI-X
                                               SPA-NAI-I04-CHOSEI-X
                                                             (IDXZ)
               IF    ( SPA-GMN-I04-CHOSEI  NOT =   ZERO )
      *            今回請求額がゼロ以上であること
                   COMPUTE WRK-SEIKYU      =   SPA-NAI-I04-CHOSEI (IDXZ)
                                           +   SPA-NAI-I04-SEIKYU-KON
                                                                  (IDXZ)
                   IF     (WRK-SEIKYU          <   ZERO )  AND
                          (SPA-NAI-I04-SEIKYU-KON (IDXZ)
                                               >   ZERO )
                       MOVE    "0004"      TO  SPA-ERRCD
                       MOVE    102         TO  SPA-GMN-I04-CUR
                       GO      TO      4100-CHOSEI-CHK-EXT
                   END-IF
               END-IF
      *
               MOVE    IDXZ                TO  IDX-G
      *
      *        合計再計算
               PERFORM 4101-SEIKYU-KEI-SEC
      *
      *        合計額がゼロ以下の時、発行なしとする
               EVALUATE    WRK-MCP-WIDGET
      *        調整金
               WHEN    "CHOSEI"
                   IF    ( SPA-GMN-I04-SEIGOK  <=  ZERO )
                       MOVE    SPA-GMN-I04-SKYPRTOUT-ITM (1)
                                               TO  SPA-GMN-I04-SKYPRTOUT
                   END-IF
               END-EVALUATE
           END-IF
      *
           .
       4100-CHOSEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金チェック処理
      *****************************************************************
       4100-NYUKIN-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-I04-HKNCOMBILST-SEL >   ZERO )
            AND  ( SPA-GMN-I04-HKNCOMBILST-SEL
                           <=  SPA-GMN-I04-HKNCOMBILST-CNT )
               CONTINUE
           ELSE
               GO  TO  4100-NYUKIN-CHK-EXT
           END-IF
      *
           MOVE    SPA-NAI-I04-TSYUNOUIDX
                                  (SPA-GMN-I04-HKNCOMBILST-SEL)
                                       TO  IDXZ
           IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDXZ)
                                   NOT =   ZERO )
               GO  TO  4100-NYUKIN-CHK-EXT
           END-IF
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN-I04-NYUKIN-X    TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-SEISUU         >   7     )   OR
                  (SNUM-SYOKBN         =   "1"   )
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-I04-CUR
           ELSE
               MOVE    SNUM-OUTNUM         TO  SPA-GMN-I04-NYUKIN
                                               SPA-NAI-I04-NYUKIN (IDXZ)
               MOVE    SPA-GMN-I04-NYUKIN  TO  WRK-Z92
               MOVE    WRK-Z92             TO  SPA-GMN-I04-NYUKIN-X
                                               SPA-NAI-I04-NYUKIN-X
                                                                  (IDXZ)
      *        今回入金がゼロ以上であること
               IF    ( SPA-GMN-I04-NYUKIN  <   ZERO )
                   MOVE    "0007"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-I04-CUR
                   GO  TO      4100-NYUKIN-CHK-EXT
               END-IF
      *        今回入金が請求額以下であること
               IF    ( SPA-GMN-I04-SEIKYU  >   ZERO  )  AND
                     ( SPA-GMN-I04-NYUKIN  >   SPA-GMN-I04-SEIKYU )
                   MOVE    "0008"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-I04-CUR
                   GO  TO      4100-NYUKIN-CHK-EXT
               END-IF
      *
      *        合計行編集処理
               PERFORM 4101-TTL-LINE-EDIT-SEC
      *
           END-IF
      *
           .
       4100-NYUKIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    合計請求額計算処理
      *****************************************************************
       4101-SEIKYU-KEI-SEC                  SECTION.
      *
      *    今回請求額
           COMPUTE SPA-NAI-I04-SEIKYU (IDX-G)
                                       =   SPA-NAI-I04-SEIKYU-KON
                                                              (IDX-G)
                                       +   SPA-NAI-I04-CHOSEI (IDX-G)
      *
           MOVE    SPA-NAI-I04-SEIKYU (IDX-G)
                                       TO  SPA-GMN-I04-SEIKYU
      *
      *    今回入金
           IF    ( SPA-GMN-I04-SEIKYU  <   ZERO )
               MOVE    ZERO            TO  SPA-NAI-I04-NYUKIN (IDX-G)
           ELSE
               MOVE    SPA-GMN-I04-SEIKYU
                                       TO  SPA-NAI-I04-NYUKIN (IDX-G)
           END-IF
      *
           MOVE    SPA-NAI-I04-NYUKIN (IDX-G)
                                       TO  WRK-Z92
           MOVE    WRK-Z92             TO  SPA-NAI-I04-NYUKIN-X (IDX-G)
      *
      *    合計行編集処理
           PERFORM 4101-TTL-LINE-EDIT-SEC
      *
      *    合計請求額
           COMPUTE SPA-GMN-I04-SEIGOK  =   SPA-NAI-I04-SEIKYU
                                                (SPA-I0-SYUNOU-CNT)
                                       +   SPA-GMN-I04-ZENMISYU-GAI
                                       +   SPA-GMN-I04-ZENMISYU-NYU
           .
       4101-SEIKYU-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求書兼領収書発行チェック処理
      *****************************************************************
       4101-TTL-LINE-EDIT-SEC          SECTION.
      *
           MOVE    1               TO  WRK-SYU-SUM-IDX
      *
           COMPUTE WRK-IDX
               =   SPA-I0-SYUNOU-CNT   -   1
      *
           MOVE    ZERO    TO  SPA-NAI-I04-SEIKYU (SPA-I0-SYUNOU-CNT)
                               SPA-NAI-I04-CHOSEI (SPA-I0-SYUNOU-CNT)
                               SPA-NAI-I04-NYUKIN (SPA-I0-SYUNOU-CNT)
      *
           PERFORM VARYING IDXY    FROM    1   BY  1
                   UNTIL ( IDXY            >   WRK-IDX )
                    OR   ( IDXY            >   15 )
      *
               IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDXY)
                                           =   1 )
      *
                   MOVE    IDXY    TO  WRK-SYU-SUM-IDX
                   MOVE    ZERO
                           TO  SPA-NAI-I04-SEIKYU (WRK-SYU-SUM-IDX)
                               SPA-NAI-I04-CHOSEI (WRK-SYU-SUM-IDX)
                               SPA-NAI-I04-NYUKIN (WRK-SYU-SUM-IDX)
               ELSE
      *
      *            調整金額
                   COMPUTE SPA-NAI-I04-SEIKYU (WRK-SYU-SUM-IDX)
                       =   SPA-NAI-I04-SEIKYU (WRK-SYU-SUM-IDX)
                       +   SPA-NAI-I04-SEIKYU (IDXY)
                   COMPUTE SPA-NAI-I04-SEIKYU (SPA-I0-SYUNOU-CNT)
                       =   SPA-NAI-I04-SEIKYU (SPA-I0-SYUNOU-CNT)
                       +   SPA-NAI-I04-SEIKYU (IDXY)
      *
      *            請求金額
                   COMPUTE SPA-NAI-I04-CHOSEI (WRK-SYU-SUM-IDX)
                       =   SPA-NAI-I04-CHOSEI (WRK-SYU-SUM-IDX)
                       +   SPA-NAI-I04-CHOSEI (IDXY)
                   COMPUTE SPA-NAI-I04-CHOSEI (SPA-I0-SYUNOU-CNT)
                       =   SPA-NAI-I04-CHOSEI (SPA-I0-SYUNOU-CNT)
                       +   SPA-NAI-I04-CHOSEI (IDXY)
      *
      *            入金額
                   COMPUTE SPA-NAI-I04-NYUKIN (WRK-SYU-SUM-IDX)
                       =   SPA-NAI-I04-NYUKIN (WRK-SYU-SUM-IDX)
                       +   SPA-NAI-I04-NYUKIN (IDXY)
                   COMPUTE SPA-NAI-I04-NYUKIN (SPA-I0-SYUNOU-CNT)
                       =   SPA-NAI-I04-NYUKIN (SPA-I0-SYUNOU-CNT)
                       +   SPA-NAI-I04-NYUKIN (IDXY)
      *
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDXY    FROM    1   BY  1
                   UNTIL ( IDXY            >   SPA-I0-SYUNOU-CNT )
                    OR   ( IDXY            >   15 )
               IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDXY)
                                           =   1 OR 2)
                   MOVE    SPA-NAI-I04-CHOSEI (IDXY)
                                           TO  WRK-Z9
                   MOVE    WRK-Z9          TO  SPA-NAI-I04-CHOSEI-X
                                                               (IDXY)
                   MOVE    SPA-NAI-I04-NYUKIN (IDXY)
                                           TO  WRK-Z9
                   MOVE    WRK-Z9          TO  SPA-NAI-I04-NYUKIN-X
                                                               (IDXY)
               END-IF
           END-PERFORM
      *
           .
       4101-TTL-LINE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入金方法チェック処理
      *****************************************************************
       4100-NYKNHOHO-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-I04-NYKNHOHO   =   SPACE )
               MOVE    "0009"      TO  SPA-ERRCD
               MOVE    5           TO  SPA-GMN-I04-CUR
               GO  TO  4100-NYKNHOHO-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I04-NYKNHOHO-CD
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-NYKNHOHO-SRH-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    "0010"      TO  SPA-ERRCD
               MOVE    5           TO  SPA-GMN-I04-CUR
               GO  TO  4100-NYKNHOHO-CHK-EXT
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I04-NYKNHOHO
               IF    ( WRK-CMB-ITM2    NOT = SPA-NAI-I04-NYKNJYOTAI )
                   MOVE    1       TO  SPA-GMN-I04-CUR
               END-IF
               MOVE   WRK-CMB-ITM2 TO  SPA-NAI-I04-NYKNJYOTAI
           END-IF
      *
           .
       4100-NYKNHOHO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書発行チェック処理
      *****************************************************************
       4100-SKYPRTOUT-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-I04-SKYPRTOUT   =   SPACE )
               MOVE    SPA-GMN-I04-SKYPRTOUT-ITM (1)
                                   TO  SPA-GMN-I04-SKYPRTOUT
               GO  TO  4100-SKYPRTOUT-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I04-SKYPRTOUT-CD
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-SKYPRTOUT-SRH-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-I04-SKYPRTOUT-ITM (1)
                                   TO  SPA-GMN-I04-SKYPRTOUT
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I04-SKYPRTOUT
           END-IF
      *
           .
       4100-SKYPRTOUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院証明書発行チェック処理
      *****************************************************************
       4100-TAIPRTOUT-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-I04-TAIPRTOUT   =   SPACE )
               MOVE    SPA-GMN-I04-TAIPRTOUT-ITM (1)
                                   TO  SPA-GMN-I04-TAIPRTOUT
               GO  TO  4100-TAIPRTOUT-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I04-TAIPRTOUT-CD
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-TAIPRTOUT-SRH-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-I04-TAIPRTOUT-ITM (2)
                                   TO  SPA-GMN-I04-TAIPRTOUT
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I04-TAIPRTOUT
           END-IF
      *
           .
       4100-TAIPRTOUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    主治医氏名チェック処理
      *****************************************************************
       4100-DR-CHK-SEC                 SECTION.
      *
           IF    ( SPA-GMN-I04-DR      =   SPACE )
               GO  TO  4100-DR-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I04-DR-CD
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-DR-SRH-SEC
           MOVE    WRK-CMB-ITM TO  SPA-GMN-I04-DR
      *
           .
       4100-DR-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4100-SELNUM-CHK-SEC             SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4100-SELNUM-CHK-EXT
           END-IF
      *
           MOVE    101                 TO  SPA-GMN-I04-CUR
           MOVE    I04-SELNUM          TO  SPA-GMN-I04-SELNUM
      *
           IF    ( SPA-GMN-I04-SELNUM  =   ZERO )
               GO  TO  4100-SELNUM-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-I04-SELNUM  >   SPA-GMN-I04-HKNCOMBILST-CNT )
               MOVE    "0001"          TO  SPA-ERRCD
               GO  TO  4100-SELNUM-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I04-SELNUM  TO  SPA-GMN-I04-HKNCOMBILST-SEL
      *
           MOVE    SPA-NAI-I04-TSYUNOUIDX (SPA-GMN-I04-SELNUM)
                                       TO  WRK-LST-IDX
           PERFORM 3100-SEIKYU-DAT-EDIT-SEC
      *
           MOVE    1                   TO  SPA-GMN-I04-CUR
      *
           .
       4100-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入金方法サーチ処理
      *****************************************************************
       4103-CMB-NYKNHOHO-SRH-SEC   SECTION.
      *
           INITIALIZE              WRK-CMB-ITM
                                   WRK-CMB-ITM2
                                   WRK-CMB-IDX
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I04-NYKNHOHO-CNT )
                     OR  ( SPA-GMN-I04-NYKNHOHO-ITM (IDX-CMB)(1:2)
                                   =   WRK-CMB-CD )
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( SPA-GMN-I04-NYKNHOHO-CNT
                                   >=  IDX-CMB    )
               MOVE    SPA-GMN-I04-NYKNHOHO-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
               MOVE    SPA-GMN-I04-NYKNJYOTAI-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM2
               MOVE    IDX-CMB     TO  WRK-CMB-IDX
           END-IF
      *
           .
      *
       4103-CMB-NYKNHOHO-SRH-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書発行サーチ処理
      *****************************************************************
       4103-CMB-SKYPRTOUT-SRH-SEC  SECTION.
      *
           INITIALIZE              WRK-CMB-ITM
                                   WRK-CMB-IDX
      *
           SET     CMB-IDX12       TO  1
           SEARCH  SPA-GMN-I04-SKYPRTOUT-ITM
                   VARYING CMB-IDX12
           AT  END
               INITIALIZE          WRK-CMB-ITM
                                   WRK-CMB-IDX
           WHEN    SPA-GMN-I04-SKYPRTOUT-ITM (CMB-IDX12) (1 : 1)
                                   =   WRK-CMB-CD
               SET WRK-CMB-IDX     TO  CMB-IDX12
               MOVE    SPA-GMN-I04-SKYPRTOUT-ITM (CMB-IDX12)
                                   TO  WRK-CMB-ITM
           WHEN    SPA-GMN-I04-SKYPRTOUT-ITM (CMB-IDX12)
                                   =   SPACE
               INITIALIZE          WRK-CMB-ITM
                                   WRK-CMB-IDX
           END-SEARCH
      *
           .
      *
       4103-CMB-SKYPRTOUT-SRH-EXT.
           EXIT.
      *****************************************************************
      *    退院証明書発行サーチ処理
      *****************************************************************
       4103-CMB-TAIPRTOUT-SRH-SEC  SECTION.
      *
           INITIALIZE              WRK-CMB-ITM
                                   WRK-CMB-IDX
      *
           SET     CMB-IDX13       TO  1
           SEARCH  SPA-GMN-I04-TAIPRTOUT-ITM
                   VARYING CMB-IDX13
           AT  END
               INITIALIZE          WRK-CMB-ITM
                                   WRK-CMB-IDX
           WHEN    SPA-GMN-I04-TAIPRTOUT-ITM (CMB-IDX13) (1 : 1)
                                   =   WRK-CMB-CD
               SET WRK-CMB-IDX     TO  CMB-IDX13
               MOVE    SPA-GMN-I04-TAIPRTOUT-ITM (CMB-IDX13)
                                   TO  WRK-CMB-ITM
           WHEN    SPA-GMN-I04-TAIPRTOUT-ITM (CMB-IDX13)
                                   =   SPACE
               INITIALIZE          WRK-CMB-ITM
                                   WRK-CMB-IDX
           END-SEARCH
      *
           .
      *
       4103-CMB-TAIPRTOUT-SRH-EXT.
           EXIT.
      *****************************************************************
      *    主治医サーチ処理
      *****************************************************************
       4103-CMB-DR-SRH-SEC         SECTION.
      *
           INITIALIZE              WRK-CMB-ITM
                                   WRK-CMB-IDX
      *
           SET     CMB-IDX15       TO  1
           SEARCH  SPA-GMN-I04-DR-ITM
                   VARYING CMB-IDX15
           AT  END
               INITIALIZE          WRK-CMB-ITM
                                   WRK-CMB-IDX
           WHEN    SPA-GMN-I04-DR-ITM (CMB-IDX15) (1 : 4)
                                   =   WRK-CMB-CD
               SET WRK-CMB-IDX     TO  CMB-IDX15
               MOVE    SPA-GMN-I04-DR-ITM (CMB-IDX15)
                                   TO  WRK-CMB-ITM
           WHEN    SPA-GMN-I04-DR-ITM (CMB-IDX15)
                                   =   SPACE
               INITIALIZE          WRK-CMB-ITM
                                   WRK-CMB-IDX
           END-SEARCH
      *
           .
      *
       4103-CMB-DR-SRH-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "I01"               TO  SPA-SAKIPG
           MOVE    "I04"               TO  SPA-MOTOPG
      *
      ***  MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       450-TOUROKU-SEC             SECTION.
      *
      *    定期請求処理中チェック
           PERFORM 4801-TEIKI-JOB-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  450-TOUROKU-EXT
           END-IF
      *
      *    入力時チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  450-TOUROKU-EXT
           END-IF
      *
      *    関連チェック処理
           PERFORM 4801-KANREN-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  450-TOUROKU-EXT
           END-IF
      *
           IF    ( SPA-I01-I04-PROCKBN     =   "2" )
               MOVE    "3001"      TO  SPA-I0IDCD
               GO  TO  450-TOUROKU-EXT
           END-IF
      *
      *    ＤＢ更新処理
           PERFORM 451-TOUROKU-SYORI-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  450-TOUROKU-EXT
           END-IF
      *
      *    帳票出力処理
           PERFORM 451-PRTOUT-SEC
      *
      *    前回患者情報を設定
           MOVE SPA-NAI-I04-PTID   TO  SPA-LAST-PTID
           MOVE SPA-GMN-I04-PTNUM  TO  SPA-LAST-PTNUM
      *
           MOVE    "B12"           TO  SPA-I01-I04-BTN
           MOVE    "JOIN"          TO  MCP-PUTTYPE
           PERFORM 210-BACK
      *
           .
       450-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       451-TOUROKU-SYORI-SEC           SECTION.
      *
           MOVE    SPACE               TO  WKPTNYUINRRK-REC
           INITIALIZE                      WKPTNYUINRRK-REC
      *
      *    退院日更新処理
           PERFORM 4801-TAIINYMD-UPD-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  451-TOUROKU-SYORI-EXT
           END-IF
      *
           MOVE    WKPTNYUINRRK-REC    TO  PTNYUINRRK-REC
      *
           IF    ( SPA-I01-I04-TORIKESI-FLG    =   1 )
      *        請求取消更新処理
               PERFORM 4701-TORIKESI-KOUSHIN-SEC
           END-IF
      *
      *    入院会計テーブルを編集する（退院時）
           PERFORM 4801-TAIINKAIKEI-EDIT-SEC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-I0-SYUNOU-CNT )
               IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX1)
                                   =   ZERO )
                   MOVE    IDX1    TO  IDX-SYU
      *            収納マスタ更新処理
                   PERFORM 4801-SYUNOU-UPD-SEC
                   IF    ( SPA-ERRCD       NOT =   SPACE )
                       GO  TO  451-TOUROKU-SYORI-EXT
                   END-IF
               END-IF
           END-PERFORM
      *
      *    患者情報の入院歴番号を更新する
           PERFORM 4801-PTINF-UPD-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  451-TOUROKU-SYORI-EXT
           END-IF
      *
           .
       451-TOUROKU-SYORI-EXT.
           EXIT.
      *
      **************************************************************
      *    請求取消更新処理
      **************************************************************
       4701-TORIKESI-KOUSHIN-SEC       SECTION.
      *
      *    患者定期請求履歴を削除する
           PERFORM 4701-PTTEIKIRRK-DEL-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4701-TORIKESI-KOUSHIN-EXT
           END-IF
      *
      *    定期請求履歴を更新する
           PERFORM 4701-TEIKIRRK-UPD-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4701-TORIKESI-KOUSHIN-EXT
           END-IF
      *
      *    収納データを更新する
           PERFORM 4701-SYUNOU-UPD-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4701-TORIKESI-KOUSHIN-EXT
           END-IF
      *
           .
       4701-TORIKESI-KOUSHIN-EXT.
           EXIT.
      *****************************************************************
      *    収納データ更新処理
      *****************************************************************
       4701-SYUNOU-UPD-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
           INITIALIZE  SYUNOU-REC
      *
           MOVE    SPA-HOSPID          TO  SYU-HOSPID
           MOVE    SPA-NAI-I04-PTID    TO  SYU-PTID
           MOVE    "1"                 TO  SYU-NYUGAIKBN
           MOVE    SPA-I01-I04-SKYSTYMD
                                       TO  SYU-SKYSTYMD
           MOVE    SPA-I01-I04-SKYEDYMD
                                       TO  SYU-SKYEDYMD
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
               IF    ( SYU-NYUIN-RRKNUM    =   SPA-NAI-I04-PTRRKNUM )
                AND  ( SYU-DENPJTIKBN  NOT =   "3" )
      *            収納明作成処理
                   PERFORM 4701-SYUMEI-SAKUSEI-SEC
                   IF    ( SPA-ERRCD   NOT =   SPACE )
                       GO  TO  4701-SYUNOU-UPD-EXT
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
                       MOVE    "4001"      TO  SPA-ERRCD
                       GO  TO  4701-SYUNOU-UPD-EXT
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
       4701-SYUNOU-UPD-EXT.
           EXIT.
      *****************************************************************
      *    収納明細作成処理
      *****************************************************************
       4701-SYUMEI-SAKUSEI-SEC         SECTION.
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
           PERFORM VARYING  IDX6   FROM    1   BY   1
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
           MOVE    IDX6                TO  SMEI-DENPEDANUM
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
               MOVE    "4002"          TO  SPA-ERRCD
               GO  TO  4701-SYUMEI-SAKUSEI-EXT
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
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           PERFORM 900-TBL-INSERT-SEC
           IF    ( FLG-DBRC        NOT =   ZERO )
               MOVE    "4003"          TO  SPA-ERRCD
               GO  TO  4701-SYUMEI-SAKUSEI-EXT
           END-IF
      *
           .
       4701-SYUMEI-SAKUSEI-EXT.
           EXIT.
      *****************************************************************
      *    患者定期請求履歴削除処理
      *****************************************************************
       4701-PTTEIKIRRK-DEL-SEC         SECTION.
      *
           INITIALIZE  SUMTEIKIRRK-REC
           MOVE    ZERO                TO  FLG-PTTEIKIRRK
      *
           INITIALIZE  PTTEIKIRRK-REC
           MOVE    SPA-HOSPID          TO  PTTEIKIRRK-HOSPID
           MOVE    SPA-NAI-I04-PTID    TO  PTTEIKIRRK-PTID
           MOVE    SPA-I01-I04-SKYSTYMD
                                       TO  PTTEIKIRRK-SKYSTYMD
           MOVE    SPA-I01-I04-SKYEDYMD
                                       TO  PTTEIKIRRK-SKYEDYMD
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "PTTEIKIRRK-KEY4"   TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        NOT =   ZERO )
               MOVE    1               TO  FLG-PTTEIKIRRK
           END-IF
      *
           PERFORM UNTIL ( FLG-PTTEIKIRRK  NOT =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  PTTEIKIRRK-REC
      *
               IF    ( PTTEIKIRRK-RRKNUM   =   SPA-NAI-I04-PTRRKNUM )
      *            定期履歴管理情報集計
                   PERFORM 4701-TEIKIRRK-SUM-SEC
               END-IF
      *
               MOVE    "PTTEIKIRRK-KEY4"   TO  WRK-DBPATH
               PERFORM 900-TBL-READ-SEC
               IF    ( FLG-DBRC        NOT =   ZERO )
                   MOVE    1           TO  FLG-PTTEIKIRRK
               END-IF
      *
           END-PERFORM
      *
           MOVE    "PTTEIKIRRK-KEY4"   TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
      *    患者請求履歴削除
           INITIALIZE  PTTEIKIRRK-REC
           MOVE    SPA-HOSPID          TO  PTTEIKIRRK-HOSPID
           MOVE    SPA-NAI-I04-PTID    TO  PTTEIKIRRK-PTID
           MOVE    SPA-I01-I04-SKYSTYMD
                                       TO  PTTEIKIRRK-SKYSTYMD
           MOVE    SPA-I01-I04-SKYEDYMD
                                       TO  PTTEIKIRRK-SKYEDYMD
           MOVE    SPA-I01-I04-RRKNUM
                                       TO  PTTEIKIRRK-RRKNUM
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "PTTEIKIRRK-DEL3"   TO  WRK-DBPATH
           PERFORM 900-TBL-DELETE-SEC
           IF    ( FLG-DBRC        NOT =   ZERO )
               MOVE    "4004"          TO  SPA-ERRCD
               GO  TO  4701-PTTEIKIRRK-DEL-EXT
           END-IF
      *
           .
       4701-PTTEIKIRRK-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    定期請求管理情報集計（マイナス分）
      *****************************************************************
       4701-TEIKIRRK-SUM-SEC           SECTION.
      *
           INITIALIZE  SYUNOU-REC
      *
           MOVE    SPA-HOSPID          TO  SYU-HOSPID
           MOVE    SPA-NAI-I04-PTID    TO  SYU-PTID
           MOVE    "1"                 TO  SYU-NYUGAIKBN
           MOVE    PTTEIKIRRK-DENPNUM  TO  SYU-DENPNUM
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "SYUNOU-KEY"        TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC            =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
           END-IF
      *
           MOVE    "SYUNOU-KEY"        TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
      *    定期請求管理情報集計（マイナス分）
           PERFORM VARYING IDX6   FROM    1   BY  1
                   UNTIL ( IDX6   >   100 )
               EVALUATE SUMTEIKIRRK-SRYYM (IDX6)
                                   ALSO    SUMTEIKIRRK-SAKKBN (IDX6)
               WHEN    SPACE       ALSO    ANY
                   MOVE    PTTEIKIRRK-SRYYM
                                   TO  SUMTEIKIRRK-SRYYM   (IDX6)
                   MOVE    PTTEIKIRRK-SAKKBN
                                   TO  SUMTEIKIRRK-SAKKBN  (IDX6)
                   MOVE    PTTEIKIRRK-SKYSTYMD
                                   TO  SUMTEIKIRRK-SKYSTYMD(IDX6)
                   MOVE    PTTEIKIRRK-SKYEDYMD
                                   TO  SUMTEIKIRRK-SKYEDYMD(IDX6)
                   COMPUTE SUMTEIKIRRK-SKYTEN (IDX6)
                       =   SYU-TOTAL-HKNTEN    *   -1
                   COMPUTE SUMTEIKIRRK-SKYMONEY (IDX6)
                       =   SYU-SKYMONEY        *   -1
                   COMPUTE SUMTEIKIRRK-MAX
                       =   SUMTEIKIRRK-MAX     +   1
                   MOVE    100         TO  IDX6
               WHEN    PTTEIKIRRK-SRYYM
                                   ALSO    PTTEIKIRRK-SAKKBN
                   COMPUTE SUMTEIKIRRK-SKYTEN (IDX6)
                       =   SUMTEIKIRRK-SKYTEN (IDX6)
                       + ( SYU-TOTAL-HKNTEN    *  -1 )
                   COMPUTE SUMTEIKIRRK-SKYMONEY (IDX6)
                       =   SUMTEIKIRRK-SKYMONEY (IDX6)
                       + ( SYU-SKYMONEY        *  -1 )
                   MOVE    100         TO  IDX6
               END-EVALUATE
           END-PERFORM
      *
           .
       4701-TEIKIRRK-SUM-EXT.
           EXIT. 
      *****************************************************************
      *    定期請求履歴更新処理
      *****************************************************************
       4701-TEIKIRRK-UPD-SEC           SECTION.
      *
           PERFORM VARYING IDX6    FROM    1   BY  1
                   UNTIL ( IDX6    >   SUMTEIKIRRK-MAX )
      *
               INITIALIZE  TEIKIRRK-REC
      *
               MOVE    SUMTEIKIRRK-OCC (IDX6)
                                       TO  TEIKIRRK-REC
               MOVE    SPA-TERMID      TO  TEIKIRRK-TERMID
      *
               MOVE    TEIKIRRK-REC    TO  MCPDATA-REC
               MOVE    "TEIKIRRK-KEY"  TO  WRK-DBPATH
               PERFORM 900-TBL-SELECT-SEC
      *        更新対象のデータがすでに存在する場合は更新
      *        存在しない場合は追加
               IF    ( FLG-DBRC        =   ZERO )
                   MOVE    MCPDATA-REC TO  TEIKIRRK-REC
      *
                   COMPUTE TEIKIRRK-SKYTEN
                       =   TEIKIRRK-SKYTEN
                       +   SUMTEIKIRRK-SKYTEN   (IDX6)
      *
                   COMPUTE TEIKIRRK-SKYMONEY
                       =   TEIKIRRK-SKYMONEY
                       +   SUMTEIKIRRK-SKYMONEY (IDX6)
      *
                   PERFORM 800-MCNDATE-SEC
                   MOVE    SMCNDATE-YMD    TO  TEIKIRRK-UPYMD
                   MOVE    SMCNDATE-HMS    TO  TEIKIRRK-UPHMS
      *
      *            定期請求管理データ更新
                   MOVE    TEIKIRRK-REC    TO  MCPDATA-REC
                   MOVE    "TEIKIRRK-KEY"  TO  WRK-DBPATH
                   PERFORM 900-TBL-UPDATE-SEC
                   IF  (   FLG-DBRC    NOT =   ZERO )
                       MOVE    "4005"      TO  SPA-ERRCD
                       GO  TO  4701-TEIKIRRK-UPD-EXT
                   END-IF
               ELSE
      *
                   PERFORM 800-MCNDATE-SEC
                   MOVE    SMCNDATE-YMD    TO  TEIKIRRK-CREYMD
                                               TEIKIRRK-UPYMD
                   MOVE    SMCNDATE-HMS    TO  TEIKIRRK-UPHMS
      *
                   MOVE    TEIKIRRK-REC    TO  MCPDATA-REC
                   MOVE    "TEIKIRRK-KEY"  TO  WRK-DBPATH
                   PERFORM 900-TBL-INSERT-SEC
                   IF  (   FLG-DBRC    NOT =   ZERO )
                       MOVE    "4006"      TO  SPA-ERRCD
                       GO  TO  4701-TEIKIRRK-UPD-EXT
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       4701-TEIKIRRK-UPD-EXT.
           EXIT. 
      *****************************************************************
      *    登録処理（退院再計算）
      *****************************************************************
       452-TOUROKU-SEC             SECTION.
      *
      *    ＤＢ更新処理
           PERFORM 452-TOUROKU-SYORI-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  452-TOUROKU-EXT
           END-IF
      *
      *    帳票出力処理
           PERFORM 451-PRTOUT-SEC
      *
           MOVE    "B12"           TO  SPA-I01-I04-BTN
           MOVE    "JOIN"          TO  MCP-PUTTYPE
           PERFORM 210-BACK
      *
           .
      *
       452-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       452-TOUROKU-SYORI-SEC           SECTION.
      *
      *    収納データを更新する（請求取消）
           PERFORM 4701-SYUNOU-UPD-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  452-TOUROKU-SYORI-EXT
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-I0-SYUNOU-CNT )
               IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX1)
                                   =   ZERO )
                   MOVE    IDX1    TO  IDX-SYU
      *            収納マスタ更新処理
                   PERFORM 4801-SYUNOU-UPD-SEC
                   IF    ( SPA-ERRCD       NOT =   SPACE )
                       GO  TO  452-TOUROKU-SYORI-EXT
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       452-TOUROKU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    定期請求処理中チェック
      *****************************************************************
       4801-TEIKI-JOB-CHK-SEC          SECTION.
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    CONST-JOB-JOBID     TO  JOB-JOBID
           MOVE    CONST-JOB-SHELLID   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
           IF    ( SJOBKANRI-RETURN    =   ZERO )
               IF    ( JOB-ENDYMD      =   SPACE )
                   MOVE    "1001"      TO  SPA-ERRCD
               END-IF
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
      *
           .
       4801-KANREN-CHK-EXT.
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
           MOVE    SPA-NAI-I04-PTID    TO  PTNYUINRRK-PTID
           MOVE    SPA-NAI-I04-PTRRKNUM
                                       TO  PTNYUINRRK-RRKNUM
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "PTNYUINRRK-KEY4"   TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
      *
      *    入院歴の退院日を転科歴も含めて更新する。
           PERFORM UNTIL ( FLG-DBRC    NOT =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
      *
               COMPUTE WRK-TAIINUPDCNT
                   =   WRK-TAIINUPDCNT +   1
      *
               IF    ( WRK-TAIINUPDCNT =   1 )
                   IF    ( SPA-NAI-I04-PTRRKNUM
                                       =   PTNYUINRRK-RRKNUM )
                    AND  ( SPA-NAI-I04-PTRRKEDANUM
                                       =   PTNYUINRRK-RRKEDANUM )
      *
                       MOVE    SPA-NAI-I04-TAIINYMD
                                       TO  PTNYUINRRK-TAIINYMD
                                           PTNYUINRRK-TENSTUYMD
      *
                       MOVE    PTNYUINRRK-REC
                                       TO  WKPTNYUINRRK-REC
      *
                   ELSE
                       MOVE    "4007"  TO  SPA-ERRCD
                       GO  TO  4801-TAIINYMD-UPD-EXT
                   END-IF
               ELSE
                   MOVE    SPA-NAI-I04-TAIINYMD
                                       TO  PTNYUINRRK-TAIINYMD
               END-IF
      *
               MOVE    SPA-NAI-I04-TAIINCD
                                       TO  PTNYUINRRK-TAIINCD
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD    TO  PTNYUINRRK-UPYMD
               MOVE    SMCNDATE-HMS    TO  PTNYUINRRK-UPHMS
      *
               MOVE    "PTNYUINRRK-KEY"
                                   TO  WRK-DBPATH
               MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
               PERFORM 900-TBL-UPDATE-SEC
               IF    ( FLG-DBRC    NOT =   ZERO )
                   MOVE    "4008"  TO  SPA-ERRCD
                   GO  TO  4801-TAIINYMD-UPD-EXT
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
               MOVE    "4009"  TO  SPA-ERRCD
               GO  TO  4801-TAIINYMD-UPD-EXT
           END-IF
      *
      *
           .
       4801-TAIINYMD-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    退院　会計テーブル編集処理
      *****************************************************************
       4801-TAIINKAIKEI-EDIT-SEC         SECTION.
      *
      *    入院会計作成サブの呼出し
           INITIALIZE            ORCSNYUACCT-AREA
           MOVE     "2"     TO   LNK-SCNYUACCT-SYORI-KBN
           MOVE     "1"                TO  SPA-NYUGAIKBN
           MOVE     PTNYUINRRK-PTID    TO  SPA-PTID
           MOVE     PTNYUINRRK-NYUINKA TO  SPA-SRYKA
           MOVE     PTNYUINRRK-TAIINYMD
                                       TO   LNK-SCNYUACCT-EDSRYYMD
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
           .
       4801-TAIINKAIKEI-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスタ更新処理
      *****************************************************************
       4801-SYUNOU-UPD-SEC         SECTION.
      *
           MOVE    SPA-I0-SYUNOU-DAT (IDX-SYU)
                                   TO  SYUNOU-REC
      *
           IF    ( SPA-I01-I04-SKYSTYMD (1:6)
                                   NOT =   SPA-I01-I04-SKYEDYMD (1:6) )
               IF    ( SPA-I01-I04-SKYSTYMD (1:6)
                                       =   SYU-SKYSTYMD (1:6))
                   MOVE    SPA-I01-I04-SKYSTYMD    TO  SYU-SKYSTYMD
                   MOVE    SPA-I01-I04-SKYSTYMD    TO  WRK-SYMD
                   PERFORM 5002-GETUMATU-SEC
                   MOVE    WRK-SYMD                TO  SYU-SKYEDYMD
               END-IF
               IF    ( SPA-I01-I04-SKYEDYMD (1:6)
                                       =   SYU-SKYEDYMD (1:6))
                   MOVE    SPA-I01-I04-SKYEDYMD (1:6)
                                                   TO  SYU-SKYSTYMD(1:6)
                   MOVE    "01"                    TO  SYU-SKYSTYMD(7:2)
                   MOVE    SPA-I01-I04-SKYEDYMD    TO  SYU-SKYEDYMD
               END-IF
           ELSE
               MOVE    SPA-I01-I04-SKYSTYMD    TO  SYU-SKYSTYMD
               MOVE    SPA-I01-I04-SKYEDYMD    TO  SYU-SKYEDYMD
           END-IF
      *
      *    伝票番号取得処理
           PERFORM 4801-DENPNUM-SET-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4801-SYUNOU-UPD-EXT
           END-IF
      *
           MOVE    WRK-DENPNUM     TO  SYU-DENPNUM
      *
      *    請求時最終履歴取得処理
           PERFORM 4801-SEILASTRRK-SET-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4801-SYUNOU-UPD-EXT
           END-IF
      *
           MOVE    WRK-NYUINKA     TO  SYU-SRYKA
           MOVE    WRK-BRMNUM      TO  SYU-ROOMNUM
           MOVE    WRK-BTUNUM      TO  SYU-BYOTONUM
           MOVE    WRK-DRCD        TO  SYU-DRCD
      *
           MOVE    SPA-NAI-I04-CHOSEI (IDX-SYU)
                                   TO  SYU-CHOSEI
           MOVE    SPA-NAI-I04-SEIKYU (IDX-SYU)
                                   TO  SYU-SKYMONEY
           MOVE    SPA-NAI-I04-NYUKIN (IDX-SYU)
                                   TO  SYU-NYUKIN-TOTAL
           IF    ( SYU-NYUKIN-TOTAL    >   ZERO )
               MOVE    1           TO  SYU-NYUKIN-KAISU
           END-IF
      *
           IF    ( SYU-SKYMONEY        =   ZERO )
               MOVE    SPACE       TO  SYU-DENPJTIKBN
           ELSE
               IF    ( SYU-SKYMONEY   <=  SYU-NYUKIN-TOTAL )
                   MOVE    "1"     TO  SYU-DENPJTIKBN
               ELSE
                   MOVE    "2"     TO  SYU-DENPJTIKBN
               END-IF
           END-IF
      *
           MOVE    SPA-NAI-I04-PTRRKNUM
                                   TO  SYU-NYUIN-RRKNUM
      *
           MOVE    SYUNOU-REC      TO  WKSYUNOU-REC
      *
      *    更新前チェック（既に重複するデータがある場合はエラー）
           MOVE   ZERO             TO  FLG-SYUNOU
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "SYUNOU-KEY"    TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    NOT =   ZERO )
               MOVE    1           TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "SYUNOU-KEY"    TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF    ( FLG-SYUNOU      =   ZERO )
               MOVE    "4010"      TO  SPA-ERRCD
               GO  TO  4801-SYUNOU-UPD-EXT
           END-IF
      *
           MOVE    WKSYUNOU-REC    TO  SYUNOU-REC
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD    TO  SYU-CREYMD
                                       SYU-UPYMD
           MOVE    SMCNDATE-HMS    TO  SYU-UPHMS
      *
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "SYUNOU-KEY"    TO  WRK-DBPATH
           PERFORM 900-TBL-INSERT-SEC
           IF    ( FLG-DBRC    NOT =   ZERO )
               MOVE    "4011"      TO  SPA-ERRCD
               GO  TO  4801-SYUNOU-UPD-EXT
           END-IF
      *
           MOVE      SYUNOU-REC    TO  SPA-I0-SYUNOU-DAT (IDX-SYU)
      *
      *    日別収納マスタ更新処理
           PERFORM 4801-SYUDAY-UPD-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4801-SYUNOU-UPD-EXT
           END-IF
      *
      *    収納明細マスタ更新処理
           PERFORM 4801-SYUMEI-UPD-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4801-SYUNOU-UPD-EXT
           END-IF
      *
           .
       4801-SYUNOU-UPD-EXT.
           EXIT.
      *****************************************************************
      *    日別収納マスタ更新処理
      *****************************************************************
       4801-SYUDAY-UPD-SEC         SECTION.
      *
           MOVE    SPA-I0-SYUDAY-DAT (IDX-SYU)
                                       TO  SYUDAY-REC
      *
           IF    ( SDAY-DAYINFFLG      =   "1" )
      *
               MOVE    SYU-DENPNUM     TO  SDAY-DENPNUM
      *
      *
               MOVE    SYUDAY-REC      TO  WKSYUDAY-REC
      *
      *        更新前チェック（既に重複するデータがある場合はエラー）
               MOVE   ZERO             TO  FLG-SYUDAY
               MOVE    SYUDAY-REC      TO  MCPDATA-REC
               MOVE    "SYUDAY-KEY"    TO  WRK-DBPATH
               PERFORM 900-TBL-SELECT-SEC
               IF    ( FLG-DBRC    NOT =   ZERO )
                   MOVE    1           TO  FLG-SYUDAY
               END-IF
      *
               MOVE    "SYUDAY-KEY"    TO  WRK-DBPATH
               PERFORM 900-TBL-CLOSE-SEC
      *
               IF    ( FLG-SYUDAY      =   ZERO )
                   MOVE    "4019"      TO  SPA-ERRCD
                   GO  TO  4801-SYUDAY-UPD-EXT
               END-IF
      *
               MOVE    WKSYUDAY-REC    TO  SYUDAY-REC
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD    TO  SDAY-CREYMD
                                           SDAY-UPYMD
               MOVE    SMCNDATE-HMS    TO  SDAY-UPHMS
      *
               MOVE    SYUDAY-REC      TO  MCPDATA-REC
               MOVE    "SYUDAY-KEY"    TO  WRK-DBPATH
               PERFORM 900-TBL-INSERT-SEC
               IF    ( FLG-DBRC    NOT =   ZERO )
                   MOVE    "4020"      TO  SPA-ERRCD
                   GO  TO  4801-SYUDAY-UPD-EXT
               END-IF
           END-IF
      *
           .
       4801-SYUDAY-UPD-EXT.
           EXIT.
      *****************************************************************
      *    収納明細マスタ更新処理
      *****************************************************************
       4801-SYUMEI-UPD-SEC         SECTION.
      *
           MOVE    SPACE               TO  SYUMEI-REC
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPID          TO  SMEI-HOSPID
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
      *
      *    伝票番号
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
      *    伝票番号枝番
           MOVE    01                  TO  SMEI-DENPEDANUM
      *    診療科
           MOVE    SYU-SRYKA           TO  SMEI-SRYKA
      *    伝票状態区分
           MOVE    SPACE               TO  SMEI-MEISAIJOUTAIKBN
      *    請求発行日（発行日の西暦）
           MOVE    SPA-SYSYMD
                                       TO  SMEI-SKYPRINTYMD
      *    請求再発行日
           MOVE    SPACE               TO  SMEI-SKYREPRINTYMD
      *    領収書発行日
           MOVE    SMEI-SKYPRINTYMD    TO  SMEI-RYOSYUPRINTYMD
      *    領収書再発行日
           MOVE    SPACE               TO  SMEI-RYOSYUREPRINTYMD
      *    請求金額
           MOVE    SYU-SKYMONEY        TO  SMEI-SKYMONEY
      *    入返金日
           MOVE    SMEI-SKYPRINTYMD    TO  SMEI-NYUHEN-YMD
      *    入金連番
           MOVE    01                  TO  SMEI-NYUKINRENNUM
      *
      *    入金額がゼロの時、入金力なし
           IF    ( SYU-NYUKIN-TOTAL    =   ZERO )
               MOVE    SPA-GMN-I04-NYKNHOHO-CD
                                           TO  SMEI-NYUKIN-HOHO
           ELSE
      *        入返金区分
               MOVE    "1"                 TO  SMEI-NYUHEN-KBN
      *        入返金額
               MOVE    SYU-NYUKIN-TOTAL    TO  SMEI-NYUHEN-MONEY
      *        入金方法
               MOVE    SPA-GMN-I04-NYKNHOHO-CD
                                           TO  SMEI-NYUKIN-HOHO
           END-IF
      *    状態区分
           IF    ( SYU-SKYMONEY        =   ZERO )
               MOVE    SPACE               TO  SMEI-JOUTAIKBN
           ELSE
               IF   (  SYU-SKYMONEY    >   SYU-NYUKIN-TOTAL )
                   MOVE    "9"             TO  SMEI-JOUTAIKBN
               ELSE
                   MOVE    "1"             TO  SMEI-JOUTAIKBN
               END-IF
           END-IF
      *
           MOVE    SYUMEI-REC      TO  WKSYUMEI-REC
      *
      *    更新前チェック（既に重複するデータがある場合はエラー）
           MOVE   ZERO             TO  FLG-SYUMEI
           MOVE    SYUMEI-REC      TO  MCPDATA-REC
           MOVE    "SYUMEI-KEY"    TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    NOT =   ZERO )
               MOVE    1           TO  FLG-SYUMEI
           END-IF
      *
           MOVE    "SYUMEI-KEY"    TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF    ( FLG-SYUMEI      =   ZERO )
               MOVE    "4012"      TO  SPA-ERRCD
               GO  TO  4801-SYUMEI-UPD-EXT
           END-IF
      *
           MOVE    WKSYUMEI-REC    TO  SYUMEI-REC
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
           MOVE    SYUMEI-REC      TO  MCPDATA-REC
           MOVE    "SYUMEI-KEY"    TO  WRK-DBPATH
           PERFORM 900-TBL-INSERT-SEC
           IF    ( FLG-DBRC    NOT =   ZERO )
               MOVE    "4013"      TO  SPA-ERRCD
               GO  TO  4801-SYUMEI-UPD-EXT
           END-IF
      *
      *
           .
       4801-SYUMEI-UPD-EXT.
           EXIT.
      *****************************************************************
      *    伝票番号取得処理
      *****************************************************************
       4801-DENPNUM-SET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
                                           WRK-DENPNUM
      *
           INITIALIZE                  SYS-0042-REC.
           MOVE    "0042"              TO  SYS-0042-KANRICD
           MOVE    "*"                 TO  SYS-0042-KBNCD
           MOVE    ZERO                TO  SYS-0042-STYUKYMD
           MOVE    ALL "9"             TO  SYS-0042-EDYUKYMD
      *
           MOVE    SPA-SYSYMD
                                       TO  ORC-DBYMD
           MOVE    SYS-0042-REC        TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-0042-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYS-0042-DENPNUMMAX-NYU
                                       TO  WRK-DENPNUM-X
               IF    ( WRK-DENPNUM-X   =   SPACE )
                   MOVE    ZERO        TO  SYS-0042-DENPNUMMAX-NYU
               ELSE
                   MOVE    WRK-DENPNUM-X
                                       TO  SYS-0042-DENPNUMMAX-NYU
               END-IF
      *
               COMPUTE SYS-0042-DENPNUMMAX-NYU
                   =   SYS-0042-DENPNUMMAX-NYU +   1
      *
               MOVE    SYS-0042-DENPNUMMAX-NYU
                                           TO  WRK-DENPNUM
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD        TO  SYS-0042-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-0042-UPHMS
      *
               MOVE    SYS-0042-REC        TO  MCPDATA-REC
               MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
               PERFORM 900-TBL-UPDATE-SEC
               IF    ( FLG-DBRC    NOT =   ZERO )
                   MOVE    "4014"              TO  SPA-ERRCD
                   GO  TO  4801-DENPNUM-SET-EXT
               END-IF
           ELSE
               MOVE    "4015"              TO  SPA-ERRCD
               GO  TO  4801-DENPNUM-SET-EXT
           END-IF
      *
           .
       4801-DENPNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求時最終履歴取得処理
      *****************************************************************
       4801-SEILASTRRK-SET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINKA
           MOVE    SPACE               TO  WRK-NYUINKA
                                           WRK-BTUNUM
                                           WRK-BRMNUM
                                           WRK-DRCD
      *
      *    入院履歴マスタ検索
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPID          TO  PTNYUINRRK-HOSPID
           MOVE    SPA-NAI-I04-PTID    TO  PTNYUINRRK-PTID
           MOVE    SPA-NAI-I04-PTRRKNUM
                                       TO  PTNYUINRRK-RRKNUM
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "PTNYUINRRK-KEY4"   TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
      *
           PERFORM UNTIL ( FLG-DBRC    NOT =   ZERO )
                    OR   ( FLG-NYUINKA =   1 )
      *
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
      *
               IF    ( PTNYUINRRK-TENNYUYMD
                                       <=  SYU-SKYEDYMD )
                AND  ( PTNYUINRRK-TENSTUYMD
                                       >=  SYU-SKYEDYMD )
                   MOVE    1           TO  FLG-NYUINKA
                   MOVE    PTNYUINRRK-NYUINKA
                                       TO  WRK-NYUINKA
                   MOVE    PTNYUINRRK-BTUNUM
                                       TO  WRK-BTUNUM
                   MOVE    PTNYUINRRK-BRMNUM
                                       TO  WRK-BRMNUM
                   MOVE    PTNYUINRRK-DRCD1 (2 : 4)
                                       TO  WRK-DRCD
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
           IF    ( FLG-NYUINKA         =   ZERO )
               MOVE    "4016"  TO  SPA-ERRCD
               GO  TO  4801-SEILASTRRK-SET-EXT
           END-IF
      *
      *
           .
       4801-SEILASTRRK-SET-EXT.
           EXIT.
      *****************************************************************
      *    患者情報更新処理
      *****************************************************************
       4801-PTINF-UPD-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-ZI-SAISIN
      *    入院歴より最新の歴情報を取得
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPID          TO  PTNYUINRRK-HOSPID
           MOVE    SPA-NAI-I04-PTID    TO  PTNYUINRRK-PTID
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
                   MOVE    PTNYUINRRK-RRKNUM
                                       TO  WRK-ZI-SAISIN-RRKNUM
                   MOVE    PTNYUINRRK-RRKEDANUM
                                       TO  WRK-ZI-SAISIN-RRKEDANUM
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
           PERFORM 900-PTINF-KEY-SEL-SEC
           IF    ( FLG-PTINF           NOT =   ZERO )
               MOVE    "4017"          TO  SPA-ERRCD
               GO  TO  4801-PTINF-UPD-EXT
           END-IF
      *
      *    患者情報の入院歴番号を更新する
           IF    ( FLG-ZI-SAISIN       =   1 )
               MOVE   WRK-ZI-SAISIN-RRKNUM
                                       TO  PTINF-RRKNUM
               MOVE   WRK-ZI-SAISIN-RRKEDANUM
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
               MOVE    "4018"          TO  SPA-ERRCD
               GO  TO  4801-PTINF-UPD-EXT
           END-IF
      *
           .
       4801-PTINF-UPD-EXT.
           EXIT.
      *****************************************************************
      *    帳票出力処理
      *****************************************************************
       451-PRTOUT-SEC              SECTION.
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           MOVE    SPA-AREA        TO  WRK-SPA-AREA
      *
           MOVE    "1"             TO  WRK-SPA-NYUGAIKBN
      *
           MOVE    PTINF-PTID      TO  WRK-SPA-PTID
           MOVE    SPA-GMN-I04-PTNUM
                                   TO  WRK-SPA-PTNUM
           MOVE    PTINF-KANANAME
                                   TO  WRK-SPA-KANANAME
           MOVE    PTINF-NAME      TO  WRK-SPA-NAME
           MOVE    PTINF-SEX       TO  WRK-SPA-SEX
           MOVE    PTINF-BIRTHDAY
                                   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  WRK-SPA-BIRTHDAYW
           MOVE    WRK-SYMD        TO  WRK-SPA-BIRTHDAY
      *
      *    請求書兼領収書
           IF    ( SPA-GMN-I04-SKYPRTOUT-CD
                                       =   "1" OR "2" )
               PERFORM 451-SKYPRT-OUT-SEC
           END-IF
      *
      *    退院証明書
           IF    ( SPA-GMN-I04-TAIPRTOUT-CD
                                       =   "1" )
               PERFORM 451-TAIPRT-OUT-SEC
           END-IF
      *
           .
       451-PRTOUT-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書出力処理
      *****************************************************************
       451-SKYPRT-OUT-SEC              SECTION.
      *
           MOVE    "ORCHCN03"      TO  WRK-ORG-PRINT-PG
           MOVE    "00000003"      TO  WRK-KBNCD
           PERFORM 4511-PRINT-PG-EDIT-SEC
      *
           INITIALIZE      ORCHCN03AREA
           MOVE    WRK-SPA-HOSPID  TO  ORCHCN03-HEAD-HOSPID
           MOVE    WRK-SPA-STAFFCD TO  ORCHCN03-STAFFCD
           MOVE    WRK-SPA-SYSYMD  TO  ORCHCN03-SYSYMD
           MOVE    WRK-SPA-TERMID  TO  ORCHCN03-TERMID
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1        >   SPA-I0-SYUNOU-CNT )
      *
               MOVE    SPA-I0-SYUNOU-DAT (IDX1)
                                       TO  WKSYUNOU-REC
      *
               MOVE  1                 TO  FLG-SKYPRT
      *
               IF    ( SPA-I0-SYUNOU-TTLDAT-FLG (IDX1)
                                   NOT =   ZERO )
                       MOVE    ZERO    TO  FLG-SKYPRT
               ELSE
                   IF    ( SPA-GMN-I04-SKYPRTOUT-CD    =   "2" )
                     AND ( WKSYU-SKYMONEY             <=   ZERO )
                       MOVE    ZERO    TO  FLG-SKYPRT
                   END-IF
               END-IF
      *
               IF    ( FLG-SKYPRT      =   1 )
      *
                   COMPUTE ORCHCN03-CNT
                                       =   ORCHCN03-CNT +   1
                   MOVE    ORCHCN03-CNT
                                       TO  IDX2
      *
                   MOVE    WRK-SPA-HOSPID
                                       TO ORCHCN03-HOSPID (IDX2)
                   MOVE    WRK-SPA-PTID
                                       TO ORCHCN03-PTID   (IDX2)
                   MOVE    WRK-SPA-PTNUM
                                       TO ORCHCN03-PTNUM  (IDX2)
                   MOVE    "1"         TO ORCHCN03-NYUGAIKBN (IDX2)
      *
                   MOVE    WKSYU-SKYSTYMD
                                       TO  ORCHCN03-KJNYMD  (IDX2)
                   MOVE    WKSYU-DENPNUM
                                       TO  ORCHCN03-DENPNUM (IDX2)
                   MOVE    WKSYU-SKYMONEY-TAX-SAI
                                       TO  ORCHCN03-SEIKYU-TAX-SAI
                                                            (IDX2)
                   MOVE    WKSYU-SKYMONEY
                                       TO  ORCHCN03-SEIKYU  (IDX2)
                   MOVE    WKSYU-NYUKIN-TOTAL
                                       TO  ORCHCN03-NYUKIN  (IDX2)
                   MOVE    ZERO        TO  ORCHCN03-HAKKOFLG (IDX2)
      *
               END-IF
           END-PERFORM
      *
           IF    ( ORCHCN03-CNT        >   ZERO )
               CALL    WRK-PRINT-PG    USING   ORCHCN03AREA
           END-IF
      *
           .
       451-SKYPRT-OUT-EXT.
           EXIT.
      *****************************************************************
      *    退院証明書出力処理
      *****************************************************************
       451-TAIPRT-OUT-SEC          SECTION.
      *
           MOVE    "ORCHCN02"      TO  WRK-ORG-PRINT-PG
           MOVE    "00000002"      TO  WRK-KBNCD
           PERFORM 4511-PRINT-PG-EDIT-SEC
      *
           INITIALIZE              LNK-PTNYUINRRK
      *
           IF    ( SPA-GMN-I04-DR-CD   NOT =   SPACE )
               MOVE    "1"         TO  WKPTNYUINRRK-DRCD1
               MOVE    SPA-GMN-I04-DR-CD
                                   TO  WKPTNYUINRRK-DRCD1 (2 : )
               MOVE    WKPTNYUINRRK-REC
                                   TO  LNK-PTNYUINRRK
           END-IF
      *
           CALL    WRK-PRINT-PG    USING   WRK-SPA-AREA
                                           LNK-PTNYUINRRK
      *
           .
       451-TAIPRT-OUT-EXT.
           EXIT.
      *****************************************************************
      *   帳票プログラム名編集処理
      *****************************************************************
       4511-PRINT-PG-EDIT-SEC         SECTION.
      *
           INITIALIZE                      WRK-PRINT-PG
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    WRK-KBNCD           TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "1"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           WRK-SPA-AREA
                                           SYS-1035-REC
           IF    ( ORCSPRTNM-RC        =   ZERO )
              MOVE    ORCSPRTNM-PRTPG     TO  WRK-PRINT-PG
           ELSE
              MOVE    WRK-ORG-PRINT-PG    TO  WRK-PRINT-PG
           END-IF
      *
           .
       4511-PRINT-PG-EDIT-EXT.
           EXIT.
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
           IF      SPA-I0IDCD      NOT =   SPACE
               PERFORM 520-I0IDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "I04"               TO  MCP-WINDOW.
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
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "入力エラー"    TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "主治医氏名を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE
                   "今回請求額がゼロ以下になります。"
                                       TO  SPA-ERRMSG(1:)
               MOVE
                   "調整金を再入力して下さい"
                                       TO  SPA-ERRMSG(33:)
           WHEN    "0007"
               MOVE    "入金額は整数で入力して下さい。"
                                       TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "入金額は今回請求額以下にして下さい。"
                                       TO  SPA-ERRMSG
           WHEN    "0009"
               MOVE    "入金方法を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0010"
               MOVE    "入金方法入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "定期請求処理中のため登録はできません"
                                       TO  SPA-ERRMSG
           WHEN    "4001"  THRU    "4999"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  I0ERR
           MOVE    SPA-ERRCD           TO  I0ERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  I0ERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "I04"               TO  SPA-MOTOPG
           MOVE    "I0ERR"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "I0ERR"             TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-I0IDSET-SEC                 SECTION.
      *
           EVALUATE    SPA-I0IDCD
           WHEN    "3001"
               MOVE    "元の請求データは取り消されます。よろしいですか"
                                       TO  WRK-I0IDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  I0ID1
           MOVE    SPA-I0IDCD          TO  I0ID1-ID1CODE
           MOVE    WRK-I0IDMSG         TO  I0ID1-ID1MSG
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "I04"               TO  SPA-MOTOPG
           MOVE    "I0ID1"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "I0ID1"             TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-I0IDSET-EXT.
           EXIT.
      *
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
      *    月末日取得処理
      *****************************************************************
       5002-GETUMATU-SEC      SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD (1 : 6)    TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD
      *
           .
       5002-GETUMATU-EXT.
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
      *    患者情報検索処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
           INITIALIZE  PTINF-REC
           MOVE    SPA-HOSPID          TO  PTINF-HOSPID
           MOVE    SPA-NAI-I04-PTID    TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *
           MOVE    "PTINF-KEY"         TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF     ( FLG-DBRC           =  ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "PTINF-KEY"         TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
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
      *
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
      *
