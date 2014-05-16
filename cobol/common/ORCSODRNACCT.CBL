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
       PROGRAM-ID.                 ORCSODRNACCT.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : 電子カルテ入院会計作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  02.07.01    NACL-小豆沢  05.12.06  入院登録時の食事と食堂加算の
      *                                     カレンダークリア対応と、朝食優先
      *                                     の条件追加
      *  02.07.02    NACL-小豆沢  05.12.12  転科、転棟、転室対応
      *  02.07.03    NACL-小豆沢  05.12.13  入院取消対応
      *  03.05.00    NACL-太田    07/05/28  グループ診療対応
      *  04.03.00    NACL-小豆沢  08/08/11  特定入院料コード取得をサブプロに変更
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
      *    入退院登録共通領域
           COPY    "I0COMMON-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-ORCSDAY                 PIC 9(01).
           03  FLG-NYUKSN                  PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-SRYACCT                 PIC 9(01).
           03  FLG-RJN                     PIC 9(01).
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-PTCONF                  PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                         PIC 9(05).
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
           03  START-IDX                   PIC 9(05).
           03  IDX-SKJ-TAIHI               PIC 9(02).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD     PIC X(08).
           03  SYS-TIME    PIC X(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ERRCD                   PIC X(04).
           03  WRK-NAI-I01-IDOYMD          PIC X(08).
           03  WRK-NAI-I01-IDOYMD-MAE      PIC X(08).
           03  WRK-YMD                     PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY                 PIC 9(04).
               05  WRK-SMM                 PIC 9(02).
               05  WRK-SDD                 PIC 9(02).
           03  WRK-HENYMDG                 PIC X(09).
      *
           03  WRK-AGE-YY                  PIC S9(04).
           03  WRK-AGE-MM                  PIC S9(02).
           03  WRK-AGE-G.
               05  WRK-AGE-Z               PIC ---9.
               05  WRK-AGE-MEI             PIC X(04).
           03  WRK-BIRTHDAY                PIC X(08).
           03  WRK-SAGAKU.
               05  WRK-SAGAKU-NUM          PIC ZZZZ9.
               05  WRK-SAGAKU-EN           PIC X(02).
           03  WRK-HKNCOMBI                PIC X(100).
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
           03  WRK-STYMD                       PIC X(08).
           03  WRK-EDYMD                       PIC X(08).
           03  WRK-LASTSRYYM                   PIC X(06).
           03  WRK-NISSU                       PIC 9(05).
           03  WRK-NISSU1                      PIC 9(05).
           03  WRK-NISSU2                      PIC 9(05).
           03  WRK-TSUSAN                      PIC 9(05).
           03  WRK-STUSAN-NISSU03              PIC 9(05).
           03  WRK-NYUINNISSU                  PIC 9(05).
           03  WRK-SANTEISTYMD                 PIC X(08).
           03  WRK-SENTEI-STYMD                PIC X(08).
           03  WRK-BIRTHDAY-15                 PIC X(08).
           03  WRK-BIRTHDAY-15R   REDEFINES    WRK-BIRTHDAY-15.
               05  WRK-BIRTHDAY-15Y            PIC 9(04).
               05  WRK-BIRTHDAY-15MD           PIC 9(04).
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-KEISANYMD               PIC X(08).
           03  WRK-HKNCOMBINUM             PIC X(04).
           03  WRK-HKNCOMBINUM9   REDEFINES  WRK-HKNCOMBINUM
                                           PIC 9(04).
           03  WRK-SAGAKU-KBN              PIC X(02).
           03  WRK-SAGAKU-KBN9  REDEFINES  WRK-SAGAKU-KBN
                                           PIC 9(02).
           03  WRK-TAIHI-SRYKA             PIC X(02).
      *
      *    医療機関設定の入院料加算チェック用
           03  WRK-NYUKHNKSN-SRYCD         PIC X(09)  OCCURS 20.
           03  WRK-NYUKSNKBN               PIC X(03).
           03  WRK-BTU-NYUKSNKBN           PIC X(03).
           03  WRK-BTU-CDKBN-KBNNUM        PIC 9(03).
           03  WRK-BTU-CDKBN-KBNNUM-EDA    PIC 9(02).
           03  WRK-KSN-NYUKSNKBN           PIC X(03).
           03  WRK-KSN-CDKBN-KBNNUM        PIC 9(03).
           03  WRK-KSN-CDKBN-KBNNUM-EDA    PIC 9(02).
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
           03  WRK-TOKUTEI-SRYCD3          PIC X(09).
           03  WRK-TOKUTEI-SRYCD4          PIC X(09).
           03  WRK-SRYCD                   PIC X(09).
           03  WRK-SHOKAI-NYUINYMD-W       PIC X(10).
           03  WRK-CHK-SHONUM              PIC 9(03).
           03  WRK-I01-I04-PROCKBN         PIC X(01).
           03  WRK-FTN-NYUINYMD            PIC X(08).
           03  WRK-FTN-TAIINYMD            PIC X(08).
           03  WRK-FTN-RRKNUM              PIC 9(03).
           03  WRK-NYUREKILST-SEL          PIC 9(05).
      *
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
                   07  WRK-TENROOM-GAIHAKU PIC 9(03)  OCCURS  31.
           03  WRK-NAI-NYUINYMD.
               05  WRK-NAI-NYUINYM         PIC X(06).
               05  WRK-NAI-NYUINDD         PIC 9(02).
      *
           03  WRK-MAEIDOYMD               PIC X(08).
      *
      *    出生児体重
           03  WRK-WEIGHT                  PIC 9(04).
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
      *
      *    食堂加算設定用の退避領域
       01   WRK-SHOKUDOU                   PIC 9(02).
      *
      *    診療所老人医療管理料（転科、転棟）計算エリア
       01  TN-IDX                          PIC 9(02).
       01  TN-IDX2                         PIC 9(02).
       01  WK-TOKUTEI-DAY                  PIC 9(04).
       01  WK-TOKUTEI-DAY2                 PIC 9(04).
      *
      *    外泊食事オーダーの日付退避
       01  WRK-STARTYMD.
           03  WRK-STARTYMD-YM.
               05  WRK-STARTYMD-Y          PIC 9(04).
               05  WRK-STARTYMD-M          PIC 9(02).
           03  WRK-STARTYMD-D              PIC 9(02).
       01  WRK-GETSUMATSU-YMD.
           03  WRK-GETSUMATSU-Y            PIC 9(04).
           03  WRK-GETSUMATSU-M            PIC 9(02).
           03  WRK-GETSUMATSU-D            PIC 9(02).
      *
       01  CONST-AREA.
           03  CONST-H260401               PIC X(08)   VALUE "20140401".
           03  CONST-H241001               PIC X(08)   VALUE "20121001".
           03  CONST-H240401               PIC X(08)   VALUE "20120401".
           03  CONST-H220401               PIC X(08)   VALUE "20100401".
           03  CONST-H211001               PIC X(08)   VALUE "20091001".
           03  CONST-H181001               PIC X(08)   VALUE "20061001".
           03  CONST-H180401               PIC X(08)   VALUE "20060401".
           03  CONST-H180701               PIC X(08)   VALUE "20060701".
           03  CONST-H200401               PIC X(08)   VALUE "20080401".
      *
       01  WRK-ROW.
           03  WRK-NYUREKILST-ROW          PIC S9(9)   BINARY.
      *
      *    スパ領域 ワーク
           COPY    "COMMON-SPA"      REPLACING
                               //SPA-// BY //WRK-SPA-//.
      *
      *    入院履歴ワーク
       01  NRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING
                               //PTNYUINRRK-// BY //NRRK-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    （医療機関基本情報）
           COPY    "CPSK1001.INC".
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
      *    患者他院歴付加情報
       01  PTTAINFUKA-REC.
           COPY    "CPPTTAINFUKA.INC".
      *
      *    老人一般コード変換
        01  SRYCDCHG-REC.
            COPY    "CPSRYCDCHG.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    入院料加算チェック
       01  NYUKSNCHK-REC.
           COPY    "CPNYUKSNCHK.INC".
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
      *
      *    患者個別設定
       01  PTCONF-REC.
           COPY    "CPPTCONF.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
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
      *    初期加算算定サブ
           COPY    "CPORCSSYOKIKSN.INC".
      *
      *    入院会計作成サブ
           COPY    "CPORCSNYUACCT.INC".
      *
      *    通算日数取得サブ
           COPY "CPORCSTUSAN.INC".
      *
      *    保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    保険組合せ編集サブ
           COPY    "CPORCSHKNSET.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    入院診療行為照会テーブル
           COPY  "CPORCSS010.INC".
      *
      *    特定入院料コード取得サブ
           COPY  "CPORCSTOKNYUIN.INC".
      *
      *    医療観察入院料コード取得パラメタ
           COPY  "CPORCSIRYOKNS.INC".
      *
      *    特定入院料算定期間取得パラメタ
           COPY  "CPORCSTOKNYUIN2.INC".
      *
      *    亜急性期入院医療管理料判定パラメタ
           COPY  "CPORCSAKYUSEI.INC".
      *
      *    入院料加算取得パラメタ
           COPY  "CPORCSNYUKSNGET.INC".
      *
      *    夜勤時間超過減算入院料コード取得サブ
           COPY "CPORCSYAKINGEN.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
      *
        LINKAGE                     SECTION.
      *
      *    入院オーダー会計作成
           COPY    "CPORCSODRNACCT.INC".
      *    入院履歴
       01  LNK-PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC"   REPLACING //PTNYUINRRK-//
                                        BY        //LNK-PTNYUINRRK-//.
      *
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
                                               SODRNACCT-AREA
                                               LNK-PTNYUINRRK-REC
                                               SPA-AREA.
      *****************************************************************
      *    主    処    理
      *****************************************************************
      *
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 1001-SPA-SET-SEC
      *
           DISPLAY "SODRNACCT-ORDERID = " SODRNACCT-ORDERID
           MOVE    SPACE           TO  WRK-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    SODRNACCT-ORDERID
      *       （入院）
               WHEN    "0101"
                   PERFORM   4801-NYUKAIKEI-EDIT-SEC
      *       （退院）
               WHEN    "0102"
                   PERFORM   4801-TAIINKAIKEI-EDIT-SEC
      *       （転科／転棟／転室）
               WHEN    "0103"
               WHEN    "0104"
               WHEN    "0105"
                   PERFORM   4801-TENROOM-EDIT-SEC
                   IF    IPN-NYUKAIKEI-SAKUSEI-FLG   =  "1"
                       PERFORM   4801-TENROOM-EDIT-SEC
                   END-IF
      *       （入院取消）
               WHEN    "0107"
      *       （入院取消−会計含む）
               WHEN    "0108"
                   PERFORM   4801-NYUKAIKEI-ERASE-SEC
      *       （退院取消し）
               WHEN    "0115"
                   PERFORM   4801-TAIINKAIKEI-CANCEL-SEC
      *       （異動取消し）
               WHEN    "0116"
                   PERFORM   4801-IDOKAIKEI-CANCEL-SEC
      *       （外泊）
               WHEN    "0201"
               WHEN    "0202"
               WHEN    "0203"
               WHEN    "0204"
               WHEN    "0205"
                   PERFORM   4801-GAIHAKU-SEC
      *       （食事）
               WHEN    "0301"
               WHEN    "0302"
               WHEN    "0303"
               WHEN    "0304"
               WHEN    "0305"
                   IF   SODRNACCT-STARTYMD(1:6)  >= "200604"
                       PERFORM   4801-SHOKUJI-200604-SEC
                   ELSE
                       PERFORM   4801-SHOKUJI-SEC
                   END-IF
           END-EVALUATE
      *
           PERFORM 4900-ORCSS010-SEC
      *
           MOVE      ZERO    TO   SODRNACCT-RC
           .
      *
       000-PROC-EXT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           PERFORM 900-NYUINACCT-KEY19-SEL-SEC
           MOVE    NACCT-SRYYM         TO  WRK-LASTSRYYM
           MOVE    LNK-PTNYUINRRK-REC  TO  NRRK-REC
      *
           .
      *
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    SPA基本情報設定処理
      *****************************************************************
       1001-SPA-SET-SEC                SECTION.
      *
      *    INITIALIZE                          SPA-AREA
      *
           INITIALIZE                          SYSKANRI-REC
           MOVE    "1001"                  TO  SYS-KANRICD
           MOVE    "*"                     TO  SYS-KBNCD
           MOVE    LNK-PTNYUINRRK-TENNYUYMD TO SYS-STYUKYMD
                                               SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC            TO  SYS-1001-REC
      *
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  SPA-HOSPNUM
           MOVE    SYS-1001-PREFNUM        TO  SPA-PREFNUM
           MOVE    "1"                     TO  SPA-NYUGAIKBN
           MOVE    LNK-PTNYUINRRK-PTID     TO  SPA-PTID
           PERFORM 900-PTINF-KEY-SEL-SEC
           MOVE    PTINF-BIRTHDAY          TO  SPA-BIRTHDAY
      *
           .
      *
       1001-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院　会計テーブル編集処理
      *****************************************************************
       4801-NYUKAIKEI-EDIT-SEC         SECTION.
      *
           INITIALIZE                      PTINF-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  PTINF-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF     MCP-RC    =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *---------------退院日の同日入院処理ここから----------------------
           MOVE   SPACE                TO  WRK-DOU-TENNYUYMD
      *    同日再入院用に入院日（転入日）を退避
           MOVE   LNK-PTNYUINRRK-NYUINYMD
                                       TO  WRK-DOU-NAI-I01-NYUINYMD
           MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                       TO  WRK-DOU-PTNYUINRRK-TENNYUYMD
      *    入院履歴の退避
           MOVE   LNK-PTNYUINRRK-REC   TO  WRK-PTNYUINRRK-REC
      *    最終入院履歴が自院歴かを判定する
           MOVE    ZERO                TO  FLG-JIINRRK
           INITIALIZE  PTNYUINRRK-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM
                                       TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DB-SELECT-SEC
           IF    ( MCP-RC      =   ZERO  )
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM  910-DBFETCH-SEC
           END-IF
           IF    ( MCP-RC      =   ZERO  )
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM  910-DBFETCH-SEC
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
               IF      PTNYUINRRK-JTIKBN   NOT =   "5"  AND "6"
                   IF  PTNYUINRRK-TAIINYMD  = WRK-DOU-NAI-I01-NYUINYMD                   
                       MOVE   1        TO  FLG-JIINRRK
                   END-IF
               END-IF
           END-IF
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *    入院履歴の復元
           MOVE    WRK-PTNYUINRRK-REC  TO  LNK-PTNYUINRRK-REC
      *    前歴が自院歴（同日退院分）の場合、入院会計作成日を１日ずらす
           IF     FLG-JIINRRK    =     1
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    LNK-PTNYUINRRK-NYUINYMD
                                       TO  LNK-DAY6-KIJUN
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE    1               TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"       USING     STS-AREA-DAY
                                                 LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  LNK-PTNYUINRRK-NYUINYMD
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
           IF    ( WRK-ERRCD    NOT =  SPACE )
               GO  TO  4801-NYUKAIKEI-EDIT-EXT
           END-IF
      *
           MOVE    WRK-TSUSAN      TO  SYOKIKSN-IN-TUSANNISSU
           MOVE    WRK-SANTEISTYMD
                                   TO  SYOKIKSN-IN-SANTEISTYMD
      *
           PERFORM  4801-BTUSEL-SEC
      *
           IF      LNK-PTNYUINRRK-TOKUTEI-NYUIN    NOT =   SPACE
               PERFORM   4801-TOKTEI-NYUIN-SET-SEC
      *        特定一般病棟入院料の場合は期間加算が存在するため、
      *        初期加算サブを呼び出す（平成２４年改正で追加）
      *        療養病床で有床診療所入院基本料を相互算定する場合も
      *        初期加算サブを呼び出す（平成２４年改正で追加）
      *        特定入院料の算定要件外に算定にする以下入院料を追加（平成２６年改正で対応）
      *        ・一般病棟特別入院基本料
      *        ・精神病棟１５対１入院基本料
      *        ・精神病棟特別入院基本料
               IF    SYOKIKSN-IN-KHNSRYCD   =   "190151910"   OR
                                                "190152010"   OR
                                                "099999950"   OR
                                                "099999951"   OR
                                                "099999952"   OR
                                                "099999955"   OR
                                                "099999956"   OR
                                                "099999957"   OR
      *                                         特定入院料の算定要件外の算定
                                                "190079010"   OR
                                                "190083810"   OR
                                                "190085010"
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "4017"      TO  SPA-ERRCD
                       GO  TO  4801-NYUKAIKEI-EDIT-EXT
                   END-IF
               END-IF
           ELSE
               IF  SYS-5001-BTU-KHNSRYCD      NOT =   SPACE
                   MOVE    SYS-5001-BTU-KHNSRYCD
                                       TO  SYOKIKSN-IN-KHNSRYCD
                   PERFORM   4801-RJNCD-CHG-SEC
                   PERFORM   4801-YAKINGEN-CHG-SEC
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "4017"      TO  WRK-ERRCD
                       GO  TO  4801-NYUKAIKEI-EDIT-EXT
                   END-IF
               ELSE
                   MOVE    "4018"      TO  WRK-ERRCD
                   GO  TO  4801-NYUKAIKEI-EDIT-EXT
               END-IF
           END-IF
      *
      *    入院会計作成サブの呼出し
           INITIALIZE            ORCSNYUACCT-AREA
           MOVE     "1"     TO   LNK-SCNYUACCT-SYORI-KBN
           MOVE     "1"                TO  SPA-NYUGAIKBN
           MOVE     LNK-PTNYUINRRK-PTID    TO  SPA-PTID
           MOVE     LNK-PTNYUINRRK-NYUINKA TO  SPA-SRYKA
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
      *    医療観察入院セット
           INITIALIZE            ORCSIRYOKNS-AREA
           MOVE     LNK-PTNYUINRRK-HOSPNUM   TO  LNK-IRYOKNS-HOSPNUM
           MOVE     LNK-PTNYUINRRK-TOKUTEI-KBN
                                             TO  LNK-IRYOKNS-KBN
           MOVE     LNK-PTNYUINRRK-TOKUTEI-NYUIN
                                             TO  LNK-IRYOKNS-NYUIN
           MOVE     LNK-PTNYUINRRK-BTUNUM    TO  LNK-IRYOKNS-BTUNUM
           DISPLAY  "==  ORCSIRYOKNS CALL ==="
           DISPLAY  "SYOKIKSN-IN-SANTEISTYMD = " SYOKIKSN-IN-SANTEISTYMD
           MOVE     SYOKIKSN-IN-SANTEISTYMD(1:6)
                                         TO  LNK-IRYOKNS-SANTEIYM
           CALL     "ORCSIRYOKNS"   USING   ORCSIRYOKNS-AREA
                                            SPA-AREA
           DISPLAY   "LNK-IRYOKNS-RC = " LNK-IRYOKNS-RC
           IF       LNK-IRYOKNS-RC     =    ZERO
               MOVE   LNK-IRYOKNS-SRYCD(1)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(1)
               MOVE   LNK-IRYOKNS-SRYCD(2)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(2)
               MOVE   LNK-IRYOKNS-SRYCD(3)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(3)
               MOVE   LNK-IRYOKNS-SRYCD(4)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(4)
               MOVE   LNK-IRYOKNS-SRYCD(5)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(5)
               MOVE   SYOKIKSN-IN-SANTEISTYMD
                                        TO  LNK-SCNYUACCT-STSRYYMD
               MOVE   "99999999"        TO  LNK-SCNYUACCT-EDSRYYMD
           END-IF
      *
      *    通算算定済日数
           MOVE   WRK-TSUSAN    TO  LNK-SCNYUACCT-NYUKHN-NISUU
      *
      *    特定入院で入院料が変更になるコードの対応
      *
           INITIALIZE                       ORCSTOKNYUIN2-AREA
           MOVE   LNK-PTNYUINRRK-HOSPNUM
                                        TO  LNK-TOKNYUIN2-HOSPNUM
           MOVE   LNK-PTNYUINRRK-PTID   TO  LNK-TOKNYUIN2-PTID
           MOVE   "1"                   TO  LNK-TOKNYUIN2-IDOKBN
           MOVE   LNK-PTNYUINRRK-NYUINYMD
                                        TO  LNK-TOKNYUIN2-NYUINYMD
           MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                        TO  LNK-TOKNYUIN2-SANTEIYMD
           MOVE   SYOKIKSN-IN-KHNSRYCD  TO  LNK-TOKNYUIN2-SET-SRYCD
           MOVE   STUSAN-NISSU08        TO  LNK-TOKNYUIN2-NISSU08
           MOVE   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                             TO  LNK-TOKNYUIN2-PTNYUINRRK-TOKUTEI-NYUIN
           IF     LNK-AKYUSEI-RC          =   1
              MOVE   "1"      TO   LNK-TOKNYUIN2-TOKUBETSU-SYORI-FLG(1)
           END-IF
           CALL  "ORCSTOKNYUIN2"     USING  ORCSTOKNYUIN2-AREA
                                            SPA-AREA
           IF     LNK-TOKNYUIN2-RC      =   ZERO
              MOVE   SPACE           TO  SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
              PERFORM   VARYING   IDX    FROM    1   BY  1
                UNTIL   IDX       >    5
                  IF   LNK-TOKNYUIN2-SRYCD(IDX)  NOT =   SPACE
                    MOVE  LNK-TOKNYUIN2-SRYCD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                    MOVE  LNK-TOKNYUIN2-STYMD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX)
                    MOVE  LNK-TOKNYUIN2-EDYMD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX)
                  END-IF
              END-PERFORM
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                          TO  IPN-NYUKAIKEI-SAKUSEI-FLG
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                                          TO  IPN-SANTEISTYMD
           END-IF
      *
      *
      *-------特定入院対応ここまで-----------------------------------
      *
      *    入院料加算診療コード取り込み
           PERFORM    4801-NYUKSN-GET-SEC
      *
      *    特定入院料区分のセット
           IF      LNK-PTNYUINRRK-TOKUTEI-NYUIN    =   SPACE
               MOVE     "0"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
           ELSE
               MOVE     "1"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
      *        特定入院基本料、療養病棟入院基本料、療養病床入院基本料等は
      *        特定入院料扱いから外す
               IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   SPA-HOSPNUM       TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
               IF    LNK-SCNYUACCT-KAGEN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   SPA-HOSPNUM       TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
           END-IF
      *    外泊
           MOVE   "099999911"   TO  LNK-SCNYUACCT-GAIHAKU-SRYCD
      *    室料差額
           MOVE   "099999912"   TO  LNK-SCNYUACCT-SAGAKU-SRYCD
           IF     LNK-PTNYUINRRK-RM-SAGAKU   =   SPACE
               MOVE    ZERO     TO  LNK-SCNYUACCT-SAGAKU-KBN
           ELSE
               MOVE   LNK-PTNYUINRRK-RM-SAGAKU 
                                TO  WRK-SAGAKU-KBN
               MOVE   WRK-SAGAKU-KBN9
                                TO  LNK-SCNYUACCT-SAGAKU-KBN
           END-IF
      *    食事区分
           MOVE   "099999913"   TO  LNK-SCNYUACCT-SHOKUJI-SRYCD
           MOVE   0             TO  LNK-SCNYUACCT-SHOKUJIKBN
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
           MOVE   LNK-PTNYUINRRK-HKNCOMBINUM
                                TO  WRK-HKNCOMBINUM
           MOVE   WRK-HKNCOMBINUM9 
                                TO  LNK-SCNYUACCT-HKNCOMBINUM
           MOVE   SPA-SRYKA     TO  WRK-TAIHI-SRYKA
           MOVE   "00"          TO  SPA-SRYKA
           PERFORM 4801-SENTEI-SET-SEC
           MOVE  LNK-PTNYUINRRK-HOSPNUM   TO  SPA-HOSPNUM
           MOVE  LNK-PTNYUINRRK-PTID     TO  SPA-PTID
      *
      *    有床診療所入院基本料の相互算定コード置き換え
           IF    (LNK-SCNYUACCT-STSRYYMD   >=   "20120401")  AND
                 (LNK-SCNYUACCT-STSRYYMD   <=   "20140331")
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    有床診療所入院基本料の相互算定コード置き換え（平成２６年改定）
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20140401"
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3" OR
                                                "4" OR "5" OR "6")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169610"
                             MOVE "190170810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169710"
                             MOVE "190170910" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169810"
                             MOVE "190171010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169910"
                             MOVE "190171110" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170010"
                             MOVE "190171210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170110"
                             MOVE "190171310" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170210"
                             MOVE "190171410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170310"
                             MOVE "190171510" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170410"
                             MOVE "190171610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    特定入院基本料のコード細分化置き換え
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20120401"
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410"
                   EVALUATE   SYS-5001-BTU-SBT
      *                専門病院
                       WHEN    "06"
                             MOVE "190161010" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                障害者施設等
                       WHEN    "05"
                             MOVE "190161210" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
      *
      *            特定機能病院は病棟設定された入院コードから判定
                   IF   SYS-5001-BTU-KHNSRYCD =  "190118610" OR
                                                 "190705610" OR
                                                 "190085910"
                     MOVE "190160810" TO  LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-IF
               END-IF
      *        特別入院基本料
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799510"
                   EVALUATE   SYS-5001-BTU-SBT
      *                専門病院
                       WHEN    "06"
                             MOVE "190161110" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                障害者施設等
                       WHEN    "05"
                             MOVE "190161310" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
               END-IF
           END-IF
           MOVE   LNK-PTNYUINRRK-BTUNUM
                                       TO  LNK-SCNYUACCT-BTU-NUM
      *
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
               DISPLAY  "ORCGI01 ORCSNYUACCT ERR RC = "
                         LNK-SCNYUACCT-RC
           END-IF
      *
           IF     IPN-NYUKAIKEI-SAKUSEI-FLG    =   "1"
               MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
               MOVE    IPN-SANTEISTYMD   TO   WRK-SANTEISTYMD
               MOVE    IPN-SANTEISTYMD   TO   WRK-NAI-I01-IDOYMD
               MOVE    IPN-SANTEISTYMD   TO   LNK-PTNYUINRRK-TENNYUYMD
                   PERFORM   4801-TENROOM-EDIT-SEC
           END-IF
      *
           .
       4801-NYUKAIKEI-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料加算の取り込み処理
      *****************************************************************
       4801-NYUKSN-GET-SEC         SECTION.
      *
      *
      *    入院料加算取得サブ
           INITIALIZE                       ORCSNYUKSNGET-AREA
           MOVE   LNK-PTNYUINRRK-HOSPNUM
                                        TO  LNK-NYUKSNGET-HOSPNUM
           MOVE   LNK-PTNYUINRRK-PTID   TO  LNK-NYUKSNGET-PTID
           EVALUATE   SODRNACCT-ORDERID(3:2)
               WHEN   "01"
               WHEN   "03"
               WHEN   "04"
               WHEN   "05"
                   MOVE   LNK-PTNYUINRRK-TENNYUYMD(1:6)
                                        TO  LNK-NYUKSNGET-SANTEIYM
               WHEN   OTHER
                   MOVE    SPA-SYSYMD   TO  LNK-NYUKSNGET-SANTEIYM
           END-EVALUATE
           MOVE   LNK-PTNYUINRRK-BTUNUM TO  LNK-NYUKSNGET-BTUNUM
           MOVE   LNK-PTNYUINRRK-BRMNUM TO  LNK-NYUKSNGET-BRMNUM
           IF      LNK-SCNYUACCT-NYUKHN-SRYCD(1) NOT = SPACE
               MOVE   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                        TO  LNK-NYUKSNGET-NYUIN-SRYCD
           ELSE
               MOVE   LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                        TO  LNK-NYUKSNGET-NYUIN-SRYCD
           END-IF
      *
      *    特定入院料算定要件外で療養病棟入院基本料Ｉを算定時に、入院料
      *    加算を一般病棟特別入院基本料の例で算定する為、コードの置き換え
           IF      LNK-TOKNYUIN-GAI-NYUKSNKBN   =    "1"
               MOVE   "190079010"       TO  LNK-NYUKSNGET-NYUIN-SRYCD
           END-IF
      *
           CALL    "ORCSNYUKSNGET"      USING  ORCSNYUKSNGET-AREA
                                               SPA-AREA
      *    入院料加算取得サブからの返却値セット
           PERFORM    VARYING    IDX8   FROM   1   BY   1
             UNTIL    (LNK-NYUKSNGET-KSNSRYCD(IDX8)  =  SPACE)  OR
                      (IDX8   >     20)
                 MOVE   LNK-NYUKSNGET-NYUKSNKBN(IDX8)
                             TO  LNK-SCNYUACCT-NYUKHNKSN-KBN(IDX8)
                 MOVE   LNK-NYUKSNGET-KSNSRYCD(IDX8)
                             TO  LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX8)
           END-PERFORM
           MOVE    LNK-NYUKSNGET-TOKUBETSU-NYUIN
                                        TO  WRK-TOKUBETSU-NYUIN
      *
           .
      *
       4801-NYUKSN-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院患者の年齢算出
      *****************************************************************
       4801-PT-AGE-SEC         SECTION.
      *
           INITIALIZE                      PTINF-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  PTINF-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC       =   ZERO  )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
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
      *     選定療養費診療行為コード取得処理
      *****************************************************************
       4801-SENTEI-SET-SEC         SECTION.
      *
           INITIALIZE  SENTEICDCHG-REC
           MOVE    LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                   TO  SENCHG-IPNSRYCD
           MOVE    SENTEICDCHG-REC TO  MCPDATA-REC
           MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO  )
               MOVE    MCPDATA-REC TO  SENTEICDCHG-REC
           ELSE
               INITIALIZE              SENTEICDCHG-REC
           END-IF
      *
           MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      **     MOVE    SENCHG-SENTEISRYCD
      **                             TO  LNK-SCNYUACCT-SENTEI-SRYCD
           IF      SENCHG-SENTEISRYCD    NOT =  SPACE
               MOVE    "099999916"   TO  LNK-SCNYUACCT-SENTEI-SRYCD
               MOVE    WRK-SENTEI-STYMD
                                     TO  LNK-SCNYUACCT-SENTEI-STYMD
           END-IF
      *
           INITIALIZE                      PTINF-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  PTINF-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =   ZERO  )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    １５才未満の患者は選定入院対象外
           DISPLAY "PTINF-BIRTHDAY = " PTINF-BIRTHDAY
           MOVE     PTINF-BIRTHDAY   TO  WRK-BIRTHDAY-15
           ADD      15               TO  WRK-BIRTHDAY-15Y
           DISPLAY  "WRK-SENTEI-STYMD = " WRK-SENTEI-STYMD
           DISPLAY  "WRK-BIRTHDAY-15 = " WRK-BIRTHDAY-15
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
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID TO  STUSAN-PTID
           IF   IPN-NYUKAIKEI-SAKUSEI-FLG  =   "1"
               MOVE   IPN-SANTEISTYMD  TO  STUSAN-KJNYMD
           ELSE
      *---------------退院日の同日入院処理ここから----------------------
               IF   WRK-DOU-TENNYUYMD    NOT =   SPACE
                   MOVE    WRK-DOU-TENNYUYMD
                                       TO  LNK-PTNYUINRRK-TENNYUYMD
               END-IF
      *---------------退院日の同日入院処理ここまで----------------------
               MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                       TO  STUSAN-KJNYMD
           END-IF
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF    ( STUSAN-RC       NOT =   ZERO )
      *         MOVE    "4025"      TO  WRK-ERRCD
               DISPLAY  "WRK-ERRCD = " WRK-ERRCD
      *         GO  TO  4801-TSUSAN-SANTEI-GET-EXT
           END-IF
      *
      *    他院（特別な関係にある医療機関）の日数を加算
           ADD     STUSAN-TAINREL-NISSU  TO   STUSAN-NISSU03
           ADD     STUSAN-TAINREL-NISSU  TO   STUSAN-NISSU05
      *
           COMPUTE WRK-TSUSAN  =   STUSAN-NISSU03  -   1
           MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                   TO  WRK-SANTEISTYMD
      *
           MOVE    STUSAN-KJNYMD   TO  WRK-YMD
           MOVE    "1"             TO  WRK-ZOGENPTN
           COMPUTE WRK-ZOGEN       =   180 - ( STUSAN-NISSU04 )
           PERFORM 5002-CALENDAR-SEC
           MOVE    WRK-KEISANYMD   TO  WRK-SENTEI-STYMD
      *
      *
           .
       4801-TSUSAN-SANTEI-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    病棟情報検索処理
      *****************************************************************
       4801-BTUSEL-SEC             SECTION.
      *
           INITIALIZE              SYSKANRI-REC
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    LNK-PTNYUINRRK-BTUNUM
                                   TO  SYS-KBNCD
           EVALUATE   SODRNACCT-ORDERID(3:2)
               WHEN   "01"
               WHEN   "03"
               WHEN   "04"
               WHEN   "05"
                   MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
      **         WHEN   "08"
      **             MOVE   WRK-NAI-I01-IDOYMD
      **                             TO  SYS-STYUKYMD
      **                                 SYS-EDYUKYMD
      **         WHEN   "09"
      **             MOVE   SPA-NAI-I01-MAEIDOYMD
      **                             TO  SYS-STYUKYMD
      **                                 SYS-EDYUKYMD
               WHEN   OTHER
                   MOVE    SPA-SYSYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           END-EVALUATE
           MOVE    LNK-PTNYUINRRK-HOSPNUM
                                   TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =   ZERO )
               MOVE    MCPDATA-REC
                                  TO  SYS-5001-REC
           ELSE
               MOVE    1          TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
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
           MOVE    LNK-PTNYUINRRK-BRMNUM
                                   TO  SYS-KBNCD
           EVALUATE   SODRNACCT-ORDERID(3:2)
               WHEN   "01"
               WHEN   "03"
               WHEN   "04"
               WHEN   "05"
                   MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
      **         WHEN   "08"
      **             MOVE   WRK-NAI-I01-IDOYMD
      **                             TO  SYS-STYUKYMD
      **                                 SYS-EDYUKYMD
      **         WHEN   "09"
      **             MOVE   SPA-NAI-I01-MAEIDOYMD
      **                             TO  SYS-STYUKYMD
      **                                 SYS-EDYUKYMD
               WHEN   OTHER
                   MOVE    SPA-SYSYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           END-EVALUATE
           MOVE    LNK-PTNYUINRRK-HOSPNUM
                                   TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =   ZERO )
               MOVE    MCPDATA-REC
                                  TO  SYS-5002-REC
           ELSE
               MOVE    1          TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       4801-BRMSEL-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料セット処理
      *****************************************************************
       4801-TOKTEI-NYUIN-SET-SEC   SECTION.
      *
      *    特定入院料コードをサブプロより取得
           INITIALIZE                           ORCSTOKNYUIN-AREA
           MOVE    LNK-PTNYUINRRK-TOKUTEI-KBN   TO  LNK-TOKNYUIN-KBN
           MOVE    LNK-PTNYUINRRK-TOKUTEI-NYUIN TO  LNK-TOKNYUIN-NYUIN
           MOVE    LNK-PTNYUINRRK-BTUNUM        TO  LNK-TOKNYUIN-BTUNUM
           MOVE    LNK-PTNYUINRRK-TOKU-SANTEIYOUKENKBN
                                        TO  LNK-TOKNYUIN-SANTEIYOUKENKBN
           MOVE    SYOKIKSN-IN-SANTEISTYMD(1:6)
                                            TO  LNK-TOKNYUIN-SANTEIYM
           CALL    "ORCSTOKNYUIN"      USING    ORCSTOKNYUIN-AREA
                                                SPA-AREA
           IF      LNK-TOKNYUIN-RC          =   ZERO
               IF    LNK-TOKNYUIN-GAI-SRYCD =  SPACE
                   MOVE   LNK-TOKNYUIN-SRYCD   TO  SYOKIKSN-IN-KHNSRYCD
               ELSE
                   MOVE   LNK-TOKNYUIN-GAI-SRYCD
                                               TO  SYOKIKSN-IN-KHNSRYCD
               END-IF
           END-IF
      *
           .
       4801-TOKTEI-NYUIN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    夜勤時間超過減算コードへの置き換え処理
      *****************************************************************
       4801-YAKINGEN-CHG-SEC   SECTION.
      *
           IF    SYOKIKSN-IN-SANTEISTYMD   >=   "20140401"
               INITIALIZE    ORCSYAKINGEN-AREA
               MOVE   SYOKIKSN-IN-SANTEISTYMD(1:6)
                                   TO    LNK-YAKINGEN-SANTEIYM
               MOVE   SYS-5001-BTU-NUM
                                   TO    LNK-YAKINGEN-BTU-NUM
               MOVE   SYOKIKSN-IN-KHNSRYCD
                                   TO    LNK-YAKINGEN-SRYCD
               CALL   "ORCSYAKINGEN"    USING   ORCSYAKINGEN-AREA
                                                SPA-AREA
               IF   LNK-YAKINGEN-RC     =       ZERO
                    MOVE LNK-YAKINGEN-CHGSRYCD
                                   TO    SYOKIKSN-IN-KHNSRYCD
               END-IF
           END-IF
      *
           .
       4801-YAKINGEN-CHG-EXT.
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
                       MOVE    "tbl_srycdchg"  TO MCP-TABLE
                       MOVE    "key2"          TO MCP-PATHNAME
                       PERFORM 910-DBSELECT-SEC 
                       IF    ( MCP-RC      =   ZERO )
                           MOVE    MCPDATA-REC
                                               TO   SRYCDCHG-REC
                           IF   CHG-RJNSRYCD   NOT =   SPACE
                                MOVE   CHG-RJNSRYCD
                                               TO  SYOKIKSN-IN-KHNSRYCD
                           END-IF
                       END-IF
      *
                       MOVE    "tbl_srycdchg"  TO MCP-TABLE
                       MOVE    "key2"          TO MCP-PATHNAME
                       PERFORM 910-DBCLOSECURSOR-SEC
                   END-IF
               ELSE
                   INITIALIZE              SRYCDCHG-REC
                   MOVE    SYOKIKSN-IN-KHNSRYCD
                                           TO   CHG-IPNSRYCD
                   MOVE    SRYCDCHG-REC    TO   MCPDATA-REC
                   MOVE    "tbl_srycdchg"  TO MCP-TABLE
                   MOVE    "key2"          TO MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC 
                   IF    ( MCP-RC      =   ZERO )
                       MOVE    MCPDATA-REC
                                           TO   SRYCDCHG-REC
                       IF   CHG-RJNSRYCD   NOT =   SPACE
                            MOVE   CHG-RJNSRYCD
                                           TO  SYOKIKSN-IN-KHNSRYCD
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_srycdchg"  TO MCP-TABLE
                   MOVE    "key2"          TO MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
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
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO   COMB-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID
                                       TO   COMB-PTID
           IF      SODRNACCT-ORDERID(3:2) =   "07"  OR  "09"
               MOVE    WRK-TAIHI-HKN   TO   COMB-HKNCOMBINUM
           ELSE
               MOVE    LNK-PTNYUINRRK-HKNCOMBINUM
                                       TO   COMB-HKNCOMBINUM
           END-IF
           MOVE    HKNCOMBI-REC        TO   MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC 
           IF    ( MCP-RC    =   ZERO )
               MOVE    MCPDATA-REC
                                       TO   HKNCOMBI-REC
           ELSE
               MOVE    1               TO   FLG-HKNCOMBI
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           PERFORM  4801-PT-AGE-SEC
      *    入院患者の年齢計算
           INITIALIZE ORCSAGECHKAREA
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  AGECHK-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID
                                       TO  AGECHK-PTID
           MOVE    WRK-AGECHK-SRYYMD
                                       TO  AGECHK-SRYYMD
           MOVE    PTINF-BIRTHDAY
                                       TO  AGECHK-BIRTHDAY
           DISPLAY "AGECHK-SRYYMD" AGECHK-SRYYMD
           DISPLAY "AGECHK-BIRTHDAY" AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING
                                           ORCSAGECHKAREA
                                           SPA-AREA
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
           MOVE    LNK-PTNYUINRRK-HOSPNUM  TO   PTKOH-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID    TO   PTKOH-PTID
           MOVE    "027"            TO   PTKOH-KOHNUM
           MOVE    WRK-AGECHK-SRYYMD
                                    TO   PTKOH-TEKSTYMD
           MOVE    WRK-AGECHK-SRYYMD
                                    TO   PTKOH-TEKEDYMD
           MOVE    PTKOHINF-REC     TO   MCPDATA-REC
           MOVE    "tbl_ptkohinf"   TO   MCP-TABLE
           MOVE    "key6"           TO   MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC 
           IF    ( MCP-RC    =   ZERO )
               MOVE    1            TO  FLG-RJN
           END-IF
           MOVE    "tbl_ptkohinf"   TO   MCP-TABLE
           MOVE    "key6"           TO   MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       4801-RJN-TAISYO-CHK-EXT.
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
           MOVE    LNK-PTNYUINRRK-PTID     TO  SPA-PTID
           MOVE    PTINF-BIRTHDAY
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
               MOVE    "0001"          TO  WRK-ERRCD
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
      *    転科／転棟／転室時　会計テーブル編集処理
      *****************************************************************
       4801-TENROOM-EDIT-SEC         SECTION.
      *
           MOVE      SPACE        TO    WRK-DOU-TENNYUYMD
           MOVE      SPACE        TO    IPN-NYUKAIKEI-SAKUSEI-FLG
      *
           INITIALIZE      ORCSSYOKIKSNAREA
      *
           MOVE    SPACE   TO   SPA-ERRCD
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
      *    救命救急入院料１
      *    救命救急入院料２
      *    一類感染症患者入院医療管理料
      *    救命救急入院料３（救命救急入院料）
      *    救命救急入院料４（救命救急入院料）
      *    特定集中治療室管理料２（特定集中治療室管理料）
      *    小児特定集中治療室管理料
      *    特定集中治療室管理料１（平成２６年改正）
      *    特定集中治療室管理料２（特定集中治療室管理料）（平成２６年改正）
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF LNK-PTNYUINRRK-TOKUTEI-NYUIN = "03" OR "04" OR "05" OR
                                                 "06" OR "07" OR "08" OR
                                                 "09" OR "14" OR "16" OR
                                                 "55" OR "57" OR "59" OR
                                                 "91" OR "AH" OR "AI"
                   IF   STUSAN-NISSU08   >=   14
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    地域包括ケア病棟入院料（平成２６年改正）の算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "2"
              IF  LNK-PTNYUINRRK-TOKUTEI-NYUIN  =  "41" OR "42" OR "43"
                                                OR "44" OR "45" OR "46"
                                                OR "47" OR "48"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
              END-IF
           END-IF
      *
      *    地域包括ケア入院医療管理料（平成２６年改正）の算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
              IF  LNK-PTNYUINRRK-TOKUTEI-NYUIN = "AL" OR "AM" OR "AN" OR
                                                 "AO" OR "AP" OR "AQ" OR
                                                 "AR" OR "AS"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    特定一般病棟入院料（地域包括ケア入院医療管理１）（平成２６年改正）
      *    特定一般病棟入院料（地域包括ケア入院医療管理２）（平成２６年改正）
      *    の算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN  = "AU" OR "AV"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    広範囲熱傷特定集中治療室管理料算定時に特定入院での算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "13"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    ハイケアユニット入院医療管理料算定時に特定入院での算定日が２１日を
      *    経過していた場合は一般の入院料算定とする
      *    以下の特定入院料も同様
      *    ハイケアユニット入院医療管理料１（平成２６年改正）
      *    ハイケアユニット入院医療管理料２（平成２６年改正）
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN  = "17" OR "AJ" OR "AK"
                   IF   STUSAN-NISSU08   >=   21
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *     DISPLAY "====AZU  TENROOM-EDIT =================="
      *     DISPLAY "RRKNUM     = " PTNYUINRRK-RRKNUM
      *     DISPLAY "RRKEDANUM  = " PTNYUINRRK-RRKEDANUM
      *     DISPLAY "NYUINYMD   = " PTNYUINRRK-NYUINYMD
      *     DISPLAY "TAIINYMD   = " PTNYUINRRK-TAIINYMD
      *     DISPLAY "TENNYUYMD  = " PTNYUINRRK-TENNYUYMD
      *     DISPLAY "TENSTUYMD  = " PTNYUINRRK-TENSTUYMD
      *    平成２４年３月３１日時点で亜急性期入院医療管理料１を算定
      *    している状態か判定するサブを呼び出し
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF  LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "18"  OR  "54"
                   INITIALIZE                  ORCSAKYUSEI-AREA
                   MOVE LNK-PTNYUINRRK-HOSPNUM  TO  LNK-AKYUSEI-HOSPNUM
                   MOVE LNK-PTNYUINRRK-PTID     TO  LNK-AKYUSEI-PTID
                   MOVE LNK-PTNYUINRRK-RRKNUM   TO  LNK-AKYUSEI-RRKNUM
                   CALL    "ORCSAKYUSEI"    USING  ORCSAKYUSEI-AREA
                                                   SPA-AREA
                   DISPLAY  "LNK-AKYUSEI-RC = " LNK-AKYUSEI-RC
               END-IF
           END-IF
      *
      *    亜急性期入院医療管理料１算定時に特定入院での算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
      *    （平成２４年３月３１日までの入院分は９０日限度）
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "18"
                  IF   PTNYUINRRK-NYUINYMD   >=   "20120401"
                      IF  STUSAN-NISSU08   >=   60
                          MOVE  SPACE  TO  LNK-PTNYUINRRK-TOKUTEI-NYUIN
                      END-IF
                  ELSE
                      IF  STUSAN-NISSU08   >=   90
                          MOVE  SPACE  TO  LNK-PTNYUINRRK-TOKUTEI-NYUIN
                      END-IF
                  END-IF
               END-IF
           END-IF
      *
      *    亜急性期入院医療管理料２算定時に特定入院での算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
      *    （経過措置により平成２４年３月３１日時点で亜急性期入院医療
      *      管理料１を算定している場合は９０日限度とする対応追加）
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
             IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "54"
               IF  LNK-PTNYUINRRK-NYUINYMD   >=   "20120401"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO  LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               ELSE
                   IF    LNK-AKYUSEI-RC   =   1
                      IF   STUSAN-NISSU08   >=   90
                          MOVE  SPACE  TO  LNK-PTNYUINRRK-TOKUTEI-NYUIN
                      END-IF
                   ELSE
                      IF   STUSAN-NISSU06   >=   60
                          MOVE  SPACE  TO  LNK-PTNYUINRRK-TOKUTEI-NYUIN
                      END-IF
                   END-IF
               END-IF
             END-IF
           END-IF
      *
      *    亜急性期入院医療管理料１（指定地域）算定時に特定入院での算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "92"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    亜急性期入院医療管理料２（指定地域）算定時に特定入院での算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "93"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    特定一般病棟入院料（亜急性期入院医療管理）算定時に特定入院での
      *    算定日が６０日を経過していた場合は一般の入院料算定とする
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "95"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    特定一般病棟入院料（亜急性期入院医療管理・リハビリ算定患者）
      *    算定時に特定入院での算定日が６０日を経過していた場合は一般の
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "96"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    救命救急入院料３（広範囲熱傷特定集中治療）の場合は最長６０日間の算定の為、
      *    次月の会計作成時に６０日経過時は無条件に一般の入院料算定とする
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN = "56"
                   IF       STUSAN-NISSU08  >=  60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    救命救急入院料４（広範囲熱傷特定集中治療）の場合は最長６０日間の算定の為、
      *    次月の会計作成時に６０日経過時は無条件に一般の入院料算定とする
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN = "58"
                   IF       STUSAN-NISSU08  >=  60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    特定集中治療室管理料２（広範囲熱傷特定集中治療）の場合は最長６０日間の算定の為、
      *    次月の会計作成時に６０日経過時は無条件に一般の入院料算定とする
      *    以下の特定入院料も同様
      *    特定集中治療室管理料２（広範囲熱傷特定集中治療）（平成２６年改正）
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN = "60" OR "AT"
                   IF       STUSAN-NISSU08  >=  60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    脳卒中ケアユニット入院医療管理料算定時に特定入院での算定日が１４日を
      *    経過していた場合は一般の入院料算定とする
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "19"
                   IF   STUSAN-NISSU08   >=   14
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    総合周産期特定集中治療室管理料（母体・胎児）算定時に特定入院での
      *    算定日が１４日を経過していた場合は一般の入院料算定とする
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "11"
                   IF   STUSAN-NISSU08   >=   14
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    以下の特定入院料は出生児体重より算定上限日数を判定し、
      *    経過していた場合は一般の入院料算定とする
      *    ・新生児特定集中治療室管理料１
      *    ・新生児特定集中治療室管理料２
      *    ・総合周産期特定集中治療室管理料（新生児）
      *    ・新生児治療回復室入院医療管理料
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "10"  OR
                                                     "61"  OR
                                                     "12"  OR
                                                     "62"
      *          出生児体重の取得
                 MOVE   SPACE                TO  PTCONF-REC
                 INITIALIZE                      PTCONF-REC
                 MOVE   LNK-PTNYUINRRK-HOSPNUM
                                             TO  PTCONF-HOSPNUM
                 MOVE   LNK-PTNYUINRRK-PTID  TO  PTCONF-PTID
                 MOVE   "BIRTHWEIGHT"        TO  PTCONF-CKEY
                 PERFORM 900-PTCONF-READ-SEC
                 IF     FLG-PTCONF           =   ZERO
                     MOVE   PTCONF-CDATA(1:4)  TO  WRK-WEIGHT
                 ELSE
                     MOVE   9999               TO  WRK-WEIGHT
                 END-IF
      *
      *          新生児特定集中治療室管理料１
      *          新生児特定集中治療室管理料２
                 IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "10"  OR  "61"
                    IF    WRK-WEIGHT     <   1000
                       IF   STUSAN-NISSU08   >=   90
                          MOVE    SPACE   TO
                                          LNK-PTNYUINRRK-TOKUTEI-NYUIN
                       END-IF
                    END-IF
                    IF   (WRK-WEIGHT     >=  1000)   AND
                         (WRK-WEIGHT     <   1500)
                       IF   STUSAN-NISSU08   >=   60
                          MOVE    SPACE   TO
                                          LNK-PTNYUINRRK-TOKUTEI-NYUIN
                       END-IF
                    END-IF
                    IF    WRK-WEIGHT     >=  1500
                       IF   STUSAN-NISSU08   >=   21
                          MOVE    SPACE   TO
                                          LNK-PTNYUINRRK-TOKUTEI-NYUIN
                       END-IF
                    END-IF
                 END-IF
      *
      *          総合周産期特定集中治療室管理料（新生児）
                 IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "12"
                    IF    WRK-WEIGHT     <   1000
                       IF   STUSAN-NISSU08   >=   90
                          MOVE    SPACE   TO
                                          LNK-PTNYUINRRK-TOKUTEI-NYUIN
                       END-IF
                    END-IF
                    IF   (WRK-WEIGHT     >=  1000)   AND
                         (WRK-WEIGHT     <   1500)
                       IF   STUSAN-NISSU08   >=   60
                          MOVE    SPACE   TO
                                          LNK-PTNYUINRRK-TOKUTEI-NYUIN
                       END-IF
                    END-IF
                    IF    WRK-WEIGHT     >=  1500
                       IF   STUSAN-NISSU08   >=   21
                          MOVE    SPACE   TO
                                          LNK-PTNYUINRRK-TOKUTEI-NYUIN
                       END-IF
                    END-IF
                 END-IF
      *
      *          新生児治療回復室入院医療管理料
                 IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "62"
                    IF    WRK-WEIGHT     <   1000
                       IF   STUSAN-NISSU08   >=   120
                          MOVE    SPACE   TO
                                          LNK-PTNYUINRRK-TOKUTEI-NYUIN
                       END-IF
                    END-IF
                    IF   (WRK-WEIGHT     >=  1000)   AND
                         (WRK-WEIGHT     <   1500)
                       IF   STUSAN-NISSU08   >=   90
                          MOVE    SPACE   TO
                                          LNK-PTNYUINRRK-TOKUTEI-NYUIN
                       END-IF
                    END-IF
                    IF    WRK-WEIGHT     >=  1500
                       IF   STUSAN-NISSU08   >=   30
                          MOVE    SPACE   TO
                                          LNK-PTNYUINRRK-TOKUTEI-NYUIN
                       END-IF
                    END-IF
                 END-IF
      *
               END-IF
           END-IF
      *
           IF      LNK-PTNYUINRRK-TOKUTEI-NYUIN    NOT =   SPACE
               PERFORM   4801-TOKTEI-NYUIN-SET-SEC
      *        特定一般病棟入院料の場合は期間加算が存在するため、
      *        初期加算サブを呼び出す（平成２４年改正で追加）
      *        療養病床で有床診療所入院基本料を相互算定する場合も
      *        初期加算サブを呼び出す（平成２４年改正で追加）
      *        特定入院料の算定要件外に算定にする以下入院料を追加（平成２６年改正で対応）
      *        ・一般病棟特別入院基本料
      *        ・精神病棟１５対１入院基本料
      *        ・精神病棟特別入院基本料
               IF    SYOKIKSN-IN-KHNSRYCD   =   "190151910"   OR
                                                "190152010"   OR
                                                "099999950"   OR
                                                "099999951"   OR
                                                "099999952"   OR
                                                "099999955"   OR
                                                "099999956"   OR
                                                "099999957"   OR
      *                                         特定入院料の算定要件外の算定
                                                "190079010"   OR
                                                "190083810"   OR
                                                "190085010"
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "4021"      TO  SPA-ERRCD
                       GO  TO  4801-TENROOM-EDIT-EXT
                   END-IF
               END-IF
           ELSE
               PERFORM   4801-BTUSEL-SEC
               IF  SYS-5001-BTU-KHNSRYCD      NOT =   SPACE
                   MOVE    SYS-5001-BTU-KHNSRYCD
                                       TO  SYOKIKSN-IN-KHNSRYCD
                   PERFORM   4801-RJNCD-CHG-SEC
                   PERFORM   4801-YAKINGEN-CHG-SEC
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
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
           IF  LNK-PTNYUINRRK-TENNYUYMD(1:6)  >= "200604"
      *      （異動月分）
             PERFORM   VARYING   IDX-SKJ-TAIHI  FROM  1 BY  1
               UNTIL   IDX-SKJ-TAIHI  >    3
               INITIALIZE                      NYUINACCT-REC
               MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
               MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    LNK-PTNYUINRRK-TENNYUYMD(1:6)
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
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD(7:2)
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
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD(1:6)
                                         TO  WRK-TENROOM-SKJ-SRYYM(1)
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD(7:2)
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
               PERFORM 910-DBCLOSECURSOR-SEC
             END-PERFORM
      *      （異動翌月分）
             PERFORM   VARYING   IDX-SKJ-TAIHI  FROM  1 BY  1
                UNTIL   IDX-SKJ-TAIHI  >    3
               INITIALIZE                      NYUINACCT-REC
               MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
               MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
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
               PERFORM 910-DBCLOSECURSOR-SEC
             END-PERFORM
           ELSE
      *      （異動月分）
             INITIALIZE                      NYUINACCT-REC
             MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
             MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
             MOVE    LNK-PTNYUINRRK-TENNYUYMD(1:6)
                                         TO  NACCT-SRYYM
                                             WRK-TENROOM-SKJ-SRYYMX
      *
             MOVE    NYUINACCT-REC       TO  MCPDATA-REC
             MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
             MOVE    "key28"             TO  MCP-PATHNAME
             PERFORM   910-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *          食事を退避
                 MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(1)
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD(7:2)
                                         TO  IDX-SHOKUJIX
                 PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                           UNTIL     IDX-SKJ   >   31
                      MOVE   NACCT-DAY(IDX-SKJ)
                                         TO  WRK-TENROOM-SKJ(1 IDX-SKJ)
                 END-PERFORM
             ELSE
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD(1:6)
                                         TO  WRK-TENROOM-SKJ-SRYYM(1)
                 MOVE    LNK-PTNYUINRRK-TENNYUYMD(7:2)
                                         TO  IDX-SHOKUJIX
                 PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                           UNTIL     IDX-SKJ   >   31
                      MOVE   ZERO        TO  WRK-TENROOM-SKJ(1 IDX-SKJ)
                 END-PERFORM
             END-IF
      *      入院会計マスタクローズ
             MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
             MOVE    "key28"             TO  MCP-PATHNAME
             PERFORM 910-DBCLOSECURSOR-SEC
      *      （異動翌月分）
             INITIALIZE                      NYUINACCT-REC
             MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
             MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
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
             MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
             MOVE    "key28"             TO  MCP-PATHNAME
             PERFORM   910-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
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
             MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
             MOVE    "key28"             TO  MCP-PATHNAME
             PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
      *    入院会計作成前に会計データの外泊区分を退避
      *    （外泊異動月分）
           INITIALIZE                       NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID      TO  NACCT-PTID
           MOVE    LNK-PTNYUINRRK-TENNYUYMD(1:6)
                                       TO  NACCT-SRYYM
                                           WRK-TENROOM-SKJ-SRYYMX
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM   910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *        外泊を退避
               MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(1)
               MOVE    LNK-PTNYUINRRK-TENNYUYMD(7:2)
                                       TO  IDX-SHOKUJIX
               PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                         UNTIL     IDX-SKJ   >   31
                    MOVE   NACCT-DAY(IDX-SKJ)
                                       TO WRK-TENROOM-GAIHAKU(1 IDX-SKJ)
               END-PERFORM
           ELSE
               MOVE    LNK-PTNYUINRRK-TENNYUYMD(1:6)
                                       TO  WRK-TENROOM-SKJ-SRYYM(1)
               MOVE    LNK-PTNYUINRRK-TENNYUYMD(7:2)
                                       TO  IDX-SHOKUJIX
               PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                         UNTIL     IDX-SKJ   >   31
                    MOVE   ZERO        TO WRK-TENROOM-GAIHAKU(1 IDX-SKJ)
               END-PERFORM
           END-IF
      *    入院会計マスタクローズ
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *    （外泊異動翌月分）
           INITIALIZE                       NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID      TO  NACCT-PTID
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
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM   910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *        外泊を退避
               PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                         UNTIL     IDX-SKJ   >   31
                    MOVE   NACCT-DAY(IDX-SKJ)
                                       TO WRK-TENROOM-GAIHAKU(2 IDX-SKJ)
               END-PERFORM
           END-IF
      *    入院会計マスタクローズ
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
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
           MOVE    LNK-PTNYUINRRK-TENNYUYMD
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
      *
      *    医療観察入院セット
           INITIALIZE            ORCSIRYOKNS-AREA
           MOVE     LNK-PTNYUINRRK-HOSPNUM   TO  LNK-IRYOKNS-HOSPNUM
           MOVE     LNK-PTNYUINRRK-TOKUTEI-KBN
                                             TO  LNK-IRYOKNS-KBN
           MOVE     LNK-PTNYUINRRK-TOKUTEI-NYUIN
                                             TO  LNK-IRYOKNS-NYUIN
           MOVE     LNK-PTNYUINRRK-BTUNUM    TO  LNK-IRYOKNS-BTUNUM
           DISPLAY  "==  ORCSIRYOKNS CALL ==="
           DISPLAY  "SYOKIKSN-IN-SANTEISTYMD = " SYOKIKSN-IN-SANTEISTYMD
           MOVE     SYOKIKSN-IN-SANTEISTYMD(1:6)
                                         TO  LNK-IRYOKNS-SANTEIYM
           CALL     "ORCSIRYOKNS"   USING   ORCSIRYOKNS-AREA
                                            SPA-AREA
           DISPLAY   "LNK-IRYOKNS-RC = " LNK-IRYOKNS-RC
           IF       LNK-IRYOKNS-RC     =    ZERO
               MOVE   LNK-IRYOKNS-SRYCD(1)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(1)
               MOVE   LNK-IRYOKNS-SRYCD(2)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(2)
               MOVE   LNK-IRYOKNS-SRYCD(3)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(3)
               MOVE   LNK-IRYOKNS-SRYCD(4)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(4)
               MOVE   LNK-IRYOKNS-SRYCD(5)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(5)
               MOVE   SYOKIKSN-IN-SANTEISTYMD
                                        TO  LNK-SCNYUACCT-STSRYYMD
               MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                        TO  LNK-SCNYUACCT-EDSRYYMD
           END-IF
      *
      *    通算算定済日数
           MOVE   WRK-TSUSAN    TO  LNK-SCNYUACCT-NYUKHN-NISUU
      *
      *    特定入院で入院料が変更になるコードの対応
           INITIALIZE                       ORCSTOKNYUIN2-AREA
           MOVE   LNK-PTNYUINRRK-HOSPNUM
                                        TO  LNK-TOKNYUIN2-HOSPNUM
           MOVE   LNK-PTNYUINRRK-PTID   TO  LNK-TOKNYUIN2-PTID
           MOVE   "1"                   TO  LNK-TOKNYUIN2-IDOKBN
           MOVE   LNK-PTNYUINRRK-NYUINYMD
                                        TO  LNK-TOKNYUIN2-NYUINYMD
           MOVE   LNK-PTNYUINRRK-TENNYUYMD
                                        TO  LNK-TOKNYUIN2-SANTEIYMD
           MOVE   SYOKIKSN-IN-KHNSRYCD  TO  LNK-TOKNYUIN2-SET-SRYCD
           MOVE   STUSAN-NISSU08        TO  LNK-TOKNYUIN2-NISSU08
           MOVE   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                             TO  LNK-TOKNYUIN2-PTNYUINRRK-TOKUTEI-NYUIN
           IF     LNK-AKYUSEI-RC          =   1
              MOVE   "1"      TO   LNK-TOKNYUIN2-TOKUBETSU-SYORI-FLG(1)
           END-IF
           CALL  "ORCSTOKNYUIN2"     USING  ORCSTOKNYUIN2-AREA
                                            SPA-AREA
           IF     LNK-TOKNYUIN2-RC      =   ZERO
              MOVE   SPACE           TO  SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
              PERFORM   VARYING   IDX    FROM    1   BY  1
                UNTIL   IDX       >    5
                  IF   LNK-TOKNYUIN2-SRYCD(IDX)  NOT =   SPACE
                    MOVE  LNK-TOKNYUIN2-SRYCD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                    MOVE  LNK-TOKNYUIN2-STYMD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX)
                    MOVE  LNK-TOKNYUIN2-EDYMD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX)
                  END-IF
              END-PERFORM
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                          TO  IPN-NYUKAIKEI-SAKUSEI-FLG
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                                          TO  IPN-SANTEISTYMD
           END-IF
      *
      *-------特定入院対応ここまで-----------------------------------
      *
      *    入院料加算診療コード取り込み
           PERFORM    4801-NYUKSN-GET-SEC
      *
      *    特定入院料区分のセット
           IF      LNK-PTNYUINRRK-TOKUTEI-NYUIN    =   SPACE
               MOVE     "0"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
           ELSE
               MOVE     "1"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
      *        特定入院基本料、療養病棟入院基本料、療養病床入院基本料等は
      *        特定入院料扱いから外す
               IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   SPA-HOSPNUM       TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
               IF    LNK-SCNYUACCT-KAGEN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   SPA-HOSPNUM       TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
           END-IF
      *    外泊
           MOVE   "099999911"   TO  LNK-SCNYUACCT-GAIHAKU-SRYCD
      *    室料差額
           MOVE   "099999912"   TO  LNK-SCNYUACCT-SAGAKU-SRYCD
           IF     LNK-PTNYUINRRK-RM-SAGAKU   =   SPACE
               MOVE    ZERO     TO  LNK-SCNYUACCT-SAGAKU-KBN
           ELSE
               MOVE   LNK-PTNYUINRRK-RM-SAGAKU 
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
                  MOVE   0      TO  LNK-SCNYUACCT-SHOKUJIKBN
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
           MOVE   LNK-PTNYUINRRK-HKNCOMBINUM
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
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  SPA-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  SPA-PTID
      *
      *    有床診療所入院基本料の相互算定コード置き換え
           IF    (LNK-SCNYUACCT-STSRYYMD   >=   "20120401")  AND
                 (LNK-SCNYUACCT-STSRYYMD   <=   "20140331")
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    有床診療所入院基本料の相互算定コード置き換え（平成２６年改定）
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20140401"
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3" OR
                                                "4" OR "5" OR "6")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169610"
                             MOVE "190170810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169710"
                             MOVE "190170910" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169810"
                             MOVE "190171010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169910"
                             MOVE "190171110" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170010"
                             MOVE "190171210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170110"
                             MOVE "190171310" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170210"
                             MOVE "190171410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170310"
                             MOVE "190171510" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170410"
                             MOVE "190171610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    特定入院基本料のコード細分化置き換え
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20120401"
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410"
                   EVALUATE   SYS-5001-BTU-SBT
      *                専門病院
                       WHEN    "06"
                             MOVE "190161010" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                障害者施設等
                       WHEN    "05"
                             MOVE "190161210" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
      *
      *            特定機能病院は病棟設定された入院コードから判定
                   IF   SYS-5001-BTU-KHNSRYCD =  "190118610" OR
                                                 "190705610" OR
                                                 "190085910"
                     MOVE "190160810" TO  LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-IF
               END-IF
      *        特別入院基本料
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799510"
                   EVALUATE   SYS-5001-BTU-SBT
      *                専門病院
                       WHEN    "06"
                             MOVE "190161110" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                障害者施設等
                       WHEN    "05"
                             MOVE "190161310" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
               END-IF
           END-IF
      *
           MOVE   LNK-PTNYUINRRK-BTUNUM
                                       TO  LNK-SCNYUACCT-BTU-NUM
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
               DISPLAY  "ORCGI01 ORCSNYUACCT ERR RC = "
                         LNK-SCNYUACCT-RC
           END-IF
      *
           IF     IPN-NYUKAIKEI-SAKUSEI-FLG    =   "1"
               MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
               MOVE    IPN-SANTEISTYMD   TO   WRK-SANTEISTYMD
               MOVE    IPN-SANTEISTYMD   TO   LNK-PTNYUINRRK-TENNYUYMD
               MOVE    IPN-SANTEISTYMD   TO   PTNYUINRRK-TENNYUYMD
           END-IF
      *
      *    入院会計作成後に退避データの食事区分を復元
           IF  LNK-PTNYUINRRK-TENNYUYMD(1:6)  >= "200604"
      *       （異動月分）
               IF   WRK-TENROOM-SKJ-SRYYM(1)  NOT =   SPACE
                PERFORM    VARYING  IDX-SKJ-TAIHI  FROM  1  BY  1
                 UNTIL    IDX-SKJ-TAIHI   >   3
                 INITIALIZE                      NYUINACCT-REC
                 MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
                 MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
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
                     MOVE    LNK-PTNYUINRRK-TENNYUYMD(7:2)
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
                 MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
                 MOVE     "key"              TO  MCP-PATHNAME
                 PERFORM 910-DBUPDATE-SEC
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
                 PERFORM 910-DBCLOSECURSOR-SEC
                END-PERFORM
               END-IF
      *
      *       （異動翌月分）
               IF   WRK-TENROOM-SKJ-SRYYM(2)  NOT =   SPACE
               PERFORM    VARYING  IDX-SKJ-TAIHI  FROM  1  BY  1
                 UNTIL    IDX-SKJ-TAIHI   >   3
                   INITIALIZE                      NYUINACCT-REC
                   MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
                   MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
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
                   MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
                   MOVE     "key"              TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
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
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-PERFORM
               END-IF
           ELSE
      *       （異動月分）
               INITIALIZE                      NYUINACCT-REC
               MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
               MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    WRK-TENROOM-SKJ-SRYYM(1)
                                           TO  NACCT-SRYYM
      *
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM   910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *            食事を復元
                   MOVE    LNK-PTNYUINRRK-TENNYUYMD(7:2)
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
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key"              TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
      *        入院会計マスタクローズ
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
      *       （異動翌月分）
               IF   WRK-TENROOM-SKJ-SRYYM(2)  NOT =   SPACE
                   INITIALIZE                      NYUINACCT-REC
                   MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
                   MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
                   MOVE    WRK-TENROOM-SKJ-SRYYM(2)
                                               TO  NACCT-SRYYM
      *
                   MOVE    NYUINACCT-REC       TO  MCPDATA-REC
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key28"             TO  MCP-PATHNAME
                   PERFORM   910-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
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
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key28"             TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
           END-IF
      *
      *    入院会計作成後に退避データの外泊区分を復元
      *    （異動月分）
           INITIALIZE                        NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID      TO  NACCT-PTID
           MOVE    WRK-TENROOM-SKJ-SRYYM(1)
                                       TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM   910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *        外泊を復元
               MOVE    LNK-PTNYUINRRK-TENNYUYMD(7:2)
                                       TO  IDX-SHOKUJIX
               PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                         UNTIL     IDX-SKJ   >   31
                     MOVE   WRK-TENROOM-GAIHAKU(1 IDX-SKJ)
                                       TO  NACCT-DAY(IDX-SKJ)
               END-PERFORM
      *        剤回数再セット
               MOVE  ZERO              TO  NACCT-ZAIKAISU
               PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                        UNTIL     IDX-SKJ   >   31
                   IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                       ADD   1        TO  NACCT-ZAIKAISU
                   END-IF
               END-PERFORM   
           END-IF
           MOVE     NYUINACCT-REC      TO  MCPDATA-REC
           MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
           MOVE     "key"              TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
      *    入院会計マスタクローズ
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *   （異動翌月分）
           IF   WRK-TENROOM-SKJ-SRYYM(2)  NOT =   SPACE
               INITIALIZE                       NYUINACCT-REC
               MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
               MOVE    LNK-PTNYUINRRK-PTID      TO  NACCT-PTID
               MOVE    WRK-TENROOM-SKJ-SRYYM(2)
                                           TO  NACCT-SRYYM
      *
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key26"             TO  MCP-PATHNAME
               PERFORM   910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *            外泊を復元
                   PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                             UNTIL     IDX-SKJ   >   31
                        MOVE   WRK-TENROOM-GAIHAKU(2 IDX-SKJ)
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
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key26"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
           .
       4801-TENROOM-EDIT-EXT.
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
      *      救命救急入院料１
      *      救命救急入院料２
      *      一類感染症患者入院医療管理料
      *      広範囲熱傷特定集中治療室管理料
      *      ハイケアユニット入院医療管理料
      *      亜急性期入院医療管理料
      *      脳卒中ケアユニット入院医療管理料
      *      亜急性期入院医療管理料２
      *      亜急性期入院医療管理料１（指定地域）
      *      亜急性期入院医療管理料２（指定地域）
      *      救命救急入院料３（救命救急入院料）
      *      救命救急入院料３（広範囲熱傷特定集中治療）
      *      救命救急入院料４（救命救急入院料）
      *      救命救急入院料４（広範囲熱傷特定集中治療）
      *      特定集中治療室管理料２（特定集中治療室管理料）
      *      特定集中治療室管理料２（広範囲熱傷特定集中治療）
      *      小児特定集中治療室管理料
      *      特定一般病棟入院料（亜急性期入院医療管理）
      *      特定一般病棟入院料（亜急性期入院医療管理・リハビリ算定患者）
      *      特定集中治療室管理料１（平成２６年改正）
      *      特定集中治療室管理料２（特定集中治療室管理料）（平成２６年改正）
      *      特定集中治療室管理料２（広範囲熱傷特定集中治療）（平成２６年改正）
      *      ハイケアユニット入院医療管理料１（平成２６年改正）
      *      ハイケアユニット入院医療管理料２（平成２６年改正）
      *      地域包括ケア入院医療管理料１（平成２６年改正）
      *      地域包括ケア入院医療管理料１（生活療養）（平成２６年改正）
      *      地域包括ケア入院医療管理料２（平成２６年改正）
      *      地域包括ケア入院医療管理料２（生活療養）（平成２６年改正）
      *      地域包括ケア入院医療管理料１（特定地域）（平成２６年改正）
      *      地域包括ケア入院医療管理料１（生活療養）（特定地域）（平成２６年改正）
      *      地域包括ケア入院医療管理料２（特定地域）（平成２６年改正）
      *      地域包括ケア入院医療管理料２（生活療養）（特定地域）（平成２６年改正）
      *      特定一般病棟入院料（地域包括ケア入院医療管理１）（平成２６年改正）
      *      特定一般病棟入院料（地域包括ケア入院医療管理２）（平成２６年改正）
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF LNK-PTNYUINRRK-TOKUTEI-NYUIN = "03" OR "04" OR "05" OR
                                                "06" OR "07" OR "08" OR
                                                "09" OR "13" OR "14" OR
                                                "16" OR "17" OR "18" OR
                                                "19" OR "54" OR "55" OR
                                                "56" OR "57" OR "58" OR
                                                "59" OR "60" OR "91" OR
                                                "92" OR "93" OR "95" OR
                                                "96" OR "AH" OR "AI" OR
                                                "AT" OR "AJ" OR "AK" OR
                                                "AL" OR "AM" OR "AN" OR
                                                "AO" OR "AP" OR "AQ" OR
                                                "AR" OR "AS" OR "AU" OR
                                                "AV"
                    MOVE   LNK-PTNYUINRRK-TAIINYMD
                                       TO   LNK-PTNYUINRRK-TENNYUYMD
      **                                      SPA-NAI-I01-IDOYMD
               END-IF
      *        療養病棟と療養病床も判定条件に追加
               IF    ((LNK-PTNYUINRRK-TOKUTEI-NYUIN >=   "25")  AND
                      (LNK-PTNYUINRRK-TOKUTEI-NYUIN <=   "48"))
                                             OR
                     ((LNK-PTNYUINRRK-TOKUTEI-NYUIN >=   "63")  AND
                      (LNK-PTNYUINRRK-TOKUTEI-NYUIN <=   "88"))
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    LNK-PTNYUINRRK-TAIINYMD
                                               TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE     1                  TO  LNK-DAY6-ZOGEN
                   CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN     TO
                                               LNK-PTNYUINRRK-TENNYUYMD
               END-IF
           ELSE
               IF  LNK-PTNYUINRRK-TOKUTEI-KBN   =   SPACE
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    LNK-PTNYUINRRK-TAIINYMD
                                               TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE     1                  TO  LNK-DAY6-ZOGEN
                   CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN     TO
                                               LNK-PTNYUINRRK-TENNYUYMD
      *             MOVE   SPA-NAI-I01-TAIINYMD
      *                                 TO   PTNYUINRRK-TENNYUYMD
               END-IF
      *        緩和ケア病棟入院料、認知症治療病棟入院料１、認知症治療病棟入院料２
      *        を算定時に退院日をＳＰＡ−異動日と転入日にセット
      *        以下の特定入院料も同様
      *        地域包括ケア病棟入院料１（平成２６年改正）
      *        地域包括ケア病棟入院料１（生活療養）（平成２６年改正）
      *        地域包括ケア病棟入院料２（平成２６年改正）
      *        地域包括ケア病棟入院料２（生活療養）（平成２６年改正）
      *        地域包括ケア病棟入院料１（特定地域）（平成２６年改正）
      *        地域包括ケア病棟入院料１（生活療養）（特定地域）（平成２６年改正）
      *        地域包括ケア病棟入院料２（特定地域）（平成２６年改正）
      *        地域包括ケア病棟入院料２（生活療養）（特定地域）（平成２６年改正）
               IF   (PTNYUINRRK-TOKUTEI-KBN   =   "2")  AND
                    (PTNYUINRRK-TOKUTEI-NYUIN  = "09" OR "05" OR "15"
                                              OR "41" OR "42" OR "43"
                                              OR "44" OR "45" OR "46"
                                              OR "47" OR "48" )
                    MOVE   LNK-PTNYUINRRK-TAIINYMD
                                       TO   LNK-PTNYUINRRK-TENNYUYMD
      **                                      SPA-NAI-I01-IDOYMD
               END-IF
           END-IF
      *
      *    退院取消前の会計元データ作成
           INITIALIZE      ORCSSYOKIKSNAREA
      *
      *    入院会計削除前に会計データの食事区分と保険組合せを退避
           INITIALIZE                      NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM
                                       TO  NACCT-HOSPNUM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    LNK-PTNYUINRRK-PTID TO  NACCT-PTID
           MOVE    "00"                TO  NACCT-SRYKA
           MOVE    LNK-PTNYUINRRK-TAIINYMD(1:6)
                                       TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM   910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO        TO      WRK-TAIHI-SHOKUJI
               PERFORM UNTIL       MCP-RC       NOT =   ZERO
      *
                   MOVE     MCPDATA-REC   TO   NYUINACCT-REC
      *
      *            食事を退避
                   IF      NACCT-ZAISKBKBN     =   "4"
                       MOVE    LNK-PTNYUINRRK-TAIINYMD(7:2)
                                         TO    IDX-SHOKUJIX
                       IF      NACCT-DAY(IDX-SHOKUJI9)  NOT =   ZERO
                            MOVE   NACCT-DAY(IDX-SHOKUJI9) TO
                                                  WRK-TAIHI-SHOKUJI
                       END-IF
                   END-IF
      *            保険を退避
                   IF      NACCT-ZAISKBKBN     =   "5"
                       MOVE   LNK-PTNYUINRRK-TAIINYMD(7:2)
                                         TO    IDX-SHOKUJIX
                       IF      NACCT-DAY(IDX-SHOKUJI9)  NOT =   ZERO
                            MOVE   NACCT-DAY(IDX-SHOKUJI9) TO
                                                  WRK-TAIHI-HKN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key16"             TO  MCP-PATHNAME
                   PERFORM 910-DBFETCH-SEC
               END-PERFORM
           ELSE
               INITIALIZE                  NYUINACCT-REC
           END-IF
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *
           DISPLAY "=========KAKUNIN =================="
           MOVE     LNK-PTNYUINRRK-TAIINYMD
                                       TO  LNK-PTNYUINRRK-TENNYUYMD
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
      *    救命救急入院料１
      *    救命救急入院料２
      *    一類感染症患者入院医療管理料
      *    救命救急入院料３（救命救急入院料）
      *    救命救急入院料４（救命救急入院料）
      *    特定集中治療室管理料２（特定集中治療室管理料）
      *    小児特定集中治療室管理料
      *    特定集中治療室管理料１（平成２６年改正）
      *    特定集中治療室管理料２（特定集中治療室管理料）（平成２６年改正）
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
             IF  LNK-PTNYUINRRK-TOKUTEI-NYUIN  = "03" OR "04" OR "05" OR
                                                 "06" OR "07" OR "08" OR
                                                 "09" OR "14" OR "16" OR
                                                 "55" OR "57" OR "59" OR
                                                 "91" OR "AH" OR "AI"
                   IF   STUSAN-NISSU08   >=   14
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
             END-IF
           END-IF
      *
      *    地域包括ケア病棟入院料（平成２６年改正）の算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "2"
               IF LNK-PTNYUINRRK-TOKUTEI-NYUIN = "41" OR "42" OR "43"
                                              OR "44" OR "45" OR "46"
                                              OR "47" OR "48"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    地域包括ケア入院医療管理料（平成２６年改正）の算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF LNK-PTNYUINRRK-TOKUTEI-NYUIN = "AL" OR "AM" OR "AN" OR
                                                "AO" OR "AP" OR "AQ" OR
                                                "AR" OR "AS"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    特定一般病棟入院料（地域包括ケア入院医療管理１）（平成２６年改正）
      *    特定一般病棟入院料（地域包括ケア入院医療管理２）（平成２６年改正）
      *    の算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN  = "AU" OR "AV"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    救命救急入院料３（広範囲熱傷特定集中治療）の算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "56"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    救命救急入院料４（広範囲熱傷特定集中治療）の算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "58"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    特定集中治療室管理料（広範囲熱傷特定集中治療）の算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
      *    以下の特定入院料も同様
      *    特定集中治療室管理料２（広範囲熱傷特定集中治療）（平成２６年改正）
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "60" OR "AT"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    広範囲熱傷特定集中治療室管理料算定時に特定入院での算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "13"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    ハイケアユニット入院医療管理料算定時に特定入院での算定日が２１日を
      *    経過していた場合は一般の入院料算定とする
      *    以下の特定入院料も同様
      *    ハイケアユニット入院医療管理料１（平成２６年改正）
      *    ハイケアユニット入院医療管理料２（平成２６年改正）
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "17" OR "AJ" OR "AK"
                   IF   STUSAN-NISSU08   >=   21
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *     DISPLAY "====AZU  TAIIN-CANCEL=================="
      *     DISPLAY "RRKNUM     = " PTNYUINRRK-RRKNUM
      *     DISPLAY "RRKEDANUM  = " PTNYUINRRK-RRKEDANUM
      *     DISPLAY "NYUINYMD   = " PTNYUINRRK-NYUINYMD
      *     DISPLAY "TAIINYMD   = " PTNYUINRRK-TAIINYMD
      *     DISPLAY "TENNYUYMD  = " PTNYUINRRK-TENNYUYMD
      *     DISPLAY "TENSTUYMD  = " PTNYUINRRK-TENSTUYMD
      *    平成２４年３月３１日時点で亜急性期入院医療管理料１を算定
      *    している状態か判定するサブを呼び出し
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "18"  OR  "54"
                   INITIALIZE                  ORCSAKYUSEI-AREA
                   MOVE  LNK-PTNYUINRRK-HOSPNUM  TO  LNK-AKYUSEI-HOSPNUM
                   MOVE  LNK-PTNYUINRRK-PTID     TO  LNK-AKYUSEI-PTID
                   MOVE  LNK-PTNYUINRRK-RRKNUM   TO  LNK-AKYUSEI-RRKNUM
                   CALL    "ORCSAKYUSEI"    USING  ORCSAKYUSEI-AREA
                                                   SPA-AREA
                   DISPLAY  "LNK-AKYUSEI-RC = " LNK-AKYUSEI-RC
               END-IF
           END-IF
      *
      *    亜急性期入院医療管理料１算定時に特定入院での算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
      *    （平成２４年３月３１日までの入院分は９０日限度）
           IF  LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "18"
                  IF   LNK-PTNYUINRRK-NYUINYMD   >=   "20120401"
                      IF  STUSAN-NISSU08   >=   60
                          MOVE  SPACE  TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                      END-IF
                  ELSE
                      IF  STUSAN-NISSU08   >=   90
                          MOVE  SPACE  TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                      END-IF
                  END-IF
               END-IF
           END-IF
      *
      *    亜急性期入院医療管理料２算定時に特定入院での算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
      *    （経過措置により平成２４年３月３１日時点で亜急性期入院医療
      *      管理料１を算定している場合は９０日限度とする対応追加）
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
             IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "54"
               IF  LNK-PTNYUINRRK-NYUINYMD   >=   "20120401"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               ELSE
                   IF    LNK-AKYUSEI-RC   =   1
                      IF   STUSAN-NISSU08   >=   90
                          MOVE  SPACE   TO  LNK-PTNYUINRRK-TOKUTEI-NYUIN
                      END-IF
                   ELSE
                      IF   STUSAN-NISSU08   >=   60
                          MOVE  SPACE   TO  LNK-PTNYUINRRK-TOKUTEI-NYUIN
                      END-IF
                   END-IF
               END-IF
             END-IF
           END-IF
      *
      *    亜急性期入院医療管理料１（指定地域）算定時に特定入院での算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "92"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    亜急性期入院医療管理料２（指定地域）算定時に特定入院での算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "93"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    脳卒中ケアユニット入院医療管理料算定時に特定入院での算定日が１４日を
      *    経過していた場合は一般の入院料算定とする
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "19"
                   IF   STUSAN-NISSU08   >=   14
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    特定一般病棟入院料（亜急性期入院医療管理）算定時に特定入院での
      *    算定日が６０日を経過していた場合は一般の入院料算定とする
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "95"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    特定一般病棟入院料（亜急性期入院医療管理・リハビリ算定患者）
      *    算定時に特定入院での算定日が６０日を経過していた場合は一般の
      *    入院料算定とする
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "96"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    総合周産期特定集中治療室管理料（母体・胎児）算定時に特定入院での
      *    算定日が１４日を経過していた場合は一般の入院料算定とする
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "11"
                   IF   STUSAN-NISSU08   >=   14
                       MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    以下の特定入院料は出生児体重より算定上限日数を判定し、
      *    経過していた場合は一般の入院料算定とする
      *    ・新生児特定集中治療室管理料１
      *    ・新生児特定集中治療室管理料２
      *    ・総合周産期特定集中治療室管理料（新生児）
      *    ・新生児治療回復室入院医療管理料
           IF   LNK-PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "10"  OR
                                                 "61"  OR
                                                 "12"  OR
                                                 "62"
      *          出生児体重の取得
                 MOVE   SPACE                TO  PTCONF-REC
                 INITIALIZE                      PTCONF-REC
                 MOVE   PTNYUINRRK-HOSPNUM   TO  PTCONF-HOSPNUM
                 MOVE   PTNYUINRRK-PTID      TO  PTCONF-PTID
                 MOVE   "BIRTHWEIGHT"        TO  PTCONF-CKEY
                 PERFORM 900-PTCONF-READ-SEC
                 IF     FLG-PTCONF           =   ZERO
                     MOVE   PTCONF-CDATA(1:4)  TO  WRK-WEIGHT
                 ELSE
                     MOVE   9999               TO  WRK-WEIGHT
                 END-IF
      *
      *          新生児特定集中治療室管理料１
      *          新生児特定集中治療室管理料２
                 IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "10"  OR  "61"
                    IF    WRK-WEIGHT     <   1000
                       IF   STUSAN-NISSU08   >=   90
                          MOVE  SPACE  TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                       END-IF
                    END-IF
                    IF   (WRK-WEIGHT     >=  1000)   AND
                         (WRK-WEIGHT     <   1500)
                       IF   STUSAN-NISSU08   >=   60
                          MOVE  SPACE  TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                       END-IF
                    END-IF
                    IF    WRK-WEIGHT     >=  1500
                       IF   STUSAN-NISSU08   >=   21
                          MOVE  SPACE  TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                       END-IF
                    END-IF
                 END-IF
      *
      *          総合周産期特定集中治療室管理料（新生児）
                 IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "12"
                    IF    WRK-WEIGHT     <   1000
                       IF   STUSAN-NISSU08   >=   90
                          MOVE  SPACE  TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                       END-IF
                    END-IF
                    IF   (WRK-WEIGHT     >=  1000)   AND
                         (WRK-WEIGHT     <   1500)
                       IF   STUSAN-NISSU08   >=   60
                          MOVE  SPACE  TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                       END-IF
                    END-IF
                    IF    WRK-WEIGHT     >=  1500
                       IF   STUSAN-NISSU08   >=   21
                          MOVE  SPACE  TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                       END-IF
                    END-IF
                 END-IF
      *
      *          新生児治療回復室入院医療管理料
                 IF   LNK-PTNYUINRRK-TOKUTEI-NYUIN   = "62"
                    IF    WRK-WEIGHT     <   1000
                       IF   STUSAN-NISSU08   >=   120
                          MOVE  SPACE  TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                       END-IF
                    END-IF
                    IF   (WRK-WEIGHT     >=  1000)   AND
                         (WRK-WEIGHT     <   1500)
                       IF   STUSAN-NISSU08   >=   90
                          MOVE  SPACE  TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                       END-IF
                    END-IF
                    IF    WRK-WEIGHT     >=  1500
                       IF   STUSAN-NISSU08   >=   30
                          MOVE  SPACE  TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                       END-IF
                    END-IF
                 END-IF
      *
               END-IF
           END-IF
      *
           PERFORM  4801-BTUSEL-SEC
           MOVE     SYS-5001-BTU-KHNSRYCD
                          TO    SYOKIKSN-IN-KHNSRYCD
      *
      *
           IF      LNK-PTNYUINRRK-TOKUTEI-NYUIN    NOT =   SPACE
               PERFORM   4801-TOKTEI-NYUIN-SET-SEC
      *        特定一般病棟入院料の場合は期間加算が存在するため、
      *        初期加算サブを呼び出す（平成２４年改正で追加）
      *        療養病床で有床診療所入院基本料を相互算定する場合も
      *        初期加算サブを呼び出す（平成２４年改正で追加）
      *        特定入院料の算定要件外に算定にする以下入院料を追加（平成２６年改正で対応）
      *        ・一般病棟特別入院基本料
      *        ・精神病棟１５対１入院基本料
      *        ・精神病棟特別入院基本料
               IF    SYOKIKSN-IN-KHNSRYCD   =   "190151910"   OR
                                                "190152010"   OR
                                                "099999950"   OR
                                                "099999951"   OR
                                                "099999952"   OR
                                                "099999955"   OR
                                                "099999956"   OR
                                                "099999957"   OR
      *                                         特定入院料の算定要件外の算定
                                                "190079010"   OR
                                                "190083810"   OR
                                                "190085010"
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "4017"      TO  SPA-ERRCD
                       GO  TO  4801-TAIINKAIKEI-CANCEL-EXT
                   END-IF
               END-IF
           ELSE
               IF  SYS-5001-BTU-KHNSRYCD      NOT =   SPACE
                   MOVE    SYS-5001-BTU-KHNSRYCD
                                       TO  SYOKIKSN-IN-KHNSRYCD
                   PERFORM   4801-RJNCD-CHG-SEC
                   PERFORM   4801-YAKINGEN-CHG-SEC
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
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
      *    入院会計作成サブの呼出し
           INITIALIZE                      ORCSNYUACCT-AREA
           MOVE     "7"                TO  LNK-SCNYUACCT-SYORI-KBN
           MOVE     "1"                TO  SPA-NYUGAIKBN
           MOVE     LNK-PTNYUINRRK-PTID
                                       TO  SPA-PTID
           MOVE     LNK-PTNYUINRRK-NYUINKA
                                       TO  SPA-SRYKA
           MOVE     LNK-PTNYUINRRK-NYUINYMD
                                       TO   LNK-SCNYUACCT-STSRYYMD
           MOVE    SYOKIKSN-IN-KHNSRYCD
                            TO   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *退院日からの会計再作成に変更 ---------------------------------
      *///     MOVE     SPA-NAI-I01-TAIINYMD
      *///                                 TO   LNK-SCNYUACCT-STSRYYMD
           MOVE     LNK-PTNYUINRRK-TAIINYMD
                                       TO   LNK-SCNYUACCT-STSRYYMD
      *h26.05.16 ADD
           MOVE     LNK-PTNYUINRRK-TAIINYMD
                                       TO   LNK-SCNYUACCT-EDSRYYMD
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
      *
           DISPLAY "===  TEST TAIINKAIKEI-CANCEL ==="
           INITIALIZE                       ORCSTOKNYUIN2-AREA
           MOVE   LNK-PTNYUINRRK-HOSPNUM
                                        TO  LNK-TOKNYUIN2-HOSPNUM
           MOVE   LNK-PTNYUINRRK-PTID   TO  LNK-TOKNYUIN2-PTID
           MOVE   "1"                   TO  LNK-TOKNYUIN2-IDOKBN
           MOVE   LNK-PTNYUINRRK-NYUINYMD
                                        TO  LNK-TOKNYUIN2-NYUINYMD
           MOVE   LNK-PTNYUINRRK-TAIINYMD
                                        TO  LNK-TOKNYUIN2-SANTEIYMD
           MOVE   SYOKIKSN-IN-KHNSRYCD  TO  LNK-TOKNYUIN2-SET-SRYCD
           MOVE   STUSAN-NISSU08        TO  LNK-TOKNYUIN2-NISSU08
           MOVE   LNK-PTNYUINRRK-TOKUTEI-NYUIN
                             TO  LNK-TOKNYUIN2-PTNYUINRRK-TOKUTEI-NYUIN
           IF    (LNK-AKYUSEI-RC        =   1)           AND
                 (LNK-PTNYUINRRK-TAIINYMD  >=   "20120401")
              MOVE   "1"      TO   LNK-TOKNYUIN2-TOKUBETSU-SYORI-FLG(1)
           END-IF
           CALL  "ORCSTOKNYUIN2"     USING  ORCSTOKNYUIN2-AREA
                                            SPA-AREA
           IF     LNK-TOKNYUIN2-RC      =   ZERO
              MOVE   SPACE           TO  SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
              PERFORM   VARYING   IDX    FROM    1   BY  1
                UNTIL   IDX       >    5
                  IF   LNK-TOKNYUIN2-SRYCD(IDX)  NOT =   SPACE
                    MOVE  LNK-TOKNYUIN2-SRYCD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                    MOVE  LNK-TOKNYUIN2-STYMD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX)
                    MOVE  LNK-TOKNYUIN2-EDYMD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX)
                  END-IF
              END-PERFORM
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                          TO  IPN-NYUKAIKEI-SAKUSEI-FLG
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                                          TO  IPN-SANTEISTYMD
           END-IF
      *
      *
      *-------特定入院対応ここまで-----------------------------------
      *
      *    入院料加算診療コード取り込み
           PERFORM    4801-NYUKSN-GET-SEC
      *
      *    退院日に救急・在宅等支援療養病床初期加算（病院）が算定
      *    されているかチェックして、算定されていれば入院料加算の
      *    扱いに含める
      *//     PERFORM   210-KYUKYU-KSN1-SEC
      *
      *    退院日に救急・在宅等支援療養病床初期加算（診療所）が算定
      *    されているかチェックして、算定されていれば入院料加算の
      *    扱いに含める
      *//     PERFORM   210-KYUKYU-KSN2-SEC
      *
      *    退院日に救急・在宅等支援療養病床初期加算（療養病棟入院基本料１）が算定
      *    されているかチェックして、算定されていれば入院料加算の
      *    扱いに含める
      *//     PERFORM   210-KYUKYU-KSN3-SEC
      *
      *    退院日に救急・在宅等支援療養病床初期加算（一般病棟入院基本料）が算定
      *    されているかチェックして、算定されていれば入院料加算の
      *    扱いに含める
      *//     PERFORM   210-KYUKYU-KSN4-SEC
      *
      *    退院日に救急・在宅等支援療養病床初期加算（特定一般病棟入院料）が算定
      *    されているかチェックして、算定されていれば入院料加算の
      *    扱いに含める
      *//     PERFORM   210-KYUKYU-KSN5-SEC
      *
      *    退院日に救急・在宅等支援療養病床初期加算（地域包括ケア病棟入院料）が算定
      *    されているかチェックして、算定されていれば入院料加算の
      *    扱いに含める
      *//     PERFORM   210-KYUKYU-KSN6-SEC
      *
      *    退院日に有床診療所一般病床初期加算が算定
      *    されているかチェックして、算定されていれば入院料加算の
      *    扱いに含める
      *//     PERFORM   210-YUUSHOUIPPAN-KSN-SEC
      *
      *    特定入院料区分のセット
           IF      LNK-PTNYUINRRK-TOKUTEI-NYUIN    =   SPACE
               MOVE     "0"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
           ELSE
               MOVE     "1"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
      *        特定入院基本料、療養病棟入院基本料、療養病床入院基本料等は
      *        特定入院料扱いから外す
               IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   SPA-HOSPNUM       TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
               IF    LNK-SCNYUACCT-KAGEN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   SPA-HOSPNUM       TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
           END-IF
      *    外泊
           MOVE   "099999911"   TO  LNK-SCNYUACCT-GAIHAKU-SRYCD
      *    室料差額
           MOVE   "099999912"   TO  LNK-SCNYUACCT-SAGAKU-SRYCD
           IF     LNK-PTNYUINRRK-RM-SAGAKU   =   SPACE
               MOVE    ZERO     TO  LNK-SCNYUACCT-SAGAKU-KBN
           ELSE
               MOVE   LNK-PTNYUINRRK-RM-SAGAKU 
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
                  MOVE   LNK-PTNYUINRRK-HKNCOMBINUM
                                TO  WRK-HKNCOMBINUM
                  MOVE   WRK-HKNCOMBINUM9 
                                TO  LNK-SCNYUACCT-HKNCOMBINUM
           END-IF
           MOVE   SPA-SRYKA     TO  WRK-TAIHI-SRYKA
           MOVE   "00"          TO  SPA-SRYKA
           IF     LNK-PTNYUINRRK-SENTEIKBN  =   "1"
               PERFORM 4801-SENTEI-SET-SEC
           END-IF
      *    入院カレンダー未設定の判定
           IF     LNK-PTNYUINRRK-NYUCALKBN   =   "1"
                  MOVE   1      TO  LNK-SCNYUACCT-NYUCAL-KBN
           ELSE
                  MOVE   ZERO   TO  LNK-SCNYUACCT-NYUCAL-KBN
           END-IF
      *
      *    有床診療所入院基本料の相互算定コード置き換え
           IF    (LNK-SCNYUACCT-STSRYYMD   >=   "20120401")  AND
                 (LNK-SCNYUACCT-STSRYYMD   <=   "20140331")
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    有床診療所入院基本料の相互算定コード置き換え（平成２６年改定）
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20140401"
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3" OR
                                                "4" OR "5" OR "6")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169610"
                             MOVE "190170810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169710"
                             MOVE "190170910" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169810"
                             MOVE "190171010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169910"
                             MOVE "190171110" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170010"
                             MOVE "190171210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170110"
                             MOVE "190171310" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170210"
                             MOVE "190171410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170310"
                             MOVE "190171510" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170410"
                             MOVE "190171610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    特定入院基本料のコード細分化置き換え
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20120401"
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410"
                   EVALUATE   SYS-5001-BTU-SBT
      *                専門病院
                       WHEN    "06"
                             MOVE "190161010" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                障害者施設等
                       WHEN    "05"
                             MOVE "190161210" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
      *
      *            特定機能病院は病棟設定された入院コードから判定
                   IF   SYS-5001-BTU-KHNSRYCD =  "190118610" OR
                                                 "190705610" OR
                                                 "190085910"
                     MOVE "190160810" TO  LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-IF
               END-IF
      *        特別入院基本料
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799510"
                   EVALUATE   SYS-5001-BTU-SBT
      *                専門病院
                       WHEN    "06"
                             MOVE "190161110" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                障害者施設等
                       WHEN    "05"
                             MOVE "190161310" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
               END-IF
           END-IF
      *
           MOVE   LNK-PTNYUINRRK-BTUNUM   TO  LNK-SCNYUACCT-BTU-NUM
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
                  DISPLAY  "ORCGI01 ORCSNYUACCT ERR RC = "
                            LNK-SCNYUACCT-RC
           END-IF
      *
           IF     IPN-NYUKAIKEI-SAKUSEI-FLG    =   "1"
               MOVE    SPACE   TO   LNK-PTNYUINRRK-TOKUTEI-NYUIN
               MOVE    IPN-SANTEISTYMD   TO   WRK-SANTEISTYMD
      **         MOVE    IPN-SANTEISTYMD   TO   SPA-NAI-I01-IDOYMD
               MOVE    IPN-SANTEISTYMD   TO   LNK-PTNYUINRRK-TENNYUYMD
               PERFORM   4801-TENROOM-EDIT-SEC
           END-IF
      *
           .
       4801-TAIINKAIKEI-CANCEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    異動取消時会計テーブル編集処理
      *****************************************************************
       4801-IDOKAIKEI-CANCEL-SEC       SECTION.
      *
           MOVE   SPACE                 TO  WRK-DOU-TENNYUYMD
                                            WRK-MAEIDOYMD
      *     DISPLAY "=============AZU1  SAKUJO ========"
      *     DISPLAY "LNK-PTNYUINRRK-TOKUTEI-KBN = " 
      *              LNK-PTNYUINRRK-TOKUTEI-KBN
      *     DISPLAY "LNK-PTNYUINRRK-TOKUTEI-NYUIN= "
      *              LNK-PTNYUINRRK-TOKUTEI-NYUIN
      *    入院会計作成サブの呼出し（異動分の会計削除）
           INITIALIZE            ORCSNYUACCT-AREA
           MOVE     "4"     TO   LNK-SCNYUACCT-SYORI-KBN
           MOVE     "1"                 TO  SPA-NYUGAIKBN
           MOVE     LNK-PTNYUINRRK-PTID TO  SPA-PTID
           MOVE     LNK-PTNYUINRRK-NYUINKA
                                        TO  SPA-SRYKA
           INITIALIZE                   STS-AREA-DAY
           INITIALIZE                   LNK-DAY6-AREA
           MOVE    "61"                 TO  LNK-DAY6-IRAI
           MOVE    LNK-PTNYUINRRK-TENNYUYMD
                                        TO  LNK-DAY6-KIJUN
           MOVE    "1"                  TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                   TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"              USING   STS-AREA-DAY
                                                LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN      TO  LNK-SCNYUACCT-EDSRYYMD
                                            WRK-MAEIDOYMD
      *
      *    入院会計削除前に会計データの食事区分と保険組合せを退避
           INITIALIZE                       NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    "1"                      TO  NACCT-NYUGAIKBN
           MOVE    LNK-PTNYUINRRK-PTID      TO  NACCT-PTID
           MOVE    "00"                     TO  NACCT-SRYKA
           MOVE    LNK-SCNYUACCT-EDSRYYMD(1:6)
                                        TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC        TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
           MOVE    "key16"              TO  MCP-PATHNAME
           PERFORM   910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO        TO      WRK-TAIHI-SHOKUJI
               PERFORM UNTIL       MCP-RC       NOT =   ZERO
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
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key16"             TO  MCP-PATHNAME
                   PERFORM 910-DBFETCH-SEC
               END-PERFORM
           ELSE
               INITIALIZE                  NYUINACCT-REC
           END-IF
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    入院会計作成前に会計データの食事区分を退避
           IF  LNK-SCNYUACCT-EDSRYYMD(1:6)  >= "200604"
      *      （異動月分）
             PERFORM   VARYING   IDX-SKJ-TAIHI  FROM  1 BY  1
               UNTIL   IDX-SKJ-TAIHI  >    3
               INITIALIZE                      NYUINACCT-REC
               MOVE    LNK-PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
               MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    LNK-SCNYUACCT-EDSRYYMD(1:6)
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
                 EVALUATE  IDX-SKJ-TAIHI
                     WHEN  1
                        PERFORM VARYING IDX-SKJ FROM  1  BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   NACCT-DAY(IDX-SKJ)
                                         TO WRK-TENROOM-SKJ18(1 IDX-SKJ)
                        END-PERFORM
                     WHEN  2
                        PERFORM VARYING IDX-SKJ FROM  1  BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   NACCT-DAY(IDX-SKJ)
                                         TO WRK-TENROOM-SKJ19(1 IDX-SKJ)
                        END-PERFORM
                     WHEN  3
                        PERFORM VARYING IDX-SKJ FROM  1  BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   NACCT-DAY(IDX-SKJ)
                                         TO WRK-TENROOM-SKJ20(1 IDX-SKJ)
                        END-PERFORM
                 END-EVALUATE
               ELSE
                 MOVE    LNK-SCNYUACCT-EDSRYYMD(1:6)
                                         TO  WRK-TENROOM-SKJ-SRYYM(1)
                 MOVE    LNK-SCNYUACCT-EDSRYYMD(7:2)
                                         TO  IDX-SHOKUJIX
                 EVALUATE  IDX-SKJ-TAIHI
                     WHEN  1
                        PERFORM VARYING IDX-SKJ FROM  1  BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   ZERO
                                         TO WRK-TENROOM-SKJ18(1 IDX-SKJ)
                        END-PERFORM
                     WHEN  2
                        PERFORM VARYING IDX-SKJ FROM  1  BY  1
                           UNTIL     IDX-SKJ   >   31
                               MOVE   ZERO
                                         TO WRK-TENROOM-SKJ19(1 IDX-SKJ)
                        END-PERFORM
                     WHEN  3
                        PERFORM VARYING IDX-SKJ FROM  1  BY  1
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
               PERFORM 910-DBCLOSECURSOR-SEC
             END-PERFORM
      *     （異動翌月分）
             PERFORM   VARYING   IDX-SKJ-TAIHI  FROM  1 BY  1
                UNTIL   IDX-SKJ-TAIHI  >    3
               INITIALIZE                      NYUINACCT-REC
               MOVE    LNK-PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
               MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
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
               PERFORM 910-DBCLOSECURSOR-SEC
             END-PERFORM
           ELSE
      *        （異動月分）
               INITIALIZE                      NYUINACCT-REC
               MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
               MOVE    LNK-PTNYUINRRK-PTID      TO  NACCT-PTID
               MOVE    LNK-SCNYUACCT-EDSRYYMD(1:6)
                                           TO  NACCT-SRYYM
                                               WRK-TENROOM-SKJ-SRYYMX
      *
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM   910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *            食事を退避
                   MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(1)
                   MOVE    LNK-SCNYUACCT-EDSRYYMD(7:2)
                                           TO  IDX-SHOKUJIX
                   MOVE    LNK-SCNYUACCT-EDSRYYMD(7:2)
                                           TO  WRK-TENROOM-SKJ-DD(1)
                   PERFORM  VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                            UNTIL     IDX-SKJ   >   31
                        MOVE   NACCT-DAY(IDX-SKJ)
                                       TO  WRK-TENROOM-SKJ(1 IDX-SKJ)
                   END-PERFORM
               END-IF
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *        （異動翌月分）
               INITIALIZE                      NYUINACCT-REC
               MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
               MOVE    LNK-PTNYUINRRK-PTID      TO  NACCT-PTID
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
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM   910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *            食事を退避
                   MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(2)
                   PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                             UNTIL     IDX-SKJ   >   31
                        MOVE   NACCT-DAY(IDX-SKJ)
                                        TO  WRK-TENROOM-SKJ(2 IDX-SKJ)
                   END-PERFORM
               END-IF
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
           END-IF
      *
      *    入院会計作成前に会計データの外泊区分を退避
      *    （外泊異動月分）
           INITIALIZE                      NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID      TO  NACCT-PTID
           MOVE    LNK-SCNYUACCT-EDSRYYMD(1:6)
                                       TO  NACCT-SRYYM
                                           WRK-TENROOM-SKJ-SRYYMX
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM   910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *        外泊を退避
               MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(1)
               MOVE    LNK-SCNYUACCT-EDSRYYMD(7:2)
                                       TO  IDX-SHOKUJIX
               MOVE    LNK-SCNYUACCT-EDSRYYMD(7:2)
                                       TO  WRK-TENROOM-SKJ-DD(1)
               PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                         UNTIL     IDX-SKJ   >   31
                    MOVE   NACCT-DAY(IDX-SKJ)
                                       TO WRK-TENROOM-GAIHAKU(1 IDX-SKJ)
               END-PERFORM
           END-IF
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *    （外泊異動翌月分）
           INITIALIZE                      NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID      TO  NACCT-PTID
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
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM   910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *        外泊を退避
               MOVE    NACCT-SRYYM     TO  WRK-TENROOM-SKJ-SRYYM(2)
               PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                         UNTIL     IDX-SKJ   >   31
                    MOVE   NACCT-DAY(IDX-SKJ)
                                       TO WRK-TENROOM-GAIHAKU(2 IDX-SKJ)
               END-PERFORM
           END-IF
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    保険組合せ
           MOVE   LNK-PTNYUINRRK-HKNCOMBINUM
                                       TO  WRK-HKNCOMBINUM
           MOVE   WRK-HKNCOMBINUM9     TO  LNK-SCNYUACCT-HKNCOMBINUM
           MOVE   SPA-SRYKA            TO  WRK-TAIHI-SRYKA
           MOVE   "00"                 TO  SPA-SRYKA
      *    入院カレンダー未設定の判定
           IF     PTNYUINRRK-NYUCALKBN   =   "1"
                  MOVE   1      TO  LNK-SCNYUACCT-NYUCAL-KBN
           ELSE
                  MOVE   ZERO   TO  LNK-SCNYUACCT-NYUCAL-KBN
           END-IF
      *
      *    入院カレンダー未設定の判定
      ***     IF     PTNYUINRRK-NYUCALKBN   =   "1"
      ***            MOVE   1      TO  LNK-SCNYUACCT-NYUCAL-KBN
      ***     ELSE
      ***            MOVE   ZERO   TO  LNK-SCNYUACCT-NYUCAL-KBN
      ***     END-IF
      *
           MOVE   LNK-PTNYUINRRK-BTUNUM   TO  LNK-SCNYUACCT-BTU-NUM
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
           MOVE    LNK-PTNYUINRRK-HOSPNUM
                                       TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO  )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE              PTNYUINRRK-REC
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *     DISPLAY "=============AZU2  IDO-MAE-SAISAKUSEI========"
      *     DISPLAY "PTNYUINRRK-TOKUTEI-KBN = " PTNYUINRRK-TOKUTEI-KBN
      *     DISPLAY "PTNYUINRRK-TOKUTEI-NYUIN= "PTNYUINRRK-TOKUTEI-NYUIN
      *     DISPLAY "WRK-MAEIDOYMD = " WRK-MAEIDOYMD
      *
      *    特定集中治療室管理料算定時に退院日をＳＰＡ−異動日と転入日にセット
      *    以下の特定入院料も同様
      *    救命救急入院料１
      *    救命救急入院料２
      *    一類感染症患者入院医療管理料
      *    広範囲熱傷特定集中治療室管理料
      *    ハイケアユニット入院医療管理料
      *    亜急性期入院医療管理料
      *    脳卒中ケアユニット入院医療管理料
      *    亜急性期入院医療管理料２
      *    亜急性期入院医療管理料１（指定地域）
      *    亜急性期入院医療管理料２（指定地域）
      *    救命救急入院料３（救命救急入院料）
      *    救命救急入院料３（広範囲熱傷特定集中治療）
      *    救命救急入院料４（救命救急入院料）
      *    救命救急入院料４（広範囲熱傷特定集中治療）
      *    特定集中治療室管理料２（特定集中治療室管理料）
      *    特定集中治療室管理料２（広範囲熱傷特定集中治療）
      *    小児特定集中治療室管理料
      *    特定一般病棟入院料（亜急性期入院医療管理）
      *    特定一般病棟入院料（亜急性期入院医療管理・リハビリ算定患者）
      *    特定集中治療室管理料１（平成２６年改正）
      *    特定集中治療室管理料２（特定集中治療室管理料）（平成２６年改正）
      *    ハイケアユニット入院医療管理料１（平成２６年改正）
      *    ハイケアユニット入院医療管理料２（平成２６年改正）
      *    地域包括ケア入院医療管理料１（平成２６年改正）
      *    地域包括ケア入院医療管理料１（生活療養）（平成２６年改正）
      *    地域包括ケア入院医療管理料２（平成２６年改正）
      *    地域包括ケア入院医療管理料２（生活療養）（平成２６年改正）
      *    地域包括ケア入院医療管理料１（特定地域）（平成２６年改正）
      *    地域包括ケア入院医療管理料１（生活療養）（特定地域）（平成２６年改正）
      *    地域包括ケア入院医療管理料２（特定地域）（平成２６年改正）
      *    地域包括ケア入院医療管理料２（生活療養）（特定地域）（平成２６年改正）
      *    特定一般病棟入院料（地域包括ケア入院医療管理１）（平成２６年改正）
      *    特定一般病棟入院料（地域包括ケア入院医療管理２）（平成２６年改正）
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN  = "03" OR "04" OR "05" OR
                                                "06" OR "07" OR "08" OR
                                                "09" OR "13" OR "14" OR
                                                "16" OR "17" OR "18" OR
                                                "19" OR "54" OR "55" OR
                                                "56" OR "57" OR "58" OR
                                                "59" OR "60" OR "91" OR
                                                "92" OR "93" OR "95" OR
                                                "96" OR "AH" OR "AI" OR
                                                "AJ" OR "AK" OR
                                                "AL" OR "AM" OR "AN" OR
                                                "AO" OR "AP" OR "AQ" OR
                                                "AR" OR "AS" OR "AU" OR
                                                "AV"
                    MOVE   WRK-MAEIDOYMD
                                       TO   PTNYUINRRK-TENNYUYMD
      **                                      SPA-NAI-I01-IDOYMD
               END-IF
           END-IF
      *
      *    緩和ケア病棟入院料、認知症治療病棟入院料１、認知症治療病棟入院料２
      *    を算定時に退院日をＳＰＡ−異動日と転入日にセット
      *    以下の特定入院料も同様
      *    地域包括ケア病棟入院料１（平成２６年改正）
      *    地域包括ケア病棟入院料１（生活療養）（平成２６年改正）
      *    地域包括ケア病棟入院料２（平成２６年改正）
      *    地域包括ケア病棟入院料２（生活療養）（平成２６年改正）
      *    地域包括ケア病棟入院料１（特定地域）（平成２６年改正）
      *    地域包括ケア病棟入院料１（生活療養）（特定地域）（平成２６年改正）
      *    地域包括ケア病棟入院料２（特定地域）（平成２６年改正）
      *    地域包括ケア病棟入院料２（生活療養）（特定地域）（平成２６年改正）
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "2"
              IF    PTNYUINRRK-TOKUTEI-NYUIN  = "09" OR "05" OR "15"
                                             OR "41" OR "42" OR "43"
                                             OR "44" OR "45" OR "46"
                                             OR "47" OR "48"
                    MOVE   WRK-MAEIDOYMD
                                       TO   PTNYUINRRK-TENNYUYMD
      **                                      SPA-NAI-I01-IDOYMD
              END-IF
           END-IF
      *
           INITIALIZE      ORCSSYOKIKSNAREA
      *
      *    通算日数、算定開始日を求める
           MOVE    WRK-MAEIDOYMD  TO   PTNYUINRRK-TENNYUYMD
           PERFORM 4801-TSUSAN-SANTEI-GET-SEC
           IF    ( SPA-ERRCD    NOT =  SPACE )
               GO  TO  4801-IDOKAIKEI-CANCEL-EXT
           END-IF
      *
      *    介護病棟からの急性増悪による入院時
           IF      PTNYUINRRK-KAIGOKBN    =    "2"
               COMPUTE    WRK-TSUSAN   =   STUSAN-NISSU01  -  1
           END-IF
      *
           MOVE    WRK-TSUSAN      TO  SYOKIKSN-IN-TUSANNISSU
           MOVE    WRK-SANTEISTYMD TO  SYOKIKSN-IN-SANTEISTYMD
      *
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    PTNYUINRRK-BTUNUM
                                   TO  SYS-KBNCD
           MOVE    WRK-MAEIDOYMD   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5001-REC
      *
      *    特定集中治療室管理料算定時に特定入院での算定日が１４日を
      *    経過していた場合は一般の入院料算定とする
      *    以下の特定入院料も同様
      *    救命救急入院料１
      *    救命救急入院料２
      *    一類感染症患者入院医療管理料
      *    救命救急入院料３（救命救急入院料）
      *    救命救急入院料４（救命救急入院料）
      *    特定集中治療室管理料２（特定集中治療室管理料）
      *    小児特定集中治療室管理料
      *    特定集中治療室管理料１（平成２６年改正）
      *    特定集中治療室管理料２（特定集中治療室管理料）（平成２６年改正）
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "03" OR "04" OR "05" OR
                                                 "06" OR "07" OR "08" OR
                                                 "09" OR "14" OR "16" OR
                                                 "55" OR "57" OR "59" OR
                                                 "91" OR "AH" OR "AI"
                   IF   STUSAN-NISSU08   >=   14
                       MOVE    SPACE   TO   PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    地域包括ケア病棟入院料（平成２６年改正）の算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "2"
               IF   PTNYUINRRK-TOKUTEI-NYUIN  =  "41" OR "42" OR "43"
                                              OR "44" OR "45" OR "46"
                                              OR "47" OR "48"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    地域包括ケア入院医療管理料（平成２６年改正）の算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN  = "AL" OR "AM" OR "AN" OR
                                                "AO" OR "AP" OR "AQ" OR
                                                "AR" OR "AS"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    特定一般病棟入院料（地域包括ケア入院医療管理１）（平成２６年改正）
      *    特定一般病棟入院料（地域包括ケア入院医療管理２）（平成２６年改正）
      *    の算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN  = "AU" OR "AV"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    救命救急入院料３（広範囲熱傷特定集中治療）の算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "56"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    救命救急入院料４（広範囲熱傷特定集中治療）の算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "58"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    特定集中治療室管理料（広範囲熱傷特定集中治療）の算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
      *    以下の特定入院料も同様
      *    特定集中治療室管理料２（広範囲熱傷特定集中治療）（平成２６年改正）
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "60" OR "AT"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    広範囲熱傷特定集中治療室管理料算定時に特定入院での算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "13"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    ハイケアユニット入院医療管理料算定時に特定入院での算定日が２１日を
      *    経過していた場合は一般の入院料算定とする
      *    以下の特定入院料も同様
      *    ハイケアユニット入院医療管理料１（平成２６年改正）
      *    ハイケアユニット入院医療管理料２（平成２６年改正）
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "17" OR "AJ" OR "AK"
                   IF   STUSAN-NISSU08   >=   21
                       MOVE    SPACE   TO   PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *     DISPLAY "====AZU  TAIIN-CANCEL=================="
      *     DISPLAY "RRKNUM     = " PTNYUINRRK-RRKNUM
      *     DISPLAY "RRKEDANUM  = " PTNYUINRRK-RRKEDANUM
      *     DISPLAY "NYUINYMD   = " PTNYUINRRK-NYUINYMD
      *     DISPLAY "TAIINYMD   = " PTNYUINRRK-TAIINYMD
      *     DISPLAY "TENNYUYMD  = " PTNYUINRRK-TENNYUYMD
      *     DISPLAY "TENSTUYMD  = " PTNYUINRRK-TENSTUYMD
      *    平成２４年３月３１日時点で亜急性期入院医療管理料１を算定
      *    している状態か判定するサブを呼び出し
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "18"  OR  "54"
                   INITIALIZE                  ORCSAKYUSEI-AREA
                   MOVE    PTNYUINRRK-HOSPNUM  TO  LNK-AKYUSEI-HOSPNUM
                   MOVE    PTNYUINRRK-PTID     TO  LNK-AKYUSEI-PTID
                   MOVE    PTNYUINRRK-RRKNUM   TO  LNK-AKYUSEI-RRKNUM
                   CALL    "ORCSAKYUSEI"    USING  ORCSAKYUSEI-AREA
                                                   SPA-AREA
                   DISPLAY  "LNK-AKYUSEI-RC = " LNK-AKYUSEI-RC
               END-IF
           END-IF
      *
      *    亜急性期入院医療管理料１算定時に特定入院での算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
      *    （平成２４年３月３１日までの入院分は９０日限度）
           IF PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "18"
                  IF   PTNYUINRRK-NYUINYMD   >=   "20120401"
                      IF  STUSAN-NISSU08   >=   60
                          MOVE    SPACE   TO   PTNYUINRRK-TOKUTEI-NYUIN
                      END-IF
                  ELSE
                      IF  STUSAN-NISSU08   >=   90
                          MOVE    SPACE   TO   PTNYUINRRK-TOKUTEI-NYUIN
                      END-IF
                  END-IF
               END-IF
           END-IF
      *
      *    亜急性期入院医療管理料２算定時に特定入院での算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
      *    （経過措置により平成２４年３月３１日時点で亜急性期入院医療
      *      管理料１を算定している場合は９０日限度とする対応追加）
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
             IF   PTNYUINRRK-TOKUTEI-NYUIN   = "54"
               IF  PTNYUINRRK-NYUINYMD   >=   "20120401"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               ELSE
                   IF    LNK-AKYUSEI-RC   =   1
                      IF   STUSAN-NISSU08   >=   90
                          MOVE    SPACE   TO   PTNYUINRRK-TOKUTEI-NYUIN
                      END-IF
                   ELSE
                      IF   STUSAN-NISSU08   >=   60
                          MOVE    SPACE   TO   PTNYUINRRK-TOKUTEI-NYUIN
                      END-IF
                   END-IF
               END-IF
             END-IF
           END-IF
      *
      *    亜急性期入院医療管理料１（指定地域）算定時に特定入院での算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "92"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    亜急性期入院医療管理料２（指定地域）算定時に特定入院での算定日が６０日を
      *    経過していた場合は一般の入院料算定とする
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "93"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    脳卒中ケアユニット入院医療管理料算定時に特定入院での算定日が１４日を
      *    経過していた場合は一般の入院料算定とする
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "19"
                   IF   STUSAN-NISSU08   >=   14
                       MOVE    SPACE   TO   PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    特定一般病棟入院料（亜急性期入院医療管理）算定時に特定入院での
      *    算定日が６０日を経過していた場合は一般の入院料算定とする
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "95"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    特定一般病棟入院料（亜急性期入院医療管理・リハビリ算定患者）
      *    算定時に特定入院での算定日が６０日を経過していた場合は一般の
      *    入院料算定とする
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "96"
                   IF   STUSAN-NISSU08   >=   60
                       MOVE    SPACE   TO   PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    総合周産期特定集中治療室管理料（母体・胎児）算定時に特定入院での
      *    算定日が１４日を経過していた場合は一般の入院料算定とする
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "11"
                   IF   STUSAN-NISSU08   >=   14
                       MOVE    SPACE   TO   PTNYUINRRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    以下の特定入院料は出生児体重より算定上限日数を判定し、
      *    経過していた場合は一般の入院料算定とする
      *    ・新生児特定集中治療室管理料１
      *    ・新生児特定集中治療室管理料２
      *    ・総合周産期特定集中治療室管理料（新生児）
      *    ・新生児治療回復室入院医療管理料
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "10"  OR
                                                 "61"  OR
                                                 "12"  OR
                                                 "62"
      *          出生児体重の取得
                 MOVE   SPACE                TO  PTCONF-REC
                 INITIALIZE                      PTCONF-REC
                 MOVE   PTNYUINRRK-HOSPNUM   TO  PTCONF-HOSPNUM
                 MOVE   PTNYUINRRK-PTID      TO  PTCONF-PTID
                 MOVE   "BIRTHWEIGHT"        TO  PTCONF-CKEY
                 PERFORM 900-PTCONF-READ-SEC
                 IF     FLG-PTCONF           =   ZERO
                     MOVE   PTCONF-CDATA(1:4)  TO  WRK-WEIGHT
                 ELSE
                     MOVE   9999               TO  WRK-WEIGHT
                 END-IF
      *
      *          新生児特定集中治療室管理料１
      *          新生児特定集中治療室管理料２
                 IF   PTNYUINRRK-TOKUTEI-NYUIN   = "10"  OR  "61"
                    IF    WRK-WEIGHT     <   1000
                       IF   STUSAN-NISSU08   >=   90
                          MOVE    SPACE   TO   PTNYUINRRK-TOKUTEI-NYUIN
                       END-IF
                    END-IF
                    IF   (WRK-WEIGHT     >=  1000)   AND
                         (WRK-WEIGHT     <   1500)
                       IF   STUSAN-NISSU08   >=   60
                          MOVE    SPACE   TO   PTNYUINRRK-TOKUTEI-NYUIN
                       END-IF
                    END-IF
                    IF    WRK-WEIGHT     >=  1500
                       IF   STUSAN-NISSU08   >=   21
                          MOVE    SPACE   TO   PTNYUINRRK-TOKUTEI-NYUIN
                       END-IF
                    END-IF
                 END-IF
      *
      *          総合周産期特定集中治療室管理料（新生児）
                 IF   PTNYUINRRK-TOKUTEI-NYUIN   = "12"
                    IF    WRK-WEIGHT     <   1000
                       IF   STUSAN-NISSU08   >=   90
                          MOVE    SPACE   TO   PTNYUINRRK-TOKUTEI-NYUIN
                       END-IF
                    END-IF
                    IF   (WRK-WEIGHT     >=  1000)   AND
                         (WRK-WEIGHT     <   1500)
                       IF   STUSAN-NISSU08   >=   60
                          MOVE    SPACE   TO   PTNYUINRRK-TOKUTEI-NYUIN
                       END-IF
                    END-IF
                    IF    WRK-WEIGHT     >=  1500
                       IF   STUSAN-NISSU08   >=   21
                          MOVE    SPACE   TO   PTNYUINRRK-TOKUTEI-NYUIN
                       END-IF
                    END-IF
                 END-IF
      *
      *          新生児治療回復室入院医療管理料
                 IF   PTNYUINRRK-TOKUTEI-NYUIN   = "62"
                    IF    WRK-WEIGHT     <   1000
                       IF   STUSAN-NISSU08   >=   120
                          MOVE    SPACE   TO   PTNYUINRRK-TOKUTEI-NYUIN
                       END-IF
                    END-IF
                    IF   (WRK-WEIGHT     >=  1000)   AND
                         (WRK-WEIGHT     <   1500)
                       IF   STUSAN-NISSU08   >=   90
                          MOVE    SPACE   TO   PTNYUINRRK-TOKUTEI-NYUIN
                       END-IF
                    END-IF
                    IF    WRK-WEIGHT     >=  1500
                       IF   STUSAN-NISSU08   >=   30
                          MOVE    SPACE   TO   PTNYUINRRK-TOKUTEI-NYUIN
                       END-IF
                    END-IF
                 END-IF
      *
               END-IF
           END-IF
      *
           IF      PTNYUINRRK-TOKUTEI-NYUIN    NOT =   SPACE
               MOVE    PTNYUINRRK-TOKUTEI-KBN
                                      TO  LNK-PTNYUINRRK-TOKUTEI-KBN
               MOVE    PTNYUINRRK-TOKUTEI-NYUIN
                                      TO  LNK-PTNYUINRRK-TOKUTEI-NYUIN
               MOVE    PTNYUINRRK-BTUNUM
                                      TO  LNK-PTNYUINRRK-BTUNUM
               MOVE    PTNYUINRRK-TOKU-SANTEIYOUKENKBN
                               TO  LNK-PTNYUINRRK-TOKU-SANTEIYOUKENKBN
               PERFORM   4801-TOKTEI-NYUIN-SET-SEC
      *        特定一般病棟入院料の場合は期間加算が存在するため、
      *        初期加算サブを呼び出す（平成２４年改正で追加）
      *        療養病床で有床診療所入院基本料を相互算定する場合も
      *        初期加算サブを呼び出す（平成２４年改正で追加）
      *        特定入院料の算定要件外に算定にする以下入院料を追加（平成２６年改正で対応）
      *        ・一般病棟特別入院基本料
      *        ・精神病棟１５対１入院基本料
      *        ・精神病棟特別入院基本料
               IF    SYOKIKSN-IN-KHNSRYCD   =   "190151910"   OR
                                                "190152010"   OR
                                                "099999950"   OR
                                                "099999951"   OR
                                                "099999952"   OR
                                                "099999955"   OR
                                                "099999956"   OR
                                                "099999957"   OR
      *                                         特定入院料の算定要件外の算定
                                                "190079010"   OR
                                                "190083810"   OR
                                                "190085010"
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "4017"      TO  SPA-ERRCD
                       GO  TO  4801-IDOKAIKEI-CANCEL-EXT
                   END-IF
               END-IF
           ELSE
               IF    ( WRK-MAEIDOYMD     =  CONST-H180701 )
                AND  ( SYS-5001-BTU-SBT        =  "04"  OR "09" )
                   GO  TO  4801-IDOKAIKEI-CANCEL-EXT
               ELSE
                   IF  SYS-5001-BTU-KHNSRYCD      NOT =   SPACE
                       MOVE    SYS-5001-BTU-KHNSRYCD
                                       TO  SYOKIKSN-IN-KHNSRYCD
                       PERFORM   4801-RJNCD-CHG-SEC
                       PERFORM   4801-YAKINGEN-CHG-SEC
                       CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                       SPA-AREA
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
      *
      *    医療観察入院セット
           INITIALIZE            ORCSIRYOKNS-AREA
           MOVE     PTNYUINRRK-HOSPNUM   TO  LNK-IRYOKNS-HOSPNUM
           MOVE     PTNYUINRRK-TOKUTEI-KBN
                                         TO  LNK-IRYOKNS-KBN
           MOVE     PTNYUINRRK-TOKUTEI-NYUIN
                                         TO  LNK-IRYOKNS-NYUIN
           MOVE     PTNYUINRRK-BTUNUM    TO  LNK-IRYOKNS-BTUNUM
           DISPLAY  "==  ORCSIRYOKNS IDOKAIKEI-CANCEL-CALL ==="
           DISPLAY  "SYOKIKSN-IN-SANTEISTYMD = " SYOKIKSN-IN-SANTEISTYMD
      **     DISPLAY  "SPA-NAI-I01-NYUINYMD    = " SPA-NAI-I01-NYUINYMD
           MOVE     SYOKIKSN-IN-SANTEISTYMD(1:6)
                                         TO  LNK-IRYOKNS-SANTEIYM
           CALL     "ORCSIRYOKNS"   USING   ORCSIRYOKNS-AREA
                                            SPA-AREA
           DISPLAY   "LNK-IRYOKNS-RC = " LNK-IRYOKNS-RC
           IF       LNK-IRYOKNS-RC     =    ZERO
               MOVE   LNK-IRYOKNS-SRYCD(1)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(1)
               MOVE   LNK-IRYOKNS-SRYCD(2)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(2)
               MOVE   LNK-IRYOKNS-SRYCD(3)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(3)
               MOVE   LNK-IRYOKNS-SRYCD(4)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(4)
               MOVE   LNK-IRYOKNS-SRYCD(5)
                                        TO LNK-SCNYUACCT-NYUKHN-SRYCD(5)
               MOVE   SYOKIKSN-IN-SANTEISTYMD
                                  TO  LNK-SCNYUACCT-STSRYYMD
                                      LNK-SCNYUACCT-EDSRYYMD
           END-IF
      *
      *    通算算定済日数
           MOVE   WRK-TSUSAN    TO  LNK-SCNYUACCT-NYUKHN-NISUU
      *
      *    特定入院で入院料が変更になるコードの対応
      *
           DISPLAY "===  TEST IDOKAIKEI-CANCEL ==="
           INITIALIZE                       ORCSTOKNYUIN2-AREA
           MOVE   PTNYUINRRK-HOSPNUM    TO  LNK-TOKNYUIN2-HOSPNUM
           MOVE   PTNYUINRRK-PTID       TO  LNK-TOKNYUIN2-PTID
           MOVE   "1"                   TO  LNK-TOKNYUIN2-IDOKBN
           MOVE   PTNYUINRRK-NYUINYMD   TO  LNK-TOKNYUIN2-NYUINYMD
           MOVE   WRK-MAEIDOYMD         TO  LNK-TOKNYUIN2-SANTEIYMD
           MOVE   SYOKIKSN-IN-KHNSRYCD  TO  LNK-TOKNYUIN2-SET-SRYCD
           MOVE   STUSAN-NISSU08        TO  LNK-TOKNYUIN2-NISSU08
           MOVE   PTNYUINRRK-TOKUTEI-NYUIN
                             TO  LNK-TOKNYUIN2-PTNYUINRRK-TOKUTEI-NYUIN
           CALL  "ORCSTOKNYUIN2"     USING  ORCSTOKNYUIN2-AREA
                                            SPA-AREA
           IF     LNK-TOKNYUIN2-RC      =   ZERO
              MOVE   SPACE           TO  SYOKIKSN-IN-KHNSRYCD
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
              PERFORM   VARYING   IDX    FROM    1   BY  1
                UNTIL   IDX       >    5
                  IF   LNK-TOKNYUIN2-SRYCD(IDX)  NOT =   SPACE
                    MOVE  LNK-TOKNYUIN2-SRYCD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                    MOVE  LNK-TOKNYUIN2-STYMD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX)
                    MOVE  LNK-TOKNYUIN2-EDYMD(IDX)
                                  TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX)
                  END-IF
              END-PERFORM
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                          TO  IPN-NYUKAIKEI-SAKUSEI-FLG
              MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                                          TO  IPN-SANTEISTYMD
           ELSE
              IF   PTNYUINRRK-TOKUTEI-NYUIN  NOT =   SPACE
                  MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                        TO  IPN-NYUKAIKEI-SAKUSEI-FLG
                  MOVE    LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                                        TO  IPN-SANTEISTYMD
              END-IF
           END-IF
      *
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
      *        特定入院基本料、療養病棟入院基本料、療養病床入院基本料等は
      *        特定入院料扱いから外す
               IF    LNK-SCNYUACCT-NYUKHN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   SPA-HOSPNUM       TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
               IF    LNK-SCNYUACCT-KAGEN-SRYCD(1)  NOT =   SPACE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE   SPA-HOSPNUM       TO  TNS-HOSPNUM
                   MOVE   LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                            TO  TNS-SRYCD
                   MOVE    LNK-SCNYUACCT-STSRYYMD
                                            TO  TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
                   MOVE    TNS-TENSU-REC    TO  MCPDATA-REC
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF    ( MCP-RC           =   ZERO )
                       MOVE    MCPDATA-REC  TO  TNS-TENSU-REC
                       IF      TNS-NYUTENTTLKBN   =   903
                           MOVE   "0"       TO
                                        LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"      TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
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
           IF     PTNYUINRRK-SENTEIKBN  =   "1"
               PERFORM 4801-SENTEI-SET-SEC
           END-IF
      *    入院カレンダー未設定の判定
           IF     PTNYUINRRK-NYUCALKBN   =   "1"
                  MOVE   1      TO  LNK-SCNYUACCT-NYUCAL-KBN
           ELSE
                  MOVE   ZERO   TO  LNK-SCNYUACCT-NYUCAL-KBN
           END-IF
      *
      *    有床診療所入院基本料の相互算定コード置き換え
           IF    (LNK-SCNYUACCT-STSRYYMD   >=   "20120401")  AND
                 (LNK-SCNYUACCT-STSRYYMD   <=   "20140331")
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    有床診療所入院基本料の相互算定コード置き換え（平成２６年改定）
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20140401"
               IF  (SYS-5001-BTU-SBT       =    "09")  AND
                   (SYS-5001-CRYOYOKBN     =    "1" OR "2" OR "3" OR
                                                "4" OR "5" OR "6")
                  PERFORM    VARYING   IDX  FROM   1  BY  1
                    UNTIL (LNK-SCNYUACCT-KAGEN-SRYCD(IDX)  =  SPACE) OR
                          (IDX           >   10)
                      EVALUATE   LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169610"
                             MOVE "190170810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169710"
                             MOVE "190170910" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169810"
                             MOVE "190171010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190169910"
                             MOVE "190171110" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170010"
                             MOVE "190171210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170110"
                             MOVE "190171310" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170210"
                             MOVE "190171410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170310"
                             MOVE "190171510" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190170410"
                             MOVE "190171610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097010"
                             MOVE "190153410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119510"
                             MOVE "190153610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119610"
                             MOVE "190153810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190097210"
                             MOVE "190154010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119710"
                             MOVE "190154210" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190119810"
                             MOVE "190154410" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135110"
                             MOVE "190154610" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135210"
                             MOVE "190154810" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                          WHEN    "190135310"
                             MOVE "190155010" TO
                                        LNK-SCNYUACCT-KAGEN-SRYCD(IDX)
                      END-EVALUATE
                  END-PERFORM
               END-IF
           END-IF
      *
      *    特定入院基本料のコード細分化置き換え
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20120401"
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799410"
                   EVALUATE   SYS-5001-BTU-SBT
      *                専門病院
                       WHEN    "06"
                             MOVE "190161010" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                障害者施設等
                       WHEN    "05"
                             MOVE "190161210" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
      *
      *            特定機能病院は病棟設定された入院コードから判定
                   IF   SYS-5001-BTU-KHNSRYCD =  "190118610" OR
                                                 "190705610" OR
                                                 "190085910"
                     MOVE "190160810" TO  LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-IF
               END-IF
      *        特別入院基本料
               IF  LNK-SCNYUACCT-NYUKHN-SRYCD(1)  =  "190799510"
                   EVALUATE   SYS-5001-BTU-SBT
      *                専門病院
                       WHEN    "06"
                             MOVE "190161110" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *                障害者施設等
                       WHEN    "05"
                             MOVE "190161310" TO
                                        LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                   END-EVALUATE
               END-IF
           END-IF
      *
           MOVE   PTNYUINRRK-BTUNUM   TO  LNK-SCNYUACCT-BTU-NUM
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
               DISPLAY  "ORCGI01 ORCSNYUACCT ERR RC = "
                         LNK-SCNYUACCT-RC
           END-IF
      *
      *    入院会計作成後に退避データの食事区分を復元
      *   （異動月分）
           IF  (WRK-TENROOM-SKJ-SRYYM(1) >= "200604")   AND
               (WRK-TENROOM-SKJ-SRYYM(1)  NOT =   SPACE)
              PERFORM    VARYING  IDX-SKJ-TAIHI  FROM  1  BY  1
                 UNTIL    IDX-SKJ-TAIHI   >   3
                 INITIALIZE                      NYUINACCT-REC
                 MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
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
                     EVALUATE  IDX-SKJ-TAIHI
                         WHEN  1
                          PERFORM VARYING IDX-SKJ FROM  1  BY 1
                            UNTIL     IDX-SKJ   >   31
                               MOVE   WRK-TENROOM-SKJ18(1 IDX-SKJ)
                                             TO  NACCT-DAY(IDX-SKJ)
                          END-PERFORM
                         WHEN  2
                          PERFORM VARYING IDX-SKJ FROM  1  BY 1
                            UNTIL     IDX-SKJ   >   31
                               MOVE   WRK-TENROOM-SKJ19(1 IDX-SKJ)
                                             TO  NACCT-DAY(IDX-SKJ)
                          END-PERFORM
                         WHEN  3
                          PERFORM VARYING IDX-SKJ FROM  1  BY 1
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
                 MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
                 MOVE     "key"              TO  MCP-PATHNAME
                 PERFORM 910-DBUPDATE-SEC
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
                 PERFORM 910-DBCLOSECURSOR-SEC
              END-PERFORM
           END-IF
      *   （異動翌月分）
           IF  (WRK-TENROOM-SKJ-SRYYM(2) >= "200604")   AND
               (WRK-TENROOM-SKJ-SRYYM(2)  NOT =   SPACE)
              PERFORM    VARYING  IDX-SKJ-TAIHI  FROM  1  BY  1
                 UNTIL    IDX-SKJ-TAIHI   >   3
                   INITIALIZE                      NYUINACCT-REC
                   MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
                   MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
                   MOVE    WRK-TENROOM-SKJ-SRYYM(2)
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
                   MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
                   MOVE     "key"              TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
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
                   PERFORM 910-DBCLOSECURSOR-SEC
              END-PERFORM
           END-IF
      *   （異動月分）
           IF  (WRK-TENROOM-SKJ-SRYYM(1) < "200603")   AND
               (WRK-TENROOM-SKJ-SRYYM(1)  NOT =   SPACE)
               INITIALIZE                      NYUINACCT-REC
               MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    WRK-TENROOM-SKJ-SRYYM(1)
                                           TO  NACCT-SRYYM
      *
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM   910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *            食事を復元
                   MOVE    WRK-TENROOM-SKJ-DD(1)
                                       TO  IDX-SHOKUJIX
                   PERFORM  VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                            UNTIL     IDX-SKJ   >   31
                       MOVE   WRK-TENROOM-SKJ(1 IDX-SKJ)
                                           TO  NACCT-DAY(IDX-SKJ)
                   END-PERFORM
               END-IF
               MOVE     NYUINACCT-REC      TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *   （異動翌月分）
           IF  (WRK-TENROOM-SKJ-SRYYM(2) < "200603")   AND
               (WRK-TENROOM-SKJ-SRYYM(2)  NOT =   SPACE)
               INITIALIZE                      NYUINACCT-REC
               MOVE    PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    WRK-TENROOM-SKJ-SRYYM(2)
                                           TO  NACCT-SRYYM
      *
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM   910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *            食事を復元
                   PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                             UNTIL     IDX-SKJ   >   31
                        MOVE   WRK-TENROOM-SKJ(2 IDX-SKJ)
                                           TO  NACCT-DAY(IDX-SKJ)
                   END-PERFORM
               END-IF
               MOVE     NYUINACCT-REC      TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
      *    入院会計作成後に退避データの外泊区分を復元
      *    （外泊異動月分）
           INITIALIZE                      NYUINACCT-REC
           MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    WRK-TENROOM-SKJ-SRYYM(1)
                                       TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM   910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *        外泊を復元
               MOVE    WRK-TENROOM-SKJ-DD(1)
                                       TO  IDX-SHOKUJIX
               PERFORM   VARYING   IDX-SKJ FROM  IDX-SHOKUJI9  BY  1
                         UNTIL     IDX-SKJ   >   31
                    MOVE   WRK-TENROOM-GAIHAKU(1 IDX-SKJ)
                                       TO  NACCT-DAY(IDX-SKJ)
               END-PERFORM
           END-IF
           MOVE     NYUINACCT-REC      TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *    （異動翌月分）
           IF   WRK-TENROOM-SKJ-SRYYM(2)  NOT =   SPACE
               INITIALIZE                      NYUINACCT-REC
               MOVE    PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    WRK-TENROOM-SKJ-SRYYM(2)
                                           TO  NACCT-SRYYM
      *
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key26"             TO  MCP-PATHNAME
               PERFORM   910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE     MCPDATA-REC    TO  NYUINACCT-REC
      *            外泊を復元
                   PERFORM   VARYING   IDX-SKJ FROM  1  BY  1
                             UNTIL     IDX-SKJ   >   31
                        MOVE   WRK-TENROOM-GAIHAKU(2 IDX-SKJ)
                                           TO  NACCT-DAY(IDX-SKJ)
                   END-PERFORM
               END-IF
               MOVE     NYUINACCT-REC      TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key26"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
      *     DISPLAY "=============AZU3========================"
      *     DISPLAY "IPN-NYUKAIKEI-SAKUSEI-FLG = "
      *              IPN-NYUKAIKEI-SAKUSEI-FLG
      *     DISPLAY "IPN-SANTEISTYMD = " IPN-SANTEISTYMD
           IF     IPN-NYUKAIKEI-SAKUSEI-FLG    =   "1"
      **         MOVE    SPACE   TO   SPA-GMN-I01-TOKNYUIN-CD
               MOVE    SPACE            TO  LNK-PTNYUINRRK-TOKUTEI-NYUIN
               MOVE    IPN-SANTEISTYMD  TO  WRK-SANTEISTYMD
      **        MOVE    IPN-SANTEISTYMD  TO  SPA-NAI-I01-IDOYMD
               MOVE    IPN-SANTEISTYMD  TO  LNK-PTNYUINRRK-TENNYUYMD
               PERFORM   4801-TENROOM-EDIT-SEC
           END-IF
      *
           .
       4801-IDOKAIKEI-CANCEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院取消　会計テーブル編集処理
      *****************************************************************
       4801-NYUKAIKEI-ERASE-SEC         SECTION.
      *
           DISPLAY "4801-NYUKAIKEI-ERASE-SEC"
           DISPLAY "PTNYUINRRK-PTID      = " LNK-PTNYUINRRK-PTID
           DISPLAY "PTNYUINRRK-NYUINYMD  = " LNK-PTNYUINRRK-NYUINYMD
           DISPLAY "PTNYUINRRK-TAIINYMD  = " LNK-PTNYUINRRK-TAIINYMD
           DISPLAY "PTNYUINRRK-HKNCOMBINUM  = "
                                         LNK-PTNYUINRRK-HKNCOMBINUM
      *    入院会計作成サブの呼出し
           INITIALIZE            ORCSNYUACCT-AREA
           EVALUATE   SODRNACCT-ORDERID
               WHEN   "0107"
                      MOVE   "5"       TO   LNK-SCNYUACCT-SYORI-KBN
               WHEN   "0108"
                      MOVE   "6"       TO   LNK-SCNYUACCT-SYORI-KBN
           END-EVALUATE
           DISPLAY  "LNK-SCNYUACCT-SYORI-KBN = " 
                     LNK-SCNYUACCT-SYORI-KBN
           MOVE     LNK-PTNYUINRRK-HOSPNUM  TO  SPA-HOSPNUM
           MOVE     "1"                    TO  SPA-NYUGAIKBN
           MOVE     LNK-PTNYUINRRK-PTID    TO  SPA-PTID
           MOVE     LNK-PTNYUINRRK-NYUINYMD
                                       TO   LNK-SCNYUACCT-STSRYYMD
      *    保険組合せ
           MOVE   LNK-PTNYUINRRK-HKNCOMBINUM
                                        TO  WRK-HKNCOMBINUM
           MOVE   WRK-HKNCOMBINUM9      TO  LNK-SCNYUACCT-HKNCOMBINUM
           MOVE   SPA-SRYKA     TO  WRK-TAIHI-SRYKA
           MOVE   "00"          TO  SPA-SRYKA
           MOVE   LNK-PTNYUINRRK-BTUNUM TO  LNK-SCNYUACCT-BTU-NUM
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
                  DISPLAY  "ORCSNYUACCT ERR RC = "
                            LNK-SCNYUACCT-RC
           END-IF
      *
           .
       4801-NYUKAIKEI-ERASE-EXT.
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
           MOVE     LNK-PTNYUINRRK-HOSPNUM  TO  SPA-HOSPNUM
           MOVE     LNK-PTNYUINRRK-PTID    TO  SPA-PTID
           MOVE     LNK-PTNYUINRRK-NYUINKA TO  SPA-SRYKA
           MOVE     LNK-PTNYUINRRK-TAIINYMD
                                       TO   LNK-SCNYUACCT-EDSRYYMD
      *    保険組合せ
           MOVE   LNK-PTNYUINRRK-HKNCOMBINUM
                                       TO  WRK-HKNCOMBINUM
           MOVE   WRK-HKNCOMBINUM9     TO  LNK-SCNYUACCT-HKNCOMBINUM
      *     MOVE   SPA-SRYKA           TO  WRK-TAIHI-SRYKA
           MOVE   "00"                 TO  SPA-SRYKA
           MOVE   LNK-PTNYUINRRK-BTUNUM
                                       TO  LNK-SCNYUACCT-BTU-NUM
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
      *     MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
                  DISPLAY  "ORCGI01 ORCSNYUACCT ERR RC = "
                            LNK-SCNYUACCT-RC
           END-IF
      *
           .
       4801-TAIINKAIKEI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    外泊　会計テーブル編集処理
      *****************************************************************
       4801-GAIHAKU-SEC         SECTION.
      *
      *    外泊のセット
           IF      LNK-PTNYUINRRK-TAIINYMD =   "99999999"
               PERFORM    4801-GAIHAKU-1-SEC
           ELSE
               PERFORM    4801-GAIHAKU-2-SEC
           END-IF
      *
           .
       4801-GAIHAKU-EXT.
           EXIT.
      *****************************************************************
      *    外泊　会計テーブル編集処理（入院中）
      *****************************************************************
       4801-GAIHAKU-1-SEC         SECTION.
      *
      *    入院中患者の外泊データ更新なので、入院会計が存在する会計データに
      *    ついて全て更新を行う
      *
      *    外泊のセット
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    SODRNACCT-STARTYMD(1:6) TO  NACCT-SRYYM
           MOVE    "2"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               MOVE    SODRNACCT-STARTYMD  TO  WRK-STARTYMD
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
      *            開始日のセット
                   IF      NACCT-SRYYM    =   SODRNACCT-STARTYMD(1:6)
                       MOVE   WRK-STARTYMD-D   TO   START-IDX
                   ELSE
                       MOVE   1                TO   START-IDX
                   END-IF
      *            月末日を算定
                   INITIALIZE                STS-AREA-DAY
                   INITIALIZE                LNK-DAY7-AREA
                   MOVE    "71"              TO  LNK-DAY7-IRAI
                   MOVE   NACCT-SRYYM        TO  LNK-DAY7-YM
                   CALL  "ORCSDAY"           USING STS-AREA-DAY
                                                   LNK-DAY7-AREA
                   MOVE   LNK-DAY7-KOYOMI    TO    WRK-GETSUMATSU-YMD
      *            外泊区分のセット
                   PERFORM  VARYING IDX  FROM  START-IDX   BY 1
                                UNTIL   IDX  >    WRK-GETSUMATSU-D
                       EVALUATE   SODRNACCT-ORDERID(3:2)
                           WHEN   "01"
                             MOVE   1    TO   NACCT-DAY(IDX)
                           WHEN   "02"
                             MOVE   2    TO   NACCT-DAY(IDX)
                           WHEN   "03"
                             MOVE   3    TO   NACCT-DAY(IDX)
                           WHEN   "04"
                             MOVE   4    TO   NACCT-DAY(IDX)
                           WHEN   "05"
                             MOVE   0    TO   NACCT-DAY(IDX)
                       END-EVALUATE
                   END-PERFORM
                   MOVE    NYUINACCT-REC   TO   MCPDATA-REC
                   MOVE    "DBUPDATE"      TO   MCP-FUNC
                   MOVE    "tbl_nyuinacct" TO   MCP-TABLE
                   MOVE    "key"           TO   MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *            入院会計クローズ
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key17"             TO  MCP-PATHNAME
                   PERFORM   910-DBCLOSECURSOR-SEC
      *            次月の会計読み込み
                   MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
                   MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
                   IF      WRK-STARTYMD-M  =   12
                      ADD     1      TO    WRK-STARTYMD-Y
                      MOVE    1      TO    WRK-STARTYMD-M
                   ELSE
                      ADD     1      TO    WRK-STARTYMD-M
                   END-IF
                   MOVE    "2"                     TO  NACCT-ZAISKBKBN
                   MOVE    WRK-STARTYMD-YM         TO  NACCT-SRYYM
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-SELECT-SEC
                   IF      FLG-NYUINACCT    =   ZERO
                       MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                       MOVE    "key17"                 TO  MCP-PATHNAME
                       PERFORM 910-NYUINACCT-READ-SEC
                   END-IF
               END-PERFORM
           END-IF
      *    入院会計クローズ
           MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE     "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DBCLOSECURSOR-SEC
      *
           .
       4801-GAIHAKU-1-EXT.
           EXIT.
      *****************************************************************
      *    外泊　会計テーブル編集処理（退院済）
      *****************************************************************
       4801-GAIHAKU-2-SEC         SECTION.
      *
      *    退院済患者の外泊データ更新なので、退院日までの会計データについて
      *    更新を行う
      *
      *    外泊のセット
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    SODRNACCT-STARTYMD(1:6) TO  NACCT-SRYYM
           MOVE    "2"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               MOVE    SODRNACCT-STARTYMD  TO  WRK-STARTYMD
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)  OR
                      (NACCT-SRYYM    > LNK-PTNYUINRRK-TAIINYMD(1:6))
      *            開始日のセット
                   IF      NACCT-SRYYM      =  SODRNACCT-STARTYMD(1:6)
                       MOVE   WRK-STARTYMD-D   TO   START-IDX
                   ELSE
                       MOVE   1                TO   START-IDX
                   END-IF
      *            月末日を算定
                   INITIALIZE                STS-AREA-DAY
                   INITIALIZE                LNK-DAY7-AREA
                   MOVE    "71"              TO  LNK-DAY7-IRAI
                   MOVE   NACCT-SRYYM        TO  LNK-DAY7-YM
                   CALL  "ORCSDAY"           USING STS-AREA-DAY
                                                   LNK-DAY7-AREA
                   MOVE   LNK-DAY7-KOYOMI    TO    WRK-GETSUMATSU-YMD
      *            退院月については退院日をセット
                   IF   NACCT-SRYYM    =  LNK-PTNYUINRRK-TAIINYMD(1:6)
                       MOVE    LNK-PTNYUINRRK-TAIINYMD
                                             TO    WRK-GETSUMATSU-YMD
                   END-IF
      *            外泊区分のセット
                   PERFORM  VARYING IDX  FROM  START-IDX   BY 1
                                UNTIL   IDX  >    WRK-GETSUMATSU-D
                       EVALUATE   SODRNACCT-ORDERID(3:2)
                           WHEN   "01"
                             MOVE   1    TO   NACCT-DAY(IDX)
                           WHEN   "02"
                             MOVE   2    TO   NACCT-DAY(IDX)
                           WHEN   "03"
                             MOVE   3    TO   NACCT-DAY(IDX)
                           WHEN   "04"
                             MOVE   4    TO   NACCT-DAY(IDX)
                           WHEN   "05"
                             MOVE   0    TO   NACCT-DAY(IDX)
                       END-EVALUATE
                   END-PERFORM
                   MOVE    NYUINACCT-REC   TO   MCPDATA-REC
                   MOVE    "DBUPDATE"      TO   MCP-FUNC
                   MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *            入院会計クローズ
                   MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE     "key17"                 TO  MCP-PATHNAME
                   PERFORM   910-DBCLOSECURSOR-SEC
      *            次月の会計読み込み
                   MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
                   MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
                   IF      WRK-STARTYMD-M  =   12
                      ADD     1      TO    WRK-STARTYMD-Y
                      MOVE    1      TO    WRK-STARTYMD-M
                   ELSE
                      ADD     1      TO    WRK-STARTYMD-M
                   END-IF
                   MOVE    "2"                     TO  NACCT-ZAISKBKBN
                   MOVE    WRK-STARTYMD-YM         TO  NACCT-SRYYM
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-SELECT-SEC
                   IF      FLG-NYUINACCT    =   ZERO
                       MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                       MOVE    "key17"                 TO  MCP-PATHNAME
                       PERFORM 910-NYUINACCT-READ-SEC
                   END-IF
               END-PERFORM
           END-IF
      *    入院会計クローズ
           MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE     "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DBCLOSECURSOR-SEC
      *
           .
       4801-GAIHAKU-2-EXT.
           EXIT.
      *
      *****************************************************************
      *    食事　会計テーブル編集処理（平成１８年３月以前）
      *****************************************************************
       4801-SHOKUJI-SEC         SECTION.
      *
      *    食事のセット
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    SODRNACCT-STARTYMD(1:6) TO  NACCT-SRYYM
           MOVE    "4"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key28"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key28"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               IF        FLG-NYUINACCT     =   ZERO
                 IF   NACCT-SRYCDTOTAL  =  99999913
      *            食事区分のセット
                   MOVE    SODRNACCT-STARTYMD      TO  WRK-STARTYMD
                   IF   SODRNACCT-SKJTIME   =  "01"
                     EVALUATE   SODRNACCT-ORDERID(3:2)
                       WHEN   "01"
                           MOVE  1  TO   NACCT-DAY(WRK-STARTYMD-D)
                       WHEN   "02"
                           MOVE  2  TO   NACCT-DAY(WRK-STARTYMD-D)
                       WHEN   "03"
                           MOVE  3  TO   NACCT-DAY(WRK-STARTYMD-D)
                       WHEN   "04"
                           MOVE   4     TO   NACCT-DAY(WRK-STARTYMD-D)
                       WHEN   "05"
                           MOVE  0  TO   NACCT-DAY(WRK-STARTYMD-D)
                     END-EVALUATE
                   ELSE
                     EVALUATE   SODRNACCT-ORDERID(3:2)
                       WHEN   "01"
                           IF     NACCT-DAY(WRK-STARTYMD-D)  =  0
                               MOVE  1  TO   NACCT-DAY(WRK-STARTYMD-D)
                           END-IF
                       WHEN   "02"
                           IF     NACCT-DAY(WRK-STARTYMD-D) =  0    OR
                                                               1    OR
                                                               3
                               MOVE  2  TO   NACCT-DAY(WRK-STARTYMD-D)
                           END-IF
                       WHEN   "03"
                           IF     NACCT-DAY(WRK-STARTYMD-D) =  0  OR 1
                               MOVE  3  TO   NACCT-DAY(WRK-STARTYMD-D)
                           END-IF
                       WHEN   "04"
                           MOVE   4     TO   NACCT-DAY(WRK-STARTYMD-D)
                     END-EVALUATE
                   END-IF
      *            剤回数再セット
                   MOVE  ZERO           TO  NACCT-ZAIKAISU
                   PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                            UNTIL     IDX-SKJ   >   31
                       IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                           ADD   1        TO  NACCT-ZAIKAISU
                       END-IF
                   END-PERFORM   
      *            食堂加算の編集用に退避
                   MOVE    NACCT-DAY(WRK-STARTYMD-D)  TO WRK-SHOKUDOU
      *
                   MOVE    NYUINACCT-REC   TO   MCPDATA-REC
                   MOVE    "DBUPDATE"      TO   MCP-FUNC
                   MOVE    "tbl_nyuinacct" TO   MCP-TABLE
                   MOVE    "key"           TO   MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
                 END-IF
               END-IF
           END-IF
      *    入院会計クローズ
           MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE     "key28"                 TO  MCP-PATHNAME
           PERFORM   910-DBCLOSECURSOR-SEC
      *
      *-------------------食堂加算ここから----------------------------
      *    食事のセット
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    SODRNACCT-STARTYMD(1:6) TO  NACCT-SRYYM
           MOVE    "4"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                 IF   NACCT-SRYCDTOTAL  =  197000570
      *            食堂加算のセット
                   MOVE    SODRNACCT-STARTYMD      TO  WRK-STARTYMD
                   IF    WRK-SHOKUDOU    NOT =  ZERO
                         MOVE   1   TO   NACCT-DAY(WRK-STARTYMD-D)
                   ELSE
                         MOVE   0   TO   NACCT-DAY(WRK-STARTYMD-D)
                   END-IF
      *            剤回数再セット
                   MOVE  ZERO           TO  NACCT-ZAIKAISU
                   PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                            UNTIL     IDX-SKJ   >   31
                       IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                           ADD   1        TO  NACCT-ZAIKAISU
                       END-IF
                   END-PERFORM   
      *
                   MOVE    NYUINACCT-REC   TO   MCPDATA-REC
                   MOVE    "DBUPDATE"      TO   MCP-FUNC
                   MOVE    "tbl_nyuinacct" TO   MCP-TABLE
                   MOVE    "key"           TO   MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
                 END-IF
                 MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                 MOVE    "key17"             TO  MCP-PATHNAME
                 PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    入院会計クローズ
           MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE     "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DBCLOSECURSOR-SEC
      *-------------------食堂加算ここまで----------------------------
           .
       4801-SHOKUJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    食事　会計テーブル編集処理（平成１８年４月以降）
      *****************************************************************
       4801-SHOKUJI-200604-SEC         SECTION.
      *
      *    食事のセット
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    SODRNACCT-STARTYMD(1:6) TO  NACCT-SRYYM
           MOVE    "4"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           EVALUATE   SODRNACCT-SKJTIME
              WHEN    "01"
                  MOVE    "key30"          TO  MCP-PATHNAME
              WHEN    "02"
                  MOVE    "key31"          TO  MCP-PATHNAME
              WHEN    "03"
                  MOVE    "key32"          TO  MCP-PATHNAME
           END-EVALUATE
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               EVALUATE   SODRNACCT-SKJTIME
                   WHEN    "01"
                       MOVE   "key30"      TO  MCP-PATHNAME
                   WHEN    "02"
                       MOVE   "key31"      TO  MCP-PATHNAME
                   WHEN    "03"
                       MOVE   "key32"      TO  MCP-PATHNAME
               END-EVALUATE
               PERFORM   910-NYUINACCT-READ-SEC
               IF        FLG-NYUINACCT     =   ZERO
      *            食事区分のセット
      *             MOVE    SODRNACCT-STARTYMD      TO  WRK-STARTYMD
      *             EVALUATE   SODRNACCT-ORDERID(3:2)
      *                 WHEN   "01"
      *                     MOVE   1     TO   NACCT-DAY(WRK-STARTYMD-D)
      *                 WHEN   "02"
      *                     MOVE   2     TO   NACCT-DAY(WRK-STARTYMD-D)
      *                 WHEN   "05"
      *                     MOVE   0     TO   NACCT-DAY(WRK-STARTYMD-D)
      *             END-EVALUATE
                   PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                            UNTIL     IDX-SKJ   >   31
                       IF   SODRNACCT-DAY(IDX-SKJ)   =   1
                           EVALUATE   SODRNACCT-ORDERID(3:2)
                               WHEN   "01"
                                   MOVE   1     TO   NACCT-DAY(IDX-SKJ)
                               WHEN   "02"
                                   MOVE   2     TO   NACCT-DAY(IDX-SKJ)
                               WHEN   "05"
                                   MOVE   0     TO   NACCT-DAY(IDX-SKJ)
                           END-EVALUATE
                       END-IF
                   END-PERFORM
      *            剤回数再セット
                   MOVE  ZERO           TO  NACCT-ZAIKAISU
                   PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
                            UNTIL     IDX-SKJ   >   31
                       IF   NACCT-DAY(IDX-SKJ)   NOT =  0
                           ADD   1        TO  NACCT-ZAIKAISU
                       END-IF
                   END-PERFORM   
      *            食堂加算の編集用に退避
                   MOVE    NACCT-DAY(WRK-STARTYMD-D)  TO WRK-SHOKUDOU
      *
                   MOVE    NYUINACCT-REC   TO   MCPDATA-REC
                   MOVE    "DBUPDATE"      TO   MCP-FUNC
                   MOVE    "tbl_nyuinacct" TO   MCP-TABLE
                   MOVE    "key"           TO   MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
               END-IF
           END-IF
      *    入院会計クローズ
           MOVE     "tbl_nyuinacct"        TO  MCP-TABLE
           EVALUATE   SODRNACCT-SKJTIME
              WHEN    "01"
                  MOVE    "key30"          TO  MCP-PATHNAME
              WHEN    "02"
                  MOVE    "key31"          TO  MCP-PATHNAME
              WHEN    "03"
                  MOVE    "key32"          TO  MCP-PATHNAME
           END-EVALUATE
           PERFORM   910-DBCLOSECURSOR-SEC
      *
      *-------------------食堂加算ここから----------------------------
      *    食事のセット
      *     INITIALIZE                          NYUINACCT-REC
      *     MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  NACCT-HOSPNUM
      *     MOVE    LNK-PTNYUINRRK-PTID     TO  NACCT-PTID
      *     MOVE    SODRNACCT-STARTYMD(1:6) TO  NACCT-SRYYM
      *     MOVE    "4"                     TO  NACCT-ZAISKBKBN
      *     MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
      *     MOVE    "key17"                 TO  MCP-PATHNAME
      *     PERFORM 910-NYUINACCT-SELECT-SEC
      *     IF      FLG-NYUINACCT           =   ZERO
      *         MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
      *         MOVE     "key17"            TO  MCP-PATHNAME
      *         PERFORM   910-NYUINACCT-READ-SEC
      *         PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
      *           IF   NACCT-SRYCDTOTAL  =  197000570
      *            食堂加算のセット
      *             MOVE    SODRNACCT-STARTYMD      TO  WRK-STARTYMD
      *             IF    WRK-SHOKUDOU    NOT =  ZERO
      *                   MOVE   1   TO   NACCT-DAY(WRK-STARTYMD-D)
      *             ELSE
      *                   MOVE   0   TO   NACCT-DAY(WRK-STARTYMD-D)
      *             END-IF
      *            剤回数再セット
      *             MOVE  ZERO           TO  NACCT-ZAIKAISU
      *             PERFORM  VARYING   IDX-SKJ FROM  1  BY  1
      *                      UNTIL     IDX-SKJ   >   31
      *                 IF   NACCT-DAY(IDX-SKJ)   NOT =  0
      *                     ADD   1        TO  NACCT-ZAIKAISU
      *                 END-IF
      *             END-PERFORM   
      **
      *             MOVE    NYUINACCT-REC   TO   MCPDATA-REC
      *             MOVE    "DBUPDATE"      TO   MCP-FUNC
      *             MOVE    "tbl_nyuinacct" TO   MCP-TABLE
      *             MOVE    "key"           TO   MCP-PATHNAME
      *             CALL    "ORCDBMAIN"         USING
      *                                         MCPAREA
      *                                         MCPDATA-REC
      *                                         SPA-AREA
      *
      *           END-IF
      *           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
      *           MOVE    "key17"             TO  MCP-PATHNAME
      *           PERFORM 910-NYUINACCT-READ-SEC
      *         END-PERFORM
      *     END-IF
      *    入院会計クローズ
      *     MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
      *     MOVE     "key17"                 TO  MCP-PATHNAME
      *     PERFORM   910-DBCLOSECURSOR-SEC
      *-------------------食堂加算ここまで----------------------------
           .
       4801-SHOKUJI-200604-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為照会テーブル編集サブ呼び出し
      *****************************************************************
       4900-ORCSS010-SEC         SECTION.
      *
           MOVE    NRRK-TENNYUYMD  TO  WRK-STYMD
           MOVE    WRK-LASTSRYYM   TO  WRK-EDYMD
      *
           PERFORM 900-NYUINACCT-KEY19-SEL-SEC
           IF    ( WRK-EDYMD       =   SPACE )
            OR   ( NACCT-SRYYM >=  WRK-EDYMD (1:6) )
               MOVE    NACCT-SRYYM TO  WRK-EDYMD
           END-IF
      *
           IF    ( WRK-EDYMD       =   SPACE )
               DISPLAY "SELECT FAILED [NYUINACCT] PTID:"
                        LNK-PTNYUINRRK-PTID
               MOVE    1               TO  FLG-END
               MOVE    "tbl_nyuinacct" TO  MCP-TABLE
               MOVE    "abort"         TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
           END-IF
      *
           PERFORM UNTIL ( WRK-STYMD (1:6) >   WRK-EDYMD (1:6))
                    OR   ( FLG-END     NOT =   ZERO )
      *
               INITIALIZE                      SS010-AREA
               MOVE    LNK-PTNYUINRRK-PTID TO  SS010-PTID
               MOVE    WRK-STYMD (1:6)     TO  SS010-SRYYM
               CALL    "ORCSS010"      USING   SS010-AREA
                                               SPA-AREA
               IF    ( SS010-RC        NOT =   ZERO )
                   MOVE    1           TO  FLG-END
               END-IF
      *
               MOVE    WRK-STYMD       TO  WRK-YMD
               MOVE    "2"             TO  WRK-ZOGENPTN
               MOVE     1              TO  WRK-ZOGEN
               PERFORM 5002-CALENDAR-SEC
               MOVE    WRK-KEISANYMD   TO  WRK-STYMD
      *
           END-PERFORM
      *
           .
      *
       4900-ORCSS010-EXT.
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
      *
      *****************************************************************
      *    患者情報検索処理(KEY)
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE                      PTINF-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM   TO  PTINF-HOSPNUM
           MOVE    LNK-PTNYUINRRK-PTID     TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF     MCP-RC    =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    LNK-PTNYUINRRK-HOSPNUM  TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF     MCP-RC    =   ZERO
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC              SECTION.
      *
           PERFORM 910-DB-SELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DB-SELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター検索
      *****************************************************************
       910-NYUINACCT-SELECT-SEC        SECTION.
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター読込
      *****************************************************************
       910-NYUINACCT-READ-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター検索
      *****************************************************************
       900-NYUINACCT-KEY19-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           INITIALIZE                      NYUINACCT-REC
           MOVE    LNK-PTNYUINRRK-HOSPNUM
                                       TO  NACCT-HOSPNUM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    LNK-PTNYUINRRK-PTID TO  NACCT-PTID
           MOVE    "999999"            TO  NACCT-SRYYM
           MOVE    "5"                 TO  NACCT-ZAISKBKBN
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-NYUINACCT-KEY19-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者個別設定テーブル読込処理
      *****************************************************************
       900-PTCONF-READ-SEC         SECTION.
      *
           MOVE    PTCONF-REC          TO  MCPDATA-REC
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DB-SELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTCONF-REC
                   MOVE    ZERO        TO  FLG-PTCONF
               ELSE
                   INITIALIZE              PTCONF-REC
                   MOVE    1           TO  FLG-PTCONF
               END-IF
           ELSE
               INITIALIZE                  PTCONF-REC
               MOVE    1               TO  FLG-PTCONF
           END-IF
      *
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
       900-PTCONF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ更新処理
      *****************************************************************
       910-DBUPDATE-SEC                 SECTION.
      *
           MOVE     "DBUPDATE"         TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBUPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC            SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.
