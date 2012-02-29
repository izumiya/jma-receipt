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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBNYUINCHG.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次
      *  コンポーネント名  : 入院会計再作成（システム管理変更時の置き換え）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/05/15    NACL-小豆沢   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.01    NACL-小豆沢  07/06/15  グループ診療対応
      *平成２０年４月改定対応
      *  04.02.00    NACL-小豆沢  08/03/06  療養病棟（病床）入院患者についても
      *                                     処理可能となるように改修
      *  04.02.00    NACL-小豆沢  08/03/07  シス管病棟設定の加算判定変更
      *  04.02.00    NACL-小豆沢  08/06/26  他院（特別な関係の医療機関）通算日数
      *                                     カウント変更
      *  04.03.00    NACL-小豆沢  08/07/08  褥瘡患者管理加算対応
      *  04.03.00    NACL-小豆沢  08/08/11  特定入院料コード取得をサブプロに変更
      *  04.05.00    NACL-太田    10/01/29  拡張漢字対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                     PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    エラーファイル 名称領域 
      **     COPY    "CPCOMMONDAT2.INC"      REPLACING   //RECE01PARA//
      **                                     BY          //RECEERR//.
      **     03  FILLER                      PIC X(04)   VALUE   ".dat".
      *    ステータス領域
      *
      *    一覧
           COPY    "HCMSL55.INC".
      *
      *
       01  STS-AREA.
           03  STS-RECEERR                 PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-PTNUM                   PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-NYUINACCTARI            PIC 9(01).
           03  FLG-NYUINACCT-MAE-CHK       PIC 9(01).
           03  FLG-ENTRYCHK                PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-PTRSIINF                PIC 9(01).
           03  FLG-INPUTCD                 PIC 9(01).
           03  FLG-NYUKSN                  PIC 9(01).
           03  FLG-SYUNOU                  PIC 9(01).
           03  FLG-DBRC                    PIC 9(01).
           03  FLG-TAISYO                  PIC 9(01).
           03  FLG-NG                      PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE                    PIC 9(05).
           03  CNT-PAGE                    PIC 9(05).
           03  CNT-JOB-UPDCNT              PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME                    PIC 9(08).
           03  SYS-YMD.
               05  SYS-YY                  PIC 9(02).
               05  SYS-MM                  PIC 9(02).
               05  SYS-DD                  PIC 9(02).
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
           03  IDXA                        PIC 9(05).
           03  IDXB                        PIC 9(05).
           03  TN-IDX                      PIC 9(02).
           03  IDX-DAY                     PIC 9(02).
           03  IDX-SKJ                     PIC 9(02).
           03  IDX-GAIHAKU                 PIC 9(02).
           03  IDX-HKN                     PIC 9(02).
           03  KIKAN-IDX1                  PIC 9(04).
           03  KIKAN-IDX2                  PIC 9(04).
           03  IDX-SKJ-TAIHI               PIC 9(02).
      *
      *    パラメタ領域
       01  PARA-AREA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID              PIC 9(07).
           03  WRK-PARA-SHELLID            PIC X(08).
           03  WRK-PARA-HOSPNUM            PIC 9(02).
           03  RECEERR                     PIC X(100).
           03  WRK-PARA-SRYYM              PIC X(06).
           03  WRK-PARA-BTUNUM             PIC X(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MCP-TABLE               PIC X(64).
           03  WRK-MCP-PATHNAME            PIC X(64).
           03  WRK-ERRNO                   PIC 9(2).
           03  WRK-RECEERR                 PIC X(200).
           03  WRK-PTID                    PIC 9(10).
           03  WRK-SRYYM                   PIC X(06).
           03  WRK-TNS-YMD                 PIC X(08).
           03  WRK-NAIYO                   PIC X(100).
           03  WRK-KANACHK-MAE-INPUT       PIC X(400).
      *
      *    プレイルーム、保育士加算加算退避領域
           03  WRK-BTU-HIKSISUKBN          PIC X(01).
           03  WRK-BTU-KNGTEK-KBN          PIC X(01).
           03  WRK-BTU-DRTEK-KBN           PIC X(01).
      *
      *    入院会計作成用ワーク
           03  WRK-YMD                     PIC X(10). 
           03  WRK-NAI-SRYYM-41.
               05  WRK-NAI-SRYYM-41-YY     PIC 9(04).
               05  WRK-NAI-SRYYM-41-MM     PIC 9(02).
           03  WRK-TSUSAN                  PIC 9(05).
           03  WRK-SHONUM                  PIC 9(03).
           03  WRK-STYMD                   PIC X(08).
           03  WRK-EDYMD                   PIC X(08).
           03  WRK-NISSU                   PIC 9(05).
           03  WRK-NISSU1                  PIC 9(05).
           03  WRK-NISSU2                  PIC 9(05).
           03  WRK-SENTEI-STYMD            PIC X(08).
           03  WRK-BIRTHDAY-15             PIC X(08).
           03  WRK-BIRTHDAY-15R   REDEFINES    WRK-BIRTHDAY-15.
               05  WRK-BIRTHDAY-15Y        PIC 9(04).
               05  WRK-BIRTHDAY-15MD       PIC 9(04).
           03  WRK-SANTEISTYMD             PIC X(08).
           03  WRK-TOKUTEI-NYUIN           PIC X(02).
           03  WRK-SAGAKU-KBN              PIC X(02).
           03  WRK-SAGAKU-KBN9     REDEFINES  WRK-SAGAKU-KBN
                                           PIC 9(02).
           03  WRK-HKNCOMBINUM             PIC X(04).
           03  WRK-HKNCOMBINUM9    REDEFINES  WRK-HKNCOMBINUM
                                           PIC 9(04).
           03  WRK-TAIHI-SRYKA             PIC X(02).
           03  WRK-TAIHI-SKJ18             PIC 9(02)   OCCURS  31.
           03  WRK-TAIHI-SKJ19             PIC 9(02)   OCCURS  31.
           03  WRK-TAIHI-SKJ20             PIC 9(02)   OCCURS  31.
           03  WRK-TAIHI-GAIHAKU           PIC 9(01)   OCCURS  31.
           03  WRK-TAIHI-HKN               PIC 9(04)   OCCURS  31.
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-KEISANYMD               PIC X(08).
      **** 03  WRK-TAIHI-SRYYM-41          PIC X(06).
      *    医療機関設定の入院料加算チェック用
      *****03  WRK-NYUKHNKSN-SRYCD PIC X(09)  OCCURS 20.
           03  WRK-NYUKHNKSN-AREA.
               05  WRK-NYUKHNKSN-SRYCD PIC X(09)  OCCURS 60.
           03  WRK-NYUKSNKBN               PIC X(03).
           03  WRK-BTU-NYUKSNKBN           PIC X(03).
           03  WRK-BTU-CHUKSNCD            PIC X(04).
           03  WRK-KSN-NYUKSNKBN           PIC X(03).
           03  WRK-KSN-CHUKSNCD            PIC X(04).
           03  WRK-SRYCD                   PIC X(09).
           03  WRK-STYUKYMD                PIC X(08).
           03  WRK-NYUINYM                 PIC X(06).
           03  WRK-TOKUBETSU-NYUIN         PIC X(01).
      *    特定入院料の会計作成後に一般の入院料で会計を作成するための
      *    領域
           03  WK-TOKUTEI-DAY              PIC 9(04).
           03  IPN-NYUKAIKEI-SAKUSEI-FLG   PIC X(01).
           03  IPN-SANTEISTYMD             PIC X(08).
           03  WRK-IPN-HANTEI-YMD1.
               05  WRK-IPN-HANTEI-YM1.
                   07  WRK-IPN-HANTEI-Y1   PIC 9(04).
                   07  WRK-IPN-HANTEI-M1   PIC 9(02).
               05  WRK-IPN-HANTEI-D1       PIC 9(02).
           03  WRK-IPN-HANTEI-YMD2         PIC X(08).
      *
           03  WRK-CHK-SRYYM               PIC X(06).
           03  WRK-CHK-SRYYMR      REDEFINES  WRK-CHK-SRYYM.
               05  WRK-CHK-SRYYR           PIC 9(04).
               05  WRK-CHK-SRYMR           PIC 9(02).
           03  WRK-ERRCD                   PIC X(04).
      *
           03  WRK-NENREIXXX               PIC 9(03).
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
       01  LST-AREA.
           03  LST-HEAD1.
               05                      PIC X(40)   VALUE   " ".
               05                      PIC X(30)   VALUE   
                             "入院会計一括置換結果リスト".
               05                      PIC X(30)   VALUE   " ".
               05  LST-HEAD-PAGE       PIC Z,ZZ9.
               05                      PIC X(05)   VALUE   "頁".
           03  LST-HEAD.
               05                      PIC X(04)   VALUE   "番号".
               05                      PIC X(02)   VALUE   " ".
               05                      PIC X(20)   VALUE   "患者番号".
               05                      PIC X(02)   VALUE   " ".
               05                      PIC X(16)   VALUE   "病棟".
               05                      PIC X(02)   VALUE   " ".
               05                      PIC X(26)   VALUE   "氏名".
               05                      PIC X(01)   VALUE   " ".
               05                      PIC X(08)   VALUE   "対象年月".
               05                      PIC X(02)   VALUE   " ".
               05                      PIC X(30)   VALUE   "処理結果".
      *
           03  LST-MEISAI-G.
               05  LST-MEISAI-OCC      OCCURS      50.
                   07  LST-NO          PIC Z(04).
                   07  LST-FIL1        PIC X(01).
                   07  LST-PTNUM       PIC X(20).
                   07  LST-FIL2        PIC X(01).
                   07  LST-BYOTO       PIC X(20).
                   07  LST-FIL3        PIC X(01).
                   07  LST-NAME        PIC X(30).
                   07  LST-FIL4        PIC X(01).
                   07  LST-SRYYM       PIC X(08).
                   07  LST-FIL5        PIC X(01).
                   07  LST-NAIYO       PIC X(52).
      *
       01  CONST-AREA.
           03  CONST-LST-HEAD-LINE     PIC 9(05)   VALUE   2.
           03  CONST-LST-MEISAI-MAX    PIC 9(05)   VALUE   50.
      *
      *    入院履歴退避用ワーク
       01  WK-PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC"     REPLACING
                                //PTNYUINRRK-// BY //WK-PTNYUINRRK-//.
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
           COPY    "CPSK5000.INC".
           COPY    "CPSK5001.INC".
           COPY    "CPSK5002.INC".
           COPY    "CPSK2009.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院診療行為
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    入院料加算チェック
       01  NYUKSNCHK-REC.
           COPY    "CPNYUKSNCHK.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    老人一般コード変換
        01  SRYCDCHG-REC.
            COPY    "CPSRYCDCHG.INC".
      *
      *    選定療養費
       01  SENTEICDCHG-REC.
           COPY    "CPSENTEICDCHG.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷用データ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK2.INC".
      *
      *    初期加算算定サブ
           COPY    "CPORCSSYOKIKSN.INC".
      *
      *    入院会計作成サブ
           COPY    "CPORCSNYUACCT.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    通算日数取得サブ
           COPY "CPORCSTUSAN.INC".
      *
      *    入院行為照会用テーブル作成
           COPY  "CPORCSS010.INC".
      *
      *    特定入院料コード取得サブ
           COPY  "CPORCSTOKNYUIN.INC".
      *
      *    医療観察入院料コード取得パラメタ
           COPY  "CPORCSIRYOKNS.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(1000).
      *
      ******************************************************************
      *
       PROCEDURE               DIVISION
                                       USING
                                       COMMAND-PARAM.
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
           .
       000-PROC-EXT.
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
      *
           PERFORM 100-DBOPEN-SEC
      *
           INITIALIZE                  PARA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                       INTO            LNK-PRTKANRI-RENNUM
                                       LNK-PRTKANRI-TBL-KEY
                                       LNK-PRTKANRI-TBL-GROUP
                                       LNK-PRTKANRI-SHORI-RENNUM
                                       LNK-PRTKANRI-SRYYM
                                       LNK-PRTKANRI-SKYYMD
                                       LNK-PRTKANRI-SHELLID
                                       LNK-PRTKANRI-PRIORITY
                                       LNK-PRTKANRI-TERMID
                                       LNK-PRTKANRI-OPID
                                       LNK-PRTKANRI-PRTNM
                                       WRK-PARA-JOBID
                                       WRK-PARA-SHELLID
                                       WRK-PARA-HOSPNUM
                                       RECEERR
                                       WRK-PARA-SRYYM
                                       WRK-PARA-BTUNUM
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO   SPA-HOSPNUM
      *
           CALL    "ORCSENCODING"      USING
                                       MCPAREA
                                       SPA-AREA
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "ORCBNYUINCHG"      TO  JOB-PGID
           MOVE    "入院会計一括置換"  TO  JOB-SHELLMSG
      *
           PERFORM   900-CALL-ORCSJOB-SEC
      *
      **     MOVE    "RECEERR"           TO  RECEERR-FILE-ID
      **     MOVE    LNK-PRTKANRI-TERMID TO  RECEERR-TERMID
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    医療機関ＩＤ編集
           INITIALIZE                      SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-1001-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-1001-STYUKYMD (7:2)
           MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-SYSKANRI-SELECT-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    "tbl_syskanri"     TO  MCP-TABLE
               MOVE    "key10"            TO  MCP-PATHNAME
               PERFORM     910-SYSKANRI-READ-SEC
                   IF    FLG-SYSKANRI  =   ZERO
                         MOVE    SYS-1001-PREFNUM TO  SPA-PREFNUM
                   ELSE
                         MOVE  "医療機関情報が取得できませんでした。"
                                           TO  WRK-RECEERR
                         PERFORM 500-ERR-HENSYU-SEC
                         GO  TO  100-INIT-EXT
                   END-IF
           ELSE
               MOVE    "医療機関情報が取得できませんでした。"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  100-INIT-EXT
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    主科情報取得
           INITIALIZE                      SYS-2009-REC
           MOVE    "2009"              TO  SYS-2009-KANRICD
           MOVE    "*"                 TO  SYS-2009-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-2009-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2009-STYUKYMD (7:2)
           MOVE    SYS-2009-STYUKYMD   TO  SYS-2009-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-2009-HOSPNUM
           MOVE    SYS-2009-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-2009-REC
               IF      SYS-2009-SYUKA-KBN  =  "1"
                   MOVE   "1"          TO  SPA-SYUKA-KBN
                   MOVE   SYS-2009-SYUKA-NYUIN
                                       TO  SPA-SYUKA-NYUIN
                   MOVE   SYS-2009-SYUKA-MODE
                                       TO  SPA-SYUKA-MODE
                   MOVE   SYS-2009-SYUKA-SELECT
                                       TO  SPA-SYUKA-SELECT
               END-IF
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
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
      *    帳票初期処理
           PERFORM 2001-HCMSL55-INIT-SEC
      *
      *    作成対象患者検索
           MOVE    ZERO                 TO      FLG-PTNYUINRRK
           INITIALIZE                           PTNYUINRRK-REC
           MOVE    SYS-1001-HOSPNUM     TO      PTNYUINRRK-HOSPNUM
      *    月末日
           INITIALIZE                           STS-AREA-DAY
           INITIALIZE                           LNK-DAY7-AREA
           MOVE    "71"                 TO      LNK-DAY7-IRAI
           MOVE    WRK-PARA-SRYYM       TO      LNK-DAY7-YM
           CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI      TO      PTNYUINRRK-TENNYUYMD
      *    月初日
           MOVE    WRK-PARA-SRYYM       TO      PTNYUINRRK-TENSTUYMD
                                                                (1:6)
           MOVE    "01"                 TO      PTNYUINRRK-TENSTUYMD
                                                                (7:2)
           MOVE    "tbl_ptnyuinrrk"     TO      MCP-TABLE
           MOVE    "key50"              TO      MCP-PATHNAME
           PERFORM   910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK       =       ZERO
               MOVE    "tbl_ptnyuinrrk"     TO      MCP-TABLE
               MOVE    "key50"              TO      MCP-PATHNAME
               PERFORM  910-PTNYUINRRK-READ-SEC
               PERFORM  UNTIL  FLG-PTNYUINRRK    =    1
                       DISPLAY "WRK-PARA-BTUNUM = " WRK-PARA-BTUNUM
                       IF   WRK-PARA-BTUNUM      =    SPACE
                           MOVE    PTNYUINRRK-PTID   TO  WRK-PTID
      *                    入院会計存在チェック
                           PERFORM    2001-NYUINACCT-CHK-SEC
      *                    入院会計更新処理
                           PERFORM     2001-NYUINACCT-UPD-SEC
                       ELSE
                         IF   PTNYUINRRK-BTUNUM  =   WRK-PARA-BTUNUM
                             MOVE    PTNYUINRRK-PTID   TO  WRK-PTID
      *                      入院会計存在チェック
                             PERFORM    2001-NYUINACCT-CHK-SEC
      *                      入院会計更新処理
                             PERFORM     2001-NYUINACCT-UPD-SEC
                         END-IF
                       END-IF
                   MOVE    "tbl_ptnyuinrrk"     TO      MCP-TABLE
                   MOVE    "key50"              TO      MCP-PATHNAME
                   PERFORM  910-PTNYUINRRK-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"     TO      MCP-TABLE
           MOVE    "key50"              TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           IF    ( CNT-LINE            >   ZERO )
               PERFORM 2002-HCMSL55-PRT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票初期処理
      *****************************************************************
       2001-HCMSL55-INIT-SEC           SECTION.
      *
           MOVE    ZERO                TO  CNT-LINE
                                           CNT-PAGE
      *
           INITIALIZE                      LST-MEISAI-G
      *
           .
       2001-HCMSL55-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計存在チェック処理（食事と保険の退避処理も併せて処理）
      *****************************************************************
       2001-NYUINACCT-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCTARI
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                      NYUINACCT-REC
           MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    WRK-PARA-SRYYM      TO  NACCT-SRYYM
           DISPLAY "NACCT-HOSPNUM = " NACCT-HOSPNUM
           DISPLAY "NACCT-NYUGAIKBN = " NACCT-NYUGAIKBN
           DISPLAY "NACCT-PTID = " NACCT-PTID
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT       =   ZERO
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               PERFORM UNTIL ( FLG-NYUINACCT       NOT =   ZERO )
      ***                  OR   ( FLG-NYUINACCTARI    NOT =   ZERO )
      *
                   IF    ( NACCT-ZAISKBKBN     =    "1"    )
                       MOVE    1           TO  FLG-NYUINACCTARI
                   END-IF
      *            食事の退避
                   IF    ( NACCT-ZAISKBKBN     =    "4"    )   AND
                         ( NACCT-ZAITEN        =    ZERO   )
                       PERFORM VARYING IDX-SKJ FROM 1 BY 1
                          UNTIL (IDX-SKJ    >    31)
                              EVALUATE   NACCT-SRYCDTOTAL
      *                           朝食
                                  WHEN   099999918
                                      MOVE  NACCT-DAY(IDX-SKJ)
                                             TO  WRK-TAIHI-SKJ18
                                                     (IDX-SKJ)
      *                           昼食
                                  WHEN   099999919
                                      MOVE  NACCT-DAY(IDX-SKJ)
                                             TO  WRK-TAIHI-SKJ19
                                                     (IDX-SKJ)
      *                           夕食
                                  WHEN   099999920
                                      MOVE  NACCT-DAY(IDX-SKJ)
                                             TO  WRK-TAIHI-SKJ20
                                                     (IDX-SKJ)
                              END-EVALUATE
                       END-PERFORM
                   END-IF
      *            外泊の退避
                   IF    ( NACCT-ZAISKBKBN     =    "2"    )
                       PERFORM VARYING IDX-GAIHAKU  FROM 1 BY 1
                          UNTIL (IDX-GAIHAKU   >    31)
                           MOVE    NACCT-DAY(IDX-GAIHAKU)
                                           TO  WRK-TAIHI-GAIHAKU
                                                       (IDX-GAIHAKU)
                       END-PERFORM
                   END-IF
      *            保険組合せの退避
                   IF    ( NACCT-ZAISKBKBN     =    "5"    )
                       PERFORM VARYING IDX-HKN  FROM 1 BY 1
                          UNTIL (IDX-HKN       >    31)
                           MOVE    NACCT-DAY(IDX-HKN)
                                           TO  WRK-TAIHI-HKN(IDX-HKN)
                       END-PERFORM
                   END-IF
      *
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key7"              TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
      *
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2001-NYUINACCT-SONZAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計更新処理
      *****************************************************************
       2001-NYUINACCT-UPD-SEC          SECTION.
      *
           MOVE    SPACE          TO        WRK-ERRCD
      *
      *    入院当月分でないことのチェック
           IF  (WRK-PARA-SRYYM    =    PTNYUINRRK-NYUINYMD(1:6)) AND
               (WRK-PARA-SRYYM    =    PTNYUINRRK-TENNYUYMD(1:6))
               MOVE   "2007"      TO        WRK-ERRCD
           END-IF
      *
      *    退院済分でないことのチェック
           IF  (PTNYUINRRK-TAIINYMD    NOT =  "99999999")
               MOVE   "2008"      TO        WRK-ERRCD
           END-IF
      *
      *    定期請求済分でないことのチェック
           IF   WRK-ERRCD         =     SPACE
               INITIALIZE                      SYUNOU-REC
               MOVE    PTNYUINRRK-HOSPNUM  TO  SYU-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  SYU-PTID
               MOVE    WRK-PARA-SRYYM      TO  SYU-SRYYMD(1:6)
               MOVE    "%"                 TO  SYU-SRYYMD(7:1)
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key60"             TO  MCP-PATHNAME
               PERFORM 910-SYUNOU-SELECT-SEC
               IF      FLG-SYUNOU          =   ZERO
                   MOVE    "tbl_syunou"    TO  MCP-TABLE
                   MOVE    "key60"         TO  MCP-PATHNAME
                   PERFORM 910-SYUNOU-READ-SEC
                   IF    FLG-SYUNOU        =   ZERO
                       MOVE   "2009"       TO  WRK-ERRCD
                   END-IF
               END-IF
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key60"             TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *
      *    月内で異動処理分のデータでないことのチェック
           IF   WRK-ERRCD           =    SPACE
               IF  (WRK-PARA-SRYYM  =    PTNYUINRRK-TENNYUYMD(1:6)) OR
                   (WRK-PARA-SRYYM  =    PTNYUINRRK-TENSTUYMD(1:6))
                   MOVE   "2010"    TO   WRK-ERRCD
               END-IF
           END-IF
      *
      *    特定入院料算定患者でないことのチェック
           IF   WRK-ERRCD           =    SPACE
              DISPLAY "PTNYUINRRK-TOKUTEI-KBN ="
                       PTNYUINRRK-TOKUTEI-KBN
              DISPLAY "PTNYUINRRK-TOKUTEI-NYUIN ="
                       PTNYUINRRK-TOKUTEI-NYUIN
              IF        PTNYUINRRK-TOKUTEI-KBN  =  SPACE
                  CONTINUE
              ELSE
                IF  (PTNYUINRRK-TOKUTEI-KBN    =   "3")     AND
                   ((PTNYUINRRK-TOKUTEI-NYUIN  >=   "25")   AND
                    (PTNYUINRRK-TOKUTEI-NYUIN  <=   "48"))
                         CONTINUE
                ELSE
                    MOVE   "2011"    TO   WRK-ERRCD
                END-IF
              END-IF
           END-IF
      *
           IF        (FLG-NYUINACCTARI   =    1)   AND
                     (WRK-ERRCD          =    SPACE)
      *        既に作成済の入院会計が存在すれば
      *        削除を行う
               INITIALIZE                      NYUINACCT-REC
               MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
               MOVE    WRK-PARA-SRYYM      TO  NACCT-SRYYM
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "del2"              TO  MCP-PATHNAME
               PERFORM   900-ORCDBMAIN-SEC
      *
               INITIALIZE                      NYUINACT-REC
               MOVE    PTNYUINRRK-HOSPNUM  TO  NSRY-HOSPNUM
               MOVE    PTNYUINRRK-PTID     TO  NSRY-PTID
               MOVE    WRK-PARA-SRYYM      TO  NSRY-SRYYM
               MOVE    NYUINACT-REC        TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_nyuinact"      TO  MCP-TABLE
               MOVE    "del13"             TO  MCP-PATHNAME
               PERFORM   900-ORCDBMAIN-SEC
      *
      *        会計再作成
               PERFORM     410191-KAIKEI-SAKUSEI-SEC
      *
           END-IF
      *
           PERFORM     2002-HCMSL55-HEN-SEC
      *
           .
       2001-NYUINACCT-UPD-EXT.
           EXIT.
      ******************************************************************
      *    帳票編集処理
      *****************************************************************
       2002-HCMSL55-HEN-SEC            SECTION.
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
      *    患者情報検索
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
      *    患者番号検索
           PERFORM 900-PTNUM-KEY-SEL-SEC
      *
           ADD     1                   TO  CNT-JOB-UPDCNT
           MOVE    CNT-JOB-UPDCNT      TO  LST-NO    (CNT-LINE)
           MOVE    PTNUM-PTNUM         TO  LST-PTNUM (CNT-LINE)
      *
           MOVE    PTINF-NAME          TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-ENRTRY-CHK-SEC
           IF    ( FLG-ENTRYCHK        =   2 )
               MOVE    ALL "　"        TO  LST-NAME  (CNT-LINE)
               STRING  PTINF-NAME          DELIMITED   BY  SPACE
               INTO    LST-NAME  (CNT-LINE)
               END-STRING
           ELSE
               MOVE    PTINF-NAME      TO  LST-NAME  (CNT-LINE)
           END-IF
      *
           IF    ( SPA-ENCODING    =   2 )
               INITIALIZE                  ORCSKANACHK2AREA
               MOVE    "1"             TO  KANACHK2-SYORI
               MOVE    LST-NAME  (CNT-LINE)
                                       TO  KANACHK2-MAE-INPUT
               CALL    "ORCSKANACHK2"  USING
                                       ORCSKANACHK2AREA
               MOVE    KANACHK2-MAE-INPUT
                                       TO  LST-NAME  (CNT-LINE)
           END-IF
      *
      *    病棟名編集
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    PTNYUINRRK-BTUNUM   TO  SYS-5001-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-5001-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-5001-STYUKYMD (7:2)
           MOVE    SYS-5001-STYUKYMD   TO  SYS-5001-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-SYSKANRI-SELECT-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM     910-SYSKANRI-READ-SEC
                   IF    FLG-SYSKANRI  =   ZERO
                       MOVE    ALL "　"        TO  LST-BYOTO
                                                     (CNT-LINE)
                       STRING  SYS-5001-BTU-NAME  DELIMITED  BY  SPACE
                       INTO    LST-BYOTO  (CNT-LINE)
                       END-STRING
                   END-IF
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    WRK-PARA-SRYYM      TO  WRK-SRYYM
           MOVE    WRK-SRYYM           TO  LST-SRYYM (CNT-LINE)
           IF      FLG-NYUINACCTARI   =   ZERO
               MOVE  "更新対象入院会計がありませんでした" TO  WRK-NAIYO
           ELSE
               EVALUATE    WRK-ERRCD
                   WHEN    SPACE
                       MOVE  "入院会計を更新しました" TO  WRK-NAIYO
      *
      **                 IF   WRK-PARA-SRYYM = "200804"
      **                   PERFORM 20021-GAITOU-HENSYU-SEC
      **
      **                   EVALUATE   FLG-TAISYO
      **                    WHEN        1
      **                  MOVE  "入院会計を更新しました（後期高齢該当）"
      **                                                TO  WRK-NAIYO
      **                    WHEN        2
      **                  MOVE  "入院会計を更新しました（生活療養該当）"
      **                                                TO  WRK-NAIYO
      **                   END-EVALUATE
      **                 END-IF
      *
                   WHEN    "2003"
                       MOVE  "通算日数の取得に失敗しました"
                                                      TO  WRK-NAIYO
                   WHEN    "2006"
                       MOVE  "入院料加算の取得に失敗しました"
                                                      TO  WRK-NAIYO
                   WHEN    "2007"
                       MOVE  "対象月入院登録分のため処理できません"
                                                      TO  WRK-NAIYO
                   WHEN    "2008"
                       MOVE  "退院済み患者のため処理できません"
                                                      TO  WRK-NAIYO
                   WHEN    "2009"
                       MOVE  "定期請求済みのため処理できません"
                                                      TO  WRK-NAIYO
                   WHEN    "2010"
                    MOVE  "対象月での異動処理があるため処理できません"
                                                      TO  WRK-NAIYO
                   WHEN    "2011"
                      MOVE  "特定入院料患者のため処理できません"
                                                      TO  WRK-NAIYO
                   WHEN    "2012"
                      MOVE  "入院行為照会用テーブルの更新に失敗"
                                                      TO  WRK-NAIYO
                   END-EVALUATE
           END-IF
           MOVE    WRK-NAIYO           TO  LST-NAIYO (CNT-LINE)
      *
           IF    ( CNT-LINE            >=  CONST-LST-MEISAI-MAX )
               PERFORM 2002-HCMSL55-PRT-SEC
           END-IF
      *
           .
       2002-HCMSL55-HEN-EXT.
           EXIT.
      *****************************************************************
      *    該当者編集処理
      *****************************************************************
       20021-GAITOU-HENSYU-SEC            SECTION.
      *
           MOVE     ZERO     TO     FLG-TAISYO
      *
      *    保険組合せマスタの検索
           INITIALIZE                           HKNCOMBI-REC
           MOVE    WRK-PARA-HOSPNUM     TO      COMB-HOSPNUM
           MOVE    WRK-PTID             TO      COMB-PTID
           MOVE    WRK-TAIHI-HKN(1)     TO      COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC         TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "tbl_hkncombi"       TO      MCP-TABLE
           MOVE    "key"                TO      MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_hkncombi"   TO      MCP-TABLE
               MOVE    "key"            TO      MCP-PATHNAME
               MOVE    "DBFETCH"        TO      MCP-FUNC
               CALL    "ORCDBMAIN"      USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
               IF      MCP-RC           =       ZERO
                  MOVE    MCPDATA-REC   TO  HKNCOMBI-REC
               ELSE
                  INITIALIZE                HKNCOMBI-REC
                  DISPLAY "HKNCOMBI DBFETCH ERR"
               END-IF
           ELSE
                  INITIALIZE                HKNCOMBI-REC
                  DISPLAY "HKNCOMBI DBSELECT ERR"
           END-IF
           MOVE    "tbl_hkncombi"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
      *    入院患者の年齢計算（３月１日時点）
           MOVE    ZERO                TO  WRK-NENREIXXX
           INITIALIZE                      ORCSAGECHKAREA
           MOVE    WRK-PARA-HOSPNUM    TO  AGECHK-HOSPNUM
           MOVE    WRK-PTID            TO  AGECHK-PTID
           MOVE    "200803"            TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"                TO  AGECHK-SRYYMD(7:2)
           MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           IF      AGECHK-RC           =   ZERO
              MOVE   AGECHK-NENREI     TO  WRK-NENREIXXX
           END-IF
      *    入院患者の年齢計算（４月１日時点）
           INITIALIZE                      ORCSAGECHKAREA
           MOVE    WRK-PARA-HOSPNUM    TO  AGECHK-HOSPNUM
           MOVE    WRK-PTID            TO  AGECHK-PTID
           MOVE    "200804"            TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"                TO  AGECHK-SRYYMD(7:2)
           MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           IF      AGECHK-RC           =   ZERO
      *
             IF     (COMB-HKNNUM  =  "971" OR "973" OR "975")
             OR     (COMB-HKNNUM(1:2) = "98"                )
             OR     (COMB-HKNNUM  =  SPACE                  )
               CONTINUE
             ELSE
               IF   (COMB-KOH1HKNNUM  =  "027")
               OR   (AGECHK-NENREI   >=   75  )
                   MOVE     1     TO     FLG-TAISYO
               END-IF
             END-IF
      *
             IF   (FLG-TAISYO             =    1            )
             OR   (COMB-HKNNUM            =  "971" OR "973" )
             OR   (AGECHK-NENREI          <   65            )
             OR   (WRK-NENREIXXX         >=   70            )
             OR   (SYS-5001-BTU-SBT   NOT =  "04" AND "09"  )
                 CONTINUE
             ELSE
                 MOVE    ZERO                 TO  FLG-NG
                 INITIALIZE                       PTKOHINF-REC
                 MOVE    WRK-PARA-HOSPNUM     TO  PTKOH-HOSPNUM
                 MOVE    WRK-PTID             TO  PTKOH-PTID
                 MOVE    "027"                TO  PTKOH-KOHNUM
                 MOVE    "20080331"           TO  PTKOH-TEKSTYMD
                                                  PTKOH-TEKEDYMD
                 MOVE    PTKOHINF-REC         TO  MCPDATA-REC
                 MOVE    "DBSELECT"           TO  MCP-FUNC
                 MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                 MOVE    "key6"               TO  MCP-PATHNAME
                 CALL    "ORCDBMAIN"          USING  MCPAREA
                                                     MCPDATA-REC
                                                     SPA-AREA
                 IF      MCP-RC               =      ZERO
                    MOVE    "tbl_ptkohinf"    TO  MCP-TABLE
                    MOVE    "key6"            TO  MCP-PATHNAME
                    MOVE    "DBFETCH"         TO  MCP-FUNC
                    CALL    "ORCDBMAIN"       USING  MCPAREA
                                                     MCPDATA-REC
                                                     SPA-AREA
                    IF      MCP-RC            =      ZERO
                      MOVE    MCPDATA-REC     TO  PTKOHINF-REC
                      MOVE    1               TO  FLG-NG
                    END-IF
                 END-IF
                 MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                 MOVE    "key6"               TO  MCP-PATHNAME
                 PERFORM  900-CLOSE-SEC
      *
                 IF    FLG-NG   =   ZERO
                     MOVE     2     TO     FLG-TAISYO
                 END-IF
             END-IF
      *
           END-IF
      *
           .
       20021-GAITOU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       2002-HCMSL55-PRT-SEC            SECTION.
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
      *
           MOVE    CNT-PAGE            TO  LST-HEAD-PAGE
           MOVE    LST-HEAD1           TO  HCMSL55-MOJI (1)
           MOVE    LST-HEAD            TO  HCMSL55-MOJI (2)
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-LST-MEISAI-MAX )
      *
               MOVE    LST-MEISAI-OCC (IDX0)
                                       TO  HCMSL55-MOJI
                                          (IDX0 + CONST-LST-HEAD-LINE)
      *
           END-PERFORM
      *
           PERFORM 290-PRINT-SEC
      *
           MOVE    ZERO                    TO  CNT-LINE
      *
           INITIALIZE                      LST-MEISAI-G
      *
           .
       2002-HCMSL55-PRT-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       290-PRINT-SEC                   SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"           TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                   TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SPRT-PRIORITY
           MOVE   "HCMSL55.red"    TO  SPRT-PRTID
           MOVE    "入院会計一括置換え"
                                    TO  SPRT-TITLE
           MOVE    HCMSL55          TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID
                                    TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID
                                    TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM
                                    TO  SPRT-PRTNM
           MOVE    "1"              TO  SPRT-SITEKBN
           CALL    "ORCSPRT"            USING
                                        ORCSPRTAREA
                                        SPA-AREA
           IF      SPRT-RETURN          =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                        TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       290-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC              SECTION.
      *
           OPEN    OUTPUT      RECEERR-FILE
      *
           MOVE    WRK-RECEERR     TO  RECEERR-REC
           WRITE   RECEERR-REC
           CLOSE   RECEERR-FILE
      *
      *    ジョブ管理終了処理
           MOVE    "JBE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-RECEERR     TO  JOB-YOBI
           MOVE    "9999"          TO  JOB-ERRCD
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           STOP RUN
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角半角混在チェック
      *****************************************************************
       800-ENRTRY-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-ENTRYCHK
      *
           IF  ( WRK-KANACHK-MAE-INPUT =   SPACE )
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           MOVE    SPACE               TO  ORCSKANACHKAREA
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-MAE-INPUT
                                       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      ( KANACHK-RC    NOT =   ZERO )
               MOVE    9               TO  FLG-ENTRYCHK
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
      *
      *    全角文字の場合、間に半角スペースがなかったかチェック
           IF    ( KANACHK-SYUBETU     =   2  )
            AND  ( KANACHK-RC2         =   8  )
               MOVE    9               TO  FLG-ENTRYCHK
           ELSE
               MOVE    KANACHK-SYUBETU TO  FLG-ENTRYCHK
           END-IF
      *
           .
       800-ENRTRY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   会計作成処理
      *****************************************************************
       410191-KAIKEI-SAKUSEI-SEC            SECTION.
      *
      *    入院会計テーブル作成処理
           PERFORM 410191-NYUACCT-SAKUSEI-SEC
           IF      IPN-NYUKAIKEI-SAKUSEI-FLG   =   "1"
               PERFORM 410191-IPNNYUACCT-SAKUSEI-SEC
           END-IF
      *
           .
      *
       410191-KAIKEI-SAKUSEI-EXT.
           EXIT.
      *
      *021118
      *****************************************************************
      *    入院会計テーブル作成処理                                    
      *****************************************************************
       410191-NYUACCT-SAKUSEI-SEC   SECTION.
      *
           MOVE   SPACE                TO  IPN-NYUKAIKEI-SAKUSEI-FLG
           INITIALIZE      ORCSSYOKIKSNAREA
           MOVE    WRK-PARA-SRYYM      TO  WRK-NAI-SRYYM-41.
      *
      *    通算日数を求める（入院履歴を検索）
           MOVE    ZERO                TO  WRK-TSUSAN
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    WRK-PARA-HOSPNUM    TO  PTNYUINRRK-HOSPNUM
           MOVE    WRK-PTID            TO  PTNYUINRRK-PTID
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM  910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK      =   ZERO
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
               MOVE    PTNYUINRRK-SHONUM   TO  WRK-SHONUM
               MOVE    PTNYUINRRK-TENNYUYMD
                                           TO  WRK-SANTEISTYMD
               MOVE    PTNYUINRRK-TOKUTEI-NYUIN
                                           TO  WRK-TOKUTEI-NYUIN
      *        老人性認知症疾患療養病棟入院料は平成１８年３月迄
               IF     (PTNYUINRRK-TOKUTEI-KBN   =   "2")   AND
                      (PTNYUINRRK-TOKUTEI-NYUIN =   "06")  AND
                      (WRK-PARA-SRYYM           >=  "200604")
                       MOVE      SPACE     TO  WRK-TOKUTEI-NYUIN
               END-IF
               MOVE    PTNYUINRRK-REC      TO  WK-PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    通算日数を求める（通算日数取得サブを使用）
           PERFORM 4801-TSUSAN-SANTEI-GET-SEC
           IF    ( WRK-ERRCD       NOT =  SPACE )
               GO  TO  410191-NYUACCT-SAKUSEI-EXT
           END-IF
      *
           MOVE    WK-PTNYUINRRK-REC   TO  PTNYUINRRK-REC
      *
           MOVE    WRK-TSUSAN          TO  SYOKIKSN-IN-TUSANNISSU
           MOVE    WRK-PARA-SRYYM      TO  SYOKIKSN-IN-SANTEISTYMD(1:6)
           MOVE    "01"                TO  SYOKIKSN-IN-SANTEISTYMD(7:2)
      *
      *    特定集中治療室管理料の場合は最長１４日間の算定の為、
      *    次月の会計作成時は無条件に一般の入院料算定とする
      *    以下の特定入院料も同様
      *    救命救急入院料１
      *    救命救急入院料２
      *    一類感染症患者入院医療管理料
      *    ハイケアユニット入院医療管理料
      *    脳卒中ケアユニット入院医療管理料
      *    救命救急入院料３（救命救急入院料）
      *    救命救急入院料４（救命救急入院料）
      *    特定集中治療室管理料２（特定集中治療室管理料）
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN = "03" OR "04" OR "05" OR
                                               "06" OR "07" OR "08" OR
                                               "09" OR "14" OR "16" OR
                                               "17" OR "19" OR "55" OR
                                               "57" OR "59"
                   MOVE    SPACE   TO    WRK-TOKUTEI-NYUIN
               END-IF
           END-IF
      *
      *    広範囲熱傷特定集中治療室管理料の場合は最長６０日間の算定の為、
      *    次月の会計作成時に６０日経過時は無条件に一般の入院料算定とする
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN = "13"
                   IF       STUSAN-NISSU06  >=  60
                       MOVE    SPACE   TO    WRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    亜急性期入院医療管理料の場合は最長９０日間の算定の為、
      *    次月の会計作成時に９０日経過時は無条件に一般の入院料算定とする
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN = "18"
                   IF       STUSAN-NISSU06  >=  90
                       MOVE    SPACE   TO    WRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    亜急性期入院医療管理料２の場合は最長６０日間の算定の為、
      *    次月の会計作成時に６０日経過時は無条件に一般の入院料算定とする
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN = "54"
                   IF       STUSAN-NISSU06  >=  60
                       MOVE    SPACE   TO    WRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    救命救急入院料３（広範囲熱傷特定集中治療）の場合は最長６０日間の算定の為、
      *    次月の会計作成時に６０日経過時は無条件に一般の入院料算定とする
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN = "56"
                   IF       STUSAN-NISSU06  >=  60
                       MOVE    SPACE   TO    WRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    救命救急入院料４（広範囲熱傷特定集中治療）の場合は最長６０日間の算定の為、
      *    次月の会計作成時に６０日経過時は無条件に一般の入院料算定とする
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN = "58"
                   IF       STUSAN-NISSU06  >=  60
                       MOVE    SPACE   TO    WRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    特定集中治療室管理料２（広範囲熱傷特定集中治療）の場合は最長６０日間の算定の為、
      *    次月の会計作成時に６０日経過時は無条件に一般の入院料算定とする
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN = "60"
                   IF       STUSAN-NISSU06  >=  60
                       MOVE    SPACE   TO    WRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
           IF      WRK-TOKUTEI-NYUIN           NOT =   SPACE
               PERFORM 4801-TOKTEI-NYUIN-SET-SEC
           ELSE
               PERFORM 4801-BTUSEL-SEC
               IF      SYS-5001-BTU-KHNSRYCD   NOT =   SPACE
                   MOVE    SYS-5001-BTU-KHNSRYCD
                                               TO  SYOKIKSN-IN-KHNSRYCD
                   PERFORM 4801-RJNCD-CHG-SEC
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "2006"          TO  WRK-ERRCD
                       GO  TO  410191-NYUACCT-SAKUSEI-EXT
                   END-IF
               ELSE
                   MOVE    "2006"          TO  WRK-ERRCD
                   GO  TO  410191-NYUACCT-SAKUSEI-EXT
               END-IF
           END-IF
      *
      *    入院会計作成サブの呼出し（処理区分９により次月分のみ作成）
           INITIALIZE                      ORCSNYUACCT-AREA
           MOVE    "9"                 TO  LNK-SCNYUACCT-SYORI-KBN
           MOVE    "1"                 TO  SPA-NYUGAIKBN
           MOVE    PTNYUINRRK-PTID     TO  SPA-PTID
           MOVE    PTNYUINRRK-NYUINKA  TO  SPA-SRYKA
           MOVE    SYOKIKSN-IN-KHNSRYCD
                                      TO  LNK-SCNYUACCT-NYUKHN-SRYCD(1)
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                                       TO  LNK-SCNYUACCT-STSRYYMD
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                                       TO  LNK-SCNYUACCT-EDSRYYMD
           PERFORM VARYING IDX6        FROM    1   BY   1
                   UNTIL  (IDX6            >     7 )    OR
                          (SYOKIKSN-OT-KGNSRYCD(IDX6)   =  SPACE)
               MOVE   SYOKIKSN-OT-KGNSRYCD(IDX6)
                                   TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOSTYMD(IDX6)
                                   TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                                (IDX6)
               MOVE   SYOKIKSN-OT-YUKOEDYMD(IDX6)
                                   TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (IDX6)
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
                                      LNK-SCNYUACCT-EDSRYYMD
           END-IF
      *
      *    通算算定済日数
           MOVE    WRK-TSUSAN          TO  LNK-SCNYUACCT-NYUKHN-NISUU
      *    特定入院で入院料が変更になるコードの対応
      *
      *    診療所老人医療管理料
           IF      SYOKIKSN-IN-KHNSRYCD    =   "190774110"
               MOVE   SPACE            TO  SYOKIKSN-IN-KHNSRYCD
                                          LNK-SCNYUACCT-NYUKHN-SRYCD(1)
               MOVE   "190774110"      TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
               MOVE   PTNYUINRRK-TENNYUYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                                    (1)
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE    13              TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                   (1)
      *
               MOVE   "190774210"      TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                       TO  LNK-DAY6-KIJUN
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE     1              TO  LNK-DAY6-ZOGEN
               CALL  "ORCSDAY"         USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                                  (2)
               MOVE    "99999999"      TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                  (2)
               PERFORM    4801-KIKAN-CHG-SEC
           END-IF
      *
      *    診療所老人医療管理料（生活療養）
           IF      SYOKIKSN-IN-KHNSRYCD    =   "190125010"
               MOVE   SPACE            TO  SYOKIKSN-IN-KHNSRYCD
                                          LNK-SCNYUACCT-NYUKHN-SRYCD(1)
               MOVE   "190125010"      TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
               MOVE   PTNYUINRRK-TENNYUYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                                    (1)
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE    13              TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                   (1)
      *
               MOVE   "190125110"      TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                       TO  LNK-DAY6-KIJUN
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE     1              TO  LNK-DAY6-ZOGEN
               CALL  "ORCSDAY"         USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                                  (2)
               MOVE    "99999999"      TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                  (2)
               PERFORM    4801-KIKAN-CHG-SEC
           END-IF
      *
      *    認知症病棟入院料１（平成２２年３月までの期間対応）
           IF      PTNYUINRRK-TENNYUYMD   <=   "20100331"
             IF    SYOKIKSN-IN-KHNSRYCD    =   "190739910"
               MOVE   SPACE            TO  SYOKIKSN-IN-KHNSRYCD
                                          LNK-SCNYUACCT-NYUKHN-SRYCD(1)
               MOVE   "190739910"      TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
               MOVE   PTNYUINRRK-TENNYUYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                                   (1)
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    "61"            TO  LNK-DAY6-IRAI
      **         MOVE   PTNYUINRRK-TENNYUYMD
      **                                 TO  LNK-DAY6-KIJUN
               IF      (PTNYUINRRK-SHOKAIKBN  = "1")  AND
                       (PTNYUINRRK-JTIKBN  = "1")
                   MOVE   PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
               ELSE
                   MOVE   SYOKIKSN-IN-SANTEISTYMD
                                       TO  LNK-DAY6-KIJUN
               END-IF
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
      **         IF      PTNYUINRRK-SHOKAIKBN  = "1"
               IF      (PTNYUINRRK-SHOKAIKBN  = "1")  AND
                       (PTNYUINRRK-JTIKBN  = "1")
                   MOVE    89              TO  LNK-DAY6-ZOGEN
               ELSE
                   COMPUTE  LNK-DAY6-ZOGEN = 90  -  STUSAN-NISSU03
               END-IF
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                    (1)
               MOVE    SYOKIKSN-IN-SANTEISTYMD
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
      *
               IF      LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           <   SYOKIKSN-IN-SANTEISTYMD
                   MOVE    1               TO  TN-IDX
               ELSE
                   MOVE    2               TO  TN-IDX
               END-IF
               MOVE    "190740010"        TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               IF      TN-IDX              =   2
                   MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                               TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE     1                  TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN     TO
                                           LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
               ELSE
                   MOVE    SYOKIKSN-IN-SANTEISTYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
               END-IF
               MOVE    "99999999"      TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                              (TN-IDX)
             END-IF
           END-IF
      *
      *    認知症治療病棟入院料１（平成２２年４月からの期間対応）
           IF      PTNYUINRRK-TENNYUYMD   >=   "20100401"
             IF    SYOKIKSN-IN-KHNSRYCD    =   "190739910"
               MOVE   SPACE            TO  SYOKIKSN-IN-KHNSRYCD
                                          LNK-SCNYUACCT-NYUKHN-SRYCD(1)
               MOVE   "190739910"      TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
               MOVE   PTNYUINRRK-TENNYUYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                                   (1)
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    "61"            TO  LNK-DAY6-IRAI
      **         MOVE   PTNYUINRRK-TENNYUYMD
      **                                 TO  LNK-DAY6-KIJUN
               IF      (PTNYUINRRK-SHOKAIKBN  = "1")  AND
                       (PTNYUINRRK-JTIKBN  = "1")
                   MOVE   PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
               ELSE
                   MOVE   SYOKIKSN-IN-SANTEISTYMD
                                       TO  LNK-DAY6-KIJUN
               END-IF
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
      **         IF      PTNYUINRRK-SHOKAIKBN  = "1"
               IF      (PTNYUINRRK-SHOKAIKBN  = "1")  AND
                       (PTNYUINRRK-JTIKBN  = "1")
                   MOVE    59              TO  LNK-DAY6-ZOGEN
               ELSE
                   COMPUTE  LNK-DAY6-ZOGEN = 60  -  STUSAN-NISSU03
               END-IF
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                    (1)
               MOVE    SYOKIKSN-IN-SANTEISTYMD
                                  TO LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
      *
               IF      LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           <   SYOKIKSN-IN-SANTEISTYMD
                   MOVE    1               TO  TN-IDX
               ELSE
                   MOVE    2               TO  TN-IDX
               END-IF
               MOVE    "190740010"        TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               IF      TN-IDX              =   2
                   MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                               TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE     1                  TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN     TO
                                           LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
               ELSE
                   MOVE    SYOKIKSN-IN-SANTEISTYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
               END-IF
               MOVE    "99999999"      TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                              (TN-IDX)
             END-IF
           END-IF
      *
      *    認知症病棟入院料２（平成２２年３月までの期間対応）
           IF      PTNYUINRRK-TENNYUYMD   <=   "20100331"
             IF    SYOKIKSN-IN-KHNSRYCD    =   "190813410"
               MOVE    SPACE           TO  SYOKIKSN-IN-KHNSRYCD
                                          LNK-SCNYUACCT-NYUKHN-SRYCD(1)
               MOVE   "190813410"      TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
               MOVE   PTNYUINRRK-TENNYUYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                                    (1)
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
      **         MOVE   PTNYUINRRK-TENNYUYMD
      **                                 TO  LNK-DAY6-KIJUN
               IF      (PTNYUINRRK-SHOKAIKBN  = "1")  AND
                       (PTNYUINRRK-JTIKBN  = "1")
                   MOVE   PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
               ELSE
                   MOVE   SYOKIKSN-IN-SANTEISTYMD
                                       TO  LNK-DAY6-KIJUN
               END-IF
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
      **         IF      PTNYUINRRK-SHOKAIKBN  = "1"
               IF      (PTNYUINRRK-SHOKAIKBN  = "1")  AND
                       (PTNYUINRRK-JTIKBN  = "1")
                   MOVE    89              TO  LNK-DAY6-ZOGEN
               ELSE
                   COMPUTE  LNK-DAY6-ZOGEN = 90  -  STUSAN-NISSU03
               END-IF
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                    (1)
               MOVE    SYOKIKSN-IN-SANTEISTYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                                    (1)
      *
               IF       LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           <   SYOKIKSN-IN-SANTEISTYMD
                   MOVE    1               TO  TN-IDX
               ELSE
                   MOVE    2               TO  TN-IDX
               END-IF
               MOVE   "190813510"          TO  
                                      LNK-SCNYUACCT-KAGEN-SRYCD(TN-IDX)
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               IF      TN-IDX              =   2
                   MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                               TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE     1                  TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN     TO
                                           LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                              (TN-IDX)
               ELSE
                   MOVE    SYOKIKSN-IN-SANTEISTYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                              (TN-IDX)
               END-IF
               MOVE    "99999999"      TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                              (TN-IDX)
             END-IF
           END-IF
      *
      *    認知症治療病棟入院料２（平成２２年４月からの期間対応）
           IF      PTNYUINRRK-TENNYUYMD   >=   "20100401"
             IF    SYOKIKSN-IN-KHNSRYCD    =   "190813410"
               MOVE    SPACE           TO  SYOKIKSN-IN-KHNSRYCD
                                          LNK-SCNYUACCT-NYUKHN-SRYCD(1)
               MOVE   "190813410"      TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
               MOVE   PTNYUINRRK-TENNYUYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                                    (1)
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
      **         MOVE   PTNYUINRRK-TENNYUYMD
      **                                 TO  LNK-DAY6-KIJUN
               IF      (PTNYUINRRK-SHOKAIKBN  = "1")  AND
                       (PTNYUINRRK-JTIKBN  = "1")
                   MOVE   PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
               ELSE
                   MOVE   SYOKIKSN-IN-SANTEISTYMD
                                       TO  LNK-DAY6-KIJUN
               END-IF
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
      **         IF      PTNYUINRRK-SHOKAIKBN  = "1"
               IF      (PTNYUINRRK-SHOKAIKBN  = "1")  AND
                       (PTNYUINRRK-JTIKBN  = "1")
                   MOVE    59              TO  LNK-DAY6-ZOGEN
               ELSE
                   COMPUTE  LNK-DAY6-ZOGEN = 60  -  STUSAN-NISSU03
               END-IF
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                    (1)
               MOVE    SYOKIKSN-IN-SANTEISTYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                                    (1)
      *
               IF       LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           <   SYOKIKSN-IN-SANTEISTYMD
                   MOVE    1               TO  TN-IDX
               ELSE
                   MOVE    2               TO  TN-IDX
               END-IF
               MOVE   "190813510"          TO  
                                      LNK-SCNYUACCT-KAGEN-SRYCD(TN-IDX)
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               IF      TN-IDX              =   2
                   MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                               TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE     1                  TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN     TO
                                           LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                              (TN-IDX)
               ELSE
                   MOVE    SYOKIKSN-IN-SANTEISTYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                              (TN-IDX)
               END-IF
               MOVE    "99999999"      TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                              (TN-IDX)
             END-IF
           END-IF
      *
      *    広範囲熱傷特定集中治療室管理料
           IF      SYOKIKSN-IN-KHNSRYCD   =   "190024810"
               MOVE   SPACE               TO  IPN-NYUKAIKEI-SAKUSEI-FLG
               MOVE   WRK-PARA-SRYYM      TO  PTNYUINRRK-TENNYUYMD(1:6)
               MOVE   "01"                TO  PTNYUINRRK-TENNYUYMD(7:2)
               MOVE   SPACE               TO  SYOKIKSN-IN-KHNSRYCD
                                          LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *        広範囲熱傷特定集中治療室管理料の算定可能日数を計算
               MOVE     ZERO              TO  WK-TOKUTEI-DAY
               COMPUTE  WK-TOKUTEI-DAY    =   60  -    STUSAN-NISSU06
               IF       STUSAN-NISSU06    <   60
                   MOVE   "190024810"         TO
                                           LNK-SCNYUACCT-KAGEN-SRYCD(1)
                   MOVE   PTNYUINRRK-TENNYUYMD TO
                                           LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                                   (1)
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE   PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   COMPUTE LNK-DAY6-ZOGEN      =   WK-TOKUTEI-DAY  -  1
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN     TO
                                           LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                    (1)
               END-IF
      *        一般入院料の算定開始日を算定
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE   PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               MOVE   WK-TOKUTEI-DAY       TO  LNK-DAY6-ZOGEN
               CALL   "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN     TO  IPN-SANTEISTYMD
      *        特定入院期間（６０日経過）した際の一般入院料作成の
      *        可否を判定
               MOVE    PTNYUINRRK-TENNYUYMD
                                           TO  WRK-IPN-HANTEI-YMD1
               IF      WRK-IPN-HANTEI-M1   =   12
                   ADD    1                TO  WRK-IPN-HANTEI-Y1
                   MOVE   1                TO  WRK-IPN-HANTEI-M1
               ELSE
                   ADD    1                TO  WRK-IPN-HANTEI-M1
               END-IF
      *        月末日を算定
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY7-AREA
               MOVE    "71"                TO  LNK-DAY7-IRAI
               MOVE   WRK-IPN-HANTEI-YM1   TO  LNK-DAY7-YM
               CALL   "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
               MOVE    LNK-DAY7-KOYOMI     TO  WRK-IPN-HANTEI-YMD1
               MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  WRK-IPN-HANTEI-YMD2
      *        一般入院会計作成用のフラグ設定
               IF      WRK-IPN-HANTEI-YMD1 >   WRK-IPN-HANTEI-YMD2
                   MOVE    "1"            TO  IPN-NYUKAIKEI-SAKUSEI-FLG
               END-IF
           END-IF
      *
      *    亜急性期入院医療管理料
           IF      SYOKIKSN-IN-KHNSRYCD   =   "190117410"
               MOVE    SPACE              TO  IPN-NYUKAIKEI-SAKUSEI-FLG
               MOVE    WRK-PARA-SRYYM     TO  PTNYUINRRK-TENNYUYMD(1:6)
               MOVE    "01"               TO  PTNYUINRRK-TENNYUYMD(7:2)
               MOVE    SPACE              TO  SYOKIKSN-IN-KHNSRYCD
                                          LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *        亜急性期入院医療管理料の算定可能日数を計算
               MOVE    ZERO                TO  WK-TOKUTEI-DAY
               COMPUTE WK-TOKUTEI-DAY      =   90  -   STUSAN-NISSU06
               IF      STUSAN-NISSU06      <   90
                   MOVE   "190117410"      TO
                                           LNK-SCNYUACCT-KAGEN-SRYCD(1)
                   MOVE   PTNYUINRRK-TENNYUYMD TO
                                           LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                                   (1)
                   INITIALIZE                    STS-AREA-DAY
                   INITIALIZE                      LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   COMPUTE LNK-DAY6-ZOGEN      =   WK-TOKUTEI-DAY  -  1
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN     TO
                                       LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
               END-IF
      *        一般入院料の算定開始日を算定
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    WK-TOKUTEI-DAY      TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN     TO  IPN-SANTEISTYMD
      *        特定入院期間（６０日経過）した際の一般入院料作成の
      *        可否を判定
               MOVE    PTNYUINRRK-TENNYUYMD
                                           TO  WRK-IPN-HANTEI-YMD1
               IF      WRK-IPN-HANTEI-M1   =   12
                   ADD     1               TO  WRK-IPN-HANTEI-Y1
                   MOVE    1               TO  WRK-IPN-HANTEI-M1
               ELSE
                   ADD     1               TO  WRK-IPN-HANTEI-M1
               END-IF
      *        月末日を算定
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY7-AREA
               MOVE    "71"            TO  LNK-DAY7-IRAI
               MOVE    WRK-IPN-HANTEI-YM1  TO  LNK-DAY7-YM
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
               MOVE    LNK-DAY7-KOYOMI     TO  WRK-IPN-HANTEI-YMD1
               MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                       TO  WRK-IPN-HANTEI-YMD2
      *          一般入院会計作成用のフラグ設定
               IF     (WRK-IPN-HANTEI-YMD1    >  WRK-IPN-HANTEI-YMD2)
                 AND  (WRK-IPN-HANTEI-YMD2(1:6) =  WRK-PARA-SRYYM)
                   MOVE    "1"         TO  IPN-NYUKAIKEI-SAKUSEI-FLG
               END-IF
           END-IF
      *
      *    亜急性期入院医療管理料２
           IF      SYOKIKSN-IN-KHNSRYCD    =   "190129910"
               MOVE   SPACE                TO  IPN-NYUKAIKEI-SAKUSEI-FLG
               MOVE   WRK-PARA-SRYYM       TO  PTNYUINRRK-TENNYUYMD(1:6)
               MOVE   "01"                 TO  PTNYUINRRK-TENNYUYMD(7:2)
               MOVE   SPACE                TO  SYOKIKSN-IN-KHNSRYCD
                                           LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *        亜急性期入院医療管理料２の算定可能日数を計算
               MOVE     ZERO               TO  WK-TOKUTEI-DAY
               COMPUTE  WK-TOKUTEI-DAY     =   60  -    STUSAN-NISSU06
               IF       STUSAN-NISSU06     <   60
                   MOVE   "190129910"          TO
                                           LNK-SCNYUACCT-KAGEN-SRYCD(1)
                   MOVE   PTNYUINRRK-TENNYUYMD TO
                                           LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                                   (1)
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE   PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   COMPUTE LNK-DAY6-ZOGEN      =   WK-TOKUTEI-DAY  -  1
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN     TO
                                           LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                    (1)
               END-IF
      *        一般入院料の算定開始日を算定
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE   PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               MOVE   WK-TOKUTEI-DAY       TO  LNK-DAY6-ZOGEN
               CALL   "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN     TO  IPN-SANTEISTYMD
      *        特定入院期間（６０日経過）した際の一般入院料作成の
      *        可否を判定
               MOVE    PTNYUINRRK-TENNYUYMD
                                           TO  WRK-IPN-HANTEI-YMD1
               IF      WRK-IPN-HANTEI-M1   =   12
                   ADD    1                TO  WRK-IPN-HANTEI-Y1
                   MOVE   1                TO  WRK-IPN-HANTEI-M1
               ELSE
                   ADD    1                TO  WRK-IPN-HANTEI-M1
               END-IF
      *        月末日を算定
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY7-AREA
               MOVE    "71"                TO  LNK-DAY7-IRAI
               MOVE   WRK-IPN-HANTEI-YM1   TO  LNK-DAY7-YM
               CALL   "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
               MOVE    LNK-DAY7-KOYOMI     TO  WRK-IPN-HANTEI-YMD1
               MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  WRK-IPN-HANTEI-YMD2
      *        一般入院会計作成用のフラグ設定
               IF      WRK-IPN-HANTEI-YMD1 >   WRK-IPN-HANTEI-YMD2
                   MOVE    "1"             TO  IPN-NYUKAIKEI-SAKUSEI-FLG
               END-IF
           END-IF
      *
      *    救命救急入院料３（広範囲熱傷特定集中治療）
           IF      SYOKIKSN-IN-KHNSRYCD    =   "190138410"   OR
                                               "190138510"   OR
                                               "190138610"
               MOVE   SPACE                TO  IPN-NYUKAIKEI-SAKUSEI-FLG
               MOVE   WRK-PARA-SRYYM       TO  PTNYUINRRK-TENNYUYMD(1:6)
               MOVE   "01"                 TO  PTNYUINRRK-TENNYUYMD(7:2)
               MOVE   SPACE                TO  SYOKIKSN-IN-KHNSRYCD
                                           LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *        救命救急入院料３（広範囲熱傷特定集中治療）の算定可能日数を計算
               MOVE     ZERO               TO  WK-TOKUTEI-DAY
               COMPUTE  WK-TOKUTEI-DAY     =   60  -    STUSAN-NISSU06
               IF       STUSAN-NISSU06     <   60
                   MOVE   "190138610"          TO
                                           LNK-SCNYUACCT-KAGEN-SRYCD(1)
                   MOVE   PTNYUINRRK-TENNYUYMD TO
                                           LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                                   (1)
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE   PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   COMPUTE LNK-DAY6-ZOGEN      =   WK-TOKUTEI-DAY  -  1
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN     TO
                                           LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                    (1)
               END-IF
      *        一般入院料の算定開始日を算定
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE   PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               MOVE   WK-TOKUTEI-DAY       TO  LNK-DAY6-ZOGEN
               CALL   "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN     TO  IPN-SANTEISTYMD
      *        特定入院期間（６０日経過）した際の一般入院料作成の
      *        可否を判定
               MOVE    PTNYUINRRK-TENNYUYMD
                                           TO  WRK-IPN-HANTEI-YMD1
               IF      WRK-IPN-HANTEI-M1   =   12
                   ADD    1                TO  WRK-IPN-HANTEI-Y1
                   MOVE   1                TO  WRK-IPN-HANTEI-M1
               ELSE
                   ADD    1                TO  WRK-IPN-HANTEI-M1
               END-IF
      *        月末日を算定
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY7-AREA
               MOVE    "71"                TO  LNK-DAY7-IRAI
               MOVE   WRK-IPN-HANTEI-YM1   TO  LNK-DAY7-YM
               CALL   "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
               MOVE    LNK-DAY7-KOYOMI     TO  WRK-IPN-HANTEI-YMD1
               MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  WRK-IPN-HANTEI-YMD2
      *        一般入院会計作成用のフラグ設定
               IF      WRK-IPN-HANTEI-YMD1 >   WRK-IPN-HANTEI-YMD2
                   MOVE    "1"             TO  IPN-NYUKAIKEI-SAKUSEI-FLG
               END-IF
           END-IF
      *
      *    救命救急入院料４（広範囲熱傷特定集中治療）
           IF      SYOKIKSN-IN-KHNSRYCD    =   "190139010"   OR
                                               "190139110"   OR
                                               "190139210"   OR
                                               "190139310"
               MOVE   SPACE                TO  IPN-NYUKAIKEI-SAKUSEI-FLG
               MOVE   WRK-PARA-SRYYM       TO  PTNYUINRRK-TENNYUYMD(1:6)
               MOVE   "01"                 TO  PTNYUINRRK-TENNYUYMD(7:2)
               MOVE   SPACE                TO  SYOKIKSN-IN-KHNSRYCD
                                           LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *        救命救急入院料４（広範囲熱傷特定集中治療）の算定可能日数を計算
               MOVE     ZERO               TO  WK-TOKUTEI-DAY
               COMPUTE  WK-TOKUTEI-DAY     =   60  -    STUSAN-NISSU06
               IF       STUSAN-NISSU06     <   60
                   MOVE   "190139310"          TO
                                           LNK-SCNYUACCT-KAGEN-SRYCD(1)
                   MOVE   PTNYUINRRK-TENNYUYMD TO
                                           LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                                   (1)
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE   PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   COMPUTE LNK-DAY6-ZOGEN      =   WK-TOKUTEI-DAY  -  1
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN     TO
                                           LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                    (1)
               END-IF
      *        一般入院料の算定開始日を算定
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE   PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               MOVE   WK-TOKUTEI-DAY       TO  LNK-DAY6-ZOGEN
               CALL   "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN     TO  IPN-SANTEISTYMD
      *        特定入院期間（６０日経過）した際の一般入院料作成の
      *        可否を判定
               MOVE    PTNYUINRRK-TENNYUYMD
                                           TO  WRK-IPN-HANTEI-YMD1
               IF      WRK-IPN-HANTEI-M1   =   12
                   ADD    1                TO  WRK-IPN-HANTEI-Y1
                   MOVE   1                TO  WRK-IPN-HANTEI-M1
               ELSE
                   ADD    1                TO  WRK-IPN-HANTEI-M1
               END-IF
      *        月末日を算定
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY7-AREA
               MOVE    "71"                TO  LNK-DAY7-IRAI
               MOVE   WRK-IPN-HANTEI-YM1   TO  LNK-DAY7-YM
               CALL   "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
               MOVE    LNK-DAY7-KOYOMI     TO  WRK-IPN-HANTEI-YMD1
               MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  WRK-IPN-HANTEI-YMD2
      *        一般入院会計作成用のフラグ設定
               IF      WRK-IPN-HANTEI-YMD1 >   WRK-IPN-HANTEI-YMD2
                   MOVE    "1"             TO  IPN-NYUKAIKEI-SAKUSEI-FLG
               END-IF
           END-IF
      *
      *    特定集中治療室管理料２（広範囲熱傷特定集中治療）
           IF      SYOKIKSN-IN-KHNSRYCD    =   "190140010"   OR
                                               "190140110"
               MOVE   SPACE                TO  IPN-NYUKAIKEI-SAKUSEI-FLG
               MOVE   WRK-PARA-SRYYM       TO  PTNYUINRRK-TENNYUYMD(1:6)
               MOVE   "01"                 TO  PTNYUINRRK-TENNYUYMD(7:2)
               MOVE   SPACE                TO  SYOKIKSN-IN-KHNSRYCD
                                           LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *        特定集中治療室管理料２（広範囲熱傷特定集中治療）の算定可能日数を計算
               MOVE     ZERO               TO  WK-TOKUTEI-DAY
               COMPUTE  WK-TOKUTEI-DAY     =   60  -    STUSAN-NISSU06
               IF       STUSAN-NISSU06     <   60
                   MOVE   "190140110"          TO
                                           LNK-SCNYUACCT-KAGEN-SRYCD(1)
                   MOVE   PTNYUINRRK-TENNYUYMD TO
                                           LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                                   (1)
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE   PTNYUINRRK-TENNYUYMD
                                               TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   COMPUTE LNK-DAY6-ZOGEN      =   WK-TOKUTEI-DAY  -  1
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN     TO
                                           LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                    (1)
               END-IF
      *        一般入院料の算定開始日を算定
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE   PTNYUINRRK-TENNYUYMD
                                           TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               MOVE   WK-TOKUTEI-DAY       TO  LNK-DAY6-ZOGEN
               CALL   "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN     TO  IPN-SANTEISTYMD
      *        特定入院期間（６０日経過）した際の一般入院料作成の
      *        可否を判定
               MOVE    PTNYUINRRK-TENNYUYMD
                                           TO  WRK-IPN-HANTEI-YMD1
               IF      WRK-IPN-HANTEI-M1   =   12
                   ADD    1                TO  WRK-IPN-HANTEI-Y1
                   MOVE   1                TO  WRK-IPN-HANTEI-M1
               ELSE
                   ADD    1                TO  WRK-IPN-HANTEI-M1
               END-IF
      *        月末日を算定
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY7-AREA
               MOVE    "71"                TO  LNK-DAY7-IRAI
               MOVE   WRK-IPN-HANTEI-YM1   TO  LNK-DAY7-YM
               CALL   "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
               MOVE    LNK-DAY7-KOYOMI     TO  WRK-IPN-HANTEI-YMD1
               MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  WRK-IPN-HANTEI-YMD2
      *        一般入院会計作成用のフラグ設定
               IF      WRK-IPN-HANTEI-YMD1 >   WRK-IPN-HANTEI-YMD2
                   MOVE    "1"             TO  IPN-NYUKAIKEI-SAKUSEI-FLG
               END-IF
           END-IF
      *
      *    精神科救急入院料
           IF      SYOKIKSN-IN-KHNSRYCD    =   "190121010"
               MOVE   SPACE            TO  SYOKIKSN-IN-KHNSRYCD
               MOVE   "190111410"      TO  LNK-SCNYUACCT-NYUKHN-SRYCD
                                                                    (1)
               MOVE   "190121010"      TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
               MOVE   PTNYUINRRK-TENNYUYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                                    (1)
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE    29              TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                   (1)
      *
               MOVE   "190111410"      TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                       TO  LNK-DAY6-KIJUN
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE     1              TO  LNK-DAY6-ZOGEN
               CALL  "ORCSDAY"         USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                                  (2)
               MOVE    "99999999"      TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                  (2)
           END-IF
      *
      *    精神科急性期治療病棟入院料１
           IF      SYOKIKSN-IN-KHNSRYCD    =   "190121110"
               MOVE   SPACE            TO  SYOKIKSN-IN-KHNSRYCD
               MOVE   "190066910"      TO  LNK-SCNYUACCT-NYUKHN-SRYCD
                                                                    (1)
               MOVE   "190121110"      TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
               MOVE   PTNYUINRRK-TENNYUYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                                    (1)
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE    29              TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                   (1)
      *
               MOVE   "190066910"      TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                       TO  LNK-DAY6-KIJUN
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE     1              TO  LNK-DAY6-ZOGEN
               CALL  "ORCSDAY"         USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                                  (2)
               MOVE    "99999999"      TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                  (2)
           END-IF
      *
      *    精神科急性期治療病棟入院料２
           IF      SYOKIKSN-IN-KHNSRYCD    =   "190121210"
               MOVE   SPACE            TO  SYOKIKSN-IN-KHNSRYCD
               MOVE   "190067010"      TO  LNK-SCNYUACCT-NYUKHN-SRYCD
                                                                    (1)
               MOVE   "190121210"      TO  LNK-SCNYUACCT-KAGEN-SRYCD(1)
               MOVE   PTNYUINRRK-TENNYUYMD
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                                    (1)
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE    29              TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                   (1)
      *
               MOVE   "190067010"      TO  LNK-SCNYUACCT-KAGEN-SRYCD(2)
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                       TO  LNK-DAY6-KIJUN
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE     1              TO  LNK-DAY6-ZOGEN
               CALL  "ORCSDAY"         USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                                  (2)
               MOVE    "99999999"      TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                  (2)
           END-IF
      *
      *    入院料加算診療コード取り込み
           PERFORM    410191-NYUKSN-GET-SEC
      *
      *    特定入院料区分のセット
           IF      WRK-TOKUTEI-NYUIN   =   SPACE
               MOVE     "0"        TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
           ELSE
               MOVE     "1"        TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
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
                                                       "190121510" OR
                                                       "190123710" OR
                                                       "190123810" OR
                                                       "190123910" OR
                                                       "190124010" OR
                                                       "190124110" OR
                                                       "190124210" OR
                                                       "190124310" OR
                                                       "190124410" OR
                                                       "190124510" OR
                                                       "190124610" OR
                                                       "190124710" OR
                                                       "190124810" OR
                                                       "190131610" OR
                                                       "190131710" OR
                                                       "190131810" OR
                                                       "190131910" OR
                                                       "190132010" OR
                                                       "190132110" OR
                                                       "190132210" OR
                                                       "190132310" OR
                                                       "190132410" OR
                                                       "190132510" OR
                                                       "190132610" OR
                                                       "190132710" OR
                                                       "190132810" OR
                                                       "190132910" OR
                                                       "190133010" OR
                                                       "190133110" OR
                                                       "190133210" OR
                                                       "190133310" OR
                                                       "190133410" OR
                                                       "190133510" OR
                                                       "190133610" OR
                                                       "190133710" OR
                                                       "190133810" OR
                                                       "190133910" OR
                                                       "190134010" OR
                                                       "190134110"
                   MOVE  "0"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
               END-IF
           END-IF
      *    外泊
           MOVE   "099999911"      TO  LNK-SCNYUACCT-GAIHAKU-SRYCD
      *    室料差額
           MOVE   "099999912"      TO  LNK-SCNYUACCT-SAGAKU-SRYCD
           IF     PTNYUINRRK-RM-SAGAKU =   SPACE
               MOVE    ZERO            TO  LNK-SCNYUACCT-SAGAKU-KBN
           ELSE
               MOVE   PTNYUINRRK-RM-SAGAKU 
                                       TO  WRK-SAGAKU-KBN
               MOVE   WRK-SAGAKU-KBN9  TO  LNK-SCNYUACCT-SAGAKU-KBN
           END-IF
      *    食事区分
           MOVE    "099999913"         TO  LNK-SCNYUACCT-SHOKUJI-SRYCD
           MOVE    0                   TO  LNK-SCNYUACCT-SHOKUJIKBN
      *    食堂加算
           PERFORM 4801-BTUSEL-SEC
           IF      FLG-SYSKANRI        =    ZERO
               IF      SYS-5001-BTU-SYOKUDOKBN     =    "2"
                   MOVE    "197000570" TO  LNK-SCNYUACCT-SHOKUDO-SRYCD
                   MOVE    1           TO  LNK-SCNYUACCT-SHOKUDO-KBN
               END-IF
           END-IF
      *    保険組合せ
           MOVE    "099999914"     TO  LNK-SCNYUACCT-HKNCOMBINUM-SRYCD
           MOVE    0               TO  LNK-SCNYUACCT-HKNCOMBINUM
      *    転科元入院科
           MOVE   "00"             TO  LNK-SCNYUACCT-MOTO-NYUINKA
           MOVE    SPA-SRYKA       TO  WRK-TAIHI-SRYKA
           MOVE    "00"            TO  SPA-SRYKA
      *
      *     DISPLAY   "****4801SENTEI-SET-SEC MAE **** "
           IF     PTNYUINRRK-SENTEIKBN  =   "1"
               PERFORM 4801-SENTEI-SET-SEC
           END-IF
      *    入院カレンダー未設定の判定
           IF     PTNYUINRRK-NYUCALKBN   =   "1"
                  MOVE   1         TO  LNK-SCNYUACCT-NYUCAL-KBN
           ELSE
                  MOVE   ZERO      TO  LNK-SCNYUACCT-NYUCAL-KBN
           END-IF
      *
      *     DISPLAY   "********ORCSNYUACCT MAE **********"
           MOVE    WRK-PARA-HOSPNUM TO  SPA-HOSPNUM
           MOVE    WRK-PTID        TO  SPA-PTID
           MOVE    "00"            TO  SPA-SRYKA
           MOVE    "1"             TO  SPA-NYUGAIKBN
           DISPLAY "SPA-HOSPNUM = " SPA-HOSPNUM
           DISPLAY "SPA-PTID = " SPA-PTID
           CALL    "ORCSNYUACCT"       USING   SPA-AREA
                                               ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA      TO  SPA-SRYKA
      *
      *    入院会計作成後に退避データの食事区分を復元
           IF WRK-PARA-SRYYM        >= "200604"
      *       （更新月分）
              PERFORM    VARYING  IDX-SKJ-TAIHI  FROM  1  BY  1
                 UNTIL    IDX-SKJ-TAIHI   >   3
                 INITIALIZE                      NYUINACCT-REC
                 MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
                 MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
                 MOVE    WRK-PARA-SRYYM      TO  NACCT-SRYYM
      *
                 MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                 EVALUATE   IDX-SKJ-TAIHI
                     WHEN    1
                         MOVE    "key30"     TO  MCP-PATHNAME
                     WHEN    2
                         MOVE    "key31"     TO  MCP-PATHNAME
                     WHEN    3
                         MOVE    "key32"     TO  MCP-PATHNAME
                 END-EVALUATE
                 PERFORM  910-NYUINACCT-SELECT-SEC
      *
                 MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
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
      *              食事を復元
                     EVALUATE  IDX-SKJ-TAIHI
                         WHEN  1
                          PERFORM VARYING IDX-SKJ FROM 1 BY 1
                            UNTIL     IDX-SKJ   >   31
                               MOVE   WRK-TAIHI-SKJ18(IDX-SKJ)
                                             TO  NACCT-DAY(IDX-SKJ)
                          END-PERFORM
                         WHEN  2
                          PERFORM VARYING IDX-SKJ FROM 1 BY 1
                            UNTIL     IDX-SKJ   >   31
                               MOVE   WRK-TAIHI-SKJ19(IDX-SKJ)
                                             TO  NACCT-DAY(IDX-SKJ)
                          END-PERFORM
                         WHEN  3
                          PERFORM VARYING IDX-SKJ FROM 1 BY 1
                            UNTIL     IDX-SKJ   >   31
                               MOVE   WRK-TAIHI-SKJ20(IDX-SKJ)
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
      *              入院会計更新
                     MOVE     NYUINACCT-REC      TO  MCPDATA-REC
                     MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
                     MOVE     "key"              TO  MCP-PATHNAME
                     PERFORM 910-DBUPDATE-SEC
                 END-IF
      *          入院会計マスタクローズ
                 MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
                 EVALUATE   IDX-SKJ-TAIHI
                     WHEN    1
                         MOVE    "key30"     TO  MCP-PATHNAME
                     WHEN    2
                         MOVE    "key31"     TO  MCP-PATHNAME
                     WHEN    3
                         MOVE    "key32"     TO  MCP-PATHNAME
                 END-EVALUATE
                 PERFORM 900-CLOSE-SEC

              END-PERFORM
           END-IF
      *
      *    入院会計作成後に退避データの外泊区分を復元
      *    （更新月分）
           INITIALIZE                      NYUINACCT-REC
           MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    WRK-PARA-SRYYM      TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM  910-NYUINACCT-SELECT-SEC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM  910-NYUINACCT-READ-SEC
           IF      FLG-NYUINACCT        =   ZERO
      *        外泊を復元
               PERFORM   VARYING   IDX-GAIHAKU FROM  1  BY  1
                         UNTIL     IDX-GAIHAKU   >   31
                     MOVE   WRK-TAIHI-GAIHAKU(IDX-GAIHAKU)
                                       TO  NACCT-DAY(IDX-GAIHAKU)
               END-PERFORM
      *        剤回数再セット
               MOVE  ZERO              TO  NACCT-ZAIKAISU
               PERFORM  VARYING   IDX-GAIHAKU FROM  1  BY  1
                        UNTIL     IDX-GAIHAKU   >   31
                   IF   NACCT-DAY(IDX-GAIHAKU)   NOT =  0
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
           PERFORM  900-CLOSE-SEC
      *
      *    入院会計作成後に退避データの保険組合せを復元
      *    （更新月分）
           INITIALIZE                      NYUINACCT-REC
           MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    WRK-PARA-SRYYM      TO  NACCT-SRYYM
           MOVE    "5"                 TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM  910-NYUINACCT-SELECT-SEC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM  910-NYUINACCT-READ-SEC
           IF      FLG-NYUINACCT        =   ZERO
      *        保険組合せを復元
               PERFORM   VARYING   IDX-HKN FROM  1  BY  1
                         UNTIL     IDX-HKN   >   31
                     MOVE   WRK-TAIHI-HKN(IDX-HKN)
                                       TO  NACCT-DAY(IDX-HKN)
               END-PERFORM
      *        剤回数再セット
               MOVE  ZERO              TO  NACCT-ZAIKAISU
               PERFORM  VARYING   IDX-HKN FROM  1  BY  1
                        UNTIL     IDX-HKN   >   31
                   IF   NACCT-DAY(IDX-HKN)   NOT =  0
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
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
      *     入院行為照会用テーブル作成サブ呼び出し
            INITIALIZE                  SS010-AREA
            MOVE    PTNYUINRRK-PTID     TO  SS010-PTID
            MOVE    WRK-PARA-SRYYM      TO  SS010-SRYYM
            CALL    "ORCSS010"      USING   SS010-AREA
                                            SPA-AREA
            IF    ( SS010-RC        NOT =   ZERO )
                MOVE    "0052"          TO  WRK-ERRCD
            END-IF
      *
           .
      *
       410191-NYUACCT-SAKUSEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計テーブル作成処理（特定入院料後の一般入院料）
      *****************************************************************
       410191-IPNNYUACCT-SAKUSEI-SEC   SECTION.
      *
           INITIALIZE      ORCSSYOKIKSNAREA
           MOVE    IPN-SANTEISTYMD(1:6)  TO WRK-NAI-SRYYM-41(1:6)
      *
      *    通算日数を求める（入院履歴を検索）
           MOVE    ZERO                TO  WRK-TSUSAN
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    WRK-PARA-HOSPNUM    TO  PTNYUINRRK-HOSPNUM
           MOVE    WRK-PTID            TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =  ZERO
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
               MOVE    PTNYUINRRK-SHONUM
                                       TO  WRK-SHONUM
               MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  WRK-SANTEISTYMD
               MOVE    PTNYUINRRK-REC  TO  WK-PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    通算日数を求める（通算日数取得サブを使用）
           PERFORM 4801-TSUSAN-SANTEI-GET-SEC
           IF    ( WRK-ERRCD    NOT =  SPACE )
               GO  TO  410191-IPNNYUACCT-SAKUSEI-EXT
           END-IF
      *
           MOVE    WK-PTNYUINRRK-REC   TO  PTNYUINRRK-REC
      *
           MOVE    WRK-TSUSAN          TO  SYOKIKSN-IN-TUSANNISSU
           MOVE    IPN-SANTEISTYMD     TO  SYOKIKSN-IN-SANTEISTYMD
      *
           MOVE    SPACE               TO  WRK-TOKUTEI-NYUIN
           IF      WRK-TOKUTEI-NYUIN   NOT =   SPACE
               PERFORM   4801-TOKTEI-NYUIN-SET-SEC
           ELSE
               PERFORM   4801-BTUSEL-SEC
               IF  SYS-5001-BTU-KHNSRYCD   NOT =   SPACE
                   MOVE    SYS-5001-BTU-KHNSRYCD
                                           TO  SYOKIKSN-IN-KHNSRYCD
                   PERFORM 4801-RJNCD-CHG-SEC
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "2006"          TO  WRK-ERRCD
                       GO  TO  410191-NYUACCT-SAKUSEI-EXT
                   END-IF
               ELSE
                   MOVE    "2006"          TO  WRK-ERRCD
                   GO  TO  410191-NYUACCT-SAKUSEI-EXT
               END-IF
           END-IF
      *
      *    入院会計作成サブの呼出し
           INITIALIZE                      ORCSNYUACCT-AREA
           MOVE    "9"                 TO  LNK-SCNYUACCT-SYORI-KBN
           MOVE    "1"                 TO  SPA-NYUGAIKBN
           MOVE    PTNYUINRRK-PTID     TO  SPA-PTID
           MOVE    PTNYUINRRK-NYUINKA  TO  SPA-SRYKA
           MOVE    SYOKIKSN-IN-KHNSRYCD
                                       TO LNK-SCNYUACCT-NYUKHN-SRYCD(1)
           MOVE      SYOKIKSN-IN-SANTEISTYMD
                                       TO  LNK-SCNYUACCT-STSRYYMD
                                           LNK-SCNYUACCT-EDSRYYMD
           PERFORM VARYING     IDX6    FROM    1   BY   1
               UNTIL          (IDX6        >   7   )   OR
                              (SYOKIKSN-OT-KGNSRYCD(IDX6)  =  SPACE)
               MOVE    SYOKIKSN-OT-KGNSRYCD(IDX6)
                                       TO  LNK-SCNYUACCT-KAGEN-SRYCD
                                                             (IDX6)
               MOVE   SYOKIKSN-OT-YUKOSTYMD(IDX6)
                                       TO  LNK-SCNYUACCT-KAGEN-STYUKYMD
                                                              (IDX6)
               MOVE   SYOKIKSN-OT-YUKOEDYMD(IDX6)
                                       TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD
                                                              (IDX6)
           END-PERFORM
      *    通算算定済日数
           MOVE    WRK-TSUSAN          TO  LNK-SCNYUACCT-NYUKHN-NISUU
      *    入院料加算診療コード取り込み
           PERFORM 410191-NYUKSN-GET-SEC
      *    特定入院料区分のセット
           MOVE    "0"             TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
      *    外泊
           MOVE    "099999911"     TO  LNK-SCNYUACCT-GAIHAKU-SRYCD
      *    室料差額
           MOVE    "099999912"     TO  LNK-SCNYUACCT-SAGAKU-SRYCD
           IF     PTNYUINRRK-RM-SAGAKU =   SPACE
               MOVE    ZERO            TO  LNK-SCNYUACCT-SAGAKU-KBN
           ELSE
               MOVE   PTNYUINRRK-RM-SAGAKU 
                                       TO  WRK-SAGAKU-KBN
               MOVE   WRK-SAGAKU-KBN9  TO  LNK-SCNYUACCT-SAGAKU-KBN
           END-IF
      *    食事区分
           MOVE    "099999913"         TO  LNK-SCNYUACCT-SHOKUJI-SRYCD
           MOVE     0                  TO  LNK-SCNYUACCT-SHOKUJIKBN
      *    食堂加算
           PERFORM 4801-BTUSEL-SEC
           IF      FLG-SYSKANRI           =    ZERO
               IF      SYS-5001-BTU-SYOKUDOKBN     =    "2"
                   MOVE    "197000570" TO  LNK-SCNYUACCT-SHOKUDO-SRYCD
                   MOVE    1           TO  LNK-SCNYUACCT-SHOKUDO-KBN
               END-IF
           END-IF
      *    保険組合せ
           MOVE    "099999914"     TO  LNK-SCNYUACCT-HKNCOMBINUM-SRYCD
           MOVE    0               TO  LNK-SCNYUACCT-HKNCOMBINUM
      *    転科元入院科
           MOVE    "00"            TO  LNK-SCNYUACCT-MOTO-NYUINKA
           MOVE    SPA-SRYKA       TO  WRK-TAIHI-SRYKA
           MOVE    "00"            TO  SPA-SRYKA
      **   
           IF     PTNYUINRRK-SENTEIKBN  =   "1"
               PERFORM 4801-SENTEI-SET-SEC
           END-IF
      *    入院カレンダー未設定の判定
           IF     PTNYUINRRK-NYUCALKBN   =   "1"
                  MOVE   1         TO  LNK-SCNYUACCT-NYUCAL-KBN
           ELSE
                  MOVE   ZERO      TO  LNK-SCNYUACCT-NYUCAL-KBN
           END-IF
      *
           MOVE    WRK-PARA-HOSPNUM TO  SPA-HOSPNUM
           MOVE    WRK-PTID        TO  SPA-PTID
           MOVE    "00"            TO  SPA-SRYKA
           MOVE    "1"             TO  SPA-NYUGAIKBN
           CALL   "ORCSNYUACCT"    USING   SPA-AREA
                                           ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA  TO  SPA-SRYKA
      *
      *     入院行為照会用テーブル作成サブ呼び出し
            INITIALIZE                  SS010-AREA
            MOVE    PTNYUINRRK-PTID     TO  SS010-PTID
            MOVE    SYOKIKSN-IN-SANTEISTYMD(1:6)
                                        TO  SS010-SRYYM
            CALL    "ORCSS010"      USING   SS010-AREA
                                            SPA-AREA
            IF    ( SS010-RC        NOT =   ZERO )
                MOVE    "0052"          TO  WRK-ERRCD
            END-IF
      *
      *
           .
      *
       410191-IPNNYUACCT-SAKUSEI-EXT.
           EXIT.
      *021118
      *****************************************************************
      *    通算日数取得処理
      *****************************************************************
       4801-TSUSAN-SANTEI-GET-SEC      SECTION.
      *
           INITIALIZE                  WRK-TSUSAN
                                       WRK-SANTEISTYMD
           MOVE    SPACE           TO  WRK-ERRCD
      *
           INITIALIZE                  ORCSTUSANAREA
           MOVE    WRK-PARA-HOSPNUM  TO  STUSAN-HOSPNUM
           MOVE    PTNYUINRRK-PTID TO  STUSAN-PTID
           MOVE    WRK-PARA-SRYYM  TO  STUSAN-KJNYMD(1:6)
           IF   IPN-NYUKAIKEI-SAKUSEI-FLG  =   "1"
               MOVE   IPN-SANTEISTYMD(7:2) TO  STUSAN-KJNYMD(7:2)
           ELSE
               MOVE    "01"                TO  STUSAN-KJNYMD(7:2)
           END-IF
      *
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF    ( STUSAN-RC       NOT =   ZERO )
               MOVE    "2003"      TO  WRK-ERRCD
               GO  TO  4801-TSUSAN-SANTEI-GET-EXT
           END-IF
      *
      *    他院（特別な関係にある医療機関）の日数を加算
           ADD     STUSAN-TAINREL-NISSU  TO   STUSAN-NISSU03
           ADD     STUSAN-TAINREL-NISSU  TO   STUSAN-NISSU05
      *
           COMPUTE WRK-TSUSAN      =   STUSAN-NISSU03  -   1
           MOVE    PTNYUINRRK-TENNYUYMD
                                   TO  WRK-SANTEISTYMD
      *
           MOVE    STUSAN-KJNYMD   TO  WRK-YMD
           MOVE    "1"             TO  WRK-ZOGENPTN
           COMPUTE WRK-ZOGEN       =   180 - ( STUSAN-NISSU04 )
           PERFORM 5002-CALENDAR-SEC
           MOVE    WRK-KEISANYMD   TO  WRK-SENTEI-STYMD
      *
           .
       4801-TSUSAN-SANTEI-GET-EXT.
           EXIT.
      *
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
      *
      *****************************************************************
      *     選定療養費診療行為コード取得処理
      *****************************************************************
       4801-SENTEI-SET-SEC         SECTION.
      *
           INITIALIZE                  SENTEICDCHG-REC
           MOVE    LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                       TO  SENCHG-IPNSRYCD
           MOVE    SENTEICDCHG-REC     TO  MCPDATA-REC
           MOVE    "tbl_senteicdchg"   TO  WRK-MCP-TABLE
           MOVE    "key2"              TO  WRK-MCP-PATHNAME
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    =   ZERO  )
               MOVE    MCPDATA-REC     TO  SENTEICDCHG-REC
           ELSE
               INITIALIZE                  SENTEICDCHG-REC
           END-IF
      *
           MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      **     MOVE    SENCHG-SENTEISRYCD
      **                               TO  LNK-SCNYUACCT-SENTEI-SRYCD
           IF      SENCHG-SENTEISRYCD  NOT =  SPACE
               MOVE    "099999916"     TO  LNK-SCNYUACCT-SENTEI-SRYCD
               MOVE    WRK-SENTEI-STYMD
                                       TO  LNK-SCNYUACCT-SENTEI-STYMD
           END-IF
           MOVE    WRK-SENTEI-STYMD    TO  LNK-SCNYUACCT-SENTEI-STYMD
      *    １５才未満の患者は選定入院対象外
           MOVE    WRK-PARA-HOSPNUM    TO  PTINF-HOSPNUM
           MOVE    WRK-PTID            TO  PTINF-PTID
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-PTINF-SELECT-SEC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-PTINF-READ-SEC
           MOVE     PTINF-BIRTHDAY     TO  WRK-BIRTHDAY-15
                                           SPA-BIRTHDAY
           ADD      15                 TO  WRK-BIRTHDAY-15Y
           IF       WRK-SENTEI-STYMD   <=  WRK-BIRTHDAY-15
               MOVE   WRK-BIRTHDAY-15  TO  LNK-SCNYUACCT-SENTEI-STYMD
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *
           .
       4801-SENTEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    期間算出処理
      *****************************************************************
       410191-KIKAN-CALC-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-NISSU1
                                           WRK-NISSU2
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY5-AREA
           MOVE    "51"                TO  LNK-DAY5-IRAI
           MOVE    WRK-STYMD           TO  LNK-DAY5-START
           MOVE    WRK-EDYMD           TO  LNK-DAY5-END
           CALL  "ORCSDAY"             USING
                                           STS-AREA-DAY
                                           LNK-DAY5-AREA
      *
           MOVE    LNK-DAY5-NISSU1     TO  WRK-NISSU1
           MOVE    LNK-DAY5-NISSU2     TO  WRK-NISSU2
      *
           .
       410191-KIKAN-CALC-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定入院料セット処理
      *****************************************************************
       4801-TOKTEI-NYUIN-SET-SEC   SECTION.
      *
      *    特定入院料コードをサブプロより取得
           INITIALIZE                           ORCSTOKNYUIN-AREA
           MOVE    PTNYUINRRK-TOKUTEI-KBN   TO  LNK-TOKNYUIN-KBN
           MOVE    PTNYUINRRK-TOKUTEI-NYUIN TO  LNK-TOKNYUIN-NYUIN
           MOVE    SYOKIKSN-IN-SANTEISTYMD(1:6)
                                            TO  LNK-TOKNYUIN-SANTEIYM
           CALL    "ORCSTOKNYUIN"      USING    ORCSTOKNYUIN-AREA
                                                SPA-AREA
           IF      LNK-TOKNYUIN-RC          =   ZERO
               MOVE   LNK-TOKNYUIN-SRYCD    TO  SYOKIKSN-IN-KHNSRYCD
           END-IF
      *
           .
       4801-TOKTEI-NYUIN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料加算の取り込み処理
      *****************************************************************
       410191-NYUKSN-GET-SEC         SECTION.
      *
           INITIALIZE                       WRK-NYUKHNKSN-AREA
           MOVE    ZERO                TO   IDX8
      *
      *医療機関加算（減算）情報
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    WRK-PARA-SRYYM      TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
      *
      *    システム管理マスタの医療機関情報より取得
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key2"              TO  WRK-MCP-PATHNAME
           PERFORM 900-TBL-SELECT-SEC
      *
           IF      FLG-DBRC            =   ZERO
               MOVE    MCPDATA-REC         TO   SYS-5000-REC
      *        入院料加算
               PERFORM VARYING     IDX7    FROM   1  BY  1
                   UNTIL          (IDX7           >   20)  OR
                                  (SYS-5000-KSNCD(IDX7)  =   SPACE)
                   IF      SYS-5000-KSNKBN(IDX7)   =   "2"
                       ADD    1                    TO  IDX8
                       MOVE   SYS-5000-KSNCD(IDX7) TO
                                             WRK-NYUKHNKSN-SRYCD(IDX8)
                   END-IF
               END-PERFORM
      *        地域加算
               IF      SYS-5000-CHIIKIKSN  NOT =   SPACE
                    ADD    1               TO  IDX8
                    MOVE   SYS-5000-CHIIKIKSN  TO
                                           WRK-NYUKHNKSN-SRYCD(IDX8)
               END-IF
           END-IF
      *
      *    システム管理マスタクローズ
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *病棟加算情報
           PERFORM 4801-BTUSEL-SEC
      *
           IF      FLG-SYSKANRI        =   ZERO
      *
      *        看護補助加算
               IF      SYS-5001-BTU-KNGHJOKSN  NOT =   SPACE
                   ADD     1               TO     IDX8
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
      *      病棟入院料加算
      *
      *      平成２０年３月までの加算セット処理
             IF     WRK-PARA-SRYYM     <=     "200803"
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
      *      平成２０年４月から平成２２年３月までの加算セット処理
             IF    (WRK-PARA-SRYYM   >=     "200804")    AND
                   (WRK-PARA-SRYYM   <=     "201003")
                 PERFORM   VARYING   IDX7   FROM   1  BY  1
                   UNTIL  (IDX7      >   14)
                     IF   SYS-5001-KSNKBN(IDX7)   =   "2"
                         EVALUATE    SYS-5001-KSNCD(IDX7)
      *                      児童思春期精神科入院医療管理加算（２０才未満）
                             WHEN    "190110170"
                                PERFORM   4801-PT-AGE-SEC
                                IF      WRK-NENREI   <      20
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      重症患者回復病棟加算（回復期リハビリテーション入院料１）
                             WHEN    "190129870"
                                IF  SYOKIKSN-IN-KHNSRYCD = "190129410"
                                                        OR "190129510"  
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      医師配置適合加算（有床診療所入院基本料１）
      *                      看護配置加算１（有床診療所入院基本料１）
      *                      看護配置加算２（有床診療所入院基本料１）
      *                      夜間看護配置加算１（有床診療所入院基本料１）
      *                      夜間看護配置加算２（有床診療所入院基本料１）
                             WHEN    "190117070"
                             WHEN    "190126570"
                             WHEN    "190109670"
                             WHEN    "190126670"
                             WHEN    "190126770"
                                IF  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                                         = "190097010"
                                                        OR "190097110"
                                                        OR "190119510"
                                                        OR "190119610"
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      夜間緊急体制確保加算（有床診療所入院基本料１と２）
                             WHEN    "190126470"
                                IF  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                                         = "190097010"
                                                        OR "190097110"
                                                        OR "190119510"
                                                        OR "190119610"
                                                        OR "190097210"
                                                        OR "190097310"
                                                        OR "190119710"
                                                        OR "190119810"
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
                             WHEN   OTHER
                                ADD    1                TO  IDX8
                                MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                         END-EVALUATE
                     END-IF
                 END-PERFORM
             END-IF
      *
      *      平成２２年４月から平成２４年３月までの加算セット処理
             IF    (WRK-PARA-SRYYM   >=     "201004")    AND
                   (WRK-PARA-SRYYM   <=     "201203")
      *         （システム管理の病棟設定見直しによるもの）
               PERFORM   VARYING   IDX7   FROM   1  BY  1
                 UNTIL  (IDX7      >   14)
                     IF   SYS-5001-KSNKBN(IDX7)   =   "2"
                         EVALUATE    SYS-5001-KSNCD(IDX7)
      *                      児童思春期精神科入院医療管理加算（２０才未満）
                             WHEN    "190110170"
                                PERFORM   4801-PT-AGE-SEC
                                IF      WRK-NENREI   <      20
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      重症患者回復病棟加算（回復期リハビリテーション入院料１）
                             WHEN    "190129870"
                                IF  SYOKIKSN-IN-KHNSRYCD = "190129410"
                                                        OR "190129510"
                                                        OR "190141610"
                                                        OR "190141710"
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
      *                            基準未適合の場合は重症患者回復病棟加算（基準未適合）
      *                            に置き換える
                                   IF SYOKIKSN-IN-KHNSRYCD = "190129410"
                                                          OR "190129510"
                                     MOVE   "190142370"  TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                   END-IF
                                END-IF
      *                      医師配置加算１（有床診療所入院基本料１と２）
      *                      医師配置加算２（有床診療所入院基本料１と２）
      *                      看護配置加算１（有床診療所入院基本料１と２）
      *                      看護配置加算２（有床診療所入院基本料１と２）
      *                      夜間看護配置加算１（有床診療所入院基本料１と２）
      *                      夜間看護配置加算２（有床診療所入院基本料１と２）
                             WHEN    "190135570"
                             WHEN    "190117070"
                             WHEN    "190109670"
                             WHEN    "190126570"
                             WHEN    "190126770"
                             WHEN    "190126670"
                                IF  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                                         = "190097010"
                                                        OR "190119510"
                                                        OR "190119610"
                                                        OR "190097210"
                                                        OR "190119710"
                                                        OR "190119810"
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      夜間緊急体制確保加算（有床診療所入院基本料１と２と３）
                             WHEN    "190126470"
                                IF  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                                         = "190097010"
                                                        OR "190119510"
                                                        OR "190119610"
                                                        OR "190097210"
                                                        OR "190119710"
                                                        OR "190119810"
                                                        OR "190135110"
                                                        OR "190135210"
                                                        OR "190135310"
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      一般病棟看護必要度評価加算（１０対１入院基本料）
                             WHEN    "190131570"
      *                            一般病棟看護必要度評価加算
                                   IF SYOKIKSN-IN-KHNSRYCD = "190077410"
                                     ADD    1            TO  IDX8
                                     MOVE   "190131570"  TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                   END-IF
      *                            一般病棟看護必要度評価加算（特定機能病院）
                                   IF SYOKIKSN-IN-KHNSRYCD = "190085910"
                                     ADD    1            TO  IDX8
                                     MOVE   "190134870"  TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                   END-IF
      *                            一般病棟看護必要度評価加算（専門病院）
                                   IF SYOKIKSN-IN-KHNSRYCD = "190090210"
                                     ADD    1            TO  IDX8
                                     MOVE   "190134970"  TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                   END-IF
      *                      休日リハビリテーション提供体制加算（回復期リハビリテーション病棟入院料）
                             WHEN    "190140870"
                                IF  SYOKIKSN-IN-KHNSRYCD = "190141610"
                                                        OR "190141710"
                                                        OR "190141810"
                                                        OR "190141910"
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
                             WHEN   OTHER
                                ADD    1                TO  IDX8
                                MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                         END-EVALUATE
                     END-IF
               END-PERFORM
             END-IF
      *
      *      平成２４年４月からの加算セット処理
             IF     WRK-PARA-SRYYM      >=     "201204"
      *         （システム管理の病棟設定見直しによるもの）
               PERFORM   VARYING   IDX7   FROM   1  BY  1
                 UNTIL  (IDX7      >   14)
                     IF   SYS-5001-KSNKBN(IDX7)   =   "2"
                         EVALUATE    SYS-5001-KSNCD(IDX7)
      *                      医師配置加算１（有床診療所入院基本料１と２）
      *                      医師配置加算２（有床診療所入院基本料１と２）
      *                      看護配置加算１（有床診療所入院基本料１と２）
      *                      看護配置加算２（有床診療所入院基本料１と２）
      *                      夜間看護配置加算１（有床診療所入院基本料１と２）
      *                      夜間看護配置加算２（有床診療所入院基本料１と２）
                             WHEN    "190135570"
                             WHEN    "190117070"
                             WHEN    "190109670"
                             WHEN    "190126570"
                             WHEN    "190126770"
                             WHEN    "190126670"
                                IF  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                                         = "190097010"
                                                        OR "190119510"
                                                        OR "190119610"
                                                        OR "190097210"
                                                        OR "190119710"
                                                        OR "190119810"
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      夜間緊急体制確保加算（有床診療所入院基本料１と２と３）
                             WHEN    "190126470"
                                IF  LNK-SCNYUACCT-KAGEN-SRYCD(1)
                                                         = "190097010"
                                                        OR "190119510"
                                                        OR "190119610"
                                                        OR "190097210"
                                                        OR "190119710"
                                                        OR "190119810"
                                                        OR "190135110"
                                                        OR "190135210"
                                                        OR "190135310"
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      一般病棟看護必要度評価加算（１３対１入院基本料）
                             WHEN    "190131570"
      *                            一般病棟看護必要度評価加算
                                   IF SYOKIKSN-IN-KHNSRYCD = "190077510"
                                     ADD    1            TO  IDX8
                                     MOVE   "190131570"  TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                   END-IF
      *                            一般病棟看護必要度評価加算（専門病院）
                                   IF SYOKIKSN-IN-KHNSRYCD = "190090310"
                                     ADD    1            TO  IDX8
                                     MOVE   "190134970"  TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                   END-IF
      *                      休日リハビリテーション提供体制加算（回復期リハビリテーション病棟入院料）
                             WHEN    "190140870"
                                IF  SYOKIKSN-IN-KHNSRYCD = "190141610"
                                                        OR "190141710"
                                                        OR "190141810"
                                                        OR "190141910"
                                                        OR "190150110"
                                                        OR "190150210"
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      看護必要度加算１（１０対１入院基本料）
                             WHEN    "190143070"
      *                         看護必要度加算１（一般病棟入院基本料）
                                IF SYOKIKSN-IN-KHNSRYCD = "190077410"
                                   ADD    1            TO  IDX8
                                   MOVE   "190143070"  TO
                                             WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                         看護必要度加算１（特定機能病院）
                                IF SYOKIKSN-IN-KHNSRYCD = "190085910"
                                   ADD    1            TO  IDX8
                                   MOVE   "190143970"  TO
                                             WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                         看護必要度加算１（専門病院）
                                IF SYOKIKSN-IN-KHNSRYCD = "190090210"
                                   ADD    1            TO  IDX8
                                   MOVE   "190144570"  TO
                                             WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      看護必要度加算２（１０対１入院基本料）
                             WHEN    "190143170"
      *                         看護必要度加算２（一般病棟入院基本料）
                                IF SYOKIKSN-IN-KHNSRYCD = "190077410"
                                   ADD    1            TO  IDX8
                                   MOVE   "190143170"  TO
                                             WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                         看護必要度加算２（特定機能病院）
                                IF SYOKIKSN-IN-KHNSRYCD = "190085910"
                                   ADD    1            TO  IDX8
                                   MOVE   "190144070"  TO
                                             WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                         看護必要度加算２（専門病院）
                                IF SYOKIKSN-IN-KHNSRYCD = "190090210"
                                   ADD    1            TO  IDX8
                                   MOVE   "190144670"  TO
                                             WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
                             WHEN   OTHER
                                ADD    1                TO  IDX8
                                MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                         END-EVALUATE
                     END-IF
               END-PERFORM
             END-IF
      *
      *      入院料加算（ユーザー設定）
             PERFORM   VARYING   IDX7   FROM   1  BY  1
               UNTIL  (IDX7      >   10)
                   IF   SYS-5001-KSNCD2(IDX7)  NOT =   SPACE
                       ADD   1     TO    IDX8
                       MOVE   SYS-5001-KSNCD2(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                   END-IF
             END-PERFORM
      *
           END-IF
      *
      *病室情報
           PERFORM 4801-BRMSEL-SEC
      *
           IF   FLG-SYSKANRI       =    ZERO
      *        新生児入院医療管理加算
               IF   SYS-5002-SINSEIJI-KBN      =   "2"
                    ADD     1                  TO IDX8
                    MOVE   SYS-5002-SINSEIJI-CD    TO
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
           MOVE    ZERO                TO     IDX9
           PERFORM VARYING     IDX8    FROM   1   BY   1
                   UNTIL      (IDX8    >     20)
                         OR   (WRK-NYUKHNKSN-SRYCD(IDX8)   =  SPACE)
               PERFORM 4801-NYUKSN-CHK-SEC
               IF      FLG-NYUKSN          =   ZERO
                   ADD    1                TO  IDX9
                   MOVE   WRK-NYUKHNKSN-SRYCD(IDX8)  TO
                                   LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX9)
      *            褥瘡患者管理加算のフラグセット
                   IF   WRK-NYUKHNKSN-SRYCD(IDX8)  =   "190116990"
                        MOVE   "1"    TO
                                   LNK-SCNYUACCT-NYUKHNKSN-KBN(IDX9)
                   END-IF
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
           IF     WRK-PARA-SRYYM     <=     "200803"
      *      看護配置適合加算
             IF    (WRK-BTU-KNGTEK-KBN   =   "2")  AND
                   (IDX9    <   20 )
                 ADD    1            TO  IDX9
                 MOVE  "190109670"   TO
                                   LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX9)
             END-IF
      *
      *      複数医師及び看護配置適合加算
             IF    (WRK-BTU-DRTEK-KBN   =   "2")  AND
                   (IDX9    <   20 )
                 ADD    1            TO  IDX9
                 MOVE  "190117070"   TO
                                   LNK-SCNYUACCT-NYUKHNKSN-SRYCD(IDX9)
             END-IF
           END-IF
      *
           .
      *
       410191-NYUKSN-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料加算の算定可・不可チェック
      *****************************************************************
       4801-NYUKSN-CHK-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-NYUKSN
      *
      *     EVALUATE   SYS-5000-KSNCD(IDX7)
      *        診療録管理体制加算
      *         WHEN   "190100370"
      *             IF   SPA-GMN-I01-SYORIKBN-CD   =  "08"
      *                  MOVE   1        TO     FLG-NYUKSN
      *             END-IF
      *            自院での継続入院チェック
      *     END-EVALUATE
      *
      *    入院料加算チェックテーブルによる判定
           INITIALIZE  NYUKSNCHK-REC
      *
           PERFORM 48011-TENSU-SEL-SEC
           MOVE    WRK-BTU-NYUKSNKBN   TO  NYUKSNCHK-NYUINKBN
           PERFORM 48012-TENSU-SEL-SEC
           IF      TNS-NYUKHNKSNKBN    NOT =   ZERO
               MOVE    WRK-KSN-NYUKSNKBN   TO  NYUKSNCHK-KSNKBN
               MOVE    SYOKIKSN-IN-SANTEISTYMD
                                           TO  NYUKSNCHK-YUKOSTYMD
               MOVE    SYOKIKSN-IN-SANTEISTYMD
                                           TO  NYUKSNCHK-YUKOEDYMD
      *
               MOVE    "tbl_nyuksnchk"     TO  WRK-MCP-TABLE
               MOVE    "key"               TO  WRK-MCP-PATHNAME
               MOVE    NYUKSNCHK-REC       TO  MCPDATA-REC
               PERFORM 900-TBL-SELECT-SEC
               IF    ( FLG-DBRC            =   ZERO )
                   MOVE    MCPDATA-REC         TO  NYUKSNCHK-REC
                   IF    ( NYUKSNCHK-CHKKBN    =   "1" OR "2" )
                       CONTINUE
                   ELSE
                       MOVE   1        TO     FLG-NYUKSN
                   END-IF
               ELSE
                   MOVE   1            TO  FLG-NYUKSN
               END-IF
      *
               MOVE    "tbl_nyuksnchk"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *
      *    注加算コードが同一であることの判定
           IF   WRK-KSN-CHUKSNCD      NOT =    SPACE
              IF   WRK-BTU-CHUKSNCD   =        WRK-KSN-CHUKSNCD
                  CONTINUE
              ELSE
                  MOVE    1     TO    FLG-NYUKSN
              END-IF
           END-IF
      *
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
           MOVE    SPACE           TO  WRK-BTU-CHUKSNCD
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
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                                   TO  TNS-YUKOSTYMD
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                                   TO  TNS-YUKOEDYMD
           MOVE    WRK-PARA-HOSPNUM TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"     TO  WRK-MCP-TABLE
           MOVE    "key"           TO  WRK-MCP-PATHNAME
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
      *    点数マスタクローズ
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF    ( FLG-TENSU       NOT =   ZERO )
               GO  TO  48011-TENSU-SEL-EXT
           END-IF
      *
           MOVE    TNS-NYUKHNKSNKBN
                                   TO  WRK-BTU-NYUKSNKBN
           MOVE    TNS-CHUKSNCD    TO  WRK-BTU-CHUKSNCD
      *    特別入院料の判定
           EVALUATE   TNS-NYUKHNKBN
               WHEN   73
               WHEN   74
               WHEN   75
               WHEN   76
               WHEN   77
               WHEN   78
               WHEN   79
               WHEN   83
               WHEN   84
                   MOVE    "1"     TO  WRK-TOKUBETSU-NYUIN
           END-EVALUATE
           IF  TNS-SRYCD      =    "190121810"
                   MOVE    "1"     TO  WRK-TOKUBETSU-NYUIN
           END-IF
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
           MOVE    SPACE           TO  WRK-KSN-CHUKSNCD
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
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                                   TO  TNS-YUKOSTYMD
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                                   TO  TNS-YUKOEDYMD
           MOVE    WRK-PARA-HOSPNUM TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"     TO  WRK-MCP-TABLE
           MOVE    "key"           TO  WRK-MCP-PATHNAME
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
      *    点数マスタクローズ
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF    ( FLG-TENSU       NOT =   ZERO )
               GO  TO  48012-TENSU-SEL-EXT
           END-IF
      *
           MOVE    TNS-NYUKHNKSNKBN
                                   TO  WRK-KSN-NYUKSNKBN
      *
      *    入院料（Ａ１００番台）、特定入院料（Ａ３００番台）に含まれる
      *    注加算のみが判定対象となる
           IF      (TNS-CDKBN-KBN       =   "A")     AND
                   (TNS-CDKBN-KBNNUM   >=   100)     AND
                   (TNS-CDKBN-KBNNUM   <=   199)
               MOVE    TNS-CHUKSNCD    TO  WRK-KSN-CHUKSNCD
           END-IF
      *
           IF      (TNS-CDKBN-KBN       =   "A")     AND
                   (TNS-CDKBN-KBNNUM   >=   300)     AND
                   (TNS-CDKBN-KBNNUM   <=   399)
               MOVE    TNS-CHUKSNCD    TO  WRK-KSN-CHUKSNCD
           END-IF
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
      *    入院患者の年齢計算
           INITIALIZE                      ORCSAGECHKAREA
           MOVE    WRK-PARA-HOSPNUM    TO  AGECHK-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  AGECHK-PTID
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                                       TO  AGECHK-SRYYMD
           MOVE    WRK-PARA-HOSPNUM    TO  PTINF-HOSPNUM
           MOVE    WRK-PTID            TO  PTINF-PTID
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-PTINF-SELECT-SEC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-PTINF-READ-SEC
           MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
                                           SPA-BIRTHDAY
           CALL    "ORCSAGECHK"        USING
                                           ORCSAGECHKAREA
                                           SPA-AREA
           IF    ( AGECHK-RC           =   ZERO )
               MOVE     AGECHK-NENREI2-YY  TO  WRK-NENREI
           ELSE
               INITIALIZE  ORCSAGECHKAREA
               MOVE     ZERO               TO  WRK-NENREI
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       4801-PT-AGE-EXT.
           EXIT.
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
           MOVE    WRK-PARA-SRYYM  TO  SYS-STYUKYMD(1:6)
           MOVE    "01"            TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM  TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  WRK-MCP-TABLE
           MOVE    "key10"         TO  WRK-MCP-PATHNAME
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    =   ZERO )
               MOVE    MCPDATA-REC
                                  TO   SYS-5001-REC
           ELSE
               MOVE    1          TO   FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
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
           MOVE    WRK-PARA-SRYYM  TO  SYS-STYUKYMD(1:6)
           MOVE    "01"            TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  WRK-MCP-TABLE
           MOVE    "key10"         TO  WRK-MCP-PATHNAME
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    =   ZERO )
               MOVE    MCPDATA-REC
                                  TO  SYS-5002-REC
           ELSE
               MOVE    1          TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       4801-BRMSEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料一般コードから老人コードへの置き換え処理
      *****************************************************************
       4801-RJNCD-CHG-SEC   SECTION.
      *
      *    保険算定年齢チェック等
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    WRK-PARA-HOSPNUM    TO  AGECHK-HOSPNUM
           MOVE    WRK-PTID            TO  AGECHK-PTID
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                                       TO  AGECHK-SRYYMD
           INITIALIZE                      PTINF-REC
           MOVE    WRK-PARA-HOSPNUM    TO  PTINF-HOSPNUM
           MOVE    WRK-PTID            TO  PTINF-PTID
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-PTINF-SELECT-SEC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-PTINF-READ-SEC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
           MOVE    PTNYUINRRK-HKNCOMBINUM
                                       TO  AGECHK-HKNCOMBINUM
           MOVE    "1"                 TO  AGECHK-NYUGAIKBN
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
      *
      *    老人対象患者であれば入院診療コードの置き換え（労災除く）
           IF     (AGECHK-HKNKBN       =   1  OR  2 )  OR
                  (AGECHK-ROUJIN2      =   ZERO     )
               CONTINUE
           ELSE
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
                       MOVE    WRK-PARA-HOSPNUM TO   CHG-HOSPNUM
                       MOVE    SRYCDCHG-REC    TO   MCPDATA-REC
                       MOVE    "tbl_srycdchg"  TO   WRK-MCP-TABLE
                       MOVE    "key2"          TO   WRK-MCP-PATHNAME
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
                       MOVE    "tbl_srycdchg"  TO   MCP-TABLE
                       MOVE    "key2"          TO   MCP-PATHNAME
                       PERFORM 900-CLOSE-SEC
                   END-IF
               ELSE
                   INITIALIZE              SRYCDCHG-REC
                   MOVE    SYOKIKSN-IN-KHNSRYCD
                                           TO   CHG-IPNSRYCD
                   MOVE    WRK-PARA-HOSPNUM TO   CHG-HOSPNUM
                   MOVE    SRYCDCHG-REC    TO   MCPDATA-REC
                   MOVE    "tbl_srycdchg"  TO  WRK-MCP-TABLE
                   MOVE    "key2"          TO  WRK-MCP-PATHNAME
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
                   MOVE    "tbl_srycdchg"  TO   MCP-TABLE
                   MOVE    "key2"          TO   MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
               END-IF
           END-IF
      *
           .
       4801-RJNCD-CHG-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定入院料の算定期間置き換え
      *****************************************************************
       4801-KIKAN-CHG-SEC   SECTION.
      *
           MOVE      ZERO              TO   KIKAN-IDX2
           PERFORM   VARYING   KIKAN-IDX1   FROM  1  BY  1
             UNTIL  (KIKAN-IDX1        >    6  )
              IF    LNK-SCNYUACCT-KAGEN-STYUKYMD(KIKAN-IDX1)
                                    <  LNK-SCNYUACCT-STSRYYMD
                IF    LNK-SCNYUACCT-STSRYYMD    <=
                          LNK-SCNYUACCT-KAGEN-EDYUKYMD(KIKAN-IDX1)
                    ADD   1       TO   KIKAN-IDX2
                    MOVE  LNK-SCNYUACCT-KAGEN-SRYCD(KIKAN-IDX1) TO
                          LNK-SCNYUACCT-KAGEN-SRYCD(KIKAN-IDX2)
                    MOVE  LNK-SCNYUACCT-STSRYYMD   TO
                          LNK-SCNYUACCT-KAGEN-STYUKYMD(KIKAN-IDX2)
                    MOVE  LNK-SCNYUACCT-KAGEN-EDYUKYMD(KIKAN-IDX1) TO
                          LNK-SCNYUACCT-KAGEN-EDYUKYMD(KIKAN-IDX2)
                END-IF
              ELSE
                    ADD   1       TO   KIKAN-IDX2
                    MOVE  LNK-SCNYUACCT-KAGEN-SRYCD(KIKAN-IDX1) TO
                          LNK-SCNYUACCT-KAGEN-SRYCD(KIKAN-IDX2)
                    MOVE  LNK-SCNYUACCT-KAGEN-STYUKYMD(KIKAN-IDX1) TO
                          LNK-SCNYUACCT-KAGEN-STYUKYMD(KIKAN-IDX2)
                    MOVE  LNK-SCNYUACCT-KAGEN-EDYUKYMD(KIKAN-IDX1) TO
                          LNK-SCNYUACCT-KAGEN-EDYUKYMD(KIKAN-IDX2)
              END-IF
           END-PERFORM 
      *
           IF    KIKAN-IDX2    NOT =  ZERO
             ADD       1         TO    KIKAN-IDX2
             PERFORM   VARYING   KIKAN-IDX1   FROM  KIKAN-IDX2  BY  1
               UNTIL  (KIKAN-IDX1        >    6  )
                 MOVE   SPACE   TO  
                             LNK-SCNYUACCT-KAGEN-SRYCD(KIKAN-IDX1)
                             LNK-SCNYUACCT-KAGEN-STYUKYMD(KIKAN-IDX1)
                             LNK-SCNYUACCT-KAGEN-EDYUKYMD(KIKAN-IDX1)
             END-PERFORM
           END-IF

           .
       4801-KIKAN-CHG-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       900-TBL-SELECT-SEC              SECTION.
      *
           MOVE    ZERO              TO  FLG-DBRC
           MOVE    WRK-MCP-TABLE     TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME  TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF    ( MCP-RC            =   ZERO )
               MOVE    WRK-MCP-TABLE     TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME  TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
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
      *
      *****************************************************************
      *    患者番号取得処理
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
           INITIALIZE                      PTNUM-REC
           MOVE    WRK-PARA-HOSPNUM    TO  PTNUM-HOSPNUM
           MOVE    WRK-PTID            TO  PTNUM-PTID
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-PTNUM-SELECT-SEC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-PTNUM-READ-SEC
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       900-PTNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者情報取得処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
           INITIALIZE                      PTINF-REC
           MOVE    WRK-PARA-HOSPNUM    TO  PTINF-HOSPNUM
           MOVE    WRK-PTID            TO  PTINF-PTID
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-PTINF-SELECT-SEC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-PTINF-READ-SEC
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *      
      *****************************************************************
      *    システム管理マスター検索
      *****************************************************************
       910-SYSKANRI-SELECT-SEC        SECTION.
      *
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスター読込
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
                                           SYS-5000-REC
                                           SYS-5001-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYS-1001-REC
                                           SYS-5000-REC
                                           SYS-5001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴マスター検索
      *****************************************************************
       910-PTNYUINRRK-SELECT-SEC        SECTION.
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-PTNYUINRRK
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴マスター読込
      *****************************************************************
       910-PTNYUINRRK-READ-SEC        SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  PTNYUINRRK-REC
               MOVE    ZERO               TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                     PTNYUINRRK-REC
               MOVE    1                  TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター検索
      *****************************************************************
       910-NYUINACCT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           PERFORM 900-DBSELECT-SEC
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
       910-NYUINACCT-READ-SEC        SECTION.
      *
           PERFORM 900-DBFETCH-SEC
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
      *    収納マスター検索
      *****************************************************************
       910-SYUNOU-SELECT-SEC        SECTION.
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       910-SYUNOU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       910-SYUNOU-READ-SEC        SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       910-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号マスター検索
      *****************************************************************
       910-PTNUM-SELECT-SEC        SECTION.
      *
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-PTNUM
           ELSE
               MOVE    1               TO  FLG-PTNUM
           END-IF
      *
           .
       910-PTNUM-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号マスター読込
      *****************************************************************
       910-PTNUM-READ-SEC        SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTNUM-REC
               MOVE    ZERO            TO  FLG-PTNUM
           ELSE
               INITIALIZE                  PTNUM-REC
               MOVE    1               TO  FLG-PTNUM
           END-IF
      *
           .
       910-PTNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報マスター検索
      *****************************************************************
       910-PTINF-SELECT-SEC        SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           .
       910-PTINF-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報マスター読込
      *****************************************************************
       910-PTINF-READ-SEC        SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           .
       910-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           PERFORM   900-ORCDBMAIN-SEC
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    WRK-PARA-HOSPNUM TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *      
