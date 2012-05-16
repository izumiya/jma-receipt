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
       PROGRAM-ID.             ORCBCRENYUACCT.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次
      *  コンポーネント名  : 入院会計一括作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/05/15    NACL-小豆沢   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *平成１８年４月改定対応
      *  02.08.01    NACL-小豆沢  06/03/16  入院料老人コード置き換えは療養病棟
      *                                     または療養病床のみを対象とする
      *              NACL-小豆沢  06/03/17  特定入院料対応
      *                                       脳卒中ケアユニット入院医療管理料
      *                                       精神科救急入院料
      *                                       精神科急性期治療病棟入院料１
      *                                       精神科急性期治療病棟入院料２
      *                                     看護補助加算の変更
      *                                     改定による特定入院料次月作成変更
      *  02.09.01    NACL-小豆沢  06/05/16  老人特定入院基本料を特定入院料
      *                                     より外す
      *平成１８年７月改定対応
      *  03.00.01    NACL-小豆沢  06/06/19  療養病棟、療養病床改定対応
      *  03.05.01    NACL-小豆沢  07/06/13  グループ診療対応
      *
      *平成２０年４月改定対応
      *  04.02.00    NACL-小豆沢  08/03/05  亜急性期入院医療管理料２
      *                                     精神科救急入院料２
      *                                     小児入院医療管理料１
      *                                     回復期リハビリテーション入院料１
      *                                     回復期リハビリテーション入院料２
      *                                     精神科救急・合併症入院料
      *                                     短期滞在手術基本料３
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
           03  FLG-DBRC                    PIC 9(01).
           03  FLG-WWK-PTNYUINRRK          PIC 9(01).
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
           03  IDXA                        PIC 9(05).
           03  IDXB                        PIC 9(05).
           03  TN-IDX                      PIC 9(02).
           03  IDX-DAY                     PIC 9(02).
           03  IDX-SHOKUJI                 PIC 9(02).
           03  KIKAN-IDX1                  PIC 9(04).
           03  KIKAN-IDX2                  PIC 9(04).
           03  SC-IDX                      PIC 9(02).
           03  SC-IDX2                     PIC 9(02).
      *
      *    パラメタ領域
       01  PARA-AREA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID                      PIC 9(07).
           03  WRK-PARA-SHELLID                    PIC X(08).
           03  WRK-PARA-HOSPNUM                    PIC 9(02).
           03  RECEERR                             PIC X(100).
           03  WRK-PARA-SRYYM                      PIC X(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MCP-TABLE                       PIC X(64).
           03  WRK-MCP-PATHNAME                    PIC X(64).
           03  WRK-ERRNO                           PIC 9(2).
           03  WRK-RECEERR                         PIC X(200).
           03  WRK-PTID                            PIC 9(10).
           03  WRK-SRYYM                           PIC X(06).
           03  WRK-TNS-YMD                         PIC X(08).
           03  WRK-NAIYO                           PIC X(100).
           03  WRK-KANACHK-MAE-INPUT               PIC X(400).
      *    月末日算定領域
           03  WRK-MATSUYMD.
               05  WRK-MATSUYY                     PIC 9(04).
               05  WRK-MATSUMM                     PIC 9(02).
               05  WRK-MATSUDD                     PIC 9(02).
      *
      *    プレイルーム、保育士加算加算退避領域
           03  WRK-BTU-HIKSISUKBN                  PIC X(01).
           03  WRK-BTU-KNGTEK-KBN                  PIC X(01).
           03  WRK-BTU-DRTEK-KBN                   PIC X(01).
      *
      *    入院会計作成用ワーク
           03  WRK-YMD             PIC X(10). 
           03  WRK-NAI-SRYYM-41.
               05  WRK-NAI-SRYYM-41-YY  PIC 9(04).
               05  WRK-NAI-SRYYM-41-MM  PIC 9(02).
           03  WRK-TSUSAN          PIC 9(05).
           03  WRK-SHONUM          PIC 9(03).
           03  WRK-STYMD           PIC X(08).
           03  WRK-EDYMD           PIC X(08).
           03  WRK-NISSU           PIC 9(05).
           03  WRK-NISSU1          PIC 9(05).
           03  WRK-NISSU2          PIC 9(05).
           03  WRK-SENTEI-STYMD    PIC X(08).
           03  WRK-BIRTHDAY-15                 PIC X(08).
           03  WRK-BIRTHDAY-15R   REDEFINES    WRK-BIRTHDAY-15.
               05  WRK-BIRTHDAY-15Y            PIC 9(04).
               05  WRK-BIRTHDAY-15MD           PIC 9(04).
           03  WRK-SANTEISTYMD     PIC X(08).
           03  WRK-TOKUTEI-NYUIN   PIC X(02).
           03  WRK-SAGAKU-KBN      PIC X(02).
           03  WRK-SAGAKU-KBN9     REDEFINES  WRK-SAGAKU-KBN
                                   PIC 9(02).
           03  WRK-HKNCOMBINUM     PIC X(04).
           03  WRK-HKNCOMBINUM9    REDEFINES  WRK-HKNCOMBINUM
                                   PIC 9(04).
           03  WRK-TAIHI-SRYKA     PIC X(02).
           03  WRK-TAIHI-SHOKUJI   PIC 9(02).
           03  WRK-TAIHI-SHOKUDOU  PIC 9(02).
           03  WRK-TAIHI-HKN       PIC 9(04).
           03  WRK-ZOGENPTN        PIC X(01).
           03  WRK-ZOGEN           PIC S9(05).
           03  WRK-KEISANYMD       PIC X(08).
      **** 03  WRK-TAIHI-SRYYM-41  PIC X(06).
      *    医療機関設定の入院料加算チェック用
      *****03  WRK-NYUKHNKSN-SRYCD PIC X(09)  OCCURS 20.
           03  WRK-NYUKHNKSN-AREA.
               05  WRK-NYUKHNKSN-SRYCD PIC X(09)  OCCURS 60.
           03  WRK-NYUKSNKBN       PIC X(03).
           03  WRK-BTU-NYUKSNKBN   PIC X(03).
           03  WRK-BTU-CDKBN-KBNNUM  PIC 9(03).
           03  WRK-BTU-CDKBN-KBNNUM-EDA PIC 9(02).
           03  WRK-KSN-NYUKSNKBN   PIC X(03).
           03  WRK-KSN-CDKBN-KBNNUM  PIC 9(03).
           03  WRK-KSN-CDKBN-KBNNUM-EDA PIC 9(02).
           03  WRK-SRYCD           PIC X(09).
           03  WRK-STYUKYMD        PIC X(08).
           03  WRK-NYUINYM         PIC X(06).
           03  WRK-TOKUBETSU-NYUIN PIC X(01).
           03  WRK-TOKUTEI-TENNYUYMD  PIC X(08).
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
      *    患者年齢算出用エリア
       01  WRK-NENREI-AREA.
           03  WRK-NENREI          PIC 9(03).
           03  WRK-YMDS1.
               05  WRK-YYS1        PIC 9(04).
               05  WRK-MMS1        PIC 9(02).
               05  WRK-DDS1        PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2        PIC 9(04).
               05  WRK-MMS2        PIC 9(02).
               05  WRK-DDS2        PIC 9(02).
      *
       01  LST-AREA.
           03  LST-HEAD1.
               05                      PIC X(40)   VALUE   " ".
               05                      PIC X(30)   VALUE   
                             "入院会計一括作成結果リスト".
               05                      PIC X(30)   VALUE   " ".
               05  LST-HEAD-PAGE       PIC Z,ZZ9.
               05                      PIC X(05)   VALUE   "頁".
           03  LST-HEAD.
               05                      PIC X(04)   VALUE   "番号".
               05                      PIC X(01)   VALUE   " ".
               05                      PIC X(20)   VALUE   "患者番号".
               05                      PIC X(01)   VALUE   " ".
               05                      PIC X(16)   VALUE   "病棟".
               05                      PIC X(05)   VALUE   " ".
               05                      PIC X(26)   VALUE   "氏名".
               05                      PIC X(05)   VALUE   " ".
               05                      PIC X(08)   VALUE   "対象年月".
               05                      PIC X(01)   VALUE   " ".
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
      *    入院履歴退避用ワーク
       01  WWK-PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC"     REPLACING
                                //PTNYUINRRK-// BY //WWK-PTNYUINRRK-//.
      *    特定入院期間算定用ワーク
       01  WRK-SCNYUACCT-AREA.
           03  WRK-SCNYUACCT-KAGENCD-TBL         OCCURS  7.
               05  WRK-SCNYUACCT-KAGEN-SRYCD     PIC  X(09).
               05  WRK-SCNYUACCT-KAGEN-STYUKYMD  PIC  X(08).
               05  WRK-SCNYUACCT-KAGEN-EDYUKYMD  PIC  X(08).
      *
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
      *    亜急性期入院医療管理料判定パラメタ
           COPY  "CPORCSAKYUSEI.INC".
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
           INITIALIZE                  SPA-AREA
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
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO   SPA-HOSPNUM
           CALL    "ORCSENCODING"      USING
                                       MCPAREA
                                       SPA-AREA
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "ORCBCRENYUACCT"    TO  JOB-PGID
           MOVE    "入院会計一括作成"  TO  JOB-SHELLMSG
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
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC         =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM     910-SYSKANRI-READ-SEC
                   IF    FLG-SYSKANRI  =   ZERO
                         MOVE    SYS-1001-PREFNUM TO  SPA-PREFNUM
                   ELSE
                         MOVE  "医療機関情報が取得できませんでした。"
                                           TO  WRK-RECEERR
                         PERFORM 500-ERR-HENSYU-SEC
                   END-IF
           ELSE
               MOVE    "医療機関情報が取得できませんでした。"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF    MCP-RC   NOT =    ZERO
               GO    TO   100-INIT-EXT
           END-IF
      *
      *    主科情報取得
           INITIALIZE                      SYS-2009-REC
           MOVE    "2009"              TO  SYS-2009-KANRICD
           MOVE    "*"                 TO  SYS-2009-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-2009-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2009-STYUKYMD (7:2)
           MOVE    SYS-2009-STYUKYMD   TO  SYS-2009-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-2009-HOSPNUM
           MOVE    SYS-2009-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC     =    ZERO
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
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    前月分会計取得用の年月算出
           MOVE    WRK-PARA-SRYYM      TO  WRK-CHK-SRYYM
           IF      WRK-CHK-SRYMR        =   1
               COMPUTE   WRK-CHK-SRYYR =   WRK-CHK-SRYYR - 1
               MOVE      12            TO  WRK-CHK-SRYMR
           ELSE
               COMPUTE   WRK-CHK-SRYMR =   WRK-CHK-SRYMR - 1
           END-IF
      *    前月の末日を算定
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    WRK-CHK-SRYYM       TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  IDX-DAY
      *     DISPLAY   "WRK-CHK-SRYYM = " WRK-CHK-SRYYM
      *     DISPLAY   "IDX-DAY = " IDX-DAY
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
           INITIALIZE                           PTNYUINRRK-REC
           MOVE    SYS-1001-HOSPNUM     TO      PTNYUINRRK-HOSPNUM
           INITIALIZE                           STS-AREA-DAY
           INITIALIZE                           LNK-DAY7-AREA
           MOVE    "71"                 TO      LNK-DAY7-IRAI
           MOVE    WRK-PARA-SRYYM       TO      LNK-DAY7-YM
           CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI      TO      PTNYUINRRK-NYUINYMD
           MOVE    PTNYUINRRK-REC       TO      MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"     TO      MCP-TABLE
           MOVE    "key27"              TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_ptnyuinrrk"     TO      MCP-TABLE
               MOVE    "key27"              TO      MCP-PATHNAME
               PERFORM  910-PTNYUINRRK-READ-SEC
               PERFORM  UNTIL  FLG-PTNYUINRRK    =    1
                       MOVE    PTNYUINRRK-PTID   TO  WRK-PTID
      *                入院会計存在チェック
                       PERFORM    2001-NYUINACCT-CHK-SEC
                       IF         FLG-NYUINACCTARI   =    ZERO
      *                     前月分入院会計の存在チェック
      *                     （併せて保険組合せと食事の情報を退避）
                            PERFORM     2001-NYUINACCT-MAECHK-SEC
      *                     入院会計作成処理
                            PERFORM     2001-NYUINACCT-INS-SEC
                       END-IF
                   MOVE    "tbl_ptnyuinrrk"     TO      MCP-TABLE
                   MOVE    "key27"              TO      MCP-PATHNAME
                   PERFORM  910-PTNYUINRRK-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"     TO      MCP-TABLE
           MOVE    "key27"              TO      MCP-PATHNAME
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
      *    入院会計存在チェック処理
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
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               PERFORM UNTIL ( FLG-NYUINACCT       NOT =   ZERO )
                        OR   ( FLG-NYUINACCTARI    NOT =   ZERO )
      *
                   IF    ( NACCT-ZAISKBKBN     =    "1"    )
                       MOVE    1           TO  FLG-NYUINACCTARI
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
      *    入院会計前月データ存在チェック処理
      *    （食事と保険組合せの退避処理も併せて行う）
      *****************************************************************
       2001-NYUINACCT-MAECHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-TAIHI-SHOKUJI
           MOVE    ZERO                TO  WRK-TAIHI-SHOKUDOU
           MOVE    ZERO                TO  WRK-TAIHI-HKN
      *
           MOVE    ZERO                TO  FLG-NYUINACCT-MAE-CHK
           INITIALIZE                      NYUINACCT-REC
           MOVE    PTNYUINRRK-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    PTNYUINRRK-PTID     TO  NACCT-PTID
           MOVE    WRK-CHK-SRYYM       TO  NACCT-SRYYM
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               PERFORM UNTIL  FLG-NYUINACCT NOT =   ZERO
      *
                   IF    ( NACCT-ZAISKBKBN     =    "1"    )
                       MOVE    1           TO  FLG-NYUINACCT-MAE-CHK
                   END-IF
      *            食事
                   IF    ( NACCT-ZAISKBKBN     =    "4"    )   AND
                         ( NACCT-ZAITEN        =    ZERO   )
                       MOVE    ZERO      TO   IDX-SHOKUJI
                       PERFORM VARYING IDX-SHOKUJI FROM IDX-DAY BY -1
                          UNTIL (IDX-SHOKUJI  < 1)
                                IF  NACCT-DAY(IDX-SHOKUJI) NOT = ZERO
                                    MOVE  NACCT-DAY(IDX-SHOKUJI)
                                            TO  WRK-TAIHI-SHOKUJI
                                    MOVE  1   TO  IDX-SHOKUJI
                                END-IF
                       END-PERFORM
                   END-IF
      *            食堂加算
                   IF    ( NACCT-ZAISKBKBN     =    "4"    )   AND
                         (NACCT-SRYCDTOTAL     =   197000570)
      *                  月末日を取得
                         INITIALIZE                 STS-AREA-DAY
                         INITIALIZE                 LNK-DAY7-AREA
                         MOVE     "71"        TO    LNK-DAY7-IRAI
                         MOVE     NACCT-SRYYM TO    LNK-DAY7-YM
                         CALL  "ORCSDAY"    USING   STS-AREA-DAY
                                                    LNK-DAY7-AREA
                         MOVE    LNK-DAY7-KOYOMI
                                              TO    WRK-MATSUYMD
                         MOVE  NACCT-DAY(WRK-MATSUDD)
                                              TO    WRK-TAIHI-SHOKUDOU
                   END-IF
      *            保険組合せ
                   IF    ( NACCT-ZAISKBKBN     =    "5"    )
                       MOVE    NACCT-DAY(IDX-DAY)
                                           TO  WRK-TAIHI-HKN
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
       2001-NYUINACCT-MAECHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計作成処理
      *****************************************************************
       2001-NYUINACCT-INS-SEC          SECTION.
      *
           IF    FLG-NYUINACCT-MAE-CHK     =    1
               PERFORM     410191-KAIKEI-SAKUSEI-SEC
           END-IF
      *
           PERFORM     2002-HCMSL55-HEN-SEC
      *
           .
       2001-NYUINACCT-INS-EXT.
           EXIT.
      ****************************a*************************************
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
               INITIALIZE              ORCSKANACHK2AREA
               MOVE    "1"         TO  KANACHK2-SYORI
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
           MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
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
           IF      FLG-NYUINACCT-MAE-CHK   =   ZERO
               MOVE  "前月入院会計がありませんでした" TO  WRK-NAIYO
           ELSE
               EVALUATE    WRK-ERRCD
                   WHEN    SPACE
                       MOVE  "入院会計を作成しました" TO  WRK-NAIYO
                   WHEN    "2003"
                       MOVE  "通算日数の取得に失敗しました"
                                                      TO  WRK-NAIYO
                   WHEN    "2006"
                       MOVE  "入院料加算の取得に失敗しました"
                                                      TO  WRK-NAIYO
                   WHEN    "2007"
                       MOVE  "入院行為照会用テーブル更新失敗"
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
           MOVE    "入院会計一括作成"
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
      *****************************************************************
      *    患者番号取得処理
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
           INITIALIZE                      PTNUM-REC
           MOVE    WRK-PARA-HOSPNUM    TO  PTNUM-HOSPNUM
           MOVE    WRK-PTID            TO  PTNUM-PTID
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
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
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
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
      *
      *****************************************************************
      *    システム管理マスター読込
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
                                               SYS-5000-REC
                                               SYS-5001-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                      SYS-1001-REC
                                               SYS-5000-REC
                                               SYS-5001-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴マスター読込
      *****************************************************************
       910-PTNYUINRRK-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
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
      *    入院会計マスター読込
      *****************************************************************
       910-NYUINACCT-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
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
      *
      *
      *****************************************************************
      *    患者番号マスター読込
      *****************************************************************
       910-PTNUM-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  PTNUM-REC
               MOVE    ZERO               TO  FLG-PTNUM
           ELSE
               INITIALIZE                     PTNUM-REC
               MOVE    1                  TO  FLG-PTNUM
           END-IF
      *
           .
       910-PTNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報マスター読込
      *****************************************************************
       910-PTINF-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  PTINF-REC
               MOVE    ZERO               TO  FLG-PTINF
           ELSE
               INITIALIZE                     PTINF-REC
               MOVE    1                  TO  FLG-PTINF
           END-IF
      *
           .
       910-PTINF-READ-EXT.
           EXIT.
      *
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
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    WRK-PTID            TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM  900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
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
      *    小児特定集中治療室管理料
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN = "03" OR "04" OR "05" OR
                                               "06" OR "07" OR "08" OR
                                               "09" OR "14" OR "16" OR
                                               "17" OR "19" OR "55" OR
                                               "57" OR "59" OR "91"
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
      *     DISPLAY "====AZU  KAIKEI-SAKUSEIL=================="
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
      *    亜急性期入院医療管理料１の場合は最長６０日間の算定の為、
      *    次月の会計作成時に９０日経過時は無条件に一般の入院料算定とする
      *    （平成２４年３月３１日までの入院分は９０日限度）
           IF PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN   = "18"
                  IF   PTNYUINRRK-NYUINYMD   >=   "20120401"
                      IF  STUSAN-NISSU06   >=   60
                          MOVE    SPACE   TO   WRK-TOKUTEI-NYUIN
                      END-IF
                  ELSE
                      IF  STUSAN-NISSU06   >=   90
                          MOVE    SPACE   TO   WRK-TOKUTEI-NYUIN
                      END-IF
                  END-IF
               END-IF
           END-IF
      *
      *    亜急性期入院医療管理料２の場合は最長６０日間の算定の為、
      *    次月の会計作成時に６０日経過時は無条件に一般の入院料算定とする
      *  （経過措置により平成２４年３月３１日時点で亜急性期入院医療
      *    管理料１を算定している場合は９０日限度とする対応追加）
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
             IF   PTNYUINRRK-TOKUTEI-NYUIN   = "54"
               IF  PTNYUINRRK-NYUINYMD   >=   "20120401"
                   IF   STUSAN-NISSU06   >=   60
                       MOVE    SPACE   TO   WRK-TOKUTEI-NYUIN
                   END-IF
               ELSE
                   IF    LNK-AKYUSEI-RC   =   1
                      IF   STUSAN-NISSU06   >=   90
                          MOVE    SPACE   TO   WRK-TOKUTEI-NYUIN
                      END-IF
                   ELSE
                      IF   STUSAN-NISSU06   >=   60
                          MOVE    SPACE   TO   WRK-TOKUTEI-NYUIN
                      END-IF
                   END-IF
               END-IF
             END-IF
           END-IF
      *
      *    亜急性期入院医療管理料１（指定地域）の場合は最長６０日間の算定の為、
      *    次月の会計作成時に６０日経過時は無条件に一般の入院料算定とする
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN = "92"
                   IF       STUSAN-NISSU06  >=  60
                       MOVE    SPACE   TO    WRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    亜急性期入院医療管理料２（指定地域）の場合は最長６０日間の算定の為、
      *    次月の会計作成時に６０日経過時は無条件に一般の入院料算定とする
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN = "93"
                   IF       STUSAN-NISSU06  >=  60
                       MOVE    SPACE   TO    WRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    特定一般病棟入院料（亜急性期入院医療管理）の場合は最長６０日間の算定の為、
      *    次月の会計作成時に６０日経過時は無条件に一般の入院料算定とする
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN = "95"
                   IF       STUSAN-NISSU06  >=  60
                       MOVE    SPACE   TO    WRK-TOKUTEI-NYUIN
                   END-IF
               END-IF
           END-IF
      *
      *    特定一般病棟入院料（亜急性期入院医療管理・リハビリ算定患者）の場合は
      *    最長６０日間の算定の為、次月の会計作成時に６０日経過時は無条件に
           IF   PTNYUINRRK-TOKUTEI-KBN   =   "3"
               IF   PTNYUINRRK-TOKUTEI-NYUIN = "96"
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
      *        特定一般病棟入院料の場合は期間加算が存在するため、
      *        初期加算サブを呼び出す（平成２４年改正で追加）
      *        療養病床で有床診療所入院基本料を相互算定する場合も
      *        初期加算サブを呼び出す（平成２４年改正で追加）
               IF    SYOKIKSN-IN-KHNSRYCD   =   "190151910"   OR
                                                "190152010"   OR
                                                "099999950"   OR
                                                "099999951"   OR
                                                "099999952"
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                                                   SPA-AREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "2006"      TO  SPA-ERRCD
                       GO  TO  410191-NYUACCT-SAKUSEI-EXT
                   END-IF
               END-IF
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
           IF      PTNYUINRRK-TENNYUYMD    <=  "20100331"
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
               IF      (PTNYUINRRK-SHOKAIKBN  = "1")  AND
                       (PTNYUINRRK-JTIKBN  = "1")
                   MOVE   PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
               ELSE
                   MOVE   SYOKIKSN-IN-SANTEISTYMD
                                       TO  LNK-DAY6-KIJUN
               END-IF
               DISPLAY "STUSAN-NISSU03 = " STUSAN-NISSU03
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
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
               MOVE    "190740010"         TO  LNK-SCNYUACCT-KAGEN-SRYCD
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
      *    認知症治療病棟入院料１（平成２４年３月までの期間対応）
           IF     (PTNYUINRRK-TENNYUYMD    >=  "20100401")   AND
                  (PTNYUINRRK-TENNYUYMD    <=  "20120331")
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
               IF      (PTNYUINRRK-SHOKAIKBN  = "1")  AND
                       (PTNYUINRRK-JTIKBN  = "1")
                   MOVE   PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
               ELSE
                   MOVE   SYOKIKSN-IN-SANTEISTYMD
                                       TO  LNK-DAY6-KIJUN
               END-IF
               DISPLAY "STUSAN-NISSU03 = " STUSAN-NISSU03
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
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
               MOVE    "190740010"         TO  LNK-SCNYUACCT-KAGEN-SRYCD
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
      *    認知症治療病棟入院料１（平成２４年４月からの期間対応）
           IF      PTNYUINRRK-TENNYUYMD    >=  "20120401"
             IF      SYOKIKSN-IN-KHNSRYCD    =   "190151610"
               MOVE   SPACE            TO  SYOKIKSN-IN-KHNSRYCD
               MOVE   SPACE            TO  LNK-SCNYUACCT-NYUKHN-SRYCD
                                                                    (1)
               MOVE   SPACE            TO  WRK-SCNYUACCT-AREA
               INITIALIZE                  WRK-SCNYUACCT-AREA
               MOVE   ZERO             TO  TN-IDX
               IF     STUSAN-NISSU03      <=  30
                   ADD    1      TO    TN-IDX
                   MOVE   "190151610"  TO  WRK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                   MOVE   SYOKIKSN-IN-SANTEISTYMD
                                       TO  WRK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                   INITIALIZE              STS-AREA-DAY
                   INITIALIZE              LNK-DAY6-AREA
                   MOVE    "61"        TO  LNK-DAY6-IRAI
                   MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
                   MOVE    "1"         TO  LNK-DAY6-ZOGENPTN
                   MOVE    29          TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN
                                       TO  WRK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
               END-IF
      *
               IF     STUSAN-NISSU03   <=  60
                   ADD    1            TO  TN-IDX
                   MOVE   "190739910"  TO  WRK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *            認知症治療病棟入院料１の６０日以内の開始日セット
                   IF     TN-IDX        =      1
                       MOVE   PTNYUINRRK-TENNYUYMD
                                       TO  WRK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                   END-IF
                   IF     TN-IDX       =      2
                       MOVE    WRK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                              TO  LNK-DAY6-KIJUN
                       MOVE    "1"            TO  LNK-DAY6-ZOGENPTN
                       MOVE     1             TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                     TO WRK-SCNYUACCT-KAGEN-STYUKYMD
                                                             (TN-IDX)
                   END-IF
      *            認知症治療病棟入院料１の６０日以内の終日
      *            を再計算
                   MOVE     ZERO      TO   WK-TOKUTEI-DAY
                   COMPUTE  WK-TOKUTEI-DAY  =  60  - STUSAN-NISSU03
                   INITIALIZE                STS-AREA-DAY
                   INITIALIZE                LNK-DAY6-AREA
                   MOVE    "61"              TO  LNK-DAY6-IRAI
                   MOVE   SYOKIKSN-IN-SANTEISTYMD
                                             TO  LNK-DAY6-KIJUN
                   MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                   MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                   CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                     LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN
                                      TO WRK-SCNYUACCT-KAGEN-EDYUKYMD
                                                             (TN-IDX)
               END-IF
      *
               ADD    1   TO    TN-IDX
      *
               MOVE   "190740010"  TO  WRK-SCNYUACCT-KAGEN-SRYCD
                                                           (TN-IDX)
               INITIALIZE                STS-AREA-DAY
               INITIALIZE                LNK-DAY6-AREA
               MOVE    "61"              TO  LNK-DAY6-IRAI
               IF      TN-IDX      =     2
                   MOVE    WRK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                         TO  LNK-DAY6-KIJUN
                   MOVE    "1"           TO  LNK-DAY6-ZOGENPTN
                   MOVE     1            TO  LNK-DAY6-ZOGEN
                   CALL  "ORCSDAY"       USING   STS-AREA-DAY
                                                 LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN
                                     TO WRK-SCNYUACCT-KAGEN-STYUKYMD
                                                             (TN-IDX)
               ELSE
                   IF   TN-IDX      =     3
                       MOVE    WRK-SCNYUACCT-KAGEN-EDYUKYMD(2)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                              LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                 TO WRK-SCNYUACCT-KAGEN-STYUKYMD
                                                         (TN-IDX)
                   ELSE
                       MOVE    PTNYUINRRK-TENNYUYMD
                                     TO WRK-SCNYUACCT-KAGEN-STYUKYMD
                                                             (TN-IDX)
                   END-IF
               END-IF
               MOVE    "99999999"    TO WRK-SCNYUACCT-KAGEN-EDYUKYMD
                                                             (TN-IDX)
      *
               MOVE    ZERO            TO  SC-IDX2
               PERFORM    VARYING    SC-IDX   FROM  1   BY   1
                 UNTIL  (WRK-SCNYUACCT-KAGEN-SRYCD(SC-IDX) = SPACE) OR
                        (SC-IDX        >    7)
                 IF     (WRK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX) <=
                                          LNK-SCNYUACCT-STSRYYMD)  AND
                        (WRK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX) >=
                                          LNK-SCNYUACCT-STSRYYMD)
                      ADD     1     TO   SC-IDX2
                      MOVE  WRK-SCNYUACCT-KAGEN-SRYCD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-SRYCD(SC-IDX2)
                      MOVE  LNK-SCNYUACCT-STSRYYMD  TO
                            LNK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX2)
                      MOVE  WRK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX2)
                 END-IF
                 IF     (WRK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX) >
                                          LNK-SCNYUACCT-STSRYYMD)
                      ADD     1     TO   SC-IDX2
                      MOVE  WRK-SCNYUACCT-KAGEN-SRYCD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-SRYCD(SC-IDX2)
                      MOVE  WRK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX2)
                      MOVE  WRK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX2)
                 END-IF
               END-PERFORM
             END-IF
           END-IF
      *
      *    認知症病棟入院料２（平成２２年３月までの期間対応）
           IF      PTNYUINRRK-TENNYUYMD    <=  "20100331"
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
               IF      (PTNYUINRRK-SHOKAIKBN  = "1")  AND
                       (PTNYUINRRK-JTIKBN  = "1")
                   MOVE   PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
               ELSE
                   MOVE   SYOKIKSN-IN-SANTEISTYMD
                                       TO  LNK-DAY6-KIJUN
               END-IF
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
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
      *    認知症治療病棟入院料２（平成２４年３月までの期間対応）
           IF     (PTNYUINRRK-TENNYUYMD    >=  "20100401")   AND
                  (PTNYUINRRK-TENNYUYMD    <=  "20120331")
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
               IF      (PTNYUINRRK-SHOKAIKBN  = "1")  AND
                       (PTNYUINRRK-JTIKBN  = "1")
                   MOVE   PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
               ELSE
                   MOVE   SYOKIKSN-IN-SANTEISTYMD
                                       TO  LNK-DAY6-KIJUN
               END-IF
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
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
      *    認知症治療病棟入院料２（平成２４年４月からの期間対応）
           IF      PTNYUINRRK-TENNYUYMD    >=  "20120401"
             IF      SYOKIKSN-IN-KHNSRYCD    =   "190151710"
               MOVE   SPACE            TO  SYOKIKSN-IN-KHNSRYCD
               MOVE   SPACE            TO  LNK-SCNYUACCT-NYUKHN-SRYCD
                                                                    (1)
               MOVE   SPACE            TO  WRK-SCNYUACCT-AREA
               INITIALIZE                  WRK-SCNYUACCT-AREA
               MOVE   ZERO             TO  TN-IDX
               IF     STUSAN-NISSU03      <=  30
                   ADD    1      TO    TN-IDX
                   MOVE   "190151710"  TO  WRK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
                   MOVE   SYOKIKSN-IN-SANTEISTYMD
                                       TO  WRK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                   INITIALIZE              STS-AREA-DAY
                   INITIALIZE              LNK-DAY6-AREA
                   MOVE    "61"        TO  LNK-DAY6-IRAI
                   MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
                   MOVE    "1"         TO  LNK-DAY6-ZOGENPTN
                   MOVE    29          TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN
                                       TO  WRK-SCNYUACCT-KAGEN-EDYUKYMD
                                                               (TN-IDX)
               END-IF
      *
               IF     STUSAN-NISSU03   <=  60
                   ADD    1            TO  TN-IDX
                   MOVE   "190813410"  TO  WRK-SCNYUACCT-KAGEN-SRYCD
                                                               (TN-IDX)
      *            認知症治療病棟入院料２の６０日以内の開始日セット
                   IF     TN-IDX        =      1
                       MOVE   PTNYUINRRK-TENNYUYMD
                                       TO  WRK-SCNYUACCT-KAGEN-STYUKYMD
                                                               (TN-IDX)
                   END-IF
                   IF     TN-IDX       =      2
                       MOVE    WRK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                              TO  LNK-DAY6-KIJUN
                       MOVE    "1"            TO  LNK-DAY6-ZOGENPTN
                       MOVE     1             TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                     TO WRK-SCNYUACCT-KAGEN-STYUKYMD
                                                             (TN-IDX)
                   END-IF
      *            認知症治療病棟入院料２の６０日以内の終日
      *            を再計算
                   MOVE     ZERO      TO   WK-TOKUTEI-DAY
                   COMPUTE  WK-TOKUTEI-DAY  =  60  - STUSAN-NISSU03
                   INITIALIZE                STS-AREA-DAY
                   INITIALIZE                LNK-DAY6-AREA
                   MOVE    "61"              TO  LNK-DAY6-IRAI
                   MOVE   SYOKIKSN-IN-SANTEISTYMD
                                             TO  LNK-DAY6-KIJUN
                   MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                   MOVE    WK-TOKUTEI-DAY    TO  LNK-DAY6-ZOGEN
                   CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                     LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN
                                      TO WRK-SCNYUACCT-KAGEN-EDYUKYMD
                                                             (TN-IDX)
               END-IF
      *
               ADD    1   TO    TN-IDX
      *
               MOVE   "190813510"  TO  WRK-SCNYUACCT-KAGEN-SRYCD
                                                           (TN-IDX)
               INITIALIZE                STS-AREA-DAY
               INITIALIZE                LNK-DAY6-AREA
               MOVE    "61"              TO  LNK-DAY6-IRAI
               IF      TN-IDX      =     2
                   MOVE    WRK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                         TO  LNK-DAY6-KIJUN
                   MOVE    "1"           TO  LNK-DAY6-ZOGENPTN
                   MOVE     1            TO  LNK-DAY6-ZOGEN
                   CALL  "ORCSDAY"       USING   STS-AREA-DAY
                                                 LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN
                                     TO WRK-SCNYUACCT-KAGEN-STYUKYMD
                                                             (TN-IDX)
               ELSE
                   IF   TN-IDX      =     3
                       MOVE    WRK-SCNYUACCT-KAGEN-EDYUKYMD(2)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                              LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                 TO WRK-SCNYUACCT-KAGEN-STYUKYMD
                                                         (TN-IDX)
                   ELSE
                       MOVE    PTNYUINRRK-TENNYUYMD
                                     TO WRK-SCNYUACCT-KAGEN-STYUKYMD
                                                             (TN-IDX)
                   END-IF
               END-IF
               MOVE    "99999999"    TO WRK-SCNYUACCT-KAGEN-EDYUKYMD
                                                             (TN-IDX)
      *
               MOVE    ZERO            TO  SC-IDX2
               PERFORM    VARYING    SC-IDX   FROM  1   BY   1
                 UNTIL  (WRK-SCNYUACCT-KAGEN-SRYCD(SC-IDX) = SPACE) OR
                        (SC-IDX        >    7)
                 IF     (WRK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX) <=
                                          LNK-SCNYUACCT-STSRYYMD)  AND
                        (WRK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX) >=
                                          LNK-SCNYUACCT-STSRYYMD)
                      ADD     1     TO   SC-IDX2
                      MOVE  WRK-SCNYUACCT-KAGEN-SRYCD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-SRYCD(SC-IDX2)
                      MOVE  LNK-SCNYUACCT-STSRYYMD  TO
                            LNK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX2)
                      MOVE  WRK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX2)
                 END-IF
                 IF     (WRK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX) >
                                          LNK-SCNYUACCT-STSRYYMD)
                      ADD     1     TO   SC-IDX2
                      MOVE  WRK-SCNYUACCT-KAGEN-SRYCD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-SRYCD(SC-IDX2)
                      MOVE  WRK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX2)
                      MOVE  WRK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX2)
                 END-IF
               END-PERFORM
             END-IF
           END-IF
      *
      *    緩和ケア病棟入院料（平成２４年４月からの期間対応）
           IF      PTNYUINRRK-TENNYUYMD    >=  "20120401"
             IF      SYOKIKSN-IN-KHNSRYCD    =   "190028910"
               MOVE   SPACE            TO  SYOKIKSN-IN-KHNSRYCD
               MOVE   SPACE            TO  LNK-SCNYUACCT-NYUKHN-SRYCD
                                                                    (1)
               MOVE   SPACE            TO  WRK-SCNYUACCT-AREA
               INITIALIZE                  WRK-SCNYUACCT-AREA
      *
      *        緩和ケアの算定開始日を検索
               MOVE   SPACE            TO  WRK-TOKUTEI-TENNYUYMD
               INITIALIZE                  WWK-PTNYUINRRK-REC
               MOVE   SPA-HOSPNUM      TO  WWK-PTNYUINRRK-HOSPNUM
               MOVE   PTNYUINRRK-PTID  TO  WWK-PTNYUINRRK-PTID
               MOVE   PTNYUINRRK-RRKNUM TO WWK-PTNYUINRRK-RRKNUM
               PERFORM    980-PTNYUINRRK-SELECT-SEC
               IF     FLG-WWK-PTNYUINRRK   =   ZERO
                   MOVE  "tbl_ptnyuinrrk"  TO  MCP-TABLE
                   MOVE  "key4"            TO  MCP-PATHNAME
                   PERFORM 980-PTNYUINRRK-READ-SEC
                   IF   ( PTNYUINRRK-TOKUTEI-KBN  =
                                   WWK-PTNYUINRRK-TOKUTEI-KBN ) AND
                        ( PTNYUINRRK-TOKUTEI-NYUIN  =
                                   WWK-PTNYUINRRK-TOKUTEI-NYUIN )
                       MOVE   WWK-PTNYUINRRK-TENNYUYMD
                                           TO   WRK-TOKUTEI-TENNYUYMD
                   END-IF
                   PERFORM   UNTIL  ( FLG-WWK-PTNYUINRRK   =   1 )  OR
                                    ( PTNYUINRRK-TOKUTEI-KBN NOT =
                                       WWK-PTNYUINRRK-TOKUTEI-KBN ) OR
                                    ( PTNYUINRRK-TOKUTEI-NYUIN NOT =
                                       WWK-PTNYUINRRK-TOKUTEI-NYUIN )
                       MOVE  "tbl_ptnyuinrrk"  TO  MCP-TABLE
                       MOVE  "key4"            TO  MCP-PATHNAME
                       PERFORM 980-PTNYUINRRK-READ-SEC
                       IF   ( PTNYUINRRK-TOKUTEI-KBN  =
                                       WWK-PTNYUINRRK-TOKUTEI-KBN ) AND
                            ( PTNYUINRRK-TOKUTEI-NYUIN  =
                                       WWK-PTNYUINRRK-TOKUTEI-NYUIN )
                           MOVE   WWK-PTNYUINRRK-TENNYUYMD
                                            TO   WRK-TOKUTEI-TENNYUYMD
                       END-IF
                   END-PERFORM
               END-IF
               MOVE  "tbl_ptnyuinrrk"  TO  MCP-TABLE
               MOVE  "key4"            TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
               MOVE   "190028910"      TO  WRK-SCNYUACCT-KAGEN-SRYCD(1)
      **         MOVE   PTNYUINRRK-TENNYUYMD
               MOVE   WRK-TOKUTEI-TENNYUYMD
                                       TO  WRK-SCNYUACCT-KAGEN-STYUKYMD
                                                                    (1)
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
      **         MOVE   PTNYUINRRK-TENNYUYMD
               MOVE   WRK-TOKUTEI-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE    29              TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  WRK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                   (1)
      *
               MOVE   "190150510"      TO  WRK-SCNYUACCT-KAGEN-SRYCD(2)
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    WRK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                       TO  LNK-DAY6-KIJUN
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE     1              TO  LNK-DAY6-ZOGEN
               CALL  "ORCSDAY"         USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  WRK-SCNYUACCT-KAGEN-STYUKYMD
                                                                  (2)
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
      **         MOVE   PTNYUINRRK-TENNYUYMD
               MOVE   WRK-TOKUTEI-TENNYUYMD
                                       TO  LNK-DAY6-KIJUN
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE    59              TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  WRK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                   (2)
      *
               MOVE   "190150610"      TO  WRK-SCNYUACCT-KAGEN-SRYCD(3)
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    WRK-SCNYUACCT-KAGEN-EDYUKYMD(2)
                                       TO  LNK-DAY6-KIJUN
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE     1              TO  LNK-DAY6-ZOGEN
               CALL  "ORCSDAY"         USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  WRK-SCNYUACCT-KAGEN-STYUKYMD
                                                                  (3)
               MOVE    "99999999"      TO  WRK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                  (3)
      *
               MOVE    ZERO            TO  SC-IDX2
               PERFORM    VARYING    SC-IDX   FROM  1   BY   1
                 UNTIL  (WRK-SCNYUACCT-KAGEN-SRYCD(SC-IDX) = SPACE) OR
                        (SC-IDX        >    7)
                 IF     (WRK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX) <=
                                          LNK-SCNYUACCT-STSRYYMD)  AND
                        (WRK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX) >=
                                          LNK-SCNYUACCT-STSRYYMD)
                      ADD     1     TO   SC-IDX2
                      MOVE  WRK-SCNYUACCT-KAGEN-SRYCD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-SRYCD(SC-IDX2)
                      MOVE  LNK-SCNYUACCT-STSRYYMD  TO
                            LNK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX2)
                      MOVE  WRK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX2)
                 END-IF
                 IF     (WRK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX) >
                                          LNK-SCNYUACCT-STSRYYMD)
                      ADD     1     TO   SC-IDX2
                      MOVE  WRK-SCNYUACCT-KAGEN-SRYCD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-SRYCD(SC-IDX2)
                      MOVE  WRK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX2)
                      MOVE  WRK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX2)
                 END-IF
               END-PERFORM
             END-IF
           END-IF
      *
      *    広範囲熱傷特定集中治療室管理料
           IF      SYOKIKSN-IN-KHNSRYCD    =   "190024810"
               MOVE   SPACE                TO  IPN-NYUKAIKEI-SAKUSEI-FLG
               MOVE   WRK-PARA-SRYYM       TO  PTNYUINRRK-TENNYUYMD(1:6)
               MOVE   "01"                 TO  PTNYUINRRK-TENNYUYMD(7:2)
               MOVE   SPACE                TO  SYOKIKSN-IN-KHNSRYCD
                                           LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *        広範囲熱傷特定集中治療室管理料の算定可能日数を計算
               MOVE     ZERO               TO  WK-TOKUTEI-DAY
               COMPUTE  WK-TOKUTEI-DAY     =   60  -    STUSAN-NISSU06
               IF       STUSAN-NISSU06     <   60
                   MOVE   "190024810"          TO
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
      *    亜急性期入院医療管理料１（９０日限度）
           IF   PTNYUINRRK-NYUINYMD   <=   "20120331"
             IF   SYOKIKSN-IN-KHNSRYCD    =   "190117410"
               MOVE    SPACE               TO  IPN-NYUKAIKEI-SAKUSEI-FLG
               MOVE    WRK-PARA-SRYYM      TO  PTNYUINRRK-TENNYUYMD(1:6)
               MOVE    "01"                TO  PTNYUINRRK-TENNYUYMD(7:2)
               MOVE    SPACE               TO  SYOKIKSN-IN-KHNSRYCD
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
               MOVE    "71"                TO  LNK-DAY7-IRAI
               MOVE    WRK-IPN-HANTEI-YM1  TO  LNK-DAY7-YM
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
               MOVE    LNK-DAY7-KOYOMI     TO  WRK-IPN-HANTEI-YMD1
               MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  WRK-IPN-HANTEI-YMD2
      *          一般入院会計作成用のフラグ設定
               IF     (WRK-IPN-HANTEI-YMD1      >  WRK-IPN-HANTEI-YMD2)
                 AND  (WRK-IPN-HANTEI-YMD2(1:6) =  WRK-PARA-SRYYM)
                   MOVE    "1"             TO  IPN-NYUKAIKEI-SAKUSEI-FLG
               END-IF
             END-IF
           END-IF
      *
      *    亜急性期入院医療管理料１（平成２４年改正から６０日限度）
           IF   PTNYUINRRK-NYUINYMD   >=   "20120401"
             IF      SYOKIKSN-IN-KHNSRYCD    =   "190117410"
               MOVE   SPACE                TO  IPN-NYUKAIKEI-SAKUSEI-FLG
               MOVE   WRK-PARA-SRYYM       TO  PTNYUINRRK-TENNYUYMD(1:6)
               MOVE   "01"                 TO  PTNYUINRRK-TENNYUYMD(7:2)
               MOVE   SPACE                TO  SYOKIKSN-IN-KHNSRYCD
                                           LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *        亜急性期入院医療管理料１の算定可能日数を計算
               MOVE     ZERO               TO  WK-TOKUTEI-DAY
               COMPUTE  WK-TOKUTEI-DAY     =   60  -    STUSAN-NISSU06
               IF       STUSAN-NISSU06     <   60
                   MOVE   "190117410"          TO
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
           END-IF
      *
      *    亜急性期入院医療管理料２
           IF      SYOKIKSN-IN-KHNSRYCD    =   "190129910"
             IF (PTNYUINRRK-NYUINYMD   >=   "20120401")    OR
               ((PTNYUINRRK-NYUINYMD   <=   "20120331")    AND
                (LNK-AKYUSEI-RC        NOT =  1  ))
      *          ６０日限度
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
             ELSE
      *        経過措置により９０日限度
               MOVE    SPACE               TO  IPN-NYUKAIKEI-SAKUSEI-FLG
               MOVE    WRK-PARA-SRYYM      TO  PTNYUINRRK-TENNYUYMD(1:6)
               MOVE    "01"                TO  PTNYUINRRK-TENNYUYMD(7:2)
               MOVE    SPACE               TO  SYOKIKSN-IN-KHNSRYCD
                                           LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *        亜急性期入院医療管理料２の算定可能日数を計算
               MOVE    ZERO                TO  WK-TOKUTEI-DAY
               COMPUTE WK-TOKUTEI-DAY      =   90  -   STUSAN-NISSU06
               IF      STUSAN-NISSU06      <   90
                   MOVE   "190129910"      TO
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
               MOVE    "71"                TO  LNK-DAY7-IRAI
               MOVE    WRK-IPN-HANTEI-YM1  TO  LNK-DAY7-YM
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
               MOVE    LNK-DAY7-KOYOMI     TO  WRK-IPN-HANTEI-YMD1
               MOVE    LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                           TO  WRK-IPN-HANTEI-YMD2
      *          一般入院会計作成用のフラグ設定
               IF     (WRK-IPN-HANTEI-YMD1      >  WRK-IPN-HANTEI-YMD2)
                 AND  (WRK-IPN-HANTEI-YMD2(1:6) =  WRK-PARA-SRYYM)
                   MOVE    "1"             TO  IPN-NYUKAIKEI-SAKUSEI-FLG
               END-IF
             END-IF
           END-IF
      *
      *    亜急性期入院医療管理料１（指定地域）
           IF      SYOKIKSN-IN-KHNSRYCD    =   "190150310"
               MOVE   SPACE                TO  IPN-NYUKAIKEI-SAKUSEI-FLG
               MOVE   WRK-PARA-SRYYM       TO  PTNYUINRRK-TENNYUYMD(1:6)
               MOVE   "01"                 TO  PTNYUINRRK-TENNYUYMD(7:2)
               MOVE   SPACE                TO  SYOKIKSN-IN-KHNSRYCD
                                           LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *        亜急性期入院医療管理料１（指定地域）の算定可能日数を計算
               MOVE     ZERO               TO  WK-TOKUTEI-DAY
               COMPUTE  WK-TOKUTEI-DAY     =   60  -    STUSAN-NISSU06
               IF       STUSAN-NISSU06     <   60
                   MOVE   "190150310"          TO
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
      *    亜急性期入院医療管理料２（指定地域）
           IF      SYOKIKSN-IN-KHNSRYCD    =   "190150410"
               MOVE   SPACE                TO  IPN-NYUKAIKEI-SAKUSEI-FLG
               MOVE   WRK-PARA-SRYYM       TO  PTNYUINRRK-TENNYUYMD(1:6)
               MOVE   "01"                 TO  PTNYUINRRK-TENNYUYMD(7:2)
               MOVE   SPACE                TO  SYOKIKSN-IN-KHNSRYCD
                                           LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *        亜急性期入院医療管理料２（指定地域）の算定可能日数を計算
               MOVE     ZERO               TO  WK-TOKUTEI-DAY
               COMPUTE  WK-TOKUTEI-DAY     =   60  -    STUSAN-NISSU06
               IF       STUSAN-NISSU06     <   60
                   MOVE   "190150410"          TO
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
      *    特定一般病棟入院料（亜急性期入院医療管理）
           IF      SYOKIKSN-IN-KHNSRYCD    =   "190152610"
               MOVE   SPACE                TO  IPN-NYUKAIKEI-SAKUSEI-FLG
               MOVE   WRK-PARA-SRYYM       TO  PTNYUINRRK-TENNYUYMD(1:6)
               MOVE   "01"                 TO  PTNYUINRRK-TENNYUYMD(7:2)
               MOVE   SPACE                TO  SYOKIKSN-IN-KHNSRYCD
                                           LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *        特定一般病棟入院料（亜急性期入院医療管理）の算定可能日数を計算
               MOVE     ZERO               TO  WK-TOKUTEI-DAY
               COMPUTE  WK-TOKUTEI-DAY     =   60  -    STUSAN-NISSU06
               IF       STUSAN-NISSU06     <   60
                   MOVE   "190152610"          TO
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
      *    特定一般病棟入院料（亜急性期入院医療管理・リハビリ算定患者）
           IF      SYOKIKSN-IN-KHNSRYCD    =   "190152710"
               MOVE   SPACE                TO  IPN-NYUKAIKEI-SAKUSEI-FLG
               MOVE   WRK-PARA-SRYYM       TO  PTNYUINRRK-TENNYUYMD(1:6)
               MOVE   "01"                 TO  PTNYUINRRK-TENNYUYMD(7:2)
               MOVE   SPACE                TO  SYOKIKSN-IN-KHNSRYCD
                                           LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      *        特定一般病棟入院料（亜急性期入院医療管理・リハビリ算定患者）
      *        の算定可能日数を計算
               MOVE     ZERO               TO  WK-TOKUTEI-DAY
               COMPUTE  WK-TOKUTEI-DAY     =   60  -    STUSAN-NISSU06
               IF       STUSAN-NISSU06     <   60
                   MOVE   "190152710"          TO
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
      *    精神科救急入院料１
           IF      SYOKIKSN-IN-KHNSRYCD    =   "190121010"
               MOVE   SPACE            TO  SYOKIKSN-IN-KHNSRYCD
               MOVE   SPACE            TO  LNK-SCNYUACCT-NYUKHN-SRYCD
                                                                    (1)
               MOVE   "190121010"      TO  WRK-SCNYUACCT-KAGEN-SRYCD(1)
               MOVE   PTNYUINRRK-TENNYUYMD
                                       TO  WRK-SCNYUACCT-KAGEN-STYUKYMD
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
               MOVE    LNK-DAY6-KEISAN TO  WRK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                   (1)
      *
               MOVE   "190111410"      TO  WRK-SCNYUACCT-KAGEN-SRYCD(2)
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    WRK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                       TO  LNK-DAY6-KIJUN
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE     1              TO  LNK-DAY6-ZOGEN
               CALL  "ORCSDAY"         USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  WRK-SCNYUACCT-KAGEN-STYUKYMD
                                                                  (2)
               MOVE    "99999999"      TO  WRK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                  (2)
      *
               MOVE    ZERO            TO  SC-IDX2
               PERFORM    VARYING    SC-IDX   FROM  1   BY   1
                 UNTIL  (WRK-SCNYUACCT-KAGEN-SRYCD(SC-IDX) = SPACE) OR
                        (SC-IDX        >    7)
                 IF     (WRK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX) <=
                                          LNK-SCNYUACCT-STSRYYMD)  AND
                        (WRK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX) >=
                                          LNK-SCNYUACCT-STSRYYMD)
                      ADD     1     TO   SC-IDX2
                      MOVE  WRK-SCNYUACCT-KAGEN-SRYCD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-SRYCD(SC-IDX2)
                      MOVE  LNK-SCNYUACCT-STSRYYMD  TO
                            LNK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX2)
                      MOVE  WRK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX2)
                 END-IF
                 IF     (WRK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX) >
                                          LNK-SCNYUACCT-STSRYYMD)
                      ADD     1     TO   SC-IDX2
                      MOVE  WRK-SCNYUACCT-KAGEN-SRYCD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-SRYCD(SC-IDX2)
                      MOVE  WRK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX2)
                      MOVE  WRK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX2)
                 END-IF
               END-PERFORM
           END-IF
      *
      *    精神科救急入院料２
           IF      SYOKIKSN-IN-KHNSRYCD    =   "190130010"
               MOVE   SPACE            TO  SYOKIKSN-IN-KHNSRYCD
               MOVE   SPACE            TO  LNK-SCNYUACCT-NYUKHN-SRYCD
                                                                    (1)
               MOVE   "190130010"      TO  WRK-SCNYUACCT-KAGEN-SRYCD(1)
               MOVE   PTNYUINRRK-TENNYUYMD
                                       TO  WRK-SCNYUACCT-KAGEN-STYUKYMD
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
               MOVE    LNK-DAY6-KEISAN TO  WRK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                   (1)
      *
               MOVE   "190130110"      TO  WRK-SCNYUACCT-KAGEN-SRYCD(2)
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    WRK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                       TO  LNK-DAY6-KIJUN
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE     1              TO  LNK-DAY6-ZOGEN
               CALL  "ORCSDAY"         USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  WRK-SCNYUACCT-KAGEN-STYUKYMD
                                                                  (2)
               MOVE    "99999999"      TO  WRK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                  (2)
      *
               MOVE    ZERO            TO  SC-IDX2
               PERFORM    VARYING    SC-IDX   FROM  1   BY   1
                 UNTIL  (WRK-SCNYUACCT-KAGEN-SRYCD(SC-IDX) = SPACE) OR
                        (SC-IDX        >    7)
                 IF     (WRK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX) <=
                                          LNK-SCNYUACCT-STSRYYMD)  AND
                        (WRK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX) >=
                                          LNK-SCNYUACCT-STSRYYMD)
                      ADD     1     TO   SC-IDX2
                      MOVE  WRK-SCNYUACCT-KAGEN-SRYCD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-SRYCD(SC-IDX2)
                      MOVE  LNK-SCNYUACCT-STSRYYMD  TO
                            LNK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX2)
                      MOVE  WRK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX2)
                 END-IF
                 IF     (WRK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX) >
                                          LNK-SCNYUACCT-STSRYYMD)
                      ADD     1     TO   SC-IDX2
                      MOVE  WRK-SCNYUACCT-KAGEN-SRYCD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-SRYCD(SC-IDX2)
                      MOVE  WRK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX2)
                      MOVE  WRK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX2)
                 END-IF
               END-PERFORM
           END-IF
      *
      *    精神科救急・合併症入院料
           IF      SYOKIKSN-IN-KHNSRYCD    =   "190130210"
               MOVE   SPACE            TO  SYOKIKSN-IN-KHNSRYCD
               MOVE   SPACE            TO  LNK-SCNYUACCT-NYUKHN-SRYCD
                                                                    (1)
               MOVE   "190130210"      TO  WRK-SCNYUACCT-KAGEN-SRYCD(1)
               MOVE   PTNYUINRRK-TENNYUYMD
                                       TO  WRK-SCNYUACCT-KAGEN-STYUKYMD
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
               MOVE    LNK-DAY6-KEISAN TO  WRK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                   (1)
      *
               MOVE   "190130310"      TO  WRK-SCNYUACCT-KAGEN-SRYCD(2)
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    WRK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                       TO  LNK-DAY6-KIJUN
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE     1              TO  LNK-DAY6-ZOGEN
               CALL  "ORCSDAY"         USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  WRK-SCNYUACCT-KAGEN-STYUKYMD
                                                                  (2)
               MOVE    "99999999"      TO  WRK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                  (2)
      *
               MOVE    ZERO            TO  SC-IDX2
               PERFORM    VARYING    SC-IDX   FROM  1   BY   1
                 UNTIL  (WRK-SCNYUACCT-KAGEN-SRYCD(SC-IDX) = SPACE) OR
                        (SC-IDX        >    7)
                 IF     (WRK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX) <=
                                          LNK-SCNYUACCT-STSRYYMD)  AND
                        (WRK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX) >=
                                          LNK-SCNYUACCT-STSRYYMD)
                      ADD     1     TO   SC-IDX2
                      MOVE  WRK-SCNYUACCT-KAGEN-SRYCD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-SRYCD(SC-IDX2)
                      MOVE  LNK-SCNYUACCT-STSRYYMD  TO
                            LNK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX2)
                      MOVE  WRK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX2)
                 END-IF
                 IF     (WRK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX) >
                                          LNK-SCNYUACCT-STSRYYMD)
                      ADD     1     TO   SC-IDX2
                      MOVE  WRK-SCNYUACCT-KAGEN-SRYCD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-SRYCD(SC-IDX2)
                      MOVE  WRK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX2)
                      MOVE  WRK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX2)
                 END-IF
               END-PERFORM
           END-IF
      *
      *    精神科急性期治療病棟入院料１
           IF      SYOKIKSN-IN-KHNSRYCD    =   "190121110"
               MOVE   SPACE            TO  SYOKIKSN-IN-KHNSRYCD
               MOVE   SPACE            TO  LNK-SCNYUACCT-NYUKHN-SRYCD
                                                                    (1)
               MOVE   SPACE            TO  WRK-SCNYUACCT-AREA
               INITIALIZE                  WRK-SCNYUACCT-AREA
               MOVE   "190121110"      TO  WRK-SCNYUACCT-KAGEN-SRYCD(1)
               MOVE   PTNYUINRRK-TENNYUYMD
                                       TO  WRK-SCNYUACCT-KAGEN-STYUKYMD
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
               MOVE    LNK-DAY6-KEISAN TO  WRK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                   (1)
      *
               MOVE   "190066910"      TO  WRK-SCNYUACCT-KAGEN-SRYCD(2)
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    WRK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                       TO  LNK-DAY6-KIJUN
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE     1              TO  LNK-DAY6-ZOGEN
               CALL  "ORCSDAY"         USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  WRK-SCNYUACCT-KAGEN-STYUKYMD
                                                                  (2)
               MOVE    "99999999"      TO  WRK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                  (2)
      *
               MOVE    ZERO            TO  SC-IDX2
               PERFORM    VARYING    SC-IDX   FROM  1   BY   1
                 UNTIL  (WRK-SCNYUACCT-KAGEN-SRYCD(SC-IDX) = SPACE) OR
                        (SC-IDX        >    7)
                 IF     (WRK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX) <=
                                          LNK-SCNYUACCT-STSRYYMD)  AND
                        (WRK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX) >=
                                          LNK-SCNYUACCT-STSRYYMD)
                      ADD     1     TO   SC-IDX2
                      MOVE  WRK-SCNYUACCT-KAGEN-SRYCD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-SRYCD(SC-IDX2)
                      MOVE  LNK-SCNYUACCT-STSRYYMD  TO
                            LNK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX2)
                      MOVE  WRK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX2)
                 END-IF
                 IF     (WRK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX) >
                                          LNK-SCNYUACCT-STSRYYMD)
                      ADD     1     TO   SC-IDX2
                      MOVE  WRK-SCNYUACCT-KAGEN-SRYCD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-SRYCD(SC-IDX2)
                      MOVE  WRK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX2)
                      MOVE  WRK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX2)
                 END-IF
               END-PERFORM
           END-IF
      *
      *    精神科急性期治療病棟入院料２
           IF      SYOKIKSN-IN-KHNSRYCD    =   "190121210"
               MOVE   SPACE            TO  SYOKIKSN-IN-KHNSRYCD
               MOVE   SPACE            TO  LNK-SCNYUACCT-NYUKHN-SRYCD
                                                                    (1)
               MOVE   SPACE            TO  WRK-SCNYUACCT-AREA
               INITIALIZE                  WRK-SCNYUACCT-AREA
               MOVE   "190121210"      TO  WRK-SCNYUACCT-KAGEN-SRYCD(1)
               MOVE   PTNYUINRRK-TENNYUYMD
                                       TO  WRK-SCNYUACCT-KAGEN-STYUKYMD
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
               MOVE    LNK-DAY6-KEISAN TO  WRK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                   (1)
      *
               MOVE   "190067010"      TO  WRK-SCNYUACCT-KAGEN-SRYCD(2)
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    WRK-SCNYUACCT-KAGEN-EDYUKYMD(1)
                                       TO  LNK-DAY6-KIJUN
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE     1              TO  LNK-DAY6-ZOGEN
               CALL  "ORCSDAY"         USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  WRK-SCNYUACCT-KAGEN-STYUKYMD
                                                                  (2)
               MOVE    "99999999"      TO  WRK-SCNYUACCT-KAGEN-EDYUKYMD
                                                                  (2)
      *
               MOVE    ZERO            TO  SC-IDX2
               PERFORM    VARYING    SC-IDX   FROM  1   BY   1
                 UNTIL  (WRK-SCNYUACCT-KAGEN-SRYCD(SC-IDX) = SPACE) OR
                        (SC-IDX        >    7)
                 IF     (WRK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX) <=
                                          LNK-SCNYUACCT-STSRYYMD)  AND
                        (WRK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX) >=
                                          LNK-SCNYUACCT-STSRYYMD)
                      ADD     1     TO   SC-IDX2
                      MOVE  WRK-SCNYUACCT-KAGEN-SRYCD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-SRYCD(SC-IDX2)
                      MOVE  LNK-SCNYUACCT-STSRYYMD  TO
                            LNK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX2)
                      MOVE  WRK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX2)
                 END-IF
                 IF     (WRK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX) >
                                          LNK-SCNYUACCT-STSRYYMD)
                      ADD     1     TO   SC-IDX2
                      MOVE  WRK-SCNYUACCT-KAGEN-SRYCD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-SRYCD(SC-IDX2)
                      MOVE  WRK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-STYUKYMD(SC-IDX2)
                      MOVE  WRK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX)  TO
                            LNK-SCNYUACCT-KAGEN-EDYUKYMD(SC-IDX2)
                 END-IF
               END-PERFORM
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
                   PERFORM 900-CLOSE-SEC
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
                   PERFORM 900-CLOSE-SEC
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
           MOVE   WRK-TAIHI-SHOKUJI    TO  LNK-SCNYUACCT-SHOKUJIKBN
      *    食堂加算
           PERFORM 4801-BTUSEL-SEC
           IF      FLG-SYSKANRI        =    ZERO
               IF      SYS-5001-BTU-SYOKUDOKBN     =    "2"
                   MOVE    "197000570"     TO
                                           LNK-SCNYUACCT-SHOKUDO-SRYCD
                   MOVE    1               TO  LNK-SCNYUACCT-SHOKUDO-KBN
                   IF      WRK-TAIHI-SHOKUDOU   =   0
                        MOVE   0           TO  LNK-SCNYUACCT-SHOKUDO-KBN
                   END-IF
               END-IF
           END-IF
      *    保険組合せ
           MOVE    "099999914"     TO  LNK-SCNYUACCT-HKNCOMBINUM-SRYCD
           MOVE    WRK-TAIHI-HKN   TO  LNK-SCNYUACCT-HKNCOMBINUM
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
      *    有床診療所入院基本料の相互算定コード置き換え
           IF     LNK-SCNYUACCT-STSRYYMD   >=   "20120401"
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
      *     DISPLAY   "********ORCSNYUACCT MAE **********"
           MOVE    WRK-PTID        TO  SPA-PTID
           MOVE    "00"            TO  SPA-SRYKA
           MOVE    "1"             TO  SPA-NYUGAIKBN
           CALL    "ORCSNYUACCT"       USING   SPA-AREA
                                               ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA      TO  SPA-SRYKA
      *
      *     入院行為照会用テーブル作成サブ呼び出し
            INITIALIZE                  SS010-AREA
            MOVE    WRK-PTID            TO  SS010-PTID
            MOVE    WRK-PARA-SRYYM      TO  SS010-SRYYM
            CALL    "ORCSS010"      USING   SS010-AREA
                                            SPA-AREA
            IF    ( SS010-RC        NOT =   ZERO )
                MOVE    "2007"          TO  WRK-ERRCD
            END-IF
      *
           .
      *
       410191-NYUACCT-SAKUSEI-EXT.
           EXIT.
      *
      *21118
      *****************************************************************
      *    入院会計テーブル作成処理（特定入院料後の一般入院料）
      *****************************************************************
       410191-IPNNYUACCT-SAKUSEI-SEC   SECTION.
      *
           DISPLAY "PTID = " WRK-PTID
      *     DISPLAY   "410191-NYUACCT-SAKUSEI-SEC"
           INITIALIZE      ORCSSYOKIKSNAREA
           MOVE    IPN-SANTEISTYMD(1:6)  TO WRK-NAI-SRYYM-41(1:6)
      *
      *    通算日数を求める（入院履歴を検索）
           MOVE    ZERO                TO  WRK-TSUSAN
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
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
      *         DISPLAY  "PTNYUINRRK-RRKNUM = " PTNYUINRRK-RRKNUM
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
      *     DISPLAY  "PTNYUINRRK-PTID = " PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-NYUINKA  TO  SPA-SRYKA
           MOVE    SYOKIKSN-IN-KHNSRYCD
                                       TO  LNK-SCNYUACCT-NYUKHN-SRYCD(1)
           MOVE      SYOKIKSN-IN-SANTEISTYMD
                                       TO  LNK-SCNYUACCT-STSRYYMD
                                           LNK-SCNYUACCT-EDSRYYMD
      *     DISPLAY "LNK-SCNYUACCT-STSRYYMD = " LNK-SCNYUACCT-STSRYYMD
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
           MOVE    WRK-TAIHI-SHOKUJI   TO  LNK-SCNYUACCT-SHOKUJIKBN
      *    食堂加算
           PERFORM 4801-BTUSEL-SEC
           IF      FLG-SYSKANRI           =    ZERO
               IF      SYS-5001-BTU-SYOKUDOKBN     =    "2"
                   MOVE    "197000570"     TO
                                           LNK-SCNYUACCT-SHOKUDO-SRYCD
                   MOVE    1               TO  LNK-SCNYUACCT-SHOKUDO-KBN
                   IF      WRK-TAIHI-SHOKUDOU   =   0
                        MOVE   0           TO  LNK-SCNYUACCT-SHOKUDO-KBN
                   END-IF
               END-IF
           END-IF
      *    保険組合せ
           MOVE    "099999914"     TO  LNK-SCNYUACCT-HKNCOMBINUM-SRYCD
           MOVE    WRK-TAIHI-HKN   TO  LNK-SCNYUACCT-HKNCOMBINUM
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
           MOVE    WRK-PTID        TO  SPA-PTID
           MOVE    "00"            TO  SPA-SRYKA
           MOVE    "1"             TO  SPA-NYUGAIKBN
           CALL   "ORCSNYUACCT"    USING   SPA-AREA
                                           ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA  TO  SPA-SRYKA
      *
      *     入院行為照会用テーブル作成サブ呼び出し
            INITIALIZE                  SS010-AREA
            MOVE    WRK-PTID            TO  SS010-PTID
            MOVE    SYOKIKSN-IN-SANTEISTYMD(1:6)
                                        TO  SS010-SRYYM
            CALL    "ORCSS010"      USING   SS010-AREA
                                            SPA-AREA
            IF    ( SS010-RC        NOT =   ZERO )
                MOVE    "2007"          TO  WRK-ERRCD
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
           MOVE    SPA-HOSPNUM     TO  STUSAN-HOSPNUM
           MOVE    PTNYUINRRK-PTID TO  STUSAN-PTID
           MOVE    WRK-PARA-SRYYM  TO  STUSAN-KJNYMD(1:6)
           IF   IPN-NYUKAIKEI-SAKUSEI-FLG  =   "1"
               MOVE   IPN-SANTEISTYMD(7:2) TO  STUSAN-KJNYMD(7:2)
           ELSE
               MOVE    "01"                TO  STUSAN-KJNYMD(7:2)
           END-IF
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
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    WRK-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
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
      *     DISPLAY "LNK-SCNYUACCT-SENTEI-SRYCD = "
      *              LNK-SCNYUACCT-SENTEI-SRYCD
      *     DISPLAY "LNK-SCNYUACCT-SENTEI-STYMD = "
      *              LNK-SCNYUACCT-SENTEI-STYMD
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
      *     DISPLAY "410191-KIKAN-CALC-SEC WRK-STYMD = " WRK-STYMD
      *     DISPLAY "410191-KIKAN-CALC-SEC WRK-EDYMD = " WRK-EDYMD
      *     DISPLAY "410191-KIKAN-CALC-SEC WRK-NISSU2 = " WRK-NISSU2
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
           MOVE    PTNYUINRRK-BTUNUM        TO  LNK-TOKNYUIN-BTUNUM
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
           DISPLAY "410191-NYUKSN-GET-SEC"
           INITIALIZE                       WRK-NYUKHNKSN-AREA
           MOVE    ZERO                TO   IDX8
      *
      *医療機関加算（減算）情報
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    WRK-PARA-SRYYM      TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
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
                    ADD    1                   TO  IDX8
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
           DISPLAY "4801-BTUSEL-SEC"
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
               IF    SYOKIKSN-IN-KHNSRYCD      =   "190129310"  OR
                                                   "190111110"  OR
                                                   "190111210"  OR
                                                   "190075810"  OR
                                                   "190140710"
                   IF    SYS-5001-BTU-HIKSISUKBN   =   "2"
                       MOVE   "2"    TO   WRK-BTU-HIKSISUKBN
                   END-IF
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
      *                            一般病棟看護必要度評価加算（特定一般病棟入院料１）
                                   IF SYOKIKSN-IN-KHNSRYCD = "190151910"
                                     ADD    1            TO  IDX8
                                     MOVE   "190152570"  TO
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
      *病室入院料加算
           PERFORM     4801-BRMSEL-SEC
      *
           IF   FLG-SYSKANRI       =    ZERO
      *
               PERFORM   VARYING   IDX7   FROM   1  BY  1
                 UNTIL    (IDX7                    >   12)
                     IF   SYS-5002-KSNKBN(IDX7)   =   "2"
                          ADD    1                TO  IDX8
                          MOVE   SYS-5002-KSNCD(IDX7)  TO
                                             WRK-NYUKHNKSN-SRYCD(IDX8)
                     END-IF
               END-PERFORM
      *
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
                                               NYUKSNCHK-YUKOEDYMD
      *
               MOVE    NYUKSNCHK-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuksnchk"     TO  WRK-MCP-TABLE
               MOVE    "key"               TO  WRK-MCP-PATHNAME
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
      *    表用番号が同一であることの判定
           IF   WRK-KSN-CDKBN-KBNNUM    NOT =    ZERO
              IF  (WRK-BTU-CDKBN-KBNNUM  = WRK-KSN-CDKBN-KBNNUM)  AND
                  (WRK-BTU-CDKBN-KBNNUM-EDA = WRK-KSN-CDKBN-KBNNUM-EDA)
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
           MOVE    ZERO            TO  WRK-BTU-CDKBN-KBNNUM
           MOVE    ZERO            TO  WRK-BTU-CDKBN-KBNNUM-EDA
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
                                       TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
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
           MOVE    TNS-CDKBN-KBNNUM TO  WRK-BTU-CDKBN-KBNNUM
           MOVE    TNS-CDKBN-KBNNUM-EDA TO WRK-BTU-CDKBN-KBNNUM-EDA
      *    療養病床以外・有床診療所療養病床入院基本料のとき、
      *    表用番号の置き換え
           IF     (TNS-CDKBN-KBNNUM  = 108)   AND
                  (TNS-CDKBN-KOUBAN  =  10)   AND
                  (TNS-YUKOSTYMD    >=  "20120401")
               MOVE   109           TO WRK-BTU-CDKBN-KBNNUM
               MOVE   ZERO          TO WRK-BTU-CDKBN-KBNNUM-EDA
           END-IF
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
           IF  TNS-SRYCD      =    "190799510"
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
           MOVE    ZERO            TO  WRK-KSN-CDKBN-KBNNUM
           MOVE    ZERO            TO  WRK-KSN-CDKBN-KBNNUM-EDA
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
                                       TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
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
               MOVE  TNS-CDKBN-KBNNUM TO  WRK-KSN-CDKBN-KBNNUM
               MOVE  TNS-CDKBN-KBNNUM-EDA
                                      TO  WRK-KSN-CDKBN-KBNNUM-EDA
           END-IF
      *
           IF      (TNS-CDKBN-KBN       =   "A")     AND
                   (TNS-CDKBN-KBNNUM   >=   300)     AND
                   (TNS-CDKBN-KBNNUM   <=   399)
               MOVE  TNS-CDKBN-KBNNUM TO  WRK-KSN-CDKBN-KBNNUM
               MOVE  TNS-CDKBN-KBNNUM-EDA
                                      TO  WRK-KSN-CDKBN-KBNNUM-EDA
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
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  AGECHK-PTID
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                                       TO  AGECHK-SRYYMD
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    WRK-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
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
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  WRK-MCP-TABLE
           MOVE    "key10"         TO  WRK-MCP-PATHNAME
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    =   ZERO )
               MOVE    MCPDATA-REC
                                  TO  SYS-5001-REC
           ELSE
               MOVE    1          TO  FLG-SYSKANRI
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
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
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
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    WRK-PTID            TO  AGECHK-PTID
           MOVE    SYOKIKSN-IN-SANTEISTYMD
                                       TO  AGECHK-SRYYMD
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    WRK-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
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
                       MOVE    SPA-HOSPNUM     TO   CHG-HOSPNUM
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
                   MOVE    SPA-HOSPNUM     TO   CHG-HOSPNUM
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
           IF    ( MCP-RC          =   ZERO )
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
      *    患者入院履歴マスター検索
      *****************************************************************
       980-PTNYUINRRK-SELECT-SEC     SECTION.
      *
           MOVE    WWK-PTNYUINRRK-REC   TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key4"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    ZERO             TO  FLG-WWK-PTNYUINRRK
           ELSE
               MOVE    1                TO  FLG-WWK-PTNYUINRRK
               INITIALIZE                   WWK-PTNYUINRRK-REC
           END-IF
      *
           .
      *
       980-PTNYUINRRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴マスター読込
      *****************************************************************
       980-PTNYUINRRK-READ-SEC     SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  WWK-PTNYUINRRK-REC
               MOVE    ZERO            TO  FLG-WWK-PTNYUINRRK
           ELSE
               INITIALIZE                  WWK-PTNYUINRRK-REC
               MOVE    1               TO  FLG-WWK-PTNYUINRRK
           END-IF.
      *
       980-PTNYUINRRK-READ-EXT.
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
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
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
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 900-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
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
      *****************************************************************
      *    ＤＢ　ＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　ＳＴＡＲＴ処理
      *****************************************************************
       900-DBSTART-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSTART-EXT.
           EXIT.
