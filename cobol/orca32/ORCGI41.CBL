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
       PROGRAM-ID.                 ORCGI41.
      ********************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院会計照会
      *  コンポーネント名  : 入院会計カード入力（Ｉ４１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02.09.19    ひさなが      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *
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
      *    画面コントロール
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    会計照会共通パラメタ
           COPY    "I4COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "I4SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SPASCR          PIC 9(01).
           03  FLG-SRYKBN          PIC 9(01).
           03  FLG-NYUINACT        PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-NYUINACCT       PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-INPUTSET        PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
      *
           03  FLG-NAIYAK          PIC X(01).
           03  FLG-TONPUK          PIC X(01).
           03  FLG-GAIYAK          PIC X(01).
           03  FLG-SP              PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
           03  FLG-HENSYU          PIC 9(01).
           03  FLG-DOUJI           PIC 9(01).
      *
           03  FLG-YAKUSOKU        PIC 9(01).
      *
           03  FLG-UPD             PIC 9(01).
           03  FLG-SET             PIC 9(01).
           03  FLG-KAIKEI-SAKUSEI  PIC 9(01).
           03  FLG-DBRC            PIC 9(01).
           03  FLG-NYUKSN          PIC 9(01).
           03  FLG-SST             PIC 9(01).
           03  FLG-NYUINKHNRYO     PIC 9(01).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(04).
           03  IDX6                PIC 9(04).
           03  IDX7                PIC 9(04).
           03  IDX8                PIC 9(03).
           03  IDX9                PIC 9(03).
           03  IDY                 PIC 9(04).
           03  IDG                 PIC 9(04).
           03  IDH                 PIC 9(04).
           03  IDH2                PIC 9(04).
           03  IDH3                PIC 9(04).
           03  IDK                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDZ2                PIC 9(04).
           03  IDZ3                PIC 9(04).
           03  IDM                 PIC 9(04).
           03  IDX-H               PIC 9(04).
           03  IDX-D               PIC 9(02).
           03  IDXS1               PIC 9(04).
           03  IDW3                PIC 9(04).
           03  IDW4                PIC 9(04).
           03  IDW5                PIC 9(04).
           03  IDW6                PIC 9(04).
           03  IDW7                PIC 9(04).
           03  IDW8                PIC 9(04).
           03  IDW9                PIC 9(04).
           03  IDV                 PIC 9(04).
      *    点数マスタ予約コード領域
       01  WRK-YOYAKU.
           03  W-YOYAKU            PIC X(09)   VALUE   "099999916".
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYYMD.
               05  WRK-SRYYY       PIC 9(04).
               05  WRK-SRYMM       PIC 9(02).
               05  WRK-SRYDD       PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-GMN-SRYYM       PIC X(06).
           03  WRK-NAI-SRYYM       PIC X(06).
           03  WRK-ENDDD           PIC 9(02).
           03  WRK-DD              PIC 9(02).
           03  WRK-SRYKA           PIC X(02).
           03  WRK-BTU             PIC X(02).
           03  WRK-BTU-NAME        PIC X(20).
           03  WRK-KUMI-NAME       PIC X(32).
      *    パス名領域
           03  WRK-DBPATH          PIC X(20).
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
           03  WRK-SANTEISTYMD     PIC X(08).
           03  WRK-TOKUTEI-NYUIN   PIC X(02).
           03  WRK-SAGAKU-KBN      PIC X(02).
           03  WRK-SAGAKU-KBN9  REDEFINES  WRK-SAGAKU-KBN
                                   PIC 9(02).
           03  WRK-HKNCOMBINUM     PIC X(04).
           03  WRK-HKNCOMBINUM9   REDEFINES  WRK-HKNCOMBINUM
                                   PIC 9(04).
           03  WRK-TAIHI-SRYKA     PIC X(02).
           03  WRK-ZOGENPTN        PIC X(01).
           03  WRK-ZOGEN           PIC S9(05).
           03  WRK-KEISANYMD       PIC X(08).
      *    医療機関設定の入院料加算チェック用
           03  WRK-NYUKHNKSN-SRYCD PIC X(09)  OCCURS 20.
           03  WRK-NYUKSNKBN       PIC X(03).
           03  WRK-BTU-NYUKSNKBN   PIC X(03).
           03  WRK-KSN-NYUKSNKBN   PIC X(03).
           03  WRK-SRYCD           PIC X(09).
           03  WRK-STYUKYMD        PIC X(08).
           03  WRK-NYUINYM         PIC X(06).
      *
           03  WRK-TENSU-X.
               05  WRK-TENSU       PIC Z(07).
           03  WRK-KEI-X.
               05  WRK-KEI         PIC Z(07).
      *
           03  WRK-MEISYO          PIC X(40).
      *    入院履歴退避用ワーク
       01  WK-PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC"     REPLACING
                                //PTNYUINRRK-// BY //WK-PTNYUINRRK-//.
      *    数量表示用
           03  WRK-SURYO           PIC 9(05)V9(03).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(03).
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1    PIC ZZZZZ.
               05  WRK-SURYO-Z19   REDEFINES   WRK-SURYO-Z1
                                   PIC ZZZZ9.
               05  WRK-SURYO-Z2    PIC X(01).
               05  WRK-SURYO-Z3    PIC ZZZ.
               05  WRK-SURYO-Z3-X  REDEFINES   WRK-SURYO-Z3
                                   PIC X(03).
           03  WRK-SURYO-J         PIC X(18).
           03  WRK-SAGAKU.
               05  WRK-SAGAKU-Z1   PIC ------,--9.
               05  WRK-SAGAKU-X1   PIC X(02).
      *    分画数画面編集用
           03  WRK-BUNGSU-X.
               05  WRK-BUNGSU-Z1   PIC ZZZ.
           03  WRK-BUNGSU-X2.
               05  WRK-BUNGSU-Z2   PIC ZZZ.
      *    剤回数等画面編集用
           03  WRK-KAISU-H         PIC X(08).
           03  WRK-KEI-H           PIC X(08).
           03  WRK-ZAIKEI          PIC X(40).
           03  WRK-ZAIKEIJ         PIC X(80).
           03  WRK-IDG             PIC 9(04).
      *    保険組合せ名称
           03  WRK-HKNCOMBI        PIC X(04).
           03  WRK-HKNCOMBIMEI     PIC X(100).
           03  WRK-HKNKBN-MEI      PIC X(02).
      *
           03  WRK-ZAIKAISU        PIC 9(07).
      *    診療行為名称
           03  WRK-SRYSYUMEI       PIC X(40).
      *
           03  WRK-ZZZZ-X.
               05  WRK-ZZZZ        PIC ZZZ.
           03  WRK-ZZ-X.
               05  WRK-ZZ          PIC ZZ.
      *    行選択用
           03  WRK-SELNUM          PIC 9(03).
           03  WRK-SELNUM2         PIC 9(03).
      *
           03  WRK-TBANGO          PIC 9(04).
      *
           03  WRK-MOTOPG          PIC X(08).
      *
           03  WRK-ERRMSG          PIC X(80).
           03  WRK-JIDMSG.
               05  WRK-JIDMSG1     PIC X(40).
               05  WRK-JIDMSG2     PIC X(40).
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
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *    表示明細変更作業
       01  WRK-GMN-DSP-TBL.
           03  WRK-GMN-DAY-G.
               05  WRK-GMN-DAY     PIC 9(03)  OCCURS  31.
           03  WRK-CHK-TBL.
               05  WRK-CHK-DAY     PIC 9(03)  OCCURS  31.
           03  WRK-CHK-ZAISKBKBN   PIC X(01).
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *    スパ領域 ワーク
           COPY    "COMMON-SPA"           REPLACING
                                //SPA-// BY //WRK-SPA-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1006.INC".
      *
           COPY    "CPSK5000.INC".
      *
           COPY    "CPSK5001.INC".
      *
           COPY    "CPSK5002.INC".
      *
           COPY    "CPSK5113.INC".
      *
      *    点数マスタ−
           COPY    "CPTENSU.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    入院行為マスタ−
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    入院会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    入力セットコード
       01  INPUTSET-REC.
           COPY    "CPINPUTSET.INC".
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    入院料加算チェック
       01  NYUKSNCHK-REC.
           COPY    "CPNYUKSNCHK.INC".
      *
      *    老人一般コード変換
        01  SRYCDCHG-REC.
            COPY    "CPSRYCDCHG.INC".
      *
      *    選定療養費
       01  SENTEICDCHG-REC.
           COPY    "CPSENTEICDCHG.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *   診療科算定履歴更新サブ
           COPY    "CPORCSKARRK.INC".
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
      *    排他制御パラメタ
           COPY    "CPORCSLOCK.INC".
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
           COPY    "I41.INC".
           COPY    "I42.INC".
           COPY    "I43.INC".
           COPY    "I44.INC".
           COPY    "I45.INC".
           COPY    "I46.INC".
           COPY    "I47.INC".
           COPY    "I48.INC".
           COPY    "I49.INC".
           COPY    "I4ERR.INC".
           COPY    "I4ID1.INC".
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
           INITIALIZE                  FLG-AREA.
           INITIALIZE                  WRK-AREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-I4.
           MOVE    SPA-WORK        TO  SPA-I4KYOUTU.
      *
           IF      SPA-MOTOPG          NOT =   "I4ERR"
               MOVE    SPACE           TO  SPA-ERRCD
           END-IF.
           MOVE    ZERO            TO  FLG-END.
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO    "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF.
      *
           MOVE    SPA-I4KYOUTU    TO  SPA-WORK.
           MOVE    SPA-AREA        TO  SPA-COMMON.
           MOVE    SPA-I4          TO  SPA-FREE.
      *
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    診療種別テーブルセット処理
      *****************************************************************
       1001-SRYKBN-SET-SEC             SECTION.
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC.
      *
       1001-SRYKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "I4ERR"
               IF      SPA-ERRCD           =   "0020"
                   INITIALIZE                 SPA-SCR-REC-41
                   INITIALIZE                 SPA-5000-GENSAN-DSP-TBL-41
                   INITIALIZE                 SPA-5000-GENSAN-CHK-TBL-41
                   INITIALIZE                 SPA-GMN-INPUT-41
                   INITIALIZE                 SPA-NAIGMN-AREA-41
      *    画面編集
                   PERFORM 500-GMNHEN-SEC
               END-IF
               IF      SPA-ERRCD           =   "0027"  OR  "0028"
                   MOVE    SPA-SAG-SRYYM-41   TO  SPA-NAI-SRYYM-41
                   MOVE    SPA-NAI-SRYYM-41   TO  WRK-SYMD(1:6)
                   MOVE    "01"               TO  WRK-SYMD(7:2)
                   PERFORM 41012-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG(1:6)   TO  SPA-GMN-SRYYM-41
      *    画面編集
                   PERFORM 500-GMNHEN-SEC
               END-IF
               MOVE    SPACE               TO  SPA-MOTOPG
               MOVE    SPACE               TO  SPA-ERRCD
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *    初期画面編集
               MOVE    ZERO                TO  IDX4
               MOVE    ZERO                TO  IDXS1
               PERFORM 300-SCREEN-SEC
               IF      FLG-END         =   1
                   GO      TO      100-INIT-EXT
               END-IF
      *        診療種別テーブルセット
               PERFORM 1001-SRYKBN-SET-SEC
      *        入院科テーブルセット
               PERFORM 3005-NYUINKA-HEN-SEC
      *        室料差額テーブルセット
               PERFORM 3113-SAGAKU-HEN-SEC
      *        入院基本テーブルセット
               PERFORM 3500-KIHON-HEN-SEC
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF.
           MOVE   SPACE                TO  LNK-KYOUTU.
      *
           MOVE   SPACE                TO  MCP-PUTTYPE.
           MOVE   "I41"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他画面より
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *
               INITIALIZE                  SPA-I4
               MOVE    ZERO                TO  FLG-SPASCR
               MOVE    ZERO                TO  IDX
               MOVE    ZERO                TO  IDG
      *
      *        元画面表示
               IF      WRK-MOTOPG(1:3)     =   "Y01"  OR  "U01"  OR
                                               "P97"  OR  "I42"  OR
                                               "I43"  OR  "I44"
                   MOVE    LNK-COMMON           TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA           TO  SPA-I4
                   MOVE    SPA-WORK             TO  SPA-I4KYOUTU
                   MOVE    SPA-GMN-PTNUM        TO  I41-PTNUM
                   MOVE    SPA-GMN-PTID         TO  SPA-GMN-PTID
                   MOVE    1                    TO  SPA-NYUGAIKBN
                   MOVE    SPA-I40-SRYYMDW(1:6) TO SPA-GMN-SRYYM-41
                   MOVE    SPA-I40-SRYYMD (1:6) TO SPA-NAI-SRYYM-41
                   MOVE    SPA-I40-SRYYMD (1:6) TO SPA-SAG-SRYYM-41
                   MOVE    SPACE                TO SPA-GMN-SRYKA-IG-41
                   MOVE    SPA-I40-SRYKA        TO SPA-GMN-SRYKA-I-41
                   MOVE    SPA-I40-SRYKAMEI     TO SPA-GMN-SRYKAMEI-I-41
               END-IF
      *
      *        予約の時、チェックの時、フリーコメント、そのまま画面表示へ
               IF      WRK-MOTOPG(1:3)     =   "U01"  OR  "Y01"  OR
                                               "I42"  OR  "I43"  OR
                                               "I44"
                   PERFORM 41010-PTNUM-SYORI-SEC
                   GO      TO      300-SCREEN-EXT
               END-IF
      *        外部より選択がある時、そのまま画面表示へ
               IF      WRK-MOTOPG(1:3)     =   "P97"
                   PERFORM 3101-P97SPASET-SEC
                   PERFORM 41010-PTNUM-SYORI-SEC
                   GO      TO      300-SCREEN-EXT
               END-IF
      *
               MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
           END-IF.
      *
      *            確認画面より
           IF      SPA-MOTOPG(1:5)     =   "I4ID1"
                   PERFORM 3003-JID1-SET-SEC
                   MOVE    SPACE           TO  SPA-IIDCD
                   MOVE    ZERO            TO  SPA-GENSAN-RE1-41
                   MOVE    ZERO            TO  SPA-GENSAN-RE2-41
                   GO      TO      300-SCREEN-EXT
           END-IF.
      *
           MOVE    SPACE               TO  SPA-IIDCD.
           INITIALIZE                  SPA-I4-AREA.
           INITIALIZE                  SPA-SCR-REC-41.
           INITIALIZE                  SPA-5000-GENSAN-DSP-TBL-41.
           INITIALIZE                  SPA-5000-GENSAN-CHK-TBL-41.
           EVALUATE    SPA-MOTOPG(1:3)
      *         業務選択より
               WHEN    "M01"
                   INITIALIZE                 SPA-I4KYOUTU
                   MOVE    SPA-MOTOPG         TO  SPA-MOTOPG2
                   MOVE    SPA-MOTOPG         TO  SPA-I40-MOTOPG
                   MOVE    "0"                TO  SPA-I40-MOTOFLG
                   MOVE    SPACE              TO  I41-PTNUM
                   MOVE    SPA-SYSYMDWH(1:6)  TO  SPA-GMN-SRYYM-41
                   MOVE    SPA-SYSYMD  (1:6)  TO  SPA-NAI-SRYYM-41
                   MOVE    SPA-SYSYMD  (1:6)  TO  SPA-SAG-SRYYM-41
                   MOVE    SPA-SYSYMDWH       TO  SPA-I40-SRYYMDW
                   MOVE    SPA-SYSYMD         TO  SPA-I40-SRYYMD
                   MOVE    1                  TO  SPA-NYUGAIKBN
                   MOVE    SPACE              TO  SPA-GMN-SRYKA-IG-41
                   MOVE    "00"               TO  SPA-GMN-SRYKA-I-41
                   MOVE    "全科指定"         TO  SPA-GMN-SRYKAMEI-I-41
                   MOVE    ZERO               TO  SPA-GMN-CUR-41
      *            入院登録より
               WHEN    "I01"
                   INITIALIZE                 SPA-I4KYOUTU
                   MOVE    SPA-MOTOPG         TO  SPA-MOTOPG2
                   MOVE    SPA-MOTOPG         TO  SPA-I40-MOTOPG
                   MOVE    SPA-PTNUM          TO  SPA-GMN-PTNUM
                   MOVE    SPA-PTNUM          TO  I41-PTNUM
                   MOVE    SPA-PTID           TO  SPA-GMN-PTID
                   MOVE    SPA-SYSYMDWH(1:6)  TO  SPA-GMN-SRYYM-41
                   MOVE    SPA-SYSYMD  (1:6)  TO  SPA-NAI-SRYYM-41
                   MOVE    SPA-SYSYMD  (1:6)  TO  SPA-SAG-SRYYM-41
                   MOVE    SPA-SYSYMDWH       TO  SPA-I40-SRYYMDW
                   MOVE    SPA-SYSYMD         TO  SPA-I40-SRYYMD
                   MOVE    1                  TO  SPA-NYUGAIKBN
                   MOVE    SPACE              TO  SPA-GMN-SRYKA-IG-41
                   MOVE    "00"               TO  SPA-GMN-SRYKA-I-41
                   MOVE    "全科指定"         TO  SPA-GMN-SRYKAMEI-I-41
                   MOVE    ZERO               TO  SPA-GMN-CUR-41
                   IF      SPA-PTID            =   ZERO
                       MOVE    ZERO                TO  SPA-GMN-CUR-41
                   ELSE
      *                排他制御処理
                       PERFORM 990-LOCK-IN-SEC
                       PERFORM 41010-PTNUM-SYORI-SEC
                       GO      TO      300-SCREEN-EXT
                   END-IF
      *
               WHEN     OTHER
                   MOVE    SPA-I40-SRYYMDW(1:6) TO  SPA-GMN-SRYYM-41
                   MOVE    SPA-I40-SRYYMD (1:6) TO  SPA-NAI-SRYYM-41
                   MOVE    SPA-I40-SRYYMD (1:6) TO  SPA-SAG-SRYYM-41
                   MOVE    1                    TO  SPA-NYUGAIKBN
      *
                   MOVE    1                    TO  FLG-HENSYU
                   PERFORM 310-SPA-SET-SEC
                   MOVE    1                    TO  SPA-GMN-CUR-41
           END-EVALUATE.
      *
           MOVE    1                   TO  SPA-GMN-PAGE-41.
           MOVE    1                   TO  SPA-IDXZ-41.
      *
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     確認画面よりの処理
      *****************************************************************
       3003-JID1-SET-SEC                  SECTION.
      *
           EVALUATE    SPA-IIDCD
               WHEN    "0102"
      *            戻るの確認
                   IF      SPA-I4ID-FLG        =   "OK"
                       MOVE    1                  TO  SPA-GMN-CUR-41
                       MOVE    SPACE              TO  SPA-UPDFLG-41
                       PERFORM 210-BACK-SYORI-SEC
                   ELSE
                       MOVE    1                  TO  SPA-GMN-CUR-41
                   END-IF
      *
               WHEN    "0103"
      *            明細選択の確認
                   IF      SPA-I4ID-FLG        =   "OK"
                       MOVE    1                  TO  SPA-GMN-CUR-41
                       MOVE    SPA-GMN-NUM-41     TO  I41-NUM
                       PERFORM 41011-BANGO-CHK-SEC
                       MOVE    SPACE              TO  SPA-GMN-KOMENTO-41
                   ELSE
                       MOVE    2                  TO  SPA-GMN-CUR-41
                       MOVE    SPA-MAE-BANGO-41   TO  SPA-GMN-NUM-41
                   END-IF
      *
               WHEN    "0104"
      *            剤変更の確認
                   IF      SPA-I4ID-FLG        =   "OK"
                       MOVE     1             TO  SPA-GMN-CUR-41
                       MOVE     1             TO  SPA-I4-SYORI
                       PERFORM 420-ZAIHENKOU-SYORI-SEC
                   ELSE
                       MOVE     1             TO  SPA-GMN-CUR-41
                   END-IF
      *
               WHEN    "0106"
      *            診療年月変更の確認
                   IF      SPA-I4ID-FLG        =   "OK"
                       IF      SPA-I4-SEL         =   1
      *                    前月処理へ
                           PERFORM 41018-ZENGETU-SYORI-SEC
                       ELSE
      *                    次月処理へ
                           PERFORM 41019-JIGETU-SYORI-SEC
                       END-IF
                   ELSE
                       MOVE    1                  TO  SPA-GMN-CUR-41
                   END-IF
      *
               WHEN    "0107"
      *            入院科変更の確認
                   IF      SPA-I4ID-FLG        =   "OK"
                       PERFORM 41013-NYUINKA-CHANGE-SEC
                   END-IF
                   MOVE    1                  TO  SPA-GMN-CUR-41
      *
               WHEN    "0108"
      *            診療年月変更の確認
                   IF      SPA-I4ID-FLG        =   "OK"
                       PERFORM 410181-SRYYM-CHANGE-SEC
                       MOVE    1                    TO  SPA-GMN-CUR-41
                   ELSE
                       MOVE    SPA-I40-SRYYMDW(1:6) TO  SPA-GMN-SRYYM-41
                       MOVE    SPA-I40-SRYYMD(1:6)  TO  SPA-NAI-SRYYM-41
                       MOVE    1                    TO  SPA-GMN-CUR-41
                   END-IF
      *
               WHEN    "0109"
      *            一括変更の未設定項目発見の確認
                   PERFORM 4901-NYUINACCT-UPD3-SEC
                   MOVE    ZERO           TO  FLG-NYUINACT
                   PERFORM 4901-NYUINACCT-UPD2-SEC
                           UNTIL      FLG-NYUINACT   =   1
                   MOVE    ZERO           TO  SPA-GMN-CUR-41
      *
                   PERFORM 490-TOROKU-CRE-SEC
      *
               WHEN    "0110"
      *            最新入院履歴の変更の確認
                   IF      SPA-I4ID-FLG        =   "OK"
                       PERFORM 470-KAKUTEI-2-SEC
                   END-IF
      *
           END-EVALUATE.
      *
       3003-JID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＬＤ外の項目入力編集処理
      *****************************************************************
       3101-P97SPASET-SEC              SECTION.
      *
           MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU.
      *    受付、患者検索より
           IF      WRK-SPA-PTNUM       =   SPA-GMN-PTNUM
               CONTINUE
           ELSE
               IF      WRK-SPA-PTNUM       NOT =   SPACE
      *            初期表示へ
                   MOVE    WRK-SPA-PTNUM   TO  I41-PTNUM
                   PERFORM 41010-PTNUM-SYORI-SEC
               END-IF
           END-IF.
      *
           IF      SPA-GMN-PTID        =   ZERO
               MOVE    ZERO                TO  SPA-GMN-CUR-41
           ELSE
               MOVE    01                  TO  SPA-GMN-CUR-41
           END-IF.
      *
       3101-P97SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院科初期編集
      *****************************************************************
       3005-NYUINKA-HEN-SEC                SECTION.
      *
           INITIALIZE                  SPA-GMN-SRYKA-AREA-L-41.
           MOVE    "00"                TO  SPA-GMN-SRYKA-L-41 (1).
           MOVE    "全科指定"          TO  SPA-GMN-SRYKAMEI-L-41 (1).
           MOVE     1                  TO  SPA-GMN-SRYKA-MAX-41.
      *    入院科
           MOVE    SPACE               TO  SYS-1005-REC.
           MOVE    "1005"              TO  SYS-1005-KANRICD.
           MOVE    ZERO                TO  SYS-1005-STYUKYMD.
           MOVE    ALL "9"             TO  SYS-1005-EDYUKYMD.
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  MCPDATA-REC.
           MOVE    SPA-NAI-SRYYM-41    TO  ORC-DBYMD(1:6).
           MOVE    "01"                TO  ORC-DBYMD(7:2).
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF.
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       (IDX            >   100)  OR
                               (FLG-SYSKANRI   =   1)
               MOVE    MCPDATA-REC           TO  SYS-1005-REC
               COMPUTE IDX2     =     IDX     +     1
               MOVE    SYS-1005-KBNCD (1:2)  TO
                                         SPA-GMN-SRYKA-L-41 (IDX2)
               MOVE    SYS-1005-SRYKANAME1   TO
                                         SPA-GMN-SRYKAMEI-L-41 (IDX2)
               MOVE    IDX2                  TO  SPA-GMN-SRYKA-MAX-41
               MOVE    "SYSKANRI-KEY2"       TO  ORC-DBPATH
               PERFORM 900-KANRIMST-READ-SEC
           END-PERFORM.
      *
       3005-NYUINKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    室料差額初期編集
      *****************************************************************
       3113-SAGAKU-HEN-SEC                 SECTION.
      *
           INITIALIZE                  SPA-GMN-SAGAKUTBL-41.
      *    室料差額
           MOVE    SPACE               TO  SYS-5113-REC.
           MOVE    "5113"              TO  SYS-5113-KANRICD.
           MOVE    ZERO                TO  SYS-5113-STYUKYMD.
           MOVE    ALL "9"             TO  SYS-5113-EDYUKYMD.
      *    システム管理検索
           MOVE    SYS-5113-REC        TO  MCPDATA-REC.
           MOVE    SPA-NAI-SRYYM-41    TO  ORC-DBYMD(1:6).
           MOVE    "01"                TO  ORC-DBYMD(7:2).
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               INITIALIZE                  SYS-5113-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF.
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       (IDX            >   30)  OR
                               (FLG-SYSKANRI   =   1)
               MOVE    MCPDATA-REC             TO  SYS-5113-REC
               MOVE    SYS-5113-KBNCD(1:4)     TO
                                               SPA-GMN-SAGAKUNO-41 (IDX)
               MOVE    SYS-5113-BRM-SAGAKU-NM  TO  WRK-SAGAKU-Z1
               MOVE    "円"                    TO  WRK-SAGAKU-X1
               MOVE    WRK-SAGAKU              TO
                                               SPA-GMN-SAGAKU-41 (IDX)
               MOVE    IDX                     TO  SPA-SAGAKU-MAX-41
               MOVE    "SYSKANRI-KEY2"         TO  ORC-DBPATH
               PERFORM 900-KANRIMST-READ-SEC
           END-PERFORM.
      *
       3113-SAGAKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院基本情報初期編集
      *****************************************************************
       3500-KIHON-HEN-SEC                  SECTION.
      *
           INITIALIZE                  SPA-5000-GENSAN-INF-TBL-41.
      *    入院基本情報
           MOVE    SPACE               TO  SYS-5000-REC.
           MOVE    "5000"              TO  SYS-5000-KANRICD.
           MOVE    "*"                 TO  SYS-5000-KBNCD.
           MOVE    ZERO                TO  SYS-5000-STYUKYMD.
           MOVE    ALL "9"             TO  SYS-5000-EDYUKYMD.
      *    システム管理検索
           MOVE    SYS-5000-REC        TO  MCPDATA-REC.
           MOVE    SPA-NAI-SRYYM-41    TO  ORC-DBYMD(1:6).
           MOVE    "01"                TO  ORC-DBYMD(7:2).
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               INITIALIZE                  SYS-5000-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF.
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC             TO  SYS-5000-REC
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX             >   10
               MOVE    SYS-5000-DSPCD (IDX)    TO
                                               SPA-5000-DSPCD-41 (IDX)
               END-PERFORM
           END-IF.
      *
       3500-KIHON-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
      *    診療会計マスターより編集をする
      *    診療年月の末日算定
           INITIALIZE                      STS-AREA-DAY.
           INITIALIZE                      LNK-DAY7-AREA.
           MOVE    "71"                TO  LNK-DAY7-IRAI.
           MOVE    SPA-NAI-SRYYM-41    TO  LNK-DAY7-YM.
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA.
           MOVE    LNK-DAY7-KOYOMI(7:2)    TO  SPA-SRYMATU-41.
      *
      *    年齢計算（算定月の１日の年齢）
           MOVE    SPA-GMN-BIRTHDAY    TO  WRK-YMDS1.
           MOVE    SPA-NAI-SRYYM-41    TO  WRK-YMDS2(1:6).
           MOVE    01                  TO  WRK-DDS2.
           COMPUTE SPA-NAI-NENREI-YY   =   WRK-YYS2
                                       -   WRK-YYS1.
           IF      WRK-YMDS1(5:4)      >   WRK-YMDS2(5:4)
               COMPUTE SPA-NAI-NENREI-YY   =   SPA-NAI-NENREI-YY
                                           -   1
               COMPUTE SPA-NAI-NENREI-MM   =  (WRK-MMS2  +  12)
                                           -   WRK-MMS1
           ELSE
               COMPUTE SPA-NAI-NENREI-MM   =   WRK-MMS2
                                           -   WRK-MMS1
           END-IF.
      *    該当月で６歳になる時、年齢フラグを立てる
           IF     (SPA-NAI-NENREI-YY   =   05)  AND
                  (SPA-NAI-NENREI-MM   =   11)  AND
                  (WRK-YMDS1(5:2)      =   WRK-YMDS2(5:2))
               MOVE    1                   TO  SPA-FLG-NENREI
           END-IF.
      *
      *    乳幼児チェック
           IF     (SPA-NAI-NENREI-YY       =   ZERO)  AND
                  (SPA-NAI-NENREI-MM       =   ZERO  OR  1)
      *        年齢期間算定
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY5-AREA
               MOVE    "51"                TO  LNK-DAY5-IRAI
               MOVE    SPA-GMN-BIRTHDAY    TO  LNK-DAY5-START
               MOVE    SPA-NAI-SRYYM-41    TO  LNK-DAY5-END(1:6)
               MOVE    SPA-SRYMATU-41      TO  LNK-DAY5-END(7:2)
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY5-AREA
               IF      LNK-DAY5-NISSU2     >   28
                   MOVE    1                   TO  SPA-FLG-NENREI
                   IF      LNK-DAY5-NISSU2     <=  31
                       MOVE    LNK-DAY5-NISSU2 TO  SPA-NAI-NENREI-DD
                       MOVE    ZERO            TO  SPA-NAI-NENREI-MM
                   END-IF
               END-IF
           END-IF.
      *
      *    該当月１日が７０歳以上の時、老人とする
           IF      SPA-NAI-NENREI-YY   >=  70
               MOVE    1                   TO  SPA-I40-ROUJIN
           ELSE
               MOVE    ZERO                TO  SPA-I40-ROUJIN
           END-IF.
      *
           MOVE    ZERO                TO  SPA-GMN-MAX-41.
           INITIALIZE                  SPA-5000-GENSAN-DSP-TBL-41.
           INITIALIZE                  SPA-5000-GENSAN-CHK-TBL-41.
      *
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   患者入院履歴編集処理
      *****************************************************************
       3103-PTNYUINRRK-HEN-SEC          SECTION.
      *
      *    患者入院履歴を検索する  医療機関、患者ＩＤ
           INITIALIZE                  PTNYUINRRK-REC.
           MOVE    SPA-HOSPID          TO  PTNYUINRRK-HOSPID.
           MOVE    SPA-GMN-PTID        TO  PTNYUINRRK-PTID.
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "PTNYUINRRK-KEY3"   TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "PTNYUINRRK-KEY3"   TO  ORC-DBPATH
               PERFORM 970-PTNYUINRRK-READ-SEC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF.
      *
           IF      FLG-PTNYUINRRK      =   ZERO
               IF      PTNYUINRRK-NYUINYMD NOT =   SPACE
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    PTNYUINRRK-NYUINYMD TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE    91                  TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KOYOMI     TO  SPA-GMN-IJYOU91
                   MOVE    LNK-DAY6-KOYOMI     TO  WRK-SYMD
                   PERFORM 41012-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG         TO  SPA-GMN-IJYOU91
      *
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    PTNYUINRRK-NYUINYMD TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE    180                 TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KOYOMI     TO  SPA-GMN-IJYOU180
                   MOVE    LNK-DAY6-KOYOMI     TO  WRK-SYMD
                   PERFORM 41012-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG         TO  SPA-GMN-IJYOU180
      *
                   MOVE    PTNYUINRRK-NYUINYMD TO  SPA-GMN-NYUINBI-41
                   MOVE    PTNYUINRRK-NYUINYMD TO  WRK-SYMD
                   PERFORM 41012-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG         TO  SPA-GMN-NYUINBI-41
               END-IF
               IF      PTNYUINRRK-TAIINYMD NOT =   SPACE
                   IF   PTNYUINRRK-TAIINYMD    =   "99999999"
                       MOVE    SPACE               TO  SPA-GMN-TAINBI-41
                   ELSE
                       MOVE    PTNYUINRRK-TAIINYMD TO SPA-GMN-TAINBI-41
                       MOVE    PTNYUINRRK-TAIINYMD TO WRK-SYMD
                       PERFORM 41012-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG         TO SPA-GMN-TAINBI-41
                   END-IF
               END-IF
           END-IF.
      *
       3103-PTNYUINRRK-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   入院会計マスターより画面内容編集処理
      *****************************************************************
       3101-NYUINACCT-SPA-SEC           SECTION.
      *
      *    入院会計マスタ内容編集
           MOVE    IDX                  TO  SPA-NAI-RENNUM-41  (IDX).
           MOVE    NACCT-SRYKBN         TO  SPA-NAI-SRYKBN-41  (IDX).
           MOVE    NACCT-ZAINUM         TO  SPA-NAI-ZAINUM-41  (IDX).
           MOVE    NACCT-HKNCOMBI       TO  SPA-NAI-HKNCOMBI-41 (IDX).
           MOVE    NACCT-ZAITEN         TO  SPA-NAI-ZAITEN-41  (IDX).
           MOVE    NACCT-ZAIKAISU       TO  SPA-NAI-ZAIKAISU-41 (IDX).
           MOVE    NACCT-DAY-AREA       TO  SPA-NAI-DAY-AREA-41 (IDX).
      *    労災で外来管理加算の振替えありの時、手技料２がある
           MOVE    NACCT-SYUTEN2        TO  SPA-NAI-SYUTEN2-41 (IDX).
           MOVE    NACCT-NYUGAIKBN      TO  SPA-NAI-TNYUGAIKBN-41 (IDX).
           MOVE    NACCT-SRYKA          TO  SPA-NAI-TSRYKA-41 (IDX).
           MOVE    NACCT-ZAISKBKBN      TO  SPA-NAI-TZAISKBKBN-41 (IDX).
      *
           ADD     1                   TO  IDG.
           IF      IDG                 >   400
               GO      TO      3101-NYUINACCT-SPA-EXT
           END-IF.
      *
           MOVE    IDX                 TO  SPA-GMN-TBANGO-41  (IDG).
           MOVE    IDG                 TO  SPA-NAI-TBANGO-41  (IDX).
      *
           MOVE    ZERO                TO  WRK-IDG.
      *    入院行為マスター読み込み
           INITIALIZE                      NYUINACT-REC.
           MOVE    NACCT-HOSPID        TO  NSRY-HOSPID.
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN.
           MOVE    NACCT-PTID          TO  NSRY-PTID.
           MOVE    NACCT-SRYKA         TO  NSRY-SRYKA.
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM.
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM.
      *    点数検索用日付編集
           MOVE    NACCT-SRYYM         TO  WRK-SRYYMD(1:6).
           MOVE    01                  TO  WRK-SRYDD.
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      NACCT-DAY (IDX-D)  NOT =   ZERO
                   MOVE    IDX-D           TO  WRK-SRYDD
                   MOVE    31              TO  IDX-D
               END-IF
           END-PERFORM.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "NYUINACT-KEY2"     TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "NYUINACT-KEY2"     TO  ORC-DBPATH
               PERFORM 930-NYUINACT-RAED-SEC
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1               TO  FLG-NYUINACT
           END-IF.
           MOVE    ZERO            TO  FLG-YAKUSOKU.
           PERFORM UNTIL       FLG-NYUINACT    =   1
      *        剤内１行目
               IF      NSRY-RENNUM      =   1
                   MOVE    ZERO            TO  FLG-YAKUSOKU
                   PERFORM 31011-GYO1-HEN-SEC
               END-IF
      *        剤明細編集
               PERFORM VARYING     IDZ     FROM    1  BY  1
                       UNTIL      (IDZ         >   5  )  OR
                                  (IDG         >   400)  OR
                                  (NSRY-SRYCD (IDZ) =   SPACE)
      *            自費の時、金額を表示
                   IF      NSRY-SRYKBN          =   "95"  OR  "96"
                       PERFORM 31013-JIHIMEI-HEN-SEC
                   ELSE
                       PERFORM 31012-ZAIMEI-HEN-SEC
                   END-IF
      *            約束セットの時
                   IF      NSRY-SRYCD (IDZ)(1:1)   =   "S"
                       MOVE    1                  TO  FLG-YAKUSOKU
                   END-IF
                   MOVE    NSRY-SRYCD (IDZ)    TO
                                            SPA-GMN-TSRYCD-41 (IDG)
                   MOVE    NACCT-NYUGAIKBN     TO
                                            SPA-GMN-TNYUGAIKBN-41 (IDG)
                   MOVE    NACCT-SRYKA         TO
                                            SPA-GMN-TSRYKA-41 (IDG)
                   MOVE    NACCT-ZAISKBKBN     TO
                                            SPA-GMN-TZAISKBKBN-41 (IDG)
               END-PERFORM
      *
               MOVE    "NYUINACT-KEY2"     TO  ORC-DBPATH
               PERFORM 930-NYUINACT-RAED-SEC
           END-PERFORM.
      *
      *    自費の時、計を表示しない
           IF      ACCT-SRYKBN         =   "95"  OR  "96"
               CONTINUE
           ELSE
               PERFORM 31013-INPUTCD-HEN-SEC
               MOVE    WRK-TENSU-X         TO  SPA-GMN-TTENSU-41   (IDG)
               MOVE    IDG                 TO  SPA-NAI-ENDBANGO-41 (IDX)
           END-IF.
      *
      *    剤終了編集
           ADD     1                   TO  IDG.
           IF      IDG                 >   400
               GO      TO      3101-NYUINACCT-SPA-EXT
           END-IF.
           MOVE    ALL "-"             TO  SPA-GMN-TBLREC-41 (IDG).
      *
       3101-NYUINACCT-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療会計マスターより画面内容編集処理
      *****************************************************************
       3101-SRYACCT-SPA-SEC             SECTION.
      *
      *    診療会計マスタ内容編集
           ADD     1                   TO  IDG.
           IF      IDG                 >   400
               GO      TO      3101-SRYACCT-SPA-EXT
           END-IF.
           INITIALIZE                  SPA-GMN-TBLREC-41 (IDG).
           COMPUTE  SPA-GMN-TBANGO-41  (IDG)   =   SPA-GENSAN-ST3-41
                                                   +   1.
      *
           MOVE    ZERO                TO  WRK-IDG.
      *    診療行為マスター読み込み
           INITIALIZE                      SRYACT-REC.
           MOVE    ACCT-HOSPID         TO  SRY-HOSPID.
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN.
           MOVE    ACCT-PTID           TO  SRY-PTID.
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA.
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM.
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM.
      *    点数検索用日付編集
           MOVE    ACCT-SRYYM          TO  WRK-SRYYMD(1:6).
           MOVE    01                  TO  WRK-SRYDD.
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      ACCT-DAY (1 IDX-D)   NOT =   ZERO
                   MOVE    IDX-D           TO  WRK-SRYDD
                   MOVE    31              TO  IDX-D
               END-IF
           END-PERFORM.
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
               PERFORM 930-SRYACT-RAED-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF.
           MOVE    ZERO            TO  FLG-YAKUSOKU.
           PERFORM UNTIL       FLG-SRYACT    =   1
      *        剤内１行目
               IF      SRY-RENNUM       =   1
                   MOVE    ZERO            TO  FLG-YAKUSOKU
                   PERFORM 31011-GYO1-HEN2-SEC
               END-IF
      *        剤明細編集
               PERFORM VARYING     IDZ     FROM    1  BY  1
                       UNTIL      (IDZ         >   5)    OR
                                  (IDG         >   400)  OR
                                  (SRY-SRYCD (IDZ)  =   SPACE)
      *            自費の時、金額を表示
                   IF      SRY-SRYKBN           =   "95"  OR  "96"
                       PERFORM 31013-JIHIMEI-HEN2-SEC
                   ELSE
                       PERFORM 31012-ZAIMEI-HEN2-SEC
                   END-IF
      *            約束セットの時
                   IF      SRY-SRYCD (IDZ)(1:1)   =   "S"
                       MOVE    1                  TO  FLG-YAKUSOKU
                   END-IF
                   MOVE    SRY-SRYCD (IDZ)     TO
                                            SPA-GMN-TSRYCD-41 (IDG)
               END-PERFORM
      *
               MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
               PERFORM 930-SRYACT-RAED-SEC
           END-PERFORM.
      *
      *    自費の時、計を表示しない
           IF      ACCT-SRYKBN         =   "95"  OR  "96"
               CONTINUE
           ELSE
               PERFORM 31013-INPUTCD-HEN-SEC
               MOVE    WRK-TENSU-X         TO  SPA-GMN-TTENSU-41   (IDG)
               MOVE    IDG                 TO  SPA-NAI-ENDBANGO-41 (IDX)
           END-IF.
      *
      *    剤終了編集
           ADD     1                   TO  IDG.
           IF      IDG                 >   400
               GO      TO      3101-SRYACCT-SPA-EXT
           END-IF.
           MOVE    ALL "-"             TO  SPA-GMN-TBLREC-41 (IDG).
      *
       3101-SRYACCT-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤内１行目  編集処理
      *****************************************************************
       31011-GYO1-HEN-SEC                  SECTION.
      *
      *    診療種別区分名称、診療行為区分セット
           SET     MSYU-IDX            TO  1.
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE       TO  WRK-SRYSYUMEI
                   WHEN    NSRY-SRYSYUKBN   =   MSYU-SRYSYUKBN
                                                      (MSYU-IDX)
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                                TO  WRK-SRYSYUMEI
           END-SEARCH.
      *
      *    診療種別がない時、診療行為区分より
           IF      NSRY-SRYSYUKBN       =   SPACE
               SET     MSYU-IDX            TO  1
               SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYSYUMEI
                   WHEN    NACCT-SRYKBN     =   MSYU-SRYKBN(MSYU-IDX)
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                               TO  WRK-SRYSYUMEI
               END-SEARCH
           END-IF.
      *
           MOVE    SPACE           TO  SPA-GMN-TMEISYO-41 (IDG).
           MOVE    WRK-SRYSYUMEI   TO  SPA-GMN-TMEISYO-41 (IDG)(7:).
      *
      *回数
           PERFORM VARYING    IDK      FROM    1   BY  1
                   UNTIL      IDK      >   31
               MOVE    NACCT-DAY  (IDK)    TO  WRK-ZZ
               MOVE    WRK-ZZ-X            TO  SPA-GMN-TDAY-41 (IDG IDK)
           END-PERFORM.
      *
           COMPUTE IDG   =   IDG   -   1.
      *
       31011-GYO1-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤内１行目  編集処理
      *****************************************************************
       31011-GYO1-HEN2-SEC                 SECTION.
      *
      *    診療種別区分名称、診療行為区分セット
           SET     MSYU-IDX            TO  1.
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE            TO  WRK-SRYSYUMEI
                   WHEN    SRY-SRYSYUKBN    =   MSYU-SRYSYUKBN
                                                      (MSYU-IDX)
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                                TO  WRK-SRYSYUMEI
           END-SEARCH.
      *
      *    診療種別がない時、診療行為区分より
           IF      SRY-SRYSYUKBN        =   SPACE
               SET     MSYU-IDX            TO  1
               SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYSYUMEI
                   WHEN    ACCT-SRYKBN      =   MSYU-SRYKBN (MSYU-IDX)
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                               TO  WRK-SRYSYUMEI
               END-SEARCH
           END-IF.
      *
           IF      SRY-SRYSYUKBN          =   SPACE
               MOVE    SPACE           TO  SPA-GMN-TMEISYO-41 (IDG)
               MOVE    WRK-SRYSYUMEI   TO  SPA-GMN-TMEISYO-41 (IDG)(7:)
           ELSE
               MOVE    "."             TO  SPA-GMN-TMEISYO-41 (IDG)(1:1)
               MOVE    SRY-SRYSYUKBN   TO  SPA-GMN-TMEISYO-41 (IDG)(2:3)
               MOVE    WRK-SRYSYUMEI   TO  SPA-GMN-TMEISYO-41 (IDG)(7:)
           END-IF.
      *
      *回数
           PERFORM VARYING    IDK      FROM    1   BY  1
                   UNTIL      IDK      >   31
               MOVE    ACCT-DAY  (1 IDK)   TO  WRK-ZZ
               MOVE    WRK-ZZ-X            TO  SPA-GMN-TDAY-41 (IDG IDK)
           END-PERFORM.
      *
       31011-GYO1-HEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤内明細  編集処理
      *****************************************************************
       31012-ZAIMEI-HEN-SEC                  SECTION.
      *
           ADD     1                   TO  IDG.
           IF      IDG                 >   400
               GO      TO      31012-ZAIMEI-HEN-EXT
           END-IF.
      *
      *    約束セット編集
           IF      NSRY-SRYCD (IDZ)(1:1)   =   "S"
               PERFORM 310120-YAKUSOKU-SET-SEC
               GO      TO      31012-ZAIMEI-HEN-EXT
           END-IF.
      *
      *    診療コードより診療行為名称を取得
           MOVE    NSRY-SRYCD (IDZ)    TO  TNS-SRYCD.
           PERFORM 910-TENSU-READ-SEC.
           IF      FLG-TENSU       NOT =   ZERO
               MOVE    NSRY-SRYCD (IDZ)    TO  TNS-NAME
           END-IF.
      *    行為名称
           MOVE    TNS-NAME            TO  SPA-GMN-TMEISYO-41 (IDG).
      *    薬剤服用コメントの時、再編集
           IF      NSRY-SRYCD (IDZ)(1:3)    =   "001"
                PERFORM 3100121-FUKIYO-MEISYO-SEC
                MOVE    WRK-MEISYO    TO  SPA-GMN-TMEISYO-41 (IDG)
           END-IF.
      *
      *    約束の内容の時、再編集
           IF      FLG-YAKUSOKU        =   1
               MOVE    SPA-GMN-TMEISYO-41 (IDG) TO  WRK-MEISYO
               MOVE    SPACE                    TO
                                                SPA-GMN-TMEISYO-41 (IDG)
               STRING     "＞"             DELIMITED   BY  SIZE
                          WRK-MEISYO       DELIMITED   BY  SIZE
                          INTO             SPA-GMN-TMEISYO-41 (IDG)
               END-STRING
           END-IF.
      *
      *    コメントの時、内容設定
           IF     (NSRY-SRYCD  (IDZ)(1:1)   =   "8")  AND
                  (NSRY-INPUTNUM (IDZ)  NOT =   ZERO)
               PERFORM 310122-KNJCOM-SET-SEC
           END-IF.
      *
      *    数量（薬剤、器材のとき）
           IF      NSRY-SRYCD  (IDZ)(1:1)   =   "6"  OR  "7"
               MOVE    NSRY-SRYSURYO (IDZ) TO  WRK-SURYO
               PERFORM 310121-SURYO-HEN-SEC
               MOVE    WRK-SURYO-X         TO  SPA-GMN-TSURYOU-41 (IDG)
           END-IF.
      *    分画数
           IF     (NSRY-SRYCD  (IDZ)(1:1)  =   "7")  AND
                  (TNS-DATAKBN             =    3)  AND
                  (NSRY-SRYKAISU (IDZ)     >   ZERO)
               MOVE    NSRY-SRYSURYO (IDZ) TO  WRK-BUNGSU-Z1
               MOVE    NSRY-SRYKAISU (IDZ) TO  WRK-BUNGSU-Z2
               MOVE    SPACE               TO  SPA-GMN-TSURYOU-41 (IDG)
               STRING  WRK-BUNGSU-Z1           DELIMITED   BY  SIZE
                       "/"                     DELIMITED   BY  SIZE
                       WRK-BUNGSU-Z2           DELIMITED   BY  SIZE
                       INTO                    SPA-GMN-TSURYOU-41 (IDG)
               END-STRING
           END-IF.
      *
      *    老人適用区分（同一剤は同じ）
           IF      TNS-ROUTEKKBN       =   ZERO
               IF      SPA-NAI-ROUTEKKBN-41 (IDX)  =   ZERO
                   MOVE    TNS-ROUTEKKBN  TO  SPA-NAI-ROUTEKKBN-41 (IDX)
               END-IF
           ELSE
               MOVE    TNS-ROUTEKKBN      TO  SPA-NAI-ROUTEKKBN-41 (IDX)
           END-IF.
      *
      *    コメント以外の最終行セット
           IF      NSRY-SRYCD  (IDZ)(1:1)   =   "0"  OR  "8"
               CONTINUE
           ELSE
               MOVE    IDG                TO  WRK-IDG
           END-IF.
      *
       31012-ZAIMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤内明細  編集処理
      *****************************************************************
       31012-ZAIMEI-HEN2-SEC                 SECTION.
      *
           ADD     1                   TO  IDG.
           IF      IDG                 >   400
               GO      TO      31012-ZAIMEI-HEN2-EXT
           END-IF.
      *
           INITIALIZE                  SPA-GMN-TBLREC-41 (IDG).
      *    約束セット編集
           IF      SRY-SRYCD (IDZ)(1:1)    =   "S"
               PERFORM 310120-YAKUSOKU-SET2-SEC
               GO      TO      31012-ZAIMEI-HEN2-EXT
           END-IF.
      *
      *    診療コードより診療行為名称を取得
           MOVE    SRY-SRYCD (IDZ)     TO  TNS-SRYCD.
           PERFORM 910-TENSU-READ-SEC.
           IF      FLG-TENSU       NOT =   ZERO
               MOVE    SRY-SRYCD (IDZ)     TO  TNS-NAME
           END-IF.
      *    行為名称
           MOVE    TNS-NAME            TO  SPA-GMN-TMEISYO-41 (IDG).
      *    薬剤服用コメントの時、再編集
           IF      SRY-SRYCD (IDZ)(1:3)     =   "001"
                PERFORM 3100121-FUKIYO-MEISYO-SEC
                MOVE    WRK-MEISYO    TO  SPA-GMN-TMEISYO-41 (IDG)
           END-IF.
      *
      *    約束の内容の時、再編集
           IF      FLG-YAKUSOKU        =   1
               MOVE    SPA-GMN-TMEISYO-41 (IDG) TO  WRK-MEISYO
               MOVE    SPACE                    TO
                                           SPA-GMN-TMEISYO-41 (IDG)
               STRING     "＞"             DELIMITED   BY  SIZE
                          WRK-MEISYO       DELIMITED   BY  SIZE
                          INTO             SPA-GMN-TMEISYO-41 (IDG)
               END-STRING
           END-IF.
      *
      *    コメントの時、内容設定
           IF     (SRY-SRYCD  (IDZ)(1:1)    =   "8")  AND
                  (SRY-INPUTNUM (IDZ)   NOT =   ZERO)
               PERFORM 310122-KNJCOM-SET2-SEC
           END-IF.
      *
      *    数量（薬剤、器材のとき）
           IF      SRY-SRYCD  (IDZ)(1:1)    =   "6"  OR  "7"
               MOVE    SRY-SRYSURYO (IDZ)  TO  WRK-SURYO
               PERFORM 310121-SURYO-HEN-SEC
               MOVE    WRK-SURYO-X         TO  SPA-GMN-TSURYOU-41 (IDG)
           END-IF.
      *    分画数
           IF     (SRY-SRYCD  (IDZ)(1:1)   =   "7")  AND
                  (TNS-DATAKBN             =    3)  AND
                  (SRY-SRYKAISU (IDZ)      >   ZERO)
               MOVE    SRY-SRYSURYO (IDZ)  TO  WRK-BUNGSU-Z1
               MOVE    SRY-SRYKAISU (IDZ)  TO  WRK-BUNGSU-Z2
               MOVE    SPACE               TO  SPA-GMN-TSURYOU-41 (IDG)
               STRING  WRK-BUNGSU-Z1           DELIMITED   BY  SIZE
                       "/"                     DELIMITED   BY  SIZE
                       WRK-BUNGSU-Z2           DELIMITED   BY  SIZE
                       INTO                    SPA-GMN-TSURYOU-41 (IDG)
               END-STRING
           END-IF.
      *
      *    コメント以外の最終行セット
           IF      SRY-SRYCD  (IDZ)(1:1)    =   "0"  OR  "8"
               CONTINUE
           ELSE
               MOVE    IDG             TO  WRK-IDG
           END-IF.
      *
       31012-ZAIMEI-HEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤内明細（自費）  編集処理
      *****************************************************************
       31013-JIHIMEI-HEN-SEC           SECTION.
      *
           ADD     1                   TO  IDG.
           IF      IDG                 >   400
               GO      TO      31013-JIHIMEI-HEN-EXT
           END-IF.
      *
      *    約束セット編集
           IF      NSRY-SRYCD (IDZ)(1:1)   =   "S"
               PERFORM 310120-YAKUSOKU-SET-SEC
               GO      TO      31013-JIHIMEI-HEN-EXT
           END-IF.
      *
      *    診療コードより診療行為名称を取得
           MOVE    NSRY-SRYCD (IDZ)     TO  TNS-SRYCD.
           PERFORM 910-TENSU-READ-SEC.
           IF      FLG-TENSU       NOT =   ZERO
               MOVE    NSRY-SRYCD (IDZ)    TO  TNS-NAME
           END-IF.
      *    行為名称
           MOVE    TNS-NAME            TO  SPA-GMN-TMEISYO-41 (IDG).
      *
      *    約束の内容の時、再編集
           IF      FLG-YAKUSOKU        =   1
               MOVE    SPA-GMN-TMEISYO-41 (IDG)   TO  WRK-MEISYO
               MOVE    SPACE               TO  SPA-GMN-TMEISYO-41 (IDG)
               STRING     "＞"             DELIMITED   BY  SIZE
                          WRK-MEISYO       DELIMITED   BY  SIZE
                          INTO             SPA-GMN-TMEISYO-41 (IDG)
               END-STRING
           END-IF.
      *
      *    コメントの時、内容設定
           IF     (NSRY-SRYCD (IDZ)(1:1)    =   "8")  AND
                  (NSRY-INPUTNUM (IDZ)  NOT =   ZERO)
               PERFORM 310122-KNJCOM-SET-SEC
           END-IF.
      *
      *    計
           MOVE    NSRY-JIHIMONEY (IDZ) TO  WRK-KEI.
           MOVE    SPACE                TO  WRK-ZAIKEI.
           MOVE    WRK-KEI-X            TO  WRK-ZAIKEI(10:).
      *
           IF      NSRY-JIHIMONEY (IDZ)     =   ZERO
               MOVE    SPACE               TO  SPA-GMN-TTENSU-41 (IDG)
           ELSE
               MOVE    WRK-KEI-X           TO  SPA-GMN-TTENSU-41 (IDG)
           END-IF.
      *
      *    コメント以外の最終行セット
           IF      NSRY-SRYCD (IDZ)(1:1)    =   "0"  OR  "8"
               CONTINUE
           ELSE
               MOVE    IDG             TO  WRK-IDG
           END-IF.
      *
       31013-JIHIMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤内明細（自費）  編集処理
      *****************************************************************
       31013-JIHIMEI-HEN2-SEC          SECTION.
      *
           ADD     1                   TO  IDG.
           IF      IDG                 >   400
               GO      TO      31013-JIHIMEI-HEN2-EXT
           END-IF.
      *
           INITIALIZE                  SPA-GMN-TBLREC-41 (IDG).
      *    約束セット編集
           IF      SRY-SRYCD (IDZ)(1:1)    =   "S"
               PERFORM 310120-YAKUSOKU-SET2-SEC
               GO      TO      31013-JIHIMEI-HEN2-EXT
           END-IF.
      *
      *    診療コードより診療行為名称を取得
           MOVE    SRY-SRYCD (IDZ)      TO  TNS-SRYCD.
           PERFORM 910-TENSU-READ-SEC.
           IF      FLG-TENSU       NOT =   ZERO
               MOVE    NSRY-SRYCD (IDZ)    TO  TNS-NAME
           END-IF.
      *    行為名称
           MOVE    TNS-NAME            TO  SPA-GMN-TMEISYO-41 (IDG).
      *
      *    約束の内容の時、再編集
           IF      FLG-YAKUSOKU        =   1
               MOVE    SPA-GMN-TMEISYO-41 (IDG)   TO  WRK-MEISYO
               MOVE    SPACE               TO  SPA-GMN-TMEISYO-41 (IDG)
               STRING     "＞"             DELIMITED   BY  SIZE
                          WRK-MEISYO       DELIMITED   BY  SIZE
                          INTO             SPA-GMN-TMEISYO-41 (IDG)
               END-STRING
           END-IF.
      *
      *    コメントの時、内容設定
           IF     (SRY-SRYCD (IDZ)(1:1)     =   "8")  AND
                  (SRY-INPUTNUM (IDZ)   NOT =   ZERO)
               PERFORM 310122-KNJCOM-SET2-SEC
           END-IF.
      *
      *    計
           MOVE    NSRY-JIHIMONEY (IDZ) TO  WRK-KEI.
           MOVE    SPACE                TO  WRK-ZAIKEI.
           MOVE    WRK-KEI-X            TO  WRK-ZAIKEI(10:).
      *
           IF      SRY-JIHIMONEY (IDZ)      =   ZERO
               MOVE    SPACE               TO  SPA-GMN-TTENSU-41 (IDG)
           ELSE
               MOVE    WRK-KEI-X           TO  SPA-GMN-TTENSU-41 (IDG)
           END-IF.
      *
      *    コメント以外の最終行セット
           IF      SRY-SRYCD (IDZ)(1:1)     =   "0"  OR  "8"
               CONTINUE
           ELSE
               MOVE    IDG             TO  WRK-IDG
           END-IF.
      *
       31013-JIHIMEI-HEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *     約束セットの時、再編集処理
      *****************************************************************
       310120-YAKUSOKU-SET-SEC        SECTION.
      *
           MOVE    SPACE                  TO  INPUTCD-REC.
           MOVE    SPA-HOSPID             TO  ICD-HOSPID.
      *    セットの桁数は６とする
           MOVE    "6"                    TO  ICD-CDSYU.
           MOVE    NSRY-SRYCD (IDZ)(1:6)  TO  ICD-INPUTCD.
      *
           PERFORM 930-INPUTCD-READ-SEC.
      *
           IF      FLG-INPUTCD         =   ZERO
               MOVE    ICD-DSPNAME         TO  SPA-GMN-TMEISYO-41 (IDG)
           ELSE
               MOVE    NSRY-SRYCD (IDZ)    TO  SPA-GMN-TMEISYO-41 (IDG)
           END-IF.
      *
      *    数量（薬剤、器材のとき）
           MOVE    NSRY-SRYSURYO (IDZ) TO  WRK-SURYO.
           PERFORM 310121-SURYO-HEN-SEC.
           MOVE    WRK-SURYO-X         TO  SPA-GMN-TSURYOU-41 (IDG).
      *
       310120-YAKUSOKU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     約束セットの時、再編集処理
      *****************************************************************
       310120-YAKUSOKU-SET2-SEC       SECTION.
      *
           MOVE    SPACE                  TO  INPUTCD-REC.
           MOVE    SPA-HOSPID             TO  ICD-HOSPID.
      *    セットの桁数は６とする
           MOVE    "6"                    TO  ICD-CDSYU.
           MOVE    SRY-SRYCD (IDZ)(1:6)   TO  ICD-INPUTCD.
      *
           PERFORM 930-INPUTCD-READ-SEC.
      *
           IF      FLG-INPUTCD         =   ZERO
               MOVE    ICD-DSPNAME         TO  SPA-GMN-TMEISYO-41 (IDG)
           ELSE
               MOVE    SRY-SRYCD (IDZ)     TO  SPA-GMN-TMEISYO-41 (IDG)
           END-IF.
      *
      *    数量（薬剤、器材のとき）
           MOVE    SRY-SRYSURYO (IDZ)  TO  WRK-SURYO.
           PERFORM 310121-SURYO-HEN-SEC.
           MOVE    WRK-SURYO-X         TO  SPA-GMN-TSURYOU-41 (IDG).
      *
       310120-YAKUSOKU-SET2-EXT.
           EXIT.
      *
      *****************************************************************
      *     薬剤服用コメントの時、再編集処理
      *****************************************************************
       3100121-FUKIYO-MEISYO-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-MEISYO.
      *
           STRING     "【"             DELIMITED   BY  SIZE
                      TNS-NAME         DELIMITED   BY  "    "
                      "】"             DELIMITED   BY  SIZE
                      INTO             WRK-MEISYO
           END-STRING.
      *
       3100121-FUKIYO-MEISYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量表示用編集処理
      *****************************************************************
       310121-SURYO-HEN-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X.
           MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1.
      *
           MOVE    ZERO                TO  FLG-SP.
           MOVE    ZERO                TO  WRK-SURYO-Z3.
           PERFORM VARYING     IDM     FROM    3  BY  -1
                   UNTIL       IDM     <   1
               IF      WRK-SURYO-S2(IDM:1) NOT =   ZERO
                   MOVE    1               TO  FLG-SP
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(IDM:1) TO  WRK-SURYO-Z3(IDM:1)
               END-IF
           END-PERFORM.
      *
           IF      WRK-SURYO-Z3-X      NOT =   SPACE
               MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z19
               MOVE    "."                 TO  WRK-SURYO-Z2
           END-IF.
      *
       310121-SURYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントから編集する 
      *****************************************************************
       310122-KNJCOM-SET-SEC              SECTION.
      *
           INITIALIZE                      PTCOM-REC.
           MOVE    NSRY-HOSPID         TO  PTCOM-HOSPID.
           MOVE    NSRY-PTID           TO  PTCOM-PTID.
           MOVE    NSRY-ZAINUM         TO  PTCOM-ZAINUM.
           MOVE    NSRY-SRYCD (IDZ)    TO  PTCOM-SRYCD.
           MOVE    NSRY-INPUTNUM (IDZ) TO  PTCOM-RENNUM.
           PERFORM 950-PTCOM-READ-SEC.
           IF      FLG-PTCOM           =   ZERO
               IF      PTCOM-INPUTCOMENT  NOT =   SPACE
                   MOVE    PTCOM-INPUTCOMENT TO SPA-GMN-TMEISYO-41 (IDG)
               END-IF
           END-IF.
      *
       310122-KNJCOM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントから編集する
      *****************************************************************
       310122-KNJCOM-SET2-SEC             SECTION.
      *
           INITIALIZE                      PTCOM-REC.
           MOVE    SRY-HOSPID          TO  PTCOM-HOSPID.
           MOVE    SRY-PTID            TO  PTCOM-PTID.
           MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM.
           MOVE    SRY-SRYCD (IDZ)     TO  PTCOM-SRYCD.
           MOVE    SRY-INPUTNUM (IDZ)  TO  PTCOM-RENNUM.
           PERFORM 950-PTCOM-READ-SEC.
           IF      FLG-PTCOM           =   ZERO
               IF      PTCOM-INPUTCOMENT  NOT =   SPACE
                   MOVE    PTCOM-INPUTCOMENT TO SPA-GMN-TMEISYO-41 (IDG)
               END-IF
           END-IF.
      *
       310122-KNJCOM-SET2-EXT.
           EXIT.
      *
      *****************************************************************
      *   入力コードの画面用再編集処理
      *****************************************************************
       31013-INPUTCD-HEN-SEC                  SECTION.
      *
      *    点数＊回数  計
           MOVE    SPA-NAI-ZAITEN-41 (IDX)    TO  WRK-TENSU.
      *
       31013-INPUTCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合編集処理
      *****************************************************************
       3105-HKNCOMBI-HEN-SEC          SECTION.
      *
           INITIALIZE                  SPA-I41-AREA-41.
      *
      *    保険組合マスター編集
           INITIALIZE                  HKNCOMBI-REC.
           MOVE    SPA-HOSPID          TO  COMB-HOSPID.
           MOVE    SPA-GMN-PTID        TO  COMB-PTID.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "HKNCOMBI-KEY2"     TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "HKNCOMBI-KEY2"      TO  ORC-DBPATH
               PERFORM 940-HKNCOMBI-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF.
           PERFORM VARYING     IDX-H   FROM    1   BY  1
                   UNTIL      (IDX-H           >   40)   OR
                              (FLG-HKNCOMBI    =   1)
             IF      COMB-DELKBN           =   "1"
               CONTINUE
             ELSE
               ADD     1                   TO  SPA-I41-MAX-41
               MOVE    COMB-HKNCOMBINUM    TO  SPA-I41-THKNCOMBI-41
                                                    (SPA-I41-MAX-41)
               IF      COMB-GAI-KFTNRATE   =   ZERO
                   MOVE     SPACE          TO  SPA-I41-TFTNWARI-41
                                                     (SPA-I41-MAX-41)
               ELSE
                   MOVE    COMB-GAI-KFTNRATE   TO  WRK-ZZZZ
                   MOVE    WRK-ZZZZ-X          TO  SPA-I41-TFTNWARI-41
                                                   (SPA-I41-MAX-41)(1:3)
                   MOVE    "%"                 TO  SPA-I41-TFTNWARI-41
                                                   (SPA-I41-MAX-41)(4:1)
               END-IF
               MOVE    COMB-TEKSTYMD       TO  SPA-I41-TTEKSTYMD-41
                                                        (SPA-I41-MAX-41)
               MOVE    COMB-TEKEDYMD       TO  SPA-I41-TTEKEDYMD-41
                                                        (SPA-I41-MAX-41)
               MOVE    COMB-HKNNUM         TO  SPA-I41-HKNNUM-41
                                                        (SPA-I41-MAX-41)
      *        保険組合せ名称
               MOVE    COMB-HKNCOMBINUM    TO  WRK-HKNCOMBI
               PERFORM 5101-HKNCOMBI-SET-SEC
               MOVE    WRK-HKNCOMBIMEI     TO  SPA-I41-TKUMI-41
                                                        (SPA-I41-MAX-41)
               MOVE    PTRSI-SISIKBN       TO  SPA-I41-SISIKBN-41
                                                        (SPA-I41-MAX-41)
               MOVE    PTRSI-HKNKBN        TO  SPA-I41-HKNKBN-41
                                                        (SPA-I41-MAX-41)
             END-IF
      *
               MOVE    "HKNCOMBI-KEY2"      TO  ORC-DBPATH
               PERFORM 940-HKNCOMBI-NEXT-SEC
           END-PERFORM.
      *
       3105-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  I41.
      *
           MOVE    SPA-GMN-PTNUM       TO  I41-PTNUM.
           MOVE    SPA-GMN-KANANAME    TO  I41-KANANAME.
           MOVE    SPA-GMN-NAME        TO  I41-NAME.
           MOVE    SPA-GMN-SEX         TO  I41-SEX.
           MOVE    SPA-GMN-SRYYM-41    TO  I41-SRYYM.
           MOVE    SPA-GMN-BIRTHDAYW   TO  I41-BIRTHDAY.
           MOVE    SPA-GMN-SRYKA-IG-41 TO  I41-NYUINKA.
           MOVE    SPA-GMN-IJYOU91     TO  I41-IJYOU91.
           MOVE    SPA-GMN-IJYOU180    TO  I41-IJYOU180.
           MOVE    SPA-GMN-NYUINBI-41  TO  I41-NYUINBI.
           MOVE    SPA-GMN-TAINBI-41   TO  I41-TAINBI.
      *    入院科
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-GMN-SRYKA-MAX-41
               MOVE    SPA-GMN-SRYKA-LIST-L-41 (IDX)
                                          TO  I41-NYUINKA-ITEM (IDX)
           END-PERFORM.
           MOVE    SPA-GMN-SRYKA-MAX-41   TO  I41-NYUINKA-COUNT.
      *    保険組合せ
           MOVE    ZERO                TO  I41-KUMILIST-COUNT.
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-I41-MAX-41
               IF     (SPA-I41-TTEKSTYMD-41 (IDX)(1:6)
                                                <= SPA-NAI-SRYYM-41) AND
                      (SPA-I41-TTEKEDYMD-41 (IDX)(1:6)
                                                >= SPA-NAI-SRYYM-41)
                   ADD     1                  TO  I41-KUMILIST-COUNT
                   MOVE    SPA-I41-THKNCOMBI-41 (IDX)
                                     TO  I41-KUMINO (I41-KUMILIST-COUNT)
                   MOVE    SPA-I41-TKUMI-41 (IDX)(1:32)
                                     TO  I41-KUMI (I41-KUMILIST-COUNT)
               END-IF
           END-PERFORM.
      *    室料差額
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-SAGAKU-MAX-41
               MOVE    SPA-GMN-SAGAKULIST-41 (IDX)
                                          TO  I41-SAGAKU-ITEM (IDX)
           END-PERFORM.
           MOVE    SPA-SAGAKU-MAX-41   TO  I41-SAGAKULIST-COUNT.
      *    明細
           INITIALIZE                  WRK-CHK-TBL.
           PERFORM   VARYING   IDX    FROM    1   BY  1
                     UNTIL     IDX        >   SPA-GMN-MAX-41
               IF      SPA-GMN-TMEISYO-41 (IDX)   =   ALL "-"
                   MOVE    ALL "-"           TO  I41-DATELIST-ITEM (IDX)
               ELSE
                   IF      SPA-GMN-TBANGO-41 (IDX)    >   ZERO
                       MOVE    SPA-GMN-TBANGO-41  (IDX)
                                                 TO  WRK-ZZZZ
                       MOVE    WRK-ZZZZ-X        TO  I41-TNO    (IDX)
                       MOVE    SPA-GMN-TBANGO-41  (IDX)
                                                 TO  SPA-GENSAN-ST3-41
                       MOVE    SPA-GMN-TBANGO-41  (IDX)
                                                 TO  SPA-GENSAN-ST4-41
                   END-IF
                   MOVE    SPA-GMN-TMEISYO-41 (IDX)
                                                 TO  I41-TMEISYO(IDX)
                   MOVE    SPA-GMN-TTENSU-41  (IDX)(3:5)
                                                 TO  I41-TTENSU (IDX)
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   31
                       IF  (SPA-GMN-SRYKA-I-41      =   "00")        OR
                           (SPA-WRK-CHK-KBN (IDY)   NOT =  ALL SPACE)
                           MOVE    SPA-GMN-TDAY-41 (IDX IDY)(1:2)
                                                 TO  I41-TDAY (IDX IDY)
                           IF (SPA-GMN-TZAISKBKBN-41 (IDX)  = "1")  AND
                              (SPA-GMN-TSRYCD-41 (IDX)
                                                   NOT = W-YOYAKU)  AND
                              (SPA-GMN-TDAY-41 (IDX IDY)
                                                   NOT = SPACE)
                               MOVE    SPA-GMN-TDAY-41 (IDX IDY)
                                                 TO  WRK-CHK-DAY (IDY)
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
      *
               IF     (SPA-GMN-TMEISYO-41 (IDX) NOT =   ALL "-")  AND
                      (SPA-GMN-TBANGO-41 (IDX)  NOT =   ZERO)     AND
                      (SPA-GMN-TBANGO-41 (IDX)      =   SPA-GMN-NUM-41)
                   MOVE    "T"             TO  I41-DATELIST-SELECT (IDX)
               ELSE
                   MOVE    "F"             TO  I41-DATELIST-SELECT (IDX)
               END-IF
           END-PERFORM.
           MOVE    SPA-GMN-MAX-41      TO  I41-DATELIST-COUNT.
      *
           IF      SPA-GENSAN-RE1-41       =   1
      *        診療行為マスターチェックする
               MOVE    ZERO                TO  IDZ3
               PERFORM VARYING     IDZ2    FROM    1   BY  1
                       UNTIL       IDZ2            >   10
                   IF      SPA-5000-DSPCD-41 (IDZ2) NOT = SPACE
                       PERFORM 410181-SRYACT-CHK-SEC
                   END-IF
               END-PERFORM
      *        診療会計マスターより編集をする
               IF      SPA-GENSAN-RE2-41       =   1
                   IF      SPA-GMN-MAX-41       =   ZERO
                       MOVE    1                   TO  IDX4
                       COMPUTE IDXS1   =   IDG   +   1
                   ELSE
                       MOVE    SPA-GMN-MAX-41      TO  IDX4
                       MOVE    SPA-GMN-MAX-41      TO  IDXS1
                   END-IF
                   PERFORM VARYING     IDZ2    FROM    1   BY  1
                           UNTIL       IDZ2            >   50
                       IF      SPA-5000-NYUGAIKBN-41 (IDZ2) NOT = SPACE
                           PERFORM 410181-SRYACCT-HEN-SEC
                       END-IF
                   END-PERFORM
               END-IF
               MOVE    ZERO                TO  SPA-GENSAN-RE1-41
           END-IF.
      *
           IF      (IDXS1            NOT =   ZERO)   AND
                   (SPA-GENSAN-ST2-41    >   IDXS1)
               MOVE    IDX4                 TO  IDX5
               PERFORM   VARYING   IDX    FROM    IDXS1  BY  1
                         UNTIL     IDX        >   SPA-GENSAN-ST2-41
                   IF      SPA-GMN-TMEISYO-41 (IDX)   =   ALL "-"
                       MOVE    ALL "-"      TO  I41-DATELIST-ITEM (IDX4)
                   ELSE
                       IF      SPA-GMN-TBANGO-41 (IDX)    >   ZERO
                           MOVE    SPA-GMN-TBANGO-41  (IDX)
                                                 TO  WRK-ZZZZ
                           MOVE    WRK-ZZZZ-X    TO  I41-TNO    (IDX4)
                       END-IF
                       MOVE    SPA-GMN-TMEISYO-41 (IDX)
                                                 TO  I41-TMEISYO(IDX4)
                       MOVE    SPA-GMN-TTENSU-41  (IDX)(3:5)
                                                 TO  I41-TTENSU (IDX4)
                       PERFORM VARYING     IDY     FROM    1   BY  1
                               UNTIL       IDY     >   31
                           MOVE  SPA-GMN-TDAY-41 (IDX IDY)(1:2)
                                                 TO  I41-TDAY (IDX4 IDY)
                       END-PERFORM
                   END-IF
      *
                   MOVE    "F"            TO  I41-DATELIST-SELECT (IDX4)
                   ADD     1              TO  IDX4
               END-PERFORM
               MOVE    IDX5                 TO  IDX4
               MOVE    SPA-GENSAN-ST2-41    TO  I41-DATELIST-COUNT
               IF      SPA-ERRCD   =   "0031"
                   MOVE    SPACE                TO  SPA-ERRCD
               END-IF
           END-IF.
      *
           IF      SPA-GMN-NUM-41   NOT =   ZERO
               MOVE    SPA-GMN-NUM-41      TO  WRK-ZZZZ
               MOVE    WRK-ZZZZ-X          TO  I41-NUM
           END-IF.
      *
           MOVE    SPA-GMN-MEISYO-41   TO  I41-MEISYO.
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   31
               IF  (SPA-GMN-SRYKA-I-41      =   "00")          OR
                   (SPA-WRK-CHK-KBN (IDX)   NOT =   ALL SPACE)
                   MOVE    SPA-GMN-DAY-41 (IDX)    TO  WRK-ZZ
                   MOVE    WRK-ZZ-X             TO  I41-DAY (IDX)
               END-IF
           END-PERFORM.
      *
           MOVE    SPA-GMN-KOMENTO-41     TO  I41-KOMENTO.
      *    カーソル
           PERFORM 5001-MAPCUR-SEC.
      *
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-GMN-CUR-41
               WHEN    00
                   MOVE    "PTNUM"         TO  MCP-WIDGET
               WHEN    01
                   MOVE    "NUM"           TO  MCP-WIDGET
               WHEN    02
                   MOVE    "DAY"           TO  MCP-WIDGET
                   IF      SPA-DAY-IDX-41      =   ZERO  OR
                                               >   SPA-SRYMATU-41
                       MOVE    1               TO  SPA-DAY-IDX-41
                   END-IF
                   IF     (SPA-BIRTHDAY(1:6)   =   SPA-NAI-SRYYM-41) AND
                          (SPA-DAY-IDX-41      <   SPA-BIRTHDAY(7:2))
                       MOVE    SPA-BIRTHDAY(7:2)   TO  SPA-DAY-IDX-41
                   END-IF
                   MOVE    SPA-DAY-IDX-41      TO  MCP-WIDGET(4:2)
               WHEN    03
                   MOVE    "KOMENTO"       TO  MCP-WIDGET
               WHEN    05
                   MOVE    "B08"           TO  MCP-WIDGET
               WHEN    06
                   MOVE    "NYUINKA"       TO  MCP-WIDGET
               WHEN    07
                   MOVE    "SRYYM"         TO  MCP-WIDGET
           END-EVALUATE.
      *
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
      *    クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 420-CLEA-SEC
      *    剤変更へ
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 420-ZAIHENKOU-SEC
      *    前回患者処理
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 430-MAEKPTNUM-SEC
      *    チェック処理
               WHEN    "CLICKED"       ALSO    "B03S"
                   PERFORM 430-CHEKU-SEC
      *    フリーコメント処理
               WHEN    "CLICKED"       ALSO    "B04S"
                   PERFORM 430-FREEMSG-SEC
      *    前月選択
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 41018-ZENGETU-HEN-SEC
      *    次月選択
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 41019-JIGETU-HEN-SEC
      *    変更確定
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 470-KAKUTEI-SEC
      *    氏名検索（Ｐ９７）
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 470-SIMEIKEN-SEC
      *    予約登録へ
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 480-YOYAKU-SEC
      *    受付一覧へ
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-UKEICHI-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOROKU-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        患者番号入力
               WHEN    "ACTIVATE"      ALSO    "PTNUM"
                   PERFORM 41010-PTNUM-SYORI-SEC
      *        入院科入力
               WHEN    "ACTIVATE"      ALSO    "NYUINKA"
                   PERFORM 41013-NYUINKA-SYORI-SEC
      *        診療年月
               WHEN    "ACTIVATE"      ALSO    "SRYYM"
                   PERFORM 41018-SRYYMD-SYORI-SEC
      *        番号選択
               WHEN    "ACTIVATE"      ALSO    "NUM"
                   PERFORM 41011-BANGO-CHK-SEC
      *        明細選択
               WHEN    ANY             ALSO    "DATELIST"
                   PERFORM 41012-DATALIST-SEC
      *        一括修正
               WHEN    "ACTIVATE"      ALSO    "KOMENTO"
                   PERFORM 41017-KOMENTO-CHK-SEC
      *
               WHEN    OTHER
      *        剤回数
                   IF    (MCP-EVENT       =   "ACTIVATE") AND
                         (MCP-WIDGET(1:3) =   "DAY")
                       MOVE    MCP-WIDGET(4:2)     TO  SPA-DAY-IDX-41
                       PERFORM 41012-DAY-CHK-SEC
                   END-IF
           END-EVALUATE.
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号入力処理
      *****************************************************************
       41010-PTNUM-SYORI-SEC         SECTION.
      *
      *    前回患者へ待避
           IF      SPA-GMN-PTNUM      NOT =   SPACE
               MOVE    SPA-GMN-PTNUM       TO  SPA-LAST-PTNUM
               MOVE    SPA-GMN-PTID        TO  SPA-LAST-PTID
           END-IF.
      *
           MOVE    I41-PTNUM           TO  SPA-GMN-PTNUM.
      *
           INITIALIZE                      ORCSPTNUMAREA.
           MOVE    SPA-GMN-PTNUM       TO  SPTNUM-PTNUM.
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA.
           EVALUATE    SPTNUM-RC
               WHEN    00
                   MOVE    SPTNUM-PTNUM        TO  SPTID-PTNUM
                   MOVE    SPTNUM-PTID         TO  SPTID-PTID
               WHEN   10
                   MOVE    ZERO                TO  SPA-GMN-CUR-41
                   INITIALIZE                SPA-I4-AREA
                   INITIALIZE                SPA-SCR-REC-41
                   INITIALIZE                SPA-5000-GENSAN-DSP-TBL-41
                   INITIALIZE                SPA-5000-GENSAN-CHK-TBL-41
                   INITIALIZE                SPA-I40-SET
                   INITIALIZE                SPA-I41-AREA-41
                   MOVE    I41-PTNUM            TO  SPA-GMN-PTNUM
                   MOVE    SPA-I40-SRYYMDW(1:6) TO  SPA-GMN-SRYYM-41
                   MOVE    SPA-I40-SRYYMD (1:6) TO  SPA-NAI-SRYYM-41
                   MOVE    SPA-I40-SRYYMD (1:6) TO  SPA-SAG-SRYYM-41
                   MOVE    "0017"              TO  SPA-ERRCD
                   GO      TO     41010-PTNUM-SYORI-EXT
               WHEN    20
      *            氏名検索へ
                   MOVE    SPACE               TO  LINKAREA
                   MOVE    SPA-KYOUTU          TO  LNK-COMMON
                   MOVE    SPA-I4              TO  LNK-SCRSPA
      *
                   MOVE    "I41"               TO  SPA-MOTOPG
                   MOVE    "P97"               TO  SPA-SAKIPG
      *            氏名セット
                   MOVE    SPA-GMN-PTNUM       TO  SPA-NAME
                   MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
                   MOVE    "CHANGE"            TO  MCP-PUTTYPE
                   MOVE    "P97"               TO  MCP-WINDOW
                   PERFORM 900-PUT-WINDOW
                   MOVE    1                   TO  FLG-END
                   GO      TO     41010-PTNUM-SYORI-EXT
               WHEN    OTHER
                   MOVE    ZERO                TO  SPA-GMN-CUR-41
                   INITIALIZE                SPA-I4-AREA
                   INITIALIZE                SPA-SCR-REC-41
                   INITIALIZE                SPA-5000-GENSAN-DSP-TBL-41
                   INITIALIZE                SPA-5000-GENSAN-CHK-TBL-41
                   INITIALIZE                SPA-I40-SET
                   INITIALIZE                SPA-I41-AREA-41
                   MOVE    I41-PTNUM            TO  SPA-GMN-PTNUM
                   MOVE    SPA-I40-SRYYMDW(1:6) TO  SPA-GMN-SRYYM-41
                   MOVE    SPA-I40-SRYYMD (1:6) TO  SPA-NAI-SRYYM-41
                   MOVE    SPA-I40-SRYYMD (1:6) TO  SPA-SAG-SRYYM-41
                   MOVE    "0017"              TO  SPA-ERRCD
                   GO      TO     41010-PTNUM-SYORI-EXT
           END-EVALUATE.
      *
           MOVE    SPTID-PTNUM         TO  SPA-GMN-PTNUM.
           MOVE    SPTID-PTID          TO  SPA-GMN-PTID.
      *    患者マスター存在チェック
           MOVE    SPA-HOSPID          TO  PTINF-HOSPID.
           MOVE    SPA-GMN-PTID        TO  PTINF-PTID.
           PERFORM 900-PTINF-READ-SEC.
           IF      FLG-PTINF      NOT =   ZERO
               MOVE    ZERO                TO  SPA-GMN-CUR-41
               INITIALIZE                SPA-I4-AREA
               INITIALIZE                SPA-SCR-REC-41
               INITIALIZE                SPA-5000-GENSAN-DSP-TBL-41
               INITIALIZE                SPA-5000-GENSAN-CHK-TBL-41
               INITIALIZE                SPA-I40-SET
               INITIALIZE                SPA-I41-AREA-41
               MOVE    I41-PTNUM            TO  SPA-GMN-PTNUM
               MOVE    SPA-I40-SRYYMDW(1:6) TO  SPA-GMN-SRYYM-41
               MOVE    SPA-I40-SRYYMD (1:6) TO  SPA-NAI-SRYYM-41
               MOVE    SPA-I40-SRYYMD (1:6) TO  SPA-SAG-SRYYM-41
               MOVE    "0017"              TO  SPA-ERRCD
               GO      TO      41010-PTNUM-SYORI-EXT
           END-IF.
      *
           MOVE    PTINF-NAME          TO  SPA-GMN-NAME.
           MOVE    PTINF-KANANAME      TO  SPA-GMN-KANANAME.
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"                TO  SPA-GMN-SEX
               WHEN    "2"
                   MOVE    "女"                TO  SPA-GMN-SEX
           END-EVALUATE.
      *
           MOVE    PTINF-BIRTHDAY      TO  SPA-GMN-BIRTHDAY.
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD.
           PERFORM 41012-SEIWA-HEN-SEC.
           MOVE    WRK-HENYMDG         TO  SPA-GMN-BIRTHDAYW.
      *
           INITIALIZE                  SPA-NAIGMN-AREA-41.
           INITIALIZE                  SPA-GMN-IJYOU91.
           INITIALIZE                  SPA-GMN-IJYOU180.
           INITIALIZE                  SPA-GMN-NYUINBI-41.
           INITIALIZE                  SPA-GMN-TAINBI-41.
      *
           PERFORM 310-SPA-SET-SEC.
           IF     SPA-ERRCD       NOT =   SPACE
               GO      TO      41010-PTNUM-SYORI-EXT
           END-IF.
      *
      *    排他制御処理
           PERFORM 990-LOCK-IN-SEC.
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41010-PTNUM-SYORI-EXT
           END-IF.
      *
      *    患者入院履歴編集
           PERFORM 3103-PTNYUINRRK-HEN-SEC.
      *
      *    入院会計マスターより編集をする
           PERFORM 410181-NYUINACCT-HEN-SEC.
           PERFORM 410181-NYUINACCT-SEL-SEC.
      *
      *    保険組合一覧編集
           PERFORM 3105-HKNCOMBI-HEN-SEC.
      *
           IF      SPA-GMN-MAX-41      =   ZERO
               MOVE    07                  TO  SPA-GMN-CUR-41
               MOVE    SPACE               TO  SPA-ERRCD
           ELSE
               MOVE    01                  TO  SPA-GMN-CUR-41
           END-IF.
      *
       41010-PTNUM-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院科入力処理
      *****************************************************************
       41013-NYUINKA-SYORI-SEC        SECTION.
      *
           IF      SPA-GMN-NUM-41   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      41013-NYUINKA-SYORI-EXT
           END-IF.
      *
           MOVE    01                  TO  SPA-GMN-CUR-41.
      *
           MOVE    SPA-GMN-SRYKA-IG-41 TO  SPA-MAE-SRYKA-G-41.
           MOVE    I41-NYUINKA         TO  SPA-GMN-SRYKA-IG-41.
      *    入院科選択なし
           IF     (SPA-GMN-SRYKA-I-41  =   SPACE)   OR
                  (SPA-GMN-PTID        =   ZERO)
               GO      TO      41013-NYUINKA-SYORI-EXT
           END-IF.
      *    入院科選択
           IF     (SPA-MAE-SRYKA-41    =   SPACE)  OR
                  (SPA-UPDFLG-41       =   SPACE)
               PERFORM 41013-NYUINKA-CHANGE-SEC
               GO      TO      41013-NYUINKA-SYORI-EXT
           END-IF.
      *
      *    確認表示
           MOVE    "0107"              TO  SPA-IIDCD.
      *
       41013-NYUINKA-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院科変更処理
      *****************************************************************
       41013-NYUINKA-CHANGE-SEC            SECTION.
      *
      *    現在表示されている内容を更新する
      *    診療会計マスター更新
           IF      SPA-GMN-MAX-41   >   ZERO
               IF      SPA-UPDFLG-41          =   "1"
                   PERFORM 4901-NYUINACCT-UPD-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       GO      TO       41013-NYUINKA-CHANGE-EXT
                   END-IF
               END-IF
           END-IF.
      *
           MOVE    SPA-GMN-SRYYM-41    TO  SPA-I40-SRYYMDW(1:6).
           MOVE    SPA-NAI-SRYYM-41    TO  SPA-I40-SRYYMD(1:6).
      *
           MOVE    SPA-I40-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM-41.
           MOVE    SPA-I40-SRYYMD(1:6)     TO  SPA-NAI-SRYYM-41.
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKA-MAX-41
               IF      SPA-GMN-SRYKA-I-41 =   SPA-GMN-SRYKA-L-41 (IDX)
                   MOVE    SPA-GMN-SRYKA-LIST-L-41 (IDX)
                                           TO  SPA-GMN-SRYKA-IG-41
               END-IF
           END-PERFORM.
      *
           MOVE    SPA-GMN-SRYKAMEI-I-41 TO  SPA-I40-SRYKAMEI.
           MOVE    SPA-GMN-SRYKA-I-41    TO  SPA-I40-SRYKA.
      *
      *    入院会計マスターより編集をする
           PERFORM 410181-NYUINACCT-HEN-SEC.
           PERFORM 410181-NYUINACCT-SEL-SEC.
      *
      *    保険組合一覧編集
           PERFORM 3105-HKNCOMBI-HEN-SEC.
      *
           IF      SPA-GMN-MAX-41      =   ZERO
               MOVE    06                  TO  SPA-GMN-CUR-41
           ELSE
               MOVE    01                  TO  SPA-GMN-CUR-41
           END-IF.
      *
       41013-NYUINKA-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月入力処理
      *****************************************************************
       41018-SRYYMD-SYORI-SEC             SECTION.
      *
           IF      SPA-GMN-NUM-41   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      41018-SRYYMD-SYORI-EXT
           END-IF.
      *
      *    現在表示されている内容を更新する
      *    診療会計マスター更新
           IF      SPA-GMN-MAX-41   >   ZERO
               IF      SPA-UPDFLG-41          =   "1"
                   PERFORM 4901-NYUINACCT-UPD-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       GO      TO       41018-SRYYMD-SYORI-EXT
                   END-IF
               END-IF
           END-IF.
      *
           MOVE    01                  TO  SPA-GMN-CUR-41.
      *
           MOVE    I41-SRYYM           TO  SPA-GMN-SRYYM-41.
      *
           INITIALIZE                      ORCSGDAYAREA.
           MOVE    SPA-GMN-SRYYM-41    TO  SGDAY-INDAY.
           MOVE    "1"                 TO  SGDAY-INTYPE.
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA.
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY(1:6)    TO  SPA-GMN-SRYYM-41
      *        患者の確定してない時
               IF      SPA-GMN-PTID        =   ZERO
                    MOVE    SGDAY-SDAY(1:6)     TO  SPA-NAI-SRYYM-41
                    MOVE    ZERO                TO  SPA-GMN-CUR-41
                    GO      TO      41018-SRYYMD-SYORI-EXT
               END-IF
      *        変更のない時、以降の処理はそのまま
               IF      SGDAY-SDAY(1:6)     =   SPA-NAI-SRYYM-41
                   IF      SPA-GMN-MAX-41      >   ZERO
                        MOVE    1                   TO  SPA-GMN-CUR-41
                        GO      TO      41018-SRYYMD-SYORI-EXT
                   END-IF
               END-IF
               MOVE    SGDAY-SDAY(1:6)     TO  SPA-NAI-SRYYM-41
               MOVE    SGDAY-SDAY(1:6)     TO  SPA-SAG-SRYYM-41
               MOVE    1                   TO  SPA-GENSAN-RE1-41
               MOVE    ZERO                TO  SPA-GENSAN-RE2-41
               INITIALIZE                  SPA-5000-GENSAN-CHK-TBL-41
           ELSE
               MOVE    "0018"              TO  SPA-ERRCD
               MOVE    07                  TO  SPA-GMN-CUR-41
               GO      TO      41018-SRYYMD-SYORI-EXT
           END-IF.
      *
      *    入院会計マスターより編集をする
           PERFORM 410181-NYUINACCT-HEN-SEC.
           PERFORM 410181-NYUINACCT-SEL-SEC.
      *
      *    保険組合一覧編集
           PERFORM 3105-HKNCOMBI-HEN-SEC.
      *
           IF      SPA-GMN-MAX-41      =   ZERO
               MOVE    07                  TO  SPA-GMN-CUR-41
           ELSE
               MOVE    01                  TO  SPA-GMN-CUR-41
           END-IF.
      *
       41018-SRYYMD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月変更の確認処理
      *****************************************************************
       410181-SRYYM-CHANGE-SEC             SECTION.
      *
      *    入院会計マスター更新
           IF      SPA-UPDFLG-41          =   "1"
               MOVE    SPA-NAI-SRYYM-41        TO  WRK-NAI-SRYYM
               MOVE    SPA-GMN-SRYYM-41        TO  WRK-GMN-SRYYM
               MOVE    SPA-I40-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM-41
               MOVE    SPA-I40-SRYYMD(1:6)     TO  SPA-NAI-SRYYM-41
               PERFORM 4901-NYUINACCT-UPD-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO       410181-SRYYM-CHANGE-EXT
               END-IF
               MOVE    WRK-NAI-SRYYM       TO  SPA-NAI-SRYYM-41
               MOVE    WRK-GMN-SRYYM       TO  SPA-GMN-SRYYM-41
           END-IF.
      *
           MOVE    SPACE               TO  SPA-I40-SRYKA.
           MOVE    SPACE               TO  SPA-I40-SRYKAMEI.
      *
      *    入院会計マスターより編集をする
           PERFORM 410181-NYUINACCT-HEN-SEC.
           PERFORM 410181-NYUINACCT-SEL-SEC.
      *
           IF      SPA-GMN-MAX-41      =   ZERO
               MOVE    07                  TO  SPA-GMN-CUR-41
           ELSE
               MOVE    01                  TO  SPA-GMN-CUR-41
           END-IF.
      *
       410181-SRYYM-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号選択入力処理
      *****************************************************************
       41011-BANGO-CHK-SEC             SECTION.
      *
           MOVE    01                  TO  SPA-GMN-CUR-41.
      *
           IF      I41-NUM             =   SPACE
               MOVE    ZERO                TO  I41-NUM
           END-IF.
           MOVE    SPACE               TO  SPA-ERRCD.
           PERFORM VARYING    IDW5     FROM    1   BY  1
                   UNTIL      IDW5     >   3
               IF      I41-NUM(IDW5:1)     =   "*" OR "+" OR "-" OR "\"
                                                   OR "." OR ","
                   MOVE    "0001"              TO  SPA-ERRCD
                   MOVE    99                  TO  IDW5
               END-IF
           END-PERFORM.
           IF     SPA-ERRCD        NOT =   SPACE
               GO      TO      41011-BANGO-CHK-EXT
           END-IF.
           INITIALIZE                      ORCSNUMAREA.
           MOVE    I41-NUM             TO  SNUM-INX.
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA.
           IF     (SNUM-RC         NOT =   ZERO)    OR
                  (SNUM-MINKBN     NOT =   SPACE)   OR
                  (SNUM-SYOKBN     NOT =   SPACE)
               MOVE    "0001"              TO  SPA-ERRCD
               GO      TO      41011-BANGO-CHK-EXT
           ELSE
               MOVE    SNUM-OUTNUM         TO  SPA-GMN-NUM-41
           END-IF.
      *
           IF      SPA-GMN-NUM-41    >   SPA-GENSAN-ST4-41
               MOVE    ZERO                TO  SPA-GMN-NUM-41
           END-IF.
      *
           IF      SPA-GMN-NUM-41    >   SPA-NAI-MAX-41
               MOVE    "0002"              TO  SPA-ERRCD
               GO      TO      41011-BANGO-CHK-EXT
           END-IF.
      *
           MOVE    SPACE               TO  SPA-WRK-KOMENTO-SRYCD.
           IF      SPA-GMN-NUM-41    =   ZERO
               INITIALIZE                  SPA-GMN-INPUT-41
               INITIALIZE                  SPA-NAIGMN-AREA-41
               MOVE    ZERO                TO  SPA-I4-SYORI
      *
           ELSE
               INITIALIZE                  SPA-NAIGMN-AREA-41
               MOVE    ZERO                TO  SPA-I4-SYORI
      *
               MOVE    SPA-NAI-TBANGO-41 (SPA-GMN-NUM-41)
                                           TO  IDX
               MOVE    SPA-GMN-TMEISYO-41  (IDX)
                                           TO  SPA-GMN-MEISYO-41
               MOVE    SPA-NAI-DAY-AREA-41 (SPA-GMN-NUM-41)
                                           TO  SPA-GMN-DAY-G-41
               MOVE    SPA-NAI-ROUJIN-41   (SPA-GMN-NUM-41)
                                           TO  SPA-GMN-ROUJIN-41
      *        老人判定
               IF     (COMB-KOH1HKNNUM     =   "027")  OR
                      (COMB-KOH2HKNNUM     =   "027")  OR
                      (COMB-KOH3HKNNUM     =   "027")  OR
                      (COMB-KOH4HKNNUM     =   "027")  OR
                      (SPA-I40-ROUJIN      =   1)
                   MOVE    1               TO  SPA-NAI-ROUJIN-41
                                                      (SPA-GMN-NUM-41)
               ELSE
                   MOVE    ZERO            TO  SPA-NAI-ROUJIN-41
                                                     (SPA-GMN-NUM-41)
               END-IF
               MOVE    SPA-NAI-ROUJIN-41   (SPA-GMN-NUM-41)
                                           TO  SPA-GMN-ROUJIN-41
      *
               IF     SPA-GMN-TZAISKBKBN-41  (IDX)   =   "2"  OR  "3"
                                                     OR  "4"  OR  "5"
                   MOVE    SPA-GMN-TSRYCD-41  (IDX)
                                           TO  SPA-WRK-KOMENTO-SRYCD
               END-IF
      *
               MOVE    1                   TO  SPA-IDXZ-41
      *
               MOVE    2                   TO  SPA-GMN-CUR-41
               MOVE    ZERO                TO  SPA-DAY-IDX-41
           END-IF.
      *
       41011-BANGO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤回数入力処理
      *****************************************************************
       41012-DAY-CHK-SEC             SECTION.
      *
           MOVE    2                   TO  SPA-GMN-CUR-41.
           MOVE    ZERO                TO  SPA-WRK-KOMENTO-E.
      *
           MOVE    SPA-GMN-DAY-G-41    TO  WRK-GMN-DAY-G.
           MOVE    SPA-DAY-IDX-41      TO  IDX.
           MOVE    SPACE               TO  SPA-ERRCD.
           PERFORM VARYING    IDW5     FROM    1   BY  1
                   UNTIL      IDW5     >   2
               IF      I41-DAY (IDX)(IDW5:1)   =   "*" OR "+" OR "-" OR
                                                   "\" OR "." OR ","
                   MOVE    "0003"              TO  SPA-ERRCD
                   MOVE    99                  TO  IDW5
               END-IF
           END-PERFORM.
           IF     SPA-ERRCD        NOT =   SPACE
               GO      TO      41012-DAY-CHK-EXT
           END-IF.
           INITIALIZE                      ORCSNUMAREA.
           MOVE    I41-DAY (IDX)       TO  SNUM-INX.
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA.
           IF     (SNUM-RC         NOT =   ZERO)    OR
                  (SNUM-MINKBN     NOT =   SPACE)   OR
                  (SNUM-SYOKBN     NOT =   SPACE)
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    WRK-GMN-DAY-G   TO  SPA-GMN-DAY-G-41
               GO      TO      41012-DAY-CHK-EXT
           ELSE
               MOVE    SNUM-OUTNUM     TO  SPA-GMN-DAY-41 (IDX)
           END-IF.
           INITIALIZE                      ORCSNUMAREA.
           MOVE    I41-NUM             TO  SNUM-INX.
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA.
           IF     SNUM-RC         =   ZERO
               MOVE    SNUM-OUTNUM     TO  IDX3
           END-IF.
           IF     SPA-GMN-NUM-41              =   ZERO
               MOVE    "0013"              TO  SPA-ERRCD
               MOVE    WRK-GMN-DAY-G   TO  SPA-GMN-DAY-G-41
               GO      TO      41012-DAY-CHK-EXT
           END-IF.
           MOVE    SPA-NAI-TBANGO-41 (SPA-GMN-NUM-41)   TO  IDX2.
           IF     SPA-WRK-CHK-KBN (IDX)   NOT =  "1"
               MOVE    "0024"          TO  SPA-ERRCD
               MOVE    WRK-GMN-DAY-G   TO  SPA-GMN-DAY-G-41
               GO      TO      41012-DAY-CHK-EXT
           END-IF.
           IF    (SPA-GMN-TZAISKBKBN-41 (IDX2)   =  "1")   AND
                 (I41-DAY (IDX)              NOT = SPACE)  AND
                 (SPA-GMN-DAY-41 (IDX)       NOT =   1)
               MOVE    "0024"          TO  SPA-ERRCD
               MOVE    WRK-GMN-DAY-G   TO  SPA-GMN-DAY-G-41
               GO      TO      41012-DAY-CHK-EXT
           END-IF.
           IF    (SPA-GMN-TZAISKBKBN-41 (IDX2)   =  "2")   AND
                 (I41-DAY (IDX)              NOT = SPACE)  AND
                 (SPA-GMN-DAY-41 (IDX)       NOT =   1)    AND
                 (SPA-GMN-DAY-41 (IDX)       NOT =   2)    AND
                 (SPA-GMN-DAY-41 (IDX)       NOT =   3)    AND
                 (SPA-GMN-DAY-41 (IDX)       NOT =   4)
               MOVE    "0024"          TO  SPA-ERRCD
               MOVE    WRK-GMN-DAY-G   TO  SPA-GMN-DAY-G-41
               GO      TO      41012-DAY-CHK-EXT
           END-IF.
           IF    SPA-GMN-TZAISKBKBN-41 (IDX2)   =  "3"
               MOVE    "0024"          TO  SPA-ERRCD
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY             >   SPA-SAGAKU-MAX-41
                   IF    SPA-GMN-DAY-41 (IDX)(2:2)
                                      =  SPA-GMN-SAGAKUNO-41 (IDY)(1:2)
                       MOVE    SPACE           TO  SPA-ERRCD
                       MOVE    99              TO  IDY
                   END-IF
               END-PERFORM
               IF    I41-DAY (IDX)   =  SPACE
                   MOVE    SPACE           TO  SPA-ERRCD
               END-IF
               IF    SPA-ERRCD    NOT = SPACE
                   MOVE    WRK-GMN-DAY-G   TO  SPA-GMN-DAY-G-41
                   GO      TO      41012-DAY-CHK-EXT
               END-IF
           END-IF.
           IF    (SPA-GMN-TZAISKBKBN-41 (IDX2)   =  "4")  AND
                 (I41-DAY (IDX)              NOT = SPACE)  AND
                 (SPA-GMN-DAY-41 (IDX)       NOT = ZERO)   AND
                 (SPA-GMN-DAY-41 (IDX)       NOT =   1)    AND
                 (SPA-GMN-DAY-41 (IDX)       NOT =   2)    AND
                 (SPA-GMN-DAY-41 (IDX)       NOT =   3)    AND
                 (SPA-GMN-DAY-41 (IDX)       NOT =   4)
               MOVE    "0024"          TO  SPA-ERRCD
               MOVE    WRK-GMN-DAY-G   TO  SPA-GMN-DAY-G-41
               GO      TO      41012-DAY-CHK-EXT
           END-IF.
           IF    (SPA-GMN-TZAISKBKBN-41 (IDX2)   =  "4")  AND
                 (SPA-WRK-CHK-SIKIBETU (IDX) NOT = SPACE)
               MOVE    "0024"          TO  SPA-ERRCD
               MOVE    WRK-GMN-DAY-G   TO  SPA-GMN-DAY-G-41
               GO      TO      41012-DAY-CHK-EXT
           END-IF.
           IF    SPA-GMN-TZAISKBKBN-41 (IDX2)   =  "5"
               MOVE    "0024"          TO  SPA-ERRCD
               PERFORM VARYING     IDY     FROM   1   BY  1
                       UNTIL       IDY            >   I41-KUMILIST-COUNT
                   IF    SPA-GMN-DAY-41 (IDX)(2:2)
                                      =  I41-KUMINO (IDY)(3:2)
                       MOVE    SPACE           TO  SPA-ERRCD
                       MOVE    99              TO  IDY
                   END-IF
               END-PERFORM
               IF    SPA-ERRCD    NOT = SPACE
                   MOVE    WRK-GMN-DAY-G   TO  SPA-GMN-DAY-G-41
                   GO      TO      41012-DAY-CHK-EXT
               END-IF
           END-IF.
           IF    (SPA-GMN-TZAISKBKBN-41 (IDX2)   =  "6")   AND
                 (I41-DAY (IDX)              NOT = SPACE)  AND
                 (SPA-GMN-DAY-41 (IDX)       NOT =   1)
               MOVE    "0024"          TO  SPA-ERRCD
               MOVE    WRK-GMN-DAY-G   TO  SPA-GMN-DAY-G-41
               GO      TO      41012-DAY-CHK-EXT
           END-IF.
           IF    (SPA-GMN-TZAISKBKBN-41 (IDX2)   =  "6")  AND
                 (SPA-WRK-CHK-SIKIBETU (IDX) NOT = SPACE)
               MOVE    "0024"          TO  SPA-ERRCD
               MOVE    WRK-GMN-DAY-G   TO  SPA-GMN-DAY-G-41
               GO      TO      41012-DAY-CHK-EXT
           END-IF.
           IF    (SPA-NAI-TZAISKBKBN-41 (IDX3)      =  "1")      AND
                 (SPA-GMN-TSRYCD-41 (IDX2)      NOT = W-YOYAKU)  AND
                 (SPA-GMN-DAY-41 (IDX)          NOT = ZERO)      AND
                 (WRK-GMN-DAY (IDX)                 = ZERO)      AND
                 (WRK-CHK-DAY (IDX)             NOT = ZERO)
               MOVE    "0024"          TO  SPA-ERRCD
               MOVE    WRK-GMN-DAY-G   TO  SPA-GMN-DAY-G-41
               GO      TO      41012-DAY-CHK-EXT
           END-IF.
           IF    (SPA-NAI-TZAISKBKBN-41 (IDX3)      =  "1")      AND
                 (SPA-GMN-TSRYCD-41 (IDX2)          = W-YOYAKU)  AND
                 (SPA-GMN-DAY-41 (IDX)          NOT = ZERO)      AND
                 (WRK-CHK-DAY (IDX)                 = ZERO)
               MOVE    "0024"          TO  SPA-ERRCD
               MOVE    WRK-GMN-DAY-G   TO  SPA-GMN-DAY-G-41
               GO      TO      41012-DAY-CHK-EXT
           END-IF.
      *     IF    (SPA-NAI-TZAISKBKBN-41 (IDX3)   =  "2" OR "4")  AND
      *           (SPA-GMN-SRYKA-I-41             =  "00")
      *         MOVE    "0025"          TO  SPA-ERRCD
      *         MOVE    WRK-GMN-DAY-G   TO  SPA-GMN-DAY-G-41
      *         GO      TO      41012-DAY-CHK-EXT
      *     END-IF.
           IF    (SPA-NAI-TZAISKBKBN-41 (IDX3)  NOT =  "1")  AND
                 (SPA-GMN-DAY-41 (IDX)          NOT = ZERO)  AND
                 (WRK-CHK-DAY (IDX)                 = ZERO)
               MOVE    "0026"          TO  SPA-ERRCD
               MOVE    WRK-GMN-DAY-G   TO  SPA-GMN-DAY-G-41
               GO      TO      41012-DAY-CHK-EXT
           END-IF.
      *
           IF      SPA-IDXZ-41         >=  SPA-SRYMATU-41
               MOVE    3                   TO  SPA-GMN-CUR-41
           ELSE
               ADD     1                   TO  SPA-IDXZ-41
           END-IF.
           ADD     1               TO  SPA-DAY-IDX-41.
           IF      SPA-DAY-IDX-41      >   SPA-SRYMATU-41
               MOVE    5                   TO  SPA-GMN-CUR-41
           END-IF.
      *
       41012-DAY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細選択処理
      *****************************************************************
       41012-DATALIST-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM.
           MOVE    ZERO                TO  WRK-SELNUM2.
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX-41
              IF      I41-DATELIST-SELECT (IDX)   =  "T"
                  IF      SPA-NAI-TBANGO2-41 (IDX)    =   SPA-GMN-NUM-41
                      MOVE    SPA-NAI-TBANGO2-41 (IDX)   TO  WRK-SELNUM
                  ELSE
                      MOVE    SPA-NAI-TBANGO2-41 (IDX)   TO  WRK-SELNUM2
                  END-IF
              END-IF
           END-PERFORM.
      *
           IF     (WRK-SELNUM          =   ZERO)  AND
                  (WRK-SELNUM2     NOT =   ZERO)
               MOVE    WRK-SELNUM2         TO  SPA-GMN-NUM-41
           ELSE
               IF      WRK-SELNUM2         =   ZERO
                   CONTINUE
               ELSE
                   MOVE    SPA-GMN-NUM-41      TO  SPA-MAE-BANGO-41
                   MOVE    WRK-SELNUM2         TO  SPA-GMN-NUM-41
                   MOVE    "0103"              TO  SPA-IIDCD
                   GO      TO      41012-DATALIST-EXT
               END-IF
           END-IF.
      *
           MOVE    1                   TO  SPA-GMN-CUR-41.
           MOVE    SPA-GMN-NUM-41      TO  I41-NUM.
           PERFORM 41011-BANGO-CHK-SEC.
      *
       41012-DATALIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括修正入力処理
      *****************************************************************
       41017-KOMENTO-CHK-SEC              SECTION.
      *
           MOVE    I41-KOMENTO         TO  SPA-GMN-KOMENTO-41.
           MOVE    05                  TO  SPA-GMN-CUR-41.
      *
           IF     I41-KOMENTO          =   SPACE
               MOVE    03                  TO  SPA-GMN-CUR-41
               GO      TO      41017-KOMENTO-CHK-EXT
           END-IF.
      *
           MOVE    SPA-GMN-DAY-G-41    TO  WRK-GMN-DAY-G.
           MOVE    SPACE               TO  SPA-ERRCD.
           MOVE    ZERO                TO  IDW3.
           MOVE    ZERO                TO  IDW4.
           MOVE    ZERO                TO  IDW6.
           MOVE    ZERO                TO  IDW7.
           PERFORM VARYING    IDW5     FROM    1   BY  1
                   UNTIL      IDW5     >   8
               IF      I41-KOMENTO(IDW5:1)   =   "*" OR "+" OR "\" OR
                                                 ","
                   MOVE    "0022"              TO  SPA-ERRCD
                   MOVE    99                  TO  IDW5
               END-IF
               IF      I41-KOMENTO(IDW5:1)   =   "-"
                   MOVE    IDW5                TO  IDW3
                   COMPUTE   IDW6    =    IDW6    +    1
               END-IF
               IF      I41-KOMENTO(IDW5:1)   =   "/"
                   MOVE    IDW5                TO  IDW4
                   COMPUTE   IDW7    =    IDW7    +    1
               END-IF
           END-PERFORM.
           IF     (IDW6   NOT =   1)     OR
                  (IDW7   NOT =   1)     OR
                  (IDW3       <   IDW4)
               MOVE    "0022"              TO  SPA-ERRCD
           END-IF.
           IF     SPA-ERRCD        NOT =   SPACE
               MOVE    03                  TO  SPA-GMN-CUR-41
               GO      TO      41017-KOMENTO-CHK-EXT
           END-IF.
           INITIALIZE                      ORCSNUMAREA.
           MOVE    1                   TO  IDW6.
           MOVE    1                   TO  IDW7.
           PERFORM UNTIL   I41-KOMENTO(IDW6:1)   =   "/"
               MOVE    I41-KOMENTO(IDW6:1) TO  SNUM-INX(IDW7:1)
               ADD     1                   TO  IDW6
               ADD     1                   TO  IDW7
           END-PERFORM.
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA.
           IF      SNUM-RC         NOT =   ZERO
               MOVE    03                  TO  SPA-GMN-CUR-41
               MOVE    "0022"              TO  SPA-ERRCD
               GO      TO      41017-KOMENTO-CHK-EXT
           END-IF.
           MOVE    SNUM-OUTNUM         TO  SPA-WRK-KOMENTO-D.
           INITIALIZE                      ORCSNUMAREA.
           ADD     1                   TO  IDW6.
           MOVE    1                   TO  IDW7.
           PERFORM UNTIL   I41-KOMENTO(IDW6:1)   =   "-"
               MOVE    I41-KOMENTO(IDW6:1) TO  SNUM-INX(IDW7:1)
               ADD     1                   TO  IDW6
               ADD     1                   TO  IDW7
           END-PERFORM.
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA.
           IF      SNUM-RC         NOT =   ZERO
               MOVE    03                  TO  SPA-GMN-CUR-41
               MOVE    "0022"              TO  SPA-ERRCD
               GO      TO      41017-KOMENTO-CHK-EXT
           END-IF.
           MOVE    SNUM-OUTNUM         TO  SPA-WRK-KOMENTO-S.
           INITIALIZE                      ORCSNUMAREA.
           ADD     1                   TO  IDW6.
           MOVE    I41-KOMENTO(IDW6:2) TO  SNUM-INX(1:2).
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA.
           IF      SNUM-RC         NOT =   ZERO
               MOVE    03                  TO  SPA-GMN-CUR-41
               MOVE    "0022"              TO  SPA-ERRCD
               GO      TO      41017-KOMENTO-CHK-EXT
           END-IF.
           MOVE    SNUM-OUTNUM         TO  SPA-WRK-KOMENTO-E.
           IF     (SPA-WRK-KOMENTO-S      <   1)    OR
                  (SPA-WRK-KOMENTO-S      >   31)   OR
                  (SPA-WRK-KOMENTO-E      <   1)    OR
                  ((SPA-WRK-KOMENTO-E     >   31)   AND
                   (SPA-WRK-KOMENTO-E NOT =   99))   OR
                  (SPA-WRK-KOMENTO-S      >   SPA-WRK-KOMENTO-E)
               MOVE    03                  TO  SPA-GMN-CUR-41
               MOVE    "0022"              TO  SPA-ERRCD
               GO      TO      41017-KOMENTO-CHK-EXT
           END-IF.
           IF     (SPA-WRK-KOMENTO-SRYCD   =   SPACE)    AND
                  (SPA-WRK-KOMENTO-E       =   99)
               MOVE    03                  TO  SPA-GMN-CUR-41
               MOVE    "0024"              TO  SPA-ERRCD
               GO      TO      41017-KOMENTO-CHK-EXT
           END-IF.
           INITIALIZE                      ORCSNUMAREA.
           MOVE    I41-NUM             TO  SNUM-INX.
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA.
           IF      SNUM-RC         NOT =   ZERO
               MOVE    03                  TO  SPA-GMN-CUR-41
               MOVE    "0022"              TO  SPA-ERRCD
               GO      TO      41017-KOMENTO-CHK-EXT
           END-IF.
           MOVE    SNUM-OUTNUM         TO  IDX3.
           IF      IDX3         =   ZERO
               MOVE    03                  TO  SPA-GMN-CUR-41
               MOVE    "0013"              TO  SPA-ERRCD
               GO      TO      41017-KOMENTO-CHK-EXT
           END-IF.
           MOVE    SPA-NAI-TBANGO-41 (SPA-GMN-NUM-41)   TO  IDX2.
           IF     SPA-WRK-CHK-KBN (SPA-WRK-KOMENTO-S)   NOT =  "1"
               MOVE    03                  TO  SPA-GMN-CUR-41
               MOVE    "0024"              TO  SPA-ERRCD
               GO      TO      41017-KOMENTO-CHK-EXT
           END-IF.
           IF     SPA-WRK-KOMENTO-E   NOT =  99
               IF     SPA-WRK-CHK-KBN (SPA-WRK-KOMENTO-E)   NOT =  "1"
                   MOVE    03                  TO  SPA-GMN-CUR-41
                   MOVE    "0024"              TO  SPA-ERRCD
                   GO      TO      41017-KOMENTO-CHK-EXT
               END-IF
           ELSE
               IF     SPA-WRK-CHK-KBN (IDV)   NOT =  "1"
                   MOVE    03                  TO  SPA-GMN-CUR-41
                   MOVE    "0024"              TO  SPA-ERRCD
                   GO      TO      41017-KOMENTO-CHK-EXT
               END-IF
           END-IF.
           IF     (SPA-WRK-KOMENTO-E              =   99)   AND
                  (SPA-GMN-TZAISKBKBN-41 (IDX2)   =  "1"  OR  "2")
               MOVE    03                  TO  SPA-GMN-CUR-41
               MOVE    "0024"              TO  SPA-ERRCD
               GO      TO      41017-KOMENTO-CHK-EXT
           END-IF.
           IF    (SPA-GMN-TZAISKBKBN-41 (IDX2)   =  "1")   AND
                 (SPA-WRK-KOMENTO-D          NOT =  ZERO)  AND
                 (SPA-WRK-KOMENTO-D          NOT =   1)
               MOVE    "0024"          TO  SPA-ERRCD
               MOVE    WRK-GMN-DAY-G   TO  SPA-GMN-DAY-G-41
               MOVE    03              TO  SPA-GMN-CUR-41
               GO      TO      41017-KOMENTO-CHK-EXT
           END-IF.
           IF    (SPA-GMN-TZAISKBKBN-41 (IDX2)   =  "2")   AND
                 (SPA-WRK-KOMENTO-D          NOT =  ZERO)  AND
                 (SPA-WRK-KOMENTO-D          NOT =   1)    AND
                 (SPA-WRK-KOMENTO-D          NOT =   2)    AND
                 (SPA-WRK-KOMENTO-D          NOT =   3)    AND
                 (SPA-WRK-KOMENTO-D          NOT =   4)
               MOVE    "0024"          TO  SPA-ERRCD
               MOVE    WRK-GMN-DAY-G   TO  SPA-GMN-DAY-G-41
               MOVE    03              TO  SPA-GMN-CUR-41
               GO      TO      41017-KOMENTO-CHK-EXT
           END-IF.
           IF    SPA-GMN-TZAISKBKBN-41 (IDX2)   =  "3"
               MOVE    "0024"          TO  SPA-ERRCD
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY             >   SPA-SAGAKU-MAX-41
                   IF    SPA-WRK-KOMENTO-D(1:2)
                                       =  SPA-GMN-SAGAKUNO-41 (IDY)(1:2)
                       MOVE    SPACE           TO  SPA-ERRCD
                       MOVE    99              TO  IDY
                   END-IF
               END-PERFORM
               IF    SPA-WRK-KOMENTO-D   =  ZERO
                   MOVE    SPACE           TO  SPA-ERRCD
               END-IF
               IF    SPA-ERRCD    NOT = SPACE
                   MOVE    WRK-GMN-DAY-G   TO  SPA-GMN-DAY-G-41
                   MOVE    03              TO  SPA-GMN-CUR-41
                   GO      TO      41017-KOMENTO-CHK-EXT
               END-IF
           END-IF.
           IF    (SPA-GMN-TZAISKBKBN-41 (IDX2)   =  "4")  AND
                 (SPA-WRK-KOMENTO-D          NOT = ZERO)  AND
                 (SPA-WRK-KOMENTO-D          NOT =   1)   AND
                 (SPA-WRK-KOMENTO-D          NOT =   2)   AND
                 (SPA-WRK-KOMENTO-D          NOT =   3)   AND
                 (SPA-WRK-KOMENTO-D          NOT =   4)
               MOVE    "0024"          TO  SPA-ERRCD
               MOVE    WRK-GMN-DAY-G   TO  SPA-GMN-DAY-G-41
               MOVE    03              TO  SPA-GMN-CUR-41
               GO      TO      41017-KOMENTO-CHK-EXT
           END-IF.
           IF    SPA-GMN-TZAISKBKBN-41 (IDX2)   =  "5"
               MOVE    "0024"          TO  SPA-ERRCD
               PERFORM VARYING     IDY     FROM   1   BY  1
                       UNTIL       IDY            >   I41-KUMILIST-COUNT
                   IF    SPA-WRK-KOMENTO-D(1:2)
                                      =  I41-KUMINO (IDY)(3:2)
                       MOVE    SPACE           TO  SPA-ERRCD
                       MOVE    99              TO  IDY
                   END-IF
               END-PERFORM
               IF    SPA-ERRCD    NOT = SPACE
                   MOVE    WRK-GMN-DAY-G   TO  SPA-GMN-DAY-G-41
                   MOVE    03              TO  SPA-GMN-CUR-41
                   GO      TO      41017-KOMENTO-CHK-EXT
               END-IF
           END-IF.
           IF    (SPA-GMN-TZAISKBKBN-41 (IDX2)   =  "6")   AND
                 (SPA-WRK-KOMENTO-D          NOT =  ZERO)  AND
                 (SPA-WRK-KOMENTO-D          NOT =   1)
               MOVE    "0024"          TO  SPA-ERRCD
               MOVE    WRK-GMN-DAY-G   TO  SPA-GMN-DAY-G-41
               MOVE    03              TO  SPA-GMN-CUR-41
               GO      TO      41017-KOMENTO-CHK-EXT
           END-IF.
      *     IF   (SPA-NAI-TZAISKBKBN-41 (IDX3)  =  "2" OR "4")  AND
      *          (SPA-GMN-SRYKA-I-41            =  "00")
      *         MOVE    03            TO  SPA-GMN-CUR-41
      *         MOVE    "0025"        TO  SPA-ERRCD
      *         GO      TO      41017-KOMENTO-CHK-EXT
      *     END-IF.
           IF     SPA-WRK-KOMENTO-E   NOT =  99
               PERFORM   VARYING  IDX  FROM  SPA-WRK-KOMENTO-S  BY  1
                         UNTIL    IDX       >    SPA-WRK-KOMENTO-E
                  IF    (SPA-NAI-TZAISKBKBN-41 (IDX3)    =  "1")     AND
                        (SPA-GMN-TSRYCD-41 (IDX2)    NOT = W-YOYAKU) AND
                        (WRK-GMN-DAY (IDX)               = ZERO)     AND
                        (WRK-CHK-DAY (IDX)           NOT = ZERO)
                      MOVE    03           TO  SPA-GMN-CUR-41
                      MOVE    "0024"       TO  SPA-ERRCD
                  END-IF
                  IF    (SPA-NAI-TZAISKBKBN-41 (IDX3)    =  "1")     AND
                        (SPA-GMN-TSRYCD-41 (IDX2)        = W-YOYAKU) AND
                        (WRK-CHK-DAY (IDX)               = ZERO)
                      MOVE    03           TO  SPA-GMN-CUR-41
                      MOVE    "0024"       TO  SPA-ERRCD
                  END-IF
                  IF   (SPA-NAI-TZAISKBKBN-41 (IDX3) NOT =  "1")  AND
                       (WRK-CHK-DAY (IDX)                = ZERO)
                     MOVE    03            TO  SPA-GMN-CUR-41
                     MOVE    "0026"        TO  SPA-ERRCD
                  END-IF
                  IF   (SPA-NAI-TZAISKBKBN-41 (IDX3)  =  "4" OR "6") AND
                       (SPA-WRK-CHK-SIKIBETU (IDX)    =  "1")
                     MOVE    03            TO  SPA-GMN-CUR-41
                     MOVE    "0024"        TO  SPA-ERRCD
                  END-IF
               END-PERFORM
               IF   SPA-ERRCD   =  SPACE
                  PERFORM   VARYING  IDX  FROM  SPA-WRK-KOMENTO-S  BY  1
                            UNTIL    IDX       >    SPA-WRK-KOMENTO-E
                      MOVE   SPA-WRK-KOMENTO-D TO  SPA-GMN-DAY-41 (IDX)
                  END-PERFORM
               END-IF
           ELSE
               PERFORM   VARYING  IDX  FROM  SPA-WRK-KOMENTO-S  BY  1
                         UNTIL    IDX      >    IDV
                  IF    (SPA-NAI-TZAISKBKBN-41 (IDX3)    =  "1")     AND
                        (SPA-GMN-TSRYCD-41 (IDX2)    NOT = W-YOYAKU) AND
                        (WRK-GMN-DAY (IDX)               = ZERO)     AND
                        (WRK-CHK-DAY (IDX)           NOT = ZERO)
                      MOVE    03           TO  SPA-GMN-CUR-41
                      MOVE    "0024"       TO  SPA-ERRCD
                  END-IF
                  IF    (SPA-NAI-TZAISKBKBN-41 (IDX3)    =  "1")     AND
                        (SPA-GMN-TSRYCD-41 (IDX2)        = W-YOYAKU) AND
                        (WRK-CHK-DAY (IDX)               = ZERO)
                      MOVE    03           TO  SPA-GMN-CUR-41
                      MOVE    "0024"       TO  SPA-ERRCD
                  END-IF
                  IF   (SPA-NAI-TZAISKBKBN-41 (IDX3) NOT =  "1")  AND
                       (WRK-CHK-DAY (IDX)                = ZERO)
                     MOVE    03            TO  SPA-GMN-CUR-41
                     MOVE    "0026"        TO  SPA-ERRCD
                  END-IF
                  IF   (SPA-NAI-TZAISKBKBN-41 (IDX3)  =  "4" OR "6") AND
                       (SPA-WRK-CHK-SIKIBETU (IDX)    =  "1")
                     MOVE    03            TO  SPA-GMN-CUR-41
                     MOVE    "0024"        TO  SPA-ERRCD
                  END-IF
               END-PERFORM
               IF   SPA-ERRCD   =  SPACE
                  PERFORM   VARYING  IDX  FROM  SPA-WRK-KOMENTO-S  BY  1
                            UNTIL    IDX      >    IDV
                      MOVE   SPA-WRK-KOMENTO-D TO  SPA-GMN-DAY-41 (IDX)
                  END-PERFORM
               END-IF
           END-IF.
      *
       41017-KOMENTO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤変更入力可能区分セット
      *****************************************************************
       41013-DAY-CHK2-SEC              SECTION.
      *
      *    患者入院履歴を検索する
      *    医療機関、患者ＩＤ、転入日、転出日を参照
           INITIALIZE                  PTNYUINRRK-REC.
           MOVE    SPA-HOSPID          TO  PTNYUINRRK-HOSPID.
           MOVE    SPA-GMN-PTID        TO  PTNYUINRRK-PTID.
           IF     SPA-GMN-SRYKA-I-41   NOT =   "00"
               MOVE    SPA-GMN-SRYKA-I-41  TO  PTNYUINRRK-NYUINKA
               MOVE    "PTNYUINRRK-KEY18"  TO  WRK-DBPATH
           ELSE
               MOVE    "PTNYUINRRK-KEY19"  TO  WRK-DBPATH
           END-IF.
           MOVE    SPA-NAI-SRYYM-41    TO  PTNYUINRRK-TENNYUYMD(1:6).
           MOVE    "31"                TO  PTNYUINRRK-TENNYUYMD(7:2).
           MOVE    SPA-NAI-SRYYM-41    TO  PTNYUINRRK-TENSTUYMD(1:6).
           MOVE    "01"                TO  PTNYUINRRK-TENSTUYMD(7:2).
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    WRK-DBPATH          TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    WRK-DBPATH          TO  ORC-DBPATH
               PERFORM 970-PTNYUINRRK-READ-SEC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF.
      *
           MOVE    ALL SPACE           TO  SPA-WRK-CHK-KBN-TBL.
           IF      FLG-PTNYUINRRK             =   1
               MOVE    "0031"              TO  SPA-ERRCD
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX             >   SPA-GMN-MAX-41
                   MOVE    ALL SPACE      TO  SPA-GMN-TBLREC-41 (IDX)
                   MOVE    ZERO           TO  SPA-GMN-MAX-41
               END-PERFORM
               GO      TO       41013-DAY-CHK2-EXT
           ELSE
               PERFORM UNTIL      FLG-PTNYUINRRK    =   1
                   IF     SPA-NAI-SRYYM-41   =
                                             PTNYUINRRK-TENNYUYMD(1:6)
                       MOVE    PTNYUINRRK-TENNYUYMD(7:2)  TO  SNUM-INX
                       CALL    "ORCSNUM"           USING
                                                   ORCSNUMAREA
                       IF     (SNUM-RC         NOT =   ZERO)    OR
                              (SNUM-MINKBN     NOT =   SPACE)   OR
                              (SNUM-SYOKBN     NOT =   SPACE)
                           MOVE    "0031"          TO  SPA-ERRCD
                           GO      TO      41013-DAY-CHK2-EXT
                       ELSE
                           MOVE    SNUM-OUTNUM     TO  IDW8
                       END-IF
                   ELSE
                       MOVE    1               TO  IDW8
                   END-IF
      *
                   IF     SPA-NAI-SRYYM-41   =
                                             PTNYUINRRK-TENSTUYMD(1:6)
                       MOVE    PTNYUINRRK-TENSTUYMD(7:2)  TO  SNUM-INX
                       CALL    "ORCSNUM"           USING
                                                   ORCSNUMAREA
                       IF     (SNUM-RC         NOT =   ZERO)    OR
                              (SNUM-MINKBN     NOT =   SPACE)   OR
                              (SNUM-SYOKBN     NOT =   SPACE)
                           MOVE    "0031"          TO  SPA-ERRCD
                           GO      TO      41013-DAY-CHK2-EXT
                       ELSE
                           IF     PTNYUINRRK-TAIINYMD   =
                                                 PTNYUINRRK-TENSTUYMD
                               MOVE    SNUM-OUTNUM     TO  IDW9
                           ELSE
                               COMPUTE   IDW9   =   SNUM-OUTNUM   -   1
                           END-IF
                       END-IF
                   ELSE
                       MOVE    31              TO  IDW9
                   END-IF
      *
                   PERFORM VARYING    IDK     FROM    IDW8   BY  1
                           UNTIL      IDK     >   IDW9
                       MOVE    "1"    TO  SPA-WRK-CHK-KBN (IDK)
                   END-PERFORM
      *
                   MOVE    WRK-DBPATH          TO  ORC-DBPATH
                   PERFORM 970-PTNYUINRRK-READ-SEC
               END-PERFORM
           END-IF.
      *
           INITIALIZE                        STS-AREA-DAY.
           INITIALIZE                        LNK-DAY7-AREA.
           MOVE    "71"                  TO  LNK-DAY7-IRAI.
           MOVE    SPA-NAI-SRYYM-41      TO  LNK-DAY7-YM.
           CALL    "ORCSDAY"             USING   STS-AREA-DAY
                                                 LNK-DAY7-AREA.
           MOVE    LNK-DAY7-KOYOMI(7:2)  TO  SNUM-INX.
           CALL    "ORCSNUM"             USING
                                         ORCSNUMAREA.
           IF     (SNUM-RC         NOT =   ZERO)    OR
                  (SNUM-MINKBN     NOT =   SPACE)   OR
                  (SNUM-SYOKBN     NOT =   SPACE)
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    WRK-GMN-DAY-G   TO  SPA-GMN-DAY-G-41
               GO      TO      41013-DAY-CHK2-EXT
           ELSE
               MOVE    SNUM-OUTNUM     TO  IDV
               COMPUTE   IDW9   =   SNUM-OUTNUM   +   1
           END-IF.
           PERFORM VARYING    IDW8     FROM    IDW9   BY  1
                   UNTIL      IDW8     >   31
               MOVE    ALL SPACE       TO  SPA-WRK-CHK-KBN (IDW8)
           END-PERFORM.
      *
           PERFORM   VARYING  IDX  FROM  1  BY 1
                     UNTIL    IDX    >   SPA-GMN-MAX-41
               IF      SPA-GMN-TZAISKBKBN-41 (IDX)   =  "2"
                   PERFORM VARYING    IDK      FROM    1   BY  1
                           UNTIL      IDK      >   31
                       IF    SPA-GMN-TDAY-41 (IDX IDK)   NOT =   SPACE
                           MOVE    "1"    TO  SPA-WRK-CHK-SIKIBETU (IDK)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM.
      *
       41013-DAY-CHK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       41012-SEIWA-HEN-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF.
      *
       41012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア画面へ
      *****************************************************************
       420-CLEA-SEC        SECTION.
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC.
      *
           IF      SPA-GMN-NUM-41    =   ZERO
      *        前回患者へ待避
               IF      SPA-GMN-PTNUM    NOT =   SPACE
                   MOVE    SPA-GMN-PTNUM        TO  SPA-LAST-PTNUM
                   MOVE    SPA-GMN-PTID         TO  SPA-LAST-PTID
               END-IF
      *        クリア
               MOVE    ZERO                 TO  SPA-GMN-CUR-41
               INITIALIZE                   SPA-I4-AREA
               INITIALIZE                   SPA-SCR-REC-41
               INITIALIZE                   SPA-5000-GENSAN-DSP-TBL-41
               INITIALIZE                   SPA-5000-GENSAN-CHK-TBL-41
               INITIALIZE                   SPA-I40-SET
               INITIALIZE                   SPA-I41-AREA-41
               MOVE    SPA-I40-SRYYMDW(1:6) TO  SPA-GMN-SRYYM-41
               MOVE    SPA-I40-SRYYMD (1:6) TO  SPA-NAI-SRYYM-41
               MOVE    SPA-I40-SRYYMD (1:6) TO  SPA-SAG-SRYYM-41
               MOVE    SPACE                TO  SPA-GMN-SRYKA-IG-41
               MOVE    "00"                 TO  SPA-GMN-SRYKA-I-41
               MOVE    "全科指定"           TO  SPA-GMN-SRYKAMEI-I-41
               MOVE    ZERO                 TO  IDX
               MOVE    ZERO                 TO  IDG
           ELSE
      *        変更項目クリア
               INITIALIZE                   SPA-GMN-INPUT-41
               INITIALIZE                   SPA-NAIGMN-AREA-41
               MOVE    ZERO                 TO  SPA-I4-SYORI
               MOVE    1                    TO  SPA-GMN-CUR-41
           END-IF.
      *
       420-CLEA-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤変更画面へ
      *****************************************************************
       420-ZAIHENKOU-SEC        SECTION.
      *
           IF      SPA-GMN-NUM-41    =   ZERO
               MOVE    1                   TO  SPA-GMN-CUR-41
               MOVE    "0013"              TO  SPA-ERRCD
               GO      TO      420-ZAIHENKOU-EXT
           END-IF.
      *
      *    剤修正のチェック
           IF     (SPA-GMN-KOMENTO-41    NOT =   SPACE)   OR
                  (SPA-NAI-DAY-AREA-41 (SPA-GMN-NUM-41)    NOT =
                                                       SPA-GMN-DAY-G-41)
               MOVE    1                   TO  SPA-GMN-CUR-41
               MOVE    "0009"              TO  SPA-ERRCD
               GO      TO      420-ZAIHENKOU-EXT
           END-IF.
      *
           IF      SPA-NAI-TZAISKBKBN-41 (SPA-GMN-NUM-41)   NOT =   "1"
               MOVE    1                   TO  SPA-GMN-CUR-41
               MOVE    "0029"              TO  SPA-ERRCD
               GO      TO      420-ZAIHENKOU-EXT
           END-IF.
           IF      SPA-UPDFLG-41       =   "1"
               PERFORM 4901-NYUINACCT-UPD-SEC
               IF     (SPA-ERRCD       NOT =   SPACE)   OR
                      (SPA-IIDCD       NOT =   SPACE)
                   GO      TO      420-ZAIHENKOU-EXT
               END-IF
           END-IF.
      *
      *    施設基準情報検索
           MOVE    SPACE               TO  SYS-1006-REC.
           MOVE    "1006"              TO  SYS-1006-KANRICD.
           MOVE    "1"                 TO  SYS-1006-KBNCD.
           MOVE    ZERO                TO  SYS-1006-STYUKYMD.
           MOVE    ALL "9"             TO  SYS-1006-EDYUKYMD.
      *    システム管理検索
           MOVE    SYS-1006-REC        TO  MCPDATA-REC.
           MOVE    SPA-NAI-SRYYM-41    TO  ORC-DBYMD(1:6).
           MOVE    "01"                TO  ORC-DBYMD(7:2).
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF.
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1006-REC
           ELSE
               INITIALIZE                  SYS-1006-REC
           END-IF.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX         >  400
               IF      SPA-GMN-TMEISYO-41 (IDX)  NOT =   ALL "-"
                   IF      SPA-GMN-TBANGO-41 (IDX)   =   SPA-GMN-NUM-41
                       MOVE    SPA-GMN-TSRYCD-41 (IDX)   TO  TNS-SRYCD
                       MOVE    500                       TO  IDX
                   END-IF
               END-IF
           END-PERFORM.
      *
           PERFORM 910-TENSU-READ-SEC.
      *
           MOVE    ZERO                TO  FLG-SST.
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   10  )   OR
                              (FLG-SST     =   1   )
               IF      TNS-SSTKIJUNCD (IDX)    =   ZERO
                   CONTINUE
               ELSE
                   MOVE    TNS-SSTKIJUNCD (IDX)    TO  IDY
                   IF      SYS-1006-SSTKIJUN (IDY) =   1
                       MOVE    1               TO  FLG-SST
                   END-IF
               END-IF
           END-PERFORM.
           IF      FLG-SST             =   ZERO
               MOVE    1                   TO  SPA-GMN-CUR-41
               MOVE    "0030"              TO  SPA-ERRCD
               GO      TO      420-ZAIHENKOU-EXT
           END-IF.
      *
           PERFORM VARYING    IDH2     FROM    1   BY  1
                   UNTIL      IDH2     >   31
               IF    SPA-NAI-DAY-41  (SPA-GMN-NUM-41 IDH2)  NOT =  ZERO
                   MOVE  IDH2      TO  IDH3
                   MOVE  32        TO  IDH2
               END-IF
           END-PERFORM.
      *
           PERFORM   VARYING  IDH  FROM  1  BY 1
                     UNTIL    IDH    >   SPA-GENSAN-ST4-41
               IF      SPA-NAI-TZAISKBKBN-41 (IDH)   =  "5"
                   PERFORM VARYING     IDH2    FROM    1   BY  1
                           UNTIL       IDH2    >   SPA-I41-MAX-41
                       IF      SPA-NAI-DAY-41 (IDH IDH3)
                                        =   SPA-I41-THKNCOMBI-41 (IDH2)
                           MOVE    SPA-I41-TKUMI-41  (IDH2)
                                                TO  SPA-I40-HKNCOMBIMEI
                           MOVE    SPA-I41-TFTNWARI-41  (IDH2)
                                                TO  SPA-I40-FTNRATEMEI
      *    労災、自賠責 と健保の保険区分設定
                           IF      SPA-I41-HKNNUM-41 (IDH2)
                                                   =   "971"  OR  "973"
                               MOVE    1            TO  SPA-HKNKBN
                               MOVE    SPA-I41-SISIKBN-41 (IDH2)
                                                    TO  SPA-RSI-SISIKBN
                               MOVE    SPA-I41-HKNKBN-41 (IDH2)
                                                    TO  SPA-RSI-HKNKBN
                           ELSE
                               MOVE    ZERO         TO  SPA-HKNKBN
                           END-IF
                           MOVE    SPA-I41-MAX-41  TO  IDH2
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM.
      *
      *    労災の時、剤点数が外来管理加算料に振り替えた時、エラー
           IF      SPA-HKNKBN   =   1
               PERFORM 4201-RIS-ZAIHENKOU-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    1                   TO  SPA-GMN-CUR-41
                   GO      TO      420-ZAIHENKOU-EXT
               END-IF
           END-IF.
      *
           MOVE    "0104"              TO  SPA-IIDCD.
      *
       420-ZAIHENKOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災の時剤変更チェック処理
      *****************************************************************
       4201-RIS-ZAIHENKOU-SEC        SECTION.
      *
           IF     (SPA-NAI-SYUTEN2-41 (SPA-GMN-NUM-41)
                                        >   ZERO )  AND
                  (SPA-NAI-ZAITEN-41  (SPA-GMN-NUM-41)
                                        >   SPA-NAI-SYUTEN2-41
                                                (SPA-GMN-NUM-41))
               MOVE    "0021"               TO  SPA-ERRCD
           END-IF.
      *
       4201-RIS-ZAIHENKOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤変更画面へ
      *****************************************************************
       420-ZAIHENKOU-SYORI-SEC        SECTION.
      *
      *    選択の内容をパラメタに編集
           MOVE    SPA-NAI-SRYYM-41          TO  SPA-I40-SRYYMD(1:6).
           MOVE    SPA-GMN-SRYYM-41          TO  SPA-I40-SRYYMDW(1:6).
      *
           MOVE    SPA-GMN-NUM-41            TO  IDX.
      *    診療区分剤番号
           MOVE    SPA-NAI-SRYKBN-41 (IDX)   TO  SPA-SRYKBN.
           MOVE    SPA-NAI-ZAINUM-41 (IDX)   TO  SPA-ZAINUM.
      *    枝番
           MOVE    SPA-NAI-RENNUM-41 (IDX)   TO  SPA-RENNUM.
      *    老人区分
           MOVE    SPA-NAI-ROUJIN-41 (IDX)   TO  SPA-ROUJIN.
      *    剤点数
           MOVE    SPA-NAI-ZAITEN-41 (IDX)   TO  SPA-ZAITEN.
      *    回数テーブル
           MOVE    SPA-GMN-DAY-G-41    TO  SPA-SEL-DAY-G.
      *
           MOVE    SPACE               TO  SPA-SEL-RRKYMD.
           MOVE    SPACE               TO  SPA-SEL-RRKYMDG.
           MOVE    ZERO                TO  SPA-SEL-RENNUM.
           MOVE    ZERO                TO  SPA-SEL-DOUJI-RENNUM.
           MOVE    ZERO                TO  SPA-SEL-DENPNUM.
      *
      *    診療開始年月日
           MOVE    ZERO                TO  SPA-I40-SRYYMD(7:2).
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      SPA-GMN-DAY-41 (IDX-D) NOT =   ZERO
                   MOVE    IDX-D           TO  WRK-DD
                   MOVE    WRK-DD          TO  SPA-I40-SRYYMD(7:2)
                   MOVE    31              TO  IDX-D
               END-IF
           END-PERFORM.
      *
           MOVE    SPA-SRYYMD              TO  SPA-MAE-SRYYMD.
           MOVE    SPA-PTID                TO  SPA-MAE-PTID.
           MOVE    SPA-I40-SRYYMD          TO  SPA-SRYYMD.
           MOVE    SPA-GMN-PTID            TO  SPA-PTID.
      *
           MOVE    SPA-GMN-SRYKA-I-41      TO  SPA-I40-SRYKA.
           MOVE    SPA-GMN-SRYKAMEI-I-41   TO  SPA-I40-SRYKAMEI.
           MOVE    SPA-NAI-TSRYKA-41 (IDX) TO  SPA-I40-SRYKA-READ.
      *
      *    入院会計マスター更新
           PERFORM 4901-NYUINACCT-UPD-SEC.
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO       420-ZAIHENKOU-SYORI-EXT
           END-IF.
      *
           MOVE    SPACE               TO  SPA-IIDCD.
           INITIALIZE                  SPA-GMN-INPUT-41.
      *
           MOVE    "I41"               TO  SPA-MOTOPG.
           MOVE    "I44"               TO  SPA-SAKIPG.
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE.
           MOVE   "I44"                TO  MCP-WINDOW.
      *
           MOVE    SPA-I4KYOUTU        TO  SPA-WORK.
           MOVE    SPA-AREA            TO  SPA-COMMON.
      *
           MOVE    SPACE               TO  LINKAREA.
           MOVE    SPA-KYOUTU          TO  LNK-COMMON.
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU.
           MOVE    SPA-I4              TO  LNK-SCRSPA.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       420-ZAIHENKOU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *    更新前のチェック
           IF      SPA-UPDFLG-41          =   "1"
               MOVE    "0102"               TO  SPA-IIDCD
               MOVE    99                   TO  SPA-GMN-CUR-41
               GO      TO      210-BACK-EXT
           END-IF.
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC.
      *
      *    前回患者へ待避
           IF      SPA-GMN-PTNUM    NOT =   SPACE
               MOVE    SPA-GMN-PTNUM        TO  SPA-LAST-PTNUM
               MOVE    SPA-GMN-PTID         TO  SPA-LAST-PTID
           END-IF.
      *
           MOVE    "I41"               TO  SPA-MOTOPG.
           MOVE    SPA-MOTOPG2         TO  SPA-SAKIPG.
           IF      SPA-I40-MOTOPG      =   "M01"
      *        業務選択へ
               MOVE   "M01"                TO  SPA-SAKIPG
               MOVE    SPACE               TO  LINKAREA
               INITIALIZE                  SPA-KYOTUKEY
               MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           ELSE
      *        元画面へ
               MOVE    SPACE               TO  LINKAREA
               IF      SPA-I40-MOTOFLG     =   "0"
                   INITIALIZE                  SPA-KYOTUKEY
               END-IF
               MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
               MOVE    SPACE               TO  SPA-MOTOPG2
           END-IF.
      *
           MOVE    SPACE               TO  SPA-MOTOPG2.
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK-SYORI-SEC           SECTION.
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC.
      *
           MOVE    "I41"               TO  SPA-MOTOPG.
           MOVE    SPA-I40-MOTOPG      TO  SPA-SAKIPG.
           IF      SPA-I40-MOTOFLG     =   "0"
               INITIALIZE                  SPA-KYOTUKEY
           END-IF.
           IF      SPA-I40-MOTOPG      =   "M01"
      *        業務選択へ
               MOVE   "M01"                TO  SPA-SAKIPG
               MOVE    SPACE               TO  LINKAREA
               INITIALIZE                  SPA-KYOTUKEY
               MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           ELSE 
      *        元画面へ
               MOVE    SPACE               TO  LINKAREA
               MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
               MOVE    SPACE               TO  SPA-MOTOPG2
           END-IF.
      *
           MOVE    SPACE               TO  SPA-MOTOPG2.
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回患者処理
      *****************************************************************
       430-MAEKPTNUM-SEC              SECTION.
      *
      *    剤修正のチェック
           IF      SPA-GMN-NUM-41   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      430-MAEKPTNUM-EXT
           END-IF.
      *
           IF      SPA-UPDFLG-41       =   "1"
               PERFORM 4901-NYUINACCT-UPD-SEC
               IF     (SPA-ERRCD       NOT =   SPACE)   OR
                      (SPA-IIDCD       NOT =   SPACE)
                   GO      TO      430-MAEKPTNUM-EXT
               END-IF
           END-IF.
           IF      SPA-LAST-PTNUM      =   SPACE
               GO      TO      430-MAEKPTNUM-EXT
           END-IF.
      *
           MOVE    SPA-LAST-PTNUM      TO  I41-PTNUM.
           PERFORM 41010-PTNUM-SYORI-SEC.
      *
       430-MAEKPTNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック画面へ
      *****************************************************************
       430-CHEKU-SEC              SECTION.
      *    剤修正のチェック
           IF      SPA-GMN-NUM-41   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      430-CHEKU-EXT
           END-IF.
      *    患者選択チェック
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    "0019"          TO  SPA-ERRCD
               GO      TO      430-CHEKU-EXT
           END-IF.
           IF      SPA-UPDFLG-41       =   "1"
               PERFORM 4901-NYUINACCT-UPD-SEC
               IF     (SPA-ERRCD       NOT =   SPACE)   OR
                      (SPA-IIDCD       NOT =   SPACE)
                   GO      TO      430-CHEKU-EXT
               END-IF
           END-IF.
      *
           MOVE    SPA-GMN-SRYYM-41       TO  SPA-I40-SRYYMDW(1:6).
           MOVE    SPA-NAI-SRYYM-41       TO  SPA-I40-SRYYMD (1:6).
           MOVE    SPA-GMN-SRYKA-I-41     TO  SPA-I40-SRYKA.
           MOVE    SPA-GMN-SRYKAMEI-I-41  TO  SPA-I40-SRYKAMEI.
      *
           MOVE    "I41"               TO  SPA-MOTOPG.
           MOVE    "I42"               TO  SPA-SAKIPG.
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "I42"                TO  MCP-WINDOW.
      *
           MOVE    SPA-I4KYOUTU        TO  SPA-WORK.
           MOVE    SPA-AREA            TO  SPA-COMMON.
      *
           MOVE    SPACE               TO  LINKAREA.
           MOVE    SPA-KYOUTU          TO  LNK-COMMON.
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU.
           MOVE    SPA-I4              TO  LNK-SCRSPA.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       430-CHEKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    フリーコメント画面へ
      *****************************************************************
       430-FREEMSG-SEC              SECTION.
      *
      *    剤修正のチェック
           IF      SPA-GMN-NUM-41   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      430-FREEMSG-EXT
           END-IF.
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    "0019"          TO  SPA-ERRCD
               GO      TO      430-FREEMSG-EXT
           END-IF.
           IF      SPA-UPDFLG-41       =   "1"
               PERFORM 4901-NYUINACCT-UPD-SEC
               IF     (SPA-ERRCD       NOT =   SPACE)   OR
                      (SPA-IIDCD       NOT =   SPACE)
                   GO      TO      430-FREEMSG-EXT
               END-IF
           END-IF.
      *
           MOVE    SPA-GMN-SRYYM-41      TO  SPA-I40-SRYYMDW(1:6).
           MOVE    SPA-NAI-SRYYM-41      TO  SPA-I40-SRYYMD (1:6).
           MOVE    SPA-GMN-SRYKA-I-41    TO  SPA-I40-SRYKA.
           MOVE    SPA-GMN-SRYKAMEI-I-41 TO  SPA-I40-SRYKAMEI.
      *
           MOVE    "I41"                 TO  SPA-MOTOPG.
           MOVE    "I43"                 TO  SPA-SAKIPG.
      *
           MOVE   "COMMENT"              TO  MCP-WIDGET.
           MOVE   "CHANGE"               TO  MCP-PUTTYPE.
           MOVE   "I43"                  TO  MCP-WINDOW.
      *
           MOVE    SPA-I4KYOUTU          TO  SPA-WORK.
           MOVE    SPA-AREA              TO  SPA-COMMON.
      *
           MOVE    SPACE                 TO  LINKAREA.
           MOVE    SPA-KYOUTU            TO  LNK-COMMON.
           MOVE    SPA-KYOUTU            TO  LNK-KYOUTU.
           MOVE    SPA-I4                TO  LNK-SCRSPA.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       430-FREEMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    前月選択　処理
      *****************************************************************
       41018-ZENGETU-HEN-SEC             SECTION.
      *
           MOVE    01                  TO  SPA-GMN-CUR-41.
      *
      *    剤修正のチェック
           IF      SPA-GMN-NUM-41   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      41018-ZENGETU-HEN-EXT
           END-IF.
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    "0019"          TO  SPA-ERRCD
               GO      TO      41018-ZENGETU-HEN-EXT
           END-IF.
      *
           MOVE    1                   TO  SPA-I4-SEL.
      *
           IF      SPA-UPDFLG-41          =   "1"
               MOVE    "0106"              TO  SPA-IIDCD
           ELSE
               PERFORM 41018-ZENGETU-SYORI-SEC
           END-IF.
      *
       41018-ZENGETU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    前月選択　処理
      *****************************************************************
       41018-ZENGETU-SYORI-SEC             SECTION.
      *
      *    現在表示されている内容を更新する
      *    入院会計マスター更新
           IF      SPA-UPDFLG-41          =   "1"
               PERFORM 4901-NYUINACCT-UPD-SEC
           END-IF.
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO       41018-ZENGETU-SYORI-EXT
           END-IF.
      *
           IF      SPA-BIRTHDAY(1:6)   =   SPA-NAI-SRYMM-41
               MOVE    "0015"              TO  SPA-ERRCD
               GO      TO       41018-ZENGETU-SYORI-EXT
            END-IF.
      *
           MOVE    ZERO               TO  FLG-SET.
           PERFORM 41018-ZENGETU-COMPUTE-SEC.
      *
           PERFORM UNTIL       FLG-SET   =   1
      *        入院会計マスターより編集をする
               MOVE    SPACE               TO  SPA-ERRCD
               PERFORM 410181-NYUINACCT-HEN-SEC
               IF      SPA-ERRCD          NOT =   SPACE
                   IF     WRK-SRYYM-SML   <   SPA-NAI-SRYYM-41
                       PERFORM 41018-ZENGETU-COMPUTE-SEC
                   ELSE
                       MOVE    1                   TO  FLG-SET
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-SET
               END-IF
           END-PERFORM.
      *
           IF      SPA-ERRCD              =   "0031"
               GO      TO       41018-ZENGETU-SYORI-EXT
           END-IF.
           IF      SPA-ERRCD          NOT =   SPACE
               MOVE    SPA-SAG-SRYYM-41    TO  WRK-SYMD(1:6)
               MOVE    "01"                TO  WRK-SYMD(7:2)
               PERFORM 41012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG(1:6)    TO  SPA-I40-SRYYMDW
               MOVE    WRK-HENYMDG(1:6)    TO  SPA-GMN-SRYYM-41
               MOVE    SPA-SAG-SRYYM-41    TO  SPA-I40-SRYYMD
               MOVE    SPA-SAG-SRYYM-41    TO  SPA-NAI-SRYYM-41
               MOVE    "0027"              TO  SPA-ERRCD
               IF      SPA-GMN-MAX-41     =   ZERO
                   MOVE    07                  TO  SPA-GMN-CUR-41
               END-IF
               GO      TO       41018-ZENGETU-SYORI-EXT
           END-IF.
      *
           MOVE    1                  TO  SPA-GMN-CUR-41.
      *
       41018-ZENGETU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *
       41018-ZENGETU-COMPUTE-SEC           SECTION.
      *
           MOVE    SPA-NAI-SRYYM-41    TO  SPA-SAG-SRYYM-41.
           COMPUTE SPA-NAI-SRYMM-41    =   SPA-NAI-SRYMM-41    -   1.
           IF      SPA-NAI-SRYMM-41    =   ZERO
               MOVE    12                  TO  SPA-NAI-SRYMM-41
               COMPUTE SPA-NAI-SRYYY-41    =   SPA-NAI-SRYYY-41    -   1
           END-IF.
      *
       41018-ZENGETU-COMPUTE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスターより編集　処理
      *****************************************************************
       410181-NYUINACCT-HEN-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINKHNRYO.
           MOVE    SPA-NAI-SRYYM-41    TO  WRK-SYMD(1:6).
           MOVE    "01"                TO  WRK-SYMD(7:2).
           PERFORM 41012-SEIWA-HEN-SEC.
           MOVE    WRK-HENYMDG(1:6)    TO  SPA-GMN-SRYYM-41.
      *
           MOVE    SPA-GMN-SRYYM-41    TO  SPA-I40-SRYYMDW.
           MOVE    SPA-NAI-SRYYM-41    TO  SPA-I40-SRYYMD.
      *
      *    診療年月の末日算定
           INITIALIZE                      STS-AREA-DAY.
           INITIALIZE                      LNK-DAY7-AREA.
           MOVE    "71"                TO  LNK-DAY7-IRAI.
           MOVE    SPA-NAI-SRYYM-41    TO  LNK-DAY7-YM.
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA.
           MOVE    LNK-DAY7-KOYOMI(7:2)    TO  SPA-SRYMATU-41.
      *
           MOVE    ZERO                TO  IDX.
           MOVE    ZERO                TO  IDG.
      *    入院会計マスターを編集
           INITIALIZE                      NYUINACCT-REC.
           MOVE    SPA-HOSPID          TO  NACCT-HOSPID.
           MOVE    SPA-NYUGAIKBN       TO  NACCT-NYUGAIKBN.
           MOVE    SPA-GMN-PTID        TO  NACCT-PTID.
           MOVE    "00"                TO  NACCT-SRYKA.
           MOVE    SPA-NAI-SRYYM-41    TO  NACCT-SRYYM.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "NYUINACCT-KEY16"   TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "NYUINACCT-KEY16"   TO  ORC-DBPATH
               PERFORM 900-NYUINACCT-READ-SEC
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF.
           IF      FLG-NYUINACCT              =   1
               IF      MCP-WIDGET             =   "B06"   OR
                                                  "B07"   OR
                                                  "B12"
                   CONTINUE
               ELSE
                   MOVE    ZERO                TO  SPA-GMN-MAX-41
               END-IF
               MOVE    "0020"              TO  SPA-ERRCD
               GO      TO       410181-NYUINACCT-HEN-EXT
           ELSE
      *
               MOVE    ZERO                TO  SPA-GMN-MAX-41
               INITIALIZE                  SPA-GMN-TBL-41
               INITIALIZE                  SPA-NAI-TBL-41
               INITIALIZE                  SPA-GMN-INPUT-41
               INITIALIZE                  SPA-5000-GENSAN-DSP-TBL-41
               MOVE    1                   TO  SPA-GENSAN-RE1-41
               INITIALIZE                  SPA-5000-GENSAN-CHK-TBL-41
      *
               PERFORM UNTIL      (FLG-NYUINACCT     =   1)
      *
                   ADD     1                   TO  IDX
                   PERFORM 3101-NYUINACCT-SPA-SEC
                   IF      NACCT-ZAISKBKBN     =   "1"
                       MOVE   1                TO   FLG-NYUINKHNRYO
                   END-IF
      *
                   MOVE    "NYUINACCT-KEY16"   TO  ORC-DBPATH
                   PERFORM 900-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF.
      *
           MOVE    IDX                 TO  SPA-NAI-MAX-41.
           MOVE    IDG                 TO  SPA-GMN-MAX-41.
      *
      *    すべての行を選択可能とする
           MOVE    ZERO                TO  WRK-TBANGO.
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   SPA-GMN-MAX-41
               IF      SPA-GMN-TBLREC-41 (IDX)(1:3)    NOT =   "---"
                   IF      SPA-GMN-TBANGO-41 (IDX)     NOT =   ZERO
                       MOVE    SPA-GMN-TBANGO-41 (IDX)    TO  WRK-TBANGO
                   END-IF
                   MOVE    WRK-TBANGO      TO  SPA-NAI-TBANGO2-41 (IDX)
               END-IF
           END-PERFORM.
      *
           IF      IDG                 >   ZERO
               COMPUTE SPA-MAX-PAGE-41    =   (SPA-GMN-MAX-41   -   1)
                                       /    17    +   1
           ELSE
               MOVE    1               TO  SPA-MAX-PAGE-41
           END-IF.
      *
           PERFORM 41013-DAY-CHK2-SEC.
      *
       410181-NYUINACCT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター最大診療年月、最小診療年月抽出　処理
      *****************************************************************
       410181-NYUINACCT-SEL-SEC           SECTION.
      *
           MOVE    "999999"            TO  WRK-SRYYM-SML.
           MOVE    "000000"            TO  WRK-SRYYM-BIG.
           INITIALIZE                      NYUINACCT-REC.
           MOVE    SPA-HOSPID          TO  NACCT-HOSPID.
           MOVE    SPA-NYUGAIKBN       TO  NACCT-NYUGAIKBN.
           MOVE    SPA-GMN-PTID        TO  NACCT-PTID.
           MOVE    "00"                TO  NACCT-SRYKA.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "NYUINACCT-KEY4"    TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "NYUINACCT-KEY4"    TO  ORC-DBPATH
               PERFORM 900-NYUINACCT-READ-SEC
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF.
           PERFORM UNTIL       FLG-NYUINACCT   =   1
               MOVE    MCPDATA-REC           TO  NYUINACCT-REC
               IF      WRK-SRYYM-SML    >   NACCT-SRYYM
                   MOVE    NACCT-SRYYM           TO  WRK-SRYYM-SML
               END-IF
               IF      WRK-SRYYM-BIG    <   NACCT-SRYYM
                   MOVE    NACCT-SRYYM           TO  WRK-SRYYM-BIG
               END-IF
               MOVE    "NYUINACCT-KEY4"      TO  ORC-DBPATH
               PERFORM 900-NYUINACCT-READ-SEC
           END-PERFORM.
      *
       410181-NYUINACCT-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスターチェック　処理
      *****************************************************************
       410181-SRYACT-CHK-SEC               SECTION.
      *
      *    診療行為マスターチェック
           INITIALIZE                       SRYACT-REC.
           MOVE    SPA-HOSPID               TO  SRY-HOSPID.
           MOVE    1                        TO  SRY-NYUGAIKBN.
           MOVE    SPA-GMN-PTID             TO  SRY-PTID.
           MOVE    SPA-NAI-SRYYM-41         TO  SRY-SRYYM.
           MOVE    SPA-5000-DSPCD-41 (IDZ2) TO  SRY-SRYCD (1).
           MOVE    SPA-5000-DSPCD-41 (IDZ2) TO  SRY-SRYCD (2).
           MOVE    SPA-5000-DSPCD-41 (IDZ2) TO  SRY-SRYCD (3).
           MOVE    SPA-5000-DSPCD-41 (IDZ2) TO  SRY-SRYCD (4).
           MOVE    SPA-5000-DSPCD-41 (IDZ2) TO  SRY-SRYCD (5).
      *
           MOVE    SRYACT-REC               TO  MCPDATA-REC.
           MOVE    "DBSELECT"               TO  MCP-FUNC.
           MOVE    "SRYACT-KEY7"            TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"           USING
                                         MCPAREA
                                         ORCMCPAREA
                                         MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "SRYACT-KEY7"       TO  ORC-DBPATH
               PERFORM 930-SRYACT-RAED-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF.
      *
           IF      FLG-SRYACT               =   1
               CONTINUE
           ELSE
               PERFORM UNTIL      (FLG-SRYACT     =   1)  OR
                                  (IDZ3           =   50)
                   ADD     1               TO  IDZ3
                   IF     (SPA-GMN-SRYKA-I-41     =   "00")  OR
                          (SPA-GMN-SRYKA-I-41     =   SRY-SRYKA)
                       MOVE    1             TO
                                         SPA-GENSAN-RE2-41
                       MOVE    SRY-NYUGAIKBN TO
                                         SPA-5000-NYUGAIKBN-41 (IDZ3)
                       MOVE    SRY-SRYKA     TO
                                         SPA-5000-SRYKA-41 (IDZ3)
                       MOVE    SRY-ZAINUM    TO
                                         SPA-5000-ZAINUM-41 (IDZ3)
                       MOVE    SRY-SRYKBN    TO
                                         SPA-5000-SRYKBN-41 (IDZ3)
                   END-IF
                   PERFORM 930-SRYACT-RAED-SEC
               END-PERFORM
           END-IF.
      *
       410181-SRYACT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスターより編集　処理
      *****************************************************************
       410181-SRYACCT-HEN-SEC              SECTION.
      *
      *    診療会計マスターを編集
           INITIALIZE                           SRYACCT-REC.
           MOVE    SPA-HOSPID                   TO  ACCT-HOSPID.
           MOVE    SPA-5000-NYUGAIKBN-41 (IDZ2) TO  ACCT-NYUGAIKBN.
           MOVE    SPA-GMN-PTID                 TO  ACCT-PTID.
           MOVE    SPA-5000-SRYKA-41 (IDZ2)     TO  ACCT-SRYKA.
           MOVE    SPA-NAI-SRYYM-41             TO  ACCT-SRYYM.
           MOVE    SPA-5000-ZAINUM-41 (IDZ2)    TO  ACCT-ZAINUM.
           MOVE    SPA-5000-SRYKBN-41 (IDZ2)    TO  ACCT-SRYKBN.
      *
           MOVE    SRYACCT-REC                  TO  MCPDATA-REC.
           MOVE    "DBSELECT"                   TO  MCP-FUNC.
           MOVE    "SRYACCT-KEY"                TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"                  USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "SRYACCT-KEY"      TO  ORC-DBPATH
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF.
      *
           IF      FLG-SRYACCT               =   1
               CONTINUE
           ELSE
               ADD     1                   TO  IDX
               PERFORM 3101-SRYACCT-SPA-SEC
               MOVE    IDX                 TO  SPA-GENSAN-ST1-41
               MOVE    IDG                 TO  SPA-GENSAN-ST2-41
               ADD     1                   TO  SPA-GENSAN-ST3-41
           END-IF.
      *
           IF      IDG                 >   ZERO
               COMPUTE SPA-MAX-PAGE-41    =   (SPA-GENSAN-ST2-41  -  1)
                                       /    17    +   1
           ELSE
               MOVE    1               TO  SPA-MAX-PAGE-41
           END-IF.
      *
       410181-SRYACCT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次月選択　処理
      *****************************************************************
       41019-JIGETU-HEN-SEC             SECTION.
      *
           MOVE    01                  TO  SPA-GMN-CUR-41.
      *
      *    剤修正のチェック
           IF      SPA-GMN-NUM-41   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      41019-JIGETU-HEN-EXT
           END-IF.
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    "0019"          TO  SPA-ERRCD
               GO      TO      41019-JIGETU-HEN-EXT
           END-IF.
      *
           MOVE    2                   TO  SPA-I4-SEL.
           IF      SPA-UPDFLG-41          =   "1"
               MOVE    "0106"              TO  SPA-IIDCD
           ELSE
               PERFORM 41019-JIGETU-SYORI-SEC
           END-IF.
      *
       41019-JIGETU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次月選択　処理
      *****************************************************************
       41019-JIGETU-SYORI-SEC             SECTION.
      *
      *    現在表示されている内容を更新する
      *    入院会計マスター更新
           IF      SPA-UPDFLG-41          =   "1"
               PERFORM 4901-NYUINACCT-UPD-SEC
           END-IF.
           IF      SPA-ERRCD      NOT  =   SPACE
               GO      TO       41019-JIGETU-SYORI-EXT
           END-IF.
      *
           MOVE    ZERO               TO  FLG-SET.
           PERFORM 41019-JIGETU-COMPUTE-SEC.
      *
           PERFORM UNTIL       FLG-SET   =   1
      *        入院会計マスターより編集をする
               MOVE    SPACE               TO  SPA-ERRCD
               PERFORM 410181-NYUINACCT-HEN-SEC
               IF      SPA-ERRCD          NOT =   SPACE
                   IF     WRK-SRYYM-BIG   >   SPA-NAI-SRYYM-41
                       PERFORM 41019-JIGETU-COMPUTE-SEC
                   ELSE
                       MOVE    1                   TO  FLG-SET
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-SET
               END-IF
           END-PERFORM.
      *
           IF     (SPA-ERRCD          NOT =   SPACE)   OR
                 ((SPA-ERRCD          =       SPACE)  AND
                  (FLG-NYUINKHNRYO    =       ZERO))
               MOVE    ZERO           TO      FLG-KAIKEI-SAKUSEI
               MOVE    SPACE          TO      SPA-ERRCD
               PERFORM   410191-KAIKEI-SAKUSEI-SEC
               IF      FLG-KAIKEI-SAKUSEI    =    1
                    MOVE    SPACE     TO      SPA-ERRCD
                    PERFORM 41019-NEXT-DISP-SEC
               ELSE
      *
                 MOVE    SPA-SAG-SRYYM-41    TO  WRK-SYMD(1:6)
                 MOVE    "01"                TO  WRK-SYMD(7:2)
                 PERFORM 41012-SEIWA-HEN-SEC
                 MOVE    WRK-HENYMDG(1:6)    TO  SPA-I40-SRYYMDW
                 MOVE    WRK-HENYMDG(1:6)    TO  SPA-GMN-SRYYM-41
                 MOVE    SPA-SAG-SRYYM-41    TO  SPA-I40-SRYYMD
                 MOVE    SPA-SAG-SRYYM-41    TO  SPA-NAI-SRYYM-41
                 MOVE    "0028"              TO  SPA-ERRCD
                 IF      SPA-GMN-MAX-41     =   ZERO
                     MOVE    07                  TO  SPA-GMN-CUR-41
                 END-IF
                 GO      TO       41019-JIGETU-SYORI-EXT
               END-IF
           END-IF.
      *
           MOVE    1                  TO  SPA-GMN-CUR-41.
      *
       41019-JIGETU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *
       41019-JIGETU-COMPUTE-SEC            SECTION.
      *
           MOVE    SPA-NAI-SRYYM-41    TO  SPA-SAG-SRYYM-41.
           COMPUTE SPA-NAI-SRYMM-41    =   SPA-NAI-SRYMM-41    +   1.
           IF      SPA-NAI-SRYMM-41    >   12
               MOVE    1                   TO  SPA-NAI-SRYMM-41
               COMPUTE SPA-NAI-SRYYY-41    =   SPA-NAI-SRYYY-41    +   1
           END-IF.
      *
       41019-JIGETU-COMPUTE-EXT.
           EXIT.
      *
      *****************************************************************
      *   会計自動作成分表示処理
      *****************************************************************
       41019-NEXT-DISP-SEC            SECTION.
      *
           MOVE    ZERO               TO  FLG-SET.
      *
           DISPLAY  "SPA-NAI-SRYYM-41 = " SPA-NAI-SRYYM-41
           MOVE    SPA-NAI-SRYYM-41   TO  WRK-SYMD(1:6)
                                          WRK-NAI-SRYYM-41.
           MOVE    "01"               TO  WRK-SYMD(7:2).
           PERFORM 41012-SEIWA-HEN-SEC.
           MOVE    WRK-HENYMDG(1:6)   TO  SPA-GMN-SRYYM-41.
      *
           PERFORM UNTIL       FLG-SET   =   1
      *        入院会計マスターより編集をする
               MOVE    SPACE               TO  SPA-ERRCD
               PERFORM 410181-NYUINACCT-HEN-SEC
               IF      SPA-ERRCD          NOT =   SPACE
                   IF     WRK-SRYYM-BIG   >   SPA-NAI-SRYYM-41
                       PERFORM 41019-JIGETU-COMPUTE-SEC
                   ELSE
                       MOVE    1                   TO  FLG-SET
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-SET
               END-IF
           END-PERFORM.
      *
       41019-NEXT-DISP-EXT.
           EXIT.
      *
      *****************************************************************
      *   会計作成処理
      *****************************************************************
       410191-KAIKEI-SAKUSEI-SEC            SECTION.
      *
      *    画面で開いている当月で退院済みの患者については
      *    入院会計を作成しない
      *
      *    患者入院履歴の読み込み
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPID          TO  PTNYUINRRK-HOSPID
           MOVE    SPA-GMN-PTID        TO  PTNYUINRRK-PTID
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTNYUINRRK-KEY9"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC               =  ZERO
               MOVE    "PTNYUINRRK-KEY9"   TO  ORC-DBPATH
               PERFORM 970-PTNYUINRRK-READ-SEC
               IF      FLG-PTNYUINRRK   =  ZERO
                   IF       PTNYUINRRK-TAIINYMD    =   "99999999"
                            PERFORM  410191-NYUACCT-SAKUSEI-SEC
                            MOVE     1          TO  FLG-KAIKEI-SAKUSEI
                   ELSE
                            MOVE     ZERO       TO  FLG-KAIKEI-SAKUSEI
                   END-IF
               END-IF
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "PTNYUINRRK-KEY9"   TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
      *
       410191-KAIKEI-SAKUSEI-EXT.
           EXIT.
      *
      ***************************************************************** 021118
      *    入院会計テーブル作成処理                                     021118
      ***************************************************************** 021118
       410191-NYUACCT-SAKUSEI-SEC   SECTION.                             021118
      *                                                                 021118
           DISPLAY   "410191-NYUACCT-SAKUSEI-SEC"
           INITIALIZE      ORCSSYOKIKSNAREA
           MOVE    SPA-NAI-SRYYM-41   TO   WRK-NAI-SRYYM-41.
      *
      *    通算日数を求める（入院履歴を検索）
           MOVE    ZERO                TO  WRK-TSUSAN
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPID          TO  PTNYUINRRK-HOSPID
           MOVE    SPA-GMN-PTID        TO  PTNYUINRRK-PTID
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTNYUINRRK-KEY9"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC               =  ZERO
               MOVE    "PTNYUINRRK-KEY9"   TO  ORC-DBPATH
               PERFORM 970-PTNYUINRRK-READ-SEC
               MOVE    PTNYUINRRK-SHONUM
                                       TO  WRK-SHONUM
               MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  WRK-SANTEISTYMD
               MOVE    PTNYUINRRK-TOKUTEI-NYUIN
                                       TO  WRK-TOKUTEI-NYUIN
               MOVE    PTNYUINRRK-REC  TO  WK-PTNYUINRRK-REC
               DISPLAY  "PTNYUINRRK-RRKNUM = " PTNYUINRRK-RRKNUM
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      ****     DISPLAY   "410191-NYUACCT-SAKUSEI-SEC2"
      *
      ****     PERFORM    UNTIL   (FLG-PTNYUINRRK        =   1)   OR
      ****                        (PTNYUINRRK-SHONUM  NOT =  WRK-SHONUM)
      ****         IF      PTNYUINRRK-JTIKBN      NOT =    "5"
      ****             MOVE    PTNYUINRRK-TENNYUYMD
      ****                                         TO  WRK-STYMD
      ****             IF      PTNYUINRRK-TAISYONISSU   =    ZERO
      *                   月末算定
      ****                     INITIALIZE                    STS-AREA-DAY
      ****                     INITIALIZE                    LNK-DAY7-AREA
      ****                     MOVE    "71"              TO  LNK-DAY7-IRAI
      ****                  DISPLAY "WRK-NAI-SRYYM-41 = " WRK-NAI-SRYYM-41
      ****                     IF      WRK-NAI-SRYYM-41-MM    =   1
      ****                             COMPUTE  WRK-NAI-SRYYM-41-YY
      ****                                  =   WRK-NAI-SRYYM-41-YY   -  1
      ****                             MOVE   12   TO  WRK-NAI-SRYYM-41-MM
      ****                     ELSE
      ****                             COMPUTE  WRK-NAI-SRYYM-41-MM
      ****                                  =   WRK-NAI-SRYYM-41-MM   -  1
      ****                     END-IF
      ****                   DISPLAY "GETSUMATSU YYMM = " WRK-NAI-SRYYM-41
      ****                     MOVE    WRK-NAI-SRYYM-41    TO  LNK-DAY7-YM
      ****                     CALL    "ORCSDAY"       USING STS-AREA-DAY
      ****                                                   LNK-DAY7-AREA
      ****                     IF      STS-DAY-RC1         =   ZERO
      ****                         MOVE    LNK-DAY7-KOYOMI   TO  WRK-EDYMD
      ****                     END-IF
      ****                     PERFORM    410191-KIKAN-CALC-SEC
      ****                     ADD      WRK-NISSU2         TO  WRK-TSUSAN
      ****             ELSE
      ****                 ADD    PTNYUINRRK-TAISYONISSU   TO  WRK-TSUSAN
      ****             END-IF
      ****         END-IF
      ****         MOVE    "PTNYUINRRK-KEY9"   TO  ORC-DBPATH
      ****         PERFORM  970-PTNYUINRRK-READ-SEC
      ****     END-PERFORM
      ****     DISPLAY   "410191-NYUACCT-SAKUSEI-SEC3"
      *
      *
      *    通算日数を求める（通算日数取得サブを使用）
           PERFORM 4801-TSUSAN-SANTEI-GET-SEC
           IF    ( SPA-ERRCD    NOT =  SPACE )
               GO  TO  410191-NYUACCT-SAKUSEI-EXT
           END-IF
      *
           MOVE    WK-PTNYUINRRK-REC   TO  PTNYUINRRK-REC
      *
           MOVE    WRK-TSUSAN      TO  SYOKIKSN-IN-TUSANNISSU
           MOVE    SPA-NAI-SRYYM-41
                                   TO  SYOKIKSN-IN-SANTEISTYMD(1:6)
           MOVE    "01"            TO  SYOKIKSN-IN-SANTEISTYMD(7:2)
      *
           IF      WRK-TOKUTEI-NYUIN          NOT =   SPACE
               PERFORM   410191-TOKTEI-NYUIN-SET-SEC
           ELSE
               PERFORM   4801-BTUSEL-SEC
               IF  SYS-5001-BTU-KHNSRYCD      NOT =   SPACE
                   MOVE    SYS-5001-BTU-KHNSRYCD
                                       TO  SYOKIKSN-IN-KHNSRYCD
                   PERFORM   4801-RJNCD-CHG-SEC
                   CALL    "ORCSSYOKIKSN"  USING   ORCSSYOKIKSNAREA
                   IF    ( SYOKIKSN-OT-RC  NOT =   ZERO )
                       MOVE    "2006"      TO  SPA-ERRCD
                       GO  TO  410191-NYUACCT-SAKUSEI-EXT
                   END-IF
               ELSE
                   MOVE    "2006"      TO  SPA-ERRCD
                   GO  TO  410191-NYUACCT-SAKUSEI-EXT
               END-IF
           END-IF
           DISPLAY   "410191-NYUACCT-SAKUSEI-SEC4"
      *
      *    入院会計作成サブの呼出し（処理区分９により次月分のみ作成）
           INITIALIZE            ORCSNYUACCT-AREA
           MOVE    "9"                TO  LNK-SCNYUACCT-SYORI-KBN
           MOVE    "1"                TO  SPA-NYUGAIKBN
           MOVE    PTNYUINRRK-PTID    TO  SPA-PTID
           DISPLAY  "PTNYUINRRK-PTID = " PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-NYUINKA TO  SPA-SRYKA
           MOVE    SYOKIKSN-IN-KHNSRYCD
                           TO  LNK-SCNYUACCT-NYUKHN-SRYCD(1)
      **     MOVE    SYOKIKSN-IN-SANTEISTYMD
      **                     TO   LNK-SCNYUACCT-STSRYYMD
      **     MOVE    SPA-NAI-I01-IDOYMD
      **                     TO   LNK-SCNYUACCT-EDSRYYMD
           MOVE      SYOKIKSN-IN-SANTEISTYMD
                             TO   LNK-SCNYUACCT-STSRYYMD
                                  LNK-SCNYUACCT-EDSRYYMD
           DISPLAY "LNK-SCNYUACCT-STSRYYMD = " LNK-SCNYUACCT-STSRYYMD
           PERFORM   VARYING   IDX6   FROM   1   BY   1
               UNTIL (SYOKIKSN-OT-KGNSRYCD(IDX6)  =  SPACE) OR
                     (IDX6   >     7)
               MOVE   SYOKIKSN-OT-KGNSRYCD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-SRYCD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOSTYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-STYUKYMD(IDX6)
               MOVE   SYOKIKSN-OT-YUKOEDYMD(IDX6)
                         TO  LNK-SCNYUACCT-KAGEN-EDYUKYMD(IDX6)
               DISPLAY "SYOKIKSN-OT-KGNSRYCD(IDX6) = "
                                      SYOKIKSN-OT-KGNSRYCD(IDX6)
               DISPLAY "SYOKIKSN-OT-YUKOSTYMD(IDX6) = "
                                      SYOKIKSN-OT-YUKOSTYMD(IDX6)
               DISPLAY "SYOKIKSN-OT-YUKOEDYMD(IDX6) = "
                                      SYOKIKSN-OT-YUKOEDYMD(IDX6)
               DISPLAY  "IDX6 = " IDX6
           END-PERFORM
      *    通算算定済日数
               MOVE   WRK-TSUSAN    TO  LNK-SCNYUACCT-NYUKHN-NISUU
               DISPLAY  "WRK-TSUSAN = " WRK-TSUSAN
      *    入院料加算診療コード取り込み
           PERFORM    410191-NYUKSN-GET-SEC
      *    特定入院料区分のセット
           IF      WRK-TOKUTEI-NYUIN    =   SPACE
               MOVE     "0"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
           ELSE
               MOVE     "1"     TO  LNK-SCNYUACCT-TOKUTEINYUIN-KBN
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
      *    保険組合せ
           MOVE   "099999914"   TO  LNK-SCNYUACCT-HKNCOMBINUM-SRYCD
           MOVE   PTNYUINRRK-HKNCOMBINUM
                                TO  WRK-HKNCOMBINUM
           MOVE   WRK-HKNCOMBINUM9 
                                TO  LNK-SCNYUACCT-HKNCOMBINUM
           DISPLAY   "LNK-SCNYUACCT-HKNCOMBINUM = "
                                    LNK-SCNYUACCT-HKNCOMBINUM
      *    転科元入院科
           MOVE   "00"          TO  LNK-SCNYUACCT-MOTO-NYUINKA
      **     MOVE   SPA-GMN-I01-NYUINKA-MOT (1 : 2)
      **                          TO  LNK-SCNYUACCT-MOTO-NYUINKA
           MOVE   SPA-SRYKA     TO  WRK-TAIHI-SRYKA
           MOVE   "00"          TO  SPA-SRYKA
      **   
           DISPLAY "ﾆｭｳｲﾝｶｲｹｲ ｻｸｾｲﾏｴ ﾋｮｳｼﾞ"
           DISPLAY  "SPA-HOSPID = " SPA-HOSPID
           DISPLAY  "SPA-NYUGAIKBN = " SPA-NYUGAIKBN
           DISPLAY  "SPA-PTID = " SPA-PTID
           DISPLAY  "SPA-SRYKA = " SPA-SRYKA
           DISPLAY  "LNK-SCNYUACCT-STSRYYMD = " LNK-SCNYUACCT-STSRYYMD
           DISPLAY  "LNK-SCNYUACCT-EDSRYYMD = " LNK-SCNYUACCT-EDSRYYMD
           DISPLAY  "LNK-SCNYUACCT-NYUKHN-SRYCD(1) = " 
                                         LNK-SCNYUACCT-NYUKHN-SRYCD(1)
           DISPLAY  "LNK-SCNYUACCT-KAGEN-SRYCD(1) = "
                                         LNK-SCNYUACCT-KAGEN-SRYCD(1)
           DISPLAY  "LNK-SCNYUACCT-KAGEN-STYUKYMD(1) = "
                                      LNK-SCNYUACCT-KAGEN-STYUKYMD(1)
           DISPLAY  "LNK-SCNYUACCT-KAGEN-EDYUKYMD(1) = "
                                      LNK-SCNYUACCT-KAGEN-EDYUKYMD(1)
           DISPLAY  "LNK-SCNYUACCT-NYUKHN-NISUU = "
                                         LNK-SCNYUACCT-NYUKHN-NISUU
           DISPLAY  "LNK-SCNYUACCT-NYUKHNKSN-SRYCD(1) = "
                                     LNK-SCNYUACCT-NYUKHNKSN-SRYCD(1)
           DISPLAY  "LNK-SCNYUACCT-NYUKHNKSN-SRYCD(2) = "
                                     LNK-SCNYUACCT-NYUKHNKSN-SRYCD(2)
           DISPLAY  "LNK-SCNYUACCT-NYUKHNKSN-SRYCD(3) = "
                                     LNK-SCNYUACCT-NYUKHNKSN-SRYCD(3)
           DISPLAY  "LNK-SCNYUACCT-NYUKHNKSN-SRYCD(4) = "
                                     LNK-SCNYUACCT-NYUKHNKSN-SRYCD(4)
           DISPLAY  "LNK-SCNYUACCT-NYUKHNKSN-SRYCD(5) = "
                                     LNK-SCNYUACCT-NYUKHNKSN-SRYCD(5)
           DISPLAY  "ORCSNYUACCT CALL "
           PERFORM 4801-SENTEI-SET-SEC
           CALL   "ORCSNYUACCT"  USING   SPA-AREA
                                         ORCSNYUACCT-AREA
           MOVE   WRK-TAIHI-SRYKA TO     SPA-SRYKA
           IF     LNK-SCNYUACCT-RC   NOT =  ZERO
               DISPLAY  "ORCGI01 ORCSNYUACCT ERR RC = "
                         LNK-SCNYUACCT-RC
           END-IF
      *
           .
      *                                                                 021118
       410191-NYUACCT-SAKUSEI-EXT.                                       021118
           EXIT.                                                        021118
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
           MOVE    SPA-NAI-SRYYM-41
                                   TO  STUSAN-KJNYMD(1:6)
           MOVE    "01"            TO  STUSAN-KJNYMD(7:2)
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
           IF    ( STUSAN-RC       NOT =   ZERO )
               MOVE    "2003"      TO  SPA-ERRCD
               GO  TO  4801-TSUSAN-SANTEI-GET-EXT
           END-IF
      *
           COMPUTE WRK-TSUSAN  =   STUSAN-NISSU03  -   1
           MOVE    PTNYUINRRK-TENNYUYMD
                                   TO  WRK-SANTEISTYMD
      *
           MOVE    STUSAN-KJNYMD
                                   TO  WRK-YMD
           MOVE    "1"             TO  WRK-ZOGENPTN
           DISPLAY "STUSAN-NISSU01 =   "  STUSAN-NISSU01
           DISPLAY "STUSAN-NISSU02 =   "  STUSAN-NISSU02
           DISPLAY "STUSAN-NISSU03 =   "  STUSAN-NISSU03
           DISPLAY "STUSAN-NISSU04 =   "  STUSAN-NISSU04
           COMPUTE WRK-ZOGEN       =   180 - ( STUSAN-NISSU04 )
           PERFORM 5002-CALENDAR-SEC
           MOVE    WRK-KEISANYMD   TO  WRK-SENTEI-STYMD
      *
           DISPLAY "************************"
           DISPLAY "** TSUSAN-SANTEI      **"
           DISPLAY "WRK-TSUSAN       = " WRK-TSUSAN
           DISPLAY "WRK-SANTEISTYMD  = " WRK-SANTEISTYMD
           DISPLAY "WRK-SENTEI-STYMD = " WRK-SENTEI-STYMD
           DISPLAY "************************"
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
           IF    ( LNK-SCNYUACCT-TOKUTEINYUIN-KBN
                                       =   "1" )
               GO  TO  4801-SENTEI-SET-EXT
           END-IF
      *
           INITIALIZE  SENTEICDCHG-REC
           MOVE    LNK-SCNYUACCT-NYUKHN-SRYCD(1)
                                   TO  SENCHG-IPNSRYCD
           DISPLAY "SENCHG-IPNSRYCD = " SENCHG-IPNSRYCD
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
           MOVE    WRK-SENTEI-STYMD
                                   TO  LNK-SCNYUACCT-SENTEI-STYMD
      *
      *
           DISPLAY "LNK-SCNYUACCT-SENTEI-SRYCD = "
                    LNK-SCNYUACCT-SENTEI-SRYCD
           DISPLAY "LNK-SCNYUACCT-SENTEI-STYMD = "
                    LNK-SCNYUACCT-SENTEI-STYMD
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
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                           LNK-DAY5-AREA
      *
           MOVE    LNK-DAY5-NISSU1     TO  WRK-NISSU1
           MOVE    LNK-DAY5-NISSU2     TO  WRK-NISSU2
           DISPLAY "410191-KIKAN-CALC-SEC WRK-STYMD = " WRK-STYMD
           DISPLAY "410191-KIKAN-CALC-SEC WRK-EDYMD = " WRK-EDYMD
           DISPLAY "410191-KIKAN-CALC-SEC WRK-NISSU2 = " WRK-NISSU2
      *
           .
       410191-KIKAN-CALC-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定入院料セット処理
      *****************************************************************
       410191-TOKTEI-NYUIN-SET-SEC   SECTION.
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
                       MOVE   "190111410"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "11"
                       MOVE   "190066910"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "12"
                       MOVE   "190067010"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "13"
                       MOVE   "190075910"   TO  SYOKIKSN-IN-KHNSRYCD
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
               WHEN    "20"
                       MOVE   "190076710"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "21"
                       MOVE   "190076810"   TO  SYOKIKSN-IN-KHNSRYCD
               WHEN    "22"
                       MOVE   "190799410"   TO  SYOKIKSN-IN-KHNSRYCD
             END-EVALUATE
           END-IF
      *
           .
       410191-BTU-TOKTEI-NYUIN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料加算の取り込み処理
      *****************************************************************
       410191-NYUKSN-GET-SEC         SECTION.
      *
           MOVE   ZERO             TO   IDX8
      *
      *医療機関加算（減算）情報
           INITIALIZE              SYSKANRI-REC
           MOVE    "5000"          TO  SYS-KANRICD
           MOVE    SPA-SYSYMD      TO  ORC-DBYMD
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
               IF    SYS-5001-BTU-HIKSISUKBN   =   "2"
                   ADD      1          TO     IDX8
                   MOVE  "190111370"   TO  WRK-NYUKHNKSN-SRYCD(IDX8)
               END-IF
      *
      *        病棟入院料加算
               PERFORM   VARYING   IDX7   FROM   1  BY  1
                 UNTIL (IDX7                    >   12)
                    IF   IDX7          =       6
      *                 児童思春期精神科入院医療管理加算（２０才未満）
                        IF   SYS-5001-KSNKBN(IDX7)   =   "2"
                             PERFORM   4801-PT-AGE-SEC
                             IF     WRK-NENREI    <      20
                                 ADD    1                TO  IDX8
                                 MOVE   SYS-5001-KSNCD(IDX7)  TO
                                           WRK-NYUKHNKSN-SRYCD(IDX8)
                             END-IF
                        END-IF
                    ELSE
                        IF   IDX7  =  3  OR  4  OR  5
                             CONTINUE
                        ELSE
                            IF   SYS-5001-KSNKBN(IDX7)   =   "2"
                                 ADD    1                TO  IDX8
                                 MOVE   SYS-5001-KSNCD(IDX7)  TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                            END-IF
                        END-IF
                    END-IF
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
      *        療養病棟療養環境加算１
               IF   SYS-5002-BTU-RYOYO-KBN(1)     =   "2"
                    ADD     1       TO     IDX8
                    MOVE   "190106070"  TO
                                   WRK-NYUKHNKSN-SRYCD(IDX8)
               END-IF
      *        療養病棟療養環境加算２
               IF   SYS-5002-BTU-RYOYO-KBN(2)     =   "2"
                    ADD     1       TO     IDX8
                    MOVE   "190106170"  TO
                                   WRK-NYUKHNKSN-SRYCD(IDX8)
               END-IF
      *        療養病棟療養環境加算３
               IF   SYS-5002-BTU-RYOYO-KBN(3)     =   "2"
                    ADD     1       TO     IDX8
                    MOVE   "190106270"  TO
                                   WRK-NYUKHNKSN-SRYCD(IDX8)
               END-IF
           END-IF
      *
      *    入院料と入院料加算の併算定チェック
           MOVE       ZERO       TO     IDX9
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
           ELSE
               GO  TO          4801-NYUKSN-CHK-EXT
           END-IF
      *
           IF    ( NYUKSNCHK-CHKKBN    =   "1" OR "2" )
               CONTINUE
           ELSE
               MOVE   1        TO     FLG-NYUKSN
               GO  TO  4801-NYUKSN-CHK-EXT
           END-IF
      *
           .
      *
       4801-NYUKSN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       48011-TENSU-SEL-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-NYUKSNKBN
      *
           MOVE    ZERO            TO  FLG-TENSU
           INITIALIZE              TNS-TENSU-REC
      *
           MOVE    LNK-SCNYUACCT-NYUKHN-SRYCD(1)
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
               GO  TO  48011-TENSU-SEL-EXT
           END-IF
      *
           MOVE    TNS-NYUKHNKSNKBN
                                   TO  WRK-BTU-NYUKSNKBN
           .
      *
       48011-TENSU-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       48012-TENSU-SEL-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-NYUKSNKBN
      *
           MOVE    ZERO            TO  FLG-TENSU
           INITIALIZE              TNS-TENSU-REC
      *
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
      *****************************************************************
      *    病棟情報検索処理
      *****************************************************************
       4801-BTUSEL-SEC             SECTION.
           DISPLAY  "4801-BTUSEL-SEC"
      *
           INITIALIZE              SYSKANRI-REC
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    PTNYUINRRK-BTUNUM
                                   TO  SYS-KBNCD
           MOVE    SPA-SYSYMD      TO  ORC-DBYMD
           DISPLAY "4801-BTUSEL-SEC PTNYUINRRK-BTUNUM = " 
                                        PTNYUINRRK-BTUNUM
           DISPLAY "4801-BTUSEL-SEC SPA-SYSYMD = " SPA-SYSYMD
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY10"
                                   TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           DISPLAY "4801-BTUSEL-SEC FLG-DBRC = " FLG-DBRC
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
           MOVE    SPA-SYSYMD      TO  ORC-DBYMD
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
      *
      *****************************************************************
      *    入院料一般コードから老人コードへの置き換え処理
      *****************************************************************
       4801-RJNCD-CHG-SEC   SECTION.
      *
      *    入院患者の年齢計算
           PERFORM    4801-PT-AGE-SEC
      *
      *    老人対象患者であれば入院診療コードの置き換え
           IF    ((FLG-HKNCOMBI     =   ZERO)    AND
                  (COMB-KOH1HKNNUM  =   "027"))      OR
                 ((WRK-NENREI      >=    70)     AND
                  (PTINF-BIRTHDAY  <=   "19320930"))
               INITIALIZE              SRYCDCHG-REC
      *
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
      *
           .
           .
       4801-RJNCD-CHG-EXT.
           EXIT.
      *
      *****************************************************************
      *    変更確定　処理
      *****************************************************************
       470-KAKUTEI-SEC             SECTION.
      *
           IF      SPA-GMN-NUM-41       =   ZERO
               GO      TO      470-KAKUTEI-EXT
           END-IF.
      *
           INITIALIZE                      ORCSGDAYAREA.
           MOVE    SPA-GMN-NYUINBI-41(1:6) TO  SGDAY-INDAY.
           MOVE    "1"                     TO  SGDAY-INTYPE.
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA.
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-SDAY(1:6) TO  WRK-NYUINYM
           ELSE
               GO      TO      470-KAKUTEI-EXT
           END-IF.
           INITIALIZE                      ORCSGDAYAREA.
           MOVE    SPA-GMN-SRYYM-41        TO  SGDAY-INDAY.
           MOVE    "1"                     TO  SGDAY-INTYPE.
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA.
           MOVE    SPACE           TO  SPA-IIDCD.
           IF      SGDAY-SDAY(1:6)    <   WRK-NYUINYM
               MOVE    "0110"          TO  SPA-IIDCD
           ELSE
               IF      SGDAY-SDAY(1:6)    =   WRK-NYUINYM
                   MOVE    SPA-GMN-NYUINBI-41(8:2) TO  SNUM-INX
                   CALL    "ORCSNUM"               USING
                                                   ORCSNUMAREA
                   IF     (SNUM-RC         NOT =   ZERO)    OR
                          (SNUM-MINKBN     NOT =   SPACE)   OR
                          (SNUM-SYOKBN     NOT =   SPACE)
                       MOVE    WRK-GMN-DAY-G   TO  SPA-GMN-DAY-G-41
                       GO      TO      470-KAKUTEI-EXT
                   ELSE
                       COMPUTE IDW9   =    SNUM-OUTNUM   -   1
                   END-IF
                   MOVE    1               TO  IDW8
                   MOVE   SPA-GMN-NUM-41   TO  IDX
                   PERFORM UNTIL      IDW8     >   IDW9
                       IF      SPA-GMN-DAY-41 (IDW8) NOT =
                                              SPA-NAI-DAY-41 (IDX IDW8)
                           MOVE    "0110"          TO  SPA-IIDCD
                           MOVE    90              TO  IDW8
                        END-IF
                        ADD     1               TO  IDW8
                   END-PERFORM
               END-IF
           END-IF.
      *
           IF      SPA-IIDCD    =   SPACE
               PERFORM 470-KAKUTEI-2-SEC
           END-IF.
      *
       470-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *
       470-KAKUTEI-2-SEC           SECTION.
      *
           MOVE   SPA-GMN-NUM-41              TO  IDX.
           MOVE   SPA-NAI-TZAISKBKBN-41 (IDX) TO  WRK-CHK-ZAISKBKBN.
           MOVE   SPA-NAI-TBANGO-41 (IDX)     TO  IDG.
           MOVE   SPA-GMN-DAY-G-41        TO  SPA-NAI-DAY-AREA-41 (IDX).
      *
           MOVE   SPA-GMN-ROUJIN-41       TO  SPA-NAI-ROUJIN-41   (IDX).
      *
           MOVE    ZERO                   TO  WRK-ZAIKAISU.
           PERFORM VARYING    IDK      FROM    1   BY  1
                   UNTIL      IDK      >   31
               ADD     SPA-NAI-DAY-41  (IDX IDK)
                                       TO  WRK-ZAIKAISU
               MOVE    SPA-NAI-DAY-41  (IDX IDK)
                                       TO  WRK-ZZ
               MOVE    WRK-ZZ-X        TO  SPA-GMN-TDAY-41 (IDG IDK)
           END-PERFORM.
           MOVE    WRK-ZAIKAISU        TO  SPA-NAI-ZAIKAISU-41 (IDX).
      *
      *    合計回数の再表示
           PERFORM 31013-INPUTCD-HEN-SEC.
           MOVE    SPA-NAI-ENDBANGO-41 (IDX)
                                       TO  IDG. 
           MOVE    WRK-TENSU-X         TO  SPA-GMN-TTENSU-41  (IDG).
      *
           MOVE   ALL SPACE            TO  SPA-WRK-CHK-KBN-TBL.
           PERFORM   VARYING  IDX  FROM  1  BY 1
                     UNTIL    IDX    >   SPA-GMN-MAX-41
               IF     (SPA-GMN-TZAISKBKBN-41 (IDX)    =  "1")  AND
                      (SPA-GMN-TSRYCD-41 (IDX)    NOT =  W-YOYAKU)
                   PERFORM VARYING    IDK      FROM    1   BY  1
                           UNTIL      IDK      >   31
                       IF    SPA-GMN-TDAY-41   (IDX IDK)   NOT =  
                                                               ALL SPACE
                           MOVE    "1"    TO  SPA-WRK-CHK-KBN   (IDK)
                       END-IF
                   END-PERFORM
               END-IF
               IF      SPA-GMN-TZAISKBKBN-41 (IDX)   =  "2"
                   PERFORM VARYING    IDK      FROM    1   BY  1
                           UNTIL      IDK      >   31
                       IF    SPA-GMN-TDAY-41   (IDX IDK)   NOT =  
                                                               ALL SPACE
                           MOVE    "1"    TO  SPA-WRK-CHK-SIKIBETU (IDK)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM.
      *
           IF     WRK-CHK-ZAISKBKBN                 =  "1"
               PERFORM   VARYING  IDX  FROM  1  BY 1
                         UNTIL    IDX    >   SPA-GMN-MAX-41
                   IF      SPA-GMN-TZAISKBKBN-41 (IDX)   NOT =  "1"
                       PERFORM VARYING    IDK      FROM    1   BY  1
                               UNTIL      IDK      >   31
                           IF    SPA-WRK-CHK-KBN   (IDK)   =    SPACE
                               MOVE  SPACE TO  SPA-GMN-TDAY-41 (IDX IDK)
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
               PERFORM   VARYING  IDX  FROM  1  BY 1
                         UNTIL    IDX    >   SPA-GENSAN-ST4-41
                   IF      SPA-NAI-TZAISKBKBN-41 (IDX)   NOT =  "1"
                       PERFORM VARYING    IDK      FROM    1   BY  1
                               UNTIL      IDK      >   31
                           IF    SPA-WRK-CHK-KBN   (IDK)   =    SPACE
                               MOVE  ZERO  TO  SPA-NAI-DAY-41 (IDX IDK)
                           END-IF
                       END-PERFORM
                       MOVE    ZERO              TO  WRK-ZAIKAISU
                       PERFORM VARYING    IDK      FROM    1   BY  1
                               UNTIL      IDK      >   31
                           ADD     SPA-NAI-DAY-41  (IDX IDK)
                                           TO  WRK-ZAIKAISU
                       END-PERFORM
                       MOVE  WRK-ZAIKAISU  TO  SPA-NAI-ZAIKAISU-41 (IDX)
                   END-IF
               END-PERFORM
           END-IF.
      *
           IF     WRK-CHK-ZAISKBKBN                 =  "2"
               PERFORM   VARYING  IDX  FROM  1  BY 1
                         UNTIL    IDX    >   SPA-GMN-MAX-41
                   IF      SPA-GMN-TZAISKBKBN-41 (IDX)   =  "4" OR "6"
                       PERFORM VARYING    IDK      FROM    1   BY  1
                               UNTIL      IDK      >   31
                           IF    SPA-WRK-CHK-SIKIBETU (IDK)   =    "1"
                               MOVE  SPACE TO  SPA-GMN-TDAY-41 (IDX IDK)
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
               PERFORM   VARYING  IDX  FROM  1  BY 1
                         UNTIL    IDX    >   SPA-GENSAN-ST4-41
                   IF      SPA-NAI-TZAISKBKBN-41 (IDX)   =  "4" OR "6"
                       PERFORM VARYING    IDK      FROM    1   BY  1
                               UNTIL      IDK      >   31
                           IF    SPA-WRK-CHK-SIKIBETU (IDK)   =    "1"
                               MOVE  ZERO  TO  SPA-NAI-DAY-41 (IDX IDK)
                           END-IF
                       END-PERFORM
                       MOVE    ZERO              TO  WRK-ZAIKAISU
                       PERFORM VARYING    IDK      FROM    1   BY  1
                               UNTIL      IDK      >   31
                           ADD     SPA-NAI-DAY-41  (IDX IDK)
                                           TO  WRK-ZAIKAISU
                       END-PERFORM
                       MOVE  WRK-ZAIKAISU  TO  SPA-NAI-ZAIKAISU-41 (IDX)
                   END-IF
               END-PERFORM
           END-IF.
      *
      *    変更項目クリア
           INITIALIZE                  SPA-GMN-INPUT-41.
           INITIALIZE                  SPA-NAIGMN-AREA-41.
           MOVE    ZERO                TO  SPA-I4-SYORI.
      *
      *    番号選択へカーソル移動
           MOVE    1                   TO  SPA-GMN-CUR-41.
      *
      *    剤確定あり
           MOVE    "1"                 TO  SPA-UPDFLG-41.
      *
           PERFORM 41013-DAY-CHK2-SEC.
      *
       470-KAKUTEI-2-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名検索へ
      *****************************************************************
       470-SIMEIKEN-SEC             SECTION.
      *
           IF      SPA-GMN-NUM-41   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      470-SIMEIKEN-EXT
           END-IF.
           IF      SPA-UPDFLG-41       =   "1"
               PERFORM 4901-NYUINACCT-UPD-SEC
               IF     (SPA-ERRCD       NOT =   SPACE)   OR
                      (SPA-IIDCD       NOT =   SPACE)
                   GO      TO      470-SIMEIKEN-EXT
               END-IF
           END-IF.
      *
           MOVE    SPA-GMN-SRYKAMEI-I-41 TO  SPA-I40-SRYKAMEI.
           MOVE    SPA-GMN-SRYKA-I-41    TO  SPA-I40-SRYKA.
      *
           MOVE    SPA-I4KYOUTU          TO  SPA-WORK.
           MOVE    SPA-AREA              TO  SPA-COMMON.
      *
           MOVE    SPACE                 TO  LINKAREA.
           MOVE    SPA-KYOUTU            TO  LNK-COMMON.
           MOVE    SPA-I4                TO  LNK-SCRSPA.
      *
      *    画面移行用のパラメタ変更
           MOVE    "I41"                 TO  SPA-MOTOPG.
           MOVE    "P97"                 TO  SPA-SAKIPG.
      *
      *    氏名をクリアする
           MOVE    SPACE                 TO  SPA-NAME.
           MOVE    SPA-KYOUTU            TO  LNK-KYOUTU.
      *
           MOVE    "CHANGE"              TO  MCP-PUTTYPE.
           MOVE    "P97"                 TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                     TO  FLG-END.
      *
       470-SIMEIKEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約登録へ
      *****************************************************************
       480-YOYAKU-SEC             SECTION.
      *
      *    剤修正のチェック
           IF      SPA-GMN-NUM-41   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      480-YOYAKU-EXT
           END-IF.
           IF      SPA-UPDFLG-41       =   "1"
               PERFORM 4901-NYUINACCT-UPD-SEC
               IF     (SPA-ERRCD       NOT =   SPACE)   OR
                      (SPA-IIDCD       NOT =   SPACE)
                   GO      TO      480-YOYAKU-EXT
               END-IF
           END-IF.
      *
           MOVE    SPA-GMN-PTID           TO  SPA-PTID.
           MOVE    SPA-GMN-PTNUM          TO  SPA-PTNUM.
           MOVE    SPA-GMN-NAME           TO  SPA-NAME.
           MOVE    SPA-GMN-SEX            TO  SPA-SEX.
           MOVE    SPA-GMN-BIRTHDAYW      TO  SPA-BIRTHDAYW.
           MOVE    SPA-GMN-SRYKA-I-41     TO  SPA-I40-SRYKA.
           MOVE    SPA-GMN-SRYKAMEI-I-41  TO  SPA-I40-SRYKAMEI.
      *
           MOVE    "I41"                  TO  SPA-MOTOPG.
           MOVE    "I41"                  TO  SPA-MOTOPG3.
           MOVE    "Y01"                  TO  SPA-SAKIPG.
      *
           MOVE   "CHANGE"                TO  MCP-PUTTYPE.
           MOVE   "Y01"                   TO  MCP-WINDOW.
      *
           MOVE    SPA-I4KYOUTU           TO  SPA-WORK.
           MOVE    SPA-AREA               TO  SPA-COMMON.
      *
           MOVE    SPACE                  TO  LINKAREA.
           MOVE    SPA-KYOUTU             TO  LNK-COMMON.
           MOVE    SPA-KYOUTU             TO  LNK-KYOUTU.
           MOVE    SPA-I4                 TO  LNK-SCRSPA.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                      TO  FLG-END.
      *
       480-YOYAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付一覧へ
      *****************************************************************
       490-UKEICHI-SEC             SECTION.
      *
           IF      SPA-GMN-NUM-41   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      490-UKEICHI-EXT
           END-IF.
           IF      SPA-UPDFLG-41       =   "1"
               PERFORM 4901-NYUINACCT-UPD-SEC
               IF     (SPA-ERRCD       NOT =   SPACE)   OR
                      (SPA-IIDCD       NOT =   SPACE)
                   GO      TO      490-UKEICHI-EXT
               END-IF
           END-IF.
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPA-GMN-SRYKA-I-41     TO  SPA-I40-SRYKA.
           MOVE    SPA-GMN-SRYKAMEI-I-41  TO  SPA-I40-SRYKAMEI.
           MOVE    SPA-I4KYOUTU           TO  SPA-WORK.
           MOVE    SPA-AREA               TO  SPA-COMMON.
      *
           MOVE    SPACE                  TO  LINKAREA.
           MOVE    SPA-KYOUTU             TO  LNK-COMMON.
           MOVE    SPA-I4                 TO  LNK-SCRSPA.
      *
      *    画面移行用のパラメタ変更
           MOVE    "I41"                  TO  SPA-MOTOPG.
           MOVE    "U01"                  TO  SPA-SAKIPG.
           MOVE    SPA-KYOUTU             TO  LNK-KYOUTU.
      *
           MOVE   "CHANGE"                TO  MCP-PUTTYPE.
           MOVE   "U01    "               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                      TO  FLG-END.
      *
       490-UKEICHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       490-TOROKU-SEC              SECTION.
      *
      *    更新前のチェック
           IF      SPA-GMN-NUM-41   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      490-TOROKU-EXT
           END-IF.
      *
      *    入院会計マスター更新
           PERFORM 4901-NYUINACCT-UPD-SEC.
           IF     (SPA-ERRCD       NOT =   SPACE)   OR
                  (SPA-IIDCD       NOT =   SPACE)
               GO      TO      490-TOROKU-EXT
           END-IF.
      *
           PERFORM 490-TOROKU-CRE-SEC.
      *
       490-TOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *
       490-TOROKU-CRE-SEC                SECTION.
      *
      *    患者が選択されてきた時は、元の画面へ
           IF      SPA-I40-MOTOFLG     =   "0"
      *    前回患者へ待避
               IF      SPA-GMN-PTNUM    NOT =   SPACE
                   MOVE    SPA-GMN-PTNUM        TO  SPA-LAST-PTNUM
                   MOVE    SPA-GMN-PTID         TO  SPA-LAST-PTID
               END-IF
      *        クリア
               MOVE    ZERO                 TO  SPA-GMN-CUR-41
               INITIALIZE                   SPA-I4-AREA
               INITIALIZE                   SPA-SCR-REC-41
               INITIALIZE                   SPA-5000-GENSAN-DSP-TBL-41
               INITIALIZE                   SPA-5000-GENSAN-CHK-TBL-41
               INITIALIZE                   SPA-I40-SET
               INITIALIZE                   SPA-I41-AREA-41
               MOVE    SPA-I40-SRYYMDW(1:6) TO  SPA-GMN-SRYYM-41
               MOVE    SPA-I40-SRYYMD (1:6) TO  SPA-NAI-SRYYM-41
               MOVE    SPA-I40-SRYYMD (1:6) TO  SPA-SAG-SRYYM-41
               MOVE    SPACE                TO  SPA-GMN-SRYKA-IG-41
               MOVE    "00"                 TO  SPA-GMN-SRYKA-I-41
               MOVE    "全科指定"           TO  SPA-GMN-SRYKAMEI-I-41
      *        排他制御解除処理
               PERFORM 990-LOCK-OUT-SEC
           ELSE
      *        更新後、戻るへ
               PERFORM 210-BACK
           END-IF.
      *
       490-TOROKU-CRE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター更新処理
      *****************************************************************
       4901-NYUINACCT-UPD-SEC            SECTION.
      *
      *    対象がない時、処理なし
           IF     (SPA-UPDFLG-41          =   SPACE)   OR
                  (SPA-GENSAN-ST4-41      =   ZERO)
               GO      TO      4901-NYUINACCT-UPD-EXT
           END-IF.
      *
           MOVE    SPACE               TO  SPA-UPDFLG-41.
      *
      *    入院会計マスターを編集
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GENSAN-ST4-41
               INITIALIZE                          NYUINACCT-REC
               MOVE    SPA-HOSPID                  TO  NACCT-HOSPID
               MOVE    SPA-NAI-TNYUGAIKBN-41 (IDX) TO  NACCT-NYUGAIKBN
               MOVE    SPA-GMN-PTID                TO  NACCT-PTID
               MOVE    SPA-NAI-TSRYKA-41 (IDX)     TO  NACCT-SRYKA
               MOVE    SPA-NAI-SRYYM-41            TO  NACCT-SRYYM
               MOVE    SPA-NAI-SRYKBN-41 (IDX)     TO  NACCT-SRYKBN
               MOVE    SPA-NAI-ZAINUM-41 (IDX)     TO  NACCT-ZAINUM
      *
               MOVE    NYUINACCT-REC            TO  MCPDATA-REC
               MOVE    "DBSELECT"               TO  MCP-FUNC
               MOVE    "NYUINACCT-KEY"          TO  ORC-DBPATH
               CALL    "ORCMCPSUB"              USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    "NYUINACCT-KEY"     TO  ORC-DBPATH
                   PERFORM 900-NYUINACCT-READ-SEC
               ELSE
                   INITIALIZE                      NYUINACCT-REC
                   MOVE    1                   TO  FLG-NYUINACCT
               END-IF
               IF      FLG-NYUINACCT        =   ZERO
      *            入院会計マスターを変更する
                   PERFORM 4901-NYUINACCT-KOUSIN-SEC
      *
                   MOVE    NYUINACCT-REC       TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "NYUINACCT-UPD1"    TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"         USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
                   IF      MCP-RC          NOT =   ZERO
                        DISPLAY "I41 NYUINACCT UPD ERR01:" MCP-RC
                        MOVE    "2000"          TO  SPA-ERRCD
                        GO  TO  4901-NYUINACCT-UPD-EXT
                   END-IF
               ELSE
                   DISPLAY "I41 NYUINACCT READ ERR01:" NACCT-KEY
                   GO  TO  4901-NYUINACCT-UPD-EXT
               END-IF
           END-PERFORM.
      *
           MOVE    SPA-NAI-SRYYM-41          TO  SPA-WRK-KOMENTO-SRYYM.
      *
           IF     (SPA-WRK-KOMENTO-SRYCD   NOT =   SPACE)   AND
                  (SPA-WRK-KOMENTO-E           =   99)
               MOVE    ZERO           TO  FLG-NYUINACT
               PERFORM 4901-NYUINACCT-UPD2-SEC
                       UNTIL      FLG-NYUINACT   =   1
           END-IF.
      *
       4901-NYUINACCT-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター更新処理（一括修正で終了日が９９の時）
      *****************************************************************
       4901-NYUINACCT-UPD2-SEC           SECTION.
      *
           COMPUTE SPA-WRK-KOMENTO-SRYM  =  SPA-WRK-KOMENTO-SRYM  +  1.
           IF      SPA-WRK-KOMENTO-SRYM    >   12
               MOVE    1                   TO  SPA-WRK-KOMENTO-SRYM
               COMPUTE SPA-WRK-KOMENTO-SRYY = SPA-WRK-KOMENTO-SRYY + 1
           END-IF.
           PERFORM 41013-DAY-CHK2-SEC.
           IF     SPA-ERRCD        NOT =   SPACE
               MOVE    1               TO  FLG-NYUINACT
               GO      TO      4901-NYUINACCT-UPD2-EXT
           END-IF.
           INITIALIZE                          STS-AREA-DAY.
           INITIALIZE                          LNK-DAY7-AREA.
           MOVE    "71"                    TO  LNK-DAY7-IRAI.
           MOVE    SPA-WRK-KOMENTO-SRYYM   TO  LNK-DAY7-YM.
           CALL    "ORCSDAY"               USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA.
           MOVE    LNK-DAY7-KOYOMI(7:2)    TO  SNUM-INX.
           CALL    "ORCSNUM"               USING   ORCSNUMAREA.
           IF     (SNUM-RC         NOT =   ZERO)    OR
                  (SNUM-MINKBN     NOT =   SPACE)   OR
                  (SNUM-SYOKBN     NOT =   SPACE)
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    1               TO  FLG-NYUINACT
               GO      TO      4901-NYUINACCT-UPD2-EXT
           ELSE
               MOVE    SNUM-OUTNUM     TO  IDV
           END-IF.
      *
      *    入院会計マスター読み込み
           INITIALIZE                          WRK-CHK-TBL.
           INITIALIZE                          NYUINACCT-REC.
           MOVE    ZERO                    TO  FLG-UPD.
           MOVE    SPA-HOSPID              TO  NACCT-HOSPID.
           MOVE    SPA-GMN-PTID            TO  NACCT-PTID.
           MOVE    SPA-WRK-KOMENTO-SRYYM   TO  NACCT-SRYYM.
      *
           MOVE    NYUINACCT-REC           TO  MCPDATA-REC.
           MOVE    "DBSELECT"              TO  MCP-FUNC.
           MOVE    "NYUINACCT-KEY14"       TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "NYUINACCT-KEY14"   TO  ORC-DBPATH
               PERFORM 900-NYUINACCT-READ-SEC
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF.
      *
           PERFORM UNTIL       FLG-NYUINACCT   =   1
               IF    (NACCT-NYUGAIKBN    = "1")  AND
                     (NACCT-SRYKA        = SPA-GMN-SRYKA-I-41)  AND
                     (NACCT-ZAISKBKBN    = "1")
                   MOVE    1                   TO  FLG-UPD
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   31
                       IF    NACCT-DAY (IDY)   NOT =   ZERO
                           MOVE    NACCT-DAY (IDY)
                                                 TO  WRK-CHK-DAY (IDY)
                       END-IF
                   END-PERFORM
               END-IF
      *
               MOVE    "NYUINACCT-KEY14"   TO  ORC-DBPATH
               PERFORM 900-NYUINACCT-READ-SEC
           END-PERFORM.
      *
           IF    FLG-UPD   =   ZERO
               MOVE    1               TO  FLG-NYUINACT
               GO      TO      4901-NYUINACCT-UPD2-EXT
           END-IF.
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   31
               IF    WRK-CHK-DAY (IDY)   =   ZERO
                   MOVE    SPACE         TO  SPA-WRK-CHK-KBN (IDY)
               END-IF
           END-PERFORM.
           MOVE    SPACE         TO  SPA-IIDCD.
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   IDV
               IF    SPA-WRK-CHK-KBN (IDY)   =   SPACE
                   MOVE    "0109"        TO  SPA-IIDCD
                   MOVE    1             TO  FLG-NYUINACT
               END-IF
           END-PERFORM.
           IF    SPA-IIDCD   =   SPACE
               MOVE    "OK"          TO  SPA-I4ID-FLG
               PERFORM 4901-NYUINACCT-UPD3-SEC
           END-IF.
      *
       4901-NYUINACCT-UPD2-EXT.
           EXIT.
      *
      *****************************************************************
      *
       4901-NYUINACCT-UPD3-SEC           SECTION.
      *
      *    入院行為マスター読み込み
           INITIALIZE                          NYUINACT-REC.
           MOVE    SPA-HOSPID              TO  NSRY-HOSPID.
           MOVE    "1"                     TO  NSRY-NYUGAIKBN.
           MOVE    SPA-GMN-PTID            TO  NSRY-PTID.
           MOVE    SPA-WRK-KOMENTO-SRYYM   TO  NSRY-SRYYM.
           MOVE    SPA-WRK-KOMENTO-SRYCD   TO  NSRY-SRYCD (1).
           MOVE    SPA-WRK-KOMENTO-SRYCD   TO  NSRY-SRYCD (2).
           MOVE    SPA-WRK-KOMENTO-SRYCD   TO  NSRY-SRYCD (3).
           MOVE    SPA-WRK-KOMENTO-SRYCD   TO  NSRY-SRYCD (4).
           MOVE    SPA-WRK-KOMENTO-SRYCD   TO  NSRY-SRYCD (5).
      *
           MOVE    NYUINACT-REC            TO  MCPDATA-REC.
           MOVE    "DBSELECT"              TO  MCP-FUNC.
           MOVE    "NYUINACT-KEY7"         TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "NYUINACT-KEY7"     TO  ORC-DBPATH
               PERFORM 930-NYUINACT-RAED-SEC
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1               TO  FLG-NYUINACT
           END-IF.
      *
           PERFORM UNTIL       FLG-NYUINACT    =   1
               INITIALIZE                       NYUINACCT-REC
               MOVE    NSRY-HOSPID              TO  NACCT-HOSPID
               MOVE    NSRY-NYUGAIKBN           TO  NACCT-NYUGAIKBN
               MOVE    NSRY-PTID                TO  NACCT-PTID
               MOVE    NSRY-SRYKA               TO  NACCT-SRYKA
               MOVE    NSRY-SRYYM               TO  NACCT-SRYYM
               MOVE    NSRY-SRYKBN              TO  NACCT-SRYKBN
               MOVE    NSRY-ZAINUM              TO  NACCT-ZAINUM
      *
               MOVE    NYUINACCT-REC            TO  MCPDATA-REC
               MOVE    "DBSELECT"               TO  MCP-FUNC
               MOVE    "NYUINACCT-KEY"          TO  ORC-DBPATH
               CALL    "ORCMCPSUB"              USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    "NYUINACCT-KEY"     TO  ORC-DBPATH
                   PERFORM 900-NYUINACCT-READ-SEC
               ELSE
                   INITIALIZE                      NYUINACCT-REC
                   MOVE    1                   TO  FLG-NYUINACCT
               END-IF
               IF      FLG-NYUINACCT        =   ZERO
      *            入院会計マスターを変更する
                   MOVE    ZERO                TO  NACCT-ZAIKAISU
                   PERFORM   VARYING  IDX  FROM  1  BY  1
                             UNTIL    IDX    >   IDV
                       IF    SPA-I4ID-FLG   =   "OK"
                           MOVE    SPA-WRK-KOMENTO-D
                                                  TO  NACCT-DAY  (IDX)
                           ADD     SPA-WRK-KOMENTO-D
                                                  TO  NACCT-ZAIKAISU
                       ELSE
                           IF    SPA-WRK-CHK-KBN (IDX)   =   SPACE
                               MOVE    ZERO         TO  NACCT-DAY  (IDX)
                           ELSE
                               MOVE    SPA-WRK-KOMENTO-D
                                                    TO  NACCT-DAY (IDX)
                               ADD     SPA-WRK-KOMENTO-D
                                                    TO  NACCT-ZAIKAISU
                           END-IF
                       END-IF
                   END-PERFORM
      *
                   MOVE    NYUINACCT-REC       TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "NYUINACCT-UPD1"    TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"         USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
                   IF      MCP-RC          NOT =   ZERO
                        MOVE    1               TO  FLG-NYUINACT
                        DISPLAY "I41 NYUINACCT UPD ERR02:" MCP-RC
                        MOVE    "2000"          TO  SPA-ERRCD
                        GO  TO  4901-NYUINACCT-UPD3-EXT
                   END-IF
               ELSE
                   DISPLAY "I41 NYUINACCT READ ERR02:" NACCT-KEY
                   GO  TO  4901-NYUINACCT-UPD3-EXT
               END-IF
      *
               MOVE    "NYUINACT-KEY7"     TO  ORC-DBPATH
               PERFORM 930-NYUINACT-RAED-SEC
           END-PERFORM.
      *
       4901-NYUINACCT-UPD3-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター  変更処理
      *****************************************************************
       4901-NYUINACCT-KOUSIN-SEC         SECTION.
      *
      *    保険組合せ
           MOVE    SPA-NAI-HKNCOMBI-41 (IDX) TO  NACCT-HKNCOMBI.
      *    剤回数
           MOVE    SPA-NAI-ZAIKAISU-41 (IDX) TO  NACCT-ZAIKAISU.
      *    回数テーブル（１日から３１日）
           MOVE    SPA-NAI-DAY-AREA-41 (IDX) TO  NACCT-DAY-AREA.
      *
       4901-NYUINACCT-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       5101-HKNCOMBI-SET-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBIMEI.
      *
           STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBIMEI
           END-STRING.
      *    主保険がない時
           IF      COMB-SYU-TANSEIDONAME   =   SPACE
               MOVE    SPACE               TO  WRK-HKNCOMBIMEI
               STRING  COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBIMEI
               END-STRING
           END-IF.
      *
      *    労災の時、コメントの追加
           IF      COMB-HKNNUM         =   "971"  OR  "973"
      *        患者保険情報検索
               INITIALIZE                      PTRSIINF-REC
               MOVE    SPA-HOSPID          TO  PTRSI-HOSPID
               MOVE    SPA-GMN-PTID        TO  PTRSI-PTID
               MOVE    COMB-HKNID          TO  PTRSI-HKNID
               MOVE    PTRSIINF-REC        TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "PTRSIINF-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    "PTRSIINF-KEY"     TO  ORC-DBPATH
                   PERFORM 993-PTRSIINF-NEXT-SEC
               ELSE
                   INITIALIZE                      PTRSIINF-REC
                   MOVE    1                   TO  FLG-PTRSIINF
               END-IF
               MOVE    SPACE               TO  WRK-HKNKBN-MEI
               EVALUATE    PTRSI-HKNKBN
                   WHEN    "1"
                       MOVE    "短"        TO  WRK-HKNKBN-MEI
                   WHEN    "2"
                       MOVE    "傷"        TO  WRK-HKNKBN-MEI
                   WHEN    "3"
                       MOVE    "ア"        TO  WRK-HKNKBN-MEI
               END-EVALUATE
               IF      PTRSI-COMMENT       =   SPACE
                   MOVE    SPACE               TO  WRK-HKNCOMBIMEI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           INTO              WRK-HKNCOMBIMEI
                   END-STRING
               ELSE
                   MOVE    SPACE               TO  WRK-HKNCOMBIMEI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           "（"                    DELIMITED  BY  SIZE
                           PTRSI-COMMENT           DELIMITED  BY  SPACE
                           "）"                    DELIMITED  BY  SIZE
                           INTO              WRK-HKNCOMBIMEI
                   END-STRING
               END-IF
           END-IF.
      *
       5101-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF.
      *
           IF      SPA-IIDCD       NOT =   SPACE
               PERFORM 520-JIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF.
      *
           MOVE    SPACE               TO  LINKAREA.
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "I41    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG.
           MOVE    SPACE               TO  WRK-ERRMSG.
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "剤番号入力エラー"
                                       TO  WRK-ERRMSG
               WHEN    "0002"
                   MOVE    "剤の変更番号がありません"
                                       TO  WRK-ERRMSG
               WHEN    "0003"
                   MOVE    "診療回数は数字で入力して下さい"
                                       TO  WRK-ERRMSG
               WHEN    "0004"
                   MOVE    "受診履歴がない日です。入力できません"
                                       TO  WRK-ERRMSG
               WHEN    "0005"
                   MOVE    "受診履歴番号がありません。"
                                       TO  WRK-ERRMSG
               WHEN    "0006"
                   MOVE    "受診履歴の選択できない処理です。"
                                       TO  WRK-ERRMSG
               WHEN    "0007"
                   MOVE
                   "変更後診療日が暦日エラーです"
                                       TO  WRK-ERRMSG
               WHEN    "0008"
                   MOVE
                   "変更後診療日は診療月内での変更のみでできます"
                                       TO  WRK-ERRMSG
               WHEN    "0009"
                   MOVE
                   "剤が修正中です。変更確定かクリアをして下さい。"
                                       TO  WRK-ERRMSG
               WHEN    "0010"
                   MOVE    "入院科履歴更新エラー" 
                                       TO  WRK-ERRMSG
               WHEN    "0011"
                   MOVE    "既に受診のある日です。変更できません。"
                                       TO  WRK-ERRMSG
               WHEN    "0012"
                   MOVE
               "受診日変更処理で受診歴確定時のみ入力できます。"
                                       TO  WRK-ERRMSG
               WHEN    "0013"
                   MOVE    "剤の変更番号を入力して下さい"
                                       TO  WRK-ERRMSG
               WHEN    "0014"
                   MOVE    "パラメタ出力エラー"
                                       TO  WRK-ERRMSG
               WHEN    "0015"
                   MOVE    "誕生日前の診療日は入力できません"
                                       TO  WRK-ERRMSG
               WHEN    "0016"
                   MOVE    "ＩＤ取得サブでエラー" 
                                       TO  WRK-ERRMSG
               WHEN    "0017"
                   MOVE    "該当の患者が存在しません。"
                                       TO  WRK-ERRMSG
               WHEN    "0018"
                   MOVE    "診療年月入力エラー"
                                       TO  WRK-ERRMSG
               WHEN    "0019"
                   MOVE    "患者番号を入力して下さい"
                                       TO  WRK-ERRMSG
               WHEN    "0020"
                   MOVE    "対象の入院年月はありません。"
                                       TO  WRK-ERRMSG
               WHEN    "0021"
                   MOVE    "対象の剤は労災の外来管理加算読み替え分です"
                                       TO  WRK-ERRMSG
                   MOVE    "。剤変更はできません。"
                                       TO  WRK-ERRMSG(43:)
               WHEN    "0022"
                   MOVE    "数字とスラッシュと"
                                       TO  WRK-ERRMSG(1:18)
                   MOVE    "ハイフンで入力して下さい。"
                                       TO  WRK-ERRMSG(19:)
               WHEN    "0023"
                   MOVE    "全科指定では指定できません。"
                                       TO  WRK-ERRMSG
               WHEN    "0024"
                   MOVE    "回数の関連誤りのため変更できません。"
                                       TO  WRK-ERRMSG
               WHEN    "0025"
                   MOVE    "全科指定では変更できません。"
                                       TO  WRK-ERRMSG
               WHEN    "0026"
                   MOVE    "入院基本料の設定がないため変更できません。"
                                       TO  WRK-ERRMSG
               WHEN    "0027"
                   MOVE    "前月データはありません。"
                                       TO  WRK-ERRMSG
               WHEN    "0028"
                   MOVE    "次月データはありません。"
                                       TO  WRK-ERRMSG
               WHEN    "0029"
                   MOVE    "剤識別区分が入院料以外の指定はできません。"
                                       TO  WRK-ERRMSG
               WHEN    "0030"
                   MOVE    "施設基準ではありません。算定できません"
                                       TO  WRK-ERRMSG
               WHEN    "0031"
                   MOVE    "指定された科のデータはありません。"
                                       TO  WRK-ERRMSG
               WHEN    "1005"
                   MOVE    "受診履歴番号が存在しません"
                                       TO  WRK-ERRMSG
               WHEN    "9999"
                   MOVE    "他端末で使用中です。"
                                       TO  WRK-ERRMSG
                   MOVE    SLOCK-MSG   TO  WRK-ERRMSG(21:)
           END-EVALUATE.
      *
           MOVE    SPACE               TO  I4ERR.
           MOVE    SPA-ERRCD           TO  I4ERR-ERRCODE.
           MOVE    WRK-ERRMSG          TO  I4ERR-ERRMSG.
           MOVE    "B01"               TO  MCP-WIDGET.
      *
           MOVE    "I41"               TO  SPA-MOTOPG.
           MOVE    "I4ERR"             TO  SPA-SAKIPG.
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "I4ERR"             TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-IIDCD
               WHEN    "0102"
               MOVE
           "剤が修正されています。ＯＫで修正分は反映されずに終了します"
                                       TO  WRK-JIDMSG
               WHEN    "0103"
               MOVE
               "剤選択されました。現在選択中の内容はクリアされます。"
                                       TO  WRK-JIDMSG
               WHEN    "0104"
                   MOVE    "現在までの修正分を登録後、受診履歴の選択"
                                       TO  WRK-JIDMSG1
                   MOVE    "で当日の剤内容を追加修正します"
                                       TO  WRK-JIDMSG2
               WHEN    "0106"
               MOVE
           "診療年月が変更されました。現在までの修正分を登録します。"
                                       TO  WRK-JIDMSG
               WHEN    "0107"
                   MOVE    "入院科が変更されました。現在までの修正分"
                                       TO  WRK-JIDMSG1
                   MOVE    "を登録します。"
                                       TO  WRK-JIDMSG2
               WHEN    "0108"
                   MOVE    "診療年月が変更されました。現在までの修正"
                                       TO  WRK-JIDMSG1
                   MOVE    "分を登録します。"
                                       TO  WRK-JIDMSG2
               WHEN    "0109"
                   MOVE    "入院基本料が未設定の項目があります。未設"
                                       TO  WRK-JIDMSG1
                   MOVE    "定部分も登録します。"
                                       TO  WRK-JIDMSG2
               WHEN    "0110"
                   MOVE    "最新でない入院履歴を更新することになりま"
                                       TO  WRK-JIDMSG1
                   MOVE    "す、よろしいですか。"
                                       TO  WRK-JIDMSG2
               WHEN    OTHER
                   MOVE    SPA-ERRCD
                                       TO  WRK-JIDMSG
           END-EVALUATE.
      *
           MOVE    SPACE               TO  SPA-I4ID-FLG.
      *
           MOVE    SPACE               TO  I4ID1.
           MOVE    SPA-IIDCD           TO  I4ID1-ID1CODE.
           MOVE    WRK-JIDMSG          TO  I4ID1-ID1MSG.
           MOVE    "B12"               TO  MCP-WIDGET.
      *
           MOVE    "I41"               TO  SPA-MOTOPG.
           MOVE    "I4ID1"             TO  SPA-SAKIPG.
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "I4ID1"             TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       520-JIDSET-EXT.
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
      *
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
      *
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
      *
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
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-KANRIMST-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF.
      *
       900-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    ZERO                TO  TNS-YUKOSTYMD.
           MOVE    ALL "9"             TO  TNS-YUKOEDYMD.
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "TENSU-KEY"         TO  ORC-DBPATH.
           MOVE    WRK-SRYYMD          TO  ORC-DBYMD.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "TENSU-KEY"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF.
      *
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "PTINF-KEY"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF.
      *
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター次読込
      *****************************************************************
       900-NYUINACCT-READ-SEC       SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
               MOVE    ZERO                TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                      NYUINACCT-REC
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF.
      *
       900-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター次読込
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF.
      *
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院行為マスター読込
      *****************************************************************
       930-NYUINACT-RAED-SEC       SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACT-REC
               MOVE    ZERO                TO  FLG-NYUINACT
           ELSE
               INITIALIZE                      NYUINACT-REC
               MOVE    1                   TO  FLG-NYUINACT
           END-IF.
      *
       930-NYUINACT-RAED-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       930-SRYACT-RAED-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF.
      *
       930-SRYACT-RAED-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       940-HKNCOMBI-NEXT-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               MOVE    ZERO                TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                      HKNCOMBI-REC
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF.
      *
       940-HKNCOMBI-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントマスタ読み込み
      *****************************************************************
       950-PTCOM-READ-SEC              SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "PTCOM-KEY"         TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "PTCOM-KEY"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTCOM-REC
                   MOVE    ZERO                TO  FLG-PTCOM
               ELSE
                   INITIALIZE                  PTCOM-REC
                   MOVE    1                   TO  FLG-PTCOM
               END-IF
           ELSE
               INITIALIZE                  PTCOM-REC
               MOVE    1                   TO  FLG-PTCOM
           END-IF.
      *
       950-KNJCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴マスター読込
      *****************************************************************
       970-PTNYUINRRK-READ-SEC     SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
               MOVE    ZERO            TO  FLG-PTNYUINRRK
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF.
      *
       970-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスター読込
      *****************************************************************
       930-INPUTCD-READ-SEC         SECTION.
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "INPUTCD-KEY"       TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "INPUTCD-KEY"       TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  INPUTCD-REC
                   MOVE    ZERO                TO  FLG-INPUTCD
               ELSE
                   INITIALIZE                  INPUTCD-REC
                   MOVE    1                   TO  FLG-INPUTCD
               END-IF
           ELSE
               INITIALIZE                  INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF.
      *
       930-INPUTCD-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報読み込み
      *****************************************************************
       993-PTRSIINF-NEXT-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               MOVE    ZERO            TO  FLG-PTRSIINF
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1               TO  FLG-PTRSIINF
           END-IF.
      *
       993-PTRSIINF-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       980-SYUNOU-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF.
      *
       980-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC             SECTION.
      *
      *    排他制御処理
           MOVE    1                   TO  SLOCK-MODE.
           MOVE    SPA-GMN-PTID        TO  SLOCK-PTID.
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA.
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
           END-IF.
      *
       990-LOCK-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理
      *****************************************************************
       990-LOCK-OUT-SEC            SECTION.
      *
      *    排他制御解除処理
           MOVE    2                   TO  SLOCK-MODE.
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA.
      *
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
       900-PUT-WINDOW-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-TBL-CLOSE-SEC               SECTION.
      *
           MOVE    WRK-DBPATH      TO  ORC-DBPATH.
           PERFORM 910-DB-CLOSE-SEC.
      *
       900-TBL-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DB-CLOSE-SEC                SECTION.
      *
           MOVE    "DBCLOSE"            TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
       910-DB-CLOSE-EXT.
           EXIT.
      *
