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
      ******************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGI44.
      ******************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院会計照会
      *  コンポーネント名  : 診療行為入力（Ｉ４４）
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
      *    ＳＰＡパラメタ
       01  SPA-SCR-REC.
           COPY    "I4SCR-NYU.INC".
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
           COPY    "I4SCR-SPA4".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
           03  STS-SRYKBN          PIC X(02).
           03  STS-SPASCR          PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SPASCR          PIC 9(01).
           03  FLG-SRYKBN          PIC 9(01).
           03  FLG-KNJBYO          PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-NYUINACT        PIC 9(01).
           03  FLG-NYUINACCT       PIC 9(01).
           03  FLG-CHKMST          PIC 9(01).
           03  FLG-INPUTSET        PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SRYCDCHG        PIC 9(01).
      *
           03  FLG-GMN             PIC 9(01).
           03  FLG-KANJI           PIC 9(01).
           03  FLG-KANA            PIC 9(01).
           03  FLG-SUJI            PIC 9(01).
           03  FLG-TOROKU          PIC X(01).
           03  FLG-SST             PIC 9(01).
           03  FLG-KINKI           PIC 9(01).
      *
           03  FLG-NAIYAK          PIC X(01).
           03  FLG-TONPUK          PIC X(01).
           03  FLG-GAIYAK          PIC X(01).
           03  FLG-ARI             PIC X(01).
      *    セット処理有無
           03  FLG-SETSYORI        PIC 9(01).
           03  FLG-SET             PIC 9(01).
           03  FLG-YAKUSOKU        PIC 9(01).
      *    漢字短縮
           03  FLG-TANSYUKU        PIC 9(01).
      *    自費入力
           03  FLG-JIHI            PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-OK2             PIC 9(01).
           03  FLG-UPD             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
      *    空白入力
           03  FLG-KUHAKU          PIC 9(01).
           03  FLG-INS             PIC 9(01).
           03  FLG-MEI             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDS                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-T               PIC 9(04).
      *
           03  IDX-STR             PIC 9(04).
           03  IDY-STR             PIC 9(04).
           03  IDX-SIN             PIC 9(04).
      *
           03  IDX-K               PIC 9(04).
           03  IDX-K1              PIC 9(04).
           03  IDX-C               PIC 9(04).
           03  IDX-X               PIC 9(04).
           03  IDX-S2              PIC 9(04).
           03  IDX-IP              PIC 9(04).
           03  IDX-SST             PIC 9(04).
      *
           03  IDX-SS2             PIC 9(04).
           03  IDX-D1              PIC 9(02).
           03  IDX-D2              PIC 9(02).
           03  IDX-ATO             PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-C1              PIC 9(04).
           03  IDX-C2              PIC 9(04).
           03  IDX-C3              PIC 9(04).
           03  IDX-C4              PIC 9(04).
      *
           03  IDX-I48             PIC 9(04).
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-FREE            PIC 9(04).
           03  WRK-IDX             PIC 9(03).
      *    入力項目名編集用 
           03  WRK-IDX2            PIC 9(02).
           03  WRK-IN-MAP-IDX      PIC 9(02).
      *
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1    PIC ZZZZZZZ.
               05  WRK-SURYO-Z19   REDEFINES  WRK-SURYO-Z1
                                   PIC ZZZZZZ9.
               05  WRK-SURYO-Z2    PIC X(01).
               05  WRK-SURYO-Z3    PIC ZZZ.
           03  WRK-TENSU-X.
               05  WRK-TENSU-Z     PIC ZZZZZZZZ.
           03  WRK-TENSUS-X.
               05  WRK-TENSUS-Z    PIC ZZZZ9.99.
      *
           03  WRK-ZAITEN          PIC 9(08).
           03  WRK-ZAITEN-90       PIC 9(08)V9(02).
      *
           03  WRK-KINGAK          PIC 9(08).
           03  WRK-SRYSYUKBN       PIC X(03).
           03  WRK-SRYSYUKBNMEI    PIC X(40).
           03  WRK-SRYKBN          PIC X(02).
      *
           03  WRK-SURYOKEI        PIC 9(07)V9(03).
           03  WRK-SRYCDKEI        PIC 9(14).
           03  WRK-MEISAISU        PIC 9(07).
           03  WRK-SRYCD-9         PIC 9(09).
      *
           03  WRK-MEINUM          PIC 9(04).
           03  WRK-RENNUM          PIC 9(04).
           03  WRK-ZAINUM          PIC 9(08).
           03  WRK-H-ZAINUM        PIC 9(08).
      *    診療会計用集計
           03  WRK-WKSRY-AREA.
               05  WRK-SYUTEN1             PIC  9(07).
               05  WRK-SYUTEN2             PIC  9(07).
               05  WRK-YKZTEN              PIC  9(07).
               05  WRK-KIZAITEN            PIC  9(07).
      *
           03  WRK-MKBN            PIC X(01).
      *    警告済フラグ保存
           03  WRK-TIMEFLG         PIC X(01).
      *    入力位置
           03  WRK-FLG-IN          PIC X(01).
      *    入力セット処理
           03  WRK-SETCD           PIC X(06).
      *
           03  WRK-GMN-IDX         PIC 9(04).
      *    入力位置判定用
           03  WRK-IN-IDX4         PIC 9(04).
           03  WRK-IN-IDX2         PIC 9(04).
      *
           03  WRK-PATH            PIC X(64).
      *
           03  WRK-DAY             PIC 9(03).
      *    入力時表示用
           03  FLG-NYURYOKU        PIC 9(01).
           03  FLG-IN-ATAI         PIC 9(01).
      *    画面制御
           03  WRK-PUTTYPE         PIC X(08).
           03  WRK-JIDMSG          PIC X(80).
      *
           03  WRK-INPUTCD         PIC X(22).
           03  WRK-INPUTCD2        PIC X(22).
           03  WRK-INPUTCD3        PIC X(22).
           03  WRK-CHK-FLG         PIC X(01).
           03  WRK-STR             PIC 9(02).
           03  WRK-END             PIC 9(02).
           03  WRK-HENKAN          PIC X(22).
           03  WRK-CD              PIC X(22).
           03  WRK-WK-CD           PIC X(22).
           03  WRK-CD-MCNT         PIC 9(02).
           03  WRK-ICD-CDSYU       PIC X(01).
      *
      *    入力値の入力判定用
           03  WRK-HZN-ATAI-AREA.
               05  WRK-HZN-ATAI1   PIC X(10).
               05  WRK-HZN-ATAI2   PIC X(10).
               05  WRK-HZN-ATAI3   PIC X(10).
               05  WRK-HZN-ATAI4   PIC X(10).
      *    入力名称
           03  WRK-MEISYO-G.
               05  WRK-MEIH        PIC X(02).
               05  WRK-MEISYO      PIC X(60).
      *
      *    行位置編集
       01  WRK-GMNGYO-AREA.
           03  WRK-MAP-MAX         PIC 9(04).
           03  WRK-MAP-CUR         PIC 9(04).
           03  WRK-MAP-CUR2        PIC 9(04).
           03  WRK-MAP-CUR3        PIC 9(04).
           03  WRK-PAGE            PIC 9(04).
           03  WRK-MAP-PAGE        PIC 9(04).
           03  WRK-MAP-PAGE2       PIC 9(04).
           03  WRK-MAP-PAGE3       PIC 9(04).
           03  WRK-MAP-MAXPAGE     PIC 9(04).
      *
      *    画面数量編集用
       01  WRK-GMN-SURYO-AREA.
           03  WRK-SURYO-HENSYU.
               05  WRK-H-SURYO     PIC X(09).
               05  WRK-H-TANINAME  PIC X(04).
               05  WRK-H-F1        PIC X(02).
               05  WRK-H-ZAIKEI    PIC X(08).
           03  WRK-ZZ              PIC ZZ.
      *
      *    診療行為内容比較用テーブル
       01  TBL-AREA.
           03  TBL-MAE-AREA.
               05  TBL-MAE-REC     OCCURS   30.
                   07  TBL-MAE-SRYCD       PIC  X(09).
                   07  TBL-MAE-SRYSURYO    PIC  9(05)V9(03).
                   07  TBL-MAE-SRYKAISU    PIC  9(03).
                   07  TBL-MAE-INPUTCOMENT PIC  X(40).
                   07  TBL-MAE-ATAI-G.
                       09  TBL-MAE-ATAI    PIC  X(08)  OCCURS  3.
           03  TBL-ATO-AREA.
               05  TBL-ATO-REC     OCCURS   30.
                   07  TBL-ATO-SRYCD       PIC  X(09).
                   07  TBL-ATO-SRYSURYO    PIC  9(05)V9(03).
                   07  TBL-ATO-SRYKAISU    PIC  9(03).
                   07  TBL-ATO-INPUTCOMENT PIC  X(40).
                   07  TBL-ATO-ATAI-G.
                       09  TBL-ATO-ATAI    PIC  X(08)  OCCURS  3.
      *
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
      *
      *
       01  WRK-MOJI-AREA.
           03  FLG-SP              PIC 9(02).
           03  FLG-JKNKSN          PIC 9(02).
           03  WRK-IDX-FT          PIC 9(02).
           03  WRK-KAI             PIC X(20).
           03  WRK-SUR             PIC X(20).
           03  IDM                 PIC 9(04).
      *
           03  WRK-HENKAN-MAE      PIC X(20).
           03  WRK-HENKAN-ATO      PIC X(20).
           03  WRK-JODOU           PIC 9(01).
           03  WRK-KENSAKU         PIC X(01).
           03  IDH1                PIC 9(04).
           03  IDH2                PIC 9(04).
           03  WRK-TEN-GMN-IDX     PIC 9(04).
           03  WRK-TEN-SRYCD       PIC X(09).
           03  WRK-TEN-NENREI.
               05  WRK-TEN-NENREI-YY     PIC 9(03).
               05  WRK-TEN-NENREI-MM     PIC 9(02).
               05  WRK-TEN-NENREI-DD     PIC 9(02).
           03  WRK-TEN-ROUJIN            PIC 9(01).
           03  WRK-KGNAGE          PIC 9(03).
           03  WRK-JGNAGE          PIC 9(03).
           03  WRK-MAX             PIC 9(04).
           03  WRK-TIME            PIC 9(01).
      *    老人・一般変換用
           03  WRK-SRYCD-CHG-AREA.
               05  WRK-SRYCDCHG        PIC X(09)   OCCURS  5.
           03  WRK-ATAI-X.
               05  WRK-ATAI-9          PIC 9(15).
      *
      *    禁忌チェック用テーブル
       01  WRK-KNKCHK-AREA.
           03  WRK-KNK-TBL                  OCCURS   30.
               05  WRK-KNK-TOUMEI           PIC X(40). 
               05  WRK-KNK-TOUSRYCD         PIC X(09). 
               05  WRK-KNK-TBL2A.
                   07  WRK-KNK-TBL2         OCCURS   30.
                       09  WRK-KNK-KNKMEI   PIC X(40).
                       09  WRK-KNK-SRYYM    PIC X(06).
                       09  WRK-KNK-SRYYMW   PIC X(07).
                       09  WRK-KNK-SRYCD    PIC X(09).
      *
       01  WRK-SORT-AREA.
           03  WRK-SRT-TBL2A.
               05  WRK-SRT-TBL2            OCCURS   30.
                   07  WRK-SRT-KNKMEI      PIC X(40).
                   07  WRK-SRT-SRYYM       PIC X(06).
                   07  WRK-SRT-SRYYMW      PIC X(07).
                   07  WRK-SRT-SRYCD       PIC X(09).
           03  WRK-KEY-SRYYM               PIC X(06).
           03  WRK-KEY-SRYCD               PIC X(09).
      *
      *    コメントの入力値用
       01  WRK-INPUT-SET-AREA.
           03  WRK-INPUT-TBL.
               05  WRK-INPUT-SET       PIC 9(03)   OCCURS  8.
           03  WRK-INPUT-TBL-R         REDEFINES   WRK-INPUT-TBL.
               05  WRK-INPUT-TBL2      OCCURS  4.
                   07  WRK-INPUT-ICHI  PIC 9(03).
                   07  WRK-INPUT-KETA  PIC 9(03).
           03  WRK-KETA                PIC 9(02).
           03  WRK-KETA2               PIC 9(02).
           03  WRK-ICHI                PIC 9(02).
           03  WRK-ICHI2               PIC 9(02).
           03  IDX-KNJ                 PIC 9(02).
           03  IDX-KNJ1                PIC 9(01).
           03  IDX-KNJ2                PIC 9(04).
      *
      *    画面ＳＰＡ領域ワーク
       01  WRK-SCR-REC.
           COPY    "I4SCR-NYU.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *
      *    ワークテーブル２(一時保存用）
       01  WRK-HOZ-AREA.
           03  WRK-HGMN-TBL            OCCURS   200.
               05  WRK-HGMN-TBLREC.
                   07  WRK-HGMN-DATA   PIC X(130).
               05  WRK-HCOM-TBLREC.
                   07  WRK-HCOM-DATA   PIC X(400).
           03  WRK-HOZ-IDX             PIC 9(04).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1006.INC".
      *
      *    点数マスタ−
           COPY    "CPTENSU.INC".
      *
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *  診療行為テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *    入院行為マスタ−
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    入院会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    チェックマスタ−
       01  CHK-REC.
           COPY    "CPCHK.INC".
      *
      *    入力セットコード
       01  INPUTSET-REC.
           COPY    "CPINPUTSET.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    老人一般コード変換
       01  SRYCDCHG-REC.
           COPY    "CPSRYCDCHG.INC".
      *
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
      *
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC00.INC".
      *
      *    算定履歴チェックパラメタ
           COPY    "CPORCSC91.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
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
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-I44-I48.
           MOVE    SPA-WORK        TO  SPA-I4KYOUTU.
      *
           MOVE    SPACE           TO  SPA-ERRCD.
           MOVE    ZERO            TO  FLG-END.
      *
      *    診療種別テーブルセット
           PERFORM 1001-SRYKBN-SET-SEC.
      *
      *    画面制御
           MOVE    SPACE           TO  WRK-PUTTYPE.
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
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF.
      *
           MOVE    SPA-I4KYOUTU      TO  SPA-WORK.
           MOVE    SPA-AREA          TO  SPA-COMMON.
           MOVE    SPA-I44-I48       TO  SPA-FREE.
      *
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA.
           INITIALIZE                  STS-AREA.
           INITIALIZE                  WRK-AREA.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "I4ERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               MOVE    SPA-I44GMN-AREA(5:) TO  SPA-SCR-REC(5:)
               PERFORM 500-GMNHEN-SEC
           ELSE
      *
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO      TO      100-INIT-EXT
               END-IF
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF.
      *
           MOVE   SPACE                TO  LNK-KYOUTU.
      *
           MOVE   SPACE                TO  MCP-PUTTYPE.
           MOVE   "I44"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       100-INIT-EXT.
           EXIT.
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
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-GMN.
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "I41"
                   INITIALIZE              SPA-I48-AREA
                   INITIALIZE              SPA-I44-I48
                   INITIALIZE              SPA-SCR-REC
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-I4
                   MOVE    SPA-WORK        TO  SPA-I4KYOUTU
                   MOVE    9999            TO  IDY-STR
               WHEN    "I49"
                   IF      SPA-I49-DATA    NOT =   SPACE
                       MOVE    SPA-I44GMN-AREA    TO  SPA-SCR-REC
                       MOVE    SPA-COM-GYOU(SPA-GMN-IDX4)
                                               TO  WRK-IN-MAP-IDX
                       MOVE    SPA-GMN-IDX4    TO  WRK-STR
                       MOVE    SPA-I49-DATA    TO  SPA-GMN-INPUTCD
                                                          (SPA-GMN-IDX4)
                       MOVE    "1"             TO  SPA-COM-IN
                                                          (SPA-GMN-IDX4)
      *
      *                入力コード・名称チェック処理
                       MOVE    SPACE           TO  FLG-TOROKU
                       PERFORM 410112-KOUI-SPACHK-SEC
                       MOVE    1               TO  FLG-GMN
                   ELSE
                       MOVE    SPA-I44GMN-AREA TO  SPA-SCR-REC
                       MOVE    "1"             TO  SPA-COM-CUR
                                                          (SPA-GMN-IDX4)
                       MOVE    2               TO  FLG-GMN
                   END-IF
               WHEN    "I48"
                   IF      SPA-I48-SRYCD-G   NOT =   SPACE
                       PERFORM 3011-I48-SET-SEC
                       IF      SPA-ERRCD       NOT =   SPACE
                           PERFORM 510-ERRSET-SEC
                           GO      TO      300-SCREEN-EXT
                       END-IF
                       IF      WRK-MKBN       =   "1"
                           MOVE    "1"             TO  SPA-COM-CUR
                                                          (SPA-GMN-IDX4)
                       END-IF
                       MOVE    1               TO  FLG-GMN
                   ELSE
                       MOVE    SPA-I44GMN-AREA TO  SPA-SCR-REC
                       MOVE    "1"             TO  SPA-COM-CUR
                                                          (SPA-GMN-IDX4)
                       MOVE    2               TO  FLG-GMN
                       MOVE    1               TO  SPA-GMN-CUR4
                   END-IF
               WHEN    "I46"
      *             禁忌一覧 ＯＫの時、更新処理へ
                   IF      SPA-I46-CHK        =   1
      *                画面制御
                       MOVE    "JOIN"          TO  WRK-PUTTYPE
      *                更新処理
                       PERFORM 41002-KOUSIN-SYORI-SEC
                   END-IF
                   MOVE    2               TO  FLG-GMN
               WHEN    "I47"
      *            禁忌画面より
                   MOVE    2               TO  FLG-GMN
               WHEN    "I4ID1"
      *            確認画面より
                   PERFORM 3003-JID1-SET-SEC
                   MOVE    SPACE           TO  SPA-IIDCD
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE.
           MOVE    SPACE           TO  SPA-IIDCD.
      *
           IF      FLG-GMN         NOT =   ZERO
               GO      TO      300-SCREEN-EXT
           END-IF.
      *
      *    画面用スパ編集処理
           PERFORM                 310-SPA-SET-SEC.
      *
           MOVE    1               TO  SPA-GMN-PAGE4.
           MOVE    1               TO  SPA-GMN-CUR4.
           MOVE    1               TO  SPA-MAP-IDX4.
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
               WHEN    "0101"
      *            戻るの確認
                   IF      SPA-I4ID-FLG        =   "OK"
                       MOVE    SPACE           TO  SPA-IIDCD
      *                画面制御
                       MOVE    "JOIN"          TO  WRK-PUTTYPE
      *                更新処理
                       PERFORM 41002-KOUSIN-SYORI-SEC
                   ELSE
                       MOVE    1                   TO  SPA-GMN-CUR4
                   END-IF
      *
           END-EVALUATE.
      *
       3003-JID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
           MOVE    SPACE               TO  SYS-1006-REC.
           INITIALIZE                      SYS-1006-REC.
           MOVE    "1006"              TO  SYS-1006-KANRICD.
           MOVE    "1"                 TO  SYS-1006-KBNCD.
           MOVE    ZERO                TO  SYS-1006-STYUKYMD.
           MOVE    ALL "9"             TO  SYS-1006-EDYUKYMD.
      *    システム管理検索（施設基準）
           MOVE    SYS-1006-REC        TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH.
           MOVE    SPA-SRYYMD          TO  ORC-DBYMD.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1006-REC
               INITIALIZE                  SYS-1006-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF.
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1006-REC
               IF      SYS-1006-SSTKIJUN (121) =   1
                   MOVE    1               TO  SPA-SYONIKA
               END-IF
      *        時間外特例医療機関
               IF      SYS-1006-SSTKIJUN (011) =   1
                   MOVE    1               TO  SPA-JIKANTOKU
               END-IF
           END-IF.
      *
           MOVE    ZERO                TO  SPA-GMN-MAX.
           MOVE    ZERO                TO  IDX2.
           MOVE    ZERO                TO  SPA-GMN-IDX4.
      *
           MOVE    "2"                 TO  FLG-TOROKU.
      *
      *    入院行為マスターを編集する
           PERFORM 3103-NYUIN-HEN-SEC.
      *
      *    診療行為、計計算処理、編集
           MOVE    "2"             TO  FLG-TOROKU.
           PERFORM 510-TBLHEN-SEC.
           MOVE    SPACE           TO  FLG-TOROKU.
      *
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧画面（Ｉ４８）ＯＫ処理
      *****************************************************************
       3011-I48-SET-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-IN-MAP-IDX.
           MOVE    SPA-GMN-IDX4        TO  WRK-STR.
           MOVE    ZERO                TO  IDY-STR.
      *
           IF      IDY-STR             =   ZERO
               MOVE    SPA-GMN-IDX4        TO  IDY-STR
           END-IF.
      *
           MOVE    SPA-I48-SRYCD(1)    TO  SPA-GMN-INPUTCD
                                                   (SPA-GMN-IDX4).
           IF      SPA-I48-SRYCD(1)(1:2)   =
                                   "81"  OR  "83"  OR  "84"  OR  "89"
               IF      SPA-GMN-IDX4   =    SPA-GMN-MAX
                   MOVE    SPACE               TO  WRK-MKBN
                   MOVE    ZERO                TO  FLG-MEI
               ELSE
                   MOVE    "1"                 TO  WRK-MKBN
               END-IF
           ELSE
               MOVE    SPACE               TO  WRK-MKBN
               MOVE    1                   TO  FLG-MEI
           END-IF.
      *
      *    入力コード・名称チェック処理
           MOVE    SPACE           TO  FLG-TOROKU.
           PERFORM 410112-KOUI-SPACHK-SEC.
      *
       3011-I48-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   入院行為マスタより編集処理
      *****************************************************************
       3103-NYUIN-HEN-SEC               SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-MAX.
           MOVE    ZERO                TO  IDX2.
      *
           INITIALIZE                      NYUINACT-REC.
           MOVE    SPA-HOSPID          TO  NSRY-HOSPID.
           MOVE    "1"                 TO  NSRY-NYUGAIKBN.
           MOVE    SPA-GMN-PTID        TO  NSRY-PTID.
           MOVE    SPA-I40-SRYKA-READ  TO  NSRY-SRYKA.
           MOVE    SPA-I40-SRYYMD(1:6) TO  NSRY-SRYYM.
           MOVE    SPA-ZAINUM          TO  NSRY-ZAINUM.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "NYUINACT-KEY2"     TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "NYUINACT-KEY2"     TO  ORC-DBPATH
               PERFORM 900-NYUINACT-READ-SEC
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1               TO  FLG-NYUINACT
           END-IF.
      *
           MOVE    ZERO                TO  FLG-YAKUSOKU.
      *
           PERFORM     UNTIL     FLG-NYUINACT      =   1
      *
               PERFORM VARYING     IDS     FROM    1   BY  1
                       UNTIL      (IDS             >   5     )  OR
                                  (NSRY-SRYCD (IDS) =   SPACE)
                   PERFORM 31011-SPA-HEN2-SEC
               END-PERFORM
      *
               MOVE    "NYUINACT-KEY2"     TO  ORC-DBPATH
               PERFORM 900-NYUINACT-READ-SEC
           END-PERFORM.
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "NYUINACT-KEY2"     TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
           MOVE    IDX2                TO  SPA-GMN-MAX.
      *
       3103-NYUIN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   再表示あり　編集処理
      *****************************************************************
       31011-SPA-HEN2-SEC             SECTION.
      *
      *    診療種別セット
           IF     (IDS                 =   1)  AND
                  (NSRY-RENNUM         =   1)
               IF      NSRY-SRYSYUKBN  NOT =   SPACE
                   ADD     1                   TO  IDX2
                   MOVE    "."                 TO  SPA-GMN-INPUTCD
                                                            (IDX2)(1:1)
                   MOVE    NSRY-SRYSYUKBN      TO  SPA-GMN-INPUTCD
                                                           (IDX2)(2:3)
                   MOVE    NSRY-SRYSYUKBN      TO  SPA-COM-SRYSYUKBN
                                                            (IDX2)
                   MOVE    NSRY-SRYKBN         TO  SPA-COM-SRYKBN(IDX2)
               END-IF
           END-IF.
      *
      *    明細編集
           ADD     1                   TO  IDX2.
           MOVE    NSRY-SRYKBN         TO  SPA-COM-SRYKBN    (IDX2).
           MOVE    NSRY-SRYSYUKBN      TO  SPA-COM-SRYSYUKBN (IDX2).
           MOVE    NSRY-SRYCD   (IDS)  TO  SPA-COM-INPUTCD   (IDX2).
           MOVE    NSRY-SRYSURYO(IDS)  TO  SPA-COM-SURYO     (IDX2).
           MOVE    NSRY-SRYSURYO(IDS)  TO  SPA-COM-SURYO2    (IDX2).
           MOVE    NSRY-AUTOKBN (IDS)  TO  SPA-COM-AUTOKBN   (IDX2).
      *
      *    約束セットの時、表示なしとする（但し、減は表示する）
           IF     (FLG-YAKUSOKU         =   1          )  AND
                  (NSRY-SRYCD (IDS) NOT =   "820000047")
               MOVE    "1"                 TO  SPA-COM-SET (IDX2)
           END-IF.
      *
      *    回数を編集
           MOVE    IDX2                    TO  IDXS.
      *
      *    分画数を編集
           MOVE    NSRY-SRYKAISU (IDS)     TO  SPA-COM-BUNGSU (IDX2).
      *
           MOVE    SPA-COM-INPUTCD (IDX2)  TO  SPA-GMN-INPUTCD  (IDX2).
      *
      *    約束セットの時、以下処理なし
           IF      NSRY-SRYCD(IDS)(1:1) =   "S"
               MOVE    1               TO  FLG-YAKUSOKU
               GO      TO      31011-SPA-HEN2-EXT
           END-IF.
      *
           MOVE    IDX2                TO  WRK-TEN-GMN-IDX.
           MOVE    SPA-COM-INPUTCD(IDX2)
                                       TO  WRK-TEN-SRYCD.
           MOVE    SPA-NAI-NENREI      TO  WRK-TEN-NENREI.
           MOVE    SPA-ROUJIN          TO  WRK-TEN-ROUJIN.
           PERFORM 31011-TEN-HENKAN-SEC.
           IF      SPA-ERRCD   NOT =   SPACE
               GO      TO      31011-SPA-HEN2-EXT
           END-IF.
      *
      *    コメントの時、コメント内容編集
           IF     (SPA-COM-INPUTCD (IDX2)(1:1) =   "0"  OR  "8")  AND
                  (NSRY-INPUTNUM   (IDS)   NOT =   ZERO        )
               INITIALIZE                  PTCOM-REC
               MOVE    SPA-HOSPID          TO  PTCOM-HOSPID
               MOVE    SPA-GMN-PTID        TO  PTCOM-PTID
               MOVE    SPA-ZAINUM          TO  PTCOM-ZAINUM
               MOVE    NSRY-SRYCD    (IDS) TO  PTCOM-SRYCD
               MOVE    NSRY-INPUTNUM (IDS) TO  PTCOM-RENNUM
               PERFORM 960-PTCOM-READ-SEC
               IF      FLG-PTCOM           =   ZERO
                   IF      PTCOM-INPUTCOMENT   NOT =   SPACE
                       IF      NSRY-SRYCD (IDS)        =   "810000001"
                           MOVE    "1"         TO  SPA-COM-MEIFLG
                                                                 (IDX2)
                       END-IF
                   END-IF
                   MOVE    PTCOM-INPUTCOMENT   TO  SPA-GMN-MEISYO-G
                                                                 (IDX2)
                   MOVE    PTCOM-INPUTCHI-AREA TO  SPA-COM-INPUTCHI-G
                                                                 (IDX2)
                END-IF
           END-IF.
      *
           IF      SPA-GMN-INPUTCD(IDX2)(1:2)  =   "84"
               MOVE    ZERO                     TO  IDX-C3
               MOVE    1                        TO  IDX-C4
               PERFORM UNTIL       IDX-C4       >   22
                   IF      SPA-GMN-INPUTCD (IDX2)(IDX-C4:1)  =  SPACE
                       MOVE    IDX-C4                TO  IDX-C3
                       MOVE    30                    TO  IDX-C4
                   END-IF
                   COMPUTE   IDX-C4   =   IDX-C4   +   1
               END-PERFORM
               COMPUTE   IDX-C3   =   IDX-C3   +   1
               MOVE    1                        TO  IDX-C4
               PERFORM UNTIL       IDX-C4       >   60
                   MOVE    1                        TO  WRK-STR
                   MOVE    1                        TO  WRK-END
                   IF      SPA-GMN-MEISYO (IDX2)(IDX-C4:1)  >=  X"A1"
                       MOVE    SPA-GMN-MEISYO (IDX2)(IDX-C4:2)
                                                    TO  WRK-HENKAN-MAE
                       PERFORM 4101-KOUI-MOJI-HENKAN-SEC
                       COMPUTE   IDX-C4   =   IDX-C4   +   2
                       IF     (WRK-HENKAN-ATO(1:1)    >=  X"A1")   OR
                              (WRK-HENKAN-ATO(1:1)     =  SPACE)
                           CONTINUE
                       ELSE
                           MOVE    WRK-HENKAN-ATO(1:1)
                                             TO  SPA-GMN-INPUTCD
                                                     (IDX2)(IDX-C3:1)
                           COMPUTE   IDX-C3   =   IDX-C3   +   1
                       END-IF
                   ELSE
                       MOVE    70            TO  IDX-C4
                   END-IF
               END-PERFORM
           END-IF.
           MOVE    SPA-GMN-INPUTCD (IDX2)  TO  SPA-MAE-INPUTCD (IDX2).
      *
           IF      SPA-COM-INPUTCD(IDX2)(1:3)  =   "095"  OR  "096"
               IF      SPA-COM-KISOTEN (IDX2)      =   ZERO
                   MOVE    "1"                 TO  SPA-COM-KEIFLG(IDX2)
                   MOVE    NSRY-JIHIMONEY(IDS)
                                               TO  SPA-COM-KEI   (IDX2)
                   MOVE    NSRY-JIHIMONEY(IDS)
                                               TO  SPA-GMN-KEI   (IDX2)
               ELSE
                   MOVE    "2"                 TO  SPA-COM-KEIFLG(IDX2)
                   MOVE    SPA-COM-KISOTEN (IDX2)
                                               TO  SPA-COM-KEI   (IDX2)
                   MOVE    SPA-COM-KISOTEN (IDX2)
                                               TO  SPA-GMN-KEI   (IDX2)
               END-IF
      *
      *        自費の時、金額編集
           ELSE
               MOVE    SPACE               TO  SPA-COM-KEIFLG(IDX2)
           END-IF.
      *
       31011-SPA-HEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数編集変換処理
      *****************************************************************
       31011-TEN-HENKAN-SEC             SECTION.
      *
           MOVE    WRK-TEN-GMN-IDX     TO  IDX-T.
      *    約束の時、処理なし
           IF     (WRK-TEN-SRYCD(1:1) =   "S")  AND
                  (WRK-TEN-SRYCD(2:5) NUMERIC)  AND
                  (WRK-TEN-SRYCD(7:)  =   SPACE)
               GO      TO      31011-TEN-HENKAN-EXT
           END-IF.
      *    入院回数処理なし
           IF      WRK-TEN-SRYCD(1:1) =   "*"   OR  SPACE
               GO      TO      31011-TEN-HENKAN-EXT
           END-IF.
      *
           MOVE    WRK-TEN-SRYCD      TO  TNS-SRYCD.
           PERFORM 910-TENSU-READ-SEC.
           IF      FLG-TENSU           =   1
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    "1"                 TO  SPA-COM-CUR (IDX-T)
               GO      TO      31011-TEN-HENKAN-EXT
           END-IF.
      *
      *    病院診療所区分チェック
           IF     (TNS-HOSPSRYKBN      =   1)   AND
                  (SPA-HOSPSBT     NOT =   1)
               MOVE    "0016"          TO  SPA-ERRCD
               MOVE    "1"             TO  SPA-COM-CUR (IDX-T)
               GO      TO      31011-TEN-HENKAN-EXT
           END-IF.
           IF     (TNS-HOSPSRYKBN      =   2)   AND
                  (SPA-HOSPSBT     NOT =   2)
               MOVE    "0016"          TO  SPA-ERRCD
               MOVE    "1"             TO  SPA-COM-CUR (IDX-T)
               GO      TO      31011-TEN-HENKAN-EXT
           END-IF.
      *
      *    入外適用区分チェック
           IF     (TNS-NYUGAITEKKBN    =   1 )   AND
                  (SPA-NYUGAIKBN   NOT =  "1")
               MOVE    "0014"          TO  SPA-ERRCD
               MOVE    "1"             TO  SPA-COM-CUR (IDX-T)
               GO      TO      31011-TEN-HENKAN-EXT
           END-IF.
           IF     (TNS-NYUGAITEKKBN    =   2 )   AND
                  (SPA-NYUGAIKBN   NOT =  "2")
               MOVE    "0014"          TO  SPA-ERRCD
               MOVE    "1"             TO  SPA-COM-CUR (IDX-T)
               GO      TO      31011-TEN-HENKAN-EXT
           END-IF.
      *
      *    老人適用区分チェック
           IF     (TNS-ROUTEKKBN        =   1   )   AND
                  (WRK-TEN-ROUJIN   NOT =   ZERO)
               PERFORM 31011-TEN-SRYCDCHG-HEN-SEC
           END-IF.
           IF     (TNS-ROUTEKKBN        =   2)   AND
                  (WRK-TEN-ROUJIN   NOT =   1)
               PERFORM 31011-TEN-SRYCDCHG-HEN-SEC
           END-IF.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "1"             TO  SPA-COM-CUR (IDX-T)
               GO      TO      31011-TEN-HENKAN-EXT
           END-IF.
      *
      *    施設基準適合分チェック（コメント・用法は対象外）
           IF     (WRK-TEN-SRYCD  (1:1)   =   "8"  )  OR
                  (WRK-TEN-SRYCD  (1:3)   =   "001")  OR
                  (TNS-SSTKIJUNCD(01)     =   ZERO )
               CONTINUE
           ELSE
      *        施設基準情報検索
               MOVE    SPACE               TO  SYS-1006-REC
               MOVE    "1006"              TO  SYS-1006-KANRICD
               MOVE    "1"                 TO  SYS-1006-KBNCD
               MOVE    ZERO                TO  SYS-1006-STYUKYMD
               MOVE    ALL "9"             TO  SYS-1006-EDYUKYMD
      *        システム管理検索
               MOVE    SYS-1006-REC        TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               MOVE    SPA-SRYYMD          TO  ORC-DBYMD
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
                   PERFORM 900-KANRIMST-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1006-REC
               ELSE
                   INITIALIZE                  SYS-1006-REC
               END-IF
      *
               MOVE    ZERO                TO  FLG-SST
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX         >   10)   OR
                                  (FLG-SST     =   1 )
                   IF      TNS-SSTKIJUNCD (IDX)    =   ZERO
                       CONTINUE
                   ELSE
                       MOVE    TNS-SSTKIJUNCD (IDX)    TO  IDY
                       IF      SYS-1006-SSTKIJUN (IDY) =   1
                           MOVE    1               TO  FLG-SST
                       END-IF
                   END-IF
               END-PERFORM
               IF      FLG-SST             =   ZERO
                   MOVE    "0024"          TO  SPA-ERRCD
                   MOVE    "1"             TO  SPA-COM-CUR (IDX-T)
                   GO      TO      31011-TEN-HENKAN-EXT
               END-IF
           END-IF.
      *
      *    対象年齢チェック
           IF      WRK-TEN-SRYCD(1:1) =   "1"
               MOVE    ZERO                TO  WRK-KGNAGE
               MOVE    ZERO                TO  WRK-JGNAGE
               MOVE    TNS-KGNAGE          TO  WRK-KGNAGE(2:2)
               MOVE    TNS-JGNAGE          TO  WRK-JGNAGE(2:2)
      *
      *        下限年齢チェック
               IF      TNS-KGNAGE        =   SPACE  OR  ZERO  OR  "AA"
                   CONTINUE
               ELSE
                   IF      WRK-TEN-NENREI-YY   >=  WRK-KGNAGE
                       CONTINUE
                   ELSE
                       MOVE    "0032"          TO  SPA-ERRCD
                       MOVE    "1"             TO  SPA-COM-CUR (IDX-T)
                       GO      TO      31011-TEN-HENKAN-EXT
                   END-IF
               END-IF
      *
      *        上限年齢チェック
               IF      TNS-JGNAGE          =   SPACE  OR  ZERO
                   CONTINUE
               ELSE
                   IF      TNS-JGNAGE          =   "AA"
      *                新生児判定
                       IF     (WRK-TEN-NENREI-YY    =   ZERO)  AND
                              (WRK-TEN-NENREI-MM    =   ZERO)  AND
                              (WRK-TEN-NENREI-DD    <=  28)
                           CONTINUE
                       ELSE
                           MOVE    "0032"              TO  SPA-ERRCD
                       END-IF
                   ELSE
                       IF      WRK-TEN-NENREI-YY     <   WRK-JGNAGE
                           CONTINUE
                       ELSE
                           MOVE    "0032"              TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
           END-IF.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "1"                 TO  SPA-COM-CUR (IDX-T)
               GO      TO      31011-TEN-HENKAN-EXT
           END-IF.
      *
      *    病床数チェック
           IF      TNS-BYOSYOKBN   NOT =   ZERO
      *        医療機関情報
               MOVE    SPACE               TO  SYS-1001-REC
               INITIALIZE                  SYS-1001-REC
               MOVE    "1001"              TO  SYS-1001-KANRICD
               MOVE    "*"                 TO  SYS-1001-KBNCD
               MOVE    ZERO                TO  SYS-1001-STYUKYMD
               MOVE    ALL "9"             TO  SYS-1001-EDYUKYMD
      *        システム管理検索
               MOVE    SYS-1001-REC        TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               MOVE    SPA-SRYYMD          TO  ORC-DBYMD
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
                   PERFORM 900-KANRIMST-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1001-REC
               ELSE
                   INITIALIZE                  SYS-1001-REC
               END-IF
      *
               EVALUATE    TNS-BYOSYOKBN
                   WHEN    1
                       IF      SYS-1001-BEDSU      >=  1  AND  <=  99
                           CONTINUE
                       ELSE
                           MOVE    "0075"      TO  SPA-ERRCD
                           MOVE    "1"         TO  SPA-COM-CUR (IDX-T)
                           GO      TO      31011-TEN-HENKAN-EXT
                       END-IF
                   WHEN    2
                       IF      SYS-1001-BEDSU      >=  100 AND  <=  199
                           CONTINUE
                       ELSE
                           MOVE    "0075"      TO  SPA-ERRCD
                           MOVE    "1"         TO  SPA-COM-CUR (IDX-T)
                           GO      TO      31011-TEN-HENKAN-EXT
                       END-IF
                   WHEN    3
                       IF      SYS-1001-BEDSU      <   200
                           CONTINUE
                       ELSE
                           MOVE    "0075"      TO  SPA-ERRCD
                           MOVE    "1"         TO  SPA-COM-CUR (IDX-T)
                           GO      TO      31011-TEN-HENKAN-EXT
                       END-IF
                   WHEN    4
                       IF      SYS-1001-BEDSU      >=  200
                           CONTINUE
                       ELSE
                           MOVE    "0075"      TO  SPA-ERRCD
                           MOVE    "1"         TO  SPA-COM-CUR (IDX-T)
                           GO      TO      31011-TEN-HENKAN-EXT
                       END-IF
               END-EVALUATE
           END-IF.
      *
      *    労災・自賠責チェック
      *    労災のコード
           IF     (WRK-TEN-SRYCD (1:1)    =   "1" )  AND
                  (WRK-TEN-SRYCD (2:2)    =   "01")  AND
                  (SPA-HKNKBN         NOT =   1   )
                   MOVE    "8000"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  SPA-COM-CUR (IDX-T)
                   GO      TO      31011-TEN-HENKAN-EXT
           END-IF.
      *    労災（器材）
           IF     (WRK-TEN-SRYCD (1:1)    =   "7"    )  AND
                  (WRK-TEN-SRYCD (2:5)    =   "88888")  AND
                  (SPA-HKNKBN         NOT =   1      )
                   MOVE    "8000"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  SPA-COM-CUR (IDX-T)
                   GO      TO      31011-TEN-HENKAN-EXT
           END-IF.
      *    労災（コメント）
           IF     (WRK-TEN-SRYCD (1:1)    =   "8")  AND
                  (WRK-TEN-SRYCD (4:1)    =   "8")  AND
                  (SPA-HKNKBN         NOT =   1  )
                   MOVE    "8000"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  SPA-COM-CUR (IDX-T)
                   GO      TO      31011-TEN-HENKAN-EXT
           END-IF.
      *
      *    労災加算振替え
           IF     (WRK-TEN-SRYCD          =   "099509901")  AND
                  (SPA-HKNKBN         NOT =   1          )
                   MOVE    "8000"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  SPA-COM-CUR (IDX-T)
                   GO      TO      31011-TEN-HENKAN-EXT
           END-IF.
      *
      *    診療区分
           IF      (TNS-SRYKBN            =   "90")  OR
                   (TNS-SRYKBN            =   "99")
               MOVE    SPACE               TO  SPA-ERRCD
           ELSE
               MOVE    "1020"              TO  SPA-ERRCD
               IF      TNS-SRYCD(1:2)        =   "10"
                   MOVE    SPACE               TO  SPA-ERRCD
               END-IF
               IF      TNS-SRYCD(1:6)        =   "788888"
                   MOVE    SPACE               TO  SPA-ERRCD
               END-IF
               IF      TNS-SRYCD(1:1)        =   "8"
                   MOVE    SPACE               TO  SPA-ERRCD
               END-IF
           END-IF.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "1"                 TO  SPA-COM-CUR (IDX-T)
               GO      TO      31011-TEN-HENKAN-EXT
           END-IF.
      *
           MOVE    WRK-TEN-SRYCD       TO  SPA-COM-INPUTCD (IDX-T).
      *
      *    最終行判定
           MOVE    ZERO                TO  WRK-MAX.
           PERFORM VARYING     IDX     FROM    200   BY  -1
                   UNTIL       IDX     <   1
               IF     (SPA-GMN-INPUTCD (IDX)   NOT =   SPACE) OR
                      (SPA-COM-INPUTCD (IDX)   NOT =   SPACE)
                   MOVE    IDX             TO  WRK-MAX
                   MOVE    1               TO  IDX
               END-IF
           END-PERFORM.
      *
      *    時間外加算チェック
           IF      SPA-COM-TIMEFLG(IDX-T)  NOT =   ZERO
               IF      TNS-TIMEKSNKBN          =   ZERO
                   MOVE    "0030"              TO  SPA-ERRCD
      *        診療の時、手技料ならＯＫとする（現在、時間加算区分＝０）
                   IF    ((SPA-COM-INPUTCD (IDX-T) (2:2)
                                                   =   "11"  OR
                                                       "12")  AND
                          (TNS-DATAKBN             =   "1" ))  OR
      *               小児科外来診療料の時、時間外ありとする
                          (SPA-COM-INPUTCD(IDX-T)  =   "113003610"  OR
                                                       "113003510"  OR
                                                       "113003810"  OR
                                                       "113003710")  OR
      *         労災の時、診察料ならＯＫとする
                         ((SPA-HKNKBN          =   1    )  AND
                          (SPA-COM-INPUTCD(IDX-T)  =   "101110010" OR
                                                       "101120010" OR
                                                       "101120040" OR
                                                       "101120050" OR
                                                       "101120060"))
                       MOVE     SPACE              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF.
      *
      *    注加算コード、注加算連番チェック
           IF     (TNS-CHUKSNCD        =   ZERO  OR  SPACE)  OR
                  (TNS-CHUKSNTUBAN     =   ZERO  OR  SPACE)
               CONTINUE
           ELSE
      *        追加挿入の時のチェックとする
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   WRK-MAX
                   IF      IDX         NOT =   IDX-T
                       IF      (SPA-COM-CHUKSNCD   (IDX)
                                                   =  TNS-CHUKSNCD) AND
                               (SPA-COM-CHUKSNTUBAN(IDX)
                                                   =   TNS-CHUKSNTUBAN)
                           MOVE    "0042"          TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-PERFORM
           END-IF.
      *
      *    同一剤内に時間加算は１つのみ
           IF     (TNS-TIMEKSNKBN      NOT =   ZERO)  AND
                  (TNS-DATAKBN             =   "2")
      *
               MOVE    ZERO                TO  WRK-TIME
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   WRK-MAX          )  OR
                                  (SPA-ERRCD       NOT =   SPACE)
                   IF      IDX             NOT =   IDX-T
                       IF     (SPA-COM-DATAKBN   (IDX)   =   "2") AND
                              (SPA-COM-TIMEKSNKBN(IDX) NOT =   ZERO)
                           ADD     1               TO  WRK-TIME
                           IF      WRK-TIME        >   1
                               MOVE    "0042"          TO  SPA-ERRCD
                           END-IF
                       END-IF
                   END-IF
      *
                   IF      SPA-COM-ZAIEND (IDX)    =   "1"
                       MOVE    ZERO                TO  WRK-TIME
                   END-IF
               END-PERFORM
           END-IF.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "1"                 TO  SPA-COM-CUR (IDX-T)
               GO      TO      31011-TEN-HENKAN-EXT
           END-IF.
      *
      *    ＳＰＡ編集
      *    基礎点数
           MOVE    TNS-TEN             TO  SPA-COM-TEN      (IDX-T).
           MOVE    TNS-TEN             TO  SPA-COM-KISOTEN  (IDX-T).
      *
           IF      SPA-HKNKBN          =   1
      *        労災・自賠責 編集
               PERFORM 31011-TEN-RSIJIB-HEN-SEC
           END-IF.
      *
           MOVE    TNS-NAME      TO  SPA-GMN-MEISYO   (IDX-T).
      *
      *    診療科名埋め込み再編集
      *    初診料算定科、再診料算定科のみ対象とする
           IF     (SPA-COM-INPUTCD (IDX-T) =   "830000020"  OR
                                               "830000021")  AND
                  (SPA-COM-ATAI1   (IDX-T)     NOT =   SPACE)
                PERFORM 31011-TEN-INPUT-MEISYO2-SEC
                MOVE    WRK-MEISYO    TO  SPA-GMN-MEISYO (IDX-T)
           END-IF.
      *
           MOVE    SPACE               TO  SPA-GMN-TANINAME (IDX-T).
      *
      *    診療種別区分
           IF      SPA-COM-INPUTCD (IDX-T)(1:1)    =   "1"
               MOVE    TNS-SRYSYUKBN   TO  SPA-COM-TNSSRYSYUKBN(IDX-T)
               MOVE    TNS-SRYSYUKBN   TO  SPA-COM-SRYSYUKBN   (IDX-T)
      *        画像診断の時、すべて"700"とする
               EVALUATE    TNS-SRYSYUKBN(1:1)
                   WHEN    "7"
                       MOVE    "700"       TO  SPA-COM-SRYSYUKBN(IDX-T)
               END-EVALUATE
           END-IF.
      *    データ区分
           MOVE    TNS-DATAKBN         TO  SPA-COM-DATAKBN    (IDX-T).
      *    診療区分
           MOVE    TNS-SRYKBN          TO  SPA-COM-TNSSRYKBN  (IDX-T).
      *    医薬品　項目
           MOVE    TNS-MADOKUKBN       TO  SPA-COM-MADOKUKBN  (IDX-T).
           MOVE    TNS-SINKEIHAKAIKBN  TO  SPA-COM-SINKEIKBN  (IDX-T).
           MOVE    TNS-SEIBUTUGAKUKBN  TO  SPA-COM-SEIBUTUKBN (IDX-T).
           MOVE    TNS-ZOEIZAIKBN      TO  SPA-COM-ZOEIZAIKBN (IDX-T).
           MOVE    TNS-CSYYOURYO       TO  SPA-COM-CSYYOURYO  (IDX-T).
      *    単位コード
           MOVE    TNS-TANICD          TO  SPA-COM-TANICD     (IDX-T).
      *    薬剤区分
           MOVE    TNS-YKZKBN          TO  SPA-COM-YKZKBN     (IDX-T).
      *    剤型区分
           MOVE    TNS-ZAIGATAKBN      TO  SPA-COM-ZAIGATAKBN (IDX-T).
      *    部位区分
           MOVE    TNS-BUIKBN          TO  SPA-COM-BUIKBN     (IDX-T).
      *    検査等実施判断区分
           MOVE    TNS-KNSJISKBN       TO  SPA-COM-KNSJISKBN  (IDX-T).
      *    検査等実施判断グループ
           MOVE    TNS-KNSJISGRPKBN    TO  SPA-COM-KNSJISGRPKBN
                                                              (IDX-T).
      *    注加算コード
           MOVE    TNS-CHUKSNCD        TO  SPA-COM-CHUKSNCD   (IDX-T).
      *    注加算通番
           MOVE    TNS-CHUKSNTUBAN     TO  SPA-COM-CHUKSNTUBAN
                                                              (IDX-T).
      *    通則加算対象外区分
           MOVE    TNS-TUSOKUGAIKBN    TO  SPA-COM-TUSOKUGAIKBN
                                                              (IDX-T).
      *    通則年齢区分
           MOVE    TNS-TSUSOKUAGE      TO  SPA-COM-TSUSOKUAGE (IDX-T).
      *    時間加算区分
           MOVE    TNS-TIMEKSNKBN      TO  SPA-COM-TIMEKSNKBN (IDX-T).
      *    外来管理加算区分
           MOVE    TNS-GAIKANRIKBN     TO  SPA-COM-GAIKANRIKBN
                                                              (IDX-T).
      *    検査用　区分番号
           MOVE    TNS-CDKBN-KBN       TO  SPA-COM-CDKBN-KBN  (IDX-T).
           MOVE    TNS-CDKBN-SYO       TO  SPA-COM-CDKBN-SYO  (IDX-T).
           MOVE    TNS-CDKBN-BU        TO  SPA-COM-CDKBN-BU   (IDX-T).
      *    労災用　区分番号
           MOVE    TNS-CDKBN-KBNNUM    TO  SPA-COM-CDKBN-KBNNUM
                                                              (IDX-T).
      *    検査用　包括対象検査
           MOVE    TNS-HOUKNSKBN       TO  SPA-COM-HOUKNSKBN  (IDX-T).
      *    算定履歴区分
           MOVE    TNS-SANTEIRRKKBN    TO  SPA-COM-SANTEIRRKKBN
                                                              (IDX-T).
      *    指導管理料
           MOVE    TNS-SDOKNRYO        TO  SPA-COM-SDOKNRYO   (IDX-T).
      *    点数識別
           MOVE    TNS-TENSIKIBETU     TO  SPA-COM-TENSIKIBETU
                                                              (IDX-T).
      *    特定器材年齢加算区分
           MOVE    TNS-TOKUKIZAIAGEKSNKBN
                                       TO  SPA-COM-TOKUKIZAIAGEKSNKBN
                                                              (IDX-T).
      *    酸素等区分
           MOVE    TNS-SANSOKBN        TO  SPA-COM-SANSOKBN   (IDX-T).
      *    特定器材種別１
           MOVE    TNS-TOKUKIZAISBT1   TO  SPA-COM-TOKUKIZAISBT1
                                                              (IDX-T).
      *    上限点数（器材の上限）
           MOVE    TNS-JGNTEN          TO  SPA-COM-JGNTEN     (IDX-T).
      *    保険適用区分
           MOVE    TNS-HKNTEKKBN       TO  SPA-COM-HKNTEKKBN  (IDX-T).
      *    点数欄集計先識別
           MOVE    TNS-GAITENTTLKBN    TO  SPA-COM-GAITENTTLKBN
                                                              (IDX-T).
      *    点数欄集計先識別
           MOVE    TNS-NYUTENTTLKBN    TO  SPA-COM-NYUTENTTLKBN
                                                              (IDX-T).
      *    履歴算定用診療行為コード
           MOVE    TNS-SRYCD           TO  SPA-COM-CHK-SRYCD  (IDX-T).
      *    きざみ値計算識別判定
           MOVE    TNS-KZMCOMPSIKIBETU TO  SPA-COM-KZMCOMPSIKIBETU
                                                              (IDX-T).
      *    きざみ値計上限
           MOVE    TNS-KZMJGN          TO  SPA-COM-KZMJGN     (IDX-T).
      *    検査等実施判定区分
           MOVE    TNS-HOUKATUTEIGENKBN    TO  SPA-COM-HOUKATUTEIGENKBN
                                                              (IDX-T).
      *    年齢加算
           MOVE    TNS-AGEKSNTBL(1)    TO  SPA-COM-AGEKSNTBL(IDX-T 1).
           MOVE    TNS-AGEKSNTBL(2)    TO  SPA-COM-AGEKSNTBL(IDX-T 2).
           MOVE    TNS-AGEKSNTBL(3)    TO  SPA-COM-AGEKSNTBL(IDX-T 3).
           MOVE    TNS-AGEKSNTBL(4)    TO  SPA-COM-AGEKSNTBL(IDX-T 4).
      *    往診区分
           MOVE    TNS-OSINKBN         TO  SPA-COM-OSINKBN    (IDX-T).
      *
      *    レセプト用実日数（実日数＝２の日数・回数をセット）
           IF      TNS-JITUDAY         =   2
               MOVE    TNS-DAYCNT      TO  SPA-COM-DAYCNT (IDX-T)
           END-IF.
      *
      *    逓減検査区分
           MOVE    TNS-TEIGENKBN       TO  SPA-COM-TEIGENKBN (IDX-T).
      *
      *    後発医薬品区分
           MOVE    TNS-KOUHATUKBN      TO  SPA-COM-KOUHATUKBN (IDX-T).
      *    基準不適合逓減区分
           MOVE    TNS-KIJUNTEIGENKBN  TO  SPA-COM-KIJUNTEIGENKBN
                                                               (IDX-T).
      *    基準不適合逓減対象施設基準
           MOVE    TNS-KIJUNTEIGENCD   TO  SPA-COM-KIJUNTEIGENCD
                                                               (IDX-T).
      *
      *    点数マスタ付加情報より編集
           MOVE    SPACE               TO  TENSUPLUS-REC.
           INITIALIZE                      TENSUPLUS-REC.
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD.
           MOVE    TNS-YUKOSTYMD       TO  TNSP-YUKOSTYMD.
           MOVE    TNS-YUKOEDYMD       TO  TNSP-YUKOEDYMD.
      *
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "TENSUPLUS-KEY"     TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "TENSUPLUS-KEY"     TO  ORC-DBPATH
               PERFORM 980-TENSUPLUS-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSUPLUS
               INITIALIZE                      TENSUPLUS-REC
           END-IF.
      *
      *    採血料区分
           MOVE    TNSP-SAIKETUKBN     TO  SPA-COM-SAIKETUKBN (IDX-T).
      *    入力チェック区分
           MOVE    TNSP-INPCHKKBN      TO  SPA-COM-INPCHKKBN  (IDX-T).
      *
      *    入力コードの主表示コードに編集
           IF      SPA-COM-CHKFLG (IDX-T)  =   SPACE
               MOVE    SPACE           TO  SPA-COM-ICD-INPUTCD(IDX-T)
      *
               INITIALIZE                      INPUTCD-REC
               MOVE    SPA-HOSPID      TO  ICD-HOSPID
               MOVE    SPA-COM-INPUTCD(IDX-T)
                                       TO  ICD-SRYCD
      *
               MOVE    INPUTCD-REC     TO  MCPDATA-REC
               MOVE    "DBSELECT"      TO  MCP-FUNC
               MOVE    "INPUTCD-KEY4"  TO  ORC-DBPATH
               CALL    "ORCMCPSUB"     USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    "INPUTCD-KEY4" TO  ORC-DBPATH
                   PERFORM 920-INPUTCD-NEXT-SEC
               ELSE
                   MOVE    1              TO  FLG-INPUTCD
               END-IF
      *
               IF      FLG-INPUTCD         =   ZERO
                   MOVE    ICD-INPUTCD  TO  SPA-COM-ICD-INPUTCD(IDX-T)
               END-IF
           END-IF.
      *
      *    併用算定チェック（算定回数がＯＫの時）
           IF     (SPA-COM-CHKFLG (IDX-T)  =   SPACE) AND
                  (SPA-ERRCD               =   SPACE) AND
                  (WRK-TEN-SRYCD(1:1)      =   "1"  )
               PERFORM 31011-TEN-HEIYOU-CHK-SEC
           END-IF.
      *
      *    基本チェック済フラグ
           IF      SPA-ERRCD               =   SPACE
               MOVE    "1"             TO  SPA-COM-CHKFLG (IDX-T)
           ELSE
               MOVE    SPACE           TO  SPA-COM-CHKFLG (IDX-T)
               MOVE    "1"             TO  SPA-COM-CUR (IDX-T)
           END-IF.
      *
       31011-TEN-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人一般コード変換処理
      *****************************************************************
       31011-TEN-SRYCDCHG-HEN-SEC        SECTION.
      *
           INITIALIZE                  SRYCDCHG-REC.
      *    コード変換読込
           EVALUATE    TNS-ROUTEKKBN
      *        一般
               WHEN    1
                   MOVE    TNS-SRYCD           TO  CHG-IPNSRYCD
                   MOVE    "SRYCDCHG-KEY2"     TO  WRK-PATH
               WHEN    2
                   MOVE    TNS-SRYCD           TO  CHG-RJNSRYCD
                   MOVE    "SRYCDCHG-KEY3"     TO  WRK-PATH
           END-EVALUATE.
           MOVE    SRYCDCHG-REC        TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    WRK-PATH            TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    WRK-PATH            TO  ORC-DBPATH
               PERFORM 990-SRYCDCHG-READ-SEC
           ELSE
               INITIALIZE                  SRYCDCHG-REC
               MOVE    1               TO  FLG-SRYCDCHG
           END-IF.
      *
           INITIALIZE                  WRK-SRYCD-CHG-AREA.
           MOVE    ZERO                TO  IDX.
           PERFORM UNTIL     FLG-SRYCDCHG      =   1
               ADD     1               TO  IDX
               IF      TNS-ROUTEKKBN   =   1
                   MOVE    CHG-RJNSRYCD    TO  WRK-SRYCDCHG (IDX)
               ELSE
                   MOVE    CHG-IPNSRYCD    TO  WRK-SRYCDCHG (IDX)
               END-IF
      *
               MOVE    WRK-PATH            TO  ORC-DBPATH
               PERFORM 990-SRYCDCHG-READ-SEC
           END-PERFORM.
      *
           IF      IDX                 =   ZERO
               MOVE    "0015"          TO  SPA-ERRCD
               GO      TO      31011-TEN-SRYCDCHG-HEN-EXT
           END-IF.
      *
      *    点数マスタ編集
           MOVE    WRK-SRYCDCHG (01)   TO  TNS-SRYCD.
           PERFORM 910-TENSU-READ-SEC.
           IF      FLG-TENSU           =   1
               MOVE    "0015"              TO  SPA-ERRCD
               MOVE    "1"                 TO  SPA-COM-CUR (IDX-T)
               GO      TO      31011-TEN-SRYCDCHG-HEN-EXT
           END-IF.
      *
           MOVE    WRK-SRYCDCHG (01)   TO  SPA-COM-INPUTCD (IDX-T).
      *    画面入力値の入れ替え
           MOVE    ZERO                TO  WRK-CD-MCNT.
      *    画面再表示
      *     文字列調査の開始位置　取得
           IF  (SPA-GMN-INPUTCD(IDX-T)(1:1)    IS  NUMERIC) AND
               (SPA-GMN-INPUTCD(IDX-T)(2:1)    =   SPACE  ) AND
               (SPA-GMN-INPUTCD(IDX-T)(3:1)    NOT =   SPACE)
      *        時間加算あり
               MOVE    3                   TO  WRK-IDX-FT
           ELSE
               MOVE    1                   TO  WRK-IDX-FT
           END-IF.
           PERFORM VARYING     IDX     FROM    WRK-IDX-FT  BY  1
                   UNTIL      (IDX         >   22  )   OR
                              (SPA-GMN-INPUTCD(IDX-T)(IDX:1)
                                           =   SPACE   OR  "*")
               MOVE    IDX                 TO  WRK-CD-MCNT
           END-PERFORM.
           ADD     1                   TO  WRK-CD-MCNT.
           MOVE    WRK-TEN-SRYCD       TO  SPA-GMN-INPUTCD (IDX-T)
                                                   (1:9).
           MOVE    SPA-GMN-INPUTCD(IDX-T)(WRK-CD-MCNT:)
                                       TO  SPA-GMN-INPUTCD (IDX-T)
                                                   (10:).
      *
       31011-TEN-SRYCDCHG-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    併用算定チェック処理
      *****************************************************************
       31011-TEN-HEIYOU-CHK-SEC        SECTION.
      *
      *    チェックテーブル検索
           INITIALIZE                  CHK-REC.
           MOVE    "5"                 TO  CHK-CHKKBN.
           MOVE    WRK-TEN-SRYCD       TO  CHK-SRYCD.
           MOVE    "2"                 TO  CHK-CDKBN.
           MOVE    ZERO                TO  CHK-RENNUM.
      *
           MOVE    ZERO                TO  FLG-CHKMST.
           MOVE    CHK-REC             TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "CHK-KEY3"          TO  ORC-DBPATH.
           MOVE    SPA-SRYYMD          TO  ORC-DBYMD.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "CHK-KEY3"          TO  ORC-DBPATH
               PERFORM 950-CHK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-CHKMST
           END-IF.
           PERFORM UNTIL     (FLG-CHKMST       =   1     )  OR
                             (SPA-ERRCD     NOT =   SPACE)
               PERFORM VARYING     IDX-C       FROM    1   BY  1
                       UNTIL      (IDX-C            >   10   )  OR
                                  (CHK-CD (IDX-C)   =   SPACE)  OR
                                  (SPA-ERRCD    NOT =   SPACE)
      *            入力値とのチェック
                   PERFORM 31011-TEN-CHK-INPUT-SEC
               END-PERFORM
      *
               MOVE    "CHK-KEY3"          TO  ORC-DBPATH
               PERFORM 950-CHK-READ-SEC
           END-PERFORM.
      *
       31011-TEN-HEIYOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力値とのチェック処理
      *****************************************************************
       31011-TEN-CHK-INPUT-SEC            SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   WRK-MAX)  OR
                              (SPA-ERRCD   NOT =   SPACE  )
               IF      IDX                 =   IDX-T
                   CONTINUE
               ELSE
                   IF      SPA-COM-INPUTCD(IDX)  =   CHK-CD (IDX-C)
                       MOVE    "0033"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-PERFORM.
      *
       31011-TEN-CHK-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *     労災・自賠責 編集処理
      *****************************************************************
       31011-TEN-RSIJIB-HEN-SEC     SECTION.
      *
           IF      SPA-COM-INPUTCD (IDX-T)(1:1)    =   "1"
      *        労災のコード
               IF      SPA-COM-INPUTCD (IDX-T)(2:2)    =   "01"
                   MOVE    1               TO  SPA-COM-RSIKBN (IDX-T)
               END-IF
      *        金額を点数へ置換える
               IF      TNS-TENSIKIBETU     =   1
                   COMPUTE SPA-COM-KISOTEN  (IDX-T)    =   TNS-TEN
                                                       /   10
               END-IF
           END-IF.
      *    労災（器材）
           IF     (SPA-COM-INPUTCD (IDX-T)(1:1)    =   "7"    )  AND
                  (SPA-COM-INPUTCD (IDX-T)(2:5)    =   "88888")
               MOVE    1               TO  SPA-COM-RSIKBN (IDX-T)
           END-IF.
      *    労災（コメント）
           IF     (SPA-COM-INPUTCD (IDX-T)(1:1)    =   "8")  AND
                  (SPA-COM-INPUTCD (IDX-T)(4:1)    =   "8")
               MOVE    1               TO  SPA-COM-RSIKBN (IDX-T)
           END-IF.
      *
       31011-TEN-RSIJIB-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     入力値コメントの時、再編集処理
      *****************************************************************
       31011-TEN-INPUT-MEISYO2-SEC     SECTION.
      *
           MOVE    SPACE               TO  WRK-MEISYO.
           MOVE    TNS-NAME            TO  WRK-MEISYO.
           INITIALIZE                  WRK-INPUT-SET-AREA.
      *
      *    施設基準コード（入力値編集）
           PERFORM VARYING IDX-SST     FROM    1   BY  1
                   UNTIL   IDX-SST     >   8
               MOVE    TNS-SSTKIJUNCDTBL(IDX-SST)
                                      TO  WRK-INPUT-SET (IDX-SST)
           END-PERFORM.
      *
      *    診療科名編集へ
           PERFORM VARYING IDX-SST     FROM    1   BY  1
                   UNTIL  (IDX-SST         >   4     )   OR
                          (WRK-INPUT-KETA (IDX-SST)  =   ZERO)
               IF      SPA-COM-ATAI(IDX-T IDX-SST)   =   SPACE
                   CONTINUE
               ELSE
                   MOVE    WRK-INPUT-KETA (IDX-SST)    TO  WRK-KETA
                   MOVE    WRK-INPUT-ICHI (IDX-SST)    TO  WRK-ICHI
                   COMPUTE WRK-KETA        =   WRK-KETA  *   2
                   COMPUTE IDX-K           =   (10       -   WRK-KETA)
                                           +  1
                   COMPUTE WRK-ICHI2       =   (WRK-ICHI     *  2  )
                                           -  1
                   PERFORM 31011-TEN-SRYKA-MEI-HEN-SEC
               END-IF
           END-PERFORM.
      *
       3101-TEN-INPUT-MEISYO2-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療科名称編集処理
      *****************************************************************
       31011-TEN-SRYKA-MEI-HEN-SEC       SECTION.
      *
      *    診療科
           MOVE    SPACE               TO  SYS-1005-REC.
           INITIALIZE                  SYS-1005-REC.
           MOVE    "1005"              TO  SYS-1005-KANRICD.
           MOVE    SPA-COM-ATAI(IDX-T IDX-SST)(1:2)
                                       TO  SYS-1005-KBNCD (1:2).
           MOVE    ZERO                TO  SYS-1005-STYUKYMD.
           MOVE    ALL "9"             TO  SYS-1005-EDYUKYMD.
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH.
           MOVE    SPA-SRYYMD          TO  ORC-DBYMD.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1005-REC
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF.
           MOVE    SPACE               TO  WRK-MEISYO (WRK-ICHI2:)
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC           TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME1   TO  WRK-MEISYO
                                                   (WRK-ICHI2:)
           END-IF.
      *
       31011-TEN-SRYKA-MEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 450-MAEGMN-SEC
      *    次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 460-ATOGMN-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                    PERFORM 4100-TOROKU-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    入力個所のセット
           PERFORM 210-GMN-INPUT-SEC.
      *
      *    入力チェック処理
           PERFORM 220-INPUT-CHK-SEC.
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       210-GMN-INPUT-SEC               SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-CUR4.
      *    入力項目の決定
           PERFORM 2001-INPUT-SET-SEC.
      *
      *    画面をＳＰＡへ編集
           PERFORM 2002-GMNSPA-SET-SEC.
      *
       210-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目の決定
      *****************************************************************
       2001-INPUT-SET-SEC               SECTION.
      *
      *    入力行NOの設定
           MOVE    ZERO                TO  WRK-IDX2.
           IF      MCP-WIDGET(1:7)     =   "INPUTCD"
               MOVE    MCP-WIDGET(8:2)     TO  WRK-IDX2
           END-IF.
           IF      MCP-WIDGET(1:6)     =   "MEISYO"
               MOVE    MCP-WIDGET(7:2)     TO  WRK-IDX2
           END-IF.
      *
           MOVE    ZERO                TO  SPA-GMN-IDX4.
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   200
               IF     (SPA-COM-PAGE (IDX)  =   SPA-GMN-PAGE4)  AND
                      (SPA-COM-GYOU (IDX)  =   WRK-IDX2     )
                   MOVE    IDX                 TO  SPA-GMN-IDX4
                   MOVE    200                 TO  IDX
               END-IF
           END-PERFORM.
      *
      *    同一行がない時、計算で求める
           IF      SPA-GMN-IDX4        =   ZERO
               COMPUTE SPA-GMN-IDX4    =   (SPA-GMN-PAGE4   -   1)
                                       *   20
                                       +   WRK-IDX2
           END-IF.
      *
       2001-INPUT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面項目→ＳＰＡ編集処理
      *****************************************************************
       2002-GMNSPA-SET-SEC               SECTION.
      *
      *    入力コード入力時、カーソルの保存
           IF     (MCP-EVENT           =   "ACTIVATE") AND
                  (MCP-WIDGET(1:7)     =   "INPUTCD" )
               MOVE    WRK-IDX2            TO  WRK-IN-MAP-IDX
           ELSE
               MOVE    ZERO                TO  WRK-IN-MAP-IDX
           END-IF.
      *
      *    カーソル位置のクリア等
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   200
               MOVE    SPACE               TO  SPA-COM-CUR (IDX)
               MOVE    SPACE               TO  SPA-COM-IN  (IDX)
               MOVE    SPACE               TO  SPA-COM-IN2 (IDX)
           END-PERFORM.
      *
           MOVE    ZERO                TO  WRK-STR.
           MOVE    ZERO                TO  FLG-KUHAKU.
           PERFORM VARYING     WRK-IDX2    FROM    1   BY  1
                   UNTIL       WRK-IDX2    >   20
      *        ＳＰＡ内の行決定
               PERFORM 41011-GYOU-SET-SEC
               IF     ((I44-INPUTCD (WRK-IDX2)  NOT =
                                   SPA-GMN-INPUTCD (SPA-GMN-IDX4)) OR
                       (I44-MEISYO  (WRK-IDX2)   NOT =
                                   SPA-GMN-MEISYO-G (SPA-GMN-IDX4))) AND
                      (MCP-EVENT           =   "ACTIVATE") AND
                      (MCP-WIDGET(1:7)     =   "INPUTCD" )
                   MOVE    "1"                 TO  SPA-COM-IN2
                                                          (SPA-GMN-IDX4)
               END-IF
      *
               MOVE    I44-INPUTCD (WRK-IDX2)  TO  SPA-GMN-INPUTCD
                                                          (SPA-GMN-IDX4)
               MOVE    I44-MEISYO  (WRK-IDX2)  TO  SPA-GMN-MEISYO-G
                                                          (SPA-GMN-IDX4)
      *        入力行の保存
               IF     (WRK-IN-MAP-IDX  NOT =   ZERO )  AND
                      (WRK-IDX2            =   WRK-IN-MAP-IDX)
                   MOVE    "1"                 TO  SPA-COM-IN
                                                          (SPA-GMN-IDX4)
                   IF     (I44-INPUTCD (WRK-IDX2)  =   SPACE)  AND
                          (SPA-MAE-INPUTCD (SPA-GMN-IDX4)
                                                   =   SPACE)  AND
                          (SPA-COM-INPUTCD (SPA-GMN-IDX4)
                                                   =   SPACE)  AND
                          (SPA-GMN-IDX4            >   SPA-GMN-MAX)
                       MOVE    1                   TO  FLG-KUHAKU
                   END-IF
               END-IF
      *
               IF      I44-INPUTCD (WRK-IDX2)  NOT =   SPACE
                   IF      WRK-STR             =   ZERO
                       MOVE    SPA-GMN-IDX4        TO  WRK-STR
                   END-IF
               END-IF
           END-PERFORM.
      *
       2002-GMNSPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       41011-GYOU-SET-SEC               SECTION.
      *    行決定
           MOVE    ZERO                TO  SPA-GMN-IDX4.
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   200
               IF     (SPA-COM-PAGE (IDX)  =   SPA-GMN-PAGE4)  AND
                      (SPA-COM-GYOU (IDX)  =   WRK-IDX2     )
                   MOVE    IDX                 TO  SPA-GMN-IDX4
                   MOVE    200                 TO  IDX
               END-IF
           END-PERFORM.
      *    同一行がない時、計算で求める
           IF      SPA-GMN-IDX4        =   ZERO
               COMPUTE SPA-GMN-IDX4    =   (SPA-GMN-PAGE4  -   1)
                                       *   20
                                       +   WRK-IDX2
           END-IF.
      *
       41011-GYOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       220-INPUT-CHK-SEC           SECTION.
      *
      *    画面ＳＰＡチェック処理
           MOVE    SPACE           TO  FLG-TOROKU.
           PERFORM 410112-KOUI-SPACHK-SEC.
      *
       220-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡチェック処理
      *****************************************************************
       410112-KOUI-SPACHK-SEC          SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR4.
      *
      *    変換・削除処理
           MOVE    WRK-STR             TO  SPA-GMN-IDX4.
           MOVE    ZERO                TO  WRK-IDX2.
           MOVE    ZERO                TO  FLG-INS.
           PERFORM VARYING SPA-GMN-IDX4    FROM    WRK-STR  BY  1
                   UNTIL   SPA-GMN-IDX4    >   200
               IF     (SPA-MAE-INPUTCD (SPA-GMN-IDX4)  NOT =
                                      SPA-GMN-INPUTCD (SPA-GMN-IDX4)) OR
                      (SPA-COM-IN2 (SPA-GMN-IDX4)   =   "1")
                   PERFORM 41011-KOUI-HENDEL-SEC
               END-IF
           END-PERFORM.
           IF      FLG-INS             =   1
      *        空白行をなくす
               PERFORM 420-SYOKYO-SEC
           END-IF.
      *
      *    入力のチェック
           MOVE    WRK-STR             TO  SPA-GMN-IDX4.
           MOVE    ZERO                TO  FLG-INS.
           PERFORM UNTIL      (SPA-GMN-IDX4    >   200  )  OR
                              (SPA-ERRCD   NOT =   SPACE)  OR
                              (FLG-END     NOT =   ZERO )
               IF     (SPA-MAE-INPUTCD (SPA-GMN-IDX4)  NOT =
                                      SPA-GMN-INPUTCD (SPA-GMN-IDX4)) OR
                      (SPA-COM-IN2 (SPA-GMN-IDX4)   =   "1")
                   PERFORM 41011-KOUI-HEN2-SEC
               END-IF
      *
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE     SPA-GMN-MEISYO (SPA-GMN-IDX4)
                                              TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF     (KANACHK-RC      NOT =   ZERO)  OR
                      (KANACHK-SYUBETU     =   1)
                   MOVE    "0080"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  SPA-COM-CUR (IDX-T)
               END-IF
      *
               IF     (SPA-ERRCD       =   SPACE)  AND
                      (FLG-END         =   ZERO )
                   ADD     1                   TO  SPA-GMN-IDX4
               END-IF
           END-PERFORM.
      *
           IF     (SPA-ERRCD       NOT =   SPACE)  OR
                  (FLG-END             =   1    )
               IF      FLG-END         =   1
      *            ＳＰＡの内容を画面へ編集する。カーソル設定なし
                   PERFORM 500-GMNHEN-SEC
               END-IF
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF.
      *    行挿入、警告あり
           IF      FLG-INS             =   1
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF.
      *
           IF      MCP-WIDGET    =   "B06"  OR  "B07"
               CONTINUE
           ELSE
      *        点数計算・剤別チェック処理
               PERFORM 510-TBLHEN-SEC
           END-IF.
           IF     (SPA-ERRCD       =   SPACE)  AND
                  (FLG-END         =   ZERO )
               CONTINUE
           ELSE
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF.
      *
      *    診療コード入力
           PERFORM 410119-SAIINPUT-SEC.
      *
       410112-KOUI-SPACHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面変換・削除処理
      *****************************************************************
       41011-KOUI-HENDEL-SEC               SECTION.
      *
           INITIALIZE                      WRK-MOJI-AREA.
      *
           MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX4) TO  WRK-HENKAN
           PERFORM 41011-KOUI-INPUTCD-SYORI-SEC.
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    WRK-HENKAN      TO  SPA-GMN-INPUTCD(SPA-GMN-IDX4)
               MOVE    ZERO            TO  SPA-GMN-INCNT  (SPA-GMN-IDX4)
               MOVE    SPACE           TO  SPA-COM-GMNFLG (SPA-GMN-IDX4)
               MOVE    "1"             TO  SPA-COM-CUR    (SPA-GMN-IDX4)
           END-IF.
      *
           IF     (SPA-COM-INPUTCD (SPA-GMN-IDX4)(1:2)    =   "84")  AND
                  (IDX-C1                             NOT =   ZERO)
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI
               MOVE     SPA-GMN-INPUTCD (SPA-GMN-IDX4)(IDX-C1:IDX-C2)
                                           TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF     KANACHK-RC          =   ZERO
                   COMPUTE   IDX-C2   =   IDX-C2   *   2   -   1
                   PERFORM VARYING     IDX     FROM    59   BY  -2
                           UNTIL       IDX     =   1
                       IF      SPA-GMN-MEISYO (SPA-GMN-IDX4)(IDX:2)
                                                                =   "　"
                           MOVE     KANACHK-OUT-INPUT (IDX-C2:2)
                                           TO  SPA-GMN-MEISYO
                                                   (SPA-GMN-IDX4)(IDX:2)
                           IF      IDX-C2    =   1
                               MOVE     1    TO  IDX
                           ELSE
                               COMPUTE   IDX-C2   =   IDX-C2   -   2
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-IF.
      *
      *    削除時
           IF      WRK-HENKAN(1:1)   =   SPACE
      *
      *        セットが削除の時セット内容を削除する
               IF      SPA-MAE-INPUTCD(SPA-GMN-IDX4)(1:1)   =   "S"
                   PERFORM 41016-SETDEL-SEC
               END-IF
               MOVE    SPACE           TO  SPA-GMN-INPUTCD(SPA-GMN-IDX4)
               MOVE    1               TO  FLG-INS
               MOVE    1               TO  FLG-MEI
               GO  TO                  41011-KOUI-HENDEL-EXT
           END-IF.
      *
      *    剤削除時
      *    '-'入力がある時、剤削除ができなので、エラーとする
           IF      WRK-HENKAN(1:1)   =   "-"
               MOVE    WRK-HENKAN      TO  SPA-GMN-INPUTCD(SPA-GMN-IDX4)
               MOVE    "1030"          TO  SPA-ERRCD
               MOVE    "1"             TO  SPA-COM-CUR    (SPA-GMN-IDX4)
               GO  TO                  41011-KOUI-HENDEL-EXT
           END-IF.
      *
       41011-KOUI-HENDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       41011-KOUI-HEN2-SEC               SECTION.
      *
           INITIALIZE                      WRK-MOJI-AREA.
      *
           MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX4)  TO  WRK-HENKAN
           PERFORM 41011-KOUI-INPUTCD-SYORI-SEC
      *
           MOVE    WRK-HENKAN         TO  SPA-GMN-INPUTCD(SPA-GMN-IDX4).
      *    '_'を削除した時、クリアする
           IF      WRK-JODOU      >   ZERO
               MOVE    "3"             TO  SPA-COM-IN  (SPA-GMN-IDX4)
           END-IF.
      *
      *    行挿入
           IF      WRK-HENKAN(1:1)   =   "+"
               MOVE     SPA-MAE-INPUTCD(SPA-GMN-IDX4)
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX4)
               PERFORM 41014-GYOINSN-SEC
               MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX4)
               ADD     1               TO  SPA-GMN-IDX4
               MOVE    1               TO  FLG-INS
               GO  TO                  41011-KOUI-HEN2-EXT
           END-IF.
      *    一覧へ
           IF      WRK-HENKAN(1:2)   =   "//"
               MOVE    SPA-MAE-INPUTCD(SPA-GMN-IDX4)
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX4)
               INITIALIZE                      SPA-I48-AREA
               INITIALIZE                      SPA-I44-I48-REC
               MOVE    WRK-HENKAN          TO  SPA-I48-INPUTCD
               PERFORM 410-I48-SYORI-SEC
               GO      TO                  41011-KOUI-HEN2-EXT
           END-IF.
      *    Ｐ入力へ
           IF      WRK-HENKAN(1:1)   =   "/"
               MOVE    SPA-MAE-INPUTCD(SPA-GMN-IDX4)
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX4)
               PERFORM 430-I49-SYORI-SEC
               GO      TO                  41011-KOUI-HEN2-EXT
           END-IF.
      *
           IF      FLG-NYURYOKU    =   1
      *        セットが変更削除の時セット内容を削除する
               IF      SPA-MAE-INPUTCD(SPA-GMN-IDX4)(1:1)   =   "S"
                   PERFORM 41016-SETDEL-SEC
               END-IF
               MOVE    SPA-COM-TIMEFLG(SPA-GMN-IDX4)   TO  WRK-TIMEFLG
               MOVE    SPA-COM-IN     (SPA-GMN-IDX4)   TO  WRK-FLG-IN
               MOVE    SPA-GMN-MEISYO-G(SPA-GMN-IDX4)  TO  WRK-MEISYO-G
               INITIALIZE                  SPA-GMN-TBLREC (SPA-GMN-IDX4)
               INITIALIZE                  SPA-COM-TBLREC (SPA-GMN-IDX4)
               MOVE    WRK-TIMEFLG     TO  SPA-COM-TIMEFLG(SPA-GMN-IDX4)
               MOVE    WRK-FLG-IN      TO  SPA-COM-IN  (SPA-GMN-IDX4)
               MOVE    "1"             TO  SPA-COM-IN2 (SPA-GMN-IDX4)
           ELSE
               MOVE    SPA-GMN-MEISYO-G(SPA-GMN-IDX4)   TO  WRK-MEISYO-G
           END-IF.
      *
           MOVE    WRK-HENKAN       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX4).
      *
      *    診療行為一覧へ
           IF      MCP-WIDGET    =   "B06"  OR  "B07"
               CONTINUE
           ELSE
               IF      WRK-KENSAKU    =   "1"
                   INITIALIZE                 SPA-I48-AREA
                   INITIALIZE                 SPA-I44-I48-REC
                   MOVE    WRK-CD         TO  SPA-I48-INPUTCD
                   MOVE    "1"            TO  SPA-I48-TYPE
                   MOVE    WRK-ICD-CDSYU  TO  SPA-I48-CDSYUBETU
                   MOVE    "1"            TO  SPA-COM-CUR (SPA-GMN-IDX4)
      *
      *            診療行為一覧へ
                   PERFORM 410-I48-SYORI-SEC
                   GO      TO                 41011-KOUI-HEN2-EXT
               END-IF
      *
      *        診療種別入力
               IF      (SPA-GMN-INPUTCD(SPA-GMN-IDX4)(1:1)  =   ".") AND
                       (SPA-GMN-INPUTCD(SPA-GMN-IDX4)(2:3)  NUMERIC)
                   INITIALIZE             SPA-GMN-TBLREC (SPA-GMN-IDX4)
                   INITIALIZE             SPA-COM-TBLREC (SPA-GMN-IDX4)
                   MOVE    WRK-HENKAN(1:4)   TO
                                          SPA-GMN-INPUTCD (SPA-GMN-IDX4)
                   MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX4)(2:3)
                                                   TO  WRK-SRYSYUKBN
      *            診療種別区分名称チェック
                   PERFORM 510-SRYKBN-MEI-SEC
               ELSE
      *            文字変換・編集処理
                   PERFORM 410112-INPUTCD-HENKAN-SEC
      *
               END-IF
           END-IF.
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX4)
                                       TO  SPA-MAE-INPUTCD(SPA-GMN-IDX4)
           ELSE
               MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX4)
           END-IF.
      *
       41011-KOUI-HEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療コード入力処理
      *****************************************************************
       410119-SAIINPUT-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-IN-IDX4.
           MOVE    ZERO                TO  WRK-IN-IDX2.
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   200
               IF      SPA-COM-IN  (IDX)   =   "1"
                   MOVE    IDX                 TO  WRK-IN-IDX4
               END-IF
               IF      SPA-COM-IN  (IDX)   =   "3"
                   MOVE    IDX                 TO  WRK-IN-IDX4
                   MOVE    IDX                 TO  WRK-IN-IDX2
               END-IF
           END-PERFORM.
      *
           IF      WRK-IN-IDX4         =   ZERO
               MOVE    SPA-GMN-MAX         TO  SPA-GMN-IDX4
           ELSE
               MOVE    WRK-IN-IDX4         TO  SPA-GMN-IDX4
           END-IF.
      *
           IF     WRK-MKBN     =   "1"
               MOVE    WRK-STR             TO  SPA-GMN-IDX4
           END-IF.
           MOVE    SPACE               TO  WRK-MKBN.
      *
           IF     FLG-MEI   NOT =   1
               IF     (SPA-COM-SURFLG (SPA-GMN-IDX4)  =   SPACE) AND
                      (SPA-COM-IN2    (SPA-GMN-IDX4)  =   "2"  ) AND
                      (SPA-COM-KAIFLG (SPA-GMN-IDX4)  =   SPACE) AND
                      (WRK-IN-IDX2                    =   ZERO )
                   MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX4)
                                                TO  WRK-INPUTCD
                   MOVE    "1"                  TO  SPA-COM-CUR
                                                         (SPA-GMN-IDX4)
                   IF      SPA-COM-INPUTCD (SPA-GMN-IDX4)(1:3)  =  "001"
                       MOVE    SPACE            TO  SPA-COM-CUR
                                                         (SPA-GMN-IDX4)
                   ELSE
                       STRING  WRK-INPUTCD      DELIMITED  BY  SPACE
                               "_"              DELIMITED  BY  SIZE
                                                INTO  SPA-GMN-INPUTCD
                                                       (SPA-GMN-IDX4)
                       END-STRING
                   END-IF
                   MOVE    SPACE                TO  SPA-ERRCD
               END-IF
      *        名称入力へのカーソル移動
               IF      WRK-IN-IDX4         >   ZERO
                   IF      SPA-COM-INPUTCD (SPA-GMN-IDX4)(1:2)
                                           =   "81"  OR  "83"  OR  "89"
                       MOVE    "3"        TO  SPA-COM-IN2 (SPA-GMN-IDX4)
                   END-IF
                   IF      SPA-COM-INPUTCD (SPA-GMN-IDX4)(1:2)
                                           =   "84"
                       MOVE    "3"        TO  SPA-COM-IN2 (SPA-GMN-IDX4)
                       MOVE    "1"        TO  SPA-COM-IN  (SPA-GMN-IDX4)
                   END-IF
               END-IF
           END-IF.
      *
      *    空白行入力時、前の行へ
           IF      FLG-KUHAKU          =   1
      *        空白行入力時、最終行へ
               MOVE    "1"                 TO  SPA-COM-CUR
                                                     (SPA-GMN-IDX4)
           END-IF.
      *
       410119-SAIINPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    文字列変換・編集処理
      *****************************************************************
       410112-INPUTCD-HENKAN-SEC      SECTION.
      *
      *    セット入力処理へ
           IF      SPA-GMN-INPUTCD(SPA-GMN-IDX4)(1:1)
                                           =   "S"  OR  "P"
               PERFORM 410119-INPSET-SEC
               GO      TO      410112-INPUTCD-HENKAN-EXT
           END-IF.
      *
      *    入力内容チェック、編集
           IF       FLG-NYURYOKU   =   1
               MOVE    WRK-CD          TO  SPA-COM-INPUTCD(SPA-GMN-IDX4)
           END-IF.
           PERFORM 4101113-INPUTCD-CHK-SEC.
      *    名称チェック
           PERFORM 40113-MEISYOU-CHK-SEC.
      *
       410112-INPUTCD-HENKAN-EXT.
           EXIT.
      *****************************************************************
      *    入力内容チェック、編集処理
      *****************************************************************
       4101113-INPUTCD-CHK-SEC       SECTION.
      *
      *    診療行為コードをもとに点数マスター検索
           MOVE    ZERO                TO  FLG-TENSU.
           MOVE    SPA-COM-INPUTCD(SPA-GMN-IDX4)
                                       TO  TNS-SRYCD.
      *
           PERFORM 910-TENSU-READ-SEC.
           IF      FLG-TENSU           =   1
               MOVE    "0001"              TO  SPA-ERRCD
               GO      TO      4101113-INPUTCD-CHK-EXT
           END-IF.
      *
           MOVE    SPACE               TO  SPA-COM-IN3 (SPA-GMN-IDX4).
      *
           IF     (SPA-COM-INPUTCD(SPA-GMN-IDX4)(1:2)   =   "84")  AND
                  (TNS-SSTKIJUNCDTBL(1)             NOT =   ZERO)
               MOVE    "2"            TO  SPA-COM-IN2 (SPA-GMN-IDX4)
               MOVE    "1"            TO  SPA-COM-IN3 (SPA-GMN-IDX4)
           END-IF.
      *
      *    数量・回数編集
           PERFORM 4101122-KOMOKU-HENSYU-SEC.
      *
           MOVE    SPA-GMN-IDX4        TO  WRK-TEN-GMN-IDX.
           MOVE    SPA-COM-INPUTCD (SPA-GMN-IDX4)
                                       TO  WRK-TEN-SRYCD.
           MOVE    SPA-NAI-NENREI      TO  WRK-TEN-NENREI.
           MOVE    SPA-ROUJIN          TO  WRK-TEN-ROUJIN.
           PERFORM 31011-TEN-HENKAN-SEC.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    SPACE           TO  SPA-COM-CHKFLG (SPA-GMN-IDX4)
               GO      TO      4101113-INPUTCD-CHK-EXT
           END-IF.
      *
      *    その他値の入力の有無
           IF     (WRK-HZN-ATAI1   =   SPA-COM-ATAI1 (SPA-GMN-IDX4)) AND
                  (WRK-HZN-ATAI2   =   SPA-COM-ATAI2 (SPA-GMN-IDX4)) AND
                  (WRK-HZN-ATAI3   =   SPA-COM-ATAI3 (SPA-GMN-IDX4)) AND
                  (WRK-HZN-ATAI3   =   SPA-COM-ATAI4 (SPA-GMN-IDX4))
               MOVE    ZERO                TO  FLG-IN-ATAI
           ELSE
               MOVE    1                   TO  FLG-IN-ATAI
           END-IF.
      *
           IF      SPA-COM-HKNTEKKBN(SPA-GMN-IDX4)  =   2
               IF      SPA-COM-KISOTEN (SPA-GMN-IDX4)  =   ZERO
                   MOVE    "1"             TO  SPA-COM-KEIFLG
                                                          (SPA-GMN-IDX4)
               ELSE
                   MOVE    "2"             TO  SPA-COM-KEIFLG
                                                          (SPA-GMN-IDX4)
               END-IF
           ELSE
               MOVE    SPACE           TO  SPA-COM-KEIFLG (SPA-GMN-IDX4)
           END-IF.
      *
           MOVE    TNS-SRYKBN          TO  SPA-COM-SRYKBN
                                                         (SPA-GMN-IDX4).
           MOVE    TNS-TEN             TO  SPA-COM-TENSU
                                                         (SPA-GMN-IDX4).
      *
       4101113-INPUTCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為名入力処理
      *****************************************************************
       40113-MEISYOU-CHK-SEC        SECTION.
      *
           IF      SPA-COM-INPUTCD (SPA-GMN-IDX4)(1:2)  =  "81"
               MOVE    WRK-MEISYO-G        TO  SPA-GMN-MEISYO-G
                                                         (SPA-GMN-IDX4)
               MOVE    "1"                 TO  SPA-COM-MEIFLG
                                                         (SPA-GMN-IDX4)
           END-IF.
           IF     (SPA-COM-INPUTCD (SPA-GMN-IDX4)(1:2)  = "83" OR "84")
               IF     (FLG-NYURYOKU        =   ZERO)  AND
                      (FLG-IN-ATAI         =   ZERO)
                   MOVE    WRK-MEISYO-G        TO  SPA-GMN-MEISYO-G
                                                         (SPA-GMN-IDX4)
               END-IF
               MOVE    "1"             TO  SPA-COM-MEIFLG
                                                       (SPA-GMN-IDX4)
           END-IF.
           IF     (SPA-COM-INPUTCD (SPA-GMN-IDX4)(1:2)  =  "81"
                                                        OR "83"
                                                        OR "84") AND
                  (SPA-GMN-MEISYO-G(SPA-GMN-IDX4) NOT =   SPACE)
               CONTINUE
           ELSE
               GO      TO      40113-MEISYOU-CHK-EXT
           END-IF.
      *
      *    半角の空白を全角に置きかえる
      *    フリーコメントは１桁目から
           IF      SPA-COM-INPUTCD(SPA-GMN-IDX4)   =   "810000001"
               MOVE    1                   TO  IDX-STR
           ELSE
               MOVE    3                   TO  IDX-STR
           END-IF.
           INITIALIZE                  ORCSKANACHKAREA.
           MOVE    "1"                 TO  KANACHK-SYORI.
           MOVE    SPA-GMN-MEISYO-G (SPA-GMN-IDX4)(IDX-STR:)
                                       TO  KANACHK-MAE-INPUT.
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA.
           IF     (KANACHK-RC      NOT =   ZERO)  OR
                  (KANACHK-SYUBETU     =   1   )
               MOVE    "0080"          TO  SPA-ERRCD
               MOVE    "3"             TO  SPA-COM-CUR (SPA-GMN-IDX4)
               MOVE    2               TO  SPA-GMN-CUR4
               MOVE    SPA-SCR-REC     TO  SPA-I44GMN-AREA
               GO      TO      40113-MEISYOU-CHK-EXT
           END-IF.
           MOVE    KANACHK-OUT-INPUT  TO  SPA-GMN-MEISYO-G(SPA-GMN-IDX4)
                                                            (IDX-STR:).
      *
       40113-MEISYOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｐ入力画面へ移行
      *****************************************************************
       430-I49-SYORI-SEC       SECTION.
      *
      *    画面編集
           INITIALIZE                  SPA-I49GMN-AREA.
           MOVE    SPACE               TO  SPA-I49-DATA.
      *
           MOVE    "I49"               TO  SPA-SAKIPG.
           MOVE    "I44"               TO  SPA-MOTOPG.
      *
           MOVE    "PIN"               TO  MCP-WIDGET.
           MOVE    "CHANGE"            TO  MCP-PUTTYPE.
           MOVE    "I49"               TO  MCP-WINDOW.
      *
           MOVE    SPA-I4KYOUTU        TO  SPA-WORK.
           MOVE    SPA-AREA            TO  SPA-COMMON.
           MOVE    SPA-I44-I48         TO  SPA-FREE.
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
       430-I49-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為一覧へ
      *****************************************************************
       410-I48-SYORI-SEC          SECTION.
      *
      *    内服を編集する
           INITIALIZE                  SPA-I48GMN-AREA.
           IF      SPA-I48-INPUTCD         =   SPACE
               MOVE    SPACE               TO  I48
               INITIALIZE                      I48
           ELSE
               INITIALIZE                  SPA-I48GMN-AREA
               MOVE    "8"                 TO  SPA-I48-GMN-SYORI
           END-IF.
      *
           MOVE    "I48"               TO  SPA-SAKIPG.
           MOVE    "I44"               TO  SPA-MOTOPG.
      *
           IF      SPA-I48-GMN-MAX     =   ZERO
               MOVE    "INPUT"             TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM1"           TO  MCP-WIDGET
           END-IF.
           MOVE    "CHANGE"            TO  MCP-PUTTYPE.
           MOVE    "I48"               TO  MCP-WINDOW.
      *
           MOVE    SPA-I4KYOUTU        TO  SPA-WORK.
           MOVE    SPA-AREA            TO  SPA-COMMON.
           MOVE    SPA-I44-I48         TO  SPA-FREE.
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
       410-I48-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目編集処理
      *****************************************************************
       4101122-KOMOKU-HENSYU-SEC      SECTION.
      *
      *    数量
           MOVE    1                   TO  SPA-COM-SURYO(SPA-GMN-IDX4).
           MOVE    SPACE               TO  SPA-COM-SURFLG(SPA-GMN-IDX4).
           MOVE    1                   TO  SPA-COM-SURYO2(SPA-GMN-IDX4).
      *
      *    回数（未入力時、１回）
           IF      SPA-COM-KAISU(SPA-GMN-IDX4)  =  ZERO
               MOVE    1               TO  SPA-COM-KAISU (SPA-GMN-IDX4)
           END-IF.
           MOVE    SPACE               TO  SPA-COM-KAIFLG(SPA-GMN-IDX4).
           MOVE    SPA-COM-KAISU(SPA-GMN-IDX4)
                                       TO  SPA-COM-KAISU2(SPA-GMN-IDX4).
      *
      *    分画数（未入力時、１回）
           MOVE    1                   TO  SPA-COM-BUNGSU(SPA-GMN-IDX4).
      *
      *    入力値の入力判定用
      *    その他値
           MOVE    SPA-COM-ATAI1 (SPA-GMN-IDX4) TO  WRK-HZN-ATAI1.
           MOVE    SPA-COM-ATAI2 (SPA-GMN-IDX4) TO  WRK-HZN-ATAI2.
           MOVE    SPA-COM-ATAI3 (SPA-GMN-IDX4) TO  WRK-HZN-ATAI3.
           MOVE    SPA-COM-ATAI4 (SPA-GMN-IDX4) TO  WRK-HZN-ATAI4.
      *
           MOVE    SPACE               TO  SPA-COM-ATAI1 (SPA-GMN-IDX4).
           MOVE    SPACE               TO  SPA-COM-ATAI2 (SPA-GMN-IDX4).
           MOVE    SPACE               TO  SPA-COM-ATAI3 (SPA-GMN-IDX4).
           MOVE    SPACE               TO  SPA-COM-ATAI4 (SPA-GMN-IDX4).
      *
       4101122-KOMOKU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット入力処理
      *****************************************************************
       410119-INPSET-SEC           SECTION.
      *
      *    約束セット以外をエラーとする
           IF      SPA-GMN-INPUTCD(SPA-GMN-IDX4)(1:1)   =   "S"
               CONTINUE
           ELSE
               MOVE    "0005"              TO  SPA-ERRCD
               MOVE    "1"                 TO  SPA-COM-CUR(SPA-GMN-IDX4)
               GO      TO      410119-INPSET-EXT
           END-IF.
      *
      *    時間外がないこと
           IF      FLG-JKNKSN      NOT =   ZERO
               MOVE    "0010"              TO  SPA-ERRCD
               GO  TO                 410119-INPSET-EXT
           END-IF.
      *
           MOVE    1                   TO  FLG-SETSYORI.
      *
      *    約束の時、１行前に診療種別があること
           IF      SPA-GMN-INPUTCD(SPA-GMN-IDX4)(1:1)   =   "S"
               IF     (SPA-GMN-IDX4        =   1)   OR
                      (SPA-GMN-INPUTCD(SPA-GMN-IDX4 - 1)(1:1)
                                       NOT = ".")
                   MOVE    "0048"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  SPA-COM-CUR
                                                         (SPA-GMN-IDX4)
               END-IF
           END-IF.
      *
      *    行オーバーの為、ＳＰＡを保存する
           MOVE    SPA-SCR-REC         TO  WRK-SCR-REC.
           MOVE    SPA-GMN-IDX4        TO  WRK-GMN-IDX.
      *
           MOVE    ZERO                TO  IDX-IP.
           MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX4)(1:6)
                                       TO  WRK-SETCD.
      *    入力セットマスタ検索
           INITIALIZE                      INPUTSET-REC.
           MOVE    SPA-HOSPID          TO  ISET-HOSPID.
           MOVE    WRK-SETCD           TO  ISET-SETCD.
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "INPUTSET-KEY2"     TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "INPUTSET-KEY2"     TO  ORC-DBPATH
               PERFORM 975-INPUTSET-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTSET
           END-IF.
      *
      *    約束処方の初期処理
           IF     (SPA-GMN-INPUTCD(SPA-GMN-IDX4)(1:1)  =   "S")  AND
                  (FLG-INPUTSET        =   ZERO  )
      *
               IF      FLG-TOROKU      NOT =   "2"
                   PERFORM 4101192-INPSET-SET-SEC
               END-IF
      *        減がある時、その次へ
               IF      SPA-COM-INPUTCD(SPA-GMN-IDX4 + 1)  =  "820000047"
                   ADD     1               TO  SPA-GMN-IDX4
               END-IF
           END-IF.
      *
           PERFORM UNTIL     (FLG-INPUTSET     =   1    )  OR
                             (SPA-ERRCD    NOT =   SPACE)
      *        行を挿入して追加する
               ADD     1               TO  SPA-GMN-IDX4
               PERFORM 41014-GYOINSN-SEC
      *
               ADD     1               TO  IDX-IP
      *
               MOVE    ISET-INPUTCD    TO  SPA-GMN-INPUTCD(SPA-GMN-IDX4)
               IF      ISET-SURYO1     =   ZERO
                   MOVE    1           TO  SPA-COM-SURYO (SPA-GMN-IDX4)
               ELSE
                   MOVE    ISET-SURYO1 TO  SPA-COM-SURYO (SPA-GMN-IDX4)
               END-IF
               MOVE    SPA-COM-SURYO(SPA-GMN-IDX4)
                                       TO  SPA-COM-SURYO2(SPA-GMN-IDX4)
               IF      ISET-SURYO2     =   ZERO
                   MOVE    1           TO  SPA-COM-BUNGSU(SPA-GMN-IDX4)
               ELSE
                   MOVE    ISET-SURYO2 TO  SPA-COM-BUNGSU(SPA-GMN-IDX4)
               END-IF
               IF      ISET-KAISU      =   ZERO
                   MOVE    1           TO  SPA-COM-KAISU  (SPA-GMN-IDX4)
               ELSE
                   MOVE    ISET-KAISU  TO  SPA-COM-KAISU  (SPA-GMN-IDX4)
                   MOVE    "1"         TO  SPA-COM-KAIFLG(SPA-GMN-IDX4)
               END-IF
               MOVE    SPA-COM-KAISU(SPA-GMN-IDX4)
                                       TO  SPA-COM-KAISU2(SPA-GMN-IDX4)
               MOVE    ISET-COMMENT    TO  SPA-GMN-MEISYO(SPA-GMN-IDX4)
      *
               MOVE    ISET-ATAI (01)  TO  SPA-COM-ATAI1 (SPA-GMN-IDX4)
               MOVE    ISET-ATAI (02)  TO  SPA-COM-ATAI2 (SPA-GMN-IDX4)
               MOVE    ISET-ATAI (03)  TO  SPA-COM-ATAI3 (SPA-GMN-IDX4)
               MOVE    ISET-ATAI (04)  TO  SPA-COM-ATAI4 (SPA-GMN-IDX4)
      *        画面表示なし
               IF      SPA-GMN-INPUTCD(WRK-GMN-IDX)(1:1)   =   "S"
                   MOVE    "1"             TO  SPA-COM-SET(SPA-GMN-IDX4)
               END-IF
      *
               IF       SPA-GMN-INPUTCD(SPA-GMN-IDX4)(1:1)  =   "."
      *            診療種別区分名称チェック
                   MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX4)(2:3)
                                                 TO  WRK-SRYSYUKBN
                   PERFORM 510-SRYKBN-MEI-SEC
               ELSE
      *
                   MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX4)(1:9)
                                               TO  SPA-COM-INPUTCD
                                                       (SPA-GMN-IDX4)
      *
      *            約束処方の時、数量を計算する
                   IF      SPA-GMN-INPUTCD(WRK-GMN-IDX)(1:1)   =   "S"
      *
      *                数量変更
                       COMPUTE SPA-COM-SURYO (SPA-GMN-IDX4)
                                               =   SPA-COM-SURYO
                                                         (SPA-GMN-IDX4)
                                               *   SPA-COM-SURYO
                                                         (WRK-GMN-IDX)
                       MOVE    SPA-COM-SURYO(SPA-GMN-IDX4)
                                       TO  SPA-COM-SURYO2(SPA-GMN-IDX4)
      *                回数変更
                       MOVE    SPA-COM-KAISU (WRK-GMN-IDX)
                                               TO  SPA-COM-KAISU
                                                         (SPA-GMN-IDX4)
                       MOVE    SPA-COM-KAISU(SPA-GMN-IDX4)
                                       TO  SPA-COM-KAISU2(SPA-GMN-IDX4)
                   END-IF
      *
      *            点数マスタ編集・基本チェック
                   MOVE    SPA-GMN-IDX4        TO  WRK-TEN-GMN-IDX
                   MOVE    SPA-COM-INPUTCD(SPA-GMN-IDX4)
                                               TO  WRK-TEN-SRYCD
                   MOVE    SPA-NAI-NENREI      TO  WRK-TEN-NENREI
                   MOVE    SPA-ROUJIN          TO  WRK-TEN-ROUJIN
                   PERFORM 31011-TEN-HENKAN-SEC
      *
      *           エラー時、追加しない
                  IF      SPA-ERRCD           NOT =   SPACE
                      MOVE    SPACE           TO  SPA-ERRCD
                      INITIALIZE          SPA-GMN-TBLREC (SPA-GMN-IDX4)
                      INITIALIZE          SPA-COM-TBLREC (SPA-GMN-IDX4)
                      COMPUTE SPA-GMN-IDX4     =   SPA-GMN-IDX4
                                               -   1
                   ELSE
      *                保険外金額判定
                       PERFORM 4101191-JIHI-HEN-SEC
                   END-IF
      *
               END-IF
      *
               IF      SPA-COM-KAIFLG (SPA-GMN-IDX4)    =   "1"
                   MOVE    SPACE               TO  WRK-SRYKBN
               END-IF
      *
               MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX4)
                                       TO  SPA-MAE-INPUTCD(SPA-GMN-IDX4)
      *
               MOVE    "INPUTSET-KEY2"     TO  ORC-DBPATH
               PERFORM 975-INPUTSET-READ-SEC
      *
           END-PERFORM.
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "INPUTSET-KEY2"     TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
          IF      (SPA-ERRCD       NOT =   SPACE)  AND
                  (FLG-INPUTSET        =   ZERO )
               MOVE    WRK-SCR-REC        TO  SPA-SCR-REC
               MOVE    WRK-GMN-IDX        TO  SPA-GMN-IDX4
               GO      TO    410119-INPSET-EXT
           END-IF.
           MOVE    SPACE               TO  SPA-ERRCD.
      *
      *    対象なし
           IF      IDX-IP              =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               GO      TO    410119-INPSET-EXT
           END-IF.
      *    約束セットの時
           IF      SPA-GMN-INPUTCD(WRK-GMN-IDX)(1:1)   =   "S"
      *        セットの時前に編集
               MOVE    SPA-GMN-INPUTCD(WRK-GMN-IDX)
                                       TO  SPA-MAE-INPUTCD(WRK-GMN-IDX)
           END-IF.
      *
           MOVE    1                   TO  SPA-GMN-CUR4.
      *
       410119-INPSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険外金額判定処理
      *****************************************************************
       4101191-JIHI-HEN-SEC           SECTION.
      *
           IF      SPA-COM-INPUTCD(SPA-GMN-IDX4)(1:3)  =  "095" OR "096"
               IF      SPA-COM-KISOTEN (SPA-GMN-IDX4)  =  ZERO
                   MOVE    "1"                 TO  SPA-COM-KEIFLG
                                                          (SPA-GMN-IDX4)
                   MOVE    ISET-SURYO1         TO  SPA-COM-KEI
                                                          (SPA-GMN-IDX4)
                   MOVE    ISET-SURYO1         TO  SPA-GMN-KEI
                                                          (SPA-GMN-IDX4)
                   MOVE    1                   TO  SPA-COM-SURYO
                                                          (SPA-GMN-IDX4)
               ELSE
                   MOVE    "2"                 TO  SPA-COM-KEIFLG
                                                          (SPA-GMN-IDX4)
               END-IF
           ELSE
               IF      WRK-SRYKBN          =   "95"  OR  "96"
                   MOVE    "2"                 TO  SPA-COM-KEIFLG
                                                          (SPA-GMN-IDX4)
               END-IF
           END-IF.
      *
       4101191-JIHI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    約束処方の処理
      *****************************************************************
       4101192-INPSET-SET-SEC           SECTION.
      *
      *    入力位置のセット
           MOVE    SPA-GMN-IDX4        TO  WRK-IN-IDX4.
      *    数量以降入力なし
           IF      SPA-GMN-INPUTCD (SPA-GMN-IDX4)(1:1)  =   "S"
               MOVE    "1"                 TO  SPA-COM-IN
                                                      (SPA-GMN-IDX4)
           END-IF.
      *
      *    数量・回数編集
           PERFORM 4101122-KOMOKU-HENSYU-SEC.
           MOVE    WRK-CD(1:WRK-CD-MCNT)   TO  WRK-INPUTCD.
      *
      *    約束名称編集
           PERFORM 41011921-INPSET-MEI-SEC.
      *
       4101192-INPSET-SET-EXT.
           EXIT.
      *****************************************************************
      *    約束処方の処理
      *****************************************************************
       41011921-INPSET-MEI-SEC           SECTION.
      *
           MOVE    SPACE               TO  INPUTCD-REC.
           MOVE    SPA-HOSPID          TO  ICD-HOSPID.
      *
           MOVE    WRK-ICD-CDSYU       TO  ICD-CDSYU.
           MOVE    WRK-INPUTCD         TO  ICD-INPUTCD.
      *
           PERFORM 930-INPUTCD-READ-SEC.
      *
           IF      FLG-INPUTCD     NOT =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
               GO  TO                  41011921-INPSET-MEI-EXT
           END-IF.
           MOVE    ICD-DSPNAME         TO  SPA-GMN-MEISYO(SPA-GMN-IDX4).
      *    回数入力としない
           MOVE    SPACE               TO  SPA-COM-KAIFLG(SPA-GMN-IDX4).
      *
       41011921-INPSET-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為、計計算処理、編集処理
      *****************************************************************
       510-TBLHEN-SEC           SECTION.
      *
      *    空白行をなくす
           PERFORM 420-SYOKYO-SEC.
      *
           MOVE    ZERO                TO  SPA-GMN-MAX.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   200
               MOVE    SPACE           TO  SPA-COM-ZAIEND (IDX)
      *        計算チェック済フラグ
               MOVE    ZERO            TO  SPA-COM-SANFLG (IDX)
      *        カーソルのクリア
               MOVE    SPACE           TO  SPA-COM-CUR    (IDX)
      *
               IF      (SPA-GMN-INPUTCD(IDX) =   SPACE)   AND
                       (SPA-COM-INPUTCD(IDX) =   SPACE)
                   CONTINUE
               ELSE
                   ADD     1                   TO  SPA-GMN-MAX
               END-IF
           END-PERFORM.
      *
      *    診療種別区分編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX              >   200      )   OR
                              (SPA-ERRCD        NOT =   SPACE)   OR
                              (SPA-GMN-INPUTCD(IDX) =   SPACE  AND
                               SPA-COM-INPUTCD(IDX) =   SPACE)
      *
      *        剤分離
               PERFORM 5101-SYRKBN-SET-SEC
           END-PERFORM.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      510-TBLHEN-EXT
           END-IF.
      *
      *    診療種別件数クリア
           INITIALIZE                  SPA-SRYKBN-AREA.
      *    剤ごとの処理
           MOVE    ZERO                TO  IDX-Z.
           INITIALIZE                  WRK-SCR-REC.
           INITIALIZE                  WRK-HOZ-AREA.
           INITIALIZE                  WRK-HOZ-IDX.
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   200      )   OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE)  OR
                              (SPA-ERRCD           NOT =   SPACE)
      *        剤ごとの算定処理で変更される加算データは対象としない
               ADD     1               TO  IDX-Z
               MOVE    SPA-COM-TBLREC (IDX)
                                       TO  WRK-COM-TBLREC(IDX-Z)
               MOVE    SPA-GMN-TBLREC (IDX)
                                       TO  WRK-GMN-TBLREC(IDX-Z)
      *
               IF     (IDX                    =   SPA-GMN-MAX)  OR
                      (SPA-COM-ZAIEND (IDX)   =   "1"        )
      *            剤ごとの処理
                   PERFORM 5102-ZAIBETU-SEC
                   INITIALIZE                  WRK-SCR-REC
                   MOVE    ZERO                TO  IDX-Z
               END-IF
           END-PERFORM.
      *
      *    スパ領域へ戻す
           MOVE    ZERO                TO  SPA-GMN-MAX.
           INITIALIZE                  SPA-GMN-REC.
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   200
               IF      WRK-HGMN-TBLREC (IDX)   NOT =   SPACE
                   ADD     1                       TO  SPA-GMN-MAX
                   MOVE    WRK-HGMN-TBLREC (IDX)   TO  SPA-GMN-TBLREC
                                                          (SPA-GMN-MAX)
                   MOVE    WRK-HCOM-TBLREC (IDX)   TO  SPA-COM-TBLREC
                                                         (SPA-GMN-MAX)
               END-IF
           END-PERFORM.
      *
           IF      SPA-GMN-MAX         =   ZERO
               MOVE    1               TO  SPA-MAX-PAGE4
           ELSE
               COMPUTE SPA-MAX-PAGE4   =  (SPA-GMN-MAX    -  1)
                                       /   20     +    1
           END-IF.
      *
      *    禁忌チェックを行う
           IF      FLG-TOROKU          =   SPACE
               PERFORM 51010-KNKCHK-SEC
               IF      FLG-KINKI           =   1
                   GO      TO      510-TBLHEN-EXT
               END-IF
           END-IF.
      *
        510-TBLHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療種別区分編集決定処理
      *****************************************************************
       5101-SYRKBN-SET-SEC             SECTION.
      *
      *    セット入力時、前の診療種別とする
           IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "S"
               IF     (IDX                 =   1   )   OR
                      (SPA-GMN-INPUTCD(IDX - 1)(1:1)   NOT =   ".")
                   MOVE    "1"                 TO  SPA-COM-CUR (IDX)
                   MOVE    "0048"              TO  SPA-ERRCD
                   GO      TO      5101-SYRKBN-SET-EXT
               END-IF
               MOVE    SPA-COM-SRYSYUKBN (IDX - 1)
                                           TO  SPA-COM-SRYSYUKBN (IDX)
               MOVE    SPA-COM-SRYKBN (IDX - 1)
                                           TO  SPA-COM-SRYKBN (IDX)
               MOVE    SPACE               TO  SPA-COM-ZAIEND (IDX)
               GO      TO      5101-SYRKBN-SET-EXT
           END-IF.
      *
      *    診療種別入力時、剤の終了とする
           IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "."
               MOVE    SPA-GMN-INPUTCD(IDX)(2:3)   TO  WRK-SRYSYUKBN
      *
      *        診療種別区分名称、診療行為区分セット
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
               MOVE    WRK-SRYSYUKBNMEI    TO  SPA-GMN-MEISYO (IDX)
               MOVE    WRK-SRYKBN          TO  SPA-GMN-SRYKBN (IDX)
      *
               MOVE    WRK-SRYSYUKBN       TO  SPA-COM-SRYSYUKBN (IDX)
               MOVE    WRK-SRYKBN          TO  SPA-COM-SRYKBN (IDX)
               IF      IDX                 >   1
                   MOVE    "1"                 TO  SPA-COM-ZAIEND
                                                             (IDX - 1)
               END-IF
      *        自費の時、回数入力まで自費とする
               IF      WRK-SRYSYUKBN       =   "950"  OR  "960"
                   MOVE    1                   TO  FLG-JIHI
               ELSE
                   MOVE    ZERO                TO  FLG-JIHI
               END-IF
               GO      TO      5101-SYRKBN-SET-EXT
           END-IF.
      *
      *    自費の時、回数入力まで自費とする
           IF      FLG-JIHI            =   1
               MOVE    WRK-SRYSYUKBN       TO  SPA-COM-SRYSYUKBN (IDX)
               MOVE    WRK-SRYKBN          TO  SPA-COM-SRYKBN (IDX)
               IF      SPA-COM-KAIFLG(IDX)     =   "1"
                   MOVE    "1"             TO  SPA-COM-ZAIEND (IDX)
                   MOVE    SPACE           TO  SPA-COM-SRYSYUKBN (IDX)
               END-IF
      *            金額入力の自費に付いては、１剤１コードとする
               IF     (SPA-COM-INPUTCD(IDX)(1:3)   =   "095" OR
                                                       "096" )  AND
                      (SPA-COM-KISOTEN(IDX)        =   ZERO  )
                   MOVE    "1"             TO  SPA-COM-ZAIEND (IDX)
                   MOVE    SPACE           TO  SPA-COM-SRYSYUKBN (IDX)
               END-IF
               GO      TO      5101-SYRKBN-SET-EXT
           END-IF.
      *
           IF      IDX                  >  1
               MOVE    "1020"          TO  SPA-ERRCD
               MOVE    "1"             TO  SPA-COM-CUR(IDX)
           END-IF.
           IF      SPA-COM-SRYKBN (IDX)(1:1)   =   "9"
               MOVE    SPACE           TO  SPA-ERRCD
               MOVE    SPACE           TO  SPA-COM-CUR (IDX)
           END-IF.
           IF      SPA-COM-INPUTCD (IDX)(1:2)   =   "10"
               MOVE    SPACE           TO  SPA-ERRCD
               MOVE    SPACE           TO  SPA-COM-CUR (IDX)
           END-IF.
           IF      SPA-COM-INPUTCD (IDX)(1:6)   =   "788888"
               MOVE    SPACE           TO  SPA-ERRCD
               MOVE    SPACE           TO  SPA-COM-CUR (IDX)
           END-IF.
           IF      SPA-COM-INPUTCD (IDX)(1:1)   =   "8"
               MOVE    SPACE           TO  SPA-ERRCD
               MOVE    SPACE           TO  SPA-COM-CUR (IDX)
           END-IF.
      *
           IF      SPA-COM-CUR (IDX)   =   "1"
               GO      TO      5101-SYRKBN-SET-EXT
           END-IF.
      *
      *    前行に回数入力時、前回を終了とする
           IF     (IDX                                 >   1 )  AND
                  (SPA-COM-KAIFLG (IDX - 1)            =  "1")  AND
                  (SPA-GMN-INPUTCD(IDX - 1)(1:1)   NOT =  ".")
               MOVE    "1"             TO  SPA-COM-ZAIEND (IDX - 1)
           END-IF.
      *
      *    入院回数入力時、終了とする
           IF     (IDX                         >   1  )  AND
                  (SPA-GMN-INPUTCD(IDX)(1:1)   =   "*")
               MOVE    SPACE           TO  SPA-COM-ZAIEND (IDX - 1)
               MOVE    "1"             TO  SPA-COM-ZAIEND (IDX)
               MOVE    SPA-COM-SRYSYUKBN(IDX - 1)
                                       TO  SPA-COM-SRYSYUKBN (IDX)
               MOVE    SPA-COM-SRYKBN   (IDX - 1)
                                       TO  SPA-COM-SRYKBN (IDX)
           END-IF.
      *
      *    最終行を剤終了とする
           IF      IDX                 =   SPA-GMN-MAX
               MOVE    "1"             TO  SPA-COM-ZAIEND (IDX)
           END-IF.
      *
       5101-SYRKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤ごとの処理
      *****************************************************************
       5102-ZAIBETU-SEC             SECTION.
      *
      *    編集後内容セット
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   200)  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE)
      *        診区
               IF      IDX-Z               =   1
                   MOVE    WRK-COM-SRYKBN (IDX-Z)
                                           TO  WRK-GMN-SRYKBN(IDX-Z)
               ELSE
                   MOVE    SPACE           TO  WRK-GMN-SRYKBN(IDX-Z)
                   MOVE    WRK-COM-SRYSYUKBN (01)
                                           TO  WRK-COM-SRYSYUKBN(IDX-Z)
               END-IF
      *        名称（最初の行に’＊’）
               IF      WRK-COM-INPUTCD(IDX-Z)(1:1) =   "8"
                   CONTINUE
               ELSE
                   IF      IDX-Z               =   1
                       MOVE    "* "            TO  WRK-GMN-MEIH (IDX-Z)
                   ELSE
                       MOVE    SPACE           TO  WRK-GMN-MEIH (IDX-Z)
                   END-IF
               END-IF
      *
      *        最終行を剤終了とする
               IF      (IDX-Z                      =   200  )  OR
                       (WRK-COM-INPUTCD(IDX-Z + 1) =   SPACE)
                   MOVE    "1"             TO  WRK-COM-ZAIEND (IDX-Z)
               ELSE
                   MOVE    SPACE           TO  WRK-COM-ZAIEND (IDX-Z)
               END-IF
      *
               ADD     1                   TO  WRK-HOZ-IDX
               MOVE    WRK-COM-TBLREC (IDX-Z)
                                           TO  WRK-HCOM-TBLREC
                                                       (WRK-HOZ-IDX)
               MOVE    WRK-GMN-TBLREC (IDX-Z)
                                           TO  WRK-HGMN-TBLREC
                                                       (WRK-HOZ-IDX)
      *
           END-PERFORM.
      *
       5102-ZAIBETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌チェック処理
      *****************************************************************
       51010-KNKCHK-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-KINKI.
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z                   >   200  )   OR
                              (SPA-GMN-INPUTCD (IDX-Z) =   SPACE)
               IF     (SPA-COM-SRYKBN  (IDX-Z)     =   "21" OR  "22"
                                                   OR  "23" OR  "31"
                                                   OR  "32" OR  "33")
                 AND  (SPA-COM-INPUTCD (IDX-Z)(1:1)    =  "6")
      *            禁忌薬剤チェック処理
                   IF      (SPA-COM-KNKFLG (IDX-Z)  =   SPACE)  AND
                           (SPA-COM-KNKUMU (IDX-Z)  =   SPACE)
                       PERFORM 510222-KNKYAK-CHK-SEC
                   END-IF
               END-IF
           END-PERFORM.
      *
       51010-KNKCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌薬剤チェック処理
      *****************************************************************
       510222-KNKYAK-CHK-SEC           SECTION.
      *    チェックレコードより、禁忌有無のチェックを行う
           INITIALIZE                  CHK-REC.
           MOVE    "4"                 TO  CHK-CHKKBN.
           MOVE    SPA-COM-INPUTCD (IDX-Z)
                                       TO  CHK-SRYCD.
           MOVE    "2"                 TO  CHK-CDKBN.
           MOVE    ZERO                TO  CHK-RENNUM.
      *
           MOVE    ZERO                TO  FLG-CHKMST.
           MOVE    CHK-REC             TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "CHK-KEY3"          TO  ORC-DBPATH.
           MOVE     SPA-I40-SRYYMD     TO  ORC-DBYMD.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "CHK-KEY3"          TO  ORC-DBPATH
               PERFORM 950-CHK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-CHKMST
           END-IF.
           IF      FLG-CHKMST          =   1
      *
               MOVE    "DBCLOSE"           TO  MCP-FUNC
               MOVE    "CHK-KEY3"          TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
      *
               MOVE    "1"             TO  SPA-COM-KNKUMU (IDX-Z)
               GO      TO      510222-KNKYAK-CHK-EXT
           END-IF.
      *
      *    禁忌チェック
           MOVE    ZERO                TO  IDX-K.
           INITIALIZE                  WRK-KNKCHK-AREA.
           PERFORM
                   UNTIL      FLG-CHKMST        =   1
               PERFORM VARYING     IDX-C       FROM    1   BY  1
                       UNTIL      (IDX-C           >   10   )  OR
                                  (CHK-CD (IDX-C)  =   SPACE)
                   PERFORM 5102221-KNKCHK-SYORI-SEC
               END-PERFORM
      *
               MOVE    "CHK-KEY3"          TO  ORC-DBPATH
               PERFORM 950-CHK-READ-SEC
           END-PERFORM.
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "CHK-KEY3"          TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
           IF      IDX-K               >   ZERO
               MOVE    "1"             TO  SPA-COM-KNKFLG (IDX-Z)
               MOVE    SPA-GMN-MEISYO (IDX-Z)  TO  WRK-KNK-TOUMEI (01)
      *        同一診療コードをなくす
               PERFORM VARYING     IDX-C   FROM    1   BY  1
                       UNTIL       IDX-C   >   30
                   PERFORM VARYING     IDX-X   FROM    IDX-C   BY  1
                           UNTIL       IDX-X   >   30
                       IF     (IDX-C       NOT =   IDX-X)   AND
                              (WRK-KNK-SRYCD (1 IDX-C)
                                               =   WRK-KNK-SRYCD
                                                             (1 IDX-X))
                           MOVE    SPACE       TO  WRK-KNK-TBL2
                                                             (1 IDX-X)
                       END-IF
                   END-PERFORM
               END-PERFORM
      *
      *        初期画面の時、エラー画面なし
               IF      FLG-TOROKU          =   "2"
                   CONTINUE
               ELSE
                   PERFORM 510229-POP-ERR-SEC
               END-IF
           END-IF.
      *
       510222-KNKYAK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌薬剤チェック処理
      *****************************************************************
       5102221-KNKCHK-SYORI-SEC           SECTION.
      *
      *    診療会計マスタとのチェック
           PERFORM VARYING     IDX-S       FROM    1   BY  1
                   UNTIL      (IDX-S                   >   100  )  OR
                              (SPA-KNK-SRYCD (IDX-S)   =   SPACE)
               IF     (SPA-KNK-SRYCD (IDX-S)   =   CHK-CD  (IDX-C)) AND
                      (SPA-KNK-TOGETU (IDX-S)  =   "1"  ) 
                   ADD     1                   TO  IDX-K
                   MOVE    SPA-KNK-SRYMEI (IDX-S)  TO  WRK-KNK-KNKMEI
                                                              (1 IDX-K)
                   MOVE    SPA-KNK-SRYCD (IDX-S)   TO  WRK-KNK-SRYCD
                                                              (1 IDX-K)
               END-IF
           END-PERFORM.
      *
       5102221-KNKCHK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療種別区分名称、診療行為区分セット
      *****************************************************************
       510-SRYKBN-MEI-SEC            SECTION.
      *
           SET     MSYU-IDX            TO  1.
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
           END-SEARCH.
      *
       510-SRYKBN-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    行挿入処理
      *****************************************************************
       41014-GYOINSN-SEC         SECTION.
      *
           IF      SPA-GMN-MAX         =   200
               MOVE    "0007"              TO  SPA-ERRCD
               GO      TO      41014-GYOINSN-EXT
           END-IF.
      *
           MOVE    SPA-GMN-REC         TO  WRK-GMN-REC.
           INITIALIZE                      SPA-GMN-REC.
           MOVE    ZERO                TO  IDY.
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   200
               IF      IDX             =   SPA-GMN-IDX4
                   CONTINUE
               ELSE
                   ADD     1               TO  IDY
                   MOVE    WRK-COM-TBLREC (IDY) 
                                           TO  SPA-COM-TBLREC(IDX)
                   MOVE    WRK-GMN-TBLREC (IDY) 
                                           TO  SPA-GMN-TBLREC(IDX)
               END-IF
               IF      SPA-GMN-INPUTCD (IDX)   NOT =   SPACE
                   MOVE    IDX                 TO  SPA-GMN-MAX
               END-IF
           END-PERFORM.
      *
           IF      SPA-GMN-MAX         =   ZERO
               MOVE    1               TO  SPA-MAX-PAGE4
           ELSE
               COMPUTE SPA-MAX-PAGE4   =  (SPA-GMN-MAX    -  1)
                                       /   20     +    1
           END-IF.
      *
           MOVE    1                   TO  SPA-GMN-CUR4.
      *
       41014-GYOINSN-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット削除処理
      *****************************************************************
       41016-SETDEL-SEC              SECTION.
      *
           COMPUTE IDX2                =   SPA-GMN-IDX4   +   1.
      *    減がある時、その次より
           IF      SPA-COM-INPUTCD (IDX2)  =   "820000047"
               ADD     1                   TO  IDX2
           END-IF.
      *
           PERFORM VARYING     IDX     FROM    IDX2    BY  1
                   UNTIL      (IDX     >   200 )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE)
               IF      SPA-COM-SET     (IDX)   =   "1"
                   MOVE    SPACE               TO  SPA-GMN-INPUTCD(IDX)
               ELSE
                   MOVE    200                 TO  IDX
               END-IF
           END-PERFORM.
      *
           MOVE    SPA-GMN-REC         TO  WRK-GMN-REC.
           INITIALIZE                      SPA-GMN-REC.
           MOVE    ZERO                TO  SPA-GMN-MAX.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   200
               IF      WRK-GMN-INPUTCD(IDX)    =   SPACE
                   CONTINUE
               ELSE
                   ADD     1               TO  SPA-GMN-MAX
                   MOVE    WRK-COM-TBLREC (IDX) 
                                           TO  SPA-COM-TBLREC
                                                        (SPA-GMN-MAX)
                   MOVE    WRK-GMN-TBLREC (IDX) 
                                           TO  SPA-GMN-TBLREC
                                                        (SPA-GMN-MAX)
               END-IF
           END-PERFORM.
      *
       41016-SETDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    １行削除処理
      *****************************************************************
       420-SYOKYO-SEC              SECTION.
      *
           MOVE    SPA-GMN-REC         TO  WRK-GMN-REC.
           INITIALIZE                      SPA-GMN-REC.
           MOVE    ZERO                TO  SPA-GMN-MAX.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   200
               IF      WRK-GMN-INPUTCD(IDX)    =   SPACE
      *            削除行が加算料を自動追加した時、自動追加分も削除
                   IF     (WRK-COM-INPUTCD(IDX)    NOT =   SPACE)  AND
                          (IDX                         <   200  )  AND
                          (WRK-COM-AUTOKBN  (IDX + 1)  =   "1"  )
                       MOVE    SPACE           TO  WRK-GMN-INPUTCD
                                                             (IDX + 1)
                   END-IF
               ELSE
                   ADD     1                      TO  SPA-GMN-MAX
                   MOVE    WRK-COM-TBLREC (IDX)   TO  SPA-COM-TBLREC
                                                        (SPA-GMN-MAX)
                   MOVE    WRK-GMN-TBLREC (IDX)   TO  SPA-GMN-TBLREC
                                                        (SPA-GMN-MAX)
               END-IF
           END-PERFORM.
      *
           IF      SPA-GMN-MAX         =   ZERO
               MOVE    1               TO  SPA-MAX-PAGE4
           ELSE
               COMPUTE SPA-MAX-PAGE4   =  (SPA-GMN-MAX    -  1)
                                       /   20     +    1
           END-IF.
      *
       420-SYOKYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "I41"               TO  SPA-SAKIPG.
           MOVE    "I44"               TO  SPA-MOTOPG.
      *
           MOVE    SPA-I4KYOUTU        TO  SPA-WORK.
      *
           MOVE    SPA-KYOUTU          TO  LNK-COMMON.
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU.
           MOVE    SPA-I4              TO  LNK-SCRSPA.
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE    "I41"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       450-MAEGMN-SEC             SECTION.
      *
      *    診療行為入力チェック
           PERFORM 41001-KOUI-ALLCHK-SEC.
           IF     (SPA-ERRCD       NOT =   SPACE)  OR
                  (FLG-END             =   1    )
               GO      TO      450-MAEGMN-EXT
           END-IF.
      *
           IF      SPA-GMN-PAGE4       =   1
               MOVE    1               TO  SPA-GMN-PAGE4
           ELSE
               COMPUTE SPA-GMN-PAGE4   =   SPA-GMN-PAGE4  -   1
           END-IF.
      *
           MOVE    1                   TO  SPA-MAP-IDX4.
           MOVE    1                   TO  SPA-GMN-CUR4.
      *
       450-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       460-ATOGMN-SEC             SECTION.
      *
      *    診療行為入力チェック
           PERFORM 41001-KOUI-ALLCHK-SEC.
           IF     (SPA-ERRCD       NOT =   SPACE)  OR
                  (FLG-END             =   1    )
               GO      TO      460-ATOGMN-EXT
           END-IF.
      *
           COMPUTE WRK-FREE            =   SPA-GMN-PAGE4  *  20.
           IF      SPA-GMN-INPUTCD(WRK-FREE)   =   SPACE
               GO  TO                 460-ATOGMN-EXT
           END-IF.
      *
           ADD     1                   TO  SPA-GMN-PAGE4.
      *
           MOVE    1                   TO  SPA-GMN-CUR4.
      *
       460-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新前チェック処理
      *****************************************************************
       41001-KOUI-ALLCHK-SEC       SECTION.
      *
      *    画面をＳＰＡへ編集
           MOVE    1                   TO  WRK-IDX2.
           PERFORM 2002-GMNSPA-SET-SEC.
      *
      *    入力コード・名称チェック処理
           MOVE    "1"                 TO  FLG-TOROKU.
           PERFORM 410112-KOUI-SPACHK-SEC.
      *
       41001-KOUI-ALLCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       4100-TOROKU-SEC             SECTION.
      *
      *    診療行為入力チェック
           PERFORM 41001-KOUI-ALLCHK-SEC.
           IF     (SPA-ERRCD       NOT =   SPACE)  OR
                  (FLG-END             =   1    )
               GO      TO      4100-TOROKU-EXT
           END-IF,
      *
      *    禁忌更新前のチェック
           PERFORM 41001-YAKKNK-CHK-SEC.
      *
      *     禁忌エラーの時、エラー表示へ
           IF      WRK-KNKCHK-AREA     NOT =   SPACE
               PERFORM 410019-POP-J041-SEC
               GO      TO      4100-TOROKU-EXT
           END-IF.
      *
      *    剤のチェック
           PERFORM 45011-NAIYOU-CHK-SEC.
      *    同一剤が存在している時、エラーとする
           IF      FLG-OK              =   1
               MOVE    "1025"              TO  SPA-ERRCD
               GO      TO      4100-TOROKU-EXT
           END-IF.
      *
           IF     (FLG-OK2             =   1   )  AND
                  (WRK-ZAINUM          =   ZERO)
      *        剤変更なしとして、処理を終了する
               PERFORM 210-BACK
               GO      TO      4100-TOROKU-EXT
           END-IF.
      *
           MOVE    "0101"              TO  SPA-IIDCD.
      *
       4100-TOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       41002-KOUSIN-SYORI-SEC          SECTION.
      *
      *    入院行為マスタ更新
           PERFORM 4501-NYUKOUI-KOU-SEC.
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41002-KOUSIN-SYORI-EXT
           END-IF.
      *
           IF      SPA-SEL-RRKYMD      =   SPACE
      *        入院会計マスタ更新
               PERFORM 4502-NYUKAI-KOU-SEC
           ELSE
      *        入院会計マスタ追加、受診履歴変更
               PERFORM 4504-NYUACCT-ADD-SEC
           END-IF.
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41002-KOUSIN-SYORI-EXT
           END-IF.
      *
           MOVE    "I41"               TO  SPA-SAKIPG.
           MOVE    "I44"               TO  SPA-MOTOPG.
      *
           MOVE    SPA-I4KYOUTU        TO  SPA-WORK.
      *
           MOVE    SPA-KYOUTU          TO  LNK-COMMON.
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU.
           MOVE    SPA-I4              TO  LNK-SCRSPA.
      *
           IF      WRK-PUTTYPE         =   SPACE
               MOVE    "CHANGE"            TO  MCP-PUTTYPE
           ELSE
               MOVE    "JOIN"              TO  MCP-PUTTYPE
           END-IF.
      *
           MOVE    "I41"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       41002-KOUSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角、半角変更編集処理
      *****************************************************************
       41011-KOUI-INPUTCD-SYORI-SEC     SECTION.
      *
           MOVE    ZERO                TO  IDX-C1.
           MOVE    ZERO                TO  IDX-C2.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   22
               IF      WRK-HENKAN(IDX:1)    =   "_"
                   COMPUTE   IDX-C1   =   IDX   +   1
                   MOVE    99              TO  IDX
               END-IF
           END-PERFORM.
           IF      IDX-C1    NOT =   ZERO
               PERFORM VARYING     IDX     FROM    IDX-C1   BY  1
                       UNTIL       IDX     >   22
                   IF      WRK-HENKAN(IDX:1)    =   " "
                       COMPUTE   IDX-C2   =   IDX   -   IDX-C1
                       MOVE    99              TO  IDX
                   END-IF
               END-PERFORM
           END-IF.
      *
      *    '_'をなくす
           INSPECT WRK-HENKAN
                   TALLYING    WRK-JODOU  FOR  ALL "_"
                   REPLACING   ALL "_"  BY  " ".
      *
      *    １桁目全角、半角変更
      *
           MOVE    SPACE               TO  WRK-INPUTCD2.
           MOVE    SPACE               TO  WRK-INPUTCD.
      *    漢字変換
           MOVE    1                   TO  IDH1.
           MOVE    ZERO                TO  IDH2.
           PERFORM   UNTIL       IDH1    >   22
             IF      WRK-HENKAN(IDH1:1)    >=  X"A1"
               EVALUATE    WRK-HENKAN(IDH1:2)
                   WHEN    "−"
                       ADD     1           TO  IDH2
                       MOVE    "-"         TO  WRK-INPUTCD2(IDH2:1)
                   WHEN    "＋"
                       ADD     1           TO  IDH2
                       MOVE    "+"         TO  WRK-INPUTCD2 (IDH2:1)
                   WHEN    "！"
                       ADD     1           TO  IDH2
                       MOVE    "!"         TO  WRK-INPUTCD2 (IDH2:1)
                   WHEN    "／"
                       ADD     1           TO  IDH2
                       MOVE    "/"         TO  WRK-INPUTCD2 (IDH2:1)
                   WHEN    "　"
                       ADD     1           TO  IDH2
                       MOVE    SPACE       TO  WRK-INPUTCD2 (IDH2:1)
                   WHEN    OTHER
                       ADD     1           TO  IDH2
                       MOVE    WRK-HENKAN(IDH1:2)
                                           TO  WRK-INPUTCD2 (IDH2:2)
                       ADD     1           TO  IDH2
               END-EVALUATE
               ADD     2                   TO  IDH1
             ELSE
               ADD     1                   TO  IDH2
               MOVE    WRK-HENKAN(IDH1:1)  TO  WRK-INPUTCD2 (IDH2:1)
               ADD     1                   TO  IDH1
             END-IF
           END-PERFORM.
      *
      *    ’＋’、’−’のチェック
           MOVE    SPACE               TO  WRK-CHK-FLG.
           MOVE    SPACE               TO  WRK-INPUTCD3.
           MOVE    ZERO                TO  IDX2.
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX    >   22
               IF      WRK-INPUTCD2(IDX:1)     =   "+"  OR  "-"
                   COMPUTE IDX3                =   IDX  +  1
                   IF     (WRK-CHK-FLG         =   SPACE)  AND
                          (IDX2                =   ZERO   OR
                           IDX                 =   22     OR
                           WRK-INPUTCD2(IDX3:) =   SPACE )
                       MOVE    WRK-INPUTCD2(IDX:1) TO  WRK-CHK-FLG
                   END-IF
               ELSE
                   ADD     1                   TO  IDX2
                   MOVE    WRK-INPUTCD2(IDX:1) TO  WRK-INPUTCD3
                                                             (IDX2:1)
      *            １，２桁目以外の '/'は、空白とする
                   IF     (WRK-INPUTCD2(IDX:1)     =   "/" )  AND
                          (IDX                 NOT =   1  AND   2)
                       IF      WRK-INPUTCD2(1:1)   =   "*"
                           CONTINUE
                       ELSE
                           MOVE    SPACE       TO  WRK-INPUTCD2(IDX:1)
                           MOVE    SPACE       TO  WRK-INPUTCD3(IDX2:1)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.
      *
           MOVE    WRK-INPUTCD2        TO  WRK-HENKAN.
      *
      *    １桁目へ−，＋編集
           IF      WRK-CHK-FLG     NOT =   SPACE
               MOVE    WRK-CHK-FLG         TO  WRK-INPUTCD(1:1)
               MOVE    WRK-INPUTCD3        TO  WRK-INPUTCD(2:)
           END-IF.
      *
           IF      WRK-HENKAN(1:2)   =   "!!"
               MOVE    WRK-HENKAN          TO  WRK-INPUTCD
           END-IF.
           IF      WRK-HENKAN(1:1)         =   "!"  OR
                                               " "  OR
                                               "/"  OR
                                               "-"  OR
                                               "+"
               MOVE    WRK-HENKAN          TO  WRK-INPUTCD
           END-IF.
      *
           IF      WRK-INPUTCD     NOT =   SPACE
               MOVE    WRK-INPUTCD         TO  WRK-HENKAN
               GO      TO          4101-KOUI-INPUTCD-SYORI-EXT
           END-IF.
      *
      *    入力コードの判定
           MOVE    WRK-HENKAN          TO  WRK-INPUTCD.
      *
      *    １桁が数字で２桁目が空白の判定を全角チェック
           IF     (WRK-HENKAN(1:1)         >=  X"A1")  AND
                  (WRK-HENKAN(3:1)         =   " "  )  AND
                  (WRK-HENKAN(4:)      NOT =   SPACE)
               MOVE    WRK-HENKAN          TO  WRK-HENKAN-MAE
               MOVE    1                   TO  WRK-STR
               MOVE    2                   TO  WRK-END
               PERFORM 4101-KOUI-MOJI-HENKAN-SEC
               MOVE    SPACE               TO  WRK-INPUTCD
               MOVE    WRK-HENKAN-ATO      TO  WRK-INPUTCD(1:1)
               MOVE    WRK-HENKAN(3:)      TO  WRK-INPUTCD(2:)
           END-IF.
      *
      *    １桁が’．’の時、種別入力とする
           IF      WRK-HENKAN(1:2)   =   "．" 
               MOVE    WRK-HENKAN          TO  WRK-HENKAN-MAE
               MOVE    1                   TO  WRK-STR
               MOVE    22                  TO  WRK-END
               PERFORM 4101-KOUI-MOJI-HENKAN-SEC
               MOVE    WRK-HENKAN-ATO      TO  WRK-HENKAN
           END-IF.
      *    診療種別
           IF      WRK-HENKAN(1:1)   =   "."
               GO      TO      4101-KOUI-INPUTCD-SYORI-EXT
           END-IF.
      *
      *    入力コード文字列調査処理
           PERFORM 4101-KOUI-INPUTCD-CHOSA-SEC.
           MOVE    WRK-INPUTCD         TO  WRK-HENKAN.
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4101-KOUI-INPUTCD-SYORI-EXT
           END-IF.
      *
      *    入力コード変更時、点数マスタの決定
           IF      FLG-NYURYOKU   =   1
               PERFORM 4101-KOUI-INPUTCD-KENSAKU-SEC
           END-IF.
      *
       4101-KOUI-INPUTCD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード文字列調査処理
      *****************************************************************
       4101-KOUI-INPUTCD-CHOSA-SEC       SECTION.
      *
      *--- 文字列調査の開始位置　取得
           IF  (WRK-INPUTCD(1:1)       IS  NUMERIC) AND
               (WRK-INPUTCD(2:1)       =   SPACE  ) AND
               (WRK-INPUTCD(3:1)   NOT =   SPACE  )
      *---     時間特例チェック
               IF      WRK-INPUTCD(1:1)     =   4
                   IF      SPA-JIKANTOKU      =   1
                       CONTINUE
                   ELSE
                       MOVE    "0005"          TO  SPA-ERRCD
                       GO      TO      4101-KOUI-INPUTCD-CHOSA-EXT
                   END-IF
               END-IF
               MOVE    WRK-INPUTCD(1:1)  TO  FLG-JKNKSN
               MOVE    3                 TO  WRK-IDX-FT
           ELSE
               MOVE    ZERO            TO  FLG-JKNKSN
               MOVE    1               TO  WRK-IDX-FT
           END-IF.
      *
           MOVE    ZERO                TO  FLG-KANJI.
           MOVE    ZERO                TO  FLG-KANA.
           MOVE    ZERO                TO  FLG-SUJI.
           MOVE    ZERO                TO  WRK-CD-MCNT.
           MOVE    SPACE               TO  WRK-CD.
           PERFORM VARYING IDX     FROM    WRK-IDX-FT   BY  1
                   UNTIL  (IDX         >   22     )  OR
                          (WRK-INPUTCD(IDX:1)
                                       =   SPACE  OR  "*" )
      *      全角の＊を半角の*とする
             IF      WRK-INPUTCD(IDX:2)    =  "＊" OR "　"
               MOVE    22                  TO  IDX
             ELSE
      *
               ADD     1                   TO  WRK-CD-MCNT
               MOVE    WRK-INPUTCD(IDX:1)  TO  WRK-CD (WRK-CD-MCNT:1)
      *
               IF      WRK-INPUTCD (IDX:1)     >=  X"A1"
                   MOVE    1               TO  FLG-KANJI
               ELSE
                   IF     (WRK-INPUTCD (IDX:1)
                                          IS  ALPHABETIC)  OR
                          (WRK-INPUTCD (IDX:1)
                                          IS  NUMERIC   )  OR
                          (WRK-INPUTCD (IDX:1)
                                          =   "."  OR  "/" )
                       MOVE    1              TO  FLG-SUJI
                   ELSE
                       MOVE    1              TO  FLG-KANA
                   END-IF
               END-IF
             END-IF
           END-PERFORM.
      *
      *    コード変更の有無
           IF      SPA-COM-ICD-INPUTCD(SPA-GMN-IDX4)
                                           =   SPACE
      *        入力コードが無い時、点数コードと判定する
               IF      SPA-COM-INPUTCD(SPA-GMN-IDX4)
                                           =   WRK-CD
                   MOVE    ZERO                TO  FLG-NYURYOKU
               ELSE
                   MOVE    1                   TO  FLG-NYURYOKU
               END-IF
           ELSE
      *        入力コードがある時、入力コードと判定する
               IF     (SPA-COM-ICD-INPUTCD(SPA-GMN-IDX4)
                                           =   WRK-CD  )  OR
                      (SPA-COM-INPUTCD(SPA-GMN-IDX4)
                                           =   WRK-CD  )
                   MOVE    ZERO                TO  FLG-NYURYOKU
               ELSE
                   MOVE    1                   TO  FLG-NYURYOKU
               END-IF
           END-IF.
      *
      *    半角カナは入力不可
           IF      FLG-KANA            =   1
               MOVE    "0005"          TO  SPA-ERRCD
           END-IF.
      *    半角、全角混在は入力不可
           IF     (FLG-KANJI           =   1)  AND
                  (FLG-SUJI            =   1)
               MOVE    "0005"          TO  SPA-ERRCD
           END-IF.
      *
       4101-KOUI-INPUTCD-CHOSA-EXT.
           EXIT.
      *
      *****************************************************************
      *    漢字変換処理
      *****************************************************************
       4101-KOUI-MOJI-HENKAN-SEC    SECTION.
      *    漢字変換
           MOVE    SPACE               TO  WRK-HENKAN-ATO.
           MOVE    WRK-STR             TO  IDH1.
           MOVE    ZERO                TO  IDH2.
           PERFORM   UNTIL       IDH1    >   WRK-END
             IF      WRK-HENKAN-MAE(IDH1:1)
                                          >=  X"A1"
               EVALUATE    WRK-HENKAN-MAE(IDH1:2)
                   WHEN    "０"
                       ADD     1           TO  IDH2
                       MOVE    "0"         TO  WRK-HENKAN-ATO (IDH2:1)
                   WHEN    "１"
                       ADD     1           TO  IDH2
                       MOVE    "1"         TO  WRK-HENKAN-ATO (IDH2:1)
                   WHEN    "２"
                       ADD     1           TO  IDH2
                       MOVE    "2"         TO  WRK-HENKAN-ATO (IDH2:1)
                   WHEN    "３"
                       ADD     1           TO  IDH2
                       MOVE    "3"         TO  WRK-HENKAN-ATO (IDH2:1)
                   WHEN    "４"
                       ADD     1           TO  IDH2
                       MOVE    "4"         TO  WRK-HENKAN-ATO (IDH2:1)
                   WHEN    "５"
                       ADD     1           TO  IDH2
                       MOVE    "5"         TO  WRK-HENKAN-ATO (IDH2:1)
                   WHEN    "６"
                       ADD     1           TO  IDH2
                       MOVE    "6"         TO  WRK-HENKAN-ATO (IDH2:1)
                   WHEN    "７"
                       ADD     1           TO  IDH2
                       MOVE    "7"         TO  WRK-HENKAN-ATO (IDH2:1)
                   WHEN    "８"
                       ADD     1           TO  IDH2
                       MOVE    "8"         TO  WRK-HENKAN-ATO (IDH2:1)
                   WHEN    "９"
                       ADD     1           TO  IDH2
                       MOVE    "9"         TO  WRK-HENKAN-ATO (IDH2:1)
                   WHEN    "．"
                       ADD     1           TO  IDH2
                       MOVE    "."         TO  WRK-HENKAN-ATO (IDH2:1)
                   WHEN    "＊"
                       ADD     1           TO  IDH2
                       MOVE    "*"         TO  WRK-HENKAN-ATO (IDH2:1)
                   WHEN    "　"
                       ADD     1           TO  IDH2
                       MOVE    SPACE       TO  WRK-HENKAN-ATO (IDH2:1)
                   WHEN    OTHER
                       ADD     1           TO  IDH2
                       MOVE    WRK-HENKAN-MAE(IDH1:2)
                                           TO  WRK-HENKAN-ATO (IDH2:2)
                       ADD     1           TO  IDH2
               END-EVALUATE
               ADD     2                   TO  IDH1
             ELSE
               ADD     1                   TO  IDH2
               MOVE    WRK-HENKAN-MAE(IDH1:1)
                                           TO  WRK-HENKAN-ATO (IDH2:1)
               ADD     1                   TO  IDH1
             END-IF
           END-PERFORM.
      *
       4101-KOUI-MOJI-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力文字を数字変換処理
      *****************************************************************
       4101-KOUI-INPUTCD-KENSAKU-SEC        SECTION.
      *
      *    セットの時、時間外がないこと
           IF     (WRK-CD(1:1)         =   "S" )   AND
                  (FLG-JKNKSN      NOT =   ZERO)
               MOVE    "0005"              TO  SPA-ERRCD
               GO  TO                 4101-KOUI-INPUTCD-KENSAKU-EXT
           END-IF.
      *
           MOVE    SPACE               TO  WRK-ICD-CDSYU.
           IF      FLG-KANJI           =   1
      *        全角のコードチェック
               MOVE    "J"             TO  WRK-ICD-CDSYU
           ELSE
      *        診療コードチェック
               IF      WRK-CD(1:1)         =   "P"  OR  "S"
                   MOVE    "0005"              TO  SPA-ERRCD
                   GO  TO                 4101-KOUI-INPUTCD-KENSAKU-EXT
               ELSE
                   IF      WRK-CD(1:WRK-CD-MCNT)   NUMERIC
                       EVALUATE    WRK-CD-MCNT
                           WHEN    9
      *                    診療行為コード入力
                               MOVE    WRK-CD(1:WRK-CD-MCNT)
                                                   TO  WRK-WK-CD
                               MOVE    WRK-WK-CD   TO  WRK-CD
      *                    短縮３桁
                           WHEN    3
                               MOVE    "6"         TO  WRK-ICD-CDSYU
      *                    ４桁
                           WHEN    4
                               MOVE    "4"         TO  WRK-ICD-CDSYU
      *                    ５桁
                           WHEN    5
                               MOVE    "5"         TO  WRK-ICD-CDSYU
      *                    ６桁
                           WHEN    6
                               MOVE    "6"         TO  WRK-ICD-CDSYU
      *
                           WHEN    OTHER
                               MOVE    "A"         TO  WRK-ICD-CDSYU
                       END-EVALUATE
                   ELSE
                       MOVE    "A"         TO  WRK-ICD-CDSYU
                   END-IF
               END-IF
           END-IF.
      *
      *    入力コード存在チェック
           IF     WRK-ICD-CDSYU    NOT =   SPACE
      *
               MOVE    SPACE               TO  INPUTCD-REC
               MOVE    SPA-HOSPID          TO  ICD-HOSPID
               MOVE    WRK-ICD-CDSYU       TO  ICD-CDSYU
               MOVE    WRK-CD              TO  ICD-INPUTCD
      *
               PERFORM 930-INPUTCD-READ-SEC
      *        入力コードなし
               IF      FLG-INPUTCD     NOT =   ZERO
                   IF      WRK-ICD-CDSYU       =   "4" OR "5" OR "6"
                       MOVE    "0050"          TO  SPA-ERRCD
                       GO  TO            4101-KOUI-INPUTCD-KENSAKU-EXT
                   END-IF
               END-IF
      *
      *        全桁対象がある時
               IF     (WRK-CD              =  ICD-INPUTCD  )  AND
                      (ICD-SRYCD(1:1)      =   "S"  OR  "P")
      *
      *            セットの時、時間外がないこと
                   IF      FLG-JKNKSN      NOT =   ZERO
                       MOVE    "0010"              TO  SPA-ERRCD
                       GO  TO            4101-KOUI-INPUTCD-KENSAKU-EXT
                   END-IF
      *
                   MOVE    WRK-HENKAN          TO  WRK-INPUTCD
                   MOVE    ICD-SRYCD(1:6)      TO  WRK-HENKAN(1:6)
                   COMPUTE IDY                 =   WRK-CD-MCNT   +  1
                   MOVE    WRK-INPUTCD(IDY:)   TO  WRK-HENKAN(7:)
               ELSE
                   IF     (WRK-CD              =   ICD-INPUTCD)
      *                点数コード決定
                       MOVE    ICD-SRYCD           TO  WRK-CD
                   ELSE
      *                一覧表示へ
                       MOVE    "1"                 TO  WRK-KENSAKU
                   END-IF
               END-IF
           END-IF.
      *
       4101-KOUI-INPUTCD-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院行為マスタ更新処理
      *****************************************************************
       4501-NYUKOUI-KOU-SEC            SECTION.
      *
           IF      SPA-SEL-RRKYMD      =   SPACE
      *        受診日選択なしの時、対象の剤をクリアする
               PERFORM 45012-DBDELETE-SEC
               MOVE    SPA-ZAINUM          TO  WRK-ZAINUM
           ELSE
      *        剤番号取得処理
               PERFORM 45013-ZAINUM-SET-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      4501-NYUKOUI-KOU-EXT
               END-IF
           END-IF.
      *
           MOVE    ZERO                TO  IDX.
           MOVE    ZERO                TO  WRK-KINGAK.
           MOVE    ZERO                TO  WRK-MEINUM.
           MOVE    ZERO                TO  WRK-ZAITEN.
           MOVE    ZERO                TO  WRK-ZAITEN-90.
           MOVE    ZERO                TO  WRK-SRYCDKEI.
           MOVE    ZERO                TO  WRK-SURYOKEI.
           MOVE    ZERO                TO  WRK-MEISAISU.
           INITIALIZE                      WRK-WKSRY-AREA.
      *
      *    入院行為新規作成
           MOVE    ZERO                TO  IDS.
           MOVE    ZERO                TO  WRK-RENNUM.
           INITIALIZE                  NYUINACT-REC.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-MAX
               IF      SPA-GMN-INPUTCD (IDX)(1:1)  =   "."
                   CONTINUE
               ELSE
                   ADD     1               TO  IDS
      *            入院行為内容編集
                   PERFORM 45101-NYUKOUI-HEN-SEC
      *
                   IF      IDS             >=  5
      *                新規レコード追加
                       PERFORM 45102-NYUKOU-INS-SEC
                   END-IF
               END-IF
           END-PERFORM.
      *
           IF      IDS                 >   ZERO
      *        新規レコード追加
               PERFORM 45102-NYUKOU-INS-SEC
           END-IF.
      *
       4501-NYUKOUI-KOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤のチェック処理
      *****************************************************************
       45011-NAIYOU-CHK-SEC              SECTION.
      *
      *    診療コード計計算
           MOVE    ZERO                TO  WRK-KINGAK.
           MOVE    ZERO                TO  WRK-MEINUM.
           MOVE    ZERO                TO  WRK-ZAITEN.
           MOVE    ZERO                TO  WRK-SRYCDKEI.
           MOVE    ZERO                TO  WRK-SURYOKEI.
           MOVE    ZERO                TO  WRK-MEISAISU.
           INITIALIZE                      WRK-WKSRY-AREA.
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
             IF    SPA-GMN-INPUTCD(IDX)(1:1)   =   "." 
               CONTINUE
             ELSE
      *
      *        自費
               IF      SPA-COM-KEIFLG (IDX)    =   "1"  OR  "2"
                   ADD     SPA-COM-KEI  (IDX)  TO  WRK-KINGAK
               END-IF
      *
      *        画面の計を集計する（１剤で１件）
               IF      SPA-COM-ZAIEND (IDX)    =   "1"
                   IF      SPA-COM-KEIFLG (IDX)    =   "1"  OR  "2"
                       MOVE    WRK-KINGAK              TO  WRK-ZAITEN
                   ELSE
                       MOVE    SPA-COM-TENSU  (IDX)    TO  WRK-ZAITEN
                   END-IF
                   MOVE    SPA-COM-SYUTEN1(IDX)    TO  WRK-SYUTEN1
                   MOVE    SPA-COM-SYUTEN2 (IDX)   TO  WRK-SYUTEN2
                   MOVE    SPA-COM-YKZTEN  (IDX)   TO  WRK-YKZTEN
                   MOVE    SPA-COM-KIZAITEN(IDX)   TO  WRK-KIZAITEN
               END-IF
      *        約束セット
               IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "S"
                   CONTINUE
               ELSE
      *            診療コード計
                   MOVE    SPA-COM-INPUTCD(IDX)    TO  WRK-SRYCD-9
                   ADD     WRK-SRYCD-9             TO  WRK-SRYCDKEI
      *            数量
                   ADD     SPA-COM-SURYO  (IDX)    TO  WRK-SURYOKEI
               END-IF
      *        明細数
               ADD     1                       TO  WRK-MEISAISU
             END-IF
      *
           END-PERFORM.
      *
           MOVE    ZERO                TO  WRK-ZAINUM.
           MOVE    ZERO                TO  FLG-OK.
           MOVE    ZERO                TO  FLG-OK2.
      *    入院会計マスターを特定する
           INITIALIZE                      NYUINACCT-REC.
           MOVE    SPA-HOSPID          TO  NACCT-HOSPID.
           MOVE    SPA-NYUGAIKBN       TO  NACCT-NYUGAIKBN.
           MOVE    SPA-GMN-PTID        TO  NACCT-PTID.
           MOVE    SPA-I40-SRYKA-READ  TO  NACCT-SRYKA.
           MOVE    SPA-I40-SRYYMD(1:6) TO  NACCT-SRYYM.
           MOVE    SPA-SRYKBN          TO  NACCT-SRYKBN.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "NYUINACCT-KEY3"    TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "NYUINACCT-KEY3"      TO  ORC-DBPATH
               PERFORM 940-NYUINACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF.
           PERFORM UNTIL   FLG-NYUINACCT      =   1
      *
      *        診療コード計を比較する
               IF     (NACCT-HKNCOMBI        =   SPA-HKNCOMBINUM)  AND
                      (NACCT-SRYCDTOTAL      =   WRK-SRYCDKEI   )  AND
                      (NACCT-ZAITEN          =   WRK-ZAITEN     )  AND
                      (NACCT-SURYOUTOTAL     =   WRK-SURYOKEI   )  AND
                      (NACCT-MEISAISU        =   WRK-MEISAISU   ) 
                   MOVE    ZERO             TO  FLG-OK
                   MOVE    NACCT-ZAINUM     TO  WRK-H-ZAINUM
      *            診療行為内容の比較処理
                   PERFORM 45012-NYUINACCT-CHK-SEC
      *
      *            同一診療会計マスターあり
                   IF      FLG-OK          =   1
                       IF      WRK-H-ZAINUM    =   SPA-ZAINUM
                           MOVE    1               TO  FLG-OK2
                       ELSE
      *                    同一あり
                           MOVE    WRK-H-ZAINUM        TO  WRK-ZAINUM
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "NYUINACCT-KEY3"    TO  ORC-DBPATH
               PERFORM 940-NYUINACCT-READ-SEC
           END-PERFORM.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "NYUINACCT-KEY3"    TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
           IF      WRK-ZAINUM          =   ZERO
               MOVE    ZERO                TO  FLG-OK
           END-IF.
      *
       45011-NAIYOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   入院行為内容の比較処理
      *****************************************************************
       45012-NYUINACCT-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDX-SIN.
           INITIALIZE                      TBL-MAE-AREA. 
      *    入院会計マスターから入院行為マスターを検索する
           INITIALIZE                      NYUINACT-REC.
           MOVE    NACCT-HOSPID        TO  NSRY-HOSPID.
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN.
           MOVE    NACCT-PTID          TO  NSRY-PTID.
           MOVE    NACCT-SRYKA         TO  NSRY-SRYKA.
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM.
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "NYUINACT-KEY2"     TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "NYUINACT-KEY2"     TO  ORC-DBPATH
               PERFORM 900-NYUINACT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-NYUINACT
           END-IF.
           PERFORM UNTIL     (FLG-NYUINACT     =   1)
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   5)  OR
                                  (NSRY-SRYCD(IDX2)    =   SPACE)
                   ADD     1                   TO  IDX-SIN
                   MOVE    NSRY-SRYCD   (IDX2)  TO  TBL-MAE-SRYCD
                                                             (IDX-SIN)
                   MOVE    NSRY-SRYSURYO(IDX2)  TO  TBL-MAE-SRYSURYO
                                                             (IDX-SIN)
                   MOVE    NSRY-SRYKAISU(IDX2)  TO  TBL-MAE-SRYKAISU
                                                             (IDX-SIN)
                   IF      NSRY-SRYCD (IDX2)(1:1)   =   "0"  OR  "8"
                       MOVE    NSRY-HOSPID          TO  PTCOM-HOSPID
                       MOVE    NSRY-PTID            TO  PTCOM-PTID
                       MOVE    NSRY-ZAINUM          TO  PTCOM-ZAINUM
                       MOVE    NSRY-SRYCD (IDX2)    TO  PTCOM-SRYCD
                       MOVE    NSRY-INPUTNUM(IDX2)  TO  PTCOM-RENNUM
                       PERFORM 960-PTCOM-READ-SEC
                       IF      FLG-PTCOM           =   ZERO
                           MOVE    PTCOM-INPUTCOMENT   TO
                                               TBL-MAE-INPUTCOMENT
                                                            (IDX-SIN)
                           MOVE    PTCOM-INPUTCHI-AREA TO
                                               TBL-MAE-ATAI-G
                                                            (IDX-SIN)
                       END-IF
                   END-IF
               END-PERFORM
               MOVE    "NYUINACT-KEY2"     TO  ORC-DBPATH
               PERFORM 900-NYUINACT-READ-SEC
           END-PERFORM.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "NYUINACT-KEY2"     TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
      *    入院行為内容のみテーブルに保存する
           MOVE    ZERO                TO  IDX-ATO.
           INITIALIZE                      TBL-ATO-AREA. 
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL       IDX1        >   SPA-GMN-MAX
               IF      SPA-GMN-INPUTCD (IDX1)(1:1) NOT =   "."
                   ADD     1                   TO  IDX-ATO
                   MOVE    SPA-COM-INPUTCD (IDX1)
                                               TO  TBL-ATO-SRYCD
                                                             (IDX-ATO)
                   MOVE    SPA-COM-SURYO   (IDX1)
                                               TO  TBL-ATO-SRYSURYO
                                                             (IDX-ATO)
                   IF      SPA-COM-DATAKBN (IDX1)   =   "3"
                       MOVE    SPA-COM-BUNGSU (IDX)
                                               TO  TBL-ATO-SRYKAISU
                                                             (IDX-ATO)
                   END-IF
      *            入力コメント
                   IF      SPA-COM-MEIFLG (IDX1)    =   "1"
                       IF      SPA-COM-INPUTCD (IDX1)(1:2)
                                                       =   "83"
                           MOVE    SPA-GMN-MEISYO   (IDX1)
                                               TO  TBL-ATO-INPUTCOMENT
                                                             (IDX-ATO)
                       ELSE
                           MOVE    SPA-GMN-MEISYO-G (IDX1)
                                               TO  TBL-ATO-INPUTCOMENT
                                                             (IDX-ATO)
                       END-IF
                   ELSE
                       IF     (SPA-COM-INPUTCD (IDX1)(1:1)
                                                       =   "8" )  AND
                              (SPA-COM-INPUTCHI-G(IDX1)
                                                   NOT =   SPACE)
                           MOVE    SPA-GMN-MEISYO(IDX)
                                               TO  TBL-ATO-INPUTCOMENT
                                                             (IDX-ATO)
                       END-IF
                   END-IF
                   MOVE    SPA-COM-INPUTCHI-G(IDX1)
                                               TO  TBL-ATO-ATAI-G
                                                             (IDX-ATO)
               END-IF
           END-PERFORM.
      *
      *    入院行為数が違う
           IF      IDX-SIN         NOT =   IDX-ATO
               GO      TO      45012-NYUINACCT-CHK-EXT
           END-IF.
           MOVE    ZERO                TO  FLG-ERR
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   IDX-ATO)  OR
                              (FLG-ERR     =   1      )
               MOVE    ZERO                TO  FLG-ARI
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   IDX-SIN)  OR
                                  (FLG-ARI     =   1      )
                   IF      TBL-ATO-REC (IDX1)  =   TBL-MAE-REC(IDX2)
                        MOVE    1                  TO  FLG-ARI
                   END-IF
               END-PERFORM
               IF      FLG-ARI             =   ZERO
                    MOVE    1                  TO  FLG-ERR
               END-IF
           END-PERFORM.
      *
           IF      FLG-ERR             =   ZERO
               MOVE    1               TO  FLG-OK
           END-IF.
      *
       45012-NYUINACCT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象の剤クリア処理
      *****************************************************************
       45012-DBDELETE-SEC              SECTION.
      *
      *    患者コメントをクリアする
           INITIALIZE                  PTCOM-REC.
           MOVE    SPA-HOSPID          TO  PTCOM-HOSPID.
           MOVE    SPA-GMN-PTID        TO  PTCOM-PTID.
           MOVE    SPA-ZAINUM          TO  PTCOM-ZAINUM.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC.
           MOVE    "PTCOM-KEY2"        TO  ORC-DBPATH.
           MOVE    "DBDELETE"          TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
      *    入院行為データ削除
           INITIALIZE                      NYUINACT-REC.
           MOVE    SPA-HOSPID          TO  NSRY-HOSPID.
           MOVE    SPA-NYUGAIKBN       TO  NSRY-NYUGAIKBN.
           MOVE    SPA-GMN-PTID        TO  NSRY-PTID.
           MOVE    SPA-I40-SRYKA-READ  TO  NSRY-SRYKA.
           MOVE    SPA-I40-SRYYMD(1:6) TO  NSRY-SRYYM.
           MOVE    SPA-ZAINUM          TO  NSRY-ZAINUM.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC.
           MOVE    "DBDELETE"          TO  MCP-FUNC.
           MOVE    "NYUINACT-DEL11"    TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "I44 NYUINACT DEL ERR;"    MCP-RC
           END-IF.
      *
       45012-DBDELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤番号取得処理
      *****************************************************************
       45013-ZAINUM-SET-SEC          SECTION.
      *
      *    患者マスターより、最終剤番号を再番し、患者マスターを更新する
           INITIALIZE                      PTINF-REC.
           MOVE    SPA-HOSPID          TO  PTINF-HOSPID.
           MOVE    SPA-GMN-PTID        TO  PTINF-PTID.
           PERFORM 940-PTINF-READ-SEC.
           IF      FLG-PTINF           =   1
               MOVE    "0002"          TO  SPA-ERRCD
               DISPLAY "I44 ZAINUM-SET ERR:" SPA-ERRCD
               GO      TO      45013-ZAINUM-SET-EXT
           END-IF.
      *
      *    最大剤番号
           ADD     1                   TO  PTINF-MAXZAINUM.
           MOVE    PTINF-MAXZAINUM     TO  WRK-ZAINUM.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC.
           MOVE    "DBUPDATE"          TO  MCP-FUNC.
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "PTINF UPDTE ERR:"   MCP-RC
               MOVE    "1028"              TO  SPA-ERRCD
           END-IF.
      *
       45013-ZAINUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院行為内容編集処理
      *****************************************************************
       45101-NYUKOUI-HEN-SEC              SECTION.
      *
      *    行為コード
           MOVE    SPA-COM-INPUTCD(IDX)    TO  NSRY-SRYCD   (IDS).
      *
      *    約束セット
           IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "S"
      *        行為コード
               MOVE    SPA-GMN-INPUTCD(IDX)(1:6)   TO  NSRY-SRYCD (IDS)
           END-IF.
      *
      *    数量
           MOVE    SPA-COM-SURYO  (IDX)    TO  NSRY-SRYSURYO(IDS).
      *    回数
           IF      SPA-COM-DATAKBN (IDX)   =   "3"
               MOVE    SPA-COM-BUNGSU (IDX)
                                           TO  NSRY-SRYKAISU(IDS)
           END-IF.
      *    自動算定区分
           MOVE    SPA-COM-AUTOKBN (IDX)   TO  NSRY-AUTOKBN (IDS)
      *    時間外区分入力がある時、英字変換後セット
           IF      SPA-COM-AUTOKBN (IDX)    =   SPACE
                PERFORM 45022-JIKANGAI-SET-SEC
           END-IF.
      *    入力コメント番号
           IF      SPA-COM-INPUTCD(IDX)(1:1)   =   "0"  OR  "8"
               PERFORM 450111-KNJCOM-SYROI-SEC
               MOVE    WRK-MEINUM          TO  NSRY-INPUTNUM (IDS)
           END-IF.
      *    自費
           IF      SPA-COM-KEIFLG (IDX)    =   "1"  OR  "2"
               MOVE    SPA-COM-KEI  (IDX)      TO  NSRY-JIHIMONEY(IDS)
               ADD     SPA-COM-KEI  (IDX)      TO  WRK-KINGAK
               MOVE    WRK-KINGAK              TO  NSRY-JIHIMONEYTOTAL
           END-IF.
      *    画面の計を集計する（１剤で１件）
           IF      SPA-COM-ZAIEND (IDX)    =   "1"
               IF      SPA-COM-KEIFLG (IDX)   =   "1"  OR  "2"
                   MOVE    WRK-KINGAK              TO  WRK-ZAITEN
               ELSE
                   MOVE    SPA-COM-TENSU  (IDX)    TO  WRK-ZAITEN
               END-IF
               MOVE    SPA-COM-SYUTEN1(IDX)    TO  WRK-SYUTEN1
               MOVE    SPA-COM-SYUTEN2 (IDX)   TO  WRK-SYUTEN2
               MOVE    SPA-COM-YKZTEN  (IDX)   TO  WRK-YKZTEN
               MOVE    SPA-COM-KIZAITEN(IDX)   TO  WRK-KIZAITEN
           END-IF.
      *
      *    集計
           IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "S"
               CONTINUE
           ELSE
      *        診療コード計
               MOVE    SPA-COM-INPUTCD(IDX)    TO  WRK-SRYCD-9
               ADD     WRK-SRYCD-9             TO  WRK-SRYCDKEI
      *        数量
               ADD     SPA-COM-SURYO  (IDX)    TO  WRK-SURYOKEI
           END-IF.
      *
           ADD     SPA-COM-TEN (IDX)       TO  WRK-ZAITEN-90.
      *
      *    明細数
           ADD     1                       TO  WRK-MEISAISU.
      *
       45101-NYUKOUI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    時間外区分 編集処理
      *****************************************************************
       45022-JIKANGAI-SET-SEC            SECTION.
      *
           IF     (SPA-GMN-INPUTCD(IDX)(1:1)       NUMERIC  ) AND
                  (SPA-GMN-INPUTCD(IDX)(2:1)       =   SPACE) AND
                  (SPA-GMN-INPUTCD(IDX)(3:1)   NOT =   SPACE)
               EVALUATE    SPA-GMN-INPUTCD (IDX)(1:1)
                   WHEN    "1"
                       MOVE    "A"      TO  NSRY-AUTOKBN  (IDS)
                   WHEN    "2"
                       MOVE    "B"      TO  NSRY-AUTOKBN  (IDS)
                   WHEN    "3"
                       MOVE    "C"      TO  NSRY-AUTOKBN  (IDS)
                   WHEN    "4"
                       MOVE    "D"      TO  NSRY-AUTOKBN  (IDS)
               END-EVALUATE
           END-IF.
      *
       45022-JIKANGAI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメント作成処理
      *****************************************************************
       450111-KNJCOM-SYROI-SEC           SECTION.
      *
           ADD     1                         TO  WRK-MEINUM.
      *
           INITIALIZE                        PTCOM-REC.
      *医療機関ＩＤ
           MOVE    SPA-HOSPID                TO  PTCOM-HOSPID.
      *患者ＩＤ
           MOVE    SPA-GMN-PTID              TO  PTCOM-PTID.
      *剤番号
           MOVE    WRK-ZAINUM                TO  PTCOM-ZAINUM.
      *診療コード
           MOVE    NSRY-SRYCD   (IDS)        TO  PTCOM-SRYCD.
      *コメント枝番番号
           MOVE    WRK-MEINUM                TO  PTCOM-RENNUM.
      *入力コメント
           MOVE    SPA-GMN-MEISYO-G (IDX)    TO  PTCOM-INPUTCOMENT.
      *入力値
           MOVE    SPA-COM-INPUTCHI-G (IDX)  TO  PTCOM-INPUTCHI-AREA.
      *
           MOVE    PTCOM-REC                 TO  MCPDATA-REC.
           MOVE    "PTCOM-KEY"               TO  ORC-DBPATH.
           MOVE    "DBINSERT"                TO  MCP-FUNC.
           CALL    "ORCMCPSUB"               USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC.
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "I44 PTCOM INS ERR;"    MCP-RC
               MOVE    "1029"              TO  SPA-ERRCD
           END-IF.
      *
       450111-KNJCOM-SYROI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院行為新規レコード編集
      *****************************************************************
       45102-NYUKOU-INS-SEC            SECTION.
      *
           ADD     1                     TO  WRK-RENNUM.
           MOVE    SPA-HOSPID            TO  NSRY-HOSPID.
           MOVE    SPA-NYUGAIKBN         TO  NSRY-NYUGAIKBN.
           MOVE    SPA-GMN-PTID          TO  NSRY-PTID.
           MOVE    SPA-I40-SRYKA-READ    TO  NSRY-SRYKA.
           MOVE    SPA-I40-SRYYMD(1:6)   TO  NSRY-SRYYM.
           MOVE    WRK-ZAINUM            TO  NSRY-ZAINUM.
           MOVE    WRK-RENNUM            TO  NSRY-RENNUM.
      *
           MOVE    SPA-COM-SRYSYUKBN(01) TO  NSRY-SRYSYUKBN.
           MOVE    SPA-SRYKBN            TO  NSRY-SRYKBN.
      *
           MOVE    NYUINACT-REC          TO  MCPDATA-REC.
           MOVE    "NYUINACT-KEY"        TO  ORC-DBPATH.
           MOVE    "DBINSERT"            TO  MCP-FUNC.
           CALL    "ORCMCPSUB"           USING
                                         MCPAREA
                                         ORCMCPAREA
                                         MCPDATA-REC.
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "I44 NYUINACT INS ERR;"    MCP-RC 
               MOVE    "1029"          TO  SPA-ERRCD
           END-IF.
      *
           MOVE    ZERO                  TO  IDS.
           INITIALIZE                    NYUINACT-REC.
      *
       45102-NYUKOU-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスタ更新処理
      *****************************************************************
       4502-NYUKAI-KOU-SEC          SECTION.
      *
           INITIALIZE                      NYUINACCT-REC.
           MOVE    SPA-HOSPID          TO  NACCT-HOSPID.
           MOVE    SPA-NYUGAIKBN       TO  NACCT-NYUGAIKBN.
           MOVE    SPA-GMN-PTID        TO  NACCT-PTID.
           MOVE    SPA-I40-SRYKA-READ  TO  NACCT-SRYKA.
           MOVE    SPA-I40-SRYYMD(1:6) TO  NACCT-SRYYM.
           MOVE    SPA-SRYKBN          TO  NACCT-SRYKBN.
           MOVE    SPA-ZAINUM          TO  NACCT-ZAINUM.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "NYUINACCT-KEY"     TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "NYUINACCT-KEY"     TO  ORC-DBPATH
               PERFORM 940-NYUINACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "NYUINACCT-KEY"     TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
           IF      FLG-NYUINACCT      NOT =  ZERO
               MOVE    "1027"              TO  SPA-ERRCD
               GO      TO      4502-NYUKAI-KOU-EXT
           END-IF.
      *
      *    剤点数
           MOVE    WRK-ZAITEN-90       TO  NACCT-ZAITEN.
      *    診療コード計
           MOVE    WRK-SRYCDKEI        TO  NACCT-SRYCDTOTAL.
      *    数量計
           MOVE    WRK-SURYOKEI        TO  NACCT-SURYOUTOTAL.
      *    明細数
           MOVE    WRK-MEISAISU        TO  NACCT-MEISAISU.
      *    手技点数１
           MOVE    WRK-SYUTEN1         TO  NACCT-SYUTEN1.
      *    手技点数２
           MOVE    WRK-SYUTEN2         TO  NACCT-SYUTEN2.
      *    薬剤点数
           MOVE    WRK-YKZTEN          TO  NACCT-YKZTEN.
      *    器材点数
           MOVE    WRK-KIZAITEN        TO  NACCT-KIZAITEN.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC.
           MOVE    "DBUPDATE"          TO  MCP-FUNC.
           MOVE    "NYUINACCT-KEY"     TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "I44 NYUINACCT UPD ERR:"  MCP-RC
           END-IF.
      *
       4502-NYUKAI-KOU-EXT.
           EXIT.
      *
      *****************************************************************
      *   入院会計マスタ追加、受診履歴変更処理
      *****************************************************************
       4504-NYUACCT-ADD-SEC          SECTION.
      *
      *    変更前の入院会計マスタを修正する
           PERFORM 45041-NYUACCT-MAE-SEC.
      *
      *    入院会計マスタ追加
           PERFORM 45042-NYUACCT-INS-SEC.
      *
       4504-NYUACCT-ADD-EXT.
           EXIT.
      *****************************************************************
      *    前入院会計マスタ修正処理
      *****************************************************************
       45041-NYUACCT-MAE-SEC          SECTION.
      *
           INITIALIZE                      NYUINACCT-REC.
           MOVE    SPA-HOSPID          TO  NACCT-HOSPID.
           MOVE    SPA-NYUGAIKBN       TO  NACCT-NYUGAIKBN.
           MOVE    SPA-GMN-PTID        TO  NACCT-PTID.
           MOVE    SPA-I40-SRYKA-READ  TO  NACCT-SRYKA.
           MOVE    SPA-I40-SRYYMD(1:6) TO  NACCT-SRYYM.
           MOVE    SPA-SRYKBN          TO  NACCT-SRYKBN.
           MOVE    SPA-ZAINUM          TO  NACCT-ZAINUM.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "NYUINACCT-KEY"     TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "NYUINACCT-KEY"     TO  ORC-DBPATH
               PERFORM 940-NYUINACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF.
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "NYUINACCT-KEY"     TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
           IF      FLG-NYUINACCT    NOT =  ZERO
               MOVE    "1027"              TO  SPA-ERRCD
               GO      TO      45041-NYUACCT-MAE-EXT
           END-IF.
      *
      *    剤回数
           MOVE    SPA-SEL-RRKYMD(7:2) TO  IDX-D2.
           COMPUTE IDX-D1              =   SPA-SEL-RENNUM   +  1.
           MOVE    NACCT-DAY (IDX-D2)
                                       TO  WRK-DAY.
           MOVE    ZERO                TO  NACCT-DAY
                                                (IDX-D2).
           COMPUTE NACCT-ZAIKAISU      =   NACCT-ZAIKAISU
                                       -   WRK-DAY.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC.
           MOVE    "DBUPDATE"          TO  MCP-FUNC.
           MOVE    "NYUINACCT-KEY"     TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "I44 NYUINACCT UPD ERR:"  MCP-RC
           END-IF.
      *
       45041-NYUACCT-MAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスタ追加処理
      *****************************************************************
       45042-NYUACCT-INS-SEC          SECTION.
      *
           INITIALIZE                      NYUINACCT-REC.
           MOVE    SPA-HOSPID          TO  NACCT-HOSPID.
           MOVE    SPA-NYUGAIKBN       TO  NACCT-NYUGAIKBN.
           MOVE    SPA-GMN-PTID        TO  NACCT-PTID.
           MOVE    SPA-I40-SRYKA-READ  TO  NACCT-SRYKA.
           MOVE    SPA-I40-SRYYMD(1:6) TO  NACCT-SRYYM.
           MOVE    SPA-SRYKBN          TO  NACCT-SRYKBN.
      *
           MOVE    WRK-ZAINUM          TO  NACCT-ZAINUM.
      *    保険組合せ
           MOVE    SPA-HKNCOMBINUM     TO  NACCT-HKNCOMBI.
      *    剤点数
           MOVE    WRK-ZAITEN          TO  NACCT-ZAITEN.
      *    診療コード計
           MOVE    WRK-SRYCDKEI        TO  NACCT-SRYCDTOTAL.
      *    数量計
           MOVE    WRK-SURYOKEI        TO  NACCT-SURYOUTOTAL.
      *    明細数
           MOVE    WRK-MEISAISU        TO  NACCT-MEISAISU.
      *    手技点数１
           MOVE    WRK-SYUTEN1         TO  NACCT-SYUTEN1.
      *    手技点数２
           MOVE    WRK-SYUTEN2         TO  NACCT-SYUTEN2.
      *    薬剤点数
           MOVE    WRK-YKZTEN          TO  NACCT-YKZTEN.
      *    器材点数
           MOVE    WRK-KIZAITEN        TO  NACCT-KIZAITEN.
      *
      *    剤回数
           MOVE    WRK-DAY             TO  NACCT-ZAIKAISU.
      *    診療日
           MOVE    SPA-SEL-RRKYMD(7:2) TO  IDX-D2.
           COMPUTE IDX-D1              =   SPA-SEL-RENNUM   +  1.
           MOVE    WRK-DAY             TO  NACCT-DAY
                                             (IDX-D2).
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC.
           MOVE    "DBINSERT"          TO  MCP-FUNC.
           MOVE    "NYUINACCT-KEY"     TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC          NOT =   ZERO
               MOVE    "1029"          TO  SPA-ERRCD
               DISPLAY "I44 NYUINACCT INS ERR:"  MCP-RC
           END-IF.
      *
       45042-NYUACCT-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌更新前のチェック処理
      *****************************************************************
       41001-YAKKNK-CHK-SEC            SECTION.
      *    禁忌チェック
           MOVE    1                   TO  IDX-K1.
           INITIALIZE                  WRK-KNKCHK-AREA.
      *
      *    テーブル再編集処理
           PERFORM 410012-YAKKNK-TBL-SEC.
      *
       41001-YAKKNK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    全件　禁忌薬剤チェックテーブル再編集
      *****************************************************************
       410012-YAKKNK-TBL-SEC           SECTION.
      *
           PERFORM VARYING     IDX-K1      FROM    1   BY  1
                   UNTIL      (IDX-K1          >   30  )  OR
                              (WRK-KNK-TOUSRYCD (IDX-K1)  =   SPACE)
      *        薬剤ごとに投与年月を降順にソートする
               MOVE    WRK-KNK-TBL2A(IDX-K1)   TO  WRK-SRT-TBL2A
               INITIALIZE                      WRK-KNK-TBL2A(IDX-K1)
               PERFORM VARYING     IDX-K       FROM    1   BY  1
                       UNTIL       IDX-K       >   30
                   MOVE    LOW-VALUE           TO  WRK-KEY-SRYYM
                   MOVE    HIGH-VALUE          TO  WRK-KEY-SRYCD
                   MOVE    ZERO                TO  IDX2
                   PERFORM VARYING     IDX1    FROM    1   BY  1
                           UNTIL       IDX1        >   30
                       EVALUATE    TRUE
                         WHEN    WRK-SRT-SRYYM (IDX1) >  WRK-KEY-SRYYM
                               MOVE    WRK-SRT-SRYYM (IDX1)
                                                   TO  WRK-KEY-SRYYM
                               MOVE    WRK-SRT-SRYCD   (IDX1)
                                                   TO  WRK-KEY-SRYCD
                               MOVE    IDX1        TO  IDX2
                         WHEN   (WRK-SRT-SRYYM (IDX1) =  WRK-KEY-SRYYM)
                            AND (WRK-SRT-SRYCD (IDX1) <  WRK-KEY-SRYCD)
                               MOVE    WRK-SRT-SRYYM (IDX1)
                                                   TO  WRK-KEY-SRYYM
                               MOVE    WRK-SRT-SRYCD   (IDX1)
                                                   TO  WRK-KEY-SRYCD
                               MOVE    IDX1        TO  IDX2
                         WHEN   (WRK-SRT-SRYYM (IDX1) = WRK-KEY-SRYYM)
                            AND (WRK-SRT-SRYCD (IDX1) = WRK-KEY-SRYCD)
                               MOVE    WRK-SRT-SRYYM (IDX1)
                                                   TO  WRK-KEY-SRYYM
                               MOVE    WRK-SRT-SRYCD   (IDX1)
                                                   TO  WRK-KEY-SRYCD
                               MOVE    IDX1        TO  IDX2
                       END-EVALUATE
                   END-PERFORM
                   IF      IDX2                >   ZERO
                       MOVE    WRK-SRT-TBL2(IDX2)  TO  WRK-KNK-TBL2
                                                       (IDX-K1 IDX-K)
                       INITIALIZE                  WRK-SRT-TBL2 (IDX2)
                   END-IF
               END-PERFORM
           END-PERFORM.
      *
       410012-YAKKNK-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌エラーメッセージ表示（入力ごと）
      *****************************************************************
       510229-POP-ERR-SEC              SECTION.
      *
           MOVE    SPACE               TO  I47.
           MOVE    WRK-KNK-TOUMEI (1)  TO  I47-POP-TOUMEI.
      *
           MOVE    ZERO                TO  IDX-X.
           PERFORM VARYING     IDX-K   FROM    1   BY  1
                   UNTIL      (IDX-K       >   30)   OR
                              (IDX-X       >=  3 )  
               IF      WRK-KNK-KNKMEI (1 IDX-K)    NOT =   SPACE
                   ADD     1               TO  IDX-X
                   MOVE    WRK-KNK-KNKMEI (1 IDX-K)
                                           TO  I47-KNKMEI (IDX-X)
               END-IF
           END-PERFORM.
      *
           MOVE    IDX-X               TO  I47-COUNT.
      *    初期カーソル設定
           MOVE    "B01"               TO  MCP-WIDGET.
      *
           MOVE    "I47"               TO  SPA-SAKIPG.
           MOVE    "I44"               TO  SPA-MOTOPG.
      *
           MOVE    SPA-I4KYOUTU        TO  SPA-WORK.
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "I47"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       510229-POP-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌エラーメッセージ表示（登録前）
      *****************************************************************
       410019-POP-J041-SEC              SECTION.
      *
           MOVE    SPACE               TO  I46.
      *
           MOVE    1                   TO  IDX-K.
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   30)   OR
                              (IDX-K       >=  50)
                IF      WRK-KNK-TOUMEI (IDX1)  NOT =   SPACE
                    MOVE    WRK-KNK-TOUMEI (IDX1)  TO  I46-TOUMEI
                                                            (IDX-K)
                END-IF
                PERFORM VARYING    IDX2    FROM    1   BY  1
                       UNTIL      (IDX2                    >  30)   OR
                                  (WRK-KNK-TOUMEI (IDX1)   =  SPACE)
                   IF      WRK-KNK-KNKMEI (IDX1 IDX2)  NOT =  SPACE
                       MOVE    WRK-KNK-KNKMEI (IDX1 IDX2)
                                               TO  I46-KNKMEI (IDX-K)
                       MOVE    WRK-KNK-SRYYM(IDX1 IDX2)
                                               TO  WRK-SYMD(1:6)
                       MOVE    01              TO  WRK-SDD
                       PERFORM 520-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG     TO  I46-TOUYM  (IDX-K)
                       ADD     1               TO  IDX-K
                   END-IF
               END-PERFORM
           END-PERFORM.
      *
           IF      IDX-K               >   1
               COMPUTE IDX-K           =   IDX-K       -   1
           END-IF.
           MOVE    IDX-K               TO  I46-COUNT.
      *
      *    初期カーソル設定
           MOVE    "B12"               TO  MCP-WIDGET.
      *
           MOVE    "I46"               TO  SPA-SAKIPG.
           MOVE    "I44"               TO  SPA-MOTOPG.
           MOVE    SPA-I4KYOUTU        TO  SPA-WORK.
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "I46"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       410019-POP-J041-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       520-SEIWA-HEN-SEC             SECTION.
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
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF.
      *
       520-SEIWA-HEN-EXT.
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
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "I44    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
      *    行位置とカーソル位置の決定
           PERFORM 5002-GMNSPA-HEN-SEC.
      *
           MOVE    SPACE               TO  I44.
           INITIALIZE                      I44.
      *
           MOVE    ZERO                TO  IDX.
      *
           PERFORM VARYING     WRK-IDX     FROM    1   BY  1
                   UNTIL      (WRK-IDX     >   200)  OR
                              (IDX         >=  20 )
      *        画面表示
               IF      SPA-COM-PAGE(WRK-IDX)   =   SPA-GMN-PAGE4
                   ADD     1                TO  IDX
                   MOVE    SPA-GMN-SRYKBN (WRK-IDX)
                                            TO  I44-SRYKBN(IDX)
                   MOVE    SPA-GMN-INPUTCD(WRK-IDX)
                                            TO  I44-INPUTCD(IDX)
                   MOVE    SPA-GMN-MEISYO-G(WRK-IDX)
                                            TO  I44-MEISYO(IDX)
                   MOVE    SPA-COM-TEN(WRK-IDX)
                                            TO  WRK-TENSU-Z
                   MOVE    WRK-TENSU-X      TO  SPA-GMN-TENSU(WRK-IDX)
                   MOVE    SPACE            TO  WRK-SURYO-HENSYU
                   MOVE    WRK-TENSU-X      TO  WRK-H-ZAIKEI
                   MOVE    WRK-SURYO-HENSYU TO  I44-SURYO (IDX)
               END-IF
      *
           END-PERFORM.
      *
           MOVE    SPA-GMN-PTNUM       TO  I44-PTNUM.
           MOVE    SPA-GMN-KANANAME    TO  I44-KANANAME.
           MOVE    SPA-GMN-NAME        TO  I44-NAME.
           MOVE    SPA-GMN-SEX         TO  I44-SEX.
           MOVE    SPA-I40-HKNCOMBIMEI TO  I44-HKNCOMBI.
           MOVE    SPA-I40-FTNRATEMEI  TO  I44-RATE.
           MOVE    SPA-I40-SRYYMDW(1:6)    TO  I44-SRYYM.
           MOVE    SPA-GMN-BIRTHDAYW   TO  I44-BIRTHDAY.
           MOVE    SPA-I40-SRYKAMEI    TO  I44-SRYKA.
      *
           MOVE    SPA-SEL-RRKYMDG     TO  I44-RRKYMD.
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   31
               IF      SPA-WRK-CHK-KBN (IDX)   NOT =   ALL SPACE
                   MOVE    SPA-SEL-DAY (IDX)    TO  WRK-ZZ
                   MOVE    WRK-ZZ               TO  I44-DAY(IDX)
               END-IF
           END-PERFORM.
      *
           IF      (WRK-HENKAN(1:1)            <   X"A1")  AND
                   (WRK-HENKAN(1:1)         NOT =    "/")   AND
                   (SPA-ERRCD                   =   SPACE)
               MOVE    SPA-SCR-REC TO  SPA-I44GMN-AREA
           END-IF.
      *
           PERFORM 5001-MAPCUR-SEC.
      *
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    行位置とカーソル位置の決定処理
      *****************************************************************
       5002-GMNSPA-HEN-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX.
           MOVE    ZERO                TO  WRK-MAP-MAX.
           MOVE    ZERO                TO  WRK-MAP-CUR.
           MOVE    ZERO                TO  WRK-MAP-CUR2.
           MOVE    ZERO                TO  WRK-MAP-CUR3.
           MOVE    ZERO                TO  WRK-MAP-MAXPAGE.
           MOVE    ZERO                TO  WRK-MAP-PAGE.
           MOVE    ZERO                TO  WRK-MAP-PAGE2.
           MOVE    ZERO                TO  WRK-MAP-PAGE3.
           MOVE    1                   TO  WRK-PAGE.
           MOVE    ZERO                TO  SPA-GMN-MAX.
      *
           PERFORM VARYING     WRK-IDX     FROM    1   BY  1
                   UNTIL       WRK-IDX     >   200
               MOVE    ZERO                TO  SPA-COM-PAGE (WRK-IDX)
               MOVE    ZERO                TO  SPA-COM-GYOU (WRK-IDX)
      *        画面表示外のものに頁、行位置を設定する
               IF      SPA-COM-SET    (WRK-IDX)  =   SPACE
                   ADD     1                 TO  IDX
                   IF      IDX               >   20
                       ADD     1               TO  WRK-PAGE
                       MOVE    1               TO  IDX
                   END-IF
                   IF     (SPA-GMN-INPUTCD(WRK-IDX)  NOT =   SPACE) OR
                          (SPA-COM-CUR (WRK-IDX)     NOT =   SPACE)
                       MOVE    IDX             TO  WRK-MAP-MAX
                       MOVE    WRK-PAGE        TO  WRK-MAP-MAXPAGE
      *                エラー位置
                       IF      SPA-COM-CUR (WRK-IDX)   =   "1"
                           IF      WRK-MAP-CUR         =   ZERO
                              MOVE    IDX             TO  WRK-MAP-CUR
                              MOVE    WRK-PAGE        TO  WRK-MAP-PAGE
                           END-IF
                       END-IF
      *                警告位置
                       IF      SPA-COM-CUR (WRK-IDX)   =   "2"  OR  "3"
                           IF      WRK-MAP-CUR2        =   ZERO
                               MOVE    IDX             TO  WRK-MAP-CUR2
                               MOVE    WRK-PAGE        TO  WRK-MAP-PAGE2
                           END-IF
                       END-IF
      *                フリーコメントで未入力の行
                       IF     SPA-COM-IN2 (IDX)   =   "3"
                           MOVE    IDX             TO  WRK-MAP-CUR3
                           MOVE    WRK-PAGE        TO  WRK-MAP-PAGE3
                       END-IF
                   END-IF
      *
                   MOVE    WRK-PAGE        TO  SPA-COM-PAGE(WRK-IDX)
                   MOVE    IDX             TO  SPA-COM-GYOU(WRK-IDX)
      *
                   IF      SPA-ERRCD       =   SPACE
                       MOVE    SPACE           TO  SPA-COM-CUR(WRK-IDX)
                   END-IF
               END-IF
      *
               IF      (SPA-COM-INPUTCD(WRK-IDX)   =   SPACE) AND
                       (SPA-GMN-INPUTCD(WRK-IDX)   =   SPACE)
                   CONTINUE
               ELSE
                   ADD     1               TO  SPA-GMN-MAX
               END-IF
           END-PERFORM.
      *
      *    画面カーソル位置決定（警告はエラーの次に）
           MOVE    ZERO                TO  SPA-MAP-IDX4.
           IF      WRK-MAP-CUR          =   ZERO
               IF      WRK-MAP-CUR2    NOT =   ZERO
                   MOVE    WRK-MAP-CUR2        TO  SPA-MAP-IDX4
                   MOVE    WRK-MAP-PAGE2       TO  SPA-GMN-PAGE4
               END-IF
           ELSE
               MOVE    WRK-MAP-CUR         TO  SPA-MAP-IDX4
               MOVE    WRK-MAP-PAGE        TO  SPA-GMN-PAGE4
           END-IF.
      *
           IF      SPA-MAP-IDX4     NOT =   ZERO
               GO      TO      5002-GMNSPA-HEN-EXT
           END-IF.
      *    名称入力へ
           IF      WRK-MAP-CUR3    NOT =   ZERO
               MOVE    WRK-MAP-CUR3        TO  SPA-MAP-IDX4
               MOVE    WRK-MAP-PAGE3       TO  SPA-GMN-PAGE4
               MOVE    2                   TO  SPA-GMN-CUR4
               GO      TO      5002-GMNSPA-HEN-EXT
           END-IF.
      *
      *    カーソル指定のない時、次ぎの空白行へ
      *    後画面へ
           IF      SPA-GMN-CUR4        =   88  OR  99
               IF      WRK-MAP-MAXPAGE     =   SPA-GMN-PAGE4
                   COMPUTE SPA-MAP-IDX4        =   WRK-MAP-MAX   +   1
               ELSE
                   MOVE    1                   TO  SPA-MAP-IDX4
               END-IF
           ELSE
               COMPUTE SPA-MAP-IDX4        =   WRK-MAP-MAX   +   1
               IF      SPA-MAP-IDX4        >   20
                   OR  SPA-GMN-PAGE4       <   WRK-MAP-MAXPAGE
                   MOVE    99                  TO  SPA-MAP-IDX4
               ELSE
                   IF      SPA-GMN-PAGE4   >   WRK-MAP-MAXPAGE
                       MOVE    WRK-MAP-MAXPAGE     TO  SPA-GMN-PAGE4
                   END-IF
               END-IF
           END-IF.
           IF      SPA-GMN-PAGE4       =   ZERO
               MOVE    1               TO  SPA-GMN-PAGE4
           END-IF.
      *
       5002-GMNSPA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
      *    カーソルセット
           MOVE    SPA-MAP-IDX4        TO  WRK-IDX2
           IF      WRK-IDX2            =   ZERO
               MOVE    1               TO  WRK-IDX2
           END-IF.
      *
           IF      WRK-IDX2            >   20
               MOVE    00              TO  SPA-GMN-CUR4
           END-IF.
      *
           EVALUATE    SPA-GMN-CUR4
               WHEN    00
                   MOVE    "B06"           TO  MCP-WIDGET
               WHEN    01
                   MOVE    "INPUTCD"       TO  MCP-WIDGET
                   MOVE    WRK-IDX2        TO  MCP-WIDGET(8:2)
               WHEN    02
                   MOVE    "MEISYO"        TO  MCP-WIDGET
                   MOVE    WRK-IDX2        TO  MCP-WIDGET(7:2)
               WHEN    03
                   MOVE    "KEI"           TO  MCP-WIDGET
                   MOVE    WRK-IDX2        TO  MCP-WIDGET(4:2)
           END-EVALUATE.
      *
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "該当する点数マスターが存在しません"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "診療行為は数量を入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "入力コードの文字エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "診療種別がありません"
                                       TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE
                   "２００行を超えます。これ以上挿入はできません"
                                       TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "時間外の入力はできません"
                                       TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "入外適用エラー" 
                                       TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "老人適用エラー" 
                                       TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "病院診療所適用エラー" 
                                       TO  SPA-ERRMSG
               WHEN    "0024"
                   MOVE    "施設基準ではありません。算定できません"
                                       TO  SPA-ERRMSG
               WHEN    "0029"
                   MOVE    "コメントの入力値エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0030"
                   MOVE    "時間加算はできません。"
                                       TO  SPA-ERRMSG
               WHEN    "0032"
                   MOVE    "年齢関連誤り"
                                       TO  SPA-ERRMSG
               WHEN    "0033"
                   MOVE    "併用算定チェック"
                                       TO  SPA-ERRMSG
               WHEN    "0042"
                   MOVE    "注加算連番チェック"
                                       TO  SPA-ERRMSG
               WHEN    "0048"
                   MOVE    "約束セットの時、診療種別を入力して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0050"
                   MOVE    "入力コードがありません"
                                       TO  SPA-ERRMSG
               WHEN    "0075"
                   MOVE    "病床数関連誤り"
                                       TO  SPA-ERRMSG
               WHEN    "0080"
                   MOVE    "名称は全角で入力して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "1020"
                   MOVE    "区分が異なります"
                                       TO  SPA-ERRMSG
               WHEN    "1025"
                   MOVE    "既に同じ診療行為の剤が存在します。"
                                       TO  SPA-ERRMSG(1:34)
                   MOVE    "回数の変更で修正して下さい"
                                       TO  SPA-ERRMSG(35:)
               WHEN    "1027"
                   MOVE    "診療会計マスタ読込みエラー"
                                       TO  SPA-ERRMSG
               WHEN    "1028"
                   MOVE    "剤番号取り込みエラー"
                                       TO  SPA-ERRMSG
               WHEN    "1029"
                   MOVE    "コメントマスター更新エラー"
                                       TO  SPA-ERRMSG
               WHEN    "1030"
                   MOVE    "剤削除は入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "8000"
                   MOVE    "保険区分が異なります"
                                       TO  SPA-ERRMSG
               WHEN    OTHER
                   MOVE    SPA-ERRCD
                                       TO  SPA-ERRMSG
           END-EVALUATE.
      *
           MOVE    SPACE               TO  I4ERR.
           MOVE    SPA-ERRCD           TO  I4ERR-ERRCODE.
           MOVE    SPA-ERRMSG          TO  I4ERR-ERRMSG.
           MOVE    "B01"               TO  MCP-WIDGET.
      *
           MOVE    "I44"               TO  SPA-MOTOPG.
           MOVE    "I4ERR"             TO  SPA-SAKIPG.
           MOVE    SPA-I4KYOUTU        TO  SPA-WORK.
           MOVE    SPA-AREA            TO  SPA-COMMON.
           MOVE    SPA-I44-I48         TO  SPA-FREE.
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
               WHEN    "0101"
                   MOVE    "入院内容を変更登録します。"
                                       TO  WRK-JIDMSG(1:26)
                   MOVE
                   "回数も登録されますので取消ができなくなります"
                                       TO  WRK-JIDMSG(27:)
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
           MOVE    "I44"               TO  SPA-MOTOPG.
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
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-KANRIMST-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
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
      *    入院行為マスター読込
      *****************************************************************
       900-NYUINACT-READ-SEC       SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
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
       900-NYUINACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスタ読み込み
      *****************************************************************
       940-NYUINACCT-READ-SEC       SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
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
       940-NYUINACCT-READ-EXT.
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
           MOVE    SPA-I40-SRYYMD      TO  ORC-DBYMD.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "TENSU-KEY"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
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
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "TENSU-KEY"         TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスター次読込
      *****************************************************************
       920-INPUTCD-NEXT-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  INPUTCD-REC
               MOVE    ZERO                TO  FLG-INPUTCD
           ELSE
               INITIALIZE                      INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF.
      *
       920-INPUTCD-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスター読込
      *****************************************************************
       930-INPUTCD-READ-SEC         SECTION.
      *
           IF      WRK-CD-MCNT         =   20
               MOVE    "INPUTCD-KEY2"      TO  WRK-PATH
           ELSE
               MOVE    "INPUTCD-KEY"       TO  WRK-PATH
           END-IF.
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    WRK-PATH            TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    WRK-PATH            TO  ORC-DBPATH
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
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    WRK-PATH            TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
       920-INPUTCD-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメント読込
      *****************************************************************
       960-PTCOM-READ-SEC         SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "PTCOM-KEY"         TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
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
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "PTCOM-KEY"         TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
       960-PTCOM-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェックデータ読込
      *****************************************************************
       950-CHK-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  CHK-REC
               MOVE    ZERO                TO  FLG-CHKMST
           ELSE
               INITIALIZE                      CHK-REC
               MOVE    1                   TO  FLG-CHKMST
           END-IF.
      *
       950-CHK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力セットデータ次読込
      *****************************************************************
       975-INPUTSET-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  INPUTSET-REC
               MOVE    ZERO                TO  FLG-INPUTSET
           ELSE
               INITIALIZE                      INPUTSET-REC
               MOVE    1                   TO  FLG-INPUTSET
           END-IF.
      *
       975-INPSET-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       940-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
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
                   MOVE    MCPDATA-REC        TO  PTINF-REC
                   MOVE    ZERO               TO  FLG-PTINF
               ELSE
                   MOVE    1                  TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1              TO  FLG-PTINF
           END-IF.
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
       940-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ付加情報読込
      *****************************************************************
       980-TENSUPLUS-READ-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TENSUPLUS-REC
               MOVE    ZERO            TO  FLG-TENSUPLUS
           ELSE
               INITIALIZE                  TENSUPLUS-REC
               MOVE    1               TO  FLG-TENSUPLUS
           END-IF.
      *
       980-TENSUPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人一般コード変換読込
      *****************************************************************
       990-SRYCDCHG-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYCDCHG-REC
               MOVE    ZERO                TO  FLG-SRYCDCHG
           ELSE
               INITIALIZE                      SRYCDCHG-REC
               MOVE    1                   TO  FLG-SRYCDCHG
           END-IF.
      *
       990-SRYCDCHG-READ-EXT.
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
