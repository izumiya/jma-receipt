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
       PROGRAM-ID.                 ORCGK02NYU.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為入力（入院）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/10/17    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  02/11/20  自賠責器材追加
      * 01.00.02     NACL-多々納  02/12/02  食事追加
      * 01.00.03     NACL-多々納  02/12/17  算定チェック修正
      * 01.00.04     NACL-多々納  03/02/19  約束セット名称編集修正
      * 01.00.05     NACL-多々納  03/03/05  排他制御処理追加
      * 01.00.06     NACL-多々納  03/03/18  中途終了時修正
      * 01.00.07     NACL-多々納  03/04/11  科変更追加、履歴に科表示他
      * 01.00.08     NACL-多々納  03/05/29  入院アフターケアエラー
      * 01.00.09     NACL-多々納  03/06/09  入院日指定修正
      * 01.00.10     NACL-多々納  03/07/09  時間外区分設定修正
      * 01.00.11     NACL-多々納  03/08/01  数量入力判定追加
      * 01.00.12     NACL-多々納  04/02/20  セット展開変更
      * 01.00.13     NACL-多々納  05/01/20  ヘルプ追加
      * 01.00.14     NACL-多々納  05/02/20  入院指示せん発行追加他
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    パラメタデータ
           SELECT  PARA-FILE       ASSIGN  FILE-NAME2
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-PARA.
      *
      *    ＳＰＡデータ
           SELECT  SPASCR-FILE     ASSIGN  FILE-NAME1
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-SPASCR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    パラメタデータ
       FD  PARA-FILE.
       01  PARA-REC.
           03  PARA-KEY.
               05  PARA-FILEMEI         PIC X(08).
               05  PARA-EDANUM          PIC 9(03).
           03  PARA-DATA-REC            PIC X(60000).
      *
      *    ＳＰＡパラメタデータ
       FD  SPASCR-FILE.
       01  SPA-DATA-REC            PIC X(30000).
      *
       WORKING-STORAGE             SECTION.
      *
      * ＳＰＡデータ
       01  FILE-NAME1.
           03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
           03  FILLER              PIC X(09)   VALUE   "K02SPASCR".
           03  FILE-TERMID1        PIC X(64)   VALUE   SPACE.
           03  FILLER              PIC X(04)   VALUE   ".TXT".
      *
      *    パラメタファイル名
       01  FILE-NAME2.
           03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
           03  FILLER              PIC X(07)   VALUE   "K01PARA".
           03  FILE-TERMID2        PIC X(64)   VALUE   SPACE.
           03  FILLER              PIC X(06)   VALUE   ".TXT".
      *
       01  WRK-DATA-REC.
           03  WRK-DATA            PIC X(30000)
                                   OCCURS  10.
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    ＳＰＡパラメタ
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "K02SCR-SPA".
      *
      *    画面Ｋ０８１スパ領域
           COPY    "K081SCR-SPA".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
           03  STS-SPASCR          PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SPASCR          PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-END2            PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-KNJBYO          PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-CHKMST          PIC 9(01).
           03  FLG-INPUTSET        PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTBYOMEI        PIC 9(01).
           03  FLG-SSKIJYO         PIC 9(01).
           03  FLG-INTERACT        PIC 9(01).
           03  FLG-PTNYUINRRK        PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-HKNCOMBICHK     PIC 9(01).
           03  FLG-GMN             PIC 9(01).
           03  FLG-KINKI           PIC 9(01).
           03  FLG-KINKI-ARI       PIC 9(01).
           03  FLG-KINKI-ARI2      PIC 9(01).
           03  FLG-ALLFLG          PIC 9(01).
           03  FLG-KANA            PIC 9(01).
           03  FLG-SUJI            PIC 9(01).
           03  FLG-TOROKU          PIC X(01).
           03  FLG-SYORI           PIC X(01).
      *
           03  FLG-SST             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-OK2             PIC 9(01).
           03  FLG-OK3             PIC 9(01).
           03  FLG-ADD             PIC 9(01).
      *    初期画面かの判定
           03  FLG-SYOKI           PIC 9(01).
      *
           03  FLG-NAIYAK          PIC X(01).
           03  FLG-TONPUK          PIC X(01).
           03  FLG-GAIYAK          PIC X(01).
           03  FLG-ARI             PIC 9(01).
      *
      *****03  FLG-SYOSIN          PIC 9(01).
      *    診察料算定済み
           03  FLG-SINSATU          PIC 9(01).
      *    小児科外来診療科算定
           03  FLG-SYONIKA         PIC 9(01).
      *    算定履歴有無
           03  FLG-JYURR-ARI       PIC 9(01).
      *    時間外加算用
           03  FLG-DELFLG          PIC 9(01).
           03  FLG-ZAIEND          PIC 9(01).
      *----(01.00.01) LINE ADD START ----------------------------------
      *    セット処理有無
           03  FLG-SETSYORI        PIC 9(01).
           03  FLG-SET             PIC 9(01).
           03  FLG-YAKUSOKU        PIC 9(01).
           03  FLG-MAE             PIC 9(01).
      *    剤チェック対象外
           03  FLG-TAISYO          PIC 9(01).
      *    回数入力なし
           03  FLG-KAISU          PIC 9(01).
      *    自費入力
           03  FLG-JIHI           PIC 9(01).
      *
      *    処方せんフラグ
           03  FLG-SYOHOU         PIC 9(01).
           03  FLG-SYOHOU-ARI     PIC 9(01).
      *
           03  FLG-PARASET         PIC 9(01).
      *    検索種別
           03  FLG-KOUI            PIC 9(01).
      *    入力有無
      **** 03  FLG-SYORINASI       PIC 9(01).
      *    治癒有無
           03  FLG-KEIZOKU         PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-INS             PIC 9(01).
      *
           03  FLG-KUHAKU          PIC 9(01).
      *
           03  FLG-CHK2            PIC 9(01).
           03  FLG-ARI2            PIC 9(01).
           03  FLG-NYU-ARI         PIC 9(01).
           03  FLG-ZAIBETU         PIC 9(01).
      *
           03  FLG-K09TUIKA           PIC 9(01).
      *    入力なし
           03  FLG-INPUT-NASI      PIC 9(01).
      *
           03  FLG-HEN-CHK         PIC 9(01).
      *
           03  FLG-IN1             PIC 9(01).
      *
           03  FLG-FIRUMU           PIC 9(01).
      *
           03  FLG-SONOTA           PIC 9(01).
           03  FLG-NAIFUTAN         PIC 9(01).
      *
           03  FLG-RIHTEI           PIC 9(01).
      *
           03  FLG-TEISEI           PIC 9(01).
           03  FLG-MOD-FLG           PIC 9(01).
      *    画面変更
           03  FLG-HENKOU            PIC 9(01).
      *    入院指示せん
           03  FLG-SIJISEN           PIC 9(01).
      *    初期
           03  FLG-INIT              PIC 9(01).
           03  FLG-MAE-TENSU         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(04).
           03  IDK1                PIC 9(04).
           03  IDK2                PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-SA              PIC 9(04).
      *
           03  IDX-1               PIC 9(04).
      *
           03  IDX-ERR             PIC 9(04).
           03  IDX-SIN             PIC 9(04).
           03  IDX-SIN2            PIC 9(04).
      *
           03  IDX-STR             PIC 9(04).
      *
           03  IDX-P               PIC 9(04).
           03  IDX-K               PIC 9(04).
           03  IDX-KAI             PIC 9(04).
           03  IDX-KAI2            PIC 9(04).
           03  IDX-K1              PIC 9(04).
           03  IDX-K2              PIC 9(04).
           03  IDX-K3              PIC 9(04).
           03  IDX-K4              PIC 9(04).
           03  IDX-C               PIC 9(04).
           03  IDX-X               PIC 9(04).
           03  IDX-S2              PIC 9(04).
           03  IDX-IP              PIC 9(04).
           03  IDX-SST             PIC 9(04).
      *
           03  IDX-Y               PIC 9(04).
           03  IDX-Y2              PIC 9(02).
           03  IDX-Y3              PIC 9(02).
      *
           03  IDX-YAKUSOKU        PIC 9(04).
           03  IDX-YAK             PIC 9(04).
      *
           03  IDX-K98             PIC 9(04).
      *
           03  IDX-HKN             PIC 9(04).
      *
           03  IDN1                PIC 9(04).
      *
           03  IDX-GG              PIC 9(04).
           03  WRK-MAX             PIC 9(04).
           03  WRK-STR             PIC 9(04).
           03  IDX-G               PIC 9(04).
      *
           03  IDX-SET             PIC 9(04).
           03  IDX-TEI             PIC 9(04).
      *
           03  IDX-NUM             PIC 9(06).
      *
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-YMDS1.
               05  WRK-YYS1        PIC 9(04).
               05  WRK-MMS1        PIC 9(02).
               05  WRK-DDS1        PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2        PIC 9(04).
               05  WRK-MMS2        PIC 9(02).
               05  WRK-DDS2        PIC 9(02).
           03  WRK-SRYYM.
               05  WRK-SRYYY       PIC 9(04).
               05  WRK-SRYMM       PIC 9(02).
      *
           03  WRK-TOUYMD.
               05  WRK-TOUYY       PIC 9(04).
               05  WRK-TOUMM       PIC 9(02).
               05  WRK-TOUDD       PIC 9(02).
      *
           03  WRK-FREE            PIC 9(04).
           03  WRK-IDX             PIC 9(03).
      *    入力項目名編集用 
           03  WRK-IDX2            PIC 9(02).
           03  WRK-IDX3            PIC 9(02).
           03  WRK-WIDGET          PIC X(20).
           03  WRK-IN-MAP-IDX      PIC 9(02).
      *    年齢判定用
           03  WRK-BIRTHDAY.
               05  WRK-BIRTH-YY    PIC 9(04).
               05  WRK-BIRTH-MM    PIC 9(02).
               05  WRK-BIRTH-DD    PIC 9(02).
           03  WRK-NYUYOJI         PIC 9(04).
      *
           03  WRK-TENSU-X.
               05  WRK-TENSU-Z     PIC ZZZZZZZZ.
           03  WRK-KAISU-X.
               05  WRK-KAISU-Z     PIC ZZZZ.
           03  WRK-BUNGSU-X.
               05  WRK-BUNGSU-Z    PIC ZZZZZZZZ.
           03  WRK-GOKEI-X2.
               05  WRK-GOKEI-Z2    PIC ZZZ,ZZZ,ZZZ.
           03  WRK-GOKEI-X3.
               05  WRK-GOKEI-Z3    PIC ---,---,---.
           03  WRK-NOZ             PIC ZZ9.
      *
      *    剤回数等画面編集用
           03  WRK-KAISU-H         PIC X(04).
      *
           03  WRK-TENSUKEI        PIC 9(08).
           03  WRK-KAISU           PIC 9(04).
           03  WRK-KAISU-GOK       PIC 9(04).
           03  WRK-GMN-IDX         PIC 9(04).
      *    診療会計用集計
           03  WRK-WKSRY-AREA.
               05  WRK-SYUTEN1             PIC  9(07).
               05  WRK-SYUTEN2             PIC  9(07).
               05  WRK-YKZTEN              PIC  9(07).
               05  WRK-KIZAITEN            PIC  9(07).
      *
           03  WRK-DOSELNUM            PIC 9(03).
           03  WRK-DOSELNUM2           PIC 9(03).
      *
           03  WRK-KINGAK          PIC 9(08).
           03  WRK-KAISUKEI        PIC 9(08).
           03  WRK-INPUTCD         PIC X(10).
           03  WRK-SURYO           PIC 9(07)V9(03).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(07).
               05  WRK-SURYO-S2    PIC 9(03).
           03  WRK-BUNGSU          PIC 9(08).
           03  WRK-SRYSYUKBN       PIC X(03).
           03  WRK-SRYSYUKBNMEI    PIC X(40).
           03  WRK-SRYKBN          PIC X(02).
      *
           03  WRK-GOKEI-9         PIC 9(10).
      *
           03  WRK-MEINUM          PIC 9(04).
           03  WRK-RENNUM          PIC 9(04).
           03  WRK-ZAINUM          PIC 9(08).
           03  WRK-PARAEDABAN      PIC 9(04).
      *     一部負担金計算用
           03  WRK-S01-TENSU       PIC 9(08).
           03  WRK-ACCT-KAISU      PIC 9(03).
      *
      *
           03  WRK-MOJI            PIC X(01).
      *
           03  WRK-SRYCD           PIC X(09).
           03  IDX-S               PIC 9(04).
      *    時間外フラグ
           03  WRK-TIMEFLG         PIC X(01).
      *    入力セット処理
           03  WRK-SETCD           PIC X(06).
      *    算定回数
           03  WRK-SANTEI-CNT          PIC 9(04).
      *    入力位置
           03  WRK-FLG-IN              PIC X(01).
      *
      *    診療行為チェック用編集
           03  WRK-TDD                 PIC 9(02).
           03  FLG-ACCTSYORI           PIC 9(01).
      *
           03  FLG-TUIKA               PIC 9(01).
      *
           03  WRK-KIDMSG          PIC X(80).
           03  WRK-KID-SRYCD       PIC X(09).
      *
           03  WRK-MOTOPG          PIC X(08).
      *
      *    労災保険用
           03  WRK-TENSU           PIC 9(08).
           03  WRK-EN              PIC 9(08).
      *    訂正時の点数判定
           03  WRK-MAE-TENSU           PIC 9(08).
      *
      *    自費用
           03  WRK-TENTTLKBN       PIC 9(03).
           03  WRK-HOZ-KEI         PIC 9(08).
      *
           03  WRK-KEI-ERRCD           PIC X(04).
           03  WRK-KEI-ERRMSG2         PIC X(80).
           03  WRK-ERR-ERRCD           PIC X(04).
           03  WRK-ERR-ERRMSG2         PIC X(80).
      *
           03  WRK-KEI-IDX             PIC 9(04).
      **** 03  WRK-RIGAKU-TBL.
      *        05  WRK-RIGAKU-OCCS       OCCURS  6.
      *            07  WRK-RIGAKU-SRYCD       PIC  X(09).
      *            07  WRK-RIGAKU-CNT         PIC  9(03).
      *            入院用追加
      *            07  WRK-ORCSC-RIG-KBN       PIC  X(01).
      *            07  WRK-ORCSC-RIG-RYO       PIC  X(01).
      *            07  WRK-ORCSC-RIG-DAY-G.
      *                09  WRK-ORCSC-RIG-DAY   PIC  9(01)
      *                                        OCCURS 31.
      *        05  WRK-ORCSC-SOUKI-G.
      *            07  WRK-ORCSC-SOUKI-FLG PIC  9(01)
      ***********                              OCCURS 31.
      *    リハビリ回数集計
           03  WRK-RIGAKU-TBL.
               05  WRK-GAI-FLG             PIC 9(01)   OCCURS  31.
      *
      *    約束セット用
           03  WRK-IDX-YAKUSOKU.
               05  WRK-IDX-YAK         PIC 9(04)    OCCURS  20.
      *
      *    老人慢性疾病診察料回数
           03  WRK-ROUMANSEI       PIC 9(04).
           03  WRK-ROUMANSEI2      PIC 9(04).
           03  WRK-GAIRAI-IDX      PIC 9(04).
      *    老人年齢計算領域
           03  WRK-NENREI.
               05  WRK-NENREI-YY         PIC 9(03).
               05  WRK-NENREI-MM         PIC 9(02).
               05  WRK-NENREI-DD         PIC 9(02).
      *    老人フラグ
           03  WRK-ROUJIN              PIC 9(01).
      *
      *    入力時表示用
           03  IDX-INCNT               PIC 9(04).
           03  FLG-NYURYOKU            PIC 9(01).
           03  WRK-HEN-INPUTCD         PIC X(22).
           03  FLG-IN-ATAI             PIC 9(01).
      *
      *    院内、院外判定用
           03  WRK-CHK-SRYSYUKBN   PIC X(03).
      *    算定年月
           03  WRK-SANTEI-YM       PIC X(06).
           03  WRK-SANTEI-ARI      PIC X(06).
           03  WRK-SANTEI-NASI     PIC X(06).
      *
      *    入力位置判定用
           03  WRK-IN-SURYO        PIC 9(02).
           03  WRK-IN-IDX          PIC 9(04).
           03  WRK-IN-IDX2         PIC 9(04).
           03  WRK-MAX-IDX         PIC 9(04).
      *
           03  WRK-WKSRY-HKNCOMBI      PIC 9(04).
      *
      *    院外、院内変更
           03  WRK-IDX-IN          PIC 9(04).
      *
      *    来院回数
           03  WRK-SINSATU-CNT     PIC 9(04).
      *    ワーク処理フラグ
           03  WRK-SYORIFLG        PIC 9(01).
      *    ワークカーソル位置（Ｂ０６Ｓ）
           03  WRK-SPA-MAP-IDX     PIC 9(04).
      *
           03  WRK-ERRCD           PIC X(04).
           03  WRK-ERRCD1          PIC X(04).
           03  WRK-ERRCD2          PIC X(04).
           03  WRK-KIDCD           PIC X(04).
      *    パス名
           03  WRK-PATH            PIC X(64).
      *
           03  WRK-SAKIPG          PIC X(08).
      *    画面制御
           03  WRK-PUTTYPE         PIC X(08).
      *    ＤＯ選択編集
           03  WRK-ZZ              PIC ZZZ.
      *    保険名称編集
           03  WRK-HKNCOMBI        PIC X(50).
      *    保険名称編集
           03  WRK-MAE-GMN-MAX     PIC 9(04).
      *    保険組合せ保存
           03  WRK-MAE-HKNCOMBI    PIC X(04).
      *
      *    特定疾患自動算定チェック用
           03  FLG-KYUJITU         PIC 9(01).
           03  WRK-KEIYMD          PIC X(08).
      *
      *    入力名称
           03  WRK-MEISYO-G.
               05  WRK-MEIH                PIC X(02).
               05  WRK-MEISYO              PIC X(78).
           03  WRK-MEISYO-GHZN.
               05  WRK-MEIHHZN             PIC X(02).
               05  WRK-MEISYOHZN           PIC X(78).
           03  IDX-MEI                 PIC 9(04).
      *    禁忌期間
           03  WRK-KIKAN               PIC 9(02).
      *    入力値の入力判定用
           03  WRK-HZN-ATAI-AREA.
               05  WRK-HZN-ATAI1           PIC X(10).
               05  WRK-HZN-ATAI2           PIC X(10).
               05  WRK-HZN-ATAI3           PIC X(10).
               05  WRK-HZN-ATAI4           PIC X(10).
      *    老人年齢
           03  WRK-ROU-NEN             PIC 9(02).
      *    算定回数
           03  WRK-CNT                 PIC 9(03).
           03  FLG-AUTO-OK             PIC 9(01).
      *    入院日
           03  WRK-STDD                PIC 9(02).
           03  WRK-EDDD                PIC 9(02).
           03  WRK-SRYDD               PIC 9(02).
      *   入院回数・日数
           03  WRK-NYUIN-AREA.
               05  WRK-NYU-KAI        PIC 9(03).
               05  WRK-NYUINDAY       PIC 9(01)    OCCURS  31.
           03  WRK-NYUIN-CHK-AREA.
               05  WRK-NYUIN-KAI       PIC 9(03).
           03  IDX-N2                  PIC 9(02).
           03  WRK-NYU-KAISU           PIC 9(03).
      *    消炎鎮痛回数
           03  WRK-SYOEN-CNT           PIC 9(03).
      *    入退院日
           03  WRK-NYUINYMD            PIC X(08).
           03  WRK-TAIINYMD            PIC X(08).
      *    診療選択より
           03  WRK-DRCD                PIC X(05).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-HKNCOMBINUM         PIC X(04).
      *
      *    コラムリスト位置
           03  WRK-K02-ROW             PIC S9(09).
           03  FLG-GMN-INIT            PIC  9(01).
      *
      *    処方せんＰＧ名
           03  WRK-SYOHOU-PG           PIC X(20).
           03  WRK-CHUSYA-PG           PIC X(20).
           03  WRK-SIJISEN-PG          PIC X(20).
      *
      *   年齢表示
       01  WRK-NENREI-G.
           03  WRK-NENREI-Z        PIC ZZ9.
           03  WRK-NENREI-MEI      PIC X(04).
       01  WRK-NENREI-GR           REDEFINES   WRK-NENREI-G.
           03  WRK-NENREI-ZN       PIC Z9.
           03  WRK-NENREI-MEIN     PIC X(08).
      *
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
      *    画面数量編集用
       01  WRK-GMN-SURYO-AREA.
           03  WRK-SURYO-HENSYU.
               05  WRK-H-SURYO        PIC X(09).
               05  WRK-H-TANINAME     PIC X(04).
               05  WRK-H-F1           PIC X(01).
               05  WRK-H-ZAIKEI       PIC X(15).
               05  WRK-H-KEI          PIC X(08).
      *
           03  WRK-KEI-X.
               05  WRK-KEI-Z       PIC Z(08).
           03  WRK-KEI-H           PIC X(08).
           03  WRK-ZAIKEI-H        PIC X(40).
           03  WRK-KAKERU          PIC X(03).
      *
        01  WRK-HENSYU-AREA.
      *    ページ表示
           03  WRK-HEN-PAGE.
               05  WRK-HEN1               PIC X(02).
               05  WRK-HEN-PAGE1          PIC ZZZ.
               05  WRK-HEN2               PIC X(01).
               05  WRK-HEN-PAGE2          PIC ZZ.
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
       01  WRK-MOJI-AREA.
           03  FLG-SP              PIC 9(02).
           03  FLG-AST             PIC 9(02).
           03  WRK-IDX-FT          PIC 9(02).
           03  WRK-CD-MCNT         PIC 9(02).
           03  WRK-ACCT-MCNT       PIC 9(02).
           03  WRK-SUR-MCNT        PIC 9(02).
           03  WRK-MCNT1           PIC 9(02).
           03  WRK-MCNT2           PIC 9(02).
           03  WRK-MCNT3           PIC 9(02).
           03  WRK-MCNT4           PIC 9(02).
           03  WRK-CD              PIC X(22).
           03  WRK-KAI             PIC X(20).
           03  WRK-SUR             PIC X(20).
           03  WRK-MOJI1           PIC X(10).
           03  WRK-MOJI2           PIC X(10).
           03  WRK-MOJI3           PIC X(10).
           03  WRK-MOJI4           PIC X(10).
           03  IDM                 PIC 9(04).
           03  IDMS                PIC 9(04).
           03  WRK-HENKAN-MAE      PIC X(20).
           03  WRK-HENKAN-ATO      PIC X(20).
           03  IDH1                PIC 9(04).
           03  IDH2                PIC 9(04).
           03  IDX-D               PIC 9(04).
      *
           03  WRK-SANTEI-SYORI    PIC X(01).
      *
      *    継続の時、自動発生のチェック
       01  WRK-CONTCHK-TBL.
           03  WRK-IDX-Y               PIC 9(04).
      *    03  WRK-CONTCHK-OCC         OCCURS  30.
      *        05  WRK-TBL-SRYCD       PIC X(09).
      *    03  IDX-TBL                 PIC 9(04).
      *    03  FLG-CNTCHK              PIC 9(01).
      *
      *    自動加算テーブル
       01  WRK-KASAN-AREA.
           03  WRK-KASAN-TBL                       OCCURS   60.
               05  WRK-KSN-SRYSYUKBN       PIC X(03).
               05  WRK-KSN-SRYKBN          PIC X(02).
               05  WRK-KSN-SRYCD           PIC X(09).
               05  WRK-KSN-ATAI-G.
                   07  WRK-KSN-ATAI        PIC X(08)   OCCURS  4.
               05  WRK-KSN-TENSU           PIC 9(08).
               05  WRK-KSN-ZAINUM          PIC 9(08).
               05  WRK-KSN-ZAIEND          PIC X(01).
               05  WRK-KSN-INPUTCD         PIC X(22).
               05  WRK-KSN-AUTOKBN         PIC X(01).
           03  WRK-KSN-IDX             PIC 9(04).
      *
      *    禁忌チェック用テーブル
       01  WRK-KNKCHK-AREA.
           03  WRK-KNK-TBL             OCCURS   30.
               05  WRK-KNK-TOUMEI          PIC X(100). 
               05  WRK-KNK-TOUSRYCD        PIC X(09). 
               05  WRK-KNK-TBL2A.
                   07  WRK-KNK-TBL2            OCCURS   30.
                       09  WRK-KNK-KNKMEI          PIC X(100).
                       09  WRK-KNK-TOUYMD          PIC X(08).
                       09  WRK-KNK-TOUYMDW         PIC X(09).
                       09  WRK-KNK-SRYCD           PIC X(09).
                       09  WRK-KNK-SYOJYOUCD       PIC X(07).
           03  WRK-KNK-MAX             PIC 9(04).
      *
       01  WRK-SORT-AREA.
           03  WRK-SRT-TBL2A.
               05  WRK-SRT-TBL2            OCCURS   30.
                   09  WRK-SRT-KNKMEI          PIC X(100).
                   09  WRK-SRT-TOUYMD          PIC X(08).
                   09  WRK-SRT-TOUYMDW         PIC X(09).
                   09  WRK-SRT-SRYCD           PIC X(09).
                   09  WRK-SRT-SYOJYOUCD       PIC X(07).
      *
      *    画面ＳＰＡ領域ワーク
       01  WRK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *    画面ＳＰＡ領域ワーク
       01  HZN-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HZN-//.
      *
      *    保存ＳＰＡ領域ワーク
       01  WRK-HZN-SCR-AREA.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-HZN-//.
      *
       01 WRK-HOZ-IDX                  PIC 9(04).
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *    スパ領域 ワーク
           COPY    "COMMON-SPA"      REPLACING
                                     //SPA-// BY //WRK-SPA-//.
      *
      *
       01  WRK-SYSTIME               PIC 9(08).
      *
      *画面最大行
       01  WRK-GYO-MAX                 PIC 9(04)   VALUE   200.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
      *D   COPY    "CPSYSKANRI.INC".
      *
      *    COPY    "CPSK1005.INC".
      *
      *    COPY    "CPSK1006.INC".
      *
      *    COPY    "CPSK1010.INC".
      *    帳票ＰＧ
           COPY    "CPSK1032.INC".
      *    帳票ＰＧ
           COPY    "CPSK1034.INC".
      *
      *    入院指示せん設定情報
           COPY  "CPSK5007.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    チェックマスタ−
       01  CHK-REC.
           COPY    "CPCHK.INC".
      *
      *    入力セットコード
       01  INPUTSET-REC.
           COPY    "CPINPUTSET.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *    患者入院履歴ワーク
       01  HZN-PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING
                               //PTNYUINRRK-// BY //HZN-PTNYUINRRK-//.
      *
       01  LNK-WKSRYACT-AREA.
           02  LNK-WKSRYACT-REC          OCCURS   200.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //LNK-WKSRY-//.
       01  LNK-WKSRYACT-MAX           PIC 9(04).
      *
      *    収納マスタワーク
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納マスタワーク
      *01  HZN-SYUNOU-REC.
      *    COPY    "CPSYUNOU.INC"  REPLACING
      *                              //SYU-// BY //HZN-SYU-//.
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    相互作用マスタ
       01  INTERACT-REC.
           COPY    "CPINTERACT.INC".
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
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    負担金額計算用パラメタ
           COPY    "CPORCS01.INC".
      *
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC00.INC".
      *
      *    算定履歴チェックパラメタ
           COPY    "CPORCSC91.INC".
      *
      *    点数マスタ編集パラメタ
           COPY    "CPORCSC92.INC".
      *
      *    剤分離パラメタ
           COPY    "CPORCSC93.INC".
      *
      *    画面再編集パラメタ
           COPY    "CPORCSC94.INC".
      *
      *    初期ＳＰＡ画面編集パラメタ
           COPY    "CPORCSC95.INC".
      *
      *    エラーメッセージ編集パラメタ
           COPY    "CPORCSC96.INC".
      *
      *    診療行為入力項目変換パラメタ
           COPY    "CPORCSC97.INC".
      *
      *    診察料自動発生パラメタ
           COPY    "CPORCSC10S.INC".
      *
      *    リハビリ当月算定カウントパラメタ
           COPY    "CPORCSC80CNT.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    保険年齢・負担割合算定サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    セット展開サブ
           COPY    "CPORCSCSETHEN.INC".
      *
      *    ワーク診療行為展開・作成
           COPY    "CPORCSCWKSRY.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    入院指示せんオーダ作成サブ
           COPY    "CPORCSORDERPRT.INC".
      *    入院処方箋発行サブ
           COPY    "CPORCHC501.INC".
      *    注射処方箋発行サブ
           COPY    "CPORCHC502.INC".
      *    入院指示箋発行サブ
           COPY    "CPORCHC503.INC".
      *    内分泌負荷試験更新パラメタ
           COPY    "CPORCSSANFUKA.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
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
           COPY    "K01.INC".
           COPY    "K02.INC".
           COPY    "K02N.INC".
           COPY    "K021.INC".
           COPY    "K022.INC".
           COPY    "K023.INC".
           COPY    "K03.INC".
           COPY    "K04.INC".
           COPY    "K05.INC".
           COPY    "K051.INC".
           COPY    "K052.INC".
           COPY    "K06.INC".
           COPY    "K07.INC".
           COPY    "K08.INC".
           COPY    "K081.INC".
           COPY    "K09.INC".
           COPY    "K10.INC".
           COPY    "K97.INC".
           COPY    "K98.INC".
           COPY    "K99.INC".
           COPY    "K03X.INC".
           COPY    "KERR.INC".
           COPY    "KERR2.INC".
           COPY    "KID1.INC".
           COPY    "KID2.INC".
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
           INITIALIZE                  IDX-AREA
           MOVE    ZERO                TO  LNK-WKSRYACT-MAX
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           MOVE    "K02N"              TO  SPA-HZN-MOTOPG
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPACE               TO  SPA-ERRMSG2
           MOVE    ZERO                TO  FLG-END
      *
      *    診療種別テーブルセット
           PERFORM 1001-SRYKBN-SET-SEC
      *
      *    画面ＳＰＡ読み込み
           PERFORM 1002-K02SPA-SET-SEC
      *    画面制御
           MOVE    SPACE           TO  WRK-PUTTYPE
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
      *    画面ＳＰＡ書込み
           IF      FLG-END2            =   ZERO
               PERFORM 1003-K02SPA-OUT-SEC
           END-IF
      *
           MOVE    "K02N"          TO  SPA-HZN-MOTOPG
           MOVE    SPA-K01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-K02         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "KERR"
                                       OR  "KERR2"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
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
               IF      FLG-END             =   1
                   GO      TO      100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE   SPACE                TO  MCP-PUTTYPE
           MOVE   "K02N"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療種別テーブルセット処理
      *****************************************************************
       1001-SRYKBN-SET-SEC             SECTION.
      * 
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
      *
           .
       1001-SRYKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡデータＲＥＡＤ処理
      *****************************************************************
       1002-K02SPA-SET-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-SPASCR
           MOVE    SPA-TERMID          TO  FILE-TERMID1
      *
           OPEN    INPUT               SPASCR-FILE
           IF      STS-SPASCR     NOT =   ZERO
               MOVE    2                  TO  FLG-SPASCR
           END-IF
      *
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE          SPA-SCR-REC
           MOVE    SPACE               TO  WRK-DATA-REC
           INITIALIZE          WRK-DATA-REC
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL       FLG-SPASCR  NOT =   ZERO
               READ    SPASCR-FILE
                   AT  END
                       MOVE   1            TO  FLG-SPASCR
                   NOT AT  END
                       ADD    1            TO  IDX
                       MOVE   SPA-DATA-REC     TO  WRK-DATA (IDX)
               END-READ
           END-PERFORM
      *
           MOVE    WRK-DATA-REC        TO  SPA-SCR-REC
      *
           IF      FLG-SPASCR          =   2
               CONTINUE
           ELSE
               CLOSE                       SPASCR-FILE
           END-IF
      *
           .
       1002-K02SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡデータＷＲＩＴＥ処理
      *****************************************************************
       1003-K02SPA-OUT-SEC             SECTION.
      *
           MOVE    SPA-TERMID          TO  FILE-TERMID1
      *
           OPEN    OUTPUT              SPASCR-FILE
           IF      STS-SPASCR     NOT =   ZERO
               DISPLAY "K02SPASCR OUT OPEN ERR:"   STS-SPASCR "."
           END-IF
      *
           MOVE    SPA-SCR-REC         TO  WRK-DATA-REC
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL      (IDX     >   10  )  OR
                              (WRK-DATA (IDX)     =   SPACE)
      *
               MOVE    WRK-DATA (IDX)      TO  SPA-DATA-REC
               WRITE                       SPA-DATA-REC
               IF      STS-SPASCR     NOT =   ZERO
                   DISPLAY "K02SPASCR OUT WRITE ERR:"   STS-SPASCR "."
                END-IF
           END-PERFORM
      *
           CLOSE                       SPASCR-FILE
           .
       1003-K02SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC             SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  FLG-GMN
           MOVE    SPACE               TO  WRK-KID-SRYCD
      *
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
      *
      *        画面ＳＰＡ読み込み
               PERFORM 1002-K02SPA-SET-SEC
      *
      *        元画面へ戻す
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
               IF      WRK-MOTOPG(1:3)     =   "Y01"  OR  "U01"
                                                      OR  "P97"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-K02
                   MOVE    SPA-WORK        TO  SPA-K01KYOUTU
                   MOVE    1               TO  SPA-GMN-CUR
               END-IF
      *        予約の時、そのまま画面表示へ
               IF      WRK-MOTOPG(1:3)     =   "Y01"
                   IF      SPA-PTID            =   ZERO
                       INITIALIZE              SPA-K01SET-AREA
                       MOVE    SPA-SYOHOKBN    TO  SPA-INGAIKBN
                       MOVE    ZERO            TO  SPA-GMN-CUR
                   END-IF
                   GO      TO      300-SCREEN-EXT
               END-IF
      *        外部より選択がある時
               IF      WRK-MOTOPG(1:3)     =   "U01"  OR
                                               "P97"
                   PERFORM 3101-P97SPASET-SEC
                   GO      TO      300-SCREEN-EXT
               END-IF
           END-IF
      *
           MOVE    "1"                 TO  SPA-NYUGAIKBN
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "K99"
                   IF      SPA-K99-DATA    NOT =   SPACE
                        MOVE    SPA-COM-GYOU(SPA-GMN-IDX)
                                                   TO  WRK-IN-MAP-IDX
                       MOVE    SPA-GMN-IDX         TO  WRK-STR
                       MOVE    SPA-K99-DATA        TO  SPA-GMN-INPUTCD
                                                          (SPA-GMN-IDX)
                       MOVE    "1"                 TO  SPA-COM-IN
                                                         (SPA-GMN-IDX)
      *
      *                入力コード・名称チェック処理
                       PERFORM 410112-KOUI-SPACHK-SEC
                       MOVE    1                   TO  FLG-GMN
                   ELSE
      *                画面ＳＰＡチェック処理
                       PERFORM 410112-KOUI-SPACHK-SEC
                       MOVE    2                   TO  FLG-GMN
                   END-IF
               WHEN    "K98"
                   IF      SPA-K98-SRYCD-G   NOT =   SPACE
                       PERFORM 3011-K98-SET-SEC
                       MOVE    1                   TO  FLG-GMN
                   ELSE
                       MOVE    "1"                 TO  SPA-COM-CUR
                                                         (SPA-GMN-IDX)
                       MOVE    2                   TO  FLG-GMN
                   END-IF
               WHEN    "K04"
      *            算定処理より
                   MOVE    2                   TO  FLG-GMN
               WHEN    "K021"
      *             禁忌一覧 ＯＫの時、更新処理へ
                   IF     (SPA-K021-SYORI      =   2  )  AND
                          (SPA-K021-CHK        =   1  )
      *                画面制御
                       MOVE    "JOIN"          TO  WRK-PUTTYPE
                       EVALUATE    SPA-K021-SAKI
                           WHEN    "K03"
      *                        更新処理
                               PERFORM 41002-KOUSIN-SYORI-SEC
                           WHEN    "C02"
      *                        病名登録遷移処理
                               PERFORM 4401-C02SET-SEC
                       END-EVALUATE
                   END-IF
                   MOVE    2                   TO  FLG-GMN
               WHEN    "K022"
      *            禁忌画面より
                   MOVE    2                   TO  FLG-GMN
               WHEN    "K023"
      *            入力コード設定
                   PERFORM 30014-K023-SET-SEC
                   MOVE    2                   TO  FLG-GMN
               WHEN    "KID1"
      *            確認画面より
                   PERFORM 30010-KID1-SET-SEC
                   MOVE    2                   TO  FLG-GMN
               WHEN    "KID2"
      *            確認画面２より
                   PERFORM 30011-KID2-SET-SEC
                   MOVE    2                   TO  FLG-GMN
               WHEN    "K09"
      *            ＤＯ選択画面より
                   PERFORM 3013-K09-SET-SEC
                   MOVE    2                   TO  FLG-GMN
      *************GO      TO      300-SCREEN-EXT
               WHEN    "KHELP"
      *            ヘルプ画面より
                   MOVE    2                   TO  FLG-GMN
               WHEN    "K081"
      *            指示せん発行画面
                   PERFORM 3014-K081-SET-SEC
                   MOVE    2                   TO  FLG-GMN
           END-EVALUATE
      *
           IF      FLG-GMN         NOT =   ZERO
               IF      SPA-ERRCD       NOT =   SPACE
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
      *            エラー編集
      *            数量ゼロの場合、エラー表示しない
                   IF      SPA-ERRCD           =   "A001"
                       MOVE    SPACE               TO  SPA-ERRCD
                   ELSE
                       PERFORM 510-ERRSET-SEC
                   END-IF
                   GO      TO      300-SCREEN-EXT
               END-IF
               IF      SPA-KIDCD       NOT =   SPACE
                   PERFORM 520-KIDSET-SEC
                   GO      TO      300-SCREEN-EXT
               END-IF
               GO      TO      300-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-KIDCD
           MOVE    SPACE               TO  SPA-KIDCD2
      *
      *    患者の確定していない時、クリア画面の表示、M01より
           IF    ( SPA-MOTOPG          =   "M01"    )  OR
                 ((SPA-MOTOPG          =   "K08"  OR  "K02") AND
                  (SPA-PTNUM           =   SPACE    ))
               PERFORM 30010-INIT-GMN-SEC
               GO      TO      300-SCREEN-EXT
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "K01"
      *            診療選択より
                   IF      SPA-ERRMSG(1:2)    =   "NO"
      *                戻る
                       MOVE    ZERO               TO  FLG-PARASET
                       PERFORM 3002-PARA-SET-SEC
                       PERFORM 3003-GMN-HEN-SEC
                       IF      SPA-GMN-PTID   NOT =   ZERO
      *                    排他制御処理
                           PERFORM 990-LOCK-IN-SEC
                       END-IF
                   ELSE
                       MOVE    ZERO               TO  SPA-SAISIN-CHK
                       MOVE    2                  TO  FLG-PARASET
                       PERFORM 3002-PARA-SET-SEC
                       IF      SPA-SYORIFLG        =   ZERO
                          AND  SPA-WKSRY-HKNCOMBI  =   ZERO
                          PERFORM 3009-SAIHEN-SEC
                       END-IF
                   END-IF
               WHEN    "K02"
      *            外来診療行為より
                   IF      SPA-PTNUM           =   SPACE
                       PERFORM 30010-INIT-GMN-SEC
                   ELSE
                       MOVE    SPA-PTNUM           TO  K02N-PTNUM
                       PERFORM 41010-PTNUM-SYORI-SEC
                   END-IF
               WHEN    "P02"
      *            患者登録より再編集
                   INITIALIZE                  SPA-K02
                   MOVE    ZERO            TO  FLG-PARASET
                   PERFORM 3002-PARA-SET-SEC
                   IF      SPA-ERRMSG(1:2)    =   "NO"
      *                戻る
                       PERFORM 3003-GMN-HEN-SEC
                   ELSE
                       IF      SPA-PTNUM       =   SPACE
                           PERFORM 30010-INIT-GMN-SEC
                       ELSE
                           IF      SPA-SYORIFLG        =   ZERO
                               PERFORM 30011-INIT-HEN-SEC
                           ELSE
                               PERFORM 3003-GMN-HEN-SEC
                           END-IF
                       END-IF
                   END-IF
               WHEN    "U02"
      *            受付より再編集
                   INITIALIZE                  SPA-K02
                   MOVE    ZERO            TO  FLG-PARASET
                   PERFORM 3002-PARA-SET-SEC
                   IF     (SPA-PTNUM       NOT =   SPACE    ) AND
                          (SPA-GMN-PTNUM   NOT =   SPA-PTNUM)
                       PERFORM 3009-SAIHEN-SEC
                   ELSE
                       IF     (SPA-PTNUM       =   SPACE )  AND
                              (SPA-GMN-PTNUM   =   SPACE )
                           PERFORM 30010-INIT-GMN-SEC
                       ELSE
                           PERFORM 3003-GMN-HEN-SEC
                       END-IF
                   END-IF
      *
               WHEN    "K08"
               WHEN    "K03"
                   INITIALIZE                  SPA-K02
                   MOVE    ZERO            TO  FLG-PARASET
                   PERFORM 3002-PARA-SET-SEC
      *            特定疾患判定のクリア
                   MOVE    ZERO            TO  SPA-TOKUTEIFLG
                   MOVE    ZERO            TO  SPA-CHOZAI-CHK
      *            診療行為、計計算処理、編集
      *            MOVE    "2"             TO  FLG-TOROKU
                   MOVE    SPACE           TO  FLG-TOROKU
                   PERFORM 510-TBLHEN-SEC
                   MOVE    SPACE           TO  FLG-TOROKU
      *
               WHEN    "K10"
                   IF      SPA-K10-SELNUM      =   ZERO
                       GO      TO      300-SCREEN-EXT
                   ELSE
                       PERFORM 30091-SAIHEN2-SEC
                   END-IF
      *
               WHEN    OTHER
                   INITIALIZE                  SPA-K02
                   MOVE    ZERO            TO  FLG-PARASET
                   PERFORM 3002-PARA-SET-SEC
                   IF      SPA-MOTOPG      =   "K03"
      *                特定疾患判定のクリア
                       MOVE    ZERO            TO  SPA-TOKUTEIFLG
                       MOVE    ZERO            TO  SPA-CHOZAI-CHK
                   END-IF
                   PERFORM 3003-GMN-HEN-SEC
           END-EVALUATE
      *
           MOVE    1               TO  SPA-GMN-PAGE
           MOVE    1               TO  SPA-GMN-CUR
           MOVE    1               TO  SPA-MAP-IDX
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｋ０２Ｎ初期画面表示
      *****************************************************************
       30010-INIT-GMN-SEC              SECTION.
      *
           INITIALIZE              SPA-KYOTUKEY
           INITIALIZE              SPA-K02
           INITIALIZE              SPA-SCR-REC
      *
           MOVE    "1"                 TO  SPA-NYUGAIKBN
      *
      *    初期ＳＰＡ編集処理
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-SYSYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SYSYMDWH        TO  LNK-SC95-SRYYMDG
           MOVE    "9"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
           MOVE    1               TO  SPA-GMN-PAGE
           MOVE    0               TO  SPA-GMN-CUR
           MOVE    1               TO  SPA-MAP-IDX
      *
           .
       30010-INIT-GMNN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為再編集処理
      *****************************************************************
       30011-INIT-HEN-SEC              SECTION.
      *
           MOVE    SPA-SCR-REC         TO  WRK-SCR-REC
           MOVE    ZERO                TO  SPA-NAI-ROUSTR
      *
           MOVE    SPA-GMN-HKNCOMBINUM TO  WRK-MAE-HKNCOMBI
           MOVE    SPA-GMN-HKNCOMBINUM TO  SPA-HKNCOMBINUM
           MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
           MOVE    SPA-DRCD            TO  WRK-DRCD
      *
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
      *
           MOVE    SPA-GMN-SRYYMD      TO  SPA-SRYYMDW
           MOVE    SPA-NAI-SRYYMD      TO  SPA-SRYYMD
      *
           INITIALIZE                  ORCSC95AREA
           MOVE    "5"                 TO  LNK-SC95-KBN
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
           MOVE    "K10"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
      *    排他制御処理
           PERFORM 990-LOCK-IN2-SEC
           IF      SPA-ERRCD           =   "9999"
               GO      TO      30011-INIT-HEN-EXT
           END-IF
      *
           MOVE    WRK-DRCD            TO  SPA-DRCD
           MOVE    WRK-MAE-HKNCOMBI    TO  SPA-GMN-HKNCOMBINUM
           MOVE    WRK-MAE-HKNCOMBI    TO  SPA-HKNCOMBINUM
      *
      *    チェック用月内診療行為セット
           PERFORM                 320-SPA-CHKSET-SEC
      *
      *    スパ共通編集処理
           PERFORM                 310-SPA-SET-SEC
           MOVE    WRK-SCR-REC         TO  SPA-SCR-REC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      30011-INIT-HEN-EXT
           END-IF
      *
      *    診療行為、計計算処理、編集
           MOVE    SPACE               TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE               TO  FLG-TOROKU
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           MOVE    SPACE               TO  SPA-KID1-FLG
      *
           MOVE    ZERO                TO  SPA-NAI-ROUSTR
      *
      *
           .
       30011-INIT-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ＳＰＡのＫ０２を共通ＳＰＡへ
      *****************************************************************
       3003-GMN-HEN-SEC              SECTION.
      *
           IF      SPA-GMN-PTID        =   ZERO
               MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
               MOVE    SPA-SYSYMDWH        TO  SPA-SRYYMDW
               GO      TO      3003-GMN-HEN-EXT
           END-IF
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           MOVE    SPA-GMN-NAME        TO  SPA-NAME
           MOVE    SPA-GMN-KANANAME    TO  SPA-KANANAME
           MOVE    SPA-GMN-SEX         TO  SPA-SEX
           MOVE    SPA-GMN-BIRTHDAY    TO  SPA-BIRTHDAYW
           MOVE    SPA-NAI-BIRTHDAY    TO  SPA-BIRTHDAY
      *
           MOVE    SPA-GMN-SRYYMD      TO  SPA-SRYYMDW
           MOVE    SPA-NAI-SRYYMD      TO  SPA-SRYYMD
      *
           MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
      *
      *    排他制御処理
           PERFORM 990-LOCK-IN-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      3003-GMN-HEN-EXT
           END-IF
      *
      *    保険組合
           PERFORM 3101-HKNCOMBI-HEN-SEC
           .
       3003-GMN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為再編集処理
      *****************************************************************
       3009-SAIHEN-SEC              SECTION.
      *
      *    患者番号変更、Ｋ０１、Ｐ０２よりの時　初画面を編集
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO            TO  SPA-NAI-ROUSTR
      *
      *    初期ＳＰＡ編集処理
           IF     (SPA-MOTOPG          =   "P02"  OR  "U02" )  AND
                  (SPA-PTID        NOT =   SPA-GMN-PTID )
               IF      SPA-MOTOPG          =   "P02"  OR  "U02"
                   MOVE    SPA-GMN-SRYYMD      TO  SPA-SRYYMDW
                   MOVE    SPA-NAI-SRYYMD      TO  SPA-SRYYMD
                   IF      SPA-SRYYMD          =   SPACE
                       MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
                       MOVE    SPA-SYSYMDWH        TO  SPA-SRYYMDW
                   END-IF
               END-IF
      *        初期ＳＰＡ編集処理
               INITIALIZE                  ORCSC95AREA
               MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
               MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
               MOVE    "9"                 TO  LNK-SC95-KBN
               MOVE    "K02"               TO  LNK-SC95-PG
               CALL    "ORCSC95"           USING
                                           MCPAREA
                                           ORCSC95AREA
                                           SPA-AREA
                                           SPA-K01KYOUTU
                                           SPA-K02
           ELSE
               IF     (SPA-PTID            =   SPA-GMN-PTID )  AND
                      (SPA-NAI-SRYYMD  NOT =   SPA-SRYYMD   )
                   MOVE    SPA-GMN-SRYYMD      TO  SPA-SRYYMDW
                   MOVE    SPA-NAI-SRYYMD      TO  SPA-SRYYMD
               END-IF
           END-IF
           IF      SPA-PTID            =   ZERO
               GO      TO      3009-SAIHEN-EXT
           END-IF
      *
      *    排他制御処理
           PERFORM 990-LOCK-IN2-SEC
           IF      SPA-ERRCD           =   "9999"
               GO      TO      3009-SAIHEN-EXT
           END-IF
      *    診療選択からの時、診療選択分とする
           IF      SPA-MOTOPG          =   "K01"
               MOVE    SPA-DRCD            TO  WRK-DRCD
               MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBINUM
               MOVE    SPA-SRYKA           TO  WRK-SRYKA
           ELSE
               MOVE    SPACE               TO  WRK-DRCD
               MOVE    SPACE               TO  WRK-HKNCOMBINUM
               MOVE    SPACE               TO  WRK-SRYKA
           END-IF
      *
      *    入院が存在するかのチェック
           PERFORM 410101-NYUINRRK-CHK-SEC
      *    入外区分
           IF      SPA-NYUGAIKBN       =   "2"
      *        外来の時、外来処理へ
               MOVE    SPACE               TO  WRK-PUTTYPE
               PERFORM 410109-K02IDOU-SEC
               GO      TO      3009-SAIHEN-EXT
           END-IF
      *
      *    診療選択からの時、診療選択分とする
           IF      SPA-MOTOPG          =   "K01"
               MOVE    WRK-DRCD            TO  SPA-DRCD
               MOVE    WRK-HKNCOMBINUM     TO  SPA-HKNCOMBINUM
               MOVE    WRK-SRYKA           TO  SPA-SRYKA
               MOVE    WRK-SRYKA           TO  SPA-GMN-SRYKA
           END-IF
      *
      *
           INITIALIZE                  ORCSC95AREA
           MOVE    "5"                 TO  LNK-SC95-KBN
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
           IF     (SPA-MOTOPG          =   "K01"        )  AND
                  (SPA-PTID            =   SPA-GMN-PTID )
               MOVE    "K01"               TO  LNK-SC95-PG
           ELSE
               MOVE    "K02"               TO  LNK-SC95-PG
               IF     (SPA-MOTOPG          =   "K01"    )  AND
                      (WRK-SRYKA       NOT =   SPACE    )
                   CONTINUE
               ELSE
                   MOVE    SPACE               TO  SPA-SRYKA
               END-IF
           END-IF
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
           IF     SPA-HKNCOMBINUM      =   SPACE  OR  ZERO
      *        スパ共通編集処理
               PERFORM                 310-SPA-SET-SEC
               MOVE   "1006"           TO  SPA-ERRCD 
               MOVE    1               TO  SPA-GMN-PAGE
               MOVE    00              TO  SPA-GMN-CUR
               MOVE    3               TO  SPA-HKN-ERR
               GO      TO      3009-SAIHEN-EXT
           END-IF
      *    パラメタファイルより表示
           MOVE    3               TO  FLG-PARASET
           PERFORM                 3002-PARA-SET-SEC
      *
      *    チェック用月内診療行為セット
           PERFORM                 320-SPA-CHKSET-SEC
      *
      *    スパ共通編集処理
           PERFORM                 310-SPA-SET-SEC
      *
      *    画面用スパ編集処理
           PERFORM                 310-SPA-GMNSET-SEC
      *
           MOVE    ZERO            TO  SPA-NAI-ROUSTR
      *
           .
       3009-SAIHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為再編集処理（Ｋ１０より）
      *****************************************************************
       30091-SAIHEN2-SEC              SECTION.
      *
      *    中途終了分よりの時　初画面を編集
           INITIALIZE                  SPA-SCR-REC
      *
      *    初期ＳＰＡ編集処理
           IF     (SPA-SRYYMD      NOT =   SPA-GMN-SRYYMD )  OR
                  (SPA-PTID        NOT =   SPA-GMN-PTID   )
      *        選択された科・保険
               MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBINUM
               MOVE    SPA-SRYKA           TO  WRK-SRYKA
      *        入院が存在するかのチェック
               MOVE    SPA-SRYYMD          TO  SPA-NAI-SRYYMD
               PERFORM 410101-NYUINRRK-CHK-SEC
      *        入外区分
               MOVE    "1"                 TO  SPA-NYUGAIKBN
               MOVE    WRK-HKNCOMBINUM     TO  SPA-HKNCOMBINUM
               MOVE    WRK-SRYKA           TO  SPA-SRYKA
      *
               INITIALIZE                  ORCSC95AREA
               MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
               MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
               MOVE    "9"                 TO  LNK-SC95-KBN
               MOVE    "K10"               TO  LNK-SC95-PG
               CALL    "ORCSC95"           USING
                                           MCPAREA
                                           ORCSC95AREA
                                           SPA-AREA
                                           SPA-K01KYOUTU
                                           SPA-K02
      *        入退院日
               MOVE    WRK-NYUINYMD        TO  SPA-K02N-NYUINYMD
               MOVE    WRK-TAIINYMD        TO  SPA-K02N-TAIINYMD
      *
               INITIALIZE                  ORCSC95AREA
               MOVE    "5"                 TO  LNK-SC95-KBN
               MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
               MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
               MOVE    "K10"               TO  LNK-SC95-PG
               CALL    "ORCSC95"           USING
                                           MCPAREA
                                           ORCSC95AREA
                                           SPA-AREA
                                           SPA-K01KYOUTU
                                           SPA-K02
           ELSE
      *        中途終了分のみ
               INITIALIZE                  ORCSC95AREA
               MOVE    "3"                 TO  LNK-SC95-KBN
               MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
               MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
               MOVE    "K10"               TO  LNK-SC95-PG
               CALL    "ORCSC95"           USING
                                           MCPAREA
                                           ORCSC95AREA
                                           SPA-AREA
                                           SPA-K01KYOUTU
                                           SPA-K02
           END-IF
      *
      *!   IF     SPA-HKNCOMBINUM      =   SPACE  OR  ZERO
      *        スパ共通編集処理
      *        PERFORM                 310-SPA-SET-SEC
      *        MOVE   "1006"           TO  SPA-ERRCD 
      *        MOVE    1               TO  SPA-GMN-PAGE
      *        MOVE    00              TO  SPA-GMN-CUR
      *        MOVE    3               TO  SPA-HKN-ERR
      *        GO      TO      30091-SAIHEN2-EXT
      *!   END-IF
      *    パラメタファイルより表示
           MOVE    3               TO  FLG-PARASET
           PERFORM                 3002-PARA-SET-SEC
      *
      *    チェック用月内診療行為セット
           PERFORM                 320-SPA-CHKSET-SEC
      *
      *    スパ共通編集処理
           PERFORM                 310-SPA-SET-SEC
      *
      *    保険組合せの無い場合、エラーをクリアする
           IF      SPA-HKN-ERR         =   4
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
      *
      *    画面用スパ編集処理
           PERFORM                 310-SPA-GMNSET-SEC
      *
      *
           .
       30091-SAIHEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＬＤ外の項目入力編集処理
      *****************************************************************
       3101-P97SPASET-SEC              SECTION.
      *
           MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU
      *    受付、患者検索より
           IF      WRK-SPA-PTNUM       =   SPA-PTNUM
               CONTINUE
           ELSE
               IF      WRK-SPA-PTNUM       NOT =   SPACE
      *            初期表示へ
                   MOVE    WRK-SPA-PTNUM   TO  K02N-PTNUM
      *            受付一覧からの時、診療科を編集する
                   IF      WRK-MOTOPG          =   "U01"
                       MOVE    WRK-SPA-SRYKA   TO  SPA-SRYKA
                   END-IF
                   PERFORM 41010-PTNUM-SYORI-SEC
                   MOVE    SPACE           TO  SPA-ERRCD
                   MOVE    ZERO            TO  SPA-NAI-ROUSTR
               END-IF
           END-IF
      *
           IF     (SPA-PTID            =   ZERO  )  OR
                  (SPA-ERRCD       NOT =   SPACE )
               MOVE    ZERO                TO  SPA-GMN-CUR
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
           .
       3101-P97SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧画面（Ｋ９８）ＯＫ処理
      *****************************************************************
       3011-K98-SET-SEC          SECTION.
      *
      **** MOVE    SPA-COM-GYOU(SPA-GMN-IDX)
           MOVE    ZERO                TO  WRK-IN-MAP-IDX
           MOVE    SPA-GMN-IDX         TO  WRK-STR
      *
           PERFORM VARYING     IDX-K98 FROM    1   BY  1
                   UNTIL      (IDX-K98             >   5   )  OR
                              (SPA-K98-SRYCD(IDX-K98)  =   SPACE ) OR
                              (SPA-ERRCD           NOT =   SPACE )
      *
               IF      IDX-K98             =   1
                   INITIALIZE                  SPA-COM-TBLREC
                                                       (SPA-GMN-IDX)
                   INITIALIZE                  SPA-GMN-TBLREC
                                                       (SPA-GMN-IDX)
                   MOVE    SPA-K98-SRYCD(IDX-K98)
                                           TO  SPA-GMN-INPUTCD
                                                       (SPA-GMN-IDX)
               ELSE
                   ADD     1               TO  SPA-GMN-IDX
                   PERFORM 41014-GYOINSN-SEC
                   IF      SPA-ERRCD       =   SPACE
                       MOVE    SPA-K98-SRYCD(IDX-K98)
                                               TO  SPA-GMN-INPUTCD
                                                       (SPA-GMN-IDX)
                   END-IF
               END-IF
      *
               IF      SPA-ERRCD       =   SPACE
                   MOVE    "1"             TO  SPA-COM-IN (SPA-GMN-IDX)
                   MOVE    "1"             TO  SPA-COM-IN2(SPA-GMN-IDX)
               END-IF
      *
           END-PERFORM
      *
      *    入力コード・名称チェック処理
           PERFORM 410112-KOUI-SPACHK-SEC
      *
           .
       3011-K98-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＫＩＤ１）ＯＫ処理
      *****************************************************************
       30010-KID1-SET-SEC          SECTION.
      *
           EVALUATE    SPA-KIDCD
               WHEN    "0103"
                   MOVE    SPACE               TO  SPA-KIDCD
      *************IF      SPA-KID1-FLG        =   "OK"
      *                画面制御
                       MOVE    "JOIN"          TO  WRK-PUTTYPE
                       MOVE    1               TO  SPA-CONTFLG
      *                Ｋ０１画面へ
                       PERFORM 4001-K01-SYORI-SEC
      *************END-IF
      *
               WHEN    "0104"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
      *                終了処理へ
                       PERFORM 210-BACK-OK-SEC
                   END-IF
      *        保険組合せ変更確認
               WHEN    "0105"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    ZERO            TO  FLG-CHK2
                       PERFORM 41012-HKNCOMBI-CHANGE-SEC
                   ELSE
                       MOVE    SPA-MAE-HKNCOMBI-G
                                           TO  SPA-GMN-HKNCOMBI-G
                       PERFORM 3101-HKNCOMBI-HEN-SEC
                   END-IF
      *
      *        診療科変更確認
               WHEN    "0107"
                   MOVE    SPACE               TO  SPA-KIDCD
                   PERFORM 41013-SRYKA-CHANGE-SEC
      *        診療科変更確認
               WHEN    "0122"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    3               TO  FLG-PARASET
                       PERFORM                 3002-PARA-SET-SEC
      *                画面用スパ編集処理
                       PERFORM                     310-SPA-GMNSET-SEC
      *                ワーク診療行為クリア
                       MOVE    1                   TO  FLG-MOD-FLG
                       PERFORM 4900-WKSRYACT-CLEAR-SEC
                   ELSE
      *                ワーク診療行為クリア
                       MOVE    2                   TO  FLG-MOD-FLG
                       PERFORM 4900-WKSRYACT-CLEAR-SEC
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
      *
      *        診療科変更確認
               WHEN    "0123"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       PERFORM 41013-SRYKA-CHANGE-SEC
                   ELSE
                       MOVE    SPA-MAE-SRYKA-G     TO  SPA-GMN-SRYKA-G
                       MOVE    1                   TO  SPA-GMN-CUR
                   END-IF
      *
      *        診療日が未来日の確認
               WHEN    "0111"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
      *                診療日確定後、編集
                       PERFORM 410171-SRYYMD-OK-SEC
                   ELSE
                       MOVE    7               TO  SPA-GMN-CUR
                   END-IF
      *
      *        特定疾患処方管理加算の算定確認
               WHEN    "0113"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    2                   TO  SPA-TOKUTEIFLG
                   ELSE
                       MOVE    1                   TO  SPA-TOKUTEIFLG
                   END-IF
                   MOVE    SPACE               TO  SPA-KIDCD
                   MOVE    SPACE               TO  SPA-KID1-FLG
      *            画面制御
                   MOVE    "JOIN"              TO  WRK-PUTTYPE
                   PERFORM 41002-KOUSIN-SYORI-SEC
      *
      *        クリアの確認
               WHEN    "0116"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       PERFORM 220-CLEAR-SYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
      *
      *        保険変更時、中途終了あり
               WHEN    "0118"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
      *                内容置換え
                       PERFORM 410132-SAIHENSYU-SEC
                   ELSE
      *                ワーク診療行為クリア
                       MOVE    2                   TO  FLG-MOD-FLG
                       PERFORM 4900-WKSRYACT-CLEAR-SEC
                   END-IF
      *        中途終了時刷確認
               WHEN    "0117"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       PERFORM 4510-SNRYO-NYU-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
      *        退院時外来へ
               WHEN    "0120"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
      *                外来へ
                       MOVE    SPA-GMN-PTID        TO  SPA-PTID
                       MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
                       INITIALIZE                  SPA-K01KYOUTU
                       INITIALIZE                  SPA-K02
                       MOVE    "JOIN"              TO  WRK-PUTTYPE
                       PERFORM 410109-K02IDOU-SEC
                   ELSE
      *                退院日を診療日へ
                       MOVE    SPA-K02N-TAIINYMD   TO  WRK-SYMD
                       PERFORM 520-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG         TO  SPA-SRYYMDW
                       MOVE    SPA-K02N-TAIINYMD   TO  SPA-SRYYMD
                       MOVE    SPA-GMN-PTNUM       TO  K02N-PTNUM
                       PERFORM 41010-PTNUM-SYORI-SEC
                   END-IF
      *
      *        リハビリ逓減確認
               WHEN    "0180"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                        MOVE    1                  TO  FLG-RIHTEI
      *                 診療行為、計計算処理、編集
                        MOVE    SPACE              TO  FLG-TOROKU
                        PERFORM 510-TBLHEN-SEC
                        MOVE    SPACE              TO  FLG-TOROKU
                   ELSE
                        MOVE    ZERO               TO  FLG-RIHTEI
      *                 診療行為、計計算処理、編集
                        MOVE    SPACE              TO  FLG-TOROKU
                        PERFORM 510-TBLHEN-SEC
                        MOVE    SPACE              TO  FLG-TOROKU
                   END-IF
      *        クリア確認チェック
               WHEN    "0125"
                   IF      SPA-KID1-FLG        =   "OK"
                       PERFORM 2221-PTNUMCLEAR-SYORI-SEC
                   ELSE
                        MOVE    1               TO  SPA-GMN-CUR
                   END-IF
                   MOVE    SPACE               TO  SPA-KIDCD
                   MOVE    SPACE               TO  SPA-KID1-FLG
      *
           END-EVALUATE
           MOVE    SPACE               TO  SPA-KID1-FLG
           .
       30010-KID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＫＩＤ２）ＯＫ処理
      *****************************************************************
       30011-KID2-SET-SEC          SECTION.
      *
           MOVE    SPACE               TO  SPA-KID2-FLG
           MOVE    SPACE               TO  SPA-KIDCD2
           .
       30011-KID2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＯ選択（Ｋ０９）画面より処理
      *****************************************************************
       3013-K09-SET-SEC                  SECTION.
      *
      *    選択がない時、そのまま
           IF      SPA-K09-SELOK       =   ZERO
               MOVE    1               TO  SPA-GMN-CUR
               GO      TO      3013-K09-SET-EXT
           END-IF
      *
      *    パラメタファイルより表示
           MOVE    ZERO            TO  FLG-PARASET
           PERFORM 3002-PARA-SET-SEC
      *
      *    ＤＯ画面（Ｋ０９）より、ＤＯ内容追加編集
           MOVE    SPA-GMN-MAX         TO  IDX2
           MOVE    SPA-GMN-MAX         TO  SPA-GMN-IDX
      *
      *    再表示編集処理
           IF     (SPA-K09-SELOK       =   1    )  AND
                  (LNK-WKSRYACT-MAX    >   ZERO )
      *********MOVE    ZERO            TO  FLG-SYOSIN
               MOVE    SPACE               TO  WRK-ERRCD1
               MOVE    1               TO  FLG-K09TUIKA
               PERFORM 3103-SAIHEN-HEN-SEC
      *
               IF      WRK-ERRCD1      NOT =   SPACE
                   MOVE    WRK-ERRCD1          TO  SPA-ERRCD
               END-IF
      *
      *        診療行為、計計算処理、編集
               MOVE    SPACE           TO  FLG-TOROKU
               MOVE    1               TO  FLG-SYOKI
               PERFORM 510-TBLHEN-SEC
               MOVE    SPACE           TO  FLG-TOROKU
           END-IF
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       3013-K09-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     入力コード設定より編集処理
      *****************************************************************
       30014-K023-SET-SEC                  SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
               IF     (SPA-GMN-INPUTCD (IDX)(1:1)  =   "." OR "S" )  OR
                      (SPA-COM-INPUTCD (IDX)       =   SPACE     ) 
                   CONTINUE
               ELSE
      *
                   MOVE    SPACE               TO  INPUTCD-REC
                   MOVE    SPA-HOSPID          TO  ICD-HOSPID
                   MOVE    SPA-COM-INPUTCD(IDX) 
                                               TO  ICD-SRYCD
      *
                   MOVE    "INPUTCD-KEY4"      TO  WRK-PATH
      *
                   PERFORM 930-INPUTCD-READ-SEC
                   MOVE    SPACE           TO  SPA-COM-GMNFLG (IDX)
                   IF      FLG-INPUTCD         =   ZERO
                       MOVE    ICD-INPUTCD     TO  SPA-COM-ICD-INPUTCD
                                                              (IDX)
                   ELSE
                       MOVE    SPACE           TO  SPA-COM-ICD-INPUTCD
                                                              (IDX)
                   END-IF
                   MOVE    SPACE               TO  WRK-PATH
      *
      *            画面再編集・基本チェック
                   INITIALIZE                  ORCSC94AREA
                   MOVE    "2"                 TO  LNK-SC94-KBN
                   MOVE    "K02"               TO  LNK-SC94-PG
                   MOVE    IDX                 TO  LNK-SC94-IDX
                   MOVE    SPA-SYORIFLG        TO  LNK-SC94-SYORIFLG
                   CALL    "ORCSC94"           USING
                                               ORCSC94AREA
                                               SPA-SCR-REC
                                               SPA-AREA
               END-IF
           END-PERFORM
      *
           MOVE    1                    TO  SPA-GMN-CUR
      *
           .
       30014-K023-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    指示せん発行画面（Ｋ０８１）より処理
      *****************************************************************
       3014-K081-SET-SEC                  SECTION.
      *
           MOVE    SPA-ETCKYOUTU-AREA  TO  SPA-K081
      *
           EVALUATE    SPA-K081-OKFLG
               WHEN    ZERO
                   MOVE    1               TO  SPA-GMN-CUR
               WHEN    1
      *        登録・発行
      *            発行有無判定
                   IF      (SPA-K081GMN-SYOHOU     =   "1" )  OR
                           (SPA-K081GMN-CHUSYA     =   "1" )  OR
                           (SPA-K081GMN-SIJISEN    =   "1" )
                       MOVE    1               TO  FLG-SIJISEN
                   ELSE
                       MOVE    ZERO            TO  FLG-SIJISEN
                   END-IF
      *            中途終了登録
                   PERFORM 4510-SNRYO-NYU-SEC
               WHEN    2
      *        発行のみ
                   IF      (SPA-K081GMN-SYOHOU     =   "1" )  OR
                           (SPA-K081GMN-CHUSYA     =   "1" )  OR
                           (SPA-K081GMN-SIJISEN    =   "1" )
                       MOVE    "2"                 TO  FLG-SYORI
                       PERFORM 45101-SNRYO-SYORI-SEC
      *                入院指示せん発行処理
                       PERFORM 45900-SIJISEN-SYORI-SEC
                   END-IF
           END-EVALUATE
      *
           .
       3014-K081-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     パラメタファイルより編集処理
      *****************************************************************
       3002-PARA-SET-SEC                  SECTION.
      *
           MOVE    ZERO                TO  LNK-WKSRYACT-MAX
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA.
      *
           MOVE    SPA-TERMID          TO  FILE-TERMID2
           OPEN    INPUT               PARA-FILE
      *
           IF      STS-PARA           =  ZERO
               PERFORM 980-PARA-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PARA 
           END-IF
           PERFORM
               UNTIL   FLG-PARA            =   1
               IF      PARA-FILEMEI        =   "WKSRYACT"
                   IF      PARA-DATA-REC   NOT =   SPACE
                       ADD     1                   TO  LNK-WKSRYACT-MAX
                       MOVE    PARA-DATA-REC       TO  LNK-WKSRYACT-REC
                                                     (LNK-WKSRYACT-MAX)
      ***************  MOVE    PARA-DATA-REC       TO  LNK-WKSRYACT-REC
      *                                                (PARA-EDANUM)
      ***************  MOVE    PARA-EDANUM        TO  LNK-WKSRYACT-MAX
                   END-IF
               END-IF
      *
               IF      PARA-FILEMEI        =   "K01PARA"
                   IF      FLG-PARASET     =   ZERO    OR  1
                       MOVE    PARA-DATA-REC       TO  SPA-K01KYOUTU
                   END-IF
               END-IF
      *
               IF      PARA-FILEMEI        =   "K02SPA"
                   IF      FLG-PARASET     =   ZERO  OR  2
                       IF      PARA-EDANUM     =   1
                           MOVE    PARA-DATA-REC   TO  SPA-K02
                       ELSE
                           COMPUTE IDX-NUM     =  (PARA-EDANUM  -  1)
                                               *   60000
                                               +   1
                           MOVE    PARA-DATA-REC   TO  SPA-K02(IDX-NUM:)
                       END-IF
                   END-IF
               END-IF
      *
               PERFORM 980-PARA-READ-SEC
           END-PERFORM
      *
           CLOSE                       PARA-FILE
      *
           .
       3002-PARA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT
      *
      *    見出し
           MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
           MOVE    SPA-PTID            TO  SPA-GMN-PTID
           MOVE    SPA-NAME            TO  SPA-GMN-NAME
           MOVE    SPA-KANANAME        TO  SPA-GMN-KANANAME
           MOVE    SPA-SEX             TO  SPA-GMN-SEX
           MOVE    SPA-BIRTHDAYW       TO  SPA-GMN-BIRTHDAY
           MOVE    SPA-SRYYMDW         TO  SPA-GMN-SRYYMD
           MOVE    SPA-BIRTHDAY        TO  SPA-NAI-BIRTHDAY
           MOVE    SPA-SRYYMD          TO  SPA-NAI-SRYYMD
           MOVE    SPA-SRYKA           TO  SPA-GMN-SRYKA
           MOVE    SPA-FTNRATE         TO  SPA-GMN-FTNRATE
      *
      *    診療科
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SRYKA-MAX
               IF      SPA-GMN-SRYKA   =   SPA-GMN-SRYKAL (IDX)
                   MOVE    SPA-GMN-SRYKALST (IDX)
                                           TO  SPA-GMN-SRYKA-G
                   MOVE    SPA-SRYKA-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
      *    保険組合
           MOVE    SPA-HKNCOMBINUM     TO  SPA-GMN-HKNCOMBINUM
           PERFORM 3101-HKNCOMBI-HEN-SEC
      *
      *    時間外区分
           MOVE    "0 時間内"          TO  SPA-K02N-JIKANKBN-LST(1)
           MOVE    "1 時間外"          TO  SPA-K02N-JIKANKBN-LST(2)
           MOVE    "2 休日"            TO  SPA-K02N-JIKANKBN-LST(3)
           MOVE    "3 深夜"            TO  SPA-K02N-JIKANKBN-LST(4)
           MOVE    "4 時間外特例"      TO  SPA-K02N-JIKANKBN-LST(5)
           IF      SPA-K02N-JIKANKBN   =   SPACE
               MOVE    "0"             TO  SPA-K02N-JIKANKBN
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   5
               IF      SPA-K02N-JIKANKBN   =   SPA-K02N-JIKANKBN-LST
                                                          (IDX)(1:1)
                   MOVE    SPA-K02N-JIKANKBN-LST (IDX)
                                           TO  SPA-K02N-JIKANKBN-G
                   MOVE   5                TO  IDX
               END-IF
           END-PERFORM
      *
      *    年齢計算
      **** MOVE    SPA-BIRTHDAY        TO  WRK-YMDS1
      *    MOVE    SPA-SRYYMD          TO  WRK-YMDS2
      *    COMPUTE SPA-NENREI-YY       =   WRK-YYS2
      *                                -   WRK-YYS1
      *    IF      WRK-YMDS1(5:4)      >   WRK-YMDS2(5:4)
      *        COMPUTE SPA-NENREI-YY   =   SPA-NENREI-YY
      *                                    -   1
      *        COMPUTE SPA-NENREI-MM   =  (WRK-MMS2  +  12 )
      *                                    -   WRK-MMS1
      *    ELSE
      *        COMPUTE SPA-NENREI-MM   =   WRK-MMS2
      *                                    -   WRK-MMS1
      *    END-IF
      *    IF      WRK-DDS1            >   WRK-DDS2
      *        COMPUTE SPA-NENREI-MM   =   SPA-NENREI-MM
      *                                    -   1
      *    END-IF
      *
      *    年齢期間算定
      *    INITIALIZE                      STS-AREA-DAY
      *    INITIALIZE                      LNK-DAY5-AREA
      *    MOVE    "51"                TO  LNK-DAY5-IRAI
      *    MOVE    SPA-BIRTHDAY        TO  LNK-DAY5-START
      *    MOVE    SPA-SRYYMD          TO  LNK-DAY5-END
      *    CALL    "ORCSDAY"           USING   STS-AREA-DAY
      *                                        LNK-DAY5-AREA
      *    IF      LNK-DAY5-NISSU2     <=  31
      *        MOVE    LNK-DAY5-NISSU2     TO  SPA-NENREI-DD
      *        MOVE    ZERO                TO  SPA-NENREI-MM
      *****END-IF
      *
      *    年齢計算
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPID          TO  AGECHK-HOSPID
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    SPA-SRYYMD          TO  AGECHK-SRYYMD
           MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
           MOVE    SPA-NYUGAIKBN       TO  AGECHK-NYUGAIKBN
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
           MOVE    AGECHK-NENREI2-YY   TO  SPA-NENREI-YY
           MOVE    AGECHK-NENREI2-MM   TO  SPA-NENREI-MM
           MOVE    AGECHK-NENREI2-DD   TO  SPA-NENREI-DD
      *
      *    画面表示用
           IF      SPA-NENREI-YY       =   ZERO
               MOVE    SPA-NENREI-MM       TO  WRK-NENREI-ZN
               MOVE    "ヶ月"              TO  WRK-NENREI-MEIN
           ELSE
               MOVE    SPA-NENREI-YY       TO  WRK-NENREI-Z
               MOVE    "才"                TO  WRK-NENREI-MEI
           END-IF
           MOVE    WRK-NENREI-G        TO  SPA-NENREI-HEN
      *
      *    診療日の末日を求める
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SPA-SRYYMD(1:6)     TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY7-KOYOMI(7:2)    TO  SPA-SRYLST-DD
           ELSE
               MOVE    31                  TO  SPA-SRYLST-DD
           END-IF
      *
      **** MOVE    ZERO                TO  SPA-NAI-ROUSTR
      *****MOVE    ZERO                TO  SPA-NAI-ROUFLG
      *老人判定は ORCSAGECHK へ
      *****老人判定
      *****PERFORM 3102-ROUJIN-SET-SEC
      *
      *    ＤＯ診療日付編集
      *****PERFORM 3101-DORRKYMD-HEN-SEC
      *
      *    ＤＯ編集
           MOVE    "F"                 TO  SPA-GMN-TEISEI
           MOVE    ZERO                TO  SPA-NAI-SYORI
      *
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人判定処理
      *****************************************************************
       3102-ROUJIN-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-ROUJIN
      *    老人判定
           IF     (SPA-KOH1HKNNUM       =   "027"   )  OR
                  (SPA-KOH2HKNNUM       =   "027"   )  OR
                  (SPA-KOH3HKNNUM       =   "027"   )  OR
                  (SPA-KOH4HKNNUM       =   "027"   )
               MOVE    1                TO  SPA-NAI-ROUJIN
           ELSE
      *        Ｈ１４．１０以降７０歳を７５歳とする
      *        Ｈ１４．１０時点で７０歳未満の時
               IF     (SPA-SRYYMD          >=   "20021001" )  AND
                      (SPA-BIRTHDAY        >=   "19321001" )
                   MOVE    75                   TO  WRK-ROU-NEN
               ELSE
                   MOVE    70                   TO  WRK-ROU-NEN
               END-IF
      *        ７０歳以上は老人とする（該当月１日に７０歳）
      *        年齢計算（診療月の１日の年齢）
               MOVE    SPA-BIRTHDAY        TO  WRK-YMDS1
               MOVE    SPA-SRYYMD          TO  WRK-YMDS2
               MOVE    01                  TO  WRK-DDS2
               COMPUTE WRK-NENREI-YY       =   WRK-YYS2
                                           -   WRK-YYS1
               IF      WRK-YMDS1(5:4)      >   WRK-YMDS2(5:4)
                   COMPUTE WRK-NENREI-YY   =   WRK-NENREI-YY
                                           -   1
                   COMPUTE WRK-NENREI-MM   =  (WRK-MMS2  +  12 )
                                           -   WRK-MMS1
               ELSE
                   COMPUTE WRK-NENREI-MM   =   WRK-MMS2
                                           -   WRK-MMS1
               END-IF
               IF      WRK-DDS1            >   WRK-DDS2
                   COMPUTE WRK-NENREI-MM   =   WRK-NENREI-MM
                                           -   1
               END-IF
      *********IF      WRK-NENREI-YY       >=  70
               IF      WRK-NENREI-YY       >=  WRK-ROU-NEN
                   MOVE    1               TO  SPA-NAI-ROUJIN
               END-IF
           END-IF
      *    労災・自賠責の場合、老人でも一般とする
           IF      SPA-HKNKBN          =   1
               MOVE    ZERO                TO  SPA-NAI-ROUJIN
           END-IF
      *
      *
           .
       3102-ROUJIN-SET-EXT.
           EXIT.
      *****************************************************************
      *    保険組合決定編集処理
      *****************************************************************
       3101-HKNCOMBI-HEN-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-HKN-MAX )  OR
                              (FLG-CHK     =    1   ) 
               IF      SPA-GMN-HKNCOMBINUM   =   SPA-GMN-THKNCOMBINUM
                                                             (IDX)
                   MOVE    IDX               TO  IDY
                   MOVE    1                 TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               IF      SPA-GMN-HKNCOMBINUM =   SPACE
                   MOVE   1                TO  IDX
               ELSE
      *             選択した保険がない場合、エラー
                   MOVE   "1034"               TO  SPA-ERRCD
                   MOVE    4                   TO  SPA-HKN-ERR
                   GO     TO    3101-HKNCOMBI-HEN-EXT
               END-IF
           ELSE
               MOVE   IDY              TO  IDX
           END-IF
      *
      *    アフターケアの場合エラーとする
           IF     (SPA-NAI-HKNNUM     (IDX)    =   "971" ) AND
                  (SPA-NAI-RSI-HKNKBN (IDX)    =   "3" )
               MOVE    "9029"              TO  SPA-ERRCD
               GO      TO      3101-HKNCOMBI-HEN-EXT
           END-IF
           MOVE    ZERO                TO  SPA-HKN-ERR
      *
           MOVE    SPA-GMN-HKN-LIST (IDX)
                                       TO  SPA-GMN-HKNCOMBI-G
           MOVE    SPA-GMN-TFTNRATEMEI (IDX)
                                       TO  SPA-GMN-FTNRATEMEI
           MOVE    SPA-NAI-TFTNRATE    (IDX)
                                       TO  SPA-GMN-FTNRATE
      *    共通ＳＰＡへ
           MOVE    SPA-GMN-HKNCOMBINUM TO  SPA-HKNCOMBINUM
           MOVE    SPA-GMN-HKNCOMBIMEI TO  SPA-HKNCOMBIMEI
           MOVE    SPA-NAI-TFTNRATE    (IDX)
                                       TO  SPA-FTNRATE
           MOVE    SPA-GMN-TFTNRATEMEI (IDX)
                                       TO  SPA-FTNRATEMEI
           MOVE    SPA-NAI-HKNNUM     (IDX)
                                       TO  SPA-HKNNUM
           MOVE    SPA-NAI-KOH1HKNNUM  (IDX)
                                       TO  SPA-KOH1HKNNUM
           MOVE    SPA-NAI-KOH2HKNNUM  (IDX)
                                       TO  SPA-KOH2HKNNUM
           MOVE    SPA-NAI-KOH3HKNNUM  (IDX)
                                       TO  SPA-KOH3HKNNUM
           MOVE    SPA-NAI-KOH4HKNNUM  (IDX)
                                       TO  SPA-KOH4HKNNUM
      *
      *    労災 四肢区分
           MOVE    SPA-NAI-RSI-SISIKBN (IDX)
                                       TO  SPA-RSI-SISIKBN
      *    労災 傷病年月日
           MOVE    SPA-NAI-RSI-SHOBYOYMD (IDX)
                                       TO  SPA-RSI-SHOBYOYMD
           MOVE    SPA-NAI-RSI-HKNKBN (IDX)
                                       TO  SPA-RSI-HKNKBN
      *    労災判定
           IF      SPA-HKNNUM          =   "971"
                                       OR  "973"
               MOVE    1               TO  SPA-HKNKBN
           ELSE
               MOVE    ZERO            TO  SPA-HKNKBN
          END-IF
      *****老人判定
      *****PERFORM 3102-ROUJIN-SET-SEC
      *
      *    保険算定年齢チェック等
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPID          TO  AGECHK-HOSPID
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    SPA-SRYYMD          TO  AGECHK-SRYYMD
           MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
           MOVE    SPA-HKNCOMBINUM     TO  AGECHK-HKNCOMBINUM
           MOVE    SPA-NYUGAIKBN       TO  AGECHK-NYUGAIKBN
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
           IF      AGECHK-FTNRATE  NOT =   ZERO
      *        負担率
               MOVE    AGECHK-FTNRATEMEI   TO  SPA-GMN-FTNRATEMEI
               MOVE    AGECHK-FTNRATEMEI   TO  SPA-FTNRATEMEI
               MOVE    AGECHK-FTNRATE      TO  SPA-GMN-FTNRATE
               MOVE    AGECHK-FTNRATE      TO  SPA-FTNRATE
           END-IF
           MOVE    ZERO                TO  SPA-RJN-ERR
           MOVE    ZERO                TO  SPA-HKN-ERR
      *
      *    Ｈ１５．４以降の保険期間が違う時
           IF      AGECHK-HKNCHK       =   1
               MOVE    "1028"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-HKN-ERR
           END-IF
      *    Ｈ１５．４以降の公費期間が違う時
           IF      AGECHK-HKNCHK       =   2
               MOVE    "1032"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-HKN-ERR
           END-IF
      *    老人でＨ１４．１０の保険変更がない時
           IF      AGECHK-RJNCHK       =   1
               MOVE    "1027"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-RJN-ERR
           END-IF
           IF      AGECHK-RJNCHK       =   2
      ****     IF      SPA-NAI-ROUSTR      =   ZERO
      *            MOVE    "K008"              TO  SPA-ERRCD
      *****    END-IF
               MOVE    1                   TO  SPA-NAI-ROUFLG
           ELSE
               MOVE    ZERO                TO  SPA-NAI-ROUFLG
           END-IF
      *
      *    老人判定
           MOVE    AGECHK-ROUJIN       TO  SPA-NAI-ROUJIN
      *
           .
       3101-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面用スパ編集処理
      *****************************************************************
       310-SPA-GMNSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  SPA-GMN-IDX
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      310-SPA-GMNSET-EXT
           END-IF
      *
      *****MOVE    "2"                 TO  FLG-TOROKU
           MOVE    ZERO                TO  SPA-NAI-TIMEFLG
           MOVE    SPACE               TO  WRK-ERRCD1
      *
      *    再表示編集処理
           IF      LNK-WKSRYACT-MAX    >   ZERO
               MOVE    ZERO            TO  FLG-K09TUIKA
               PERFORM 3103-SAIHEN-HEN-SEC
           END-IF
      *    受診回数編集処理
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSC10SAREA
           MOVE    "3"                 TO  LNK-SC10S-SYORI
           CALL    "ORCSC10S"          USING
                                       ORCSC10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPACE               TO  SPA-KIDCD
      *    初診料自動発生
           IF      (SPA-NYU-SYOSIN     =   "1" )  AND
                   (SPA-SYORIFLG       =   ZERO)
               PERFORM 330-SYOSIN-SET-SEC
           END-IF
      *
      *    時間外区分
           MOVE    SPA-NAI-TIMEFLG     TO  SPA-K02N-JIKANKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   5
               IF      SPA-K02N-JIKANKBN   =   SPA-K02N-JIKANKBN-LST
                                                          (IDX)(1:1)
                   MOVE    SPA-K02N-JIKANKBN-LST (IDX)
                                           TO  SPA-K02N-JIKANKBN-G
                   MOVE    5               TO  IDX
               END-IF
           END-PERFORM
      *
      *    入力コード・名称チェック処理
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX        >   200 )  OR
                          ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
               IF      SPA-COM-CUR (IDX)   =   SPACE
                   MOVE    SPA-GMN-INPUTCD (IDX)
                                           TO  SPA-MAE-INPUTCD (IDX)
               ELSE
                   MOVE    SPACE           TO  SPA-MAE-INPUTCD (IDX)
               END-IF
      *        警告表示をしない
               MOVE    "1"                 TO  SPA-COM-KZMFLG(IDX)
           END-PERFORM
      *    内分泌負荷試験検査情報
           INITIALIZE              SPA-NAI-FUKATEN-AREA
      *    診療行為、計計算処理、編集
           MOVE    1               TO  FLG-INIT
           MOVE    SPACE           TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE           TO  FLG-TOROKU
      *    訂正時点数変更チェック
           MOVE    ZERO                TO  SPA-TEISEI-CHK
           MOVE    ZERO                TO  SPA-TEISEI-ERR
      *    警告フラグクリア
           MOVE    ZERO                TO  WRK-MAE-TENSU
           MOVE    ZERO                TO  FLG-MAE-TENSU
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX        >   200 )  OR
                          ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
               IF     (SPA-COM-KZMFLG (IDX)    NOT =   SPACE )  OR
                      (SPA-COM-KZMFLG2(IDX)    NOT =   SPACE )  OR
                      (SPA-COM-NYU-KZMFLG-G(IDX)    NOT =   SPACE )
                   MOVE    SPACE           TO  SPA-MAE-INPUTCD (IDX)
               END-IF
               MOVE    SPACE           TO  SPA-COM-KZMFLG (IDX)
               MOVE    SPACE           TO  SPA-COM-KZMFLG2(IDX)
               MOVE    SPACE           TO  SPA-COM-NYU-KZMFLG-G(IDX)
      *        訂正時の点数判定
               IF      SPA-SYORIFLG        =   1
                   IF      FLG-MAE-TENSU   =   ZERO
                       IF     (SPA-COM-MAE-TENSU(IDX)  >   ZERO  )  OR
                              (SPA-COM-INPUTCD (IDX)   NOT =   SPACE)
                           IF     (SPA-COM-MAE-TENSU(IDX)  =   ZERO) AND
                                  (SPA-COM-INPUTCD (IDX)(1:1)  =  "8"
                                                               OR "0")
                               CONTINUE
                           ELSE
                               MOVE    SPA-COM-MAE-TENSU(IDX)
                                                   TO  WRK-MAE-TENSU
                               MOVE    1           TO  FLG-MAE-TENSU
                           END-IF
                       END-IF
                   END-IF
                   IF     (SPA-COM-ZAIEND(IDX)     =   "1" )  OR
                         ((SPA-COM-SRYKBN(IDX)     =   "95"
                                                   OR  "96" ) AND
                          (SPA-COM-JIHIMONEY(IDX)  >   ZERO ))
                       IF      SPA-COM-SRYKBN(IDX)     =   "95"
                                                       OR  "96"
                           MOVE    SPA-COM-JIHIMONEY(IDX)
                                                   TO  WRK-TENSU
                       ELSE
                           MOVE    SPA-COM-TENSU (IDX)
                                                   TO  WRK-TENSU
                       END-IF
                       IF      WRK-TENSU       NOT =   WRK-MAE-TENSU
                           MOVE    1               TO  SPA-TEISEI-ERR
                           MOVE    "1"             TO  SPA-COM-CUR (IDX)
                       END-IF
                       MOVE    ZERO            TO  WRK-MAE-TENSU
                       MOVE    ZERO            TO  FLG-MAE-TENSU
                   END-IF
                   IF      SPA-COM-ZAIEND(IDX)     =   "1"
                       MOVE    ZERO                TO  WRK-MAE-TENSU
                       MOVE    ZERO                TO  FLG-MAE-TENSU
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPACE               TO  SPA-KIDCD
      *    内分泌負荷試験検査情報
           INITIALIZE              SPA-NAI-FUKATEN-AREA
      *
           IF      WRK-ERRCD1      NOT =   SPACE
               MOVE    WRK-ERRCD1          TO  SPA-ERRCD
           END-IF
      *
           IF     (SPA-TEISEI-ERR      =   1  )  AND
                  (SPA-ERRCD           =   SPACE)
               MOVE    "K999"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-TEISEI-CHK
           END-IF
      *
           .
       310-SPA-GMNSET-EXT.
           EXIT.
      *
      *****************************************************************
      *   再表示あり　編集処理
      *****************************************************************
       3103-SAIHEN-HEN-SEC                  SECTION.
      *
           INITIALIZE                  ORCSCWKSRYAREA
           MOVE    "1"                 TO  ORCSCWKSRY-KBN
           MOVE    "K02N"              TO  ORCSCWKSRY-PG
      *    処理区分（０：訂正展開、１：中途終了展開、２：ＤＯ展開）
           IF      FLG-K09TUIKA        =   1
               MOVE    2               TO  ORCSCWKSRY-SYORIFLG
           ELSE
               MOVE    1               TO  ORCSCWKSRY-SYORIFLG
               IF      SPA-SYORIFLG    =   1
                   MOVE    ZERO            TO  ORCSCWKSRY-SYORIFLG
               END-IF
           END-IF
      *    中途終了分追加フラグ
           IF      FLG-K09TUIKA        =   3
              MOVE    1                TO  ORCSCWKSRY-K09TUIKA
           END-IF
      *    追加開始位置
           MOVE    IDX2                TO  ORCSCWKSRY-STRIDX
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           CALL    "ORCSCWKSRY"        USING
                                       ORCSCWKSRYAREA
                                       SPA-AREA
                                       SPA-K02
                                       SPA-SCR-REC
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           IF      ORCSCWKSRY-ERRCD    NOT =   SPACE
               MOVE    ORCSCWKSRY-ERRCD    TO  WRK-ERRCD1
           END-IF
      *
           .
       3103-SAIHEN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック用月内診療行為セット処理
      *****************************************************************
       320-SPA-CHKSET-SEC              SECTION.
      *
      *    当月診療行為チェック用編集、当月累計点数計算
           PERFORM 3201-TOUCHKSET-SEC
      *
      *    禁忌薬剤チェック用編集
           PERFORM 3202-KNKYAKSET-SEC
      *
           .
       320-SPA-CHKSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック用月内診療行為セット処理
      *****************************************************************
       3201-TOUCHKSET-SEC              SECTION.
      *
           MOVE    ZERO                TO  SPA-ZAITENTOTAL
           MOVE    SPACE               TO  SPA-ACCT2-SRYCD
           MOVE    ZERO                TO  SPA-ACCT2-ZAINUM
           MOVE    ZERO                TO  SPA-ACCT2-CSYYOURYO
           MOVE    SPACE               TO  SPA-CHOKI-ARI
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDX-SIN
           MOVE    ZERO                TO  IDX-SIN2
      *    診療年月の診療会計マスタ読み込み
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPID          TO  ACCT-HOSPID
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
      *****MOVE    SPA-SRYKA           TO  ACCT-SRYKA
           MOVE    SPA-SRYYMD(1:6)     TO  ACCT-SRYYM
           MOVE    SPA-SRYYMD(7:2)     TO  WRK-TDD
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SRYACCT-KEY7"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SRYACCT-KEY7"      TO  ORC-DBPATH
               PERFORM 940-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           PERFORM UNTIL       (FLG-SRYACCT         =   1       )
      *
      *        点数累計
               IF     (ACCT-ZAITEN         =   ZERO )  OR
                      (ACCT-ZAIKAISU       =   ZERO )  OR
                      (ACCT-SRYKBN         =   "95" OR "96")
                   CONTINUE
               ELSE
                   COMPUTE SPA-ZAITENTOTAL  =   SPA-ZAITENTOTAL
                                            +  (ACCT-ZAITEN
                                            *   ACCT-ZAIKAISU )
               END-IF
      *
      *        検査で回数があるもの
               IF     (ACCT-ZAIKAISU   NOT =   ZERO  )  AND
                      (ACCT-SRYKBN         =   "60"  )
                   MOVE    1               TO  FLG-ACCTSYORI
      *            診療行為マスター編集
                   PERFORM 3201-SRYACT-HEN-SEC
               END-IF
      *
      *        入院調剤料で回数があるもの
               IF     (ACCT-ZAIKAISU   NOT =   ZERO  )  AND
                      (ACCT-SRYKBN         =   "24"  )
                   MOVE    "1"             TO  SPA-CHOKI-ARI
               END-IF
      *（注射のためにチェックしていたが、現在していないのではずす）
      *        当日算定分
      *        IF      ACCT-DAY(1 WRK-TDD) >   ZERO
      *            MOVE    2               TO  FLG-ACCTSYORI
      *            診療行為マスター編集
      *            PERFORM 3201-SRYACT-HEN-SEC
      ******** END-IF
      *
               MOVE    "SRYACCT-KEY7"      TO  ORC-DBPATH
               PERFORM 940-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SRYACCT-KEY7"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           .
       3201-TOUCHKSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター編集処理
      *****************************************************************
       3201-SRYACT-HEN-SEC            SECTION.
      *
      *    診療会計マスターから診療行為マスターを検索する
           MOVE    SPACE               TO  SRYACT-REC
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPID         TO  SRY-HOSPID
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
               PERFORM 900-SRYACT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL      FLG-SRYACT   =   1
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   5   )  OR
                                  (SRY-SRYCD(IDX2)     =   SPACE )
                   IF      SRY-SRYCD (IDX2)(1:1)   =   "8"  OR
                                                       "0"
                       CONTINUE
                   ELSE
                       EVALUATE    FLG-ACCTSYORI
                           WHEN    1
                               PERFORM 32011-SRYACT-HEN1-SEC
                           WHEN    2
                               PERFORM 32012-SRYACT-HEN2-SEC
                       END-EVALUATE
                   END-IF
               END-PERFORM
      *
               MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
               PERFORM 900-SRYACT-NEXT-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       3201-SRYACT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為チェック用編集１処理（検査）
      *****************************************************************
       32011-SRYACT-HEN1-SEC           SECTION.
      *
           ADD     1                   TO  IDX-SIN
           IF      IDX-SIN             >   WRK-GYO-MAX
               MOVE    WRK-GYO-MAX         TO  IDX-SIN
           END-IF
           MOVE    ACCT-SRYKBN         TO  SPA-ACCT-SRYKBN
                                                      (IDX-SIN)
           MOVE    SRY-SRYCD (IDX2)    TO  SPA-ACCT-SRYCD
                                                       (IDX-SIN)
      *    点数マスタより、区分番号編集
           MOVE    SRY-SRYCD (IDX2)    TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           MOVE    TNS-CDKBN-KBN       TO  SPA-ACCT-CDKBN-KBN
                                                          (IDX-SIN)
           MOVE    TNS-CDKBN-KBNNUM    TO  SPA-ACCT-CDKBN-KBNNUM
                                                          (IDX-SIN)
      **** MOVE    TNS-CDKBN-SYO       TO  SPA-ACCT-CDKBN-SYO
      *                                                   (IDX-SIN)
      *    MOVE    TNS-CDKBN-BU        TO  SPA-ACCT-CDKBN-BU
      *****                                               (IDX-SIN)
           .
       32011-SRYACT-HEN1-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為チェック用編集２処理（検査）
      *****************************************************************
       32012-SRYACT-HEN2-SEC           SECTION.
      *
      *    点滴
           IF      SRY-SRYKBN          =   "33"
               IF     (IDX2            =   1     ) AND
                      (SRY-SRYCD (IDX2)(1:1)   =   "1" )
                   MOVE    SRY-SRYCD (IDX2)    TO  SPA-ACCT2-SRYCD
                   MOVE    SRY-ZAINUM          TO  SPA-ACCT2-ZAINUM
               END-IF
      *        点数マスタより編集
               IF      SRY-SRYCD(IDX2)(1:1)    =   "6"
                   MOVE    SRY-SRYCD (IDX2)    TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
                   COMPUTE SPA-ACCT2-CSYYOURYO =   SPA-ACCT2-CSYYOURYO
                                               +  (TNS-CSYYOURYO
                                               *   SRY-SRYSURYO (IDX2))
               END-IF
           END-IF
      *
      *    ADD     1                   TO  IDX-SIN2
      *    IF      IDX-SIN             >   100
      *        MOVE    100             TO  IDX-SIN2
      *    END-IF
      *    MOVE    ACCT-SRYKBN         TO  SPA-ACCT2-SRYKBN
      *                                               (IDX-SIN2)
      *    MOVE    SRY-SRYCD (IDX2)    TO  SPA-ACCT2-SRYCD
      *                                                (IDX-SIN2)
      *    点数マスタより編集
      *    MOVE    SRY-SRYCD (IDX2)    TO  TNS-SRYCD
      *    PERFORM 910-TENSU-READ-SEC
      *    MOVE    TNS-GAIKANRIKBN     TO  SPA-ACCT2-GAIKANRIKBN
      ****                                             (IDX-SIN2)
           .
       32012-SRYACT-HEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌薬剤チェック用編集処理
      *****************************************************************
       3202-KNKYAKSET-SEC              SECTION.
      *
      *    相互作用チェック期間
           IF      SPA-INTERACT-KIKAN  =   ZERO
               INITIALIZE                  SPA-KNKYAK-AREA
               GO      TO      3202-KNKYAKSET-EXT
           END-IF
      *
           MOVE    ZERO                TO  IDX
      *    対象年月−１月を求める
           MOVE    SPA-SRYYMD(1:6)     TO  WRK-SRYYM
           MOVE    SPA-INTERACT-KIKAN  TO  WRK-KIKAN
           PERFORM UNTIL     WRK-KIKAN     <   12
               COMPUTE WRK-SRYYY           =   WRK-SRYYY     -   1
               COMPUTE WRK-KIKAN           =   WRK-KIKAN
                                           -   12
           END-PERFORM
           IF      WRK-SRYMM           >   WRK-KIKAN
               COMPUTE WRK-SRYMM           =   WRK-SRYMM
                                           -   WRK-KIKAN
           ELSE
               COMPUTE WRK-SRYMM           =   WRK-SRYMM
                                           +   12
                                           -   WRK-KIKAN
               COMPUTE WRK-SRYYY           =   WRK-SRYYY
                                           -   1
           END-IF
      **** COMPUTE WRK-SRYMM           =   WRK-SRYMM
      *                                -   WRK-KIKAN
      *
           PERFORM
                   UNTIL   SPA-SRYYMD(1:6)     <   WRK-SRYYM
              PERFORM 3203-KNKYAKSET-HENSYU-SEC
      *
              ADD     1                    TO  WRK-SRYMM
              IF      WRK-SRYMM            >   12
                   ADD     1                   TO  WRK-SRYYY
                   MOVE    1                   TO  WRK-SRYMM
               END-IF
           END-PERFORM
      *
           .
       3202-KNKYAKSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌薬剤チェック用編集処理
      *****************************************************************
       3203-KNKYAKSET-HENSYU-SEC              SECTION.
      *
      *    診療会計マスタよりチェックする
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPID          TO  ACCT-HOSPID
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    WRK-SRYYM           TO  ACCT-SRYYM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SRYACCT-KEY10"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SRYACCT-KEY10"     TO  ORC-DBPATH
               PERFORM 940-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           PERFORM UNTIL     (FLG-SRYACCT     =   1          )
      *        投薬・注射・自費、または、薬剤点数ありを対象とする
               IF      (ACCT-SRYKBN        =   "21"  OR  "22"  OR
                                               "23"  OR  "31"  OR
                                               "32"  OR  "33"  OR
                                               "95"  OR  "96"  ) OR
                       (ACCT-YKZTEN        >   ZERO    )
                   PERFORM 32021-KNK-KOUISET-SEC
               END-IF
      *
               MOVE    "SRYACCT-KEY10"      TO  ORC-DBPATH
               PERFORM 940-SRYACCT-READ-SEC
           END-PERFORM
           MOVE    "SRYACCT-KEY10"     TO  ORC-DBPATH
           PERFORM 990-DB-CLOSE-SEC
      *
           .
       3203-KNKYAKSET-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌薬剤編集処理
      *****************************************************************
       32021-KNK-KOUISET-SEC              SECTION.
      *
      *    最終投薬日
           MOVE    ZERO                TO  WRK-TOUYMD
           MOVE    ACCT-SRYYM          TO  WRK-TOUYMD(1:6)
           PERFORM VARYING     IDX-Y2  FROM    31  BY  -1
                   UNTIL       IDX-Y2  <   1
               IF      ACCT-DAY (1 IDX-Y2) >   ZERO
                   MOVE    IDX-Y2          TO  WRK-TOUDD
                   MOVE    1               TO  IDX-Y2
               END-IF
           END-PERFORM
      *
           IF      WRK-TOUDD           =   ZERO
               GO      TO      32021-KNK-KOUISET-EXT
           END-IF
      *
      *    診療会計マスターから診療行為マスターを検索する
           MOVE    SPACE               TO  SRYACT-REC
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPID         TO  SRY-HOSPID
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
               PERFORM 900-SRYACT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL      FLG-SRYACT   =   1
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   5   )  OR
                                  (SRY-SRYCD(IDX2)     =   SPACE )
               IF      SRY-SRYCD (IDX2)(1:1)   =   "6"
                   PERFORM VARYING     IDX3    FROM    1   BY  1
                           UNTIL       IDX3        >   100
                       IF      SPA-KNK-SRYCD (IDX3)  =   SPACE
                           MOVE    SRY-SRYCD (IDX2)  TO  SPA-KNK-SRYCD
                                                             (IDX3)
                       END-IF
                       IF      SPA-KNK-SRYCD (IDX3)
                                                   =  SRY-SRYCD (IDX2)
      *                    最終投薬日のセット
                           IF      SPA-KNK-TOUYMD(IDX3)    <  WRK-TOUYMD
                               MOVE    WRK-TOUYMD    TO  SPA-KNK-TOUYMD
                                                              (IDX3)
                           END-IF
      *
                           MOVE    100             TO  IDX3
                       END-IF
                   END-PERFORM
               END-IF
      *
               END-PERFORM
      *
               MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
               PERFORM 900-SRYACT-NEXT-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       32021-KNK-KOUISET-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診料自動発生処理
      *****************************************************************
       330-SYOSIN-SET-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-OK3
      *    初診料が算定できる時
           IF       SPA-SYOSIN         =   1
               CONTINUE
           ELSE
               MOVE    1                   TO  FLG-OK3
           END-IF
      *
      *    入院年月であること
           IF      SPA-K02N-NYUINYMD(1:6)  NOT =   SPA-SRYYMD(1:6)
               MOVE    1                   TO  FLG-OK3
           END-IF
      *
      *    初診料がないこと
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-MAX ) OR
                              (FLG-OK3     =   1       )
               IF      SPA-COM-SRYKBN (IDX)    =   "11"
                   MOVE    1               TO  FLG-OK3
               END-IF
           END-PERFORM
           IF      FLG-OK3             =   1
               MOVE    ZERO                TO  IDX2
           ELSE
      *        初診料自動発生
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
               INITIALIZE                      ORCSC10SAREA
               MOVE    "1"                 TO  LNK-SC10S-SYORI
               CALL    "ORCSC10S"          USING
                                           ORCSC10SAREA
                                           SPA-AREA
                                           SPA-SCR-REC
                                           SPA-K02
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
               MOVE    LNK-SC10S-IDX2      TO  IDX2
           END-IF
      *
      *    入院日であること
           IF     (SPA-K02N-NYUINYMD   NOT =   SPA-SRYYMD )  AND
                  (IDX2                >   ZERO           )
               ADD     1                   TO  IDX2
               MOVE    IDX2                TO  SPA-GMN-IDX
      *        行挿入処理
               PERFORM 41014-GYOINSN-SEC
               MOVE    SPA-K02N-NYUINYMD(7:2)  TO  WRK-SDD
               MOVE    "*/"                TO  SPA-GMN-INPUTCD (IDX2)
               MOVE    WRK-SDD             TO  SPA-GMN-INPUTCD (IDX2)
                                                               (3:2)
           END-IF
      *
           .
       330-SYOSIN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        Ｋ０１へ
               WHEN    "CLICKED"       ALSO    "B01S"
                   PERFORM 4001-K01-SYORI-SEC
      *        患者取消
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 222-PTNUMCLEAR-SEC
      *        途中終了
               WHEN    "CLICKED"       ALSO    "B12S"
                   PERFORM 4110-KYOSEIEND-SEC
      *        途中終了画面へ（Ｋ１０）
               WHEN    "CLICKED"       ALSO    "B12CS"
                   PERFORM 4110S-K10-SYORI-SEC
      *        セット登録（Ｋ０５）
               WHEN    "CLICKED"       ALSO    "B03S"
                   PERFORM 420-KOUISET-SEC
      *        前回の患者番号
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 230-MAEKPTNUM-SEC
      *        算定履歴更新（Ｋ０６）
               WHEN    "CLICKED"       ALSO    "B10S"
                   PERFORM 430-SANTEIRRK-SEC
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02S"
                   PERFORM 220-CLEAR-SEC
      *        患者登録へ（Ｐ０２）
               WHEN    "CLICKED"       ALSO    "B05S"
                   PERFORM 440-KNJADD-SEC
      *        病名登録　（Ｃ０２）
               WHEN    "CLICKED"       ALSO    "B07S"
                   PERFORM 440-BYOTOROKU-SEC
      *        会計カード（Ｊ０２）
               WHEN    "CLICKED"       ALSO    "B09S"
                   PERFORM 450-KAIKEI-SEC
      *        収納登録（Ｓ０２）
               WHEN    "CLICKED"       ALSO    "B08S"
                   PERFORM 460-SYUNOU-SEC
      *D       前カーソル
      *D       WHEN    "CLICKED"       ALSO    "B06S"
      *D           PERFORM 450-MAECUR-SEC
      *        前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 450-MAEGMN-SEC
      *        次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 460-ATOGMN-SEC
      *        ＤＯ（Ｋ０９）
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 480-DOSYORI-SEC
      *****    算定（Ｋ０４）
      *        WHEN    "CLICKED"       ALSO    "B23"
      ********     PERFORM 470-SANTEI-SEC
      *        訂正処理
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 4101-TEISEI-SEC
      *        訂正処理（トグルボタン）
               WHEN    "CLICKED"       ALSO    "TEISEI"
                   PERFORM 4101-TEISEI-SEC
      *        氏名検索（Ｐ９７）
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 470-SIMEIKEN-SEC
      *        予約登録へ（Ｙ０１）
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 480-YOYAKU-SEC
      *        受付一覧へ（Ｕ０１）
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-UKEICHI-SEC
      *        受付画面へ（Ｕ０２）
               WHEN    "CLICKED"       ALSO    "B04S"
                   PERFORM 4102-UKETUKE-SEC
      **       登録（Ｋ０８）
               WHEN    "CLICKED"       ALSO    "B12"
                   MOVE    SPACE           TO  WRK-PUTTYPE
                   PERFORM 4100-TOROKU-SEC
      *        入力コード設定へ
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 4106S-INPUTCD-SYORI-SEC
      *        入退院登録へ
               WHEN    "CLICKED"       ALSO    "B06S"
                   PERFORM 4106S-NYUTAIIN-SEC
      *        入院会計照会へ
               WHEN    "CLICKED"       ALSO    "B11S"
                   PERFORM 4111S-NYUKAIKEI-SEC
      *        ヘルプ
               WHEN    "CLICKED"       ALSO    "BHELP"
                   PERFORM 4199-HELP-SYORI-SEC
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
      *    入力項目の決定
           IF      MCP-EVENT           =   "ACTIVATE"   OR
                                           "ACTIVATE2"
               IF     (MCP-WIDGET(1:07)    =   "INPUTCD"  )  OR
                      (MCP-WIDGET(1:6)     =   "MEISYO"   )
                   PERFORM 2001-INPUT-SET-SEC
                   IF      SPA-ERRCD   NOT =   SPACE
                       GO      TO      200-ENTRY-EXT
                   END-IF 
                   IF      WRK-IDX2        =   ZERO
                       GO      TO      200-ENTRY-EXT
                   END-IF 
               ELSE
                   MOVE    MCP-WIDGET          TO  WRK-WIDGET
               END-IF
           ELSE
               MOVE    MCP-WIDGET          TO  WRK-WIDGET
           END-IF
      *
           EVALUATE    MCP-EVENT       ALSO    WRK-WIDGET
      *        患者番号入力
               WHEN    "ACTIVATE"      ALSO    "PTNUM"
                   PERFORM 41010-PTNUM-SYORI-SEC
      *        保険番号入力
               WHEN    "ACTIVATE"      ALSO    "HKNCOMBI"
                   PERFORM 41012-HKNCOMBI-SYORI-SEC
      *        診療日入力
               WHEN    "ACTIVATE"      ALSO    "SRYYMD"
                   PERFORM 41017-SRYYMD-SYORI-SEC
      *        診療科入力
               WHEN    "ACTIVATE"      ALSO    "SRYKA"
                   PERFORM 41013-SRYKAI-SYORI-SEC
      *        時間外入力
               WHEN    "ACTIVATE"      ALSO    "JIKANKBN"
                   PERFORM 41019-JIKANKBN-SYORI-SEC
      *        入力コード
               WHEN    "ACTIVATE"      ALSO    "INPUTCD"
                   PERFORM 41011-KOUI-ALLHEN-SEC
      *        入力コード変換
      ******** WHEN    "ACTIVATE2"     ALSO    "INPUTCD"
      *******      PERFORM 41014-INPUTCD-MODHEN-SEC
      *        名称
               WHEN    "ACTIVATE"      ALSO    "MEISYO"
                    PERFORM 41011-KOUI-ALLHEN-SEC
      *        ＤＯ選択
               WHEN    "ACTIVATE"      ALSO    "DOSELNUM"
                   PERFORM 41014-DOSELNUM-SEC
      *        診療日選択
               WHEN    ANY             ALSO    "RRKLIST"
                   PERFORM 41015-RRKLIST-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目の決定
      *****************************************************************
       2001-INPUT-SET-SEC               SECTION.
      *
           MOVE    SPACE               TO  FLG-TOROKU
      *    入力行NOの設定
           MOVE    ZERO                TO  WRK-IDX2
           MOVE    SPACE               TO  WRK-WIDGET
           IF      MCP-WIDGET(1:07)    =   "INPUTCD"
               MOVE    MCP-WIDGET(8:2)     TO  WRK-IDX2
               MOVE    MCP-WIDGET(1:7)     TO  WRK-WIDGET
           END-IF
           IF      MCP-WIDGET(1:6)     =   "MEISYO"
               MOVE    MCP-WIDGET(7:2)     TO  WRK-IDX2
               MOVE    MCP-WIDGET(1:6)     TO  WRK-WIDGET
           END-IF
      *
           MOVE    ZERO                TO  SPA-GMN-IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   WRK-GYO-MAX
               IF     (SPA-COM-PAGE (IDX)  =   SPA-GMN-PAGE )  AND
                      (SPA-COM-GYOU (IDX)  =   WRK-IDX2     )
                   MOVE    IDX                 TO  SPA-GMN-IDX
                   MOVE    WRK-GYO-MAX         TO  IDX
               END-IF
           END-PERFORM
      *
      *    同一行がない時、計算で求める
           IF      SPA-GMN-IDX         =   ZERO
               COMPUTE SPA-GMN-IDX     =   (SPA-GMN-PAGE   -   1)
                                       *   24
                                       +   WRK-IDX2
      *
               IF      SPA-GMN-IDX         >   WRK-GYO-MAX
                   MOVE    "7001"          TO  SPA-ERRCD
                   MOVE    WRK-GYO-MAX     TO  SPA-GMN-IDX
                   GO      TO      2001-INPUT-SET-EXT
               END-IF
           END-IF
      *
           .
       2001-INPUT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号入力処理
      *****************************************************************
       41010-PTNUM-SYORI-SEC           SECTION.
      *
           IF      SPA-PTID        NOT =   ZERO
      *        ワーク診療行為クリア
               PERFORM 4900-WKSRYACT-CLEAR-SEC
           END-IF
           MOVE    ZERO                TO  SPA-GMN-CUR
           MOVE    1                   TO  FLG-GMN-INIT
      *    ＳＰＡクリア処理
           INITIALIZE                  SPA-SCR-REC
           INITIALIZE                  SPA-K02
           INITIALIZE                  SPA-K01SET-AREA
           INITIALIZE                  SPA-SYORIFLG
           INITIALIZE                  SPA-DENPNUM
           INITIALIZE                  SPA-CONTFLG
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPACE               TO  SPA-SRYYMDW(10:)
           MOVE    SPA-SRYYMDW         TO  SPA-GMN-SRYYMD
           MOVE    SPA-SRYYMD          TO  SPA-NAI-SRYYMD
      *    受付一覧からの時、ドクターを編集する
           IF      WRK-MOTOPG          =   "U01"
               MOVE    WRK-SPA-DRCD    TO  SPA-DRCD
           END-IF
      *
           MOVE    K02N-PTNUM          TO  SPA-GMN-PTNUM
      *
      *    ’Ｇ’で外来へ移動する
           IF     (SPA-GMN-PTNUM(1:1)  =   "G"  OR  "g" )  AND
                  (SPA-GMN-PTNUM(2:)   =   SPACE        )
               INITIALIZE                  SPA-KYOTUKEY
               MOVE    SPACE               TO  WRK-PUTTYPE
               PERFORM 410109-K02IDOU-SEC
               GO  TO                  41010-PTNUM-SYORI-EXT
           END-IF
      *
           INITIALIZE                      ORCSPTNUMAREA
           MOVE    SPA-GMN-PTNUM       TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
      *
      *    該当患者なし
           IF      SPTNUM-RC           =   10  OR  99
               MOVE    "1003"              TO  SPA-ERRCD
               GO  TO                  41010-PTNUM-SYORI-EXT
           END-IF
      *    氏名検索へ
           IF      SPTNUM-RC           =   20
      *
               MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
               MOVE    SPACE               TO  LINKAREA
               MOVE    SPA-KYOUTU          TO  LNK-COMMON
               MOVE    SPA-K02             TO  LNK-SCRSPA
      *        画面移行用のパラメタ変更
               MOVE    "K02N"              TO  SPA-MOTOPG
               MOVE    "P97"               TO  SPA-SAKIPG
      *        氏名編集
               MOVE    SPA-GMN-PTNUM       TO  SPA-NAME
               MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
               MOVE    "CHANGE"            TO  MCP-PUTTYPE
               MOVE    "P97"               TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
               GO  TO                  41010-PTNUM-SYORI-EXT
           END-IF
      *    患者番号決定
           MOVE    SPTNUM-PTID         TO  SPA-PTID
           MOVE    SPTNUM-PTNUM        TO  SPA-PTNUM
           MOVE    SPA-PTID            TO  SPA-GMN-PTID
           MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
      *
      *    患者マスター存在チェック
           MOVE    SPA-HOSPID          TO  PTINF-HOSPID
           MOVE    SPA-GMN-PTID        TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF       NOT =   ZERO
               MOVE    "1003"              TO  SPA-ERRCD
               GO      TO      41010-PTNUM-SYORI-EXT
           END-IF
      *
      *    排他制御処理
           PERFORM 990-LOCK-IN-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41010-PTNUM-SYORI-EXT
           END-IF
      *
      *    入院が存在するかのチェック
           PERFORM 410101-NYUINRRK-CHK-SEC
      *    入外区分
           IF      SPA-NYUGAIKBN       =   "2"
      *        外来の時、外来処理へ
               MOVE    SPA-GMN-PTID        TO  SPA-PTID
               MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
               INITIALIZE                  SPA-K01KYOUTU
               INITIALIZE                  SPA-K02
               MOVE    SPACE               TO  WRK-PUTTYPE
               PERFORM 410109-K02IDOU-SEC
               GO      TO      41010-PTNUM-SYORI-EXT
           END-IF
           IF     (SPA-KIDCD       NOT =   SPACE ) OR
                  (SPA-ERRCD       NOT =   SPACE )
               GO      TO      41010-PTNUM-SYORI-EXT
           END-IF
      *    受付一覧からの時、診療科を編集する
           IF      WRK-MOTOPG          =   "U01"
               MOVE    WRK-SPA-SRYKA   TO  SPA-SRYKA
               MOVE    WRK-SPA-SRYKA   TO  SPA-GMN-SRYKA
           END-IF
      *
      *    初期ＳＰＡ編集処理
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
           MOVE    "9"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
      *    入退院日
           MOVE    WRK-NYUINYMD        TO  SPA-K02N-NYUINYMD
           MOVE    WRK-TAIINYMD        TO  SPA-K02N-TAIINYMD
      *
           MOVE    "5"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *    入退院日
           MOVE    WRK-NYUINYMD        TO  SPA-K02N-NYUINYMD
           MOVE    WRK-TAIINYMD        TO  SPA-K02N-TAIINYMD
      *
      *    スパ共通編集処理
           PERFORM                     310-SPA-SET-SEC
           IF      SPA-ERRCD       NOT =   SPACE
                GO      TO      41010-PTNUM-SYORI-EXT
           END-IF
      *
      *    チェック用月内診療行為セット
           PERFORM                 320-SPA-CHKSET-SEC
      *
      *    保険番号が未確定の時、エラー
           IF     (SPA-HKNCOMBINUM     =   SPACE )  OR
                  (SPA-GMN-HKNCOMBINUM =   SPACE )
               MOVE    "1006"          TO  SPA-ERRCD
               MOVE    3               TO  SPA-HKN-ERR
               GO      TO      41010-PTNUM-SYORI-EXT
           END-IF
      *
      *    パラメタファイルより表示
           MOVE    3               TO  FLG-PARASET
           PERFORM                 3002-PARA-SET-SEC
      *    画面用スパ編集処理
           MOVE    ZERO            TO  FLG-TEISEI
           PERFORM                 310-SPA-GMNSET-SEC
      *
           MOVE    1               TO  SPA-GMN-PAGE
           MOVE    1               TO  SPA-GMN-CUR
           MOVE    1               TO  SPA-MAP-IDX
      *
           .
       41010-PTNUM-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院存在チェック処理
      *****************************************************************
       410101-NYUINRRK-CHK-SEC         SECTION.
      *
      *    入退院日
           MOVE    SPACE               TO  WRK-NYUINYMD
           MOVE    SPACE               TO  WRK-TAIINYMD
      *    入院履歴判定
           MOVE    SPACE               TO  PTNYUINRRK-REC
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPID          TO  PTNYUINRRK-HOSPID
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTNYUINRRK-KEY34"  TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PTNYUINRRK-KEY34"  TO  ORC-DBPATH
               PERFORM 910-PTNYUINRRK-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *    入院が１件もないとき、外来とする
           IF      FLG-PTNYUINRRK      =   1
               MOVE    "2"                 TO  SPA-NYUGAIKBN
           ELSE
               MOVE    "1"                 TO  SPA-NYUGAIKBN
           END-IF
      *
           INITIALIZE                  HZN-PTNYUINRRK-REC
           INITIALIZE                  SPA-K02N-AREA
           MOVE    SPACE               TO  SPA-SRYKA
           MOVE    SPACE               TO  SPA-HKNCOMBINUM
           MOVE    SPACE               TO  SPA-DRCD
           MOVE    ZERO                TO  FLG-OK
           PERFORM
                   UNTIL      (FLG-PTNYUINRRK      =   1 )  OR
                              (FLG-OK              =   1 )
      *        転入日
               IF      PTNYUINRRK-TENNYUYMD    <=  SPA-SRYYMD
      *            転出日
                   IF     (PTNYUINRRK-TENSTUYMD    =   SPACE )  OR
                          (PTNYUINRRK-TENSTUYMD    NOT =   SPACE AND
                           PTNYUINRRK-TENSTUYMD    >=  SPA-SRYYMD)
      *                入院対象処理
                       IF     (PTNYUINRRK-TAIINYMD     =   SPA-SRYYMD)
                         OR   (PTNYUINRRK-TENSTUYMD    >   SPA-SRYYMD)
                           MOVE    1               TO  FLG-OK
                           PERFORM 4101011-PTNYUINRRK-HEN-SEC
                       END-IF
                   ELSE
                       MOVE    PTNYUINRRK-REC  TO  HZN-PTNYUINRRK-REC
                   END-IF
               END-IF
      *
               MOVE    "PTNYUINRRK-KEY34"   TO  ORC-DBPATH
               PERFORM 910-PTNYUINRRK-READ-SEC
           END-PERFORM
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "PTNYUINRRK-KEY34"  TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      FLG-OK              =   ZERO
               IF     (HZN-PTNYUINRRK-NYUINYMD     =   SPACE )  OR
                      (HZN-PTNYUINRRK-TAIINYMD NOT =   SPACE   AND
                       HZN-PTNYUINRRK-TAIINYMD     >   SPA-SRYYMD)
                   MOVE    "2"                 TO  SPA-NYUGAIKBN
               ELSE
                   MOVE    HZN-PTNYUINRRK-TAIINYMD
                                           TO  SPA-K02N-TAIINYMD
                   MOVE    "0120"          TO  SPA-KIDCD
                   MOVE    07              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *    入退院日
           MOVE    SPA-K02N-NYUINYMD   TO  WRK-NYUINYMD
           MOVE    SPA-K02N-TAIINYMD   TO  WRK-TAIINYMD
      *
           .
       410101-NYUINRRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴ＳＰＡ編集処理
      *****************************************************************
       4101011-PTNYUINRRK-HEN-SEC      SECTION.
      *
      *    入院科
           MOVE    PTNYUINRRK-NYUINKA  TO  SPA-K02N-NYUINKA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   SPA-SRYKA-MAX
               IF      SPA-K02N-NYUINKA    =   SPA-GMN-SRYKAL   (IDX)
                   MOVE    SPA-GMN-SRYKAMEIL(IDX)
                                               TO  SPA-K02N-NYUINKAMEI
                   MOVE    SPA-SRYKA-MAX       TO  IDX
               END-IF
           END-PERFORM
           MOVE    PTNYUINRRK-NYUINKA  TO  SPA-SRYKA
      *    保険組合せ番号
           MOVE    PTNYUINRRK-HKNCOMBINUM
                                       TO  SPA-HKNCOMBINUM
      *    入院日
           MOVE    PTNYUINRRK-NYUINYMD TO  SPA-K02N-NYUINYMD
           MOVE    SPA-K02N-NYUINYMD   TO  WRK-SYMD
           PERFORM 520-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-K02N-NYUINYMDW
      *    退院日
           MOVE    PTNYUINRRK-TAIINYMD TO  SPA-K02N-TAIINYMD
           IF      PTNYUINRRK-TAIINYMD =   SPACE
               MOVE    ALL "9"             TO  SPA-K02N-TAIINYMD
               MOVE    "-"                 TO  SPA-K02N-H1
               MOVE    SPACE               TO  SPA-K02N-TAIINYMDW
           ELSE
               MOVE    "-"                 TO  SPA-K02N-H1
               MOVE    SPA-K02N-TAIINYMD   TO  WRK-SYMD
               PERFORM 520-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-K02N-TAIINYMDW
           END-IF
      *    担当医１（主治医）
           MOVE    PTNYUINRRK-DRCD1    TO  SPA-DRCD
           .
       4101011-PTNYUINRRK-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来診療行為（Ｋ０２）遷移処理
      *****************************************************************
       410109-K02IDOU-SEC         SECTION.
      *
           MOVE    "K02"               TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           IF      WRK-PUTTYPE     NOT =   SPACE
               MOVE    WRK-PUTTYPE         TO  MCP-PUTTYPE
           ELSE
               MOVE    "CHANGE"            TO  MCP-PUTTYPE
           END-IF
           MOVE    "K02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       410109-K02IDOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日入力処理
      *****************************************************************
       41017-SRYYMD-SYORI-SEC         SECTION.
      *
           MOVE    7                   TO  SPA-GMN-CUR
      *
      *    訂正の時、入力エラー
           IF      SPA-SYORIFLG        =   1
               IF      K02N-SRYYMD     NOT =   SPA-GMN-SRYYMD
                   MOVE    1                   TO  SPA-GMN-CUR
                   MOVE    "1009"              TO  SPA-ERRCD
               END-IF
               GO      TO      41017-SRYYMD-SYORI-EXT
           END-IF
      *
           MOVE    K02N-SRYYMD         TO  SPA-GMN-SRYYMD
      *    空白の時、システム日付
           IF      SPA-GMN-SRYYMD      =   SPACE
               MOVE    SPA-SYSYMDWH        TO  SPA-GMN-SRYYMD
           END-IF
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN-SRYYMD      TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-GMN-SRYYMD
      *        患者の確定してない時
               IF      SPA-PTID            =   ZERO
                    MOVE    SGDAY-SDAY          TO  SPA-NAI-SRYYMD
                    MOVE    ZERO                TO  SPA-GMN-CUR
                    GO      TO      41017-SRYYMD-SYORI-EXT
               END-IF
      *        誕生日以前の時、エラー
               IF      SGDAY-SDAY          <   SPA-BIRTHDAY
                    MOVE    "1011"         TO  SPA-ERRCD
               END-IF
      *        未来日は確認メッセージ
               IF     (SGDAY-SDAY          >   SPA-SYSYMD    )
                 AND  (SPA-ERRCD           =   SPACE         )
                   MOVE    "0111"              TO  SPA-KIDCD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI-SRYYMD
                   MOVE    ZERO                TO  SPA-HKN-ERR
                   MOVE    ZERO                TO  SPA-RJN-ERR
               END-IF
      *        変更のない時、以降の処理はそのまま
               IF     (SGDAY-SDAY          =   SPA-NAI-SRYYMD)
                 AND  (SPA-ERRCD           =   SPACE         )
                 AND  (SPA-KIDCD           =   SPACE         )
                    MOVE    1                  TO  SPA-GMN-CUR
                    GO      TO      41017-SRYYMD-SYORI-EXT
               END-IF
               MOVE    SGDAY-SDAY          TO  SPA-NAI-SRYYMD
           ELSE
               MOVE    "1010"          TO  SPA-ERRCD
           END-IF
      *
      *    診療日確定後、編集
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-KIDCD           =   SPACE )
               PERFORM 410171-SRYYMD-OK-SEC
           END-IF
      *
           .
       41017-SRYYMD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日確定後、編集処理
      *****************************************************************
       410171-SRYYMD-OK-SEC         SECTION.
      *
      *    初期ＳＰＡ編集処理
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-NAI-SRYYMD      TO  LNK-SC95-SRYYMD
           MOVE    SPA-GMN-SRYYMD      TO  LNK-SC95-SRYYMDG
           INITIALIZE                  SPA-DENPNUM
           INITIALIZE                  SPA-SYORIFLG
           INITIALIZE                  SPA-K02
           INITIALIZE                  SPA-SCR-REC
           MOVE    SPA-PTID            TO  SPA-GMN-PTID
           MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
      *
           MOVE    "9"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
      *    入院が存在するかのチェック
           MOVE    SPA-SRYYMD          TO  SPA-NAI-SRYYMD
           MOVE    SPA-SRYYMDW         TO  SPA-GMN-SRYYMD
           PERFORM 410101-NYUINRRK-CHK-SEC
           IF      SPA-NYUGAIKBN       =   "2"
               MOVE    "1"             TO  SPA-NYUGAIKBN
      ******** MOVE    "9001"          TO  SPA-ERRCD
               MOVE    "0120"          TO  SPA-KIDCD
               MOVE    07              TO  SPA-GMN-CUR
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO      410171-SRYYMD-OK-EXT
           END-IF
      *
           MOVE    "5"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
      *    スパ共通編集処理
           PERFORM                 310-SPA-SET-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410171-SRYYMD-OK-EXT
           END-IF
      *
      *    チェック用月内診療行為セット
           PERFORM                 320-SPA-CHKSET-SEC
      *    パラメタファイルより表示
           MOVE    3               TO  FLG-PARASET
           PERFORM                 3002-PARA-SET-SEC
      *
      *    初診料自動発生
           PERFORM 310-SPA-GMNSET-SEC
      *
           MOVE    1               TO  SPA-GMN-PAGE
           MOVE    1               TO  SPA-GMN-CUR
           MOVE    1               TO  SPA-MAP-IDX
           .
       410171-SRYYMD-OK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号入力処理
      *****************************************************************
       41012-HKNCOMBI-SYORI-SEC         SECTION.
      *
           MOVE    5                   TO  SPA-GMN-CUR
      *
      *    保険番号入力
           IF    ( K02N-HKNCOMBI       =   SPA-GMN-HKNCOMBI-G)  OR
                 ( K02N-PTNUM          =   SPACE             )
               MOVE    1               TO  SPA-GMN-PAGE
               MOVE    1               TO  SPA-GMN-CUR
               GO      TO      41012-HKNCOMBI-SYORI-EXT
           END-IF
      *
           IF      SPA-SYORIFLG        =   1
      *        訂正で、継続確定の時、変更不可
               IF      (SPA-CONTKBN        =   "1"   OR  "2")  AND
                       (SPA-DENPJTIKBN     =   "1"   )
                   MOVE    "1013"              TO  SPA-ERRCD
                   GO      TO      41012-HKNCOMBI-SYORI-EXT
               END-IF
      *
               MOVE    SPA-GMN-HKNCOMBI-G  TO  SPA-MAE-HKNCOMBI-G
               MOVE    SPA-HKNKBN          TO  SPA-MAE-HKNKBN
               MOVE    K02N-HKNCOMBI       TO  SPA-GMN-HKNCOMBI-G
      *        包括まとめの変更不可
               IF      (SPA-GMN-HKNCOMBINUM     =   "9999" )  OR
                       (SPA-MAE-HKNCOMBINUM     =   "9999" )
                   MOVE    "1033"              TO  SPA-ERRCD
                   MOVE    SPA-MAE-HKNCOMBI-G  TO  SPA-GMN-HKNCOMBI-G
                   MOVE    SPA-MAE-HKNKBN      TO  SPA-HKNKBN
                   GO      TO      41012-HKNCOMBI-SYORI-EXT
               END-IF
               MOVE    1                   TO  FLG-CHK2
               PERFORM 41012-HKNCOMBI-CHANGE-SEC
               IF     (SPA-ERRCD           =   SPACE )  AND
                      (SPA-KIDCD           =   SPACE )
      *            訂正の時、確認メッセージ表示
                   MOVE    "0105"              TO  SPA-KIDCD
               END-IF
           ELSE
               MOVE    SPA-GMN-HKNCOMBI-G  TO  SPA-MAE-HKNCOMBI-G
               MOVE    K02N-HKNCOMBI       TO  SPA-GMN-HKNCOMBI-G
               MOVE    SPA-HKNKBN          TO  SPA-MAE-HKNKBN
               MOVE    ZERO                TO  FLG-CHK2
               PERFORM 41012-HKNCOMBI-CHANGE-SEC
           END-IF
           .
       41012-HKNCOMBI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号変更入力処理
      *****************************************************************
       41012-HKNCOMBI-CHANGE-SEC         SECTION.
      *
           MOVE    SPA-NAI-ROUJIN      TO  WRK-ROUJIN
      *    保険組合
           PERFORM 3101-HKNCOMBI-HEN-SEC
           IF      SPA-ERRCD           NOT =   SPACE
               GO      TO      41012-HKNCOMBI-CHANGE-EXT
           END-IF
      *
      *    老人区分が変更された時、チェックフラグをクリアする
           IF     (SPA-NAI-ROUJIN  NOT =   WRK-ROUJIN )  OR
                  (SPA-HKNKBN      NOT =   SPA-MAE-HKNKBN)
               PERFORM 410121-ROUJINCHK-SEC
           END-IF
      *
      *    保険区分が変更された時、ワーク診療行為を判定する
           PERFORM 410121-HKNCOMBI-SAI-SEC
      *
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       41012-HKNCOMBI-CHANGE-EXT.
           EXIT.
      *****************************************************************
      *    チェックフラグクリア処理
      *****************************************************************
       410121-ROUJINCHK-SEC         SECTION.
      *
      *    老人区分が変更された時、チェックフラグをクリアする
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   200
               MOVE    SPACE               TO  SPA-COM-CHKFLG (IDX)
           END-PERFORM
           .
       410121-ROUJINCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険区分変換処理
      *****************************************************************
       410121-HKNCOMBI-SAI-SEC         SECTION.
      *
      *    受診回数編集処理
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSC10SAREA
           MOVE    "3"                 TO  LNK-SC10S-SYORI
           CALL    "ORCSC10S"          USING
                                       ORCSC10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           MOVE    SPA-WKSRY-HKNCOMBI  TO  WRK-WKSRY-HKNCOMBI
      *    初期ＳＰＡ編集処理
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-NAI-SRYYMD      TO  LNK-SC95-SRYYMD
           MOVE    SPA-GMN-SRYYMD      TO  LNK-SC95-SRYYMDG
           MOVE    "7"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *    ワーク診療行為に存在あり
           IF      SPA-WKSRY-HKNCOMBI  NOT =   ZERO
               MOVE    "0118"          TO  SPA-KIDCD
               GO      TO      410121-HKNCOMBI-SAI-EXT
           END-IF
           MOVE    WRK-WKSRY-HKNCOMBI  TO  SPA-WKSRY-HKNCOMBI
      *
      *    保険区分が変更された時、再編集を行う
           IF     (SPA-HKNKBN          =   SPA-MAE-HKNKBN  )  AND
                  (SPA-HKNKBN          =   ZERO            )
             AND  (SPA-MAE-HKNCOMBINUM NOT =   "9999"  )
             AND  (SPA-NAI-ROUJIN      =   WRK-ROUJIN  )
               CONTINUE
           ELSE
      *        診療行為、計計算処理、編集
               MOVE    "2"             TO  FLG-TOROKU
               MOVE    SPACE           TO  FLG-TOROKU
               PERFORM 510-TBLHEN-SEC
               MOVE    SPACE           TO  FLG-TOROKU
           END-IF
     *
           .
       410121-HKNCOMBI-SAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面編集処理
      *****************************************************************
       410132-SAIHENSYU-SEC         SECTION.
      *
      *    ワーク診療行為クリア
           MOVE    1                   TO  FLG-MOD-FLG
           PERFORM 4900-WKSRYACT-CLEAR-SEC
      *
      *    スパ共通編集処理
           PERFORM                     310-SPA-SET-SEC
      *
           INITIALIZE                  SPA-SCR-REC
      *    パラメタファイルより表示
           MOVE    3               TO  FLG-PARASET
           PERFORM                 3002-PARA-SET-SEC
      *
      *    画面用スパ編集処理
           MOVE    ZERO                TO  FLG-TEISEI
           PERFORM                 310-SPA-GMNSET-SEC
      *
           MOVE    1               TO  SPA-GMN-PAGE
           MOVE    1               TO  SPA-GMN-CUR
           MOVE    1               TO  SPA-MAP-IDX
      *
           .
       410132-SAIHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科入力処理
      *****************************************************************
       41013-SRYKAI-SYORI-SEC         SECTION.
      *
           MOVE    6                   TO  SPA-GMN-CUR
      *
      *    診療科変更あり
           IF      K02N-SRYKA          =   SPA-GMN-SRYKA-G
               MOVE    1               TO  SPA-GMN-PAGE
               MOVE    1               TO  SPA-GMN-CUR
               GO      TO      41013-SRYKAI-SYORI-EXT
           END-IF
      *    訂正の時、エラー
           IF      SPA-SYORIFLG        =   1
      *********MOVE    "1008"              TO  SPA-ERRCD
               CONTINUE
           ELSE
               IF      SPA-PTID            =   ZERO
                   MOVE    ZERO                TO  SPA-GMN-CUR
                   MOVE    K02N-SRYKA          TO  SPA-GMN-SRYKA-G
                   GO      TO      41013-SRYKAI-SYORI-EXT
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-SRYKA-G     TO  SPA-MAE-SRYKA-G
           MOVE    K02N-SRYKA          TO  SPA-GMN-SRYKA-G
      *    診療科
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SRYKA-MAX
               IF      SPA-GMN-SRYKA   =   SPA-GMN-SRYKAL (IDX)
                   MOVE    SPA-GMN-SRYKALST (IDX)
                                           TO  SPA-GMN-SRYKA-G
                   MOVE    SPA-SRYKA-MAX   TO  IDX
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
      *    科が無い時、前の科に戻す
           IF      FLG-CHK             =   ZERO
               MOVE    SPA-MAE-SRYKA-G TO  SPA-GMN-SRYKA-G
               GO      TO      41013-SRYKAI-SYORI-EXT
           END-IF
      *
           IF      SPA-SYORIFLG        =   1
               MOVE    "0123"              TO  SPA-KIDCD
           ELSE
               IF      SPA-GMN-MAX         >   ZERO
                   MOVE    "0107"              TO  SPA-KIDCD
               ELSE
                   PERFORM 41013-SRYKA-CHANGE-SEC
               END-IF
           END-IF
      *********PERFORM 41013-SRYKA-CHANGE-SEC
      *****END-IF
           .
       41013-SRYKAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科変換処理
      *****************************************************************
       41013-SRYKA-CHANGE-SEC         SECTION.
      *
           MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
           MOVE    SPA-GMN-SRYKAMEI    TO  SPA-SRYKAMEI
      *
      *    対象の科の保険組合せにする
           IF      SPA-SYORIFLG        =   ZERO
               MOVE    SPACE               TO  SPA-HKNCOMBINUM
               MOVE    SPACE               TO  SPA-DRCD
      *        診療内容を残さない時、展開中のクリア
               IF      SPA-KID1-FLG        =   "OK"
                   CONTINUE
               ELSE
                   MOVE    ZERO            TO  FLG-MOD-FLG
                   PERFORM 4900-WKSRYACT-CLEAR-SEC
               END-IF
           END-IF
      *
           MOVE    SPA-SCR-REC         TO  HZN-SCR-REC
      *    初期ＳＰＡ編集処理
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-NAI-SRYYMD      TO  LNK-SC95-SRYYMD
           MOVE    SPA-GMN-SRYYMD      TO  LNK-SC95-SRYYMDG
           MOVE    "K"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
      *    再表示編集処理
           IF      LNK-SC95-WKSRYACT   =   1
               MOVE    "0122"          TO  SPA-KIDCD
           END-IF
      *
      *
           MOVE    HZN-SCR-REC         TO  SPA-SCR-REC
           IF     (SPA-SYORIFLG        =   ZERO )  AND
                  (SPA-KID1-FLG    NOT =   "OK" )
               INITIALIZE                  SPA-SCR-REC
           END-IF
      *
           MOVE    1               TO  SPA-GMN-PAGE
           MOVE    1               TO  SPA-GMN-CUR
           MOVE    1               TO  SPA-MAP-IDX
           .
       41013-SRYKA-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    時間外入力処理
      *****************************************************************
       41019-JIKANKBN-SYORI-SEC         SECTION.
      *
           MOVE    8                   TO  SPA-GMN-CUR
      *
           MOVE    K02N-JIKANKBN       TO  SPA-K02N-JIKANKBN-G
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   5
               IF      SPA-K02N-JIKANKBN   =   SPA-K02N-JIKANKBN-LST
                                                          (IDX)(1:1)
                   MOVE    SPA-K02N-JIKANKBN-LST (IDX)
                                           TO  SPA-K02N-JIKANKBN-G
                   MOVE    1               TO  FLG-CHK
                   MOVE    5                TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "9002"              TO  SPA-ERRCD
               GO      TO       41019-JIKANKBN-SYORI-EXT
           END-IF
      *    時間外特例の時、システム管理にあること
           IF      SPA-K02N-JIKANKBN   =   "4"
               IF      SPA-JIKANTOKU   NOT =   1
                   MOVE    "0069"          TO  SPA-ERRCD
               END-IF
           END-IF
           MOVE    SPA-K02N-JIKANKBN   TO  SPA-NAI-TIMEFLG
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
      *    画面→ＳＰＡ編集処理
           PERFORM 410110-INPUT-GMNSET-SEC
      *    入力コード・名称チェック処理
           MOVE    ZERO                TO  WRK-IN-MAP-IDX
           MOVE    1                   TO  WRK-STR
           PERFORM 410112-KOUI-SPACHK-SEC
      *
           .
       41019-JIKANKBN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       41011-KOUI-ALLHEN-SEC               SECTION.
      *
      *    患者の入力チェック
           IF      SPA-PTID            =   ZERO
               IF      K02N-INPUTCD(1)     NOT =   SPACE
                   MOVE    00              TO  SPA-GMN-CUR
                   MOVE    "1005"          TO  SPA-ERRCD
                   GO      TO      41011-KOUI-ALLHEN-EXT
                END-IF
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
      *    画面→ＳＰＡ編集処理
           PERFORM 410110-INPUT-GMNSET-SEC
      *
      *    入力コード・名称チェック処理
      *****PERFORM 410112-KOUI-SPACHK-SEC
      *
           IF      FLG-INPUT-NASI      =   1
      *        空白行をなくす
               PERFORM 420-SYOKYO-SEC
      *        カーソル位置
               PERFORM 410119-SAIINPUT-SEC
           ELSE
      *        入力コード・名称チェック処理
               PERFORM 410112-KOUI-SPACHK-SEC
      *
           END-IF
      *
           .
       41011-KOUI-ALLHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面→ＳＰＡ編集処理
      *****************************************************************
       410110-INPUT-GMNSET-SEC               SECTION.
      *
      *    入力コード入力時、カーソルの保存
           IF     (MCP-EVENT           =   "ACTIVATE" ) AND
                  (WRK-WIDGET          =   "INPUTCD"  )
               MOVE    WRK-IDX2            TO  WRK-IN-MAP-IDX
           ELSE
               MOVE    ZERO                TO  WRK-IN-MAP-IDX
           END-IF
      *
           MOVE    1                   TO  FLG-INPUT-NASI
      *
      *    カーソル位置のクリア等
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   WRK-GYO-MAX
               IF      SPA-COM-CUR (IDX)   =   "1"  OR  "2"
                   IF      SPA-GMN-INPUTCD (IDX)   NOT =   SPACE
                       MOVE    ZERO                TO  FLG-INPUT-NASI
                   END-IF
               END-IF
               MOVE    SPACE               TO  SPA-COM-CUR (IDX)
               MOVE    SPACE               TO  SPA-COM-IN  (IDX)
               MOVE    SPACE               TO  SPA-COM-IN2 (IDX)
           END-PERFORM
      *
           MOVE    ZERO                TO  WRK-STR
           MOVE    ZERO                TO  FLG-KUHAKU
           MOVE    ZERO                TO  SPA-GMN-IDX
           PERFORM VARYING     WRK-IDX2    FROM    1   BY  1
                   UNTIL      (WRK-IDX2    >   24   )   OR
                              (SPA-GMN-IDX     >=  WRK-GYO-MAX)
      *        ＳＰＡ内の行決定
               PERFORM 41011-GYOU-SET-SEC
               IF     (K02N-INPUTCD (WRK-IDX2)  NOT =
                                       SPA-GMN-INPUTCD (SPA-GMN-IDX)) OR
                      (K02N-MEISYO  (WRK-IDX2)   NOT =
                                       SPA-GMN-MEISYO-G (SPA-GMN-IDX))
                   MOVE    "1"                 TO  SPA-COM-IN2
                                                           (SPA-GMN-IDX)
                   MOVE    ZERO                TO  FLG-INPUT-NASI
               END-IF
      *
               MOVE    K02N-INPUTCD (WRK-IDX2)  TO  SPA-GMN-INPUTCD
                                                           (SPA-GMN-IDX)
               MOVE    K02N-MEISYO  (WRK-IDX2)  TO  SPA-GMN-MEISYO-G
                                                           (SPA-GMN-IDX)
      *        入力行の保存
               IF     (WRK-IN-MAP-IDX  NOT =   ZERO )  AND
                      (WRK-IDX2            =   WRK-IN-MAP-IDX)
                   MOVE    "1"                 TO  SPA-COM-IN
                                                          (SPA-GMN-IDX)
                   MOVE    "1"                 TO  SPA-COM-IN2
                                                           (SPA-GMN-IDX)
                   IF     (K02N-INPUTCD (WRK-IDX2)  =   SPACE )  AND
                          (SPA-MAE-INPUTCD (SPA-GMN-IDX)
                                                   =   SPACE )  AND
                          (SPA-COM-INPUTCD (SPA-GMN-IDX)
                                                   =   SPACE )  AND
                          (SPA-GMN-IDX             >   SPA-GMN-MAX)
                       MOVE    1                   TO  FLG-KUHAKU
                   END-IF
               END-IF
      *
               IF      WRK-STR             =   ZERO
                   MOVE    SPA-GMN-IDX         TO  WRK-STR
               END-IF
           END-PERFORM
      *
           IF     (SPA-GMN-IDX     >=  WRK-GYO-MAX )  AND
                  (WRK-IDX2        <=  24          )
               PERFORM VARYING     WRK-IDX3    FROM    WRK-IDX2 BY  1
                       UNTIL      (WRK-IDX3    >   24   )   OR
                                  (SPA-ERRCD   NOT =   SPACE )
                   IF      K02N-INPUTCD (WRK-IDX3) NOT =  SPACE
                       MOVE    "7004"          TO  SPA-ERRCD
                   END-IF
               END-PERFORM
           END-IF
           .
       410110-INPUT-GMNSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       41011-GYOU-SET-SEC               SECTION.
      *    行決定
           MOVE    ZERO                TO  SPA-GMN-IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   WRK-GYO-MAX
               IF     (SPA-COM-PAGE (IDX)  =   SPA-GMN-PAGE )  AND
                      (SPA-COM-GYOU (IDX)  =   WRK-IDX2     )
                   MOVE    IDX                 TO  SPA-GMN-IDX
                   MOVE    WRK-GYO-MAX         TO  IDX
               END-IF
           END-PERFORM
      *    同一行がない時、計算で求める
           IF      SPA-GMN-IDX         =   ZERO
               COMPUTE SPA-GMN-IDX     =   (SPA-GMN-PAGE   -   1)
                                       *   24
                                       +   WRK-IDX2
               IF      SPA-GMN-IDX     >   WRK-GYO-MAX
                   MOVE    "7001"          TO  SPA-ERRCD
                   MOVE    WRK-GYO-MAX     TO  SPA-GMN-IDX
                   GO      TO      41011-GYOU-SET-EXT
               END-IF
           END-IF
      *
           .
       41011-GYOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡチェック処理
      *****************************************************************
       410112-KOUI-SPACHK-SEC               SECTION.
      *
           IF      WRK-STR             =   ZERO
               MOVE    1               TO  WRK-STR
           END-IF
      *    変換・削除処理
           MOVE    WRK-STR             TO  SPA-GMN-IDX
           MOVE    ZERO                TO  WRK-IDX2
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    ZERO                TO  FLG-INS
           PERFORM VARYING SPA-GMN-IDX     FROM    WRK-STR  BY  1
                   UNTIL   SPA-GMN-IDX     >   WRK-GYO-MAX
               IF     (SPA-MAE-INPUTCD (SPA-GMN-IDX)  NOT =
                                       SPA-GMN-INPUTCD (SPA-GMN-IDX)) OR
                      (SPA-COM-IN2 (SPA-GMN-IDX)   =   "1"  )
                   PERFORM 41011-KOUI-HENDEL-SEC
               END-IF
           END-PERFORM
           IF      FLG-INS             =   1
      *        空白行をなくす
               PERFORM 420-SYOKYO-SEC
           END-IF
      *
           IF      WRK-ERRCD       NOT =   SPACE
               MOVE    WRK-ERRCD           TO  SPA-ERRCD
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF
      *
      *    入力のチェック
           MOVE    WRK-STR             TO  SPA-GMN-IDX
           MOVE    ZERO                TO  FLG-INS
           MOVE    SPACE               TO  WRK-ERRCD2
           PERFORM
                   UNTIL      (SPA-GMN-IDX     >   WRK-GYO-MAX)  OR
                              (SPA-ERRCD   NOT =   SPACE )  OR
                              (FLG-END     NOT =   ZERO  )
               IF     (SPA-MAE-INPUTCD (SPA-GMN-IDX)  NOT =
                                       SPA-GMN-INPUTCD (SPA-GMN-IDX)) OR
                      (SPA-COM-IN2 (SPA-GMN-IDX)   =   "1"  )
               OR     (SPA-GMN-INPUTCD (SPA-GMN-IDX)   NOT =   SPACE)
                   PERFORM 41011-KOUI-HEN2-SEC
               END-IF
      *        フリーコメントの名称の全角チェックはすべて行う
               IF     (SPA-ERRCD           =   SPACE )  AND
                     ((SPA-COM-INPUTCD(SPA-GMN-IDX)(1:2)
                                           =   "81" OR "83" OR "84") OR
                      (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)
                                           =   "008" )) AND
                      (SPA-GMN-MEISYO-G(SPA-GMN-IDX) 
                                       NOT =   SPACE  )
                   PERFORM 401131-MEISYO-HENKAN-SEC
               END-IF
               IF     (SPA-ERRCD       =   SPACE )  AND
                      (FLG-END         =   ZERO  )
                   ADD     1                   TO  SPA-GMN-IDX
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD           =   "0021"
      *        診療コード入力
               PERFORM 410119-SAIINPUT-SEC
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF
      *
      *    数量ゼロエラー
           IF      WRK-ERRCD2      NOT =   SPACE
               MOVE    WRK-ERRCD2          TO  SPA-ERRCD
           END-IF
      *
      *
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (FLG-END             =   1     )
               IF      FLG-END         =   1
      *            ＳＰＡの内容を画面へ編集する。カーソル設定なし
                   PERFORM 500-GMNHEN-SEC
               END-IF
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF
      *    行挿入、警告あり
           IF     (FLG-INS             =   1     )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF
      *    他画面遷移時
           IF      FLG-HENKOU          =   1
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF
      *
      *    点数計算・剤別チェック処理
           MOVE    SPACE           TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           IF     (SPA-ERRCD       =   SPACE
                                   OR  "0021" )  AND
                  (SPA-KIDCD       =   SPACE  )  AND
                  (FLG-END         =   ZERO   )
      *        診療コード入力
               PERFORM 410119-SAIINPUT-SEC
           END-IF
      *
           .
       410112-KOUI-SPACHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面変換・削除処理
      *****************************************************************
       41011-KOUI-HENDEL-SEC               SECTION.
      *
           INITIALIZE                      WRK-MOJI-AREA
      *
           INITIALIZE                  ORCSC97AREA
      *    ＰＧ名
           MOVE    "K02N"              TO  LNK-SC97-PG
           MOVE    "1"                 TO  LNK-SC97-KBN
           MOVE    SPA-GMN-IDX         TO  LNK-SC97-GMN-IDX
      *    時間外特例区分
           MOVE    SPA-JIKANTOKU       TO  LNK-SC97-JIKANTOKU
           MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                       TO  LNK-SC97-INPUTCD
           CALL    "ORCSC97"           USING
                                       SPA-AREA
                                       ORCSC97AREA
                                       SPA-SCR-REC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    LNK-SC97-INPUTCD
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               MOVE    ZERO            TO  SPA-GMN-INCNT  (SPA-GMN-IDX)
               MOVE    SPACE           TO  SPA-COM-GMNFLG (SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-CUR    (SPA-GMN-IDX)
               IF      WRK-ERRCD           =   SPACE
                   MOVE    SPA-ERRCD           TO  WRK-ERRCD
               END-IF
           END-IF
      *
           EVALUATE    LNK-SC97-INPUTCD(1:1)
           WHEN    SPACE
      *        削除時
      *        セットが削除の時セット内容を削除する
               IF     (SPA-MAE-INPUTCD(SPA-GMN-IDX)(1:1)   =   "S") OR
                       ((SPA-GMN-IDX           <   WRK-GYO-MAX  ) AND
                        (SPA-COM-SET  (SPA-GMN-IDX + 1)    =   "1"))
                   MOVE    SPA-GMN-IDX         TO  IDX
                   MOVE    SPACE       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
                   PERFORM 410151-SETDEL2-SEC
               END-IF
               MOVE    SPACE           TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               MOVE    1               TO  FLG-INS
      *
      *    剤削除時
           WHEN    "-"
               MOVE    LNK-SC97-INPUTCD
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               PERFORM 41015-ZAIDEL-SEC
               MOVE    1               TO  FLG-INS
           END-EVALUATE
      *
           .
       41011-KOUI-HENDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       41011-KOUI-HEN2-SEC               SECTION.
      *
           INITIALIZE                      WRK-MOJI-AREA
      *
           INITIALIZE                  ORCSC97AREA
      *    ＰＧ名
           MOVE    "K02N"              TO  LNK-SC97-PG
           MOVE    SPA-GMN-IDX         TO  LNK-SC97-GMN-IDX
      *    時間外特例区分
           MOVE    SPA-JIKANTOKU       TO  LNK-SC97-JIKANTOKU
           MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                       TO  LNK-SC97-INPUTCD
           CALL    "ORCSC97"           USING
                                       SPA-AREA
                                       ORCSC97AREA
                                       SPA-SCR-REC
      *
           MOVE    LNK-SC97-INPUTCD    TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
           MOVE    LNK-SC97-NYURYOKU   TO  FLG-NYURYOKU
           MOVE    LNK-SC97-INCD       TO  WRK-CD
           MOVE    LNK-SC97-INCDCNT    TO  WRK-CD-MCNT
      *    '_'を削除した時、クリアする
           IF      LNK-SC97-JODOU      >   ZERO
               MOVE    "3"             TO  SPA-COM-IN  (SPA-GMN-IDX)
           END-IF
      *
      *    行挿入
           IF      SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)   =   "+"
               MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:)
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               PERFORM 41014-GYOINSN-SEC
               IF      SPA-ERRCD       =   SPACE
                   MOVE    "3"             TO  SPA-COM-CUR (SPA-GMN-IDX)
                   ADD     1               TO  SPA-GMN-IDX
                   MOVE    1               TO  FLG-INS
               END-IF
               GO  TO                  41011-KOUI-HEN2-EXT
           END-IF
      *    一覧へ
           IF      LNK-SC97-INPUTCD(1:2)
                                       =  "//"
               MOVE    SPA-MAE-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               INITIALIZE                      SPA-K98-AREA
               MOVE    LNK-SC97-INPUTCD    TO  SPA-K98-INPUTCD
               PERFORM 410-K98-SYORI-SEC
               GO      TO                  41011-KOUI-HEN2-EXT
           END-IF
      *    Ｐ入力へ
           IF      LNK-SC97-INPUTCD(1:1)
                                       =  "/"
               MOVE    SPA-MAE-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               PERFORM 430-K99-SYORI-SEC
               GO      TO                  41011-KOUI-HEN2-EXT
           END-IF
      *
           IF     LNK-SC97-NYURYOKU    =   1
      *        セットが変更削除の時セット内容を削除する
               IF     (SPA-MAE-INPUTCD(SPA-GMN-IDX)(1:1)   =   "S") OR
                     ((SPA-GMN-IDX           <   WRK-GYO-MAX  ) AND
                      (SPA-COM-SET  (SPA-GMN-IDX + 1)      =   "1"))
                   PERFORM 41016-SETDEL-SEC
               END-IF
               MOVE    SPA-COM-TIMEFLG(SPA-GMN-IDX)    TO  WRK-TIMEFLG
               MOVE    SPA-COM-IN     (SPA-GMN-IDX)    TO  WRK-FLG-IN
               MOVE    SPA-GMN-MEISYO-G(SPA-GMN-IDX)   TO  WRK-MEISYO-G
               INITIALIZE                  SPA-GMN-TBLREC (SPA-GMN-IDX)
               INITIALIZE                  SPA-COM-TBLREC (SPA-GMN-IDX)
               MOVE    WRK-TIMEFLG     TO  SPA-COM-TIMEFLG(SPA-GMN-IDX)
               MOVE    WRK-FLG-IN      TO  SPA-COM-IN  (SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-IN2 (SPA-GMN-IDX)
           ELSE
               MOVE    SPA-GMN-MEISYO-G(SPA-GMN-IDX)   TO  WRK-MEISYO-G
           END-IF
      *
           MOVE    LNK-SC97-INPUTCD    TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
      *
      *    診療行為一覧へ
           IF      LNK-SC97-KENSAKU    =   "1"
               INITIALIZE                      SPA-K98-AREA
               MOVE    LNK-SC97-INCD       TO  SPA-K98-INPUTCD
               MOVE    "1"                 TO  SPA-K98-TYPE
               MOVE    LNK-SC97-CDSYU      TO  SPA-K98-CDSYUBETU
               MOVE    "3"                 TO  SPA-COM-CUR (SPA-GMN-IDX)
      *
      *        診療行為一覧へ
               PERFORM 410-K98-SYORI-SEC
               GO      TO                 41011-KOUI-HEN2-EXT
           END-IF
      *
      *    診療種別入力
           IF      (SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)  =   ".") AND
                   (SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:3)  NUMERIC)
               INITIALIZE                  SPA-GMN-TBLREC (SPA-GMN-IDX)
               INITIALIZE                  SPA-COM-TBLREC (SPA-GMN-IDX)
               MOVE    LNK-SC97-INPUTCD(1:4)   TO  SPA-GMN-INPUTCD
                                                          (SPA-GMN-IDX)
               MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:3)
                                               TO  WRK-SRYSYUKBN
      *        診療種別区分名称チェック
               PERFORM 510-SRYKBN-MEI-SEC
               MOVE    WRK-SRYSYUKBNMEI    TO  SPA-GMN-MEISYO
                                                    (SPA-GMN-IDX)
               MOVE    "*"                 TO  SPA-GMN-MEIH
                                                    (SPA-GMN-IDX)
               MOVE    WRK-SRYKBN          TO  SPA-GMN-SRYKBN
                                                    (SPA-GMN-IDX)
           ELSE
      *        文字変換・編集処理
               PERFORM 410112-INPUTCD-HENKAN-SEC
      *
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      FLG-HENKOU          =   1
                   CONTINUE
               ELSE
                   MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-MAE-INPUTCD(SPA-GMN-IDX)
               END-IF
           ELSE
               MOVE    SPACE           TO  SPA-MAE-INPUTCD(SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
           END-IF
      *
      *    数量ゼロエラーはエラー表示しない
           IF      SPA-ERRCD           =   "A001"
               MOVE    SPA-ERRCD           TO  WRK-ERRCD2
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
      *
           .
       41011-KOUI-HEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｐ入力画面へ移行
      *****************************************************************
       430-K99-SYORI-SEC       SECTION.
      *
      *    画面編集
           INITIALIZE                  SPA-K99GMN-AREA
           MOVE    SPACE               TO  K99
      *
      *    画面カーソルセット処理
           MOVE    "PIN"               TO  MCP-WIDGET
      *
           MOVE    SPACE               TO  SPA-K99-DATA
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    "K99"               TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "K02N"              TO  SPA-K99-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K99"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       430-K99-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為一覧へ
      *****************************************************************
       410-K98-SYORI-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-KOUI
      *
           INITIALIZE                  SPA-K98GMN-AREA
      *****IF      SPA-K98-INPUTCD         =   SPACE
           IF      SPA-K98-INPUTCD(1:2)    =   "//"
      *        コメント一覧
               IF      SPA-K98-INPUTCD(3:1)    =   "C" OR "c"
                   MOVE    "U"                 TO  SPA-K98-GMN-SYORI
                   MOVE    "U4"                TO  SPA-K98-GMN-SYORI3
                   MOVE    SPACE               TO  SPA-K98-INPUTCD
               END-IF
      *        用法
               IF      SPA-K98-INPUTCD(3:1)    =   "Y" OR "y"
                   MOVE    "U"                 TO  SPA-K98-GMN-SYORI
                   MOVE    "U1"                TO  SPA-K98-GMN-SYORI3
                   MOVE    SPACE               TO  SPA-K98-INPUTCD
               END-IF
      *        診療種別一覧
               IF      SPA-K98-INPUTCD(3:1)    =   "."
                   MOVE    "."                 TO  SPA-K98-INPUTCD
               ELSE
      *            区分番号別
                   IF      SPA-K98-INPUTCD(3:1)    =   "*"
                                                   OR  "/"
                       MOVE    SPA-K98-INPUTCD(3:) TO  SPA-K98-INPUTCD
                   ELSE
                       MOVE    SPACE               TO  SPA-K98-INPUTCD
                   END-IF
               END-IF
      *        画面編集
               PERFORM 4101-K98-SET-SEC
           ELSE
               INITIALIZE                  SPA-K98GMN-AREA
      *        内服薬剤
               MOVE    "3"                 TO  SPA-K98-GMN-SYORI
               IF     (SPA-GMN-IDX         =   1  )  OR 
                      (SPA-COM-KAIFLG (SPA-GMN-IDX - 1)    =   "1" ) OR
                      (SPA-GMN-INPUTCD(SPA-GMN-IDX - 1)(1:1)   =   "S")
      *            内服薬剤
                   MOVE    "3"                 TO  SPA-K98-GMN-SYORI
               ELSE
                   EVALUATE    SPA-COM-SRYKBN(SPA-GMN-IDX - 1)
                       WHEN    SPACE
      *                    診療区分が決定してない時
                           PERFORM  4101-K98-SYRKBN-SEC
                       WHEN    "31"
                       WHEN    "32"
                       WHEN    "33"
      *                    注射
                           MOVE    "5"         TO  SPA-K98-GMN-SYORI
                       WHEN    "23"
      *                    外薬
                           MOVE    "4"         TO  SPA-K98-GMN-SYORI
                       WHEN    "21"
                       WHEN    "22"
      *                    内服
                           MOVE    "3"         TO  SPA-K98-GMN-SYORI
                       WHEN    OTHER
      *                    診療行為
      *D                   MOVE    "8"         TO  SPA-K98-GMN-SYORI
                           MOVE    "3"         TO  SPA-K98-GMN-SYORI
                           MOVE    1           TO  FLG-KOUI
                   END-EVALUATE
               END-IF
      *        画面編集
               PERFORM 4101-K98-SET-SEC
      *
      *        点数マスタ編集で、対象データなしの時、診療行為で検索する
               IF     (SPA-K98-GMN-MAX     =   ZERO )  AND
                      (SPA-K98-CDSYUBETU   =   "J"  )  AND
                      (FLG-KOUI            =   1    )
                   INITIALIZE                  SPA-K98GMN-AREA
                   MOVE    "8"                 TO  SPA-K98-GMN-SYORI
      *            画面編集
                   PERFORM 4101-K98-SET-SEC
               END-IF
           END-IF
      *
      *    エラークリア
           MOVE    SPACE               TO  SPA-ERRCD
      *    画面カーソルセット処理
           IF      SPA-K98-GMN-MAX     =   ZERO
               MOVE    "INPUT"             TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM1"           TO  MCP-WIDGET
           END-IF
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
      *    診療行為一覧へ
           MOVE    "K98"               TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "K02N"              TO  SPA-K98-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K98"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
      *
       410-K98-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    診療区分が決定してない時診療種別処理
      *****************************************************************
       4101-K98-SYRKBN-SEC       SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-IDX
               EVALUATE    SPA-GMN-INPUTCD(IDX)(1:3)
                   WHEN    ".21"
                       MOVE    "3"                 TO  SPA-K98-GMN-SYORI
                   WHEN    ".22"
                       MOVE    "3"                 TO  SPA-K98-GMN-SYORI
                   WHEN    ".23"
                       MOVE    "4"                 TO  SPA-K98-GMN-SYORI
                   WHEN    ".31"
                   WHEN    ".32"
                   WHEN    ".33"
                       MOVE    "5"                 TO  SPA-K98-GMN-SYORI
               END-EVALUATE
           END-PERFORM
      *
           .
      *
       4101-K98-SYRKBN-EXT.
           EXIT.
      *****************************************************************
      *    Ｋ９８画面編集処理
      *****************************************************************
       4101-K98-SET-SEC       SECTION.
      *
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K02             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGK98"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-K02
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
           .
      *
       4101-K98-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療コード入力処理
      *****************************************************************
       410119-SAIINPUT-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-IN-IDX
           MOVE    ZERO                TO  WRK-IN-IDX2
           MOVE    ZERO                TO  WRK-MAX-IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   WRK-GYO-MAX
               IF      SPA-COM-IN  (IDX)   =   "1"
                   MOVE    IDX                 TO  WRK-IN-IDX
               END-IF
               IF      SPA-COM-IN  (IDX)   =   "3"
                   MOVE    IDX                 TO  WRK-IN-IDX2
               END-IF
               IF     (SPA-COM-SET (IDX)   =   SPACE )  AND
                      (SPA-GMN-INPUTCD(IDX)    NOT =   SPACE )
                   MOVE    IDX             TO  WRK-MAX-IDX
               END-IF
      *        '_'をなくす
               INSPECT SPA-GMN-INPUTCD (IDX)
                       REPLACING   ALL "_"  BY  " "
      *        ＊をなくす
               PERFORM 51011-KAISU-CHK-SEC
      *
           END-PERFORM
      *
           IF     (FLG-INPUT-NASI      =   1  )  AND
                  (FLG-KUHAKU          =   1  )
               MOVE    ZERO                TO  WRK-IN-IDX
           END-IF
      *
           IF      WRK-IN-IDX          =   ZERO
      *******  MOVE    SPA-GMN-MAX         TO  SPA-GMN-IDX
               MOVE    WRK-MAX-IDX         TO  SPA-GMN-IDX
           ELSE
               MOVE    WRK-IN-IDX          TO  SPA-GMN-IDX
           END-IF
      *
      *    数量・回数入力なし
           IF     (SPA-GMN-IDX             >   ZERO        ) AND
                  (SPA-COM-SURFLG (SPA-GMN-IDX)  =   SPACE ) AND
                  (SPA-COM-IN2    (SPA-GMN-IDX)  =   "2"   ) AND
                  (SPA-COM-KAIFLG (SPA-GMN-IDX)  =   SPACE ) AND
                  (WRK-IN-IDX2                   =   ZERO  )
               MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                            TO  WRK-INPUTCD
               MOVE    "3"                  TO  SPA-COM-CUR
                                                       (SPA-GMN-IDX)
               IF      SPA-COM-INPUTCD (SPA-GMN-IDX)(1:3)  =   "001"
      ************ 投薬の時のみ、回数入力へ
      *            IF      SPA-COM-SRYKBN(SPA-GMN-IDX)     =   "21" OR
      *                                                        "22" OR
      *                                                        "23"
      *                STRING  WRK-INPUTCD      DELIMITED  BY  SPACE
      *                        "*"              DELIMITED  BY  SIZE
      *                                         INTO  SPA-GMN-INPUTCD
      *                                                  (SPA-GMN-IDX)
      *                END-STRING
      ************ ELSE
                       MOVE    SPACE            TO  SPA-COM-CUR
                                                       (SPA-GMN-IDX)
      ************ END-IF
               ELSE
                   STRING  WRK-INPUTCD          DELIMITED  BY  SPACE
                           "_"                  DELIMITED  BY  SIZE
                                                INTO  SPA-GMN-INPUTCD
                                                         (SPA-GMN-IDX)
                   END-STRING
               END-IF
               MOVE    SPACE                TO  SPA-ERRCD
           END-IF
      *    名称入力へのカーソル移動
           IF      WRK-IN-IDX          >   ZERO
               IF     (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)
                                           =   "81"  OR  "83") OR
                      (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)
                                           =   "0083") 
                   MOVE    "3"             TO  SPA-COM-IN2 (SPA-GMN-IDX)
               END-IF
           END-IF
      *
      *    空白行入力時、前の行へ
           IF     (FLG-KUHAKU          =   1  )  AND
                  (SPA-GMN-IDX         >   ZERO)
               IF     (SPA-GMN-INPUTCD (SPA-GMN-IDX)(1:1)
                                               NOT =   "."  )  AND
                      (SPA-COM-KAIFLG (SPA-GMN-IDX)    =   SPACE )
      *            回数入力へ
                   MOVE    ZERO                TO  WRK-IN-IDX2
                   MOVE    ZERO                TO  FLG-ARI
                   PERFORM VARYING     IDX     FROM    1   BY  1
                           UNTIL       IDX     >   22
                       IF      SPA-GMN-INPUTCD (SPA-GMN-IDX)(IDX:1)
                                                   =   "*"
                           MOVE    1               TO  FLG-ARI
                       END-IF
                       IF      SPA-GMN-INPUTCD (SPA-GMN-IDX)(IDX:1)
                                               NOT =  SPACE
                           MOVE    IDX             TO  WRK-IN-IDX2
                       END-IF
                   END-PERFORM
                   IF     (FLG-ARI                 =   ZERO )  AND
                          (WRK-IN-IDX2             <   22   )
                       ADD     1                   TO  WRK-IN-IDX2
                       IF    ((SPA-COM-IN3 (SPA-GMN-IDX)  =   "1" ) AND
                              (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:3)
                                                      NOT =   "001"))
                         OR  ((SPA-GMN-INPUTCD (SPA-GMN-IDX)(1:1)
                                                       =   "S" ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
                         OR  ((SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)
                                                       =   "84" ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
                         OR  ((SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)
                                                       =   "0084") AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
                           MOVE    "_"             TO  SPA-GMN-INPUTCD
                                                     (SPA-GMN-IDX)
                                                     (WRK-IN-IDX2:)
      *********        ELSE
      *                    MOVE    "*"             TO  SPA-GMN-INPUTCD
      *                                              (SPA-GMN-IDX)
      **********                                     (WRK-IN-IDX2:)
                       END-IF
                   END-IF
               END-IF
      *        空白行入力時、最終行へ
               MOVE    "3"                 TO  SPA-COM-CUR
                                                     (SPA-GMN-IDX)
           END-IF
      *
           .
       410119-SAIINPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    文字列変換・編集処理
      *****************************************************************
       410112-INPUTCD-HENKAN-SEC      SECTION.
      *
      *    変換文字列編集
           PERFORM 4101121-MOJI-SET-SEC
      *
      *    回数入力
           IF      SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)
                                       =   "*"
               PERFORM 4101129-KAISUU-CHK-SEC
               GO      TO      410112-INPUTCD-HENKAN-EXT
           END-IF
      *    回数入力があった時エラーとする
      *    IF      LNK-SC97-KAISUCNT   >   ZERO
      *        MOVE    "9003"          TO  SPA-ERRCD
      *        GO      TO      410112-INPUTCD-HENKAN-EXT
      *    END-IF
      *
      *    セット入力処理へ
           IF      SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)
                                           =   "S"  OR  "P"
               PERFORM 410119-INPSET-SEC
               GO      TO      410112-INPUTCD-HENKAN-EXT
           END-IF
      *
      *    入力内容チェック、編集
           IF      LNK-SC97-NYURYOKU   =   1
               MOVE    LNK-SC97-INCD   TO  SPA-COM-INPUTCD(SPA-GMN-IDX)
           END-IF
           PERFORM 4101113-INPUTCD-CHK-SEC
      *
      *    名称チェック
           PERFORM 40113-MEISYOU-CHK-SEC
      *
           .
       410112-INPUTCD-HENKAN-EXT.
           EXIT.
      *****************************************************************
      *    入力内容チェック、編集処理
      *****************************************************************
       4101113-INPUTCD-CHK-SEC       SECTION.
      *
      *
      *    入力コード変更ありの時
           IF      LNK-SC97-NYURYOKU   =   1
      *
      *        診療行為コードをもとに点数マスター検索
               MOVE    ZERO                TO  FLG-TENSU
               MOVE    SPA-COM-INPUTCD(SPA-GMN-IDX)
                                           TO  TNS-SRYCD
      *
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU           =   1
                   MOVE    "0001"              TO  SPA-ERRCD
                   GO      TO      4101113-INPUTCD-CHK-EXT
               END-IF
      *
           END-IF
      *
           MOVE    SPACE               TO  SPA-COM-IN3 (SPA-GMN-IDX)
      *
      *    数量ゼロはエラーとする
           IF     (WRK-SUR-MCNT        >   ZERO  )  AND
                  (WRK-SURYO           =   ZERO  )
               IF      SPA-COM-INPUTCD(SPA-GMN-IDX)(1:1)
                                               =   "6"  OR  "7"
                   MOVE    "A001"              TO  SPA-ERRCD
               ELSE
                   MOVE    "A002"              TO  SPA-ERRCD
               END-IF
               IF      LNK-SC97-NYURYOKU   =   1
                   MOVE    TNS-NAME        TO  SPA-GMN-MEISYO
                                                     (SPA-GMN-IDX)
               END-IF
               GO      TO      4101113-INPUTCD-CHK-EXT
           END-IF
      *
      *
      *    数量以降入力なし（’＿’編集用）
           IF     (WRK-SUR-MCNT        =   ZERO )  AND
                  (WRK-ACCT-MCNT       =   ZERO )  AND
                  (WRK-MCNT1           =   ZERO )  AND
                  (WRK-MCNT2           =   ZERO )  AND
                  (WRK-MCNT3           =   ZERO )  AND
                  (WRK-MCNT4           =   ZERO )  AND
                  (FLG-KAISU           =   ZERO )
               MOVE    ZERO                TO  FLG-IN1
      *        薬剤と数量入力必須の診療行為、薬剤コメント
               IF      SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)   =   "059"
                   MOVE    "7"                     TO  WRK-MOJI
               ELSE
                   MOVE    SPA-COM-INPUTCD(SPA-GMN-IDX)(1:1)
                                                   TO  WRK-MOJI
               END-IF
      *        数量の入力の有無
               IF     (WRK-MOJI                    =   "6" OR
                                                       "7" )  OR
                     ( SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)
                                                   =   "001" )
      *            薬剤・器材・用法
                   MOVE    1                   TO  FLG-IN1
               END-IF
               IF      WRK-MOJI                =   "1"
                   IF       LNK-SC97-NYURYOKU      =   1
      *            診察料で、基準があるもの
                       IF  (TNS-SRYSYUKBN(1:1)      NOT =   "7" ) AND
                           (TNS-KZMCOMPSIKIBETU     NOT =   0   )
                           MOVE    1                   TO  FLG-IN1
                       END-IF
                   ELSE
                       IF  (SPA-COM-SRYSYUKBN(SPA-GMN-IDX)(1:1)
                                                    NOT =   "7" ) AND
                           (SPA-COM-KZMCOMPSIKIBETU(SPA-GMN-IDX)
                                                    NOT =   0   )
                           MOVE    1                   TO  FLG-IN1
                       END-IF
                   END-IF
               END-IF
               IF     (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:2)
                                                   =   "84"  )  OR
                      (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:4)
                                                   =   "0084")
      *            コメント
                   MOVE    1                   TO  FLG-IN1
               END-IF
               IF     (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:5)
                                                   =   "09500" OR
                                                       "09600" )
      *            自費
                   IF     (LNK-SC97-NYURYOKU       =   1    )  AND
                          (TNS-TEN                 =   ZERO )
                       MOVE    1                   TO  FLG-IN1
                   END-IF
      *            自費
                   IF     (LNK-SC97-NYURYOKU       =   ZERO  )  AND
                          (SPA-COM-TEN (SPA-GMN-IDX)   =   ZERO )
                       MOVE    1                   TO  FLG-IN1
                   END-IF
               END-IF
      *
               IF      FLG-IN1             =   1
                   MOVE    "2"             TO  SPA-COM-IN2 (SPA-GMN-IDX)
                   MOVE    "1"             TO  SPA-COM-IN3 (SPA-GMN-IDX)
               END-IF
           END-IF
      *
      *    数量５文字以上はエラー
           IF     (SPA-COM-INPUTCD(SPA-GMN-IDX) (1:2)
                                   NOT =   "09"  )  AND
                  (LNK-SC97-SURYOFLG    >   5    )
               MOVE    "0051"          TO  SPA-ERRCD
               GO      TO      4101113-INPUTCD-CHK-EXT
           END-IF
      *
      *    数量・回数編集
           PERFORM 4101122-KOMOKU-HENSYU-SEC
      *
      *    数量・回数入力チェック
           PERFORM 4101121-INPUTCD-CHECK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4101113-INPUTCD-CHK-EXT
           END-IF
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    SPACE               TO  LNK-SC92-KBN
      *    入力コード変更ありの時
           IF      LNK-SC97-NYURYOKU   =   1
               MOVE    SPACE               TO  LNK-SC92-KBN
           ELSE
               MOVE    "5"                 TO  LNK-SC92-KBN
           END-IF
           IF      FLG-HENKOU          =   1
      *        画面遷移前
               IF     (LNK-SC97-NYURYOKU       =   1 ) OR
                      (SPA-COM-CHKFLG (SPA-GMN-IDX)  =   SPACE)
                   MOVE    "6"                 TO  LNK-SC92-KBN
               END-IF
           END-IF
           MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
           MOVE    "K02"               TO  LNK-SC92-PG
           MOVE    SPA-GMN-IDX         TO  LNK-SC92-GMN-IDX
           MOVE    SPA-COM-INPUTCD (SPA-GMN-IDX)
                                       TO  LNK-SC92-SRYCD
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
           MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *
      *    剤内に入院回数がある時、併用算定と算定チェックをしない
      **** IF      SPA-COM-NYU-KAI  (SPA-GMN-IDX)
      ***                          NOT =   ZERO
           IF      SPA-SYORIFLG        =   ZERO
               MOVE    SPACE           TO  LNK-SC92-ERRCD(3)
               MOVE    SPACE           TO  LNK-SC92-ERRCD(6)
           END-IF
      *
           PERFORM VARYING     IDX-ERR FROM    1  BY  1
                   UNTIL      (IDX-ERR     >   10     )  OR
                              (SPA-ERRCD   NOT =   SPACE)
               IF      LNK-SC92-ERRCD(IDX-ERR) NOT =   SPACE
                   MOVE    LNK-SC92-ERRCD(IDX-ERR)     TO  SPA-ERRCD
                   IF      IDX-ERR             =   3   OR  6
                       MOVE    LNK-SC92-ERRMSG     TO  SPA-ERRMSG2
                   END-IF
               END-IF
           END-PERFORM
      *
      *    その他値の入力の有無
           IF     (WRK-HZN-ATAI1   =   SPA-COM-ATAI1 (SPA-GMN-IDX)) AND
                  (WRK-HZN-ATAI2   =   SPA-COM-ATAI2 (SPA-GMN-IDX)) AND
                  (WRK-HZN-ATAI3   =   SPA-COM-ATAI3 (SPA-GMN-IDX)) AND
                  (WRK-HZN-ATAI3   =   SPA-COM-ATAI4 (SPA-GMN-IDX))
               MOVE    ZERO                TO  FLG-IN-ATAI
           ELSE
               MOVE    1                   TO  FLG-IN-ATAI
           END-IF
      *
      *    小児科外来診療科の時、年齢エラーはそのまま
           IF     (SPA-SYONIKA-ARI     NOT =   ZERO )  AND
                  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:1)  =   "1" )
               IF      SPA-ERRCD           =   "0032"
                   MOVE    SPACE               TO  SPA-ERRCD
               END-IF
           END-IF
      *---(01.00.01) LINE ADD  START  ----
      *    警告判定
           IF      SPA-ERRCD(1:1)      =   "K"
               IF      SPA-COM-KZMFLG (SPA-GMN-IDX)  =   SPACE
                   MOVE    "1"             TO  SPA-COM-KZMFLG
                                                          (SPA-GMN-IDX)
                   MOVE    "2"             TO  SPA-COM-CUR(SPA-GMN-IDX)
               ELSE
                   MOVE    SPACE           TO  SPA-ERRCD
                   MOVE    SPACE           TO  SPA-COM-CUR(SPA-GMN-IDX)
               END-IF
           END-IF
      *---(01.00.01) LINE ADD  END    ----
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    SPACE           TO  SPA-COM-CHKFLG  (SPA-GMN-IDX)
               GO      TO      4101113-INPUTCD-CHK-EXT
           END-IF
      *
      **** 時間外加算の時、ＳＰＡへ
      *    IF      SPA-COM-TIMEFLG(SPA-GMN-IDX)  NOT =   ZERO
      *        IF      SPA-SYORIFLG        =   ZERO
      *            MOVE    SPA-COM-TIMEFLG(SPA-GMN-IDX)
      *                                    TO  SPA-NAI-TIMEFLG
      *        END-IF
      **** END-IF
      *
      *    保険適用区分が保険適用外のもの（ユーザー登録の自費分）
           IF      SPA-COM-HKNTEKKBN(SPA-GMN-IDX)  =   2
      *        数量を金額とする
               MOVE    1               TO  SPA-COM-SURYO(SPA-GMN-IDX)
               IF      SPA-COM-KISOTEN (SPA-GMN-IDX)  =   ZERO
                   MOVE    WRK-SURYO       TO  SPA-COM-KEI(SPA-GMN-IDX)
                   MOVE    WRK-SURYO       TO  SPA-COM-JIHIMONEY
                                                          (SPA-GMN-IDX)
                   MOVE    WRK-SURYO       TO  SPA-GMN-KEI(SPA-GMN-IDX)
                   MOVE    "1"             TO  SPA-COM-KEIFLG
                                                          (SPA-GMN-IDX)
               ELSE
                   MOVE    "2"             TO  SPA-COM-KEIFLG
                                                          (SPA-GMN-IDX)
      *            定額の時、数量入力エラー
                   IF      WRK-SURYO       NOT =   ZERO
                       MOVE    "1002"          TO  SPA-ERRCD
                   END-IF
               END-IF
           ELSE
               MOVE    SPACE               TO  SPA-COM-KEIFLG
                                                          (SPA-GMN-IDX)
           END-IF
      *
      *
           .
       4101113-INPUTCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力文字を数字変換処理
      *****************************************************************
       4101121-MOJI-SET-SEC        SECTION.
      *
           MOVE    LNK-SC97-SURYOCNT   TO  WRK-SUR-MCNT
           MOVE    LNK-SC97-KAISUCNT   TO  WRK-ACCT-MCNT
           MOVE    LNK-SC97-SURYO      TO  WRK-SURYO
           MOVE    LNK-SC97-KAISU      TO  WRK-KAISU
           MOVE    LNK-SC97-KAISUFLG   TO  FLG-KAISU
           MOVE    LNK-SC97-BUNGSU     TO  WRK-BUNGSU
           MOVE    LNK-SC97-MCNT1      TO  WRK-MCNT1
           MOVE    LNK-SC97-MCNT2      TO  WRK-MCNT2
           MOVE    LNK-SC97-MCNT3      TO  WRK-MCNT3
           MOVE    LNK-SC97-MCNT4      TO  WRK-MCNT4
           MOVE    LNK-SC97-MOJI1      TO  WRK-MOJI1
           MOVE    LNK-SC97-MOJI2      TO  WRK-MOJI2
           MOVE    LNK-SC97-MOJI3      TO  WRK-MOJI3
           MOVE    LNK-SC97-MOJI4      TO  WRK-MOJI4
           .
       4101121-MOJI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療コード入力チェック処理
      *****************************************************************
       4101121-INPUTCD-CHECK-SEC    SECTION.
      *
           MOVE    SPA-COM-INPUTCD (SPA-GMN-IDX)(1:1)
                                           TO  WRK-MOJI
      *
           IF      SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)   =   "059"
               MOVE    "7"                 TO  WRK-MOJI
           END-IF
           IF      SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)   =   "008"
               MOVE    "8"                 TO  WRK-MOJI
           END-IF
      *
           EVALUATE    WRK-MOJI
      *
      *        診療行為
               WHEN    "1"
      *            きざみ値計算識別判定がない時、数量入力エラー(29)
                   IF      LNK-SC97-NYURYOKU       =   1
                       IF      TNS-KZMCOMPSIKIBETU     =   0 
                           IF      WRK-SUR-MCNT    NOT =   ZERO
                               MOVE    "0002"          TO  SPA-ERRCD
                           END-IF
      *!!              ELSE
      *!!              数量　＞　きざみ値上限の時、エラー
      *                    IF      SPA-COM-SURYO(SPA-GMN-IDX)
      *                                            >   TNS-KZMJGN
      *                            MOVE    "0037"          TO  SPA-ERRCD
      *!!                  END-IF
                       END-IF
                   ELSE
      *                コード変更なし
                       IF      SPA-COM-KZMCOMPSIKIBETU(SPA-GMN-IDX)
                                                       =   0
                           IF      WRK-SUR-MCNT    NOT =   ZERO
                               MOVE    "0002"          TO  SPA-ERRCD
                           END-IF
      *!!              ELSE
      *                数量　＞　きざみ値上限の時、エラー
      *                    IF      SPA-COM-SURYO(SPA-GMN-IDX)
      *                                            >   SPA-COM-KZMJGN
      *                                                    (SPA-GMN-IDX)
      *                        MOVE    "0037"          TO  SPA-ERRCD
      *!!                  END-IF
                       END-IF
                   END-IF
      *
      *        医薬品
      *        WHEN    "6"
      *            IF      WRK-SUR-MCNT        =   ZERO
      *                MOVE    "0003"          TO  SPA-ERRCD
      *            END-IF
      *
      *        特定器材
               WHEN    "7"
      *            1行目入力不可
                   IF      SPA-GMN-IDX     =   1
                       MOVE    "0013"          TO  SPA-ERRCD
                   END-IF
      *
      *        コメント
               WHEN    "8"
      *            数量をゼロとする
                   MOVE    ZERO            TO  WRK-SUR-MCNT
      *
               WHEN    OTHER
                   CONTINUE
           END-EVALUATE
      *
      *    時間加算チェック
           IF     (SPA-ERRCD           =   SPACE   )  AND
                  (LNK-SC97-JIKAN  NOT =   ZERO    )
      *        時間加算区分
               IF     (LNK-SC97-JIKAN      =   1   OR  2  OR  3
                                                   OR  4  OR  5
                                                   OR  6  OR  7)  AND
                      (((LNK-SC97-NYURYOKU     =   1    )  AND
                        (TNS-DATAKBN           =   "1"  ))   OR
                       ((LNK-SC97-NYURYOKU     =   ZERO   )  AND
                        (SPA-COM-DATAKBN(SPA-GMN-IDX)
                                               =   "1"  )) )
                   PERFORM 41011211-JIKANGAI-CHK-SEC
               ELSE
                   MOVE    "0030"          TO  SPA-ERRCD
               END-IF
      ***  ELSE
      *        IF      SPA-COM-TIMEFLG(SPA-GMN-IDX)
      *                                    NOT =   ZERO
      *            MOVE    ZERO            TO  SPA-NAI-TIMEFLG
      *            MOVE    ZERO            TO  SPA-COM-TIMEFLG
      *                                                   (SPA-GMN-IDX)
      ******   END-IF
           END-IF
           .
       4101121-INPUTCD-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    時間外区分チェック処理
      *****************************************************************
       41011211-JIKANGAI-CHK-SEC      SECTION.
      *
           IF      LNK-SC97-NYURYOKU   =   ZERO
               MOVE    SPA-COM-SRYKBN (SPA-GMN-IDX)
                                           TO  TNS-SRYKBN
               MOVE    SPA-COM-TIMEKSNKBN(SPA-GMN-IDX)
                                           TO  TNS-TIMEKSNKBN
           END-IF
           IF       TNS-SRYKBN         =   "11"
               CONTINUE
           ELSE
      *        診察料以外は時間外加算が算定できること
               IF      TNS-TIMEKSNKBN      =   ZERO
                   MOVE    "0030"              TO  SPA-ERRCD
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    LNK-SC97-JIKAN      TO  SPA-COM-TIMEFLG
                                                          (SPA-GMN-IDX)
           END-IF
           .
       41011211-JIKANGAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目編集処理
      *****************************************************************
       4101122-KOMOKU-HENSYU-SEC      SECTION.
      *
      *    数量
           MOVE    SPACE              TO  SPA-COM-AUTOFLG2(SPA-GMN-IDX)
           IF      WRK-SUR-MCNT        =   ZERO
               MOVE    1               TO  SPA-COM-SURYO(SPA-GMN-IDX)
               MOVE    SPACE           TO  SPA-COM-SURFLG(SPA-GMN-IDX)
           ELSE
               MOVE    WRK-SURYO       TO  SPA-COM-SURYO(SPA-GMN-IDX)
               IF      SPA-COM-SURYO(SPA-GMN-IDX)  =  ZERO
                   MOVE    1           TO  SPA-COM-SURYO(SPA-GMN-IDX)
               END-IF
               MOVE    "1"             TO  SPA-COM-SURFLG(SPA-GMN-IDX)
      *!!
               IF      LNK-SC97-AUTOFLG2   =   "1"
                   MOVE    "1"         TO  SPA-COM-AUTOFLG2(SPA-GMN-IDX)
               END-IF
      *!!
           END-IF
           MOVE    SPA-COM-SURYO(SPA-GMN-IDX)
                                       TO  SPA-COM-SURYO2(SPA-GMN-IDX)
      *
      *    回数（未入力時、１回）
           IF      WRK-ACCT-MCNT       =   ZERO
               IF     (SPA-COM-KAISU(SPA-GMN-IDX)  =  ZERO )  OR
                      (SPA-COM-KAISU(SPA-GMN-IDX)  >  1    )
                   MOVE    1           TO  SPA-COM-KAISU (SPA-GMN-IDX)
               END-IF
               MOVE    SPACE           TO  SPA-COM-KAIFLG(SPA-GMN-IDX)
           ELSE
               MOVE    "1"             TO  SPA-COM-KAIFLG(SPA-GMN-IDX)
               MOVE    WRK-KAISU       TO  SPA-COM-KAISU (SPA-GMN-IDX)
               IF      SPA-COM-KAISU(SPA-GMN-IDX)
                                       =   ZERO
                   MOVE    1           TO  SPA-COM-KAISU (SPA-GMN-IDX)
               END-IF
           END-IF
           MOVE    SPA-COM-KAISU(SPA-GMN-IDX)
                                       TO  SPA-COM-KAISU2(SPA-GMN-IDX)
      *
      *    分画数（未入力時、１回）
           IF     (SPA-COM-INPUTCD(SPA-GMN-IDX) (1:1)  =   "7" ) AND
                  (WRK-BUNGSU          =   ZERO   )  AND
                  (LNK-SC97-MOJI2  NOT =   SPACE  )
      *        分画数変換
               PERFORM 41011221-BUNGSU-HENSYU-SEC
           END-IF
           MOVE    WRK-BUNGSU          TO  SPA-COM-BUNGSU(SPA-GMN-IDX)
           IF      WRK-BUNGSU          =   ZERO
               MOVE    1               TO  SPA-COM-BUNGSU(SPA-GMN-IDX)
           END-IF
      *
      *    その他値
      *    入力値の入力判定用
           MOVE    SPA-COM-ATAI1 (SPA-GMN-IDX) TO  WRK-HZN-ATAI1
           MOVE    SPA-COM-ATAI2 (SPA-GMN-IDX) TO  WRK-HZN-ATAI2
           MOVE    SPA-COM-ATAI3 (SPA-GMN-IDX) TO  WRK-HZN-ATAI3
           MOVE    SPA-COM-ATAI4 (SPA-GMN-IDX) TO  WRK-HZN-ATAI4
      *
           MOVE    LNK-SC97-MOJI1      TO  SPA-COM-ATAI1 (SPA-GMN-IDX)
           MOVE    LNK-SC97-MOJI2      TO  SPA-COM-ATAI2 (SPA-GMN-IDX)
           MOVE    LNK-SC97-MOJI3      TO  SPA-COM-ATAI3 (SPA-GMN-IDX)
           MOVE    LNK-SC97-MOJI4      TO  SPA-COM-ATAI4 (SPA-GMN-IDX)
      *
           .
       4101122-KOMOKU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    分画数変換処理
      *****************************************************************
       41011221-BUNGSU-HENSYU-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-BUNGSU
           INITIALIZE                      ORCSNUMAREA
           MOVE    LNK-SC97-MOJI2(1:LNK-SC97-MCNT2)
                                       TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN         =   "1"   )
               MOVE    "0012"              TO  SPA-ERRCD
           ELSE
               MOVE    SNUM-OUTNUM         TO  WRK-BUNGSU
           END-IF
           MOVE    SPACE              TO  LNK-SC97-MOJI2
           MOVE    SPACE              TO  WRK-MOJI2
           MOVE    ZERO               TO  LNK-SC97-MCNT2
      *
           .
       41011221-BUNGSU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    回数入力チェック処理
      *****************************************************************
       4101129-KAISUU-CHK-SEC           SECTION.
      *
      *    回数に変更があった時、剤内の警告はクリアする
           IF     SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                   NOT =   SPA-MAE-INPUTCD(SPA-GMN-IDX)
               COMPUTE IDX-K           =   SPA-GMN-IDX   -   1
               PERFORM VARYING     IDX-K2  FROM    IDX-K  BY  -1
                       UNTIL       IDX-K2  <   1
                  IF      SPA-COM-ZAIEND (IDX-K2)  =   "1"
                      MOVE    1                TO  IDX-K2
                  ELSE
                      IF      SPA-COM-SRYKBN (IDX-K2)  =  "80"
                           MOVE    SPACE           TO  SPA-COM-KZMFLG
                                                            (IDX-K2)
                           MOVE    SPACE           TO  SPA-COM-KZMFLG2
                                                            (IDX-K2)
                      END-IF
                  END-IF
               END-PERFORM
           END-IF
      *
      *    クリア
           MOVE    SPACE           TO  SPA-COM-INPUTCD(SPA-GMN-IDX)
           INITIALIZE                  SPA-COM-TBLREC (SPA-GMN-IDX)
      *
      *    回数（未入力時、１回）
           IF      WRK-ACCT-MCNT       =   ZERO
               MOVE    1               TO  SPA-COM-KAISU (SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-KAIFLG(SPA-GMN-IDX)
           ELSE
               MOVE    "1"             TO  SPA-COM-KAIFLG(SPA-GMN-IDX)
               MOVE    WRK-KAISU       TO  SPA-COM-KAISU (SPA-GMN-IDX)
               IF      SPA-COM-KAISU(SPA-GMN-IDX)
                                       =   ZERO
                   MOVE    1           TO  SPA-COM-KAISU (SPA-GMN-IDX)
               END-IF
           END-IF
           MOVE    SPA-COM-KAISU(SPA-GMN-IDX)
                                       TO  SPA-COM-KAISU2(SPA-GMN-IDX)
      *
      *    入院回数・日数
           INITIALIZE                  SPA-COM-NYUIN-AREA (SPA-GMN-IDX)
      *    日付チェック
           IF      SPA-K02N-NYUINYMD(1:6)  =   SPA-SRYYMD(1:6)
               MOVE    SPA-K02N-NYUINYMD(7:2)  TO  WRK-STDD
           ELSE
               MOVE    01                      TO  WRK-STDD
           END-IF
           IF      SPA-K02N-TAIINYMD(1:6)  =   SPA-SRYYMD(1:6)
               MOVE    SPA-K02N-TAIINYMD(7:2)  TO  WRK-EDDD
           ELSE
               MOVE    SPA-SRYLST-DD           TO  WRK-EDDD
           END-IF
           MOVE    SPA-NAI-SRYYMD (7:2)    TO  WRK-SRYDD
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   31
              IF      LNK-SC97-NYU-DAY (IDX)   NOT =   ZERO
                   IF     (IDX             >=  WRK-STDD )  AND
                          (IDX             <=  WRK-EDDD )
                       MOVE    1               TO  SPA-COM-NYUINDAY
                                                       (SPA-GMN-IDX IDX)
                       ADD     1               TO  SPA-COM-NYU-KAI
                                                       (SPA-GMN-IDX)
                   ELSE
                       MOVE    "9007"          TO  SPA-ERRCD
                   END-IF
      *            訂正の時、訂正日のみ
                   IF     (SPA-SYORIFLG        =   1   )  AND
                          (IDX             NOT =   WRK-SRYDD)
                       MOVE    "9008"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       4101129-KAISUU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット入力処理
      *****************************************************************
       410119-INPSET-SEC           SECTION.
      *
      *    時間外がないこと
           IF      LNK-SC97-JIKAN      NOT =   ZERO
               MOVE    "0010"              TO  SPA-ERRCD
               GO  TO                 410119-INPSET-EXT
           END-IF
      *
      *
           MOVE    1                   TO  SPA-SINSATU-SAI
      *
           MOVE    SPACE               TO  WRK-ERRCD
      *
           MOVE    ZERO                TO  WRK-IDX-YAKUSOKU
           MOVE    ZERO                TO  IDX-YAKUSOKU
           PERFORM 4101191-INPUTSET-SYORI-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-ERRCD       TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410119-INPSET-EXT
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       410119-INPSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット入力処理
      *****************************************************************
       4101191-INPUTSET-SYORI-SEC      SECTION.
      *
      *    約束処方の初期処理
           IF      SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)   =   "S"
               IF      FLG-TOROKU      NOT =   "2"
                   PERFORM 4101192-INPSET-SET-SEC
               END-IF
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4101191-INPUTSET-SYORI-EXT
           END-IF
      *
           INITIALIZE                  ORCSCSETHENAREA
           MOVE    "K02"               TO  ORCSCSETHEN-PG
      *    セット名
           MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                       TO  ORCSCSETHEN-SRYCD
      *    対象行
           MOVE    SPA-GMN-IDX         TO  ORCSCSETHEN-IDX
           CALL    "ORCSCSETHEN"       USING
                                       ORCSCSETHENAREA
                                       SPA-AREA
                                       SPA-K02
                                       SPA-SCR-REC
      *
      *
           .
       4101191-INPUTSET-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    約束処方の処理
      *****************************************************************
       4101192-INPSET-SET-SEC           SECTION.
      *
      *    入力文字を数字へ変換
      **** MOVE    1               TO  WRK-IDX-FT
      **** PERFORM 4101121-MOJI-SET-SEC
      *
      *    入力位置のセット
           MOVE    ZERO                TO  WRK-IN-SURYO
           MOVE    SPA-GMN-IDX         TO  WRK-IN-IDX
      *    数量以降入力なし
           IF     (WRK-SUR-MCNT        =   ZERO )  AND
                  (WRK-ACCT-MCNT       =   ZERO )  AND
                  (WRK-MCNT1           =   ZERO )  AND
                  (WRK-MCNT2           =   ZERO )  AND
                  (WRK-MCNT3           =   ZERO )  AND
                  (WRK-MCNT4           =   ZERO )
               IF      SPA-GMN-INPUTCD (SPA-GMN-IDX)(1:1)  =   "S"
                   MOVE    1                   TO  WRK-IN-SURYO
                   MOVE    "1"                 TO  SPA-COM-IN
                                                         (SPA-GMN-IDX)
               END-IF
           END-IF
      *
      *    数量・回数編集
           PERFORM 4101122-KOMOKU-HENSYU-SEC
           MOVE    WRK-CD(1:WRK-CD-MCNT)   TO  WRK-INPUTCD
      *
           .
       4101192-INPSET-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定回数チェック処理（単独）
      *****************************************************************
       41011231-SANTEI-CHK-SEC      SECTION.
      *
      *    算定履歴チェック処理
           INITIALIZE                  ORCSC91AREA
           MOVE    SPA-SYORIFLG        TO  LNK-SC91-SYORIFLG
           MOVE    SPA-COM-INPUTCD (SPA-GMN-IDX)
                                       TO  LNK-SC91-SRYCD
           MOVE    WRK-SANTEI-SYORI    TO  LNK-SC91-KBN
           MOVE    SPA-GMN-IDX         TO  LNK-SC91-GMN-IDX
           MOVE    SPA-COM-KAISU   (SPA-GMN-IDX)
                                       TO  LNK-SC91-KAISU
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC91-TEISEI-AREA
      *
           CALL    "ORCSC91"           USING    MCPAREA
                                                ORCSC91AREA
                                                SPA-SCR-REC
                                                SPA-AREA
                                                TNS-TENSU-REC
           MOVE    LNK-SC91-ERRCD      TO  SPA-ERRCD
           MOVE    LNK-SC91-ERRMSG     TO  SPA-ERRMSG2
      *
           .
       41011231-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為、計計算処理、編集処理
      *****************************************************************
       510-TBLHEN-SEC           SECTION.
      *
      *    空白行をなくす
           PERFORM 420-SYOKYO-SEC
      *
      *    診療行為画面テーブル再編集
           PERFORM 5101-TBLSAIHEN-SEC
      *
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD           =   "0180" )
               OR (FLG-INIT            =   1      )
               GO      TO      510-TBLHEN-EXT
           END-IF
      *
      *    禁忌チェックを行う
           IF     (FLG-TOROKU          =   SPACE ) 
      ************(FLG-SYOKI           =   ZERO  )
               PERFORM 51010-KNKCHK-SEC
               IF      FLG-KINKI           =   1
                   GO      TO      510-TBLHEN-EXT
               END-IF
           END-IF
      *
      *    登録前の全体のチェックを行う（労災の時は、毎回）
           IF      FLG-TOROKU      NOT =   "2"
               INITIALIZE                  ORCSC00AREA
               MOVE    "2"                 TO  LNK-ORCSC-KBN
               PERFORM 520-ALL-CHEK-SEC
      *        更新前に確認画面を表示する
               IF      FLG-TOROKU      =   SPACE
                   IF      SPA-KIDCD       =   "0102"
                       MOVE    SPACE       TO  SPA-KIDCD
                   END-IF
               END-IF
      *
      *        診療行為の追加、修正時、再編集へ
               IF      FLG-TUIKA           =   1
      *
      *            空白行をなくす
                   PERFORM 420-SYOKYO-SEC
      *            診療行為画面テーブル再編集
                   PERFORM 5101-TBLSAIHEN-SEC
      *
               END-IF
           END-IF
      *一括チェックですべて行っているため
      *    算定回数チェック（各基本チェックを行う時、ここで）
      *    IF      FLG-TOROKU          =   "1"
      *        PERFORM VARYING SPA-GMN-IDX  FROM    1   BY  1
      *                UNTIL  (SPA-GMN-IDX     >   WRK-GYO-MAX  )   OR
      *                       (SPA-GMN-INPUTCD (SPA-GMN-IDX)
      *                                        =   SPACE )  OR
      *                       (SPA-ERRCD   NOT =   SPACE )
      *            IF      SPA-COM-SANTEIRRKKBN(SPA-GMN-IDX)
      *                                        NOT =   ZERO
      *                MOVE    "3"             TO  WRK-SANTEI-SYORI
      *                PERFORM 41011231-SANTEI-CHK-SEC
      *                IF      SPA-ERRCD(1:1)      =   "K"
      *                    IF      SPA-COM-KZMFLG (SPA-GMN-IDX)
      *                                                =   SPACE
      *                         MOVE    "1"        TO  SPA-COM-KZMFLG
      *                                                  (SPA-GMN-IDX)
      *                         MOVE    "2"        TO  SPA-COM-CUR
      *                                                  (SPA-GMN-IDX)
      *                    ELSE
      *                        MOVE    SPACE           TO  SPA-ERRCD
      *                        MOVE    SPACE           TO  SPA-COM-CUR
      *                                                  (SPA-GMN-IDX)
      *                   END-IF
      *                END-IF
      *                IF      SPA-ERRCD       NOT =   SPACE
      *                    MOVE    "1"            TO  SPA-COM-CUR
      *                                                   (SPA-GMN-IDX)
      *                END-IF
      *            END-IF
      *        END-PERFORM
      *****END-IF
      *
      *    ７０歳到達の警告
           IF      SPA-ERRCD       =   SPACE
               IF      SPA-NAI-ROUFLG      =   1
                   IF      SPA-NAI-ROUSTR      =   ZERO
                       MOVE    "K008"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
        510-TBLHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療行為画面テーブル再編集処理
      *****************************************************************
       5101-TBLSAIHEN-SEC             SECTION.
      *
      *    特定の診療行為の有無
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSC10SAREA
           MOVE    "6"                 TO  LNK-SC10S-SYORI
           CALL    "ORCSC10S"          USING
                                       ORCSC10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
      *    剤分離処理
           INITIALIZE                  ORCSC93AREA
           MOVE    "K02N"              TO  LNK-SC93-PG
      *!   小児科外来診療科算定有り
      *    MOVE    SPA-SYONIKA-ARI2    TO  LNK-SC93-SYONIKA-ARI2
      *    寝たきり在宅科算定有り
      *    IF      (SPA-NETAKIRI-ARI   NOT =   ZERO  )  OR
      *            (SPA-NETAKIRI-ARI2  NOT =   ZERO  )
      *        MOVE    1                   TO  LNK-SC93-NETAKIRI-ARI
      *!   END-IF
           CALL    "ORCSC93"           USING
                                       MCPAREA
                                       SPA-SCR-REC
                                       SPA-AREA
                                       ORCSC93AREA
                                       MSYU-DATA-REC
           IF      SPA-ERRCD       NOT =   SPACE
              AND (FLG-INIT            =   ZERO )
               GO      TO      5101-TBLSAIHEN-EXT
           END-IF
      *
      *!   特定診療チェック
      *    MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *    INITIALIZE                      ORCSC10SAREA
      *    MOVE    "7"                 TO  LNK-SC10S-SYORI
      *    CALL    "ORCSC10S"          USING
      *                                ORCSC10SAREA
      *                                SPA-AREA
      *                                SPA-SCR-REC
      *                                SPA-K02
      *    MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *    IF      SPA-ERRCD       NOT =   SPACE
      *        GO      TO      5101-TBLSAIHEN-EXT
      *!   END-IF
      *
      *    剤ごとに回数を入れ直す（リハビリ等同時回数チェックのため）
           MOVE    ZERO                TO  FLG-SONOTA
           MOVE    ZERO                TO  FLG-NAIFUTAN
           MOVE    ZERO                TO  WRK-KAISU
           INITIALIZE                      WRK-NYUIN-AREA
           PERFORM VARYING     IDX     FROM    WRK-GYO-MAX BY  -1
                   UNTIL       IDX     <   1
               IF     (SPA-GMN-INPUTCD (IDX)   =   SPACE )  AND
                      (SPA-COM-INPUTCD (IDX)   =   SPACE )
                   CONTINUE
               ELSE
                   IF     (WRK-KAISU               =   ZERO   )  OR
                          (SPA-COM-KAIFLG (IDX)    =   "1"    )  OR
                          (SPA-COM-ZAIEND (IDX)    =   "1"    )  OR
                          (IDX                     =   WRK-GYO-MAX)
      *                ’＊’の数だけ更新する
                     IF      (SPA-GMN-INPUTCD(IDX)(1:1)   =   "*" )
                        AND  (IDX                <   WRK-GYO-MAX)
                        AND  (SPA-GMN-INPUTCD(IDX + 1)(1:1)
                                                 =   "*"    )
                         CONTINUE
                     ELSE
                       MOVE    SPA-COM-KAISU (IDX) TO  WRK-KAISU
                       INITIALIZE                      WRK-NYUIN-AREA
      *
      *                外用薬の時、入力回数を
                       IF     (SPA-COM-SRYKBN(IDX) =  "23"  )  AND
                              (SPA-COM-KAISU2(IDX) >   ZERO ) AND
                              (SPA-COM-KAISU (IDX) =   1    )
                           MOVE    SPA-COM-KAISU2 (IDX) TO  WRK-KAISU
                       END-IF
                       IF      WRK-KAISU      =   ZERO
                           MOVE    1              TO  WRK-KAISU
                       END-IF
                     END-IF
                   END-IF
      *            入院回数・日数
                   IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "*"
      *************    MOVE    SPA-COM-NYUIN-AREA (IDX)
      *************                            TO  WRK-NYUIN-AREA
                       PERFORM VARYING     IDX-N2  FROM    1   BY  1
                               UNTIL       IDX-N2  >   31
                           IF      SPA-COM-NYUINDAY  (IDX IDX-N2)
                                               NOT =   ZERO
                               MOVE    SPA-COM-NYUINDAY  (IDX IDX-N2)
                                                   TO  WRK-NYUINDAY
                                                           (IDX-N2)
                           END-IF
                       END-PERFORM
                       ADD     SPA-COM-NYU-KAI (IDX)
                                                   TO  WRK-NYU-KAI
                   END-IF
                   IF     (WRK-NYU-KAI         =   ZERO )
      ************** AND  (SPA-SYORIFLG        =   ZERO )
                       MOVE    SPA-SRYYMD(7:2)     TO  IDX-N2
                       MOVE    1                   TO  WRK-NYUINDAY
                                                           (IDX-N2)
      **************   MOVE    WRK-KAISU           TO  WRK-NYU-KAI
                       MOVE    1                   TO  WRK-NYU-KAI
                   END-IF
      *            入院回数入力分以外
                   IF      SPA-GMN-INPUTCD(IDX)(1:1)   NOT =   "*"
                       MOVE    WRK-KAISU       TO  SPA-COM-KAISU (IDX)
                       MOVE    WRK-NYUIN-AREA  TO  SPA-COM-NYUIN-AREA
                                                                 (IDX)
                   END-IF
      *            その他の算定有無
                   IF      SPA-COM-SRYKBN (IDX)    =   "80"
                       MOVE    1               TO  FLG-SONOTA
                   END-IF
      *            内分泌負担試験有無
                   IF     (SPA-COM-SRYKBN (IDX)    =   "60"  )  AND
                          (SPA-COM-DATAKBN(IDX)    =   "1"   )  AND
                          (SPA-COM-HOUKNSKBN(IDX)  =   08    )
                       MOVE    1               TO  FLG-NAIFUTAN
                   END-IF
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   WRK-GYO-MAX
      *        入院日数がある時回数のチェック
               IF     (SPA-COM-SANTEIRRKKBN (IDX)
                                           NOT =   ZERO  )  AND
                      (SPA-COM-NYU-KAI (IDX)   >   ZERO  )
                   PERFORM 51019-NYUIN-SANCHK-SEC
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD       NOT =   SPACE
              AND (FLG-INIT            =   ZERO )
               GO      TO      5101-TBLSAIHEN-EXT
           END-IF
      *
      *    リハビリの回数編集
           INITIALIZE                  ORCSC80CNTAREA
           IF      FLG-SONOTA          =   1
               INITIALIZE                  ORCSC80CNTAREA
               MOVE    SPA-SYORIFLG        TO  ORCSC80CNT-SYORIFLG
               CALL    "ORCSC80CNT"        USING
                                           SPA-AREA
                                           SPA-K02
                                           ORCSC80CNTAREA
           END-IF
      *!!
      *    内分泌負担試験編集
           IF      FLG-NAIFUTAN        =   1
               INITIALIZE                  ORCSSANFUKAAREA
               MOVE    2                   TO  LNK-SANFUKA-SYORI
               MOVE    "K02"               TO  LNK-SANFUKA-PG
               MOVE    SPA-SRYYMD          TO  LNK-SANFUKA-SRYYMD
      *        訂正時の診療コード
               MOVE    SPA-K02-TEISEI-AREA TO  LNK-SANFUKA-TEISEI-AREA
               MOVE    SPA-NAI-FUKATEN-MAX TO  LNK-SANFUKA-TEN-MAX
               MOVE    SPA-NAI-FUKATEN-ALL TO  LNK-SANFUKA-MSANTESU
               MOVE    SPA-NAI-FUKATEN-DAY TO  LNK-SANFUKA-MSANTEID
               MOVE    SPA-NAI-FUKATEN-DEL TO  LNK-SANFUKA-MSANTESU-DEL
               CALL    "ORCSSANFUKA"       USING
                                           ORCSSANFUKAAREA
                                           SPA-AREA
                                           SPA-SCR-REC
      *        上限点数
               MOVE    LNK-SANFUKA-TEN-MAX     TO  SPA-NAI-FUKATEN-MAX
      *        当月分合計
               MOVE    LNK-SANFUKA-MSANTESU    TO  SPA-NAI-FUKATEN-ALL
      *        当月最終算定日
               MOVE    LNK-SANFUKA-MSANTEID    TO  SPA-NAI-FUKATEN-DAY
      *        訂正分合計
               MOVE    LNK-SANFUKA-MSANTESU-DEL
                                               TO  SPA-NAI-FUKATEN-DEL
               IF     (SPA-ERRCD           =   SPACE )  AND
                      (LNK-SANFUKA-ERRMSG  NOT =   SPACE)
                   MOVE    LNK-SANFUKA-ERRMSG  TO  SPA-ERRMSG2
                   MOVE    "1061"              TO  SPA-ERRCD
               END-IF
               IF     (SPA-ERRCD(1:1)      =   "K"   )  AND
                      (LNK-SANFUKA-ERRMSG  NOT =   SPACE)
                   MOVE    LNK-SANFUKA-ERRMSG  TO  SPA-ERRMSG2
               END-IF
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE)
              AND (FLG-INIT            =   ZERO )
               GO      TO      5101-TBLSAIHEN-EXT
           END-IF
      *!!
      *    剤ごとの処理
      *    診療種別件数クリア
           INITIALIZE                  SPA-SRYKBN-AREA
           MOVE    ZERO                TO  IDX-Z
           MOVE    ZERO                TO  WRK-KAISU
           INITIALIZE                  WRK-SCR-REC
           INITIALIZE                  WRK-HZN-SCR-AREA
           INITIALIZE                  WRK-HOZ-IDX
           MOVE    ZERO                TO  WRK-TIMEFLG
           MOVE    SPACE               TO  WRK-KEI-ERRCD
           MOVE    SPACE               TO  WRK-ERR-ERRCD
           MOVE    ZERO                TO  WRK-KEI-IDX
           MOVE    ZERO                TO  WRK-GOKEI-9
           INITIALIZE                  WRK-RIGAKU-TBL
      *    特定薬剤治療管理加算の初回算定日クリア
           MOVE    SPACE               TO  SPA-TOKTEI-YMD
      *    他保険受診ありクリア
           MOVE    ZERO                TO  SPA-TAHOKEN-FLG
           MOVE    ZERO                TO  WRK-KAISU-GOK
      *    消炎鎮痛処置当月内算定
           MOVE    SPA-SYOEN-CNT       TO  WRK-SYOEN-CNT
           MOVE    SPACE               TO  WRK-KIDCD
      *    剤別初回編集
           INITIALIZE                  ORCSC00AREA
      *    早期リハ等加算（今回入力分）
           MOVE    ORCSC80CNT-RIGAKU-AREA
                                       TO  LNK-ORCSC-RIGAKU-TBL
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   WRK-GYO-MAX      )   OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE )
      *        回数の入力があるかのチェック
               PERFORM 51011-KAISU-CHK-SEC
      *        剤ごとの算定処理で変更される加算データは対象としない
      *          但し、コメントの自動追加分は、そのまま
      *              （注射の廃棄コメントははずす）
               MOVE    ZERO                TO  FLG-TAISYO
               IF      SPA-COM-AUTOKBN (IDX)    =   "2"  OR  "3"
                   IF     ( SPA-COM-INPUTCD (IDX)(1:1)
                                               NOT =   "8"   )   OR
                          ((SPA-COM-SRYKBN  (IDX)  =   "31" OR
                                                       "32" OR
                                                       "33")   AND
                           (SPA-COM-INPUTCD (IDX)  =   "099309901" ))
                        MOVE    1               TO  FLG-TAISYO
                   END-IF
      *            薬剤の自動追加コメントは対象外
                   IF     ( SPA-COM-INPUTCD (IDX)(1:1)   =  "8" ) AND
                          ( SPA-COM-SRYKBN  (IDX)    =   "21"
                                                     OR  "22"
                                                     OR  "33"  )
                        MOVE    1               TO  FLG-TAISYO
                   END-IF
      *            自動追加コメントで入力値ありの時対象外
                   IF      (SPA-COM-INPUTCD (IDX)(1:2)   =  "84")  OR
                           (SPA-COM-INPUTCD (IDX)(1:4)   =  "0084")
                        MOVE    ZERO            TO  FLG-TAISYO
                   END-IF
      *            自動追加で画像診断の時対象外
                   IF      (SPA-COM-SRYKBN(IDX) NOT =  "95" AND
                                                       "96" ) AND
                           (SPA-COM-INPUTCD (IDX)   =  "170012070"
                                                   OR  "170020470")
                        MOVE    ZERO            TO  FLG-TAISYO
                   END-IF
               END-IF
      *
               IF      FLG-TAISYO              =   ZERO
                   ADD     1               TO  IDX-Z
                   MOVE    SPA-COM-TBLREC (IDX)
                                           TO  WRK-COM-TBLREC(IDX-Z)
                   MOVE    SPA-GMN-TBLREC (IDX)
                                           TO  WRK-GMN-TBLREC(IDX-Z)
      *            自費以外の計をクリアする
                   IF      WRK-COM-SRYKBN (IDX-Z)   =   "95"  OR "96"
                       CONTINUE
                   ELSE
                       MOVE    ZERO            TO  WRK-COM-KEI(IDX-Z)
                       MOVE    ZERO            TO  WRK-COM-JIHIMONEY
                                                               (IDX-Z)
                    END-IF
               END-IF
      *        回数編集
               IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "."
                   CONTINUE
               ELSE
                   IF     (SPA-COM-KAIFLG (IDX)    =   "1" )
                      AND (WRK-KAISU               =   ZERO)
                        MOVE    SPA-COM-KAISU (IDX) TO  WRK-KAISU
      *                 外用薬の時、入力回数を
                       IF     (SPA-COM-SRYKBN(IDX) =  "23"  )  AND
                              (SPA-COM-KAISU2(IDX) >   ZERO ) AND
                              (SPA-COM-KAISU (IDX) =   1    )
                           MOVE    SPA-COM-KAISU2 (IDX) TO  WRK-KAISU
                       END-IF
                   END-IF
               END-IF
      *        入院の回数合計
               IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "*"
                   ADD     SPA-COM-NYU-KAI (IDX)
                                               TO  WRK-KAISU-GOK
               ELSE
                   ADD     WRK-KAISU           TO  WRK-KAISU-GOK
               END-IF
      *
               IF     (IDX                 =   SPA-GMN-MAX   )  OR
                      (SPA-COM-ZAIEND (IDX)
                                           =   "1"           )
      *            剤ごとの処理
                   PERFORM 5102-ZAIBETU-SEC
                   INITIALIZE                  WRK-SCR-REC
                   MOVE    ZERO                TO  IDX-Z
                   MOVE    ZERO                TO  WRK-KAISU
               END-IF
           END-PERFORM
      *
      *    早期リハ等加算（今回入力分）
           MOVE    LNK-ORCSC-GAI-FLG-G     TO  WRK-RIGAKU-TBL
      *
      *    エラーの再セットを行う
           IF      WRK-ERR-ERRCD   NOT =   SPACE
               MOVE    WRK-ERR-ERRCD       TO  SPA-ERRCD
               MOVE    WRK-ERR-ERRMSG2     TO  SPA-ERRMSG2
           END-IF
      *    警告メッセージの時、ＳＰＡの再セットを行う
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (WRK-KEI-ERRCD   NOT =   SPACE)
               MOVE    WRK-KEI-ERRCD       TO  SPA-ERRCD
               MOVE    WRK-KEI-ERRMSG2     TO  SPA-ERRMSG2
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-KIDCD           TO  SPA-KIDCD
           END-IF
      *
      *    スパ領域へ戻す
           MOVE    ZERO                TO  SPA-GMN-MAX
           INITIALIZE                  SPA-GMN-REC
           PERFORM VARYING     IDX     FROM    1   BY  1
      *************UNTIL       IDX     >   WRK-HOZ-IDX
                   UNTIL       IDX     >   WRK-GYO-MAX
               IF     (WRK-HZN-GMN-INPUTCD (IDX)   =   SPACE)  AND
                      (WRK-HZN-COM-INPUTCD (IDX)   =   SPACE)
                   CONTINUE
               ELSE
                   ADD     1                   TO  SPA-GMN-MAX
                   MOVE    WRK-HZN-GMN-TBLREC (IDX)
                                               TO  SPA-GMN-TBLREC
                                                         (SPA-GMN-MAX)
                   MOVE    WRK-HZN-COM-TBLREC (IDX)
                                               TO  SPA-COM-TBLREC
                                                         (SPA-GMN-MAX)
               END-IF
           END-PERFORM
      *
      *
           MOVE    WRK-GOKEI-9         TO  SPA-GMN-GOKEI
      *
           .
       5101-TBLSAIHEN-EXT.
           EXIT.
      *****************************************************************
      *    最終行に回数の入力があるかのチェック処理
      *****************************************************************
       51011-KAISU-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "*"
               GO      TO      51011-KAISU-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-KAISU
           PERFORM VARYING     IDX-KAI FROM    1   BY  1
                   UNTIL       IDX-KAI     >   22
               IF      FLG-OK              =   1
                   IF      SPA-GMN-INPUTCD(IDX)(IDX-KAI:1) NUMERIC
                       MOVE    1               TO  FLG-KAISU
                   END-IF
               ELSE
                   IF      SPA-GMN-INPUTCD(IDX)(IDX-KAI:1)   =   "*"
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-IF
           END-PERFORM
      *    ＊ 以降に入力のない時、＊を削除
           IF      FLG-KAISU           =   ZERO
               INSPECT SPA-GMN-INPUTCD(IDX)
                       REPLACING   FIRST "*"  BY  " "
           END-IF
      *    訂正の時、剤の最後が回数入力となっている
           IF      SPA-COM-KAIFLG (IDX)    =   "1"
               MOVE    1               TO  FLG-KAISU
           END-IF
      *
           .
       51011-KAISU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院日数がある時回数のチェック処理
      *****************************************************************
       51019-NYUIN-SANCHK-SEC             SECTION.
      *
      *    画像診断のＣＴ．ＭＲＩは２回目のコードが違うのでチェックなし
           IF     (SPA-COM-TNSSRYSYUKBN (IDX)  =   "723"
                                               OR  "724"  )  AND
                  (SPA-COM-HOUKATUTEIGENKBN(IDX)
                                               =   "201"
                                               OR  "202"
                                               OR  "203"  )  AND
                   (SPA-COM-KAISU (IDX)        =   1      )  AND
                   (SPA-COM-NYU-KAI (IDX)      <=  1      )
               GO      TO      51019-NYUIN-SANCHK-EXT
           END-IF
      *
           MOVE    SPA-COM-KAISU (IDX) TO  WRK-KAISU
           MOVE    SPA-COM-KAISU (IDX) TO  WRK-NYU-KAISU
           MOVE    SPA-SRYYMD          TO  WRK-SPA-SRYYMD
      *
           PERFORM VARYING IDX-N2      FROM    1   BY  1
                   UNTIL  (IDX-N2      >   31  )   OR
                          (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-COM-NYUINDAY(IDX IDX-N2)    >   ZERO
      *
                   MOVE    IDX-N2              TO  SPA-SRYYMD(7:2)
      *            算定履歴チェック処理
                   INITIALIZE                  ORCSC91AREA
                   MOVE    SPA-SYORIFLG        TO  LNK-SC91-SYORIFLG
                   MOVE    SPA-COM-INPUTCD (IDX)
                                               TO  LNK-SC91-SRYCD
                   MOVE    "3"                 TO  LNK-SC91-KBN
                   MOVE    IDX                 TO  LNK-SC91-GMN-IDX
                   MOVE    WRK-NYU-KAISU       TO  LNK-SC91-KAISU
                   MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC91-TEISEI-AREA
      *
                   CALL    "ORCSC91"           USING    MCPAREA
                                               ORCSC91AREA
                                               SPA-SCR-REC
                                               SPA-AREA
                                               TNS-TENSU-REC
                   IF      LNK-SC91-DAYMAXCNT  >   ZERO
                       MOVE    WRK-KAISU           TO  WRK-NYU-KAISU
                   ELSE
                       ADD     WRK-KAISU           TO  WRK-NYU-KAISU
                   END-IF
      *            警告判定
                   IF      LNK-SC91-ERRCD(1:1)     =   "K"
                       IF      SPA-COM-KZMFLG (IDX)    =   SPACE
                           MOVE    "1"             TO  SPA-COM-KZMFLG
                                                                (IDX)
                           MOVE    "2"             TO  SPA-COM-CUR
                                                                (IDX)
                       ELSE
                           MOVE    SPACE           TO  LNK-SC91-ERRCD
                           MOVE    SPACE           TO  LNK-SC91-ERRMSG
                           MOVE    SPACE           TO  SPA-COM-CUR
                                                                (IDX)
                       END-IF
                   END-IF
                   MOVE    LNK-SC91-ERRCD      TO  SPA-ERRCD
                   IF     (LNK-SC91-ERRCD  NOT =   SPACE )  AND
                          (LNK-SC91-ERRMSG     =   SPACE )
                       MOVE    IDX-N2          TO  LNK-SC91-ERRMSG(1:2)
                       MOVE    "日算定済み"    TO  LNK-SC91-ERRMSG(3:)
                   END-IF
                   MOVE    LNK-SC91-ERRMSG     TO  SPA-ERRMSG2
      *
                   IF      SPA-ERRCD           =   SPACE
      *                併用算定チェック
      *                点数マスタ編集・基本チェック
                       INITIALIZE                  ORCSC92AREA
                       MOVE    "4"                 TO  LNK-SC92-KBN
                       MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
                       MOVE    "K02"               TO  LNK-SC92-PG
                       MOVE    IDX                 TO  LNK-SC92-GMN-IDX
                       MOVE    SPA-COM-INPUTCD (IDX)
                                                   TO  LNK-SC92-SRYCD
                       MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
                       MOVE    SPA-15MIMAN-ARI     TO
                                                   LNK-SC92-15MIMAN-ARI
                       MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
                       MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
                       MOVE    SPA-LSTSRYKAMEI     TO
                                                   LNK-SC92-LSTSRYKAMEI
                       MOVE    SPA-K02-TEISEI-AREA TO
                                                   LNK-SC92-TEISEI-AREA
                       CALL    "ORCSC92"           USING
                                                   MCPAREA
                                                   ORCSC92AREA
                                                   SPA-SCR-REC
                                                   SPA-AREA
                       IF       LNK-SC92-ERRCD(6)  NOT =   SPACE
                           MOVE    LNK-SC92-ERRCD(6)   TO  SPA-ERRCD
                           MOVE    LNK-SC92-ERRMSG     TO  SPA-ERRMSG2
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "1"                 TO  SPA-COM-CUR (IDX)
           END-IF
      *
           MOVE    WRK-SPA-SRYYMD      TO  SPA-SRYYMD
           .
       51019-NYUIN-SANCHK-EXT.
           EXIT.
      *****************************************************************
      *    剤ごとの処理
      *****************************************************************
       5102-ZAIBETU-SEC             SECTION.
      *
           IF      WRK-KAISU           =   ZERO
               MOVE    1               TO  WRK-KAISU
           END-IF
      *
      *****INITIALIZE                  ORCSC00AREA
           INITIALIZE                  LNK-ORCSZAI-AREA
           MOVE    "1"                 TO  LNK-ORCSC-KBN
           MOVE    "K02"               TO  LNK-ORCSC-PG
           MOVE    WRK-KAISU           TO  LNK-ORCSC-KAISU
      *???*MOVE    WRK-KAISU-GOK       TO  LNK-ORCSC-KAISU-GOK
      **** MOVE    SPA-GMN-IDX         TO  LNK-ORCSC-GMN-IDX
           MOVE    SPA-NAI-ROUJIN      TO  LNK-ORCSC-ROUJIN 
           MOVE    SPA-NENREI          TO  LNK-ORCSC-NENREI 
           MOVE    SPA-SYOYMD          TO  LNK-ORCSC-SYOYMD 
           MOVE    SPA-LSTYMD          TO  LNK-ORCSC-LSTYMD 
           MOVE    SPA-LSTSRYKA        TO  LNK-ORCSC-LSTSRYKA 
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-ORCSC-LSTSRYKAMEI 
           MOVE    SPA-INGAIKBN        TO  LNK-ORCSC-INGAIKBN
      *****MOVE    WRK-TIMEFLG         TO  LNK-ORCSC-TIMEFLG
           MOVE    SPA-NAI-TIMEFLG     TO  LNK-ORCSC-TIMEFLG
           MOVE    SPA-NAI-GAIFLG      TO  LNK-ORCSC-GAIFLG
           MOVE    SPA-NAIACCT-AREA    TO  LNK-NAIACCT-AREA
           MOVE    SPA-NAIACCT2-AREA   TO  LNK-NAIACCT2-AREA
           MOVE    FLG-TOROKU          TO  LNK-ORCSC-TOROKU
      *    処理フラグ
           MOVE    SPA-SYORIFLG        TO  LNK-ORCSC-SYORIFLG
      *    初診フラグ
           MOVE    SPA-SYOSIN          TO  LNK-ORCSC-SYOSIN2
      *    初診フラグ（初診で、履歴の無い時、’１’）
      *    IF      (SPA-SYOSIN         =   1   )  AND
      *            (SPA-GMN-MAX1       =   ZERO)  AND
      *            (SPA-GMN-SYOSINYMD  =   SPACE)
      *        MOVE    1                   TO  LNK-ORCSC-SYOSIN
      *    ELSE
               MOVE    ZERO                TO  LNK-ORCSC-SYOSIN
      *    END-IF
      *
           MOVE    SPA-CHOKI-FLG       TO  LNK-ORCSC-CHOKI-FLG
           MOVE    SPA-YAKJYO-1        TO  LNK-ORCSC-YAKJYO-1
           MOVE    SPA-YAKJYO-2        TO  LNK-ORCSC-YAKJYO-2
           MOVE    SPA-YAKJYO-3        TO  LNK-ORCSC-YAKJYO-3
           MOVE    SPA-TOKUTEI-FLG     TO  LNK-ORCSC-TOKUTEI-FLG
      *
           MOVE    SPA-ROUMANSEI-ARI   TO  LNK-ORCSC-ROUMANSEI-ARI
           MOVE    SPA-ROUMANSEI-ARI2  TO  LNK-ORCSC-ROUMANSEI-ARI2
           MOVE    SPA-NETAKIRI-ARI    TO  LNK-ORCSC-NETAKIRI-ARI
           MOVE    SPA-NETAKIRI-ARI2   TO  LNK-ORCSC-NETAKIRI-ARI2
           MOVE    SPA-SYONIKA-ARI     TO  LNK-ORCSC-SYONIKA-ARI
           MOVE    SPA-SYONIKA-ARI2    TO  LNK-ORCSC-SYONIKA-ARI2
      *    生活習慣病指導管理料
           MOVE    SPA-SEIKATU-ARI     TO  LNK-ORCSC-SEIKATU-ARI
           MOVE    SPA-SEIKATU-ARI2    TO  LNK-ORCSC-SEIKATU-ARI2
      *    複数診療科あり
           MOVE    SPA-FUKU-SRYKA      TO  LNK-ORCSC-FUKU-SRYKA
      *    訂正前診療科
           MOVE    SPA-OLD-SRYKA       TO  LNK-ORCSC-OLD-SRYKA
           MOVE    SPA-OLD-HKNCOMBI    TO  LNK-ORCSC-OLD-HKNCOMBI
      *    時間外特例区分
           MOVE    SPA-JIKANTOKU       TO  LNK-ORCSC-JIKANTOKU
      *    時間外加算特例乳幼児
           MOVE    SPA-JIKANTOKU-NYU   TO  LNK-ORCSC-JIKANTOKU-NYU
      *
      *    電話再診の有無
           MOVE    SPA-TELSAI-ARI      TO  LNK-ORCSC-TELSAI-ARI
      *
           MOVE    SPA-HOUMON-ARI      TO  LNK-ORCSC-HOUMON-ARI
      *    特定疾患あり
           MOVE    SPA-TOKUTEI         TO  LNK-ORCSC-TOKUTEI
           MOVE    SPA-TOKUTEIFLG      TO  LNK-ORCSC-TOKUTEIFLG
      *    診察回数
           MOVE    SPA-SINSATU-CNT     TO  LNK-ORCSC-SINSATU-CNT
      *   １５歳未満再診科算定有り
           MOVE    SPA-15MIMAN-ARI     TO  LNK-ORCSC-15MIMAN-ARI
      *    特定薬剤治療管理加算の初回算定日
           MOVE    SPA-TOKTEI-YMD      TO  LNK-ORCSC-TOKTEI-YMD
      *    厚生省が定める患者
           MOVE    SPA-TOKUTEI-KNJ     TO  LNK-ORCSC-TOKUTEI-KNJ
           MOVE    SPA-TOKUTEI-KNJ2    TO  LNK-ORCSC-TOKUTEI-KNJ2
      *    人工腎臓患者
           MOVE    SPA-TOKUTEI-JNZ     TO  LNK-ORCSC-TOKUTEI-JNZ
      *    厚生省が定める患者
           MOVE    SPA-TOKUTEIG-KNJ    TO  LNK-ORCSC-TOKUTEIG-KNJ
           MOVE    SPA-TOKUTEIG-KNJ2   TO  LNK-ORCSC-TOKUTEIG-KNJ2
      *    消炎鎮痛処置当月内算定
           MOVE    WRK-SYOEN-CNT       TO  LNK-ORCSC-SYOEN-CNT
      *    急性発生１８０日以内
           MOVE    SPA-KYUSEI-90       TO  LNK-ORCSC-KYUSEI-90
           MOVE    SPA-KYUSEI-180      TO  LNK-ORCSC-KYUSEI-180
           MOVE    SPA-KYUSEI-FLG      TO  LNK-ORCSC-KYUSEI-FLG
      *    当月外来管理加算の取れない患者
           MOVE    SPA-GAIRAI-NASI     TO  LNK-ORCSC-GAIRAI-NASI
      *    再診チェック済
           MOVE    SPA-SAISIN-CHK      TO  LNK-ORCSC-SAISIN-CHK
      *    理学療法当月内算定
      *****MOVE    WRK-RIGAKU-TBL      TO  LNK-ORCSC-RIGAKU-TBL
      *    MOVE    ORCSC80CNT-RIGAKU-AREA
      *                                TO  LNK-ORCSC-RIGAKU-TBL
      *    早期リハ等加算（今回入力分）
      *****MOVE    WRK-RIGAKU-TBL      TO  LNK-ORCSC-GAI-FLG-G
      *    労災用 外来管理点数
           MOVE    SPA-GAIRAI-TEN      TO  LNK-ORCSC-GAIRAI-TEN
      *
      *    継続領域
           MOVE    SPA-CONTKBN         TO  LNK-ORCSC-CONTKBN
           MOVE    SPA-DENPNUM         TO  LNK-ORCSC-DENPNUM
           IF      SPA-SYORIFLG        =   1
               MOVE    SPA-RENNUM          TO  LNK-ORCSC-DOUJI-RENNUM
           ELSE
               MOVE    SPA-DOUJI-RENNUM    TO  LNK-ORCSC-DOUJI-RENNUM
           END-IF
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-ORCSC-TEISEI-AREA
      *
      *    入院期間
           MOVE    SPA-K02N-NYUINYMD   TO  LNK-ORCSC-NYUINYMD
           MOVE    SPA-K02N-TAIINYMD   TO  LNK-ORCSC-TAIINYMD
      *    リハビリ逓減確認
           MOVE    FLG-RIHTEI          TO  LNK-ORCSC-FLG-RIHTEI
      *
           MOVE    SPA-SRYYMD          TO  WRK-SPA-SRYYMD
      *    入院日の最後の日を診療日とする
           PERFORM VARYING IDX-N2      FROM    1   BY  1
                   UNTIL   IDX-N2      >   31
               IF      SPA-COM-NYUINDAY(IDX IDX-N2)    >   ZERO
                   MOVE    IDX-N2              TO  SPA-SRYYMD(7:2)
               END-IF
           END-PERFORM
           PERFORM 51021-ZAIBETU-CHK-SEC
           MOVE    WRK-SPA-SRYYMD      TO  SPA-SRYYMD
           .
       5102-ZAIBETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤別のチェック処理
      *****************************************************************
       51021-ZAIBETU-CHK-SEC          SECTION.
      *
      *
           EVALUATE    WRK-COM-SRYKBN (01)
      *            診療
               WHEN    "11"
               WHEN    "12"
               WHEN    "13"
               WHEN    "14"
                   CALL    "ORCSC10"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (01)
      *            初診フラグ
                   MOVE    LNK-ORCSC-SYOSIN2   TO  SPA-SYOSIN
      *            診察の時間加算の時、全体へ
                   MOVE    LNK-ORCSC-TIMEFLG   TO  WRK-TIMEFLG
      *
      *            小児科外来診療科の時、年齢エラーはそのまま
                   IF      SPA-SYONIKA-ARI     NOT =   ZERO
                       IF      SPA-ERRCD           =   "0032"
                           MOVE    SPACE               TO  SPA-ERRCD
                       END-IF
                   END-IF
      *            特定薬剤治療管理加算の初回算定日
                   MOVE    LNK-ORCSC-TOKTEI-YMD
                                           TO  SPA-TOKTEI-YMD
      *            厚生省が定める患者
                   MOVE    LNK-ORCSC-TOKUTEI-KNJ
                                           TO  SPA-TOKUTEI-KNJ
      *            人工腎臓患者
                   MOVE    LNK-ORCSC-TOKUTEI-JNZ
                                           TO  SPA-TOKUTEI-JNZ
      *            厚生省が定める患者
                   MOVE    LNK-ORCSC-TOKUTEIG-KNJ
                                           TO  SPA-TOKUTEIG-KNJ
      *            労災用 外来管理点数
      *            MOVE    LNK-ORCSC-GAIRAI-TEN
      *                                    TO  SPA-GAIRAI-TEN
      *
      *            薬剤
               WHEN    "21"
               WHEN    "22"
               WHEN    "23"
               WHEN    "25"
      *
               WHEN    "24"
               WHEN    "26"
               WHEN    "27"
                   CALL    "ORCSC20"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (02)
      *            注射
               WHEN    "31"
               WHEN    "32"
               WHEN    "33"
                   CALL    "ORCSC30"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (03)
      *            処置
               WHEN    "40"
                   CALL    "ORCSC40"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (04)
      *            消炎鎮痛処置当月内算定
                   MOVE    LNK-ORCSC-SYOEN-CNT
                                           TO  WRK-SYOEN-CNT
      *            手術
               WHEN    "50"
                   CALL    "ORCSC50"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (05)
      *            麻酔
               WHEN    "54"
                   CALL    "ORCSC54"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (06)
      *            検査
               WHEN    "60"
                   CALL    "ORCSC60"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (07)
      *            画像診断
               WHEN    "70"
                   CALL    "ORCSC70"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (08)
      *            その他
               WHEN    "80"
                   CALL    "ORCSC80"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (09)
      *****        理学療法当月内算定
      *            MOVE    LNK-ORCSC-RIGAKU-TBL
      *                                    TO  ORCSC80CNT-RIGAKU-AREA
      *            早期リハ等加算（今回入力分）
      *            MOVE    LNK-ORCSC-GAI-FLG-G
      *****                                TO  WRK-RIGAKU-TBL
      *            コメント等
      *********WHEN    "90"
               WHEN    "93"
               WHEN    "99"
                   PERFORM 510211-SRYKBN-90-SEC
      *            自費
               WHEN    "95"
               WHEN    "96"
                   CALL    "ORCSCJIH"      USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
      *            入院（食事）
               WHEN    "90"
               WHEN    "92"
               WHEN    "97"
                   CALL    "ORCSCN97"      USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (10)
           END-EVALUATE
      *
      *****MOVE    LNK-ORCSC-GMN-IDX   TO  SPA-GMN-IDX
           MOVE    LNK-ORCSC-KIDCD     TO  SPA-KIDCD
           MOVE    LNK-ORCSC-CHK-IDX   TO  SPA-CHK-IDX
           MOVE    LNK-ORCSC-ERRMSG    TO  SPA-ERRMSG2
      *
      *    エラーコードを保存する
           IF      SPA-ERRCD       NOT =   SPACE
               IF      SPA-ERRCD(1:1)      =   "K"
      *            既にエラーがある時、フラグをクリアする
                   IF     (WRK-ERR-ERRCD   NOT =   SPACE )  OR
                          (WRK-KEI-ERRCD   NOT =   SPACE )  OR
                          (WRK-KIDCD       NOT =   SPACE )
                       MOVE    SPACE           TO  WRK-COM-KZMFLG
                                                    (LNK-ORCSC-KEI-IDX)
                   ELSE
                       MOVE    SPA-ERRCD       TO  WRK-KEI-ERRCD
                       MOVE    SPA-ERRMSG2     TO  WRK-KEI-ERRMSG2
                   END-IF
               ELSE
                   IF      WRK-ERR-ERRCD       =   SPACE
                       MOVE    SPA-ERRCD       TO  WRK-ERR-ERRCD
                       MOVE    SPA-ERRMSG2     TO  WRK-ERR-ERRMSG2
                   END-IF
               END-IF
               MOVE    SPACE           TO  SPA-ERRCD
           END-IF
      *
      *    既にエラーがある時、フラグをクリアする
           IF    ( SPA-KIDCD       NOT =   SPACE )  AND
                 ((WRK-ERR-ERRCD   NOT =   SPACE )  OR
                  (WRK-KEI-ERRCD   NOT =   SPACE )  OR
                  (WRK-KIDCD       NOT =   SPACE ))
               MOVE    SPACE           TO  WRK-COM-KZMFLG
                                                    (LNK-ORCSC-KEI-IDX)
           END-IF
           IF     (SPA-KIDCD       NOT =   SPACE )  AND
                  (WRK-KIDCD           =   SPACE )
               MOVE    SPA-KIDCD           TO  WRK-KIDCD
           END-IF
      *
           MOVE    ZERO                TO  WRK-MAX
      *
      *    編集後内容セット
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   WRK-GYO-MAX  )  OR
                             ((WRK-GMN-INPUTCD (IDX-Z) =   SPACE ) AND
                              (WRK-COM-INPUTCD (IDX-Z) =   SPACE ))
      *
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
               IF     (WRK-COM-INPUTCD(IDX-Z)(1:1) =   "8" )  OR
                      (WRK-COM-INPUTCD(IDX-Z)(1:3) =   "008")
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
               IF      ( IDX-Z             =   WRK-GYO-MAX )  OR
                       ((WRK-COM-INPUTCD(IDX-Z + 1) =   SPACE ) AND
                        (WRK-GMN-INPUTCD(IDX-Z + 1) =   SPACE ))
                   MOVE    "1"             TO  WRK-COM-ZAIEND(IDX-Z)
                   MOVE    IDX-Z           TO  WRK-MAX
               ELSE
                   MOVE    SPACE           TO  WRK-COM-ZAIEND(IDX-Z)
               END-IF
      *        回数
               IF      LNK-ORCSC-KAISU NOT =   WRK-COM-KAISU (IDX-Z)
                   MOVE    LNK-ORCSC-KAISU     TO  WRK-COM-KAISU(IDX-Z)
               END-IF
      *
      *        外用の回数
               IF      WRK-COM-SRYKBN(IDX-Z)   =   "23"
                   MOVE    WRK-KAISU           TO  WRK-COM-KAISU2(IDX-Z)
               END-IF
      *        最終行に回数入力（＊）が有った時、最終行を回数入力
               IF      FLG-KAISU           NOT =   ZERO
                   IF      WRK-COM-ZAIEND (IDX-Z)  =   SPACE
                       IF      WRK-COM-KAIFLG (IDX-Z)  =   "1"
                           MOVE    SPACE           TO  WRK-COM-KAIFLG
                                                               (IDX-Z)
                           PERFORM 51021-KAISU-SAIHEN-SEC
                       END-IF
                   ELSE
      *                最終行
                       IF      WRK-COM-KAIFLG (IDX-Z)  =   SPACE
                           PERFORM 51021-KAISU-SAIHEN-SEC
                           MOVE    "1"             TO  WRK-COM-KAIFLG
                                                               (IDX-Z)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
      *    点数再セット
      *    診察日を戻す
           MOVE    WRK-SPA-SRYYMD      TO  SPA-SRYYMD
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL       IDX-Z       >   WRK-MAX
      *
               IF     (WRK-GMN-INPUTCD(IDX-Z)(1:1) =   "*"  )  OR
                      (WRK-COM-KAIFLG (IDX-Z)      =   "1"  )
                   IF      IDX-Z           NOT =   WRK-MAX
      *            各点数編集
                       MOVE    WRK-COM-TENSU (WRK-MAX)
                                           TO  WRK-COM-TENSU (IDX-Z)
                       MOVE    WRK-COM-SYUTEN1 (WRK-MAX)
                                           TO  WRK-COM-SYUTEN1 (IDX-Z)
                       MOVE    WRK-COM-SYUTEN2 (WRK-MAX)
                                           TO  WRK-COM-SYUTEN2 (IDX-Z)
                       MOVE    WRK-COM-YKZTEN (WRK-MAX)
                                           TO  WRK-COM-YKZTEN (IDX-Z)
                       MOVE    WRK-COM-KIZAITEN (WRK-MAX)
                                           TO  WRK-COM-KIZAITEN(IDX-Z)
                   END-IF
      *            計
      *            自費の時,計
                   IF      WRK-COM-SRYSYUKBN(IDX-Z)(1:2)
                                               =   "95" OR "96"
                       CONTINUE
                   ELSE
                       COMPUTE WRK-COM-KEI (IDX-Z)
                                               =   WRK-COM-TENSU(IDX-Z)
                                               *   WRK-COM-KAISU(IDX-Z)
                       MOVE    WRK-COM-KEI (IDX-Z)
                                               TO  WRK-GMN-KEI  (IDX-Z)
                   END-IF
               ELSE
      *            回数
                   IF      LNK-ORCSC-KAISU NOT =   WRK-COM-KAISU (IDX-Z)
                       MOVE    LNK-ORCSC-KAISU     TO  WRK-COM-KAISU
                                                                (IDX-Z)
                   END-IF
               END-IF
      *
      *        約束セット行の編集
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1) =   "S"
                   MOVE    LNK-ORCSC-ZAITEN        TO  WRK-COM-TENSU
                                                                (IDX-Z)
                   MOVE    LNK-ORCSC-ZAIKEI        TO  WRK-COM-KEI
                                                               (IDX-Z)
                   MOVE    LNK-ORCSC-KAISU         TO  WRK-COM-KAISU
                                                               (IDX-Z)
               END-IF
      *        画面再編集・基本チェック
               INITIALIZE                  ORCSC94AREA
               MOVE    "2"                 TO  LNK-SC94-KBN
               MOVE    "K02"               TO  LNK-SC94-PG
               MOVE    IDX-Z               TO  LNK-SC94-IDX
               MOVE    SPA-SYORIFLG        TO  LNK-SC94-SYORIFLG
               CALL    "ORCSC94"           USING
                                           ORCSC94AREA
                                           WRK-SCR-REC
                                           SPA-AREA
      *
               ADD     1                   TO  WRK-HOZ-IDX
               IF      WRK-HOZ-IDX         >   WRK-GYO-MAX
                   IF      SPA-SYORIFLG        =   1
                       MOVE    "9100"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "0007"          TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    WRK-COM-TBLREC (IDX-Z)
                                           TO  WRK-HZN-COM-TBLREC
                                                       (WRK-HOZ-IDX)
                   MOVE    WRK-GMN-TBLREC (IDX-Z)
                                           TO  WRK-HZN-GMN-TBLREC
                                                       (WRK-HOZ-IDX)
               END-IF
      *
           END-PERFORM
           .
       51021-ZAIBETU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    回数の表示変更処理
      *****************************************************************
       51021-KAISU-SAIHEN-SEC          SECTION.
      *
           IF      WRK-COM-ZAIEND (IDX-Z)  =   SPACE
      *        *　以降を削除
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDX-KAI FROM    1   BY  1
                       UNTIL      (IDX-KAI     >   22  )   OR
                                  (FLG-OK      =   1   )
                   IF      WRK-GMN-INPUTCD(IDX-Z)(IDX-KAI:1)   =   "*"
                       MOVE    IDX-KAI         TO  IDX-KAI2
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-PERFORM
               IF      FLG-OK          =   1
                   MOVE    SPACE           TO  WRK-GMN-INPUTCD(IDX-Z)
                                                   (IDX-KAI2:)
               END-IF
           ELSE
      *
      *        * を追加
               MOVE    WRK-COM-KAISU(IDX-Z)    TO  WRK-KAISU-Z
               MOVE    ZERO                TO  IDK2
               MOVE    SPACE               TO  WRK-KAISU-H 
               PERFORM VARYING     IDK1    FROM    1   BY  1
                       UNTIL       IDK1    >   4
                   IF      WRK-KAISU-X(IDK1:1)  NOT =   SPACE
                       ADD     1           TO  IDK2
                       MOVE    WRK-KAISU-X(IDK1:1)
                                           TO  WRK-KAISU-H (IDK2:1)
                   END-IF
               END-PERFORM
      *
               MOVE    WRK-GMN-INPUTCD (IDX-Z) TO  WRK-HEN-INPUTCD
               MOVE    SPACE                   TO  WRK-GMN-INPUTCD
                                                            (IDX-Z)
      *
               STRING  WRK-HEN-INPUTCD         DELIMITED   BY  SPACE
                       "*"                     DELIMITED   BY  SIZE
                       WRK-KAISU-H             DELIMITED   BY  SPACE
                       INTO                    WRK-GMN-INPUTCD(IDX-Z)
               END-STRING
      *
           END-IF
           .
       51021-KAISU-SAIHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌チェック処理
      *****************************************************************
       51010-KNKCHK-SEC                SECTION.
      *
      *
           MOVE    ZERO                TO  FLG-KINKI
           MOVE    ZERO                TO  IDX-K1
           INITIALIZE                  WRK-KNKCHK-AREA
      *    相互作用チェック期間
           IF      SPA-INTERACT-KIKAN  =   ZERO
                GO      TO      51010-KNKCHK-EXT
           END-IF
           MOVE    1                   TO  FLG-ALLFLG
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   WRK-GYO-MAX      )   OR
                              (FLG-KINKI   NOT =   ZERO )   OR
                              (SPA-GMN-INPUTCD(IDX-Z)  =   SPACE  AND
                               SPA-COM-INPUTCD(IDX-Z)  =   SPACE )
      *******  IF     (SPA-COM-SRYKBN  (IDX-Z)     =   "21" OR  "22"
      *                                            OR  "23" OR  "31"
      *                                            OR  "32" OR  "33"
      *******                                      OR  "95" OR  "96")
               IF     (SPA-COM-INPUTCD (IDX-Z)(1:1)    =  "6" )
      *            禁忌薬剤チェック処理
                   IF      (SPA-COM-KNKFLG (IDX-Z)  =   SPACE )  AND
                           (SPA-COM-KNKUMU (IDX-Z)  =   SPACE )
                       PERFORM 510222-KNKYAK-CHK-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
      *    初期画面の時、エラー画面なし
           IF      FLG-KINKI           =   1
               IF      FLG-TOROKU          =   "2"
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-ALLFLG
                   PERFORM 51020-K021-SYORI-SEC
               END-IF
           END-IF
           .
       51010-KNKCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌薬剤チェック処理
      *****************************************************************
       510222-KNKYAK-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-KINKI-ARI2
      *    相互作用レコードより、禁忌有無のチェックを行う
           INITIALIZE                  INTERACT-REC
           MOVE    SPA-COM-INPUTCD (IDX-Z)
                                       TO  INTERACT-DRUGCD
      *
           MOVE    ZERO                TO  FLG-INTERACT
           MOVE    INTERACT-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "INTERACT-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "INTERACT-KEY2"     TO  ORC-DBPATH
               PERFORM 990-INTERACT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INTERACT
           END-IF
      *    禁忌なし
           IF      FLG-INTERACT          =   1
               MOVE    "1"             TO  SPA-COM-KNKUMU (IDX-Z)
           END-IF
      *
      *    禁忌チェック
           MOVE    ZERO                TO  IDX-K2
           ADD     1                   TO  IDX-K1
           PERFORM
                   UNTIL      FLG-INTERACT     =   1
               PERFORM 5102221-KNKCHK-SYORI-SEC
      *
               MOVE    "INTERACT-KEY2"     TO  ORC-DBPATH
               PERFORM 990-INTERACT-READ-SEC
           END-PERFORM
           MOVE    "INTERACT-KEY2"     TO  ORC-DBPATH
           PERFORM 990-DB-CLOSE-SEC
      *
           IF      IDX-K2              >   ZERO
               MOVE    "1"                 TO  SPA-COM-KNKFLG (IDX-Z)
               MOVE    SPA-GMN-MEISYO (IDX-Z)
                                           TO  WRK-KNK-TOUMEI (IDX-K1)
               MOVE    SPA-COM-INPUTCD(IDX-Z)
                                           TO  WRK-KNK-TOUSRYCD (IDX-K1)
               MOVE    1                   TO  FLG-KINKI
           ELSE
               COMPUTE IDX-K1          =   IDX-K1   -   1
           END-IF
      *
           .
       510222-KNKYAK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌薬剤チェック処理
      *****************************************************************
       5102221-KNKCHK-SYORI-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-KINKI-ARI
      *    診療会計マスタとのチェック
           PERFORM VARYING     IDX-S       FROM    1   BY  1
                   UNTIL      (IDX-S           >   100 )  OR
                              (FLG-KINKI-ARI   =   1   ) OR
                              (IDX-K2          >   30  )  OR
                              (SPA-KNK-SRYCD (IDX-S)   =   SPACE )
               IF    ( SPA-KNK-SRYCD (IDX-S)   =   INTERACT-DRUGCD  OR
                                                   INTERACT-DRUGCD2) AND
                     ( SPA-COM-INPUTCD (IDX-Z)
                                           NOT =   SPA-KNK-SRYCD(IDX-S))
                   MOVE    1                   TO  FLG-KINKI-ARI
                   MOVE    1                   TO  FLG-KINKI-ARI2
                   ADD     1                   TO  IDX-K2
                   MOVE    SPA-KNK-SRYCD (IDX-S)   TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
                   IF      FLG-TENSU           =   1
                       MOVE    SPA-KNK-SRYCD (IDX-S)   TO  TNS-SRYCD
                       PERFORM 919-TENSU-READ2-SEC
                   END-IF
                   MOVE    TNS-NAME                TO  WRK-KNK-KNKMEI
                                                        (IDX-K1 IDX-K2)
                   MOVE    SPA-KNK-SRYCD (IDX-S)   TO  WRK-KNK-SRYCD
                                                        (IDX-K1 IDX-K2)
                   MOVE    SPA-KNK-TOUYMD (IDX-S)  TO  WRK-KNK-TOUYMD
                                                        (IDX-K1 IDX-K2)
               END-IF
           END-PERFORM
      *
      *    画面入力分とのチェック
           IF      FLG-ALLFLG          =   1
               MOVE    1                   TO  IDX-K3
               COMPUTE IDX-K4              =   IDX-Z   -   1
           ELSE
               COMPUTE IDX-K3              =   IDX-Z   +   1
               MOVE    WRK-GYO-MAX             TO  IDX-K4
           END-IF
           PERFORM VARYING     IDX-S       FROM    IDX-K3  BY  1
                   UNTIL      (IDX-S           >   IDX-K4 )  OR
                              (FLG-KINKI-ARI   =   1    ) OR
                              (SPA-GMN-INPUTCD(IDX-S)  =   SPACE  AND
                               SPA-COM-INPUTCD(IDX-S)  =   SPACE )
               IF    ((SPA-COM-INPUTCD (IDX-S) =   INTERACT-DRUGCD)  OR
                      (SPA-COM-INPUTCD (IDX-S) =   INTERACT-DRUGCD2))
                 AND  (SPA-COM-INPUTCD (IDX-S)
                                           NOT =   SPA-COM-INPUTCD
                                                           (IDX-Z))
                 AND  (IDX-S               NOT =   IDX-Z          )
                   ADD     1                   TO  IDX-K2
                   MOVE    SPA-GMN-MEISYO (IDX-S)  TO  WRK-KNK-KNKMEI
                                                        (IDX-K1 IDX-K2)
                   MOVE    SPA-COM-INPUTCD (IDX-S) TO  WRK-KNK-SRYCD
                                                        (IDX-K1 IDX-K2)
                   MOVE    SPA-NAI-SRYYMD          TO  WRK-KNK-TOUYMD
                                                        (IDX-K1 IDX-K2)
                   MOVE    SPA-GMN-SRYYMD(1:9)     TO  WRK-KNK-TOUYMDW
                                                        (IDX-K1 IDX-K2)
      *
                   MOVE    1                       TO  FLG-KINKI-ARI
                   MOVE    1                       TO  FLG-KINKI-ARI2
               END-IF
           END-PERFORM
      *
           IF      FLG-KINKI-ARI       =   1
               MOVE    INTERACT-SYOJYOUCD  TO  WRK-KNK-SYOJYOUCD
                                                        (IDX-K1 IDX-K2)
           END-IF
      *
           .
       5102221-KNKCHK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *     コメント等　編集処理
      *****************************************************************
       510211-SRYKBN-90-SEC            SECTION.
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   WRK-GYO-MAX  )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
               IF      WRK-COM-KISOTEN (IDX-Z) =   ZERO
                   MOVE    ZERO            TO  WRK-COM-KEI (IDX-Z)
                   MOVE    1               TO  WRK-COM-KAISU(IDX-Z)
                   MOVE    1               TO  WRK-COM-SURYO(IDX-Z)
                   MOVE    ZERO            TO  WRK-COM-TENSU(IDX-Z)
                   MOVE    SPACE           TO  WRK-GMN-TENSU(IDX-Z)
                   MOVE    SPACE           TO  WRK-GMN-SURYO(IDX-Z)
                   MOVE    SPACE           TO  WRK-GMN-KAISU(IDX-Z)
                   MOVE    ZERO            TO  WRK-GMN-KEI (IDX-Z)
               END-IF
      *        他保険受診済みが存在する
               EVALUATE    SPA-COM-INPUTCD (IDX-Z)
                   WHEN    "099999902"
                       MOVE    1                   TO  SPA-TAHOKEN-FLG
               END-EVALUATE
           END-PERFORM
      *
           .
       510211-SRYKBN-90-EXT.
           EXIT.
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
      *    診療行為名入力処理
      *****************************************************************
       40113-MEISYOU-CHK-SEC        SECTION.
      *
      *    IF      SPA-GMN-MEISYO-G (SPA-GMN-IDX)
      *                                =   SPACE
      *        GO      TO      40113-MEISYOU-CHK-EXT
      *    END-IF
      *
           IF      SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)  =  "81"
               MOVE    WRK-MEISYO-G        TO  SPA-GMN-MEISYO-G
                                                         (SPA-GMN-IDX)
               MOVE    "1"                 TO  SPA-COM-MEIFLG
                                                       (SPA-GMN-IDX)
           END-IF
           IF     (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)  = "83" OR
                                                         "84")  OR
                  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)  = "0083" OR
                                                         "0084")
               IF     (LNK-SC97-NYURYOKU   =   ZERO )  AND
                      (FLG-IN-ATAI         =   ZERO )  AND
                     ((SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)
                                           = "83"   ) OR
                      (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)
                                           = "0083"  ))
                   MOVE    WRK-MEISYO-G        TO  SPA-GMN-MEISYO-G
                                                         (SPA-GMN-IDX)
               END-IF
               MOVE    "1"             TO  SPA-COM-MEIFLG
                                                       (SPA-GMN-IDX)
           END-IF
           IF    ((SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)  =  "81"
                                                       OR "83"
                                                       OR "84") OR
                  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)  =  "0083"
                                                       OR "0084")) AND
                  (SPA-GMN-MEISYO-G(SPA-GMN-IDX) NOT =   SPACE)
               CONTINUE
           ELSE
               GO      TO      40113-MEISYOU-CHK-EXT
           END-IF
      *
      *    半角の空白を全角に置きかえる
           PERFORM 401131-MEISYO-HENKAN-SEC
      *
           .
       40113-MEISYOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    名称チェック・全角置換処理
      *****************************************************************
       401131-MEISYO-HENKAN-SEC        SECTION.
      *
      *    フリーコメントは１桁目から
           IF      SPA-COM-INPUTCD(SPA-GMN-IDX)   =   "810000001"
               MOVE    1                   TO  IDX-STR
           ELSE
               MOVE    3                   TO  IDX-STR
           END-IF
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-MEISYO-G (SPA-GMN-IDX)(IDX-STR:)
                                       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               IF      KANACHK-RC2         =   7
                   MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-MEISYO-G
                                                       (SPA-GMN-IDX)
                                                            (IDX-STR:)
               END-IF
               MOVE    "0080"          TO  SPA-ERRCD
               MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
               MOVE    2               TO  SPA-GMN-CUR
           ELSE
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-MEISYO-G
                                                       (SPA-GMN-IDX)
                                                            (IDX-STR:)
           END-IF
      *
           .
       401131-MEISYO-HENKAN-EXT.
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
                   MOVE    WRK-COM-TBLREC (IDY) 
                                           TO  SPA-COM-TBLREC(IDX)
                   MOVE    WRK-GMN-TBLREC (IDY) 
                                           TO  SPA-GMN-TBLREC(IDX)
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
      *
      *****************************************************************
      *    剤削除処理
      *****************************************************************
       41015-ZAIDEL-SEC              SECTION.
      *
      *****IF      (SPA-GMN-IDX        =   1   )  OR
      *            (SPA-COM-ZAIEND(SPA-GMN-IDX - 1 )
      *                                =   "1" )
      *        CONTINUE
      *    ELSE
      *        MOVE    "0023"              TO  SPA-ERRCD
      *        MOVE    "1"                 TO  SPA-COM-CUR(SPA-GMN-IDX)
      *        GO      TO      41015-ZAIDEL-EXT
      **** END-IF
      *
      *    ’−’は剤内のどの位置でも削除とする
      *    対象より、前
           IF      (SPA-GMN-IDX        =   1   )  OR
                   (SPA-COM-ZAIEND(SPA-GMN-IDX - 1 )
                                       =   "1" )
               CONTINUE
           ELSE
               PERFORM VARYING     IDX     FROM    SPA-GMN-IDX   BY  -1
                       UNTIL       IDX     <   1
                   IF      IDX             <   SPA-GMN-IDX
                       IF      SPA-COM-ZAIEND(IDX)     =   "1"
                          MOVE    1               TO  IDX
                       ELSE
                           MOVE    SPACE       TO  SPA-GMN-INPUTCD(IDX)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *    後ろ
           PERFORM VARYING     IDX     FROM    SPA-GMN-IDX   BY  1
                   UNTIL      (IDX     >   WRK-GYO-MAX )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE)
      *
      *        セットが削除の時セット内容を削除する
               IF     (SPA-MAE-INPUTCD(IDX)(1:1)   =   "S") OR
                     ((IDX                     <   WRK-GYO-MAX )  AND
                      (SPA-COM-SET  (IDX + 1)  =   "1"))
                   MOVE    SPACE           TO  SPA-GMN-INPUTCD(IDX)
                   PERFORM 410151-SETDEL2-SEC
                   MOVE    200             TO  IDX
               ELSE
                   MOVE    SPACE           TO  SPA-GMN-INPUTCD(IDX)
                   IF    ((IDX                 <   WRK-GYO-MAX ) AND
                          (SPA-GMN-SRYKBN (IDX + 1)
                                               NOT =   SPACE) ) OR
                         (SPA-COM-ZAIEND(IDX)  =   "1"    )
                       MOVE    WRK-GYO-MAX             TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
      *    PERFORM VARYING     IDX     FROM    SPA-GMN-IDX   BY  1
      *            UNTIL      (IDX     >   WRK-GYO-MAX )  OR
      *                       (SPA-GMN-INPUTCD (IDX)   =   SPACE)
      *        MOVE    SPACE           TO  SPA-GMN-INPUTCD(IDX)
      *        IF      (IDX            <   WRK-GYO-MAX    ) AND
      *                (SPA-GMN-SRYKBN (IDX + 1)    NOT =   SPACE )
      *            MOVE    WRK-GYO-MAX             TO  IDX
      *        END-IF
      *    END-PERFORM
      *
           .
       41015-ZAIDEL-EXT.
           EXIT.
      *****************************************************************
      *    セット削除処理（剤削除）
      *****************************************************************
       410151-SETDEL2-SEC              SECTION.
      *
           COMPUTE IDX2                =   IDX     +   1
      *
           PERFORM VARYING     IDX3    FROM    IDX2    BY  1
                   UNTIL      (IDX3    >   WRK-GYO-MAX )  OR
                              (SPA-GMN-INPUTCD (IDX3)  =   SPACE)
               IF      SPA-COM-SET     (IDX3)   =   "1"
                   MOVE    SPACE           TO  SPA-GMN-INPUTCD(IDX3)
                   ADD     1               TO  SPA-GMN-IDX
               ELSE
                   MOVE    200             TO  IDX3
               END-IF
           END-PERFORM
      *
           .
       410151-SETDEL2-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    セット削除処理
      *****************************************************************
       41016-SETDEL-SEC              SECTION.
      *
           COMPUTE IDX2                =   SPA-GMN-IDX   +   1
      *
           PERFORM VARYING     IDX     FROM    IDX2    BY  1
                   UNTIL      (IDX     >   WRK-GYO-MAX )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE)
               IF      SPA-COM-SET     (IDX)   =   "1"
                   MOVE    SPACE               TO  SPA-GMN-INPUTCD(IDX)
               ELSE
                   MOVE    WRK-GYO-MAX                 TO  IDX
               END-IF
           END-PERFORM
      *
           MOVE    SPA-GMN-REC         TO  WRK-GMN-REC
           INITIALIZE                      SPA-GMN-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   WRK-GYO-MAX
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
           END-PERFORM
      *
           .
       41016-SETDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    １行削除処理
      *****************************************************************
       420-SYOKYO-SEC              SECTION.
      *
           MOVE    SPA-GMN-REC         TO  WRK-GMN-REC
           INITIALIZE                      SPA-GMN-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    SPACE               TO  FLG-SYORI
           MOVE    ZERO                TO  FLG-ZAIEND
           MOVE    ZERO                TO  FLG-DELFLG
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   WRK-GYO-MAX
               IF      WRK-GMN-INPUTCD(IDX)    =   SPACE
                   IF      WRK-COM-TIMEFLG (IDX)   NOT =   ZERO
                       MOVE    1               TO  FLG-DELFLG
                       IF      WRK-COM-SRYKBN (IDX)    =   "11"  OR
                                                           "12"
                            MOVE    2               TO  FLG-DELFLG
                       END-IF
                   END-IF
                   IF     (WRK-COM-ZAIEND (IDX)    NOT =   "1" )  AND
                          (WRK-COM-DATAKBN(IDX)        =   "1"  OR
                           WRK-COM-INPUTCD(IDX)        =   SPACE)
                       MOVE    1               TO  FLG-ZAIEND
                   END-IF
      *            前も空白の時対象行のみ
                   IF     (WRK-COM-INPUTCD(IDX)        =   SPACE )  AND
                          (WRK-MAE-INPUTCD(IDX)        =   SPACE )
                       MOVE    ZERO            TO  FLG-ZAIEND
                   END-IF
      *
                   IF     (WRK-COM-ZAIEND (IDX)    =   "1" )  OR
                         ((IDX                     <   200 )  AND
                          (WRK-GMN-SRYKBN (IDX + 1)    NOT =   SPACE))
                       MOVE    ZERO            TO  FLG-ZAIEND
                   END-IF
               ELSE
                   IF      FLG-ZAIEND          =   1
                       IF      WRK-COM-AUTOKBN(IDX)    NOT =   SPACE
                           MOVE    SPACE           TO  WRK-GMN-INPUTCD
                                                                 (IDX)
                       END-IF
                   END-IF
      *            診療の時間加算削除の時、自動追加した時間加算を削除
                   IF      FLG-DELFLG          =   2
                       IF     (WRK-COM-AUTOKBN(IDX)    NOT =   SPACE)
                         AND  (WRK-COM-TIMEKSNKBN(IDX) NOT =   ZERO )
                           MOVE    SPACE           TO  WRK-GMN-INPUTCD
                                                                (IDX)
                       END-IF
                   END-IF
                   IF      WRK-COM-ZAIEND (IDX)    =   "1"
                       MOVE    ZERO            TO  FLG-ZAIEND
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      FLG-DELFLG      NOT =   ZERO
               MOVE    ZERO                TO  SPA-NAI-TIMEFLG
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   WRK-GYO-MAX
               IF      WRK-GMN-INPUTCD(IDX)    =   SPACE
                   CONTINUE
               ELSE
                   MOVE    ZERO                TO  FLG-ERR
      *            登録前チェックの時、診療種別のみの剤は削除する
                   IF      FLG-TOROKU          =   "1"
                       IF      WRK-GMN-INPUTCD (IDX)(1:1)  =  "."
                           IF      (IDX            =   WRK-GYO-MAX ) OR
                                   (WRK-GMN-INPUTCD(IDX + 1)(1:1)
                                                   =   "."     )  OR
                                   (WRK-COM-INPUTCD(IDX + 1)
                                                   =   SPACE  AND
                                    WRK-GMN-INPUTCD(IDX + 1)
                                                   =   SPACE   )  OR
                                   (WRK-GMN-SRYKBN (IDX + 1)
                                                   NOT =   SPACE )
                               MOVE    1               TO  FLG-ERR
                           END-IF
                       END-IF
                   END-IF
      *
                   IF      FLG-ERR             =   ZERO
      *
                       ADD     1               TO  SPA-GMN-MAX
                       MOVE    WRK-COM-TBLREC (IDX) 
                                               TO  SPA-COM-TBLREC
                                                        (SPA-GMN-MAX)
                       MOVE    WRK-GMN-TBLREC (IDX) 
                                               TO  SPA-GMN-TBLREC
                                                       (SPA-GMN-MAX)
                       IF      SPA-GMN-MAX     NOT =   IDX
                           MOVE    "1"             TO  FLG-SYORI
                       END-IF
                   END-IF
      *
               END-IF
           END-PERFORM
      *
           .
       420-SYOKYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    各診療行為チェックへ
      *****************************************************************
       520-ALL-CHEK-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-TUIKA
      *****MOVE    SPA-GMN-MAX         TO  WRK-MAX
      *    変更の判定用に保存
           MOVE    SPA-SCR-REC         TO  WRK-SCR-REC
      *
           MOVE    "K02"               TO  LNK-ORCSC-PG
      *****MOVE    SPA-GMN-IDX         TO  LNK-ORCSC-GMN-IDX
           MOVE    SPA-NAI-ROUJIN      TO  LNK-ORCSC-ROUJIN 
           MOVE    SPA-NENREI          TO  LNK-ORCSC-NENREI 
           MOVE    SPA-NAIACCT-AREA    TO  LNK-NAIACCT-AREA
           MOVE    SPA-NAIACCT2-AREA   TO  LNK-NAIACCT2-AREA
           MOVE    SPA-SYOYMD          TO  LNK-ORCSC-SYOYMD 
           MOVE    SPA-LSTYMD          TO  LNK-ORCSC-LSTYMD 
           MOVE    SPA-LSTSRYKA        TO  LNK-ORCSC-LSTSRYKA 
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-ORCSC-LSTSRYKAMEI 
           MOVE    SPA-INGAIKBN        TO  LNK-ORCSC-INGAIKBN
           MOVE    FLG-TOROKU          TO  LNK-ORCSC-TOROKU
      *    処理フラグ
           MOVE    SPA-SYORIFLG        TO  LNK-ORCSC-SYORIFLG
      *    初診フラグ
           MOVE    SPA-SYOSIN          TO  LNK-ORCSC-SYOSIN2
      *    初診フラグ（初診で、履歴の無い時、’１’）
           IF      (SPA-SYOSIN         =   1   )  AND
                   (SPA-GMN-MAX1       =   ZERO)  AND
                   (SPA-GMN-SYOSINYMD  =   SPACE)
               MOVE    1                   TO  LNK-ORCSC-SYOSIN
           ELSE
               MOVE    ZERO                TO  LNK-ORCSC-SYOSIN
           END-IF
      *
           MOVE    SPA-CHOKI-FLG       TO  LNK-ORCSC-CHOKI-FLG
           MOVE    SPA-YAKJYO-1        TO  LNK-ORCSC-YAKJYO-1
           MOVE    SPA-YAKJYO-2        TO  LNK-ORCSC-YAKJYO-2
           MOVE    SPA-YAKJYO-3        TO  LNK-ORCSC-YAKJYO-3
           MOVE    SPA-TOKUTEI-FLG     TO  LNK-ORCSC-TOKUTEI-FLG
      *
           MOVE    SPA-ROUMANSEI-ARI   TO  LNK-ORCSC-ROUMANSEI-ARI
           MOVE    SPA-ROUMANSEI-ARI2  TO  LNK-ORCSC-ROUMANSEI-ARI2
           MOVE    SPA-NETAKIRI-ARI    TO  LNK-ORCSC-NETAKIRI-ARI
           MOVE    SPA-NETAKIRI-ARI2   TO  LNK-ORCSC-NETAKIRI-ARI2
           MOVE    SPA-SYONIKA-ARI     TO  LNK-ORCSC-SYONIKA-ARI
           MOVE    SPA-SYONIKA-ARI2    TO  LNK-ORCSC-SYONIKA-ARI2
      *    生活習慣病指導管理料
           MOVE    SPA-SEIKATU-ARI     TO  LNK-ORCSC-SEIKATU-ARI
           MOVE    SPA-SEIKATU-ARI2    TO  LNK-ORCSC-SEIKATU-ARI2
           MOVE    SPA-NAI-GAIFLG      TO  LNK-ORCSC-GAIFLG
      *    複数診療科あり
           MOVE    SPA-FUKU-SRYKA      TO  LNK-ORCSC-FUKU-SRYKA
      *    訂正前診療科
           MOVE    SPA-OLD-SRYKA       TO  LNK-ORCSC-OLD-SRYKA
           MOVE    SPA-OLD-HKNCOMBI    TO  LNK-ORCSC-OLD-HKNCOMBI
      *    電話再診の有無
           MOVE    SPA-TELSAI-ARI      TO  LNK-ORCSC-TELSAI-ARI
      *    特定疾患あり
           MOVE    SPA-TOKUTEI         TO  LNK-ORCSC-TOKUTEI
           MOVE    SPA-TOKUTEIFLG      TO  LNK-ORCSC-TOKUTEIFLG
      *    時間外特例区分
           MOVE    SPA-JIKANTOKU       TO  LNK-ORCSC-JIKANTOKU
      *    時間外加算特例乳幼児
           MOVE    SPA-JIKANTOKU-NYU   TO  LNK-ORCSC-JIKANTOKU-NYU
      *    診察回数
           MOVE    SPA-SINSATU-CNT     TO  LNK-ORCSC-SINSATU-CNT
      *   １５歳未満再診科算定有り
           MOVE    SPA-15MIMAN-ARI     TO  LNK-ORCSC-15MIMAN-ARI
      *    特定薬剤治療管理加算の初回算定日
           MOVE    SPA-TOKTEI-YMD      TO LNK-ORCSC-TOKTEI-YMD
      *    厚生省が定める患者
           MOVE    SPA-TOKUTEI-KNJ     TO LNK-ORCSC-TOKUTEI-KNJ
           MOVE    SPA-TOKUTEI-KNJ2    TO LNK-ORCSC-TOKUTEI-KNJ2
      *    人工腎臓患者
           MOVE    SPA-TOKUTEI-JNZ     TO LNK-ORCSC-TOKUTEI-JNZ
      *    厚生省が定める患者
           MOVE    SPA-TOKUTEIG-KNJ    TO LNK-ORCSC-TOKUTEIG-KNJ
           MOVE    SPA-TOKUTEIG-KNJ2   TO LNK-ORCSC-TOKUTEIG-KNJ2
      *    再診チェック済
           MOVE    SPA-SAISIN-CHK      TO  LNK-ORCSC-SAISIN-CHK
      *    消炎鎮痛処置当月内算定
           MOVE    SPA-SYOEN-CNT       TO  LNK-ORCSC-SYOEN-CNT
      *    急性発生１８０日以内
           MOVE    SPA-KYUSEI-90       TO  LNK-ORCSC-KYUSEI-90
           MOVE    SPA-KYUSEI-180      TO  LNK-ORCSC-KYUSEI-180
           MOVE    SPA-KYUSEI-FLG      TO  LNK-ORCSC-KYUSEI-FLG
      *    当月外来管理加算の取れない患者
           MOVE    SPA-GAIRAI-NASI     TO  LNK-ORCSC-GAIRAI-NASI
      *    労災用 外来管理点数
           MOVE    SPA-GAIRAI-TEN      TO  LNK-ORCSC-GAIRAI-TEN
      *    理学療法当月内算定
      *****MOVE    WRK-RIGAKU-TBL      TO  LNK-ORCSC-RIGAKU-TBL
           MOVE    ORCSC80CNT-RIGAKU-AREA
                                       TO  LNK-ORCSC-RIGAKU-TBL
      *    早期リハ等加算（今回入力分）
           MOVE    WRK-RIGAKU-TBL      TO  LNK-ORCSC-GAI-FLG-G
      *    労災用 救急医療管理加算有無
      **   MOVE    SPA-RSI-KYUKASN     TO  LNK-ORCSC-RSI-KYUKASN
      *    労災用 療養の給付請求書有無
      ***  MOVE    SPA-RSI-KYUFU       TO  LNK-ORCSC-RSI-KYUFU
      *    労災用 手指の創傷に係る機能回復始動加算有無
      **** MOVE    SPA-RSI-KAIFUKU     TO  LNK-ORCSC-RSI-KAIFUKU
      *
      *    継続領域
           MOVE    SPA-CONTKBN         TO  LNK-ORCSC-CONTKBN
           MOVE    SPA-DENPNUM         TO  LNK-ORCSC-DENPNUM
           IF      SPA-SYORIFLG        =   1
               MOVE    SPA-RENNUM          TO  LNK-ORCSC-DOUJI-RENNUM
           ELSE
               MOVE    SPA-DOUJI-RENNUM    TO  LNK-ORCSC-DOUJI-RENNUM
           END-IF
      *
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-ORCSC-TEISEI-AREA
      *
      *    入院期間
           MOVE    SPA-K02N-NYUINYMD   TO  LNK-ORCSC-NYUINYMD
           MOVE    SPA-K02N-TAIINYMD   TO  LNK-ORCSC-TAIINYMD
      *    リハビリ逓減確認
           MOVE    FLG-RIHTEI          TO  LNK-ORCSC-FLG-RIHTEI
      *
      *            診療
           IF      SPA-SRYKBN-CNT (01) >   ZERO
               CALL    "ORCSC10"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *
      *        小児科外来診療科の時、年齢エラーはそのまま
               IF      SPA-SYONIKA-ARI     NOT =   ZERO
                   IF      SPA-ERRCD           =   "0032"
                       MOVE    SPACE               TO  SPA-ERRCD
                   END-IF
               END-IF
      *        厚生省が定める患者
               MOVE    LNK-ORCSC-TOKUTEI-KNJ   TO  SPA-TOKUTEI-KNJ
      *        人工腎臓患者
               MOVE    LNK-ORCSC-TOKUTEI-JNZ   TO  SPA-TOKUTEI-JNZ
      *        厚生省が定める患者
               MOVE    LNK-ORCSC-TOKUTEIG-KNJ  TO  SPA-TOKUTEIG-KNJ
      *        再診チェック済
               MOVE    LNK-ORCSC-SAISIN-CHK    TO  SPA-SAISIN-CHK
               MOVE    LNK-ORCSC-CHK-IDX       TO  SPA-CHK-IDX
      *        労災用 外来管理点数
               MOVE    LNK-ORCSC-GAIRAI-TEN    TO  SPA-GAIRAI-TEN
      *
      *        外来管理加算削除のチェック
               IF      SPA-GAIKANRIKBN-CHK     =   "1"
                   IF     (LNK-ORCSC-KIDCD     =   "0102" )  AND
                          (LNK-ORCSC-KBN       =   "2"    )  AND
                          (FLG-TOROKU          =   SPACE  )
                       MOVE    SPACE           TO  LNK-ORCSC-KIDCD
                   END-IF
               ELSE
                   IF      LNK-ORCSC-KIDCD     =   "0102"
                       MOVE    SPACE           TO  SPA-COM-INPUTCD
                                                    (LNK-ORCSC-CHK-IDX)
                       MOVE    SPACE           TO  SPA-GMN-INPUTCD
                                                    (LNK-ORCSC-CHK-IDX)
                       MOVE    SPACE           TO  LNK-ORCSC-KIDCD
                       MOVE    1               TO  FLG-TUIKA
                   END-IF
               END-IF
      *
               MOVE    LNK-ORCSC-KIDCD     TO  SPA-KIDCD
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            投薬（更新前チェックはすべて行う）
           IF     (SPA-SRYKBN-CNT (02) >   ZERO )
               OR (LNK-ORCSC-KBN       =   "3"  )
               CALL    "ORCSC20"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
               MOVE    LNK-ORCSC-CHK-IDX   TO  SPA-CHK-IDX
               MOVE    LNK-ORCSC-KIDCD     TO  SPA-KIDCD
               IF      LNK-ORCSC-KBN       =   "3"
                   MOVE    SPA-WORK        TO  SPA-K01KYOUTU
               END-IF
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            注射
           IF     (SPA-SRYKBN-CNT (03) >   ZERO )
      *            入院で手術の時も点滴チェックを行う
             OR  ((SPA-SRYKBN-CNT (05) >   ZERO  )  AND
                  (SPA-NYUGAIKBN       =   "1"     ))
      *            入院で画像診断の時も点滴チェックを行う
             OR  ((SPA-SRYKBN-CNT (08) >   ZERO  )  AND
                  (SPA-NYUGAIKBN       =   "1"     ))
               CALL    "ORCSC30"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
               MOVE    LNK-ORCSC-CHK-IDX   TO  SPA-CHK-IDX
               MOVE    LNK-ORCSC-KIDCD     TO  SPA-KIDCD
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            処置
           IF      SPA-SRYKBN-CNT (04) >   ZERO
               CALL    "ORCSC40"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
               MOVE    LNK-ORCSC-CHK-IDX   TO  SPA-CHK-IDX
               MOVE    LNK-ORCSC-KIDCD     TO  SPA-KIDCD
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            手術
           IF      SPA-SRYKBN-CNT (05) >   ZERO
               CALL    "ORCSC50"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
               MOVE    LNK-ORCSC-CHK-IDX   TO  SPA-CHK-IDX
               MOVE    LNK-ORCSC-KIDCD     TO  SPA-KIDCD
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            麻酔
           IF      SPA-SRYKBN-CNT (06) >   ZERO
               CALL    "ORCSC54"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
               MOVE    LNK-ORCSC-CHK-IDX   TO  SPA-CHK-IDX
               MOVE    LNK-ORCSC-KIDCD     TO  SPA-KIDCD
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            検査
           IF      SPA-SRYKBN-CNT (07) >   ZERO
               CALL    "ORCSC60"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
               MOVE    LNK-ORCSC-CHK-IDX   TO  SPA-CHK-IDX
               MOVE    LNK-ORCSC-KIDCD     TO  SPA-KIDCD
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            画像診断
           IF      SPA-SRYKBN-CNT (08) >   ZERO
               CALL    "ORCSC70"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
               MOVE    LNK-ORCSC-CHK-IDX   TO  SPA-CHK-IDX
               MOVE    LNK-ORCSC-KIDCD     TO  SPA-KIDCD
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            その他
           IF      SPA-SRYKBN-CNT (09) >   ZERO
               CALL    "ORCSC80"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
               MOVE    LNK-ORCSC-CHK-IDX   TO  SPA-CHK-IDX
               MOVE    LNK-ORCSC-KIDCD     TO  SPA-KIDCD
               MOVE    LNK-ORCSC-ERRMSG    TO  SPA-ERRMSG2
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            入院（食事）
           IF      SPA-SRYKBN-CNT (10) >   ZERO
               CALL    "ORCSCN97"      USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
               MOVE    LNK-ORCSC-CHK-IDX   TO  SPA-CHK-IDX
               MOVE    LNK-ORCSC-KIDCD     TO  SPA-KIDCD
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *
      *    変更の判定用に保存
           IF      SPA-SCR-REC     NOT =   WRK-SCR-REC
               MOVE    1               TO  FLG-TUIKA
           END-IF
      *    労災の時、読み替えあり
           IF      LNK-ORCSC-RSIOK     =   "1"
               MOVE    1               TO  FLG-TUIKA
           END-IF
      *
           .
       520-ALL-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＯ選択処理
      *****************************************************************
       41014-DOSELNUM-SEC              SECTION.
      *
           MOVE    4                   TO  SPA-GMN-CUR
      *
           MOVE    K02N-DOSELNUM       TO  SPA-GMN-DOSELNUM
           IF      K02N-DOSELNUM       =   SPACE
               MOVE    ZERO            TO  SPA-NAI-DOSELNUM
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    K02N-DOSELNUM       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "1000"              TO  SPA-ERRCD
                   GO      TO      41014-DOSELNUM-EXT
               ELSE
                   MOVE    SNUM-OUTNUM       TO  SPA-NAI-DOSELNUM
               END-IF
           END-IF
           MOVE    SPA-NAI-DOSELNUM    TO  WRK-ZZ
           MOVE    WRK-ZZ              TO  SPA-GMN-DOSELNUM
      *
           IF      SPA-NAI-DOSELNUM    <   01   OR
                                       >   SPA-GMN-MAX1
               MOVE    "1000"              TO  SPA-ERRCD
           ELSE
      *
               IF      SPA-NAI-SYORI       =   ZERO
      *            ＤＯ検索処理へ
                   PERFORM 4801-K09-SYORI-SEC
               ELSE
      *            訂正日編集処理
                   PERFORM 410141-TEISEI-SET-SEC
               END-IF
           END-IF
      *
           .
       41014-DOSELNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日選択処理
      *****************************************************************
       41015-RRKLIST-SEC           SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-DOSELNUM
           MOVE    ZERO                TO  WRK-DOSELNUM
           MOVE    ZERO                TO  WRK-DOSELNUM2
      *
           PERFORM VARYING     IDX   FROM   1   BY  1
                   UNTIL       IDX   >   SPA-GMN-MAX1
              IF      K02N-SELECT (IDX)        =  "T"
                  IF      IDX                 =   SPA-NAI-TESELNUM
                      CONTINUE
                  ELSE
                      MOVE    SPA-GMN-NO (IDX)    TO  WRK-DOSELNUM2
                  END-IF
                  MOVE    SPA-GMN-NO (IDX)    TO  WRK-DOSELNUM
              END-IF
           END-PERFORM
      *
           IF      WRK-DOSELNUM2       >   ZERO
               MOVE    WRK-DOSELNUM2       TO  SPA-NAI-DOSELNUM
           ELSE
               MOVE    WRK-DOSELNUM        TO  SPA-NAI-DOSELNUM
           END-IF
      *
           IF      SPA-NAI-DOSELNUM    <   01   OR
                                       >   SPA-GMN-MAX1
               CONTINUE
           ELSE
               MOVE    K02N-NO  (SPA-NAI-DOSELNUM)
                                           TO  SPA-GMN-DOSELNUM
      *
               IF      SPA-NAI-SYORI       =   ZERO
      *            ＤＯ検索処理へ
                   PERFORM 4801-K09-SYORI-SEC
               ELSE
      *            訂正日編集処理
                   PERFORM 410141-TEISEI-SET-SEC
               END-IF
           END-IF
      *
      *
           .
       41015-RRKLIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    訂正日編集処理
      *****************************************************************
       410141-TEISEI-SET-SEC           SECTION.
      *
      *    診療日付
           MOVE    SPA-GMN-RRKYMD (SPA-NAI-DOSELNUM)
                                       TO  SPA-SRYYMDW
      *    労災・自賠責の時、日付のみ
           IF      SPA-NAI-RRK-HKNKBN(SPA-NAI-DOSELNUM)  =   "1"
                                                         OR  "2"
               MOVE    SPA-GMN-RRKYMD (SPA-NAI-DOSELNUM)(1:12)
                                           TO  SPA-SRYYMDW
           END-IF
      *
           MOVE    SPA-NAI-RRKYMD (SPA-NAI-DOSELNUM)
                                       TO  SPA-SRYYMD
           MOVE    SPA-NAI-RENNUM (SPA-NAI-DOSELNUM)
                                       TO  SPA-RENNUM
           MOVE    SPA-NAI-DOUJI-RENNUM (SPA-NAI-DOSELNUM)
                                       TO  SPA-DOUJI-RENNUM
           MOVE    SPA-SRYYMD          TO  SPA-NAI-SRYYMD
           MOVE    SPA-SRYYMDW         TO  SPA-GMN-SRYYMD
      *    入院が存在するかのチェック
           PERFORM 410101-NYUINRRK-CHK-SEC
      *
      *    診療科
           MOVE    SPA-NAI-RRK-SRYKA(SPA-NAI-DOSELNUM)
                                       TO  SPA-SRYKA
           MOVE    SPA-NAI-RRK-SRYKA(SPA-NAI-DOSELNUM)
                                       TO  SPA-OLD-SRYKA
      *    保険組合せ
           MOVE    SPA-NAI-RRK-HKNCOMBI(SPA-NAI-DOSELNUM)
                                       TO  SPA-HKNCOMBINUM
           MOVE    SPA-NAI-RRK-HKNCOMBI(SPA-NAI-DOSELNUM)
                                       TO  SPA-OLD-HKNCOMBI
      *
      *    訂正処理
           MOVE    1                   TO  SPA-SYORIFLG
      *
      *    ＳＰＡ編集処理
           INITIALIZE                  SPA-SCR-REC
           INITIALIZE                  ORCSC95AREA
           MOVE    "4"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           MOVE    SPA-SRYYMD          TO  LNK-SC95-RRKYMD
           MOVE    SPA-RENNUM          TO  LNK-SC95-RENNUM
           MOVE    SPA-DOUJI-RENNUM    TO  LNK-SC95I-DOUJI-RENNUM
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
      *    パラメタファイルより表示
           MOVE    3               TO  FLG-PARASET
           PERFORM 3002-PARA-SET-SEC
      *
      *    訂正処理
      *****MOVE    1                   TO  SPA-SYORIFLG
      *
      *    スパ共通編集処理
           PERFORM                     310-SPA-SET-SEC
           MOVE    SPA-GMN-RRKYMD (SPA-NAI-DOSELNUM)
                                       TO  SPA-GMN-SRYYMD
      *    労災・自賠責の時、日付のみ
           IF      SPA-NAI-RRK-HKNKBN(SPA-NAI-DOSELNUM)  =   "1"
                                                         OR  "2"
               MOVE    SPA-GMN-RRKYMD (SPA-NAI-DOSELNUM)(1:12)
                                           TO  SPA-GMN-SRYYMD
           END-IF
      *
      *    当月診療行為チェック用編集、当月累計点数計算
           PERFORM 3201-TOUCHKSET-SEC
      *    点滴の変更
           IF      LNK-SC95-TENTEKI    =   1
               MOVE    SPACE           TO  SPA-ACCT2-SRYCD
           END-IF
           COMPUTE SPA-ACCT2-CSYYOURYO
                                       =   SPA-ACCT2-CSYYOURYO
                                       -   LNK-SC95-CHUSYA
      *
      *    画面用スパ編集処理
           MOVE    1                   TO  FLG-TEISEI
           PERFORM 310-SPA-GMNSET-SEC
      *
      *    ＤＯ編集へ戻す
           MOVE    "F"                 TO  SPA-GMN-TEISEI
           MOVE    ZERO                TO  SPA-NAI-SYORI
           MOVE    SPA-NAI-DOSELNUM    TO  SPA-NAI-TESELNUM
           MOVE    ZERO                TO  SPA-NAI-DOSELNUM
           MOVE    SPACE               TO  SPA-GMN-DOSELNUM
      *
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-GMN-CUR
           MOVE    1                   TO  SPA-MAP-IDX
      *
           .
       410141-TEISEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    ZERO                TO  SPA-RJN-ERR
           MOVE    ZERO                TO  SPA-HKN-ERR
           IF      SPA-MACHIPG         =   "U02"
               MOVE    "U02"               TO  WRK-SAKIPG
               PERFORM 210-BACK-SYORI-SEC
           ELSE
               IF      SPA-GMN-PTNUM       =   SPACE
                   MOVE    "M01"               TO  WRK-SAKIPG
                   PERFORM 210-BACK-SYORI-SEC
               ELSE
      *        ガイドメッセージ表示へ
                  MOVE    "0104"              TO  SPA-KIDCD
               END-IF
           END-IF
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK-SYORI-SEC               SECTION.
      *
           IF      SPA-GMN-PTID    NOT =   ZERO
      *        排他制御解除処理
               PERFORM 990-LOCK-OUT-SEC
           END-IF
      *
           PERFORM 210-PARA-DEL-SEC
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    WRK-SAKIPG          TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    SPACE               TO  SPA-MOTOPG2
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻るのクリア編集処理
      *****************************************************************
       210-PARA-DEL-SEC               SECTION.
      *
           MOVE    SPA-TERMID          TO  FILE-TERMID2
      *
      **** OPEN    OUTPUT              PARA-FILE
      *    MOVE    SPACE               TO  PARA-REC
      *    WRITE   PARA-REC
      *****CLOSE                       PARA-FILE
      *
           MOVE    ZERO                TO  IF-RESULT
           MOVE    FILE-NAME1          TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
      *
           MOVE    ZERO                TO  IF-RESULT
           MOVE    FILE-NAME2          TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
      *
           MOVE    1                   TO  FLG-END2
      *
           .
       210-PARA-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理（確定）
      *****************************************************************
       210-BACK-OK-SEC                SECTION.
      *
           PERFORM 210-PARA-DEL-SEC
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           MOVE    SPACE               TO  LINKAREA
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "M01"               TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    SPACE               TO  SPA-MOTOPG2
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-OK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者取消処理
      *****************************************************************
       222-PTNUMCLEAR-SEC             SECTION.
      *
      *     クリア確認チェック
           IF      (SPA-CLEARCHK-FLG   =   "1"   )  AND
                   (SPA-GMN-PTID   NOT =   ZERO  )
               AND (SPA-GMN-MAX        >   ZERO  )
               MOVE    "0125"              TO  SPA-KIDCD
           ELSE
               PERFORM 2221-PTNUMCLEAR-SYORI-SEC
           END-IF
      *
           .
       222-PTNUMCLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者取消処理
      *****************************************************************
       2221-PTNUMCLEAR-SYORI-SEC             SECTION.
      *
           IF      SPA-GMN-PTID    NOT =   ZERO
      *        排他制御解除処理
               PERFORM 990-LOCK-OUT-SEC
           END-IF
      *
           INITIALIZE              SPA-K02
           INITIALIZE              SPA-KYOTUKEY
           INITIALIZE              SPA-SCR-REC
      *
      *    初期ＳＰＡ編集処理
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-SYSYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SYSYMDWH        TO  LNK-SC95-SRYYMDG
           MOVE    "9"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
           MOVE    0               TO  SPA-GMN-CUR
      *
           .
       2221-PTNUMCLEAR-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア　処理
      *****************************************************************
       220-CLEAR-SEC             SECTION.
      *
      *    確認メッセージ
           MOVE    "0116"              TO  SPA-KIDCD
      *
           .
       220-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア　処理
      *****************************************************************
       220-CLEAR-SYORI-SEC            SECTION.
      *
           IF      SPA-GMN-PTID    NOT =   ZERO
      *        ワーク診療行為クリア
               PERFORM 4900-WKSRYACT-CLEAR-SEC
           END-IF
      *
           INITIALIZE              SPA-SCR-REC
      *
           MOVE    1               TO  SPA-GMN-CUR
           MOVE    1               TO  SPA-GMN-PAGE
      *
           .
       220-CLEAR-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為クリア処理
      *****************************************************************
       4900-WKSRYACT-CLEAR-SEC             SECTION.
      *
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPID          TO  WKSRY-HOSPID
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  WKSRY-PTID
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "WKSRYACT-KEY6"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "WKSRYACT-KEY6"     TO  ORC-DBPATH
               PERFORM 970-WKSRYACT-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
           PERFORM
               UNTIL   FLG-WKSRYACT    =   1
               EVALUATE    FLG-MOD-FLG
                   WHEN    1
      *                展開中止
                       IF      WKSRY-MOD-FLG       =   9
                           MOVE    ZERO                TO  WKSRY-MOD-FLG
                       END-IF
                   WHEN    2
      *                前の展開
                       IF      WKSRY-MOD-FLG       =   9
                           MOVE    1                   TO  WKSRY-MOD-FLG
                       ELSE
                           IF      WKSRY-MOD-FLG       =   1
                               MOVE    ZERO            TO  WKSRY-MOD-FLG
                           END-IF
                       END-IF
                   WHEN    0
                       MOVE    ZERO                TO  WKSRY-MOD-FLG
               END-EVALUATE
      *
               MOVE    WKSRYACT-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "WKSRYACT-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
               MOVE    "WKSRYACT-KEY6"     TO  ORC-DBPATH
               PERFORM 970-WKSRYACT-READ-SEC
           END-PERFORM
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "WKSRYACT-KEY6"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       4900-WKSRYACT-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット登録画面遷移処理
      *****************************************************************
       420-KOUISET-SEC             SECTION.
      *
      *****患者存在チェック
      *    IF      SPA-PTID            =   ZERO
      *        MOVE    "1005"              TO  SPA-ERRCD
      *        MOVE    00                  TO  SPA-GMN-CUR
      *        GO      TO      420-KOUISET-EXT
      *    END-IF
      *    IF      SPA-HKNCOMBINUM     =   SPACE
      *        MOVE    "1006"          TO  SPA-ERRCD
      *        MOVE    00                  TO  SPA-GMN-CUR
      *        GO      TO      420-KOUISET-EXT
      *****END-IF
      *    患者が確定してない時、戻った時表示を空白
           IF      SPA-PTID            =   ZERO
               MOVE    SPACE           TO  SPA-GMN-PTNUM
           ELSE
      *        診療行為入力チェック
               PERFORM 41011-KOUI-ALLHEN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      420-KOUISET-EXT
               END-IF
      *
           END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "K05"               TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "K05"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       420-KOUISET-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴更新画面　処理
      *****************************************************************
       430-SANTEIRRK-SEC             SECTION.
      *
      *    患者存在チェック
           IF      SPA-PTID            =   ZERO
               MOVE    "1005"              TO  SPA-ERRCD
               MOVE    00                  TO  SPA-GMN-CUR
               GO      TO      430-SANTEIRRK-EXT
           END-IF
           IF      SPA-HKNCOMBINUM     =   SPACE
               MOVE    "1006"              TO  SPA-ERRCD
               MOVE    00                  TO  SPA-GMN-CUR
               MOVE    3                   TO  SPA-HKN-ERR
               GO      TO      430-SANTEIRRK-EXT
           END-IF
      *
      *    診療行為入力チェック
           MOVE    1                   TO  FLG-HENKOU
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD           =   "0180" )
               GO      TO      430-SANTEIRRK-EXT
           END-IF
      *
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           MOVE    SPA-GMN-NAME        TO  SPA-NAME
           MOVE    SPA-GMN-KANANAME    TO  SPA-KANANAME
           MOVE    SPA-GMN-SEX         TO  SPA-SEX
           MOVE    SPA-GMN-BIRTHDAY    TO  SPA-BIRTHDAYW
           MOVE    SPA-GMN-SRYYMD      TO  SPA-SRYYMDW
           MOVE    SPA-NAI-BIRTHDAY    TO  SPA-BIRTHDAY
           MOVE    SPA-NAI-SRYYMD      TO  SPA-SRYYMD
           MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "K06"               TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "K06"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       430-SANTEIRRK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード設定登録　処理
      *****************************************************************
       4106S-INPUTCD-SYORI-SEC             SECTION.
      *
      *    更新前の基本チェック
           PERFORM 41001-KIHON-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE  )  OR
                  (SPA-KIDCD           =   "0180" )
               GO      TO      4106S-INPUTCD-SYORI-EXT
           END-IF
      *
      *    更新前のチェック
           MOVE    "1"                 TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE               TO  FLG-TOROKU
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4106S-INPUTCD-SYORI-EXT
           END-IF
      *
      *    画面ＳＰＡ書込み
           PERFORM 1003-K02SPA-OUT-SEC
      *
      *    初期表示
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K02             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGK023"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-K02
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
           IF      SPA-K023-MAX        =   ZERO
               MOVE    "1012"              TO  SPA-ERRCD
               GO      TO      4106S-INPUTCD-SYORI-EXT
           END-IF
      *    画面カーソルセット処理
           MOVE    1                   TO  SPA-K023-CUR
           MOVE    1                   TO  SPA-K023-PAGE
           MOVE    1                   TO  SPA-K023-IDX
           MOVE    1                   TO  SPA-K023-MAP
           MOVE    "INPUTCD101"        TO  MCP-WIDGET
      *
           MOVE    "K02N"              TO  SPA-HZN-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
      *    診療行為一覧へ
           MOVE    "K023"              TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K023"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4106S-INPUTCD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名登録　処理
      *****************************************************************
       440-BYOTOROKU-SEC             SECTION.
      *
           IF      SPA-GMN-PTID      =   ZERO
      *        病名登録遷移処理
               PERFORM 4401-C02SET-SEC
               GO      TO      440-BYOTOROKU-EXT
           END-IF
      *
      *    基本チェック
           MOVE    1                   TO  FLG-HENKOU
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE  )  OR
                  (SPA-KIDCD           =   "0180" )  OR
                  (FLG-END             =   1      )
               CONTINUE
           ELSE
      *        病名登録遷移処理
               PERFORM 4401-C02SET-SEC
           END-IF
      *
      *!   更新前のチェック
      *    MOVE    "1"                 TO  FLG-TOROKU
      *    PERFORM 510-TBLHEN-SEC
      *    MOVE    SPACE               TO  FLG-TOROKU
      *    IF      SPA-ERRCD       NOT =   SPACE
      *        GO      TO      440-BYOTOROKU-EXT
      *!   END-IF
      *
      *    禁忌更新前のチェック
      *    PERFORM 41001-YAKKNK-CHK-SEC
      *
      *     禁忌エラーの時、エラー表示へ
      *    IF      WRK-KNKCHK-AREA     NOT =   SPACE
      *        MOVE    "C02"               TO  SPA-K021-SAKI
      *        PERFORM 410019-POP-K021-SEC
      *        GO      TO      440-BYOTOROKU-EXT
      *    END-IF
      *    病名登録遷移処理
      *!!  PERFORM 4401-C02SET-SEC
           .
       440-BYOTOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名登録遷移処理
      *****************************************************************
       4401-C02SET-SEC             SECTION.
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    "C02"               TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "K02N"              TO  SPA-MOTOPG2
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      SPA-WORK
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "C02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       4401-C02SET-EXT.
           EXIT.
      *    
      *****************************************************************
      *    患者登録へ
      *****************************************************************
       440-KNJADD-SEC             SECTION.
      *
      *
           IF      SPA-PTNUM(1:1)      =   "*"
               MOVE    ZERO                TO  SPA-PTID
           ELSE
      *
      *        患者存在チェック
               IF      SPA-PTID            =   ZERO
                   MOVE    SPACE               TO  SPA-PTNUM
               ELSE
      *
      *            診療行為入力チェック
                   MOVE    1                   TO  FLG-HENKOU
                   PERFORM 41011-KOUI-ALLHEN-SEC
                   IF     (SPA-ERRCD       NOT =   SPACE  )  OR
                          (SPA-KIDCD           =   "0180" )  OR
                          (FLG-END             =   1      )
                      GO      TO      440-KNJADD-EXT
                   END-IF
      *********更新前のチェック
      *            MOVE    "1"                 TO  FLG-TOROKU
      *            PERFORM 510-TBLHEN-SEC
      *            MOVE    SPACE               TO  FLG-TOROKU
      *            IF      SPA-ERRCD       NOT =   SPACE
      *               GO      TO      440-KNJADD-EXT
      ***********  END-IF
               END-IF
           END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "K02N"              TO  SPA-MOTOPG2
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
       440-KNJADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    会計カードへ
      *****************************************************************
       450-KAIKEI-SEC             SECTION.
      *
      *
           IF      SPA-GMN-PTID    NOT =   ZERO
      *        更新前の基本チェック
               MOVE    1                   TO  FLG-HENKOU
               PERFORM 41011-KOUI-ALLHEN-SEC
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (SPA-KIDCD           =   "0180" ) OR
                      (FLG-END             =   1     )
                   GO      TO      450-KAIKEI-EXT
               END-IF
      *
      ******** 更新前のチェック
      *        MOVE    "1"                 TO  FLG-TOROKU
      *        PERFORM 510-TBLHEN-SEC
      *        MOVE    SPACE               TO  FLG-TOROKU
      *        IF      SPA-ERRCD       NOT =   SPACE
      *            GO      TO      450-KAIKEI-EXT
      ******** END-IF
      *
           END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "K02N"              TO  SPA-MOTOPG2
           MOVE    "J02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      SPA-WORK
           MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
           MOVE    SPA-SYSYMDWH        TO  SPA-SRYYMDW
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "J02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
          .
       450-KAIKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納登録へ
      *****************************************************************
       460-SYUNOU-SEC             SECTION.
      *
      *
           IF      SPA-GMN-PTID    NOT =   ZERO
      *        更新前の基本チェック
               MOVE    1                   TO  FLG-HENKOU
               PERFORM 41011-KOUI-ALLHEN-SEC
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (SPA-KIDCD           =   "0180" ) OR
                      (FLG-END             =   1     )
                   GO      TO      460-SYUNOU-EXT
               END-IF
      *
      ******** 更新前のチェック
      *        MOVE    "1"                 TO  FLG-TOROKU
      *        PERFORM 510-TBLHEN-SEC
      *        MOVE    SPACE               TO  FLG-TOROKU
      *        IF      SPA-ERRCD       NOT =   SPACE
      *            GO      TO      460-SYUNOU-EXT
      ******** END-IF
           END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "K02N"              TO  SPA-MOTOPG2
           MOVE    "S02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      SPA-WORK
           MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
           MOVE    SPA-SYSYMDWH        TO  SPA-SRYYMDW
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "S02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
          .
       460-SYUNOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入退院登録へ
      *****************************************************************
       4106S-NYUTAIIN-SEC             SECTION.
      *
           IF      SPA-GMN-PTID    NOT =   ZERO
      *        更新前の基本チェック
               MOVE    1                   TO  FLG-HENKOU
               PERFORM 41011-KOUI-ALLHEN-SEC
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (SPA-KIDCD           =   "0180" ) OR
                      (FLG-END             =   1     )
                   GO      TO      4106S-NYUTAIIN-EXT
               END-IF
           END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "K02N"              TO  SPA-MOTOPG2
           MOVE    "I01"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      SPA-WORK
      *?   MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
      *?   MOVE    SPA-SYSYMDWH        TO  SPA-SRYYMDW
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "I01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
          .
       4106S-NYUTAIIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入退院登録へ
      *****************************************************************
       4111S-NYUKAIKEI-SEC             SECTION.
      *
           IF      SPA-GMN-PTID    NOT =   ZERO
      *        更新前の基本チェック
               MOVE    1                   TO  FLG-HENKOU
               PERFORM 41011-KOUI-ALLHEN-SEC
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (SPA-KIDCD           =   "0180" ) OR
                      (FLG-END             =   1     )
                   GO      TO      4111S-NYUKAIKEI-EXT
               END-IF
           END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "K02N"              TO  SPA-MOTOPG2
           MOVE    "I41"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      SPA-WORK
      *?   MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
      *?   MOVE    SPA-SYSYMDWH        TO  SPA-SRYYMDW
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "I41"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
          .
       4111S-NYUKAIKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *   ワーク行為負担パラメタを作成　処理
      *****************************************************************
       4410-WSINPARA-SEC             SECTION.
      *
           INITIALIZE                  ORCSCWKSRYAREA
           MOVE    "4"                 TO  ORCSCWKSRY-KBN
           MOVE    "K02N"              TO  ORCSCWKSRY-PG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           CALL    "ORCSCWKSRY"        USING
                                       ORCSCWKSRYAREA
                                       SPA-AREA
                                       SPA-K02
                                       SPA-SCR-REC
      *
           .
       4410-WSINPARA-EXT.
           EXIT.
      *
      *****************************************************************
      *    途中終了　処理
      *****************************************************************
       4110-KYOSEIEND-SEC             SECTION.
      *
      *    訂正中の時、中途終了できない。
           IF      SPA-SYORIFLG        =   1
               MOVE    "1024"          TO  SPA-ERRCD
               GO      TO      4110-KYOSEIEND-EXT
           END-IF
      *
      *    更新前の基本チェック
           PERFORM 41001-KIHON-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE  )  OR
                  (SPA-KIDCD           =   "0180" )
               GO      TO      4110-KYOSEIEND-EXT
           END-IF
      *
      *    更新前のチェック
           MOVE    "1"                 TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE               TO  FLG-TOROKU
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO      4110-KYOSEIEND-EXT
           END-IF
      *
           PERFORM 45109-SNRYO-NYU-SEC
      *
           .
       4110-KYOSEIEND-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       450-MAEGMN-SEC             SECTION.
      *
      *    診療行為入力チェック
           MOVE    1                   TO  FLG-HENKOU
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD           =   "0180" )  OR
                  (FLG-END             =   1     )
               GO      TO      450-MAEGMN-EXT
           END-IF
      *
           IF      SPA-GMN-PAGE        =   1
               MOVE    1               TO  SPA-GMN-PAGE
           ELSE
               COMPUTE SPA-GMN-PAGE    =   SPA-GMN-PAGE   -   1
           END-IF
      *
           MOVE    1                   TO  SPA-MAP-IDX
           MOVE    99                  TO  SPA-GMN-CUR
           .
       450-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       460-ATOGMN-SEC             SECTION.
      *
      *    診療行為入力チェック
           MOVE    1                   TO  FLG-HENKOU
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD           =   "0180")  OR
                  (FLG-END             =   1     )
               GO      TO      460-ATOGMN-EXT
           END-IF
      *
           IF      SPA-GMN-PAGE        =   9
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "7002"              TO  SPA-ERRCD
               GO      TO      460-ATOGMN-EXT
           END-IF
      *
           COMPUTE WRK-FREE            =   SPA-GMN-PAGE  *  24
           IF      SPA-GMN-INPUTCD(WRK-FREE)   =   SPACE
               GO  TO                 460-ATOGMN-EXT
           END-IF
      *
           ADD     1                   TO  SPA-GMN-PAGE
           IF      SPA-GMN-PAGE        =   8
               MOVE    "7003"              TO  SPA-ERRCD
           END-IF
      *
           MOVE    88                  TO  SPA-GMN-CUR
           .
       460-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    前カーソル　処理
      *****************************************************************
       450-MAECUR-SEC             SECTION.
      *
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    01
      *            診療科へ
                   MOVE    06              TO  SPA-GMN-CUR
               WHEN    06
                   MOVE    07              TO  SPA-GMN-CUR
               WHEN    07
                   MOVE    05              TO  SPA-GMN-CUR
               WHEN    05
                   MOVE    ZERO            TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       450-MAECUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＯ処理へ
      *****************************************************************
       480-DOSYORI-SEC            SECTION.
      *
      *    患者存在チェック
           IF      SPA-PTID            =   ZERO
               MOVE    "1005"              TO  SPA-ERRCD
               MOVE    00                  TO  SPA-GMN-CUR
               GO      TO      480-DOSYORI-EXT
           END-IF
           IF      SPA-HKNCOMBINUM     =   SPACE
               MOVE    "1006"          TO  SPA-ERRCD
               MOVE    00              TO  SPA-GMN-CUR
               MOVE    3               TO  SPA-HKN-ERR
               GO      TO      480-DOSYORI-EXT
           END-IF
      *
      *    診療行為入力チェック
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD           =   "0180" )  OR
                  (FLG-END             =   1     )
               GO      TO      480-DOSYORI-EXT
           END-IF
      *
      **** MOVE    ZERO                TO  SPA-NAI-DOSELNUM
      **** MOVE    SPACE               TO  SPA-GMN-DOSELNUM
           MOVE    1                   TO  SPA-NAI-DOSELNUM
           MOVE    SPA-NAI-DOSELNUM    TO  WRK-ZZ
           MOVE    WRK-ZZ              TO  SPA-GMN-DOSELNUM
      *
      *    ＤＯ検索処理へ
           PERFORM 4801-K09-SYORI-SEC
           .
       480-DOSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＯ処理へ
      *****************************************************************
       4801-K09-SYORI-SEC            SECTION.
      *
      *    初期画面セット
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K02             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGK09"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-K02
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
           MOVE    "K09"               TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
      *
           MOVE    "K02N"              TO  SPA-HZN-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
      *****MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K09"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4801-K09-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｋ０１処理へ
      *****************************************************************
       4001-K01-SYORI-SEC                   SECTION.
      *
      *    診療行為入力チェック
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (FLG-END             =   1     )
               GO      TO      4001-K01-SYORI-EXT
           END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    "K01"               TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
      *
           MOVE    "K02N"              TO  SPA-HZN-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           IF      WRK-PUTTYPE         =   SPACE
               MOVE    "CHANGE"            TO  MCP-PUTTYPE
           ELSE
               MOVE    "JOIN"              TO  MCP-PUTTYPE
           END-IF
           MOVE    "K01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4001-K01-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定処理へ
      *****************************************************************
       470-SANTEI-SEC             SECTION.
      *
      *    診療行為入力チェック
           MOVE    1                   TO  FLG-HENKOU
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD           =   "0180" )  OR
                  (FLG-END             =   1     )
               GO      TO      470-SANTEI-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
           CALL    "ORCGK04"           USING
                   MCPAREA
                   SPAAREA
                   LINKAREA
                   SCRAREA.
      *
      *    初期カーソル設定
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "K04"               TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
      *
           MOVE    "K02N"              TO  SPA-HZN-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K04"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       470-SANTEI-EXT.
           EXIT.
      *    
      *****************************************************************
      *    前回患者番号編集処理
      *****************************************************************
       230-MAEKPTNUM-SEC             SECTION.
      *
      *
           IF      SPA-LAST-PTNUM      =   SPACE
               GO      TO      230-MAEKPTNUM-EXT
           END-IF
      *
           MOVE    SPA-LAST-PTNUM      TO  K02N-PTNUM
           PERFORM 41010-PTNUM-SYORI-SEC
      *
           .
       230-MAEKPTNUM-EXT.
           EXIT.
      *    
      *
      *****************************************************************
      *    氏名検索へ
      *****************************************************************
       470-SIMEIKEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-K02             TO  LNK-SCRSPA
      *    画面移行用のパラメタ変更
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "P97"               TO  SPA-SAKIPG
      *
      *    氏名をクリアする
           MOVE    SPACE               TO  SPA-NAME
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
      *****MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE    "P97"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       470-SIMEIKEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約登録へ
      *****************************************************************
       480-YOYAKU-SEC             SECTION.
      *
      *    診療行為入力チェック
           MOVE    1                   TO  FLG-HENKOU
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD           =   "0180" ) OR
                  (FLG-END             =   1     )
               GO      TO      480-YOYAKU-EXT
           END-IF
      *
      *    ワーク行為負担パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-K02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "K02N"              TO  SPA-MOTOPG3
           MOVE    "Y01"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE   "Y01"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       480-YOYAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付一覧へ
      *****************************************************************
       490-UKEICHI-SEC             SECTION.
      *
      *    診療行為入力チェック
           MOVE    1                   TO  FLG-HENKOU
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD           =   "0180" ) OR
                  (FLG-END             =   1     )
               GO      TO      490-UKEICHI-EXT
           END-IF
      *
      *    更新前のチェック
      **   MOVE    "1"                 TO  FLG-TOROKU
      *    PERFORM 510-TBLHEN-SEC
      *    MOVE    SPACE               TO  FLG-TOROKU
      *    IF      SPA-ERRCD       NOT =   SPACE
      *        GO      TO      490-UKEICHI-EXT
      *****END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-K02             TO  LNK-SCRSPA
      *
           MOVE    "U01"               TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "U01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-UKEICHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付画面へ
      *****************************************************************
       4102-UKETUKE-SEC             SECTION.
      *
      *    診療行為入力チェック
           MOVE    1                   TO  FLG-HENKOU
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD           =   "0180" ) OR
                  (FLG-END             =   1     )
               GO      TO      4102-UKETUKE-EXT
           END-IF
      *
      *****更新前のチェック
      *    MOVE    "1"                 TO  FLG-TOROKU
      *    PERFORM 510-TBLHEN-SEC
      *    MOVE    SPACE               TO  FLG-TOROKU
      *    IF      SPA-ERRCD       NOT =   SPACE
      *        GO      TO      4102-UKETUKE-EXT
      *****END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
      *
           MOVE    "U02"               TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    SPA-K02             TO  LNK-SCRSPA
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
      **** MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE    "U02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4102-UKETUKE-EXT.
           EXIT.
      *TEST
      *
      *****************************************************************
      *    ヘルプ処理
      *****************************************************************
       4199-HELP-SYORI-SEC             SECTION.
      *    初期画面セット
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGKHELP"         USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *    戻り先
           MOVE    "K02N"              TO  SPA-ETC-MOTOPG
           MOVE    "KHELP"             TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "SELNUM"            TO  MCP-WIDGET
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "KHELP"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       4199-HELP-SYORI-EXT.
           EXIT.
      *TEST
      *
      *****************************************************************
      *    訂正処理
      *****************************************************************
       4101-TEISEI-SEC             SECTION.
      *
           MOVE    04                  TO  SPA-GMN-CUR
      *
           EVALUATE    SPA-GMN-TEISEI
      *        ＤＯ検索
               WHEN    "T"
                   MOVE    "F"             TO  SPA-GMN-TEISEI
                   MOVE    ZERO            TO  SPA-NAI-SYORI
      *        訂正診療日選択
               WHEN    "F"
      *            診療室入力の時、訂正はできないこととする
                   IF      SPA-BUNSAN          =   "1"
                       MOVE    "1026"          TO  SPA-ERRCD
                       GO      TO      4101-TEISEI-EXT
                   END-IF
                   MOVE    "T"             TO  SPA-GMN-TEISEI
                   MOVE    1               TO  SPA-NAI-SYORI
           END-EVALUATE
      *
           .
       4101-TEISEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新前の基本チェック処理
      *****************************************************************
       41001-KIHON-CHK-SEC             SECTION.
      *
           IF      SPA-PTNUM           =   SPACE
               MOVE    "1003"              TO  SPA-ERRCD
               MOVE    00                  TO  SPA-GMN-CUR
           ELSE
      *        患者マスター存在チェック
               MOVE    SPA-HOSPID          TO  PTINF-HOSPID
               MOVE    SPA-PTID            TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               IF     (FLG-PTINF           =   1     )  OR
                      (SPA-PTNUM           =   SPACE )
                   MOVE    "1003"              TO  SPA-ERRCD
                   MOVE    00                  TO  SPA-GMN-CUR
               ELSE
                   IF      SPA-HKNCOMBINUM     =   SPACE
                       MOVE    "1006"          TO  SPA-ERRCD
                       MOVE    3               TO  SPA-HKN-ERR
                       MOVE    00              TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41001-KIHON-CHK-EXT
           END-IF
      *    老人保険編集エラーがある時、エラーとする
           IF      SPA-RJN-ERR         =   1
               MOVE    "1027"              TO  SPA-ERRCD
               GO      TO      41001-KIHON-CHK-EXT
           END-IF
      *    保険エラーがある時、エラーとする
           IF      SPA-HKN-ERR         =   1
               MOVE    "1028"              TO  SPA-ERRCD
               GO      TO      41001-KIHON-CHK-EXT
           END-IF
           IF      SPA-HKN-ERR         =   2
               MOVE    "1032"              TO  SPA-ERRCD
               GO      TO      41001-KIHON-CHK-EXT
           END-IF
      *    保険未選択の場合、エラー
           IF      SPA-HKN-ERR         =   3
               MOVE    "1006"              TO  SPA-ERRCD
               GO      TO      41001-KIHON-CHK-EXT
           END-IF
      *    保険未選択の場合、エラー
           IF      SPA-HKN-ERR         =   4
               MOVE    "1034"              TO  SPA-ERRCD
               MOVE    5                   TO  SPA-GMN-CUR
               GO      TO      41001-KIHON-CHK-EXT
           END-IF
      *
      *    診療行為入力チェック
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD           =   "0180" )  OR
                  (FLG-END             =   1     )
               GO      TO      41001-KIHON-CHK-EXT
           END-IF
      *
      *    診療日付のチェック訂正の時、入力エラー
           IF      SPA-SYORIFLG        =   1
               IF      K02N-SRYYMD     NOT =   SPA-GMN-SRYYMD
                   MOVE    1                   TO  SPA-GMN-CUR
                   MOVE    "1009"              TO  SPA-ERRCD
                   GO      TO      41001-KIHON-CHK-EXT
               END-IF
           ELSE
      *
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN-SRYYMD      TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN-SRYYMD
      *            誕生日以前の時、エラー
                   IF      SGDAY-SDAY          <   SPA-BIRTHDAY
                        MOVE    "1011"         TO  SPA-ERRCD
                        MOVE    7              TO  SPA-GMN-CUR
                        GO      TO      41001-KIHON-CHK-EXT
                   END-IF
      *            変更のない時、以降の処理はそのまま
                   IF      SGDAY-SDAY      NOT =   SPA-NAI-SRYYMD
                        MOVE    SGDAY-SDAY     TO  SPA-NAI-SRYYMD
                        GO      TO      41001-KIHON-CHK-EXT
                   END-IF
               ELSE
                   MOVE    "1010"          TO  SPA-ERRCD
                   MOVE    7               TO  SPA-GMN-CUR
                   GO      TO      41001-KIHON-CHK-EXT
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  IDX-SET
           MOVE    SPA-SRYYMD          TO  WRK-SPA-SRYYMD
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX              >   WRK-GYO-MAX)  OR
                              (SPA-ERRCD        NOT =   SPACE )   OR
                              (SPA-GMN-INPUTCD(IDX) =   SPACE )
               IF     (SPA-COM-CHKFLG (IDX)    =   SPACE )  AND
                      (SPA-GMN-INPUTCD(IDX)(1:1)
                                           NOT =   "."  AND "S"
                                                        AND "*" )
      *            入院日の最後の日を診療日とする
                   MOVE    WRK-SPA-SRYYMD      TO  SPA-SRYYMD
                   PERFORM VARYING IDX-N2      FROM    1   BY  1
                           UNTIL   IDX-N2      >   31
                       IF      SPA-COM-NYUINDAY(IDX IDX-N2)    >   ZERO
                           MOVE    IDX-N2     TO  SPA-SRYYMD(7:2)
                       END-IF
                   END-PERFORM
      *
      *           点数マスタ編集・基本チェック
                   INITIALIZE                  ORCSC92AREA
                   MOVE    SPACE               TO  LNK-SC92-KBN
                   MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
                   MOVE    "K02"               TO  LNK-SC92-PG 
                   MOVE    IDX                 TO  LNK-SC92-GMN-IDX
                   MOVE    SPA-COM-INPUTCD (IDX)
                                               TO  LNK-SC92-SRYCD
                   MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
                   MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
                   MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
                   MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
                   MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
                   MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
                   CALL    "ORCSC92"           USING
                                               MCPAREA
                                               ORCSC92AREA
                                               SPA-SCR-REC
                                               SPA-AREA
      *
      *            小児科外来診療科の時、年齢エラーはそのまま
                   IF      SPA-SYONIKA-ARI     NOT =   ZERO
                       IF      LNK-SC92-ERRCD5 NOT =   SPACE
                           MOVE    SPACE           TO  LNK-SC92-ERRCD5
                       END-IF
                   END-IF
      *
                   PERFORM VARYING     IDX-ERR FROM    1  BY  1
                           UNTIL      (IDX-ERR     >   10     )  OR
                                      (SPA-ERRCD   NOT =   SPACE)
                       IF      LNK-SC92-ERRCD(IDX-ERR) NOT =   SPACE
                           MOVE    LNK-SC92-ERRCD(IDX-ERR)
                                                       TO  SPA-ERRCD
                           IF      IDX-ERR             =   3 OR 6
                               MOVE    LNK-SC92-ERRMSG TO  SPA-ERRMSG2
                           END-IF
                       END-IF
                   END-PERFORM
      *---(01.00.01) LINE ADD  START  ----
      *            警告判定
                   IF      SPA-ERRCD(1:1)      =   "K"
                       IF      SPA-COM-KZMFLG (IDX)  =   SPACE
                           MOVE    "1"         TO  SPA-COM-KZMFLG(IDX)
                       ELSE
                           MOVE    SPACE           TO  SPA-ERRCD
                       END-IF
                   END-IF
      *---(01.00.01) LINE ADD  END    ----
                   IF      SPA-ERRCD       NOT =   SPACE
                       IF      SPA-ERRCD(1:1)      =   "K"
                           MOVE    "2"             TO  SPA-COM-CUR(IDX)
                       ELSE
                           IF      SPA-COM-SET(IDX)    =   SPACE
                               MOVE    "1"         TO  SPA-COM-CUR(IDX)
                           ELSE
                               MOVE    "S001"      TO  SPA-ERRCD
                               MOVE    "1"         TO  SPA-COM-CUR
                                                           (IDX-SET)
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *
               IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "S"
                   MOVE   IDX              TO  IDX-SET
               END-IF
           END-PERFORM
           MOVE    WRK-SPA-SRYYMD          TO  SPA-SRYYMD
      *
           .
       41001-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    途中終了画面へ（Ｋ１０）
      *****************************************************************
       4110S-K10-SYORI-SEC            SECTION.
      *
      *    初期画面セット
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K02             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGK10"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-K02
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
      *    存在しない時、メッセージ表示
           IF      SPA-K10-MAX         =   ZERO
               IF      SPA-GMN-PTNUM       =   SPACE
                   MOVE    "1019"          TO  SPA-ERRCD
               ELSE
                   MOVE    "1018"          TO  SPA-ERRCD
               END-IF
               GO      TO      4110S-K10-SYORI-EXT
           END-IF
      *
           MOVE    "K10"               TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
      *
           MOVE    "K02N"              TO  SPA-HZN-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "SELNUM"            TO  MCP-WIDGET
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K10"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4110S-K10-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       4100-TOROKU-SEC             SECTION.
      *
      *    更新前の基本チェック
           PERFORM 41001-KIHON-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD           =   "0180" )
               GO      TO      4100-TOROKU-EXT
           END-IF
      *
      *    更新前のチェック
           MOVE    "1"                 TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE               TO  FLG-TOROKU
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO      4100-TOROKU-EXT
           END-IF
      *
      *    禁忌更新前のチェック
           PERFORM 41001-YAKKNK-CHK-SEC
      *
      *     禁忌エラーの時、エラー表示へ
           IF     (FLG-KINKI           =   1   )  AND
                  (FLG-END             =   1   )
               GO      TO      4100-TOROKU-EXT
           END-IF
      *
      *    更新処理
      *****MOVE    SPACE           TO  WRK-PUTTYPE
           PERFORM 41002-KOUSIN-SYORI-SEC
           .
       4100-TOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       41002-KOUSIN-SYORI-SEC          SECTION.
      *
      *    診療室入力の時、ワーク行為負担データを作成し、選択へ
           IF      SPA-BUNSAN          =   "1"
      ******** PERFORM 4510-SNRYO-NYU-SEC
               PERFORM 45109-SNRYO-NYU-SEC
               GO      TO      41002-KOUSIN-SYORI-EXT
           END-IF
      *
      *    行為料自動算定用判定処理 
           PERFORM 4530-TBLCHI-SYORI-SEC
           IF      SPA-ERRCD       NOT =   SPACE  OR
                   SPA-KIDCD       NOT =   SPACE
               GO      TO      41002-KOUSIN-SYORI-EXT
           END-IF
      *
           INITIALIZE                  ORCSCWKSRYAREA
      *    自動発生分の編集
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   WRK-KSN-IDX
               MOVE    SPA-HKNCOMBINUM     TO  ORCSCWKSRY-KSN-HKNCOMBI
                                                                (IDX)
               MOVE    SPA-SRYKA           TO  ORCSCWKSRY-KSN-SRYKA
                                                                (IDX)
               MOVE    WRK-KSN-SRYSYUKBN (IDX)
                                           TO  ORCSCWKSRY-KSN-SRYSYUKBN
                                                                (IDX)
               MOVE    WRK-KSN-SRYSYUKBN (IDX)
                                           TO  ORCSCWKSRY-KSN-SRYSYUKBN
                                                                (IDX)
               MOVE    WRK-KSN-SRYKBN(IDX) TO  ORCSCWKSRY-KSN-SRYKBN
                                                                (IDX)
               MOVE    WRK-KSN-SRYCD(IDX)  TO  ORCSCWKSRY-KSN-SRYCD
                                                                (IDX)
               MOVE    WRK-KSN-INPUTCD(IDX)
                                           TO  ORCSCWKSRY-KSN-INPUTCD
                                                                (IDX)
               MOVE    WRK-KSN-ATAI-G (IDX)
                                           TO  ORCSCWKSRY-KSN-ATAI-G
                                                                (IDX)
               MOVE    WRK-KSN-TENSU (IDX) TO  ORCSCWKSRY-KSN-TENSU
                                                                (IDX)
               MOVE    WRK-KSN-ZAINUM(IDX) TO  ORCSCWKSRY-KSN-ZAINUM
                                                                (IDX)
               MOVE    WRK-KSN-ZAIEND(IDX) TO  ORCSCWKSRY-KSN-ZAIEND
                                                                (IDX)
               MOVE    WRK-KSN-INPUTCD(IDX) TO  ORCSCWKSRY-KSN-INPUTCD
                                                                (IDX)
               MOVE    WRK-KSN-AUTOKBN(IDX) TO  ORCSCWKSRY-KSN-AUTOKBN
                                                                (IDX)
           END-PERFORM
           MOVE    WRK-KSN-IDX         TO  ORCSCWKSRY-KSN-IDX
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    "3"                 TO  ORCSCWKSRY-KBN
           MOVE    "K02N"              TO  ORCSCWKSRY-PG
           CALL    "ORCSCWKSRY"        USING
                                       ORCSCWKSRYAREA
                                       SPA-AREA
                                       SPA-K02
                                       SPA-SCR-REC
      *
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           MOVE    SPA-K081            TO  SPA-ETCKYOUTU-AREA
      *
           MOVE    "K08"               TO  SPA-NAI-SAKIPG
      *****MOVE    "K03"               TO  SPA-SAKIPG
           MOVE    SPA-NAI-SAKIPG      TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "K02N"              TO  SPA-HZN-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
      *
           IF      WRK-PUTTYPE         =   SPACE
               MOVE    "CHANGE"            TO  MCP-PUTTYPE
           ELSE
               MOVE    "JOIN"              TO  MCP-PUTTYPE
           END-IF
           MOVE    SPA-NAI-SAKIPG      TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       41002-KOUSIN-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    診療行為自動算定・算定チェック処理
      *****************************************************************
       4530-TBLCHI-SYORI-SEC          SECTION.
      *
      *    確定前、自動算定追加
           INITIALIZE                  ORCSC00AREA
           MOVE    "3"                 TO  LNK-ORCSC-KBN
           PERFORM 520-ALL-CHEK-SEC
      *
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO  TO  4530-TBLCHI-SYORI-EXT
           END-IF
      *
      *    自動作成の診療行為作成処理
           INITIALIZE                  WRK-KASAN-AREA
           MOVE    ZERO                TO  WRK-KSN-IDX
           MOVE    ZERO                TO  WRK-ZAINUM
           MOVE    ZERO                TO  IDX-S
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   10   )  OR
                              (IDX-S   >=  60   )
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL      (IDY     >   30      )  OR
                                  (IDX-S   >=  60   )
               IF      LNK-ORCSC-SRYCD (IDX IDY)
                                       NOT = SPACE
                   ADD     1               TO  IDX-S
                   ADD     1               TO  WRK-ZAINUM
                   MOVE    WRK-ZAINUM      TO  WRK-KSN-ZAINUM
                                                              (IDX-S)
                   MOVE    LNK-ORCSC-SRYSYUKBN (IDX IDY)
                                           TO  WRK-KSN-SRYSYUKBN
                                                              (IDX-S)
                   MOVE    LNK-ORCSC-SRYKBN    (IDX IDY)
                                           TO  WRK-KSN-SRYKBN
                                                              (IDX-S)
                   MOVE    LNK-ORCSC-SRYCD (IDX IDY)
                                           TO  WRK-KSN-SRYCD
                                                              (IDX-S)
                   MOVE    LNK-ORCSC-TENSU (IDX IDY)
                                           TO  WRK-KSN-TENSU  (IDX-S)
                   MOVE    "3"             TO  WRK-KSN-AUTOKBN(IDX-S)
      *
                   IF      LNK-ORCSC-SRYCD-KS(IDX IDY)   =   SPACE
                       MOVE    "1"         TO  WRK-KSN-ZAIEND (IDX-S)
                   ELSE
                       ADD     1               TO  IDX-S
                     IF      IDX-S           <=  60
                       MOVE    WRK-ZAINUM      TO  WRK-KSN-ZAINUM
                                                              (IDX-S)
                       MOVE    LNK-ORCSC-SRYCD-KS(IDX IDY)
                                               TO  WRK-KSN-SRYCD
                                                              (IDX-S)
      *                コメントの入力値（算定日）
                       MOVE    LNK-ORCSC-ATAI-G  (IDX IDY)
                                               TO  WRK-KSN-ATAI-G
                                                               (IDX-S)
      *
                       MOVE    LNK-ORCSC-SRYSYUKBN (IDX IDY)
                                           TO  WRK-KSN-SRYSYUKBN
                                                              (IDX-S)
                       MOVE    LNK-ORCSC-SRYKBN    (IDX IDY)
                                           TO  WRK-KSN-SRYKBN
                                                              (IDX-S)
                       MOVE    LNK-ORCSC-TENSU-KS (IDX IDY)
                                           TO  WRK-KSN-TENSU  (IDX-S)
                       MOVE    "1"         TO  WRK-KSN-ZAIEND (IDX-S)
                       MOVE    "3"         TO  WRK-KSN-AUTOKBN
                                                            (IDX-S)
                     END-IF
                   END-IF
      *            回数
                   PERFORM VARYING IDN1    FROM    1   BY  1
                           UNTIL   (IDN1    >   5  )    OR
                                   (LNK-ORCSC-INPUTCD (IDX IDY IDN1)
                                               =   SPACE)
                       MOVE    SPACE           TO  WRK-KSN-ZAIEND(IDX-S)
                       ADD     1               TO  IDX-S
                       IF      IDX-S           <=  60
                           MOVE    WRK-ZAINUM      TO  WRK-KSN-ZAINUM
                                                              (IDX-S)
                           MOVE    "1"     TO  WRK-KSN-ZAIEND (IDX-S)
                           MOVE    LNK-ORCSC-SRYSYUKBN (IDX IDY)
                                           TO  WRK-KSN-SRYSYUKBN
                                                              (IDX-S)
                           MOVE    LNK-ORCSC-SRYKBN    (IDX IDY)
                                           TO  WRK-KSN-SRYKBN
                                                              (IDX-S)
                           MOVE    LNK-ORCSC-INPUTCD (IDX IDY IDN1)
                                           TO  WRK-KSN-INPUTCD
                                                              (IDX-S)
                       END-IF
                   END-PERFORM
               END-IF
               END-PERFORM
           END-PERFORM
           MOVE    IDX-S               TO  WRK-KSN-IDX
      *
           .
       4530-TBLCHI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為データ出力処理
      *****************************************************************
       45109-SNRYO-NYU-SEC              SECTION.
      *
      *    入院指示せん発行チェック
           PERFORM 451091-SIJISEN-CHK-SEC
      *
           IF     (SPA-NAI-SIJISENKBN  =   1  )  AND
                 ((SPA-NAI-SYOHOU      =   1 )  OR
                  (SPA-NAI-CHUSYA      =   1 )  OR
                  (SPA-NAI-SIJISEN     =   1 ))
      *        入院指示せん発行あり
               PERFORM 450192-K081-SYORI-SEC
           ELSE
      *        確認メッセージ
               MOVE    "0117"              TO  SPA-KIDCD
               MOVE    SPACE               TO  SPA-KID1-FLG
           END-IF
      *
           .
       45109-SNRYO-NYU-EXT.
           EXIT.
      *****************************************************************
      *    入院指示せん発行チェック
      *****************************************************************
       451091-SIJISEN-CHK-SEC              SECTION.
      *
           INITIALIZE                  SPA-K081
      *    システム管理検索
           MOVE    SPACE               TO  SYS-5007-REC
           INITIALIZE                  SYS-5007-REC
           MOVE    "5007"              TO  SYS-5007-KANRICD
           MOVE    "01"                TO  SYS-5007-KBNCD
           MOVE    SYS-5007-REC        TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           MOVE    SPA-SRYYMD          TO  ORC-DBYMD
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               PERFORM 960-KANRIMST-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-5007-REC
               INITIALIZE                  SYS-5007-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-5007-REC
           ELSE
               INITIALIZE                  SYS-5007-REC
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           IF      SYS-5007-SIJISENKBN =   "1"
               MOVE    1               TO  SPA-NAI-SIJISENKBN
           ELSE
               MOVE    ZERO            TO  SPA-NAI-SIJISENKBN
           END-IF
      *
      *    診療内容判定
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
               IF     (SPA-COM-SRYKBN (IDX)    =   "21"
                                               OR  "22"
                                               OR  "23" )
                   MOVE    1               TO  SPA-NAI-SYOHOU
               END-IF
               IF     (SPA-COM-SRYKBN (IDX)    =   "14" ) AND
                      (SPA-COM-INPUTCD(IDX)(1:1)   =   "6"
                                                   OR  "7")
                   MOVE    1               TO  SPA-NAI-SYOHOU
               END-IF
      *
               IF     (SPA-COM-SRYKBN (IDX)    =   "31"
                                               OR  "32"
                                               OR  "33" ) AND
                      (SPA-COM-INPUTCD(IDX)(1:1)   =   "6"
                                                   OR  "7")
                   MOVE    1               TO  SPA-NAI-CHUSYA
               END-IF
      *
               IF     (SPA-COM-SRYKBN (IDX)    =   "40"
                                               OR  "80" ) AND
                      (SPA-COM-INPUTCD(IDX)(1:1)   =   "1"
                                                   OR  "6" 
                                                   OR  "7")
                   MOVE    1               TO  SPA-NAI-SIJISEN
               END-IF
           END-PERFORM
           MOVE    SPA-K081            TO  SPA-ETCKYOUTU-AREA
      *
           .
       451091-SIJISEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院指示せん指示画面へ（Ｋ０８１）
      *****************************************************************
       450192-K081-SYORI-SEC              SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  SPA-MOTOPG
      *
           MOVE    SPA-K081            TO  SPA-ETCKYOUTU-AREA
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K02             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGK081"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           MOVE    SPA-ETCKYOUTU-AREA  TO  SPA-K081
      *
           MOVE    "K081"              TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
      *
           MOVE    "K02N"              TO  SPA-ETC-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K081"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       450192-K081-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ワーク行為負担データを作成、Ｋ０１へ
      *****************************************************************
       4510-SNRYO-NYU-SEC              SECTION.
      *
           MOVE    "1"                 TO  FLG-SYORI
           PERFORM 45101-SNRYO-SYORI-SEC
      *
      *    入院指示せん発行処理
           IF      FLG-SIJISEN         =   1
               PERFORM 45900-SIJISEN-SYORI-SEC
           END-IF
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
      *    患者ＩＤを初期化する
           INITIALIZE                  SPA-KYOTUKEY
      *
           INITIALIZE                  SPA-K02
           INITIALIZE                  SPA-SCR-REC
      *
      *     初期ＳＰＡ編集処理
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-SYSYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SYSYMDWH        TO  LNK-SC95-SRYYMDG
           MOVE    "9"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
           MOVE    1               TO  SPA-GMN-PAGE
      *
           .
       4510-SNRYO-NYU-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為データ出力処理
      *****************************************************************
       45101-SNRYO-SYORI-SEC              SECTION.
      *
           INITIALIZE                  ORCSCWKSRYAREA
           IF      FLG-SYORI           =   "1"
               MOVE    "2"                 TO  ORCSCWKSRY-KBN
           ELSE
               MOVE    "3"                 TO  ORCSCWKSRY-KBN
           END-IF
           MOVE    "K02N"              TO  ORCSCWKSRY-PG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           CALL    "ORCSCWKSRY"        USING
                                       ORCSCWKSRYAREA
                                       SPA-AREA
                                       SPA-K02
                                       SPA-SCR-REC
      *
           .
       45101-SNRYO-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院指示せん発行処理
      *****************************************************************
       45900-SIJISEN-SYORI-SEC            SECTION.
      *
      *    入院指示せんオーダーデータ作成
           INITIALIZE                  ORCSORDERPRTAREA
           MOVE    "K02N"              TO  LNK-ORDERPRT-PG
           IF      FLG-SYORI           =   "1"
      *        中途終了時
               MOVE    "1"                 TO  LNK-ORDERPRT-KBN
           ELSE
      *        パラメタより（印刷のみ）
               MOVE    "2"                 TO  LNK-ORDERPRT-KBN
           END-IF
      *
           IF      SPA-K081GMN-SYOHOU  =   "1"
               MOVE    1                   TO  LNK-ORDERPRT-SYOHOU
           END-IF
           IF      SPA-K081GMN-CHUSYA  =   "1"
               MOVE    1                   TO  LNK-ORDERPRT-CHUSYA
           END-IF
           IF      SPA-K081GMN-SIJISEN =   "1"
               MOVE    1                   TO  LNK-ORDERPRT-SIJISEN
           END-IF
           IF      SPA-K081GMN-DRCD    NOT =   SPACE
               MOVE    "1"                 TO  LNK-ORDERPRT-DRCD(1:1)
               MOVE    SPA-K081GMN-DRCD    TO  LNK-ORDERPRT-DRCD(2:4)
           END-IF
           CALL    "ORCSORDERPRT"      USING
                                       ORCSORDERPRTAREA
                                       SPA-AREA
           IF      LNK-ORDERPRT-RCD    NOT =   ZERO
               MOVE    "1200"              TO  SPA-ERRCD
           END-IF
      *
      *    帳票プログラム取得
           PERFORM 459001-PRTNUM-HEN-SEC
      *
      *    入院処方箋印刷
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-K081GMN-SYOHOU  =   "1"    )
               INITIALIZE              ORCHC501AREA
               MOVE    SPA-GMN-PTNUM       TO  ORCHC501-PTNUM
               MOVE    SPA-SRYYMD          TO  ORCHC501-SRYYMD
               MOVE    SPA-SYSYMD          TO  ORCHC501-SYSYMD
               MOVE    SPA-SRYKA           TO  ORCHC501-SRYKA
               MOVE    SPA-HKNCOMBINUM     TO  ORCHC501-HKNCOMBI
               MOVE    LNK-ORDERPRT-PRINTYMD   TO  ORCHC501-PRINTYMD
               MOVE    LNK-ORDERPRT-PRINTHMS   TO  ORCHC501-PRINTHMS
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   4
                   IF      SPA-K081GMN-CHK1KBN (IDX)   =   "1"
                       MOVE    1               TO  ORCHC501-CHKPRT(IDX)
                   END-IF
               END-PERFORM
               CALL    WRK-SYOHOU-PG       USING
                                           SPA-AREA
                                           ORCHC501AREA
               IF      ORCHC501-RC         NOT =   ZERO
                   MOVE    "1201"          TO  SPA-ERRCD
               END-IF
           END-IF
      *    注射処方箋印刷
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-K081GMN-CHUSYA  =   "1"    )
               INITIALIZE              ORCHC502AREA
               MOVE    SPA-GMN-PTNUM       TO  ORCHC502-PTNUM
               MOVE    SPA-SRYYMD          TO  ORCHC502-SRYYMD
               MOVE    SPA-SYSYMD          TO  ORCHC502-SYSYMD
               MOVE    SPA-SRYKA           TO  ORCHC502-SRYKA
               MOVE    SPA-HKNCOMBINUM     TO  ORCHC502-HKNCOMBI
               MOVE    LNK-ORDERPRT-PRINTYMD   TO  ORCHC502-PRINTYMD
               MOVE    LNK-ORDERPRT-PRINTHMS   TO  ORCHC502-PRINTHMS
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   4
                   IF      SPA-K081GMN-CHK2KBN (IDX)   =   "1"
                       MOVE    1               TO  ORCHC502-CHKPRT(IDX)
                   END-IF
               END-PERFORM
               CALL    WRK-CHUSYA-PG       USING
                                           SPA-AREA
                                           ORCHC502AREA
               IF      ORCHC502-RC         NOT =   ZERO
                   MOVE    "1201"          TO  SPA-ERRCD
               END-IF
           END-IF
      *    指示せん印刷
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-K081GMN-SIJISEN =   "1"    )
               INITIALIZE              ORCHC503AREA
               MOVE    SPA-GMN-PTNUM       TO  ORCHC503-PTNUM
               MOVE    SPA-SRYYMD          TO  ORCHC503-SRYYMD
               MOVE    SPA-SYSYMD          TO  ORCHC503-SYSYMD
               MOVE    SPA-SRYKA           TO  ORCHC503-SRYKA
               MOVE    SPA-HKNCOMBINUM     TO  ORCHC503-HKNCOMBI
               MOVE    LNK-ORDERPRT-PRINTYMD   TO  ORCHC503-PRINTYMD
               MOVE    LNK-ORDERPRT-PRINTHMS   TO  ORCHC503-PRINTHMS
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   4
                   IF      SPA-K081GMN-CHK3KBN (IDX)   =   "1"
                       MOVE    1               TO  ORCHC503-CHKPRT(IDX)
                   END-IF
               END-PERFORM
               CALL    WRK-SIJISEN-PG      USING
                                           SPA-AREA
                                           ORCHC503AREA
               IF      ORCHC503-RC         NOT =   ZERO
                   MOVE    "1201"          TO  SPA-ERRCD
               END-IF
           END-IF
           .
       45900-SIJISEN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票プログラム取得処理
      *****************************************************************
       459001-PRTNUM-HEN-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-SYOHOU-PG
           MOVE    SPACE               TO  WRK-CHUSYA-PG
           MOVE    SPACE               TO  WRK-SIJISEN-PG
      *    入院処方せん
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000005"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYS-1034-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               MOVE    "1202"              TO  SPA-ERRCD
           ELSE
               MOVE    ORCSPRTNM-PRTPG     TO  WRK-SYOHOU-PG
           END-IF
      *    注射処方せん
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000006"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYS-1034-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               MOVE    "1203"              TO  SPA-ERRCD
           ELSE
               MOVE    ORCSPRTNM-PRTPG     TO  WRK-CHUSYA-PG
           END-IF
      *    指示せん
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000007"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYS-1034-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               MOVE    "1204"              TO  SPA-ERRCD
           ELSE
               MOVE    ORCSPRTNM-PRTPG     TO  WRK-SIJISEN-PG
           END-IF
           .
       459001-PRTNUM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌更新前のチェック処理
      *****************************************************************
       41001-YAKKNK-CHK-SEC            SECTION.
      *
      *    禁忌チェック
           MOVE    2                   TO  FLG-ALLFLG
           MOVE    ZERO                TO  FLG-KINKI
           MOVE    ZERO                TO  IDX-K1
           INITIALIZE                  WRK-KNKCHK-AREA
      *    相互作用チェック期間
           IF      SPA-INTERACT-KIKAN  =   ZERO
                GO      TO      41001-YAKKNK-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  IDX-K1
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL       IDX-Z   >   SPA-GMN-MAX
               IF     (SPA-COM-INPUTCD(IDX-Z)(1:1)   =   "6" ) AND
                      (SPA-COM-KNKUMU (IDX-Z)  =   SPACE     )
                   MOVE    ZERO            TO  FLG-KINKI-ARI
                   PERFORM 510222-KNKYAK-CHK-SEC
                   IF      FLG-KINKI-ARI2      =   1
                       MOVE    "3"             TO  SPA-COM-CUR (IDX-Z)
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      FLG-KINKI           =   1
               MOVE    2                   TO  FLG-ALLFLG
               MOVE    "K03"               TO  SPA-K021-SAKI
               PERFORM 51020-K021-SYORI-SEC
           END-IF
      *
           .
       41001-YAKKNK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌エラーメッセージ表示処理
      *****************************************************************
       51020-K021-SYORI-SEC              SECTION.
      *
      *    同一薬剤を削除する
           PERFORM 51021-KNK-SORT-SEC
      *
      *    ＳＰＡセット
           INITIALIZE                  SPA-K021GMN-AREA
           MOVE    FLG-ALLFLG          TO  SPA-K021-SYORI
           MOVE    ZERO                TO  IDX-K
           PERFORM VARYING     IDX-K1      FROM    1   BY  1
                   UNTIL      (IDX-K1          >   30  )  OR
                              (IDX-K           >=  30  )
             IF        WRK-KNK-TOUSRYCD (IDX-K1)   NOT =   SPACE
               ADD     1                   TO  IDX-K
               MOVE    IDX-K               TO  SPA-K021-TSEQNO (IDX-K)
               MOVE    WRK-KNK-TOUMEI (IDX-K1)
                                           TO  SPA-K021-TTOUMEI(IDX-K)
               PERFORM VARYING     IDX-K2      FROM    1   BY  1
                       UNTIL      (IDX-K2          >   30  )  OR
                                  (IDX-K           >   30  )  OR
                                  (WRK-KNK-KNKMEI(IDX-K1 IDX-K2)
                                                   =   SPACE)
                   IF      IDX-K2              >   1
                       ADD     1                   TO  IDX-K
                       MOVE    IDX-K               TO  SPA-K021-TSEQNO
                                                               (IDX-K)
                   END-IF
                   MOVE    WRK-KNK-KNKMEI (IDX-K1 IDX-K2)
                                           TO  SPA-K021-TKNKMEI(IDX-K)
                   MOVE    WRK-KNK-TOUYMD (IDX-K1 IDX-K2)
                                           TO  SPA-K021-TTOUYMD (IDX-K)
                   MOVE    WRK-KNK-SYOJYOUCD (IDX-K1 IDX-K2)
                                           TO  SPA-K021-TSYOJYOUCD
                                                               (IDX-K)
                   IF      WRK-KNK-TOUYMDW (IDX-K1 IDX-K2) =   SPACE
                       MOVE    WRK-KNK-TOUYMD (IDX-K1 IDX-K2)
                                               TO  WRK-SYMD
                       PERFORM 520-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG
                                           TO  SPA-K021-TTOUYMDW(IDX-K)
                   ELSE
                       MOVE    "今回入力"  TO  SPA-K021-TTOUYMDW(IDX-K)
                   END-IF
               END-PERFORM
             END-IF
           END-PERFORM
           MOVE    IDX-K               TO  SPA-K021-MAX
      *
      *    初期画面セット
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K02             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGK021"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-K02
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
           MOVE    "K021"              TO  SPA-SAKIPG
           MOVE    "K02N"              TO  SPA-MOTOPG
      *
           MOVE    "K02N"              TO  SPA-HZN-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K021"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       51020-K021-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    全件　禁忌薬剤 同一薬剤を削除処理
      *****************************************************************
       51021-KNK-SORT-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-KNK-MAX
           PERFORM VARYING     IDX-K1      FROM    1   BY  1
                   UNTIL      (IDX-K1          >   30  )  OR
                              (WRK-KNK-TOUSRYCD (IDX-K1)  =   SPACE )
               ADD     1                   TO  WRK-KNK-MAX
      *
               MOVE    WRK-KNK-TBL2A(IDX-K1)   TO  WRK-SRT-TBL2A
               INITIALIZE                      WRK-KNK-TBL2A(IDX-K1)
               PERFORM VARYING     IDX-K       FROM    1   BY  1
                       UNTIL       IDX-K       >   30
                   PERFORM VARYING     IDX-K2      FROM    1   BY  1
                           UNTIL       IDX-K2      >   30
                       IF     (IDX-K           NOT =   IDX-K2)  AND
                              (WRK-SRT-SRYCD (IDX-K)
                                              =   WRK-SRT-SRYCD(IDX-K2))
                           IF      WRK-SRT-TOUYMD (IDX-K)
                                               <   WRK-SRT-TOUYMD
                                                               (IDX-K2)
                               MOVE    WRK-SRT-TOUYMD(IDX-K2)
                                                   TO  WRK-SRT-TOUYMD
                                                                (IDX-K)
                           END-IF
                           MOVE    SPACE           TO  WRK-SRT-TBL2
                                                           (IDX-K2)
                       END-IF
                   END-PERFORM
               END-PERFORM
               MOVE    ZERO                    TO  IDX-K2
               PERFORM VARYING     IDX-K       FROM    1   BY  1
                       UNTIL       IDX-K       >   30
                   IF      WRK-SRT-TBL2  (IDX-K)   NOT =   SPACE
                       ADD     1               TO  IDX-K2
                       MOVE    WRK-SRT-TBL2  (IDX-K)
                                               TO  WRK-KNK-TBL2
                                                       (IDX-K1 IDX-K2)
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    今回入力分は１明細のみとする
           PERFORM VARYING     IDX-K1      FROM    1   BY  1
                   UNTIL       IDX-K1          >   WRK-KNK-MAX
               PERFORM VARYING     IDX-K2      FROM    1   BY  1
                       UNTIL       IDX-K2      >   30
                   IF      WRK-KNK-TOUYMDW(IDX-K1 IDX-K2)
                                           NOT =   SPACE
                       PERFORM 510211-KNK-KONCHK-SEC
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           PERFORM VARYING     IDX-K1      FROM    1   BY  1
                   UNTIL       IDX-K1          >   WRK-KNK-MAX
               MOVE    WRK-KNK-TBL2A(IDX-K1)   TO  WRK-SRT-TBL2A
               INITIALIZE                      WRK-KNK-TBL2A(IDX-K1)
               MOVE    ZERO                TO  IDX-K2
               PERFORM VARYING     IDX-K       FROM    1   BY  1
                       UNTIL       IDX-K       >   30
                   IF      WRK-SRT-TBL2  (IDX-K)   NOT =   SPACE
                       ADD     1               TO  IDX-K2
                       MOVE    WRK-SRT-TBL2  (IDX-K)
                                               TO  WRK-KNK-TBL2
                                                       (IDX-K1 IDX-K2)
                   END-IF
               END-PERFORM
           END-PERFORM
     *
           .
       51021-KNK-SORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    今回入力分処理
      *****************************************************************
       510211-KNK-KONCHK-SEC             SECTION.
      *
           PERFORM VARYING     IDX-K3  FROM   IDX-K1 BY  1
                       UNTIL       IDX-K3  >   WRK-KNK-MAX
               IF      WRK-KNK-SRYCD (IDX-K1 IDX-K2)
                                           =   WRK-KNK-TOUSRYCD
                                                            (IDX-K3)
                   PERFORM VARYING     IDX-K4      FROM    1   BY  1
                           UNTIL       IDX-K4      >   30
                       IF     (WRK-KNK-TOUYMDW(IDX-K3 IDX-K4)
                                               NOT =   SPACE )  AND
                              (WRK-KNK-SRYCD (IDX-K3 IDX-K4)
                                                   =   WRK-KNK-TOUSRYCD
                                                            (IDX-K1))
                           MOVE    SPACE           TO  WRK-KNK-TBL2
                                                        (IDX-K3 IDX-K4)
                       END-IF
                   END-PERFORM
                   IF      WRK-KNK-TBL2A (IDX-K3)  =   SPACE
                        MOVE    SPACE       TO  WRK-KNK-TOUSRYCD
                                                            (IDX-K3)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       510211-KNK-KONCHK-EXT.
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
           END-IF
           .
       520-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
      *    老人保険編集エラーがある時、エラーとする
           IF      SPA-RJN-ERR         =   1
               MOVE    "1027"              TO  SPA-ERRCD
           END-IF
      *    保険エラーがある時、エラーとする
           IF      SPA-HKN-ERR         =   1
               MOVE    "1028"              TO  SPA-ERRCD
           END-IF
           IF      SPA-HKN-ERR         =   2
               MOVE    "1032"              TO  SPA-ERRCD
           END-IF
      *    保険未選択の場合、エラー
           IF      SPA-HKN-ERR         =   3
               MOVE    "1006"              TO  SPA-ERRCD
           END-IF
      *    保険選択エラーの場合
           IF      SPA-HKN-ERR         =   4
               MOVE    "1034"              TO  SPA-ERRCD
               MOVE    5                   TO  SPA-GMN-CUR
           END-IF
      *
      *    数量ゼロの場合、エラー表示しない
           IF      SPA-ERRCD           =   "A001"
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
      *    訂正時の登録エラー
           IF     (SPA-TEISEI-ERR      =   1  )  AND
                  (SPA-TEISEI-CHK      =   ZERO)
               MOVE    "K999"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-TEISEI-CHK
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-KIDCD       NOT =   SPACE
               PERFORM 520-KIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-KIDCD2      NOT =   SPACE
               PERFORM 530-KID2SET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "K02N"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    行位置とカーソル位置の決定処理
      *****************************************************************
       5002-GMNSPA-HEN-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  WRK-MAP-MAX
           MOVE    ZERO                TO  WRK-MAP-CUR
           MOVE    ZERO                TO  WRK-MAP-CUR2
           MOVE    ZERO                TO  WRK-MAP-CUR3
           MOVE    ZERO                TO  WRK-MAP-MAXPAGE
           MOVE    ZERO                TO  WRK-MAP-PAGE
           MOVE    ZERO                TO  WRK-MAP-PAGE2
           MOVE    ZERO                TO  WRK-MAP-PAGE3
           MOVE    1                   TO  WRK-PAGE
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
      *
           PERFORM VARYING     WRK-IDX     FROM    1   BY  1
                   UNTIL       WRK-IDX     >   WRK-GYO-MAX
               MOVE    ZERO                TO  SPA-COM-PAGE (WRK-IDX)
               MOVE    ZERO                TO  SPA-COM-GYOU (WRK-IDX)
      *        画面表示外のものに頁、行位置を設定する
               IF      SPA-COM-SET    (WRK-IDX)  =   SPACE
                   ADD     1                 TO  IDX
                   IF      IDX               >   24
                       ADD     1             TO  WRK-PAGE
                       MOVE    1             TO  IDX
                   END-IF
                   IF     (SPA-GMN-INPUTCD(WRK-IDX)  NOT =   SPACE) OR
                          (SPA-COM-CUR (WRK-IDX)     NOT =   SPACE)
                       MOVE    IDX             TO  WRK-MAP-MAX
                       MOVE    WRK-PAGE        TO  WRK-MAP-MAXPAGE
      *                エラー位置
                       IF      SPA-COM-CUR (WRK-IDX)   =   "1"  OR "3"
                           IF      WRK-MAP-CUR         =   ZERO
                               MOVE    IDX            TO  WRK-MAP-CUR
                              MOVE    WRK-PAGE        TO  WRK-MAP-PAGE
                           END-IF
                       END-IF
      *                警告位置
                       IF      SPA-COM-CUR (WRK-IDX)   =   "2"
                           IF      WRK-MAP-CUR2        =   ZERO
                               MOVE    IDX             TO  WRK-MAP-CUR2
                               MOVE    WRK-PAGE        TO  WRK-MAP-PAGE2
                           END-IF
                       END-IF
      *                フリーコメントで未入力の行
                       IF     (SPA-COM-IN2 (WRK-IDX)   =   "3" ) AND
                              (SPA-COM-IN  (WRK-IDX)   =   "1" )
                           MOVE    IDX             TO  WRK-MAP-CUR3
                           MOVE    WRK-PAGE        TO  WRK-MAP-PAGE3
                       END-IF
                   END-IF
      *
                   MOVE    WRK-PAGE        TO  SPA-COM-PAGE(WRK-IDX)
                   MOVE    IDX             TO  SPA-COM-GYOU(WRK-IDX)
      *
      *************IF      SPA-ERRCD       =   SPACE
                   IF      SPA-COM-CUR (WRK-IDX)   =   "1"
                       MOVE    SPACE       TO  SPA-MAE-INPUTCD(WRK-IDX)
                   ELSE
                       IF      SPA-COM-CUR (WRK-IDX)   NOT =   "2"
                           MOVE    SPACE       TO  SPA-COM-CUR (WRK-IDX)
                       END-IF
                   END-IF
               END-IF
      *
               IF      (SPA-COM-INPUTCD(WRK-IDX)   =   SPACE ) AND
                       (SPA-GMN-INPUTCD(WRK-IDX)   =   SPACE )
                   CONTINUE
               ELSE
                   ADD     1               TO  SPA-GMN-MAX
               END-IF
           END-PERFORM
      *
      *    画面カーソル位置決定（警告はエラーの次に）
           MOVE    ZERO                TO  SPA-MAP-IDX
           IF      WRK-MAP-CUR         =   ZERO
               IF      WRK-MAP-CUR2    NOT =   ZERO
                   MOVE    WRK-MAP-CUR2        TO  SPA-MAP-IDX
                   MOVE    WRK-MAP-PAGE2       TO  SPA-GMN-PAGE
               END-IF
           ELSE
               MOVE    WRK-MAP-CUR         TO  SPA-MAP-IDX
               MOVE    WRK-MAP-PAGE        TO  SPA-GMN-PAGE
           END-IF
      *
           IF      SPA-MAP-IDX     NOT =   ZERO
               GO      TO      5002-GMNSPA-HEN-EXT
           END-IF
      *    名称入力へ
           IF      WRK-MAP-CUR3    NOT =   ZERO
               MOVE    WRK-MAP-CUR3        TO  SPA-MAP-IDX
               MOVE    WRK-MAP-PAGE3       TO  SPA-GMN-PAGE
               MOVE    2                   TO  SPA-GMN-CUR
               GO      TO      5002-GMNSPA-HEN-EXT
           END-IF
      *
      *    カーソル指定のない時、次ぎの空白行へ
      *    後画面へ
           IF      SPA-GMN-CUR         =   88  OR  99
               IF      WRK-MAP-MAXPAGE     =   SPA-GMN-PAGE
                   COMPUTE SPA-MAP-IDX         =   WRK-MAP-MAX
                                               +   1
               ELSE
                   MOVE    1                   TO  SPA-MAP-IDX
               END-IF
           ELSE
               COMPUTE SPA-MAP-IDX         =   WRK-MAP-MAX
                                           +   1
               IF      SPA-SINSATU-SAI     =   ZERO
      *            初期表示の時１ページ目へ
                   IF     (SPA-MAP-IDX         >   24  )
                       OR (SPA-GMN-PAGE        <   WRK-MAP-MAXPAGE)
                       MOVE    99                  TO  SPA-MAP-IDX
                   ELSE
                       IF      SPA-GMN-PAGE    >   WRK-MAP-MAXPAGE
                           MOVE    WRK-MAP-MAXPAGE     TO  SPA-GMN-PAGE
                       END-IF
                   END-IF
               ELSE
                   MOVE    ZERO            TO  SPA-SINSATU-SAI
                   IF     (SPA-MAP-IDX         >   24 )
                       MOVE    99                  TO  SPA-MAP-IDX
                   ELSE
                       MOVE    WRK-MAP-MAXPAGE     TO  SPA-GMN-PAGE
                   END-IF
               END-IF
           END-IF
           IF      SPA-GMN-PAGE        =   ZERO
               MOVE    1               TO  SPA-GMN-PAGE
           END-IF
      *
           .
       5002-GMNSPA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
      *    行位置とカーソル位置の決定
           PERFORM 5002-GMNSPA-HEN-SEC
      *
           IF     (FLG-GMN-INIT        =   1   )  OR
                  (SPA-GMN-MAX1        =   ZERO )
               MOVE    ZERO                TO  WRK-K02-ROW
           ELSE
               MOVE    K02N-ROW            TO  WRK-K02-ROW
           END-IF
      *
           MOVE    SPACE               TO  K02N
           INITIALIZE                      K02N
      *
      *    コラムリストの位置指定
           MOVE    ZERO                TO  K02N-ROW
           MOVE    ZERO                TO  K02N-ROWATTR
           IF      WRK-K02-ROW     NOT =   ZERO
               MOVE    WRK-K02-ROW         TO  K02N-ROW
           END-IF
      *
           MOVE    ZERO                TO  IDX
           MOVE    1                   TO  WRK-PAGE
      *
           PERFORM VARYING     WRK-IDX     FROM    1   BY  1
                   UNTIL      (WRK-IDX     >   WRK-GYO-MAX  )  OR
                              (IDX         >=  24   )
      *        画面表示
               IF      SPA-COM-PAGE(WRK-IDX)   =   SPA-GMN-PAGE
                   ADD     1               TO  IDX
                   MOVE    SPA-GMN-SRYKBN (WRK-IDX)
                                               TO  K02N-SRYKBN(IDX)
                   MOVE    SPA-GMN-INPUTCD(WRK-IDX)
                                               TO  K02N-INPUTCD(IDX)
                   MOVE    SPA-GMN-MEISYO-G(WRK-IDX)
                                               TO  K02N-MEISYO(IDX)
                   PERFORM 5002-SURYO-HEN-SEC
               END-IF
      *
           END-PERFORM
      *
      *    ページ表示
           MOVE    SPACE               TO  WRK-HEN-PAGE
           MOVE    "頁"                TO  WRK-HEN1
           IF      WRK-MAP-MAXPAGE     >   ZERO
               MOVE    SPA-GMN-PAGE        TO  WRK-HEN-PAGE1
               MOVE    "/"                 TO  WRK-HEN2
               MOVE    WRK-MAP-MAXPAGE     TO  WRK-HEN-PAGE2
           END-IF
           MOVE    WRK-HEN-PAGE        TO  K02N-MODE
      *
      *    前頁表示の時、１行目へ
           IF      SPA-GMN-CUR         =   99  OR  88
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
      *    明細入力の時、コードが入力されていること
           IF      SPA-GMN-CUR         =   2
               IF      K02N-INPUTCD(SPA-MAP-IDX)    =   SPACE
                   MOVE    1               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-PTNUM       TO  K02N-PTNUM
           MOVE    SPA-GMN-KANANAME    TO  K02N-KANANAME
           MOVE    SPA-GMN-NAME        TO  K02N-NAME
           MOVE    SPA-GMN-SEX         TO  K02N-SEX
           MOVE    SPA-GMN-HKNCOMBI-G
                                       TO  K02N-HKNCOMBI
           MOVE    SPA-GMN-FTNRATEMEI  TO  K02N-RATE
      *    診療日
           IF      SPA-GMN-SRYYMD      =   SPACE
               MOVE    SPA-SRYYMDW         TO  K02N-SRYYMD
           ELSE
               MOVE    SPA-GMN-SRYYMD      TO  K02N-SRYYMD
           END-IF
           MOVE    SPA-GMN-BIRTHDAY    TO  K02N-BIRTHDAY
      *    年齢
           MOVE    SPA-NENREI-HEN      TO  K02N-NENREI
      *
      *    保険組合せ
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-HKN-MAX
               MOVE    SPA-GMN-HKN-LIST (IDX)  TO  K02N-HKNCOMBI-ITEM
                                                             (IDX)
           END-PERFORM
           MOVE    SPA-HKN-MAX         TO  K02N-HKNCOMBI-COUNT
      *
      *    診療科
           MOVE    SPA-GMN-SRYKA-G     TO  K02N-SRYKA
           PERFORM  VARYING   IDX     FROM    1   BY  1
                    UNTIL     IDX         >   SPA-SRYKA-MAX
               MOVE    SPA-GMN-SRYKALST (IDX)  TO  K02N-SRYKA-ITEM
                                                             (IDX)
           END-PERFORM
           MOVE    SPA-SRYKA-MAX       TO  K02N-SRYKA-COUNT
      *
      *    入院期間  
           MOVE    SPA-K02N-NYUINKIKAN TO  K02N-NYUINKIKAN
      *    入院科 
           MOVE    SPA-K02N-NYUINKAMEI TO  K02N-NYUINKA
      *
      *    時間外区分
           MOVE    SPA-K02N-JIKANKBN-G TO  K02N-JIKANKBN
           PERFORM  VARYING   IDX      FROM    1   BY  1
                    UNTIL     IDX      >   5
               MOVE    SPA-K02N-JIKANKBN-LST(IDX)
                                       TO  K02N-JIKANKBN-ITEM (IDX)
           END-PERFORM
           MOVE    5                   TO  K02N-JIKANKBN-COUNT
      *
      *    ＤＯ診療日
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-GMN-MAX1
               MOVE    SPA-GMN-NO     (IDX)    TO  WRK-ZZ
               MOVE    WRK-ZZ                  TO  K02N-NO  (IDX)
               MOVE    SPA-GMN-RRKYMD (IDX)    TO  K02N-RRKYMD(IDX)
               MOVE    SPA-GMN-RRKHKNCOMBI(IDX)
                                               TO  K02N-RRKHKNCOMBI(IDX)
               MOVE    SPA-GMN-RRKSRYKA(IDX)   TO  K02N-RRKSRYKA(IDX)
               IF      SPA-GMN-NO (IDX)    =   SPA-NAI-TESELNUM
                   MOVE    "T"             TO  K02N-SELECT  (IDX)
               ELSE
                   MOVE    "F"             TO  K02N-SELECT  (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-MAX1        TO  K02N-COUNT
      *
           MOVE    SPA-GMN-DOSELNUM    TO  K02N-DOSELNUM
      *
      *    訂正・処理
           IF      SPA-GMN-TEISEI      =   "T"
      *        訂正診療日選択以外の時、ＤＯ検索へ戻す
               IF      SPA-GMN-CUR         NOT =   4
                   MOVE    "F"             TO  SPA-GMN-TEISEI
                   MOVE    ZERO            TO  SPA-NAI-SYORI
               END-IF
           END-IF
           EVALUATE    SPA-GMN-TEISEI
               WHEN    "T"
                   MOVE    "T"             TO  K02N-TEISEI-VALUE
                   MOVE    "訂正診療日"    TO  K02N-TEISEI-LABEL
                   MOVE    1               TO  SPA-NAI-SYORI
                   MOVE    "T"             TO  SPA-GMN-TEISEI
               WHEN    "F"
               WHEN    SPACE
                   MOVE    "F"             TO  SPA-GMN-TEISEI
                   MOVE    "F"             TO  K02N-TEISEI-VALUE
                   MOVE    "ＤＯ検索"      TO  K02N-TEISEI-LABEL
                   MOVE    ZERO            TO  SPA-NAI-SYORI
           END-EVALUATE
      *
      *    処理
           EVALUATE    SPA-SYORIFLG
               WHEN    0
                   MOVE    SPACE           TO  K02N-SYORIMEI-VALUE
               WHEN    1
                   MOVE    "red"           TO  K02N-SYORIMEI-STYLE
                   MOVE    "［訂　正］"    TO  K02N-SYORIMEI-VALUE
           END-EVALUATE
      *
      *    継続
           EVALUATE    SPA-CONTKBN
               WHEN    SPACE
                   MOVE    SPACE           TO  K02N-CONTKBN-VALUE
               WHEN    "1"
                   MOVE    "red"           TO  K02N-CONTKBN-STYLE
                   MOVE    "［継］"        TO  K02N-CONTKBN-VALUE
               WHEN    "2"
                   MOVE    "red"           TO  K02N-CONTKBN-STYLE
                   MOVE    "［確］"        TO  K02N-CONTKBN-VALUE
           END-EVALUATE
      *
      *    急性発症日
           IF      SPA-GMN-KYUSEI     NOT =   SPACE
               MOVE    SPA-GMN-KYUSEI     TO  K02N-TABOO
               MOVE    SPA-GMN-KYUSEI     TO  K02N-TABOO-ITEM(1)
               MOVE    1                  TO  IDY
           ELSE
      *    禁忌
               MOVE    SPA-GMN-TABOO (1)  TO  K02N-TABOO
               MOVE    ZERO               TO  IDY
           END-IF
           PERFORM  VARYING   IDX     FROM    1   BY  1
                    UNTIL     IDX         >   SPA-TABOO-MAX
               ADD     1                      TO  IDY
               MOVE    SPA-GMN-TABOO (IDX)    TO  K02N-TABOO-ITEM
                                                             (IDY)
           END-PERFORM
      **** MOVE    SPA-TABOO-MAX       TO  K02N-TABOO-COUNT
           MOVE    IDY                 TO  K02N-TABOO-COUNT
      *
      *    非活性処理
           PERFORM 510-GSRAUTH-SEC
      *
           IF      FLG-END             =   ZERO
      *         カーソルセット
                PERFORM 5001-MAPCUR-SEC
            END-IF
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
      *業務処理権限設定
      *    受付
           IF      SPA-GSRAUTH-11      =   "1"
               MOVE    WIDGET-NORMAL       TO  K02N-B04S-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  K02N-B04S-STATE
           END-IF
      *    患者登録
           IF      SPA-GSRAUTH-12      =   "1"
               MOVE    WIDGET-NORMAL       TO  K02N-B05S-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  K02N-B05S-STATE
           END-IF
      *    病名登録
           IF      SPA-GSRAUTH-22      =   "1"
               MOVE    WIDGET-NORMAL       TO  K02N-B07S-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  K02N-B07S-STATE
           END-IF
      *    収納登録
           IF      SPA-GSRAUTH-23      =   "1"
               MOVE    WIDGET-NORMAL       TO  K02N-B08S-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  K02N-B08S-STATE
           END-IF
      *    会計照会
           IF      SPA-GSRAUTH-24      =   "1"
               MOVE    WIDGET-NORMAL       TO  K02N-B09S-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  K02N-B09S-STATE
           END-IF
      *
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    画面数量項目編集処理
      *****************************************************************
       5002-SURYO-HEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-ZAIKEI-H
           MOVE    SPACE               TO  WRK-KAISU-H 
           MOVE    SPACE               TO  WRK-KEI-H 
      *
      *    点数
           MOVE    SPA-GMN-TENSU(WRK-IDX)  TO  WRK-TENSU-X
      *    回数
           MOVE    ZERO                TO  IDK2
      *    外用薬の時、１
           IF     (SPA-COM-SRYKBN (WRK-IDX)    =   "23" )  AND
                  (SPA-GMN-KAISU  (WRK-IDX)    NOT =   SPACE )
               MOVE    1                   TO  WRK-KAISU-Z
           ELSE
               MOVE    SPA-GMN-KAISU  (WRK-IDX)    TO  WRK-KAISU-Z
           END-IF
           PERFORM VARYING     IDK1    FROM    1   BY  1
                   UNTIL       IDK1    >   4
               IF      WRK-KAISU-X (IDK1:1)    NOT =   SPACE
                   ADD     1           TO  IDK2
                   MOVE    WRK-KAISU-X (IDK1:1)
                                       TO  WRK-KAISU-H (IDK2:1)
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  WRK-ZAIKEI-H
           IF      SPA-GMN-KAISU (WRK-IDX) =   SPACE
               MOVE    SPACE               TO  WRK-KAKERU
           ELSE
               IF     (SPA-GMN-KEI   (WRK-IDX) =   ZERO )  AND
                      (SPA-COM-SRYKBN(WRK-IDX)
                                           NOT =   "21"  AND "22"
                                               AND "23" )
                   MOVE    SPACE               TO  WRK-KAISU-H
                   MOVE    SPACE               TO  WRK-KAKERU
               ELSE
                   MOVE    " X "               TO  WRK-KAKERU
               END-IF
           END-IF
           STRING  WRK-TENSU-X             DELIMITED   BY  SIZE
                   WRK-KAKERU              DELIMITED   BY  SIZE
                   WRK-KAISU-H             DELIMITED   BY  SPACE
                   INTO                    WRK-ZAIKEI-H
           END-STRING
      *
      *    画面編集
           MOVE    SPACE               TO  WRK-SURYO-HENSYU
           MOVE    SPA-GMN-SURYO   (WRK-IDX)   TO  WRK-H-SURYO
           MOVE    SPA-GMN-TANINAME(WRK-IDX)   TO  WRK-H-TANINAME
           MOVE    WRK-ZAIKEI-H                TO  WRK-H-ZAIKEI
           MOVE    SPA-GMN-KEI (WRK-IDX)
                                       TO  WRK-KEI-Z
           MOVE    WRK-KEI-Z           TO  WRK-H-KEI
      *
           MOVE    WRK-SURYO-HENSYU    TO  K02N-SURYO (IDX)
           MOVE    "user-font"         TO  K02N-SURYO-STYLE(IDX)
      *
           .
       5002-SURYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
      *    カーソルセット
           MOVE    SPA-MAP-IDX         TO  WRK-IDX2
           IF      WRK-IDX2            =   ZERO
               MOVE    1               TO  WRK-IDX2
           END-IF
      *
           IF      SPA-GMN-CUR         =   1
               IF      WRK-IDX2            >   24
                   MOVE    09              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
      *    患者番号が空白の時、患者番号へ
           IF      K02N-PTNUM          =  SPACE
               MOVE    00              TO  SPA-GMN-CUR
           END-IF
      *
      *    排他エラーの時、患者番号へ
           IF      SPA-ERRCD           =   "9999"
               MOVE    00              TO  SPA-GMN-CUR
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    09
                   MOVE    "B07"           TO  MCP-WIDGET
               WHEN    00
                   MOVE    "PTNUM"         TO  MCP-WIDGET
               WHEN    01
                   MOVE    "INPUTCD"       TO  MCP-WIDGET
                   MOVE    WRK-IDX2        TO  MCP-WIDGET(8:2)
               WHEN    02
                   MOVE    "MEISYO"        TO  MCP-WIDGET
                   MOVE    WRK-IDX2        TO  MCP-WIDGET(7:2)
               WHEN    03
                   MOVE    "KEI"           TO  MCP-WIDGET
                   MOVE    WRK-IDX2        TO  MCP-WIDGET(4:2)
               WHEN    04
                   MOVE    "DOSELNUM"      TO  MCP-WIDGET
               WHEN    05
                   MOVE    "HKNCOMBI"      TO  MCP-WIDGET
               WHEN    06
                   MOVE    "SRYKA"         TO  MCP-WIDGET
               WHEN    07
                   MOVE    "SRYYMD"        TO  MCP-WIDGET
               WHEN    08
                   MOVE    "JIKANKBN"      TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    "K02"               TO  LNK-SC96-PG
           CALL    "ORCSC96"           USING    SPA-AREA
                                                ORCSC96AREA
           IF      SPA-ERRCD           =  "K008"
               MOVE    1               TO  SPA-NAI-ROUSTR
           END-IF
      *
           EVALUATE    SPA-ERRCD
               WHEN    "9999"
                   MOVE    "他端末で使用中です。"
                                               TO  SPA-ERRMSG
                   MOVE    SLOCK-MSG           TO  SPA-ERRMSG(21:)
           END-EVALUATE
      *
      *
           IF      SPA-ERRMSG2     NOT =   SPACE
      *        エラー画面２
               PERFORM 5101-KERR2-SET-SEC
               GO      TO      510-ERRSET-EXT
           END-IF
      *
           MOVE    SPACE               TO  KERR
           MOVE    SPA-ERRCD           TO  KERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  KERR-ERRMSG
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "KERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示２処理
      *****************************************************************
       5101-KERR2-SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  KERR2
           MOVE    SPA-ERRCD           TO  KERR2-ERRCODE
           MOVE    SPA-ERRMSG2         TO  KERR2-ERRMSG(1:80)
           MOVE    SPA-ERRMSG          TO  KERR2-ERRMSG2
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "KERR2"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "KERR2"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       5101-KERR2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-KIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-KIDCD
               WHEN    "0101"
                   MOVE
                   "すでに当日診察料を算定しています。"
                                       TO  WRK-KIDMSG(1:34)
                   MOVE
                   "ＯＫで同日再診へ振り替えます。"
                                       TO  WRK-KIDMSG(35:)
               WHEN    "0102"
                   MOVE    "外来管理加算は算定できません。削除します"
                                       TO  WRK-KIDMSG
               WHEN    "0103"
                   MOVE    "継続中です。患者設定画面へ遷移します。"
                                       TO  WRK-KIDMSG
               WHEN    "0104"
                   MOVE    "処理を終了します。よろしいですか？"
                                       TO  WRK-KIDMSG
               WHEN    "0105"
                   MOVE    "保険組合せが変更されました。"
                                       TO  WRK-KIDMSG
                   MOVE    "よろしいですか？"
                                       TO  WRK-KIDMSG(29:)
               WHEN    "0106"
                   MOVE    "同日再診が３回以上になります。"
                                       TO  WRK-KIDMSG
                   MOVE    "訂正のみ行うようにして下さい。"
                                       TO  WRK-KIDMSG(31:)
               WHEN    "0107"
                   MOVE     "診療科が変更されました。"
                                       TO  WRK-KIDMSG
                   MOVE     "現在の診療内容を残しますか？"
                                       TO  WRK-KIDMSG(25:)
               WHEN    "0123"
                   MOVE     "診療科が変更されました。"
                                       TO  WRK-KIDMSG
                   MOVE     "よろしいですか？"
                                       TO  WRK-KIDMSG(25:)
               WHEN    "0122"
                   MOVE    "この科の中途終了データが存在します。"
                                       TO  WRK-KIDMSG
                   MOVE    "ＯＫで中途終了内容へ置換えます。"
                                       TO  WRK-KIDMSG(41:)
               WHEN    "0108"
               MOVE
               "初診算定日を登録します。現在の診療内容はクリアします。"
                                       TO  WRK-KIDMSG
               WHEN    "0109"
                   MOVE
                   "既に診察料を他科で算定しています。"
                                       TO  WRK-KIDMSG(1:34)
                   MOVE
                   "ＯＫで同日再診料へ変更します。"
                                       TO  WRK-KIDMSG(35:)
               WHEN    "0110"
               MOVE
                   "初診料算定へ変更します。よろしいですか？"
                                       TO  WRK-KIDMSG
               WHEN    "0111"
                   MOVE
                   "診療日が未来日です。よろしいですか？"
                                       TO  WRK-KIDMSG
               WHEN    "0112"
                   MOVE    "初診料算定へ変更しますが、"
                                       TO  WRK-KIDMSG
                   MOVE    "転帰が未入力の病名が存在します。"
                                       TO  WRK-KIDMSG(27:)
                   MOVE    "よろしいですか？"
                                       TO  WRK-KIDMSG(59:)
               WHEN    "0113"
                   MOVE    "特定疾患処方管理加算が算定できます。"
                                       TO  WRK-KIDMSG
                   MOVE    "ＯＫで自動算定します。"
                                       TO  WRK-KIDMSG(37:)
               WHEN    "0114"
                   MOVE    "厚生労働大臣が定める患者の再診４回目以降へ"
                                       TO  WRK-KIDMSG
                   MOVE    "振替えます。よろしいですか？"
                                       TO  WRK-KIDMSG(43:)
               WHEN    "0115"
                   MOVE    "人工腎臓の再診４回目以降へ振替えます。"
                                       TO  WRK-KIDMSG
                   MOVE    "よろしいですか？"
                                       TO  WRK-KIDMSG(39:)
               WHEN    "0116"
                   MOVE    "診療行為内容をクリアします。"
                                       TO  WRK-KIDMSG
                   MOVE    "よろしいですか？"
                                       TO  WRK-KIDMSG(29:)
               WHEN    "0118"
                   MOVE    "この保険の中途終了データが存在します。"
                                       TO  WRK-KIDMSG
                   MOVE    "ＯＫで中途終了内容へ置換えます。"
                                       TO  WRK-KIDMSG(41:)
               WHEN    "0119"
                   MOVE    "既に他保険で受診済みです。"
                                       TO  WRK-KIDMSG
                   MOVE    "ＯＫで他保険にて算定済へ置換えます"
                                       TO  WRK-KIDMSG(27:)
               WHEN    "0117"
                   MOVE    "中途終了します。よろしいですか？"
                                       TO  WRK-KIDMSG
               WHEN    "0120"
                   MOVE    "診療日では退院済です。"
                                       TO  WRK-KIDMSG
                   MOVE    "外来の画面に遷移しますか？"
                                       TO  WRK-KIDMSG(23:)
               WHEN    "0180"
                   MOVE    "個別療法が１０単位を超えます。"
                                       TO  WRK-KIDMSG
                   MOVE    "逓減しますか？"
                                       TO  WRK-KIDMSG(31:)
               WHEN    "0125"
                   MOVE    "患者取消が選択されました。"
                                       TO  WRK-KIDMSG
                   MOVE    "表示内容をクリアします。よろしいですか？"
                                       TO  WRK-KIDMSG(27:)
               WHEN    OTHER
                   MOVE    SPA-KIDCD
                                       TO  WRK-KIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  KID1
           MOVE    SPA-KIDCD           TO  KID1-ID1CODE
           MOVE    WRK-KIDMSG          TO  KID1-ID1MSG
      *
           MOVE    "戻る"              TO  KID1-B01
           MOVE    "ＯＫ"              TO  KID1-B12
           EVALUATE    SPA-KIDCD
               WHEN    "0109"
               WHEN    "0118"
               WHEN    "0119"
               WHEN    "0107"
               WHEN    "0180"
                   MOVE    "ＮＯ"            TO  KID1-B01
           END-EVALUATE
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "KID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "KID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-KIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       530-KID2SET-SEC              SECTION.
      *
           EVALUATE    SPA-KIDCD2
               WHEN    "0117"
                   MOVE    "登録後、帳票を発行する時は各帳票の発行を、"
                                       TO  WRK-KIDMSG
                   MOVE    "発行しない時は登録を押下して下さい。"
                                       TO  WRK-KIDMSG(43:)
               WHEN    OTHER
                   MOVE    SPA-KIDCD2
                                       TO  WRK-KIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  KID2
           MOVE    SPA-KIDCD2          TO  KID2-ID2CODE
           MOVE    WRK-KIDMSG          TO  KID2-ID2MSG
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "K02N"              TO  SPA-MOTOPG
           MOVE    "KID2"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "KID2"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       530-KID2SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       900-SRYACT-NEXT-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタ読み込み
      *****************************************************************
       940-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       940-SINACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    ZERO                TO  TNS-YUKOSTYMD
           MOVE    ALL "9"             TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "TENSU-KEY"         TO  ORC-DBPATH
           MOVE    SPA-SRYYMD          TO  ORC-DBYMD
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
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
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "TENSU-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       919-TENSU-READ2-SEC         SECTION.
      *
           MOVE    ZERO                TO  TNS-YUKOSTYMD
           MOVE    ALL "9"             TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "TENSU-KEY4"        TO  ORC-DBPATH
           MOVE    SPA-SRYYMD          TO  ORC-DBYMD
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"            TO  MCP-FUNC
               MOVE    "TENSU-KEY4"         TO  ORC-DBPATH
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
           END-IF
      *
           PERFORM UNTIL     FLG-TENSU     =   1
               MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
      *
               MOVE    "DBFETCH"            TO  MCP-FUNC
               MOVE    "TENSU-KEY4"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-TENSU
               ELSE
                   MOVE    1               TO  FLG-TENSU
               END-IF
           END-PERFORM
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "TENSU-KEY4"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           .
       919-TENSU-READ2-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスター次読込
      *****************************************************************
       920-INPUTCD-NEXT-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
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
       920-INPUTCD-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスター読込
      *****************************************************************
       930-INPUTCD-READ-SEC         SECTION.
      *
           IF      WRK-PATH            =   "INPUTCD-KEY4"
               CONTINUE
           ELSE
               IF      WRK-CD-MCNT         =   20
                   MOVE    "INPUTCD-KEY2"      TO  WRK-PATH
               ELSE
                   MOVE    "INPUTCD-KEY"       TO  WRK-PATH
               END-IF
           END-IF
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *****MOVE    "INPUTCD-KEY"       TO  ORC-DBPATH
           MOVE    WRK-PATH            TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
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
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    WRK-PATH            TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       920-INPUTCD-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    チェックデータ読込
      *****************************************************************
       950-CHK-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  CHK-REC
               MOVE    ZERO                TO  FLG-CHKMST
           ELSE
               INITIALIZE                      CHK-REC
               MOVE    1                   TO  FLG-CHKMST
           END-IF
      *
           .
       950-CHK-READ-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為データ次読込
      *****************************************************************
       970-WKSRYACT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  WKSRYACT-REC
               MOVE    ZERO                TO  FLG-WKSRYACT
           ELSE
               INITIALIZE                      WKSRYACT-REC
               MOVE    1                   TO  FLG-WKSRYACT
           END-IF
      *
           .
       970-WKSRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力セットデータ次読込
      *****************************************************************
       975-INPUTSET-READ-SEC         SECTION.
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
       975-INPSET-READ-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    算定歴マスタ読み込み
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEI-REC
               MOVE    ZERO                TO  FLG-SANTEI
           ELSE
               INITIALIZE                      SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           .
       920-SANTEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
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
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       930-SRYACT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       930-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴マスター読込
      *****************************************************************
       910-PTNYUINRRK-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
               MOVE    ZERO            TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタデータ順読み
      *****************************************************************
       980-PARA-READ-SEC         SECTION.
      *
           READ    PARA-FILE 
               AT  END
                   MOVE    1           TO  FLG-PARA
               NOT AT  END
                   MOVE    ZERO        TO  FLG-PARA
           END-READ
      *
           .
       980-PARA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名読込
      *****************************************************************
       980-PTBYOMEI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC
               MOVE    ZERO            TO  FLG-PTBYOMEI
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           .
       980-PTBYOMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    相互作用マスタ読込
      *****************************************************************
       990-INTERACT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  INTERACT-REC
               MOVE    ZERO            TO  FLG-INTERACT
           ELSE
               INITIALIZE                  INTERACT-REC
               MOVE    1               TO  FLG-INTERACT
           END-IF
      *
           .
       990-INTERACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       960-KANRIMST-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       960-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＣＬＯＳＥ処理
      *****************************************************************
       990-DB-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       990-DB-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-GMN-PTID        TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
           END-IF
           .
       990-LOCK-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN2-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-PTID            TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
           END-IF
           .
       990-LOCK-IN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
      *    ワーク診療行為クリア
           PERFORM 4900-WKSRYACT-CLEAR-SEC
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
      *
