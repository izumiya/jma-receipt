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
       PROGRAM-ID.                 ORCGK02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為入力
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC−多々納  01/04/25  セット入力修正
      *  02.00.00    MCC−多々納  01/05/15  約束セット、セットコード漢字
      *                                     追加
      *  02.00.03    MCC−多々納  01/05/17  老人慢性疾患外来総合診療科の
      *                                     追加
      *  02.00.00    MCC−多々納  01/05/21  入力コードをセットコードを
      *                                     表示へ
      *  02.00.01    MCC-多々納   01/05/24  Ｋ０８追加
      *  02.00.02    MCC-多々納   01/05/30  薬剤一部負担金修正
      *  02.00.03    MCC-多々納   02/03/01  初期自動算定をサブプロへ
      *  02.00.04    MCC-多々納   02/03/20  Ｈ１４．４改正対応他
      *  02.00.05    MCC-多々納   02/04/04  訂正時の中途終了エラー追加
      *  02.00.06    MCC-多々納   02/04/12  自費の修正
      *  02.00.07    MCC-多々納   02/04/12  労災追加
      *  02.00.08    MCC-多々納   02/04/19  生活習慣病指導算定時の追加
      *  03.00.00    MCC-多々納   02/07/01  インターフェースの変更
      *  03.00.01    NACL-多々納  02/09/03  インターフェースの変更その２
      *                                     剤削除の入力をどこでもＯＫとした
      *  03.00.02    NACL-多々納  02/09/11  自賠責追加
      *  03.00.03    NACL−多々納 02/09/24  検索修正
      *  03.00.04    NACL−多々納 02/10/08  処方せんに在宅薬剤院外追加
      *                                     フリーコメントの修正など
      *  03.00.05    NACL−多々納 02/10/10  外来管理加算チェックの有無
      *                                     訂正時の薬情表示等
      *  03.00.06    NACL−多々納 02/10/11  老人判定の修正
      *  03.00.07    NACL−多々納 02/10/31  外来管理加算自動算定なしの追加
      *  03.00.08    NACL-多々納  02/11/11  ＤＵＭＭＹ診察料追加
      *  03.00.09    NACL-多々納  02/11/20  自賠責器材追加
      *  03.00.10    NACL-多々納  02/11/28  中途終了再表示修正
      *  03.00.11    NACL-多々納  02/12/26  前回処方のドクター変更
      *  03.00.12    NACL-多々納  03/02/19  約束セット名称編集修正
      *  03.00.13    NACL-多々納  03/03/05  排他制御処理追加
      *  03.00.14    NACL-多々納  03/04/07  入院日のチェック追加
      *  03.00.15    NACL-多々納  03/05/26  最終退院日編集追加
      *  03.00.16    NACL-多々納  03/06/27  カスタマイズ帳票名取得サブ追加
      *  03.00.17    NACL-多々納  03/07/17  入力修正他
      *  03.00.18    NACL-多々納  03/08/01  数量入力判定追加
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
           03  FLG-ADD             PIC 9(01).
      *    初期画面かの判定
           03  FLG-SYOKI           PIC 9(01).
      *
           03  FLG-NAIYAK          PIC X(01).
           03  FLG-TONPUK          PIC X(01).
           03  FLG-GAIYAK          PIC X(01).
           03  FLG-ARI             PIC 9(01).
      *
           03  FLG-SYOSIN          PIC 9(01).
      *    診察料算定済み
      **** 03  FLG-SINSATU          PIC 9(01).
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
      *    入院中
           03  FLG-NYUIN           PIC 9(01).
      *
           03  FLG-SYUBETU         PIC 9(01).
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
           03  IDX-Z2              PIC 9(04).
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
           03  IDX-GG              PIC 9(04).
      *****03  WRK-MAX             PIC 9(04).
           03  WRK-STR             PIC 9(04).
      *
           03  IDX-SET             PIC 9(04).
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
           03  WRK-GMN-IDX         PIC 9(04).
      *    診療会計用集計
           03  WRK-WKSRY-AREA.
               05  WRK-SYUTEN1             PIC  9(07).
               05  WRK-SYUTEN2             PIC  9(07).
               05  WRK-YKZTEN              PIC  9(07).
               05  WRK-KIZAITEN            PIC  9(07).
      *
               05  WRK-SYUTEN2-R           PIC  9(07).
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
      *    退院日
           03  WRK-TAIINYMD        PIC X(08).
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
      *
      *    自費用
           03  WRK-TENTTLKBN       PIC 9(03).
           03  WRK-HOZ-KEI         PIC 9(08).
      *
           03  WRK-KEI-ERRCD       PIC X(04).
           03  WRK-ERR-ERRCD       PIC X(04).
           03  WRK-KEI-IDX         PIC 9(04).
           03  WRK-RIGAKU-TBL.
               05  WRK-RIGAKU-OCCS       OCCURS  6.
                   07  WRK-RIGAKU-SRYCD       PIC  X(09).
                   07  WRK-RIGAKU-CNT         PIC  9(03).
      *            入院用追加
                   07  WRK-ORCSC-RIG-KBN       PIC  X(01).
                   07  WRK-ORCSC-RIG-RYO       PIC  X(01).
                   07  WRK-ORCSC-RIG-DAY-G.
                       09  WRK-ORCSC-RIG-DAY   PIC  9(01)
                                               OCCURS 31.
               05  WRK-ORCSC-SOUKI-G.
                   07  WRK-ORCSC-SOUKI-FLG PIC  9(01)
                                               OCCURS 31.
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
      *
      *    特定疾患自動算定チェック用
           03  FLG-KYUJITU         PIC 9(01).
           03  WRK-KEIYMD          PIC X(08).
      *
      *    処方せんＰＧ名
           03  WRK-SYOHOU-PG           PIC X(20).
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
      *
      *    初期業務識別区分
           03  WRK-GYOUMU-KBN          PIC X(04).
      *    ドクターコード
           03  WRK-DRCD                PIC X(05).
      *
           03  WRK-ERRCD1              PIC X(04).
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
           03  WRK-KASAN-TBL                       OCCURS   30.
               05  WRK-KSN-SRYSYUKBN       PIC X(03).
               05  WRK-KSN-SRYKBN          PIC X(02).
               05  WRK-KSN-SRYCD           PIC X(09).
               05  WRK-KSN-ATAI-G.
                   07  WRK-KSN-ATAI        PIC X(08)   OCCURS  4.
               05  WRK-KSN-TENSU           PIC 9(08).
               05  WRK-KSN-ZAINUM          PIC 9(08).
               05  WRK-KSN-ZAIEND          PIC X(01).
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
      *
      *    保存ＳＰＡ領域ワーク
       01  MAE-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //MAE-//.
      *
      *    保存ＳＰＡ領域ワーク
       01  WRK-HZN-SCR-AREA.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-HZN-//.
      *
      *    ワークテーブル２(算定用一時保存用）
      *          (CPK02SPASCR.INC の長さを変更したら、変更すること）
       01  WRK-HOZ-AREA.
           03  WRK-HGMN-TBL            OCCURS   200.
               05  WRK-HGMN-TBLREC.
                   07  WRK-HGMN-DATA   PIC X(150).
               05  WRK-HCOM-TBLREC.
                   07  WRK-HCOM-DATA   PIC X(800).
           03  WRK-HOZ-IDX             PIC 9(04).
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
      *    職員情報
           COPY    "CPSK1010.INC".
      *    帳票ＰＧ
           COPY    "CPSK1032.INC".
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
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
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
      *    処方箋印刷パラメタ（Ａ５）
           COPY    "CPORCHC19.INC".
      *
      *    薬情報印刷パラメタ
           COPY    "CPORCSCH30.INC".
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
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
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
           COPY    "K09.INC".
           COPY    "K10.INC".
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
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  MAE-SCR-REC
           INITIALIZE                  WRK-SCR-REC
      *
           MOVE    SPA-COMMON          TO  SPA-AREA.
           MOVE    SPA-FREE            TO  SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           MOVE    "K02"               TO  SPA-HZN-MOTOPG
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPACE               TO  SPA-ERRMSG2
           MOVE    ZERO                TO  FLG-END
      **** MOVE    ZERO                TO  FLG-SYORINASI
           MOVE    ZERO                TO  WRK-MAE-GMN-MAX
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
      *
           MOVE    "K02"           TO  SPA-HZN-MOTOPG
           MOVE    SPA-K01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-K02         TO  SPA-FREE
      *
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *TEST
      *   ACCEPT  WRK-SYSTIME         FROM    TIME
      *    DISPLAY "K02 START:" WRK-SYSTIME (1:2) ":" 
      *                         WRK-SYSTIME (3:2) ":" 
      *                         WRK-SYSTIME (5:2) ":" 
      *                         WRK-SYSTIME (7:2) "/" 
      *    DISPLAY "*** ORCSNPL    START  ***"
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
      *****MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   SPACE                TO  MCP-PUTTYPE.
           MOVE   "K02"                TO  MCP-WINDOW.
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
           MOVE    SPA-TERMID          TO  FILE-TERMID1
      *
           OPEN    INPUT               SPASCR-FILE
           IF      STS-SPASCR     NOT =   ZERO
               GO      TO      1002-K02SPA-SET-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE          SPA-SCR-REC
           MOVE    SPACE               TO  WRK-DATA-REC
           INITIALIZE          WRK-DATA-REC
           MOVE    ZERO                TO  FLG-SPASCR
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL       FLG-SPASCR  =   1
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
           CLOSE                       SPASCR-FILE
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
           MOVE    "2"                 TO  SPA-NYUGAIKBN
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
      *
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
      *************GO      TO      300-SCREEN-EXT
               WHEN    "KID2"
      *            確認画面２より
                   PERFORM 30011-KID2-SET-SEC
                   MOVE    2                   TO  FLG-GMN
      *************GO      TO      300-SCREEN-EXT
               WHEN    "K09"
      *            ＤＯ選択画面より
                   PERFORM 3013-K09-SET-SEC
                   MOVE    2                   TO  FLG-GMN
      ************ GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
      *
           IF      FLG-GMN         NOT =   ZERO
               IF      SPA-ERRCD       NOT =   SPACE
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
      *            エラー編集
                   PERFORM 510-ERRSET-SEC
                   GO      TO      300-SCREEN-EXT
               END-IF
               IF      SPA-KIDCD       NOT =   SPACE
                   PERFORM 520-KIDSET-SEC
                   GO      TO      300-SCREEN-EXT
               END-IF
               GO      TO      300-SCREEN-EXT
           END-IF
           MOVE    SPACE               TO  SPA-KIDCD
           MOVE    SPACE               TO  SPA-KIDCD2
      *
      *    入院機能の判定
           IF     (SPA-MOTOPG(1:3)     =   "M01" )
               PERFORM 3009-NYUINCHK-SEC
               IF      FLG-END             =   1
                   GO      TO      300-SCREEN-EXT
               END-IF
           END-IF
      *
      *    患者の確定していない時、クリア画面の表示、M01より
           IF    ( SPA-MOTOPG(1:3)     =   "M01"    )  OR
                 ((SPA-MOTOPG          =   "K03" )  AND
                   (SPA-PTNUM          =   SPACE    ))
               PERFORM 30010-INIT-GMN-SEC
               GO      TO      300-SCREEN-EXT
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "K01"
      *            診療選択より
                   IF     (SPA-ERRMSG(1:2)    =   "NO"  )  AND
                          (SPA-CONTFLG        =   ZERO  )
      *                戻る
                       MOVE    ZERO               TO  FLG-PARASET
                       PERFORM 3002-PARA-SET-SEC
                       IF      SPA-GMN-PTID   NOT =   ZERO
      *                    排他制御処理
                           PERFORM 990-LOCK-IN-SEC
                       END-IF
                   ELSE
                       MOVE    ZERO               TO  SPA-SAISIN-CHK
                       MOVE    2                  TO  FLG-PARASET
                       PERFORM 3002-PARA-SET-SEC
                       IF     (SPA-SYORIFLG        =   ZERO )
                          AND (SPA-WKSRY-HKNCOMBI  =   ZERO )
                          PERFORM 3009-SAIHEN-SEC
                       END-IF
                   END-IF
               WHEN    "P02"
      *            患者登録より再編集
                   INITIALIZE                  SPA-K02
                   MOVE    ZERO            TO  FLG-PARASET
                   PERFORM 3002-PARA-SET-SEC
                   IF      SPA-ERRMSG(1:2)    =   "NO"
      *                戻る
                       IF      SPA-PTNUM(1:1)     =   "*"
                           MOVE    SPACE          TO  SPA-PTNUM
                           PERFORM 30010-INIT-GMN-SEC
                       ELSE
                           PERFORM 3003-GMN-HEN-SEC
                       END-IF
                   ELSE
                       IF      SPA-PTNUM       =   SPACE
                           PERFORM 30010-INIT-GMN-SEC
                       ELSE
                           IF      SPA-SYORIFLG        =   ZERO
                               PERFORM 3009-SAIHEN-SEC
                           ELSE
                               PERFORM 3003-GMN-HEN-SEC
                           END-IF
                       END-IF
                   END-IF
               WHEN    "K02N"
      *            入院
                   INITIALIZE                  SPA-K02
                   IF      SPA-PTID        NOT =   ZERO
                       PERFORM 3009-SAIHEN-SEC
                   ELSE
                       PERFORM 30010-INIT-GMN-SEC
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
                   END-IF
                   PERFORM 3003-GMN-HEN-SEC
                   IF      SPA-MOTOPG      =   "S02"  OR  "J02" OR "C02"
                       PERFORM 3004-SAI-SET-SEC
                   END-IF
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
      *    入院機能の判定処理
      *****************************************************************
       3009-NYUINCHK-SEC              SECTION.
      *
      *    職員情報より業務識別の判定
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    ZERO                TO  SYS-1010-STYUKYMD
           MOVE    ALL "9"             TO  SYS-1010-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           IF      SPA-SRYYMD          =   SPACE
               MOVE    SPA-SYSYMD          TO  ORC-DBYMD
           ELSE
               MOVE    SPA-SRYYMD          TO  ORC-DBYMD
           END-IF
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
               PERFORM 960-KANRIMST-READ-SEC
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    SPACE               TO  WRK-GYOUMU-KBN
           PERFORM
                   UNTIL       FLG-SYSKANRI    =   1
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
      *        オペレータＩＤ判定
               IF      SPA-OPID            =   SYS-1010-USERID
      *TEST        MOVE    SPA-1010-????       TO  WRK-GYOUMU-KBN
      *てすとで入院とする
      *            入退院登録の権限がある時
                   IF      SYS-1010-GSRAUTH(36:1)   =  "1"
                       MOVE    "0211"              TO  WRK-GYOUMU-KBN
                   END-IF
                   MOVE    1                   TO  FLG-SYSKANRI
               ELSE
                   MOVE    "SYSKANRI-KEY2"    TO  ORC-DBPATH
                   PERFORM 960-KANRIMST-READ-SEC
               END-IF
           END-PERFORM
      *    入院業務を表示する
           IF      (SPA-BEDSU          >   ZERO )  AND
                   (WRK-GYOUMU-KBN     =   "0211")
               INITIALIZE              SPA-KYOTUKEY
               INITIALIZE              SPA-K02
               INITIALIZE              SPA-SCR-REC
               INITIALIZE              SPA-K01KYOUTU
               PERFORM 450-K02N-SYORI-SEC
           END-IF
      *
           .
       3009-NYUINCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｋ０２初期画面表示
      *****************************************************************
       30010-INIT-GMN-SEC              SECTION.
      *
           INITIALIZE              SPA-KYOTUKEY
           INITIALIZE              SPA-K02
           INITIALIZE              SPA-SCR-REC
      *
           MOVE    "2"                 TO  SPA-NYUGAIKBN
      *
      *    初期ＳＰＡ編集処理
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-SYSYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SYSYMDWH        TO  LNK-SC95-SRYYMDG
           MOVE    "1"                 TO  LNK-SC95-KBN
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
       3004-SAI-SET-SEC              SECTION.
      *
      *    収納からの戻りの時、未収額変更
           IF      SPA-MOTOPG      =   "S02"
               INITIALIZE                  ORCSC95AREA
               MOVE    "6"                 TO  LNK-SC95-KBN
               MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
               MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
               MOVE    "K02"               TO  LNK-SC95-PG
               CALL    "ORCSC95"           USING
                                           MCPAREA
                                           ORCSC95AREA
                                           SPA-AREA
                                           SPA-K01KYOUTU
                                           SPA-K02
      *
      *        未収金
               MOVE    SPA-MISYUMONEY      TO  SPA-GMN-MISYUMONEY
           END-IF
      *
           .
       3004-SAI-SET-EXT.
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
           IF     (SPA-MOTOPG          =   "P02"  OR  "U02" 
                                                  OR  "K02N" )  AND
                  (SPA-PTID        NOT =   SPA-GMN-PTID )
               IF      SPA-MOTOPG          =   "P02"  OR  "U02"
                   MOVE    SPA-GMN-SRYYMD      TO  SPA-SRYYMDW
                   MOVE    SPA-NAI-SRYYMD      TO  SPA-SRYYMD
               END-IF
               IF      SPA-SRYYMD          =   SPACE
                   MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
                   MOVE    SPA-SYSYMDWH        TO  SPA-SRYYMDW
               END-IF
      *
               IF      SPA-MOTOPG          =   "K01"
                   MOVE    SPA-DRCD            TO  WRK-DRCD
               END-IF
      *        初期ＳＰＡ編集処理
               INITIALIZE                  ORCSC95AREA
               MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
               MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
               MOVE    "1"                 TO  LNK-SC95-KBN
               MOVE    "K02"               TO  LNK-SC95-PG
               CALL    "ORCSC95"           USING
                                           MCPAREA
                                           ORCSC95AREA
                                           SPA-AREA
                                           SPA-K01KYOUTU
                                           SPA-K02
               IF      SPA-MOTOPG          =   "K01"
                   MOVE    WRK-DRCD            TO  SPA-DRCD
               END-IF
           END-IF
           IF      SPA-PTID            =   ZERO
               GO      TO      3009-SAIHEN-EXT
           END-IF
      *
           INITIALIZE                  ORCSC95AREA
           MOVE    "2"                 TO  LNK-SC95-KBN
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
           IF     (SPA-MOTOPG          =   "K01"        )  AND
                  (SPA-PTID            =   SPA-GMN-PTID )
               MOVE    "K01"               TO  LNK-SC95-PG
           ELSE
               IF      SPA-MOTOPG          =   "U02"
                   MOVE    "U02"               TO  LNK-SC95-PG
               ELSE
                   MOVE    "K02"               TO  LNK-SC95-PG
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
      *    排他制御処理
           PERFORM 990-LOCK-IN2-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      3009-SAIHEN-EXT
           END-IF
      *
           IF     SPA-HKNCOMBINUM      =   SPACE  OR  ZERO
      *        スパ共通編集処理
               PERFORM                 310-SPA-SET-SEC
               MOVE   "1006"           TO  SPA-ERRCD 
               MOVE    1               TO  SPA-GMN-PAGE
               MOVE    00              TO  SPA-GMN-CUR
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
               INITIALIZE                  ORCSC95AREA
               MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
               MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
               MOVE    "1"                 TO  LNK-SC95-KBN
               MOVE    "K10"               TO  LNK-SC95-PG
               CALL    "ORCSC95"           USING
                                           MCPAREA
                                           ORCSC95AREA
                                           SPA-AREA
                                           SPA-K01KYOUTU
                                           SPA-K02
      *
               INITIALIZE                  ORCSC95AREA
               MOVE    "2"                 TO  LNK-SC95-KBN
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
      *        中途終了分のみ表示
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
           IF     SPA-HKNCOMBINUM      =   SPACE  OR  ZERO
      *        スパ共通編集処理
               PERFORM                 310-SPA-SET-SEC
               MOVE   "1006"           TO  SPA-ERRCD 
               MOVE    1               TO  SPA-GMN-PAGE
               MOVE    00              TO  SPA-GMN-CUR
               GO      TO      30091-SAIHEN2-EXT
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
                   MOVE    WRK-SPA-PTNUM   TO  K02-PTNUM
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
                              (SPA-K98-SRYCD(IDX-K98)  =   SPACE )
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
                   MOVE    SPA-K98-SRYCD(IDX-K98)
                                           TO  SPA-GMN-INPUTCD
                                                       (SPA-GMN-IDX)
               END-IF
      *
               MOVE    "1"                 TO  SPA-COM-IN (SPA-GMN-IDX)
               MOVE    "1"                 TO  SPA-COM-IN2(SPA-GMN-IDX)
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
               WHEN    "0101"
                   MOVE    SPACE               TO  SPA-KIDCD
                   MOVE    ZERO                TO  SPA-SYOSIN
                   PERFORM 30019-DOUSAISIN-SEC
      ************ MOVE    SPACE               TO  SPA-KIDCD
      *            診療料　自動修正
      *            MOVE    SPA-CHK-IDX         TO  SPA-GMN-IDX
      *            入力分変換処理
      *            MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *            INITIALIZE                      ORCSC10SAREA
      *            MOVE    "5"                 TO  LNK-SC10S-SYORI
      *            CALL    "ORCSC10S"          USING
      *                                        ORCSC10SAREA
      *                                        SPA-AREA
      *                                        SPA-SCR-REC
      *                                        SPA-K02
      *            MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *            MOVE    SPACE               TO  SPA-ERRCD
      *            MOVE    SPACE               TO  SPA-KIDCD
      *
      *            診療行為、計計算処理、編集
      *            MOVE    "1"             TO  FLG-TOROKU
      *            PERFORM 510-TBLHEN-SEC
      ************ MOVE    SPACE           TO  FLG-TOROKU
      *
               WHEN    "0102"
                   MOVE    SPACE               TO  SPA-KIDCD
      *            外来加算料削除
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    SPA-CHK-IDX     TO  SPA-GMN-IDX
      *                診療料をクリアする
                       MOVE    SPACE           TO  SPA-GMN-INPUTCD
                                                        (SPA-GMN-IDX)
      *
      *                診療行為、計計算処理、編集
                       MOVE    "1"             TO  FLG-TOROKU
                       PERFORM 510-TBLHEN-SEC
                       MOVE    SPACE           TO  FLG-TOROKU
      *
                       PERFORM 500-GMNHEN-SEC
      *
      *                画面制御
                       MOVE    "JOIN"          TO  WRK-PUTTYPE
                       PERFORM 4100-TOROKU-SEC
      *
                   END-IF
      *
               WHEN    "0103"
      *************IF      SPA-KID1-FLG        =   "OK"
      *                画面制御
                       MOVE    "JOIN"          TO  WRK-PUTTYPE
                       MOVE    1               TO  SPA-CONTFLG
                       MOVE    SPACE               TO  SPA-KIDCD
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
                   IF      SPA-KID1-FLG        =   "OK"
                       PERFORM 41013-SRYKA-CHANGE-SEC
                   ELSE
                       PERFORM 41013-SRYKA-CHANGE-SEC
                   END-IF
      *
      *        同日再診３回以上の確認
               WHEN    "0106"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    SPACE           TO  SPA-KID1-FLG
                       MOVE    SPACE           TO  SPA-KIDCD
      *                パラメタファイルより表示
                       MOVE    3               TO  FLG-PARASET
                       PERFORM                 3002-PARA-SET-SEC
      *                画面用スパ編集処理
                       PERFORM                 310-SPA-GMNSET-SEC
                       MOVE    1               TO  SPA-GMN-PAGE
                       MOVE    1               TO  SPA-GMN-CUR
                       MOVE    1               TO  SPA-MAP-IDX
                   ELSE
                       MOVE    ZERO                TO  SPA-GMN-CUR
                   END-IF
      *
      *        初診料自動履歴算定の確認
               WHEN    "0108"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
      *                パラメタファイルより表示
                       MOVE    SPACE               TO  SPA-KIDCD
                       PERFORM                 300108-SAISET-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
      *
      *        同日再診料算定の確認
               WHEN    "0109"
                   MOVE    SPACE               TO  SPA-KIDCD
                   PERFORM 30019-DOUSAISIN-SEC
      *
      *        再診を初診料へ自動算定の確認
               WHEN    "0110"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    SPACE               TO  SPA-KIDCD
      *                初診料算定へ
                       PERFORM                 300109-SYOSINSET-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
      *
      *        再診を初診料へ自動算定の確認（警告表示）
               WHEN    "0112"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    SPACE               TO  SPA-KIDCD
      *                初診料算定へ
                       PERFORM                 300109-SYOSINSET-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
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
      *        再診の振替え
               WHEN    "0114"
               WHEN    "0115"
               WHEN    "0120"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       PERFORM 3001010-SAISIN-HEN-SEC
                       MOVE    1               TO  SPA-GMN-CUR
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
      *            再診の振替えチェック済み
                   MOVE    1               TO  SPA-SAISIN-CHK
                   MOVE    SPACE           TO  SPA-KIDCD
                   MOVE    SPACE           TO  SPA-KID1-FLG
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
      *                診察料の変更のみ
                       PERFORM 410131-SINSATUHEN-SEC
                   END-IF
      *
      *        科変更時、中途終了あり
               WHEN    "0122"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
      *                内容置換え
                       PERFORM 410132-SAIHENSYU-SEC
                   ELSE
      *                診察料の変更のみ
                       MOVE    2                  TO  SPA-TAHOKEN-FLG
                       PERFORM 410131-SINSATUHEN-SEC
                   END-IF
      *
      *        診療科変更確認
               WHEN    "0123"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       PERFORM 41013-SRYKA-CHANGE-SEC
                   ELSE
                       MOVE    1                   TO  SPA-GMN-CUR
                       MOVE    SPA-MAE-SRYKA-G     TO  SPA-GMN-SRYKA-G
                   END-IF
      *
      *        他保険算定あり
               WHEN    "0119"
                   MOVE    SPACE               TO  SPA-KIDCD
                   PERFORM 300110-TAHOKEN-SYORI-SEC
      *
      *        入院画面へ
               WHEN    "0121"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    SPA-GMN-PTNUM   TO  SPA-PTNUM
                       MOVE    SPA-GMN-PTID    TO  SPA-PTID
                       MOVE    "JOIN"              TO  WRK-PUTTYPE
                       PERFORM 450-K02N-SYORI-SEC
                   ELSE
      *                入院日を診療日にして登録する
                       MOVE    SPA-K02N-NYUINYMD   TO  SPA-SRYYMD
                       MOVE    SPA-K02N-NYUINYMD   TO  WRK-SYMD
                       PERFORM 520-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG         TO  SPA-SRYYMDW
      *                スパ共通編集処理
                       PERFORM                 310-SPA-SET-SEC
      *                チェック用月内診療行為セット
                       PERFORM                 320-SPA-CHKSET-SEC
      *
      *                画面用スパ編集処理
                       PERFORM                 310-SPA-GMNSET-SEC
      *
                       MOVE    1               TO  SPA-GMN-PAGE
                       MOVE    1               TO  SPA-GMN-CUR
                       MOVE    1               TO  SPA-MAP-IDX
                   END-IF
      *
      *TEST
      *        同一検査削除へ
               WHEN    "0160"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       PERFORM 30016-KENSA-DELETE-SEC
                   END-IF
      *TEST
           END-EVALUATE
           MOVE    SPACE               TO  SPA-KID1-FLG
      *****MOVE    SPACE               TO  SPA-KIDCD
           .
       30010-KID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＫＩＤ２）ＯＫ処理
      *****************************************************************
       30011-KID2-SET-SEC          SECTION.
      *
           EVALUATE    SPA-KIDCD2
      *        中途終了時の印刷確認
               WHEN    "0117"
                   IF      SPA-KID2-FLG        =   SPACE
                       MOVE    1               TO  SPA-GMN-CUR
                   ELSE
                       PERFORM 4510-SNRYO-NYU-SEC
                   END-IF
           END-EVALUATE
           MOVE    SPACE               TO  SPA-KID2-FLG
           MOVE    SPACE               TO  SPA-KIDCD2
           .
       30011-KID2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診料の履歴算定、再診料の再変更処理
      *****************************************************************
       300108-SAISET-SEC     SECTION.
      *
      *    初診算定なし
           MOVE    ZERO                TO  SPA-SYOSIN
      *    初診算定日
           MOVE    SPA-SYOSIN-YMD      TO  SPA-SYOYMD
           MOVE    SPA-SYOYMD          TO  WRK-SYMD
           PERFORM 520-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-SYOSINYMD
      *
      *    画面用スパ編集処理
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  LNK-WKSRYACT-MAX
           MOVE    SPACE               TO  SPA-KIDCD
           PERFORM                     310-SPA-GMNSET-SEC
           MOVE    SPACE               TO  SPA-KID1-FLG
      *
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-GMN-CUR
           MOVE    1                   TO  SPA-MAP-IDX
      *
           .
       300108-SAISET-EXT.
           EXIT.
      *****************************************************************
      *    同日再診料の設定処理
      *****************************************************************
       30019-DOUSAISIN-SEC     SECTION.
      *
           MOVE    SPACE               TO  SPA-KIDCD
           MOVE    SPACE               TO  WRK-SRYCD
      *
           IF      SPA-KID1-FLG        =   "OK"
      *
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
               INITIALIZE                      ORCSC10SAREA
               MOVE    "2"                 TO  LNK-SC10S-SYORI
               CALL    "ORCSC10S"          USING
                                           ORCSC10SAREA
                                           SPA-AREA
                                           SPA-SCR-REC
                                           SPA-K02
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
               MOVE    LNK-SC10S-IDX2      TO  IDX2
      *
           ELSE
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
      *
      *    診療行為、計計算処理、編集
           MOVE    "2"             TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE           TO  FLG-TOROKU
      *
           MOVE    SPACE               TO  SPA-KID1-FLG
           .
       30019-DOUSAISIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    再診を初診料へ自動算定より処理
      *****************************************************************
       300109-SYOSINSET-SEC                  SECTION.
      *
      *---- 治癒への変更はやめる  ------------------------------------
      *    患者病名病名を治癒へ
      *    MOVE    SPACE               TO  PTBYOMEI-REC
      *    INITIALIZE                  PTBYOMEI-REC
      *    MOVE    SPA-HOSPID          TO  PTBYO-HOSPID
      *    MOVE    SPA-PTID            TO  PTBYO-PTID
      *
      *    MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
      *    MOVE    "DBSELECT"          TO  MCP-FUNC
      *    MOVE    "PTBYOMEI-KEY3"     TO  ORC-DBPATH
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    "PTBYOMEI-KEY3"     TO  ORC-DBPATH
      *        PERFORM 980-PTBYOMEI-READ-SEC
      *    ELSE
      *        MOVE    1                   TO  FLG-PTBYOMEI
      *        MOVE    SPACE               TO  PTBYOMEI-REC
      *        INITIALIZE                  PTBYOMEI-REC
      *    END-IF
      *    PERFORM UNTIL       FLG-PTBYOMEI    =   1
      *        IF      PTBYO-SRYYMD        <   SPA-SRYYMD
      *            EVALUATE    PTBYO-TENKIKBN
      *                継続
      *                WHEN    " "
      *                    MOVE    SPA-SRYYMD      TO  PTBYO-TENKIYMD
      *                    MOVE    "1"             TO  PTBYO-TENKIKBN
      *
      *                    MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
      *                    MOVE    "DBUPDATE"          TO  MCP-FUNC
      *                    MOVE    "PTBYOMEI-KEY"      TO  ORC-DBPATH
      *                    CALL    "ORCMCPSUB"         USING
      *                                                MCPAREA
      *                                                ORCMCPAREA
      *                                                MCPDATA-REC
      *            END-EVALUATE
      *        END-IF
      *        MOVE    "PTBYOMEI-KEY3"     TO  ORC-DBPATH
      *        PERFORM 980-PTBYOMEI-READ-SEC
      *    END-PERFORM
      *-----------------------------------------------------------------
      *
      *    初診算定
           MOVE    1                   TO  SPA-SYOSIN
           MOVE    SPACE               TO  SPA-GMN-SYOSINYMD
      *
      *    画面用スパ編集処理
      *    INITIALIZE                  SPA-SCR-REC
      *    MOVE    ZERO                TO  LNK-WKSRYACT-MAX
           MOVE    SPACE               TO  SPA-KIDCD
      *****PERFORM                     310-SPA-GMNSET-SEC
      *
      *    診察料のみ変換する
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSC10SAREA
           MOVE    "S"                 TO  LNK-SC10S-SYORI
           CALL    "ORCSC10S"          USING
                                       ORCSC10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
      *    診療行為、計計算処理、編集
           MOVE    SPACE           TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE           TO  FLG-TOROKU
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           MOVE    SPACE               TO  SPA-KID1-FLG
      *
           .
       300109-SYOSINSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    再診料の振替え（４回目以降）処理
      *****************************************************************
       3001010-SAISIN-HEN-SEC                  SECTION.
      *
      *    再診料の４回目以降の振替え処理
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSC10SAREA
           MOVE    "8"                 TO  LNK-SC10S-SYORI
           CALL    "ORCSC10S"          USING
                                       ORCSC10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
      *    診療行為、計計算処理、編集
           MOVE    "2"             TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE           TO  FLG-TOROKU
      *
      *
           .
       3001010-SAISIN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    他保険算定あり処理
      *****************************************************************
       300110-TAHOKEN-SYORI-SEC        SECTION.
      *
           IF      SPA-KID1-FLG        =   "OK"
      *        他保険算定済み変更のみ
               MOVE    9                   TO  SPA-TAHOKEN-FLG
               PERFORM 410131-SINSATUHEN-SEC
           ELSE
      *        診察料置換え
               PERFORM 410131-SINSATUHEN-SEC
           END-IF
      *
           .
       300110-TAHOKEN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    同一検査削除処理
      *****************************************************************
       30016-KENSA-DELETE-SEC                  SECTION.
      *
           INITIALIZE                  WRK-SCR-REC
           MOVE    SPA-SCR-REC         TO  WRK-SCR-REC
      *
      *    同一検査をエラーとする
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   200     )   OR
                              (SPA-GMN-INPUTCD (IDX-Z) =   SPACE  AND
                               SPA-COM-INPUTCD (IDX-Z) =   SPACE )
               IF     (SPA-COM-SRYKBN (IDX-Z)  =   "60"  )  AND
                      (SPA-COM-INPUTCD(IDX-Z)(1:1)  =   "1" )  AND
                      (SPA-COM-DATAKBN(IDX-Z)  =   "1"   )
      *        逓減以外
               AND    (SPA-COM-TEIGENKBN (IDX-Z)   NOT =   1    )
      *        包括対象以外
               AND    (SPA-COM-SRYSYUKBN (IDX-Z)   NOT =   "610")
      *        包括分
               AND     (SPA-COM-HOUKNSKBN(IDX-Z)   NOT =   ZERO )
                   PERFORM VARYING     IDX-Z2  FROM    IDX-Z   BY  1
                           UNTIL       IDX-Z2      >   200
                       IF     (IDX-Z2      NOT =   IDX-Z )  AND
                              (SPA-COM-SRYSYUKBN   (IDX-Z2)
                                           NOT =   "610"  )  AND
                              (SPA-COM-INPUTCD(IDX-Z)
                                               =   SPA-COM-INPUTCD
                                                        (IDX-Z2))
                           MOVE    SPACE           TO  SPA-GMN-INPUTCD
                                                        (IDX-Z2)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *    診療行為、計計算処理、編集
           MOVE    SPACE           TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE           TO  FLG-TOROKU
      *
           MOVE    SPACE               TO  SPA-KID1-FLG
           .
       30016-KENSA-DELETE-EXT.
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
               MOVE    ZERO            TO  FLG-SYOSIN
               MOVE    1               TO  FLG-K09TUIKA
               PERFORM 3103-SAIHEN-HEN-SEC
      *
      *        診療行為、計計算処理、編集
      ******** MOVE    "2"             TO  FLG-TOROKU
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
                   CALL    "ORCSC94"           USING
                                               ORCSC94AREA
                                               SPA-SCR-REC
                                               SPA-AREA
               END-IF
           END-PERFORM
      *
           .
       30014-K023-SET-EXT.
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
                       IF      LNK-WKSRYACT-MAX    >   200
                           DISPLAY "K02 LNK-WKSRYACT-MAX 200 OVR"
                           MOVE    "9000"          TO  SPA-ERRCD
                       ELSE
                           MOVE    PARA-DATA-REC   TO  LNK-WKSRYACT-REC
                                                      (LNK-WKSRYACT-MAX)
                       END-IF
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
                       MOVE    PARA-DATA-REC   TO  SPA-K02
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
      *    未収金
           MOVE    SPA-MISYUMONEY      TO  SPA-GMN-MISYUMONEY
      *    最終来院日
           IF      SPA-LSTYMD      NOT =   SPACE
               MOVE    SPA-LSTYMD      TO  WRK-SYMD
               PERFORM 520-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-LASTYMD
           END-IF
      *
      *    初診算定日
           IF     (SPA-SYOYMD      NOT =   SPACE )  AND
                  (SPA-SYOSIN          =   ZERO  )
               MOVE    SPA-SYOYMD      TO  WRK-SYMD
               PERFORM 520-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-SYOSINYMD
           END-IF
      *
      *    年齢計算
           MOVE    SPA-BIRTHDAY        TO  WRK-YMDS1
           MOVE    SPA-SRYYMD          TO  WRK-YMDS2
           COMPUTE SPA-NENREI-YY       =   WRK-YYS2
                                       -   WRK-YYS1
           IF      WRK-YMDS1(5:4)      >   WRK-YMDS2(5:4)
               COMPUTE SPA-NENREI-YY   =   SPA-NENREI-YY
                                           -   1
               COMPUTE SPA-NENREI-MM   =  (WRK-MMS2  +  12 )
                                           -   WRK-MMS1
           ELSE
               COMPUTE SPA-NENREI-MM   =   WRK-MMS2
                                           -   WRK-MMS1
           END-IF
           IF      WRK-DDS1            >   WRK-DDS2
               COMPUTE SPA-NENREI-MM   =   SPA-NENREI-MM
                                           -   1
           END-IF
      *
      *    年齢期間算定
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY5-AREA
           MOVE    "51"                TO  LNK-DAY5-IRAI
           MOVE    SPA-BIRTHDAY        TO  LNK-DAY5-START
           MOVE    SPA-SRYYMD          TO  LNK-DAY5-END
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY5-AREA
           IF      LNK-DAY5-NISSU2     <=  31
               MOVE    LNK-DAY5-NISSU2     TO  SPA-NENREI-DD
               MOVE    ZERO                TO  SPA-NENREI-MM
           END-IF
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
      *
      **** MOVE    ZERO                TO  SPA-NAI-ROUSTR
      *****MOVE    ZERO                TO  SPA-NAI-ROUFLG
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
      ******** ７０歳到達月の最初の診療判定（老人保険がない時警告）
      *        MOVE    SPA-BIRTHDAY        TO  WRK-YMDS1
      ******** COMPUTE WRK-YYS1            =   WRK-YYS1   +  70
      *        COMPUTE WRK-YYS1            =   WRK-YYS1   +  WRK-ROU-NEN
      *******  IF     (WRK-NENREI-YY       >=  70 )  AND
      *        IF     (WRK-NENREI-YY       >=  WRK-ROU-NEN )  AND
      *               (SPA-LSTYMD          <=  WRK-YMDS1)
      *            MOVE    1               TO  SPA-NAI-ROUFLG
      *        END-IF
      ******** MOVE    ZERO                TO  SPA-NAI-ROUSTR
           END-IF
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
               MOVE   1                TO  IDX
           ELSE
               MOVE   IDY              TO  IDX
           END-IF
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
      *
      *    保険算定年齢チェック等
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPID          TO  AGECHK-HOSPID
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    SPA-SRYYMD          TO  AGECHK-SRYYMD
           MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
           MOVE    SPA-HKNCOMBINUM     TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
      **** IF      AGECHK-FTNRATE      =   ZERO
      *        負担率
               MOVE    AGECHK-FTNRATEMEI   TO  SPA-GMN-FTNRATEMEI
               MOVE    AGECHK-FTNRATEMEI   TO  SPA-FTNRATEMEI
               MOVE    AGECHK-FTNRATE      TO  SPA-GMN-FTNRATE
               MOVE    AGECHK-FTNRATE      TO  SPA-FTNRATE
      *****END-IF
           MOVE    ZERO                TO  SPA-RJN-ERR
           MOVE    ZERO                TO  SPA-HKN-ERR
      *
      *    Ｈ１５．４以降の保険期間が違う時
           IF      AGECHK-HKNCHK       =   1
               MOVE    "1028"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-HKN-ERR
           END-IF
      *    公費期間が違う時
           IF      AGECHK-HKNCHK       =   2
               MOVE    "1032"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-HKN-ERR
           END-IF
      *
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
      *    診察料編集なし
      *****MOVE    ZERO                TO  FLG-SINSATU
      *    入院中の時、入院開始日の時のみ初診料の自動発生を行う
           MOVE    SPACE               TO  SPA-KIDCD
           PERFORM 3109-NYUIN-CHK-SEC
           IF      SPA-KIDCD      NOT =   SPACE
               GO      TO      310-SPA-GMNSET-EXT
           END-IF
      *
      *    初診料自動発生処理
           IF    ((LNK-WKSRYACT-MAX        >   ZERO    )  AND
                  (LNK-WKSRY-TERMID (01)   NOT =   SPACE ))   OR
                 ( SPA-DOUJI-DENPNUM   NOT =   ZERO  )   OR
                 ( SPA-SYORIFLG            =   1     )
               MOVE    ZERO                TO  FLG-SYOSIN
               IF     (SPA-SYORIFLG            =   1     )  OR
                      (SPA-DOUJI-DENPNUM   NOT =   ZERO  )
      *            受診回数編集処理
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
               END-IF
      *******  IF     (LNK-WKSRY-TERMID (01)   =   "WKSRYACT")
      *        AND    (FLG-NYUIN       =   ZERO  )
      *            PERFORM 3104-WKSRYACT-HEN-SEC
      ******   END-IF
           ELSE
               MOVE    1               TO  FLG-SYOSIN
               IF     (FLG-NYUIN       =   ZERO  )  OR
                      (SPA-K02N-NYUINYMD   NOT =   SPACE )
                   PERFORM 330-SYOSIN-SET-SEC
               END-IF
               IF     (SPA-ERRCD       NOT =   SPACE ) OR
                      (SPA-KIDCD       NOT =   SPACE )
                   GO      TO      310-SPA-GMNSET-EXT
               END-IF
           END-IF
      *
      *****MOVE    "2"                 TO  FLG-TOROKU
      *
      *    再表示編集処理
           IF      LNK-WKSRYACT-MAX    >   ZERO
               MOVE    ZERO            TO  FLG-K09TUIKA
               PERFORM 3103-SAIHEN-HEN-SEC
      *
               IF      SPA-SYORIFLG            =   1
      *            初診料算定の判定
                   MOVE    SPA-K01KYOUTU       TO  SPA-WORK
                   INITIALIZE                      ORCSC10SAREA
                   MOVE    "C"                 TO  LNK-SC10S-SYORI
                   CALL    "ORCSC10S"          USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
                   MOVE    SPA-WORK            TO  SPA-K01KYOUTU
               END-IF
      *
               IF     (LNK-WKSRY-TERMID (01)   =   "WKSRYACT")
               AND    (FLG-NYUIN       =   ZERO  )
                   PERFORM 3104-WKSRYACT-HEN-SEC
               END-IF
           END-IF
      *
      *    入力コード・名称チェック処理
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX        >   200 )  OR
                          ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
               IF      SPA-COM-CUR (IDX)   =   SPACE
                   MOVE    SPA-GMN-INPUTCD(IDX)
                                           TO  SPA-MAE-INPUTCD(IDX)
               END-IF
           END-PERFORM
      *    診療行為、計計算処理、編集
           MOVE    SPACE           TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE           TO  FLG-TOROKU
      *    警告フラグクリア
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX        >   200 )  OR
                          ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
               MOVE    SPACE           TO  SPA-COM-KZMFLG (IDX)
           END-PERFORM
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPACE               TO  SPA-KIDCD
      *
           .
       310-SPA-GMNSET-EXT.
           EXIT.
      *
      *****************************************************************
      *   中途終了よりの画面用再編集処理
      *****************************************************************
       3104-WKSRYACT-HEN-SEC           SECTION.
      *
      *    MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *    INITIALIZE                      ORCSC10SAREA
      *    MOVE    "W"                 TO  LNK-SC10S-SYORI
      *    CALL    "ORCSC10S"          USING
      *                                ORCSC10SAREA
      *                                SPA-AREA
      *                                SPA-SCR-REC
      *                                SPA-K02
      *    MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *    MOVE    LNK-SC10S-IDX2      TO  IDX2
      *    MOVE    ZERO                TO  FLG-SYOSIN
      *    診察料編集なし(1:診察料算定済み）
      *****MOVE    LNK-SC10S-SINSATU   TO  FLG-SINSATU
      *
      *    剤分離処理を行う
           INITIALIZE                  ORCSC93AREA
           MOVE    "K02"               TO  LNK-SC93-PG
      *    小児科外来診療科算定有り
           MOVE    SPA-SYONIKA-ARI2    TO  LNK-SC93-SYONIKA-ARI2
      *    寝たきり在宅科算定有り
           IF      (SPA-NETAKIRI-ARI   NOT =   ZERO )  OR
                   (SPA-NETAKIRI-ARI2  NOT =   ZERO )
               MOVE    1                   TO  LNK-SC93-NETAKIRI-ARI
           END-IF
           CALL    "ORCSC93"           USING
                                       MCPAREA
                                       SPA-SCR-REC
                                       SPA-AREA
                                       ORCSC93AREA
                                       MSYU-DATA-REC
      *
      *    診察料のみ変換する
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSC10SAREA
           MOVE    "W"                 TO  LNK-SC10S-SYORI
           CALL    "ORCSC10S"          USING
                                       ORCSC10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           .
       3104-WKSRYACT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   再表示あり　編集処理
      *****************************************************************
       3103-SAIHEN-HEN-SEC                  SECTION.
      *
           MOVE    ZERO               TO  FLG-SET
      *
      *    ワーク診療行為からの時、剤毎に剤点数を編集し直す
           IF      LNK-WKSRY-TERMID (1)    =   "WKSRYACT"
               PERFORM 31031-WKSRY-ZAITENHEN-SEC
           END-IF
      *
      *    診療室入力あり編集
           PERFORM     VARYING   IDX   FROM    1   BY  1
                       UNTIL    (IDX   >   LNK-WKSRYACT-MAX )  OR
                                (IDX2  >   200 )
      *
      *        ワーク診療行為からの時は、訂正と同様に編集する
               IF     (LNK-WKSRY-TERMID (IDX)    =   "WKSRYACT" )
      **********  AND (FLG-SINSATU         =   ZERO             )
                   MOVE    1               TO  WRK-SYORIFLG
               ELSE
                   MOVE    SPA-SYORIFLG    TO  WRK-SYORIFLG
               END-IF
      *
               MOVE    1               TO  IDX1
      *
      *        前回再編集の時、小児科外来診療科算定の時、薬剤のみとする
               IF      (FLG-SYOSIN             =   1     )  AND
                       (SPA-SYONIKA-ARI    NOT =   ZERO  )
                   IF      (LNK-WKSRY-SRYKBN (IDX) =   "21" AND
                                                       "22" AND
                                                       "23")  AND
                           (LNK-WKSRY-ZAITENKEI(IDX)   =   ZERO )
                       CONTINUE
                   ELSE
                       MOVE    6               TO  IDX1
                   END-IF
               END-IF
      *
      *        前回再編集の時、保険外の項目は対象外とする
               IF      (FLG-SYOSIN             =   1     )  AND
                       (LNK-WKSRY-SRYKBN (IDX) =   "95" OR  "96" )
                   MOVE    6               TO  IDX1
               END-IF
      *
               IF       FLG-SET                =   1
                   MOVE    6               TO  IDX1
               END-IF
      *
               MOVE    ZERO                TO  IDXS
               PERFORM     VARYING   IDY   FROM    IDX1   BY  1
                           UNTIL     IDY   >   5
                   MOVE    ZERO                TO  FLG-AUTO-OK
                   MOVE    ZERO                TO  FLG-HEN-CHK
      *
      *            自動作成分は編集しない
                   IF      LNK-WKSRY-SRYCD  (IDX IDY)    =   SPACE
                       MOVE    1               TO  FLG-HEN-CHK
                   END-IF
      *            前回編集時、自動作成分は編集しない
                   IF     (FLG-SYOSIN                    =   1 )  AND
                          (LNK-WKSRY-AUTOKBN(IDX IDY)    =   "2" OR
                                                             "3" )
                       MOVE    1               TO  FLG-HEN-CHK
                   END-IF
      *            登録時自動発生分は編集しない
                   IF      LNK-WKSRY-AUTOKBN(IDX IDY)    =   "3"
                       MOVE    1               TO  FLG-HEN-CHK
                   END-IF
                   IF     (WRK-SYORIFLG                  =   1  ) AND
                          (LNK-WKSRY-AUTOKBN(IDX IDY)    =  "3" )
      *                訂正自動作成で算定回数を超えている時対象とする
                       PERFORM 31012-AUTOCHK-SEC
                       IF      FLG-AUTO-OK         =   1
                           MOVE    ZERO            TO  FLG-HEN-CHK
                       END-IF
                   END-IF
                   IF      FLG-HEN-CHK         =   ZERO
                       PERFORM 31011-SPA-HEN-SEC
                   END-IF
      *
      *            約束セットの時、以下を編集しない
                   IF      LNK-WKSRY-SRYCD(IDX IDY)(1:1) =   "S"
                       MOVE    5               TO  IDY
                       MOVE    1               TO  FLG-SET
                   END-IF
               END-PERFORM
      *
      *        剤の最後に回数入力をセット
               IF     (IDXS                >   ZERO  )  OR
                      (FLG-SET             =   1     ) 
                   IF     (IDX                 =   LNK-WKSRYACT-MAX ) OR
                          ((IDX                 <   200  )  AND
                           (LNK-WKSRY-ZAINUM (IDX + 1)
                                           NOT =   LNK-WKSRY-ZAINUM
                                                                (IDX)))
                       MOVE    "1"             TO  SPA-COM-KAIFLG
                                                               (IDX2)
                       MOVE    "1"             TO  SPA-COM-ZAIEND
                                                               (IDX2)
                       MOVE    ZERO                TO  FLG-SET
                   END-IF
               END-IF
           END-PERFORM
      *
      *    約束コード以外の画面再編集
           PERFORM VARYING     IDX-1 FROM    1   BY  1
                   UNTIL      (IDX-1     >   200     )  OR
                              (IDX-1     >   IDX2   )
               IF     (SPA-GMN-INPUTCD(IDX-1)(1:1)   =   "S" OR ".") OR
                      (SPA-COM-INPUTCD(IDX-1)(1:1)   =   "S"       )
                   CONTINUE
               ELSE
      *
      *            点数マスタ編集
                   INITIALIZE                  ORCSC92AREA
                   MOVE    SPACE               TO  LNK-SC92-KBN
                   MOVE    WRK-SYORIFLG        TO  LNK-SC92-SYORIFLG
                   MOVE    "K02"               TO  LNK-SC92-PG
                   MOVE    IDX-1               TO  LNK-SC92-GMN-IDX
                   MOVE    SPA-COM-INPUTCD(IDX-1)
                                               TO  LNK-SC92-SRYCD
                   MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
                   MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
                   MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
                   MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
                   MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
                   MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
      *            当月入院有無
                   MOVE    SPA-K02N-NYUINFLG   TO  LNK-SC92-NYUINFLG
                   CALL    "ORCSC92"           USING
                                               MCPAREA
                                               ORCSC92AREA
                                               SPA-SCR-REC
                                               SPA-AREA
                   IF     (SPA-ERRCD           NOT =   SPACE )  OR
                          (LNK-SC92-ERRAREA    NOT =   SPACE )
                       MOVE    SPACE           TO  SPA-ERRCD
                       MOVE    "1"             TO  SPA-COM-CUR (IDX-1)
                   END-IF
      *
      *            回数の入力
                   IF      SPA-COM-KAIFLG (IDX-1)  =   "1"
                       IF     (SPA-COM-KAISU(IDX-1)    >   1  )  OR
                              (SPA-COM-SRYKBN(IDX-1)(1:1)
                                                   =   "2"    )  OR
                              (IDX-1               =   200   )
                           CONTINUE
                       ELSE
                           IF     (SPA-COM-INPUTCD(IDX-1 + 1)
                                                   =   SPACE   )  OR
                                  (SPA-GMN-INPUTCD(IDX-1 + 1)(1:1)
                                                   =   "."     )
                               MOVE    SPACE       TO  SPA-COM-KAIFLG
                                                         (IDX-1)
                           ELSE
                               IF    ((SPA-COM-SRYKBN (IDX-1 + 1)
                                               NOT =  SPA-COM-SRYKBN
                                                       (IDX-1) )  AND
                                      (SPA-COM-INPUTCD(IDX-1 + 1)(1:1)
                                                   =   "6"     )) 
                                   CONTINUE
                               ELSE
                                   MOVE    SPACE   TO  SPA-COM-KAIFLG
                                                         (IDX-1)
                               END-IF
                           END-IF
                       END-IF
                   END-IF
      *
      *            入力コードの画面用再編集
                   INITIALIZE                  WRK-MOJI-AREA
                   MOVE    IDX-1               TO  IDX
                   PERFORM 3101-INPUTCD-HEN-SEC
                   IF      SPA-COM-CUR (IDX)   =   SPACE
                       MOVE    SPA-GMN-INPUTCD(IDX)
                                          TO  SPA-MAE-INPUTCD (IDX)
                   ELSE
                       MOVE    SPACE      TO  SPA-MAE-INPUTCD (IDX)
                   END-IF
               END-IF
           END-PERFORM
      *
      *    画像診断のフィルム判定
           MOVE    ZERO              TO  FLG-FIRUMU
           PERFORM VARYING     IDX-1 FROM    IDX2   BY  -1
                   UNTIL       IDX-1     <   1
               IF      SPA-COM-ZAIEND (IDX-1)  =   "1"
                   MOVE    ZERO            TO  FLG-FIRUMU
               END-IF
               IF      SPA-COM-SRYKBN (IDX-1)  =   "70"
                   IF     (SPA-COM-INPUTCD(IDX-1)(1:1)   =   "7" )  AND
                          (SPA-COM-DATAKBN (IDX-1)       =   "3" )
                       MOVE    1               TO  FLG-FIRUMU
                   END-IF
                   IF     (SPA-COM-INPUTCD(IDX-1)(1:1)   =   "1" )  AND
                          (SPA-COM-DATAKBN (IDX-1)       =   "1" )  AND
                          (SPA-COM-KZMCOMPSIKIBETU(IDX-1)  NOT =  ZERO)
                     AND  (SPA-COM-SURYO (IDX-1)         >   1   )
                       IF      FLG-FIRUMU                =   ZERO
                           MOVE    "1"             TO  SPA-COM-SURFLG
                                                               (IDX-1)
                       ELSE
                           IF      SPA-COM-SURFLG (IDX-1)
                                                       NOT =   "1"
                               MOVE    1           TO  SPA-COM-SURYO
                                                               (IDX-1)
      *                        入力コードの画面用再編集
                               MOVE    IDX-1               TO  IDX
                               PERFORM 3101-INPUTCD-HEN-SEC
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
      *    約束コードの画面用再編集
           MOVE    SPACE               TO  WRK-ERRCD
           PERFORM VARYING     IDX-1   FROM    1   BY  1
                   UNTIL      (IDX-1   >   200 )  OR
                              (SPA-COM-INPUTCD(IDX-1)    =   SPACE  AND
                               SPA-GMN-INPUTCD(IDX-1)    =   SPACE )
               IF      SPA-COM-INPUTCD(IDX-1)(1:1)   =   "S"
                   INITIALIZE                  WRK-MOJI-AREA
                   MOVE    IDX-1               TO  IDX
                   MOVE    SPA-COM-INPUTCD (IDX-1)
                                               TO  SPA-GMN-INPUTCD
                                                              (IDX-1)
      *            入力コードの画面用再編集
                   PERFORM 3101-INPUTCD-HEN-SEC
                   MOVE    SPACE               TO  SPA-MAE-INPUTCD (IDX)
                   MOVE    IDX-1               TO  SPA-GMN-IDX
      *
                   MOVE    SPACE               TO  SPA-COM-INPUTCD
                                                             (IDX-1)
                   MOVE    SPACE               TO  SPA-COM-KAIFLG
                                                               (IDX-1)
      *
                   MOVE    ZERO                TO  WRK-IDX-YAKUSOKU
                   MOVE    ZERO                TO  IDX-YAKUSOKU
                   INITIALIZE                      WRK-MOJI-AREA
      *
                   INITIALIZE                  ORCSC97AREA
                   MOVE    SPA-GMN-IDX         TO  LNK-SC97-GMN-IDX
                   MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                               TO  LNK-SC97-INPUTCD
                   CALL    "ORCSC97"           USING
                                               SPA-AREA
                                               ORCSC97AREA
                                               SPA-SCR-REC
      *
                   MOVE    LNK-SC97-INCD       TO  WRK-CD
                   MOVE    LNK-SC97-INCDCNT    TO  WRK-CD-MCNT
      *            変換文字列編集
                   PERFORM 4101121-MOJI-SET-SEC
                   PERFORM 4101191-INPUTSET-SYORI-SEC
               END-IF
           END-PERFORM
      *
           IF      WRK-ERRCD1      NOT =   SPACE
               MOVE    WRK-ERRCD1          TO  SPA-ERRCD
               IF      FLG-K09TUIKA        =   1
                   MOVE    "7004"          TO  SPA-ERRCD
               END-IF
               GO      TO      3103-SAIHEN-HEN-EXT
           END-IF
           MOVE    WRK-ERRCD           TO  SPA-ERRCD
      *
           .
       3103-SAIHEN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤毎に剤点数を編集処理
      *****************************************************************
       31031-WKSRY-ZAITENHEN-SEC              SECTION.
      *
           MOVE    ZERO                TO  WRK-TENSUKEI
           PERFORM VARYING   IDX       FROM    LNK-WKSRYACT-MAX  BY  -1
                   UNTIL     IDX       <       1
               IF    ( IDX             =   LNK-WKSRYACT-MAX  )  OR
                     ((IDX             <   200  )    AND
                      (LNK-WKSRY-ZAINUM (IDX + 1)
                                       NOT =   LNK-WKSRY-ZAINUM(IDX)))
                   MOVE    LNK-WKSRY-ZAITENKEI(IDX)
                                               TO  WRK-TENSUKEI
               END-IF
               MOVE    WRK-TENSUKEI        TO  LNK-WKSRY-ZAITENKEI(IDX)
           END-PERFORM
           .
       31031-WKSRY-ZAITENHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   再表示あり　編集処理
      *****************************************************************
       31011-SPA-HEN-SEC              SECTION.
      *
      *    既に診察料を算定済みの時、対象の診察料をはずす
      *    IF      FLG-SINSATU         =   1
      *        IF      LNK-WKSRY-SRYKBN (IDX)  =   "11"  OR "12" OR
      *                                            "13"  OR  "14"
      *            IF      LNK-WKSRY-ZAINUM (IDX)
      *                                        =   LNK-SC10S-ZAINUM (1)
      *                                        OR  LNK-SC10S-ZAINUM (2)
      *                                        OR  LNK-SC10S-ZAINUM (3)
      *                                        OR  LNK-SC10S-ZAINUM (4)
      *                                        OR  LNK-SC10S-ZAINUM (5)
      *                GO      TO      31011-SPA-HEN-EXT
      **           END-IF
      *        END-IF
      *****END-IF
      *
      *    診療種別セット
           IF     (IDXS                    =   ZERO)  AND
                  (LNK-WKSRY-RENNUM (IDX)  =   1   )
               MOVE    ZERO                TO  FLG-SYUBETU
      *        診察料以外
               IF    ((LNK-WKSRY-SRYSYUKBN (IDX)   NOT =   SPACE )  AND
                      (LNK-WKSRY-SRYSYUKBN (IDX)(1:2)
                                                   NOT =   "11" AND
                                                           "12" AND
                                                           "13"  ))
                   MOVE    1               TO  FLG-SYUBETU
               END-IF
      *        診察料で前の剤が自費の時
               IF     (LNK-WKSRY-SRYSYUKBN(IDX)(1:2)
                                           =   "11" OR
                                               "12" OR
                                               "13"  )   AND
                     ((IDX2                >   ZERO  ) AND
                      (SPA-COM-SRYSYUKBN  (IDX2)(1:2)
                                           =   "96"  OR  "95"))
                   MOVE    1               TO  FLG-SYUBETU
               END-IF
      *        訂正の時、再診料算定科を再診とする
               IF     (LNK-WKSRY-SRYKBN    (IDX)       =   "12" AND
                       LNK-WKSRY-SRYCD (IDX IDY)       =   "830000021")
                   OR (LNK-WKSRY-SRYKBN    (IDX)       =   "11" AND
                       LNK-WKSRY-SRYCD (IDX IDY)       =   "830000020")
      *                訂正の時、時間外のみの時
                  OR ((LNK-WKSRY-SRYKBN    (IDX)       =   "13"  ) AND
                      (LNK-WKSRY-SRYCD (IDX IDY)       =   "112005770"
                                                       OR  "112005870"
                                                       OR  "112005970"
                                                       OR  "112006070"))
      *                訂正の時、ダミー診察の時
                  OR ((LNK-WKSRY-SRYKBN    (IDX)       =   "11" OR
                                                           "12"   ) AND
                      (LNK-WKSRY-SRYCD (IDX IDY)       =   "820000003"
                                                       OR  "820000004"
                                                       OR  "820000005"
                                                       OR  "820000006"))
                   MOVE    1                   TO  FLG-SYUBETU
               END-IF
      *
               IF      FLG-SYUBETU         =   1
                   ADD     1                   TO  IDX2
                   IF      IDX2                >   200
                       MOVE    "9100"          TO  WRK-ERRCD1
                       GO      TO      31011-SPA-HEN-EXT
                   END-IF
                   MOVE    "."                 TO  SPA-GMN-INPUTCD
                                                            (IDX2)(1:1)
                   MOVE    LNK-WKSRY-SRYSYUKBN (IDX)
                                               TO  SPA-GMN-INPUTCD
                                                           (IDX2)(2:3)
                   MOVE    LNK-WKSRY-SRYSYUKBN (IDX)
                                               TO  SPA-COM-SRYSYUKBN
                                                              (IDX2)
                   MOVE    LNK-WKSRY-SRYKBN (IDX)
                                               TO  SPA-COM-SRYKBN(IDX2)
               END-IF
      *     訂正の時、診療種別が未入力で院外の薬剤があった時
               IF     (LNK-WKSRY-SRYSYUKBN (IDX)   =   SPACE OR
                                                       "210" OR
                                                       "220" OR
                                                       "230" OR
                                                       "290" )  AND
                      (LNK-WKSRY-SRYKBN    (IDX)   =   "21" OR "22"
                                                   OR  "23" )
                 IF     (WRK-SYORIFLG            =   1    )  OR
                        (LNK-WKSRY-TERMID (IDX)  =   "WKSRYACT")
                   IF      LNK-WKSRY-ZAITENKEI (IDX)   =   ZERO
      *                剤点数＝ゼロ　院外
                       MOVE    "1"             TO  SPA-INGAIKBN
                   ELSE
      *                剤点数≠ゼロ　院内
                       MOVE    "0"             TO  SPA-INGAIKBN
                   END-IF
                 END-IF
               END-IF
           END-IF
      *
      *    明細編集
           ADD     1                   TO  IDX2
           IF      IDX2                >   200
                MOVE    "9100"         TO  WRK-ERRCD1
                GO      TO      31011-SPA-HEN-EXT
           END-IF
           MOVE    LNK-WKSRY-SRYKBN(IDX)
                                       TO  SPA-COM-SRYKBN  (IDX2)
           MOVE    LNK-WKSRY-SRYSYUKBN(IDX)
                                       TO  SPA-COM-SRYSYUKBN  (IDX2)
           MOVE    LNK-WKSRY-SRYCD (IDX IDY)
                                       TO  SPA-COM-INPUTCD (IDX2)
           MOVE    LNK-WKSRY-SRYSURYO(IDX IDY)
                                       TO  SPA-COM-SURYO   (IDX2)
           MOVE    LNK-WKSRY-SRYSURYO(IDX IDY)
                                       TO  SPA-COM-SURYO2  (IDX2)
           MOVE    LNK-WKSRY-AUTOKBN (IDX IDY)
                                       TO  SPA-COM-AUTOKBN  (IDX2)
           IF      LNK-WKSRY-AUTOKBN (IDX IDY)
                                       =   "A" OR "B" OR "C"  OR  "D"
               MOVE    SPACE               TO  SPA-COM-AUTOKBN (IDX2)
           END-IF
      *    数量入力時
           IF      LNK-WKSRY-AUTOKBN (IDX IDY)
                                       =   "S"
               MOVE    "1"                 TO  SPA-COM-SURFLG  (IDX2)
               MOVE    SPACE               TO  SPA-COM-AUTOKBN (IDX2)
           END-IF
           IF      FLG-AUTO-OK         =   1
               MOVE    SPACE               TO  SPA-COM-AUTOKBN (IDX2)
           END-IF
      *
           MOVE    LNK-WKSRY-INPUTCHI-G (IDX IDY)
                                       TO  SPA-COM-INPUTCHI-G  (IDX2)
      *
      *    回数を編集
           MOVE    LNK-WKSRY-ZAIKAIKEI(IDX) TO  SPA-COM-KAISU(IDX2)
           MOVE    LNK-WKSRY-ZAIKAIKEI(IDX) TO  SPA-COM-KAISU2(IDX2)
           MOVE    IDX2                TO  IDXS
      *    分画数を編集
           MOVE    LNK-WKSRY-SRYKAISU (IDX IDY)
                                       TO  SPA-COM-BUNGSU (IDX2)
      *
           IF      LNK-WKSRY-INPUTCD (IDX IDY) =   SPACE
               MOVE    SPA-COM-INPUTCD (IDX2)
                                           TO  SPA-GMN-INPUTCD (IDX2)
           ELSE
               MOVE    LNK-WKSRY-INPUTCD (IDX IDY)
                                           TO  SPA-GMN-INPUTCD (IDX2)
           END-IF
           MOVE    SPA-GMN-INPUTCD (IDX2)  TO  SPA-MAE-INPUTCD (IDX2)
      *
      *    約束セットの時、以下処理なし
           IF      LNK-WKSRY-SRYCD(IDX IDY)(1:1) =   "S"
               GO      TO      31011-SPA-HEN-EXT
           END-IF
      *
      *    時間加算
           IF      (SPA-GMN-INPUTCD (IDX2)(1:1)  IS  NUMERIC) AND
                   (SPA-GMN-INPUTCD (IDX2)(2:1)  =   SPACE  ) AND
                   (SPA-GMN-INPUTCD (IDX2)(3:1)  NOT =   SPACE)
               MOVE    SPA-GMN-INPUTCD (IDX2)(1:1)
                                           TO  SPA-COM-TIMEFLG(IDX2)
               MOVE    SPA-COM-TIMEFLG (IDX2)
                                           TO  SPA-NAI-TIMEFLG
           END-IF
      *
      *    コメントの時、コメント内容編集
           IF      SPA-COM-INPUTCD (IDX2)(1:1) =   "0"  OR  "8"
               IF      LNK-WKSRY-INPUTCOMENT (IDX IDY)  NOT =   SPACE
                   IF      SPA-COM-INPUTCD (IDX2)       =   "810000001"
                       MOVE    LNK-WKSRY-INPUTCOMENT (IDX IDY)
                                           TO  SPA-GMN-MEISYO-G (IDX2)
                       MOVE    "1"         TO  SPA-COM-MEIFLG   (IDX2)
                   ELSE
                       MOVE    LNK-WKSRY-INPUTCOMENT (IDX IDY)
                                           TO  SPA-GMN-MEISYO   (IDX2)
                       IF      SPA-COM-INPUTCD (IDX2)(1:2)  =   "83" 
                           MOVE    "1"     TO  SPA-COM-MEIFLG   (IDX2)
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *
      *    金額入力時、金額へ
           IF      SPA-COM-INPUTCD(IDX2)(1:3)  =   "095"  OR  "096"
      *        自賠責の時、対象外
               IF     (SPA-HKNKBN          =   1    )  AND
                      (SPA-RSI-HKNKBN      =   "4"  )  AND
                      (SPA-COM-SRYKBN (IDX2)
                                           =   "80" )  AND
                      (SPA-COM-INPUTCD(IDX2)(1:5)
                                           =   "09591"  OR
                                               "09592"  OR
                                               "09593"  )
                   MOVE    ZERO                TO  FLG-OK2
               ELSE
                   MOVE    1                   TO  FLG-OK2
               END-IF
           ELSE
               MOVE    ZERO                TO  FLG-OK2
           END-IF
           IF      FLG-OK2             =   1
               IF      SPA-COM-KISOTEN (IDX2)      =   ZERO
                   MOVE    "1"                 TO  SPA-COM-KEIFLG(IDX2)
                   MOVE    LNK-WKSRY-JIHIMONEY(IDX IDY)
                                               TO  SPA-COM-KEI   (IDX2)
                   MOVE    LNK-WKSRY-JIHIMONEY(IDX IDY)
                                               TO  SPA-GMN-KEI   (IDX2)
                   MOVE    LNK-WKSRY-JIHIMONEY(IDX IDY)
                                               TO  SPA-COM-JIHIMONEY
                                                                 (IDX2)
               ELSE
                   MOVE    "2"                 TO  SPA-COM-KEIFLG(IDX2)
                   MOVE    SPA-COM-KISOTEN (IDX2)
                                               TO  SPA-COM-KEI   (IDX2)
                   MOVE    SPA-COM-KISOTEN (IDX2)
                                               TO  SPA-GMN-KEI   (IDX2)
                   MOVE    SPA-COM-KISOTEN (IDX2)
                                               TO  SPA-COM-JIHIMONEY
                                                                 (IDX2)
               END-IF
      *        回数を１とする
      *        MOVE    1                   TO  SPA-COM-KAISU(IDX2)
      *        MOVE    1                   TO  SPA-COM-KAISU2(IDX2)
      *
      *        自費の時、金額編集
           ELSE
               IF      SPA-COM-SRYKBN (IDX2)   =   "95"  OR  "96"
                   MOVE    "2"                 TO  SPA-COM-KEIFLG(IDX2)
                   MOVE    LNK-WKSRY-JIHIMONEY(IDX IDY)
                                               TO  SPA-COM-KEI   (IDX2)
                   MOVE    LNK-WKSRY-JIHIMONEY(IDX IDY)
                                               TO  SPA-GMN-KEI   (IDX2)
                   MOVE    LNK-WKSRY-JIHIMONEY(IDX IDY)
                                               TO  SPA-COM-JIHIMONEY
                                                                 (IDX2)
      ***          回数を１とする
      *            MOVE    1                   TO  SPA-COM-KAISU(IDX2)
      ****         MOVE    1                   TO  SPA-COM-KAISU2(IDX2)
               ELSE
                   MOVE    SPACE               TO  SPA-COM-KEIFLG(IDX2)
               END-IF
           END-IF
      *
           .
       31011-SPA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   自動算定分編集処理
      *****************************************************************
       31012-AUTOCHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-AUTO-OK
      *
      *    算定履歴チェック処理
           INITIALIZE                  ORCSC91AREA
           MOVE    WRK-SYORIFLG        TO  LNK-SC91-SYORIFLG
           MOVE    LNK-WKSRY-SRYCD  (IDX IDY)
                                       TO  LNK-SC91-SRYCD
           MOVE    "4"                 TO  LNK-SC91-KBN
           MOVE    ZERO                TO  LNK-SC91-GMN-IDX
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC91-TEISEI-AREA
      *
           CALL    "ORCSC91"           USING    MCPAREA
                                                ORCSC91AREA
                                                SPA-SCR-REC
                                                SPA-AREA
                                                TNS-TENSU-REC
           IF      LNK-SC91-ERRCD      =   "K004"
               MOVE    1               TO  FLG-AUTO-OK
               MOVE    SPACE           TO  LNK-SC91-ERRCD
           END-IF
      *
           .
       31012-AUTOCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   入力コードの画面用再編集処理
      *****************************************************************
       3101-INPUTCD-HEN-SEC                  SECTION.
      *
      *    画面再編集・基本チェック
           INITIALIZE                  ORCSC94AREA
           MOVE    "1"                 TO  LNK-SC94-KBN
           MOVE    "K02"               TO  LNK-SC94-PG
           MOVE    IDX                 TO  LNK-SC94-IDX
           CALL    "ORCSC94"           USING
                                       ORCSC94AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *    前へ編集
           MOVE    SPA-GMN-INPUTCD (IDX)   TO  SPA-MAE-INPUTCD (IDX)
      *
           .
       3101-INPUTCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院中判定処理
      *****************************************************************
       3109-NYUIN-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUIN
      *    入院日
           MOVE    SPACE               TO  SPA-K02N-NYUINYMD
           MOVE    SPACE               TO  SPA-K02N-TAIINYMD
           MOVE    SPACE               TO  WRK-TAIINYMD
      *
      *    入院履歴判定
           MOVE    SPACE               TO  PTNYUINRRK-REC
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPID          TO  PTNYUINRRK-HOSPID
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTNYUINRRK-KEY8"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PTNYUINRRK-KEY8"   TO  ORC-DBPATH
               PERFORM 910-PTNYUINRRK-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *    入院が１件もないとき、外来とする
           IF      FLG-PTNYUINRRK      =   1
               MOVE    ZERO                TO  FLG-NYUIN
               GO      TO     3109-NYUIN-CHK-EXT
           END-IF
      *
           PERFORM
                   UNTIL      (FLG-PTNYUINRRK      =   1 )  OR
                              (FLG-NYUIN           =   1 )
      *        入院中
               IF     (PTNYUINRRK-TENNYUYMD    <=  SPA-NAI-SRYYMD) AND
                     ((PTNYUINRRK-TENSTUYMD    >=  SPA-NAI-SRYYMD) OR
                      (PTNYUINRRK-TENSTUYMD    =   SPACE         ))
                   IF     (PTNYUINRRK-TENNYUYMD    =   SPA-NAI-SRYYMD)
                       OR (PTNYUINRRK-TAIINYMD     =   SPA-NAI-SRYYMD)
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-NYUIN
      *                入院日
                       MOVE    PTNYUINRRK-NYUINYMD
                                               TO  SPA-K02N-NYUINYMD
                       MOVE    PTNYUINRRK-TAIINYMD
                                               TO  SPA-K02N-TAIINYMD
                   END-IF
      *            入院日
                   MOVE    PTNYUINRRK-NYUINYMD
                                               TO  SPA-K02N-NYUINYMD
                   MOVE    PTNYUINRRK-TAIINYMD
                                               TO  SPA-K02N-TAIINYMD
               END-IF
      *
      *        最終退院日
               IF      PTNYUINRRK-TAIINYMD     <=  SPA-NAI-SRYYMD
                   MOVE    PTNYUINRRK-TAIINYMD     TO  WRK-TAIINYMD
               END-IF
      *
      *        入院中でなく当月の入院有無
               IF     (PTNYUINRRK-NYUINYMD(1:6)    =   SPA-NAI-SRYYMD
                                                              (1:6)) OR
                      (PTNYUINRRK-TAIINYMD(1:6)    =   SPA-NAI-SRYYMD
                                                              (1:6))
                   MOVE    1                   TO  SPA-K02N-NYUINFLG
               END-IF
      *
               MOVE    "PTNYUINRRK-KEY8"   TO  ORC-DBPATH
               PERFORM 910-PTNYUINRRK-READ-SEC
           END-PERFORM
      *    入院中の時、エラーとする
           IF      FLG-NYUIN           =   "1"
               MOVE    "0121"          TO  SPA-KIDCD
               MOVE    ZERO            TO  SPA-GMN-CUR
           ELSE
      *        最終退院日の設定
               MOVE    WRK-TAIINYMD    TO  SPA-K02N-TAIINYMD
           END-IF
           .
       3109-NYUIN-CHK-EXT.
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
      *    MOVE    "DBCLOSE"           TO  MCP-FUNC
      *    MOVE    "SRYACCT-KEY7"      TO  ORC-DBPATH
      *    CALL    "MCPDBSUB"          USING
      *                                MCPAREA
      *                                MCPDATA-REC
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
      *    MOVE    "DBCLOSE"           TO  MCP-FUNC
      *    MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
      *    CALL    "MCPDBSUB"          USING
      *                                MCPAREA
      *                                MCPDATA-REC
      *
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
           IF      IDX-SIN             >   200
               MOVE    200             TO  IDX-SIN
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
               COMPUTE WRK-SRYMM           =  (WRK-SRYMM
                                           +   12   )
                                           -   WRK-KIKAN
               COMPUTE WRK-SRYYY           =   WRK-SRYYY
                                           -   1
           END-IF
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
      *        前カーソル
               WHEN    "CLICKED"       ALSO    "B06S"
                   PERFORM 450-MAECUR-SEC
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
      *        院外院内
               WHEN    "CLICKED"       ALSO    "INGAI"
                   PERFORM 4101-INGAI-SYORI-SEC
      *        処方（頭書）
               WHEN    "CLICKED"       ALSO    "BSH1"
                   MOVE    1                   TO  FLG-SYOHOU
                   PERFORM 4124-SYOHOU-SYORI-SEC
      *        処方（前回）
               WHEN    "CLICKED"       ALSO    "BSH2"
                   MOVE    2                   TO  FLG-SYOHOU
                   PERFORM 4124-SYOHOU-SYORI-SEC
      *        入力コード設定へ
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 4106S-INPUTCD-SYORI-SEC
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
      *        入力コード
               WHEN    "ACTIVATE"      ALSO    "INPUTCD"
      *************PERFORM 41011-KOUI-HEN-SEC
                   PERFORM 41011-KOUI-ALLHEN-SEC
      *        入力コード変換
      ******** WHEN    "ACTIVATE2"     ALSO    "INPUTCD"
      *******      PERFORM 41014-INPUTCD-MODHEN-SEC
      *        名称
               WHEN    "ACTIVATE"      ALSO    "MEISYO"
      **************PERFORM 41012-MEISYO-HEN-SEC
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
                   UNTIL       IDX     >   200
               IF     (SPA-COM-PAGE (IDX)  =   SPA-GMN-PAGE )  AND
                      (SPA-COM-GYOU (IDX)  =   WRK-IDX2     )
                   MOVE    IDX                 TO  SPA-GMN-IDX
                   MOVE    200                 TO  IDX
               END-IF
           END-PERFORM
      *
      *    同一行がない時、計算で求める
           IF      SPA-GMN-IDX         =   ZERO
               COMPUTE SPA-GMN-IDX     =   (SPA-GMN-PAGE   -   1)
                                       *   20
                                       +   WRK-IDX2
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
           MOVE    ZERO                TO  SPA-GMN-CUR
      *    ＳＰＡクリア処理
           INITIALIZE                  SPA-SCR-REC
           INITIALIZE                  SPA-K02
           INITIALIZE                  SPA-K01SET-AREA
           INITIALIZE                  SPA-SYORIFLG
           INITIALIZE                  SPA-DENPNUM
           INITIALIZE                  SPA-CONTFLG
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-SRYYMDW         TO  SPA-GMN-SRYYMD
           MOVE    SPA-SRYYMD          TO  SPA-NAI-SRYYMD
      *
           MOVE    K02-PTNUM           TO  SPA-GMN-PTNUM
      *    Ｎのとき、入院へ
           IF     (SPA-GMN-PTNUM(1:1)  =   "N" OR "n")  AND
                  (SPA-GMN-PTNUM(2:)   =   SPACE     )  AND
                  (SPA-BEDSU           >   ZERO)
               MOVE    SPACE               TO  WRK-PUTTYPE
               PERFORM 450-K02N-SYORI-SEC
               GO      TO      41010-PTNUM-SYORI-EXT
           END-IF
      *    ＊のとき、患者新規登録へ
           IF      SPA-GMN-PTNUM(1:1)      =   "*"
               MOVE    "*"                 TO  SPA-PTNUM
               PERFORM 440-KNJADD-SEC
               GO      TO      41010-PTNUM-SYORI-EXT
           END-IF
      *
           INITIALIZE                      ORCSPTNUMAREA
           MOVE    SPA-GMN-PTNUM       TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
           IF      SPTNUM-RC           =   10  OR  99
               MOVE    "1003"              TO  SPA-ERRCD
               GO  TO                  41010-PTNUM-SYORI-EXT
           END-IF
      *    氏名検索へ
           IF      SPTNUM-RC           =   20
      *
               MOVE    SPACE               TO  LINKAREA
               MOVE    SPA-KYOUTU          TO  LNK-COMMON
               MOVE    SPA-K02             TO  LNK-SCRSPA
      *        画面移行用のパラメタ変更
               MOVE    "K02"               TO  SPA-MOTOPG
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
           MOVE    SPTNUM-PTID         TO  SPTID-PTID
           MOVE    SPTNUM-PTNUM        TO  SPTID-PTNUM
           MOVE    SPTID-PTID          TO  SPA-PTID
           MOVE    SPTID-PTID          TO  SPA-GMN-PTID
           MOVE    SPTID-PTNUM         TO  SPA-PTNUM
           MOVE    SPTID-PTNUM         TO  SPA-GMN-PTNUM
      *
      *    患者マスター存在チェック
           MOVE    SPA-HOSPID          TO  PTINF-HOSPID
           MOVE    SPA-GMN-PTID        TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF       NOT =   ZERO
               MOVE    SPTID-PTID          TO  SPA-GMN-PTID
               MOVE    SPTID-PTNUM         TO  SPA-GMN-PTNUM
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
      *    初期ＳＰＡ編集処理（システム日付を診療日とする）
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-SYSYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SYSYMDWH        TO  LNK-SC95-SRYYMDG
           INITIALIZE                  SPA-DENPNUM
           INITIALIZE                  SPA-SYORIFLG
           INITIALIZE                  SPA-LAST-PTID
           INITIALIZE                  SPA-LAST-PTNUM
           INITIALIZE                  SPA-K02
           INITIALIZE                  SPA-SCR-REC
      *    診療日に変更があったらＳＰＡを指定する
           IF      SPA-SRYYMD      NOT =   SPA-SYSYMD
      *
           MOVE    "1"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
           ELSE
      *        院外処方区分
               MOVE    SPA-SYOHOKBN        TO  SPA-INGAIKBN
           END-IF
      *
           MOVE    "2"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
      *    受付選択
           IF      WRK-MOTOPG          =   "U01"
               MOVE    "U01"               TO  LNK-SC95-PG
           ELSE
               INITIALIZE                  SPA-SRYKA
           END-IF
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      ***  IF      SPA-ERRCD       NOT =   SPACE
      *         GO      TO      41010-PTNUM-SYORI-EXT
      **** END-IF
      *
      *    スパ共通編集処理
           PERFORM                 310-SPA-SET-SEC
           IF      SPA-ERRCD       NOT =   SPACE
                GO      TO      41010-PTNUM-SYORI-EXT
           END-IF
      *
      *    誕生日以前の時、エラー
           IF      SPA-SRYYMD          <   SPA-BIRTHDAY
                MOVE    "1011"         TO  SPA-ERRCD
                MOVE    7              TO  SPA-GMN-CUR
                GO      TO      41010-PTNUM-SYORI-EXT
           END-IF
      *    チェック用月内診療行為セット
           PERFORM                 320-SPA-CHKSET-SEC
      *
           IF      LNK-SC95-DOUJI-RENNUM   NOT =   ZERO
               MOVE    "0103"          TO  SPA-KIDCD
               GO      TO      41010-PTNUM-SYORI-EXT
           END-IF
      *    保険番号が未確定の時、エラー
           IF      SPA-HKNCOMBINUM     =   SPACE
               MOVE    "1006"          TO  SPA-ERRCD
               GO      TO      41010-PTNUM-SYORI-EXT
           END-IF
      *    受診履歴が３で、通常の時、警告
           IF     (SPA-RRK-LSTYMD       =   SPA-SRYYMD )  AND
                  (LNK-SC95-LAST-RENNUM    >=  3      )
               MOVE    "0106"          TO  SPA-KIDCD
               GO      TO      41010-PTNUM-SYORI-EXT
           END-IF
      *    パラメタファイルより表示
           MOVE    3               TO  FLG-PARASET
           PERFORM                 3002-PARA-SET-SEC
      *
      *    画面用スパ編集処理
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
      *    診療日入力処理
      *****************************************************************
       41017-SRYYMD-SYORI-SEC         SECTION.
      *
           MOVE    7                   TO  SPA-GMN-CUR
           MOVE    ZERO                TO  SPA-NAI-ROUSTR
      *
      *    訂正の時、入力エラー
           IF      SPA-SYORIFLG        =   1
               IF      K02-SRYYMD      NOT =   SPA-GMN-SRYYMD
                   MOVE    1                   TO  SPA-GMN-CUR
                   MOVE    "1009"              TO  SPA-ERRCD
               END-IF
               GO      TO      41017-SRYYMD-SYORI-EXT
           END-IF
      *
           MOVE    K02-SRYYMD          TO  SPA-GMN-SRYYMD
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
                    MOVE    "1011"          TO  SPA-ERRCD
                    GO      TO      41017-SRYYMD-SYORI-EXT
               END-IF
      *        未来日は確認メッセージ
               IF      SGDAY-SDAY          >   SPA-SYSYMD
                   MOVE    "0111"              TO  SPA-KIDCD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI-SRYYMD
                   GO      TO      41017-SRYYMD-SYORI-EXT
               END-IF
      *        変更のない時、以降の処理はそのまま
               IF      SGDAY-SDAY          =   SPA-NAI-SRYYMD
                    MOVE    1                  TO  SPA-GMN-CUR
                    GO      TO      41017-SRYYMD-SYORI-EXT
               END-IF
               MOVE    SGDAY-SDAY          TO  SPA-NAI-SRYYMD
           ELSE
               MOVE    "1010"          TO  SPA-ERRCD
               GO      TO      41017-SRYYMD-SYORI-EXT
           END-IF
      *
      *    診療日確定後、編集
           PERFORM 410171-SRYYMD-OK-SEC
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
           MOVE    "1"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
           MOVE    "2"                 TO  LNK-SC95-KBN
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
      *
           IF      LNK-SC95-DOUJI-RENNUM   NOT =   ZERO
               MOVE    "0103"          TO  SPA-KIDCD
               GO      TO      410171-SRYYMD-OK-EXT
           END-IF
      *    保険番号が未確定の時、エラー
           IF      SPA-HKNCOMBINUM     =   SPACE
               MOVE    "1006"          TO  SPA-ERRCD
               GO      TO      410171-SRYYMD-OK-EXT
           END-IF
      *    受診履歴が３で、通常の時、警告
           IF     (SPA-RRK-LSTYMD      =   SPA-SRYYMD )  AND
                  (LNK-SC95-LAST-RENNUM    >=  3      )
               MOVE    "0106"          TO  SPA-KIDCD
               GO      TO      410171-SRYYMD-OK-EXT
           END-IF
      *    パラメタファイルより表示
           MOVE    3               TO  FLG-PARASET
           PERFORM                 3002-PARA-SET-SEC
      *
      *    画面用スパ編集処理
           PERFORM                 310-SPA-GMNSET-SEC
      *
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
           IF    ( K02-HKNCOMBI        =   SPA-GMN-HKNCOMBI-G)  OR
                 ( K02-PTNUM           =   SPACE             )
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
               MOVE    K02-HKNCOMBI        TO  SPA-GMN-HKNCOMBI-G
               MOVE    1                   TO  FLG-CHK2
               PERFORM 41012-HKNCOMBI-CHANGE-SEC
               IF      SPA-KIDCD           =   SPACE
      *            訂正の時、確認メッセージ表示
                   MOVE    "0105"              TO  SPA-KIDCD
               END-IF
           ELSE
               MOVE    SPA-GMN-HKNCOMBI-G  TO  SPA-MAE-HKNCOMBI-G
               MOVE    K02-HKNCOMBI        TO  SPA-GMN-HKNCOMBI-G
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
      *    保険組合
           PERFORM 3101-HKNCOMBI-HEN-SEC
      *
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-GMN-CUR
      *
      *    保険区分が変更された時、再編集を行う
           IF     (SPA-SYORIFLG        =   1   )  AND
                  (SPA-HKNKBN          =   SPA-MAE-HKNKBN)
      *            労災の時、初回算定日を再編集する
             AND  (SPA-HKNKBN          =   ZERO)
               CONTINUE
           ELSE
               PERFORM 410121-HKNCOMBI-SAI-SEC
           END-IF
      *
           .
       41012-HKNCOMBI-CHANGE-EXT.
           EXIT.
      *****************************************************************
      *    保険区分変換処理
      *****************************************************************
       410121-HKNCOMBI-SAI-SEC         SECTION.
      *
      *    複数科まとめチェック
           IF      SPA-SYORIFLG        =   ZERO
               IF      SPA-HKNCOMBINUM     =   SPA-FUKU-HKNCOMBI
                   MOVE    SPA-FUKU-MATKBN     TO  SPA-FUKU-SYORI
               END-IF
           END-IF
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
      *    訂正の時、確認メッセージ表示へ
           IF      FLG-CHK2            =   1
               GO      TO      410121-HKNCOMBI-SAI-EXT
           END-IF
      *
      *    訂正でなく、同日に他の保険を算定している時、メッセージ表示
           IF     (SPA-SYORIFLG        =   ZERO        )  AND
                  (SPA-LSTYMD          =   SPA-SRYYMD  )
               PERFORM 4101213-RRKHKNCHK-SEC
               IF      SPA-KIDCD       NOT =   SPACE
                   GO      TO      410121-HKNCOMBI-SAI-EXT
               END-IF
           END-IF
      *
      *    保険区分が変更された時、再編集を行う
           IF     (SPA-HKNKBN          =   SPA-MAE-HKNKBN  )  AND
                  (SPA-HKNKBN          =   ZERO            )
               CONTINUE
           ELSE
      *        初診算定時のから変更がある時、診察料の変更のみ
               PERFORM 410131-SINSATUHEN-SEC
           END-IF
      *
      ***  IF     (SPA-SYORIFLG        =   1            )  OR
      *           (SPA-GMN-MAX     NOT =   SPA-FRST-LINE)  OR
      *           (SPA-GMN-MAX         >   ZERO    AND
      *            SPA-FRST-SRYCD  NOT =   SPA-COM-INPUTCD(SPA-GMN-MAX))
      *        初診算定時のから変更がある時、診察料の変更のみ
      *        PERFORM 410131-SINSATUHEN-SEC
      *    ELSE
      *        初診算定時のから変更がない時
      *        PERFORM 410132-SAIHENSYU-SEC
      **** END-IF
           .
       410121-HKNCOMBI-SAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    同日再診他保険の算定チェック処理
      *****************************************************************
       4101213-RRKHKNCHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX-HKN     FROM    1   BY  1
                   UNTIL      (IDX-HKN     >   10  )  OR
                              (SPA-TA-HKNCOMBI (IDX-HKN)
                                           =   ZERO )
               IF     SPA-TA-HKNCOMBI (IDX-HKN)
                                           NOT =   SPA-GMN-HKNCOMBINUM
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *    他科受診済みがないこと
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   200                )  OR
                              (SPA-COM-INPUTCD(IDX) =   SPACE  AND
                               SPA-GMN-INPUTCD(IDX) =   SPACE )
               IF     (SPA-COM-INPUTCD (IDX)   =   "830000020"   OR
                                                   "830000021" )  AND
                      (SPA-COM-SRYKBN  (IDX)   =   "12"  OR  "11")
                   MOVE    ZERO                TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   1
      *        他保険あり確認メッセージ表示
               MOVE    "0119"              TO  SPA-KIDCD
           END-IF
           .
       4101213-RRKHKNCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診察料再編集処理
      *****************************************************************
       410131-SINSATUHEN-SEC         SECTION.
      *
      *    スパ共通編集処理
           PERFORM                     310-SPA-SET-SEC
      *    他保険受診がある時、診察料はないものとする
           IF      SPA-TAHOKEN-FLG     =   1
               GO      TO      410131-SINSATUHEN-EXT
           END-IF
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSC10SAREA
           MOVE    "S"                 TO  LNK-SC10S-SYORI
           IF      SPA-TAHOKEN-FLG     =   9
      *        他保険ありの時
               MOVE    "H"                 TO  LNK-SC10S-SYORI
           END-IF
           IF      SPA-TAHOKEN-FLG     =   2
      *        科変更時
               MOVE    "K"                 TO  LNK-SC10S-SYORI
           END-IF
           CALL    "ORCSC10S"          USING
                                       ORCSC10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           MOVE    ZERO                TO  SPA-TAHOKEN-FLG
           IF      SPA-TAHOKEN-FLG     =   2
               CONTINUE
           ELSE
               IF      SPA-KIDCD           =   "0109"
                   MOVE    SPACE               TO  SPA-KIDCD
               END-IF
           END-IF
      *
      *    診療行為、計計算処理、編集
           MOVE    "2"             TO  FLG-TOROKU
           MOVE    SPACE           TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE           TO  FLG-TOROKU
      *
           .
       410131-SINSATUHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面編集処理
      *****************************************************************
       410132-SAIHENSYU-SEC         SECTION.
      *
      *    スパ共通編集処理
           PERFORM                     310-SPA-SET-SEC
      *
           IF      LNK-SC95-DOUJI-RENNUM   NOT =   ZERO
               MOVE    "0103"          TO  SPA-KIDCD
               GO      TO      410132-SAIHENSYU-EXT
           END-IF
      *    受診履歴が３で、通常の時、警告
           IF     (SPA-RRK-LSTYMD          =   SPA-SRYYMD )  AND
                  (LNK-SC95-LAST-RENNUM    >=  3      )
               MOVE    "0106"          TO  SPA-KIDCD
               GO      TO      410132-SAIHENSYU-EXT
           END-IF
           INITIALIZE                  SPA-SCR-REC
      *    パラメタファイルより表示
           MOVE    3               TO  FLG-PARASET
           PERFORM                 3002-PARA-SET-SEC
      *
      *    画面用スパ編集処理
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
           IF      K02-SRYKA           =   SPA-GMN-SRYKA-G
               MOVE    1               TO  SPA-GMN-PAGE
               MOVE    1               TO  SPA-GMN-CUR
               GO      TO      41013-SRYKAI-SYORI-EXT
           END-IF
      *    訂正の時、エラー
           IF      SPA-SYORIFLG        =   1
      *********MOVE    "1008"              TO  SPA-ERRCD
      *******  CONTINUE
      *        訂正で継続のある時、エラーとする
               IF      (SPA-CONTKBN        =   "1"   OR  "2")  AND
                       (SPA-DENPJTIKBN     =   "1"   )
                   MOVE    "1029"              TO  SPA-ERRCD
                   GO      TO      41013-SRYKAI-SYORI-EXT
               END-IF
           ELSE
               IF      SPA-PTID            =   ZERO
                   MOVE    ZERO                TO  SPA-GMN-CUR
                   MOVE    K02-SRYKA           TO  SPA-GMN-SRYKA-G
                   GO      TO      41013-SRYKAI-SYORI-EXT
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-SRYKA-G     TO  SPA-MAE-SRYKA-G
           MOVE    K02-SRYKA           TO  SPA-GMN-SRYKA-G
           IF      SPA-SYORIFLG        =   1
               MOVE    "0123"              TO  SPA-KIDCD
           ELSE
               MOVE    "0107"              TO  SPA-KIDCD
           END-IF
      *
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
      *
           MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
           MOVE    SPA-GMN-SRYKAMEI    TO  SPA-SRYKAMEI
      *    対象の科の保険組合せにする
           IF      SPA-SYORIFLG        =   ZERO
               MOVE    SPACE               TO  SPA-HKNCOMBINUM
               MOVE    SPACE               TO  SPA-DRCD
           END-IF
      *
           MOVE    SPA-SCR-REC         TO  WRK-SCR-REC
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
      *    複数科まとめチェック
           IF      SPA-SYORIFLG        =   ZERO
               IF      SPA-HKNCOMBINUM     =   SPA-FUKU-HKNCOMBI
                   MOVE    SPA-FUKU-MATKBN     TO  SPA-FUKU-SYORI
               END-IF
           END-IF
      *
           MOVE    WRK-SCR-REC         TO  SPA-SCR-REC
           IF     (SPA-KID1-FLG    NOT =   "OK"  )  AND
                  (SPA-SYORIFLG        =   ZERO  )
               INITIALIZE                  SPA-SCR-REC
           END-IF
      *
      *    再表示編集処理
           IF      LNK-SC95-WKSRYACT   =   1
               MOVE    "0122"          TO  SPA-KIDCD
               GO      TO      41013-SRYKA-CHANGE-EXT
           END-IF
           MOVE    1               TO  SPA-GMN-PAGE
           MOVE    1               TO  SPA-GMN-CUR
           MOVE    1               TO  SPA-MAP-IDX
      *    訂正の時、以下の処理なし
           IF      SPA-SYORIFLG        =   1
               GO      TO      41013-SRYKA-CHANGE-EXT
           END-IF
      *
           MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
           MOVE    SPA-GMN-SRYKAMEI    TO  SPA-SRYKAMEI
      *
      **   MOVE    SPA-DRCD            TO  WRK-DRCD
      *    MOVE    SPACE               TO  SPA-DRCD
      *
      *    初期ＳＰＡ編集処理
      *    INITIALIZE                  ORCSC95AREA
      *    MOVE    SPA-NAI-SRYYMD      TO  LNK-SC95-SRYYMD
      *    MOVE    SPA-GMN-SRYYMD      TO  LNK-SC95-SRYYMDG
      *    INITIALIZE                  SPA-DENPNUM
      *    INITIALIZE                  SPA-SYORIFLG
      *    INITIALIZE                  SPA-K02-AREA
      *    INITIALIZE                  SPA-SCR-REC
      *    MOVE    SPA-SRYKA           TO  SPA-GMN-SRYKA
      *    MOVE    SPA-SRYKAMEI        TO  SPA-GMN-SRYKAMEI
      **** MOVE    "5"                 TO  LNK-SC95-KBN
      *    MOVE    "2"                 TO  LNK-SC95-KBN
      *    MOVE    "K02"               TO  LNK-SC95-PG
      *    CALL    "ORCSC95"           USING
      *                                MCPAREA
      *                                ORCSC95AREA
      *                                SPA-AREA
      *                                SPA-K01KYOUTU
      *                                SPA-K02
      *    IF      SPA-DRCD            =   SPACE
      *        MOVE    WRK-DRCD            TO  SPA-DRCD
      *    END-IF
      *
      *    スパ共通編集処理
      *    PERFORM                 310-SPA-SET-SEC
      *
      *    チェック用月内診療行為セット
      **** PERFORM                 320-SPA-CHKSET-SEC
      *
           IF      LNK-SC95-DOUJI-RENNUM   NOT =   ZERO
               MOVE    "0103"          TO  SPA-KIDCD
               GO      TO      41013-SRYKA-CHANGE-EXT
           END-IF
      *    保険番号が未確定の時、エラー
           IF      SPA-HKNCOMBINUM     =   SPACE
               MOVE    "1006"          TO  SPA-ERRCD
               GO      TO      41013-SRYKA-CHANGE-EXT
           END-IF
      *    受診履歴が３で、通常の時、警告
           IF     (SPA-RRK-LSTYMD          =   SPA-SRYYMD )  AND
                  (LNK-SC95-LAST-RENNUM    >=  3      )
               MOVE    "0106"          TO  SPA-KIDCD
               GO      TO      41013-SRYKA-CHANGE-EXT
           END-IF
      *    パラメタファイルより表示
      ***  MOVE    3               TO  FLG-PARASET
      **** PERFORM                 3002-PARA-SET-SEC
      *
      *    スパ共通編集処理
           PERFORM                 310-SPA-SET-SEC
      *
      *    画面用スパ編集処理
      *****PERFORM                 310-SPA-GMNSET-SEC
      *
      *    診察料のみ変換する
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSC10SAREA
           MOVE    "K"                 TO  LNK-SC10S-SYORI
           CALL    "ORCSC10S"          USING
                                       ORCSC10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           MOVE    SPA-KIDCD       TO  WRK-KIDCD
      *
      *    診療行為、計計算処理、編集
           MOVE    SPACE           TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE           TO  FLG-TOROKU
      *
           MOVE    ZERO            TO  SPA-SAISIN-CHK
      *
           MOVE    1               TO  SPA-GMN-PAGE
           MOVE    1               TO  SPA-GMN-CUR
           MOVE    1               TO  SPA-MAP-IDX
           MOVE    WRK-KIDCD       TO  SPA-KIDCD
           .
       41013-SRYKA-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       41011-KOUI-ALLHEN-SEC               SECTION.
      *
      *    患者の入力チェック
           IF      SPA-PTID            =   ZERO
               IF      K02-INPUTCD(1)      NOT =   SPACE
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
      *    前回のＳＰＡを保存する
           MOVE    SPA-SCR-REC         TO  MAE-SCR-REC
      *
           MOVE    1                   TO  FLG-INPUT-NASI
      *    カーソル位置のクリア等
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   200
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
           PERFORM VARYING     WRK-IDX2    FROM    1   BY  1
                   UNTIL       WRK-IDX2    >   20
      *        ＳＰＡ内の行決定
               PERFORM 41011-GYOU-SET-SEC
               IF     (K02-INPUTCD (WRK-IDX2)  NOT =
                                       SPA-GMN-INPUTCD (SPA-GMN-IDX)) OR
                      (K02-MEISYO  (WRK-IDX2)   NOT =
                                       SPA-GMN-MEISYO-G (SPA-GMN-IDX))
                   MOVE    "1"                 TO  SPA-COM-IN2
                                                           (SPA-GMN-IDX)
      *************MOVE    "1"                 TO  SPA-COM-IN
      ************                                        (SPA-GMN-IDX)
                   MOVE    ZERO                TO  FLG-INPUT-NASI
               END-IF
      *
               MOVE    K02-INPUTCD (WRK-IDX2)  TO  SPA-GMN-INPUTCD
                                                           (SPA-GMN-IDX)
               MOVE    K02-MEISYO  (WRK-IDX2)  TO  SPA-GMN-MEISYO-G
                                                           (SPA-GMN-IDX)
      *        入力行の保存
               IF     (WRK-IN-MAP-IDX  NOT =   ZERO )  AND
                      (WRK-IDX2            =   WRK-IN-MAP-IDX)
                   MOVE    "1"                 TO  SPA-COM-IN
                                                          (SPA-GMN-IDX)
                   MOVE    "1"                 TO  SPA-COM-IN2
                                                           (SPA-GMN-IDX)
                   IF     (K02-INPUTCD (WRK-IDX2)  =   SPACE )  AND
                          (SPA-MAE-INPUTCD (SPA-GMN-IDX)
                                                   =   SPACE )  AND
                          (SPA-COM-INPUTCD (SPA-GMN-IDX)
                                                   =   SPACE )  AND
                          (SPA-GMN-IDX             >   SPA-GMN-MAX)
                       MOVE    1                   TO  FLG-KUHAKU
                   END-IF
               END-IF
      *
               IF      K02-INPUTCD (WRK-IDX2)  NOT =   SPACE
                   IF      WRK-STR             =   ZERO
                       MOVE    SPA-GMN-IDX         TO  WRK-STR
                   END-IF
               END-IF
           END-PERFORM
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
                   UNTIL       IDX     >   200
               IF     (SPA-COM-PAGE (IDX)  =   SPA-GMN-PAGE )  AND
                      (SPA-COM-GYOU (IDX)  =   WRK-IDX2     )
                   MOVE    IDX                 TO  SPA-GMN-IDX
                   MOVE    200                 TO  IDX
               END-IF
           END-PERFORM
      *    同一行がない時、計算で求める
           IF      SPA-GMN-IDX         =   ZERO
               COMPUTE SPA-GMN-IDX     =   (SPA-GMN-PAGE   -   1)
                                       *   20
                                       +   WRK-IDX2
           END-IF
      *
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
      *
      *    変換・削除処理
           MOVE    WRK-STR             TO  SPA-GMN-IDX
           MOVE    ZERO                TO  WRK-IDX2
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    ZERO                TO  FLG-INS
           PERFORM VARYING SPA-GMN-IDX     FROM    WRK-STR  BY  1
                   UNTIL   SPA-GMN-IDX     >   200
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
      *    入力がない時、以下をしない
           IF      FLG-INPUT-NASI      =   1
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF
      *
      *    入力のチェック
           MOVE    WRK-STR             TO  SPA-GMN-IDX
           MOVE    ZERO                TO  FLG-INS
           PERFORM
                   UNTIL      (SPA-GMN-IDX     >   200   )  OR
                              (SPA-ERRCD   NOT =   SPACE )  OR
                              (FLG-END     NOT =   ZERO  )
               IF     (SPA-MAE-INPUTCD (SPA-GMN-IDX)  NOT =
                                       SPA-GMN-INPUTCD (SPA-GMN-IDX)) OR
                      (SPA-COM-IN2 (SPA-GMN-IDX)   =   "1"  )
                   PERFORM 41011-KOUI-HEN2-SEC
               END-IF
      *        フリーコメントの名称の全角チェックはすべて行う
               IF     (SPA-ERRCD           =   SPACE )  AND
                      (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:2)
                                           =   "81" OR "83" OR "84") AND
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
      *
           IF      SPA-ERRCD           =   "0021"
      *        診療コード入力
               PERFORM 410119-SAIINPUT-SEC
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF
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
      *
      *    点数計算・剤別チェック処理
           MOVE    SPACE           TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           IF     (SPA-ERRCD       =   SPACE
                                   OR  "0021" )  AND
                  (FLG-END         =   ZERO   )
               CONTINUE
           ELSE
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF
      *
      *    診療コード入力
           PERFORM 410119-SAIINPUT-SEC
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
           MOVE    SPA-GMN-IDX         TO  LNK-SC97-GMN-IDX
           MOVE    "1"                 TO  LNK-SC97-KBN
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
      *    削除時
           IF      LNK-SC97-INPUTCD(1:1)   =   SPACE
      *
      *        セットが削除の時セット内容を削除する
               IF     (SPA-MAE-INPUTCD(SPA-GMN-IDX)(1:1)   =   "S") OR
                     ((SPA-GMN-IDX             <   200    )  AND
                      (SPA-COM-SET  (SPA-GMN-IDX + 1)      =   "1"))
                   PERFORM 41016-SETDEL-SEC
               END-IF
               MOVE    SPACE           TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               MOVE    1               TO  FLG-INS
               GO  TO                  41011-KOUI-HENDEL-EXT
           END-IF
      *
      *    剤削除時
           IF      LNK-SC97-INPUTCD(1:1)   =   "-"
               MOVE    LNK-SC97-INPUTCD
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               PERFORM 41015-ZAIDEL-SEC
               MOVE    1               TO  FLG-INS
               GO  TO                  41011-KOUI-HENDEL-EXT
           END-IF
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
               MOVE    "3"             TO  SPA-COM-CUR (SPA-GMN-IDX)
               ADD     1               TO  SPA-GMN-IDX
               MOVE    1               TO  FLG-INS
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
      *    再診時、初診料算定へ変更する
           IF    ((SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:4)  =   ".110"  AND
                   SPA-GMN-INPUTCD(SPA-GMN-IDX)(5:)   =   SPACE )   OR
                  (SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:3)  =   ".11"  AND
                   SPA-GMN-INPUTCD(SPA-GMN-IDX)(4:)   =   SPACE )) AND
                  (SPA-GMN-IDX                        >   1     )  AND
                  (SPA-GMN-INPUTCD(SPA-GMN-IDX + 1)   =   SPACE )
               MOVE    SPA-MAE-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               PERFORM 4101120-SAISIN-SYOSIN-SEC
               GO      TO                  41011-KOUI-HEN2-EXT
           END-IF
      *
           IF     LNK-SC97-NYURYOKU    =   1
      *        セットが変更削除の時セット内容を削除する
               IF     (SPA-MAE-INPUTCD(SPA-GMN-IDX)(1:1)   =   "S" ) OR
                      ((SPA-GMN-IDX            <   200   )  AND
                       (SPA-COM-SET  (SPA-GMN-IDX + 1)     =   "1"))
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
               MOVE    WRK-SRYSYUKBNMEI    TO  SPA-GMN-MEISYO-G
                                                    (SPA-GMN-IDX)
           ELSE
      *        文字変換・編集処理
               PERFORM 410112-INPUTCD-HENKAN-SEC
      *
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-MAE-INPUTCD(SPA-GMN-IDX)
           ELSE
               MOVE    SPACE           TO  SPA-MAE-INPUTCD(SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
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
           MOVE    "K02"               TO  SPA-MOTOPG
           MOVE    "K02"               TO  SPA-K99-MOTOPG
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
               END-IF
      *        用法
               IF      SPA-K98-INPUTCD(3:1)    =   "Y" OR "y"
                   MOVE    "U"                 TO  SPA-K98-GMN-SYORI
                   MOVE    "U1"                TO  SPA-K98-GMN-SYORI3
               END-IF
      *        診療種別一覧
               IF      SPA-K98-INPUTCD(3:1)    =   "."
                   MOVE    "."                 TO  SPA-K98-INPUTCD
               ELSE
                   MOVE    SPACE               TO  SPA-K98-INPUTCD
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
      *
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
           MOVE    "K02"               TO  SPA-MOTOPG
           MOVE    "K02"               TO  SPA-K98-MOTOPG
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
                   UNTIL       IDX     >   200
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
           IF      WRK-IN-IDX          =   ZERO
      *******  MOVE    SPA-GMN-MAX         TO  SPA-GMN-IDX
               MOVE    WRK-MAX-IDX         TO  SPA-GMN-IDX
           ELSE
               MOVE    WRK-IN-IDX          TO  SPA-GMN-IDX
           END-IF
           IF      SPA-GMN-IDX         =   ZERO
               GO      TO      410119-SAIINPUT-EXT
           END-IF
      *    数量・回数入力なし
           IF     (SPA-COM-SURFLG (SPA-GMN-IDX)  =   SPACE ) AND
                  (SPA-COM-IN2    (SPA-GMN-IDX)  =   "2"   ) AND
                  (SPA-COM-KAIFLG (SPA-GMN-IDX)  =   SPACE ) AND
                  (WRK-IN-IDX2                   =   ZERO  )
               MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                            TO  WRK-INPUTCD
               MOVE    "3"                  TO  SPA-COM-CUR
                                                       (SPA-GMN-IDX)
               IF      SPA-COM-INPUTCD (SPA-GMN-IDX)(1:3)  =   "001"
      *            投薬の時のみ、回数入力へ
                   IF      SPA-COM-SRYKBN(SPA-GMN-IDX)     =   "21" OR
                                                               "22" OR
                                                               "23"
                       STRING  WRK-INPUTCD      DELIMITED  BY  SPACE
                               "*"              DELIMITED  BY  SIZE
                                                INTO  SPA-GMN-INPUTCD
                                                         (SPA-GMN-IDX)
                       END-STRING
                   ELSE
                       MOVE    SPACE            TO  SPA-COM-CUR
                                                       (SPA-GMN-IDX)
                   END-IF
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
               IF      SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)
                                           =   "81"  OR  "83"
                   MOVE    "3"             TO  SPA-COM-IN2 (SPA-GMN-IDX)
               END-IF
           END-IF
      *
      *    空白行入力時、前の行へ
           IF     (FLG-KUHAKU          =   1  )  AND
                  (SPA-GMN-IDX         >   ZERO )
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
                         OR  ((SPA-COM-INPUTCD(SPA-GMN-IDX)(1:2)
                                                   =   "84" ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
                           MOVE    "_"             TO  SPA-GMN-INPUTCD
                                                     (SPA-GMN-IDX)
                                                     (WRK-IN-IDX2:)
                       ELSE
                           MOVE    "*"             TO  SPA-GMN-INPUTCD
                                                     (SPA-GMN-IDX)
                                                     (WRK-IN-IDX2:)
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
      *    入力コード変更ありの時
           IF      LNK-SC97-NYURYOKU   =   1
      *
      *    診療行為コードをもとに点数マスター検索
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
               IF      SPA-COM-INPUTCD(SPA-GMN-IDX)(1:2)
                                                   =   "84"
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
      *    初診料でコードの後ろの日付の入っている時初診料算定済みとする
           IF     (SPA-GMN-IDX         =   1  )     AND
                  (SPA-GMN-MAX1        =   ZERO  )  AND
                  (LNK-SC97-SURYOCNT   =   7     )  AND
                  (SPA-SYOYMD          =   SPACE )
               PERFORM 4101125-SYOSIN-CHK-SEC
               IF      SPA-KIDCD       NOT =   SPACE
                   GO      TO      4101113-INPUTCD-CHK-EXT
               END-IF
           ELSE
      *        上記以外で、数量５文字以上はエラー
               IF     (SPA-COM-INPUTCD(SPA-GMN-IDX) (1:2)
                                           NOT =   "09" )  AND
                      (LNK-SC97-SURYOFLG        >   5   )
                   MOVE    "0051"          TO  SPA-ERRCD
                   GO      TO      4101113-INPUTCD-CHK-EXT
               END-IF
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
      *    当月入院有無
           MOVE    SPA-K02N-NYUINFLG   TO  LNK-SC92-NYUINFLG
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
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
      *    時間外加算の時、ＳＰＡへ
           IF      SPA-COM-TIMEFLG(SPA-GMN-IDX)  NOT =   ZERO
               MOVE    SPA-COM-TIMEFLG(SPA-GMN-IDX)
                                           TO  SPA-NAI-TIMEFLG
           END-IF
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
      *    初診料算定済みチェック処理
      *****************************************************************
       4101125-SYOSIN-CHK-SEC       SECTION.
      *
           IF       SPA-COM-INPUTCD(SPA-GMN-IDX)
                                       =   "111700110"  OR
                                           "111700210"  OR
                                           "111000110"  OR
                                           "111003610"  OR
                                           "113003510"  OR
                                           "113003710"
      *        労災追加
                                       OR  "101110010"
               CONTINUE
           ELSE
               GO      TO      4101125-SYOSIN-CHK-EXT
           END-IF
      *
      *    数量の日付変換
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-SURYO-S1        TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-SDAY          TO  SPA-SYOSIN-YMD
               MOVE    SPA-COM-INPUTCD(SPA-GMN-IDX)
                                           TO  SPA-SYOSIN-SRYCD
               MOVE    "0108"              TO  SPA-KIDCD
           END-IF
      *
           .
       4101125-SYOSIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    再診時、初診料算定へ変更処理
      *****************************************************************
       4101120-SAISIN-SYOSIN-SEC       SECTION.
      *
      *    前に再診があること
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >=   SPA-GMN-IDX
               IF     (SPA-COM-SRYKBN (IDX) =   "12" )  OR
                      (SPA-COM-INPUTCD(IDX) =   "113003610" OR
                                                "113003810")
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK             =   ZERO
               GO      TO      4101120-SAISIN-SYOSIN-EXT
           END-IF
      *
      *    再診で、当日以降受診が無いこと
           IF      (SPA-SYORIFLG       =   ZERO )  AND
                   (SPA-SYOSIN         =   ZERO )  AND
                   (SPA-LSTYMD         <   SPA-SRYYMD)
      *        病名をチェックする
               PERFORM 41011201-BYOMEI-CHK-SEC
      *        初診料算定時、算定不可のコードチェック
               PERFORM 41011202-SYOSIN-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      4101120-SAISIN-SYOSIN-EXT
               END-IF
      *
               IF      FLG-KEIZOKU         =   ZERO
                   MOVE    "0110"              TO  SPA-KIDCD
               ELSE
                   MOVE    "0112"              TO  SPA-KIDCD
               END-IF
           ELSE
               MOVE    "1016"              TO  SPA-ERRCD
           END-IF
      *
           .
       4101120-SAISIN-SYOSIN-EXT.
           EXIT.
      *****************************************************************
      *    初診料算定へ変更の病名をチェックする処理
      *****************************************************************
       41011201-BYOMEI-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-KEIZOKU
      *    患者病名チェック
           MOVE    SPACE               TO  PTBYOMEI-REC
           INITIALIZE                  PTBYOMEI-REC
           MOVE    SPA-HOSPID          TO  PTBYO-HOSPID
           MOVE    SPA-PTID            TO  PTBYO-PTID
           MOVE    SPA-SRYYMD          TO  PTBYO-SRYYMD
           MOVE    SPA-SRYYMD          TO  PTBYO-TENKIYMD
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTBYOMEI-KEY9"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PTBYOMEI-KEY9"     TO  ORC-DBPATH
               PERFORM 980-PTBYOMEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
               MOVE    SPACE               TO  PTBYOMEI-REC
               INITIALIZE                  PTBYOMEI-REC
           END-IF
           PERFORM UNTIL       FLG-PTBYOMEI    =   1   OR
                               FLG-KEIZOKU     =   1
               IF      PTBYO-SRYYMD        <   SPA-SRYYMD
                   EVALUATE    PTBYO-TENKIKBN
      *                継続
                       WHEN    " "
                           MOVE    1               TO  FLG-KEIZOKU
                   END-EVALUATE
               END-IF
               MOVE    "PTBYOMEI-KEY9"     TO  ORC-DBPATH
               PERFORM 980-PTBYOMEI-READ-SEC
           END-PERFORM
      *
      *
           .
       41011201-BYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初診料算定時、算定不可のコードチェック処理
      *****************************************************************
       41011202-SYOSIN-CHK-SEC       SECTION.
      *
           MOVE    SPA-SCR-REC         TO  WRK-SCR-REC
           MOVE    SPA-GMN-IDX         TO  WRK-GMN-IDX
      *    初診料算定チェック
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSC10SAREA
           MOVE    "9"                 TO  LNK-SC10S-SYORI
           CALL    "ORCSC10S"          USING
                                       ORCSC10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           MOVE    WRK-SCR-REC         TO  SPA-SCR-REC
           MOVE    WRK-GMN-IDX         TO  SPA-GMN-IDX
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "1025"          TO  SPA-ERRCD
           END-IF
      *
           .
       41011202-SYOSIN-CHK-EXT.
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
                       ELSE
      *                数量　＞　きざみ値上限の時、エラー
                           IF      SPA-COM-SURYO(SPA-GMN-IDX)
                                                   >   TNS-KZMJGN
                                   MOVE    "0037"          TO  SPA-ERRCD
                           END-IF
                       END-IF
                   ELSE
      *                コード変更なし
                       IF      SPA-COM-KZMCOMPSIKIBETU(SPA-GMN-IDX)
                                                       =   0
                           IF      WRK-SUR-MCNT    NOT =   ZERO
                               MOVE    "0002"          TO  SPA-ERRCD
                           END-IF
                       ELSE
      *                数量　＞　きざみ値上限の時、エラー
                           IF      SPA-COM-SURYO(SPA-GMN-IDX)
                                                   >   SPA-COM-KZMJGN
                                                           (SPA-GMN-IDX)
                               MOVE    "0037"          TO  SPA-ERRCD
                           END-IF
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
                                                   OR  4       )  AND
                      (((LNK-SC97-NYURYOKU     =   1    )  AND
                        (TNS-DATAKBN           =   "1"  ))   OR
                       ((LNK-SC97-NYURYOKU     =   ZERO   )  AND
                        (SPA-COM-DATAKBN(SPA-GMN-IDX)
                                               =   "1"  )) )
                   IF     (SPA-NAI-TIMEFLG     NOT =   ZERO    )  AND
                          (SPA-NAI-TIMEFLG     NOT =   LNK-SC97-JIKAN)
                                                                  AND
                          (SPA-COM-TIMEFLG(SPA-GMN-IDX)
                                                   =   ZERO    )
                       MOVE    "0030"              TO  SPA-ERRCD
                   ELSE
                       MOVE    LNK-SC97-JIKAN      TO  SPA-COM-TIMEFLG
                                                          (SPA-GMN-IDX)
                   END-IF
               ELSE
                   MOVE    "0030"          TO  SPA-ERRCD
               END-IF
           ELSE
               IF      SPA-COM-TIMEFLG(SPA-GMN-IDX)
                                           NOT =   ZERO
                   MOVE    ZERO            TO  SPA-NAI-TIMEFLG
                   MOVE    ZERO            TO  SPA-COM-TIMEFLG
                                                          (SPA-GMN-IDX)
               END-IF
           END-IF
           .
       4101121-INPUTCD-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目編集処理
      *****************************************************************
       4101122-KOMOKU-HENSYU-SEC      SECTION.
      *
      *    数量
           IF      WRK-SUR-MCNT        =   ZERO
               MOVE    1               TO  SPA-COM-SURYO(SPA-GMN-IDX)
               MOVE    SPACE           TO  SPA-COM-SURFLG(SPA-GMN-IDX)
           ELSE
               MOVE    WRK-SURYO       TO  SPA-COM-SURYO(SPA-GMN-IDX)
               IF      SPA-COM-SURYO(SPA-GMN-IDX)  =  ZERO
                   MOVE    1           TO  SPA-COM-SURYO(SPA-GMN-IDX)
               END-IF
               MOVE    "1"             TO  SPA-COM-SURFLG(SPA-GMN-IDX)
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
           MOVE    1                   TO  SPA-SINSATU-SAI
      *
           MOVE    SPACE               TO  WRK-ERRCD
      *
           MOVE    ZERO                TO  WRK-IDX-YAKUSOKU
           MOVE    ZERO                TO  IDX-YAKUSOKU
           PERFORM 4101191-INPUTSET-SYORI-SEC
      *
      *    約束セットがなくなるまで以下の処理を行う
           MOVE    ZERO                TO  IDX-IP
           PERFORM VARYING     IDX-YAK FROM    1   BY  1
                   UNTIL      (IDX-YAK     >   20 )  OR
                              (WRK-IDX-YAK(IDX-YAK)  =   ZERO  )  OR
                              (SPA-ERRCD     NOT =   SPACE )
               COMPUTE SPA-GMN-IDX         =   WRK-IDX-YAK (IDX-YAK)
                                           +   IDX-IP
               MOVE    ZERO                TO  IDX-IP
               IF      SPA-GMN-INPUTCD (SPA-GMN-IDX)(1:1)  =   "S"
                   PERFORM 4101191-INPUTSET-SYORI2-SEC
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-ERRCD       TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410119-INPSET-EXT
           END-IF
      *
      *    MOVE    SPACE           TO  FLG-TOROKU
      *    PERFORM 510-TBLHEN-SEC
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
           MOVE    ZERO                TO  FLG-YAKUSOKU
           MOVE    ZERO                TO  IDX-YAKUSOKU
      *
      *    約束の時、１行前に診療種別があること
           IF      SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)   =   "S"
               IF     (SPA-GMN-IDX         =   1   )   OR
                      (SPA-GMN-INPUTCD(SPA-GMN-IDX - 1)(1:1)
                                       NOT =  ".")
                   MOVE    "0048"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  SPA-COM-CUR
                                                        (SPA-GMN-IDX)
               END-IF
           END-IF
      *
      *    行オーバーの為、ＳＰＡを保存する
           MOVE    SPA-SCR-REC         TO  WRK-SCR-REC
           MOVE    SPA-GMN-IDX         TO  WRK-GMN-IDX
      *
           MOVE    ZERO                TO  IDX-IP
           MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:6)
                                       TO  WRK-SETCD
      *    入力セットマスタ検索
           MOVE    SPACE               TO  INPUTSET-REC
           INITIALIZE                      INPUTSET-REC
           MOVE    SPA-HOSPID          TO  ISET-HOSPID
           MOVE    WRK-SETCD           TO  ISET-SETCD
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "INPUTSET-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "INPUTSET-KEY2"     TO  ORC-DBPATH
               PERFORM 975-INPUTSET-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
      *
      *    約束処方の初期処理
           IF     (SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)   =   "S" )  AND
                  (FLG-INPUTSET        =   ZERO  )
      *
               IF      FLG-TOROKU      NOT =   "2"
                   PERFORM 4101192-INPSET-SET-SEC
               END-IF
      *        行を挿入して追加する
               ADD     1               TO  SPA-GMN-IDX
               PERFORM 41014-GYOINSN-SEC
           END-IF
      *
           PERFORM
                   UNTIL     (FLG-INPUTSET     =   1 )  OR
                             (SPA-ERRCD    NOT =   SPACE ) OR
                             (SPA-GMN-IDX      >   200 )
      *        セット展開
               MOVE    1                   TO  FLG-SETSYORI
      *
               ADD     1               TO  IDX-IP
      *
               MOVE    ISET-INPUTCD    TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               IF      ISET-SURYO1     =   ZERO
                   MOVE    1           TO  SPA-COM-SURYO (SPA-GMN-IDX)
               ELSE
                   MOVE    ISET-SURYO1 TO  SPA-COM-SURYO (SPA-GMN-IDX)
               END-IF
               MOVE    SPA-COM-SURYO (SPA-GMN-IDX)
                                       TO  SPA-COM-SURYO2(SPA-GMN-IDX)
               IF      ISET-SURYO2     =   ZERO
                   MOVE    1           TO  SPA-COM-BUNGSU(SPA-GMN-IDX)
               ELSE
                   MOVE    ISET-SURYO2 TO  SPA-COM-BUNGSU(SPA-GMN-IDX)
               END-IF
               IF      ISET-KAISU      =   ZERO
                   MOVE    1           TO  SPA-COM-KAISU  (SPA-GMN-IDX)
               ELSE
                   MOVE    ISET-KAISU  TO  SPA-COM-KAISU  (SPA-GMN-IDX)
                   MOVE    "1"         TO  SPA-COM-KAIFLG (SPA-GMN-IDX)
               END-IF
               MOVE    SPA-COM-KAISU (SPA-GMN-IDX)
                                       TO  SPA-COM-KAISU2(SPA-GMN-IDX)
      *
      *********MOVE    ISET-COMMENT    TO  SPA-GMN-MEISYO(SPA-GMN-IDX)
      *
               IF      ISET-INPUTCD        =   "810000001"
                   MOVE    ISET-COMMENT    TO  SPA-GMN-MEISYO-G
                                                          (SPA-GMN-IDX)
                   MOVE    "1"             TO  SPA-COM-MEIFLG
                                                          (SPA-GMN-IDX)
               ELSE
                   MOVE    ISET-COMMENT    TO  SPA-GMN-MEISYO
                                                          (SPA-GMN-IDX)
                   IF      ISET-INPUTCD(1:2)   =   "83" 
                       MOVE    "1"         TO  SPA-COM-MEIFLG
                                                          (SPA-GMN-IDX)
                   END-IF
               END-IF
      *
               MOVE    ISET-ATAI (01)  TO  SPA-COM-ATAI1 (SPA-GMN-IDX)
               MOVE    ISET-ATAI (02)  TO  SPA-COM-ATAI2 (SPA-GMN-IDX)
               MOVE    ISET-ATAI (03)  TO  SPA-COM-ATAI3 (SPA-GMN-IDX)
               MOVE    ISET-ATAI (04)  TO  SPA-COM-ATAI4 (SPA-GMN-IDX)
      *
      *        約束処方の時、画面表示なし
               IF      SPA-GMN-INPUTCD(WRK-GMN-IDX)(1:1)   =   "S"
      *            画面表示なし
                   MOVE    "1"         TO  SPA-COM-SET (SPA-GMN-IDX)
               END-IF
      *
               IF       SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)  =   "." 
      *            診療種別区分名称チェック
                   MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:3)
                                               TO  WRK-SRYSYUKBN
                   PERFORM 510-SRYKBN-MEI-SEC
               ELSE
                   MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:9)
                                               TO  SPA-COM-INPUTCD
                                                          (SPA-GMN-IDX)
      *
      *            約束処方の時、数量を計算する
                   IF      SPA-GMN-INPUTCD(WRK-GMN-IDX)(1:1)   =   "S"
      *
      *                数量変更
                       COMPUTE SPA-COM-SURYO (SPA-GMN-IDX)
                                               =   SPA-COM-SURYO
                                                         (SPA-GMN-IDX)
                                               *   SPA-COM-SURYO
                                                         (WRK-GMN-IDX)
                       MOVE    SPA-COM-SURYO (SPA-GMN-IDX)
                                       TO  SPA-COM-SURYO2(SPA-GMN-IDX)
      *                回数変更
                       MOVE    SPA-COM-KAISU (WRK-GMN-IDX)
                                               TO  SPA-COM-KAISU
                                                         (SPA-GMN-IDX)
                       MOVE    SPA-COM-KAISU (WRK-GMN-IDX)
                                               TO  SPA-COM-KAISU2
                                                         (SPA-GMN-IDX)
                   END-IF
      *
      *            セット内に約束セットがある時
                   IF       SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)  =   "S"
                       INITIALIZE                  WRK-MOJI-AREA
                       MOVE    SPA-GMN-IDX         TO  IDX
      *                入力コードの画面用再編集
                       PERFORM 3101-INPUTCD-HEN-SEC
      *                約束名称編集
                       MOVE    6                   TO  WRK-CD-MCNT
                       MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:6)
                                                   TO  WRK-INPUTCD
                       MOVE    "6"                 TO  LNK-SC97-CDSYU
                       PERFORM 41011921-INPSET-MEI-SEC
                       MOVE    SPACE               TO  SPA-COM-INPUTCD
                                                          (SPA-GMN-IDX)
                       MOVE    1                   TO  FLG-YAKUSOKU
                       ADD     1                   TO  IDX-YAKUSOKU
                       IF      IDX-YAKUSOKU        >   20
                           MOVE    "0007"          TO  SPA-ERRCD
                       ELSE
                           MOVE    SPA-GMN-IDX     TO  WRK-IDX-YAK
                                                        (IDX-YAKUSOKU)
                       END-IF
                   ELSE
      *
      *            点数マスタ編集・基本チェック
                   INITIALIZE                  ORCSC92AREA
                   MOVE    "K02"               TO  LNK-SC92-PG
                   MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
                   MOVE    SPACE               TO  LNK-SC92-KBN
                   MOVE    SPA-GMN-IDX         TO  LNK-SC92-GMN-IDX
                   MOVE    SPA-COM-INPUTCD(SPA-GMN-IDX)
                                               TO  LNK-SC92-SRYCD
                   MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
                   MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
                   MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
                   MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
                   MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
                   MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
      *            当月入院有無
                   MOVE    SPA-K02N-NYUINFLG   TO  LNK-SC92-NYUINFLG
                   CALL    "ORCSC92"           USING
                                               MCPAREA
                                               ORCSC92AREA
                                               SPA-SCR-REC
                                               SPA-AREA
      *            点数マスタなしのエラーは表示する
                   IF      SPA-ERRCD           =   "0001"
                       IF      SPA-COM-SET (SPA-GMN-IDX)   =  "1"
                           MOVE    "S001"          TO  WRK-ERRCD
                       END-IF
                       MOVE    SPACE           TO  SPA-ERRCD
                       MOVE    "1"             TO  SPA-COM-CUR
                                                       (SPA-GMN-IDX)
                   END-IF
      *
      *           エラー時、追加しない
                  IF     (SPA-ERRCD           NOT =   SPACE )  OR
                         (LNK-SC92-ERRAREA    NOT =   SPACE )
                      MOVE    SPACE           TO  SPA-ERRCD
                      INITIALIZE              SPA-GMN-TBLREC
                                                          (SPA-GMN-IDX)
                      INITIALIZE              SPA-COM-TBLREC
                                                          (SPA-GMN-IDX)
                      COMPUTE SPA-GMN-IDX     =   SPA-GMN-IDX
                                              -   1
                   ELSE
      *                保険外金額判定
                       PERFORM 4101191-JIHI-HEN-SEC
      *                入力コードの画面用再編集
                       MOVE    SPA-GMN-IDX         TO  IDX
                       PERFORM 3101-INPUTCD-HEN-SEC
                   END-IF
      *
                   END-IF
      *
               END-IF
      *
               IF      SPA-COM-KAIFLG (SPA-GMN-IDX)    =   "1"
                   MOVE    SPACE               TO  WRK-SRYKBN
               END-IF
      *
               IF      SPA-ERRCD           =   SPACE
                   MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                           TO  SPA-MAE-INPUTCD
                                                          (SPA-GMN-IDX)
               END-IF
      *
               MOVE    "INPUTSET-KEY2"     TO  ORC-DBPATH
               PERFORM 975-INPUTSET-READ-SEC
      *
               IF      (FLG-INPUTSET     =   ZERO  )  AND
                       (SPA-ERRCD        =   SPACE )
      *
      *            行を挿入して追加する
                   ADD     1               TO  SPA-GMN-IDX
                   PERFORM 41014-GYOINSN-SEC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "INPUTSET-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
          IF      (SPA-ERRCD       NOT =   SPACE )  AND
                  (FLG-INPUTSET        =   ZERO  )
               MOVE    WRK-SCR-REC        TO  SPA-SCR-REC
               MOVE    WRK-GMN-IDX        TO  SPA-GMN-IDX
               MOVE    ZERO               TO  IDX-IP
               GO      TO    4101191-INPUTSET-SYORI-EXT
           END-IF
           IF      SPA-GMN-IDX         >   200
               MOVE    200                 TO  SPA-GMN-IDX
           END-IF
           MOVE    SPACE               TO  SPA-ERRCD
      *
      *    対象なし
           IF      IDX-IP              =   ZERO
               MOVE    "0050"              TO  SPA-ERRCD
               GO      TO    4101191-INPUTSET-SYORI-EXT
           END-IF
      *    最終行を回数入力とする
           IF      SPA-GMN-INPUTCD(WRK-GMN-IDX)(1:1)   =   "S"
               MOVE    "1"             TO  SPA-COM-KAIFLG (SPA-GMN-IDX)
      *        セットの時前に編集
               IF      WRK-ERRCD       =   SPACE
                   MOVE    SPA-GMN-INPUTCD(WRK-GMN-IDX)
                                       TO  SPA-MAE-INPUTCD(WRK-GMN-IDX)
               ELSE
                   MOVE    WRK-ERRCD   TO  SPA-ERRCD
                   MOVE    "1"         TO  SPA-COM-CUR (WRK-GMN-IDX)
               END-IF
           END-IF
      *
           .
       4101191-INPUTSET-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    約束セット展開処理
      *****************************************************************
       4101191-INPUTSET-SYORI2-SEC     SECTION.
      *
      *    約束の時、１行前に診療種別があること
           IF     (SPA-GMN-IDX         =   1   )   OR
                  (SPA-GMN-INPUTCD(SPA-GMN-IDX - 1)(1:1) NOT =   "." )
               MOVE    "0048"              TO  SPA-ERRCD
               MOVE    "1"                 TO  SPA-COM-CUR(SPA-GMN-IDX)
               GO      TO      4101191-INPUTSET-SYORI2-EXT
           END-IF
      *
      *    行オーバーの為、ＳＰＡを保存する
           MOVE    SPA-SCR-REC         TO  WRK-SCR-REC
           MOVE    SPA-GMN-IDX         TO  WRK-GMN-IDX
      *
           MOVE    ZERO                TO  IDX-IP
           MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:6)
                                       TO  WRK-SETCD
      *    入力セットマスタ検索
           MOVE    SPACE               TO  INPUTSET-REC
           INITIALIZE                      INPUTSET-REC
           MOVE    SPA-HOSPID          TO  ISET-HOSPID
           MOVE    WRK-SETCD           TO  ISET-SETCD
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "INPUTSET-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "INPUTSET-KEY2"     TO  ORC-DBPATH
               PERFORM 975-INPUTSET-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
      *
           PERFORM
                   UNTIL     (FLG-INPUTSET     =   1 )  OR
                             (SPA-ERRCD    NOT =   SPACE )
      *
      *        行を挿入して追加する
               ADD     1               TO  SPA-GMN-IDX
               PERFORM 41014-GYOINSN-SEC
               IF      SPA-ERRCD       =   SPACE
      *
               ADD     1               TO  IDX-IP
      *
               MOVE    ISET-INPUTCD    TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               IF      ISET-SURYO1     =   ZERO
                   MOVE    1           TO  SPA-COM-SURYO (SPA-GMN-IDX)
               ELSE
                   MOVE    ISET-SURYO1 TO  SPA-COM-SURYO (SPA-GMN-IDX)
               END-IF
               MOVE    SPA-COM-SURYO (WRK-GMN-IDX)
                                       TO  SPA-COM-SURYO2(SPA-GMN-IDX)
               IF      ISET-SURYO2     =   ZERO
                   MOVE    1           TO  SPA-COM-BUNGSU(SPA-GMN-IDX)
               ELSE
                   MOVE    ISET-SURYO2 TO  SPA-COM-BUNGSU(SPA-GMN-IDX)
               END-IF
               IF      ISET-KAISU      =   ZERO
                   MOVE    1           TO  SPA-COM-KAISU  (SPA-GMN-IDX)
               ELSE
                   MOVE    ISET-KAISU  TO  SPA-COM-KAISU  (SPA-GMN-IDX)
                   MOVE    "1"         TO  SPA-COM-KAIFLG (SPA-GMN-IDX)
               END-IF
               MOVE    SPA-COM-KAISU (SPA-GMN-IDX)
                                       TO  SPA-COM-KAISU2(SPA-GMN-IDX)
      *
               IF      ISET-INPUTCD        =   "810000001"
                   MOVE    ISET-COMMENT    TO  SPA-GMN-MEISYO-G
                                                          (SPA-GMN-IDX)
                   MOVE    "1"             TO  SPA-COM-MEIFLG
                                                          (SPA-GMN-IDX)
               ELSE
                   MOVE    ISET-COMMENT    TO  SPA-GMN-MEISYO
                                                          (SPA-GMN-IDX)
                   IF      ISET-INPUTCD(1:2)   =   "83" 
                       MOVE    "1"         TO  SPA-COM-MEIFLG
                                                          (SPA-GMN-IDX)
                   END-IF
               END-IF
      *
               MOVE    ISET-ATAI (01)  TO  SPA-COM-ATAI1 (SPA-GMN-IDX)
               MOVE    ISET-ATAI (02)  TO  SPA-COM-ATAI2 (SPA-GMN-IDX)
               MOVE    ISET-ATAI (03)  TO  SPA-COM-ATAI3 (SPA-GMN-IDX)
               MOVE    ISET-ATAI (04)  TO  SPA-COM-ATAI4 (SPA-GMN-IDX)
      *
      *        約束処方の時、画面表示なし
               IF      SPA-GMN-INPUTCD(WRK-GMN-IDX)(1:1)   =   "S"
      *            画面表示なし
                   MOVE    "1"             TO  SPA-COM-SET(SPA-GMN-IDX)
               END-IF
      *
               IF       SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)  =   "." 
      *            診療種別区分名称チェック
                   MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:3)
                                                 TO  WRK-SRYSYUKBN
                   PERFORM 510-SRYKBN-MEI-SEC
               ELSE
                   MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:9)
                                               TO  SPA-COM-INPUTCD
                                                          (SPA-GMN-IDX)
      *
      *            約束処方の時、数量を計算する
                   IF      SPA-GMN-INPUTCD(WRK-GMN-IDX)(1:1)   =   "S"
      *
      *                数量変更
                       COMPUTE SPA-COM-SURYO (SPA-GMN-IDX)
                                               =   SPA-COM-SURYO
                                                         (SPA-GMN-IDX)
                                               *   SPA-COM-SURYO
                                                         (WRK-GMN-IDX)
                       MOVE    SPA-COM-SURYO (WRK-GMN-IDX)
                                               TO  SPA-COM-SURYO2
                                                         (SPA-GMN-IDX)
      *                回数変更
                       MOVE    SPA-COM-KAISU (WRK-GMN-IDX)
                                               TO  SPA-COM-KAISU
                                                         (SPA-GMN-IDX)
                       MOVE    SPA-COM-KAISU (WRK-GMN-IDX)
                                               TO  SPA-COM-KAISU2
                                                         (SPA-GMN-IDX)
                   END-IF
      *
      *            点数マスタ編集・基本チェック
                   INITIALIZE                  ORCSC92AREA
                   MOVE    SPACE               TO  LNK-SC92-KBN
                   MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
                   MOVE    "K02"               TO  LNK-SC92-PG
                   MOVE    SPA-GMN-IDX         TO  LNK-SC92-GMN-IDX
                   MOVE    SPA-COM-INPUTCD(SPA-GMN-IDX)
                                               TO  LNK-SC92-SRYCD
                   MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
                   MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
                   MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
                   MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
                   MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
                   MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
      *            当月入院有無
                   MOVE    SPA-K02N-NYUINFLG   TO  LNK-SC92-NYUINFLG
                   CALL    "ORCSC92"           USING
                                               MCPAREA
                                               ORCSC92AREA
                                               SPA-SCR-REC
                                               SPA-AREA
      *            点数マスタなしのエラーは表示する
                   IF      SPA-ERRCD           =   "0001"
                       IF      SPA-COM-SET (SPA-GMN-IDX)   =  "1"
                           MOVE    "S001"          TO  WRK-ERRCD
                       END-IF
                       MOVE    SPACE           TO  SPA-ERRCD
                   END-IF
      *
      *           エラー時、追加しない
                  IF     (SPA-ERRCD           NOT =   SPACE )  OR
                         (LNK-SC92-ERRAREA    NOT =   SPACE )
                      MOVE    SPACE           TO  SPA-ERRCD
                      INITIALIZE              SPA-GMN-TBLREC
                                                     (SPA-GMN-IDX)
                      INITIALIZE              SPA-COM-TBLREC
                                                     (SPA-GMN-IDX)
                      COMPUTE SPA-GMN-IDX     =   SPA-GMN-IDX
                                              -   1
                   ELSE
                       MOVE    SPACE          TO  SPA-ERRCD
      *                保険外金額判定
                       PERFORM 4101191-JIHI-HEN-SEC
      *                入力コードの画面用再編集
                       MOVE    SPA-GMN-IDX         TO  IDX
                       PERFORM 3101-INPUTCD-HEN-SEC
                   END-IF
               END-IF
      *
               IF      SPA-COM-KAIFLG (SPA-GMN-IDX)    =   "1"
                   MOVE    SPACE               TO  WRK-SRYKBN
               END-IF
      *
               MOVE    "INPUTSET-KEY2"     TO  ORC-DBPATH
               PERFORM 975-INPUTSET-READ-SEC
      *
               END-IF
      *
           END-PERFORM
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "INPUTSET-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
          IF      (SPA-ERRCD       NOT =   SPACE )  AND
                  (FLG-INPUTSET        =   ZERO  )
               MOVE    WRK-SCR-REC        TO  SPA-SCR-REC
               MOVE    WRK-GMN-IDX        TO  SPA-GMN-IDX
               MOVE    ZERO               TO  IDX-IP
               GO      TO    4101191-INPUTSET-SYORI2-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-ERRCD
      *
      *    最終行を回数入力とする
           MOVE    "1"                 TO  SPA-COM-KAIFLG (SPA-GMN-IDX)
      *
           .
       4101191-INPUTSET-SYORI2-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険外金額判定処理
      *****************************************************************
       4101191-JIHI-HEN-SEC           SECTION.
      *
      *    自賠責の時、対象外
           IF     (SPA-HKNKBN          =   1    )  AND
                  (SPA-RSI-HKNKBN      =   "4"  )  AND
                  (SPA-COM-SRYKBN (SPA-GMN-IDX)
                                       =   "80"  )  AND
                  (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:5)
                                       =   "09591"  OR
                                           "09592"  OR
                                           "09593"  )
               GO     TO      4101191-JIHI-HEN-EXT
           END-IF
      *
           IF      SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)  =   "095"  OR
                                                          "096"
               IF      SPA-COM-KISOTEN (SPA-GMN-IDX)  =   ZERO
                   MOVE    "1"                 TO  SPA-COM-KEIFLG
                                                         (SPA-GMN-IDX)
                   MOVE    ISET-SURYO1         TO  SPA-COM-KEI
                                                         (SPA-GMN-IDX)
                   MOVE    ISET-SURYO1         TO  SPA-COM-JIHIMONEY
                                                         (SPA-GMN-IDX)
                   MOVE    ISET-SURYO1         TO  SPA-GMN-KEI
                                                         (SPA-GMN-IDX)
                   MOVE    1                   TO  SPA-COM-SURYO
                                                         (SPA-GMN-IDX)
               ELSE
                   MOVE    "2"                 TO  SPA-COM-KEIFLG
                                                         (SPA-GMN-IDX)
      *******      MOVE    SPA-COM-KISOTEN (SPA-GMN-IDX)
      *                                        TO  SPA-COM-KEI
      *                                                  (SPA-GMN-IDX)
      *            MOVE    SPA-COM-KISOTEN (SPA-GMN-IDX)
      *                                        TO  SPA-GMN-KEI
      *********                                          (SPA-GMN-IDX)
               END-IF
      ******** MOVE    1                   TO  SPA-COM-SURYO
      ********                                          (SPA-GMN-IDX)
           ELSE
               IF      WRK-SRYKBN          =   "95"  OR  "96"
                   MOVE    "2"                 TO  SPA-COM-KEIFLG
                                                         (SPA-GMN-IDX)
      **********   MOVE    ISET-SURYO1         TO  SPA-COM-KEI
      *                                                  (SPA-GMN-IDX)
      *            MOVE    ISET-SURYO1         TO  SPA-GMN-KEI
      *                                                  (SPA-GMN-IDX)
      ********     MOVE    1                   TO  SPA-COM-SURYO
      ********                                           (SPA-GMN-IDX)
               END-IF
           END-IF
      *
           .
       4101191-JIHI-HEN-EXT.
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
      *    約束名称編集
           PERFORM 41011921-INPSET-MEI-SEC
      *
           .
       4101192-INPSET-SET-EXT.
           EXIT.
      *****************************************************************
      *    約束処方の処理
      *****************************************************************
       41011921-INPSET-MEI-SEC           SECTION.
      *
           MOVE    SPACE               TO  INPUTCD-REC
           MOVE    SPA-HOSPID          TO  ICD-HOSPID
           MOVE    "6"                 TO  LNK-SC97-CDSYU
           MOVE    LNK-SC97-CDSYU      TO  ICD-CDSYU
      *
           MOVE    WRK-INPUTCD         TO  ICD-INPUTCD
      *
           PERFORM 930-INPUTCD-READ-SEC
      *
           IF      FLG-INPUTCD     NOT =   ZERO
               MOVE    "0050"          TO  SPA-ERRCD
               GO  TO                  41011921-INPSET-MEI-EXT
           END-IF
           MOVE    ICD-DSPNAME         TO  SPA-GMN-MEISYO(SPA-GMN-IDX)
      *    回数入力としない
           MOVE    SPACE               TO  SPA-COM-KAIFLG (SPA-GMN-IDX)
      *
           .
       41011921-INPSET-MEI-EXT.
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
           IF      SPA-ERRCD       NOT =   SPACE
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
      *
      *    算定回数チェック（各基本チェックを行う時、ここで）
           IF      FLG-TOROKU          =   "1"
               PERFORM VARYING SPA-GMN-IDX  FROM    1   BY  1
                       UNTIL  (SPA-GMN-IDX     >   200      )   OR
                              (SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                               =   SPACE )  OR
                              (SPA-ERRCD   NOT =   SPACE )
                   IF      SPA-COM-SANTEIRRKKBN(SPA-GMN-IDX)
                                               NOT =   ZERO
                       MOVE    "3"             TO  WRK-SANTEI-SYORI
                       PERFORM 41011231-SANTEI-CHK-SEC
                       IF      SPA-ERRCD(1:1)      =   "K"
                           IF      SPA-COM-KZMFLG (SPA-GMN-IDX)
                                                       =   SPACE
                                MOVE    "1"        TO  SPA-COM-KZMFLG
                                                         (SPA-GMN-IDX)
                                MOVE    "2"        TO  SPA-COM-CUR
                                                         (SPA-GMN-IDX)
                           ELSE
                               MOVE    SPACE           TO  SPA-ERRCD
                               MOVE    SPACE           TO  SPA-COM-CUR
                                                         (SPA-GMN-IDX)
                          END-IF
                       END-IF
                       IF      SPA-ERRCD       NOT =   SPACE
                           MOVE    "1"            TO  SPA-COM-CUR
                                                          (SPA-GMN-IDX)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
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
           MOVE    "K02"               TO  LNK-SC93-PG
      *    小児科外来診療科算定有り
           MOVE    SPA-SYONIKA-ARI2    TO  LNK-SC93-SYONIKA-ARI2
      *    寝たきり在宅科算定有り
           IF      (SPA-NETAKIRI-ARI   NOT =   ZERO )  OR
                   (SPA-NETAKIRI-ARI2  NOT =   ZERO )
               MOVE    1                   TO  LNK-SC93-NETAKIRI-ARI
           END-IF
           CALL    "ORCSC93"           USING
                                       MCPAREA
                                       SPA-SCR-REC
                                       SPA-AREA
                                       ORCSC93AREA
                                       MSYU-DATA-REC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      5101-TBLSAIHEN-EXT
           END-IF
      *
      *    特定診療チェック
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSC10SAREA
           MOVE    "7"                 TO  LNK-SC10S-SYORI
           CALL    "ORCSC10S"          USING
                                       ORCSC10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      5101-TBLSAIHEN-EXT
           END-IF
      *
      *    剤ごとに回数を入れ直す（リハビリ等同時回数チェックのため）
           MOVE    ZERO                TO  WRK-KAISU
           PERFORM VARYING     IDX     FROM    200 BY  -1
                   UNTIL       IDX     <   1
               IF     (SPA-GMN-INPUTCD (IDX)   =   SPACE )  AND
                      (SPA-COM-INPUTCD (IDX)   =   SPACE )
                   CONTINUE
               ELSE
                   IF     (WRK-KAISU               =   ZERO   )  OR
                          (SPA-COM-ZAIEND (IDX)    =   "1"    )
                       MOVE    SPA-COM-KAISU (IDX) TO  WRK-KAISU
      *                外用薬の時、入力回数を
                       IF     (SPA-COM-KAISU2(IDX) >   ZERO ) AND
                              (SPA-COM-KAISU (IDX) =   1    )
                           MOVE    SPA-COM-KAISU2 (IDX) TO  WRK-KAISU
                       END-IF
                       IF      WRK-KAISU      =   ZERO
                           MOVE    1              TO  WRK-KAISU
                       END-IF
                   END-IF
                   MOVE    WRK-KAISU           TO  SPA-COM-KAISU (IDX)
               END-IF
           END-PERFORM
      *
      *    剤ごとの処理
      *    診療種別件数クリア
           INITIALIZE                  SPA-SRYKBN-AREA
           MOVE    ZERO                TO  IDX-Z
           MOVE    ZERO                TO  WRK-KAISU
           INITIALIZE                  WRK-SCR-REC
           INITIALIZE                  WRK-HOZ-AREA
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
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   200      )   OR
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
                   IF       SPA-COM-INPUTCD (IDX)(1:2)   =  "84"
                        MOVE    ZERO            TO  FLG-TAISYO
                   END-IF
      *            自動追加で画像診断の時対象外
                   IF      (SPA-COM-SRYKBN(IDX) NOT =  "95" AND
                                                       "96" ) AND
                           (SPA-COM-INPUTCD  (IDX) =  "170012070"
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
      *
      *            自費以外の計をクリアする
                   IF      WRK-COM-SRYKBN (IDX-Z)   =   "95"  OR "96"
                       CONTINUE
                   ELSE
                       MOVE    ZERO            TO  WRK-COM-KEI(IDX-Z)
                       MOVE    ZERO            TO  WRK-COM-JIHIMONEY
                                                               (IDX-Z)
                    END-IF
      *
               END-IF
      *        回数編集
               IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "."
                   CONTINUE
               ELSE
                   IF      SPA-COM-KAIFLG (IDX)    =   "1"
                        MOVE    SPA-COM-KAISU (IDX) TO  WRK-KAISU
      *                 外用薬の時、入力回数を
                        IF     (SPA-COM-KAISU2(IDX) >   ZERO ) AND
                               (SPA-COM-KAISU (IDX) =   1    )
                            MOVE    SPA-COM-KAISU2 (IDX) TO  WRK-KAISU
                        END-IF
                   END-IF
               END-IF
      *
               IF     (IDX                 =   SPA-GMN-MAX   )  OR
                      (SPA-COM-ZAIEND (IDX)
                                           =   "1"           )
      *            最終行に回数の入力があるかのチェック
      *************PERFORM 51011-KAISU-CHK-SEC
      *            剤ごとの処理
                   PERFORM 5102-ZAIBETU-SEC
                   INITIALIZE                  WRK-SCR-REC
                   MOVE    ZERO                TO  IDX-Z
                   MOVE    ZERO                TO  WRK-KAISU
               END-IF
      *        エラーコードを保存する
               IF      SPA-ERRCD       NOT =   SPACE
                   IF      SPA-ERRCD(1:1)      =   "K"
                       MOVE    SPA-ERRCD       TO  WRK-KEI-ERRCD
                   ELSE
                       IF      WRK-ERR-ERRCD       =   SPACE
                           MOVE    SPA-ERRCD       TO  WRK-ERR-ERRCD
                       END-IF
                   END-IF
                   MOVE    SPACE           TO  SPA-ERRCD
               END-IF
           END-PERFORM
      *
      *    エラーの再セットを行う
           IF      WRK-ERR-ERRCD   NOT =   SPACE
               MOVE    WRK-ERR-ERRCD       TO  SPA-ERRCD
           END-IF
      *    警告メッセージの時、ＳＰＡの再セットを行う
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (WRK-KEI-ERRCD   NOT =   SPACE)
               MOVE    WRK-KEI-ERRCD       TO  SPA-ERRCD
           END-IF
      *
      *    スパ領域へ戻す
           MOVE    ZERO                TO  SPA-GMN-MAX
           INITIALIZE                  SPA-GMN-REC
           PERFORM VARYING     IDX     FROM    1   BY  1
      *************UNTIL       IDX     >   WRK-HOZ-IDX
                   UNTIL       IDX     >   200
               IF      WRK-HGMN-TBLREC (IDX)   NOT =   SPACE
                   ADD     1                   TO  SPA-GMN-MAX
                   MOVE    WRK-HGMN-TBLREC (IDX)   TO  SPA-GMN-TBLREC
                                                         (SPA-GMN-MAX)
                   MOVE    WRK-HCOM-TBLREC (IDX)   TO  SPA-COM-TBLREC
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
      *
           .
       51011-KAISU-CHK-EXT.
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
           INITIALIZE                  ORCSC00AREA
           MOVE    "1"                 TO  LNK-ORCSC-KBN
           MOVE    "K02"               TO  LNK-ORCSC-PG
           MOVE    WRK-KAISU           TO  LNK-ORCSC-KAISU
      **** MOVE    SPA-GMN-IDX         TO  LNK-ORCSC-GMN-IDX
           MOVE    SPA-NAI-ROUJIN      TO  LNK-ORCSC-ROUJIN 
           MOVE    SPA-NENREI          TO  LNK-ORCSC-NENREI 
           MOVE    SPA-SYOYMD          TO  LNK-ORCSC-SYOYMD 
           MOVE    SPA-LSTYMD          TO  LNK-ORCSC-LSTYMD 
           MOVE    SPA-LSTSRYKA        TO  LNK-ORCSC-LSTSRYKA 
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-ORCSC-LSTSRYKAMEI 
           MOVE    SPA-INGAIKBN        TO  LNK-ORCSC-INGAIKBN
           MOVE    WRK-TIMEFLG         TO  LNK-ORCSC-TIMEFLG
           MOVE    SPA-NAI-GAIFLG      TO  LNK-ORCSC-GAIFLG
           MOVE    SPA-NAIACCT-AREA    TO  LNK-NAIACCT-AREA
           MOVE    SPA-NAIACCT2-AREA   TO  LNK-NAIACCT2-AREA
           MOVE    FLG-TOROKU          TO  LNK-ORCSC-TOROKU
      *    最終受診日（履歴）
           MOVE    SPA-RRK-LSTYMD      TO  LNK-ORCSC-RRK-LSTYMD
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
      *    外来診療料算定有無
           MOVE    SPA-GAIRAISIN       TO  LNK-ORCSC-GAIRAISIN
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
           MOVE    SPA-SYOEN-CNT       TO  LNK-ORCSC-SYOEN-CNT
      *    当月外来管理加算の取れない患者
           MOVE    SPA-GAIRAI-NASI     TO  LNK-ORCSC-GAIRAI-NASI
      *    再診チェック済
           MOVE    SPA-SAISIN-CHK      TO  LNK-ORCSC-SAISIN-CHK
      *    理学療法当月内算定
           MOVE    WRK-RIGAKU-TBL      TO  LNK-ORCSC-RIGAKU-TBL
      *    労災用 外来管理点数
           MOVE    SPA-GAIRAI-TEN      TO  LNK-ORCSC-GAIRAI-TEN
      *    労災用 救急医療管理加算有無
      **** MOVE    SPA-RSI-KYUKASN     TO  LNK-ORCSC-RSI-KYUKASN
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
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-ORCSC-TEISEI-AREA
      *    外来管理加算削除のチェック
           MOVE    SPA-GAIKANRIKBN-CHK TO  LNK-ORCSC-GAIKANRIKBN-CHK
      *    最終退院日
           MOVE    SPA-K02N-TAIINYMD   TO  LNK-ORCSC-TAIINYMD
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
      *            外来診療料算定有無
                   MOVE    LNK-ORCSC-GAIRAISIN TO  SPA-GAIRAISIN
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
                   MOVE    LNK-ORCSC-GAIRAI-TEN
                                           TO  SPA-GAIRAI-TEN
      *
      *            薬剤
               WHEN    "21"
               WHEN    "22"
               WHEN    "23"
               WHEN    "25"
      *
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
      *            理学療法当月内算定
                   MOVE    LNK-ORCSC-RIGAKU-TBL
                                           TO  WRK-RIGAKU-TBL
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
      *************PERFORM 510212-SRYKBN-95-SEC
      *            食事
               WHEN    "90"
               WHEN    "92"
               WHEN    "97"
                   MOVE    "9200"          TO  SPA-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (01)
           END-EVALUATE
      *
      *****MOVE    LNK-ORCSC-GMN-IDX   TO  SPA-GMN-IDX
           MOVE    LNK-ORCSC-CHK-IDX   TO  SPA-CHK-IDX
           MOVE    LNK-ORCSC-KIDCD     TO  SPA-KIDCD
      *
      *    編集後内容セット
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   200  )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
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
               IF     ( IDX-Z              =   200 )  OR
                      ((WRK-COM-INPUTCD(IDX-Z + 1) =   SPACE )  AND
                       (WRK-GMN-INPUTCD(IDX-Z + 1) =   SPACE ))
                   MOVE    "1"             TO  WRK-COM-ZAIEND(IDX-Z)
               ELSE
                   MOVE    SPACE           TO  WRK-COM-ZAIEND(IDX-Z)
               END-IF
      *        回数
               IF      LNK-ORCSC-KAISU NOT =   WRK-COM-KAISU (IDX-Z)
                   MOVE    LNK-ORCSC-KAISU     TO  WRK-COM-KAISU(IDX-Z)
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
      *
      *        IF     (WRK-COM-KEI    (IDX-Z)  NOT =   ZERO )  AND
      *               (WRK-COM-KEIFLG (IDX-Z)      =   SPACE)
      *        自費以外の点数を集計
               IF     (WRK-COM-ZAIEND (IDX-Z)      =   "1"   )  AND
                      (WRK-COM-SRYKBN (IDX-Z)  NOT =   "95"  AND "96")
                   ADD     WRK-COM-KEI    (IDX-Z)  TO  WRK-GOKEI-9
               END-IF
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
               CALL    "ORCSC94"           USING
                                           ORCSC94AREA
                                           WRK-SCR-REC
                                           SPA-AREA
      *
               ADD     1                   TO  WRK-HOZ-IDX
               MOVE    WRK-COM-TBLREC (IDX-Z)
                                           TO  WRK-HCOM-TBLREC
                                                       (WRK-HOZ-IDX)
               MOVE    WRK-GMN-TBLREC (IDX-Z)
                                           TO  WRK-HGMN-TBLREC
                                                       (WRK-HOZ-IDX)
      *
           END-PERFORM
           .
       5102-ZAIBETU-EXT.
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
                   UNTIL      (IDX-Z       >   200      )   OR
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
               GO      TO      510222-KNKYAK-CHK-EXT
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
               MOVE    200                 TO  IDX-K4
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
                   UNTIL      (IDX-Z       >   200  )  OR
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
                                                         "84")
               IF     (LNK-SC97-NYURYOKU   =   ZERO )  AND
                      (FLG-IN-ATAI         =   ZERO )  AND
                      (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)
                                           = "83"   )
                   MOVE    WRK-MEISYO-G        TO  SPA-GMN-MEISYO-G
                                                         (SPA-GMN-IDX)
               END-IF
               MOVE    "1"             TO  SPA-COM-MEIFLG
                                                       (SPA-GMN-IDX)
           END-IF
           IF     (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)  =  "81"
                                                       OR "83"
                                                       OR "84") AND
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
               MOVE    "0080"          TO  SPA-ERRCD
               MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
               MOVE    2               TO  SPA-GMN-CUR
               GO      TO      401131-MEISYO-HENKAN-EXT
           END-IF
           MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-MEISYO-G(SPA-GMN-IDX)
                                                            (IDX-STR:)
      *
           .
       401131-MEISYO-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為名入力処理
      *****************************************************************
      *41012-MEISYO-HEN-SEC        SECTION.
      *
      *    MOVE    2                   TO  SPA-GMN-CUR
      *
      *    IF      SPA-PTID            =   ZERO
      *        IF      K02-MEISYO(WRK-IDX2)   NOT =   SPACE
      *            MOVE    00              TO  SPA-GMN-CUR
      *            MOVE    "1005"          TO  SPA-ERRCD
      *            GO      TO      41012-MEISYO-HEN-EXT
      *         END-IF
      *    END-IF
      *
      *    MOVE    K02-MEISYO (WRK-IDX2)
      *                                TO  SPA-GMN-MEISYO-G
      *                                                (SPA-GMN-IDX)
      *
      *    半角の空白を全角に置きかえる
      *    フリーコメントは１桁目から
      *    IF      SPA-COM-INPUTCD(SPA-GMN-IDX)   =   "810000001"
      *        MOVE    1                   TO  IDX-STR
      *    ELSE
      *        MOVE    3                   TO  IDX-STR
      *    END-IF
      *    INITIALIZE                  ORCSKANACHKAREA
      *    MOVE    "1"                 TO  KANACHK-SYORI
      *    MOVE    SPA-GMN-MEISYO-G (SPA-GMN-IDX)(IDX-STR:)
      *                                TO  KANACHK-MAE-INPUT
      *    CALL    "ORCSKANACHK"       USING
      *                                ORCSKANACHKAREA
      *    IF     (KANACHK-RC      NOT =   ZERO  )  OR
      *           (KANACHK-SYUBETU     =   1     )
      *        MOVE    "0080"          TO  SPA-ERRCD
      *        MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
      *        GO      TO      41012-MEISYO-HEN-EXT
      *    END-IF
      *    MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-MEISYO-G(SPA-GMN-IDX)
      *                                                     (IDX-STR:)
      *
      *    MOVE    "1"                 TO  SPA-COM-MEIFLG
      *                                                (SPA-GMN-IDX)
      *
      *    MOVE    SPACE           TO  FLG-TOROKU
      *    PERFORM 510-TBLHEN-SEC
      *
      *    MOVE    1                   TO  SPA-GMN-CUR
      *
      *
      *    .
      *41012-MEISYO-HEN-EXT.
      *    EXIT.
      *
      *****************************************************************
      *    行挿入処理
      *****************************************************************
       41014-GYOINSN-SEC         SECTION.
      *
           IF      SPA-GMN-MAX         =   200
               MOVE    "0007"              TO  SPA-ERRCD
               GO      TO      41014-GYOINSN-EXT
           END-IF
      *
           IF      SPA-GMN-IDX         >=  200
               MOVE    "0007"              TO  SPA-ERRCD
               GO      TO      41014-GYOINSN-EXT
           END-IF
      *
           MOVE    SPA-GMN-REC         TO  WRK-GMN-REC
           INITIALIZE                      SPA-GMN-REC
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   200
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
                   UNTIL      (IDX     >   200 )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE)
               MOVE    SPACE           TO  SPA-GMN-INPUTCD(IDX)
               IF    ((IDX                     <   200   )  AND
                      (SPA-GMN-SRYKBN (IDX + 1)    NOT =   SPACE)) OR
                      (SPA-COM-ZAIEND(IDX)     =   "1"   )
                   MOVE    200             TO  IDX
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    SPA-GMN-IDX   BY  1
                   UNTIL      (IDX     >   200 )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE)
               MOVE    SPACE           TO  SPA-GMN-INPUTCD(IDX)
               IF     (IDX                 <   200 )  AND
                      (SPA-GMN-SRYKBN (IDX + 1)    NOT =   SPACE )
                   MOVE    200             TO  IDX
               END-IF
           END-PERFORM
      *
           .
       41015-ZAIDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット削除処理
      *****************************************************************
       41016-SETDEL-SEC              SECTION.
      *
           COMPUTE IDX2                =   SPA-GMN-IDX   +   1
      *
           PERFORM VARYING     IDX     FROM    IDX2    BY  1
                   UNTIL      (IDX     >   200 )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE)
               IF      SPA-COM-SET     (IDX)   =   "1"
                   MOVE    SPACE               TO  SPA-GMN-INPUTCD(IDX)
               ELSE
                   MOVE    200                 TO  IDX
               END-IF
           END-PERFORM
      *
           MOVE    SPA-GMN-REC         TO  WRK-GMN-REC
           INITIALIZE                      SPA-GMN-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
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
                   UNTIL       IDX     >   200
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
                   UNTIL       IDX     >   200
               IF      WRK-GMN-INPUTCD(IDX)    =   SPACE
                   CONTINUE
               ELSE
                   MOVE    ZERO                TO  FLG-ERR
      *            登録前チェックの時、診療種別のみの剤は削除する
                   IF      FLG-TOROKU          =   "1"
                       IF      WRK-GMN-INPUTCD (IDX)(1:1)  =  "."
                           IF      (IDX            =   200     )  OR
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
      *    最終受診日（履歴）
           MOVE    SPA-RRK-LSTYMD      TO  LNK-ORCSC-RRK-LSTYMD
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
      *外来診療料算定有無
           MOVE    SPA-GAIRAISIN       TO  LNK-ORCSC-GAIRAISIN
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
      *    当月外来管理加算の取れない患者
           MOVE    SPA-GAIRAI-NASI     TO  LNK-ORCSC-GAIRAI-NASI
      *    労災用 外来管理点数
           MOVE    SPA-GAIRAI-TEN      TO  LNK-ORCSC-GAIRAI-TEN
      *    理学療法当月内算定
           MOVE    WRK-RIGAKU-TBL      TO  LNK-ORCSC-RIGAKU-TBL
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
      *    外来管理加算削除のチェック
           MOVE    SPA-GAIKANRIKBN-CHK TO  LNK-ORCSC-GAIKANRIKBN-CHK
      *    最終退院日
           MOVE    SPA-K02N-TAIINYMD   TO  LNK-ORCSC-TAIINYMD
      *
      *            診療
           IF     (SPA-SRYKBN-CNT (01) >   ZERO )
             OR   (SPA-HKNKBN          =   1    )
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
               IF      SPA-GAIKANRIKBN-CHK     =   "1"  OR  "2"
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
               IF      LNK-ORCSC-KBN       =   "3"
                   INITIALIZE                  SPA-SYOHOU-ZUMI
                   MOVE    SPA-K01KYOUTU   TO  SPA-WORK
               END-IF
               CALL    "ORCSC20"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
               IF      LNK-ORCSC-KBN       =   "3"
                   MOVE    SPA-WORK        TO  SPA-K01KYOUTU
                   PERFORM 5201-SYOHOU-HKN-SEC
               END-IF
               MOVE    LNK-ORCSC-CHK-IDX   TO  SPA-CHK-IDX
               MOVE    LNK-ORCSC-KIDCD     TO  SPA-KIDCD
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            注射
           IF      SPA-SRYKBN-CNT (03) >   ZERO
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
      *TEST
               IF      LNK-ORCSC-KIDCD     NOT =   SPACE
                   MOVE    SPACE           TO  SPA-ERRCD
               END-IF
      *TEST
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
      *    他保険で算定済の保険編集処理
      *****************************************************************
       5201-SYOHOU-HKN-SEC              SECTION.
      *
           IF      SPA-SYOHO-HKN       =   SPACE
               MOVE    SPACE               TO  SPA-SYOHO-HKNMEI
           ELSE
               MOVE    SPACE               TO  SPA-SYOHO-HKNMEI
               PERFORM VARYING     IDX-HKN FROM    1   BY  1
                       UNTIL       IDX-HKN >   15
                   IF      SPA-SYOHO-HKN   =   SPA-GMN-THKNCOMBINUM
                                                              (IDX-HKN)
                       MOVE    SPA-GMN-THKNCOMBIMEI (IDX-HKN)
                                           TO  SPA-SYOHO-HKNMEI
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5201-SYOHOU-HKN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＯ選択処理
      *****************************************************************
       41014-DOSELNUM-SEC              SECTION.
      *
           MOVE    4                   TO  SPA-GMN-CUR
      *
           MOVE    K02-DOSELNUM        TO  SPA-GMN-DOSELNUM
           IF      K02-DOSELNUM        =   SPACE
               MOVE    ZERO            TO  SPA-NAI-DOSELNUM
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    K02-DOSELNUM        TO  SNUM-INX
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
              IF      K02-SELECT (IDX)        =  "T"
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
               MOVE    K02-NO  (SPA-NAI-DOSELNUM)
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
      *    診療科
           MOVE    SPA-NAI-RRK-SRYKA(SPA-NAI-DOSELNUM)
                                       TO  SPA-SRYKA
           MOVE    SPA-NAI-RRK-SRYKA(SPA-NAI-DOSELNUM)
                                       TO  SPA-OLD-SRYKA
      *    保険組合せ
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
           PERFORM 310-SPA-GMNSET-SEC
      *
      *    累計点数−訂正点数
           COMPUTE SPA-ZAITENTOTAL     =   SPA-ZAITENTOTAL
                                       -   LNK-SC95-GOK-TENSU
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
           MOVE    "K02"               TO  SPA-MOTOPG
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
           MOVE    SPA-TERMID          TO  FILE-TERMID1
           MOVE    SPA-TERMID          TO  FILE-TERMID2
      *
      ***  OPEN    OUTPUT              PARA-FILE
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
      *****MOVE    "K01"               TO  SPA-SAKIPG
           MOVE    "M01"               TO  SPA-SAKIPG
           MOVE    "K02"               TO  SPA-MOTOPG
           MOVE    SPACE               TO  SPA-MOTOPG2
      *
      *****MOVE    "CHANGE"            TO  MCP-PUTTYPE
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
           MOVE    "1"                 TO  LNK-SC95-KBN
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
       222-PTNUMCLEAR-EXT.
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
      *
           INITIALIZE              SPA-SCR-REC
      *
      *    INITIALIZE              SPA-K02-AREA
      *
      *    チェック用月内診療行為セット
      *    PERFORM                 320-SPA-CHKSET-SEC
      *
      *    画面用スパ編集処理
      *    PERFORM                 310-SPA-SET-SEC
      *
           MOVE    ZERO            TO  SPA-GMN-GOKEI
      *
           MOVE    1               TO  SPA-GMN-CUR
           MOVE    1               TO  SPA-GMN-PAGE
      *
           .
       220-CLEAR-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    院外院内ボタン処理
      *****************************************************************
       4101-INGAI-SYORI-SEC             SECTION.
      *
           EVALUATE    SPA-GMN-INGAI
      *        院外へ
               WHEN    "T"
                   MOVE    "F"             TO  SPA-GMN-INGAI
                   MOVE    "0"             TO  SPA-INGAIKBN
      *        院内 
               WHEN    "F"
                   MOVE    "T"             TO  SPA-GMN-INGAI
                   MOVE    "1"             TO  SPA-INGAIKBN
           END-EVALUATE
      *
      *    コード振替え
           PERFORM 4101-INGAI-HENKOU-SEC
      *
      *    診療行為、計計算処理、編集
           MOVE    "2"             TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE           TO  FLG-TOROKU
      *
           .
       4101-INGAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    小児科外来診察料、老人外総診断の算定変換処理
      *****************************************************************
       4101-INGAI-HENKOU-SEC             SECTION.
      *
      *    院外院内振替え処理
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSC10SAREA
           MOVE    "4"                 TO  LNK-SC10S-SYORI
           CALL    "ORCSC10S"          USING
                                       ORCSC10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           .
       4101-INGAI-HENKOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    処方箋発行処理
      *****************************************************************
       4124-SYOHOU-SYORI-SEC             SECTION.
      *
      *    患者の確定されてない時、処理をしない
           IF     (SPA-GMN-PTNUM       =   SPACE )  OR
                  (SPA-PTID            =   ZERO  )
               GO      TO      4124-SYOHOU-SYORI-EXT
           END-IF
      *
      *    帳票プログラム取得
      *
      *    処方せん
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000002"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1032-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               MOVE    "1030"              TO  SPA-ERRCD
               GO      TO      4124-SYOHOU-SYORI-EXT
           END-IF
           MOVE    ORCSPRTNM-PRTPG     TO  WRK-SYOHOU-PG
      *
      *****MOVE    SPACE               TO  WRK-SYOHOU-PG
      *    MOVE    SPACE               TO  SYS-1032-REC
      *    INITIALIZE                      SYS-1032-REC
      *    MOVE    "1032"              TO  SYS-1032-KANRICD
      *    MOVE    "00000002"          TO  SYS-1032-KBNCD
      *
      *    MOVE    SYS-1032-REC        TO  MCPDATA-REC
      *    MOVE    "DBSELECT"          TO  MCP-FUNC
      *    MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *    MOVE    SPA-SRYYMD          TO  ORC-DBYMD
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *        PERFORM 960-KANRIMST-READ-SEC
      *    ELSE
      *        INITIALIZE                  SYS-1032-REC
      *        MOVE    1               TO  FLG-SYSKANRI
      *    END-IF
      *    IF      FLG-SYSKANRI        =   ZERO
      *        MOVE    MCPDATA-REC         TO  SYS-1032-REC
      *        独自開発分
      *        IF      SYS-1032-ORCACSTCHK     =   "T"
      *            MOVE    SYS-1032-ORCACSTNM  TO  WRK-SYOHOU-PG
      *        ELSE
      *        ＯＲＣＡ標準分
      *            MOVE    SYS-1032-ORCAORGNM  TO  WRK-SYOHOU-PG
      *        END-IF
      *    END-IF
      *    IF      WRK-SYOHOU-PG       =   SPACE
      *        MOVE    "ORCHC19"           TO  WRK-SYOHOU-PG
      *****END-IF
      *
      *    処方箋印刷（Ａ５）
           INITIALIZE                  ORCHC19AREA
           EVALUATE    FLG-SYOHOU
               WHEN    1
      *            頭書
                   MOVE    "2"                 TO  ORCHC19-KBN
               WHEN    3
      *            中途終了時（院外）
                   MOVE    "4"                 TO  ORCHC19-KBN
               WHEN    4
      *            中途終了時（院内）
                   MOVE    "5"                 TO  ORCHC19-KBN
               WHEN    OTHER
      *            前回処方
                   MOVE    "1"                 TO  ORCHC19-KBN
           END-EVALUATE
      *
      *****CALL    "ORCHC19"           USING   SPA-AREA
           CALL    WRK-SYOHOU-PG       USING   SPA-AREA
                                               ORCHC19AREA
      *
           .
       4124-SYOHOU-SYORI-EXT.
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
           CLOSE                       PARA-FILE
      *
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "K05"               TO  SPA-SAKIPG
           MOVE    "K02"               TO  SPA-MOTOPG
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
               MOVE    "1006"          TO  SPA-ERRCD
               MOVE    00                  TO  SPA-GMN-CUR
               GO      TO      430-SANTEIRRK-EXT
           END-IF
      *
      *    診療行為入力チェック
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      430-SANTEIRRK-EXT
           END-IF
      *
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
           CLOSE                       PARA-FILE
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "K06"               TO  SPA-SAKIPG
           MOVE    "K02"               TO  SPA-MOTOPG
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
           IF      SPA-ERRCD       NOT =   SPACE
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
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
      *    診療行為一覧へ
           MOVE    "K023"              TO  SPA-SAKIPG
           MOVE    "K02"               TO  SPA-MOTOPG
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
      *    更新前の基本チェック
           PERFORM 41001-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      440-BYOTOROKU-EXT
           END-IF
      *
      *    更新前のチェック
           MOVE    "1"                 TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE               TO  FLG-TOROKU
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      440-BYOTOROKU-EXT
           END-IF
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
           PERFORM 4401-C02SET-SEC
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
           CLOSE                       PARA-FILE
      *
           MOVE    "C02"               TO  SPA-SAKIPG
           MOVE    "K02"               TO  SPA-MOTOPG
           MOVE    "K02"               TO  SPA-MOTOPG2
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
                   PERFORM 41011-KOUI-ALLHEN-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                      GO      TO      440-KNJADD-EXT
                   END-IF
      *        更新前のチェック
                   MOVE    "1"                 TO  FLG-TOROKU
                   PERFORM 510-TBLHEN-SEC
                   MOVE    SPACE               TO  FLG-TOROKU
                   IF      SPA-ERRCD       NOT =   SPACE
                      GO      TO      440-KNJADD-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
           CLOSE                       PARA-FILE
      *
           MOVE    "K02"               TO  SPA-MOTOPG
           MOVE    "K02"               TO  SPA-MOTOPG2
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
      *    Ｋ０２Ｎ処理へ
      *****************************************************************
       450-K02N-SYORI-SEC                   SECTION.
      *
           MOVE    "K02N"              TO  SPA-SAKIPG
           MOVE    SPA-MACHIPG         TO  SPA-MOTOPG
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
           MOVE    "K02N"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       450-K02N-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会計カードへ
      *****************************************************************
       450-KAIKEI-SEC             SECTION.
      *
      *
           IF      SPA-GMN-PTID    NOT =   ZERO
      *
      *        更新前の基本チェック
               PERFORM 41001-KIHON-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      450-KAIKEI-EXT
               END-IF
      *
      *        更新前のチェック
               MOVE    "1"                 TO  FLG-TOROKU
               PERFORM 510-TBLHEN-SEC
               MOVE    SPACE               TO  FLG-TOROKU
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      450-KAIKEI-EXT
               END-IF
      *
           END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
           CLOSE                       PARA-FILE
      *
           MOVE    "K02"               TO  SPA-MOTOPG
           MOVE    "K02"               TO  SPA-MOTOPG2
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
               PERFORM 41001-KIHON-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      460-SYUNOU-EXT
               END-IF
      *
      *        更新前のチェック
               MOVE    "1"                 TO  FLG-TOROKU
               PERFORM 510-TBLHEN-SEC
               MOVE    SPACE               TO  FLG-TOROKU
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      460-SYUNOU-EXT
               END-IF
           END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
           CLOSE                       PARA-FILE
      *
           MOVE    "K02"               TO  SPA-MOTOPG
           MOVE    "K02"               TO  SPA-MOTOPG2
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
      *   ワーク行為負担パラメタを作成　処理
      *****************************************************************
       4410-WSINPARA-SEC             SECTION.
      *
      *    ＳＰＡ保存パラメタを作成
           PERFORM 410021-PARA-SYORI-SEC
      *
           MOVE    ZERO                TO  IDX-P
           MOVE    "2"                 TO  FLG-SYORI
           IF      SPA-PTID            =   ZERO
               CONTINUE
           ELSE
               PERFORM 45101-SNRYO-SYORI-SEC
           END-IF
      *
           .
       4410-WSINPARA-EXT.
           EXIT.
      *
      *****************************************************************
      *    他画面移行用　診療行為パラメタ作成
      *****************************************************************
       44101-WKSINKPARA-OUT-SEC             SECTION.
      *
      *D    ADD     1                   TO  WRK-ZAINUM
            ADD     1                   TO  WRK-RENNUM
            MOVE    SPA-HOSPID          TO  WKSRY-HOSPID
            MOVE    SPA-PTID            TO  WKSRY-PTID
            MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
            MOVE    SPA-SRYKA           TO  WKSRY-SRYKA
            MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD
            MOVE    WRK-ZAINUM          TO  WKSRY-ZAINUM 
            MOVE    WRK-RENNUM          TO  WKSRY-RENNUM 
            MOVE    WRK-SRYSYUKBN       TO  WKSRY-SRYSYUKBN
            MOVE    WRK-SRYKBN          TO  WKSRY-SRYKBN
      ******MOVE    WRK-KINGAK          TO  WKSRY-JIHIMONEY
            MOVE    WRK-KINGAK          TO  WKSRY-JIHIMONEYTOTAL
      *
            MOVE    WRK-TENSUKEI        TO  WKSRY-ZAITENKEI
            MOVE    WRK-KAISUKEI        TO  WKSRY-ZAIKAIKEI
      *     各計
            MOVE    WRK-SYUTEN1         TO  WKSRY-SYUTEN1
            MOVE    WRK-SYUTEN2         TO  WKSRY-SYUTEN2
            MOVE    WRK-YKZTEN          TO  WKSRY-YKZTEN
            MOVE    WRK-KIZAITEN        TO  WKSRY-KIZAITEN
      *
            MOVE    SPACE               TO  PARA-REC
      ******MOVE    SPA-TERMID          TO  PARA-TERMID
            MOVE    "WKSRYACT"          TO  PARA-FILEMEI
            ADD     1                   TO  IDX-P
            MOVE    IDX-P               TO  PARA-EDANUM
      *
            MOVE    ALL "1"             TO  WKSRY-TERMID
      *
            MOVE    WKSRYACT-REC        TO  PARA-DATA-REC
            WRITE                       PARA-REC
      *
            MOVE    ZERO                TO  IDY
            MOVE    SPACE               TO  WKSRYACT-REC
            INITIALIZE                  WKSRYACT-REC
      *
           IF      SPA-COM-ZAIEND(IDX) =   "1"
               ADD     1                   TO  WRK-ZAINUM
               MOVE    ZERO                TO  WRK-RENNUM
               MOVE    ZERO                TO  WRK-TENSUKEI
               MOVE    ZERO                TO  WRK-KAISUKEI
               MOVE    ZERO                TO  WRK-KINGAK
               MOVE    ZERO                TO  WRK-MEINUM
               INITIALIZE                  WRK-WKSRY-AREA
           END-IF
           .
       44101-WKSINKPARA-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    途中終了　処理
      *****************************************************************
       4110-KYOSEIEND-SEC             SECTION.
      *
      *    初診算定日の自動算定時は、中途終了できない。
           IF     (SPA-SYOSIN-YMD      =   SPACE ) AND
                  (SPA-SYOSIN-SRYCD    =   SPACE )
               CONTINUE
           ELSE
               MOVE    "1023"          TO  SPA-ERRCD
               GO      TO      4110-KYOSEIEND-EXT
           END-IF
      *    訂正中の時、中途終了できない。
           IF      SPA-SYORIFLG        =   1
               MOVE    "1024"          TO  SPA-ERRCD
               GO      TO      4110-KYOSEIEND-EXT
           END-IF
      *
      *    更新前の基本チェック
           PERFORM 41001-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4110-KYOSEIEND-EXT
           END-IF
      *
      *    更新前のチェック
           MOVE    "1"                 TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE               TO  FLG-TOROKU
      *    外来管理加算確認はしない
           IF      SPA-KIDCD           =   "0102"   OR
                                           "0113"
               MOVE    SPACE           TO  SPA-KIDCD
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO      4110-KYOSEIEND-EXT
           END-IF
      *
      *    禁忌更新前のチェック
      *D   PERFORM 41001-YAKKNK-CHK-SEC
      *D   戻るの時、再出力へ
      *D   IF      SPA-K021-STS        =   1
      *D       GO      TO      4110-KYOSEIEND-EXT
      *D   END-IF
      *
      *    ワーク行為負担データを作成後、選択画面（Ｋ０１）へ
      **** PERFORM 4510-SNRYO-NYU-SEC
           PERFORM 45109-SNRYO-NYU-SEC
      *
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
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
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
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (FLG-END             =   1     )
               GO      TO      460-ATOGMN-EXT
           END-IF
      *
           IF      SPA-GMN-PAGE        =   10
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "7002"              TO  SPA-ERRCD
               GO      TO      460-ATOGMN-EXT
           END-IF
      *
           COMPUTE WRK-FREE            =   SPA-GMN-PAGE  *  20
      *****IF      SPA-GMN-MAX         <   WRK-FREE
           IF      SPA-GMN-INPUTCD(WRK-FREE)   =   SPACE
               GO  TO                 460-ATOGMN-EXT
           END-IF
      *
           ADD     1                   TO  SPA-GMN-PAGE
           IF      SPA-GMN-PAGE        =   10
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
               MOVE    00                  TO  SPA-GMN-CUR
               GO      TO      480-DOSYORI-EXT
           END-IF
      *
      *    診療行為入力チェック
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
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
           MOVE    "K02"               TO  SPA-MOTOPG
      *
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
           CLOSE                       PARA-FILE
      *
           MOVE    "K01"               TO  SPA-SAKIPG
           MOVE    "K02"               TO  SPA-MOTOPG
      *
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
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
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
           MOVE    "K02"               TO  SPA-MOTOPG
      *
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
           MOVE    SPA-LAST-PTNUM      TO  K02-PTNUM
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
           MOVE    "K02"               TO  SPA-MOTOPG
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
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (FLG-END             =   1     )
               GO      TO      480-YOYAKU-EXT
           END-IF
      *
      *    更新前のチェック
      *    MOVE    "1"                 TO  FLG-TOROKU
      *    PERFORM 510-TBLHEN-SEC
      *    MOVE    SPACE               TO  FLG-TOROKU
      *    IF      SPA-ERRCD       NOT =   SPACE
      *        GO      TO      480-YOYAKU-EXT
      *****END-IF
      *
      *    ワーク行為負担パラメタを作成
           PERFORM 4410-WSINPARA-SEC
           CLOSE                       PARA-FILE
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-K02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "K02"               TO  SPA-MOTOPG
           MOVE    "K02"               TO  SPA-MOTOPG3
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
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
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
           CLOSE                       PARA-FILE
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-K02             TO  LNK-SCRSPA
      *
           MOVE    "U01"               TO  SPA-SAKIPG
           MOVE    "K02"               TO  SPA-MOTOPG
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
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
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
           CLOSE                       PARA-FILE
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
      *
           MOVE    "U02"               TO  SPA-SAKIPG
           MOVE    "K02"               TO  SPA-MOTOPG
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
      *    患者マスター存在チェック
           IF      SPA-PTNUM           =   SPACE
               MOVE    "1003"              TO  SPA-ERRCD
               MOVE    00                  TO  SPA-GMN-CUR
               GO      TO      41001-KIHON-CHK-EXT
           END-IF
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
                   MOVE    00              TO  SPA-GMN-CUR
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
      *
      *    診療行為入力チェック
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (FLG-END             =   1     )
               GO      TO      41001-KIHON-CHK-EXT
           END-IF
      *
      *    診療日付のチェック訂正の時、入力エラー
           IF      SPA-SYORIFLG        =   1
               IF      K02-SRYYMD      NOT =   SPA-GMN-SRYYMD
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
      *
           MOVE   ZERO                 TO  IDX-SET
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX              >   200       )   OR
                              (SPA-ERRCD        NOT =   SPACE )   OR
                              (SPA-GMN-INPUTCD(IDX) =   SPACE )
               IF     (SPA-COM-CHKFLG (IDX)    =   SPACE )  AND
                      (SPA-GMN-INPUTCD(IDX)(1:1)
                                           NOT =   "."  AND "S")
      *
      *            点数マスタ編集・基本チェック
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
      *            当月入院有無
                   MOVE    SPA-K02N-NYUINFLG   TO  LNK-SC92-NYUINFLG
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
           MOVE    "K02"               TO  SPA-MOTOPG
      *
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
           IF      SPA-ERRCD       NOT =   SPACE
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
      *    ＳＰＡ保存パラメタを作成
           PERFORM 410021-PARA-SYORI-SEC
      *
      *    負担金算定・収納マスタ処理 
           PERFORM 4520-SYUNOU-SYORI-SEC
      *
      *    診療行為マスタのパラメタを作成する
      *****OPEN    I-O                 PARA-FILE
           PERFORM 4502-SRYACT-SET-SEC
      *
      *    自動作成の診療行為作成処理
           PERFORM 4503-JIDOU-SAKU-SEC
      *
           CLOSE                   PARA-FILE
      *
           MOVE    "K08"               TO  SPA-NAI-SAKIPG
      *****MOVE    "K03"               TO  SPA-SAKIPG
           MOVE    SPA-NAI-SAKIPG      TO  SPA-SAKIPG
           MOVE    "K02"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
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
      *****************************************************************
      *    ＳＰＡをパラメタファイルへ書込処理
      *****************************************************************
       410021-PARA-SYORI-SEC          SECTION.
      *
      *     共通ＳＰＡへ編集
      *    保険組合
           IF      SPA-PTID        NOT =   ZERO
               PERFORM 3101-HKNCOMBI-HEN-SEC
            END-IF
      *
           MOVE    SPA-TERMID          TO  FILE-TERMID2
      *
           OPEN    OUTPUT              PARA-FILE
      *
      *    共通ＳＰＡをパラメタへ出力
           MOVE    SPACE               TO  PARA-REC
           MOVE    "K01PARA"           TO  PARA-FILEMEI
           MOVE    1                   TO  PARA-EDANUM
           MOVE    SPA-K01KYOUTU       TO  PARA-DATA-REC
           WRITE                       PARA-REC
      *
      *    Ｋ０２ＳＰＡをパラメタへ出力
           MOVE    SPACE               TO  PARA-REC
           MOVE    "K02SPA"            TO  PARA-FILEMEI
           MOVE    1                   TO  PARA-EDANUM
      *****MOVE    SPA-K02-AREA        TO  PARA-DATA-REC
           MOVE    SPA-K02             TO  PARA-DATA-REC
           WRITE                       PARA-REC
      *
           .
       410021-PARA-SYORI-EXT.
           EXIT.
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
                   UNTIL       IDX     >   10
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL      (IDY     >   30      )
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
                   IF      LNK-ORCSC-SRYCD-KS(IDX IDY)   =   SPACE
                       MOVE    "1"         TO  WRK-KSN-ZAIEND (IDX-S)
                   ELSE
                       ADD     1               TO  IDX-S
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
                   END-IF
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
      *    診療行為マスタのパラメタを作成処理
      *****************************************************************
       4502-SRYACT-SET-SEC             SECTION.
      *
      *    初期の内容をクリアする
      *    MOVE    ZERO                TO  FLG-PARA
      *    PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL       FLG-PARA   =    1
      *        MOVE    SPACE               TO  PARA-REC
      *        MOVE    SPA-TERMID          TO  PARA-TERMID
      *        MOVE    "WKSRYACT"          TO  PARA-FILEMEI
      *        MOVE    IDX                 TO  PARA-EDANUM
      *        PERFORM 980-PARA-READ-SEC
      *        IF      FLG-PARA            =   ZERO 
      *            DELETE      PARA-FILE     RECORD
      *        END-IF
      *    END-PERFORM
      *
      *    CLOSE   PARA-FILE
      *    OPEN    I-O       PARA-FILE
      *
           MOVE    1                   TO  WRK-ZAINUM
           MOVE    ZERO                TO  WRK-RENNUM
           MOVE    ZERO                TO  WRK-PARAEDABAN
           MOVE    ZERO                TO  WRK-TENSUKEI
           MOVE    ZERO                TO  WRK-KAISUKEI
           MOVE    ZERO                TO  WRK-KINGAK
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  WRK-MEINUM
           INITIALIZE                  WRK-WKSRY-AREA
      *
           MOVE    SPACE               TO  WKSRYACT-REC
           INITIALIZE                  WKSRYACT-REC
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
               IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "."
                   IF      IDY             >   1
                       PERFORM 45021-SRYACT-OUT-SEC
                   END-IF
                   MOVE    SPA-COM-SRYSYUKBN (IDX) TO  WRK-SRYSYUKBN
                   MOVE    SPA-COM-SRYKBN (IDX)    TO  WRK-SRYKBN
               ELSE
      *
                   ADD     1                       TO  IDY
      *            入力コード
                   MOVE    SPA-GMN-INPUTCD(IDX)    TO  WKSRY-INPUTCD
                                                                 (IDY)
      *            行為コード
                   MOVE    SPA-COM-INPUTCD(IDX)    TO  WKSRY-SRYCD
                                                                 (IDY)
      *            セットの時、セットコードを行為コードへ
                   IF     (SPA-COM-INPUTCD(IDX)    =   SPACE )  AND
                          (SPA-GMN-INPUTCD(IDX)(1:1)   =   "S" )
                       MOVE    SPA-GMN-INPUTCD(IDX)(1:6)
                                                   TO  WKSRY-SRYCD
                                                                 (IDY)
                   END-IF
      *            数量
                   IF      SPA-COM-SURYO  (IDX)    =   ZERO
                       MOVE    1                   TO  SPA-COM-SURYO
                                                                 (IDX)
                   END-IF
                   MOVE    SPA-COM-SURYO  (IDX)    TO  WKSRY-SRYSURYO
                                                                  (IDY)
      *            回数
                   IF      SPA-COM-DATAKBN (IDX)   =   "3"
                       MOVE    SPA-COM-BUNGSU (IDX)
                                                  TO  WKSRY-SRYKAISU
                                                                 (IDY)
                   END-IF
      *            自動算定区分
                   MOVE    SPA-COM-AUTOKBN (IDX)    TO  WKSRY-AUTOKBN
                                                                 (IDY)
      *            数量の入力がある時
                   IF     (SPA-COM-AUTOKBN (IDX)    =   SPACE )  AND
                          (SPA-COM-SURFLG  (IDX)    =   "1"   )
                       MOVE    "S"                  TO  WKSRY-AUTOKBN
                                                                 (IDY)
                   END-IF
      *            時間外区分入力がある時、英字変換後セット
                   IF      SPA-COM-AUTOKBN (IDX)    =   SPACE
                       PERFORM 45022-JIKANGAI-SET-SEC
                   END-IF
      *            自費金額
                   IF      SPA-COM-SRYKBN (IDX)     =   "96"  OR  "95"
      *****************MOVE    SPA-COM-KEI (IDX)    TO  WKSRY-JIHIMONEY
                       MOVE    SPA-COM-JIHIMONEY (IDX)
                                                    TO  WKSRY-JIHIMONEY
                                                                 (IDY)
                   END-IF
      *            入力コメント番号
                   IF      SPA-COM-INPUTCD(IDX)(1:1)   =   "8"
                       ADD     1                   TO  WRK-MEINUM
                       MOVE    WRK-MEINUM          TO  WKSRY-INPUTNUM
                                                                 (IDY)
      *
                       MOVE    SPA-COM-ATAI1(IDX)  TO  WKSRY-INPUTCHI
                                                               (IDY 1)
                       MOVE    SPA-COM-ATAI2(IDX)  TO  WKSRY-INPUTCHI
                                                               (IDY 2)
      *                入力値
                       MOVE    SPA-COM-ATAI3(IDX)  TO  WKSRY-INPUTCHI
                                                               (IDY 3)
      *                入力値
                       MOVE    SPA-COM-ATAI4(IDX)  TO  WKSRY-INPUTCHI
                                                               (IDY 4)
      *                入力コメント
                       IF      SPA-COM-MEIFLG (IDX)    =   "1"
                           IF      SPA-COM-INPUTCD(IDX)(1:2) =   "83"
                                                             OR  "84"
                               MOVE    SPA-GMN-MEISYO (IDX)
                                                  TO  WKSRY-INPUTCOMENT
                                                                  (IDY)
                           ELSE
                               MOVE    SPA-GMN-MEISYO-G (IDX)
                                                  TO  WKSRY-INPUTCOMENT
                                                                  (IDY)
                           END-IF
                       ELSE
                           IF     (SPA-COM-INPUTCD (IDX)(1:1)
                                                       =  "8" )
      *****************      AND  (SPA-COM-INPUTCHI-G(IDX)
      *****************                            NOT =  SPACE)
                               MOVE    SPA-GMN-MEISYO(IDX)
                                                 TO  WKSRY-INPUTCOMENT
                                                                  (IDY)
                           END-IF
                       END-IF
                   END-IF
      *            回数保存
                   MOVE    SPA-COM-KAISU  (IDX)    TO  WRK-KAISUKEI
      *            画面の計を集計する（１剤で１件）
                   IF      SPA-COM-ZAIEND (IDX)       =   "1"
                       MOVE    SPA-COM-TENSU  (IDX)   TO  WRK-TENSUKEI
                       MOVE    SPA-COM-SYUTEN1(IDX)   TO  WRK-SYUTEN1
                       MOVE    SPA-COM-SYUTEN2 (IDX)  TO  WRK-SYUTEN2
                       MOVE    SPA-COM-YKZTEN  (IDX)  TO  WRK-YKZTEN
                       MOVE    SPA-COM-KIZAITEN(IDX)  TO  WRK-KIZAITEN
                   END-IF
      *
                   IF      SPA-COM-KEIFLG (IDX)   =   "1"  OR  "2"
      *****************MOVE     SPA-GMN-KEI    (IDX)   TO  WRK-KINGAK
      ***************  ADD      SPA-GMN-KEI    (IDX)   TO  WRK-KINGAK
                       ADD     SPA-COM-JIHIMONEY (IDX) TO  WRK-KINGAK
                   END-IF
      *
      *            剤終了か、ＭＡＸ件数の時出力
                   IF     (SPA-COM-ZAIEND(IDX) =   "1"    )  OR
                          (IDY                 =   5      )
                       MOVE    SPA-COM-SRYSYUKBN (IDX)
                                                   TO  WRK-SRYSYUKBN
                       MOVE    SPA-COM-SRYKBN (IDX)    TO  WRK-SRYKBN
                       PERFORM 45021-SRYACT-OUT-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      IDY                 >   ZERO
               MOVE    SPA-GMN-MAX             TO  IDX
               MOVE    SPA-COM-SRYSYUKBN (IDX) TO  WRK-SRYSYUKBN
               MOVE    SPA-COM-SRYKBN (IDX)    TO  WRK-SRYKBN
               PERFORM 45021-SRYACT-OUT-SEC
           END-IF
           .
       4502-SRYACT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    時間外区分 編集処理
      *****************************************************************
       45022-JIKANGAI-SET-SEC            SECTION.
      *
           IF     (SPA-GMN-INPUTCD(IDX)(1:1)       NUMERIC   ) AND
                  (SPA-GMN-INPUTCD(IDX)(2:1)       =   SPACE ) AND
                  (SPA-GMN-INPUTCD(IDX)(3:1)   NOT =   SPACE )
               EVALUATE    SPA-GMN-INPUTCD (IDX)(1:1)
                   WHEN    "1"
                       MOVE    "A"      TO  WKSRY-AUTOKBN  (IDY)
                   WHEN    "2"
                       MOVE    "B"      TO  WKSRY-AUTOKBN  (IDY)
                   WHEN    "3"
                       MOVE    "C"      TO  WKSRY-AUTOKBN  (IDY)
                   WHEN    "4"
                       MOVE    "D"      TO  WKSRY-AUTOKBN  (IDY)
               END-EVALUATE
           END-IF
           .
       45022-JIKANGAI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為パラメタ出力処理
      *****************************************************************
       45021-SRYACT-OUT-SEC              SECTION.
      *
            ADD     1                  TO  WRK-RENNUM
            MOVE    SPA-HOSPID         TO  WKSRY-HOSPID 
            MOVE    SPA-PTID           TO  WKSRY-PTID 
            MOVE    SPA-NYUGAIKBN      TO  WKSRY-NYUGAIKBN 
            MOVE    SPA-SRYKA          TO  WKSRY-SRYKA 
            MOVE    SPA-SRYYMD         TO  WKSRY-SRYYMD 
            MOVE    WRK-ZAINUM         TO  WKSRY-ZAINUM 
            MOVE    WRK-RENNUM         TO  WKSRY-RENNUM 
            MOVE    WRK-SRYSYUKBN      TO  WKSRY-SRYSYUKBN
            MOVE    WRK-SRYKBN         TO  WKSRY-SRYKBN
      ******MOVE    WRK-KINGAK         TO  WKSRY-JIHIMONEY
            MOVE    WRK-KINGAK         TO  WKSRY-JIHIMONEYTOTAL
      *     診療会計マスタ作成の為、剤点数計、回数を保存する
            MOVE    WRK-TENSUKEI       TO  WKSRY-ZAITENKEI
            MOVE    WRK-KAISUKEI       TO  WKSRY-ZAIKAIKEI
      *     各計
            MOVE    WRK-SYUTEN1        TO  WKSRY-SYUTEN1
            MOVE    WRK-SYUTEN2        TO  WKSRY-SYUTEN2
            MOVE    WRK-YKZTEN         TO  WKSRY-YKZTEN
            MOVE    WRK-KIZAITEN       TO  WKSRY-KIZAITEN
      *
      *     同時診療伝票番号
            MOVE    SPA-DOUJI-DENPNUM   TO  WKSRY-DOUJI-DENPNUM
      *     同時診療連番
            MOVE    SPA-DOUJI-RENNUM    TO  WKSRY-DOUJI-RENNUM
      *     継続区分
            MOVE    SPA-CONTKBN         TO  WKSRY-CONTKBN
      *
      *
           ADD     1                   TO  WRK-PARAEDABAN
           MOVE    SPACE               TO  PARA-REC
      *****MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "WKSRYACT"          TO  PARA-FILEMEI
           MOVE    WRK-PARAEDABAN      TO  PARA-EDANUM
      *    ＤＢからの作成でない
           MOVE    ALL "1"             TO  WKSRY-TERMID
      *
           MOVE    WKSRYACT-REC        TO  PARA-DATA-REC
      *
           WRITE   PARA-REC
           IF      STS-PARA        NOT =   ZERO
               DISPLAY "K02 PARA WRITE ERR:"   STS-PARA   "."
           END-IF
      *
           MOVE    ZERO                TO  WRK-TENSUKEI
           INITIALIZE                  WRK-WKSRY-AREA
           MOVE    ZERO                TO  IDY
           MOVE    SPACE               TO  WKSRYACT-REC
           INITIALIZE                  WKSRYACT-REC
      *
           IF      SPA-COM-ZAIEND(IDX) =   "1"
               ADD     1                   TO  WRK-ZAINUM
               MOVE    ZERO                TO  WRK-RENNUM
               MOVE    ZERO                TO  WRK-TENSUKEI
               MOVE    ZERO                TO  WRK-KAISUKEI
               MOVE    ZERO                TO  WRK-KINGAK
               MOVE    ZERO                TO  WRK-MEINUM
               MOVE    ZERO                TO  IDY
               INITIALIZE                  WRK-WKSRY-AREA
           END-IF
           .
       45021-SRYACT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動作成の診療行為作成処理
      *****************************************************************
       4503-JIDOU-SAKU-SEC         SECTION.
      *
      *    自動加算料
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   WRK-KSN-IDX
               IF      IDY             =   ZERO
                   MOVE    SPACE               TO  WKSRYACT-REC
                   INITIALIZE                  WKSRYACT-REC
                   ADD     1                   TO  WRK-ZAINUM
                   MOVE    ZERO                TO  WRK-RENNUM
                   MOVE    WRK-KSN-SRYSYUKBN(IDX)
                                               TO  WRK-SRYSYUKBN
                   MOVE    WRK-KSN-SRYKBN(IDX) TO  WRK-SRYKBN
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   MOVE    ZERO                TO  WRK-KAISUKEI
                   MOVE    ZERO                TO  WRK-KINGAK
                   MOVE    ZERO                TO  WRK-MEINUM
               END-IF
               ADD     1                       TO  IDY
               MOVE    WRK-KSN-SRYCD  (IDX)    TO  WKSRY-SRYCD (IDY)
               MOVE    1                       TO  WKSRY-SRYSURYO(IDY)
               MOVE    ZERO                    TO  WKSRY-ZAIKAISU(IDY)
               MOVE    "3"                     TO  WKSRY-AUTOKBN (IDY)
               IF     (WRK-KSN-SRYCD  (IDX)(1:1)   =   "8"   )  AND
                      (WRK-KSN-ATAI-G (IDX)    NOT =   SPACE )
                   PERFORM 45022-COMMNT-SET-SEC
               END-IF
               ADD     WRK-KSN-TENSU(IDX)      TO  WRK-TENSUKEI
               ADD     WRK-KSN-TENSU(IDX)      TO  WRK-SYUTEN1
               MOVE    1                       TO  WRK-KAISUKEI
      *
               IF      WRK-KSN-ZAIEND(IDX)     =   "1"
      *            診療行為パラメタ出力
                   PERFORM 45021-SRYACT-OUT-SEC
                   MOVE    ZERO                TO  IDY
               END-IF
           END-PERFORM
           .
       4503-JIDOU-SAKU-EXT.
           EXIT.
      *****************************************************************
      *    追加のコメントの日数変換編集処理
      *****************************************************************
       45022-COMMNT-SET-SEC              SECTION.
      *
           INITIALIZE                  WRK-SCR-REC
           MOVE    WKSRY-SRYCD (IDY)   TO  WRK-COM-INPUTCD (01)
           MOVE    WRK-KSN-ATAI-G(IDX) TO  WRK-COM-INPUTCHI-G(01)
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    "K02"               TO  LNK-SC92-PG
           MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
           MOVE    SPACE               TO  LNK-SC92-KBN
           MOVE    1                   TO  LNK-SC92-GMN-IDX
           MOVE    WKSRY-SRYCD (IDY)   TO  LNK-SC92-SRYCD
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
           MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
      *    当月入院有無
           MOVE    SPA-K02N-NYUINFLG   TO  LNK-SC92-NYUINFLG
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
      *    入力コメント番号
           MOVE    1                   TO  WKSRY-INPUTNUM  (IDY)
      *
           MOVE    WRK-COM-ATAI1(01)   TO  WKSRY-INPUTCHI  (IDY 1)
           MOVE    WRK-COM-ATAI2(01)   TO  WKSRY-INPUTCHI  (IDY 2)
           MOVE    WRK-COM-ATAI3(01)   TO  WKSRY-INPUTCHI  (IDY 3)
           MOVE    WRK-COM-ATAI4(01)   TO  WKSRY-INPUTCHI  (IDY 4)
           MOVE    WRK-GMN-MEISYO(01)  TO  WKSRY-INPUTCOMENT (IDY)
      *
           .
       45022-COMMNT-SET-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為データ出力処理
      *****************************************************************
       45109-SNRYO-NYU-SEC              SECTION.
      *
      *****処方せん発行の有無
      *    MOVE    ZERO                TO  FLG-SYOHOU-ARI
      *    PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL      (IDX         >   SPA-GMN-MAX  )  OR
      *                       (FLG-SYOHOU-ARI  =   1     )
      *        IF    ((SPA-COM-SRYKBN(IDX)     =   "21"  OR  "22"  OR
      *                                            "23"   )  AND
      *               (SPA-COM-ZAIEND(IDX)     =   "1"    )  AND
      *               (SPA-COM-TENSU (IDX)     =   ZERO   ))     OR
      *              ((SPA-COM-SRYKBN(IDX)     =   "14"   )  AND
      *               (SPA-COM-YKZTEN (IDX)    >   ZERO   ))
      *            MOVE    1                   TO  FLG-SYOHOU-ARI
      *        END-IF
      *    END-PERFORM
      *
      *****IF      FLG-SYOHOU-ARI      =   1
      *********処方せん発行あり
      *        確認メッセージ
               MOVE    "0117"              TO  SPA-KIDCD2
               MOVE    SPACE               TO  SPA-KID2-FLG
      **** ELSE
      *        処方せん発行なし
      *        PERFORM 4510-SNRYO-NYU-SEC
      **** END-IF
      *
           .
       45109-SNRYO-NYU-EXT.
           EXIT.
      *****************************************************************
      *    ワーク行為負担データを作成、Ｋ０１へ
      *****************************************************************
       4510-SNRYO-NYU-SEC              SECTION.
      *
      *    現在の内容をクリア
           MOVE    SPACE               TO  WKSRYACT-REC
           INITIALIZE                      WKSRYACT-REC
           MOVE    SPA-HOSPID          TO  WKSRY-HOSPID
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-SRYKA           TO  WKSRY-SRYKA
           MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD
           MOVE    SPA-HKNCOMBINUM     TO  WKSRY-HKNCOMBI
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "WKSRYACT-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           MOVE    "1"                 TO  FLG-SYORI
           PERFORM 45101-SNRYO-SYORI-SEC
      *
      *    処方せん発行の有無
           MOVE    ZERO                TO  FLG-SYOHOU-ARI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-GMN-MAX  )  OR
                              (FLG-SYOHOU-ARI  =   1     )
      *        院外処方
               IF    ((SPA-COM-SRYKBN(IDX)     =   "21"  OR  "22"  OR
                                                   "23"   )  AND
                      (SPA-COM-ZAIEND(IDX)     =   "1"    )  AND
                      (SPA-COM-TENSU (IDX)     =   ZERO   ))
      ************** ((SPA-COM-SRYKBN(IDX)     =   "14"   )  AND
      *************   (SPA-COM-YKZTEN(IDX)     >   ZERO   ))
      *            薬剤のみの時、院内分とする
                   IF      SPA-COM-SRYSYUKBN(IDX)  =  "213"  OR
                                                      "223"  OR
                                                      "233"
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-SYOHOU-ARI
                   END-IF
               END-IF
      *        在宅注射院外処方
               IF     (SPA-COM-SRYKBN(IDX)     =   "14"  )
      ******** AND   ((SPA-COM-YKZTEN(IDX)     >   ZERO  ) OR
      *********       (SPA-COM-KIZAITEN(IDX)   >   ZERO  ))
                   IF      SPA-COM-SRYSYUKBN(IDX)  =   "148"  OR
                                                       "149"
      *                院外分
                       MOVE    1                   TO  FLG-SYOHOU-ARI
                   END-IF
               END-IF
           END-PERFORM
      *
      *    中途終了時、処方せんの有無
           EVALUATE    SPA-KID2-FLG
      *        処方せん発行
               WHEN    "P1"
                   IF      FLG-SYOHOU-ARI      =   1
      *                院外
                       MOVE    3                   TO  FLG-SYOHOU
                    ELSE
      *                院内
                       MOVE    4                   TO  FLG-SYOHOU
                   END-IF
                   PERFORM 4124-SYOHOU-SYORI-SEC
               WHEN    "P2"
      *            薬剤情報発行
                   PERFORM 4125-YAKJYO-SYORI-SEC
               WHEN    "P3"
      *            処方せん発行
                   IF      FLG-SYOHOU-ARI      =   1
                       MOVE    3                   TO  FLG-SYOHOU
                    ELSE
                       MOVE    4                   TO  FLG-SYOHOU
                   END-IF
                   PERFORM 4124-SYOHOU-SYORI-SEC
      *            薬剤情報発行
                   PERFORM 4125-YAKJYO-SYORI-SEC
           END-EVALUATE
           MOVE    SPACE               TO  SPA-KID1-FLG
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
           MOVE    "1"                 TO  LNK-SC95-KBN
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
      **
      **** 選択画面戻る
      *****MOVE    "K01"               TO  SPA-SAKIPG
      *    MOVE    "K02"               TO  SPA-MOTOPG
      *    MOVE    SPACE               TO  SPA-MOTOPG2
      *
      *    MOVE    SPACE               TO  LINKAREA
      *    INITIALIZE                      SPA-KYOTUKEY
      *
      *    MOVE   "CHANGE"             TO  MCP-PUTTYPE
      *    MOVE    "K01"               TO  MCP-WINDOW
      *
      *    PERFORM 900-PUT-WINDOW
      *
      *****MOVE    1                   TO  FLG-END
           .
       4510-SNRYO-NYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報発行処理
      *****************************************************************
       4125-YAKJYO-SYORI-SEC             SECTION.
      *
           INITIALIZE                      ORCSCH30AREA
           IF      FLG-SYOHOU-ARI      =   1
      *        院外分
               MOVE    "4"                 TO  ORCSCH30-KBN
           ELSE
      *        院内分
               MOVE    "3"                 TO  ORCSCH30-KBN
           END-IF
           CALL    "ORCSCH30"          USING   SPA-AREA
                                               ORCSCH30AREA
      *
           .
       4125-YAKJYO-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為データ出力処理
      *****************************************************************
       45101-SNRYO-SYORI-SEC              SECTION.
      *
           MOVE    1                   TO  WRK-ZAINUM
           MOVE    ZERO                TO  WRK-RENNUM
           MOVE    ZERO                TO  WRK-MEINUM
           MOVE    ZERO                TO  WRK-TENSUKEI
           MOVE    ZERO                TO  WRK-KAISUKEI
           MOVE    ZERO                TO  WRK-KINGAK
           MOVE    ZERO                TO  IDY
           INITIALIZE                  WRK-WKSRY-AREA
      *
           MOVE    SPACE               TO  WKSRYACT-REC
           INITIALIZE                  WKSRYACT-REC
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
               IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "."
                   MOVE    SPA-COM-SRYKBN (IDX)    TO  WRK-SRYKBN
                   MOVE    SPA-COM-SRYSYUKBN(IDX)  TO  WRK-SRYSYUKBN
               ELSE
      *
                   ADD     1                       TO  IDY
      *            入力コード
                   MOVE    SPA-GMN-INPUTCD(IDX)    TO  WKSRY-INPUTCD
                                                                 (IDY)
      *            行為コード
                   MOVE    SPA-COM-INPUTCD(IDX)    TO  WKSRY-SRYCD
                                                                 (IDY)
      *            セットの時、セットコードを行為コードへ
                   IF     (SPA-COM-INPUTCD(IDX)    =   SPACE )  AND
                          (SPA-GMN-INPUTCD(IDX)(1:1)   =   "S" )
                       MOVE    SPA-GMN-INPUTCD(IDX)(1:6)
                                                    TO  WKSRY-SRYCD
                                                                 (IDY)
                   END-IF
      *
      *            数量
                   IF      SPA-COM-SURYO  (IDX)    =   ZERO
                       MOVE    1                   TO  SPA-COM-SURYO
                                                                 (IDX)
                   END-IF
                   MOVE    SPA-COM-SURYO  (IDX)    TO  WKSRY-SRYSURYO
                                                                 (IDY)
      *            回数
                   IF      SPA-COM-DATAKBN (IDX)   =   "3"
                       MOVE    SPA-COM-BUNGSU(IDX) TO  WKSRY-SRYKAISU
                                                                 (IDY)
                   END-IF
      *            自動算定区分
                   MOVE    SPA-COM-AUTOKBN (IDX)    TO  WKSRY-AUTOKBN
                                                                 (IDY)
      *            数量の入力がある時
                   IF     (SPA-COM-AUTOKBN (IDX)    =   SPACE )  AND
                          (SPA-COM-SURFLG  (IDX)    =   "1"   )
                       MOVE    "S"                  TO  WKSRY-AUTOKBN
                                                                 (IDY)
                   END-IF
      *            時間外区分入力がある時、英字変換後セット
                   IF      SPA-COM-AUTOKBN (IDX)    =   SPACE
                       PERFORM 45022-JIKANGAI-SET-SEC
                   END-IF
      *
      *            自費金額
                   IF      SPA-COM-SRYKBN (IDX)     =   "96"  OR  "95"
      *****************MOVE    SPA-COM-KEI (IDX)    TO  WKSRY-JIHIMONEY
                       MOVE    SPA-COM-JIHIMONEY (IDX)
                                                    TO  WKSRY-JIHIMONEY
                                                                 (IDY)
                   END-IF
      *            入力コメント番号
                   IF      SPA-COM-INPUTCD(IDX)(1:1)   =   "8"
                       ADD     1                   TO  WRK-MEINUM
                       MOVE    WRK-MEINUM          TO  WKSRY-INPUTNUM
                                                                 (IDY)
      *
                       MOVE    SPA-COM-ATAI1(IDX)  TO  WKSRY-INPUTCHI
                                                                (IDY 1)
                       MOVE    SPA-COM-ATAI2(IDX)  TO  WKSRY-INPUTCHI
                                                                (IDY 2)
      *                入力値
                       MOVE    SPA-COM-ATAI3(IDX)  TO  WKSRY-INPUTCHI
                                                                (IDY 3)
      *                入力値
                       MOVE    SPA-COM-ATAI4(IDX)  TO  WKSRY-INPUTCHI
                                                                (IDY 4)
      *                入力コメント
                       IF      SPA-COM-MEIFLG (IDX)    =   "1"
                           IF      SPA-COM-INPUTCD(IDX)(1:2) =   "83"
                                                             OR  "84"
                               MOVE    SPA-GMN-MEISYO (IDX)
                                                  TO  WKSRY-INPUTCOMENT
                                                                  (IDY)
                           ELSE
                               MOVE    SPA-GMN-MEISYO-G (IDX)
                                                  TO  WKSRY-INPUTCOMENT
                                                                  (IDY)
                           END-IF
                       ELSE
                           IF     (SPA-COM-INPUTCD (IDX)(1:1)
                                                       =   "8" )  AND
                                  (SPA-COM-INPUTCHI-G(IDX)
                                                   NOT =   SPACE)
                               MOVE    SPA-GMN-MEISYO(IDX)
                                                  TO  WKSRY-INPUTCOMENT
                                                                  (IDY)
                           END-IF
                       END-IF
                   END-IF
      *
                   MOVE    SPA-COM-KAISU  (IDX)   TO  WRK-KAISUKEI
      *            画面の計を集計する（１剤で１件）
                   IF      SPA-COM-ZAIEND (IDX)       =   "1"
                       MOVE    SPA-GMN-KEI    (IDX)   TO  WRK-TENSUKEI
      *                各計を編集する
                       MOVE    SPA-COM-SYUTEN1(IDX)   TO  WRK-SYUTEN1
                       MOVE    SPA-COM-SYUTEN2 (IDX)  TO  WRK-SYUTEN2
                       MOVE    SPA-COM-YKZTEN  (IDX)  TO  WRK-YKZTEN
                       MOVE    SPA-COM-KIZAITEN(IDX)  TO  WRK-KIZAITEN
                   END-IF
      *
                   IF      SPA-COM-KEIFLG (IDX)   =   "1"  OR  "2"
      *****************MOVE     SPA-GMN-KEI (IDX) TO  WRK-KINGAK
      **************** ADD      SPA-GMN-KEI (IDX) TO  WRK-KINGAK
                       ADD      SPA-COM-JIHIMONEY (IDX)
                                                  TO  WRK-KINGAK
                   END-IF
      *
                   IF     (SPA-COM-ZAIEND (IDX)   =   "1"   )  OR
                          (IDY                    =   5     )
                       MOVE    SPA-COM-SRYKBN  (IDX)  TO  WRK-SRYKBN
                       MOVE    SPA-COM-SRYSYUKBN(IDX) TO  WRK-SRYSYUKBN
                       IF      FLG-SYORI           =   "1"
                           PERFORM 45101-WKSRYACT-OUT-SEC
                       ELSE
                           PERFORM 44101-WKSINKPARA-OUT-SEC
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      IDY                 >   ZERO
               MOVE    SPA-GMN-MAX             TO  IDX
               MOVE    SPA-COM-SRYKBN (IDX)    TO  WRK-SRYKBN
               MOVE    SPA-COM-SRYSYUKBN(IDX)  TO  WRK-SRYSYUKBN
               IF      FLG-SYORI           =   "1"
                   PERFORM 45101-WKSRYACT-OUT-SEC
               ELSE
                   PERFORM 44101-WKSINKPARA-OUT-SEC
               END-IF
           END-IF
      *
           .
       45101-SNRYO-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為データ出力処理
      *****************************************************************
       45101-WKSRYACT-OUT-SEC              SECTION.
      *
      ******ADD     1                   TO  WRK-ZAINUM
            ADD     1                   TO  WRK-RENNUM
            MOVE    SPA-HOSPID          TO  WKSRY-HOSPID
            MOVE    SPA-PTID            TO  WKSRY-PTID
            MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
            MOVE    SPA-SRYKA           TO  WKSRY-SRYKA
            MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD
            MOVE    WRK-ZAINUM          TO  WKSRY-ZAINUM 
            MOVE    WRK-RENNUM          TO  WKSRY-RENNUM 
            MOVE    WRK-SRYSYUKBN       TO  WKSRY-SRYSYUKBN
            MOVE    WRK-SRYKBN          TO  WKSRY-SRYKBN
      ******MOVE    WRK-KINGAK          TO  WKSRY-JIHIMONEY
            MOVE    WRK-KINGAK          TO  WKSRY-JIHIMONEYTOTAL
      *
            MOVE    WRK-TENSUKEI        TO  WKSRY-ZAITENKEI
            MOVE    WRK-KAISUKEI        TO  WKSRY-ZAIKAIKEI
      *     各計
            MOVE    WRK-SYUTEN1         TO  WKSRY-SYUTEN1
            MOVE    WRK-SYUTEN2         TO  WKSRY-SYUTEN2
            MOVE    WRK-YKZTEN          TO  WKSRY-YKZTEN
            MOVE    WRK-KIZAITEN        TO  WKSRY-KIZAITEN
      *
            MOVE    SPA-HKNCOMBINUM     TO  WKSRY-HKNCOMBI
      *     同時診療伝票番号
            MOVE    SPA-DOUJI-DENPNUM   TO  WKSRY-DOUJI-DENPNUM
      *     同時診療連番
            MOVE    SPA-DOUJI-RENNUM    TO  WKSRY-DOUJI-RENNUM
      *     継続区分
            MOVE    SPA-CONTKBN         TO  WKSRY-CONTKBN
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "WKSRYACT-KEY"      TO  ORC-DBPATH
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               MOVE    "0090"              TO  SPA-ERRCD
               DISPLAY "K02 WKSRYACT INS ERR:"  MCP-RC
           END-IF
      *
           MOVE    ZERO                TO  IDY
           INITIALIZE                  WKSRYACT-REC
      *
           IF      SPA-COM-ZAIEND(IDX) =   "1"
               ADD     1                   TO  WRK-ZAINUM
               MOVE    ZERO                TO  WRK-RENNUM
               MOVE    ZERO                TO  WRK-TENSUKEI
               MOVE    ZERO                TO  WRK-KAISUKEI
               MOVE    ZERO                TO  WRK-KINGAK
               MOVE    ZERO                TO  WRK-MEINUM
               INITIALIZE                  WRK-WKSRY-AREA
           END-IF
            .
       45021-SRYACT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *   負担金算定・収納マスタ処理
      *****************************************************************
       4520-SYUNOU-SYORI-SEC             SECTION.
      *
           MOVE    SPACE               TO  FLG-NAIYAK
           MOVE    SPACE               TO  FLG-TONPUK
           MOVE    SPACE               TO  FLG-GAIYAK
      *
      *    診療行為より、収納マスタのパラメタを作成する 
      *****OPEN    I-O                 PARA-FILE
           MOVE    SPACE               TO  PARA-REC
      *    MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SYUNOU"            TO  PARA-FILEMEI
           MOVE    01                  TO  PARA-EDANUM
      *****PERFORM 980-PARA-READ-SEC
      *
      *    修正の時、収納テーブルを検索
           IF      SPA-SYORIFLG        =   1
               INITIALIZE                  SYUNOU-REC
               MOVE    SPA-HOSPID          TO  SYU-HOSPID
               MOVE    SPA-PTID            TO  SYU-PTID
               MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
               MOVE    SPA-DENPNUM         TO  SYU-DENPNUM
      *
               MOVE    SYUNOU-REC          TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
                   PERFORM 980-SYUNOU-READ-SEC
               ELSE
                   MOVE    SPACE               TO  SYUNOU-REC
                   INITIALIZE                  SYUNOU-REC
                   MOVE    1               TO  FLG-SYUNOU
               END-IF
               MOVE    "DBCLOSE"           TO  MCP-FUNC
               MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
           ELSE
      *
               MOVE    SPACE               TO  SYUNOU-REC
               INITIALIZE                  SYUNOU-REC
           END-IF
      *
           IF      SYU-PTID                =   ZERO
               MOVE    SPA-HOSPID          TO  SYU-HOSPID
               MOVE    SPA-PTID            TO  SYU-PTID
               MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
               MOVE    1                   TO  SYU-DENPNUM
               MOVE    SPA-SRYKA           TO  SYU-SRYKA
               MOVE    SPA-SRYYMD          TO  SYU-SRYYMD
               MOVE    SPA-SRYYMD          TO  SYU-SKYSTYMD
               MOVE    ALL "9"             TO  SYU-SKYEDYMD
               MOVE    "1"                 TO  SYU-DENPJTIKBN
           ELSE
      *        請求内容クリア
               INITIALIZE                  SYU-SKY-NAIYOU
               INITIALIZE                  SYU-JIHI-NAIYOU
               INITIALIZE                  SYU-JIHI-TOTAL
               INITIALIZE                  SYU-JIHI-TAX
      *        労災保険クリア
               INITIALIZE                  SYU-RSISHOSHIN-MONEY
               INITIALIZE                  SYU-RSISAISHIN-MONEY
               INITIALIZE                  SYU-RSISDO-MONEY
               INITIALIZE                  SYU-RSIETC-MONEY
      *        継続区分
      ******** MOVE    SYU-CONTKBN         TO  SPA-CONTKBN
      *        診療日
               MOVE    SPA-SRYYMD          TO  SYU-SRYYMD
           END-IF
      *
           MOVE    SPA-HKNCOMBINUM     TO  SYU-HKNCOMBINUM
           MOVE    SPA-FTNRATE         TO  SYU-PTFTNRATE
      *    IF      SPA-K03-CHOSEI      =   ZERO
      *        CONTINUE
      *    ELSE
      *        MOVE    SPA-K03-CHOSEI      TO  SYU-CHOSEI
      *    END-IF
      *****MOVE    SPA-K03-SKYKUKBN    TO  SYU-SKYKUKBN
      *    継続区分
      *****MOVE    SPA-CONTKBN         TO  SYU-CONTKBN
      *    保険情報
           MOVE    SPA-HKNNUM          TO  SYU-SYUHKNNUM
           MOVE    SPA-KOH1HKNNUM      TO  SYU-KOH1HKNNUM
           MOVE    SPA-KOH2HKNNUM      TO  SYU-KOH2HKNNUM
           MOVE    SPA-KOH3HKNNUM      TO  SYU-KOH3HKNNUM
           MOVE    SPA-KOH4HKNNUM      TO  SYU-KOH4HKNNUM
      *    同時診療伝票番号
           MOVE    SPA-DOUJI-DENPNUM   TO  SYU-DOUJI-DENPNUM
      *    院外処方区分
           MOVE    SPA-INGAIKBN        TO  SYU-INGAISHOHOKBN
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
               IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "."
                   CONTINUE
               ELSE
      ***********  IF      SPA-COM-ZAIEND (IDX)    =   "1"
                       PERFORM 4501-SEIKYU-KEI-SEC
      ***********  END-IF
               END-IF
           END-PERFORM
      *
      *    自動計算処理
           PERFORM 4504-JIDOUKEI-SEC
      *    各合計計算処理
           PERFORM 4506-TOTALKEI-SEC
      *    薬剤一部負担金計算処理
      *****PERFORM 4505-YAKFTN-KEI-SEC
      *
      *****IF      FLG-PARA            =   ZERO
      *        MOVE    SYUNOU-REC          TO  PARA-DATA-REC
      *        REWRITE                     PARA-REC
      *****ELSE
               MOVE    SPACE               TO  PARA-REC
      *********MOVE    SPA-TERMID          TO  PARA-TERMID
               MOVE    "SYUNOU"            TO  PARA-FILEMEI
               MOVE    01                  TO  PARA-EDANUM
               MOVE    SYUNOU-REC          TO  PARA-DATA-REC
               WRITE   PARA-REC
      *****END-IF
      *
      *****CLOSE                   PARA-FILE
      *
           .
       4520-SYUNOU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為より、収納マスタの請求額の計算処理
      *****************************************************************
       4501-SEIKYU-KEI-SEC             SECTION.
      *
      *   自費の集計（診療コードが'09500'で始まるもの）
          IF     (SPA-COM-INPUTCD(IDX)(1:5)   =   "09500"  OR
                                                  "09600" )  OR
                ((SPA-COM-INPUTCD(IDX)(1:5)   =   "09591"  OR
                                                  "09592"  OR
                                                  "09593" )  AND
                 (SPA-COM-SRYKBN (IDX)    NOT =   "80"   ))
               PERFORM 45010-JIHI-SET-SEC
               GO      TO      4501-SEIKYU-KEI-EXT
           END-IF
      *
      *   保険適用外の集計（診療コードが'095XX'で始まるもの）または、
      *                     自費の診療種別のもの
          IF     (SPA-COM-INPUTCD(IDX)(1:3)   =   "095"  OR  "096")  OR
                 (SPA-COM-SRYSYUKBN (IDX)     =   "950"  OR  "960")
               PERFORM 45011-TGMONEY-SET-SEC
               GO      TO      4501-SEIKYU-KEI-EXT
           END-IF
      *
      *    労災保険は円と点数に分ける
           IF      SPA-HKNKBN          =   1
               PERFORM 45011-RSI-SYUNOU-SEC
               GO      TO      4501-SEIKYU-KEI-EXT
           END-IF
      *
      *
      *    保険適用分は剤の最後の計を集計
           IF      SPA-COM-ZAIEND (IDX)    NOT =   "1"
               GO      TO      4501-SEIKYU-KEI-EXT
           END-IF
      *
           EVALUATE    SPA-COM-SRYKBN  (IDX)
      *        診療
               WHEN    "11"
               WHEN    "12"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-HKNTEN (01)
      *        指導
               WHEN    "13"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-HKNTEN (02)
      *        在宅
               WHEN    "14"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-HKNTEN (03)
      *        投薬
               WHEN    "21"   THRU  "27"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-HKNTEN (04)
      *        注射
               WHEN    "31"
               WHEN    "32"
               WHEN    "33"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-HKNTEN (05)
      *        処置
               WHEN    "40"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-HKNTEN (06)
      *        手術
               WHEN    "50"
               WHEN    "54"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-HKNTEN (07)
      *        検査
               WHEN    "60"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-HKNTEN (08)
      *        Ｘ線
               WHEN    "70"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-HKNTEN (09)
      *        その他
               WHEN    "80"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-HKNTEN (10)
           END-EVALUATE
      *
           .
       4501-SEIKYU-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費セット集計処理
      *****************************************************************
       45010-JIHI-SET-SEC               SECTION.
      *
           IF      SPA-NYUGAIKBN          =   "1"
               MOVE    SPA-COM-NYUTENTTLKBN(IDX)  TO  WRK-TENTTLKBN
           ELSE
               MOVE    SPA-COM-GAITENTTLKBN(IDX)  TO  WRK-TENTTLKBN
           END-IF
      *
           EVALUATE    SPA-COM-SRYSYUKBN (IDX)
      *        消費税なし
               WHEN    "950"
               EVALUATE    WRK-TENTTLKBN
                   WHEN    1
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-1
                   WHEN    2
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-2
                   WHEN    3
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-3
                   WHEN    4
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-4
                   WHEN    5
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-5
               END-EVALUATE
      *        消費税あり
               WHEN    "960"
               EVALUATE    WRK-TENTTLKBN
                   WHEN    1
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-1-TAX
                   WHEN    2
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-2-TAX
                   WHEN    3
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-3-TAX
                   WHEN    4
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-4-TAX
                   WHEN    5
                       ADD     SPA-COM-KEI (IDX)    TO  SYU-JIHI-5-TAX
               END-EVALUATE
           END-EVALUATE
      *
           .
       45010-JIHI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険適用外の集計処理
      *****************************************************************
       45011-TGMONEY-SET-SEC               SECTION.
      *
      *    自賠責の時、診断料・明細料はその他の編集する
           IF     (SPA-HKNKBN          =   1   )  AND
                  (SPA-RSI-HKNKBN      =   "4" )  AND
                  (SPA-COM-SRYKBN (IDX)
                                       =   "80"  )  AND
                  (SPA-COM-INPUTCD(IDX)(1:5)   =   "09591"
                                               OR  "09592"
                                               OR  "09593")
               PERFORM 450112-JIBAISEKI-SET-SEC
               GO      TO      45011-TGMONEY-SET-EXT
           END-IF
      *
           IF      SPA-COM-INPUTCD(IDX)(1:3)   =   "095"  OR  "096"
               MOVE    SPA-COM-INPUTCD(IDX)(4:2)   TO  WRK-SRYKBN
           ELSE
               MOVE    SPA-COM-TNSSRYKBN(IDX)      TO  WRK-SRYKBN
           END-IF
      *
      *    薬剤の時、投薬へ
           IF      SPA-COM-INPUTCD(IDX)(1:1)   =   "6"
      *        注射薬の時、注射へ
               IF      SPA-COM-YKZKBN (IDX)    =   "4"
                   MOVE    "31"                        TO  WRK-SRYKBN
               ELSE
                   MOVE    "21"                        TO  WRK-SRYKBN
               END-IF
           END-IF
      *    器材の時、その他へ
           IF      SPA-COM-INPUTCD(IDX)(1:1)   =   "7"
               MOVE    "80"                        TO  WRK-SRYKBN
           END-IF
      *
           EVALUATE    SPA-COM-SRYSYUKBN (IDX)
      *        消費税なし
               WHEN    "950"
                   PERFORM 450111-TGMONEY-SEC
      *        消費税あり
               WHEN    "960"
                   PERFORM 450111-TGMONEY-TAX-SEC
           END-EVALUATE
           .
       45011-TGMONEY-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険適用外の集計処理(消費税なし)
      *****************************************************************
       450111-TGMONEY-SEC               SECTION.
      *
           EVALUATE    WRK-SRYKBN
      *        診療
               WHEN    "11"
               WHEN    "12"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (01)
      *        指導
               WHEN    "13"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (02)
      *        在宅
               WHEN    "14"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (03)
      *        投薬
               WHEN    "21"   THRU  "27"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (04)
      *        注射
               WHEN    "31"
               WHEN    "32"
               WHEN    "33"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (05)
      *        処置
               WHEN    "40"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (06)
      *        手術
               WHEN    "50"
               WHEN    "54"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (07)
      *        検査
               WHEN    "60"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (08)
      *        Ｘ線
               WHEN    "70"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (09)
      *        その他
               WHEN    "80"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY (10)
           END-EVALUATE
      *
           .
       450111-TGMONEY-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険適用外の集計処理(消費税あり)
      *****************************************************************
       450111-TGMONEY-TAX-SEC               SECTION.
      *
           EVALUATE    WRK-SRYKBN
      *        診療
               WHEN    "11"
               WHEN    "12"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (01)
      *        指導
               WHEN    "13"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (02)
      *        在宅
               WHEN    "14"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (03)
      *        投薬
               WHEN    "21"   THRU  "27"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (04)
      *        注射
               WHEN    "31"
               WHEN    "32"
               WHEN    "33"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (05)
      *        処置
               WHEN    "40"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (06)
      *        手術
               WHEN    "50"
               WHEN    "54"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (07)
      *        検査
               WHEN    "60"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (08)
      *        Ｘ線
               WHEN    "70"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (09)
      *        その他
               WHEN    "80"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-TGMONEY-TAX (10)
           END-EVALUATE
      *
           .
       450111-TGMONEY-TAX-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    自賠責診断料・明細料その他の編集処理
      *****************************************************************
       450112-JIBAISEKI-SET-SEC        SECTION.
      *
      *    自賠責の時、診断料・明細料はその他の編集する
      *    労災保険−その他
           COMPUTE WRK-EN              =   SPA-COM-TEN (IDX)
                                       *   SPA-COM-KAISU (IDX)
           ADD     WRK-EN              TO  SYU-RSIETC-MONEY
      *
           .
       450112-JIBAISEKI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災保険計算処理
      *****************************************************************
       45011-RSI-SYUNOU-SEC               SECTION.
      *
      *    労災保険は円と点数に分ける
           IF     (SPA-COM-TENSIKIBETU (IDX)   =   1  )  AND
                  (SPA-COM-RSIKBN      (IDX)   =   1  )  AND
                  (SPA-COM-INPUTCD (IDX)(1:1)  =   "1")
      *        円
               COMPUTE WRK-EN          =   SPA-COM-TEN (IDX)
                                       *   SPA-COM-KAISU (IDX)
               MOVE    ZERO            TO  WRK-TENSU
           ELSE
               COMPUTE WRK-TENSU       =   SPA-COM-KISOTEN (IDX)
                                       *   SPA-COM-KAISU (IDX)
               MOVE    ZERO            TO  WRK-EN
           END-IF
      *
           EVALUATE    SPA-COM-SRYKBN  (IDX)
      *        初診
               WHEN    "11"
      *
      *        労災保険−初診料
                   ADD     WRK-EN              TO  SYU-RSISHOSHIN-MONEY
                   ADD     WRK-TENSU           TO  SYU-HKNTEN (01)
      *        再診
               WHEN    "12"
      *            *労災保険−再診料
                   ADD     WRK-EN              TO  SYU-RSISAISHIN-MONEY
                   ADD     WRK-TENSU           TO  SYU-HKNTEN (01)
      *        指導
               WHEN    "13"
      *            労災保険−指導料
                   ADD     WRK-EN              TO  SYU-RSISDO-MONEY
                   ADD     WRK-TENSU           TO  SYU-HKNTEN (02)
      *        在宅
               WHEN    "14"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-HKNTEN (03)
      *        投薬
               WHEN    "21"   THRU  "27"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-HKNTEN (04)
      *        注射
               WHEN    "31"
               WHEN    "32"
               WHEN    "33"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-HKNTEN (05)
      *        処置
               WHEN    "40"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-HKNTEN (06)
      *        手術
               WHEN    "50"
               WHEN    "54"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-HKNTEN (07)
      *        検査
               WHEN    "60"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-HKNTEN (08)
      *        Ｘ線
               WHEN    "70"
                   ADD     SPA-GMN-KEI (IDX)   TO  SYU-HKNTEN (09)
      *        その他
               WHEN    "80"
      *            労災保険−その他
                   ADD     WRK-EN              TO  SYU-RSIETC-MONEY
      *            その他の円と点数は剤内で混在しないこととする(H14.8)
                   IF      WRK-EN              =   ZERO
                       ADD     SPA-GMN-KEI (IDX)   TO  SYU-HKNTEN (10)
                   END-IF
           END-EVALUATE
           .
       45011-RSI-SYUNOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動計算処理
      *****************************************************************
       4504-JIDOUKEI-SEC               SECTION.
      *
      *    自動加算分集計
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX         >   WRK-KSN-IDX  
               EVALUATE    WRK-KSN-SRYKBN  (IDX)
      *            診療
                   WHEN    "11"
                   WHEN    "12"
                       ADD     WRK-KSN-TENSU (IDX) TO  SYU-HKNTEN (01)
      *            指導
                   WHEN    "13"
                       ADD     WRK-KSN-TENSU (IDX) TO  SYU-HKNTEN (02)
      *            在宅
                   WHEN    "14"
                       ADD     WRK-KSN-TENSU (IDX) TO  SYU-HKNTEN (03)
      *            投薬
                   WHEN    "21"   THRU  "27"
      *                薬剤料逓減　を減算
                       IF      WRK-KSN-SRYCD (IDX)   =   "630010002"
                           COMPUTE SYU-HKNTEN (04)   =   SYU-HKNTEN(04)
                                                     -   WRK-KSN-TENSU
                                                                 (IDX)
                       ELSE
                           ADD     WRK-KSN-TENSU (IDX)   TO  SYU-HKNTEN
                                                                  (04)
                       END-IF
      *            注射
                   WHEN    "31"
                   WHEN    "32"
                   WHEN    "33"
                       ADD     WRK-KSN-TENSU (IDX)   TO  SYU-HKNTEN(05)
      *            処置
                   WHEN    "40"
                       ADD     WRK-KSN-TENSU (IDX)   TO  SYU-HKNTEN(06)
      *            手術
                   WHEN    "50"
                   WHEN    "54"
                       ADD     WRK-KSN-TENSU (IDX)   TO  SYU-HKNTEN(07)
      *            検査
                   WHEN    "60"
                       ADD     WRK-KSN-TENSU (IDX)   TO  SYU-HKNTEN(08)
      *            Ｘ線
                   WHEN    "70"
                       ADD     WRK-KSN-TENSU (IDX)   TO  SYU-HKNTEN(09)
      *            その他
                   WHEN    "80"
                       ADD     WRK-KSN-TENSU (IDX)   TO  SYU-HKNTEN(10)
               END-EVALUATE
           END-PERFORM
      *
           .
       4504-JIDOUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    各合計計算処理
      *****************************************************************
       4506-TOTALKEI-SEC             SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX         >   11
      *       保険点数
              ADD     SYU-HKNTEN(IDX)      TO  SYU-HKNTEN(12)
      *       金額
              ADD     SYU-MONEY (IDX)      TO  SYU-MONEY (12)
      *       適用外金額
              ADD     SYU-TGMONEY(IDX)     TO  SYU-TGMONEY(12)
      *       適用外金額（消費税あり）
              ADD     SYU-TGMONEY-TAX(IDX) TO  SYU-TGMONEY-TAX(12)
      *
           END-PERFORM
      *
           .
       4506-TOTALKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤一部負担金計算処理
      *****************************************************************
       4505-YAKFTN-KEI-SEC             SECTION.
      *
           INITIALIZE                  LNK-ORCS01-REC
           MOVE    ZERO                TO  IDX-Y
      *
      *    継続の時、前回までの内容をセット
           IF     (SPA-CONTKBN         NOT =   SPACE AND "0")  AND
                  (SPA-DOUJI-DENPNUM   NOT =   ZERO  )
               PERFORM 45051-YAKFTN-MAESET-SEC
           END-IF
           IF     (SPA-CONTKBN             =   "1"  OR  "2" )  AND
                  (SPA-DOUJI-DENPNUM   NOT =   ZERO  )
               MOVE    WRK-IDX-Y           TO  IDX-Y
           ELSE
               INITIALIZE                  LNK-ORCS01-REC
               MOVE    ZERO                TO  IDX-Y
           END-IF
      *
      *    修正の時、差額をセット
      *    IF      SPA-SYORIFLG        =   1
      *        PERFORM 45052-SEIKYU-SAGAKU-SEC
      *    END-IF
      *
           MOVE    ZERO                TO  IDY
           MOVE    1                   TO  WRK-ZAINUM
           ADD     1                   TO  IDX-Y
      *
      *    １診療行為で投薬の薬剤は４０以上ある時、無視する
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-MAX)
                       OR     (IDY     >=  40         )
      *        診療内容編集（薬剤と服用方法)
               IF     (SPA-COM-INPUTCD(IDX)(1:1)   =   "6"   )  OR
                      (SPA-COM-INPUTCD(IDX)(1:3)   =   "001" )
      *        院内薬剤のみとする
                   MOVE    SPA-COM-SRYSYUKBN (IDX) TO
                                                   WRK-CHK-SRYSYUKBN
                   IF      WRK-CHK-SRYSYUKBN       =   SPACE
                       EVALUATE    SPA-COM-SRYKBN (IDX)
                           WHEN    "21"
                               MOVE    "210"     TO  WRK-CHK-SRYSYUKBN
                           WHEN    "22"
                               MOVE    "220"     TO  WRK-CHK-SRYSYUKBN
                           WHEN    "23"
                               MOVE    "230"     TO  WRK-CHK-SRYSYUKBN
                      END-EVALUATE
                   END-IF
                   IF     (WRK-CHK-SRYSYUKBN           =   "211"  OR
                                                           "221"  OR
                                                           "231"  OR
                                                           "291" )  OR
                         ((SPA-INGAIKBN            NOT =   "1"   ) AND
                          (WRK-CHK-SRYSYUKBN           =   "210"  OR
                                                           "220"  OR
                                                           "230"  OR
                                                           "290" ))
                       ADD     1                   TO  IDY
                       MOVE    SPA-COM-SRYKBN(IDX) TO  LNK-S01-SRYKBN
                                                          (IDX-Y IDY)
                       MOVE    WRK-ZAINUM          TO  LNK-S01-ZAIBAN
                                                          (IDX-Y IDY)
                       MOVE    SPA-COM-INPUTCD(IDX)
                                                   TO  LNK-S01-SRYCD
                                                          (IDX-Y IDY)
                       MOVE    SPA-COM-SURYO(IDX)  TO  LNK-S01-SURYOU
                                                          (IDX-Y IDY)
                       MOVE    SPA-COM-KAISU(IDX)  TO  LNK-S01-KAISUU
                                                          (IDX-Y IDY)
                       MOVE    SPA-COM-TENSU(IDX)  TO  LNK-S01-TENSUU
                                                          (IDX-Y IDY)
                   END-IF
               END-IF
      *        剤ごとのカウント
               IF      SPA-COM-ZAIEND (IDX)    =   "1"
                   ADD     1               TO  WRK-ZAINUM
               END-IF
           END-PERFORM
      *
      *    剤ごとに点数を入れ直す
           PERFORM VARYING     IDY     FROM    40 BY  -1
                   UNTIL       IDY     <   1
      *
               IF      LNK-S01-SRYKBN (IDX-Y IDY)  NOT =   SPACE
                   IF     (IDY                     =   40    )  OR
                          (LNK-S01-ZAIBAN (IDX-Y IDY)  NOT =
                                       LNK-S01-ZAIBAN (IDX-Y IDY + 1))
                       MOVE    LNK-S01-TENSUU (IDX-Y IDY)
                                               TO  WRK-S01-TENSU
                   END-IF
                   MOVE    WRK-S01-TENSU       TO  LNK-S01-TENSUU
                                                       (IDX-Y IDY)
               END-IF
           END-PERFORM
      *
      *    薬剤一部負担金計算処理
           IF      SPA-SYORIFLG        =   1
               MOVE    "1"             TO  LNK-S01-SAIKEISAN-KBN
               MOVE    SPA-DENPNUM     TO  LNK-S01-SAIKEISAN-DENPYO
           END-IF
      *
      **** CALL    "ORCS01NEW"         USING   MCPAREA
      *                                        LNK-ORCS01-REC
      ****                                     SYUNOU-REC
      *
           .
       4505-YAKFTN-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    一部負担額計算用診療行為コードセット処理
      *****************************************************************
       45051-YAKFTN-MAESET-SEC            SECTION.
      *
           INITIALIZE                  LNK-ORCS01-REC
           MOVE    ZERO                TO  WRK-IDX-Y
           MOVE    ZERO                TO  IDX-Y
      *
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPID          TO  JYURRK-HOSPID
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-SRYKA           TO  JYURRK-SRYKA
           MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
      *****MOVE    SPA-DOUJI-RENNUM    TO  JYURRK-RENNUM
           IF      SPA-SYORIFLG        =   ZERO
               MOVE    SPA-DOUJI-RENNUM    TO  JYURRK-RENNUM
           ELSE
               MOVE    SPA-RENNUM          TO  JYURRK-RENNUM
           END-IF
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "JYURRK-KEY5"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "JYURRK-KEY5"       TO  ORC-DBPATH
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM
               UNTIL    FLG-JYURRK         =   1
            IF     (SPA-SYORIFLG           =   1      AND
                    SPA-DOUJI-RENNUM    NOT =   JYURRK-DOUJI-RENNUM) OR
                   (SPA-SYORIFLG           =   ZERO      )
               IF      JYURRK-EDANUM       =   1
                   MOVE    ZERO                TO  FLG-CHK
               END-IF
      *
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX                 >   15  )  OR
                                  (JYURRK-ZAINUM(IDX)  =   SPACE)
      *            診療会計マスタ読み込み
                   MOVE    SPACE               TO  SRYACCT-REC
                   INITIALIZE                      SRYACCT-REC
                   MOVE    SPA-HOSPID          TO  ACCT-HOSPID
                   MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
                   MOVE    SPA-PTID            TO  ACCT-PTID
                   MOVE    SPA-SRYKA           TO  ACCT-SRYKA
                   MOVE    SPA-SRYYMD(1:6)     TO  ACCT-SRYYM
                   MOVE    JYURRK-ZAINUM(IDX)  TO  ACCT-ZAINUM
      *
                   MOVE    SRYACCT-REC         TO  MCPDATA-REC
                   MOVE    "DBSELECT"          TO  MCP-FUNC
                   MOVE    "SRYACCT-KEY5"      TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"          USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
                   IF      MCP-RC              =   ZERO
                       MOVE    "SRYACCT-KEY5"      TO  ORC-DBPATH
                       PERFORM 940-SRYACCT-READ-SEC
                   ELSE
                       MOVE    1                   TO  FLG-SRYACCT
                   END-IF
                   PERFORM UNTIL    FLG-SRYACCT    =   1
      *
                       IF     (ACCT-SRYKBN(1:1)    =   "2"   OR
                               ACCT-SRYKBN         =   "80" )
                         AND  (ACCT-ZAITEN         >   ZERO )
      *                    診療行為マスター編集
                           PERFORM 450511-YAKFTN-SRYACT-SEC
                       END-IF
      *
                       MOVE    "SRYACCT-KEY5"      TO  ORC-DBPATH
                       PERFORM 940-SRYACCT-READ-SEC
                   END-PERFORM
      *
                   MOVE    "DBCLOSE"           TO  MCP-FUNC
                   MOVE    "SRYACCT-KEY5"      TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"          USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
               END-PERFORM
           END-IF
      *
               MOVE    "JYURRK-KEY5"       TO  ORC-DBPATH
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "JYURRK-KEY5"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           MOVE    IDX-Y               TO  WRK-IDX-Y
           .
       45051-YAKFTN-MAESET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター編集処理
      *****************************************************************
       450511-YAKFTN-SRYACT-SEC            SECTION.
      *
           COMPUTE IDX-Y3              =   JYURRK-RENNUM
                                       +   1
           IF      IDX-Y3              >   4
               MOVE    4               TO  IDX-Y3
           END-IF
           MOVE    ZERO                TO  WRK-ACCT-KAISU
           MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
           MOVE    ACCT-DAY (IDX-Y3 IDX-Y2)    TO  WRK-ACCT-KAISU
      *   診療行為マスター編集
           IF      FLG-CHK             =   ZERO
               ADD     1                   TO  IDX-Y
               MOVE    ZERO                TO  IDY
               MOVE    1                   TO  FLG-CHK
           END-IF
      *
      *    MOVE    ZERO                TO  IDY
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
      *            薬剤一部負担金用
                   IF     (ACCT-SRYKBN             =   "21"  OR
                                                       "22"  OR
                                                       "23" )   AND
                         ((SRY-SRYCD (IDX2)(1:1)   =   "6" )  OR
                          (SRY-SRYCD (IDX2)(1:3)   =   "001" ) )
                       ADD     1                   TO  IDY
                       MOVE    ACCT-SRYKBN         TO  LNK-S01-SRYKBN
                                                          (IDX-Y IDY)
                       MOVE    ACCT-ZAINUM         TO  LNK-S01-ZAIBAN
                                                          (IDX-Y IDY)
                       MOVE    SRY-SRYCD (IDX2)    TO  LNK-S01-SRYCD
                                                          (IDX-Y IDY)
                       MOVE    SRY-SRYSURYO(IDX2)  TO  LNK-S01-SURYOU
                                                          (IDX-Y IDY)
                       MOVE    WRK-ACCT-KAISU      TO  LNK-S01-KAISUU
                                                          (IDX-Y IDY)
                       MOVE    ACCT-ZAITEN         TO  LNK-S01-TENSUU
                                                          (IDX-Y IDY)
                   END-IF
      *
               END-PERFORM
      *
               MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
               PERFORM 900-SRYACT-NEXT-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       450511-YAKFTN-SRYACT-EXT.
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
      *****************MOVE    WRK-KNK-TOUYMDW (IDX-K1 IDX-K2)
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
           MOVE    "K02"               TO  SPA-MOTOPG
      *
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
           MOVE    "K02    "           TO  MCP-WINDOW.
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
                   UNTIL       WRK-IDX     >   200
               MOVE    ZERO                TO  SPA-COM-PAGE (WRK-IDX)
               MOVE    ZERO                TO  SPA-COM-GYOU (WRK-IDX)
               MOVE    SPACE               TO  SPA-COM-IN3  (WRK-IDX)
      *        画面表示外のものに頁、行位置を設定する
               IF      SPA-COM-SET    (WRK-IDX)  =   SPACE
                   ADD     1                 TO  IDX
                   IF      IDX               >   20
                       ADD     1             TO  WRK-PAGE
                       MOVE    1             TO  IDX
                   END-IF
                   IF     (SPA-GMN-INPUTCD(WRK-IDX)  NOT =   SPACE) OR
                          (SPA-COM-CUR (WRK-IDX)     NOT =   SPACE)
                       MOVE    IDX             TO  WRK-MAP-MAX
                       MOVE    WRK-PAGE        TO  WRK-MAP-MAXPAGE
      *                エラー位置
                       IF      SPA-COM-CUR (WRK-IDX)   =   "1" OR
                                                           "3"
                           IF      WRK-MAP-CUR         =   ZERO
                               MOVE    IDX            TO  WRK-MAP-CUR
                               MOVE    WRK-PAGE       TO  WRK-MAP-PAGE
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
                   IF     (SPA-MAP-IDX         >   20 )
                       OR (SPA-GMN-PAGE        <   WRK-MAP-MAXPAGE)
                       MOVE    99                  TO  SPA-MAP-IDX
                   ELSE
                       IF      SPA-GMN-PAGE    >   WRK-MAP-MAXPAGE
                           MOVE    WRK-MAP-MAXPAGE     TO  SPA-GMN-PAGE
                       END-IF
                   END-IF
               ELSE
                   MOVE    ZERO            TO  SPA-SINSATU-SAI
      *            最終行へ
                   IF      SPA-MAP-IDX         >   20
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
           MOVE    SPACE               TO  K02
           INITIALIZE                      K02
      *
           MOVE    ZERO                TO  IDX
           MOVE    1                   TO  WRK-PAGE
      *
           PERFORM VARYING     WRK-IDX     FROM    1   BY  1
                   UNTIL      (WRK-IDX     >   200  )  OR
                              (IDX         >=  20   )
      *        画面表示
               IF      SPA-COM-PAGE(WRK-IDX)   =   SPA-GMN-PAGE
                   ADD     1               TO  IDX
                   MOVE    SPA-GMN-SRYKBN (WRK-IDX)
                                               TO  K02-SRYKBN(IDX)
                   MOVE    SPA-GMN-INPUTCD(WRK-IDX)
                                               TO  K02-INPUTCD(IDX)
                   MOVE    SPA-GMN-MEISYO-G(WRK-IDX)
                                               TO  K02-MEISYO(IDX)
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
           MOVE    WRK-HEN-PAGE        TO  K02-MODE
      *
      *    前頁表示の時、１行目へ
           IF      SPA-GMN-CUR         =   99  OR  88
               MOVE    1                   TO  SPA-GMN-CUR
      *********MOVE    1                   TO  SPA-MAP-IDX
           END-IF
      *
      *    明細入力の時、コードが入力されていること
           IF      SPA-GMN-CUR         =   2
               IF      K02-INPUTCD(SPA-MAP-IDX)    =   SPACE
                   MOVE    1               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-PTNUM       TO  K02-PTNUM
           MOVE    SPA-GMN-KANANAME    TO  K02-KANANAME
           MOVE    SPA-GMN-NAME        TO  K02-NAME
           MOVE    SPA-GMN-SEX         TO  K02-SEX
      **** MOVE    SPA-GMN-HKNCOMBIMEI(1:28)
           MOVE    SPA-GMN-HKNCOMBI-G
                                       TO  K02-HKNCOMBI
           MOVE    SPA-GMN-FTNRATEMEI  TO  K02-RATE
      *    診療日
           IF      SPA-GMN-SRYYMD      =   SPACE
               MOVE    SPA-SRYYMDW         TO  K02-SRYYMD
           ELSE
               MOVE    SPA-GMN-SRYYMD      TO  K02-SRYYMD
           END-IF
           MOVE    SPA-GMN-BIRTHDAY    TO  K02-BIRTHDAY
      *****MOVE    SPA-GMN-SRYKAMEI    TO  K02-SRYKA
      *    年齢
           MOVE    SPA-NENREI-HEN      TO  K02-NENREI
      *
           MOVE    SPA-GMN-GOKEI       TO  WRK-GOKEI-Z2
           MOVE    WRK-GOKEI-X2        TO  K02-GOKEITEN
           MOVE    SPA-GMN-LASTYMD     TO  K02-LASTYMD
           MOVE    SPA-GMN-SYOSINYMD   TO  K02-SYOSINYMD
      *
           MOVE    SPA-GMN-MISYUMONEY  TO  WRK-GOKEI-Z3
           MOVE    WRK-GOKEI-X3        TO  K02-MISYUMONEY
      *    累計点数
      *        訂正の時、”＊”
      *    IF      SPA-SYORIFLG        =   1
      *        MOVE    ALL "*"             TO  K02-ZAITENTOTAL
      *    ELSE
               COMPUTE WRK-GOKEI-Z2        =   SPA-GMN-GOKEI
                                           +   SPA-ZAITENTOTAL
               MOVE    WRK-GOKEI-Z2        TO  K02-ZAITENTOTAL
      *    END-IF
      *
      *    保険組合せ
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-HKN-MAX
               MOVE    SPA-GMN-HKN-LIST (IDX)  TO  K02-HKNCOMBI-ITEM
                                                             (IDX)
           END-PERFORM
           MOVE    SPA-HKN-MAX         TO  K02-HKNCOMBI-COUNT
      *
      *    診療科
           MOVE    SPA-GMN-SRYKA-G     TO  K02-SRYKA
           PERFORM  VARYING   IDX     FROM    1   BY  1
                    UNTIL     IDX         >   SPA-SRYKA-MAX
               MOVE    SPA-GMN-SRYKALST (IDX)  TO  K02-SRYKA-ITEM
                                                             (IDX)
           END-PERFORM
           MOVE    SPA-SRYKA-MAX       TO  K02-SRYKA-COUNT
      *
      *    ＤＯ診療日
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-GMN-MAX1
               MOVE    SPA-GMN-NO     (IDX)    TO  WRK-ZZ
               MOVE    WRK-ZZ                  TO  K02-NO  (IDX)
               MOVE    SPA-GMN-RRKYMD (IDX)    TO  K02-RRKYMD(IDX)
               MOVE    SPA-GMN-RRKSRYKA(IDX)   TO  K02-RRKSRYKA(IDX)
               MOVE    SPA-GMN-RRKHKNCOMBI(IDX)
                                               TO  K02-RRKHKNCOMBI(IDX)
               IF      SPA-GMN-NO (IDX)    =   SPA-NAI-TESELNUM
                   MOVE    "T"             TO  K02-SELECT  (IDX)
               ELSE
                   MOVE    "F"             TO  K02-SELECT  (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-MAX1        TO  K02-COUNT
      *
           MOVE    SPA-GMN-DOSELNUM    TO  K02-DOSELNUM
      *
      *    院内院外
           IF      SPA-INGAIKBN        =   "1"  OR  "0"
               CONTINUE
           ELSE
               MOVE    SPA-SYOHOKBN        TO  SPA-INGAIKBN
           END-IF
           EVALUATE    SPA-INGAIKBN
               WHEN    "1"
                   MOVE    "T"             TO  SPA-GMN-INGAI
                   MOVE    "T"             TO  K02-INGAI-VALUE
                   MOVE    "院外"          TO  K02-INGAI-LABEL
               WHEN    "0"
                   MOVE    "F"             TO  SPA-GMN-INGAI
                   MOVE    "F"             TO  K02-INGAI-VALUE
                   MOVE    "院内"          TO  K02-INGAI-LABEL
           END-EVALUATE
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
                   MOVE    "T"             TO  K02-TEISEI-VALUE
                   MOVE    "訂正診療日"    TO  K02-TEISEI-LABEL
                   MOVE    1               TO  SPA-NAI-SYORI
                   MOVE    "T"             TO  SPA-GMN-TEISEI
               WHEN    "F"
               WHEN    SPACE
                   MOVE    "F"             TO  SPA-GMN-TEISEI
                   MOVE    "F"             TO  K02-TEISEI-VALUE
                   MOVE    "ＤＯ検索"      TO  K02-TEISEI-LABEL
                   MOVE    ZERO            TO  SPA-NAI-SYORI
           END-EVALUATE
      *
      *    処理
           EVALUATE    SPA-SYORIFLG
               WHEN    0
                   MOVE    SPACE           TO  K02-SYORIMEI-VALUE
               WHEN    1
                   MOVE    "red"           TO  K02-SYORIMEI-STYLE
                   MOVE    "［訂　正］"    TO  K02-SYORIMEI-VALUE
           END-EVALUATE
      *
      *    継続
           EVALUATE    SPA-CONTKBN
               WHEN    SPACE
                   MOVE    SPACE           TO  K02-CONTKBN-VALUE
                   IF      SPA-FUKU-SYORI      =   "1"  OR  "2"
                       MOVE    "red"       TO  K02-CONTKBN-STYLE
                       MOVE    "［複］"    TO  K02-CONTKBN-VALUE
                   END-IF
                   IF      SPA-FUKU-SYORI      =   "F"
                       MOVE    "black"     TO  K02-CONTKBN-STYLE
                       MOVE    "［複］"    TO  K02-CONTKBN-VALUE
                   END-IF
               WHEN    "1"
                   MOVE    "red"           TO  K02-CONTKBN-STYLE
                   MOVE    "［継］"        TO  K02-CONTKBN-VALUE
               WHEN    "2"
                   MOVE    "red"           TO  K02-CONTKBN-STYLE
                   MOVE    "［確］"        TO  K02-CONTKBN-VALUE
           END-EVALUATE
      *
      *    禁忌
           MOVE    SPA-GMN-TABOO (1)  TO  K02-TABOO
           PERFORM  VARYING   IDX     FROM    1   BY  1
                    UNTIL     IDX         >   SPA-TABOO-MAX
               MOVE    SPA-GMN-TABOO (IDX)    TO  K02-TABOO-ITEM
                                                             (IDX)
           END-PERFORM
           MOVE    SPA-TABOO-MAX       TO  K02-TABOO-COUNT
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
               IF     (SPA-GMN-KEI   (WRK-IDX) =   ZERO )
      *            院外処方の時、回数表示
                   IF     (SPA-COM-SRYKBN(WRK-IDX)
                                               =   "21"  OR "22"
                                               OR  "23" )   OR
                          (SPA-COM-SRYSYUKBN(WRK-IDX)
                                               =   "148"  OR
                                                   "149"  )
                       MOVE    " X "               TO  WRK-KAKERU
                   ELSE
                       MOVE    SPACE               TO  WRK-KAISU-H
                       MOVE    SPACE               TO  WRK-KAKERU
                   END-IF
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
           MOVE    WRK-SURYO-HENSYU    TO  K02-SURYO (IDX)
           MOVE    "user-font"         TO  K02-SURYO-STYLE(IDX)
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
               IF      WRK-IDX2            >   20
                   MOVE    08              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
      *    患者番号が空白の時、患者番号へ
           IF      K02-PTNUM       =  SPACE
               MOVE    00              TO  SPA-GMN-CUR
           END-IF
      *
      *    排他エラーの時、患者番号へ
           IF      SPA-ERRCD           =   "9999"
               MOVE    00              TO  SPA-GMN-CUR
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    08
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
           MOVE    "K02"               TO  SPA-MOTOPG
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
           MOVE    "K02"               TO  SPA-MOTOPG
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
                   MOVE     "よろしいですか。"
                                       TO  WRK-KIDMSG(25:)
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
               WHEN    "0120"
                   MOVE    "再診料（３月超）（５回以内）へ"
                                       TO  WRK-KIDMSG
                   MOVE    "振替えます。よろしいですか？"
                                       TO  WRK-KIDMSG(31:)
               WHEN    "0121"
                   MOVE    "入院中の患者です。入院画面へ遷移します。"
                                       TO  WRK-KIDMSG
                   MOVE    "よろしいですか？"
                                       TO  WRK-KIDMSG(41:)
               WHEN    "0122"
                   MOVE    "この科の中途終了データが存在します。"
                                       TO  WRK-KIDMSG
                   MOVE    "ＯＫで中途終了内容へ置換えます。"
                                       TO  WRK-KIDMSG(41:)
               WHEN    "0160"
                   MOVE    "包括検査で同じ検査があります。"
                                       TO  WRK-KIDMSG
                   MOVE    "ＯＫですべての同一検査を削除します。"
                                       TO  WRK-KIDMSG(31:)
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
               WHEN    "0113"
               WHEN    "0118"
               WHEN    "0119"
               WHEN    "0107"
               WHEN    "0122"
                   MOVE    "ＮＯ"            TO  KID1-B01
           END-EVALUATE
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "K02"               TO  SPA-MOTOPG
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
           MOVE    "K02"               TO  SPA-MOTOPG
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
           MOVE    "DBCLOSE"           TO  MCP-FUNC
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
           MOVE    "DBCLOSE"           TO  MCP-FUNC
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
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       980-SYUNOU-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       980-SYUNOU-READ-EXT.
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
