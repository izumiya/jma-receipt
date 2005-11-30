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
       PROGRAM-ID.                 ORCGJ04.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 会計照会 
      *  コンポーネント名  : 診療行為入力
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC-多々納   01/05/17  年齢計算をＪ０２へ
      * 02.00.00     MCC−多々納  01/05/22  入力コードをセットコードを表示等
      * 02.00.01     MCC-多々納   02/04/12  自費の修正
      * 02.00.02     NACL-多々納  02/07/29  パラメタファイルをＤＢへ
      * 02.00.03     NACL−多々納 02/09/19  インターフェースの変更
      * 02.00.04     NACL-多々納  02/11/20  自賠責器材追加
      * 02.00.05     NACL-多々納  02/12/02  食事の追加
      * 02.00.06     NACL-多々納  02/12/18  診療種別の判定追加
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    パラメタデータ
      *****SELECT  PARA-FILE       ASSIGN  "/var/tmp/J01PARA"
      *    SELECT  PARA-FILE       ASSIGN  FILE-NAME2
      *                            ORGANIZATION    IS  INDEXED
      *                            ACCESS  MODE    IS  DYNAMIC
      *                            RECORD  KEY     IS  PARA-KEY
      *                            FILE    STATUS  IS  STS-PARA.
      *
      *    ＳＰＡデータ
      *D   SELECT  SPASCR-FILE     ASSIGN  "J04SPASCR.TXT"
           SELECT  SPASCR-FILE     ASSIGN  FILE-NAME1
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-SPASCR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    パラメタデータ
      *FD  PARA-FILE.
      *01  PARA-REC.
      *    03  PARA-KEY.
      *********05  PARA-TERMID          PIC X(36).
      *        05  PARA-FILEMEI         PIC X(08).
      *        05  PARA-EDANUM          PIC 9(03).
      *    03  PARA-DATA-REC            PIC X(3500).
      *
      *    ＳＰＡパラメタデータ
       FD  SPASCR-FILE.
       01  SPA-DATA-REC            PIC X(30000).
      *
      *
       WORKING-STORAGE             SECTION.
      * ＳＰＡパラメタデータ
       01  FILE-NAME1.
           03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
           03  FILLER              PIC X(09)   VALUE   "J04SPASCR".
           03  FILE-TERMID         PIC X(64)   VALUE   SPACE.
           03  FILLER              PIC X(06)   VALUE   ".TXT".
      *
       01  FILE-NAME2.
           03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
           03  FILLER              PIC X(07)   VALUE   "J01PARA".
           03  FILE-TERMID2        PIC X(64)   VALUE   SPACE.
      *
       01  WRK-DATA-REC.
           03  WRK-DATA            PIC X(30000)
                                   OCCURS  10.
      *    ＳＰＡパラメタ
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
           COPY    "J01COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "J04SCR-SPA".
      *
      *  診療行為テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
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
           03  FLG-END2            PIC 9(01).
           03  FLG-SPASCR          PIC 9(01).
           03  FLG-SRYKBN          PIC 9(01).
           03  FLG-KNJBYO          PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-CHKMST          PIC 9(01).
           03  FLG-INPUTSET        PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
      *
           03  FLG-GMN             PIC 9(01).
           03  FLG-KANA            PIC 9(01).
           03  FLG-SUJI            PIC 9(01).
           03  FLG-TOROKU          PIC X(01).
           03  FLG-SYORI           PIC X(01).
           03  FLG-SST             PIC 9(01).
           03  FLG-KINKI           PIC 9(01).
           03  FLG-YAKZAI          PIC 9(01).
      *
           03  FLG-NAIYAK          PIC X(01).
           03  FLG-TONPUK          PIC X(01).
           03  FLG-GAIYAK          PIC X(01).
           03  FLG-ARI             PIC X(01).
      *----(01.00.01) LINE ADD START ----------------------------------
      *    セット処理有無
           03  FLG-SETSYORI        PIC 9(01).
           03  FLG-SET             PIC 9(01).
           03  FLG-YAKUSOKU        PIC 9(01).
      *----(01.00.01) LINE ADD END   ----------------------------------
      *    漢字短縮
           03  FLG-TANSYUKU        PIC 9(01).
      *
           03  FLG-MAE             PIC 9(01).
      *    自費入力
           03  FLG-JIHI           PIC 9(01).
      *
           03  FLG-OK             PIC 9(01).
           03  FLG-OK2            PIC 9(01).
           03  FLG-UPD            PIC 9(01).
           03  FLG-ERR            PIC 9(01).
      *    検索種別
           03  FLG-KOUI            PIC 9(01).
      *    空白入力
           03  FLG-KUHAKU          PIC 9(01).
           03  FLG-INS             PIC 9(01).
      *
           03  FLG-CHK-OK          PIC 9(01).
      *
           03  FLG-FIRUMU          PIC 9(01).
           03  FLG-SYUNOU-ERR      PIC 9(01).
      *
           03  FLG-NAIFUTAN        PIC 9(01).
           03  FLG-NAIBUN          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDS                 PIC 9(04).
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
           03  IDX-STR             PIC 9(04).
      *
           03  IDX-SIN             PIC 9(04).
           03  IDX-ERR             PIC 9(04).
      *
           03  IDX-P               PIC 9(04).
           03  IDX-K               PIC 9(04).
           03  IDX-K1              PIC 9(04).
           03  IDX-C               PIC 9(04).
           03  IDX-X               PIC 9(04).
           03  IDX-S2              PIC 9(04).
           03  IDX-IP              PIC 9(04).
           03  IDX-SST             PIC 9(04).
      *
           03  IDX-YAKUSOKU        PIC 9(04).
           03  IDX-YAK             PIC 9(04).
           03  IDX-1               PIC 9(04).
           03  IDX-SET             PIC 9(04).
           03  IDX-D2              PIC 9(02).
           03  IDX-D1              PIC 9(02).
      *
           03  IDX-ATO             PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
           03  IDX-J98             PIC 9(04).
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-MATUYMD         PIC X(08).
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
           03  WRK-MOJISU          PIC 9(04).
           03  WRK-FREE            PIC 9(04).
      *
           03  WRK-KEI             PIC 9(08).
           03  WRK-IDX             PIC 9(03).
      *    入力項目名編集用 
           03  WRK-IDX2            PIC 9(02).
           03  WRK-WIDGET          PIC X(20).
           03  WRK-IN-MAP-IDX      PIC 9(02).
           03  WRK-STR             PIC 9(04).
      *Ｋ９８カウント+
           03  WRK-CNT-ENT         PIC 9(06).
      *
      *D   03  WRK-MAP-INDEX       PIC 9(03).
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
           03  WRK-KAISU-X.
               05  WRK-KAISU-Z     PIC ZZZZ.
           03  WRK-BUNGSU-X.
               05  WRK-BUNGSU-Z    PIC ZZZZZZZZ.
           03  WRK-KEICOM-X.
               05  WRK-KEICOM-Z    PIC ZZZZZZZ9.
           03  WRK-KEICOMS-X.
               05  WRK-KEICOMS-Z   PIC ZZZZ9V99.
           03  WRK-GOKEI-X.
               05  WRK-GOKEI-Z     PIC ZZZZZZZZ.
           03  WRK-NOZ             PIC ZZ9.
      *
           03  WRK-SURYO-H         PIC X(09).
           03  WRK-KAISU-H         PIC X(04).
           03  WRK-BUNGSU-H        PIC X(09).
           03  WRK-KAKERU          PIC X(01).
      *
           03  WRK-SYOKEIS         PIC 9(08)V99.
           03  WRK-SYOKEIS2        PIC 9(08)V99.
           03  WRK-SYOKEI          PIC 9(08).
      *
           03  WRK-ZAIKEI          PIC 9(08)V99.
           03  WRK-GAIKEI          PIC 9(08)V99.
           03  WRK-ZAITEN          PIC 9(08).
           03  WRK-GAITEN          PIC 9(08).
           03  WRK-KIZKEI          PIC 9(08).
           03  WRK-KIZTEN          PIC 9(08).
           03  WRK-FIRTEN          PIC 9(08).
           03  WRK-MAISU           PIC 9(08).
           03  WRK-KEIMAI          PIC 9(08).
           03  WRK-TENSUKEI        PIC 9(08).
           03  WRK-TENSU1          PIC 9(08).
           03  WRK-KAISU           PIC 9(04).
      *    分画数フラグ
           03  FLG-BUIKBN          PIC X(01).
      *
           03  WRK-KINGAK          PIC 9(08).
           03  WRK-KAISUKEI        PIC 9(08).
           03  WRK-INPUTCD         PIC X(10).
           03  WRK-SURYO           PIC 9(07)V9(03).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(07).
               05  WRK-SURYO-S2    PIC 9(03).
           03  WRK-BUNGSU          PIC 9(08).
           03  WRK-GOKEI           PIC 9(08).
           03  WRK-SRYSYUKBN       PIC X(03).
           03  WRK-SRYSYUKBNMEI    PIC X(40).
           03  WRK-SRYKBN          PIC X(02).
           03  WRK-SRYKBN-1        PIC X(02).
      *
           03  WRK-SURYOKEI        PIC 9(07)V9(03).
           03  WRK-SRYCDKEI        PIC 9(14).
           03  WRK-MEISAISU        PIC 9(07).
           03  WRK-ZAIKAISU        PIC 9(03).
           03  WRK-GOKEI-9         PIC 9(10).
           03  WRK-SRYCD-9         PIC 9(09).
      *
           03  WRK-TANINAME        PIC X(04).
      *
           03  WRK-MEINUM          PIC 9(04).
           03  WRK-RENNUM          PIC 9(04).
           03  WRK-ZAINUM          PIC 9(08).
           03  WRK-H-ZAINUM        PIC 9(08).
      *
           03  WRK-PARAEDABAN      PIC 9(04).
      *D   03  WRK-ZAIIDX          PIC 9(04).
           03  WRK-H-SRYSYUKBN     PIC X(03).
           03  WRK-H-SRYKBN        PIC X(02).
      *    診療会計用集計
           03  WRK-WKSRY-AREA.
               05  WRK-SYUTEN1             PIC  9(07).
               05  WRK-SYUTEN2             PIC  9(07).
               05  WRK-YKZTEN              PIC  9(07).
               05  WRK-KIZAITEN            PIC  9(07).
               05  WRK-JIHIMONEY           PIC  9(08).
      *     一部負担金計算用
           03  WRK-S01-TENSU       PIC 9(08).
      *
           03  WRK-NAICHORYO       PIC 9(07).
           03  WRK-GAICHORYO       PIC 9(07).
           03  WRK-SYOHORYO        PIC 9(07).
      *
           03  WRK-MOJI            PIC X(01).
           03  WRK-ATAI            PIC X(08).
      *   注射・点滴用チェック
           03  FLG-MADOKU          PIC X(01).
           03  FLG-SEIBETU         PIC X(01).
           03  WRK-SRYCD           PIC X(09).
           03  WRK-CHUSYA          PIC 9(09).
           03  IDX-S               PIC 9(04).
           03  IDX-SW              PIC 9(04).
      *   きざみ値行為再計算用
           03  WRK-KEI-TEN         PIC 9(08)V9(03).
           03  WRK-KEI-TEN1        PIC 9(08)V9(03).
           03  WRK-KEI-TEN2        PIC 9(08).
      *    警告済フラグ保存
           03  WRK-KNKFLG          PIC X(01).
           03  WRK-KNKUMU          PIC X(01).
           03  WRK-KEIFLG          PIC X(01).
           03  WRK-KZMFLG          PIC X(01).
           03  WRK-SDOFLG          PIC X(01).
           03  WRK-CHKFLG          PIC X(01).
           03  WRK-TIMEFLG         PIC X(01).
      *    入力位置
           03  WRK-FLG-IN              PIC X(01).
      *    入力セット処理
           03  WRK-SETCD           PIC X(06).
      *    薬剤麻毒加算用ワーク
           03  WRK-MADOKU-SRYCD    PIC X(09).
           03  WRK-MADOKU-TENSU    PIC 9(05)V9(03).
      *Ｋ９８、Ｋ９９戻りＩＤＸ保存
           03  WRK-HZN-IDX         PIC 9(04).
      *
           03  WRK-GMN-IDX         PIC 9(04).
      *
           03  WRK-MAX-IDX         PIC 9(04).
      *
      *    自費用
           03  WRK-TENTTLKBN       PIC 9(03).
           03  WRK-HOZ-KEI         PIC 9(08).
      *
      *    約束セット用
           03  WRK-IDX-YAKUSOKU.
               05  WRK-IDX-YAK         PIC 9(04)    OCCURS  20.
      *    入力位置判定用
           03  CNT-IN              PIC 9(02).
           03  WRK-IN-SURYO        PIC 9(02).
           03  WRK-IN-IDX          PIC 9(04).
           03  WRK-IN-IDX2         PIC 9(04).
      *
           03  WRK-PATH            PIC X(64).
      *
           03  WRK-DAY                 PIC 9(03).
      *    入力時表示用
           03  WRK-ERR-IDX             PIC 9(04).
           03  IDX-INCNT               PIC 9(04).
           03  FLG-NYURYOKU            PIC 9(01).
           03  WRK-HEN-INPUTCD         PIC X(22).
           03  FLG-IN-ATAI             PIC 9(01).
      *    検査回数
           03  WRK-KENSA-CNT       PIC 9(04).
           03  WRK-KENSA-MAX       PIC 9(04).
           03  WRK-CHK-MAX         PIC 9(04).
           03  IDX-KEN             PIC 9(04).
           03  IDX-KEN2            PIC 9(04).
           03  FLG-KENSA           PIC 9(01).
      *    画面制御
           03  WRK-PUTTYPE         PIC X(08).
      *
           03  WRK-JIDMSG              PIC X(80).
      *
           03  WRK-ERRCD           PIC X(04).
      *
      *    入力値の入力判定用
           03  WRK-HZN-ATAI-AREA.
               05  WRK-HZN-ATAI1           PIC X(10).
               05  WRK-HZN-ATAI2           PIC X(10).
               05  WRK-HZN-ATAI3           PIC X(10).
               05  WRK-HZN-ATAI4           PIC X(10).
      *    入力名称
           03  WRK-MEISYO-G.
               05  WRK-MEIH                PIC X(02).
               05  WRK-MEISYO              PIC X(78).
           03  WRK-MEISYO-GHZN.
               05  WRK-MEIHHZN             PIC X(02).
               05  WRK-MEISYOHZN           PIC X(78).
           03  IDX-MEI                 PIC 9(04).
      *
      *    診療種別
           03  WRK-MAE-SRYSYUKBN          PIC X(03).
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
               05  WRK-H-SURYO        PIC X(09).
               05  WRK-H-TANINAME     PIC X(04).
               05  WRK-H-F1           PIC X(02).
               05  WRK-H-ZAIKEI       PIC X(08).
      *
           03  WRK-KEI-X.
               05  WRK-KEI-Z       PIC Z(08).
           03  WRK-KEI-H           PIC X(08).
           03  WRK-ZAIKEI-H        PIC X(40).
           03  WRK-ZZ              PIC ZZZ.
      *
      *    診療行為内容比較用テーブル
      *01  TBL-AREA.
      *    03  TBL-MAE-AREA.
      *        05  TBL-MAE-REC     OCCURS   30.
      *            07  TBL-MAE-SRYCD       PIC  X(09).
      *            07  TBL-MAE-SRYSURYO    PIC  9(05)V9(03).
      *            07  TBL-MAE-SRYKAISU    PIC  9(03).
      *            07  TBL-MAE-INPUTCOMENT PIC  X(40).
      *            07  TBL-MAE-ATAI-G.
      *                09  TBL-MAE-ATAI    PIC  X(08)  OCCURS  3.
      *    03  TBL-ATO-AREA.
      *        05  TBL-ATO-REC     OCCURS   30.
      *            07  TBL-ATO-SRYCD       PIC  X(09).
      *            07  TBL-ATO-SRYSURYO    PIC  9(05)V9(03).
      *            07  TBL-ATO-SRYKAISU    PIC  9(03).
      *            07  TBL-ATO-INPUTCOMENT PIC  X(40).
      *            07  TBL-ATO-ATAI-G.
      ******           09  TBL-ATO-ATAI    PIC  X(08)  OCCURS  3.
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
      *01  WRK-GMNGYO-AREA.
      *    03  WRK-MAP-MAX         PIC 9(04).
      *    03  WRK-MAP-CUR         PIC 9(04).
      *    03  WRK-MAP-CUR2        PIC 9(04).
      *****03  WRK-PAGE            PIC 9(04).
      *
       01  WRK-MOJI-AREA.
           03  FLG-SP              PIC 9(02).
           03  FLG-AST             PIC 9(02).
           03  FLG-JKNKSN          PIC 9(02).
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
      *
           03  WRK-HENKAN-MAE      PIC X(20).
           03  WRK-HENKAN-ATO      PIC X(20).
           03  IDH1                PIC 9(04).
           03  IDH2                PIC 9(04).
           03  IDX-D               PIC 9(04).
      *
      *    検査　判断グループ
       01  WRK-KENSA-AREA.
           03  WRK-KENSA-TBL                       OCCURS   20.
               05  WRK-KENSAGRP        PIC 9(02).
               05  WRK-KEN-SRYCD       PIC X(09).
               05  WRK-KEN-TENSU       PIC 9(05)V9(03).
           03  WRK-KENIDX              PIC 9(04).
      *
      *    禁忌チェック用テーブル
       01  WRK-KNKCHK-AREA.
           03  WRK-KNK-TBL             OCCURS   30.
               05  WRK-KNK-TOUMEI          PIC X(40). 
               05  WRK-KNK-TOUSRYCD        PIC X(09). 
               05  WRK-KNK-TBL2A.
                   07  WRK-KNK-TBL2            OCCURS   30.
                       09  WRK-KNK-KNKMEI          PIC X(40).
                       09  WRK-KNK-SRYYM         PIC X(06).
                       09  WRK-KNK-SRYYMW        PIC X(07).
                       09  WRK-KNK-SRYCD           PIC X(09).
      *
       01  WRK-SORT-AREA.
           03  WRK-SRT-TBL2A.
               05  WRK-SRT-TBL2            OCCURS   30.
                   07  WRK-SRT-KNKMEI          PIC X(40).
                   07  WRK-SRT-SRYYM         PIC X(06).
                   07  WRK-SRT-SRYYMW        PIC X(07).
                   07  WRK-SRT-SRYCD           PIC X(09).
           03  WRK-KEY-SRYYM             PIC X(06).
           03  WRK-KEY-SRYCD               PIC X(09).
      *
      *    コメントの入力値用
       01  WRK-INPUT-SET-AREA.
           03  WRK-INPUT-TBL.
               05  WRK-INPUT-SET       PIC 9(03)   OCCURS  8.
           03  WRK-INPUT-TBL-R         REDEFINES   WRK-INPUT-TBL.
               05  WRK-INPUT-TBL2          OCCURS  4.
                   07  WRK-INPUT-ICHI      PIC 9(03).
                   07  WRK-INPUT-KETA      PIC 9(03).
           03  WRK-KETA                PIC 9(02).
           03  WRK-KETA2               PIC 9(02).
           03  WRK-ICHI                PIC 9(02).
           03  WRK-ICHI2               PIC 9(02).
           03  IDX-KNJ                 PIC 9(02).
           03  IDX-KNJ2                PIC 9(04).
           03  WRK-ZZZ9-X.
               05  WRK-ZZZ9                PIC ZZZZ9.
           03  WRK-KANJI.
               05  WRK-KANJI-X             PIC X(02)
                                           OCCURS   5.
      *
      *    画面ＳＰＡ領域ワーク
       01  WRK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *
      *    保存ＳＰＡ領域ワーク
       01  WRK-HZN-SCR-AREA.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-HZN-//.
      *
       01  WRK-HOZ-IDX             PIC 9(04).
      *
      *    ワークテーブル２(一時保存用）
      *01  WRK-HOZ-AREA.
      *    03  WRK-HGMN-TBL            OCCURS   200.
      *        05  WRK-HGMN-TBLREC.
      *            07  WRK-HGMN-DATA   PIC X(150).
      *        05  WRK-HCOM-TBLREC.
      *            07  WRK-HCOM-DATA   PIC X(800).
      *****03  WRK-HOZ-IDX             PIC 9(04).
      *
      *
      *   日本語数字
       01  TBL-SUJI-AREA.
           03  FILLER          PIC X(20)   VALUE
              "１２３４５６７８９０".
       01  TBL-SUJI-R          REDEFINES   TBL-SUJI-AREA.
           03  TBL-SUJI        PIC X(02)   OCCURS   10.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
      *D   COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1006.INC".
      *
           COPY    "CPSK1010.INC".
      *
           COPY    "CPSK1008.INC".
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
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *     パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *    附加情報
       01  WKSRYACTPLUS-REC.
           COPY    "CPWKSRYACTPLUS.INC".
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
      *    点数編集パラメタ
           COPY    "CPORCSC92.INC".
      *
      *    剤分離パラメタ
           COPY    "CPORCSC93.INC".
      *
      *    画面再編集パラメタ
           COPY    "CPORCSC94.INC".
      *
      *    エラーメッセージパラメタ
           COPY    "CPORCSC96.INC".
      *
      *    入力項目変換パラメタ
           COPY    "CPORCSC97.INC".
      *
      *   算定履歴更新サブ
           COPY    "CPORCSSANTEI.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    診療会計剤チェックパラメタ
           COPY    "CPORCSCZAICHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    内分泌負荷試験更新パラメタ
           COPY    "CPORCSSANFUKA.INC".
      *
      *    レセプト作成管理マスタ更新サブ
           COPY    "CPORCSRECEUPD.INC".
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
           COPY    "J02.INC".
           COPY    "J021.INC".
           COPY    "J022.INC".
           COPY    "J023.INC".
           COPY    "J024.INC".
           COPY    "J025.INC".
           COPY    "J026.INC".
           COPY    "J03.INC".
           COPY    "J04.INC".
           COPY    "J041.INC".
           COPY    "J042.INC".
           COPY    "J98.INC".
           COPY    "J99.INC".
           COPY    "JERR.INC".
           COPY    "JID1.INC".
           COPY    "JID2.INC".
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
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-J04
           MOVE    SPA-WORK        TO  SPA-J01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
      *    診療種別テーブルセット
           PERFORM 1001-SRYKBN-SET-SEC
      *
      *    画面ＳＰＡ読み込み
           PERFORM 1002-J04SPA-SET-SEC
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
               PERFORM 1003-J04SPA-OUT-SEC
           END-IF
      *
           MOVE    SPA-J01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-J04         TO  SPA-FREE
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
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "JERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               IF      SPA-KEI-FLG         =   1
      *            終了
                   MOVE    "JOIN"          TO  WRK-PUTTYPE
                   PERFORM 410021-END-SYORI-SEC
               ELSE
                   PERFORM 5001-MAPCUR-SEC
               END-IF
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END             =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
               MOVE   SPACE                TO  MCP-PUTTYPE
               MOVE   "J04"                TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
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
      *    ＳＰＡパラメタデータ読込処理
      *****************************************************************
       1002-J04SPA-SET-SEC             SECTION.
      *
           MOVE    SPA-TERMID          TO  FILE-TERMID
      *
           OPEN    INPUT               SPASCR-FILE
           IF      STS-SPASCR     NOT =   ZERO
               GO      TO      1002-J04SPA-SET-EXT
           END-IF
      *
           INITIALIZE          SPA-SCR-REC
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
       1002-J04SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡパラメタデータＷＲＩＴＥ処理
      *****************************************************************
       1003-J04SPA-OUT-SEC             SECTION.
      *
           MOVE    SPA-TERMID          TO  FILE-TERMID
      *
           OPEN    OUTPUT              SPASCR-FILE
           IF      STS-SPASCR     NOT =   ZERO
               DISPLAY "J04SPASCR OUT OPEN ERR:"   STS-SPASCR "."
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
                   DISPLAY "J04SPASCR OUT WRITE ERR:"   STS-SPASCR "."
                END-IF
           END-PERFORM
      *
           CLOSE                       SPASCR-FILE
           .
       1003-J04SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-GMN
      *
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
      *
      *        元画面表示
               IF      SPA-MOTOPG(1:3)     =   "Y01"  OR  "U01" 
                   MOVE    LNK-COMMON      TO  SPA-KYOTUKEY
                   MOVE    LNK-SCRSPA      TO  SPA-J04
                   MOVE    SPA-WORK        TO  SPA-J01KYOUTU
               END-IF
      *        予約とエラー表示の時、そのまま画面表示へ
               IF      SPA-MOTOPG(1:3)     =   "Y01"  OR
                                               "U01"
                   GO      TO      300-SCREEN-EXT
               END-IF
           END-IF
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "J99"
                   IF      SPA-J99-DATA    NOT =   SPACE
                       MOVE    SPA-COM-GYOU(SPA-GMN-IDX)
                                                   TO  WRK-IN-MAP-IDX
                       MOVE    SPA-GMN-IDX         TO  WRK-STR
                       MOVE    SPA-J99-DATA        TO  SPA-GMN-INPUTCD
                                                          (SPA-GMN-IDX)
                       MOVE    "1"                 TO  SPA-COM-IN
                                                         (SPA-GMN-IDX)
      *
      *                入力コード・名称チェック処理
                       MOVE    SPACE           TO  FLG-TOROKU
                       PERFORM 410112-KOUI-SPACHK-SEC
                       MOVE    1                   TO  FLG-GMN
                   ELSE
      *                画面ＳＰＡチェック処理
                       MOVE    SPACE           TO  FLG-TOROKU
                       PERFORM 410112-KOUI-SPACHK-SEC
                       MOVE    2                   TO  FLG-GMN
                   END-IF
               WHEN    "J98"
                   IF      SPA-J98-SRYCD-G   NOT =   SPACE
                       PERFORM 3011-J98-SET-SEC
                       MOVE    1               TO  FLG-GMN
                   ELSE
                       MOVE    "1"             TO  SPA-COM-CUR
                                                      (SPA-GMN-IDX)
                       MOVE    2               TO  FLG-GMN
                   END-IF
               WHEN    "J041"
      *             禁忌一覧 ＯＫの時、更新処理へ
                   IF      SPA-J041-CHK        =   1
      *                画面制御
                       MOVE    "JOIN"          TO  WRK-PUTTYPE
      *                更新処理
                       PERFORM 41002-KOUSIN-SYORI-SEC
                   END-IF
                   MOVE    2               TO  FLG-GMN
               WHEN    "J042"
      *            禁忌画面より
                   MOVE    2                   TO  FLG-GMN
      *            確認画面より
               WHEN    "JID1"
                   PERFORM 3003-JID1-SET-SEC
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
           MOVE    SPACE           TO  SPA-JIDCD
           MOVE    SPACE           TO  SPA-JID1-FLG
      *
           IF      FLG-GMN         NOT =   ZERO
      *        入力コード処理へ
      ******** IF     (SPA-MOTOPG(1:3)     =   "J98"  OR  "J99" )  AND
      *        IF     (SPA-MOTOPG(1:3)     =   "J99" )  AND
      *               (FLG-GMN             =   1     )
      *            MOVE    SPA-MAP-IDX     TO  WRK-IDX2
      *            MOVE    SPA-GMN-IDX     TO  WRK-HZN-IDX
      *            PERFORM 41011-KOUI-HEN-SEC
      *********END-IF
               GO      TO      300-SCREEN-EXT
           END-IF
      *
           INITIALIZE              SPA-J04-AREA
           INITIALIZE              SPA-SCR-REC
      *
      *    チェック用月内診療行為セット
           PERFORM                 320-SPA-CHKSET-SEC
      *
      *    画面用スパ編集処理
           PERFORM                 310-SPA-SET-SEC
      *
           MOVE    1               TO  SPA-GMN-PAGE
           MOVE    1               TO  SPA-GMN-CUR
           MOVE    1               TO  SPA-MAP-IDX
      *
      *D   MOVE    1               TO  SPA-GMN-IDXST
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     確認画面よりの処理
      *****************************************************************
       3003-JID1-SET-SEC                  SECTION.
      *
           EVALUATE    SPA-JIDCD
               WHEN    "0101"
      *            戻るの確認
                   IF      SPA-JID1-FLG        =   "OK"
      *                画面制御
                       MOVE    "JOIN"          TO  WRK-PUTTYPE
      *                更新処理
                       PERFORM 41002-KOUSIN-SYORI-SEC
      *
                       IF      SPA-ERRCD       NOT =   SPACE
                           PERFORM 510-ERRSET-SEC
                       END-IF
                   ELSE
                       MOVE    1                   TO  SPA-GMN-CUR
                   END-IF
                   MOVE    SPACE           TO  SPA-JIDCD
                   MOVE    SPACE           TO  SPA-JID1-FLG
      *
               WHEN    "0102"
      *            外用の確認
                   MOVE    SPACE           TO  SPA-JIDCD
                   IF      SPA-JID1-FLG        =   "OK"
      *                更新処理
                       PERFORM 4100-TOROKU-SEC
      *
                       IF      SPA-JIDCD       NOT =   SPACE
                           PERFORM 520-JIDSET-SEC
                       END-IF
                   ELSE
                       MOVE    1                   TO  SPA-GMN-CUR
                   END-IF
                   MOVE    SPACE           TO  SPA-JID1-FLG
      *
           END-EVALUATE
           .
       3003-JID1-SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
      *    年齢計算（最初の対象日の年齢）
      *    MOVE    SPA-BIRTHDAY        TO  WRK-YMDS1
      *    MOVE    SPA-SRYYMD(1:6)     TO  WRK-YMDS2(1:6)
      *    MOVE    01                  TO  WRK-DDS2
      *    COMPUTE SPA-NAI-NENREI-YY   =   WRK-YYS2
      *                                -   WRK-YYS1
      *    IF      WRK-YMDS1(5:4)      >   WRK-YMDS2(5:4)
      *        COMPUTE SPA-NAI-NENREI-YY   =   SPA-NAI-NENREI-YY
      *                                    -   1
      *        COMPUTE SPA-NAI-NENREI-MM   =  (WRK-MMS2  +  12 )
      *                                    -   WRK-MMS1
      *    ELSE
      *        COMPUTE SPA-NAI-NENREI-MM   =   WRK-MMS2
      *                                    -   WRK-MMS1
      *    END-IF
      *    該当月で６歳になる時、年齢フラグを立てる
      *    IF     (SPA-NAI-NENREI-YY       =   06  )  AND
      *           (WRK-YMDS1(5:2)          =   WRK-YMDS2(5:2))
      *        MOVE    1                   TO  SPA-FLG-NENREI
      *    END-IF
      *
      *    診療年月の末日算定
      *    INITIALIZE                      STS-AREA-DAY
      *    INITIALIZE                      LNK-DAY7-AREA
      *    MOVE    "71"                TO  LNK-DAY7-IRAI
      *    MOVE    SPA-SRYYMD(1:6)     TO  LNK-DAY7-YM
      *    CALL    "ORCSDAY"           USING   STS-AREA-DAY
      *                                        LNK-DAY7-AREA
      *    MOVE    LNK-DAY7-KOYOMI     TO  WRK-MATUYMD
      *
      *    乳幼児チェック
      *    IF      SPA-NAI-NENREI-MM   =   ZERO  OR  1
      *
      *        年齢期間算定
      *        INITIALIZE                      STS-AREA-DAY
      *        INITIALIZE                      LNK-DAY5-AREA
      *        MOVE    "51"                TO  LNK-DAY5-IRAI
      *        MOVE    SPA-BIRTHDAY        TO  LNK-DAY5-START
      *        MOVE    WRK-MATUYMD         TO  LNK-DAY5-END
      *        CALL    "ORCSDAY"           USING   STS-AREA-DAY
      *                                            LNK-DAY5-AREA
      *        IF      LNK-DAY5-NISSU2     >   28
      *            MOVE    1                   TO  SPA-FLG-NENREI
      *            IF      LNK-DAY5-NISSU2     <=  31
      *                MOVE    LNK-DAY5-NISSU2     TO  SPA-NAI-NENREI-DD
      *                MOVE    ZERO                TO  SPA-NAI-NENREI-MM
      *            END-IF
      *        END-IF
      *****END-IF
      *
           MOVE    SPACE               TO  SYS-1006-REC
           INITIALIZE                      SYS-1006-REC
           MOVE    "1006"              TO  SYS-1006-KANRICD
           MOVE    "1"                 TO  SYS-1006-KBNCD
           MOVE    ZERO                TO  SYS-1006-STYUKYMD
           MOVE    ALL "9"             TO  SYS-1006-EDYUKYMD
      *    システム管理検索（施設基準）
           MOVE    SYS-1006-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           MOVE    SPA-SRYYMD          TO  ORC-DBYMD
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1006-REC
               INITIALIZE                  SYS-1006-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1006-REC
               IF      SYS-1006-SSTKIJUN (121) =   1
                   MOVE    1               TO  SPA-SYONIKA
               END-IF
      *        時間外特例医療機関
               IF      SYS-1006-SSTKIJUN (011) =   1
                   MOVE    1               TO  SPA-JIKANTOKU
               END-IF
      *
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    システム管理検索（チェック機能制御情報）
           MOVE    SPACE               TO  SYS-1008-REC
           INITIALIZE                  SYS-1008-REC
           MOVE    "1008"              TO  SYS-1008-KANRICD
           MOVE    "*"                 TO  SYS-1008-KBNCD
           MOVE    SYS-1008-REC        TO  MCPDATA-REC
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
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1008-REC
               INITIALIZE                  SYS-1008-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1008-REC
      *
      *        時間外加算特例乳幼児
               IF      SYS-1008-JIKANTOKU-NYU-FLG  =   SPACE
                   MOVE    ZERO            TO  SPA-JIKANTOKU-NYU
               ELSE
                   MOVE    SYS-1008-JIKANTOKU-NYU-FLG
                                           TO  SPA-JIKANTOKU-NYU
               END-IF
           ELSE
               MOVE    ZERO                TO  SPA-JIKANTOKU-NYU
           END-IF
      *
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  SPA-GMN-IDX
      *
      *
           MOVE    "2"             TO  FLG-TOROKU
      *
      *    診療行為マスターを編集する
           PERFORM 3103-SINKOU-HEN-SEC
      *    入力コードの画面用再編集
           PERFORM VARYING     IDX-1   FROM    1   BY  1
                   UNTIL      (IDX-1   >   200 )  OR
                              (SPA-GMN-INPUTCD(IDX-1)  =   SPACE  AND
                               SPA-COM-INPUTCD(IDX-1)  =   SPACE )
               EVALUATE    SPA-GMN-INPUTCD(IDX-1)(1:1)
                   WHEN    "."
                       CONTINUE
      *            約束セット名称編集
                   WHEN    "S"
                       INITIALIZE                  WRK-MOJI-AREA
                       MOVE    IDX-1               TO  IDX
      *                入力コードの画面用再編集
                       PERFORM 3101-INPUTCD-HEN-SEC
                       MOVE    IDX-1               TO  SPA-GMN-IDX
      *                約束名称編集
                       MOVE    6                   TO  WRK-CD-MCNT
                       MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:6)
                                                   TO  WRK-INPUTCD
                       PERFORM 41011921-INPSET-MEI-SEC
                       MOVE    SPACE               TO  SPA-COM-INPUTCD
                                                        (SPA-GMN-IDX)
      *            その他
                   WHEN    OTHER
      *                入力コードの画面用再編集
                       INITIALIZE                  WRK-MOJI-AREA
                       MOVE    IDX-1               TO  IDX
                       PERFORM 3101-INPUTCD-HEN-SEC
                       MOVE    IDX-1               TO  SPA-GMN-IDX
                       IF      SPA-ERRCD           =   SPACE
                           MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                                   TO  SPA-MAE-INPUTCD
                                                          (SPA-GMN-IDX)
                       END-IF
               END-EVALUATE
           END-PERFORM
      *
      *    診療行為、計計算処理、編集
           MOVE    "2"             TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE           TO  FLG-TOROKU
      *
      *    カーソルの位置を最初の行へ
      *****MOVE    "1"                 TO  SPA-COM-CUR (01)
      *
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧画面（Ｋ９８）ＯＫ処理
      *****************************************************************
       3011-J98-SET-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-IN-MAP-IDX
           MOVE    SPA-GMN-IDX         TO  WRK-STR
      *
           PERFORM VARYING     IDX-J98 FROM    1   BY  1
                   UNTIL      (IDX-J98             >   5   )  OR
                              (SPA-J98-SRYCD(IDX-J98)  =   SPACE )
      *
               MOVE    "1"                 TO  SPA-COM-IN (SPA-GMN-IDX)
      *
               IF      IDX-J98             =   1
                   MOVE    SPA-J98-SRYCD(IDX-J98)
                                           TO  SPA-GMN-INPUTCD
                                                       (SPA-GMN-IDX)
               ELSE
                   ADD     1               TO  SPA-GMN-IDX
                   PERFORM 41014-GYOINSN-SEC
                   MOVE    SPA-J98-SRYCD(IDX-J98)
                                           TO  SPA-GMN-INPUTCD
                                                       (SPA-GMN-IDX)
               END-IF
           END-PERFORM
      *
      *    入力コード・名称チェック処理
           MOVE    SPACE           TO  FLG-TOROKU
           PERFORM 410112-KOUI-SPACHK-SEC
      *
           .
       3011-J98-SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *   診療行為マスタより編集処理
      *****************************************************************
       3103-SINKOU-HEN-SEC              SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  FLG-YAKZAI
           MOVE    ZERO                TO  FLG-FIRUMU
           MOVE    ZERO                TO  SPA-CHOKITOFLG
      *
           INITIALIZE                      SRYACT-REC
           MOVE    SPA-HOSPID          TO  SRY-HOSPID
           MOVE    SPA-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SRY-PTID
           MOVE    SPA-J01-SRYKA       TO  SRY-SRYKA
           MOVE    SPA-J02-SRYYMD(1:6) TO  SRY-SRYYM
           MOVE    SPA-ZAINUM          TO  SRY-ZAINUM
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
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           MOVE    ZERO                TO  FLG-YAKUSOKU
      *
           PERFORM
               UNTIL    (FLG-SRYACT        =   1          )
      *
               PERFORM VARYING     IDS     FROM    1   BY  1
                       UNTIL      (IDS             >   5    )  OR
                                  (SRY-SRYCD (IDS) =   SPACE)
                   PERFORM 31011-SPA-HEN-SEC
      *            薬剤の有無
                   IF      SRY-SRYCD (IDS)(1:1)    =   "6"
                       MOVE    1               TO  FLG-YAKZAI
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
      *    薬剤で点数がゼロの時、院外処方分とする
           IF      (IDX2                   >   ZERO  )  AND
                   (SPA-COM-SRYKBN(IDX2)   =   "21"  OR  "22"
                                           OR  "23") AND
                   (FLG-YAKZAI             =   1     )  AND
                   (SPA-ZAITEN             =   ZERO  )
               MOVE    1                   TO  SPA-NAI-INGAIKBN
           END-IF
      *
      *    剤の最後に回数入力をセット
      *D   IF      IDX2                >   ZERO
      *D       MOVE    "1"                 TO  SPA-COM-KAIFLG(IDX2)
      *D   END-IF
      *
           MOVE    IDX2                TO  SPA-GMN-MAX
      *
           INITIALIZE                  SPA-J04-TEISEI-AREA
      *
      *    残量廃棄の判定
           PERFORM VARYING     IDX-1 FROM    1   BY  1
                   UNTIL      (IDX-1     >   200     )  OR
                              (IDX-1     >   SPA-GMN-MAX )
      *        残量廃棄の判定
               IF     (SPA-COM-SRYKBN (IDX-1)(1:1)  =   "3" )  AND
                      (SPA-COM-INPUTCD(IDX-1)(1:1)  =   "6" )
                   PERFORM 31034-HAIKI-CHKI-SEC
               END-IF
      *        展開時の内容編集（内分泌負荷試験用）
               IF     (SPA-COM-INPUTCD(IDX-1)(1:1) =   "1" ) AND
                      (SPA-COM-SANTEIRRKKBN (IDX-1)   =   1   )
                   ADD     1                   TO  IDX-S
                   MOVE    SPA-COM-INPUTCD(IDX-1)  TO  SPA-J04-TEI-SRYCD
                                                       (IDX-S)
                   MOVE    1                   TO  SPA-J04-TEI-KAISU
                                                       (IDX-S)
               END-IF
           END-PERFORM
      *    自費の時展開時なし
           IF      SPA-COM-SRYKBN(1)   =   "96"  OR  "95"
               INITIALIZE                  SPA-J04-TEISEI-AREA
           END-IF
      *    画像診断のフィルム判定
           PERFORM VARYING     IDX-1   FROM    1   BY 1
                   UNTIL       IDX-1   >   IDX2
               IF     (SPA-COM-INPUTCD(IDX-1)(1:1)   =   "1" )  AND
                      (SPA-COM-DATAKBN (IDX-1)       =   "1" )  AND
                      (SPA-COM-KZMCOMPSIKIBETU(IDX-1)  NOT =  ZERO)  AND
                      (SPA-COM-SURYO (IDX-1)         >   1   )
                   IF      FLG-FIRUMU                =   ZERO
                       MOVE    "1"             TO  SPA-COM-SURFLG
                                                               (IDX-1)
                   ELSE
                       IF      SPA-COM-SURFLG (IDX-1)
                                                   NOT =   "1"
                           MOVE    1           TO  SPA-COM-SURYO
                                                               (IDX-1)
      *                    入力コードの画面用再編集
                           MOVE    IDX-1               TO  IDX
                           PERFORM 3101-INPUTCD-HEN-SEC
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       3103-SINKOU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    残量廃棄の判定処理
      *****************************************************************
       31034-HAIKI-CHKI-SEC              SECTION.
      *
           IF      (SPA-COM-YKZKBN (IDX-1)  =   "4"  )  AND
                   (SPA-COM-TANICD (IDX-1)  =   "014"
                                            OR  "022"
                                            OR  "046" )  AND
                   (SPA-COM-SURYO(IDX-1)(6:3)  >   ZERO)
               IF     (IDX-1               <   200  )  AND
                      (SPA-COM-INPUTCD(IDX-1 + 1)  =   "099309901")
                   CONTINUE
               ELSE
                   MOVE    "1"             TO  SPA-COM-AUTOFLG2(IDX-1)
               END-IF
           END-IF
      *
           .
       31034-HAIKI-CHKI-EXT.
           EXIT.
      *
      *****************************************************************
      *   再表示あり　編集処理
      *****************************************************************
       31011-SPA-HEN-SEC              SECTION.
      *
      *    診療種別セット
           IF     (IDS                 =   1   )  AND
                  (SRY-RENNUM          =   1   )
               IF      SRY-SRYSYUKBN   NOT =   SPACE
                   ADD     1                   TO  IDX2
                   MOVE    "."                 TO  SPA-GMN-INPUTCD
                                                            (IDX2)(1:1)
                   MOVE    SRY-SRYSYUKBN       TO  SPA-GMN-INPUTCD
                                                           (IDX2)(2:3)
                   MOVE    SRY-SRYSYUKBN       TO  SPA-COM-SRYSYUKBN
                                                            (IDX2)
                   MOVE    SRY-SRYKBN          TO  SPA-COM-SRYKBN(IDX2)
               END-IF
           END-IF
      *
      *    明細編集
           ADD     1                   TO  IDX2
           MOVE    SRY-SRYKBN          TO  SPA-COM-SRYKBN    (IDX2)
           MOVE    SRY-SRYSYUKBN       TO  SPA-COM-SRYSYUKBN (IDX2)
           MOVE    SRY-SRYCD   (IDS)   TO  SPA-COM-INPUTCD   (IDX2)
           MOVE    SRY-SRYSURYO(IDS)   TO  SPA-COM-SURYO     (IDX2)
           MOVE    SRY-SRYSURYO(IDS)   TO  SPA-COM-SURYO2    (IDX2)
           MOVE    SRY-AUTOKBN (IDS)   TO  SPA-COM-AUTOKBN   (IDX2)
      *
      *    約束セットの時、表示なしとする（但し、減は表示する）
           IF     (FLG-YAKUSOKU        =   1   )  AND
                  (SRY-SRYCD (IDS) NOT =   "820000047" )
               MOVE    "1"                 TO  SPA-COM-SET (IDX2)
           END-IF
      *
      *    回数を編集
           MOVE    IDX2                TO  IDXS
      *
           IF      SRY-SRYKBN          =   "70"
      *        分画数を編集
               MOVE    SRY-SRYKAISU (IDS)  TO  SPA-COM-BUNGSU (IDX2)
           END-IF
      *    分画数を編集
           IF      SRY-SRYKBN          =   "23"
               MOVE    SRY-SRYKAISU (IDS)  TO  SPA-CHOKIKAISU
               IF      SRY-SRYKAISU (IDS)  >=  28
                   MOVE    1               TO  SPA-CHOKITOFLG
               END-IF
           END-IF
      *
           MOVE    SPA-COM-INPUTCD (IDX2)  TO  SPA-GMN-INPUTCD  (IDX2)
           IF      SRY-AUTOKBN (IDS)   =   "A" OR "B" OR "C"  OR  "D"
                                       OR  "E" OR "F" OR "G"
               PERFORM 31011-JIKANKBN-SET-SEC
               MOVE    SPACE               TO  SPA-COM-AUTOKBN (IDX2)
           END-IF
      *    数量入力時
           IF      SRY-AUTOKBN (IDS)   =   "S"
               MOVE    "1"                 TO  SPA-COM-SURFLG  (IDX2)
               MOVE    SPACE               TO  SPA-COM-AUTOKBN (IDX2)
           END-IF
      *
      *    約束セットの時、以下処理なし
           IF      SRY-SRYCD(IDS)(1:1) =   "S"
               MOVE    1               TO  FLG-YAKUSOKU
               GO      TO      31011-SPA-HEN-EXT
           END-IF
      *
           INITIALIZE                  ORCSC92AREA
           MOVE    IDX2                TO  LNK-SC92-GMN-IDX
           MOVE    "J04"               TO  LNK-SC92-PG
           MOVE    SPA-COM-INPUTCD(IDX2)
                                       TO  LNK-SC92-SRYCD
           MOVE    SPA-NAI-NENREI      TO  LNK-SC92-NENREI
           MOVE    SPA-ROUJIN          TO  LNK-SC92-ROUJIN
           MOVE    SPACE               TO  LNK-SC92-LSTSRYKA
           MOVE    SPACE               TO  LNK-SC92-LSTSRYKAMEI
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    SPACE           TO  SPA-ERRCD
           END-IF 
      *
      *    フィルムの判定
           IF      SRY-SRYKBN          =   "70"
               IF     (SPA-COM-INPUTCD (IDX2)(1:1) =   "7" )  AND
                      (SPA-COM-DATAKBN (IDX2)      =   "3" )
                   MOVE    1               TO  FLG-FIRUMU
               END-IF
           END-IF
      *
      *    コメントの時、コメント内容編集
           IF     (SPA-COM-INPUTCD (IDX2)(1:1) =   "0"  OR  "8" )  AND
                  (SRY-INPUTNUM    (IDS)   NOT =   ZERO         )
               INITIALIZE                  PTCOM-REC
               MOVE    SPA-HOSPID          TO  PTCOM-HOSPID
               MOVE    SPA-GMN-PTID        TO  PTCOM-PTID
               MOVE    SPA-ZAINUM          TO  PTCOM-ZAINUM
               MOVE    SRY-SRYCD    (IDS)  TO  PTCOM-SRYCD
               MOVE    SRY-INPUTNUM (IDS)  TO  PTCOM-RENNUM
               PERFORM 960-PTCOM-READ-SEC
               IF      FLG-PTCOM           =   ZERO
                   IF      PTCOM-INPUTCOMENT   NOT =   SPACE
                       IF      SRY-SRYCD (IDS)         =   "810000001"
                           MOVE    PTCOM-INPUTCOMENT
                                               TO  SPA-GMN-MEISYO-G
                                                                 (IDX2)
                           MOVE    "1"             TO  SPA-COM-MEIFLG
                                                                 (IDX2)
                       ELSE
                           MOVE    PTCOM-INPUTCOMENT
                                               TO  SPA-GMN-MEISYO
                                                                 (IDX2)
                       END-IF
                   END-IF
                   MOVE    PTCOM-INPUTCHI-AREA  TO  SPA-COM-INPUTCHI-G
                                                                (IDX2)
                END-IF
           END-IF
      *
      *    金額入力時、金額へ
      *
      *    自賠責の時、対象外
           IF     (SPA-HKNKBN          =   1    )  AND
      **********  (SPA-RSI-HKNKBN      =   "4"  )  AND
                  (SPA-COM-SRYKBN (IDX2)
                                       =   "80"  )  AND
                  (SPA-COM-INPUTCD(IDX2)(1:5)
                                       =   "09591"  OR
                                           "09592"  OR
                                           "09593"  OR
                                           "09594"  )
               MOVE    SPACE               TO  SPA-COM-KEIFLG(IDX2)
               GO      TO      31011-SPA-HEN-EXT
           END-IF
      *
           IF      SPA-COM-INPUTCD(IDX2)(1:3)  =   "095"  OR  "096"
               IF      SPA-COM-KISOTEN (IDX2)      =   ZERO
                   MOVE    "1"                 TO  SPA-COM-KEIFLG(IDX2)
                   MOVE    SRY-JIHIMONEY(IDS)
                                               TO  SPA-COM-KEI   (IDX2)
                   MOVE    SRY-JIHIMONEY(IDS)
                                               TO  SPA-COM-JIHIMONEY
                                                                  (IDX2)
                   MOVE    SRY-JIHIMONEY(IDS)
                                               TO  SPA-GMN-KEI   (IDX2)
               ELSE
                   MOVE    "2"                 TO  SPA-COM-KEIFLG(IDX2)
                   MOVE    SPA-COM-KISOTEN (IDX2)
                                               TO  SPA-COM-KEI   (IDX2)
                   MOVE    SPA-COM-KISOTEN (IDX2)
                                               TO  SPA-COM-JIHIMONEY
                                                                 (IDX2)
                   MOVE    SPA-COM-KISOTEN (IDX2)
                                               TO  SPA-GMN-KEI   (IDX2)
               END-IF
      *
      *        自費の時、金額編集
           ELSE
               IF      SPA-COM-SRYKBN (IDX2)   =   "95"  OR  "96"
                   MOVE    "2"                 TO  SPA-COM-KEIFLG(IDX2)
                   MOVE    SRY-JIHIMONEY(IDS)
                                               TO  SPA-COM-KEI   (IDX2)
                   MOVE    SRY-JIHIMONEY(IDS)
                                               TO  SPA-COM-JIHIMONEY
                                                                 (IDX2)
                   MOVE    SRY-JIHIMONEY(IDS)
                                               TO  SPA-GMN-KEI   (IDX2)
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
      *   時間外区分の画面用再編集処理
      *****************************************************************
       31011-JIKANKBN-SET-SEC          SECTION.
      *
           EVALUATE    SRY-AUTOKBN (IDS)
               WHEN    "A"
                   MOVE    "1"             TO  WRK-TIMEFLG
               WHEN    "B"
                   MOVE    "2"             TO  WRK-TIMEFLG
               WHEN    "C"
                   MOVE    "3"             TO  WRK-TIMEFLG
               WHEN    "D"
                   MOVE    "4"             TO  WRK-TIMEFLG
               WHEN    "E"
                   MOVE    "5"             TO  WRK-TIMEFLG
               WHEN    "F"
                   MOVE    "6"             TO  WRK-TIMEFLG
               WHEN    "G"
                   MOVE    "7"             TO  WRK-TIMEFLG
           END-EVALUATE
           MOVE    SPACE               TO  SPA-GMN-INPUTCD (IDX2)
           MOVE    WRK-TIMEFLG         TO  SPA-GMN-INPUTCD (IDX2)(1:1)
           MOVE    SPA-COM-INPUTCD (IDX2)
                                       TO  SPA-GMN-INPUTCD (IDX2)(3:)
           MOVE    WRK-TIMEFLG         TO  SPA-COM-TIMEFLG (IDX2)
      *
           .
       31011-JIKANKBN-SET-EXT.
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
           MOVE    "J04"               TO  LNK-SC94-PG
           MOVE    IDX                 TO  LNK-SC94-IDX
           CALL    "ORCSC94"           USING
                                       ORCSC94AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *
           .
       3101-INPUTCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量表示用編集処理
      *****************************************************************
       31011-SURYO-HEN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
      *
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING     IDM     FROM    3  BY  -1
                   UNTIL       IDM     <   1
               IF      WRK-SURYO-S2(IDM:1) NOT =   ZERO
                   MOVE    1               TO  FLG-SP
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(IDM:1) TO  WRK-SURYO-Z3(IDM:1)
               END-IF
           END-PERFORM
      *
           IF      WRK-SURYO-Z3        NOT =   SPACE
               MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z19
               MOVE    "."                 TO  WRK-SURYO-Z2
           END-IF
           .
       31011-SURYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック用月内診療行為セット処理
      *****************************************************************
       320-SPA-CHKSET-SEC              SECTION.
      *
      *    禁忌薬剤チェック用編集
      *****PERFORM 3202-KNKYAKSET-SEC
      *
           .
       320-SPA-CHKSET-EXT.
           EXIT.
      *****************************************************************
      *    禁忌薬剤チェック用編集処理
      *****************************************************************
       3202-KNKYAKSET-SEC              SECTION.
      *
           MOVE    ZERO                TO  IDX
      *    対象年月−１年を求める
           MOVE    SPA-J02-SRYYMD(1:6) TO  WRK-SRYYM
           COMPUTE WRK-SRYYY           =   WRK-SRYYY     -   1
      *
           PERFORM
                   UNTIL   SPA-J02-SRYYMD(1:6) <   WRK-SRYYM
              PERFORM 3203-KNKYAKSET-HEN-SEC
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
       3203-KNKYAKSET-HEN-SEC              SECTION.
      *
      *    診療行為マスターを検索する
           INITIALIZE                      SRYACT-REC
           MOVE    SPA-HOSPID          TO  SRY-HOSPID
           MOVE    SPA-GMN-PTID        TO  SRY-PTID
           MOVE    WRK-SRYYM           TO  SRY-SRYYM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SRYACT-KEY3"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SRYACT-KEY3"       TO  ORC-DBPATH
               PERFORM 900-SRYACT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL     (FLG-SRYACT      =   1          )
      *        投薬・注射を対象とする
               IF       SRY-SRYKBN         =   "21"  OR  "22"  OR
                                               "23"  OR  "31"  OR
                                               "32"  OR  "33"
                   PERFORM 32021-KNK-KOUI-SEC
               END-IF
      *
               MOVE    "SRYACT-KEY3"       TO  ORC-DBPATH
               PERFORM 900-SRYACT-NEXT-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SRYACT-KEY3"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       3203-KNKYAKSET-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌薬剤チェック用編集処理
      *****************************************************************
       32021-KNK-KOUI-SEC              SECTION.
      *
      *    回数を診療会計マスタよりチェックする
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPID          TO  ACCT-HOSPID
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  ACCT-PTID
           MOVE    SPA-J01-SRYKA       TO  ACCT-SRYKA
           MOVE    SRY-SRYYM           TO  ACCT-SRYYM
           MOVE    SRY-SRYKBN          TO  ACCT-SRYKBN
           MOVE    SRY-ZAINUM          TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SRYACCT-KEY"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SRYACCT-KEY"       TO  ORC-DBPATH
               PERFORM 940-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SRYACCT-KEY"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           IF     (FLG-SRYACCT          =   ZERO  )  AND
                  (ACCT-ZAIKAISU        >   ZERO  )
               CONTINUE
           ELSE
               GO      TO      32021-KNK-KOUI-EXT
           END-IF
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >   5   )  OR
                              (SRY-SRYCD(IDX2)     =   SPACE )
               IF      SRY-SRYCD (IDX2)(1:1)   =   "6"
                   PERFORM VARYING     IDX3    FROM    1   BY  1
                           UNTIL       IDX3        >   100
                       IF      SPA-KNK-SRYCD (IDX3)  =   SPACE
                           MOVE    SRY-SRYCD (IDX2)  TO  SPA-KNK-SRYCD
                                                             (IDX3)
                           MOVE    SRY-SRYCD (IDX2)  TO  TNS-SRYCD
                           PERFORM 910-TENSU-READ-SEC
                           IF      FLG-TENSU         =   ZERO
                               MOVE    TNS-NAME      TO  SPA-KNK-SRYMEI
                                                               (IDX3)
                           END-IF
                       END-IF
                       IF      SPA-KNK-SRYCD (IDX3)
                                                   =  SRY-SRYCD (IDX2)
                           PERFORM VARYING     IDX4   FROM    1   BY  1
                                   UNTIL       IDX4        >   13
                               IF      SPA-KNK-SRYYM (IDX3 IDX4)
                                                           =   SPACE
                                   MOVE    ACCT-SRYYM     TO
                                                   SPA-KNK-SRYYM
                                                            (IDX3 IDX4)
                                   IF      ACCT-SRYYM
                                                 =  SPA-J02-SRYYMD(1:6)
                                       MOVE    "1"   TO  SPA-KNK-TOGETU
                                                                (IDX3)
                                   END-IF
                                   MOVE    13              TO  IDX4
                               ELSE
                                   IF      SPA-KNK-SRYYM (IDX3 IDX4)
                                                       =   ACCT-SRYYM
                                       MOVE    13              TO  IDX4
                                   END-IF
                               END-IF
                           END-PERFORM
                           MOVE    100             TO  IDX3
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           .
       32021-KNK-KOUI-EXT.
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
      *    行為セット
      *D       WHEN    "CLICKED"       ALSO    "B03"
      *D           PERFORM 420-KOUISET-SEC
      *    番号検索
      *D       WHEN    "CLICKED"       ALSO    "B04"
      *D           PERFORM 430-BANKENSAKU-SEC
      *    前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 450-MAEGMN-SEC
      *    次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 460-ATOGMN-SEC
      *    予約登録へ
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 480-YOYAKU-SEC
      *    受付一覧へ
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-UKEICHI-SEC
      **    登録
               WHEN    "CLICKED"       ALSO    "B12"
                    PERFORM 4100-TOROKU-SEC
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
      *    入力チェック処理
           PERFORM 220-INPUT-CHK-SEC
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
           MOVE    ZERO                TO  SPA-GMN-CUR
      *    入力項目の決定
           PERFORM 2001-INPUT-SET-SEC
      *
      *    画面をＳＰＡへ編集
           PERFORM 2002-GMNSPA-SET-SEC
      *
           .
       210-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目の決定
      *****************************************************************
       2001-INPUT-SET-SEC               SECTION.
      *
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
           IF      WRK-WIDGET          =   SPACE
               MOVE    MCP-WIDGET          TO  WRK-WIDGET
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
      *    画面項目→ＳＰＡ編集処理
      *****************************************************************
       2002-GMNSPA-SET-SEC               SECTION.
      *
           PERFORM 20021-INPUT-GMNSET-SEC
           .
       2002-GMNSPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードのＳＰＡ編集処理
      *****************************************************************
       20021-INPUT-GMNSET-SEC               SECTION.
      *
      *    入力コード入力時、カーソルの保存
           IF     (MCP-EVENT           =   "ACTIVATE" ) AND
                  (WRK-WIDGET          =   "INPUTCD"  )
               MOVE    WRK-IDX2            TO  WRK-IN-MAP-IDX
           ELSE
               MOVE    ZERO                TO  WRK-IN-MAP-IDX
           END-IF
      *
      *    カーソル位置のクリア等
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   200
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
               IF     (J04-INPUTCD (WRK-IDX2)  NOT =
                                       SPA-GMN-INPUTCD (SPA-GMN-IDX)) OR
                      (J04-MEISYO  (WRK-IDX2)   NOT =
                                       SPA-GMN-MEISYO-G (SPA-GMN-IDX))
                   MOVE    "1"                 TO  SPA-COM-IN2
                                                           (SPA-GMN-IDX)
               END-IF
      *
               MOVE    J04-INPUTCD (WRK-IDX2)  TO  SPA-GMN-INPUTCD
                                                           (SPA-GMN-IDX)
               MOVE    J04-MEISYO  (WRK-IDX2)  TO  SPA-GMN-MEISYO-G
                                                           (SPA-GMN-IDX)
      *        入力行の保存
               IF     (WRK-IN-MAP-IDX  NOT =   ZERO )  AND
                      (WRK-IDX2            =   WRK-IN-MAP-IDX)
                   MOVE    "1"                 TO  SPA-COM-IN
                                                          (SPA-GMN-IDX)
                   IF     (J04-INPUTCD (WRK-IDX2)  =   SPACE )  AND
                          (SPA-MAE-INPUTCD (SPA-GMN-IDX)
                                                   =   SPACE )  AND
                          (SPA-COM-INPUTCD (SPA-GMN-IDX)
                                                   =   SPACE )  AND
                          (SPA-GMN-IDX             >   SPA-GMN-MAX)
                       MOVE    1                   TO  FLG-KUHAKU
                   END-IF
               END-IF
      *
               IF      J04-INPUTCD (WRK-IDX2)  NOT =   SPACE
                   IF      WRK-STR             =   ZERO
                       MOVE    SPA-GMN-IDX         TO  WRK-STR
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       20021-INPUT-GMNSET-EXT.
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
      *    入力チェック処理
      *****************************************************************
       220-INPUT-CHK-SEC           SECTION.
      *
      *    画面ＳＰＡチェック処理
           MOVE    SPACE           TO  FLG-TOROKU
           PERFORM 410112-KOUI-SPACHK-SEC
      *
           .
       220-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡチェック処理
      *****************************************************************
       410112-KOUI-SPACHK-SEC               SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
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
           IF      FLG-INS             =   1
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF
      *
      *    点数計算・剤別チェック処理
      **** MOVE    SPACE           TO  FLG-TOROKU
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
               IF      SPA-MAE-INPUTCD(SPA-GMN-IDX)(1:1)   =   "S"
                   PERFORM 41016-SETDEL-SEC
               END-IF
               MOVE    SPACE           TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               MOVE    1               TO  FLG-INS
               GO  TO                  41011-KOUI-HENDEL-EXT
           END-IF
      *
      *    剤削除時
      *    '-'入力がある時、剤削除ができなので、エラーとする
           IF      LNK-SC97-INPUTCD(1:1)   =   "-"
               MOVE    LNK-SC97-INPUTCD
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               MOVE    "1030"          TO  SPA-ERRCD
               MOVE    "1"             TO  SPA-COM-CUR    (SPA-GMN-IDX)
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
               MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
               ADD     1               TO  SPA-GMN-IDX
               MOVE    1               TO  FLG-INS
               GO  TO                  41011-KOUI-HEN2-EXT
           END-IF
      *    一覧へ
           IF      LNK-SC97-INPUTCD(1:2)
                                       =  "//"
               MOVE    SPA-MAE-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               INITIALIZE                      SPA-J98-AREA
               MOVE    LNK-SC97-INPUTCD    TO  SPA-J98-INPUTCD
               PERFORM 410-J98-SYORI-SEC
               GO      TO                  41011-KOUI-HEN2-EXT
           END-IF
      *    Ｐ入力へ
           IF      LNK-SC97-INPUTCD(1:1)
                                       =  "/"
               MOVE    SPA-MAE-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               PERFORM 430-J99-SYORI-SEC
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
               INITIALIZE                      SPA-J98-AREA
               MOVE    LNK-SC97-INCD       TO  SPA-J98-INPUTCD
               MOVE    "1"                 TO  SPA-J98-TYPE
               MOVE    LNK-SC97-CDSYU      TO  SPA-J98-CDSYUBETU
               MOVE    "1"                 TO  SPA-COM-CUR (SPA-GMN-IDX)
      *
      *        診療行為一覧へ
               PERFORM 410-J98-SYORI-SEC
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
               MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
           END-IF
      *
           .
       41011-KOUI-HEN2-EXT.
           EXIT.
      *
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
           END-PERFORM
      *
           IF      WRK-IN-IDX          =   ZERO
      ******** MOVE    SPA-GMN-MAX         TO  SPA-GMN-IDX
               MOVE    WRK-MAX-IDX         TO  SPA-GMN-IDX
           ELSE
               MOVE    WRK-IN-IDX          TO  SPA-GMN-IDX
           END-IF
           IF      SPA-GMN-IDX         =   ZERO
               GO      TO      410119-SAIINPUT-EXT
           END-IF
      *
      *    数量・回数入力なし
           IF     (SPA-COM-SURFLG (SPA-GMN-IDX)  =   SPACE ) AND
                  (SPA-COM-IN2    (SPA-GMN-IDX)  =   "2"   ) AND
                  (SPA-COM-KAIFLG (SPA-GMN-IDX)  =   SPACE ) AND
                  (WRK-IN-IDX2                   =   ZERO  )
               MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                            TO  WRK-INPUTCD
               MOVE    "1"                  TO  SPA-COM-CUR
                                                       (SPA-GMN-IDX)
               IF      SPA-COM-INPUTCD (SPA-GMN-IDX)(1:3)  =   "001"
                   MOVE    SPACE            TO  SPA-COM-CUR
                                                       (SPA-GMN-IDX)
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
                                           =   "0083"  )
                   MOVE    "3"             TO  SPA-COM-IN2 (SPA-GMN-IDX)
               END-IF
           END-IF
      *
      *    空白行入力時、前の行へ
           IF      FLG-KUHAKU          =   1
      *        空白行入力時、最終行へ
               MOVE    "1"                 TO  SPA-COM-CUR
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
      *
           MOVE    SPACE               TO  SPA-COM-IN3 (SPA-GMN-IDX)
      *
      *    数量以降入力なし（’＿’編集用）
           IF     (WRK-SUR-MCNT        =   ZERO )  AND
                  (WRK-ACCT-MCNT       =   ZERO )  AND
                  (WRK-MCNT1           =   ZERO )  AND
                  (WRK-MCNT2           =   ZERO )  AND
                  (WRK-MCNT3           =   ZERO )  AND
                  (WRK-MCNT4           =   ZERO )
      *        薬剤と数量入力必須の診療行為、薬剤コメント
               IF      SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)   =   "059"
                   MOVE    "7"                     TO  WRK-MOJI
               ELSE
                   MOVE    SPA-COM-INPUTCD(SPA-GMN-IDX)(1:1)
                                                   TO  WRK-MOJI
               END-IF
      *
               IF     (WRK-MOJI                    =   "6" OR
                                                       "7" )  OR
                     ((WRK-MOJI                    =   "1" ) AND
                      (TNS-SRYSYUKBN(1:1)      NOT =   "7" ) AND
                      (TNS-KZMCOMPSIKIBETU     NOT =   0   ))  OR
                     ( SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)
                                                   =   "001" ) OR
                     ((SPA-COM-INPUTCD(SPA-GMN-IDX)(1:2)
                                                   =   "84" ) AND
                      (TNS-SSTKIJUNCDTBL(1)    NOT =   ZERO ))  OR
                     ((SPA-COM-INPUTCD(SPA-GMN-IDX)(1:4)
                                                   =   "0084") AND
                      (TNS-SSTKIJUNCDTBL(1)    NOT =   ZERO ))  OR
                     ((SPA-COM-INPUTCD(SPA-GMN-IDX)(1:5)
                                                   =   "09500" OR
                                                       "09600" ) AND
                      (TNS-TEN                     =   ZERO ))
                   MOVE    "2"             TO  SPA-COM-IN2 (SPA-GMN-IDX)
                   MOVE    "1"             TO  SPA-COM-IN3 (SPA-GMN-IDX)
               END-IF
           END-IF
      *
      *    数量５文字以上はエラー
           IF     (SPA-COM-INPUTCD(SPA-GMN-IDX) (1:2)
                                           NOT =   "09" )  AND
                  (LNK-SC97-SURYOFLG       >   5    )
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
      *
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    "J04"               TO  LNK-SC92-PG
           MOVE    SPACE               TO  LNK-SC92-KBN
           MOVE    SPA-GMN-IDX         TO  LNK-SC92-GMN-IDX
           MOVE    SPA-COM-INPUTCD (SPA-GMN-IDX)
                                       TO  LNK-SC92-SRYCD
           MOVE    SPA-NAI-NENREI      TO  LNK-SC92-NENREI
           MOVE    SPA-ROUJIN          TO  LNK-SC92-ROUJIN
           MOVE    SPACE               TO  LNK-SC92-LSTSRYKA
           MOVE    SPACE               TO  LNK-SC92-LSTSRYKAMEI
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
      *            算定回数、併用算定チェックはエラーとしない
                   IF     IDX-ERR              =   3  OR  6
                       MOVE    SPACE           TO  SPA-ERRCD
                   END-IF
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    SPACE           TO  SPA-COM-CHKFLG (SPA-GMN-IDX)
               GO      TO      4101113-INPUTCD-CHK-EXT
           END-IF
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
      *    時間外加算の時、ＳＰＡへ
      *    IF      SPA-COM-TIMEFLG(SPA-GMN-IDX)  NOT =   ZERO
      *        MOVE    SPA-COM-TIMEFLG(SPA-GMN-IDX)
      *                                    TO  SPA-NAI-TIMEFLG
      *    END-IF
      *
      *    金額入力判定
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
                                           =   "83"   )  OR
                      (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)
                                           =   "0083" ))
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
               IF      KANACHK-RC2         =   7
                   MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-MEISYO-G
                                                       (SPA-GMN-IDX)
                                                            (IDX-STR:)
               END-IF
               MOVE    "3"             TO  SPA-COM-CUR (SPA-GMN-IDX)
               MOVE    2               TO  SPA-GMN-CUR
           ELSE
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-MEISYO-G
                                                         (SPA-GMN-IDX)
                                                            (IDX-STR:)
           END-IF
      *
           .
       40113-MEISYOU-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    Ｐ入力画面へ移行
      *****************************************************************
       430-J99-SYORI-SEC       SECTION.
      *
      *    画面編集
           INITIALIZE                  SPA-J99GMN-AREA
      *****MOVE    "後退--"            TO  J99-PBK
           MOVE    SPACE               TO  J99-PIN
           MOVE    SPACE               TO  J99-PED
      *
      *    画面カーソルセット処理
           MOVE    "PIN"               TO  MCP-WIDGET
      *
           MOVE    SPACE               TO  SPA-J99-DATA
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
      *
           MOVE    "J99"               TO  SPA-SAKIPG
           MOVE    "J04"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "J99"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       430-J99-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為一覧へ
      *****************************************************************
       410-J98-SYORI-SEC          SECTION.
      *
      *    内服を編集する
           INITIALIZE                  SPA-J98GMN-AREA
      *!!  IF      SPA-J98-INPUTCD         =   SPACE
           IF      SPA-J98-INPUTCD(1:2)    =   "//"
               EVALUATE    SPA-J98-INPUTCD(3:1)
      *        コメント一覧
                   WHEN    "C"
                   WHEN    "c"
                       MOVE    "U"             TO  SPA-J98-GMN-SYORI
                       MOVE    "U4"            TO  SPA-J98-GMN-SYORI3
                       MOVE    SPACE               TO  SPA-J98-INPUTCD
      *        用法
                   WHEN    "Y"
                   WHEN    "y"
                       MOVE    "U"             TO  SPA-J98-GMN-SYORI
                       MOVE    "U1"            TO  SPA-J98-GMN-SYORI3
                       MOVE    SPACE               TO  SPA-J98-INPUTCD
      *        診療種別一覧
                   WHEN    "."
                       MOVE    "."             TO  SPA-J98-INPUTCD
      *            区分番号別
                   WHEN    "*"
                       MOVE    SPA-J98-INPUTCD(3:) TO  SPA-J98-INPUTCD
                   WHEN    "/"
                       MOVE    SPA-J98-INPUTCD(3:) TO  SPA-J98-INPUTCD
                   WHEN    OTHER
                       MOVE    SPACE               TO  SPA-J98-INPUTCD
               END-EVALUATE
      *        画面編集
               PERFORM 4101-J98-SET-SEC
           ELSE
               INITIALIZE                  SPA-J98GMN-AREA
      *        内服薬剤
               MOVE    "3"                 TO  SPA-J98-GMN-SYORI
               IF     (SPA-GMN-IDX         =   1  )  OR 
                      (SPA-COM-KAIFLG (SPA-GMN-IDX - 1)    =   "1" ) OR
                      (SPA-GMN-INPUTCD(SPA-GMN-IDX - 1)(1:1)  =  "S")
      *            内服薬剤
                   MOVE    "3"                 TO  SPA-J98-GMN-SYORI
               ELSE
                   EVALUATE    SPA-COM-SRYKBN(SPA-GMN-IDX - 1)
                       WHEN    SPACE
      *                    診療区分が決定してない時
                           PERFORM  4101-J98-SYRKBN-SEC
                       WHEN    "31"
                       WHEN    "32"
                       WHEN    "33"
      *                    注射
                           MOVE    "5"         TO  SPA-J98-GMN-SYORI
                       WHEN    "23"
      *                    外薬
                           MOVE    "4"         TO  SPA-J98-GMN-SYORI
                       WHEN    "21"
                       WHEN    "22"
      *                    外薬
                           MOVE    "3"         TO  SPA-J98-GMN-SYORI
                       WHEN    OTHER
      *                    診療行為
      ******************   MOVE    "3"         TO  SPA-J98-GMN-SYORI
                           MOVE    "3"         TO  SPA-J98-GMN-SYORI
                           MOVE    1           TO  FLG-KOUI
                   END-EVALUATE
               END-IF
      *        画面編集
               PERFORM 4101-J98-SET-SEC
      *
      *
      *        点数マスタ編集で、対象データなしの時、診療行為で検索する
               IF     (SPA-J98-GMN-MAX     =   ZERO )  AND
      ************    (SPA-J98-TYPE        =   "2"  )  AND
      ************    (SPA-J98-GMN-SYORI   =   "8"  )
                      (SPA-J98-CDSYUBETU   =   "J"  )  AND
                      (FLG-KOUI            =   1    )
                   INITIALIZE                  SPA-J98GMN-AREA
                   MOVE    "8"                 TO  SPA-J98-GMN-SYORI
      *            画面編集
                   PERFORM 4101-J98-SET-SEC
               END-IF
           END-IF
      *
      *****点数マスタ編集で、対象データなしの時、診療行為で検索する
      *    IF     (SPA-J98-GMN-MAX     =   ZERO )  AND
      *           (SPA-J98-TYPE        =   "2"  )
      *        INITIALIZE                  SPA-J98GMN-AREA
      *        MOVE    "8"                 TO  SPA-J98-GMN-SYORI
      *        画面編集
      *        PERFORM 4101-J98-SET-SEC
      *    END-IF
      *    対象データなし
      *****IF      SPA-J98-GMN-MAX     =   ZERO
      *        MOVE    "0001"              TO  SPA-ERRCD
      *        MOVE    1                   TO  SPA-GMN-CUR
      *        MOVE    "1"                 TO  SPA-COM-CUR(SPA-GMN-IDX)
      *        GO      TO      410-J98-SYORI-EXT
      *****END-IF
      *
      *    画面カーソルセット処理
           IF      SPA-J98-GMN-MAX     =   ZERO
               MOVE    "INPUT"             TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM1"           TO  MCP-WIDGET
           END-IF
      *
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
      *
      *    診療行為一覧へ
           MOVE    "J98"               TO  SPA-SAKIPG
           MOVE    "J04"               TO  SPA-MOTOPG
           MOVE    "J04"               TO  SPA-J98-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "J98"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
      *
       410-J98-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    診療区分が決定してない時診療種別処理
      *****************************************************************
       4101-J98-SYRKBN-SEC       SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-IDX
               EVALUATE    SPA-GMN-INPUTCD(IDX)(1:3)
                   WHEN    ".21"
                       MOVE    "3"                 TO  SPA-J98-GMN-SYORI
                   WHEN    ".22"
                       MOVE    "3"                 TO  SPA-J98-GMN-SYORI
                   WHEN    ".23"
                       MOVE    "4"                 TO  SPA-J98-GMN-SYORI
                   WHEN    ".31"
                   WHEN    ".32"
                   WHEN    ".33"
                       MOVE    "5"                 TO  SPA-J98-GMN-SYORI
               END-EVALUATE
           END-PERFORM
      *
           .
      *
       4101-J98-SYRKBN-EXT.
           EXIT.
      *****************************************************************
      *    Ｋ９８画面編集処理
      *****************************************************************
       4101-J98-SET-SEC       SECTION.
      *
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-J04             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGJ98"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-J04
           MOVE    SPA-WORK        TO  SPA-J01KYOUTU
           .
      *
       4101-J98-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力文字を数字変換処理
      *****************************************************************
       4101121-MOJI-SET-SEC        SECTION.
      *
      *
           MOVE    LNK-SC97-SURYOCNT   TO  WRK-SUR-MCNT
           MOVE    LNK-SC97-KAISUCNT   TO  WRK-ACCT-MCNT
           MOVE    LNK-SC97-SURYO      TO  WRK-SURYO
           MOVE    LNK-SC97-KAISU      TO  WRK-KAISU
      *****MOVE    LNK-SC97-KAISUFLG   TO  FLG-KAISU
           MOVE    LNK-SC97-BUNGSU     TO  WRK-BUNGSU
           MOVE    LNK-SC97-MCNT1      TO  WRK-MCNT1
           MOVE    LNK-SC97-MCNT2      TO  WRK-MCNT2
           MOVE    LNK-SC97-MCNT3      TO  WRK-MCNT3
           MOVE    LNK-SC97-MCNT4      TO  WRK-MCNT4
           MOVE    LNK-SC97-MOJI1      TO  WRK-MOJI1
           MOVE    LNK-SC97-MOJI2      TO  WRK-MOJI2
           MOVE    LNK-SC97-MOJI3      TO  WRK-MOJI3
           MOVE    LNK-SC97-MOJI4      TO  WRK-MOJI4
      *
           .
       4101121-MOJI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療コード入力チェック処理
      *****************************************************************
       4101121-INPUTCD-CHECK-SEC    SECTION.
      *
      *    回数入力不可(J04)
           IF      WRK-ACCT-MCNT   NOT =   ZERO
               MOVE    "1021"          TO  SPA-ERRCD
               GO      TO      4101121-INPUTCD-CHECK-EXT
           END-IF
      *
           MOVE    SPA-COM-INPUTCD (SPA-GMN-IDX)(1:1)
                                           TO  WRK-MOJI
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
                   IF      TNS-KZMCOMPSIKIBETU     =   0
                       IF      WRK-SUR-MCNT    NOT =   ZERO
                           MOVE    "0002"          TO  SPA-ERRCD
                       END-IF
                   ELSE
      *                数量　＞　きざみ値上限の時、エラー
                       IF      SPA-COM-SURYO(SPA-GMN-IDX)
                                                   >  TNS-KZMJGN
                           MOVE    "0037"          TO  SPA-ERRCD
                       END-IF
                   END-IF
      *
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
      *        診療で、手技料の時
               IF     (LNK-SC97-JIKAN      =   1   OR  2  OR  3
                                                   OR  4
                                                   OR  5
                                                   OR  6
                                                   OR  7       )  AND
                      (TNS-DATAKBN         =   "1"             )
                  MOVE    LNK-SC97-JIKAN       TO  SPA-COM-TIMEFLG
                                                          (SPA-GMN-IDX)
               ELSE
                   MOVE    "0030"          TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    LNK-SC97-JIKAN      TO  SPA-COM-TIMEFLG
                                                          (SPA-GMN-IDX)
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
           MOVE    SPACE               TO  SPA-COM-AUTOFLG2(SPA-GMN-IDX)
           IF      WRK-SUR-MCNT        =   ZERO
               MOVE    1               TO  SPA-COM-SURYO(SPA-GMN-IDX)
               MOVE    SPACE           TO  SPA-COM-SURFLG(SPA-GMN-IDX)
           ELSE
               MOVE    WRK-SURYO       TO  SPA-COM-SURYO(SPA-GMN-IDX)
               IF      SPA-COM-SURYO(SPA-GMN-IDX)  =  ZERO
                   MOVE    1           TO  SPA-COM-SURYO(SPA-GMN-IDX)
               END-IF
               MOVE    "1"             TO  SPA-COM-SURFLG(SPA-GMN-IDX)
               IF      LNK-SC97-AUTOFLG2   =   "1"
                   MOVE    "1"         TO  SPA-COM-AUTOFLG2(SPA-GMN-IDX)
               END-IF
           END-IF
           MOVE    SPA-COM-SURYO(SPA-GMN-IDX)
                                       TO  SPA-COM-SURYO2(SPA-GMN-IDX)
      *
      *    回数（未入力時、１回）
           IF      WRK-ACCT-MCNT       =   ZERO
               IF      SPA-COM-KAISU(SPA-GMN-IDX)  =  ZERO
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
      *    入力値の入力判定用
      *    その他値
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
      *    約束セット以外をエラーとする
           IF      SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)   =   "S"
               CONTINUE
           ELSE
               MOVE    "1022"              TO  SPA-ERRCD
               MOVE    "1"                 TO  SPA-COM-CUR(SPA-GMN-IDX)
               GO      TO      410119-INPSET-EXT
           END-IF
      *
      *    時間外がないこと
           IF      FLG-JKNKSN      NOT =   ZERO
               MOVE    "0010"              TO  SPA-ERRCD
               GO  TO                 410119-INPSET-EXT
           END-IF
      *
      *
           MOVE    1                   TO  FLG-SETSYORI
      *
      *    約束の時、１行前に診療種別があること
           IF      SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)   =   "S"
               IF     (SPA-GMN-IDX         =   1   )   OR
                      (SPA-GMN-INPUTCD(SPA-GMN-IDX - 1)(1:1)
                                       NOT = "." )
                   MOVE    "0048"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  SPA-COM-CUR
                                                         (SPA-GMN-IDX)
      *************GO      TO      410119-INPSET-EXT
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
           IF     (SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)  =   "S" )  AND
                  (FLG-INPUTSET        =   ZERO  )
      *
               IF      FLG-TOROKU      NOT =   "2"
                   PERFORM 4101192-INPSET-SET-SEC
               END-IF
      *        減がある時、その次へ
               IF      SPA-COM-INPUTCD(SPA-GMN-IDX + 1)  =  "820000047"
                   ADD     1               TO  SPA-GMN-IDX
               END-IF
           END-IF
      *
           PERFORM
                   UNTIL     (FLG-INPUTSET     =   1 )  OR
                             (SPA-ERRCD    NOT =   SPACE )
      *        行を挿入して追加する
               ADD     1               TO  SPA-GMN-IDX
               PERFORM 41014-GYOINSN-SEC
      *
               ADD     1               TO  IDX-IP
      *
               MOVE    ISET-INPUTCD    TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               IF      ISET-SURYO1     =   ZERO
                   MOVE    1           TO  SPA-COM-SURYO (SPA-GMN-IDX)
               ELSE
                   MOVE    ISET-SURYO1 TO  SPA-COM-SURYO (SPA-GMN-IDX)
               END-IF
               MOVE    SPA-COM-SURYO(SPA-GMN-IDX)
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
                   MOVE    "1"         TO  SPA-COM-KAIFLG(SPA-GMN-IDX)
               END-IF
               MOVE    SPA-COM-KAISU(SPA-GMN-IDX)
                                       TO  SPA-COM-KAISU2(SPA-GMN-IDX)
      *
               MOVE    ISET-COMMENT    TO  SPA-GMN-MEISYO(SPA-GMN-IDX)
      *
               MOVE    ISET-ATAI (01)  TO  SPA-COM-ATAI1 (SPA-GMN-IDX)
               MOVE    ISET-ATAI (02)  TO  SPA-COM-ATAI2 (SPA-GMN-IDX)
               MOVE    ISET-ATAI (03)  TO  SPA-COM-ATAI3 (SPA-GMN-IDX)
               MOVE    ISET-ATAI (04)  TO  SPA-COM-ATAI4 (SPA-GMN-IDX)
      *
      *        画面表示なし
               IF      SPA-GMN-INPUTCD(WRK-GMN-IDX)(1:1)   =   "S"
                   MOVE    "1"             TO  SPA-COM-SET(SPA-GMN-IDX)
               END-IF
      *
               IF       SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)  =   "."
      *            診療種別区分名称チェック
                   MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:3)
                                                 TO  WRK-SRYSYUKBN
                   PERFORM 510-SRYKBN-MEI-SEC
               ELSE
      *
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
                       MOVE    SPA-COM-SURYO(SPA-GMN-IDX)
                                       TO  SPA-COM-SURYO2(SPA-GMN-IDX)
      *                回数変更
                       MOVE    SPA-COM-KAISU (WRK-GMN-IDX)
                                               TO  SPA-COM-KAISU
                                                         (SPA-GMN-IDX)
                       MOVE    SPA-COM-KAISU(SPA-GMN-IDX)
                                       TO  SPA-COM-KAISU2(SPA-GMN-IDX)
                   END-IF
      *
      *            点数マスタ編集・基本チェック
                   INITIALIZE                  ORCSC92AREA
                   MOVE    SPACE               TO  LNK-SC92-KBN
                   MOVE    "J04"               TO  LNK-SC92-PG
                   MOVE    SPA-GMN-IDX         TO  LNK-SC92-GMN-IDX
                   MOVE    SPA-COM-INPUTCD(SPA-GMN-IDX)
                                               TO  LNK-SC92-SRYCD
                   MOVE    SPA-NAI-NENREI      TO  LNK-SC92-NENREI
                   MOVE    SPA-ROUJIN          TO  LNK-SC92-ROUJIN
                   MOVE    SPACE               TO  LNK-SC92-LSTSRYKA
                   MOVE    SPACE               TO  LNK-SC92-LSTSRYKAMEI
                   CALL    "ORCSC92"           USING
                                               MCPAREA
                                               ORCSC92AREA
                                               SPA-SCR-REC
                                               SPA-AREA
      *
      *           エラー時、追加しない
                  IF     (SPA-ERRCD           NOT =   SPACE )  OR
                         (LNK-SC92-ERRAREA    NOT =   SPACE )
                      MOVE    SPACE           TO  SPA-ERRCD
                      INITIALIZE          SPA-GMN-TBLREC (SPA-GMN-IDX)
                      INITIALIZE          SPA-COM-TBLREC (SPA-GMN-IDX)
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
               IF      SPA-COM-KAIFLG (SPA-GMN-IDX)    =   "1"
                   MOVE    SPACE               TO  WRK-SRYKBN
               END-IF
      *
               MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-MAE-INPUTCD(SPA-GMN-IDX)
      *
               MOVE    "INPUTSET-KEY2"     TO  ORC-DBPATH
               PERFORM 975-INPUTSET-READ-SEC
      *
           END-PERFORM
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
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
               GO      TO    410119-INPSET-EXT
           END-IF
           MOVE    SPACE               TO  SPA-ERRCD
      *
      *    対象なし
           IF      IDX-IP              =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               GO      TO    410119-INPSET-EXT
           END-IF
      *    約束セットの時
           IF      SPA-GMN-INPUTCD(WRK-GMN-IDX)(1:1)   =   "S"
      *        セットの時前に編集
               IF      WRK-ERRCD       =   SPACE
                   MOVE    SPA-GMN-INPUTCD(WRK-GMN-IDX)
                                       TO  SPA-MAE-INPUTCD(WRK-GMN-IDX)
               ELSE
                   MOVE    "1"         TO  SPA-COM-CUR (WRK-GMN-IDX)
               END-IF
           END-IF
      *
      *****PERFORM 510-TBLHEN-SEC
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       410119-INPSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険外金額判定処理
      *****************************************************************
       4101191-JIHI-HEN-SEC           SECTION.
      *
      *    自賠責の時、対象外
           IF     (SPA-HKNKBN          =   1    )  AND
      ************(SPA-RSI-HKNKBN      =   "4"  )  AND
                  (SPA-COM-SRYKBN (SPA-GMN-IDX)
                                       =   "80"  )  AND
                  (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:5)
                                       =   "09591"  OR
                                           "09592"  OR
                                           "09593"  OR
                                           "09594"  )
               MOVE    SPACE               TO  SPA-COM-KEIFLG
                                                           (SPA-GMN-IDX)
               GO      TO      4101191-JIHI-HEN-EXT
           END-IF
      *
           IF      SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)  =   "095" OR "096"
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
      *****        MOVE    SPA-COM-KISOTEN (SPA-GMN-IDX)
      *                                        TO  SPA-COM-KEI
      *                                                   (SPA-GMN-IDX)
      *            MOVE    SPA-COM-KISOTEN (SPA-GMN-IDX)
      *                                        TO  SPA-GMN-KEI
      *****                                               (SPA-GMN-IDX)
               END-IF
           ELSE
               IF      WRK-SRYKBN          =   "95"  OR  "96"
                   MOVE    "2"                 TO  SPA-COM-KEIFLG
                                                          (SPA-GMN-IDX)
      *****        MOVE    ISET-SURYO1         TO  SPA-COM-KEI
      *                                                   (SPA-GMN-IDX)
      *            MOVE    ISET-SURYO1         TO  SPA-GMN-KEI
      *                                                   (SPA-GMN-IDX)
      *            MOVE    1                   TO  SPA-COM-SURYO
      ****                                                (SPA-GMN-IDX)
               END-IF
           END-IF
      *
           .
       4101191-JIHI-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    約束処方の処理
      *****************************************************************
       4101192-INPSET-SET-SEC           SECTION.
      *
      *    入力文字を数字へ変換
           MOVE    1                   TO  WRK-IDX-FT
           PERFORM 4101121-MOJI-SET-SEC
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
      *****EVALUATE    WRK-CD-MCNT
      *        WHEN    4
      *            MOVE    "4"                 TO  ICD-CDSYU
      *        WHEN    5
      *            MOVE    "5"                 TO  ICD-CDSYU
      *        WHEN    6
      *            MOVE    "6"                 TO  ICD-CDSYU
      *        WHEN    OTHER
      *            MOVE    "0001"              TO  SPA-ERRCD
      *            GO  TO                  4101192-INPSET-SET-EXT
      **** END-EVALUATE
      *
      **   MOVE    LNK-SC97-CDSYU      TO  ICD-CDSYU
           MOVE    "6"                 TO  ICD-CDSYU
           MOVE    WRK-INPUTCD             TO  ICD-INPUTCD
      *
           PERFORM 930-INPUTCD-READ-SEC
      *
           IF      FLG-INPUTCD     NOT =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
               GO  TO                  41011921-INPSET-MEI-EXT
           END-IF
           MOVE    ICD-DSPNAME         TO  SPA-GMN-MEISYO(SPA-GMN-IDX)
      *    回数入力としない
           MOVE    SPACE               TO  SPA-COM-KAIFLG (SPA-GMN-IDX)
      *
      *
           .
       41011921-INPSET-MEI-EXT.
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
      *    剤分離処理
           INITIALIZE                  ORCSC93AREA
           MOVE    "J04"               TO  LNK-SC93-PG
      *    小児科外来診療科があり、３歳満の時
           IF      (SPA-SYONIKA        =   1   )  AND
                   (SPA-NAI-NENREI-YY  <   3   )
               MOVE    1                   TO  LNK-SC93-SYONIKA-ARI2
           END-IF
      *    寝たきり在宅科算定有りとする
           MOVE    1                   TO  LNK-SC93-NETAKIRI-ARI
           CALL    "ORCSC93"           USING
                                       MCPAREA
                                       SPA-SCR-REC
                                       SPA-AREA
                                       ORCSC93AREA
                                       MSYU-DATA-REC
           IF      SPA-SRYKBN      NOT =   SPA-COM-SRYKBN(01)
               MOVE    "1020"          TO  SPA-ERRCD
               MOVE    "1"             TO  SPA-COM-CUR  (01)
               MOVE    SPACE           TO  SPA-MAE-INPUTCD (01)
               GO      TO      510-TBLHEN-EXT
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      510-TBLHEN-EXT
           END-IF
      *!!
      *    内分泌負担試験編集
           MOVE    ZERO                TO  FLG-NAIFUTAN
           PERFORM VARYING     IDX     FROM    1   BY   1
                   UNTIL      (IDX     >   200     )  OR
                             ((SPA-GMN-INPUTCD (IDX)   =   SPACE )  AND
                              (SPA-COM-INPUTCD (IDX)   =   SPACE ))
      *        内分泌負担試験有無
               IF     (SPA-COM-SRYKBN (IDX)    =   "60"  )  AND
                      (SPA-COM-DATAKBN(IDX)    =   "1"   )  AND
                      (SPA-COM-HOUKNSKBN(IDX)  =   08    )
                   MOVE    1               TO  FLG-NAIFUTAN
               END-IF
           END-PERFORM
           IF      FLG-NAIFUTAN        =   1
               INITIALIZE                  ORCSSANFUKAAREA
               MOVE    2                   TO  LNK-SANFUKA-SYORI
               MOVE    "J04"               TO  LNK-SANFUKA-PG
               MOVE    SPA-J02-SRYYMD      TO  LNK-SANFUKA-SRYYMD
      *        訂正時の診療コード
               MOVE    SPA-J04-TEISEI-AREA TO  LNK-SANFUKA-TEISEI-AREA
               CALL    "ORCSSANFUKA"       USING
                                           ORCSSANFUKAAREA
                                           SPA-AREA
                                           SPA-SCR-REC
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      510-TBLHEN-EXT
           END-IF
      *!!
      *
      *    診療種別件数クリア
           INITIALIZE                  SPA-SRYKBN-AREA
      *    剤ごとの処理
           MOVE    ZERO                TO  IDX-Z
           MOVE    ZERO                TO  WRK-KAISU
           INITIALIZE                  WRK-SCR-REC
           INITIALIZE                  WRK-HZN-SCR-AREA
           INITIALIZE                  WRK-HOZ-IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   200      )   OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE )  OR
                              (SPA-ERRCD           NOT =   SPACE )
      *        剤ごとの算定処理で変更される加算データは対象としない
      *DDDDDDDDIF      SPA-COM-AUTOKBN (IDX)    =   "2"
      *DD          CONTINUE
      *DD      ELSE
               IF     ((SPA-COM-SRYKBN (IDX)   =   "11"  OR  "12") AND
                       (SPA-COM-AUTOKBN(IDX)   =   "2"   )  )  OR
      *            注射の残量廃棄も対象外
                      ((SPA-COM-SRYKBN (IDX)(1:1)  =   "3") AND
                       (SPA-COM-AUTOKBN(IDX)   =   "2"   )  AND
                       (SPA-COM-INPUTCD(IDX)   =   "099309901"))
                   CONTINUE
               ELSE
                   ADD     1               TO  IDX-Z
                   MOVE    SPA-COM-TBLREC (IDX)
                                           TO  WRK-COM-TBLREC(IDX-Z)
                   MOVE    SPA-GMN-TBLREC (IDX)
                                           TO  WRK-GMN-TBLREC(IDX-Z)
               END-IF
      *DDDDDDDDEND-IF
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
      *            剤ごとの処理
                   PERFORM 5102-ZAIBETU-SEC
                   INITIALIZE                  WRK-SCR-REC
                   MOVE    ZERO                TO  IDX-Z
                   MOVE    ZERO                TO  WRK-KAISU
               END-IF
           END-PERFORM
      *
      *    警告メッセージの時、ＳＰＡの再セットを行う
      *    IF     (SPA-ERRCD       NOT =   SPACE)  AND
      *           (SPA-ERRCD(1:1)  NOT =   "K"  )
      *        CONTINUE
      *    ELSE
      *
      *    スパ領域へ戻す
           MOVE    ZERO                TO  SPA-GMN-MAX
           INITIALIZE                  SPA-GMN-REC
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   200
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
      *****END-IF
      *
      *D   MOVE    WRK-GOKEI-9         TO  SPA-GMN-GOKEI
      *
      *
           IF      SPA-GMN-MAX         =   ZERO
               MOVE    1               TO  SPA-MAX-PAGE
           ELSE
               COMPUTE SPA-MAX-PAGE    =  (SPA-GMN-MAX    -  1  )
                                       /   20     +    1
           END-IF
      *
      *    禁忌チェックを行う
           IF      FLG-TOROKU          =   SPACE
               PERFORM 51010-KNKCHK-SEC
               IF      FLG-KINKI           =   1
                   GO      TO      510-TBLHEN-EXT
               END-IF
           END-IF
      *
      *    登録前の全体のチェックを行う
      *D   IF      FLG-TOROKU      NOT =   "2"
      *D       INITIALIZE                  ORCSC00AREA
      *D       MOVE    "2"                 TO  LNK-ORCSC-KBN
      *D       PERFORM 520-ALL-CHEK-SEC
      *D       IF      SPA-ERRCD   NOT =   SPACE
      *D           DISPLAY "WRK-COM-KEIFLG:" WRK-COM-KEIFLG (3)
      *D       END-IF
      *D   END-IF
           .
        510-TBLHEN-EXT.
           EXIT.
      *
      *
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
           MOVE    "J04"               TO  LNK-ORCSC-PG
           MOVE    WRK-KAISU           TO  LNK-ORCSC-KAISU
      **** MOVE    SPA-MAP-IDX         TO  LNK-ORCSC-MAP-IDX
      *****MOVE    SPA-GMN-IDX         TO  LNK-ORCSC-GMN-IDX
           MOVE    SPA-ROUJIN          TO  LNK-ORCSC-ROUJIN 
           MOVE    SPA-NAI-NENREI      TO  LNK-ORCSC-NENREI 
      *D   MOVE    SPA-LSTYMD          TO  LNK-ORCSC-LSTYMD 
      *D   MOVE    SPA-LSTSRYKA        TO  LNK-ORCSC-LSTSRYKA 
      *D   MOVE    SPA-LSTSRYKAMEI     TO  LNK-ORCSC-LSTSRYKAMEI 
           MOVE    SPA-NAI-INGAIKBN    TO  LNK-ORCSC-INGAIKBN
           MOVE    FLG-TOROKU          TO  LNK-ORCSC-TOROKU
      *    特定薬剤治療管理加算の初回算定日
           MOVE    SPA-TOKTEI-YMD      TO  LNK-ORCSC-TOKTEI-YMD
      *    時間外特例区分
           MOVE    SPA-JIKANTOKU       TO  LNK-ORCSC-JIKANTOKU
      *    時間外加算特例乳幼児
           MOVE    SPA-JIKANTOKU-NYU   TO  LNK-ORCSC-JIKANTOKU-NYU
      *    小児科外来診療科があり、３歳満の時
           IF      (SPA-SYONIKA        =   1   )  AND
                   (SPA-NAI-NENREI-YY  <   3   )
               MOVE    1                   TO  LNK-ORCSC-SYONIKA-ARI2
           END-IF
      *    ２４時間連携体制加算は自動発生しない
           MOVE    1                   TO  LNK-ORCSC-NETAKIRI-ARI3
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
      *            特定薬剤治療管理加算の初回算定日
                   MOVE    LNK-ORCSC-TOKTEI-YMD    TO  SPA-TOKTEI-YMD
      *            薬剤
               WHEN    "21"
               WHEN    "22"
               WHEN    "23"
               WHEN    "24"
               WHEN    "25"
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
      *            入院（食事）
               WHEN    "97"
                   IF      SPA-NYUGAIKBN       =   1
                       CALL    "ORCSCN97"      USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
                       ADD     1               TO  SPA-SRYKBN-CNT (10)
                   ELSE
                       MOVE    "9020"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (01)
                   END-IF
           END-EVALUATE
      *
      *****MOVE    LNK-ORCSC-MAP-IDX   TO  SPA-MAP-IDX
      **** MOVE    LNK-ORCSC-GMN-IDX   TO  SPA-GMN-IDX
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
      *        約束セット行の編集
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1) =   "S"
                   MOVE    LNK-ORCSC-ZAITEN        TO  WRK-COM-TENSU
                                                                (IDX-Z)
                   MOVE    LNK-ORCSC-ZAIKEI        TO  WRK-COM-KEI
                                                               (IDX-Z)
                   MOVE    LNK-ORCSC-KAISU         TO  WRK-COM-KAISU
                                                               (IDX-Z)
               END-IF
      *
      *        最終行を剤終了とする
               IF      (IDX-Z              =   200 )  OR
                       (WRK-COM-INPUTCD(IDX-Z + 1) =   SPACE )
                   MOVE    "1"             TO  WRK-COM-ZAIEND (IDX-Z)
               ELSE
                   MOVE    SPACE           TO  WRK-COM-ZAIEND (IDX-Z)
               END-IF
      *        画面再編集・基本チェック
               INITIALIZE                  ORCSC94AREA
               MOVE    "2"                 TO  LNK-SC94-KBN
               MOVE    "J04"               TO  LNK-SC94-PG
               MOVE    IDX-Z               TO  LNK-SC94-IDX
               CALL    "ORCSC94"           USING
                                           ORCSC94AREA
                                           WRK-SCR-REC
                                           SPA-AREA
      *
               ADD     1                   TO  WRK-HOZ-IDX
               MOVE    WRK-COM-TBLREC (IDX-Z)
                                           TO  WRK-HZN-COM-TBLREC
                                                       (WRK-HOZ-IDX)
               MOVE    WRK-GMN-TBLREC (IDX-Z)
                                           TO  WRK-HZN-GMN-TBLREC
                                                       (WRK-HOZ-IDX)
      *
           END-PERFORM
           .
       5102-ZAIBETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌チェック処理
      *****************************************************************
       51010-KNKCHK-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-KINKI
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   200      )   OR
                              (SPA-GMN-INPUTCD (IDX-Z) =   SPACE )
               IF     (SPA-COM-SRYKBN  (IDX-Z)     =   "21" OR  "22"
                                                   OR  "23" OR  "31"
                                                   OR  "32" OR  "33")
                 AND  (SPA-COM-INPUTCD (IDX-Z)(1:1)    =  "6" )
      *            禁忌薬剤チェック処理
                   IF      (SPA-COM-KNKFLG (IDX-Z)  =   SPACE )  AND
                           (SPA-COM-KNKUMU (IDX-Z)  =   SPACE )
                       PERFORM 510222-KNKYAK-CHK-SEC
                   END-IF
               END-IF
           END-PERFORM
           .
       51010-KNKCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌薬剤チェック処理
      *****************************************************************
       510222-KNKYAK-CHK-SEC           SECTION.
      *    チェックレコードより、禁忌有無のチェックを行う
           INITIALIZE                  CHK-REC
           MOVE    "4"                 TO  CHK-CHKKBN
           MOVE    SPA-COM-INPUTCD (IDX-Z)
                                       TO  CHK-SRYCD
           MOVE    "2"                 TO  CHK-CDKBN
           MOVE    ZERO                TO  CHK-RENNUM
      *
           MOVE    ZERO                TO  FLG-CHKMST
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "CHK-KEY3"          TO  ORC-DBPATH
           MOVE     SPA-J02-SRYYMD     TO  ORC-DBYMD
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "CHK-KEY3"          TO  ORC-DBPATH
               PERFORM 950-CHK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-CHKMST
           END-IF
      *
           IF      FLG-CHKMST          =   1
               MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
               MOVE    "CHK-KEY3"          TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
      *
               MOVE    "1"             TO  SPA-COM-KNKUMU (IDX-Z)
               GO      TO      510222-KNKYAK-CHK-EXT
           END-IF
      *
      *    禁忌チェック
           MOVE    ZERO                TO  IDX-K
           INITIALIZE                  WRK-KNKCHK-AREA
           PERFORM
                   UNTIL      FLG-CHKMST        =   1
               PERFORM VARYING     IDX-C       FROM    1   BY  1
                       UNTIL      (IDX-C           >   10  )  OR
                                  (CHK-CD (IDX-C)  =   SPACE )
                   PERFORM 5102221-KNKCHK-SYORI-SEC
               END-PERFORM
      *
               MOVE    "CHK-KEY3"          TO  ORC-DBPATH
               PERFORM 950-CHK-READ-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "CHK-KEY3"          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           IF      IDX-K               >   ZERO
               MOVE    "1"             TO  SPA-COM-KNKFLG (IDX-Z)
               MOVE    SPA-GMN-MEISYO (IDX-Z)  TO  WRK-KNK-TOUMEI (01)
      *        同一診療コードをなくす
               PERFORM VARYING     IDX-C   FROM    1   BY  1
                       UNTIL       IDX-C   >   30
                   PERFORM VARYING     IDX-X   FROM    IDX-C   BY  1
                           UNTIL       IDX-X   >   30
                       IF     (IDX-C       NOT =   IDX-X   )   AND
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
           END-IF
           .
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
                   UNTIL      (IDX-S           >   100 )  OR
                              (SPA-KNK-SRYCD (IDX-S)   =   SPACE )
               IF     (SPA-KNK-SRYCD (IDX-S)   =   CHK-CD  (IDX-C)) AND
                      (SPA-KNK-TOGETU (IDX-S)  =   "1"  ) 
                   ADD     1                   TO  IDX-K
                   MOVE    SPA-KNK-SRYMEI (IDX-S)  TO  WRK-KNK-KNKMEI
                                                              (1 IDX-K)
                   MOVE    SPA-KNK-SRYCD (IDX-S)   TO  WRK-KNK-SRYCD
                                                              (1 IDX-K)
               END-IF
           END-PERFORM
      *
      *    画面入力分とのチェック
           PERFORM VARYING     IDX-S       FROM    1   BY  1
                   UNTIL      (IDX-S           >   200 )  OR
                              (SPA-GMN-INPUTCD(IDX-S)  =   SPACE )
               IF     (SPA-COM-SRYKBN  (IDX-S)     =   "21" OR  "22"
                                                   OR  "23" OR  "31"
                                                   OR  "32" OR  "33")
                 AND  (SPA-COM-INPUTCD (IDX-S)     =   CHK-CD  (IDX-C))
                 AND  (IDX-S                   NOT =   IDX-Z          )
                   ADD     1                   TO  IDX-K
                   MOVE    SPA-GMN-MEISYO (IDX-S)  TO  WRK-KNK-KNKMEI
                                                              (1 IDX-K)
                   MOVE    SPA-GMN-INPUTCD (IDX-S) TO  WRK-KNK-SRYCD
                                                              (1 IDX-K)
               END-IF
           END-PERFORM
           .
       5102221-KNKCHK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *     コメント等　編集処理
      *****************************************************************
       510211-SRYKBN-90-SEC            SECTION.
      *
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
           END-PERFORM
      *
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
      *41012-MEISYO-HEN-SEC        SECTION.
      *
      *    MOVE    J04-MEISYO (WRK-IDX2)
      *                                TO  SPA-GMN-MEISYO-G
      *                                                (SPA-GMN-IDX)
      *
      *    MOVE    2                   TO  SPA-GMN-CUR
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
      **** MOVE    ZERO                TO  IDX-MEI
      *    PERFORM VARYING     IDX     FROM    IDX-STR BY  1
      *            UNTIL       IDX     >   62
      *        IF      SPA-GMN-MEISYO-G (SPA-GMN-IDX)(IDX:1)
      *                                NOT =   SPACE
      *            MOVE    IDX         TO  IDX-MEI
      *        END-IF
      *    END-PERFORM
      *    MOVE    SPACE               TO  WRK-MEISYO
      *    MOVE    IDX-STR             TO  IDY
      *    MOVE    IDX-STR             TO  IDX
      *    PERFORM
      *            UNTIL      (IDX     >   IDX-MEI  )  OR
      *                       (SPA-ERRCD   NOT =   SPACE )
      *        IF      SPA-GMN-MEISYO-G (SPA-GMN-IDX)(IDX:1)
      *            半角空白を全角のへ
      *                                =   SPACE
      *            MOVE    "　"        TO  WRK-MEISYO(IDY:2)
      *            ADD     1           TO  IDX
      *            ADD     2           TO  IDY
      *        ELSE
      *            全角のみ入力可能とする。
      *            IF      SPA-GMN-MEISYO-G (SPA-GMN-IDX)(IDX:1)
      *                                    >=  X"A1"
      *                MOVE    SPA-GMN-MEISYO-G (SPA-GMN-IDX)(IDX:2)
      *                                    TO  WRK-MEISYO(IDY:2)
      *                ADD     2           TO  IDX
      *                ADD     2           TO  IDY
      *            ELSE
      *                MOVE    "0080"          TO  SPA-ERRCD
      *            END-IF
      *        END-IF
      *    END-PERFORM
      *    IF      SPA-ERRCD       NOT =   SPACE
      *        MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
      *        GO      TO      41012-MEISYO-HEN-EXT
      *    END-IF
      *
      *****MOVE    WRK-MEISYO      TO  SPA-GMN-MEISYO-G(SPA-GMN-IDX)
      *
      *    MOVE    "1"                 TO  SPA-COM-MEIFLG
      *                                                (SPA-GMN-IDX)
      *
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
      *    計 入力計算処理
      *****************************************************************
      *41013-KEI-HEN-SEC         SECTION.
      *    入力可能かのチェック
      *    IF      SPA-COM-KEIFLG (SPA-GMN-IDX)
      *                            NOT =   "1"
      *        MOVE    "1002"              TO  SPA-ERRCD
      *        GO      TO      41013-KEI-HEN-EXT
      *    END-IF
      *
      *    INITIALIZE                      ORCSNUMAREA
      *    MOVE    J04-KEI (WRK-IDX2)  TO  SNUM-INX
      *    CALL    "ORCSNUM"           USING
      *                                ORCSNUMAREA
      *    IF     (SNUM-RC         NOT =   ZERO  )   OR
      *           (SNUM-MINKBN         =   "1"   )   OR
      *           (SNUM-SYOKBN         =   "1"   )   OR
      *           (SNUM-SYOSUU     NOT =   ZERO  )
      *        MOVE    "1001"              TO  SPA-ERRCD
      *        GO      TO      41013-KEI-HEN-EXT
      *    ELSE
      *        MOVE    SNUM-OUTNUM     TO  SPA-GMN-KEI    (SPA-GMN-IDX)
      *    END-IF
      *    MOVE    SPA-GMN-KEI (SPA-GMN-IDX)
      *                                TO  SPA-COM-KEI    (SPA-GMN-IDX)
      *    MOVE    "1"                 TO  SPA-COM-KEIFLG (SPA-GMN-IDX)
      *    MOVE    "1"                 TO  SPA-COM-ZAIEND (SPA-GMN-IDX)
      *
      *    PERFORM 510-TBLHEN-SEC
      *
      *    MOVE    1                   TO  SPA-GMN-CUR
      *
      *    IF      SPA-ERRCD           =   SPACE
      *        ADD     1                   TO  SPA-GMN-MAX
      *        ADD     1                   TO  SPA-MAP-IDX
      *    END-IF
      *
      *    IF      SPA-MAP-IDX         >   20
      *        MOVE    20              TO  SPA-MAP-IDX
      *        MOVE    00              TO  SPA-GMN-CUR
      *    END-IF
      *
      *    .
      *41013-KEI-HEN-EXT.
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
           IF      SPA-GMN-MAX         =   ZERO
               MOVE    1               TO  SPA-MAX-PAGE
           ELSE
               COMPUTE SPA-MAX-PAGE    =  (SPA-GMN-MAX    -  1  )
                                       /   20     +    1
           END-IF
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
           IF      (SPA-GMN-IDX        =   1   )  OR
                   (SPA-COM-ZAIEND(SPA-GMN-IDX - 1 )
                                       =   "1" )
               CONTINUE
           ELSE
               MOVE    "0023"              TO  SPA-ERRCD
               GO      TO      41015-ZAIDEL-EXT
           END-IF
      *
           PERFORM VARYING     IDX     FROM    SPA-GMN-IDX   BY  1
                   UNTIL       IDX     >   200
               MOVE    SPACE           TO  SPA-GMN-INPUTCD(IDX)
               IF      SPA-COM-ZAIEND (IDX)    =   "1"
                   MOVE    200             TO  IDX
               END-IF
           END-PERFORM
      *
           PERFORM 510-TBLHEN-SEC
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
      *    減がある時、その次より
           IF      SPA-COM-INPUTCD (IDX2)  =   "820000047"
               ADD     1                   TO  IDX2
           END-IF
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
      *****************************************************************
      *    １行前へカーソル移動処理
      *****************************************************************
       41017-ZENGYOU-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-MAE
           IF      SPA-GMN-IDX         =   1
               GO      TO      41017-ZENGYOU-SET-EXT
           END-IF
      *    次の行がある時、削除行とする
           COMPUTE IDX                 =   SPA-GMN-IDX    +   1
           PERFORM VARYING     WRK-IDX     FROM    IDX    BY   1
                   UNTIL      (WRK-IDX     >   200 )  OR
                              (FLG-MAE     =   1   )
               IF     (SPA-GMN-INPUTCD (WRK-IDX)   NOT =   SPACE )  AND
                      (SPA-COM-SET     (WRK-IDX)       =   SPACE )
                   MOVE    1               TO  FLG-MAE
               END-IF
           END-PERFORM
           IF      FLG-MAE             =    1
               MOVE    ZERO            TO   FLG-MAE
               GO      TO      41017-ZENGYOU-SET-EXT
           END-IF
      *    カーソル位置決定
           COMPUTE IDX                 =   SPA-GMN-IDX    -   1
           MOVE    ZERO                TO  WRK-IN-IDX
           PERFORM VARYING     WRK-IDX     FROM    IDX  BY  -1
                   UNTIL      (WRK-IDX     <   1   )  OR
                              (WRK-IN-IDX  >   ZERO)
               IF     (SPA-GMN-INPUTCD (WRK-IDX)   NOT =   SPACE )  AND
                      (SPA-COM-SET     (WRK-IDX)       =   SPACE )
                   MOVE    WRK-IDX             TO  WRK-IN-IDX
               END-IF
           END-PERFORM
           IF      WRK-IN-IDX          =   ZERO
               GO      TO      41017-ZENGYOU-SET-EXT
           END-IF
      *
           MOVE    1                   TO  FLG-MAE
      *
           IF      SPA-MAP-IDX         =   1
               COMPUTE SPA-GMN-PAGE        =   SPA-GMN-PAGE
                                           -   1
           END-IF
      *
           MOVE    WRK-IN-IDX          TO  SPA-GMN-IDX
           MOVE    "1"                 TO  SPA-COM-CUR (SPA-GMN-IDX)
      *
           MOVE    1                   TO  FLG-MAE
      *
           .
       41017-ZENGYOU-SET-EXT.
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
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   200
               IF      WRK-GMN-INPUTCD(IDX)    =   SPACE
      *            削除行が加算料を自動追加した時、自動追加分も削除
                   IF     (WRK-COM-INPUTCD(IDX)    NOT =   SPACE )  AND
                          (IDX                         <   200   )  AND
                          (WRK-COM-AUTOKBN  (IDX + 1)  =   "1"   )
                       MOVE    SPACE           TO  WRK-GMN-INPUTCD
                                                             (IDX + 1)
                   END-IF
               ELSE
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
           END-PERFORM
      *
           IF      SPA-GMN-MAX         =   ZERO
               MOVE    1               TO  SPA-MAX-PAGE
           ELSE
               COMPUTE SPA-MAX-PAGE    =  (SPA-GMN-MAX    -  1  )
                                       /   20     +    1
           END-IF
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
      *****MOVE    SPA-MAP-IDX         TO  LNK-ORCSC-MAP-IDX
      *****MOVE    SPA-GMN-IDX         TO  LNK-ORCSC-GMN-IDX
           MOVE    SPA-ROUJIN          TO  LNK-ORCSC-ROUJIN 
           MOVE    SPA-NAI-NENREI      TO  LNK-ORCSC-NENREI 
           MOVE    SPA-NAI-INGAIKBN    TO  LNK-ORCSC-INGAIKBN
           MOVE    FLG-TOROKU          TO  LNK-ORCSC-TOROKU
      *D   MOVE    SPA-NAIACCT-AREA    TO  LNK-NAIACCT-AREA
      *    特定薬剤治療管理加算の初回算定日
           MOVE    SPA-TOKTEI-YMD      TO LNK-ORCSC-TOKTEI-YMD
      *    時間外特例区分
           MOVE    SPA-JIKANTOKU       TO  LNK-ORCSC-JIKANTOKU
      *    時間外加算特例乳幼児
           MOVE    SPA-JIKANTOKU-NYU   TO  LNK-ORCSC-JIKANTOKU-NYU
      *    小児科外来診療科があり、３歳満の時
           IF      (SPA-SYONIKA        =   1   )  AND
                   (SPA-NAI-NENREI-YY  <   3   )
               MOVE    1                   TO  LNK-ORCSC-SYONIKA-ARI2
           END-IF
      *    ２４時間連携体制加算は自動発生しない
           MOVE    1                   TO  LNK-ORCSC-NETAKIRI-ARI3
      *            診療
           IF      SPA-SRYKBN-CNT (01) >   ZERO
               CALL    "ORCSC10"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      ******** MOVE    LNK-ORCSC-MAP-IDX   TO  SPA-MAP-IDX
      ******** MOVE    LNK-ORCSC-GMN-IDX   TO  SPA-GMN-IDX
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            投薬
           IF      SPA-SRYKBN-CNT (02) >   ZERO
               CALL    "ORCSC20"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *********MOVE    LNK-ORCSC-MAP-IDX   TO  SPA-MAP-IDX
     **********MOVE    LNK-ORCSC-GMN-IDX   TO  SPA-GMN-IDX
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            注射
           IF      SPA-SRYKBN-CNT (03) >   ZERO
               CALL    "ORCSC30"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *********MOVE    LNK-ORCSC-MAP-IDX   TO  SPA-MAP-IDX
      *********MOVE    LNK-ORCSC-GMN-IDX   TO  SPA-GMN-IDX
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            処置
           IF      SPA-SRYKBN-CNT (04) >   ZERO
               CALL    "ORCSC40"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *********MOVE    LNK-ORCSC-MAP-IDX   TO  SPA-MAP-IDX
      ******** MOVE    LNK-ORCSC-GMN-IDX   TO  SPA-GMN-IDX
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            手術
           IF      SPA-SRYKBN-CNT (05) >   ZERO
               CALL    "ORCSC50"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *********MOVE    LNK-ORCSC-MAP-IDX   TO  SPA-MAP-IDX
      ******** MOVE    LNK-ORCSC-GMN-IDX   TO  SPA-GMN-IDX
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            麻酔
           IF      SPA-SRYKBN-CNT (06) >   ZERO
               CALL    "ORCSC54"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *********MOVE    LNK-ORCSC-MAP-IDX   TO  SPA-MAP-IDX
      ******** MOVE    LNK-ORCSC-GMN-IDX   TO  SPA-GMN-IDX
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
      *********MOVE    LNK-ORCSC-MAP-IDX   TO  SPA-MAP-IDX
      *********MOVE    LNK-ORCSC-GMN-IDX   TO  SPA-GMN-IDX
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            画像診断
           IF      SPA-SRYKBN-CNT (08) >   ZERO
               CALL    "ORCSC70"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *********MOVE    LNK-ORCSC-MAP-IDX   TO  SPA-MAP-IDX
      ******** MOVE    LNK-ORCSC-GMN-IDX   TO  SPA-GMN-IDX
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            その他
           IF      SPA-SRYKBN-CNT (09) >   ZERO
               CALL    "ORCSC80"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *********MOVE    LNK-ORCSC-MAP-IDX   TO  SPA-MAP-IDX
      *********MOVE    LNK-ORCSC-GMN-IDX   TO  SPA-GMN-IDX
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *        入院（食事）
           IF      SPA-SRYKBN-CNT (10) >   ZERO
               CALL    "ORCSCN97"      USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
           END-IF
      *
           .
       520-ALL-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *    ファイル削除
           MOVE    SPA-TERMID          TO  FILE-TERMID
           MOVE    ZERO                TO  IF-RESULT
           MOVE    FILE-NAME1          TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
           MOVE    1                   TO  FLG-END2
      *
           MOVE    "J02"               TO  SPA-SAKIPG
           MOVE    "J04"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
      *****MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "J02"               TO  MCP-WINDOW
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
      *    行為セット　処理
      *****************************************************************
       420-KOUISET-SEC             SECTION.
      *
           .
       420-KOUISET-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号検索　処理
      *****************************************************************
       430-BANKENSAKU-SEC             SECTION.
      *
           .
       430-BANKENSAKU-EXT.
           EXIT.
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       450-MAEGMN-SEC             SECTION.
      *
      *    診療行為入力チェック
           PERFORM 41001-KOUI-ALLCHK-SEC
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
           MOVE    1                   TO  SPA-GMN-CUR
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
           PERFORM 41001-KOUI-ALLCHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (FLG-END             =   1     )
               GO      TO      460-ATOGMN-EXT
           END-IF
      *
           COMPUTE WRK-FREE            =   SPA-GMN-PAGE  *  20
           IF      SPA-GMN-INPUTCD(WRK-FREE)   =   SPACE
               GO  TO                 460-ATOGMN-EXT
           END-IF
      *
           ADD     1                   TO  SPA-GMN-PAGE
      *
           MOVE    1                   TO  SPA-GMN-CUR
           .
       460-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約登録へ
      *****************************************************************
       480-YOYAKU-SEC             SECTION.
      *
      *    診療行為入力チェック
           PERFORM 41001-KOUI-ALLCHK-SEC
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
      **** END-IF
      *
      *    画面移行用のパラメタ変更
           MOVE    "J04"               TO  SPA-MOTOPG
           MOVE    "J04"               TO  SPA-MOTOPG3
           MOVE    "Y01"               TO  SPA-SAKIPG
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOTUKEY        TO  LNK-COMMON
           MOVE    SPA-J04             TO  LNK-SCRSPA
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
      **** MOVE    "JOIN"              TO  MCP-PUTTYPE
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
           PERFORM 41001-KOUI-ALLCHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (FLG-END             =   1     )
               GO      TO      490-UKEICHI-EXT
           END-IF
      *    更新前のチェック
      *    MOVE    "1"                 TO  FLG-TOROKU
      *    PERFORM 510-TBLHEN-SEC
      **** MOVE    SPACE               TO  FLG-TOROKU
      *    IF      SPA-ERRCD       NOT =   SPACE
      *        GO      TO      490-UKEICHI-EXT
      *****END-IF
      *
           MOVE    "U01"               TO  SPA-SAKIPG
           MOVE    "J04"               TO  SPA-MOTOPG
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOTUKEY        TO  LNK-COMMON
           MOVE    SPA-J04             TO  LNK-SCRSPA
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
      *****MOVE    "JOIN"              TO  MCP-PUTTYPE
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
      *    更新前チェック処理
      *****************************************************************
       41001-KOUI-ALLCHK-SEC       SECTION.
      *
      *    画面をＳＰＡへ編集
           MOVE    1                   TO  WRK-IDX2
           PERFORM 2002-GMNSPA-SET-SEC
      *
      *    入力コード・名称チェック処理
           MOVE    "1"                 TO  FLG-TOROKU
           MOVE    1                   TO  WRK-STR
           PERFORM 410112-KOUI-SPACHK-SEC
           .
       41001-KOUI-ALLCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       4100-TOROKU-SEC             SECTION.
      *
      *    診療行為入力チェック
           PERFORM 41001-KOUI-ALLCHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (FLG-END             =   1     )
               GO      TO      4100-TOROKU-EXT
           END-IF
      *
      **** 更新前のチェック
      *    MOVE    "1"                 TO  FLG-TOROKU
      *    PERFORM 510-TBLHEN-SEC
      *    MOVE    SPACE               TO  FLG-TOROKU
      *    IF      SPA-ERRCD       NOT =   SPACE
      *        GO      TO      4100-TOROKU-EXT
      *****END-IF
      *
      *    禁忌更新前のチェック
           PERFORM 41001-YAKKNK-CHK-SEC
      *
      *     禁忌エラーの時、エラー表示へ
           IF      WRK-KNKCHK-AREA     NOT =   SPACE
               MOVE    "J03"               TO  SPA-J041-SAKI
               PERFORM 410019-POP-J041-SEC
               GO      TO      4100-TOROKU-EXT
           END-IF
      *
      *    剤のチェック
           PERFORM 45011-NAIYOU-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4100-TOROKU-EXT
           END-IF
      *    同一剤が存在している時、エラーとする
           IF     (FLG-OK              =   1   )  AND
                  (FLG-OK2             =   ZERO  OR  1  )
               MOVE    "1025"              TO  SPA-ERRCD
               GO      TO      4100-TOROKU-EXT
           END-IF
      *
      ***  IF     (FLG-OK2             =   1   )  AND
      *           (WRK-ZAINUM          =   ZERO )
      *        剤変更なしとして、処理を終了する
      *        PERFORM 210-BACK
      *        GO      TO      4100-TOROKU-EXT
      ***  END-IF
      *
      *    外用薬で回数が２８以上の時、警告表示
           IF      SPA-CHOKITOFLG      =   1
               IF      SPA-JID1-FLG        =   SPACE
                   MOVE    "0102"          TO  SPA-JIDCD
                   GO      TO      4100-TOROKU-EXT
               END-IF
           END-IF
      *
           MOVE    "0101"              TO  SPA-JIDCD
      *
      *    更新処理
      *****PERFORM 41002-KOUSIN-SYORI-SEC
           .
       4100-TOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       41002-KOUSIN-SYORI-SEC          SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    診療行為マスタ更新
           PERFORM 4501-SINKOUI-KOU-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41002-KOUSIN-SYORI-EXT
           END-IF
      *
           IF      SPA-SEL-RRKYMD      =   SPACE
      *        診療会計マスタ更新
               PERFORM 4502-SINKAI-KOU-SEC
           ELSE
      *        診療会計マスタ追加、受診履歴変更
               PERFORM 4504-SRYACCT-ADD-SEC
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41002-KOUSIN-SYORI-EXT
           END-IF
      *    内分泌負荷試験判定
           IF      FLG-NAIBUN          =   1
      *        内分泌負荷試験包括点数チェック
               INITIALIZE                  ORCSSANFUKAAREA
               INITIALIZE                  SPA-SCR-REC
               MOVE    3                   TO  LNK-SANFUKA-SYORI
               MOVE    "J04"               TO  LNK-SANFUKA-PG
               IF      SPA-SEL-RRKYMD      =   SPACE
                   MOVE    SPA-J02-SRYYMD(1:6) TO  LNK-SANFUKA-SRYYMD
                   MOVE    "01"                TO  LNK-SANFUKA-SRYYMD
                                                             (7:2)
               ELSE
                   MOVE    SPA-SEL-RRKYMD      TO  LNK-SANFUKA-SRYYMD
               END-IF
               CALL    "ORCSSANFUKA"       USING
                                           ORCSSANFUKAAREA
                                           SPA-AREA
                                           SPA-SCR-REC
               IF      LNK-SANFUKA-ERRMSG  NOT =   SPACE
                   MOVE    "K001"              TO  SPA-ERRCD
      *            警告済み
                   MOVE    1                    TO  SPA-KEI-FLG
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        終了
               PERFORM 410021-END-SYORI-SEC
           END-IF
      *
           .
       41002-KOUSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       410021-END-SYORI-SEC            SECTION.
      *
      *    ファイル削除
           MOVE    SPA-TERMID          TO  FILE-TERMID
           MOVE    ZERO                TO  IF-RESULT
           MOVE    FILE-NAME1          TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
           MOVE    1                   TO  FLG-END2
      *
           MOVE    "J02"               TO  SPA-SAKIPG
           MOVE    "J04"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
      *
           IF      WRK-PUTTYPE         =   SPACE
               MOVE    "CHANGE"            TO  MCP-PUTTYPE
           ELSE
               MOVE    "JOIN"              TO  MCP-PUTTYPE
           END-IF
      *
           MOVE    "J02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       410021-END-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスタ更新処理
      *****************************************************************
       4501-SINKOUI-KOU-SEC            SECTION.
      *
      *    剤のチェック
      *    PERFORM 45011-NAIYOU-CHK-SEC
      *    同一剤が存在している時、エラーとする
      *    IF      FLG-OK              =   1
      *        MOVE    "1025"              TO  SPA-ERRCD
      *        GO      TO      4501-SINKOUI-KOU-EXT
      *    END-IF
      *
      *    IF     (FLG-OK2             =   1   )  AND
      *           (WRK-ZAINUM          =   ZERO )
      *        剤変更なしとして、処理を終了する
      *        GO      TO      4501-SINKOUI-KOU-EXT
      *****END-IF
      *
           MOVE    ZERO                TO  FLG-NAIBUN
      *
      *    算定履歴の削除
           PERFORM 45013-SANTEI-DELETE-SEC
      *
           IF      SPA-SEL-RRKYMD      =   SPACE
      *        受診日選択なしの時、対象の剤をクリアする
               PERFORM 45012-DBDELETE-SEC
               MOVE    SPA-ZAINUM          TO  WRK-ZAINUM
           ELSE
      *        剤番号取得処理
               PERFORM 45013-ZAINUM-SET-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      4501-SINKOUI-KOU-EXT
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  WRK-TENSUKEI
           MOVE    ZERO                TO  WRK-KAISUKEI
           MOVE    ZERO                TO  WRK-KINGAK
           MOVE    ZERO                TO  WRK-MEINUM
           MOVE    ZERO                TO  WRK-ZAITEN
           MOVE    ZERO                TO  WRK-SRYCDKEI
           MOVE    ZERO                TO  WRK-SURYOKEI
           MOVE    ZERO                TO  WRK-MEISAISU
           INITIALIZE                      WRK-WKSRY-AREA
           MOVE    ZERO                TO  FLG-SYORI
      *    附加情報
           INITIALIZE                      WKSRYACTPLUS-REC
      *
      *    診療行為新規作成
           MOVE    ZERO                TO  IDS
           MOVE    ZERO                TO  WRK-RENNUM
           INITIALIZE                  SRYACT-REC
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-MAX
               IF      SPA-GMN-INPUTCD (IDX)(1:1)  =   "."
                   CONTINUE
               ELSE
                   ADD     1               TO  IDS
      *            診療行為内容編集
                   PERFORM 45101-SINKOUI-HEN-SEC
      *
                   IF      IDS             >=  5
      *                新規レコード追加
                       PERFORM 45102-SINKOU-INS-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      IDS                 >   ZERO
      *        新規レコード追加
               PERFORM 45102-SINKOU-INS-SEC
           END-IF
      *
           .
       4501-SINKOUI-KOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤のチェック処理
      *****************************************************************
       45011-NAIYOU-CHK-SEC              SECTION.
      *
      *    診療コード計計算
           MOVE    ZERO                TO  WRK-TENSUKEI
           MOVE    ZERO                TO  WRK-KAISUKEI
           MOVE    ZERO                TO  WRK-KINGAK
           MOVE    ZERO                TO  WRK-MEINUM
           MOVE    ZERO                TO  WRK-ZAITEN
           MOVE    ZERO                TO  WRK-SRYCDKEI
           MOVE    ZERO                TO  WRK-SURYOKEI
           MOVE    ZERO                TO  WRK-MEISAISU
           INITIALIZE                      WRK-WKSRY-AREA
      *    附加情報
           INITIALIZE                      WKSRYACTPLUS-REC
           MOVE    ZERO                TO  FLG-NAIBUN
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
             IF    SPA-GMN-INPUTCD(IDX)(1:1)   =   "." 
               CONTINUE
             ELSE
      *
      *        自費
      *********IF      SPA-COM-SRYKBN (IDX)    =   "95" OR "96"
               IF      SPA-COM-KEIFLG (IDX)    =   "1"  OR  "2"
      *************ADD     SPA-COM-KEI  (IDX)  TO  WRK-KINGAK
                   ADD     SPA-COM-JIHIMONEY  (IDX)  TO  WRK-KINGAK
               END-IF
      *
      *        画面の計を集計する（１剤で１件）
               IF      SPA-COM-ZAIEND (IDX)    =   "1"
      *************IF      SPA-COM-SRYKBN (IDX)    =   "95" OR "96"
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
      *
      *        各附加情報
               PERFORM 450112-WKSRYACTPLUS-SEC
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
           END-PERFORM
      *
      *    剤点数がゼロの時、自費を点数へ
           IF      WRK-ZAITEN          =   ZERO
               MOVE   WRK-JIHIMONEY        TO  WRK-ZAITEN
           END-IF
      *
      *    診療会計剤チェックサブ
           INITIALIZE                      ORCSCZAICHKAREA
           MOVE    "1"                     TO  ORCSCZAICHK-KBN
           MOVE    SPA-J01-SRYKA           TO  ORCSCZAICHK-SRYKA
           MOVE    SPA-J02-SRYYMD(1:6)
                                           TO  ORCSCZAICHK-SRYYM
           MOVE    SPA-SRYKBN              TO  ORCSCZAICHK-SRYKBN
           MOVE    SPA-COM-SRYSYUKBN(01)   TO  ORCSCZAICHK-SRYSYUKBN
           MOVE    SPA-HKNCOMBINUM         TO  ORCSCZAICHK-HKNCOMBI
           MOVE    WRK-SRYCDKEI            TO  ORCSCZAICHK-SRYCDTOTAL
           MOVE    WRK-ZAITEN              TO  ORCSCZAICHK-ZAITEN
           MOVE    WRK-MEISAISU            TO  ORCSCZAICHK-MEISAISU
           MOVE    WRK-SURYOKEI            TO  ORCSCZAICHK-SURYOUTOTAL
           MOVE    1                       TO  ORCSCZAICHK-ZAIKAISU
           MOVE    ZERO                TO  IDX-ATO
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL       IDX1        >   SPA-GMN-MAX
               IF      SPA-GMN-INPUTCD (IDX1)(1:1) NOT =   "."
                   ADD     1                   TO  IDX-ATO
                   MOVE    SPA-COM-INPUTCD (IDX1)
                                               TO  ORCSCZAICHK-SRYCD
                                                             (IDX-ATO)
                   MOVE    SPA-COM-SURYO   (IDX1)
                                               TO  ORCSCZAICHK-SRYSURYO
                                                             (IDX-ATO)
                   IF      SPA-COM-DATAKBN (IDX1)   =   "3"
                       MOVE    SPA-COM-BUNGSU (IDX)
                                               TO  ORCSCZAICHK-SRYKAISU
                                                             (IDX-ATO)
                   END-IF
      *            外用薬の入力回数
                   IF      SPA-COM-SRYKBN  (IDX1)   =   "23"
                       MOVE    SPA-CHOKIKAISU
                                               TO  ORCSCZAICHK-SRYKAISU
                                                             (IDX-ATO)
                   END-IF
      *            入力コメント
                   IF      SPA-COM-MEIFLG (IDX1)    =   "1"
                       IF     (SPA-COM-INPUTCD (IDX1)(1:2)
                                                       =   "83" OR "84")
                         OR   (SPA-COM-INPUTCD (IDX1)(1:4)
                                                       =   "0083"
                                                       OR  "0084" )
                           MOVE    SPA-GMN-MEISYO   (IDX1)
                                           TO  ORCSCZAICHK-INPUTCOMENT
                                                             (IDX-ATO)
                       ELSE
                           MOVE    SPA-GMN-MEISYO-G (IDX1)
                                           TO  ORCSCZAICHK-INPUTCOMENT
                                                             (IDX-ATO)
                       END-IF
                   ELSE
                       IF     (SPA-COM-INPUTCD(IDX)(1:2) =   "83"
                                                         OR  "84")
                          OR  (SPA-COM-INPUTCD(IDX)(1:4) =   "0083"
                                                         OR  "0084")
                           MOVE    SPA-GMN-MEISYO(IDX)
                                           TO  ORCSCZAICHK-INPUTCOMENT
                                                             (IDX-ATO)
                       END-IF
                   END-IF
                   IF     (SPA-COM-INPUTCD(IDX1)(1:1)  =   "8"   ) OR
                          (SPA-COM-INPUTCD(IDX1)(1:4)  =   "0084")
                       MOVE    SPA-COM-INPUTCHI-G(IDX1)
                                           TO  ORCSCZAICHK-INPUTCHI-G
                                                             (IDX-ATO)
                   END-IF
               END-IF
           END-PERFORM
      *    明細がゼロの時、エラー
           IF      WRK-MEISAISU        =   ZERO
               MOVE    "1031"              TO  SPA-ERRCD
           ELSE
               CALL    "ORCSCZAICHK"       USING   SPA-AREA
                                                   ORCSCZAICHKAREA
               MOVE    ORCSCZAICHK-ZAINUM  TO  WRK-ZAINUM
      *
      *    同一剤なし
           IF      WRK-ZAINUM          =   ZERO
               MOVE    ZERO            TO  FLG-OK
           ELSE
               MOVE    1               TO  FLG-OK
               IF      WRK-ZAINUM      =   SPA-ZAINUM
      *            変更なし
                   IF      SPA-SEL-RRKYMD      =   SPACE
                       MOVE    2               TO  FLG-OK2
                   ELSE
                       MOVE    1               TO  FLG-OK2
                   END-IF
               END-IF
           END-IF
      *
           END-IF
      *
           .
       45011-NAIYOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    各附加情報編集処理
      *****************************************************************
       450112-WKSRYACTPLUS-SEC          SECTION.
      *
      *    労災の円
           IF      SPA-HKNKBN          =   1
               MOVE    SPA-COM-RSIKIN(IDX) TO  WKSRYP-RSIKINGAKU
           ELSE
               MOVE    ZERO                TO  WKSRYP-RSIKINGAKU
           END-IF
      *    点数区分
           IF     (SPA-COM-TENSIKIBETU(IDX)    =   7
                                               OR  8  )  AND
                  (SPA-COM-KISOTEN (IDX)   =   SPA-COM-TENSU(IDX))
               MOVE    1                   TO  WKSRYP-PLUSKBN
           END-IF
      *    内分泌負荷試験用
           IF     (SPA-COM-SRYKBN(IDX)     =   "60" )  AND
                  (SPA-COM-DATAKBN(IDX)    =   "1"   )  AND
                  (SPA-COM-HOUKNSKBN(IDX)  =   08    )
      *        当月算定フラグ
               MOVE    1                   TO  WKSRYP-MSANTEITFLG
      *        当月算定点数区分
               IF      SPA-COM-TEN (IDX)   =   SPA-COM-KISOTEN(IDX)
                   MOVE    1               TO  WKSRYP-MSANTEITENKBN
               ELSE
                   MOVE    2               TO  WKSRYP-MSANTEITENKBN
               END-IF
      *        当月算定点数
               MOVE    SPA-COM-KISOTEN(IDX)    TO WKSRYP-MSANTEITEN
           END-IF
      *
      *    内服７種類逓減対象
           IF     (SPA-COM-SRYKBN(IDX)     =   "21" )  AND
                  (SPA-COM-INPUTCD(IDX)    =   "820000047")
               MOVE    1                   TO  WKSRYP-TEI-NKBN
           END-IF
      *
           .
       450112-WKSRYACTPLUS-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象の剤クリア処理
      *****************************************************************
       45012-DBDELETE-SEC              SECTION.
      *
      *    患者コメントをクリアする
           INITIALIZE                  PTCOM-REC
           MOVE    SPA-HOSPID          TO  PTCOM-HOSPID
           MOVE    SPA-GMN-PTID        TO  PTCOM-PTID
           MOVE    SPA-ZAINUM          TO  PTCOM-ZAINUM
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "PTCOM-KEY2"        TO  ORC-DBPATH
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *D   IF      MCP-RC          NOT =   ZERO
      *D       DISPLAY "J04 PTCOM DEL ERR;"    MCP-RC
      *D   END-IF
      *
      *    診療行為データ削除
           INITIALIZE                      SRYACT-REC
           MOVE    SPA-HOSPID          TO  SRY-HOSPID
           MOVE    SPA-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SRY-PTID
           MOVE    SPA-J01-SRYKA       TO  SRY-SRYKA
           MOVE    SPA-J02-SRYYMD(1:6) TO  SRY-SRYYM
           MOVE    SPA-ZAINUM          TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J04 SRYACT DEL ERR;"    MCP-RC
           END-IF
      *
           .
       45012-DBDELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *   算定履歴削除処理
      *****************************************************************
       45013-SANTEI-DELETE-SEC          SECTION.
      *
      *    算定履歴削除
           INITIALIZE                      SRYACT-REC
           MOVE    SPA-HOSPID          TO  SRY-HOSPID
           MOVE    SPA-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SRY-PTID
           MOVE    SPA-J01-SRYKA       TO  SRY-SRYKA
           MOVE    SPA-J02-SRYYMD(1:6) TO  SRY-SRYYM
           MOVE    SPA-ZAINUM          TO  SRY-ZAINUM
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
           PERFORM UNTIL     (FLG-SRYACT       =   1  )
               PERFORM VARYING     IDZ    FROM    1   BY  1
                       UNTIL      (IDZ        >   5   )  OR
                                  (SRY-SRYCD(IDZ)     =   SPACE )
      *            手技料の時、算定歴を判定する
                   IF     (SRY-SRYCD (IDZ)(1:1)    =   "1"  )
      *                システム予約で履歴があるもの
                       OR (SRY-SRYCD (IDZ)(1:3)    =   "099" )
      *
                       MOVE    SRY-SRYCD (IDZ)     TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
                       IF      TNS-SANTEIRRKKBN    NOT =   ZERO
                           PERFORM 450122-SANTEI-DEL-SEC
                       END-IF
                   END-IF
               END-PERFORM
      *
               MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
               PERFORM 900-SRYACT-NEXT-SEC
           END-PERFORM
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       45013-SANTEI-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *   算定履歴削除処理
      *****************************************************************
       450122-SANTEI-DEL-SEC          SECTION.
      *
           INITIALIZE                  ORCSSANTEIAREA
           MOVE    "3"                 TO  SSANTEI-SYORI
           MOVE    "J04"               TO  SSANTEI-PG
           IF      SPA-SEL-RRKYMD      =   SPACE
               MOVE    SRY-SRYYM           TO  SSANTEI-SRYYMD(1:6)
               MOVE    "01"                TO  SSANTEI-SRYYMD(7:2)
           ELSE
               MOVE    SPA-SEL-RRKYMD      TO  SSANTEI-SRYYMD
           END-IF
           MOVE    SRY-SRYCD (IDZ)     TO  SSANTEI-SRYCD
           MOVE    SRY-PTID            TO  SSANTEI-PTID
           MOVE    SRY-NYUGAIKBN       TO  SSANTEI-NYUGAIKBN
           MOVE    SRY-SRYKA           TO  SSANTEI-SRYKA
           MOVE    ZERO                TO  SSANTEI-MSANTEID
           MOVE    ZERO                TO  SSANTEI-MSANTEICNT
           MOVE    SPA-SEL-DAY-G       TO  SSANTEI-DAY-AREA
      *    特定薬剤の初回日
           MOVE    SPA-TOKTEI-YMD      TO  SSANTEI-SYOKAIYMD
      *
           CALL    "ORCSSANTEI"        USING
                                       ORCSSANTEIAREA
                                       SPA-AREA
      *    内分泌負荷試験削除フラグ
           IF      SSANTEI-MSANTEIDEL  =   1
               MOVE    1               TO  FLG-NAIBUN
           END-IF
      *
           .
       450122-SANTEI-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤番号取得処理
      *****************************************************************
       45013-ZAINUM-SET-SEC          SECTION.
      *
      *    患者マスターより、最終剤番号を再番し、患者マスターを更新する
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPID          TO  PTINF-HOSPID
           MOVE    SPA-GMN-PTID        TO  PTINF-PTID
           PERFORM 940-PTINF-READ-SEC
           IF      FLG-PTINF           =   1
               MOVE    "0002"          TO  SPA-ERRCD
               DISPLAY "J04 ZAINUM-SET ERR:" SPA-ERRCD
               GO      TO      45013-ZAINUM-SET-EXT
           END-IF
      *
      *    最大剤番号
           IF      PTINF-MAXZAINUM     =   ALL "9"
               MOVE    ZERO                TO  PTINF-MAXZAINUM
           END-IF
           ADD     1                   TO  PTINF-MAXZAINUM
           MOVE    PTINF-MAXZAINUM     TO  WRK-ZAINUM
      *
           MOVE    SMCNDATE-YMD        TO  PTINF-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTINF-UPHMS
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "PTINF UPDTE ERR:"   MCP-RC
               MOVE    "1028"              TO  SPA-ERRCD
           END-IF 
      *
           .
      *
       45013-ZAINUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    診療行為内容編集処理
      *****************************************************************
       45101-SINKOUI-HEN-SEC              SECTION.
      *
      *    行為コード
           MOVE    SPA-COM-INPUTCD(IDX)    TO  SRY-SRYCD   (IDS)
      *
      *    約束セット
           IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "S"
      *        行為コード
               MOVE    SPA-GMN-INPUTCD(IDX)(1:6)   TO  SRY-SRYCD (IDS)
           END-IF
      *
      *    数量
           MOVE    SPA-COM-SURYO  (IDX)    TO  SRY-SRYSURYO(IDS)
      *    回数
           IF      SPA-COM-DATAKBN (IDX)   =   "3"
               MOVE    SPA-COM-BUNGSU (IDX)
                                           TO  SRY-SRYKAISU(IDS)
           END-IF
      *    自動算定区分
           MOVE    SPA-COM-AUTOKBN (IDX)   TO  SRY-AUTOKBN (IDS)
      *    数量の入力がある時
           IF     (SPA-COM-AUTOKBN (IDX)   =   SPACE )  AND
                  (SPA-COM-SURFLG  (IDX)   =   "1"   )
               MOVE    "S"                     TO  SRY-AUTOKBN (IDS)
           END-IF
      *    時間外区分入力がある時、英字変換後セット
           IF      SPA-COM-AUTOKBN (IDX)    =   SPACE
                PERFORM 45022-JIKANGAI-SET-SEC
           END-IF
      *    入力コメント番号
           IF     (SPA-COM-INPUTCD(IDX)(1:1)   =   "8" )  OR
                  (SPA-COM-INPUTCD(IDX)(1:3)   =   "008")
               PERFORM 450111-KNJCOM-SYROI-SEC
      *******  MOVE    WRK-MEINUM          TO  SRY-INPUTNUM (IDS)
           END-IF
      *    自費
      *****IF      SPA-COM-SRYKBN (IDX)    =   "95" OR "96"
           IF      SPA-COM-KEIFLG (IDX)    =   "1"  OR  "2"
               MOVE    SPA-COM-JIHIMONEY(IDX)  TO  SRY-JIHIMONEY(IDS)
               ADD     SPA-COM-JIHIMONEY(IDX)  TO  WRK-KINGAK
               MOVE    WRK-KINGAK              TO  SRY-JIHIMONEYTOTAL
           END-IF
      *
      *
      *    画面の計を集計する（１剤で１件）
           IF      SPA-COM-ZAIEND (IDX)    =   "1"
      *********IF      SPA-COM-SRYKBN (IDX)    =   "95" OR  96"
               IF      SPA-COM-KEIFLG (IDX)   =   "1"  OR  "2"
                   MOVE    WRK-KINGAK              TO  WRK-ZAITEN
               ELSE
                   MOVE    SPA-COM-TENSU  (IDX)    TO  WRK-ZAITEN
               END-IF
               MOVE    SPA-COM-SYUTEN1(IDX)    TO  WRK-SYUTEN1
               MOVE    SPA-COM-SYUTEN2 (IDX)   TO  WRK-SYUTEN2
               MOVE    SPA-COM-YKZTEN  (IDX)   TO  WRK-YKZTEN
               MOVE    SPA-COM-KIZAITEN(IDX)   TO  WRK-KIZAITEN
           END-IF
      *****IF      SPA-COM-KEIFLG (IDX)    =   "1" OR  "2"
      *        MOVE    SPA-COM-KEI  (IDX)      TO  SRY-JIHIMONEY
      *        MOVE    SPA-COM-KEI  (IDX)      TO  WRK-ZAITEN
      *****END-IF
      *
      *    各附加情報
           PERFORM 450112-WKSRYACTPLUS-SEC
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
           END-IF
      *
      *    明細数
           ADD     1                       TO  WRK-MEISAISU
           .
       45101-SINKOUI-HEN-EXT.
           EXIT.
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
                       MOVE    "A"      TO  SRY-AUTOKBN  (IDS)
                   WHEN    "2"
                       MOVE    "B"      TO  SRY-AUTOKBN  (IDS)
                   WHEN    "3"
                       MOVE    "C"      TO  SRY-AUTOKBN  (IDS)
                   WHEN    "4"
                       MOVE    "D"      TO  SRY-AUTOKBN  (IDS)
                   WHEN    "5"
                       MOVE    "E"      TO  SRY-AUTOKBN  (IDS)
                   WHEN    "6"
                       MOVE    "F"      TO  SRY-AUTOKBN  (IDS)
                   WHEN    "7"
                       MOVE    "G"      TO  SRY-AUTOKBN  (IDS)
               END-EVALUATE
           END-IF
           .
       45022-JIKANGAI-SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    患者コメント作成処理
      *****************************************************************
       450111-KNJCOM-SYROI-SEC           SECTION.
      *
      *
           INITIALIZE                  PTCOM-REC
      *医療機関ＩＤ
           MOVE    SPA-HOSPID          TO  PTCOM-HOSPID
      *患者ＩＤ
           MOVE    SPA-GMN-PTID            TO  PTCOM-PTID
      *剤番号
      *****MOVE    SPA-ZAINUM          TO  PTCOM-ZAINUM
           MOVE    WRK-ZAINUM          TO  PTCOM-ZAINUM
      *診療コード
           MOVE    SRY-SRYCD   (IDS)   TO  PTCOM-SRYCD
      *入力コメント
           IF      SPA-COM-MEIFLG (IDX)    =   "1"
               IF     (SPA-COM-INPUTCD(IDX)(1:2) =   "83"
                                                 OR  "84" )  OR
                      (SPA-COM-INPUTCD(IDX)(1:4) =   "0083"
                                                 OR  "0084" )
                   MOVE    SPA-GMN-MEISYO (IDX)
                                       TO  PTCOM-INPUTCOMENT
               ELSE
                   MOVE    SPA-GMN-MEISYO-G (IDX)
                                       TO  PTCOM-INPUTCOMENT
               END-IF
           ELSE
               IF     (SPA-COM-INPUTCD(IDX)(1:1)   =   "8"  ) OR
                      (SPA-COM-INPUTCD(IDX)(1:4)   =   "0083"
                                                   OR  "0084")
                    MOVE    SPA-GMN-MEISYO (IDX)
                                               TO  PTCOM-INPUTCOMENT
               ELSE
      *             作成しない
                    GO      TO      450111-KNJCOM-SYROI-EXT
               END-IF
           END-IF
      *入力値
           MOVE    SPA-COM-INPUTCHI-G (IDX)
                                       TO  PTCOM-INPUTCHI-AREA
      *コメント枝番番号
           ADD     1                   TO  WRK-MEINUM
           MOVE    WRK-MEINUM          TO  PTCOM-RENNUM
      *
           MOVE    SMCNDATE-YMD        TO  PTCOM-CREYMD
           MOVE    SMCNDATE-YMD        TO  PTCOM-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTCOM-UPHMS
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "PTCOM-KEY"         TO  ORC-DBPATH
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J04 PTCOM INS ERR;"    MCP-RC
                       ",KEY:" PTCOM-KEY
               MOVE    "1029"              TO  SPA-ERRCD
           END-IF
      *
           MOVE    WRK-MEINUM          TO  SRY-INPUTNUM (IDS)
      *
           .
       450111-KNJCOM-SYROI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為新規レコード編集
      *****************************************************************
       45102-SINKOU-INS-SEC            SECTION.
      *
           ADD     1                   TO  WRK-RENNUM
           MOVE    SPA-HOSPID          TO  SRY-HOSPID
           MOVE    SPA-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SRY-PTID
           MOVE    SPA-J01-SRYKA       TO  SRY-SRYKA
           MOVE    SPA-J02-SRYYMD(1:6) TO  SRY-SRYYM
      *****MOVE    SPA-ZAINUM          TO  SRY-ZAINUM
           MOVE    WRK-ZAINUM          TO  SRY-ZAINUM
           MOVE    WRK-RENNUM          TO  SRY-RENNUM
      *
           MOVE    SPA-COM-SRYSYUKBN(01)
                                       TO  SRY-SRYSYUKBN
           MOVE    SPA-SRYKBN          TO  SRY-SRYKBN
      *
           MOVE    SMCNDATE-YMD        TO  SRY-CREYMD
           MOVE    SMCNDATE-YMD        TO  SRY-UPYMD
           MOVE    SMCNDATE-HMS        TO  SRY-UPHMS
      *
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "SRYACT-KEY"        TO  ORC-DBPATH
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J04 SRYACT INS ERR;"    MCP-RC 
               MOVE    "1024"          TO  SPA-ERRCD
           END-IF
      *
           MOVE    ZERO                TO  IDS
           INITIALIZE                  SRYACT-REC
      *
           .
       45102-SINKOU-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタ更新処理
      *****************************************************************
       4502-SINKAI-KOU-SEC          SECTION.
      *
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPID          TO  ACCT-HOSPID
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  ACCT-PTID
           MOVE    SPA-J01-SRYKA       TO  ACCT-SRYKA
           MOVE    SPA-J02-SRYYMD(1:6) TO  ACCT-SRYYM
           MOVE    SPA-SRYKBN          TO  ACCT-SRYKBN
           MOVE    SPA-ZAINUM          TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SRYACCT-KEY"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SRYACCT-KEY"       TO  ORC-DBPATH
               PERFORM 940-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SRYACCT-KEY"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           IF      FLG-SRYACCT      NOT =  ZERO
               MOVE    "1027"              TO  SPA-ERRCD
               GO      TO      4502-SINKAI-KOU-EXT
           END-IF
           IF      WRK-ZAITEN          =   ACCT-ZAITEN
               MOVE    ZERO            TO  FLG-SYUNOU-ERR
           ELSE
               MOVE    1               TO  FLG-SYUNOU-ERR
           END-IF
      *
      *    剤点数
           MOVE    WRK-ZAITEN          TO  ACCT-ZAITEN
      *    診療コード計
           MOVE    WRK-SRYCDKEI        TO  ACCT-SRYCDTOTAL
      *    数量計
           MOVE    WRK-SURYOKEI        TO  ACCT-SURYOUTOTAL
      *    明細数
           MOVE    WRK-MEISAISU        TO  ACCT-MEISAISU
      *    手技点数１
           MOVE    WRK-SYUTEN1         TO  ACCT-SYUTEN1
      *    手技点数２
           MOVE    WRK-SYUTEN2         TO  ACCT-SYUTEN2
      *    薬剤点数
           MOVE    WRK-YKZTEN          TO  ACCT-YKZTEN
      *    器材点数
           MOVE    WRK-KIZAITEN        TO  ACCT-KIZAITEN
      *
           MOVE    SMCNDATE-YMD        TO  ACCT-UPYMD
           MOVE    SMCNDATE-HMS        TO  ACCT-UPHMS
      *
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "SRYACCT-KEY"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J04 SRYACCT UPD ERR:"  MCP-RC
           END-IF
      *
      *    算定履歴追加
           PERFORM 450422-SANTEI-UPD-SEC
      *    収納変更処理
           IF     (SPA-NYUGAIKBN       =   "2" )  AND
                  (FLG-SYUNOU-ERR      =   1   )
               PERFORM 450423-SYUNOU-UPD1-SEC
           END-IF
      *
      *    レセプト作成管理マスタ更新処理
           IF     (SPA-NYUGAIKBN       =   "2" )  AND
                  (SPA-HKNCOMBINUM NOT =   "9999")
               PERFORM 45021-RECEUPD-UPDATE-SEC
           END-IF
      *
           .
      *
       4502-SINKAI-KOU-EXT.
           EXIT.
      *****************************************************************
      *   算定履歴追加処理
      *****************************************************************
       450422-SANTEI-UPD-SEC          SECTION.
      *
           IF     ACCT-HKNCOMBI        =   "9999"
               GO      TO      450422-SANTEI-UPD-EXT
           END-IF
      *
      *    自費の時対象外
           IF      ACCT-SRYKBN         =   "95"  OR  "96"
               GO      TO      450422-SANTEI-UPD-EXT
           END-IF
      *
      *    診療会計マスターから診療行為マスターを検索する
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
           PERFORM UNTIL     (FLG-SRYACT       =   1  )
               PERFORM VARYING     IDZ    FROM    1   BY  1
                       UNTIL      (IDZ        >   5   )  OR
                                  (SRY-SRYCD(IDZ)     =   SPACE )
      *        手技料の時、算定歴を判定する
                   IF     (SRY-SRYCD (IDZ)(1:1)    =   "1"   )
      *                システム予約で履歴があるもの
                       OR (SRY-SRYCD (IDZ)(1:3)    =   "099" )
      *
                       MOVE    SRY-SRYCD (IDZ)     TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
                       IF      TNS-SANTEIRRKKBN    NOT =   ZERO
                           PERFORM 450123-SANTEI-UPD-SEC
                       END-IF
                   END-IF
               END-PERFORM
      *
               MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
               PERFORM 900-SRYACT-NEXT-SEC
           END-PERFORM
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           .
      *
       450422-SANTEI-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *   算定履歴追加更新処理
      *****************************************************************
       450123-SANTEI-UPD-SEC          SECTION.
      *
           INITIALIZE                  ORCSSANTEIAREA
           MOVE    "1"                 TO  SSANTEI-SYORI
           MOVE    "J04"               TO  SSANTEI-PG
           MOVE    SRY-SRYCD (IDZ)     TO  SSANTEI-SRYCD
           MOVE    SRY-PTID            TO  SSANTEI-PTID
           MOVE    SRY-NYUGAIKBN       TO  SSANTEI-NYUGAIKBN
           MOVE    SRY-SRYKA           TO  SSANTEI-SRYKA
           MOVE    ZERO                TO  SSANTEI-MSANTEID
           MOVE    ZERO                TO  SSANTEI-MSANTEICNT
           MOVE    SPA-SEL-DAY-G       TO  SSANTEI-DAY-AREA
           IF      SPA-SEL-RRKYMD      =   SPACE
               MOVE    SRY-SRYYM           TO  SSANTEI-SRYYMD(1:6)
               MOVE    "01"                TO  SSANTEI-SRYYMD(7:2)
           ELSE
               MOVE    SPA-SEL-RRKYMD      TO  SSANTEI-SRYYMD
           END-IF
      *    特定薬剤の初回日
           MOVE    SPA-TOKTEI-YMD      TO  SSANTEI-SYOKAIYMD
      *    内分泌負荷試験用
           IF      WKSRYP-MSANTEITFLG  =   1
               MOVE    WKSRYP-MSANTEITEN
                                        TO  SSANTEI-MSANTEITEN
               MOVE    WKSRYP-MSANTEITENKBN
                                        TO  SSANTEI-MSANTEITENKBN
               MOVE    1                TO  FLG-NAIBUN
           END-IF
      *
           CALL    "ORCSSANTEI"        USING
                                       ORCSSANTEIAREA
                                       SPA-AREA
      *
           .
       450123-SANTEI-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *   更新時の収納更新処理
      *****************************************************************
       450423-SYUNOU-UPD1-SEC          SECTION.
      *
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPID          TO  JYURRK-HOSPID
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-J01-SRYKA       TO  JYURRK-SRYKA
           MOVE    SPA-J01-SRYKA       TO  JYURRK-SRYKA
           MOVE    SPA-HKNCOMBINUM     TO  JYURRK-HKNCOMBINUM
           PERFORM VARYING     IDZ     FROM    1   BY  1
                   UNTIL       IDZ     >   15
               MOVE    SPA-ZAINUM          TO  JYURRK-ZAINUM (IDZ)
           END-PERFORM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "JYURRK-KEY20"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "JYURRK-KEY20"      TO  ORC-DBPATH
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           MOVE    ZERO                TO  FLG-UPD
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
               IF      JYURRK-SRYYMD(1:6)  =   SPA-J02-SRYYMD(1:6)
      *            収納の会計変更区分変更
                   IF      SPA-NYUGAIKBN       =   "2"
                       PERFORM 450431-SYUNOU-UPD-SEC
                   END-IF
               END-IF
      *
               MOVE    "JYURRK-KEY20"          TO  ORC-DBPATH
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "JYURRK-KEY20"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
      *
       450423-SYUNOU-UPD1-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト作成管理マスタ更新処理
      *****************************************************************
       45021-RECEUPD-UPDATE-SEC           SECTION.
      *
           INITIALIZE                      ORCSRECEUPDAREA
           MOVE    "2"                 TO  LNK-UPD-SYORI
           MOVE    SPA-HOSPID          TO  LNK-UPD-HOSPID
           MOVE    SPA-GMN-PTID        TO  LNK-UPD-PTID
           MOVE    SPA-TERMID          TO  LNK-UPD-TERMID
           MOVE    SPA-OPID            TO  LNK-UPD-OPID
           MOVE    SPA-NYUGAIKBN       TO  LNK-UPD-NYUGAIKBN
           MOVE    SPA-SRYYMD(1:6)     TO  LNK-UPD-SRYYM
           MOVE    SPA-HKNKBN          TO  LNK-UPD-HKNKBN
           IF      SPA-HKNKBN          =   "1"
               EVALUATE    SPA-RSI-HKNKBN
                   WHEN    "1"
                   WHEN    "2"
                   WHEN    "3"
      *                 労災
                       MOVE    "1"                 TO  LNK-UPD-HKNKBN
                   WHEN    "4"
      *                 自賠責
                       MOVE    "2"                 TO  LNK-UPD-HKNKBN
               END-EVALUATE
           END-IF
           CALL    "ORCSRECEUPD"       USING    ORCSRECEUPDAREA
           IF      LNK-UPD-RC      NOT =   ZERO
               MOVE    "0005"              TO  SPA-ERRCD
               DISPLAY "LNK-UPD-RC =>" LNK-UPD-RC
           END-IF
      *
           .
       45021-RECEUPD-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療会計マスタ追加、受診履歴変更処理
      *****************************************************************
       4504-SRYACCT-ADD-SEC          SECTION.
      *
      *    変更前の診療会計マスタを修正する
           PERFORM 45041-SRYACCT-MAE-SEC
      *
      *    診療会計マスタ追加
           PERFORM 45042-SRYACCT-INS-SEC
      *
      *    受診履歴修正
           PERFORM 45043-JYURRK-UPD-SEC
      *
           .
      *
       4504-SRYACCT-ADD-EXT.
           EXIT.
      *****************************************************************
      *    前診療会計マスタ修正処理
      *****************************************************************
       45041-SRYACCT-MAE-SEC          SECTION.
      *
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPID          TO  ACCT-HOSPID
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  ACCT-PTID
           MOVE    SPA-J01-SRYKA       TO  ACCT-SRYKA
           MOVE    SPA-J02-SRYYMD(1:6) TO  ACCT-SRYYM
           MOVE    SPA-SRYKBN          TO  ACCT-SRYKBN
           MOVE    SPA-ZAINUM          TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SRYACCT-KEY"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SRYACCT-KEY"       TO  ORC-DBPATH
               PERFORM 940-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SRYACCT-KEY"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           IF      FLG-SRYACCT      NOT =  ZERO
               MOVE    "1027"              TO  SPA-ERRCD
               GO      TO      45041-SRYACCT-MAE-EXT
           END-IF
      *
      *    剤回数
           MOVE    SPA-SEL-RRKYMD(7:2) TO  IDX-D2
           COMPUTE IDX-D1              =   SPA-SEL-RENNUM   +  1
           MOVE    ACCT-DAY (IDX-D1 IDX-D2)
                                       TO  WRK-DAY
           COMPUTE ACCT-DAY (1 IDX-D2) =   ACCT-DAY (1 IDX-D2)
                                       -   WRK-DAY
           MOVE    ZERO                TO  ACCT-DAY
                                                (IDX-D1 IDX-D2)
           COMPUTE ACCT-ZAIKAISU       =   ACCT-ZAIKAISU
                                       -   WRK-DAY
      *
           MOVE    SMCNDATE-YMD        TO  ACCT-UPYMD
           MOVE    SMCNDATE-HMS        TO  ACCT-UPHMS
      *
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "SRYACCT-KEY"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J04 SRYACCT UPD ERR:"  MCP-RC
           END-IF
      *
           .
      *
       45041-SRYACCT-MAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタ追加処理
      *****************************************************************
       45042-SRYACCT-INS-SEC          SECTION.
      *
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPID          TO  ACCT-HOSPID
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  ACCT-PTID
           MOVE    SPA-J01-SRYKA       TO  ACCT-SRYKA
           MOVE    SPA-J02-SRYYMD(1:6) TO  ACCT-SRYYM
           MOVE    SPA-SRYKBN          TO  ACCT-SRYKBN
      *
           MOVE    WRK-ZAINUM          TO  ACCT-ZAINUM
      *    保険組合せ
           MOVE    SPA-HKNCOMBINUM     TO  ACCT-HKNCOMBI
      *    剤点数
           MOVE    WRK-ZAITEN          TO  ACCT-ZAITEN
      *    診療コード計
           MOVE    WRK-SRYCDKEI        TO  ACCT-SRYCDTOTAL
      *    数量計
           MOVE    WRK-SURYOKEI        TO  ACCT-SURYOUTOTAL
      *    明細数
           MOVE    WRK-MEISAISU        TO  ACCT-MEISAISU
      *    手技点数１
           MOVE    WRK-SYUTEN1         TO  ACCT-SYUTEN1
      *    手技点数２
           MOVE    WRK-SYUTEN2         TO  ACCT-SYUTEN2
      *    薬剤点数
           MOVE    WRK-YKZTEN          TO  ACCT-YKZTEN
      *    器材点数
           MOVE    WRK-KIZAITEN        TO  ACCT-KIZAITEN
      *
      *    剤回数
           MOVE    WRK-DAY             TO  ACCT-ZAIKAISU
      *    診療日
           MOVE    SPA-SEL-RRKYMD(7:2) TO  IDX-D2
           COMPUTE IDX-D1              =   SPA-SEL-RENNUM   +  1
           MOVE    WRK-DAY             TO  ACCT-DAY
                                             (IDX-D1 IDX-D2)
           MOVE    WRK-DAY             TO  ACCT-DAY
                                             (1    IDX-D2)
      *
           MOVE    SMCNDATE-YMD        TO  ACCT-CREYMD
           MOVE    SMCNDATE-YMD        TO  ACCT-UPYMD
           MOVE    SMCNDATE-HMS        TO  ACCT-UPHMS
      *
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "SRYACCT-KEY"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC          NOT =   ZERO
               MOVE    "1026"          TO  SPA-ERRCD
               DISPLAY "J04 SRYACCT INS ERR:"  MCP-RC
           END-IF
      *
      *    算定履歴追加
           PERFORM 450422-SANTEI-UPD-SEC
      *
           .
       45042-SRYACCT-INS-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴修正処理
      *****************************************************************
       45043-JYURRK-UPD-SEC          SECTION.
      *
      *    受診履歴の剤変更
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPID          TO  JYURRK-HOSPID
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-J01-SRYKA       TO  JYURRK-SRYKA
           IF      SPA-NYUGAIKBN       =   "2"
      *        外来の時、伝票番号
               MOVE    SPA-SEL-DENPNUM     TO  JYURRK-DENPNUM
               MOVE    "JYURRK-KEY6"       TO  WRK-PATH
           ELSE
      *        入院の時、診療年月日、保険組合せ
               MOVE    SPA-SEL-RRKYMD      TO  JYURRK-SRYYMD
               MOVE    SPA-SEL-RENNUM      TO  JYURRK-RENNUM
               MOVE    SPA-HKNCOMBINUM     TO  JYURRK-HKNCOMBINUM
               MOVE    "JYURRK-KEY19"      TO  WRK-PATH
           END-IF
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-PATH            TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-PATH            TO  ORC-DBPATH
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           MOVE    ZERO                TO  FLG-UPD
           PERFORM
                   UNTIL      (FLG-JYURRK      =   1  )  OR
                              (FLG-UPD         =   1  )
               IF     (SPA-SEL-RENNUM          =   JYURRK-RENNUM )  AND
                      (SPA-SEL-DOUJI-RENNUM    =   JYURRK-DOUJI-RENNUM)
                   PERFORM VARYING     IDX     FROM    1   BY  1
                           UNTIL       IDX     >   15
                       IF      JYURRK-ZAINUM (IDX)     =   SPA-ZAINUM
                           MOVE    WRK-ZAINUM      TO  JYURRK-ZAINUM
                                                                 (IDX)
                           MOVE    15              TO  IDX
                           MOVE    1               TO  FLG-UPD
                       END-IF
                   END-PERFORM
               END-IF
      *
               IF      FLG-UPD             =   1
      *            収納の会計変更区分変更
                   IF      SPA-NYUGAIKBN       =   "2"
                       PERFORM 450431-SYUNOU-UPD-SEC
                   END-IF
      *
                   MOVE    SMCNDATE-YMD        TO  JYURRK-UPYMD
                   MOVE    SMCNDATE-HMS        TO  JYURRK-UPHMS
      *
                   MOVE    JYURRK-REC          TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "JYURRK-KEY"        TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"          USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
               END-IF
      *
               MOVE    WRK-PATH               TO  ORC-DBPATH
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    WRK-PATH            TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       45043-JYURRK-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納更新処理
      *****************************************************************
       450431-SYUNOU-UPD-SEC          SECTION.
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    JYURRK-HOSPID       TO  SYU-HOSPID
           MOVE    JYURRK-PTID         TO  SYU-PTID
           MOVE    JYURRK-NYUGAIKBN    TO  SYU-NYUGAIKBN
           MOVE    JYURRK-DENPNUM      TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           IF      FLG-SYUNOU          =   ZERO
               MOVE    "1"                 TO  SYU-ACCT-UPDKBN
               MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
      *
               MOVE    SYUNOU-REC          TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           END-IF
      *
           .
       450431-SYUNOU-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療会計パラメタ更新処理
      *****************************************************************
       4503-PARA-KOU-SEC          SECTION.
      *
           INITIALIZE                      PARA-REC
           MOVE    "J02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SRYACCT"           TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PARA-KEY2"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PARA-KEY2"         TO  ORC-DBPATH
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           PERFORM
               UNTIL   FLG-PARA            =   1
      *
               MOVE    PARA-DATA-REC       TO  SRYACCT-REC
               IF     (ACCT-SRYKBN         =   SPA-SRYKBN)  AND
                      (ACCT-ZAINUM         =   SPA-ZAINUM)
      *            剤点数
                   MOVE    WRK-ZAITEN          TO  ACCT-ZAITEN
      *            診療コード計
                   MOVE    WRK-SRYCDKEI        TO  ACCT-SRYCDTOTAL
      *            数量計
                   MOVE    WRK-SURYOKEI        TO  ACCT-SURYOUTOTAL
      *            明細数
                   MOVE    WRK-MEISAISU        TO  ACCT-MEISAISU
      *            手技点数１
                   MOVE    WRK-SYUTEN1         TO  ACCT-SYUTEN1
      *            手技点数２
                   MOVE    WRK-SYUTEN2         TO  ACCT-SYUTEN2
      *            薬剤点数
                   MOVE    WRK-YKZTEN          TO  ACCT-YKZTEN
      *            器材点数
                   MOVE    WRK-KIZAITEN        TO  ACCT-KIZAITEN
      *
                   MOVE    SRYACCT-REC         TO  PARA-DATA-REC
      *
      *
                   MOVE    PARA-REC            TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "PARA-KEY"          TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"         USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "J04 PARA REWRITE ERR:"  STS-PARA
                   END-IF
                   MOVE    1                   TO  FLG-PARA
               ELSE
                   MOVE    "PARA-KEY2"         TO  ORC-DBPATH
                   PERFORM 990-PARA-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "PARA-KEY2"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
      *
       4503-PARA-KOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌更新前のチェック処理
      *****************************************************************
       41001-YAKKNK-CHK-SEC            SECTION.
      *    禁忌チェック
           MOVE    1                   TO  IDX-K1
           INITIALIZE                  WRK-KNKCHK-AREA
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
               IF     (SPA-COM-INPUTCD(IDX)(1:1)   =   "6"   )  AND
                      (SPA-COM-KNKUMU (IDX)        =   SPACE )  AND
                      (SPA-COM-SRYKBN (IDX)        =   "21" OR  "22"
                                                   OR "23"  OR  "31"
                                                   OR  "32" OR "33")
                   MOVE    ZERO                TO  IDX-K
      *           チェックレコードより、禁忌有無のチェックを行う
                   INITIALIZE                  CHK-REC
                   MOVE    "4"                 TO  CHK-CHKKBN
                   MOVE    SPA-COM-INPUTCD (IDX)
                                               TO  CHK-SRYCD
                   MOVE    "2"                 TO  CHK-CDKBN
      *
                   MOVE    CHK-REC             TO  MCPDATA-REC
                   MOVE    "DBSELECT"          TO  MCP-FUNC
                   MOVE    "CHK-KEY3"          TO  ORC-DBPATH
                   MOVE     SPA-J02-SRYYMD     TO  ORC-DBYMD
                   CALL    "ORCMCPSUB"          USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
                   IF      MCP-RC              =   ZERO
                       MOVE    "CHK-KEY3"          TO  ORC-DBPATH
                       PERFORM 950-CHK-READ-SEC
                   ELSE
                       MOVE    1                   TO  FLG-CHKMST
                   END-IF
                   PERFORM
                           UNTIL      FLG-CHKMST   =   1
                       PERFORM VARYING     IDX-C   FROM    1   BY  1
                               UNTIL      (IDX-C           >   10 ) OR
                                          (CHK-CD (IDX-C)  =   SPACE)
                           PERFORM 410011-YAKKNK-SYORI-SEC
                       END-PERFORM
      *
                       MOVE    "CHK-KEY3"          TO  ORC-DBPATH
                       PERFORM 950-CHK-READ-SEC
                   END-PERFORM
      *
                   MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
                   MOVE    "CHK-KEY3"          TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"          USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
      *
                   IF      IDX-K             >   ZERO
                      MOVE    SPA-COM-INPUTCD (IDX) TO WRK-KNK-TOUSRYCD
                                                              (IDX-K1)
                      MOVE    SPA-GMN-MEISYO  (IDX) TO  WRK-KNK-TOUMEI
                                                              (IDX-K1)
                      ADD     1                     TO  IDX-K1
                   END-IF
               END-IF
           END-PERFORM
      *
      *    テーブル再編集処理
           PERFORM 410012-YAKKNK-TBL-SEC
      *
           .
       41001-YAKKNK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    全件　禁忌薬剤チェック処理
      *****************************************************************
       410011-YAKKNK-SYORI-SEC           SECTION.
      *
      *    診療会計マスタとのチェック
           PERFORM VARYING     IDX-S       FROM    1   BY  1
                   UNTIL      (IDX-S           >   100 )  OR
                              (SPA-KNK-SRYCD (IDX-S)   =   SPACE )
               IF      SPA-KNK-SRYCD (IDX-S)   =   CHK-CD  (IDX-C)
                   PERFORM VARYING     IDX-S2      FROM    1   BY  1
                           UNTIL       IDX-S2      >   12
                       IF      SPA-KNK-SRYYM (IDX-S IDX-S2)
                                                   NOT =   SPACE
                           ADD     1               TO  IDX-K
                           MOVE    SPA-KNK-SRYMEI (IDX-S) 
                                                   TO  WRK-KNK-KNKMEI
                                                        (IDX-K1 IDX-K)
                           MOVE    SPA-KNK-SRYCD  (IDX-S)
                                                   TO  WRK-KNK-SRYCD
                                                         (IDX-K1 IDX-K)
                           MOVE    SPA-KNK-SRYYM (IDX-S IDX-S2)
                                                   TO  WRK-KNK-SRYYM
                                                         (IDX-K1 IDX-K)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *    画面入力分とのチェック
           PERFORM VARYING     IDX-S       FROM    1   BY  1
                   UNTIL      (IDX-S           >   40  )  OR
                              (SPA-GMN-INPUTCD(IDX-S)  =   SPACE )
               IF     (SPA-COM-SRYKBN  (IDX-S)     =   "21" OR  "22"
                                                   OR  "23" OR  "31"
                                                   OR  "32"  OR  "33")
                 AND  (SPA-COM-INPUTCD (IDX-S)     =   CHK-CD  (IDX-C))
                 AND  (IDX-S                   NOT =   IDX            )
                   ADD     1                       TO  IDX-K
                   MOVE    SPA-GMN-MEISYO (IDX-S)  TO  WRK-KNK-KNKMEI
                                                        (IDX-K1 IDX-K)
                   MOVE    SPA-GMN-INPUTCD(IDX-S)  TO  WRK-KNK-SRYCD
                                                        (IDX-K1 IDX-K)
                   MOVE    SPA-J02-SRYYMD(1:6)     TO  WRK-KNK-SRYYM
                                                        (IDX-K1 IDX-K)
               END-IF
           END-PERFORM
           .
       410011-YAKKNK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    全件　禁忌薬剤チェックテーブル再編集
      *****************************************************************
       410012-YAKKNK-TBL-SEC           SECTION.
      *
           PERFORM VARYING     IDX-K1      FROM    1   BY  1
                   UNTIL      (IDX-K1          >   30  )  OR
                              (WRK-KNK-TOUSRYCD (IDX-K1)  =   SPACE )
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
           END-PERFORM
      *
           .
       410012-YAKKNK-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌エラーメッセージ表示（入力ごと）
      *****************************************************************
       510229-POP-ERR-SEC              SECTION.
      *
           MOVE    SPACE               TO  J042
           MOVE    WRK-KNK-TOUMEI (1)  TO  J042-POP-TOUMEI
      *
           MOVE    ZERO                TO  IDX-X
           PERFORM VARYING     IDX-K   FROM    1   BY  1
                   UNTIL      (IDX-K       >   30  )   OR
                              (IDX-X       >=  3   )  
               IF      WRK-KNK-KNKMEI (1 IDX-K)    NOT =   SPACE
                   ADD     1               TO  IDX-X
                   MOVE    WRK-KNK-KNKMEI (1 IDX-K)
                                           TO  J042-KNKMEI (IDX-X)
               END-IF
           END-PERFORM
      *
           MOVE    IDX-X               TO  J042-COUNT
      *    初期カーソル設定
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "J042"              TO  SPA-SAKIPG
           MOVE    "J04"               TO  SPA-MOTOPG
      *
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "J042"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510229-POP-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌エラーメッセージ表示（登録前）
      *****************************************************************
       410019-POP-J041-SEC              SECTION.
      *
           MOVE    SPACE               TO  J041
      *
           MOVE    1                   TO  IDX-K
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   30  )   OR
                              (IDX-K       >=  50  )
                IF      WRK-KNK-TOUMEI (IDX1)  NOT =   SPACE
                    MOVE    WRK-KNK-TOUMEI (IDX1)  TO  J041-TOUMEI
                                                            (IDX-K)
                END-IF
                PERFORM VARYING    IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   30  )   OR
                                  (WRK-KNK-TOUMEI (IDX1)   =   SPACE )
                   IF      WRK-KNK-KNKMEI (IDX1 IDX2)  NOT =   SPACE
                       MOVE    WRK-KNK-KNKMEI (IDX1 IDX2)
                                               TO  J041-KNKMEI (IDX-K)
                       MOVE    WRK-KNK-SRYYM(IDX1 IDX2)
                                               TO  WRK-SYMD(1:6)
                       MOVE    01              TO  WRK-SDD
                       PERFORM 520-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG     TO  J041-TOUYM  (IDX-K)
                       ADD     1               TO  IDX-K
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           IF      IDX-K               >   1
               COMPUTE IDX-K           =   IDX-K       -   1
           END-IF
           MOVE    IDX-K               TO  J041-COUNT
      *
      *    COMPUTE SPA-J041-PAGE       =  (IDX-K       -    1  )
      *                                /   20
      *                                +   1
      *
      *    初期カーソル設定
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "J041"              TO  SPA-SAKIPG
           MOVE    "J04"               TO  SPA-MOTOPG
      *
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "J041"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
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
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-JIDCD       NOT =   SPACE
               PERFORM 520-JIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "J04    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
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
           MOVE    SPACE               TO  J04
           INITIALIZE                      J04
      *
           MOVE    ZERO                TO  IDX
      *
           PERFORM VARYING     WRK-IDX     FROM    1   BY  1
                   UNTIL      (WRK-IDX     >   200  )  OR
                              (IDX         >=  20   )
      *        画面表示
               IF      SPA-COM-PAGE(WRK-IDX)   =   SPA-GMN-PAGE
                   ADD     1               TO  IDX
                   MOVE    SPA-GMN-SRYKBN (WRK-IDX)
                                               TO  J04-SRYKBN(IDX)
                   MOVE    SPA-GMN-INPUTCD(WRK-IDX)
                                               TO  J04-INPUTCD(IDX)
                   MOVE    SPA-GMN-MEISYO-G(WRK-IDX)
                                               TO  J04-MEISYO(IDX)
                   PERFORM 5002-SURYO-HEN-SEC
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-PTNUM       TO  J04-PTNUM
           MOVE    SPA-GMN-KANANAME    TO  J04-KANANAME
           MOVE    SPA-GMN-NAME        TO  J04-NAME
           MOVE    SPA-GMN-SEX         TO  J04-SEX
           MOVE    SPA-J02-HKNCOMBIMEI TO  J04-HKNCOMBI
           MOVE    SPA-J02-FTNRATEMEI  TO  J04-RATE
           MOVE    SPA-J02-SRYYMDW(1:6)    TO  J04-SRYYM
           MOVE    SPA-GMN-BIRTHDAYW   TO  J04-BIRTHDAY
           MOVE    SPA-J01-SRYKAMEI    TO  J04-SRYKA
      *
           MOVE    SPA-SEL-RRKYMDG     TO  J04-RRKYMD
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   31
               MOVE    SPA-SEL-DAY (IDX)    TO  WRK-ZZ
               MOVE    WRK-ZZ               TO  J04-DAY(IDX)
           END-PERFORM
      *
           IF      FLG-END             =   ZERO
               PERFORM 5001-MAPCUR-SEC
           END-IF
           .
       500-GMNHEN-EXT.
           EXIT.
      *
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
                       IF      SPA-COM-CUR (WRK-IDX)   =   "1"
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
                       IF     (SPA-COM-IN2 (IDX)   =   "3" ) AND
                              (SPA-COM-IN  (IDX)   =   "1" )
                           MOVE    IDX             TO  WRK-MAP-CUR3
                           MOVE    WRK-PAGE        TO  WRK-MAP-PAGE3
                       END-IF
                   END-IF
      *
                   MOVE    WRK-PAGE        TO  SPA-COM-PAGE(WRK-IDX)
                   MOVE    IDX             TO  SPA-COM-GYOU(WRK-IDX)
      *
                   IF      SPA-ERRCD       =   SPACE
                       MOVE    SPACE       TO  SPA-COM-CUR (WRK-IDX)
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
               IF      SPA-MAP-IDX         >   20
                   OR  SPA-GMN-PAGE        <   WRK-MAP-MAXPAGE
                   MOVE    99                  TO  SPA-MAP-IDX
               ELSE
                   IF      SPA-GMN-PAGE    >   WRK-MAP-MAXPAGE
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
      *    金額入力時のみ、編集
           IF      SPA-COM-KEIFLG (WRK-IDX)   =   SPACE
               MOVE    SPACE                   TO  WRK-GOKEI-X
           ELSE
               MOVE    SPA-GMN-KEI (WRK-IDX)   TO  WRK-GOKEI-Z
           END-IF
      *
      *    画面編集
           MOVE    SPACE               TO  WRK-SURYO-HENSYU
           IF       WRK-GOKEI-X        =   SPACE
               MOVE    SPA-GMN-SURYO   (WRK-IDX)   TO  WRK-H-SURYO
               MOVE    SPA-GMN-TANINAME(WRK-IDX)   TO  WRK-H-TANINAME
           ELSE
               MOVE    WRK-GOKEI-X                 TO  WRK-H-SURYO
           END-IF
           MOVE    SPA-GMN-TENSU(WRK-IDX)   TO  WRK-H-ZAIKEI
      *
           MOVE    WRK-SURYO-HENSYU    TO  J04-SURYO (IDX)
           MOVE    "user-font"         TO  J04-SURYO-STYLE(IDX)
      *
           .
       5002-SURYO-HEN-EXT.
           EXIT.
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
           IF      WRK-IDX2            >   20
               MOVE    00              TO  SPA-GMN-CUR
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
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
           IF      SPA-ERRCD           =   "K001"
               MOVE    LNK-SANFUKA-ERRMSG  TO  SPA-ERRMSG
           ELSE
               MOVE    "J04"               TO  LNK-SC96-PG
               CALL    "ORCSC96"           USING
                                           SPA-AREA
                                           ORCSC96AREA
           END-IF
      *
           MOVE    SPACE               TO  JERR
           MOVE    SPA-ERRCD           TO  JERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  JERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "J04"               TO  SPA-MOTOPG
           MOVE    "JERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "JERR"              TO  MCP-WINDOW.
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
       520-JIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-JIDCD
               WHEN    "0101"
                   MOVE    "診療内容を変更登録します。"
                                       TO  WRK-JIDMSG(1:26)
                   MOVE
                   "回数も登録されますので取消ができなくなります"
                                       TO  WRK-JIDMSG(27:)
               WHEN    "0102"
                   MOVE    "外用薬を総量で登録します。"
                                       TO  WRK-JIDMSG
                   MOVE    "診療行為訂正時では、回数が１回となります。"
                                       TO  WRK-JIDMSG(27:)
               WHEN    OTHER
                   MOVE    SPA-ERRCD
                                       TO  WRK-JIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-JID1-FLG
      *
           MOVE    SPACE               TO  JID1
           MOVE    SPA-JIDCD           TO  JID1-ID1CODE
           MOVE    WRK-JIDMSG          TO  JID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "J04"               TO  SPA-MOTOPG
           MOVE    "JID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "JID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-JIDSET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-KANRIMST-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-KANRIMST-READ-EXT.
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
       940-SRYACCT-READ-EXT.
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
           MOVE    SPA-J02-SRYYMD      TO  ORC-DBYMD
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
      *
      *****************************************************************
      *    入力コードマスター次読込
      *****************************************************************
       920-ENT-NEXT-SEC         SECTION.
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
       920-ENT-NEXT-EXT.
           EXIT.
      *
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
           END-IF
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
      *    診療会計マスター読込
      *****************************************************************
       930-SRYACCT-NEXT-SEC         SECTION.
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
       930-SRYACCT-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメント読込
      *****************************************************************
       960-PTCOM-READ-SEC         SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTCOM-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
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
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "PTCOM-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       960-PTCOM-NEXT-EXT.
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
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       940-PTINF-READ-SEC         SECTION.
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
                   MOVE    MCPDATA-REC        TO  PTINF-REC
                   MOVE    ZERO               TO  FLG-PTINF
               ELSE
                   MOVE    1                  TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1              TO  FLG-PTINF
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
       940-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴順読み
      *****************************************************************
       970-JYURRK-NEXT-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  JYURRK-REC 
               MOVE    ZERO                TO  FLG-JYURRK
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-NEXT-EXT.
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
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           .
       990-PARA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタデータ順読み
      *****************************************************************
      *990-PARA-NEXT-SEC           SECTION.
      *
      *    READ    PARA-FILE       NEXT
      *        AT  END
      *            MOVE    1           TO  FLG-PARA
      *        NOT AT  END
      *            MOVE    ZERO        TO  FLG-PARA
      *    END-READ
      *
      *    .
      *990-PARA-NEXT-EXT.
      *    EXIT.
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
