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
       PROGRAM-ID.                 ORCGK08.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為確認　（Ｋ０８）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  01/05/25    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-多々納   02/04/12  自費の修正
      *  01.00.02    NACL-多々納  02/07/19  判断料の削除
      *  01.00.03    NACL-多々納  02/09/13  自賠責の追加
      *  01.00.04    NACL-多々納  02/09/18  内服薬粉砕の追加
      *  01.00.05    NACL-多々納  02/10/18  入院の追加
      *  01.00.06    NACL-多々納  02/11/20  自賠責器材追加
      *  01.00.07    NACL-多々納  02/12/02  食事の追加
      *  01.00.08    NACL-多々納  02/12/13  継続区分再編集、継続訂正修正
      *  01.00.09    NACL-多々納  03/01/16  自費回数表示修正
      *  01.00.10    NACL-多々納  03/03/05  排他制御処理追加
      *  01.00.11    NACL-多々納  03/04/30  自費編集修正
      *  01.00.12    NACL-多々納  03/05/13  自賠責の腰部固定帯加算修正
      *  01.00.13    NACL-多々納  03/08/25  福岡県地方公費対応
      *  01.00.14    NACL-多々納  03/09/04  算定済メッセージ編集追加
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
       WORKING-STORAGE             SECTION.
      *
      *    パラメタファイル名
       01  FILE-NAME2.
           03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
           03  FILLER              PIC X(07)   VALUE   "K01PARA".
           03  FILE-TERMID2        PIC X(64)   VALUE   SPACE.
           03  FILLER              PIC X(06)   VALUE   ".TXT".
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    画面スパ領域
       01  SPA-K08.
           03  SPA-K08-AREA.
               05  SPA-GMN-PAGE                  PIC 9(02).
               05  SPA-MAX-PAGE                  PIC 9(02).
               05  SPA-GMN-LINE                  PIC 9(02).
               05  SPA-K08-MAX                   PIC 9(04).
               05  SPA-GMN-CUR                   PIC 9(02).
      *
               05  SPA-GMN-TBL.
                   07  SPA-GMN-TBLREC  OCCURS  600.
                       09  SPA-GMN-TBANGO      PIC 9(03).
                       09  SPA-GMN-TDELFLG     PIC 9(01).
                       09  SPA-GMN-TMEISYO     PIC X(200).
                       09  SPA-GMN-TMEISYO-1   REDEFINES
                                                   SPA-GMN-TMEISYO.
                           11  SPA-GMN-TMEISYO1    PIC X(200).
                       09  SPA-GMN-TMEISYO-2   REDEFINES
                                               SPA-GMN-TMEISYO.
                           11  SPA-GMN-TBANGO2     PIC X(02).
                           11  SPA-GMN-TMEISYO2    PIC X(60).
                           11  SPA-GMN-HEN1.
                               13  SPA-GMN-TSURYOU     PIC X(18).
                               13  SPA-GMN-TTANI1      PIC X(08).
                               13  SPA-GMN-TTANMEI     PIC X(06).
                               13  SPA-GMN-TTANI2      PIC X(04).
                               13  SPA-GMN-TYOBI1      PIC X(32).
                           11  SPA-GMN-HEN2    REDEFINES
                                               SPA-GMN-HEN1.
                               13  SPA-GMN-TYOBI       PIC X(02).
                               13  SPA-GMN-TZAIKEI     PIC X(60).
                               13  SPA-GMN-TYOBI2      PIC X(06).
                           11  SPA-GMN-HEN3    REDEFINES
                                               SPA-GMN-HEN1.
                               13  SPA-GMN-TSURYOU3    PIC X(18).
                               13  SPA-GMN-TTANI13     PIC X(08).
                               13  SPA-GMN-TZAIKEI3    PIC X(40).
                               13  SPA-GMN-TZAIKEI3-R  REDEFINES
                                                       SPA-GMN-TZAIKEI3.
                                   15  SPA-GMN-TYOBI32     PIC X(06).
                                   15  SPA-GMN-TYOBI33     PIC X(34).
                           11  SPA-GMN-HEN4    REDEFINES
                                               SPA-GMN-HEN1.
                               13  SPA-GMN-TYOBI41     PIC X(16).
                               13  SPA-GMN-TZAIKEI4    PIC X(10).
                               13  SPA-GMN-TKINGAK4    PIC X(40).
      *                内部位置
                       09  SPA-GMN-NAIIDX      PIC 9(03).
                       09  SPA-GMN-NAIBANGO    PIC 9(03).
      *
               05  SPA-GMN-GOKEI             PIC 9(11).
               05  SPA-GMN-LASTYMD           PIC X(09).
               05  SPA-GMN-SYOSINYMD         PIC X(09).
               05  SPA-GMN-MISYUMONEY        PIC S9(08).
      *
               05  SPA-GMN-HKNTEN-TBL        OCCURS  10.
                   07  SPA-GMN-HKNTEN        PIC 9(07).
      *
               05  SPA-GMN-SELNUM-G.
                   07  SPA-GMN-SELNUM        PIC 9(03)   OCCURS  10.
      *        メッセージ
               05  SPA-GMN-MSGDATA           PIC X(100).
      *
           03  SPA-NAI-AREA.
               05  SPA-NAI-TBL.
                   07  SPA-NAI-TBLREC  OCCURS  600.
                       09  SPA-NAI-BANGO           PIC 9(03).
                       09  SPA-NAI-EDANUM          PIC 9(03).
                       09  SPA-NAI-ZAINUM          PIC 9(08).
                       09  SPA-NAI-SRYKBN          PIC X(02).
                       09  SPA-NAI-HKNTEN          PIC 9(08).
                       09  SPA-NAI-TENSFLG         PIC 9(01).
                       09  SPA-NAI-DELFLG          PIC 9(01).
                       09  SPA-NAI-HKNSFLG         PIC 9(01).
                       09  SPA-NAI-HKNCOMBI        PIC X(04).
                       09  SPA-NAI-SRYKA           PIC X(02).
      *
                       09  SPA-NAI-JIDFLG          PIC 9(01).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
           03  STS-SRYKBN          PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SRYKBN          PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-SINKI           PIC 9(01).
           03  FLG-YAKUSOKU        PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-DEL             PIC 9(01).
           03  FLG-DEL-ARI         PIC 9(01).
           03  FLG-RSIFLG          PIC 9(01).
           03  FLG-KAISUU          PIC 9(01).
           03  FLG-KAISUU-ARI      PIC 9(01).
           03  FLG-KAISUU-ARI2     PIC 9(01).
           03  FLG-KAISUU-ARI3     PIC 9(01).
      *
           03  FLG-KAKU-FLG        PIC 9(01).
           03  FLG-SYORI-END       PIC 9(01).
      *
           03  FLG-SYOSIN-OK       PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDS                 PIC 9(04).
           03  IDG                 PIC 9(04).
           03  IDM                 PIC 9(04).
           03  IDK1                PIC 9(04).
           03  IDK2                PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX-KOUI            PIC 9(04).
           03  IDX-SIN             PIC 9(04).
           03  IDX-ATO             PIC 9(04).
           03  IDX-YAKUZAI         PIC 9(04).
      *
           03  IDX-Y               PIC 9(04).
           03  IDX-Y2              PIC 9(02).
           03  IDX-Y3              PIC 9(02).
      *
           03  IDH                 PIC 9(04).
           03  IDH1                PIC 9(04).
           03  IDH2                PIC 9(04).
           03  IDH3                PIC 9(04).
           03  IDK                 PIC 9(04).
           03  IDX-MAX             PIC 9(04).
      *
           03  IDS1                PIC 9(04).
           03  IDS2                PIC 9(04).
           03  IDS3                PIC 9(04).
      *    ＳＰＡ内番号
           03  IDX-NAI             PIC 9(04).
      *
      *    福岡県対応用
           03  IDX-SYU             PIC 9(04).
           03  IDX-SYU2            PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
      *
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WFIL1       PIC X(01).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WFIL2       PIC X(01).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *    保険組合せ
           03  WRK-HKNCOMBI          PIC X(04).
           03  WRK-HKNCOMBIMEI       PIC X(60).
           03  WRK-SRYKA             PIC X(02).
           03  WRK-SRYKAMEI          PIC X(60).
           03  WRK-DRCD              PIC X(05).
           03  WRK-DRCD-MEI          PIC X(50).
      *    選択番号
           03  WRK-SELNUM          PIC 9(03).
      *    画面番号
           03  WRK-BANGO           PIC 9(04).
      *
           03  WRK-TENSU-X.
               05  WRK-TENSU       PIC Z(08).
           03  WRK-KAISU-X.
      *!*******05  WRK-KAISU       PIC Z(08).
               05  WRK-KAISU       PIC Z(03).
           03  WRK-KEI-X.
               05  WRK-KEI         PIC -(08).
           03  WRK-KEI-G           PIC 9(08).
           03  WRK-KEI-GS          PIC S9(08).
           03  WRK-KAISU-9         PIC 9(03).
      *
           03  WRK-ZZ              PIC ZZZ.
      *
      *    診療行為名称
           03  WRK-SRYSYUMEI       PIC X(40).
      *
           03  WRK-MEISYO          PIC X(40).
      *    数量表示用
           03  WRK-SURYO           PIC 9(05)V9(03).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(03).
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1    PIC ZZZZZ.
               05  WRK-SURYO-Z1-9  REDEFINES   WRK-SURYO-Z1
                                   PIC ZZZZ9.
               05  WRK-SURYO-Z2    PIC X(01).
               05  WRK-SURYO-Z3    PIC ZZZ.
           03  WRK-SURYO-J         PIC X(18).
      *    分画数画面編集用
           03  WRK-BUNGSU-X.
               05  WRK-BUNGSU-Z    PIC ZZZ.
           03  WRK-BUNGSU-J        PIC X(06).
      *    剤回数等画面編集用
      *!***03  WRK-KAISU-H         PIC X(08).
           03  WRK-KAISU-H         PIC X(03).
           03  WRK-KAISU-H2        PIC X(03).
           03  WRK-KEI-H           PIC X(08).
           03  WRK-ZAIKEI          PIC X(40).
           03  WRK-IDG             PIC 9(04).
           03  WRK-ZAIKEIJ         PIC X(80).
           03  IDJ                 PIC 9(04).
           03  IDJ1                PIC 9(01).
           03  IDJ2                PIC 9(04).
      *
           03  WRK-ZAIKEIJIH       PIC X(15).
           03  WRK-ZAIKEIJIJ       PIC X(30).
           03  WRK-KAISU-J         PIC X(16).
      *
           03  WRK-Z9              PIC Z,ZZZ,ZZZ.
           03  WRK-Z92             PIC --,---,---.
      *
           03  WRK-KIN             PIC 9(07).
      *
           03  WRK-ZAITEN              PIC 9(08).
           03  WRK-SRYKAISUKEI         PIC 9(03).
           03  WRK-SRYSURYOKEI         PIC 9(07)V9(03).
           03  WRK-MEISAISU            PIC 9(07).
           03  WRK-JIHIMONEY           PIC 9(07).
           03  WRK-JIHIMONEY-KEI       PIC 9(07).
      *
           03  WRK-SEIKYU              PIC S9(07).
      *
           03  WRK-SINDAN              PIC S9(07).
      *
           03  WRK-DENPNUM         PIC 9(07).
           03  WRK-MEIBAN          PIC 9(03).
      *
           03  WRK-ERRMSG          PIC X(40).
      *
           03  WRK-GOKEI-X2.
               05  WRK-GOKEI-Z2    PIC ZZZ,ZZZ,ZZZ.
           03  WRK-GOKEI-X3.
               05  WRK-GOKEI-Z3    PIC ---,---,---.
      *
           03  WRK-PATH                    PIC X(64).
           03  WRK-MCP-WIDGET              PIC X(20).
      *     一部負担金計算用
           03  WRK-S01-TENSU           PIC 9(08).
           03  WRK-ACCT-KAISU          PIC 9(03).
      *    院内、院外判定用
           03  WRK-CHK-SRYSYUKBN       PIC X(03).
      *     一部負担金計算用
           03  WRK-SYUTEN1             PIC 9(08).
           03  WRK-TEN                 PIC 9(08).
      *
      *    診療行為ＳＯＲＴ
           03  WRK-SORT-KEY.
               05  WRK-ZAINUM-X.
                   07  WRK-ZAINUM          PIC  9(08).
               05  WRK-RENNUM-X.
                   07  WRK-RENNUM          PIC  9(02).
               05  WRK-SRYKBN          PIC  X(02).
           03  WRK-MAX                 PIC  9(04).
      *    日数
           03  WRK-NISUU               PIC  9(02).
      *
      *    粉砕判定用
           03  WRK-FUNSA-AREA.
               05  FLG-FUNSAI          PIC 9(01).
               05  WRK-FUNSAI-CNT      PIC 9(04).
      *
           03  WRK-CONTKBN         PIC X(01).
      **** 03  WRK-SYU-CONTKBN         PIC X(01).
      *
           03  WRK-KIDMSG          PIC X(80).
      *
           03  WRK-K08-ROW         PIC S9(09).
           03  FLG-GMN-INIT        PIC  9(01).
      *請求金額
           03  WRK-SKYMONEY            PIC S9(07).
      *差額の絶対値
           03  WRK-GRP-SGKMONEY        PIC  9(07).
      *
      *    継続の時、自動発生のチェック
       01  WRK-CONTCHK-TBL.
           03  WRK-IDX-Y               PIC 9(04).
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
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *   日本語数字
       01  TBL-SUJI-AREA.
           03  FILLER          PIC X(20)   VALUE
              "１２３４５６７８９０".
       01  TBL-SUJI-R          REDEFINES   TBL-SUJI-AREA.
           03  TBL-SUJI        PIC X(02)   OCCURS   10.
      *
      *    パラメタファイル内容保存
       01  HZN-PARA-AREA.
           03  HZN-PARA-REC                 OCCURS  5.
               05  HZN-PARA-KEY.
                   07  HZN-PARA-FILEMEI         PIC X(08).
                   07  HZN-PARA-EDANUM          PIC 9(03).
               05  HZN-PARA-DATA-REC            PIC X(60000).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    職員情報
           COPY    "CPSK1010.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    収納マスタワーク
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    収納マスタワーク
       01  WRK-SYUNOU-REC.
           COPY    "CPSYUNOU.INC"      REPLACING
                                       //SYU-// BY //WRK-SYU-//.
      *    収納マスタワーク
       01  LNK-SYUNOU-AREA.
           02  LNK-SYUNOU-REC      OCCURS   15.
           COPY    "CPSYUNOU.INC"      REPLACING
                                       //SYU-// BY //LNK-SYU-//.
       01  LNK-SYUNOU-MAX              PIC 9(04).
      *
      *    収納マスタワーク２
       01  LNK-SYUNOU2-AREA.
           02  LNK-SYUNOU2-REC      OCCURS   15.
           COPY    "CPSYUNOU.INC"      REPLACING
                                       //SYU-// BY //LNK-SYU2-//.
       01  LNK-SYUNOU2-MAX              PIC 9(04).
      *
      *    診療行為マスタワーク
       01  LNK-WKSRYACT-AREA.
           02  LNK-WKSRYACT-REC      OCCURS   600.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //LNK-WKSRY-//.
       01  LNK-WKSRYACT-MAX           PIC 9(04).
      *
      *    診療行為マスタワーク
       01  HZN-WKSRYACT-AREA.
           02  HZN-WKSRYACT-REC      OCCURS   600.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //HZN-WKSRY-//.
       01  HZN-WKSRYACT-MAX           PIC 9(04).
      *
      *    診療行為マスタワーク
       01  SRT-WKSRYACT-AREA.
           02  SRT-WKSRYACT-REC      OCCURS   600.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //SRT-WKSRY-//.
      *
      *    診療行為マスタワーク
       01  WRK-WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                    //WKSRY-// BY //WRK-WKSRY-//.
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
      *    負担金額計算用パラメタ
           COPY    "CPORCS01.INC".
      *
      *    全角変換サブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    保険年齢・負担割合算定サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    診療行為入力項目変換パラメタ
           COPY    "CPORCSC97.INC".
      *
      *    診療行為入院更新パラメタ領域
           COPY    "CPORCSCNYUIN.INC".
      *
      *    診療行為外来更新パラメタ領域
           COPY    "CPORCSCGAIRAI.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSHKNSET.INC".
      *
      *福岡県対応用
      *    給付対象外点数編集サブパラメタ
           COPY    "CPORCSSYU40.INC".
      *
      *
      *    ＳＰＡパラメタ（ORCSC97.CBL 用のDAMMY)
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
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
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-K08
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
      *    診療種別テーブルセット
           PERFORM 1001-SRYKBN-SET-SEC
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
           MOVE    SPA-K01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-K08         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    診療種別テーブルセット処理
      *****************************************************************
       1001-SRYKBN-SET-SEC             SECTION.
      *
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
           .
      *
       1001-SRYKBN-SET-EXT.
           EXIT.
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
           IF      SPA-MOTOPG          =   "KERR"
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
               IF      FLG-END             =   1
                   GO      TO      100-INIT-EXT
               END-IF
           END-IF
      *
      *****MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   SPACE                TO  MCP-PUTTYPE.
           MOVE   "K08"                TO  MCP-WINDOW.
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
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "KID1"
      *            確認画面より
                   PERFORM 30010-KID1-SET-SEC
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-KIDCD
      *
           INITIALIZE                  SPA-K08
           INITIALIZE                  SPA-K08-AREA
           PERFORM                 310-SPA-SET-SEC
      *
      *    削除区分設置
           PERFORM 41031-DELFLG-SET-SEC
      *
           IF      FLG-DEL-ARI         =   ZERO
               MOVE    11              TO  SPA-GMN-CUR
           ELSE
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＫＩＤ１）ＯＫ処理
      *****************************************************************
       30010-KID1-SET-SEC          SECTION.
      *
           EVALUATE    SPA-KIDCD
      *        入院削除確認
               WHEN    "0101"
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    1               TO  FLG-KAKU-FLG
                       PERFORM 4502-NYUKOUSIN-SYORI-SEC
                   END-IF
      *        包括まとめ確認
               WHEN    "0102"
               WHEN    "0103"
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    1               TO  FLG-KAKU-FLG
                       PERFORM 4503-HOUKATU-SYORI-SEC
                   END-IF
           END-EVALUATE
           MOVE    SPACE               TO  SPA-KIDCD
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
           END-IF
      *
           .
       30010-KID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT
      *
      *    未収金
           MOVE    SPA-MISYUMONEY      TO  SPA-GMN-MISYUMONEY
      *    最終来院日
           IF      SPA-LSTYMD      NOT =   SPACE
               MOVE    SPA-LSTYMD      TO  WRK-SYMD
               PERFORM 520-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-LASTYMD
           END-IF
      *    入院の時、表示なしとする
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    SPACE           TO  SPA-GMN-LASTYMD
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
           MOVE    ZERO                TO  SPA-GMN-GOKEI
      *!
      *    保険組合せ検索
           INITIALIZE                  ORCSHKNSETAREA
           MOVE    "1"                 TO  ORCSHKN-KBN
           MOVE    "1"                 TO  ORCSHKN-KBN2
           CALL    "ORCSHKNSET"        USING
                                       SPA-AREA
                                       ORCSHKNSETAREA
      *!
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDG
           MOVE    ZERO                TO  IDX-NAI
           MOVE    ZERO                TO  WRK-BANGO
           MOVE    ZERO                TO  IDX-KOUI
      *
      *    パラメタファイルより、診療会計マスターを編集
      *    パラメタファイルよりワーク編集とＳＯＲＴ
           PERFORM 3101-WKSRYACT-SORT-SEC
      *
      **** MOVE    ZERO                TO  IDX
      *    MOVE    ZERO                TO  IDG
      *    MOVE    ZERO                TO  IDX-NAI
      *    MOVE    ZERO                TO  WRK-BANGO
      *    MOVE    ZERO                TO  IDX-KOUI
      *    MOVE    SPACE               TO  LNK-WKSRYACT-AREA
      *    INITIALIZE                      LNK-WKSRYACT-AREA
      *
      *    PERFORM VARYING     IDX2    FROM    1   BY  1
      *            UNTIL       IDX2    >   WRK-MAX
      *
      *            IF     (IDX-KOUI        >   ZERO  )  AND
      *                   (SRT-WKSRY-ZAINUM (IDX2) NOT =
      *                                    LNK-WKSRY-ZAINUM (IDX-KOUI))
      *                剤毎に処理を行う
      *                PERFORM 3101-WKSRYACT-HEN-SEC
      *                MOVE    SPACE           TO  LNK-WKSRYACT-AREA
      *                INITIALIZE                  LNK-WKSRYACT-AREA
      *                MOVE    ZERO            TO  IDX-KOUI
      *            END-IF
      *            ADD     1               TO  IDX-KOUI
      *            MOVE    SRT-WKSRYACT-REC (IDX2)
      *                                    TO  LNK-WKSRYACT-REC
      *                                                     (IDX-KOUI)
      *    END-PERFORM
      *    剤毎に処理を行う
      *    IF      IDX-KOUI        >   ZERO
      *        PERFORM 3101-WKSRYACT-HEN-SEC
      *****END-IF
      *
      *    最大件数
           IF      IDG                 >   600
               MOVE    600                 TO  SPA-K08-MAX
           ELSE
               MOVE    IDG                 TO  SPA-K08-MAX
           END-IF
      *    各点数
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   LNK-SYUNOU-MAX
               MOVE    LNK-SYUNOU-REC (IDX2)   TO  SYUNOU-REC
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   10
                   ADD     SYU-HKNTEN (IDX)    TO  SPA-GMN-HKNTEN (IDX)
               END-PERFORM
           END-PERFORM
      *    点数再計算
           IF     (SPA-SYORIFLG    NOT =   1   )  AND
                  (SPA-NYUGAIKBN       =   "1" )
               PERFORM 41032-TENSU-KEI-SEC
           END-IF
      *
      *    調剤料のメッセージ
           MOVE    SPACE               TO  SPA-GMN-MSGDATA
           IF      SPA-SYOHO-KBN   NOT =   ZERO
      *        保険空白日本語変換
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI
               MOVE    SPA-SYOHO-HKNMEI    TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT   TO  SPA-SYOHO-HKNMEI
      *
               STRING    SPA-SYOHO-MEI     DELIMITED   BY  SPACE
                         "は他保険で算定済。"
                                           DELIMITED   BY  SIZE
                         "※"              DELIMITED   BY  SIZE
                         SPA-SYOHO-HKNMEI  DELIMITED   BY  SIZE
                                       INTO      SPA-GMN-MSGDATA
               END-STRING
           END-IF
      *
            .
       310-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *     パラメタファイルよりワーク編集とＳＯＲＴ処理
      *****************************************************************
       3101-WKSRYACT-SORT-SEC              SECTION.
      *
      *    パラメタファイルより、診療会計マスターを編集
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDG
      *
           MOVE    ZERO                TO  FLG-PARA
           MOVE    SPACE               TO  SRT-WKSRYACT-AREA
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA
           INITIALIZE                  LNK-WKSRYACT-AREA
           INITIALIZE                  SRT-WKSRYACT-AREA
           MOVE    SPACE               TO  SYUNOU-REC
           MOVE    ZERO                TO  IDX-KOUI
           MOVE    SPACE               TO  LNK-SYUNOU-AREA
           INITIALIZE                  LNK-SYUNOU-AREA
           MOVE    ZERO                TO  LNK-SYUNOU-MAX
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
                   UNTIL      (FLG-PARA    =    1   )  OR
                              (IDX         >=   600 )
      *        ワーク診療行
               IF      PARA-FILEMEI    =   "WKSRYACT"
                   ADD     1                    TO  IDX
                   MOVE    PARA-DATA-REC        TO  LNK-WKSRYACT-REC
                                                                (IDX)
                   IF      LNK-WKSRY-HKNCOMBI(IDX) =   ZERO
                       MOVE    SPA-HKNCOMBINUM TO
                                                LNK-WKSRY-HKNCOMBI(IDX)
                   END-IF
                   IF      LNK-WKSRY-SRYKA   (IDX) =   SPACE
                       MOVE    SPA-SRYKA       TO
                                                LNK-WKSRY-SRYKA   (IDX)
                   END-IF
               END-IF
      *        収納
               IF      PARA-FILEMEI        =   "SYUNOU"
                   ADD     1                   TO  LNK-SYUNOU-MAX
                   IF      LNK-SYUNOU-MAX      <=  15
                       MOVE    PARA-DATA-REC       TO  LNK-SYUNOU-REC
                                                       (LNK-SYUNOU-MAX)
                   END-IF
               END-IF
      *
               PERFORM 980-PARA-READ-SEC
           END-PERFORM
           MOVE    IDX                 TO  WRK-MAX
      *
           CLOSE                       PARA-FILE
      *
           MOVE    ZERO                TO  IDX4
      *    保険毎にＳＯＲＴする
           MOVE    SPACE               TO  HZN-WKSRYACT-AREA
           INITIALIZE                  HZN-WKSRYACT-AREA
           MOVE    LNK-WKSRYACT-AREA   TO  HZN-WKSRYACT-AREA
           IF      SPA-TBL-HKNCOMBI(1) =   SPACE
               MOVE    SPA-HKNCOMBINUM     TO  SPA-TBL-HKNCOMBI(1)
               MOVE    SPA-HKNCOMBIMEI     TO  SPA-TBL-HKNCOMBIMEI(1)
               MOVE    SPA-SRYKA           TO  SPA-TBL-SRYKA (1 1)
               MOVE    SPA-DRCD            TO  SPA-TBL-DRCD  (1 1)
           END-IF
           PERFORM VARYING   IDH1      FROM    1   BY  1
                   UNTIL    (IDH1      >   15  )   OR
                            (SPA-TBL-HKNCOMBI(IDH1)    =   SPACE)
               MOVE    SPA-TBL-HKNCOMBI(IDH1)     TO  WRK-HKNCOMBI
               MOVE    SPA-TBL-HKNCOMBIMEI(IDH1)  TO  WRK-HKNCOMBIMEI
               IF      SPA-TBL-SRYKA (IDH1 1)     =   SPACE
                   MOVE    SPA-SRYKA           TO  SPA-TBL-SRYKA(IDH1 1)
                   MOVE    SPA-DRCD            TO  SPA-TBL-DRCD (IDH1 1)
               END-IF
               PERFORM VARYING   IDH2      FROM   1   BY  1
                       UNTIL    (IDH2      >   10  )  OR
                                (SPA-TBL-SRYKA (IDH1 IDH2) =   SPACE )
                   MOVE    SPA-TBL-SRYKA (IDH1 IDH2)   TO  WRK-SRYKA
                   MOVE    SPA-TBL-DRCD  (IDH1 IDH2)   TO  WRK-DRCD
                   PERFORM 31012-WKSRYACT-SORT2-SEC
                   PERFORM 31013-WKSRYACT-HENSYORI-SEC
               END-PERFORM
           END-PERFORM
      *
            .
       3101-WKSRYACT-SORT-EXT.
           EXIT.
      *****************************************************************
      *     診療行為編集処理
      *****************************************************************
       31012-WKSRYACT-SORT2-SEC              SECTION.
      *
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA
           INITIALIZE                  LNK-WKSRYACT-AREA
           MOVE    ZERO                TO  IDX-MAX
           PERFORM VARYING   IDX1      FROM   1   BY  1
                   UNTIL    (IDX1      >   WRK-MAX    )
               IF     (HZN-WKSRY-HKNCOMBI(IDX1)   =   WRK-HKNCOMBI) AND
                      (HZN-WKSRY-SRYKA   (IDX1)   =   WRK-SRYKA   )
                   ADD     1                  TO  IDX-MAX
                   MOVE    HZN-WKSRYACT-REC(IDX1)
                                              TO  LNK-WKSRYACT-REC
                                                               (IDX-MAX)
                   MOVE    SPACE              TO  HZN-WKSRYACT-REC
                                                               (IDX1)
                   INITIALIZE                 HZN-WKSRYACT-REC(IDX1)
               END-IF
           END-PERFORM
      *
      *    ＳＯＲＴ（診療区分別にＳＯＲＴする）
           MOVE    ZERO                TO  IDX1
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  IDX4
           MOVE    SPACE               TO  SRT-WKSRYACT-AREA
           INITIALIZE                  SRT-WKSRYACT-AREA
      *
           PERFORM VARYING   IDX       FROM   1   BY  1
                   UNTIL     IDX       >   IDX-MAX
               MOVE    ALL "9"             TO  WRK-ZAINUM-X
               MOVE    ALL "9"             TO  WRK-RENNUM-X
               MOVE    ALL "9"             TO  WRK-SRYKBN
               MOVE    ZERO                TO  IDX2
               PERFORM VARYING   IDX1      FROM   1   BY  1
                       UNTIL     IDX1      >   IDX-MAX
                   IF   LNK-WKSRY-INPUTCD(IDX1 1)(1:1)  =   "#"
                                                        OR  "$"
                       MOVE    IDX1                TO  IDX2
                       MOVE    IDX-MAX             TO  IDX1
                   ELSE
                       IF      LNK-WKSRY-SRYKBN (IDX1) NOT =   SPACE
                           MOVE   ZERO                 TO  FLG-CHK
                           IF     LNK-WKSRY-SRYKBN (IDX1)
                                                       <   WRK-SRYKBN
                               MOVE    1                   TO  FLG-CHK
                           END-IF
                           IF      LNK-WKSRY-SRYKBN (IDX1)
                                                   =   WRK-SRYKBN
                               IF      LNK-WKSRY-ZAINUM (IDX1)
                                                   <    WRK-ZAINUM
                                   MOVE    1                TO  FLG-CHK
                               END-IF
                               IF     (LNK-WKSRY-ZAINUM (IDX1)
                                                   =   WRK-ZAINUM) AND
                                      (LNK-WKSRY-RENNUM (IDX1)
                                                   <=   WRK-RENNUM)
                                   MOVE    1                TO  FLG-CHK
                               END-IF
                           END-IF
                           IF      FLG-CHK         =   1
                               MOVE    LNK-WKSRY-SRYKBN (IDX1)
                                                   TO  WRK-SRYKBN
                               MOVE    LNK-WKSRY-ZAINUM (IDX1)
                                                   TO  WRK-ZAINUM
                               MOVE    LNK-WKSRY-RENNUM (IDX1)
                                                   TO  WRK-RENNUM
                               MOVE    IDX1            TO  IDX2
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
               IF     IDX2                 >   ZERO
                   ADD     1              TO  IDX4
                   MOVE    LNK-WKSRYACT-REC(IDX2)
                                          TO  SRT-WKSRYACT-REC (IDX4)
                   MOVE    SPACE          TO  LNK-WKSRYACT-REC(IDX2)
                   INITIALIZE                 LNK-WKSRYACT-REC(IDX2)
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA
           INITIALIZE                  LNK-WKSRYACT-AREA
      *
            .
       31012-WKSRYACT-SORT2-EXT.
           EXIT.
      *****************************************************************
      *     診療行為内容編集処理
      *****************************************************************
       31013-WKSRYACT-HENSYORI-SEC              SECTION.
      *
           MOVE    ZERO                TO  IDX-KOUI
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA
           INITIALIZE                      LNK-WKSRYACT-AREA
      *!
      *!   IF     (IDH1                >   1  )  AND
           IF     (IDH2                =   1  )
             AND  (SPA-TBL-HKNCOMBI(2) NOT =   SPACE )
               PERFORM 31012-HKNCOMBI-HEN-SEC
           END-IF
      *!   IF     (IDH1                =   1  )  AND
      *           (IDH2                =   1  )
      *!       CONTINUE
      *!   ELSE
           IF     (SPA-TBL-HKNCOMBI(2) NOT =   SPACE )  OR
                  (SPA-TBL-SRYKA   (IDH1 2)    NOT =   SPACE )
               PERFORM 31013-SRYKA-HEN-SEC
           END-IF
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   IDX-MAX
      *
               IF     (IDX-KOUI        >   ZERO  )  AND
                      (SRT-WKSRY-ZAINUM (IDX2) NOT =
                                           LNK-WKSRY-ZAINUM (IDX-KOUI))
      *            剤毎に処理を行う
                   PERFORM 3101-WKSRYACT-HEN-SEC
                   MOVE    SPACE           TO  LNK-WKSRYACT-AREA
                   INITIALIZE                  LNK-WKSRYACT-AREA
                   MOVE    ZERO            TO  IDX-KOUI
               END-IF
               ADD     1               TO  IDX-KOUI
               MOVE    SRT-WKSRYACT-REC (IDX2)
                                           TO  LNK-WKSRYACT-REC
                                                            (IDX-KOUI)
           END-PERFORM
      *    剤毎に処理を行う
           IF      IDX-KOUI        >   ZERO
               PERFORM 3101-WKSRYACT-HEN-SEC
           END-IF
      *
            .
       31013-WKSRYACT-HENSYORI-EXT.
           EXIT.
      *****************************************************************
      *     診療行為内容編集処理
      *****************************************************************
       3101-WKSRYACT-HEN-SEC              SECTION.
      *
      *!
           IF      LNK-WKSRY-INPUTCD(1 1)(1:1)     =   "#"
               GO      TO      3101-WKSRYACT-HEN-EXT
           END-IF
           IF      LNK-WKSRY-INPUTCD(1 1)(1:1)     =   "$"
               GO      TO      3101-WKSRYACT-HEN-EXT
           END-IF
      *!
      *    診療コード計計算
           MOVE    ZERO                TO  IDZ
           MOVE    ZERO                TO  FLG-YAKUSOKU
           MOVE    ZERO                TO  WRK-MEISAISU
           MOVE    ZERO                TO  WRK-ZAITEN
           MOVE    ZERO                TO  WRK-SRYKAISUKEI
           MOVE    ZERO                TO  WRK-JIHIMONEY
           MOVE    ZERO                TO  WRK-JIHIMONEY-KEI
           MOVE    ZERO                TO  FLG-KAISUU-ARI
           MOVE    ZERO                TO  FLG-KAISUU-ARI2
           MOVE    ZERO                TO  FLG-KAISUU-ARI3
      *
      *    入院で日付指定の有無
           PERFORM VARYING     IDZ     FROM    1   BY  1
                   UNTIL      (IDZ         >   5   )  OR
                             ((LNK-WKSRY-SRYCD (IDX-KOUI IDZ)
                                                  =   SPACE ) AND
                              (LNK-WKSRY-INPUTCD(IDX-KOUI IDZ)
                                                  =   SPACE ))
      *        回数入力の有無
               IF      LNK-WKSRY-INPUTCD(IDX-KOUI IDZ)(1:1) =   "*"
                   MOVE    1                   TO  FLG-KAISUU-ARI2
                   PERFORM VARYING     IDX3    FROM   2   BY  1
                           UNTIL       IDX3    >   22
                       IF       LNK-WKSRY-INPUTCD(IDX-KOUI IDZ)
                                                   (IDX3:1) =   "/"
                           MOVE    1               TO  FLG-KAISUU-ARI3
                           MOVE    22              TO  IDX3
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *    剤別編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   IDX-KOUI )
                           OR (IDG     >   600 )
      *        剤点数計
               MOVE    LNK-WKSRY-ZAITENKEI(IDX)    TO  WRK-ZAITEN
      *        回数計
               MOVE    LNK-WKSRY-ZAIKAIKEI(IDX)    TO  WRK-SRYKAISUKEI
      *        自費
      *        IF      LNK-WKSRY-JIHIMONEYTOTAL (IDX)
      ***                                      NOT =   ZERO
                   MOVE    LNK-WKSRY-JIHIMONEYTOTAL (IDX)
                                                  TO  WRK-JIHIMONEY
                   ADD     LNK-WKSRY-JIHIMONEYTOTAL (IDX)
                                                  TO  WRK-JIHIMONEY-KEI
      ***      END-IF
      *
      *        剤内１行目
               IF      IDX                 =   1
                   MOVE    ZERO                TO  FLG-RSIFLG
                   ADD     1                   TO  IDG
                   IF      IDG                 >   600
                       MOVE    "1000"              TO  SPA-ERRCD
                   ELSE
                       PERFORM 31011-GYO1-HEN-SEC
                   END-IF
               END-IF
      *
      *        剤明細編集
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL      (IDZ         >   5   )  OR
                                  (IDG         >   600 )  OR
                                  (LNK-WKSRY-SRYCD (IDX IDZ)
                                                  =   SPACE  AND
                                   LNK-WKSRY-INPUTCD(IDX IDZ)
                                                  =   SPACE )
      *          回数入力の有無
                 PERFORM 310100-KAISUU-CHK-SEC
                 IF      LNK-WKSRY-INPUTCD(IDX IDZ)(1:1) =   "*"
                     CONTINUE
                 ELSE
      *
                   ADD     1                   TO  WRK-MEISAISU
      *            自費の時、金額を表示
                   IF      LNK-WKSRY-SRYKBN (IDX)  =   "95" OR "96"
                       PERFORM 31013-JIHIMEI-HEN-SEC
                   ELSE
                       PERFORM 31012-ZAIMEI-HEN-SEC
                   END-IF
      *            約束セットの時
                   IF      LNK-WKSRY-SRYCD (IDX IDZ)(1:1)   =   "S"
                       MOVE    1                  TO  FLG-YAKUSOKU
                   END-IF
      *            削除できるかの判定
                   IF      LNK-WKSRY-AUTOKBN (IDX IDZ)   =   "3"
                       MOVE    1                   TO  SPA-NAI-JIDFLG
                                                              (IDX-NAI)
                   END-IF
      *
                 END-IF
               END-PERFORM
           END-PERFORM
      *
      *    合計点数集計
           IF      WRK-SRYKAISUKEI     =   ZERO
               MOVE   1                    TO  WRK-SRYKAISUKEI
           END-IF
           COMPUTE WRK-KEI-G           =   WRK-ZAITEN
                                       *   WRK-SRYKAISUKEI
           MOVE    WRK-KEI-G           TO  SPA-NAI-HKNTEN  (IDX-NAI)
      *    労災金額の時、ゼロ（指導料、その他のみ）
           IF      LNK-WKSRY-SRYKBN (01)   =   "13"  OR  "80"
               IF      FLG-RSIFLG          =   1
                   MOVE    ZERO            TO  SPA-NAI-HKNTEN
                                                             (IDX-NAI)
               END-IF
           END-IF
      *
      *    剤点数がゼロの時、自費を点数へ
      *    IF      WRK-ZAITEN          =   ZERO
      *        MOVE   WRK-JIHIMONEY        TO  WRK-ZAITEN
      *    END-IF
      *
      *    診療コードがない時、処理をしない
           IF      WRK-MEISAISU        =   ZERO
               COMPUTE IDZ             =   IDZ     -   1
               GO      TO      3101-WKSRYACT-HEN-EXT
           END-IF
      *
      *    自費の時、計を表示しない
           IF     (WRK-JIHIMONEY       >   ZERO  )  OR
      *        回数入力がなく、入院回数入力がある時、入院回数のみ
                 ((FLG-KAISUU-ARI      =   ZERO )  AND
                  (FLG-KAISUU-ARI2     =   1    ))
               MOVE    ZERO                TO  SPA-NAI-HKNTEN  (IDX-NAI)
               CONTINUE
           ELSE
      *        剤　点数＊日数　計　編集 （最終行に追加）
               IF      SPA-GMN-HEN1 (IDG)      =   SPACE  OR  ALL "　"
                   CONTINUE
               ELSE
                   ADD    1                    TO  IDG
               END-IF
               IF      IDG                 <=  600
               PERFORM 31013-INPUTCD-HEN-SEC
               MOVE    WRK-ZAIKEIJ         TO  SPA-GMN-TZAIKEI (IDG)
      *
               INSPECT SPA-GMN-TBANGO2 (IDG)   REPLACING
                                               ALL "  "  BY  "　"
               INSPECT SPA-GMN-TMEISYO2(IDG)   REPLACING
                                               ALL "  "  BY  "　"
               INSPECT SPA-GMN-TYOBI   (IDG)   REPLACING
                                               ALL "  "  BY  "　"
               INSPECT SPA-GMN-TZAIKEI (IDG)   REPLACING
                                               ALL "  "  BY  "　"
               INSPECT SPA-GMN-TYOBI2  (IDG)   REPLACING
                                               ALL "  "  BY  "　"
      *
               END-IF
      *
           END-IF
           MOVE    WRK-BANGO           TO  SPA-GMN-NAIBANGO (IDG)
      *
      *    入院で訂正以外の時、日付により回数を変更する
           IF      FLG-KAISUU-ARI2     =   1
               PERFORM 31019-NYUIN-SYORI-SEC
           END-IF
      *
      *    剤終了編集
           ADD     1                   TO  IDG
           IF      IDG                 >   600
               GO      TO      3101-WKSRYACT-HEN-EXT
           END-IF
           MOVE    ALL "-"             TO  SPA-GMN-TMEISYO (IDG)
      *
            .
       3101-WKSRYACT-HEN-EXT.
           EXIT.
      *!
      *****************************************************************
      *   保険組合せ編集処理
      *****************************************************************
       31012-HKNCOMBI-HEN-SEC          SECTION.
      *
      *    剤編集
           ADD     1                   TO  IDG
           IF      IDG                 >   600
               GO      TO      31012-HKNCOMBI-HEN-EXT
           END-IF
           MOVE    ALL "#"             TO  SPA-GMN-TMEISYO (IDG)
           STRING  "《"                DELIMITED  BY  SIZE
                   WRK-HKNCOMBIMEI     DELIMITED  BY  "  "
                   "》"                DELIMITED  BY  SIZE
                  INTO  SPA-GMN-TMEISYO (IDG)(3:)
           END-STRING
      *    剤終了編集
           ADD     1                   TO  IDG
           IF      IDG                 >   600
               GO      TO      31012-HKNCOMBI-HEN-EXT
           END-IF
           MOVE    ALL "-"             TO  SPA-GMN-TMEISYO (IDG)
      *
            .
       31012-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       31013-SRYKA-HEN-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYKAMEI
           PERFORM VARYING     IDK     FROM   1   BY  1
                   UNTIL       IDK     >   SPA-SRYKA-MAX
               IF     SPA-GMN-SRYKAL (IDK)
                                       =   WRK-SRYKA
                   MOVE    SPA-GMN-SRYKAMEIL (IDK)
                                               TO  WRK-SRYKAMEI
                   MOVE    SPA-SRYKA-MAX       TO  IDH
               END-IF
           END-PERFORM
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    WRK-DRCD            TO  SYS-1010-KBNCD
      *    システム管理検索
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           MOVE    SPA-SRYYMD          TO  ORC-DBYMD
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               PERFORM 960-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               MOVE    SYS-1010-NAME       TO  WRK-DRCD-MEI
           ELSE
               MOVE    SPACE               TO  WRK-DRCD-MEI
           END-IF
      *
      *    剤編集
           ADD     1                   TO  IDG
           IF      IDG                 >   600
               GO      TO      31013-SRYKA-HEN-EXT
           END-IF
           MOVE    ALL "="             TO  SPA-GMN-TMEISYO (IDG)
           STRING  "＜＜"              DELIMITED  BY  SIZE
                   WRK-SRYKAMEI        DELIMITED  BY  SPACE
                   "　ドクター："      DELIMITED  BY  SIZE
                   WRK-DRCD-MEI        DELIMITED  BY  SPACE
                    "＞＞"             DELIMITED  BY  SIZE
               INTO                    SPA-GMN-TMEISYO (IDG)(3:)
           END-STRING
      *    剤終了編集
           ADD     1                   TO  IDG
           IF      IDG                 >   600
               GO      TO      31013-SRYKA-HEN-EXT
           END-IF
           MOVE    ALL "-"             TO  SPA-GMN-TMEISYO (IDG)
      *
            .
       31013-SRYKA-HEN-EXT.
           EXIT.
      *!
      *****************************************************************
      *   入院時回数編集処理
      *****************************************************************
       310100-KAISUU-CHK-SEC                  SECTION.
      *
           PERFORM VARYING     IDX3    FROM   1   BY  1
                   UNTIL       IDX3    >   22
               IF       LNK-WKSRY-INPUTCD(IDX IDZ)(IDX3:1) =   "*" 
                   IF       IDX3                >   1
                       MOVE    1               TO  FLG-KAISUU-ARI
                   ELSE
                       MOVE    1               TO  FLG-KAISUU-ARI2
                   END-IF
               END-IF
           END-PERFORM
            .
       310100-KAISUU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   入院時回数編集処理
      *****************************************************************
       31019-NYUIN-SYORI-SEC                  SECTION.
      *
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-KOUI
      *        剤明細編集
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL       IDZ         >   5
                   IF      LNK-WKSRY-INPUTCD (IDX IDZ)(1:1) =   "*"
                       PERFORM 310191-DAYSET-SEC
                   END-IF
               END-PERFORM
           END-PERFORM
      *
            .
       31019-NYUIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   入院編集処理
      *****************************************************************
       310191-DAYSET-SEC                  SECTION.
      *
           INITIALIZE                  ORCSC97AREA
           MOVE    SPACE               TO  SPA-SCR-REC
      *    ＰＧ名
           MOVE    "K02N"              TO  LNK-SC97-PG
           MOVE    LNK-WKSRY-INPUTCD (IDX IDZ)
                                       TO  LNK-SC97-INPUTCD
           CALL    "ORCSC97"           USING
                                       SPA-AREA
                                       ORCSC97AREA
                                       SPA-SCR-REC
      *    回数
           IF      LNK-SC97-KAISU      =   ZERO
               MOVE    1               TO  LNK-SC97-KAISU
           END-IF
      *    外用薬の時、回数は１回とする
           IF      LNK-WKSRY-SRYKBN (IDX)  =   "23"
               MOVE    1               TO  LNK-SC97-KAISU
           END-IF
      *
           MOVE    ZERO                TO  IDX1
           MOVE    ZERO                TO  WRK-NISUU
           PERFORM VARYING     IDX4    FROM    1   BY  1
                   UNTIL       IDX4        >   31
               IF      LNK-SC97-NYU-DAY (IDX4) NOT =   ZERO
                   ADD     1               TO  WRK-NISUU
               END-IF
           END-PERFORM
      *    ゼロの時、
           IF      WRK-NISUU           =   ZERO
               MOVE    1                   TO  WRK-NISUU
           END-IF
      *
      *    計を表示
           IF      WRK-JIHIMONEY       >   ZERO
      *        自費
               IF      FLG-KAISUU-ARI3     =   1
                   ADD    1                    TO  IDG
                   PERFORM 3101913-NYUINPUTCD-JIHI-SEC
                   MOVE    WRK-BANGO       TO  SPA-GMN-NAIBANGO (IDG)
               END-IF
           ELSE
               ADD    1                    TO  IDG
               PERFORM 3101912-NYUINPUTCD-HEN-SEC
               MOVE    WRK-ZAIKEIJ         TO  SPA-GMN-HEN2 (IDG)
      *
               INSPECT SPA-GMN-TBANGO2 (IDG)   REPLACING
                                               ALL "  "  BY  "　"
               INSPECT SPA-GMN-TMEISYO2(IDG)   REPLACING
                                               ALL "  "  BY  "　"
               INSPECT SPA-GMN-TYOBI   (IDG)   REPLACING
                                               ALL "  "  BY  "　"
               INSPECT SPA-GMN-TZAIKEI (IDG)   REPLACING
                                               ALL "  "  BY  "　"
               INSPECT SPA-GMN-TYOBI2  (IDG)   REPLACING
                                               ALL "  "  BY  "　"
      *
               MOVE    WRK-BANGO           TO  SPA-GMN-NAIBANGO (IDG)
           END-IF
      *
            .
       310191-DAYSET-EXT.
           EXIT.
      *
      *****************************************************************
      *   入院回数編集処理
      *****************************************************************
       3101912-NYUINPUTCD-HEN-SEC      SECTION.
      *
      *    点数＊回数　計
      *    点数
           MOVE    WRK-ZAITEN          TO  WRK-TENSU
      *
           COMPUTE WRK-KAISU-9         =   WRK-NISUU
                                       *   LNK-SC97-KAISU
           COMPUTE WRK-KEI-G           =   WRK-ZAITEN
                                       *   WRK-KAISU-9
           ADD     WRK-KEI-G           TO  SPA-NAI-HKNTEN  (IDX-NAI)
      *    労災金額の時、ゼロ（指導料、その他のみ）
           IF      LNK-WKSRY-SRYKBN (01)   =   "13"  OR  "80"
               IF      FLG-RSIFLG          =   1
                   MOVE    ZERO            TO  SPA-NAI-HKNTEN
                                                             (IDX-NAI)
               END-IF
           END-IF
      *    計集計
           MOVE    WRK-KEI-G           TO  WRK-KEI
           ADD     WRK-KEI-G           TO  SPA-GMN-GOKEI
           MOVE    SPACE               TO  WRK-ZAIKEI 
           MOVE    SPACE               TO  WRK-KAISU-H 
           MOVE    SPACE               TO  WRK-KAISU-H2
           MOVE    SPACE               TO  WRK-KEI-H 
      *    回数
           MOVE    LNK-SC97-KAISU      TO  WRK-KAISU
           MOVE    ZERO                TO  IDK2
           PERFORM VARYING     IDK1    FROM    1   BY  1
                   UNTIL       IDK1    >   3
               IF      WRK-KAISU-X (IDK1:1)    NOT =   SPACE
                   ADD     1               TO  IDK2
                   MOVE    WRK-KAISU-X (IDK1:1)
                                           TO  WRK-KAISU-H (IDK2:1)
               END-IF
           END-PERFORM
      *    回数
           MOVE    WRK-NISUU           TO  WRK-KAISU
           MOVE    ZERO                TO  IDK2
           PERFORM VARYING     IDK1    FROM    1   BY  1
                   UNTIL       IDK1    >   3
               IF      WRK-KAISU-X (IDK1:1)    NOT =   SPACE
                   ADD     1           TO  IDK2
                   MOVE    WRK-KAISU-X (IDK1:1)
                                       TO  WRK-KAISU-H2 (IDK2:1)
               END-IF
           END-PERFORM
      *    計
           MOVE    ZERO                TO  IDK2
           PERFORM VARYING     IDK1    FROM    1   BY  1
                   UNTIL       IDK1    >   8
               IF      WRK-KEI-X   (IDK1:1)    NOT =   SPACE
                   ADD     1           TO  IDK2
                   MOVE    WRK-KEI-X   (IDK1:1)
                                       TO  WRK-KEI-H   (IDK2:1)
               END-IF
           END-PERFORM
      *
           STRING  WRK-TENSU               DELIMITED   BY  SIZE
                   "X"                     DELIMITED   BY  SIZE
                   WRK-KAISU-H             DELIMITED   BY  SPACE
                   "X"                     DELIMITED   BY  SIZE
                   WRK-KAISU-H2            DELIMITED   BY  SPACE
                   "  "                    DELIMITED   BY  SIZE
                   WRK-KEI-H               DELIMITED   BY  SPACE
                   INTO                    WRK-ZAIKEI
           END-STRING
      *
      *    日本語変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-ZAIKEI          TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  WRK-ZAIKEIJ
           INSPECT WRK-ZAIKEIJ         REPLACING
                                       ALL "Ｘ"  BY  "×"
      *
      *
            .
       3101912-NYUINPUTCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   入院回数編集処理
      *****************************************************************
       3101913-NYUINPUTCD-JIHI-SEC     SECTION.
      *
           COMPUTE WRK-KAISU-9         =   WRK-NISUU
                                       *   LNK-SC97-KAISU
           COMPUTE WRK-KEI-G           =   WRK-JIHIMONEY-KEI
                                       *   WRK-KAISU-9
           MOVE    WRK-KEI-G           TO  WRK-KEI
           MOVE    SPACE               TO  WRK-ZAIKEIJIJ
      *    計集計
           MOVE    SPACE               TO  WRK-ZAIKEI 
           MOVE    SPACE               TO  WRK-KAISU-H 
           MOVE    SPACE               TO  WRK-KAISU-H2
           MOVE    SPACE               TO  WRK-KEI-H 
      *    回数
           MOVE    LNK-SC97-KAISU      TO  WRK-KAISU
           MOVE    ZERO                TO  IDK2
           PERFORM VARYING     IDK1    FROM    1   BY  1
                   UNTIL       IDK1    >   3
               IF      WRK-KAISU-X (IDK1:1)    NOT =   SPACE
                   ADD     1           TO  IDK2
                   MOVE    WRK-KAISU-X (IDK1:1)
                                       TO  WRK-KAISU-H (IDK2:1)
               END-IF
           END-PERFORM
      *    回数
           MOVE    WRK-NISUU           TO  WRK-KAISU
           MOVE    ZERO                TO  IDK2
           PERFORM VARYING     IDK1    FROM    1   BY  1
                   UNTIL       IDK1    >   3
               IF      WRK-KAISU-X (IDK1:1)    NOT =   SPACE
                   ADD     1           TO  IDK2
                   MOVE    WRK-KAISU-X (IDK1:1)
                                       TO  WRK-KAISU-H2 (IDK2:1)
               END-IF
           END-PERFORM
      *
           STRING  WRK-KAISU-H             DELIMITED   BY  SPACE
                   "X"                     DELIMITED   BY  SIZE
                   WRK-KAISU-H2            DELIMITED   BY  SPACE
                   INTO                    WRK-ZAIKEI
           END-STRING
      *
      *    日本語変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-ZAIKEI          TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  WRK-ZAIKEIJ
           INSPECT WRK-ZAIKEIJ         REPLACING
                                       ALL "Ｘ"  BY  "×"
           MOVE    WRK-ZAIKEIJ         TO  SPA-GMN-TZAIKEI4(IDG)
      *
      *    日本語変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-KEI-X           TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  WRK-ZAIKEIJIJ
      *
           MOVE    "円"                TO  WRK-ZAIKEIJIJ(17:2)
           MOVE    WRK-ZAIKEIJIJ       TO  SPA-GMN-TKINGAK4(IDG)
      *
           INSPECT SPA-GMN-TBANGO2 (IDG)  REPLACING
                                              ALL "  "  BY  "　"
           INSPECT SPA-GMN-TMEISYO2(IDG)  REPLACING
                                              ALL "  "  BY  "　"
           INSPECT SPA-GMN-TYOBI41(IDG)  REPLACING
                                              ALL "  "  BY  "　"
           INSPECT SPA-GMN-TZAIKEI4 (IDG)  REPLACING
                                              ALL "  "  BY  "　"
           INSPECT SPA-GMN-TKINGAK4 (IDG)  REPLACING
                                              ALL "  "  BY  "　"
      *
            .
       3101913-NYUINPUTCD-JIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤内１行目  編集処理
      *****************************************************************
       31011-GYO1-HEN-SEC                  SECTION.
      *
      *    番号セット
           ADD     1                   TO  WRK-BANGO
           MOVE    WRK-BANGO           TO  SPA-GMN-TBANGO (IDG)
           MOVE    WRK-BANGO           TO  SPA-GMN-NAIBANGO (IDG)
      *    内部セット
           ADD     1                   TO  IDX-NAI
           MOVE    WRK-BANGO           TO  SPA-NAI-BANGO  (IDX-NAI)
      *    パラメタの連番
           MOVE    IDX2                TO  SPA-NAI-EDANUM  (IDX-NAI)
           MOVE    LNK-WKSRY-ZAINUM (IDX)
                                       TO  SPA-NAI-ZAINUM  (IDX-NAI)
           MOVE    LNK-WKSRY-SRYKBN (IDX)
                                       TO  SPA-NAI-SRYKBN  (IDX-NAI)
           MOVE    LNK-WKSRY-HKNCOMBI(IDX)
                                       TO  SPA-NAI-HKNCOMBI(IDX-NAI)
           MOVE    LNK-WKSRY-SRYKA   (IDX)
                                       TO  SPA-NAI-SRYKA   (IDX-NAI)
      *****合計点数集計
      *    IF      WRK-SRYKAISUKEI     =   ZERO
      *        MOVE   1                    TO  WRK-SRYKAISUKEI
      *    END-IF
      *    COMPUTE WRK-KEI-G           =   WRK-ZAITEN
      *                                *   WRK-SRYKAISUKEI
      *****MOVE    WRK-KEI-G           TO  SPA-NAI-HKNTEN  (IDX-NAI)
           IF      LNK-WKSRY-SRYCD (01 01)     =   "630010002"
      *        薬剤料逓減　を減算
               MOVE    1                   TO  SPA-NAI-HKNSFLG(IDX-NAI)
           ELSE
               MOVE    ZERO                TO  SPA-NAI-HKNSFLG(IDX-NAI)
           END-IF
      *
           MOVE    IDX-NAI             TO  SPA-GMN-NAIIDX  (IDG)
      *
      *
      *     診療種別区分名称、診療行為区分セット
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE       TO  WRK-SRYSYUMEI
                   WHEN    LNK-WKSRY-SRYSYUKBN (IDX) =   MSYU-SRYSYUKBN
                                                      (MSYU-IDX)
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                                TO  WRK-SRYSYUMEI
           END-SEARCH
      *
      *    診療種別がない時、診療行為区分より
           IF      LNK-WKSRY-SRYSYUKBN (IDX)   =   SPACE
               SET     MSYU-IDX            TO  1
               SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYSYUMEI
                   WHEN    LNK-WKSRY-SRYKBN(IDX) =   MSYU-SRYKBN
                                                         (MSYU-IDX)
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                               TO  WRK-SRYSYUMEI
               END-SEARCH
           END-IF
      *
           IF      LNK-WKSRY-SRYSYUKBN (IDX)   =   SPACE
               MOVE    SPACE           TO  SPA-GMN-TMEISYO1(IDG)
               MOVE    WRK-SRYSYUMEI   TO  SPA-GMN-TMEISYO1(IDG)(7:)
           ELSE
               MOVE    "."             TO  SPA-GMN-TMEISYO1(IDG)(1:1)
               MOVE    LNK-WKSRY-SRYSYUKBN (IDX)
                                       TO  SPA-GMN-TMEISYO1(IDG)(2:3)
               MOVE    WRK-SRYSYUMEI   TO  SPA-GMN-TMEISYO1(IDG)(7:)
           END-IF
      *
           INSPECT SPA-GMN-TMEISYO1 (IDG)  REPLACING  ALL "  "  BY "　"
      *
           .
       31011-GYO1-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤内明細  編集処理
      *****************************************************************
       31012-ZAIMEI-HEN-SEC                  SECTION.
      *
           ADD     1                   TO  IDG
           IF      IDG                 >   600
               GO      TO      31012-ZAIMEI-HEN-EXT
           END-IF
           MOVE    WRK-BANGO           TO  SPA-GMN-NAIBANGO (IDG)
      *
      *    約束セット編集
           IF      LNK-WKSRY-SRYCD (IDX IDZ)(1:1)   =   "S"
               PERFORM 310120-YAKUSOKU-SET-SEC
               GO      TO      31012-ZAIMEI-HEN-EXT
           END-IF
      *
      *    診療コードより診療行為名称を取得
           MOVE    LNK-WKSRY-SRYCD (IDX IDZ)   TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU       NOT =   ZERO
               MOVE    LNK-WKSRY-SRYCD (IDX IDZ)   TO  TNS-NAME
           END-IF
      *    労災の金額算定分の時、点数をゼロとする
           IF     (LNK-WKSRY-SRYCD (IDX IDZ)(1:3)  =   "101" ) AND
                  (TNS-TENSIKIBETU     =   1    )
               MOVE    1                   TO  FLG-RSIFLG
           END-IF
      *    自賠責で証明料等の時、点数をゼロとする
           IF     (SPA-HKNKBN          =   1    )  AND
      *********** (SPA-RSI-HKNKBN      =   "4"  )  AND
                  (LNK-WKSRY-SRYKBN(IDX)   =   "80" )  AND
                  (LNK-WKSRY-SRYCD (IDX IDZ)(1:5)  =   "09591" OR
                                                       "09592" OR
                                                       "09593" OR
                                                       "09594" )
               MOVE    1                   TO  FLG-RSIFLG
           END-IF
      *
      *    行為名称
           MOVE    TNS-NAME            TO  SPA-GMN-TMEISYO2(IDG)
      *    薬剤服用コメントの時、再編集
           IF      LNK-WKSRY-SRYCD (IDX IDZ)(1:3)  =   "001"
                PERFORM 3100121-FUKIYO-MEISYO-SEC
                MOVE    WRK-MEISYO    TO  SPA-GMN-TMEISYO2(IDG)
           END-IF
      *
      *    約束の内容の時、再編集
           IF      FLG-YAKUSOKU        =   1
               MOVE    SPA-GMN-TMEISYO2(IDG)   TO  WRK-MEISYO
               MOVE    SPACE                   TO  SPA-GMN-TMEISYO2
                                                               (IDG)
               STRING     "＞"             DELIMITED   BY  SIZE
                          WRK-MEISYO       DELIMITED   BY  SIZE
                          INTO             SPA-GMN-TMEISYO2(IDG)
               END-STRING
           END-IF
      *
      *    コメントの時、内容設定
           IF    ((LNK-WKSRY-SRYCD   (IDX IDZ)(1:1)  =   "8"   ) OR
                  (LNK-WKSRY-SRYCD   (IDX IDZ)(1:3)  =   "008" )) AND
      *DDDDDDDDDD (LNK-WKSRY-INPUTNUM(IDX IDZ)    NOT =   ZERO OR
                  (LNK-WKSRY-INPUTCOMENT(IDX IDZ) NOT =  SPACE )
               MOVE    LNK-WKSRY-INPUTCOMENT(IDX IDZ)
                                           TO  SPA-GMN-TMEISYO2(IDG)
           END-IF
      *
      *    数量（薬剤、器材のとき）
           IF     (LNK-WKSRY-SRYCD (IDX IDZ)(1:1)   =   "6"  OR  "7" )
             OR   (LNK-WKSRY-SRYCD (IDX IDZ)(1:3)   =   "059"    )
               MOVE    LNK-WKSRY-SRYSURYO (IDX IDZ)  TO  WRK-SURYO
               PERFORM 310121-SURYO-HEN-SEC
               MOVE    WRK-SURYO-J         TO  SPA-GMN-TSURYOU (IDG)
               MOVE    TNS-TANINAME        TO  SPA-GMN-TTANI1  (IDG)
           END-IF
      *    分画数
           IF     (LNK-WKSRY-SRYCD (IDX IDZ)(1:1)   =   "7"  )  AND
                  (TNS-DATAKBN             =   3    )  AND
                  (LNK-WKSRY-SRYKAISU (IDX IDZ)      >   ZERO )
               MOVE    LNK-WKSRY-SRYKAISU (IDX IDZ)
                                           TO  WRK-BUNGSU-Z
               PERFORM 310122-BUNGSU-HEN-SEC
               MOVE    WRK-BUNGSU-J        TO  SPA-GMN-TTANMEI (IDG)
               MOVE    "分画"              TO  SPA-GMN-TTANI2  (IDG)
           END-IF
           INSPECT SPA-GMN-TBANGO2 (IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TMEISYO2(IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TSURYOU (IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TTANMEI (IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TTANI1  (IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TTANI2  (IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TYOBI1  (IDG) REPLACING  ALL "  "  BY  "　"
      *
      *    コメント以外の最終行セット
           IF      LNK-WKSRY-SRYCD (IDX IDZ)(1:1)  =   "0"  OR  "8"
               CONTINUE
           ELSE
               MOVE    IDG             TO  WRK-IDG
           END-IF
      *
           .
       31012-ZAIMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤内明細（自費）  編集処理
      *****************************************************************
       31013-JIHIMEI-HEN-SEC           SECTION.
      *
           ADD     1                   TO  IDG
           IF      IDG                 >   600
               GO      TO      31013-JIHIMEI-HEN-EXT
           END-IF
           MOVE    WRK-BANGO           TO  SPA-GMN-NAIBANGO (IDG)
      *
      *    約束セット編集
           IF      LNK-WKSRY-SRYCD (IDX IDZ)(1:1)   =   "S"
               PERFORM 310120-YAKUSOKU-SET-SEC
               GO      TO      31013-JIHIMEI-HEN-EXT
           END-IF
      *
      *    診療コードより診療行為名称を取得
           MOVE    LNK-WKSRY-SRYCD (IDX IDZ)   TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU       NOT =   ZERO
               MOVE    LNK-WKSRY-SRYCD (IDX IDZ)   TO  TNS-NAME
           END-IF
      *    行為名称
           MOVE    TNS-NAME            TO  SPA-GMN-TMEISYO2(IDG)
      *    薬剤服用コメントの時、再編集
           IF      LNK-WKSRY-SRYCD (IDX IDZ)(1:3)  =   "001"
                PERFORM 3100121-FUKIYO-MEISYO-SEC
                MOVE    WRK-MEISYO    TO  SPA-GMN-TMEISYO2(IDG)
           END-IF
      *    コメントの時、内容設定
           IF    ((LNK-WKSRY-SRYCD   (IDX IDZ)(1:1)  =   "8"   ) OR
                  (LNK-WKSRY-SRYCD   (IDX IDZ)(1:3)  =   "008" ))  AND
                  (LNK-WKSRY-INPUTCOMENT(IDX IDZ) NOT =  SPACE )
               MOVE    LNK-WKSRY-INPUTCOMENT(IDX IDZ)
                                           TO  SPA-GMN-TMEISYO2(IDG)
           END-IF
      *
           MOVE    ZERO                TO  FLG-KAISUU
      *    数量（薬剤、器材のとき）
           IF     (LNK-WKSRY-SRYCD (IDX IDZ)(1:1)   =   "6"  OR  "7" )
             OR   (LNK-WKSRY-SRYCD (IDX IDZ)(1:3)   =   "059"    )
               MOVE    LNK-WKSRY-SRYSURYO (IDX IDZ)  TO  WRK-SURYO
               PERFORM 310121-SURYO-HEN-SEC
               MOVE    WRK-SURYO-J         TO  SPA-GMN-TSURYOU3 (IDG)
               MOVE    TNS-TANINAME        TO  SPA-GMN-TTANI13  (IDG)
           END-IF
      *
      *    計
           MOVE    LNK-WKSRY-JIHIMONEY(IDX IDZ)
                                       TO  WRK-KEI
           MOVE    SPACE               TO  WRK-ZAIKEIJIH
           MOVE    WRK-KEI             TO  WRK-ZAIKEIJIH
      *
      *    日本語変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-ZAIKEIJIH       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  WRK-ZAIKEIJIJ
      *
           IF      LNK-WKSRY-JIHIMONEY(IDX IDZ)    =   ZERO
               MOVE    SPACE               TO  SPA-GMN-TZAIKEI3(IDG)
           ELSE
               MOVE    "円"                TO  WRK-ZAIKEIJIJ(17:2)
               MOVE    WRK-ZAIKEIJIJ       TO  SPA-GMN-TZAIKEI3(IDG)
           END-IF
      *
           INSPECT SPA-GMN-TBANGO2 (IDG)  REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TMEISYO2(IDG)  REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TSURYOU3(IDG)  REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TTANI13 (IDG)  REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TZAIKEI3(IDG)  REPLACING  ALL "  "  BY  "　"
      *
      *    回数
           MOVE    SPACE               TO  WRK-KAISU-H
           IF     (WRK-SRYKAISUKEI     >   1    )
             AND ((FLG-KAISUU-ARI2     =   ZERO )  OR
                  (FLG-KAISUU-ARI3     =   ZERO ))
               ADD     1                   TO  IDG
               IF      IDG                 >   600
                   GO      TO      31013-JIHIMEI-HEN-EXT
               END-IF
      *        回数
               MOVE    WRK-SRYKAISUKEI     TO  WRK-KAISU
               MOVE    ZERO                TO  IDK2
               MOVE    SPACE               TO  WRK-KAISU-H
               PERFORM VARYING     IDK1    FROM    1   BY  1
                       UNTIL       IDK1    >   3
                   IF      WRK-KAISU-X (IDK1:1)    NOT =   SPACE
                       ADD     1           TO  IDK2
                       MOVE    WRK-KAISU-X (IDK1:1)
                                           TO  WRK-KAISU-H (IDK2:1)
                   END-IF
               END-PERFORM
               MOVE    SPACE               TO  WRK-KAISU-J
               MOVE    1                   TO  IDJ2
               PERFORM VARYING     IDJ     FROM    1  BY  1
                       UNTIL       IDJ     >   3
                   EVALUATE    WRK-KAISU-H (IDJ:1)
                       WHEN    SPACE
                           MOVE    "　"                TO  WRK-KAISU-J
                                                               (IDJ2:2)
                           ADD     2                   TO  IDJ2
                       WHEN    "1"  THRU  "9"
                           MOVE    WRK-KAISU-H (IDJ:1) TO  IDJ1
                           MOVE    TBL-SUJI (IDJ1)     TO  WRK-KAISU-J
                                                               (IDJ2:2)
                           ADD     2                   TO  IDJ2
                       WHEN    "0"
                           MOVE    TBL-SUJI (10)       TO  WRK-KAISU-J
                                                               (IDJ2:2)
                           ADD     2                   TO  IDJ2
                   END-EVALUATE
               END-PERFORM
               MOVE   "×"                 TO  SPA-GMN-TZAIKEI4 (IDG)
                                                               (3:2)
               MOVE   WRK-KAISU-J(1:4)     TO  SPA-GMN-TZAIKEI4 (IDG)
                                                               (5:4)
      *        計
               COMPUTE WRK-KEI             =   LNK-WKSRY-JIHIMONEY
                                                            (IDX IDZ)
                                           *   WRK-SRYKAISUKEI
               MOVE    SPACE               TO  WRK-ZAIKEIJIH
               MOVE    WRK-KEI             TO  WRK-ZAIKEIJIH
      *        日本語変換
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI
               MOVE    WRK-ZAIKEIJIH       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT   TO  WRK-ZAIKEIJIJ
               IF      LNK-WKSRY-JIHIMONEY(IDX IDZ)    =   ZERO
                   MOVE    SPACE               TO  SPA-GMN-TKINGAK4(IDG)
               ELSE
                   MOVE    "円"                TO  WRK-ZAIKEIJIJ(17:2)
                   MOVE    WRK-ZAIKEIJIJ       TO  SPA-GMN-TKINGAK4(IDG)
               END-IF
               MOVE    1                   TO  FLG-KAISUU
      *
               INSPECT SPA-GMN-TBANGO2 (IDG)
                                           REPLACING  ALL "  "  BY  "　"
               INSPECT SPA-GMN-TMEISYO2(IDG)
                                           REPLACING  ALL "  "  BY  "　"
               INSPECT SPA-GMN-TYOBI41 (IDG)
                                           REPLACING  ALL "  "  BY  "　"
               INSPECT SPA-GMN-TZAIKEI4(IDG)
                                           REPLACING  ALL "  "  BY  "　"
               INSPECT SPA-GMN-TKINGAK4(IDG)
                                           REPLACING  ALL "  "  BY  "　"
           END-IF
      *
      *    コメント以外の最終行セット
           IF      LNK-WKSRY-SRYCD (IDX IDZ)(1:1)   =   "0"  OR  "8"
               CONTINUE
           ELSE
               MOVE    IDG             TO  WRK-IDG
           END-IF
      *
           .
       31013-JIHIMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *     約束セットの時、再編集処理
      *****************************************************************
       310120-YAKUSOKU-SET-SEC        SECTION.
      *
           MOVE    SPACE               TO  INPUTCD-REC
           MOVE    SPA-HOSPID          TO  ICD-HOSPID
      *    セットの桁数は６とする
           MOVE    "6"                 TO  ICD-CDSYU
           MOVE    LNK-WKSRY-SRYCD (IDX IDZ)(1:6)
                                       TO  ICD-INPUTCD
      *
           PERFORM 930-INPUTCD-READ-SEC
      *
           IF      FLG-INPUTCD         =   ZERO
               MOVE    ICD-DSPNAME         TO  SPA-GMN-TMEISYO2(IDG)
           ELSE
               MOVE    LNK-WKSRY-SRYCD (IDX IDZ)
                                           TO  SPA-GMN-TMEISYO2(IDG)
           END-IF
      *
      *    数量（薬剤、器材のとき）
           MOVE    LNK-WKSRY-SRYSURYO (IDX IDZ)  TO  WRK-SURYO
           PERFORM 310121-SURYO-HEN-SEC
           MOVE    WRK-SURYO-J         TO  SPA-GMN-TSURYOU (IDG)
      *
           .
       310120-YAKUSOKU-SET-EXT.
           EXIT.
      *****************************************************************
      *     薬剤服用コメントの時、再編集処理
      *****************************************************************
       3100121-FUKIYO-MEISYO-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-MEISYO
      *
           STRING     "【"             DELIMITED   BY  SIZE
                      TNS-NAME         DELIMITED   BY  "    "
                      "】"             DELIMITED   BY  SIZE
                      INTO             WRK-MEISYO
           END-STRING
           .
       3100121-FUKIYO-MEISYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量表示用編集処理
      *****************************************************************
       310121-SURYO-HEN-SEC            SECTION.
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
               MOVE    "."                 TO  WRK-SURYO-Z2
               MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1-9
           END-IF
      *
      *    日本語変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-SURYO-X         TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  WRK-SURYO-J
           INSPECT WRK-SURYO-J         REPLACING
                                       ALL "Ｘ"  BY  "×"
      **** MOVE    SPACE               TO  WRK-SURYO-J
      *    MOVE    1                   TO  IDJ2
      *    PERFORM VARYING     IDJ     FROM    1  BY  1
      *            UNTIL       IDJ     >   9
      *       EVALUATE     WRK-SURYO-X (IDJ:1)
      *            WHEN    SPACE
      *                MOVE    "　"            TO  WRK-SURYO-J (IDJ2:2)
      *                ADD     2               TO  IDJ2
      *            WHEN    "."
      *                MOVE    "．"            TO  WRK-SURYO-J (IDJ2:2)
      *                ADD     2               TO  IDJ2
      *            WHEN    "1"  THRU  "9"
      *                MOVE    WRK-SURYO-X (IDJ:1) TO  IDJ1
      *                MOVE    TBL-SUJI (IDJ1)     TO  WRK-SURYO-J
      *                                                        (IDJ2:2)
      *                ADD     2                   TO  IDJ2
      *            WHEN    "0"
      *                MOVE    TBL-SUJI (10)       TO  WRK-SURYO-J
      *                                                        (IDJ2:2)
      *                ADD     2                   TO  IDJ2
      *        END-EVALUATE
      *****END-PERFORM
           .
       310121-SURYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量表示用編集処理
      *****************************************************************
       310122-BUNGSU-HEN-SEC           SECTION.
      *
      *    日本語変換
           MOVE    SPACE               TO  WRK-BUNGSU-J
           MOVE    1                   TO  IDJ2
           PERFORM VARYING     IDJ     FROM    1  BY  1
                   UNTIL       IDJ     >   3
              EVALUATE     WRK-BUNGSU-X (IDJ:1)
                   WHEN    SPACE
                       MOVE    "　"            TO  WRK-BUNGSU-J(IDJ2:2)
                       ADD     2               TO  IDJ2
                   WHEN    "."
                       MOVE    "．"            TO  WRK-BUNGSU-J(IDJ2:2)
                       ADD     2               TO  IDJ2
                   WHEN    "1"  THRU  "9"
                       MOVE    WRK-BUNGSU-X (IDJ:1)    TO  IDJ1
                       MOVE    TBL-SUJI (IDJ1)     TO  WRK-BUNGSU-J
                                                              (IDJ2:2)
                       ADD     2                   TO  IDJ2
                   WHEN    "0"
                       MOVE    TBL-SUJI (10)       TO  WRK-BUNGSU-J
                                                              (IDJ2:2)
                       ADD     2                   TO  IDJ2
               END-EVALUATE
           END-PERFORM
           .
       310122-BUNGSU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   入力コードの画面用再編集処理
      *****************************************************************
       31013-INPUTCD-HEN-SEC                  SECTION.
      *
      *    点数＊回数　計
      *    点数
           MOVE    WRK-ZAITEN          TO  WRK-TENSU
      *
           IF      WRK-SRYKAISUKEI     =   ZERO
               MOVE   1                    TO  WRK-SRYKAISUKEI
           END-IF
           COMPUTE WRK-KEI-G           =   WRK-ZAITEN
                                       *   WRK-SRYKAISUKEI
           MOVE    WRK-KEI-G           TO  WRK-KEI
      *    合計点数集計
           IF      LNK-WKSRY-SRYCD (01 01)     =   "630010002"
      *        薬剤料逓減　を減算
               COMPUTE SPA-GMN-GOKEI       =   SPA-GMN-GOKEI
                                           -   WRK-KEI-G
               COMPUTE WRK-KEI-GS          =   WRK-KEI-G
                                           *   -1
               MOVE    WRK-KEI-GS          TO  WRK-KEI
           ELSE
               ADD     WRK-KEI-G           TO  SPA-GMN-GOKEI
           END-IF
      *    回数
           MOVE    WRK-SRYKAISUKEI     TO  WRK-KAISU
           MOVE    ZERO                TO  IDK2
           MOVE    SPACE               TO  WRK-ZAIKEI 
           MOVE    SPACE               TO  WRK-KAISU-H 
           MOVE    SPACE               TO  WRK-KEI-H 
           PERFORM VARYING     IDK1    FROM    1   BY  1
      *!           UNTIL       IDK1    >   8
                   UNTIL       IDK1    >   3
               IF      WRK-KAISU-X (IDK1:1)    NOT =   SPACE
                   ADD     1           TO  IDK2
                   MOVE    WRK-KAISU-X (IDK1:1)
                                       TO  WRK-KAISU-H (IDK2:1)
               END-IF
           END-PERFORM
      *    計
           MOVE    ZERO                TO  IDK2
           PERFORM VARYING     IDK1    FROM    1   BY  1
                   UNTIL       IDK1    >   8
               IF      WRK-KEI-X   (IDK1:1)    NOT =   SPACE
                   ADD     1           TO  IDK2
                   MOVE    WRK-KEI-X   (IDK1:1)
                                       TO  WRK-KEI-H   (IDK2:1)
               END-IF
           END-PERFORM
      *
           STRING  WRK-TENSU               DELIMITED   BY  SIZE
                   "X"                     DELIMITED   BY  SIZE
      *!           WRK-KAISU-H             DELIMITED   BY  SPACE
                   WRK-KAISU-H             DELIMITED   BY  SIZE
                   "  "                    DELIMITED   BY  SIZE
                   WRK-KEI-H               DELIMITED   BY  SPACE
                   INTO                    WRK-ZAIKEI
           END-STRING
      *
      *    日本語変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-ZAIKEI          TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  WRK-ZAIKEIJ
           INSPECT WRK-ZAIKEIJ         REPLACING
                                       ALL "Ｘ"  BY  "×"
      *
           .
       31013-INPUTCD-HEN-EXT.
           EXIT.
      *
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
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                    PERFORM 450-TOROKU-SEC
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
           PERFORM 400-GMN-INPUT-SEC
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
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
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    保険組合せ選択
               WHEN    ANY             ALSO    "MEILIST"
                   PERFORM 4001-MEILIST-SEL-SEC
           END-EVALUATE
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ選択処理
      *****************************************************************
       4001-MEILIST-SEL-SEC          SECTION.
      *
      *    行選択
           MOVE    ZERO                TO  WRK-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   600
               IF      K08-SELECT (IDX)        =   "T"
                   MOVE    SPA-GMN-NAIBANGO (IDX)  TO  WRK-SELNUM
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM          >   ZERO
               MOVE    ZERO            TO  IDX-Y2
      *        選択の次ぎへ
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   10
                   IF      WRK-SELNUM      =   K08-SELNUM (IDX)
      *            既にある時は、削除を解除する
                       MOVE    ZERO            TO  K08-SELNUM (IDX)
                       MOVE    IDX             TO  IDX-Y2
                       MOVE    10              TO  IDX
                   ELSE
                       IF      K08-SELNUM (IDX)    =   ZERO
                           MOVE    WRK-SELNUM      TO  SPA-GMN-SELNUM
                                                                  (IDX)
                           PERFORM 41021-SELNUM-CHK-SEC
                           IF      SPA-ERRCD       =   SPACE
                               MOVE    WRK-SELNUM      TO  K08-SELNUM
                                                                   (IDX)
                           END-IF
                           MOVE    IDX             TO  IDX-Y2
                           MOVE    10              TO  IDX
                           MOVE    "SELNUM"        TO  WRK-MCP-WIDGET
                           MOVE    IDX-Y2          TO  WRK-MCP-WIDGET
                                                                (7:2)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           .
      *
       4001-MEILIST-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
      *
      *    関連処理
           PERFORM 4103-KANREI-SEC
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
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   10
               MOVE    K08-SELNUM (IDX)    TO  SPA-GMN-SELNUM (IDX)
           END-PERFORM
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    同一連番がある時、１つとする
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   10
               IF      SPA-GMN-SELNUM (IDX)    NOT =   ZERO
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   10
                       IF     (IDX         NOT =   IDY )  AND
                              (SPA-GMN-SELNUM (IDY)
                                           NOT =   ZERO )  AND
                              (SPA-GMN-SELNUM (IDX)
                                               =   SPA-GMN-SELNUM (IDY))
                           MOVE    ZERO        TO  SPA-GMN-SELNUM (IDY)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *    ゼロずめ
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >=  10
               IF      SPA-GMN-SELNUM (IDX)    =   ZERO
                   MOVE    SPA-GMN-SELNUM (IDX + 1 )
                                               TO  SPA-GMN-SELNUM (IDX)
                   MOVE    ZERO                TO  SPA-GMN-SELNUM
                                                            (IDX + 1)
               END-IF
           END-PERFORM
      *    番号存在チェック
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   10  )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-SELNUM (IDX)    NOT =   ZERO
                   PERFORM 41021-SELNUM-CHK-SEC
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除選択処理
      *****************************************************************
       41021-SELNUM-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY             >   600 )  OR
                              (FLG-OK          =   1     )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-SELNUM (IDX)    =   SPA-NAI-BANGO (IDY)
                   MOVE    1                   TO  FLG-OK
                   IF      SPA-NAI-JIDFLG (IDY)    =   ZERO
                       MOVE    "0003"              TO  SPA-ERRCD
                       MOVE    IDX                 TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    IDX                 TO  SPA-GMN-CUR
           END-IF
           .
       41021-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    関連処理
      *****************************************************************
       4103-KANREI-SEC              SECTION.
      *
      *    削除区分設置
           PERFORM 41031-DELFLG-SET-SEC
      *    点数再計算
           PERFORM 41032-TENSU-KEI-SEC
      *
           .
       4103-KANREI-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除区分設置処理
      *****************************************************************
       41031-DELFLG-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-DEL-ARI
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-K08-MAX
               MOVE    ZERO                TO  SPA-GMN-TDELFLG (IDX)
               MOVE    ZERO                TO  SPA-NAI-DELFLG  (IDX)
               IF      SPA-GMN-NAIIDX  (IDX)   >   ZERO
                   MOVE    SPA-GMN-NAIIDX  (IDX)   TO  IDX-NAI
                   IF      SPA-NAI-JIDFLG (IDX-NAI)    =   1
                       MOVE    1               TO  SPA-GMN-TDELFLG (IDX)
                       MOVE    1               TO  FLG-DEL-ARI
                   END-IF
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL      (IDY     >   10  )  OR
                                      (SPA-GMN-SELNUM (IDY)    =  ZERO)
                       IF      SPA-GMN-TBANGO (IDX)
                                               =   SPA-GMN-SELNUM (IDY)
                           MOVE    2               TO  SPA-GMN-TDELFLG
                                                                  (IDX)
                           MOVE    1               TO  SPA-NAI-DELFLG
                                                              (IDX-NAI)
                           MOVE    10              TO  IDY
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           .
       41031-DELFLG-SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    点数再計算処理
      *****************************************************************
       41032-TENSU-KEI-SEC              SECTION.
      *
      *    診察料・在宅料は削除できないこととする（金額算定）
           MOVE    ZERO                TO  SPA-GMN-HKNTEN (02)
           MOVE    ZERO                TO  SPA-GMN-HKNTEN (04)
           MOVE    ZERO                TO  SPA-GMN-HKNTEN (05)
           MOVE    ZERO                TO  SPA-GMN-HKNTEN (06)
           MOVE    ZERO                TO  SPA-GMN-HKNTEN (07)
           MOVE    ZERO                TO  SPA-GMN-HKNTEN (08)
           MOVE    ZERO                TO  SPA-GMN-HKNTEN (09)
           MOVE    ZERO                TO  SPA-GMN-HKNTEN (10)
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-K08-MAX
             IF      SPA-GMN-NAIIDX  (IDX)   >   ZERO
               MOVE    SPA-GMN-NAIIDX  (IDX)   TO  IDX-NAI
               IF      SPA-GMN-TDELFLG (IDX)   =   2
                   CONTINUE
               ELSE
                   EVALUATE    SPA-NAI-SRYKBN (IDX-NAI)(1:1)
      *                指導
                       WHEN    "1"
                           IF      SPA-NAI-SRYKBN (IDX-NAI)  =   "13"
                               ADD     SPA-NAI-HKNTEN(IDX-NAI)
                                               TO  SPA-GMN-HKNTEN (02)
                           END-IF
      *                投薬
                       WHEN    "2"
                           IF      SPA-NAI-HKNSFLG (IDX-NAI) =   ZERO
                               ADD     SPA-NAI-HKNTEN (IDX-NAI)
                                               TO  SPA-GMN-HKNTEN (04)
                           ELSE
                               COMPUTE SPA-GMN-HKNTEN (04)
                                               =   SPA-GMN-HKNTEN (04)
                                               -   SPA-NAI-HKNTEN
                                                              (IDX-NAI)
                           END-IF
                       WHEN    "3"
                           ADD     SPA-NAI-HKNTEN(IDX-NAI)
                                               TO  SPA-GMN-HKNTEN (05)
                       WHEN    "4"
                           ADD     SPA-NAI-HKNTEN(IDX-NAI)
                                               TO  SPA-GMN-HKNTEN (06)
                       WHEN    "5"
                           ADD     SPA-NAI-HKNTEN(IDX-NAI)
                                               TO  SPA-GMN-HKNTEN (07)
                       WHEN    "6"
                           ADD     SPA-NAI-HKNTEN(IDX-NAI)
                                               TO  SPA-GMN-HKNTEN (08)
                       WHEN    "7"
                           ADD     SPA-NAI-HKNTEN(IDX-NAI)
                                               TO  SPA-GMN-HKNTEN (09)
                       WHEN    "8"
                           ADD     SPA-NAI-HKNTEN(IDX-NAI)
                                               TO  SPA-GMN-HKNTEN (10)
      *                食事
                       WHEN    "9"
                           ADD     SPA-NAI-HKNTEN(IDX-NAI)
                                               TO  SPA-GMN-HKNTEN (10)
                   END-EVALUATE
               END-IF
             END-IF
           END-PERFORM
      *    各点数
           MOVE    ZERO                TO  SPA-GMN-GOKEI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   10
               ADD     SPA-GMN-HKNTEN (IDX)    TO  SPA-GMN-GOKEI
           END-PERFORM
           .
       41032-TENSU-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    SPA-HZN-MOTOPG      TO  SPA-SAKIPG
           MOVE    "K08"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
      **** MOVE    "JOIN"              TO  MCP-PUTTYPE.
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
      *    確定　処理
      *****************************************************************
       450-TOROKU-SEC             SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      450-TOROKU-EXT
           END-IF
      *
      *    入院更新処理
           IF      SPA-HZN-MOTOPG      =   "K02N"
               PERFORM 4502-NYUKOUSIN-SYORI-SEC
               GO      TO      450-TOROKU-EXT
           END-IF
      *
      *   包括分入力処理
          IF      SPA-HKNCOMBINUM     =   "9999"
               PERFORM 4503-HOUKATU-SYORI-SEC
               GO      TO      450-TOROKU-EXT
           END-IF
      *
      *    更新処理
           PERFORM 4501-KOUSIN-SYORI-SEC
      *
           MOVE    "K03"               TO  SPA-SAKIPG
           MOVE    "K08"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "K03"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       450-TOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       4501-KOUSIN-SYORI-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  IDG
           MOVE    ZERO                TO  FLG-SYOSIN-OK
      *
           MOVE    ZERO                TO  FLG-PARA
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA
           INITIALIZE                  LNK-WKSRYACT-AREA
           MOVE    SPACE               TO  SYUNOU-REC
           MOVE    ZERO                TO  IDX-KOUI
           MOVE    ZERO                TO  LNK-WKSRYACT-MAX
           INITIALIZE                  HZN-PARA-AREA
           MOVE    SPACE               TO  LNK-SYUNOU-AREA
           INITIALIZE                  LNK-SYUNOU-AREA
           MOVE    ZERO                TO  LNK-SYUNOU-MAX
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
                   UNTIL       FLG-PARA    =    1
               EVALUATE    PARA-FILEMEI
      *            ワーク診療行
                   WHEN    "WKSRYACT"
                       ADD     1                   TO  IDX
                       MOVE    PARA-DATA-REC       TO  LNK-WKSRYACT-REC
                                                                (IDX)
                       IF      LNK-WKSRY-HKNCOMBI(IDX) =   ZERO
                           MOVE    SPA-HKNCOMBINUM TO
                                                LNK-WKSRY-HKNCOMBI(IDX)
                       END-IF
                       IF      LNK-WKSRY-SRYKA   (IDX) =   SPACE
                           MOVE    SPA-SRYKA       TO
                                                LNK-WKSRY-SRYKA   (IDX)
                       END-IF
                       MOVE    IDX                 TO  LNK-WKSRYACT-MAX
      *            収納
                   WHEN    "SYUNOU"
                       ADD     1                   TO  LNK-SYUNOU-MAX
                       IF      LNK-SYUNOU-MAX      <=  15
                           MOVE    PARA-DATA-REC   TO  LNK-SYUNOU-REC
                                                       (LNK-SYUNOU-MAX)
                       END-IF
      *            その他
                   WHEN    OTHER
                       ADD     1                   TO  IDY
                       MOVE    PARA-REC            TO  HZN-PARA-REC(IDY)
               END-EVALUATE
      *
               PERFORM 980-PARA-READ-SEC
           END-PERFORM
           MOVE    IDX                 TO  WRK-MAX
      *
           CLOSE                       PARA-FILE
      *
      *    追加
           MOVE    SPA-TERMID          TO  FILE-TERMID2
           OPEN    OUTPUT              PARA-FILE
      *
      *    診療行為更新
           PERFORM 45011-SRYACT-SYORI-SEC
      *    収納更新
           PERFORM 45012-SYUNOU-SYORI-SEC
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   5
               IF      HZN-PARA-DATA-REC (IDY) NOT =   SPACE
                   MOVE    HZN-PARA-REC(IDY)   TO  PARA-REC
                   WRITE                       PARA-REC
               END-IF
           END-PERFORM
      *
           CLOSE                       PARA-FILE
      *
           .
       4501-KOUSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為更新処理
      *****************************************************************
       45011-SRYACT-SYORI-SEC             SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   LNK-WKSRYACT-MAX
               MOVE    ZERO                TO  FLG-DEL
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >   600
                   IF      LNK-WKSRY-ZAINUM (IDX)  =   SPA-NAI-ZAINUM
                                                                 (IDY)
                       IF      SPA-NAI-DELFLG (IDY)    =   1
                           MOVE    1               TO  FLG-DEL
                       END-IF
                       MOVE    600             TO  IDY
                   END-IF
               END-PERFORM
               IF      FLG-DEL             =   ZERO
      *            初診料算定判定
                   PERFORM 450112-SYOSIN-CHK-SEC
                   ADD     1                   TO  IDX2
                   MOVE    "WKSRYACT"          TO  PARA-FILEMEI
                   MOVE    IDX2                TO  PARA-EDANUM
                   MOVE    LNK-WKSRYACT-REC (IDX)
                                               TO  PARA-DATA-REC
                   WRITE                   PARA-REC
               ELSE
                   MOVE    SPACE               TO  LNK-WKSRYACT-REC(IDX)
                   INITIALIZE                      LNK-WKSRYACT-REC(IDX)
               END-IF
           END-PERFORM
      *
           .
       45011-SRYACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診料算定判定処理
      *****************************************************************
       450112-SYOSIN-CHK-SEC             SECTION.
      *
           IF      LNK-WKSRY-SRYKBN(IDX)   =   "11"  OR  "13"
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >   5
                   EVALUATE    LNK-WKSRY-SRYCD  (IDX IDY)
                       WHEN    "111000110"
                       WHEN    "111003610"
                       WHEN    "111700110"
                       WHEN    "111700210"
                       WHEN    "113003510"
                       WHEN    "113003710"
                       WHEN    "101110010"
                           MOVE    1             TO  FLG-SYOSIN-OK
                   END-EVALUATE
               END-PERFORM
           END-IF
           .
       450112-SYOSIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納更新処理
      *****************************************************************
       45012-SYUNOU-SYORI-SEC             SECTION.
      *
      *    初期設定処理
           PERFORM 450121-GAIFTN-SYOKI-SEC
      *
           MOVE    SPACE               TO  LNK-SYUNOU2-AREA
           INITIALIZE                  LNK-SYUNOU2-AREA
           INITIALIZE                  LNK-SYUNOU2-MAX
      *
           PERFORM VARYING   IDH1      FROM    1   BY  1
                   UNTIL    (IDH1      >   15  )   OR
                            (SPA-TBL-HKNCOMBI(IDH1)    =   SPACE)
               MOVE    SPA-TBL-HKNCOMBI(IDH1)     TO  WRK-HKNCOMBI
               PERFORM VARYING   IDH2      FROM   1   BY  1
                       UNTIL    (IDH2      >   10  )  OR
                                (SPA-TBL-SRYKA (IDH1 IDH2) =   SPACE )
                   MOVE    SPA-TBL-SRYKA (IDH1 IDH2)   TO  WRK-SRYKA
                   PERFORM VARYING     IDH     FROM    1   BY  1
                           UNTIL      (IDH     >   15  )  OR
                                      (IDH     >   LNK-SYUNOU-MAX)
                       IF      (LNK-SYU-HKNCOMBINUM (IDH)
                                               =   WRK-HKNCOMBI ) AND
                               (LNK-SYU-SRYKA (IDH)
                                               =   WRK-SRYKA    )
                           PERFORM 45013-SYUNOU-HENSYU-SEC
                       END-IF
                   END-PERFORM
               END-PERFORM
               IF     (SPA-TBL-SRYKA (IDH1 2)  NOT =   SPACE)
      *            複数科合計追加
                   MOVE    ZERO            TO  WRK-SRYKA
                   PERFORM 45014-SYUNOU-ADD-SEC
               END-IF
           END-PERFORM
      *    複数科まとめ集計フラグ
           MOVE    SPA-KAGRPFLG        TO  LNK-S01-KAGRPFLG
      *    負担金計算処理
           CALL    "ORCSGAIFTN"        USING
                                       MCPAREA
                                       LNK-ORCS01-REC
                                       LNK-SYUNOU2-AREA
      *
           IF      SPA-KAGRPFLG        =   "0"
      *        複数科まとめ集計の差額計算
               PERFORM 45016-SYUNOU-SAGAKU-SEC
           ELSE
      *        複数科まとめ集計をしない場合、科合計を計算
               PERFORM 45015-SYUNOU-GOUKEI-SEC
           END-IF
      *
           MOVE    ZERO                TO  PARA-EDANUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   LNK-SYUNOU2-MAX
               ADD     1                   TO  PARA-EDANUM
               MOVE    "SYUNOU"            TO  PARA-FILEMEI
               MOVE    LNK-SYUNOU2-REC(IDX)
                                           TO  PARA-DATA-REC
               WRITE                       PARA-REC
           END-PERFORM
      *
           .
       45012-SYUNOU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期設定処理
      *****************************************************************
       450121-GAIFTN-SYOKI-SEC             SECTION.
      *
           INITIALIZE                  LNK-ORCS01-REC
           MOVE    ZERO                TO  IDX-Y
           INITIALIZE                  WRK-CONTCHK-TBL
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
           MOVE    ZERO                TO  IDY
           ADD     1                   TO  IDX-Y
      *
           MOVE    ZERO                TO  FLG-FUNSAI
           MOVE    ZERO                TO  WRK-FUNSAI-CNT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   LNK-WKSRYACT-MAX
      *        剤毎にクリアする
               IF     (IDX                 >   1    )  AND
                      (LNK-WKSRY-ZAINUM (IDX) NOT =
                                           LNK-WKSRY-ZAINUM (IDX - 1))
                   MOVE    ZERO            TO  FLG-FUNSAI
                   MOVE    ZERO            TO  WRK-FUNSAI-CNT
               END-IF
      *
               IF     (LNK-WKSRY-SRYYMD (IDX)      =   SPACE ) OR
                      (LNK-WKSRY-HKNCOMBI(IDX) NOT =   SYU-HKNCOMBINUM)
                  OR  ((SYU-SRYKA              NOT =   ZERO  )  AND
                      (LNK-WKSRY-SRYKA   (IDX) NOT =   SYU-SRYKA  ))
                   CONTINUE
               ELSE
      *            診療内容編集（薬剤と服用方法)
                   PERFORM VARYING     IDS     FROM    1   BY  1
                           UNTIL      (IDS         >   5   )  OR
                                      (LNK-WKSRY-SRYCD(IDX IDS)
                                                   =   SPACE )
                       PERFORM 45051-ORCS01-SET-SEC
                   END-PERFORM
               END-IF
      *        診察料算定フラグ（地方公費用）
               IF      LNK-WKSRY-SRYKBN (IDX)  =   "11"
                                               OR  "12"
                                               OR  "13"
                                               OR  "14"
                   MOVE    "1"                 TO  LNK-S01-SINSATUFLG
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
      *
               END-IF
           END-PERFORM
      *    訂正処理
           IF      SPA-SYORIFLG        =   1
               MOVE    "1"             TO  LNK-S01-SAIKEISAN-KBN
           END-IF
      *Ｈ１４．１０以降
      *    高齢者（在宅総合・在宅末期）算定患者区分
           IF     ( SPA-SRYYMD         <   "20021001")  OR
                  ((SPA-NETAKIRI-ARI   =   ZERO )  AND
                   (SPA-NETAKIRI-ARI2  =   ZERO )  AND
                   (SPA-ZAIMATU-ARI    =   ZERO )  AND
                   (SPA-ZAIMATU-ARI2   =   ZERO )  )
               CONTINUE
           ELSE
      *        保険算定年齢が７０歳以上か、老人保険の時
               INITIALIZE                  ORCSAGECHKAREA
               MOVE    SPA-HOSPID          TO  AGECHK-HOSPID
               MOVE    SPA-PTID            TO  AGECHK-PTID
               MOVE    SPA-SRYYMD          TO  AGECHK-SRYYMD
               MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
               MOVE    SPA-NYUGAIKBN       TO  AGECHK-NYUGAIKBN
               CALL    "ORCSAGECHK"        USING
                                           ORCSAGECHKAREA
               IF     (AGECHK-NENREI       >=   70  )  OR
                      (SPA-NAI-ROUJIN      =    1   )
                   MOVE    "1"             TO
                                           LNK-S01-KOUREI-ZAITAKU-KBN
               END-IF
           END-IF
      *    初診料算定フラグ（地方公費用）
           IF      FLG-SYOSIN-OK       =   1
               MOVE    "1"             TO  LNK-S01-SYOSINFLG
           END-IF
      *
           .
       450121-GAIFTN-SYOKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納更新処理
      *****************************************************************
       45013-SYUNOU-HENSYU-SEC             SECTION.
      *
           MOVE    LNK-SYUNOU-REC (IDH)    TO  SYUNOU-REC
           MOVE    ZERO                TO  SYU-HKNTEN(02)
           MOVE    ZERO                TO  SYU-HKNTEN(04)
           MOVE    ZERO                TO  SYU-HKNTEN(05)
           MOVE    ZERO                TO  SYU-HKNTEN(06)
           MOVE    ZERO                TO  SYU-HKNTEN(07)
           MOVE    ZERO                TO  SYU-HKNTEN(08)
           MOVE    ZERO                TO  SYU-HKNTEN(09)
           MOVE    ZERO                TO  SYU-HKNTEN(10)
      *    各点数
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >  600
               IF     (SPA-NAI-HKNCOMBI(IDY)   =   LNK-SYU-HKNCOMBINUM
                                                             (IDH)) AND
                     ((LNK-SYU-SRYKA   (IDH)   =   ZERO      )  OR
                      (SPA-NAI-SRYKA   (IDY)   =   LNK-SYU-SRYKA
                                                             (IDH))) AND
                      (SPA-NAI-DELFLG (IDY)    NOT =   1  )
                   PERFORM 450120-HKNTEN-KEI-SEC
               END-IF
           END-PERFORM
           MOVE    ZERO                TO  SYU-HKNTEN(12)
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   11
      *        保険合計点数
               ADD     SYU-HKNTEN(IDX)         TO  SYU-HKNTEN(12)
           END-PERFORM
      *
           ADD     1                   TO  LNK-SYUNOU2-MAX
      *
      *    薬剤一部負担金計算処理
           PERFORM 4505-YAKFTN-KEI-SEC
      *
           MOVE    SYUNOU-REC          TO  LNK-SYUNOU2-REC
                                                   (LNK-SYUNOU2-MAX)
           .
       45013-SYUNOU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納更新・科合計追加処理
      *****************************************************************
       45014-SYUNOU-ADD-SEC             SECTION.
      *
           MOVE    SPACE               TO  SYUNOU-REC
           INITIALIZE                  SYUNOU-REC
           PERFORM VARYING     IDS1    FROM    1   BY  1
                   UNTIL       IDS1    >   LNK-SYUNOU2-MAX
               IF      LNK-SYU2-HKNCOMBINUM(IDS1)
                                           =   WRK-HKNCOMBI
                   IF      SYU-PTID        =   ZERO
                       MOVE    LNK-SYUNOU2-REC (IDS1)
                                           TO  SYUNOU-REC
                   ELSE
                       MOVE    LNK-SYUNOU2-REC (IDS1)
                                               TO  WRK-SYUNOU-REC
                       PERFORM 450141-SYUNOU-ADD-HEN-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    ZERO                TO  SYU-SRYKA
           ADD     1                   TO  LNK-SYUNOU2-MAX
           MOVE    SYUNOU-REC          TO  LNK-SYUNOU2-REC
                                                   (LNK-SYUNOU2-MAX)
      *
           .
       45014-SYUNOU-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納更新・科合計追加処理
      *****************************************************************
       450141-SYUNOU-ADD-HEN-SEC             SECTION.
      *
           ADD     WRK-SYU-TAX-TAISHOU TO  SYU-TAX-TAISHOU
           ADD     WRK-SYU-TAX-MONEY   TO  SYU-TAX-MONEY
           ADD     WRK-SYU-SKYGK       TO  SYU-SKYGK
           ADD     WRK-SYU-HKNTEKMONEY TO  SYU-HKNTEKMONEY
           PERFORM VARYING     IDS2    FROM    1   BY  1
                   UNTIL       IDS2    >   12
               ADD    WRK-SYU-HKNTEN (IDS2)    TO  SYU-HKNTEN(IDS2)
               ADD    WRK-SYU-MONEY (IDS2)     TO  SYU-MONEY (IDS2)
               ADD    WRK-SYU-TGMONEY (IDS2)   TO  SYU-TGMONEY(IDS2)
               ADD    WRK-SYU-TGMONEY-TAX (IDS2)
                                               TO  SYU-TGMONEY-TAX(IDS2)
           END-PERFORM
           PERFORM VARYING     IDS2    FROM    1   BY  1
                   UNTIL       IDS2    >   5
               ADD    WRK-SYU-JIHI-TAXNASI (IDS2)
                                           TO  SYU-JIHI-TAXNASI(IDS2)
               ADD    WRK-SYU-JIHI-TAXARI (IDS2)
                                           TO  SYU-JIHI-TAXARI(IDS2)
           END-PERFORM
           ADD     WRK-SYU-JIHI-TOTAL      TO  SYU-JIHI-TOTAL
           ADD     WRK-SYU-JIHI-TOTAL-TAX  TO  SYU-JIHI-TOTAL-TAX
           ADD     WRK-SYU-JIHI-TAX        TO  SYU-JIHI-TAX
           ADD     WRK-SYU-RJN-FTN         TO  SYU-RJN-FTN
           ADD     WRK-SYU-KOH-FTN         TO  SYU-KOH-FTN
           ADD     WRK-SYU-KOH-FTN-ENTANI  TO  SYU-KOH-FTN-ENTANI
           ADD     WRK-SYU-YKZ-FTN         TO  SYU-YKZ-FTN
      *調整金
           ADD     WRK-SYU-CHOSEI          TO  SYU-CHOSEI
      *労災保険
           ADD     WRK-SYU-RSISHOSHIN-MONEY
                                           TO  SYU-RSISHOSHIN-MONEY
           ADD     WRK-SYU-RSISAISHIN-MONEY
                                           TO  SYU-RSISAISHIN-MONEY
           ADD     WRK-SYU-RSISDO-MONEY    TO  SYU-RSISDO-MONEY
           ADD     WRK-SYU-RSIETC-MONEY    TO  SYU-RSIETC-MONEY
           ADD     WRK-SYU-RSI-TAX-SAI     TO  SYU-RSI-TAX-SAI
           ADD     WRK-SYU-RSI-TOTAL       TO  SYU-RSI-TOTAL
           ADD     WRK-SYU-RSIJIBAI-SKYMONEY
                                           TO  SYU-RSIJIBAI-SKYMONEY
      *請求金額−消費税（再掲）
           ADD     WRK-SYU-SKYMONEY-TAX-SAI TO  SYU-SKYMONEY-TAX-SAI
      *請求金額
           ADD     WRK-SYU-SKYMONEY        TO  SYU-SKYMONEY
      *入金合計額
           ADD     WRK-SYU-NYUKIN-TOTAL    TO  SYU-NYUKIN-TOTAL 
      *診療区分別給付外点数
           PERFORM VARYING     IDS2    FROM    1   BY  1
                   UNTIL       IDS2    >   6
               ADD    WRK-SYU-KYUFUGAI-TEN (IDS2)
                                       TO  SYU-KYUFUGAI-TEN(IDS2)
           END-PERFORM
      *
           .
       450141-SYUNOU-ADD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数科まとめ集計の差額計算処理
      *****************************************************************
       45016-SYUNOU-SAGAKU-SEC             SECTION.
      *
           PERFORM VARYING     IDS1    FROM    1   BY  1
                   UNTIL       IDS1    >   LNK-SYUNOU2-MAX
               IF      LNK-SYU2-SRYKA (IDS1)   =   ZERO
                   MOVE    LNK-SYU2-HKNCOMBINUM(IDS1)
                                               TO  WRK-HKNCOMBI
                   MOVE    ZERO                TO  WRK-SKYMONEY
                   MOVE    LNK-SYUNOU2-REC (IDS1)  TO  SYUNOU-REC
                   PERFORM VARYING     IDS2    FROM    1   BY  1
                           UNTIL       IDS2    >   LNK-SYUNOU2-MAX
                       IF     (LNK-SYU2-HKNCOMBINUM(IDS2)
                                               =   WRK-HKNCOMBI ) AND
                              (LNK-SYU2-SRYKA (IDS2)
                                           NOT =   ZERO  )
                           ADD     LNK-SYU2-SKYMONEY(IDS2)
                                               TO  WRK-SKYMONEY
                       END-IF
                   END-PERFORM
                   COMPUTE SYU-GRP-SGKMONEY    =   SYU-SKYMONEY
                                               -   WRK-SKYMONEY
                   MOVE    SYUNOU-REC          TO  LNK-SYUNOU2-REC(IDS1)
      *            収納に差額編集、請求額変更
                   MOVE    ZERO                TO  IDS3
                   MOVE    SYU-GRP-SGKMONEY    TO  WRK-GRP-SGKMONEY
                   PERFORM VARYING     IDS2    FROM    LNK-SYUNOU2-MAX
                                                       BY  -1
                           UNTIL      (IDS2    <   1   )  OR
                                      (IDS3    >   ZERO )
                       IF     (LNK-SYU2-HKNCOMBINUM(IDS2) 
                                               =   WRK-HKNCOMBI ) AND
                              (LNK-SYU2-SRYKA (IDS2)
                                           NOT =   ZERO  )
                           IF     (SYU-GRP-SGKMONEY    >   ZERO ) OR
                                  (LNK-SYU2-SKYMONEY(IDS2)
                                               >=  WRK-GRP-SGKMONEY)
                               MOVE    IDS2        TO  IDS3
                           END-IF
                       END-IF
                   END-PERFORM
                   IF      IDS3                >   ZERO
                       MOVE    SYU-GRP-SGKMONEY
                                              TO  LNK-SYU2-GRP-SGKMONEY
                                                                (IDS3)
                       COMPUTE LNK-SYU2-SKYMONEY(IDS3)
                                               =   LNK-SYU2-SKYMONEY
                                                                (IDS3)
                                               +   SYU-GRP-SGKMONEY
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       45016-SYUNOU-SAGAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数科まとめ集計なしの収納合計作成処理
      *****************************************************************
       45015-SYUNOU-GOUKEI-SEC             SECTION.
      *
           MOVE    SPACE               TO  SYUNOU-REC
           INITIALIZE                  SYUNOU-REC
           PERFORM VARYING     IDS1    FROM    1   BY  1
                   UNTIL       IDS1    >   LNK-SYUNOU2-MAX
               IF      LNK-SYU2-SRYKA (IDS1)   =   ZERO
                   MOVE    LNK-SYU2-HKNCOMBINUM(IDS1)
                                               TO  WRK-HKNCOMBI
                   MOVE    SPACE               TO  SYUNOU-REC
                   INITIALIZE                  SYUNOU-REC
                   PERFORM VARYING     IDS3    FROM    1   BY  1
                           UNTIL       IDS3    >   LNK-SYUNOU2-MAX
                       IF     (LNK-SYU2-HKNCOMBINUM(IDS3)
                                               =   WRK-HKNCOMBI ) AND
                              (LNK-SYU2-SRYKA (IDS3)
                                           NOT =   ZERO  )
                           IF      SYU-PTID        =   ZERO
                               MOVE    LNK-SYUNOU2-REC (IDS3)
                                                   TO  SYUNOU-REC
                               MOVE    ZERO        TO  SYU-SRYKA
                           ELSE
                               MOVE    LNK-SYUNOU2-REC (IDS3)
                                                   TO  WRK-SYUNOU-REC
                               PERFORM 450141-SYUNOU-ADD-HEN-SEC
                           END-IF
                       END-IF
                   END-PERFORM
                   MOVE    SYUNOU-REC      TO  LNK-SYUNOU2-REC (IDS1)
               END-IF
           END-PERFORM
      *
           .
       45015-SYUNOU-GOUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤一部負担金計算処理
      *****************************************************************
       450120-HKNTEN-KEI-SEC             SECTION.
      *
           EVALUATE    SPA-NAI-SRYKBN (IDY)(1:1)
      *        指導
               WHEN    "1"
                   IF      SPA-NAI-SRYKBN (IDY)    =   "13"
                       ADD     SPA-NAI-HKNTEN(IDY)
                                                   TO  SYU-HKNTEN (02)
                   END-IF
      *        投薬
                   WHEN    "2"
                       IF      SPA-NAI-HKNSFLG (IDY)   =   ZERO
                           ADD     SPA-NAI-HKNTEN (IDY)
                                                   TO  SYU-HKNTEN (04)
                       ELSE
                           COMPUTE SYU-HKNTEN (04)
                                                   =   SYU-HKNTEN (04)
                                                   -   SPA-NAI-HKNTEN
                                                                 (IDY)
                       END-IF
                   WHEN    "3"
                       ADD     SPA-NAI-HKNTEN(IDY)
                                               TO  SYU-HKNTEN (05)
                   WHEN    "4"
                       ADD     SPA-NAI-HKNTEN(IDY)
                                               TO  SYU-HKNTEN (06)
                   WHEN    "5"
                       ADD     SPA-NAI-HKNTEN(IDY)
                                               TO  SYU-HKNTEN (07)
                   WHEN    "6"
                       ADD     SPA-NAI-HKNTEN(IDY)
                                               TO  SYU-HKNTEN (08)
                   WHEN    "7"
                       ADD     SPA-NAI-HKNTEN(IDY)
                                               TO  SYU-HKNTEN (09)
                   WHEN    "8"
                       ADD     SPA-NAI-HKNTEN(IDY)
                                               TO  SYU-HKNTEN (10)
      *                食事
                   WHEN    "9"
                       ADD     SPA-NAI-HKNTEN(IDY)
                                               TO  SYU-HKNTEN (10)
           END-EVALUATE
      *
           .
       450120-HKNTEN-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤一部負担金計算処理
      *****************************************************************
       4505-YAKFTN-KEI-SEC             SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   LNK-WKSRYACT-MAX
      *
               IF     (LNK-WKSRY-SRYYMD (IDX)      =   SPACE ) OR
                      (LNK-WKSRY-HKNCOMBI(IDX) NOT =   SYU-HKNCOMBINUM)
                  OR  ((SYU-SRYKA              NOT =   ZERO  )  AND
                      (LNK-WKSRY-SRYKA   (IDX) NOT =   SYU-SRYKA  ))
                   CONTINUE
               ELSE
      *            自賠責の時、技術点を集計する
                   IF      (SPA-HKNKBN         =   1   )  AND
                           (SPA-RSI-HKNKBN     =   "4" )  AND
                           (LNK-WKSRY-SYUTEN1 (IDX)    >   ZERO )
                       PERFORM 45052-JIBAI-SYORI-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
      *    船員保険で初診料算定時
           IF     (SYU-SYUHKNNUM       =   "002" )  AND
                  (SYU-HKNTEN (01)     >   ZERO  )  AND
                  (SPA-SYOSIN          =   1     )
               PERFORM 45053-SENINFLG-SET-SEC
           END-IF
      *
      *福岡県公費対応
      *    福岡県で、一般保険算定時
           IF     (SPA-PREFNUM         =   40   )  AND
                  (SPA-HKNKBN          =   ZERO )
               PERFORM 45054-PREFNUM-40-SEC
           END-IF
      *
           .
       4505-YAKFTN-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    一部負担額計算用セット処理
      *****************************************************************
       45051-ORCS01-SET-SEC            SECTION.
      *
      *    粉砕の時最初の１剤のみとする
           IF      LNK-WKSRY-SRYCD(IDX IDS)    =   "099209901"
               ADD     1                   TO  WRK-FUNSAI-CNT
               MOVE    1                   TO  FLG-FUNSAI
               GO      TO      45051-ORCS01-SET-EXT
           END-IF
           IF     (FLG-FUNSAI          =   1   )  AND
                  (WRK-FUNSAI-CNT      >   1   )
               MOVE    ZERO                TO  FLG-FUNSAI
               GO      TO      45051-ORCS01-SET-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-FUNSAI
           IF     (LNK-WKSRY-SRYCD(IDX IDS)(1:1) =   "6"  ) OR
                  (LNK-WKSRY-SRYCD(IDX IDS)(1:3) =   "001")
      *        院内薬剤のみとする
               MOVE    LNK-WKSRY-SRYSYUKBN (IDX) TO
                                                   WRK-CHK-SRYSYUKBN
               IF      WRK-CHK-SRYSYUKBN       =   SPACE
                   EVALUATE    LNK-WKSRY-SRYKBN (IDX)
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
                   IF      IDY                 >   40
                       GO      TO      45051-ORCS01-SET-EXT
                   END-IF
                   MOVE    LNK-WKSRY-SRYKBN(IDX)
                                                   TO  LNK-S01-SRYKBN
                                                          (IDX-Y IDY)
                   MOVE    LNK-WKSRY-ZAINUM (IDX)
                                                   TO  LNK-S01-ZAIBAN
                                                          (IDX-Y IDY)
                   MOVE    LNK-WKSRY-SRYCD (IDX IDS)
                                                   TO  LNK-S01-SRYCD
                                                          (IDX-Y IDY)
                   MOVE    LNK-WKSRY-SRYSURYO(IDX IDS)
                                                   TO  LNK-S01-SURYOU
                                                          (IDX-Y IDY)
                   MOVE    LNK-WKSRY-ZAIKAIKEI(IDX)
                                                   TO  LNK-S01-KAISUU
                                                          (IDX-Y IDY)
                   MOVE    LNK-WKSRY-ZAITENKEI(IDX)
                                                   TO  LNK-S01-TENSUU
                                                          (IDX-Y IDY)
               END-IF
           END-IF
      *
           .
       45051-ORCS01-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責の時、技術点を集計処理
      *****************************************************************
       45052-JIBAI-SYORI-SEC          SECTION.
      *
      *    診察料と診断料等の時、診療コードを判定する
           MOVE    ZERO                TO  WRK-SYUTEN1
           IF      LNK-WKSRY-SRYKBN (IDX)  =   "11"  OR  "12"  OR
                                               "13"  OR  "80"
               PERFORM VARYING     IDS     FROM    1   BY  1
                       UNTIL      (IDS         >   5   )  OR
                                  (LNK-WKSRY-SRYCD(IDX IDS)
                                              =   SPACE )
      *
                   MOVE    ZERO                TO  FLG-RSIFLG
                   MOVE    LNK-WKSRY-SRYCD (IDX IDS)   TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
      *            労災の金額算定分の時、点数をゼロとする
                   IF     (LNK-WKSRY-SRYCD (IDX IDS)(1:3)
                                               =   "101" ) AND
                          (TNS-TENSIKIBETU     =   1    )
                       MOVE    1                   TO  FLG-RSIFLG
                   END-IF
      *            自賠責で証明料等の時、点数をゼロとする
                   IF     (LNK-WKSRY-SRYKBN(IDX)       =   "80")  AND
                          (LNK-WKSRY-SRYCD (IDX IDS)(1:5)
                                                       =   "09591" OR
                                                           "09592" OR
                                                           "09593" OR
                                                           "09594" )
                       MOVE    1                   TO  FLG-RSIFLG
                   END-IF
                   IF      FLG-RSIFLG          =   1
                       COMPUTE WRK-TEN         =   TNS-TEN   /   10
                       ADD     WRK-TEN         TO  WRK-SYUTEN1
                   END-IF
      *            自賠責金額集計
                   IF      LNK-WKSRY-SRYKBN(IDX)       =   "80"
      *                自賠責診断書料集計
                       IF      LNK-WKSRY-SRYCD (IDX IDS)(1:5)
                                                       =   "09591"
                           COMPUTE WRK-SINDAN  =   TNS-TEN
                                               *   LNK-WKSRY-ZAIKAIKEI
                                                               (IDX)
                           ADD     WRK-SINDAN      TO  LNK-S01-SINDAN
                                                      (LNK-SYUNOU2-MAX)
                       END-IF
      *                自賠責明細書料集計
                       IF      LNK-WKSRY-SRYCD (IDX IDS)(1:5)
                                                       =   "09592"
                           COMPUTE WRK-SINDAN  =   TNS-TEN
                                               *   LNK-WKSRY-ZAIKAIKEI
                                                               (IDX)
                           ADD     WRK-SINDAN      TO  LNK-S01-MEISAI
                                                      (LNK-SYUNOU2-MAX)
                       END-IF
                   END-IF
      *            療養担当手当の点数をゼロとする
                   IF     (LNK-WKSRY-SRYKBN(IDX)       =   "80")  AND
                          (LNK-WKSRY-SRYCD (IDX IDS)   =   "199000610")
                       ADD     TNS-TEN         TO  WRK-SYUTEN1
      *                自賠責療養担当手当点数
                       COMPUTE WRK-SINDAN      =   TNS-TEN
                                               *   LNK-WKSRY-ZAIKAIKEI
                                                               (IDX)
                       ADD     WRK-SINDAN      TO  LNK-S01-RYOYO
                                                      (LNK-SYUNOU2-MAX)
                   END-IF
      *
               END-PERFORM
           END-IF
      *
      *    薬剤料逓減は対象外
           IF      LNK-WKSRY-SRYCD (IDX 01)    =   "630010002"
               GO      TO      450511-JIBAI-SYORI-EXT
           END-IF
      *
      *    画像診断の時、フィルム点数を対象外とする
           IF      LNK-WKSRY-SRYKBN (IDX)  =   "70"
               MOVE    LNK-WKSRY-SYUTEN2 (IDX)     TO  WRK-SYUTEN1
           END-IF
      *
      *    腰部固定帯加算を器材点数として対象外とする
           IF      LNK-WKSRY-SRYKBN(IDX)(1:1)  NOT =   "9"
               PERFORM VARYING     IDS     FROM    1   BY  1
                       UNTIL      (IDS         >   5   )  OR
                                  (LNK-WKSRY-SRYCD(IDX IDS)
                                              =   SPACE )
                   IF     LNK-WKSRY-SRYCD(IDX IDS)     =   "140037490"
      ***                                              OR  "140040110"
      ***                                              OR  "150266970"
                       MOVE    LNK-WKSRY-SRYCD (IDX IDS)   TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
                       ADD     TNS-TEN         TO  WRK-SYUTEN1
                   END-IF
               END-PERFORM
           END-IF
      *
      *    自賠責手技点合計
           IF      LNK-WKSRY-SYUTEN1 (IDX) <   WRK-SYUTEN1
               CONTINUE
           ELSE
               COMPUTE LNK-S01-SYUTEN (LNK-SYUNOU2-MAX)
                                           =   LNK-S01-SYUTEN
                                                      (LNK-SYUNOU2-MAX)
                                           + ((LNK-WKSRY-SYUTEN1 (IDX)
                                           -   WRK-SYUTEN1    )
                                           *   LNK-WKSRY-ZAIKAIKEI
                                                             (IDX) )
           END-IF
      *
           .
       450511-JIBAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    船員保険で初診料算定時セット処理
      *****************************************************************
       45053-SENINFLG-SET-SEC            SECTION.
      *
      *    保険組合マスター編集
           MOVE    SPACE               TO  HKNCOMBI-REC
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPID         TO  COMB-HOSPID
           MOVE    SPA-PTID           TO  COMB-PTID
           MOVE    SPA-HKNCOMBINUM    TO  COMB-HKNCOMBINUM
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
               PERFORM 910-HKNCOMBI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           IF      FLG-HKNCOMBI        =   ZERO
      *        本人で通勤災害
               IF     (COMB-HONKZKKBN      =   "1"  )  AND
                      (COMB-HOJOKBN        =   "3"  OR
                                               "C"  OR
                                               "F"  )
                   MOVE    "1"             TO  LNK-S01-SENINFLG
                                                      (LNK-SYUNOU2-MAX)
               END-IF
           END-IF
           .
       45053-SENINFLG-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    福岡県公費対応処理
      *****************************************************************
       45054-PREFNUM-40-SEC            SECTION.
      *
           MOVE    ZERO                TO  IDX-SYU
           MOVE    ZERO                TO  IDX-SYU2
           INITIALIZE                  ORCSSYU40AREA
           MOVE    "1"                 TO  ORCS40-KBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   LNK-WKSRYACT-MAX )  OR
                              (IDX-SYU >=  5                )
      *
           IF     (LNK-WKSRY-SRYYMD (IDX)      =   SPACE ) OR
                  (LNK-WKSRY-HKNCOMBI(IDX) NOT =   SYU-HKNCOMBINUM)
              OR  ((SYU-SRYKA              NOT =   ZERO  )  AND
                  (LNK-WKSRY-SRYKA   (IDX) NOT =   SYU-SRYKA  ))
               CONTINUE
           ELSE
      *        剤毎にクリアする
               IF     (IDX                 >   1    )  AND
                      (LNK-WKSRY-ZAINUM (IDX) NOT =
                                           LNK-WKSRY-ZAINUM (IDX - 1))
                   MOVE    ZERO            TO  IDX-SYU2
               END-IF
      *
      *        初診・往診、小児科外来診察料の初診時
               IF     (LNK-WKSRY-SRYKBN (IDX)  =   "11"
                                               OR  "14" )  OR
                     ((SPA-SYONIKA-ARI         =   1    )  AND
                      (SPA-SYOSIN              =   1    )  AND
                      (LNK-WKSRY-SRYKBN (IDX)  =   "13"
                                               OR  "99"  ))
      *            連番が１の時、テーブル編集
                   IF      LNK-WKSRY-RENNUM (IDX)  =   1
                       ADD     1                   TO  IDX-SYU
                       MOVE    LNK-WKSRY-SRYKBN (IDX)  TO  ORCS40-SRYKBN
                                                           (IDX-SYU)
                   END-IF
                   MOVE    LNK-WKSRY-ZAITENKEI (IDX)   TO  ORCS40-ZAITEN
                                                           (IDX-SYU)
      *            診療内容編集
                   PERFORM VARYING     IDS     FROM    1   BY  1
                           UNTIL      (IDS         >   5   )  OR
                                      (LNK-WKSRY-SRYCD(IDX IDS)
                                                   =   SPACE )  OR
                                      (IDX-SYU2    >=  5     )
                       IF     (LNK-WKSRY-SRYCD(IDX IDS)(1:1) =   "1") OR
                              (LNK-WKSRY-SRYCD(IDX IDS) =   "099114001")
                            ADD     1              TO  IDX-SYU2
                            MOVE    LNK-WKSRY-SRYCD(IDX IDS)
                                                   TO  ORCS40-SRYCD
                                                     (IDX-SYU IDX-SYU2)
                       END-IF
                   END-PERFORM
      *
               END-IF
      *
               END-IF
           END-PERFORM
      *
      *    対象があった場合
           IF      IDX-SYU             >   ZERO
      *        給付対象外点数編集
               CALL    "ORCSSYU40"     USING
                                       SPA-AREA
                                       ORCSSYU40AREA
                                       SYUNOU-REC
           END-IF
      *
           .
       45054-PREFNUM-40-EXT.
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
            IF    ((SPA-SYORIFLG           =   1    ) AND
      **********    SPA-DOUJI-RENNUM    NOT =   JYURRK-DOUJI-RENNUM) OR
                   (SPA-DOUJI-RENNUM       >    JYURRK-DOUJI-RENNUM)) OR
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
                       IF    ((ACCT-SRYKBN(1:1)    =   "2"  )  OR
                              (ACCT-SRYKBN         =   "80" ))
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
                             OR   (IDY         >=  40   )
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
      *    入院更新処理
      *****************************************************************
       4502-NYUKOUSIN-SYORI-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  IDG
      *
           MOVE    ZERO                TO  FLG-PARA
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA
           INITIALIZE                  LNK-WKSRYACT-AREA
           MOVE    SPACE               TO  SYUNOU-REC
           MOVE    ZERO                TO  IDX-KOUI
           MOVE    ZERO                TO  LNK-WKSRYACT-MAX
           INITIALIZE                  HZN-PARA-AREA
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
                   UNTIL       FLG-PARA    =    1
               EVALUATE    PARA-FILEMEI
      *            ワーク診療行
                   WHEN    "WKSRYACT"
                       ADD     1                   TO  IDX
                       MOVE    PARA-DATA-REC       TO  LNK-WKSRYACT-REC
                                                                (IDX)
                       MOVE    IDX                 TO  LNK-WKSRYACT-MAX
      *            その他
                   WHEN    OTHER
                       ADD     1                   TO  IDY
                       MOVE    PARA-REC            TO  HZN-PARA-REC(IDY)
               END-EVALUATE
      *
               PERFORM 980-PARA-READ-SEC
           END-PERFORM
           MOVE    IDX                 TO  WRK-MAX
      *
           CLOSE                       PARA-FILE
      *
      *    削除
           IF      (SPA-SYORIFLG       =   1  )  AND
                   (LNK-WKSRYACT-MAX   =   ZERO)
               IF      FLG-KAKU-FLG        =   ZERO
                   MOVE    "0101"              TO  SPA-KIDCD
                   GO      TO     4502-NYUKOUSIN-SYORI-EXT
               END-IF
           END-IF
      *
      *    追加
           MOVE    SPA-TERMID          TO  FILE-TERMID2
           OPEN    OUTPUT              PARA-FILE
      *
      *    診療行為更新
           PERFORM 45011-SRYACT-SYORI-SEC
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   5
               IF      HZN-PARA-DATA-REC (IDY) NOT =   SPACE
                   MOVE    HZN-PARA-REC(IDY)   TO  PARA-REC
                   WRITE                       PARA-REC
               END-IF
           END-PERFORM
      *
           CLOSE                       PARA-FILE
      *
      *    入院更新処理
           INITIALIZE                  ORCSCNYUINAREA
           MOVE    "1"                 TO  ORCSCNYUIN-KBN
      *    退院日
           MOVE    SPA-K02N-TAIINYMD   TO  ORCSCNYUIN-TAIINYMD
           CALL    "ORCSCNYUIN"        USING
                                       SPA-AREA
                                       ORCSCNYUINAREA
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO     4502-NYUKOUSIN-SYORI-EXT
           END-IF
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           INITIALIZE                      SPA-KYOTUKEY
           INITIALIZE                      SPA-K01KYOUTU
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    "K02N"              TO  SPA-SAKIPG
           MOVE    "K08"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           IF      FLG-KAKU-FLG        =   1
               MOVE    "JOIN"              TO  MCP-PUTTYPE
           END-IF
      *
           MOVE    "K02N"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4502-NYUKOUSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括分入力更新（外来）処理
      *****************************************************************
       4503-HOUKATU-SYORI-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  IDG
      *
           MOVE    ZERO                TO  FLG-PARA
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA
           INITIALIZE                  LNK-WKSRYACT-AREA
           MOVE    SPACE               TO  SYUNOU-REC
           MOVE    ZERO                TO  IDX-KOUI
           MOVE    ZERO                TO  LNK-WKSRYACT-MAX
           INITIALIZE                  HZN-PARA-AREA
           MOVE    SPACE               TO  LNK-SYUNOU-AREA
           INITIALIZE                  LNK-SYUNOU-AREA
           MOVE    ZERO                TO  LNK-SYUNOU-MAX
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
                   UNTIL       FLG-PARA    =    1
               EVALUATE    PARA-FILEMEI
      *            ワーク診療行
                   WHEN    "WKSRYACT"
                       ADD     1                   TO  IDX
                       MOVE    PARA-DATA-REC       TO  LNK-WKSRYACT-REC
                                                                (IDX)
                       IF      LNK-WKSRY-HKNCOMBI(IDX) =   ZERO
                           MOVE    SPA-HKNCOMBINUM TO
                                                LNK-WKSRY-HKNCOMBI(IDX)
                       END-IF
                       IF      LNK-WKSRY-SRYKA   (IDX) =   SPACE
                           MOVE    SPA-SRYKA       TO
                                                LNK-WKSRY-SRYKA   (IDX)
                       END-IF
                       MOVE    IDX                 TO  LNK-WKSRYACT-MAX
      *            収納
                   WHEN    "SYUNOU"
                       ADD     1                   TO  LNK-SYUNOU-MAX
                       IF      LNK-SYUNOU-MAX      <=  15
                           MOVE    PARA-DATA-REC   TO  LNK-SYUNOU-REC
                                                       (LNK-SYUNOU-MAX)
                       END-IF
      *            その他
                   WHEN    OTHER
                       ADD     1                   TO  IDY
                       MOVE    PARA-REC            TO  HZN-PARA-REC(IDY)
               END-EVALUATE
      *
               PERFORM 980-PARA-READ-SEC
           END-PERFORM
           MOVE    IDX                 TO  WRK-MAX
      *
           CLOSE                       PARA-FILE
      *
      *    確認メッセージ
           IF      FLG-KAKU-FLG        =   ZERO
               IF      (SPA-SYORIFLG       =   1  )  AND
                       (LNK-WKSRYACT-MAX   =   ZERO)
                   MOVE    "0103"              TO  SPA-KIDCD
               ELSE
                   MOVE    "0102"              TO  SPA-KIDCD
               END-IF
               GO      TO     4503-HOUKATU-SYORI-EXT
           END-IF
      *
      *    追加
           MOVE    SPA-TERMID          TO  FILE-TERMID2
           OPEN    OUTPUT              PARA-FILE
      *
      *    診療行為更新
           PERFORM 45011-SRYACT-SYORI-SEC
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   5
               IF      HZN-PARA-DATA-REC (IDY) NOT =   SPACE
                   MOVE    HZN-PARA-REC(IDY)   TO  PARA-REC
                   WRITE                       PARA-REC
               END-IF
           END-PERFORM
      *
           CLOSE                       PARA-FILE
      *
      *    外来更新処理
           INITIALIZE                  ORCSCGAIRAIAREA
           IF      LNK-WKSRYACT-MAX   =   ZERO
      *        削除
               MOVE    "2"                 TO  ORCSCGAIRAI-KBN
           ELSE
      *        新規、更新
               MOVE    "1"                 TO  ORCSCGAIRAI-KBN
           END-IF
      *    収納情報
           MOVE    LNK-SYUNOU-MAX      TO  ORCSCGAIRAI-SYUNOU-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   LNK-SYUNOU-MAX
               MOVE    LNK-SYUNOU-REC(IDX) TO  ORCSCGAIRAI-SYUNOU-REC
                                                               (IDX)
           END-PERFORM
      *
           CALL    "ORCSCGAIRAI"       USING
                                       SPA-AREA
                                       ORCSCGAIRAIAREA
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO     4503-HOUKATU-SYORI-EXT
           END-IF
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           INITIALIZE                      SPA-KYOTUKEY
           INITIALIZE                      SPA-K01KYOUTU
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    "K02"               TO  SPA-SAKIPG
           MOVE    "K08"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           IF      FLG-KAKU-FLG        =   1
               MOVE    "JOIN"              TO  MCP-PUTTYPE
           END-IF
      *
           MOVE    "K02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4503-HOUKATU-SYORI-EXT.
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
           IF      SPA-KIDCD       NOT =   SPACE
               PERFORM 520-KIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "K08"               TO  MCP-WINDOW.
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
           IF     (FLG-GMN-INIT        =   1   )  OR
                  (SPA-K08-MAX         =   ZERO )
               MOVE    ZERO                TO  WRK-K08-ROW
           ELSE
               MOVE    K08-ROW             TO  WRK-K08-ROW
           END-IF
      *
           MOVE    SPACE               TO  K08
      *
      *    コラムリストの位置指定
           MOVE    ZERO                TO  K08-ROW
           MOVE    ZERO                TO  K08-ROWATTR
           IF      WRK-K08-ROW     NOT =   ZERO
               MOVE    WRK-K08-ROW         TO  K08-ROW
           END-IF
      *
      *
           MOVE    SPA-PTNUM           TO  K08-PTNUM
           MOVE    SPA-KANANAME        TO  K08-KANANAME
           MOVE    SPA-NAME            TO  K08-NAME
           MOVE    SPA-SEX             TO  K08-SEX
           MOVE    SPA-HKNCOMBIMEI
                                       TO  K08-HKNKUMI
           MOVE    SPA-FTNRATEMEI      TO  K08-RATE
           MOVE    SPA-SRYYMDW         TO  K08-SRYYMD
           MOVE    SPA-BIRTHDAYW       TO  K08-BIRTHDAY
           MOVE    SPA-SRYKAMEI        TO  K08-SRYKA
      *
           MOVE    SPA-NENREI-HEN      TO  K08-NENREI
      *
      *    処理
           EVALUATE    SPA-SYORIFLG
               WHEN    0
                   MOVE    SPACE           TO  K08-SYORIMEI-VALUE
               WHEN    1
                   MOVE    "red"           TO  K08-SYORIMEI-STYLE
                   MOVE    "［訂　正］"    TO  K08-SYORIMEI-VALUE
           END-EVALUATE
      *
      *
           MOVE    SPA-GMN-GOKEI       TO  WRK-GOKEI-Z2
           MOVE    WRK-GOKEI-X2        TO  K08-GOKEITEN
           MOVE    SPA-GMN-LASTYMD     TO  K08-LASTYMD
           MOVE    SPA-GMN-SYOSINYMD   TO  K08-SYOSINYMD
      *
           MOVE    SPA-GMN-MISYUMONEY  TO  WRK-GOKEI-Z3
           MOVE    WRK-GOKEI-X3        TO  K08-MISYUMONEY
      *
      *    累計点数
      *        訂正の時、”＊”
      *    IF      SPA-SYORIFLG        =   1
      *        MOVE    ALL "*"             TO  K08-ZAITENTOTAL
      **** ELSE
      *    累計点数（入院のみ）
           IF      SPA-NYUGAIKBN       =   "2"
               COMPUTE WRK-GOKEI-Z2        =   SPA-GMN-GOKEI
                                           +   SPA-ZAITENTOTAL
               MOVE    WRK-GOKEI-Z2        TO  K08-ZAITENTOTAL
           END-IF
      *
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-K08-MAX
      *********MOVE    SPA-GMN-TMEISYO (IDX)   TO  K08-ITEM(IDX)
               MOVE    SPA-GMN-TMEISYO (IDX)   TO  K08-MEISAI (IDX)
      *        番号
               IF      SPA-GMN-TBANGO  (IDX)   NOT =   ZERO
                   MOVE    SPA-GMN-TBANGO  (IDX)   TO  WRK-ZZ
                   MOVE    WRK-ZZ                  TO  K08-BANGO (IDX)
               END-IF
      *        削除フラグ
               EVALUATE    SPA-GMN-TDELFLG (IDX)
                   WHEN    0
                       MOVE    SPACE           TO  K08-DELFLG (IDX)
                   WHEN    1
                       MOVE    "◎"            TO  K08-DELFLG (IDX)
                   WHEN    2
                       MOVE    "削"            TO  K08-DELFLG (IDX)
               END-EVALUATE
      *
               IF      SPA-GMN-TMEISYO (IDX)(1:1)  =  "-"
                   MOVE    ALL "-"                 TO  K08-BANGO (IDX)
                   MOVE    ALL "-"                 TO  K08-DELFLG(IDX)
               END-IF
               IF      SPA-GMN-TMEISYO (IDX)(1:1)  =  "#"
                   MOVE    ALL "#"                 TO  K08-BANGO (IDX)
                   MOVE    ALL "#"                 TO  K08-DELFLG(IDX)
               END-IF
               IF      SPA-GMN-TMEISYO (IDX)(1:1)  =  "="
                   MOVE    ALL "="                 TO  K08-BANGO (IDX)
                   MOVE    ALL "="                 TO  K08-DELFLG(IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-K08-MAX         TO  K08-COUNT
           MOVE    "user-font"         TO  K08-MEI-STYLE
      *
      *    各点数
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   10
               MOVE    SPA-GMN-HKNTEN  (IDX)   TO  WRK-Z9
               EVALUATE    IDX
                   WHEN    1
                       MOVE    WRK-Z9          TO  K08-SSUHKNTEN
                   WHEN    2
                       MOVE    WRK-Z9          TO  K08-SDOHKNTEN
                   WHEN    3
                       MOVE    WRK-Z9          TO  K08-ZTKHKNTEN
                   WHEN    4
                       MOVE    WRK-Z9          TO  K08-TYKHKNTEN
                   WHEN    5
                       MOVE    WRK-Z9          TO  K08-CSYHKNTEN
                   WHEN    6
                       MOVE    WRK-Z9          TO  K08-SYCHKNTEN
                   WHEN    7
                       MOVE    WRK-Z9          TO  K08-SJTHKNTEN
                   WHEN    8
                       MOVE    WRK-Z9          TO  K08-KNSHKNTEN
                   WHEN    9
                       MOVE    WRK-Z9          TO  K08-GZUHKNTEN
                   WHEN    10
                       MOVE    WRK-Z9          TO  K08-ETCHKNTEN
               END-EVALUATE
           END-PERFORM
      *    選択
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   10
               MOVE    SPA-GMN-SELNUM  (IDX)   TO  K08-SELNUM (IDX)
           END-PERFORM
      *
      *    メッセージ
           MOVE    SPA-GMN-MSGDATA     TO  K08-MSGDATA-VALUE
           IF      SPA-GMN-MSGDATA     =   SPACE
               MOVE    "black"             TO  K08-MSGDATA-STYLE
           ELSE
               MOVE    "red"               TO  K08-MSGDATA-STYLE
           END-IF
      *
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
           IF      SPA-GMN-CUR         =   ZERO
               IF      WRK-MCP-WIDGET(1:6)     =   "SELNUM"
                   MOVE    WRK-MCP-WIDGET(7:2)     TO  IDX-Y2
                   IF      SPA-GMN-SELNUM (IDX-Y2) =   ZERO
                       MOVE    11                  TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
           IF      SPA-GMN-CUR         =   ZERO
               PERFORM VARYING     IDX-Y2  FROM    1   BY  1
                       UNTIL       IDX-Y2  >   10
                   IF      SPA-GMN-SELNUM (IDX-Y2) =   ZERO
                       MOVE    IDX-Y2              TO  SPA-GMN-CUR
                       MOVE    10                  TO  IDX-Y2
                   END-IF
               END-PERFORM
               IF      SPA-GMN-CUR         =   ZERO
                   MOVE    11                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    01
                   MOVE    "SELNUM01"        TO  MCP-WIDGET
               WHEN    02
                   MOVE    "SELNUM02"        TO  MCP-WIDGET
               WHEN    03
                   MOVE    "SELNUM03"        TO  MCP-WIDGET
               WHEN    04
                   MOVE    "SELNUM04"        TO  MCP-WIDGET
               WHEN    05
                   MOVE    "SELNUM05"        TO  MCP-WIDGET
               WHEN    06
                   MOVE    "SELNUM06"        TO  MCP-WIDGET
               WHEN    07
                   MOVE    "SELNUM07"        TO  MCP-WIDGET
               WHEN    08
                   MOVE    "SELNUM08"        TO  MCP-WIDGET
               WHEN    09
                   MOVE    "SELNUM09"        TO  MCP-WIDGET
               WHEN    10
                   MOVE    "SELNUM10"        TO  MCP-WIDGET
               WHEN    11
                   MOVE    "B12"             TO  MCP-WIDGET
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
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "剤番号がありません"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "自動算定分ではありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "削除できません。"
                                       TO  SPA-ERRMSG(27:)
               WHEN    "0005"
                   MOVE    "更新ができませんでした。"
                                       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "１日の受診履歴が１３５剤以上になります。"
                                       TO  SPA-ERRMSG
                   MOVE    "剤を減らして入力して下さい。"
                                       TO  SPA-ERRMSG(41:)
               WHEN    "1000"
                   MOVE    "５００行以上になります。"
                                       TO  SPA-ERRMSG
                   MOVE    "５００行以上は表示しません。"
                                       TO  SPA-ERRMSG(25:)
               WHEN    "1036"
                   MOVE    "受診履歴の伝票番号がゼロです。"
                                       TO  SPA-ERRMSG
                   MOVE    "訂正できません。"
                                       TO  SPA-ERRMSG(31:)
               WHEN    OTHER
                   MOVE    SPA-ERRCD   TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  KERR
           MOVE    SPA-ERRCD           TO  KERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  KERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "K08"               TO  SPA-MOTOPG
           MOVE    "KERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KERR"              TO  MCP-WINDOW.
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
      *
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-KIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-KIDCD
               WHEN    "0101"
                   MOVE    "該当の受診履歴を削除します。"
                                       TO  WRK-KIDMSG
                   MOVE    "よろしいですか？"
                                       TO  WRK-KIDMSG(29:)
               WHEN    "0102"
                   MOVE    "包括分入力を登録します。"
                                       TO  WRK-KIDMSG
               WHEN    "0103"
                   MOVE    "包括分入力の受診履歴を削除します。"
                                       TO  WRK-KIDMSG
                   MOVE    "よろしいですか？"
                                       TO  WRK-KIDMSG(35:)
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
               WHEN    "0101"
                   MOVE    "ＮＯ"            TO  KID1-B01
           END-EVALUATE
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "K08"               TO  SPA-MOTOPG
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
      *    パラメタデータ順読み
      *****************************************************************
       990-PARA-RAND-SEC         SECTION.
      *
           READ    PARA-FILE
               INVALID
                   MOVE    1           TO  FLG-PARA
               NOT INVALID
                   MOVE    ZERO        TO  FLG-PARA
           END-READ
      *
           .
       990-PARA-RAND-EXT.
           EXIT.
      *****************************************************************
      *    パラメタデータ順読み
      *****************************************************************
       990-PARA-NEXT-SEC         SECTION.
      *
           READ    PARA-FILE           NEXT
               AT  END
                   MOVE    1           TO  FLG-PARA
               NOT AT  END
                   MOVE    ZERO        TO  FLG-PARA
           END-READ
      *
           .
       990-PARA-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    入力コードマスター読込
      *****************************************************************
       930-INPUTCD-READ-SEC         SECTION.
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "INPUTCD-KEY"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
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
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "INPUTCD-KEY"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       930-INPUTCD-NEXT-EXT.
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
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       960-SYSKANRI-READ-SEC         SECTION.
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
       960-SYSKANRI-READ-EXT.
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
