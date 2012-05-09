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
       PROGRAM-ID.                 ORCSC92.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為の点数編集処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    多々納       01/04/11  施設基準の修正等
      *  01.00.02    多々納       01/05/18  入力コードの表示連番追加
      *  01.00.03    多々納       01/07/09  老人一般コード変換
      *  01.00.04    多々納       01/07/25  チェックのみ（区分：３）
      *                                     併用算定チェック追加
      *  02.00.00    多々納       02/03/01  訂正時、併用算定回数を
      *                                     カウントしない
      *  02.00.01    NACL-多々納  02/08/06  時間外判定に労災再診を追加
      *  02.00.02    NACL-多々納  02/08/26  用法の時施設基準チェックをし
      *                                     ない
      *  02.00.03    NACL-多々納  02/10/02  注加算チェックを剤毎とする
      *  02.00.04    NACL-多々納  02/10/03  労災加算なしの追加
      *  02.00.05    NACL-多々納  02/10/18  入院回数追加
      *  02.00.06    NACL-多々納  02/10/21  慢性疼痛管理料の改正追加
      *  02.00.07    NACL-多々納  02/11/28  慢性疼痛管理料の改正修正
      *  02.00.08    NACL-多々納  02/12/06  慢性疼痛の算定チェック修正
      *  02.00.09    NACL-多々納  03/02/10  病床数（一般）チェック追加
      *  02.00.10    NACL-多々納  03/05/21  慢性疼痛の同時チェック追加
      *  02.00.11    NACL-多々納  03/07/02  入院の消炎鎮痛処理と
      *                                     在宅寝たきり処置チェック変更
      *  02.00.12    NACL-多々納  04/11/30  きざみ値上限判定修正他
      *                                     コメントコード判定追加
      *  02.00.13    NACL-多々納  05/01/13  入院の消炎鎮痛処理と
      *                                     慢性疼痛管理料チェック追加
      *  02.00.14    NACL-多々納  05/11/22  MONFUNC 対応 他
      *  02.00.15    NACL-多々納  06/01/23  用量割合コード追加
      *  02.00.16    NACL-多々納  06/03/06  経過措置対応、入力値設定他
      *  03.03.00    NACL-多々納  06/10/20  矯正固定等追加
      *  03.04.00    NACL-多々納  06/11/13  検査正式名称表示追加
      *  03.04.01    NACL-多々納  06/12/11  薬剤附加情報追加
      *  03.04.02    NACL-多々納  07/03/XX  リハビリ医学管理料追加(H19.4改正）
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  04.01.00    NACL-多々納  07/11/21  器材商品名表示対応
      *  04.01.00    NACL-多々納  07/12/XX  患者禁忌薬剤対応
      *  04.01.01    NACL-多々納  08/01/XX  チェックマスタ変更対応
      *  04.05.00    NACL-多々納  09/06/XX  数量小数５桁対応
      *  04.05.00    NACL-多々納  09/10/XX  多剤投与・換算値等対応
      *  04.05.00    NACL-多々納  09/12/09  患者禁忌薬剤期間対応
      *  04.05.00    NACL-多々納  10/03/XX  平成２２年４月改正対応
      ***04.06.00    NACL-多々納  10/07/XX  ユーザーコメント端数対応
      *  04.05.00    NACL-多々納  12/03/XX  平成２４年４月改正対応
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-SRYCDCHG        PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
           03  FLG-TENSUPLUS2      PIC 9(01).
           03  FLG-CHKMST          PIC 9(01).
           03  FLG-PTKINKI         PIC 9(01).
      *H24.4
           03  FLG-GENERICPRICE    PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-SST             PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-NYUIN           PIC 9(01).
      *
           03  FLG-MANSEI-ARI      PIC 9(01).
      *
           03  FLG-SANTEI-OK       PIC 9(01).
           03  FLG-SANTEI-OK2      PIC 9(01).
      *    手術施設基準不適合
           03  FLG-SSTKIJUNCD-ERR      PIC  9(01).
           03  FLG-INPUT-CHK           PIC  9(01).
      *
           03  FLG-SP                  PIC 9(01).
      *
           03  FLG-MANSEI-CHK          PIC 9(01).
      *
           03  FLG-KONKAI              PIC 9(01).
           03  FLG-DAYCHK              PIC 9(01).
           03  FLG-DAYERR              PIC 9(01).
      *
           03  FLG-RKAN-OK             PIC 9(01).
           03  FLG-RKAN-OK2            PIC 9(01).
           03  FLG-RKAN-ARI            PIC 9(01).
           03  FLG-RKAN-ARI2           PIC 9(01).
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
      *
           03  IDX-C               PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-K               PIC 9(04).
           03  IDX-SST             PIC 9(04).
           03  IDX-SS2             PIC 9(04).
           03  IDX-Y2              PIC 9(02).
           03  IDX-DD              PIC 9(02).
      *
           03  IDZ                 PIC 9(02).
           03  IDX-ICD1            PIC 9(01).
           03  IDX-ICD             PIC 9(02).
      *
           03  IDX-STR             PIC 9(02).
           03  IDX-END             PIC 9(02).
           03  IDX-Y3              PIC 9(02).
      *
           03  IDM                 PIC 9(04).
           03  IDM2                PIC 9(04).
      *
           03  IDX-T2              PIC 9(04).
           03  IDX-T3              PIC 9(04).
           03  IDX-N2              PIC 9(02).
           03  IDX-SN              PIC 9(04).
           03  IDD                 PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
      *    単位名称
           03  WRK-TANINAME        PIC X(04).
      *
           03  WRK-MEISYO          PIC X(80).
           03  WRK-MEISYO-2        PIC X(80).
           03  WRK-MSG1            PIC X(04).
      *   きざみ値行為再計算用
           03  WRK-KEI-TEN         PIC 9(08)V9(03).
           03  WRK-KEI-TEN1        PIC 9(08)V9(03).
           03  WRK-KEI-TEN2        PIC 9(08).
           03  WRK-SURYO           PIC 9(05)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(05).
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1    PIC ZZZZZ.
               05  WRK-SURYO-Z1-9  REDEFINES   WRK-SURYO-Z1
                                   PIC ZZZZ9.
               05  WRK-SURYO-Z2    PIC X(01).
               05  WRK-SURYO-Z3    PIC ZZZZZ.
           03  WRK-SURYO-H         PIC X(20).
      *    年齢判定用
           03  WRK-KGNAGE          PIC 9(03).
           03  WRK-JGNAGE          PIC 9(03).
      *    最大行
           03  WRK-MAX             PIC 9(04).
      *    算定回数
           03  WRK-SANTEI-CNT      PIC 9(04).
      *    診療科名称
           03  WRK-SRYKAMEI        PIC X(20).
      *
           03  WRK-SRYCD               PIC X(09).
           03  WRK-SRYCD2              PIC X(09).
           03  WRK-SRYCD-3             PIC X(09).
      *
           03  WRK-SYOSINYMD1          PIC X(08).
           03  WRK-YMD.
               05  WRK-YYMM            PIC X(06).
               05  WRK-DD              PIC 9(02).
           03  WRK-YMD2.
               05  WRK-YYMM2           PIC X(06).
               05  WRK-DD2             PIC 9(02).
           03  WRK-FSANTEIYMD          PIC X(08).
           03  WRK-STYMD.
               05  WRK-STYYMM          PIC X(06).
               05  WRK-STDD            PIC 9(02).
           03  WRK-EDYMD.
               05  WRK-EDYYMM          PIC X(06).
               05  WRK-EDDD            PIC 9(02).
      *
           03  WRK-CHK-CD              PIC X(09).
           03  WRK-CHK-CD2             PIC X(09).
      *
           03  WRK-MOJI                PIC X(01).
           03  WRK-BUNGSU              PIC 9(08).
      *    エラーメッセージ
           03  WRK-MANSEI-ERR          PIC X(04).
      *    年齢上限
           03  WRK-TNS-KGNAGE          PIC X(02).
           03  WRK-TNS-JGNAGE          PIC X(02).
           03  WRK-LNK-SC92-ERRCD5     PIC X(04).
      *
           03  WRK-KAN-KBN             PIC X(01).
           03  WRK-KAN-KBN2            PIC X(01).
      *
      *    チェックマスタ変更
           03  WRK-ERR-SRYCD           PIC X(09).
           03  WRK-ERR-YMD             PIC X(08).
           03  WRK-KEI-SRYCD           PIC X(09).
           03  WRK-KEI-YMD             PIC X(08).
           03  WRK-MANSEI-YMD          PIC X(08).
           03  FLG-CHKERR              PIC 9(01).
           03  FLG-SANTEI-TBL          PIC 9(01).
           03  WRK-TEI-CNT             PIC 9(03).
           03  FLG-SYO-ERR             PIC 9(01).
           03  WRK-DAYERR              PIC 9(02).
      *ver.4.5
      *    換算値計算エラー
           03  WRK-SURYO-KEI       PIC 9(10)V9(8).
           03  WRK-SURYO-KEI-R     REDEFINES   WRK-SURYO-KEI.
               05  WRK-SURYO-KEI1  PIC 9(05).
               05  WRK-SURYO-KEI2  PIC 9(05).
               05  WRK-SURYO-KEI3  PIC 9(05).
               05  WRK-SURYO-KEI4  PIC 9(03).
      *H24.4
           03  WRK-NEN-03.
               05  WRK-NEN03-YM.
                   07  WRK-NEN03-YY        PIC 9(04).
                   07  WRK-NEN03-MM        PIC 9(02).
               05  WRK-NEN03-DD            PIC 9(02).
           03  WRK-NEN-15.
               05  WRK-NEN15-YM.
                   07  WRK-NEN15-YY        PIC 9(04).
                   07  WRK-NEN15-MM        PIC 9(02).
               05  WRK-NEN15-DD            PIC 9(02).
      *H24.4
      *
      *    コメントの入力値用
       01  WRK-INPUT-SET-AREA.
           03  WRK-INPUT-TBL.
               05  WRK-INPUT-SET       PIC 9(04)   OCCURS  8.
           03  WRK-INPUT-TBL-R         REDEFINES   WRK-INPUT-TBL.
               05  WRK-INPUT-TBL2          OCCURS  4.
                   07  WRK-INPUT-ICHI      PIC 9(04).
                   07  WRK-INPUT-KETA      PIC 9(04).
           03  WRK-KETA                PIC 9(02).
           03  WRK-KETA2               PIC 9(02).
           03  WRK-ICHI                PIC 9(02).
           03  WRK-ICHI2               PIC 9(02).
           03  IDX-KNJ                 PIC 9(02).
           03  IDX-KNJ1                PIC 9(01).
           03  IDX-KNJ2                PIC 9(04).
           03  WRK-ZZZ9-X.
               05  WRK-ZZZ9                PIC ZZZZZZZ9.
           03  WRK-ATAI-X.
               05  WRK-ATAI-9          PIC 9(15).
           03  WRK-KANJI.
               05  WRK-KANJI-X             PIC X(02)
                                           OCCURS   8.
           03  WRK-ATAI-XX             PIC X(08).
      *    小数点以下あり
           03  WRK-SYOKBN-G.
               05  WRK-SYOKBN          PIC 9(01)    OCCURS   4.
           03  IDX-S1                  PIC 9(02).
           03  IDX-S2                  PIC 9(02).
      *
           03  WRK-TIME                PIC 9(01).
      *
           03  WRK-HEN-INPUTCD         PIC X(22).
           03  WRK-IDX-FT              PIC 9(04).
           03  WRK-CD-MCNT             PIC 9(04).
      *
      *    老人・一般変換用
           03  WRK-SRYCD-CHG-AREA.
               05  WRK-SRYCDCHG        PIC X(09)   OCCURS  5.
           03  WRK-NAIYOU              PIC X(22).
           03  WRK-PATH                PIC X(64).
      *H24.4
      *    最低薬価
           03  WRK-CHKTEN               PIC 9(09)V9(2).
      *
      *    施設規準テーブル
       01  WRK-TBL-SSTKIJUN-AREA.
           03  WRK-TBL-SSTKIJUN-OCC       OCCURS  3500.
               05  WRK-TBL-SSTKIJUN       PIC 9(01).
      *
      *   日本語数字
       01  TBL-SUJI-AREA.
           03  FILLER          PIC X(20)   VALUE
              "１２３４５６７８９０".
       01  TBL-SUJI-R          REDEFINES   TBL-SUJI-AREA.
           03  TBL-SUJI        PIC X(02)   OCCURS   10.
      *
      *    リハビリ医学管理料テーブル
       01  TBL-RIHA-KANRI-AREA.
      *    リハビリ医学管理料
           03  FILLER              PIC X(11)   VALUE   "1 180029910".
           03  FILLER              PIC X(11)   VALUE   "1 180030010".
           03  FILLER              PIC X(11)   VALUE   "2 180030110".
           03  FILLER              PIC X(11)   VALUE   "2 180030210".
           03  FILLER              PIC X(11)   VALUE   "3 180030310".
           03  FILLER              PIC X(11)   VALUE   "3 180030410".
           03  FILLER              PIC X(11)   VALUE   "4 180030510".
           03  FILLER              PIC X(11)   VALUE   "4 180030610".
       01  TBL-RIHA-KANRI-AREA-R   REDEFINES   TBL-RIHA-KANRI-AREA.
           03  TBL-RIHA-KANRI-OCC      OCCURS   8
                                                INDEXED   TBL-RKA.
               05  TBL-RIHA-KAN-KBN        PIC X(01).
               05  TBL-RIHA-YOBI           PIC X(01).
               05  TBL-RIHA-KAN-SRYCD      PIC X(09).
      *
      *    保険別算定履歴テーブル
           COPY   "CMSANTEITBL.INC".
      *    リハビリ同一算定コードテーブル
           COPY   "CMRIHACDTBL.INC".
      *
      *    酸素コードテーブル
           COPY     "CMSANSOTBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *    点数マスタ薬剤付加コード
        01 TENSUPLUS2-REC.
           COPY    "CPTENSUPLUS2.INC".
      *
      *    点数マスタ−（ワーク）
           COPY    "CPTENSU.INC"       REPLACING
                                       //TNS-// BY //HZN-TNS-//.
      *
      *    システム管理マスタ
           COPY    "CPSK1006.INC".
      *    システム管理マスタ
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1010.INC".
      *
           COPY    "CPSK1001.INC".
      *
      *----(01.00.02) LINE ADD START ----------------------------------
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *----(01.00.02) LINE ADD END   ----------------------------------
      *----(01.00.03) LINE ADD START ----------------------------------
      *    老人一般コード変換
       01  SRYCDCHG-REC.
           COPY    "CPSRYCDCHG.INC".
      *----(01.00.03) LINE ADD END   ----------------------------------
      *
      *    チェックマスタ−
       01  CHK-REC.
           COPY    "CPCHK.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *H24.4
      *    最低薬価マスタ
       01  GENERICPRICE-REC.
           COPY    "CPGENERICPRICE.INC".
      *
      *    患者禁忌薬剤マスタ
       01  PTKINKI-REC.
           COPY    "CPPTKINKI.INC".
      *
      *    算定履歴
       01  HZN-SANTEI-REC.
           COPY    "CPSANTEI.INC"    REPLACING
                                     //SANTEI-// BY //HZN-SANTEI-//.
      *    算定履歴（発生日用）
       01  HZN2-SANTEI-REC.
           COPY    "CPSANTEI.INC"    REPLACING
                                     //SANTEI-// BY //HZN2-SANTEI-//.
      *    算定履歴
       01  TBLW-SANTEI-AREA.
         02  TBLW-SANTEI-REC          OCCURS   100.
           COPY    "CPSANTEI.INC"    REPLACING
                                     //SANTEI-// BY //TBLW-SANTEI-//.
       01  IDX-TBL                   PIC 9(04).
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
      *    画面表示編集パラメタ
           COPY    "CPORCSC94.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *    内分泌負荷試験更新パラメタ
           COPY    "CPORCSSANFUKA.INC".
      *    表示名称編集パラメタ領域
           COPY    "CPORCSMEIHEN.INC".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    入力値編集用のパラメタ
           COPY    "CPORCSC97.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      *
           COPY    "ORCA-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC92.INC".
      *
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           ORCSC92AREA
           SPA-SCR-REC
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-INPUT-SET-AREA
      *
           IF      LNK-SC92-PG         =   "K02"
                                       OR  "KT01"
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           ELSE
               INITIALIZE                      SPA-K01KYOUTU
           END-IF
      *
           MOVE    SPACE               TO  LNK-SC92-ERRAREA
           MOVE    LNK-SC92-GMN-IDX    TO  IDX-T
      *
           IF     (LNK-SC92-INFLG      =   "1" )  AND
                  (LNK-SC92-SRYCD(1:1) =   "S" )
      *        入力値編集
               PERFORM 1001-SETINPUT-HEN-SEC
               MOVE    1               TO  FLG-ERR
           END-IF
      *    約束の時、処理なし
           IF     (LNK-SC92-SRYCD(1:1) =   "S"
                                       OR  "P" )
               MOVE    1               TO  FLG-ERR
           END-IF
      *    入院回数処理なし
           IF      LNK-SC92-SRYCD(1:1) =   "*"   OR  SPACE
               MOVE    1               TO  FLG-ERR
           END-IF
      *    ２００行以上エラー
           IF      LNK-SC92-GMN-IDX    >   SPA-MAX-LINE
               MOVE    "0007"          TO  SPA-ERRCD
               MOVE    1               TO  FLG-ERR
           END-IF
      *
      *    ’８９’のレセ電用コメント入力エラー
           IF      LNK-SC92-SRYCD(1:1) =   "8"
               IF      LNK-SC92-SRYCD(1:2) =   "81"
                                           OR  "82"
                                           OR  "83"
                                           OR  "84"
                   CONTINUE
               ELSE
                   MOVE    "0890"          TO  SPA-ERRCD
                   MOVE    1               TO  FLG-ERR
               END-IF
           END-IF
      *
      *    主処理
           IF     (FLG-ERR             =   ZERO )  AND
                  (SPA-ERRCD           =   SPACE )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    セットの入力値編集処理
      *****************************************************************
       1001-SETINPUT-HEN-SEC      SECTION.
      *
           MOVE    LNK-SC92-INPUT-G    TO  LNK-SC97-INPUT-G
      *    数量５文字以上はエラー
           IF     (LNK-SC97-SURYOFLG       >   5   )
               MOVE    "0051"          TO  SPA-ERRCD
           END-IF
      *    入力項目編集処理
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2002-KOMOKU-HENSYU-SEC
           END-IF
      *
           .
       1001-SETINPUT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC      SECTION.
      *
           EVALUATE    LNK-SC92-KBN
                WHEN   "5"
      *            数量・回数のみ変更の時
                   IF      SPA-ERRCD           =   SPACE
                       IF     (SPA-COM-INPUTCD(IDX-T)  NOT =   SPACE )
                         AND  (SPA-COM-CHKFLG (IDX-T)  NOT =   SPACE )
                           PERFORM 300-SURYOHEN-SEC
                       ELSE
                           MOVE    LNK-SC92-SRYCD  TO  SPA-COM-INPUTCD
                                                            (IDX-T)
                           PERFORM 200-KIHON-CHK-SEC
                           IF      SPA-ERRCD           =   SPACE
      *                        基本編集・併用チェック
                               PERFORM 300-KIHON-HEN-SEC
                           END-IF
                       END-IF
                   END-IF
               WHEN    "1"
      *            行挿入
                   PERFORM 400-GYOINSN-SEC
                   IF      SPA-ERRCD           =   SPACE
      *                基本チェック
                       PERFORM 200-KIHON-CHK-SEC
                   END-IF
                   IF      SPA-ERRCD           =   SPACE
      *                基本編集・併用チェック
                       PERFORM 300-KIHON-HEN-SEC
                   END-IF
      *!!
                   IF      SPA-ERRCD           =   SPACE
      *                器材商品名
                       IF      LNK-SC92-SRYCD(1:3) =   "058"
                           PERFORM 400-SYOHINMEI-SEC
                       END-IF
                   END-IF
      *!!
               WHEN    "3"
      *            チェックのみ
                   MOVE    LNK-SC92-SRYCD  TO  SPA-COM-INPUTCD(IDX-T)
                   PERFORM 200-KIHON-CHK-SEC
                   IF     (SPA-ERRCD           =   SPACE ) AND
                          (LNK-SC92-ERRCD3     =   SPACE ) AND
                          (LNK-SC92-SRYCD(1:1) =   "1"   )
                       PERFORM 2005-HEIYOU-CHK-SEC
                   END-IF
               WHEN    "4"
      *            併用算定チェックのみ
                   PERFORM 2005-HEIYOU-CHK-SEC
               WHEN    "6"
      *            画面遷移前の編集
                   MOVE    LNK-SC92-SRYCD  TO  SPA-COM-INPUTCD(IDX-T)
                   PERFORM 200-KIHON-CHK-SEC
                   IF      SPA-ERRCD           =   SPACE
      *                ＳＰＡ編集
                       PERFORM 300-TENSU-HENSYU-SEC
                   END-IF
               WHEN    OTHER
      *            その他
                   MOVE    LNK-SC92-SRYCD  TO  SPA-COM-INPUTCD(IDX-T)
                   PERFORM 200-KIHON-CHK-SEC
                   IF      SPA-ERRCD           =   SPACE
      *                基本編集・併用チェック
                       PERFORM 300-KIHON-HEN-SEC
                   END-IF
                   IF      SPA-ERRCD           =   SPACE
      *                器材商品名
                       IF      LNK-SC92-SRYCD(1:3) =   "058"
                           PERFORM 400-SYOHINMEI-SEC
                       END-IF
                   END-IF
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       200-KIHON-CHK-SEC      SECTION.
      *
      *    点数マスタ読込み
           MOVE    LNK-SC92-SRYCD      TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU           =   1
      *??
               IF     (LNK-SC92-PG     NOT =   "K05" ) AND
                      (LNK-SC92-SRYCD(1:1) =   "6"   )
      *            経過措置のチェック
                   PERFORM 2001-TENSU-CHK-SEC
               ELSE
                   MOVE    "0001"              TO  SPA-ERRCD
               END-IF
      *??
      ******   MOVE    "0001"              TO  SPA-ERRCD
           END-IF
      *    基本チェック１
           IF      SPA-ERRCD           =   SPACE
               PERFORM 200-INPUTCD-KIHON-SEC
           END-IF
      *    入力値チェック
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (LNK-SC92-INFLG      =   "1"    )
               PERFORM 200-INPUT-SURYO-CHK-SEC
               IF     (LNK-SC92-SRYCD(1:1) =   "6" ) AND
                      (SPA-ERRCD(1:1)      =   "A" )
                   PERFORM 3002-KOUHATU-MEISYO-SEC
                   MOVE    WRK-MEISYO      TO  SPA-GMN-MEISYO
                                                        (IDX-T)
               END-IF
           END-IF
      *    算定回数チェック
      **** IF      LNK-SC92-KBN    NOT =   "6"
           IF      LNK-SC92-KBN        =   "1"
                                       OR  "3"
                                       OR  "8"
               IF     (TNS-SANTEIRRKKBN    =   ZERO   )  OR
                      (SPA-HKNCOMBINUM     =   "9999" )
                   OR (LNK-SC92-PG     NOT =   "K02"  )
                   CONTINUE
               ELSE
                   PERFORM 2001-SANTEI-CHK-SEC
               END-IF
           END-IF
      *
      *    患者禁忌薬剤チェック
      *    IF     (LNK-SC92-SRYCD(1:1) =   "6" )  AND
      *           (LNK-SC92-PG     NOT =   "K05")
      *        IF      SPA-COM-KINKI (IDX-T)   =   ZERO
      *            PERFORM 2009-PTKINKI-CHK-SEC
      *        END-IF
      *    END-IF
      *
      *    基本チェック２
           IF      SPA-ERRCD           =   SPACE
               PERFORM 200-INPUTCD-KIHON2-SEC
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               IF      LNK-SC92-KBN    NOT =   "3"
                   MOVE    "1"                 TO  SPA-COM-CUR (IDX-T)
               END-IF
           END-IF
      *
           .
       200-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    経過措置のチェック処理（薬剤のみ）
      *****************************************************************
       2001-TENSU-CHK-SEC      SECTION.
      *
           INITIALIZE                  SRYCDCHG-REC 
           MOVE    LNK-SC92-SRYCD      TO  CHG-IPNSRYCD
      *
           MOVE    SPA-HOSPNUM         TO  CHG-HOSPNUM
           MOVE    SRYCDCHG-REC        TO  MCPDATA-REC
           MOVE    "tbl_srycdchg"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srycdchg"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-SRYCDCHG-READ-SEC
           ELSE
               INITIALIZE                  SRYCDCHG-REC
               MOVE    1               TO  FLG-SRYCDCHG
           END-IF
           MOVE    "tbl_srycdchg"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SRYCDCHG        =   ZERO
      *        点数マスタ編集
               MOVE    CHG-RJNSRYCD       TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU           =   1
                   MOVE    "0001"              TO  SPA-ERRCD
               ELSE
                   MOVE    CHG-RJNSRYCD        TO  LNK-SC92-SRYCD
               END-IF
           ELSE
               MOVE    "0001"              TO  SPA-ERRCD
           END-IF
      *
      *    画面入力値の入れ替え
           IF      SPA-ERRCD           =   SPACE
               MOVE    LNK-SC92-SRYCD      TO  SPA-COM-INPUTCD (IDX-T)
               MOVE    ZERO                TO  WRK-CD-MCNT
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX         >   22  )   OR
                                  (SPA-GMN-INPUTCD(IDX-T)(IDX:1)
                                           =   SPACE   OR  "*" )
                   MOVE    IDX                 TO  WRK-CD-MCNT
               END-PERFORM
               ADD     1                   TO  WRK-CD-MCNT
               MOVE    SPA-GMN-INPUTCD(IDX-T)(WRK-CD-MCNT:)
                                           TO  WRK-NAIYOU
               MOVE    SPACE               TO  SPA-GMN-INPUTCD (IDX-T)
               MOVE    LNK-SC92-SRYCD      TO  SPA-GMN-INPUTCD (IDX-T)
                                                         (1:9)
               MOVE    WRK-NAIYOU          TO  SPA-GMN-INPUTCD (IDX-T)
                                                         (10:)
           END-IF
           .
       2001-TENSU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本編集・併用算定チェック処理
      *****************************************************************
       300-KIHON-HEN-SEC      SECTION.
      *
      *    ＳＰＡ編集
           PERFORM 300-TENSU-HENSYU-SEC
      *
      *----(01.00.02) LINE ADD START ----------------------------------
      *    入力コードの主表示コードに編集
           IF      SPA-COM-CHKFLG (IDX-T)  =   SPACE
               PERFORM 400-INPUTCD-HEN-SEC
           END-IF
      *----(01.00.02) LINE ADD END   ----------------------------------
      *
      *    併用算定チェック（算定回数がＯＫの時）
           IF      SPA-COM-CHKFLG (IDX-T)  =   SPACE
               IF     (LNK-SC92-ERRCD3     =   SPACE ) AND
                      (LNK-SC92-SRYCD(1:1) =   "1"   )
                  AND (LNK-SC92-PG         =   "K02" )
                  AND (LNK-SC92-KBN        =   "1"
                                           OR  "3"
                                           OR  "8" )
                   PERFORM 2005-HEIYOU-CHK-SEC
               END-IF
           END-IF
      *
      *    基本チェック済フラグ
           IF     (SPA-ERRCD               =   SPACE )  AND
                  (LNK-SC92-ERRAREA        =   SPACE )
      *        画面再編集（追加の時） 
               IF     (SPA-COM-GMNFLG (IDX-T)  =   SPACE  )  AND
                      (LNK-SC92-KBN            =   "1"    )
                   PERFORM 500-GMN-HEN-SEC
               END-IF
               MOVE    "1"             TO  SPA-COM-CHKFLG (IDX-T)
           ELSE
               MOVE    SPACE           TO  SPA-COM-CHKFLG (IDX-T)
           END-IF
      *
           .
       300-KIHON-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       200-INPUTCD-KIHON-SEC      SECTION.
      *
      *    老人適用区分チェック
           EVALUATE    TNS-ROUTEKKBN
               WHEN    0
                   CONTINUE
               WHEN    1
                   IF      LNK-SC92-ROUJIN  NOT =   ZERO
                       PERFORM 2002-SRYCDCHG-HEN-SEC
                   END-IF
               WHEN    2
                   IF      LNK-SC92-ROUJIN  NOT =   1
                       PERFORM 2002-SRYCDCHG-HEN-SEC
                   END-IF
           END-EVALUATE
      *
      *    病院診療所区分チェック
           EVALUATE    TNS-HOSPSRYKBN
               WHEN    0
                   CONTINUE
               WHEN    1
                   IF      SPA-HOSPSBT     NOT =   1
                       MOVE    "0016"          TO  SPA-ERRCD
                   END-IF
               WHEN    2
                   IF      SPA-HOSPSBT     NOT =   2
                       MOVE    "0116"          TO  SPA-ERRCD
                   END-IF
           END-EVALUATE
      *
      *    入外適用区分チェック
           EVALUATE    TNS-NYUGAITEKKBN
               WHEN    0
                   CONTINUE
               WHEN    1
                   IF      SPA-NYUGAIKBN   NOT =   "1"
                       MOVE    "0014"          TO  LNK-SC92-ERRCD1
                   END-IF
               WHEN    2
                   IF      SPA-NYUGAIKBN   NOT =   "2"
                       MOVE    "0114"          TO  LNK-SC92-ERRCD1
                   END-IF
           END-EVALUATE
      *
      *    施設基準適合分チェック（コメント・用法は対象外）
           IF     (SPA-ERRCD           NOT =   SPACE  )  OR
                  (LNK-SC92-SRYCD  (1:1)   =   "8"    )  OR
                  (LNK-SC92-SRYCD  (1:3)   =   "001"  )  OR
                  (LNK-SC92-SRYCD  (1:3)   =   "008"  )  OR
                  (TNS-SSTKIJUNCD(01)      =   ZERO   )
               CONTINUE
           ELSE
               PERFORM 2002-SSTKIJUN-CHK-SEC
           END-IF
      *
      *    対象年齢チェック
           IF     (LNK-SC92-SRYCD(1:1) =   "1" )  OR
                  (LNK-SC92-SRYCD(1:1) =   "7" )
             OR   (LNK-SC92-SRYCD(1:3) =   "059" )
               MOVE    TNS-KGNAGE          TO  WRK-TNS-KGNAGE
               MOVE    TNS-JGNAGE          TO  WRK-TNS-JGNAGE
               PERFORM 2003-NENREI-CHK-SEC
           END-IF
      *
      *    病床数チェック
           IF      TNS-BYOSYOKBN   NOT =   ZERO
               PERFORM 2004-BYOSYOKBN-CHK-SEC
           END-IF
      *
      *    労災・自賠責チェック
           PERFORM 20088-RSIJIB-CHK-SEC
      *
           .
       200-INPUTCD-KIHON-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人一般コード変換処理
      *****************************************************************
       2002-SRYCDCHG-HEN-SEC             SECTION.
      *
           INITIALIZE                  SRYCDCHG-REC 
      *    コード変換読込
           EVALUATE    TNS-ROUTEKKBN
      *        一般
               WHEN    1
                   MOVE    TNS-SRYCD           TO  CHG-IPNSRYCD
                   MOVE    "key2"              TO  WRK-PATH
               WHEN    2
                   MOVE    TNS-SRYCD           TO  CHG-RJNSRYCD
                   MOVE    "key3"              TO  WRK-PATH
           END-EVALUATE
           MOVE    SPA-HOSPNUM         TO  CHG-HOSPNUM
           MOVE    SRYCDCHG-REC        TO  MCPDATA-REC
           MOVE    "tbl_srycdchg"      TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srycdchg"      TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM 990-SRYCDCHG-READ-SEC
           ELSE
               INITIALIZE                  SRYCDCHG-REC
               MOVE    1               TO  FLG-SRYCDCHG
           END-IF
      *
           INITIALIZE                  WRK-SRYCD-CHG-AREA
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL     FLG-SRYCDCHG      =   1
               ADD     1               TO  IDX
               IF      TNS-ROUTEKKBN   =   1
                   MOVE    CHG-RJNSRYCD    TO  WRK-SRYCDCHG (IDX)
               ELSE
                   MOVE    CHG-IPNSRYCD    TO  WRK-SRYCDCHG (IDX)
               END-IF
      *
               MOVE    "tbl_srycdchg"      TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM 990-SRYCDCHG-READ-SEC
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_srycdchg"      TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX                 =   ZERO
               IF      TNS-ROUTEKKBN   =   1
                   MOVE    "0115"          TO  LNK-SC92-ERRCD2
               ELSE
                   MOVE    "0015"          TO  LNK-SC92-ERRCD2
               END-IF
               GO      TO      2002-SRYCDCHG-HEN-EXT
           END-IF
      *
      *    点数マスタ編集
           MOVE    WRK-SRYCDCHG (01)   TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU           =   1
               MOVE    "0015"              TO  LNK-SC92-ERRCD2
               MOVE    "1"                 TO  SPA-COM-CUR (IDX-T)
               GO      TO      2002-SRYCDCHG-HEN-EXT
           END-IF
      *
           MOVE    WRK-SRYCDCHG (01)   TO  LNK-SC92-SRYCD
           MOVE    LNK-SC92-SRYCD      TO  SPA-COM-INPUTCD (IDX-T)
      *    画面入力値の入れ替え
           MOVE    ZERO                TO  WRK-CD-MCNT
      *    画面再表示
      *     文字列調査の開始位置　取得
           IF  (SPA-GMN-INPUTCD(IDX-T)(1:1)    IS  NUMERIC) AND
               (SPA-GMN-INPUTCD(IDX-T)(2:1)    =   SPACE  ) AND
               (SPA-GMN-INPUTCD(IDX-T)(3:1)    NOT =   SPACE)
      *        時間加算あり
               MOVE    3                   TO  WRK-IDX-FT
           ELSE
               MOVE    1                   TO  WRK-IDX-FT
           END-IF
           PERFORM VARYING     IDX     FROM    WRK-IDX-FT  BY  1
                   UNTIL      (IDX         >   22  )   OR
                              (SPA-GMN-INPUTCD(IDX-T)(IDX:1)
                                           =   SPACE   OR  "*" )
               MOVE    IDX                 TO  WRK-CD-MCNT
           END-PERFORM
           ADD     1                   TO  WRK-CD-MCNT
           MOVE    SPA-GMN-INPUTCD(IDX-T)(WRK-CD-MCNT:)
                                       TO  WRK-NAIYOU
           MOVE    LNK-SC92-SRYCD      TO  SPA-GMN-INPUTCD (IDX-T)
                                                   (WRK-IDX-FT:9)
           COMPUTE IDY                 =   WRK-IDX-FT   +  9
           MOVE    WRK-NAIYOU          TO  SPA-GMN-INPUTCD (IDX-T)
                                                   (IDY:)
      *
           .
       2002-SRYCDCHG-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力値チェック処理
      *****************************************************************
       200-INPUT-SURYO-CHK-SEC             SECTION.
      *
           MOVE    LNK-SC92-INPUT-G    TO  LNK-SC97-INPUT-G
      *
      *    数量ゼロはエラーとする
           IF      LNK-SC92-PG         =   "K02"
                                       OR  "J04"
                                       OR  "KT01"
             IF     (LNK-SC97-SURYOCNT   >   ZERO  )  AND
                    (LNK-SC97-SURYO      =   ZERO  )
               IF     (SPA-COM-INPUTCD(IDX-T)(1:1)
                                               =   "6"  OR  "7") OR
                      (SPA-COM-INPUTCD(IDX-T)(1:3)
                                               =   "059"      )
                OR    ((SPA-COM-INPUTCD(IDX-T)(1:1)
                                               =   "1"     ) AND
                       (SPA-COM-KZMCOMPSIKIBETU(IDX-T)
                                           NOT =   0        )  )
                   MOVE    "A001"              TO  SPA-ERRCD
               ELSE
                   MOVE    "A002"              TO  SPA-ERRCD
               END-IF
               MOVE    TNS-NAME            TO  SPA-GMN-MEISYO
                                                        (IDX-T)
             END-IF
           ELSE
               MOVE    TNS-NAME            TO  SPA-GMN-MEISYO
                                                        (IDX-T)
           END-IF
      *    数量５文字以上はエラー
           IF     (SPA-COM-INPUTCD(IDX-T) (1:2)
                                       NOT =   "09" )  AND
                  (LNK-SC97-SURYOFLG       >   5   )
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "0051"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2001-INPUTCD-CHECK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        入力項目編集処理
               PERFORM 2002-KOMOKU-HENSYU-SEC
           END-IF
      *
           .
       200-INPUT-SURYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療コード入力チェック処理
      *****************************************************************
       2001-INPUTCD-CHECK-SEC    SECTION.
      *
           MOVE    SPA-COM-INPUTCD (IDX-T)(1:1)
                                           TO  WRK-MOJI
      *
           IF      SPA-COM-INPUTCD(IDX-T)(1:3)   =   "059"
               MOVE    "7"                 TO  WRK-MOJI
           END-IF
           IF      SPA-COM-INPUTCD(IDX-T)(1:3)   =   "008"
               MOVE    "8"                 TO  WRK-MOJI
           END-IF
      *
           EVALUATE    WRK-MOJI
      *
      *        診療行為
               WHEN    "1"
      *            きざみ値計算識別判定がない時、数量入力エラー(29)
                   IF      TNS-KZMCOMPSIKIBETU     =   0 
                       IF      LNK-SC97-SURYOCNT   NOT =   ZERO
                           MOVE    "0002"          TO  SPA-ERRCD
                       END-IF
                   END-IF
      *        特定器材
               WHEN    "7"
      *            1行目入力不可
                   IF      IDX-T           =   1
                       MOVE    "0013"          TO  SPA-ERRCD
                   END-IF
      *        コメント
               WHEN    "8"
      *            数量をゼロとする
                   MOVE    ZERO            TO  LNK-SC97-SURYO
      *
               WHEN    OTHER
                   CONTINUE
           END-EVALUATE
      *
      *    時間加算チェック
           IF     (SPA-ERRCD           =   SPACE   )  AND
                  (LNK-SC97-JIKAN  NOT =   ZERO    )
      *        時間加算区分は手技料であること
               IF     (WRK-MOJI            =   "1"  )  AND
                      (TNS-DATAKBN         =   "1"  )
                   PERFORM 20011-JIKANGAI-CHK-SEC
               ELSE
                   MOVE    "0030"          TO  SPA-ERRCD
               END-IF
           ELSE
               IF      SPA-COM-TIMEFLG(IDX-T)
                                           NOT =   ZERO
                   MOVE    ZERO            TO  LNK-SC92-TIMEFLG
                   MOVE    ZERO            TO  SPA-COM-TIMEFLG
                                                          (IDX-T)
               END-IF
           END-IF
      *ver.4.5
      *    換算値入力等は、Ｈ２０年４月からとする
           IF     (SPA-SRYYMD      <   "20080401" )
              AND (SPA-ERRCD       =   SPACE      )
               IF     (LNK-SC97-INKBNFLG1  =   "1"  )  OR
                      (LNK-SC97-INKBNFLG2  =   "1"  )  OR
                      (LNK-SC97-CONTFLG    =   "1"  )
                   MOVE    "0206"              TO  SPA-ERRCD
               END-IF
           END-IF
      *    薬剤のみ
           IF     (SPA-ERRCD       =   SPACE      )
               IF     (LNK-SC97-INKBNFLG1  =   "1"  )  AND
                      (SPA-COM-INPUTCD (IDX-T)(1:1)
                                       NOT =   "6" )
                   MOVE    "0207"              TO  SPA-ERRCD
               END-IF
               IF     (LNK-SC97-INKBNFLG2  =   "1"  )  AND
                      (SPA-COM-INPUTCD (IDX-T)(1:1)
                                       NOT =   "6" )
                   MOVE    "0203"              TO  SPA-ERRCD
               END-IF
           END-IF
           .
       2001-INPUTCD-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    時間外区分チェック処理
      *****************************************************************
       20011-JIKANGAI-CHK-SEC      SECTION.
      *
      *    時間外区分に変更が無い時
           IF     (LNK-SC92-TIMEFLG    =   ZERO  )  OR
                  (LNK-SC92-TIMEFLG    =   LNK-SC97-JIKAN)
               MOVE    LNK-SC97-JIKAN      TO  SPA-COM-TIMEFLG
                                                          (IDX-T)
               MOVE    LNK-SC97-JIKAN      TO  LNK-SC92-TIMEFLG
           ELSE
      *    診察料の時、時間外区分の変更
               IF      LNK-SC92-TIMEFLG    =   SPA-COM-TIMEFLG
                                                          (IDX-T)
                   MOVE    LNK-SC97-JIKAN      TO  SPA-COM-TIMEFLG
                                                          (IDX-T)
               ELSE
                   MOVE    "0089"              TO  SPA-ERRCD
               END-IF
           END-IF
           .
       20011-JIKANGAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目編集処理
      *****************************************************************
       2002-KOMOKU-HENSYU-SEC      SECTION.
      *
      *    数量
           MOVE    SPACE               TO  SPA-COM-AUTOFLG2(IDX-T)
      *    数量変更の時、薬剤警告をクリア
           IF     (LNK-SC97-SURYO  NOT =   SPA-COM-SURYO(IDX-T))
               IF     (LNK-SC97-SURYO          =   ZERO   )  AND
                      (SPA-COM-SURYO(IDX-T)    =   1      )
                   CONTINUE
               ELSE
                   IF     (SPA-COM-SRYKBN (IDX-T)  =   "23" )
                   AND    (SPA-COM-SURYO2 (IDX-T)  =   LNK-SC97-SURYO)
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  SPA-COM-KZMFLG5
                                                              (IDX-T)
                       MOVE    SPACE           TO  SPA-COM-TUKIJGNFLG
                                                              (IDX-T)
                   END-IF
               END-IF
           END-IF
      *!!
      *    薬評・器評は数量ゼロとする
           IF      SPA-COM-INPUTCD(IDX-T)  =   "630010011"
                                           OR  "630010012"
                                           OR  "630010013"
               MOVE    ZERO            TO  LNK-SC97-SURYOCNT
               MOVE    ZERO            TO  LNK-SC97-SURYO
           END-IF
      *
           IF      LNK-SC97-SURYOCNT   =   ZERO
               MOVE    1               TO  SPA-COM-SURYO(IDX-T)
               MOVE    SPACE           TO  SPA-COM-SURFLG(IDX-T)
           ELSE
               MOVE    LNK-SC97-SURYO  TO  SPA-COM-SURYO(IDX-T)
               IF     (SPA-COM-SURYO(IDX-T)  =  ZERO )
               AND    (LNK-SC92-PG     NOT =   "K05" )
                   MOVE    1           TO  SPA-COM-SURYO(IDX-T)
               END-IF
               MOVE    "1"             TO  SPA-COM-SURFLG(IDX-T)
      *!!
               IF      LNK-SC97-AUTOFLG2   =   "1"
                   MOVE    "1"         TO  SPA-COM-AUTOFLG2(IDX-T)
               END-IF
      *!!
           END-IF
      *
      *ver.4.5
           MOVE    ZERO            TO  SPA-COM-KANSURYO  (IDX-T)
           MOVE    SPACE           TO  SPA-COM-TANDKKBN (IDX-T)
           MOVE    SPACE           TO  SPA-COM-INPUTNAIF (IDX-T)
           MOVE    SPACE           TO  SPA-COM-INPUTCONT (IDX-T)
      *    薬剤の換算値、１種類入力
           IF     (LNK-SC92-SRYCD (1:1)    =   "6" )
               PERFORM 20023-KANSAN-HEN-SEC
           END-IF
      *    継続コメント
           IF      LNK-SC97-CONTFLG    =   "1"
               IF     (LNK-SC92-SRYCD (1:1)    =   "8"   )  OR
                      (LNK-SC92-SRYCD (1:3)    =   "008" )
                   MOVE    "1"             TO  SPA-COM-INPUTCONT
                                                             (IDX-T)
               ELSE
                   MOVE    "0205"          TO  SPA-ERRCD
                   MOVE    "1"             TO  SPA-COM-CUR (IDX-T)
               END-IF
           END-IF
      *ver.4.5
      *
           MOVE    SPA-COM-SURYO(IDX-T)
                                       TO  SPA-COM-SURYO2(IDX-T)
      *
      *    回数（未入力時、１回）
           IF      LNK-SC97-KAISUCNT   =   ZERO
               IF     (SPA-COM-KAISU(IDX-T)  =  ZERO )  OR
                      (SPA-COM-KAISU(IDX-T)  >  1    )
                   MOVE    1           TO  SPA-COM-KAISU (IDX-T)
               END-IF
               MOVE    SPACE           TO  SPA-COM-KAIFLG(IDX-T)
           ELSE
               MOVE    "1"             TO  SPA-COM-KAIFLG(IDX-T)
               IF      LNK-SC97-KAISU  NOT =   SPA-COM-KAISU (IDX-T)
                   IF     (SPA-COM-SRYKBN (IDX-T)  =   "23" )
                   AND    (SPA-COM-KAISU2 (IDX-T)  =   LNK-SC97-KAISU)
                       CONTINUE
                   ELSE
                       MOVE    "1"         TO  SPA-COM-KAIFLG2(IDX-T)
                   END-IF
               END-IF
               MOVE    LNK-SC97-KAISU  TO  SPA-COM-KAISU (IDX-T)
               IF      SPA-COM-KAISU(IDX-T)
                                       =   ZERO
                   MOVE    1           TO  SPA-COM-KAISU (IDX-T)
               END-IF
           END-IF
           MOVE    SPA-COM-KAISU(IDX-T)
                                       TO  SPA-COM-KAISU2(IDX-T)
      *
      *    分画数（未入力時、１回）
           IF     (LNK-SC92-SRYCD (1:1)    =   "7" ) AND
                  (LNK-SC97-BUNGSU     =   ZERO   )  AND
                  (LNK-SC97-MOJI2  NOT =   SPACE  )
      *        分画数変換
               PERFORM 20021-BUNGSU-HENSYU-SEC
           ELSE
               MOVE    LNK-SC97-BUNGSU     TO  WRK-BUNGSU
           END-IF
           MOVE    WRK-BUNGSU          TO  SPA-COM-BUNGSU(IDX-T)
           IF      WRK-BUNGSU          =   ZERO
               MOVE    1               TO  SPA-COM-BUNGSU(IDX-T)
           END-IF
      *
      *    その他値
           MOVE    LNK-SC97-MOJI1      TO  SPA-COM-ATAI1 (IDX-T)
           MOVE    LNK-SC97-MOJI2      TO  SPA-COM-ATAI2 (IDX-T)
           MOVE    LNK-SC97-MOJI3      TO  SPA-COM-ATAI3 (IDX-T)
           MOVE    LNK-SC97-MOJI4      TO  SPA-COM-ATAI4 (IDX-T)
      *
           .
       2002-KOMOKU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    分画数変換処理
      *****************************************************************
       20021-BUNGSU-HENSYU-SEC           SECTION.
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
           MOVE    ZERO               TO  LNK-SC97-MCNT2
      *
           .
       20021-BUNGSU-HENSYU-EXT.
           EXIT.
      *
      *ver.4.5
      *****************************************************************
      *    薬剤の換算値、１種類入力処理
      *****************************************************************
       20023-KANSAN-HEN-SEC             SECTION.
      *
      *    換算値入力
           IF      LNK-SC97-INKBNFLG1  =   "1"
               IF      TNSP-KANZANCHI       >  ZERO
                   COMPUTE WRK-SURYO-KEI   =  LNK-SC97-SURYO
                                           *  TNSP-KANZANCHI
      *            数量の桁あふれエラー
                   IF     (WRK-SURYO-KEI1  NOT =   ZERO )
                       OR (WRK-SURYO-KEI4  NOT =   ZERO )
                       MOVE    "0202"          TO  SPA-ERRCD
                       MOVE    "1"             TO  SPA-COM-CUR
                                                         (IDX-T)
                   END-IF
                   COMPUTE SPA-COM-SURYO(IDX-T)  =  LNK-SC97-SURYO
                                                 *  TNSP-KANZANCHI
                   MOVE    LNK-SC97-SURYO      TO  SPA-COM-KANSURYO
                                                           (IDX-T)
      *           外来投薬で、１種類設定可能かの判定
                   IF      (SPA-COM-SRYKBN (IDX-T) =   "21"  )
                   AND     (SPA-NYUGAIKBN          =   "2"   )
      *                ｇ、�咫，髻（顱　■蹌譟，髻）�
                       IF    ((TNS-TANICD          =   "032"
                                                   OR  "033")  AND 
                              (TNSP-KANZANTANICD   =  "018"))
                       OR    ((TNS-TANICD          =   "036") AND
                              (TNSP-KANZANTANICD   =   "007"))
                           MOVE    "1"             TO  SPA-COM-TANDKKBN
                                                               (IDX-T)
                       END-IF
                   END-IF
               ELSE
      *            換算値設定なし
                   MOVE    "0204"          TO  SPA-ERRCD
                   MOVE    "1"             TO  SPA-COM-CUR (IDX-T)
               END-IF
           END-IF
      *    内服１種類入力
           IF      LNK-SC97-INKBNFLG2  =   "1"
      ****     IF      (SPA-COM-SRYKBN (IDX-T) =   "21"  )
               IF      (SPA-NYUGAIKBN          =   "2"   )
                   MOVE    "1"             TO  SPA-COM-INPUTNAIF (IDX-T)
                   MOVE    "1"             TO  SPA-COM-TANDKKBN(IDX-T)
               ELSE
                   MOVE    "0203"          TO  SPA-ERRCD
                   MOVE    "1"             TO  SPA-COM-CUR (IDX-T)
               END-IF
           END-IF
           .
       20023-KANSAN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理（挿入後）
      *****************************************************************
       200-INPUTCD-KIHON2-SEC             SECTION.
      *
      *    最終行判定
           MOVE    ZERO                TO  WRK-MAX
           PERFORM VARYING     IDX     FROM    SPA-MAX-LINE   BY  -1
                   UNTIL       IDX     <   1
               IF     (SPA-GMN-INPUTCD (IDX)   NOT =   SPACE ) OR
                      (SPA-COM-INPUTCD (IDX)   NOT =   SPACE )
                   MOVE    IDX             TO  WRK-MAX
                   MOVE    1               TO  IDX
               END-IF
           END-PERFORM
      *
      *    時間外加算チェック
           IF      SPA-COM-TIMEFLG(IDX-T)  NOT =   ZERO
               IF      TNS-TIMEKSNKBN          =   ZERO
                   MOVE    "0030"              TO  SPA-ERRCD
      *        診療の時、手技料ならＯＫとする（現在、時間加算区分＝０）
                   IF    ((SPA-COM-INPUTCD (IDX-T) (2:2)
                                                   =   "11"  OR
                                                       "12" )  AND
                          (TNS-DATAKBN             =   "1"  ) )  OR
      *               小児科外来診療料の時、時間外ありとする
                          (SPA-COM-INPUTCD (IDX-T)
                                                   =   "113003610"  OR
                                                       "113003510"  OR
                                                       "113003810"  OR
                                                       "113003710")  OR
      *         労災の時、診察料ならＯＫとする
                         ((SPA-HKNKBN          =   1    )  AND
                          (SPA-COM-INPUTCD(IDX-T)  =   "101110010" OR
                                                       "101120010" OR
                                                       "101120040" OR
                                                       "101120050" OR
                                                       "101120060" ))
                       MOVE     SPACE              TO  SPA-ERRCD
                   END-IF
      *            処置の時
                   IF      TNS-SRYKBN      =   "40"
                       MOVE     SPACE              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
      *    注加算コード、注加算連番チェック
           IF     (TNS-CHUKSNCD        =   ZERO  OR  SPACE )  OR
                  (TNS-CHUKSNTUBAN     =   ZERO  OR  SPACE )
               CONTINUE
           ELSE
             IF      LNK-SC92-KBN        =   "1"
      *        追加挿入の時のチェックとする
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   WRK-MAX
                   IF      IDX         NOT =   IDX-T
      *                    注加算コード、通番が同じ時エラー
                       IF      (SPA-COM-CHUKSNCD   (IDX)
                                                   =  TNS-CHUKSNCD) AND
                               (SPA-COM-CHUKSNTUBAN(IDX)
                                                   =   TNS-CHUKSNTUBAN)
                          MOVE    "0042"          TO  SPA-ERRCD
                          IF      (LNK-SC92-CHUKSN    =   "1" )  AND
                                  (SPA-COM-INPUTCD (IDX)
                                                   =   LNK-SC92-SRYCD)
                               MOVE    SPACE           TO  SPA-ERRCD
                          END-IF
      *
                       END-IF
                   END-IF
               END-PERFORM
             END-IF
           END-IF
      *
      *    同一剤内に時間加算は１つのみ
           IF     (TNS-TIMEKSNKBN      NOT =   ZERO )
      *----(01.00.01) LINE ADD START ----------------------------------
              AND (TNS-DATAKBN             =   "2"  )
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
               MOVE    ZERO                TO  WRK-TIME
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   WRK-MAX   )  OR
                                  (SPA-ERRCD       NOT =   SPACE )
                   IF      IDX             NOT =   IDX-T
                       IF     (SPA-COM-DATAKBN   (IDX)   =   "2" ) AND
                              (SPA-COM-TIMEKSNKBN(IDX) NOT =   ZERO)
                           IF      WRK-TIME        >=  1
                               MOVE    "0043"          TO  SPA-ERRCD
                           END-IF
                           ADD     1               TO  WRK-TIME
                       END-IF
                   END-IF
      *
                   IF      SPA-COM-ZAIEND (IDX)    =   "1"
                       MOVE    ZERO                TO  WRK-TIME
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       200-INPUTCD-KIHON2-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定回数チェック処理（単独）
      *****************************************************************
       2001-SANTEI-CHK-SEC             SECTION.
      *
           MOVE    TNS-TENSU-REC       TO  HZN-TNS-TENSU-REC
      *    算定履歴チェック処理
           INITIALIZE                  ORCSC91AREA
           MOVE    LNK-SC92-SYORIFLG   TO  LNK-SC91-SYORIFLG
           MOVE    LNK-SC92-SRYCD      TO  LNK-SC91-SRYCD
           MOVE    "1"                 TO  LNK-SC91-KBN
           MOVE    IDX-T               TO  LNK-SC91-GMN-IDX
           MOVE    SPA-COM-KAISU (IDX-T)
                                       TO  LNK-SC91-KAISU
      *    数量反映
           MOVE    SPA-COM-SURYO (IDX-T)
                                       TO  LNK-SC91-SURYO
           MOVE    LNK-SC92-TEISEI-AREA
                                       TO  LNK-SC91-TEISEI-AREA
           MOVE    LNK-SC92-KT01-AREA
                                       TO  LNK-SC91-KT01-AREA
      *
           CALL    "ORCSC91"           USING    MCPAREA
                                                ORCSC91AREA
                                                SPA-SCR-REC
                                                SPA-AREA
                                                TNS-TENSU-REC
           MOVE    LNK-SC91-ERRCD      TO  LNK-SC92-ERRCD3
           MOVE    LNK-SC91-ERRMSG     TO  LNK-SC92-ERRMSG
      *
           MOVE    HZN-TNS-TENSU-REC   TO  TNS-TENSU-REC
      *
           .
       2001-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    施設基準適合分チェック
      *****************************************************************
       2002-SSTKIJUN-CHK-SEC      SECTION.
      *
           INITIALIZE                  WRK-TBL-SSTKIJUN-AREA
      *    施設基準情報検索
           MOVE    SPACE               TO  SYS-1006-REC
           MOVE    "1006"              TO  SYS-1006-KANRICD
           MOVE    SPA-SRYYMD          TO  SYS-1006-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1006-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1006-HOSPNUM
           MOVE    SYS-1006-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL   (FLG-SYSKANRI   =   1    )
                   OR      (IDX            >=  3500 )
               MOVE    MCPDATA-REC         TO  SYS-1006-REC
      *H24.4
               EVALUATE    SYS-1006-KBNCD
                   WHEN    "1"
                       MOVE    0               TO  IDX
                   WHEN    "2"
                       MOVE    500             TO  IDX
                   WHEN    "7"
                       MOVE    3000            TO  IDX
                   END-EVALUATE
      *H24.4
               PERFORM VARYING IDY         FROM    1   BY  1
                       UNTIL   IDY         >       500
                   ADD     1               TO  IDX
                   MOVE    SYS-1006-SSTKIJUN  (IDY)
                                           TO  WRK-TBL-SSTKIJUN (IDX)
               END-PERFORM
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    ZERO                TO  FLG-SST
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   10  )   OR
                              (FLG-SST     =   1   )
               IF      TNS-SSTKIJUNCD (IDX)    =   ZERO
                   CONTINUE
               ELSE
                   MOVE    TNS-SSTKIJUNCD (IDX)    TO  IDY
                   IF      WRK-TBL-SSTKIJUN (IDY)  =   1
                       MOVE    1               TO  FLG-SST
                   END-IF
               END-IF
           END-PERFORM
           IF      FLG-SST             =   ZERO
               MOVE    "0024"          TO  LNK-SC92-ERRCD4
           END-IF
      *
      *平成１６年４月から
      *    手術の基準不適合逓減判定
           MOVE    ZERO                TO  FLG-SSTKIJUNCD-ERR
           IF     (SPA-SRYYMD          >=  "20040401" )  AND
                  (TNS-SRYKBN          =   "50"       )  AND
                  (TNS-KIJUNTEIGENKBN  =   "2"  OR  "3" )
               IF      FLG-SST             =   ZERO
                   MOVE    SPACE           TO  LNK-SC92-ERRCD4
                   MOVE    1               TO  FLG-SSTKIJUNCD-ERR
               END-IF
           END-IF
      *
           .
       2002-SSTKIJUN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢チェック処理
      *****************************************************************
       2003-NENREI-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-KGNAGE
           MOVE    ZERO                TO  WRK-JGNAGE
           MOVE    WRK-TNS-KGNAGE      TO  WRK-KGNAGE(2:2)
           MOVE    WRK-TNS-JGNAGE      TO  WRK-JGNAGE(2:2)
      *
      *H24.4
           IF      SPA-PTID            =   ZERO
               MOVE    ZERO                TO  WRK-NEN-03
               MOVE    ZERO                TO  WRK-NEN-15
           ELSE
               MOVE    SPA-BIRTHDAY        TO  WRK-NEN-03
               MOVE    SPA-BIRTHDAY        TO  WRK-NEN-15
               COMPUTE WRK-NEN03-YY        =   WRK-NEN03-YY  +  3
               COMPUTE WRK-NEN15-YY        =   WRK-NEN15-YY  +  15
           END-IF
      *
      *    下限年齢チェック
           IF      WRK-TNS-KGNAGE          =   SPACE  OR  ZERO
               CONTINUE
           ELSE
               IF      WRK-TNS-KGNAGE          =   "AA"
      *            新生児判定（２８日まで新生児）
                   IF     (LNK-SC92-NENREI-YY   =   ZERO )  AND
                          (LNK-SC92-NENREI-MM   =   ZERO )  AND
                          (LNK-SC92-NENREI-DD   <=  28   )
                       MOVE    "0032"              TO  LNK-SC92-ERRCD5
                   END-IF
               ELSE
                   IF      LNK-SC92-NENREI-YY   >=  WRK-KGNAGE
                      CONTINUE
                   ELSE
                       MOVE    "0032"              TO  LNK-SC92-ERRCD5
                   END-IF
               END-IF
           END-IF
      *
      *    上限年齢チェック
           IF      WRK-TNS-JGNAGE          =   SPACE  OR  ZERO
               CONTINUE
           ELSE
      *        IF      WRK-TNS-JGNAGE          =   "AA"
      *            新生児判定
      *            IF     (LNK-SC92-NENREI-YY    =   ZERO )  AND
      *                   (LNK-SC92-NENREI-MM    =   ZERO )  AND
      *                   (LNK-SC92-NENREI-DD    <=  28   )
      *                CONTINUE
      *            ELSE
      *                MOVE    "0032"              TO  LNK-SC92-ERRCD5
      *            END-IF
      *        ELSE
      *            IF      LNK-SC92-NENREI-YY    <   WRK-JGNAGE
      *                CONTINUE
      *            ELSE
      *                MOVE    "0032"              TO  LNK-SC92-ERRCD5
      *            END-IF
      *            １５歳未満で、１５歳到達月の判定
      *            IF      (WRK-JGNAGE             =   15  )  AND
      *                    (LNK-SC92-NENREI-YY     =   15  )  AND
      *                    (LNK-SC92-15MIMAN-ARI   =   1   )
      *                初診・再診で手技料の時、エラーとしない
      *                IF     (TNS-DATAKBN             =   1  )  AND
      *                       (TNS-SRYKBN              =   11  OR  12 )
      *                    MOVE    SPACE           TO  LNK-SC92-ERRCD5
      *                END-IF
      *            END-IF
      *        END-IF
      *
      *H24.4
               EVALUATE    WRK-TNS-JGNAGE
               WHEN    "AA"
      *            新生児判定（２８日まで新生児）
                   IF     (LNK-SC92-NENREI-YY   =   ZERO )  AND
                          (LNK-SC92-NENREI-MM   =   ZERO )  AND
                          (LNK-SC92-NENREI-DD   <=  28   )
                       CONTINUE
                   ELSE
                       MOVE    "0032"              TO  LNK-SC92-ERRCD5
                   END-IF
               WHEN    "B3"
      *            ３歳までに受診があった時にのみ可能（H24.4改正）
                   IF     (LNK-SC92-NENREI-YY      >=  3  )
                       MOVE    "0032"              TO  LNK-SC92-ERRCD5
                       IF     (LNK-SC92-NENREI-YY  =   3  )
                          AND (SPA-SRYYMD(1:6)     =   WRK-NEN03-YM )
                          AND (LNK-SC92-15MIMAN-ARI    =   2  )
      *                    同じコードの算定チェック
                           PERFORM 20032-NENREI-RRKCHK-SEC
                       END-IF
                   END-IF
               WHEN    "BF"
      *           １５歳までに受診があった時にのみ可能（H24.4改正）
                   IF     (LNK-SC92-NENREI-YY   >=    15    )
                       MOVE    "0032"              TO  LNK-SC92-ERRCD5
                       IF     (LNK-SC92-NENREI-YY  =   15  )
                          AND (SPA-SRYYMD(1:6)     =   WRK-NEN15-YM )
                          AND (LNK-SC92-15MIMAN-ARI    =   1  )
      *                    同じコードの算定チェック
                           PERFORM 20032-NENREI-RRKCHK-SEC
                       END-IF
                   END-IF
               WHEN    OTHER
      *            年齢判定
                   IF      LNK-SC92-NENREI-YY    <   WRK-JGNAGE
                       CONTINUE
                   ELSE
                       MOVE    "0032"              TO  LNK-SC92-ERRCD5
                   END-IF
      *            １５歳未満で、１５歳到達月の判定
                   IF      (WRK-JGNAGE             =   15  )  AND
                           (LNK-SC92-NENREI-YY     =   15  )  AND
                           (LNK-SC92-15MIMAN-ARI   =   1   )
      *                初診・再診で手技料の時、エラーとしない
                       IF     (TNS-DATAKBN             =   1  )  AND
                              (TNS-SRYKBN              =   11  OR  12 )
                           MOVE    SPACE           TO  LNK-SC92-ERRCD5
                       END-IF
                   END-IF
               END-EVALUATE
      *H24.4
           END-IF
      *
           .
       2003-NENREI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    同じコードの算定チェック
      *****************************************************************
       20032-NENREI-RRKCHK-SEC      SECTION.
      *
      *    小児科外来診察料は別チェックとする
           IF     (TNS-CDKBN-KBN       =   "B" )
              AND (TNS-CDKBN-KBNNUM    =   001 )
              AND (TNS-CDKBN-KBNNUM-EDA    =   2 )
               GO      TO      20032-NENREI-RRKCHK-EXT
           END-IF
      *
           MOVE    SPACE               TO  LNK-SC92-ERRCD5
           .
       20032-NENREI-RRKCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病床数チェック
      *****************************************************************
       2004-BYOSYOKBN-CHK-SEC      SECTION.
      *
      *    病床数チェック（ＳＰＡの病床数）
           EVALUATE    TNS-BYOSYOKBN
               WHEN    1
                   IF      SPA-BEDSU           >=  1  AND  <=  99
                       CONTINUE
                   ELSE
                       MOVE    "0075"          TO  LNK-SC92-ERRCD7
                   END-IF
               WHEN    2
                   IF      SPA-BEDSU           >=  100 AND  <=  199
                       CONTINUE
                   ELSE
                       MOVE    "0075"          TO  LNK-SC92-ERRCD7
                   END-IF
               WHEN    3
                   IF      SPA-BEDSU           <   200
                       CONTINUE
                   ELSE
                       MOVE    "0075"          TO  LNK-SC92-ERRCD7
                   END-IF
               WHEN    4
                   IF      SPA-BEDSU           >=  200
                       CONTINUE
                   ELSE
                       MOVE    "0075"          TO  LNK-SC92-ERRCD7
                   END-IF
      *        一般病床数判定
               WHEN    5
                   IF      SPA-BEDSUIPN        >=  0  AND  <=  199
                       CONTINUE
                   ELSE
                       MOVE    "0075"          TO  LNK-SC92-ERRCD7
                   END-IF
               WHEN    6
                   IF      SPA-BEDSUIPN        >=  200
                       CONTINUE
                   ELSE
                       MOVE    "0075"          TO  LNK-SC92-ERRCD7
                   END-IF
           END-EVALUATE
      *
           .
       2004-BYOSYOKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    併用算定チェック処理
      *****************************************************************
       2005-HEIYOU-CHK-SEC           SECTION.
      *
      *   包括まとめ入力は以下対象外
          IF      SPA-HKNCOMBINUM     =   "9999"
               GO      TO      2005-HEIYOU-CHK-EXT
           END-IF
           IF      SPA-COM-HKNCOMBI(IDX-T)  =   "9999"
               GO      TO      2005-HEIYOU-CHK-EXT
           END-IF
      *    併用算定済み
           IF      SPA-COM-HCHKFLG (IDX-T) =   "1"
               GO      TO      2005-HEIYOU-CHK-EXT
           END-IF
      *
      *    チェックテーブル検索
           INITIALIZE                  CHK-REC
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
           MOVE    LNK-SC92-SRYCD      TO  CHK-SRYCD
           MOVE    "2"                 TO  CHK-CDKBN
           MOVE    SPA-SRYYMD          TO  CHK-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  CHK-YUKOEDYMD
      *
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_chk"           TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 950-CHK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-CHKMST
           END-IF
           MOVE    SPACE               TO  WRK-MANSEI-ERR
           MOVE    SPACE               TO  WRK-KEI-SRYCD
           MOVE    SPACE               TO  WRK-ERR-SRYCD
           MOVE    SPACE               TO  WRK-KEI-YMD
           MOVE    SPACE               TO  WRK-ERR-YMD
           MOVE    SPACE               TO  WRK-ERR-YMD
           MOVE    ZERO                TO  FLG-CHKERR
           MOVE    ZERO                TO  FLG-MANSEI-ARI
           MOVE    ZERO                TO  FLG-MANSEI-CHK
           MOVE    SPACE               TO  WRK-MANSEI-YMD
           MOVE    ZERO                TO  FLG-KONKAI
           MOVE    ZERO                TO  FLG-DAYCHK
           MOVE    ZERO                TO  FLG-SANTEI-TBL
           MOVE    ZERO                TO  FLG-DAYERR
           MOVE    ZERO                TO  WRK-DAYERR
           PERFORM
                   UNTIL     (FLG-CHKMST       =   1    ) 
                         OR  (FLG-CHKERR       =   1 )
               EVALUATE    CHK-CHKKBN
                   WHEN    "5"
      *            同月内チェック
      *                同一画面でのチェック
                       PERFORM 20052-CHK-INPUT-SEC
                       IF      FLG-CHKERR          =   ZERO
      *                    算定履歴とのチェック
                           PERFORM 20051-CHK-SANTEI-SEC
                       END-IF
                   WHEN    "A"
      *            同日内チェック
                       IF      SPA-NYUGAIKBN       =   "2"
      *                    同一画面でのチェック
                           PERFORM 20052-CHK-INPUT-SEC
                           IF      FLG-CHKERR          =   ZERO
      *                        算定履歴とのチェック
                               PERFORM 20053-DAYCHK-SANTEI-SEC
                           END-IF
                       END-IF
      *                同日内チェックあり
                       MOVE    1               TO  SPA-COM-DAYCHK-FLG
                                                              (IDX-T)
      *                入院
                       IF    ((SPA-NYUGAIKBN       =   "1" )
                           OR (LNK-SC92-PG         =   "KT01"))
                          AND (LNK-SC92-KBN        =   "4" )
      *                    同一画面でのチェック
                           PERFORM 200521-CHKNYU-INPUT-SEC
                           IF      FLG-CHKERR          =   ZERO
      *                        算定履歴とのチェック
                               PERFORM 200531-DAYCHKNYU-SANTEI-SEC
                           END-IF
                           MOVE    2            TO  SPA-COM-DAYCHK-FLG
                                                              (IDX-T)
                       END-IF
                   WHEN    "B"
      *            同会計チェック
                       IF      SPA-NYUGAIKBN       =   "2"
      *                    同一画面でのチェック
                           PERFORM 20052-CHK-INPUT-SEC
                       END-IF
               END-EVALUATE
               MOVE    "tbl_chk"           TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 950-CHK-READ-SEC
           END-PERFORM
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *****リハビリテーション管理料チェック
      *    IF    ((LNK-SC92-ERRCD6 NOT =   SPACE      )  AND
      *           (SPA-SRYYMD          >=  "20070401" )
      *        今回入力時エラーはそのまま
      *        IF      FLG-KONKAI          =   ZERO
      *            PERFORM 20054-RIHA-CHKSYORI-SEC
      *        END-IF
      **** END-IF
      *
           IF      FLG-CHKERR          =   1
      *        エラーメッセージ
               PERFORM 20053-CHKERRMSG-SEC
           ELSE
               IF      WRK-KEI-SRYCD   NOT =   SPACE
      *        警告メッセージ
                   IF      LNK-SC92-KEIFLG     =   "1"
                       PERFORM 20054-CHKKEIMSG-SEC
                   ELSE
                       MOVE    "1"             TO  LNK-SC92-KEIARI
                   END-IF
                   MOVE    "1"             TO  SPA-COM-HKEIFLG (IDX-T)
               END-IF
           END-IF
      *
           IF     (LNK-SC92-ERRCD6     =   SPACE )
               MOVE    "1"             TO  SPA-COM-HCHKFLG (IDX-T)
           ELSE
               MOVE    SPACE           TO  SPA-COM-HCHKFLG (IDX-T)
               MOVE    "1"             TO  SPA-COM-CUR (IDX-T)
           END-IF
      *
           .
       2005-HEIYOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       20053-CHKERRMSG-SEC            SECTION.
      *
           MOVE    "0033"              TO  LNK-SC92-ERRCD6
           MOVE    WRK-ERR-SRYCD       TO  TNS-SRYCD
           PERFORM 911-TENSU-READ22-SEC
      *
           PERFORM 200531-ERRMSG-HEN-SEC
           .
       20053-CHKERRMSG-EXT.
           EXIT.
      *****************************************************************
      *    メッセージ編集処理
      *****************************************************************
       200531-ERRMSG-HEN-SEC            SECTION.
      *
           MOVE    SPACE               TO  LNK-SC92-ERRMSG
           IF      FLG-KONKAI          =   ZERO
      *         当日
               IF      FLG-DAYCHK          =   1
                   STRING  "当日　"            DELIMITED  BY  SIZE
                           TNS-NAME            DELIMITED  BY  SPACE
                           INTO                 LNK-SC92-ERRMSG
                   END-STRING
               ELSE
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY2-AREA
                   MOVE    "21"                TO  LNK-DAY2-IRAI
                   MOVE    WRK-ERR-YMD         TO  LNK-DAY2-YMD
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY2-AREA
                   STRING  LNK-DAY2-EDTYMD3    DELIMITED  BY  SIZE
                           "　"                DELIMITED  BY  SIZE
                           TNS-NAME            DELIMITED  BY  SPACE
                          INTO                 LNK-SC92-ERRMSG
                   END-STRING
               END-IF
           ELSE
               STRING  "今回　"            DELIMITED  BY  SIZE
                       TNS-NAME            DELIMITED  BY  SPACE
                       INTO                 LNK-SC92-ERRMSG
               END-STRING
      *        入院の日指定
               IF      WRK-DAYERR      NOT =   ZERO
                   MOVE    SPACE               TO  LNK-SC92-ERRMSG
                   STRING  "今回　"            DELIMITED  BY  SIZE
                           WRK-DAYERR          DELIMITED  BY  SIZE
                           "日指定　"          DELIMITED  BY  SIZE
                           TNS-NAME            DELIMITED  BY  SPACE
                           INTO                LNK-SC92-ERRMSG
                   END-STRING
               END-IF
           END-IF
           .
       200531-ERRMSG-HEN-EXT.
           EXIT.
      *****************************************************************
      *    警告メッセージ編集処理
      *****************************************************************
       20054-CHKKEIMSG-SEC            SECTION.
      *
           MOVE    "K033"              TO  LNK-SC92-ERRCD6
           MOVE    WRK-KEI-SRYCD       TO  TNS-SRYCD
           PERFORM 911-TENSU-READ22-SEC
      *
           PERFORM 200531-ERRMSG-HEN-SEC
           .
       20054-CHKKEIMSG-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴の同月併算定チェック
      *****************************************************************
       20051-CHK-SANTEI-SEC            SECTION.
      *
      *    算定履歴テーブル保存
           IF      FLG-SANTEI-TBL      =   ZERO
               PERFORM 200512-SANTEI-SELECT-SEC
               MOVE    1               TO  FLG-SANTEI-TBL
           END-IF
      *    保険毎のコードの判定
           PERFORM 1000-SANTEI-CHK-SEC
      *
           INITIALIZE                  SANTEI-REC
           MOVE    ZERO                TO  FLG-SANTEI-OK
           PERFORM VARYING     IDX2    FROM    1   BY   1
                   UNTIL      (IDX2    >   IDX-TBL )  OR
                              (FLG-SANTEI-OK   =   1  )
               IF      CHK-CD              =   TBLW-SANTEI-SRYCD(IDX2)
      *            対象の算定履歴判定
                   MOVE    TBLW-SANTEI-REC(IDX2)   TO  SANTEI-REC
                   PERFORM 200511-SANTEI-CHK-SEC
               END-IF
           END-PERFORM
           IF      FLG-SANTEI-OK       =   ZERO
               INITIALIZE                  SANTEI-REC
           END-IF
      *
      *    算定歴の回数をチェックする
           MOVE    SANTEI-MSANTEICNT   TO  WRK-SANTEI-CNT
      *
      *    訂正の時、訂正分はカウントしない
           IF      LNK-SC92-SYORIFLG   =   1
               MOVE    ZERO                TO  WRK-TEI-CNT
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2    >   SPA-MAX-LINE )  OR
                                  (LNK-SC92-TEI-SRYCD(IDX2)
                                               =   SPACE )
                   IF      SANTEI-SRYCD    =   LNK-SC92-TEI-SRYCD(IDX2)
                       ADD     LNK-SC92-TEI-KAISU(IDX2)
                                               TO  WRK-TEI-CNT
                   END-IF
               END-PERFORM
               MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
               IF     (SANTEI-SRYYM        =   SPA-SRYYMD(1:6)) AND
                      (SANTEI-DAY (IDX-Y2) >   ZERO )  AND
                      (WRK-SANTEI-CNT      >   ZERO )
                   COMPUTE WRK-SANTEI-CNT  =   WRK-SANTEI-CNT
                                           -   WRK-TEI-CNT
                   IF      SANTEI-DAY (IDX-Y2) >=  WRK-TEI-CNT
                       COMPUTE SANTEI-DAY (IDX-Y2)
                                           =   SANTEI-DAY (IDX-Y2)
                                           -   WRK-TEI-CNT
                   ELSE
                       MOVE    ZERO            TO  SANTEI-DAY (IDX-Y2)
                   END-IF
               END-IF
           END-IF
      *    入院・外来チェック
           IF     (WRK-SANTEI-CNT      >   ZERO    )
               IF      CHK-NYUGAIBNDKBN    =   "1"
                   PERFORM 20055-NYUGAIBND-CHK-SEC
               END-IF
           END-IF
      *
      *    慢性疼痛疾患管理料と消炎鎮痛等処置
           IF     (WRK-SANTEI-CNT      >   ZERO    )  AND
                  (CHK-SYORIKBN        =   "1"     )
               PERFORM 20055-MANSEI-CHK-SEC
           END-IF
      *
      *    初診料とのチェック処理
           IF     (WRK-SANTEI-CNT      >   ZERO    )  AND
                  (CHK-SYORIKBN        =   "2"     )
               PERFORM 20056-SYOSIN-CHK-SEC
           END-IF
      *
      *    算定歴の回数をチェックする
           IF      WRK-SANTEI-CNT      >   ZERO
               IF      CHK-CHKERR          =   ZERO
                   MOVE    SANTEI-SRYYM        TO  WRK-ERR-YMD(1:6)
                   MOVE    SANTEI-MSANTEID     TO  WRK-ERR-YMD(7:2)
                   MOVE    CHK-CD              TO  WRK-ERR-SRYCD
                   MOVE    1                   TO  FLG-CHKERR
               ELSE
      *            警告
                   IF      SPA-COM-HKEIFLG (IDX-T) =   SPACE
                       IF      WRK-KEI-SRYCD       =   SPACE
                           MOVE    CHK-CD          TO  WRK-KEI-SRYCD
                           MOVE    SANTEI-SRYYM    TO  WRK-ERR-YMD(1:6)
                           MOVE    SANTEI-MSANTEID TO  WRK-ERR-YMD(7:2)
                      END-IF
                  END-IF
              END-IF
           END-IF
      *    リハビリテーション管理料チェック
           IF    ((FLG-CHKERR          =   1          )
              OR  (WRK-KEI-SRYCD   NOT =   SPACE      ))  AND
                  (SPA-SRYYMD          >=  "20070401" )
               PERFORM 20054-RIHA-CHKSYORI-SEC
           END-IF
           .
       20051-CHK-SANTEI-EXT.
           EXIT.
      *****************************************************************
      *    入力値とのチェック処理
      *****************************************************************
       20052-CHK-INPUT-SEC                SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE          )  OR
                             ((SPA-GMN-INPUTCD (IDX)   =   SPACE ) AND
                              (SPA-COM-INPUTCD (IDX)   =   SPACE )) OR
                              (FLG-KONKAI          =   1     )
               IF     (IDX                 =   IDX-T   )
                 OR   (SPA-COM-HKNCOMBI (IDX)  =   "9999" )
                   CONTINUE
               ELSE
                   IF      SPA-COM-INPUTCD(IDX)    =   CHK-CD
                       IF      CHK-CHKERR          =   ZERO
                           MOVE    CHK-CD              TO  WRK-ERR-SRYCD
                           MOVE    1                   TO  FLG-CHKERR
                           MOVE    1                   TO  FLG-KONKAI
                       ELSE
      *                    警告
                           IF     (SPA-COM-HKEIFLG (IDX-T) =   SPACE)
                              AND (WRK-KEI-SRYCD       =   SPACE    )
                               MOVE    CHK-CD          TO  WRK-KEI-SRYCD
                               MOVE    1               TO  FLG-KONKAI
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .
       20052-CHK-INPUT-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴の同日併算定チェック
      *****************************************************************
       20053-DAYCHK-SANTEI-SEC            SECTION.
      *
      *    算定履歴テーブル保存
           IF      FLG-SANTEI-TBL      =   ZERO
               PERFORM 200512-SANTEI-SELECT-SEC
               MOVE    1               TO  FLG-SANTEI-TBL
           END-IF
      *    保険毎のコードの判定
           PERFORM 1000-SANTEI-CHK-SEC
      *
           INITIALIZE                  SANTEI-REC
           MOVE    ZERO                TO  FLG-SANTEI-OK
           PERFORM VARYING     IDX2    FROM    1   BY   1
                   UNTIL      (IDX2    >   IDX-TBL )  OR
                              (FLG-SANTEI-OK   =   1  )
               IF      CHK-CD              =   TBLW-SANTEI-SRYCD(IDX2)
      *            対象の算定履歴判定
                   MOVE    TBLW-SANTEI-REC(IDX2)   TO  SANTEI-REC
                   PERFORM 200511-SANTEI-CHK-SEC
               END-IF
           END-PERFORM
           IF      FLG-SANTEI-OK       =   ZERO
               INITIALIZE                  SANTEI-REC
           END-IF
      *
      *    訂正の時、訂正分はカウントしない
           MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
           IF      LNK-SC92-SYORIFLG   =   1
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2    >   SPA-MAX-LINE )  OR
                                  (LNK-SC92-TEI-SRYCD(IDX2)
                                               =   SPACE )
                   IF      SANTEI-SRYCD    =   LNK-SC92-TEI-SRYCD(IDX2)
                       ADD     LNK-SC92-TEI-KAISU(IDX2)
                                               TO  WRK-TEI-CNT
                   END-IF
               END-PERFORM
               IF      SANTEI-DAY (IDX-Y2) >=  WRK-TEI-CNT
                   COMPUTE SANTEI-DAY (IDX-Y2) =   SANTEI-DAY (IDX-Y2)
                                               -   WRK-TEI-CNT
               ELSE
                    MOVE    ZERO           TO  SANTEI-DAY (IDX-Y2)
               END-IF
           END-IF
      *    当日に算定があればエラー
           IF      SANTEI-DAY (IDX-Y2) >   ZERO
               IF      CHK-CHKERR          =   ZERO
                   MOVE    CHK-CD              TO  WRK-ERR-SRYCD
                   MOVE    1                   TO  FLG-CHKERR
                   MOVE    1                   TO  FLG-DAYCHK
               ELSE
      *            警告
                   IF      SPA-COM-HKEIFLG (IDX-T) =   SPACE
                       IF      WRK-KEI-SRYCD       =   SPACE
                           MOVE    CHK-CD          TO  WRK-KEI-SRYCD
                           MOVE    1               TO  FLG-DAYCHK
                      END-IF
                  END-IF
               END-IF
           END-IF
      *
           .
       20053-DAYCHK-SANTEI-EXT.
           EXIT.
      *****************************************************************
      *    入院・外来チェック処理
      *****************************************************************
       20055-NYUGAIBND-CHK-SEC            SECTION.
      *
           IF      SPA-NYUGAIKBN       =   "1"
      *        入院は、外来分は算定対象外
               PERFORM VARYING     IDX-Y3  FROM    1   BY   1
                       UNTIL       IDX-Y3  >   31
      *            入院期間チェック
                   IF      SPA-K02-NYUDAY(IDX-Y3)  =   ZERO
                       COMPUTE WRK-SANTEI-CNT  =   WRK-SANTEI-CNT
                                               -   SANTEI-DAY (IDX-Y3)
                       MOVE    ZERO            TO  SANTEI-DAY (IDX-Y3)
                   END-IF
               END-PERFORM
           ELSE
      *        外来は、入院分は対象外
               PERFORM VARYING     IDX-Y3  FROM    1   BY   1
                       UNTIL       IDX-Y3  >   31
      *            入院期間チェック
                   IF      SPA-K02-NYUDAY(IDX-Y3)  =   1
                       COMPUTE WRK-SANTEI-CNT  =   WRK-SANTEI-CNT
                                               -   SANTEI-DAY (IDX-Y3)
                       MOVE    ZERO            TO  SANTEI-DAY (IDX-Y3)
                   END-IF
               END-PERFORM
           END-IF
      *
      *    初回算定日の変更
           IF      WRK-SANTEI-CNT      >   ZERO
               PERFORM VARYING     IDX-Y3  FROM    1   BY   1
                       UNTIL       IDX-Y3  >   31
                   IF      SANTEI-DAY (IDX-Y3)     >   ZERO
                       MOVE    IDX-Y3          TO  SANTEI-MSANTEID
                       MOVE    31              TO  IDX-Y3
                   END-IF
               END-PERFORM
           END-IF
           .
       20055-NYUGAIBND-CHK-EXT.
           EXIT.
      *****************************************************************
      *     慢性疼痛疾患管理料と消炎鎮痛等処置のチェック処理
      *****************************************************************
       20055-MANSEI-CHK-SEC        SECTION.
      *
           MOVE    SANTEI-REC          TO  HZN2-SANTEI-REC
      *    直帰の初診算定日を決定する
           PERFORM 200531-SYOSIN-SANTEI-SEC
      *
      *    直帰の慢性疼痛疾患管理料算定日
           MOVE    "113006510"         TO  WRK-SRYCD
           IF      FLG-MANSEI-CHK      =   ZERO
      *        直帰の初診算定日を決定する
               PERFORM 200531-SYOSIN-SANTEI-SEC
               MOVE    1                   TO  FLG-MANSEI-CHK
               PERFORM 2005332-SANTEI-CHK-SEC
               MOVE    WRK-YMD             TO  WRK-MANSEI-YMD
      *        訂正の時、当日ならＯＫとする
               IF      LNK-SC92-SYORIFLG   =   1
                   IF     (WRK-YMD             =   SPA-SRYYMD  )
                       MOVE    SPACE           TO  WRK-MANSEI-YMD
                   END-IF
               END-IF
      *        初診算定日が前回の慢性疼痛より後の時、初回とする
               IF     (WRK-MANSEI-YMD      =   SPACE   )  OR
                      (WRK-SYOSINYMD1      >   WRK-MANSEI-YMD)
                   MOVE    ZERO            TO  FLG-MANSEI-ARI
               ELSE
                   MOVE    1               TO  FLG-MANSEI-ARI
               END-IF
           END-IF
      *
      *    慢性疼痛入力
           IF      LNK-SC92-SRYCD      =   WRK-SRYCD
      *        初回慢性疼痛がない時ＯＫ
               IF      FLG-MANSEI-ARI      =   ZERO
                   MOVE    ZERO            TO  WRK-SANTEI-CNT
               END-IF
           ELSE
               IF      FLG-MANSEI-ARI      =   ZERO
                   MOVE    ZERO            TO  WRK-SANTEI-CNT
               END-IF
           END-IF
           MOVE    HZN2-SANTEI-REC     TO  SANTEI-REC
           .
       20055-MANSEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    当月算定済み
      *****************************************************************
       200512-SANTEI-SELECT-SEC            SECTION.
      *
      *    外来まとめはＳＰＡに編集
           IF      LNK-SC92-KT01FLG    =   "1"
               PERFORM 2005121-KT01-SANTEI-SEC
               GO      TO   200512-SANTEI-SELECT-EXT
           END-IF
      *
           INITIALIZE                  TBLW-SANTEI-AREA
           MOVE    ZERO                TO  IDX-TBL
      *    算定履歴検索
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-SRYYMD (1:6)    TO  SANTEI-SRYYM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           PERFORM
               UNTIL      (FLG-SANTEI      =   1  )  OR
                          (IDX-TBL         >=  100)
               IF      SANTEI-MSANTEICNT   >   ZERO
                   ADD     1                   TO  IDX-TBL
                   MOVE    SANTEI-REC          TO  TBLW-SANTEI-REC
                                                         (IDX-TBL)
               END-IF
      *
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       200512-SANTEI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来まとめ処理
      *****************************************************************
       2005121-KT01-SANTEI-SEC      SECTION.
      *
           INITIALIZE                  TBLW-SANTEI-AREA
           MOVE    ZERO                TO  IDX-TBL
           PERFORM VARYING     IDX-SN      FROM    1   BY  1
                   UNTIL      (IDX-SN      >   100 )   OR
                              (LNK-SC92-KT01-SRYCD  (IDX-SN) =   SPACE)
               INITIALIZE                  SANTEI-REC
               MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
               MOVE    SPA-PTID            TO  SANTEI-PTID
               MOVE    SPA-SRYYMD (1:6)    TO  SANTEI-SRYYM
               MOVE    LNK-SC92-KT01-SRYCD (IDX-SN)
                                           TO  SANTEI-SRYCD
               MOVE    ZERO                TO  SANTEI-SRYKA
               MOVE    ZERO                TO  SANTEI-NYUGAIKBN
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
      *        MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
               MOVE    31                  TO  IDX-Y2
               PERFORM VARYING     IDD         FROM    1   BY  1
                       UNTIL      (IDD         >   IDX-Y2 )
                   MOVE    LNK-SC92-KT01-DAY (IDX-SN IDD)
                                           TO  SANTEI-DAY (IDD)
                   ADD     SANTEI-DAY (IDD)    TO  SANTEI-MSANTEICNT
                   IF      SANTEI-DAY (IDD)    >   ZERO
                       IF      SANTEI-MSANTEID     =   SPACE
                           MOVE    IDD             TO  SANTEI-MSANTEID
                       END-IF
                   END-IF
               END-PERFORM
               ADD     1                   TO  IDX-TBL
               MOVE    SANTEI-REC          TO  TBLW-SANTEI-REC
                                                         (IDX-TBL)
           END-PERFORM
           .
       2005121-KT01-SANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険毎のコードの判定処理
      *****************************************************************
       1000-SANTEI-CHK-SEC      SECTION.
      *
           SET     TBL-SAN             TO  1
           SEARCH  TBL-SANTEI-OCC      VARYING   TBL-SAN
               AT  END
                   MOVE    ZERO            TO  FLG-HKNSAN
                   MOVE    SPACE           TO  WRK-RSI-KBN
               WHEN    CHK-CD              =   TBL-SANTEI-SRYCD
                                                          (TBL-SAN)
                   MOVE    1               TO  FLG-HKNSAN
                   MOVE    TBL-RSI-KBN    (TBL-SAN)
                                           TO  WRK-RSI-KBN
      *            Ｈ１８／４月から対応
                   IF      TBL-RIGAKU-RYO(TBL-SAN) =   "H"
                       IF      SPA-SRYYMD      <   "20061001"
                           MOVE    ZERO            TO  FLG-HKNSAN
                           MOVE    SPACE           TO  WRK-RSI-KBN
                       END-IF
                   END-IF
           END-SEARCH
      *
           .
       1000-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象の算定履歴判定処理
      *****************************************************************
       200511-SANTEI-CHK-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-SANTEI-OK
      *    保険毎の時、保険組合せが同じこと
           IF     (FLG-HKNSAN          =   1   )  AND
                  (SPA-HKNKBN      NOT =   ZERO)
               IF      SPA-HKNCOMBINUM     =   SANTEI-HKNCOMBINUM
                   CONTINUE
               ELSE
                   GO      TO      200511-SANTEI-CHK-EXT
               END-IF
           ELSE
               IF      SANTEI-HKNCOMBINUM  =   ZERO
                   CONTINUE
               ELSE
                   GO      TO      200511-SANTEI-CHK-EXT
               END-IF
           END-IF
      *
           EVALUATE    TRUE
      *         算定履歴区分＝１
               WHEN   (SANTEI-NYUGAIKBN    =   ZERO )  AND
                      (SANTEI-SRYKA        =   ZERO )
                   MOVE    1                   TO  FLG-SANTEI-OK
      *         算定履歴区分＝２
               WHEN   (SANTEI-NYUGAIKBN    =   SPA-NYUGAIKBN) AND
                      (SANTEI-SRYKA        =   ZERO )
                   MOVE    1                   TO  FLG-SANTEI-OK
      *
      *         算定履歴区分＝３
               WHEN   (SANTEI-NYUGAIKBN    =   ZERO )  AND
                      (SANTEI-SRYKA        =   SPA-SRYKA )
                   MOVE    1                   TO  FLG-SANTEI-OK
      *
      *         算定履歴区分＝４
               WHEN   (SANTEI-NYUGAIKBN    =   SPA-NYUGAIKBN) AND
                      (SANTEI-SRYKA        =   SPA-SRYKA )
                   MOVE    1                   TO  FLG-SANTEI-OK
           END-EVALUATE
      *
           .
       200511-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ管理料のチェック処理
      *****************************************************************
       20054-RIHA-CHKSYORI-SEC        SECTION.
      *
           SET     TBL-RKA             TO  1
           SEARCH  TBL-RIHA-KANRI-OCC  VARYING   TBL-RKA
                   AT  END
                       MOVE    ZERO            TO  FLG-RKAN-OK
                       MOVE    SPACE           TO  WRK-KAN-KBN
                   WHEN   (LNK-SC92-SRYCD      =   TBL-RIHA-KAN-SRYCD
                                                           (TBL-RKA))
                       MOVE    1               TO  FLG-RKAN-OK
                       MOVE    TBL-RIHA-KAN-KBN (TBL-RKA)
                                               TO  WRK-KAN-KBN
           END-SEARCH
      *
           SET     TBL-RKA             TO  1
           SEARCH  TBL-RIHA-KANRI-OCC  VARYING   TBL-RKA
                   AT  END
                       MOVE    ZERO            TO  FLG-RKAN-OK2
                       MOVE    SPACE           TO  WRK-KAN-KBN2
                   WHEN   (CHK-CD              =   TBL-RIHA-KAN-SRYCD
                                                          (TBL-RKA))
                       MOVE    1               TO  FLG-RKAN-OK2
                       MOVE    TBL-RIHA-KAN-KBN (TBL-RKA)
                                               TO  WRK-KAN-KBN2
           END-SEARCH
      *    両方ともにリハビリ管理料の場合、エラー
           IF    ((FLG-RKAN-OK         =   ZERO )  AND
                  (FLG-RKAN-OK2        =   ZERO ))     OR
                 ((FLG-RKAN-OK         =   1    )  AND
                  (FLG-RKAN-OK2        =   1    ))
               GO      TO      20054-RIHA-CHKSYORI-EXT
           END-IF
      *    どちらかが慢性疼痛疾患管理料の場合、エラー
           IF     (LNK-SC92-SRYCD      =   "113006510" OR
                   CHK-CD              =   "113006510" )
               GO      TO      20054-RIHA-CHKSYORI-EXT
           END-IF
      *
      *    直帰の初診算定日を決定する
           PERFORM 200531-SYOSIN-SANTEI-SEC
      *
      *    発生日の判定
           PERFORM 200542-HASSYOHI-SEC
      *
      *    直帰のリハビリ管理料算定日
           IF      FLG-RKAN-OK         =   1
               MOVE    LNK-SC92-SRYCD      TO  WRK-SRYCD
               MOVE    CHK-CD              TO  WRK-SRYCD2
           ELSE
               MOVE    CHK-CD              TO  WRK-SRYCD
               MOVE    LNK-SC92-SRYCD      TO  WRK-SRYCD2
           END-IF
           MOVE    SPACE               TO  WRK-FSANTEIYMD
      *    入力が医学管理料の時、算定履歴検索
           IF      LNK-SC92-SRYCD      =   WRK-SRYCD
               PERFORM 2005332-SANTEI-CHK-SEC
           ELSE
      *        今月の算定有無
               PERFORM 200543-KANRICHK-SEC
           END-IF
      *    訂正の時、訂正日が初回の場合
           IF      LNK-SC92-SYORIFLG   =   1
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-MAX-LINE )  OR
                                  (LNK-SC92-TEI-SRYCD (IDX)  =   SPACE)
                   IF      LNK-SC92-TEI-SRYCD (IDX)    =   WRK-SRYCD
                       IF      WRK-FSANTEIYMD      =   SPA-SRYYMD
                           MOVE    SPACE           TO  WRK-FSANTEIYMD
                           MOVE    SPACE           TO  WRK-YMD
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           PERFORM 200541-RIHASANTEI-CHK-SEC
      *    今回入力でないこと
           MOVE    ZERO                TO  FLG-RKAN-ARI
           MOVE    ZERO                TO  FLG-RKAN-ARI2
           IF      LNK-SC92-SRYCD  NOT =   WRK-SRYCD
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                                 ((SPA-GMN-INPUTCD (IDX)
                                               =   SPACE ) AND
                                  (SPA-COM-INPUTCD (IDX)
                                               =   SPACE ))
                   IF      SPA-COM-INPUTCD(IDX)    =   WRK-SRYCD
                       MOVE    1                   TO  FLG-RKAN-ARI
                   END-IF
                   IF      SPA-COM-INPUTCD(IDX)    =   WRK-SRYCD2
                       MOVE    1                   TO  FLG-RKAN-ARI2
                   END-IF
               END-PERFORM
           END-IF
      *
      *    初回リハビリ管理料がない時ＯＫ
           IF     (WRK-YMD             =   SPACE  )  AND
                  (FLG-RKAN-ARI        =   ZERO   )
               MOVE    SPACE           TO  LNK-SC92-ERRCD6
               MOVE    SPACE           TO  LNK-SC92-ERRMSG
               MOVE    ZERO            TO  FLG-CHKERR 
               MOVE    SPACE           TO  WRK-KEI-SRYCD
           END-IF
      *    訂正の時、当日ならＯＫとする
           IF      LNK-SC92-SYORIFLG   =   1
               IF     (WRK-YMD             =   SPA-SRYYMD  )  AND
                      (FLG-RKAN-ARI        =   ZERO   )  AND
                      (FLG-RKAN-ARI2       =   ZERO   )
                   MOVE    SPACE           TO  LNK-SC92-ERRCD6
                   MOVE    SPACE           TO  LNK-SC92-ERRMSG
                   MOVE    ZERO            TO  FLG-CHKERR 
                   MOVE    SPACE           TO  WRK-KEI-SRYCD
               END-IF
           END-IF
      *
      *    初診算定日が前回の算定日より後の時、算定ができること
           IF    ((WRK-YMD             =   SPACE  )  OR
                  (WRK-SYOSINYMD1      >   WRK-YMD))
              AND (FLG-RKAN-ARI        =   ZERO   )
              AND (FLG-RKAN-ARI2       =   ZERO   )
               MOVE    SPACE           TO  LNK-SC92-ERRCD6
               MOVE    SPACE           TO  LNK-SC92-ERRMSG
               MOVE    ZERO            TO  FLG-CHKERR 
               MOVE    SPACE           TO  WRK-KEI-SRYCD
           END-IF
      *
           .
       20054-RIHA-CHKSYORI-EXT.
           EXIT.
      *****************************************************************
      *    リハビリ発生日処理
      *****************************************************************
       200542-HASSYOHI-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
           MOVE    SPACE               TO  WRK-SRYCD2
      *    心大血管疾患リハビリテーション
           IF     (WRK-KAN-KBN         =   "1"  )  OR
                  (WRK-KAN-KBN2        =   "1"  )
               MOVE    "099800111"         TO  WRK-SRYCD
               MOVE    "099800112"         TO  WRK-SRYCD2
           END-IF
      *    脳血管疾患リハビリテーション
           IF     (WRK-KAN-KBN         =   "2"  )  OR
                  (WRK-KAN-KBN2        =   "2"  )
               MOVE    "099800121"         TO  WRK-SRYCD
               MOVE    "099800122"         TO  WRK-SRYCD2
           END-IF
      *    運動器リハビリテーション
           IF     (WRK-KAN-KBN         =   "3"  )  OR
                  (WRK-KAN-KBN2        =   "3"  )
               MOVE    "099800131"         TO  WRK-SRYCD
               MOVE    "099800132"         TO  WRK-SRYCD2
           END-IF
      *    呼吸器リハビリテーション
           IF     (WRK-KAN-KBN         =   "4"  )  OR
                  (WRK-KAN-KBN2        =   "4"  )
               MOVE    "099800141"         TO  WRK-SRYCD
               MOVE    "099800142"         TO  WRK-SRYCD2
           END-IF
      *
      *    保険毎
           MOVE    1                   TO  FLG-HKNSAN
           INITIALIZE                  HZN2-SANTEI-REC
           INITIALIZE                  HZN-SANTEI-REC
           MOVE    ZERO                TO  FLG-SANTEI-OK
           PERFORM VARYING     IDX2    FROM    1   BY   1
                   UNTIL      (IDX2    >   IDX-TBL )  OR
                              (FLG-SANTEI-OK   =   1  )
               IF      WRK-SRYCD           =   TBLW-SANTEI-SRYCD(IDX2)
      *            対象の算定履歴判定
                   MOVE    TBLW-SANTEI-REC(IDX2)   TO  HZN-SANTEI-REC
                   PERFORM 200511-SANTEI-CHK-SEC
                   IF      FLG-SANTEI-OK       =   ZERO
                       INITIALIZE                  HZN-SANTEI-REC
                   END-IF
               END-IF
               IF      WRK-SRYCD2          =   TBLW-SANTEI-SRYCD(IDX2)
      *            対象の算定履歴判定
                   MOVE    TBLW-SANTEI-REC(IDX2)   TO  HZN2-SANTEI-REC
                   PERFORM 200511-SANTEI-CHK-SEC
                   IF      FLG-SANTEI-OK       =   ZERO
                       INITIALIZE                  HZN2-SANTEI-REC
                   END-IF
               END-IF
           END-PERFORM
      *
      *    訂正の時、訂正分はカウントしない
           IF      LNK-SC92-SYORIFLG   =   1
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   (IDX     >   SPA-MAX-LINE )  OR
                               (LNK-SC92-TEI-SRYCD (IDX)  =   SPACE)
                   IF      LNK-SC92-TEI-SRYCD (IDX)    =   WRK-SRYCD
                       MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
                       MOVE    ZERO                TO  HZN-SANTEI-DAY
                                                           (IDX-Y2)
                   END-IF
               END-PERFORM
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   (IDX     >   SPA-MAX-LINE )  OR
                               (LNK-SC92-TEI-SRYCD (IDX)  =   SPACE)
                   IF      LNK-SC92-TEI-SRYCD (IDX)    =   WRK-SRYCD2
                       MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
                       MOVE    ZERO                TO  HZN2-SANTEI-DAY
                                                           (IDX-Y2)
                   END-IF
               END-PERFORM
           END-IF
           MOVE    SPACE               TO  WRK-STYMD
           IF      HZN-SANTEI-MSANTEICNT   >   ZERO
               MOVE    SPA-SRYYMD(7:2)     TO  IDX-DD
               MOVE    ZERO                TO  WRK-STDD
               PERFORM VARYING     IDX-Y2  FROM    1   BY  1
                       UNTIL       IDX-Y2      >   31
                   IF      HZN-SANTEI-DAY (IDX-Y2)    >   ZERO
                       IF      WRK-KAN-KBN         =   SPACE
      *                    医学管理料以外を今回入力時、前の発生日
                           IF      IDX-Y2          <=  IDX-DD
                               MOVE    IDX-Y2          TO  WRK-STDD
                               MOVE    31              TO  IDX-Y2
                           END-IF
                       ELSE
      *                    医学管理料を今回入力時、後の発生日
                           IF      IDX-Y2          >   IDX-DD
                               MOVE    IDX-Y2          TO  WRK-STDD
                               MOVE    31              TO  IDX-Y2
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
               IF      WRK-STDD        NOT =   ZERO
                   MOVE    SANTEI-SRYYM        TO  WRK-STYYMM
               ELSE
                   MOVE    SPACE               TO  WRK-STYMD
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  WRK-EDYMD
           IF      HZN2-SANTEI-MSANTEICNT  >   ZERO
               MOVE    SPA-SRYYMD(7:2)     TO  IDX-DD
               MOVE    ZERO                TO  WRK-EDDD
               PERFORM VARYING     IDX-Y2  FROM    1   BY  1
                       UNTIL       IDX-Y2      >   31
                   IF      HZN2-SANTEI-DAY (IDX-Y2)    >   ZERO
                       IF      WRK-KAN-KBN         =   SPACE
      *                    医学管理料以外を今回入力時、前の発生日
                           IF      IDX-Y2          <=  IDX-DD
                               MOVE    IDX-Y2          TO  WRK-EDDD
                               MOVE    31              TO  IDX-Y2
                           END-IF
                       ELSE
      *                    医学管理料を今回入力時、後の発生日
                           IF      IDX-Y2          >   IDX-DD
                               MOVE    IDX-Y2          TO  WRK-EDDD
                               MOVE    31              TO  IDX-Y2
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
               IF      WRK-EDDD        NOT =   ZERO
                   MOVE    SANTEI-SRYYM        TO  WRK-EDYYMM
               ELSE
                   MOVE    SPACE               TO  WRK-EDYMD
               END-IF
           END-IF
      *    画面入力
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF      SPA-COM-INPUTCD (IDX)   =   WRK-SRYCD
                   MOVE    SPA-SRYYMD          TO  WRK-STYMD
               END-IF
               IF      SPA-COM-INPUTCD (IDX)   =   WRK-SRYCD2
                   MOVE    SPA-SRYYMD          TO  WRK-EDYMD
               END-IF
           END-PERFORM
      *
           IF     (WRK-STYMD           =   SPACE )
               MOVE    WRK-EDYMD           TO  WRK-STYMD
           END-IF
           .
       200542-HASSYOHI-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴チェック処理（リハビリ）
      *****************************************************************
       200543-KANRICHK-SEC                SECTION.
      *
      *    保険毎のコードの判定
           SET     TBL-SAN             TO  1
           SEARCH  TBL-SANTEI-OCC      VARYING   TBL-SAN
               AT  END
                   MOVE    ZERO            TO  FLG-HKNSAN
                   MOVE    SPACE           TO  WRK-RSI-KBN
               WHEN    WRK-SRYCD           =   TBL-SANTEI-SRYCD
                                                          (TBL-SAN)
                   MOVE    1               TO  FLG-HKNSAN
                   MOVE    TBL-RSI-KBN    (TBL-SAN)
                                           TO  WRK-RSI-KBN
      *            Ｈ１８／４月から対応
                   IF      TBL-RIGAKU-RYO(TBL-SAN) =   "H"
                       IF      SPA-SRYYMD      >=  "20060401"
                           MOVE    ZERO            TO  FLG-HKNSAN
                           MOVE    SPACE           TO  WRK-RSI-KBN
                       END-IF
                   END-IF
           END-SEARCH
      *
           INITIALIZE                  SANTEI-REC
           MOVE    ZERO                TO  FLG-SANTEI-OK
           PERFORM VARYING     IDX2    FROM    1   BY   1
                   UNTIL      (IDX2    >   IDX-TBL )  OR
                              (FLG-SANTEI-OK   =   1  )
               IF      WRK-SRYCD       =   TBLW-SANTEI-SRYCD(IDX2)
      *            対象の算定履歴判定
                   MOVE    TBLW-SANTEI-REC(IDX2)   TO  SANTEI-REC
                   PERFORM 200511-SANTEI-CHK-SEC
               END-IF
           END-PERFORM
           IF      FLG-SANTEI-OK       =   ZERO
               INITIALIZE                  SANTEI-REC
           END-IF
           IF      SANTEI-MSANTEICNT   >   ZERO
               PERFORM VARYING     IDX-Y2  FROM    1  BY  1
                       UNTIL       IDX-Y2      >   31
                   IF      SANTEI-DAY (IDX-Y2) >   ZERO
                       MOVE    IDX-Y2          TO  WRK-DD
      *D               MOVE    31              TO  IDX-Y2
                   END-IF
               END-PERFORM
               MOVE    SANTEI-SRYYM        TO  WRK-YYMM
      *        初回リハビリ管理料の時、診療日より後の時なしとする
               IF    ((FLG-RKAN-OK         =   1  )  OR
                      (FLG-RKAN-OK2        =   1  ))  AND
                      (SANTEI-FSANTEIYMD(1:6)
                                           =   SANTEI-SRYYM ) AND
                       (SPA-SRYYMD(1:6)    =   SANTEI-SRYYM ) AND
                       (SPA-SRYYMD(7:2)    <   SANTEI-MSANTEID)
                   MOVE    SPACE               TO  WRK-YMD
               END-IF
               IF    ((FLG-RKAN-OK         =   1  )  OR
                      (FLG-RKAN-OK2        =   1  ))
      *            診療日より前
                   IF      SPA-SRYYMD      >   SANTEI-FSANTEIYMD
                       MOVE    SANTEI-FSANTEIYMD   TO  WRK-FSANTEIYMD
                   END-IF
               END-IF
               IF    ((FLG-RKAN-OK         =   1  )  OR
                      (FLG-RKAN-OK2        =   1  ))
      *            発生日より前の時、算定なしとする
                   IF      WRK-STYMD       >   WRK-YMD
                       MOVE    SPACE               TO  WRK-YMD
                   END-IF
               END-IF
           END-IF
      *
           .
       200543-KANRICHK-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴チェック処理（リハビリ）
      *****************************************************************
       200541-RIHASANTEI-CHK-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-YMD2
      *
      *    保険毎のコードの判定
           SET     TBL-SAN             TO  1
           SEARCH  TBL-SANTEI-OCC      VARYING   TBL-SAN
               AT  END
                   MOVE    ZERO            TO  FLG-HKNSAN
                   MOVE    SPACE           TO  WRK-RSI-KBN
               WHEN    WRK-SRYCD2          =   TBL-SANTEI-SRYCD
                                                          (TBL-SAN)
                   MOVE    1               TO  FLG-HKNSAN
                   MOVE    TBL-RSI-KBN    (TBL-SAN)
                                           TO  WRK-RSI-KBN
      *            Ｈ１８／４月から対応
                   IF      TBL-RIGAKU-RYO(TBL-SAN) =   "H"
                       IF      SPA-SRYYMD      >=  "20060401"
                           MOVE    ZERO            TO  FLG-HKNSAN
                           MOVE    SPACE           TO  WRK-RSI-KBN
                       END-IF
                   END-IF
           END-SEARCH
      *
           INITIALIZE                  HZN-SANTEI-REC
      *
           INITIALIZE                  SANTEI-REC
           MOVE    ZERO                TO  FLG-SANTEI-OK
           PERFORM VARYING     IDX2    FROM    1   BY   1
                   UNTIL      (IDX2    >   IDX-TBL )  OR
                              (FLG-SANTEI-OK   =   1  )
               IF      WRK-SRYCD2          =   TBLW-SANTEI-SRYCD(IDX2)
      *            対象の算定履歴判定
                   MOVE    TBLW-SANTEI-REC(IDX2)   TO  SANTEI-REC
                   PERFORM 200511-SANTEI-CHK-SEC
               END-IF
           END-PERFORM
           IF      FLG-SANTEI-OK       =   ZERO
               INITIALIZE                  SANTEI-REC
           END-IF
      *
      *    訂正の時、訂正分はカウントしない
           IF      LNK-SC92-SYORIFLG   =   1
               MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
               IF     (SANTEI-SRYYM        =   SPA-SRYYMD(1:6)) AND
                      (SANTEI-DAY (IDX-Y2) >   ZERO ) 
                   MOVE    ZERO            TO  SANTEI-DAY (IDX-Y2)
               END-IF
           END-IF
           IF      SANTEI-MSANTEICNT   >   ZERO
               PERFORM VARYING     IDX-Y2  FROM    1  BY  1
                       UNTIL       IDX-Y2      >   31
                   IF      SANTEI-DAY (IDX-Y2) >   ZERO
                       MOVE    IDX-Y2          TO  WRK-DD2
                   END-IF
               END-PERFORM
               IF      WRK-DD2         NOT =   SPACE
                   MOVE    SANTEI-SRYYM        TO  WRK-YYMM2
               END-IF
      *        初回リハビリ管理料より前の時なしとする
               IF     (WRK-FSANTEIYMD  NOT =   SPACE )  AND
                      (WRK-YMD2            <   WRK-FSANTEIYMD) AND
                      (WRK-FSANTEIYMD      <   SPA-SRYYMD    )
                   IF      LNK-SC92-SRYCD  =   WRK-SRYCD
                       MOVE    SPACE           TO  WRK-YMD
                   END-IF
               END-IF
      *        今回が初回
               IF     (WRK-FSANTEIYMD      =   SPACE )  AND
                      (WRK-YMD2            >=  SPA-SRYYMD)
                   IF      LNK-SC92-SRYCD  =   WRK-SRYCD
                       MOVE    SPA-SRYYMD      TO  WRK-YMD
                   END-IF
               END-IF
      *        発生日より後の時、算定なしとする
               IF     (WRK-STYMD       NOT =   SPACE     )  AND
                      (WRK-STYMD           <=  WRK-YMD2  )
                   MOVE    SPACE               TO  WRK-YMD
               END-IF
      *!
      *        医学管理料算定より前の時、なし
               IF     (LNK-SC92-SRYCD      =   WRK-SRYCD  )
                  AND (WRK-YMD2            <   WRK-YMD    )
                   MOVE    SPACE               TO  WRK-YMD
               END-IF
           END-IF
      *
           .
      *
       200541-RIHASANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     リハビリ料チェック処理
      *****************************************************************
      *200542-RIHA-HENKAN-SEC        SECTION.
      *
      *    SET     TBL-RIH             TO  1
      *    SEARCH  TBL-RIHABIRI-OCC    VARYING   TBL-RIH
      *            AT  END
      *                MOVE    ZERO            TO  FLG-RIHA-OK
      *            WHEN    WRK-SRYCD-3     =   TBL-RIHA-SRYCD(TBL-RIH)
      *                MOVE    1               TO  FLG-RIHA-OK
      *    END-SEARCH
      *    .
      *200542-RIHA-HENKAN-EXT.
      *    EXIT.
      *
      *****************************************************************
      *    入院中の場合、算定なしとする
      *****************************************************************
       200541-NYUIN-CHK-SEC                SECTION.
      *
           PERFORM VARYING     IDX-Y3  FROM    1   BY   1
                   UNTIL       IDX-Y3  >   31
      *        入院期間チェック
               IF      SPA-K02-NYUDAY(IDX-Y3)  =   1
                   COMPUTE WRK-SANTEI-CNT  =   WRK-SANTEI-CNT
                                           -   SANTEI-DAY (IDX-Y3)
                   MOVE    ZERO            TO  SANTEI-DAY (IDX-Y3)
               END-IF
           END-PERFORM
      *
      *    初回算定日の変更
           IF      WRK-SANTEI-CNT      >   ZERO
               PERFORM VARYING     IDX-Y3  FROM    1   BY   1
                       UNTIL       IDX-Y3  >   31
                   IF      SANTEI-DAY (IDX-Y3)     >   ZERO
                       MOVE    IDX-Y3          TO  SANTEI-MSANTEID
                       MOVE    31              TO  IDX-Y3
                   END-IF
               END-PERFORM
           END-IF
           .
       200541-NYUIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴チェック処理
      *****************************************************************
       200531-SYOSIN-SANTEI-SEC                SECTION.
      *
      *    初診料の算定履歴のチェック
           MOVE    SPACE               TO  WRK-SYOSINYMD1
      *
           IF      LNK-SC92-SYORIFLG   =   1
               MOVE    SPA-SYOYMD-OLD      TO  WRK-SYOSINYMD1
           ELSE
               IF      SPA-SYOYMD-OLD      =   SPACE
                   MOVE    SPA-SYOYMD          TO  WRK-SYOSINYMD1
               ELSE
                   MOVE    SPA-SYOYMD-OLD      TO  WRK-SYOSINYMD1
               END-IF
           END-IF
           .
      *
       200531-SYOSIN-SANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴チェック処理
      *****************************************************************
       2005332-SANTEI-CHK-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-YMD
      *
      *    保険毎のコードの判定
           SET     TBL-SAN             TO  1
           SEARCH  TBL-SANTEI-OCC      VARYING   TBL-SAN
               AT  END
                   MOVE    ZERO            TO  FLG-HKNSAN
                   MOVE    SPACE           TO  WRK-RSI-KBN
               WHEN    WRK-SRYCD           =   TBL-SANTEI-SRYCD
                                                          (TBL-SAN)
                   MOVE    1               TO  FLG-HKNSAN
                   MOVE    TBL-RSI-KBN    (TBL-SAN)
                                           TO  WRK-RSI-KBN
      *            Ｈ１８／４月から対応
                   IF      TBL-RIGAKU-RYO(TBL-SAN) =   "H"
                       IF      SPA-SRYYMD      >=  "20060401"
                           MOVE    ZERO            TO  FLG-HKNSAN
                           MOVE    SPACE           TO  WRK-RSI-KBN
                       END-IF
                   END-IF
           END-SEARCH
      *
           INITIALIZE                  HZN-SANTEI-REC
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    SPA-SRYYMD(1:6)     TO  SANTEI-SRYYM
           IF     (FLG-HKNSAN          =   1   )  AND
                  (SPA-HKNKBN      NOT =   ZERO)
               MOVE    SPA-HKNCOMBINUM     TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           END-IF
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           PERFORM UNTIL       FLG-SANTEI          =   1
      *        終了判定
               IF     (SANTEI-MSANTEICNT   >   ZERO )   AND
                      (SANTEI-SRYYM        <   HZN-SANTEI-SRYYM)
                   MOVE    1                   TO  FLG-SANTEI
               ELSE
      *            対象の算定履歴判定
                   PERFORM 200511-SANTEI-CHK-SEC
                   IF      FLG-SANTEI-OK       =   1
                       MOVE    SANTEI-REC          TO  HZN-SANTEI-REC
                   END-IF
               END-IF
      *
               IF      FLG-SANTEI          =   ZERO
                   MOVE    "tbl_santei"        TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 920-SANTEI-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *!!!
      *    外来まとめは今回分ＴＢＬ編集
           IF      LNK-SC92-KT01FLG    =   "1" 
               PERFORM VARYING     IDX2    FROM    1   BY   1
                       UNTIL      (IDX2    >   IDX-TBL    )
                   IF      WRK-SRYCD       =   TBLW-SANTEI-SRYCD(IDX2)
      *            対象の算定履歴判定
                       MOVE    TBLW-SANTEI-REC(IDX2)   TO  SANTEI-REC
      *                初回あり
                       IF      HZN-SANTEI-MSANTEICNT   >   ZERO
                           MOVE    HZN-SANTEI-FSANTEIYMD
                                                   TO  SANTEI-FSANTEIYMD
                       ELSE
                           MOVE    SANTEI-SRYYM    TO  SANTEI-FSANTEIYMD
                                                                  (1:6)
                           MOVE    SANTEI-MSANTEID TO  SANTEI-FSANTEIYMD
                                                                  (7:2)
                       END-IF
                       MOVE    SANTEI-REC          TO  HZN-SANTEI-REC
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    HZN-SANTEI-REC          TO  SANTEI-REC
           IF      SANTEI-MSANTEICNT   >   ZERO
               PERFORM VARYING     IDX-Y2  FROM    1  BY  1
                       UNTIL       IDX-Y2      >   31
                   IF      SANTEI-DAY (IDX-Y2) >   ZERO
                       MOVE    IDX-Y2          TO  WRK-DD
                       MOVE    31              TO  IDX-Y2
                   END-IF
               END-PERFORM
               MOVE    SANTEI-SRYYM        TO  WRK-YYMM
      *        初回慢性疼痛の時、診療日より後の時なしとする
               IF     (WRK-SRYCD       =   "113006510" ) AND
                      (SANTEI-FSANTEIYMD(1:6)
                                           =   SANTEI-SRYYM ) AND
                          (SPA-SRYYMD(1:6) =   SANTEI-SRYYM ) AND
                          (SPA-SRYYMD(7:2) <   SANTEI-MSANTEID)
                   MOVE    SPACE               TO  WRK-YMD
               END-IF
      *        初回リハビリ管理料の時、診療日より後の時なしとする
               IF    ((FLG-RKAN-OK         =   1  )  OR
                      (FLG-RKAN-OK2        =   1  ))  AND
                      (SANTEI-FSANTEIYMD(1:6)
                                           =   SANTEI-SRYYM ) AND
                       (SPA-SRYYMD(1:6)    =   SANTEI-SRYYM ) AND
                       (SPA-SRYYMD(7:2)    <   SANTEI-MSANTEID)
                   MOVE    SPACE               TO  WRK-YMD
               END-IF
               IF    ((FLG-RKAN-OK         =   1  )  OR
                      (FLG-RKAN-OK2        =   1  ))
      *            診療日より前
                   IF      SPA-SRYYMD      >   SANTEI-FSANTEIYMD
                       MOVE    SANTEI-FSANTEIYMD   TO  WRK-FSANTEIYMD
                   END-IF
               END-IF
           END-IF
      *
           .
      *
       2005332-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     初診料とのチェック処理
      *****************************************************************
       20056-SYOSIN-CHK-SEC        SECTION.
      *
      *    初診料
           MOVE    "111000110"         TO  WRK-SRYCD
      *
           MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
           MOVE    ZERO                TO  FLG-SYO-ERR
           IF      CHK-CD              =   WRK-SRYCD
      *        初診以外入力時、前に初診算定があればエラー
               PERFORM VARYING     IDX-Y3  FROM    1   BY   1
                       UNTIL       IDX-Y3  >   31
                   IF     (SANTEI-DAY (IDX-Y3) >   ZERO )
                      AND (IDX-Y2              >=  IDX-Y3 )
                       MOVE    2               TO  FLG-SYO-ERR
                   END-IF
               END-PERFORM
           ELSE
      *        初診入力時、後に算定があればエラー
               PERFORM VARYING     IDX-Y3  FROM    1   BY   1
                       UNTIL       IDX-Y3  >   31
                   IF     (SANTEI-DAY (IDX-Y3) >   ZERO )
                      AND (IDX-Y2              <=  IDX-Y3 )
                       MOVE    1               TO  FLG-SYO-ERR
                   END-IF
               END-PERFORM
           END-IF
           IF      FLG-SYO-ERR         =   ZERO
               MOVE    "1"             TO  LNK-SC92-SYOKBN
               MOVE    ZERO            TO  WRK-SANTEI-CNT
           END-IF
           .
       20056-SYOSIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力値とのチェック処理
      *****************************************************************
       200521-CHKNYU-INPUT-SEC                SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE          )  OR
                             ((SPA-GMN-INPUTCD (IDX)   =   SPACE ) AND
                              (SPA-COM-INPUTCD (IDX)   =   SPACE )) OR
                              (FLG-KONKAI          =   1     )
               IF     (IDX                 =   IDX-T   )
                 OR   (SPA-COM-HKNCOMBI (IDX)  =   "9999" )
                   CONTINUE
               ELSE
                   MOVE    ZERO                TO  FLG-DAYERR
                   IF      SPA-COM-INPUTCD(IDX)    =   CHK-CD
      *                同じ日の指定があること
                       PERFORM VARYING IDX-N2      FROM    1   BY  1
                               UNTIL  (IDX-N2      >   31  )   OR
                                      (FLG-DAYERR  =   1   )
                           IF     (SPA-COM-NYUINDAY(IDX IDX-N2)
                                                        >   ZERO ) AND
                                  (SPA-COM-NYUINDAY(IDX-T IDX-N2)
                                                        >   ZERO )
                               MOVE    1                TO  FLG-DAYERR
                               MOVE    IDX-N2           TO  WRK-DAYERR
                           END-IF
                       END-PERFORM
                   END-IF
                   IF      FLG-DAYERR          =   1
                       IF      CHK-CHKERR          =   ZERO
                           MOVE    CHK-CD              TO  WRK-ERR-SRYCD
                           MOVE    1                   TO  FLG-CHKERR
                           MOVE    1                   TO  FLG-KONKAI
                       ELSE
      *                    警告
                           IF     (SPA-COM-HKEIFLG (IDX-T) =   SPACE)
                              AND (WRK-KEI-SRYCD       =   SPACE    )
                               MOVE    CHK-CD          TO  WRK-KEI-SRYCD
                               MOVE    1               TO  FLG-KONKAI
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .
       200521-CHKNYU-INPUT-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴の入院同日併算定チェック
      *****************************************************************
       200531-DAYCHKNYU-SANTEI-SEC            SECTION.
      *
      *    算定履歴テーブル保存
           IF      FLG-SANTEI-TBL      =   ZERO
               PERFORM 200512-SANTEI-SELECT-SEC
               MOVE    1               TO  FLG-SANTEI-TBL
           END-IF
      *    保険毎のコードの判定
           PERFORM 1000-SANTEI-CHK-SEC
      *
           INITIALIZE                  SANTEI-REC
           MOVE    ZERO                TO  FLG-SANTEI-OK
           PERFORM VARYING     IDX2    FROM    1   BY   1
                   UNTIL      (IDX2    >   IDX-TBL )  OR
                              (FLG-SANTEI-OK   =   1  )
               IF      CHK-CD              =   TBLW-SANTEI-SRYCD(IDX2)
      *            対象の算定履歴判定
                   MOVE    TBLW-SANTEI-REC(IDX2)   TO  SANTEI-REC
                   PERFORM 200511-SANTEI-CHK-SEC
               END-IF
           END-PERFORM
           IF      FLG-SANTEI-OK       =   ZERO
               INITIALIZE                  SANTEI-REC
           END-IF
      *    訂正の時、訂正分はカウントしない
           MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
           IF      LNK-SC92-SYORIFLG   =   1
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2    >   SPA-MAX-LINE )  OR
                                  (LNK-SC92-TEI-SRYCD(IDX2)
                                               =   SPACE )
                   IF      SANTEI-SRYCD    =   LNK-SC92-TEI-SRYCD(IDX2)
                       ADD     LNK-SC92-TEI-KAISU(IDX2)
                                               TO  WRK-TEI-CNT
                   END-IF
               END-PERFORM
               IF      SANTEI-DAY (IDX-Y2) >=  WRK-TEI-CNT
                   COMPUTE SANTEI-DAY (IDX-Y2) =   SANTEI-DAY (IDX-Y2)
                                               -   WRK-TEI-CNT
               ELSE
                    MOVE    ZERO           TO  SANTEI-DAY (IDX-Y2)
               END-IF
           END-IF
      *    同日に算定があればエラー
           MOVE    ZERO                TO  FLG-DAYERR
           PERFORM VARYING IDX-N2      FROM    1   BY  1
                   UNTIL  (IDX-N2      >   31  )   OR
                          (FLG-DAYERR  =   1   )
               IF     (SANTEI-DAY (IDX-N2)              >   ZERO ) AND
                      (SPA-COM-NYUINDAY(IDX-T IDX-N2)
                                                        >   ZERO )
                   MOVE    1                TO  FLG-DAYERR
                   MOVE    IDX-N2           TO  WRK-DAYERR
               END-IF
           END-PERFORM
           IF      FLG-DAYERR          =   1
               IF      CHK-CHKERR          =   ZERO
                   MOVE    CHK-CD              TO  WRK-ERR-SRYCD
                   MOVE    1                   TO  FLG-CHKERR
                   MOVE    1                   TO  FLG-DAYCHK
               ELSE
      *            警告
                   IF      SPA-COM-HKEIFLG (IDX-T) =   SPACE
                       IF      WRK-KEI-SRYCD       =   SPACE
                           MOVE    CHK-CD          TO  WRK-KEI-SRYCD
                           MOVE    1               TO  FLG-DAYCHK
                      END-IF
                  END-IF
               END-IF
           END-IF
      *
           .
       200531-DAYCHKNYU-SANTEI-EXT.
           EXIT.
      *****************************************************************
      *     労災・自賠責 チェック処理
      *****************************************************************
       20088-RSIJIB-CHK-SEC        SECTION.
      *
      *    包括まとめ入力は以下対象外
          IF      SPA-HKNCOMBINUM     =   "9999"
               GO      TO      20088-RSIJIB-CHK-EXT
           END-IF
      *
      *     労災のコード
           IF     (LNK-SC92-SRYCD (1:1)    =   "1"  )  AND
                  (LNK-SC92-SRYCD (2:2)    =   "01" )
               IF     (SPA-HKNKBN          NOT =   1 )
      ********    OR  (SPA-RSI-HKNKBN          =   "5" )
                  OR  (SPA-RSI-JUNKYO          =   "1" )
                   MOVE    "8000"              TO  SPA-ERRCD
               END-IF
           END-IF
      *    労災（器材）
           IF     (LNK-SC92-SRYCD (1:1)    =   "7"  )  AND
                  (LNK-SC92-SRYCD (2:5)    =   "88888")
               IF     (SPA-HKNKBN          NOT =   1 )
      *******     OR  (SPA-RSI-HKNKBN          =   "5" )
                  OR  (SPA-RSI-JUNKYO          =   "1" )
                   MOVE    "8000"              TO  SPA-ERRCD
               END-IF
           END-IF
      *    労災（コメント）
           IF     (LNK-SC92-SRYCD (1:1)    =   "8"  )  AND
                  (LNK-SC92-SRYCD (4:1)    =   "8"  )
               IF     (SPA-HKNKBN          NOT =   1 )
     ***********  OR  (SPA-RSI-HKNKBN          =   "5" )
                  OR  (SPA-RSI-JUNKYO          =   "1" )
                   MOVE    "8000"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *    労災加算振替え
           IF      LNK-SC92-SRYCD          =   "099509901"
               IF     (SPA-HKNKBN          NOT =   1   )
                  OR  (SPA-RSI-JUNKYO          =   "1" )
                   MOVE    "8000"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *!!
      *    公害のコード
           IF     (LNK-SC92-SRYCD (1:1)    =   "1"  )  AND
                  (LNK-SC92-SRYCD (2:2)    =   "02" )
               IF     (SPA-HKNKBN          NOT =   2 )
                   MOVE    "8100"              TO  SPA-ERRCD
               END-IF
           END-IF
           .
       20088-RSIJIB-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者禁忌薬剤チェック処理
      *****************************************************************
       3008-PTKINKI-CHK-SEC           SECTION.
      *
           INITIALIZE                      PTKINKI-REC
           MOVE    SPA-HOSPNUM         TO  PTKINKI-HOSPNUM
           MOVE    SPA-PTID            TO  PTKINKI-PTID
           MOVE    TNS-SRYCD           TO  PTKINKI-SRYCD
           MOVE    PTKINKI-REC         TO  MCPDATA-REC
           MOVE    "tbl_ptkinki"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkinki"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-PTKINKI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTKINKI
               INITIALIZE                      PTKINKI-REC
           END-IF
           MOVE    "tbl_ptkinki"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    患者禁忌薬剤チェック
           IF      FLG-PTKINKI         =   ZERO
               MOVE    1               TO  SPA-COM-PTKINKI-ARI
                                                           (IDX-T)
      *        禁忌開始日判定
               IF     (PTKINKI-KINKSTYMD   NOT =   SPACE )
                   IF      (SPA-NYUGAIKBN      =   "2"     )    AND
                           (PTKINKI-KINKSTYMD  >   SPA-SRYYMD )
      *                外来で当日が開始日以前
                       MOVE    ZERO            TO  SPA-COM-PTKINKI-ARI
                                                               (IDX-T)
                   END-IF
                   IF      (SPA-NYUGAIKBN      =   "1"   )  AND
                           (PTKINKI-KINKSTYMD(1:6)  >=  SPA-SRYYMD(1:6))
      *                入院で当月が開始日以前
                       MOVE    ZERO            TO  SPA-COM-PTKINKI-ARI
                                                               (IDX-T)
                   END-IF
               END-IF
               IF     (SPA-COM-PTKINKI-ARI (IDX-T) =   1  )
               OR     (PTKINKI-KINKSTYMD(1:6)      =  SPA-SRYYMD(1:6))
                   MOVE    PTKINKI-KINKSTYMD
                                       TO  SPA-COM-KINKSTYMD (IDX-T)
                   MOVE    PTKINKI-TOUYOYMD
                                       TO  SPA-COM-TOUYOYMD  (IDX-T)
               END-IF
           END-IF
           MOVE    1                   TO  SPA-COM-PTKINKI (IDX-T)
           .
       2009-PTKINKI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ内容編集処理
      *****************************************************************
       300-TENSU-HENSYU-SEC           SECTION.
      *
      *    基礎点数
           MOVE    TNS-TEN             TO  SPA-COM-TEN      (IDX-T)
           MOVE    TNS-TEN             TO  SPA-COM-KISOTEN  (IDX-T)
      *    名称
           MOVE    TNS-NAME            TO  SPA-COM-NAME     (IDX-T)
      *
           IF      SPA-HKNKBN          =   1
      *        労災・自賠責 編集
               PERFORM 3000-RSIJIB-HEN-SEC
           END-IF
      *
      *    名称(フリーコメントの時、前回のコメントをのこす）
           IF    ((SPA-COM-INPUTCD (IDX-T)   =   "810000001"
                                             OR  "008500000"
                                             OR  "008600000" )  AND
                  (SPA-GMN-MEISYO-G(IDX-T)   NOT =   SPACE   )) OR
                 ((SPA-COM-INPUTCD (IDX-T)(1:2)  =   "83"    ) AND
                  (SPA-GMN-MEISYO  (IDX-T)   NOT =   SPACE   ))  OR
      *         ユザーの埋め込みコメント
                 ((SPA-COM-INPUTCD (IDX-T)(1:4)  =   "0083"
                                                 OR  "0086"
                                                 OR  "0085"  ) AND
                  (SPA-GMN-MEISYO  (IDX-T)   NOT =   SPACE   ))
               CONTINUE
           ELSE
               MOVE    TNS-NAME      TO  SPA-GMN-MEISYO  (IDX-T)
      *        薬剤服用コメントの時、再編集
               IF      SPA-COM-INPUTCD (IDX-T)(1:3)    =   "001"
                   PERFORM 3001-FUKIYO-MEISYO-SEC
                   MOVE    WRK-MEISYO    TO  SPA-GMN-MEISYO (IDX-T)
               END-IF
      *        後発医薬品区分編集
               IF     (SPA-COM-INPUTCD (IDX-T)(1:1)    =   "6")
                   PERFORM 3002-KOUHATU-MEISYO-SEC
                   MOVE    WRK-MEISYO    TO  SPA-GMN-MEISYO (IDX-T)
               END-IF
      *       後発医薬品変更再編集
              IF      SPA-COM-INPUTCD (IDX-T)  =   "099209903"
                                               OR  "099209905"
                                               OR  "099209906"
      *H24.4 改正
                                               OR  "099209907"
                                               OR  "099209908"
      *H24.4 改正
                   PERFORM 30021-KOUHEN-MEISYO-SEC
                   MOVE    WRK-MEISYO    TO  SPA-GMN-MEISYO (IDX-T)
               END-IF
           END-IF
      *    入力値のある時、再編集
           IF     (SPA-COM-INPUTCD (IDX-T)(1:2)    =   "84" )  OR
                  (SPA-COM-INPUTCD (IDX-T)(1:4)    =   "0084")
                PERFORM 3002-INPUT-MEISYO-SEC
                MOVE    WRK-MEISYO    TO  SPA-GMN-MEISYO (IDX-T)
           END-IF
      *    診療科名埋め込み再編集
           IF     (SPA-COM-INPUTCD (IDX-T)(1:2)    =   "83")  AND
                  (SPA-COM-ATAI1   (IDX-T)     NOT =   SPACE)
      *D          (LNK-SC92-LSTSRYKA           NOT =   SPACE)
                PERFORM 3003-INPUT-MEISYO2-SEC
                MOVE    WRK-MEISYO    TO  SPA-GMN-MEISYO (IDX-T)
           END-IF
      *!
      *    用量割合再編集
           IF     (SPA-COM-INPUTCD (IDX-T)(1:7)    =   "0992000")  AND
                  (SPA-COM-SURYO   (IDX-T)     NOT =   ZERO     )
                PERFORM 3005-INPUT-MEISYO3-SEC
                MOVE    WRK-MEISYO    TO  SPA-GMN-MEISYO (IDX-T)
           END-IF
      *!
      *!
      *    器材商品名コードの発生分
           IF     (SPA-COM-INPUTCD (IDX-T)(1:1)    =   "7"   )  AND
                  (SPA-COM-AUTOKBN2(IDX-T)         =   "1"   )
                PERFORM 3005-INPUT-MEISYO4-SEC
                MOVE    WRK-MEISYO    TO  SPA-GMN-MEISYO (IDX-T)
      *H23.12
      *        器材商品名点数へ変更
               IF      SPA-COM-TEN (IDX-T - 1)  >   ZERO
                   MOVE    SPA-COM-TEN (IDX-T - 1) 
                                           TO  SPA-COM-TEN (IDX-T)
                   MOVE    SPA-COM-TEN (IDX-T - 1) 
                                           TO  SPA-COM-KISOTEN(IDX-T)
               END-IF
           END-IF
      *!
      *
      *    数量単位（薬剤・器材の時）
           IF     (SPA-COM-INPUTCD (IDX-T)(1:1)   =   "6"  OR  "7" ) OR
                  (SPA-COM-INPUTCD (IDX-T)(1:3)   =   "059" )  
      *       単位があるもの（画像診断以外）
              OR ((SPA-COM-INPUTCD (IDX-T)(1:1)   =   "1" )  AND
                  (TNS-TANICD                 NOT =   ZERO ))
      *        画像診断で
               IF     (TNS-SRYKBN             NOT =   "70" )   OR
                     ((TNS-SRYKBN                 =   "70" ) AND
                      (TNS-SRYSYUKBN              =   "700"))
                   PERFORM 3004-TANI-HENSYU-SEC
                   MOVE    WRK-TANINAME    TO  SPA-GMN-TANINAME (IDX-T)
               ELSE
                   MOVE    SPACE           TO  SPA-GMN-TANINAME (IDX-T)
               END-IF
           ELSE
               MOVE    SPACE               TO  SPA-GMN-TANINAME (IDX-T)
           END-IF
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
      *        診療区分 ８０ の変更
               IF      TNS-SRYKBN          =   "80"
                   EVALUATE    TNS-CDKBN-KBN
                       WHEN    "I"
      *                    精神科専門療法
                           MOVE    "830"       TO  SPA-COM-TNSSRYSYUKBN
                                                              (IDX-T)
      *                    放射線治療
                       WHEN    "M"
                           MOVE    "840"       TO  SPA-COM-TNSSRYSYUKBN
                                                              (IDX-T)
                   END-EVALUATE
      *            療養担当手当
                   IF      TNS-SRYCD           =   "199000510"
                                               OR  "199000610"
                       MOVE    "850"           TO  SPA-COM-TNSSRYSYUKBN
                                                              (IDX-T)
                   END-IF
      *            入院料（外来)
                   IF      TNS-SRYCD           =   "190076710"
                       MOVE    "890"           TO  SPA-COM-TNSSRYSYUKBN
                                                              (IDX-T)
                   END-IF
               END-IF
           END-IF
      *    データ区分
           MOVE    TNS-DATAKBN         TO  SPA-COM-DATAKBN   (IDX-T)
      *    診療区分
           MOVE    TNS-SRYKBN          TO  SPA-COM-TNSSRYKBN (IDX-T)
      *    医薬品　項目
           MOVE    TNS-MADOKUKBN       TO  SPA-COM-MADOKUKBN  (IDX-T)
           MOVE    TNS-SINKEIHAKAIKBN  TO  SPA-COM-SINKEIKBN  (IDX-T)
           MOVE    TNS-SEIBUTUGAKUKBN  TO  SPA-COM-SEIBUTUKBN (IDX-T)
           MOVE    TNS-ZOEIZAIKBN      TO  SPA-COM-ZOEIZAIKBN (IDX-T)
           MOVE    TNS-CSYYOURYO       TO  SPA-COM-CSYYOURYO  (IDX-T)
      *    単位コード
           MOVE    TNS-TANICD          TO  SPA-COM-TANICD    (IDX-T)
      *    薬剤区分
           MOVE    TNS-YKZKBN          TO  SPA-COM-YKZKBN    (IDX-T)
      *    剤型区分
           MOVE    TNS-ZAIGATAKBN      TO  SPA-COM-ZAIGATAKBN (IDX-T)
      *    部位区分
           MOVE    TNS-BUIKBN          TO  SPA-COM-BUIKBN   (IDX-T)
      *    検査等実施判断区分
           MOVE    TNS-KNSJISKBN       TO  SPA-COM-KNSJISKBN (IDX-T)
      *    検査等実施判断グループ
           MOVE    TNS-KNSJISGRPKBN    TO  SPA-COM-KNSJISGRPKBN (IDX-T)
      *    注加算コード
           MOVE    TNS-CHUKSNCD        TO  SPA-COM-CHUKSNCD (IDX-T)
      *    注加算通番
           MOVE    TNS-CHUKSNTUBAN     TO  SPA-COM-CHUKSNTUBAN (IDX-T)
      *    通則加算対象外区分
           MOVE    TNS-TUSOKUGAIKBN    TO  SPA-COM-TUSOKUGAIKBN (IDX-T)
      *    通則年齢区分
           MOVE    TNS-TSUSOKUAGE      TO  SPA-COM-TSUSOKUAGE (IDX-T)
      *    時間加算区分
           MOVE    TNS-TIMEKSNKBN      TO  SPA-COM-TIMEKSNKBN (IDX-T)
      *    外来管理加算区分
           MOVE    TNS-GAIKANRIKBN     TO  SPA-COM-GAIKANRIKBN (IDX-T)
      *    検査用　区分番号
           MOVE    TNS-CDKBN-KBN       TO  SPA-COM-CDKBN-KBN (IDX-T)
           MOVE    TNS-CDKBN-SYO       TO  SPA-COM-CDKBN-SYO (IDX-T)
           MOVE    TNS-CDKBN-BU        TO  SPA-COM-CDKBN-BU  (IDX-T)
      *    労災用　区分番号
           MOVE    TNS-CDKBN-KBNNUM    TO  SPA-COM-CDKBN-KBNNUM (IDX-T)
      *    労災用　区分番号枝番
           MOVE    TNS-CDKBN-KBNNUM-EDA
                                       TO  SPA-COM-CDKBN-KBNNUM-EDA
                                                              (IDX-T)
      *    検査用　包括対象検査
           MOVE    TNS-HOUKNSKBN       TO  SPA-COM-HOUKNSKBN (IDX-T)
      *    検査用　検体検査コメント
           MOVE    TNS-KENTAICOMMENT   TO  SPA-COM-KENTAICOMMENT (IDX-T)
      *    算定履歴区分
           MOVE    TNS-SANTEIRRKKBN    TO  SPA-COM-SANTEIRRKKBN (IDX-T)
      *    指導管理料
           MOVE    TNS-SDOKNRYO        TO  SPA-COM-SDOKNRYO (IDX-T)
      *    点数識別
           MOVE    TNS-TENSIKIBETU     TO  SPA-COM-TENSIKIBETU (IDX-T)
      *    減点
           IF     (TNS-TENSIKIBETU     =   8       ) AND
                  (TNS-TEN             >   ZERO    )
               MOVE    1                   TO  SPA-COM-PLUSKBN(IDX-T)
           END-IF
      *    特定器材年齢加算区分
           MOVE    TNS-TOKUKIZAIAGEKSNKBN
                                       TO  SPA-COM-TOKUKIZAIAGEKSNKBN
                                                               (IDX-T)
      *    酸素等区分
           MOVE    TNS-SANSOKBN        TO  SPA-COM-SANSOKBN (IDX-T)
      *    酸素・窒素の時、酸素区分再設定
           IF      TNS-TOKUKIZAISBT1    =   2
               PERFORM 3009-SANSO-CHK-SEC
           END-IF
      *    特定器材種別１
           MOVE    TNS-TOKUKIZAISBT1   TO  SPA-COM-TOKUKIZAISBT1(IDX-T)
      *    上限点数（器材の上限）
           MOVE    TNS-JGNTEN          TO  SPA-COM-JGNTEN (IDX-T)
      *    保険適用区分
           MOVE    TNS-HKNTEKKBN       TO  SPA-COM-HKNTEKKBN  (IDX-T)
      *    点数欄集計先識別
           MOVE    TNS-GAITENTTLKBN    TO  SPA-COM-GAITENTTLKBN (IDX-T)
      *    点数欄集計先識別
           MOVE    TNS-NYUTENTTLKBN    TO  SPA-COM-NYUTENTTLKBN (IDX-T)
      *    履歴算定用診療行為コード
           MOVE    TNS-SRYCD           TO  SPA-COM-CHK-SRYCD (IDX-T)
      *    きざみ値計算識別判定
           MOVE    TNS-KZMCOMPSIKIBETU TO  SPA-COM-KZMCOMPSIKIBETU
                                                             (IDX-T)
      *    きざみ値計上限
           MOVE    TNS-KZMJGN          TO  SPA-COM-KZMJGN (IDX-T)
           MOVE    TNS-KZMKGN          TO  SPA-COM-KZMKGN (IDX-T)
           MOVE    TNS-KZM             TO  SPA-COM-KZM (IDX-T)
           MOVE    TNS-KZMTEN          TO  SPA-COM-KZMTEN (IDX-T)
           MOVE    TNS-KZMERR          TO  SPA-COM-KZMERR (IDX-T)
      *    検査等実施判定区分
           MOVE    TNS-HOUKATUTEIGENKBN    TO  SPA-COM-HOUKATUTEIGENKBN
                                                             (IDX-T)
      *    年齢加算
           PERFORM 3007-AGEKSN-NENREI-SEC
      *    往診区分
           MOVE    TNS-OSINKBN         TO  SPA-COM-OSINKBN   (IDX-T)
      *    告示識別区分
           MOVE    TNS-KOKUJISKBKBN1   TO  SPA-COM-KOKUJISKBKBN1(IDX-T)
           MOVE    TNS-KOKUJISKBKBN2   TO  SPA-COM-KOKUJISKBKBN2(IDX-T)
      *
      *    きざみ値計算識別判定
           IF     (SPA-COM-INPUTCD (IDX-T)(1:1)
                                       =   "1"     )  AND
                  (TNS-KZMCOMPSIKIBETU =   1       )
               PERFORM 3005-SYOCHI-SYUGI-SEC
           END-IF
      *
      *    レセプト用実日数（実日数＝２の日数・回数をセット）
           IF      TNS-JITUDAY         =   2
               MOVE    TNS-DAYCNT      TO  SPA-COM-DAYCNT (IDX-T)
           END-IF
      *
      *    逓減検査区分
           MOVE    TNS-TEIGENKBN       TO  SPA-COM-TEIGENKBN (IDX-T)
      *
      *    後発医薬品区分
           MOVE    TNS-KOUHATUKBN       TO  SPA-COM-KOUHATUKBN (IDX-T)
      *    基準不適合逓減区分
           MOVE    TNS-KIJUNTEIGENKBN   TO  SPA-COM-KIJUNTEIGENKBN
                                                               (IDX-T)
      *    基準不適合逓減対象施設基準
           MOVE    TNS-KIJUNTEIGENCD    TO  SPA-COM-KIJUNTEIGENCD
                                                               (IDX-T)
      *Ｈ１６年４月から
      *    手術施設基準不適合
           MOVE    FLG-SSTKIJUNCD-ERR  TO  SPA-COM-KIJUNCHK-FLG (IDX-T)
      *Ｈ１８年４月から
      *    処置乳幼児加算区分
           MOVE    TNS-LASERKBN        TO  SPA-COM-LASERKBN (IDX-T)
      *    極低出生体重児加算区分
           MOVE    TNS-CHPMESUKSN      TO  SPA-COM-CHPMESUKSN (IDX-T)
      *Ｈ２０年４月から
      *    画像等手術支援加算
           MOVE    TNS-GAZOOPESUP      TO  SPA-COM-GAZOOPESUP (IDX-T)
      *    麻酔識別区分
           MOVE    TNS-MASUISKBKBN     TO  SPA-COM-MASUISKBKBN (IDX-T)
      *
      *    点数マスタ付加情報より編集
           PERFORM 3006-TENSUPLUS-HEN-SEC
      *
      *    点数マスタ薬剤付加情報より編集
           IF     (TNS-SRYCD(1:1)      =   "6" )
               PERFORM 3007-TENSUPLUS2-HEN-SEC
           END-IF
      *
      *    患者禁忌薬剤情報
           IF     (TNS-SRYCD(1:1)      =   "6" )
             AND  (LNK-SC92-PG     NOT =   "K05")
               PERFORM 3008-PTKINKI-CHK-SEC
           END-IF
      *
      *TEST
      *H24.4
      *    処方箋一般名記載区分（告示識別区分を使用）
           IF      SPA-SRYYMD          >=  "20120401"
               IF     (TNS-SRYCD(1:1)      =   "6"  )
                 AND ((TNS-KOUHATUKBN      =   1  OR  2 )  OR
                      (TNSP-IPN-KISAIKBN   =   3    )  OR
                      (TNSP-IPN-KISAIKBN   =   1    ))
      *            後発品・後発のある先発品の最低薬価
                   IF      TNSP-IPN-KISAIKBN   =   1
                                               OR  3
                       MOVE    9               TO  SPA-COM-KOKUJISKBKBN2
                                                               (IDX-T)
                   END-IF
      *            最低金額の編集（きざみ値を流用する）ver.4.7は追加）
      *            最低薬価編集
                   PERFORM 300001-GENERICPRICE-CHEK-SEC
                   MOVE    WRK-CHKTEN      TO  SPA-COM-KZMTEN (IDX-T)
               END-IF
           END-IF
      *!!
           .
       300-TENSU-HENSYU-EXT.
           EXIT.
      *
      *H24.4
      *****************************************************************
      *     最低薬価の編集処理
      *****************************************************************
       300001-GENERICPRICE-CHEK-SEC        SECTION.
      *
           MOVE    TNS-TEN             TO  WRK-CHKTEN
      *    薬価基準コードの最低薬価
           IF      TNS-YAKKAKJNCD      =   SPACE
               GO      TO    300001-GENERICPRICE-CHEK-EXT
           END-IF
      *
           MOVE    SPACE               TO  GENERICPRICE-REC
           INITIALIZE                      GENERICPRICE-REC
           MOVE    TNS-YAKKAKJNCD      TO  GENERICPRICE-YAKKAKJNCD
           MOVE    SPA-SRYYMD          TO  GENERICPRICE-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  GENERICPRICE-YUKOEDYMD
           MOVE    GENERICPRICE-REC    TO  MCPDATA-REC
           MOVE    "tbl_generic_price"
                                      TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_generic_price"
                                          TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  GENERICPRICE-REC
      *            薬価
                   MOVE    GENERICPRICE-PRICE  TO  WRK-CHKTEN
               ELSE
                   INITIALIZE                  GENERICPRICE-REC
                   MOVE    1                   TO  FLG-GENERICPRICE
               END-IF
           ELSE
               INITIALIZE                  GENERICPRICE-REC
               MOVE    1                   TO  FLG-GENERICPRICE
           END-IF
           MOVE    "tbl_generic_price"
                                      TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       300001-GENERICPRICE-CHEK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *     酸素・窒素の時、酸素区分再設定処理
      *****************************************************************
       3009-SANSO-CHK-SEC        SECTION.
      *
           SET     TBL-SANSO           TO  1
           SEARCH      TBL-SANSO-OCC   VARYING   TBL-SANSO
                   AT  END
                       MOVE    ZERO            TO  FLG-SANSO-OK
                   WHEN    TNS-SRYCD       =   TBL-SANSO-SRYCD
                                                           (TBL-SANSO)
                       MOVE    1               TO  FLG-SANSO-OK
           END-SEARCH
      *
           IF      FLG-SANSO-OK        =   1
      *        酸素
               MOVE    1               TO  SPA-COM-SANSOKBN (IDX-T)
           ELSE
      *        窒素
               MOVE    ZERO            TO  SPA-COM-SANSOKBN (IDX-T)
           END-IF
           .
       3009-SANSO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     労災・自賠責 編集処理
      *****************************************************************
       3000-RSIJIB-HEN-SEC        SECTION.
      *
           IF      SPA-COM-INPUTCD (IDX-T)(1:1)    =   "1"
      *        労災のコード
               IF      SPA-COM-INPUTCD (IDX-T)(2:2)    =   "01"
                   MOVE    1               TO  SPA-COM-RSIKBN (IDX-T)
               END-IF
      *        金額を点数へ置換える
               IF     (TNS-TENSIKIBETU     =   1     )
      *         食事以外
               AND    (TNS-SRYKBN      NOT =   "97"  )
                   COMPUTE SPA-COM-KISOTEN  (IDX-T)    =   TNS-TEN
                                                       /   10
               END-IF
           END-IF
      *    労災（器材）
           IF     (SPA-COM-INPUTCD (IDX-T)(1:1)    =   "7"  )  AND
                  (SPA-COM-INPUTCD (IDX-T)(2:5)    =   "88888")
               MOVE    1               TO  SPA-COM-RSIKBN (IDX-T)
           END-IF
      *    労災（コメント）
           IF     (SPA-COM-INPUTCD (IDX-T)(1:1)    =   "8"  )  AND
                  (SPA-COM-INPUTCD (IDX-T)(4:1)    =   "8"  )
               MOVE    1               TO  SPA-COM-RSIKBN (IDX-T)
           END-IF
      *
           .
       3000-RSIJIB-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     薬剤服用コメントの時、再編集処理
      *****************************************************************
       3001-FUKIYO-MEISYO-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-MEISYO
      *
           STRING     "【"             DELIMITED   BY  SIZE
                      TNS-NAME         DELIMITED   BY  "    "
                      "】"             DELIMITED   BY  SIZE
                      INTO             WRK-MEISYO
           END-STRING
           .
       3001-FUKIYO-MEISYO-EXT.
           EXIT.
      *****************************************************************
      *     後発医薬品区分再編集処理
      *****************************************************************
       3002-KOUHATU-MEISYO-SEC        SECTION.
      *
      *H24.4
           MOVE    SPACE               TO  WRK-MSG1
           IF      SPA-SRYYMD          >=  "20120401"
               IF     (TNSP-IPN-KISAIKBN   =   1     )
      *            一般名記載あり
                   MOVE    "般"                TO  WRK-MSG1
               END-IF
               IF     (TNSP-IPN-KISAIKBN   =   3    )
      *            処方名称（一般名）記載
                   MOVE    "般"                TO  WRK-MSG1
               END-IF
           END-IF
      *
           MOVE    TNS-NAME            TO  WRK-MEISYO
           EVALUATE    TNS-KOUHATUKBN
               WHEN    1
      *            後発品（先発品のある後発品）
                   MOVE    SPACE                TO  WRK-MEISYO
                   STRING      "【"             DELIMITED   BY  SIZE
                               WRK-MSG1         DELIMITED   BY  SPACE
                               "後】"           DELIMITED   BY  SIZE
                               TNS-NAME         DELIMITED   BY  SIZE
                               INTO             WRK-MEISYO
                   END-STRING
               WHEN    2
      *            後発品のある先発品
                   MOVE    SPACE               TO  WRK-MEISYO
                   STRING      "【"             DELIMITED   BY  SIZE
                               WRK-MSG1         DELIMITED   BY  SPACE
                               "先】"           DELIMITED   BY  SIZE
                               TNS-NAME         DELIMITED   BY  SIZE
                               INTO             WRK-MEISYO
                   END-STRING
      *??
               WHEN    0
      *            後発品のない先発品で一般名記載区分あり
                   IF      WRK-MSG1        NOT =   SPACE
                       MOVE    SPACE               TO  WRK-MEISYO
                       STRING  "【"             DELIMITED   BY  SIZE
                               WRK-MSG1         DELIMITED   BY  SPACE
                               "】"             DELIMITED   BY  SIZE
                               TNS-NAME         DELIMITED   BY  SIZE
                               INTO             WRK-MEISYO
                       END-STRING
                   END-IF
      *
               WHEN    7
      *            先発品のない後発品
                   MOVE    SPACE                TO  WRK-MEISYO
                   STRING      "【"             DELIMITED   BY  SIZE
                               WRK-MSG1         DELIMITED   BY  SPACE
                               "無】"           DELIMITED   BY  SIZE
                               TNS-NAME         DELIMITED   BY  SIZE
                               INTO             WRK-MEISYO
                   END-STRING
      *??
           END-EVALUATE
      *!!!!
      *ver.4.5（削除かも〜）
      *    換算値あり
           IF     (TNSP-KANZANTANINAME =   SPACE )
               OR (TNSP-KANZANCHI      =   ZERO  )
               CONTINUE
           ELSE
               MOVE    TNSP-KANZANCHI      TO  WRK-SURYO
               PERFORM 2001-SURYO-HEN-SEC
               MOVE    WRK-MEISYO          TO  WRK-MEISYO-2
               MOVE    SPACE               TO  WRK-MEISYO
               STRING  WRK-MEISYO-2        DELIMITED   BY  SPACE
                       "〔"
                       WRK-SURYO-H          DELIMITED   BY  SPACE
                       "／"
                       TNSP-KANZANTANINAME     DELIMITED   BY  SPACE
                       "〕"                    DELIMITED   BY  SIZE
                       INTO                WRK-MEISYO
               END-STRING
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI
               MOVE    WRK-MEISYO          TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF      KANACHK-RC          =   ZERO
                   MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  WRK-MEISYO
                END-IF
           END-IF
      *TEST
      *TEST
           .
       3002-KOUHATU-MEISYO-EXT.
           EXIT.
      *TEST
      *ver.4.5（削除かも〜）
      *****************************************************************
      *    数量表示用編集処理
      *****************************************************************
       2001-SURYO-HEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    SPACE               TO  WRK-SURYO-H
           MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
      *
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING     IDM     FROM    5  BY  -1
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
               IF      WRK-SURYO-X(1:5)    =   SPACE
                   MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1-9
               END-IF
           END-IF
           MOVE    ZERO                TO  IDM2
           PERFORM VARYING     IDM     FROM    1  BY  1
                   UNTIL       IDM     >   11
               IF      WRK-SURYO-X (IDM:1) NOT =   SPACE
                   ADD     1               TO  IDM2
                   MOVE    WRK-SURYO-X (IDM:1) TO  WRK-SURYO-H(IDM2:1)
               END-IF
           END-PERFORM
           .
       2001-SURYO-HEN-EXT.
           EXIT.
      *TEST
      *TEST
      ******************************************************************
      *     後発医薬品変更再編集処理
      *****************************************************************
       30021-KOUHEN-MEISYO-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-MEISYO
      *
           STRING     "【"             DELIMITED   BY  SIZE
                      TNS-NAME         DELIMITED   BY  "    "
                      "】"             DELIMITED   BY  SIZE
                      INTO             WRK-MEISYO
           END-STRING
           .
       30021-KOUHEN-MEISYO-EXT.
           EXIT.
     *
      *****************************************************************
      *     入力値コメントの時、再編集処理
      *****************************************************************
       3002-INPUT-MEISYO-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-MEISYO
           MOVE    TNS-NAME            TO  WRK-MEISYO
           INITIALIZE                  WRK-INPUT-SET-AREA
           INITIALIZE                  WRK-SYOKBN-G
      *
      *    施設基準コード（入力値編集）
           PERFORM VARYING IDX-SST     FROM    1   BY  1
                   UNTIL   IDX-SST     >   8
               MOVE    TNS-SSTKIJUNCD(IDX-SST)
                                      TO  WRK-INPUT-SET (IDX-SST)
           END-PERFORM
      *
           MOVE    ZERO                TO  FLG-INPUT-CHK
      *
      *    入力値を桁数にあわせて数字変換する
           PERFORM VARYING IDX-SST     FROM    1   BY  1
                   UNTIL  (IDX-SST         >   4     )   OR
                          (SPA-ERRCD   NOT =   SPACE )
               MOVE    WRK-INPUT-KETA (IDX-SST)    TO  WRK-KETA
               COMPUTE WRK-KETA2           =   15  -   WRK-KETA   +  1
               IF     (WRK-INPUT-KETA (IDX-SST)      NOT =   ZERO ) AND
                      (SPA-COM-ATAI (IDX-T IDX-SST)  NOT =   SPACE)
                   INITIALIZE                      ORCSNUMAREA
                   MOVE    SPA-COM-ATAI (IDX-T IDX-SST)
                                               TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   IF     (SNUM-RC         NOT =   ZERO  )   OR
                          (SNUM-MINKBN     NOT =   SPACE )   OR
                          (SNUM-SYOKBN     NOT =   SPACE )   OR
                          (SNUM-KETSUU         >   WRK-KETA)
                       MOVE    "0029"          TO  SPA-ERRCD
      ****             ユーザコメントで小数点以下は対象
      *                IF     (SPA-COM-INPUTCD (IDX-T)(1:4) =   "0084")
      *                    IF     (SNUM-RC         NOT =   ZERO  )   OR
      *                           (SNUM-MINKBN     NOT =   SPACE )   OR
      *                          ((SNUM-SEISUU +  SNUM-SYOSUU + 1)
      *                                                >   WRK-KETA )
      *                        CONTINUE
      *                    ELSE
      *                        MOVE    1               TO  FLG-INPUT-CHK
      *                        MOVE    SPACE           TO  SPA-ERRCD
      *                        MOVE    1               TO  WRK-SYOKBN
      *                                                        (IDX-SST)
      *                    END-IF
      *****            END-IF
                   ELSE
                       MOVE    SNUM-OUTEDT(WRK-KETA2:WRK-KETA)
                                               TO  SPA-COM-ATAI
                                                       (IDX-T IDX-SST)
                       IF      SNUM-OUTEDT(WRK-KETA2:WRK-KETA)
                                               NOT =   ZERO
                           MOVE    1               TO  FLG-INPUT-CHK
                       END-IF
                   END-IF
               ELSE
                   MOVE    SPACE           TO  SPA-COM-ATAI
                                                        (IDX-T IDX-SST)
               END-IF
           END-PERFORM
      *
      *    漢字変換後、名称へ
           PERFORM VARYING IDX-SST     FROM    1   BY  1
                   UNTIL  (IDX-SST         >   4     )   OR
                          (WRK-INPUT-KETA (IDX-SST)  =   ZERO)
                      OR  (SPA-ERRCD             NOT =   SPACE)
               IF      SPA-COM-ATAI(IDX-T IDX-SST)   =   SPACE
                   CONTINUE
               ELSE
                   IF      WRK-SYOKBN (IDX-SST)    =   ZERO
      *                数値
                       PERFORM 300211-SUCHIHEN-SEC
                   ELSE
      *                文字（小数点以下あり）
                       PERFORM 300212-MOJIHEN-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
      *    埋め込み値がすべてゼロの場合、エラー
           IF     (FLG-INPUT-CHK       =   ZERO )  AND
                  (SPA-ERRCD           =   SPACE)  AND
                 ((SPA-COM-ATAI1(IDX-T)    NOT =   SPACE )  OR
                  (SPA-COM-ATAI2(IDX-T)    NOT =   SPACE )  OR
                  (SPA-COM-ATAI3(IDX-T)    NOT =   SPACE )  OR
                  (SPA-COM-ATAI4(IDX-T)    NOT =   SPACE ))
               MOVE    "2001"              TO  SPA-ERRCD
           END-IF
      *
           .
       3002-INPUT-MEISYO-EXT.
           EXIT.
      *
      *****************************************************************
      *     数字変換処理
      *****************************************************************
       300211-SUCHIHEN-SEC            SECTION.
      *
           MOVE    WRK-INPUT-KETA (IDX-SST)    TO  WRK-KETA
           MOVE    WRK-INPUT-ICHI (IDX-SST)    TO  WRK-ICHI
           MOVE    ZERO                TO  WRK-ATAI-X
           COMPUTE IDX-SS2             =   16   -   WRK-KETA
           MOVE    SPA-COM-ATAI(IDX-T IDX-SST)(1:WRK-KETA)
                                       TO  WRK-ATAI-X
                                                   (IDX-SS2:WRK-KETA)
           MOVE    WRK-ATAI-9          TO  WRK-ZZZ9
      *
           PERFORM 30021-KANJIHEN-SEC
           COMPUTE WRK-KETA            =   WRK-KETA  *   2
           COMPUTE IDX-K               =   (16       -   WRK-KETA)
                                       +   1
           COMPUTE WRK-ICHI2           =   (WRK-ICHI     *  2  )
                                           -   1
           MOVE    WRK-KANJI(IDX-K:)   TO  WRK-MEISYO
                                                 (WRK-ICHI2:WRK-KETA)
           .
       300211-SUCHIHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     数字変換処理
      *****************************************************************
       300212-MOJIHEN-SEC            SECTION.
      *
           MOVE    WRK-INPUT-KETA (IDX-SST)    TO  WRK-KETA
           MOVE    WRK-INPUT-ICHI (IDX-SST)    TO  WRK-ICHI
      *
           MOVE    SPACE               TO  WRK-ATAI-XX
           MOVE    WRK-KETA            TO  IDX-S1
           PERFORM VARYING     IDX-S2  FROM    8   BY  -1
                   UNTIL       IDX-S2      <   1
               IF     (SPA-COM-ATAI(IDX-T IDX-SST)(IDX-S2:1)
                                           NOT =   SPACE )
                  OR  (IDX-S1              <   WRK-KETA    )
                   MOVE    SPA-COM-ATAI(IDX-T IDX-SST)(IDX-S2:1)
                                           TO  WRK-ATAI-XX(IDX-S1:1)
                   COMPUTE IDX-S1          =   IDX-S1   -   1
               END-IF
           END-PERFORM
      *    漢字変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-ATAI-XX         TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-KANJI
           END-IF
      *
           COMPUTE WRK-KETA            =   WRK-KETA    *   2
           COMPUTE WRK-ICHI2           =  (WRK-ICHI    *   2  )
                                       -   1
           MOVE    WRK-KANJI(1:WRK-KETA)   TO  WRK-MEISYO
                                                 (WRK-ICHI2:WRK-KETA)
           .
       300212-MOJIHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     数字漢字変換処理
      *****************************************************************
       30021-KANJIHEN-SEC            SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-ZZZ9            TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-KANJI
               GO      TO      30021-KANJIHEN-EXT
           END-IF
      *    変換できなかったもの
           MOVE    SPACE               TO  WRK-KANJI
           PERFORM VARYING     IDX-KNJ2    FROM    1   BY  1
                   UNTIL       IDX-KNJ2    >   5
               IF      WRK-ZZZ9-X (IDX-KNJ2:1)  NUMERIC
                   MOVE    WRK-ZZZ9-X (IDX-KNJ2:1) TO  IDX-KNJ1
                   IF      IDX-KNJ1         =   ZERO
                       MOVE    10                  TO  IDX-KNJ
                   ELSE
                       MOVE    IDX-KNJ1            TO  IDX-KNJ
                   END-IF
                   MOVE    TBL-SUJI (IDX-KNJ)  TO  WRK-KANJI-X
                                                      (IDX-KNJ2)
               END-IF
           END-PERFORM
           .
       30021-KANJIHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     入力値コメントの時、再編集処理
      *****************************************************************
       3003-INPUT-MEISYO2-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-MEISYO
           MOVE    TNS-NAME            TO  WRK-MEISYO
           INITIALIZE                  WRK-INPUT-SET-AREA
      *
      *    初診料算定科、再診料算定科のみ対象とする
           IF      SPA-COM-INPUTCD (IDX-T) =   "830000020"  OR
                                               "830000021"
               CONTINUE
           ELSE
               GO      TO      3003-INPUT-MEISYO2-EXT
            END-IF
      *
      *    施設基準コード（入力値編集）
           PERFORM VARYING IDX-SST     FROM    1   BY  1
                   UNTIL   IDX-SST     >   8
               MOVE    TNS-SSTKIJUNCD(IDX-SST)
                                      TO  WRK-INPUT-SET (IDX-SST)
           END-PERFORM
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
                   PERFORM 30031-SRYKA-MEI-HEN-SEC
      *************MOVE    LNK-SC92-LSTSRYKAMEI  TO  WRK-MEISYO
                   MOVE    WRK-SRYKAMEI          TO  WRK-MEISYO
                                                       (WRK-ICHI2:)
               END-IF
           END-PERFORM
      *
           .
       3003-INPUT-MEISYO2-EXT.
           EXIT.
      *
      *****************************************************************
      *     用量割合再編集処理
      *****************************************************************
       3005-INPUT-MEISYO3-SEC        SECTION.
      *
           INITIALIZE                  ORCSMEIHENAREA
           MOVE    "1"                     TO  ORCSMEIHEN-KBN
           MOVE    SPA-COM-SURYO (IDX-T)   TO  ORCSMEIHEN-SURYO
           MOVE    SPA-COM-INPUTCD(IDX-T)  TO  ORCSMEIHEN-SRYCD
           MOVE    TNS-NAME            TO  ORCSMEIHEN-TNS-NAME
           CALL    "ORCSMEIHEN"        USING
                                       SPA-AREA
                                       ORCSMEIHENAREA
           MOVE    ORCSMEIHEN-MEISYO   TO  WRK-MEISYO
      *
           .
       3005-INPUT-MEISYO3-EXT.
           EXIT.
      *****************************************************************
      *    器材商品名コードの器材編集
      *****************************************************************
       3005-INPUT-MEISYO4-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-MEISYO
      *
           STRING     "〔"             DELIMITED   BY  SIZE
                      TNS-NAME         DELIMITED   BY  "    "
                      "〕"             DELIMITED   BY  SIZE
                      INTO             WRK-MEISYO
           END-STRING
           .
       3005-INPUT-MEISYO4-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療科名称編集処理
      *****************************************************************
       30031-SRYKA-MEI-HEN-SEC            SECTION.
      *
      *    診療科
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-COM-ATAI(IDX-T IDX-SST)(1:2)
                                       TO  SYS-1005-KBNCD (1:2)
           MOVE    SPA-SRYYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1005-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1005-REC
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           MOVE    SPACE               TO  WRK-SRYKAMEI
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC           TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME1   TO  WRK-SRYKAMEI
           END-IF
      *
           .
       30031-SRYKA-MEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     数量単位編集処理
      *****************************************************************
       3004-TANI-HENSYU-SEC            SECTION.
      *
           MOVE    TNS-TANINAME(1:2)     TO  WRK-TANINAME
      *
           EVALUATE    TNS-TANICD
      *            セット
               WHEN    009
                   MOVE    "Set"               TO  WRK-TANINAME
      *            トローチ
               WHEN    013
                   MOVE    TNS-TANINAME(1:2) TO  WRK-TANINAME
      *            アンプル
               WHEN    014
                   MOVE    "A"                 TO  WRK-TANINAME
      *            カプセル
               WHEN    015
                   MOVE    "Cap"               TO  WRK-TANINAME
      *            バイアル
               WHEN    039
                   MOVE    "B"                 TO  WRK-TANINAME
      *            キット
               WHEN    051
                   MOVE    "Kit"               TO  WRK-TANINAME
      *            フィート
               WHEN    030
                   MOVE    "FT"                TO  WRK-TANINAME
      *            ｍｇ
               WHEN    032
                   MOVE    "mg"                TO  WRK-TANINAME
               WHEN    034
                   MOVE    "Kg"                TO  WRK-TANINAME
               WHEN    035
                   MOVE    "cc"                TO  WRK-TANINAME
               WHEN    036
                   MOVE    "mL"                TO  WRK-TANINAME
               WHEN    038
                   MOVE    "mLV"               TO  WRK-TANINAME
               WHEN    040
                   MOVE    "cm"                TO  WRK-TANINAME
               WHEN    041
                   MOVE    "cm2"               TO  WRK-TANINAME
      ****     WHEN    042
      ****         MOVE    "m"                 TO  WRK-TANINAME
               WHEN    043
                   MOVE    "uCi"               TO  WRK-TANINAME
               WHEN    044
                   MOVE    "mCi"               TO  WRK-TANINAME
               WHEN    045
                   MOVE    "ug"                TO  WRK-TANINAME
               WHEN    048
                   MOVE    "GBq"                TO  WRK-TANINAME
               WHEN    049
                   MOVE    "MBq"                TO  WRK-TANINAME
               WHEN    050
                   MOVE    "KBq"                TO  WRK-TANINAME
               WHEN    058
                   MOVE    "mLg"                TO  WRK-TANINAME
           END-EVALUATE
           .
       3004-TANI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *     きざみ値計算・編集処理
      *****************************************************************
       3005-SYOCHI-SYUGI-SEC            SECTION.
      *
      *    きざみ値計算
           MOVE    SPA-COM-SURYO (IDX-T)   TO  WRK-SURYO
           MOVE    ZERO                    TO  WRK-KEI-TEN2
      *    きざみ値判定
           EVALUATE    TRUE
      *            データ≦（下限値−きざみ値）
               WHEN    WRK-SURYO       <=  (SPA-COM-KZMKGN(IDX-T)
                                        -   SPA-COM-KZM   (IDX-T) )
                   IF      SPA-COM-KZMERR (IDX-T)  =   2  OR  3
                       MOVE    "0021"          TO  SPA-ERRCD
                   END-IF
      *
      *            下限値＜データ≦上限値
               WHEN     (SPA-COM-KZMKGN (IDX-T)    <   WRK-SURYO )   AND
                        (SPA-COM-KZMJGN (IDX-T)    >=  WRK-SURYO )
      *            点数計算式１
                   COMPUTE WRK-KEI-TEN1    =  (WRK-SURYO
                                           -   SPA-COM-KZMKGN (IDX-T))
                                           /   TNS-KZM
                                           +   0.999
                   MOVE    WRK-KEI-TEN1    TO  WRK-KEI-TEN2
                   COMPUTE WRK-KEI-TEN     =   SPA-COM-TEN (IDX-T)
                                           + ( WRK-KEI-TEN2
                                           *   SPA-COM-KZMTEN (IDX-T))
                   COMPUTE WRK-KEI-TEN2    =   WRK-KEI-TEN    +  0.5
      *
      *            上限値＜データ
               WHEN    SPA-COM-KZMJGN (IDX-T)  <   WRK-SURYO
                   IF      SPA-COM-KZMERR (IDX-T)  =   0  OR  2
      *            警告エラーとする
                       MOVE    "K030"          TO  SPA-ERRCD
      *            点数計算式１
                       COMPUTE WRK-KEI-TEN1    =  (WRK-SURYO
                                               -   SPA-COM-KZMKGN
                                                               (IDX-T))
                                               /   SPA-COM-KZM (IDX-T)
                                               +   0.999
                       MOVE    WRK-KEI-TEN1    TO  WRK-KEI-TEN2
                       COMPUTE WRK-KEI-TEN     =   SPA-COM-TEN (IDX-T)
                                               + ( WRK-KEI-TEN2
                                               *   SPA-COM-KZMTEN
                                                               (IDX-T))
                       COMPUTE WRK-KEI-TEN2    =   WRK-KEI-TEN    +  0.5
                   ELSE
      *            点数計算式２
                       COMPUTE WRK-KEI-TEN1    =  (SPA-COM-KZMJGN(IDX-T)
                                               -   SPA-COM-KZMKGN
                                                                (IDX-T))
                                               /   SPA-COM-KZM  (IDX-T)
                                               +   0.999
                       MOVE    WRK-KEI-TEN1    TO  WRK-KEI-TEN2
                       COMPUTE WRK-KEI-TEN     =   SPA-COM-TEN (IDX-T)
                                               + ( WRK-KEI-TEN2
                                               *   SPA-COM-KZMTEN
                                                               (IDX-T))
                       COMPUTE WRK-KEI-TEN2    =   WRK-KEI-TEN  +  0.5
                   END-IF
           END-EVALUATE
      *
      *    数量は整数であること
           MOVE    SPA-COM-SURYO (IDX-T)   TO  WRK-SURYO
           IF      WRK-SURYO-S2        >   ZERO
               MOVE    "0004"              TO  SPA-ERRCD
           END-IF
      *
           IF    ((SPA-ERRCD           =   SPACE )  OR
                  (SPA-ERRCD(1:1)      =   "K"   )) AND
                  (WRK-KEI-TEN2        >   ZERO  )
               MOVE    WRK-KEI-TEN2    TO  SPA-COM-KISOTEN  (IDX-T)
           END-IF
      *
      *    減点分は数量に減点を入力
           IF     (SPA-COM-TENSIKIBETU (IDX-T) =   7 )
             AND  (SPA-COM-TEN (IDX-T)     =   ZERO  )
               MOVE    SPA-COM-SURYO (IDX-T)   TO  SPA-COM-KISOTEN
                                                              (IDX-T)
               MOVE    1                   TO  SPA-COM-PLUSKBN(IDX-T)
           ELSE
               MOVE    ZERO                TO  SPA-COM-PLUSKBN(IDX-T)
           END-IF
           .
       3005-SYOCHI-SYUGI-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢加算コードの年齢編集処理
      *****************************************************************
       3007-AGEKSN-NENREI-SEC          SECTION.
      *
           MOVE    LNK-SC92-ERRCD5     TO  WRK-LNK-SC92-ERRCD5
           MOVE    ZERO                TO  IDX4
           PERFORM VARYING     IDX3    FROM    1   BY  1
                   UNTIL      (IDX3        >   4       )   OR
                              (TNS-AGEKSNSRYCD(IDX3)
                                          =   SPACE  OR  ZERO )
               MOVE    SPACE                  TO  LNK-SC92-ERRCD5
               IF     (TNS-AGEKSNKGN (IDX3)   =   SPACE  OR  ZERO )  AND
                      (TNS-AGEKSNJGN (IDX3)   =   SPACE  OR  ZERO )
                   CONTINUE
               ELSE
                   MOVE    TNS-AGEKSNKGN (IDX3)   TO  WRK-TNS-KGNAGE
                   MOVE    TNS-AGEKSNJGN (IDX3)   TO  WRK-TNS-JGNAGE
                   PERFORM 2003-NENREI-CHK-SEC
               END-IF
               IF      LNK-SC92-ERRCD5        =   SPACE
                   ADD     1                  TO  IDX4
                   MOVE    TNS-AGEKSNTBL(IDX3)
                                              TO  SPA-COM-AGEKSNTBL
                                                         (IDX-T IDX4)
               END-IF
           END-PERFORM
           MOVE    WRK-LNK-SC92-ERRCD5     TO  LNK-SC92-ERRCD5
           .
       3007-AGEKSN-NENREI-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ付加情報より編集処理
      *****************************************************************
       3006-TENSUPLUS-HEN-SEC          SECTION.
      *
      *    MOVE    SPACE               TO  TENSUPLUS-REC
      *    INITIALIZE                      TENSUPLUS-REC
      *    MOVE    TNS-SRYCD           TO  TNSP-SRYCD
      *    MOVE    TNS-YUKOSTYMD       TO  TNSP-YUKOSTYMD
      *    MOVE    TNS-YUKOEDYMD       TO  TNSP-YUKOEDYMD
      *
      *    MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
      *    MOVE    "tbl_tensuplus"     TO  MCP-TABLE
      *    MOVE    "key"               TO  MCP-PATHNAME
      *    PERFORM 910-DBSELECT-SEC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    "tbl_tensuplus"     TO  MCP-TABLE
      *        MOVE    "key"               TO  MCP-PATHNAME
      *        PERFORM 980-TENSUPLUS-READ-SEC
      *    ELSE
      *        MOVE    1                   TO  FLG-TENSUPLUS
      *        INITIALIZE                      TENSUPLUS-REC
      *    END-IF
      *    MOVE    "tbl_tensuplus"     TO  MCP-TABLE
      *    MOVE    "key"               TO  MCP-PATHNAME
      **** PERFORM 990-DBCLOSE-SEC
      *
      *    採血料区分
           MOVE    TNSP-SAIKETUKBN     TO  SPA-COM-SAIKETUKBN (IDX-T)
      *    前回検査日コメント
           MOVE    TNSP-ZENKNSKBN      TO  SPA-COM-ZENKNSKBN (IDX-T)
      *    入力チェック区分
           MOVE    TNSP-INPCHKKBN      TO  SPA-COM-INPCHKKBN  (IDX-T)
      *ver.4.5
      *    数量換算単位コード
           MOVE    TNSP-KANZANTANICD   TO  SPA-COM-KANZANTANICD (IDX-T)
      *    数量換算単位名称
      *    MOVE    TNSP-KANZANTANINAME TO  SPA-COM-KANZANTANINAME(IDX-T)
      *    数量換算値
           MOVE    TNSP-KANZANCHI      TO  SPA-COM-KANZANCHI (IDX-T)
      *ver.4.5
           .
       3006-TENSUPLUS-HEN-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ薬剤付加情報より編集処理
      *****************************************************************
       3007-TENSUPLUS2-HEN-SEC          SECTION.
      *
           PERFORM 9102-TENSUPLUS2-READ-SEC
      *
           IF      FLG-TENSUPLUS2      =   ZERO
      *        内服算定
               MOVE    TNSP2-NAIFUKUKBN    TO  SPA-COM-NAIFUKUKBN
                                                              (IDX-T)
           END-IF
      *    セット登録でのチェックなし
           IF      (LNK-SC92-PG    NOT =   "K05" ) 
             AND   (FLG-TENSUPLUS2     =   ZERO  )
      *        長期投与日数
               IF     (TNSP2-CHOKI-KIGEN   =   SPACE )  OR
                      (TNSP2-CHOKI-KIGEN   >=  SPA-SRYYMD)
                   MOVE    TNSP2-CHOKI-NISSU   TO  SPA-COM-CHOKINISSU
                                                              (IDX-T)
                   MOVE    TNSP2-CHOKIERR      TO  SPA-COM-CHOKIERR
                                                              (IDX-T)
               END-IF
               MOVE    ZERO                TO  IDX-T2
               MOVE    TNSP2-TOUYOERR      TO  SPA-COM-TOUYOERR (IDX-T)
               PERFORM VARYING     IDX-T3  FROM    1   BY  1
                       UNTIL       IDX-T3  >   10
                   IF     (TNSP2-TOUYOKGNAGE (IDX-T3) =   ZERO )  AND
                          (TNSP2-TOUYOJGNAGE (IDX-T3) =   ZERO )  AND
                          (TNSP2-TOUYORYO1   (IDX-T3) =   ZERO )  AND
                          (TNSP2-TOUYORYO2   (IDX-T3) =   ZERO )
                       CONTINUE
                   ELSE
                       ADD     1               TO  IDX-T2
                       MOVE    TNSP2-TOUYOKGNAGE (IDX-T3)
                                           TO  SPA-COM-TOUYOKGNAGE
                                                         (IDX-T IDX-T2)
                       MOVE    TNSP2-TOUYOJGNAGE (IDX-T3)
                                           TO  SPA-COM-TOUYOJGNAGE
                                                         (IDX-T IDX-T2)
                       MOVE    TNSP2-TOUYORYO1 (IDX-T3)
                                           TO  SPA-COM-TOUYORYO1
                                                         (IDX-T IDX-T2)
                       MOVE    TNSP2-TOUYORYO2 (IDX-T3)
                                           TO  SPA-COM-TOUYORYO2
                                                         (IDX-T IDX-T2)
                   END-IF
               END-PERFORM
      *        月上限回数
               MOVE    TNSP2-TUKIJGNKAISU  TO  SPA-COM-TUKIJGNKAISU
                                                              (IDX-T)
      *        月総投与量
               MOVE    TNSP2-TUKIJGNTOUYORYO
                                           TO  SPA-COM-TUKIJGNTOUYORYO
                                                              (IDX-T)
           END-IF
           .
       3007-TENSUPLUS2-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算行追加編集処理
      *****************************************************************
       400-GYOINSN-SEC                 SECTION.
      *
           IF      SPA-GMN-INPUTCD (200)   NOT =   SPACE
               MOVE    "0007"          TO  SPA-ERRCD
               GO      TO      400-GYOINSN-EXT
           END-IF
      *
      *    加算行を開ける（対象行の次に追加）
           MOVE    ZERO                TO  IDX2
           PERFORM VARYING     IDX1    FROM    SPA-MAX-LINE  BY  -1
                   UNTIL       IDX1    <   IDX-T
               IF      SPA-GMN-INPUTCD (IDX1)  NOT =   SPACE
                   COMPUTE IDX2            =   IDX1    +   1
                   MOVE    SPA-COM-TBLREC (IDX1)
                                           TO  SPA-COM-TBLREC (IDX2)
                   MOVE    SPA-GMN-TBLREC (IDX1)
                                           TO  SPA-GMN-TBLREC (IDX2)
                   INITIALIZE              SPA-COM-TBLREC (IDX1)
                   INITIALIZE              SPA-GMN-TBLREC (IDX1)
               END-IF
           END-PERFORM
      *
           IF      SPA-COM-INPUTCD(IDX-T)  NOT =   SPACE
               MOVE    "0020"              TO  SPA-ERRCD
           ELSE
               MOVE    LNK-SC92-SRYCD      TO  SPA-COM-INPUTCD(IDX-T)
               MOVE    LNK-SC92-SRYCD      TO  SPA-GMN-INPUTCD(IDX-T)
           END-IF
           .
       400-GYOINSN-EXT.
           EXIT.
      *
      *----(01.00.02) LINE ADD START ----------------------------------
      *
      *****************************************************************
      *    入力コードの主表示コードに編集処理
      *****************************************************************
       400-INPUTCD-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  SPA-COM-ICD-INPUTCD(IDX-T)
      *
           INITIALIZE                      INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    SPA-COM-INPUTCD(IDX-T)
                                       TO  ICD-SRYCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM                     920-INPUTCD-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-INPUTCD         =   ZERO
               MOVE    ICD-INPUTCD     TO  SPA-COM-ICD-INPUTCD(IDX-T)
           END-IF
      *
           .
       400-INPUTCD-HEN-EXT.
           EXIT.
      *----(01.00.02) LINE ADD END   ----------------------------------
      *
      *****************************************************************
      *    入力コードの主表示コードに編集処理
      *****************************************************************
       500-GMN-HEN-SEC                SECTION.
      *
      *    数量・回数等の初期化
           MOVE    1                   TO  SPA-COM-SURYO  (IDX-T)
           MOVE    SPACE               TO  SPA-COM-SURFLG (IDX-T)
           MOVE    1                   TO  SPA-COM-BUNGSU (IDX-T)
           MOVE    1                   TO  SPA-COM-KAISU  (IDX-T)
      *
      *    画面再編集・基本チェック
           INITIALIZE                  ORCSC94AREA
           MOVE    "1"                 TO  LNK-SC94-KBN
           MOVE    LNK-SC92-PG         TO  LNK-SC94-PG
           MOVE    IDX-T               TO  LNK-SC94-IDX
           CALL    "ORCSC94"           USING
                                       ORCSC94AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *
           .
       500-GMN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量・回数のみ変更の時の編集処理
      *****************************************************************
       300-SURYOHEN-SEC         SECTION.
      *
      *    入力値チェック
           IF      LNK-SC92-INFLG      =   "1"
               MOVE    SPA-GMN-MEISYO(IDX-T)
                                       TO  TNS-NAME
               MOVE    SPA-COM-KZMCOMPSIKIBETU(IDX-T)
                                       TO  TNS-KZMCOMPSIKIBETU
               MOVE    SPA-COM-DATAKBN (IDX-T)
                                       TO  TNS-DATAKBN
      *        換算値用
               MOVE    SPA-COM-KANZANCHI (IDX-T)
                                       TO  TNSP-KANZANCHI
               MOVE    SPA-COM-KANZANTANICD (IDX-T)
                                       TO  TNSP-KANZANTANICD
               MOVE    SPA-COM-TANICD (IDX-T)
                                       TO  TNS-TANICD
      *
               PERFORM 200-INPUT-SURYO-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *
      *    回数が１以上の時、算定回数チェック
             IF     (SPA-COM-INPUTCD(IDX-T)(1:1)     =   "1" )  AND
                  (SPA-COM-SRYKBN (IDX-T)(1:1) NOT =  "9"  )  AND
                 ((SPA-COM-KAISU  (IDX-T)          >  1    )
             OR   (SPA-COM-SURYO  (IDX-T)          >  1    ))
      *
      *        IF     (SPA-COM-SANTEIRRKKBN (IDX-T)    =   ZERO)  OR
      *               (SPA-COM-HKNCOMBI (IDX-T)    =   "9999" )  OR
      *               (SPA-HKNCOMBINUM             =   "9999" )
      *            CONTINUE
      *        ELSE
      *            MOVE    LNK-SC92-SRYCD      TO  TNS-SRYCD
      *            PERFORM 910-TENSU-READ-SEC
      *            算定回数チェック
      *            IF     (TNS-SANTEIRRKKBN    =   ZERO   )
      *                CONTINUE
      *            ELSE
      *                PERFORM 2001-SANTEI-CHK-SEC
      *            END-IF
      *?       END-IF
      *        算定履歴・併用チェッククリア
               MOVE    SPACE               TO  SPA-COM-HCHKFLG(IDX-T)
            END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *
      *    入力値のある時、再編集
           IF     (SPA-COM-INPUTCD (IDX-T)(1:2)    =   "84" )  OR
                  (SPA-COM-INPUTCD (IDX-T)(1:4)    =   "0084")
               MOVE    LNK-SC92-SRYCD      TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU           =   ZERO
                   PERFORM 3002-INPUT-MEISYO-SEC
                   MOVE    WRK-MEISYO    TO  SPA-GMN-MEISYO (IDX-T)
               ELSE
                   MOVE    "0001"              TO  SPA-ERRCD
               END-IF
           END-IF
      *    診療科名埋め込み再編集
           IF     (SPA-COM-INPUTCD (IDX-T)(1:2)    =   "83")  AND
                  (SPA-COM-ATAI1   (IDX-T)     NOT =   SPACE)
               MOVE    LNK-SC92-SRYCD      TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU           =   ZERO
                    PERFORM 3003-INPUT-MEISYO2-SEC
                    MOVE    WRK-MEISYO    TO  SPA-GMN-MEISYO (IDX-T)
               ELSE
                   MOVE    "0001"              TO  SPA-ERRCD
               END-IF
           END-IF
      *!
      *    用量割合再編集
           IF     (SPA-COM-INPUTCD (IDX-T)(1:7)    =   "0992000")  AND
                  (SPA-COM-SURYO   (IDX-T)     NOT =   ZERO     )
               MOVE    LNK-SC92-SRYCD      TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU           =   ZERO
                   PERFORM 3005-INPUT-MEISYO3-SEC
                   MOVE    WRK-MEISYO      TO  SPA-GMN-MEISYO (IDX-T)
               ELSE
                   MOVE    "0001"              TO  SPA-ERRCD
               END-IF
           END-IF
      *!
      *
      *    きざみ値計算識別判定
           IF     (SPA-COM-INPUTCD (IDX-T)(1:1)
                                       =   "1"     )  AND
                  (SPA-COM-KZMCOMPSIKIBETU (IDX-T) =  1 )
               MOVE    LNK-SC92-SRYCD      TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU           =   ZERO
      *            ＳＰＡ編集
                   PERFORM 300-TENSU-HENSYU-SEC
               ELSE
                   MOVE    "0001"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           END-IF
      *
           .
       300-SURYOHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    器材商品名コード処理
      *****************************************************************
       400-SYOHINMEI-SEC         SECTION.
      *
      *    商品金額編集
           MOVE    TNS-TEN             TO  SPA-COM-TEN    (IDX-T)
      *
           IF     (IDX-T               <   SPA-MAX-LINE )  AND
                  (SPA-COM-AUTOKBN2(IDX-T + 1)  =   "1" )  AND
                  (SPA-COM-INPUTCD (IDX-T + 1)  =   TNS-IDOKANREN)
      *        自動発生済み
               CONTINUE
           ELSE
      *        下に器材コードを自動発生する
               IF     (IDX-T               <   SPA-MAX-LINE )  AND
                     ((SPA-COM-AUTOKBN2(IDX-T + 1)  =   "1" )
                   OR (SPA-COM-INPUTCD (IDX-T + 1)  =   TNS-IDOKANREN))
                   ADD     1               TO  IDX-T
               ELSE
      *            行挿入
                   ADD     1               TO  IDX-T
                   PERFORM 400-GYOINSN-SEC
               END-IF
               IF      SPA-ERRCD           =   SPACE
                   MOVE    TNS-IDOKANREN   TO  LNK-SC92-SRYCD
                   MOVE    TNS-IDOKANREN   TO  SPA-COM-INPUTCD(IDX-T)
                   MOVE    TNS-IDOKANREN   TO  SPA-GMN-INPUTCD(IDX-T)
                   MOVE    "1"             TO  SPA-COM-AUTOKBN2 (IDX-T)
      *            基本編集・併用チェック
                   PERFORM 200-KIHON-CHK-SEC
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 300-KIHON-HEN-SEC
                   END-IF
                   MOVE    SPA-COM-IN (IDX-T - 1)
                                           TO  SPA-COM-IN       (IDX-T)
                   MOVE    SPA-COM-IN2(IDX-T - 1)
                                           TO  SPA-COM-IN2      (IDX-T)
                   MOVE    IDX-T           TO  LNK-SC92-GMN-IDX
               END-IF
           END-IF
           .
       400-SYOHINMEI-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-KANRIMST-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
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
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
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
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *!
      *    検査正式名称
           IF      SPA-FORMALFLG       =   "1"
               IF     (TNS-SRYCD(1:1)      =   "1") AND
                      (TNS-SRYKBN          =   "60"
                                           OR  "64" )  AND
                      (TNS-FORMALNAME  NOT =   SPACE )  AND
                      (TNS-FORMALNAME  NOT =   TNS-NAME )
                   MOVE    TNS-FORMALNAME  TO  TNS-NAME
               END-IF
           END-IF
      *!!
      *    医療観察コード名称編集
           IF      (TNS-SRYCD(1:1)     =   "1"  )  AND
                   (TNS-IRYOKANSATUKBN =   4    )
      *        医療観察区分
               MOVE    SPACE               TO  WRK-MEISYO
               STRING  "【医観】"          DELIMITED   BY  SIZE
                      TNS-NAME             DELIMITED   BY  SPACE
                      INTO             WRK-MEISYO
               END-STRING
               MOVE    WRK-MEISYO          TO  TNS-NAME
           END-IF
      *!!
      *!
      *    附加情報の上限回数設定
           IF     (TNS-SRYCD(1:1)      =   "1"
                                       OR  "6"  ) AND
                  (FLG-TENSU           =   ZERO)
               PERFORM 9101-TENSUPLUS-SYORI-SEC
      *H24.4
      *        システム管理の変更可によって一般名記載とする
               IF     (SPA-SRYYMD          >=  "20120401"  )
                 AND  (SPA-KOUHATUKA       =   "1"         )
                 AND  (TNS-SRYCD(1:1)      =   "6"         )
                   IF     (TNSP-IPN-KISAIKBN   =   2    )
                   OR     (TNS-KOUHATUKBN      =   ZERO )
                   OR     (TNSP-IPN-KISAIKBN   =   3    )
      *                処方名記載、先発品は変更なし
                       CONTINUE
                   ELSE
                       MOVE    1               TO  TNSP-IPN-KISAIKBN
                   END-IF
               END-IF
           ELSE
               MOVE    SPACE               TO  TENSUPLUS-REC
               INITIALIZE                      TENSUPLUS-REC
           END-IF
          .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ付加情報より編集処理
      *****************************************************************
       9101-TENSUPLUS-SYORI-SEC          SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           INITIALIZE                      TENSUPLUS-REC
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
           MOVE    TNS-YUKOSTYMD       TO  TNSP-YUKOSTYMD
           MOVE    TNS-YUKOEDYMD       TO  TNSP-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 980-TENSUPLUS-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSUPLUS
               INITIALIZE                      TENSUPLUS-REC
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    上限回数
           IF      TNSP-SANTEIRRKKBN   =   1
               MOVE    TNSP-SANTEIRRKKBN   TO  TNS-SANTEIRRKKBN
               MOVE    TNSP-JGNCNT         TO  TNS-JGNCNT
               MOVE    TNSP-JGNCNT1D       TO  TNS-JGNCNT1D
               MOVE    TNSP-JGNCNTERR      TO  TNS-JGNCNTERR
      *
               MOVE    TNSP-JGNCNTETCM     TO  TNS-JGNCNTETCM
               MOVE    TNSP-JGNCNTETC      TO  TNS-JGNCNTETC
           END-IF
      *
           .
       9101-TENSUPLUS-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ薬剤付加情報より編集処理
      *****************************************************************
       9102-TENSUPLUS2-READ-SEC          SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS2-REC
           INITIALIZE                      TENSUPLUS2-REC
           MOVE    TNS-SRYCD           TO  TNSP2-SRYCD
      *
           MOVE    SPA-HOSPNUM         TO  TNSP2-HOSPNUM
           MOVE    TENSUPLUS2-REC      TO  MCPDATA-REC
           MOVE    "tbl_tensuplus2"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensuplus2"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TENSUPLUS2-REC
                   MOVE    ZERO                TO  FLG-TENSUPLUS2
               ELSE
                   MOVE    1                   TO  FLG-TENSUPLUS2
                   INITIALIZE                      TENSUPLUS2-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSUPLUS2
               INITIALIZE                      TENSUPLUS2-REC
           END-IF
           MOVE    "tbl_tensuplus2"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9102-TENSUPLUS2-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込（算定履歴検索用）
      *****************************************************************
       911-TENSU-READ22-SEC         SECTION.
      *
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key22"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key22"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
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
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key22"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *!
      *    検査正式名称
           IF      SPA-FORMALFLG       =   "1"
               IF     (TNS-SRYCD(1:1)      =   "1") AND
                      (TNS-SRYKBN          =   "60"
                                           OR  "64"   )  AND
                      (TNS-FORMALNAME  NOT =   SPACE )  AND
                      (TNS-FORMALNAME  NOT =   TNS-NAME )
                   MOVE    TNS-FORMALNAME  TO  TNS-NAME
               END-IF
           END-IF
      *!!
      *    医療観察コード名称編集
           IF      (TNS-SRYCD(1:1)     =   "1"  )  AND
                   (TNS-IRYOKANSATUKBN =   4    )
      *        医療観察区分
               MOVE    SPACE               TO  WRK-MEISYO
               STRING  "【医観】"          DELIMITED   BY  SIZE
                      TNS-NAME             DELIMITED   BY  SPACE
                      INTO             WRK-MEISYO
               END-STRING
               MOVE    WRK-MEISYO          TO  TNS-NAME
           END-IF
      *!!
      *
           .
       911-TENSU-READ22-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスター次読込
      *****************************************************************
       920-INPUTCD-NEXT-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
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
      *    算定歴マスタ読み込み
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
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
      *****************************************************************
      *    チェックデータ読込
      *****************************************************************
       950-CHK-READ-SEC           SECTION.
      *
           PERFORM 920-DBFETCH-SEC
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
      *    点数マスタ付加情報読込
      *****************************************************************
       980-TENSUPLUS-READ-SEC          SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TENSUPLUS-REC
               MOVE    ZERO            TO  FLG-TENSUPLUS
           ELSE
               INITIALIZE                  TENSUPLUS-REC
               MOVE    1               TO  FLG-TENSUPLUS
           END-IF
           .
      *
       980-TENSUPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人一般コード変換読込
      *****************************************************************
       990-SRYCDCHG-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYCDCHG-REC
               MOVE    ZERO                TO  FLG-SRYCDCHG
           ELSE
               INITIALIZE                      SRYCDCHG-REC
               MOVE    1                   TO  FLG-SRYCDCHG
           END-IF
      *
           .
       990-SRYCDCHG-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者禁忌薬剤マスタ読込処理
      *****************************************************************
       900-PTKINKI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKINKI-REC
               MOVE    ZERO            TO  FLG-PTKINKI
           ELSE
               INITIALIZE                  PTKINKI-REC
               MOVE    1               TO  FLG-PTKINKI
           END-IF
           .
       900-PTKINKI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    クロース処理
      *****************************************************************
       990-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
