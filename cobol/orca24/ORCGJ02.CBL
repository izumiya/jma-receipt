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
       PROGRAM-ID.                 ORCGJ02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 会計照会
      *  コンポーネント名  : 会計カード入力（Ｊ０２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-多々納   01.04.17  戻る時、確認画面を表示する等
      *  01.00.02    MCC-多々納   01/05/17  年齢計算、老人判定追加
      *  01.00.03    MCC-多々納   02/04/25  労災追加
      *  01.00.04    MCC-多々納   02/06/05  患者氏名での検索を追加
      *  01.00.05    NACL-多々納  02/07/29  パラメタファイルをＤＢへ
      *  01.00.06    NACL-多々納  02/09/11  自賠責追加
      *  01.00.07    NACL-多々納  02/10/25  入院追加
      *  01.00.08    NACL-多々納  02/12/09  剤削除修正
      *  01.00.09    NACL-多々納  03/03/05  排他制御処理追加
      *  01.00.10    NACL-多々納  03/04/04  中心静脈自動算定追加
      *  01.00.11    NACL-多々納  03/05/12  前次頁対応、診療日変更修正
      *  01.00.12    NACL-多々納  03/08/28  入院の受診履歴追加修正
      *  01.00.13    NACL-多々納  04/01/09  保険毎表示追加修正
      *  01.00.14    NACL-多々納  04/05/09  収納の会計変更区分追加
      *                                     受診履歴更新修正
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
      *******                      FILE    STATUS  IS  STS-PARA.
      *
      *    ＳＰＡデータ
      *D   SELECT  SPASCR-FILE     ASSIGN  "J02SPASCR.TXT,T"
           SELECT  SPASCR-FILE     ASSIGN  FILE-NAME1
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-SPASCR.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ＳＰＡパラメタデータ
       FD  SPASCR-FILE.
       01  SPA-DATA-REC            PIC X(30000).
      *
      *    パラメタデータ
      *FD  PARA-FILE.
      *01  PARA-REC.
      *    03  PARA-KEY.
      *********05  PARA-TERMID          PIC X(64).
      *        05  PARA-FILEMEI         PIC X(08).
      *        05  PARA-EDANUM          PIC 9(03).
      *    03  PARA-DATA-REC            PIC X(3500).
      *
      *
       WORKING-STORAGE             SECTION.
      * ＳＰＡパラメタデータ
       01  FILE-NAME1.
           03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
           03  FILLER              PIC X(09)   VALUE   "J02SPASCR".
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
       01  SPA-SCR-REC.
           COPY    "CPJ02SPASCR.INC".
      *
      *    画面コントロール
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    会計照会共通パラメタ
           COPY    "J01COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "J02SCR-SPA".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
           03  STS-SPASCR          PIC X(02).
           03  STS-SRYKBN          PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-END2            PIC 9(01).
           03  FLG-SPASCR          PIC 9(01).
           03  FLG-SRYKBN          PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-INPUTSET        PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
      *
           03  FLG-NAIYAK          PIC X(01).
           03  FLG-TONPUK          PIC X(01).
           03  FLG-GAIYAK          PIC X(01).
           03  FLG-SP              PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
           03  FLG-HENSYU          PIC 9(01).
           03  FLG-DOUJI           PIC 9(01).
      *
           03  FLG-YAKUSOKU        PIC 9(01).
      *
           03  FLG-UPD             PIC 9(01).
      *    老人フラグ
           03  FLG-ROUJIN          PIC 9(01).
      *    編集フラグ
           03  FLG-HEN-OK          PIC 9(01).
      *    受診履歴変更フラグ
           03  FLG-JYURRK-UPD      PIC 9(01).
      *    受診履歴エラー
           03  FLG-JYURRK-ERR      PIC 9(01).
      *
           03  FLG-ACCT-ERR        PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-JYURRK-ARI      PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
           03  CNT-ENT             PIC 9(06).
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
           03  IDY                 PIC 9(04).
           03  IDG                 PIC 9(04).
           03  IDH                 PIC 9(04).
           03  IDK                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDS                 PIC 9(04).
           03  IDX-L               PIC 9(04).
           03  IDK1                PIC 9(04).
           03  IDK2                PIC 9(04).
           03  IDM                 PIC 9(04).
           03  IDXG                PIC 9(04).
           03  IDX-H               PIC 9(04).
           03  IDX-D               PIC 9(02).
      *    漢字変換用
           03  IDJ                 PIC 9(04).
           03  IDJ1                PIC 9(01).
           03  IDJ2                PIC 9(04).
      *
           03  IDX-S               PIC 9(04).
      *
           03  IDX-R               PIC 9(04).
           03  IDR                 PIC 9(04).
           03  IDR2                PIC 9(02).
           03  IDR3                PIC 9(04).
      *    受診履歴更新用
           03  IDX-JYU             PIC 9(04).
           03  IDX-JYU2            PIC 9(04).
      *
      *    入院点滴用
           03  IDX-TEN             PIC 9(04).
           03  IDX-TEN1            PIC 9(04).
           03  IDX-TEN2            PIC 9(04).
           03  IDX-TEN3            PIC 9(04).
           03  IDX-TENT1           PIC 9(04).
           03  IDX-TENT3           PIC 9(04).
           03  IDX-REN             PIC 9(04).
           03  IDX-REN2            PIC 9(04).
           03  IDX-DEL             PIC 9(04).
      *
           03  IDX-J               PIC 9(04).
      *
           03  IDX-Z               PIC 9(04).
           03  IDX-IN              PIC 9(02).
           03  IDX-IN2             PIC 9(04).
      *
           03  IDX3                PIC 9(04).
           03  IDX-T               PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-PARA-EDANUM         PIC 9(03).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
           03  WRK-SRYYMD.
               05  WRK-SRYYY       PIC 9(04).
               05  WRK-SRYMM       PIC 9(02).
               05  WRK-SRYDD       PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-GMN-SRYYM       PIC X(06).
           03  WRK-NAI-SRYYM       PIC X(06).
           03  WRK-ENDDD           PIC 9(02).
           03  WRK-DD              PIC 9(02).
      *
           03  WRK-SYM-1.
               05  WRK-SYY-1       PIC 9(04).
               05  WRK-SMM-1       PIC 9(02).
           03  WRK-SYM-2.
               05  WRK-SYY-2       PIC 9(04).
               05  WRK-SMM-2       PIC 9(02).
      *    連番
           03  WRK-BANGO           PIC 9(04).
      *    年齢計算用
           03  WRK-YMDS1.
               05  WRK-YYS1        PIC 9(04).
               05  WRK-MMS1        PIC 9(02).
               05  WRK-DDS1        PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2        PIC 9(04).
               05  WRK-MMS2        PIC 9(02).
               05  WRK-DDS2        PIC 9(02).
      *
           03  WRK-TENSU-X.
               05  WRK-TENSU       PIC Z(07).
               05  WRK-TENSU-2     REDEFINES   WRK-TENSU
                                   PIC -(07).
           03  WRK-KAISU-X.
               05  WRK-KAISU       PIC Z(08).
           03  WRK-KEI-X.
               05  WRK-KEI         PIC Z(07).
      *
           03  WRK-MEISYO          PIC X(80).
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
      *    分画数画面編集用
           03  WRK-BUNGSU-X.
               05  WRK-BUNGSU-Z1   PIC ZZZ.
           03  WRK-BUNGSU-X2.
               05  WRK-BUNGSU-Z2   PIC ZZZ.
           03  WRK-BUNGSU-J        PIC X(06).
      *    剤回数等画面編集用
           03  WRK-KAISU-H         PIC X(08).
           03  WRK-KEI-H           PIC X(08).
           03  WRK-ZAIKEI          PIC X(40).
           03  WRK-ZAIKEIJ         PIC X(80).
           03  WRK-IDG             PIC 9(04).
      *    マップ画面行
           03  WRK-MAP-INDEX        PIC 9(04).
      *    保険組合せ名称
           03  WRK-HKNCOMBI         PIC X(04).
           03  WRK-HKNCOMBIMEI      PIC X(100).
           03  WRK-HKNKBN-MEI       PIC X(02).
      *
           03  WRK-ZAIKAISU        PIC 9(07).
      *
      *    診療行為名称
           03  WRK-SRYSYUMEI       PIC X(40).
           03  WRK-SRYSYUMEI2      PIC X(40).
           03  WRK-SRYKAMEI        PIC X(12).
           03  WRK-TEKEDYMD        PIC X(08).
      *
           03  WRK-ZZZZ-X.
               05  WRK-ZZZZ            PIC ZZZ.
      *
           03  WRK-ZZ-X.
               05  WRK-ZZ              PIC ZZ.
           03  WRK-ZZZ-X.
               05  WRK-ZZZ             PIC ZZZ.
      *
           03  WRK-Z9-X.
               05  WRK-Z9              PIC Z9.
           03  WRK-ZZ9-X.
               05  WRK-ZZ9             PIC ZZ9.
      *
           03  WRK-HENNYMD             PIC X(22).
      *
           03  WRK-IDX                 PIC 9(04).
      *
      *    算定履歴更新用
           03  WRK-MSANTEICNT          PIC  9(03).
           03  WRK-MSANTEID            PIC  9(02).
           03  WRK-SANCNT              PIC S9(03).
           03  WRK-SANTEI              PIC S9(03).
           03  IDX-SAN                 PIC  9(04).
      *    行選択用
           03  WRK-SELNUM              PIC 9(03).
           03  WRK-SELNUM2             PIC 9(03).
      *
      *    受診履歴変更用
           03  WRK-RRKYMD              PIC X(08).
           03  WRK-RENNUM              PIC 9(01).
           03  WRK-MAE-RENNUM          PIC 9(01).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-DAY-S               PIC 9(03).
           03  IDX-D1                  PIC 9(02).
           03  IDX-D2                  PIC 9(02).
      *****03  IDX-D3                  PIC 9(04).
           03  IDX-D3                  PIC 9(01).
           03  IDX-D4                  PIC 9(04).
      *    履歴剤変更用
           03  WRK-RRKZAI-HEN.
               05  WRK-IDR             PIC 9(04).
               05  WRK-IDR-MAX         PIC 9(04).
           03  IDX-RRK                 PIC 9(04).
           03  IDX-N                   PIC 9(04).
      *
           03  WRK-TBANGO              PIC  9(04).
      *
           03  WRK-MOTOPG              PIC X(08).
      *    老人年齢
           03  WRK-ROU-NEN             PIC 9(02).
      *
           03  WRK-ERRMSG              PIC X(80).
           03  WRK-JIDMSG.
               05  WRK-JIDMSG1              PIC X(40).
               05  WRK-JIDMSG2              PIC X(40).
      *
      *    診療コード
           03  WRK-SRYCD              PIC X(09).
      *    発生年月日
           03  WRK-HASSYOYMD.
               05  WRK-HAS-YY         PIC 9(04).
               05  WRK-HAS-MM         PIC 9(02).
               05  WRK-HAS-DD         PIC 9(02).
      *
           03  WRK-ORC-DBPATH          PIC X(64).
      *    クリア用保存
           03  WRK-NYUGAIKBN-G         PIC X(10).
           03  WRK-SRYKBN-G            PIC X(19).
           03  WRK-HKNCOMBI-G          PIC X(55).
      *    入院エラー
           03  WRK-NYUIN-ERR           PIC X(200).
           03  IDX-N2                  PIC 9(02).
      *    受診履歴追加用
           03  WRK-RRK-HKNKBN          PIC X(01).
           03  WRK-RRK-RENNUM          PIC 9(01).
           03  WRK-RRK-DOUJI-RENNUM    PIC 9(01).
      *    点滴算定エラー
           03  WRK-ERR-TBL.
               05  WRK-ERR-DAY         PIC 9(01)   OCCURS  31.
      *
      *    カルテ３号紙プログラム名
           03  WRK-PRINT-PG            PIC X(20).
      *
      *    コラムリスト位置
           03  WRK-J02-DATALIST-ROW    PIC S9(09).
           03  WRK-J02-RRKLIST-ROW     PIC S9(09).
           03  FLG-GMN-INIT            PIC  9(01).
      *    剤変更チェック用
           03  TBL-MAE-ZAINUM-AREA.
               05  TBL-MAE-ZAINUM      PIC 9(08)
                                       OCCURS   135.
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
      *    保険別算定履歴テーブル
           COPY    "CMSANTEITBL.INC".
      *
      *   日本語数字
       01  TBL-SUJI-AREA.
           03  FILLER          PIC X(20)   VALUE
              "１２３４５６７８９０".
       01  TBL-SUJI-R          REDEFINES   TBL-SUJI-AREA.
           03  TBL-SUJI        PIC X(02)   OCCURS   10.
      *
      *    受診履歴
       01  WRK-JYURRK-REC.
           COPY    "CPJYURRK.INC"     REPLACING
                                     //JYURRK-// BY //WRK-JYURRK-//.
      *    受診履歴
       01  HZN-JYURRK-REC.
           COPY    "CPJYURRK.INC"     REPLACING
                                     //JYURRK-// BY //HZN-JYURRK-//.
      *    受診履歴
       01  TBL-JYURRK-AREA.
           02  TBL-JYURRK-REC         OCCURS  100.
           COPY    "CPJYURRK.INC"     REPLACING
                                     //JYURRK-// BY //TBL-JYURRK-//.
      *
      *    スパ領域 ワーク
           COPY    "COMMON-SPA"      REPLACING
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
           COPY    "CPSK0041.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
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
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    入力セットコード
       01  INPUTSET-REC.
           COPY    "CPINPUTSET.INC".
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *    収納マスタ
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *     パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
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
      *   算定履歴更新サブ
           COPY    "CPORCSSANTEI.INC".
      *
      *    保険算定チェックサブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    入院点滴一括更新サブ
           COPY    "CPORCSTENTEK.INC".
      *
      *    入院調剤料更新サブ
           COPY    "CPORCSCHOZAI.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    カルテ３号様式出力パラメタ
           COPY    "CPORCHC33.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSHKNSET.INC".
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
           COPY    "J03.INC".
           COPY    "J04.INC".
           COPY    "J041.INC".
           COPY    "J042.INC".
           COPY    "J98.INC".
           COPY    "J99.INC".
           COPY    "JERR.INC".
           COPY    "JID1.INC".
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
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-J02
           MOVE    SPA-WORK        TO  SPA-J01KYOUTU
      *
      *    診療種別テーブルセット
           PERFORM 1001-SRYKBN-SET-SEC
      *
      *    画面ＳＰＡ読み込み
           PERFORM 1002-J02SPA-SET-SEC
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
      *    画面ＳＰＡ書込み
           IF      FLG-END2            =   ZERO
               PERFORM 1003-J02SPA-OUT-SEC
           END-IF
      *
           MOVE    SPA-J01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-J02         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    診療種別テーブルセット処理
      *****************************************************************
       1001-SRYKBN-SET-SEC             SECTION.
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
      *
           .
       1001-SRYKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡデータセット処理
      *****************************************************************
      *
       1002-J02SPA-SET-SEC             SECTION.
      *
           MOVE    SPA-TERMID          TO  FILE-TERMID
      *
           OPEN    INPUT               SPASCR-FILE
           IF      STS-SPASCR     NOT =   ZERO
               GO      TO         1002-J02SPA-SET-EXT
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
       1002-J02SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    ＳＰＡデータＷＲＩＴＥ処理
      *****************************************************************
       1003-J02SPA-OUT-SEC             SECTION.
      *
           MOVE    SPA-TERMID          TO  FILE-TERMID
      *
           OPEN    OUTPUT              SPASCR-FILE
           IF      STS-SPASCR     NOT =   ZERO
               DISPLAY "J02SPASCR OUT OPEN ERR:"   STS-SPASCR "."
           END-IF
           MOVE    SPA-SCR-REC         TO  WRK-DATA-REC
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL      (IDX     >   10  )  OR
                              (WRK-DATA (IDX)     =   SPACE)
      *
               MOVE    WRK-DATA (IDX)      TO  SPA-DATA-REC
               WRITE                       SPA-DATA-REC
               IF      STS-SPASCR     NOT =   ZERO
                   DISPLAY "J02SPASCR OUT WRITE ERR:"   STS-SPASCR "."
                END-IF
           END-PERFORM
      *
           CLOSE                       SPASCR-FILE
           .
       1003-J02SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "JERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
               IF      FLG-END         =   1
                   GO      TO      100-INIT-EXT
               END-IF
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
           MOVE    SPACE               TO  LINKAREA
      *
      *****MOVE   "NEW"                TO  MCP-PUTTYPE
           MOVE   SPACE                TO  MCP-PUTTYPE
           MOVE   "J02"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
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
      *
      *        画面ＳＰＡ読み込み
               PERFORM 1002-J02SPA-SET-SEC
      *
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *        元画面表示
               IF      WRK-MOTOPG(1:3)     =   "Y01"  OR  "U01"
                                                      OR  "P97"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-J02
                   MOVE    SPA-WORK        TO  SPA-J01KYOUTU
               END-IF
      *        予約の時、そのまま画面表示へ
               IF      WRK-MOTOPG(1:3)     =   "Y01"
                   GO      TO      300-SCREEN-EXT
               END-IF
      *        外部より選択がある時
               IF      WRK-MOTOPG(1:3)     =   "U01"  OR
                                               "P97"
                   PERFORM 3101-P97SPASET-SEC
                   GO      TO      300-SCREEN-EXT
               END-IF
      *
               MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
           END-IF
      *
           EVALUATE    SPA-MOTOPG(1:4)
      *        保険組合一覧より
      **       WHEN    "J021"
      *******      IF      SPA-J021-HKNCOMBI       NOT =   SPACE
      *                MOVE    SPA-J021-HKNCOMBI   TO  J02-HKNCOMBI
      *                PERFORM 41017-HKNCOMBI-CHK-SEC
      ************ END-IF
      *            PERFORM 3004-HKNCOMBI-HENKAN-SEC
      ************ GO      TO      300-SCREEN-EXT
      *        回数入力より
               WHEN    "J022"
                   PERFORM 3002-J022-HEN-SEC
                   GO      TO      300-SCREEN-EXT
      *        レセコメントより
               WHEN    "J023"
                   GO      TO      300-SCREEN-EXT
      *            確認画面より
               WHEN    "JID1"
                   PERFORM 3003-JID1-SET-SEC
                   MOVE    SPACE           TO  SPA-JIDCD
                   IF      SPA-ERRCD       NOT =   SPACE
                       PERFORM 500-GMNHEN-SEC
                       PERFORM 510-ERRSET-SEC
                       MOVE    1               TO  FLG-END
                   END-IF
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-JIDCD
           INITIALIZE                  SPA-J02-AREA
           INITIALIZE                  SPA-J02-SYOKI
           INITIALIZE                  SPA-SCR-REC
           EVALUATE    SPA-MOTOPG
      *         業務選択より
               WHEN    "M01"
                   INITIALIZE                  SPA-J01KYOUTU
                   MOVE    SPA-MOTOPG          TO  SPA-J02-MOTOPG
                   MOVE    "0"                 TO  SPA-J02-MOTOFLG
                   MOVE    SPA-SYSYMDWH(1:6)   TO  SPA-GMN-SRYYM
                   MOVE    SPA-SYSYMD  (1:6)   TO  SPA-NAI-SRYYM
                   MOVE    SPA-SYSYMDWH        TO  SPA-J02-SRYYMDW
                   MOVE    SPA-SYSYMD          TO  SPA-J02-SRYYMD
                   MOVE    "2"                 TO  SPA-NYUGAIKBN
                   PERFORM 3005-SRYKA-HEN-SEC
                   MOVE    ZERO                TO  SPA-GMN-CUR
               WHEN    "K02N"
               WHEN    "K02"
      *            診療行為より
                   INITIALIZE                  SPA-J01KYOUTU
      *
                   MOVE    SPA-MOTOPG          TO  SPA-J02-MOTOPG
                   IF      SPA-PTID            =   ZERO
                       MOVE    "0"                 TO  SPA-J02-MOTOFLG
                   ELSE
                       MOVE    "1"                 TO  SPA-J02-MOTOFLG
                   END-IF
                   MOVE    SPA-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM
                   MOVE    SPA-SRYYMD (1:6)    TO  SPA-NAI-SRYYM
                   MOVE    SPA-SRYYMDW         TO  SPA-J02-SRYYMDW
                   MOVE    SPA-SRYYMD          TO  SPA-J02-SRYYMD
                   MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
                   MOVE    SPA-PTID            TO  SPA-GMN-PTID
                   MOVE    SPA-KANANAME        TO  SPA-GMN-KANANAME
                   MOVE    SPA-NAME            TO  SPA-GMN-NAME
                   MOVE    SPA-SEX             TO  SPA-GMN-SEX
                   MOVE    SPA-BIRTHDAYW       TO  SPA-GMN-BIRTHDAYW
                   MOVE    SPA-BIRTHDAY        TO  SPA-GMN-BIRTHDAY
      *
                   IF      SPA-PTID            =   ZERO
                       MOVE    SPACE               TO  SPA-GMN-SRYKA
                       MOVE    SPACE               TO  SPA-J01-SRYKA
                       MOVE    SPACE               TO  SPA-J01-SRYKAMEI
                       MOVE    ZERO                TO  SPA-GMN-CUR
                       PERFORM 3005-SRYKA-HEN-SEC
                   ELSE
                       MOVE    SPA-SRYKA           TO  SPA-J01-SRYKA
                       MOVE    SPA-SRYKA           TO  SPA-GMN-SRYKA
                       MOVE    SPA-SRYKAMEI        TO  SPA-J01-SRYKAMEI
                       MOVE    1                   TO  FLG-HENSYU
                       PERFORM 310-SPA-SET-SEC
                       MOVE    1                   TO  SPA-GMN-CUR
      *                排他制御処理
                       PERFORM 990-LOCK-IN-SEC
                   END-IF
               WHEN    "U02"
                   INITIALIZE                  SPA-J01KYOUTU
      *            受付より
                   MOVE    SPA-MOTOPG          TO  SPA-J02-MOTOPG
                   IF      SPA-PTID            =   ZERO
                       MOVE    "0"                 TO  SPA-J02-MOTOFLG
                   ELSE
                       MOVE    "1"                 TO  SPA-J02-MOTOFLG
                   END-IF
                   MOVE    SPA-SYSYMDWH(1:6)   TO  SPA-GMN-SRYYM
                   MOVE    SPA-SYSYMD  (1:6)   TO  SPA-NAI-SRYYM
                   MOVE    SPA-SYSYMDWH        TO  SPA-J02-SRYYMDW
                   MOVE    SPA-SYSYMD          TO  SPA-J02-SRYYMD
                   MOVE    "2"                 TO  SPA-NYUGAIKBN
                   IF      SPA-PTID            =   ZERO
                       MOVE    ZERO                TO  SPA-GMN-CUR
                       PERFORM 3005-SRYKA-HEN-SEC
                   ELSE
                       MOVE    SPA-SRYKA           TO  SPA-J01-SRYKA
                       MOVE    SPA-SRYKA           TO  SPA-GMN-SRYKA
                       MOVE    SPA-SRYKAMEI        TO  SPA-J01-SRYKAMEI
                       MOVE    1                   TO  FLG-HENSYU
                       MOVE    SPA-PTNUM           TO  J02-PTNUM
                       PERFORM 41010-PTNUM-SYORI-SEC
                       MOVE    1                   TO  FLG-HENSYU
                       PERFORM 310-SPA-SET-SEC
                       MOVE    1                   TO  SPA-GMN-CUR
                   END-IF
      *
               WHEN     OTHER
                   MOVE    SPA-J02-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM
                   MOVE    SPA-J02-SRYYMD (1:6)    TO  SPA-NAI-SRYYM
                   IF      SPA-MOTOPG(1:3)     =   "J04"  OR  "J03"
                       MOVE    SPA-MAE-SRYYMD      TO  SPA-SRYYMD
                       MOVE    SPA-MAE-PTID        TO  SPA-PTID
                   END-IF
      *
                   MOVE    1                   TO  FLG-HENSYU
                   PERFORM 310-SPA-SET-SEC
                   MOVE    1                   TO  SPA-GMN-CUR
           END-EVALUATE
      *
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-IDXZ
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    回数入力画面より編集処理
      *****************************************************************
       3002-J022-HEN-SEC                  SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   31
               IF      SPA-NAI-DAYCHK (IDX)    =   1
                   MOVE    SPA-NAI-DAY1(IDX)   TO  SPA-GNAI-DAY (2 IDX)
                   MOVE    SPA-NAI-DAY2(IDX)   TO  SPA-GNAI-DAY (3 IDX)
                   MOVE    SPA-NAI-DAY3(IDX)   TO  SPA-GNAI-DAY (4 IDX)
                   COMPUTE SPA-GNAI-DAY (1 IDX )
                                               =   SPA-NAI-DAY1 (IDX)
                                               +   SPA-NAI-DAY2 (IDX)
                                               +   SPA-NAI-DAY3 (IDX)
                   MOVE    SPA-GNAI-DAY (1 IDX )   TO  SPA-GMN-DAY(IDX)
               END-IF
           END-PERFORM
      *    受診履歴の剤変更
           PERFORM 410122-RRK-ZAINUM-SEC
      *
           IF      SPA-GMN-CUR         =   2
               IF      SPA-DAY-IDX             >=  SPA-SRYMATU
                   MOVE    5                   TO  SPA-GMN-CUR
               ELSE
                   MOVE    2                   TO  SPA-GMN-CUR
                   ADD     1                   TO  SPA-DAY-IDX
               END-IF
           END-IF
      *
           .
       3002-J022-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     確認画面よりの処理
      *****************************************************************
       3003-JID1-SET-SEC                  SECTION.
      *
           EVALUATE    SPA-JIDCD
               WHEN    "0101"
               WHEN    "0102"
      *            戻るの確認
                   IF      SPA-JID1-FLG        =   "OK"
                       MOVE    1                   TO  SPA-GMN-CUR
                       MOVE    SPACE               TO  SPA-UPDFLG
                       PERFORM 210-BACK-SYORI-SEC
                   ELSE
                       MOVE    1                   TO  SPA-GMN-CUR
                   END-IF
      *
               WHEN    "0103"
      *            明細選択の確認
                   IF      SPA-JID1-FLG        =   "OK"
                       MOVE    1                   TO  SPA-GMN-CUR
                       MOVE    SPA-GMN-BANGO       TO  J02-NUM
                       PERFORM 41011-BANGO-CHK-SEC
                   ELSE
                       MOVE    2                   TO  SPA-GMN-CUR
                       MOVE    SPA-MAE-BANGO       TO  SPA-GMN-BANGO
                       MOVE    SPA-MAEN-BANGO      TO  SPA-NAI-BANGO
                   END-IF
      *
               WHEN    "0104"
      *            剤変更の確認
                   IF      SPA-JID1-FLG        =   "OK"
                       MOVE     1             TO  SPA-J02-SYORI
                       PERFORM 420-ZAIHENKOU-SYORI-SEC
                   ELSE
                       MOVE     1             TO  SPA-GMN-CUR
                   END-IF
      *
               WHEN    "0106"
      *            診療年月変更の確認
                   IF      SPA-JID1-FLG        =   "OK"
                       IF      SPA-J02-SEL         =   1
      *                    前月処理へ
                           PERFORM 41018-ZENGETU-SYORI-SEC
                       ELSE
      *                    次月処理へ
                           PERFORM 41019-JIGETU-SYORI-SEC
                       END-IF
                   ELSE
                       MOVE    1                  TO  SPA-GMN-CUR
                   END-IF
      *
               WHEN    "0107"
      *            診療科変更の確認
                   IF      SPA-JID1-FLG        =   "OK"
                       PERFORM 41013-SRYKA-CHANGE-SEC
                       MOVE    1                  TO  SPA-GMN-CUR
                   ELSE
                       MOVE    SPA-MAE-SRYKA-G    TO  SPA-GMN-SRYKA-G
                       MOVE    1                  TO  SPA-GMN-CUR
                   END-IF
      *
               WHEN    "0108"
      *            診療年月変更の確認
                   IF      SPA-JID1-FLG        =   "OK"
                       PERFORM 410181-SRYYM-CHANGE-SEC
                       MOVE    1                  TO  SPA-GMN-CUR
                   ELSE
                       MOVE    SPA-J02-SRYYMDW(1:6)  TO  SPA-GMN-SRYYM
                       MOVE    SPA-J02-SRYYMD(1:6)   TO  SPA-NAI-SRYYM
                       MOVE    1                  TO  SPA-GMN-CUR
                   END-IF
               WHEN    "0109"
      *            保険一括変換の確認
                   IF      SPA-JID1-FLG        =   "OK"
                       PERFORM 41001-HKNHEN-SYORI-SEC
                   ELSE
                       MOVE    1                  TO  SPA-GMN-CUR
                   END-IF
      *
               WHEN    "0110"
      *            診療内容変更の確認
                   IF      SPA-JID1-FLG        =   "OK"
                       PERFORM 410171-NYUGAIKBN-CHANGE-SEC
                       MOVE    1                  TO  SPA-GMN-CUR
                   ELSE
                       MOVE    SPA-J02-SRYYMDW(1:6)  TO  SPA-GMN-SRYYM
                       MOVE    SPA-J02-SRYYMD(1:6)   TO  SPA-NAI-SRYYM
                       MOVE    1                  TO  SPA-GMN-CUR
                   END-IF
      *
               WHEN    "0113"
      *            前頁表示の確認
                   IF      SPA-JID1-FLG        =   "OK"
                       PERFORM 4401-MAEGMN-SYORI-SEC
                   ELSE
                       MOVE    1                  TO  SPA-GMN-CUR
                   END-IF
      *
               WHEN    "0114"
      *            前頁表示の確認
                   IF      SPA-JID1-FLG        =   "OK"
                       PERFORM 4501-ATOGMN-SYORI-SEC
                   ELSE
                       MOVE    1                  TO  SPA-GMN-CUR
                   END-IF
      *
               WHEN    "0115"
      *            受診日の変更確認
                   IF      SPA-JID1-FLG        =   "OK"
                       PERFORM 410161-HENSRYYMD-UPDSYORI-SEC
                   ELSE
                       MOVE    SPACE           TO  SPA-NAI-HENSRYYMD
                       MOVE    SPACE           TO  SPA-GMN-HENSRYYMD
                   END-IF
      *
               WHEN    "0116"
               WHEN    "0117"
      *            印刷確認
                   IF      SPA-JID1-FLG        =   "OK"
                       PERFORM 41201-PRINT-SYORI-SEC
                   ELSE
                       MOVE    1                  TO  SPA-GMN-CUR
                   END-IF
      *
           END-EVALUATE
           .
       3003-JID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ一括変換より処理
      *****************************************************************
       3004-HKNCOMBI-HENKAN-SEC              SECTION.
      *
      *    保存用パラメタ作成
           PERFORM 4201-SRYACCT-PARA-SEC
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-STRNUM
      *
      *    診療会計マスターより編集をする
           PERFORM 410181-SRYACCT-HEN-SEC
      *
           .
       3004-HKNCOMBI-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＬＤ外の項目入力編集処理
      *****************************************************************
       3101-P97SPASET-SEC              SECTION.
      *
           MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU
      *    受付、患者検索より
           IF      WRK-SPA-PTNUM       =   SPA-GMN-PTNUM
               CONTINUE
           ELSE
               IF      WRK-SPA-PTNUM       NOT =   SPACE
      *            初期表示へ
                   MOVE    WRK-SPA-PTNUM   TO  J02-PTNUM
                   PERFORM 41010-PTNUM-SYORI-SEC
               END-IF
           END-IF
      *
           IF      SPA-GMN-PTID        =   ZERO
               MOVE    ZERO                TO  SPA-GMN-CUR
           ELSE
               MOVE    6                   TO  SPA-GMN-CUR
           END-IF
           .
       3101-P97SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科初期編集
      *****************************************************************
       3005-SRYKA-HEN-SEC                  SECTION.
      *
           INITIALIZE                  SPA-GMN-SRYKA-AREA
      *    診療科
           MOVE    SPACE               TO  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    ZERO                TO  SYS-1005-STYUKYMD
           MOVE    ALL "9"             TO  SYS-1005-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    SPA-NAI-SRYYM       TO  ORC-DBYMD(1:6)
           MOVE    "01"                TO  ORC-DBYMD(7:2)
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       (IDX            >   40   )  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
      *
               MOVE    SYS-1005-KBNCD(1:2) TO
                                         SPA-GMN-SRYKAL (IDX)
               MOVE    SYS-1005-SRYKANAME1 TO
                                         SPA-GMN-SRYKAMEIL (IDX)
      *
               MOVE    IDX                 TO  SPA-SRYKA-MAX
      *
               MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
               PERFORM 900-KANRIMST-READ-SEC
           END-PERFORM
      *
           ADD     1                   TO  SPA-SRYKA-MAX
           MOVE    "00 全科"           TO  SPA-GMN-SRYKA-LIST
                                                    (SPA-SRYKA-MAX)
      *
      *    MOVE    "DBCLOSE"           TO  MCP-FUNC
      *    MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
      *
           .
       3005-SRYKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
      *    入外区分
           MOVE    SPA-NYUGAIKBN       TO  SPA-GMN-NYUGAIKBN
           IF      SPA-BEDSU           >   ZERO
               MOVE    "1 入院"            TO  SPA-GMN-NYUGAIKBN-LST (1)
               MOVE    "2 外来"            TO  SPA-GMN-NYUGAIKBN-LST (2)
               MOVE    2                   TO  SPA-NYUGAI-MAX
           ELSE
               MOVE    "2 外来"            TO  SPA-GMN-NYUGAIKBN-LST (1)
               MOVE    1                   TO  SPA-NYUGAI-MAX
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-NYUGAI-MAX
               IF      SPA-GMN-NYUGAIKBN   =   SPA-GMN-NYUGAIKBN-LST
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-NYUGAIKBN-LST (IDX)
                                           TO  SPA-GMN-NYUGAIKBN-G
               END-IF
           END-PERFORM
      *
      *    診療科一覧編集
           PERFORM 3005-SRYKA-HEN-SEC
      *
      *    診療種別
           PERFORM 3006-SRYKBN-HEN-SEC
      *
           IF     (SPA-PTID            =   ZERO )  AND
                  (SPA-GMN-PTID        =   ZERO )
               MOVE    00              TO  SPA-GMN-CUR
               GO      TO      310-SPA-SET-EXT
           END-IF
      *
      *    保険一覧編集
           PERFORM 3008-HKNCOMBI-HEN-SEC
      *
      *    診療年月の末日算定
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SPA-NAI-SRYYM       TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI(7:2)    TO  SPA-SRYMATU
      *
      *----(01.00.01) LINE ADD START ----------------------------------
      *    年齢計算（算定月の１日の年齢）
           MOVE    SPA-NAI-SRYYM       TO  WRK-YMDS2(1:6)
           MOVE    01                  TO  WRK-DDS2
           PERFORM 30101-NENREI-SET-SEC
      *    該当月で６歳になる時、年齢フラグを立てる
           IF     (SPA-NAI-NENREI-YY   =   05  )  AND
                  (SPA-NAI-NENREI-MM   =   11  )  AND
                  (WRK-YMDS1(5:2)      =   WRK-YMDS2(5:2))
               MOVE    1                   TO  SPA-FLG-NENREI
           END-IF
      *
      *
      *    Ｈ１４．１０時点で７０歳未満の時
           IF     (SPA-NAI-SRYYM       >=  "200210"   )  AND
                  (SPA-BIRTHDAY        >=  "19321001" )
               MOVE    75                  TO  WRK-ROU-NEN
           ELSE
               MOVE    70                  TO  WRK-ROU-NEN
           END-IF
      *    該当月１日が７５歳以上の時、老人とする
           IF      SPA-NAI-NENREI-YY   >=  WRK-ROU-NEN
               MOVE    1                   TO  SPA-J02-ROUJIN
           ELSE
               MOVE    ZERO                TO  SPA-J02-ROUJIN
           END-IF
      *
      **** IF      SPA-NAI-SRYYM       >=  "200210"
      *        該当月１日が７５歳以上の時、老人とする
      *        IF      SPA-NAI-NENREI-YY   >=  75
      *            MOVE    1                   TO  SPA-J02-ROUJIN
      *        ELSE
      *            MOVE    ZERO                TO  SPA-J02-ROUJIN
      *        END-IF
      *    ELSE
      *        該当月１日が７０歳以上の時、老人とする
      *        IF      SPA-NAI-NENREI-YY   >=  70
      *            MOVE    1                   TO  SPA-J02-ROUJIN
      *        ELSE
      *            MOVE    ZERO                TO  SPA-J02-ROUJIN
      *        END-IF
      *****END-IF
      *
      *    診療科選択のない時、以下中止
           MOVE    ZERO                TO  SPA-GMN-MAX
      **** IF      FLG-HENSYU          =   ZERO
      *         GO      TO      310-SPA-SET-EXT
      **** END-IF
      *    診療科編集
           IF      SPA-J01-SRYKA       =   SPACE
      *
      *        受診履歴より診療科決定
               PERFORM 310310-JYURRK-CHK-SEC
               IF      SPA-J01-SRYKA       =   SPACE
                   MOVE    "0020"          TO  SPA-ERRCD
                   MOVE    07              TO  SPA-GMN-CUR
                   GO      TO      310-SPA-SET-EXT
               END-IF
           END-IF
           PERFORM 3101-SRYKA-HEN-SEC
      *
      *    入院期間編集
           IF     (SPA-BEDSU           >   ZERO ) AND
                  (SPA-NYUGAIKBN       =  "1"   ) AND
                  (SPA-NAI-NYUINYMD    =  SPACE )
               PERFORM 410101-NYUINRRK-CHK-SEC
               PERFORM 410102-NYUINRRK-LST-SEC
           END-IF
      *
      *    診療科未入力の時、終了へ
           IF      FLG-HENSYU          =   ZERO
                MOVE    06              TO  SPA-GMN-CUR
                GO      TO      310-SPA-SET-EXT
           END-IF
      *    受診履歴編集
           PERFORM 3103-JYURRK-HEN-SEC
           IF     (SPA-NYUGAIKBN       =  "1"   ) AND
                  (SPA-J01-SRYKA   NOT =   SPACE)
               CONTINUE
           ELSE
               IF      SPA-RRK-MAX         =   ZERO
                   IF      SPA-SRYKA-MAX       =   1
                       MOVE    07              TO  SPA-GMN-CUR
                   ELSE
                       MOVE    06              TO  SPA-GMN-CUR
                   END-IF
                   GO      TO      310-SPA-SET-EXT
               END-IF
           END-IF
      *
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
      *    ＰＡＲＡ削除
           IF      SPA-MOTOPG(1:1)     =   "J"
               CONTINUE
           ELSE
               PERFORM 3106-PARA-DEL-SEC
           END-IF
      *
           IF     (SPA-MOTOPG2(1:3)    =   "K02" OR  "U02"  ) OR
                  (SPA-MOTOPG          =   "J04" OR  "J03"  OR  "J021")
      *        保存用パラメタ作成
               PERFORM 4201-SRYACCT-PARA-SEC
               MOVE    1                   TO  SPA-GMN-PAGE
               MOVE    1                   TO  SPA-STRNUM
      *        診療会計マスターより編集をする
               PERFORM 410181-SRYACCT-HEN-SEC
               GO      TO      310-SPA-SET-EXT
           END-IF
      *
      *    診療会計マスターより編集をする
           PERFORM 410181-SRYACCT-HEN-SEC
      *
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢計算処理
      *****************************************************************
       30101-NENREI-SET-SEC                  SECTION.
      *
           MOVE    SPA-GMN-BIRTHDAY    TO  WRK-YMDS1
           COMPUTE SPA-NAI-NENREI-YY   =   WRK-YYS2
                                       -   WRK-YYS1
           IF      WRK-YMDS1(5:4)      >   WRK-YMDS2(5:4)
               COMPUTE SPA-NAI-NENREI-YY   =   SPA-NAI-NENREI-YY
                                           -   1
               COMPUTE SPA-NAI-NENREI-MM   =  (WRK-MMS2  +  12 )
                                           -   WRK-MMS1
           ELSE
               COMPUTE SPA-NAI-NENREI-MM   =   WRK-MMS2
                                           -   WRK-MMS1
           END-IF
      *
      *    乳幼児チェック
           IF     (SPA-NAI-NENREI-YY       =   ZERO  )  AND
                  (SPA-NAI-NENREI-MM       =   ZERO  OR  1 )
      *        年齢期間算定
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY5-AREA
               MOVE    "51"                TO  LNK-DAY5-IRAI
               MOVE    SPA-GMN-BIRTHDAY    TO  LNK-DAY5-START
               MOVE    SPA-NAI-SRYYM       TO  LNK-DAY5-END(1:6)
               MOVE    SPA-SRYMATU         TO  LNK-DAY5-END(7:2)
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY5-AREA
               IF      LNK-DAY5-NISSU2     >   28
                   MOVE    1                   TO  SPA-FLG-NENREI
                   IF      LNK-DAY5-NISSU2     <=  31
                       MOVE    LNK-DAY5-NISSU2 TO  SPA-NAI-NENREI-DD
                       MOVE    ZERO            TO  SPA-NAI-NENREI-MM
                   END-IF
               END-IF
           END-IF
      *
      *
           .
       30101-NENREI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分編集処理
      *****************************************************************
       410102-NYUINRRK-LST-SEC                  SECTION.
      *
           MOVE    SPA-NYUGAIKBN       TO  SPA-GMN-NYUGAIKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-NYUGAI-MAX
               IF      SPA-GMN-NYUGAIKBN   =   SPA-GMN-NYUGAIKBN-LST
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-NYUGAIKBN-LST (IDX)
                                           TO  SPA-GMN-NYUGAIKBN-G
               END-IF
           END-PERFORM
           .
       410102-NYUINRRK-LST-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療種別初期編集処理
      *****************************************************************
       3006-SRYKBN-HEN-SEC                  SECTION.
      *
           INITIALIZE                  SPA-GMN-SRYKBN-COMB
           MOVE    "00 全体"           TO  SPA-GMN-SRYKBN-LST (01)
           MOVE    "10 診察"           TO  SPA-GMN-SRYKBN-LST (02)
           MOVE    "20 投薬"           TO  SPA-GMN-SRYKBN-LST (03)
           MOVE    "30 注射"           TO  SPA-GMN-SRYKBN-LST (04)
           MOVE    "40 処置"           TO  SPA-GMN-SRYKBN-LST (05)
           MOVE    "50 手術"           TO  SPA-GMN-SRYKBN-LST (06)
           MOVE    "60 検査"           TO  SPA-GMN-SRYKBN-LST (07)
           MOVE    "70 画像診断"       TO  SPA-GMN-SRYKBN-LST (08)
           MOVE    "80 理学・その他"   TO  SPA-GMN-SRYKBN-LST (09)
           MOVE    "90 自費・その他"   TO  SPA-GMN-SRYKBN-LST (10)
      *
           MOVE    10                  TO  SPA-SRYKBN-MAX
      *
           IF      SPA-GMN-SRYKBN-MAE  =   SPACE
               MOVE    SPA-GMN-SRYKBN-LST (01)
                                       TO  SPA-GMN-SRYKBN-G
           ELSE
               MOVE    SPA-GMN-SRYKBN-MAE  TO  SPA-GMN-SRYKBN
               PERFORM VARYING     IDX     FROM   1   BY  1
                       UNTIL       IDX     >   SPA-SRYKBN-MAX
                  IF      SPA-GMN-SRYKBN-LST (IDX)(1:2)
                                           =   SPA-GMN-SRYKBN
                       MOVE    SPA-GMN-SRYKBN-LST (IDX)
                                               TO  SPA-GMN-SRYKBN-G
                   END-IF
               END-PERFORM
           END-IF
           .
       3006-SRYKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *   保険一覧編集処理
      *****************************************************************
       3008-HKNCOMBI-HEN-SEC             SECTION.
      *
           INITIALIZE                  SPA-GMN-HKNCOMBI-LISTG
           MOVE    SPA-GMN-BIRTHDAY    TO  SPA-BIRTHDAY
           MOVE    SPA-NAI-SRYYM       TO  SPA-SRYYMD(1:6)
           MOVE    "00"                TO  SPA-SRYYMD(7:2)
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
      *
           INITIALIZE                  ORCSHKNSETAREA
           MOVE    "1"                 TO  ORCSHKN-KBN
           MOVE    "1"                 TO  ORCSHKN-KBN2
           CALL    "ORCSHKNSET"        USING
                                       SPA-AREA
                                       ORCSHKNSETAREA
      *
           MOVE    ORCSHKN-HKN-MAX     TO  SPA-HKN-MAX
           IF      SPA-HKN-MAX         =   ZERO
               GO      TO      3008-HKNCOMBI-HEN-EXT
           END-IF
      *
      *    全体を追加
           MOVE    "0000"              TO  SPA-GMN-THKNCOMBINUM (01)
           MOVE    "全保険"            TO  SPA-GMN-THKNCOMBIMEI (01)
           MOVE    ZERO                TO  SPA-NAI-THKNKBN      (01)
      *
           MOVE    1                   TO  IDH
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL      (IDX     >   ORCSHKN-HKN-MAX )  OR
                              (IDH     >=  20              )
               ADD     1                   TO  IDH
               MOVE    ORCSHKN-GMN-HKNCOMBINUM (IDX)
                                       TO  SPA-GMN-THKNCOMBINUM (IDH)
               MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX)
                                       TO  SPA-GMN-THKNCOMBIMEI (IDH)
      *
               MOVE    ORCSHKN-NAI-HKNID (IDX)
                                       TO  SPA-NAI-THKNID  (IDH)
               MOVE    ORCSHKN-NAI-HKNNUM (IDX)
                                       TO  SPA-NAI-THKNNUM  (IDH)
               MOVE    ORCSHKN-NAI-HKNKBN (IDX)
                                       TO  SPA-NAI-THKNKBN  (IDH)
               MOVE    ORCSHKN-NAI-KOH1HKNNUM (IDX)
                                       TO  SPA-NAI-TKOH1HKNNUM (IDH)
               MOVE    ORCSHKN-NAI-KOH2HKNNUM (IDX)
                                       TO  SPA-NAI-TKOH2HKNNUM (IDH)
               MOVE    ORCSHKN-NAI-KOH3HKNNUM (IDX)
                                       TO  SPA-NAI-TKOH3HKNNUM (IDH)
               MOVE    ORCSHKN-NAI-KOH4HKNNUM (IDX)
                                       TO  SPA-NAI-TKOH4HKNNUM (IDH)
      *
               MOVE    ORCSHKN-NAI-FTNRATE (IDX)
                                       TO  SPA-NAI-TFTNRATE (IDH)
      *        四肢区分
               MOVE    ORCSHKN-NAI-RSI-SISIKBN (IDX)
                                       TO  SPA-NAI-TRSI-SISIKBN (IDH)
      *        傷病年月日
               MOVE    ORCSHKN-NAI-RSI-SHOBYOYMD (IDX)
                                       TO  SPA-NAI-TRSI-SHOBYOYMD(IDH)
      *        労災区分
               MOVE    ORCSHKN-NAI-RSI-HKNKBN (IDX)
                                       TO  SPA-NAI-TRSI-HKNKBN  (IDH)
      *
               MOVE    IDH             TO  SPA-HKN-MAX
           END-PERFORM
      *
           MOVE    SPA-GMN-THKNCOMBINUM-G (01)
                                       TO  SPA-GMN-HHKNCOMBI-G
      *
           .
       3008-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   パラメタ削除処理
      *****************************************************************
       3106-PARA-DEL-SEC             SECTION.
      *
      *    パラメタ削除
           INITIALIZE                      PARA-REC
           MOVE    "J02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SRYACCT"           TO  PARA-FILEMEI
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
      *****MOVE    "PARA-DEL11"        TO  ORC-DBPATH
           MOVE    "PARA-KEY2"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    パラメタ削除
           INITIALIZE                      PARA-REC
           MOVE    "J02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "JYURRK"            TO  PARA-FILEMEI
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
      *****MOVE    "PARA-DEL11"        TO  ORC-DBPATH
           MOVE    "PARA-KEY2"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       3106-PARA-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *   受診履歴より診療科決定処理
      *****************************************************************
       310310-JYURRK-CHK-SEC             SECTION.
      *
      *    受診歴を検索する  医療機関、患者ＩＤ
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPID          TO  JYURRK-HOSPID
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-SRYYMD(1:6)
           MOVE    "31"                TO  JYURRK-SRYYMD(7:2)
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "JYURRK-KEY10"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "JYURRK-KEY10"      TO  ORC-DBPATH
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           PERFORM
               UNTIL   FLG-JYURRK          =   1
               IF      SPA-NAI-SRYYM       =   JYURRK-SRYYMD(1:6)
                   MOVE    JYURRK-SRYKA        TO  SPA-J01-SRYKA
                   MOVE    JYURRK-SRYKA        TO  SPA-GMN-SRYKA
               END-IF
      *
               MOVE    "JYURRK-KEY10"      TO  ORC-DBPATH
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
      *
      *    入院で調剤料のみの時のチェック
           IF     (SPA-NYUGAIKBN       =   "1" )  AND
                  (SPA-J01-SRYKA       =   SPACE ) 
      *        診療会計マスターを編集
               INITIALIZE                      SRYACCT-REC 
               MOVE    SPA-HOSPID          TO  ACCT-HOSPID
               MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
               MOVE    SPA-GMN-PTID        TO  ACCT-PTID
               MOVE    SPA-NAI-SRYYM       TO  ACCT-SRYYM
               MOVE    "SRYACCT-KEY7"      TO  WRK-ORC-DBPATH
      *
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    WRK-ORC-DBPATH      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    WRK-ORC-DBPATH      TO  ORC-DBPATH
                   PERFORM 900-SRYACCT-READ-SEC
               ELSE
                   INITIALIZE                  SRYACCT-REC
                   MOVE    1               TO  FLG-SRYACCT
               END-IF
               IF      FLG-SRYACCT         =   ZERO
                   MOVE    ACCT-SRYKA         TO  SPA-J01-SRYKA
                   MOVE    ACCT-SRYKA         TO  SPA-GMN-SRYKA
               END-IF
           END-IF
      *
           .
       310310-JYURRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療科内容編集処理
      *****************************************************************
       3101-SRYKA-HEN-SEC             SECTION.
      *
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SRYKA-MAX
               IF      SPA-J01-SRYKA       =   SPA-GMN-SRYKAL (IDX)
                   MOVE    SPA-GMN-SRYKA-LIST(IDX)
                                           TO  SPA-GMN-SRYKA-G
               END-IF
           END-PERFORM
      *
      *
           .
       3101-SRYKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   受診履歴編集処理
      *****************************************************************
       3103-JYURRK-HEN-SEC             SECTION.
      *
      *    受診歴を検索する  医療機関、患者ＩＤ
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPID          TO  JYURRK-HOSPID
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-J01-SRYKA       TO  JYURRK-SRYKA
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-SRYYMD(1:6)
           MOVE    "01"                TO  JYURRK-SRYYMD(7:2)
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-UPSRYYMD(1:6)
           MOVE    "31"                TO  JYURRK-UPSRYYMD(7:2)
           IF      SPA-J01-SRYKA       =   "00"
               MOVE    "JYURRK-KEY13"      TO  WRK-ORC-DBPATH
           ELSE
               MOVE    "JYURRK-KEY12"      TO  WRK-ORC-DBPATH
           END-IF
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      **** MOVE    "JYURRK-KEY12"      TO  ORC-DBPATH
           MOVE    WRK-ORC-DBPATH      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
      ******** MOVE    "JYURRK-KEY12"      TO  ORC-DBPATH
               MOVE    WRK-ORC-DBPATH      TO  ORC-DBPATH
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           MOVE    ZERO                TO  SPA-RRK-MAX
           INITIALIZE                  SPA-GMN-RRKTBL
           INITIALIZE                  SPA-NAI-RRKTBL
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  FLG-JYURRK-ERR
           PERFORM
               UNTIL   (FLG-JYURRK         =   1   )  OR
                       (FLG-JYURRK-ERR NOT =   ZERO )
      *        診療日、連番で１行とする
               MOVE    ZERO            TO  IDX-S
      *        対象年月のみ
               IF      JYURRK-SRYYMD(1:6)  =   SPA-NAI-SRYYM
      *            診療日、連番で１行とする
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL       IDX2    >   SPA-RRK-MAX
                       IF     (JYURRK-RENNUM       =   SPA-RRK-RENNUM
                                                       (IDX2))  AND
                              (JYURRK-DOUJI-RENNUM =
                                                   SPA-RRK-DOUJI-RENNUM
                                                         (IDX2))  AND
                              (JYURRK-SRYYMD       =   SPA-RRK-RRKYMD
                                                       (IDX2)  )  AND
                              (JYURRK-SRYKA        =   SPA-RRK-SRYKA
                                                       (IDX2)  )
                           MOVE    IDX2            TO  IDX-S
                           MOVE    SPA-RRK-MAX     TO  IDX2
                       END-IF
                   END-PERFORM
               ELSE
                   MOVE    99                  TO  IDX-S
               END-IF
      *
               IF      JYURRK-SRYYMD(1:6)  =   SPA-NAI-SRYYM
                   IF      IDX-S           =   ZERO
                       ADD     1               TO  SPA-RRK-MAX
                       IF      SPA-RRK-MAX     >   100
                           MOVE    1               TO  FLG-JYURRK-ERR
                           MOVE    100             TO  SPA-RRK-MAX
                           MOVE    ZERO            TO  IDX
                       ELSE
                           MOVE    SPA-RRK-MAX     TO  IDX
                       END-IF
                   ELSE
                       MOVE    IDX-S           TO  IDX
                   END-IF
                   IF      IDX             >   ZERO  AND
                                           <=  100
      *                明細項目退避
                       PERFORM 3101-JYURRK-HEN-SEC
                   END-IF
               END-IF
      *
      *********MOVE    "JYURRK-KEY12"      TO  ORC-DBPATH
               MOVE    WRK-ORC-DBPATH      TO  ORC-DBPATH
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           IF      FLG-JYURRK-ERR      =   1
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "0023"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *    診察日の再編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-RRK-MAX
               IF     (SPA-RRK-RENNUM (IDX)    =   1  )  AND
                      (IDX                     <   SPA-RRK-MAX )  AND
                      (SPA-RRK-RRKYMD (IDX)
                                           NOT =   SPA-RRK-RRKYMD
                                                       (IDX + 1))  AND
                      (SPA-RRK-DOUJI-RENNUM (IDX)  =   ZERO  )
                   CONTINUE
               ELSE
                   MOVE    "("                 TO  SPA-GMN-RRKYMD (IDX)
                                                               (10:1)
                   MOVE    SPA-RRK-RENNUM (IDX) 
                                               TO  SPA-GMN-RRKYMD (IDX)
                                                              (11:1)
                   MOVE    ")"                 TO  SPA-GMN-RRKYMD (IDX)
                                                              (12:1)
                   IF      SPA-RRK-DOUJI-RENNUM (IDX)  NOT =   ZERO
                       MOVE    SPA-RRK-DOUJI-RENNUM (IDX) 
                                               TO  SPA-GMN-RRKYMD (IDX)
                                                              (13:1)
                   END-IF
               END-IF
               IF      SPA-RRK-HKNKBN  (IDX)   =   "1"
                   MOVE    "(労)"              TO  SPA-GMN-RRKYMD (IDX)
                                                              (13:)
               END-IF
               IF      SPA-RRK-HKNKBN  (IDX)   =   "2"
                   MOVE    "(自)"              TO  SPA-GMN-RRKYMD (IDX)
                                                              (13:)
               END-IF
           END-PERFORM
      *
           .
       3103-JYURRK-HEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       3101-JYURRK-HEN-SEC             SECTION.
      *
           MOVE    IDX                 TO  SPA-GMN-RRKNO (IDX)
      *    連番
           MOVE    JYURRK-RENNUM       TO  SPA-RRK-RENNUM(IDX)
      *    診療科
           MOVE    JYURRK-SRYKA        TO  SPA-RRK-SRYKA (IDX)
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-SRYKA-MAX
               IF      JYURRK-SRYKA        =   SPA-GMN-SRYKAL (IDY)
                   MOVE    SPA-GMN-SRYKAMEIL(IDY)(1:2)
                                           TO  SPA-GMN-RRKSRYKA (IDX)
               END-IF
           END-PERFORM
      *
      *    同時連番
           MOVE    JYURRK-DOUJI-RENNUM TO  SPA-RRK-DOUJI-RENNUM(IDX)
      *    診察日
           MOVE    JYURRK-SRYYMD       TO  SPA-RRK-RRKYMD(IDX)
           MOVE    JYURRK-SRYYMD       TO  SPA-RRK-RRKYMD-MAE(IDX)
           MOVE    JYURRK-SRYYMD       TO  WRK-SYMD
           PERFORM 41012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-RRKYMD (IDX)
      *    伝票番号
           MOVE    JYURRK-DENPNUM      TO  SPA-RRK-DENPNUM (IDX)
      *    労災用
      *    保険区分
           MOVE    JYURRK-HKNKBN       TO  SPA-RRK-HKNKBN (IDX)
      *    保険組合せ
           MOVE    JYURRK-HKNCOMBINUM  TO  SPA-RRK-HKNCOMBI (IDX)
      *
      *    内部の剤番号
           MOVE    ZERO                TO  IDX-RRK
           PERFORM VARYING     IDX-N   FROM    1   BY  1
                   UNTIL      (IDX-N       >   135  )  OR
                              (SPA-RRK-ZAINUM (IDX IDX-N)  =   ZERO )
               ADD     1               TO  IDX-RRK
           END-PERFORM
           PERFORM VARYING     IDX-N   FROM    1   BY  1
                   UNTIL      (IDX-N       >   15  )  OR
                              (IDX-RRK     >=  135 )  OR
                              (JYURRK-ZAINUM (IDX-N)  =   ZERO )
               ADD     1               TO  IDX-RRK
               MOVE    JYURRK-ZAINUM (IDX-N)   TO  SPA-RRK-ZAINUM
                                                         (IDX IDX-RRK)
           END-PERFORM
      *
           .
       3101-JYURRK-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療会計マスターより画面内容編集処理
      *****************************************************************
       3101-SRYACCT-SPA-SEC             SECTION.
      *
      *    診療会計マスタ内容編集
           MOVE    WRK-BANGO            TO  SPA-NAI-RENNUM  (IDX)
           MOVE    ACCT-SRYKA           TO  SPA-NAI-SRYKA   (IDX)
           MOVE    ACCT-SRYKBN          TO  SPA-NAI-SRYKBN  (IDX)
           MOVE    ACCT-ZAINUM          TO  SPA-NAI-ZAINUM  (IDX)
           MOVE    ACCT-HKNCOMBI        TO  SPA-NAI-HKNCOMBI (IDX)
           MOVE    ACCT-ZAITEN          TO  SPA-NAI-ZAITEN  (IDX)
           MOVE    ACCT-ZAIKAISU        TO  SPA-NAI-ZAIKAISU(IDX)
           MOVE    ACCT-DAY-AREA        TO  SPA-NAI-DAY-AREA (IDX)
      *    労災で外来管理加算の振替えありの時、手技料２がある
           MOVE    ACCT-SYUTEN2         TO  SPA-NAI-SYUTEN2 (IDX)
      *
      *    老人判定
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPID          TO  COMB-HOSPID
           MOVE    SPA-GMN-PTID        TO  COMB-PTID
           MOVE    ACCT-HKNCOMBI       TO  COMB-HKNCOMBINUM
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
               PERFORM 940-HKNCOMBI-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           IF     (COMB-KOH1HKNNUM     =   "027"  )  OR
                  (COMB-KOH2HKNNUM     =   "027"  )  OR
                  (COMB-KOH3HKNNUM     =   "027"  )  OR
                  (COMB-KOH4HKNNUM     =   "027"  )  OR
                  (SPA-J02-ROUJIN      =   1      )
               MOVE    1               TO  SPA-NAI-ROUJIN (IDX)
           ELSE
               MOVE    ZERO            TO  SPA-NAI-ROUJIN (IDX)
           END-IF
      *
           ADD     1                   TO  IDG
           IF      IDG                 >   400
               GO      TO      3101-SRYACCT-SPA-EXT
           END-IF
      *****MOVE    IDX                 TO  SPA-GMN-TBANGO  (IDG)
           MOVE    WRK-BANGO           TO  SPA-GMN-TBANGO  (IDG)
           MOVE    IDG                 TO  SPA-NAI-TBANGO  (IDX)
      *
           MOVE    ZERO                TO  WRK-IDG
      *    診療行為マスター読み込み
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPID         TO  SRY-HOSPID
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *    点数検索用日付編集
           MOVE    ACCT-SRYYM          TO  WRK-SRYYMD(1:6)
           MOVE    01                  TO  WRK-SRYDD
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      ACCT-DAY (1 IDX-D)  NOT =   ZERO
                   MOVE    IDX-D           TO  WRK-SRYDD
                   MOVE    31              TO  IDX-D
               END-IF
           END-PERFORM
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
               PERFORM 930-SRYACT-RAED-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
           MOVE    ZERO            TO  FLG-YAKUSOKU
           PERFORM UNTIL       FLG-SRYACT      =   1
      *
      *        剤内１行目
               IF      SRY-RENNUM      =   1
                   MOVE    ZERO            TO  FLG-YAKUSOKU
                   PERFORM 31011-GYO1-HEN-SEC
               END-IF
      *        剤明細編集
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL      (IDZ         >   5   )  OR
                                  (IDG         >   400 )  OR
                                  (SRY-SRYCD (IDZ) =   SPACE )
      *            自費の時、金額を表示
                   IF      SRY-SRYKBN          =   "95"  OR  "96"
                       PERFORM 31013-JIHIMEI-HEN-SEC
                   ELSE
                       PERFORM 31012-ZAIMEI-HEN-SEC
                   END-IF
      *            約束セットの時
                   IF      SRY-SRYCD (IDZ)(1:1)   =   "S"
                       MOVE    1                  TO  FLG-YAKUSOKU
                   END-IF
               END-PERFORM
      *
               MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
               PERFORM 930-SRYACT-RAED-SEC
           END-PERFORM
      *
           IF      IDG                 >   400
               GO      TO      3101-SRYACCT-SPA-EXT
           END-IF
      *
      *    MOVE    "DBCLOSE"           TO  MCP-FUNC
      *    MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
      *
      *    自費の時、計を表示しない
           IF      ACCT-SRYKBN         =   "95"  OR  "96"
               CONTINUE
           ELSE
      *********点数　編集 （最終行に追加）
      *        IF      SPA-GMN-HEN1 (IDG)      =   SPACE  OR  ALL "　"
      * *          CONTINUE
      *        ELSE
      *           ADD    1                    TO  IDG
      *********END-IF
               PERFORM 31013-INPUTCD-HEN-SEC
               MOVE    WRK-TENSU-X         TO  SPA-GMN-TTENSU  (IDG)
               MOVE    IDG                 TO  SPA-NAI-ENDBANGO(IDX)
      *
           END-IF
      *    剤内容をすべて編集
           MOVE    1                  TO  FLG-HEN-OK
      *
      *    剤終了編集
           ADD     1                   TO  IDG
           IF      IDG                 >   400
               GO      TO      3101-SRYACCT-SPA-EXT
           END-IF
           MOVE    ALL "-"             TO  SPA-GMN-TBLREC (IDG)
      *
           .
       3101-SRYACCT-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤内１行目  編集処理
      *****************************************************************
       31011-GYO1-HEN-SEC                  SECTION.
      *
      *     診療種別区分名称、診療行為区分セット
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE       TO  WRK-SRYSYUMEI
                   WHEN    SRY-SRYSYUKBN   =   MSYU-SRYSYUKBN
                                                      (MSYU-IDX)
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                                TO  WRK-SRYSYUMEI
           END-SEARCH
      *
      *    診療種別がない時、診療行為区分より
           IF      SRY-SRYSYUKBN       =   SPACE
               SET     MSYU-IDX            TO  1
               SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYSYUMEI
                   WHEN    ACCT-SRYKBN     =   MSYU-SRYKBN(MSYU-IDX)
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                               TO  WRK-SRYSYUMEI
               END-SEARCH
           END-IF
      *
      *    包括分の名称編集
           IF      ACCT-HKNCOMBI       =   "9999"
               MOVE    SPACE               TO  WRK-SRYSYUMEI2
               STRING  WRK-SRYSYUMEI       DELIMITED  BY  SPACE
                       "（包括分）"        DELIMITED  BY  SIZE
                                       INTO    WRK-SRYSYUMEI2
               END-STRING
               MOVE    WRK-SRYSYUMEI2      TO  WRK-SRYSYUMEI
           END-IF
      *
           IF      SRY-SRYSYUKBN          =   SPACE
               MOVE    SPACE           TO  SPA-GMN-TMEISYO (IDG)
               MOVE    WRK-SRYSYUMEI   TO  SPA-GMN-TMEISYO (IDG)(7:)
           ELSE
               MOVE    "."             TO  SPA-GMN-TMEISYO (IDG)(1:1)
               MOVE    SRY-SRYSYUKBN   TO  SPA-GMN-TMEISYO (IDG)(2:3)
               MOVE    WRK-SRYSYUMEI   TO  SPA-GMN-TMEISYO (IDG)(7:)
           END-IF
      *
           MOVE    SRY-SRYSYUKBN       TO  SPA-NAI-SRYSYU (IDG)
      *保険組合せ
           MOVE    ACCT-HKNCOMBI       TO  SPA-GMN-THKNCOMBI (IDG)
      *回数
           PERFORM VARYING    IDK      FROM    1   BY  1
                   UNTIL      IDK      >   31
      *        ３桁判定
               IF      ACCT-DAY  (1 IDK)   >=  100
                   MOVE    ACCT-DAY  (1 IDK)   TO  WRK-ZZZ
                   MOVE    WRK-ZZZ-X           TO  SPA-GMN-TDAY(IDG IDK)
               ELSE
                   MOVE    ACCT-DAY  (1 IDK)   TO  WRK-ZZ
                   MOVE    WRK-ZZ-X            TO  SPA-GMN-TDAY(IDG IDK)
               END-IF
           END-PERFORM
      *    合計
           IF      SPA-NAI-ZAIKAISU(IDX)   >=  100
               MOVE    SPA-NAI-ZAIKAISU(IDX)   TO  WRK-ZZ9
               MOVE    WRK-ZZ9-X               TO  SPA-GMN-TDAYGOK(IDG)
           ELSE
               MOVE    SPA-NAI-ZAIKAISU(IDX)   TO  WRK-Z9
               MOVE    WRK-Z9-X                TO  SPA-GMN-TDAYGOK(IDG)
           END-IF
      *
      *    診療科が全科の時、次の行へ科名称を編集
           IF     SPA-J01-SRYKA        =   ZERO
               MOVE    SPACE                   TO  WRK-SRYKAMEI
               PERFORM VARYING    IDK      FROM    1   BY  1
                       UNTIL      IDK      >   SPA-SRYKA-MAX
                   IF      ACCT-SRYKA      =   SPA-GMN-SRYKAL (IDK)
                       MOVE    SPA-GMN-SRYKAMEIL (IDK)
                                               TO  WRK-SRYKAMEI
                       MOVE    SPA-SRYKA-MAX   TO  IDK
                   END-IF
               END-PERFORM
               IF      IDG                 <   400
                   MOVE    WRK-SRYKAMEI(1:2)   TO  SPA-NAI-SRYKAMEI
                                                              (IDG + 1)
               END-IF
           END-IF
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
           IF      IDG                 >   400
               GO      TO      31012-ZAIMEI-HEN-EXT
           END-IF
      *
      *    約束セット編集
           IF      SRY-SRYCD (IDZ)(1:1)   =   "S"
               PERFORM 310120-YAKUSOKU-SET-SEC
               GO      TO      31012-ZAIMEI-HEN-EXT
           END-IF
      *
      *    診療コードより診療行為名称を取得
           MOVE    SRY-SRYCD (IDZ)     TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU       NOT =   ZERO
               MOVE    SRY-SRYCD (IDZ)     TO  TNS-NAME
           END-IF
      *    行為名称
           MOVE    TNS-NAME            TO  SPA-GMN-TMEISYO (IDG)
      *    薬剤服用コメントの時、再編集
           IF      SRY-SRYCD (IDZ)(1:3)    =   "001"
                PERFORM 3100121-FUKIYO-MEISYO-SEC
                MOVE    WRK-MEISYO    TO  SPA-GMN-TMEISYO (IDG)
           END-IF
      *
      *    約束の内容の時、再編集
           IF      FLG-YAKUSOKU        =   1
               MOVE    SPA-GMN-TMEISYO (IDG)   TO  WRK-MEISYO
               MOVE    SPACE                   TO  SPA-GMN-TMEISYO(IDG)
               STRING     "＞"             DELIMITED   BY  SIZE
                          WRK-MEISYO       DELIMITED   BY  SIZE
                          INTO             SPA-GMN-TMEISYO (IDG)
               END-STRING
           END-IF
      *
      *    コメントの時、内容設定
           IF    ((SRY-SRYCD  (IDZ)(1:1)   =   "8"  )  OR
                  (SRY-SRYCD  (IDZ)(1:3)   =   "008"))  AND
                  (SRY-INPUTNUM(IDZ)   NOT =   ZERO  )
               PERFORM 310122-KNJCOM-SET-SEC
           END-IF
      *
      *    数量（薬剤、器材のとき）
           IF      SRY-SRYCD  (IDZ)(1:1)   =   "6"  OR  "7"
               MOVE    SRY-SRYSURYO (IDZ)  TO  WRK-SURYO
               PERFORM 310121-SURYO-HEN-SEC
               MOVE    WRK-SURYO-X         TO  SPA-GMN-TSURYOU (IDG)
      *********MOVE    TNS-TANINAME        TO  SPA-GMN-TTANI1  (IDG)
           END-IF
      *    分画数
           IF     (SRY-SRYCD  (IDZ)(1:1)   =   "7"  )  AND
                  (TNS-DATAKBN             =   3    )  AND
                  (SRY-SRYKAISU (IDZ)      >   ZERO )
               MOVE    SRY-SRYSURYO (IDZ)  TO  WRK-BUNGSU-Z1
               MOVE    SRY-SRYKAISU (IDZ)  TO  WRK-BUNGSU-Z2
               MOVE    SPACE               TO  SPA-GMN-TSURYOU (IDG)
               STRING  WRK-BUNGSU-Z1           DELIMITED   BY  SIZE
                       "/"                     DELIMITED   BY  SIZE
                       WRK-BUNGSU-Z2           DELIMITED   BY  SIZE
                       INTO                    SPA-GMN-TSURYOU (IDG)
               END-STRING
      *********PERFORM 310122-BUNGSU-HEN-SEC
      *        MOVE    WRK-BUNGSU-Z        TO  SPA-GMN-TTANMEI (IDG)
      *********MOVE    "分画"              TO  SPA-GMN-TTANI2  (IDG)
           END-IF
      *
      *    撮影回数（フィルムなしの場合）
           IF     (SRY-SRYCD  (IDZ)(1:1)   =   "1" )  AND
                  (SRY-AUTOKBN(IDZ)        =   "S" )  AND
                  (SRY-SRYSURYO (IDZ)      >   1   )
               MOVE    SRY-SRYSURYO (IDZ)  TO  WRK-SURYO
               PERFORM 310121-SURYO-HEN-SEC
               MOVE    WRK-SURYO-X         TO  SPA-GMN-TSURYOU (IDG)
           END-IF
      *
      *    老人適用区分（同一剤は同じ）
           IF      TNS-ROUTEKKBN       =   ZERO
               IF      SPA-NAI-ROUTEKKBN(IDX)  =   ZERO
                   MOVE    TNS-ROUTEKKBN   TO  SPA-NAI-ROUTEKKBN(IDX)
               END-IF
           ELSE
               MOVE    TNS-ROUTEKKBN       TO  SPA-NAI-ROUTEKKBN(IDX)
           END-IF
      *    内服の逓減の時、マイナス
           IF      SRY-SRYCD  (IDZ)        =   "630010002"
               MOVE    1                   TO  SPA-NAI-ZAITENFLG (IDX)
           END-IF
      *
      *    コメント以外の最終行セット
           IF      SRY-SRYCD  (IDZ)(1:1)   =   "0"  OR  "8"
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
           IF      IDG                 >   400
               GO      TO      31013-JIHIMEI-HEN-EXT
           END-IF
      *
      *    約束セット編集
           IF      SRY-SRYCD (IDZ)(1:1)   =   "S"
               PERFORM 310120-YAKUSOKU-SET-SEC
               GO      TO      31013-JIHIMEI-HEN-EXT
           END-IF
      *
      *    診療コードより診療行為名称を取得
           MOVE    SRY-SRYCD (IDZ)     TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU       NOT =   ZERO
               MOVE    SRY-SRYCD (IDZ)     TO  TNS-NAME
           END-IF
      *    行為名称
      *****MOVE    TNS-NAME            TO  SPA-GMN-TMEISYO2(IDG)
           MOVE    TNS-NAME            TO  SPA-GMN-TMEISYO (IDG)
      *
      *    約束の内容の時、再編集
           IF      FLG-YAKUSOKU        =   1
               MOVE    SPA-GMN-TMEISYO (IDG)   TO  WRK-MEISYO
               MOVE    SPACE               TO  SPA-GMN-TMEISYO (IDG)
               STRING     "＞"             DELIMITED   BY  SIZE
                          WRK-MEISYO       DELIMITED   BY  SIZE
                          INTO             SPA-GMN-TMEISYO (IDG)
               END-STRING
           END-IF
      *
      *    コメントの時、内容設定
           IF    ((SRY-SRYCD  (IDZ)(1:1)   =   "8"   ) OR
                  (SRY-SRYCD  (IDZ)(1:3)   =   "008" )) AND
                  (SRY-INPUTNUM(IDZ)   NOT =   ZERO  )
               PERFORM 310122-KNJCOM-SET-SEC
           END-IF
      *
      *    計
           MOVE    SRY-JIHIMONEY (IDZ)
                                       TO  WRK-KEI
           MOVE    SPACE               TO  WRK-ZAIKEI
           MOVE    WRK-KEI-X           TO  WRK-ZAIKEI(10:)
      *
      *
      *    日本語変換
      *    MOVE    SPACE               TO  WRK-ZAIKEIJ
      *    MOVE    1                   TO  IDJ2
      *    PERFORM VARYING     IDJ     FROM    1  BY  1
      *            UNTIL       IDJ     >   40
      *       EVALUATE     WRK-ZAIKEI (IDJ:1)
      *            WHEN    SPACE
      *                MOVE    "　"            TO  WRK-ZAIKEIJ (IDJ2:2)
      *                ADD     2               TO  IDJ2
      *            WHEN    "X"
      *                MOVE    "×"            TO  WRK-ZAIKEIJ (IDJ2:2)
      *                ADD     2               TO  IDJ2
      *            WHEN    "1"  THRU  "9"
      *                MOVE    WRK-ZAIKEI (IDJ:1)  TO  IDJ1
      *                MOVE    TBL-SUJI (IDJ1)     TO  WRK-ZAIKEIJ (IDJ2:2)
      *                ADD     2                   TO  IDJ2
      *            WHEN    "0"
      *                MOVE    TBL-SUJI (10)       TO  WRK-ZAIKEIJ (IDJ2:2)
      *                ADD     2                   TO  IDJ2
      *        END-EVALUATE
      *****END-PERFORM
      *
           IF      SRY-JIHIMONEY (IDZ)     =   ZERO
               MOVE    SPACE               TO  SPA-GMN-TTENSU (IDG)
           ELSE
      ******** MOVE    "円"                TO  WRK-ZAIKEIJ(37:2)
      *******  MOVE    WRK-ZAIKEI          TO  SPA-GMN-TZAIKEI (IDG)
               MOVE    WRK-KEI-X           TO  SPA-GMN-TTENSU (IDG)
           END-IF
      *
      *    コメント以外の最終行セット
           IF      SRY-SRYCD (IDZ)(1:1)    =   "0"  OR  "8"
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
           MOVE    SRY-SRYCD (IDZ)(1:6)
                                       TO  ICD-INPUTCD
      *
           PERFORM 930-INPUTCD-READ-SEC
      *
           IF      FLG-INPUTCD         =   ZERO
               MOVE    ICD-DSPNAME         TO  SPA-GMN-TMEISYO (IDG)
           ELSE
               MOVE    SRY-SRYCD (IDZ)     TO  SPA-GMN-TMEISYO (IDG)
           END-IF
      *
      *    数量（薬剤、器材のとき）
           MOVE    SRY-SRYSURYO (IDZ)  TO  WRK-SURYO
           PERFORM 310121-SURYO-HEN-SEC
           MOVE    WRK-SURYO-X         TO  SPA-GMN-TSURYOU (IDG)
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
           MOVE    ZERO                TO  WRK-SURYO-Z3
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
           IF      WRK-SURYO-Z3-X      NOT =   SPACE
               MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z19
               MOVE    "."                 TO  WRK-SURYO-Z2
           END-IF
      *
           .
       310121-SURYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントから編集する 
      *****************************************************************
       310122-KNJCOM-SET-SEC              SECTION.
      *
           INITIALIZE                      PTCOM-REC
           MOVE    SRY-HOSPID          TO  PTCOM-HOSPID
           MOVE    SRY-PTID            TO  PTCOM-PTID
           MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
           MOVE    SRY-SRYCD (IDZ)     TO  PTCOM-SRYCD
           MOVE    SRY-INPUTNUM(IDZ)   TO  PTCOM-RENNUM
           PERFORM 950-PTCOM-READ-SEC
           IF      FLG-PTCOM           =   ZERO
               IF      PTCOM-INPUTCOMENT  NOT =   SPACE
                   MOVE    PTCOM-INPUTCOMENT  TO  SPA-GMN-TMEISYO (IDG)
               END-IF
               MOVE    PTCOM-INPUTCHI (1)  TO  SPA-NAI-INPUTCHI1(IDG)
               MOVE    PTCOM-INPUTCHI (2)  TO  SPA-NAI-INPUTCHI2(IDG)
               MOVE    PTCOM-INPUTCHI (3)  TO  SPA-NAI-INPUTCHI3(IDG)
               MOVE    PTCOM-INPUTCHI (4)  TO  SPA-NAI-INPUTCHI4(IDG)
           END-IF
      *
           .
       310122-KNJCOM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   入力コードの画面用再編集処理
      *****************************************************************
       31013-INPUTCD-HEN-SEC                  SECTION.
      *
      *    点数＊回数　計
           MOVE    SPA-NAI-ZAITEN  (IDX)   TO  WRK-TENSU
           IF      SPA-NAI-ZAITENFLG (IDX) =   1
               COMPUTE WRK-TENSU-2         =   SPA-NAI-ZAITEN  (IDX)
                                           *   -1
           END-IF
      *
           .
       31013-INPUTCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
      *    コラムリスト位置保存
           IF      FLG-GMN-INIT        =   1
               MOVE    ZERO                TO  WRK-J02-DATALIST-ROW
               MOVE    ZERO                TO  WRK-J02-RRKLIST-ROW
           ELSE
               MOVE    J02-DATALIST-ROW    TO  WRK-J02-DATALIST-ROW
               MOVE    J02-RRKLIST-ROW     TO  WRK-J02-RRKLIST-ROW
           END-IF
      *
           MOVE    SPACE               TO  J02
      *
      *    コラムリストの位置指定
           MOVE    ZERO                TO  J02-DATALIST-ROW
           MOVE    ZERO                TO  J02-DATALIST-ROWATTR
           MOVE    ZERO                TO  J02-RRKLIST-ROW
           MOVE    ZERO                TO  J02-RRKLIST-ROWATTR
           IF      WRK-J02-DATALIST-ROW    NOT =   ZERO
               MOVE    WRK-J02-DATALIST-ROW    TO  J02-DATALIST-ROW
           END-IF
           IF      WRK-J02-RRKLIST-ROW     NOT =   ZERO
               MOVE    WRK-J02-RRKLIST-ROW     TO  J02-RRKLIST-ROW
           END-IF
      *
           MOVE    SPA-GMN-PTNUM       TO  J02-PTNUM
           MOVE    SPA-GMN-KANANAME    TO  J02-KANANAME
           MOVE    SPA-GMN-NAME        TO  J02-NAME
           MOVE    SPA-GMN-SEX         TO  J02-SEX
           MOVE    SPA-GMN-SRYYM       TO  J02-SRYYM
           MOVE    SPA-GMN-BIRTHDAYW   TO  J02-BIRTHDAY
      *****MOVE    SPA-SRYKAMEI        TO  J02-SRYKA
           MOVE    SPA-GMN-SRYKA-G     TO  J02-SRYKA
      *    診療科
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-SRYKA-MAX
               MOVE    SPA-GMN-SRYKA-LIST (IDX)
                                          TO  J02-SRYKA-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-SRYKA-MAX       TO  J02-SRYKA-COUNT
      *    入外区分
           MOVE    SPA-GMN-NYUGAIKBN-G TO  J02-NYUGAIKBN
           PERFORM VARYING   IDX     FROM    1   BY  1
                   UNTIL     IDX         >   SPA-NYUGAI-MAX
               MOVE    SPA-GMN-NYUGAIKBN-LST (IDX)
                                          TO  J02-NYUGAIKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-NYUGAI-MAX      TO  J02-NYUGAIKBN-COUNT
      *    診療種別
           MOVE    SPA-GMN-SRYKBN-G    TO  J02-SRYKBN
           PERFORM VARYING   IDX     FROM    1   BY  1
                   UNTIL     IDX         >   SPA-SRYKBN-MAX
               MOVE    SPA-GMN-SRYKBN-LST (IDX)
                                          TO  J02-SRYKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-SRYKBN-MAX         TO  J02-SRYKBN-COUNT
      *
      *    保険組合せ
           MOVE    SPA-GMN-HHKNCOMBI-G TO  J02-HHKNCOMBI
           PERFORM VARYING   IDX     FROM    1   BY  1
                   UNTIL     IDX         >   SPA-HKN-MAX
               MOVE    SPA-GMN-THKNCOMBINUM-G (IDX)
                                          TO  J02-HHKNCOMBI-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-HKN-MAX         TO  J02-HHKNCOMBI-COUNT
      *
      *    受診履歴
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-RRK-MAX
               MOVE    SPA-GMN-RRKNO  (IDX)    TO  WRK-ZZZ
               MOVE    WRK-ZZZ-X               TO  J02-RRKNO   (IDX)
               MOVE    SPA-GMN-RRKYMD (IDX)    TO  J02-RRKYMD  (IDX)
               IF     (SPA-NAI-RRKSELNUM       >   ZERO   )   AND
                      (SPA-GMN-RRKNO  (IDX)    =   SPA-NAI-RRKSELNUM)
                   MOVE    "T"             TO  J02-RRKLIST-SELECT(IDX)
               ELSE
                   MOVE    "F"             TO  J02-RRKLIST-SELECT(IDX)
               END-IF
      *        全科の時科指定
               IF      SPA-J01-SRYKA       =   ZERO
      *            MOVE    SPA-GMN-RRKSRYKA(IDX)  TO  J02-RRKYMD  (IDX)
      *                                                      (17:4)
      *************MOVE    SPA-GMN-RRKSRYKA(IDX)  TO  J02-RRKSRYKA(IDX)
                   MOVE    SPA-GMN-RRKSRYKA(IDX)  TO  J02-RRKSRYKA(IDX)
               END-IF
      *        保険組合せ
               MOVE    SPA-RRK-HKNCOMBI (IDX)  TO  J02-RRKHKNCOMBI (IDX)
      *
           END-PERFORM
           MOVE    SPA-RRK-MAX         TO  J02-RRKLIST-COUNT
      *
           MOVE    SPA-GMN-RRKSELNUM   TO  J02-RRKSELNUM
      *    訂正日
           MOVE    SPA-GMN-HENSRYYMD   TO  J02-HENSRYYMD
      *
      *    明細
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-GMN-MAX
               IF      SPA-GMN-TMEISYO (IDX)   =   ALL "-"
                   MOVE    ALL "-"         TO  J02-DATALIST-ITEM(IDX)
               ELSE
                   IF      SPA-GMN-TBANGO (IDX)    >   ZERO
                       MOVE    SPA-GMN-TBANGO  (IDX)   TO  WRK-ZZZZ
                       MOVE    WRK-ZZZZ-X          TO  J02-TNO    (IDX)
                   END-IF
                   IF      SPA-NAI-SRYKAMEI(IDX)   NOT =   SPACE
                       MOVE    SPA-NAI-SRYKAMEI(IDX)
                                                   TO  J02-TNO    (IDX)
                   END-IF
      *
                   MOVE    SPA-GMN-TMEISYO (IDX)   TO  J02-TMEISYO(IDX)
                   MOVE    SPA-GMN-TSURYO  (IDX)   TO  J02-TSURYO (IDX)
                   MOVE    SPA-GMN-TTENSU  (IDX)   TO  J02-TTENSU (IDX)
                   MOVE    SPA-GMN-THKNCOMBI(IDX)  TO  J02-THKNCOMBI
                                                                 (IDX)
                   MOVE    SPA-GMN-TDAYGOK (IDX)   TO  J02-TDAYGOK(IDX)
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   31
                       MOVE    SPA-GMN-TDAY (IDX IDY)
                                                   TO  J02-TDAY
                                                             (IDX IDY)
                   END-PERFORM
      *            入院で調剤料の時、＊表示をする
                   IF     (SPA-NAI-SRYYM       >=  SPA-SYSYMD(1:6)) AND
                          (SPA-GMN-NYUGAIKBN   =   "1" )  AND
                          (SPA-GMN-TMEISYO (IDX)(2:3)
                                               =   "240" OR "260")
                       MOVE    ALL "*"         TO  J02-TDAY-G (IDX)
                   END-IF
               END-IF
      *
               IF     (SPA-GMN-TMEISYO (IDX)   NOT =   ALL "-" )  AND
                      (SPA-GMN-TBANGO(IDX) NOT =   ZERO  )  AND
                      (SPA-GMN-TBANGO(IDX)     =   SPA-GMN-BANGO )
                   MOVE    "T"             TO  J02-DATALIST-SELECT(IDX)
               ELSE
                   MOVE    "F"             TO  J02-DATALIST-SELECT(IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-MAX         TO  J02-DATALIST-COUNT
      *
      *
           IF      SPA-GMN-BANGO   NOT =   ZERO
               MOVE    SPA-GMN-BANGO       TO  WRK-ZZZZ
               MOVE    WRK-ZZZZ-X          TO  J02-NUM
           END-IF
      *
           MOVE    SPA-GMN-MEISYO      TO  J02-MEISYO
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   31
               IF      SPA-GMN-DAY (IDX)    <   100
                   MOVE    SPA-GMN-DAY (IDX)    TO  WRK-ZZ
                   MOVE    WRK-ZZ-X             TO  J02-DAY(IDX)
               ELSE
                   MOVE    SPA-GMN-DAY (IDX)    TO  J02-DAY(IDX)
               END-IF
           END-PERFORM
      *
      *
           IF      SPA-GMN-HKNCOMBI   NOT =   ZERO
               MOVE    SPA-GMN-HKNCOMBI   TO  J02-HKNCOMBI
           END-IF
           MOVE    SPA-GMN-HKNCOMBIMEI    TO  J02-HKNCOMBIMEI
      *
           MOVE    SPA-GMN-BSRYKAMEI      TO  J02-SRYKAMEI
      *    入院の時、入院日
           IF      SPA-GMN-NYUGAIKBN   =   "1"
               MOVE    "入院期間："       TO  J02-NYUINKIKAN
               MOVE    SPA-GMN-NYUINYMD   TO  J02-NYUINKIKAN(11:9)
               MOVE    "-"                TO  J02-NYUINKIKAN(20:1)
               MOVE    SPA-GMN-TAIINYMD   TO  J02-NYUINKIKAN(21:9)
           ELSE
               MOVE    SPACE              TO  J02-NYUINKIKAN
           END-IF
      *
           MOVE    SPA-GMN-NYUINMSG(1) TO  J02-NYUINMSG1-VALUE
           MOVE    SPA-GMN-NYUINMSG(2) TO  J02-NYUINMSG2-VALUE
      *
           MOVE    "red"               TO  J02-NYUINMSG1-STYLE
           MOVE    "red"               TO  J02-NYUINMSG2-STYLE
      *
      *    非活性処理
           PERFORM 510-GSRAUTH-SEC
      *    カーソル
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
      *    履歴選択
      *    MOVE    WIDGET-INSENSITIVE  TO  J02-RRKSELNUM-STATE
      *
      *    IF      SPA-J02-SYORI       =   1  OR  2
      *        MOVE    WIDGET-NORMAL       TO  J02-RRKSELNUM-STATE
      *    END-IF
      *
      *    訂正日入力
      *    MOVE    WIDGET-INSENSITIVE  TO  J02-HENSRYYMD-STATE
      *
      *    IF      SPA-J02-SYORI       =   2
      *        MOVE    WIDGET-NORMAL       TO  J02-HENSRYYMD-STATE
      *    END-IF
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
      *
      *    排他エラーの時、患者番号へ
           IF      SPA-ERRCD           =   "9999"
               MOVE    00              TO  SPA-GMN-CUR
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    00
                   MOVE    "PTNUM"         TO  MCP-WIDGET
               WHEN    01
                   MOVE    "NUM"           TO  MCP-WIDGET
               WHEN    02
                   MOVE    "DAY"           TO  MCP-WIDGET
                   IF      SPA-DAY-IDX         =   ZERO  OR
                                               >   SPA-SRYMATU
                       MOVE    1               TO  SPA-DAY-IDX
                   END-IF
                   IF     (SPA-BIRTHDAY(1:6)   =   SPA-NAI-SRYYM )  AND
                          (SPA-DAY-IDX         <   SPA-BIRTHDAY(7:2))
                       MOVE    SPA-BIRTHDAY(7:2)   TO  SPA-DAY-IDX
                   END-IF
                   MOVE    SPA-DAY-IDX         TO  MCP-WIDGET(4:2)
               WHEN    03
                   MOVE    "RRKSELNUM"     TO  MCP-WIDGET
               WHEN    04
                   MOVE    "HENSRYYMD"     TO  MCP-WIDGET
               WHEN    05
                   MOVE    "B08"           TO  MCP-WIDGET
               WHEN    06
                   MOVE    "SRYKA"         TO  MCP-WIDGET
               WHEN    07
                   MOVE    "SRYYM"         TO  MCP-WIDGET
               WHEN    08
                   MOVE    "NYUGAIKBN"     TO  MCP-WIDGET
               WHEN    09
                   MOVE    "SRYKBN"        TO  MCP-WIDGET
               WHEN    10
                   MOVE    "HHKNCOMBI"     TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
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
      *    受診日訂正へ
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 440-SRYYMD-TEISEI-SEC
      *    前月選択
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 41018-ZENGETU-HEN-SEC
      **   前頁
               WHEN    "CLICKED"       ALSO    "B06S"
                   PERFORM 440-MAEGMN-SEC
      *    次頁
               WHEN    "CLICKED"       ALSO    "B07S"
                   PERFORM 450-ATOGMN-SEC
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
      *     登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOROKU-SEC
      *     保険一括変更
               WHEN    "CLICKED"       ALSO    "B10S"
                   PERFORM 4100-HKNHEN-SEC
      *     入院調剤料変更
               WHEN    "CLICKED"       ALSO    "B11S"
                   PERFORM 4110-CHOZAI-SEC
      *     印刷
               WHEN    "CLICKED"       ALSO    "B12S"
                   PERFORM 4120-PRINT-SEC
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
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        患者番号入力
               WHEN    "ACTIVATE"      ALSO    "PTNUM"
                   PERFORM 41010-PTNUM-SYORI-SEC
      *        診療科入力
               WHEN    "ACTIVATE"      ALSO    "SRYKA"
                   PERFORM 41013-SRYKAI-SYORI-SEC
      *        診療年月
               WHEN    "ACTIVATE"      ALSO    "SRYYM"
                   PERFORM 41018-SRYYMD-SYORI-SEC
      *        番号選択
               WHEN    "ACTIVATE"      ALSO    "NUM"
                   PERFORM 41011-BANGO-CHK-SEC
      *        明細選択
               WHEN    ANY             ALSO    "DATALIST"
                   PERFORM 41012-DATALIST-SEC
      *        保険組合せ
               WHEN    "ACTIVATE"      ALSO    "HHKNCOMBI"
                   PERFORM 41017-HKNCOMBI-CHK-SEC
      *        受診履歴選択
               WHEN    "ACTIVATE"      ALSO    "RRKSELNUM"
                   PERFORM 41014-RRKSELNUMI-CHK-SEC
      *        明細選択
               WHEN    ANY             ALSO    "RRKLIST"
                   PERFORM 41015-RRKLIST-SEC
      *        変更受診日
               WHEN    "ACTIVATE"      ALSO    "HENSRYYMD"
                   PERFORM 41016-HENSRYYMD-CHK-SEC
      *        入外区分
               WHEN    "ACTIVATE"      ALSO    "NYUGAIKBN"
                   PERFORM 41017-NYUGAIKBN-CHK-SEC
      *        診療種別
               WHEN    "ACTIVATE"      ALSO    "SRYKBN"
                   PERFORM 41018-SRYKBN-CHK-SEC
      *
               WHEN    OTHER
      *
      *            剤回数
                   IF    (MCP-EVENT       =   "ACTIVATE"  ) AND
                         (MCP-WIDGET(1:3) =   "DAY"       )
                       MOVE    MCP-WIDGET(4:2)     TO  SPA-DAY-IDX
                       PERFORM 41012-DAY-CHK-SEC
                   END-IF
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号入力処理
      *****************************************************************
       41010-PTNUM-SYORI-SEC         SECTION.
      *
           MOVE    J02-PTNUM           TO  SPA-GMN-PTNUM
      *
      **** INITIALIZE                      ORCSPTIDAREA
      *    MOVE    SPA-HOSPID          TO  SPTID-HOSPID
      *    MOVE    SPA-GMN-PTNUM       TO  SPTID-PTNUM
      *    CALL    "ORCSPTID"          USING   ORCSPTIDAREA
      *                                        SPA-1009-TBL
      *    IF      SPTID-RC        NOT =   ZERO
      *        INITIALIZE                  SPA-J02-AREA
      *        INITIALIZE                  SPA-SCR-REC
      *        MOVE    SPA-J02-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM
      *        MOVE    SPA-J02-SRYYMD (1:6)    TO  SPA-NAI-SRYYM
      *        MOVE    "0016"              TO  SPA-ERRCD
      *        GO      TO     41010-PTNUM-SYORI-EXT
      **** END-IF
      *
           INITIALIZE                      ORCSPTNUMAREA
           MOVE    SPA-GMN-PTNUM       TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
           EVALUATE    SPTNUM-RC
               WHEN    00
                   MOVE    SPTNUM-PTNUM        TO  SPTID-PTNUM
                   MOVE    SPTNUM-PTID         TO  SPTID-PTID
               WHEN   10
                   INITIALIZE                  SPA-J02-AREA
                   INITIALIZE                  SPA-SCR-REC
                   INITIALIZE                  SPA-GMN-NYUIN-AREA
                   MOVE    SPA-J02-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM
                   MOVE    SPA-J02-SRYYMD (1:6)    TO  SPA-NAI-SRYYM
                   MOVE    "0017"              TO  SPA-ERRCD
                   GO      TO     41010-PTNUM-SYORI-EXT
               WHEN    20
                   MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
      *            氏名検索へ
                   MOVE    SPACE               TO  LINKAREA
                   MOVE    SPA-KYOUTU          TO  LNK-COMMON
                   MOVE    SPA-J02             TO  LNK-SCRSPA
      *
                   MOVE    "J02"               TO  SPA-MOTOPG
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
                   INITIALIZE                  SPA-J02-AREA
                   INITIALIZE                  SPA-SCR-REC
                   INITIALIZE                  SPA-GMN-NYUIN-AREA
                   MOVE    SPA-J02-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM
                   MOVE    SPA-J02-SRYYMD (1:6)    TO  SPA-NAI-SRYYM
                   MOVE    "0017"              TO  SPA-ERRCD
                   GO      TO     41010-PTNUM-SYORI-EXT
           END-EVALUATE
      *
           MOVE    SPTID-PTNUM         TO  SPA-GMN-PTNUM
           MOVE    SPTID-PTID          TO  SPA-GMN-PTID
      *    患者マスター存在チェック
           MOVE    SPA-HOSPID         TO  PTINF-HOSPID
           MOVE    SPA-GMN-PTID       TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF      NOT =   ZERO
               MOVE    SPACE               TO  SPA-GMN-NAME
               MOVE    "0017"              TO  SPA-ERRCD
               GO      TO      41010-PTNUM-SYORI-EXT
           END-IF
      *
           MOVE    PTINF-NAME          TO  SPA-GMN-NAME
           MOVE    PTINF-KANANAME      TO  SPA-GMN-KANANAME
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"                TO  SPA-GMN-SEX
               WHEN    "2"
                   MOVE    "女"                TO  SPA-GMN-SEX
           END-EVALUATE
      *
           MOVE    PTINF-BIRTHDAY      TO  SPA-GMN-BIRTHDAY
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 41012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-BIRTHDAYW
      *
      *    排他制御処理
           PERFORM 990-LOCK-IN-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41010-PTNUM-SYORI-EXT
           END-IF
      *
      *
      *    前回患者
           MOVE    SPA-GMN-PTNUM       TO  SPA-LAST-PTNUM
           MOVE    SPA-GMN-PTID        TO  SPA-LAST-PTID
      *
      *    診療科をクリアする
           IF      SPA-SRYKA-MAX       =   1
               MOVE    SPA-GMN-SRYKA-LIST(01)
                                           TO  SPA-GMN-SRYKA-G
               MOVE    SPA-GMN-SRYKA       TO  SPA-J01-SRYKA
               MOVE    SPA-GMN-SRYKAMEI    TO  SPA-J01-SRYKAMEI
               MOVE    1                   TO  FLG-HENSYU
           ELSE
               MOVE    SPACE               TO  SPA-GMN-SRYKA-G
               MOVE    SPACE               TO  SPA-J01-SRYKA
               MOVE    SPACE               TO  SPA-J01-SRYKAMEI
      ******** MOVE    ZERO                TO  FLG-HENSYU
               MOVE    1                   TO  FLG-HENSYU
           END-IF
      *    入院が存在するかのチェック
           IF      SPA-BEDSU           >   ZERO
               PERFORM 410101-NYUINRRK-CHK-SEC
               IF      SPA-NAI-NYUINKA NOT =   SPACE
                   MOVE    SPA-NAI-NYUINKA     TO  SPA-J01-SRYKA
                   PERFORM 3101-SRYKA-HEN-SEC
               END-IF
           END-IF
      *
           INITIALIZE                  SPA-GMN-RRKTBL
           INITIALIZE                  SPA-NAI-RRKTBL
           INITIALIZE                  SPA-NAIGMN-AREA
           INITIALIZE                  SPA-GMN-NYUIN-AREA
      *
      *    パラメタファイルのクリア
           PERFORM 3106-PARA-DEL-SEC
      *
           PERFORM 310-SPA-SET-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (FLG-HENSYU          =   ZERO  )
               GO      TO      41010-PTNUM-SYORI-EXT
           END-IF
      *
      *    受診履歴編集
           PERFORM 3103-JYURRK-HEN-SEC
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
      *    保存用パラメタ作成
           PERFORM 4201-SRYACCT-PARA-SEC
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-STRNUM
      *
      *    診療会計マスターより編集をする
           PERFORM 410181-SRYACCT-HEN-SEC
      *
      *    保険組合一覧編集
      **** PERFORM 3105-HKNCOMBI-HEN-SEC
      *
           IF     (SPA-GMN-SRYKA-G     =   SPACE )  OR
                  (SPA-GMN-MAX         =   ZERO  )
               MOVE    6                   TO  SPA-GMN-CUR
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *    受診履歴が無い時、メッセージ表示
           IF      SPA-RRK-MAX         =   ZERO
               IF      SPA-SRYKA-MAX       =   1
                   MOVE    07              TO  SPA-GMN-CUR
               ELSE
                   MOVE    06              TO  SPA-GMN-CUR
               END-IF
           END-IF
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
      *    入院履歴判定
           MOVE    SPACE               TO  PTNYUINRRK-REC
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPID          TO  PTNYUINRRK-HOSPID
           MOVE    SPA-GMN-PTID        TO  PTNYUINRRK-PTID
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
           MOVE    SPACE               TO  SPA-NAI-NYUINYMD
           MOVE    SPACE               TO  SPA-GMN-TAIINYMD
           MOVE    SPACE               TO  SPA-NAI-NYUINYMD
           MOVE    SPACE               TO  SPA-GMN-NYUINYMD
           MOVE    SPACE               TO  SPA-NAI-NYUINKA
      *
      *    入院が１件もないとき、外来とする
           IF      FLG-PTNYUINRRK      =   1
               MOVE    "2"             TO  SPA-NYUGAIKBN
               GO      TO     410101-NYUINRRK-CHK-EXT
           END-IF
           MOVE    ZERO                TO  FLG-OK
           PERFORM
                   UNTIL      (FLG-PTNYUINRRK      =   1 )
      ******************* OR  (FLG-OK              =   1 )
      *        転入日
               IF      PTNYUINRRK-TENNYUYMD(1:6)   <=  SPA-NAI-SRYYM
      *            転出日
                   IF     ( PTNYUINRRK-TENSTUYMD   =   SPACE )  OR
                          ((PTNYUINRRK-TENSTUYMD   NOT =   SPACE ) AND
                           (PTNYUINRRK-TENSTUYMD(1:6)  >=
                                                       SPA-NAI-SRYYM))
      *                入院日
                       MOVE    PTNYUINRRK-NYUINYMD TO  SPA-NAI-NYUINYMD
                       MOVE    PTNYUINRRK-NYUINYMD TO  WRK-SYMD
                       PERFORM 41012-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG         TO  SPA-GMN-NYUINYMD
      *                退院日
                       MOVE    PTNYUINRRK-TAIINYMD TO  SPA-NAI-TAIINYMD
                       MOVE    PTNYUINRRK-TAIINYMD TO  WRK-SYMD
                       PERFORM 41012-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG         TO  SPA-GMN-TAIINYMD
      *                入院科
                       MOVE    PTNYUINRRK-NYUINKA  TO  SPA-NAI-NYUINKA
                       MOVE    1                   TO  FLG-OK
                   END-IF
               END-IF
      *
               MOVE    "PTNYUINRRK-KEY8"   TO  ORC-DBPATH
               PERFORM 910-PTNYUINRRK-READ-SEC
           END-PERFORM
      *
           IF      FLG-OK              =   1
      *        入院中の場合入院とする
               IF      (SPA-NAI-NYUINYMD   <   SPA-NAI-SRYYM )  AND
                       (SPA-NAI-TAIINYMD   >   SPA-NAI-SRYYM )
                   MOVE    "1"             TO  SPA-NYUGAIKBN
                   GO      TO      410101-NYUINRRK-CHK-EXT
               END-IF
           END-IF
      *
           .
       410101-NYUINRRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科入力処理
      *****************************************************************
       41013-SRYKAI-SYORI-SEC         SECTION.
      *
           MOVE    6                   TO  SPA-GMN-CUR
      *
      *    診療科変更なし
      *    IF     (J02-SRYKA       NOT =   SPACE )  AND
      *           (J02-SRYKA           =   SPA-GMN-SRYKA-G )
      *
      *        IF      SPA-GMN-MAX         >   ZERO
      *            MOVE    1                   TO  SPA-GMN-CUR
      *        END-IF
      *        GO      TO      41013-SRYKAI-SYORI-EXT
      *    END-IF
      *
           MOVE    SPA-GMN-SRYKA-G     TO  SPA-MAE-SRYKA-G
           MOVE    J02-SRYKA           TO  SPA-GMN-SRYKA-G
      *    診療科選択なし
           IF     (SPA-GMN-SRYKA       =   SPACE ) AND
                  (SPA-GMN-PTID        =   ZERO  )
               GO      TO      41013-SRYKAI-SYORI-EXT
           END-IF
      *    診療科
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SRYKA-MAX
               IF      SPA-GMN-SRYKA   =   SPA-GMN-SRYKAL (IDX)
                   MOVE    SPA-GMN-SRYKA-LIST (IDX)
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
      *    診療科選択
           IF     (SPA-MAE-SRYKA       =   SPACE )  OR
                  (SPA-UPDFLG          =   SPACE )
               PERFORM 41013-SRYKA-CHANGE-SEC
               GO      TO      41013-SRYKAI-SYORI-EXT
           END-IF
      *
      *    確認表示
           MOVE    "0107"              TO  SPA-JIDCD
           .
       41013-SRYKAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科変更処理
      *****************************************************************
       41013-SRYKA-CHANGE-SEC             SECTION.
      *
      *    現在表示されている内容を更新する
      *    診療会計マスター更新
           IF      SPA-GMN-MAX         >   ZERO
               PERFORM 4901-SRYACCT-UPD-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO       41013-SRYKA-CHANGE-EXT
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-SRYKA       TO  SPA-J01-SRYKA
           MOVE    SPA-GMN-SRYKAMEI    TO  SPA-J01-SRYKAMEI
           MOVE    SPA-GMN-SRYYM       TO  SPA-J02-SRYYMDW(1:6)
           MOVE    SPA-NAI-SRYYM       TO  SPA-J02-SRYYMD (1:6)
           MOVE    SPA-GMN-NYUGAIKBN-G TO  WRK-NYUGAIKBN-G
           MOVE    SPA-GMN-SRYKBN-G    TO  WRK-SRYKBN-G
           MOVE    SPA-GMN-HHKNCOMBI-G TO  WRK-HKNCOMBI-G
           INITIALIZE                  SPA-J02-AREA
           MOVE    WRK-NYUGAIKBN-G     TO  SPA-GMN-NYUGAIKBN-G
           MOVE    WRK-SRYKBN-G        TO  SPA-GMN-SRYKBN-G
           MOVE    WRK-HKNCOMBI-G      TO  SPA-GMN-HHKNCOMBI-G
      *
           MOVE    SPA-J02-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM
           MOVE    SPA-J02-SRYYMD (1:6)    TO  SPA-NAI-SRYYM
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-SRYKA-MAX
               IF      SPA-J01-SRYKA       =   SPA-GMN-SRYKAL (IDX)
                   MOVE    SPA-GMN-SRYKA-LIST(IDX)
                                           TO  SPA-GMN-SRYKA-G
               END-IF
           END-PERFORM
      *
      *    受診履歴編集
           PERFORM 3103-JYURRK-HEN-SEC
      *
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
      *    保存用パラメタ作成
           PERFORM 4201-SRYACCT-PARA-SEC
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-STRNUM
      *
      *    診療会計マスターより編集をする
           PERFORM 410181-SRYACCT-HEN-SEC
      *    保険組合一覧編集
      **** PERFORM 3105-HKNCOMBI-HEN-SEC
      *
           IF      SPA-GMN-MAX         =   ZERO
               MOVE    6                   TO  SPA-GMN-CUR
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *    受診履歴が無い時、メッセージ表示
           IF      SPA-RRK-MAX         =   ZERO
               IF      SPA-SRYKA-MAX       =   1
                   MOVE    07              TO  SPA-GMN-CUR
               ELSE
                   MOVE    06              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       41013-SRYKA-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月入力処理
      *****************************************************************
       41018-SRYYMD-SYORI-SEC             SECTION.
      *
           MOVE    7                   TO  SPA-GMN-CUR
      *
           MOVE    J02-SRYYM           TO  SPA-GMN-SRYYM
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN-SRYYM       TO  SGDAY-INDAY
           MOVE    "1"                 TO  SGDAY-INTYPE
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY(1:6)    TO  SPA-GMN-SRYYM
      *        患者の確定してない時
               IF      SPA-GMN-PTID        =   ZERO
                    MOVE    SGDAY-SDAY(1:6)     TO  SPA-NAI-SRYYM
                    MOVE    ZERO                TO  SPA-GMN-CUR
                    GO      TO      41018-SRYYMD-SYORI-EXT
               END-IF
      *        誕生日以前の時、エラー
               IF      SGDAY-SDAY(1:6)     <   SPA-GMN-BIRTHDAY(1:6)
                    MOVE    "0015"              TO  SPA-ERRCD
                    GO      TO      41018-SRYYMD-SYORI-EXT
               END-IF
      *        変更のない時、以降の処理はそのまま
               IF      SGDAY-SDAY(1:6)     =   SPA-NAI-SRYYM
                   IF      SPA-GMN-MAX         >   ZERO
                        MOVE    1                   TO  SPA-GMN-CUR
                        GO      TO      41018-SRYYMD-SYORI-EXT
                   END-IF
               END-IF
               MOVE    SGDAY-SDAY(1:6)     TO  SPA-NAI-SRYYM
           ELSE
               MOVE    "0018"          TO  SPA-ERRCD
               GO      TO      41018-SRYYMD-SYORI-EXT
           END-IF
      *
           IF      SPA-UPDFLG          =   SPACE
               PERFORM 410181-SRYYM-CHANGE-SEC
           ELSE
               MOVE    "0108"              TO  SPA-JIDCD
           END-IF
      *
           .
       41018-SRYYMD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号選択入力処理
      *****************************************************************
       410181-SRYYM-CHANGE-SEC             SECTION.
      *
      *    診療会計マスター更新
           IF      SPA-UPDFLG          =   "1"
               MOVE    SPA-NAI-SRYYM       TO  WRK-NAI-SRYYM
               MOVE    SPA-GMN-SRYYM       TO  WRK-GMN-SRYYM
               MOVE    SPA-J02-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM
               MOVE    SPA-J02-SRYYMD (1:6)    TO  SPA-NAI-SRYYM
               PERFORM 4901-SRYACCT-UPD-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO       410181-SRYYM-CHANGE-EXT
               END-IF
               MOVE    WRK-NAI-SRYYM       TO  SPA-NAI-SRYYM
               MOVE    WRK-GMN-SRYYM       TO  SPA-GMN-SRYYM
           END-IF
      *
           MOVE    ZERO                TO  SPA-RRK-MAX
           INITIALIZE                  SPA-GMN-RRKTBL
           INITIALIZE                  SPA-NAI-RRKTBL
           MOVE    ZERO                TO  SPA-GMN-MAX
           INITIALIZE                  SPA-GMN-TBL
           INITIALIZE                  SPA-NAI-TBL
           INITIALIZE                  SPA-GMN-INPUT
           INITIALIZE                  SPA-GMN-NYUIN-AREA
      *
      *    診療科一覧編集
           PERFORM 3005-SRYKA-HEN-SEC
      *
      *    保険一覧編集
           PERFORM 3008-HKNCOMBI-HEN-SEC
      *
      *    受診履歴より診療科決定
           MOVE    SPACE               TO  SPA-J01-SRYKA
           MOVE    SPACE               TO  SPA-J01-SRYKAMEI
           PERFORM 310310-JYURRK-CHK-SEC
           IF      SPA-J01-SRYKA       =   SPACE
               MOVE    "0020"          TO  SPA-ERRCD
               MOVE    07              TO  SPA-GMN-CUR
               GO      TO       410181-SRYYM-CHANGE-EXT
           END-IF
      *
           PERFORM 3101-SRYKA-HEN-SEC
      *    入院が存在するかのチェック
           IF      SPA-BEDSU           >   ZERO
               PERFORM 410101-NYUINRRK-CHK-SEC
               PERFORM 410102-NYUINRRK-LST-SEC
           END-IF
      *
      *    受診履歴編集
           PERFORM 3103-JYURRK-HEN-SEC
      *
      *    保存用パラメタ作成
           PERFORM 4201-SRYACCT-PARA-SEC
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-STRNUM
      *
      *    診療会計マスターより編集をする
           PERFORM 410181-SRYACCT-HEN-SEC
      *
           IF      SPA-GMN-MAX         =   ZERO
               MOVE    7                   TO  SPA-GMN-CUR
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *    受診履歴が無い時、メッセージ表示
           IF      SPA-RRK-MAX         =   ZERO
               IF      SPA-SRYKA-MAX       =   1
                   MOVE    07              TO  SPA-GMN-CUR
               ELSE
                   MOVE    06              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       410181-SRYYM-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号選択入力処理
      *****************************************************************
       41011-BANGO-CHK-SEC             SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           IF      J02-NUM             =   SPACE
               MOVE    ZERO                TO  J02-NUM
           END-IF
           INITIALIZE                      ORCSNUMAREA
           MOVE    J02-NUM             TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0001"              TO  SPA-ERRCD
               GO      TO      41011-BANGO-CHK-EXT
           ELSE
               MOVE    SNUM-OUTNUM         TO  SPA-GMN-BANGO
           END-IF
      *
      *    IF      SPA-GMN-BANGO       >   SPA-NAI-MAX
      *        MOVE    "0002"              TO  SPA-ERRCD
      *        GO      TO      41011-BANGO-CHK-EXT
      *    END-IF
           MOVE    ZERO                TO  SPA-NAI-BANGO
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
               IF      SPA-GMN-TBLREC(IDX)(1:3)    =   "---"
                   CONTINUE
               ELSE
                   IF      SPA-GMN-BANGO       =   SPA-GMN-TBANGO  (IDX)
                       MOVE    IDX             TO  WRK-BANGO
                       MOVE    SPA-GMN-MAX     TO  IDX
                   END-IF
               END-IF
           END-PERFORM
           IF      WRK-BANGO           =   ZERO
               MOVE    "0002"              TO  SPA-ERRCD
               GO      TO      41011-BANGO-CHK-EXT
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-NAI-MAX
               IF      WRK-BANGO       =   SPA-NAI-TBANGO (IDX)
                   MOVE    IDX             TO  SPA-NAI-BANGO
                   MOVE    SPA-NAI-MAX     TO  IDX
               END-IF
           END-PERFORM
      *
           IF      SPA-GMN-BANGO       =   ZERO
               INITIALIZE                  SPA-GMN-INPUT
               INITIALIZE                  SPA-NAIGMN-AREA
               MOVE    ZERO                TO  SPA-J02-SYORI
      *
               MOVE    1                   TO  SPA-GMN-CUR
           ELSE
               INITIALIZE                  SPA-NAIGMN-AREA
               MOVE    ZERO                TO  SPA-J02-SYORI
      *
      ******** MOVE    SPA-NAI-TBANGO (SPA-GMN-BANGO)
               MOVE    SPA-NAI-TBANGO (SPA-NAI-BANGO)
                                           TO  IDX
               MOVE    SPA-GMN-TMEISYO  (IDX)
                                           TO  SPA-GMN-MEISYO
      *******  MOVE    SPA-NAI-DAY-AREA (SPA-GMN-BANGO)
               MOVE    SPA-NAI-DAY-AREA (SPA-NAI-BANGO)
                                           TO  SPA-GNAI-DAY-G
               MOVE    SPA-GNAI-DAY-TBL (01)
                                           TO  SPA-GMN-DAY-G
      *        保険番号
               MOVE    SPA-NAI-HKNCOMBI (SPA-NAI-BANGO)
                                           TO  SPA-GMN-HKNCOMBI
               MOVE    SPA-NAI-ROUJIN   (SPA-NAI-BANGO)
                                           TO  SPA-GMN-ROUJIN
      *        剤番号
               MOVE    SPA-NAI-ZAINUM  (SPA-NAI-BANGO)
                                           TO  SPA-GMN-ZAINUM
      *        診療科
               MOVE    SPA-NAI-SRYKA   (SPA-NAI-BANGO)
                                           TO  SPA-NAIGMN-SRYKA
               MOVE    SPA-NAI-SRYKAMEI(SPA-NAI-BANGO)
                                           TO  SPA-GMN-BSRYKAMEI
      *        診療区分
               MOVE    SPA-NAI-SRYKBN  (SPA-NAI-BANGO)
                                           TO  SPA-NAIGMN-SRYKBN
      *        保険内容編集
               PERFORM VARYING     IDH     FROM    1   BY  1
                       UNTIL       IDH     >   SPA-HKN-MAX
                   IF      SPA-GMN-THKNCOMBINUM (IDH)
                                               =   SPA-GMN-HKNCOMBI
                       MOVE    SPA-GMN-THKNCOMBIMEI (IDH)
                                               TO  SPA-GMN-HKNCOMBIMEI
                       MOVE    SPA-NAI-THKNKBN (IDH)
                                               TO  SPA-NAI-HKNKBN
                       MOVE    SPA-NAI-TRSI-SISIKBN (IDH)
                                               TO  SPA-NAI-SISIKBN
                       MOVE    SPA-NAI-TRSI-HKNKBN (IDH)
                                               TO  SPA-NAI-RSI-HKNKBN
                       MOVE    SPA-NAI-TRSI-SHOBYOYMD (IDH)
                                               TO  SPA-RSI-SHOBYOYMD
                   END-IF
               END-PERFORM
      *!       MOVE    SPA-GMN-HKNCOMBI    TO  WRK-HKNCOMBI
      *        PERFORM 510-HKNCOMBI-MEI-SEC
      *        MOVE    WRK-HKNCOMBIMEI     TO  SPA-GMN-HKNCOMBIMEI
      *----(01.00.01) LINE ADD START ----------------------------------
      *        老人判定
      *        IF     (COMB-KOH1HKNNUM     =   "027"  )  OR
      *               (COMB-KOH2HKNNUM     =   "027"  )  OR
      *               (COMB-KOH3HKNNUM     =   "027"  )  OR
      *               (COMB-KOH4HKNNUM     =   "027"  )  OR
      *               (SPA-J02-ROUJIN      =   1      )
      *            MOVE    1               TO  SPA-NAI-ROUJIN
      *                                               (SPA-NAI-BANGO)
      *        ELSE
      *            MOVE    ZERO            TO  SPA-NAI-ROUJIN
      *                                              (SPA-NAI-BANGO)
      *        END-IF
      *        労災・自賠責の場合、老人でも一般とする
      *        IF      SPA-NAI-HKNKBN      =   1
      *            MOVE    ZERO            TO  SPA-NAI-ROUJIN
      *                                              (SPA-NAI-BANGO)
      *        END-IF
      *
      *        MOVE    SPA-NAI-ROUJIN   (SPA-NAI-BANGO)
      *!                                   TO  SPA-GMN-ROUJIN
      *----(01.00.01) LINE ADD START ----------------------------------
      *
               MOVE    1                   TO  SPA-IDXZ
      *
               MOVE    2                   TO  SPA-GMN-CUR
               MOVE    ZERO                TO  SPA-DAY-IDX
           END-IF
      *
           .
       41011-BANGO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤回数入力処理
      *****************************************************************
       41012-DAY-CHK-SEC             SECTION.
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *
      *    IF      WRK-MAP-INDEX       >   ZERO  AND  <  32
      *        MOVE    WRK-MAP-INDEX       TO  SPA-IDXZ
      *    END-IF
      *
      *    PERFORM   VARYING   IDX     FROM    1   BY  1
      *              UNTIL     IDX         >   31
      *    誕生日以前はエラー
           IF      SPA-BIRTHDAY(1:6)   =   SPA-NAI-SRYYM
               IF      SPA-DAY-IDX        <   SPA-BIRTHDAY(7:2)
                   MOVE    "0015"              TO  SPA-ERRCD
                   GO      TO      41012-DAY-CHK-EXT
               END-IF
           END-IF
      *
      *    一括入力とする
           MOVE    ZERO                TO  WRK-IDX
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   31
               INITIALIZE                      ORCSNUMAREA
               MOVE    J02-DAY(IDX)     TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   IF      WRK-IDX             =   ZERO
                       MOVE    "0003"              TO  SPA-ERRCD
                       MOVE    IDX                 TO  WRK-IDX
                   END-IF
               ELSE
                   MOVE    SNUM-OUTNUM     TO  SPA-GMN-DAY (IDX)
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    WRK-IDX         TO  SPA-DAY-IDX
               GO      TO     41012-DAY-CHK-EXT
           END-IF
      *
      *    受診歴有無のチェック
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL    (IDX         >   31  )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-DAY (IDX)   >   ZERO
      *            受診歴有無のチェック
                   PERFORM 410121-RRK-DAYCHK-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       MOVE    IDX             TO  WRK-IDX
                       MOVE    WRK-IDX         TO  SPA-DAY-IDX
                   END-IF
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    WRK-IDX         TO  SPA-DAY-IDX
               GO      TO     41012-DAY-CHK-EXT
           END-IF
      *
      *
      *    IF      SPA-GMN-DAY (SPA-DAY-IDX)   =   SPA-GNAI-DAY
      *                                            (1 SPA-DAY-IDX)
      *        CONTINUE
      *****ELSE
      *    今回入力位置
           IF    (MCP-EVENT       =   "ACTIVATE"  ) AND
                 (MCP-WIDGET(1:3) =   "DAY"       )
               MOVE    MCP-WIDGET(4:2)     TO  IDX-IN
           ELSE
               MOVE    ZERO                TO  IDX-IN
           END-IF
      *
           PERFORM   VARYING   IDX-IN2         FROM    1   BY  1
                     UNTIL    (IDX-IN2         >   31  )  OR
                              (SPA-ERRCD   NOT =   SPACE) OR
                              (FLG-END         =   1    )
               MOVE    IDX-IN2             TO  SPA-DAY-IDX
               IF     (SPA-GMN-DAY (SPA-DAY-IDX)
                                       NOT =   SPA-GNAI-DAY
                                                    (1 SPA-DAY-IDX))
                   OR (SPA-DAY-IDX         =   IDX-IN )
      *            今回変更があり、履歴がある時
                   IF     (SPA-GNAI-DAY (3 SPA-DAY-IDX)  >   ZERO  )  OR
                          (SPA-GNAI-DAY (4 SPA-DAY-IDX)  >   ZERO  )
                        PERFORM 410121-J022-SET-SEC
                    END-IF
                END-IF
                IF     FLG-END             =   ZERO
                    MOVE    SPA-GMN-DAY (SPA-DAY-IDX)
                                               TO  SPA-GNAI-DAY
                                                        (1 SPA-DAY-IDX)
                   IF     (SPA-GNAI-DAY (3 SPA-DAY-IDX)  >   ZERO  )  OR
                          (SPA-GNAI-DAY (4 SPA-DAY-IDX)  >   ZERO  )
                       INITIALIZE                  SPA-J022-AREA
                       MOVE    1               TO  SPA-NAI-DAYCHK
                                                         (SPA-DAY-IDX)
                       MOVE    SPA-GNAI-DAY (2 SPA-DAY-IDX)
                                               TO  SPA-NAI-DAY1
                                                        (SPA-DAY-IDX)
                       MOVE    SPA-GNAI-DAY (3 SPA-DAY-IDX)
                                               TO  SPA-NAI-DAY2
                                                        (SPA-DAY-IDX)
                       MOVE    SPA-GNAI-DAY (4 SPA-DAY-IDX)
                                               TO  SPA-NAI-DAY3
                                                        (SPA-DAY-IDX)
      *************PERFORM 410121-J022-SET-SEC
      *************GO      TO      41012-DAY-CHK-EXT
                   ELSE
                       MOVE    SPA-GMN-DAY (SPA-DAY-IDX)
                                           TO  SPA-GNAI-DAY
                                                        (1 SPA-DAY-IDX)
                       MOVE    SPA-GMN-DAY (SPA-DAY-IDX)
                                           TO  SPA-GNAI-DAY
                                                        (2 SPA-DAY-IDX)
      *                受診履歴の剤変更
                       PERFORM 410122-RRK-ZAINUM-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      FLG-END             =   1
               GO      TO      41012-DAY-CHK-EXT
           END-IF
      *
      *    ＯＫの時、次ぎの日へ
           IF    (MCP-EVENT       =   "ACTIVATE"  ) AND
                 (MCP-WIDGET(1:3) =   "DAY"       )
               MOVE    MCP-WIDGET(4:2)     TO  SPA-DAY-IDX
           END-IF
      *****IF      SPA-DAY-IDX             >=  SPA-SRYMATU
      *        MOVE    3                   TO  SPA-GMN-CUR
      *    ELSE
      *        ADD     1                   TO  SPA-IDXZ
      *****END-IF
           IF      SPA-ERRCD           =   SPACE
               ADD     1               TO  SPA-DAY-IDX
               IF      SPA-DAY-IDX         >   SPA-SRYMATU
                   MOVE    5                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       41012-DAY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診歴有無のチェック処理
      *****************************************************************
       410121-RRK-DAYCHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDR     FROM    1   BY   1
                   UNTIL      (IDR     >   SPA-RRK-MAX  )  OR
                              (FLG-OK      =   1        )
               IF     (SPA-NAIGMN-SRYKA    =   SPA-RRK-SRYKA (IDR)) AND
                      (SPA-GMN-HKNCOMBI    =   SPA-RRK-HKNCOMBI(IDR))
                   MOVE    SPA-RRK-RRKYMD(IDR)(7:2)    TO  IDR2
                   IF      IDR2                =   IDX
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
      *        外来の時エラー
               IF      SPA-GMN-NYUGAIKBN   =   "2"
                   MOVE    "0004"              TO  SPA-ERRCD
               ELSE
      *            入院の時、入院期間中ならＯＫ
                   MOVE    IDX                 TO  IDX-D2
                   IF     (SPA-NAI-TAIINYMD(1:6)   =  SPA-NAI-SRYYM) AND
                          (SPA-NAI-TAIINYMD(7:2)   <  IDX-D2       )
                       MOVE    "0004"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       410121-RRK-DAYCHK-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴の剤変更処理
      *****************************************************************
       410122-RRK-ZAINUM-SEC           SECTION.
      *
      *    対象履歴判定
           MOVE    ZERO                TO  IDX-RRK
           MOVE    ZERO                TO  WRK-RRK-RENNUM
           MOVE    ZERO                TO  WRK-RRK-DOUJI-RENNUM
           MOVE    ZERO                TO  WRK-RRK-HKNKBN
           PERFORM VARYING     IDR     FROM    1   BY   1
                   UNTIL       IDR     >   SPA-RRK-MAX
               MOVE    SPA-RRK-RRKYMD(IDR)(7:2)    TO  IDR2
               IF     (IDR2                =   SPA-DAY-IDX    ) AND
                      (SPA-RRK-SRYKA (IDR) =   SPA-NAIGMN-SRYKA)
                   MOVE    SPA-RRK-RENNUM(IDR) TO  WRK-RRK-RENNUM
      *            入院の場合、最大同時連番を取得する
                   IF     (SPA-NYUGAIKBN       =   "1" )  AND
                          (SPA-RRK-DOUJI-RENNUM(IDR)
                                               >   WRK-RRK-DOUJI-RENNUM)
                       MOVE    SPA-RRK-DOUJI-RENNUM(IDR)
                                              TO  WRK-RRK-DOUJI-RENNUM
                   END-IF
               END-IF
               IF     (SPA-RRK-HKNCOMBI(IDR)   =   SPA-GMN-HKNCOMBI)
                   MOVE    SPA-RRK-HKNKBN(IDR) TO  WRK-RRK-HKNKBN
               END-IF
      *
               IF     (IDR2                =   SPA-DAY-IDX    ) AND
                      (SPA-RRK-SRYKA (IDR) =   SPA-NAIGMN-SRYKA) AND
                      (SPA-RRK-HKNCOMBI(IDR)   =  SPA-GMN-HKNCOMBI)
                   MOVE    IDR             TO  IDX-RRK
                   MOVE    SPA-RRK-RENNUM (IDR)    TO  IDX2
                   ADD     1                   TO  IDX2
                   MOVE    SPA-GNAI-DAY (IDX2 SPA-DAY-IDX)
                                               TO  WRK-DAY-S
      *            対象剤判定
                   MOVE    ZERO                TO  WRK-IDR
                   MOVE    ZERO                TO  WRK-IDR-MAX
                   PERFORM VARYING     IDR3    FROM    1   BY   1
      *********************UNTIL       IDR3    >   60
                           UNTIL       IDR3    >   135
                       IF      SPA-RRK-ZAINUM (IDX-RRK IDR3)
                                                  =   SPA-GMN-ZAINUM
                           MOVE    IDR3           TO  WRK-IDR
                       END-IF
                       IF      SPA-RRK-ZAINUM (IDX-RRK IDR3)
                                              NOT =  ZERO
                           MOVE    IDR3           TO  WRK-IDR-MAX
                       END-IF
                   END-PERFORM
      *
                   IF      WRK-IDR             =   ZERO
                       IF      WRK-DAY-S           >   ZERO
      *                    剤追加
                           ADD     1               TO  WRK-IDR-MAX
                           IF      WRK-IDR-MAX     <=  135
                               MOVE    SPA-GMN-ZAINUM
                                                   TO  SPA-RRK-ZAINUM
                                                 (IDX-RRK WRK-IDR-MAX)
                           END-IF
                       END-IF
                   ELSE
                       IF      WRK-DAY-S           =   ZERO
      *                    剤削除
                           MOVE    ZERO            TO  SPA-RRK-ZAINUM
      ***************************************    (IDX-RRK WRK-IDR-MAX)
                                                 (IDX-RRK WRK-IDR)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *    対象がなかった時追加
           IF      IDX-RRK             =   ZERO
               IF      SPA-GNAI-DAY (1 SPA-DAY-IDX)    >   ZERO
                   PERFORM 41001221-RRK-ADD-SEC
               END-IF
           END-IF
      *
           .
       410122-RRK-ZAINUM-EXT.
           EXIT.
      *            
      *****************************************************************
      *    受診履歴テーブル追加処理
      *****************************************************************
       41001221-RRK-ADD-SEC             SECTION.
      *
           IF      SPA-RRK-MAX         >=  100
               MOVE    "0024"          TO  SPA-ERRCD
               GO      TO      41001221-RRK-ADD-EXT
           END-IF
      *
           ADD     1                   TO  SPA-RRK-MAX
           MOVE    SPA-RRK-MAX         TO  IDR
           MOVE    SPA-NAI-SRYYM       TO  SPA-RRK-RRKYMD(IDR)(1:6)
           MOVE    SPA-DAY-IDX         TO  SPA-RRK-RRKYMD(IDR)(7:2)
           MOVE    SPA-NAIGMN-SRYKA    TO  SPA-RRK-SRYKA (IDR)
           MOVE    SPA-GMN-HKNCOMBI    TO  SPA-RRK-HKNCOMBI (IDR)
           MOVE    WRK-RRK-HKNKBN      TO  SPA-RRK-HKNKBN   (IDR)
           ADD     1                   TO  WRK-RRK-RENNUM
           MOVE    WRK-RRK-RENNUM      TO  SPA-RRK-RENNUM   (IDR)
           MOVE    ZERO                TO  SPA-RRK-DOUJI-RENNUM(IDR)
      *
      *    入院の場合、同時連番を連番とする
           IF     (SPA-NYUGAIKBN       =   "1"  )  AND
                  (WRK-RRK-RENNUM      >   1    )
               MOVE    1                   TO  WRK-RRK-RENNUM
               ADD     1                   TO  WRK-RRK-DOUJI-RENNUM
               MOVE    WRK-RRK-RENNUM      TO  SPA-RRK-RENNUM   (IDR)
               MOVE    WRK-RRK-DOUJI-RENNUM
                                           TO  SPA-RRK-DOUJI-RENNUM(IDR)
           END-IF
      *
           MOVE    1                   TO  SPA-RRK-KAIKEI-RENNUM(IDR)
           MOVE    ZERO                TO  SPA-RRK-DENPNUM (IDR)
           MOVE    SPA-GMN-ZAINUM      TO  SPA-RRK-ZAINUM  (IDR 1)
           MOVE    SPA-NAIGMN-SRYKBN   TO  SPA-RRK-SRYKBN  (IDR)
           MOVE    "1"                 TO  SPA-RRK-ADD     (IDR)
      *    画面表示用
           MOVE    IDR                 TO  SPA-GMN-RRKNO (IDR)
           MOVE    SPA-RRK-RRKYMD(IDR) TO  WRK-SYMD
           PERFORM 41012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-RRKYMD (IDR)
           MOVE    SPA-GMN-BSRYKAMEI   TO  SPA-GMN-RRKSRYKA (IDR)
      *
           .
       41001221-RRK-ADD-EXT.
           EXIT.
      *            
      *****************************************************************
      *    回数分割入力画面へ（Ｊ０２２）処理
      *****************************************************************
       410121-J022-SET-SEC             SECTION.
      *
           MOVE    SPA-GMN-DAY (SPA-DAY-IDX)
                                       TO  SPA-GNAI-DAY
                                                        (1 SPA-DAY-IDX)
           INITIALIZE                  SPA-J022-AREA
           MOVE    1                   TO  SPA-NAI-DAYCHK
                                                        (SPA-DAY-IDX)
           MOVE    SPA-GNAI-DAY (2 SPA-DAY-IDX)
                                       TO  SPA-NAI-DAY1 (SPA-DAY-IDX)
           MOVE    SPA-GNAI-DAY (3 SPA-DAY-IDX)
                                       TO  SPA-NAI-DAY2 (SPA-DAY-IDX)
           MOVE    SPA-GNAI-DAY (4 SPA-DAY-IDX)
                                       TO  SPA-NAI-DAY3 (SPA-DAY-IDX)
      *
           MOVE    SPA-DAY-IDX         TO  SPA-J022-IDX
      **** 開始位置
      *    PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL       IDX     >   31
      *        IF      SPA-NAI-DAYCHK (IDX)    =   1
      *            MOVE    IDX             TO  SPA-J022-IDX
      *            MOVE    31              TO  IDX
      *        END-IF
      *****END-PERFORM
      *
           MOVE    SPA-GNAI-DAY (2 SPA-J022-IDX) 
                                       TO  SPA-J022-DAY1
           MOVE    SPA-GNAI-DAY (3 SPA-J022-IDX) 
                                       TO  SPA-J022-DAY2
           MOVE    SPA-GNAI-DAY (4 SPA-J022-IDX) 
                                       TO  SPA-J022-DAY3
      *
      *    日付変換
           MOVE    SPA-NAI-SRYYM       TO  WRK-SYMD(1:6)
           MOVE    SPA-J022-IDX        TO  WRK-SDD
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING  STS-AREA-DAY
                                              LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE  LNK-DAY2-EDTYMD3      TO  SPA-HENNYMD
           END-IF
      *
           MOVE    SPACE               TO  J022
      *
      *    画面編集
           MOVE    SPA-HENNYMD(11:)    TO  J022-SRYYMD
           MOVE    SPA-J022-DAY1       TO  WRK-ZZZ
           MOVE    WRK-ZZZ-X           TO  J022-DAY1
           MOVE    SPA-J022-DAY2       TO  WRK-ZZZ
           MOVE    WRK-ZZZ-X           TO  J022-DAY2
           MOVE    SPA-J022-DAY3       TO  WRK-ZZZ
           MOVE    WRK-ZZZ-X           TO  J022-DAY3
      *
           MOVE    1                   TO  SPA-J022-CUR
      *
      *    カーソル移動
           MOVE    "DAY1"              TO  MCP-WIDGET
      *
      *
           MOVE    "J02"               TO  SPA-MOTOPG
           MOVE    "J022"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "J022"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       410121-J022-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細選択処理
      *****************************************************************
       41012-DATALIST-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
           MOVE    ZERO                TO  WRK-SELNUM2
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
              IF      J02-DATALIST-SELECT (IDX)   =  "T"
      ************IF      SPA-GMN-TBANGO(IDX)     =   SPA-GMN-BANGO
      *               MOVE    SPA-GMN-TBANGO (IDX)    TO  WRK-SELNUM
      *           ELSE
      *************** MOVE    SPA-GMN-TBANGO (IDX)    TO  WRK-SELNUM2
                  IF      SPA-NAI-TBANGO2(IDX)    =   SPA-GMN-BANGO
                      MOVE    SPA-NAI-TBANGO2 (IDX)   TO  WRK-SELNUM
                  ELSE
                      MOVE    SPA-NAI-TBANGO2 (IDX)   TO  WRK-SELNUM2
                  END-IF
              END-IF
           END-PERFORM
      *
           IF     (WRK-SELNUM          =   ZERO )  AND
                  (WRK-SELNUM2     NOT =   ZERO )
               MOVE    WRK-SELNUM2         TO  SPA-GMN-BANGO
           ELSE
               IF      WRK-SELNUM2         =   ZERO
                   CONTINUE
               ELSE
                   MOVE    SPA-GMN-BANGO       TO  SPA-MAE-BANGO
                   MOVE    SPA-NAI-BANGO       TO  SPA-MAEN-BANGO
                   MOVE    WRK-SELNUM2         TO  SPA-GMN-BANGO
                   MOVE    "0103"              TO  SPA-JIDCD
                   GO      TO      41012-DATALIST-EXT
               END-IF
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-CUR
           MOVE    SPA-GMN-BANGO       TO  J02-NUM
           PERFORM 41011-BANGO-CHK-SEC
      *
           .
       41012-DATALIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴選択処理
      *****************************************************************
       41014-RRKSELNUMI-CHK-SEC             SECTION.
      *
      *    剤修正のチェック
      **** IF      SPA-GMN-BANGO   NOT =   ZERO
      *        MOVE    "0009"          TO  SPA-ERRCD
      *        GO      TO      41014-RRKSELNUMI-CHK-EXT
      **** END-IF
      *
           MOVE    3                   TO  SPA-GMN-CUR
      *
           MOVE    J02-RRKSELNUM       TO  SPA-GMN-RRKSELNUM
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    J02-RRKSELNUM       TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    ZERO                TO  SPA-NAI-RRKSELNUM
               GO      TO      41014-RRKSELNUMI-CHK-EXT
           ELSE
               MOVE    SNUM-OUTNUM         TO  SPA-NAI-RRKSELNUM
               MOVE    SPA-NAI-RRKSELNUM   TO  WRK-ZZZZ
               MOVE    WRK-ZZZZ-X          TO  SPA-GMN-RRKSELNUM
           END-IF
      *
           IF      SPA-NAI-RRKSELNUM   >   SPA-RRK-MAX
               MOVE    "1005"          TO  SPA-ERRCD
           ELSE
               IF      SPA-NAI-RRKSELNUM   =   ZERO
                   MOVE    "1005"          TO  SPA-ERRCD
               ELSE
      *            受診日変更とする
                   MOVE    2               TO  SPA-J02-SYORI
                   MOVE    4               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       41014-RRKSELNUMI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細選択処理
      *****************************************************************
       41015-RRKLIST-SEC             SECTION.
      *
      *    剤修正のチェック
      *    IF      SPA-GMN-BANGO   NOT =   ZERO
      *        MOVE    "0009"          TO  SPA-ERRCD
      *        GO      TO      41015-RRKLIST-EXT
      *    END-IF
      *
           MOVE    ZERO                TO  WRK-SELNUM
           MOVE    ZERO                TO  WRK-SELNUM2
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-RRK-MAX
              IF      J02-RRKLIST-SELECT (IDX)    =  "T"
                  IF      SPA-GMN-RRKNO (IDX)     =   SPA-NAI-RRKSELNUM
                      MOVE    SPA-GMN-RRKNO (IDX)     TO  WRK-SELNUM
                  ELSE
                      MOVE    SPA-GMN-RRKNO (IDX)     TO  WRK-SELNUM2
                  END-IF
              END-IF
           END-PERFORM
      *
           IF     (WRK-SELNUM          =   ZERO )  AND
                  (WRK-SELNUM2     NOT =   ZERO )
               MOVE    WRK-SELNUM2         TO  SPA-NAI-RRKSELNUM
           ELSE
               IF      WRK-SELNUM2         =   ZERO
                   CONTINUE
               ELSE
                   MOVE    WRK-SELNUM2         TO  SPA-NAI-RRKSELNUM
               END-IF
           END-IF
      *
      *    IF     (WRK-SELNUM          =   ZERO )  AND
      *           (WRK-SELNUM2     NOT =   ZERO )
      *        MOVE    WRK-SELNUM2         TO  SPA-NAI-RRKSELNUM
      *    ELSE
      *        IF     (WRK-SELNUM      NOT =   ZERO )  AND
      *               (WRK-SELNUM2         =   ZERO )
      *            MOVE    WRK-SELNUM          TO  SPA-NAI-RRKSELNUM
      *        ELSE
      *            MOVE    WRK-SELNUM2         TO  SPA-NAI-RRKSELNUM
      *        END-IF
      *    END-IF
      *
      *
           IF      SPA-NAI-RRKSELNUM    =   ZERO
               MOVE    SPACE               TO  SPA-GMN-RRKSELNUM
           ELSE
               MOVE    SPA-NAI-RRKSELNUM   TO  J02-RRKSELNUM
               PERFORM 41014-RRKSELNUMI-CHK-SEC
           END-IF
      *
           .
       41015-RRKLIST-EXT.
           EXIT.
      *
      *****************************************************************
      *   変更受診日処理
      *****************************************************************
       41016-HENSRYYMD-CHK-SEC             SECTION.
      *
      *    剤修正のチェック
           IF      SPA-GMN-BANGO   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      41016-HENSRYYMD-CHK-EXT
           END-IF
      *
           IF     (SPA-J02-SYORI       =   2  )  AND
                  (SPA-NAI-RRKSELNUM   >   ZERO )
               CONTINUE
           ELSE
               MOVE    "0012"          TO  SPA-ERRCD
               GO      TO      41016-HENSRYYMD-CHK-EXT
           END-IF
      *
      *
           MOVE    4                   TO  SPA-GMN-CUR
      *
           MOVE    J02-HENSRYYMD       TO  SPA-GMN-HENSRYYMD
      *
      *    数字２桁なら当月分とする
           IF     (J02-HENSRYYMD(1:2)  NOT =   SPACE  )  AND
                  (J02-HENSRYYMD(3:)       =   SPACE  )
               INITIALIZE                      ORCSNUMAREA
               MOVE    J02-HENSRYYMD(1:2)  TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "0007"              TO  SPA-ERRCD
                   GO      TO     41016-HENSRYYMD-CHK-EXT
               ELSE
                   MOVE    SNUM-OUTNUM          TO  WRK-DD
               END-IF
               MOVE    SPACE               TO  SPA-GMN-HENSRYYMD
               MOVE    SPA-GMN-SRYYM       TO  SPA-GMN-HENSRYYMD(1:6)
               MOVE    "."                 TO  SPA-GMN-HENSRYYMD(7:1)
               MOVE    WRK-DD              TO  SPA-GMN-HENSRYYMD(8:2)
           END-IF
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN-HENSRYYMD   TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-GMN-HENSRYYMD
               IF      SPA-BIRTHDAY        >   SGDAY-SDAY
                   MOVE    "0015"              TO  SPA-ERRCD
                   GO      TO     41016-HENSRYYMD-CHK-EXT
               END-IF
               MOVE    SGDAY-SDAY          TO  SPA-NAI-HENSRYYMD
           ELSE
               MOVE    "0007"          TO  SPA-ERRCD
               GO      TO     41016-HENSRYYMD-CHK-EXT
           END-IF
      *
           IF      SPA-NAI-HENSRYYMD(1:6)  =   SPA-NAI-SRYYM
               CONTINUE
      *
      ******** PERFORM 410161-HENSRYYMD-SYORI-SEC
      *
      *        クリア
      *        MOVE    ZERO                TO  SPA-NAI-RRKSELNUM
      *        MOVE    SPACE               TO  SPA-NAI-HENSRYYMD
      *        MOVE    SPACE               TO  SPA-GMN-RRKSELNUM
      *        MOVE    SPACE               TO  SPA-GMN-HENSRYYMD
      *        MOVE    1                   TO  SPA-GMN-CUR
      *        処理あり
      ******** MOVE    "1"                 TO  SPA-UPDFLG
           ELSE
               MOVE    "0008"          TO  SPA-ERRCD
               GO      TO      41016-HENSRYYMD-CHK-EXT
           END-IF
      *
      *    既に算定のある日には変更できないものとする。
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-RRK-MAX
               IF     (SPA-RRK-RRKYMD (IDX)   =   SPA-NAI-HENSRYYMD) AND
                      (SPA-RRK-SRYKA  (SPA-NAI-RRKSELNUM)
                                               =   SPA-RRK-SRYKA (IDX))
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
      *    既に算定のある日には変更できないものとする。
           IF      FLG-OK             =   1
               MOVE    "0011"         TO  SPA-ERRCD
               GO      TO      41016-HENSRYYMD-CHK-EXT
           END-IF
      *    更新の確認メッセージ
           MOVE    "0115"              TO  SPA-JIDCD
      *
           .
       41016-HENSRYYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    受診日変更処理
      *****************************************************************
       410161-HENSRYYMD-UPDSYORI-SEC      SECTION.
      *
      *    現在表示されている内容を更新する
      *    診療会計マスター更新
           PERFORM 4901-SRYACCT-UPD-SEC
           IF      SPA-ERRCD      NOT  =   SPACE
               GO      TO       410161-HENSRYYMD-UPDSYORI-EXT
           END-IF
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    受診履歴変更
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPID          TO  JYURRK-HOSPID
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-GMN-NYUGAIKBN   TO  JYURRK-NYUGAIKBN
           MOVE    SPA-RRK-SRYKA  (SPA-NAI-RRKSELNUM)
                                       TO  JYURRK-SRYKA
           MOVE    SPA-RRK-RRKYMD (SPA-NAI-RRKSELNUM)
                                       TO  JYURRK-SRYYMD
           MOVE    SPA-RRK-RENNUM (SPA-NAI-RRKSELNUM)
                                       TO  JYURRK-RENNUM
      *    入院の場合、保険毎に変更する
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    SPA-RRK-HKNCOMBI(SPA-NAI-RRKSELNUM)
                                           TO  JYURRK-HKNCOMBINUM
               MOVE    "JYURRK-KEY19"      TO  WRK-ORC-DBPATH
           ELSE
               MOVE    "JYURRK-KEY5"       TO  WRK-ORC-DBPATH
           END-IF
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      ***  MOVE    "JYURRK-KEY5"       TO  ORC-DBPATH
           MOVE    WRK-ORC-DBPATH      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
      *******  MOVE    "JYURRK-KEY5"       TO  ORC-DBPATH
               MOVE    WRK-ORC-DBPATH      TO  ORC-DBPATH
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
      *    変更前日
           MOVE    SPA-RRK-RRKYMD (SPA-NAI-RRKSELNUM)(7:2)
                                       TO  IDX-D1
      *    変更後日
           MOVE    SPA-NAI-HENSRYYMD(7:2)
                                       TO  IDX-D2
           PERFORM
               UNTIL    FLG-JYURRK         =   1
      *        対象の診療会計を更新する
      *        診療会計更新
               MOVE    1               TO  FLG-SYORI
               PERFORM VARYING     IDX-Z   FROM    1  BY  1
                       UNTIL       IDX-Z   >   15
                   IF      JYURRK-ZAINUM (IDX-Z)  NOT =   ZERO
                       PERFORM 4101612-ACCT-UPDSYORI-SEC
                   END-IF
               END-PERFORM
      *        受診履歴変更
               MOVE    JYURRK-KEY          TO  JYURRK-UPKEY
      *
               MOVE    SPA-NAI-HENSRYYMD   TO  JYURRK-SRYYMD
               MOVE    1                   TO  JYURRK-RENNUM
      *
               MOVE    SMCNDATE-YMD        TO  JYURRK-UPYMD
               MOVE    SMCNDATE-HMS        TO  JYURRK-UPHMS
      *
               MOVE    JYURRK-REC          TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "JYURRK-UPD1"       TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "J02 JYURRK UPD ERR:" MCP-RC
                           ",KEY:" JYURRK-KEY
               END-IF
      *
               MOVE    WRK-ORC-DBPATH      TO  ORC-DBPATH
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
      *
      *    外来の時、収納の更新
           IF      SPA-NYUGAIKBN       =   "2"
               PERFORM 4101613-SYUNOU-UPD-SEC
           END-IF
      *
      *    診療科履歴変更
           INITIALIZE                  ORCSKARRKAREA
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           MOVE    SPA-NAI-SRYYM       TO  SKARRK-SRYYM
           MOVE    SPA-SRYKA           TO  WRK-SRYKA
           MOVE    SPA-RRK-SRYKA  (SPA-NAI-RRKSELNUM)
                                       TO  SPA-SRYKA
           CALL    "ORCSKARRK"         USING
                                       ORCSKARRKAREA
                                       SPA-AREA
           MOVE    WRK-SRYKA           TO  SPA-SRYKA
      *
      *
      *    受診連番の変更日再編集
           MOVE    ZERO                TO  FLG-JYURRK-UPD
           PERFORM 4101613-JYURRK-SEISET-SEC
      *
           MOVE    ZERO                TO  SPA-NAI-RRKSELNUM
           MOVE    SPACE               TO  SPA-NAI-HENSRYYMD
           MOVE    SPACE               TO  SPA-GMN-RRKSELNUM
           MOVE    SPACE               TO  SPA-GMN-HENSRYYMD
      *
      *    受診履歴編集
           PERFORM 3103-JYURRK-HEN-SEC
      *
      *    保存用パラメタ作成
           PERFORM 4201-SRYACCT-PARA-SEC
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-STRNUM
      *
      *    診療会計マスターより編集をする
           PERFORM 410181-SRYACCT-HEN-SEC
      *    受診履歴の連番を変更した時、メッセージを表示
           IF     (FLG-JYURRK-UPD      =   1   )  AND
                  (SPA-NYUGAIKBN       =   "2" )
               MOVE    "K001"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-RRK-MAX         =   ZERO
               IF      SPA-SRYKA-MAX       =   1
                   MOVE    07              TO  SPA-GMN-CUR
               ELSE
                   MOVE    06              TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
      *
           .
       410161-HENSRYYMD-UPDSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診日変更診療会計変更処理
      *****************************************************************
       4101612-ACCT-UPDSYORI-SEC      SECTION.
      *
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPID          TO  ACCT-HOSPID
           MOVE    JYURRK-NYUGAIKBN    TO  ACCT-NYUGAIKBN
           MOVE    JYURRK-PTID         TO  ACCT-PTID
           MOVE    JYURRK-SRYKA        TO  ACCT-SRYKA
           MOVE    JYURRK-SRYYMD(1:6)  TO  ACCT-SRYYM
           MOVE    JYURRK-ZAINUM(IDX-Z)
                                       TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SRYACCT-KEY5"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SRYACCT-KEY5"      TO  ORC-DBPATH
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
           PERFORM
               UNTIL   FLG-SRYACCT          =   1
      *
               IF      JYURRK-HKNCOMBINUM   =   ACCT-HKNCOMBI
                   MOVE    1                TO  IDX
                   MOVE    1                TO  IDY
                   MOVE    ACCT-DAY-AREA    TO  SPA-NAI-DAY-AREA (IDX)
      *
                   EVALUATE    FLG-SYORI
                       WHEN    1
      *                    受診日の変更
                           PERFORM 41016111-YMDHENKOU-SEC
                        WHEN    2
      *                     連番の変更
                           PERFORM 41016111-RENHENKOU-SEC
                   END-EVALUATE
      *            受診日・回数変更時、算定履歴を変更する
                   IF     (SPA-NAI-DAY-AREA (IDX)  NOT =  ACCT-DAY-AREA)
                   AND    (FLG-SYORI           =   1      )
                       PERFORM 4902-SANTEI-KOUSIN-SEC
                   END-IF
      *
                   MOVE    SPA-NAI-DAY-AREA (IDX)  TO  ACCT-DAY-AREA
      *
                   MOVE    SMCNDATE-YMD        TO  ACCT-UPYMD
                   MOVE    SMCNDATE-HMS        TO  ACCT-UPHMS
      *
                   MOVE    SRYACCT-REC         TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "SRYACCT-KEY"       TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"         USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "J02 SRYACCT UPD ERR:" MCP-RC
                               ",KEY:" ACCT-KEY
                   END-IF
               END-IF
      *
               MOVE    "SRYACCT-KEY5"      TO  ORC-DBPATH
               PERFORM 900-SRYACCT-READ-SEC
           END-PERFORM
      *
           .
       4101612-ACCT-UPDSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納の更新編集処理
      *****************************************************************
       4101613-SYUNOU-UPD-SEC      SECTION.
      *
           IF      SPA-RRK-HKNCOMBI(SPA-NAI-RRKSELNUM)
                                       =   "9999"
               GO      TO      4101613-SYUNOU-UPD-EXT
           END-IF
      *    伝票番号ゼロをエラーとする
           IF      SPA-RRK-DENPNUM(SPA-NAI-RRKSELNUM)
                                       =   ZERO
               MOVE    "1006"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      4101613-SYUNOU-UPD-EXT
           END-IF
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPID          TO  SYU-HOSPID
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-RRK-DENPNUM(SPA-NAI-RRKSELNUM)
                                       TO  SYU-DENPNUM
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
               GO      TO      4101613-SYUNOU-UPD-EXT
           END-IF
      *
      *    診療日が変更された時、収納も変更する
           IF      SYU-SRYYMD      NOT =   SPA-NAI-HENSRYYMD
               MOVE    SPA-NAI-HENSRYYMD   TO  SYU-SRYYMD
      *
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
       4101613-SYUNOU-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診連番の変更日再編集処理
      *****************************************************************
       4101613-JYURRK-SEISET-SEC      SECTION.
      *
      *    受診履歴変更
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPID          TO  JYURRK-HOSPID
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-RRK-SRYKA  (SPA-NAI-RRKSELNUM)
                                       TO  JYURRK-SRYKA
           MOVE    SPA-RRK-RRKYMD (SPA-NAI-RRKSELNUM)
                                       TO  JYURRK-SRYYMD
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "JYURRK-KEY4"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "JYURRK-KEY4"       TO  ORC-DBPATH
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *    変更前日
           MOVE    SPA-RRK-RRKYMD (SPA-NAI-RRKSELNUM)(7:2)
                                       TO  IDX-D1
      *
           MOVE    ZERO                TO  WRK-RENNUM
           MOVE    ZERO                TO  WRK-MAE-RENNUM
           PERFORM
               UNTIL    FLG-JYURRK         =   1
      *        受診履歴変更
               MOVE    JYURRK-KEY          TO  JYURRK-UPKEY
      *
               IF      WRK-MAE-RENNUM      =   JYURRK-RENNUM
                   MOVE    WRK-RENNUM          TO  JYURRK-RENNUM
               ELSE
                   MOVE    JYURRK-RENNUM       TO  WRK-MAE-RENNUM
                   IF      JYURRK-RENNUM       NOT =   WRK-RENNUM
                       ADD     1               TO  WRK-RENNUM
                   END-IF
                   MOVE    WRK-RENNUM          TO  JYURRK-RENNUM
               END-IF
               IF      JYURRK-KEY      NOT =   JYURRK-UPKEY
      *            対象の診療会計を更新する
      *            連番の変更
                   MOVE    2               TO  FLG-SYORI
                   PERFORM VARYING     IDX-Z   FROM    1  BY  1
                           UNTIL       IDX-Z   >   15
                       IF      JYURRK-ZAINUM (IDX-Z)  NOT =   ZERO
                           PERFORM 4101612-ACCT-UPDSYORI-SEC
                       END-IF
                   END-PERFORM
      *
                   MOVE    1                   TO  FLG-JYURRK-UPD
      *
                   MOVE    SMCNDATE-YMD        TO  JYURRK-UPYMD
                   MOVE    SMCNDATE-HMS        TO  JYURRK-UPHMS
      *
                   MOVE    JYURRK-REC          TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "JYURRK-UPD1"       TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"         USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
                   IF      MCP-RC          NOT =   ZERO
                        DISPLAY "J02 JYURRK UPD ERR:" MCP-RC
                               ",KEY:" JYURRK-KEY
                   END-IF
               END-IF
      *
               MOVE    "JYURRK-KEY4"       TO  ORC-DBPATH
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
      *
      *
           .
       4101613-JYURRK-SEISET-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診日の変更処理
      *****************************************************************
       41016111-YMDHENKOU-SEC             SECTION.
      *
      *    日の変更（変更後は連番を１とする）
           MOVE    JYURRK-RENNUM       TO  IDX-D3
           COMPUTE IDX-D3              =   IDX-D3    +   1
      *
           MOVE    SPA-NAI-DAY (IDY IDX-D3 IDX-D1)
                                       TO  WRK-DAY-S
           MOVE    WRK-DAY-S           TO  SPA-NAI-DAY (IDY 2 IDX-D2)
           MOVE    WRK-DAY-S           TO  SPA-NAI-DAY (IDY 1 IDX-D2)
           COMPUTE SPA-NAI-DAY (IDY 1   IDX-D1)
                                       =   SPA-NAI-DAY
                                                  (IDY  1    IDX-D1)
                                       -   WRK-DAY-S
           MOVE    ZERO                TO  SPA-NAI-DAY
                                                   (IDY IDX-D3 IDX-D1)
           .
       41016111-YMDHENKOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    連番の変更処理
      *****************************************************************
       41016111-RENHENKOU-SEC             SECTION.
      *
      *    日の変更（変更後は連番を１とする）
      **** MOVE    JYURRK-RENNUM       TO  IDX-D3
           MOVE    JYURRK-UPRENNUM     TO  IDX-D3
           COMPUTE IDX-D3              =   IDX-D3    +   1
           COMPUTE IDX-D4              =   WRK-RENNUM    +   1
      *
           MOVE    SPA-NAI-DAY (IDY IDX-D3 IDX-D1)
                                       TO  WRK-DAY-S
           MOVE    WRK-DAY-S           TO  SPA-NAI-DAY
                                                (IDY IDX-D4 IDX-D1)
           MOVE    ZERO                TO  SPA-NAI-DAY
                                                (IDY IDX-D3 IDX-D1)
           .
       41016111-RENHENKOU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    保険組合せ入力処理
      *****************************************************************
       41017-HKNCOMBI-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-HHKNCOMBI   TO  SPA-MAE-HHKNCOMBI
           MOVE    J02-HHKNCOMBI       TO  SPA-GMN-HHKNCOMBI-G
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-HKN-MAX
              IF      SPA-GMN-THKNCOMBINUM(IDX)
                                           =   SPA-GMN-HHKNCOMBI
                   MOVE    SPA-GMN-THKNCOMBINUM-G (IDX)
                                               TO  SPA-GMN-HHKNCOMBI-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0032"              TO  SPA-ERRCD
               MOVE    10                  TO  SPA-GMN-CUR
               GO      TO      41017-HKNCOMBI-CHK-EXT
           END-IF
      *    変更の有無
           IF     (SPA-GMN-HHKNCOMBI   =   SPA-MAE-HHKNCOMBI)
               GO      TO      41017-HKNCOMBI-CHK-EXT
           END-IF
      *
           PERFORM 410171-NYUGAIKBN-CHANGE-SEC
           IF      SPA-GMN-MAX         =   ZERO
               MOVE    10                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       41017-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    入外区分処理
      *****************************************************************
       41017-NYUGAIKBN-CHK-SEC             SECTION.
      *
           MOVE    J02-NYUGAIKBN       TO  SPA-GMN-NYUGAIKBN-G
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-NYUGAI-MAX
              IF      SPA-GMN-NYUGAIKBN-LST (IDX)(1:1)
                                           =   SPA-GMN-NYUGAIKBN
                   MOVE    SPA-GMN-NYUGAIKBN-LST (IDX)
                                               TO  SPA-GMN-NYUGAIKBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0030"              TO  SPA-ERRCD
               MOVE    08              TO  SPA-GMN-CUR
           END-IF
      *    変更の有無
           IF      SPA-NYUGAIKBN       =   SPA-GMN-NYUGAIKBN
               MOVE    1               TO  SPA-GMN-CUR
               GO      TO      41017-NYUGAIKBN-CHK-EXT
           END-IF
           IF      SPA-GMN-PTID        =   ZERO
               MOVE    00              TO  SPA-GMN-CUR
               GO      TO      41017-NYUGAIKBN-CHK-EXT
           END-IF
      *
           PERFORM 410171-NYUGAIKBN-CHANGE-SEC
      *
           .
       41017-NYUGAIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分変更処理
      *****************************************************************
       410171-NYUGAIKBN-CHANGE-SEC     SECTION.
      *
      *    現在表示されている内容を更新する
      *    診療会計マスター更新
           IF      SPA-GMN-MAX         >   ZERO
               PERFORM 4901-SRYACCT-UPD-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO       410171-NYUGAIKBN-CHANGE-EXT
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-NYUGAIKBN   TO  SPA-NYUGAIKBN
      *
      *****INITIALIZE                  SPA-J02-AREA
           IF      SPA-GMN-NYUGAIKBN   =   "2"
               INITIALIZE                  SPA-GMN-NYUIN-AREA
           END-IF
      *
      *    受診履歴編集
           PERFORM 3103-JYURRK-HEN-SEC
      *
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
      *    保存用パラメタ作成
           PERFORM 4201-SRYACCT-PARA-SEC
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-STRNUM
      *
      *    診療会計マスターより編集をする
           PERFORM 410181-SRYACCT-HEN-SEC
      *    保険組合一覧編集
      **** PERFORM 3105-HKNCOMBI-HEN-SEC
      *
           IF      SPA-GMN-MAX         =   ZERO
               MOVE    8                   TO  SPA-GMN-CUR
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *    受診履歴が無い時、メッセージ表示
           IF      SPA-RRK-MAX         =   ZERO
               IF      SPA-SRYKA-MAX       =   1
                   MOVE    07              TO  SPA-GMN-CUR
               ELSE
                   MOVE    09              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       410171-NYUGAIKBN-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療種別処理
      *****************************************************************
       41018-SRYKBN-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-SRYKBN      TO  SPA-MAE-SRYKBN
           MOVE    J02-SRYKBN          TO  SPA-GMN-SRYKBN-G
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-SRYKBN-MAX
              IF      SPA-GMN-SRYKBN-LST (IDX)(1:2)
                                           =   SPA-GMN-SRYKBN
                   MOVE    SPA-GMN-SRYKBN-LST (IDX)
                                               TO  SPA-GMN-SRYKBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0033"              TO  SPA-ERRCD
               MOVE    09                  TO  SPA-GMN-CUR
               GO      TO      41018-SRYKBN-CHK-EXT
           END-IF
      *    変更の有無
           IF     (SPA-GMN-SRYKBN      =   SPA-MAE-SRYKBN )  OR
                  (SPA-GMN-PTID        =   ZERO           )
               GO      TO      41018-SRYKBN-CHK-EXT
           END-IF
      *
           IF      SPA-UPDFLG          =   SPACE
               PERFORM 410171-NYUGAIKBN-CHANGE-SEC
           ELSE
               MOVE    "0110"              TO  SPA-JIDCD
           END-IF
      *
           .
       41018-SRYKBN-CHK-EXT.
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
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
       41012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア画面へ
      *****************************************************************
       420-CLEA-SEC        SECTION.
      *
           IF      SPA-GMN-BANGO       =   ZERO
      *        クリア
               MOVE    ZERO                TO  SPA-GMN-CUR
               INITIALIZE                  SPA-J02-AREA
               INITIALIZE                  SPA-SCR-REC
               INITIALIZE                  SPA-J02-SET
               INITIALIZE                  SPA-J02-SYOKI
               MOVE    SPA-J02-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM
               MOVE    SPA-J02-SRYYMD (1:6)    TO  SPA-NAI-SRYYM
               MOVE    SPACE               TO  SPA-GMN-SRYKA-G
               MOVE    ZERO                TO  SPA-PTID
               MOVE    SPACE               TO  SPA-PTNUM
               PERFORM 310-SPA-SET-SEC
           ELSE
      *        変更項目クリア
               INITIALIZE                  SPA-GMN-INPUT
               INITIALIZE                  SPA-NAIGMN-AREA
               MOVE    ZERO                TO  SPA-J02-SYORI
      *
               MOVE    1                   TO  SPA-GMN-CUR
      *
      *        排他制御解除処理
               PERFORM 990-LOCK-OUT-SEC
           END-IF
      *
           .
       420-CLEA-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤変更画面へ
      *****************************************************************
       420-ZAIHENKOU-SEC        SECTION.
      *
           IF      SPA-GMN-BANGO       =   ZERO
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "0013"              TO  SPA-ERRCD
               GO      TO      420-ZAIHENKOU-EXT
           END-IF
      *
           IF      SPA-NAI-RRKSELNUM   NOT =   ZERO
               MOVE    SPA-RRK-RRKYMD (SPA-NAI-RRKSELNUM)(7:2)
                                           TO  IDX-D
               IF      SPA-GMN-DAY (IDX-D) =   ZERO
                   MOVE    "0004"              TO  SPA-ERRCD
                   MOVE    3                   TO  SPA-GMN-CUR
                   GO      TO      420-ZAIHENKOU-EXT
               END-IF
           END-IF
      *
      *    労災の時、剤点数が外来管理加算料に振り替えた時、エラー
           IF      SPA-NAI-HKNKBN      =   1
               PERFORM 4201-RIS-ZAIHENKOU-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    1                   TO  SPA-GMN-CUR
                   GO      TO      420-ZAIHENKOU-EXT
               END-IF
           END-IF
      *
           MOVE    "0104"              TO  SPA-JIDCD
      *
           .
       420-ZAIHENKOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災の時剤変更チェック処理
      *****************************************************************
       4201-RIS-ZAIHENKOU-SEC        SECTION.
      *
           IF     (SPA-NAI-SYUTEN2 (SPA-NAI-BANGO)
                                        >   ZERO )  AND
                  (SPA-NAI-ZAITEN  (SPA-NAI-BANGO)
                                        >   SPA-NAI-SYUTEN2
                                                (SPA-NAI-BANGO))
               MOVE    "0021"               TO  SPA-ERRCD
           END-IF
           .
       4201-RIS-ZAIHENKOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤変更画面へ
      *****************************************************************
       420-ZAIHENKOU-SYORI-SEC        SECTION.
      *
      *    選択の内容をパラメタに編集
           MOVE    SPA-NAI-SRYYM       TO  SPA-J02-SRYYMD(1:6)
           MOVE    SPA-GMN-SRYYM       TO  SPA-J02-SRYYMDW(1:6)
      *
           MOVE    SPA-NAI-BANGO       TO  IDX
      *    見出し
           MOVE    SPA-GMN-SRYKAMEI    TO  SPA-J01-SRYKAMEI
      *    診療科
      **** MOVE    SPA-GMN-SRYKA       TO  SPA-J01-SRYKA
           MOVE    SPA-NAI-SRYKA(IDX)  TO  SPA-J01-SRYKA
      *    剤番号
           MOVE    SPA-NAI-SRYKBN(IDX)
                                       TO  SPA-SRYKBN
           MOVE    SPA-NAI-ZAINUM(IDX)
                                       TO  SPA-ZAINUM
      *    枝番
           MOVE    SPA-NAI-RENNUM(IDX) TO  SPA-RENNUM
      *    老人区分
      **** MOVE    SPA-NAI-ROUJIN(IDX)
      ****************                 TO  SPA-ROUJIN
      *    剤点数
           MOVE    SPA-NAI-ZAITEN(IDX)
                                       TO  SPA-ZAITEN
      *    保険組合番号
           MOVE    SPA-NAI-HKNCOMBI(IDX)
                                       TO  SPA-HKNCOMBINUM
      *    保険組合せ内容
           MOVE    SPA-GMN-HKNCOMBIMEI TO  SPA-J02-HKNCOMBIMEI
      *    診療種別
           MOVE    SPA-GMN-SRYKBN      TO  SPA-GMN-SRYKBN-MAE
      *
      *    保険算定年齢チェック等
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPID          TO  AGECHK-HOSPID
           MOVE    SPA-GMN-PTID        TO  AGECHK-PTID
           MOVE    SPA-NAI-SRYYM       TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"                TO  AGECHK-SRYYMD(7:2)
           MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
           MOVE    SPA-HKNCOMBINUM     TO  AGECHK-HKNCOMBINUM
           MOVE    SPA-GMN-NYUGAIKBN   TO  AGECHK-NYUGAIKBN
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
      *    負担率
           MOVE    AGECHK-FTNRATEMEI   TO  SPA-J02-FTNRATEMEI
      *    老人区分
           MOVE    AGECHK-ROUJIN       TO  SPA-ROUJIN
      *    回数テーブル
           MOVE    SPA-GMN-DAY-G       TO  SPA-SEL-DAY-G
      *
      *    受診履歴決定
           IF      SPA-NAI-RRKSELNUM   =   ZERO
               MOVE    SPACE               TO  SPA-SEL-RRKYMD
               MOVE    SPACE               TO  SPA-SEL-RRKYMDG
               MOVE    ZERO                TO  SPA-SEL-RENNUM
               MOVE    ZERO                TO  SPA-SEL-DOUJI-RENNUM
               MOVE    ZERO                TO  SPA-SEL-DENPNUM
           ELSE
               MOVE    SPA-GMN-RRKYMD (SPA-NAI-RRKSELNUM)
                                           TO  SPA-SEL-RRKYMDG
               MOVE    SPA-RRK-RRKYMD (SPA-NAI-RRKSELNUM)
                                           TO  SPA-SEL-RRKYMD
               MOVE    SPA-RRK-RENNUM (SPA-NAI-RRKSELNUM)
                                           TO  SPA-SEL-RENNUM
               MOVE    SPA-RRK-DOUJI-RENNUM (SPA-NAI-RRKSELNUM)
                                           TO  SPA-SEL-DOUJI-RENNUM
               MOVE    SPA-RRK-DENPNUM (SPA-NAI-RRKSELNUM)
                                           TO  SPA-SEL-DENPNUM
               MOVE    ZERO                TO  SPA-SEL-DAY-G
               MOVE    SPA-SEL-RRKYMD(7:2)     TO  IDX-D2
               COMPUTE IDX-D               =   SPA-SEL-RENNUM  + 1
               MOVE    SPA-NAI-DAY (IDX IDX-D  IDX-D2)
                                           TO  SPA-SEL-DAY (IDX-D2)
           END-IF
      *
      *    診療開始年月日
           MOVE    ZERO                TO  SPA-J02-SRYYMD(7:2)
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      SPA-GMN-DAY (IDX-D) NOT =   ZERO
                   MOVE    IDX-D           TO  WRK-DD
                   MOVE    WRK-DD          TO  SPA-J02-SRYYMD(7:2)
                   MOVE    31              TO  IDX-D
               END-IF
           END-PERFORM
      *
           MOVE    SPA-SRYYMD          TO  SPA-MAE-SRYYMD
           MOVE    SPA-PTID            TO  SPA-MAE-PTID
           MOVE    SPA-J02-SRYYMD      TO  SPA-SRYYMD
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
      *
      *    労災
           MOVE    SPA-NAI-HKNKBN      TO  SPA-HKNKBN
           MOVE    SPA-NAI-SISIKBN     TO  SPA-RSI-SISIKBN
      *    労災区分
           MOVE    SPA-NAI-RSI-HKNKBN  TO  SPA-RSI-HKNKBN
      *
      *    診療会計マスター更新
           PERFORM 4901-SRYACCT-UPD-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO       420-ZAIHENKOU-SYORI-EXT
           END-IF
      *
      *    診療開始日時点の年齢計算
           MOVE    SPA-SRYYMD          TO  WRK-YMDS2
           PERFORM 30101-NENREI-SET-SEC
      *
      *    保存用パラメタ作成
           PERFORM 4201-SRYACCT-PARA-SEC
      *
           MOVE    "J02"               TO  SPA-MOTOPG
           MOVE    "J04"               TO  SPA-SAKIPG
      *
      *****MOVE    "CHANGE"            TO  MCP-PUTTYPE.
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE    "J04"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       420-ZAIHENKOU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診日訂正処理
      *****************************************************************
       440-SRYYMD-TEISEI-SEC           SECTION.
      *
      *    患者選択チェック
           IF      SPA-GMN-MAX         =   ZERO
               GO      TO      440-SRYYMD-TEISEI-EXT
           END-IF
      *    受診日訂正へ
           MOVE    2                   TO  SPA-J02-SYORI
      *
           MOVE    3                   TO  SPA-GMN-CUR
           .
       440-SRYYMD-TEISEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター  保存用パラメタ作成処理
      *****************************************************************
       4201-SRYACCT-PARA-SEC           SECTION.
      *
      *    パラメタ削除
           INITIALIZE                      PARA-REC
           MOVE    "J02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SRYACCT"           TO  PARA-FILEMEI
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
      *****MOVE    "PARA-DEL11"        TO  ORC-DBPATH
           MOVE    "PARA-KEY2"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    診療会計マスターを編集
           INITIALIZE                      SRYACCT-REC 
           MOVE    SPA-HOSPID          TO  ACCT-HOSPID
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  ACCT-PTID
           MOVE    SPA-NAI-SRYYM       TO  ACCT-SRYYM
           IF      SPA-J01-SRYKA       =   ZERO
               MOVE    "SRYACCT-KEY19"     TO  WRK-ORC-DBPATH
           ELSE
               MOVE    SPA-J01-SRYKA       TO  ACCT-SRYKA
               MOVE    "SRYACCT-KEY18"     TO  WRK-ORC-DBPATH
           END-IF
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-ORC-DBPATH      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-ORC-DBPATH      TO  ORC-DBPATH
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
           MOVE    ZERO                TO  SPA-SRYACCT-MAX
           PERFORM UNTIL      (FLG-SRYACCT     =   1   )
      *
               MOVE    ZERO                 TO  FLG-ACCT-ERR
               IF     (SPA-GMN-HHKNCOMBI        =   ZERO    )  OR
                     ((SPA-GMN-HHKNCOMBI    NOT =   ZERO    )  AND
                      (SPA-GMN-HHKNCOMBI        =   ACCT-HKNCOMBI))
                  CONTINUE
               ELSE
                  MOVE    1                    TO  FLG-ACCT-ERR
               END-IF
               IF     (SPA-GMN-SRYKBN      =   ZERO            )  OR
                      (SPA-GMN-SRYKBN  NOT =   ZERO    AND
                       SPA-GMN-SRYKBN(1:1) =   ACCT-SRYKBN(1:1) )
                  CONTINUE
               ELSE
                  MOVE    1                    TO  FLG-ACCT-ERR
               END-IF
      *
               IF      FLG-ACCT-ERR        =   ZERO
                   ADD     1                   TO  SPA-SRYACCT-MAX
      *            パラメタ登録
                   INITIALIZE                      PARA-REC
                   MOVE    "J02"               TO  PARA-GYOUMUID
                   MOVE    SPA-TERMID          TO  PARA-TERMID
                   MOVE    "SRYACCT"           TO  PARA-FILEMEI
                   MOVE    SPA-SRYACCT-MAX     TO  PARA-EDANUM
                   MOVE    SRYACCT-REC         TO  PARA-DATA-REC
      *
                   MOVE    PARA-REC            TO  MCPDATA-REC
                   MOVE    "DBINSERT"          TO  MCP-FUNC
                   MOVE    "PARA-KEY"          TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"         USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "J02 PARA INSERT ERR:"  MCP-RC
                               ",KEY1:" PARA-KEY(1:42)
                       MOVE    "0014"              TO  SPA-ERRCD
                   END-IF
               END-IF
      *
               MOVE    WRK-ORC-DBPATH      TO  ORC-DBPATH
               PERFORM 900-SRYACCT-READ-SEC
           END-PERFORM
      *
      *    MOVE    "DBCLOSE"           TO  MCP-FUNC
      *    MOVE    WRK-ORC-DBPATH      TO  ORC-DBPATH
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
      *    最大ページ数
           IF      SPA-SRYACCT-MAX     =   ZERO
               MOVE    1               TO  SPA-MAX-PAGE
           ELSE
               COMPUTE SPA-MAX-PAGE    =  (SPA-SRYACCT-MAX   /   400)
                                       +   1
           END-IF
      *****MOVE    1                   TO  SPA-GMN-PAGE
      *****MOVE    1                   TO  SPA-STRNUM
      *
           .
       4201-SRYACCT-PARA-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *----(01.00.01) LINE ADD START ----------------------------------
      *    更新前のチェック
           IF      SPA-UPDFLG          =   "1"
               MOVE    "0102"          TO  SPA-JIDCD
               MOVE    99              TO  SPA-GMN-CUR
               GO      TO      210-BACK-EXT
           END-IF
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
      *    ファイル削除
           PERFORM 999-FILE-DEL-SEC
      *
      *
           MOVE    "J02"               TO  SPA-MOTOPG
           MOVE    SPA-MOTOPG2         TO  SPA-SAKIPG
           IF      SPA-J02-MOTOPG      =   "M01"
      *        業務選択へ
               MOVE   "M01"                TO  SPA-SAKIPG
               MOVE    SPACE               TO  LINKAREA
               INITIALIZE                  SPA-KYOTUKEY
               MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           ELSE
      *        元画面へ
               MOVE    SPACE               TO  LINKAREA
               IF      SPA-J02-MOTOFLG     =   "0"
                   INITIALIZE                  SPA-KYOTUKEY
               END-IF
               MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
               MOVE    SPACE               TO  SPA-MOTOPG2
           END-IF
      *
           MOVE    SPACE               TO  SPA-MOTOPG2
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK-SYORI-SEC           SECTION.
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
      *    ファイル削除
           PERFORM 999-FILE-DEL-SEC
      *
           MOVE    "J02"               TO  SPA-MOTOPG
           MOVE    SPA-J02-MOTOPG      TO  SPA-SAKIPG
           IF      SPA-J02-MOTOFLG     =   "0"
               INITIALIZE                  SPA-KYOTUKEY
           END-IF
           IF      SPA-J02-MOTOPG      =   "M01"
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
           END-IF
      *
           MOVE    SPACE               TO  SPA-MOTOPG2
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回患者処理
      *****************************************************************
       430-MAEKPTNUM-SEC              SECTION.
      *
           IF      SPA-LAST-PTNUM      =   SPACE
               GO      TO      430-MAEKPTNUM-EXT
           END-IF
      *
           MOVE    SPA-LAST-PTNUM      TO  J02-PTNUM
           PERFORM 41010-PTNUM-SYORI-SEC
      *
           .
       430-MAEKPTNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック画面へ
      *****************************************************************
       430-CHEKU-SEC              SECTION.
      *
      *----(01.00.01) LINE ADD START ----------------------------------
      *    剤修正のチェック
           IF      SPA-GMN-BANGO   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      430-CHEKU-EXT
           END-IF
      *    患者選択チェック
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    "0019"          TO  SPA-ERRCD
               GO      TO      430-CHEKU-EXT
           END-IF
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
      *
           MOVE    SPA-GMN-SRYYM       TO  SPA-J02-SRYYMDW(1:6)
           MOVE    SPA-NAI-SRYYM       TO  SPA-J02-SRYYMD (1:6)
           MOVE    SPA-GMN-SRYKA       TO  SPA-J01-SRYKA
           MOVE    SPA-GMN-SRYKAMEI    TO  SPA-J01-SRYKAMEI
      *    診療種別
           MOVE    SPA-GMN-SRYKBN      TO  SPA-GMN-SRYKBN-MAE
      *
           MOVE    "J02"               TO  SPA-MOTOPG
           MOVE    "J03"               TO  SPA-SAKIPG
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE.
      *****MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE    "J03"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       430-CHEKU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    フリーコメント画面へ
      *****************************************************************
       430-FREEMSG-SEC              SECTION.
      *
      *    剤修正のチェック
           IF      SPA-GMN-BANGO   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      430-FREEMSG-EXT
           END-IF
      *
           MOVE    SPA-GMN-SRYYM       TO  SPA-J02-SRYYMDW(1:6)
           MOVE    SPA-NAI-SRYYM       TO  SPA-J02-SRYYMD (1:6)
           MOVE    SPA-GMN-SRYKA       TO  SPA-J01-SRYKA
           MOVE    SPA-GMN-SRYKAMEI    TO  SPA-J01-SRYKAMEI
      *    診療種別
           MOVE    SPA-GMN-SRYKBN      TO  SPA-GMN-SRYKBN-MAE
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-J02             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGJ023"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-J02
           MOVE    SPA-WORK        TO  SPA-J01KYOUTU
      *
      *    カーソル移動
           MOVE    "COMMENT"           TO  MCP-WIDGET
      *
           MOVE    "J02"               TO  SPA-MOTOPG
           MOVE    "J023"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "J023"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       430-FREEMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    前月選択　処理
      *****************************************************************
       41018-ZENGETU-HEN-SEC             SECTION.
      *
      *----(01.00.01) LINE ADD START ----------------------------------
      *    剤修正のチェック
           IF      SPA-GMN-BANGO   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      41018-ZENGETU-HEN-EXT
           END-IF
      *
           MOVE    1                   TO  SPA-J02-SEL
      *
           IF      SPA-UPDFLG          =   "1"
               MOVE    "0106"              TO  SPA-JIDCD
           ELSE
               PERFORM 41018-ZENGETU-SYORI-SEC
           END-IF
      *
           .
       41018-ZENGETU-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    前月選択　処理
      *****************************************************************
       41018-ZENGETU-SYORI-SEC             SECTION.
      *
      *----(01.00.01) LINE ADD END   ----------------------------------
      *    現在表示されている内容を更新する
      *    診療会計マスター更新
           PERFORM 4901-SRYACCT-UPD-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO       41018-ZENGETU-SYORI-EXT
           END-IF
      *
           IF      SPA-BIRTHDAY(1:6)   =   SPA-NAI-SRYMM
               MOVE    "0015"              TO  SPA-ERRCD
               GO      TO       41018-ZENGETU-SYORI-EXT
            END-IF
      *
           COMPUTE SPA-NAI-SRYMM       =   SPA-NAI-SRYMM
                                       -   1
           IF      SPA-NAI-SRYMM       =   ZERO
               MOVE    12                  TO  SPA-NAI-SRYMM
               COMPUTE SPA-NAI-SRYYY       =   SPA-NAI-SRYYY    -   1
           END-IF
      *
      *
           MOVE    SPA-NAI-SRYYM      TO  WRK-SYMD(1:6)
           MOVE    "01"               TO  WRK-SYMD(7:2)
           PERFORM 41012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:6)    TO  SPA-GMN-SRYYM
      *
           MOVE    ZERO                TO  SPA-RRK-MAX
           INITIALIZE                  SPA-GMN-RRKTBL
           INITIALIZE                  SPA-NAI-RRKTBL
           MOVE    ZERO                TO  SPA-GMN-MAX
           INITIALIZE                  SPA-GMN-TBL
           INITIALIZE                  SPA-NAI-TBL
           INITIALIZE                  SPA-GMN-INPUT
           INITIALIZE                  SPA-GMN-NYUIN-AREA
      *
      *    診療科一覧編集
           PERFORM 3005-SRYKA-HEN-SEC
      *
      *    保険一覧編集
           PERFORM 3008-HKNCOMBI-HEN-SEC
      *
      *    受診履歴より診療科決定
           MOVE    SPACE               TO  SPA-J01-SRYKA
           MOVE    SPACE               TO  SPA-J01-SRYKAMEI
           PERFORM 310310-JYURRK-CHK-SEC
           IF      SPA-J01-SRYKA       =   SPACE
               MOVE    "0020"          TO  SPA-ERRCD
               MOVE    07              TO  SPA-GMN-CUR
               GO      TO       41018-ZENGETU-SYORI-EXT
           END-IF
      *
           PERFORM 3101-SRYKA-HEN-SEC
      *    入院が存在するかのチェック
           IF      SPA-BEDSU           >   ZERO
               PERFORM 410101-NYUINRRK-CHK-SEC
               PERFORM 410102-NYUINRRK-LST-SEC
           END-IF
      *
      *    受診履歴編集
           PERFORM 3103-JYURRK-HEN-SEC
      *
      *    保存用パラメタ作成
           PERFORM 4201-SRYACCT-PARA-SEC
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-STRNUM
      *
      *    診療会計マスターより編集をする
           PERFORM 410181-SRYACCT-HEN-SEC
      *
           IF      SPA-RRK-MAX         =   ZERO
               IF      SPA-SRYKA-MAX       =   1
                   MOVE    07              TO  SPA-GMN-CUR
               ELSE
                   MOVE    06              TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
      *
           .
       41018-ZENGETU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスターより編集　処理
      *****************************************************************
       410181-SRYACCT-HEN-SEC             SECTION.
      *
      *    コラムリスト位置
           MOVE    1                  TO  FLG-GMN-INIT
      *
           MOVE    SPA-NAI-SRYYM      TO  WRK-SYMD(1:6)
           MOVE    "01"               TO  WRK-SYMD(7:2)
           PERFORM 41012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:6)    TO  SPA-GMN-SRYYM
      *
           MOVE    SPA-GMN-SRYYM       TO  SPA-J02-SRYYMDW
           MOVE    SPA-NAI-SRYYM       TO  SPA-J02-SRYYMD
      *    診療年月の末日算定
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SPA-NAI-SRYYM       TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI(7:2)    TO  SPA-SRYMATU
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDG
           MOVE    ZERO                TO  SPA-GMN-MAX
           INITIALIZE                  SPA-GMN-TBL
           INITIALIZE                  SPA-NAI-TBL
           INITIALIZE                  SPA-GMN-INPUT
      *
      *    パラメタより診療会計マスターを編集
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
           MOVE    ZERO                TO  SPA-ENDNUM
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDG
           MOVE    ZERO                TO  WRK-BANGO
           MOVE    SPA-STRNUM          TO  SPA-MAE-STRNUM
           PERFORM
               UNTIL   FLG-PARA            =   1
      *        番号
               ADD     1                   TO  WRK-BANGO
      *        開始位置の判定
               IF      SPA-STRNUM          >   PARA-EDANUM
                   CONTINUE
               ELSE
                   MOVE    ZERO                TO  FLG-HEN-OK
                   MOVE    PARA-DATA-REC       TO  SRYACCT-REC
                   ADD     1                   TO  IDX
                   PERFORM 3101-SRYACCT-SPA-SEC
                   IF      FLG-HEN-OK           =   1
                       MOVE    PARA-EDANUM         TO  SPA-ENDNUM
                   END-IF
                   IF      IDG                 >=  400
                       MOVE    1               TO  FLG-PARA
                   END-IF
               END-IF
      *
               IF      FLG-PARA            =   ZERO
                   MOVE    "PARA-KEY2"         TO  ORC-DBPATH
                   PERFORM 990-PARA-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    IDX                 TO  SPA-NAI-MAX
           MOVE    IDG                 TO  SPA-GMN-MAX
           IF      IDX                 >   400
               MOVE    400             TO  SPA-NAI-MAX
           END-IF
           IF      IDG                 >   400
               MOVE    400             TO  SPA-GMN-MAX
           END-IF
      *
      *    すべての行を選択可能とする
           MOVE    ZERO                TO  WRK-TBANGO
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   SPA-GMN-MAX
               IF      SPA-GMN-TBLREC(IDX)(1:3)    NOT =   "---"
                   IF      SPA-GMN-TBANGO (IDX)    NOT =   ZERO
                       MOVE    SPA-GMN-TBANGO (IDX)    TO  WRK-TBANGO
                   END-IF
                   MOVE    WRK-TBANGO      TO  SPA-NAI-TBANGO2(IDX)
               END-IF
           END-PERFORM
      *
           .
       410181-SRYACCT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次月選択　処理
      *****************************************************************
       41019-JIGETU-HEN-SEC             SECTION.
      *
      *----(01.00.01) LINE ADD START ----------------------------------
      *    剤修正のチェック
           IF      SPA-GMN-BANGO   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      41019-JIGETU-HEN-EXT
           END-IF
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
           MOVE    2                   TO  SPA-J02-SEL
           IF      SPA-UPDFLG          =   "1"
               MOVE    "0106"              TO  SPA-JIDCD
           ELSE
               PERFORM 41019-JIGETU-SYORI-SEC
           END-IF
      *
           .
       41019-JIGETU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次月選択　処理
      *****************************************************************
       41019-JIGETU-SYORI-SEC             SECTION.
      *
      *    現在表示されている内容を更新する
      *    診療会計マスター更新
           PERFORM 4901-SRYACCT-UPD-SEC
           IF      SPA-ERRCD      NOT  =   SPACE
               GO      TO       41019-JIGETU-SYORI-EXT
           END-IF
      *
      *
           COMPUTE SPA-NAI-SRYMM       =   SPA-NAI-SRYMM
                                       +   1
           IF      SPA-NAI-SRYMM       >   12
               MOVE    1                   TO  SPA-NAI-SRYMM
               COMPUTE SPA-NAI-SRYYY       =   SPA-NAI-SRYYY    +   1
           END-IF
      *
           MOVE    SPA-NAI-SRYYM      TO  WRK-SYMD(1:6)
           MOVE    "01"               TO  WRK-SYMD(7:2)
           PERFORM 41012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:6)    TO  SPA-GMN-SRYYM
      *
      *
           MOVE    ZERO                TO  SPA-RRK-MAX
           INITIALIZE                  SPA-GMN-RRKTBL
           INITIALIZE                  SPA-NAI-RRKTBL
           MOVE    ZERO                TO  SPA-GMN-MAX
           INITIALIZE                  SPA-GMN-TBL
           INITIALIZE                  SPA-NAI-TBL
           INITIALIZE                  SPA-GMN-INPUT
           INITIALIZE                  SPA-GMN-NYUIN-AREA
      *
      *    診療科一覧編集
           PERFORM 3005-SRYKA-HEN-SEC
      *
      *    保険一覧編集
           PERFORM 3008-HKNCOMBI-HEN-SEC
      *
      *    受診履歴より診療科決定
           MOVE    SPACE               TO  SPA-J01-SRYKA
           MOVE    SPACE               TO  SPA-J01-SRYKAMEI
           PERFORM 310310-JYURRK-CHK-SEC
           IF      SPA-J01-SRYKA       =   SPACE
               MOVE    "0020"          TO  SPA-ERRCD
               MOVE    07              TO  SPA-GMN-CUR
               GO      TO       41019-JIGETU-SYORI-EXT
           END-IF
      *
           PERFORM 3101-SRYKA-HEN-SEC
      *    入院が存在するかのチェック
           IF      SPA-BEDSU           >   ZERO
               PERFORM 410101-NYUINRRK-CHK-SEC
               PERFORM 410102-NYUINRRK-LST-SEC
           END-IF
      *
      *    受診履歴編集
           PERFORM 3103-JYURRK-HEN-SEC
      *
      *    保存用パラメタ作成
           PERFORM 4201-SRYACCT-PARA-SEC
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-STRNUM
      *
      *    診療会計マスターより編集をする
           PERFORM 410181-SRYACCT-HEN-SEC
      *
           IF      SPA-RRK-MAX         =   ZERO
               IF      SPA-SRYKA-MAX       =   1
                   MOVE    07              TO  SPA-GMN-CUR
               ELSE
                   MOVE    06              TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
           .
       41019-JIGETU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       440-MAEGMN-SEC             SECTION.
      *
           IF      SPA-GMN-PAGE        =   1
               GO      TO       440-MAEGMN-EXT
           END-IF
      *
           IF      SPA-UPDFLG          =   "1"
               MOVE    "0113"              TO  SPA-JIDCD
           ELSE
               PERFORM 4401-MAEGMN-SYORI-SEC
           END-IF
           .
       440-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       4401-MAEGMN-SYORI-SEC             SECTION.
      *
      *    現在表示されている内容を更新する
      *    診療会計マスター更新
           PERFORM 4901-SRYACCT-UPD-SEC
           IF      SPA-ERRCD      NOT  =   SPACE
               GO      TO       4401-MAEGMN-SYORI-EXT
           END-IF
      *
           IF      SPA-GMN-PAGE        =   1
               MOVE    1               TO  SPA-GMN-PAGE
           ELSE
               COMPUTE SPA-GMN-PAGE    =   SPA-GMN-PAGE   -   1
           END-IF
      *
           IF      SPA-GMN-PAGE        =   1
               MOVE    1               TO  SPA-STRNUM
           ELSE
               MOVE    SPA-MAE-STRNUM  TO  SPA-STRNUM
           END-IF
      *
      *    保存用パラメタ作成
           PERFORM 4201-SRYACCT-PARA-SEC
      *
      *    診療会計マスターより編集をする
           PERFORM 410181-SRYACCT-HEN-SEC
      *
           IF      SPA-RRK-MAX         =   ZERO
               IF      SPA-SRYKA-MAX       =   1
                   MOVE    07              TO  SPA-GMN-CUR
               ELSE
                   MOVE    06              TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
           .
       4401-MAEGMN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       450-ATOGMN-SEC             SECTION.
      *
          IF      SPA-SRYACCT-MAX     <=  SPA-ENDNUM
               GO      TO       450-ATOGMN-EXT
           END-IF
      *
           IF      SPA-UPDFLG          =   "1"
               MOVE    "0114"              TO  SPA-JIDCD
           ELSE
               PERFORM 4501-ATOGMN-SYORI-SEC
           END-IF
           .
       450-ATOGMN-EXT.
           EXIT.
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       4501-ATOGMN-SYORI-SEC             SECTION.
      *
      *    現在表示されている内容を更新する
      *    診療会計マスター更新
           PERFORM 4901-SRYACCT-UPD-SEC
           IF      SPA-ERRCD      NOT  =   SPACE
               GO      TO       4501-ATOGMN-SYORI-EXT
           END-IF
      *
           ADD     1                   TO  SPA-GMN-PAGE
           MOVE    SPA-ENDNUM          TO  SPA-STRNUM
           ADD     1                   TO  SPA-STRNUM
      *
      *    保存用パラメタ作成
           PERFORM 4201-SRYACCT-PARA-SEC
      *
      *
      *    診療会計マスターより編集をする
           PERFORM 410181-SRYACCT-HEN-SEC
      *
           IF      SPA-RRK-MAX         =   ZERO
               IF      SPA-SRYKA-MAX       =   1
                   MOVE    07              TO  SPA-GMN-CUR
               ELSE
                   MOVE    06              TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
           .
       4501-ATOGMN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    変更確定　処理
      *****************************************************************
       470-KAKUTEI-SEC             SECTION.
      *
           IF      SPA-GMN-BANGO       =   ZERO
               GO      TO      470-KAKUTEI-EXT
           END-IF
      *
      *    点滴エラークリア
           MOVE    ZERO                TO  SPA-J02-TENTEKI
      *
      *****更新前チェック
      *    PERFORM 41017-HKNCOMBI-CHK-SEC
      *    IF      SPA-ERRCD       NOT =   SPACE
      *        GO      TO      470-KAKUTEI-EXT
      *****END-IF
      *
      *****MOVE    SPA-GMN-BANGO       TO  IDX
           MOVE    SPA-NAI-BANGO       TO  IDX
      *
      **** MOVE    SPA-NAI-TBANGO (IDX)
      ****                             TO  IDG
           MOVE    ZERO                TO  IDG
           PERFORM VARYING     IDXG    FROM    1   BY  1
                   UNTIL       IDXG    >   SPA-GMN-MAX
               IF      SPA-GMN-BANGO       =   SPA-GMN-TBANGO
                                                            (IDXG)
                   MOVE    IDXG                TO  IDG
                   MOVE    SPA-GMN-MAX         TO  IDXG
               END-IF
           END-PERFORM
      *
           MOVE   SPA-GNAI-DAY-G       TO  SPA-NAI-DAY-AREA (IDX)
      *
           MOVE   SPA-GMN-HKNCOMBI     TO  SPA-NAI-HKNCOMBI  (IDX)
      *!   MOVE   SPA-GMN-ROUJIN       TO  SPA-NAI-ROUJIN    (IDX)
      *    組合せ
           MOVE   SPA-GMN-HKNCOMBI     TO  SPA-GMN-THKNCOMBI (IDG)
      *    回数
           MOVE    ZERO                TO  WRK-ZAIKAISU
           PERFORM VARYING    IDK      FROM    1   BY  1
                   UNTIL      IDK      >   31
               ADD     SPA-NAI-DAY  (IDX 1 IDK)
                                       TO  WRK-ZAIKAISU
               MOVE    SPA-NAI-DAY  (IDX 1 IDK)
                                       TO  WRK-ZZ
               MOVE    WRK-ZZ-X        TO  SPA-GMN-TDAY    (IDG IDK)
               IF      SPA-NAI-DAY  (IDX 1 IDK)    >=  100
                   MOVE    SPA-NAI-DAY  (IDX 1 IDK)
                                           TO  SPA-GMN-TDAY (IDG IDK)
               END-IF
           END-PERFORM
           MOVE    WRK-ZAIKAISU        TO  SPA-NAI-ZAIKAISU (IDX)
           IF      SPA-NAI-ZAIKAISU (IDX)  >=   100
               MOVE    SPA-NAI-ZAIKAISU (IDX)  TO  WRK-ZZ9
               MOVE    WRK-ZZ9-X               TO  SPA-GMN-TDAYGOK(IDG)
           ELSE
               MOVE    SPA-NAI-ZAIKAISU (IDX)   TO  WRK-Z9
               MOVE    WRK-Z9-X                 TO  SPA-GMN-TDAYGOK(IDG)
           END-IF
      *
      *    合計回数の再表示
           IF      SPA-NAIGMN-SRYKBN   =   "95"  OR  "96"
               CONTINUE
           ELSE
               PERFORM 31013-INPUTCD-HEN-SEC
               MOVE    SPA-NAI-ENDBANGO (IDX)
                                           TO  IDG 
               MOVE    WRK-TENSU-X         TO  SPA-GMN-TTENSU  (IDG)
           END-IF
      *
      *    変更項目クリア
           INITIALIZE                  SPA-GMN-INPUT
           INITIALIZE                  SPA-NAIGMN-AREA
           MOVE    ZERO                TO  SPA-J02-SYORI
      *
      *    番号選択へカーソル移動
           MOVE    1                   TO  SPA-GMN-CUR
      *
      *----(01.00.01) LINE ADD START ----------------------------------
      *    剤確定あり
           MOVE    "1"                 TO  SPA-UPDFLG
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
           .
       470-KAKUTEI-EXT.
           EXIT.
      *****************************************************************
      *    氏名検索へ
      *****************************************************************
       470-SIMEIKEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-J02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "J02"               TO  SPA-MOTOPG
           MOVE    "P97"               TO  SPA-SAKIPG
      *
      *    氏名をクリアする
           MOVE    SPACE               TO  SPA-NAME
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
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
      *----(01.00.01) LINE ADD START ----------------------------------
      *    剤修正のチェック
           IF      SPA-GMN-BANGO   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      480-YOYAKU-EXT
           END-IF
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-J02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "J02"               TO  SPA-MOTOPG
           MOVE    "J02"               TO  SPA-MOTOPG3
           MOVE    "Y01"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
      **** MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE   "Y01"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       480-YOYAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付一覧へ
      *****************************************************************
       490-UKEICHI-SEC             SECTION.
      *
      *    保存用パラメタ作成
           PERFORM 4201-SRYACCT-PARA-SEC
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-J02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "J02"               TO  SPA-MOTOPG
           MOVE    "U01"               TO  SPA-SAKIPG
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
      **** MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE   "U01    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-UKEICHI-EXT.
           EXIT.
      *
      *****************************************************************
      *   画面移行用のパラメタを作成
      *****************************************************************
       4430-SPA-OUT-SEC                SECTION.
      *
           .
       4430-SPA-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       490-TOROKU-SEC              SECTION.
      *
      *    更新前のチェック
           IF      SPA-GMN-BANGO   NOT =   ZERO
      *----(01.00.01) LINE UPD START ----------------------------------
      ******** MOVE    "0101"          TO  SPA-JIDCD
               MOVE    "0009"          TO  SPA-ERRCD
      *----(01.00.01) LINE UPD END   ----------------------------------
               GO      TO      490-TOROKU-EXT
           END-IF
      *
      *    診療会計マスター更新
           PERFORM 4901-SRYACCT-UPD-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO       490-TOROKU-EXT
           END-IF
      *
      *    患者が選択されてきた時は、元の画面へ
           IF      SPA-J02-MOTOFLG     =   "0"
               MOVE    ZERO            TO  SPA-GMN-CUR
               INITIALIZE                  SPA-J02-AREA
               INITIALIZE                  SPA-SCR-REC
               INITIALIZE                  SPA-J02-SET
               INITIALIZE                  SPA-GMN-NYUIN-AREA
               MOVE    SPA-J02-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM
               MOVE    SPA-J02-SRYYMD (1:6)    TO  SPA-NAI-SRYYM
               MOVE    SPACE               TO  SPA-GMN-SRYKA-G
      *
      *        排他制御解除処理
               PERFORM 990-LOCK-OUT-SEC
           ELSE
      *        更新後、戻るへ
               PERFORM 210-BACK
           END-IF
           .
       490-TOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター更新処理
      *****************************************************************
       4901-SRYACCT-UPD-SEC              SECTION.
      *
      *    変更がない時、処理なし
           IF      SPA-UPDFLG          =   "1"
               CONTINUE
           ELSE
               GO      TO      4901-SRYACCT-UPD-EXT
           END-IF
      *
      *----(01.00.01) LINE ADD START ----------------------------------
           MOVE    SPACE               TO  SPA-UPDFLG
      *----(01.00.01) LINE ADD END   ----------------------------------
      *    対象がない時、処理なし
           IF      SPA-NAI-MAX         =   ZERO
               GO      TO      4901-SRYACCT-UPD-EXT
           END-IF
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    診療会計マスターを編集
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-NAI-MAX
               INITIALIZE                      SRYACCT-REC
               MOVE    SPA-HOSPID          TO  ACCT-HOSPID
               MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
               MOVE    SPA-GMN-PTID        TO  ACCT-PTID
      *********MOVE    SPA-J01-SRYKA       TO  ACCT-SRYKA
               MOVE    SPA-NAI-SRYKA (IDX) TO  ACCT-SRYKA
               MOVE    SPA-NAI-SRYYM       TO  ACCT-SRYYM
               MOVE    SPA-NAI-SRYKBN(IDX) TO  ACCT-SRYKBN
               MOVE    SPA-NAI-ZAINUM(IDX) TO  ACCT-ZAINUM
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
                   PERFORM 900-SRYACCT-READ-SEC
               ELSE
                   INITIALIZE                  SRYACCT-REC
                   MOVE    1               TO  FLG-SRYACCT
               END-IF
               IF      FLG-SRYACCT          =   ZERO
      *            診療会計マスターを変更する
                   PERFORM 4901-SRYACCT-KOUSIN-SEC
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
                        DISPLAY "J02 SRYACCT UPD ERR:" MCP-RC
                        MOVE    "2000"          TO  SPA-ERRCD
                        GO  TO  4901-SRYACCT-UPD-EXT
                   END-IF
               ELSE
                   DISPLAY "J02 SRYACCT READ ERR:" ACCT-KEY
               END-IF
      *
      *        MOVE    "DBCLOSE"           TO  MCP-FUNC
      *        MOVE    "SRYACCT-KEY"       TO  ORC-DBPATH
      *        CALL    "ORCMCPSUB"          USING
      *                                     MCPAREA
      *                                     ORCMCPAREA
      *                                     MCPDATA-REC
           END-PERFORM
      *
      *    受診履歴の更新
           PERFORM 49011-JYURRK-UPD-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4901-SRYACCT-UPD-EXT
           END-IF
      *
      *    診療科履歴の初診算定日修正処理
      *
      *    全科指定の時、対象の科すべて
           IF      SPA-J01-SRYKA       =   ZERO
               MOVE    SPA-SRYKA           TO  WRK-SRYKA
               PERFORM VARYING    IDX      FROM    1   BY  1
                       UNTIL      IDX      >   SPA-NAI-MAX
                   IF    ( IDX             =   1  )  OR
                         ((SPA-NAI-SRYKA (IDX)
                                       NOT =   SPACE  AND ZERO ) AND
                          (SPA-NAI-SRYKA (IDX)
                                       NOT =   SPA-NAI-SRYKA (IDX - 1)))
                       INITIALIZE                  ORCSKARRKAREA
                       MOVE    SPA-GMN-PTID        TO  SPA-PTID
                       MOVE    SPA-NAI-SRYYM       TO  SKARRK-SRYYM
                       MOVE    SPA-NAI-SRYKA (IDX)
                                                   TO  SPA-SRYKA
      *
                       CALL    "ORCSKARRK"         USING
                                                   ORCSKARRKAREA
                                                   SPA-AREA
                   END-IF
               END-PERFORM
               MOVE    WRK-SRYKA           TO  SPA-SRYKA
               IF      SKARRK-ERRMSG   NOT =   SPACE
                   MOVE    "0010"              TO  SPA-ERRCD
                   GO      TO      4901-SRYACCT-UPD-EXT
               END-IF
           ELSE
               INITIALIZE                  ORCSKARRKAREA
               MOVE    SPA-GMN-PTID        TO  SPA-PTID
               MOVE    SPA-NAI-SRYYM       TO  SKARRK-SRYYM
               MOVE    SPA-SRYKA           TO  WRK-SRYKA
               MOVE    SPA-J01-SRYKA       TO  SPA-SRYKA
      *
               CALL    "ORCSKARRK"         USING
                                           ORCSKARRKAREA
                                           SPA-AREA
               MOVE    WRK-SRYKA           TO  SPA-SRYKA
      *
               IF      SKARRK-ERRMSG   NOT =   SPACE
                   MOVE    "0010"              TO  SPA-ERRCD
                   GO  TO  4901-SRYACCT-UPD-EXT
               END-IF
           END-IF
      *
      *    点滴チェック
           IF      SPA-GMN-NYUGAIKBN   =   "1"
      *        点滴一括変更処理
               PERFORM 42001-TENTEKI-SYORI-SEC
           END-IF
      *
           .
       4901-SRYACCT-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター  変更処理
      *****************************************************************
       4901-SRYACCT-KOUSIN-SEC           SECTION.
      *
      *    回数変更時、算定履歴を変更する
           IF      SPA-NAI-DAY-AREA (IDX)  NOT =   ACCT-DAY-AREA
               PERFORM 4902-SANTEI-KOUSIN-SEC
           END-IF
      *
      *    保険組合せ
           MOVE    SPA-NAI-HKNCOMBI(IDX)   TO  ACCT-HKNCOMBI
      *    剤回数
           MOVE    SPA-NAI-ZAIKAISU(IDX)   TO  ACCT-ZAIKAISU
      *    回数テーブル（１日から３１日）
           MOVE    SPA-NAI-DAY-AREA (IDX)  TO  ACCT-DAY-AREA
           .
       4901-SRYACCT-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴の更新処理
      *****************************************************************
       49011-JYURRK-UPD-SEC           SECTION.
      *
           MOVE    ZERO                TO  IDG
      *    受診履歴パラメタ削除
           INITIALIZE                      PARA-REC
           MOVE    "J02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "JYURRK"            TO  PARA-FILEMEI
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
      *****MOVE    "PARA-DEL11"        TO  ORC-DBPATH
           MOVE    "PARA-KEY2"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           MOVE    ZERO                TO  WRK-PARA-EDANUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-RRK-MAX )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               PERFORM 49011-JYURRK-PARAUPD-SEC
           END-PERFORM
      *
      *    パラメタファイルより受診履歴作成
           INITIALIZE                      PARA-REC
           MOVE    "J02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "JYURRK"            TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PARA-KEY2"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
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
           PERFORM
               UNTIL   FLG-PARA            =   1
      *
               MOVE    PARA-DATA-REC       TO  JYURRK-REC
      *
               MOVE    SMCNDATE-YMD        TO  JYURRK-CREYMD
               MOVE    SMCNDATE-YMD        TO  JYURRK-UPYMD
               MOVE    SMCNDATE-HMS        TO  JYURRK-UPHMS
      *
               MOVE    JYURRK-REC          TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "JYURRK-KEY"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC          NOT =   ZERO
                     DISPLAY "J02 JYURRK INS ERR:" MCP-RC
                             ",KEY:" JYURRK-KEY  "."
               END-IF
      *
               MOVE    "PARA-KEY2"         TO  ORC-DBPATH
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
      *
           .
       49011-JYURRK-UPD-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴の更新処理
      *****************************************************************
       49011-JYURRK-PARAUPD-SEC           SECTION.
      *
           MOVE    1                   TO  IDR
           MOVE    ZERO                TO  FLG-JYURRK-ARI
      *
      *    受診歴を検索する  医療機関、患者ＩＤ、伝票番号
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPID          TO  JYURRK-HOSPID
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-RRK-SRYKA (IDX) TO  JYURRK-SRYKA
           IF      SPA-NYUGAIKBN       =   "2"
      *        外来の時、伝票番号
               MOVE    SPA-RRK-DENPNUM(IDX)
                                           TO  JYURRK-DENPNUM
      *
               MOVE    "JYURRK-KEY6"       TO  WRK-ORC-DBPATH
           ELSE
      *        入院の時、診療年月日、保険組合せ
               MOVE    SPA-RRK-RRKYMD-MAE(IDX)
                                           TO  JYURRK-SRYYMD
               MOVE    SPA-RRK-RENNUM(IDX)
                                           TO  JYURRK-RENNUM
               MOVE    SPA-RRK-HKNCOMBI(IDX)
                                           TO  JYURRK-HKNCOMBINUM
               MOVE    "JYURRK-KEY19"      TO  WRK-ORC-DBPATH
           END-IF
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-ORC-DBPATH      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-ORC-DBPATH      TO  ORC-DBPATH
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           MOVE    ZERO                TO  IDX3
           MOVE    ZERO                TO  IDX-T
           INITIALIZE                  TBL-JYURRK-AREA
           INITIALIZE                  TBL-MAE-ZAINUM-AREA
           PERFORM UNTIL   FLG-JYURRK      =   1
               ADD     1                   TO  IDX-T
               MOVE    JYURRK-REC          TO  TBL-JYURRK-REC(IDX-T)
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2    >   15  )  OR
                                  (IDX3    >=  135 )
                   IF      JYURRK-ZAINUM (IDX2)    NOT =   ZERO
                       ADD     1               TO  IDX3
                       MOVE    JYURRK-ZAINUM (IDX2)
                                               TO  TBL-MAE-ZAINUM (IDX3)
                   END-IF
               END-PERFORM
      *
               MOVE    WRK-ORC-DBPATH      TO  ORC-DBPATH
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
      *    剤変更チェック
           MOVE    ZERO                TO  FLG-ERR
           PERFORM VARYING     IDR     FROM    1   BY  1
                   UNTIL      (IDR     >   135  )  OR
                              (FLG-ERR =   1    )
               IF      SPA-RRK-ZAINUM (IDX IDR)    NOT =   ZERO
                   MOVE    ZERO                TO  FLG-OK
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL      (IDX2    >   135  )  OR
                                      (FLG-OK      =   1 )
                       IF      SPA-RRK-ZAINUM (IDX IDR)
                                               =   TBL-MAE-ZAINUM (IDX2)
                           MOVE    ZERO        TO  TBL-MAE-ZAINUM (IDX2)
                           MOVE    1           TO  FLG-OK
                       END-IF
                   END-PERFORM
                   IF      FLG-OK              =   ZERO
                       MOVE    1               TO  FLG-ERR
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      FLG-ERR             =   ZERO
               IF      TBL-MAE-ZAINUM-AREA =   ZERO
      *            剤がすべて同じ時、変更なし
                   GO      TO      49011-JYURRK-PARAUPD-EXT
               END-IF
            END-IF
           MOVE    1                   TO  IDR
           PERFORM VARYING     IDX3    FROM    1   BY  1
                   UNTIL      (IDX3    >   IDX-T   )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               MOVE    TBL-JYURRK-REC(IDX3)    TO  JYURRK-REC
      *        キー更新チェック
               IF     (SPA-RRK-RRKYMD (IDX)    =   JYURRK-SRYYMD )
                 AND  (SPA-RRK-RENNUM (IDX)    =   JYURRK-RENNUM )
                 AND  (SPA-RRK-DOUJI-RENNUM(IDX)
                                               =   JYURRK-DOUJI-RENNUM )
                 AND  (SPA-RRK-HKNCOMBI(IDX)   =   JYURRK-HKNCOMBINUM)
                   MOVE    1                   TO  FLG-UPD
               ELSE
                   MOVE    2                   TO  FLG-UPD
               END-IF
      *        剤番号変更
               INITIALIZE                  JYURRK-ZAINUM-G
               MOVE    ZERO                TO  IDX2
               PERFORM
                       UNTIL      (IDX2    >=  15  )   OR
                                  (IDR     >   135 )
                   MOVE    ZERO                TO  FLG-OK
                   PERFORM
                           UNTIL  (FLG-OK      =   1   )  OR
                                  (IDR         >   135 )
                       IF      SPA-RRK-ZAINUM (IDX IDR)    =   ZERO
                           CONTINUE
                       ELSE
                           ADD     1               TO  IDX2
                           MOVE    SPA-RRK-ZAINUM (IDX IDR)
                                                   TO  JYURRK-ZAINUM
                                                              (IDX2)
                           MOVE    ZERO            TO  SPA-RRK-ZAINUM
                                                            (IDX IDR)
                           MOVE    1               TO  FLG-OK
                       END-IF
                       ADD     1                   TO  IDR
                   END-PERFORM
               END-PERFORM
               IF     (IDX2                =   ZERO ) AND
                      (JYURRK-EDANUM       >   1    )
                   PERFORM 490112-JYURRK-DEL-SEC
               ELSE
                   PERFORM 490113-JYURRK-UPD-SEC
               END-IF
           END-PERFORM
      *
      *    受診履歴の追加判定
           IF     (SPA-RRK-ADD (IDX)   =   "1" )  OR
                  (IDX-T               =   ZERO )
               PERFORM 4901121-JYURRK-ADDHEN-SEC
               GO      TO      49011-JYURRK-PARAUPD-EXT
           END-IF
      *
           MOVE    IDR                 TO  IDX2
           MOVE    ZERO                TO  IDR3
           MOVE    TBL-JYURRK-REC(IDX-T)
                                       TO  JYURRK-REC
           INITIALIZE                  JYURRK-ZAINUM-G
           PERFORM VARYING     IDR     FROM    IDX2   BY  1
                   UNTIL       IDR     >   135
               IF      SPA-RRK-ZAINUM (IDX IDR) NOT=   ZERO
      *            受診履歴の追加
                   ADD     1               TO  IDR3
                   IF      IDR3            >   15
                       PERFORM 490111-JYURRK-INS-SEC
                       MOVE    1               TO  IDR3
                   END-IF
      *
                   MOVE    SPA-RRK-ZAINUM (IDX IDR)
                                           TO  JYURRK-ZAINUM (IDR3)
               END-IF
           END-PERFORM
           IF      IDR3                >   ZERO
               PERFORM 490111-JYURRK-INS-SEC
           END-IF
      *    収納マスタの更新
           IF      SPA-NYUGAIKBN       =   "2"
               PERFORM 490113-SYUNOU-UPD-SEC
           END-IF
           .
       49011-JYURRK-PARAUPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴の更新処理
      *****************************************************************
       490113-JYURRK-UPD-SEC           SECTION.
      *
      *    受診履歴変更
           MOVE    JYURRK-KEY          TO  JYURRK-UPKEY
           MOVE    SMCNDATE-YMD        TO  JYURRK-UPYMD
           MOVE    SMCNDATE-HMS        TO  JYURRK-UPHMS
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "JYURRK-UPD1"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J02 JYURRK UPD ERR:" MCP-RC
                               ",KEY:" JYURRK-KEY
               MOVE    "2001"              TO  SPA-ERRCD
           END-IF
           MOVE    1                   TO  FLG-JYURRK-ARI
      *
           .
       490113-JYURRK-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴の追加処理
      *****************************************************************
       490111-JYURRK-INS-SEC           SECTION.
      *
      *    枝番
           ADD     1                   TO  JYURRK-EDANUM
      *
           MOVE    SMCNDATE-YMD        TO  JYURRK-CREYMD
           MOVE    SMCNDATE-YMD        TO  JYURRK-UPYMD
           MOVE    SMCNDATE-HMS        TO  JYURRK-UPHMS
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "JYURRK-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J02 JYURRK INS ERR:" MCP-RC
                               ",KEY:" JYURRK-KEY
               MOVE    "2001"              TO  SPA-ERRCD
           END-IF
           INITIALIZE                  JYURRK-ZAINUM-G
           MOVE    1                   TO  FLG-JYURRK-ARI
           .
       490111-JYURRK-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴の削除処理
      *****************************************************************
       490112-JYURRK-DEL-SEC           SECTION.
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "JYURRK-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J02 JYURRK DEL ERR:" MCP-RC
                               ",KEY:" JYURRK-KEY
               MOVE    "2001"              TO  SPA-ERRCD
           END-IF
      *
           .
       490112-JYURRK-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴の追加出力処理
      *****************************************************************
       4901121-JYURRK-ADDHEN-SEC           SECTION.
      *
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPID          TO  JYURRK-HOSPID
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-RRK-SRYKA (IDX) TO  JYURRK-SRYKA
           MOVE    SPA-RRK-RRKYMD(IDX) TO  JYURRK-SRYYMD
           MOVE    SPA-RRK-RENNUM(IDX) TO  JYURRK-RENNUM
           MOVE    SPA-RRK-DOUJI-RENNUM(IDX)
                                       TO  JYURRK-DOUJI-RENNUM
           MOVE    ZERO                TO  JYURRK-EDANUM
      *    会計連番
           MOVE    SPA-RRK-KAIKEI-RENNUM(IDX)
                                       TO  JYURRK-KAIKEI-RENNUM
      *
           MOVE    "1"                 TO  JYURRK-LASTKBN
      *
           MOVE    SPA-DRCD            TO  JYURRK-DRCD
           MOVE    SPA-RRK-HKNCOMBI(IDX)
                                       TO  JYURRK-HKNCOMBINUM
           MOVE    SPA-RRK-HKNKBN(IDX) TO  JYURRK-HKNKBN
           MOVE    ZERO                TO  JYURRK-DENPNUM
      *    診療区分領域の編集
           PERFORM 4901122-JYURRK-SRYKBN-SEC
      *    剤番号変更
           MOVE    ZERO                TO  IDX2
           PERFORM VARYING     IDR     FROM    1   BY   1
                   UNTIL       IDR         >   135
               IF      SPA-RRK-ZAINUM (IDX IDR)    =   ZERO
                   CONTINUE
               ELSE
                   ADD     1               TO  IDX2
                   IF      IDX2            >   15
      *                出力処理
                       PERFORM 490111-JYURRK-INS-SEC
                       MOVE    1               TO  IDX2
                   END-IF
                   MOVE    SPA-RRK-ZAINUM (IDX IDR)
                                           TO  JYURRK-ZAINUM (IDX2)
                   MOVE    ZERO            TO  SPA-RRK-ZAINUM
                                                             (IDX IDR)
               END-IF
           END-PERFORM
      *    パラメタ出力処理
           IF      IDX2                >   ZERO
               PERFORM 490111-JYURRK-INS-SEC
           END-IF
      *
           .
       4901121-JYURRK-ADDHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴種別編集処理
      *****************************************************************
       4901122-JYURRK-SRYKBN-SEC           SECTION.
      *
           EVALUATE    SPA-RRK-SRYKBN(IDX)
               WHEN   "11"
               WHEN   "12"
               WHEN   "13"
               WHEN   "14"
                   MOVE    "01"        TO  JYURRK-SRYKBN1
               WHEN   "21"
                   MOVE    "01"        TO  JYURRK-SRYKBN2
               WHEN   "22"
                   MOVE    "01"        TO  JYURRK-SRYKBN3
               WHEN   "23"
                   MOVE    "01"        TO  JYURRK-SRYKBN4
               WHEN   "31"
               WHEN   "32"
               WHEN   "33"
                   MOVE    "01"        TO  JYURRK-SRYKBN5
               WHEN   "40"
                   MOVE    "01"        TO  JYURRK-SRYKBN6
               WHEN   "50"
                   MOVE    "01"        TO  JYURRK-SRYKBN7
               WHEN   "54"
                   MOVE    "01"        TO  JYURRK-SRYKBN8
               WHEN   "60"
                   MOVE    "01"        TO  JYURRK-SRYKBN9
               WHEN   "70"
                   MOVE    "01"        TO  JYURRK-SRYKBN10
               WHEN   "80"
                   MOVE    "01"        TO  JYURRK-SRYKBN11
      ****     WHEN   "93"
      *        WHEN   "95"
      *        WHEN   "96"
      *        WHEN   "99"
      ****         MOVE    "01"        TO  JYURRK-SRYKBN11
           END-EVALUATE
           .
       4901122-JYURRK-SRYKBNN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスタの更新処理
      *****************************************************************
       490113-SYUNOU-UPD-SEC           SECTION.
      *
      *    包括入力の時対象外
           IF      SPA-RRK-HKNCOMBI(IDX)   =   "9999"
               GO      TO      490113-SYUNOU-UPD-EXT
           END-IF
      *    伝票番号ゼロをエラーとする
           IF      SPA-RRK-DENPNUM(IDX)
                                       =   ZERO
               MOVE    "1006"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      490113-SYUNOU-UPD-EXT
           END-IF
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPID          TO  SYU-HOSPID
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-RRK-DENPNUM(IDX)
                                       TO  SYU-DENPNUM
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
               GO      TO      490113-SYUNOU-UPD-EXT
           END-IF
      *
           IF      FLG-SYUNOU          =   ZERO
      *********受診履歴が無い時、削除
      *        IF      FLG-JYURRK-ARI      =   ZERO
      *            MOVE    SYUNOU-REC          TO  MCPDATA-REC
      *            MOVE    "DBDELETE"          TO  MCP-FUNC
      *            MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
      *            CALL    "ORCMCPSUB"         USING
      *                                        MCPAREA
      *                                        ORCMCPAREA
      *                                        MCPDATA-REC
      *            IF      MCP-RC         NOT  =   ZERO
      *                MOVE    "2001"              TO  SPA-ERRCD
      *                DISPLAY "J02 SYUNOU DEL ERR:" MCP-RC
      *                       ",KEY:" SYU-KEY
      *            END-IF
      *******  ELSE
      *            会計変更区分
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
                   IF      MCP-RC         NOT  =   ZERO
                       MOVE    "2001"              TO  SPA-ERRCD
                       DISPLAY "J02 SYUNOU UPDATE:" MCP-RC
                              ",KEY:" SYU-KEY
                   END-IF
      *********END-IF
           END-IF
      *
           .
       490113-SYUNOU-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴  変更処理
      *****************************************************************
       4902-SANTEI-KOUSIN-SEC           SECTION.
      *
      *    包括入力の時対象外
           IF      ACCT-HKNCOMBI       =   "9999"
               GO      TO      4902-SANTEI-KOUSIN-EXT
           END-IF
      *
      *    自費の時対象外
           IF      ACCT-SRYKBN         =   "95"  OR  "96"
               GO      TO      4902-SANTEI-KOUSIN-EXT
           END-IF
      *
      *    診療行為マスター読み込み
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPID         TO  SRY-HOSPID
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *    点数検索用日付編集
           MOVE    ACCT-SRYYM          TO  WRK-SRYYMD(1:6)
           MOVE    01                  TO  WRK-SRYDD
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      ACCT-DAY (1 IDX-D)  NOT =   ZERO
                   MOVE    IDX-D           TO  WRK-SRYDD
                   MOVE    31              TO  IDX-D
               END-IF
           END-PERFORM
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
               PERFORM 930-SRYACT-RAED-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL       FLG-SRYACT      =   1
      *        剤明細編集
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL      (IDZ         >   5        )  OR
                                  (SRY-SRYCD (IDZ) =   SPACE)
      *            手技料の時、算定歴を判定する
                   IF     (SRY-SRYCD (IDZ)(1:1)    =   "1" )
      *                システム予約コード
                       OR (SRY-SRYCD (IDZ)(1:3)    =   "099" )
      *
                       MOVE    SRY-SRYCD (IDZ)     TO  TNS-SRYCD
                       MOVE    SRY-SRYCD (IDZ)     TO  WRK-SRYCD
                       PERFORM 910-TENSU-READ-SEC
      *                算定用の診療コードへ変更
                       PERFORM 45020-SANTEI-SRYCD-SEC
                       IF      SRY-SRYCD (IDZ)
                                               NOT =   WRK-SRYCD
                           MOVE    WRK-SRYCD           TO  TNS-SRYCD
                           PERFORM 910-TENSU-READ-SEC
                       END-IF
                       IF      TNS-SANTEIRRKKBN    NOT =   ZERO
                           PERFORM 49021-SANTEI-UPD-SEC
                       END-IF
                   END-IF
               END-PERFORM
      *
               MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
               PERFORM 930-SRYACT-RAED-SEC
           END-PERFORM
      *
      *    MOVE    "DBCLOSE"           TO  MCP-FUNC
      *    MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
           .
       4902-SANTEI-KOUSIN-EXT.
           EXIT.
      *****************************************************************
      *    算定用の診療コードへ変更処理
      *****************************************************************
       45020-SANTEI-SRYCD-SEC              SECTION.
      *
      *    包括逓減区分判定
           EVALUATE    TNS-HOUKATUTEIGENKBN
      *        頭部
               WHEN    201
                   MOVE    "099700101"         TO  WRK-SRYCD
      *        躯幹
               WHEN    202
                   MOVE    "099700102"         TO  WRK-SRYCD
      *        四肢
               WHEN    203
                   MOVE    "099700103"         TO  WRK-SRYCD
           END-EVALUATE
           .
       45020-SANTEI-SRYCD-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴更新処理
      *****************************************************************
       49021-SANTEI-UPD-SEC           SECTION.
      *
      *    今回分の登録
           INITIALIZE                  ORCSSANTEIAREA
           MOVE    "1"                 TO  SSANTEI-SYORI
           MOVE    "J02"               TO  SSANTEI-PG
      **** MOVE    SRY-SRYCD (IDZ)     TO  SSANTEI-SRYCD
           MOVE    WRK-SRYCD           TO  SSANTEI-SRYCD
           MOVE    ACCT-SRYYM          TO  SSANTEI-SRYYMD(1:6)
           MOVE    "01"                TO  SSANTEI-SRYYMD(7:2)
           MOVE    ACCT-PTID           TO  SSANTEI-PTID
           MOVE    ACCT-NYUGAIKBN      TO  SSANTEI-NYUGAIKBN
           MOVE    ACCT-SRYKA          TO  SSANTEI-SRYKA
           MOVE    ZERO                TO  SSANTEI-MSANTEID
           MOVE    ZERO                TO  SSANTEI-MSANTEICNT
      *    保険組合せ
           MOVE    ACCT-HKNCOMBI       TO  SPA-HKNCOMBINUM
      *!   保険内容編集
      *!   MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBI
      *!   PERFORM 510-HKNCOMBI-MEI-SEC
      *!   MOVE    SPA-NAI-HKNKBN      TO  SPA-HKNKBN
      *    保険内容編集
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL       IDH     >   SPA-HKN-MAX
               IF      SPA-GMN-THKNCOMBINUM (IDH)
                                           =   SPA-HKNCOMBINUM
      *            労災判定
                   MOVE    SPA-NAI-THKNKBN(IDH)    TO  SPA-HKNKBN
               END-IF
           END-PERFORM
      *
      *    今回分
           PERFORM VARYING IDX-D       FROM    1   BY  1
                   UNTIL   IDX-D       >   31
               IF      SPA-NAI-DAY (IDX 1 IDX-D)  >   ZERO
                   MOVE    SPA-NAI-DAY (IDX 1 IDX-D)
                                           TO  SSANTEI-DAY (IDX-D)
               END-IF
           END-PERFORM
      *    前回分
           PERFORM VARYING IDX-D       FROM    1   BY  1
                   UNTIL   IDX-D       >   31
               IF      ACCT-DAY (1 IDX-D)  >   ZERO
                   MOVE    ACCT-DAY (1 IDX-D)
                                           TO  SSANTEI-MAE-DAY (IDX-D)
               END-IF
           END-PERFORM
           CALL    "ORCSSANTEI"        USING
                                       ORCSSANTEIAREA
                                       SPA-AREA
      *
           .
       49021-SANTEI-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       510-HKNCOMBI-MEI-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBIMEI
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPID          TO  COMB-HOSPID
           MOVE    SPA-GMN-PTID        TO  COMB-PTID
           MOVE    WRK-HKNCOMBI        TO  COMB-HKNCOMBINUM
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
               PERFORM 940-HKNCOMBI-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
           IF      FLG-HKNCOMBI        =   ZERO
               PERFORM 5101-HKNCOMBI-SET-SEC
           END-IF
      *
      *    MOVE    "DBCLOSE"           TO  MCP-FUNC
      *    MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
      *
           .
       510-HKNCOMBI-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       5101-HKNCOMBI-SET-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBIMEI
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
           END-STRING
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
           END-IF
           MOVE    ZERO                TO  SPA-NAI-SISIKBN
      *    保険区分編集
           EVALUATE    COMB-HKNNUM
      *        労災
               WHEN    "971"
      *        自賠責
               WHEN    "973"
                   MOVE    1                   TO  SPA-NAI-HKNKBN
               WHEN    OTHER
      *        健保
                   MOVE    ZERO                TO  SPA-NAI-HKNKBN
           END-EVALUATE
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
      *
      *        四肢区分
               MOVE    PTRSI-SISIKBN       TO  SPA-NAI-SISIKBN
      *        労災区分
               MOVE    PTRSI-HKNKBN        TO  SPA-NAI-RSI-HKNKBN
      *        傷病年月日
               IF      PTRSI-HKNKBN        =   "1"  OR  "2"  OR  "4"
                   MOVE    PTRSI-SHOBYOYMD     TO  SPA-RSI-SHOBYOYMD
               ELSE
                   MOVE    PTRSI-SINSATUYMD    TO  SPA-RSI-SHOBYOYMD
               END-IF
      *        発生年月日＋３月
      *        発生日を求める
      *        救急医療管理加算のチェック
               MOVE    SPACE               TO  WRK-HASSYOYMD
               MOVE    "101110030"         TO  WRK-SRYCD
               PERFORM 51011-SANTEI-CHK-SEC
               IF      WRK-HASSYOYMD       =   SPACE
                  MOVE    SPA-RSI-SHOBYOYMD    TO  WRK-HASSYOYMD
               END-IF
      *        発生日から３ヶ月の計算
               COMPUTE WRK-HAS-MM          =   WRK-HAS-MM
                                           +   3
               IF      WRK-HAS-MM          >   12
                   COMPUTE WRK-HAS-MM      =   WRK-HAS-MM    -   12
                   COMPUTE WRK-HAS-YY      =   WRK-HAS-YY    +   1
               END-IF
               MOVE    WRK-HASSYOYMD       TO  SPA-RSI-HASSYOYMD3
           END-IF
      *
           .
       5101-HKNCOMBI-SET-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴チェック処理
      *****************************************************************
       51011-SANTEI-CHK-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-HASSYOYMD
      *
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU           =   1
               GO      TO      51011-SANTEI-CHK-EXT
           END-IF
      *    保険をキーにするかのチェック
           SET     TBL-SAN             TO  1
           SEARCH  TBL-SANTEI-OCC      VARYING   TBL-SAN
               AT  END
                   MOVE    ZERO            TO  FLG-HKNSAN
               WHEN    WRK-SRYCD       =   TBL-SANTEI-SRYCD
                                                          (TBL-SAN)
                   MOVE    1               TO  FLG-HKNSAN
           END-SEARCH
      *
      *    算定履歴検索
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPID          TO  SANTEI-HOSPID
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           EVALUATE    TNS-SANTEIRRKKBN
               WHEN    1
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    2
                   MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    3
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
               WHEN    4
                   MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                   MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
           END-EVALUATE
      *
           IF     (FLG-HKNSAN          =   1   )  AND
                  (SPA-NAI-HKNKBN  NOT =   ZERO)
               MOVE     WRK-HKNCOMBI       TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           END-IF
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SANTEI-KEY3"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SANTEI-KEY3"       TO  ORC-DBPATH
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           PERFORM
               UNTIL       FLG-SANTEI      =   1
      *
               IF      SANTEI-MSANTEICNT   >   ZERO
      *            初回算定日
                   MOVE    SANTEI-FSANTEIYMD   TO  WRK-HASSYOYMD
               END-IF
      *
               IF     FLG-SANTEI           =   ZERO
                  MOVE    "SANTEI-KEY3"       TO  ORC-DBPATH
                  PERFORM 920-SANTEI-READ-SEC
               END-IF
           END-PERFORM
      *
           .
       51011-SANTEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険一括変更処理
      *****************************************************************
       4100-HKNHEN-SEC            SECTION.
      *
           IF      SPA-GMN-BANGO   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      4100-HKNHEN-EXT
           END-IF
      *
           IF      SPA-J01-SRYKA       =   ZERO
               MOVE    "0022"          TO  SPA-ERRCD
               GO      TO      4100-HKNHEN-EXT
           END-IF
      *
           MOVE    "0109"              TO  SPA-JIDCD
      *
           .
       4100-HKNHEN-EXT.
           EXIT.
      *****************************************************************
      *    保険一括変更処理
      *****************************************************************
       41001-HKNHEN-SYORI-SEC            SECTION.
      *
      *    現在表示されている内容を更新する
      *    診療会計マスター更新
           PERFORM 4901-SRYACCT-UPD-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO       41001-HKNHEN-SYORI-EXT
           END-IF
      *
           INITIALIZE                  SPA-J021-AREA
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-J02             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGJ021"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-J02
           MOVE    SPA-WORK        TO  SPA-J01KYOUTU
      *
      *    画面カーソルセット処理
           MOVE    "MAE_HKNCOMBI"      TO  MCP-WIDGET
      *
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
      *
           MOVE    "J021"              TO  SPA-SAKIPG
           MOVE    "J02"               TO  SPA-MOTOPG
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "J021"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
      *
       41001-HKNHEN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    点滴一括変更処理
      *****************************************************************
       42001-TENTEKI-SYORI-SEC            SECTION.
      *
      *    点滴エラー後、変更のない時、エラーとしない
           IF      SPA-J02-TENTEKI     =   1
               GO      TO      42001-TENTEKI-SYORI-EXT
           END-IF
      *
      *    入院の時、点滴液量の再計算
           INITIALIZE                  ORCSTENTEKAREA
           MOVE    "1"                 TO  ORCSTEN-KBN
           MOVE    SPA-GMN-PTID        TO  ORCSTEN-PTID
           MOVE    SPA-NAI-SRYYM       TO  ORCSTEN-SRYYM
           CALL    "ORCSTENTEK"        USING
                                       SPA-AREA
                                       ORCSTENTEKAREA
      *
           IF      ORCSTEN-ERRCD   NOT =   ZERO
               MOVE    "2000"              TO  SPA-ERRCD
               GO      TO      42001-TENTEKI-SYORI-EXT
           END-IF
           MOVE    SPACE               TO  SPA-GMN-NYUIN-AREA
           MOVE    ZERO                TO  IDX-D
      *    点滴手技料重複エラー編集
           IF      ORCSTEN-FUKUDAY-AREA    NOT =   ZERO
               PERFORM 420011-FUKUERR-SYORI-SEC
           END-IF
      *    中心静脈エラー編集
           IF     (IDX-D               <   2         )  AND
                  (ORCSTEN-MIIVHDAY-AREA   NOT =   ZERO  )
               PERFORM 420013-MIIVHDAY-HEN-SEC
           END-IF
      *    点滴手技料エラー編集
           IF     (IDX-D               <   2         )  AND
                  (ORCSTEN-MIDAY-AREA  NOT =   ZERO  )
               PERFORM 420012-MIDAY-HEN-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      ORCSTEN-HENFLG      =   1
                    MOVE    "0053"              TO  SPA-ERRCD
               END-IF
      *        点滴エラーあり
           ELSE
               MOVE    1                   TO  SPA-J02-TENTEKI
           END-IF
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (ORCSTEN-HENFLG      =   ZERO  )
               GO      TO    42001-TENTEKI-SYORI-EXT
           END-IF
      *    変更があった時、点滴のみ再表示する
      *    全科
           MOVE    SPA-GMN-SRYKA-LIST(SPA-SRYKA-MAX)
                                       TO  SPA-GMN-SRYKA-G
           MOVE    SPA-GMN-SRYKA       TO  SPA-J01-SRYKA
      *    点滴
           MOVE    SPA-GMN-SRYKBN-LST(04)
                                       TO  SPA-GMN-SRYKBN-G
           MOVE    SPA-GMN-SRYKAMEI    TO  SPA-J01-SRYKAMEI
      *
      *    受診履歴編集
           PERFORM 3103-JYURRK-HEN-SEC
      *
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
      *    保存用パラメタ作成
           PERFORM 4201-SRYACCT-PARA-SEC
      *
      *    診療会計マスターより編集をする
           PERFORM 410181-SRYACCT-HEN-SEC
      *
           MOVE    1                   TO  SPA-GMN-CUR
           .
       42001-TENTEKI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    点滴手技料重複エラー編集処理
      *****************************************************************
       420011-FUKUERR-SYORI-SEC          SECTION.
      *
      *    点滴手技
           IF      ORCSTEN-FUKU-AREA     NOT =   ZERO
               MOVE    ORCSTEN-FUKU-AREA   TO  WRK-ERR-TBL
               PERFORM 4200111-NYUIN-ERR-HEN-SEC
               ADD     1                   TO  IDX-D
               MOVE    "点滴同時算定："    TO  SPA-GMN-NYUINMSG(IDX-D)
               MOVE    WRK-NYUIN-ERR(1:36) TO  SPA-GMN-NYUINMSG(IDX-D)
                                                               (15:)
               IF      WRK-NYUIN-ERR(37:)  NOT =   SPACE
                   ADD     1                   TO  IDX-D
                   MOVE    WRK-NYUIN-ERR(37:)  TO  SPA-GMN-NYUINMSG
                                                              (IDX-D)
               END-IF
               MOVE    "0051"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
      *    動脈
           IF      IDX-D               =   2
               GO      TO      420011-FUKUERR-SYORI-EXT
           END-IF
           IF      ORCSTEN-DOU-AREA    NOT =   ZERO
               MOVE    ORCSTEN-DOU-AREA    TO  WRK-ERR-TBL
               PERFORM 4200111-NYUIN-ERR-HEN-SEC
               ADD     1                   TO  IDX-D
               MOVE    "動脈同時算定："    TO  SPA-GMN-NYUINMSG (IDX-D)
               MOVE    WRK-NYUIN-ERR(1:36) TO  SPA-GMN-NYUINMSG (IDX-D)
                                                               (15:)
               IF     (WRK-NYUIN-ERR(37:)  NOT =   SPACE ) AND
                      (IDX-D                   <   2     )
                   ADD    1                    TO  IDX-D
                   MOVE    WRK-NYUIN-ERR(37:)  TO  SPA-GMN-NYUINMSG
                                                              (IDX-D)
               END-IF
               MOVE    "0054"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
      *    中心静脈
           IF      IDX-D               =   2
               GO      TO      420011-FUKUERR-SYORI-EXT
           END-IF
           IF      ORCSTEN-CHU-AREA    NOT =   ZERO
               MOVE    ORCSTEN-CHU-AREA    TO  WRK-ERR-TBL
               PERFORM 4200111-NYUIN-ERR-HEN-SEC
               ADD     1                   TO  IDX-D
               MOVE    "中静同時算定："    TO  SPA-GMN-NYUINMSG(IDX-D)
               MOVE    WRK-NYUIN-ERR(1:36) TO  SPA-GMN-NYUINMSG(IDX-D)
                                                           (15:)
               IF     (WRK-NYUIN-ERR(37:)  NOT =   SPACE ) AND
                      (IDX-D                   <   2     )
                   ADD    1                    TO  IDX-D
                   MOVE    WRK-NYUIN-ERR(37:)  TO  SPA-GMN-NYUINMSG
                                                              (IDX-D)
               END-IF
               MOVE    "0055"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       420011-FUKUERR-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    中心静脈加算算定エラー編集処理
      *****************************************************************
       420013-MIIVHDAY-HEN-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-NYUIN-ERR
           MOVE    1                   TO  IDX-N
           PERFORM VARYING     IDX-N2  FROM    1   BY  1
                   UNTIL       IDX-N2  >   31
               IF      ORCSTEN-MIIVH-DAY (IDX-N2) =   1
                   IF      IDX-N       >   1
                       MOVE    ","         TO  WRK-NYUIN-ERR
                                                            (IDX-N:1)
                       ADD     1           TO  IDX-N
                   END-IF
                   MOVE    IDX-N2          TO  WRK-NYUIN-ERR
                                                           (IDX-N:2)
                   ADD     2               TO  IDX-N
               END-IF
           END-PERFORM
           IF      WRK-NYUIN-ERR       =   SPACE
               GO      TO     420013-MIIVHDAY-HEN-EXT 
           END-IF
           ADD     1                   TO  IDX-D
           MOVE    "中静未算定："      TO  SPA-GMN-NYUINMSG(IDX-D)
           MOVE    WRK-NYUIN-ERR(1:38) TO  SPA-GMN-NYUINMSG(IDX-D)(13:)
           IF     (WRK-NYUIN-ERR(39:)  NOT =   SPACE )  AND
                  (IDX-D               <   2         )
               ADD     1                   TO  IDX-D
               MOVE    WRK-NYUIN-ERR(39:)  TO  SPA-GMN-NYUINMSG(IDX-D)
           END-IF
           MOVE    "0056"              TO  SPA-ERRCD
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       420013-MIIVHDAY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    加算算定エラー編集処理
      *****************************************************************
       420012-MIDAY-HEN-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-NYUIN-ERR
           MOVE    1                   TO  IDX-N
           PERFORM VARYING     IDX-N2  FROM    1   BY  1
                   UNTIL       IDX-N2  >   31
               IF      ORCSTEN-MI-DAY (IDX-N2) =   1
                   IF      IDX-N       >   1
                       MOVE    ","         TO  WRK-NYUIN-ERR (IDX-N:1)
                       ADD     1           TO  IDX-N
                   END-IF
                   MOVE    IDX-N2          TO  WRK-NYUIN-ERR (IDX-N:2)
                   ADD     2               TO  IDX-N
               END-IF
           END-PERFORM
           IF      WRK-NYUIN-ERR       =   SPACE
               PERFORM 420011-KASAN-ERR-SEC
           ELSE
               ADD     1                   TO  IDX-D
               MOVE    "点滴未算定："      TO  SPA-GMN-NYUINMSG(IDX-D)
               MOVE    WRK-NYUIN-ERR(1:38) TO  SPA-GMN-NYUINMSG(IDX-D)
                                                                   (13:)
               IF      (WRK-NYUIN-ERR(39:) NOT =   SPACE)  AND
                       (IDX-D              <   2   )
                   ADD     1                   TO  IDX-D
                   MOVE    WRK-NYUIN-ERR(39:)  TO  SPA-GMN-NYUINMSG
                                                                 (IDX-D)
               END-IF
               MOVE    "0052"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       420012-MIDAY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    加算算定エラー編集処理
      *****************************************************************
       4200111-NYUIN-ERR-HEN-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-NYUIN-ERR
           MOVE    1                   TO  IDX-N
           PERFORM VARYING     IDX-N2  FROM    1   BY  1
                   UNTIL       IDX-N2  >   31
               IF      WRK-ERR-DAY (IDX-N2)    =   1
                   IF      IDX-N       >   1
                       MOVE    ","         TO  WRK-NYUIN-ERR(IDX-N:1)
                       ADD     1           TO  IDX-N
                   END-IF
                   MOVE    IDX-N2          TO  WRK-NYUIN-ERR
                                                            (IDX-N:2)
                   ADD     2               TO  IDX-N
               END-IF
           END-PERFORM
           .
       4200111-NYUIN-ERR-HEN-EXT.
           EXIT.
      *****************************************************************
      *    加算算定エラー編集処理
      *****************************************************************
       420011-KASAN-ERR-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-NYUIN-ERR
           MOVE    1                   TO  IDX-N
           PERFORM VARYING     IDX-N2  FROM    1   BY  1
                   UNTIL       IDX-N2  >   31
               IF      ORCSTEN-MI-DAY (IDX-N2) =   3
                   IF      IDX-N       >   1
                       MOVE    ","         TO  WRK-NYUIN-ERR(IDX-N:1)
                       ADD     1           TO  IDX-N
                   END-IF
                   MOVE    IDX-N2          TO  WRK-NYUIN-ERR
                                                            (IDX-N:2)
                   ADD     2               TO  IDX-N
               END-IF
           END-PERFORM
      *
           IF      WRK-NYUIN-ERR   NOT=   SPACE
               ADD     1                   TO  IDX-D
               MOVE    "加算算定日："      TO  SPA-GMN-NYUINMSG(IDX-D)
               MOVE    WRK-NYUIN-ERR(1:38) TO  SPA-GMN-NYUINMSG(IDX-D)
                                                               (13:)
               IF     (WRK-NYUIN-ERR(39:)  NOT =   SPACE )  AND
                      (IDX-D               <   2         )
                   ADD     1                   TO  IDX-D
                   MOVE    WRK-NYUIN-ERR(39:)  TO  SPA-GMN-NYUINMSG
                                                              (IDX-D)
               END-IF
               MOVE    "0058"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
           IF      IDX-D               >=  2
               GO      TO      420011-KASAN-ERR-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-NYUIN-ERR
           MOVE    1                   TO  IDX-N
           PERFORM VARYING     IDX-N2  FROM    1   BY  1
                   UNTIL       IDX-N2  >   31
               IF      ORCSTEN-MI-DAY (IDX-N2) =   2
                   IF      IDX-N       >   1
                       MOVE    ","         TO  WRK-NYUIN-ERR(IDX-N:1)
                       ADD     1           TO  IDX-N
                   END-IF
                   MOVE    IDX-N2          TO  WRK-NYUIN-ERR
                                                            (IDX-N:2)
                   ADD     2               TO  IDX-N
               END-IF
           END-PERFORM
           IF      WRK-NYUIN-ERR       =   SPACE
               GO      TO      420011-KASAN-ERR-EXT
           END-IF
      *
           ADD     1                   TO  IDX-D
           MOVE    "加算未算定："      TO  SPA-GMN-NYUINMSG(IDX-D)
           MOVE    WRK-NYUIN-ERR(1:38) TO  SPA-GMN-NYUINMSG(IDX-D)(13:)
           IF     (WRK-NYUIN-ERR(39:)  NOT =   SPACE )  AND
                  (IDX-D               <   2         )
               ADD     1                   TO  IDX-D
               MOVE    WRK-NYUIN-ERR(39:)  TO  SPA-GMN-NYUINMSG(IDX-D)
           END-IF
           MOVE    "0057"              TO  SPA-ERRCD
           MOVE    1                   TO  SPA-GMN-CUR
           .
       420011-KASAN-ERR-EXT.
           EXIT.
      *****************************************************************
      *    入院調剤料変更処理
      *****************************************************************
       4110-CHOZAI-SEC            SECTION.
      *
      *    点滴エラー後、変更のない時、エラーとしない
           IF      SPA-GMN-NYUGAIKBN   =   "2"
               GO      TO      4110-CHOZAI-EXT
           END-IF
      *
      *    入院の時、点滴液量の再計算
           INITIALIZE                  ORCSCHOZAIAREA
           MOVE    "1"                 TO  ORCSCHO-KBN
           MOVE    SPA-GMN-PTID        TO  ORCSCHO-PTID
           MOVE    SPA-NAI-SRYYM       TO  ORCSCHO-SRYYM
           CALL    "ORCSCHOZAI"        USING
                                       SPA-AREA
                                       ORCSCHOZAIAREA
      *
           IF      ORCSCHO-ERRCD   NOT =   ZERO
               MOVE    "3333"              TO  SPA-ERRCD
               GO      TO      4110-CHOZAI-EXT
           END-IF
      *
      *    受診履歴編集
           PERFORM 3103-JYURRK-HEN-SEC
      *
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
      *    保存用パラメタ作成
           PERFORM 4201-SRYACCT-PARA-SEC
      *
      *    診療会計マスターより編集をする
           PERFORM 410181-SRYACCT-HEN-SEC
      *
           MOVE    1                   TO  SPA-GMN-CUR
           .
       4110-CHOZAI-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       4120-PRINT-SEC            SECTION.
      *
      *    番号選択中でないこと
           IF      SPA-GMN-BANGO   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      4120-PRINT-EXT
           END-IF
      *
           IF      SPA-UPDFLG          =   "1"
               MOVE    "0116"              TO  SPA-JIDCD
           ELSE
               MOVE    "0117"              TO  SPA-JIDCD
           END-IF
      *
           .
       4120-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    カルテ３号印刷処理
      *****************************************************************
       41201-PRINT-SYORI-SEC            SECTION.
      *
           IF      SPA-UPDFLG          =   "1"
      *        診療会計マスター更新
               PERFORM 4901-SRYACCT-UPD-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO       41201-PRINT-SYORI-EXT
               END-IF
           END-IF
      *
      *    カルテ３号紙
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000005"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-NAI-SRYYM       TO  ORCSPRTNM-SRYYMD(1:6)
           MOVE    "01"                TO  ORCSPRTNM-SRYYMD(7:2)
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYSKANRI-REC
           IF      ORCSPRTNM-RC        =   ZERO
               MOVE    ORCSPRTNM-PRTPG     TO  WRK-PRINT-PG
           ELSE
               MOVE    "ORCHC33"           TO  WRK-PRINT-PG
           END-IF
      *
           INITIALIZE                  ORCHC33AREA
           MOVE    "1"                 TO  ORCHC33-KBN
           MOVE    SPA-GMN-PTNUM       TO  ORCHC33-PTNUM
           MOVE    SPA-GMN-PTID        TO  ORCHC33-PTID
           MOVE    SPA-GMN-NAME        TO  ORCHC33-NAME
           MOVE    SPA-GMN-BIRTHDAY    TO  ORCHC33-BIRTHDAY
           MOVE    SPA-GMN-SEX         TO  ORCHC33-SEX
           MOVE    SPA-NAI-SRYYM       TO  ORCHC33-SRYYM
           MOVE    SPACE               TO  ORCHC33-SRYYM-END
           MOVE    SPA-GMN-NYUGAIKBN   TO  ORCHC33-NYUGAIKBN
           MOVE    SPA-J01-SRYKA       TO  ORCHC33-SRYKA
           CALL    WRK-PRINT-PG        USING
                                       SPA-AREA
                                       ORCHC33AREA
      *
           .
       41201-PRINT-SYORI-EXT.
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
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "J02    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
           MOVE    SPACE               TO  WRK-ERRMSG
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
                   MOVE    "対象の診療科で受診履歴がない日です。"
                                       TO  WRK-ERRMSG
                   MOVE    "入力できません"
                                       TO  WRK-ERRMSG(37:)
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
      *----(01.00.01) LINE ADD START ----------------------------------
               WHEN    "0009"
                   MOVE
                   "剤が修正中です。変更確定かクリアをして下さい。"
                                       TO  WRK-ERRMSG
      *----(01.00.01) LINE ADD END   ----------------------------------
               WHEN    "0010"
                   MOVE    "診療科履歴更新エラー" 
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
                   MOVE    "対象の診療年月に受診はありません。"
                                       TO  WRK-ERRMSG
               WHEN    "0021"
                   MOVE    "対象の剤は労災の外来管理加算読み替え分です"
                                       TO  WRK-ERRMSG
                   MOVE    "。剤変更はできません。"
                                       TO  WRK-ERRMSG(43:)
               WHEN    "0022"
                   MOVE    "保険の一括変更は科毎に行って下さい。"
                                       TO  WRK-ERRMSG
                   MOVE    "全科では変更できません。"
                                       TO  WRK-ERRMSG(37:)
               WHEN    "0023"
                   MOVE    "当月の受診履歴が１００以上になります。"
                                       TO  WRK-ERRMSG
                   MOVE    "全体の受診履歴表示はできません。"
                                       TO  WRK-ERRMSG(39:)
               WHEN    "0024"
                   MOVE    "当月の受診履歴が１００以上になります。"
                                       TO  WRK-ERRMSG
                   MOVE    "追加はできません。"
                                       TO  WRK-ERRMSG(39:)
               WHEN    "0030"
                   MOVE    "入外区分がありません。"
                                       TO  WRK-ERRMSG
               WHEN    "0031"
                   MOVE    "診療種別がありません。"
                                       TO  WRK-ERRMSG
               WHEN    "0032"
                   MOVE    "保険組合せがありません。"
                                       TO  WRK-ERRMSG
               WHEN    "0033"
                   MOVE    "診療種別がありません。"
                                       TO  WRK-ERRMSG
               WHEN    "0040"
                   MOVE    "算定できる点滴手技料がありません。"
                                       TO  WRK-ERRMSG
                   MOVE    "訂正で点滴を修正して下さい。"
                                       TO  WRK-ERRMSG(35:)
               WHEN    "0050"
                   MOVE    "点滴一括処理は入院の時のみ選択できます。"
                                       TO  WRK-ERRMSG
               WHEN    "0051"
                   MOVE    "点滴手技料が同時算定されています。"
                                       TO  WRK-ERRMSG
               WHEN    "0052"
                   MOVE    "点滴手技料が算定できる日があります。"
                                       TO  WRK-ERRMSG
               WHEN    "0053"
                   MOVE    "点滴手技料を削除しました。"
                                       TO  WRK-ERRMSG
                   MOVE    "確認して下さい"
                                       TO  WRK-ERRMSG(27:)
               WHEN    "0054"
                   MOVE    "動脈注射手技料が同時算定されています。"
                                       TO  WRK-ERRMSG
               WHEN    "0055"
                   MOVE    "中心静脈注射料が同時算定されています。"
                                       TO  WRK-ERRMSG
               WHEN    "0056"
                   MOVE    "中心静脈注射料が算定できる日があります。"
                                       TO  WRK-ERRMSG
               WHEN    "0057"
                   MOVE
                   "麻毒・生物学的製剤加算が算定できる日があります。"
                                       TO  WRK-ERRMSG
               WHEN    "0058"
                   MOVE    "麻毒・生物学的製剤加算のみ算定している日が"
                                       TO  WRK-ERRMSG
                   MOVE    "あります。確認して下さい。"
                                       TO  WRK-ERRMSG(43:)
               WHEN    "2000"
                   MOVE    "更新エラーです。"
                                       TO  WRK-ERRMSG
               WHEN    "2001"
                   MOVE    "受診履歴の更新エラーです。"
                                       TO  WRK-ERRMSG
               WHEN    "2002"
                   MOVE    "受診履歴の新規エラーです。"
                                       TO  WRK-ERRMSG
               WHEN    "1005"
                   MOVE    "受診履歴番号が存在しません"
                                       TO  WRK-ERRMSG
               WHEN    "1006"
                   MOVE    "受診履歴の伝票番号がゼロのものがあります。"
                                       TO  WRK-ERRMSG
                   MOVE    "変更できません。"
                                       TO  WRK-ERRMSG(43:)
      *
               WHEN    "K001"
                   MOVE    "受診日変更により当日の受診回数が変更"
                                       TO  WRK-ERRMSG
                   MOVE    "されました。診察料を確認して下さい。"
                                       TO  WRK-ERRMSG(37:)
      *
               WHEN    "9999"
                   MOVE    "他端末で使用中です。"
                                               TO  WRK-ERRMSG
                   MOVE    SLOCK-MSG           TO  WRK-ERRMSG(21:)
      *
               WHEN    OTHER
                   MOVE    SPA-ERRCD
                                       TO  WRK-ERRMSG
      *
           END-EVALUATE
      *
           MOVE    SPACE               TO  JERR
           MOVE    SPA-ERRCD           TO  JERR-ERRCODE
           MOVE    WRK-ERRMSG          TO  JERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "J02"               TO  SPA-MOTOPG
           MOVE    "JERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "JERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
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
               MOVE
               "剤が選択されています。ＯＫ　で選択内容はクリアされます"
                                       TO  WRK-JIDMSG
      *----(01.00.01) LINE ADD START ----------------------------------
               WHEN    "0102"
               MOVE
           "剤が修正されています。ＯＫで修正分は反映されずに終了します"
                                       TO  WRK-JIDMSG
      *----(01.00.01) LINE ADD END   ----------------------------------
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
                   MOVE    "診療科が変更されました。現在までの修正分"
                                       TO  WRK-JIDMSG1
                   MOVE    "を登録します。"
                                       TO  WRK-JIDMSG2
               WHEN    "0108"
                   MOVE    "診療年月が変更されました。現在までの修正"
                                       TO  WRK-JIDMSG1
                   MOVE    "分を登録します。"
                                       TO  WRK-JIDMSG2
               WHEN    "0109"
                   MOVE    "保険一括変更が選択されました。"
                                       TO  WRK-JIDMSG
                   MOVE    "現在までの修正分を登録します。"
                                       TO  WRK-JIDMSG(31:)
               WHEN    "0112"
                   MOVE    "点滴一括変更が選択されました。"
                                       TO  WRK-JIDMSG
                   MOVE    "現在までの修正分を登録します。"
                                       TO  WRK-JIDMSG(31:)
               WHEN    "0113"
                   MOVE    "現在までの修正分を登録後、"
                                       TO  WRK-JIDMSG
                   MOVE    "前頁を表示します。"
                                       TO  WRK-JIDMSG(27:)
               WHEN    "0114"
                   MOVE    "現在までの修正分を登録後、"
                                       TO  WRK-JIDMSG
                   MOVE    "次頁を表示します。"
                                       TO  WRK-JIDMSG(27:)
               WHEN    "0115"
                   MOVE    "診療日を変更します。現在までの修正分の"
                                       TO  WRK-JIDMSG
                   MOVE    "登録もしますので取消はできません。"
                                       TO  WRK-JIDMSG(39:)
               WHEN    "0116"
                   MOVE    "現在までの修正分を登録後、"
                                       TO  WRK-JIDMSG
                   MOVE    "カルテ３号紙を印刷します。"
                                       TO  WRK-JIDMSG(27:)
               WHEN    "0117"
                   MOVE    "カルテ３号紙を印刷します。"
                                       TO  WRK-JIDMSG
      *
               WHEN    OTHER
                   MOVE    SPA-JIDCD
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
           MOVE    "J02"               TO  SPA-MOTOPG
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
           MOVE    WRK-SRYYMD          TO  ORC-DBYMD
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
      *    MOVE    "DBCLOSE"           TO  MCP-FUNC
      *    MOVE    "TENSU-KEY"         TO  ORC-DBPATH
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
      *
           .
       910-TENSU-READ-EXT.
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
      *    MOVE    "DBCLOSE"           TO  MCP-FUNC
      *    MOVE    "PTINF-KEY"         TO  ORC-DBPATH
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター次読込
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
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
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       930-SRYACT-RAED-SEC         SECTION.
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
       930-SRYACT-RAED-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       940-HKNCOMBI-NEXT-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               MOVE    ZERO                TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                      HKNCOMBI-REC
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
      *
           .
       940-HKNCOMBI-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントマスタ読み込み
      *****************************************************************
       950-PTCOM-READ-SEC              SECTION.
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
      *    MOVE    "DBCLOSE"           TO  MCP-FUNC
      *    MOVE    "PTCOM-KEY"         TO  ORC-DBPATH
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
      *
           .
       950-KNJCOM-READ-EXT.
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
      *********INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-READ-EXT.
           EXIT.
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
      *    MOVE    "DBCLOSE"           TO  MCP-FUNC
      *    MOVE    "INPUTCD-KEY"       TO  ORC-DBPATH
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
      *
           .
       930-INPUTCD-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報読み込み
      *****************************************************************
       993-PTRSIINF-NEXT-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               MOVE    ZERO            TO  FLG-PTRSIINF
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
      *
           .
       993-PTRSIINF-NEXT-EXT.
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
      *    ファイル削除処理
      *****************************************************************
       999-FILE-DEL-SEC         SECTION.
      *
           MOVE    SPA-TERMID          TO  FILE-TERMID
           MOVE    ZERO                TO  IF-RESULT
           MOVE    FILE-NAME1          TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
           MOVE    1                   TO  FLG-END2
      *
      *    パラメタ削除
           INITIALIZE                      PARA-REC
           MOVE    "J02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "PARA-DEL2"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       999-FILE-DEL-EXT.
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
