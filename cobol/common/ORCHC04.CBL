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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCHC04.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 診療費明細書　印刷（ＨＣＭ０４）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/04/07    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  06/07/07  領収書変更対応
      *  03.04.01    NACL-森脇    07/01/17  医療機関名称、住所セット変更
      *  03.04.02    NACL-多々納  07/02/28  包括分対象外追加
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  03.05.00    NACL-多々納  07/05/22  再印刷処理追加
      *  04.01.00    NACL-多々納  07/10/23  公務災害対応
      *  04.01.00    NACL-多々納  07/11/22  器材商品名コード対応
      *  04.02.00    NACL-多々納  08/03/06  Ｈ２０年４月改正対応
      *  04.03.00    NACL-太田    08/09/17  改正対応
      *  04.05.00    NACL-多々納  09/06/XX  数量小数５桁対応
      *  04.05.00    NACL-多々納  10/03/XX  平成２２年４月改正対応
      *  04.05.00    NACL-太田    10/03/11  平成２２年４月改正対応
      *  04.05.00    NACL-多々納  10/05/12  Ａ５追加、複数まとめ対応他
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
      *
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
           SELECT  PRT-FILE        ASSIGN  HC01PARA
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
       01  PRT-REC.
           03  PRT-DATA                PIC X(50000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    診療費明細書（A4)
           COPY    "HCM04.INC".
      *
      *    診療費明細書（A5)
           COPY    "HCM04A5.INC".
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SRYACCTPLUS     PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
      *
           03  FLG-SP              PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-PLUSKBN         PIC 9(01).
      *
           03  FLG-NEXT            PIC 9(01).
           03  FLG-MEISAI          PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
           03  FLG-LINE            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDXA                PIC 9(04).
           03  IDX2                PIC 9(04).
      *
           03  IDX-JYU             PIC 9(04).
           03  IDX-REN             PIC 9(04).
           03  IDX-ACT             PIC 9(02).
           03  IDX-S               PIC 9(04).
           03  IDX-H               PIC 9(04).
           03  IDX-TBL             PIC 9(04).
           03  IDX-TBL2            PIC 9(04).
           03  IDX-HZN             PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDXM                PIC 9(04).
           03  IDXD                PIC 9(04).
      *
           03  IDX-SYU             PIC 9(04).
           03  IDX-SYU2            PIC 9(04).
           03  IDX-SYUMAX          PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WYY             PIC X(03).
               05  WRK-WMM             PIC 9(02).
               05  WRK-WDD             PIC 9(02).
           03  WRK-HENYMDG             PIC X(22).
      *
           03  WRK-KEY-AREA.
               05  WRK-KEY-SRYKA           PIC X(02).
               05  WRK-KEY-HKNCOMBINUM     PIC X(04).
               05  WRK-KEY-DENPNUM         PIC 9(07).
               05  WRK-KEY-PCNT            PIC 9(07).
           03  WRK-HKNCOMBINUM             PIC X(04).
           03  WRK-SYU-SRYKA               PIC X(02).
           03  WRK-SYU-HKNCOMBINUM         PIC X(04).
           03  WRK-SYU-SRYYMD              PIC X(08).
      *    見出し編集
           03  WRK-STYMD               PIC X(22).
           03  WRK-EDYMD.
               05  WRK-EDYY            PIC X(10).
               05  WRK-EDMM            PIC X(06).
               05  WRK-EDDD            PIC X(06).
           03  WRK-H-SRYYMD            PIC X(50).
           03  WRK-H-HAKYMD            PIC X(22).
           03  WRK-FUTAN               PIC X(03).
           03  WRK-FUTAN-MEI           PIC X(02).
           03  WRK-SRYKA-MEI           PIC X(30).
           03  WRK-HKNCOMBIMEI         PIC X(40).
           03  WRK-LBLSRYYMD           PIC X(10).
      *
           03  WRK-H-DENPNUM           PIC 9(07).
      *
      *
           03  WRK-SRYKA               PIC X(02).
           03  WRK-PAGE                PIC 9(03).
           03  WRK-PAGE-CNT            PIC 9(03).
           03  WRK-PAGE-MAX            PIC 9(03).
      *
           03  WRK-KAISU               PIC 9(03).
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-SRYSYUKBN           PIC X(03).
           03  WRK-SRYMEI              PIC X(10).
           03  WRK-SRYMEI-OLD          PIC X(10).
      *
           03  WRK-Z119                PIC Z,ZZZ,ZZ9.
           03  WRK-Z119-2              PIC -,---,--9.
           03  WRK-ZZ9                 PIC ZZ9.
           03  WRK-NO                  PIC Z(7).
           03  WRK-WARI                PIC 9(02)V9.
           03  WRK-Z6                  PIC ZZ9.
           03  WRK-Z62                 PIC 9.9.
      *
           03  WRK-DENPNUM             PIC 9(07).
           03  WRK-GRP-DENPNUM         PIC 9(07).
      *
           03  WRK-SURYO           PIC 9(05)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(05).
      *
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1    PIC ZZZZ9.
               05  WRK-SURYO-Z2    PIC X(01).
               05  WRK-SURYO-Z3    PIC ZZZZZ.
           03  WRK-SURYO-X-R       REDEFINES   WRK-SURYO-X.
               05  WRK-SURYO-ZF    PIC X(06).
               05  WRK-SURYO-Z4    PIC ZZZZ9.
      *    診療日編集
           03  WRK-SRYMEI-H            PIC X(80).
           03  WRK-DAY-HEN.
               05  WRK-DAY-H1          PIC X(02).
               05  WRK-DAY-H2          PIC X(06).
               05  WRK-DAY-H3          PIC X(02).
      *
      *    医療機関情報名称
           03  WRK-HOSPNAME            PIC X(100).
           03  WRK-HOSPNAME-R          REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1       PIC X(50).
               05  WRK-HOSPNAME2       PIC X(50).
           03  WRK-HOSPADRS            PIC X(100).
           03  WRK-HOSPADRS-R          REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1       PIC X(50).
               05  WRK-HOSPADRS2       PIC X(50).
           03  WRK-HOSPTEL         PIC X(15).
      *    名称＋数量編集
           03  WRK-A5-NAME             PIC X(148).
           03  WRK-A5-NAME-R           REDEFINES   WRK-A5-NAME.
               05  WRK-A5-NAME-1       PIC X(74).
               05  WRK-A5-NAME-2       PIC X(74).
      *
           03  WRK-H-NAME.
               05  WRK-H-NAME1             PIC X(74).
               05  WRK-H-NAME2             PIC X(74).
      *    03  WRK-H-NAME-R             REDEFINES  WRK-H-NAME.
      *        05  WRK-H-NAME2-1           PIC X(68).
      *        05  WRK-H-NAME2-2           PIC X(32).
      *
      *    帳票テーブル（Ａ４サイズ）
       01  TBL-MEISAI-AREA.
           03  TBL-MEISA-PAGE          OCCURS   10.
               05  TBL-MEISA-REC           OCCURS   70.
                   07  TBL-H-SRYKBN          PIC X(10).
                   07  TBL-H-SRYMEI.
                       09  TBL-H-ZKBN            PIC X(02).
                       09  TBL-H-NAME            PIC X(74).
                   07  TBL-H-TANINAME        PIC X(10).
                   07  TBL-H-SURYO           PIC X(11).
                   07  TBL-H-KAISU           PIC X(03).
                   07  TBL-H-TENSU.
                       09  TBL-H-TENSU1          PIC X(02).
                       09  TBL-H-TENSU2          PIC X(07).
                   07  TBL-H-KINGAKU.
                       09  TBL-H-KINGAKU1        PIC X(02).
                       09  TBL-H-KINGAKU2        PIC X(09).
                   07  TBL-H-EN              PIC X(01).
      *帳票明細数
       01  LINE-MAX                    PIC 9(03)   VALUE   70.
       01  PAGE-MAX                    PIC 9(03)   VALUE   10.
      *
      *    帳票テーブル（Ａ５サイズ）
       01  TBL2-MEISAI-A5-AREA.
           03  TBL2-MEISA-A5-PAGE       OCCURS   25.
               05  TBL2-MEISA-A5-REC        OCCURS   31.
                   07  TBL2-H-SRYKBN          PIC X(10).
                   07  TBL2-H-SRYMEI.
                       09  TBL2-H-ZKBN            PIC X(02).
                       09  TBL2-H-NAME            PIC X(74).
                   07  TBL2-H-TANINAME        PIC X(10).
                   07  TBL2-H-SURYO           PIC X(11).
                   07  TBL2-H-KAISU           PIC X(03).
                   07  TBL2-H-TENSU.
                       09  TBL2-H-TENSU1          PIC X(02).
                       09  TBL2-H-TENSU2          PIC X(07).
                   07  TBL2-H-KINGAKU.
                       09  TBL2-H-KINGAKU1        PIC X(02).
                       09  TBL2-H-KINGAKU2        PIC X(09).
                   07  TBL2-H-EN              PIC X(01).
      *
      *帳票明細数
       01  LINE-MAX2                   PIC 9(03)   VALUE   31.
       01  PAGE-MAX2                   PIC 9(03)   VALUE   25.
      *
      *01  CONST-H201001               PIC X(08)   VALUE   "20081001".
      *
      *点数記載用
       01  CONS-LABEL1.
           03  FILLER          PIC X(48)   VALUE
           "※金額欄については１０割分の表示をしております。".
           03  FILLER          PIC X(52)   VALUE
           "患者一部負担金は保険公費による負担割合分となります。".
      *金額記載用
       01  CONS-LABEL2.
           03  FILLER          PIC X(36)   VALUE
           "※合計欄は点数での表示をしています。".
           03  FILLER          PIC X(52)   VALUE
           "患者一部負担金はこの点数より計算（１点＝１０円）し、".
           03  FILLER          PIC X(38)   VALUE
           "保険公費による負担割合分となります。".
      *
      *    プログラムオプションパラメタ
       01  PARA-AREA.
           03  PARA-GOKEITANI          PIC X(01).
           03  PARA-SHADE              PIC X(01).
           03  PARA-HKNMEI             PIC X(01).
           03  PARA-FTNKBN             PIC X(01).
           03  PARA-SRYKA              PIC X(01).
           03  PARA-LINE               PIC X(01).
           03  PARA-FORM               PIC X(01).
           03  PARA-HAKKOUBIKBN        PIC X(01).
      *
           03  WRK-PARA-NAME           PIC X(25).
           03  WRK-PARA                PIC X(50).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    医療機関情報−基本情報 
           COPY    "CPSK1001.INC".
      *    医療機関情報−所在地、連絡先 
           COPY    "CPSK1002.INC".
      *
           COPY    "CPSK1005.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *
      *----(03.04.01)--ADD-START---
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *----(03.04.01)--ADD-END-----
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *    診療会計附加情報マスタ−
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    保存用収納ワーク
       01  TBL-SYUNOU-AREA.
         02  TBL-SYUNOU-REC        OCCURS   15.
           COPY    "CPSYUNOU.INC"  REPLACING
                                     //SYU-// BY //TBL-SYU-//.
      *    保存用収納ワーク
       01  HZN-SYUNOU-REC.
           COPY    "CPSYUNOU.INC"  REPLACING
                                     //SYU-// BY //HZN-SYU-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *    請求書編集サブパラメタ
           COPY    "CPORCHC04SUB.INC".
      *
      *    保険組合せ名称編集サブ
           COPY    "CPORCSHKNMEI.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    請求書パラメタ
           COPY    "CPORCHC04.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCHC04AREA
           .
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
           MOVE    ZERO           TO   ORCHC04-PAGE
      *
           IF    ( ORCHC04-PAGE-CNT    IS  NOT NUMERIC )
               MOVE    ZERO       TO   WRK-PAGE-CNT
           ELSE
               MOVE    ORCHC04-PAGE-CNT
                                  TO   WRK-PAGE-CNT
           END-IF
      *
           ACCEPT  SYS-TIME            FROM    TIME
      *
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
      *    各ＤＢ読込
           PERFORM 100-SYORI-SEC
      *
           IF      ORCHC04-KIKAN-STYMD =   SPACE
      *        受診日
               PERFORM 200-DAY-MAIN-SEC
           ELSE
      *        期間指定
               PERFORM 200-KIKAN-MAIN-SEC
           END-IF
      *
           MOVE    WRK-PAGE-CNT    TO  ORCHC04-PAGE-CNT
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           INITIALIZE                  PARA-AREA
      *    合計単位
           MOVE    "GOKEITANI"         TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-GOKEITANI
      *    網掛け
           MOVE    "SHADE"             TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-SHADE
      *
      *    保険組合せ表示
           MOVE    "HKNMEI"            TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-HKNMEI
           IF      PARA-HKNMEI         =   SPACE
               MOVE    "1"                 TO  PARA-HKNMEI
           END-IF
      *    負担割合表示
           MOVE    "FTNKBN"           TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-FTNKBN
           IF      PARA-FTNKBN         =   SPACE
               MOVE    "2"                 TO  PARA-FTNKBN
           END-IF
      *    診療科表示
           MOVE    "SRYKA"             TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-SRYKA
           IF      PARA-SRYKA          =   SPACE
               MOVE    "1"                 TO  PARA-SRYKA
           END-IF
      *
      *    行間
           MOVE    "LINE"              TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-LINE
      *
      *    帳票サイズ
           MOVE    "FORM"              TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-FORM
      *
      *    伝票発行日区分
           MOVE    "HAKKOUBIKBN"       TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-HAKKOUBIKBN
           IF      PARA-HAKKOUBIKBN    =   SPACE
               MOVE    "0"                 TO  PARA-HAKKOUBIKBN
           END-IF
      *
           .
       100-OPTION-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    プログラムオプションパラメタ取得処理
      *****************************************************************
       1001-OPTION-CALL-SEC             SECTION.
      *
           INITIALIZE                  ORCSPRGOPTIONAREA
           MOVE    "ORCHC04"           TO  ORCSPRGOPTION-PRGID
           MOVE    WRK-PARA-NAME       TO  ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"     USING   SPA-AREA
                                               ORCSPRGOPTIONAREA
           MOVE    SPACE               TO  WRK-PARA
           IF      ORCSPRGOPTION-OT-TYPE   =   1
      *        半角で１文字目を値とする
               MOVE    ORCSPRGOPTION-OT-PARA(1:1)  TO  WRK-PARA(1:1)
           END-IF
           .
       1001-OPTION-CALL-EXT.
           EXIT.
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       100-SYORI-SEC                SECTION.
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1001-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1001-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1001-REC
           ELSE
               INITIALIZE                  SYS-1001-REC
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1002-REC
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1002-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1002-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1002-REC
           ELSE
               INITIALIZE                  SYS-1002-REC
           END-IF
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYS-1031-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYS-1031-REC
      *
      *----(03.04.01)--ADD-START---
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *----(03.04.01)--ADD-END-----
      *
           .
       100-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷開始処理
      *****************************************************************
       400-SYOKI-PRINT-SEC             SECTION.
      *
      *    ファイルＯＰＥＮ
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
           MOVE    "HC04"              TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
      *
           ADD     1                   TO  WRK-PAGE-CNT
           MOVE    WRK-PAGE-CNT        TO  HC01PARA-CNT
      *
           OPEN    OUTPUT              PRT-FILE
      *
           .
       400-SYOKI-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       400-PRINT-SEC                SECTION.
      *
      *    印刷用データ出力
           IF      PARA-FORM           =   "1"
      *        A5
               MOVE    HCM04A5             TO  PRT-REC
           ELSE
      *        A4
               MOVE    HCM04               TO  PRT-REC
           END-IF
           WRITE   PRT-REC
      *
           CLOSE                       PRT-FILE
      *
           INITIALIZE                 ORCSMKPRTAREA
           MOVE   "HC01.sh"           TO  MKPRT-ID
           IF      PARA-FORM           =   "1"
      *        A5
               MOVE    "HCM04A5.red"       TO  MKPRT-DIA
           ELSE
      *        A4
               MOVE    "HCM04.red"         TO  MKPRT-DIA
           END-IF
           MOVE   SPACE               TO  MKPRT-DEF
           MOVE   HC01PARA            TO  MKPRT-DAT
           MOVE   SYS-1031-PRTNM(14)  TO  MKPRT-PRTNM
           MOVE   "診療費明細書"      TO  MKPRT-INFO
           CALL   "ORCSMKPRT1"        USING
                                      ORCSMKPRTAREA
                                      SPA-AREA
      *
           .
       400-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理 (印刷ＤＢに出力）
      *****************************************************************
       401-PRINT-SEC                SECTION.
      *
           MOVE    ORCHC04-PRTAREA     TO  ORCSPRTAREA
      *
           IF      PARA-FORM           =   "1"
      *        A5
               MOVE    HCM04A5             TO  SPRT-PRTDATA
               MOVE    "HCM04A5.red"       TO  SPRT-PRTID
           ELSE
      *        A4
               MOVE    HCM04               TO  SPRT-PRTDATA
               MOVE    "HCM04.red"         TO  SPRT-PRTID
           END-IF
      *-----------------------------------------------------------------
           MOVE    "1"                 TO  SPRT-SITEKBN
      *    カスタマイズＰＧの場合"2"をセットすること。
      *    MOVE    "2"                 TO  SPRT-SITEKBN
      *-----------------------------------------------------------------
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF    ( SPRT-RETURN         =   ZERO )
               CONTINUE
           ELSE
               DISPLAY "ORCSPRT ERR: PTID = " SPRT-PTID
           END-IF
      *
           .
       401-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       400-PRINT-SYORI-SEC          SECTION.
      *
           IF      PARA-FORM           =   "1"
               PERFORM 400-A5-PRINT-SYORI-SEC
           ELSE
               PERFORM 400-A4-PRINT-SYORI-SEC
           END-IF
           .
       400-PRINT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理（Ａ４）
      *****************************************************************
       400-A4-PRINT-SYORI-SEC          SECTION.
      *
      *    最終行
           IF      TBL-H-SRYMEI(WRK-PAGE LINE-MAX)
                                       =   SPACE
               IF      PARA-LINE           =   "1"
                   ADD     2               TO  IDX-H
               ELSE
                   ADD     1                   TO  IDX-H
               END-IF
               IF      IDX-H               >   LINE-MAX
                   MOVE    LINE-MAX            TO  IDX-H
               END-IF
               MOVE    "以下余白"          TO  TBL-H-SRYMEI
                                               (WRK-PAGE IDX-H)(30:)
           END-IF
      *
      *    各ＤＢ検索
           PERFORM 4001-DB-KENSAKU-SEC
      *
           MOVE    ZERO                TO  WRK-PAGE
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   WRK-PAGE-MAX
      *
               COMPUTE ORCHC04-PAGE    =   ORCHC04-PAGE    +   1
      *
               IF    ( ORCHC04-PRTKBN      =   "1" )
      *
      *            帳票編集処理
                   PERFORM 300-HCM04-HEN-SEC
      *
      *            帳票印刷処理
                   PERFORM 401-PRINT-SEC
               ELSE
      *
      *            帳票印刷開始処理
                   PERFORM 400-SYOKI-PRINT-SEC
      *
      *            帳票編集処理
                   PERFORM 300-HCM04-HEN-SEC
      *
      *            帳票印刷処理
                   PERFORM 400-PRINT-SEC
               END-IF
           END-PERFORM
      *
           INITIALIZE                  TBL-MEISAI-AREA
           INITIALIZE                  TBL2-MEISAI-A5-AREA
           MOVE    1                   TO  WRK-PAGE
           MOVE    ZERO                TO  IDX-H
           .
       400-A4-PRINT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理（Ａ５）
      *****************************************************************
       400-A5-PRINT-SYORI-SEC          SECTION.
      *
      *    最終行
           IF      TBL2-H-SRYMEI(WRK-PAGE LINE-MAX2)
                                       =   SPACE
               IF      PARA-LINE           =   "1"
                   ADD     2               TO  IDX-H
               ELSE
                   ADD     1                   TO  IDX-H
               END-IF
               IF      IDX-H               >   LINE-MAX2
                   MOVE    LINE-MAX2           TO  IDX-H
               END-IF
               MOVE    "以下余白"          TO  TBL2-H-SRYMEI
                                               (WRK-PAGE IDX-H)(30:)
           END-IF
      *
      *    各ＤＢ検索
           PERFORM 4001-DB-KENSAKU-SEC
      *
           MOVE    ZERO                TO  WRK-PAGE
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   WRK-PAGE-MAX
      *
               COMPUTE ORCHC04-PAGE    =   ORCHC04-PAGE    +   1
      *
               IF    ( ORCHC04-PRTKBN      =   "1" )
      *
      *            帳票編集処理
                   PERFORM 300-HCM04A5-HEN-SEC
      *
      *            帳票印刷処理
                   PERFORM 401-PRINT-SEC
               ELSE
      *
      *            帳票印刷開始処理
                   PERFORM 400-SYOKI-PRINT-SEC
      *
      *            帳票編集処理
                   PERFORM 300-HCM04A5-HEN-SEC
      *
      *            帳票印刷処理
                   PERFORM 400-PRINT-SEC
               END-IF
           END-PERFORM
      *
           INITIALIZE                  TBL-MEISAI-AREA
           INITIALIZE                  TBL2-MEISAI-A5-AREA
           MOVE    1                   TO  WRK-PAGE
           MOVE    ZERO                TO  IDX-H
           .
       400-A5-PRINT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票各ＤＢ検索処理
      *****************************************************************
       4001-DB-KENSAKU-SEC                SECTION.
      *
      *    診療科名編集
           MOVE    SPACE               TO  WRK-SRYKA-MEI
           MOVE    HZN-SYU-SRYKA       TO  WRK-SRYKA
           IF      PARA-SRYKA          =   "0"
               MOVE    SPACE               TO  WRK-SRYKA-MEI
           ELSE
               PERFORM 40011-SRYKAMEI-HEN-SEC
           END-IF
      *    保険組合せ
           MOVE    HZN-SYU-HKNCOMBINUM TO  WRK-HKNCOMBINUM
      *    編集なし
           IF     (PARA-HKNMEI         =   "0" )
               MOVE    SPACE               TO  WRK-HKNCOMBIMEI
           ELSE
               PERFORM 40012-HKNCOMBI-HEN-SEC
           END-IF
      *    負担割合
           MOVE    SPACE               TO  WRK-FUTAN
           MOVE    SPACE               TO  WRK-FUTAN-MEI
           IF      WRK-HKNCOMBIMEI NOT =   SPACE
               EVALUATE    PARA-FTNKBN
                   WHEN    "0"
                       MOVE    SPACE               TO  WRK-FUTAN
                   WHEN    "1"
      *                %　表示
                       MOVE    HZN-SYU-PTFTNRATE   TO  WRK-Z6
                       MOVE    WRK-Z6              TO  WRK-FUTAN
                       MOVE    "％"                TO  WRK-FUTAN-MEI
                   WHEN    "2"
      *                割　表示
                       IF      HZN-SYU-PTFTNRATE   >   ZERO
                           COMPUTE WRK-WARI        =   HZN-SYU-PTFTNRATE
                                                   /   10
                           IF      HZN-SYU-PTFTNRATE(3:1)  =   ZERO
                               MOVE    WRK-WARI            TO  WRK-Z6
                               MOVE    WRK-Z6              TO  WRK-FUTAN
                           ELSE
                               MOVE    WRK-WARI            TO  WRK-Z62
                               MOVE    WRK-Z62             TO  WRK-FUTAN
                           END-IF
                       ELSE
                           MOVE    HZN-SYU-PTFTNRATE   TO  WRK-Z6
                           MOVE    WRK-Z6              TO  WRK-FUTAN
                       END-IF
                       MOVE    "割"                TO  WRK-FUTAN-MEI
               END-EVALUATE
           END-IF
           .
       4001-DB-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       40012-HKNCOMBI-HEN-SEC         SECTION.
      *
           INITIALIZE                  ORCSHKNMEIAREA
           MOVE    WRK-HKNCOMBINUM     TO  ORCSHKNMEI-HKNCOMBINUM
           MOVE    HZN-SYU-SRYYMD      TO  ORCSHKNMEI-SRYYMD
           MOVE    SPA-PTID            TO  ORCSHKNMEI-PTID
           CALL    "ORCSHKNMEI"        USING
                                       SPA-AREA
                                       ORCSHKNMEIAREA
           MOVE    ORCSHKNMEI-HKNCOMBIMEI  TO  WRK-HKNCOMBIMEI
      *
           .
       40012-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科名編集処理
      *****************************************************************
       40011-SRYKAMEI-HEN-SEC                SECTION.
      *
      *    診療科
           MOVE    SPACE               TO  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-SRYKA           TO  SYS-1005-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1005-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME1
                                           TO  WRK-SRYKA-MEI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       40011-SRYKAMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       300-HCM04-HEN-SEC                SECTION.
      *
           INITIALIZE                  HCM04
      *
           ADD     1                   TO  WRK-PAGE
      *    伝票番号
           MOVE    WRK-PAGE            TO  WRK-ZZ9
           MOVE    WRK-ZZ9             TO  HCM04-PAGE
      *    伝票番号
           MOVE    WRK-H-DENPNUM       TO  WRK-NO
           MOVE    WRK-NO              TO  HCM04-NO
      *    診療科
           MOVE    WRK-SRYKA-MEI       TO  HCM04-SRYKANAME
      *    患者番号
           MOVE    SPA-PTNUM           TO  HCM04-PTNUM
      *    患者名称
           MOVE    SPA-NAME            TO  HCM04-NAME
      *    保険種別
           MOVE    WRK-HKNCOMBIMEI     TO  HCM04-HOKEN
      *    負担割合
           MOVE    WRK-FUTAN           TO  HCM04-FUTAN
           MOVE    WRK-FUTAN-MEI       TO  HCM04-FUTANLABEL
      *    診療日
           MOVE    WRK-H-SRYYMD        TO  HCM04-SRYYMD
      *    発行日
           MOVE    WRK-H-HAKYMD        TO  HCM04-HAKYMD
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS        TO  HCM04-IRYOADRS
      *    医療機関名
           MOVE    WRK-HOSPNAME        TO  HCM04-IRYOMEI
      *    医療機関ＴＥＬ
           MOVE    WRK-HOSPTEL         TO  HCM04-IRYOTEL
      *
      *    列名（点数記載）
           IF      PARA-GOKEITANI      =   "1"
               MOVE    "　点　数　"      TO  HCM04-LABEL3
           ELSE
               MOVE    "　単　価　"      TO  HCM04-LABEL3
           END-IF
           MOVE    "　　合　　計　　"    TO  HCM04-LABEL1
      *
      *    明細内容
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   LINE-MAX
               MOVE    TBL-H-SRYKBN (IDX IDX2) TO  HCM04-SRYKBN (IDX2)
               MOVE    TBL-H-SRYMEI (IDX IDX2) TO  HCM04-SRYMEI (IDX2)
      *
               IF      TBL-H-KAISU  (IDX IDX2) =   "---"
                   MOVE    ALL "―"            TO  HCM04-KAISU2 (IDX2)
                   MOVE    ALL "―"            TO  HCM04-KINGAKU2(IDX2)
               ELSE
                   MOVE    TBL-H-KAISU  (IDX IDX2)
                                               TO  HCM04-KAISU (IDX2)
                   MOVE    TBL-H-KINGAKU(IDX IDX2)
                                               TO  HCM04-KINGAKU(IDX2)
                   MOVE    TBL-H-TENSU  (IDX IDX2)
                                               TO  HCM04-TENSU  (IDX2)
               END-IF
      *        点数記載で円のもの
               IF      PARA-GOKEITANI      =   "1"
                   IF      TBL-H-EN   (IDX IDX2)    =   "1"
                       MOVE    SPACE           TO  HCM04-TENSU  (IDX2)
                       STRING  TBL-H-TENSU2 (IDX IDX2)
                               "　"
                                           DELIMITED  BY  SIZE
                                       INTO   HCM04-TENSU (IDX2)
                       END-STRING
                       MOVE    "円"           TO  HCM04-TENSU2  (IDX2)
      *
                       MOVE    SPACE           TO  HCM04-KINGAKU(IDX2)
                       STRING  TBL-H-KINGAKU2(IDX IDX2)
                               "　"
                                           DELIMITED  BY  SIZE
                                       INTO   HCM04-KINGAKU(IDX2)
                       END-STRING
                       MOVE    "円"           TO  HCM04-KINGAKU2(IDX2)
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      PARA-GOKEITANI      =   "1"
               MOVE    CONS-LABEL2         TO  HCM04-LABEL2
           ELSE
               MOVE    CONS-LABEL1         TO  HCM04-LABEL2
           END-IF
      *
           EVALUATE   PARA-SHADE
               WHEN    "0"
      *            網掛けなし
                   STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                           " -L L1  -L L2 -L L3 "
                                               DELIMITED  BY  SIZE
                           "MonpeLayerOut"     DELIMITED  BY  SIZE
                                               INTO    HCM04-LOPTION
                   END-STRING
               WHEN    "1"
      *            網掛の濃さ１
                   STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                           " -L L2  -L L3 "
                                               DELIMITED  BY  SIZE
                           "MonpeLayerOut"     DELIMITED  BY  SIZE
                                               INTO    HCM04-LOPTION
                   END-STRING
               WHEN    "2"
      *            網掛の濃さ２
                   STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                           " -L L1  -L L3 "
                                               DELIMITED  BY  SIZE
                           "MonpeLayerOut"     DELIMITED  BY  SIZE
                                               INTO    HCM04-LOPTION
                   END-STRING
               WHEN    OTHER
      *            網掛濃さ３（標準）
                   STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                           " -L L1  -L L2 "
                                               DELIMITED  BY  SIZE
                           "MonpeLayerOut"     DELIMITED  BY  SIZE
                                               INTO    HCM04-LOPTION
                   END-STRING
           END-EVALUATE
           .
       300-HCM04-HEN-EXT.
           EXIT.
      *****************************************************************
      *   受診日分処理
      *****************************************************************
       200-DAY-MAIN-SEC                SECTION.
      *
           MOVE    1                   TO  FLG-SYORI
           IF      ORCHC04-HAKHOUFLG   =   ZERO
      *        対象の伝票番号分のみ
               PERFORM 201-ONLY-SYUNOU-SEC
           ELSE
      *        発行区分による
               PERFORM 202-KAKHOU-SYUNOU-SEC
           END-IF
           .
       200-DAY-MAIN-EXT.
           EXIT.
      *****************************************************************
      *   対象の伝票番号分のみ処理
      *****************************************************************
       201-ONLY-SYUNOU-SEC                SECTION.
      *
      *    伝票番号
           MOVE    ORCHC04-DENPNUM     TO  WRK-H-DENPNUM
      *    収納検索
           MOVE    ORCHC04-DENPNUM     TO  WRK-DENPNUM
           PERFORM 2011-SYUNOU-HEN-SEC
      *    診療日・発行日編集処理
           PERFORM 20021-INIT-HENSYU-SEC
      *    伝票番号分印刷
           PERFORM 2001-ONLY-SYUNOU-HEN-SEC
           IF      IDX-H               >   ZERO
      *        帳票印刷
               PERFORM 400-PRINT-SYORI-SEC
           END-IF
      *
           .
       201-ONLY-SYUNOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日・発行日編集処理
      *****************************************************************
       20021-INIT-HENSYU-SEC             SECTION.
      *
      *    診療日
           MOVE    "診療日"            TO  WRK-LBLSRYYMD
           MOVE    SYU-SRYYMD          TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-H-SRYYMD
      *    発行日
           IF     (ORCHC04-DENPPRTYMD  =   SPACE )
               IF     (PARA-HAKKOUBIKBN    =   "1" )
                 AND  (ORCHC04-HAKKOFLG    =    1   OR   3 )
                   MOVE    SPA-SYSYMD      TO  WRK-SYMD
               ELSE
                   MOVE    SYU-DENPPRTYMD  TO  WRK-SYMD
               END-IF
           ELSE
               MOVE    ORCHC04-DENPPRTYMD  TO  WRK-SYMD
           END-IF
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-H-HAKYMD
           .
       20021-INIT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納検索処理
      *****************************************************************
       2011-SYUNOU-HEN-SEC             SECTION.
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    WRK-DENPNUM         TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2011-SYUNOU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   複数科・保険入力の印刷処理
      *****************************************************************
       202-KAKHOU-SYUNOU-SEC                SECTION.
      *
      *    収納検索
           MOVE    ORCHC04-DENPNUM     TO  WRK-DENPNUM
           PERFORM 2011-SYUNOU-HEN-SEC
      *    診療日・発行日編集処理
           PERFORM 20021-INIT-HENSYU-SEC
      *
           MOVE    ZERO                TO  IDX-H
           IF      SYU-GRP-DENPNUM     =   ZERO
      *        単独入力
               PERFORM 2001-ONLY-SYUNOU-HEN-SEC
           ELSE
      *        複数入力
               PERFORM 2002-GRP-SYUNOU-HEN-SEC
           END-IF
      *
           IF      IDX-H               >   ZERO
      *        帳票印刷
               PERFORM 400-PRINT-SYORI-SEC
           END-IF
      *
           .
       202-KAKHOU-SYUNOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    単独入力編集処理
      *****************************************************************
       2001-ONLY-SYUNOU-HEN-SEC          SECTION.
      *
           INITIALIZE                  TBL-MEISAI-AREA
           INITIALIZE                  TBL2-MEISAI-A5-AREA
           MOVE    1                   TO  WRK-PAGE
           MOVE    ZERO                TO  IDX-H
      *
           INITIALIZE                  WRK-KEY-AREA
           MOVE    SYU-SRYKA           TO  WRK-KEY-SRYKA
           MOVE    SYU-HKNCOMBINUM     TO  WRK-KEY-HKNCOMBINUM
           MOVE    SYU-DENPNUM         TO  WRK-KEY-DENPNUM
           MOVE    ZERO                TO  WRK-KEY-PCNT
           MOVE    SYUNOU-REC          TO  HZN-SYUNOU-REC
           PERFORM 300-MEISAI-SYORI-SEC
           .
       2001-ONLY-SYUNOU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数診療科・保険組合せ別編集処理
      *****************************************************************
       2002-GRP-SYUNOU-HEN-SEC          SECTION.
      *
           INITIALIZE                  TBL-MEISAI-AREA
           INITIALIZE                  TBL2-MEISAI-A5-AREA
           MOVE    1                   TO  WRK-PAGE
           MOVE    ZERO                TO  IDX-H
      *
           INITIALIZE                  TBL-SYUNOU-AREA
           MOVE    ZERO                TO  IDX-SYUMAX
           MOVE    SYU-GRP-DENPNUM     TO  WRK-GRP-DENPNUM
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    WRK-GRP-DENPNUM     TO  SYU-GRP-DENPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key27"             TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
           PERFORM
               UNTIL       FLG-SYUNOU      =   1
      *        収納編集
               ADD     1                   TO  IDX-SYUMAX
               MOVE    SYUNOU-REC          TO  TBL-SYUNOU-REC
                                                   (IDX-SYUMAX)
      *
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key27"             TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           EVALUATE    ORCHC04-HAKHOUFLG
      *        科・保険別
               WHEN    1
                   PERFORM 2001-SYORI-01-SEC
      *        保険別
               WHEN    2
                   PERFORM 2002-SYORI-02-SEC
      *        科別
               WHEN    3
                   PERFORM 2003-SYORI-03-SEC
      *        合計
               WHEN    4
                   PERFORM 2004-SYORI-04-SEC
           END-EVALUATE
           .
       2002-GRP-SYUNOU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療科・保険組合せ別編集処理
      *****************************************************************
       2001-SYORI-01-SEC             SECTION.
      *
           INITIALIZE                  WRK-KEY-AREA
           PERFORM VARYING     IDX-SYU  FROM    1   BY  1
                   UNTIL       IDX-SYU >   IDX-SYUMAX
               MOVE    TBL-SYUNOU-REC(IDX-SYU) TO  SYUNOU-REC
               MOVE    SYU-SRYKA           TO  WRK-KEY-SRYKA
               MOVE    SYU-HKNCOMBINUM     TO  WRK-KEY-HKNCOMBINUM
               MOVE    SYU-DENPNUM         TO  WRK-KEY-DENPNUM
               MOVE    ZERO                TO  WRK-KEY-PCNT
      *        表示伝票番号
               MOVE    SYU-DENPNUM         TO  WRK-H-DENPNUM
               MOVE    TBL-SYUNOU-REC (IDX-SYU)
                                           TO  HZN-SYUNOU-REC
               PERFORM 300-MEISAI-SYORI-SEC
               IF      IDX-H           >   ZERO
      *            帳票印刷
                   PERFORM 400-PRINT-SYORI-SEC
               END-IF
           END-PERFORM
           .
       2001-SYORI-01-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ別編集処理
      *****************************************************************
       2002-SYORI-02-SEC               SECTION.
      *
           INITIALIZE                  WRK-KEY-AREA
      *
           PERFORM VARYING     IDX-SYU     FROM    1   BY  1
                   UNTIL       IDX-SYU     >   IDX-SYUMAX
               IF      TBL-SYUNOU-REC (IDX-SYU)    =   SPACE
                   CONTINUE
               ELSE
                   MOVE    TBL-SYU-HKNCOMBINUM (IDX-SYU)
                                               TO  WRK-KEY-HKNCOMBINUM
                   MOVE    ZERO                TO  WRK-KEY-PCNT
      *            表示伝票番号
                   MOVE    TBL-SYU-DENPNUM (IDX-SYU)
                                               TO  WRK-H-DENPNUM
                   MOVE    TBL-SYUNOU-REC (IDX-SYU)
                                               TO  HZN-SYUNOU-REC
                   PERFORM VARYING     IDX-SYU2    FROM    1   BY  1
                           UNTIL       IDX-SYU2    >   IDX-SYUMAX
                       IF      WRK-KEY-HKNCOMBINUM =
                                         TBL-SYU-HKNCOMBINUM (IDX-SYU2)
                           MOVE    TBL-SYU-DENPNUM (IDX-SYU2)
                                                   TO  WRK-KEY-DENPNUM
                           MOVE    TBL-SYUNOU-REC (IDX-SYU2)
                                                   TO   SYUNOU-REC
                           PERFORM 300-MEISAI-SYORI-SEC
                           MOVE    SPACE           TO  TBL-SYUNOU-REC
                                                             (IDX-SYU2)
                       END-IF
                   END-PERFORM
                   IF      IDX-H           >   ZERO
      *                帳票印刷
                       PERFORM 400-PRINT-SYORI-SEC
                   END-IF
               END-IF
           END-PERFORM
           .
       2002-SYORI-02-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科別編集処理
      *****************************************************************
       2003-SYORI-03-SEC               SECTION.
      *
           INITIALIZE                  WRK-KEY-AREA
      *
           PERFORM VARYING     IDX-SYU     FROM    1   BY  1
                   UNTIL       IDX-SYU     >   IDX-SYUMAX
               IF      TBL-SYUNOU-REC (IDX-SYU)    =   SPACE
                   CONTINUE
               ELSE
                   MOVE    TBL-SYU-SRYKA (IDX-SYU)
                                               TO  WRK-KEY-SRYKA
                   MOVE    ZERO                TO  WRK-KEY-PCNT
      *            表示伝票番号
                   MOVE    TBL-SYU-DENPNUM (IDX-SYU)
                                               TO  WRK-H-DENPNUM
                   MOVE    TBL-SYUNOU-REC (IDX-SYU)
                                               TO  HZN-SYUNOU-REC
                   PERFORM VARYING     IDX-SYU2    FROM    1   BY  1
                           UNTIL       IDX-SYU2    >   IDX-SYUMAX
                       IF      WRK-KEY-SRYKA   =
                                           TBL-SYU-SRYKA (IDX-SYU2)
                           MOVE    TBL-SYU-DENPNUM (IDX-SYU2)
                                                   TO  WRK-KEY-DENPNUM
                           MOVE    TBL-SYUNOU-REC (IDX-SYU2)
                                                   TO   SYUNOU-REC
                           PERFORM 300-MEISAI-SYORI-SEC
                           MOVE    SPACE           TO  TBL-SYUNOU-REC
                                                           (IDX-SYU2)
                       END-IF
                   END-PERFORM
                   IF      IDX-H           >   ZERO
      *                帳票印刷
                       PERFORM 400-PRINT-SYORI-SEC
                   END-IF
               END-IF
           END-PERFORM
           .
       2003-SYORI-03-EXT.
           EXIT.
      *
      *****************************************************************
      *    合計編集処理
      *****************************************************************
       2004-SYORI-04-SEC               SECTION.
      *
           INITIALIZE                  WRK-KEY-AREA
      *
           PERFORM VARYING     IDX-SYU   FROM    1   BY  1
                   UNTIL       IDX-SYU   >   IDX-SYUMAX
               MOVE    TBL-SYU-DENPNUM (IDX-SYU)
                                           TO  WRK-KEY-DENPNUM
               MOVE    TBL-SYUNOU-REC (IDX-SYU)
                                           TO   SYUNOU-REC
               IF      IDX-SYU             =   1
                   MOVE    TBL-SYUNOU-REC (IDX-SYU)
                                               TO  HZN-SYUNOU-REC
      *            表示伝票番号
                   MOVE    TBL-SYU-DENPNUM (IDX-SYU)
                                               TO  WRK-H-DENPNUM
               END-IF
               PERFORM 300-MEISAI-SYORI-SEC
           END-PERFORM
           IF      IDX-H           >   ZERO
      *        帳票印刷
               PERFORM 400-PRINT-SYORI-SEC
           END-IF
           .
       2004-SYORI-04-EXT.
           EXIT.
      *
      *****************************************************************
      *    伝票毎明細編集処理
      *****************************************************************
       300-MEISAI-SYORI-SEC        SECTION.
      *
           IF      WRK-KEY-PCNT        =   ZERO
               IF      IDX-H           >   ZERO
      *            帳票印刷
                   PERFORM 400-PRINT-SYORI-SEC
               END-IF
           END-IF
      *
      *    伝票番号毎明細編集処理
           INITIALIZE                  ORCHC04SUBAREA
           MOVE    WRK-KEY-DENPNUM     TO  ORCHC04SUB-DENPNUM
           MOVE    PARA-GOKEITANI      TO  ORCHC04SUB-GOKEITANI
           CALL    "ORCHC04SUB"        USING
                                       SPA-AREA
                                       ORCHC04SUBAREA
           MOVE    ORCHC04SUB-TBL-MAX  TO  IDX-TBL
      *
           IF      IDX-TBL             >   ZERO
      *        診療科・保険組合せ
               IF      WRK-KEY-PCNT        =   ZERO
                   MOVE    SYU-SRYKA       TO  WRK-SYU-SRYKA
                   MOVE    SYU-HKNCOMBINUM TO  WRK-SYU-HKNCOMBINUM
               ELSE
      *            伝票毎の切り替え
                   PERFORM 3006-KEY-HENSYU-SEC
               END-IF
               PERFORM 3005-PRTTBL-HENSYU-SEC
               ADD     1                   TO  WRK-KEY-PCNT
           END-IF
           .
       300-MEISAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    伝票毎の切り替え処理
      *****************************************************************
       3006-KEY-HENSYU-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYKA-MEI
           MOVE    SPACE               TO  WRK-HKNCOMBIMEI
      *    診療科名編集
           IF      WRK-KEY-SRYKA       =   SPACE
               IF     (PARA-SRYKA          =   "0" )
                   MOVE    SPACE               TO  WRK-SRYKA-MEI
               ELSE
                   MOVE    SYU-SRYKA           TO  WRK-SRYKA
                   PERFORM 40011-SRYKAMEI-HEN-SEC
               END-IF
           END-IF
      *    保険組合せ
           IF      WRK-KEY-HKNCOMBINUM =   SPACE
               IF     (PARA-HKNMEI         =   "0" )
                   MOVE    SPACE               TO  WRK-HKNCOMBIMEI
               ELSE
                   MOVE    SYU-HKNCOMBINUM     TO  WRK-HKNCOMBINUM
                   PERFORM 40012-HKNCOMBI-HEN-SEC
               END-IF
           END-IF
      *
           IF      PARA-FORM           =   "1"
      *        Ａ５
               PERFORM 30062-KEY-HENSYU-A5-SEC
           ELSE
      *        Ａ４
               PERFORM 30061-KEY-HENSYU-A4-SEC
           END-IF
      *
           IF      PARA-LINE           =   "1"
               CONTINUE
           ELSE
               ADD     1                   TO  IDX-H
           END-IF
           .
        3006-KEY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    伝票毎の切り替え処理（Ａ４）
      *****************************************************************
       30061-KEY-HENSYU-A4-SEC        SECTION.
      *
      *    剤毎に１行開ける
           IF     (PARA-LINE           =   "1"  )
             AND  (IDX-H               >   ZERO )
               ADD     2                   TO  IDX-H
           ELSE
               ADD     1                   TO  IDX-H
           END-IF
           IF      IDX-H               >=  LINE-MAX
               MOVE    1               TO  FLG-NEXT
               PERFORM 200520-PAGE-CNT-SEC
           END-IF
           MOVE    ALL "―"            TO  TBL-H-SRYKBN (WRK-PAGE IDX-H)
           MOVE    ALL "-"             TO  TBL-H-KAISU  (WRK-PAGE IDX-H)
           MOVE    ALL "-"             TO  TBL-H-KINGAKU(WRK-PAGE IDX-H)
           EVALUATE    TRUE
               WHEN   (WRK-SRYKA-MEI   NOT =   SPACE )  AND
                      (WRK-HKNCOMBIMEI NOT =   SPACE )
                   STRING    "≪"
                             WRK-SRYKA-MEI
                             "≫　" 
                             "【"
                             WRK-HKNCOMBIMEI
                             "】"              DELIMITED  BY  SPACE
                                   INTO    TBL-H-SRYMEI(WRK-PAGE IDX-H)
                   END-STRING
               WHEN   (WRK-SRYKA-MEI   NOT =   SPACE )  AND
                      (WRK-HKNCOMBIMEI     =   SPACE )
                   STRING    "≪"
                             WRK-SRYKA-MEI
                             "≫"              DELIMITED  BY  SPACE
                                   INTO    TBL-H-SRYMEI(WRK-PAGE IDX-H)
                   END-STRING
               WHEN   (WRK-SRYKA-MEI       =   SPACE )  AND
                      (WRK-HKNCOMBIMEI NOT =   SPACE )
                   STRING    "【"
                             WRK-HKNCOMBIMEI
                             "】"              DELIMITED  BY  SPACE
                                   INTO    TBL-H-SRYMEI(WRK-PAGE IDX-H)
                   END-STRING
           END-EVALUATE
      *
           IF      FLG-SYORI           =   2
               MOVE    TBL-H-SRYMEI(WRK-PAGE IDX-H)  TO  WRK-SRYMEI-H
               MOVE    SPACE           TO  TBL-H-SRYMEI(WRK-PAGE IDX-H)
               STRING  WRK-DAY-HEN
                       WRK-SRYMEI-H         DELIMITED  BY  SIZE
                                   INTO    TBL-H-SRYMEI(WRK-PAGE IDX-H)
               END-STRING
           END-IF
      *
           INSPECT TBL-H-SRYMEI(WRK-PAGE IDX-H)
                           REPLACING    ALL    "  "   BY  "―"
           .
       30061-KEY-HENSYU-A4-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷内容明細編集処理
      *****************************************************************
       3005-PRTTBL-HENSYU-SEC        SECTION.
      *
           IF      PARA-FORM           =   "1"
      *        Ａ５サイズ
               PERFORM 30052-A5-PRTTBL-HENSYU-SEC
           ELSE
      *        Ａ４サイズ
               PERFORM 30051-PRTTBL-HENSYU-SEC
           END-IF
           .
        3005-PRTTBL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷内容明細編集処理（Ａ４）
      *****************************************************************
       30051-PRTTBL-HENSYU-SEC        SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   ORCHC04SUB-TBL-MAX
               ADD     1                   TO  IDX-H
               IF      IDX-H               >=  LINE-MAX
                   MOVE    1                   TO  FLG-NEXT
                   PERFORM 200520-PAGE-CNT-SEC
               END-IF
      *        名称と単位名称チェックの改行対応
      *        MOVE    ORCHC04SUB-NAME (IDX)   TO  WRK-H-NAME
      *        MOVE    ZERO                    TO  FLG-LINE
      *        IF    ((ORCHC04SUB-TANINAME(IDX)
      *                                    NOT =   SPACE)
      *          AND  (WRK-H-NAME2-2       NOT =   SPACE) )
      *        OR    ( WRK-H-NAME2         NOT =   SPACE  )
      *            MOVE    1                   TO  FLG-LINE
      *            最終行の時、改ページ
      *            IF      IDX-H               >=  (LINE-MAX - 1 )
      *                MOVE    1               TO  FLG-NEXT
      *                PERFORM 200520-PAGE-CNT-SEC
      *            END-IF
      ****     END-IF
      *        名称＋数量編集
               IF     (ORCHC04SUB-SURYO  (IDX) =   ZERO  )
                 OR   (ORCHC04SUB-SRYKBN (IDX) =   "95"
                                               OR  "96"  )
      *            自費の数量記載なし
                   MOVE    ORCHC04SUB-NAME (IDX)   TO  WRK-H-NAME
               ELSE
                   MOVE    SPACE                   TO  WRK-H-NAME
                   STRING  ORCHC04SUB-NAME (IDX)
                           "　　"
                           ORCHC04SUB-H-ZEN-SURYO (IDX)
                           DELIMITED  BY  SPACE
                                           INTO   WRK-H-NAME
                   END-STRING
               END-IF
      *
      *        名称と単位名称チェック
               IF     (WRK-H-NAME2         NOT =   SPACE)
                   MOVE    1                   TO  FLG-LINE
      *            最終行の時、改ページ
                   IF      IDX-H               >=  (LINE-MAX - 1 )
                       MOVE    1               TO  FLG-NEXT
                       PERFORM 200520-PAGE-CNT-SEC
                   END-IF
               END-IF
               MOVE    ORCHC04SUB-H-SRYKBN(IDX)    TO  TBL-H-SRYKBN
                                                     (WRK-PAGE IDX-H)
      *        剤開始（＊）
               MOVE    ORCHC04SUB-H-ZAIHEN (IDX)
                                               TO  TBL-H-ZKBN
                                                     (WRK-PAGE IDX-H)
      *        名称
      ******** MOVE    ORCHC04SUB-NAME (IDX)   TO  WRK-H-NAME
               MOVE    WRK-H-NAME1             TO  TBL-H-NAME
                                                     (WRK-PAGE IDX-H)
               IF      FLG-LINE            =   1
                   ADD     1                   TO  IDX-H
               END-IF
               IF      WRK-H-NAME2         NOT =   SPACE
                   MOVE    WRK-H-NAME2         TO  TBL-H-NAME
                                                     (WRK-PAGE IDX-H)
                   MOVE    "　"                TO  TBL-H-ZKBN
                                                     (WRK-PAGE IDX-H)
               END-IF
      *        単位名称
      *        MOVE    ORCHC04SUB-TANINAME (IDX)   TO  TBL-H-TANINAME
      *                                            (WRK-PAGE IDX-H)
      *        数量
      *        MOVE    ORCHC04SUB-H-SURYO (IDX)    TO  TBL-H-SURYO
      *                                            (WRK-PAGE IDX-H)
      *        IF      ORCHC04SUB-SRYKBN (IDX) =   "95"
      *                                        OR  "96"
      *            自費の数量記載なし
      *            MOVE    SPACE               TO  TBL-H-SURYO
      *                                            (WRK-PAGE IDX-H)
      *****    END-IF
      *        点数（単価）
               MOVE    ORCHC04SUB-H-TENSU (IDX)    TO  TBL-H-TENSU
                                                   (WRK-PAGE IDX-H)
      *        合計
               MOVE    ORCHC04SUB-H-KINGAKU(IDX)   TO  TBL-H-KINGAKU
                                                   (WRK-PAGE IDX-H)
      *        円
               MOVE    ORCHC04SUB-H-ENKBN (IDX)    TO  TBL-H-EN
                                                   (WRK-PAGE IDX-H)
      *        回数
               MOVE    ORCHC04SUB-H-KAISU (IDX)    TO  TBL-H-KAISU
                                                   (WRK-PAGE IDX-H)
      *        剤毎に１行開ける
               IF      PARA-LINE           =   "1"
                   CONTINUE
               ELSE
                   IF      ORCHC04SUB-ZAIEND (IDX) =   "1"
                       ADD     1                   TO  IDX-H
                   END-IF
               END-IF
           END-PERFORM
           MOVE    WRK-PAGE            TO  WRK-PAGE-MAX
      *
           .
       30051-PRTTBL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ページカウント処理
      *****************************************************************
       200520-PAGE-CNT-SEC                  SECTION.
      *
      *    最終明細の時、改ページなし
           IF      FLG-NEXT            =   1
               MOVE    LINE-MAX            TO  IDX-H
               MOVE    "次ページへ続く"    TO  TBL-H-SRYMEI
                                               (WRK-PAGE IDX-H)(30:)
               ADD     1                   TO  WRK-PAGE
      *        テーブル最大
               IF      WRK-PAGE            >   PAGE-MAX
      *            帳票印刷
                   PERFORM 400-PRINT-SYORI-SEC
               END-IF
      *
               MOVE    1                   TO  IDX-H
               MOVE    SPACE               TO  WRK-SRYMEI-OLD
           END-IF
           .
       200520-PAGE-CNT-EXT.
           EXIT.
      *****************************************************************
      *    期間指定処理
      *****************************************************************
       200-KIKAN-MAIN-SEC                SECTION.
      *
      *    伝票番号
           MOVE    ORCHC04-DENPNUM     TO  WRK-H-DENPNUM
      *    診療日
           MOVE    "診療日"            TO  WRK-LBLSRYYMD
           MOVE    ORCHC04-KIKAN-STYMD TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-STYMD
      *    開始日＝終了日は期間指定なしとする
           IF      ORCHC04-KIKAN-STYMD =   ORCHC04-KIKAN-EDYMD
               MOVE    WRK-STYMD           TO  WRK-H-SRYYMD
               MOVE    1                   TO  FLG-SYORI
           ELSE
               MOVE    ORCHC04-KIKAN-EDYMD TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG         TO  WRK-EDYMD
               STRING  WRK-STYMD
                   "〜"
                   WRK-EDDD            DELIMITED  BY  SIZE
                                       INTO    WRK-H-SRYYMD
               END-STRING
               MOVE    2                   TO  FLG-SYORI
           END-IF
      *    発行日
           IF    ( ORCHC04-DENPPRTYMD  NOT =   SPACE )
               MOVE    ORCHC04-DENPPRTYMD  TO  WRK-SYMD
           ELSE
               MOVE    SPA-SYSYMD          TO  WRK-SYMD
           END-IF
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-H-HAKYMD
      *
           INITIALIZE                  TBL-MEISAI-AREA
           INITIALIZE                  TBL2-MEISAI-A5-AREA
           MOVE    1                   TO  WRK-PAGE
           MOVE    ZERO                TO  IDX-H
      *
           INITIALIZE                  WRK-KEY-AREA
      *
           PERFORM VARYING     IDX-SYU   FROM    1   BY  1
                   UNTIL      (IDX-SYU   >   100 )
                         OR   (ORCHC04-KIKAN-DENPNUM (IDX-SYU)
                                         =   ZERO )
               MOVE    ORCHC04-KIKAN-DENPNUM (IDX-SYU)
                                           TO  WRK-DENPNUM
               PERFORM 2011-SYUNOU-HEN-SEC
               IF      IDX-SYU             =   1
                   MOVE    SYUNOU-REC          TO  HZN-SYUNOU-REC
                   MOVE    SYU-SRYYMD          TO  WRK-SYU-SRYYMD
                   MOVE    SYU-SRYKA           TO  WRK-SYU-SRYKA
                   MOVE    SYU-HKNCOMBINUM     TO  WRK-SYU-HKNCOMBINUM
               END-IF
               MOVE    ORCHC04-KIKAN-DENPNUM (IDX-SYU)
                                           TO  WRK-KEY-DENPNUM
               PERFORM 230-MEISAI-SYORI-2-SEC
           END-PERFORM
           IF      IDX-H           >   ZERO
      *        帳票印刷
               PERFORM 400-PRINT-SYORI-SEC
           END-IF
           .
       200-KIKAN-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    伝票番号毎処理
      *****************************************************************
       230-MEISAI-SYORI-2-SEC        SECTION.
      *
           EVALUATE    ORCHC04-HAKHOUFLG
      *        科・保険別
               WHEN    1
                   MOVE    SYU-SRYKA       TO  WRK-KEY-SRYKA
                   MOVE    SYU-HKNCOMBINUM TO  WRK-KEY-HKNCOMBINUM
      *        保険別
               WHEN    2
                   MOVE    SPACE           TO  WRK-KEY-SRYKA
                   MOVE    SYU-HKNCOMBINUM TO  WRK-KEY-HKNCOMBINUM
      *        科別
               WHEN    3
                   MOVE    SYU-SRYKA       TO  WRK-KEY-SRYKA
                   MOVE    SPACE           TO  WRK-KEY-HKNCOMBINUM
      *        合計
               WHEN    4
                   MOVE    SPACE           TO  WRK-KEY-SRYKA
                   MOVE    SPACE           TO  WRK-KEY-HKNCOMBINUM
           END-EVALUATE
      *
      *    伝票番号毎明細編集処理
           INITIALIZE                  ORCHC04SUBAREA
           MOVE    WRK-KEY-DENPNUM     TO  ORCHC04SUB-DENPNUM
           MOVE    PARA-GOKEITANI      TO  ORCHC04SUB-GOKEITANI
           CALL    "ORCHC04SUB"        USING
                                       SPA-AREA
                                       ORCHC04SUBAREA
           MOVE    ORCHC04SUB-TBL-MAX  TO  IDX-TBL
      *
           IF      IDX-TBL             >   ZERO
      *        診療科・保険組合せ
               IF     (WRK-KEY-PCNT        =   ZERO )
                AND   (FLG-SYORI           =   1    )
                   CONTINUE
               ELSE
                   IF      FLG-SYORI           =   2
                       MOVE    SYU-SRYYMD          TO  WRK-SYMD
                       PERFORM 800-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG(17:6)   TO  WRK-DAY-H2
                       MOVE    "〔"                TO  WRK-DAY-H1
                       MOVE    "〕"                TO  WRK-DAY-H3
                   END-IF
      *            伝票毎の切り替え
                   PERFORM 3006-KEY-HENSYU-SEC
               END-IF
               PERFORM 3005-PRTTBL-HENSYU-SEC
               ADD     1                   TO  WRK-KEY-PCNT
      *        診療科・保険組合せ
               MOVE    SYU-SRYYMD          TO  WRK-SYU-SRYYMD
               MOVE    SYU-SRYKA           TO  WRK-SYU-SRYKA
               MOVE    SYU-HKNCOMBINUM     TO  WRK-SYU-HKNCOMBINUM
           END-IF
           .
       230-MEISAI-SYORI-2-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷内容明細編集処理（Ａ５）
      *****************************************************************
       30052-A5-PRTTBL-HENSYU-SEC        SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   ORCHC04SUB-TBL-MAX
               ADD     1                   TO  IDX-H
               IF      IDX-H               >=  LINE-MAX2
                   MOVE    1                   TO  FLG-NEXT
                   PERFORM 200521-A5PAGE-CNT-SEC
               END-IF
      *        名称＋数量編集
               IF     (ORCHC04SUB-SURYO  (IDX) =   ZERO )
                 OR   (ORCHC04SUB-SRYKBN (IDX) =   "95"
                                               OR  "96"     )
      *            自費の数量記載なし
                   MOVE    ORCHC04SUB-NAME (IDX)   TO  WRK-A5-NAME
               ELSE
                   MOVE    SPACE                   TO  WRK-A5-NAME
                   STRING  ORCHC04SUB-NAME (IDX)
                           "　　"
                           ORCHC04SUB-H-ZEN-SURYO (IDX)
                           DELIMITED  BY  SPACE
                                           INTO   WRK-A5-NAME
                   END-STRING
               END-IF
      *
      *        名称と単位名称チェック
               IF     (WRK-A5-NAME-2       NOT =   SPACE)
      *            最終行の時、改ページ
                   IF      IDX-H               >=  (LINE-MAX2 - 1 )
                       MOVE    1               TO  FLG-NEXT
                       PERFORM 200521-A5PAGE-CNT-SEC
                   END-IF
               END-IF
               MOVE    ORCHC04SUB-H-SRYKBN(IDX)    TO  TBL2-H-SRYKBN
                                                     (WRK-PAGE IDX-H)
      *        ＊
               MOVE    ORCHC04SUB-H-ZAIHEN (IDX)
                                               TO  TBL2-H-ZKBN
                                                     (WRK-PAGE IDX-H)
               MOVE    WRK-A5-NAME-1           TO  TBL2-H-NAME
                                                     (WRK-PAGE IDX-H)
      *        単位名称
               IF     (WRK-A5-NAME-2       NOT =   SPACE)
                   ADD     1                   TO  IDX-H
                   MOVE    WRK-A5-NAME-2       TO  TBL2-H-NAME
                                                     (WRK-PAGE IDX-H)
                   MOVE    "　"                TO  TBL2-H-ZKBN
                                                     (WRK-PAGE IDX-H)
               END-IF
      *        点数（単価）
               MOVE    ORCHC04SUB-H-TENSU (IDX)    TO  TBL2-H-TENSU
                                                   (WRK-PAGE IDX-H)
      *        合計
               MOVE    ORCHC04SUB-H-KINGAKU (IDX)  TO  TBL2-H-KINGAKU
                                                   (WRK-PAGE IDX-H)
      *        円
               MOVE    ORCHC04SUB-H-ENKBN (IDX)    TO  TBL2-H-EN
                                                   (WRK-PAGE IDX-H)
      *        回数
               MOVE    ORCHC04SUB-H-KAISU (IDX)    TO  TBL2-H-KAISU
                                                   (WRK-PAGE IDX-H)
      *        剤毎に１行開ける
               IF      PARA-LINE           =   "1"
                   CONTINUE
               ELSE
                   IF      ORCHC04SUB-ZAIEND (IDX) =   "1"
                       ADD     1                   TO  IDX-H
                   END-IF
               END-IF
           END-PERFORM
           MOVE    WRK-PAGE            TO  WRK-PAGE-MAX
      *
           .
       30052-A5-PRTTBL-HENSYUEXT.
           EXIT.
      *
      *****************************************************************
      *    ページカウント処理（Ａ５）
      *****************************************************************
       200521-A5PAGE-CNT-SEC                  SECTION.
      *
      *    最終明細の時、改ページなし
           IF      FLG-NEXT            =   1
               MOVE    LINE-MAX2           TO  IDX-H
               MOVE    "次ページへ続く"    TO  TBL2-H-SRYMEI
                                               (WRK-PAGE IDX-H)(30:)
               ADD     1                   TO  WRK-PAGE
      *        テーブル最大
               IF      WRK-PAGE            >   PAGE-MAX2
      *            帳票印刷
                   PERFORM 400-PRINT-SYORI-SEC
               END-IF
      *
               MOVE    1                   TO  IDX-H
               MOVE    SPACE               TO  WRK-SRYMEI-OLD
           END-IF
           .
       200521-A5PAGE-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    伝票毎の切り替え処理（Ａ５）
      *****************************************************************
       30062-KEY-HENSYU-A5-SEC        SECTION.
      *
      *    剤毎に１行開ける
           IF     (PARA-LINE           =   "1"  )
             AND  (IDX-H               >   ZERO )
               ADD     2                   TO  IDX-H
           ELSE
               ADD     1                   TO  IDX-H
           END-IF
           IF      IDX-H               >=  LINE-MAX2
               MOVE    1               TO  FLG-NEXT
               PERFORM 200521-A5PAGE-CNT-SEC
           END-IF
           MOVE    ALL "―"            TO  TBL2-H-SRYKBN(WRK-PAGE IDX-H)
           MOVE    ALL "-"             TO  TBL2-H-KAISU (WRK-PAGE IDX-H)
           MOVE    ALL "-"             TO  TBL2-H-KINGAKU
                                                       (WRK-PAGE IDX-H)
           EVALUATE    TRUE
               WHEN   (WRK-SRYKA-MEI   NOT =   SPACE )  AND
                      (WRK-HKNCOMBIMEI NOT =   SPACE )
                   STRING    "≪"
                             WRK-SRYKA-MEI
                             "≫　" 
                             "【"
                             WRK-HKNCOMBIMEI
                             "】"              DELIMITED  BY  SPACE
                                   INTO    TBL2-H-SRYMEI(WRK-PAGE IDX-H)
                   END-STRING
               WHEN   (WRK-SRYKA-MEI   NOT =   SPACE )  AND
                      (WRK-HKNCOMBIMEI     =   SPACE )
                   STRING    "≪"
                             WRK-SRYKA-MEI
                             "≫"              DELIMITED  BY  SPACE
                                   INTO    TBL2-H-SRYMEI(WRK-PAGE IDX-H)
                   END-STRING
               WHEN   (WRK-SRYKA-MEI       =   SPACE )  AND
                      (WRK-HKNCOMBIMEI NOT =   SPACE )
                   STRING    "【"
                             WRK-HKNCOMBIMEI
                             "】"              DELIMITED  BY  SPACE
                                   INTO    TBL2-H-SRYMEI(WRK-PAGE IDX-H)
                   END-STRING
           END-EVALUATE
      *
           IF      FLG-SYORI           =   2
               MOVE    TBL2-H-SRYMEI(WRK-PAGE IDX-H)  TO  WRK-SRYMEI-H
               MOVE    SPACE           TO  TBL2-H-SRYMEI(WRK-PAGE IDX-H)
               STRING  WRK-DAY-HEN
                       WRK-SRYMEI-H         DELIMITED  BY  SIZE
                                   INTO    TBL2-H-SRYMEI(WRK-PAGE IDX-H)
               END-STRING
           END-IF
      *
           INSPECT TBL2-H-SRYMEI(WRK-PAGE IDX-H)
                           REPLACING    ALL    "  "   BY  "―"
           .
       30062-KEY-HENSYU-A5-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理（Ａ５）
      *****************************************************************
       300-HCM04A5-HEN-SEC                SECTION.
      *
           INITIALIZE                  HCM04A5
      *
           ADD     1                   TO  WRK-PAGE
      *    伝票番号
           MOVE    WRK-PAGE            TO  WRK-ZZ9
           MOVE    WRK-ZZ9             TO  HCM04A5-PAGE
      *    伝票番号
           MOVE    WRK-H-DENPNUM       TO  WRK-NO
           MOVE    WRK-NO              TO  HCM04A5-NO
      *    診療科
           MOVE    WRK-SRYKA-MEI       TO  HCM04A5-SRYKANAME
      *    患者番号
           MOVE    SPA-PTNUM           TO  HCM04A5-PTNUM
      *    患者名称
           MOVE    SPA-NAME            TO  HCM04A5-NAME
      *    保険種別
           MOVE    WRK-HKNCOMBIMEI     TO  HCM04A5-HOKEN
      *    負担割合
           MOVE    WRK-FUTAN           TO  HCM04A5-FUTAN
           MOVE    WRK-FUTAN-MEI       TO  HCM04A5-FUTANLABEL
      *    診療日
           MOVE    WRK-LBLSRYYMD       TO  HCM04A5-LBLSRYYMD
           MOVE    WRK-H-SRYYMD        TO  HCM04A5-SRYYMD
      *    発行日
           MOVE    WRK-H-HAKYMD        TO  HCM04A5-HAKYMD
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS        TO  HCM04A5-IRYOADRS
      *    医療機関名
           MOVE    WRK-HOSPNAME        TO  HCM04A5-IRYOMEI
      *    医療機関ＴＥＬ
           MOVE    WRK-HOSPTEL         TO  HCM04A5-IRYOTEL
      *
      *    列名（点数記載）
           IF      PARA-GOKEITANI      =   "1"
               MOVE    "　点　数　"        TO  HCM04A5-LABEL2
           ELSE
               MOVE    "　単　価　"        TO  HCM04A5-LABEL2
           END-IF
           MOVE    "　　合　　計　　"  TO  HCM04A5-LABEL
      *
      *    明細内容
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   LINE-MAX2
               MOVE    TBL2-H-SRYKBN (IDX IDX2)
                                          TO  HCM04A5-SRYKBN (IDX2)
               MOVE    TBL2-H-SRYMEI (IDX IDX2)
                                          TO  HCM04A5-SRYMEI (IDX2)
               IF      TBL2-H-KAISU  (IDX IDX2)    =   "---"
                   MOVE    ALL "―"           TO  HCM04A5-KAISU2(IDX2)
               ELSE
                   MOVE    TBL2-H-KAISU  (IDX IDX2)
                                              TO  HCM04A5-KAISU (IDX2)
                   MOVE    TBL2-H-KINGAKU(IDX IDX2)
                                              TO  HCM04A5-KINGAKU(IDX2)
                   MOVE    TBL2-H-TENSU  (IDX IDX2)
                                              TO  HCM04A5-TENSU  (IDX2)
               END-IF
      *        点数記載で円のもの
               IF      PARA-GOKEITANI      =   "1"
                   IF      TBL2-H-EN (IDX IDX2)    =   "1"
                       MOVE    SPACE           TO  HCM04A5-TENSU (IDX2)
                       STRING  TBL2-H-TENSU2(IDX IDX2)
                               "　"
                                           DELIMITED  BY  SIZE
                                       INTO   HCM04A5-TENSU (IDX2)
                       END-STRING
                       MOVE    "円"            TO  HCM04A5-TENSU2(IDX2)
      *
                       MOVE    SPACE           TO  HCM04A5-KINGAKU(IDX2)
                       STRING  TBL2-H-KINGAKU2(IDX IDX2)
                               "　"
                                           DELIMITED  BY  SIZE
                                       INTO   HCM04A5-KINGAKU(IDX2)
                       END-STRING
                       MOVE    "円"           TO  HCM04A5-KINGAKU2(IDX2)
                   END-IF
               END-IF
           END-PERFORM
      *
           EVALUATE   PARA-SHADE
               WHEN    "0"
      *            網掛けなし
                   STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                           " -L L1  -L L2 -L L3 "
                                               DELIMITED  BY  SIZE
                           "MonpeLayerOut"     DELIMITED  BY  SIZE
                                               INTO    HCM04A5-LOPTION
                   END-STRING
               WHEN    "1"
      *            網掛の濃さ１
                   STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                           " -L L2  -L L3 "
                                               DELIMITED  BY  SIZE
                           "MonpeLayerOut"     DELIMITED  BY  SIZE
                                               INTO    HCM04A5-LOPTION
                   END-STRING
               WHEN    "2"
      *            網掛の濃さ２
                   STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                           " -L L1  -L L3 "
                                               DELIMITED  BY  SIZE
                           "MonpeLayerOut"     DELIMITED  BY  SIZE
                                               INTO    HCM04A5-LOPTION
                   END-STRING
               WHEN    OTHER
      *            網掛濃さ３（標準）
                   STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                           " -L L1  -L L2 "
                                               DELIMITED  BY  SIZE
                           "MonpeLayerOut"     DELIMITED  BY  SIZE
                                               INTO    HCM04A5-LOPTION
                   END-STRING
           END-EVALUATE
      *
           .
       300-HCM04A5-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *
      *----(03.04.01)--ADD-START---
      *****************************************************************
      *    医療機関編集情報読み込み
      *****************************************************************
       900-1900-READ-SEC           SECTION.
      *
      *    医療機関編集情報読み込み
           MOVE    SPACE           TO  SYS-1900-REC
           INITIALIZE                  SYS-1900-REC
           MOVE    "1900"          TO  SYS-1900-KANRICD
           MOVE    "*"             TO  SYS-1900-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-1900-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1900-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1900-HOSPNUM
           MOVE    SYS-1900-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(12) NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-1902-READ-SEC
               END-IF
           ELSE
               INITIALIZE                  SYS-1900-REC
           END-IF
           .
      *
       900-1900-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関名称編集情報読み込み
      *****************************************************************
       900-1901-READ-SEC           SECTION.
      *
      *    医療機関名称編集情報読み込み
           MOVE    SPACE           TO  SYS-1901-REC
           INITIALIZE                  SYS-1901-REC
           MOVE    "1901"          TO  SYS-1901-KANRICD
           MOVE    SYS-1900-PRTKBN(12)
                                   TO  SYS-1901-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-1901-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1901-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1901-HOSPNUM
           MOVE    SYS-1901-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1  TO  WRK-HOSPNAME1
               MOVE    SYS-1901-HOSPNAME2  TO  WRK-HOSPNAME2
           ELSE
               INITIALIZE                  SYS-1901-REC
           END-IF
           .
      *
       900-1901-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関住所編集情報読み込み
      *****************************************************************
       900-1902-READ-SEC           SECTION.
      *
      *    医療機関住所編集情報読み込み
           MOVE    SPACE           TO  SYS-1902-REC
           INITIALIZE                  SYS-1902-REC
           MOVE    "1902"          TO  SYS-1902-KANRICD
           MOVE    SYS-1900-PRTKBN(12)
                                   TO  SYS-1902-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-1902-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1902-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
           MOVE    SYS-1902-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC    TO  SYS-1902-REC
               MOVE    SYS-1902-ADRS1  TO  WRK-HOSPADRS1
               MOVE    SYS-1902-ADRS2  TO  WRK-HOSPADRS2
               MOVE    SYS-1902-TEL    TO  WRK-HOSPTEL
           ELSE
               INITIALIZE                  SYS-1902-REC
           END-IF
           .
      *
       900-1902-READ-EXT.
           EXIT.
      *
      *----(03.04.01)--ADD-END-----
      *****************************************************************
      *    システム管理マスタ主キー読み込み処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEC         SECTION.
      *
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-KEY10-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       980-SYUNOU-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
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
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
           .
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
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           .
       300-END-EXT.
           EXIT.
      *
