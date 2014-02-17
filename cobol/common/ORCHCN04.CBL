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
       PROGRAM-ID.             ORCHCN04.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 入院診療費明細書　印刷（ＨＣＮ０４）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/04/12    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  06/07/07  領収書変更対応
      *  03.04.01    NACL-森脇    07/01/17  医療機関名称、住所セット変更
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  04.01.00    NACL-多々納  07/10/23  公務災害対応
      *  04.01.00    NACL-多々納  07/11/22  器材商品名コード対応
      *  04.01.00    NACL-太田    08/01/30  同日再入院対応
      *  04.02.00    NACL-多々納  08/03/06  Ｈ２０年４月改正対応
      *  04.02.00    NACL-太田    08/06/02  労災・自賠責入院対応
      *  04.03.00    NACL-太田    08/09/17  改正対応
      *  04.03.00    NACL-太田    08/12/25  入院料包括対応（対象外判定追加）
      *  04.05.00    NACL-多々納  09/06/XX  数量小数５桁対応
      *  04.05.00    NACL-多々納  10/03/XX  平成２２年４月改正対応
      *  04.05.00    NACL-太田    10/03/11  平成２２年４月改正対応
      *  04.06.00    NACL-多々納  10/12/01  自賠責金額入力対応
      *  04.07.00    NACL-多々納  11/10/XX  同日再入院対応
      *  04.07.00    NACL-多々納  11/12/22  商品名コード単価対応
      *  04.07.00    NACL-多々納  14/02/13  労災金額％加算対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
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
       01  PRT-REC                 PIC X(30000).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "HCN04.INC".
      *
      *     印刷用データ 名称領域 
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
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
      *
           03  FLG-SP              PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-PLUSKBN         PIC 9(01).
           03  FLG-RSIKSN          PIC 9(01).
           03  FLG-MEISAI          PIC 9(01).
           03  FLG-LINE            PIC 9(01).
      *
           03  FLG-RSIKIN          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDXA                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
      *
           03  IDX-JYU             PIC 9(04).
           03  IDX-REN             PIC 9(04).
           03  IDX-ACT             PIC 9(02).
           03  IDX-S               PIC 9(04).
           03  IDX-H               PIC 9(04).
           03  IDX-SYU             PIC 9(04).
           03  IDX-HZN             PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDXD                PIC 9(04).
           03  IDXM                PIC 9(04).
           03  IDXJ                PIC 9(04).
           03  IDXM2               PIC 9(04).
      *
           03  IDX-STR             PIC 9(02).
           03  IDX-END             PIC 9(02).
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
           03  WRK-SRYYMD              PIC X(08).
      *
           03  WRK-ROOM                PIC X(50).
           03  WRK-ROOMNUM-NAME        PIC X(10).
           03  WRK-BTUNUM              PIC X(02).
           03  WRK-BRMNUM              PIC X(06).
      *
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-MEI           PIC X(30).
           03  WRK-PAGE                PIC 9(03).
           03  WRK-PAGE-CNT            PIC 9(03).
           03  WRK-PAGE-MAX            PIC 9(03).
      *
           03  WRK-HEAD-SRYKA-MEI      PIC X(30).
           03  WRK-HEAD-HKNCOMBIMEI    PIC X(40).
           03  WRK-HEAD-FUTAN          PIC X(03).
           03  WRK-HEAD-FUTAN-MEI      PIC X(02).
           03  WRK-HEAD-HAKYMD         PIC X(22).
           03  WRK-HEAD-NYUINSTRYMD    PIC X(22).
           03  WRK-HEAD-NYUINENDYMD    PIC X(22).
           03  WRK-HEAD-DENPNUM        PIC 9(07).
      *
           03  WRK-KEY-AREA.
               05  WRK-KEY-SRYYM           PIC X(06).
               05  WRK-KEY-HKNCOMBINUM     PIC X(04).
      *
           03  WRK-KAISU               PIC 9(03).
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-SRYSYUKBN           PIC X(03).
           03  WRK-TENSU               PIC S9(11).
           03  WRK-KINGAKU             PIC S9(11).
           03  WRK-SRYMEI              PIC X(10).
           03  WRK-SRYMEI-OLD          PIC X(10).
           03  WRK-SRYCD               PIC X(09).
      *
           03  WRK-Z119Z               PIC ZZZZZZZZ9.
           03  WRK-Z119Z-2             PIC --------9.
      *
           03  WRK-Z119                PIC ZZZ,ZZZ,ZZ9.
           03  WRK-Z119-2              PIC ---,---,--9.
           03  WRK-Z7                  PIC ZZZZZZZZ9.
           03  WRK-Z7-2                PIC --------9.
           03  WRK-ZZ9                 PIC ZZZ.
           03  WRK-NO                  PIC Z(7).
           03  WRK-WARI                PIC 9(02)V9.
           03  WRK-Z6                  PIC ZZ9.
           03  WRK-Z62                 PIC 9.9.
      *
           03  WRK-HKNCOMBIMEI         PIC X(40).
           03  WRK-HEN-SRYYM           PIC X(16).
      *
           03  WRK-DENPNUM             PIC 9(07).
           03  WRK-GRP-DENPNUM         PIC 9(07).
      *ver.4.7
      *    同日再入院区分
           03  FLG-DOUJITU             PIC 9(01).
           03  WRK-NYUINYMD.
               05  WRK-NYUINYM         PIC X(06).
               05  WRK-NYUINDD         PIC 9(02).
           03  WRK-DOUDAY              PIC 9(02).
      *    商品名単位名称
           03  WRK-SYO-TANINAME        PIC X(12).
           03  WRK-SYO-TEN             PIC 9(07)V99.
      *
           03  WRK-SUM-TENSU                   PIC 9(09)V99.
           03  WRK-SUM-KINGAKU                 PIC 9(09).
      *
           03  WRK-H-NAME.
               05  WRK-H-NAME1             PIC X(74).
               05  WRK-H-NAME2             PIC X(74).
           03  WRK-H-NAME-R             REDEFINES  WRK-H-NAME.
               05  WRK-H-NAME2-1           PIC X(68).
               05  WRK-H-NAME2-2           PIC X(32).
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
      *    数量文字変換
           03  WRK-ZENKAKU-G           PIC X(100).
           03  WRK-MOJI                PIC X(100).
           03  WRK-MOJISU              PIC 9(02).
           03  WRK-Z-SURYO             PIC X(11).
           03  WRK-K-SURYO             PIC X(22).
           03  WRK-ZENHEN-SURYO        PIC X(80).
           03  WRK-H-SURYO             PIC X(80).
      *
           03  WRK-HOSPNAME            PIC X(100).
           03  WRK-HOSPNAME-R          REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1       PIC X(50).
               05  WRK-HOSPNAME2       PIC X(50).
           03  WRK-HOSPADRS            PIC X(100).
           03  WRK-HOSPADRS-R          REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1       PIC X(50).
               05  WRK-HOSPADRS2       PIC X(50).
           03  WRK-HOSPTEL             PIC X(15).
      *
       01  TBL-DATA-AREA.
           03  TBL-SRYKBN              PIC X(02).
           03  TBL-SRYSYUKBN           PIC X(03).
           03  TBL-ZAITENSU            PIC S9(07).
           03  TBL-RSIKINGAKU          PIC S9(07).
           03  TBL-KINGAKU             PIC S9(08).
           03  TBL-ENKBN               PIC X(01).
           03  TBL-KAISU               PIC 9(03).
           03  TBL-ZAI-TBL             OCCURS   50.
               05  TBL-SRYCD           PIC X(09).
               05  TBL-NAME            PIC X(100).
               05  TBL-TANINAME        PIC X(10).
               05  TBL-SURYO           PIC 9(05)V9(5).
               05  TBL-JIHIMONEY       PIC S9(07).
               05  TBL-TEN             PIC 9(07).
               05  TBL-TENSIKIBETU     PIC 9(01).
      *    自費分
       01  TBL-JIHI-AREA.
           03  TBL-JIHI-TBL            OCCURS   300.
               05  TBL-JIHI-SRYKBN          PIC X(02).
               05  TBL-JIHI-SRYSYUKBN       PIC X(03).
               05  TBL-JIHI-KAISU           PIC 9(03).
               05  TBL-JIHI-SRYCD           PIC X(09).
               05  TBL-JIHI-NAME            PIC X(80).
               05  TBL-JIHI-JIHIMONEY       PIC S9(07).
           03  IDX-JIHI                PIC 9(04).
      *
      *
       01  TBL-MEISAI-AREA.
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
      *
      *帳票明細数
       01  LINE-MAX                    PIC 9(03)   VALUE   70.
       01  CONST-H180401               PIC X(08)   VALUE "20060401".
       01  CONST-H201001               PIC X(08)   VALUE "20081001".
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
           03  PARA-HAKKOUBIKBN        PIC X(01).
           03  PARA-ROOMKBN            PIC X(01).
           03  PARA-KANANAME           PIC X(01).
      *
           03  WRK-PARA-NAME           PIC X(25).
           03  WRK-PARA                PIC X(50).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
      *    医療機関情報−基本情報 
           COPY    "CPSK1001.INC".
      *    医療機関情報−所在地、連絡先 
           COPY    "CPSK1002.INC".
      *
           COPY    "CPSK1005.INC".
           COPY    "CPSK5001.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1034.INC".
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
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
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
      *    入院履歴
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
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *    病室名称編集
           COPY    "CPORCSBRMNUM.INC".
      *
      *    入院会計作成パラ
           COPY    "CPORCHCM34S01.INC".
      *
      *    労災入院診療コードチェックサブ
           COPY    "CPORCHCM34S03.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    入院請求期間取得サブ
           COPY    "CPORCSS006.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *
           COPY    "MCPDATA.INC".
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    保険組合せ名称編集サブ
           COPY    "CPORCSHKNMEI.INC".
      *
      *    請求期間編集サブ
           COPY    "CPORCHCN03S03.INC".
      *
           COPY    "MCPAREA".
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
           COPY    "CPORCHCN04.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCHCN04AREA
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
           MOVE    ZERO           TO   ORCHCN04-PAGE
      *
           IF    ( ORCHCN04-PAGE-CNT   IS  NOT NUMERIC )
               MOVE    ZERO       TO   WRK-PAGE-CNT
           ELSE
               MOVE    ORCHCN04-PAGE-CNT
                                  TO   WRK-PAGE-CNT
           END-IF
      *
           ACCEPT  SYS-TIME            FROM    TIME
      *
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
      *    各ＤＢ読込
           PERFORM 100-SYORI-SEC
      *    発行処理
           PERFORM 200-MAIN-SEC
      *
           MOVE    WRK-PAGE-CNT        TO   ORCHCN04-PAGE-CNT
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
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
      *    行間
           MOVE    "LINE"              TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-LINE
      *    伝票発行日区分
           MOVE    "HAKKOUBIKBN"       TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-HAKKOUBIKBN
           IF      PARA-HAKKOUBIKBN    =   SPACE
               MOVE    "0"                 TO  PARA-HAKKOUBIKBN
           END-IF
      *    病棟病室区分
           MOVE    "ROOMKBN"           TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-ROOMKBN
           IF      PARA-ROOMKBN        =   SPACE
               MOVE    "1"                 TO  PARA-ROOMKBN
           END-IF
      *    カナ氏名
           MOVE    "KANANAME"          TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-KANANAME
           IF      PARA-KANANAME       =   SPACE
               MOVE    "0"                 TO  PARA-KANANAME
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
           MOVE    "ORCHCN04"          TO  ORCSPRGOPTION-PRGID
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
           MOVE    SPA-SYSYMD          TO  SYS-1001-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1001-EDYUKYMD
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
           MOVE    SPA-SYSYMD          TO  SYS-1002-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1002-EDYUKYMD
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
           INITIALIZE                      SYS-1034-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "1"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1034-REC
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
           MOVE   HCN04            TO  PRT-REC
           WRITE  PRT-REC
      *
           CLOSE                       PRT-FILE
      *
           INITIALIZE                 ORCSMKPRTAREA
           MOVE   "HC01.sh"           TO  MKPRT-ID
           MOVE   "HCN04.red"         TO  MKPRT-DIA
           MOVE   SPACE               TO  MKPRT-DEF
           MOVE   HC01PARA            TO  MKPRT-DAT
           MOVE   SYS-1034-PRTNM(09)  TO  MKPRT-PRTNM
           MOVE   "入院診療費明細書"  TO  MKPRT-INFO
           CALL   "ORCSMKPRT1"        USING
                                      ORCSMKPRTAREA
                                      SPA-AREA
      *
           .
       400-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理 (印刷ＤＢに出力）
      *****************************************************************
       401-PRINT-SEC                SECTION.
      *
           MOVE    ORCHCN04-PRTAREA    TO  ORCSPRTAREA
      *
           MOVE    "HCN04.red"         TO  SPRT-PRTID
           MOVE    HCN04               TO  SPRT-PRTDATA
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
      *
      *****************************************************************
      *   出　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    初期
           MOVE    ZERO                TO  WRK-PAGE
           MOVE    SPACE               TO  WRK-SRYMEI-OLD
           INITIALIZE                  TBL-MEISAI-AREA
           MOVE    ZERO                TO  IDX-H
      *
           PERFORM VARYING IDX-SYU     FROM    1   BY  1
                   UNTIL   (IDX-SYU    >   10  )   OR
                           (ORCHCN04-DENPNUM (IDX-SYU) =   ZERO)
               MOVE    ORCHCN04-DENPNUM (IDX-SYU)  TO  WRK-DENPNUM
      *        収納検索
               PERFORM 2001-SYUNOU-HEN-SEC
               IF      IDX-SYU             =   1
      *            病棟他ＤＢ検索
                   PERFORM 2002-DB-KENSAKU-SEC
               ELSE
      *            伝票毎の処理
                   PERFORM 2003-DENPNUM-HEN-SEC
               END-IF
      *        伝票番号毎の印刷
               PERFORM 200-MAIN-SYORI-SEC
           END-PERFORM
      *    最終行
           IF      IDX-H           >   ZERO
               PERFORM 2006-LAST-PRINT-SEC
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *   伝票番号毎の印刷処理
      *****************************************************************
       200-MAIN-SYORI-SEC                SECTION.
      *
      *    入院基本料編集
           PERFORM 2002-NYUIN-HEN-SEC
      *    診療行為印刷
           PERFORM 2003-SRYACCT-SYORI-SEC
      *    入院料印刷
           PERFORM 2004-NYUIN-SYORI-SEC
      *    自費分印刷
           PERFORM 2005-JIHI-SYORI-SEC
      *
           .
       200-MAIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納検索処理
      *****************************************************************
       2001-SYUNOU-HEN-SEC             SECTION.
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    "1"                 TO  SYU-NYUGAIKBN
           MOVE    ORCHCN04-PTID       TO  SYU-PTID
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
       2001-SYUNOU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   請求期間編集処理
      *****************************************************************
       20021-INIT-HENSYU-SEC          SECTION.
      *
           INITIALIZE                  ORCHCN03S03AREA
           MOVE    ORCHCN04-PTID   TO  ORCHCN03S03-PTID
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   10  )
                      OR ( ORCHCN04-DENPNUM (IDXD) =   ZERO )
               MOVE    ORCHCN04-DENPNUM (IDXD)
                                   TO  ORCHCN03S03-DENPNUM (IDXD)
           END-PERFORM
           IF      ORCHCN04-SKYSTYMD   =   SPACE
               MOVE    SYU-SKYSTYMD        TO  ORCHCN03S03-SYUSTYMD
           ELSE
               MOVE    ORCHCN04-SKYSTYMD   TO  ORCHCN03S03-SYUSTYMD
           END-IF

           IF      ORCHCN04-SKYEDYMD   =   SPACE
               MOVE    SYU-SKYEDYMD        TO  ORCHCN03S03-SYUEDYMD
           ELSE
               MOVE    ORCHCN04-SKYEDYMD   TO  ORCHCN03S03-SYUEDYMD
           END-IF
           CALL    "ORCHCN03S03"       USING
                                       SPA-AREA
                                       ORCHCN03S03AREA
      *
           MOVE    SYU-DENPNUM         TO  WRK-HEAD-DENPNUM
      *    発行日
           IF     (ORCHCN04-DENPPRTYMD NOT =   SPACE )
               MOVE    ORCHCN04-DENPPRTYMD
                                           TO  WRK-SYMD
           ELSE
               IF     (PARA-HAKKOUBIKBN    =   "1" )
                 AND  (ORCHCN04-HAKKOFLG   =    1   OR   3 )
                   MOVE    SPA-SYSYMD      TO  WRK-SYMD
               ELSE
                   MOVE    SYU-DENPPRTYMD  TO  WRK-SYMD
               END-IF
           END-IF
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-HEAD-HAKYMD
      *    請求開始日
           MOVE    ORCHCN03S03-SKYSTYMD TO WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-HEAD-NYUINSTRYMD
      *
           MOVE    ORCHCN03S03-SKYEDYMD TO WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-HEAD-NYUINENDYMD
      *
           .
       20021-INIT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *   病棟他ＤＢ検索処理
      *****************************************************************
       2002-DB-KENSAKU-SEC          SECTION.
      *
      *    請求期間編集
           PERFORM 20021-INIT-HENSYU-SEC
      *
           MOVE    SYU-SRYYMD(1:6)     TO  WRK-KEY-SRYYM
           MOVE    SYU-HKNCOMBINUM     TO  WRK-KEY-HKNCOMBINUM
      *
           MOVE    SPACE               TO  WRK-HEAD-SRYKA-MEI
           MOVE    SPACE               TO  WRK-HEAD-HKNCOMBIMEI
      *    診療科名編集
           IF     (PARA-SRYKA          =   "0" )
               MOVE    SPACE               TO  WRK-SRYKA-MEI
           ELSE
               PERFORM 20011-SRYKAMEI-HEN-SEC
               MOVE    WRK-SRYKA-MEI       TO  WRK-HEAD-SRYKA-MEI
           END-IF
      *    保険組合せ
           PERFORM 20012-HKNCOMBI-HEN-SEC
           MOVE    WRK-HKNCOMBIMEI     TO  WRK-HEAD-HKNCOMBIMEI
      *    負担割合
           MOVE    SPACE               TO  WRK-HEAD-FUTAN
           MOVE    SPACE               TO  WRK-HEAD-FUTAN-MEI
      *
           EVALUATE    PARA-FTNKBN
               WHEN    "0"
                   MOVE    SPACE               TO  WRK-HEAD-FUTAN
               WHEN    "1"
      *            %　表示
                   MOVE    SYU-PTFTNRATE       TO  WRK-Z6
                   MOVE    WRK-Z6              TO  WRK-HEAD-FUTAN
                   MOVE    "％"                TO
                                                   WRK-HEAD-FUTAN-MEI
               WHEN    "2"
      *            割　表示
                   IF      SYU-PTFTNRATE       >   ZERO
                       COMPUTE WRK-WARI        =   SYU-PTFTNRATE
                                               /   10
                       IF      SYU-PTFTNRATE(3:1)      =   ZERO
                           MOVE    WRK-WARI    TO  WRK-Z6
                           MOVE    WRK-Z6      TO  WRK-HEAD-FUTAN
                       ELSE
                           MOVE    WRK-WARI    TO  WRK-Z62
                           MOVE    WRK-Z62     TO  WRK-HEAD-FUTAN
                       END-IF
                   ELSE
                       MOVE    SYU-PTFTNRATE   TO  WRK-Z6
                       MOVE    WRK-Z6          TO  WRK-HEAD-FUTAN
                   END-IF
                   MOVE    "割"            TO  WRK-HEAD-FUTAN-MEI
           END-EVALUATE
      *    病棟・病室
           IF      PARA-ROOMKBN        =   "1"
               PERFORM 20013-BYOTO-HEN-SEC
           ELSE
               MOVE    SPACE               TO  WRK-ROOM
           END-IF
           .
       2002-DB-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       20012-HKNCOMBI-HEN-SEC         SECTION.
      *
      *    編集なし
           IF     (PARA-HKNMEI         =   "0" )
               MOVE    SPACE               TO  WRK-HKNCOMBIMEI
           ELSE
               INITIALIZE                  ORCSHKNMEIAREA
               MOVE    SYU-HKNCOMBINUM     TO  ORCSHKNMEI-HKNCOMBINUM
               MOVE    SYU-SRYYMD          TO  ORCSHKNMEI-SRYYMD
               MOVE    ORCHCN04-PTID       TO  ORCSHKNMEI-PTID
               CALL    "ORCSHKNMEI"        USING
                                           SPA-AREA
                                           ORCSHKNMEIAREA
               MOVE    ORCSHKNMEI-HKNCOMBIMEI  TO  WRK-HKNCOMBIMEI
           END-IF
           .
       20012-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科名編集処理
      *****************************************************************
       20011-SRYKAMEI-HEN-SEC                SECTION.
      *
      *    診療科
           MOVE    SPACE               TO  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SYU-SRYKA           TO  SYS-1005-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1005-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME1
                                           TO  WRK-SRYKA-MEI
           ELSE
               INITIALIZE                  SYS-1005-REC
           END-IF
      *
           .
       20011-SRYKAMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    病棟・病室編集処理
      *****************************************************************
       20013-BYOTO-HEN-SEC                SECTION.
      *
           MOVE    SYU-BYOTONUM        TO  WRK-BTUNUM
           MOVE    SYU-ROOMNUM (3:)    TO  WRK-BRMNUM
      *
      *    入院履歴検索処理
           PERFORM 900-PTNYUINRRK-KEY45-SEL-SEC
           IF    ( FLG-PTNYUINRRK  =     ZERO )
               MOVE    PTNYUINRRK-BTUNUM
                                       TO  WRK-BTUNUM
               MOVE    PTNYUINRRK-BRMNUM (3:)
                                       TO  WRK-BRMNUM
           END-IF
      *    病棟情報検索
           MOVE    SPACE               TO  SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    WRK-BTUNUM          TO  SYS-5001-KBNCD
           MOVE    SYU-SKYSTYMD        TO  SYS-5001-STYUKYMD
           MOVE    SYU-SKYSTYMD        TO  SYS-5001-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-5001-REC
           ELSE
               INITIALIZE                  SYS-5001-REC
           END-IF
      *    病室名編集処理
           MOVE    SPACE               TO  WRK-ROOMNUM-NAME
           INITIALIZE                      ORCSBRMNUMAREA
           MOVE    "1"                 TO  SBRMNUM-IN-PROCKBN
           MOVE    WRK-BRMNUM          TO  SBRMNUM-IN-BRMNUM
           CALL    "ORCSBRMNUM"        USING    ORCSBRMNUMAREA
           IF       SBRMNUM-RC         =   ZERO
      *        全角編集処理
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI
               MOVE    SBRMNUM-OT-BRMNUM   TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF      KANACHK-RC          =   ZERO
                   MOVE    KANACHK-OUT-INPUT
                                           TO  WRK-ROOMNUM-NAME
               END-IF
           END-IF
           MOVE    SPACE               TO  WRK-ROOM
           IF      WRK-ROOMNUM-NAME    =   SPACE
               STRING  SYS-5001-BTU-NAME   DELIMITED   BY  SPACE
                       "　　　　"          DELIMITED   BY  SIZE
                       "号室"              DELIMITED   BY  SIZE
                                       INTO    WRK-ROOM
               END-STRING
           ELSE
               STRING  SYS-5001-BTU-NAME   DELIMITED   BY  SPACE
                       "　"                DELIMITED   BY  SIZE
                       WRK-ROOMNUM-NAME    DELIMITED   BY  SPACE
                       "号室"              DELIMITED   BY  SIZE
                                       INTO    WRK-ROOM
               END-STRING
           END-IF
      *
           .
       20013-BYOTO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   伝票番号毎の編集処理
      *****************************************************************
       2003-DENPNUM-HEN-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-HEN-SRYYM
           MOVE    SPACE               TO  WRK-HKNCOMBIMEI
      *    保険組合せ
           IF      WRK-KEY-HKNCOMBINUM NOT =   SYU-HKNCOMBINUM
                PERFORM 20012-HKNCOMBI-HEN-SEC
           END-IF
      *    診療月
           IF      WRK-KEY-SRYYM       NOT =   SYU-SRYYMD(1:6)
               MOVE    SYU-SRYYMD          TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG (1:16)  TO  WRK-HEN-SRYYM
           END-IF
      *    剤毎に１行開ける
           IF     (PARA-LINE           =   "1" )
             AND  (IDX-H               >   ZERO )
               ADD     2                   TO  IDX-H
           ELSE
               ADD     1                   TO  IDX-H
           END-IF
           IF      IDX-H               >=  LINE-MAX
               PERFORM 200520-PAGE-CNT-SEC
           END-IF
           MOVE    ALL "―"            TO  TBL-H-SRYKBN (IDX-H)
           MOVE    ALL "-"             TO  TBL-H-KAISU  (IDX-H)
           MOVE    ALL "-"             TO  TBL-H-KINGAKU(IDX-H)
           EVALUATE    TRUE
               WHEN   (WRK-HEN-SRYYM   NOT =   SPACE )  AND
                      (WRK-HKNCOMBIMEI NOT =   SPACE )
                   STRING    "〔"
                             WRK-HEN-SRYYM
                             "〕　" 
                             "【"
                             WRK-HKNCOMBIMEI
                             "】"              DELIMITED  BY  SPACE
                                   INTO    TBL-H-SRYMEI(IDX-H)
                   END-STRING
               WHEN   (WRK-HEN-SRYYM   NOT =   SPACE )  AND
                      (WRK-HKNCOMBIMEI     =   SPACE )
                   STRING    "〔"
                             WRK-HEN-SRYYM
                             "〕"              DELIMITED  BY  SPACE
                                   INTO    TBL-H-SRYMEI(IDX-H)
                   END-STRING
               WHEN   (WRK-HEN-SRYYM       =   SPACE )  AND
                      (WRK-HKNCOMBIMEI NOT =   SPACE )
                   STRING    "【"
                             WRK-HKNCOMBIMEI
                             "】"              DELIMITED  BY  SPACE
                                   INTO    TBL-H-SRYMEI(IDX-H)
                   END-STRING
           END-EVALUATE
      *
           INSPECT TBL-H-SRYMEI(IDX-H)
                           REPLACING    ALL    "  "   BY  "―"
      *
           MOVE    SYU-SRYYMD(1:6)     TO  WRK-KEY-SRYYM
           MOVE    SYU-HKNCOMBINUM     TO  WRK-KEY-HKNCOMBINUM
           .
       2003-DENPNUM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療行為印刷処理
      *****************************************************************
       2003-SRYACCT-SYORI-SEC          SECTION.
      *
      *    対象期間
           MOVE    SYU-SKYSTYMD(7:2)   TO  IDX-STR
           MOVE    SYU-SKYEDYMD(7:2)   TO  IDX-END
           MOVE    SYU-SKYSTYMD        TO  WRK-SRYYMD
      *ver.4.7
           MOVE    ZERO                TO  WRK-DOUDAY
           IF      FLG-DOUJITU         =   1
               IF      WRK-NYUINYM     =   SYU-SRYYMD(1:6)
                   MOVE    WRK-NYUINDD     TO  WRK-DOUDAY
               END-IF
           END-IF
      *
      *
           INITIALIZE                      TBL-JIHI-AREA
      *    診療会計
           INITIALIZE                      SRYACCT-REC
           MOVE    SYU-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SYU-PTID            TO  ACCT-PTID
           MOVE    SYU-SRYYMD(1:6)     TO  ACCT-SRYYM
           MOVE    SYU-HKNCOMBINUM     TO  ACCT-HKNCOMBI
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key56"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key56"             TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           PERFORM
                   UNTIL    FLG-SRYACCT    =   1
      *        対象期間の回数
               MOVE    ZERO                TO  WRK-KAISU
               PERFORM VARYING     IDX     FROM    IDX-STR   BY  1
                       UNTIL       IDX     >   IDX-END
                   IF      WRK-DOUDAY          =   IDX
      *                同日再入院分
                       ADD     ACCT-DAY (3 IDX)    TO  WRK-KAISU
                   ELSE
                       ADD     ACCT-DAY (2 IDX)    TO  WRK-KAISU
                   END-IF
               END-PERFORM
      *
               IF    ( WRK-KAISU           =   ZERO )
      ****      OR   ( ACCT-ZAITEN         =   ZERO )
                OR   ( ACCT-SRYKBN         =   "98" OR "99" )
                OR   ( ACCT-HKNCOMBI       =   "9999")
                OR   ( ACCT-JIHOKBN        =   "1"   )
                   CONTINUE
               ELSE
                   PERFORM 20031-MEI-HENSYU-SEC
               END-IF
      *
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key56"             TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key56"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2003-SRYACCT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       20031-MEI-HENSYU-SEC          SECTION.
      *
           INITIALIZE                  TBL-DATA-AREA
           MOVE    ZERO                TO  WRK-SUM-TENSU
           MOVE    ZERO                TO  WRK-SUM-KINGAKU
      *
           INITIALIZE                  SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SRYACT-READ-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
      *    自費金額・点数＝ゼロは対象外
           IF     (ACCT-ZAITEN         =   ZERO )  AND
                  (SRY-JIHIMONEYTOTAL  =   ZERO )
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           MOVE    ZERO                TO  IDX-S
           MOVE    SRY-SRYCD(1)        TO  WRK-SRYCD
           PERFORM 2005-RSIKSN-CHK-SEC
           IF    ( FLG-RSIKSN          =   ZERO )
               PERFORM UNTIL   FLG-SRYACT      =   1
                   PERFORM VARYING     IDXA    FROM    1   BY  1
                           UNTIL      (IDXA    >   5   )   OR
                                      (SRY-SRYCD (IDXA)    =   SPACE)
      *                診療・薬剤・器材・部位・自費を対象とする
                       IF      (SRY-SRYCD (IDXA)(1:1)      =   "1"
                                                           OR  "6"
                                                           OR  "7" ) OR
                               (SRY-SRYCD (IDXA)(1:3)      =   "059"
                                                           OR  "002"
                                                           OR  "095"
                                                           OR  "096")
      *                器材商品名対象
                           OR  (SRY-SRYCD (IDXA)(1:3)      =   "058")
                           PERFORM 200321-MEISAI-HEN-SEC
                       END-IF
                   END-PERFORM
                   MOVE    "tbl_sryact"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 920-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    剤ごとに、編集する
           IF      IDX-S               >   ZERO
               IF      ACCT-SRYKBN         =   "95"   OR  "96"
      *            自費分
                   PERFORM 200322-JIHI-HENSYU-SEC
               ELSE
                   PERFORM 20023-TBL-HENSYU-SEC
               END-IF
           END-IF
           .
       20031-MEI-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *        明細編集処理
      *****************************************************************
       200321-MEISAI-HEN-SEC      SECTION.
      *
           MOVE    SRY-SRYCD (IDXA)    TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *
           ADD     1                   TO  IDX-S
           MOVE    SRY-SRYCD (IDXA)    TO  TBL-SRYCD (IDX-S)
      *    薬剤以外で帳票名があれば帳票名称
           IF     (SRY-SRYCD (IDXA)(1:1)   =   "6" )
              OR  (TNSP-SHONAME        =   SPACE )
               CONTINUE
           ELSE
               MOVE    TNSP-SHONAME        TO  TNS-NAME
           END-IF
           MOVE    TNS-NAME            TO  TBL-NAME  (IDX-S)
      *    器材商品名コード
           IF      SRY-SRYCD (IDXA)(1:3)   =   "058"
               MOVE   TNS-FORMALNAME       TO  TBL-NAME (IDX-S)
      *ver.4.7 単位名称
               MOVE    TNS-TANINAME        TO  WRK-SYO-TANINAME
               MOVE    TNS-TEN             TO  WRK-SYO-TEN
               MOVE    SPACE               TO  TNS-TANINAME
               MOVE    ZERO                TO  TNS-TANICD
           END-IF
      *    器材商品名の器材コード
           IF     (SRY-SRYCD (IDXA)(1:1)   =   "7" ) AND
                  (SRY-MEISKYFLG  (IDXA)   =   "1" )
               MOVE    SPACE               TO  TBL-NAME  (IDX-S)
               STRING     "〔"             DELIMITED   BY  SIZE
                          TNS-NAME         DELIMITED   BY  "    "
                          "〕"             DELIMITED   BY  SIZE
                          INTO             TBL-NAME  (IDX-S)
               END-STRING
      *ver.4.7
      *        商品名コードの単位へ
               IF      WRK-SYO-TANINAME    NOT =   SPACE
                   MOVE    WRK-SYO-TANINAME    TO  TNS-TANINAME
                   MOVE    SPACE               TO  WRK-SYO-TANINAME
               END-IF
               IF      WRK-SYO-TEN         >   ZERO
                   MOVE    WRK-SYO-TEN     TO  TNS-TEN
               END-IF
           END-IF
      *
      *    単位
           MOVE    TNS-TANINAME        TO  TBL-TANINAME(IDX-S)
      *    数量
           MOVE    SRY-SRYSURYO(IDXA)  TO  TBL-SURYO (IDX-S)
           IF     (TNS-TANICD          =   ZERO
                                       OR  SPACE )
      *        器材は単位なしでも表示
      *        IF     (SRY-SRYCD (IDXA)(1:1)   =   "7" )
      *          AND  (SRY-SRYSURYO(IDXA)      >   1   )
      *        器材、薬剤は単位なしでも表示（％加算、減算等は対象外）
               IF    ( SRY-SRYCD (IDXA)(1:1)   =   "6"
                                               OR  "7"  )  AND
                     ( TNS-TEN                 >   ZERO )  AND
                     ( TNS-TENSIKIBETU         =   1
                                               OR  2    )
                   CONTINUE
               ELSE
                   MOVE    ZERO            TO  TBL-SURYO (IDX-S)
               END-IF
           END-IF
      *    初診・再診料は明細毎の点数
           IF      SRY-SRYKBN          =   "95"
                                       OR  "96"
               CONTINUE
           ELSE
               IF     (TNS-TENSIKIBETU     =   3
                                           OR  1        )
               AND    (SRY-SRYCD (IDXA)(1:1)   =   "1"  )
                   MOVE    TNS-TEN             TO  TBL-TEN (IDX-S)
                   MOVE    TNS-TENSIKIBETU     TO  TBL-TENSIKIBETU
                                                            (IDX-S)
                   IF      TNS-TENSIKIBETU     =   3
                       ADD     TNS-TEN         TO  WRK-SUM-TENSU
                   ELSE
                       ADD     TNS-TEN         TO  WRK-SUM-KINGAKU
                   END-IF
               END-IF
      *        自賠責のその他
               IF     (SRY-SRYCD (IDXA)(1:4)   =   "0959" )
      *            自賠責金額入力を点数とする
                   IF     (SRY-SRYSYUKBN       =   "809")
                       MOVE    SRY-JIHIMONEY(IDXA)     TO  TNS-TEN
                   END-IF
                   MOVE    TNS-TEN             TO  TBL-TEN (IDX-S)
                   MOVE    TNS-TENSIKIBETU     TO  TBL-TENSIKIBETU
                                                            (IDX-S)
                   ADD     TNS-TEN             TO  WRK-SUM-KINGAKU
               END-IF
           END-IF
      *
           IF      SRY-SRYKBN          =   "95"
                                       OR  "96"
      *        自費は１行１明細とする
               COMPUTE TBL-JIHIMONEY(IDX-S)
                                       =   SRY-JIHIMONEY(IDXA)
      *****                            *   WRK-KAISU
               MOVE    ZERO                TO  TBL-SURYO (IDX-S)
           END-IF
           MOVE    SRY-SRYSYUKBN       TO  WRK-SRYSYUKBN
           .
       200321-MEISAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *        テーブル編集処理
      *****************************************************************
       20023-TBL-HENSYU-SEC      SECTION.
      *
           IF      ACCT-ZAIREQFLG      =   "1"
      *        附加情報あり
               PERFORM 200231-SRYACCTPLUS-CHK-SEC
           ELSE
               MOVE    ACCT-ZAITEN     TO  TBL-ZAITENSU
           END-IF
           MOVE    ACCT-SRYKBN         TO  TBL-SRYKBN 
           MOVE    WRK-SRYSYUKBN       TO  TBL-SRYSYUKBN
           MOVE    WRK-KAISU           TO  TBL-KAISU
      *    診療区分 ８０ の時、変更
           IF      ACCT-SRYKBN         =   "80"
               EVALUATE    WRK-SRYSYUKBN
                   WHEN    "800"
                       MOVE    "80"        TO  TBL-SRYKBN
                   WHEN    "820"
                       MOVE    "29"        TO  TBL-SRYKBN
                   WHEN    "830"
                       MOVE    "83"        TO  TBL-SRYKBN
                   WHEN    "840"
                       MOVE    "84"        TO  TBL-SRYKBN
                   WHEN    "850"
                       MOVE    "85"        TO  TBL-SRYKBN
                   WHEN    "890"
                       MOVE    "89"        TO  TBL-SRYKBN
                   WHEN    "809"
                       MOVE    "86"        TO  TBL-SRYKBN
                   WHEN    OTHER
                       MOVE    "80"        TO  TBL-SRYKBN
               END-EVALUATE
               IF     (WRK-SRYSYUKBN           =   "800"  )  AND
                      (TBL-SRYKBN              =   "80"   )
                   PERFORM VARYING IDX     FROM    1   BY  1
                           UNTIL   IDX     >   IDX-S
      *            自賠責ユーザコードは、その他とする
                       IF      TBL-SRYCD (IDX)(1:4)    =   "0959"
                           MOVE    "86"            TO  TBL-SRYKBN
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-MEISAI
           IF     (WRK-SUM-TENSU       NOT =   ZERO )  AND
                  (WRK-SUM-KINGAKU     NOT =   ZERO )
      *        金額・点数混在時は、明細
               MOVE    1               TO  FLG-MEISAI
               MOVE    ZERO            TO  TBL-ZAITENSU
           END-IF
           IF     (WRK-SUM-KINGAKU     =   ZERO )  AND
                  (WRK-SUM-TENSU       =   TBL-ZAITENSU)
      *        点数合計の時、初診・再診のみ明細
               IF      ACCT-SRYKBN         =   "11"
                                           OR  "12"
                   MOVE    1               TO  FLG-MEISAI
                   MOVE    ZERO            TO  TBL-ZAITENSU
               END-IF
           END-IF
           IF     (WRK-SUM-KINGAKU NOT =   ZERO )  AND
                  (WRK-SUM-TENSU       =   ZERO )
      *        自賠責の金額分
               MOVE    1               TO  FLG-MEISAI
               MOVE    ZERO            TO  TBL-ZAITENSU 
           END-IF
      *!!!
           MOVE    ZERO                TO  FLG-RSIKIN
      *    労災金額
           IF     (WRK-SUM-KINGAKU NOT =   ZERO )  AND
                  (WRK-SUM-TENSU       =   ZERO )  AND
                  (TBL-RSIKINGAKU  NOT =   ZERO )
               IF      ACCT-SRYKBN         =   "11"
                                           OR  "12"
                   CONTINUE
               ELSE
      *            初診・再診以外は１剤毎の金額とする 
                   MOVE    TBL-RSIKINGAKU  TO  TBL-ZAITENSU
                   MOVE    1               TO  FLG-RSIKIN
                   MOVE    ZERO            TO  FLG-MEISAI
               END-IF
           END-IF
      *
      *    金額
           IF     (TBL-SRYKBN          =   "97" )
      *        食事は金額
              OR  (FLG-RSIKIN          =   1    )
      *        労災金額
               COMPUTE TBL-KINGAKU         =  (TBL-ZAITENSU
                                           *   TBL-KAISU )
               MOVE    "1"                 TO  TBL-ENKBN
           ELSE
               MOVE    SPACE               TO  TBL-ENKBN
               IF      PARA-GOKEITANI      =   "1"
      *            点数表示
                   COMPUTE TBL-KINGAKU         =  (TBL-ZAITENSU
                                               *   TBL-KAISU )
               ELSE
      *            金額表示
                   COMPUTE TBL-KINGAKU         =  (TBL-ZAITENSU
                                               *   TBL-KAISU )
                                               *   10
               END-IF
           END-IF
      *
      *    帳票印刷
           PERFORM 2005-PRINT-SYORI-SEC
      *
           .
       20023-TBL-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    附加情報編集処理
      *****************************************************************
       200231-SRYACCTPLUS-CHK-SEC       SECTION.
      *
           MOVE    SPACE               TO  SRYACCTPLUS-REC
           INITIALIZE                  SRYACCTPLUS-REC
           MOVE    ACCT-HOSPNUM        TO  ACCTP-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  ACCTP-NYUGAIKBN
           MOVE    ACCT-PTID           TO  ACCTP-PTID
           MOVE    ACCT-SRYKA          TO  ACCTP-SRYKA
           MOVE    ACCT-SRYYM          TO  ACCTP-SRYYM
           MOVE    ACCT-ZAINUM         TO  ACCTP-ZAINUM
      *
           MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACCTPLUS-READ-SEC
           ELSE
               INITIALIZE                  SRYACCTPLUS-REC
               MOVE    1               TO  FLG-SRYACCTPLUS
           END-IF
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    ZERO                TO  FLG-PLUSKBN
           IF      FLG-SRYACCTPLUS     =   ZERO
               IF  ACCTP-PLUSKBN       =   1
      *            マイナス点数
                   COMPUTE TBL-ZAITENSU    =   ACCT-ZAITEN
                                           *   -1
               ELSE
                   MOVE    ACCT-ZAITEN     TO  TBL-ZAITENSU
               END-IF
      *!
      *        労災金額
               MOVE    ACCTP-RSIKINGAKU    TO  TBL-RSIKINGAKU
           ELSE
               MOVE    ACCT-ZAITEN     TO  TBL-ZAITENSU
           END-IF
      *
           .
       200231-SRYACCTPLUS-CHK-EXT.
           EXIT.
      *****************************************************************
      *    自費テーブル編集処理
      *****************************************************************
       200322-JIHI-HENSYU-SEC      SECTION.
      *
           PERFORM VARYING     IDXJ    FROM    1   BY  1
                   UNTIL      (IDXJ    >   50  )   OR
                              (TBL-SRYCD (IDXJ)    =   SPACE)
               ADD     1                   TO  IDX-JIHI
               IF      IDX-JIHI            >   300
                   DISPLAY "ORCHCN40 JIHI 300 OVER!!"
               ELSE
                   MOVE    ACCT-SRYKBN     TO  TBL-JIHI-SRYKBN(IDX-JIHI)
                   MOVE    WRK-KAISU       TO  TBL-JIHI-KAISU(IDX-JIHI)
                   MOVE    WRK-SRYSYUKBN   TO  TBL-JIHI-SRYSYUKBN
                                                            (IDX-JIHI)
                   MOVE    TBL-SRYCD (IDXJ)
                                           TO  TBL-JIHI-SRYCD
                                                            (IDX-JIHI)
                   MOVE    TBL-NAME (IDXJ)
                                           TO  TBL-JIHI-NAME
                                                            (IDX-JIHI)
                   MOVE    TBL-JIHIMONEY (IDXJ)
                                           TO  TBL-JIHI-JIHIMONEY
                                                            (IDX-JIHI)
               END-IF
           END-PERFORM
           .
       200322-JIHI-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       2005-PRINT-SYORI-SEC                SECTION.
      *
      *    帳票テーブル編集
           PERFORM 20052-TBL-HEN-SEC
      *
           .
       2005-PRINT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票テーブル編集処理
      *****************************************************************
       20052-TBL-HEN-SEC             SECTION.
      *
      *    剤毎に編集
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   50  )  OR
                              (TBL-SRYCD (IDX2)    =  SPACE)
               ADD     1               TO  IDX-H
               IF      IDX-H           >=  LINE-MAX
                   PERFORM 200520-PAGE-CNT-SEC
               END-IF
      *        数量
               IF      TBL-SURYO (IDX2)        >   ZERO
      *            数量＋単位名称（全角）
                   PERFORM 20052-SURYO-ZENHEN-SEC
                   MOVE    SPACE                   TO  WRK-H-NAME
                   STRING  TBL-NAME (IDX2)
                           "　　"
                           WRK-H-SURYO
                           DELIMITED  BY  SPACE
                                           INTO   WRK-H-NAME
                   END-STRING
               ELSE
                   MOVE    TBL-NAME     (IDX2)     TO  WRK-H-NAME
               END-IF
      *        名称と単位名称チェックの改行対応
               MOVE    ZERO                    TO  FLG-LINE
               IF    ( WRK-H-NAME2         NOT =   SPACE  )
                   MOVE    1                   TO  FLG-LINE
      *            最終行の時、改ページ
                   IF      IDX-H          >=  (LINE-MAX - 1 )
                       PERFORM 200520-PAGE-CNT-SEC
                   END-IF
               END-IF
               IF     IDX2                =   1
                   PERFORM 200521-SRYSYUMEI-SEC
                   IF      WRK-SRYMEI     NOT  =   WRK-SRYMEI-OLD
                       MOVE    WRK-SRYMEI      TO  TBL-H-SRYKBN
                                                       (IDX-H)
                       MOVE    WRK-SRYMEI      TO  WRK-SRYMEI-OLD
                   END-IF
               END-IF
      *        剤開始と明細点数記載行
               IF     (IDX2                =   1  )
                   MOVE    "＊"                TO  TBL-H-ZKBN (IDX-H)
               ELSE
                   MOVE    "　"                TO  TBL-H-ZKBN (IDX-H)
               END-IF
      *        名称
               MOVE    WRK-H-NAME1             TO  TBL-H-NAME
                                                     (IDX-H)
               IF      FLG-LINE            =   1
                   ADD     1                   TO  IDX-H
               END-IF
               IF      WRK-H-NAME2         NOT =   SPACE
                   MOVE    WRK-H-NAME2         TO  TBL-H-NAME
                                                     (IDX-H)
               END-IF
      *        単位名称
      *        MOVE    TBL-TANINAME (IDX2)     TO  TBL-H-TANINAME
      *                                                (IDX-H)
      *        数量
      *        IF      TBL-SURYO (IDX2)        >   ZERO
      *            MOVE    TBL-SURYO (IDX2)    TO  WRK-SURYO
      *            PERFORM 200521-SURYO-HEN-SEC
      *            MOVE    WRK-SURYO-X         TO  TBL-H-SURYO
      *                                                (IDX-H)
      *****    END-IF
      *        自費金額
               IF      TBL-JIHIMONEY (IDX2)     >   ZERO
                   MOVE    TBL-JIHIMONEY(IDX2) TO  WRK-Z7
                   MOVE    WRK-Z7              TO  TBL-H-TENSU
                                                      (IDX-H)
      *            合計
                   COMPUTE WRK-KINGAKU         =   TBL-JIHIMONEY(IDX2)
                                               *   TBL-KAISU
                   MOVE    WRK-KINGAKU         TO  WRK-Z119
                   MOVE    WRK-Z119            TO  TBL-H-KINGAKU
                                                      (IDX-H)
      *            円
                   MOVE    "1"                 TO  TBL-H-EN
                                                      (IDX-H)
                   MOVE    "◇"                TO  TBL-H-ZKBN (IDX-H)
               END-IF
      *        明細点数記載
               IF     (FLG-MEISAI          =   1  )  AND
                      (TBL-TEN (IDX2)  NOT =   ZERO )
                   PERFORM 30051-TENSU-HEN-SEC
               END-IF
           END-PERFORM
           IF     (TBL-SRYKBN          =   "95"
                                       OR  "96"  )  OR
                  (TBL-ZAITENSU        =   ZERO  )
               CONTINUE
           ELSE
               IF     (PARA-GOKEITANI      =   "1"  )
               OR     (TBL-ENKBN           =   "1"  )
      *            点数表示
                   MOVE    TBL-ZAITENSU        TO  WRK-TENSU
               ELSE
      *            金額
                   COMPUTE WRK-TENSU           =   TBL-ZAITENSU
                                               *   10
               END-IF
      *        点数
               IF      WRK-TENSU           >   ZERO
                   MOVE    WRK-TENSU           TO  WRK-Z7
                   MOVE    WRK-Z7              TO  TBL-H-TENSU
                                                          (IDX-H)
               ELSE
                   MOVE    WRK-TENSU           TO  WRK-Z7-2
                   MOVE    WRK-Z7-2            TO  TBL-H-TENSU
                                                          (IDX-H)
               END-IF
      *        合計
               COMPUTE WRK-KINGAKU         =   WRK-TENSU
                                           *   TBL-KAISU
               IF      WRK-KINGAKU         >   ZERO
                   MOVE    WRK-KINGAKU         TO  WRK-Z119
                   MOVE    WRK-Z119            TO  TBL-H-KINGAKU
                                                       (IDX-H)
               ELSE
                   MOVE    WRK-KINGAKU         TO  WRK-Z119-2
                   MOVE    WRK-Z119-2          TO  TBL-H-KINGAKU
                                                       (IDX-H)
               END-IF
      *        円（食事）
               IF      TBL-ENKBN           =   "1"
                   MOVE    "1"                 TO  TBL-H-EN
                                                      (IDX-H)
               END-IF
           END-IF
      *    回数
           MOVE    TBL-KAISU           TO  WRK-ZZ9
           MOVE    WRK-ZZ9             TO  TBL-H-KAISU (IDX-H)
      *    剤毎に１行開ける
           IF      PARA-LINE           =   "1"
               CONTINUE
           ELSE
               ADD     1                   TO  IDX-H
           END-IF
      *
           .
       20052-TBL-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数編集処理
      *****************************************************************
       30051-TENSU-HEN-SEC       SECTION.
      *
           IF      TBL-TENSIKIBETU (IDX2)  =   3
      *        点数
               IF      PARA-GOKEITANI      =   "1"
      *            点数表示
                   MOVE    TBL-TEN  (IDX2)     TO  WRK-TENSU
               ELSE
      *            金額表示
                   COMPUTE WRK-TENSU           =   TBL-TEN (IDX2)
                                               *   10
               END-IF
           ELSE
      *        金額
               MOVE    TBL-TEN (IDX2)      TO  WRK-TENSU
      *        円
               MOVE    "1"                 TO  TBL-H-EN   (IDX-H)
           END-IF
           IF      WRK-TENSU           >   ZERO
               MOVE    WRK-TENSU           TO  WRK-Z7
               MOVE    WRK-Z7              TO  TBL-H-TENSU (IDX-H)
           ELSE
               MOVE    WRK-TENSU           TO  WRK-Z7-2
               MOVE    WRK-Z7-2            TO  TBL-H-TENSU (IDX-H)
           END-IF
      *    合計
           COMPUTE WRK-KINGAKU         =   WRK-TENSU
                                       *   TBL-KAISU
           IF      WRK-KINGAKU         >   ZERO
               MOVE    WRK-KINGAKU         TO  WRK-Z119
               MOVE    WRK-Z119            TO  TBL-H-KINGAKU
                                                            (IDX-H)
           ELSE
               MOVE    WRK-KINGAKU         TO  WRK-Z119-2
               MOVE    WRK-Z119-2          TO  TBL-H-KINGAKU
                                                            (IDX-H)
           END-IF
      *    円表示で桁あふれ
           IF     (PARA-GOKEITANI      =   "1" )
              AND (TBL-H-EN (IDX-H)    =   "1" )
              AND (TBL-H-KINGAKU1 (IDX-H)  NOT =   SPACE )
               MOVE    SPACE               TO  TBL-H-KINGAKU (IDX-H)
               IF      WRK-KINGAKU         >   ZERO
                   MOVE    WRK-KINGAKU         TO  WRK-Z119Z
                   MOVE    WRK-Z119Z           TO  TBL-H-KINGAKU2
                                                            (IDX-H)
               ELSE
                   MOVE    WRK-KINGAKU         TO  WRK-Z119-2
                   MOVE    WRK-Z119Z-2         TO  TBL-H-KINGAKU2
                                                            (IDX-H)
               END-IF
           END-IF
      *    回数
           MOVE    TBL-KAISU           TO  WRK-ZZ9
           MOVE    WRK-ZZ9             TO  TBL-H-KAISU  (IDX-H)
      *    点数表示行
           MOVE    "＊"                TO  TBL-H-ZKBN   (IDX-H)
           .
       30051-TENSU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ページカウント処理
      *****************************************************************
       200520-PAGE-CNT-SEC                  SECTION.
      *
           MOVE    LINE-MAX            TO  IDX-H
           MOVE    "次ページへ続く"    TO  TBL-H-SRYMEI
                                                   (IDX-H)(30:)
      *    帳票印刷
           PERFORM 400-PRINT-SYORI-SEC
           MOVE    1                   TO  IDX-H
           MOVE    SPACE               TO  WRK-SRYMEI-OLD
           INITIALIZE                  TBL-MEISAI-AREA
           .
       200520-PAGE-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量表示用編集処理
      *****************************************************************
       200521-SURYO-HEN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
      *
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING     IDXM   FROM    5  BY  -1
                   UNTIL       IDXM    <   1
               IF      WRK-SURYO-S2(IDXM:1) NOT =   ZERO
                   MOVE    1               TO  FLG-SP
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(IDXM:1) TO  WRK-SURYO-Z3(IDXM:1)
               END-IF
           END-PERFORM
      *
           IF      WRK-SURYO-Z3        NOT =   SPACE
               MOVE    "."                 TO  WRK-SURYO-Z2
           ELSE
               MOVE    SPACE               TO  WRK-SURYO-X
               MOVE    WRK-SURYO           TO  WRK-SURYO-Z4
           END-IF
      *
           .
       200521-SURYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    数量＋単位名称（全角）編集処理
      *****************************************************************
       20052-SURYO-ZENHEN-SEC                  SECTION.
      *
      *    数量表示用編集
           MOVE    TBL-SURYO (IDX2)    TO  WRK-SURYO
           PERFORM 200521-SURYO-HEN-SEC
      *    全角変換
           MOVE    SPACE               TO  WRK-Z-SURYO
           MOVE    ZERO                TO  IDXM2
           PERFORM VARYING     IDXM    FROM    1   BY  1
                   UNTIL       IDXM    >   11
               IF      WRK-SURYO-X (IDXM:1) NOT =   SPACE
                   MOVE    WRK-SURYO-X(IDXM:)   TO  WRK-Z-SURYO
                   MOVE    11                   TO  IDXM
               END-IF
           END-PERFORM
           MOVE    WRK-Z-SURYO         TO  WRK-MOJI
           PERFORM 2009-KANACHK-SEC
           MOVE    WRK-ZENKAKU-G       TO  WRK-K-SURYO
      *
           MOVE    1                   TO  IDXM2
           MOVE    SPACE               TO  WRK-ZENHEN-SURYO
           PERFORM VARYING     IDXM    FROM    1   BY  2
                   UNTIL       IDXM    >   22
               IF      WRK-K-SURYO (IDXM:2)     =   SPACE
                                                OR  "　"
                   CONTINUE
               ELSE
                   MOVE    WRK-K-SURYO(IDXM:2)  TO  WRK-ZENHEN-SURYO
                                                         (IDXM2:2)
                   ADD     2                    TO  IDXM2
               END-IF
           END-PERFORM 
      *    単位名称
           PERFORM VARYING     IDXM    FROM    1   BY  2
                   UNTIL       IDXM    >   10
               IF      TBL-TANINAME (IDX2) (IDXM:2)
                                                =   SPACE
                                                OR  "　"
                   CONTINUE
               ELSE
                   MOVE    TBL-TANINAME (IDX2)(IDXM:2)
                                                TO  WRK-ZENHEN-SURYO
                                                         (IDXM2:2)
                   ADD     2                    TO  IDXM2
               END-IF
           END-PERFORM
           MOVE    WRK-ZENHEN-SURYO    TO  WRK-H-SURYO
      *
           .
       20052-SURYO-ZENHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療種別区分名称処理
      *****************************************************************
       200521-SRYSYUMEI-SEC             SECTION.
      *
           EVALUATE    TBL-SRYKBN
               WHEN    "11"
               WHEN    "12"
                   MOVE    "初・再診料"        TO  WRK-SRYMEI
               WHEN    "13"
                   MOVE    "医学管理等"        TO  WRK-SRYMEI
               WHEN    "14"
                   MOVE    "在宅医療"          TO  WRK-SRYMEI
               WHEN    "21"
               WHEN    "22"
               WHEN    "23"
               WHEN    "24"
               WHEN    "25"
               WHEN    "26"
               WHEN    "27"
                   MOVE    "投　　薬"          TO  WRK-SRYMEI
               WHEN    "31"
               WHEN    "32"
               WHEN    "33"
                   MOVE    "注　　射"          TO  WRK-SRYMEI
               WHEN    "40"
                   MOVE    "処　　置"          TO  WRK-SRYMEI
               WHEN    "50"
                   MOVE    "手　　術"          TO  WRK-SRYMEI
               WHEN    "54"
                   MOVE    "麻　　酔"          TO  WRK-SRYMEI
               WHEN    "60"
                   MOVE    "検　　査"          TO  WRK-SRYMEI
               WHEN    "64"
                   MOVE    "病理診断"          TO  WRK-SRYMEI
               WHEN    "70"
                   MOVE    "画像診断"          TO  WRK-SRYMEI
               WHEN    "80"
                   MOVE    "リハビリ"          TO  WRK-SRYMEI
               WHEN    "83"
                   MOVE    "精神科専門"        TO  WRK-SRYMEI
               WHEN    "84"
                   MOVE    "放射線治療"        TO  WRK-SRYMEI
               WHEN    "85"
                   MOVE    "療養担当"          TO  WRK-SRYMEI
               WHEN    "89"
                   MOVE    "入院料等"          TO  WRK-SRYMEI
               WHEN    "86"
                   MOVE    "その他"            TO  WRK-SRYMEI
               WHEN    "90"
               WHEN    "92"
                   MOVE    "入院料等"          TO  WRK-SRYMEI
               WHEN    "97"
                   MOVE    "入院料等"          TO  WRK-SRYMEI
               WHEN    "95"
               WHEN    "96"
                   MOVE    "給付外"            TO  WRK-SRYMEI
               WHEN    OTHER
                   MOVE    "その他"            TO  WRK-SRYMEI
           END-EVALUATE
           .
       200521-SRYSYUMEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    最終印刷処理
      *****************************************************************
       2006-LAST-PRINT-SEC             SECTION.
      *
      *    最終行
           IF      TBL-H-SRYMEI(LINE-MAX)
                                       =   SPACE
               ADD     1                   TO  IDX-H
               IF      IDX-H               >   LINE-MAX
                   MOVE    LINE-MAX            TO  IDX-H
               END-IF
               MOVE    "以下余白"          TO  TBL-H-SRYMEI
                                                    (IDX-H)(30:)
           END-IF
      *    帳票印刷
           PERFORM 400-PRINT-SYORI-SEC
           .
       2006-LAST-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       400-PRINT-SYORI-SEC          SECTION.
      *
           COMPUTE ORCHCN04-PAGE       =   ORCHCN04-PAGE    +   1
      *
           IF    ( ORCHCN04-PRTKBN  =      "1" )
      *
      *        帳票編集処理
               PERFORM 300-HCN04-HEN-SEC
      *
      *        帳票印刷処理
               PERFORM 401-PRINT-SEC
           ELSE
      *
      *        帳票印刷開始処理
               PERFORM 400-SYOKI-PRINT-SEC
      *
      *        帳票編集処理
               PERFORM 300-HCN04-HEN-SEC
      *
      *        帳票印刷処理
               PERFORM 400-PRINT-SEC
           END-IF
      *
           .
       400-PRINT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       300-HCN04-HEN-SEC                SECTION.
      *
           INITIALIZE                  HCN04
      *
           ADD     1                   TO  WRK-PAGE
      *    頁
           MOVE    WRK-PAGE            TO  WRK-ZZ9
           MOVE    WRK-ZZ9             TO  HCN04-PAGE
      *    伝票番号
           MOVE    WRK-HEAD-DENPNUM    TO  WRK-NO
           MOVE    WRK-NO              TO  HCN04-NO
      *    診療科
           MOVE    WRK-HEAD-SRYKA-MEI  TO  HCN04-SRYKANAME
      *    患者番号
           MOVE    ORCHCN04-PTNUM      TO  HCN04-PTNUM
      *    カナ氏名
           IF    ( PARA-KANANAME       =   "1" )
               MOVE    SPA-KANANAME    TO  HCN04-KANANAME
           END-IF
      *    患者名称
           MOVE    ORCHCN04-NAME       TO  HCN04-NAME
      *    保険種別
           MOVE    WRK-HEAD-HKNCOMBIMEI    TO  HCN04-HOKEN
      *    負担割合
           MOVE    WRK-HEAD-FUTAN      TO  HCN04-FUTAN
           MOVE    WRK-HEAD-FUTAN-MEI  TO  HCN04-FUTANLABEL
      *    発行日
           MOVE    WRK-HEAD-HAKYMD     TO  HCN04-HAKYMD
      *    請求開始日
           MOVE    WRK-HEAD-NYUINSTRYMD    TO  HCN04-NYUINSTRYMD
           MOVE    WRK-HEAD-NYUINENDYMD    TO  HCN04-NYUINENDYMD
      *
      *    病棟
           MOVE    WRK-ROOM            TO  HCN04-ROOM
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS        TO  HCN04-IRYOADRS
           INSPECT HCN04-IRYOADRS      REPLACING ALL "  " BY "　"
      *    医療機関名
           MOVE    WRK-HOSPNAME        TO  HCN04-IRYOMEI
           INSPECT HCN04-IRYOMEI       REPLACING ALL "  " BY "　"
      *    医療機関ＴＥＬ
           MOVE    WRK-HOSPTEL         TO  HCN04-IRYOTEL
      *
      *    列名（点数記載）
           IF      PARA-GOKEITANI      =   "1"
               MOVE    "　点　数　"        TO  HCN04-LABEL3
           ELSE
               MOVE    "　単　価　"        TO  HCN04-LABEL3
           END-IF
           MOVE    "　　合　　計　　"  TO  HCN04-LABEL1
      *
      *    明細内容
           PERFORM VARYING     IDX3    FROM    1   BY  1
                   UNTIL       IDX3    >   LINE-MAX
               MOVE    TBL-H-SRYKBN (IDX3) TO  HCN04-SRYKBN (IDX3)
               MOVE    TBL-H-SRYMEI (IDX3) TO  HCN04-SRYMEI (IDX3)
      *        MOVE    TBL-H-TANINAME(IDX3)
      *                                    TO  HCN04-TANI  (IDX3)
               IF      TBL-H-KAISU  (IDX3) =   "---"
                   MOVE    ALL "―"            TO  HCN04-KAISU2 (IDX3)
                   MOVE    ALL "―"            TO  HCN04-KINGAKU2(IDX3)
               ELSE
      ***          MOVE    TBL-H-SURYO  (IDX3) TO  HCN04-SURYO (IDX3)
                   MOVE    TBL-H-KAISU  (IDX3) TO  HCN04-KAISU (IDX3)
                   MOVE    TBL-H-TENSU  (IDX3) TO  HCN04-TENSU  (IDX3)
                   MOVE    TBL-H-KINGAKU(IDX3) TO  HCN04-KINGAKU(IDX3)
               END-IF
      *        点数記載で円のもの
               IF      PARA-GOKEITANI      =   "1"
                   IF      TBL-H-EN  (IDX3)    =   "1"
      *
                       MOVE    SPACE           TO  HCN04-TENSU  (IDX3)
                       STRING  TBL-H-TENSU2  (IDX3)
                               "　"
                                           DELIMITED  BY  SIZE
                                           INTO    HCN04-TENSU(IDX3)
                       END-STRING
                       MOVE    "円"           TO  HCN04-TENSU2(IDX3)
      *
                       MOVE    SPACE           TO  HCN04-KINGAKU(IDX3)
                       STRING  TBL-H-KINGAKU2(IDX3)
                               "　"
                                           DELIMITED  BY  SIZE
                                           INTO    HCN04-KINGAKU(IDX3)
                       END-STRING
                       MOVE    "円"           TO  HCN04-KINGAKU2(IDX3)
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      PARA-GOKEITANI      =   "1"
               MOVE    CONS-LABEL2         TO  HCN04-LABEL2
           ELSE
               MOVE    CONS-LABEL1         TO  HCN04-LABEL2
           END-IF
      *
           EVALUATE   PARA-SHADE
               WHEN    "0"
      *            網掛けなし
                   STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                           " -L L1  -L L2 -L L3 "
                                               DELIMITED  BY  SIZE
                           "MonpeLayerOut"     DELIMITED  BY  SIZE
                                               INTO    HCN04-LOPTION
                   END-STRING
               WHEN    "1"
      *            網掛の濃さ１
                   STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                           " -L L2  -L L3 "
                                               DELIMITED  BY  SIZE
                           "MonpeLayerOut"     DELIMITED  BY  SIZE
                                               INTO    HCN04-LOPTION
                   END-STRING
               WHEN    "2"
      *            網掛の濃さ２
                   STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                           " -L L1  -L L3 "
                                               DELIMITED  BY  SIZE
                           "MonpeLayerOut"     DELIMITED  BY  SIZE
                                               INTO    HCN04-LOPTION
                   END-STRING
               WHEN    OTHER
      *            網掛濃さ３（標準）
                   STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                           " -L L1  -L L2 "
                                               DELIMITED  BY  SIZE
                           "MonpeLayerOut"     DELIMITED  BY  SIZE
                                               INTO    HCN04-LOPTION
                   END-STRING
           END-EVALUATE
           .
       300-HCN04-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   入院基本料編集処理
      *****************************************************************
       2002-NYUIN-HEN-SEC                SECTION.
      *
           PERFORM 900-PTNYUINRRK-KEY-SEL-SEC
      *
      *    同日再入院区分
           IF      PTNYUINRRK-DOUJITSUKBN  =   "1"
               MOVE    1                   TO  FLG-DOUJITU
               MOVE    PTNYUINRRK-NYUINYMD TO  WRK-NYUINYMD
           ELSE
               MOVE    ZERO                TO  FLG-DOUJITU
               MOVE    SPACE               TO  WRK-NYUINYMD
           END-IF
           .
       2002-NYUIN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   入院基本料編集処理
      *****************************************************************
       2004-NYUIN-SYORI-SEC                SECTION.
      *
           INITIALIZE                      SS006-AREA
           MOVE    SYU-SKYSTYMD        TO  SS006-SKYSTYMD
           MOVE    SYU-SKYEDYMD        TO  SS006-SKYEDYMD
           IF    ( SYU-CREATEKBN       =   "1" )
               IF    ( SYU-SKYEDYMD    =   PTNYUINRRK-TAIINYMD )
                   MOVE    "1"         TO  SS006-SKYKBN
               ELSE
                   MOVE    "2"         TO  SS006-SKYKBN
               END-IF
           ELSE
               MOVE    SYU-CREATEKBN   TO  SS006-SKYKBN
           END-IF
           CALL    "ORCSS006"              USING
                                           SS006-AREA
                                           SPA-AREA
      *
           INITIALIZE                      HCM34S01-AREA
           MOVE    SYU-HOSPNUM         TO  HCM34S01-HOSPNUM
           MOVE    SYU-PTID            TO  HCM34S01-PTID
           MOVE    SYU-SRYYMD(1:6)     TO  HCM34S01-SANTEIYM
           MOVE    "1"                 TO  HCM34S01-KIKANKBN
           MOVE    SYU-SKYSTYMD        TO  HCM34S01-SKYSTYMD
           MOVE    SYU-SKYEDYMD        TO  HCM34S01-SKYEDYMD
           IF    ( SYU-SKYSTYMD        <   CONST-H180401 )
               CONTINUE
           ELSE
               MOVE    SS006-DOJITUFLG-ST  TO  HCM34S01-DOJITUFLG-ST
               MOVE    SS006-TIMEKBN-ST    TO  HCM34S01-TIMEKBN-ST
           END-IF
           IF    ( SYU-SKYEDYMD        <  CONST-H180401 )
               CONTINUE
           ELSE
               MOVE    SS006-DOJITUFLG-ED  TO  HCM34S01-DOJITUFLG-ED
               MOVE    SS006-TIMEKBN-ED    TO  HCM34S01-TIMEKBN-ED
           END-IF
           MOVE    SYU-HKNCOMBINUM     TO  HCM34S01-HKNCOMBI
           MOVE    SYU-SYUHKNNUM       TO  HCM34S01-HKNNUM
           MOVE    "1"                 TO  HCM34S01-SKJFTN
      *
           IF      SYU-SYUHKNNUM       =   "971"
                                       OR  "973"
               CALL    "ORCHCM34S02"       USING
                                           HCM34S01-AREA
                                           SPA-AREA
           ELSE
               CALL    "ORCHCM34S01"       USING
                                           HCM34S01-AREA
                                           SPA-AREA
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   HCM34S01-HENKYAKU-MAX
      *
               MOVE    1               TO  FLG-MEISAI
      *
               IF    ( HCM34S01-ZAITEN (IDX)   =   ZERO )
                   MOVE    ZERO        TO  FLG-MEISAI
               END-IF
      *
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2    >   20   )  OR
                                  (HCM34S01-SRYCD(IDX IDX2)
                                               =   SPACE  ) OR
                                  (FLG-MEISAI  =   ZERO   )
                   IF    ( HCM34S01-SRYCD(IDX IDX2)
                                               =   "099999907" )
                       MOVE    ZERO        TO  FLG-MEISAI
                   END-IF
               END-PERFORM
      *
               IF    ( FLG-MEISAI      =   1 )
                   INITIALIZE                  TBL-DATA-AREA
                   MOVE    ZERO            TO  IDX-S
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL      (IDX2    >   20   )  OR
                                      (HCM34S01-SRYCD(IDX IDX2)
                                                   =   SPACE   )
                       PERFORM 20041-SRYCD-HEN-SEC
                   END-PERFORM
                   PERFORM 20042-NYUTBL-HENSYU-SEC
               END-IF
           END-PERFORM
      *    選定入院料患者自己負担額
           IF      HCM34S01-SENTEI-PTFTN   >   ZERO
               PERFORM 20043-SENTEI-HENSYU-SEC
           END-IF
          .
       2004-NYUIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   入院基本料明細編集
      *****************************************************************
       20041-SRYCD-HEN-SEC                SECTION.
      *
           MOVE    HCM34S01-SRYCD(IDX IDX2)    TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *
           ADD     1                   TO  IDX-S
           MOVE    HCM34S01-SRYCD(IDX IDX2)
                                       TO  TBL-SRYCD (IDX-S)
           MOVE    TNS-NAME            TO  TBL-NAME  (IDX-S)
           IF      TNS-TANICD          =   ZERO
                                       OR  SPACE
               CONTINUE
           ELSE
               MOVE    TNS-TANINAME        TO  TBL-TANINAME(IDX-S)
               MOVE    HCM34S01-SURYO(IDX IDX2)
                                           TO  TBL-SURYO (IDX-S)
           END-IF
          .
       20041-SRYCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   入院印刷処理
      *****************************************************************
       20042-NYUTBL-HENSYU-SEC                SECTION.
      *
      *    室料差額は自費
           EVALUATE    HCM34S01-ZAISKBKBN(IDX)
           WHEN    "3"
               MOVE    "95"                    TO  TBL-SRYKBN
           WHEN    "7"
               MOVE    "85"                    TO  TBL-SRYKBN
           WHEN    OTHER
               MOVE    HCM34S01-SRYKBN (IDX)   TO  TBL-SRYKBN
           END-EVALUATE
           MOVE    HCM34S01-SRYSYUKBN(IDX) TO  TBL-SRYSYUKBN
      *    剤点数
           MOVE    HCM34S01-ZAITEN (IDX)   TO  TBL-ZAITENSU
      *    剤回数
           MOVE    HCM34S01-ZAIKAISU(IDX)  TO  TBL-KAISU
      *    金額
      *    室料差額は自費
           IF      HCM34S01-ZAISKBKBN(IDX) =   "3"
               MOVE    "95"                    TO  TBL-SRYKBN
               MOVE    HCM34S01-ZAITEN (IDX)   TO  TBL-JIHIMONEY(1)
               MOVE    ZERO                    TO  TBL-ZAITENSU
           END-IF
      *    食事・室料差額は金額
           IF      HCM34S01-ZAISKBKBN(IDX) =   "4"
                                           OR  "3"
      *        COMPUTE TBL-KINGAKU         =  (TBL-ZAITENSU
      *                                    *   TBL-KAISU )
               MOVE    "1"                 TO  TBL-ENKBN
           ELSE
      *        IF      PARA-GOKEITANI      =   "1"
      *            点数表示
      *            COMPUTE TBL-KINGAKU         =  (TBL-ZAITENSU
      *                                        *   TBL-KAISU )
      *            MOVE    SPACE               TO  TBL-ENKBN
      *        ELSE
      *            金額表示
      *            COMPUTE TBL-KINGAKU         =  (TBL-ZAITENSU
      *                                        *   TBL-KAISU )
      *                                        *   10
                   MOVE    SPACE               TO  TBL-ENKBN
      *        END-IF
           END-IF
      *
      *    帳票印刷
           PERFORM 2005-PRINT-SYORI-SEC
           .
       20042-NYUTBL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *   選定入院料患者自己負担額印刷処理
      *****************************************************************
       20043-SENTEI-HENSYU-SEC                SECTION.
      *
           INITIALIZE                  TBL-DATA-AREA
           MOVE    "95"                TO  TBL-SRYKBN
           MOVE    1                   TO  TBL-KAISU
           MOVE    "99999999"          TO  TBL-SRYCD  (1)
           MOVE    "選定入院料自己負担額"
                                       TO  TBL-NAME   (1)
           MOVE    HCM34S01-SENTEI-PTFTN
                                       TO  TBL-JIHIMONEY(1)
      *    帳票印刷
           PERFORM 2005-PRINT-SYORI-SEC
           .
       20043-SENTEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *   自費分印刷処理
      *****************************************************************
       2005-JIHI-SYORI-SEC                SECTION.
      *
           PERFORM VARYING     IDXJ    FROM    1   BY  1
                   UNTIL      (IDXJ    >   50  )   OR
                              (TBL-JIHI-SRYCD(IDXJ)    =   SPACE)
      *        １明細１剤とする
               INITIALIZE                  TBL-DATA-AREA
               MOVE    TBL-JIHI-SRYKBN(IDXJ)   TO  TBL-SRYKBN
               MOVE    TBL-JIHI-KAISU (IDXJ)   TO  TBL-KAISU
               MOVE    TBL-JIHI-SRYSYUKBN(IDXJ)
                                               TO  TBL-SRYSYUKBN
               MOVE    TBL-JIHI-SRYCD(IDXJ)    TO  TBL-SRYCD(1)
               MOVE    TBL-JIHI-NAME(IDXJ)     TO  TBL-NAME(1)
               MOVE    TBL-JIHI-JIHIMONEY(IDXJ)
                                               TO  TBL-JIHIMONEY(1)
      *        帳票印刷編集
               PERFORM 2005-PRINT-SYORI-SEC
           END-PERFORM
      *
           .
       2005-JIHI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *   労災入院料加算チェック
      *****************************************************************
       2005-RSIKSN-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-RSIKSN
      *
           IF    ( SYU-SYUHKNNUM       =   "971"  OR "973" )
               INITIALIZE                  HCM34S03-AREA
               MOVE    SYU-SRYYMD(1:6) TO  HCM34S03-SRYYM
               MOVE    WRK-SRYCD       TO  HCM34S03-SRYCD
               CALL    "ORCHCM34S03"   USING
                                       HCM34S03-AREA
                                       SPA-AREA
               IF    ( HCM34S03-IGNORE =   1 )
                   MOVE    1           TO  FLG-RSIKSN
               END-IF
           END-IF
      *
           .
       2005-RSIKSN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       2009-KANACHK-SEC                 SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MOJI            TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC          =   ZERO )  AND
                  (KANACHK-MAX         >   ZERO )
               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-ZENKAKU-G
           ELSE
               MOVE    WRK-MOJI            TO  WRK-ZENKAKU-G
           END-IF
      *
           .
       2009-KANACHK-EXT.
           EXIT.
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
           MOVE    SPA-SYSYMD      TO  SYS-1900-STYUKYMD
           MOVE    SPA-SYSYMD      TO  SYS-1900-EDYUKYMD
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
           MOVE    SPA-SYSYMD      TO  SYS-1901-STYUKYMD
           MOVE    SPA-SYSYMD      TO  SYS-1901-EDYUKYMD
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
           MOVE    SPA-SYSYMD      TO  SYS-1902-STYUKYMD
           MOVE    SPA-SYSYMD      TO  SYS-1902-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
           MOVE    SYS-1902-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1902-REC
               MOVE    SYS-1902-ADRS1      TO  WRK-HOSPADRS1
               MOVE    SYS-1902-ADRS2      TO  WRK-HOSPADRS2
               MOVE    SYS-1902-TEL        TO  WRK-HOSPTEL
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
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       910-JYURRK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       910-JYURRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       920-SRYACT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       920-SRYACT-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       930-SRYACCT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       930-SRYACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       930-SRYACCTPLUS-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCTPLUS-REC
               MOVE    ZERO            TO  FLG-SRYACCTPLUS
           ELSE
               INITIALIZE                  SRYACCTPLUS-REC
               MOVE    1               TO  FLG-SRYACCTPLUS
           END-IF
      *
           .
       930-SRYACCTPLUS-READ-EXT.
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
      *    労災保険マスター読込
      *****************************************************************
       940-PTRSIINF-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               MOVE    ZERO            TO  FLG-PTRSIINF
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
      *
           .
       940-PTRSIINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険マスター読み込み
      *****************************************************************
       950-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTHKNINF-REC
                   MOVE    ZERO            TO  FLG-PTHKNINF
               ELSE
                   MOVE    1               TO  FLG-PTHKNINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
     *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       950-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   MOVE    ZERO            TO  FLG-HKNCOMBI
               ELSE
                   MOVE    1               TO  FLG-HKNCOMBI
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
     *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    WRK-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
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
      *
      *    点数附加情報検索
           IF     (FLG-TENSU           =   ZERO)
               PERFORM 9101-TENSUPLUS-READ-SEC
           ELSE
               INITIALIZE                      TENSUPLUS-REC
           END-IF
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ付加情報検索処理
      *****************************************************************
       9101-TENSUPLUS-READ-SEC          SECTION.
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
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TENSUPLUS-REC
                   MOVE    ZERO                TO  FLG-TENSUPLUS
               ELSE
                   INITIALIZE                  TENSUPLUS-REC
                   MOVE    1                   TO  FLG-TENSUPLUS
               END-IF
           ELSE
               INITIALIZE                      TENSUPLUS-REC
               MOVE    1                   TO  FLG-TENSUPLUS
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9101-TENSUPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           INITIALIZE                      PTNYUINRRK-REC
      *
           MOVE    SYU-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SYU-PTID            TO  PTNYUINRRK-PTID
           MOVE    SYU-NYUIN-RRKNUM    TO  PTNYUINRRK-RRKNUM
           MOVE    1                   TO  PTNYUINRRK-RRKEDANUM
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 950-PTNYUINRRK-READ-SEC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTNYUINRRK-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者入院履歴検索処理(KEY45)
      *****************************************************************
       900-PTNYUINRRK-KEY45-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           INITIALIZE                      PTNYUINRRK-REC
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SYU-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SYU-PTID            TO  PTNYUINRRK-PTID
           IF     (ORCHCN04-DENPPRTYMD     =   SPACE )
               MOVE    SYU-DENPPRTYMD      TO  PTNYUINRRK-TENNYUYMD
           ELSE
               MOVE    ORCHCN04-DENPPRTYMD TO  PTNYUINRRK-TENNYUYMD
           END-IF
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key45"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 950-PTNYUINRRK-READ-SEC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO MCP-TABLE
           MOVE    "key45"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTNYUINRRK-KEY45-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者入院履歴検索
      *****************************************************************
       950-PTNYUINRRK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
               MOVE    ZERO            TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       950-PTNYUINRRK-READ-EXT.
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
