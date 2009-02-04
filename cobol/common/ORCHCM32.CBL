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
       PROGRAM-ID.             ORCHCM32.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 支払証明書（月別）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/12/11    NACL−森脇    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.01.01    NACL-森脇    03.03.18  出力先プリンタの変更
      *  01.01.02    NACL-森脇    03/07/15  出力先プリンタサブ追加
      *  02.01.01    NACL-森脇    03/12/10  出力条件修正
      *  02.01.02    NACL-森脇    04/10/25  CLOSE追加
      *  02.01.03    NACL-森脇    04/11/09  パラメタ変更,バッチ処理に変更
      *  03.04.01    NACL-森脇    07/01/30  自費消費税H19年4月対応
      *  03.05.00    NACL-太田    07/05/24  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    帳票
           COPY    "HCM32G.INC".
           COPY    "HCM32N.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTNUM           PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-TAISYO          PIC 9(01).
           03  FLG-LOOP-END        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(05).
           03  IDY                 PIC 9(05).
           03  IDX1                PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDX3                PIC 9(05).
           03  IDX4                PIC 9(05).
           03  IDX-SRH             PIC 9(05).
           03  IDX-LINE            PIC 9(05).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(03).
           03  CNT-LINE            PIC 9(03).
           03  CNT-OUT             PIC 9(03).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-TABLE           PIC X(64).
           03  WRK-PATHNAME        PIC X(64).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WYY         PIC X(03).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-HENSYU-TBL.
               05  WRK-KEY-YMD     PIC X(08).
               05  WRK-HEN-MAX     PIC 9(04).
               05  WRK-NYUIN       PIC 9(01).
      *        最大８０１件・・・８０１件目は並び替え用
               05  WRK-HENSYU-OCC  OCCURS  801.
                   07  WRK-HENSYU-KEY.
                       09  WRK-NYUGAIKBN  PIC X(01).
                       09  WRK-HIDUKE PIC X(06).
                   07  WRK-TENSU      PIC S9(07).
                   07  WRK-HOKEN      PIC S9(11).
                   07  WRK-SYOKUJI    PIC S9(11).
                   07  WRK-JIHI       PIC S9(11).
                   07  WRK-GAKU       PIC S9(11).
                   07  WRK-BFGAKU     PIC S9(11).
      *
           03  WRK-GHENSYU-TBL.
               05  WRK-GHKNTEN     PIC S9(07).
               05  WRK-GFTNMONEY   PIC S9(11).
               05  WRK-GSKJMONEY   PIC S9(11).
               05  WRK-GJIHIMONEY  PIC S9(11).
               05  WRK-GSKYMONEY   PIC S9(11).
      *
           03  WRK-HKNTEN          PIC S9(11).
           03  WRK-FTNMONEY        PIC S9(11).
           03  WRK-SKJMONEY        PIC S9(11).
           03  WRK-JIHIMONEY       PIC S9(11).
           03  WRK-HKNCOMBINUM-X.
               05  WRK-HKNCOMBINUM PIC 9(04).
           03  WRK-HKN-SKYMONEY    PIC S9(11).
      *
           03  WRK-Z3              PIC ZZZ.
           03  WRK-Z6              PIC ZZZ,ZZZ.
           03  WRK-Z7              PIC Z,ZZZ,ZZZ.
           03  WRK-Z9              PIC ---,---,---.
           03  WRK-HOJOKBN         PIC X(01).
      *
       01  SRH-AREA.
           03  SRH-CD                      PIC X(10).
           03  SRH-DAT-G.
               05  SRH-DAT                 PIC X(100).
           03  SRH-COMB-DAT.
               05  SRH-COMB-HKNCOMBINUM    PIC 9(04).
               05  SRH-COMB-HOJOKBN        PIC X(01).
               05  SRH-COMB-MAX            PIC 9(03).
               05  SRH-COMB-OCC            OCCURS  100.
                   07  SRH-COMB-HKNCOMBINUML
                                           PIC X(04).
                   07  SRH-COMB-HOJOKBNL   PIC X(01).
      *
       01  KEY-AREA.
           03  KEY-NYUGAIKBN       PIC X(01).
      *
       01  CONST-AREA.
           03  CONST-HEN-MAX       PIC 9(05)   VALUE   24.
           03  CONST-LINE-MAX      PIC 9(05)   VALUE   12.
           03  CONST-SRH-COMB-MAX  PIC 9(03)   VALUE   100.
      *----(03.04.01)--ADD-START---
           03  CONST-H190401       PIC X(08)   VALUE   "20070401".
      *----(03.04.01)--ADD-END-----
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報−基本情報 
           COPY    "CPSK1001.INC".
      *
      *    医療機関情報−所在地、連絡先
           COPY    "CPSK1002.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *    シェル用領域
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "CPORCHCM31S01.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                    SECTION.
      *
           COPY    "COMMON-SPA".
           COPY    "CPORCSPRT.INC".
           COPY    "CPORCHCM31.INC".
      *
       PROCEDURE                   DIVISION    USING
                                               SPA-AREA
                                               ORCSPRTAREA
                                               ORCHCM31AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SYS-AREA
           INITIALIZE                  WRK-HENSYU-TBL
           INITIALIZE                  ORCHCM31-OT-AREA
           INITIALIZE                  SRH-AREA
           INITIALIZE                  KEY-AREA
      *
           ACCEPT  SYS-TIME            FROM    TIME
      *
      *    医療機関情報−基本情報読み込み
           PERFORM 900-SYS1001-READ-SEC
      *
      *    医療機関情報−所在地、連絡先読み込み
           PERFORM 900-SYS1002-READ-SEC
      *
      *    患者情報読み込み
           PERFORM 900-PTINF-READ-SEC
      *
      *    患者番号読み込み
           PERFORM 900-PTNUM-READ-SEC
      *
      *    患者保険組合せ取得
           PERFORM 1001-HKNCOMBI-HEN-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険組合せ取得処理
      *****************************************************************
       1001-HKNCOMBI-HEN-SEC       SECTION.
      *
           PERFORM 900-HKNCOMBI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-HKNCOMBI    NOT =   ZERO )
                    OR   ( SRH-COMB-MAX       >=   CONST-SRH-COMB-MAX )
      *
               IF    ( COMB-TEKSTYMD   <=  ORCHCM31-ENDSRYYMD )
                AND  ( COMB-TEKEDYMD   >=  ORCHCM31-STRSRYYMD )
      *
                   COMPUTE SRH-COMB-MAX
                       =   SRH-COMB-MAX    +   1
      *
                   MOVE    COMB-HKNCOMBINUM
                                       TO  WRK-HKNCOMBINUM
                   MOVE    WRK-HKNCOMBINUM-X
                                       TO  SRH-COMB-HKNCOMBINUML
                                                          (SRH-COMB-MAX)
                   MOVE    COMB-HOJOKBN TO SRH-COMB-HOJOKBNL
                                                          (SRH-COMB-MAX)
      *
               END-IF
      *
               PERFORM 900-HKNCOMBI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       1001-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    統計データ編集
           PERFORM 210-HENSYU-SEC
      *
      *    統計データ並び替え
           PERFORM 230-NARABIKAE-SEC
      *
           MOVE    ZERO            TO  CNT-LINE
      *
           MOVE    1               TO  IDX4
           PERFORM UNTIL ( IDX4    >   WRK-HEN-MAX )
      *
               INITIALIZE              WRK-GHENSYU-TBL
               MOVE    ZERO        TO  CNT-PAGE
      *
               MOVE    WRK-NYUGAIKBN (IDX4)
                                   TO  KEY-NYUGAIKBN
      *
               PERFORM UNTIL ( IDX4    >   WRK-HEN-MAX )
                        OR   ( WRK-NYUGAIKBN (IDX4)
                                   NOT =   KEY-NYUGAIKBN )
      *
                   PERFORM 2001-EDIT-SEC
      *
               END-PERFORM
      *
           END-PERFORM
      *
           MOVE    1               TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    編集処理
      *****************************************************************
       2001-EDIT-SEC               SECTION.
      *
           PERFORM UNTIL ( IDX4    >   WRK-HEN-MAX )
                    OR   ( WRK-NYUGAIKBN(IDX4) NOT =   KEY-NYUGAIKBN )
                    OR   ( CNT-LINE               >=  CONST-LINE-MAX )
      *
      *        対象データチェック処理
               PERFORM 230-TAISYO-CHK-SEC
               IF    ( FLG-TAISYO  =   1 )
      *
                   PERFORM 240-MEISAI-HEN-SEC
      *
               END-IF
      *
               COMPUTE IDX4    =   IDX4    +   1
      *
           END-PERFORM
      *
           IF    ( IDX4                    >   WRK-HEN-MAX )
            OR   ( WRK-NYUGAIKBN(IDX4) NOT =   KEY-NYUGAIKBN )
                       PERFORM 240-GOKEI-HEN-SEC
           END-IF
      *
           IF    ( CNT-LINE                >   ZERO )
               PERFORM 250-PRINT-SEC
           END-IF
      *
           .
       2001-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    編集処理
      *****************************************************************
       210-HENSYU-SEC              SECTION.
      *
           IF    ( ORCHCM31-KIKANKBN   =   "2" )
               MOVE    "tbl_syunou"    TO  WRK-TABLE
               MOVE    "key50"         TO  WRK-PATHNAME
           ELSE
               MOVE    "tbl_syunou"    TO  WRK-TABLE
               MOVE    "key65"         TO  WRK-PATHNAME
           END-IF
      *
      *    収納マスター読み込み
           PERFORM 900-SYUNOU-SELECT-SEC
      *
      *    収納マスターがなくなるまで繰り返す
           PERFORM UNTIL   FLG-SYUNOU      =   1
      *
               PERFORM 2101-TAISYO-CHK-SEC
      *
               IF    ( FLG-TAISYO      =   1 )
      *
                   IF    ( ORCHCM31-KIKANKBN   =   "1" )
                       PERFORM 2101-SRYKIKAN-HEN-SEC
                   END-IF
      *
                   IF    ( ORCHCM31-KIKANKBN   =   "2" )
                       PERFORM 2101-SIHKIKAN-HEN-SEC
                   END-IF
               END-IF
      *
      *        収納マスターFETCH
               PERFORM 900-SYUNOU-READ-SEC
      *
           END-PERFORM
      *
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象データチェック処理
      *****************************************************************
       2101-TAISYO-CHK-SEC             SECTION.
      *
           MOVE    1               TO  FLG-TAISYO
      *
           IF    ( SYU-DENPJTIKBN      =   "3" )
                   MOVE    ZERO    TO  FLG-TAISYO
           END-IF
      *
      *
           IF    ( ORCHCM31-DSPKBN     =   "3" )
               IF    ( SYU-SYUHKNNUM (1:2) =   "98" )
                OR   ( SYU-SYUHKNNUM       =   "971" )
                OR   ( SYU-SYUHKNNUM       =   "973" )
                OR   ( SYU-SYUHKNNUM       =   "975" )
                   MOVE    ZERO    TO  FLG-TAISYO
               END-IF
           END-IF
      *
           .
       2101-TAISYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療期間より編集処理
      *****************************************************************
       2101-SRYKIKAN-HEN-SEC           SECTION.
      *
           MOVE    ZERO            TO  IDX2
           MOVE    ZERO            TO  WRK-HKNTEN
                                       WRK-FTNMONEY
                                       WRK-SKJMONEY
                                       WRK-JIHIMONEY
      *
           IF    ( SYU-SYUHKNNUM (1:2) =   "98" )
      *
      *        保険組合せ取得
               MOVE    SYU-HKNCOMBINUM TO  SRH-CD
               PERFORM 21011-HKNCOMBI-HEN-SEC
               MOVE    SRH-DAT         TO  WRK-HOJOKBN
      *
               COMPUTE WRK-JIHIMONEY   =   SYU-HKNTEKMONEY
                                       +   SYU-RJN-FTN
                                       +   SYU-KOH-FTN
                                       +   SYU-RSI-TOTAL
      *
                                       +   SYU-SKYMONEY-SKJ-KEI
                                       +   SYU-SKYMONEY-LIFE-KEI
      *
                                       +   SYU-RMSAGAKU
                                       +   SYU-SKYMONEY-SKJ-JIHI-KEI
                                       +   SYU-SKYMONEY-LIFE-JIHI-KEI
                                       +   SYU-TOTAL-TGMONEY
                                       +   SYU-TOTAL-TGMONEY-TAX
                                       +   SYU-JIHI-TOTAL
                                       +   SYU-JIHI-TOTAL-TAX
      *----(03.04.01)--UPD-START---
               IF      SYU-SRYYMD  <   CONST-H190401
                   COMPUTE WRK-JIHIMONEY   =   WRK-JIHIMONEY
                                           +   SYU-JIHI-TAX
                   IF    ( WRK-HOJOKBN     =   "1" )
                       COMPUTE WRK-JIHIMONEY   =   WRK-JIHIMONEY
                                               +   SYU-SKYMONEY-TAX-SAI
                   END-IF
               END-IF
      *----(03.04.01)--UPD-END-----
           ELSE
      *        保険点数
               COMPUTE WRK-HKNTEN      =   SYU-TOTAL-HKNTEN
      *
      *        保険分負担額
               COMPUTE WRK-FTNMONEY    =   SYU-HKNTEKMONEY
                                       +   SYU-RJN-FTN
                                       +   SYU-KOH-FTN
                                       +   SYU-RSI-TOTAL
      *
      *        食事・生活負担額
               COMPUTE WRK-SKJMONEY    =   SYU-SKYMONEY-SKJ-KEI
                                       +   SYU-SKYMONEY-LIFE-KEI
      *
      *        自費分負担額
               COMPUTE WRK-JIHIMONEY   =   SYU-RMSAGAKU
                                       +   SYU-SKYMONEY-SKJ-JIHI-KEI
                                       +   SYU-SKYMONEY-LIFE-JIHI-KEI
                                       +   SYU-TOTAL-TGMONEY
                                       +   SYU-TOTAL-TGMONEY-TAX
                                       +   SYU-JIHI-TOTAL
                                       +   SYU-JIHI-TOTAL-TAX
      *----(03.04.01)--UPD-START---
               IF      SYU-SRYYMD  <   CONST-H190401
                   COMPUTE WRK-JIHIMONEY   =   WRK-JIHIMONEY
                                           +   SYU-JIHI-TAX
               END-IF
           END-IF
      *
      *    入金編集処理
           PERFORM 220-NUKIN-HENSYU-SEC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1        >   WRK-HEN-MAX )
                    OR   ( IDX2    NOT =   ZERO )
      *
               IF    ( SYU-NYUGAIKBN
                                           =   WRK-NYUGAIKBN(IDX1) )
                AND  ( SYU-SRYYMD (1:6)    =   WRK-HIDUKE (IDX1) )
                   MOVE    IDX1    TO  IDX2
               END-IF
      *
           END-PERFORM
      *
           IF    ( IDX2            =   ZERO )
               IF    ( WRK-HEN-MAX >=  CONST-HEN-MAX )
                   GO  TO  2101-SRYKIKAN-HEN-EXT
               END-IF
      *
               COMPUTE WRK-HEN-MAX =   WRK-HEN-MAX     +   1
      *
               MOVE    WRK-HEN-MAX TO  IDX2
      *
               MOVE    SYU-NYUGAIKBN   TO  WRK-NYUGAIKBN (IDX2)
               MOVE    SYU-SRYYMD (1:6)
                                   TO  WRK-HIDUKE (IDX2)
      *
           END-IF
      *
           COMPUTE WRK-TENSU(IDX2)
               =   WRK-TENSU(IDX2)     +   WRK-HKNTEN
      *
           COMPUTE WRK-HOKEN(IDX2)
               =   WRK-HOKEN(IDX2)     +   WRK-FTNMONEY
      *
           COMPUTE WRK-SYOKUJI(IDX2)
               =   WRK-SYOKUJI(IDX2)   +   WRK-SKJMONEY
      *
           IF    ( ORCHCM31-DSPKBN =   "3" )
               CONTINUE
           ELSE
               COMPUTE WRK-JIHI(IDX2)
                   =   WRK-JIHI(IDX2)  +   WRK-JIHIMONEY
           END-IF
      *
           IF    ( ORCHCM31-DSPKBN =   "3" )
      *
               COMPUTE WRK-HKN-SKYMONEY
                   =   WRK-FTNMONEY
                   +   WRK-SKJMONEY
      *
               IF    ( ORCHCM31-SUMKBN     =   "1" )
                   IF    ( WRK-HKN-SKYMONEY
                                           >   HCM31S01-BFKINGAKU (1))
                       COMPUTE WRK-BFGAKU (IDX2)
                           =   WRK-BFGAKU (IDX2)
                           +   HCM31S01-BFKINGAKU (1)
                       COMPUTE WRK-HKN-SKYMONEY
                           =   WRK-HKN-SKYMONEY
                           -   HCM31S01-BFKINGAKU (1)
                   ELSE
                       COMPUTE WRK-BFGAKU (IDX2)
                           =   WRK-BFGAKU (IDX2) +   WRK-HKN-SKYMONEY
                       MOVE    ZERO        TO  WRK-HKN-SKYMONEY
                   END-IF
               END-IF
      *
               IF    ( WRK-HKN-SKYMONEY    >   HCM31S01-KINGAKU (1) )
                   COMPUTE WRK-GAKU (IDX2)
                       =   WRK-GAKU (IDX2) +   HCM31S01-KINGAKU (1)
               ELSE
                   COMPUTE WRK-GAKU (IDX2)
                       =   WRK-GAKU (IDX2) +   WRK-HKN-SKYMONEY
               END-IF
           ELSE
               COMPUTE WRK-GAKU (IDX2)
                   =   WRK-GAKU (IDX2)     +   HCM31S01-KINGAKU (1)
               IF    ( ORCHCM31-SUMKBN     =   "1" )
                   COMPUTE WRK-BFGAKU (IDX2)
                       =   WRK-BFGAKU (IDX2)
                       +   HCM31S01-BFKINGAKU (1)
               END-IF
           END-IF
      *
           .
       2101-SRYKIKAN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ情報取得処理
      *****************************************************************
       21011-HKNCOMBI-HEN-SEC          SECTION.
      *
           INITIALIZE                  SRH-DAT-G
      *
           PERFORM VARYING IDX-SRH FROM    1   BY  1
                   UNTIL ( IDX-SRH     >   SRH-COMB-MAX )
                    OR   ( SRH-DAT NOT =   SPACE )
      *
               IF  ( SRH-COMB-HKNCOMBINUML (IDX-SRH)
                                       =   SRH-CD )
                   MOVE    SRH-COMB-HOJOKBNL (IDX-SRH)
                                       TO  SRH-DAT
               END-IF
      *
           END-PERFORM
      *
           .
       21011-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    支払期間より編集処理
      *****************************************************************
       2101-SIHKIKAN-HEN-SEC           SECTION.
      *
      *    入金編集処理
           PERFORM 220-NUKIN-HENSYU-SEC
      *
      *    収納明細マスターがなくなるまで繰り返す
           PERFORM VARYING IDX3     FROM    1   BY  1
                   UNTIL ( IDX3     >   HCM31S01-OT-MAX )
      *
      *
               IF    ( HCM31S01-KINGAKU(IDX3)   >   ZERO )
      *
                   MOVE    ZERO            TO  IDX2
      *
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL ( IDX1    >   WRK-HEN-MAX )
                            OR   ( IDX2    NOT =   ZERO )
      *
                       IF    ( SYU-NYUGAIKBN
                                           =   WRK-NYUGAIKBN(IDX1) )
                        AND  ( HCM31S01-NYKNYMD(IDX3) (1:6)
                                           =   WRK-HIDUKE (IDX1) )
                           MOVE    IDX1    TO  IDX2
                       END-IF
      *
                   END-PERFORM
      *
                   IF    ( IDX2            =   ZERO )
                       IF    ( WRK-HEN-MAX <   CONST-HEN-MAX )
                           COMPUTE WRK-HEN-MAX =   WRK-HEN-MAX +   1
                           MOVE    WRK-HEN-MAX TO  IDX2
                       END-IF
                   END-IF
      *
                   IF    ( IDX2        NOT =   ZERO )
      *
                       MOVE    SYU-NYUGAIKBN
                                           TO  WRK-NYUGAIKBN (IDX2)
                       MOVE    HCM31S01-NYKNYMD(IDX3) (1:6)
                                           TO  WRK-HIDUKE (IDX2)
      *
                       COMPUTE WRK-GAKU (IDX2)
                           =   WRK-GAKU (IDX2)
                           +   HCM31S01-KINGAKU (IDX3)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       2101-SIHKIKAN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入金編集処理
      *****************************************************************
       220-NUKIN-HENSYU-SEC        SECTION.
      *
      *    編集用サブプロ読み込み
           INITIALIZE                  HCM31S01-AREA
      *
           EVALUATE    ORCHCM31-KIKANKBN
                               ALSO    ORCHCM31-SYORIKBN
      *      診療日付で期間外の入金も含める場合
             WHEN      "1"     ALSO    "1"
               MOVE    "1"             TO  HCM31S01-PROCKBN
      *      診療日付で期間外の入金は含めない場合
             WHEN      "1"     ALSO    "2"
               MOVE    "2"             TO  HCM31S01-PROCKBN
      *      支払日付の場合
             WHEN      "2"     ALSO    ANY
               MOVE    "3"             TO  HCM31S01-PROCKBN
           END-EVALUATE
      *
           MOVE    SYU-HOSPNUM      TO  HCM31S01-HOSPNUM
           MOVE    SYU-NYUGAIKBN   TO  HCM31S01-NYUGAIKBN
           MOVE    SYU-PTID        TO  HCM31S01-PTID
           MOVE    SYU-DENPNUM     TO  HCM31S01-DENPNUM
           MOVE    SYU-SRYYMD      TO  HCM31S01-SRYYMD
           MOVE    ORCHCM31-STRSRYYMD
                                   TO  HCM31S01-STYMD
           MOVE    ORCHCM31-ENDSRYYMD
                                   TO  HCM31S01-EDYMD
           MOVE    ORCHCM31-SUMKBN
                                   TO  HCM31S01-SUMKBN
      *    支払い金額サブプロ呼び出し
           CALL    "ORCHCM31S01"       USING
                                       HCM31S01-AREA
                                       SPA-AREA
      *
           .
       220-NYUKIN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    並び替え処理
      *****************************************************************
       230-NARABIKAE-SEC           SECTION.
      *
           MOVE    WRK-HEN-MAX     TO  IDY
      *
           PERFORM UNTIL   IDY =   ZERO
               PERFORM VARYING IDX FROM 1 BY 1
                       UNTIL   IDX >   IDY
                   IF      WRK-HENSYU-KEY (IDX)
                                   >   WRK-HENSYU-KEY (IDY)
                       MOVE    WRK-HENSYU-OCC(IDX)
                                       TO  WRK-HENSYU-OCC(801)
                       MOVE    WRK-HENSYU-OCC(IDY)
                                       TO  WRK-HENSYU-OCC(IDX)
                       MOVE    WRK-HENSYU-OCC(801)
                                       TO  WRK-HENSYU-OCC(IDY)
                   END-IF
               END-PERFORM
               COMPUTE IDY =   IDY -   1
           END-PERFORM
      *
           .
       230-NARABIKAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象データチェック処理
      *****************************************************************
       230-TAISYO-CHK-SEC              SECTION.
      *
           MOVE    1               TO  FLG-TAISYO
      *
           IF    ( WRK-TENSU    (IDX4) =   ZERO )
            AND  ( WRK-HOKEN    (IDX4) =   ZERO )
            AND  ( WRK-SYOKUJI  (IDX4) =   ZERO )
            AND  ( WRK-JIHI     (IDX4) =   ZERO )
            AND  ( WRK-GAKU     (IDX4) =   ZERO )
                   MOVE    ZERO    TO  FLG-TAISYO
           END-IF
      *
           IF    ( ORCHCM31-KIKANKBN   =   "1"   )
            AND  ( ORCHCM31-DSPKBN     =   "2"   )
            AND  ( WRK-GAKU (IDX4) =   ZERO )
                   MOVE    ZERO    TO  FLG-TAISYO
           END-IF
      *
           IF    ( ORCHCM31-KIKANKBN   =   "1"   )
            AND  ( ORCHCM31-SUMKBN     =   "1"   )
            AND  ( WRK-GAKU (IDX4) =   ZERO )
                   MOVE    ZERO    TO  FLG-TAISYO
           END-IF
      *
           .
       230-TAISYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ヘッダ編集処理
      *****************************************************************
       240-HEAD-HEN-SEC            SECTION.
      *
           COMPUTE CNT-PAGE            =   CNT-PAGE    +   1
      *
           INITIALIZE                  HCM32N
      *
      *    開始診療日
           MOVE    ORCHCM31-STRSRYYMD  TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  HCM32N-STRSRYYMD
      *
      *    終了診療日
           MOVE    ORCHCM31-ENDSRYYMD  TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  HCM32N-ENDSRYYMD
      *
      *    患者番号
           MOVE    PTNUM-PTNUM         TO  HCM32N-PTNUM
      *
      *    患者名称
           MOVE    PTINF-NAME          TO  HCM32N-PTNAME
      *
      *    発行日
           MOVE    SPA-SYSYMD          TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  HCM32N-HAKKOYMD
      *
      *
      *    医療機関住所
           IF    ( SYS-1002-ADRS (61:) =   SPACE )
               MOVE    SYS-1002-ADRS   TO  HCM32N-HOSPADRS
           ELSE
               MOVE    SYS-1002-ADRS   TO  HCM32N-HOSPADRS2
           END-IF
      *
      *    医療機関名
           MOVE    SYS-1001-HOSPNAME   TO  HCM32N-HOSPNAME
      *
      *    医療機関ＴＥＬ
           MOVE    SYS-1002-TEL        TO  HCM32N-HOSPTEL
      *
           IF    ( ORCHCM31-KIKANKBN   =   "2" )
               MOVE    "支払月"        TO  HCM32N-LABELYM
           ELSE
               MOVE    "診療月"        TO  HCM32N-LABELYM
           END-IF
      *
           IF    ( ORCHCM31-SUMKBN     =   "1" )
               MOVE    CNT-PAGE        TO  WRK-Z3
               STRING  WRK-Z3          DELIMITED   BY  SIZE
                       "頁"            DELIMITED   BY  SIZE
               INTO    HCM32N-PAGE
               END-STRING
           END-IF
      *
           .
       240-HEAD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       240-MEISAI-HEN-SEC              SECTION.
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
           IF    ( CNT-LINE    =   1 )
               PERFORM 240-HEAD-HEN-SEC
           END-IF
      *
      *    日付
           MOVE    WRK-HIDUKE(IDX4)    TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           IF    ( ORCHCM31-SUMKBN     =   "1" )
               MOVE    WRK-HENYMDG(1:10)
                                       TO  HCM32N-SRYYY(CNT-LINE)
               STRING  "　　"              DELIMITED   BY  SIZE
                       WRK-HENYMDG(11:6)   DELIMITED   BY  SIZE
               INTO    HCM32N-SRYMM(CNT-LINE)
               END-STRING
           ELSE
               MOVE    WRK-HENYMDG(11:6)
                                      TO  HCM32N-SRYYYMM(CNT-LINE)
           END-IF
      *
           IF    ( ORCHCM31-KIKANKBN   =   "1"   )
            AND  ( ORCHCM31-DSPKBN     =   "1" OR "3" )
      *
      *        保険点数
               IF      WRK-TENSU(IDX4)     NOT =   ZERO
                   MOVE    WRK-TENSU(IDX4) TO WRK-Z7
                   MOVE    WRK-Z7          TO HCM32N-HKNTEN  (CNT-LINE)
                   MOVE    "点"            TO HCM32N-TEN     (CNT-LINE)
               END-IF
      *
      *        保険分負担額
               IF      WRK-HOKEN(IDX4)     NOT =   ZERO
                   MOVE    WRK-HOKEN(IDX4) TO WRK-Z9
                   MOVE    WRK-Z9          TO HCM32N-FTNMONEY(CNT-LINE)
                   MOVE    "円"            TO HCM32N-HYEN    (CNT-LINE)
               END-IF
      *
      *        食事・生活負担額
               IF      WRK-SYOKUJI(IDX4)   NOT =   ZERO
                   MOVE    WRK-SYOKUJI(IDX4) TO  WRK-Z9
                   MOVE    WRK-Z9          TO HCM32N-SKJMONEY(CNT-LINE)
                   MOVE    "円"            TO HCM32N-SYEN    (CNT-LINE)
               END-IF
      *
      *        自費分負担額
               IF      WRK-JIHI(IDX4)      NOT =   ZERO
                   MOVE    WRK-JIHI(IDX4)  TO WRK-Z9
                   MOVE    WRK-Z9          TO HCM32N-JIHIMONEY(CNT-LINE)
                   MOVE    "円"            TO HCM32N-JYEN     (CNT-LINE)
               END-IF
           END-IF
      *
      *    入返金額
           IF    ( WRK-GAKU(IDX4)      NOT =   ZERO )
               MOVE    WRK-GAKU(IDX4)  TO  WRK-Z9
               MOVE    WRK-Z9          TO  HCM32N-SKYMONEY(CNT-LINE)
               MOVE    "円"            TO  HCM32N-YEN     (CNT-LINE)
           END-IF
      *
           IF    ( ORCHCM31-SUMKBN     =   "1" )
               IF    ( WRK-BFGAKU(IDX4)    NOT =   ZERO )
                   MOVE    "期間前支払金額"
                                       TO  HCM32N-BFNYUKINMSG(CNT-LINE)
                   MOVE    WRK-BFGAKU(IDX4)
                                       TO  WRK-Z9
                   MOVE    WRK-Z9      TO  HCM32N-BFNYUKIN(CNT-LINE)
                   MOVE    "円"        TO  HCM32N-BFYEN   (CNT-LINE)
               END-IF
           END-IF
      *
           COMPUTE WRK-GHKNTEN
               =   WRK-GHKNTEN     +   WRK-TENSU(IDX4)
      *
           COMPUTE WRK-GFTNMONEY
               =   WRK-GFTNMONEY   +   WRK-HOKEN(IDX4)
      *
           COMPUTE WRK-GSKJMONEY
               =   WRK-GSKJMONEY   +   WRK-SYOKUJI(IDX4)
      *
           COMPUTE WRK-GJIHIMONEY
               =   WRK-GJIHIMONEY  +   WRK-JIHI(IDX4)
      *
           COMPUTE WRK-GSKYMONEY
               =   WRK-GSKYMONEY   +   WRK-GAKU(IDX4)
      *
           .
       240-MEISAI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    合計行編集処理
      *****************************************************************
       240-GOKEI-HEN-SEC               SECTION.
      *
           IF    ( ORCHCM31-KIKANKBN   =   "1"   )
            AND  ( ORCHCM31-DSPKBN     =   "1" OR "3"  )
      *
               MOVE    WRK-GHKNTEN     TO  WRK-Z7
               MOVE    WRK-Z7          TO  HCM32N-GHKNTEN
      *
               MOVE    WRK-GFTNMONEY   TO  WRK-Z9
               MOVE    WRK-Z9          TO  HCM32N-GFTNMONEY
      *
               MOVE    WRK-GSKJMONEY   TO  WRK-Z9
               MOVE    WRK-Z9          TO  HCM32N-GSKJMONEY
      *
               MOVE    WRK-GJIHIMONEY  TO  WRK-Z9
               MOVE    WRK-Z9          TO  HCM32N-GJIHIMONEY
           END-IF
      *
      *
      *    合計入返金額
           MOVE    WRK-GSKYMONEY   TO  WRK-Z9
           MOVE    WRK-Z9          TO  HCM32N-GSKYMONEY
      *
           .
       240-GOKEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票出力処理
      *****************************************************************
       250-PRINT-SEC                   SECTION.
      *
           IF    ( KEY-NYUGAIKBN       =   "1" )
               MOVE    "HCM32N.red"    TO  SPRT-PRTID
               MOVE    HCM32N          TO  SPRT-PRTDATA
           ELSE
               PERFORM 2501-HCM32G-HEN-SEC
               MOVE    "HCM32G.red"    TO  SPRT-PRTID
               MOVE    HCM32G          TO  SPRT-PRTDATA
           END-IF
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF    ( SPRT-RETURN         =   ZERO )
               COMPUTE CNT-OUT =   CNT-OUT +   1
           ELSE
               DISPLAY "ORCSPRT ERR !!! PTID = " SPA-PTID
           END-IF
      *
           MOVE    ZERO            TO  CNT-LINE
      *
           .
       250-PRT-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来支払証明書編集処理
      *****************************************************************
       2501-HCM32G-HEN-SEC             SECTION.
      *
           INITIALIZE                      HCM32G
      *
           MOVE    HCM32N-PAGE         TO  HCM32G-PAGE
           MOVE    HCM32N-STRSRYYMD    TO  HCM32G-STRSRYYMD
           MOVE    HCM32N-ENDSRYYMD    TO  HCM32G-ENDSRYYMD
           MOVE    HCM32N-PTNUM        TO  HCM32G-PTNUM
           MOVE    HCM32N-PTNAME       TO  HCM32G-PTNAME
           MOVE    HCM32N-HAKKOYMD     TO  HCM32G-HAKKOYMD
           MOVE    HCM32N-LABELYM      TO  HCM32G-LABELYM
      *
           PERFORM VARYING IDX-LINE    FROM    1   BY  1
                   UNTIL ( IDX-LINE    >   CONST-LINE-MAX )
      *
                MOVE    HCM32N-SRYYYMM (IDX-LINE)
                                       TO  HCM32G-SRYYYMM  (IDX-LINE)
                MOVE    HCM32N-SRYYY   (IDX-LINE)
                                       TO  HCM32G-SRYYY    (IDX-LINE)
                MOVE    HCM32N-SRYMM   (IDX-LINE)
                                       TO  HCM32G-SRYMM    (IDX-LINE)
                MOVE    HCM32N-HKNTEN   (IDX-LINE)
                                       TO  HCM32G-HKNTEN   (IDX-LINE)
                MOVE    HCM32N-TEN      (IDX-LINE)
                                       TO  HCM32G-TEN      (IDX-LINE)
                MOVE    HCM32N-FTNMONEY (IDX-LINE)
                                       TO  HCM32G-FTNMONEY (IDX-LINE)
                MOVE    HCM32N-HYEN     (IDX-LINE)
                                       TO  HCM32G-HYEN     (IDX-LINE)
                MOVE    HCM32N-JIHIMONEY(IDX-LINE)
                                       TO  HCM32G-JIHIMONEY(IDX-LINE)
                MOVE    HCM32N-JYEN     (IDX-LINE)
                                       TO  HCM32G-JYEN     (IDX-LINE)
                MOVE    HCM32N-SKYMONEY (IDX-LINE)
                                       TO  HCM32G-SKYMONEY (IDX-LINE)
                MOVE    HCM32N-YEN      (IDX-LINE)
                                       TO  HCM32G-YEN      (IDX-LINE)
                MOVE    HCM32N-BFNYUKINMSG (IDX-LINE)
                                       TO  HCM32G-BFNYUKINMSG
                                                           (IDX-LINE)
                MOVE    HCM32N-BFNYUKIN (IDX-LINE)
                                       TO  HCM32G-BFNYUKIN
                                                           (IDX-LINE)
                MOVE    HCM32N-BFYEN (IDX-LINE)
                                       TO  HCM32G-BFYEN
                                                           (IDX-LINE)
      *
            END-PERFORM
      *
            MOVE    HCM32N-GHKNTEN     TO  HCM32G-GHKNTEN
            MOVE    HCM32N-GFTNMONEY   TO  HCM32G-GFTNMONEY
            MOVE    HCM32N-GJIHIMONEY  TO  HCM32G-GJIHIMONEY
            MOVE    HCM32N-GSKYMONEY   TO  HCM32G-GSKYMONEY
            MOVE    HCM32N-HOSPADRS    TO  HCM32G-HOSPADRS
            MOVE    HCM32N-HOSPADRS2   TO  HCM32G-HOSPADRS2
            MOVE    HCM32N-HOSPNAME    TO  HCM32G-HOSPNAME
            MOVE    HCM32N-HOSPTEL     TO  HCM32G-HOSPTEL
      *
           .
       2501-HCM32G-HEN-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           MOVE    CNT-OUT         TO  ORCHCM31-OT-CNT
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC         SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理基本情報読み込み
      *****************************************************************
       900-SYS1001-READ-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPACE           TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    SPA-HOSPNUM     TO  SYS-1001-HOSPNUM
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    "*"             TO  SYS-1001-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-1001-STYUKYMD
                                       SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYS-1001-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYS-1001-REC
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYS1001-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理所在地読み込み
      *****************************************************************
       900-SYS1002-READ-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPACE           TO  SYS-1002-REC
           INITIALIZE                  SYS-1002-REC
           MOVE    SPA-HOSPNUM     TO  SYS-1002-HOSPNUM
           MOVE    "1002"          TO  SYS-1002-KANRICD
           MOVE    "*"             TO  SYS-1002-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-1002-STYUKYMD
                                       SYS-1002-EDYUKYMD
           MOVE    SYS-1002-REC    TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYS-1002-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYS-1002-REC
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYS1002-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号読み込み
      *****************************************************************
       900-PTNUM-READ-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNUM
      *
           INITIALIZE                  PTNUM-REC
           MOVE    SPA-HOSPNUM      TO  PTNUM-HOSPNUM
           MOVE    SPA-PTID        TO  PTNUM-PTID
           MOVE    PTNUM-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNUM-REC
           ELSE
               MOVE    1           TO  FLG-PTNUM
               INITIALIZE              PTNUM-REC
           END-IF
      *
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報読み込み
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM      TO  PTINF-HOSPNUM
           MOVE    SPA-PTID        TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTINF-REC
           ELSE
               MOVE    1           TO  FLG-PTINF
               INITIALIZE              PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスターSELECT
      *****************************************************************
       900-SYUNOU-SELECT-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    SPACE           TO  SYUNOU-REC
           INITIALIZE                  SYUNOU-REC
      *
           MOVE    SPA-HOSPNUM          TO  SYU-HOSPNUM
           MOVE    ORCHCM31-PTID       TO  SYU-PTID
           MOVE    ORCHCM31-STRSRYYMD  TO  SYU-SKYSTYMD
           MOVE    ORCHCM31-ENDSRYYMD  TO  SYU-SKYEDYMD
      *
           EVALUATE    ORCHCM31-NYUGAI
               WHEN    "0"
                   MOVE    "%"         TO  SYU-NYUGAIKBN
               WHEN    "1"
                   MOVE    "1"         TO  SYU-NYUGAIKBN
               WHEN    "2"
                   MOVE    "2"         TO  SYU-NYUGAIKBN
           END-EVALUATE
      *
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
      *
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 900-SYUNOU-READ-SEC
           ELSE
               MOVE    1           TO  FLG-SYUNOU
               INITIALIZE              SYUNOU-REC
           END-IF
      *
           .
       900-SYUNOU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスターFETCH
      *****************************************************************
       900-SYUNOU-READ-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-LOOP-END
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           PERFORM UNTIL ( FLG-SYUNOU      NOT =   ZERO )
                    OR   ( FLG-LOOP-END    NOT =   ZERO )
      *
               MOVE    WRK-TABLE       TO  MCP-TABLE
               MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  SYUNOU-REC
      *
                   MOVE    1           TO  FLG-LOOP-END
      *
                   IF    ( ORCHCM31-KIKANKBN   =   "1" )
                       IF    ( ORCHCM31-SUMKBN =   "2" )
                           IF    ( SYU-SRYYMD >=   ORCHCM31-STRSRYYMD )
                            AND  ( SYU-SRYYMD <=   ORCHCM31-ENDSRYYMD )
                               CONTINUE
                           ELSE
                               MOVE    ZERO    TO  FLG-LOOP-END
                           END-IF
                       END-IF
                   END-IF
      *
               ELSE
                   MOVE    1               TO  FLG-SYUNOU
                   INITIALIZE              SYUNOU-REC
               END-IF
           END-PERFORM
      *
           .
       900-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ取得処理(KEY2)
      *****************************************************************
       900-HKNCOMBI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNCOMBI
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM          TO  COMB-HOSPNUM
           MOVE    ORCHCM31-PTID       TO  COMB-PTID
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       900-HKNCOMBI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せFETCH処理(KEY2)
      *****************************************************************
       900-HKNCOMBI-KEY2-FET-SEC       SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       900-HKNCOMBI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC          SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF        ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       911-DBSELECT-SEC          SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       910-DBFETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.
