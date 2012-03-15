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
       PROGRAM-ID.                 ORCSNYULIFE.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : 入院時生活療養費負担額取得サブ
      *                      （平成１８年１０月改正対応）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/09/12    NACL−小豆沢  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07/05/10  グループ診療対応
      *              NACL-小豆沢  07/07/12  患者所得情報の取得をサブ
      *                                     ルーチンに変更
      *  04.01.00    NACL-小豆沢  07/12/17  短期滞在手術基本料２の入院
      *                                     翌日食事算定対応
      *平成２０年４月改定対応
      *  04.02.00    NACL-小豆沢  08/03/05  後期高齢者の判定変更
      *                                     生活療養の入院料判定追加
      *                                       短期滞在手術基本料３
      *                                       回復期リハビリテーション病棟入院料１
      *                                       回復期リハビリテーション病棟入院料２
      *  04.02.00    NACL-小豆沢  08/04/11  後期高齢者の判定をサブルーチン使用
      *                                     に変更
      *  04.02.00    NACL-小豆沢  08/05/02  特別入院基本料算定時にＡＤＬ入力から
      *                                     医療度の高い患者を判定
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
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-NYUINACT                PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-PTHKNINF                PIC 9(01).
           03  FLG-PTKOHINF                PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-OK                      PIC 9(01).
           03  FLG-TSYRRK                  PIC 9(01).
           03  FLG-TNKRRK                  PIC 9(01).
           03  FLG-RJN                     PIC 9(01).
           03  FLG-SEI                     PIC 9(01).
      *    添字領域
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
           03  IDX1                        PIC 9(04).
           03  TSY90-IDX                   PIC 9(02).
           03  IDX-RJN                     PIC 9(02).
           03  IDX-SEI                     PIC 9(04).
           03  IDX-SEI2                    PIC 9(04).
      *
      *    コンスタント値
      *    生活療養に関する入院診療コード
       01  CONSTANT-AREA.
      *    療養病棟入院基本料２
           03   CONS-RYOYO2A               PIC X(09) VALUE "190123710".
           03   CONS-RYOYO2B               PIC X(09) VALUE "190123810".
           03   CONS-RYOYO2C               PIC X(09) VALUE "190123910".
           03   CONS-RYOYO2D               PIC X(09) VALUE "190124010".
           03   CONS-RYOYO2E               PIC X(09) VALUE "190124110".
           03   CONS-RYOYO2TOKU            PIC X(09) VALUE "190124210".
      *    有床診療所療養病床入院基本料２
           03   CONS-SRYOYO2A              PIC X(09) VALUE "190124310".
           03   CONS-SRYOYO2B              PIC X(09) VALUE "190124410".
           03   CONS-SRYOYO2C              PIC X(09) VALUE "190124510".
           03   CONS-SRYOYO2D              PIC X(09) VALUE "190124610".
           03   CONS-SRYOYO2E              PIC X(09) VALUE "190124710".
           03   CONS-SRYOYO2TOKU           PIC X(09) VALUE "190124810".
      *    回復期リハビリテーション病棟入院料
           03   CONS-KAIFUKU               PIC X(09) VALUE "190124910".
      *    回復期リハビリテーション病棟入院料１（平成２２年以降は基準未適合）
           03   CONS-KAIFUKU1              PIC X(09) VALUE "190129510".
      *    回復期リハビリテーション病棟入院料２（平成２２年以降は基準未適合）
           03   CONS-KAIFUKU2              PIC X(09) VALUE "190129710".
      *    診療所老人医療管理料
           03   CONS-ROUKAN1               PIC X(09) VALUE "190125010".
           03   CONS-ROUKAN2               PIC X(09) VALUE "190125110".
           03   CONS-ROUKAN3               PIC X(09) VALUE "190125210".
      *    短期滞在手術基本料２
           03   CONS-TANKI2                PIC X(09) VALUE "190125310".
      *    短期滞在手術基本料３
           03   CONS-TANKI3                PIC X(09) VALUE "190130510".
      *
      *    平成２２年度改定追加分
      *    療養病棟入院基本料１（ＦからＧの生活療養）
           03   CONS-H22-RYOYO1F           PIC X(09) VALUE "190131710".
           03   CONS-H22-RYOYO1G           PIC X(09) VALUE "190131910".
           03   CONS-H22-RYOYO1H           PIC X(09) VALUE "190132110".
           03   CONS-H22-RYOYO1I           PIC X(09) VALUE "190132310".
      *    療養病棟入院基本料２（ＡからＩの生活療養）
           03   CONS-H22-RYOYO2A           PIC X(09) VALUE "190132510".
           03   CONS-H22-RYOYO2B           PIC X(09) VALUE "190132710".
           03   CONS-H22-RYOYO2C           PIC X(09) VALUE "190132910".
           03   CONS-H22-RYOYO2D           PIC X(09) VALUE "190133110".
           03   CONS-H22-RYOYO2E           PIC X(09) VALUE "190133310".
           03   CONS-H22-RYOYO2F           PIC X(09) VALUE "190133510".
           03   CONS-H22-RYOYO2G           PIC X(09) VALUE "190133710".
           03   CONS-H22-RYOYO2H           PIC X(09) VALUE "190133910".
           03   CONS-H22-RYOYO2I           PIC X(09) VALUE "190134110".
      *    回復期リハビリテーション病棟入院料１（生活療養）
           03   CONS-H22-KAIFUKU1          PIC X(09) VALUE "190141710".
      *    回復期リハビリテーション病棟入院料２（生活療養）
           03   CONS-H22-KAIFUKU2          PIC X(09) VALUE "190141910".
      *
      *    平成２４年度改定生活療養判定用
      *  （医療度の高い生活療養を上位にまとめているので、データの並びに注意すること）
      *  （特別入院基本料は特別な判定があることからデータの最後とする）
       01  TBL-H24-SEIKATSU-AREA.
      *  医療度の高い生活療養
      *    回復期リハビリテーション病棟入院料１（生活療養）
           03   FILLER                     PIC X(09) VALUE "190150210".
      *    回復期リハビリテーション病棟入院料１（生活療養）（栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190719610".
      *    回復期リハビリテーション病棟入院料２（生活療養）
           03   FILLER                     PIC X(09) VALUE "190141710".
      *    回復期リハビリテーション病棟入院料２（生活療養）（栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190719810".
      *    回復期リハビリテーション病棟入院料３（生活療養）
           03   FILLER                     PIC X(09) VALUE "190141910".
      *    回復期リハビリテーション病棟入院料３（生活療養）（栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190720010".
      *    短期滞在手術基本料２（生活療養）
           03   FILLER                     PIC X(09) VALUE "190125310".
      *    短期滞在手術基本料３（生活療養）
           03   FILLER                     PIC X(09) VALUE "190130510".
      *    有床診療所療養病床入院基本料Ａ（生活療養）
           03   FILLER                     PIC X(09) VALUE "190124310".
      *10  有床診療所療養病床入院基本料Ａ（生活療養）（栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190711510".
      *    有床診療所療養病床入院基本料Ｂ（生活療養）
           03   FILLER                     PIC X(09) VALUE "190124410".
      *    有床診療所療養病床入院基本料Ｂ（生活療養）（栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190711710".
      *    有床診療所療養病床入院基本料Ｃ（生活療養）
           03   FILLER                     PIC X(09) VALUE "190124510".
      *    有床診療所療養病床入院基本料Ｃ（生活療養）（栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190711910".
      *    療養病床以外・有床診療所療養病床入院Ａ（生活療養）
           03   FILLER                     PIC X(09) VALUE "190155410".
      *    療養病床以外・有床診療所療養病床入院Ａ（生活療養・栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190155510".
      *    療養病床以外・有床診療所療養病床入院Ｂ（生活療養）
           03   FILLER                     PIC X(09) VALUE "190155810".
      *    療養病床以外・有床診療所療養病床入院Ｂ（生活療養・栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190155910".
      *    療養病床以外・有床診療所療養病床入院Ｃ（生活療養）
           03   FILLER                     PIC X(09) VALUE "190156210".
      *20  療養病床以外・有床診療所療養病床入院Ｃ（生活療養・栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190156310".
      *    療養病棟入院基本料１（入院基本料Ａ）（生活療養）
           03   FILLER                     PIC X(09) VALUE "190123710".
      *    療養病棟入院基本料１（入院基本料Ａ）（生活療養）（栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190701010".
      *    療養病棟入院基本料１（入院基本料Ｂ）（生活療養）
           03   FILLER                     PIC X(09) VALUE "190123810".
      *    療養病棟入院基本料１（入院基本料Ｂ）（生活療養）（栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190701210".
      *    療養病棟入院基本料１（入院基本料Ｃ）（生活療養）
           03   FILLER                     PIC X(09) VALUE "190123910".
      *    療養病棟入院基本料１（入院基本料Ｃ）（生活療養）（栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190701410".
      *    療養病棟入院基本料１（入院基本料Ｄ）（生活療養）
           03   FILLER                     PIC X(09) VALUE "190124010".
      *    療養病棟入院基本料１（入院基本料Ｄ）（生活療養）（栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190701610".
      *    療養病棟入院基本料１（入院基本料Ｅ）（生活療養）
           03   FILLER                     PIC X(09) VALUE "190124110".
      *30  療養病棟入院基本料１（入院基本料Ｅ）（生活療養）（栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190701810".
      *    療養病棟入院基本料１（入院基本料Ｆ）（生活療養）
           03   FILLER                     PIC X(09) VALUE "190131710".
      *    療養病棟入院基本料１（入院基本料Ｆ）（生活療養）（栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190702010".
      *    療養病棟入院基本料２（入院基本料Ａ）（生活療養）
           03   FILLER                     PIC X(09) VALUE "190132510".
      *    療養病棟入院基本料２（入院基本料Ａ）（生活療養）（栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190702810".
      *    療養病棟入院基本料２（入院基本料Ｂ）（生活療養）
           03   FILLER                     PIC X(09) VALUE "190132710".
      *    療養病棟入院基本料２（入院基本料Ｂ）（生活療養）（栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190703010".
      *    療養病棟入院基本料２（入院基本料Ｃ）（生活療養）
           03   FILLER                     PIC X(09) VALUE "190132910".
      *    療養病棟入院基本料２（入院基本料Ｃ）（生活療養）（栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190703210".
      *    療養病棟入院基本料２（入院基本料Ｄ）（生活療養）
           03   FILLER                     PIC X(09) VALUE "190133110".
      *40  療養病棟入院基本料２（入院基本料Ｄ）（生活療養）（栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190703410".
      *    療養病棟入院基本料２（入院基本料Ｅ）（生活療養）
           03   FILLER                     PIC X(09) VALUE "190133310".
      *    療養病棟入院基本料２（入院基本料Ｅ）（生活療養）（栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190703610".
      *   療養病棟入院基本料２（入院基本料Ｆ）（生活療養）
           03   FILLER                     PIC X(09) VALUE "190133510".
      *    療養病棟入院基本料２（入院基本料Ｆ）（生活療養）（栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190703810".
      *    特定一般病棟・療養病棟入院基本料１（Ａ）（生活療養）
           03   FILLER                     PIC X(09) VALUE "190157410".
      *    特定一般病棟・療養病棟入院基本料１（Ａ・生活療養・栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190157510".
      *    特定一般病棟・療養病棟入院基本料１（Ｂ）（生活療養）
           03   FILLER                     PIC X(09) VALUE "190157810".
      *    特定一般病棟・療養病棟入院基本料１（Ｂ・生活療養・栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190157910".
      *    特定一般病棟・療養病棟入院基本料１（Ｃ）（生活療養）
           03   FILLER                     PIC X(09) VALUE "190158210".
      *50  特定一般病棟・療養病棟入院基本料１（Ｃ・生活療養・栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190158310".
      *    特定一般病棟・療養病棟入院基本料１（Ｄ）（生活療養）
           03   FILLER                     PIC X(09) VALUE "190158610".
      *    特定一般病棟・療養病棟入院基本料１（Ｄ・生活療養・栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190158710".
      *   特定一般病棟・療養病棟入院基本料１（Ｅ）（生活療養）
           03   FILLER                     PIC X(09) VALUE "190159010".
      *    特定一般病棟・療養病棟入院基本料１（Ｅ・生活療養・栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190159110".
      *    特定一般病棟・療養病棟入院基本料１（Ｆ）（生活療養）
           03   FILLER                     PIC X(09) VALUE "190159410".
      *56  特定一般病棟・療養病棟入院基本料１（Ｆ・生活療養・栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190159510".
      *
      *  医療度の高くない生活療養
      *57  有床診療所療養病床入院基本料Ｄ（生活療養）
           03   FILLER                     PIC X(09) VALUE "190124610".
      *    有床診療所療養病床入院基本料Ｄ（生活療養）（栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190712110".
      *    有床診療所療養病床入院基本料Ｅ（生活療養）
           03   FILLER                     PIC X(09) VALUE "190124710".
      *60  有床診療所療養病床入院基本料Ｅ（生活療養）（栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190712310".
      *    療養病床以外・有床診療所療養病床入院Ｄ（生活療養）
           03   FILLER                     PIC X(09) VALUE "190156610".
      *    療養病床以外・有床診療所療養病床入院Ｄ（生活療養・栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190156710".
      *    療養病床以外・有床診療所療養病床入院Ｅ（生活療養・栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190157110".
      *    療養病床以外・有床診療所療養病床入院Ｅ（生活療養）
           03   FILLER                     PIC X(09) VALUE "190157010".
      *    療養病棟入院基本料１（入院基本料Ｇ）（生活療養）
           03   FILLER                     PIC X(09) VALUE "190131910".
      *    療養病棟入院基本料１（入院基本料Ｇ）（生活療養）（栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190702210".
      *    療養病棟入院基本料１（入院基本料Ｈ）（生活療養）
           03   FILLER                     PIC X(09) VALUE "190132110".
      *    療養病棟入院基本料１（入院基本料Ｈ）（生活療養）（栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190702410".
      *    療養病棟入院基本料１（入院基本料Ｉ）（生活療養）
           03   FILLER                     PIC X(09) VALUE "190132310".
      *70  療養病棟入院基本料１（入院基本料Ｉ）（生活療養）（栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190702610".
      *    療養病棟入院基本料２（入院基本料Ｇ）（生活療養）
           03   FILLER                     PIC X(09) VALUE "190133710".
      *    療養病棟入院基本料２（入院基本料Ｇ）（生活療養）（栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190704010".
      *    療養病棟入院基本料２（入院基本料Ｈ）（生活療養）
           03   FILLER                     PIC X(09) VALUE "190133910".
      *    療養病棟入院基本料２（入院基本料Ｈ）（生活療養）（栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190704210".
      *    療養病棟入院基本料２（入院基本料Ｉ）（生活療養）
           03   FILLER                     PIC X(09) VALUE "190134110".
      *    療養病棟入院基本料２（入院基本料Ｉ）（生活療養）（栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190704410".
      *    特定一般病棟・療養病棟入院基本料１（Ｇ）（生活療養）
           03   FILLER                     PIC X(09) VALUE "190159810".
      *    特定一般病棟・療養病棟入院基本料１（Ｇ・生活療養・栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190159910".
      *    特定一般病棟・療養病棟入院基本料１（Ｈ）（生活療養）
           03   FILLER                     PIC X(09) VALUE "190160210".
      *80  特定一般病棟・療養病棟入院基本料１（Ｈ・生活療養・栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190160310".
      *    特定一般病棟・療養病棟入院基本料１（Ｉ）（生活療養）
           03   FILLER                     PIC X(09) VALUE "190160610".
      *82  特定一般病棟・療養病棟入院基本料１（Ｉ・生活療養・栄管経過措置）
           03   FILLER                     PIC X(09) VALUE "190160710".
      *
      *83  有床診療所療養病床入院基本料（特別入院基本料）（生活療養）
           03   FILLER                     PIC X(09) VALUE "190124810".
      *84  療養病棟入院基本料（特別入院基本料）（生活療養）
           03   FILLER                     PIC X(09) VALUE "190124210".
      *
       01  TBL-H24-SEIKATSU-R     REDEFINES  TBL-H24-SEIKATSU-AREA.
           03   TBL-H24-SEISRYCD           PIC X(09)  OCCURS  84.
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-YMD.
               05  WRK-YYMM                PIC X(06).
               05  WRK-DD                  PIC 9(02).
           03  WRK-RJN-TBL.
               05  WRK-RJN                 PIC 9(01)   OCCURS   31.
      *    
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK5000.INC".
      *
      *    入院診療会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院診療行為マスター
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者公費情報マスタ−
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    患者保険情報マスタ−
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    低所得
       01  TSYRRK-REC.
           COPY    "CPTSYRRK.INC".
      *
      *    年金
       01  TNKRRK-REC.
           COPY    "CPTNKRRK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    患者所得情報編集サブ
           COPY    "CPORCSSYOTOKU.INC".
      *
      *    入院料未算定日取得サブ
           COPY    "CPORCSNYUINDAY.INC".
      *
      *    医療区分取得サブ
           COPY    "CPORCSS008.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    入院会計作成用パラメタ
           COPY    "CPORCSNYULIFE.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSNYULIFE-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  LNK-NYULIFE-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-NYULIFE-RC
      *
      *    パラメータチェック
           PERFORM 100-PRM-CHK-SEC
           IF    ( LNK-NYULIFE-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    主処理
           IF      LNK-NYULIFE-RC      =    ZERO
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
      *    パラメータチェック
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    算定年月チェック
           IF      LNK-NYULIFE-SANTEIYM    =   SPACE
               MOVE    1               TO  LNK-NYULIFE-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    患者マスター検索
           INITIALIZE                       PTINF-REC
           MOVE    LNK-NYULIFE-HOSPNUM  TO  PTINF-HOSPNUM
           MOVE    LNK-NYULIFE-PTID     TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF            NOT =   ZERO
               MOVE    2                TO  LNK-NYULIFE-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    入院中のチェック（入院会計マスター検索）
           INITIALIZE                      NYUINACCT-REC
           MOVE    LNK-NYULIFE-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-NYULIFE-PTID     TO  NACCT-PTID
           MOVE    LNK-NYULIFE-SANTEIYM TO  NACCT-SRYYM
           MOVE    "5"                  TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
           MOVE    "key17"              TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT        NOT =   ZERO
               MOVE    3                TO  LNK-NYULIFE-RC
           ELSE
               MOVE    "tbl_nyuinacct"  TO  MCP-TABLE
               MOVE    "key17"          TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT       NOT =   ZERO
                    MOVE    3           TO  LNK-NYULIFE-RC
               END-IF
           END-IF
      *
      *    入院会計クローズ
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC-SEC
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主取得
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
      *    医療区分取得サブの呼出し（特別入院基本料算定時の医療区分判定用）
           INITIALIZE                      SS008-AREA
           MOVE    LNK-NYULIFE-HOSPNUM     TO  SPA-HOSPNUM
           MOVE    LNK-NYULIFE-PTID        TO  SPA-PTID
           MOVE    LNK-NYULIFE-SANTEIYM    TO  SS008-SRYYM
           CALL   "ORCSS008"       USING   SS008-AREA
                                           SPA-AREA
      *
      *    入院時生活療養費の該当日と軽減区分２（入院料判定）を取得
           MOVE    ZERO                    TO  FLG-OK
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-NYULIFE-HOSPNUM     TO  NACCT-HOSPNUM
           MOVE    LNK-NYULIFE-PTID        TO  NACCT-PTID
           MOVE    LNK-NYULIFE-SANTEIYM    TO  NACCT-SRYYM
           MOVE    "1"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   IF    NACCT-SRYCDTOTAL     =    99999916
                       CONTINUE
                   ELSE
      *
      *            （平成２２年３月以前）
                     IF   LNK-NYULIFE-SANTEIYM <=  "201003"
      *                生活療養に該当する入院料を判定し、
      *                返却値の対象区分を設定する
                       PERFORM    210-NYUINACT-GET-SEC
                       IF   NSRY-SRYCD(1)   =  CONS-RYOYO2A     OR
                                               CONS-RYOYO2B     OR
                                               CONS-RYOYO2C     OR
                                               CONS-RYOYO2D     OR
                                               CONS-RYOYO2E     OR
                                               CONS-RYOYO2TOKU  OR
                                               CONS-SRYOYO2A    OR
                                               CONS-SRYOYO2B    OR
                                               CONS-SRYOYO2C    OR
                                               CONS-SRYOYO2D    OR
                                               CONS-SRYOYO2E    OR
                                               CONS-SRYOYO2TOKU OR
                                               CONS-KAIFUKU     OR
                                               CONS-KAIFUKU1    OR
                                               CONS-KAIFUKU2    OR
                                               CONS-ROUKAN1     OR
                                               CONS-ROUKAN2     OR
                                               CONS-ROUKAN3     OR
                                               CONS-TANKI2      OR
                                               CONS-TANKI3
                          PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                            IF     NACCT-DAY(IDX1)  NOT =  ZERO
                               MOVE   "1"     TO   LNK-NYULIFE-KBN(IDX1)
                               MOVE    1      TO   FLG-OK
      *                        病状の程度、治療の内容をしん酌して標準
      *                        負担額を軽減する患者の判定（入院料判断）
                               IF  NSRY-SRYCD(1)  =  CONS-RYOYO2A  OR
                                                     CONS-RYOYO2B  OR
                                                     CONS-RYOYO2C  OR
                                                     CONS-SRYOYO2A OR
                                                     CONS-SRYOYO2B OR
                                                     CONS-SRYOYO2C OR
                                                     CONS-KAIFUKU  OR
                                                     CONS-KAIFUKU1 OR
                                                     CONS-KAIFUKU2 OR
                                                     CONS-ROUKAN1  OR
                                                     CONS-TANKI2   OR
                                                     CONS-TANKI3
                                   MOVE   "1"   TO   
                                             LNK-NYULIFE-FTNKBN2(IDX1)
                               END-IF
      *                        病状の程度、治療の内容をしん酌して標準
      *                        負担額を軽減する患者の判定（特別入院基本料算定時の
      *                                                    ＡＤＬ入力判断）
                               IF (NSRY-SRYCD(1)  =  CONS-RYOYO2TOKU OR
                                                     CONS-SRYOYO2TOKU)
                                                 AND
                                  (SS008-MEDKBNDAY(IDX1)  =  "2" OR
                                                             "3" )
                                   MOVE   "1"   TO   
                                             LNK-NYULIFE-FTNKBN2(IDX1)
                               END-IF
                            END-IF
                          END-PERFORM
                       END-IF
                     END-IF
      *
      *            （平成２２年４月から平成２４年３月まで）
                     IF  (LNK-NYULIFE-SANTEIYM >=  "201004")    AND
                         (LNK-NYULIFE-SANTEIYM <=  "201203")
      *                生活療養に該当する入院料を判定し、
      *                返却値の対象区分を設定する
                       PERFORM    210-NYUINACT-GET-SEC
                       IF   NSRY-SRYCD(1)   =  CONS-RYOYO2A     OR
                                               CONS-RYOYO2B     OR
                                               CONS-RYOYO2C     OR
                                               CONS-RYOYO2D     OR
                                               CONS-RYOYO2E     OR
                                               CONS-RYOYO2TOKU  OR
                                               CONS-SRYOYO2A    OR
                                               CONS-SRYOYO2B    OR
                                               CONS-SRYOYO2C    OR
                                               CONS-SRYOYO2D    OR
                                               CONS-SRYOYO2E    OR
                                               CONS-SRYOYO2TOKU OR
                                               CONS-KAIFUKU1    OR
                                               CONS-KAIFUKU2    OR
                                               CONS-TANKI2      OR
                                               CONS-TANKI3      OR
                                               CONS-H22-RYOYO1F OR
                                               CONS-H22-RYOYO1G OR
                                               CONS-H22-RYOYO1H OR
                                               CONS-H22-RYOYO1I OR
                                               CONS-H22-RYOYO2A OR
                                               CONS-H22-RYOYO2B OR
                                               CONS-H22-RYOYO2C OR
                                               CONS-H22-RYOYO2D OR
                                               CONS-H22-RYOYO2E OR
                                               CONS-H22-RYOYO2F OR
                                               CONS-H22-RYOYO2G OR
                                               CONS-H22-RYOYO2H OR
                                               CONS-H22-RYOYO2I OR
                                               CONS-H22-KAIFUKU1 OR
                                               CONS-H22-KAIFUKU2
                          PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                            IF     NACCT-DAY(IDX1)  NOT =  ZERO
                               MOVE   "1"     TO   LNK-NYULIFE-KBN(IDX1)
                               MOVE    1      TO   FLG-OK
      *                        病状の程度、治療の内容をしん酌して標準
      *                        負担額を軽減する患者の判定（入院料判断）
                               IF  NSRY-SRYCD(1)  =  CONS-RYOYO2A  OR
                                                     CONS-RYOYO2B  OR
                                                     CONS-RYOYO2C  OR
                                                     CONS-RYOYO2D  OR
                                                     CONS-RYOYO2E  OR
                                                     CONS-H22-RYOYO1F OR
                                                     CONS-SRYOYO2A OR
                                                     CONS-SRYOYO2B OR
                                                     CONS-SRYOYO2C OR
                                                     CONS-KAIFUKU1 OR
                                                     CONS-KAIFUKU2 OR
                                                     CONS-TANKI2   OR
                                                     CONS-TANKI3   OR
                                                     CONS-H22-RYOYO2A OR
                                                     CONS-H22-RYOYO2B OR
                                                     CONS-H22-RYOYO2C OR
                                                     CONS-H22-RYOYO2D OR
                                                     CONS-H22-RYOYO2E OR
                                                     CONS-H22-RYOYO2F OR
                                                    CONS-H22-KAIFUKU1 OR
                                                    CONS-H22-KAIFUKU2
                                   MOVE   "1"   TO   
                                             LNK-NYULIFE-FTNKBN2(IDX1)
                               END-IF
      *                        病状の程度、治療の内容をしん酌して標準
      *                        負担額を軽減する患者の判定（特別入院基本料算定時の
      *                                                    ＡＤＬ入力判断）
                               IF (NSRY-SRYCD(1)  =  CONS-RYOYO2TOKU OR
                                                     CONS-SRYOYO2TOKU)
                                                 AND
                                  (SS008-MEDKBNDAY(IDX1)  =  "2" OR
                                                             "3" )
                                   MOVE   "1"   TO   
                                             LNK-NYULIFE-FTNKBN2(IDX1)
                               END-IF
                            END-IF
                          END-PERFORM
                       END-IF
                     END-IF
      *
      *            （平成２４年４月から）
                     IF  LNK-NYULIFE-SANTEIYM >=  "201204"
      *                生活療養に該当する入院料を判定し、
      *                返却値の対象区分を設定する
                       PERFORM    210-NYUINACT-GET-SEC
                       MOVE       ZERO        TO   FLG-SEI
                                                   IDX-SEI2
                       PERFORM    VARYING   IDX-SEI  FROM 1  BY  1
                         UNTIL   (IDX-SEI   >   84)    OR
                                 (FLG-SEI   =    1)
                             IF  NSRY-SRYCD(1) = TBL-H24-SEISRYCD
                                                        (IDX-SEI)
                                 MOVE   1        TO   FLG-SEI
                                 MOVE   IDX-SEI  TO   IDX-SEI2
                             END-IF
                       END-PERFORM
                       IF   FLG-SEI    =    1
                          PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                            IF     NACCT-DAY(IDX1)  NOT =  ZERO
                               MOVE   "1"     TO   LNK-NYULIFE-KBN(IDX1)
                               MOVE    1      TO   FLG-OK
      *                        病状の程度、治療の内容をしん酌して標準
      *                        負担額を軽減する患者の判定（入院料判断）
                               IF  (IDX-SEI2   >=   1 )   AND
                                   (IDX-SEI2   <=   56)
                                   MOVE   "1"   TO   
                                             LNK-NYULIFE-FTNKBN2(IDX1)
                               END-IF
      *                        病状の程度、治療の内容をしん酌して標準
      *                        負担額を軽減する患者の判定（特別入院基本料算定時の
      *                                                    ＡＤＬ入力判断）
                               IF (IDX-SEI2   =   83  OR  84)    AND
                                  (SS008-MEDKBNDAY(IDX1)  =  "2" OR
                                                             "3" )
                                   MOVE   "1"   TO   
                                             LNK-NYULIFE-FTNKBN2(IDX1)
                               END-IF
                            END-IF
                          END-PERFORM
                       END-IF
                     END-IF
      *
                   END-IF
      *
                   MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
                   MOVE    "key17"              TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *
      *    入院会計クローズ
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC-SEC
      *
      *    短期滞在手術基本料２（短期滞在手術基本料３）の入院料未算定日
      *    について生活療養の該当日と軽減区分２の情報を取得（サブプロより取得）
           INITIALIZE                          ORCSNYUINDAY-AREA
           MOVE    LNK-NYULIFE-HOSPNUM     TO  LNK-NYUINDAY-HOSPNUM
           MOVE    LNK-NYULIFE-PTID        TO  LNK-NYUINDAY-PTID
           MOVE    LNK-NYULIFE-SANTEIYM    TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"      USING   ORCSNYUINDAY-AREA
                                               SPA-AREA
           IF      LNK-NYUINDAY-RC         =   ZERO
               IF    LNK-NYUINDAY-TANKI2-TBL   NOT =    SPACE
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                      UNTIL   IDX1   >   31
                         IF    LNK-NYUINDAY-TANKI2-KBN(IDX1)   =   "2"
                               MOVE   "1"   TO   LNK-NYULIFE-KBN(IDX1)
                               MOVE   "1"   TO
                                             LNK-NYULIFE-FTNKBN2(IDX1)
                               MOVE    1    TO   FLG-OK
                         END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
      *−−−−医療機関食事情報取得ここから−−−−−−−−−−−
      *医療機関食事情報の取得（システム管理テーブルより）
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    LNK-NYULIFE-HOSPNUM TO  SYS-HOSPNUM
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-NYULIFE-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
      *        食事情報の退避
               IF     SYS-5000-SHOKUJIRYOYOCD  NOT =  SPACE
                   PERFORM    VARYING   IDX1  FROM  1  BY  1
                          UNTIL     IDX1   >    31
                       IF    LNK-NYULIFE-KBN(IDX1)   =   "1"
      *                   入院時食事療養費
                           IF   SYS-5000-SHOKUJIRYOYOCD  = "197000110"
                                MOVE   "1"     TO  
                                          LNK-NYULIFE-SKJKBN(IDX1)
                           ELSE
                                MOVE   "2"     TO  
                                          LNK-NYULIFE-SKJKBN(IDX1)
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *    システム管理クローズ
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC-SEC
      *−−−−医療機関食事情報取得取得ここまで−−−−−−−−−−−
      *
      *
           IF    FLG-OK   =    1
      *
      *        老人適用のチェック
               IF      LNK-NYULIFE-SANTEIYM <=  "200803"
      *          （平成２０年３月以前は老人保健の判定チェック）
                   MOVE    ZERO                 TO  FLG-RJN 
                   INITIALIZE                       PTKOHINF-REC
                   MOVE    LNK-NYULIFE-HOSPNUM  TO  PTKOH-HOSPNUM
                   MOVE    LNK-NYULIFE-PTID     TO  PTKOH-PTID
                   MOVE    "027"                TO  PTKOH-KOHNUM
                   MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                   MOVE    "key4"               TO  MCP-PATHNAME
                   PERFORM 910-PTKOHINF-SELECT-SEC
                   IF      FLG-PTKOHINF         =   ZERO
                       MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                       MOVE    "key4"               TO  MCP-PATHNAME
                       PERFORM   910-PTKOHINF-READ-SEC
                       PERFORM   UNTIL   (FLG-PTKOHINF   =   1)
                           IF    PTKOH-SAKJOKBN     =    SPACE
                                 MOVE   1           TO   FLG-RJN
                           END-IF
                           MOVE    "tbl_ptkohinf"   TO  MCP-TABLE
                           MOVE    "key4"           TO  MCP-PATHNAME
                           PERFORM 910-PTKOHINF-READ-SEC
                       END-PERFORM
                   END-IF
      *            患者公費情報クローズ
                   MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                   MOVE    "key4"               TO  MCP-PATHNAME
                   PERFORM   900-CLOSE-SEC-SEC
               ELSE
      *          （平成２０年４月以降はサブルーチンによりチェック）
                   PERFORM  VARYING    IDX-RJN    FROM   1   BY  1
                     UNTIL  IDX-RJN    >   31
                       INITIALIZE                   ORCSAGECHKAREA
                       MOVE    LNK-NYULIFE-HOSPNUM  TO  AGECHK-HOSPNUM
                       MOVE    LNK-NYULIFE-PTID     TO  AGECHK-PTID
                       MOVE    LNK-NYULIFE-SANTEIYM TO  WRK-YYMM
                       MOVE    IDX-RJN              TO  WRK-DD
                       MOVE    WRK-YMD              TO  AGECHK-SRYYMD
                       CALL   "ORCSAGECHK"          USING
                                                    ORCSAGECHKAREA
                                                    SPA-AREA
                       IF      AGECHK-ROUJIN2    =   1
                           MOVE   1              TO  WRK-RJN(IDX-RJN)
                       END-IF
                   END-PERFORM
               END-IF
      *
      *−−−−サブルーチンでの患者所得情報の取得ここから−−−−−
               INITIALIZE                          ORCSSYOTOKUAREA
               MOVE    LNK-NYULIFE-HOSPNUM     TO  LNK-SYOTOKU-HOSPNUM
               MOVE    LNK-NYULIFE-PTID        TO  LNK-SYOTOKU-PTID
               MOVE    LNK-NYULIFE-SANTEIYM    TO  LNK-SYOTOKU-SRYYM
               CALL   "ORCSSYOTOKU"    USING    ORCSSYOTOKUAREA
                                                SPA-AREA
               IF    LNK-SYOTOKU-RC    =        ZERO
                    PERFORM   VARYING   IDX1   FROM   1  BY  1
                              UNTIL     IDX1   >    31
                        EVALUATE   LNK-SYOTOKU-SKJKBN(IDX1)
                            WHEN  "1"
                                MOVE  "3"  TO  LNK-NYULIFE-FTNKBN1(IDX1)
                            WHEN  "2"
                                MOVE  "1"  TO  LNK-NYULIFE-FTNKBN1(IDX1)
                            WHEN  "3"
                                MOVE  "2"  TO  LNK-NYULIFE-FTNKBN1(IDX1)
                            WHEN  "4"
                                MOVE  "4"  TO  LNK-NYULIFE-FTNKBN1(IDX1)
                        END-EVALUATE
                    END-PERFORM
               ELSE
                    DISPLAY "ORCSSYTOKU RC = " LNK-SYOTOKU-RC
               END-IF
      *−−−−サブルーチンでの患者所得情報の取得ここまで−−−−−
      *
      *
      *−−−−食事療養、環境療養コード、単価設定ここから−−−−−
             PERFORM   VARYING   IDX1   FROM   1  BY  1
               UNTIL   IDX1   >    31
                IF   LNK-NYULIFE-KBN(IDX1)   =   "1"
                    EVALUATE  LNK-NYULIFE-SKJKBN(IDX1)
                        WHEN  "1"
                            MOVE  "197001310"   TO
                                            LNK-NYULIFE-SKJ-SRYCD(IDX1)
                            MOVE   554          TO
                                            LNK-NYULIFE-SKJ-MONEY(IDX1)
                            MOVE  "197001410"   TO
                                            LNK-NYULIFE-KAN-SRYCD(IDX1)
                            MOVE   398          TO
                                            LNK-NYULIFE-KAN-MONEY(IDX1)
                        WHEN  "2"
                            MOVE  "197001710"   TO
                                            LNK-NYULIFE-SKJ-SRYCD(IDX1)
                            MOVE   420          TO
                                            LNK-NYULIFE-SKJ-MONEY(IDX1)
                            MOVE  "197001810"   TO
                                            LNK-NYULIFE-KAN-SRYCD(IDX1)
                            MOVE   398          TO
                                            LNK-NYULIFE-KAN-MONEY(IDX1)
                    END-EVALUATE
                END-IF
             END-PERFORM
      *−−−−食事療養、環境療養コード、単価設定ここまで−−−−−
      *
      *−−−−食事負担、環境療養負担コード、単価設定ここから−−−
      *      生活療養食事標準負担額設定（一般）
             PERFORM   VARYING   IDX1   FROM   1  BY  1
               UNTIL   IDX1   >    31
                IF   LNK-NYULIFE-KBN(IDX1)   =   "1"
                    EVALUATE  LNK-NYULIFE-SKJKBN(IDX1)
                        WHEN  "1"
                            MOVE  "197002010"   TO
                                         LNK-NYULIFE-SKJFTN-SRYCD(IDX1)
                            MOVE   460          TO
                                         LNK-NYULIFE-SKJFTN-MONEY(IDX1)
                        WHEN  "2"
                            MOVE  "197002110"   TO
                                         LNK-NYULIFE-SKJFTN-SRYCD(IDX1)
                            MOVE   420          TO
                                         LNK-NYULIFE-SKJFTN-MONEY(IDX1)
                    END-EVALUATE
                END-IF
             END-PERFORM
      *
      *      生活療養食事標準負担額設定（軽減ありの患者）
             PERFORM   VARYING   IDX1   FROM   1  BY  1
               UNTIL   IDX1   >    31
                IF   LNK-NYULIFE-KBN(IDX1)   =   "1"
                    EVALUATE  LNK-NYULIFE-FTNKBN2(IDX1)
                        WHEN  SPACE
      *                   所得の状況による軽減
                            EVALUATE   LNK-NYULIFE-FTNKBN1(IDX1)
                                WHEN   "1"
                                WHEN   "2"
      *                           （低２）
                                    MOVE  "197002210"   TO
                                         LNK-NYULIFE-SKJFTN-SRYCD(IDX1)
                                    MOVE   210          TO
                                         LNK-NYULIFE-SKJFTN-MONEY(IDX1)
                                WHEN   "3"
      *                           （低１）
                                    IF  (FLG-RJN         =  0)   AND
                                        (WRK-RJN(IDX1)   =  0)                                        
                                       MOVE  "197002310"   TO
                                         LNK-NYULIFE-SKJFTN-SRYCD(IDX1)
                                       MOVE   130          TO
                                         LNK-NYULIFE-SKJFTN-MONEY(IDX1)
                                    ELSE
                                       MOVE  "197002410"   TO
                                         LNK-NYULIFE-SKJFTN-SRYCD(IDX1)
                                       MOVE   130          TO
                                         LNK-NYULIFE-SKJFTN-MONEY(IDX1)
                                    END-IF
                                WHEN   "4"
      *                           （低１−老齢福祉年金受給者）
                                    MOVE  "197002510"   TO
                                         LNK-NYULIFE-SKJFTN-SRYCD(IDX1)
                                    MOVE   100          TO
                                         LNK-NYULIFE-SKJFTN-MONEY(IDX1)
                            END-EVALUATE
                        WHEN  "1"
      *                   病状の程度による軽減
                            EVALUATE   LNK-NYULIFE-FTNKBN1(IDX1)
                                WHEN   SPACE
      *                           （医療区分２、３（一般））
                                    MOVE  "197002710"   TO
                                         LNK-NYULIFE-SKJFTN-SRYCD(IDX1)
                                    MOVE   260          TO
                                         LNK-NYULIFE-SKJFTN-MONEY(IDX1)
                                WHEN   "1"
      *                           （医療区分２、３（低２　９０日以下））
                                    MOVE  "197002810"   TO
                                         LNK-NYULIFE-SKJFTN-SRYCD(IDX1)
                                    MOVE   210          TO
                                         LNK-NYULIFE-SKJFTN-MONEY(IDX1)
                                WHEN   "2"
      *                           （医療区分２、３（低２　９０日超））
                                    MOVE  "197002910"   TO
                                         LNK-NYULIFE-SKJFTN-SRYCD(IDX1)
                                    MOVE   160          TO
                                         LNK-NYULIFE-SKJFTN-MONEY(IDX1)
                                WHEN   "3"
                                WHEN   "4"
      *                           （医療区分２、３（低１））
                                    MOVE  "197003010"   TO
                                         LNK-NYULIFE-SKJFTN-SRYCD(IDX1)
                                    MOVE   100          TO
                                         LNK-NYULIFE-SKJFTN-MONEY(IDX1)
                            END-EVALUATE
                    END-EVALUATE
                END-IF
             END-PERFORM
      *
      *      生活療養環境療養標準負担額設定
             PERFORM   VARYING   IDX1   FROM   1  BY  1
               UNTIL   IDX1   >    31
                IF   LNK-NYULIFE-KBN(IDX1)   =   "1"
                    IF  LNK-NYULIFE-FTNKBN2(IDX1)   =   SPACE
                        MOVE  "197001910"       TO
                                        LNK-NYULIFE-KANFTN-SRYCD(IDX1)
                        MOVE  320       TO
                                        LNK-NYULIFE-KANFTN-MONEY(IDX1)
                    END-IF
      *             老齢福祉年金受給者は環境療養患者負担無しに置換え
                    IF  LNK-NYULIFE-FTNKBN1(IDX1)   =   "4"
                        MOVE  SPACE  TO   LNK-NYULIFE-KANFTN-SRYCD(IDX1)
                        MOVE  ZERO   TO   LNK-NYULIFE-KANFTN-MONEY(IDX1)
                    END-IF
                END-IF
             END-PERFORM
      *
      *−−−−食事負担、環境療養負担コード、単価設定ここまで−−−
      *
           END-IF
      *
           IF   FLG-OK   =   ZERO
               MOVE   4      TO   LNK-NYULIFE-RC
           END-IF
      *
      *     PERFORM   VARYING  IDX1   FROM  1   BY  1
      *               UNTIL  IDX1   >   31
      *          DISPLAY "IDX1 = " IDX1
      *          DISPLAY "LNK-NYULIFE-FTNKBN1(IDX1) = " 
      *                   LNK-NYULIFE-FTNKBN1(IDX1)
      *          DISPLAY "LNK-NYULIFE-FTNKBN2(IDX1) = " 
      *                   LNK-NYULIFE-FTNKBN2(IDX1)
      *          DISPLAY  "LNK-NYULIFE-SKJ-SRYCD(IDX1) = "
      *                    LNK-NYULIFE-SKJ-SRYCD(IDX1)
      *          DISPLAY "LNK-NYULIFE-SKJ-MONEY(IDX1) = "
      *                   LNK-NYULIFE-SKJ-MONEY(IDX1)
      *          DISPLAY  "LNK-NYULIFE-SKJFTN-SRYCD(IDX1) = "
      *                    LNK-NYULIFE-SKJFTN-SRYCD(IDX1)
      *          DISPLAY "LNK-NYULIFE-SKJFTN-MONEY(IDX1) = "
      *                   LNK-NYULIFE-SKJFTN-MONEY(IDX1)
      *          DISPLAY  "LNK-NYULIFE-KAN-SRYCD(IDX1) = "
      *                    LNK-NYULIFE-KAN-SRYCD(IDX1)
      *          DISPLAY "LNK-NYULIFE-KAN-MONEY(IDX1) = "
      *                   LNK-NYULIFE-KAN-MONEY(IDX1)
      *          DISPLAY  "LNK-NYULIFE-KANFTN-SRYCD(IDX1) = "
      *                    LNK-NYULIFE-KANFTN-SRYCD(IDX1)
      *          DISPLAY "LNK-NYULIFE-KANFTN-MONEY(IDX1) = "
      *                   LNK-NYULIFE-KANFTN-MONEY(IDX1)
      *     END-PERFORM
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為取得
      *****************************************************************
       210-NYUINACT-GET-SEC            SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-NYUINACT-SELECT-SEC
           IF      FLG-NYUINACT        =   ZERO
               MOVE    "tbl_nyuinact"  TO  MCP-TABLE
               MOVE    "key9"          TO  MCP-PATHNAME
               PERFORM   910-NYUINACT-READ-SEC
           END-IF
      *
      *    入院行為クローズ
           MOVE     "tbl_nyuinact"     TO  MCP-TABLE
           MOVE     "key9"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC-SEC
      *
           .
       210-NYUINACT-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスター読込
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-5000-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                      SYS-5000-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTINF-REC
                   MOVE    ZERO        TO  FLG-PTINF
               ELSE
                   INITIALIZE              PTINF-REC
                   MOVE    1           TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報マスター検索
      *****************************************************************
       910-PTHKNINF-SELECT-SEC        SECTION.
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-PTHKNINF
           ELSE
               MOVE    1                  TO  FLG-PTHKNINF
           END-IF
      *
           .
       910-PTHKNINF-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報マスター読込
      *****************************************************************
       910-PTHKNINF-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  PTHKNINF-REC
               MOVE    ZERO               TO  FLG-PTHKNINF
           ELSE
               INITIALIZE                     PTHKNINF-REC
               MOVE    1                  TO  FLG-PTHKNINF
           END-IF
      *
           .
       910-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報マスター検索
      *****************************************************************
       910-PTKOHINF-SELECT-SEC        SECTION.
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-PTKOHINF
           ELSE
               MOVE    1                  TO  FLG-PTKOHINF
           END-IF
      *
           .
       910-PTKOHINF-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報マスター読込
      *****************************************************************
       910-PTKOHINF-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  PTKOHINF-REC
               MOVE    ZERO               TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                     PTKOHINF-REC
               MOVE    1                  TO  FLG-PTKOHINF
           END-IF
      *
           .
       910-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター検索
      *****************************************************************
       910-NYUINACCT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター読込
      *****************************************************************
       910-NYUINACCT-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  NYUINACCT-REC
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                     NYUINACCT-REC
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得マスター検索
      *****************************************************************
       910-TSYRRK-SELECT-SEC        SECTION.
      *
           MOVE    TSYRRK-REC          TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-TSYRRK
           ELSE
               MOVE    1                  TO  FLG-TSYRRK
           END-IF
      *
           .
       910-TSYRRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得マスター読込
      *****************************************************************
       910-TSYRRK-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  TSYRRK-REC
               MOVE    ZERO               TO  FLG-TSYRRK
           ELSE
               INITIALIZE                     TSYRRK-REC
               MOVE    1                  TO  FLG-TSYRRK
           END-IF
      *
           .
       910-TSYRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    低年金マスター検索
      *****************************************************************
       910-TNKRRK-SELECT-SEC        SECTION.
      *
           MOVE    TNKRRK-REC          TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-TNKRRK
           ELSE
               MOVE    1                  TO  FLG-TNKRRK
           END-IF
      *
           .
       910-TNKRRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    低年金マスター読込
      *****************************************************************
       910-TNKRRK-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  TNKRRK-REC
               MOVE    ZERO               TO  FLG-TNKRRK
           ELSE
               INITIALIZE                     TNKRRK-REC
               MOVE    1                  TO  FLG-TNKRRK
           END-IF
      *
           .
       910-TNKRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為マスター検索
      *****************************************************************
       910-NYUINACT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACT
           ELSE
               MOVE    1                  TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為マスター読込
      *****************************************************************
       910-NYUINACT-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACT-REC
               MOVE    ZERO                TO  FLG-NYUINACT
           ELSE
               INITIALIZE                      NYUINACT-REC
               MOVE    1                   TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       910-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
      *
       900-CLOSE-SEC-EXT.
           EXIT.
