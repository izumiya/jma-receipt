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
       PROGRAM-ID.             ORCHCM31.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 支払証明書（日別）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/12/09    NACL−森脇    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.01.01    NACL-森脇    03.03.18  出力先プリンタの変更
      *  01.01.02    NACL-森脇    03/07/15  出力先プリンタサブ追加
      *  02.01.01    NACL-森脇    03/12/10  出力条件修正
      *  02.01.02    NACL-森脇    04/10/25  CLOSE追加
      *  02.01.03    NACL-森脇    04/11/09  パラメタ変更,バッチ処理に変更
      *  02.01.04    NACL-森脇    04/11/25  印刷ＤＢに書き込むよう変更
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
           COPY    "HCM31V02.INC"  REPLACING   //HCM31V02//
                                   BY          //HCM31//.
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
           03  FLG-TAISYO          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(05).
           03  IDY                 PIC 9(05).
           03  IDX1                PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDX3                PIC 9(05).
           03  IDX4                PIC 9(05).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(03).
           03  CNT-LINE            PIC 9(03).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-TABLE           PIC X(10).
           03  WRK-PATH            PIC X(10).
      *
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
      *        最大２０００件・・・２００１件目は並び替え用
               05  WRK-HENSYU-OCC  OCCURS  2001.
                   07  WRK-HIDUKE  PIC X(08).
                   07  WRK-TENSU   PIC S9(07).
                   07  WRK-JIHI    PIC S9(11).
                   07  WRK-GAKU    PIC S9(11).
      *
           03  WRK-GHENSYU-TBL.
               05  WRK-GHKNTEN     PIC S9(07).
               05  WRK-GJIHIMONEY  PIC S9(11).
               05  WRK-GSKYMONEY   PIC S9(11).
      *
           03  WRK-JIHIMONEY       PIC S9(11).
      *
           03  WRK-Z3              PIC ZZZ.
           03  WRK-Z6              PIC ZZZ,ZZZ.
           03  WRK-Z7              PIC ZZZZZZZ.
           03  WRK-Z9              PIC ---,---,---.
      *
       01  CONST-AREA.
           03  CONST-HEN-MAX       PIC 9(05)   VALUE   2000.
           03  CONST-LINE-MAX      PIC 9(05)   VALUE   30.
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
           COPY    "ORCA-DBPATH".
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
           .
       100-INIT-EXT.
           EXIT.
      *
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
           MOVE    ZERO            TO  CNT-PAGE
      *
           PERFORM VARYING IDX4    FROM    1   BY  1
                   UNTIL ( IDX4    >   WRK-HEN-MAX )
      *
      *        対象データチェック処理
               PERFORM 230-TAISYO-CHK-SEC
               IF    ( FLG-TAISYO  =   1 )
      *
                   PERFORM 240-MEISAI-HEN-SEC
      *
               END-IF
      *
               IF    ( CNT-LINE    >=  CONST-LINE-MAX )
      *
                   IF    ( IDX4    >=  WRK-HEN-MAX )
                       PERFORM 240-GOKEI-HEN-SEC
                   END-IF
      *
                       PERFORM 250-PRINT-SEC
               END-IF
      *
           END-PERFORM
      *
           IF    ( CNT-LINE    >   ZERO )
               PERFORM 240-GOKEI-HEN-SEC
               PERFORM 250-PRINT-SEC
           END-IF
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
       210-HENSYU-SEC              SECTION.
      *
           IF    ( ORCHCM31-KIKANKBN   =   "2" )
               MOVE    "tbl_syunou"    TO  WRK-TABLE
               MOVE    "key50"         TO  WRK-PATH
           ELSE
               MOVE    "tbl_syunou"    TO  WRK-TABLE
               MOVE    "key49"         TO  WRK-PATH
           END-IF
      *
      *    収納マスター読み込み
           PERFORM 900-SYUNOU-SELECT-SEC
      *
      *    収納マスターがなくなるまで繰り返す
           PERFORM UNTIL   FLG-SYUNOU      =   1
      *
               IF    ( SYU-DENPJTIKBN  NOT =   "3" )
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
           MOVE    WRK-PATH        TO  MCP-PATHNAME
           PERFORM 910-DBCLOSE-SEC
      *
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療期間より編集処理
      *****************************************************************
       2101-SRYKIKAN-HEN-SEC           SECTION.
      *
           MOVE    ZERO            TO  IDX2
      *
      *    自費計算
           COMPUTE WRK-JIHIMONEY   =   SYU-RMSAGAKU
                                   +   SYU-SKYMONEY-SKJ-JIHI-KEI
                                   +   SYU-TOTAL-TGMONEY
                                   +   SYU-TOTAL-TGMONEY-TAX
                                   +   SYU-JIHI-TOTAL
                                   +   SYU-JIHI-TOTAL-TAX
                                   +   SYU-JIHI-TAX
      *
      *    入金編集処理
           PERFORM 220-NUKIN-HENSYU-SEC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1        >   WRK-HEN-MAX )
                    OR   ( IDX2    NOT =   ZERO )
      *
               IF    ( SYU-SRYYMD  =   WRK-HIDUKE (IDX1) )
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
               MOVE    SYU-SRYYMD  TO  WRK-HIDUKE (IDX2)
      *
           END-IF
      *
           COMPUTE WRK-TENSU(IDX2)
               =   WRK-TENSU(IDX2) +   SYU-TOTAL-HKNTEN
      *
           COMPUTE WRK-JIHI(IDX2)
               =   WRK-JIHI(IDX2)  +   WRK-JIHIMONEY
      *
           COMPUTE WRK-GAKU (IDX2)
               =   WRK-GAKU (IDX2) +   HCM31S01-KINGAKU (1)
      *
      *    合計集計
           COMPUTE WRK-GHKNTEN
               =   WRK-GHKNTEN     +   SYU-TOTAL-HKNTEN
      *
      *    自費合計集計
           COMPUTE WRK-GJIHIMONEY
               =   WRK-GJIHIMONEY  +   WRK-JIHIMONEY
      *
      *    支払額合計
           COMPUTE WRK-GSKYMONEY
               =   WRK-GSKYMONEY   +   HCM31S01-KINGAKU (1)
      *
           .
       2101-SRYKIKAN-HEN-EXT.
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
                       IF    ( HCM31S01-NYKNYMD(IDX3)
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
                       MOVE    HCM31S01-NYKNYMD(IDX3)
                                           TO  WRK-HIDUKE (IDX2)
      *
                       COMPUTE WRK-GAKU (IDX2)
                           =   WRK-GAKU (IDX2)
                           +   HCM31S01-KINGAKU (IDX3)
      *
                       COMPUTE WRK-GSKYMONEY
                           =   WRK-GSKYMONEY
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
           MOVE    SYU-HOSPID      TO  HCM31S01-HOSPID
           MOVE    SYU-NYUGAIKBN   TO  HCM31S01-NYUGAIKBN
           MOVE    SYU-PTID        TO  HCM31S01-PTID
           MOVE    SYU-DENPNUM     TO  HCM31S01-DENPNUM
           MOVE    SYU-SRYYMD      TO  HCM31S01-SRYYMD
           MOVE    ORCHCM31-STRSRYYMD
                                   TO  HCM31S01-STYMD
           MOVE    ORCHCM31-ENDSRYYMD
                                   TO  HCM31S01-EDYMD
      *    支払い金額サブプロ呼び出し
           CALL    "ORCHCM31S01"       USING
                                       HCM31S01-AREA
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
                   IF      WRK-HIDUKE(IDX)     >   WRK-HIDUKE(IDY)
                       MOVE    WRK-HENSYU-OCC(IDX)
                                       TO  WRK-HENSYU-OCC(2001)
                       MOVE    WRK-HENSYU-OCC(IDY)
                                       TO  WRK-HENSYU-OCC(IDX)
                       MOVE    WRK-HENSYU-OCC(2001)
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
           MOVE    ZERO            TO  FLG-TAISYO
      *
           IF    ( ORCHCM31-KIKANKBN   =   "1"   )
               IF    ( ORCHCM31-DSPKBN =   "2"   )
                   IF    ( WRK-GAKU (IDX4) >   ZERO )
                       MOVE    1       TO  FLG-TAISYO
                   END-IF
               ELSE
                       MOVE    1       TO  FLG-TAISYO
               END-IF
           ELSE
                       MOVE    1       TO  FLG-TAISYO
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
           INITIALIZE                  HCM31
      *
      *    開始診療日
           MOVE    ORCHCM31-STRSRYYMD  TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  HCM31-STRSRYYMD
      *
      *    終了診療日
           MOVE    ORCHCM31-ENDSRYYMD  TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  HCM31-ENDSRYYMD
      *
      *    患者番号
           MOVE    PTNUM-PTNUM         TO  HCM31-PTNUM
      *
      *    患者名称
           MOVE    PTINF-NAME          TO  HCM31-PTNAME
      *
      *    発行日
           MOVE    SPA-SYSYMD          TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  HCM31-HAKKOYMD
      *
      *    入外
           EVALUATE    ORCHCM31-NYUGAI
               WHEN    "1"
                   MOVE    "（入院）"      TO  HCM31-NYUGAI
               WHEN    "2"
                   MOVE    "（外来）"      TO  HCM31-NYUGAI
           END-EVALUATE
      *
      *    頁
           COMPUTE CNT-PAGE            =   CNT-PAGE    +   1
           MOVE    CNT-PAGE            TO  WRK-Z3
           MOVE    WRK-Z3              TO  HCM31-PAGE
      *
      *    医療機関住所
           IF    ( SYS-1002-ADRS (61:) =   SPACE )
               MOVE    SYS-1002-ADRS       TO  HCM31-HOSPADRS
           ELSE
               MOVE    SYS-1002-ADRS       TO  HCM31-HOSPADRS2
           END-IF
      *
      *    医療機関名
           MOVE    SYS-1001-HOSPNAME   TO  HCM31-HOSPNAME
      *
      *    医療機関ＴＥＬ
           MOVE    SYS-1002-TEL        TO  HCM31-HOSPTEL
      *
           IF    ( ORCHCM31-KIKANKBN   =   "2" )
               MOVE    "支　払　日"    TO  HCM31-LABELYMD
           ELSE
               MOVE    "診　療　日"    TO  HCM31-LABELYMD
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
           MOVE    WRK-HIDUKE(IDX4)    TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(11:12)  TO  HCM31-SRYYMD(CNT-LINE)
      *
           IF    ( ORCHCM31-KIKANKBN   =   "1"   )
            AND  ( ORCHCM31-DSPKBN     =   "1"   )
      *
      *        保険点数
               IF      WRK-TENSU(IDX4)     NOT =   ZERO
                   MOVE    WRK-TENSU(IDX4) TO  WRK-Z7
                   MOVE    WRK-Z7          TO  HCM31-HKNTEN   (CNT-LINE)
                   MOVE    "点"            TO  HCM31-TEN      (CNT-LINE)
               END-IF
      *
      *        自費診療
               IF      WRK-JIHI(IDX4)      NOT =   ZERO
                   MOVE    WRK-JIHI(IDX4)  TO  WRK-Z9
                   MOVE    WRK-Z9          TO  HCM31-JIHIMONEY(CNT-LINE)
                   MOVE    "円"            TO  HCM31-JYEN     (CNT-LINE)
               END-IF
           END-IF
      *
      *    入返金額
           IF    ( WRK-GAKU(IDX4)      NOT =   ZERO )
               MOVE    WRK-GAKU(IDX4)  TO  WRK-Z9
               MOVE    WRK-Z9          TO  HCM31-SKYMONEY(CNT-LINE)
               MOVE    "円"            TO  HCM31-YEN     (CNT-LINE)
           END-IF
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
            AND  ( ORCHCM31-DSPKBN     =   "1"   )
      *
      *        合計保険点数
               MOVE    WRK-GHKNTEN     TO  WRK-Z7
               MOVE    WRK-Z7          TO  HCM31-GHKNTEN
      *        合計自費診療
               MOVE    WRK-GJIHIMONEY  TO  WRK-Z9
               MOVE    WRK-Z9          TO  HCM31-GJIHIMONEY
           END-IF
      *
      *    合計入返金額
           MOVE    WRK-GSKYMONEY   TO  WRK-Z9
           MOVE    WRK-Z9          TO  HCM31-GSKYMONEY
      *
           .
       240-GOKEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票出力処理
      *****************************************************************
       250-PRINT-SEC                   SECTION.
      *
           MOVE    "HCM31V02.red"      TO  SPRT-PRTID
           MOVE    HCM31               TO  SPRT-PRTDATA
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
           IF    ( SPRT-RETURN         =   ZERO )
               CONTINUE
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
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           MOVE    CNT-PAGE        TO  ORCHCM31-OT-CNT
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
           PERFORM 910-DBCLOSE-SEC
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
           PERFORM 910-DBCLOSE-SEC
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
           MOVE    SPA-HOSPID      TO  PTNUM-HOSPID
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
           PERFORM 910-DBCLOSE-SEC
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
           MOVE    SPA-HOSPID      TO  PTINF-HOSPID
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
           PERFORM 910-DBCLOSE-SEC
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
           MOVE    SPA-HOSPID          TO  SYU-HOSPID
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
           MOVE    WRK-PATH        TO  MCP-PATHNAME
      *
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
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
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-PATH        TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
               INITIALIZE              SYUNOU-REC
           END-IF
      *
           .
       900-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC          SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           IF        ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       910-DBFETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       910-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
       910-DBCLOSE-EXT.
           EXIT.
