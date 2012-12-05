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
       PROGRAM-ID.             ORCHCN02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 退院証明書
      *  コンポーネント名  : 退院証明書印刷
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/10/23    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    03.07.01  出力先プリンタサブ追加
      *  01.00.02    NACL-太田    03.09.19  自院歴より継続して入院している
      *                                     場合の入院日を修正
      *  01.00.03    NACL-小豆沢  06.06.12  入院歴、通算期間の編集追加
      *  03.05.00    NACL-多々納  07/05/09  グループ診療対応
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
       01  PRT-REC                 PIC X(10000).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "HCN02V02.INC".
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
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-YOBI            PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  MEISAI-IDX          PIC 9(04).
           03  KISAI-IDX           PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
      *
           03  WRK-WSYSYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-PAGE            PIC 9(03).
      *
           03  WRK-ADRS            PIC X(200).
           03  WRK-NENREI-X.
               05  WRK-NENREI      PIC 9(03).
           03  WRK-YOBI-NISSU      PIC 9(05).
      *
      *    文字変換
           03  WRK-ZENKAKU-G.
               05  WRK-ZENKAKU         PIC X(02)   OCCURS   10.
           03  WRK-MOJI                PIC X(10).
           03  WRK-MOJISU              PIC 9(02).
           03  WRK-MAE-INPUT           PIC X(200).
           03  WRK-OUT-INPUT           PIC X(200).
      *
           03  WRK-Z3              PIC ZZ9.
           03  WRK-Z4              PIC ZZZ9.
           03  WRK-Z5              PIC ZZZZ9.
      *
           03  WRK-ERRMSG          PIC X(40).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC X(02).
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC X(02).
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC X(02).
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
           03  WRK-SPA-SYSYYYY.
               05  WRK-SPA-SYSYY   PIC  9(04).
      *
       01  WRK-HCN02-AREA.
           03  WRK-HCN02-TBL                    OCCURS  8.
               05  FILLER                   PIC X(6)  VALUE   "種別：".
               05  WRK-HCN02-SYUBETU        PIC X(26).
               05  FILLER                   PIC X(2)  VALUE  "　".
               05  WRK-HCN02-KIKAN          PIC X(4).
               05  FILLER                   PIC X(2)  VALUE  "日".
               05  FILLER                   PIC X(6)  VALUE "（平成".
               05  WRK-HCN02-KIKANSTRYY     PIC X(4).
               05  FILLER                   PIC X(2)  VALUE  "年".
               05  WRK-HCN02-KIKANSTRMM     PIC X(4).
               05  FILLER                   PIC X(2)  VALUE  "月".
               05  WRK-HCN02-KIKANSTRDD     PIC X(4).
               05  FILLER                   PIC X(8)  VALUE  "日〜平成".
               05  WRK-HCN02-KIKANENDYY     PIC X(4).
               05  FILLER                   PIC X(2)  VALUE  "年".
               05  WRK-HCN02-KIKANENDMM     PIC X(4).
               05  FILLER                   PIC X(2)  VALUE  "月".
               05  WRK-HCN02-KIKANENDDD     PIC X(4).
               05  FILLER                   PIC X(8)  VALUE  "日）".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    基本情報
           COPY    "CPSK1001.INC".
      *
      *    所在地、連絡先
           COPY    "CPSK1002.INC".
      *
      *    職員情報
           COPY    "CPSK1010.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1034.INC".
      *
      *    入院基本情報
           COPY    "CPSK5000.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    入退院日取得サブパラメタ
           COPY    "CPORCSNTYMD.INC".
      *
      *    日付サブルーチン領域
           COPY  "CPORCSDAY.INC".
           COPY  "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY  "CPORCSNUM.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    退院証明書入院取得サブ
           COPY    "CPORCHCN02SUB.INC".
      *
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "COMMON-SPA".
      *    入院履歴
       01  LNK-PTNYUINRRK.
           COPY    "CPPTNYUINRRK.INC"
               REPLACING   //PTNYUINRRK-//
               BY          //LNK-PTNYUINRRK-//.
      *
       PROCEDURE                   DIVISION
           USING
           SPA-AREA
           LNK-PTNYUINRRK
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END  =   1
      *
           PERFORM 300-END-SEC
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  HCN02
           MOVE    ZERO                TO  WRK-PAGE
      *
           ACCEPT  SYS-TIME            FROM    TIME
      *
      *    ファイルＯＰＥＮ
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
           MOVE    "HCN2"              TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
           ADD     1                   TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HC01PARA-CNT
           OPEN    OUTPUT              PRT-FILE
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC                   SECTION.
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "1"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1034-REC
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *　　印刷項目編集
            PERFORM 2001-SYORI-SEC
      *　　印刷用データ出力
            MOVE   HCN02               TO  PRT-REC
            WRITE  PRT-REC
      *
            CLOSE                       PRT-FILE
      *
            INITIALIZE                 ORCSMKPRTAREA
            MOVE   "HC01.sh"           TO  MKPRT-ID
            MOVE   "HCN02V02.red"      TO  MKPRT-DIA
            MOVE   SPACE               TO  MKPRT-DEF
            MOVE   HC01PARA            TO  MKPRT-DAT
            MOVE   SYS-1034-PRTNM(2)   TO  MKPRT-PRTNM
            MOVE   "退院証明書"        TO  MKPRT-INFO
            CALL   "ORCSMKPRT1"        USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       2001-SYORI-SEC                SECTION.
      *
      *    患者マスター検索
           INITIALIZE                    PTINF-REC
           MOVE    SPA-HOSPNUM       TO  PTINF-HOSPNUM
           MOVE    SPA-PTID          TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           =   1
               DISPLAY "*** PTINF  INVALIE:"   PTINF-KEY
               GO      TO      2001-SYORI-EXT
           END-IF
      *
      *    基本情報
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SPA-SYSYMD          TO  SYS-1001-STYUKYMD
                                           SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
           END-IF
      *
      *    所在地、連絡先
           INITIALIZE                  SYS-1002-REC
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
           MOVE    SPA-SYSYMD          TO  SYS-1002-STYUKYMD
                                           SYS-1002-EDYUKYMD
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1002-REC
           END-IF
      *
      *    職員情報
           IF          LNK-PTNYUINRRK-DRCD1    =   SPACE
               MOVE    SPACE                   TO  SYS-1010-REC
           ELSE
               INITIALIZE                      SYS-1010-REC
               MOVE    "1010"                  TO  SYS-1010-KANRICD
               MOVE    LNK-PTNYUINRRK-DRCD1    TO  SYS-1010-KBNCD
               MOVE    SPA-HOSPNUM             TO  SYS-1010-HOSPNUM
               MOVE    SPA-SYSYMD              TO  SYS-1010-STYUKYMD
                                                   SYS-1010-EDYUKYMD
               MOVE    SYS-1010-REC            TO  MCPDATA-REC
               PERFORM 800-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1010-REC
               END-IF
           END-IF
      *
      *    帳票編集処理
           PERFORM 310-HC01-HEN-SEC
      *
           .
       2001-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       310-HC01-HEN-SEC                SECTION.
      *
           INITIALIZE  HCN02
      *
      *    医療機関名称
           MOVE    SYS-1001-HOSPNAME   TO  HCN02-HOSPNAME
      *
      *    住所
           MOVE    SYS-1002-ADRS       TO  HCN02-HOSPADRS
      *
      *    電話番号
           MOVE    SYS-1002-TEL        TO  HCN02-HOSPTEL
      *
      *    主治医氏名
           MOVE    SYS-1010-NAME       TO  HCN02-DRNAME
      *
      *    患者氏名
           MOVE    PTINF-NAME          TO  HCN02-PTNAME
      *
      *    患者住所
           MOVE    SPACE               TO  WRK-ADRS
           STRING  PTINF-HOME-ADRS         DELIMITED   BY  SPACE
                   PTINF-HOME-BANTI        DELIMITED   BY  SPACE
                   INTO                    WRK-ADRS
           END-STRING
           MOVE    WRK-ADRS            TO  HCN02-PTADRS
      *
      *    電話番号
           MOVE    PTINF-HOME-TEL1     TO  HCN02-PTTEL
      *
      *    性別
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "○"        TO  HCN02-PTSEX(1)
               WHEN    "2"
                   MOVE    "○"        TO  HCN02-PTSEX(2)
           END-EVALUATE
      *
      *    誕生日
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
      *
           EVALUATE    WRK-HENYMDG(1:4)
               WHEN    "明治"
                   MOVE    "○"        TO  HCN02-PTBIRTHG(1)
               WHEN    "大正"
                   MOVE    "○"        TO  HCN02-PTBIRTHG(2)
               WHEN    "昭和"
                   MOVE    "○"        TO  HCN02-PTBIRTHG(3)
               WHEN    "平成"
                   MOVE    "○"        TO  HCN02-PTBIRTHG(4)
           END-EVALUATE
      *
           MOVE    WRK-HENYMDG(5:4)    TO  HCN02-PTBIRTHYY
           MOVE    WRK-HENYMDG(11:4)   TO  HCN02-PTBIRTHMM
           MOVE    WRK-HENYMDG(17:4)   TO  HCN02-PTBIRTHDD
      *
      *    年齢
           MOVE    SPA-SYSYMD(1:4)     TO  WRK-SPA-SYSYYYY
           COMPUTE WRK-NENREI          =   WRK-SPA-SYSYY
                                       -   WRK-SYY
           IF      SPA-SYSYMD(5:4)     <   WRK-SYMD(5:4)
               COMPUTE WRK-NENREI      =   WRK-NENREI  -   1
           END-IF
           MOVE    WRK-NENREI          TO  WRK-Z3
           MOVE    WRK-Z3              TO  HCN02-PTNENREI
      *
      *
      *    入退院日編集処理
           INITIALIZE                  SYS-5000-REC
           MOVE    "5000"              TO  SYS-5000-KANRICD
           MOVE    "*"                 TO  SYS-5000-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-5000-HOSPNUM
           MOVE    SPA-SYSYMD          TO  SYS-5000-STYUKYMD
                                           SYS-5000-EDYUKYMD
           MOVE    SYS-5000-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-5000-REC
           END-IF
           IF   SYS-5000-TAIKSIKBN     =   SPACE OR "0"
               PERFORM 3101-NTYMD-HEN1-SEC
           ELSE
               PERFORM 3101-NTYMD-HEN2-SEC
           END-IF
      *
      *    通算期間統計
      *     MOVE    ORCHCMT-TKIKAN      TO  WRK-Z4
      *     MOVE    WRK-Z4              TO  HCN02-TKIKAN
      *
      *    通算期間年月日
      *     IF      ORCHCMT-TKIKANYMD   =   SPACE
      *         CONTINUE
      *     ELSE
      *         MOVE    ORCHCMT-TKIKANYMD    TO  WRK-SYMD
      *         PERFORM 31012-SEIWA-HEN-SEC
      *         MOVE    WRK-HENYMDG(5:4)    TO  HCN02-TKIKANYY
      *         MOVE    WRK-HENYMDG(11:4)   TO  HCN02-TKIKANMM
      *         MOVE    WRK-HENYMDG(17:4)   TO  HCN02-TKIKANDD
      *     END-IF
      *
      *    特記事項
      *     PERFORM VARYING IDX     FROM    1    BY   1
      *             UNTIL   IDX     >       ORCHCMT-KENSU
      *    入院種別
      *         MOVE    ORCHCMT-SYUBETU(IDX)    TO  WRK-HCN02-SYUBETU(IDX)
      *    通算期間
      *         MOVE    ORCHCMT-KIKAN(IDX)      TO  WRK-Z4
      *         MOVE    WRK-Z4                  TO  WRK-HCN02-KIKAN(IDX)
      *    通算期間開始年月日
      *         MOVE    ORCHCMT-KIKANSTRYMD(IDX)    TO  WRK-SYMD
      *         PERFORM 31012-SEIWA-HEN-SEC
      *         MOVE    WRK-HENYMDG(5:4)    TO  WRK-HCN02-KIKANSTRYY(IDX)
      *         MOVE    WRK-HENYMDG(11:4)   TO  WRK-HCN02-KIKANSTRMM(IDX)
      *         MOVE    WRK-HENYMDG(17:4)   TO  WRK-HCN02-KIKANSTRDD(IDX)
      *    通算期間終了年月日
      *         MOVE    ORCHCMT-KIKANENDYMD(IDX)    TO  WRK-SYMD
      *         PERFORM 31012-SEIWA-HEN-SEC
      *         MOVE    WRK-HENYMDG(5:4)    TO  WRK-HCN02-KIKANENDYY(IDX)
      *         MOVE    WRK-HENYMDG(11:4)   TO  WRK-HCN02-KIKANENDMM(IDX)
      *         MOVE    WRK-HENYMDG(17:4)   TO  WRK-HCN02-KIKANENDDD(IDX)
      *     END-PERFORM
      *
      *     IF      ORCHCMT-KENSU   =    0
      *         GO  TO  310-HC01-HEN-EXT
      *     END-IF
      *
      *     IF          ORCHCMT-KENSU   =    1
      *         MOVE    WRK-HCMT-SYUBETU(1)     TO HCN02-SYUBETU
      *         MOVE    WRK-HCMT-KIKAN(1)       TO HCN02-KIKAN
      *         MOVE    WRK-HCMT-KIKANSTRYY(1)  TO HCN02-KIKANSTRYY
      *         MOVE    WRK-HCMT-KIKANSTRMM(1)  TO HCN02-KIKANSTRMM
      *         MOVE    WRK-HCMT-KIKANSTRDD(1)  TO HCN02-KIKANSTRDD
      *         MOVE    WRK-HCMT-KIKANENDYY(1)  TO HCN02-KIKANENDYY
      *         MOVE    WRK-HCMT-KIKANENDMM(1)  TO HCN02-KIKANENDMM
      *         MOVE    WRK-HCMT-KIKANENDDD(1)  TO HCN02-KIKANENDDD
      *     ELSE
      *         PERFORM VARYING IDX     FROM    1    BY   1
      *                 UNTIL   IDX     >       ORCHCMT-KENSU
      *             MOVE    WRK-HCN02-TBL(IDX)   TO  HCN02-SONOTA(IDX)
      *         END-PERFORM
      *     END-IF
      *
           .
       310-HC01-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入退院日編集処理（入院料等の記載なし）
      *****************************************************************
       3101-NTYMD-HEN1-SEC              SECTION.
      *
           INITIALIZE                  ORCSNTYMDAREA
           MOVE    PTINF-HOSPNUM       TO  SNTYMD-HOSPNUM
           MOVE    PTINF-PTID          TO  SNTYMD-PTID
           MOVE    "3"                 TO  SNTYMD-KBN
           CALL    "ORCSNTYMD" USING   ORCSNTYMDAREA
                                       SPA-AREA
      *
           PERFORM VARYING IDX2    FROM    1   BY   1
                   UNTIL ( IDX2    >   SNTYMD-MAX )
               IF    ( LNK-PTNYUINRRK-RRKNUM
                                   =   SNTYMD-RRKNUM (IDX2))
      *            入院日
                   MOVE    SNTYMD-NYUINYMD(IDX2)
                                               TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG(1:4)    TO  HCN02-NYUINGEN
                   MOVE    WRK-HENYMDG(5:4)    TO  HCN02-NYUINYY
                   MOVE    WRK-HENYMDG(11:4)   TO  HCN02-NYUINMM
                   MOVE    WRK-HENYMDG(17:4)   TO  HCN02-NYUINDD
      *
      *            退院日
                   MOVE    LNK-PTNYUINRRK-TAIINYMD
                                               TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG(1:4)    TO  HCN02-TAIINGEN
                   MOVE    WRK-HENYMDG(5:4)    TO  HCN02-TAIINYY
                   MOVE    WRK-HENYMDG(11:4)   TO  HCN02-TAIINMM
                   MOVE    WRK-HENYMDG(17:4)   TO  HCN02-TAIINDD
      *
                   MOVE    SNTYMD-MAX          TO  IDX2
      *
               END-IF
           END-PERFORM
      *
      *
           .
       3101-NTYMD-HEN1-EXT.
           EXIT.
      *
      *****************************************************************
      *    入退院日編集処理（入院料等の記載あり）
      *****************************************************************
       3101-NTYMD-HEN2-SEC              SECTION.
      *
           INITIALIZE                  ORCHCN02SUB-AREA
      *
           INITIALIZE                  ORCSNTYMDAREA
           MOVE    PTINF-HOSPNUM       TO  SNTYMD-HOSPNUM
           MOVE    PTINF-PTID          TO  SNTYMD-PTID
           MOVE    "3"                 TO  SNTYMD-KBN
           CALL    "ORCSNTYMD" USING   ORCSNTYMDAREA
                                       SPA-AREA
      *
           PERFORM VARYING IDX2    FROM    1   BY   1
                   UNTIL ( IDX2    >   SNTYMD-MAX )
               IF    ( LNK-PTNYUINRRK-RRKNUM
                                   =   SNTYMD-RRKNUM (IDX2))
      *            入院日
                   MOVE    SNTYMD-NYUINYMD(IDX2)
                                               TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG(1:4)    TO  HCN02-NYUINGEN
                                                   HCN02-KIKANSTRGEN
                   MOVE    WRK-HENYMDG(5:4)    TO  HCN02-NYUINYY
                                                   HCN02-KIKANSTRYY
                   MOVE    WRK-HENYMDG(11:4)   TO  HCN02-NYUINMM
                                                   HCN02-KIKANSTRMM
                   MOVE    WRK-HENYMDG(17:4)   TO  HCN02-NYUINDD
                                                   HCN02-KIKANSTRDD
                   MOVE    SNTYMD-NYUINYMD(IDX2)
                                       TO  LNK-ORCHCN02SUB-NYUINYMD
      *
      *            退院日
                   MOVE    LNK-PTNYUINRRK-TAIINYMD
                                               TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG(1:4)    TO  HCN02-TAIINGEN
                                                   HCN02-KIKANENDGEN
                                                   HCN02-TKIKANGEN
                   MOVE    WRK-HENYMDG(5:4)    TO  HCN02-TAIINYY
                                                   HCN02-KIKANENDYY
                                                   HCN02-TKIKANYY
                   MOVE    WRK-HENYMDG(11:4)   TO  HCN02-TAIINMM
                                                   HCN02-KIKANENDMM
                                                   HCN02-TKIKANMM
                   MOVE    WRK-HENYMDG(17:4)   TO  HCN02-TAIINDD
                                                   HCN02-KIKANENDDD
                                                   HCN02-TKIKANDD
                   MOVE    LNK-PTNYUINRRK-TAIINYMD
                                       TO  LNK-ORCHCN02SUB-TAIINYMD
      *
                   MOVE    SNTYMD-MAX          TO  IDX2
      *
               END-IF
           END-PERFORM
      *
      *    退院証明書用入院取得サブ
      *
           MOVE    PTINF-HOSPNUM       TO  LNK-ORCHCN02SUB-HOSPNUM
           MOVE    PTINF-PTID          TO  LNK-ORCHCN02SUB-PTID
           CALL    "ORCHCN02SUB"       USING   ORCHCN02SUB-AREA
                                               SPA-AREA
           IF      LNK-ORCHCN02SUB-RC  NOT =   ZERO
               DISPLAY "LNK-ORCHCN02SUB-RC=" LNK-ORCHCN02SUB-RC 
           END-IF
      *
           MOVE    ZERO      TO     KISAI-IDX
      *
           PERFORM    VARYING   MEISAI-IDX  FROM  1   BY   1
              UNTIL   (MEISAI-IDX            >   10)     OR
                      (LNK-ORCHCN02SUB-SYUBETUCD(MEISAI-IDX) = SPACE) OR
                      (KISAI-IDX             >=   3)                       
               IF   (LNK-ORCHCN02SUB-SYUBETUCD(MEISAI-IDX) NOT = SPACE)
                AND (LNK-ORCHCN02SUB-NISSU(MEISAI-IDX)   NOT =   ZERO)
                   ADD     1                   TO   KISAI-IDX
                   MOVE  LNK-ORCHCN02SUB-SYUBETUMEI(MEISAI-IDX)
                                           TO   HCN02-SYUBETU(KISAI-IDX)
                   MOVE  LNK-ORCHCN02SUB-NISSU(MEISAI-IDX)
                                           TO   WRK-Z5
                   MOVE  WRK-Z5            TO   HCN02-NISSU(KISAI-IDX)
                   MOVE  "日間"            TO   HCN02-NISSU(KISAI-IDX)
                                                           (6:)
               END-IF
           END-PERFORM
           COMPUTE   WRK-Z5   =   LNK-ORCHCN02SUB-NISSU(1)   +
                                  LNK-ORCHCN02SUB-NISSU(2)   +
                                  LNK-ORCHCN02SUB-NISSU(3)   +
                                  LNK-ORCHCN02SUB-NISSU(4)   +
                                  LNK-ORCHCN02SUB-NISSU(5)   +
                                  LNK-ORCHCN02SUB-NISSU(6)   +
                                  LNK-ORCHCN02SUB-NISSU(7)   +
                                  LNK-ORCHCN02SUB-NISSU(8)   +
                                  LNK-ORCHCN02SUB-NISSU(9)   +
                                  LNK-ORCHCN02SUB-NISSU(10)
           MOVE  WRK-Z5                    TO   HCN02-KIKAN
      *
           MOVE   LNK-ORCHCN02SUB-TKIKAN   TO   WRK-Z5
           MOVE   WRK-Z5                   TO   HCN02-TKIKAN
      *
      *    予備エリアの印字
           MOVE      ZERO         TO      FLG-YOBI
           MOVE      ZERO         TO      WRK-YOBI-NISSU
           PERFORM   VARYING   IDX   FROM  MEISAI-IDX  BY  1
             UNTIL  (IDX        >    10)    OR
                    (LNK-ORCHCN02SUB-SYUBETUCD(IDX)   =   SPACE)
                IF   LNK-ORCHCN02SUB-NISSU(IDX)    NOT =   ZERO
                     MOVE   1      TO     FLG-YOBI
                     ADD    LNK-ORCHCN02SUB-NISSU(IDX)
                                   TO     WRK-YOBI-NISSU
               END-IF
           END-PERFORM
           IF     FLG-YOBI     =     1
               MOVE  "上記他"              TO   HCN02-YOBI(1:6)
               MOVE  WRK-YOBI-NISSU        TO   WRK-Z5
               MOVE  WRK-Z5                TO   HCN02-YOBI(7:5)
               MOVE  "日有"                TO   HCN02-YOBI(12:4)
           END-IF
      *
      *    印刷対象外箇所の初期化
           IF   SYS-5000-TAIKSIKBN     =   "2"
               MOVE   SPACE        TO  HCN02-TKIKAN
               MOVE   SPACE        TO  HCN02-TKIKANGEN
               MOVE   SPACE        TO  HCN02-TKIKANYY
               MOVE   SPACE        TO  HCN02-TKIKANMM
               MOVE   SPACE        TO  HCN02-TKIKANDD
           END-IF
      *
           IF   SYS-5000-TAIKSIKBN     =   "3"
               PERFORM    VARYING   IDX   FROM  1  BY  1
                 UNTIL    IDX   >   3
                     MOVE   SPACE  TO  HCN02-SYUBETU(IDX)
                     MOVE   SPACE  TO  HCN02-NISSU(IDX)
               END-PERFORM
               MOVE   SPACE        TO  HCN02-KIKAN
               MOVE   SPACE        TO  HCN02-KIKANSTRGEN
               MOVE   SPACE        TO  HCN02-KIKANSTRYY
               MOVE   SPACE        TO  HCN02-KIKANSTRMM
               MOVE   SPACE        TO  HCN02-KIKANSTRDD
               MOVE   SPACE        TO  HCN02-KIKANENDGEN
               MOVE   SPACE        TO  HCN02-KIKANENDYY
               MOVE   SPACE        TO  HCN02-KIKANENDMM
               MOVE   SPACE        TO  HCN02-KIKANENDDD
               MOVE   SPACE        TO  HCN02-YOBI
           END-IF
      *
           .
       3101-NTYMD-HEN2-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       2009-HENKAN-SEC                 SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-OUT-INPUT
           ELSE
               MOVE    WRK-MAE-INPUT       TO  WRK-OUT-INPUT
           END-IF
      *
           .
       2009-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読み込み
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    ZERO           TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri" TO  MCP-TABLE
           MOVE    "key10"        TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC         =   ZERO )
               CONTINUE
           ELSE
               MOVE    1          TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri" TO  MCP-TABLE
           MOVE    "key10"        TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
