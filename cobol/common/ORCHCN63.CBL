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
       PROGRAM-ID.             ORCHCN63.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 分娩費用明細書
      *  コンポーネント名  : 分娩費用明細書（ＨＣＮ６３）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09/08/26    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
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
       01  PRT-REC.
           03  PRT-PRTDATA         PIC X(20000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *
      *    帳票領域
           COPY    "HCN63.INC".
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-BUNBEN          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-HOSPNAME        PIC X(100).
           03  WRK-HOSPNAME-R      REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1   PIC X(50).
               05  WRK-HOSPNAME2   PIC X(50).
           03  WRK-HOSPADRS        PIC X(100).
           03  WRK-HOSPADRS-R      REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1   PIC X(50).
               05  WRK-HOSPADRS2   PIC X(50).
           03  WRK-HOSPTEL         PIC X(15).
           03  WRK-999             PIC 9(03).
      *
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-Z03             PIC ZZ9.
           03  WRK-Z10             PIC ZZ,ZZZ,ZZ9.
      *
           03  WRK-MOJI-IN         PIC X(100).
           03  WRK-MOJI-OUT        PIC X(100).
           03  WRK-PNT             PIC 9(05).
           03  WRK-SKYMONEY        PIC 9(10).
           03  WRK-STR-G.
               05  WRK-STR1        PIC X(100).
               05  WRK-STR2        PIC X(100).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    医療機関情報−基本情報
           COPY    "CPSK1001.INC".
      *    医療機関情報−所在地、連絡先
           COPY    "CPSK1002.INC".
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1034.INC".
      *
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *
      *    分娩情報
       01  BUNBEN-REC.
           COPY    "CPBUNBEN.INC".
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角変換サブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
           COPY    "CPORCHCN63.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       COPY    "COMMON-SPA".
       COPY    "CPONPRTDATA.INC".
      *
      ******************************************************************
       PROCEDURE               DIVISION
                                   USING
                                   SPA-AREA
                                   ONPRTDATA
                                   .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  SYS-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
           ACCEPT  SYS-TIME    FROM    TIME
      *
           MOVE    ONPRTDATA-DATA  TO  ORCHCN63AREA
      *
      *    出力先プリンタ名割り当て情報
           PERFORM 900-1034-READ-SEC
      *
      *    医療機関情報−基本情報
           PERFORM 900-1001-READ-SEC
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
      *
      *    医療機関情報−所在地、連絡先
           PERFORM 900-1002-READ-SEC
           MOVE    SYS-1002-ADRS   TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL    TO  WRK-HOSPTEL
      *
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *
      *    分娩情報
           PERFORM 900-BUNBEN-READ-SEC
      *    患者情報
           PERFORM 900-PTINF-READ-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                  HCN63
      *
      *    分娩区分
           EVALUATE    BUNBEN-BUNBENKBN
               WHEN    1
                   MOVE    "（正常分娩）"      TO  HCN63-BUNBENKBN
      *            分娩介助料
                   MOVE    "−　　　　"        TO  HCN63-KAIJO-MONEY
      *            分娩料
                   MOVE    BUNBEN-BUNBEN-MONEY TO  WRK-Z10
                   MOVE    WRK-Z10             TO  WRK-MOJI-IN
                   PERFORM 31011-KANA-HEN-SEC
                   MOVE    WRK-MOJI-OUT        TO  HCN63-BUNBEN-MONEY
               WHEN    2
                   MOVE    "（異常分娩）"      TO  HCN63-BUNBENKBN
      *            分娩介助料
                   MOVE    BUNBEN-KAIJO-MONEY  TO  WRK-Z10
                   MOVE    WRK-Z10             TO  WRK-MOJI-IN
                   PERFORM 31011-KANA-HEN-SEC
                   MOVE    WRK-MOJI-OUT        TO  HCN63-KAIJO-MONEY
      *            分娩料
                   MOVE    "−　　　　"        TO  HCN63-BUNBEN-MONEY
           END-EVALUATE
      *
      *    患者番号
           MOVE    BUNBEN-PTNUM    TO  HCN63-PTNUM
      *
      *    患者氏名
           MOVE    PTINF-NAME      TO  HCN63-NAME
      *
      *    日付セット
           MOVE    SPA-SYSYMD      TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  HCN63-PRTYMD
      *
      *    出産日
           MOVE    BUNBEN-BIRTHDAY TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  HCN63-BIRTHDAY
      *
      *    直接支払制度
           EVALUATE    BUNBEN-RIYOUKBN
               WHEN    "0"
                   MOVE    "利用しない"    TO  HCN63-RIYOUKBN
                   MOVE
        "この明細書の内容については「直接支払制度」を利用していません。"
                                           TO  HCN63-COMMENT
               WHEN    "1"
                   MOVE    "利用する"      TO  HCN63-RIYOUKBN
                   MOVE
              "この明細書の内容は「専用請求書」の内容と相違ありません。"
                                           TO  HCN63-COMMENT
           END-EVALUATE
      *
      *    産科医療補償制度区分
           EVALUATE    BUNBEN-SANKAKBN
               WHEN    "1"
                   MOVE    "対象"          TO  HCN63-SANKAKBN
               WHEN    "2"
                   MOVE    "対象外"        TO  HCN63-SANKAKBN
               WHEN    "3"
                   MOVE    "混在"          TO  HCN63-SANKAKBN
           END-EVALUATE
      *
      *    入院日数
           MOVE    BUNBEN-NYUIN-DAY    TO  WRK-Z03
           MOVE    WRK-Z03             TO  WRK-MOJI-IN
           PERFORM 31011-KANA-HEN-SEC
           MOVE    WRK-MOJI-OUT        TO  HCN63-NYUIN-DAY
      *
      *    入院料
           MOVE    BUNBEN-NYU-MONEY    TO  WRK-Z10
           MOVE    WRK-Z10             TO  WRK-MOJI-IN
           PERFORM 31011-KANA-HEN-SEC
           MOVE    WRK-MOJI-OUT        TO  HCN63-NYU-MONEY
      *
      *    室料差額
           MOVE    BUNBEN-RMSGK-MONEY  TO  WRK-Z10
           MOVE    WRK-Z10             TO  WRK-MOJI-IN
           PERFORM 31011-KANA-HEN-SEC
           MOVE    WRK-MOJI-OUT        TO  HCN63-RMSGK-MONEY
      *
      *    新生児管理保育料
           MOVE    BUNBEN-SHIN-MONEY   TO  WRK-Z10
           MOVE    WRK-Z10             TO  WRK-MOJI-IN
           PERFORM 31011-KANA-HEN-SEC
           MOVE    WRK-MOJI-OUT        TO  HCN63-SHIN-MONEY
      *
      *    検査・薬剤料
           MOVE    BUNBEN-KNS-MONEY    TO  WRK-Z10
           MOVE    WRK-Z10             TO  WRK-MOJI-IN
           PERFORM 31011-KANA-HEN-SEC
           MOVE    WRK-MOJI-OUT        TO  HCN63-KNS-MONEY
      *
      *    処置・手当料
           MOVE    BUNBEN-SYC-MONEY    TO  WRK-Z10
           MOVE    WRK-Z10             TO  WRK-MOJI-IN
           PERFORM 31011-KANA-HEN-SEC
           MOVE    WRK-MOJI-OUT        TO  HCN63-SYC-MONEY
      *
      *    産科医療補償制度料
           MOVE    BUNBEN-SNK-MONEY    TO  WRK-Z10
           MOVE    WRK-Z10             TO  WRK-MOJI-IN
           PERFORM 31011-KANA-HEN-SEC
           MOVE    WRK-MOJI-OUT        TO  HCN63-SNK-MONEY
      *
      *    その他
           MOVE    BUNBEN-ETC-MONEY    TO  WRK-Z10
           MOVE    WRK-Z10             TO  WRK-MOJI-IN
           PERFORM 31011-KANA-HEN-SEC
           MOVE    WRK-MOJI-OUT        TO  HCN63-ETC-MONEY
      *
      *    一部負担金等
           MOVE    BUNBEN-ICBFTN-MONEY TO  WRK-Z10
           MOVE    WRK-Z10             TO  WRK-MOJI-IN
           PERFORM 31011-KANA-HEN-SEC
           MOVE    WRK-MOJI-OUT        TO  HCN63-ICBFTN-MONEY
      *
      *    妊婦合計負担額
           MOVE    BUNBEN-NINPUFTN-MONEY
                                       TO  WRK-Z10
           MOVE    WRK-Z10             TO  WRK-MOJI-IN
           PERFORM 31011-KANA-HEN-SEC
           MOVE    WRK-MOJI-OUT        TO  HCN63-NINPUFTN-MONEY
      *
      *    代理受取額
           MOVE    BUNBEN-DAIRI-MONEY  TO  WRK-Z10
           MOVE    WRK-Z10             TO  WRK-MOJI-IN
           PERFORM 31011-KANA-HEN-SEC
           MOVE    WRK-MOJI-OUT        TO  HCN63-DAIRI-MONEY
      *
      *    所在地
           MOVE    WRK-HOSPADRS    TO  HCN63-HOSPADRS
      *    医療機関名称
           MOVE    WRK-HOSPNAME    TO  HCN63-HOSPNAME
      *    電話番号
           MOVE    WRK-HOSPTEL     TO  HCN63-HOSPTEL
      *
      *    文字１
           MOVE    SPACE           TO  HCN63-MOJI1
           IF    ( BUNBEN-NINPUFTN-MONEY   >   BUNBEN-DAIRI-MONEY )
               MOVE    1           TO  WRK-PNT
               STRING  "代理受取額"  DELIMITED   BY  SIZE
               INTO    HCN63-MOJI1
               WITH POINTER WRK-PNT
               END-STRING
               MOVE    BUNBEN-DAIRI-MONEY 
                                       TO  WRK-Z10
               MOVE    WRK-Z10         TO  WRK-MOJI-IN
               PERFORM 31011-KANA-HEN-SEC
               PERFORM 31011-LTRIM-SEC
               STRING  WRK-MOJI-OUT    DELIMITED   BY  SPACE
                       "円につき、現金清算は"
               INTO    HCN63-MOJI1
               WITH POINTER WRK-PNT
               END-STRING
               COMPUTE WRK-SKYMONEY
                   =   BUNBEN-NINPUFTN-MONEY
                   -   BUNBEN-DAIRI-MONEY
               MOVE    WRK-SKYMONEY   TO   WRK-Z10
               MOVE    WRK-Z10        TO   WRK-MOJI-IN
               PERFORM 31011-KANA-HEN-SEC
               PERFORM 31011-LTRIM-SEC
               STRING  WRK-MOJI-OUT    DELIMITED   BY  SPACE
                       "円"
               INTO    HCN63-MOJI1
               WITH POINTER WRK-PNT
               END-STRING
           END-IF
      *
      *    出産児数
           MOVE    SPACE           TO  WRK-MOJI-IN
           STRING  "（出産児数："      DELIMITED BY SIZE
                   BUNBEN-BIRTHCNT     DELIMITED BY SIZE
                   "人）"              DELIMITED BY SIZE
                   INTO                WRK-MOJI-IN
           END-STRING
           PERFORM 31011-KANA-HEN-SEC
           MOVE    WRK-MOJI-OUT        TO  HCN63-BIRTHCNT
      *
           IF    ( BUNBEN-BUNBENKBN    =   "1" )
      *        文字２
               STRING  "※上記一部負担金等は、専用請求書上"
                                           DELIMITED   BY  SIZE
                       "「その他」の費用として計上しています。"
                                           DELIMITED   BY  SIZE
               INTO    HCN63-MOJI2
               END-STRING
           END-IF
      *
      *    備考
      *     MOVE    BUNBEN-BIKOU        TO  WRK-MOJI-IN
      *     PERFORM 31011-KANA-HEN-SEC
      *     MOVE    WRK-MOJI-OUT        TO  HCN63-BIKOU
      *
      *    帳票印刷処理
           PERFORM 400-PRT-WRITE-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力処理
      *****************************************************************
       400-PRT-WRITE-SEC           SECTION.
      *
      *    印刷用ファイル作成
           MOVE    SPA-HOSPNUM     TO  HC01PARA-HOSPNUM
           MOVE    "HCN63"         TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID      TO  HC01PARA-TERMID
           MOVE    SYS-TIME        TO  HC01PARA-SYOHMS
           MOVE    1               TO  HC01PARA-CNT
           OPEN    OUTPUT              PRT-FILE
           MOVE    SPACE           TO  PRT-REC
      *
      *    印刷用データ出力
           MOVE    HCN63           TO  PRT-PRTDATA
           WRITE                       PRT-REC
      *
           CLOSE                       PRT-FILE
      *
      *    帳票印刷処理
           INITIALIZE                  ORCSMKPRTAREA
           MOVE    "HC01.sh"       TO  MKPRT-ID
           MOVE    "HCN63.red"     TO  MKPRT-DIA
           MOVE    SPACE           TO  MKPRT-DEF
           MOVE    HC01PARA        TO  MKPRT-DAT
           MOVE    SYS-1034-PRTNM(13)
                                   TO  MKPRT-PRTNM
           MOVE   "分娩費用明細書" TO  MKPRT-INFO
           CALL   "ORCSMKPRT1"         USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           .
       400-PRT-WRITE-EXT.
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
      *****************************************************************
      *    左詰め処理
      *****************************************************************
       31011-LTRIM-SEC             SECTION.
      *
           MOVE    SPACE           TO  WRK-STR-G
           UNSTRING    WRK-MOJI-OUT    DELIMITED   BY  ALL "　"
           INTO        WRK-STR1 ,
                       WRK-STR2
           END-UNSTRING
           IF    ( WRK-STR1    =   SPACE )
               MOVE    WRK-STR2    TO  WRK-STR1
           END-IF
      *
           MOVE    WRK-STR1        TO  WRK-MOJI-OUT
      *
           .
       31011-LTRIM-EXT.
           EXIT.
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       31011-KANA-HEN-SEC          SECTION.
      *
      *    全角変換
           MOVE    SPACE           TO  ORCSKANACHKAREA
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI
           MOVE    WRK-MOJI-IN     TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                   TO  WRK-MOJI-OUT
      *
           .
       31011-KANA-HEN-EXT.
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
           MOVE    LNK-DAY2-EDTYMD3
                                   TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG         REPLACING  ALL "  "  BY  "　"
      *
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み
      *****************************************************************
       900-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関情報−基本情報読み込み
      *****************************************************************
       900-1001-READ-SEC           SECTION.
      *
      *    医療機関情報−基本情報読み込み
           MOVE    SPACE           TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    "*"             TO  SYS-1001-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-1001-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
           ELSE
               INITIALIZE                  SYS-1001-REC
           END-IF
      *
           .
       900-1001-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関情報−所在地、連絡先読み込み
      *****************************************************************
       900-1002-READ-SEC           SECTION.
      *
      *    医療機関情報−所在地、連絡先読み込み
           MOVE    SPACE           TO  SYS-1002-REC
           INITIALIZE                  SYS-1002-REC
           MOVE    "1002"          TO  SYS-1002-KANRICD
           MOVE    "*"             TO  SYS-1002-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-1002-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1002-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1002-REC
           ELSE
               INITIALIZE                  SYS-1002-REC
           END-IF
      *
           .
       900-1002-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力先プリンタ名割り当て情報読み込み
      *****************************************************************
       900-1034-READ-SEC           SECTION.
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                  SYS-1034-REC
           INITIALIZE                  ORCSPRTNMAREA
           MOVE    "2"             TO  ORCSPRTNM-KBN
           MOVE    SPA-SRYYMD      TO  ORCSPRTNM-SRYYMD
           MOVE    "1"             TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYS-1034-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               INITIALIZE                  SYS-1034-REC
           END-IF
      *
           .
       900-1034-READ-EXT.
           EXIT.
      *
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
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(11) NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-1902-READ-SEC
               END-IF
           ELSE
               INITIALIZE                  SYS-1900-REC
           END-IF
      *
           .
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
           MOVE    SYS-1900-PRTKBN(11)
                                   TO  SYS-1901-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-1901-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1901-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1901-HOSPNUM
           MOVE    SYS-1901-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1
                                       TO  WRK-HOSPNAME1
               MOVE    SYS-1901-HOSPNAME2
                                       TO  WRK-HOSPNAME2
           ELSE
               INITIALIZE                  SYS-1901-REC
           END-IF
      *
           .
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
           MOVE    SYS-1900-PRTKBN(11)
                                   TO  SYS-1902-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-1902-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1902-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
           MOVE    SYS-1902-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1902-REC
               MOVE    SYS-1902-ADRS1  TO  WRK-HOSPADRS1
               MOVE    SYS-1902-ADRS2  TO  WRK-HOSPADRS2
               MOVE    SYS-1902-TEL    TO  WRK-HOSPTEL
           ELSE
               INITIALIZE                  SYS-1902-REC
           END-IF
      *
           .
       900-1902-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    分娩情報
      *****************************************************************
       900-BUNBEN-READ-SEC         SECTION.
      *
           MOVE    SPACE           TO  BUNBEN-REC
           INITIALIZE                  BUNBEN-REC
           MOVE    SPA-HOSPNUM     TO  BUNBEN-HOSPNUM
           MOVE    ORCHCN63-PTID   TO  BUNBEN-PTID
           MOVE    ORCHCN63-RRKNUM TO  BUNBEN-RRKNUM
           MOVE    BUNBEN-REC      TO  MCPDATA-REC
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_bunben"    TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-BUNBEN
                   MOVE    MCPDATA-REC     TO  BUNBEN-REC
               ELSE
                   MOVE    1               TO  FLG-BUNBEN
                   INITIALIZE                  BUNBEN-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-BUNBEN
               INITIALIZE                  BUNBEN-REC
           END-IF
      *
           MOVE    "tbl_bunben"   TO  MCP-TABLE
           MOVE    "key3"         TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       900-BUNBEN-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           MOVE    SPACE           TO  PTINF-REC
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    ORCHCN63-PTID   TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-PTINF
                   MOVE    MCPDATA-REC     TO  PTINF-REC
               ELSE
                   MOVE    1               TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読込開始処理
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読込検索処理
      *****************************************************************
       920-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読込終了処理
      *****************************************************************
       930-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       930-DBCLOSE-EXT.
           EXIT.
      *
