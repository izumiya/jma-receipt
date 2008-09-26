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
           COPY    "HCM04.INC".
      *
       WORKING-STORAGE             SECTION.
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
      *
           03  FLG-SP              PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-PLUSKBN         PIC 9(01).
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
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-MEI           PIC X(30).
           03  WRK-PAGE                PIC 9(03).
           03  WRK-PAGE-CNT            PIC 9(03).
           03  WRK-PAGE-MAX            PIC 9(03).
      *
           03  WRK-KAISU               PIC 9(03).
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-SRYSYUKBN           PIC X(03).
           03  WRK-KINGAKU             PIC S9(11).
           03  WRK-SRYMEI              PIC X(10).
           03  WRK-SRYMEI-OLD          PIC X(10).
      *
           03  WRK-Z119                PIC Z,ZZZ,ZZ9.
           03  WRK-Z119-2              PIC -,---,--9.
           03  WRK-ZZ9                 PIC ZZ9.
           03  WRK-NO                  PIC Z(7).
           03  WRK-WARI                PIC 9(02)V9.
           03  WRK-Z6                  PIC ZZZ.
           03  WRK-Z62                 PIC 9.9.
      *
           03  WRK-HKNCOMBIMEI     PIC X(40).
      *
           03  WRK-DENPNUM             PIC 9(07).
           03  WRK-GRP-DENPNUM         PIC 9(07).
      *
           03  WRK-SURYO           PIC 9(05)V9(03).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(03).
      *
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1    PIC ZZZ9.
               05  WRK-SURYO-Z2    PIC X(01).
               05  WRK-SURYO-Z3    PIC ZZZ.
           03  WRK-SURYO-X-R       REDEFINES   WRK-SURYO-X.
               05  WRK-SURYO-ZF    PIC X(03).
               05  WRK-SURYO-Z4    PIC ZZZZ9.
      *
      *----(03.04.01)--ADD-START---
           03  WRK-HOSPNAME            PIC X(100).
           03  WRK-HOSPNAME-R          REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1       PIC X(50).
               05  WRK-HOSPNAME2       PIC X(50).
           03  WRK-HOSPADRS            PIC X(100).
           03  WRK-HOSPADRS-R          REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1       PIC X(50).
               05  WRK-HOSPADRS2       PIC X(50).
           03  WRK-HOSPTEL         PIC X(15).
      *----(03.04.01)--ADD-END-----
      *
       01  WRK-MEISAI-AREA.
           03  WRK-TBL-AREA            OCCURS   50.
               05  WRK-TBL-SRYCD        PIC X(09).
               05  WRK-TBL-NAME         PIC X(80).
               05  WRK-TBL-TANINAME     PIC X(10).
               05  WRK-TBL-SURYO        PIC 9(05)V9(3).
               05  WRK-TBL-JIHIMONEY    PIC 9(09).
      *
       01  TBL-DATA-AREA.
           03  TBL-DATA-TBL            OCCURS   200.
               05  TBL-SRYKBN          PIC X(02).
               05  TBL-SRYSYUKBN       PIC X(03).
               05  TBL-ZAITENSU        PIC S9(07).
               05  TBL-KAISU           PIC 9(03).
               05  TBL-ZAI-TBL         OCCURS   50.
                   07  TBL-SRYCD           PIC X(09).
                   07  TBL-NAME            PIC X(80).
                   07  TBL-TANINAME        PIC X(10).
                   07  TBL-SURYO           PIC 9(05)V9(3).
                   07  TBL-JIHIMONEY       PIC S9(07).
      *
      *
       01  TBL-SORT-AREA.
           03  TBL-SORT-TBL            OCCURS   200.
               05  TBL-SORT-SRYKBN          PIC X(02).
               05  TBL-SORT-SRYSYUKBN       PIC X(03).
               05  TBL-SORT-ZAITENSU        PIC S9(07).
               05  TBL-SORT-KAISU           PIC 9(03).
               05  TBL-SORT-ZAI-TBL         OCCURS   50.
                   07  TBL-SORT-SRYCD           PIC X(09).
                   07  TBL-SORT-NAME            PIC X(80).
                   07  TBL-SORT-TANINAME        PIC X(10).
                   07  TBL-SORT-SURYO           PIC 9(05)V9(3).
                   07  TBL-SORT-JIHIMONEY       PIC S9(07).
      *
       01  TBL-MEISAI-AREA.
           03  TBL-MEISA-PAGE          OCCURS   10.
               05  TBL-MEISA-REC           OCCURS   70.
                   07  TBL-H-SRYKBN          PIC X(10).
                   07  TBL-H-SRYMEI          PIC X(80).
                   07  TBL-H-TANINAME        PIC X(10).
                   07  TBL-H-SURYO           PIC X(08).
                   07  TBL-H-KAISU           PIC X(03).
                   07  TBL-H-KINGAKU         PIC X(11).
      *帳票明細数
       01  LINE-MAX                    PIC 9(03)   VALUE   70.
       01  CONST-H201001               PIC X(08)   VALUE   "20081001".
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
      *
           COPY    "MCPDATA.INC".
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
           ACCEPT  SYS-TIME            FROM    TIME
      *
      *    各ＤＢ読込
           PERFORM 100-SYORI-SEC
      *
           IF      ORCHC04-HAKHOUFLG   =   ZERO
      *        対象の伝票番号分のみ
               PERFORM 201-ONLY-SYUNOU-SEC
           ELSE
      *        発行区分による
               PERFORM 202-KAKHOU-SYUNOU-SEC
           END-IF
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
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
           WRITE  HCM04
      *
           CLOSE                       PRT-FILE
      *
           INITIALIZE                 ORCSMKPRTAREA
           MOVE   "HC01.sh"           TO  MKPRT-ID
           MOVE   "HCM04.red"         TO  MKPRT-DIA
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
      *   対象の伝票番号分のみ処理
      *****************************************************************
       201-ONLY-SYUNOU-SEC                SECTION.
      *
      *    収納検索
           PERFORM 2011-SYUNOU-HEN-SEC
      *    伝票番号毎に印刷
           PERFORM 2012-DENPNUM-SYORI-SEC
           .
       201-ONLY-SYUNOU-EXT.
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
           MOVE    ORCHC04-DENPNUM     TO  SYU-DENPNUM
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
      *   伝票番号毎印刷処理
      *****************************************************************
       2012-DENPNUM-SYORI-SEC          SECTION.
      *
           INITIALIZE                  TBL-DATA-AREA
           MOVE    ZERO                TO  IDX-TBL
      *    受診履歴検索
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SYU-DENPNUM         TO  JYURRK-DENPNUM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 910-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           PERFORM UNTIL    FLG-JYURRK     =   1
      *        受診履歴より編集
               PERFORM 2001-JYURRK-HEN-SEC
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 910-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    帳票発行
           IF      IDX-TBL             >   ZERO
               PERFORM 2005-PRINT-SYORI-SEC
           END-IF
           .
       2012-DENPNUM-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴からの判定編集処理
      *****************************************************************
       2001-JYURRK-HEN-SEC          SECTION.
      *
           PERFORM VARYING     IDX-JYU     FROM    1   BY  1
                   UNTIL      (IDX-JYU         >   15  )   OR
                              (JYURRK-ZAINUM (IDX-JYU) =   ZERO )
               COMPUTE IDX-REN         =   JYURRK-RENNUM
                                       +   1
               IF      IDX-REN         >   4
                   MOVE    4               TO  IDX-REN
               END-IF
               PERFORM 2002-SRYACCT-SYORI-SEC
           END-PERFORM
      *
           .
       2001-JYURRK-HEN-EXT.
           EXIT.
      *****************************************************************
      *        診療行為より編集処理
      *****************************************************************
       2002-SRYACCT-SYORI-SEC      SECTION.
      *
      *    診療会計
           INITIALIZE                      SRYACCT-REC
           MOVE    JYURRK-HOSPNUM      TO  ACCT-HOSPNUM
           MOVE    JYURRK-NYUGAIKBN    TO  ACCT-NYUGAIKBN
           MOVE    JYURRK-PTID         TO  ACCT-PTID
           MOVE    JYURRK-SRYKA        TO  ACCT-SRYKA
           MOVE    JYURRK-SRYYMD(1:6)  TO  ACCT-SRYYM
           MOVE    JYURRK-ZAINUM (IDX-JYU)
                                       TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    JYURRK-SRYYMD(7:2)  TO  IDX-ACT
           MOVE    ACCT-DAY (IDX-REN IDX-ACT)
                                       TO  WRK-KAISU
      *    回数ゼロ、点数ゼロ、コメントは対象外
           IF     (WRK-KAISU           =   ZERO )  OR
                  (ACCT-ZAITEN         =   ZERO )  OR
                  (ACCT-SRYKBN         =   "98"
                                       OR  "99" )
      *---(03.04.02) LINE ADD START -----------------------------------
      *      包括分は対象外
               OR (ACCT-HKNCOMBI       =   "9999")
               OR (ACCT-JIHOKBN        =   "1"   )
      *---(03.04.02) LINE ADD END   -----------------------------------
               CONTINUE
           ELSE
               PERFORM 20021-SRYACT-SYORI-SEC
           END-IF
           .
       2002-SRYACCT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *        診療行為より編集処理
      *****************************************************************
       20021-SRYACT-SYORI-SEC      SECTION.
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
           INITIALIZE                  WRK-MEISAI-AREA
           MOVE    ZERO                TO  IDX-S
           PERFORM
                   UNTIL   FLG-SRYACT      =   1
               PERFORM VARYING     IDXA    FROM    1   BY  1
                       UNTIL      (IDXA    >   5   )   OR
                                  (SRY-SRYCD (IDXA)    =   SPACE)
      *            診療・薬剤・器材・部位・自費を対象とする
                   IF      (SRY-SRYCD (IDXA)(1:1)      =   "1"
                                                       OR  "6"
                                                       OR  "7" ) OR
                           (SRY-SRYCD (IDXA)(1:3)      =   "059"
                                                       OR  "002"
                                                       OR  "095"
                                                       OR  "096")
      *            器材商品名対象
                       OR  (SRY-SRYCD (IDXA)(1:3)      =   "058")
                       PERFORM 200211-MEISE-HEN-SEC
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SRYACT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    剤ごとに、編集する
           IF      IDX-S               >   ZERO
               PERFORM 20023-TBL-HENSYU-SEC
           END-IF
           .
       20021-SRYACT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *        明細編集処理
      *****************************************************************
       200211-MEISE-HEN-SEC      SECTION.
      *
      *
           MOVE    SRY-SRYCD (IDXA)    TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *
           ADD     1                   TO  IDX-S
           MOVE    SRY-SRYCD (IDXA)    TO  WRK-TBL-SRYCD (IDX-S)
           MOVE    TNS-NAME            TO  WRK-TBL-NAME  (IDX-S)
      *    器材商品名コード
           IF      SRY-SRYCD (IDXA)(1:3)   =   "058"
               MOVE   TNS-FORMALNAME       TO  WRK-TBL-NAME  (IDX-S)
           END-IF
      *    器材商品名の器材コード
           IF     (SRY-SRYCD (IDXA)(1:1)   =   "7" ) AND
                  (SRY-MEISKYFLG  (IDXA)   =   "1" )
               MOVE    SPACE               TO  WRK-TBL-NAME  (IDX-S)
               STRING     "〔"             DELIMITED   BY  SIZE
                          TNS-NAME         DELIMITED   BY  "    "
                          "〕"             DELIMITED   BY  SIZE
                          INTO             WRK-TBL-NAME  (IDX-S)
               END-STRING
           END-IF
      *
           IF      TNS-TANICD          =   ZERO
                                       OR  SPACE
               CONTINUE
           ELSE
               MOVE    TNS-TANINAME        TO  WRK-TBL-TANINAME(IDX-S)
               MOVE    SRY-SRYSURYO(IDXA)  TO  WRK-TBL-SURYO (IDX-S)
           END-IF
           IF      SRY-SRYKBN          =   "95"
                                       OR  "96"
      *        自費は１行１明細とする
               COMPUTE WRK-TBL-JIHIMONEY(IDX-S)
                                       =   SRY-JIHIMONEY(IDXA)
                                       *   WRK-KAISU
               MOVE    ZERO                TO  WRK-TBL-SURYO (IDX-S)
           END-IF
           MOVE    SRY-SRYSYUKBN       TO  WRK-SRYSYUKBN
           .
       200211-MEISE-HEN-EXT.
           EXIT.
      *****************************************************************
      *        テーブル編集処理
      *****************************************************************
       20023-TBL-HENSYU-SEC      SECTION.
      *
           ADD     1                   TO  IDX-TBL
           IF      IDX-TBL             >   200
               MOVE    200                 TO  IDX-TBL
               PERFORM 2005-PRINT-SYORI-SEC
               MOVE    1                   TO  IDX-TBL
           END-IF
      *
           IF      ACCT-SRYKBN         =   "95"
                                       OR  "96"
               CONTINUE
           ELSE
               IF      ACCT-ZAIREQFLG      =   "1"
      *            附加情報あり
                   PERFORM 200231-SRYACCTPLUS-CHK-SEC
               ELSE
                   MOVE    ACCT-ZAITEN     TO  TBL-ZAITENSU (IDX-TBL)
               END-IF
           END-IF
           MOVE    ACCT-SRYKBN         TO  TBL-SRYKBN (IDX-TBL)
      *    診療区分 ８０ の時、変更
           IF      ACCT-SRYKBN         =   "80"
               EVALUATE    WRK-SRYSYUKBN
                   WHEN    "800"
                       MOVE    "80"        TO  TBL-SRYKBN (IDX-TBL)
                   WHEN    "820"
                       MOVE    "29"        TO  TBL-SRYKBN (IDX-TBL)
                   WHEN    "830"
                       MOVE    "83"        TO  TBL-SRYKBN (IDX-TBL)
                   WHEN    "840"
                       MOVE    "84"        TO  TBL-SRYKBN (IDX-TBL)
                   WHEN    "850"
                       MOVE    "85"        TO  TBL-SRYKBN (IDX-TBL)
                   WHEN    "890"
                       MOVE    "89"        TO  TBL-SRYKBN (IDX-TBL)
                   WHEN    OTHER
                       MOVE    "80"        TO  TBL-SRYKBN (IDX-TBL)
               END-EVALUATE
           END-IF
           MOVE    WRK-SRYSYUKBN       TO  TBL-SRYSYUKBN(IDX-TBL)
           MOVE    WRK-KAISU           TO  TBL-KAISU (IDX-TBL)
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-S
               MOVE    WRK-TBL-SRYCD (IDX) TO  TBL-SRYCD (IDX-TBL IDX)
               MOVE    WRK-TBL-NAME  (IDX) TO  TBL-NAME  (IDX-TBL IDX)
               MOVE    WRK-TBL-TANINAME(IDX)
                                           TO  TBL-TANINAME(IDX-TBL IDX)
               MOVE    WRK-TBL-SURYO (IDX) TO  TBL-SURYO (IDX-TBL IDX)
               MOVE    WRK-TBL-JIHIMONEY (IDX)
                                           TO  TBL-JIHIMONEY
                                                        (IDX-TBL IDX)
               IF     (WRK-TBL-SRYCD (IDX)(1:4)    =   "0959" )  AND
                      (WRK-SRYSYUKBN           =   "800"  )  AND
                      (TBL-SRYKBN (IDX-TBL)    =   "80"   )
      *            自賠責ユーザコードは、その他とする
                   MOVE    "86"            TO  TBL-SRYKBN (IDX-TBL)
               END-IF
           END-PERFORM
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
                   COMPUTE TBL-ZAITENSU (IDX-TBL)
                                               =   ACCT-ZAITEN
                                               *   -1
               ELSE
                   MOVE    ACCT-ZAITEN     TO  TBL-ZAITENSU(IDX-TBL)
               END-IF
           ELSE
               MOVE    ACCT-ZAITEN     TO  TBL-ZAITENSU(IDX-TBL)
           END-IF
      *
           .
       200231-SRYACCTPLUS-CHK-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       2005-PRINT-SYORI-SEC                SECTION.
      *
      *    診療区分毎にＳＯＲＴ
           PERFORM 20051-TBL-SORT-SEC
      *    帳票テーブル編集
           PERFORM 20052-TBL-HEN-SEC
      *
      *    帳票印刷
           PERFORM 400-PRINT-SYORI-SEC
      *
           .
       2005-PRINT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルＳＯＲＴ処理
      *****************************************************************
       20051-TBL-SORT-SEC             SECTION.
      *
      *
           INITIALIZE                  TBL-SORT-AREA
           MOVE    TBL-DATA-AREA       TO  TBL-SORT-AREA
           INITIALIZE                  TBL-DATA-AREA
           MOVE    ZERO                TO  IDX-TBL2
           MOVE    SPACE               TO  WRK-SRYKBN
           PERFORM VARYING   IDX       FROM   1   BY  1
                   UNTIL     IDX       >   IDX-TBL
               MOVE    "99"            TO  WRK-SRYKBN
               MOVE    ZERO            TO  IDX-HZN
               PERFORM VARYING   IDX1      FROM   1   BY  1
                       UNTIL     IDX1      >   IDX-TBL
                   IF      TBL-SORT-SRYKBN (IDX1)   NOT =   SPACE
                       IF      TBL-SORT-SRYKBN (IDX1)  <   WRK-SRYKBN
                           MOVE    IDX1            TO  IDX-HZN
                           MOVE    TBL-SORT-SRYKBN (IDX1)
                                                   TO  WRK-SRYKBN
                       END-IF
                   END-IF
               END-PERFORM
               IF      IDX-HZN             >   ZERO
                   ADD     1               TO  IDX-TBL2
                   MOVE    TBL-SORT-TBL (IDX-HZN)
                                           TO  TBL-DATA-TBL (IDX-TBL2)
                   INITIALIZE              TBL-SORT-TBL (IDX-HZN)
               END-IF
           END-PERFORM
           MOVE    IDX-TBL2            TO  IDX-TBL
           .
       20051-TBL-SORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票テーブル編集処理
      *****************************************************************
       20052-TBL-HEN-SEC             SECTION.
      *
           INITIALIZE                  TBL-MEISAI-AREA
           MOVE    1                   TO  WRK-PAGE
           MOVE    ZERO                TO  IDX-H
           MOVE    SPACE               TO  WRK-SRYMEI-OLD
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-TBL
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2    >   50  )  OR
                                  (TBL-SRYCD (IDX IDX2)  =   SPACE)
                   ADD     1              TO  IDX-H
                   IF      IDX-H          >=  LINE-MAX
                       PERFORM 200520-PAGE-CNT-SEC
                   END-IF
      *            名称と単位名称チェック
                   IF     (TBL-TANINAME (IDX IDX2)    NOT =   SPACE)
                     AND  (TBL-NAME (IDX IDX2)(71:)   NOT =   SPACE)
      *                最終行の時、改ページ
                       IF      IDX-H          >=  (LINE-MAX - 1 )
                           PERFORM 200520-PAGE-CNT-SEC
                       END-IF
                   END-IF
                   IF     IDX2                =   1
                       PERFORM 200521-SRYSYUMEI-SEC
                       IF      WRK-SRYMEI     NOT  =   WRK-SRYMEI-OLD
                           MOVE    WRK-SRYMEI      TO  TBL-H-SRYKBN
                                                   (WRK-PAGE IDX-H)
                           MOVE    WRK-SRYMEI      TO  WRK-SRYMEI-OLD
                       END-IF
                   END-IF
                   MOVE    TBL-NAME (IDX IDX2)    TO  TBL-H-SRYMEI
                                                   (WRK-PAGE IDX-H)
      *            単位名称
                   IF     (TBL-TANINAME (IDX IDX2)    NOT =   SPACE)
                     AND  (TBL-NAME (IDX IDX2)(71:)   NOT =   SPACE)
                       ADD     1                   TO  IDX-H
                   END-IF
                   MOVE    TBL-TANINAME (IDX IDX2) TO  TBL-H-TANINAME
                                                   (WRK-PAGE IDX-H)
                   IF      TBL-SURYO (IDX IDX2)   >   ZERO
                       MOVE    TBL-SURYO (IDX IDX2)   TO  WRK-SURYO
                       PERFORM 200521-SURYO-HEN-SEC
                       MOVE    WRK-SURYO-X         TO  TBL-H-SURYO
                                                   (WRK-PAGE IDX-H)
                   END-IF
                   IF      TBL-JIHIMONEY (IDX IDX2)   >   ZERO
                       MOVE    TBL-JIHIMONEY(IDX IDX2) TO  WRK-Z119
                       MOVE    WRK-Z119            TO  TBL-H-KINGAKU
                                                      (WRK-PAGE IDX-H)
                   END-IF
               END-PERFORM
               IF      TBL-SRYKBN (IDX)    =   "95"
                                           OR  "96"
                   CONTINUE
               ELSE
      *        金額
                   COMPUTE WRK-KINGAKU    =  (TBL-ZAITENSU (IDX)
                                          *   TBL-KAISU    (IDX) )
                                          *   10
                   IF      WRK-KINGAKU        >   ZERO
                       MOVE    WRK-KINGAKU    TO  WRK-Z119
                       MOVE    WRK-Z119       TO  TBL-H-KINGAKU
                                                      (WRK-PAGE IDX-H)
                   ELSE
                       MOVE    WRK-KINGAKU    TO  WRK-Z119-2
                       MOVE    WRK-Z119-2     TO  TBL-H-KINGAKU
                                                      (WRK-PAGE IDX-H)
                   END-IF
               END-IF
      *        回数
               MOVE    TBL-KAISU (IDX)     TO  WRK-ZZ9
               MOVE    WRK-ZZ9             TO  TBL-H-KAISU
                                                   (WRK-PAGE IDX-H)
      *        剤毎に１行開ける
               ADD     1                   TO  IDX-H
           END-PERFORM
           MOVE    WRK-PAGE            TO  WRK-PAGE-MAX
      *
      *    最終行
           IF      TBL-H-SRYMEI(WRK-PAGE LINE-MAX)
                                       =   SPACE
               ADD     1                   TO  IDX-H
               IF      IDX-H               >   LINE-MAX
                   MOVE    LINE-MAX            TO  IDX-H
               END-IF
               MOVE    "以下余白"          TO  TBL-H-SRYMEI
                                               (WRK-PAGE IDX-H)(30:)
           END-IF
      *
           .
       20052-TBL-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ページカウント処理
      *****************************************************************
       200520-PAGE-CNT-SEC                  SECTION.
      *
      *    最終明細の時、改ページなし
           IF    ( IDX-H               =   LINE-MAX)  AND
                 ( IDX                 =   IDX-TBL )  AND
                 ((IDX2                =   50      )  OR
                  (TBL-SRYCD (IDX IDX2 + 1)    =   SPACE ))
               CONTINUE
           ELSE
               MOVE    LINE-MAX            TO  IDX-H
               MOVE    "次ページへ続く"    TO  TBL-H-SRYMEI
                                               (WRK-PAGE IDX-H)(30:)
               ADD     1                   TO  WRK-PAGE
               MOVE    1                   TO  IDX-H
               MOVE    SPACE               TO  WRK-SRYMEI-OLD
           END-IF
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
           PERFORM VARYING     IDXM   FROM    3  BY  -1
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
      *
      *****************************************************************
      *     診療種別区分名称処理
      *****************************************************************
       200521-SRYSYUMEI-SEC             SECTION.
      *
           EVALUATE    TBL-SRYKBN (IDX)
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
               WHEN    "29"
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
      *    印刷処理
      *****************************************************************
       400-PRINT-SYORI-SEC          SECTION.
      *
      *    各ＤＢ検索
           PERFORM 4001-DB-KENSAKU-SEC
      *
           MOVE    ZERO                TO  WRK-PAGE
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   WRK-PAGE-MAX
      *
      *        帳票印刷開始処理
               PERFORM 400-SYOKI-PRINT-SEC
      *
      *        帳票編集処理
               PERFORM 300-HCM04-HEN-SEC
      *
      *        帳票印刷処理
               PERFORM 400-PRINT-SEC
           END-PERFORM
      *
           .
       400-PRINT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票各ＤＢ検索処理
      *****************************************************************
       4001-DB-KENSAKU-SEC                SECTION.
      *
      *    診療科名編集
           MOVE    SPACE                TO  WRK-SRYKA-MEI
           MOVE    SYU-SRYKA            TO  WRK-SRYKA
           PERFORM 40011-SRYKAMEI-HEN-SEC
      *    保険組合せ
           PERFORM 40012-HKNCOMBI-HEN-SEC
           .
       4001-DB-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       40012-HKNCOMBI-HEN-SEC         SECTION.
      *
      *    保険組合せマスター検索
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-PTID            TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM     TO  COMB-HKNCOMBINUM
           PERFORM 910-HKNCOMBI-READ-SEC
           IF      FLG-HKNCOMBI        =   1
                DISPLAY "*** HKNCOMBI  ERR:"   COMB-KEY "."
           END-IF
      *    労災の公務災害判定
           IF      COMB-HKNNUM         =   "971"
               PERFORM 40013-PTRSIINF-HEN-SEC
           ELSE
               PERFORM 40012-HKNCOMBIMEI-HEN-SEC
           END-IF
      *
           .
       40012-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       40012-HKNCOMBIMEI-HEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBIMEI
      *
           IF    ( SYU-SRYYMD      >=      CONST-H201001 )
            AND  ( COMB-HKNNUM      =      "001" )
                   INSPECT COMB-SYU-TANSEIDONAME
                           REPLACING    ALL    "政管"   BY  "協会"
           END-IF
      *
           STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                   "　"                    DELIMITED  BY  SIZE
                   COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                   "　"                    DELIMITED  BY  SIZE
                   COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                   "　"                    DELIMITED  BY  SIZE
                   COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                   "　"                    DELIMITED  BY  SIZE
                   COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBIMEI
           END-STRING
      *
           .
       40012-HKNCOMBIMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    労災公務災害保険組合せ名称編集処理
      *****************************************************************
       40013-PTRSIINF-HEN-SEC             SECTION.
      *
           INITIALIZE                      PTRSIINF-REC
           MOVE    COMB-HOSPNUM        TO  PTRSI-HOSPNUM
           MOVE    COMB-PTID           TO  PTRSI-PTID
           MOVE    COMB-HKNID          TO  PTRSI-HKNID
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 940-PTRSIINF-READ-SEC
           ELSE
               INITIALIZE                      PTRSIINF-REC
               MOVE    1                   TO  FLG-PTRSIINF
           END-IF
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      PTRSI-HKNKBN        =   "5"
      *        公務災害
               INITIALIZE                      PTHKNINF-REC
               MOVE    COMB-HOSPNUM        TO  PTHKN-HOSPNUM
               MOVE    COMB-PTID           TO  PTHKN-PTID
               MOVE    COMB-HKNID          TO  PTHKN-HKNID
               PERFORM 950-PTHKNINF-READ-SEC
               IF      FLG-PTHKNINF        =   ZERO
                  MOVE    PTHKN-KIGO           TO  WRK-HKNCOMBIMEI
               END-IF
           ELSE
      *        公務災害以外
               PERFORM 40012-HKNCOMBIMEI-HEN-SEC
           END-IF
           .
       40013-PTRSIINF-HEN-EXT.
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
           MOVE    SYU-DENPNUM         TO  WRK-NO
           MOVE    WRK-NO              TO  HCM04-NO
      *    診療科
           MOVE    WRK-SRYKA-MEI(1:20) TO  HCM04-SRYKANAME
      *    患者番号
           MOVE    SPA-PTNUM           TO  HCM04-PTNUM
      *    患者名称
           MOVE    SPA-NAME            TO  HCM04-NAME
      *    保険種別
           MOVE    WRK-HKNCOMBIMEI     TO  HCM04-HOKEN
      *    負担割合
           IF      SYU-PTFTNRATE       >   ZERO
               COMPUTE WRK-WARI        =   SYU-PTFTNRATE   /   10
               IF      SYU-PTFTNRATE(3:1)  =   ZERO
                   MOVE    WRK-WARI            TO  WRK-Z6
                   MOVE    WRK-Z6              TO  HCM04-FUTAN
               ELSE
                   MOVE    WRK-WARI            TO  WRK-Z62
                   MOVE    WRK-Z62             TO  HCM04-FUTAN
               END-IF
           END-IF
      *    診療日
           MOVE    SYU-SRYYMD          TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  HCM04-SRYYMD
      *    発行日
           MOVE    SYU-DENPPRTYMD      TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  HCM04-HAKYMD
      *
      *----(03.04.01)--UPD-START---
      *    医療機関住所
      *    MOVE    SYS-1002-ADRS       TO  HCM04-IRYOADRS
      *    医療機関名
      *    MOVE    SYS-1001-HOSPNAME   TO  HCM04-IRYOMEI
      *    医療機関ＴＥＬ
      **** MOVE    SYS-1002-TEL        TO  HCM04-IRYOTEL
      *    医療機関住所
           MOVE    WRK-HOSPADRS        TO  HCM04-IRYOADRS
      *    医療機関名
           MOVE    WRK-HOSPNAME        TO  HCM04-IRYOMEI
      *    医療機関ＴＥＬ
           MOVE    WRK-HOSPTEL         TO  HCM04-IRYOTEL
      *----(03.04.01)--UPD-END-----
      *
      *    明細内容
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   LINE-MAX
               MOVE    TBL-H-SRYKBN (IDX IDX2) TO  HCM04-SRYKBN (IDX2)
               MOVE    TBL-H-SRYMEI (IDX IDX2) TO  HCM04-SRYMEI (IDX2)
               MOVE    TBL-H-TANINAME(IDX IDX2)
                                               TO  HCM04-TANI  (IDX2)
               MOVE    TBL-H-SURYO  (IDX IDX2) TO  HCM04-SURYO (IDX2)
               MOVE    TBL-H-KAISU  (IDX IDX2) TO  HCM04-KAISU (IDX2)
               MOVE    TBL-H-KINGAKU(IDX IDX2) TO  HCM04-KINGAKU(IDX2)
           END-PERFORM
           .
       300-HCM04-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   複数科・保険入力の印刷処理
      *****************************************************************
       202-KAKHOU-SYUNOU-SEC                SECTION.
      *
      *    収納検索
           PERFORM 2011-SYUNOU-HEN-SEC
      *    複数発行
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
           PERFORM UNTIL    FLG-SYUNOU =   1
      *        伝票番号毎に印刷
               PERFORM 2012-DENPNUM-SYORI-SEC
      *
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key27"             TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           END-PERFORM
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       202-KAKHOU-SYUNOU-EXT.
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
           MOVE    SPA-HOSPNUM         TO  SYS-1900-HOSPNUM
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
           MOVE    SPA-HOSPNUM         TO  SYS-1901-HOSPNUM
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
           MOVE    SPA-HOSPNUM         TO  SYS-1902-HOSPNUM
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
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
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
           .
       910-TENSU-READ-EXT.
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
