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
       PROGRAM-ID.             ORCHCN03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : 入院請求書兼領収書　印刷（ＨＣＭＮ０３）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/10/24    NACL−森脇    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    03.03.13  入院請求書複数枚一括印刷対応
      *  01.00.02    NACL-太田    03.04.28  自費分負担金額に消費税が
      *                                     落ちていたのを修正
      *  01.00.03    NACL-太田    03.07.01  出力先プリンタサブ追加
      *  01.00.04    NACL-太田    03.08.26  労災自賠責編集修正
      *  01.00.05    NACL-太田    03.10.10  食事負担編集修正
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
           SELECT  PRT-FILE        ASSIGN  HC01PARA
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL 
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
       01  PRT-REC.
           03  PRT-PRTID               PIC X(30).
           03  PRT-PRTNM               PIC X(20).
           03  PRT-SITEKBN             PIC X(01).
           03  PRT-PRTDATA             PIC X(10000).
      *
       WORKING-STORAGE             SECTION.
      *
      *     印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *
      *    シェル用領域
      *    COPY    "CPCOMMONSHELL.INC".
           COPY    "CPSHELLTBL.INC".
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-SYUNOU2         PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
      *
           03  FLG-SP              PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                 PIC 9(04).
           03  IDX-M                PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    一時領域
       01  WRK-AREA.
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
           03  WRK-HKNCOMBI        PIC X(40).
      *
           03  WRK-GOKTEN          PIC S9(11).
      *
           03  WRK-DENPNUM         PIC 9(07).
      *
           03  WRK-WARI            PIC 9(02)V9.
           03  WRK-Z6              PIC ZZZZZZ.
           03  WRK-Z62             PIC ZZZ9.9.
      *
           03  WRK-ZZZ             PIC ZZZ.
           03  WRK-NO              PIC Z(7).
           03  WRK-Z7-G.
               05  WRK-FIL             PIC X(09).
               05  WRK-Z7              PIC Z,ZZZ,ZZZ.
           03  WRK-Z72-G.
               05  WRK-FIL2            PIC X(09).
               05  WRK-Z72             PIC -,---,---.
      *
           03  WRK-ZHENSYU-G.
               05  WRK-ZHENSYU9        PIC X(09).
               05  WRK-ZHENSYU18       PIC X(18).
      *    自費集計
           03  WRK-JIHI-REC.
               05  WRK-JIHI            PIC S9(07)   OCCURS  5.
           03  WRK-JIHI-TOTAL          PIC S9(08).
      *
      *    文字変換
           03  WRK-ZENKAKU-G.
               05  WRK-ZENKAKU         PIC X(02)   OCCURS   10.
           03  WRK-MOJI                PIC X(10).
           03  WRK-MOJISU              PIC 9(02).
           03  WRK-PRTNM               PIC X(20).
           03  WRK-ROOM                PIC X(50).
           03  WRK-BRMNUM-NAME         PIC X(50).
           03  WRK-BRMNUM-IN           PIC X(06).
           03  WRK-BRMNUM-EDT          PIC X(06).
           03  WRK-BRMNUM-OT           PIC X(06).
           03  WRK-STR1                PIC X(20).
           03  WRK-STR2                PIC X(20).
           03  WRK-IDX1                PIC 9(05).
           03  WRK-IDX2                PIC 9(05).
           03  WRK-IDX3                PIC 9(05).
           03  WRK-SKYMISYUPRTKBN      PIC X(01).
           03  WRK-GAI-MISYU           PIC S9(09).
           03  WRK-NYU-MISYU           PIC S9(09).
           03  WRK-SKYMONEY-SKJ        PIC S9(09).
      *
      *   日本語数字
       01  TBL-SUJI-AREA.
           03  FILLER          PIC X(20)   VALUE
              "１２３４５６７８９０".
       01  TBL-SUJI-R          REDEFINES   TBL-SUJI-AREA.
           03  TBL-SUJI        PIC X(02)   OCCURS   10.
      *
      *    帳票カウント
       01  PRT-CNT                     PIC 9(10)   VALUE   ZERO.
      *
      *
      *    未収編集用ワーク
       01  MISYU-REC.
           COPY    "CPSYUNOU.INC"      REPLACING   //SYU-//
                                       BY          //MISYU-//.
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    医療機関情報−基本情報 
           COPY    "CPSK1001.INC".
      *    医療機関情報−所在地、連絡先 
           COPY    "CPSK1002.INC".
      *    診療科目情報
           COPY    "CPSK1005.INC".
      *    請求書自費名称情報
           COPY    "CPSK1013.INC".
      *    医療機関基本情報
           COPY    "CPSK5000.INC".
      *    病棟情報
           COPY    "CPSK5001.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1034.INC".
      *
      *    労災医療機関情報
           COPY    "CPSK4001.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    保険マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
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
      *
      *    入院請求書
           COPY "HCN03.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
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
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
           COPY    "ORCA-DBPATH".
      *
           COPY    "MCPAREA".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    請求書兼領収書共通パラメタ
           COPY    "CPORCHCN03.INC".
       PROCEDURE                   DIVISION    USING
           ORCHCN03AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       STS-AREA
                                       WRK-AREA
                                       IDX-AREA
      *
           ACCEPT  SYS-TIME            FROM    TIME
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
           PERFORM VARYING IDX-M   FROM    1   BY  1
                   UNTIL ( IDX-M   >   ORCHCN03-CNT )
      *
      *        主処理
               PERFORM 200-MAIN-SEC
      *
           END-PERFORM
      *
      *    帳票印刷処理
           PERFORM 300-PRINT-SEC
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    ZERO                TO  SYS-1001-STYUKYMD
           MOVE    ALL "9"             TO  SYS-1001-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           MOVE    ORCHCN03-SYSYMD     TO  ORC-DBYMD
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1001-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1002-REC
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
           MOVE    ZERO                TO  SYS-1002-STYUKYMD
           MOVE    ALL "9"             TO  SYS-1002-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           MOVE    ORCHCN03-SYSYMD     TO  ORC-DBYMD
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"  TO  ORC-DBPATH
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC TO  SYS-1002-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1002-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    出力先プリンタ名割り当て情報編集
           MOVE    SPACE               TO  SPA-AREA
           INITIALIZE                      SPA-AREA
      *
           MOVE    ORCHCN03-HEAD-HOSPID
                                       TO  SPA-HOSPID
           MOVE    ORCHCN03-STAFFCD    TO  SPA-STAFFCD
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    ORCHCN03-SYSYMD     TO  ORCSPRTNM-SRYYMD
           MOVE    "1"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1034-REC
      *
           MOVE    SYS-1034-PRTNM(3)
                                       TO  WRK-PRTNM
      *
      *    労災医療機関情報編集
           MOVE    SPACE               TO  SYS-4001-REC
           INITIALIZE                  SYS-4001-REC
           MOVE    "4001"              TO  SYS-4001-KANRICD
           MOVE    "*"                 TO  SYS-4001-KBNCD
           MOVE    SYS-4001-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           MOVE    ORCHCN03-SYSYMD     TO  ORC-DBYMD
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-4001-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-4001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    医療機関基本情報
           MOVE    SPACE               TO  SYS-5000-REC
           INITIALIZE                  SYS-5000-REC
           MOVE    "5000"              TO  SYS-5000-KANRICD
           MOVE    "*"                 TO  SYS-5000-KBNCD
           MOVE    SYS-5000-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           MOVE    ORCHCN03-SYSYMD     TO  ORC-DBYMD
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-5000-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-5000-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           MOVE    SYS-5000-SKYMISYUPRTKBN
                                       TO  WRK-SKYMISYUPRTKBN
      *
      *    ファイルＯＰＥＮ
           MOVE    "HCN3"              TO  HC01PARA-FORM-ID
           MOVE    ORCHCN03-TERMID     TO  HC01PARA-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
           MOVE    1                   TO  HC01PARA-CNT
           OPEN    OUTPUT               PRT-FILE
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                      SECTION.
      *
      *
      *    患者マスター検索
           INITIALIZE                      PTINF-REC
           MOVE    ORCHCN03-HOSPID (IDX-M)
                                       TO  PTINF-HOSPID
           MOVE    ORCHCN03-PTID   (IDX-M)
                                       TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF    ( FLG-PTINF           =   1 )
               DISPLAY "*** PTINF  INV :"   PTINF-KEY
               GO      TO      200-MAIN-EXT
           END-IF

      *    収納マスタ検索処理
           PERFORM 2001-SYUNOU-SEL-SEC
      *
      *    入院請求書編集処理
           PERFORM 2001-HCN03-HEN-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスタ検索処理
      *****************************************************************
       2001-SYUNOU-SEL-SEC             SECTION.
      *
      *    収納マスタ検索
           INITIALIZE                  SYUNOU-REC
           MOVE    ORCHCN03-HOSPID (IDX-M)
                                       TO  SYU-HOSPID
           MOVE    ORCHCN03-NYUGAIKBN (IDX-M)
                                       TO  SYU-NYUGAIKBN
           MOVE    ORCHCN03-PTID (IDX-M)
                                       TO  SYU-PTID
           MOVE    ORCHCN03-DENPNUM (IDX-M)
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
               MOVE    "SYUNOU-KEY"    TO  ORC-DBPATH
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               INITIALIZE              SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       2001-SYUNOU-SEL-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       2001-HCN03-HEN-SEC              SECTION.
      *
           INITIALIZE                  HCN03
      *
      *    伝票番号
           MOVE    SYU-DENPNUM         TO  WRK-NO
           MOVE    WRK-NO              TO  HCN03-NO
      *
      *    診療科名検索処理
           PERFORM 3102-SRYKA-SEL-SEC
      *
      *    診療科
           MOVE    SYS-1005-SRYKANAME2 TO  HCN03-MIDASI
      *    患者番号
           MOVE    ORCHCN03-PTNUM (IDX-M)
                                       TO  HCN03-KNJNO
      *    患者名称
           MOVE    PTINF-NAME
                                       TO  HCN03-KNJNAMW
      *
      *    病棟・病室名編集処理
           PERFORM 3102-ROOM-HEN-SEC
           MOVE    WRK-ROOM            TO  HCN03-ROOM
      *
      *    保険組合せ名称編集処理
           PERFORM 3102-HKNCOMBI-HEN-SEC
      *
           IF    ( COMB-HKNNUM         =   "971" OR "973" )
      *
      *        患者労災保険情報検索
               INITIALIZE                      PTRSIINF-REC
               MOVE    ORCHCN03-HOSPID(IDX-M)
                                           TO  PTRSI-HOSPID
               MOVE    ORCHCN03-PTID  (IDX-M)
                                           TO  PTRSI-PTID
               MOVE    COMB-HKNID          TO  PTRSI-HKNID
               PERFORM 910-PTRSIINF-READ-SEC
           ELSE
               INITIALIZE                      PTRSIINF-REC
           END-IF
      *
      *    保険種別
           MOVE    WRK-HKNCOMBI        TO  HCN03-HOKEN
      *    負担割合
           IF      SYU-PTFTNRATE       >   ZERO
               COMPUTE WRK-WARI        =   SYU-PTFTNRATE   /   10
               IF      SYU-PTFTNRATE(3:1)  =   ZERO
                   MOVE    WRK-WARI            TO  WRK-Z6
                   MOVE    WRK-Z6              TO  HCN03-FUTAN
               ELSE
                   MOVE    WRK-WARI            TO  WRK-Z62
                   MOVE    WRK-Z62             TO  HCN03-FUTAN
               END-IF
           END-IF
      *    請求開始日
           MOVE    SYU-SKYSTYMD        TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  HCN03-NYUINSTRYMD
      *    請求終了日
           MOVE    SYU-SKYEDYMD        TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  HCN03-NYUINENDYMD
      *    発行日
           MOVE    SYU-DENPPRTYMD      TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  HCN03-HAKYMD
      *
      *    医療機関住所
           MOVE    SYS-1002-ADRS       TO  HCN03-IRYOADRS
      *    医療機関名
           MOVE    SYS-1001-HOSPNAME   TO  HCN03-IRYOMEI
      *    医療機関ＴＥＬ
           MOVE    SYS-1002-TEL        TO  HCN03-IRYOTEL
      *    自費名称編集
           PERFORM 3102-JIHIMSG-HEN-SEC
      *
           MOVE    SPACE               TO  WRK-Z7-G
           MOVE    SPACE               TO  WRK-Z72-G
           MOVE    SPACE               TO  WRK-ZHENSYU-G
      *
      *    請求内容
           PERFORM VARYING     IDX1    FROM  1   BY  1
                   UNTIL       IDX1    >     12
               MOVE    SYU-HKNTEN(IDX1)     TO  WRK-Z7
               MOVE    WRK-Z7              TO  WRK-ZHENSYU9
               PERFORM 800-HENKAN-SEC
               MOVE    WRK-ZHENSYU18       TO  HCN03-HKNRYO(IDX1)
           END-PERFORM
      *
      *    労災・自賠の時、点数表示なし
           IF  COMB-HKNNUM     =    "971" OR "973"
               PERFORM VARYING     IDX1    FROM  1   BY  1
                       UNTIL       IDX1    >     11
                   MOVE    SPACE           TO  HCN03-HKNRYO(IDX1)
               END-PERFORM
               MOVE    ALL "*"             TO  HCN03-HKNRYO(12)
           END-IF
      *
      *    保険適用分
           MOVE    SYU-HKNTEKMONEY         TO  WRK-Z7
           MOVE    WRK-Z7                  TO  WRK-ZHENSYU9
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-ZHENSYU18           TO  HCN03-HOKENBUN
      *
      *    労災・自賠の時、点数表示なし
           IF    ( COMB-HKNNUM             =    "971"
                                           OR   "973" )
               EVALUATE    TRUE
               WHEN      ( COMB-HKNNUM             =    "971" )
                   AND   ( SYS-4001-RSIIRYOKBN NOT =   "1" )
      *
               WHEN      ( COMB-HKNNUM             =    "973" )
                   AND   ( PTRSI-HKNKBN        =    "4" ) 
                   AND   ( SYU-PTFTNRATE   NOT =   ZERO  )
                       COMPUTE WRK-GOKTEN  =    SYU-HKNTEKMONEY
                                           +    SYU-RSISHOSHIN-MONEY
                                           +    SYU-RSISAISHIN-MONEY
                                           +    SYU-RSISDO-MONEY
                                           +    SYU-RSIETC-MONEY
                       MOVE    WRK-GOKTEN      TO  WRK-Z7
                       MOVE    WRK-Z7          TO  WRK-ZHENSYU9
                       PERFORM 800-HENKAN-SEC
                       MOVE    WRK-ZHENSYU18   TO  HCN03-HOKENBUN
               WHEN    OTHER
                   MOVE    SPACE           TO  HCN03-HOKENBUN
               END-EVALUATE
           END-IF
      *
      *    保険適用外分
           COMPUTE WRK-GOKTEN
               =   SYU-TGMONEY (12)
               +   SYU-TGMONEY-TAX (12)
      *
           MOVE    WRK-GOKTEN          TO  WRK-Z72
           MOVE    WRK-Z72             TO  WRK-ZHENSYU9
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-ZHENSYU18       TO  HCN03-HOKENGAI
      *    自費
           MOVE    ZERO                TO  WRK-JIHI-REC
           PERFORM VARYING     IDX1     FROM    1   BY  1
                   UNTIL       IDX1     >   5
               COMPUTE WRK-JIHI (IDX1)     =   SYU-JIHI-TAXNASI (IDX1)
                                           +   SYU-JIHI-TAXARI  (IDX1)
               MOVE    WRK-JIHI (IDX1)     TO  WRK-Z72
               MOVE    WRK-Z72             TO  WRK-ZHENSYU9
               PERFORM 800-HENKAN-SEC
               MOVE    WRK-ZHENSYU18       TO  HCN03-JIHI(IDX1)
           END-PERFORM
      *
      *    自費計
           COMPUTE WRK-JIHI-TOTAL      =   SYU-JIHI-TOTAL
                                       +   SYU-JIHI-TOTAL-TAX
           MOVE    WRK-JIHI-TOTAL      TO  WRK-Z7
           MOVE    WRK-Z7              TO  WRK-ZHENSYU9
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-ZHENSYU18       TO  HCN03-JIHI(6)
      *    自費消費税
           MOVE    SYU-JIHI-TAX        TO  WRK-Z7
           MOVE    WRK-Z7              TO  WRK-ZHENSYU9
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-ZHENSYU18       TO  HCN03-JIHI(7)
      *    自費合計
           COMPUTE WRK-GOKTEN          =   WRK-JIHI-TOTAL
                                       +   SYU-JIHI-TAX
           MOVE    WRK-GOKTEN          TO  WRK-Z7
           MOVE    WRK-Z7              TO  WRK-ZHENSYU9
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-ZHENSYU18       TO  HCN03-JIHI(8)
      *    食事負担額
           COMPUTE WRK-SKYMONEY-SKJ
               =   SYU-SKYMONEY-SKJ-KEI
               +   SYU-SKYMONEY-SKJ-JIHI-KEI
           MOVE    WRK-SKYMONEY-SKJ    TO  WRK-Z7
           MOVE    WRK-Z7              TO  WRK-ZHENSYU9
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-ZHENSYU18       TO  HCN03-SYOKUJI
      *    老人一部負担金
           MOVE    SYU-RJN-FTN         TO  WRK-Z7
           MOVE    WRK-Z7              TO  WRK-ZHENSYU9
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-ZHENSYU18       TO  HCN03-ROUJIN
      *    公費一部負担金
           MOVE    SYU-KOH-FTN         TO  WRK-Z7
           MOVE    WRK-Z7              TO  WRK-ZHENSYU9
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-ZHENSYU18       TO  HCN03-KOUHI
      *    室料差額
           MOVE    SYU-RMSAGAKU        TO  WRK-Z7
           MOVE    WRK-Z7              TO  WRK-ZHENSYU9
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-ZHENSYU18       TO  HCN03-RMSAGAKU
      *    調整金
           MOVE    SYU-CHOSEI          TO  WRK-Z72
           MOVE    WRK-Z72             TO  WRK-ZHENSYU9
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-ZHENSYU18       TO  HCN03-CHOSEI
      *    今回請求額
           MOVE    ORCHCN03-SEIKYU (IDX-M)
                                       TO  WRK-Z72
           MOVE    WRK-Z72             TO  WRK-ZHENSYU9
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-ZHENSYU18       TO  HCN03-KONSEIKYU
      *    今回入金額
           MOVE    ORCHCN03-NYUKIN (IDX-M)
                                       TO  WRK-Z7
           MOVE    WRK-Z7              TO  WRK-ZHENSYU9
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-ZHENSYU18       TO  HCN03-KONNYUKIN
      *
           IF    ( WRK-SKYMISYUPRTKBN  =   "1" )
      *        未収額編集処理
               PERFORM 3102-MISYU-EDIT-SEC
               MOVE    "外　来　未　収　額"
                                       TO  HCN03-LBL-G-MISYU
               MOVE    "円"            TO  HCN03-LBL-G-MISYU-EN
               MOVE    WRK-GAI-MISYU   TO  WRK-Z72
               MOVE    WRK-Z72         TO  WRK-ZHENSYU9
               PERFORM 800-HENKAN-SEC
               MOVE    WRK-ZHENSYU18   TO  HCN03-GAI-MISYUKIN
      *
               MOVE    "入　院　未　収　額"
                                       TO  HCN03-LBL-N-MISYU
               MOVE    "円"            TO  HCN03-LBL-N-MISYU-EN
               MOVE    WRK-NYU-MISYU   TO  WRK-Z72
               MOVE    WRK-Z72         TO  WRK-ZHENSYU9
               PERFORM 800-HENKAN-SEC
               MOVE    WRK-ZHENSYU18   TO  HCN03-NYU-MISYUKIN
           END-IF
      *
      *    印刷用データ出力
           MOVE    SPACE               TO  PRT-REC
           MOVE    "HCN03.red"         TO  PRT-PRTID
           MOVE    WRK-PRTNM           TO  PRT-PRTNM
           MOVE    "1"                 TO  PRT-SITEKBN
           MOVE    HCN03               TO  PRT-PRTDATA
           WRITE   PRT-REC
      *
           .
       2001-HCN03-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角全角変換処理
      *****************************************************************
       800-HENKAN-SEC             SECTION.
     *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-ZHENSYU9        TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT(1:18)    TO  WRK-ZHENSYU18
           END-IF
           .
       800-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科検索処理
      *****************************************************************
       3102-SRYKA-SEL-SEC              SECTION.
      *
      *    診療科
           MOVE    SPACE               TO  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SYU-SRYKA           TO  SYS-1005-KBNCD
           MOVE    ZERO                TO  SYS-1005-STYUKYMD
           MOVE    ALL "9"             TO  SYS-1005-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           MOVE    ORCHCN03-KJNYMD (IDX-M)
                                       TO  ORC-DBYMD
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1005-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       3102-SRYKA-SEL-EXT.
           EXIT.
      *****************************************************************
      *    病棟・病室名編集処理
      *****************************************************************
       3102-ROOM-HEN-SEC                 SECTION.
      *
      *    病棟情報検索処理
           PERFORM 3102-SYS-5001-SEL-SEC
      *
      *    病室名編集処理
           MOVE    SYU-ROOMNUM (3 : )
                                       TO  WRK-BRMNUM-IN
           PERFORM 3102-BRMNUM-EDIT-SEC
      *
           MOVE    SPACE               TO  WRK-ROOM
      *
           IF    ( WRK-BRMNUM-NAME     NOT =   SPACE )
               STRING  SYS-5001-BTU-NAME   DELIMITED   BY  SPACE
                       "　"                DELIMITED   BY  SIZE
                       WRK-BRMNUM-NAME     DELIMITED   BY  SPACE
                       "号室"              DELIMITED   BY  SIZE
               INTO    WRK-ROOM
               END-STRING
           ELSE
               STRING  SYS-5001-BTU-NAME   DELIMITED   BY  SPACE
                       "　　　　"          DELIMITED   BY  SIZE
                       "号室"              DELIMITED   BY  SIZE
               INTO    WRK-ROOM
               END-STRING
           END-IF
      *
           .
       3102-ROOM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    病棟情報検索処理
      *****************************************************************
       3102-SYS-5001-SEL-SEC             SECTION.
      *
      *    病棟情報検索
           MOVE    SPACE               TO  SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    SYU-BYOTONUM        TO  SYS-5001-KBNCD
      *    システム管理検索
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           MOVE    ORCHCN03-KJNYMD (IDX-M)
                                       TO  ORC-DBYMD
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-5001-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-5001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           .
       3102-SYS-5001-SEL-EXT.
           EXIT.
      *****************************************************************
      *    病室番号編集処理
      *****************************************************************
       3102-BRMNUM-EDIT-SEC            SECTION.
      *
      *    病室番号左詰め処理
           PERFORM 3102-BRM-NUM-LEFT-EDIT-SEC
      *
      *    全角編集処理
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-BRMNUM-OT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT
                                       TO  WRK-BRMNUM-NAME
           END-IF
      *
           .
       3102-BRMNUM-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病室番号左詰処理
      *****************************************************************
       3102-BRM-NUM-LEFT-EDIT-SEC      SECTION.
      *
           MOVE    SPACE               TO  WRK-BRMNUM-OT
      *
           IF    ( WRK-BRMNUM-IN       =   SPACE )
               GO  TO  3102-BRMNUM-LEFT-EDIT-EXT
           END-IF
      *
           IF    ( WRK-BRMNUM-IN (1 : 1)  =   SPACE )
               MOVE    SPACE           TO  WRK-STR1
               MOVE    1               TO  WRK-IDX1
               UNSTRING    WRK-BRMNUM-IN
                   DELIMITED   BY  ALL SPACE
                   INTO    WRK-STR1
                   WITH    POINTER WRK-IDX1
               END-UNSTRING
               MOVE    WRK-BRMNUM-IN (WRK-IDX1 : )
                                       TO  WRK-BRMNUM-EDT
           ELSE
               MOVE    WRK-BRMNUM-IN  TO  WRK-BRMNUM-EDT
           END-IF
      *
           PERFORM VARYING WRK-IDX1    FROM    6   BY  -1
               UNTIL     ( WRK-IDX1    <   1 )
                 OR      ( WRK-BRMNUM-EDT (WRK-IDX1 : 1)
                                   NOT =   SPACE )
               CONTINUE
           END-PERFORM
      *
           IF    ( WRK-IDX1    <   1 )
               MOVE    WRK-BRMNUM-EDT TO  WRK-BRMNUM-OT
               GO  TO  3102-BRMNUM-LEFT-EDIT-EXT
           END-IF
      *
      *    病室番号が数字の場合前ゼロをとる
           IF    ( WRK-BRMNUM-EDT ( 1 : WRK-IDX1 )
                                       IS  NUMERIC )
               IF    ( WRK-BRMNUM-EDT ( 1 : 1 )
                                       =   ZERO )
                   MOVE    1               TO  WRK-IDX2
                   MOVE    SPACE           TO  WRK-STR1
                   UNSTRING    WRK-BRMNUM-EDT
                       DELIMITED BY  ALL ZERO
                   INTO    WRK-STR1
                   WITH    POINTER WRK-IDX2
                   END-UNSTRING
                   MOVE    WRK-BRMNUM-EDT (WRK-IDX2 : )
                                           TO  WRK-BRMNUM-OT
               ELSE
                   MOVE    WRK-BRMNUM-EDT TO  WRK-BRMNUM-OT
               END-IF
           ELSE
                   MOVE    WRK-BRMNUM-EDT TO  WRK-BRMNUM-OT
           END-IF
      *
           .
       3102-BRMNUM-LEFT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       3102-HKNCOMBI-HEN-SEC             SECTION.
      *
      *    保険組合せマスター検索
           INITIALIZE                      HKNCOMBI-REC
           MOVE    ORCHCN03-HOSPID (IDX-M)
                                       TO  COMB-HOSPID
           MOVE    ORCHCN03-PTID   (IDX-M)
                                       TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM     TO  COMB-HKNCOMBINUM
           PERFORM 910-HKNCOMBI-READ-SEC
           IF      FLG-HKNCOMBI        =   1
                DISPLAY "*** HKNCOMBI  ERR:"   COMB-KEY "."
                INITIALIZE             HKNCOMBI-REC
           END-IF
      *
           MOVE    SPACE               TO  WRK-HKNCOMBI
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
                         INTO              WRK-HKNCOMBI
           END-STRING
      *
           .
       3102-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    自費名称編集処理
      *****************************************************************
       3102-JIHIMSG-HEN-SEC             SECTION.
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    ZERO                TO  SYS-1013-STYUKYMD
           MOVE    ALL "9"             TO  SYS-1013-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           MOVE    ORCHCN03-KJNYMD (IDX-M)
                                       TO  ORC-DBYMD
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY2"      TO  ORC-DBPATH
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1013-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1013-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM UNTIL       FLG-SYSKANRI    =   1
               IF      ORCHCN03-NYUGAIKBN (IDX-M)
                                       =   SYS-1013-KBNCD(1:1)
                   EVALUATE    SYS-1013-KBNCD(2:1)
                       WHEN    "1"
                           MOVE    SYS-1013-JIHINAME  TO
                                                      HCN03-JIHIMSG (1)
                       WHEN    "2"
                           MOVE    SYS-1013-JIHINAME  TO
                                                      HCN03-JIHIMSG (2)
                       WHEN    "3"
                           MOVE    SYS-1013-JIHINAME  TO
                                                      HCN03-JIHIMSG (3)
                       WHEN    "4"
                           MOVE    SYS-1013-JIHINAME  TO
                                                      HCN03-JIHIMSG (4)
                       WHEN    "5"
                           MOVE    SYS-1013-JIHINAME  TO
                                                      HCN03-JIHIMSG (5)
                   END-EVALUATE
               END-IF
      *
               MOVE    "SYSKANRI-KEY2"      TO  ORC-DBPATH
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1013-REC
               END-IF
           END-PERFORM
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       3102-JIHIMSG-HEN-EXT.
           EXIT.
      *****************************************************************
      *    未収額編集処理
      *****************************************************************
       3102-MISYU-EDIT-SEC         SECTION.
      *
           MOVE    ZERO            TO  WRK-NYU-MISYU
                                       WRK-GAI-MISYU
      *
           MOVE    ZERO            TO  FLG-SYUNOU2
      *
      *    収納マスタ検索
           PERFORM 31021-SYUNOU-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUNOU2 NOT =   ZERO )
      *
               EVALUATE    TRUE
               WHEN    ( MISYU-NYUGAIKBN   =   SYU-NYUGAIKBN )
                   AND ( MISYU-DENPNUM     =   SYU-DENPNUM   )
                   CONTINUE
      *
               WHEN    OTHER
      *
                   IF    ( MISYU-NYUGAIKBN     =   "1" )
                       COMPUTE WRK-NYU-MISYU
                           =   WRK-NYU-MISYU
                           + ( MISYU-SKYMONEY  -   MISYU-NYUKIN-TOTAL )
                   END-IF
      *
                   IF    ( MISYU-NYUGAIKBN     =   "2" )
                       COMPUTE WRK-GAI-MISYU
                           =   WRK-GAI-MISYU
                           + ( MISYU-SKYMONEY  -   MISYU-NYUKIN-TOTAL )
                   END-IF
      *
               END-EVALUATE
      *
      *        収納マスタ読込
               PERFORM 31021-SYUNOU-READ-SEC
      *
           END-PERFORM
      *
           MOVE    "SYUNOU-KEY6"   TO  ORC-DBPATH
           MOVE    "DBCLOSE"       TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       3102-MISYU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    収納マスタ検索処理
      *****************************************************************
       31021-SYUNOU-SEL-SEC             SECTION.
      *
           INITIALIZE              MISYU-REC
           MOVE    SYU-HOSPID      TO  MISYU-HOSPID
           MOVE    SYU-PTID        TO  MISYU-PTID
           MOVE    "%"             TO  MISYU-SRYYMD
           MOVE    MISYU-REC       TO  MCPDATA-REC
           MOVE    "SYUNOU-KEY6"   TO  ORC-DBPATH
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1           TO  FLG-SYUNOU2
           END-IF
      *
           .
       31021-SYUNOU-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納マスタ読込処理
      *****************************************************************
       31021-SYUNOU-READ-SEC           SECTION.
      *
           MOVE    "SYUNOU-KEY6"   TO  ORC-DBPATH
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1           TO  FLG-SYUNOU2
           ELSE
               MOVE    MCPDATA-REC TO  MISYU-REC
           END-IF
      *
           .
       31021-SYUNOU-READ-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       300-PRINT-SEC                SECTION.
      *
           CLOSE   PRT-FILE
      *
           MOVE    SPACE           TO  SHELLTBL
      *    ファイルデイレクトリ位置指定
           INITIALIZE              ORCSMKPASSAREA
           MOVE    "scripts/daily/HCALL2.sh"
                                   TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"    USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01    TO  SHELLTBL-NAME
      *
           MOVE    HC01PARA        TO  SHELLTBL-ARG1
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    PATH-SHELL-SHELL
                                   TO  MCP-PATH
           CALL    "MCPSUB"        USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
       300-PRINT-EXT.
           EXIT.
      *
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
           .
       31012-SEIWA-HEN-EXT.
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
      *    患者マスター読み込み
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
                   MOVE    MCPDATA-REC     TO  PTINF-REC
                   MOVE    ZERO            TO  FLG-PTINF
               ELSE
                   MOVE    1               TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       900-PTINF-READ-EXT.
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
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
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
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報読み込み
      *****************************************************************
       910-PTRSIINF-READ-SEC         SECTION.
      *
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTRSIINF-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "PTRSIINF-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTRSIINF-REC
                   MOVE    ZERO            TO  FLG-PTRSIINF
               ELSE
                   MOVE    1               TO  FLG-PTRSIINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
     *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTRSIINF-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       910-PTRSIINF-READ-EXT.
           EXIT.
