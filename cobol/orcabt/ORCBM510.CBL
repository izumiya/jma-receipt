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
       PROGRAM-ID.             ORCBM510.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 症状詳記
      *  管理者            :
      *  作成日付    作業者        記述
      *  03/08/12    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev  修正者       日付      内容
      *  01.01.01     NACL-森脇    03/10/10  公費情報読み込み修正
      *  01.01.02     NACL-森脇    04/06/10  月代わり受給者番号
      *  01.01.03     NACL-森脇    04/08/05  修正
      *  01.01.04     NACL-森脇    04/09/09  対象日付修正
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01PARA//
                                       BY         //RECEERR//.
           03  FILLER                  PIC X(04)   VALUE   ".dat".
      *
           COPY    "HCM37.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-RECEDEN             PIC 9(01).
           03  FLG-RECE10              PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTKOH               PIC 9(01).
           03  FLG-MONTHLYNUM          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE                PIC 9(03).
           03  CNT-LINE                PIC 9(02).
           03  CNT-RECEDEN             PIC 9(06).
           03  CNT-RECE10              PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           05  WRK-PARA-TEISYUTUSAKI   PIC X(01).
           05  WRK-PARA-JOBID          PIC 9(07).
           05  WRK-PARA-SHELLID        PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
      *
           03  WRK-PATH                PIC X(64).
           03  WRK-KOHNUM              PIC X(03).
      *
           03  WRK-SYMD                PIC 9(08).
           03  WRK-HENYMDG             PIC X(22).
           03  WRK-SRYYMWH             PIC X(16).
      *
           03  WRK-ZZ9                 PIC ZZ9.
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "ORCA-DBPATH".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    診療科情報
      *     COPY    "CPSK1005.INC".
      *
      *    請求管理情報
           COPY    "CPRCF010.INC".
      *
      *    レセプト電算情報
           COPY    "CPRECEDEN.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者公費情報
       01  PTKOH-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    月代わり受給者番号
       01  MONTHLYNUM-REC.
           COPY    "CPMONTHLYNUM.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷用データ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(256).
      *
      ******************************************************************
      *
       PROCEDURE               DIVISION
                                   USING
                                   COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           DISPLAY "*** ORCBM510 START ***"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  IDX-AREA
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
                                               LNK-PRTKANRI-TBL-KEY
                                               LNK-PRTKANRI-TBL-GROUP
                                               LNK-PRTKANRI-SHORI-RENNUM
                                               LNK-PRTKANRI-SRYYM
                                               LNK-PRTKANRI-SKYYMD
                                               LNK-PRTKANRI-SHELLID
                                               LNK-PRTKANRI-PRIORITY
                                               LNK-PRTKANRI-TERMID
                                               LNK-PRTKANRI-OPID
                                               LNK-PRTKANRI-PRTNM
                                               WRK-PARA-TEISYUTUSAKI
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
           END-UNSTRING
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCBM510"          TO  JOB-PGID
           IF      WRK-PARA-TEISYUTUSAKI   =   "1"
               MOVE    "症状詳記（社保）"
                                       TO  JOB-SHELLMSG
           ELSE
               MOVE    "症状詳記（国保）"
                                       TO  JOB-SHELLMSG
           END-IF
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
      *
      *    システム管理基本情報読込
           PERFORM 900-SYS1001-READ-SEC
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECEERR-TERMID
      *
      *    診療年月
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE     WRK-HENYMDG(1:16)  TO  WRK-SRYYMWH
      *
           IF      WRK-PARA-TEISYUTUSAKI
                                       =   "1"
               MOVE    "SEIKYU-KEY23"      TO  WRK-PATH
           ELSE  
               MOVE    "SEIKYU-KEY24"      TO  WRK-PATH
           END-IF
      *
      *    請求管理読み込み
           PERFORM 900-SEIKYU-SELECT-SEC
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
           PERFORM UNTIL   FLG-END     =   1
      *        レセプト電算情報読み込み
               PERFORM 900-RECEDEN-SELECT-SEC
      *
               PERFORM UNTIL   FLG-RECEDEN =   1
      *            帳票編集<ヘッダー部>処理
                   PERFORM 210-HEAD-HEN-SEC
      *
                   PERFORM VARYING CNT-LINE    FROM    1   BY  1
                           UNTIL ( FLG-RECEDEN =   1 )
                           OR    ( CNT-LINE    >   32 )
                       MOVE   RECEDEN-RECEDATA
                                       TO  HCM37-NAIYO(CNT-LINE)
      *                レセプト電算情報読み込み
                       PERFORM 900-RECEDEN-READ-SEC
                   END-PERFORM
      *
      *            帳票印刷処理
                   PERFORM 400-PRINT-OUT-SEC
               END-PERFORM
      *
               MOVE    "DBCLOSE"           TO  MCP-FUNC
               MOVE    "RECEDEN-KEY2"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
               MOVE    WRK-PATH            TO  ORC-DBPATH
      *        請求管理読み込み
               PERFORM 900-SEIKYU-READ-SEC
           END-PERFORM
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       210-HEAD-HEN-SEC          SECTION.
      *
           INITIALIZE                      HCM37
      *
           ADD     1                   TO  CNT-PAGE
           MOVE    CNT-PAGE            TO  WRK-ZZ9
           MOVE    WRK-ZZ9             TO  HCM37-PAGE
      *
      *    保険種別１
           EVALUATE    RECE10-RECESYUBETU (2:1)
           WHEN    "1"
               EVALUATE    RECE10-TEISYUTUSAKI
               WHEN    1
                   MOVE    "１社"      TO  HCM37-HKNSBT1
               WHEN    2
                   MOVE    "１国"      TO  HCM37-HKNSBT1
               END-EVALUATE
           WHEN    "2"
               MOVE    "２公費"        TO  HCM37-HKNSBT1
           WHEN    "3"
               MOVE    "３老人"        TO  HCM37-HKNSBT1
           WHEN    "4"
               MOVE    "４退職"        TO  HCM37-HKNSBT1
           END-EVALUATE
      *
      *    保険種別２
           EVALUATE    RECE10-RECESYUBETU (3:1)
           WHEN    "1"
               MOVE    "１単独"        TO  HCM37-HKNSBT2
           WHEN    "2"
               MOVE    "２２併"        TO  HCM37-HKNSBT2
           WHEN    "3"
               MOVE    "３３併"        TO  HCM37-HKNSBT2
           END-EVALUATE
      *
      *    本人・家族
           EVALUATE    RECE10-RECESYUBETU (4:1)
           WHEN    "0"
               MOVE    "０高外８"      TO  HCM37-HONKZK
           WHEN    "2"
               MOVE    "２本外"        TO  HCM37-HONKZK
           WHEN    "4"
               MOVE    "４三外"        TO  HCM37-HONKZK
           WHEN    "6"
               MOVE    "６家外"        TO  HCM37-HONKZK
           WHEN    "8"
               MOVE    "８高外９"      TO  HCM37-HONKZK
           END-EVALUATE
      *
      *    診療年月
           MOVE    WRK-SRYYMWH         TO  HCM37-SRYM
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD     TO  HCM37-HOSPCD
      *
      *    医療機関名
           MOVE    SYS-1001-HOSPNAME   TO  HCM37-HOSPNAME
      *
      *    システム管理診療科読み込み
      *     PERFORM 900-SYS1005-READ-SEC
      *     IF      FLG-SYSKANRI        =   ZERO
      *        診療科
      *         MOVE    SYS-1005-SRYKANAME  TO  HCM37-SRYKA
      *     END-IF
      *
      *    患者情報読み込み
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           =   ZERO
      *        患者氏名
               MOVE    PTINF-NAME          TO  HCM37-NAME
           END-IF
      *
      *    生年月日
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
      *
           EVALUATE    WRK-HENYMDG(1:4)
           WHEN    "明治"
               MOVE    "○"                TO  HCM37-BIRTHG(1)
           WHEN    "大正"
               MOVE    "○"                TO  HCM37-BIRTHG(2)
           WHEN    "昭和"
               MOVE    "○"                TO  HCM37-BIRTHG(3)
           WHEN    "平成"
               MOVE    "○"                TO  HCM37-BIRTHG(4)
           END-EVALUATE
      *
           MOVE    WRK-HENYMDG(5:4)       TO  HCM37-BIRTHY
           MOVE    WRK-HENYMDG(11:4)      TO  HCM37-BIRTHM
           MOVE    WRK-HENYMDG(17:4)      TO  HCM37-BIRTHD
      *
      *    入外区分
           EVALUATE    RECE10-NYUGAIKBN
           WHEN    "1"
               MOVE    "○"                TO  HCM37-NYUGAIKBN(1)
           WHEN    "2"
               MOVE    "○"                TO  HCM37-NYUGAIKBN(2)
           END-EVALUATE
      *
      *    保険者番号
           MOVE    RECE10-HKNJANUM     TO  HCM37-HKNJANUM
      *
      *    老人保険
           IF      RECE10-RJNHKNNUM    NOT =   SPACE
               MOVE    RECE10-RJNHKNNUM    TO  WRK-KOHNUM
               PERFORM 900-PTKOH-READ-SEC
               IF      FLG-PTKOH           =   ZERO
                   MOVE    PTKOH-FTNJANUM      TO  HCM37-RJNFTNJANUM
                   MOVE    PTKOH-JKYSNUM       TO  HCM37-RJNJKYSNUM
               END-IF
           END-IF
      *
      *----(01.01.02)--UPD-START---
      *    保険公費
           PERFORM VARYING   IDX    FROM    1   BY   1
                   UNTIL    (IDX    >       4)
                   OR       (RECE10-KOHNUM(IDX) =   SPACE)
               MOVE    RECE10-KOHNUM(IDX)   TO  WRK-KOHNUM
               PERFORM 900-PTKOH-READ-SEC
               IF      FLG-PTKOH           =   ZERO
      *            生保のとき月代わり受給者番号マスタより受給者番号をセット 
                   IF    ( PTKOH-KOHNUM    =   "012" )
                       PERFORM 910-MONTHLYNUM-READ-SEC
                       IF      FLG-MONTHLYNUM  =   ZERO
                           MOVE    MONTHLYNUM-JKYSNUM
                                                   TO  HCM37-KOHJKYSNUM
      *----(01.01.03)--ADD-START---
                       ELSE
                           MOVE    PTKOH-JKYSNUM   TO  HCM37-KOHJKYSNUM
      *----(01.01.03)--ADD-END-----
                       END-IF
                   ELSE
                       MOVE    PTKOH-JKYSNUM   TO  HCM37-KOHJKYSNUM
                   END-IF    
                   MOVE    PTKOH-FTNJANUM  TO  HCM37-KOHFTNJANUM
      *            MOVE    4               TO  IDX
               END-IF
           END-PERFORM
      *----(01.01.02)--UPD-END-----
      *
      *    カルテ番号
           MOVE    RECE10-PTNUM        TO  HCM37-DEMPNUM
      *
           .
       210-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       400-PRINT-OUT-SEC           SECTION.
      *
           INITIALIZE                      ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE   "HCM37.red"          TO  SPRT-PRTID
           IF      WRK-PARA-TEISYUTUSAKI   =   "1"
               MOVE    "症状詳記（社保）"
                                       TO  SPRT-TITLE
           ELSE
               MOVE    "症状詳記（国保）"
                                       TO  SPRT-TITLE
           END-IF
           MOVE    HCM37               TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"               USING
                                           ORCSPRTAREA
           IF          SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       400-PRINT-OUT-EXT.
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
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
057400*
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *         
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    WRK-PATH            TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "*** ORCBM510 END ***"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    LNK-PRTKANRI-SKYYMD TO  ORC-DBYMD
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理基本情報読込
      *****************************************************************
       900-SYS1001-READ-SEC            SECTION.
      *
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
           ELSE
               MOVE    "*** ORCBM510 SYS1001 INVALID ***" 
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       900-SYS1001-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理診療科読込
      *****************************************************************
      * 900-SYS1005-READ-SEC            SECTION.
      *
      *     INITIALIZE                  SYS-1005-REC
      *     MOVE    "1005"              TO  SYS-1005-KANRICD
      *     MOVE    RECE10-RECEKA       TO  SYS-1005-KBNCD
      *     MOVE    SYS-1005-REC        TO  MCPDATA-REC
      *     PERFORM 800-SYSKANRI-READ-SEC
      *     IF      FLG-SYSKANRI        =   ZERO
      *         MOVE    MCPDATA-REC         TO  SYS-1005-REC
      *     ELSE
      *         INITIALIZE                      SYS-1005-REC
      *     END-IF
      *
      *     .
      * 900-SYS1005-READ-EXT.
      *     EXIT.
      *
      *****************************************************************
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-SELECT-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-END
      *     
      *    請求管理ＤＢ検索
           MOVE    SPACE               TO  RECE10-REC
           INITIALIZE                  RECE10-REC
           MOVE    SYS-1001-HOSPID     TO  RECE10-HOSPID
           MOVE    LNK-PRTKANRI-SRYYM  TO  RECE10-SKYYM
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-PATH            TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-PATH            TO  ORC-DBPATH
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
           .
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
           MOVE    WRK-PATH            TO  ORC-DBPATH
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-END
               MOVE    MCPDATA-REC         TO  RECE10-REC
               IF      RECE10-NYUGAIKBN    =   "1"
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
               ADD     1                   TO  CNT-RECE10
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電ＤＢ読込
      *****************************************************************
       900-RECEDEN-SELECT-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-RECEDEN
      *     
           MOVE    SPACE               TO  RECEDEN-REC
           INITIALIZE                  RECEDEN-REC
           MOVE    RECE10-KEY          TO  RECEDEN-KEY
           MOVE    "2"                 TO  RECEDEN-RECKBN
           MOVE    RECEDEN-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "RECEDEN-KEY2"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               PERFORM 900-RECEDEN-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-RECEDEN
           END-IF
           .
       900-RECEDEN-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電ＤＢ読込
      *****************************************************************
       900-RECEDEN-READ-SEC           SECTION.
      *
           MOVE    "RECEDEN-KEY2"      TO  ORC-DBPATH
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-RECEDEN
               MOVE    MCPDATA-REC         TO  RECEDEN-REC
               ADD     1                   TO  CNT-RECEDEN
           ELSE
               MOVE    1                   TO  FLG-RECEDEN
           END-IF
      *
           .
       900-RECEDEN-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報マスター読込
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           INITIALIZE                  PTINF-REC
           MOVE    SYS-1001-HOSPID     TO  PTINF-HOSPID
           MOVE    RECE10-PTID         TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "PTINF-KEY"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTINF-REC
                   MOVE    ZERO            TO  FLG-PTINF
               ELSE
                   INITIALIZE                  PTINF-REC
                   MOVE    1               TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *----(01.01.02)--UPD-START---
      *****************************************************************
      *    患者公費情報マスター読込
      *****************************************************************
       900-PTKOH-READ-SEC              SECTION.
      *
           INITIALIZE                  PTKOH-REC
           MOVE    SYS-1001-HOSPID     TO  PTKOH-HOSPID
           MOVE    RECE10-PTID         TO  PTKOH-PTID
           MOVE    WRK-KOHNUM          TO  PTKOH-KOHNUM
      *----(01.01.04)--UPD-START---
           MOVE    RECE10-TEKSTYMD     TO  PTKOH-TEKSTYMD
           MOVE    RECE10-TEKSTYMD     TO  PTKOH-TEKEDYMD
      *----(01.01.04)--UPD-END-----
           MOVE    PTKOH-REC           TO  MCPDATA-REC
      *     MOVE    RECE10-TEKSTYMD     TO  ORC-DBYMD
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTKOHINF-KEY6"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "PTKOHINF-KEY6"     TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTKOH-REC
                   MOVE    ZERO            TO  FLG-PTKOH
               ELSE
                   INITIALIZE                  PTKOH-REC
                   MOVE    1               TO  FLG-PTKOH
               END-IF
           ELSE
               INITIALIZE                  PTKOH-REC
               MOVE    1               TO  FLG-PTKOH
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTKOHINF-KEY6"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       900-PTKOH-READ-EXT.
           EXIT.
      *
      *----(01.01.02)--UPD-END-----
      *----(01.01.02)--ADD-START---
      *****************************************************************
      *    月代わり受給者番号マスタ読込
      *****************************************************************
       910-MONTHLYNUM-READ-SEC     SECTION.
      *
           INITIALIZE                  MONTHLYNUM-REC
           MOVE    RECE10-HOSPID   TO  MONTHLYNUM-HOSPID
           MOVE    RECE10-PTID     TO  MONTHLYNUM-PTID
           MOVE    "012"           TO  MONTHLYNUM-KOHNUM
      *----(01.01.03)--UPD-START---
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  MONTHLYNUM-SRYYM
      *----(01.01.03)--UPD-END-----
           MOVE    RECE10-NYUGAIKBN TO  MONTHLYNUM-NYUGAIKBN
           MOVE    MONTHLYNUM-REC   TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "MONTHLYNUM-KEY"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC               =   ZERO
               MOVE    "DBFETCH"            TO  MCP-FUNC
               MOVE    "MONTHLYNUM-KEY"     TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC               =   ZERO
                   MOVE    ZERO                 TO  FLG-MONTHLYNUM
                   MOVE    MCPDATA-REC          TO  MONTHLYNUM-REC
               ELSE
                   MOVE    1                   TO  FLG-MONTHLYNUM
               END-IF
           ELSE
               MOVE    1                   TO  FLG-MONTHLYNUM
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "MONTHLYNUM-KEY"    TO      ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       910-MONTHLYNUM-READ-EXT.
           EXIT.
      *
      *----(01.01.02)--ADD-END-----
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           .
       900-DBCLOSE-EXT.
           EXIT.
