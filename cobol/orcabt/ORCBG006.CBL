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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBG006.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 薬剤使用量統計（月報）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/04/19    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.01.01    NACL-森脇    04/08/31  条件修正
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    統計用ファイル
           SELECT  TOKEI-FILE          ASSIGN    TOKEIPARA
                                       ORGANIZATION    IS  INDEXED
                                       ACCESS  MODE    IS  DYNAMIC
                                       RECORD  KEY     IS  TOKEI-KEY
                                       FILE    STATUS  IS  STS-TOKEI.
      *
           SELECT  TOKEIX-FILE         ASSIGN    TOKEIPARA
                                       ORGANIZATION    IS  INDEXED
                                       ACCESS  MODE    IS  SEQUENTIAL
                                       RECORD  KEY     IS  TOKEIX-KEY
                                       FILE    STATUS  IS  STS-TOKEIX.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE        ASSIGN  RECEERR
                                       FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    統計用ファイル
       FD  TOKEI-FILE.
       01  TOKEI-REC. 
           03  TOKEI-KEY.
               05  TOKEI-INGAIKBN      PIC 9(01).
               05  TOKEI-YKZKBN        PIC 9(01).
               05  TOKEI-KANANAME      PIC X(200).
               05  TOKEI-SRYCD         PIC X(09).
               05  TOKEI-SRYKA         PIC X(02).
           03  TOKEI-NAME              PIC X(40).
           03  TOKEI-TANI              PIC X(10).
           03  TOKEI-TEN               PIC 9(07)V9(02).
           03  TOKEI-SRYSURYO          PIC 9(10)V9(03).
      *
       FD  TOKEIX-FILE.
       01  TOKEIX-REC. 
           03  TOKEIX-KEY.
               05  TOKEIX-INGAIKBN     PIC 9(01).
               05  TOKEIX-YKZKBN       PIC 9(01).
               05  TOKEIX-KANANAME     PIC X(200).
               05  TOKEIX-SRYCD        PIC X(09).
               05  TOKEIX-SRYKA        PIC X(02).
           03  TOKEIX-NAME             PIC X(40).
           03  TOKEIX-TANI             PIC X(10).
           03  TOKEIX-TEN              PIC 9(07)V9(02).
           03  TOKEIX-SRYSURYO         PIC 9(10)V9(03).
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01//
                                       BY         //TOKEI//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01PARA//
                                       BY         //RECEERR//.
           03  FILLER                  PIC X(04)   VALUE   ".dat".
      *
           COPY    "HCMG006.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-TOKEI               PIC X(02).
           03  STS-TOKEIX              PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-TOKEI               PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-SRYACT              PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
      *
           03  FLG-OK                  PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE                PIC 9(02).
           03  CNT-PAGE                PIC 9(03).
           03  CNT-PAGE2               PIC 9(03).
           03  CNT-GPAGE               PIC 9(03).
           03  CNT-TOKEI               PIC 9(05).
           03  CNT-SRYKA               PIC 9(05).
           03  CNT-MAX                 PIC 9(02).
           03  CNT-KETA                PIC 9(02).
           03  CNT-KETA2               PIC 9(02).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY              PIC 9(02).
               05  SYS-MM              PIC 9(02).
               05  SYS-DD              PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(02).
      *
           03  IDX-D                   PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPID         PIC X(24).
           03  WRK-PARA-DENPPRTYM      PIC X(06).
           03  WRK-PARA-YKZKBN         PIC 9(01).
           03  WRK-PARA-INGAIKBN       PIC 9(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYY           PIC 9(04).
               05  WRK-SYSMM           PIC 9(02).
               05  WRK-SYSDD           PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-HENYMDG             PIC X(22).
      *
           03  WRK-SRYCD               PIC X(9).
           03  WRK-YKZKBN              PIC X(01).
           03  WRK-INGAIKBN            PIC X(01).
      *
           03  WRK-GSURYO              PIC 9(10)V9(03).
      *
           03  WRK-HENSURYO            PIC X(14).
      *
           03  WRK-SURYO               PIC 9(10)V9(03).
           03  WRK-SURYO-R             REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1        PIC 9(10).
               05  WRK-SURYO-S2        PIC 9(03).
      *
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1        PIC ZZZZZZZZZ9.
               05  WRK-SURYO-Z2        PIC X(01).
               05  WRK-SURYO-Z3        PIC 9(03).
      *
           03  WRK-HENTEN              PIC X(10).
      *
           03  WRK-TEN                 PIC 9(07)V9(02).
           03  WRK-TEN-R               REDEFINES   WRK-TEN.
               05  WRK-TEN-S1          PIC 9(07).
               05  WRK-TEN-S2          PIC 9(02).
      *
           03  WRK-TEN-X.
               05  WRK-TEN-Z1          PIC ZZZZZZ9.
               05  WRK-TEN-Z2          PIC X(01).
               05  WRK-TEN-Z3          PIC 9(02).
      *
           03  WRK-HENKIN              PIC X(14).
      *
           03  WRK-KIN                 PIC 9(10)V9(03).
           03  WRK-KIN-R               REDEFINES   WRK-KIN.
               05  WRK-KIN-S1          PIC 9(10).
               05  WRK-KIN-S2          PIC 9(03).
      *
           03  WRK-KIN-X.
               05  WRK-KIN-Z1          PIC ZZZZZZZZZ9.
               05  WRK-KIN-Z2          PIC X(01).
               05  WRK-KIN-Z3          PIC 9(03).
      *
           03  WRK-ZZ9                 PIC ZZ9.
           03  WRK-Z9                  PIC Z9.
      *
           03  WRK-YMD.
               05  WRK-SYSYMDWH        PIC X(22).
               05  WRK-YMD-MSG1        PIC X(4).
           03  WRK-DAI.
               05  WRK-DAI-MSG1        PIC X(32).
               05  WRK-DENPPRTYMWH     PIC X(16).
               05  WRK-DAI-MSG2        PIC X(6).
               05  WRK-YKZKBNWH        PIC X(12).
               05  WRK-INGAIKBNWH      PIC X(12).
           03  WRK-PAGE.
               05  WRK-PAGE1           PIC X(03).
               05  WRK-PAGE-MSG1       PIC X(01).
               05  WRK-PAGE2           PIC X(03).
      *
           03  WRK-DD                  PIC 9(02).
      *
           03  WRK-TAI-AREA.
               05  WRK-TAI-TBL         OCCURS  49  TIMES.
                 07  WRK-TAI-SRYKA     PIC X(02).
                 07  WRK-TAI-NAIYO     PIC X(14).
           03  WRK-NAI-AREA.
               05  WRK-NAI-TBL         OCCURS  25  TIMES.
                 07  WRK-NAI-SRYCD     PIC X(9).
                 07  WRK-NAI-NAME      PIC X(40).
                 07  WRK-NAI-TEN       PIC X(10).
                 07  WRK-NAI-TANI      PIC X(10).
                 07  WRK-NAI-SURYO     PIC X(14)
                                       OCCURS  49  TIMES.
                 07  WRK-NAI-GAKU      PIC X(14)
                                       OCCURS  49  TIMES.
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
      *    診療科目情報情報
           COPY    "CPSK1005.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為マスタ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
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
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                       INTO            LNK-PRTKANRI-RENNUM
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
                                       WRK-PARA-JOBID
                                       WRK-PARA-SHELLID
                                       WRK-PARA-HOSPID
                                       WRK-PARA-DENPPRTYM
                                       WRK-PARA-YKZKBN
                                       WRK-PARA-INGAIKBN
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "ORCBG006"      TO  JOB-PGID
           MOVE    "診療科別医薬品使用量統計（月報）"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
      *
           MOVE    "TOKEIZZ"       TO  TOKEIPARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  TOKEIPARA-TERMID
      *
           MOVE    "RECEERR"       TO  RECEERR-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  RECEERR-TERMID
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
       110-PARA-HENSYU-SEC         SECTION.
      *
      *    システム日付セット
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  WRK-SYMD
           PERFORM 400-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  WRK-SYSYMDWH
      *
      *    対象年月編集
           MOVE    WRK-PARA-DENPPRTYM
                                   TO  WRK-SYMD (1:6)
           MOVE    "01"            TO  WRK-SYMD (7:2)
           PERFORM 400-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)
                                   TO  WRK-DENPPRTYMWH
      *    表題編集
           MOVE    "診療科別医薬品使用量統計（月報）"
                                   TO  WRK-DAI-MSG1
           MOVE    "分　　"        TO  WRK-DAI-MSG2
           MOVE    "作成"          TO  WRK-YMD-MSG1
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    診療科セット処理
           PERFORM 210-MAIN-SRYKA-SEC
      *
      *    データをクリアする
           OPEN    OUTPUT              TOKEIX-FILE
           CLOSE                       TOKEIX-FILE
      *
           OPEN    I-O                 TOKEI-FILE
      *    診療会計集計処理
           PERFORM 220-MAIN-ACCT-SEC
      *
           CLOSE                       TOKEI-FILE
      *
           OPEN    INPUT               TOKEI-FILE
      *    集計統計データ順読込み
           PERFORM 900-TOKEI-NEXT-SEC
      *    統計印刷処理
           PERFORM 260-TOKEI-SET-SEC
                   UNTIL      FLG-TOKEI       =   1
           CLOSE                       TOKEI-FILE
      *
      *    データをクリアする
           OPEN    OUTPUT              TOKEIX-FILE
           CLOSE                       TOKEIX-FILE
      *
      *    ファイル削除
           MOVE    ZERO        TO  IF-RESULT
           MOVE    TOKEIPARA   TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科セット処理
      *****************************************************************
       210-MAIN-SRYKA-SEC          SECTION.
      *
           INITIALIZE                  WRK-TAI-AREA
           MOVE    ZERO            TO  CNT-SRYKA
      *
      *    診療科目編集
           PERFORM 900-SYS1005-DBSELECT-SEC
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL ( FLG-SYSKANRI    =   1 )   OR
                         ( IDX             >   49  )
               MOVE    SYS-1005-KBNCD  TO  WRK-TAI-SRYKA(IDX)
               MOVE    SYS-1005-SRYKANAME1
                                       TO  WRK-TAI-NAIYO(IDX)
               COMPUTE CNT-SRYKA       =   CNT-SRYKA   +   1
      *        診療科目編集
               PERFORM 900-SYS1005-DBFETCH-SEC
           END-PERFORM
      *
           EVALUATE    CNT-SRYKA
               WHEN    1   THRU    4
                   MOVE    5           TO  CNT-MAX
               WHEN    5   THRU    9
                   MOVE    10          TO  CNT-MAX
               WHEN    10  THRU    14
                   MOVE    15          TO  CNT-MAX
               WHEN    15  THRU    19
                   MOVE    20          TO  CNT-MAX
               WHEN    20  THRU    24
                   MOVE    25          TO  CNT-MAX
               WHEN    25  THRU    29
                   MOVE    30          TO  CNT-MAX
               WHEN    30  THRU    34
                   MOVE    35          TO  CNT-MAX
               WHEN    35  THRU    39
                   MOVE    40          TO  CNT-MAX
               WHEN    40  THRU    44
                   MOVE    45          TO  CNT-MAX
               WHEN    45  THRU    49
                   MOVE    50          TO  CNT-MAX
           END-EVALUATE
      *
           .
       210-MAIN-SRYKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計集計処理
      *****************************************************************
       220-MAIN-ACCT-SEC           SECTION.
      *
      *    対象診療年月で検索
           INITIALIZE                  SRYACCT-REC
           MOVE    WRK-PARA-HOSPID TO  ACCT-HOSPID
           MOVE    WRK-PARA-DENPPRTYM
                                   TO  ACCT-SRYYM
      *
           MOVE    SRYACCT-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "SRYACCT-KEY26" TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SRYACCT-KEY26" TO  ORC-DBPATH
               PERFORM 900-ACCT-DBFETCH-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           PERFORM UNTIL     ( FLG-SRYACCT     =   1 )
      *        最初の算定日を取得する
               MOVE    ZERO                TO  WRK-DD
               PERFORM VARYING     IDX-D   FROM    1   BY   1
                       UNTIL       IDX-D   >   31
                   IF      ACCT-DAY (1 IDX-D)  >   ZERO
                       MOVE    IDX-D           TO  WRK-DD
                       MOVE    31              TO  IDX-D
                   END-IF
               END-PERFORM
      *        算定がある時対象
               IF      WRK-DD          >   ZERO
      *----(01.01.01)--UPD-START---
      *            薬剤点数がなく、初診、再診、指導のときは対象外とする
                   IF    ( ACCT-YKZTEN     =   ZERO )  AND
                        (( ACCT-SRYKBN     =   "11" )  OR
                         ( ACCT-SRYKBN     =   "12" )  OR
                         ( ACCT-SRYKBN     =   "13" ))
                       CONTINUE
                   ELSE
                       PERFORM 230-MAIN-SRY-SEC
                   END-IF
      *----(01.01.01)--UPD-END-----
               END-IF
      *
               MOVE    "SRYACCT-KEY26" TO  ORC-DBPATH
               PERFORM 900-ACCT-DBFETCH-SEC
           END-PERFORM
      *
           MOVE    "SRYACCT-KEY26" TO  ORC-DBPATH
           PERFORM 900-ACCT-DBCLOSE-SEC
      *
           .
       220-MAIN-ACCT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為処理
      *****************************************************************
       230-MAIN-SRY-SEC            SECTION.
      *
           INITIALIZE                  SRYACT-REC
           MOVE    ACCT-HOSPID         TO  SRY-HOSPID
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
               PERFORM 900-SRY-DBFETCH-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           PERFORM UNTIL ( FLG-SRYACT      =   1 )
      *----(01.01.01)--UPD-START---
               IF    ( ACCT-ZAITEN         =   ZERO ) AND
                    (( ACCT-SRYKBN         =   "21" )  OR
                     ( ACCT-SRYKBN         =   "22" )  OR
                     ( ACCT-SRYKBN         =   "23" )  OR
                     ( ACCT-SRYKBN         =   "14" ))
                   IF    ( SRY-SRYSYUKBN       =   "213")  OR
                         ( SRY-SRYSYUKBN       =   "223")  OR
                         ( SRY-SRYSYUKBN       =   "233")
                       MOVE    "1"         TO  WRK-INGAIKBN
                   ELSE
                       MOVE    "2"         TO  WRK-INGAIKBN
                   END-IF
               ELSE
                   MOVE    "1"         TO  WRK-INGAIKBN
               END-IF
      *
      *        院内
               IF    ( WRK-INGAIKBN    =   "1" ) AND
                     ( WRK-PARA-INGAIKBN   NOT =   1 )
                   PERFORM 240-MAIN-HEN-SEC
               END-IF
      *        院外
               IF    ( WRK-INGAIKBN    =   "2" ) AND
                     ( WRK-PARA-INGAIKBN NOT =   0 ) AND
                     ( WRK-PARA-INGAIKBN NOT =   SPACE )
                   PERFORM 240-MAIN-HEN-SEC
               END-IF
      *----(01.01.01)--UPD-END-----
      *
               MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
               PERFORM 900-SRY-DBFETCH-SEC
      *
           END-PERFORM
      *
           MOVE    "SRYACT-KEY2"   TO  ORC-DBPATH
           PERFORM 900-SRY-DBCLOSE-SEC
            .
       230-MAIN-SRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    主編集処理
      *****************************************************************
       240-MAIN-HEN-SEC            SECTION.
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL ( IDX     >   5 )
                   OR    ( SRY-SRYCD(IDX)  =   SPACE )
      *        薬剤を対象とする
               IF      SRY-SRYCD(IDX)(1:1)     =   "6"
                   PERFORM 250-TOKEI-HEN-SEC 
               END-IF
           END-PERFORM
            .
       240-MAIN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計編集処理
      *****************************************************************
       250-TOKEI-HEN-SEC           SECTION.
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    SRY-SRYCD(IDX)      TO  TNS-SRYCD
      *    点数マスタ検索
           PERFORM 900-TNS-READ-SEC
      *
      *    薬剤区分チェック
           MOVE    ZERO                TO  FLG-OK
           EVALUATE    WRK-PARA-YKZKBN
      *        全体
               WHEN    0
                   IF      TNS-YKZKBN      =   3
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-OK
                   END-IF
      *        内服薬
               WHEN    1
                   IF      TNS-YKZKBN      =   1
                       MOVE    1               TO  FLG-OK
                   END-IF
      *        注射薬
               WHEN    2
                   IF      TNS-YKZKBN      =   4
                       MOVE    1               TO  FLG-OK
                   END-IF
      *        外用薬
               WHEN    3
                   IF      TNS-YKZKBN      =   6
                       MOVE    1               TO  FLG-OK
                   END-IF
      *        歯科用薬剤
               WHEN    4
                   IF      TNS-YKZKBN      =   8
                       MOVE    1               TO  FLG-OK
                   END-IF
      *        歯科用特定薬剤
               WHEN    5
                   IF      TNS-YKZKBN      =   9
                       MOVE    1               TO  FLG-OK
                   END-IF
           END-EVALUATE
           IF      FLG-OK              =   ZERO
               GO  TO  250-TOKEI-HEN-EXT
           END-IF
      *
           MOVE    ZERO            TO  WRK-SURYO
           COMPUTE WRK-SURYO       =   SRY-SRYSURYO(IDX)
                                   *   ACCT-ZAIKAISU
      *
      *    統計データ検索
           INITIALIZE                  TOKEI-REC
           MOVE    WRK-INGAIKBN    TO  TOKEI-INGAIKBN
           MOVE    TNS-YKZKBN      TO  TOKEI-YKZKBN
           MOVE    TNS-KANANAME    TO  TOKEI-KANANAME
           MOVE    SRY-SRYCD(IDX)  TO  TOKEI-SRYCD
           MOVE    ACCT-SRYKA      TO  TOKEI-SRYKA
           PERFORM 900-TOKEI-READ-SEC
      *
           IF        ( FLG-TOKEI       =   1 )
               INITIALIZE                  TOKEI-REC
               MOVE    WRK-INGAIKBN    TO  TOKEI-INGAIKBN
               MOVE    TNS-YKZKBN      TO  TOKEI-YKZKBN
               MOVE    TNS-KANANAME    TO  TOKEI-KANANAME
               MOVE    SRY-SRYCD(IDX)  TO  TOKEI-SRYCD
               MOVE    ACCT-SRYKA      TO  TOKEI-SRYKA
               MOVE    TNS-NAME        TO  TOKEI-NAME
               MOVE    TNS-TEN         TO  TOKEI-TEN
               MOVE    TNS-TANINAME    TO  TOKEI-TANI
               MOVE    ZERO            TO  TOKEI-SRYSURYO
               COMPUTE TOKEI-SRYSURYO  =   TOKEI-SRYSURYO
                                       +   WRK-SURYO
               WRITE   TOKEI-REC
               COMPUTE CNT-TOKEI       =   CNT-TOKEI   +   1
           ELSE
               COMPUTE TOKEI-SRYSURYO  =   TOKEI-SRYSURYO
                                       +   WRK-SURYO
               REWRITE TOKEI-REC
           END-IF
             .
       250-TOKEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計印刷処理
      *****************************************************************
       260-TOKEI-SET-SEC           SECTION.
      *
      *    薬剤区分毎に印刷する
           INITIALIZE                      WRK-NAI-AREA
           MOVE    ZERO                TO  WRK-GSURYO
           MOVE    ZERO                TO  CNT-PAGE
           MOVE    ZERO                TO  IDY
      *
           MOVE    TOKEI-INGAIKBN      TO  WRK-INGAIKBN
           MOVE    TOKEI-YKZKBN        TO  WRK-YKZKBN
           PERFORM
               UNTIL    (FLG-TOKEI         =   1  )  OR
                        (TOKEI-INGAIKBN    NOT = WRK-INGAIKBN) OR
                        (TOKEI-YKZKBN  NOT = WRK-YKZKBN)
      *        統計編集処理
               PERFORM 2601-TOUKEI-HEN-SEC
           END-PERFORM
      *
      *    印刷処理
           PERFORM 2602-PRINT-SEC
      *
             .
       260-TOKEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計編集処理
      *****************************************************************
       2601-TOUKEI-HEN-SEC           SECTION.
      *
           ADD     1                   TO  IDY
      *
      *    １枚２５明細とする
           IF      IDY                 >   25
      *        印刷処理
               PERFORM 2602-PRINT-SEC
               MOVE    1                   TO  IDY
               INITIALIZE                  WRK-NAI-AREA
           END-IF
      *
      *    薬剤毎の編集
           MOVE    TOKEI-SRYCD         TO  WRK-NAI-SRYCD (IDY)
           MOVE    TOKEI-NAME          TO  WRK-NAI-NAME  (IDY)
           MOVE    TOKEI-TANI          TO  WRK-NAI-TANI  (IDY)
      *    点数編集
           MOVE    TOKEI-TEN           TO  WRK-TEN
           PERFORM 400-ZHEN-TEN-SET-SEC
           MOVE    WRK-HENTEN          TO  WRK-NAI-TEN(IDY)
      *
           MOVE    TOKEI-SRYCD         TO  WRK-SRYCD
           MOVE    ZERO                TO  WRK-GSURYO
           PERFORM
               UNTIL    (FLG-TOKEI         =   1  )  OR
                        (TOKEI-YKZKBN  NOT = WRK-YKZKBN)  OR
                        (TOKEI-INGAIKBN    NOT = WRK-INGAIKBN)  OR
                        (TOKEI-SRYCD   NOT = WRK-SRYCD )
      *        数量編集処理
               PERFORM 26011-SURYOU-HEN-SEC
      *
      *        データ順読込み処理
               PERFORM 900-TOKEI-NEXT-SEC
           END-PERFORM
      *
      *    科合計編集
           MOVE    WRK-GSURYO          TO  WRK-SURYO
           PERFORM 400-ZHEN-SURYO-SET-SEC
           MOVE    WRK-HENSURYO        TO  WRK-NAI-SURYO(IDY CNT-MAX)
      *    合計編集
           MOVE    ZERO                TO  WRK-KIN
           COMPUTE WRK-KIN         =   WRK-TEN
                                   *   WRK-GSURYO
           PERFORM 400-ZHEN-KIN-SET-SEC
           MOVE    WRK-HENKIN          TO  WRK-NAI-GAKU(IDY CNT-MAX)
      *
             .
       2601-TOUKEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票内容テーブルセット
      *****************************************************************
       26011-SURYOU-HEN-SEC        SECTION.
      *
      *    科毎に数量を編集する
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   CNT-SRYKA )
               IF      TOKEI-SRYKA         =   WRK-TAI-SRYKA (IDX)
      *            数量編集
                   MOVE    TOKEI-SRYSURYO      TO  WRK-SURYO
                   PERFORM 400-ZHEN-SURYO-SET-SEC
                   MOVE    WRK-HENSURYO        TO  WRK-NAI-SURYO
                                                           (IDY IDX)
      *            合計編集
                   COMPUTE WRK-KIN         =   TOKEI-TEN
                                           *   TOKEI-SRYSURYO
                   PERFORM 400-ZHEN-KIN-SET-SEC
                   MOVE    WRK-HENKIN          TO  WRK-NAI-GAKU
                                                           (IDY IDX)
                   MOVE    CNT-SRYKA           TO  IDX
               END-IF
           END-PERFORM
      *
      *    数量集計
           COMPUTE WRK-GSURYO      =   WRK-GSURYO
                                   +   TOKEI-SRYSURYO
      *
             .
       26011-SURYOU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       2602-PRINT-SEC              SECTION.
      *
           COMPUTE CNT-PAGE        =   CNT-PAGE    +   1
           MOVE    ZERO            TO  CNT-PAGE2
           MOVE    ZERO            TO  IDZ
      *
           PERFORM UNTIL ( IDZ     =   CNT-MAX )
      *        見出編集
               PERFORM 280-PRT-HEAD-SET-SEC
      *        明細編集
               PERFORM 290-PRT-BODY-SET-SEC
      *        印刷処理
               PERFORM 400-PRINT-OUT-SEC
           END-PERFORM
      *
             .
       2602-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理見出し
      *****************************************************************
       280-PRT-HEAD-SET-SEC        SECTION.
      *
           MOVE    ZERO            TO  CNT-LINE
           COMPUTE CNT-PAGE2       =   CNT-PAGE2   +   1
           COMPUTE CNT-GPAGE       =   CNT-GPAGE   +   1
      *
           MOVE    SPACE           TO  HCMG006
      *
      *    見出し
           EVALUATE  WRK-YKZKBN
               WHEN  "1"
                   MOVE    "（内服薬）"        TO  WRK-YKZKBNWH
      *         WHEN  "3"
      *             MOVE   "（その他）"        TO  WRK-YKZKBNWH
               WHEN  "4"
                   MOVE    "（注射薬）"        TO  WRK-YKZKBNWH
               WHEN  "6"
                   MOVE    "（外用薬）"        TO  WRK-YKZKBNWH
               WHEN  "8"
                   MOVE    "（歯科薬剤）"      TO  WRK-YKZKBNWH
               WHEN  "9"
                   MOVE    "（歯科特定）"      TO  WRK-YKZKBNWH
               WHEN  OTHER
                   MOVE    "＊＊＊＊"          TO  WRK-YKZKBNWH
           END-EVALUATE
           EVALUATE  WRK-INGAIKBN
               WHEN  "1"
                   MOVE    "（院内）"        TO  WRK-INGAIKBNWH
               WHEN  "2"
                   MOVE    "（院外）"        TO  WRK-INGAIKBNWH
           END-EVALUATE
      *
           IF          CNT-MAX         <=  5
               MOVE    CNT-PAGE        TO  WRK-ZZ9
               MOVE    WRK-ZZ9         TO  WRK-PAGE2
           ELSE
               MOVE    CNT-PAGE        TO  WRK-ZZ9
               MOVE    WRK-ZZ9         TO  WRK-PAGE1
               MOVE    CNT-PAGE2       TO  WRK-ZZ9
               MOVE    WRK-ZZ9         TO  WRK-PAGE2
               MOVE    "-"             TO  WRK-PAGE-MSG1
           END-IF
      *
           MOVE    WRK-DAI         TO  HCMG006-DAI
           MOVE    WRK-YMD         TO  HCMG006-YMD
           MOVE    WRK-PAGE        TO  HCMG006-PAGE
      *
           MOVE    "診療行為コード"
                                   TO  HCMG006-TAI-CD
           MOVE    "名称"          TO  HCMG006-TAI-NAME
           MOVE    "単位"          TO  HCMG006-TAI-TANI
           MOVE    "薬価"          TO  HCMG006-TAI-TEN
      *
           .
       280-PRT-HEAD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理明細
      *****************************************************************
       290-PRT-BODY-SET-SEC        SECTION.
      *
      *    明細
           PERFORM VARYING CNT-LINE    FROM    1   BY   1
                   UNTIL ( CNT-LINE     >   25 )
               MOVE    WRK-NAI-SRYCD(CNT-LINE)
                                       TO  HCMG006-CD(CNT-LINE)
               MOVE    WRK-NAI-NAME(CNT-LINE)
                                       TO  HCMG006-NAME(CNT-LINE)
               MOVE    WRK-NAI-TANI(CNT-LINE)
                                       TO  HCMG006-TANI(CNT-LINE)
               MOVE    WRK-NAI-TEN(CNT-LINE)
                                       TO  HCMG006-TEN(CNT-LINE)
           END-PERFORM
      *
      *    診療科別編集
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL ( IDX         >   5 )
                   OR    ( IDZ         =   CNT-MAX )
               COMPUTE IDZ                 =   IDZ +   1
      *        科名編集
               MOVE    WRK-TAI-NAIYO(IDZ)  TO  HCMG006-TAI-NAIYO(IDX)
               IF      (IDZ            =   CNT-MAX )
                   MOVE    "合計"          TO  HCMG006-TAI-NAIYO(IDX)
               END-IF
      *        数量編集
               PERFORM VARYING CNT-LINE    FROM    1   BY   1
                       UNTIL ( CNT-LINE    >   25 )
                   MOVE    WRK-NAI-SURYO(CNT-LINE IDZ)
                                       TO  HCMG006-SURYO(CNT-LINE IDX)
                   MOVE    WRK-NAI-GAKU(CNT-LINE IDZ)
                                       TO  HCMG006-GAKU(CNT-LINE IDX)
               END-PERFORM
           END-PERFORM
      *
           .
       290-PRT-BODY-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       400-SEIWA-HEN-SEC           SECTION.
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
           INSPECT WRK-HENYMDG     REPLACING  ALL "  "  BY  "　"
           .
       400-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *   帳票Ｚ編集処理
      *****************************************************************
       400-ZHEN-SURYO-SET-SEC      SECTION.
      *
           MOVE    SPACE           TO  WRK-HENSURYO
           MOVE    SPACE           TO  WRK-SURYO-X
      *
      *    点数Ｚ編集
           IF        ( WRK-SURYO-S1    =   ZERO )
               AND   ( WRK-SURYO-S2    =   ZERO )
               GO  TO  400-ZHEN-SURYO-SET-EXT
           END-IF
      *
           MOVE    WRK-SURYO-S1    TO  WRK-SURYO-Z1
           MOVE    "."             TO  WRK-SURYO-Z2
           MOVE    WRK-SURYO-S2    TO  WRK-SURYO-Z3
      *
           MOVE    WRK-SURYO-X     TO  WRK-HENSURYO
      *
           .
       400-ZHEN-SURYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票Ｚ編集処理
      *****************************************************************
       400-ZHEN-TEN-SET-SEC        SECTION.
      *
           MOVE    SPACE           TO  WRK-HENTEN
           MOVE    SPACE           TO  WRK-TEN-X
      *
      *    点数Ｚ編集
           IF        ( WRK-TEN-S1      =   ZERO )
               AND   ( WRK-TEN-S2      =   ZERO )
               GO  TO  400-ZHEN-TEN-SET-EXT
           END-IF
      *
           MOVE    WRK-TEN-S1      TO  WRK-TEN-Z1
           MOVE    "."             TO  WRK-TEN-Z2
           MOVE    WRK-TEN-S2      TO  WRK-TEN-Z3
      *
           MOVE    WRK-TEN-X       TO  WRK-HENTEN
      *
           .
       400-ZHEN-TEN-SET-EXT.
           EXIT.
      *****************************************************************
      *   帳票Ｚ編集処理
      *****************************************************************
       400-ZHEN-KIN-SET-SEC        SECTION.
      *
           MOVE    SPACE           TO  WRK-HENKIN
           MOVE    SPACE           TO  WRK-KIN-X
      *
      *    点数Ｚ編集
           IF        ( WRK-KIN-S1      =   ZERO )
               AND   ( WRK-KIN-S2      =   ZERO )
               GO  TO  400-ZHEN-KIN-SET-EXT
           END-IF
      *
           MOVE    WRK-KIN-S1      TO  WRK-KIN-Z1
           MOVE    "."             TO  WRK-KIN-Z2
           MOVE    WRK-KIN-S2      TO  WRK-KIN-Z3
      *
           MOVE    WRK-KIN-X       TO  WRK-HENKIN
      *
           .
       400-ZHEN-KIN-SET-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       400-PRINT-OUT-SEC           SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
      *     MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    WRK-PARA-DENPPRTYM  TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE   "HCMG006.red"        TO  SPRT-PRTID
           MOVE    "薬剤使用量統計（月報）"
                                       TO  SPRT-TITLE
           MOVE    HCMG006             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       400-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT               RECEERR-FILE
           IF        ( STS-RECEERR     =   ZERO )
               CONTINUE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR     TO  RECEERR-REC
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
               CALL    "ORCSJOB"           USING
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
           IF          CNT-TOKEI   =   ZERO 
               MOVE    "処理対象のデータがありませんでした"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           DISPLAY "*** ORCBG006 IN "      CNT-TOKEI
           DISPLAY "*** ORCBG006 INKA "    CNT-SRYKA
           DISPLAY "*** ORCBG006 END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    CNT-GPAGE       TO  JOB-UPDCNT
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療会計マスタ検索処理
      *****************************************************************
       900-ACCT-DBFETCH-SEC        SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF        ( MCP-RC          =   ZERO )
               MOVE    ZERO            TO  FLG-SRYACCT
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
           END-IF
           .
       900-ACCT-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタクローズ処理
      *****************************************************************
       900-ACCT-DBCLOSE-SEC        SECTION.
      *
           MOVE    "DBCLOSE"       TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           .
       900-ACCT-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療行為マスタ検索処理
      *****************************************************************
       900-SRY-DBFETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF        ( MCP-RC          =   ZERO )
               MOVE    ZERO            TO  FLG-SRYACT
               MOVE    MCPDATA-REC     TO  SRYACT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACT
           END-IF
           .
       900-SRY-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスタクローズ処理
      *****************************************************************
       900-SRY-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSE"       TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           .
       900-SRY-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計ファイル読込
      *****************************************************************
       900-TOKEI-READ-SEC          SECTION.
      *
           READ    TOKEI-FILE
               INVALID
                   MOVE    1           TO  FLG-TOKEI
               NOT INVALID
                   MOVE    ZERO        TO  FLG-TOKEI
           END-READ
           .
       900-TOKEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計ファイル順読込
      *****************************************************************
       900-TOKEI-NEXT-SEC          SECTION.
      *
           READ    TOKEI-FILE      NEXT
               AT  END
                   MOVE    1           TO  FLG-TOKEI
               NOT AT  END
                   MOVE    ZERO        TO  FLG-TOKEI
           END-READ
           .
       900-TOKEI-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ検索
      *****************************************************************
       900-TNS-READ-SEC            SECTION.
      *
           MOVE    ZERO            TO  TNS-YUKOSTYMD
           MOVE    ALL "9"         TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "TENSU-KEY"     TO  ORC-DBPATH
           MOVE    WRK-PARA-DENPPRTYM
                                   TO  ORC-DBYMD(1:6)
           MOVE    WRK-DD          TO  ORC-DBYMD(7:2)
      *
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF        ( MCP-RC          =   ZERO )
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "TENSU-KEY"     TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF        ( MCP-RC          =   ZERO )
                   MOVE    ZERO            TO  FLG-TENSU
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1               TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
           .
       900-TNS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ診療科検索
      *****************************************************************
       900-SYS1005-DBSELECT-SEC    SECTION.
      *
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"          TO  SYS-1005-KANRICD
           MOVE    ZERO            TO  SYS-1005-STYUKYMD
           MOVE    ALL "9"         TO  SYS-1005-EDYUKYMD
           MOVE    SYS-1005-REC    TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY2" TO  ORC-DBPATH
           MOVE    WRK-SYSYMD      TO  ORC-DBYMD
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF        ( MCP-RC          =   ZERO )
               PERFORM 900-SYS1005-DBFETCH-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYS1005-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     管理マスタ診療科検索処理
      *****************************************************************
       900-SYS1005-DBFETCH-SEC     SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY2" TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF        ( MCP-RC          =   ZERO )
               MOVE    ZERO            TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYS1005-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ診療科クローズ処理
      *****************************************************************
       900-SYS1005-DBCLOSE-SEC     SECTION.
      *
           MOVE    "DBCLOSE"       TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY2" TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           .
       900-SYS1005-DBCLOSE-EXT.
           EXIT.
      *
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
       900-DBDISCONNECT-EXT.
           EXIT.
