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
       PROGRAM-ID.             ORCBPRVPRT.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : プレビューDBからの印刷
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/08/01    NACL-竹田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.04.01    NACL-藤原    07/10/05  印刷ＤＢ更新時のエラー対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE        ASSIGN  RECEERR
                                       FILE    STATUS  IS  STS-RECEERR.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01PARA//
                                       BY         //RECEERR//.
           03  FILLER                  PIC X(04)   VALUE   ".dat".
      *
      *    一覧
           COPY    "HCMSL80.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-OUT                 PIC X(02).
           03  STS-PTHKN               PIC X(02).
           03  STS-ERR                 PIC X(02).
           03  STS-RECLST              PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-SRYKARRK            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
      *
           03  FLG-CHK                 PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-ERR3                PIC 9(01).
           03  FLG-HOJOKBN             PIC 9(01).
           03  FLG-OK                  PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
      *
           03  CNT-LCNT1               PIC 9(02).
           03  CNT-LCNT2               PIC 9(02).
           03  CNT-PAGE                PIC 9(02).
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
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPID         PIC X(24).
           03  WRK-PARA-LSTYMD         PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-RECEERR             PIC X(200).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-SYMD2.
               05  WRK-SYY2            PIC 9(02).
               05  WRK-SMM2            PIC 9(02).
               05  WRK-SDD2            PIC 9(02).
           03  WRK-MAE-TEKSTYMD        PIC X(08).
           03  WRK-MAE-TEKEDYMD        PIC X(08).
           03  WRK-MAE-HKNNUM          PIC X(03).
           03  WRK-MAE-HKNID           PIC 9(10).
      *
           03  WRK-PTID                PIC 9(10).
           03  WRK-PAYKBN              PIC X(02).
           03  WRK-HKNNUM              PIC X(03).
      *
           03  WRK-Z3                  PIC ZZ9.
      *
           03  WRK-LSTYMD              PIC X(08).
      *
           03  WRK-BYMD-70.
               05  WRK-BYY             PIC 9(04).
               05  WRK-BMM             PIC 9(02).
               05  WRK-BDD             PIC 9(02).
      *
      *    帳票出力領域
       01  ERRMES-AREA.
           03  ERRMES-TITLE1.
               05  FILLER              PIC X(02)   VALUE  SPACE.
               05  FILLER              PIC X(60)   VALUE
               "＜＜　レセプトチェックリスト一覧　＞＞".
               05  ERRMES-TITLE1-YY    PIC X(04).
               05  FILLER              PIC X(01) VALUE ".".
               05  ERRMES-TITLE1-MM    PIC X(02).
               05  FILLER              PIC X(01) VALUE ".".
               05  ERRMES-TITLE1-DD    PIC X(02).
           03  ERRMES-TITLE3.
               05  FILLER              PIC X(04) VALUE
                                                 "NO".
               05  FILLER              PIC X(38) VALUE
                                                 "ERR-MESSAGE".
               05  FILLER              PIC X(20) VALUE
                   "患者番号".
               05  FILLER              PIC X(01) VALUE SPACE.
               05  FILLER              PIC X(10) VALUE SPACE.
               05  FILLER              PIC X(04) VALUE SPACE.
               05  FILLER              PIC X(10) VALUE
                   "患者ＩＤ".
      *
      *    出力エラーファイル
       01  ERR-REC.
               05  ERR-NO              PIC X(05).
               05  FILLER              PIC X(01).
               05  ERR-MES             PIC X(40).
               05  FILLER              PIC X(01).
               05  ERR-PTNUM           PIC X(20).
               05  FILLER              PIC X(01).
               05  ERR-LSTYMD          PIC X(10).
               05  FILLER              PIC X(03).
               05  ERR-PTID            PIC X(10).
               05  FILLER              PIC X(03).
               05  ERR-ERRMSG          PIC X(50).
      *
      *    変換明細
       01  MEISAI-HEAD.
           03  FILLER                  PIC X(02)   VALUE   SPACE.
           03  FILLER                  PIC X(60)   VALUE
               "＜＜　レセプトチェックリスト一覧　＞＞".
           03  FILLER                  PIC X(10)   VALUE   SPACE.
           03  MEISAI-HEAD-YY          PIC X(04).
           03  FILLER                  PIC X(01) VALUE ".".
           03  MEISAI-HEAD-MM          PIC X(02).
           03  FILLER                  PIC X(01) VALUE ".".
           03  MEISAI-HEAD-DD          PIC X(02).
      *
       01  MEISAI-HEAD2.
           03  FILLER                  PIC X(22)   VALUE
               "患者番号".
           03  FILLER                  PIC X(20)   VALUE
               "氏　　名".
           03  FILLER                  PIC X(08)   VALUE
               "診療年月".
           03  FILLER                  PIC X(02)   VALUE   SPACE.
           03  FILLER                  PIC X(06)   VALUE   SPACE.
           03  FILLER                  PIC X(02)   VALUE   SPACE.
           03  FILLER                  PIC X(12)   VALUE   SPACE.
           03  FILLER                  PIC X(10)   VALUE
               "修正内容".
      *
       01  MEISAI-BODY1.
           03  MEISAI-PTNUM            PIC X(20).
           03  MEISAI-NAME             PIC X(20).
           03  FILLER                  PIC X(02)   VALUE   SPACE.
           03  MEISAI-SRYYM            PIC X(08).
           03  FILLER                  PIC X(01)   VALUE   SPACE.
           03  MEISAI-MSG              PIC X(58).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    プレビューＤＢ
           COPY    "CPPRVKANRI.INC".          
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    システム管理（医療機関情報基本情報）
           COPY    "CPSK1001.INC".
      *
      *    患者番号構成管理情報
           COPY  "CPSK1009.INC".
      *
      *    患者番号変換マスタ
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
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
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "ORCA-DBPATH".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
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
                   UNTIL       FLG-END     =   1
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
                                       WRK-PARA-LSTYMD
           END-UNSTRING
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "ORCBPRVPRT"    TO  JOB-PGID
           MOVE    "プレビューチェックリスト出力"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
      *
           INITIALIZE                  SPA-AREA
      *
      *    システム日付を２００３０４０１とする
           MOVE    "20030401"      TO  SPA-SYSYMD
      *    作成日
           ACCEPT  WRK-SYMD2       FROM    DATE
           COMPUTE WRK-SYY         =   WRK-SYY2    +   2000
           MOVE    WRK-SMM2        TO  WRK-SMM
           MOVE    WRK-SDD2        TO  WRK-SDD
      *
           MOVE    WRK-SYY         TO  ERRMES-TITLE1-YY
           MOVE    WRK-SMM         TO  ERRMES-TITLE1-MM
           MOVE    WRK-SDD         TO  ERRMES-TITLE1-DD
      *
           MOVE    WRK-SYY         TO  MEISAI-HEAD-YY
           MOVE    WRK-SMM         TO  MEISAI-HEAD-MM
           MOVE    WRK-SDD         TO  MEISAI-HEAD-DD
      *
      *    端末ＩＤ固定
           MOVE    "ORCATERM"          TO  SPA-TERMID
      *
           MOVE    WRK-PARA-HOSPID     TO  SPA-HOSPID
      *
      *    患者番号構成管理情報編集
           INITIALIZE                  SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    SPA-SYSYMD          TO  ORC-DBYMD
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1009-REC
               MOVE    SYS-1009-PTNUMKSIKBN
                                           TO  SPA-1009-PTNUMKSIKBN
               MOVE    SYS-1009-HJNKSIKBN  TO  SPA-1009-HJNKSIKBN
               MOVE    SYS-1009-HJNKSINENKBN 
                                           TO  SPA-1009-HJNKSINENKBN
               MOVE    SYS-1009-HJNKSIRENNUMKBN
                                           TO  SPA-1009-HJNKSIRENNUMKBN
               MOVE    SYS-1009-HJNKSIRENNUMKETA
                                           TO  SPA-1009-HJNKSIRENNUMKETA
           ELSE
               MOVE    "患者番号構成管理情報が取得できませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           END-IF
      *
           PERFORM 900-PRVKANRI-INV-SEC
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
           ADD     1                   TO  CNT-IN
           MOVE    1                   TO  CNT-LCNT1
      *
           INITIALIZE                  HCMSL80
           MOVE    MEISAI-HEAD         TO  HCMSL80-MOJI (CNT-LCNT1)
           ADD     2                   TO  CNT-LCNT1
           MOVE    MEISAI-HEAD2        TO  HCMSL80-MOJI (CNT-LCNT1)
      *
           PERFORM             UNTIL   FLG-END     =   1
                               OR      CNT-LCNT1   >=  80
      *
               ADD     1                   TO  CNT-LCNT1
      *
               MOVE    SPACE               TO  MEISAI-BODY1
      *
               INITIALIZE                      ORCSPTIDAREA
               MOVE    SPA-HOSPID          TO  SPTID-HOSPID
               MOVE    PRVKANRI-PTID       TO  SPTID-PTID
               CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                                   SPA-1009-TBL
               MOVE    SPTID-PTNUM         TO  MEISAI-PTNUM
      * 
               INITIALIZE                          PTINF-REC
               MOVE    SPA-HOSPID          TO      PTINF-HOSPID
               MOVE    PRVKANRI-PTID       TO      PTINF-PTID
               MOVE    PTINF-REC           TO      MCPDATA-REC
               PERFORM 900-PTINF-READ-SEC
               IF      FLG-PTINF           =   ZERO
                   INSPECT PTINF-NAME      REPLACING   ALL "  "
                                                       BY  "　"
                   MOVE    PTINF-NAME      TO  MEISAI-NAME
               END-IF 
      *
               MOVE    PRVKANRI-SRYYM      TO  MEISAI-SRYYM
               MOVE    PRVKANRI-MEMO       TO  MEISAI-MSG  
      *
               MOVE    MEISAI-BODY1        TO  HCMSL80-MOJI (CNT-LCNT1)
      *
               PERFORM 900-PRVKANRI-FETCH-SEC
           END-PERFORM 
      *
      *    帳票印刷処理
           PERFORM 400-PRINT-OUT-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       400-PRINT-OUT-SEC                SECTION.
      *
           ADD     1                   TO  CNT-PAGE
      *
           INITIALIZE                  ORCSPRTAREA
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
           MOVE   "HCMSL80.red"        TO  SPRT-PRTID
           MOVE    "プレビューチェックリスト出力"
                                       TO  SPRT-TITLE
           MOVE    HCMSL80             TO  SPRT-PRTDATA
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
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           DISPLAY "*** ORCBPRVPRT PRVKANRI:"       CNT-IN
           DISPLAY "*** ORCBPRVPRT END ***"
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
           .
       300-END-EXT.
           EXIT.
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
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
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
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
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
      *     プレビューＤＢ読み込み
      *****************************************************************
       900-PRVKANRI-INV-SEC            SECTION.
      *
           MOVE    PRVKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    PATH-TBL-PRVKANRI-KEY5
                                       TO  MCP-PATH
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           IF      MCP-RC             =   ZERO
               PERFORM 900-PRVKANRI-FETCH-SEC 
           ELSE 
                   MOVE    1               TO  FLG-END
           END-IF
           .
       900-PRVKANRI-INV-EXT.
           EXIT.
      *      
      *****************************************************************
      *     プレビューＤＢ読み込み
      *****************************************************************
       900-PRVKANRI-FETCH-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    PATH-TBL-PRVKANRI-KEY5
                                       TO  MCP-PATH
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PRVKANRI-REC
                   MOVE    ZERO            TO  FLG-END
           ELSE  
                   MOVE    SPACE           TO  PRVKANRI-REC
                   MOVE    1               TO  FLG-END
           END-IF 
           .
       900-PRVKANRI-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    患者情報読込
      *****************************************************************
       900-PTINF-READ-SEC           SECTION.
      *       
           MOVE    "DBSELECT"          TO      MCP-FUNC
           MOVE    "PTINF-KEY"         TO      ORC-DBPATH
           CALL    "ORCMCPSUB"         USING   MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
           IF      MCP-RC              =       ZERO
               MOVE    "PTINF-KEY"     TO      ORC-DBPATH
               MOVE    "DBFETCH"           TO  MCP-FUNC
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
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"             TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
           PERFORM 1001-DBSTART-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＴＡＲＴ処理
      *****************************************************************
       1001-DBSTART-SEC        SECTION.
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           .
       1001-DBSTART-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＴＡＲＴ処理
      *****************************************************************
       9001-DBDBCOMMIT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           .
       9001-DBDBCOMMIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           IF      FLG-ERR         =   ZERO
               MOVE    "DBCOMMIT"      TO  MCP-FUNC
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           END-IF.
      *
           MOVE    "DBDISCONNECT"       TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
       900-DBDISCONNECT-EXT.
           EXIT.
