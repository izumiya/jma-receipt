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
       PROGRAM-ID.             ORCT0010.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 定期請求
      *  コンポーネント名  : 収納データ作成
      *  管理者            :
      *  作成日付    作業者        記述
      *  02/01/07    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    03.04.02  負担金計算サブ引数修正
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    定期請求収納ファイル(初期化用)
           SELECT  TEIKI01CL-FILE
                                  ASSIGN   TEIKI01PARA
                                  ORGANIZATION    IS  INDEXED
                                  ACCESS  MODE    IS  SEQUENTIAL
                                  RECORD  KEY     IS  TEIKI01CL-KEY
                                  FILE    STATUS  IS  STS-TEIKI01CL.
      *
      *    定期請求収納ファイル(OT)
           SELECT  TEIKI01-FILE   ASSIGN   TEIKI01PARA
                                  ORGANIZATION    IS  INDEXED
                                  ACCESS  MODE    IS  DYNAMIC
                                  RECORD  KEY     IS  TEIKI01-KEY
                                  FILE    STATUS  IS  STS-TEIKI01.
      *
      *    患者請求履歴ファイル(OT)
           SELECT  TEIKI02-FILE    ASSIGN  TEIKI02PARA
                   ORGANIZATION   LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-TEIKI02.
      *
      *    定期請求期間情報ファイル(OT)
           SELECT  TEIKI03-FILE    ASSIGN  TEIKI03PARA
                   ORGANIZATION   LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-TEIKI03.
      *
      *    エラーファイル
           SELECT  TEIKIERR-FILE   ASSIGN  TEIKIERR
                                   FILE    STATUS  IS  STS-TEIKIERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    定期請求収納ファイル（初期化用）
       FD  TEIKI01CL-FILE.
       01  TEIKI01CL-REC.
           COPY "CPTEIKI01.INC"        REPLACING   //TEIKI01-//
                                       BY          //TEIKI01CL-//.
      *
      *    定期請求収納ファイル
       FD  TEIKI01-FILE.
       01  TEIKI01-REC.
           COPY    "CPTEIKI01.INC".
      *
      *    定期請求履歴情報ファイル
       FD  TEIKI02-FILE.
       01  TEIKI02-REC.
           COPY    "CPPTTEIKIRRK.INC"  REPLACING   //PTTEIKIRRK-//
                                       BY          //TEIKI02-//.
      *
      *    定期請求期間情報
       FD  TEIKI03-FILE.
           COPY "CPTEIKI03.INC".
      *
      *    エラーファイル
       FD  TEIKIERR-FILE.
       01  TEIKIERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    定期請求収納ファイル名称領域 
           COPY    "CPCOMMONDAT5.INC".
      *
      *
      *    定期請求履歴情報ファイル名称領域 
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01//
                                   BY         //TEIKI02//.
      *
      *
      *    定期請求期間情報名称領域 
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01//
                                   BY         //TEIKI03//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01PARA//
                                   BY         //TEIKIERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-TEIKIERR                            PIC X(02).
           03  STS-TEIKI01                             PIC X(02).
           03  STS-TEIKI01CL                           PIC X(02).
           03  STS-TEIKI02                             PIC X(02).
           03  STS-TEIKI03                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-ERR                                 PIC 9(01).
           03  FLG-PTNYUINRRK                          PIC 9(01).
           03  FLG-PTNYUINRRK2                         PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-SYS5012                             PIC 9(01).
           03  FLG-PTINF                               PIC 9(01).
           03  FLG-ORCSDAY                             PIC 9(01).
           03  FLG-NYUINKA                             PIC 9(01).
           03  FLG-PTTEIKIRRK                          PIC 9(01).
           03  FLG-GETUMATUSITEI                       PIC 9(01).
           03  FLG-TAISYO                              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-OUT1                                PIC 9(06).
           03  CNT-OUT2                                PIC 9(06).
           03  CNT-OUT3                                PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX1                                    PIC 9(04). 
           03  IDX2                                    PIC 9(04). 
           03  IDX3                                    PIC 9(04). 
           03  IDX10                                   PIC 9(04). 
           03  IDX11                                   PIC 9(04). 
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME.
               05  SYS-THH         PIC 9(02).
               05  SYS-TMM         PIC 9(02).
               05  SYS-TSS         PIC 9(02).
               05  SYS-TDD         PIC 9(02).
           03  SYS-YMD.
               05  SYS-YY          PIC 9(04).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
           03  SYS-WKYMD.
               05  SYS-WKYY        PIC 9(02).
               05  SYS-WKMM        PIC 9(02).
               05  SYS-WKDD        PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-DBPATH                          PIC  X(100).
           03  WRK-PARA.
               05  WRK-PARA-ARG1.
                   07   WRK-PARA-SRYYM             PIC  X(06).
                   07   WRK-PARA-TERMID            PIC  X(64).
                   07   WRK-PARA-SYSYMD            PIC  X(08).
               05  WRK-PARA-SYOKBN                 PIC  X(01).
               05  WRK-PARA-KIKAN-OCC  OCCURS 3.
                   07  WRK-PARA-KIKAN-ST       PIC  X(08).
                   07  WRK-PARA-KIKAN-ED       PIC  X(08).
                   07  WRK-PARA-KIKAN-KBN      PIC  X(01).
               05  WRK-PARA-JOBID              PIC  9(07).
               05  WRK-PARA-SHELLID            PIC  X(08).
      *
           03  WRK-TEIKIERR            PIC X(200).
      *
           03  WRK-STYMD               PIC X(08).
           03  WRK-EDYMD               PIC X(08).
           03  WRK-KIKAN-TBL.
               05  WRK-KIKAN-MAX       PIC 9(04).
               05  WRK-KIKAN-OCC       OCCURS 3.
                   07  WRK-KIKANST     PIC X(08).
                   07  WRK-KIKANED     PIC X(08).
                   07  WRK-KIKANSAKKBN PIC 9(01).
      *
           03  WRK-TEIKISTYMD          PIC X(08).
           03  WRK-TEIKIEDYMD          PIC X(08).
           03  WRK-TEIKISAKKBN         PIC X(01).
           03  WRK-TEIKIGETUMATU       PIC X(01).
      *
           03  WRK-YMD.
               05  WRK-YY              PIC X(04).
               05  WRK-MM              PIC X(02).
               05  WRK-DD              PIC X(02).
           03  WRK-NISSU1              PIC 9(05).
           03  WRK-NISSU2              PIC 9(05).
           03  WRK-NYUINKA             PIC X(02).
           03  WRK-BTUNUM              PIC X(02).
           03  WRK-BRMNUM              PIC X(08).
           03  WRK-DRCD                PIC X(04).
           03  WRK-SEIKYUSTYMD         PIC X(08).
           03  WRK-SEIKYUEDYMD         PIC X(08).
           03  WRK-CNT                 PIC 9(05).
           03  WRK-SAKKBN              PIC X(01).
      *
       01  ERR-EDIT-AREA.
           03  ERR-PTID                PIC 9(10).
           03  ERR-PTNUM               PIC X(20).
           03  ERR-ERRMSG              PIC X(80).
           03  ERR-HEN-ERRMSG          PIC X(80).
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *
      *    ワーク患者入院履歴
       01  WKPTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING   //PTNYUINRRK-//
                                       BY          //WKPTNYUINRR-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *    患者番号構成情報
           COPY    "CPSK1009.INC".
      *
      *    請求管理
           COPY    "CPSK5010.INC".
      *    定期請求作成指示コード
           COPY    "CPSK5011.INC".
      *    定期請求作成指示（個別指示）コード
           COPY    "CPSK5012.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    収納マスター
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *                                                                 TR1226
      *    患者定期請求履歴                                             TR1226
       01  PTTEIKIRRK-REC.                                              TR1226
           COPY    "CPPTTEIKIRRK.INC".                                  TR1226
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    定期請求用収納作成サブ
       01  LNK-ORCS02-REC.
           COPY  "CPORCS02.INC".
       01  LNK-SYUNOU-REC-T.
         02 LNK-SYUNOU-REC     OCCURS 10.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-// BY //LNK-SYU-//.
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(2000).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL ( FLG-END =   1 )
                                      OR ( FLG-ERR =   1 )
      *
           PERFORM 300-TERM-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  INDEX-AREA
                                       CNT-AREA
                                       WRK-AREA
                                       SYS-AREA
                                       SPA-AREA
      *
           ACCEPT  SYS-TIME    FROM    TIME
      *
           ACCEPT  SYS-WKYMD   FROM    DATE
           COMPUTE SYS-YY  =   SYS-WKYY    +   2000
           COMPUTE SYS-MM  =   SYS-WKMM
           COMPUTE SYS-DD  =   SYS-WKDD
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    WRK-PARA-ARG1
                                               WRK-PARA-SYOKBN
                                               WRK-PARA-KIKAN-OCC (1)
                                               WRK-PARA-KIKAN-OCC (2)
                                               WRK-PARA-KIKAN-OCC (3)
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "ORCT0010"      TO  JOB-PGID
           MOVE    "収納データ作成"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *
           MOVE    ZERO                TO  FLG-END
                                           FLG-ERR
      *
           MOVE    "TEIKI001"          TO  TEIKI01PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  TEIKI01PARA-TERMID
      *
           MOVE    "TEIKI002"          TO  TEIKI02PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  TEIKI02PARA-TERMID
      *
           MOVE    "TEIKI003"          TO  TEIKI03PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  TEIKI03PARA-TERMID
      *
           MOVE    "TEIKIERR"          TO  TEIKIERR-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  TEIKIERR-TERMID
      *
           DISPLAY "1:TEIKI01PARA = " TEIKI01PARA
           DISPLAY "1:TEIKI02PARA = " TEIKI02PARA
           DISPLAY "1:TEIKI03PARA = " TEIKI03PARA
      *
      *    定期請求収納ファイル初期化
           OPEN    OUTPUT              TEIKI01CL-FILE
           CLOSE   TEIKI01CL-FILE
      *
           OPEN    OUTPUT              TEIKI01-FILE
           OPEN    OUTPUT              TEIKI02-FILE
           OPEN    OUTPUT              TEIKI03-FILE
      *
      *    医療機関ＩＤ編集
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    ZERO                TO  ORC-DBYMD
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
           ELSE
               MOVE    "医療機関情報が取得できませんでした。"
                                           TO  WRK-TEIKIERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    患者番号構成管理情報編集
           INITIALIZE                  SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  ORC-DBYMD (1:6)
           MOVE    "01"                TO  ORC-DBYMD (7:2)
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1009-REC
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
                                           TO  WRK-TEIKIERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  100-INIT-EXT
           END-IF
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
           EVALUATE    WRK-PARA-SYOKBN
           WHEN    "1"
               PERFORM 2001-IKKATU-SYORI-SEC
               IF    ( FLG-ERR     =   1 )
                   GO  TO  200-MAIN-EXT
               END-IF
           WHEN    "2"
               PERFORM 2002-KOBETU-SYORI-SEC
               IF    ( FLG-ERR     =   1 )
                   GO  TO  200-MAIN-EXT
               END-IF
           END-EVALUATE
      *
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括処理
      *****************************************************************
       2001-IKKATU-SYORI-SEC               SECTION.
      *
           INITIALIZE    WRK-KIKAN-TBL
           MOVE    ZERO            TO  WRK-CNT
           PERFORM VARYING IDX10   FROM    1   BY  1
                   UNTIL ( IDX10   >   3 )
               IF    ( WRK-PARA-KIKAN-KBN (IDX10)  =   "1" )
      *
                   COMPUTE WRK-CNT =   WRK-CNT +   1
                   MOVE    WRK-CNT TO  WRK-KIKAN-MAX
      *
                   MOVE    WRK-PARA-KIKAN-ST (IDX10)
                                       TO  WRK-KIKANST (WRK-CNT)
                   MOVE    WRK-PARA-KIKAN-ED (IDX10)
                                       TO  WRK-KIKANED (WRK-CNT)
                   MOVE    IDX10       TO  WRK-KIKANSAKKBN (WRK-CNT)
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX10   FROM    1   BY  1
                   UNTIL ( IDX10   >   WRK-KIKAN-MAX )
      *
               MOVE    WRK-KIKANST (IDX10)
                                       TO  WRK-TEIKISTYMD
               MOVE    WRK-KIKANED (IDX10)
                                       TO  WRK-TEIKIEDYMD
               MOVE    WRK-KIKANSAKKBN (IDX10)
                                       TO  WRK-TEIKISAKKBN
      *
      *        入院履歴読み込み
               INITIALIZE              PTNYUINRRK-REC
               MOVE    SYS-1001-HOSPID     TO  PTNYUINRRK-HOSPID
               MOVE    WRK-TEIKIEDYMD      TO  PTNYUINRRK-NYUINYMD
               MOVE    WRK-TEIKIEDYMD      TO  PTNYUINRRK-TAIINYMD
               MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "PTNYUINRRK-KEY14"  TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    "PTNYUINRRK-KEY14"  TO  WRK-DBPATH
                   PERFORM 900-PTNYUINRRK-READ-SEC
               ELSE
                   INITIALIZE          PTNYUINRRK-REC
                   MOVE    1           TO  FLG-PTNYUINRRK
               END-IF
      *
               PERFORM UNTIL ( FLG-PTNYUINRRK  =   1 )
      *
      *            歴番号の最新の入院歴に読み直す
                   MOVE    "PTNYUINRRK-KEY4"
                                           TO  WRK-DBPATH
                   PERFORM 920-PTNYUINRRK-READ-SEC
                   IF    ( FLG-PTNYUINRRK2     NOT =   ZERO )
                       MOVE    PTNYUINRRK-PTID
                                       TO  ERR-PTID
                       MOVE    "入院履歴の取得に失敗しました"
                                       TO  ERR-ERRMSG
                       PERFORM 800-ERRMSG-EDIT-SEC
                       MOVE    ERR-HEN-ERRMSG
                                       TO  WRK-TEIKIERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  2001-IKKATU-SYORI-EXT
                   END-IF
      *
      *            処理対象チェック処理
                   PERFORM 20011-TAISYO-CHK-SEC
      *
                   IF    ( FLG-TAISYO  =   1 )
                       PERFORM 20011-HENSYU-SEC
                       IF    ( FLG-ERR     =   1 )
                           GO  TO  2001-IKKATU-SYORI-EXT
                       END-IF
                   END-IF
      *
                   MOVE    "PTNYUINRRK-KEY14"  TO  WRK-DBPATH
                   PERFORM 900-PTNYUINRRK-READ-SEC
      *
               END-PERFORM
      *
      *        カーソルクロース
               MOVE    "DBCLOSE"           TO  MCP-FUNC
               MOVE    "PTNYUINRRK-KEY14"  TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           END-PERFORM
           .
       2001-IKKATU-SYORI-EXT.
           EXIT. 
      *
      *****************************************************************
      *   処理対象チェック処理
      *****************************************************************
       20011-TAISYO-CHK-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-TAISYO
           MOVE    SPACE           TO  WRK-SAKKBN
           MOVE    ZERO            TO  FLG-GETUMATUSITEI
      *
      *    既に退院している患者は対象外とする
           IF    ( PTNYUINRRK-TAIINYMD
                               NOT =   "99999999" )
               GO  TO  20011-TAISYO-CHK-EXT
           END-IF
      *
      *    患者定期請求履歴読み込み処理
           PERFORM 900-PTTEIKIRRK-READ-SEC
           IF    ( FLG-PTTEIKIRRK  =   ZERO )
               IF    ( PTTEIKIRRK-SAKKBN   =   "9" )
                   MOVE    1       TO  FLG-GETUMATUSITEI
               END-IF
           END-IF
      *
           IF    ( PTNYUINRRK-TEIKI-SEIKYUKBN
                                           =   "2" )
               MOVE    1           TO  FLG-GETUMATUSITEI
           END-IF
      *
           IF    ( FLG-GETUMATUSITEI       =   1 )
      *
      *        今回処理が月末日を含む場合は処理対象とする
               MOVE    WRK-PARA-SRYYM  TO  WRK-YMD
               PERFORM 20021-GETUMATU-GET-SEC
               IF    ( WRK-YMD             =   WRK-TEIKIEDYMD )
                   MOVE    1           TO  FLG-TAISYO
               END-IF
           ELSE
               MOVE    1               TO  FLG-TAISYO
           END-IF
      *
           IF    ( FLG-TAISYO              =   ZERO )
               GO  TO  20011-TAISYO-CHK-EXT
           END-IF
      *
           IF    ( FLG-GETUMATUSITEI       =   ZERO )
               MOVE    WRK-TEIKISAKKBN TO  WRK-SAKKBN
           ELSE
               MOVE    "9"             TO  WRK-SAKKBN
           END-IF
      *
           .
       20011-TAISYO-CHK-EXT.
           EXIT. 
      *****************************************************************
      *   編集処理
      *****************************************************************
       20011-HENSYU-SEC                SECTION.
      *
      *    患者情報読み込み
           MOVE    SYS-1001-HOSPID     TO  PTINF-HOSPID
           MOVE    PTNYUINRRK-PTID     TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF    ( FLG-PTINF       NOT =   ZERO )
               MOVE    PTNYUINRRK-PTID
                                       TO  ERR-PTID
               MOVE    "患者情報の取得に失敗しました"
                                       TO  ERR-ERRMSG
               PERFORM 800-ERRMSG-EDIT-SEC
               MOVE    ERR-HEN-ERRMSG  TO  WRK-TEIKIERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  20011-HENSYU-EXT
           END-IF
      *
      *    テスト患者のときは読み飛ばす
           IF    ( PTINF-TSTPTNUMKBN   =   "1" )
               GO  TO  20011-HENSYU-EXT
           END-IF
      *
      *    収納データ取得処理
           PERFORM 20011-SYUNOU-GET-SEC
           IF (  FLG-ERR   =   1 )
               GO  TO  20011-HENSYU-EXT
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >    10 )
                   OR    ( LNK-SYU-SKYSTYMD (IDX1) =   SPACE )
      *
      *        収納データを定期収納ファイルに出力
               PERFORM 20011-TEIKI01-OUTPUT-SEC
               IF (  FLG-ERR   =   1 )
                   GO  TO  20011-HENSYU-EXT
               END-IF
      *
      *        患者請求履歴データを患者請求履歴ファイルに出力
               PERFORM 20011-TEIKI02-OUTPUT-SEC
               IF (  FLG-ERR   =   1 )
                   GO  TO  20011-HENSYU-EXT
               END-IF
      *
           END-PERFORM
      *
      *    患者請求期間情報を患者請求期間情報ファイルに出力
           PERFORM 20011-TEIKI03-OUTPUT-SEC
           IF (  FLG-ERR   =   1 )
               GO  TO  20011-HENSYU-EXT
           END-IF
      *
           .
       20011-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    収納データ取得処理
      *****************************************************************
       20011-SYUNOU-GET-SEC            SECTION.
      *
      * ----   請求開始日を取得 -----
           MOVE    SPACE           TO  WRK-SEIKYUSTYMD
           IF    ( WRK-SAKKBN      =   "9" )
               IF    ( WRK-PARA-SYOKBN =   "1" )
                   MOVE    WRK-PARA-SRYYM (1 : 6)
                                   TO  WRK-YMD (1 : 6)
                   MOVE    "01"    TO  WRK-YMD (7 : 2)
               ELSE
                   MOVE    SYS-5012-SRYYM
                                   TO  WRK-YMD (1 : 6)
                   MOVE    "01"    TO  WRK-YMD (7 : 2)
               END-IF
           ELSE
               MOVE    WRK-TEIKISTYMD
                                   TO  WRK-YMD
           END-IF
      *
           IF  ( PTNYUINRRK-NYUINYMD   >   WRK-YMD )
               MOVE    PTNYUINRRK-NYUINYMD
                                   TO  WRK-SEIKYUSTYMD
           ELSE
               MOVE    WRK-YMD     TO  WRK-SEIKYUSTYMD
           END-IF
      * ------------------------------
           MOVE    WRK-TEIKIEDYMD  TO  WRK-SEIKYUEDYMD
      *
      *    通算日数を求める
           MOVE    PTNYUINRRK-NYUINYMD
                                   TO  WRK-STYMD
      *    暫定的に月初め又は入院日を設定（実際は請求開始日）
           MOVE    WRK-SEIKYUSTYMD
                                   TO  WRK-EDYMD
           PERFORM 5002-KIKAN-CALC-SEC
           IF    ( FLG-ORCSDAY     NOT =   ZERO )
               MOVE    PTNYUINRRK-PTID
                                       TO  ERR-PTID
               MOVE    "通算日数の取得に失敗しました"
                                       TO  ERR-ERRMSG
               PERFORM 800-ERRMSG-EDIT-SEC
               MOVE    ERR-HEN-ERRMSG  TO  WRK-TEIKIERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  20011-SYUNOU-GET-EXT
           END-IF
      *
           MOVE    SPACE           TO  LNK-ORCS02-REC
                                       LNK-SYUNOU-REC-T
      *
           INITIALIZE                  LNK-ORCS02-REC
                                       LNK-SYUNOU-REC-T
      *
           MOVE    SYS-1001-HOSPID TO  LNK-S02-HOSPID
           MOVE    "1"             TO  LNK-S02-NYUGAIKBN
           MOVE    PTNYUINRRK-PTID TO  LNK-S02-PTID
           MOVE    WRK-NISSU1      TO  LNK-S02-NYUIN-TSUSAN-NISSU
      *    暫定的に月初め又は入院日を設定（実際は請求開始日）
           MOVE    WRK-SEIKYUSTYMD
                                   TO  LNK-S02-SKYSTYMD
           MOVE    WRK-SEIKYUEDYMD
                                   TO  LNK-S02-SKYEDYMD
           MOVE    "A"             TO  LNK-S02-TEIKI-KBN
           MOVE    WRK-PARA-SYSYMD TO  LNK-S02-SYSYMD
           CALL    "ORCSTEIKI"     USING
                                   SPA-AREA
                                   LNK-ORCS02-REC
                                   LNK-SYUNOU-REC-T
           IF    ( LNK-S02-RC      NOT = ZERO )
               MOVE    PTNYUINRRK-PTID
                                       TO  ERR-PTID
               EVALUATE LNK-S02-RC
               WHEN    13
                   MOVE    "会計情報の取得に失敗しました"
                                       TO  ERR-ERRMSG
               WHEN    OTHER
                   MOVE    "収納データの取得に失敗しました"
                                       TO  ERR-ERRMSG
               END-EVALUATE
               PERFORM 800-ERRMSG-EDIT-SEC
               MOVE    ERR-HEN-ERRMSG  TO  WRK-TEIKIERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  20011-SYUNOU-GET-EXT
           END-IF
      *
           PERFORM VARYING IDX1   FROM    1   BY  1
                   UNTIL ( IDX1   >   10 )
      *
               DISPLAY "LNK-SYU-SKYMONEY (" IDX1" ) = "
                        LNK-SYU-SKYMONEY ( IDX1 )
      *
           END-PERFORM
      *
           .
       20011-SYUNOU-GET-EXT.
           EXIT. 
      *****************************************************************
      *    定期収納ファイル出力処理
      *****************************************************************
       20011-TEIKI01-OUTPUT-SEC      SECTION.
      *
           MOVE    LNK-SYUNOU-REC (IDX1)
                                       TO  SYUNOU-REC
      *
           MOVE    PTNYUINRRK-REC      TO  WKPTNYUINRRK-REC
      *    請求時最終履歴取得処理
           PERFORM 20011-SEILASTRRK-SET-SEC
           IF (  FLG-ERR   =   1 )
               GO  TO  20011-TEIKI01-OUTPUT-EXT
           END-IF
           MOVE    WKPTNYUINRRK-REC    TO  PTNYUINRRK-REC
      *
           MOVE    WRK-NYUINKA         TO  SYU-SRYKA
           MOVE    WRK-BTUNUM          TO  SYU-ROOMNUM
           MOVE    WRK-BRMNUM          TO  SYU-BYOTONUM
           MOVE    WRK-DRCD            TO  SYU-DRCD
      *
      *
           MOVE    WRK-PARA-TERMID     TO  SYU-TERMID
           MOVE    SYS-YMD             TO  SYU-CREYMD
                                           SYU-UPYMD
           MOVE    SYS-TIME            TO  SYU-UPHMS
      *
           MOVE    SYU-HOSPID          TO  TEIKI01-HOSPID
           MOVE    SYU-PTID            TO  TEIKI01-PTID
           MOVE    SYU-SKYSTYMD        TO  TEIKI01-SKYSTYMD
           MOVE    SYU-SKYEDYMD        TO  TEIKI01-SKYEDYMD
           MOVE    SYU-HKNCOMBINUM     TO  TEIKI01-HKNCOMBINUM
      *
           MOVE    SYUNOU-REC          TO  TEIKI01-DAT
      *
           WRITE   TEIKI01-REC
      *
           COMPUTE CNT-OUT1    =   CNT-OUT1    +   1
      *
           .
       20011-TEIKI01-OUTPUT-EXT.
           EXIT. 
      *****************************************************************
      *    患者請求履歴ファイル出力処理
      *****************************************************************
       20011-TEIKI02-OUTPUT-SEC      SECTION.
      *
           INITIALIZE                  PTTEIKIRRK-REC
      *
           MOVE    SYU-SRYYMD (1 : 6)  TO  PTTEIKIRRK-SRYYM
           MOVE    SYU-HOSPID          TO  PTTEIKIRRK-HOSPID
           MOVE    SYU-PTID            TO  PTTEIKIRRK-PTID
           MOVE    WRK-SAKKBN          TO  PTTEIKIRRK-SAKKBN
           MOVE    SYU-HKNCOMBINUM     TO  PTTEIKIRRK-HKNCOMBINUM
           MOVE    WRK-SEIKYUSTYMD     TO  PTTEIKIRRK-SKYSTYMD
           MOVE    WRK-SEIKYUEDYMD     TO  PTTEIKIRRK-SKYEDYMD
           MOVE    SYU-TOTAL-HKNTEN    TO  PTTEIKIRRK-SKYTEN
           MOVE    SYU-SKYMONEY        TO  PTTEIKIRRK-SKYMONEY
      *
           MOVE    WRK-PARA-TERMID     TO  PTTEIKIRRK-TERMID
           MOVE    SYS-YMD             TO  PTTEIKIRRK-CREYMD
                                           PTTEIKIRRK-UPYMD
           MOVE    SYS-TIME            TO  PTTEIKIRRK-UPHMS
      *
           MOVE    PTTEIKIRRK-REC      TO  TEIKI02-REC
      *
           WRITE   TEIKI02-REC
      *
           COMPUTE CNT-OUT2    =   CNT-OUT2    +   1
      *
           .
       20011-TEIKI02-OUTPUT-EXT.
           EXIT. 
      *****************************************************************
      *    患者請求期間情報ファイル出力処理
      *****************************************************************
       20011-TEIKI03-OUTPUT-SEC        SECTION.
      *
           INITIALIZE                  TEIKI03-REC
           MOVE    PTNYUINRRK-HOSPID   TO  TEIKI03-HOSPID
           MOVE    PTNYUINRRK-PTID     TO  TEIKI03-PTID
           MOVE    WRK-SEIKYUSTYMD     TO  TEIKI03-SKYSTYMD
           MOVE    WRK-SEIKYUEDYMD     TO  TEIKI03-SKYEDYMD
      *
           WRITE   TEIKI03-REC
      *
           COMPUTE CNT-OUT3    =   CNT-OUT3    +   1
      *
           .
       20011-TEIKI03-OUTPUT-EXT.
           EXIT. 
      *****************************************************************
      *    請求時最終履歴取得処理
      *****************************************************************
       20011-SEILASTRRK-SET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINKA
           MOVE    SPACE               TO  WRK-NYUINKA
                                           WRK-BTUNUM
                                           WRK-BRMNUM
                                           WRK-DRCD
      *
      *    入院履歴マスタ検索
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "PTNYUINRRK-KEY4"   TO  ORC-DBPATH
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF    ( MCP-RC          =   ZERO )
               MOVE    "PTNYUINRRK-KEY4"  TO  ORC-DBPATH
               PERFORM 910-PTNYUINRRK-READ-SEC
           ELSE
               INITIALIZE          PTNYUINRRK-REC
               MOVE    1           TO  FLG-PTNYUINRRK2
           END-IF
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK2 NOT =   ZERO )
                    OR   ( FLG-NYUINKA =   1 )
      *
               IF    ( PTNYUINRRK-TENNYUYMD
                                       <=  SYU-SKYEDYMD )
                AND  ( PTNYUINRRK-TENSTUYMD
                                       >=  SYU-SKYEDYMD )
                   MOVE    1           TO  FLG-NYUINKA
                   MOVE    PTNYUINRRK-NYUINKA
                                       TO  WRK-NYUINKA
                   MOVE    PTNYUINRRK-BTUNUM
                                       TO  WRK-BTUNUM
                   MOVE    PTNYUINRRK-BRMNUM
                                       TO  WRK-BRMNUM
                   MOVE    PTNYUINRRK-DRCD1 (2 : 4)
                                       TO  WRK-DRCD
               END-IF
      *
               MOVE    "PTNYUINRRK-KEY4"  TO  ORC-DBPATH
               PERFORM 910-PTNYUINRRK-READ-SEC
      *
           END-PERFORM
      *
           MOVE    "PTNYUINRRK-KEY4"   TO  ORC-DBPATH
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
           IF    ( FLG-NYUINKA         =   ZERO )
               MOVE    PTNYUINRRK-PTID
                                       TO  ERR-PTID
               MOVE    "入院履歴の取得に失敗しました"
                                       TO  ERR-ERRMSG
               PERFORM 800-ERRMSG-EDIT-SEC
               MOVE    ERR-HEN-ERRMSG
                                       TO  WRK-TEIKIERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  20011-SEILASTRRK-SET-EXT
           END-IF
      *
      *
           .
       20011-SEILASTRRK-SET-EXT.
           EXIT.
      *****************************************************************
      *    個別処理
      *****************************************************************
       2002-KOBETU-SYORI-SEC               SECTION.
      *
      *    定期請求指示（個別指示）コード読み込み
           INITIALIZE                      SYS-5012-REC
           MOVE    "5012"              TO  SYS-5012-KANRICD
           MOVE    ZERO                TO  ORC-DBYMD
           MOVE    SYS-5012-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY7"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               PERFORM 900-SYS5012-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYS5012
           END-IF
      *
           PERFORM 20021-KOBETU-HENSYU-SEC
                              UNTIL       FLG-SYS5012     =   1
                              OR          FLG-ERR         =   1
      *    カーソルクロース
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY7"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           .
      *
       2002-KOBETU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    個別編集処理
      *****************************************************************
       20021-KOBETU-HENSYU-SEC              SECTION.
      *
      *    請求期間取得処理
           PERFORM 20021-SEIKYU-KIKAN-GET-SEC
           IF    ( FLG-ERR         =   1 )
               GO  TO  20021-KOBETU-HENSYU-EXT
           END-IF
      *
           PERFORM VARYING IDX10   FROM    1   BY  1
                   UNTIL ( IDX10   >   WRK-KIKAN-MAX )
      *
               MOVE    WRK-KIKANST (IDX10)
                                       TO  WRK-TEIKISTYMD
               MOVE    WRK-KIKANED (IDX10)
                                       TO  WRK-TEIKIEDYMD
               MOVE    WRK-KIKANSAKKBN (IDX10)
                                       TO  WRK-SAKKBN
      *        入院履歴読み込み
               INITIALIZE              PTNYUINRRK-REC
               MOVE    SYS-1001-HOSPID     TO  PTNYUINRRK-HOSPID
               MOVE    SYS-5012-PTID       TO  PTNYUINRRK-PTID
               MOVE    WRK-TEIKIEDYMD      TO  PTNYUINRRK-NYUINYMD
               MOVE    WRK-TEIKIEDYMD      TO  PTNYUINRRK-TAIINYMD
               MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "PTNYUINRRK-KEY10"  TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    "PTNYUINRRK-KEY10"  TO  WRK-DBPATH
                   PERFORM 900-PTNYUINRRK-READ-SEC
               ELSE
                   INITIALIZE          PTNYUINRRK-REC
                   MOVE    1           TO  FLG-PTNYUINRRK
               END-IF
      *
      *        カーソルクロース
               MOVE    "DBCLOSE"           TO  MCP-FUNC
               MOVE    "PTNYUINRRK-KEY10"  TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
               DISPLAY "WRK-TEIKISTYMD = " WRK-TEIKISTYMD
               DISPLAY "WRK-TEIKIEDYMD = " WRK-TEIKIEDYMD
               DISPLAY "20021-KOBETU-HENSYU-SEC = " FLG-PTNYUINRRK
               IF    ( FLG-PTNYUINRRK  =   ZERO )
                   PERFORM 20011-HENSYU-SEC
                   IF    ( FLG-ERR     =   1 )
                       GO  TO  20021-KOBETU-HENSYU-EXT
                   END-IF
               END-IF
           END-PERFORM
      *
           PERFORM 900-SYS5012-READ-SEC
      *
           .
       20021-KOBETU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求期間取得処理
      *****************************************************************
       20021-SEIKYU-KIKAN-GET-SEC      SECTION.
      *
           INITIALIZE  WRK-KIKAN-TBL
           MOVE        ZERO            TO  WRK-CNT
      *
      *    作成区分が“９”の場合、月末時のみ指定
           IF    ( SYS-5012-SAKKBN     =   "9" )
               MOVE    1               TO  WRK-KIKAN-MAX
               MOVE    SYS-5012-SRYYM  (1 : 6)
                                       TO  WRK-KIKANST (1)(1 : 6)
               MOVE    "01"            TO  WRK-KIKANST (1)(7 : 2)
               MOVE    SYS-5012-SRYYM      TO  WRK-YMD
               PERFORM 20021-GETUMATU-GET-SEC
               MOVE    WRK-YMD     TO  WRK-KIKANED (1)
               MOVE    9               TO  WRK-KIKANSAKKBN (1)
               GO  TO  20021-SEIKYU-KIKAN-GET-EXT
           END-IF
      *
      *    請求期間編集
           INITIALIZE                      SYS-5010-REC
           MOVE    "5010"              TO  SYS-5010-KANRICD
           MOVE    "*"                 TO  SYS-5010-KBNCD
           MOVE    SYS-5012-SRYYM      TO  ORC-DBYMD (1 : 6)
           MOVE    "01"                TO  ORC-DBYMD (7 : 2)
      *
           MOVE    SYS-5010-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-5010-REC
           ELSE
               MOVE    "定期請求管理情報の取得に失敗しました"
                                           TO  WRK-TEIKIERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  20021-SEIKYU-KIKAN-GET-EXT
           END-IF
      *
           IF    ( SYS-5012-SAKKBN =   "0" OR  "1" )
               COMPUTE WRK-CNT =   WRK-CNT +   1
               MOVE    WRK-CNT TO  WRK-KIKAN-MAX
               MOVE    SYS-5012-SRYYM  (1 : 6)
                                       TO  WRK-KIKANST (WRK-CNT)(1 : 6)
               MOVE    "01"            TO  WRK-KIKANST (WRK-CNT)(7 : 2)
               IF    ( SYS-5010-SEIKYU-DAY1    =   99 )
                   MOVE    SYS-5012-SRYYM      TO  WRK-YMD
                   PERFORM 20021-GETUMATU-GET-SEC
                   MOVE    WRK-YMD     TO  WRK-KIKANED (WRK-CNT)
               ELSE
                   MOVE    SYS-5012-SRYYM (1 : 6)
                                       TO  WRK-KIKANED (WRK-CNT)(1 : 6)
                   MOVE    SYS-5010-SEIKYU-DAY1
                                       TO  WRK-KIKANED (WRK-CNT)(7 : 2)
               END-IF
               MOVE    1               TO  WRK-KIKANSAKKBN (WRK-CNT)
           END-IF
      *
           IF    ( SYS-5012-SAKKBN =   "0" OR  "2" )
               COMPUTE WRK-CNT =   WRK-CNT +   1
               MOVE    WRK-CNT TO  WRK-KIKAN-MAX
               MOVE    SYS-5012-SRYYM (1 : 6)  TO  WRK-YMD (1 : 6)
               MOVE    SYS-5010-SEIKYU-DAY1    TO  WRK-YMD (7 : 2)
               PERFORM 20021-YOKUZITU-GET-SEC
               MOVE    WRK-YMD         TO  WRK-KIKANST (WRK-CNT)
      *
               IF    ( SYS-5010-SEIKYU-DAY2    =   99 )
                   MOVE    SYS-5012-SRYYM      TO  WRK-YMD
                   PERFORM 20021-GETUMATU-GET-SEC
                   MOVE    WRK-YMD     TO  WRK-KIKANED (WRK-CNT)
               ELSE
                   MOVE    SYS-5012-SRYYM (1 : 6)
                                       TO  WRK-KIKANED (WRK-CNT)(1 : 6)
                   MOVE    SYS-5010-SEIKYU-DAY2
                                       TO  WRK-KIKANED (WRK-CNT)(7 : 2)
               END-IF
               MOVE    2               TO  WRK-KIKANSAKKBN (WRK-CNT)
           END-IF
      *
           IF    ( SYS-5012-SAKKBN =   "0" OR  "3" )
               COMPUTE WRK-CNT =   WRK-CNT +   1
               MOVE    WRK-CNT TO  WRK-KIKAN-MAX
               MOVE    SYS-5012-SRYYM (1 : 6)  TO  WRK-YMD (1 : 6)
               MOVE    SYS-5010-SEIKYU-DAY2    TO  WRK-YMD (7 : 2)
               PERFORM 20021-YOKUZITU-GET-SEC
               MOVE    WRK-YMD         TO  WRK-KIKANST (WRK-CNT)
      *
               IF    ( SYS-5010-SEIKYU-DAY3    =   99 )
                   MOVE    SYS-5012-SRYYM      TO  WRK-YMD
                   PERFORM 20021-GETUMATU-GET-SEC
                   MOVE    WRK-YMD     TO  WRK-KIKANED (WRK-CNT)
               ELSE
                   MOVE    SYS-5012-SRYYM (1 : 6)
                                       TO  WRK-KIKANED (WRK-CNT)(1 : 6)
                   MOVE    SYS-5010-SEIKYU-DAY3
                                       TO  WRK-KIKANED (WRK-CNT)(7 : 2)
               END-IF
               MOVE    3               TO  WRK-KIKANSAKKBN (WRK-CNT)
           END-IF
      *
           IF    ( WRK-KIKAN-MAX   <=  ZERO )
               MOVE    "定期期間の取得に失敗しました"
                                           TO  WRK-TEIKIERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  20021-SEIKYU-KIKAN-GET-EXT
           END-IF
      *
           .
       20021-SEIKYU-KIKAN-GET-EXT.
            EXIT.
      *****************************************************************
      *    月末日取得処理
      *****************************************************************
       20021-GETUMATU-GET-SEC      SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-YMD (1 : 6)     TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-YMD
      *
           .
       20021-GETUMATU-GET-EXT.
            EXIT.
      *****************************************************************
      *    翌日取得処理
      *****************************************************************
       20021-YOKUZITU-GET-SEC      SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    1                   TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                           LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-YMD
      *
           .
       20021-YOKUZITU-GET-EXT.
            EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   TEIKIERR-FILE
           IF      STS-TEIKIERR         =   ZERO
               CLOSE   TEIKIERR-FILE
           ELSE
               OPEN    OUTPUT              TEIKIERR-FILE
      *
               MOVE    WRK-TEIKIERR         TO  TEIKIERR-REC
               WRITE   TEIKIERR-REC
               CLOSE   TEIKIERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-TEIKIERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
           END-IF
      *                             
           MOVE    1                   TO  FLG-ERR
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   TEIKI01-FILE
                   TEIKI02-FILE
                   TEIKI03-FILE
      *
           IF      CNT-OUT1            =   ZERO 
               MOVE    "処理対象のデータがありませんでした"
                                           TO  WRK-TEIKIERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *     
           DISPLAY "TEIKI01 (ORCT0010) CNT " CNT-OUT1 
           DISPLAY "*** ORCT0010 END ***"
      ***  ACCEPT  WRK-IN FROM CONSOLE 
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    期間算出処理
      *****************************************************************
       5002-KIKAN-CALC-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-ORCSDAY
                                           WRK-NISSU1
                                           WRK-NISSU2
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY5-AREA
           MOVE    "51"                TO  LNK-DAY5-IRAI
           MOVE    WRK-STYMD           TO  LNK-DAY5-START
           MOVE    WRK-EDYMD           TO  LNK-DAY5-END
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                           LNK-DAY5-AREA
           IF      ( STS-DAY-RC1   NOT =   ZERO )
               MOVE    1           TO  FLG-ORCSDAY
               GO  TO              5002-KIKAN-CALC-EXT
           END-IF
      *
           MOVE    LNK-DAY5-NISSU1     TO  WRK-NISSU1
           MOVE    LNK-DAY5-NISSU2     TO  WRK-NISSU2
      *
           .
       5002-KIKAN-CALC-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       800-ERRMSG-EDIT-SEC             SECTION.
      *
           MOVE    SPACE               TO  ERR-HEN-ERRMSG
	                                   ERR-PTNUM
      *
      *    患者番号を取得
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SYS-1001-HOSPID     TO  SPTID-HOSPID
           MOVE    ERR-PTID            TO  SPTID-PTID
           CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                           SPA-1009-TBL
           IF    ( SPTID-RC            =   ZERO )
               MOVE    SPTID-PTNUM     TO  ERR-PTNUM
           ELSE
               STRING "?(ID:"          DELIMITED   BY  SIZE
                      ERR-PTID         DELIMITED   BY  SIZE
                      ")"              DELIMITED   BY  SIZE
               INTO   ERR-PTNUM
               END-STRING
           END-IF
      *
           STRING  ERR-ERRMSG          DELIMITED   BY  SPACE
                   "[患者番号:"        DELIMITED   BY  SIZE
                   ERR-PTNUM           DELIMITED   BY  SPACE
                   "]"                 DELIMITED   BY  SIZE
           INTO    ERR-HEN-ERRMSG
           END-STRING
      *
           .
       800-ERRMSG-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYS5012-READ-SEC         SECTION.
      *
           MOVE    "SYSKANRI-KEY7"     TO  ORC-DBPATH
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           DISPLAY "SYS5012 MCP-RC = " MCP-RC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYS5012
               MOVE    MCPDATA-REC     TO  SYS-5012-REC
           ELSE
               MOVE    1               TO  FLG-SYS5012
           END-IF
           .
       900-SYS5012-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
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
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
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
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    入院履歴マスタ　ＲＥＡＤ
      *****************************************************************
       900-PTNYUINRRK-READ-SEC         SECTION.
      *
           MOVE    WRK-DBPATH      TO  ORC-DBPATH
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF    ( MCP-RC          =   ZERO )
               MOVE    ZERO        TO  FLG-PTNYUINRRK
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               MOVE    1           TO  FLG-PTNYUINRRK
               GO  TO  900-PTNYUINRRK-READ-EXT
           END-IF
      *
           .
       900-PTNYUINRRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者定期請求履歴マスタ　ＲＥＡＤ
      *****************************************************************
       900-PTTEIKIRRK-READ-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-PTTEIKIRRK
           INITIALIZE                  PTTEIKIRRK-REC
      *
           MOVE    WRK-PARA-SRYYM      TO  PTTEIKIRRK-SRYYM
           MOVE    PTNYUINRRK-HOSPID   TO  PTTEIKIRRK-HOSPID
           MOVE    PTNYUINRRK-PTID     TO  PTTEIKIRRK-PTID
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTTEIKIRRK-KEY2"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "PTTEIKIRRK-KEY2"
                                       TO  ORC-DBPATH
               CALL    "ORCMCPSUB"     USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF    ( MCP-RC      =   ZERO )
                   MOVE    MCPDATA-REC
                                   TO  PTTEIKIRRK-REC
               ELSE
                   MOVE    1       TO  FLG-PTTEIKIRRK
               END-IF
           ELSE
               MOVE    1           TO  FLG-PTTEIKIRRK
           END-IF
      *
      *    カーソルクロース
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTTEIKIRRK-KEY2"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       900-PTTEIKIRRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴マスタ　ＲＥＡＤ
      *****************************************************************
       910-PTNYUINRRK-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-PTNYUINRRK2
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK2
               GO  TO  910-PTNYUINRRK-READ-EXT
           END-IF
      *
           .
       910-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴マスター読込
      *****************************************************************
       920-PTNYUINRRK-READ-SEC     SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNYUINRRK2
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-DBPATH          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "DBFETCH"   TO  MCP-FUNC
               MOVE    WRK-DBPATH  TO  ORC-DBPATH
               CALL    "ORCMCPSUB"     USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF    ( MCP-RC      =   ZERO )
                   MOVE    MCPDATA-REC
                                   TO  PTNYUINRRK-REC
               ELSE
                   MOVE    1       TO  FLG-PTNYUINRRK2
               END-IF
           ELSE
               MOVE    1           TO  FLG-PTNYUINRRK2
           END-IF
      *
      *    カーソルクロース
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    WRK-DBPATH          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       920-PTNYUINRRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者マスター読込（主キー）
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
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
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
       900-DBCLOSE-EXT.
           EXIT.
