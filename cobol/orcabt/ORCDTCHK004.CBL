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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCDTCHK004.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データチェック
      *  コンポーネント名  : 診療会計情報抽出
      *  管理者            :
      *  作成日付    作業者        記述
      *  05/03/31    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
      *
      *    データチェック中間ファイル（受診患者）
           SELECT  DTCHK002-FILE   ASSIGN  DTCHK002PARA
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-DTCHK002.
      *
      *    データチェック中間ファイル（診療会計:入院）
           SELECT  DTCHK005N-FILE
                                   ASSIGN  DTCHK005NPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  DTCHK005N-KEY
                                   FILE    STATUS  IS  STS-DTCHK005.
      *
      *    データチェック中間ファイル（診療会計:外来）
           SELECT  DTCHK005G-FILE
                                   ASSIGN  DTCHK005GPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  DTCHK005G-KEY
                                   FILE    STATUS  IS  STS-DTCHK005.
      *
      *    データチェック診療会計
           SELECT  DCACCT-FILE
                                   ASSIGN  DTCHKACCTPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DCACCT-KEY
                                   FILE    STATUS  IS  STS-DCACCT.
      *
      *
      *    エラーファイル
           SELECT  DTCHKERR-FILE   ASSIGN  DTCHKERRPARA
                                   FILE    STATUS  IS  STS-DTCHKERR.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *    データチェック中間ファイル（受診患者）
       FD  DTCHK002-FILE.
           COPY    "CPDTCHK002.INC".
      *
      *    データチェック中間ファイル（診療会計:入院）
       FD  DTCHK005N-FILE.
           COPY    "CPDTCHK005.INC"
                                   REPLACING  //DTCHK005//
                                   BY         //DTCHK005N//.
      *
      *    データチェック中間ファイル（診療会計:外来）
       FD  DTCHK005G-FILE.
           COPY    "CPDTCHK005.INC"
                                   REPLACING  //DTCHK005//
                                   BY         //DTCHK005G//.
      *
      *
      *    データチェック診療会計
       FD  DCACCT-FILE.
       01  DCACCT-REC.
           COPY    "CPDCACCT.INC".
      *
      *    エラーファイル
       FD  DTCHKERR-FILE.
       01  DTCHKERR-REC                PIC X(200). 
      *
       WORKING-STORAGE                 SECTION.
      *
      *    エラーファイルファイル名称領域 
           COPY    "CPCOMMONDAT6.INC"
                                   REPLACING  //DTCHK//
                                   BY         //DTCHKERR//.
      *
      *    中間ファイルファイル名称領域 
           COPY    "CPCOMMONDAT6.INC"
                                   REPLACING  //DTCHK//
                                   BY         //DTCHK002//.
           COPY    "CPCOMMONDAT6.INC"
                                   REPLACING  //DTCHK//
                                   BY         //DTCHK005N//.
           COPY    "CPCOMMONDAT6.INC"
                                   REPLACING  //DTCHK//
                                   BY         //DTCHK005G//.
           COPY    "CPCOMMONDAT6.INC"
                                   REPLACING  //DTCHK//
                                   BY         //DTCHKACCT//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-DTCHK002                            PIC X(02).
           03  STS-DTCHK005                            PIC X(02).
           03  STS-DTCHKERR                            PIC X(02).
           03  STS-DCACCT                              PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-ERR                                 PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-DTCHK002                            PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-DCACCT                              PIC 9(08).
      *
       01  IDX-AREA.
           03  IDX0                                    PIC 9(05).
           03  IDX-COMB                                PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-SYMD                                PIC X(08).
           03  WRK-SRYYMD.
               05  WRK-SRYYM                           PIC X(06).
               05  WRK-SRYDAY                          PIC 9(02).
           03  WRK-DTCHKERR                            PIC X(200).
           03  WRK-JOB-YOBI                            PIC X(500).
           03  WRK-HKNCOMBI                            PIC 9(04).
      *
       01  KEY-AREA.
           03  KEY-PTID                                PIC 9(10).
           03  KEY-HKNCOMBI                            PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID                          PIC 9(07).
           03  WRK-PARA-SHELLID                        PIC X(08).
           03  WRK-PARA-HOSPID                         PIC X(24).
      *
      *    データチェック一括処理パラメータ
           COPY    "CPORCBSD01PARA.INC".
      *
           COPY    "CPDTCHK005.INC".
      *
      *    固定値領域
      *01  CONST-AREA.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    保険組合せ取得サブ
           COPY    "CPORCSDTCHK02.INC".
           01  LNK-HKNCOMBI-AREA.
             02  LNK-HKNCOMBI-OCC      OCCURS  1000.
           COPY    "CPHKNCOMBI.INC"    REPLACING  //COMB-//
                                       BY         //LNK-COMB-//.
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      ****************************************************************
       LINKAGE                         SECTION.
       01  DTCHK-RC        PIC S9(09).
       01  COMMAND-PARAM.
           02  FILLER      PIC X(2000).
      ****************************************************************
       PROCEDURE                       DIVISION
               USING
           DTCHK-RC
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           DISPLAY "*** ORCDTCHK004 START ***"
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           MOVE    FLG-ERR         TO  DTCHK-RC
      *
           EXIT    PROGRAM
      *
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  STS-AREA
                                       IDX-AREA
                                       CNT-AREA
                                       FLG-AREA
                                       WRK-AREA
                                       WRK-PARA
                                       KEY-AREA
                                       ORCBSD01PARA
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
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPID
                                               DTCHKERRPARA
                                               ORCBSD01-IKTKBT
                                               ORCBSD01-SRYYM
                                               ORCBSD01-NYUGAIKBN
                                               ORCBSD01-CHKKBN-G
                                               ORCBSD01-SYAHOPROCKBN
                                               ORCBSD01-KOKUHOPROCKBN
                                               ORCBSD01-RSIPROCKBN
                                               ORCBSD01-JBIPROCKBN
                                               ORCBSD01-JIHIPROCKBN
                                               ORCBSD01-INGAIPROCKBN
                                               ORCBSD01-PRTSRTKBN
                                               ORCBSD01-PRTPTLSTKBN
                                               ORCBSD01-SRYYMDKBN
                                               ORCBSD01-STSRYYMD
                                               ORCBSD01-EDSRYYMD
      *
           PERFORM 900-DBSTART-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCDTCHK004"       TO  JOB-PGID
           MOVE    "診療会計情報抽出"  TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
      *
           MOVE    ZERO                TO  FLG-END
                                           FLG-ERR
      *
      *    データチェック中間ファイル名
           MOVE    "DTCHK002"          TO  DTCHK002-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK002-TERMID
      *
           MOVE    "DTCHK05N"          TO  DTCHK005N-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK005N-TERMID
      *
           MOVE    "DTCHK05G"          TO  DTCHK005G-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK005G-TERMID
      *
           MOVE    "DCACCT00"          TO  DTCHKACCT-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHKACCT-TERMID
      *
      *    初期値設定処理
           PERFORM 1001-INIT-VAL-SET-SEC
      *
           PERFORM 900-DBCOMMIT-SEC
      *
           PERFORM 900-DBSTART-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期値設定処理
      *****************************************************************
       1001-INIT-VAL-SET-SEC           SECTION.
      *
           MOVE    ORCBSD01-SRYYM      TO  WRK-SYMD
           MOVE    "01"                TO  WRK-SYMD(7:2)
      *
           .
       1001-INIT-VAL-SET-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYACCT
      *
      *    診療会計編集処理
           IF    ( ORCBSD01-IKTKBT     =   "2" )
            OR   ( ORCBSD01-SRYYMDKBN  =   "1" )
               PERFORM 2002-SRYACCT-EDIT-SEC
           ELSE
               PERFORM 2001-SRYACCT-EDIT-SEC
           END-IF
      *
      *    データチェック診療会計テーブル追加処理
           PERFORM 2001-DCACCT-INS-SEC
      *
           MOVE    1               TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    診療会計編集処理
      *****************************************************************
       2001-SRYACCT-EDIT-SEC           SECTION.
      *
           OPEN    OUTPUT  DTCHK005N-FILE
           OPEN    OUTPUT  DTCHK005G-FILE
      *
           PERFORM 900-SRYACCT-ALL2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SRYACCT  NOT =   ZERO )
      *
               PERFORM 20011-SRYACCT-EDIT-SEC
      *
               PERFORM 900-SRYACCT-ALL2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "all2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   DTCHK005N-FILE
           CLOSE   DTCHK005G-FILE
      *
           .
       2001-SRYACCT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療会計編集処理
      *****************************************************************
       20011-SRYACCT-EDIT-SEC          SECTION.
      *
           IF    ( ACCT-ZAIKAISU   >   ZERO )
               INITIALIZE                  DTCHK005-REC
               MOVE    ACCT-PTID       TO  DTCHK005-PTID
               MOVE    ACCT-ZAINUM     TO  DTCHK005-ZAINUM
               MOVE    ACCT-SRYKA      TO  DTCHK005-SRYKA
               MOVE    ACCT-SRYKBN     TO  DTCHK005-SRYKBN
               MOVE    ACCT-JIHOKBN    TO  DTCHK005-JIHOKBN
               MOVE    ACCT-HKNCOMBI   TO  DTCHK005-HKNCOMBI
               MOVE    ACCT-ZAITEN     TO  DTCHK005-ZAITEN
               MOVE    ACCT-ZAIKAISU   TO  DTCHK005-ZAIKAISU
               MOVE    ACCT-NYUGAIKBN  TO  DTCHK005-NYUGAIKBN
               MOVE    ACCT-SRYYM      TO  WRK-SRYYM
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   31 )
                   IF    ( ACCT-DAY (1 IDX0)   >   ZERO )
      *
                       COMPUTE DTCHK005-ACCTRENNUM
                           =   DTCHK005-ACCTRENNUM  +   1
      *
                       MOVE    IDX0        TO  WRK-SRYDAY
                       MOVE    WRK-SRYYMD  TO  DTCHK005-SRYYMD
                       MOVE    IDX0        TO  DTCHK005-DAY
                       MOVE    ACCT-DAY (1 IDX0)
                                           TO  DTCHK005-DAYCNT
                       IF    ( ACCT-NYUGAIKBN  =   "1" )
      *
                           MOVE    DTCHK005-REC    TO  DTCHK005N-REC
                           WRITE   DTCHK005N-REC
      *
                       ELSE
      *
                           MOVE    DTCHK005-REC    TO  DTCHK005G-REC
                           WRITE   DTCHK005G-REC
      *
                       END-IF
      *
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       20011-SRYACCT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療会計編集処理
      *****************************************************************
       2002-SRYACCT-EDIT-SEC           SECTION.
      *
           OPEN    INPUT   DTCHK002-FILE
           OPEN    OUTPUT  DTCHK005N-FILE
           OPEN    OUTPUT  DTCHK005G-FILE
      *
           PERFORM 900-DTCHK002-READ-SEC
      *
           PERFORM UNTIL ( FLG-DTCHK002    NOT =   ZERO )
      *
               PERFORM 900-SRYACCT-KEY48-SEL-SEC
      *
               PERFORM UNTIL ( FLG-SRYACCT  NOT =   ZERO )
      *
                   PERFORM 20011-SRYACCT-EDIT-SEC
      *
                   PERFORM 900-SRYACCT-KEY48-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key48"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
               PERFORM 900-DTCHK002-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   DTCHK002-FILE
           CLOSE   DTCHK005N-FILE
           CLOSE   DTCHK005G-FILE
      *
           .
       2002-SRYACCT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    データチェック診療会計テーブル追加処理
      *****************************************************************
       2001-DCACCT-INS-SEC             SECTION.
      *
           INITIALIZE                  KEY-AREA
      *
           OPEN    INPUT               DTCHK005N-FILE
           OPEN    INPUT               DTCHK005G-FILE
           OPEN    OUTPUT              DCACCT-FILE
      *
           PERFORM 900-DTCHK005N-READ-SEC
           PERFORM 900-DTCHK005G-READ-SEC
      *
           PERFORM UNTIL ( DTCHK005N-KEY   =   HIGH-VALUE )
                    AND  ( DTCHK005G-KEY   =   HIGH-VALUE )
      *
               EVALUATE    TRUE
               WHEN  ( DTCHK005N-PTID-X    >   DTCHK005G-PTID-X  )
                   IF    ( ORCBSD01-NYUGAIKBN  =   "1" )
                       PERFORM 900-DTCHK005G-READ-SEC
                   ELSE
                       MOVE    DTCHK005G-REC   TO  DTCHK005-REC
                       PERFORM 200111-DCACCT-INS-SEC
                       PERFORM 900-DTCHK005G-READ-SEC
                   END-IF
               WHEN  ( DTCHK005N-PTID-X    <   DTCHK005G-PTID-X  )
                   IF    ( ORCBSD01-NYUGAIKBN  =   "1" )
                       MOVE    DTCHK005N-REC   TO  DTCHK005-REC
                       PERFORM 200111-DCACCT-INS-SEC
                       PERFORM 900-DTCHK005N-READ-SEC
                   ELSE
                       PERFORM 900-DTCHK005N-READ-SEC
                   END-IF
               WHEN  ( DTCHK005N-PTID-X    =   DTCHK005G-PTID-X  )
      *
                   IF    ( ORCBSD01-NYUGAIKBN  =   "1" )
                       MOVE    DTCHK005G-REC   TO  DTCHK005-REC
                       PERFORM 200111-DCACCT-INS-SEC
                       PERFORM 900-DTCHK005G-READ-SEC
                   ELSE
                       MOVE    DTCHK005N-REC   TO  DTCHK005-REC
                       PERFORM 200111-DCACCT-INS-SEC
                       PERFORM 900-DTCHK005N-READ-SEC
                   END-IF
      *
               END-EVALUATE
      *
           END-PERFORM
      *
      *
           CLOSE   DTCHK005N-FILE
           CLOSE   DTCHK005G-FILE
      *
           CLOSE   DCACCT-FILE
      *
           .
       2001-DCACCT-INS-EXT.
           EXIT.
      *****************************************************************
      *    データチェック診療会計テーブル追加処理
      *****************************************************************
       200111-DCACCT-INS-SEC             SECTION.
      *
           IF    ( KEY-PTID                =   ZERO )
            OR   ( KEY-PTID            NOT =   DTCHK005-PTID )
                   PERFORM 200111-PTHKNCOMBI-GET-SEC
                   MOVE    DTCHK005-HKNCOMBI   TO  WRK-HKNCOMBI
                   PERFORM 220-HKNCOMBI-GET-SEC
      *
                   MOVE    DTCHK005-PTID       TO   KEY-PTID
                   MOVE    DTCHK005-HKNCOMBI   TO   KEY-HKNCOMBI
           ELSE
               IF    ( KEY-HKNCOMBI    NOT =   DTCHK005-HKNCOMBI )
                   MOVE    DTCHK005-HKNCOMBI   TO  WRK-HKNCOMBI
                   PERFORM 220-HKNCOMBI-GET-SEC
                   MOVE    DTCHK005-HKNCOMBI   TO   KEY-HKNCOMBI
               END-IF
           END-IF
      *
           INITIALIZE                  DCACCT-REC
           MOVE    DTCHK005-PTID       TO  DCACCT-PTID
           MOVE    DTCHK005-ZAINUM     TO  DCACCT-ZAINUM
           MOVE    DTCHK005-ACCTRENNUM TO  DCACCT-ACCTRENNUM
           MOVE    DTCHK005-SRYYMD     TO  DCACCT-SRYYMD
           MOVE    "1"                 TO  DCACCT-ACCTKBN
           MOVE    DTCHK005-SRYKA      TO  DCACCT-SRYKA
           MOVE    DTCHK005-SRYKBN     TO  DCACCT-SRYKBN
           MOVE    DTCHK005-JIHOKBN    TO  DCACCT-JIHOKBN
           MOVE    DTCHK005-HKNCOMBI   TO  DCACCT-HKNCOMBI
           MOVE    COMB-HKNNUM         TO  DCACCT-HKNNUM
           MOVE    DTCHK005-ZAITEN     TO  DCACCT-ZAITEN
           MOVE    DTCHK005-ZAIKAISU   TO  DCACCT-ZAIKAISU
           MOVE    DTCHK005-NYUGAIKBN  TO  DCACCT-NYUGAIKBN
           MOVE    DTCHK005-DAY        TO  DCACCT-DAY
           MOVE    DTCHK005-DAYCNT     TO  DCACCT-DAYCNT
      *
           PERFORM 800-MCNDATE-SEC
      *
           MOVE    SMCNDATE-YMD        TO  DCACCT-CREYMD
                                           DCACCT-UPYMD
           MOVE    SMCNDATE-HMS        TO  DCACCT-UPHMS
      *
           WRITE   DCACCT-REC
      *
           .
       200111-DCACCT-INS-EXT.
           EXIT.
      *****************************************************************
      *    患者保険組合せ情報取得
      *****************************************************************
       200111-PTHKNCOMBI-GET-SEC       SECTION.
      *
      *    保険組み合わせ
           INITIALIZE                  SDTCHK02AREA
                                       LNK-HKNCOMBI-AREA
      *
           MOVE    WRK-PARA-HOSPID     TO  SDTCHK02-HOSPID
           MOVE    DTCHK005-PTID       TO  SDTCHK02-PTID
      *
           CALL    "ORCSDTCHK02"       USING   SDTCHK02AREA
                                               LNK-HKNCOMBI-AREA
      *
           .
       200111-PTHKNCOMBI-GET-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ取得処理
      *****************************************************************
       220-HKNCOMBI-GET-SEC            SECTION.
      *
           INITIALIZE                  HKNCOMBI-REC
      *
           PERFORM VARYING IDX-COMB    FROM    1   BY  1
                   UNTIL ( IDX-COMB    >   SDTCHK02-O-MAX )
                    OR   ( COMB-HKNNUM    NOT =   SPACE )
      *
               IF    ( LNK-COMB-HKNCOMBINUM (IDX-COMB)
                                  =    WRK-HKNCOMBI )
                   MOVE    LNK-HKNCOMBI-OCC (IDX-COMB)
                                  TO   HKNCOMBI-REC
               END-IF
      *
           END-PERFORM
      *
           .
       220-HKNCOMBI-GET-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC              SECTION.
      *
           OPEN    INPUT   DTCHKERR-FILE
           IF    ( STS-DTCHKERR        =   ZERO )
               CLOSE   DTCHKERR-FILE
           ELSE
               OPEN    OUTPUT              DTCHKERR-FILE
      *
               MOVE    WRK-DTCHKERR    TO  DTCHKERR-REC
               WRITE   DTCHKERR-REC
               CLOSE   DTCHKERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-DTCHKERR    TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
           END-IF
      *
           MOVE    1                   TO  FLG-END
           MOVE    1                   TO  FLG-ERR
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    ZERO            TO  JOB-UPDCNT
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
      *
           PERFORM 900-DBCOMMIT-SEC
      *     
           DISPLAY "*** ORCDTCHK004 END ***"
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理途中経過表示処理
      *****************************************************************
       800-ORCSJOB-STM-SEC             SECTION.
      *
           MOVE    "STM"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-JOB-YOBI        TO  JOB-YOBI
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCDTCHK004"       TO  JOB-PGID
           MOVE    "診療会計情報抽出"  TO  JOB-SHELLMSG
      *
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
           .
       800-ORCSJOB-STM-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    データチェック中間ファイル（診療会計：入院）読込処理
      *****************************************************************
       900-DTCHK005N-READ-SEC           SECTION.
      *
           READ    DTCHK005N-FILE       NEXT
           AT  END
               MOVE    HIGH-VALUE  TO  DTCHK005N-KEY
           END-READ
      *
           .
       900-DTCHK005N-READ-EXT.
           EXIT.
      *****************************************************************
      *    データチェック中間ファイル（診療会計：外来）読込処理
      *****************************************************************
       900-DTCHK005G-READ-SEC           SECTION.
      *
           READ    DTCHK005G-FILE       NEXT
           AT  END
               MOVE    HIGH-VALUE  TO  DTCHK005G-KEY
           END-READ
      *
           .
       900-DTCHK005G-READ-EXT.
           EXIT.
      *****************************************************************
      *    データチェック中間ファイル（受診患者）読込処理
      *****************************************************************
       900-DTCHK002-READ-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-DTCHK002
      *
           READ    DTCHK002-FILE
           AT  END
               MOVE    1           TO  FLG-DTCHK002
           END-READ
      *
           .
       900-DTCHK002-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計検索処理(ALL2)
      *****************************************************************
       900-SRYACCT-ALL2-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACCT
           INITIALIZE                  SRYACCT-REC
      *
           MOVE    ORCBSD01-SRYYM      TO  ACCT-SRYYM
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "all2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
               INITIALIZE                  SRYACCT-REC
           END-IF
      *
           .
       900-SRYACCT-ALL2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療会計FETCH処理(ALL2)
      *****************************************************************
       900-SRYACCT-ALL2-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACCT
           INITIALIZE                  SRYACCT-REC
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "all2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
               INITIALIZE                  SRYACCT-REC
           END-IF
      *
           .
       900-SRYACCT-ALL2-FET-EXT.
           EXIT.
      *****************************************************************
      *    診療会計検索処理
      *****************************************************************
       900-SRYACCT-KEY48-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACCT
           INITIALIZE                  SRYACCT-REC
      *
           MOVE    WRK-PARA-HOSPID     TO  ACCT-HOSPID
           MOVE    ORCBSD01-SRYYM      TO  ACCT-SRYYM
           MOVE    DTCHK002-PTID       TO  ACCT-PTID
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key48"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
               INITIALIZE                  SRYACCT-REC
           END-IF
      *
           .
       900-SRYACCT-KEY48-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療会計FETCH処理
      *****************************************************************
       900-SRYACCT-KEY48-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACCT
           INITIALIZE                  SRYACCT-REC
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key48"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
               INITIALIZE                  SRYACCT-REC
           END-IF
      *
           .
       900-SRYACCT-KEY48-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　ＳＴＡＲＴ処理
      *****************************************************************
       900-DBSTART-SEC                 SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           .
       900-DBSTART-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　ＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           .
       900-DBCOMMIT-EXT.
           EXIT.
