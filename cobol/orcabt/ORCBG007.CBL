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
       PROGRAM-ID.             ORCBG007.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 会計カード
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/07/30    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    処理対象患者ファイル
           SELECT  BG00701-FILE    ASSIGN   ORCBG00701PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  BG00701-KEY
                                   FILE    STATUS  IS  STS-BG00701.
      *
      *
006900*    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    処理対象患者ソートファイル
       FD  BG00701-FILE.
       01  BG00701-REC.
           03  BG00701-KEY                     PIC X(130).
           03  BG00701-PTID                    PIC 9(10).
           03  BG00701-SRYKA                   PIC X(02).
           03  BG00701-SRYKADAY-G.
               05  BG00701-SRYKADAY            PIC 9(01)
                                               OCCURS  31.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                         PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    処理対象患者ファイル
           COPY    "CPCOMMONDAT2.INC"
                                           REPLACING   //RECE01//
                                           BY          //ORCBG00701//.
           03  FILLER                      PIC X(04)   VALUE   ".dat".
      *
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"      REPLACING  //RECE01PARA//
                                           BY         //RECEERR//.
           03  FILLER                      PIC X(04)   VALUE   ".dat".
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-RECEERR                         PIC X(02).
           03  STS-BG00701                         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-SRYACCT                         PIC 9(01).
           03  FLG-BG00701                         PIC 9(01).
           03  FLG-PTNYUINRRK                      PIC 9(01).
           03  FLG-CHKRRK                          PIC 9(01).
           03  FLG-CHKACCT                         PIC 9(01).
           03  FLG-SKIP                            PIC 9(01).
           03  FLG-SRYKADAY-G.
               05  FLG-SRYKADAY                    PIC 9(01)
                                                   OCCURS  31.
           03  FLG-PTNUM                           PIC 9(01).
           03  FLG-PTINF                           PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-BG007                           PIC 9(05).
           03  CNT-PAGE                            PIC 9(08).
           03  CNT-JOB-UPDCNT                      PIC 9(08).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME                            PIC 9(08).
           03  SYS-YMD.
               05  SYS-YY                          PIC 9(02).
               05  SYS-MM                          PIC 9(02).
               05  SYS-DD                          PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                                PIC 9(05).
           03  IDX1                                PIC 9(05).
           03  IDX2                                PIC 9(05).
           03  IDX3                                PIC 9(05).
           03  IDXE                                PIC 9(05).
           03  IDX-DAY                             PIC 9(05).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID                      PIC 9(07).
           03  WRK-PARA-SHELLID                    PIC X(08).
           03  WRK-PARA-HOSPID                     PIC X(24).
           03  WRK-PARA-SRYYM                      PIC X(06).
           03  WRK-PARA-PTNUM                      PIC X(20).
           03  WRK-PARA-NYUGAIKBN                  PIC X(01).
           03  WRK-PARA-BTUNUM                     PIC X(02).
           03  WRK-PARA-SRTKBN                     PIC X(01).
           03  WRK-PARA-SRYKAKBN                   PIC X(01).
           03  WRK-PARA-SRYKA                      PIC X(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD                            PIC X(08).
           03  WRK-EDTYMD1                         PIC X(10).
           03  WRK-EDTYMD3                         PIC X(22).
           03  WRK-RECEERR                         PIC X(200).
           03  WRK-TITLE                           PIC X(100).
           03  WRK-STR                             PIC X(200).
           03  WRK-LEN                             PIC 9(05).
           03  WRK-SHO                             PIC 9(05).
           03  WRK-AMARI                           PIC 9(05).
           03  WRK-MAE-INPUT                       PIC X(300).
           03  WRK-OUT-INPUT                       PIC X(300).
           03  WRK-OUT-MAX                         PIC 9(05).
           03  WRK-STR-LEFT                        PIC X(500).
           03  WRK-STR-LEFT-Z      REDEFINES       WRK-STR-LEFT.
               05  WRK-STR-LEFT-ZX99               PIC Z(9)9.
               05                                  PIC X(490).
           03  WRK-STR-LEFT-TMP                    PIC X(500).
      *        全角編集用項目
           03  WRK-N.
               05  WRK-BG007-KNS-N                 PIC X(10).
               05  WRK-BG007-TIME-N                PIC X(16).
           03  WRK-SEL-PTID                        PIC 9(10).
           03  WRK-SEL-PTNUM                       PIC X(20).
           03  WRK-STIDX                           PIC 9(05).
           03  WRK-EDIDX                           PIC 9(05).
           03  WRK-YMD-G.
               05  WRK-YMD-YM                      PIC X(06).
               05  WRK-YMD-DD                      PIC 9(02).
      *
       01  SORTKEY-AREA.
      *    病棟番号・カナ氏名順
           03  SORTKEY1-KEY.
               05  SORTKEY1-SRYKA                  PIC X(02).
               05  SORTKEY1-BTUNUM                 PIC 9(02).
               05  SORTKEY1-KANANAME               PIC X(100).
               05  SORTKEY1-PTNUM                  PIC X(20).
               05  SORTKEY1-DUMMY                  PIC X(06).
      *    病棟番号・患者番号順
           03  SORTKEY2-KEY    REDEFINES           SORTKEY1-KEY.
               05  SORTKEY2-SRYKA                  PIC X(02).
               05  SORTKEY2-BTUNUM                 PIC 9(02).
               05  SORTKEY2-PTNUM                  PIC X(20).
               05  SORTKEY2-DUMMY                  PIC X(106).
      *    カナ氏名順
           03  SORTKEY3-KEY    REDEFINES           SORTKEY1-KEY.
               05  SORTKEY3-SRYKA                  PIC X(02).
               05  SORTKEY3-KANANAME               PIC X(100).
               05  SORTKEY3-PTNUM                  PIC X(20).
               05  SORTKEY3-DUMMY                  PIC X(08).
      *    患者番号順
           03  SORTKEY4-KEY    REDEFINES           SORTKEY1-KEY.
               05  SORTKEY4-SRYKA                  PIC X(02).
               05  SORTKEY4-PTNUM                  PIC X(20).
               05  SORTKEY4-DUMMY                  PIC X(108).
      *
       01  TSRYKA-AREA.
           03  TSRYKA-IDX                          PIC 9(05).
           03  TSRYKA-CHK-SRYKA                    PIC X(02).
           03  TSRYKA-MAX                          PIC 9(05).
           03  TSRYKA-OCC                          OCCURS  50.
               05  TSRYKA-SRYKA                    PIC X(02).
           03  TSRYKA-DAY                          PIC X(02)
                                                   OCCURS  31.
      *
       01  KEY-AREA.
           03  KEY-SRYKA                           PIC X(02).
           03  KEY-PTID                            PIC 9(10).
      *
       01  ERR-EDIT-AREA.
           03  ERR-PTID                            PIC 9(10).
           03  ERR-PTNUM                           PIC X(20).
           03  ERR-ERRMSG                          PIC X(80).
           03  ERR-HEN-ERRMSG                      PIC X(80).
      *
       01  CONST-AREA.
      *    診療科退避ワーク
           03  CONST-TSRYKA-MAX        PIC 9(05)   VALUE 50.
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    点数マスタ退避ワーク
           COPY    "CPTTNS.INC".
      *
      *    患者入院履歴ワーク
       01  CHKRRK-REC.
           COPY    "CPPTNYUINRRK.INC"      REPLACING   //PTNYUINRRK-//
                                           BY          //CHKRRK-//.
      *
       01  LASTRRK-REC.
           COPY    "CPPTNYUINRRK.INC"      REPLACING   //PTNYUINRRK-//
                                           BY          //LASTRRK-//.
      *    診療会計ワーク
       01  CHKACCT-REC.
           COPY    "CPSRYACCT.INC"         REPLACING   //ACCT-//
                                           BY          //CHKACCT-//.
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY    "CPSK1009.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
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
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    会計カード編集サブ
           COPY    "CPORCHCM34.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(1000).
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
           PERFORM UNTIL ( FLG-END NOT =   ZERO )
               PERFORM 200-MAIN-SEC
           END-PERFORM
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
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
           INITIALIZE                  KEY-AREA
           INITIALIZE                  TTNS-AREA
           INITIALIZE                  ERR-EDIT-AREA
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
                                       WRK-PARA-SRYYM
                                       WRK-PARA-PTNUM
                                       WRK-PARA-NYUGAIKBN
                                       WRK-PARA-BTUNUM
                                       WRK-PARA-SRTKBN
                                       WRK-PARA-SRYKAKBN
                                       WRK-PARA-SRYKA
      *
           MOVE    SPACE               TO  WRK-TITLE
      *
           IF    ( WRK-PARA-NYUGAIKBN  =   "1" )
             OR  ( WRK-PARA-NYUGAIKBN  =   "2" )
               CONTINUE
           ELSE
               MOVE    "2"             TO  WRK-PARA-NYUGAIKBN
           END-IF
      *
           IF    ( WRK-PARA-SRYKA      NOT =   SPACE )
            AND  ( WRK-PARA-SRYKAKBN       =   SPACE )
               MOVE    "1"             TO  WRK-PARA-SRYKAKBN
           END-IF
      *
           IF    ( WRK-PARA-NYUGAIKBN  =   "1" )
               MOVE    "会計カード−入院"
                                       TO  WRK-TITLE
           ELSE
               MOVE    "会計カード−外来"
                                       TO  WRK-TITLE
           END-IF
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCBG007"          TO  JOB-PGID
           MOVE    WRK-TITLE           TO  JOB-SHELLMSG
      *
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
      *
           MOVE    "BG00701"           TO  ORCBG00701PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  ORCBG00701PARA-TERMID
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECEERR-TERMID
      *
      *    ＳＰＡエリアセット処理
           PERFORM 110-SPA-SET-SEC
      *
      *    印刷パラメタエリアセット処理
           PERFORM 110-ORCSPRTAREA-SET-SEC
      *
           MOVE    ZERO                TO  CNT-BG007
                                           CNT-PAGE
                                           CNT-JOB-UPDCNT
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡエリアセット処理
      *****************************************************************
       110-SPA-SET-SEC                 SECTION.
      *
      *    システム日付セット
           INITIALIZE                      SPA-AREA
           MOVE    LNK-PRTKANRI-SKYYMD
                                       TO  SPA-SYSYMD
           MOVE    WRK-PARA-HOSPID     TO  SPA-HOSPID
      *
           MOVE    LNK-PRTKANRI-TERMID TO  SPA-TERMID
      *
           .
       110-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷パラメタエリアセット処理
      *****************************************************************
       110-ORCSPRTAREA-SET-SEC         SECTION.
      *
      *    印刷パラメタ編集
           INITIALIZE                      ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
      *     MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    WRK-PARA-SRYYM      TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE    WRK-TITLE           TO  SPRT-TITLE
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
      *
           .
       110-ORCSPRTAREA-SET-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           EVALUATE    WRK-PARA-NYUGAIKBN
                                   ALSO WRK-PARA-PTNUM
      *    入院全患者
           WHEN        "1"         ALSO SPACE
               PERFORM 210-NYU-ALL-HEN-SEC
               IF    ( FLG-END     NOT =   ZERO )
                   GO  TO  200-MAIN-EXT
               END-IF
      *    外来全患者
           WHEN        "2"         ALSO SPACE
               PERFORM 210-GAI-ALL-HEN-SEC
               IF    ( FLG-END     NOT =   ZERO )
                   GO  TO  200-MAIN-EXT
               END-IF
      *    入院指定患者
           WHEN        "1"         ALSO NOT SPACE
      *    外来指定患者
           WHEN        "2"         ALSO NOT SPACE
               PERFORM 210-KJN-HEN-SEC
               IF    ( FLG-END     NOT =   ZERO )
                   GO  TO  200-MAIN-EXT
               END-IF
           END-EVALUATE
      *
           PERFORM 2101-KAIKEI-HEN-SEC
      *
           MOVE    1                   TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    会計カード編集処理−入院対象診療年月全患者−
      *****************************************************************
       210-NYU-ALL-HEN-SEC         SECTION.
      *
      *    処理対象患者ファイル編集処理
           PERFORM 2101-NYU-BG00701-HEN-SEC
           IF    ( FLG-END     NOT =   ZERO )
               GO  TO  210-NYU-ALL-HEN-EXT
           END-IF
      *
           .
       210-NYU-ALL-HEN-EXT.
           EXIT.
      *****************************************************************
      *    処理対象患者ファイル編集処理
      *****************************************************************
       2101-NYU-BG00701-HEN-SEC        SECTION.
      *
           OPEN    OUTPUT  BG00701-FILE
      *
           PERFORM 900-PTNYUINRRK-KEY43-SEL-SEC
           IF    ( FLG-PTNYUINRRK  =   ZERO )
               PERFORM 900-PTNYUINRRK-KEY43-FET-SEC
           END-IF
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
      *
               INITIALIZE                  TSRYKA-AREA
      *
               IF    ( WRK-PARA-SRYKAKBN   =   "1" )
                   MOVE    PTNYUINRRK-PTID TO  WRK-SEL-PTID
                   PERFORM 21011-NYU-TSRYKA-HEN-SEC
               ELSE
                   MOVE    1               TO  TSRYKA-MAX
                   MOVE    ZERO            TO  TSRYKA-SRYKA (1)
               END-IF
      *
               IF    ( TSRYKA-MAX      >   ZERO )
                   PERFORM 21011-NYU-BG00701-OUT-SEC
                   IF    ( FLG-END     =   1 )
                       GO  TO  2101-NYU-BG00701-HEN-EXT
                   END-IF
               END-IF
      *
               PERFORM 900-PTNYUINRRK-KEY43-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key43"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   BG00701-FILE
      *
           .
       2101-NYU-BG00701-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院科退避処理
      *****************************************************************
       21011-NYU-TSRYKA-HEN-SEC        SECTION.
      *
           PERFORM 900-CHKRRK-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKRRK  NOT =   ZERO )
      *
               MOVE    CHKRRK-NYUINKA  TO  TSRYKA-CHK-SRYKA
               PERFORM 210111-CHK-SRYKA-SEC
      *
               IF    ( TSRYKA-IDX  NOT =   ZERO )
                   PERFORM 210111-NYU-CHK-SRYKA-DAY-SEC
               END-IF
      *
               PERFORM 900-CHKRRK-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key44"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           PERFORM 900-CHKACCT-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKACCT NOT =   ZERO )
      *
               MOVE    CHKACCT-SRYKA   TO  TSRYKA-CHK-SRYKA
               PERFORM 210111-CHK-SRYKA-SEC
      *
               PERFORM 900-CHKACCT-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key42"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       21011-NYU-TSRYKA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療科チェック処理
      *****************************************************************
       210111-CHK-SRYKA-SEC            SECTION.
      *
           MOVE    ZERO                    TO  TSRYKA-IDX
      *
           IF    ( WRK-PARA-SRYKAKBN      =   "1" )
            AND  ( WRK-PARA-SRYKA     NOT =   SPACE )
      *
               IF    ( WRK-PARA-SRYKA      =   TSRYKA-CHK-SRYKA )
                       MOVE    1           TO  TSRYKA-MAX
                       MOVE    1           TO  TSRYKA-IDX
                       MOVE    TSRYKA-CHK-SRYKA
                                           TO  TSRYKA-SRYKA (1)
               END-IF
      *
           ELSE
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0            >   TSRYKA-MAX )
                        OR   ( TSRYKA-IDX  NOT =   ZERO )
      *
                   IF    ( TSRYKA-SRYKA (IDX0) =   TSRYKA-CHK-SRYKA )
                       MOVE    IDX0        TO  TSRYKA-IDX
                   END-IF
      *
               END-PERFORM
      *
               IF    ( TSRYKA-IDX      =   ZERO )
                   IF    ( TSRYKA-MAX  <   CONST-TSRYKA-MAX )
                       COMPUTE TSRYKA-MAX  =   TSRYKA-MAX    +   1
                       MOVE    TSRYKA-MAX  TO  TSRYKA-IDX
                       MOVE    TSRYKA-CHK-SRYKA
                                           TO  TSRYKA-SRYKA (TSRYKA-IDX)
                   END-IF
               END-IF
           END-IF
      *
           .
       210111-CHK-SRYKA-EXT.
           EXIT.
      *****************************************************************
      *    入院科日付チェック処理
      *****************************************************************
       210111-NYU-CHK-SRYKA-DAY-SEC    SECTION.
      *
           IF    ( CHKRRK-TENNYUYMD (1:6)  <   WRK-PARA-SRYYM )
               MOVE    1                   TO  WRK-STIDX
           ELSE
               MOVE    CHKRRK-TENNYUYMD    TO  WRK-YMD-G
               MOVE    WRK-YMD-DD          TO  WRK-STIDX
           END-IF
      *
           IF    ( CHKRRK-TENSTUYMD (1:6)  >   WRK-PARA-SRYYM )
               MOVE    31                  TO  WRK-EDIDX
           ELSE
               MOVE    CHKRRK-TENSTUYMD    TO  WRK-YMD-G
               MOVE    WRK-YMD-DD          TO  WRK-EDIDX
           END-IF
      *
           PERFORM VARYING IDX-DAY     FROM    WRK-STIDX   BY  1
                   UNTIL ( IDX-DAY     >   WRK-EDIDX )
      *
               MOVE    CHKRRK-NYUINKA  TO  TSRYKA-DAY (IDX-DAY)
      *
           END-PERFORM
      *
           .
       210111-NYU-CHK-SRYKA-DAY-EXT.
           EXIT.
      *****************************************************************
      *    処理対象患者ファイル出力処理（入院）
      *****************************************************************
       21011-NYU-BG00701-OUT-SEC       SECTION.
      *
           PERFORM 21011-LASTRRK-GET-SEC
           IF    ( FLG-END     =   1 )
               GO  TO  21011-NYU-BG00701-OUT-EXT
           END-IF
      *
           MOVE    PTNYUINRRK-PTID     TO  WRK-SEL-PTID
           PERFORM 900-PTNUM-KEY-SEL-SEC
           IF    ( FLG-PTNUM       NOT =   ZERO )
               INITIALIZE          ERR-EDIT-AREA
               MOVE    PTNYUINRRK-PTID TO  ERR-PTID
               MOVE    "患者番号の取得に失敗しました"
                                       TO  ERR-ERRMSG
               PERFORM 800-ERRMSG-EDIT-SEC
               MOVE    ERR-HEN-ERRMSG  TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  21011-NYU-BG00701-OUT-EXT
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   TSRYKA-MAX )
      *
               IF    ( WRK-PARA-BTUNUM     =   SPACE )
                       PERFORM 21011-NYU-BG00701-WRITE-SEC
                       IF    ( FLG-END     =   1 )
                           GO  TO  21011-NYU-BG00701-OUT-EXT
                       END-IF
               ELSE
                   IF    ( WRK-PARA-BTUNUM =   LASTRRK-BTUNUM )
                       PERFORM 21011-NYU-BG00701-WRITE-SEC
                       IF    ( FLG-END         NOT =   ZERO )
                           GO  TO  21011-NYU-BG00701-OUT-EXT
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       21011-NYU-BG00701-OUT-EXT.
           EXIT.
      *****************************************************************
      *    処理対象患者ファイル出力処理
      *****************************************************************
       21011-NYU-BG00701-WRITE-SEC     SECTION.
      *
           INITIALIZE                  BG00701-REC
                                       SORTKEY-AREA
      *
           EVALUATE    WRK-PARA-SRTKBN
      *
           WHEN    "1"
      *
               MOVE    TSRYKA-SRYKA (IDX1) TO  SORTKEY1-SRYKA
               MOVE    LASTRRK-BTUNUM      TO  SORTKEY1-BTUNUM
               MOVE    PTINF-KANANAME      TO  SORTKEY1-KANANAME
               MOVE    PTNUM-PTNUM         TO  SORTKEY1-PTNUM
               MOVE    SORTKEY1-KEY        TO  BG00701-KEY
      *
           WHEN    "2"
      *
               MOVE    TSRYKA-SRYKA (IDX1) TO  SORTKEY2-SRYKA
               MOVE    LASTRRK-BTUNUM      TO  SORTKEY2-BTUNUM
               MOVE    PTNUM-PTNUM         TO  SORTKEY2-PTNUM
               MOVE    SORTKEY2-KEY        TO  BG00701-KEY
      *
           WHEN    "3"
      *
               MOVE    TSRYKA-SRYKA (IDX1) TO  SORTKEY3-SRYKA
               MOVE    PTINF-KANANAME      TO  SORTKEY3-KANANAME
               MOVE    PTNUM-PTNUM         TO  SORTKEY3-PTNUM
               MOVE    SORTKEY3-KEY        TO  BG00701-KEY
      *
           WHEN    "4"
      *
               MOVE    TSRYKA-SRYKA (IDX1) TO  SORTKEY4-SRYKA
               MOVE    PTNUM-PTNUM         TO  SORTKEY4-PTNUM
               MOVE    SORTKEY4-KEY        TO  BG00701-KEY
      *
           WHEN    OTHER
               MOVE    TSRYKA-SRYKA (IDX1) TO  SORTKEY1-SRYKA
               MOVE    LASTRRK-BTUNUM      TO  SORTKEY1-BTUNUM
               MOVE    PTINF-KANANAME      TO  SORTKEY1-KANANAME
               MOVE    PTNUM-PTNUM         TO  SORTKEY1-PTNUM
               MOVE    SORTKEY1-KEY        TO  BG00701-KEY
           END-EVALUATE
      *
           MOVE    PTNUM-PTID              TO  BG00701-PTID
           MOVE    TSRYKA-SRYKA (IDX1)     TO  BG00701-SRYKA
      *
           IF    ( WRK-PARA-SRYKAKBN   =   "1" )
               PERFORM 21011-FLG-SRYKADAY-HEN-SEC
      *
               MOVE    FLG-SRYKADAY-G          TO  BG00701-SRYKADAY-G
           END-IF
      *
           WRITE   BG00701-REC
      *
           .
       21011-NYU-BG00701-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    診療科日付フラグ編集処理
      *****************************************************************
       21011-FLG-SRYKADAY-HEN-SEC      SECTION.
      *
           INITIALIZE              FLG-SRYKADAY-G
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
               IF    ( TSRYKA-SRYKA (IDX1)
                                       =   TSRYKA-DAY   (IDX-DAY))
                   MOVE    1           TO  FLG-SRYKADAY (IDX-DAY)
               END-IF
           END-PERFORM
      *
           .
       21011-FLG-SRYKADAY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    最新入院歴情報取得処理
      *****************************************************************
       21011-LASTRRK-GET-SEC           SECTION.
      *
      *    最新の入院履歴情報を取得
           INITIALIZE  LASTRRK-REC
           MOVE    PTINF-HOSPID        TO  LASTRRK-HOSPID
           MOVE    PTINF-PTID          TO  LASTRRK-PTID
           MOVE    PTINF-RRKNUM        TO  LASTRRK-RRKNUM
           MOVE    PTINF-RRKEDANUM     TO  LASTRRK-RRKEDANUM
           MOVE    LASTRRK-REC         TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  LASTRRK-REC
           ELSE
               INITIALIZE              ERR-EDIT-AREA
               MOVE    PTINF-PTID      TO  ERR-PTID
               MOVE    "最新入院履歴の取得に失敗しました"
                                       TO  ERR-ERRMSG
               PERFORM 800-ERRMSG-EDIT-SEC
               MOVE    ERR-HEN-ERRMSG
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  21011-LASTRRK-GET-EXT
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       21011-LASTRRK-GET-EXT.
           EXIT.
      *****************************************************************
      *    会計カード編集処理−指定患者のみ−
      *****************************************************************
       210-KJN-HEN-SEC             SECTION.
      *
           OPEN    OUTPUT  BG00701-FILE
      *
           INITIALIZE              TSRYKA-AREA
      *
           MOVE    WRK-PARA-PTNUM  TO  WRK-SEL-PTNUM
           PERFORM 900-PTNUM-KEY4-SEL-SEC
           IF    ( FLG-PTNUM      =    ZERO )
               CONTINUE
           ELSE
               INITIALIZE              ERR-EDIT-AREA
               MOVE    WRK-PARA-PTNUM  TO  ERR-PTNUM
               MOVE    "該当の患者は存在しません"
                                       TO  ERR-ERRMSG
               PERFORM 800-ERRMSG-EDIT-SEC
               MOVE    ERR-HEN-ERRMSG  TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  210-KJN-HEN-EXT
           END-IF
      *
           IF    ( WRK-PARA-SRYKAKBN   =   "1" )
               MOVE    PTNUM-PTID      TO  WRK-SEL-PTID
               IF    ( WRK-PARA-NYUGAIKBN  =   "1" )
                   PERFORM 21011-NYU-TSRYKA-HEN-SEC
               ELSE
                   PERFORM 21011-GAI-TSRYKA-HEN-SEC
               END-IF
           ELSE
               MOVE    1               TO  TSRYKA-MAX
               MOVE    ZERO            TO  TSRYKA-SRYKA (1)
           END-IF
      *
           IF    ( TSRYKA-MAX      >   ZERO )
      *
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   TSRYKA-MAX )
      *
                   INITIALIZE          BG00701-REC
                                       SORTKEY-AREA
      *
                   MOVE    TSRYKA-SRYKA (IDX1) TO  SORTKEY4-SRYKA
                   MOVE    PTNUM-PTNUM         TO  SORTKEY4-PTNUM
                   MOVE    SORTKEY4-KEY        TO  BG00701-KEY
      *
                   MOVE    PTNUM-PTID          TO  BG00701-PTID
                   MOVE    TSRYKA-SRYKA (IDX1) TO  BG00701-SRYKA
      *
                   IF    ( WRK-PARA-SRYKAKBN   =   "1" )
                       IF    (  WRK-PARA-NYUGAIKBN  =   "1" )
                           PERFORM 21011-FLG-SRYKADAY-HEN-SEC
      *
                           MOVE    FLG-SRYKADAY-G
                                               TO  BG00701-SRYKADAY-G
                       END-IF
                   END-IF
      *
                   WRITE   BG00701-REC
      *
               END-PERFORM
      *
           ELSE
               INITIALIZE              ERR-EDIT-AREA
               MOVE    WRK-PARA-PTNUM  TO  ERR-PTNUM
               MOVE    "該当の患者の診療情報は存在しません"
                                       TO  ERR-ERRMSG
               PERFORM 800-ERRMSG-EDIT-SEC
               MOVE    ERR-HEN-ERRMSG  TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  210-KJN-HEN-EXT
           END-IF
      *
           CLOSE   BG00701-FILE
      *
           .
       210-KJN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    会計カード編集処理−外来対象診療年月全患者−
      *****************************************************************
       210-GAI-ALL-HEN-SEC         SECTION.
      *
      *    処理対象患者ファイル出力処理
           PERFORM 2101-GAI-BG00701-HEN-SEC
           IF    ( FLG-END     NOT =   ZERO )
               GO  TO  210-GAI-ALL-HEN-EXT
           END-IF
      *
           .
       210-GAI-ALL-HEN-EXT.
           EXIT.
      *****************************************************************
      *    処理対象患者ファイル出力処理（外来）
      *****************************************************************
       2101-GAI-BG00701-HEN-SEC        SECTION.
      *
           OPEN    OUTPUT  BG00701-FILE
      *
           MOVE    ZERO            TO  FLG-SRYACCT
      *
           PERFORM 900-SRYACCT-KEY43-SEL-SEC
      *
           INITIALIZE                  TSRYKA-AREA
           MOVE    ACCT-PTID       TO  KEY-PTID
      *
           PERFORM UNTIL ( FLG-SRYACCT NOT =   ZERO )
      *
               IF    ( KEY-PTID        NOT =   ACCT-PTID )
                   IF    ( TSRYKA-MAX      >   ZERO )
                       PERFORM 21011-GAI-BG00701-OUT-SEC
                       IF    ( FLG-END     =   1 )
                           GO  TO  2101-GAI-BG00701-HEN-EXT
                       END-IF
                   END-IF
      *
                   INITIALIZE                  TSRYKA-AREA
                   MOVE    ACCT-PTID       TO  KEY-PTID
      *
               END-IF
      *
               IF    ( WRK-PARA-SRYKAKBN   =   "1" )
                   MOVE    ACCT-SRYKA      TO  TSRYKA-CHK-SRYKA
                   PERFORM 210111-CHK-SRYKA-SEC
               ELSE
                   MOVE    1               TO  TSRYKA-MAX
                   MOVE    ZERO            TO  TSRYKA-SRYKA (1)
               END-IF
      *
               PERFORM 900-SRYACCT-KEY43-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key43"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( TSRYKA-MAX      >   ZERO )
               PERFORM 21011-GAI-BG00701-OUT-SEC
               IF    ( FLG-END     =   1 )
                   GO  TO  2101-GAI-BG00701-HEN-EXT
               END-IF
           END-IF
      *
      *
           CLOSE   BG00701-FILE
      *
           .
       2101-GAI-BG00701-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療科退避処理
      *****************************************************************
       21011-GAI-TSRYKA-HEN-SEC        SECTION.
      *
           PERFORM 900-CHKACCT-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKACCT NOT =   ZERO )
      *
               MOVE    CHKACCT-SRYKA   TO  TSRYKA-CHK-SRYKA
               PERFORM 210111-CHK-SRYKA-SEC
      *
               PERFORM 900-CHKACCT-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key42"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       21011-GAI-TSRYKA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    処理対象患者ファイル出力処理（外来）
      *****************************************************************
       21011-GAI-BG00701-OUT-SEC       SECTION.
      *
           MOVE    KEY-PTID        TO  WRK-SEL-PTID
           PERFORM 900-PTNUM-KEY-SEL-SEC
           IF    ( FLG-PTNUM       NOT =   ZERO )
               INITIALIZE          ERR-EDIT-AREA
               MOVE    ACCT-PTID       TO  ERR-PTID
               MOVE    "患者番号の取得に失敗しました"
                                       TO  ERR-ERRMSG
               PERFORM 800-ERRMSG-EDIT-SEC
               MOVE    ERR-HEN-ERRMSG  TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  21011-GAI-BG00701-OUT-EXT
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   TSRYKA-MAX )
      *
               PERFORM 21011-GAI-BG00701-WRITE-SEC
               IF    ( FLG-END     =   1 )
                   GO  TO  21011-GAI-BG00701-OUT-EXT
               END-IF
      *
           END-PERFORM
      *
           .
       21011-GAI-BG00701-OUT-EXT.
           EXIT.
      *****************************************************************
      *    処理対象患者ファイル出力処理
      *****************************************************************
       21011-GAI-BG00701-WRITE-SEC     SECTION.
      *
           INITIALIZE                  BG00701-REC
                                       SORTKEY-AREA
      *
           EVALUATE    WRK-PARA-SRTKBN
      *
           WHEN    "1"
           WHEN    "3"
      *
               MOVE    TSRYKA-SRYKA (IDX1) TO  SORTKEY3-SRYKA
               MOVE    PTINF-KANANAME      TO  SORTKEY3-KANANAME
               MOVE    PTNUM-PTNUM         TO  SORTKEY3-PTNUM
               MOVE    SORTKEY3-KEY        TO  BG00701-KEY
      *
           WHEN    "2"
           WHEN    "4"
      *
               MOVE    TSRYKA-SRYKA (IDX1) TO  SORTKEY4-SRYKA
               MOVE    PTNUM-PTNUM         TO  SORTKEY4-PTNUM
               MOVE    SORTKEY4-KEY        TO  BG00701-KEY
      *
           WHEN    OTHER
               MOVE    TSRYKA-SRYKA (IDX1) TO  SORTKEY4-SRYKA
               MOVE    PTNUM-PTNUM         TO  SORTKEY4-PTNUM
               MOVE    SORTKEY4-KEY        TO  BG00701-KEY
           END-EVALUATE
      *
           MOVE    PTNUM-PTID              TO  BG00701-PTID
           MOVE    TSRYKA-SRYKA (IDX1)     TO  BG00701-SRYKA
      *
           WRITE   BG00701-REC
      *
           .
       21011-GAI-BG00701-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    会計カード編集処理
      *****************************************************************
       2101-KAIKEI-HEN-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-BG00701
           OPEN    INPUT  BG00701-FILE
      *
           PERFORM 900-BG00701-READ-SEC
      *
           PERFORM UNTIL ( FLG-BG00701 NOT =   ZERO )
      *
               PERFORM 2101-ORCHCM34-CALL-SEC
               IF    ( FLG-END     NOT =   ZERO )
                   GO  TO  2101-KAIKEI-HEN-EXT
               END-IF
      *
               PERFORM 2101-SYORI-KEIKA-HEN-SEC
      *
               PERFORM 900-BG00701-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   BG00701-FILE
      *
           .
       2101-KAIKEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    処理経過編集処理
      *****************************************************************
       2101-SYORI-KEIKA-HEN-SEC        SECTION.
      *
           MOVE    ZERO            TO  WRK-SHO
                                       WRK-AMARI
           DIVIDE  50          INTO    CNT-BG007   GIVING  WRK-SHO
           REMAINDER WRK-AMARI
           END-DIVIDE
      *
           IF    ( WRK-AMARI       NOT =   ZERO )
               GO  TO  2101-SYORI-KEIKA-HEN-EXT
           END-IF
      *
           MOVE    CNT-BG007       TO  WRK-STR-LEFT-ZX99
           PERFORM 800-STR-HIDARIZUME-HEN-SEC
           MOVE    WRK-STR-LEFT    TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT   TO  WRK-BG007-KNS-N
           ACCEPT  SYS-TIME        FROM    TIME
           MOVE    SPACE           TO  WRK-MAE-INPUT
           MOVE    SYS-TIME(1:2)   TO  WRK-MAE-INPUT(1:2)
           MOVE    "."             TO  WRK-MAE-INPUT(3:1)
           MOVE    SYS-TIME(3:2)   TO  WRK-MAE-INPUT(4:2)
           MOVE    "."             TO  WRK-MAE-INPUT(6:1)
           MOVE    SYS-TIME(5:2)   TO  WRK-MAE-INPUT(7:2)
      *    PERFORM 800-ZENKAKU-HEN-SEC
      *    MOVE    WRK-OUT-INPUT   TO  WRK-BG007-TIME-N
           MOVE    WRK-MAE-INPUT   TO  WRK-BG007-TIME-N
      *
      *    ステップ途中処理
           MOVE    "STM"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           STRING  "処理件数："        DELIMITED BY SIZE
                   WRK-BG007-KNS-N     DELIMITED BY SPACE
                   "件　"              DELIMITED BY SIZE
                   WRK-BG007-TIME-N    DELIMITED BY SPACE
           INTO    JOB-YOBI
           END-STRING
           MOVE    CNT-JOB-UPDCNT      TO  JOB-UPDCNT
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCBG007"          TO  JOB-PGID
           MOVE    WRK-TITLE           TO  JOB-SHELLMSG
      *
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
      *
           MOVE    ZERO                TO  CNT-JOB-UPDCNT
      *
           .
       2101-SYORI-KEIKA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    会計カード編集サブ呼出処理
      *****************************************************************
       2101-ORCHCM34-CALL-SEC          SECTION.
      *
           MOVE    BG00701-PTID        TO  SPA-PTID
      *
           INITIALIZE                      ORCHCM34AREA
           MOVE    "2"                 TO  ORCHCM34-KBN
           MOVE    WRK-PARA-SRYYM      TO  ORCHCM34-SRYYM
           MOVE    WRK-PARA-NYUGAIKBN  TO  ORCHCM34-NYUGAIKBN
           MOVE    WRK-PARA-SRYKAKBN   TO  ORCHCM34-SRYKAKBN
           MOVE    BG00701-SRYKA       TO  ORCHCM34-SRYKA
           MOVE    BG00701-SRYKADAY-G  TO  ORCHCM34-SRYKADAY-G
           MOVE    ORCSPRTAREA         TO  ORCHCM34-PRTAREA
      *
           CALL    "ORCHCM34"          USING
                                       SPA-AREA
                                       ORCHCM34AREA
                                       TTNS-AREA
                                       TTNS-CONST-MAX
           IF    ( ORCHCM34-RC     NOT =   ZERO )
               INITIALIZE              ERR-EDIT-AREA
               MOVE    SPA-PTID        TO  ERR-PTID
               MOVE    "会計カードの作成に失敗しました"
                                       TO  ERR-ERRMSG
               PERFORM 800-ERRMSG-EDIT-SEC
               MOVE    ERR-HEN-ERRMSG  TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  2101-ORCHCM34-CALL-EXT
           END-IF
      *
           COMPUTE CNT-BG007   =   CNT-BG007   +   1
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   ORCHCM34-PAGE
           COMPUTE CNT-JOB-UPDCNT  =   CNT-JOB-UPDCNT
                                   +   ORCHCM34-PAGE
      *
           .
       2101-ORCHCM34-CALL-EXT.
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
           MOVE    1               TO  FLG-END
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
           PERFORM 800-BG00701-DEL-SEC
      *
           IF    ( CNT-BG007           =   ZERO )
               MOVE    "処理対象のデータがありませんでした"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           DISPLAY "*** ORCBG007 IN " CNT-BG007  " ***"
           DISPLAY "*** ORCBG007 END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    CNT-JOB-UPDCNT  TO  JOB-UPDCNT
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理対象患者ファイル削除処理
      *****************************************************************
       800-BG00701-DEL-SEC             SECTION.
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    ORCBG00701PARA  TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
           .
       800-BG00701-DEL-EXT.
           EXIT.
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       800-ZENKAKU-HEN-SEC         SECTION.
      *
           MOVE    SPACE           TO  WRK-OUT-INPUT
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI
           MOVE  WRK-MAE-INPUT     TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF    ( KANACHK-RC      =   ZERO )
               MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-OUT-INPUT
               MOVE    KANACHK-MAX TO  WRK-OUT-MAX
           END-IF
      *
           .
       800-ZENKAKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    文字列左詰処理
      *****************************************************************
       800-STR-HIDARIZUME-HEN-SEC      SECTION.
      *
           MOVE    WRK-STR-LEFT    TO  IDXE
           PERFORM VARYING IDXE    FROM    1   BY  1
                   UNTIL ( IDXE    >       500 )
      *
               IF    ( WRK-STR-LEFT (IDXE:1)
                                       NOT =   SPACE )
                   MOVE    WRK-STR-LEFT (IDXE:)
                                           TO  WRK-STR-LEFT-TMP
                   MOVE    WRK-STR-LEFT-TMP
                                           TO  WRK-STR-LEFT
                   MOVE    500             TO  IDXE
               END-IF
      *
           END-PERFORM
      *
           .
       800-STR-HIDARIZUME-HEN-EXT.
           EXIT.
      *****************************************************************
      *    月末日取得処理
      *****************************************************************
       800-GETUMATU-SEC      SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD (1 : 6)    TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD
      *
           .
       800-GETUMATU-EXT.
            EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC           SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
                                       WRK-EDTYMD3
      *
           IF        ( WRK-SYMD        =   SPACE )
               GO  TO  800-SEIWA-HEN-EXT
           END-IF
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
           INSPECT WRK-EDTYMD3     REPLACING  ALL "  "  BY  "　"
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       800-ERRMSG-EDIT-SEC         SECTION.
      *
           MOVE    SPACE           TO  ERR-HEN-ERRMSG
      *
           IF    ( ERR-PTNUM       =   SPACE )
               MOVE    ERR-PTID    TO  WRK-SEL-PTID
               PERFORM 900-PTNUM-KEY-SEL-SEC
               IF    ( FLG-PTNUM   =   ZERO )
                   MOVE    PTNUM-PTNUM TO  ERR-PTNUM
               ELSE
                   STRING "?(ID:"      DELIMITED   BY  SIZE
                      ERR-PTID         DELIMITED   BY  SIZE
                      ")"              DELIMITED   BY  SIZE
                   INTO   ERR-PTNUM
                   END-STRING
               END-IF
           END-IF
      *
           MOVE    1               TO  WRK-LEN
           MOVE    SPACE           TO  WRK-STR
           STRING  ERR-PTNUM           DELIMITED   BY  SPACE
           INTO    WRK-STR     WITH    POINTER WRK-LEN
           END-STRING
      *
           COMPUTE WRK-LEN         =   WRK-LEN -   1
      *
           MOVE    ZERO            TO  WRK-SHO
                                       WRK-AMARI
           DIVIDE  2   INTO    WRK-LEN GIVING  WRK-SHO
           REMAINDER WRK-AMARI
           END-DIVIDE
      *
           IF    ( WRK-AMARI   =   ZERO )
               STRING  ERR-ERRMSG          DELIMITED   BY  SPACE
                       "［患者番号："      DELIMITED   BY  SIZE
                       ERR-PTNUM           DELIMITED   BY  SPACE
                       "］"                DELIMITED   BY  SIZE
               INTO    ERR-HEN-ERRMSG
               END-STRING
           ELSE
               STRING  ERR-ERRMSG          DELIMITED   BY  SPACE
                       "［患者番号:"       DELIMITED   BY  SIZE
                       ERR-PTNUM           DELIMITED   BY  SPACE
                       "］"                DELIMITED   BY  SIZE
               INTO    ERR-HEN-ERRMSG
               END-STRING
           END-IF
      *
           .
       800-ERRMSG-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴検索処理(KEY43)
      *****************************************************************
       900-PTNYUINRRK-KEY43-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
      *    対象患者抽出
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPID          TO  PTNYUINRRK-HOSPID
           MOVE    WRK-PARA-SRYYM      TO  WRK-SYMD
           PERFORM 800-GETUMATU-SEC
           MOVE    WRK-SYMD            TO  PTNYUINRRK-TENNYUYMD
           MOVE    WRK-PARA-SRYYM      TO  PTNYUINRRK-TENSTUYMD
           MOVE    "01"                TO  PTNYUINRRK-TENSTUYMD(7:2)
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key43"             TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               CONTINUE
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       900-PTNYUINRRK-KEY43-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴FETCH処理(KEY43)
      *****************************************************************
       900-PTNYUINRRK-KEY43-FET-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           MOVE    1                   TO  FLG-SKIP
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
                    OR   ( FLG-SKIP            =   ZERO )
      *
               MOVE    ZERO            TO  FLG-SKIP
      *
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key43"             TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
                   MOVE    PTNYUINRRK-PTID TO  WRK-SEL-PTID
                   PERFORM 900-PTINF-KEY-SEL-SEC
                   IF    ( FLG-PTINF       =   ZERO )
                       IF    ( PTINF-TSTPTNUMKBN   =   "1" )
                           MOVE    1       TO  FLG-SKIP
                       END-IF
                   ELSE
                       INITIALIZE          ERR-EDIT-AREA
                       MOVE   PTNYUINRRK-PTID  TO  ERR-PTID
                       MOVE    "患者情報の取得に失敗しました"
                                           TO  ERR-ERRMSG
                       PERFORM 800-ERRMSG-EDIT-SEC
                       MOVE    ERR-HEN-ERRMSG  TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
      *
                       MOVE    1           TO  FLG-PTNYUINRRK
      *
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-PTNYUINRRK
               END-IF
      *
           END-PERFORM
      *
      *
           .
       900-PTNYUINRRK-KEY43-FET-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理(KEY44)
      *****************************************************************
       900-CHKRRK-SEL-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKRRK
      *
      *    対象患者抽出
           INITIALIZE                      CHKRRK-REC
           MOVE    SPA-HOSPID          TO  CHKRRK-HOSPID
           MOVE    WRK-SEL-PTID        TO  CHKRRK-PTID
           MOVE    WRK-PARA-SRYYM      TO  WRK-SYMD
           PERFORM 800-GETUMATU-SEC
           MOVE    WRK-SYMD            TO  CHKRRK-TENNYUYMD
           MOVE    WRK-PARA-SRYYM      TO  CHKRRK-TENSTUYMD
           MOVE    "01"                TO  CHKRRK-TENSTUYMD(7:2)
           MOVE    CHKRRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key44"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKRRK-REC
           ELSE
               MOVE    1               TO  FLG-CHKRRK
           END-IF
      *
           .
       900-CHKRRK-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴FETCH処理(KEY44)
      *****************************************************************
       900-CHKRRK-FET-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKRRK
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key44"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKRRK-REC
           ELSE
               MOVE    1               TO  FLG-CHKRRK
           END-IF
      *
           .
       900-CHKRRK-FET-EXT.
           EXIT.
      *****************************************************************
      *    診療会計検索処理(KEY42)
      *****************************************************************
       900-CHKACCT-SEL-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKACCT
      *
           INITIALIZE                      CHKACCT-REC
           MOVE    WRK-PARA-HOSPID     TO  CHKACCT-HOSPID
           MOVE    WRK-PARA-NYUGAIKBN  TO  CHKACCT-NYUGAIKBN
           MOVE    WRK-SEL-PTID        TO  CHKACCT-PTID
           MOVE    WRK-PARA-SRYYM      TO  CHKACCT-SRYYM
           MOVE    CHKACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key42"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKACCT-REC
           ELSE
               MOVE    1               TO  FLG-CHKACCT
           END-IF
      *
           .
       900-CHKACCT-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療会計FETCH処理(KEY42)
      *****************************************************************
       900-CHKACCT-FET-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKACCT
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key42"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKACCT-REC
           ELSE
               MOVE    1               TO  FLG-CHKACCT
           END-IF
      *
           .
       900-CHKACCT-FET-EXT.
           EXIT.
      *****************************************************************
      *    診療会計検索処理(KEY43)
      *****************************************************************
       900-SRYACCT-KEY43-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACCT
      *
           INITIALIZE                  SRYACCT-REC
           MOVE    SPA-HOSPID      TO  ACCT-HOSPID
           MOVE    WRK-PARA-NYUGAIKBN
                                   TO  ACCT-NYUGAIKBN
           MOVE    WRK-PARA-SRYYM  TO  ACCT-SRYYM
           MOVE    SRYACCT-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key43"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       900-SRYACCT-KEY43-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療会計FETCH処理(KEY43)
      *****************************************************************
       900-SRYACCT-KEY43-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACCT
      *
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key43"         TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  SRYACCT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       900-SRYACCT-KEY43-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者情報検索処理(KEY)
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
      *    対象患者抽出
           INITIALIZE                      PTINF-REC
           MOVE    WRK-PARA-HOSPID     TO  PTINF-HOSPID
           MOVE    WRK-SEL-PTID        TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-PTINF
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
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者番号検索処理(KEY)
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
      *
           INITIALIZE                      PTNUM-REC
           MOVE    WRK-PARA-HOSPID     TO  PTNUM-HOSPID
           MOVE    WRK-SEL-PTID        TO  PTNUM-PTID
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-PTNUM
               MOVE    MCPDATA-REC     TO  PTNUM-REC
           ELSE
               MOVE    1               TO  FLG-PTNUM
           END-IF
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者番号検索処理(KEY4)
      *****************************************************************
       900-PTNUM-KEY4-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
      *
           INITIALIZE                      PTNUM-REC
           MOVE    WRK-PARA-HOSPID     TO  PTNUM-HOSPID
           MOVE    WRK-SEL-PTNUM       TO  PTNUM-PTNUM
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-PTNUM
               MOVE    MCPDATA-REC     TO  PTNUM-REC
           ELSE
               MOVE    1               TO  FLG-PTNUM
           END-IF
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    処理対象患者ファイル読込処理
      *****************************************************************
       900-BG00701-READ-SEC            SECTION.
      *
           READ    BG00701-FILE    NEXT
           AT  END
               MOVE    1       TO  FLG-BG00701
           END-READ
      *
           .
       900-BG00701-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
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
      *    テーブル検索処理(FETCHは行なわない）
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
       911-DBSELECT-EXT.
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
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                  SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　ＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　ＳＴＡＲＴ処理
      *****************************************************************
       900-DBSTART-SEC                SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           .
       900-DBSTART-EXT.
           EXIT.
