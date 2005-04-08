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
       PROGRAM-ID.             ORCT0030.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 定期請求
      *  コンポーネント名  : 収納データチェック
      *  管理者            :
      *  作成日付    作業者        記述
      *  02/01/07    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    03.12.19  日別収納マスタ対応
      *  01.00.02    NACL-太田    04.03.03  削除対象患者請求履歴ファイル
      *                                     の編集をORCT0040に移す
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    定期請求期間情報ファイル(IN)
           SELECT  TEIKI03-FILE    ASSIGN  TEIKI03PARA
                   ORGANIZATION    LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-TEIKI03.
      *
      *    請求取消対象収納ファイル(初期化用)
           SELECT  TEIKI04CL-FILE  ASSIGN   TEIKI04PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  TEIKI04CL-KEY
                                   FILE    STATUS  IS  STS-TEIKI04CL.
      *
      *
      *    請求取消対象収納ファイル(OT)
           SELECT  TEIKI04-FILE    ASSIGN   TEIKI04PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TEIKI04-KEY
                                   FILE    STATUS  IS  STS-TEIKI04.
      *
      *    エラーファイル
           SELECT  TEIKIERR-FILE   ASSIGN  TEIKIERR
                                   FILE    STATUS  IS  STS-TEIKIERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    定期請求期間情報
       FD  TEIKI03-FILE.
           COPY "CPTEIKI03.INC".
      *
      *    請求取消対象収納ファイル（初期化用）
       FD  TEIKI04CL-FILE.
       01  TEIKI04CL-REC.
           COPY "CPTEIKI01.INC"            REPLACING   //TEIKI01-//
                                           BY          //TEIKI04CL-//.
      *
      *    請求取消対象収納ファイル
       FD  TEIKI04-FILE.
       01  TEIKI04-REC.
           COPY "CPTEIKI01.INC"            REPLACING   //TEIKI01-//
                                           BY          //TEIKI04-//.
      *
      *    エラーファイル
       FD  TEIKIERR-FILE.
       01  TEIKIERR-REC                    PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    定期請求期間情報名称領域 
           COPY    "CPCOMMONDAT5.INC"      REPLACING  //TEIKI01//
                                           BY         //TEIKI03//.
      *
      *    請求取消対象収納ファイル名称領域 
           COPY    "CPCOMMONDAT5.INC"      REPLACING  //TEIKI01//
                                           BY         //TEIKI04//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT5.INC"      REPLACING  //TEIKI01PARA//
                                           BY         //TEIKIERR//.
           03  FILLER                      PIC X(04)   VALUE   ".dat".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-TEIKIERR                PIC X(02).
           03  STS-TEIKI03                 PIC X(02).
           03  STS-TEIKI04                 PIC X(02).
           03  STS-TEIKI04CL               PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-ERR                     PIC 9(01).
           03  FLG-TEIKI03                 PIC 9(01).
           03  FLG-SYUNOU                  PIC 9(01).
           03  FLG-PTTEIKIRRK              PIC 9(01).
           03  FLG-SYUDAY                  PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-PTNUM                   PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-OUT1                    PIC 9(06).
           03  CNT-OUT2                    PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX1                        PIC 9(04). 
           03  IDX2                        PIC 9(04). 
           03  IDX3                        PIC 9(04). 
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-TEIKIERR                PIC X(200).
           03  WRK-STR                     PIC X(200).
           03  WRK-LEN                     PIC 9(05).
           03  WRK-SHO                     PIC 9(05).
           03  WRK-AMARI                   PIC 9(05).
           03  WRK-KUGIRI                  PIC X(02).
           03  WRK-PTID                    PIC 9(10).
      *
       01  ERR-EDIT-AREA.
           03  ERR-PTID                    PIC 9(10).
           03  ERR-PTNUM                   PIC X(20).
           03  ERR-ERRMSG                  PIC X(80).
           03  ERR-HEN-ERRMSG              PIC X(80).
      *
       01  ERR-MES-AREA.
           03  ERR-MES01                   PIC X(100)    VALUE
               "患者定期請求履歴データの取得に失敗しました".
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *    定期請求一括処理パラメータ
           COPY    "CPTEIKI1PARA.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    収納マスター
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    日別収納マスタ
       01  SYUDAY-REC.
           COPY    "CPSYUDAY.INC".
      *
      *    患者定期請求履歴
       01  PTTEIKIRRK-REC.
           COPY    "CPPTTEIKIRRK.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    定期請求用収納作成サブ
       01  LNK-ORCS02-REC.
           COPY  "CPORCS02.INC".
       01  LNK-SYUNOU-REC-T.
         02 LNK-SYUNOU-REC     OCCURS 10.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-// BY //LNK-SYU-//.
       01  LNK-SYUDAY-REC-T.
         02 LNK-SYUDAY-REC     OCCURS 10.
           COPY  "CPSYUDAY.INC"  REPLACING  //SDAY-// BY //LNK-SDAY-//.
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
      *
           COPY    "CPCOMMAND-PARAM.INC".
      *
      ****************************************************************
       PROCEDURE                       DIVISION
           USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-TERM-SEC
      *
           PERFORM 990-PROGRAM-FINISH-SEC
      *
           .
       000-PROC-EXT.
           EXIT.
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  INDEX-AREA
                                       CNT-AREA
                                       WRK-AREA
                                       SPA-AREA
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    TEIKI1P-ARG1
                                               TEIKI1P-SYOKBN
                                               TEIKI1P-KIKAN-OCC (1)
                                               TEIKI1P-KIKAN-OCC (2)
                                               TEIKI1P-KIKAN-OCC (3)
                                               TEIKI1P-JOBID
                                               TEIKI1P-SHELLID
                                               TEIKI1P-HOSPID
                                               TEIKI1P-ERRFILE
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    TEIKI1P-JOBID   TO  JOB-JOBID
           MOVE    TEIKI1P-SHELLID TO  JOB-SHELLID
           MOVE    "ORCT0030"      TO  JOB-PGID
           MOVE    "収納データチェック"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
      *
           MOVE    ZERO            TO  FLG-END
                                       FLG-ERR
      *
           MOVE    "TEIKI003"      TO  TEIKI03PARA-FILE-ID
           MOVE    TEIKI1P-TERMID  TO  TEIKI03PARA-TERMID
      *
           MOVE    "TEIKI004"      TO  TEIKI04PARA-FILE-ID
           MOVE    TEIKI1P-TERMID  TO  TEIKI04PARA-TERMID
      *
           MOVE    TEIKI1P-ERRFILE TO  TEIKIERR
      *
      *    請求取消対象収納ファイル初期化
           OPEN    OUTPUT              TEIKI04CL-FILE
           CLOSE   TEIKI04CL-FILE
      *
           OPEN    INPUT               TEIKI03-FILE
           OPEN    OUTPUT              TEIKI04-FILE
      *
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    ZERO            TO  FLG-TEIKI03
      *
           PERFORM 900-TEIKI03-READ-SEC
      *
           PERFORM UNTIL ( FLG-TEIKI03 NOT =   ZERO )
      *
      *        収納データチェック処理
               PERFORM 2001-SYUNOU-CHK-SEC
      *
               PERFORM 900-TEIKI03-READ-SEC
      *
           END-PERFORM
      *
           MOVE    1               TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納データチェック処理
      *****************************************************************
       2001-SYUNOU-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
           INITIALIZE   SYUNOU-REC
           MOVE    TEIKI03-HOSPID      TO  SYU-HOSPID
           MOVE     "1"                TO  SYU-NYUGAIKBN
           MOVE    TEIKI03-PTID        TO  SYU-PTID
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "SYUNOU-KEY2"       TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
      *    今回定期請求期間にかかる収納データを抽出する
           PERFORM UNTIL ( FLG-SYUNOU  NOT     =   ZERO )
      *
               IF    ( TEIKI03-SKYSTYMD       <=  SYU-SKYEDYMD )
                AND  ( TEIKI03-SKYEDYMD       >=  SYU-SKYSTYMD )
                   IF    ( SYU-DENPJTIKBN  NOT =   "3" )
                       PERFORM 20011-TEIKI04-OUTPUT-SEC
                   END-IF
               END-IF
      *
               MOVE    "SYUNOU-KEY2"   TO  ORC-DBPATH
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC TO  SYUNOU-REC
               ELSE
                   MOVE    1           TO  FLG-SYUNOU
               END-IF
      *
           END-PERFORM
      *
           MOVE    "SYUNOU-KEY2"       TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
       2001-SYUNOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求取消対象収納ファイル出力処理
      *****************************************************************
       20011-TEIKI04-OUTPUT-SEC      SECTION.
      *
           MOVE    TEIKI1P-TERMID      TO  SYU-TERMID
      *
           MOVE    SYUNOU-REC          TO  TEIKI04-SYU-DAT
      *
      *    日別収納マスタ検索処理
           PERFORM 900-SYUDAY-KEY-SEL-SEC
           IF    ( FLG-SYUDAY          =   ZERO )
               MOVE    1               TO  TEIKI04-SYUDAY-EXIST-FLG
               MOVE    SYUDAY-REC      TO  TEIKI04-SYUDAY-DAT
           ELSE
               MOVE    ZERO            TO  TEIKI04-SYUDAY-EXIST-FLG
               MOVE    SPACE           TO  TEIKI04-SYUDAY-DAT
           END-IF
      *
      *    患者定期請求履歴検索処理
           PERFORM 900-PTTEIKIRRK-KEY6-SEL-SEC
           IF    ( FLG-PTTEIKIRRK  NOT =   ZERO )
               MOVE    SYU-PTID        TO  ERR-PTID
               MOVE    ERR-MES01       TO  ERR-ERRMSG
               PERFORM 800-ERRMSG-EDIT-SEC
           END-IF
      *
           MOVE    SYU-HOSPID          TO  TEIKI04-HOSPID
           MOVE    SYU-PTID            TO  TEIKI04-PTID
           MOVE    PTTEIKIRRK-SAKKBN   TO  TEIKI04-SAKKBN
           MOVE    PTTEIKIRRK-SKYSTYMD TO  TEIKI04-SKYSTYMD
           MOVE    PTTEIKIRRK-SKYEDYMD TO  TEIKI04-SKYEDYMD
           MOVE    SYU-HKNCOMBINUM     TO  TEIKI04-HKNCOMBINUM
      *
           WRITE   TEIKI04-REC
      *
           COMPUTE CNT-OUT1    =   CNT-OUT1    +   1
      *
           .
       20011-TEIKI04-OUTPUT-EXT.
           EXIT. 
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    OUTPUT              TEIKIERR-FILE
      *
           MOVE    WRK-TEIKIERR        TO  TEIKIERR-REC
           WRITE   TEIKIERR-REC
           CLOSE   TEIKIERR-FILE
      *
      *    ジョブ管理開始処理
           MOVE    "JBE"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    TEIKI1P-JOBID       TO  JOB-JOBID
           MOVE    TEIKI1P-SHELLID     TO  JOB-SHELLID
           MOVE    WRK-TEIKIERR        TO  JOB-YOBI
           MOVE    "9999"              TO  JOB-ERRCD
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
      *
           MOVE    1                   TO  FLG-ERR
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
      *    プログラム終了処理
           PERFORM 990-PROGRAM-FINISH-SEC
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   TEIKI03-FILE
                   TEIKI04-FILE
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    TEIKI1P-JOBID   TO  JOB-JOBID
           MOVE    TEIKI1P-SHELLID TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *     
           DISPLAY "TEIKI01 (ORCT0030) CNT " CNT-OUT1 
           DISPLAY "*** ORCT0030 END ***"
      ***  ACCEPT  WRK-IN FROM CONSOLE 
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       800-ERRMSG-EDIT-SEC             SECTION.
      *
           MOVE    SPACE               TO  ERR-HEN-ERRMSG
	                                   ERR-PTNUM
      *
      *    患者番号を取得
           MOVE    ERR-PTID            TO  WRK-PTID
           PERFORM 900-PTNUM-KEY-SEL-SEC
           IF    ( FLG-PTNUM           =   ZERO )
               MOVE    PTNUM-PTNUM     TO  ERR-PTNUM
           ELSE
               STRING "?(ID:"          DELIMITED   BY  SIZE
                      ERR-PTID         DELIMITED   BY  SIZE
                      ")"              DELIMITED   BY  SIZE
               INTO   ERR-PTNUM
               END-STRING
           END-IF
      *
           MOVE    1                   TO  WRK-LEN
           MOVE    SPACE               TO  WRK-STR
           STRING  ERR-PTNUM           DELIMITED   BY  SPACE
           INTO    WRK-STR
           WITH  POINTER WRK-LEN
           END-STRING
      *
           COMPUTE WRK-LEN =   WRK-LEN -   1
      *
           MOVE    ZERO                TO  WRK-SHO
                                           WRK-AMARI
           MOVE    SPACE               TO  WRK-KUGIRI
      *
           DIVIDE  2   INTO    WRK-LEN GIVING  WRK-SHO
           REMAINDER WRK-AMARI
           END-DIVIDE
      *
           IF    ( WRK-AMARI   =   ZERO )
               MOVE    "："            TO  WRK-KUGIRI
           ELSE
               MOVE    ":"             TO  WRK-KUGIRI
           END-IF
      *
           STRING  ERR-ERRMSG          DELIMITED   BY  SPACE
                   "［患者番号"        DELIMITED   BY  SIZE
                   WRK-KUGIRI          DELIMITED   BY  SPACE
                   ERR-PTNUM           DELIMITED   BY  SPACE
                   "］"                DELIMITED   BY  SIZE
           INTO    ERR-HEN-ERRMSG
           END-STRING
      *
           MOVE    ERR-HEN-ERRMSG      TO  WRK-TEIKIERR
           PERFORM 500-ERR-HENSYU-SEC
      *
           .
       800-ERRMSG-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    日別収納マスタ検索処理
      *****************************************************************
       900-SYUDAY-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUDAY
      *
           INITIALIZE                      SYUDAY-REC
           MOVE    SYU-HOSPID          TO  SDAY-HOSPID
           MOVE    SYU-NYUGAIKBN       TO  SDAY-NYUGAIKBN
           MOVE    SYU-PTID            TO  SDAY-PTID
           MOVE    SYU-DENPNUM         TO  SDAY-DENPNUM
           MOVE    SYUDAY-REC          TO  MCPDATA-REC
           MOVE    "SYUDAY-KEY"        TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUDAY-REC
           ELSE
               MOVE    1               TO  FLG-SYUDAY
           END-IF
      *
           MOVE    "SYUDAY-KEY"        TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
       900-SYUDAY-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者定期請求履歴検索処理
      *****************************************************************
       900-PTTEIKIRRK-KEY6-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTTEIKIRRK
      *
           INITIALIZE                      PTTEIKIRRK-REC
      *
           MOVE    SYU-HOSPID          TO  PTTEIKIRRK-HOSPID
           MOVE    SYU-PTID            TO  PTTEIKIRRK-PTID
           MOVE    SYU-DENPNUM         TO  PTTEIKIRRK-DENPNUM
      *
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "PTTEIKIRRK-KEY6"   TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTTEIKIRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTTEIKIRRK
           END-IF
      *
           MOVE    "PTTEIKIRRK-KEY6"   TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
       900-PTTEIKIRRK-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    定期請求期間情報読込
      *****************************************************************
       900-TEIKI03-READ-SEC            SECTION.
      *
           READ    TEIKI03-FILE
           AT  END
               MOVE    1               TO  FLG-TEIKI03
           END-READ
      *
           .
       900-TEIKI03-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号マスター読込（主キー）
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
      *
           INITIALIZE                  PTNUM-REC
           MOVE    TEIKI1P-HOSPID      TO  PTNUM-HOSPID
           MOVE    WRK-PTID            TO  PTNUM-PTID
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "PTNUM-KEY"         TO  ORC-DBPATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNUM-REC
           ELSE
               MOVE    1               TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
      *
           MOVE    "PTNUM-KEY"         TO  ORC-DBPATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
       900-PTNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
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
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
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
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
      *
       910-DBCLOSE-EXT.
           EXIT.
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
      *****************************************************************
      *    プログラム終了処理
      *****************************************************************
       990-PROGRAM-FINISH-SEC          SECTION.
      *
            STOP   RUN
      *
           .
       990-PROGRAM-FINISH-EXT.
           EXIT.
