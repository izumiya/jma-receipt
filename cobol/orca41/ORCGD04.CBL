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
      *
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGD04.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データチェックサブ
      *  コンポーネント名  : チェック確認振り分けデータ印刷イメージセット印刷
      *  管理者            : 
      *  作成日付    作業者        記述
      *  01.07.13    ひさなが 　   新規作成
      *  01.10.04    森脇          明細２段追加
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    ひさなが     01/10/15  印刷順を患者番号へ変更
      *  01.00.01    ひさなが     02/03/01  印刷順、印刷グループの変更
      *  01.00.01    ひさなが     02/03/05  診療年が和暦の場合西暦変換
      *  01.00.02    ひさなが     02/05/07  半角チェックの追加
      *  01.00.02    ひさなが     02/05/10  ジョブ管理検索追加
      *  01.00.02    ひさなが     02/06/26  印刷管理・印刷ＤＢ追加
      *  01.00.05    ひさなが     02/09/13  ディレクトリをサブプロへ
      *  01.00.06    ひさなが     03/03/03  出力範囲の桁誤り変更
      *  01.00.07    太田         03/03/31  SHELL引数の受け渡しを
      *                                     コピー句を用いて行うよう修正
      *
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    チェク条件用ファイル
           SELECT  DID1-FILE       ASSIGN  DID1PARA
                                   FILE    STATUS  IS  STS-DID1.
      *    振り分け用ファイル１
           SELECT  DID5-FILE       ASSIGN  DID5PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  DID5-KEY
                                   FILE    STATUS  IS  STS-DID5.
      *    振り分け用ファイル２
           SELECT  DID6-FILE       ASSIGN  DID6PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  DID6-KEY
                                   FILE    STATUS  IS  STS-DID6.
      *    振り分け用ファイル３
           SELECT  DID7-FILE       ASSIGN  DID7PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DID7-KEY
                                   FILE    STATUS  IS  STS-DID7.
      *    振り分け用ファイル４
           SELECT  DID9-FILE       ASSIGN  DID9PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DID9-KEY
                                   FILE    STATUS  IS  STS-DID9.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    チェク条件用ファイル
       FD  DID1-FILE.
       01  DID1-REC                PIC X(200). 
      *    振り分け用ファイル１
       FD  DID5-FILE.
       01  DID5-REC. 
           COPY    "CPRECORCGD06.INC"
                                   REPLACING  //DID6//
                                   BY         //DID5//.
      *    振り分け用ファイル２
       FD  DID6-FILE.
       01  DID6-REC. 
           COPY    "CPRECORCGD06.INC".
      *    振り分け用ファイル３
       FD  DID7-FILE.
       01  DID7-REC. 
           COPY    "CPRECORCGD06.INC"
                                   REPLACING  //DID6//
                                   BY         //DID7//.
      *    振り分け用ファイル４
       FD  DID9-FILE.
       01  DID9-REC. 
           COPY    "CPRECORCGD06.INC"
                                   REPLACING  //DID6//
                                   BY         //DID9//.
      *
       WORKING-STORAGE             SECTION.
      *
      *    チェク条件用ファイル 名称領域 
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //ORCGD//
                                   BY         //DID1//.
      *
      *    振り分け用ファイル１ 名称領域 
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //ORCGD//
                                   BY         //DID5//.
      *
      *    振り分け用ファイル２ 名称領域 
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //ORCGD//
                                   BY         //DID6//.
      *
      *    振り分け用ファイル３ 名称領域
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //ORCGD//
                                   BY         //DID7//.
      *
      *    振り分け用ファイル４ 名称領域
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //ORCGD//
                                   BY         //DID9//.
      *
      *    システム時刻用領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    診療年月領域
       01  T-SINRYOU-AREA.
           03  T-SINRYOU-G         PIC X(04).
           03  T-SINRYOU-Y         PIC X(02).
           03  T-SINRYOU-M         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
      *    エンドステータス領域
       01  STS-AREA.
           03  STS-DID1            PIC X(02).
           03  STS-DID5            PIC X(02).
           03  STS-DID6            PIC X(02).
           03  STS-DID7            PIC X(02).
           03  STS-DID9            PIC X(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(03).
           03  IDX2                PIC 9(03).
           03  IDX3                PIC 9(03).
           03  IDX4                PIC 9(03).
      *
      *    連番、頁、編集作業
       01  W-AREA.
           03  W-RENBAN            PIC 9(05).
           03  W-PAGE              PIC 9(03).
           03  W-HEN02-X.
               05  W-HEN02-C       PIC Z9.
           03  W-HEN05-X.
               05  W-HEN05-C       PIC ZZZZ9.
           03  W-HEN10-X.
               05  W-HEN10-C       PIC ZZZZZZZZZ9.
           03  W-HEN03-X.
               05  W-HEN03-C       PIC ZZ9.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                  PIC X(06).
               05  WRK-PARA-TERMID                 PIC X(64).
               05  WRK-PARA-SYSYMD                 PIC X(08).
               05  WRK-PARA-OPID                   PIC X(16).
               05  WRK-PARA-SYOKBN                 PIC X(01).
               05  WRK-PARA-RECEKBN                PIC X(01).
      *
      *    印刷イメージ作業（印刷用ｄｅｆ、印刷用ＩＮＣの並びと同じにする）
           COPY    "HC401.INC"
                                   REPLACING  //HC401//
                                   BY         //W-HC401//.
      *
      *    シェル用領域
           COPY    "CPSHELLTBL.INC".
           COPY    "ORCA-DBPATH".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    テーブルアクセス
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ジョブ管理
       01  WRK-JOBKANRI-AREA.
           03  WRK-JOBKANRI-MODE       PIC X(03).
           03  WRK-JOBKANRI-RETURN     PIC S9(03).
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    フィアルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *    帳票ＤＢ印刷サブコピー句
           COPY    "CPORCSPRTLNK.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
      *
        LINKAGE                     SECTION.
      *
        01  COMMAND-PARAM.
            02  FILLSE              PIC X(2000).
      *
      *
       PROCEDURE                   DIVISION    USING
                                               COMMAND-PARAM.
      *****************************************************************
      *    主    処    理
      *****************************************************************
      *
       000-PROC-SEC                SECTION.
      *    初期処理
           PERFORM T-000-CHEKINIT-SEC.
      *    チェック確認振り分けデータ１ 読み込み処理
           PERFORM T-100-CHEKCHEK-SEC  UNTIL   FLG-END =   1.
      *    チェック確認振り分けデータ３ 読み込み処理
           PERFORM T-150-CHEKCHEK-SEC  UNTIL   FLG-END =   2.
      *    チェック確認振り分けデータ２ 読み込み処理
           PERFORM T-200-CHEKCHEK-SEC  UNTIL   FLG-END =   3.
      *    チェック確認振り分けデータ４ 読み込み処理
           PERFORM T-400-CHEKCHEK-SEC  UNTIL   FLG-END =   4.
      *    終了処理
           PERFORM T-000-CHEKTERM-SEC.
      *
       000-PROC-SEC-EXT.
           STOP    RUN.
      *
      *****************************************************************
      *    初　期　処　理
      *****************************************************************
       T-000-CHEKINIT-SEC                SECTION.
      *    外部ファイル名 設定処理
           PERFORM T-000-CHEKSETT-SEC.
      *
           ACCEPT  SYS-TIME            FROM    TIME.
      *
           OPEN    INPUT                   DID1-FILE.
      *    診療年月 入力処理
           PERFORM T-000-CHEKCHEK-SEC  VARYING IDX1  FROM  1  BY 1
                                       UNTIL   IDX1  >     2.
      *
           CLOSE                           DID1-FILE.
      *
           OPEN    INPUT                   DID5-FILE.
           OPEN    INPUT                   DID6-FILE.
           OPEN    INPUT                   DID7-FILE.
           OPEN    INPUT                   DID9-FILE.
           MOVE    ZERO                TO  FLG-END.
           MOVE    ZERO                TO  W-PAGE.
           MOVE    ZERO                TO  W-RENBAN.
           MOVE    SPACE               TO  W-HC401.
           MOVE    1                   TO  IDX1.
      *    出力先プリンタ名割り当て情報編集
           PERFORM T-000-DBOPEN-SEC.
           MOVE    SPACE               TO  SYS-1031-REC.
           INITIALIZE                  SYS-1031-REC.
           MOVE    "1031"              TO  SYS-1031-KANRICD.
           MOVE    "*"                 TO  SYS-1031-KBNCD.
           MOVE    SYS-1031-REC        TO  MCPDATA-REC.
           PERFORM T-000-SYSKANRI-READ-SEC.
           IF  FLG-SYSKANRI   =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1031-REC
           ELSE
               INITIALIZE                  SYS-1031-REC
           END-IF.
      *
           INITIALIZE                      WRK-JOBKANRI-AREA.
           MOVE    "CHK"               TO  WRK-JOBKANRI-MODE.
           INITIALIZE                      JOBKANRI-REC.
           MOVE    1                   TO  JOB-JOBID.
           MOVE    "ORCBSD1"           TO  JOB-SHELLID.
           CALL    "ORCSJOB"           USING
                                       WRK-JOBKANRI-AREA  
                                       JOBKANRI-REC.
           IF  WRK-JOBKANRI-RETURN   =   ZERO
               INITIALIZE                      WRK-JOBKANRI-AREA
               MOVE    "STS"               TO  WRK-JOBKANRI-MODE
               MOVE    1                   TO  JOB-JOBID
               MOVE    "ORCBSD1"           TO  JOB-SHELLID
               CALL    "ORCSJOB"           USING
                                           WRK-JOBKANRI-AREA
                                           JOBKANRI-REC
           END-IF.
      *
       T-000-CHEKINITK-EXT.
           EXIT.
      *
      *****************************************************************
      *    終　了　処　理
      *****************************************************************
       T-000-CHEKTERM-SEC                SECTION.
      *
           IF  IDX1 >  1
                  PERFORM T-300-CHEKWEITE-SEC
           END-IF.
      *
           PERFORM T-310-CHEKWEITE-SEC.
      *
           PERFORM T-000-DBCLOSE-SEC.
      *
           CLOSE                           DID5-FILE.
           CLOSE                           DID6-FILE.
           CLOSE                           DID7-FILE.
           CLOSE                           DID9-FILE.
      *
       T-000-CHEKTERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    外部ファイル名 設定処理
      *****************************************************************
       T-000-CHEKSETT-SEC                SECTION.
      *
           MOVE    COMMAND-PARAM       TO  WRK-PARA.
      *
           MOVE    "ORCGD1"            TO  DID1PARA-FILE-ID.
           MOVE    WRK-PARA-TERMID     TO  DID1PARA-TERMID.
      *
           MOVE    "ORCGD5"            TO  DID5PARA-FILE-ID.
           MOVE    WRK-PARA-TERMID     TO  DID5PARA-TERMID.
      *
           MOVE    "ORCGD6"            TO  DID6PARA-FILE-ID.
           MOVE    WRK-PARA-TERMID     TO  DID6PARA-TERMID.
      *
           MOVE    "ORCGD7"            TO  DID7PARA-FILE-ID.
           MOVE    WRK-PARA-TERMID     TO  DID7PARA-TERMID.
      *
           MOVE    "ORCGD9"            TO  DID9PARA-FILE-ID.
           MOVE    WRK-PARA-TERMID     TO  DID9PARA-TERMID.
      *
       T-000-CHEKCHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月 入力処理
      *****************************************************************
       T-000-CHEKCHEK-SEC                SECTION.
      *
           READ    DID1-FILE
                   AT   END
                        MOVE     3       TO    IDX1
                   NOT  AT  END
                        IF  IDX1   =   2
                            PERFORM T-000-CNV-YM-SEC
                        END-IF
           END-READ.
      *
       T-000-CHEKCHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月 変換処理
      *****************************************************************
       T-000-CNV-YM-SEC                  SECTION.
      *
           INITIALIZE                    STS-AREA-DAY.
           INITIALIZE                    LNK-DAY2-AREA.
           MOVE    ZERO                  TO  LNK-DAY2-YMD.
           MOVE    "21"                  TO  LNK-DAY2-IRAI.
           MOVE    DID1-REC(7:6)         TO  LNK-DAY2-YMD(1:6).
           MOVE    "01"                  TO  LNK-DAY2-YMD(7:2).
      *
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA.
      *
           IF  STS-DAY-RC1   NOT =   ZERO
               MOVE    SPACE               TO  T-SINRYOU-AREA
           ELSE
               MOVE    LNK-DAY2-EDTYMD2-G  TO  T-SINRYOU-G
               MOVE    LNK-DAY2-EDTYMD1-Y  TO  T-SINRYOU-Y
               MOVE    LNK-DAY2-EDTYMD1-M  TO  T-SINRYOU-M
           END-IF.
      *
       T-000-CNV-YM-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       T-000-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    WRK-PARA-SYSYMD     TO  ORC-DBYMD.
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
           IF  MCP-RC   =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF  MCP-RC   =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF.
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC.
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-000-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       T-000-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-000-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       T-000-DBCLOSE-SEC               SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
       T-000-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック確認振り分けデータ１ 読み込み処理
      *****************************************************************
       T-100-CHEKCHEK-SEC                SECTION.
      *
           READ    DID5-FILE    NEXT
                   AT   END
                        MOVE     1                     TO    FLG-END
                   NOT  AT  END
                        PERFORM T-100-CHEKWEITE-SEC
           END-READ.
      *
       T-100-CHEKCHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック確認振り分けデータ印刷イメージ１ セット処理
      *****************************************************************
       T-100-CHEKWEITE-SEC               SECTION.
      *
           IF  IDX1 >  20
               PERFORM T-300-CHEKWEITE-SEC
           END-IF.
      *
           COMPUTE     W-RENBAN     =     W-RENBAN     +     1.
           MOVE    W-RENBAN             TO    W-HEN05-C.
           MOVE    W-HEN05-X            TO    W-HC401-RENBAN (IDX1).
           MOVE    20                   TO    IDX2.
           PERFORM UNTIL   IDX2  <     1
                   IF  DID5-PTNUM(IDX2:1) NOT =  SPACE
                       MOVE    IDX2                   TO    IDX3
                       MOVE    ZERO                   TO    IDX2
                   ELSE
                       COMPUTE IDX2  =  IDX2  -  1
                   END-IF
           END-PERFORM.
           MOVE    20                       TO    IDX4.
           PERFORM UNTIL   IDX3  <     1
                   MOVE    DID5-PTNUM(IDX3:1)
                           TO    W-HC401-PTID (IDX1)(IDX4:1)
                   COMPUTE IDX3  =  IDX3  -  1
                   COMPUTE IDX4  =  IDX4  -  1
           END-PERFORM.
           MOVE    DID5-NAME(1:30)       TO    W-HC401-NAME (IDX1).
           MOVE    DID5-NENREI           TO    W-HEN03-C.
           MOVE    W-HEN03-X             TO    W-HC401-NENREI (IDX1).
           MOVE    DID5-MESG(77:90)      TO    W-HC401-MESG (IDX1).
           COMPUTE     IDX1     =     IDX1     +     1.
      *
       T-100-CHEKWEITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック確認振り分けデータ３ 読み込み処理
      *****************************************************************
       T-150-CHEKCHEK-SEC                SECTION.
      *
           READ    DID7-FILE    NEXT
                   AT   END
                        MOVE     2                     TO    FLG-END
                   NOT  AT  END
                        PERFORM T-150-CHEKWEITE-SEC
           END-READ.
      *
       T-150-CHEKCHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック確認振り分けデータ印刷イメージ３ セット処理
      *****************************************************************
       T-150-CHEKWEITE-SEC               SECTION.
      *
           IF  IDX1 >  20
               PERFORM T-300-CHEKWEITE-SEC
           END-IF.
      *
           COMPUTE     W-RENBAN     =     W-RENBAN     +     1.
           MOVE    W-RENBAN             TO    W-HEN05-C.
           MOVE    W-HEN05-X            TO    W-HC401-RENBAN (IDX1).
           MOVE    20                   TO    IDX2.
           PERFORM UNTIL   IDX2  <     1
                   IF  DID7-PTNUM(IDX2:1) NOT =  SPACE
                       MOVE    IDX2                   TO    IDX3
                       MOVE    ZERO                   TO    IDX2
                   ELSE
                       COMPUTE IDX2  =  IDX2  -  1
                   END-IF
           END-PERFORM.
           MOVE    20                       TO    IDX4.
           PERFORM UNTIL   IDX3  <     1
                   MOVE    DID7-PTNUM(IDX3:1)
                           TO    W-HC401-PTID (IDX1)(IDX4:1)
                   COMPUTE IDX3  =  IDX3  -  1
                   COMPUTE IDX4  =  IDX4  -  1
           END-PERFORM.
           MOVE    DID7-NAME(1:30)       TO    W-HC401-NAME (IDX1).
           MOVE    DID7-NENREI           TO    W-HEN03-C.
           MOVE    W-HEN03-X             TO    W-HC401-NENREI (IDX1).
           MOVE    DID7-MESG(77:90)      TO    W-HC401-MESG (IDX1).
           COMPUTE     IDX1     =     IDX1     +     1.
      *
       T-150-CHEKWEITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック確認振り分けデータ２ 読み込み処理
      *****************************************************************
       T-200-CHEKCHEK-SEC                SECTION.
      *
           READ    DID6-FILE    NEXT
                   AT   END
                        MOVE     3                     TO    FLG-END
                   NOT  AT  END
                        PERFORM T-200-CHEKWEITE-SEC
           END-READ.
      *
       T-200-CHEKCHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック確認振り分けデータ印刷イメージ２ セット処理
      *****************************************************************
       T-200-CHEKWEITE-SEC               SECTION.
      *
           IF  IDX1 >  20
               PERFORM T-300-CHEKWEITE-SEC
           END-IF.
      *
           COMPUTE     W-RENBAN     =     W-RENBAN     +     1.
           MOVE    W-RENBAN              TO    W-HEN05-C.
           MOVE    W-HEN05-X             TO    W-HC401-RENBAN (IDX1).
           MOVE    20                    TO    IDX2.
           PERFORM UNTIL   IDX2  <     1
                   IF  DID6-PTNUM(IDX2:1) NOT =  SPACE
                       MOVE    IDX2                     TO    IDX3
                       MOVE    ZERO                     TO    IDX2
                   ELSE
                       COMPUTE IDX2  =  IDX2  -  1
                   END-IF
           END-PERFORM.
           MOVE    20                       TO    IDX4.
           PERFORM UNTIL   IDX3  <     1
                   MOVE    DID6-PTNUM(IDX3:1)
                           TO    W-HC401-PTID (IDX1)(IDX4:1)
                   COMPUTE IDX3  =  IDX3  -  1
                   COMPUTE IDX4  =  IDX4  -  1
           END-PERFORM.
           MOVE    DID6-NAME(1:30)       TO    W-HC401-NAME (IDX1).
           MOVE    DID6-NENREI           TO    W-HEN03-C.
           MOVE    W-HEN03-X             TO    W-HC401-NENREI (IDX1).
           MOVE    DID6-MESG(77:90)      TO    W-HC401-MESG (IDX1).
           COMPUTE     IDX1     =     IDX1     +     1.
      *
       T-200-CHEKWEITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック確認振り分けデータ４ 読み込み処理
      *****************************************************************
       T-400-CHEKCHEK-SEC                SECTION.
      *
           READ    DID9-FILE    NEXT
                   AT   END
                        MOVE     4                     TO    FLG-END
                   NOT  AT  END
                        PERFORM T-400-CHEKWEITE-SEC
           END-READ.
      *
       T-400-CHEKCHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック確認振り分けデータ印刷イメージ４ セット処理
      *****************************************************************
       T-400-CHEKWEITE-SEC               SECTION.
      *
           IF  IDX1 >  20
               PERFORM T-300-CHEKWEITE-SEC
           END-IF.
      *
           COMPUTE     W-RENBAN     =     W-RENBAN     +     1.
           MOVE    W-RENBAN              TO    W-HEN05-C.
           MOVE    W-HEN05-X             TO    W-HC401-RENBAN (IDX1).
           MOVE    20                    TO    IDX2.
           PERFORM UNTIL   IDX2  <     1
                   IF  DID9-PTNUM(IDX2:1) NOT =  SPACE
                       MOVE    IDX2                     TO    IDX3
                       MOVE    ZERO                     TO    IDX2
                   ELSE
                       COMPUTE IDX2  =  IDX2  -  1
                   END-IF
           END-PERFORM.
           MOVE    20                       TO    IDX4.
           PERFORM UNTIL   IDX3  <     1
                   MOVE    DID9-PTNUM(IDX3:1)
                           TO    W-HC401-PTID (IDX1)(IDX4:1)
                   COMPUTE IDX3  =  IDX3  -  1
                   COMPUTE IDX4  =  IDX4  -  1
           END-PERFORM.
           MOVE    DID9-NAME(1:30)       TO    W-HC401-NAME (IDX1).
           MOVE    DID9-NENREI           TO    W-HEN03-C.
           MOVE    W-HEN03-X             TO    W-HC401-NENREI (IDX1).
           MOVE    DID9-MESG(77:90)      TO    W-HC401-MESG (IDX1).
           COMPUTE     IDX1     =     IDX1     +     1.
      *
       T-400-CHEKWEITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック確認振り分けデータ 出力処理
      *****************************************************************
       T-300-CHEKWEITE-SEC               SECTION.
      *
           COMPUTE     W-PAGE   =   W-PAGE   +   1.
      *
           MOVE    T-SINRYOU-G         TO  W-HC401-SINRYOU-G.
           MOVE    T-SINRYOU-Y         TO  W-HC401-SINRYOU-Y.
           MOVE    T-SINRYOU-M         TO  W-HC401-SINRYOU-M.
           MOVE    W-PAGE              TO  W-HEN03-C.
           MOVE    W-HEN03-X           TO  W-HC401-PAGE.
      *
      *    印刷用スクリプト処理
           INITIALIZE                  ORCSPRTAREA.
           MOVE    "INS"               TO  SPRT-MODE.
           MOVE    1                   TO  SPRT-RENNUM.
           MOVE    "ORCBSD1"           TO  SPRT-TBL-KEY.
           MOVE    WRK-PARA-SYSYMD     TO  SPRT-TBL-GROUP(1:8).
      *修正/*開始*03.03.03**********************************************
      *     MOVE    SYS-TIME            TO  SPRT-TBL-GROUP(9:8).
           MOVE    SYS-TIME(1:6)       TO  SPRT-TBL-GROUP(9:6).
      *修正/*終了*03.03.03**********************************************
           MOVE    WRK-PARA-SRYYM      TO  SPRT-SRYYM.
           MOVE    WRK-PARA-SYSYMD     TO  SPRT-SKYYMD.
           MOVE    "orcbsd01.sh"       TO  SPRT-SHELLID.
           MOVE    1                   TO  SPRT-SHORI-RENNUM.
           MOVE    ZERO                TO  SPRT-PRIORITY.
           MOVE    "HC401"             TO  SPRT-PRTID.
           MOVE    LNK-DAY2-EDTYMD3(1:16)  TO  SPRT-TITLE.
           MOVE    "分チェックリスト"  TO  SPRT-TITLE(17:16).       
           MOVE    W-HC401             TO  SPRT-PRTDATA.     
           MOVE    WRK-PARA-TERMID     TO  SPRT-TERMID.
           MOVE    WRK-PARA-OPID       TO  SPRT-OPID.
           MOVE    SYS-1031-PRTNM (5)  TO  SPRT-PRTNM.
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA.
      *
           MOVE    1                   TO  IDX1.
           MOVE    SPACE               TO  W-HC401.
      *
           INITIALIZE                      WRK-JOBKANRI-AREA.
           MOVE    "CHK"               TO  WRK-JOBKANRI-MODE.
           INITIALIZE                      JOBKANRI-REC.
           MOVE    1                   TO  JOB-JOBID.
           MOVE    "ORCBSD1"           TO  JOB-SHELLID.
           CALL    "ORCSJOB"           USING
                                       WRK-JOBKANRI-AREA  
                                       JOBKANRI-REC.
           IF  WRK-JOBKANRI-RETURN   =   ZERO
               INITIALIZE                      WRK-JOBKANRI-AREA
               MOVE    "STE"               TO  WRK-JOBKANRI-MODE
               MOVE    1                   TO  JOB-JOBID
               MOVE    "ORCBSD1"           TO  JOB-SHELLID
               MOVE    1                   TO  JOB-UPDCNT
               CALL    "ORCSJOB"           USING
                                           WRK-JOBKANRI-AREA
                                           JOBKANRI-REC
           END-IF.
      *
       T-300-CHEKWEITE-EXT.
           EXIT.
      *
      *******************
      *
       T-310-CHEKWEITE-SEC               SECTION.
      *
           MOVE    "CHK"               TO  WRK-JOBKANRI-MODE.
           INITIALIZE                      JOBKANRI-REC.
           MOVE    2                   TO  JOB-JOBID.
           MOVE    "ORCBSD1"           TO  JOB-SHELLID.
           MOVE    WRK-PARA-TERMID     TO  JOB-TERMID.
           CALL    "ORCSJOB"           USING
                                       WRK-JOBKANRI-AREA  
                                       JOBKANRI-REC.
           IF  WRK-JOBKANRI-RETURN   =   ZERO
               INITIALIZE                      WRK-JOBKANRI-AREA
               MOVE    "DEL"               TO  WRK-JOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    2                   TO  JOB-JOBID
               MOVE    "ORCBSD1"           TO  JOB-SHELLID
               MOVE    WRK-PARA-TERMID     TO  JOB-TERMID
               CALL    "ORCSJOB"           USING
                                           WRK-JOBKANRI-AREA
                                           JOBKANRI-REC
           END-IF.
           INITIALIZE                      WRK-JOBKANRI-AREA.
           MOVE    "JBS"               TO  WRK-JOBKANRI-MODE.
           INITIALIZE                      JOBKANRI-REC.
           MOVE    2                   TO  JOB-JOBID.
           MOVE    "ORCBSD1"           TO  JOB-SHELLID.
           MOVE    WRK-PARA-TERMID     TO  JOB-TERMID,
           MOVE    "リスト印刷"        TO  JOB-SHELLMSG.
           CALL    "ORCSJOB"           USING
                                       WRK-JOBKANRI-AREA
                                       JOBKANRI-REC.
      *
           MOVE    SPACE               TO  SHELLTBL.
           INITIALIZE                  ORCSMKPASSAREA.
           MOVE    "scripts/allways/print.sh"
                                       TO  MKPASS-IN-01.
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA.
           MOVE    MKPASS-OT-01        TO  SHELLTBL-NAME.
      *
           MOVE    SPACE               TO  LNK-PRTKANRI-AREA
           INITIALIZE                      LNK-PRTKANRI-AREA
           MOVE    1                   TO  LNK-PRTKANRI-RENNUM
           MOVE    "ORCBSD1"           TO  LNK-PRTKANRI-TBL-KEY  
           MOVE    WRK-PARA-SYSYMD     TO  LNK-PRTKANRI-TBL-GROUP (1:8)
           MOVE    SYS-TIME            TO  LNK-PRTKANRI-TBL-GROUP (9: )
           MOVE    1                   TO  LNK-PRTKANRI-SHORI-RENNUM
           MOVE    WRK-PARA-SRYYM      TO  LNK-PRTKANRI-SRYYM
           MOVE    WRK-PARA-SYSYMD     TO  LNK-PRTKANRI-SKYYMD
           MOVE    "orcbsd01"          TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO                TO  LNK-PRTKANRI-PRIORITY
           MOVE    WRK-PARA-TERMID     TO  LNK-PRTKANRI-TERMID
           MOVE    WRK-PARA-OPID       TO  LNK-PRTKANRI-OPID
           MOVE    SYS-1031-PRTNM (5)  TO  LNK-PRTKANRI-PRTNM
      *
           MOVE    LNK-PRTKANRI-RENNUM TO  SHELLTBL-ARG1
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SHELLTBL-ARG2
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SHELLTBL-ARG3
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SHELLTBL-ARG4
           MOVE    LNK-PRTKANRI-SRYYM  TO  SHELLTBL-ARG5
           MOVE    LNK-PRTKANRI-SKYYMD TO  SHELLTBL-ARG6
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SHELLTBL-ARG7
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SHELLTBL-ARG8
           MOVE    LNK-PRTKANRI-TERMID TO  SHELLTBL-ARG9
           MOVE    LNK-PRTKANRI-OPID   TO  SHELLTBL-ARG10
           MOVE    LNK-PRTKANRI-PRTNM  TO  SHELLTBL-ARG11
      *
           MOVE    "/var/tmp/ERR.dat"  TO  SHELLTBL-ARG12.
           MOVE    "0000002"           TO  SHELLTBL-ARG13.
           MOVE    "ORCBSD1"           TO  SHELLTBL-ARG14.
           MOVE    "00000"             TO  SHELLTBL-ARG15.
           MOVE    "99999"             TO  SHELLTBL-ARG16.
           MOVE    SHELLTBL            TO  MCPDATA-REC.
           MOVE    "SHELL"             TO  MCP-FUNC.
           MOVE    PATH-SHELL-SHELL    TO  MCP-PATH.
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC.
      *
       T-310-CHEKWEITE-EXT.
           EXIT.
