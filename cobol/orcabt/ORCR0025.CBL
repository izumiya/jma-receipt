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
       PROGRAM-ID.             ORCR0025.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−小豆沢   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *  01.00.01    MCC-小豆沢   01.12.18  診療実日数のカウント修正
      *  01.00.02    MCC-小豆沢   02.02.13  請求点数計算時に薬剤料低減
      *                                     分の減算
      *  01.00.03   MCC-藤原      02.02.13  保険組合せを３から１０に変更
      *  01.00.04   MCC-小豆沢    02.03.28  請求点数集計時に自費分を集計
      *                                     しないように修正
      *  01.00.05    MCC-小豆沢   02.07.08  診療実日数算定用のシステム予約
      *                                     コードをから実日数の加算 
      *  01.00.06    MCC-小豆沢   02.09.02  診療実日数算定時のフラグクリア
      *                                     を追加 
      *  01.00.07    NACL-門脇    02.11.25  診療実日数算定のコード追加
      *  01.00.08    NACL-藤原    03.03.03  診療科数１０から２０へ変更
      *****************************************************************
      *
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE02-FILE     ASSIGN  RECE01PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE02-KEY
                                   FILE    STATUS  IS  STS-RECE02.
006900*    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE02-FILE.
       01  RECE02-REC. 
           COPY    "CPRCF002.INC".
      *
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC".
      *
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE02                              PIC X(02).
           03  STS-RECEERR                             PIC X(02).
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-SRYACT                              PIC 9(01).
           03  FLG-TENSU                               PIC 9(01).
           03  FLG-SHOSAISIN                           PIC 9(01).
           03  FLG-RECE02                              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-TBL-OCC                             OCCURS  2.
               05  CNT-IN                              PIC 9(06).
               05  CNT-MOD                             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                                     PIC 9(03).
           03  IDX1                                    PIC 9(03). 
           03  IDX2                                    PIC 9(03).
           03  IDX3                                    PIC 9(03).
           03  HC-IDX                                  PIC 9(03).
           03  KH-IDX                                  PIC 9(03).
           03  HKN-IDX                                 PIC 9(02).
           03  DAY-IDX                                 PIC 9(02).
           03  ACCT-IDX                                PIC 9(02).
           03  SRYACT-IDX                              PIC 9(02).
           03  IDZ                                     PIC 9(02).
      *
      *    パス名領域
       01  WK-PATH                                     PIC X(20).
      *
      *    実日数カウント用領域
       01  TBL-JNISSU.
           03  TBL-JNISSU-HKN       OCCURS   5.
               05  TBL-JNISSU-DAY                      PIC X(01)
                                                       OCCURS   31.
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-IN                                  PIC X(01).
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                      PIC  X(06).
               05  WRK-PARA-TERMID                     PIC  X(16).
               05  WRK-PARA-SYSYMD                     PIC  X(08).
               05  WRK-PARA-JOBID                      PIC  9(07).
               05  WRK-PARA-SHELLID                    PIC  X(08).
      *
       01  WRK-CONS-AREA.    
      *    固定端末ＩＤ
           03  WRK-CONS-TERMID     PIC X(64)   VALUE
                                   "0100007FZZZZZZZZZZZZZZZZZZZZZZZZ". 
      *
           03  WRK-RECEERR             PIC X(200). 
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1 
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
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
      *
           MOVE    ZERO                TO  FLG-END
      *
           MOVE    COMMAND-PARAM       TO  WRK-PARA
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "ORCR0025"      TO  JOB-PGID
           MOVE    "診療実日数編集"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECEERR-TERMID
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
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       2
                   OR      FLG-END     =   1
      *
               IF      IDZ             =   1               
      *    レセプト処理
                   MOVE    "RECE020"           TO  RECE01PARA-FILE-ID
                   MOVE    WRK-PARA-TERMID     TO  RECE01PARA-TERMID
               ELSE
      *    請求書処理
                   MOVE    "RECE02X"           TO  RECE01PARA-FILE-ID
                   MOVE    WRK-CONS-TERMID     TO  RECE01PARA-TERMID
               END-IF
      *
               PERFORM 200-HENSYU-SEC
           END-PERFORM 
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-HENSYU-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-RECE02
      *
           OPEN    I-O                 RECE02-FILE
      *
           PERFORM    RECE02-READ-SEC
      *
           PERFORM                     UNTIL   FLG-RECE02  =   1
               PERFORM     210-HENSYU-SEC
      *
               PERFORM     RECE02-READ-SEC
           END-PERFORM
      *
           CLOSE   RECE02-FILE
           .
       200-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    編集処理
      *****************************************************************
       210-HENSYU-SEC                   SECTION.
      *
           MOVE       SPACE             TO     TBL-JNISSU
           INITIALIZE                          TBL-JNISSU
      *
           PERFORM    VARYING    IDX    FROM   1   BY   1
                      UNTIL     (IDX      >   20)  OR
                                (RECE02-SRYKA(IDX)   =   SPACE)
               PERFORM    RECE02-HENSYU-SEC
           END-PERFORM
      *
           PERFORM     JNISSU-SET-SEC
      *
           PERFORM     RECE02-REWRITE-SEC
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの編集（Ｆ００２）
      *****************************************************************
       RECE02-HENSYU-SEC                SECTION.
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE02-HOSPID        TO      ACCT-HOSPID
           MOVE    RECE02-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE02-PTID          TO      ACCT-PTID
           MOVE    RECE02-SRYKA(IDX)    TO      ACCT-SRYKA
           MOVE    RECE02-SRYYM         TO      ACCT-SRYYM
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "SRYACCT-KEY2"       TO      ORC-DBPATH
           CALL    "ORCMCPSUB"           USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "SRYACCT-KEY2"   TO      ORC-DBPATH
               PERFORM  SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
      **********   IF   (ACCT-HKNCOMBI  =  RECE02-HKNCOMBI(1)) OR
      **********        (ACCT-HKNCOMBI  =  RECE02-HKNCOMBI(2)) OR
      **********        (ACCT-HKNCOMBI  =  RECE02-HKNCOMBI(3))
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       10
                           OR      RECE02-HKNCOMBI (IDX1)  =   ZERO 
                       IF      ACCT-HKNCOMBI   =
                                               RECE02-HKNCOMBI (IDX1)
                           IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                                (ACCT-ZAIKAISU  NOT =    ZERO)
                               MOVE    ZERO     TO    FLG-SHOSAISIN
                               IF   ACCT-SRYKBN  =  "11"  OR  "12"  OR
                                                    "13"  OR  "14"
                                   PERFORM   SHOSAISIN-HANTEI-SEC
                               END-IF
                               PERFORM   RECE02-TENSOU-SEC
                           END-IF
                           IF   (ACCT-ZAITEN    =        ZERO)    AND
                                (ACCT-ZAIKAISU  NOT =    ZERO)
                               MOVE    ZERO     TO    FLG-SHOSAISIN
                               IF   ACCT-SRYKBN  =  "11"  OR  "12" 
                                                          OR  "99" 
                                   PERFORM   SHOSAISIN-HANTEI-SEC
                               END-IF
                               PERFORM   RECE02-TENSOU-SEC
                           END-IF
                       END-IF
                   END-PERFORM
      *********    END-IF                 
                   MOVE     "SRYACCT-KEY2"   TO   ORC-DBPATH
                   PERFORM   SRYACCT-READ-SEC
               END-PERFORM
           ELSE
               DISPLAY   "SRYACCT SELECT ERR" 
      *         ACCEPT     WRK-IN FROM CONSOLE
           END-IF
      *
           MOVE   "SRYACCT-KEY2"      TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
           .
       RECE02-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計テーブルの内容から診療行為テーブルを読み込み、
      *    診療実日数のカウントを行える剤であるかの判定
      *****************************************************************
       SHOSAISIN-HANTEI-SEC                 SECTION.
      *
           MOVE    ZERO                 TO      FLG-SHOSAISIN
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPID          TO      SRY-HOSPID
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
           MOVE    ACCT-SRYKA           TO      SRY-SRYKA
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "SRYACT-KEY2"        TO      ORC-DBPATH
           CALL    "ORCMCPSUB"           USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "SRYACT-KEY2"    TO      ORC-DBPATH
               PERFORM  SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
                   PERFORM   VARYING   SRYACT-IDX  FROM  1   BY   1
                             UNTIL    (SRYACT-IDX    >   5)  OR
                                      (SRY-SRYCD(SRYACT-IDX)
                                                          =   SPACE)  OR
                                      (FLG-SHOSAISIN      =    1)
                       PERFORM   TENSU-TBL-SEC
                       IF ( (TNS-JITUDAY    =     2)    AND
                            (TNS-DAYCNT     =     1   OR   2) )  OR
                          (  SRY-SRYCD(SRYACT-IDX)  =  "112700850"    OR
                                                       "112003350"    OR
                                                       "099409901"    OR
                                                       "099999902"    OR
                                                       "099110001"    OR
                                                       "099120001")
                             MOVE    1      TO    FLG-SHOSAISIN
                       END-IF
                   END-PERFORM
                   MOVE     "SRYACT-KEY2"    TO   ORC-DBPATH
                   PERFORM   SRYACT-READ-SEC
               END-PERFORM
           ELSE
               DISPLAY   "SRYACT SELECT ERR" 
      *         ACCEPT     WRK-IN FROM CONSOLE
           END-IF
      *
           MOVE   "SRYACT-KEY2"      TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
           .
      *
      *
       SHOSAISHIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数テーブルの検索
      *****************************************************************
       TENSU-TBL-SEC                    SECTION.
      *
           INITIALIZE                           TNS-TENSU-REC
           MOVE    SRY-SRYCD(SRYACT-IDX) TO     TNS-SRYCD
           MOVE    TNS-TENSU-REC        TO      MCPDATA-REC
           MOVE    ACCT-SRYYM           TO      ORC-DBYMD(1:6)
           MOVE    "01"                 TO      ORC-DBYMD(7:2)
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "TENSU-KEY"          TO      ORC-DBPATH
           CALL    "ORCMCPSUB"           USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "TENSU-KEY"      TO      ORC-DBPATH
               PERFORM  TENSU-READ-SEC
               IF       FLG-TENSU       =       1
                   DISPLAY   "TENSU FETCH ERR" 
      *             ACCEPT     WRK-IN FROM CONSOLE
               END-IF
           ELSE
               DISPLAY   "TENSU SELECT ERR" 
      *         ACCEPT     WRK-IN FROM CONSOLE
           END-IF
      *
           MOVE   "TENSU-KEY"      TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
           .
       TENSU-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送（Ｆ００２）
      *****************************************************************
       RECE02-TENSOU-SEC                 SECTION.
      *
           IF    (RECE02-HKNNUM          NOT =   SPACE)   OR
                 (RECE02-RJNHKNNUM       NOT =   SPACE)
               IF   FLG-SHOSAISIN        =       1
                    PERFORM    VARYING   ACCT-IDX   FROM   1    BY   1
                               UNTIL    ACCT-IDX   >      31
                           IF  ACCT-DAY(1 ACCT-IDX)   NOT =    ZERO
                               MOVE    "1"       TO
                                             TBL-JNISSU-DAY(1 ACCT-IDX)
                           END-IF
                    END-PERFORM
               END-IF
               IF        ACCT-SRYKBN        =    "95"   OR  "96"
                   CONTINUE
               ELSE
                 IF        ACCT-SRYCDTOTAL    =    630010002
                   COMPUTE  RECE02-TOTALTEN(1) =  RECE02-TOTALTEN(1) -
                                                 (ACCT-ZAITEN  *  
                                                  ACCT-ZAIKAISU)
                 ELSE
                   COMPUTE  RECE02-TOTALTEN(1) =  RECE02-TOTALTEN(1) +
                                                 (ACCT-ZAITEN  *  
                                                  ACCT-ZAIKAISU)
                 END-IF
               END-IF
           END-IF
      *
      *****EVALUATE  TRUE
      *      WHEN  (ACCT-HKNCOMBI     =    RECE02-HKNCOMBI(1))
      *          MOVE     1         TO   HC-IDX
      *      WHEN  (ACCT-HKNCOMBI     =    RECE02-HKNCOMBI(2))
      *          MOVE     2         TO   HC-IDX
      *      WHEN  (ACCT-HKNCOMBI     =    RECE02-HKNCOMBI(3))
      *          MOVE     3         TO   HC-IDX
      *      WHEN  OTHER
      *          MOVE     ZERO      TO   HC-IDX
      *****END-EVALUATE
           MOVE    ZERO             TO   HC-IDX
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       10
                   OR      RECE02-HKNCOMBI (IDX1)   =   ZERO 
               IF      ACCT-HKNCOMBI   =
                                        RECE02-HKNCOMBI (IDX1)
                   MOVE    IDX1            TO  HC-IDX
                   MOVE    10              TO  IDX1
               END-IF
           END-PERFORM          
      *
           MOVE       ZERO              TO   KH-IDX
           PERFORM    VARYING   IDX2    FROM   1   BY   1
             UNTIL   (IDX2      >       4)
               MOVE   ZERO               TO        KH-IDX
               IF     RECE02-KOHNUMHC(HC-IDX IDX2)  NOT =  SPACE
                 IF   RECE02-KOHNUMHC(HC-IDX IDX2) =  RECE02-KOHNUM(1)
                      MOVE    2          TO        KH-IDX
                 END-IF
                 IF   RECE02-KOHNUMHC(HC-IDX IDX2) =  RECE02-KOHNUM(2)
                      MOVE    3          TO        KH-IDX
                 END-IF
                 IF   RECE02-KOHNUMHC(HC-IDX IDX2) =  RECE02-KOHNUM(3)
                      MOVE    4          TO        KH-IDX
                 END-IF
                 IF   RECE02-KOHNUMHC(HC-IDX IDX2) =  RECE02-KOHNUM(4)
                      MOVE    5          TO        KH-IDX
                 END-IF
      *
      *           DISPLAY   "KH-IDX   "  KH-IDX
      *
                 IF   KH-IDX             NOT =    ZERO
                  IF   FLG-SHOSAISIN      =    1
                      PERFORM    VARYING  ACCT-IDX   FROM   1   BY   1
                                 UNTIL    ACCT-IDX   >      31
                             IF  ACCT-DAY(1 ACCT-IDX)   NOT =    ZERO
                                 MOVE    "1"       TO
                                      TBL-JNISSU-DAY(KH-IDX ACCT-IDX)
                             END-IF
                      END-PERFORM
                  END-IF
                  IF        ACCT-SRYKBN        =    "95"   OR  "96"
                     CONTINUE
                  ELSE
                    IF        ACCT-SRYCDTOTAL    =    630010002
                      COMPUTE  RECE02-TOTALTEN(KH-IDX) =
                                              RECE02-TOTALTEN(KH-IDX) -
                                             (ACCT-ZAITEN * 
                                              ACCT-ZAIKAISU)
                    ELSE
                      COMPUTE  RECE02-TOTALTEN(KH-IDX) =
                                              RECE02-TOTALTEN(KH-IDX) +
                                             (ACCT-ZAITEN * 
                                              ACCT-ZAIKAISU)
                    END-IF
                  END-IF
                 END-IF
               END-IF  
           END-PERFORM
           .
       RECE02-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    実日数の項目転送（Ｆ００２）
      *****************************************************************
       JNISSU-SET-SEC                    SECTION.
      *
           PERFORM    VARYING    HKN-IDX    FROM   1   BY   1
                      UNTIL      HKN-IDX    >      5
               PERFORM    VARYING    DAY-IDX    FROM    1   BY   1
                          UNTIL      DAY-IDX    >     31
                    IF    TBL-JNISSU-DAY(HKN-IDX DAY-IDX)
                                        NOT =  SPACE
                          ADD    1      TO    RECE02-JNISSU(HKN-IDX)
                    END-IF
               END-PERFORM
           END-PERFORM
           .
       JNISSU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ更新処理（Ｆ００２）
      *****************************************************************
       RECE02-REWRITE-SEC                SECTION.
      *
           REWRITE   RECE02-REC
      *
           ADD     1                   TO  CNT-MOD (IDZ)
      *
           .
       RECE02-WRITE-EXT.
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
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
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
      ***     DISPLAY "RECE02  IN  :" CNT-IN   (1) 
      ***     DISPLAY "        MOD :" CNT-MOD  (1) 
      ***     DISPLAY "RECE02X IN  :" CNT-IN   (2) 
      ***     DISPLAY "        MOD :" CNT-MOD  (2) 
      ***     DISPLAY "*** ORCR0025 END ***"
      ***     ACCEPT  WRK-IN FROM CONSOLE 
           .
       300-TERM-EXT.
           EXIT.
      *****************************************************************
      *    診療会計読込
      *****************************************************************
       SRYACCT-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       SRYACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療行為読込
      *****************************************************************
       SRYACT-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       SRYACT-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数読込
      *****************************************************************
       TENSU-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               MOVE    ZERO            TO  FLG-TENSU
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           .
       TENSU-READ-EXT.
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
      *
      *****************************************************************
      *    カーソルクローズ処理
      *****************************************************************
       CSR-CLOSE-SEC            SECTION.
      *
           INITIALIZE                  MCPAREA
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    WK-PATH             TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           .
       CSR-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理
      *****************************************************************
       RECE02-READ-SEC            SECTION.
      *
           READ    RECE02-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-RECE02
               NOT  AT  END
                   ADD     1     TO    CNT-IN (IDZ)
           END-READ
           .
       RECE02-READ-EXT.
           EXIT.
      *
