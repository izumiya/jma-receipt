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
       PROGRAM-ID.             ORCR0091.
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
      *  01.00.01    MCC-藤原     01.06.29  保険者番号・
      *                                     適用開始年月日の追加
      *                                     KEYが長すぎるので医療機関ＩＤ・
      *                                     診療年月を削除
      *  01.00.02    MCC-小豆沢   02.01.09  診療区分＝９９(コメント等)の時
      *                                     適用欄の先頭に印字するよう変更
      *  01.00.03    MCC-小豆沢   02.02.20  診療区分＝２１(内服)の時
      *                                     入力順で印字するように修正
      *  01.00.04    MCC-小豆沢   02.02.22  診療区分＝９５と９６(自費)の時
      *                                     適用欄の印字しない
      *  01.00.05    MCC-小豆沢   02.03.19  診療区分＝１２を摘要の印字対象
      *                                     に修正（同日再診対応）
      *  01.00.06    MCC-小豆沢   02.04.20  診療区分＝８０の摘要欄印字順を
      *                                     変更
      *  01.00.07    NACL-藤原    02.09.04  検査のときは表用番号を編集しない
      *                                     検査のときZAITENを編集する
      *  01.00.08    NACL-藤原    02.09.06  初診のとき編集する
      *  01.00.09    NACL-小豆沢  02.09.28  検査の摘要欄印刷順を修正
      *  01.00.10    NACL-藤原    02.11.07  医療機関ＩＤ・診療年月を追加
      *  01.00.11    NACL-門脇    02.11.26  検診コードの診療区分編集
      *  01.00.12    NACL-藤原    03.03.12  補助区分の追加
      *  01.00.13    NACL-門脇    03.03.03  剤番号桁修正対応
      *  01.00.14    NACL-門脇    03.03.06  初再診のコメント編集対応
      *  01.00.15    NACL-門脇    03.11.26  自費レセプトの自費分
      *                                     摘要欄記載対応
      *  01.00.16    NACL-門脇    05.01.06  自費レセプトの出力条件修正
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE06-FILE     ASSIGN  RECE06PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE06-KEY
                                   FILE    STATUS  IS  STS-RECE06.
           SELECT  RECE061-FILE    ASSIGN  RECE061PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE061-KEY
                                   FILE    STATUS  IS  STS-RECE061.
           SELECT  RECE0661-FILE   ASSIGN  RECE061PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE0661-KEY
                                   FILE    STATUS  IS  STS-RECE061.
006900*    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE06-FILE.
       01  RECE06-REC. 
           COPY    "CPRCF006.INC".
      *
       FD  RECE061-FILE.
       01  RECE061-REC. 
           COPY    "CPRCF0061.INC".
      *
       FD  RECE0661-FILE.
       01  RECE0661-REC. 
           COPY    "CPRCF0061.INC"  REPLACING  //RECE061//
                                   BY          //RECE0661//.
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE06//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE061//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE06                              PIC X(02).
           03  STS-RECE061                             PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  SAISHIN-FLG                             PIC X(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
           03  CNT-OUT                                 PIC 9(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                      PIC  X(06).
               05  WRK-PARA-TERMID                     PIC  X(16).
               05  WRK-PARA-SYSYMD                     PIC  X(08).
               05  WRK-PARA-JOBID                      PIC  9(07).
               05  WRK-PARA-SHELLID                    PIC  X(08).
      *
           03  WRK-RECEERR             PIC X(200). 
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *     
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
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
           MOVE    SPACE               TO  SAISHIN-FLG
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
           MOVE    "ORCR0091"      TO  JOB-PGID
           MOVE    "摘要欄編集"    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *
           MOVE    "RECE060"           TO  RECE06PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE06PARA-TERMID
      *
           MOVE    "RECE061"           TO  RECE061PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE061PARA-TERMID
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECEERR-TERMID
      *
      *
           OPEN    OUTPUT              RECE0661-FILE
           CLOSE                       RECE0661-FILE
      *
           OPEN    INPUT               RECE06-FILE
           OPEN    I-O                 RECE061-FILE
      *
           PERFORM    RECE06-READ-SEC
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
           PERFORM     210-HENSYU-SEC
      *
           PERFORM     RECE06-READ-SEC
           .
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    編集処理
      *****************************************************************
       210-HENSYU-SEC                   SECTION.
      *
           INITIALIZE                           RECE061-REC
           MOVE    RECE06-HOSPID        TO      RECE061-HOSPID
           MOVE    RECE06-SRYYM         TO      RECE061-SRYYM
           MOVE    RECE06-NYUGAIKBN     TO      RECE061-NYUGAIKBN
           MOVE    RECE06-PTID          TO      RECE061-PTID
           MOVE    RECE06-RECEKA        TO      RECE061-RECEKA
           MOVE    RECE06-TEISYUTUSAKI  TO      RECE061-TEISYUTUSAKI
           MOVE    RECE06-RECESYUBETU   TO      RECE061-RECESYUBETU
           MOVE    RECE06-HKNJANUM      TO      RECE061-HKNJANUM
           MOVE    RECE06-HOJOKBN       TO      RECE061-HOJOKBN
           MOVE    RECE06-TEKSTYMD      TO      RECE061-TEKSTYMD
           MOVE    RECE06-KOHBUNRI      TO      RECE061-KOHBUNRI
           MOVE    RECE06-ZAINUM        TO      RECE061-ZAINUM
           IF      RECE06-ZAINUM(1:7)  =  "9999998"
           AND     RECE06-ZAINUM   NOT =   99999980
                 MOVE     ZERO          TO      RECE061-ZAINUM
           END-IF
           IF      RECE06-ZAINUM       =   99999999
                 MOVE     ZERO          TO      RECE061-ZAINUM
           END-IF
           IF      RECE06-SRYKBN        =       "60"
              IF   RECE06-KNSJISKBN     =       2
                   MOVE    5            TO      RECE061-KNSJISKBN
              ELSE
                  IF   (RECE06-CDKBN-KBNNUM >=        0)   AND
                       (RECE06-CDKBN-KBNNUM <=      199)
                          MOVE   1      TO      RECE061-KNSJISKBN
                  END-IF
                  IF   (RECE06-CDKBN-KBNNUM >=      400)   AND
                       (RECE06-CDKBN-KBNNUM <=      499)
                          MOVE   2      TO      RECE061-KNSJISKBN
                  END-IF
                  IF   (RECE06-CDKBN-KBNNUM >=      200)   AND
                       (RECE06-CDKBN-KBNNUM <=      399)
                          MOVE   3      TO      RECE061-KNSJISKBN
                  END-IF
                  IF   RECE06-CDKBN-KBN      =      "9"
                          MOVE   4      TO      RECE061-KNSJISKBN
                  END-IF
              END-IF
           END-IF     
      ***     MOVE    RECE06-KNSJISKBN     TO      RECE061-KNSJISKBN
           MOVE    RECE06-LINENUM       TO      RECE061-LINENUM
           MOVE    RECE06-ZAIMEISAISU   TO      RECE061-ZAIMEISAISU
           MOVE    RECE06-PTNUM         TO      RECE061-PTNUM
           MOVE    RECE06-SRYKA         TO      RECE061-SRYKA
           IF      RECE06-SRYKBN        =       "99"                    
               MOVE  SPACE              TO      RECE061-SRYKBN
           ELSE
               MOVE  RECE06-SRYKBN      TO      RECE061-SRYKBN
           END-IF
           MOVE    RECE06-ASTER         TO      RECE061-ASTER
           MOVE    RECE06-TEKIYO        TO      RECE061-TEKIYO
           IF      RECE06-SRYKBN        =       "12"
            IF      RECE06-ASTER         =       "＊"
              IF   RECE06-TEKIYO(1:24)  =
                   "再診（診療所）（１回目）"
                   MOVE   "1"           TO      SAISHIN-FLG
              END-IF
            END-IF
            IF   RECE06-TEKIYO(49:2)  =   SPACE
                 CONTINUE
            ELSE     
              IF  SAISHIN-FLG          =      "1"
                 MOVE      SPACE        TO      SAISHIN-FLG
              END-IF
            END-IF 
           END-IF 
           IF      RECE06-SRYKBN        NOT =   "21"
               IF   RECE06-SRYKBN       NOT =   "40" AND "60" AND "80"  
      ****     IF   RECE06-SRYKBN       NOT =   "40" AND "80"  
                   MOVE    RECE06-ZAITEN    TO      RECE061-ZAITEN
               END-IF
               MOVE   RECE06-CDKBN-SYO     TO  RECE061-CDKBN-SYO
               MOVE   RECE06-CDKBN-BU      TO  RECE061-CDKBN-BU
               MOVE   RECE06-CDKBN-KBN     TO  RECE061-CDKBN-KBN
               MOVE   RECE06-CDKBN-KBNNUM  TO  RECE061-CDKBN-KBNNUM
               MOVE   RECE06-CDKBN-KBNNUM-EDA
                                           TO  RECE061-CDKBN-KBNNUM-EDA
               MOVE   RECE06-CDKBN-KOUBAN  TO  RECE061-CDKBN-KOUBAN
               MOVE   RECE06-KOUHYOJUNNUM  TO  RECE061-KOUHYOJUNNUM
           END-IF
      **         IF      RECE06-SRYKBN        =   "60"
      **            CONTINUE
      **         ELSE    
      **             MOVE    RECE06-CDKBN-SYO     TO  RECE061-CDKBN-SYO
      **             MOVE    RECE06-CDKBN-BU      TO  RECE061-CDKBN-BU
      **             MOVE    RECE06-CDKBN-KBN     TO  RECE061-CDKBN-KBN
      **             MOVE    RECE06-CDKBN-KBNNUM  TO  RECE061-CDKBN-KBNNUM
      **             MOVE    RECE06-CDKBN-KBNNUM-EDA
      **                                      TO  RECE061-CDKBN-KBNNUM-EDA
      **             MOVE    RECE06-CDKBN-KOUBAN  TO  RECE061-CDKBN-KOUBAN
      **             MOVE    RECE06-KOUHYOJUNNUM  TO  RECE061-KOUHYOJUNNUM
      **         END-IF    
      **     END-IF
      *    
      *****IF     RECE06-SRYKBN  NOT =  "11" AND "95" AND "96"
           IF     RECE06-RECESYUBETU  =  8888
              IF     RECE06-ZAITEN  NOT =  ZERO
                  PERFORM     RECE061-WRITE-SEC
              ELSE
                IF   RECE06-SRYKBN  =  "11" OR "12" OR "99"
                     PERFORM  RECE061-WRITE-SEC
                END-IF
              END-IF
           ELSE
              IF     RECE06-SRYKBN  NOT =  "95" AND "96"
                  PERFORM     RECE061-WRITE-SEC
              END-IF
           END-IF
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ更新処理（Ｆ００６１）
      *****************************************************************
       RECE061-WRITE-SEC                SECTION.
      *
           WRITE   RECE061-REC
      *
           ADD     1                   TO  CNT-OUT
      *
           .
       RECE061-WRITE-EXT.
           EXIT.
      *
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
           CLOSE   RECE06-FILE
                   RECE061-FILE
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
           DISPLAY "RECE06  CNT-IN  :" CNT-IN
           DISPLAY "RECE061 CNT-OUT :" CNT-OUT
           DISPLAY "*** ORCR0091 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理
      *****************************************************************
       RECE06-READ-SEC            SECTION.
      *
           READ    RECE06-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-END
               NOT  AT  END
                   ADD     1     TO    CNT-IN
      *
                   DISPLAY "RECE06  PTID=" RECE06-PTID
           END-READ
           .
       RECE06-READ-EXT.
           EXIT.
      *
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
      *
           .
       900-DBCLOSE-EXT.
           EXIT.
