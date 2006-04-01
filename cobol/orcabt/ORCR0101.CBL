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
      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCR0101.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : レセプト　印刷
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-太田      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01.06.29  ファイル名を連番に変更
      *  01.00.02    MCC-藤原     01.07.13  県内・県外別に印刷のため
      *                                     区分追加 
      *  01.00.03    MCC-藤原     01.10.03  出力先プリンタ名編集
      *  01.00.04    MCC-藤原     01.10.24  出力先プリンタ名が取得
      *                                     できないときエラーにしない
      *  01.01.01    MCC-藤原     02.01.23  帳票出力用パラメタファイル、
      *                                     印刷用データ(HC99)を未使用とする
      *  01.01.02    MCC-藤原     02.02.25  不明分の出力の変更
      *  01.01.03    MCC-藤原     02/02/26  レセプト管理ファイルを
      *                                     索引ファイルからＤＢへ変更
      *  01.01.04    MCC-藤原     02.03.18  端末ＩＤ
      *                                     X(16)からX(32)に変更
      *  01.01.05    MCC-藤原     02/04/30  印刷順の追加
      *  01.01.06    MCC-藤原     02.05.16  個別の時複数診療月で作成するために
      *                                     診療年月を追加
      *
      *  ２００２年１０月改正対応
      *  01.02.01    NACL-藤原    02/09/27  パラメタに入外区分の追加
      *  01.02.02    NACL-藤原    03/08/28  出力内容指定の処理の追加
      *  01.02.03    NACL-藤原    04/06/20  印刷順の追加（個別作成時のみ）
      *
      *  中止処理追加
      *  01.03.01    NACL-藤原    04/12/01  印刷中止処理の追加
      *
      *  02.06.01    NACL-竹田    05/05/30  realtime preview 処理追加
      *  02.06.02    NACL-藤原    05/07/13  国保の県内県外の印刷順の指定
      *                                     「４」の追加
      *
      *  02.07.01    NACL-藤原    05/12/12  国保レセプト編綴順対応
      *
      *  02.08.01    NACL-藤原    06/02/20  主科対応
      *  02.08.02    NACL-藤原    06/04/01  対象データ表示
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).    
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN              PIC 9(06).
           03  CNT-IN-CANCEL       PIC 9(06).
           03  CNT-OUT             PIC 9(03).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(03).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-MOJISU1         PIC 9(02).
           03  WRK-MOJISU2         PIC 9(02).
      *
           03  WRK-PARA.
               05  WRK-PARA1.  
                   07  WRK-PARA-SRYYM            PIC X(06).
                   07  WRK-PARA-TERMID           PIC X(16).
                   07  WRK-PARA-SYSYMD           PIC X(08).
               05  WRK-PARA-RECE09-REC. 
                   07  WRK-PARA-RECE09-KEY.
                       09  WRK-PARA-RECE09-PRTID     PIC X(04).
                       09  WRK-PARA-RECE09-SRYYM     PIC X(06).
                       09  WRK-PARA-RECE09-CREYMD    PIC X(08).
                       09  WRK-PARA-RECE09-CREHMS    PIC X(06).
                       09  WRK-PARA-RECE09-PREFKBN   PIC X(01).
                       09  WRK-PARA-RECE09-TEISYUTUSAKI
                                                     PIC X(01).
                       09  WRK-PARA-RECE09-RECESYUBETU
                                                     PIC X(04).
                       09  WRK-PARA-RECE09-NYUGAIKBN PIC X(01).
                   07  WRK-PARA-RECE09-PRTJYUN       PIC X(02).
                   07  WRK-PARA-RECE09-CHOUKI        PIC X(01).
                   07  WRK-PARA-RECE09-ZAITAKU       PIC X(01).
                   07  WRK-PARA-RECE09-HKNJANUM      PIC X(08).
               05  WRK-PARA-RECE09-STDATA            PIC X(100).
               05  WRK-PARA-RECE09-EDDATA            PIC X(100).
               05  WRK-PARA-PRTJYUN           PIC 9(02).
               05  WRK-PARA-SHELLID           PIC X(08).
               05  WRK-PARA-JOBID             PIC 9(07).
               05  WRK-PARA-PRTKBN            PIC X(01).
               05  WRK-PARA-PREFKBN           PIC X(01).
               05  WRK-PARA-SRYKA             PIC X(02).
      *
           03  WRK-RECE08-PRTRENNUM    PIC 9(06).
      *
           03  WRK-RECEERR             PIC X(200). 
      *
           03  WRK-PATH. 
               05  FILLER          PIC S9(9)   BINARY.
               05  FILLER          PIC S9(9)   BINARY.
               05  FILLER          PIC S9(9)   BINARY.
      *
           COPY    "CPSHELLTBL.INC".
      *     
           COPY    "ORCA-DBPATH".         
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
           COPY    "CPRCF008.INC".
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
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END    =   1
      *
           PERFORM 300-END-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               STOP    RUN
           ELSE
               EXIT    PROGRAM
           END-IF 
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
           ACCEPT  SYS-TIME            FROM    TIME     
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO  WRK-PARA-PRTKBN
                                             WRK-PARA-PREFKBN
                                             WRK-PARA-SRYKA
                                             WRK-PARA-JOBID
                                             WRK-PARA-SHELLID
                                             WRK-PARA1
                                             WRK-PARA-RECE09-REC
                                             WRK-PARA-RECE09-STDATA
                                             WRK-PARA-RECE09-EDDATA
                                             WRK-PARA-PRTJYUN
           END-UNSTRING                                  
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECEERR-TERMID
      *
           PERFORM 100-DBOPEN-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    "ORCR0101"      TO  JOB-PGID
               MOVE    "対象レセプト抽出"
                                       TO  JOB-SHELLMSG
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷開始処理
      *****************************************************************
       130-SYOKI-PRINT-SEC             SECTION.
      *
           INITIALIZE                      RECE08-REC
           MOVE    WRK-PARA-RECE09-PRTID        TO  RECE08-PRTID
           MOVE    WRK-PARA-RECE09-SRYYM        TO  RECE08-RECEYM
           MOVE    WRK-PARA-RECE09-CREYMD       TO  RECE08-CREYMD
           MOVE    WRK-PARA-RECE09-CREHMS       TO  RECE08-CREHMS
           MOVE    WRK-PARA-RECE09-PREFKBN      TO  RECE08-PREFKBN
           MOVE    WRK-PARA-RECE09-TEISYUTUSAKI TO  RECE08-TEISYUTUSAKI
           MOVE    WRK-PARA-RECE09-RECESYUBETU  TO  RECE08-RECESYUBETU
           MOVE    WRK-PARA-RECE09-NYUGAIKBN    TO  RECE08-NYUGAIKBN
           MOVE    WRK-PARA-RECE09-HKNJANUM     TO  RECE08-HKNJANUM-S
           MOVE    WRK-PARA-SRYKA               TO  RECE08-SRYKA
      *
           MOVE    WRK-PARA-RECE09-STDATA       TO  RECE08-STKANANAME
           MOVE    WRK-PARA-RECE09-EDDATA       TO  RECE08-EDKANANAME
      *
      *    出力内容による頁設定
           EVALUATE    WRK-PARA-PRTKBN
               WHEN    "1"
                   MOVE    1                    TO  RECE08-STRENNUM
                   MOVE    99                   TO  RECE08-EDRENNUM
               WHEN    "2"
                   MOVE    1                    TO  RECE08-STRENNUM
                   MOVE    1                    TO  RECE08-EDRENNUM
               WHEN    "3"
                   MOVE    2                    TO  RECE08-STRENNUM
                   MOVE    99                   TO  RECE08-EDRENNUM
           END-EVALUATE
           PERFORM 900-RECEPRT-SELECT-SEC
      *
           .
       130-SYOKI-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    ZERO                TO  WRK-RECE08-PRTRENNUM     
      *
      *    帳票印刷開始処理
           PERFORM 130-SYOKI-PRINT-SEC
      *
           PERFORM              UNTIL   FLG-END =   1        
               ADD     1                    TO  WRK-RECE08-PRTRENNUM
      *
               MOVE    WRK-PARA-PRTJYUN     TO  RECE08-PRTJYUN
               MOVE    WRK-RECE08-PRTRENNUM TO  RECE08-PRTRENNUM      
      *
               MOVE    RECE08-REC           TO  MCPDATA-REC
               MOVE    "RECEPRT-KEY"        TO  ORC-DBPATH
               MOVE    "DBUPDATE"           TO  MCP-FUNC
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
      *
      *        エラーチェック
               IF      MCP-RC             =   ZERO
                   ADD     1                  TO  CNT-OUT
                   PERFORM 900-RECEPRT-FET-SEC
      *
      *            処理中止設定処理
                   IF      CNT-IN-CANCEL       =   50
                       MOVE    ZERO                TO  CNT-IN-CANCEL
                       PERFORM 500-CANCEL-HENSYU-SEC
                   END-IF
               ELSE
                   MOVE    "帳票ＤＢの更新でエラーになりました"
                                              TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-PERFORM    
      *
           PERFORM 900-RECEPRT-CLS-SEC 
      *
           .
       200-MAIN-EXT.
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
               IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *            ジョブ管理終了処理
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
           END-IF
      *
           MOVE    1                   TO  FLG-END     
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理中止処理
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               CONTINUE
           ELSE
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
               IF    ( SJOBKANRI-RETURN
                                       =   ZERO )
               AND   ( JOB-STOPFLG     >   ZERO )
                   MOVE    "処理が中止されました"
                                           TO  WRK-RECEERR
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
                       IF      JOB-STOPFLG     =   1
      *                    ジョブ管理終了処理
                           MOVE    "CA2"           TO  SJOBKANRI-MODE
                           INITIALIZE                  JOBKANRI-REC
                           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
                           MOVE    WRK-PARA-SHELLID
                                                   TO  JOB-SHELLID
                           MOVE    WRK-RECEERR     TO  JOB-YOBI
                           MOVE    "8888"          TO  JOB-ERRCD
                           MOVE    2               TO  JOB-STOPFLG
                           CALL    "ORCSJOB"       USING
                                                   ORCSJOBKANRIAREA  
                                                   JOBKANRI-REC
                       END-IF
                   END-IF
      *                             
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF 
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
           END-IF
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "*** ORCR0101 IN   "  CNT-IN
           DISPLAY "*** ORCR0101 OUT  "  CNT-OUT 
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *     レセプト管理を主キーで読み込む
      *****************************************************************
       900-RECEPRT-SELECT-SEC          SECTION.
      *
      *    編綴順印刷
           IF      WRK-PARA-RECE09-PRTJYUN      =   "06"
               PERFORM 9001-RECEPRT-KEY-HENSYU-SEC
           ELSE
      *     入力順印刷（個別作成時）
            IF      WRK-PARA-RECE09-PRTJYUN      =   "05"
                MOVE    PATH-TBL-RECEPRT-KEY30       TO  WRK-PATH
            ELSE
               IF      WRK-PARA-PREFKBN           =   "4"
                   IF      WRK-PARA-RECE09-RECESYUBETU  =   "9999"  
                       IF      WRK-PARA-RECE09-PRTJYUN      =   "03"
                           MOVE    PATH-TBL-RECEPRT-KEY35   TO  WRK-PATH
                       ELSE
                           MOVE    PATH-TBL-RECEPRT-KEY36   TO  WRK-PATH
                       END-IF
                   ELSE 
                       IF      WRK-PARA-RECE09-RECESYUBETU  =   ZERO
                                                            OR  "8888"
                                                            OR  "6666"
                           IF      WRK-PARA-RECE09-PRTJYUN  =   "01"  OR
                                                                "03" 
                               MOVE    PATH-TBL-RECEPRT-KEY33
                                                           TO  WRK-PATH
                           ELSE
                               MOVE    PATH-TBL-RECEPRT-KEY34
                                                           TO  WRK-PATH
                           END-IF
                       ELSE 
                           IF      WRK-PARA-RECE09-PRTJYUN =   "01"
                               MOVE    PATH-TBL-RECEPRT-KEY31
                                                           TO  WRK-PATH
                           ELSE
                               MOVE    PATH-TBL-RECEPRT-KEY32
                                                           TO  WRK-PATH
                           END-IF
                       END-IF
                   END-IF
               ELSE
                   IF      WRK-PARA-RECE09-RECESYUBETU  =   "9999"  
                       IF      WRK-PARA-RECE09-PRTJYUN      =   "03"
                           MOVE    PATH-TBL-RECEPRT-KEY16   TO  WRK-PATH
                       ELSE
                           MOVE    PATH-TBL-RECEPRT-KEY17   TO  WRK-PATH
                       END-IF
                   ELSE 
                       IF      WRK-PARA-RECE09-RECESYUBETU  =   ZERO
                                                            OR  "8888"
                                                            OR  "6666"
                           IF      WRK-PARA-RECE09-PRTJYUN  =   "01"  OR
                                                                "03" 
                               MOVE    PATH-TBL-RECEPRT-KEY14
                                                            TO  WRK-PATH
                           ELSE
                               MOVE    PATH-TBL-RECEPRT-KEY15
                                                            TO  WRK-PATH
                           END-IF
                       ELSE 
                           IF      WRK-PARA-RECE09-PRTJYUN =   "01"
                               MOVE    PATH-TBL-RECEPRT-KEY10
                                                           TO  WRK-PATH
                           ELSE
                               MOVE    PATH-TBL-RECEPRT-KEY11
                                                           TO  WRK-PATH
                           END-IF
                       END-IF
                   END-IF
               END-IF
             END-IF
           END-IF            
      *
           MOVE    RECE08-REC          TO  MCPDATA-REC
      *
           MOVE    WRK-PATH            TO  MCP-PATH
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
      *
      *
      *    エラーチェック
           IF      MCP-RC             =   ZERO
               PERFORM 900-RECEPRT-FET-SEC
           ELSE 
               MOVE    1                   TO  FLG-END
           END-IF
           .
       900-RECEPRT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     入力コードマスタを主キーより検索処理
      *****************************************************************
       9001-RECEPRT-KEY-HENSYU-SEC    SECTION.
      *
           MOVE    WRK-PARA-RECE09-CHOUKI  TO  RECE08-CHOUKI
           MOVE    WRK-PARA-RECE09-ZAITAKU TO  RECE08-ZAITAKU
      *
           MOVE    PATH-TBL-RECEPRT-KEY37  TO  WRK-PATH
      *
           .
       9001-RECEPRT-KEY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *     入力コードマスタを主キーより検索処理
      *****************************************************************
       900-RECEPRT-FET-SEC            SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    WRK-PATH            TO  MCP-PATH
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
      *                                  
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  RECE08-REC
               MOVE    ZERO            TO  FLG-END
               ADD     1               TO  CNT-IN-CANCEL
                                           CNT-IN
      *
               DISPLAY "PTNUM=" RECE08-PTNUM 
           ELSE
               MOVE    1               TO  FLG-END
           END-IF
            .
       900-RECEPRT-FET-EXT.
           EXIT.
      *
      *******************************************************
      *     入力コードマスター クロース１処理
      *****************************************************************
       900-RECEPRT-CLS-SEC            SECTION.
      *
           MOVE    WRK-PATH            TO  MCP-PATH
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
           .
       900-RECEPRT-CLS-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           MOVE    "DBSTART"           TO  MCP-FUNC
           CALL    "MCPSUB"            USING
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
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
