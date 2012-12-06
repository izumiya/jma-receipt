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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGT99.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 総括・請求書
      *  コンポーネント名  : 処理結果
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    03.07.05  タイマー機能の追加
      *  01.00.02    NACL-伊藤    04.02.17  レセ電ファイルの転送エラー
      *                                     メッセージの追加
      * 公費個別発行機能対応
      *  01.06.01    NACL-藤原    04/10/25  公費個別発行機能の追加
      *
      *  02.07.01    NACL-藤原    05/10/31  MONFUNC対応
      *  02.07.02    NACL-藤原    05/12/02  レセ電データ複数枚対応
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  03.05.02    NACL-藤原    07/08/01  クライアント保存対応
      *
      *  04.06.01    NACL-藤原    12/12/04  公費ＣＳＶ、出産育児一時金の
      *                                     クライアント保存（ＣＤ−Ｒ）対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "T01SCR-SPA".
           COPY    "CPORCSPRTLNK.INC"
                                   REPLACING  //LNK-//
                                   BY         //SPA-LNK-//.
      *
           COPY    "T06SCR-SPA".
           COPY    "CPORCSPRTLNK.INC"
                                   REPLACING  //LNK-//
                                   BY         //SPA-T06-LNK-//.
      *
      *    レセプト作成指示共通パラメタ
           COPY    "T01COMMON-SPA".
      *
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-BTPARA          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
       01  WRK-AREA.
           03  WRK-PAGE            PIC ZZZZZZZZZ9.
           03  WRK-KENSU-Z         PIC ZZZZZZ9.
      *
      *    パラメタＤＢ編集用
           03  WRK-BTPARA-INFO-PARA.
               05  WRK-BTPARA-DATA PIC X(10).
               05  WRK-BTPARA-FILENM
                                   PIC X(20).
               05  WRK-BTPARA-DATAKBN
                                   PIC X(01).
               05  WRK-BTPARA-STNO PIC 9(02).
               05  WRK-BTPARA-EDNO PIC 9(02).
               05  WRK-BTPARA-WRNO PIC 9(02).  
      *     
       01  WRK-HEN-AREA.
           03  WRK-HENTIME.
               05  WRK-HH          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-MM          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-SS          PIC X(02).
      *
       01  WRK-CONS-AREA.
      *    システム管理ＤＢ（３００３）のキー値  
           03  WRK-CONS-3003-KBNCD PIC X(08)   VALUE   "044".
      *    印刷管理ＤＢのキー値  
           03  WRK-CONS-PRTKANRI-TBL-KEY
                                   PIC X(08)   VALUE   "SOKATU".
           03  WRK-CONS-PRTKANRI-TBL-GROUP
                                   PIC X(14)   VALUE   SPACE.
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "SOKATU".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   ZERO.
      *     
      *    パラメタＤＢのキー値（レセ電作成）  
           03  WRK-CONS-BTPARA-SHELLID
                                   PIC X(08)   VALUE   "SOKATU".
           03  WRK-CONS-BTPARA-SCRIPTID
                                   PIC X(10)   VALUE   "recept4.sh".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSK3003.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    クライアント保存ＤＢ制御サブ
           COPY    "CPORCSFILESV.INC".
      *
           COPY    "ORCA-BLOB".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "T01.INC".
           COPY    "T02.INC".
           COPY    "T03.INC".
           COPY    "T04.INC".
           COPY    "T05.INC".
           COPY    "T06.INC".
           COPY    "T07.INC".
           COPY    "T08.INC".
           COPY    "T09.INC".
           COPY    "T91.INC".
           COPY    "T98.INC".
           COPY    "T99.INC".
           COPY    "TERR.INC".
           COPY    "TID1.INC".
           COPY    "TID2.INC".
           COPY    "TID3.INC".
           COPY    "TFIL.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-T01KYOUTU
           IF      SPA-T99-MOTOPG  =   "T06"
               MOVE    SPA-FREE            TO  SPA-T06
               INITIALIZE                      SPA-T01FREE
               MOVE    SPA-T06-SFILESV-OCC (1)
                                           TO  SPA-SFILESV-OCC (1)
               MOVE    SPA-T06-SFILESV-IDX TO  SPA-SFILESV-IDX
               MOVE    SPA-T06-SFILESV-MAX TO  SPA-SFILESV-MAX
               MOVE    SPA-GMN-T98-DATAKBN TO  SPA-GMN-DATAKBN
               MOVE    SPA-NAI-T06-SRYYM-H TO  SPA-NAI-SKYYM 
           ELSE
               MOVE    SPA-FREE            TO  SPA-T01FREE
               INITIALIZE                      SPA-T06 
           END-IF
      * 
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN      OTHER
                   PERFORM 200-GMNSENI
           END-EVALUATE
      *
           MOVE    SPA-T01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           IF      SPA-T99-MOTOPG  =   "T06"
               MOVE    SPA-SFILESV-OCC (1)
                                       TO  SPA-T06-SFILESV-OCC (1)
               MOVE    SPA-SFILESV-IDX TO  SPA-T06-SFILESV-IDX
               MOVE    SPA-SFILESV-MAX TO  SPA-T06-SFILESV-MAX
               MOVE    SPA-T06         TO  SPA-FREE
           ELSE
               MOVE    SPA-T01FREE     TO  SPA-FREE
           END-IF
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           IF    ( SPA-MOTOPG          =   "TFIL" )
           AND   ( SPA-T99-MOTOPG  NOT =   "T04"  )
      *        ファイルエントリ処理(クライアント保存)
               IF      SPA-SFILESV-MAX     >   SPA-SFILESV-IDX
                   ADD     1                   TO  SPA-SFILESV-IDX
                   PERFORM 3001-FILEENTRY-SEC
                  GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    "B01"               TO  MCP-WIDGET
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "T99    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      * 
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    処理状態
               WHEN    "CLICKED"       ALSO    "pandatimer1"
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-KAKUNIN-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           IF      SPA-MOTOPG          =   "T05"
               MOVE    SPA-MOTOPG          TO  SPA-SAKIPG
           ELSE
               IF      SPA-T99-MOTOPG      =   "T06"
                   MOVE    "T06"               TO  SPA-SAKIPG
               ELSE
                   MOVE    "T01"               TO  SPA-SAKIPG
               END-IF
           END-IF
           MOVE    "T99"               TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *
           INITIALIZE                      T99
           MOVE    ZERO                TO  T99-COUNT
      *
           MOVE    5                   TO  T99-DURATION
      *        
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-NAI-SKYYM       TO  SYS-3003-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-3003-STYUKYMD (7:2)
           MOVE    SYS-3003-STYUKYMD   TO  SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SYS-3003-JOBID
      *            ジョブ管理チェック処理
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
                   IF      SJOBKANRI-RETURN    =   ZERO
                       MOVE    JOB-JOBID       TO  T99-RENNUM   (IDX)
                       MOVE    JOB-SHELLMSG    TO  T99-SHORINM  (IDX)
                       MOVE    JOB-STARTTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-STARTTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-STARTTIME(5:2)
                                               TO  WRK-SS
                       MOVE    WRK-HENTIME     TO  T99-STARTTIME(IDX)
                       MOVE    JOB-ENDTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-ENDTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-ENDTIME(5:2)
                                               TO  WRK-SS
                       MOVE    WRK-HENTIME     TO  T99-ENDTIME  (IDX)
                       MOVE    JOB-UPDCNT      TO  WRK-PAGE
                       MOVE    WRK-PAGE        TO  T99-PAGE     (IDX)
                       MOVE    JOB-YOBI        TO  T99-ERRMSG   (IDX)
                       IF      JOB-ENDYMD      =   SPACE
                           MOVE    "処理中です"    TO  T99-MSG
                       ELSE
                           IF      JOB-ERRCD       =   SPACE
                               CONTINUE
                           ELSE
                               MOVE    SPACE       TO  T99-MSG
                               PERFORM 510-ERRSET-SEC
                               MOVE    9           TO  SPA-T01-SYORIFLG
                               IF      T99-MSG     =   SPACE
                                 STRING  JOB-YOBI    DELIMITED BY SPACE
                                         "【"        DELIMITED BY SIZE
                                        JOB-SHELLMSG DELIMITED BY SPACE
                                        "】"         DELIMITED BY SIZE
                                                     INTO  T99-MSG
                                 END-STRING   
                               END-IF
                           END-IF
                       END-IF
                       MOVE    IDX                     TO  T99-COUNT
                   END-IF
               END-PERFORM
           END-IF
      *
           IF    ( SPA-TID1-FLG    =   "FI"    OR  "SV" )
               IF      JOB-ENDYMD  NOT =   SPACE
                   IF      JOB-ERRCD       =   "9999"
                       CONTINUE
                   ELSE
                       PERFORM 4903-FILESV-UPDATE-SEC
                   END-IF
               END-IF
            END-IF       
      *
           IF    ( SPA-TID1-FLG    =   "SV" )
               IF      JOB-ENDYMD  NOT =   SPACE
                   IF      JOB-ERRCD       =   "9999"
                       CONTINUE
                   ELSE
                       IF      SPA-T99-FILESV-CHK-FLG  =   ZERO
                           PERFORM 4902-FILE-INFO-CHK-SEC
                       ELSE
      *                    ファイルエントリ処理(クライアント保存)
                           IF      SPA-T99-MOTOPG  =   "T04"
                               CONTINUE
                           ELSE
                               MOVE    1           TO  SPA-SFILESV-IDX
                           END-IF 
                           PERFORM 3001-FILEENTRY-SEC
                           IF      FLG-END         =   1
                               GO  TO  490-KAKUNIN-EXT
                           END-IF
                       END-IF
                   END-IF
               END-IF    
           END-IF
           IF      T99-MSG              =   SPACE
               MOVE    "B01"                TO  MCP-WIDGET
               IF      T99-COUNT            >   ZERO
                   IF      SPA-T01-SYORIFLG    =   ZERO    OR  9
                       MOVE    "処理は正常に終了しました"
                                                   TO  T99-MSG
                   ELSE
                       IF      SPA-GMN-DATAKBN     =   "1"
      *                    パラメタＤＢ更新処理（レセ電のとき）
                           PERFORM 4901-BTPARA-UPDATE-SEC
                           IF      SPA-ERRCD       =   SPACE
                               IF    ( WRK-BTPARA-EDNO 
                                               NOT =   WRK-BTPARA-WRNO )
                                   PERFORM 210-BACK
                                   GO  TO  490-KAKUNIN-EXT
                               ELSE
                                   IF      SPA-RECEDEN-FLG =   "OK"
                                       PERFORM 210-BACK
                                       GO  TO  490-KAKUNIN-EXT
                                   ELSE
                                       MOVE    9   TO  SPA-T01-SYORIFLG
                                       MOVE
                                          "処理は正常に終了しました"
                                                   TO  T99-MSG
                                   END-IF
                               END-IF
                           END-IF
                       ELSE
                           IF      SPA-RECEDEN-FLG =   "OK"
                               PERFORM 210-BACK
                               GO  TO  490-KAKUNIN-EXT
                           ELSE
                               MOVE    9       TO  SPA-T01-SYORIFLG
                               MOVE    "処理は正常に終了しました"
                                               TO  T99-MSG
                           END-IF
                       END-IF
                   END-IF
               ELSE
                   MOVE    "該当する処理はありません"
                                                TO  T99-MSG
               END-IF                                 
               MOVE    WIDGET-NORMAL       TO  T99-B01-STATE
           ELSE                                        
               MOVE    "B11"               TO  MCP-WIDGET
      *        レセ電作成時にフロッピー出力の場合は戻るは押下できない
               IF    ( SPA-GMN-DATAKBN     =   "1" )
               AND   ( SPA-T01-SYORIFLG    =   1   )
                   MOVE    WIDGET-INSENSITIVE  TO  T99-B01-STATE
               ELSE
                   MOVE    WIDGET-NORMAL       TO  T99-B01-STATE
               END-IF
           END-IF                                 
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "T99    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       490-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタＤＢ更新処理
      *****************************************************************
       4901-BTPARA-UPDATE-SEC       SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    作成済枚数の更新
           INITIALIZE                      BTPARA-REC
           MOVE    WRK-CONS-BTPARA-SHELLID
                                       TO  BTPARA-SHELLID
           MOVE    WRK-CONS-BTPARA-SCRIPTID
                                       TO  BTPARA-SCRIPTID
           MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           PERFORM 900-BTPARA-READ-SEC
           IF      FLG-BTPARA          =   ZERO
               MOVE    BTPARA-INFO-PARA    TO  WRK-BTPARA-INFO-PARA
               MOVE    SPA-T01-WRNO        TO  WRK-BTPARA-WRNO
               MOVE    WRK-BTPARA-INFO-PARA
                                           TO  BTPARA-INFO-PARA
               MOVE    SMCNDATE-YMD        TO  BTPARA-UPYMD
               MOVE    SMCNDATE-HMS        TO  BTPARA-UPHMS
               MOVE    BTPARA-REC          TO  MCPDATA-REC
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBUPDATE"          TO  MCP-FUNC
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC      NOT =   ZERO
                   MOVE    "1002"              TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    "1001"              TO  SPA-ERRCD
           END-IF
      *
           .
       4901-BTPARA-UPDATE-EXT.
           EXIT. 
      *
      *****************************************************************
      *    クライアント保存情報チェック処理
      *****************************************************************
       4902-FILE-INFO-CHK-SEC          SECTION.
      *
      *    ＣＳＶ一覧から遷移したとき
           IF      SPA-T99-MOTOPG  =   "T04"
               MOVE    1                   TO  SPA-T99-FILESV-CHK-FLG
               MOVE    1                   TO  T99-DURATION
           ELSE
               INITIALIZE                  ORCSFILESVAREA
               MOVE    "R"             TO  SFILESV-MODE
               MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SFILESV-TBL-KEY
               MOVE    SPA-NAI-SKYYM   TO  SFILESV-SRYYM   (1)
               MOVE    WRK-CONS-BTPARA-SCRIPTID
                                       TO  SFILESV-SHELLID (1)
               CALL    "ORCSFILESV"    USING
                                           ORCSFILESVAREA
                                           SPA-AREA
               IF      SFILESV-RETURN  =   ZERO
                   INITIALIZE               SPA-SFILESV-AREA
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       100
                           OR      SFILESV-SHELLID (IDX)
                                       =       SPACE
                       MOVE    SFILESV-TITLE        (IDX)
                                       TO  SPA-SFILESV-TITLE     (IDX)
                       MOVE    SFILESV-CNT-ALL      (IDX)
                                       TO  SPA-SFILESV-CNT-ALL   (IDX)
                       MOVE    SFILESV-FOR-FOLDER   (IDX)
                                       TO  SPA-SFILESV-FOR-FOLDER(IDX)
                       MOVE    SFILESV-FOR-DATA     (IDX)
                                       TO  SPA-SFILESV-FOR-DATA  (IDX)
                       MOVE    SFILESV-TO-FOLDER    (IDX)
                                       TO  SPA-SFILESV-TO-FOLDER (IDX)
                       MOVE    SFILESV-TO-DATA      (IDX)
                                       TO  SPA-SFILESV-TO-DATA   (IDX)
                       MOVE    SFILESV-CODE-TYPE    (IDX)
                                       TO  SPA-SFILESV-CODE-TYPE (IDX)
                       MOVE    SFILESV-DATA-TYPE    (IDX)
                                       TO  SPA-SFILESV-DATA-TYPE (IDX)
                       ADD     1       TO  SPA-SFILESV-MAX
                   END-PERFORM
      *
                   MOVE    1           TO  SPA-T99-FILESV-CHK-FLG
      *
                   MOVE    1           TO  T99-DURATION
               END-IF
           END-IF
      *
           .
       4902-FILE-INFO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルエントリ処理(クライアント保存)
      *****************************************************************
       3001-FILEENTRY-SEC          SECTION.
      *
      *    ＣＳＶ一覧から遷移したとき
           IF      SPA-T99-MOTOPG  =   "T04"
               INITIALIZE                      ORCA-BLOB
               MOVE    "BLOBIMPORT"        TO  MCP-FUNC
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "blob"              TO  MCP-TABLE
               IF      SPA-GMN-DATAKBN     =   "6"
                   STRING  "/tmp/"             DELIMITED  BY  SIZE
                           SPA-HOSPNUM         DELIMITED  BY  SIZE
                           SPA-NAI-T04-TTO-DATA     (SPA-NAI-T04-SELNUM)
                                               DELIMITED  BY  SPACE
                            ".iso"             DELIMITED  BY  SIZE
                                               INTO  ORCA-BLOB-FILE
                   END-STRING
               ELSE
                   STRING  "/tmp/"             DELIMITED  BY  SIZE
                           SPA-HOSPNUM         DELIMITED  BY  SIZE
                           SPA-NAI-T04-TTO-DATA     (SPA-NAI-T04-SELNUM)
                                               DELIMITED  BY  SPACE
                                               INTO  ORCA-BLOB-FILE
                   END-STRING
               END-IF
               CALL   "ORCDBMAIN"          USING
                                           MCPAREA
                                           ORCA-BLOB
                                           SPA-AREA
      *
               INITIALIZE                      TFIL 
               MOVE    ORCA-BLOB-OBJECT    TO  TFIL-OBJECTDATA
               IF      SPA-GMN-DATAKBN     =   "6"
                   STRING  SPA-NAI-T04-TTO-FOLDER   (SPA-NAI-T04-SELNUM)
                                               DELIMITED  BY  SPACE
                           SPA-NAI-T04-TTO-DATA     (SPA-NAI-T04-SELNUM)
                                               DELIMITED  BY  SPACE
                            ".iso"             DELIMITED  BY  SIZE
                                               INTO  TFIL-ENTRY1
                   END-STRING
               ELSE
                   STRING  SPA-NAI-T04-TTO-FOLDER   (SPA-NAI-T04-SELNUM)
                                               DELIMITED  BY  SPACE
                           SPA-NAI-T04-TTO-DATA     (SPA-NAI-T04-SELNUM)
                                               DELIMITED  BY  SPACE
                                               INTO  TFIL-ENTRY1
                   END-STRING
               END-IF
               MOVE    SPA-GMN-T04-TTITLE   (SPA-NAI-T04-SELNUM)
                                           TO  TFIL-TITLE
               MOVE    SPA-GMN-T04-TCNT-ALL (SPA-NAI-T04-SELNUM)
                                           TO  WRK-KENSU-Z
               MOVE    WRK-KENSU-Z         TO  TFIL-KENSU
           ELSE
               INITIALIZE                      ORCA-BLOB
               MOVE    "BLOBIMPORT"        TO  MCP-FUNC
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "blob"              TO  MCP-TABLE
               STRING  SPA-SFILESV-FOR-FOLDER (SPA-SFILESV-IDX)
                                           DELIMITED  BY  SPACE
                       SPA-SFILESV-FOR-DATA   (SPA-SFILESV-IDX) 
                                           DELIMITED  BY  SPACE
                                           INTO  ORCA-BLOB-FILE
               END-STRING
               CALL   "ORCDBMAIN"          USING
                                           MCPAREA
                                           ORCA-BLOB
                                           SPA-AREA
      *
               INITIALIZE                      TFIL 
               MOVE    ORCA-BLOB-OBJECT    TO  TFIL-OBJECTDATA
               STRING  SPA-SFILESV-TO-FOLDER (SPA-SFILESV-IDX)
                                           DELIMITED  BY  SPACE
                       SPA-SFILESV-TO-DATA   (SPA-SFILESV-IDX)
                                           DELIMITED  BY  SPACE
                                           INTO  TFIL-ENTRY1
               END-STRING
               MOVE    SPA-SFILESV-TITLE   (SPA-SFILESV-IDX)
                                           TO  TFIL-TITLE
               MOVE    SPA-SFILESV-CNT-ALL (SPA-SFILESV-IDX)
                                           TO  WRK-KENSU-Z
               MOVE    WRK-KENSU-Z         TO  TFIL-KENSU
           END-IF
      *
           MOVE    ZERO                TO  T99-DURATION
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "TFIL"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
           .
       3001-FILEENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    クライアント保存情報更新（処理日）処理
      *****************************************************************
       4903-FILESV-UPDATE-SEC          SECTION.
      *
           IF      SPA-T99-MOTOPG  =   "T04"
               INITIALIZE                  ORCSFILESVAREA
               MOVE    "U"             TO  SFILESV-MODE
               MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SFILESV-TBL-KEY
               MOVE    SPA-NAI-SKYYM   TO  SFILESV-SRYYM   (1)
               MOVE    SPA-NAI-T04-TSHELLID      (SPA-NAI-T04-SELNUM)
                                       TO  SFILESV-SHELLID (1)
               MOVE    SPA-NAI-T04-TSHORI-RENNUM (SPA-NAI-T04-SELNUM)
                                       TO  SFILESV-SHORI-RENNUM (1)
               MOVE    SPA-NAI-T04-TRENNUM       (SPA-NAI-T04-SELNUM)
                                       TO  SFILESV-RENNUM  (1) 
               CALL    "ORCSFILESV"    USING
                                           ORCSFILESVAREA
                                          SPA-AREA
           ELSE
               INITIALIZE                  ORCSFILESVAREA
               MOVE    "U"             TO  SFILESV-MODE
               MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SFILESV-TBL-KEY
               MOVE    SPA-NAI-SKYYM   TO  SFILESV-SRYYM   (1)
               MOVE    WRK-CONS-BTPARA-SCRIPTID
                                       TO  SFILESV-SHELLID (1)
               MOVE    ZERO            TO  SFILESV-SHORI-RENNUM (1)
               MOVE    1               TO  SFILESV-RENNUM  (1) 
               CALL    "ORCSFILESV"    USING
                                           ORCSFILESVAREA
                                          SPA-AREA
           END-IF
      *
           .
       4903-FILESV-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージセット処理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           EVALUATE    JOB-ERRCD
               WHEN    "0001"
                   MOVE    "【ファイルがありません】" 
                                               TO  T99-MSG
               WHEN    "0002"
                   MOVE    "【ｆｄｄが起動されていません】" 
                                               TO  T99-MSG
               WHEN    "0003"
               WHEN    "0004"
                   MOVE    "【ｆｄｄで処理が失敗しました】" 
                                               TO  T99-MSG
               WHEN    "0011"
                   MOVE    "【媒体がマウントできませんでした】" 
                                               TO  T99-MSG
               WHEN    "0012"
                   MOVE    "【媒体がアンマウントできませんでした】" 
                                               TO  T99-MSG
               WHEN    "0021"
                   MOVE    "【ファイルコピーでエラーが発生しました】" 
                                               TO  T99-MSG
               WHEN    "0077"
                   MOVE    "【履歴フォルダの作成に失敗しました】" 
                                               TO  T99-MSG
               WHEN    "0127"
                   MOVE    "【スクリプトが実行できませんでした】" 
                                               TO  T99-MSG
           END-EVALUATE
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "1001"
                   MOVE    "対象のパラメタＤＢが存在しません" 
                                               TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "パラメタＤＢの更新処理でエラーとなりました" 
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  TERR
           MOVE    SPA-ERRCD            TO  TERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  TERR-ERRMSG
           MOVE    "B99"                TO  MCP-WIDGET
      *
           MOVE    "T99"                TO  SPA-MOTOPG
           MOVE    "TERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "TERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-SEC         SECTION.
      *
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-BTPARA-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-BTPARA
               INITIALIZE                      BTPARA-REC
           END-IF
      *
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-BTPARA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-N-SEC          SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  BTPARA-REC
               MOVE    ZERO                TO  FLG-BTPARA
           ELSE
               MOVE    1                   TO  FLG-BTPARA
               INITIALIZE                      BTPARA-REC
           END-IF
      *
           .
       900-BTPARA-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
      *
