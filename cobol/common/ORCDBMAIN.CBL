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
       PROGRAM-ID.             ORCDBMAIN.
      *****************************************************************
      *  システム名        : ORCA
      *  サブシステム名    : 
      *  コンポーネント名  : ORCA DB アクセスメインモジュール
      *  返却エラーコード  : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/01/23    NACL-竹田     新規作成
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
       
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDX3                PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD              PIC X(08).
           03  WRK-MCP-RC              PIC ---9.
           03  WRK-STR                 PIC X(500).
           03  WRK-ERRMSG              PIC X(500).
           03  WRK-PATHNAME            PIC X(64).
           03  WRK-FUNC                PIC X(64).
           03  WRK-TABLE               PIC X(64).
      *
       01  ORCXKANACONVAREA.
      *   IN/OUTの長さをMCPDARA-RECに合せるため独自に引数領域を編集
      *   出力バイト数
           03 KANACONV-LEN             PIC S9(9) BINARY.
      *   返り値( 0: 正常 1: 範囲外文字 2: 外字、変換不能 )
           03 KANACONV-RETURN          PIC S9(9) BINARY.
      *   変換文字列数(バイト)
           03 KANACONV-MAXLEN          PIC S9(9) BINARY.
      *   変換フラグ (0: 変換しない,     1: 変換する
      *               2: 1+カタカナ変換, 3: 1+ひらがな変換)
      *   ※ 半角カナおよび外字は全てのフラグで変換される
      *      か捨てられる。
      *     (外字は■)
           03 KANACONV-CONV-FLAG       PIC S9(9) BINARY.
      *   出力文字種指定
      *       0: 半角カナ、外字のみ変換
      *      14: 全角   2: ひらがな  4: カタカナ
      *      112: ASCII 16: 数字     32: アルファベット
      *      15: 全角+スペース改行   113: ASCII+スペース改行 
      *   出力文字種は OR をとることで複数指定できる
      *   例: (6: ひらがな、カタカナのみ)
      *       (48: 英数字のみ)
           03 KANACONV-CHAR-TYPE       PIC S9(9) BINARY.
      *   文字領域の長さ(INDATA, OUTDATAの長さと同じであること)
           03 KANACONV-STLEN           PIC S9(9) BINARY.
      *   入力文字列
           03 KANACONV-INDATA          PIC X(50000).
      *   出力文字列
           03 KANACONV-OUTDATA         PIC X(50000).
      *
       01  CONST-AREA.
           03 CONST-MAX-LEN            PIC 9(08)   VALUE 50000.
      *
           COPY    "MCPDATA.INC"       REPLACING   //MCPDATA-//
                                       BY          //WKMCPDATA-//.
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
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
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *    共通領域
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *
       PROCEDURE                    DIVISION    USING
           MCPAREA
           MCPDATA-REC
           SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
      *
           MOVE    MCP-TABLE           TO  WRK-TABLE
           MOVE    MCP-PATHNAME        TO  WRK-PATHNAME
           MOVE    MCP-FUNC            TO  WRK-FUNC
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( MCP-TABLE           =   "metadb"
                                        OR "blob" )
               CONTINUE
           ELSE
      *
               EVALUATE    MCP-FUNC
               WHEN    "DBOPEN"
               WHEN    "DBDISCONNECT"
               WHEN    "DBSTART"
               WHEN    "DBCOMMIT"
               WHEN    "PUTWINDOW"
                   MOVE    LOW-VALUE       TO  MCP-TABLE
                                               MCP-PATHNAME
               WHEN    OTHER
                   IF  MCP-FUNC            =   "DBFETCH"
                       MOVE    LOW-VALUE   TO  MCPDATA-REC
                   END-IF
                   IF    ( SPA-NOCHK-HOSPNUM   =   "1" )
                       CONTINUE
                   ELSE
                       IF    ( SPA-HOSPNUM     =   ZERO )
                        OR   ( SPA-HOSPNUM     IS  NOT NUMERIC )
      *
                           MOVE   1            TO  FLG-ERR
                       END-IF
                   END-IF
               END-EVALUATE
           END-IF
      *
           IF    ( FLG-ERR         =   ZERO )
      *
               MOVE    1           TO  MCP-VERSION
               MOVE    1           TO  MCP-REDIRECT
      *
               IF    ( MCP-TABLE   =   "tbl_srysrh"
                                    OR "tbl_skysrh"
                                    OR "tbl_rrksrh"
                                    OR "tbl_ptsrh"
                                                    )
                   MOVE    2       TO  MCP-VERSION
                   MOVE    0       TO  MCP-REDIRECT
               END-IF
      *
               IF    ( MCP-TABLE   =   "tbl_chksnd" )
                   IF    ( MCP-PATHNAME (1:3)  =   "tmp" )
                       MOVE    2   TO  MCP-VERSION
                       MOVE    0   TO  MCP-REDIRECT
                   END-IF
               END-IF
      *
               IF    ( MCP-TABLE   =   "tbl_jobkanri" )
                   MOVE    MCPDATA-REC TO  JOBKANRI-REC
                   IF    ( JOB-SHELLID =   "PRGMNT" )
                       MOVE    2   TO  MCP-VERSION
                       MOVE    0   TO  MCP-REDIRECT
                   END-IF
               END-IF
      *
               EVALUATE    MCP-TABLE
               WHEN    "tbl_sryacct"
                   CALL    "ORCDBACCT" USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
               WHEN    "tbl_seikyu"
                   CALL    "ORCDBSEIKYU" USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
               WHEN    "tbl_syunou"
                   CALL    "ORCDBSYUNOU" USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
               WHEN    OTHER
      *
                   CALL    "MONFUNC"   USING
                                       MCPAREA
                                       MCPDATA-REC
               END-EVALUATE
      *
               IF    ( MCP-RC      =   ZERO OR 1)
                   CONTINUE
               ELSE
                   PERFORM 800-EDIT-ERRMSG-SEC
               END-IF
      *
           ELSE
               MOVE    51          TO  MCP-RC
               PERFORM 800-EDIT-ERRMSG-SEC
           END-IF
      *
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           MOVE    WRK-FUNC            TO  MCP-FUNC
           MOVE    ZERO                TO  MCP-LOGFLAG
      *
           .
      *
           EXIT PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    ZERO                TO  MCP-LOGFLAG
           IF      MCP-FUNC        =  "DBINSERT" OR "DBUPDATE" 
                                   OR "DBDELETE"
               PERFORM 1001-AUDITLOG-PUT-SEC
           END-IF
      *
           .
      *
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    監視ログ出力指示処理
      *****************************************************************
       1001-AUDITLOG-PUT-SEC             SECTION.
      *
      *    IF      SPA-TERMID      =   SPACE
      *        MOVE   "batch"      TO  MCP-LOGCOMMENT
      *    END-IF
      *
      *    EVALUATE    MCP-TABLE
      *    WHEN    "tbl_lock"
      *    WHEN    "tbl_ptinf"
      *    WHEN    "tbl_jobkanri"
      *        MOVE    1           TO  MCP-LOGFLAG
      *    END-EVALUATE
           .
      *
       1001-AUDITLOG-PUT-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       800-EDIT-ERRMSG-SEC             SECTION.
      *
           MOVE   SPACE            TO  WRK-ERRMSG
           MOVE   MCP-RC           TO  WRK-MCP-RC
      *
           STRING "ORCDBMAIN ERROR RC[" DELIMITED   BY  SIZE
                  WRK-MCP-RC           DELIMITED   BY  SIZE
                  "] HOSPNUM["         DELIMITED   BY  SIZE
                  SPA-HOSPNUM          DELIMITED   BY  SIZE
                  "] FUNC["            DELIMITED   BY  SIZE
                  MCP-FUNC             DELIMITED   BY  SPACE
                  "] TABLE["           DELIMITED   BY  SIZE
                  MCP-TABLE            DELIMITED   BY  SPACE
                  "] PATHNAME["        DELIMITED   BY  SIZE
                  MCP-PATHNAME         DELIMITED   BY  SPACE
                  "] SPA["             DELIMITED   BY  SIZE
                  SPA-STAFFCD          DELIMITED   BY  SIZE
                  SPA-SYSYMD           DELIMITED   BY  SIZE
                  SPA-SYSYMDW          DELIMITED   BY  SIZE
                  SPA-SYSYMDWH         DELIMITED   BY  SIZE
                  SPA-SYSYMD-YOBI      DELIMITED   BY  SIZE
                  SPA-MACHIPG          DELIMITED   BY  SIZE
                  SPA-MOTOPG           DELIMITED   BY  SIZE
                  SPA-MOTOPG2          DELIMITED   BY  SIZE
                  "]"                  DELIMITED   BY  SIZE
                  LOW-VALUE            DELIMITED   BY  SIZE
           INTO   WRK-ERRMSG
           END-STRING
      *
           CALL    "coblog"    USING   WRK-ERRMSG
      *
           IF    ( MCP-FUNC    =   "DBINSERT"
                                OR "DBUPDATE" )
               MOVE    ZERO            TO  IDX1
               MOVE    ZERO            TO  KANACONV-RETURN
               PERFORM UNTIL ( IDX1    >=  CONST-MAX-LEN )
                        OR   ( MCPDATA-REC (IDX1 : )   =   SPACE )
                        OR   ( KANACONV-RETURN NOT =   ZERO )
      *
                   COMPUTE IDX1    =   IDX1    +   1
                   MOVE    MCPDATA-REC (1 :IDX1)
                                       TO  WKMCPDATA-REC
                   PERFORM 800-KANACONV-SEC
      *
               END-PERFORM
      *
               IF    ( KANACONV-RETURN NOT =   ZERO )
                AND  ( IDX1                >   1 )
                   COMPUTE IDX1    =   IDX1    -   1
               END-IF
      *
      *-----------------------------------------------------------------
               DISPLAY "ORCDBMAIN MCPDATA-REC(1:" IDX1 ")=["
                       MCPDATA-REC (1 : IDX1) "]"
      *-----------------------------------------------------------------
      *
               MOVE    MCPDATA-REC     TO  WKMCPDATA-REC
               PERFORM 800-KANACONV-SEC
               MOVE    ZERO            TO  IDX1
               PERFORM UNTIL ( IDX1    >=  CONST-MAX-LEN )
                        OR   ( MCPDATA-REC (IDX1 : )   =   SPACE )
      *
                   COMPUTE IDX1    =   IDX1    +   1
      *
                   IF    ( MCPDATA-REC (IDX1 : 1)
                                   NOT =  KANACONV-OUTDATA  (IDX1 : 1))
      *
                       MOVE    1        TO   IDX3
                       IF    ( IDX1     >    9 )
                           COMPUTE IDX3 =    IDX1  -   9
                       END-IF
      *
      *-----------------------------------------------------------------
                       DISPLAY "  *** FOUND BINARY"
                               " IN MCPDATA-REC:" IDX1
                               " MCPDATA-REC(" IDX3 ":" "20)["
                               MCPDATA-REC (IDX3 : 20) "]"
      *-----------------------------------------------------------------
      *
                       MOVE    MCPDATA-REC (IDX1 : )
                                        TO  WKMCPDATA-REC(IDX1:)
      *
                       PERFORM UNTIL ( IDX1    >=  CONST-MAX-LEN )
                                OR   ( MCPDATA-REC (IDX1 : ) =   SPACE )
                                OR   ( MCPDATA-REC (IDX1 : 1)
                                       =   KANACONV-OUTDATA (IDX1 : 1) )
                           COMPUTE IDX1    =   IDX1    +   1
                           MOVE    SPACE   TO  WKMCPDATA-REC
                           MOVE    MCPDATA-REC (IDX1 :)
                                           TO  WKMCPDATA-REC (IDX1 :)
                           PERFORM 800-KANACONV-SEC
                       END-PERFORM
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       800-EDIT-ERRMSG-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       800-KANACONV-SEC             SECTION.
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    CONST-MAX-LEN   TO  KANACONV-LEN
                                       KANACONV-STLEN
           MOVE    0               TO  KANACONV-CONV-FLAG
           MOVE    113             TO  KANACONV-CHAR-TYPE
           MOVE    WKMCPDATA-REC   TO  KANACONV-INDATA
           CALL    "kanaconv"      USING ORCXKANACONVAREA
      *
           .
       800-KANACONV-EXT.
           EXIT.
      *
