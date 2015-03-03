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
       IDENTIFICATION                      DIVISION.
       PROGRAM-ID.                         ORCSI01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入退院登録
      *  コンポーネント名  : 入院料加算算定可否チェック
      *  管理者            : 
      *  作成日付    作業者        記述
      *  15/01/15    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                         DIVISION.
       CONFIGURATION                       SECTION.
       INPUT-OUTPUT                        SECTION.
       DATA                                DIVISION.
       FILE                                SECTION.
       WORKING-STORAGE                     SECTION.
      *
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX-KSN                     PIC 9(05).
      *
       01  CNT-AREA.
           03  CNT-KSN                     PIC 9(05).
           03  CNT-KSNLST                  PIC 9(05).
      *
       01  FLG-AREA.
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-NYUKSNCHK               PIC 9(01).
           03  FLG-NRRKSRH                 PIC 9(01).
           03  FLG-HIT                     PIC 9(01).
      *
       01  WRK-AREA.
           03  WRK-KSNNAME                 PIC X(100).
           03  WRK-KHN-NYUKSNKBN           PIC X(03).
           03  WRK-KSN-NYUKSNKBN           PIC X(03).
           03  WRK-SRYCD                   PIC X(09).
      *
       01  CONST-AREA.
           03  CONST-H240401       PIC X(08)   VALUE   "20120401".
           03  CONST-GMN-KSN-MAX           PIC 9(05)   VALUE   3.
           03  CONST-KSNCD-G.
               05  CONST-KSNCD-MAX         PIC 9(05)   VALUE   8.
               05  CONST-KSNCD-VAL.
                   07  CONST-KSNCD-190142970
                                   PIC X(09)   VALUE   "190142970".
                   07  CONST-KSNCD-190143370
                                   PIC X(09)   VALUE   "190143370".
                   07  CONST-KSNCD-190134270
                                   PIC X(09)   VALUE   "190134270".
                   07  CONST-KSNCD-190116990
                                   PIC X(09)   VALUE   "190116990".
                   07  CONST-KSNCD-190135470
                                   PIC X(09)   VALUE   "190135470".
                   07  CONST-KSNCD-190135670
                                   PIC X(09)   VALUE   "190135670".
                   07  CONST-KSNCD-190177170
                                   PIC X(09)   VALUE   "190177170".
                   07  CONST-KSNCD-190152470
                                   PIC X(09)   VALUE   "190152470".
               05  CONST-KSNCD-R REDEFINES CONST-KSNCD-VAL.
                   07  CONST-KSNCD-OCC     OCCURS  8.
                       09  CONST-KSNCD     PIC X(09).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *    入院料加算チェック
       01  NYUKSNCHK-REC.
           COPY    "CPNYUKSNCHK.INC".
      *
      *    入院履歴照会
       01  NRRKSRH-REC.
           COPY    "CPNRRKSRH.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                             SECTION.
      *
           COPY    "CPORCSI01.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                           DIVISION    USING
           SI01-AREA
           SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                        SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           EXIT PROGRAM
      *
           .
       000-PROC-EXT.
           EXIT.
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                        SECTION.
      *
           INITIALIZE                      IDX-AREA
                                           CNT-AREA
                                           FLG-AREA
                                           WRK-AREA
      *
                                           SI01-JOKUSOUYMD
                                           SI01-JOKUSOUKBN
                                           SI01-KSN-G
                                           SI01-KSNLST-G
      *
           .
       100-INIT-EXT.
           EXIT.
           .
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                        SECTION.
      *
      *
           MOVE    ZERO                TO  CNT-KSNLST
      *
           PERFORM VARYING IDX-KSN     FROM    1   BY  1
                   UNTIL ( IDX-KSN     >   CONST-KSNCD-MAX )
      *
               MOVE    CONST-KSNCD (IDX-KSN)
                                       TO  WRK-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       =   ZERO )
                   IF    ( TNS-HOSPSRYKBN  =   0 )
                    OR   ( TNS-HOSPSRYKBN  =   SPA-HOSPSBT )
      *
                       COMPUTE CNT-KSNLST
                           =   CNT-KSNLST  +   1
      *
                       MOVE    CNT-KSNLST
                                       TO  SI01-KSNLST-MAX
                       MOVE    TNS-SRYCD
                                       TO  SI01-KSNLST-SRYCD(CNT-KSNLST)
                       MOVE    TNS-NAME
                                       TO  SI01-KSNLST-NAME (CNT-KSNLST)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPACE               TO  WRK-KSNNAME
                                           WRK-KHN-NYUKSNKBN
                                           WRK-KSN-NYUKSNKBN
      *
           MOVE    ZERO                TO  CNT-KSN
      *
           PERFORM VARYING IDX-KSN     FROM    1   BY  1
                   UNTIL ( IDX-KSN     >   CONST-KSNCD-MAX )
                    OR   ( CNT-KSN     >=  CONST-GMN-KSN-MAX )
      *
               MOVE    1               TO  FLG-HIT
      *
               MOVE    CONST-KSNCD (IDX-KSN)
                                       TO  WRK-SRYCD
      *
               MOVE    CONST-KSNCD (IDX-KSN)
                                       TO  WRK-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       =   ZERO )
                   MOVE    TNS-NAME    TO  WRK-KSNNAME
                   MOVE    TNS-NYUKHNKSNKBN
                                       TO  WRK-KSN-NYUKSNKBN
               ELSE
                   MOVE     ZERO       TO  FLG-HIT
               END-IF
      *
               IF    ( FLG-HIT         =   1 )
                   IF    ( TNS-HOSPSRYKBN  =   0 )
                    OR   ( TNS-HOSPSRYKBN  =   SPA-HOSPSBT )
                       CONTINUE
                   ELSE
                       MOVE    ZERO    TO  FLG-HIT
                   END-IF
               END-IF
      *
               IF    ( FLG-HIT         =   1 )
      *
                   EVALUATE    SI01-SNTNYUINCD
                   WHEN    "099999950"
                       MOVE    "190097010"         TO  WRK-SRYCD
                   WHEN    "099999951"
                       MOVE    "190097210"         TO  WRK-SRYCD
                   WHEN    "099999952"
                       MOVE    "190135110"         TO  WRK-SRYCD
                   WHEN    "099999955"
                       MOVE    "190169610"         TO  WRK-SRYCD
                   WHEN    "099999956"
                       MOVE    "190169910"         TO  WRK-SRYCD
                   WHEN    "099999957"
                       MOVE    "190170210"         TO  WRK-SRYCD
                   WHEN    OTHER
                       MOVE    SI01-SNTNYUINCD     TO  WRK-SRYCD
                   END-EVALUATE
      *
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   MOVE    TNS-NYUKHNKSNKBN        TO  WRK-KHN-NYUKSNKBN
      *
                   IF    ( TNS-NYUKHNKSNKBN        >   ZERO )
                       PERFORM 900-NYUKSNCHK-KEY-SEL-SEC
                       IF    ( NYUKSNCHK-CHKKBN    =   "1" OR "2" )
                           CONTINUE
                       ELSE
                           PERFORM 2001-NYUKSN-CHK-SEC
                       END-IF
                   ELSE
                           MOVE    ZERO            TO  FLG-HIT
                   END-IF
               END-IF
      *
      *        褥瘡患者管理加算の算定済みチェック
               IF    ( FLG-HIT         =   1 )
                AND  ( CONST-KSNCD (IDX-KSN)   =  "190116990")
                   MOVE    CONST-KSNCD (IDX-KSN)
                                       TO  WRK-SRYCD
                   PERFORM 2001-JOKUSOU-CHK-SEC
               END-IF
      *
               IF    ( FLG-HIT         =   1 )
                   COMPUTE CNT-KSN =   CNT-KSN +   1
                   MOVE    CNT-KSN     TO  SI01-KSN-MAX
                   MOVE    WRK-KSNNAME TO  SI01-KSN-NAME  (CNT-KSN)
                   MOVE    CONST-KSNCD (IDX-KSN)
                                       TO  SI01-KSN-SRYCD (CNT-KSN)
               END-IF
      *
           END-PERFORM
      *
           .
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    入院料加算算定可否チェック
      *****************************************************************
       2001-NYUKSN-CHK-SEC             SECTION.
      *
           EVALUATE    TRUE
           WHEN ( TNS-CDKBN-KBN            =   "A" )
            AND ( TNS-CDKBN-KBNNUM         =   308 )
            AND ( TNS-CDKBN-KBNNUM-EDA     =   3 )
               IF    ( CONST-KSNCD (IDX-KSN)
                                           =   CONST-KSNCD-190177170 )
                   CONTINUE
               ELSE
                   MOVE    ZERO            TO  FLG-HIT
               END-IF
           WHEN   OTHER
               EVALUATE    TNS-NYUKHNKSNKBN
               WHEN    2   THRU    3
                   IF    ( CONST-KSNCD (IDX-KSN)
                                           =   CONST-KSNCD-190142970 )
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  FLG-HIT
                   END-IF
               WHEN    152
               WHEN    173    THRU    190
                   IF    ( CONST-KSNCD (IDX-KSN)
                                           =   CONST-KSNCD-190134270
                                            OR CONST-KSNCD-190143370 )
                       IF    ( SI01-SRYYMD
                                          >=    CONST-H240401 )
                           PERFORM 2001-RYOYO-CHK-SEC
                       END-IF
                   ELSE
                       MOVE    ZERO        TO  FLG-HIT
                   END-IF
                   IF    ( SI01-BTUSBT     =   "04" )
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  FLG-HIT
                   END-IF
               WHEN    032
               WHEN    036
               WHEN    037
               WHEN    081
               WHEN    083
               WHEN    172
                   IF    ( CONST-KSNCD (IDX-KSN)
                                           =   CONST-KSNCD-190135470 )
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  FLG-HIT
                   END-IF
               WHEN    153    THRU    158
                   IF    ( CONST-KSNCD (IDX-KSN)
                                           =   CONST-KSNCD-190135670 )
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  FLG-HIT
                   END-IF
               WHEN    253
               WHEN    254
                   IF    ( CONST-KSNCD (IDX-KSN)
                                           =   CONST-KSNCD-190152470 )
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  FLG-HIT
                   END-IF
               WHEN    OTHER
                   MOVE    ZERO            TO  FLG-HIT
               END-EVALUATE
          END-EVALUATE
      *
           .
       2001-NYUKSN-CHK-EXT.
           EXIT.
           .
      *****************************************************************
      *    療養病棟　救急・在宅等支援療養病床初期加算算定可否チェック
      *****************************************************************
       2001-RYOYO-CHK-SEC        SECTION.
      *
           EVALUATE    WRK-KHN-NYUKSNKBN
           WHEN    173 THRU 181
               IF    ( CONST-KSNCD (IDX-KSN)
                                       =   CONST-KSNCD-190143370 )
                   CONTINUE
               ELSE
                   MOVE    ZERO        TO  FLG-HIT
               END-IF
           WHEN    152
           WHEN    182 THRU 190
               IF    ( CONST-KSNCD (IDX-KSN)
                                       =   CONST-KSNCD-190134270 )
                   CONTINUE
               ELSE
                   MOVE    ZERO        TO  FLG-HIT
               END-IF
           END-EVALUATE
      *
           .
       2001-RYOYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    褥瘡患者管理加算チェック処理
      *    SI01-JOKUSOUKBN  =  "1" OR "2"（褥瘡患者管理加算算定済み）
      *    SI01-JOKUSOUKBN  =  "1" （異動日に加算が算定されていない場合）
      *    SI01-JOKUSOUKBN  =  "2" （異動日に加算が算定されている場合）
      *****************************************************************
       2001-JOKUSOU-CHK-SEC            SECTION.
      *
           MOVE    SPACE               TO  SI01-JOKUSOUYMD
                                           SI01-JOKUSOUKBN
      *
           PERFORM 900-NRRKSRH-KEY4-SEL-SEC
           IF    ( FLG-NRRKSRH         =   ZERO )
               IF    ( SI01-SRYYMD     =   NRRKSRH-SRYYMD )
                   MOVE   "2"          TO  SI01-JOKUSOUKBN
               ELSE
                   MOVE   "1"          TO  SI01-JOKUSOUKBN
               END-IF
           END-IF
      *
           .
       2001-JOKUSOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                        SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    WRK-SRYCD       TO  TNS-SRYCD
           MOVE    SI01-SRYYMD     TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSU-KEY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料加算チェックテーブル検索処理(KEY)
      *****************************************************************
       900-NYUKSNCHK-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUKSNCHK
      *
           INITIALIZE                      NYUKSNCHK-REC
           MOVE    WRK-KHN-NYUKSNKBN   TO  NYUKSNCHK-NYUINKBN
           MOVE    WRK-KSN-NYUKSNKBN   TO  NYUKSNCHK-KSNKBN
           MOVE    SI01-SRYYMD         TO  NYUKSNCHK-YUKOSTYMD
                                           NYUKSNCHK-YUKOEDYMD
           MOVE    NYUKSNCHK-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuksnchk"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUKSNCHK-REC
           ELSE
               INITIALIZE                  NYUKSNCHK-REC
               MOVE    1               TO  FLG-NYUKSNCHK
           END-IF
      *
           MOVE    "tbl_nyuksnchk"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-NYUKSNCHK-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴照会テーブル検索処理
      *****************************************************************
       900-NRRKSRH-KEY4-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-NRRKSRH
      *
           INITIALIZE                  NRRKSRH-REC
           MOVE    SPA-HOSPNUM     TO  NRRKSRH-HOSPNUM
           MOVE    SI01-PTID       TO  NRRKSRH-PTID
           MOVE    WRK-SRYCD       TO  NRRKSRH-SELSRYCD
           MOVE    SI01-NYUINYMD   TO  NRRKSRH-UPYMD
           MOVE    SI01-SRYYMD     TO  NRRKSRH-SRYYMD
           MOVE    NRRKSRH-REC     TO  MCPDATA-REC
      *
           MOVE    "tbl_nrrksrh"   TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  NRRKSRH-REC
           ELSE
               MOVE    1           TO  FLG-NRRKSRH
           END-IF
      *
           MOVE    "tbl_nrrksrh"   TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-NRRKSRH-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
