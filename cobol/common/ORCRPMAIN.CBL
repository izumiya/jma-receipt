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
       PROGRAM-ID.             ORCRPMAIN.
      *****************************************************************
      *  システム名        : ORCA
      *  サブシステム名    : 
      *  コンポーネント名  : 外部連携用帳票データ作成サブ
      *  返却エラーコード  : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  16/05/26    NACL-太田     新規作成
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPCOMMONPRT.INC".
           COPY    "CPASGNPARA.INC"    REPLACING   //ASGNPARA//
                                       BY          //OUTPARA//.
      *
      *    シェル用領域
           COPY    "CPSHELLTBL.INC".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
       
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-NUM2-G.
               05  WRK-NUM2            PIC 9(02).
               05  WRK-SHORI-RENNUM    PIC 9(02).
      *
       01  SGYOUMU-CODE                PIC 9(03).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "ORCA-BLOB".
           COPY    "MCPDATA.INC".
           COPY    "XMLDATA.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
       01  SPRTLNK-AREA.
           COPY    "CPORCSPRTLNK.INC".
      *
      *   ＵＩＤ取得用エリア
       01 UIDIO-ST         PIC 9(2).
       01 UIDIO-AREA.
           03  UIDIO-RET   PIC X(2).
           03  UIDIO-UID   PIC X(36).
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *    共通領域
           COPY    "MCPAREA".
      *
           COPY    "CPORCRPMAIN.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *
       PROCEDURE                    DIVISION    USING
           MCPAREA
           RPMAIN-AREA
           SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                      WRK-AREA
           INITIALIZE                      FLG-AREA
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
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
           CALL     "ORCSGYOUMUGET"    USING
                                       SPA-AREA
                                       SGYOUMU-CODE
      *
           CALL    "orcuidset"         USING
                                       UIDIO-ST
                                       UIDIO-AREA
      *
           INITIALIZE                      HC01PARA
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
           MOVE    "RP00"              TO  HC01PARA-FORM-ID
           MOVE    UIDIO-UID           TO  HC01PARA-TERMID
           MOVE    SMCNDATE-TIME       TO  HC01PARA-SYOHMS
           IF      SPA-PRT-FLG         IS  NUMERIC
               MOVE SPA-PRT-FLG        TO  WRK-NUM2-G
           ELSE
               MOVE ZERO               TO  WRK-NUM2-G
           END-IF
           MOVE    WRK-NUM2            TO  HC01PARA-CNT
           INSPECT HC01PARA-BASENAME   REPLACING  ALL " " BY "_"
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HC01PARA-BASENAME   TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"           USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  OUTPARA
      *
           .
      *
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM 2001-JSON-EXPORT-SEC
      *
           PERFORM 2001-REPRINT-SEC
      *
           .
      *
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    データ出力処理（ＪＳＯＮ形式）
      *****************************************************************
       2001-JSON-EXPORT-SEC            SECTION.
      *
           INITIALIZE                      XMLDATA-REC
           MOVE    3                   TO  XMLDATA-MODE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-XMLOPEN-SEC
      *
           MOVE    RPMAIN-DATA         TO  XMLDATA-DATA
           MOVE    "data"              TO  XMLDATA-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
      *
           PERFORM 910-XMLCLOSE-SEC
      *
           INITIALIZE                      ORCA-BLOB
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "blob"              TO  MCP-TABLE
           MOVE     XMLDATA-OBJECT     TO  ORCA-BLOB-OBJECT
           MOVE     OUTPARA            TO  ORCA-BLOB-FILE
           PERFORM 910-BLOBEXPORT-SEC
      *
           .
      *
       2001-JSON-EXPORT-EXT.
           EXIT.
      *****************************************************************
      *    再印刷データ作成処理
      *****************************************************************
       2001-REPRINT-SEC                SECTION.
      *
           MOVE    SPACE               TO  SHELLTBL
           INITIALIZE                      SHELLTBL
      *
           MOVE    "online-reprint.sh" TO  SHELLTBL-NAME
           MOVE    1                   TO  LNK-PRTKANRI-RENNUM
           MOVE    "ONLINE"            TO  LNK-PRTKANRI-TBL-KEY
           MOVE    SPA-PRT-GROUP       TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    SPA-SYSYMD          TO  LNK-PRTKANRI-SKYYMD
                                           LNK-PRTKANRI-SRYYM
           MOVE    1                   TO  LNK-PRTKANRI-PRIORITY
           MOVE    SPA-TERMID          TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID            TO  LNK-PRTKANRI-OPID
      *
           MOVE    LNK-PRTKANRI-RENNUM TO  SHELLTBL-ARG1
           MOVE    LNK-PRTKANRI-TBL-KEY TO SHELLTBL-ARG2
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SHELLTBL-ARG3
           IF      SPA-PRT-FLG     IS  NUMERIC
               MOVE SPA-PRT-FLG        TO  WRK-SHORI-RENNUM
           ELSE
               MOVE ZERO               TO  WRK-SHORI-RENNUM
           END-IF
           COMPUTE LNK-PRTKANRI-SHORI-RENNUM
               =   WRK-SHORI-RENNUM    +   1000
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SHELLTBL-ARG4
           MOVE    LNK-PRTKANRI-SRYYM  TO  SHELLTBL-ARG5
           MOVE    LNK-PRTKANRI-SKYYMD TO  SHELLTBL-ARG6
           MOVE    "API"               TO  SHELLTBL-ARG7
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SHELLTBL-ARG8
           MOVE    LNK-PRTKANRI-TERMID
                                       TO  SHELLTBL-ARG9
           MOVE    LNK-PRTKANRI-OPID
                                       TO  SHELLTBL-ARG10
           MOVE    RPMAIN-ID           TO  SHELLTBL-ARG11
           MOVE    SPA-SRYYMD          TO  SHELLTBL-ARG12
           MOVE    SPA-PRT-UUID        TO  SHELLTBL-ARG13
           MOVE    SPA-PTID            TO  SHELLTBL-ARG14
      *    SITE_KBN
           MOVE    "1"                 TO  SHELLTBL-ARG15
      *    
           MOVE    SGYOUMU-CODE        TO  SHELLTBL-ARG16
           MOVE    RPMAIN-TITLE        TO  SHELLTBL-ARG17
      *    DIA
      *    MOVE    MKPRT-ID            TO  SHELLTBL-ARG18
      *    FORM
           MOVE    RPMAIN-ID           TO  SHELLTBL-ARG19
      *    DEF
           MOVE    SPACE               TO  SHELLTBL-ARG20
      *    データ内容
           MOVE    HC01PARA-BASENAME   TO  SHELLTBL-ARG21
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG22
      *
      *    IF    ( SPA-CLIENT-PRT-FLG  =   1 )
      *     AND  ( SPA-CLIENT-TEMP-FLG =   2 )
      *        MOVE    "D"             TO  SHELLTBL-ARG23
      *    ELSE
               MOVE    "1"             TO  SHELLTBL-ARG23
      *    END-IF
           MOVE    HC01PARA-BASENAME   TO  SHELLTBL-ARG24
      *
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "daily"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       2001-REPRINT-EXT.
           EXIT.
      *****************************************************************
      *    ＸＭＬＯＰＥＮ処理
      *****************************************************************
       910-XMLOPEN-SEC                 SECTION.
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           CALL    "MONFUNC"               USING
                                           MCPAREA
                                           XMLDATA-REC
      *
           .
       900-XMLOPEN-EXT.
           EXIT.
      *****************************************************************
      *    ＸＭＬＷＲＩＴＥ処理
      *****************************************************************
       910-XMLWRITE-SEC                SECTION.
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           CALL    "MONFUNC"               USING
                                           MCPAREA
                                           XMLDATA-REC
      *
           .
       900-XMLWRITE-EXT.
           EXIT.
      *****************************************************************
      *    ＸＭＬＣＬＯＳＥ処理
      *****************************************************************
       910-XMLCLOSE-SEC                SECTION.
      *
           MOVE    "XMLCLOSE"          TO  MCP-FUNC
           CALL    "MONFUNC"               USING
                                           MCPAREA
                                           XMLDATA-REC
      *
           .
       900-XMLCLOSE-EXT.
           EXIT.
      *****************************************************************
      *    ＢＬＯＢＥＸＰＯＲＴ処理
      *****************************************************************
       910-BLOBEXPORT-SEC              SECTION.
      *
           MOVE    "BLOBEXPORT"        TO  MCP-FUNC
           CALL    "MONFUNC"               USING
                                           MCPAREA
                                           ORCA-BLOB
      *
           .
       900-BLOBEXPORT-EXT.
           EXIT.
