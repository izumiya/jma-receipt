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
       PROGRAM-ID.             ORCBQ03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者照会
      *  コンポーネント名  : 検索結果データ作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/10/31    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付       内容
      *  03.05.00    NACL-太田    07.05.23 グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    パラメータファイル
           SELECT  BQ01-FILE       ASSIGN  PRFNAME
                                   ORGANIZATION    IS  SEQUENTIAL
                                   FILE    STATUS  IS  STS-BQ01.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    パラメータファイル
       FD  BQ01-FILE.
           COPY    "CPBQ01.INC".
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    シェル用領域
           COPY    "CPSHELLTBL.INC".
      *
      *    ワークパラメータファイル
           COPY    "CPBQ01.INC"    REPLACING  //BQ01//
                                   BY         //WKBQ01//.
      *
       01  ASSIGN-NAME-AREA.
           03  PRFNAME                 PIC X(100).
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-BQ01                PIC X(02).
      *
      *    インデックス領域
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
      *
       01  SEL-AREA.
           03  SEL-SRYKA                           PIC X(02).
           03  SEL-NYUGAIKBN                       PIC X(01).
           03  SEL-SRYKBN                          PIC X(02).
           03  SEL-SRYSYUKBN                       PIC X(03).
           03  SEL-STSRYYMD                        PIC X(08).
           03  SEL-EDSRYYMD                        PIC X(08).
           03  SEL-SRYCD                           PIC X(09).
           03  SEL-DATAKBN                         PIC X(01).
           03  SEL-RRKSRH                          PIC 9(01).
           03  SEL-SRYSRH                          PIC 9(01).
           03  SEL-UPYMD                           PIC X(08).
           03  SEL-UPHMS                           PIC X(06).
      *
       01  TSRYCD-G.
           03  TSRYCD-SRYCD                        PIC X(09)
                                                   OCCURS 5.
      *
       01  WRK-AREA.
           03  WRK-DELYMD                          PIC X(08).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    診療行為照会テーブル
       01  SRYSRH-REC.
           COPY    "CPSRYSRH.INC".
      *
      *    患者情報照会テーブル
       01  PTSRH-REC.
           COPY    "CPPTSRH.INC".
      *
      *    受診履歴照会テーブル
       01  RRKSRH-REC.
           COPY    "CPRRKSRH.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      *
      ******************************************************************
       PROCEDURE                       DIVISION
                                       USING     COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA
                                       FLG-AREA
                                       IDX-AREA
                                       WRK-AREA
                                       SPA-AREA
      *
           PERFORM 800-MCNDATE-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    SPA-TERMID
                                               SPA-HOSPNUM
                                               PRFNAME
                                               SEL-AREA
                                               TSRYCD-G
           END-UNSTRING
      *
      *    ＤＢオープン
           PERFORM 120-DBOPEN-SEC
      *
      *    パラメータファイルオープン
           OPEN    INPUT               BQ01-FILE
      *
      *    パラメータファイル読み込み
           READ    BQ01-FILE
      *
           MOVE    BQ01                TO  WKBQ01
      *
           CLOSE   BQ01-FILE
      *
           PERFORM 900-PTSRH-DEL2-SEC
           PERFORM 910-DBCOMMIT-SEC
           PERFORM 910-DBSTART-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                  RRKSRH-REC
           MOVE    SPA-TERMID      TO  RRKSRH-TERMID
           MOVE    RRKSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_rrksrh"    TO  MCP-TABLE
           MOVE    "crtv01"        TO  MCP-PATHNAME
           MOVE    "DBCREATE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF   ( SEL-RRKSRH       =   1 )
               PERFORM 900-RRKSRH-INS1-SEC
               PERFORM 910-DBCOMMIT-SEC
               PERFORM 910-DBSTART-SEC
           END-IF
      *
           PERFORM 900-SRYSRH-DEL1-SEC
      *
           EVALUATE    SEL-SRYSRH
           WHEN    1
               PERFORM 900-SRYSRH-INS1-SEC
           WHEN    2
               PERFORM 900-SRYSRH-INS2-SEC
           WHEN    3
               PERFORM 900-SRYSRH-INS3-SEC
           WHEN    4
               PERFORM 900-SRYSRH-INS4-SEC
           END-EVALUATE
      *
           PERFORM 910-DBCOMMIT-SEC
           PERFORM 910-DBSTART-SEC
      *
           PERFORM 900-PTSRH-INS1-SEC
           PERFORM 910-DBCOMMIT-SEC
           PERFORM 910-DBSTART-SEC
      *
           PERFORM 900-RRKSRH-DEL1-SEC
           PERFORM 910-DBCOMMIT-SEC
           PERFORM 910-DBSTART-SEC
      *
           PERFORM 900-SRYSRH-DEL1-SEC
           PERFORM 910-DBCOMMIT-SEC
           PERFORM 910-DBSTART-SEC
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  PTSRH-TERMID
           MOVE    "0"             TO  PTSRH-DATAKBN
           MOVE    SEL-UPYMD       TO  PTSRH-CREYMD
                                       PTSRH-UPYMD
           MOVE    SEL-UPHMS       TO  PTSRH-UPHMS
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           INITIALIZE                  RRKSRH-REC
           MOVE    RRKSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_rrksrh"    TO  MCP-TABLE
           MOVE    "drtv01"        TO  MCP-PATHNAME
           MOVE    "DBDROP"        TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴照会追加処理
      *****************************************************************
       900-RRKSRH-INS1-SEC             SECTION.
      *
           PERFORM 900-RRKSRH-DEL1-SEC
      *
           INITIALIZE                  RRKSRH-REC
           MOVE    SPA-HOSPNUM     TO  RRKSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  RRKSRH-TERMID
           MOVE    SEL-NYUGAIKBN   TO  RRKSRH-NYUGAIKBN
           MOVE    SEL-SRYKA       TO  RRKSRH-SRYKA
           MOVE    SEL-STSRYYMD    TO  RRKSRH-STSRYYMD
           MOVE    SEL-EDSRYYMD    TO  RRKSRH-EDSRYYMD
           MOVE    SMCNDATE-YMD    TO  RRKSRH-CREYMD
                                       RRKSRH-UPYMD
           MOVE    SMCNDATE-HMS    TO  RRKSRH-UPHMS
           MOVE    RRKSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_rrksrh"    TO  MCP-TABLE
           MOVE    "ins1"          TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           .
       900-RRKSRH-INS1-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴照会削除処理
      *****************************************************************
       900-RRKSRH-DEL1-SEC             SECTION.
      *
           INITIALIZE                  RRKSRH-REC
           MOVE    SPA-HOSPNUM     TO  RRKSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  RRKSRH-TERMID
           MOVE    RRKSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_rrksrh"    TO  MCP-TABLE
           MOVE    "del1"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       900-RRKSRH-DEL1-EXT.
           EXIT.
      *****************************************************************
      *    診療行為照会追加処理
      *****************************************************************
       900-SRYSRH-INS1-SEC             SECTION.
      *
           PERFORM 9001-SRYSRH-INS-SEC
           MOVE    SRYSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_srysrh"    TO  MCP-TABLE
           MOVE    "ins1"          TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           .
       900-SRYSRH-INS1-EXT.
           EXIT.
      *****************************************************************
      *    診療行為照会追加処理
      *****************************************************************
       900-SRYSRH-INS2-SEC             SECTION.
      *
           PERFORM 9001-SRYSRH-INS-SEC
           MOVE    SRYSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_srysrh"    TO  MCP-TABLE
           MOVE    "ins2"          TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           .
       900-SRYSRH-INS2-EXT.
           EXIT.
      *****************************************************************
      *    診療行為照会追加処理
      *****************************************************************
       900-SRYSRH-INS3-SEC             SECTION.
      *
           PERFORM 9001-SRYSRH-INS-SEC
           MOVE    SRYSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_srysrh"    TO  MCP-TABLE
           MOVE    "ins3"          TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           .
       900-SRYSRH-INS3-EXT.
           EXIT.
      *****************************************************************
      *    診療行為照会追加処理
      *****************************************************************
       900-SRYSRH-INS4-SEC             SECTION.
      *
           PERFORM 9001-SRYSRH-INS-SEC
           MOVE    SRYSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_srysrh"    TO  MCP-TABLE
           MOVE    "ins4"          TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           .
       900-SRYSRH-INS4-EXT.
           EXIT.
      *****************************************************************
      *    診療行為照会追加処理
      *****************************************************************
       9001-SRYSRH-INS-SEC             SECTION.
      *
           INITIALIZE                  SRYSRH-REC
           MOVE    SPA-HOSPNUM     TO  SRYSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  SRYSRH-TERMID
           MOVE    SEL-NYUGAIKBN   TO  SRYSRH-NYUGAIKBN
           MOVE    SEL-SRYKA       TO  SRYSRH-SRYKA
           MOVE    SEL-SRYKBN      TO  SRYSRH-SRYKBN
           MOVE    SEL-SRYSYUKBN   TO  SRYSRH-SRYSYUKBN
           MOVE    SEL-STSRYYMD    TO  SRYSRH-STSRYYMD
           MOVE    SEL-EDSRYYMD    TO  SRYSRH-EDSRYYMD
           MOVE    SEL-STSRYYMD(1:6)
                                   TO  SRYSRH-STSRYYM
           MOVE    SEL-EDSRYYMD(1:6)
                                   TO  SRYSRH-EDSRYYM
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   5 )
      *
               IF    ( TSRYCD-SRYCD (IDX0)
                                   =   SPACE )
                   MOVE    SEL-SRYCD
                                   TO  SRYSRH-SRYCD (IDX0)
               ELSE
                   MOVE    TSRYCD-SRYCD (IDX0)
                                   TO  SRYSRH-SRYCD (IDX0)
               END-IF
           END-PERFORM
           MOVE    SMCNDATE-YMD    TO  SRYSRH-CREYMD
                                       SRYSRH-UPYMD
           MOVE    SMCNDATE-HMS    TO  SRYSRH-UPHMS
      *
           .
       9001-SRYSRH-INS-EXT.
           EXIT.
      *****************************************************************
      *    患者照会追加処理
      *****************************************************************
       900-PTSRH-INS1-SEC              SECTION.
      *
           PERFORM 900-PTSRH-DEL3-SEC
      *
           INITIALIZE                  PTSRH-REC
           MOVE    WKBQ01-SQL      TO  PTSRH-SQL
           DISPLAY "PTSRH-SQL=" PTSRH-SQL
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "ins1"          TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           .
       900-PTSRH-INS1-EXT.
           EXIT.
      *****************************************************************
      *    患者照会削除処理
      *****************************************************************
       900-PTSRH-DEL3-SEC              SECTION.
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  PTSRH-TERMID
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "del3"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       900-PTSRH-DEL3-EXT.
           EXIT.
      *****************************************************************
      *    患者照会削除処理
      *****************************************************************
       900-PTSRH-DEL2-SEC              SECTION.
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM         TO  PTSRH-HOSPNUM
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SEL-UPYMD           TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -3                  TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN TO  PTSRH-CREYMD
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "del2"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       900-PTSRH-DEL2-EXT.
           EXIT.
      *****************************************************************
      *    診療行為照会削除処理
      *****************************************************************
       900-SRYSRH-DEL1-SEC             SECTION.
      *
           INITIALIZE                  SRYSRH-REC
           MOVE    SPA-HOSPNUM     TO  SRYSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  SRYSRH-TERMID
           MOVE    SRYSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_srysrh"    TO  MCP-TABLE
           MOVE    "del1"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       900-SRYSRH-DEL1-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　接続処理
      *****************************************************************
       120-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       120-DBOPEN-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　切断処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
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
       910-DBCLOSECURSOR-SEC           SECTION.
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
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＣＯＭＭＩＴ処理
      *****************************************************************
       910-DBCOMMIT-SEC                SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCOMMIT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＳＴＡＲＴ処理
      *****************************************************************
       910-DBSTART-SEC                SECTION.
      *
           MOVE    "DBSTART"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBSTART-EXT.
           EXIT.
