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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCDTCHK000.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データチェック
      *  コンポーネント名  : 各処理呼出
      *  管理者            :
      *  作成日付    作業者        記述
      *  05/03/31    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07/06/04  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
       01  DTCHK-RC                    PIC S9(09).
      *
      *    エラーファイルファイル名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //DTCHKERRPARA//.
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
      *
      *    データチェック一括処理パラメータ
           COPY    "CPORCBSD01PARA.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    データチェック診療会計
       01  DCACCT-REC.
           COPY    "CPDCACCT.INC".
      *
      *    データチェック診療行為
       01  DCSRY-REC.
           COPY    "CPDCSRY.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      ****************************************************************
       LINKAGE                         SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(2000).
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
           PERFORM 300-END-SEC
           DISPLAY "ORCDTCHK000 end"
      *
           EXIT PROGRAM
      *    STOP    RUN
      *
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  SPA-AREA
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
                                               LNK-PRTKANRI-TBL-KEY
                                               LNK-PRTKANRI-TBL-GROUP
                                               LNK-PRTKANRI-SHORI-RENNUM
                                               LNK-PRTKANRI-SRYYM
                                               LNK-PRTKANRI-SKYYMD
                                               LNK-PRTKANRI-SHELLID
                                               LNK-PRTKANRI-PRIORITY
                                               LNK-PRTKANRI-TERMID
                                               LNK-PRTKANRI-OPID
                                               LNK-PRTKANRI-PRTNM
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               DTCHKERRPARA
                                               ORCBSD01-IKTKBT
                                               ORCBSD01-SRYYM
                                               ORCBSD01-NYUGAIKBN
                                               ORCBSD01-CHKKBN-G
                                               ORCBSD01-SYAHOPROCKBN
                                               ORCBSD01-KOKUHOPROCKBN
                                               ORCBSD01-RSIPROCKBN
                                               ORCBSD01-JBIPROCKBN
                                               ORCBSD01-JIHIPROCKBN
                                               ORCBSD01-INGAIPROCKBN
                                               ORCBSD01-PRTSRTKBN
                                               ORCBSD01-PRTPTLSTKBN
                                               ORCBSD01-SRYYMDKBN
                                               ORCBSD01-STSRYYMD
                                               ORCBSD01-EDSRYYMD
                                               ORCBSD01-KGIPROCKBN
                                               ORCBSD01-KOUKIPROCKBN
                                               ORCBSD01-CHIKENPROCKBN
                                               ORCBSD01-DAISANPROCKBN
           END-UNSTRING
      *
           DISPLAY "ORCBSD01-IKTKBT = " ORCBSD01-IKTKBT
           DISPLAY "ORCBSD01-SRYYM  = " ORCBSD01-SRYYM
           DISPLAY "ORCBSD01-NYUGAIKBN = " ORCBSD01-NYUGAIKBN
           DISPLAY "ORCBSD01-CHKKBN-G  = " ORCBSD01-CHKKBN-G
           DISPLAY "ORCBSD01-SYAHOPROCKBN = " ORCBSD01-SYAHOPROCKBN
           DISPLAY "ORCBSD01-KOKUHOPROCKBN = " 
                                           ORCBSD01-KOKUHOPROCKBN
           DISPLAY "ORCBSD01-RSIPROCKBN = " ORCBSD01-RSIPROCKBN
           DISPLAY "ORCBSD01-JBIPROCKBN = " ORCBSD01-JBIPROCKBN
           DISPLAY "ORCBSD01-INGAIPROCKBN = "  ORCBSD01-INGAIPROCKBN
           DISPLAY "ORCBSD01-PRTSRTKBN = " ORCBSD01-PRTSRTKBN
           DISPLAY "ORCBSD01-PRTPTLSTKBN = " ORCBSD01-PRTPTLSTKBN 
           DISPLAY "ORCBSD01-SRYYMDKBN = " ORCBSD01-SRYYMDKBN
           DISPLAY "ORCBSD01-STSRYYMD = " ORCBSD01-STSRYYMD
           DISPLAY "ORCBSD01-EDSRYYMD = " ORCBSD01-EDSRYYMD
           DISPLAY "ORCBSD01-KGIPROCKBN = " ORCBSD01-KGIPROCKBN
           DISPLAY "ORCBSD01-CHIKENPROCKBN = " ORCBSD01-CHIKENPROCKBN
           DISPLAY "ORCBSD01-DAISANPROCKBN = " ORCBSD01-DAISANPROCKBN
           DISPLAY " " 
           DISPLAY " " 
      *
           DISPLAY "WRK-PARA-HOSPNUM = " WRK-PARA-HOSPNUM
           DISPLAY "WRK-PARA-JOBID   = " WRK-PARA-JOBID
           DISPLAY "WRK-PARA-SHELLID = " WRK-PARA-SHELLID
      *    IF      LNK-PRTKANRI-TBL-KEY  =  "RECEPTX"
      *    MOVE    "RECEPTX"           TO  WRK-PARA-SHELLID
      *    END-IF
           DISPLAY "LNK-PRTKANRI-RENNUM       = " LNK-PRTKANRI-RENNUM
           DISPLAY "LNK-PRTKANRI-TBL-KEY      = " LNK-PRTKANRI-TBL-KEY
           DISPLAY "LNK-PRTKANRI-TBL-GROUP    = " LNK-PRTKANRI-TBL-GROUP
           DISPLAY "LNK-PRTKANRI-SHORI-RENNUM = " 
                                              LNK-PRTKANRI-SHORI-RENNUM
           DISPLAY "LNK-PRTKANRI-SRYYM        = " LNK-PRTKANRI-SRYYM
           DISPLAY "LNK-PRTKANRI-SKYYMD       = " LNK-PRTKANRI-SKYYMD
           DISPLAY "LNK-PRTKANRI-SHELLID      = " LNK-PRTKANRI-SHELLID
           DISPLAY "LNK-PRTKANRI-PRIORITY     = " LNK-PRTKANRI-PRIORITY
           DISPLAY "LNK-PRTKANRI-TERMID       = " LNK-PRTKANRI-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      * take for debug start
      *    MOVE    01                  TO  SPA-HOSPNUM
      *    MOVE    "01"                TO  WRK-PARA-HOSPNUM
      *    MOVE    "2"                 TO  ORCBSD01-IKTKBT
      *    MOVE    "200903"            TO  ORCBSD01-SRYYM
      *    MOVE    "2"                 TO  ORCBSD01-NYUGAIKBN
      *    MOVE    "222222222222222222"
      *                                TO  ORCBSD01-CHKKBN-G
      *    MOVE    "1"                 TO  ORCBSD01-SYAHOPROCKBN
      *    MOVE    "1"                 TO  ORCBSD01-KOKUHOPROCKBN
      *    MOVE    "1"                 TO  ORCBSD01-RSIPROCKBN
      *    MOVE    "0"                 TO  ORCBSD01-JBIPROCKBN
      *    MOVE    "2"                 TO  ORCBSD01-PRTSRTKBN
      *    MOVE    "2"                 TO  ORCBSD01-PRTPTLSTKBN
      *    MOVE    "1"                 TO  ORCBSD01-KGIPROCKBN
      *    MOVE    "1"                 TO  ORCBSD01-KOUKIPROCKBN
      * take for debug end
      *
           IF      LNK-PRTKANRI-TBL-KEY  NOT = "RECEPTX"
                   PERFORM 100-DBOPEN-SEC
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    ZERO                TO  DTCHK-RC
      *
           IF    ( DTCHK-RC            =   ZERO )
               IF    ( ORCBSD01-IKTKBT     =   "2" )
                OR   ( ORCBSD01-SRYYMDKBN  =   "1" )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCDTCHK009"
                   CALL    "ORCDTCHK009"       USING
                                               DTCHK-RC
                                               WRK-PARA
                                               DTCHKERRPARA
                                               ORCBSD01PARA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCDTCHK009"
               END-IF
           END-IF
      *
           IF    ( DTCHK-RC            =   ZERO )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCDTCHK004"
                   CALL    "ORCDTCHK004"       USING
                                               DTCHK-RC
                                               WRK-PARA
                                               DTCHKERRPARA
                                               ORCBSD01PARA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCDTCHK004"
           END-IF
      *
           IF    ( DTCHK-RC            =   ZERO )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCDTCHK005"
                   CALL    "ORCDTCHK005"       USING
                                               DTCHK-RC
                                               WRK-PARA
                                               DTCHKERRPARA
                                               ORCBSD01PARA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCDTCHK005"
           END-IF
      *
           IF    ( DTCHK-RC            =   ZERO )
      *        入院会計は現状入院時算定可能な項目のみ
      *        登録している為、入院処理時に限り実行する。
               IF    ( ORCBSD01-NYUGAIKBN  =       "1" )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCDTCHK006"
                   CALL    "ORCDTCHK006"       USING
                                               DTCHK-RC
                                               WRK-PARA
                                               DTCHKERRPARA
                                               ORCBSD01PARA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCDTCHK006"
               END-IF
           END-IF
      *
           IF    ( DTCHK-RC            =   ZERO )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCDTCHK001"
                   CALL    "ORCDTCHK001"       USING
                                               DTCHK-RC
                                               WRK-PARA
                                               DTCHKERRPARA
                                               ORCBSD01PARA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCDTCHK001"
           END-IF
      *
           IF    ( DTCHK-RC            =   ZERO )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCDTCHK011"
                   CALL    "ORCDTCHK011"       USING
                                               DTCHK-RC
                                               WRK-PARA
                                               DTCHKERRPARA
                                               ORCBSD01PARA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCDTCHK011"
           END-IF
      *
           IF    ( DTCHK-RC            =   ZERO )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCDTCHK002"
                   CALL    "ORCDTCHK002"       USING
                                               DTCHK-RC
                                               WRK-PARA
                                               DTCHKERRPARA
                                               ORCBSD01PARA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCDTCHK002"
           END-IF
      *
           IF    ( DTCHK-RC            =   ZERO )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCDTCHK010"
                   CALL    "ORCDTCHK010"       USING
                                               DTCHK-RC
                                               WRK-PARA
                                               DTCHKERRPARA
                                               ORCBSD01PARA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCDTCHK001"
           END-IF
      *
           IF    ( DTCHK-RC            =   ZERO )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCDTCHK008"
                   CALL    "ORCDTCHK008"       USING
                                               DTCHK-RC
                                               WRK-PARA
                                               DTCHKERRPARA
                                               ORCBSD01PARA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCDTCHK008"
           END-IF
      *
           IF      LNK-PRTKANRI-TBL-KEY  = "RECEPTX"
             PERFORM 900-DBDISCONNECT-SEC
             PERFORM 100-DBOPEN-SEC
             PERFORM 100-DBSTART-SEC
           END-IF
           IF    ( DTCHK-RC            =   ZERO )
               IF  ( ORCBSD01-PRTPTLSTKBN  =   "2" )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCDTCHK007"
                   CALL    "ORCDTCHK007"       USING
                                               DTCHK-RC
                                               WRK-PARA
                                               DTCHKERRPARA
                                               ORCBSD01PARA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCDTCHK007"
               END-IF
           END-IF
      *
           IF    ( DTCHK-RC            =   ZERO )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCDTCHK003"
                   CALL    "ORCDTCHK003"       USING
                                               DTCHK-RC
                                               WRK-PARA
                                               DTCHKERRPARA
                                               ORCBSD01PARA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCDTCHK003"
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           IF      LNK-PRTKANRI-TBL-KEY  NOT = "RECEPTX"
           PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *     
           .
       300-END-EXT.
           EXIT.
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
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　スタート処理
      *****************************************************************
       100-DBSTART-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBSTART-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
