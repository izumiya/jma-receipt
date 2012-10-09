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
       PROGRAM-ID.             ORCSMKPRT1.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : オンライン帳票印刷サブ
      *                    : (再印刷用DB書き込み追加)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/09/12    NACL-竹田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
           SELECT  PRT-IN-FILE     ASSIGN  HC01PARA
                                   FILE    STATUS  IS  STS-IN.
           SELECT  PRT-OUT-FILE    ASSIGN  PRTOUTPARA
                                   FILE    STATUS  IS  STS-OUT.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-IN-FILE.
       01  PRT-IN-REC.
           03  PRT-IN-PRTDATA      PIC X(20000).
       FD  PRT-OUT-FILE.
       01  PRT-OUT-REC.
           03  PRT-OUT-PRTDATA     PIC X(20000).
      *
       WORKING-STORAGE             SECTION.
       01  PRTOUTPARA.
           03  FILLER                  PIC  X(09) VALUE "/var/tmp/".
           03  PRTOUTPARA-FORM-ID      PIC  X(04).
           03  FILLER                  PIC  X(01) VALUE ".".
           03  PRTOUTPARA-UUID         PIC  X(31).
           03  FILLER                  PIC  X(01) VALUE ".".
           03  PRTOUTPARA-SYOHMS       PIC  9(08).
           03  FILLER                  PIC  X(01) VALUE ".".
           03  PRTOUTPARA-CNT          PIC  9(06).
           03  FILLER                  PIC  X(04) VALUE ".dat".
      *
       01  STS-AREA.
           03  STS-IN              PIC X(02).
           03  STS-OUT             PIC X(02).
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *     印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *
      *    シェル用領域
           COPY    "CPSHELLTBL.INC".
      *    COPY    "ORCA-DBPATH".
      *
       01  FLG-AREA.
           03  FLG-SYORI           PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-DIA             PIC X(10).
           03  WRK-SHORI-RENNUM    PIC 9(02).
           03  SGYOUMU-CODE        PIC 9(03).
           03  WRK-ENCODING        PIC X(01).
           03  WRK-ENCODING9       REDEFINES WRK-ENCODING
                                   PIC 9(01).
      *
      *    デイレクトリ位置
       01  SHELL-NAME.
           03  FILLER              PIC X(30)   VALUE
               "scripts/daily/".
      *   Ｄｉａ
       01  SHELL-DIA.
           03  FILLER              PIC X(40)   VALUE
               "scripts/daily/print-data.rb".
      *   ＭＯＮＰＥ
       01  SHELL-MONPE.
           03  FILLER              PIC X(10)   VALUE
               "red2ps".
      *    ＦＯＲＭ位置
       01  SHELL-FORM.
           03  FILLER              PIC X(10)   VALUE
               "form/".
      *    レコード位置
       01  SHELL-RECORD.
           03  FILLER              PIC X(10)   VALUE
               "record/".
      *
      *   ONLIEN REPRINT
       01  SHELL-ONLINE.
           03  FILLER              PIC X(40)   VALUE
               "scripts/daily/online-reprint.sh".
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    monpe印刷データ泣き別れ修正サブ
           COPY  "CPORCSFIXREDDATA.INC".
      *
      *    オフセット用
           COPY   "CPSK1044.INC".
      *    ディレクトリ位置編集サブ
           COPY   "CPMKPASS.INC".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
      *
       01  WRK-LNK-AREA.
           COPY    "CPORCSPRTLNK.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
           COPY  "CPORCSMKPRT.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSMKPRTAREA
           SPA-AREA.
      *
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  IDX-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
           CALL "ORCSGYOUMUGET"        USING
                                       SPA-AREA
                                       SGYOUMU-CODE
           DISPLAY "take gyomu_cd = " SGYOUMU-CODE
      *
           CALL    "ORCSENCODING"      USING
                                       MCPAREA
                                       SPA-AREA
           MOVE    SPA-ENCODING    TO  WRK-ENCODING9
      *
      *    帳票名からＤＩＡかＭＯＮＰＥを設定する
           MOVE    SPACE               TO  WRK-DIA
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   50
               IF      MKPRT-DIA(IDX:1)    =   "."
                   MOVE    1               TO  FLG-OK
               ELSE
                   IF     (MKPRT-DIA(IDX:1)    NOT =   SPACE) AND
                          (FLG-OK              =   1        )
                       ADD     1               TO  IDY
                       MOVE    MKPRT-DIA(IDX:1)    TO  WRK-DIA (IDY:)
                   END-IF
               END-IF
               IF      MKPRT-DIA(IDX:1)    =   SPACE
                   MOVE    50              TO  IDX
               END-IF
           END-PERFORM
           EVALUATE    WRK-DIA(1:3)
               WHEN    "dia"
      *            ｄａｉ
                   PERFORM 100-DIA-PRINT-SEC
               WHEN    "red"
      *            ＭＯＮＰＥ
                   PERFORM 200-MON-PRINT-SEC
               WHEN    OTHER
                   MOVE    99              TO  MKPRT-RC
           END-EVALUATE
      *
      *    再印刷書き出しの判定
           IF      SPA-PRT-FLG     =   SPACE
               CONTINUE
           ELSE
               PERFORM 300-REPRINT-SEC
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    編集処理
      *****************************************************************
       100-DIA-PRINT-SEC            SECTION.
      *
      *    フィアルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
      *    ＳＨＥＬＬ名
           STRING  SHELL-NAME          DELIMITED  BY  SPACE
                   MKPRT-ID            DELIMITED  BY  SPACE
                                       INTO    MKPASS-IN-01
           END-STRING
      *    帳票名
           MOVE    SHELL-DIA           TO  MKPASS-IN-02
      *    ＦＯＲＭ
           STRING  SHELL-FORM          DELIMITED  BY  SPACE
                   MKPRT-DIA           DELIMITED  BY  SPACE
                                       INTO    MKPASS-IN-03
           END-STRING
      *    DEF名
           STRING  SHELL-RECORD        DELIMITED  BY  SPACE
                   MKPRT-DEF           DELIMITED  BY  SPACE
                                       INTO    MKPASS-IN-04
           END-STRING
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
      *
      *    SHELL
           MOVE    SPACE               TO  SHELLTBL
           INITIALIZE                      SHELLTBL
      *
           MOVE    MKPASS-OT-01        TO  SHELLTBL-NAME
      *    DIA
           MOVE    MKPASS-OT-02        TO  SHELLTBL-ARG1
      *    FORM
           MOVE    MKPASS-OT-03        TO  SHELLTBL-ARG2
      *    DEF
           MOVE    MKPASS-OT-04        TO  SHELLTBL-ARG3
      *    データ内容
           MOVE    MKPRT-DAT           TO  SHELLTBL-ARG4
      *    印刷位置
           MOVE    MKPRT-PRTNM         TO  SHELLTBL-ARG5
      *
grpsys*    MOVE    SPA-HOSPNUM         TO  -HOSPNUM
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "shell"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       100-DIA-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    編集処理
      *****************************************************************
       200-MON-PRINT-SEC            SECTION.
      *
           IF    ( SPA-ENCODING     =  2 )
               MOVE    MKPRT-DAT            TO  HC01PARA
               OPEN    INPUT                    PRT-IN-FILE
               IF        ( STS-IN   NOT  =   ZERO )
                   DISPLAY "INPUT OPEN ERR [" HC01PARA "]"
               ELSE
                   INITIALIZE                   PRT-IN-REC
                   READ PRT-IN-FILE
                   INITIALIZE                   SFIXREDDATA-AREA
                   MOVE    "1"              TO  SFIXREDDATA-SITEKBN
                   MOVE    MKPRT-DIA        TO  SFIXREDDATA-FORMID
                   MOVE    PRT-IN-REC       TO  SFIXREDDATA-DATA
                   CALL    "ORCSFIXREDDATA"     USING
                                                SFIXREDDATA-AREA
                                                SPA-AREA
                   CLOSE   PRT-IN-FILE
                   OPEN    OUTPUT               PRT-IN-FILE
                   MOVE    SFIXREDDATA-DATA TO  PRT-IN-REC
                   WRITE   PRT-IN-REC
                   CLOSE   PRT-IN-FILE
               END-IF
           END-IF
      *
      *    フィアルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
      *    ＳＨＥＬＬ名
           STRING  SHELL-NAME          DELIMITED  BY  SPACE
                   MKPRT-ID            DELIMITED  BY  SPACE
                                       INTO    MKPASS-IN-01
           END-STRING
      *    ＦＯＲＭ
           STRING  SHELL-FORM          DELIMITED  BY  SPACE
                   MKPRT-DIA           DELIMITED  BY  SPACE
                                       INTO    MKPASS-IN-03
           END-STRING
      *    DEF名
           STRING  SHELL-RECORD        DELIMITED  BY  SPACE
                   MKPRT-DEF           DELIMITED  BY  SPACE
                                       INTO    MKPASS-IN-04
           END-STRING
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
      *
           PERFORM 201-OFFSET-GET-SEC
      *
      *    SHELL
           MOVE    SPACE               TO  SHELLTBL
           INITIALIZE                      SHELLTBL
      *
           MOVE    MKPRT-ID            TO  SHELLTBL-NAME
      *    MONP
           MOVE    SHELL-MONPE         TO  SHELLTBL-ARG1
      *    FROM
           MOVE    MKPASS-OT-03        TO  SHELLTBL-ARG2
      *    DEF
           MOVE    "DUMMY"             TO  SHELLTBL-ARG3
      *    データ内容
           MOVE    MKPRT-DAT           TO  SHELLTBL-ARG4
      *    印刷位置
           MOVE    MKPRT-PRTNM         TO  SHELLTBL-ARG5
      *    オフセット
           MOVE    SYS-1044-OFFSET-X   TO  SHELLTBL-ARG6
           MOVE    SYS-1044-OFFSET-Y   TO  SHELLTBL-ARG7
      *    エンコーディング
           MOVE    WRK-ENCODING        TO  SHELLTBL-ARG8
      *
grpsys*    MOVE    SPA-HOSPNUM         TO  -HOSPNUM
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
      *****MOVE    PATH-SHELL-SHELL    TO  MCP-PATH
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "daily"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       200-MON-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    オフセット取得処理
      *****************************************************************
       201-OFFSET-GET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SYS-1044-REC
           INITIALIZE                  SYS-1044-REC
           MOVE    "1044"              TO  SYS-1044-KANRICD
           MOVE    MKPRT-DIA           TO  SYS-1044-FORMID
           STRING  MKPRT-PRTNM         DELIMITED  BY  SPACE
                   "%"                 DELIMITED  BY  SIZE
                                       INTO    SYS-1044-TBL(21:)
           END-STRING
           MOVE    "99999999"          TO  SYS-1044-STYUKYMD
           MOVE    "99999999"          TO  SYS-1044-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1044-HOSPNUM
grpsys*    MOVE    SPA-HOSPNUM         TO  -HOSPNUM
           MOVE    SYS-1044-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE  MCPDATA-REC   TO  SYS-1044-REC
                   IF   (SYS-1044-FORMID  =   MKPRT-DIA  )  AND
                        (SYS-1044-PRTNM   =   MKPRT-PRTNM)
                         CONTINUE
                   ELSE
                         INITIALIZE    SYS-1044-REC
                   END-IF
               ELSE
                   INITIALIZE          SYS-1044-REC
               END-IF
           ELSE
               INITIALIZE              SYS-1044-REC
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       201-OFFSET-GET-EXT.
           EXIT.
      *****************************************************************
      *    編集処理
      *****************************************************************
       300-REPRINT-SEC                 SECTION.
      *
           MOVE    MKPRT-DAT           TO  HC01PARA
           OPEN    INPUT               PRT-IN-FILE
           IF        ( STS-IN     NOT  =   ZERO )
                   DISPLAY "INPUT OPEN ERR"
           ELSE
               INITIALIZE   PRT-IN-REC
               READ PRT-IN-FILE
               MOVE HC01PARA-FORM-ID   TO PRTOUTPARA-FORM-ID
               MOVE SPA-PRT-UUID       TO PRTOUTPARA-UUID
               MOVE HC01PARA-SYOHMS    TO PRTOUTPARA-SYOHMS
               MOVE HC01PARA-CNT       TO PRTOUTPARA-CNT
               OPEN OUTPUT             PRT-OUT-FILE
               MOVE PRT-IN-REC         TO PRT-OUT-REC
               WRITE    PRT-OUT-REC
               CLOSE PRT-IN-FILE PRT-OUT-FILE
           END-IF
      *
           INITIALIZE                  LNK-PRTKANRI-AREA
      *    ファイルデイレクトリ位置指定
           INITIALIZE              ORCSMKPASSAREA
           MOVE    SHELL-ONLINE    TO  MKPASS-IN-01
      *    ＦＯＲＭ
           STRING  SHELL-FORM      DELIMITED  BY  SPACE
                   MKPRT-DIA       DELIMITED  BY  SPACE
                                   INTO    MKPASS-IN-02
           END-STRING
           CALL    "ORCSMKPASS"    USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01    TO  SHELLTBL-NAME
           MOVE    1               TO  LNK-PRTKANRI-RENNUM
           MOVE    "ONLINE"        TO  LNK-PRTKANRI-TBL-KEY
           MOVE    SPA-PRT-GROUP
                                   TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    SPA-SYSYMD      TO  LNK-PRTKANRI-SKYYMD
                                       LNK-PRTKANRI-SRYYM
           MOVE    1               TO  LNK-PRTKANRI-PRIORITY
           MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
      *
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SHELLTBL-ARG1
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SHELLTBL-ARG2
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SHELLTBL-ARG3
           IF      SPA-PRT-FLG     IS  NUMERIC
               MOVE SPA-PRT-FLG    TO  WRK-SHORI-RENNUM
           ELSE
               MOVE ZERO           TO  WRK-SHORI-RENNUM
           END-IF
           MOVE    WRK-SHORI-RENNUM
                                   TO  LNK-PRTKANRI-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SHELLTBL-ARG4
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SHELLTBL-ARG5
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SHELLTBL-ARG6
      *    MOVE    LNK-PRTKANRI-SHELLID
           MOVE   "online-reprint.sh"
                                   TO  SHELLTBL-ARG7
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SHELLTBL-ARG8
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SHELLTBL-ARG9
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SHELLTBL-ARG10
           MOVE    MKPRT-PRTNM
                                   TO  SHELLTBL-ARG11
           MOVE    SPA-SRYYMD      TO  SHELLTBL-ARG12
           MOVE    SPA-PRT-UUID    TO  SHELLTBL-ARG13
           MOVE    SPA-PTID        TO  SHELLTBL-ARG14
      *    SITE_KBN
           MOVE    "1"             TO  SHELLTBL-ARG15
      *    
           MOVE    SGYOUMU-CODE    TO  SHELLTBL-ARG16
           MOVE    MKPRT-INFO      TO  SHELLTBL-ARG17
      *    DIA
           MOVE    MKPRT-ID        TO  SHELLTBL-ARG18
      *    FORM
           MOVE    MKPRT-DIA       TO  SHELLTBL-ARG19
      *    DEF
           MOVE    MKPRT-PRTNM     TO  SHELLTBL-ARG20
      *    データ内容
           MOVE    PRTOUTPARA      TO  SHELLTBL-ARG21
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG22

      *
grpsys*    MOVE    SPA-HOSPNUM         TO  -HOSPNUM
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "shell"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       300-REPRINT-EXT.
           EXIT.
