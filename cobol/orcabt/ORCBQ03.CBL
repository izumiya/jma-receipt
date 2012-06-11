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
      *    診療行為数値検索チェックファイル
           SELECT  TMP01-FILE      ASSIGN  TMP01PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TMP01-KEY
                                   FILE    STATUS  IS  STS-TMP01.
      *
      *    同一剤チェックファイル
           SELECT  TMP02-FILE      ASSIGN  TMP02PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TMP02-KEY
                                   FILE    STATUS  IS  STS-TMP02.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    パラメータファイル
       FD  BQ01-FILE.
           COPY    "CPBQ01.INC".
      *
       FD  TMP01-FILE.
       01  TMP01-REC.
           03  TMP01-KEY.
               05  TMP01-HOSPNUM               PIC 9(02).
               05  TMP01-PTID                  PIC 9(10).
           03  TMP01-PTNUM                     PIC X(20).
           03  TMP01-KANANAME                  PIC X(100).
           03  TMP01-SEX                       PIC X(1).
           03  TMP01-BIRTHDAY                  PIC X(8).
           03  TMP01-SRYCD-G.
               05  TMP01-SRYCD-OCC OCCURS 5.
                   07  TMP01-SRYCD             PIC X(09).
                   07  TMP01-SRYSURYO1         PIC 9(05)V9(05).
                   07  TMP01-SRYSURYO2         PIC 9(05)V9(05).
                   07  TMP01-ZAIKAISU          PIC 9(07).
                   07  TMP01-SURYOTOTAL        PIC 9(07)V9(05).
                   07  TMP01-MON-G.
                       09  TMP01-MON-OCC               OCCURS 12.
                           11  TMP01-MON-SRYYM         PIC X(06).
                           11  TMP01-MON-SRYSURYO1     PIC 9(05)V9(05).
                           11  TMP01-MON-SRYSURYO2     PIC 9(05)V9(05).
                           11  TMP01-MON-ZAIKAISU      PIC 9(07).
                           11  TMP01-MON-SURYOTOTAL    PIC 9(07)V9(05).
                           11  TMP01-MON-DAY-G.
                               13  TMP01-MON-DAY       PIC 9(05)
                                                       OCCURS  31.
      *
       FD  TMP02-FILE.
       01  TMP02-REC.
           03  TMP02-KEY.
               05  TMP02-HOSPNUM               PIC 9(02).
               05  TMP02-PTID                  PIC 9(10).
               05  TMP02-ZAINUM                PIC 9(09).
               05  TMP02-SRYCD                 PIC X(09).
               05  TMP02-RENNUM                PIC 9(02).
               05  TMP02-SRYCD-IDX             PIC 9(02).
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
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //TMP01PARA//.
      *
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //TMP02PARA//.
      *
      *    画面SPA(KYOUTU領域を使用)
           COPY    "Q01SCR-SPA".
      *
       01  ASSIGN-NAME-AREA.
           03  PRFNAME                 PIC X(100).
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-BQ01                PIC X(02).
           03  STS-TMP01               PIC X(02).
           03  STS-TMP02               PIC X(02).
      *
      *    インデックス領域
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDX2                    PIC 9(05).
           03  IDXM                    PIC 9(05).
           03  IDX-SRY                 PIC 9(05).
      *
           03  IDXD                    PIC 9(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYUNOU              PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-SRYSRH              PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-LOOPEND             PIC 9(01).
           03  FLG-END                 PIC 9(01).
           03  FLG-TMP01               PIC 9(01).
           03  FLG-TMP02               PIC 9(01).
           03  FLG-TARGET              PIC 9(01).
           03  FLG-TARGET-SRYCD-G.
               05  FLG-TARGET-SRYCD    PIC 9(01)
                                       OCCURS  5.
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
           03  SEL-SKYSRH                          PIC 9(01).
           03  SEL-UPYMD                           PIC X(08).
           03  SEL-UPHMS                           PIC X(06).
           03  SEL-TENKENRECE                      PIC X(01).
           03  SEL-SRYYM                           PIC X(06)
                                                   OCCURS 12.
      *
       01  TSRYCD-G.
           03  TSRYCD-IDX                          PIC 9(05).
           03  TSRYCD-OCC                          OCCURS 5.
               05  TSRYCD-SRYVAL                   PIC X(20).
               05  TSRYCD-SRYVALNUM                PIC 9(05)V9(05).
               05  TSRYCD-SRYVALCOMP               PIC X(01).
               05  TSRYCD-KANANAME                 PIC X(100).
               05  TSRYCD-NAME                     PIC X(100).
               05  TSRYCD-SRYCD                    PIC X(09).
               05  TSRYCD-SURYO-EDIT               PIC 9(01).
      *
       01  WRK-AREA.
           03  WRK-YMD                             PIC X(10). 
           03  WRK-ZOGENPTN                        PIC X(01).
           03  WRK-ZOGEN                           PIC S9(05).
           03  WRK-KEISANYMD                       PIC X(08).
           03  WRK-DELYMD                          PIC X(08).
           03  WRK-ACCT-G.
               05  WRK-ZAIKAISU                    PIC 9(05).
               05  WRK-SURYOTOTAL                  PIC 9(07)V9(05).
               05  WRK-DAY-G.
                   07  WRK-DAY                     PIC 9(03)
                                                   OCCURS 31.
           03  WRK-SRYVAL                          PIC 9(07)V9(05).
           03  WRK-SRYYMD.
               05  WRK-SRYYM                       PIC X(06).
               05  WRK-SRYDD                       PIC 9(02).
      *
       01  COSNT-AREA.
           03  CONST-VALSURYO                      PIC X(01) VALUE "1".
           03  CONST-VALKAISU                      PIC X(01) VALUE "2".
           03  CONST-VALTOTAL                      PIC X(01) VALUE "3".
           03  CONST-SRYACCT                       PIC X(01) VALUE "1".
           03  CONST-NYUINACCT                     PIC X(01) VALUE "2".
           03  CONST-EQUAL                         PIC X(01) VALUE "1".
           03  CONST-GREATER-EQUAL                 PIC X(01) VALUE "2".
           03  CONST-LESS-EQUAL                    PIC X(01) VALUE "3".
           03  CONST-AND                           PIC X(01) VALUE "1".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    請求管理照会テーブル
       01  SKYSRH-REC.
           COPY    "CPSKYSRH.INC".
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
      *    点数マスタ
           COPY    "CPTENSU.INC".
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
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
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
                                       SEL-AREA
                                       TSRYCD-G
                                       SPA-AREA
                                       SPA-Q01FREE
      *
           PERFORM 800-MCNDATE-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    SPA-TERMID
                                               SPA-HOSPNUM
                                               PRFNAME
                                               SEL-UPYMD
                                               SEL-UPHMS
           END-UNSTRING
      *
      *    ＤＢオープン
           PERFORM 120-DBOPEN-SEC
      *
           MOVE    SPA-HOSPNUM         TO  TMP01PARA-HOSPNUM
           MOVE    "BQ03TMP1"          TO  TMP01PARA-FILE-ID
           MOVE    SPA-TERMID          TO  TMP01PARA-TERMID
      *
           MOVE    SPA-HOSPNUM         TO  TMP02PARA-HOSPNUM
           MOVE    "BQ03TMP2"          TO  TMP02PARA-FILE-ID
           MOVE    SPA-TERMID          TO  TMP02PARA-TERMID
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
           MOVE    WKBQ01-Q01KYOUTU    TO  SPA-Q01KYOUTU
      *
           PERFORM 1001-INIT-SEC
      *
           PERFORM 900-PTSRH-DEL2-SEC
           PERFORM 910-DBCOMMIT-SEC
           PERFORM 910-DBSTART-SEC
      *
           DISPLAY "SQL[" WKBQ01-SQL "]"
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       1001-INIT-SEC               SECTION.
      *
           IF    ( SPA-Q01-STSRYYMD    =   SPACE )
               MOVE    "00000000"          TO  SEL-STSRYYMD
           ELSE
               MOVE    SPA-Q01-STSRYYMD    TO  SEL-STSRYYMD
               PERFORM VARYING     IDX0    FROM    1   BY  1
                       UNTIL (     IDX0    >   12 )
                   MOVE    SEL-STSRYYMD TO WRK-YMD
                   COMPUTE WRK-ZOGEN   =   IDX0    -   1
                   MOVE    "2"         TO  WRK-ZOGENPTN
                   PERFORM 800-CALENDAR-SEC
                   MOVE    WRK-KEISANYMD(1:6)
                                           TO  SEL-SRYYM (IDX0)
               END-PERFORM
           END-IF
      *
           IF    ( SPA-Q01-EDSRYYMD    =   SPACE )
               MOVE    "99999999"          TO  SEL-EDSRYYMD
           ELSE
               MOVE    SPA-Q01-EDSRYYMD    TO  SEL-EDSRYYMD
           END-IF
      *
           IF    ( SPA-Q01-SRYKA       =   SPACE )
               MOVE    "%"                 TO  SEL-SRYKA
           ELSE
               MOVE    SPA-Q01-SRYKA       TO  SEL-SRYKA
           END-IF
      *
           IF    ( SPA-Q01-NYUGAIKBN   =   SPACE )
               MOVE    "%"                 TO  SEL-NYUGAIKBN
           ELSE
               MOVE    SPA-Q01-NYUGAIKBN   TO  SEL-NYUGAIKBN
           END-IF
      *
           IF    ( SPA-Q01-SRYKBN       =   SPACE )
               MOVE    "%"                 TO  SEL-SRYKBN
           ELSE
               MOVE    SPA-Q01-SRYKBN(1:1)
                                           TO  SEL-SRYKBN(1:1)
               MOVE    "%"                 TO  SEL-SRYKBN(2:1)
           END-IF
      *
               MOVE    "%"                 TO  SEL-SRYSYUKBN
      *
           MOVE    ZERO                    TO  IDX1
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   5 )
      *
               IF    ( SPA-Q01-SRYCD (IDX0)
                               NOT =   SPACE )
                   MOVE    SPA-Q01-SRYCD (IDX0)
                                   TO  SEL-SRYCD
      *
                   COMPUTE TSRYCD-IDX  =   TSRYCD-IDX  +   1
                   MOVE    SPA-Q01-SRYVALNUM (IDX0)
                                   TO  TSRYCD-SRYVALNUM (TSRYCD-IDX)
                   MOVE    SPA-Q01-SRYVAL    (IDX0)
                                   TO  TSRYCD-SRYVAL    (TSRYCD-IDX)
                   MOVE    SPA-Q01-SRYVALCOMP(IDX0)
                                   TO  TSRYCD-SRYVALCOMP(TSRYCD-IDX)
                   MOVE    SPA-Q01-SRYCD (IDX0)
                                   TO  TSRYCD-SRYCD     (TSRYCD-IDX)
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( TNS-NAME    NOT =   SPACE )
                       MOVE    TNS-NAME
                                   TO  TSRYCD-NAME      (TSRYCD-IDX)
                   ELSE
                       MOVE    TSRYCD-SRYCD (TSRYCD-IDX)
                                   TO  TSRYCD-NAME      (TSRYCD-IDX)
                   END-IF
                   MOVE    TNS-KANANAME
                                   TO  TSRYCD-KANANAME  (TSRYCD-IDX)
                   MOVE    1       TO  TSRYCD-SURYO-EDIT(TSRYCD-IDX)
                   IF     (TNS-TANICD  =   ZERO
                                       OR  SPACE )
                       IF    ( TNS-SRYCD (1:1)   =   "6" OR  "7" )
                        AND  ( TNS-TEN           >   ZERO )
                        AND  ( TNS-TENSIKIBETU   =   1 OR 2 )
                           CONTINUE
                       ELSE
                           MOVE    ZERO    TO  TSRYCD-SURYO-EDIT
                                                        (TSRYCD-IDX)
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
      *    診察年月日
           IF   ( SPA-Q01-GTBL (8)  =   "1" )
               MOVE    1           TO  SEL-RRKSRH
           END-IF
      *
      *    診療行為
           IF   ( SPA-Q01-GTBL (9)  =   "1" )
               IF    ( SPA-Q01-GTBL(8) =   SPACE )
                   IF    ( SPA-Q01-SRYCD-G NOT =   SPACE )
                       MOVE    1    TO  SEL-SRYSRH
                   ELSE
                       MOVE    3    TO  SEL-SRYSRH
                   END-IF
               ELSE
                   IF    ( SPA-Q01-SRYCD-G NOT =   SPACE )
                       MOVE    2    TO  SEL-SRYSRH
                   ELSE
                       MOVE    4    TO  SEL-SRYSRH
                   END-IF
               END-IF
           END-IF
      *
      *    診察年月日
           IF   ( SPA-Q01-GTBL (8)  =   "1" )
               MOVE    1           TO  SEL-RRKSRH
           END-IF
      *
           IF    ( SPA-Q01-GTBL(12) =   "1" )
               MOVE    1           TO  SEL-SKYSRH
           END-IF
      *
           IF    ( SPA-Q01-TENKENRECE   =   SPACE )
               MOVE    "%"                 TO  SEL-TENKENRECE
           ELSE
               MOVE    SPA-Q01-TENKENRECE  TO  SEL-TENKENRECE
           END-IF
      *
           .
       1001-INIT-EXT.
           EXIT.
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
           IF   ( SEL-SKYSRH       =   1 )
               PERFORM 900-SKYSRH-DEL1-SEC
               IF   ( SPA-Q01-TOTALTEN >   ZERO )
                   PERFORM 2001-SYUNOU-GET-SEC
               END-IF
               PERFORM 900-SKYSRH-INS1-SEC
               PERFORM 910-DBCOMMIT-SEC
               PERFORM 910-DBSTART-SEC
           END-IF
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
           IF    ( SPA-Q01-SRYVALITEM  NOT =   SPACE )
               PERFORM 2001-SRYVAL-EDIT-SEC
               PERFORM 2001-SRYVAL-PTSRH-EDIT-SEC
      *
               PERFORM 910-DBCOMMIT-SEC
               PERFORM 910-DBSTART-SEC
           ELSE
               PERFORM 900-PTSRH-INS1-SEC
               PERFORM 910-DBCOMMIT-SEC
               PERFORM 910-DBSTART-SEC
           END-IF
      *
      *
           PERFORM 900-SKYSRH-DEL1-SEC
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
      *    診療行為数値検索処理
      *****************************************************************
       2001-SRYVAL-EDIT-SEC            SECTION.
      *
           OPEN    OUTPUT  TMP01-FILE  TMP02-FILE
           CLOSE           TMP01-FILE  TMP02-FILE
      *
           OPEN    I-O     TMP01-FILE  TMP02-FILE
      *
           PERFORM 900-SRYSRH-KEY-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SRYSRH  NOT =   ZERO )
      *
               PERFORM VARYING IDX-SRY FROM    1   BY  1
                       UNTIL ( IDX-SRY >   5 )
                        OR   ( SRYSRH-SRYCD (IDX-SRY)   =   SPACE )
      *
                   PERFORM 20011-SRYVAL-EDIT-SEC
      *
               END-PERFORM
      *
               PERFORM 900-SRYSRH-KEY-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_srysrh"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   TMP01-FILE
                   TMP02-FILE
      *
           .
       2001-SRYVAL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為数値検索処理
      *****************************************************************
       20011-SRYVAL-EDIT-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TARGET
      *
           MOVE    ZERO                TO  IDXM
      *
           PERFORM VARYING TSRYCD-IDX  FROM    1   BY  1
                   UNTIL ( TSRYCD-IDX  >   5 )
                    OR   ( TSRYCD-SRYCD (TSRYCD-IDX)
                                       =   SPACE )
      *
               IF    ( SRYSRH-SRYCD (IDX-SRY)
                                       =    TSRYCD-SRYCD (TSRYCD-IDX))
      *
                   MOVE    1           TO   FLG-TARGET
      *
                   IF    ( SPA-Q01-SRYVALITEM
                                       =   CONST-VALSURYO )
      *
                       IF    ( TSRYCD-SRYVAL (TSRYCD-IDX)
                                   NOT =   SPACE )
                           EVALUATE    TSRYCD-SRYVALCOMP (TSRYCD-IDX)
                           WHEN    CONST-GREATER-EQUAL
                               IF    ( SRYSRH-SRYSURYO (IDX-SRY)
                                               >=  TSRYCD-SRYVALNUM
                                                           (TSRYCD-IDX))
                                   CONTINUE
                               ELSE
                                   MOVE    ZERO    TO  FLG-TARGET
                               END-IF
                           WHEN    CONST-LESS-EQUAL
                               IF    ( SRYSRH-SRYSURYO (IDX-SRY)
                                               <=  TSRYCD-SRYVALNUM
                                                           (TSRYCD-IDX))
                                   CONTINUE
                               ELSE
                                   MOVE    ZERO    TO  FLG-TARGET
                               END-IF
                           WHEN    OTHER
                               IF    ( SRYSRH-SRYSURYO (IDX-SRY)
                                                =  TSRYCD-SRYVALNUM
                                                           (TSRYCD-IDX))
                                   CONTINUE
                               ELSE
                                   MOVE    ZERO    TO  FLG-TARGET
                               END-IF
                           END-EVALUATE
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   12 )
                    OR   ( IDXM    >   ZERO  )
      *
               IF    ( SRYSRH-SRYYMD (1:6) =   SEL-SRYYM (IDX2))
                   MOVE    IDX2    TO  IDXM
               END-IF
      *
           END-PERFORM
      *
           IF    ( IDXM        =   ZERO )
               MOVE    ZERO    TO  FLG-TARGET
           END-IF
      *
           IF    ( FLG-TARGET  =   1 )
               INITIALIZE              WRK-ACCT-G
               IF    ( SRYSRH-ACCTKBN  =   CONST-SRYACCT )
                   PERFORM 901-TMP02-READ-RND-SEC
                   IF    ( FLG-TMP02   =   ZERO )
                       PERFORM 900-TMP02-READ-RND-SEC
                       IF    ( FLG-TMP02   =   ZERO )
                           CONTINUE
                       ELSE
                           PERFORM 200111-SRYVAL-SUM-SEC
                           COMPUTE WRK-SURYOTOTAL
                               =   SRYSRH-SRYSURYO (IDX-SRY)
                               *   WRK-ZAIKAISU
                           MOVE    ZERO    TO  WRK-ZAIKAISU
                           INITIALIZE          WRK-DAY-G
      *
                           PERFORM 9001-TMP02-KEY-EDIT-SEC
                           WRITE   TMP02-REC
      *
                       END-IF
                   ELSE
      *
                       PERFORM 200111-SRYVAL-SUM-SEC
      *
                       COMPUTE WRK-SURYOTOTAL
                           =   SRYSRH-SRYSURYO (IDX-SRY)
                           *   WRK-ZAIKAISU
      *
                       PERFORM 9001-TMP02-KEY-EDIT-SEC
                       WRITE   TMP02-REC
                       PERFORM 9011-TMP02-KEY-EDIT-SEC
                       WRITE   TMP02-REC
                   END-IF
               ELSE
                   MOVE    SRYSRH-ZAIKAISU
                                       TO  WRK-ZAIKAISU
                   MOVE    SRYSRH-SRYYMD
                                       TO  WRK-SRYYMD
                   MOVE    SRYSRH-ZAIKAISU
                                       TO  WRK-DAY (WRK-SRYDD)
                   COMPUTE WRK-SURYOTOTAL
                       =   SRYSRH-SRYSURYO (IDX-SRY)
                       *   WRK-ZAIKAISU
               END-IF
      *
               PERFORM 900-TMP01-READ-RND-SEC
               IF    ( FLG-TMP01       =   ZERO )
                   CONTINUE
               ELSE
                   PERFORM 9001-TMP01-KEY-EDIT-SEC
                   MOVE    SRYSRH-PTNUM    TO  TMP01-PTNUM
                   MOVE    SRYSRH-KANANAME TO  TMP01-KANANAME
                   MOVE    SRYSRH-SEX      TO  TMP01-SEX
                   MOVE    SRYSRH-BIRTHDAY TO  TMP01-BIRTHDAY
               END-IF
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   5 )
                        OR   ( TSRYCD-SRYCD (IDX0)
                                       =   SPACE )
                   IF    ( TSRYCD-SRYCD (IDX0)
                                       =   SRYSRH-SRYCD (IDX-SRY))
                       MOVE    TSRYCD-SRYCD (IDX0)
                                       TO  TMP01-SRYCD (IDX0)
                       COMPUTE TMP01-ZAIKAISU(IDX0)
                           =   TMP01-ZAIKAISU(IDX0)
                           +   WRK-ZAIKAISU
                       COMPUTE TMP01-SURYOTOTAL(IDX0)
                           =   TMP01-SURYOTOTAL(IDX0)
                           +   WRK-SURYOTOTAL
      *
                       IF    ( TMP01-SRYSURYO1 (IDX0)
                                           =   ZERO )
                           MOVE    SRYSRH-SRYSURYO(IDX-SRY)
                                   TO  TMP01-SRYSURYO1(IDX0)
                                       TMP01-SRYSURYO2(IDX0)
                       ELSE
                           IF    ( TMP01-SRYSURYO2(IDX0)
                                       =   SRYSRH-SRYSURYO (IDX-SRY))
                               CONTINUE
                           ELSE
                               MOVE    SRYSRH-SRYSURYO(IDX-SRY)
                                   TO  TMP01-SRYSURYO1(IDX0)
                           END-IF
                       END-IF
      *
                       MOVE    SEL-SRYYM (IDXM)
                                       TO  TMP01-MON-SRYYM (IDX0 IDXM)
                       COMPUTE TMP01-MON-ZAIKAISU(IDX0 IDXM)
                           =   TMP01-MON-ZAIKAISU(IDX0 IDXM)
                           +   WRK-ZAIKAISU
                       COMPUTE TMP01-MON-SURYOTOTAL(IDX0 IDXM)
                           =   TMP01-MON-SURYOTOTAL(IDX0 IDXM)
                           +   WRK-SURYOTOTAL
      *
                       IF    ( TMP01-MON-SRYSURYO1 (IDX0 IDXM)
                                           =   ZERO )
                           MOVE    SRYSRH-SRYSURYO(IDX-SRY)
                                   TO  TMP01-MON-SRYSURYO1(IDX0 IDXM)
                                       TMP01-MON-SRYSURYO2(IDX0 IDXM)
                       ELSE
                           IF    ( TMP01-MON-SRYSURYO2(IDX0 IDXM)
                                       =   SRYSRH-SRYSURYO (IDX-SRY))
                               CONTINUE
                           ELSE
                               MOVE    SRYSRH-SRYSURYO(IDX-SRY)
                                   TO  TMP01-MON-SRYSURYO1(IDX0 IDXM)
                           END-IF
                       END-IF
                       PERFORM VARYING IDXD    FROM    1   BY  1
                               UNTIL ( IDXD    >   31 )
                           COMPUTE TMP01-MON-DAY (IDX0 IDXM IDXD)
                               =   TMP01-MON-DAY (IDX0 IDXM IDXD)
                               +   WRK-DAY (IDXD)
                       END-PERFORM
                   END-IF
               END-PERFORM
      *
               IF    ( FLG-TMP01       =   ZERO )
                   REWRITE TMP01-REC
               ELSE
                   WRITE   TMP01-REC
               END-IF
      *
           END-IF
      *
           .
       20011-SRYVAL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為数値集計処理
      *****************************************************************
       200111-SRYVAL-SUM-SEC           SECTION.
      *
           PERFORM 900-SRYACCT-KEY57-SEL-SEC
           MOVE    ACCT-SRYYM      TO  WRK-SRYYMD
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
               MOVE    IDXD        TO  WRK-SRYDD
               IF    ( WRK-SRYYMD  >=  SEL-STSRYYMD )
                AND  ( WRK-SRYYMD  <=  SEL-EDSRYYMD )
                   COMPUTE WRK-ZAIKAISU
                       =   WRK-ZAIKAISU
                       +   ACCT-DAY (1 IDXD)
                   MOVE    ACCT-DAY (1 IDXD)
                                   TO  WRK-DAY (IDXD)
               END-IF
           END-PERFORM
      *
           .
       200111-SRYVAL-SUM-EXT.
           EXIT.
      *****************************************************************
      *    診療行為数値検索より患者照会編集処理
      *****************************************************************
       2001-SRYVAL-PTSRH-EDIT-SEC  SECTION.
      *
           OPEN    INPUT   TMP01-FILE
      *
           PERFORM 900-TMP01-READ-SEQ-SEC
      *
           PERFORM UNTIL ( FLG-TMP01   NOT =   ZERO )
      *
               MOVE    1       TO  FLG-TARGET
               INITIALIZE          FLG-TARGET-SRYCD-G
      *
               PERFORM VARYING TSRYCD-IDX  FROM    1   BY  1
                       UNTIL ( TSRYCD-IDX  >   5 )
                        OR   ( TSRYCD-SRYCD (TSRYCD-IDX)
                                           =   SPACE )
      *
                   IF    ( TMP01-SRYCD (TSRYCD-IDX)
                                           =   SPACE )
                       MOVE    ZERO    TO  FLG-TARGET
                                           FLG-TARGET-SRYCD (TSRYCD-IDX)
                   ELSE
                       PERFORM 20011-SRYVAL-PTSRH-EDIT-SEC
                   END-IF
      *
               END-PERFORM
      *
               PERFORM VARYING TSRYCD-IDX  FROM    1   BY  1
                       UNTIL ( TSRYCD-IDX  >   5 )
                        OR   ( TSRYCD-SRYCD (TSRYCD-IDX)
                                           =   SPACE )
      *
                   IF    ( SPA-Q01-SRYACT-JKN  =   CONST-AND )
                       IF    ( FLG-TARGET  =   1 )
                           PERFORM 20011-SRYVAL-PTSRH-INS-SEC
                       END-IF
                   ELSE
                       IF    ( FLG-TARGET-SRYCD (TSRYCD-IDX)
                                           =   1 )
                           PERFORM 20011-SRYVAL-PTSRH-INS-SEC
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
               PERFORM 900-TMP01-READ-SEQ-SEC
      *
           END-PERFORM
      *
           CLOSE           TMP01-FILE
      *
           .
       2001-SRYVAL-PTSRH-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為数値検索より患者照会編集処理
      *****************************************************************
       20011-SRYVAL-PTSRH-EDIT-SEC SECTION.
      *
           MOVE    1   TO  FLG-TARGET-SRYCD (TSRYCD-IDX)
      *
           DISPLAY "BF FLG-TARGET = " FLG-TARGET
      *
           DISPLAY "TSRYCD-SRYCD (" TSRYCD-IDX ")="
                    TSRYCD-SRYCD (TSRYCD-IDX)
           DISPLAY "TMP01-ZAIKAISU(" TSRYCD-IDX ")="
                    TMP01-ZAIKAISU (TSRYCD-IDX)
      *
           IF    ( TSRYCD-SRYVAL (TSRYCD-IDX)  NOT =   SPACE )
               IF    ( SPA-Q01-SRYVALITEM  =   CONST-VALKAISU
                                            OR CONST-VALTOTAL )
                   IF    ( SPA-Q01-SRYVALITEM  =   CONST-VALKAISU )
                       MOVE    TMP01-ZAIKAISU (TSRYCD-IDX)
                                           TO  WRK-SRYVAL
                   ELSE
                       MOVE    TMP01-SURYOTOTAL (TSRYCD-IDX)
                                           TO  WRK-SRYVAL
                   END-IF
      *
                   DISPLAY "TSRYCD-SRYVALCOMP (" TSRYCD-IDX "):"
                            TSRYCD-SRYVALCOMP (TSRYCD-IDX)
                   DISPLAY "TSRYCD-SRYVALNUM (" TSRYCD-IDX "):"
                            TSRYCD-SRYVALNUM (TSRYCD-IDX)
      *
                   EVALUATE   TSRYCD-SRYVALCOMP (TSRYCD-IDX)
                   WHEN    CONST-GREATER-EQUAL
                       IF    ( WRK-SRYVAL
                                  >=  TSRYCD-SRYVALNUM
                                                   (TSRYCD-IDX))
                           CONTINUE
                       ELSE
                           MOVE    ZERO    TO  FLG-TARGET-SRYCD
                                                   (TSRYCD-IDX)
                           MOVE    ZERO    TO  FLG-TARGET
                       END-IF
                   WHEN    CONST-LESS-EQUAL
                       IF    ( WRK-SRYVAL
                                  <=  TSRYCD-SRYVALNUM
                                                   (TSRYCD-IDX))
                           CONTINUE
                       ELSE
                           MOVE    ZERO    TO  FLG-TARGET-SRYCD
                                                   (TSRYCD-IDX)
                           MOVE    ZERO    TO  FLG-TARGET
                       END-IF
                   WHEN    OTHER
                       IF    ( WRK-SRYVAL
                                   =  TSRYCD-SRYVALNUM
                                                   (TSRYCD-IDX))
                           CONTINUE
                       ELSE
                           MOVE    ZERO    TO  FLG-TARGET-SRYCD
                                                   (TSRYCD-IDX)
                           MOVE    ZERO    TO  FLG-TARGET
                       END-IF
                   END-EVALUATE
               END-IF
           END-IF
      *
           DISPLAY "FLG-TARGET = " FLG-TARGET
      *
           .
       20011-SRYVAL-PTSRH-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為数値検索より患者照会編集処理
      *****************************************************************
       20011-SRYVAL-PTSRH-INS-SEC  SECTION.
      *
           PERFORM VARYING IDXM    FROM    1   BY   1
                   UNTIL ( IDXM    >   12 )
      *
               IF    ( TMP01-MON-SRYYM (TSRYCD-IDX IDXM)
                                   =   SPACE )
                   CONTINUE
               ELSE
                   PERFORM 200111-SRYVAL-PTSRH-INS-SEC
               END-IF
           END-PERFORM
      *
           .
       20011-SRYVAL-PTSRH-INS-EXT.
           EXIT.
      *****************************************************************
      *    診療行為数値検索より患者照会編集処理
      *****************************************************************
       200111-SRYVAL-PTSRH-INS-SEC  SECTION.
      *
           INITIALIZE                  PTSRH-REC
                                           PTSRH-MEMO-R
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  PTSRH-TERMID
           MOVE    TMP01-PTID      TO  PTSRH-PTID
           MOVE    TMP01-PTNUM     TO  PTSRH-PTNUM
           MOVE    TMP01-KANANAME  TO  PTSRH-KANANAME
           MOVE    TMP01-SEX       TO  PTSRH-SEX
           MOVE    TMP01-BIRTHDAY  TO  PTSRH-BIRTHDAY
           MOVE    TMP01-SRYCD    (TSRYCD-IDX)
                                   TO  PTSRH-SRYCD
      *
           MOVE    TSRYCD-NAME (TSRYCD-IDX)
                                   TO  PTSRH-SRYNAME
           MOVE    TSRYCD-SURYO-EDIT (TSRYCD-IDX)
                                   TO  PTSRH-SURYO-EDIT
           MOVE    TSRYCD-KANANAME (TSRYCD-IDX)
                                   TO  PTSRH-SRYKANANAME
           MOVE    TSRYCD-IDX      TO  PTSRH-SRYSEQ
      *
           MOVE    TMP01-MON-SRYYM (TSRYCD-IDX IDXM)
                                   TO  PTSRH-SRYYM
      *
           IF    ( TMP01-SRYSURYO1 (TSRYCD-IDX)
                       =   TMP01-SRYSURYO2 (TSRYCD-IDX))
               MOVE    TMP01-SRYSURYO1 (TSRYCD-IDX)
                                   TO  PTSRH-SRYSURYO
           END-IF
           MOVE    TMP01-ZAIKAISU (TSRYCD-IDX)
                                   TO  PTSRH-ZAIKAISU
           MOVE    TMP01-SURYOTOTAL(TSRYCD-IDX)
                                   TO  PTSRH-SURYOTOTAL
      *
           IF    ( TMP01-MON-SRYSURYO1 (TSRYCD-IDX IDXM)
                       =   TMP01-MON-SRYSURYO2 (TSRYCD-IDX IDXM))
               MOVE    TMP01-MON-SRYSURYO1 (TSRYCD-IDX IDXM)
                                   TO  PTSRH-SRYYM-SRYSURYO
           END-IF
           MOVE    TMP01-MON-ZAIKAISU (TSRYCD-IDX IDXM)
                                   TO  PTSRH-SRYYM-ZAIKAISU
           MOVE    TMP01-MON-SURYOTOTAL(TSRYCD-IDX IDXM)
                                   TO  PTSRH-SRYYM-SURYOTOTAL
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
      *
               MOVE    TMP01-MON-DAY (TSRYCD-IDX IDXM IDXD)
                                   TO  PTSRH-DAY (IDXD)
      *
           END-PERFORM
      *
           MOVE    "1"             TO  PTSRH-DATAKBN
           MOVE    SEL-UPYMD       TO  PTSRH-CREYMD
                                       PTSRH-UPYMD
           MOVE    SEL-UPHMS       TO  PTSRH-UPHMS
      *
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           .
       200111-SRYVAL-PTSRH-INS-EXT.
           EXIT.
      *****************************************************************
      *    収納データ取得　処理
      *****************************************************************
       2001-SYUNOU-GET-SEC         SECTION.
      *
           PERFORM 900-SYUNOU-KEY77-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
      *
               IF    ( SYU-SYUHKNNUM   NOT =   SPACE )
                   PERFORM 900-HKNCOMBI-KEY-SEL-SEC
               ELSE
                   INITIALIZE              HKNCOMBI-REC
               END-IF
      *
               INITIALIZE                  SKYSRH-REC
               MOVE    SYU-HOSPNUM     TO  SKYSRH-HOSPNUM
               MOVE    SPA-TERMID      TO  SKYSRH-TERMID
               MOVE    SYU-PTID        TO  SKYSRH-PTID
               MOVE    SYU-NYUGAIKBN   TO  SKYSRH-NYUGAIKBN
               MOVE    SEL-STSRYYMD(1:6)
                                       TO  SKYSRH-SRYYM
               MOVE    COMB-HKNID      TO  SKYSRH-HKNID
               MOVE    SYU-SYUHKNNUM   TO  SKYSRH-HKNNUM
               MOVE    SYU-TOTAL-HKNTEN
                                       TO  SKYSRH-TOTALTEN
               MOVE    SKYSRH-REC      TO  MCPDATA-REC
               MOVE    "tbl_skysrh"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
      *
               PERFORM 900-SYUNOU-KEY77-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key77"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-SYUNOU-GET-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    TMP01PARA       TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    TMP02PARA       TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
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
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    収納検索処理
      *****************************************************************
       900-SYUNOU-KEY77-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM     TO  SYU-HOSPNUM
           MOVE    SEL-NYUGAIKBN   TO  SYU-NYUGAIKBN
      *
           IF    ( SEL-NYUGAIKBN   =   "1" )
               MOVE    "3"         TO  SYU-CREATEKBN
           ELSE
               MOVE    "%"         TO  SYU-CREATEKBN
           END-IF
      *
           MOVE    SEL-SRYKA       TO  SYU-SRYKA
           MOVE    SEL-STSRYYMD(1:6)
                                   TO  SYU-SRYYMD
           MOVE    "%"             TO  SYU-SRYYMD(7:1)
           MOVE    "9%"            TO  SYU-SYUHKNNUM
           MOVE    SPA-Q01-TOTALTEN
                                   TO  SYU-TOTAL-HKNTEN
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key77"         TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 900-SYUNOU-KEY77-FET-SEC
           ELSE
               MOVE    1           TO  FLG-SYUNOU
               INITIALIZE              SYUNOU-REC
           END-IF
      *
           .
       900-SYUNOU-KEY77-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納FETCH処理
      *****************************************************************
       900-SYUNOU-KEY77-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
           MOVE    ZERO            TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               MOVE    "tbl_syunou"    TO  MCP-TABLE
               MOVE    "key77"         TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
      *
                   MOVE    MCPDATA-REC TO  SYUNOU-REC
      *
                   MOVE    1       TO  FLG-LOOPEND
      *
                   IF    ( SYU-NYUGAIKBN   =   "1" )
                       IF    ( SYU-CREATEKBN   =   "3" )
                           CONTINUE
                       ELSE
                           MOVE    ZERO    TO  FLG-LOOPEND
                       END-IF
                   END-IF
      *
                   EVALUATE    SYU-SYUHKNNUM
                   WHEN    "900"   THRU "919"
                   WHEN    "971"
                   WHEN    "973"
                   WHEN    "980"   THRU "989"
                       CONTINUE
                   WHEN    OTHER
                       MOVE    ZERO        TO  FLG-LOOPEND
                   END-EVALUATE
      *
               ELSE
                   MOVE    1       TO  FLG-SYUNOU
                   INITIALIZE          SYUNOU-REC
               END-IF
      *
           END-PERFORM
      *
           .
       900-SYUNOU-KEY77-FET-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ取得処理
      *****************************************************************
       900-HKNCOMBI-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-HKNCOMBI
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SYU-HOSPNUM     TO  COMB-HOSPNUM
           MOVE    SYU-PTID        TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  HKNCOMBI-REC
           ELSE
               MOVE    1           TO  FLG-HKNCOMBI
               INITIALIZE              HKNCOMBI-REC
           END-IF
      *
           MOVE    "tbl_hkncombi"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNCOMBI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療会計検索処理
      *****************************************************************
       900-SRYACCT-KEY57-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYACCT
      *
           INITIALIZE                  SRYACCT-REC
           MOVE    SRYSRH-HOSPNUM  TO  ACCT-HOSPNUM
           MOVE    SRYSRH-PTID     TO  ACCT-PTID
           MOVE    SRYSRH-ZAINUM   TO  ACCT-ZAINUM
           MOVE    SRYACCT-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key57"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SRYACCT-REC
           ELSE
               MOVE    1           TO  FLG-SRYACCT
               INITIALIZE              SRYACCT-REC
           END-IF
      *
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key57"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SRYACCT-KEY57-SEL-EXT.
           EXIT.
      *****************************************************************
      *    請求管理照会追加処理
      *****************************************************************
       900-SKYSRH-INS1-SEC             SECTION.
      *
           INITIALIZE                  SKYSRH-REC
           MOVE    SPA-HOSPNUM     TO  SKYSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  SKYSRH-TERMID
           MOVE    SEL-NYUGAIKBN   TO  SKYSRH-NYUGAIKBN
           MOVE    SEL-STSRYYMD(1:6)
                                   TO  SKYSRH-STSRYYM
           MOVE    SEL-EDSRYYMD(1:6)
                                   TO  SKYSRH-EDSRYYM
           MOVE    SPA-Q01-TOTALTEN
                                   TO  SKYSRH-TOTALTEN
           IF    ( SPA-Q01-TOKKICD NOT =   SPACE )
               MOVE    SPA-Q01-TOKKICD
                                   TO  SKYSRH-TOKKI (1)
               MOVE    SPACE       TO  SKYSRH-TOKKI (2)
           ELSE
               MOVE    SPACE       TO  SKYSRH-TOKKI (1)
               MOVE    SPACE       TO  SKYSRH-TOKKI (2)
           END-IF
      *
           IF    ( SPA-Q01-TOKKINAME   NOT =   SPACE )
               MOVE    SPA-Q01-TOKKINAME
                                   TO  SKYSRH-TOKKINM (1)
               MOVE    SPACE       TO  SKYSRH-TOKKINM (2)
           ELSE
               MOVE    SPACE       TO  SKYSRH-TOKKINM (1)
               MOVE    SPACE       TO  SKYSRH-TOKKINM (2)
           END-IF
      *
           MOVE    SEL-TENKENRECE  TO  SKYSRH-SKYKBN-RECEDEN
      *
           MOVE    SKYSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_skysrh"    TO  MCP-TABLE
           IF    ( SPA-Q01-SAIGAIKBN   =   SPACE )
               MOVE    "ins1"      TO  MCP-PATHNAME
           ELSE
               MOVE    "ins2"      TO  MCP-PATHNAME
           END-IF
           PERFORM 910-DBINSERT-SEC
      *
           .
       900-SKYSRH-INS1-EXT.
           EXIT.
      *****************************************************************
      *    請求管理照会削除処理
      *****************************************************************
       900-SKYSRH-DEL1-SEC             SECTION.
      *
           INITIALIZE                  SKYSRH-REC
           MOVE    SPA-HOSPNUM     TO  SKYSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  SKYSRH-TERMID
           MOVE    SKYSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_skysrh"    TO  MCP-TABLE
           MOVE    "del1"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       900-SKYSRH-DEL1-EXT.
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
      *    診療行為照会テーブル検索処理
      *****************************************************************
       900-SRYSRH-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYSRH
      *
           INITIALIZE                  SRYSRH-REC
           MOVE    WKBQ01-SQL      TO  SRYSRH-SQL
           MOVE    SRYSRH-REC      TO  MCPDATA-REC
           MOVE    "tbl_srysrh"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SRYSRH-REC
           ELSE
               INITIALIZE              SRYSRH-REC
               MOVE    1           TO  FLG-SRYSRH
           END-IF
      *
           .
       900-SRYSRH-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療行為照会テーブル検索処理
      *****************************************************************
       900-SRYSRH-KEY-FET-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYSRH
      *
           MOVE    "tbl_srysrh"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SRYSRH-REC
           ELSE
               INITIALIZE              SRYSRH-REC
               MOVE    1           TO  FLG-SRYSRH
           END-IF
      *
           .
       900-SRYSRH-KEY-FET-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO FLG-TENSU
      *
           INITIALIZE                 TNS-TENSU-REC
           MOVE    SPA-HOSPNUM     TO TNS-HOSPNUM
           MOVE    TSRYCD-SRYCD (TSRYCD-IDX)
                                   TO TNS-SRYCD
           MOVE    SEL-STSRYYMD    TO TNS-YUKOSTYMD
                                      TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               INITIALIZE              TNS-TENSU-REC
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療行為数値検索チェックファイル読込処理
      *****************************************************************
       900-TMP01-READ-RND-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-TMP01
           PERFORM 9001-TMP01-KEY-EDIT-SEC
      *
           READ    TMP01-FILE
           INVALID
               MOVE    1           TO  FLG-TMP01
           END-READ
      *
           .
       900-TMP01-READ-RND-EXT.
           EXIT.
      *****************************************************************
      *    診療行為数値検索チェックファイルキー項目編集処理
      *****************************************************************
       9001-TMP01-KEY-EDIT-SEC         SECTION.
      *
           INITIALIZE                  TMP01-REC
           MOVE    SRYSRH-HOSPNUM  TO  TMP01-HOSPNUM
           MOVE    SRYSRH-PTID     TO  TMP01-PTID
      *
           .
       9001-TMP01-COMMON-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為数値検索チェックファイル読込処理
      *****************************************************************
       900-TMP01-READ-SEQ-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-TMP01
      *
           READ    TMP01-FILE      NEXT
           AT  END
               MOVE    1           TO  FLG-TMP01
           END-READ
      *
           .
       900-TMP01-READ-SEQ-EXT.
           EXIT.
      *****************************************************************
      *    同一剤チェックファイル読込処理
      *    同じ剤の中に同じ診療コードが無いかチェック
      *    ある場合は回数はカウントせず、合計のみ集計
      *****************************************************************
       900-TMP02-READ-RND-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-TMP02
           PERFORM 9001-TMP02-KEY-EDIT-SEC
      *
           READ    TMP02-FILE
           INVALID
               MOVE    1           TO  FLG-TMP02
           END-READ
      *
           .
       900-TMP02-READ-RND-EXT.
           EXIT.
      *****************************************************************
      *    同一剤チェックファイルキー項目編集処理
      *****************************************************************
       9001-TMP02-KEY-EDIT-SEC         SECTION.
      *
           INITIALIZE                  TMP02-REC
           MOVE    SRYSRH-HOSPNUM  TO  TMP02-HOSPNUM
           MOVE    SRYSRH-PTID     TO  TMP02-PTID
           MOVE    SRYSRH-ZAINUM   TO  TMP02-ZAINUM
           MOVE    SRYSRH-SRYCD(IDX-SRY)
                                   TO  TMP02-SRYCD
           MOVE    SRYSRH-RENNUM   TO  TMP02-RENNUM
           MOVE    IDX-SRY         TO  TMP02-SRYCD-IDX
      *
           .
       9001-TMP02-COMMON-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    同一剤チェックファイル読込処理
      *****************************************************************
       901-TMP02-READ-RND-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-TMP02
           PERFORM 9011-TMP02-KEY-EDIT-SEC
      *
           READ    TMP02-FILE
           INVALID
               MOVE    1           TO  FLG-TMP02
           END-READ
      *
           .
       901-TMP02-READ-RND-EXT.
           EXIT.
      *****************************************************************
      *    同一剤チェックファイルキー項目編集処理
      *****************************************************************
       9011-TMP02-KEY-EDIT-SEC         SECTION.
      *
           INITIALIZE                  TMP02-REC
           MOVE    SRYSRH-HOSPNUM  TO  TMP02-HOSPNUM
           MOVE    SRYSRH-PTID     TO  TMP02-PTID
           MOVE    SRYSRH-ZAINUM   TO  TMP02-ZAINUM
           MOVE    SRYSRH-SRYCD(IDX-SRY)
                                   TO  TMP02-SRYCD
           MOVE    ZERO            TO  TMP02-RENNUM
                                       TMP02-SRYCD-IDX
      *
           .
       9011-TMP02-COMMON-EDIT-EXT.
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
