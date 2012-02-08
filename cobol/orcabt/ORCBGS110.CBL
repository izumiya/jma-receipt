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
       PROGRAM-ID.                 ORCBGS110.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 新保険別請求チェック表 作表用データ作成
      *  管理者            :
      *  作成日付    作業者        記述
      *  11/01/13    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    請求ファイル
           SELECT  BGSSKY-FILE     ASSIGN  ASS-BGSSKY
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  BGSSKY-KEY
                                   FILE    STATUS  IS  STS-BGSSKY.
      *
      *    公費ファイル
           SELECT  BGSKOH-FILE     ASSIGN  ASS-BGSKOH
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  BGSKOH-KEY
                                   FILE    STATUS  IS  STS-BGSKOH.
      *
      *    保険者別請求ファイル
           SELECT  BGSHKNJA-FILE  ASSIGN  ASS-BGSHKNJA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  BGSHKNJA-KEY
                                   FILE    STATUS  IS  STS-BGSHKNJA.
      *
      *    その他請求ファイル
           SELECT  BGSETC-FILE     ASSIGN  ASS-BGSETC
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  BGSETC-KEY
                                   FILE    STATUS  IS  STS-BGSETC.
      *
      *    エラーファイル
           SELECT  BGS100ERR-FILE  ASSIGN  ASS-BGS100ERR
                                   FILE    STATUS  IS  STS-BGS100ERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    請求ファイル
       FD  BGSSKY-FILE.
       01  BGSSKY-REC.
           COPY    "CPBGSF01.INC"  REPLACING  //BGSF01-//
                                   BY         //BGSSKY-//.
      *
      *    公費ファイル
       FD  BGSKOH-FILE.
       01  BGSKOH-REC.
           COPY    "CPBGSF01.INC"  REPLACING  //BGSF01-//
                                   BY         //BGSKOH-//.
      *
      *    保険者別請求ファイル
       FD  BGSHKNJA-FILE.
       01  BGSHKNJA-REC.
           COPY    "CPBGSF01.INC"  REPLACING  //BGSF01-//
                                   BY         //BGSHKNJA-//.
      *
      *    その他請求ファイル
       FD  BGSETC-FILE.
       01  BGSETC-REC.
           COPY    "CPBGSF01.INC"  REPLACING  //BGSF01-//
                                   BY         //BGSETC-//.
      *
      *    エラーファイル
       FD  BGS100ERR-FILE.
       01  BGS100ERR-REC           PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
       01  ASS-AREA.
           03  ASS-BGSSKY          PIC X(255).
           03  ASS-BGSKOH          PIC X(255).
           03  ASS-BGSHKNJA        PIC X(255).
           03  ASS-BGSETC          PIC X(255).
           03  ASS-BGS100ERR       PIC X(255).
      *
       01  CNT-AREA.
           03  CNT-SYAHKN          PIC 9(08).
           03  CNT-SYAKOH          PIC 9(08).
           03  CNT-KOKHKN          PIC 9(08).
           03  CNT-KOKKOH          PIC 9(08).
           03  CNT-KOKHKNJA       PIC 9(08).
           03  CNT-KOUHKN          PIC 9(08).
           03  CNT-KOUKOH          PIC 9(08).
           03  CNT-KOUHKNJA       PIC 9(08).
      *
       01  STS-AREA.
           03  STS-BGSSKY          PIC X(02).
           03  STS-BGSKOH          PIC X(02).
           03  STS-BGSHKNJA        PIC X(02).
           03  STS-BGSETC          PIC X(02).
           03  STS-BGS100ERR       PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-BGSSKY          PIC 9(01).
           03  FLG-BGSKOH          PIC 9(01).
           03  FLG-BGSHKNJA        PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
           03  IDX1                PIC 9(05).
           03  IDX-KOH             PIC 9(05).
           03  IDX-NG              PIC 9(05).
           03  IDX-HKN             PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-TABLE           PIC X(64).
           03  WRK-PATHNAME        PIC X(64).
           03  WRK-ERRMSG          PIC X(200).
           03  WRK-HOKEN           PIC 9(1).
           03  WRK-KOHKBN          PIC X(1).
           03  WRK-KOHNUM          PIC X(3).
           03  WRK-TTLID           PIC 9(5).
           03  WRK-RECEKA          PIC X(2).
           03  WRK-ST              PIC 9(5).
      *
       01  CONST-AREA.
           03  CONST-SYAHO         PIC 9(01)   VALUE 1.
           03  CONST-KOKUHO        PIC 9(01)   VALUE 2.
           03  CONST-KOUKI         PIC 9(01)   VALUE 3.
           03  CONST-TOKUYOU       PIC 9(01)   VALUE 4.
           03  CONST-JIHI          PIC 9(01)   VALUE 5.
           03  CONST-CHIKEN        PIC 9(01)   VALUE 6.
           03  CONST-ROUSAI        PIC 9(01)   VALUE 7.
           03  CONST-JIBAI         PIC 9(01)   VALUE 8.
           03  CONST-KOUGAI        PIC 9(01)   VALUE 9.
           03  CONST-RECEKA-MAX    PIC 9(05)   VALUE 100.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK2009.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    請求管理
           COPY    "CPRCF010.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    保険別請求チェック表サブ
           COPY    "CPORCSSKYGET.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  BGS100P-AREA.
           COPY    "CPORCSPRTLNK.INC".
           COPY    "CPBGS100PARA.INC".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       PROCEDURE               DIVISION
                                       USING
                                       BGS100P-AREA
                                       SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
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
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                      STS-AREA
                                           CNT-AREA
                                           FLG-AREA
                                           IDX-AREA
                                           WRK-AREA
      *
           MOVE    BGS100P-SKYFNAME    TO  ASS-BGSSKY
           MOVE    BGS100P-KOHFNAME    TO  ASS-BGSKOH
           MOVE    BGS100P-HKNJAFNAME  TO  ASS-BGSHKNJA
           MOVE    BGS100P-ETCFNAME    TO  ASS-BGSETC
           MOVE    BGS100P-ERRFNAME    TO  ASS-BGS100ERR
      *
           MOVE    "STM"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPACE               TO  JOB-YOBI
           PERFORM 800-ORCSJOB-SEC
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "2009"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    BGS100P-SRYYMD      TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-2009-REC
      *
           MOVE    ZERO                TO  BGS100P-SYUKA
                                           BGS100P-SYUKA-SYA
                                           BGS100P-SYUKA-KOK
                                           BGS100P-SYUKA-KOU
           INITIALIZE                      BGS100P-RECEKA-G
      *
           IF    ( SYS-2009-SYUKA-KBN  =   "1" )
      *
                   MOVE    1           TO  BGS100P-SYUKA
      *
               IF    ( SYS-2009-SYUKA-SYAHO    =   "1" )
                   MOVE    1           TO  BGS100P-SYUKA-SYA
               END-IF
               IF    ( SYS-2009-SYUKA-KOKUHO   =   "1" )
                   MOVE    1           TO  BGS100P-SYUKA-KOK
               END-IF
               IF    ( SYS-2009-SYUKA-KOUIKI   =   "1" )
                   MOVE    1           TO  BGS100P-SYUKA-KOU
               END-IF
           END-IF
      *
           OPEN    OUTPUT  BGSSKY-FILE
                           BGSKOH-FILE
                           BGSHKNJA-FILE
      *
           CLOSE           BGSSKY-FILE
                           BGSKOH-FILE
                           BGSHKNJA-FILE
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           OPEN    I-O     BGSSKY-FILE
                           BGSKOH-FILE
                           BGSHKNJA-FILE
      *
           OPEN    OUTPUT  BGSETC-FILE
      *
      *    社保分
           IF    ( BGS100P-SYAHO   =   1 )
            OR   ( BGS100P-SOUKATU =   1 )
               MOVE    "tbl_seikyu"        TO  WRK-TABLE
               MOVE    "key72"             TO  WRK-PATHNAME
               PERFORM 2001-SEIKYU-EDIT-SEC
           END-IF
      *
      *    国保分
           IF    ( BGS100P-KOKUHO  =   1 )
            OR   ( BGS100P-SOUKATU =   1 )
               MOVE    "tbl_seikyu"        TO  WRK-TABLE
               MOVE    "key73"             TO  WRK-PATHNAME
               PERFORM 2001-SEIKYU-EDIT-SEC
           END-IF
      *
      *    後期高齢者分
           IF    ( BGS100P-KOUKI   =   1 )
            OR   ( BGS100P-SOUKATU =   1 )
               MOVE    "tbl_seikyu"        TO  WRK-TABLE
               MOVE    "key74"             TO  WRK-PATHNAME
               PERFORM 2001-SEIKYU-EDIT-SEC
           END-IF
      *
      *    その他請求
           IF    ( BGS100P-RC      =   ZERO )
               PERFORM 2001-ETC-SEIKYU-EDIT-SEC
           END-IF
      *
      *    診療科編集
           IF    ( BGS100P-RC      =   ZERO )
               IF    ( BGS100P-RECEKA-MAX  >   ZERO )
                   PERFORM 2001-RECEKA-EDIT-SEC
               END-IF
           END-IF
      *
           CLOSE           BGSSKY-FILE
                           BGSKOH-FILE
                           BGSHKNJA-FILE
                           BGSETC-FILE
      *
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求データ編集処理
      *****************************************************************
       2001-SEIKYU-EDIT-SEC            SECTION.
      *
           PERFORM 900-SEIKYU-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SEIKYU  NOT =   ZERO )
                    OR   ( BGS100P-RC  NOT =   ZERO )
      *
               PERFORM 900-PTINF-KEY-SEL-SEC
               IF    ( PTINF-TSTPTNUMKBN   NOT =   "1" )
      *
                   INITIALIZE                  ORCSSKYGETAREA
                   MOVE    "1"             TO  LNK-SKYGET-KBN
                   MOVE    BGS100P-SRYYM   TO  LNK-SKYGET-SRYYM
                   CALL    "ORCSSKYGET"        USING
                                               ORCSSKYGETAREA
                                               RECE10-REC
                                               SPA-AREA
      *
                   IF    ( LNK-SKYGET-RC   =   ZERO )
                       PERFORM 20011-SEIKYU-EDIT-SEC
                   ELSE
                       DISPLAY "SKIPPED RC["   LNK-SKYGET-RC
                           "]PTID["            RECE10-PTID
                           "]PTNUM["           RECE10-PTNUM
                           "]SRYYM["           RECE10-SRYYM
                           "]NYUGAIKBN["       RECE10-NYUGAIKBN
                           "]RECEKA["          RECE10-RECEKA
                           "]TEISYUTUSAKI["    RECE10-TEISYUTUSAKI
                           "]RECESYUBETU["     RECE10-RECESYUBETU
                           "]HKNJANUM["        RECE10-HKNJANUM
                           "]HOJOKBN_KEY["     RECE10-HOJOKBN-KEY
                           "]KOHID_KEY["       RECE10-KOHID-KEY
                           "]TEKSTYMD["        RECE10-TEKSTYMD
                           "]"
                   END-IF
               END-IF
      *
               PERFORM 900-SEIKYU-FET-SEC
      *
           END-PERFORM
      *
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-SEIKYU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    請求データ編集処理
      *****************************************************************
       20011-SEIKYU-EDIT-SEC           SECTION.
      *
           MOVE    CONST-SYAHO         TO  WRK-HOKEN
           IF    ( RECE10-HKNNUM-S     =   "060" OR "067" )
               MOVE    CONST-KOKUHO    TO  WRK-HOKEN
           END-IF
           IF    ( RECE10-HKNNUM-S     =   "039" )
               MOVE    CONST-KOUKI     TO  WRK-HOKEN
           END-IF
      *
           PERFORM 200111-BGSSKY-EDIT-SEC
      *
           PERFORM VARYING IDX-KOH     FROM    1   BY  1
                   UNTIL ( IDX-KOH     >   4 )
               IF    ( LNK-SKYGET-KOH-HKNNUM (IDX-KOH)
                                   NOT =   SPACE )
                   MOVE    "1"         TO  WRK-KOHKBN
                   MOVE    LNK-SKYGET-KOH-HKNNUM (IDX-KOH)
                                       TO  WRK-KOHNUM
                   PERFORM 200111-BGSKOH-EDIT-SEC
               END-IF
               IF    ( LNK-SKYGET-KOHETC-HKNNUM (IDX-KOH)
                                   NOT =   SPACE )
                   MOVE    "2"         TO  WRK-KOHKBN
                   MOVE    LNK-SKYGET-KOHETC-HKNNUM (IDX-KOH)
                                       TO  WRK-KOHNUM
                   PERFORM 200111-BGSKOH-EDIT-SEC
               END-IF
           END-PERFORM
      *
           IF    ( WRK-HOKEN    =  CONST-KOKUHO
                                OR CONST-KOUKI )
               PERFORM 200111-BGSHKNJA-EDIT-SEC
           END-IF
      *
           IF    ( RECE10-RECEKA       NOT =   "00" )
               PERFORM VARYING IDX0 FROM    1   BY  1
                      UNTIL ( IDX0 >   CONST-RECEKA-MAX )
                       OR   ( BGS100P-RECEKA (IDX0)
                                           =   RECE10-RECEKA )
      *
                   IF    ( IDX0            >   BGS100P-RECEKA-MAX )
                       MOVE    RECE10-RECEKA
                                           TO  BGS100P-RECEKA (IDX0)
                       MOVE    IDX0        TO  BGS100P-RECEKA-MAX
                       MOVE    CONST-RECEKA-MAX
                                           TO  IDX0
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       20011-SEIKYU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    請求ファイル編集処理
      *****************************************************************
       200111-BGSSKY-EDIT-SEC          SECTION.
      *
           PERFORM 900-BGSSKY-READ-SEC
      *
           COMPUTE BGSSKY-KENSU
               =   BGSSKY-KENSU    +   1
           COMPUTE BGSSKY-NISSU
               =   BGSSKY-NISSU        + LNK-SKYGET-IRYO-NISSU
           COMPUTE BGSSKY-TENSU
               =   BGSSKY-TENSU        + LNK-SKYGET-IRYO-TOTALTEN
           COMPUTE BGSSKY-FTNMONEY
               =   BGSSKY-FTNMONEY     + LNK-SKYGET-IRYO-ICHIBUFTN
           COMPUTE BGSSKY-HKNMONEY
               =   BGSSKY-HKNMONEY     + LNK-SKYGET-IRYO-HOKENJA
           COMPUTE BGSSKY-KOHMONEY
               =   BGSSKY-KOHMONEY     + LNK-SKYGET-IRYO-KOHFTN
           COMPUTE BGSSKY-TTLMONEY
               =   BGSSKY-TTLMONEY     + LNK-SKYGET-IRYO-ICHIBUFTN
                                       + LNK-SKYGET-IRYO-HOKENJA
                                       + LNK-SKYGET-IRYO-KOHFTN
           COMPUTE BGSSKY-SKJKAISU
               =   BGSSKY-SKJKAISU     + LNK-SKYGET-SKJ-KAISU
           COMPUTE BGSSKY-SKJTTLMONEY
               =   BGSSKY-SKJTTLMONEY  + LNK-SKYGET-SKJ-RYOYOHI
           COMPUTE BGSSKY-SKJFTNMONEY
               =   BGSSKY-SKJFTNMONEY  + LNK-SKYGET-SKJ-ICHIBUFTN
           COMPUTE BGSSKY-SKJHKNMONEY
               =   BGSSKY-SKJHKNMONEY  + LNK-SKYGET-SKJ-HOKENJA
           COMPUTE BGSSKY-SKJKOHMONEY
               =   BGSSKY-SKJKOHMONEY  + LNK-SKYGET-SKJ-KOHFTN
           COMPUTE BGSSKY-TTLTTLMONEY
               =   BGSSKY-TTLTTLMONEY  + LNK-SKYGET-IRYO-ICHIBUFTN
                                       + LNK-SKYGET-IRYO-HOKENJA
                                       + LNK-SKYGET-IRYO-KOHFTN
                                       + LNK-SKYGET-SKJ-RYOYOHI
      *
           IF    ( FLG-BGSSKY          =   ZERO )
               REWRITE BGSSKY-REC
           ELSE
               WRITE   BGSSKY-REC
      *
               EVALUATE    WRK-HOKEN
               WHEN    CONST-SYAHO
                   COMPUTE CNT-SYAHKN  =   CNT-SYAHKN  +   1
               WHEN    CONST-KOKUHO
                   COMPUTE CNT-KOKHKN  =   CNT-KOKHKN  +   1
               WHEN    CONST-KOUKI
                   COMPUTE CNT-KOUHKN  =   CNT-KOUHKN  +   1
               END-EVALUATE
      *
           END-IF
      *
           .
       200111-BGSSKY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    公費ファイル編集処理
      *****************************************************************
       200111-BGSKOH-EDIT-SEC          SECTION.
      *
           PERFORM 900-BGSKOH-READ-SEC
      *
               COMPUTE BGSKOH-KENSU
                   =   BGSKOH-KENSU    + 1
      *
           IF    ( WRK-KOHKBN      =   "1" )
               COMPUTE BGSKOH-NISSU
                   =   BGSKOH-NISSU    + LNK-SKYGET-KOH-NISSU (IDX-KOH)
               COMPUTE BGSKOH-TENSU
                   =   BGSKOH-TENSU    + LNK-SKYGET-KOH-TOTALTEN
                                                              (IDX-KOH)
           ELSE
               COMPUTE BGSKOH-NISSU
                   =   BGSKOH-NISSU    + LNK-SKYGET-KOHETC-NISSU
                                                              (IDX-KOH)
               COMPUTE BGSKOH-TENSU
                   =   BGSKOH-TENSU    + LNK-SKYGET-KOHETC-TOTALTEN
                                                              (IDX-KOH)
           END-IF
      *
           IF    ( FLG-BGSKOH          =   ZERO )
               REWRITE BGSKOH-REC
           ELSE
               WRITE   BGSKOH-REC
      *
               EVALUATE    WRK-HOKEN
               WHEN    CONST-SYAHO
                   COMPUTE CNT-SYAKOH  =   CNT-SYAHKN  +   1
               WHEN    CONST-KOKUHO
                   COMPUTE CNT-KOKKOH  =   CNT-KOKHKN  +   1
               WHEN    CONST-KOUKI
                   COMPUTE CNT-KOUKOH  =   CNT-KOUHKN  +   1
               END-EVALUATE
      *
           END-IF
      *
           .
       200111-BGSKOH-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    保険者別請求ファイル編集処理
      *****************************************************************
       200111-BGSHKNJA-EDIT-SEC       SECTION.
      *
           PERFORM 900-BGSHKNJA-READ-SEC
      *
           COMPUTE BGSHKNJA-KENSU
               =   BGSHKNJA-KENSU    +   1
           COMPUTE BGSHKNJA-NISSU
               =   BGSHKNJA-NISSU        + LNK-SKYGET-IRYO-NISSU
           COMPUTE BGSHKNJA-TENSU
               =   BGSHKNJA-TENSU        + LNK-SKYGET-IRYO-TOTALTEN
           COMPUTE BGSHKNJA-FTNMONEY
               =   BGSHKNJA-FTNMONEY     + LNK-SKYGET-IRYO-ICHIBUFTN
           COMPUTE BGSHKNJA-HKNMONEY
               =   BGSHKNJA-HKNMONEY     + LNK-SKYGET-IRYO-HOKENJA
           COMPUTE BGSHKNJA-KOHMONEY
               =   BGSHKNJA-KOHMONEY     + LNK-SKYGET-IRYO-KOHFTN
           COMPUTE BGSHKNJA-TTLMONEY
               =   BGSHKNJA-TTLMONEY     + LNK-SKYGET-IRYO-ICHIBUFTN
                                         + LNK-SKYGET-IRYO-HOKENJA
                                         + LNK-SKYGET-IRYO-KOHFTN
           COMPUTE BGSHKNJA-SKJKAISU
               =   BGSHKNJA-SKJKAISU     + LNK-SKYGET-SKJ-KAISU
           COMPUTE BGSHKNJA-SKJTTLMONEY
               =   BGSHKNJA-SKJTTLMONEY  + LNK-SKYGET-SKJ-RYOYOHI
           COMPUTE BGSHKNJA-SKJFTNMONEY
               =   BGSHKNJA-SKJFTNMONEY  + LNK-SKYGET-SKJ-ICHIBUFTN
           COMPUTE BGSHKNJA-SKJHKNMONEY
               =   BGSHKNJA-SKJHKNMONEY  + LNK-SKYGET-SKJ-HOKENJA
           COMPUTE BGSHKNJA-SKJKOHMONEY
               =   BGSHKNJA-SKJKOHMONEY  + LNK-SKYGET-SKJ-KOHFTN
           COMPUTE BGSHKNJA-TTLTTLMONEY
               =   BGSHKNJA-TTLTTLMONEY  + LNK-SKYGET-IRYO-ICHIBUFTN
                                         + LNK-SKYGET-IRYO-HOKENJA
                                         + LNK-SKYGET-IRYO-KOHFTN
                                         + LNK-SKYGET-SKJ-RYOYOHI
      *
           IF    ( FLG-BGSHKNJA          =   ZERO )
               REWRITE BGSHKNJA-REC
           ELSE
               WRITE   BGSHKNJA-REC
      *
               EVALUATE    WRK-HOKEN
               WHEN    CONST-KOKUHO
                   COMPUTE CNT-KOKHKNJA   =   CNT-KOKHKNJA   +   1
               WHEN    CONST-KOUKI
                   COMPUTE CNT-KOUHKNJA   =   CNT-KOUHKNJA   +   1
               END-EVALUATE
      *
           END-IF
      *
           .
       200111-BGSHKNJA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    その他請求データ編集処理
      *****************************************************************
       2001-ETC-SEIKYU-EDIT-SEC        SECTION.
      *
           INITIALIZE                  ORCSSKYGETAREA
           MOVE    "2"             TO  LNK-SKYGET-KBN
           MOVE    BGS100P-SRYYM   TO  LNK-SKYGET-SRYYM
           CALL    "ORCSSKYGET"        USING
                                       ORCSSKYGETAREA
                                       RECE10-REC
                                       SPA-AREA
      *
           PERFORM VARYING IDX-NG    FROM    1   BY  1
                   UNTIL ( IDX-NG    >   2 )
      *
               PERFORM VARYING IDX-HKN    FROM    1   BY  1
                       UNTIL ( IDX-HKN >  9 )
      *
                   INITIALIZE              BGSETC-REC
                   MOVE   IDX-HKN      TO  BGSETC-HOKEN
                   IF    ( IDX-NG      =   1 )
                       MOVE    "1"     TO  BGSETC-NYUGAIKBN
                   ELSE
                       MOVE    "2"     TO  BGSETC-NYUGAIKBN
                   END-IF
                   MOVE   "  "         TO  BGSETC-RECEKA
                   MOVE   LNK-SKYGET-KENSU (IDX-HKN IDX-NG)
                                       TO  BGSETC-KENSU
                   MOVE   LNK-SKYGET-IRYO2-NISSU (IDX-HKN IDX-NG)
                                       TO  BGSETC-NISSU
                   MOVE   LNK-SKYGET-IRYO2-TOTALTEN (IDX-HKN IDX-NG)
                                       TO  BGSETC-TENSU
                   MOVE   LNK-SKYGET-IRYO2-ICHIBUFTN (IDX-HKN IDX-NG)
                                       TO  BGSETC-FTNMONEY
                   MOVE   LNK-SKYGET-IRYO2-HOKENJA (IDX-HKN IDX-NG)
                                       TO  BGSETC-HKNMONEY
                   MOVE   LNK-SKYGET-IRYO2-KOHFTN (IDX-HKN IDX-NG)
                                       TO  BGSETC-KOHMONEY
      *
                   COMPUTE BGSETC-TTLMONEY
                       =   LNK-SKYGET-IRYO2-ICHIBUFTN (IDX-HKN IDX-NG)
                       +   LNK-SKYGET-IRYO2-HOKENJA   (IDX-HKN IDX-NG)
                       +   LNK-SKYGET-IRYO2-KOHFTN    (IDX-HKN IDX-NG)
      *
                   MOVE    LNK-SKYGET-SKJ2-KAISU (IDX-HKN IDX-NG)
                                       TO  BGSETC-SKJKAISU
                   MOVE    LNK-SKYGET-SKJ2-RYOYOHI (IDX-HKN IDX-NG)
                                       TO  BGSETC-SKJTTLMONEY
                   MOVE    LNK-SKYGET-SKJ2-ICHIBUFTN (IDX-HKN IDX-NG)
                                       TO  BGSETC-SKJFTNMONEY
                   MOVE    LNK-SKYGET-SKJ2-HOKENJA (IDX-HKN IDX-NG)
                                       TO  BGSETC-SKJHKNMONEY
                   MOVE    LNK-SKYGET-SKJ2-KOHFTN  (IDX-HKN IDX-NG)
                                       TO  BGSETC-SKJKOHMONEY
      *
                   COMPUTE BGSETC-TTLTTLMONEY
                       =   LNK-SKYGET-IRYO2-ICHIBUFTN (IDX-HKN IDX-NG)
                       +   LNK-SKYGET-IRYO2-HOKENJA   (IDX-HKN IDX-NG)
                       +   LNK-SKYGET-IRYO2-KOHFTN    (IDX-HKN IDX-NG)
                       +   LNK-SKYGET-SKJ2-RYOYOHI    (IDX-HKN IDX-NG)
      *
      *
                   IF    ( IDX-NG            =   1 )
                       MOVE    LNK-SKYGET-JIHI (IDX-HKN 3)
                                       TO  BGSETC-JIHIMONEY
                       MOVE    LNK-SKYGET-DISCOUNT-MONEY (IDX-HKN 3)
                                       TO  BGSETC-GENMEN
                       MOVE    LNK-SKYGET-CHOSEI1 (IDX-HKN 3)
                                       TO  BGSETC-CHOSEI1
                       MOVE    LNK-SKYGET-CHOSEI2 (IDX-HKN 3)
                                       TO  BGSETC-CHOSEI2
                       MOVE    LNK-SKYGET-HASU (IDX-HKN 3)
                                       TO  BGSETC-HASU
                   ELSE
                       MOVE    LNK-SKYGET-JIHI (IDX-HKN IDX-NG)
                                       TO  BGSETC-JIHIMONEY
                       MOVE    LNK-SKYGET-DISCOUNT-MONEY(IDX-HKN IDX-NG)
                                       TO  BGSETC-GENMEN
                       MOVE    LNK-SKYGET-CHOSEI1 (IDX-HKN IDX-NG)
                                       TO  BGSETC-CHOSEI1
                       MOVE    LNK-SKYGET-CHOSEI2 (IDX-HKN IDX-NG)
                                       TO  BGSETC-CHOSEI2
                       MOVE    LNK-SKYGET-HASU (IDX-HKN IDX-NG)
                                       TO  BGSETC-HASU
                   END-IF
      *
                   WRITE   BGSETC-REC
      *
               END-PERFORM
      *
           END-PERFORM
      *
           .
       2001-ETC-SEIKYU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       2001-RECEKA-EDIT-SEC        SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   BGS100P-RECEKA-MAX  - 1 )
               COMPUTE WRK-ST  =   IDX0    +   1
               PERFORM VARYING IDX1    FROM    WRK-ST  BY  1
                       UNTIL ( IDX1    >   BGS100P-RECEKA-MAX )
                   IF    ( BGS100P-RECEKA (IDX0)
                                       >   BGS100P-RECEKA (IDX1))
                       MOVE    BGS100P-RECEKA (IDX0)
                                       TO  WRK-RECEKA
                       MOVE    BGS100P-RECEKA (IDX1)
                                       TO  BGS100P-RECEKA (IDX0)
                       MOVE    WRK-RECEKA
                                       TO  BGS100P-RECEKA (IDX1)
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           COMPUTE BGS100P-RECEKA-MAX
               =   BGS100P-RECEKA-MAX      +   1
           MOVE    "00"        TO  BGS100P-RECEKA (BGS100P-RECEKA-MAX)
      *
           .
       2001-RECEKA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           DISPLAY "CNT-SYAHKN   ["   CNT-SYAHKN "]"
           DISPLAY "CNT-SYAKOH   ["   CNT-SYAKOH "]"
           DISPLAY "CNT-KOKHKN   ["   CNT-KOKHKN "]"
           DISPLAY "CNT-KOKKOH   ["   CNT-KOKKOH "]"
           DISPLAY "CNT-KOKHKNJA ["   CNT-KOKHKNJA "]"
           DISPLAY "CNT-KOUHKN   ["   CNT-KOUHKN "]"
           DISPLAY "CNT-KOUKOH   ["   CNT-KOUKOH "]"
           DISPLAY "CNT-KOUHKNJA ["   CNT-KOUHKNJA "]"
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   BGS100ERR-FILE
           IF      STS-BGS100ERR     =   ZERO
               CLOSE   BGS100ERR-FILE
           ELSE
               OPEN    OUTPUT              BGS100ERR-FILE
      *
               MOVE    WRK-ERRMSG      TO  BGS100ERR-REC
               WRITE   BGS100ERR-REC
               CLOSE   BGS100ERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-ERRMSG      TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 800-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  BGS100P-RC
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理サブ
      *****************************************************************
       800-ORCSJOB-SEC                 SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    BGS100P-JOBID       TO  JOB-JOBID
           MOVE    BGS100P-SHELLID     TO  JOB-SHELLID
           MOVE    "ORCBGS100"         TO  JOB-PGID
           MOVE    BGS100P-SHELLMSG    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA 
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
      *
       800-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理検索処理
      *****************************************************************
       900-SEIKYU-SEL-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-SEIKYU
      *
           INITIALIZE                  RECE10-REC
           MOVE    SPA-HOSPNUM     TO  RECE10-HOSPNUM
           MOVE    BGS100P-SRYYM   TO  RECE10-SKYYM
           IF    ( BGS100P-NYUGAIKBN   =   SPACE )
            OR   ( BGS100P-SOUKATU     =   1     )
               MOVE    "%"         TO  RECE10-NYUGAIKBN
           ELSE
               MOVE    BGS100P-NYUGAIKBN
                                   TO  RECE10-NYUGAIKBN
           END-IF
           MOVE    RECE10-REC      TO  MCPDATA-REC
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 900-SEIKYU-FET-SEC
           ELSE
               MOVE    1           TO  FLG-SEIKYU
               INITIALIZE              RECE10-REC
           END-IF
      *
           .
      *
       900-SEIKYU-SEL-EXT.
           EXIT.
      *****************************************************************
      *    請求管理FETCH処理
      *****************************************************************
       900-SEIKYU-FET-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-SEIKYU
      *
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  RECE10-REC
      *
               IF    ( RECE10-SKYKBN-RECEDEN   =   "1" )
                   MOVE    2       TO  BGS100P-RC
               END-IF
      *
           ELSE
               MOVE    1           TO  FLG-SEIKYU
               INITIALIZE              RECE10-REC
           END-IF
      *
           .
      *
       900-SEIKYU-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者情報取得処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE                      PTINF-REC
           MOVE    RECE10-HOSPNUM      TO  PTINF-HOSPNUM
           MOVE    RECE10-PTID         TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    請求ファイル読込処理
      *****************************************************************
       900-BGSSKY-READ-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-BGSSKY
      *
           INITIALIZE                  BGSSKY-REC
           MOVE    RECE10-NYUGAIKBN
                                   TO  BGSSKY-NYUGAIKBN
      *
           IF    ( WRK-HOKEN       =   CONST-SYAHO
             OR                        CONST-KOKUHO
             OR                        CONST-KOUKI )
               IF    ( BGS100P-SYUKA-HKN (WRK-HOKEN)
                                   =   1 )
                   MOVE    RECE10-RECEKA
                                   TO  BGSSKY-RECEKA
               ELSE
                   MOVE    "00"    TO  BGSSKY-RECEKA
               END-IF
           ELSE
                   MOVE    "00"    TO  BGSSKY-RECEKA
           END-IF
      *
           MOVE    WRK-HOKEN       TO  BGSSKY-HOKEN
           MOVE    RECE10-RECESYUBETU
                                   TO  BGSSKY-RECESBT
           MOVE    RECE10-HKNNUM   TO  BGSSKY-HKNNUM
           READ    BGSSKY-FILE
           INVALID
               MOVE    1           TO  FLG-BGSSKY
           END-READ
      *
           .
      *
       900-BGSSKY-READ-EXT.
           EXIT.
      *****************************************************************
      *    公費ファイル読込処理
      *****************************************************************
       900-BGSKOH-READ-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-BGSKOH
      *
           INITIALIZE                  BGSKOH-REC
           MOVE    RECE10-NYUGAIKBN
                                   TO  BGSKOH-NYUGAIKBN
      *
           IF    ( WRK-HOKEN       =   CONST-SYAHO
             OR                        CONST-KOKUHO
             OR                        CONST-KOUKI )
               IF    ( BGS100P-SYUKA-HKN (WRK-HOKEN)
                                   =   1 )
                   MOVE    RECE10-RECEKA
                                   TO  BGSKOH-RECEKA
               ELSE
                   MOVE    "00"    TO  BGSKOH-RECEKA
               END-IF
           ELSE
                   MOVE    "00"    TO  BGSKOH-RECEKA
           END-IF
           MOVE    WRK-HOKEN       TO  BGSKOH-HOKEN
           MOVE    WRK-KOHKBN      TO  BGSKOH-KOHKBN
           MOVE    WRK-KOHNUM      TO  BGSKOH-KOHNUM
           READ    BGSKOH-FILE
           INVALID
               MOVE    1           TO  FLG-BGSKOH
           END-READ
      *
           .
      *
       900-BGSKOH-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険者別請求ファイル読込処理
      *****************************************************************
       900-BGSHKNJA-READ-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-BGSHKNJA
      *
           INITIALIZE                  BGSHKNJA-REC
           MOVE    RECE10-NYUGAIKBN
                                   TO  BGSHKNJA-NYUGAIKBN
           IF    ( WRK-HOKEN       =   CONST-SYAHO
             OR                        CONST-KOKUHO
             OR                        CONST-KOUKI )
               IF    ( BGS100P-SYUKA-HKN (WRK-HOKEN)
                                   =   1 )
                   MOVE    RECE10-RECEKA
                                   TO  BGSHKNJA-RECEKA
               ELSE
                   MOVE    "00"    TO  BGSHKNJA-RECEKA
               END-IF
           ELSE
                   MOVE    "00"    TO  BGSHKNJA-RECEKA
           END-IF
           MOVE    WRK-HOKEN       TO  BGSHKNJA-HOKEN
           MOVE    RECE10-PREFKBN  TO  BGSHKNJA-PREFKBN
           MOVE    RECE10-HKNJANUM-S
                                   TO  BGSHKNJA-HKNJANUM
           READ    BGSHKNJA-FILE
           INVALID
               MOVE    1           TO  FLG-BGSHKNJA
           END-READ
      *
           .
      *
       900-BGSHKNJA-READ--EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
      *    ＤＢ検索処理（FHETCHを行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
