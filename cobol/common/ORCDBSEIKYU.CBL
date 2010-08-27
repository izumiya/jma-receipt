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
       PROGRAM-ID.             ORCDBSEIKYU.
      *****************************************************************
      *  システム名        : ORCA
      *  サブシステム名    : 
      *  コンポーネント名  : ORCA DB 請求管理アクセスモジュール
      *  返却エラーコード  : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09/02/24    NACL-太田     新規作成
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
           03  FLG-SKYETC          PIC 9(01).
           03  FLG-SKYKOH          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDX-KOH             PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATHNAME            PIC X(64).
           03  WRK-FUNC                PIC X(64).
           03  WRK-TABLE               PIC X(64).
      *
       01  CONST-AREA.
           03  CONST-SRYKBNINF-MAX     PIC 9(03)   VALUE   45.
           03  CONST-KOH-MAX           PIC 9(03)   VALUE   4.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    請求管理
           COPY    "CPRCF010.INC".
      *
      *    請求管理（新）
           COPY    "CPSKYMAIN.INC".
      *
      *    請求管理（公費情報）
           COPY    "CPSKYKOH.INC".
      *
      *    請求管理（レセプト記載外公費情報）
           COPY    "CPSKYETC.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
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
           INITIALIZE                  IDX-AREA
      *
           MOVE    MCP-TABLE           TO  WRK-TABLE
           MOVE    MCP-PATHNAME        TO  WRK-PATHNAME
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-EDIT-SEIKYU-SEC
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
           .
      *
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    請求管理編集処理
      *****************************************************************
       200-EDIT-SEIKYU-SEC             SECTION.
      *
           MOVE    "tbl_seikyu_main"   TO  MCP-TABLE
           MOVE    MCPDATA-REC         TO  RECE10-REC
      *
           EVALUATE    MCP-FUNC
           WHEN    "DBSELECT"
               PERFORM 2001-EDIT-SKYMAIN-SEC
               MOVE    SKYMAIN-REC     TO  MCPDATA-REC
               CALL    "MONFUNC"   USING
                                   MCPAREA
                                   MCPDATA-REC
           WHEN    "DBCLOSECURSOR"
               PERFORM 910-DBCLOSECURSOR-SEC
           WHEN    "DBFETCH"
               CALL    "MONFUNC"   USING
                                   MCPAREA
                                   MCPDATA-REC
               IF    ( MCP-RC  =   ZERO )
                   PERFORM 2001-SKYMAIN-FET-SEC
               END-IF
           WHEN    "DBUPDATE"
               PERFORM 2001-SKYMAIN-UPD-SEC
           WHEN    "DBDELETE"
               PERFORM 2001-SKYMAIN-DEL-SEC
           WHEN    "DBINSERT"
               PERFORM 2001-SKYMAIN-INS-SEC
           END-EVALUATE
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
      *
           .
       200-EDIT-SEIKYU-EXT.
           EXIT.
      *****************************************************************
      *    新請求管理（主）編集処理
      *****************************************************************
       2001-EDIT-SKYMAIN-SEC          SECTION.
      *
           INITIALIZE                          SKYMAIN-REC
           MOVE    RECE10-HOSPNUM          TO  SKYMAIN-HOSPNUM
           MOVE    RECE10-SRYYM            TO  SKYMAIN-SRYYM
           MOVE    RECE10-NYUGAIKBN        TO  SKYMAIN-NYUGAIKBN
           MOVE    RECE10-PTID             TO  SKYMAIN-PTID
           MOVE    RECE10-RECEKA           TO  SKYMAIN-RECEKA
           MOVE    RECE10-TEISYUTUSAKI     TO  SKYMAIN-TEISYUTUSAKI
           MOVE    RECE10-RECESYUBETU      TO  SKYMAIN-RECESYUBETU
           MOVE    RECE10-HKNJANUM         TO  SKYMAIN-HKNJANUM
           MOVE    RECE10-HOJOKBN-KEY      TO  SKYMAIN-HOJOKBN-KEY
           MOVE    RECE10-TEKSTYMD         TO  SKYMAIN-TEKSTYMD
           MOVE    RECE10-PTNUM            TO  SKYMAIN-PTNUM
           MOVE    RECE10-PREFKBN          TO  SKYMAIN-PREFKBN
           MOVE    RECE10-PREFNUM          TO  SKYMAIN-PREFNUM
           MOVE    RECE10-AGE              TO  SKYMAIN-AGE
           MOVE    RECE10-KYURATE          TO  SKYMAIN-KYURATE
           MOVE    RECE10-ZAITAKU          TO  SKYMAIN-ZAITAKU
           MOVE    RECE10-SKYYM            TO  SKYMAIN-SKYYM
           MOVE    RECE10-HENREIYM         TO  SKYMAIN-HENREIYM
           MOVE    RECE10-SKYKBN           TO  SKYMAIN-SKYKBN
           MOVE    RECE10-SKYKBN-RECEDEN   TO  SKYMAIN-SKYKBN-RECEDEN
           MOVE    RECE10-TEISEIKBN        TO  SKYMAIN-TEISEIKBN
           MOVE    RECE10-HKNNUM           TO  SKYMAIN-HKNNUM
           MOVE    RECE10-HKNID            TO  SKYMAIN-HKNID
           MOVE    RECE10-HONKZKKBN        TO  SKYMAIN-HONKZKKBN
           MOVE    RECE10-HOJOKBN          TO  SKYMAIN-HOJOKBN
           MOVE    RECE10-CONTKBN          TO  SKYMAIN-CONTKBN
           MOVE    RECE10-KIGO             TO  SKYMAIN-KIGO
           MOVE    RECE10-NUM              TO  SKYMAIN-NUM
           MOVE    RECE10-RJNHKNNUM        TO  SKYMAIN-RJNHKNNUM
           MOVE    RECE10-RJNHKNID         TO  SKYMAIN-RJNHKNID
           MOVE    RECE10-KOHNUM (1)       TO  SKYMAIN-KOHNUM (1)
           MOVE    RECE10-KOHNUM (2)       TO  SKYMAIN-KOHNUM (2)
           MOVE    RECE10-KOHNUM (3)       TO  SKYMAIN-KOHNUM (3)
           MOVE    RECE10-KOHNUM (4)       TO  SKYMAIN-KOHNUM (4)
           MOVE    RECE10-JNISSU (1)       TO  SKYMAIN-JNISSU
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-SRYKBNINF-MAX )
               MOVE    RECE10-KHNTEN (IDX1)    TO  SKYMAIN-KHNTEN (IDX1)
               MOVE    RECE10-KAISU  (IDX1)    TO  SKYMAIN-KAISU  (IDX1)
               MOVE    RECE10-TENSU  (IDX1 1)  TO  SKYMAIN-TENSU  (IDX1)
           END-PERFORM
           MOVE    RECE10-TOTALTEN(1)      TO  SKYMAIN-TOTALTEN
           MOVE    RECE10-FTNMONEY(1)      TO  SKYMAIN-FTNMONEY
           MOVE    RECE10-FTNMONEY-FTN(1)  TO  SKYMAIN-FTNMONEY-FTN
           MOVE    RECE10-FTNMONEY-KOJ(1)  TO  SKYMAIN-FTNMONEY-KOJ
           MOVE    RECE10-SHOKUJINISSU(1)  TO  SKYMAIN-SHOKUJINISSU
           MOVE    RECE10-SHOKUJIRYOYOHI(1) TO SKYMAIN-SHOKUJIRYOYOHI
           MOVE    RECE10-SHOKUJIFTN(1)    TO  SKYMAIN-SHOKUJIFTN
           MOVE    RECE10-SHOKUJIFTNKBN    TO  SKYMAIN-SHOKUJIFTNKBN
           MOVE    RECE10-YKZFTN(1)        TO  SKYMAIN-YKZFTN
           MOVE    RECE10-TOKKI (1)        TO  SKYMAIN-TOKKI (1)
           MOVE    RECE10-TOKKI (2)        TO  SKYMAIN-TOKKI (2)
           MOVE    RECE10-TOKKI (3)        TO  SKYMAIN-TOKKI (3)
           MOVE    RECE10-TOKKI (4)        TO  SKYMAIN-TOKKI (4)
           MOVE    RECE10-TOKKI (5)        TO  SKYMAIN-TOKKI (5)
           MOVE    RECE10-FLG   (1)        TO  SKYMAIN-FLG   (1)
           MOVE    RECE10-FLG   (2)        TO  SKYMAIN-FLG   (2)
           MOVE    RECE10-FLG   (3)        TO  SKYMAIN-FLG   (3)
           MOVE    RECE10-FLG   (4)        TO  SKYMAIN-FLG   (4)
           MOVE    RECE10-FLG   (5)        TO  SKYMAIN-FLG   (5)
           MOVE    RECE10-RESERVE-AREA     TO  SKYMAIN-RESERVE-AREA
           MOVE    RECE10-HENREI-SIJI-FLG  TO  SKYMAIN-HENREI-SIJI-FLG
           MOVE    RECE10-HKNNUM-S         TO  SKYMAIN-HKNNUM-S
           MOVE    RECE10-HKNJANUM-S       TO  SKYMAIN-HKNJANUM-S
           MOVE    RECE10-CREYMD           TO  SKYMAIN-CREYMD
           MOVE    RECE10-UPDYMD           TO  SKYMAIN-UPDYMD
           MOVE    RECE10-UPHKNJANUM       TO  SKYMAIN-UPHKNJANUM
           MOVE    RECE10-XXPREFNUM        TO  SKYMAIN-XXPREFNUM
           MOVE    RECE10-XXSRYYM          TO  SKYMAIN-XXSRYYM
           MOVE    RECE10-XXSTHKNJANUM     TO  SKYMAIN-XXSTHKNJANUM
           MOVE    RECE10-XXEDHKNJANUM     TO  SKYMAIN-XXEDHKNJANUM
           MOVE    RECE10-XXTEISYUTUSAKI   TO  SKYMAIN-XXTEISYUTUSAKI
           IF    ( RECE10-ETC-KOHNUM (1)   NOT =   SPACE )
             OR  ( RECE10-ETC-KOHNUM (2)   NOT =   SPACE )
             OR  ( RECE10-ETC-KOHNUM (3)   NOT =   SPACE )
             OR  ( RECE10-ETC-KOHNUM (4)   NOT =   SPACE )
             OR  ( RECE10-ETC-YKZFTN (1)       >   ZERO  )
             OR  ( RECE10-ETC-YKZFTN (2)       >   ZERO  )
               MOVE    1                   TO  SKYMAIN-ETCCREATEFLG
           END-IF
      *
           .
       2001-EDIT-SKYMAIN-EXT.
           EXIT.
      *****************************************************************
      *    新請求管理（主）FETCH処理
      *****************************************************************
       2001-SKYMAIN-FET-SEC            SECTION.
      *
           MOVE    MCPDATA-REC             TO  SKYMAIN-REC
      *
           INITIALIZE                          RECE10-REC
           MOVE    SKYMAIN-HOSPNUM         TO  RECE10-HOSPNUM
           MOVE    SKYMAIN-SRYYM           TO  RECE10-SRYYM
           MOVE    SKYMAIN-NYUGAIKBN       TO  RECE10-NYUGAIKBN
           MOVE    SKYMAIN-PTID            TO  RECE10-PTID
           MOVE    SKYMAIN-RECEKA          TO  RECE10-RECEKA
           MOVE    SKYMAIN-TEISYUTUSAKI    TO  RECE10-TEISYUTUSAKI
           MOVE    SKYMAIN-RECESYUBETU     TO  RECE10-RECESYUBETU
           MOVE    SKYMAIN-HKNJANUM        TO  RECE10-HKNJANUM
           MOVE    SKYMAIN-HOJOKBN-KEY     TO  RECE10-HOJOKBN-KEY
           MOVE    SKYMAIN-TEKSTYMD        TO  RECE10-TEKSTYMD
           MOVE    SKYMAIN-PTNUM           TO  RECE10-PTNUM
           MOVE    SKYMAIN-PREFKBN         TO  RECE10-PREFKBN
           MOVE    SKYMAIN-PREFNUM         TO  RECE10-PREFNUM
           MOVE    SKYMAIN-AGE             TO  RECE10-AGE
           MOVE    SKYMAIN-KYURATE         TO  RECE10-KYURATE
           MOVE    SKYMAIN-ZAITAKU         TO  RECE10-ZAITAKU
           MOVE    SKYMAIN-SKYYM           TO  RECE10-SKYYM
           MOVE    SKYMAIN-HENREIYM        TO  RECE10-HENREIYM
           MOVE    SKYMAIN-SKYKBN          TO  RECE10-SKYKBN
           MOVE    SKYMAIN-SKYKBN-RECEDEN  TO  RECE10-SKYKBN-RECEDEN
           MOVE    SKYMAIN-TEISEIKBN       TO  RECE10-TEISEIKBN
           MOVE    SKYMAIN-HKNNUM          TO  RECE10-HKNNUM
           MOVE    SKYMAIN-HKNID           TO  RECE10-HKNID
           MOVE    SKYMAIN-HONKZKKBN       TO  RECE10-HONKZKKBN
           MOVE    SKYMAIN-HOJOKBN         TO  RECE10-HOJOKBN
           MOVE    SKYMAIN-CONTKBN         TO  RECE10-CONTKBN
           MOVE    SKYMAIN-KIGO            TO  RECE10-KIGO
           MOVE    SKYMAIN-NUM             TO  RECE10-NUM
           MOVE    SKYMAIN-RJNHKNNUM       TO  RECE10-RJNHKNNUM
           MOVE    SKYMAIN-RJNHKNID        TO  RECE10-RJNHKNID
           MOVE    SKYMAIN-KOHNUM (1)      TO  RECE10-KOHNUM (1)
           MOVE    SKYMAIN-KOHNUM (2)      TO  RECE10-KOHNUM (2)
           MOVE    SKYMAIN-KOHNUM (3)      TO  RECE10-KOHNUM (3)
           MOVE    SKYMAIN-KOHNUM (4)      TO  RECE10-KOHNUM (4)
           MOVE    SKYMAIN-JNISSU          TO  RECE10-JNISSU (1)
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-SRYKBNINF-MAX )
               MOVE    SKYMAIN-KHNTEN (IDX1)    TO  RECE10-KHNTEN (IDX1)
               MOVE    SKYMAIN-KAISU  (IDX1)    TO  RECE10-KAISU  (IDX1)
               MOVE    SKYMAIN-TENSU  (IDX1)    TO  RECE10-TENSU(IDX1 1)
           END-PERFORM
           MOVE    SKYMAIN-TOTALTEN        TO  RECE10-TOTALTEN(1)
           MOVE    SKYMAIN-FTNMONEY        TO  RECE10-FTNMONEY(1)
           MOVE    SKYMAIN-FTNMONEY-FTN    TO  RECE10-FTNMONEY-FTN(1)
           MOVE    SKYMAIN-FTNMONEY-KOJ    TO  RECE10-FTNMONEY-KOJ(1)
           MOVE    SKYMAIN-SHOKUJINISSU    TO  RECE10-SHOKUJINISSU(1)
           MOVE    SKYMAIN-SHOKUJIRYOYOHI  TO  RECE10-SHOKUJIRYOYOHI(1)
           MOVE    SKYMAIN-SHOKUJIFTN      TO  RECE10-SHOKUJIFTN(1)
           MOVE    SKYMAIN-SHOKUJIFTNKBN   TO  RECE10-SHOKUJIFTNKBN
           MOVE    SKYMAIN-YKZFTN          TO  RECE10-YKZFTN(1)
           MOVE    SKYMAIN-TOKKI (1)       TO  RECE10-TOKKI (1)
           MOVE    SKYMAIN-TOKKI (2)       TO  RECE10-TOKKI (2)
           MOVE    SKYMAIN-TOKKI (3)       TO  RECE10-TOKKI (3)
           MOVE    SKYMAIN-TOKKI (4)       TO  RECE10-TOKKI (4)
           MOVE    SKYMAIN-TOKKI (5)       TO  RECE10-TOKKI (5)
           MOVE    SKYMAIN-FLG   (1)       TO  RECE10-FLG   (1)
           MOVE    SKYMAIN-FLG   (2)       TO  RECE10-FLG   (2)
           MOVE    SKYMAIN-FLG   (3)       TO  RECE10-FLG   (3)
           MOVE    SKYMAIN-FLG   (4)       TO  RECE10-FLG   (4)
           MOVE    SKYMAIN-FLG   (5)       TO  RECE10-FLG   (5)
           MOVE    SKYMAIN-RESERVE-AREA    TO  RECE10-RESERVE-AREA
           MOVE    SKYMAIN-HENREI-SIJI-FLG TO  RECE10-HENREI-SIJI-FLG
           MOVE    SKYMAIN-HKNNUM-S        TO  RECE10-HKNNUM-S
           MOVE    SKYMAIN-HKNJANUM-S      TO  RECE10-HKNJANUM-S
           MOVE    SKYMAIN-CREYMD          TO  RECE10-CREYMD
           MOVE    SKYMAIN-UPDYMD          TO  RECE10-UPDYMD
           MOVE    SKYMAIN-UPHKNJANUM      TO  RECE10-UPHKNJANUM
           MOVE    SKYMAIN-XXPREFNUM       TO  RECE10-XXPREFNUM
           MOVE    SKYMAIN-XXSRYYM         TO  RECE10-XXSRYYM
           MOVE    SKYMAIN-XXSTHKNJANUM    TO  RECE10-XXSTHKNJANUM
           MOVE    SKYMAIN-XXEDHKNJANUM    TO  RECE10-XXEDHKNJANUM
           MOVE    SKYMAIN-XXTEISYUTUSAKI  TO  RECE10-XXTEISYUTUSAKI
      *
           IF    ( SKYMAIN-KOHNUM (1)      NOT =   SPACE )
            OR   ( SKYMAIN-KOHNUM (2)      NOT =   SPACE )
            OR   ( SKYMAIN-KOHNUM (3)      NOT =   SPACE )
            OR   ( SKYMAIN-KOHNUM (4)      NOT =   SPACE )
                   PERFORM 20011-SKYKOH-GET-SEC
           END-IF
      *
           IF    ( SKYMAIN-ETCCREATEFLG    =   1 )
               PERFORM 900-SKYETC-KEY-SEL-SEC
               PERFORM VARYING IDX-KOH FROM    1   BY  1
                       UNTIL ( IDX-KOH >   CONST-KOH-MAX )
                   MOVE    SKYETC-ETC-KOHNUM (IDX-KOH)
                                   TO  RECE10-ETC-KOHNUM      (IDX-KOH)
                   MOVE    SKYETC-ETC-JNISSU (IDX-KOH)
                                   TO  RECE10-ETC-JNISSU      (IDX-KOH)
                   MOVE    SKYETC-ETC-TOTALTEN (IDX-KOH)
                                   TO  RECE10-ETC-TOTALTEN    (IDX-KOH)
                   MOVE    SKYETC-ETC-FTNMONEY (IDX-KOH)
                                   TO  RECE10-ETC-FTNMONEY    (IDX-KOH)
                   MOVE    SKYETC-ETC-FTNMONEY-FTN (IDX-KOH)
                                   TO  RECE10-ETC-FTNMONEY-FTN(IDX-KOH)
                   MOVE    SKYETC-ETC-FTNMONEY-KOJ (IDX-KOH)
                                   TO  RECE10-ETC-FTNMONEY-KOJ(IDX-KOH)
                   MOVE    SKYETC-ETC-YKZFTN (IDX-KOH)
                                   TO  RECE10-ETC-YKZFTN      (IDX-KOH)
                   MOVE    SKYETC-ETC-SHOKUJINISSU (IDX-KOH)
                                   TO  RECE10-ETC-SHOKUJINISSU(IDX-KOH)
                   MOVE    SKYETC-ETC-SHOKUJIRYOYOHI (IDX-KOH)
                                   TO  RECE10-ETC-SHOKUJIRYOYOHI
                                                              (IDX-KOH)
                   MOVE    SKYETC-ETC-SHOKUJIFTN (IDX-KOH)
                                   TO  RECE10-ETC-SHOKUJIFTN  (IDX-KOH)
               END-PERFORM
           END-IF
      *
           MOVE    ZERO                    TO  MCP-RC
      *
           .
       2001-SKYMAIN-FET-EXT.
           EXIT.
      *****************************************************************
      *    請求管理（公費情報）取得処理
      *****************************************************************
       20011-SKYKOH-GET-SEC            SECTION.
      *
           PERFORM 900-SKYKOH-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SKYKOH  NOT =   ZERO )
      *
               COMPUTE IDX2    =   SKYKOH-RENNUM   +   1
      *
               MOVE    SKYKOH-KOHID    TO  RECE10-KOHID (SKYKOH-RENNUM)
               MOVE    SKYKOH-JNISSU   TO  RECE10-JNISSU(IDX2)
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   CONST-SRYKBNINF-MAX )
                   MOVE    SKYKOH-TENSU (IDX1)
                                           TO  RECE10-TENSU (IDX1 IDX2)
               END-PERFORM
               MOVE    SKYKOH-TOTALTEN     TO  RECE10-TOTALTEN (IDX2)
               MOVE    SKYKOH-FTNMONEY     TO  RECE10-FTNMONEY (IDX2)
               MOVE    SKYKOH-FTNMONEY-FTN TO  RECE10-FTNMONEY-FTN(IDX2)
               MOVE    SKYKOH-FTNMONEY-KOJ TO  RECE10-FTNMONEY-KOJ(IDX2)
               MOVE    SKYKOH-SHOKUJINISSU TO  RECE10-SHOKUJINISSU(IDX2)
               MOVE    SKYKOH-SHOKUJIRYOYOHI
                                           TO  RECE10-SHOKUJIRYOYOHI
                                                                  (IDX2)
               MOVE    SKYKOH-SHOKUJIFTN   TO  RECE10-SHOKUJIFTN  (IDX2)
               MOVE    SKYKOH-YKZFTN       TO  RECE10-YKZFTN      (IDX2)
      *
               PERFORM 900-SKYKOH-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_seikyu_koh"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-SKYKOH-GET-EXT.
           EXIT.
      *****************************************************************
      *    新請求管理（主）更新処理
      *****************************************************************
       2001-SKYMAIN-UPD-SEC            SECTION.
      *
           PERFORM 2001-EDIT-SKYMAIN-SEC
      *
           PERFORM 2001-SKYKOH-INS-SEC
      *
           IF    ( MCP-RC          =    ZERO )
               PERFORM 2001-SKYETC-INS-SEC
           END-IF
      *
           IF    ( MCP-RC          =    ZERO )
               MOVE    SKYMAIN-REC         TO  MCPDATA-REC
               MOVE    "tbl_seikyu_main"   TO  MCP-TABLE
               MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
           END-IF
      *
           .
       2001-SKYMAIN-UPD-EXT.
           EXIT.
      *****************************************************************
      *    新請求管理（主）追加処理
      *****************************************************************
       2001-SKYMAIN-INS-SEC           SECTION.
      *
           PERFORM 2001-EDIT-SKYMAIN-SEC
      *
           PERFORM 2001-SKYKOH-INS-SEC
      *
           IF    ( MCP-RC          =    ZERO )
               PERFORM 2001-SKYETC-INS-SEC
           END-IF
      *
           IF    ( MCP-RC          =    ZERO )
               MOVE    SKYMAIN-REC         TO  MCPDATA-REC
               MOVE    "tbl_seikyu_main"   TO  MCP-TABLE
               MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
           END-IF
      *
           .
       2001-SKYMAIN-INS-EXT.
           EXIT.
      *****************************************************************
      *    新請求管理（主）削除処理
      *****************************************************************
       2001-SKYMAIN-DEL-SEC           SECTION.
      *
           PERFORM 2001-EDIT-SKYMAIN-SEC
      *
           PERFORM 2001-SKYKOH-DEL-SEC
      *
           IF    ( MCP-RC          =    ZERO )
               PERFORM 2001-SKYETC-DEL-SEC
           END-IF
      *
           IF    ( MCP-RC          =    ZERO )
               MOVE    SKYMAIN-REC        TO  MCPDATA-REC
               MOVE    "tbl_seikyu_main"   TO  MCP-TABLE
               MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
           END-IF
      *
           .
       2001-SKYMAIN-INS-EXT.
           EXIT.
      *****************************************************************
      *    請求管理（公費情報）削除処理
      *****************************************************************
       2001-SKYKOH-DEL-SEC              SECTION.
      *
           INITIALIZE                      SKYKOH-REC
           PERFORM 9001-SKYKOH-KEY-EDIT-SEC
           IF    ( MCP-PATHNAME    =   "del1" OR "del3" OR "del4" 
                                    OR "del6" OR "del7" )
               CONTINUE
           ELSE
               MOVE    "del"           TO   MCP-PATHNAME
           END-IF
      *
           MOVE    SKYKOH-REC          TO  MCPDATA-REC
           MOVE    "tbl_seikyu_koh"    TO  MCP-TABLE
           PERFORM 910-DBDELETE-SEC
      *
           .
       2001-SKYKOH-DEL-EXT.
           EXIT.
      *****************************************************************
      *    請求管理（公費情報）追加処理
      *****************************************************************
       2001-SKYKOH-INS-SEC              SECTION.
      *
           MOVE    "del"           TO   MCP-PATHNAME
           PERFORM 2001-SKYKOH-DEL-SEC
      *
           PERFORM VARYING IDX-KOH      FROM 1 BY  1
                   UNTIL ( IDX-KOH      >    CONST-KOH-MAX )
                    OR   ( MCP-RC  NOT  =   ZERO )
      *
               IF    ( SKYMAIN-KOHNUM (IDX-KOH)    NOT =   SPACE )
                   PERFORM 20011-SKYKOH-EDIT-SEC
                   MOVE    SKYKOH-REC          TO  MCPDATA-REC
                   MOVE    "tbl_seikyu_koh"    TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
               END-IF
      *
           END-PERFORM
      *
           .
       2001-SKYKOH-INS-EXT.
           EXIT.
      *****************************************************************
      *    請求管理（公費情報）編集処理
      *****************************************************************
       20011-SKYKOH-EDIT-SEC           SECTION.
      *
           COMPUTE IDX2    =   IDX-KOH +   1
      *
           INITIALIZE                          SKYKOH-REC
           PERFORM 9001-SKYKOH-KEY-EDIT-SEC
      *
           MOVE    IDX-KOH                 TO  SKYKOH-RENNUM
           MOVE    RECE10-KOHNUM(IDX-KOH)  TO  SKYKOH-KOHNUM
           MOVE    RECE10-KOHID (IDX-KOH)  TO  SKYKOH-KOHID
           MOVE    RECE10-JNISSU(IDX2)     TO  SKYKOH-JNISSU
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-SRYKBNINF-MAX )
               MOVE    RECE10-TENSU (IDX1 IDX2)
                                           TO  SKYKOH-TENSU (IDX1)
           END-PERFORM
           MOVE    RECE10-TOTALTEN(IDX2)   TO  SKYKOH-TOTALTEN
           MOVE    RECE10-FTNMONEY (IDX2)  TO  SKYKOH-FTNMONEY
           MOVE    RECE10-FTNMONEY-FTN (IDX2)
                                           TO  SKYKOH-FTNMONEY-FTN
           MOVE    RECE10-FTNMONEY-KOJ (IDX2)
                                           TO  SKYKOH-FTNMONEY-KOJ 
           MOVE    RECE10-SHOKUJINISSU (IDX2)
                                           TO  SKYKOH-SHOKUJINISSU 
           MOVE    RECE10-SHOKUJIRYOYOHI(IDX2)
                                           TO  SKYKOH-SHOKUJIRYOYOHI
           MOVE    RECE10-SHOKUJIFTN   (IDX2)
                                           TO  SKYKOH-SHOKUJIFTN
           MOVE    RECE10-YKZFTN       (IDX2)
                                           TO  SKYKOH-YKZFTN
      *
           .
       20011-SKYKOH-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    請求管理（レセプト記載外公費情報）削除処理
      *****************************************************************
       2001-SKYETC-DEL-SEC             SECTION.
      *
           INITIALIZE                  SKYETC-REC
           PERFORM 9001-SKYETC-KEY-EDIT-SEC
           IF    ( MCP-PATHNAME    =   "del1" OR "del3" OR "del4" 
                                    OR "del6" OR "del7" )
               CONTINUE
           ELSE
               MOVE    "del"           TO   MCP-PATHNAME
           END-IF
      *
           MOVE    SKYETC-REC          TO  MCPDATA-REC
           MOVE    "tbl_seikyu_etc"    TO  MCP-TABLE
           PERFORM 910-DBDELETE-SEC
      *
           .
       2001-SKYETC-DEL-EXT.
           EXIT.
      *****************************************************************
      *    新請求管理（レセプト記載外公費情報）追加処理
      *****************************************************************
       2001-SKYETC-INS-SEC             SECTION.
      *
           MOVE    "del"           TO   MCP-PATHNAME
           PERFORM 2001-SKYETC-DEL-SEC
      *
           IF    ( SKYMAIN-ETCCREATEFLG    =   1 )
      *
               INITIALIZE                  SKYETC-REC
               PERFORM 9001-SKYETC-KEY-EDIT-SEC
               PERFORM VARYING IDX-KOH FROM    1   BY  1
                       UNTIL ( IDX-KOH >   CONST-KOH-MAX )
                   MOVE    RECE10-ETC-KOHNUM      (IDX-KOH)
                                   TO  SKYETC-ETC-KOHNUM       (IDX-KOH)
                   MOVE    RECE10-ETC-JNISSU      (IDX-KOH)
                                   TO  SKYETC-ETC-JNISSU       (IDX-KOH)
                   MOVE    RECE10-ETC-TOTALTEN    (IDX-KOH)
                                   TO  SKYETC-ETC-TOTALTEN     (IDX-KOH)
                   MOVE    RECE10-ETC-FTNMONEY    (IDX-KOH)
                                   TO  SKYETC-ETC-FTNMONEY     (IDX-KOH)
                   MOVE    RECE10-ETC-FTNMONEY-FTN(IDX-KOH)
                                   TO  SKYETC-ETC-FTNMONEY-FTN (IDX-KOH)
                   MOVE    RECE10-ETC-FTNMONEY-KOJ(IDX-KOH)
                                   TO  SKYETC-ETC-FTNMONEY-KOJ (IDX-KOH)
                   MOVE    RECE10-ETC-YKZFTN      (IDX-KOH)
                                   TO  SKYETC-ETC-YKZFTN       (IDX-KOH)
                   MOVE    RECE10-ETC-SHOKUJINISSU(IDX-KOH)
                                   TO  SKYETC-ETC-SHOKUJINISSU (IDX-KOH)
                   MOVE    RECE10-ETC-SHOKUJIRYOYOHI (IDX-KOH)
                                   TO  SKYETC-ETC-SHOKUJIRYOYOHI
                                                               (IDX-KOH)
                   MOVE    RECE10-ETC-SHOKUJIFTN (IDX-KOH)
                                   TO  SKYETC-ETC-SHOKUJIFTN   (IDX-KOH)
               END-PERFORM
               MOVE    SKYETC-REC          TO  MCPDATA-REC
               MOVE    "tbl_seikyu_etc"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
      *
           END-IF
      *
           .
       2001-SKYETC-INS-EXT.
           EXIT.
      *****************************************************************
      *    請求管理（公費情報）検索処理
      *****************************************************************
       900-SKYKOH-KEY2-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SKYKOH
      *
           INITIALIZE                      SKYKOH-REC
           PERFORM 9001-SKYKOH-KEY-EDIT-SEC
           MOVE    SKYKOH-REC          TO  MCPDATA-REC
           MOVE    "tbl_seikyu_koh"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SKYKOH-REC
           ELSE
               MOVE    1               TO  FLG-SKYKOH
               INITIALIZE                  SKYKOH-REC
           END-IF
      *
           .
       900-SKYKOH-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    請求管理（公費情報）キー情報編集処理
      *****************************************************************
       9001-SKYKOH-KEY-EDIT-SEC        SECTION.
      *
           MOVE    SKYMAIN-HOSPNUM     TO  SKYKOH-HOSPNUM
           MOVE    SKYMAIN-SRYYM       TO  SKYKOH-SRYYM
           MOVE    SKYMAIN-NYUGAIKBN   TO  SKYKOH-NYUGAIKBN
           MOVE    SKYMAIN-PTID        TO  SKYKOH-PTID
           MOVE    SKYMAIN-RECEKA      TO  SKYKOH-RECEKA
           MOVE    SKYMAIN-TEISYUTUSAKI TO SKYKOH-TEISYUTUSAKI
           MOVE    SKYMAIN-RECESYUBETU TO  SKYKOH-RECESYUBETU
           MOVE    SKYMAIN-HKNJANUM    TO  SKYKOH-HKNJANUM
           MOVE    SKYMAIN-HOJOKBN-KEY TO  SKYKOH-HOJOKBN-KEY
           MOVE    SKYMAIN-TEKSTYMD    TO  SKYKOH-TEKSTYMD
           MOVE    SKYMAIN-XXTEISYUTUSAKI
                                       TO  SKYKOH-XXTEISYUTUSAKI
      *
           .
       9001-SKYKOH-KEY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    請求管理（公費情報）FETCH処理
      *****************************************************************
       900-SKYKOH-KEY2-FET-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-SKYKOH
      *
           MOVE    "tbl_seikyu_koh"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SKYKOH-REC
           ELSE
               MOVE    1               TO  FLG-SKYKOH
               INITIALIZE                  SKYKOH-REC
           END-IF
      *
           .
       900-SKYKOH-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    請求管理（レセプト記載外公費情報）検索処理
      *****************************************************************
       900-SKYETC-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SKYETC
      *
           INITIALIZE                      SKYETC-REC
           PERFORM 9001-SKYETC-KEY-EDIT-SEC
           MOVE    SKYETC-REC          TO  MCPDATA-REC
           MOVE    "tbl_seikyu_etc"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SKYETC-REC
           ELSE
               MOVE    1               TO  FLG-SKYETC
               INITIALIZE                  SKYETC-REC
           END-IF
      *
           MOVE    "tbl_seikyu_etc"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SKYETC-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    請求管理（レセプト記載外公費情報）キー情報編集処理
      *****************************************************************
       9001-SKYETC-KEY-EDIT-SEC        SECTION.
      *
           MOVE    SKYMAIN-HOSPNUM     TO  SKYETC-HOSPNUM
           MOVE    SKYMAIN-SRYYM       TO  SKYETC-SRYYM
           MOVE    SKYMAIN-NYUGAIKBN   TO  SKYETC-NYUGAIKBN
           MOVE    SKYMAIN-PTID        TO  SKYETC-PTID
           MOVE    SKYMAIN-RECEKA      TO  SKYETC-RECEKA
           MOVE    SKYMAIN-TEISYUTUSAKI TO SKYETC-TEISYUTUSAKI
           MOVE    SKYMAIN-RECESYUBETU TO  SKYETC-RECESYUBETU
           MOVE    SKYMAIN-HKNJANUM    TO  SKYETC-HKNJANUM
           MOVE    SKYMAIN-HOJOKBN-KEY TO  SKYETC-HOJOKBN-KEY
           MOVE    SKYMAIN-TEKSTYMD    TO  SKYETC-TEKSTYMD
           MOVE    SKYMAIN-XXTEISYUTUSAKI
                                       TO  SKYETC-XXTEISYUTUSAKI
           .
       9001-SKYETC-KEY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                   MCPAREA
                                   MCPDATA-REC
      *
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                   MCPAREA
                                   MCPDATA-REC
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC       SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                   MCPAREA
                                   MCPDATA-REC
      *
           .
       990-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                   MCPAREA
                                   MCPDATA-REC
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
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                   MCPAREA
                                   MCPDATA-REC
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
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                   MCPAREA
                                   MCPDATA-REC
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
