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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORAPI012S1.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール(サンプル)
      *  コンポーネント名  : 患者基本情報設定
      *  管理者            :
      *  作成日付    作業者        記述
      *  09/04/07    NACL−竹田　　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *
      *****************************************************************
      *
       ENVIRONMENT           DIVISION.
       CONFIGURATION         SECTION.
       INPUT-OUTPUT          SECTION.
      *
       FILE-CONTROL.
      *    BLOB格納情報ファイル(CSV)
           SELECT  REQCSV-FILE     ASSIGN REQCSV
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-REQCSV.
       DATA                DIVISION.
       FILE                    SECTION.
      *    BLOB格納情報ファイル(CSV)
       FD  REQCSV-FILE.
       01  REQCSV-RECORD.
           03  REQCSV-INFO             PIC X(5000).
      *
       WORKING-STORAGE             SECTION.
       01  REQCSV                  PIC X(512).
       01  STS-AREA.
           03  STS-REQCSV          PIC X(02).
      *    スパ領域
           COPY    "COMMON-SPA".
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDY1                PIC 9(04).
           03  IDX1                PIC 9(04).
           03 TBL-MAX              PIC 9(01).
      *    公費並替え用ワーク
           03  WRK-KOHI-AREA.
      *    並び替え前
               05  WRK-KOHI-TBL        OCCURS  4.
                   07  WRK-KOHID-O     PIC  9(10).
                   07  WRK-KOHHKNNUM-O PIC  X(03).
                   07  WRK-KOHMEI-O    PIC  X(50).
                   07  WRK-GAIJGNKBN-O PIC  X(01).
      *    並び替え後
           03  WRK-KOHI-AREA-X.
               05  WRK-KOHI-TBL-X      OCCURS  4.
                   07  WRK-KOHID       PIC  9(10).
                   07  WRK-KOHHKNNUM   PIC  X(03).
                   07  WRK-KOHMEI      PIC  X(50).
                   07  WRK-GAIJGNKBN   PIC  X(01).
      *    ＳＯＲＴ用ワーク
           03  WRK-KOHI.
               05  WRK-KOHID-W         PIC  9(10).
               05  WRK-KOHHKNNUM-W     PIC  X(03).
               05  WRK-KOHMEI-W        PIC  X(50).
               05  WRK-GAIJGNKBN-W     PIC  X(01).
      *
       01  WRK-CONST-AREA.
           03  WRK-CONST-TABLE.
               05  FILLER              PIC X(20)   VALUE   
                  "ああああああああああ".
               05  FILLER              PIC X(20)   VALUE   
                  "いいいいいいいいいい".
               05  FILLER              PIC X(20)   VALUE   
                  "うううううううううう".
               05  FILLER              PIC X(20)   VALUE   
                  "ええええええええええ".
               05  FILLER              PIC X(20)   VALUE   
                  "おおおおおおおおおお".
       01  WRK-SRT-AREA.
           03  WRK-SRT-TABLE.
               05  FILLER              PIC X(03)   VALUE   "027".
               05  FILLER              PIC X(03)   VALUE   "013".
               05  FILLER              PIC X(03)   VALUE   "014".
               05  FILLER              PIC X(03)   VALUE   "018".
               05  FILLER              PIC X(03)   VALUE   "029".
               05  FILLER              PIC X(03)   VALUE   "010".
               05  FILLER              PIC X(03)   VALUE   "011".
               05  FILLER              PIC X(03)   VALUE   "020".
               05  FILLER              PIC X(03)   VALUE   "021".
               05  FILLER              PIC X(03)   VALUE   "015".
               05  FILLER              PIC X(03)   VALUE   "016".
               05  FILLER              PIC X(03)   VALUE   "024".
               05  FILLER              PIC X(03)   VALUE   "022".
               05  FILLER              PIC X(03)   VALUE   "028".
               05  FILLER              PIC X(03)   VALUE   "017".
               05  FILLER              PIC X(03)   VALUE   "079".
               05  FILLER              PIC X(03)   VALUE   "019".
               05  FILLER              PIC X(03)   VALUE   "023".
               05  FILLER              PIC X(03)   VALUE   "051".
               05  FILLER              PIC X(03)   VALUE   "091".
               05  FILLER              PIC X(03)   VALUE   "052".
               05  FILLER              PIC X(03)   VALUE   "053".
               05  FILLER              PIC X(03)   VALUE   "066".
               05  FILLER              PIC X(03)   VALUE   "012".
      *********05  FILLER              PIC X(03)   VALUE   "972".
      *********05  FILLER              PIC X(03)   VALUE   "974".
           03  WRK-SRT-TABLE-R         REDEFINES   WRK-SRT-TABLE.
               05  WRK-SRT-KOHHKNNUM   PIC X(03)   OCCURS  24.
           03  WRK-SRT-KOHHKNNUM-MAX   PIC 9(02)   VALUE   24.
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-SYSYMD-8.
               05  WRK-SYSYY           PIC 9(02).
               05  WRK-SYSYMD          PIC 9(06).
           03  WRK-DATE                PIC 9(06).
           03  WRK-PTNUM               PIC X(20).
      *
       01  WORK-AREA.
           03  WK-MSG                  PIC X(40).
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTKOHINF            PIC 9(01).
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-SYSUSER             PIC 9(01).
           03  FLG-REQCSV              PIC 9(01).
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
           COPY    "CPORCSCOMMON.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム基本
       01  SYSBASE-REC.
           COPY    "CPSYSBASE.INC".
      *
      *    システムユーザー
       01  SYSUSER-REC.
           COPY    "CPSYSUSER.INC".
      *
      *    職員情報
           COPY  "CPSK1010.INC".
      *    患者マスタ−
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
           COPY    "CPPATIENTXMLRES.INC".
      *
      *    COPY    "CPPATIENTXMLREQ.INC".
      *
           COPY    "ORCA-BLOB".
      *
           COPY    "MCPDATA.INC".
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
       01  SCRAREA.
           COPY    "P02.INC".
           COPY    "P02A.INC".
           COPY    "P02D.INC".
           COPY    "P02E.INC".
           COPY    "P02F.INC".
           COPY    "P014.INC".
           COPY    "P015.INC".
           COPY    "P98.INC".
           COPY    "P981.INC".
           COPY    "P0201.INC".
           COPY    "P982.INC".
           COPY    "P100.INC".
           COPY    "P02X.INC".
           COPY    "P02S.INC".
           COPY    "P02Q.INC".
           COPY    "P02Q1.INC".
           COPY    "P02Y.INC".
           COPY    "P02Y1.INC".
           COPY    "P02G.INC".
           COPY    "P02G2.INC".
           COPY    "P02K.INC".
           COPY    "PERR.INC".
           COPY    "PID1.INC".
           COPY    "CPPTAPIINF.INC".
      **************************************************************************
       PROCEDURE           DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
       000-MAIN                SECTION.
           DISPLAY   "********************"
           DISPLAY   "* patient res start*"
           DISPLAY   "********************"
           DISPLAY "PTAPIREQ-METHOD = " PTAPIREQ-METHOD
      *
           ACCEPT  WRK-SYSYMD    FROM DATE
           MOVE    20                  TO  WRK-SYSYY
      *
           INITIALIZE        PTNUM-REC
                             IDX-AREA
                             WK-MSG
                             XML-PATIENTINFORES
                             SPA-AREA
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Patient Info"       TO  PTRES-RESKEY
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
           MOVE    SMCNDATE-YMD(1:4)   TO  
                                       PTRES-INFORMATION-DATE(1:4)
           MOVE    "-"                 TO  
                                       PTRES-INFORMATION-DATE(5:1)
           MOVE    SMCNDATE-YMD(5:2)   TO  
                                       PTRES-INFORMATION-DATE(6:2)
           MOVE    "-"                 TO  
                                       PTRES-INFORMATION-DATE(8:1)
           MOVE    SMCNDATE-YMD(7:2)   TO  
                                       PTRES-INFORMATION-DATE(9:2)
           MOVE    SMCNDATE-HMS(1:2)   TO  
                                       PTRES-INFORMATION-TIME(1:2)
           MOVE    ":"                 TO  
                                       PTRES-INFORMATION-TIME(3:1)
           MOVE    SMCNDATE-HMS(3:2)   TO  
                                       PTRES-INFORMATION-TIME(4:2)
           MOVE    ":"                 TO  
                                       PTRES-INFORMATION-TIME(6:1)
           MOVE    SMCNDATE-HMS(5:2)   TO  
                                       PTRES-INFORMATION-TIME(7:2)
      *    医療機関識別番号セット SPA-HOSPNUM
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "99"             TO  PTRES-API-RESULT
               MOVE   "ユーザID未登録" TO  PTRES-API-RESULT-MSG
             IF    PTAPIREQ-CONTENT-TYPE =   "application/xml"
                   PERFORM 1000-PTXMLMAKE-SEC
             ELSE
                   PERFORM 1000-PTCSVMAKE-SEC
             END-IF
               GO  TO  000-MAIN-EXIT
           END-IF
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
      *
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M01"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
      *
      *    IF      APIREQ-CONTENT-TYPE =   "application/xml"
      *        PERFORM 1000-PTIXMLREAD-SEC
      *    ELSE
      *        PERFORM 1000-PTICSVREAD-SEC
      *    END-IF
      *    患者情報の取得
           DISPLAY "PTAPIREQ-CLASS = " PTAPIREQ-CLASS
           MOVE    PTAPIREQ-CLASS      TO  WRK-PTNUM
           IF      PTRES-API-RESULT    =  SPACE
               PERFORM 100-PATIENT-GET-SEC
           END-IF
      *
      *    IF      APIREQ-CONTENT-TYPE =   "application/xml"
                   PERFORM 1000-PTXMLMAKE-SEC
      *    ELSE
      *            PERFORM 1000-PTCSVMAKE-SEC
      *    END-IF
      *
           DISPLAY   "********************"
           DISPLAY   "* patient res end  *"
           DISPLAY   "********************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *****************************************************************
      *    医療機関識別番号設定処理
      *****************************************************************
       3002-SUPERVISOR-SET-SEC       SECTION.
      *
           INITIALIZE                      SYSUSER-REC
           MOVE    SPA-OPID            TO  SYSUSER-USERID
           MOVE    SYSUSER-REC         TO  MCPDATA-REC
           MOVE    "tbl_sysuser"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sysuser"   TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-SYSUSER-READ-SEC
           ELSE
               INITIALIZE                  SYSUSER-REC
               MOVE    1               TO  FLG-SYSUSER
           END-IF
           MOVE    "tbl_sysuser"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYSUSER         =   ZERO
      *        医療機関識別番号
               IF      SYSUSER-HOSPNUM     =   SPA-HOSPNUM
      *            スーパーバイザー
                   MOVE    SYSUSER-SUPERVISOR  TO  SPA-SUPERVISOR
               END-IF
           ELSE
               MOVE    "1002"              TO  SPA-ERRCD
           END-IF
      *
           MOVE    SPACE               TO  SPA-NOCHK-HOSPNUM
      *
           .
       3002-SUPERVISOR-SET-EXT.
           EXIT.
      *****************************************************************
      *    システム基本読込処理
      *****************************************************************
       900-SYSUSER-READ-SEC            SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYSUSER-REC
               MOVE    ZERO            TO  FLG-SYSUSER
           ELSE
               MOVE    1               TO  FLG-SYSUSER
           END-IF
           .
       900-SYSUSER-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSV情報から内容を取り出す
      *****************************************************************
       1000-PTICSVREAD-SEC   SECTION.
      *
           MOVE   "BLOBEXPORT"         TO  MCP-FUNC
           MOVE    "blob"              TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    PTAPI-BODY          TO  ORCA-BLOB-OBJECT
      *
           STRING  "/tmp/"             DELIMITED  BY  SPACE
                   MCP-TERM            DELIMITED  BY  SPACE
                   ".csv"              DELIMITED  BY  SPACE
                                       INTO   ORCA-BLOB-FILE
           END-STRING
      *
           CALL   "ORCDBMAIN"          USING
                                       MCPAREA
                                       ORCA-BLOB
                                       SPA-AREA
           IF (MCP-RC = ZERO OR 1)
               CONTINUE
           ELSE
               DISPLAY "BLOBEXPORT failure"
           END-IF
      *
           MOVE    ORCA-BLOB-FILE      TO  REQCSV
      *
           OPEN    INPUT     REQCSV-FILE
      *
           IF      STS-REQCSV          =   ZERO
               DISPLAY "FILE OPEN OK"
           ELSE
               DISPLAY "FILE OPEN NG " STS-REQCSV
           END-IF
      *
           READ    REQCSV-FILE
               AT  END
                   MOVE    1               TO  FLG-REQCSV
               NOT AT  END
                   MOVE    ZERO            TO  FLG-REQCSV
           END-READ
      *
           UNSTRING    REQCSV-RECORD   DELIMITED  BY  ","
                                       INTO    WRK-PTNUM
           END-UNSTRING
           MOVE    1                   TO  IDX
           PERFORM UNTIL      (IDX           >    20    ) 
               IF  WRK-PTNUM(IDX:1)    = LOW-VALUE
                   MOVE  SPACE         TO  WRK-PTNUM(IDX:1)
               END-IF
               ADD     1               TO  IDX
           END-PERFORM 
      *
           CLOSE             REQCSV-FILE
      *
           .
       1000-PTICSVREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本情報取得
      *****************************************************************
       100-PATIENT-GET-SEC   SECTION.
      *
      *    患者番号取得
           INITIALIZE                  ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    WRK-PTNUM           TO  SPTID-PTNUM
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
      *
           MOVE    ZERO                TO  PTRES-API-RESULT
           MOVE    SPTID-PTNUM         TO  PTRES-ID
           PERFORM 1001-PTINF-GET-SEC
           IF      MCP-RC              =   ZERO
               MOVE  PTINF-NAME        TO  PTRES-NAME
               MOVE  PTINF-KANANAME    TO  PTRES-KANANAME
               MOVE  PTINF-BIRTHDAY(1:4)   TO  
                                       PTRES-BIRTHDAY(1:4)
               MOVE  "-"                   TO  
                                       PTRES-BIRTHDAY(5:1)
               MOVE  PTINF-BIRTHDAY(5:2)   TO  
                                       PTRES-BIRTHDAY(6:2)
               MOVE  "-"                   TO  
                                       PTRES-BIRTHDAY(8:1)
               MOVE  PTINF-BIRTHDAY(7:2)   TO  
                                       PTRES-BIRTHDAY(9:2)
               MOVE  PTINF-SEX         TO  PTRES-SEX
               MOVE  PTINF-HOME-POST   TO  PTRES-HOME-ZIP-CODE
               MOVE  PTINF-HOME-ADRS   TO  PTRES-HOME-ADDRESSES1
               MOVE  PTINF-HOME-BANTI  TO  PTRES-HOME-ADDRESSES2
               MOVE  PTINF-HOME-TEL1   TO  PTRES-HOME-HPONE1
               MOVE  PTINF-HOME-TEL2   TO  PTRES-HOME-HPONE2
      *        MOVE  WRK-CONST-AREA    TO  PTRES-HOME-ADDRESSES
               PERFORM 1001-PTHKN-GET-SEC
           ELSE
               MOVE   "01"             TO  PTRES-API-RESULT
               MOVE   "該当患者なし"   TO  PTRES-API-RESULT-MSG
           END-IF
      *
           .
       100-PATIENT-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       1000-PTXMLMAKE-SEC   SECTION.
      *
           MOVE   "XMLOPEN"            TO  MCP-FUNC
           MOVE    "xml_patientinfores"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE   1                    TO  PTRES-MODE
           CALL   "ORCDBMAIN"          USING
                                       MCPAREA
                                       XML-PATIENTINFORES
                                       SPA-AREA
           IF (MCP-RC = ZERO OR 1)
               DISPLAY "XMLOPEN OK      = " MCP-RC
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE   "XMLWRITE"           TO  MCP-FUNC
           MOVE    "xml_patientinfores"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"          USING
                                       MCPAREA
                                       XML-PATIENTINFORES
                                       SPA-AREA
           IF (MCP-RC = ZERO OR 1)
               DISPLAY "XMLWRITE OK      = " MCP-RC
               CONTINUE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           MOVE   "XMLCLOSE"           TO  MCP-FUNC
           MOVE    "xml_patientinfores"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"          USING
                                       MCPAREA
                                       XML-PATIENTINFORES
                                       SPA-AREA
           IF (MCP-RC = ZERO OR 1)
               DISPLAY "XMLCLOSE OK     = " MCP-RC
               MOVE  PTRES-OBJECT      TO  PTAPI-BODY
               MOVE  "application/xml" TO  PTAPIREQ-CONTENT-TYPE
           ELSE
               DISPLAY "XMLCLOSE failure = " MCP-RC
               DISPLAY "patient res xml write failure "
      * CALL    "coblog"    USING   WRK-ERRMSG
           END-IF
      *
           .
       1000-PTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSV情報書き込み処理
      *****************************************************************
       1000-PTCSVMAKE-SEC   SECTION.
      *
      *
           STRING  "/tmp/"             DELIMITED  BY  SPACE
                   MCP-TERM            DELIMITED  BY  SPACE
                   ".csv"              DELIMITED  BY  SPACE
                                       INTO   REQCSV
           END-STRING
      *
           MOVE    REQCSV              TO     ORCA-BLOB-FILE 
           OPEN    OUTPUT    REQCSV-FILE
           MOVE    SPACE               TO    REQCSV-RECORD
           STRING  PTRES-INFORMATION-DATE    DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INFORMATION-TIME    DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-API-RESULT          DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-API-RESULT-MSG      DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-NAME                DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-KANANAME            DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-BIRTHDAY            DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-SEX                 DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-HOME-ZIP-CODE       DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-HOME-ADDRESSES1     DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-HOME-ADDRESSES2     DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-HOME-HPONE1         DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-HOME-HPONE2         DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-INSURANCE-CLASS(01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-INSURANCE-NUMBER(01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-INSURANCE-NAME(01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-MARK(01)       DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-NUMBER(01)     DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-CONTINUATION(01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-ASSISTANCE(01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-FAMILY(01)     DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-POLICYHOLDER(01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-START-DATE(01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-END-DATE(01)   DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-NUMBER(01 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-NAME(01 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-PROVIDER-NUMBER(01 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-RECIPIENT-NUMBER(01 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-START-DATE(01 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-END-DATE(01 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-NUMBER(01 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-NAME(01 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-PROVIDER-NUMBER(01 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-RECIPIENT-NUMBER(01 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-START-DATE(01 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-END-DATE(01 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-NUMBER(01 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-NAME(01 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-PROVIDER-NUMBER(01 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-RECIPIENT-NUMBER(01 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-START-DATE(01 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-END-DATE(01 03)
                                             DELIMITED BY SPACE
      *
      *
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-INSURANCE-CLASS(02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-INSURANCE-NUMBER(02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-INSURANCE-NAME(02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-MARK(02)       DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-NUMBER(02)     DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-CONTINUATION(02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-ASSISTANCE(02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-FAMILY(02)     DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-POLICYHOLDER(02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-START-DATE(02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-END-DATE(02)   DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-NUMBER(02 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-NAME(02 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-PROVIDER-NUMBER(02 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-RECIPIENT-NUMBER(02 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-START-DATE(02 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-END-DATE(02 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-NUMBER(02 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-NAME(02 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-PROVIDER-NUMBER(02 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-RECIPIENT-NUMBER(02 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-START-DATE(02 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-END-DATE(02 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-NUMBER(02 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-NAME(02 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-PROVIDER-NUMBER(02 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-RECIPIENT-NUMBER(02 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-START-DATE(02 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-END-DATE(02 03)
                                             DELIMITED BY SPACE
      *
      *
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-INSURANCE-CLASS(03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-INSURANCE-NUMBER(03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-INSURANCE-NAME(03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-MARK(03)       DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-NUMBER(03)     DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-CONTINUATION(03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-ASSISTANCE(03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-FAMILY(03)     DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-POLICYHOLDER(03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-START-DATE(03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-END-DATE(03)   DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-NUMBER(03 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-NAME(03 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-PROVIDER-NUMBER(03 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-RECIPIENT-NUMBER(03 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-START-DATE(03 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-END-DATE(03 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-NUMBER(03 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-NAME(03 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-PROVIDER-NUMBER(03 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-RECIPIENT-NUMBER(03 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-START-DATE(03 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-END-DATE(03 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-NUMBER(03 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-NAME(03 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-PROVIDER-NUMBER(03 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-RECIPIENT-NUMBER(03 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-START-DATE(03 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-END-DATE(03 03)
                                             DELIMITED BY SPACE
      *
      *
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-INSURANCE-CLASS(04)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-INSURANCE-NUMBER(04)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-INSURANCE-NAME(04)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-MARK(04)       DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-NUMBER(04)     DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-CONTINUATION(04)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-ASSISTANCE(04)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-FAMILY(04)     DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-POLICYHOLDER(04)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-START-DATE(04)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-MAIN-END-DATE(04)   DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-NUMBER(04 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-NAME(04 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-PROVIDER-NUMBER(04 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-RECIPIENT-NUMBER(04 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-START-DATE(04 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-END-DATE(04 01)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-NUMBER(04 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-NAME(04 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-PROVIDER-NUMBER(04 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-RECIPIENT-NUMBER(04 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-START-DATE(04 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-END-DATE(04 02)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-NUMBER(04 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-NAME(04 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-PROVIDER-NUMBER(04 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-RECIPIENT-NUMBER(04 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-START-DATE(04 03)
                                             DELIMITED BY SPACE
                   ","                       DELIMITED BY SIZE
                   PTRES-INSURANCE-END-DATE(04 03)
                                             DELIMITED BY SPACE
      *
      *
                   INTO    REQCSV-RECORD
           END-STRING
      *
           WRITE   REQCSV-RECORD
           CLOSE   REQCSV-FILE
      *
           MOVE    "BLOBIMPORT"        TO  MCP-FUNC
           MOVE    "blob"              TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING MCPAREA
                                       ORCA-BLOB
                                       SPA-AREA
      *
           IF  ORCA-BLOB-OBJECT = LOW-VALUE
               DISPLAY "object is low value"
           ELSE
               DISPLAY "object is not low value"
           END-IF
      *
           MOVE    ORCA-BLOB-OBJECT    TO  PTAPI-BODY
           MOVE    "data/csv"          TO  PTAPIREQ-CONTENT-TYPE
      *
      *
          .
       1000-PTCSVMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本情報取得
      *****************************************************************
       1001-PTINF-GET-SEC      SECTION.
      *
           INITIALIZE        PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPTID-PTID          TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              NOT =  ZERO
                   GO        TO        1001-PTINF-GET-EXT
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              NOT =  ZERO
                   GO        TO        1001-PTINF-GET-EXT
           END-IF
           MOVE    MCPDATA-REC         TO  PTINF-REC
           .
       1001-PTINF-GET-EXT.
           EXIT.
      *****************************************************************
      *    患者保険情報取得
      *****************************************************************
       1001-PTHKN-GET-SEC      SECTION.
      *
      *    保険情報
           INITIALIZE                  HKNCOMBI-REC
           MOVE    PTINF-HOSPNUM       TO  COMB-HOSPNUM
           MOVE    PTINF-PTID          TO  COMB-PTID
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
             MOVE  ZERO      TO        FLG-HKNCOMBI
             MOVE    "tbl_hkncombi"    TO  MCP-TABLE
             MOVE    "key2"            TO  MCP-PATHNAME
             PERFORM 910-DBFETCH-SEC
             MOVE  MCPDATA-REC         TO  HKNCOMBI-REC
           ELSE
             MOVE  1         TO        FLG-HKNCOMBI
             INITIALIZE                HKNCOMBI-REC
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL      (IDX           >    4    )            OR
                              (FLG-HKNCOMBI   =   1    )
               IF     (WRK-SYSYMD-8          >=  COMB-TEKSTYMD)  AND
                      (WRK-SYSYMD-8          <=  COMB-TEKEDYMD)  AND
                      (COMB-DELKBN           =   SPACE        )
                   ADD  1                    TO  IDX
                   MOVE COMB-SYU-TANSEIDONAME
                                             TO  
                                             PTRES-MAIN-INSURANCE-NAME
                                             (IDX)
      *
                       INITIALIZE                  PTHKNINF-REC
                       MOVE    PTINF-HOSPNUM       TO  PTHKN-HOSPNUM
                       MOVE    PTINF-PTID          TO  PTHKN-PTID
                       MOVE    COMB-HKNID          TO  PTHKN-HKNID
                       MOVE    WRK-SYSYMD-8        TO  PTHKN-TEKSTYMD
                       MOVE    WRK-SYSYMD-8        TO  PTHKN-TEKEDYMD
                       MOVE    PTHKNINF-REC        TO  MCPDATA-REC
                       PERFORM 910-PTHKNINF-INV-SEC
      *                MOVE    PTHKN-HKNNUM(2:2)
                       MOVE    PTHKN-HKNNUM
                                                   TO  
                                       PTRES-MAIN-INSURANCE-CLASS(IDX)
                       MOVE    PTHKN-HKNJANUM      TO
                                       PTRES-MAIN-INSURANCE-NUMBER(IDX)
                       MOVE    PTHKN-KIGO          TO
                                       PTRES-MAIN-MARK(IDX)
                       MOVE    PTHKN-NUM           TO
                                       PTRES-MAIN-NUMBER(IDX)
                       MOVE    PTHKN-CONTKBN       TO
                                       PTRES-MAIN-CONTINUATION(IDX)
                       MOVE    PTHKN-HOJOKBN       TO
                                       PTRES-MAIN-ASSISTANCE(IDX)
                       MOVE    PTHKN-HONKZKKBN     TO
                                       PTRES-MAIN-FAMILY(IDX)
                       MOVE    PTHKN-HIHKNJANAME   TO
                                       PTRES-MAIN-POLICYHOLDER(IDX)
                       IF      PTHKN-TEKSTYMD      =  SPACE
                         MOVE  SPACE               TO
                                       PTRES-MAIN-START-DATE(IDX)
                       ELSE
                         MOVE  PTHKN-TEKSTYMD(1:4) TO
                                       PTRES-MAIN-START-DATE
                                                   (IDX)(1:4)
                         MOVE  "-"                 TO
                                       PTRES-MAIN-START-DATE
                                                   (IDX)(5:1)
                         MOVE  PTHKN-TEKSTYMD(5:2) TO
                                       PTRES-MAIN-START-DATE
                                                   (IDX)(6:2)
                         MOVE  "-"                 TO
                                       PTRES-MAIN-START-DATE
                                                   (IDX)(8:1)
                         MOVE  PTHKN-TEKSTYMD(7:2) TO
                                       PTRES-MAIN-START-DATE
                                                   (IDX)(9:2)
                       END-IF
                       IF      PTHKN-TEKEDYMD      =  SPACE
                         MOVE    SPACE               TO
                                       PTRES-MAIN-END-DATE(IDX)
                       ELSE
                         MOVE    PTHKN-TEKEDYMD(1:4) TO
                                       PTRES-MAIN-END-DATE(IDX)(1:4)
                         MOVE    "-"                 TO
                                       PTRES-MAIN-END-DATE(IDX)(5:1)
                         MOVE    PTHKN-TEKEDYMD(5:2) TO
                                       PTRES-MAIN-END-DATE(IDX)(6:2)
                         MOVE    "-"                 TO
                                       PTRES-MAIN-END-DATE(IDX)(8:1)
                         MOVE    PTHKN-TEKEDYMD(7:2) TO
                                       PTRES-MAIN-END-DATE(IDX)(9:2)
                       END-IF
                       IF      PTHKN-TEKEDYMD      =  "99999999"
                         MOVE    "9999-12-31"        TO
                                       PTRES-MAIN-END-DATE(IDX)
                       END-IF
      *   公費読み出し
                   IF      COMB-KOHHKNNUM (1)      NOT =   SPACE
                       PERFORM 20012-KOHID-HENSYU-SEC
                       PERFORM VARYING IDY1 FROM    1   BY  1
                               UNTIL   IDY1 >       3
                               OR      WRK-KOHID(IDY1)    =   ZERO
                           INITIALIZE              PTKOHINF-REC
                           MOVE    PTINF-HOSPNUM   TO  PTKOH-HOSPNUM
                           MOVE    PTINF-PTID      TO  PTKOH-PTID
                           MOVE    WRK-KOHID(IDY1) TO  PTKOH-KOHID
                           MOVE    WRK-SYSYMD-8    TO  PTKOH-TEKSTYMD
                           MOVE    WRK-SYSYMD-8    TO  PTKOH-TEKEDYMD
                           MOVE    PTKOHINF-REC    TO  MCPDATA-REC
                           PERFORM 910-PTKOHINF-INV-SEC
                           MOVE    WRK-KOHMEI (IDY1)   TO
                                   PTRES-INSURANCE-NAME(IDX IDY1)
                           MOVE    PTKOH-FTNJANUM      TO
                                   PTRES-PROVIDER-NUMBER(IDX IDY1)
                           MOVE    PTKOH-JKYSNUM       TO
                                   PTRES-RECIPIENT-NUMBER(IDX IDY1)
                           MOVE    PTKOH-KOHNUM        TO
                                   PTRES-INSURANCE-NUMBER
                                                          (IDX IDY1)
                           MOVE    PTKOH-TEKSTYMD(1:4) TO
                                   PTRES-INSURANCE-START-DATE
                                                     (IDX IDY1)(1:4)
                           MOVE    "-"                 TO
                                   PTRES-INSURANCE-START-DATE
                                                     (IDX IDY1)(5:1)
                           MOVE    PTKOH-TEKSTYMD(5:2) TO
                                   PTRES-INSURANCE-START-DATE
                                                     (IDX IDY1)(6:2)
                           MOVE    "-"                 TO
                                   PTRES-INSURANCE-START-DATE
                                                     (IDX IDY1)(8:1)
                           MOVE    PTKOH-TEKSTYMD(7:2) TO
                                   PTRES-INSURANCE-START-DATE
                                                     (IDX IDY1)(9:2)
                           MOVE    PTKOH-TEKEDYMD(1:4) TO
                                   PTRES-INSURANCE-END-DATE
                                                     (IDX IDY1)(1:4)
                           MOVE    "-"                 TO
                                   PTRES-INSURANCE-END-DATE
                                                     (IDX IDY1)(5:1)
                           MOVE    PTKOH-TEKEDYMD(5:2) TO
                                   PTRES-INSURANCE-END-DATE
                                                     (IDX IDY1)(6:2)
                           MOVE    "-"                 TO
                                   PTRES-INSURANCE-END-DATE
                                                     (IDX IDY1)(8:1)
                           MOVE    PTKOH-TEKEDYMD(7:2) TO
                                   PTRES-INSURANCE-END-DATE
                                                     (IDX IDY1)(9:2)
                           IF      PTKOH-TEKEDYMD      = "99999999"
                             MOVE  "9999-12-31"        TO
                                   PTRES-INSURANCE-END-DATE
                                                     (IDX IDY1)
                           END-IF
                       END-PERFORM
                   END-IF
      *
               END-IF
                   MOVE    "tbl_hkncombi"    TO  MCP-TABLE
                   MOVE    "key2"            TO  MCP-PATHNAME
                   PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              NOT =   ZERO
                   MOVE  1         TO        FLG-HKNCOMBI
               ELSE
                   MOVE  MCPDATA-REC         TO  HKNCOMBI-REC
               END-IF
           END-PERFORM
      *
           .
       1001-PTHKN-GET-EXT.
           EXIT.
      *****************************************************************
      *    公費個別処理
      *****************************************************************
       20012-KOHID-HENSYU-SEC              SECTION.
      *
           INITIALIZE                      WRK-KOHI-AREA
                                           WRK-KOHI-AREA-X
           MOVE    ZERO                    TO  TBL-MAX
      *
      *
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       4
               MOVE    COMB-KOHID    (IDZ) TO  WRK-KOHID-O     (IDZ)
               MOVE    COMB-KOHHKNNUM(IDZ) TO  WRK-KOHHKNNUM-O (IDZ)
      *        公費名称
               MOVE    COMB-KOH-TANSEIDONAME(IDZ)
                                           TO  WRK-KOHMEI-O    (IDZ)
      *        公費−外来−上限区分
               MOVE    COMB-KOH-GAIJGNKBN (IDZ)
                                           TO  WRK-GAIJGNKBN-O (IDZ)
           END-PERFORM
      *
      *    優先順位に並べ替え
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       WRK-SRT-KOHHKNNUM-MAX
                   OR      TBL-MAX     =   4
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       4
                   IF      WRK-SRT-KOHHKNNUM   (IDZ)
                                   =   WRK-KOHHKNNUM-O (IDY)
                       ADD     1               TO  TBL-MAX
                       MOVE    WRK-KOHI-TBL    (IDY)   TO
                                           WRK-KOHI-TBL-X (TBL-MAX)
                       INITIALIZE          WRK-KOHI-TBL   (IDY)
                       MOVE    4               TO  IDY
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    すべて並べ替えた時
           IF      TBL-MAX                 =   4
               GO  TO  20012-KOHID-HENSYU-EXT
           END-IF
      *
      *    地方公費の並び替え
           PERFORM VARYING IDZ FROM    1    BY  1
                   UNTIL   IDZ >       3
               COMPUTE IDX1    =   IDZ     +   1
               PERFORM VARYING IDY FROM    IDX1    BY  1
                       UNTIL   IDY >       4
                   IF      WRK-KOHHKNNUM-O (IDZ) >
                                           WRK-KOHHKNNUM-O (IDY)
                   OR      WRK-KOHHKNNUM-O (IDZ) =   SPACE
                       MOVE    WRK-KOHI-TBL    (IDZ) TO
                                           WRK-KOHI
                       MOVE    WRK-KOHI-TBL    (IDY) TO
                                           WRK-KOHI-TBL(IDZ)
                       MOVE    WRK-KOHI              TO
                                           WRK-KOHI-TBL(IDY)
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    地方公費の追加
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ         >   4
                   OR      TBL-MAX     =   4
               IF      WRK-KOHID-O (IDZ)   >   ZERO
                   ADD     1               TO  TBL-MAX
                   MOVE    WRK-KOHI-TBL (IDZ)
                           TO  WRK-KOHI-TBL-X (TBL-MAX)
               END-IF
           END-PERFORM
           .
       20012-KOHID-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報マスタ読込
      *****************************************************************
       910-PTHKNINF-INV-SEC         SECTION.
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTHKNINF
                   MOVE    MCPDATA-REC         TO  PTHKNINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTHKNINF
                   INITIALIZE          PTHKNINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-PTHKNINF-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報マスタ読込
      *****************************************************************
       910-PTKOHINF-INV-SEC         SECTION.
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTKOHINF
                   MOVE    MCPDATA-REC         TO  PTKOHINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTKOHINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTKOHINF
           END-IF
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-PTKOHINF-INV-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      **************************************************************************
       010-INIT                SECTION.
           DISPLAY 'init'
           MOVE   'NEW'        TO  MCP-PUTTYPE.
           MOVE   'windowtitle'     TO  MCP-WINDOW.
           PERFORM 900-PUT-WINDOW.
      **************************************************************************
       800-CLICKED             SECTION.
      *    CALL    "coblog"    USING   "api clicked"
           DISPLAY 'CLICKED'
           PERFORM 900-PUT-WINDOW.
      **************************************************************************
       900-PUT-WINDOW          SECTION.
           MOVE   'PUTWINDOW'  TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
                MCPAREA.
