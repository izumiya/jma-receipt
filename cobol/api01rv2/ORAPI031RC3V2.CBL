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
       PROGRAM-ID.         ORAPI031RC3V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 室料差額編集
      *  管理者            :
      *  作成日付    作業者        記述
      *  17/12/04    NACL−太田　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  FLG-AREA.
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-LOOPEND             PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDXL                    PIC 9(05).
           03  IDXM                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-YMD                 PIC X(10). 
           03  WRK-ZOGENPTN            PIC X(01).
           03  WRK-ZOGEN               PIC S9(05).
           03  WRK-KEISANYMD           PIC X(08).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-EDTYMD1             PIC X(09).
           03  WRK-DATE.
               05  WRK-DATE-YY         PIC 9(04).
               05  WRK-DATE-FL1        PIC X(01).
               05  WRK-DATE-MM         PIC 9(02).
               05  WRK-DATE-FL2        PIC X(01).
               05  WRK-DATE-DD         PIC 9(02).
           03  WRK-HMS.
               05  WRK-HMS-HH          PIC 9(02).
               05  WRK-HMS-MM          PIC 9(02).
               05  WRK-HMS-SS          PIC 9(02).
           03  WRK-TIME.
               05  WRK-TIME-HH         PIC 9(02).
               05  WRK-TIME-FL1        PIC X(01).
               05  WRK-TIME-MM         PIC 9(02).
               05  WRK-TIME-FL2        PIC X(01).
               05  WRK-TIME-SS         PIC 9(02).
           03  WRK-SRYYMD.
               05  WRK-SRYYMD-YM       PIC X(06).
               05  WRK-SRYYMD-DD       PIC 9(02).
           03  WRK-SAGAKUZ9            PIC ZZZZZ9.
           03  WRK-ERRIDXZ9            PIC Z9.
           03  WRK-EDYUKYMD            PIC X(8).
      *
       01  API-AREA.
           03  API-BASEYMD             PIC X(08).
           03  API-ERRFLG              PIC 9(01).
           03  API-RMSAGAKU-LAST       PIC 9(05).
           03  API-RMSAGAKU-OCC        OCCURS 99.
               05  API-TKBNCD          PIC X(02).
               05  API-TSAGAKU         PIC 9(07).
               05  API-TSTYUKYMD       PIC X(08).
               05  API-TEDYUKYMD       PIC X(08).
               05  API-TLASTSTYMD      PIC X(08).
               05  API-TLASTEDYMD      PIC X(08).
               05  API-TUPEDYMD        PIC X(08).
               05  API-TFORCE          PIC X(04).
               05  API-TERRCD          PIC X(04).
      *
       01  CONST-AREA.
           03  CONST-RMSAGAKU-INF-MAX  PIC 9(05)   VALUE   99.
           03  CONST-SELECT            PIC X(02)   VALUE   "01".
           03  CONST-INSERT            PIC X(02)   VALUE   "02".
           03  CONST-UPDATE            PIC X(02)   VALUE   "03".
           03  CONST-DELETE            PIC X(02)   VALUE   "04".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPAPIV2REQ.INC".
           COPY    "CPHSCONFRCV2REQ.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPHSCONFRCV2RES.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1010.INC".
           COPY    "CPSK5113.INC".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "API01RV2SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "***************************"
           DISPLAY   "* hsconfroomcharge start  *"
           DISPLAY   "***************************"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  API-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    返却領域編集
           PERFORM 300-END-SEC
      *
           DISPLAY   "**************************"
           DISPLAY   "* hsconfroomcharge end   *"
           DISPLAY   "**************************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  XML-HSCONFRCRES
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    MCP-USER            TO  SPA-OPID
      *
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *
      *    医療機関識別番号セット 
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE   "0099"           TO  SPA-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               EVALUATE    SPA-ERRCD
               WHEN    "1001"
                   MOVE    "8901"      TO  SPA-ERRCD
               WHEN    "1002"
                   MOVE    "8902"      TO  SPA-ERRCD
               WHEN    "1003"
                   MOVE    "8903"      TO  SPA-ERRCD
               WHEN    "1005"
                   MOVE    "8905"      TO  SPA-ERRCD
               WHEN    "1015"
                   MOVE    "8915"      TO  SPA-ERRCD
               WHEN    OTHER
                   MOVE    "8900"      TO  SPA-ERRCD
               END-EVALUATE
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    XML情報取り出し
           PERFORM 900-XML-READ-SEC
           IF    ( SPA-ERRCD           NOT =   SPACE )
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 2002-DB-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2003-RESPONSE-EDIT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目チェック処理
      *****************************************************************
       2001-INPUT-CHK-SEC                 SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               EVALUATE    HSCONFRCREQ-REQNUM
               WHEN    SPACE
               WHEN    CONST-SELECT
                   MOVE    "01"        TO  HSCONFRCREQ-REQNUM
                   PERFORM 20011-INPUT-CHK-SEC
               WHEN    CONST-INSERT
               WHEN    CONST-UPDATE
               WHEN    CONST-DELETE
                   PERFORM 20012-INPUT-CHK-SEC
               WHEN    OTHER
                   MOVE    "0001"      TO  SPA-ERRCD
               END-EVALUATE
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    参照時入力項目チェック処理
      *****************************************************************
       20011-INPUT-CHK-SEC         SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-BASEYMD-CHK-SEC
           END-IF
      *
           .
       20011-INPUT-CHK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    追加・更新・削除時入力項目チェック処理
      *****************************************************************
       20012-INPUT-CHK-SEC         SECTION.
      *
           MOVE    ZERO            TO  IDX1
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-BASEYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
               PERFORM VARYING IDXL    FROM    1   BY  1
                     UNTIL ( IDXL  >   CONST-RMSAGAKU-INF-MAX )
      *
                   IF    ( HSCONFRCREQ-TKBNCD (IDXL)   NOT =   SPACE )
      *
                       COMPUTE IDX1    =   IDX1    +   1
      *
                       MOVE    IDXL    TO  API-RMSAGAKU-LAST
      *
                       MOVE    SPACE   TO  SPA-ERRCD
      *
                       EVALUATE    HSCONFRCREQ-REQNUM
                       WHEN    CONST-INSERT
                           PERFORM 2001-INSERT-INPUT-CHK-SEC
                       WHEN    CONST-UPDATE
                           PERFORM 2001-UPDATE-INPUT-CHK-SEC
                       WHEN    CONST-DELETE
                           PERFORM 2001-DELETE-INPUT-CHK-SEC
                       END-EVALUATE
      *
                       IF    ( SPA-ERRCD   NOT =   SPACE )
                           MOVE    SPA-ERRCD   TO  API-TERRCD (IDXL)
                           MOVE    1           TO  API-ERRFLG
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
               IF    ( API-ERRFLG      =   1 )
                   MOVE    "0003"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF  ( IDX1          =   ZERO )
                   MOVE    "0004"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2001-INSERT-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    追加時入力項目チェック処理
      *****************************************************************
       2001-INSERT-INPUT-CHK-SEC       SECTION.
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 20011-KBNCD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 20011-SAGAKU-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 20011-STYUKYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 20011-EDYUKYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 20011-FORCE-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 20011-OTHER-CHK-SEC
           END-IF
      *
           .
       2001-INSERT-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    更新時入力項目チェック処理
      *****************************************************************
       2001-UPDATE-INPUT-CHK-SEC       SECTION.
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 20011-KBNCD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               IF    ( HSCONFRCREQ-TSAGAKU (IDXL)      NOT =   SPACE )
                   PERFORM 20011-SAGAKU-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               IF    ( HSCONFRCREQ-TSTYUKYMD (IDXL)    NOT =   SPACE )
                   PERFORM 20011-STYUKYMD-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               IF    ( HSCONFRCREQ-TEDYUKYMD (IDXL)    NOT =   SPACE )
                   PERFORM 20011-EDYUKYMD-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 20011-OTHER-CHK-SEC
           END-IF
      *
           .
       2001-UPDATE-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    削除時入力項目チェック処理
      *****************************************************************
       2001-DELETE-INPUT-CHK-SEC       SECTION.
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 20011-KBNCD-CHK-SEC
           END-IF
      *
           .
       2001-DELETE-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基準日チェック処理
      *****************************************************************
       2001-BASEYMD-CHK-SEC            SECTION.
      *
           IF    ( HSCONFRCREQ-BASEYMD     =   SPACE )
               MOVE    SPA-SYSYMD      TO  API-BASEYMD
           ELSE
               MOVE    HSCONFRCREQ-BASEYMD
                                       TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0002"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD    TO  API-BASEYMD
               END-IF
           END-IF
      *
           .
       2001-BASEYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    室料差額コードチェック処理
      *****************************************************************
       20011-KBNCD-CHK-SEC              SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( HSCONFRCREQ-TKBNCD (IDXL)(1:2) IS NUMERIC )
                AND  ( HSCONFRCREQ-TKBNCD (IDXL)  >=  "01" )
                AND  ( HSCONFRCREQ-TKBNCD (IDXL)  <=  "99" )
                   MOVE    HSCONFRCREQ-TKBNCD (IDXL)
                                           TO  API-TKBNCD (IDXL)
               ELSE
                   MOVE    "0005"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
            AND  ( HSCONFRCREQ-REQNUM  =   CONST-UPDATE
                                        OR CONST-DELETE )
      *
               INITIALIZE                      SYSKANRI-REC
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               MOVE    "5113"              TO  SYS-KANRICD
               MOVE    API-TKBNCD (IDXL)   TO  SYS-KBNCD
               MOVE    API-BASEYMD         TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               IF    ( FLG-SYSKANRI        =   ZERO )
                   MOVE    SYSKANRI-REC    TO  SYS-5113-REC
                   MOVE    SYS-5113-BRM-SAGAKU-NM
                                           TO  API-TSAGAKU (IDXL)
                   MOVE    SYS-5113-STYUKYMD
                                           TO  API-TSTYUKYMD (IDXL)
                   MOVE    SYS-5113-EDYUKYMD
                                           TO  API-TEDYUKYMD (IDXL)
               ELSE
                   MOVE    "0006"          TO  SPA-ERRCD
               END-IF
      *
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM VARYING IDXM    FROM    1   BY  1
                       UNTIL ( IDXM           >=   IDXL )
                        OR   ( SPA-ERRCD   NOT =   SPACE )
                   IF    (  HSCONFRCREQ-TKBNCD (IDXL)
                                       =   HSCONFRCREQ-TKBNCD (IDXM))
                       MOVE    "0007"  TO  SPA-ERRCD
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       20011-KBNCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    室料差額チェック処理
      *****************************************************************
       20011-SAGAKU-CHK-SEC            SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
            AND  ( HSCONFRCREQ-REQNUM  =   CONST-INSERT )
               IF    ( HSCONFRCREQ-TSAGAKU (IDXL)
                                       =   SPACE )
                   MOVE    "0008"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
      *
               INITIALIZE                  ORCSNUMAREA
               MOVE    HSCONFRCREQ-TSAGAKU (IDXL)
                                       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF  ( SNUM-RC       NOT =   ZERO  )
                OR ( SNUM-MINKBN   NOT =   SPACE )
                OR ( SNUM-SYOKBN   NOT =   SPACE )
                   MOVE    "0009"      TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM TO  API-TSAGAKU (IDXL)
      *
                   IF    ( API-TSAGAKU (IDXL)
                                       >   300000 )
                       MOVE    "0015"  TO  SPA-ERRCD
                   END-IF
      *
               END-IF
           END-IF
      *
           .
       20011-SAGAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    有効期間開始日チェック処理
      *****************************************************************
       20011-STYUKYMD-CHK-SEC                 SECTION.
      *
           IF   ( HSCONFRCREQ-TSTYUKYMD (IDXL) =   SPACE )
            OR  ( HSCONFRCREQ-TSTYUKYMD (IDXL)(1:4)
                                               =   "0000" )
               MOVE    "00000000"      TO  API-TSTYUKYMD (IDXL)
           ELSE
               MOVE    HSCONFRCREQ-TSTYUKYMD (IDXL)
                                       TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0010"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD    TO  API-TSTYUKYMD (IDXL)
               END-IF
           END-IF
      *
           .
       20011-STYUKYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    有効期間終了日チェック処理
      *****************************************************************
       20011-EDYUKYMD-CHK-SEC                 SECTION.
      *
           IF   ( HSCONFRCREQ-TEDYUKYMD (IDXL) =   SPACE )
            OR  ( HSCONFRCREQ-TEDYUKYMD (IDXL)(1:4)
                                               =   "9999" )
               MOVE    "99999999"      TO  API-TEDYUKYMD (IDXL)
           ELSE
               MOVE    HSCONFRCREQ-TEDYUKYMD (IDXL)
                                       TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0011"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD    TO  API-TEDYUKYMD (IDXL)
               END-IF
           END-IF
      *
           .
       20011-EDYUKYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    強制更新フラグチェック処理
      *****************************************************************
       20011-FORCE-CHK-SEC                 SECTION.
      *
           EVALUATE    FUNCTION
                         UPPER-CASE(HSCONFRCREQ-TFORCE (IDXL))
           WHEN    SPACE
           WHEN    "FALSE"
               CONTINUE
           WHEN    "TRUE"
               MOVE    "1"     TO  API-TFORCE (IDXL)
           WHEN    OTHER
               MOVE "0012"     TO  SPA-ERRCD
           END-EVALUATE
      *
           .
       20011-FORCE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    その他チェック処理
      *****************************************************************
       20011-OTHER-CHK-SEC                 SECTION.
      *
           IF    ( API-TSTYUKYMD (IDXL)    >   API-TEDYUKYMD (IDXL))
               MOVE    "0013"              TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               PERFORM 200111-KIKAN-CHK-SEC
           END-IF
      *
           .
       20011-OTHER-CHK-EXT.
           EXIT.
      *****************************************************************
      *    有効期間チェック処理
      *****************************************************************
       200111-KIKAN-CHK-SEC                 SECTION.
      *
           INITIALIZE                          SYSKANRI-REC
           MOVE    SPA-HOSPNUM             TO  SYS-HOSPNUM
           MOVE    "5113"                  TO  SYS-KANRICD
           MOVE    API-TKBNCD (IDXL)       TO  SYS-KBNCD
           MOVE    API-TSTYUKYMD (IDXL)    TO  SYS-EDYUKYMD
           MOVE    API-TEDYUKYMD (IDXL)    TO  SYS-STYUKYMD
           PERFORM 900-SYSKANRI-KEY26-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( SPA-ERRCD       NOT =   SPACE )
      *
               EVALUATE    HSCONFRCREQ-REQNUM
               WHEN    CONST-INSERT
                   EVALUATE    TRUE
                   WHEN  ( API-TFORCE (IDXL)      =   SPACE )
                   WHEN  ( API-TSTYUKYMD (IDXL)  <=   SYS-STYUKYMD )
                       MOVE    "0014"          TO  SPA-ERRCD
                   END-EVALUATE
      *
                   IF    ( API-TSTYUKYMD (IDXL)   >   SYS-STYUKYMD )
      *
                       MOVE  SYS-STYUKYMD  TO  API-TLASTSTYMD (IDXL)
                       MOVE  SYS-EDYUKYMD  TO  API-TLASTEDYMD (IDXL)
      *
                       MOVE  API-TSTYUKYMD (IDXL)
                                           TO  WRK-YMD
                       MOVE    "1"         TO  WRK-ZOGENPTN
                       MOVE    -1          TO  WRK-ZOGEN
                       PERFORM 800-CALENDAR-SEC
                       MOVE    WRK-KEISANYMD
                                           TO  API-TUPEDYMD (IDXL)
                       END-IF
               WHEN    CONST-UPDATE
                   IF    ( API-BASEYMD         >=  SYS-STYUKYMD )
                    AND  ( API-BASEYMD         <=  SYS-EDYUKYMD )
                       CONTINUE
                   ELSE
                       MOVE    "0014"          TO  SPA-ERRCD
                   END-IF
               END-EVALUATE
      *
               PERFORM 900-SYSKANRI-KEY26-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       200111-KIKAN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    データベース更新処理
      *****************************************************************
       2002-DB-SEC                         SECTION.
      *
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL ( IDXL  >     API-RMSAGAKU-LAST )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               IF    ( API-TKBNCD (IDXL)   NOT =   SPACE )
                   PERFORM 20021-DB-SEC
               END-IF
           END-PERFORM
      *
           .
       2002-DB-EXT.
           EXIT.
      *****************************************************************
      *    データベース更新処理
      *****************************************************************
       20021-DB-SEC                    SECTION.
      *
           EVALUATE    HSCONFRCREQ-REQNUM
           WHEN    CONST-INSERT
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( API-TLASTSTYMD (IDXL)
                                           NOT =   SPACE )
                       PERFORM 200211-LAST-UPDATE-SEC
                   END-IF
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 200211-INSERT-SEC
               END-IF
           WHEN    CONST-UPDATE
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 200211-UPDATE-SEC
               END-IF
           WHEN    CONST-DELETE
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 200211-DELETE-SEC
               END-IF
           END-EVALUATE
      *
           .
       20021-DB-EXT.
           EXIT.
      *****************************************************************
      *    有効期間終了日更新処理
      *****************************************************************
       200211-LAST-UPDATE-SEC          SECTION.
      *
           INITIALIZE                          SYSKANRI-REC
           MOVE    SPA-HOSPNUM             TO  SYS-HOSPNUM
           MOVE    "5113"                  TO  SYS-KANRICD
           MOVE    API-TKBNCD (IDXL)       TO  SYS-KBNCD
           MOVE    API-TUPEDYMD (IDXL)     TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
           MOVE    SPA-OPID                TO  SYS-OPID
           MOVE    SMCNDATE-YMD            TO  SYS-UPYMD
           MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
           MOVE    SYSKANRI-REC            TO  MCPDATA-REC
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "upd3"                  TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC  NOT =   ZERO )
               MOVE    "4001"              TO  SPA-ERRCD
           END-IF
      *
           .
       200211-LAST-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    室料差額追加処理
      *****************************************************************
       200211-INSERT-SEC               SECTION.
      *
           INITIALIZE                          SYS-5113-REC
           MOVE    SPA-HOSPNUM             TO  SYS-5113-HOSPNUM
           MOVE    "5113"                  TO  SYS-5113-KANRICD
           MOVE    API-TKBNCD (IDXL)       TO  SYS-5113-KBNCD
           MOVE    API-TSTYUKYMD (IDXL)    TO  SYS-5113-STYUKYMD
           MOVE    API-TEDYUKYMD (IDXL)    TO  SYS-5113-EDYUKYMD
           MOVE    API-TSAGAKU (IDXL)      TO  SYS-5113-BRM-SAGAKU-NM
           MOVE    SPA-OPID                TO  SYS-5113-OPID
           MOVE    SMCNDATE-YMD            TO  SYS-5113-CREYMD
                                               SYS-5113-UPYMD
           MOVE    SMCNDATE-HMS            TO  SYS-5113-UPHMS
           MOVE    SYS-5113-REC            TO  MCPDATA-REC
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC  NOT =   ZERO )
               MOVE    "4002"              TO  SPA-ERRCD
           END-IF
      *
           .
       200211-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    室料差額更新処理
      *****************************************************************
       200211-UPDATE-SEC                   SECTION.
      *
           INITIALIZE                          SYS-5113-REC
           MOVE    SPA-HOSPNUM             TO  SYS-5113-HOSPNUM
           MOVE    "5113"                  TO  SYS-5113-KANRICD
           MOVE    API-TKBNCD (IDXL)       TO  SYS-5113-KBNCD
           MOVE    API-BASEYMD             TO  SYS-5113-STYUKYMD
                                               SYS-5113-EDYUKYMD
           MOVE    SYS-5113-REC            TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI            =   ZERO )
               MOVE    SYSKANRI-REC        TO  SYS-5113-REC
               MOVE    API-TSTYUKYMD(IDXL) TO  SYS-5113-STYUKYMD
               MOVE    API-TEDYUKYMD(IDXL) TO  SYS-5113-EDYUKYMD
               MOVE    API-TSAGAKU (IDXL)  TO  SYS-5113-BRM-SAGAKU-NM
               MOVE    SPA-OPID            TO  SYS-5113-OPID
               MOVE    SMCNDATE-YMD        TO  SYS-5113-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-5113-UPHMS
               MOVE    SYS-KEY             TO  SYS-5113-UPD-KEY
               MOVE    SYS-5113-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC  NOT =   ZERO )
                   MOVE    "4003"          TO  SPA-ERRCD
               END-IF
           ELSE
      *
               MOVE    "4004"              TO  SPA-ERRCD
      *
               INITIALIZE                  SYSKANRI-REC
               MOVE    SYSKANRI-REC    TO  MCPDATA-REC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "abort"         TO  MCP-PATHNAME
               PERFORM 911-DBSELECT-SEC
           END-IF
      *
           .
       200211-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    室料差額削除処理
      *****************************************************************
       200211-DELETE-SEC                   SECTION.
      *
           INITIALIZE                          SYS-5113-REC
           MOVE    SPA-HOSPNUM             TO  SYS-5113-HOSPNUM
           MOVE    "5113"                  TO  SYS-5113-KANRICD
           MOVE    API-TKBNCD (IDXL)       TO  SYS-5113-KBNCD
           MOVE    API-BASEYMD             TO  SYS-5113-STYUKYMD
                                               SYS-5113-EDYUKYMD
           MOVE    SYS-5113-REC            TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI            =   ZERO )
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
               IF    ( MCP-RC  NOT =   ZERO )
                   MOVE    "4005"          TO  SPA-ERRCD
               END-IF
           ELSE
      *
               MOVE    "4006"              TO  SPA-ERRCD
      *
               INITIALIZE                  SYSKANRI-REC
               MOVE    SYSKANRI-REC    TO  MCPDATA-REC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "abort"         TO  MCP-PATHNAME
               PERFORM 911-DBSELECT-SEC
           END-IF
      *
           .
       200211-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    レスポンス編集処理
      *****************************************************************
       2003-RESPONSE-EDIT-SEC          SECTION.
      *
           MOVE    HSCONFRCREQ-REQNUM
                                   TO  HSCONFRCRES-REQNUM
      *
           MOVE    API-BASEYMD     TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  HSCONFRCRES-BASEYMD
      *
           INITIALIZE                  SYS-5113-REC
           MOVE    "5113"          TO  SYS-5113-KANRICD
           MOVE    API-BASEYMD     TO  SYS-5113-STYUKYMD
                                       SYS-5113-EDYUKYMD
           MOVE    SYS-5113-REC    TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           MOVE    ZERO            TO  IDXL
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( IDXL       >=   CONST-RMSAGAKU-INF-MAX )
      *
               COMPUTE IDXL =  IDXL   +    1
      *
               MOVE    SYSKANRI-REC    TO  SYS-5113-REC
      *
               MOVE    SYS-5113-KBNCD(1:2)
                                       TO  HSCONFRCRES-TKBNCD (IDXL)
      *
               MOVE    SYS-5113-BRM-SAGAKU-NM
                                       TO  WRK-SAGAKUZ9
               MOVE    WRK-SAGAKUZ9    TO  HSCONFRCRES-TSAGAKU (IDXL)
      *
               MOVE    SYS-5113-STYUKYMD   TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE        TO  HSCONFRCRES-TSTYUKYMD (IDXL)
      *
               MOVE    SYS-5113-EDYUKYMD   TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE        TO  HSCONFRCRES-TEDYUKYMD (IDXL)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2003-RESPONSE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           PERFORM 900-XML-WRITE-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-DATE-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-DATE
      *
           IF    ( WRK-SYMD        NOT  =   SPACE )
               INITIALIZE                  WRK-DATE
               MOVE    WRK-SYY             TO  WRK-DATE-YY
               MOVE    WRK-SMM             TO  WRK-DATE-MM
               MOVE    WRK-SDD             TO  WRK-DATE-DD
               MOVE    "-"                 TO  WRK-DATE-FL1
               MOVE    "-"                 TO  WRK-DATE-FL2
      *
               EVALUATE    WRK-SYMD
               WHEN    "00000000"
                   MOVE    01              TO  WRK-DATE-MM
                   MOVE    01              TO  WRK-DATE-DD
               WHEN    "99999999"
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-EVALUATE
      *
           END-IF
      *
           .
       800-DATE-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-SYMD-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-SYMD
      *
           IF    ( WRK-DATE        NOT  =   SPACE )
               MOVE    WRK-DATE-YY     TO  WRK-SYY
               MOVE    WRK-DATE-MM     TO  WRK-SMM
               MOVE    WRK-DATE-DD     TO  WRK-SDD
           END-IF
      *
           .
       800-SYMD-EXT.
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
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-HIZUKE-SEC                      SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
      *
           IF  ( WRK-SYMD          =   ALL "9"  OR   ZERO )
               MOVE    WRK-SYMD (1:8)
                                   TO  WRK-EDTYMD1
           ELSE
               INITIALIZE              STS-AREA-DAY
               INITIALIZE              LNK-DAY2-AREA
               MOVE    "21"        TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD    TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
               IF    ( STS-DAY-RC1     =   ZERO )
                   MOVE    LNK-DAY2-EDTYMD1
                                   TO  WRK-EDTYMD1
               END-IF
           END-IF
      *
           .
       800-HIZUKE-EXT.
           EXIT.
      *****************************************************************
      *    時刻編集処理
      *****************************************************************
       800-TIME-SEC                    SECTION.
      *
           INITIALIZE                      WRK-TIME
           MOVE    WRK-HMS-HH          TO  WRK-TIME-HH
           MOVE    WRK-HMS-MM          TO  WRK-TIME-MM
           MOVE    WRK-HMS-SS          TO  WRK-TIME-SS
           MOVE    ":"                 TO  WRK-TIME-FL1
           MOVE    ":"                 TO  WRK-TIME-FL2
      *
           .
       800-TIME-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0000"
               MOVE    "処理終了"
                                       TO  SPA-ERRMSG
           WHEN    "0001"
               MOVE    "リクエスト番号の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "基準日の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "室料差額情報の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "室料差額コードが未入力です。"
                                       TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "室料差額コードの入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0006"
             MOVE "室料差額コードに該当する室料差額情報が存在しません。"
                                       TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE  "同じ室料差額コードの複数入力はできません。"
                                       TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE  "室料差額が未入力です。"
                                       TO  SPA-ERRMSG
           WHEN    "0009"
               MOVE  "室料差額の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0010"
               MOVE  "有効期間開始日の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0011"
               MOVE  "有効期間終了日の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0012"
               MOVE  "強制更新の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0013"
               MOVE  "有効期間開始日＞有効期間終了日となっています。"
                                       TO  SPA-ERRMSG
           WHEN    "0014"
               MOVE  "有効期間の重複する室料差額情報が存在します。"
                                       TO  SPA-ERRMSG
           WHEN    "0015"
               MOVE  "室料差額は３０万円以内で入力してください。"
                                       TO  SPA-ERRMSG
           WHEN    "8900"
               MOVE    "システム項目が設定できません。"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8901"
               MOVE    "職員情報が取得できません。"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8902"
               MOVE    "医療機関情報が取得できません。"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8903"
               MOVE    "システム日付が取得できません。"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8905"
               MOVE    "患者番号構成情報が取得できません。"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8915"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING "グループ医療機関が不整合です。" DELIMITED BY SIZE
                      "処理を終了して下さい。"         DELIMITED BY SIZE
               INTO  SPA-ERRMSG
               END-STRING
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "0097"
               MOVE    "送信内容に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0098"
               MOVE    "送信内容の読込ができませんでした。"
                                       TO  SPA-ERRMSG
           WHEN    "0099"
               MOVE    "ユーザＩＤが未登録です。"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       900-XML-READ-SEC             SECTION.
      *
           IF    ( APILST36-BODY     NOT =   LOW-VALUE )
               DISPLAY "hsconfroomchargev2 object is not low_value"
           END-IF
      *
           INITIALIZE                          XML-APIREQ
      * XML情報取り出し
           MOVE    "xml_hsconfroomchargev2req"   TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    APILST36-BODY            TO  APIREQ-OBJECT
           MOVE    ZERO                    TO  APIREQ-MODE
           PERFORM 910-XMLOPEN-SEC
           IF    ( MCP-RC              =   ZERO  OR  1 )
               MOVE    "xml_hsconfroomchargev2req"
                                           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "hsconfroomchargev2req"    TO  APIREQ-RECORDNAME
               MOVE    "XMLREAD"       TO  MCP-FUNC
               PERFORM 910-XMLREAD-SEC
               IF    ( MCP-RC               =   ZERO  OR  1 )
                   MOVE     XML-APIREQ      TO  XML-HSCONFRCREQ
               ELSE
                   DISPLAY "XMLREAD failure = " MCP-RC
                   MOVE   "0098"            TO  SPA-ERRCD
               END-IF
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
               MOVE   "0097"               TO  SPA-ERRCD
           END-IF
      *
           MOVE    "xml_hsconfroomchargev2req"   TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-XMLCLOSE-SEC
      *
           .
       900-XML-READ-EXT.
           EXIT.
      *****************************************************************
      *    XML情報を書き出す
      *****************************************************************
       900-XML-WRITE-SEC             SECTION.
      *
           MOVE    SMCNDATE-YMD    TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  HSCONFRCRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS    TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME        TO  HSCONFRCRES-INFORMATION-TIME
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    ZERO        TO  SPA-ERRCD
           END-IF
           PERFORM 890-ERRCD-MSG-SEC
           MOVE    SPA-ERRCD       TO  HSCONFRCRES-RESULT
           MOVE    SPA-ERRMSG      TO  HSCONFRCRES-RESULT-MSG
      *
           IF    ( API-ERRFLG      NOT =   ZERO )
      *
               MOVE    ZERO        TO  IDXM
      *
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   API-RMSAGAKU-LAST )
      *
                   IF    ( API-TERRCD (IDXL)   NOT =   SPACE )
                       COMPUTE IDXM    =   IDXM    +   1
                       MOVE    HSCONFRCREQ-TKBNCD (IDXL)
                                   TO  HSCONFRCRES-TKBNCD (IDXM)
                       MOVE    IDXL
                                   TO  WRK-ERRIDXZ9
                       MOVE    WRK-ERRIDXZ9
                                   TO  HSCONFRCRES-TERRIDX (IDXM)
                       MOVE    API-TERRCD (IDXL)
                                   TO  HSCONFRCRES-TERRCD (IDXM)
                       MOVE    API-TERRCD (IDXL)
                                   TO  SPA-ERRCD
                       PERFORM 890-ERRCD-MSG-SEC
                       MOVE    SPA-ERRMSG
                                   TO  HSCONFRCRES-TERRMSG (IDXM)
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           MOVE    XML-HSCONFRCRES    TO  XML-APIREQ
      *
           MOVE    1               TO  APIREQ-MODE
      *
           MOVE    "xml_hsconfroomchargev2res"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-XMLOPEN-SEC
           IF    ( MCP-RC              =   ZERO  OR  1  )
               MOVE    "xml_hsconfroomchargev2res"
                                      TO  MCP-TABLE
               MOVE    "hsconfroomchargev2res"   TO  APIREQ-RECORDNAME
               PERFORM 910-XMLWRITE-SEC
               IF    ( MCP-RC              =   ZERO  OR  1  )
                   CONTINUE
               ELSE
                   DISPLAY "XMLWRITE failure = " MCP-RC
               END-IF
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "xml_hsconfroomchargev2res"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-XMLCLOSE-SEC
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    APIREQ-OBJECT   TO  APILST36-BODY
               MOVE    "application/xml"
                                       TO  APILST36-CONTENT-TYPE
           ELSE
               DISPLAY "XMLCLOSE failure = " MCP-RC
               DISPLAY "hsconfroomcharge xml write failure "
           END-IF
      *
           .
       900-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY26-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key26"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY26-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理
      *****************************************************************
       900-SYSKANRI-KEY26-FET-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key26"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY26-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
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
      *    ＤＢ検索処理（FETCHはしない）
      *****************************************************************
       911-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
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
           MOVE    "DBFETCH"       TO  MCP-FUNC
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
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                 SECTION.
      *
           MOVE    "DBINSERT"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
       910-DBUPDATE-SEC                 SECTION.
      *
           MOVE    "DBUPDATE"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＸＭＬオープン処理
      *****************************************************************
       910-XMLOPEN-SEC                 SECTION.
      *
           MOVE    "XMLOPEN"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLOPEN-EXT.
           EXIT.
      *****************************************************************
      *    ＸＭＬ読込処理
      *****************************************************************
       910-XMLREAD-SEC                 SECTION.
      *
           MOVE    "XMLREAD"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLREAD-EXT.
           EXIT.
      *****************************************************************
      *    ＸＭＬ書込処理
      *****************************************************************
       910-XMLWRITE-SEC                SECTION.
      *
           MOVE    "XMLWRITE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLWRITE-EXT.
           EXIT.
      *****************************************************************
      *    ＸＭＬクローズ処理
      *****************************************************************
       910-XMLCLOSE-SEC                SECTION.
      *
           MOVE    "XMLCLOSE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラム終了処理（エラー時）
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           IF    ( SPA-ERRCD           =   "0099" )
               MOVE    404             TO  APILST36-HTTPSTATUS
           ELSE
               PERFORM 900-XML-WRITE-SEC
           END-IF
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
