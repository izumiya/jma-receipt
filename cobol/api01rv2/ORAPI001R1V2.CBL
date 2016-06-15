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
       PROGRAM-ID.         ORAPI001R1V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 帳票データ取得
      *  管理者            :
      *  作成日付    作業者        記述
      *  13/11/14    NACL−太田　新規作成
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
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  STS-AREA.
           03  STS-IN01                PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-IN01                PIC 9(01).
           03  FLG-PRTKANRI            PIC 9(01).
           03  FLG-PRTDATA             PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDXR                    PIC 9(10).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ERRCD               PIC X(04).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-YMD                 PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-EDTYMD1             PIC X(09).
           03  WRK-ZOGENPTN            PIC X(01).
           03  WRK-ZOGEN               PIC S9(05).
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
           03  WRK-RENNUM              PIC 9(10).
           03  WRK-OFFSET              PIC 9(10).
           03  WRK-NUMZ                PIC Z(10).
      *
       01  KEY-AREA.
           03  KEY-REQYMD              PIC X(08).
           03  KEY-REQHMS              PIC X(06).
           03  KEY-KEY                 PIC X(08).
           03  KEY-UUID                PIC X(36).
           03  KEY-GROUP               PIC X(14).
           03  KEY-RENNUM              PIC 9(04).
           03  KEY-SHORI-RENNUM        PIC 9(04).
           03  KEY-OFFSET              PIC 9(10).
      *
       01  CONST-AREA.
           03  CONST-LIMIT             PIC 9(05)   VALUE 10.
      *
       01  TPRTDATA-REC.
           COPY    "CPPRTDATA.INC"     REPLACING   //PRTDATA-//
                                       BY          //TPRTDATA-//.
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
      *    API XML読み込み用定義体
           COPY    "CPFORMDATAGETV2REQ.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPFORMDATAGETV2RES.INC".
      *
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSK1010.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷データ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
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
           DISPLAY   "***************"
           DISPLAY   "* accept start*"
           DISPLAY   "***************"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  KEY-AREA
           INITIALIZE                  SPA-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           IF    ( WRK-ERRCD          =   SPACE )
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    返却領域編集
           PERFORM 300-END-SEC
      *
           DISPLAY   "***************"
           DISPLAY   "* accept end  *"
           DISPLAY   "***************"
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
           INITIALIZE                  XML-FDGRES
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *
      *    医療機関識別番号セット
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE   "0099"       TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"           TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                           SYS-1010-REC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               MOVE   "0089"       TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    XML情報取り出し
           PERFORM 900-XML-READ-SEC
           IF    ( WRK-ERRCD           NOT =   SPACE )
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           PERFORM 1001-INIT-CHECK-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    電文項目チェック処理
      *****************************************************************
       1001-INIT-CHECK-SEC             SECTION.
      *
           IF    ( FDGREQ-OFFSET       =   SPACE )
               MOVE    ZERO            TO  KEY-OFFSET
           ELSE
               IF    ( FUNCTION TRIM(FDGREQ-OFFSET)    IS  NUMERIC )
                   MOVE    FUNCTION NUMVAL (FDGREQ-OFFSET)
                                       TO  KEY-OFFSET
               ELSE
                   MOVE    "0002"      TO  WRK-ERRCD
               END-IF
           END-IF
      *
           IF    ( WRK-ERRCD       =   SPACE )
               PERFORM 10011-PRTKANRI-CHECK-SEC
           END-IF
      *
           .
       1001-INIT-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    帳票管理確認処理
      *****************************************************************
       10011-PRTKANRI-CHECK-SEC            SECTION.
      *
           UNSTRING    FDGREQ-DATAID       DELIMITED BY "#"
           INTO                            KEY-KEY
                                           KEY-UUID
                                           KEY-GROUP
                                           KEY-RENNUM
                                           KEY-SHORI-RENNUM
           END-UNSTRING
      *
           PERFORM 900-PRTKANRI-KEY-SEL-SEC
           IF    ( FLG-PRTKANRI        NOT =   ZERO )
               MOVE    "0001"          TO  WRK-ERRCD
           END-IF
      *
           .
       10011-MNOTE-H-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 2001-RESPONSE-EDIT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レスポンス編集処理
      *****************************************************************
       2001-RESPONSE-EDIT-SEC              SECTION.
      *
      *
           MOVE    PRTKANRI-PRTNM      TO  FDGRES-FORMID
           MOVE    PRTKANRI-TITLE      TO  FDGRES-FORMNAME
      *
           MOVE    KEY-GROUP(1:8)      TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  FDGRES-PRTYMD
           MOVE    KEY-GROUP(9:6)      TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME            TO  FDGRES-PRTHMS
           MOVE    PRTKANRI-SKYYMD     TO  FDGRES-SRYYMD
      *
           PERFORM 900-PRTDATA-KEY3-SEL-SEC
      *
           MOVE    ZERO                TO  IDXR
      *
           PERFORM UNTIL ( FLG-PRTDATA NOT =   ZERO )
                    OR   ( IDXR           >=   CONST-LIMIT )
      *
               COMPUTE IDXR    =   IDXR    +   1
      *
               IF    ( IDXR                =   1 )
                AND  ( PRTKANRI-TBL-KEY    =   "ONLINE" )
                   PERFORM 20011-PTINF-EDIT-SEC
               END-IF
      *
               IF    ( IDXR                =   1 )
                AND  ( KEY-OFFSET          =   ZERO )
                   MOVE    "["             TO  FDGRES-RPDATA (IDXR)(1:1)
                   MOVE    PRTDATA-PRTDATA TO  FDGRES-RPDATA (IDXR)(2:)
               ELSE
                   MOVE    PRTDATA-PRTDATA TO  FDGRES-RPDATA (IDXR)
               END-IF
      *
               PERFORM 900-PRTDATA-KEY3-FET-SEC
      *
           END-PERFORM
      *
           IF    ( FLG-PRTDATA     =   ZERO )
               COMPUTE WRK-OFFSET  =   KEY-OFFSET + CONST-LIMIT
               MOVE    WRK-OFFSET      TO  WRK-NUMZ
               MOVE    FUNCTION TRIM(WRK-NUMZ)
                                       TO  FDGRES-OFFSET
           ELSE
               MOVE    SPACE           TO  TPRTDATA-PRTDATA
               STRING  FUNCTION TRIM(FDGRES-RPDATA (IDXR) TRAILING)
                                       DELIMITED BY SIZE
                       "]"             DELIMITED BY SIZE
               INTO    TPRTDATA-PRTDATA
               END-STRING
               MOVE    TPRTDATA-PRTDATA TO  FDGRES-RPDATA (IDXR)
           END-IF
      *
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-RESPONSE-EDIT-EXT.
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       20011-PTINF-EDIT-SEC            SECTION.
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
           PERFORM 900-PTNUM-KEY-SEL-SEC
      *
           MOVE    PTNUM-PTNUM         TO  FDGRES-PTNUM
           MOVE    PTINF-NAME          TO  FDGRES-NAME
           MOVE    PTINF-KANANAME      TO  FDGRES-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  FDGRES-BIRTHDAY
           MOVE    PTINF-SEX           TO  FDGRES-SEX
      *
           .
       20011-PTINF-EDIT-EXT.
      *
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  FDGRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME            TO  FDGRES-INFORMATION-TIME
      *
           IF    ( WRK-ERRCD           =   SPACE )
               MOVE    "0000"          TO  WRK-ERRCD
           END-IF
           PERFORM 890-ERRCD-MSG-SEC
           MOVE    WRK-ERRCD           TO  FDGRES-API-RESULT
           MOVE    WRK-ERRMSG          TO  FDGRES-API-RESULT-MSG
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
               IF    ( WRK-SYMD            =   "99999999" )
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-IF
           END-IF
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
      *
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC          SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    月末日取得処理
      *****************************************************************
       800-GETUMATU-SEC        SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD (1 : 6)    TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD
      *
           .
       800-GETUMATU-EXT.
            EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    WRK-ERRCD
           WHEN    "0000"
               MOVE    "処理終了"      TO  WRK-ERRMSG
           WHEN    "0001"
               MOVE    "帳票データが存在しません"
                                       TO  WRK-ERRMSG
           WHEN    "0002"
               MOVE    "オフセットの値に誤りがあります"
                                       TO  WRK-ERRMSG
      *共通エラー
           WHEN    "0089"
      *        ORCSCOMMON のエラー
               EVALUATE    SPA-ERRCD
               WHEN    "1001"
                   MOVE    "職員情報が取得できません"
                                       TO  WRK-ERRMSG
               WHEN    "1002"
                   MOVE    "医療機関情報が取得できません"
                                       TO  WRK-ERRMSG
               WHEN    "1003"
                   MOVE    "システム日付が取得できません"
                                       TO  WRK-ERRMSG
               WHEN    "1005"
                   MOVE    "患者番号構成情報が取得できません"
                                       TO  WRK-ERRMSG
               WHEN    "1015"
                   STRING  "グループ医療機関が不整合です。"
                                               DELIMITED  BY  SIZE
                           "処理を終了して下さい。"
                                               DELIMITED  BY  SIZE
                   INTO    WRK-ERRMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    "システム項目が設定できません"
                                       TO  WRK-ERRMSG
               END-EVALUATE
           WHEN    "0097"
               MOVE   "送信内容に誤りがあります"
                                       TO  WRK-ERRMSG
           WHEN    "0098"
               MOVE   "送信内容の読込ができませんでした"
                                       TO  WRK-ERRMSG
           WHEN    "0099"
               MOVE    "ユーザＩＤが未登録です"
                                       TO  WRK-ERRMSG
           WHEN    OTHER
               MOVE    "返却情報の編集でエラーが発生しました"
                                       TO  WRK-ERRMSG
           END-EVALUATE
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       900-XML-READ-SEC             SECTION.
      *
           IF    ( APILST25-BODY    NOT =   LOW-VALUE )
               DISPLAY "formdatagetv2 object is not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "xml_formdatagetv2req"  TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    APILST25-BODY           TO  APIREQ-OBJECT
           MOVE    ZERO                    TO  APIREQ-MODE
           PERFORM 910-XMLOPEN-SEC
           IF    ( MCP-RC              =   ZERO  OR  1 )
               MOVE    "xml_formdatagetv2req"
                                           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "data"              TO  APIREQ-RECORDNAME
               MOVE    "XMLREAD"           TO  MCP-FUNC
               PERFORM 910-XMLREAD-SEC
               IF    ( MCP-RC              =   ZERO  OR  1 )
                   PERFORM 9001-XML-FILTER-SEC
               ELSE
                   DISPLAY "XMLREAD failure = " MCP-RC
                   MOVE   "0098"            TO  WRK-ERRCD
               END-IF
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
               MOVE   "0097"               TO  WRK-ERRCD
           END-IF
      *
           MOVE    "xml_formdatagetv2req" TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-XMLCLOSE-SEC
       *
           .
       900-XML-READ-EXT.
           EXIT.
      *****************************************************************
      *    XMLフィルタリング処理
      *****************************************************************
       9001-XML-FILTER-SEC             SECTION.
      *
           MOVE    APIREQ-PATIENTINFOREQ
                                       TO  FDGREQ-DATA
      *
           .
       9001-XML-FILTER-EXT.
           EXIT.
      *****************************************************************
      *    XML情報を書き出す
      *****************************************************************
       900-XML-WRITE-SEC             SECTION.
      *
           IF    ( APILST25-BODY    NOT =   LOW-VALUE )
               DISPLAY "formdatagetv2 object is not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           MOVE    "xml_formdatagetv2res" TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    1                       TO  APIREQ-MODE
           PERFORM 910-XMLOPEN-SEC
           IF    ( MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    FDGRES-DATA            TO  APIREQ-PATIENTINFOREQ
           MOVE    "xml_formdatagetv2res" TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           MOVE    "data"                 TO  APIREQ-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           MOVE    "xml_formdatagetv2res" TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-XMLCLOSE-SEC
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    APIREQ-OBJECT       TO  APILST25-BODY
               MOVE    "application/xml"   TO  APILST25-CONTENT-TYPE
           ELSE
               DISPLAY "XMLCLOSE failure = " MCP-RC
               DISPLAY "appoint xml write failure "
           END-IF
      *
           .
       900-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    帳票管理検索処理
      *****************************************************************
       900-PRTKANRI-KEY-SEL-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-PRTKANRI
      *
           INITIALIZE                      PRTKANRI-REC
           MOVE    SPA-HOSPNUM         TO  PRTKANRI-HOSPNUM
           MOVE    KEY-KEY             TO  PRTKANRI-TBL-KEY
           MOVE    KEY-RENNUM          TO  PRTKANRI-RENNUM
           MOVE    KEY-GROUP           TO  PRTKANRI-TBL-GROUP
           MOVE    KEY-UUID            TO  PRTKANRI-TBL-UUID
           MOVE    KEY-SHORI-RENNUM    TO  PRTKANRI-SHORI-RENNUM
           MOVE    PRTKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PRTKANRI-REC
           ELSE
               INITIALIZE                  PRTKANRI-REC
               MOVE    1               TO  FLG-PRTKANRI
           END-IF
      *
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PRTKANRI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    帳票データ検索処理
      *****************************************************************
       900-PRTDATA-KEY3-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-PRTDATA
      *
           INITIALIZE                      PRTDATA-REC
           MOVE    SPA-HOSPNUM         TO  PRTDATA-HOSPNUM
           MOVE    KEY-KEY             TO  PRTDATA-TBL-KEY
           MOVE    KEY-RENNUM          TO  PRTDATA-RENNUM
           MOVE    KEY-GROUP           TO  PRTDATA-TBL-GROUP
           MOVE    KEY-UUID            TO  PRTDATA-TBL-UUID
           MOVE    KEY-SHORI-RENNUM    TO  PRTDATA-SHORI-RENNUM
           MOVE    KEY-OFFSET          TO  PRTDATA-PAGE
           MOVE    PRTDATA-REC         TO  MCPDATA-REC
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PRTDATA-REC
           ELSE
               INITIALIZE                  PRTDATA-REC
               MOVE    1               TO  FLG-PRTDATA
           END-IF
      *
           .
       900-PRTDATA-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    帳票データFETCH処理
      *****************************************************************
       900-PRTDATA-KEY3-FET-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-PRTDATA
      *
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PRTDATA-REC
           ELSE
               INITIALIZE                  PRTDATA-REC
               MOVE    1               TO  FLG-PRTDATA
           END-IF
      *
           .
       900-PRTDATA-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者情報検索処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    PRTDATA-PTID    TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTINF-REC
               MOVE    ZERO        TO  FLG-PTINF
           ELSE
               MOVE    1           TO  FLG-PTINF
               INITIALIZE              PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-EXT.
           EXIT.
      *****************************************************************
      *    患者番号検索処理
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNUM
      *
           INITIALIZE                  PTNUM-REC
           MOVE    SPA-HOSPNUM     TO  PTNUM-HOSPNUM
           MOVE    PRTDATA-PTID    TO  PTNUM-PTID
           MOVE    PTNUM-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNUM-REC
               MOVE    ZERO        TO  FLG-PTNUM
           ELSE
               MOVE    1           TO  FLG-PTNUM
               INITIALIZE              PTNUM-REC
           END-IF
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-KEY-EXT.
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
      *****************************************************************
      *    プログラム終了処理（エラー時）
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           IF    ( WRK-ERRCD           =   "0099" )
               MOVE    404             TO  APILST25-HTTPSTATUS
           ELSE
               PERFORM 9901-EXIT-PROGRAM-SEC
           END-IF
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
      *****************************************************************
      *    プログラム終了処理（エラー時）
      *****************************************************************
       9901-EXIT-PROGRAM-SEC            SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  FDGRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME            TO  FDGRES-INFORMATION-TIME
      *
           PERFORM 890-ERRCD-MSG-SEC
           MOVE    WRK-ERRCD           TO  FDGRES-API-RESULT
           MOVE    WRK-ERRMSG          TO  FDGRES-API-RESULT-MSG
      *
           PERFORM 900-XML-WRITE-SEC
      *
           .
       9901-EXIT-PROGRAM-EXT.
           EXIT.
