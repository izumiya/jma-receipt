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
       PROGRAM-ID.         ORAPI101R1.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : システム管理　診療科・ドクター一覧取得処理
      *  管理者            :
      *  作成日付    作業者        記述
      *  11/07/13    NACL−多々納　新規作成
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
       DATA                DIVISION.
      *FILE                    SECTION.
      *
       WORKING-STORAGE             SECTION.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC X(04).
               05  WRK-SMM             PIC X(02).
               05  WRK-SDD             PIC X(02).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *    時間編集
           03  WRK-THMS.
               05  WRK-THH             PIC X(02).
               05  WRK-TMM             PIC X(02).
               05  WRK-TSS             PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH         PIC X(02).
               05  WRK-HEN-T1          PIC X(01).
               05  WRK-HEN-TMM         PIC X(02).
               05  WRK-HEN-T2          PIC X(01).
               05  WRK-HEN-TSS         PIC X(02).
      *    エラーコード
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-KEICD               PIC X(02).
      *
           03  WRK-MCP-PATHNAME        PIC X(64).
           03  WRK-ZZ9                 PIC ZZZ9.
      *
           03  WRK-CNT                 PIC 9(04).
      *
       01  WRK-XML-AREA.
           03  WRK-IN-DATE                PIC X(10).
      *
           03  WRK-IN-YMD                 PIC X(08).
      *
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
      *    API XML読み込み用定義体
           COPY    "CPAPISYSTEM01REQ.INC".
      *    API XML書き込み用定義体（診療科）
           COPY    "CPDEPARTMENTRES.INC".
      *    API XML書き込み用定義体（ドクター）
           COPY    "CPPHYSICIANRES.INC".
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    職員情報
           COPY  "CPSK1010.INC".
      *
      *    診療内容情報
           COPY  "CPSK1005.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "ORCA-BLOB".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
       01  SCRAREA.
           COPY    "CPAPIMOD.INC".
           COPY    "CPAPILST.INC".
           COPY    "CPAPIPTA.INC".
           COPY    "CPAPILST.INC"    REPLACING
                                     //APILST-// BY //APILST2-//.
           COPY    "CPAPILST.INC"    REPLACING
                                     //APILST-// BY //APILST3-//.
           COPY    "CPAPILST.INC"    REPLACING
                                     //APILST-// BY //APILST4-//.
           COPY    "CPAPILST.INC"    REPLACING
                                     //APILST-// BY //APILST5-//.
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "********************"
           DISPLAY   "* patient res start*"
           DISPLAY   "********************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
      ***  INITIALIZE                      CNT-AREA
           INITIALIZE                      SPA-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    返却領域編集
           PERFORM 300-PTXMLMAKE-SEC
      *
           DISPLAY   "*******************"
           DISPLAY   "* patient res end *"
           DISPLAY   "*******************"
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
           INITIALIZE                      WRK-XML-AREA
           INITIALIZE                      XML-DEPARTMENTRES
           INITIALIZE                      XML-PHYSICIANRES
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Patient Info"       TO  DEPTRES-RESKEY
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  DEPTRES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  DEPTRES-INFORMATION-TIME
      *
      *    医療機関識別番号セット 
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "99"             TO  WRK-ERRCD
               GO  TO         100-INIT-EXT
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
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "89"             TO  WRK-ERRCD
           END-IF
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
      *    XML情報取り出し
           PERFORM 1000-APTLSTREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
           MOVE    ZERO                TO  DEPTRES-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-KEICD
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE    APILST5-CLASS
           WHEN    "01"
      *        診療科一覧
               MOVE    1                   TO  FLG-SYORI
           WHEN    "02"
      *        ドクター一覧
               MOVE    2                   TO  FLG-SYORI
           WHEN    "03"
      *        職員一覧
               MOVE    3                   TO  FLG-SYORI
           WHEN    OTHER
               MOVE    "91"                TO  WRK-ERRCD
           END-EVALUATE
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
      *
      *    一覧処理
           IF      WRK-ERRCD           =   SPACE
               EVALUATE    APILST5-CLASS
               WHEN    "01"
      *            診療科一覧
                   PERFORM 2002-SRYKA-LIST-SEC
               WHEN    "02"
      *            ドクター一覧
                   PERFORM 2003-DRCD-LIST-SEC
               WHEN    "03"
      *            職員一覧
                   PERFORM 2004-SYOCD-LIST-SEC
               END-EVALUATE
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-APTLSTREAD-SEC   SECTION.
      *
           IF      APILST5-BODY        NOT =   LOW-VALUE
               DISPLAY "SYS01REQ-OBJECT not low_value"
           END-IF
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_system01_managereq"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST5-BODY        TO  SYS01REQ-OBJECT
           MOVE    ZERO                TO  SYS01REQ-MODE
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-SYSTEM01MANAGEREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
               MOVE   "97"             TO  WRK-ERRCD
               GO     TO      1000-APTLSTREAD-EXT
           END-IF
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_system01_managereq"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    ORCA-BLOB-OBJECT    TO  SYS01REQ-OBJECT
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-SYSTEM01MANAGEREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               MOVE   "98"             TO  WRK-ERRCD
               DISPLAY "XMLREAD failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLCLOSE"          TO  MCP-FUNC
           MOVE    "xml_system01_managereq"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    ORCA-BLOB-OBJECT    TO  SYS01REQ-OBJECT
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-SYSTEM01MANAGEREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               CONTINUE
           END-IF
      *
           .
       1000-APTLSTREAD-EXT.
           EXIT.
      *****************************************************************
      *    読み込んだXMLのLOW-VALUE を  SPACE に置換
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
           .
       10001-XML-REMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    設定内容チェック
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
           MOVE    SYS01REQ-BASE-DATE  TO  WRK-IN-DATE
      *    処理日
           MOVE    SYS01REQ-BASE-DATE  TO  WRK-HEN-DATE
           MOVE    SPACE               TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD            TO  WRK-IN-YMD
      *    開始日未設定はシステム日付
           IF      WRK-IN-DATE         =   SPACE
               MOVE    SPA-SYSYMD          TO  WRK-IN-YMD
               MOVE    WRK-IN-YMD          TO  WRK-SYMD
               MOVE    ZERO                TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  WRK-IN-DATE
           END-IF
           MOVE    WRK-IN-DATE         TO  DEPTRES-BASE-DATE
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科一覧処理
      *****************************************************************
       2002-SRYKA-LIST-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDX
      *
      *    診療科
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                      SYS-1005-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-IN-YMD          TO  SYS-1005-STYUKYMD
           MOVE    WRK-IN-YMD          TO  SYS-1005-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM
                   UNTIL       (WRK-ERRCD  NOT =   SPACE)  OR
                               (FLG-SYSKANRI   =   1    )
               ADD     1                   TO  IDX
               IF      IDX                 >   100
                   MOVE    "10"                TO  WRK-ERRCD
               ELSE
                   MOVE    SYSKANRI-REC        TO  SYS-1005-REC
                   MOVE    SYS-1005-KBNCD(1:2) TO  DEPTRES-DEP-CODE
                                                                  (IDX)
                   MOVE    SYS-1005-SRYKANAME  TO  DEPTRES-SRYKANAME
                                                                  (IDX)
                   MOVE    SYS-1005-SRYKANAME1 TO  DEPTRES-SRYKANAME1
                                                                  (IDX)
                   MOVE    SYS-1005-SRYKANAME2 TO  DEPTRES-SRYKANAME2
                                                                  (IDX)
                   MOVE    SYS-1005-SRYKANAME3 TO  DEPTRES-SRYKANAME3
                                                                  (IDX)
                   MOVE    SYS-1005-RECESRYKA  TO  DEPTRES-RECESRYKA
                                                                  (IDX)
               END-IF
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX                 =   ZERO
               MOVE    "11"            TO  WRK-ERRCD
           END-IF
           .
       2002-SRYKA-LIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター一覧処理
      *****************************************************************
       2003-DRCD-LIST-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDX
      *
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                      SYS-1010-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    WRK-IN-YMD          TO  SYS-1010-STYUKYMD
           MOVE    WRK-IN-YMD          TO  SYS-1010-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM
                   UNTIL       (WRK-ERRCD  NOT =   SPACE)  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
               IF      SYS-1010-KBNCD(1:1) =   "1"
      *            ドクターのみ
                   ADD     1                   TO  IDX
                   IF      IDX                 >   100
                       MOVE    "10"                TO  WRK-ERRCD
                   ELSE
                       MOVE    SYS-1010-KBNCD(1:5) TO  PHYSRES-DRCD
                                                                 (IDX)
      *                氏名
                       MOVE    SYS-1010-NAME       TO  PHYSRES-NAME
                                                                 (IDX)
                       MOVE    SYS-1010-KANANAME   TO  PHYSRES-KANANAME
                                                                 (IDX)
      *                医籍登録番号
                       MOVE    SYS-1010-ISEKINUM   TO  PHYSRES-ISEKINUM
                                                                 (IDX)
      *                麻薬ライセンス
                       MOVE    SYS-1010-MAYAKULICENSE
                                               TO  PHYSRES-MAYAKULICENSE
                                                                 (IDX)
      *                専門科コード
                       PERFORM VARYING    IDY      FROM    1   BY  1
                               UNTIL      IDY      >   5
                           MOVE    SYS-1010-SEMONKACD-X (IDY)
                                               TO  PHYSRES-SEMONKACD-X
                                                              (IDX IDY)
                       END-PERFORM
                   END-IF
               END-IF
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX                 =   ZERO
               MOVE    "11"            TO  WRK-ERRCD
           END-IF
           .
       2003-DRCD-LIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    職員一覧処理
      *****************************************************************
       2004-SYOCD-LIST-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDX
      *
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                      SYS-1010-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    WRK-IN-YMD          TO  SYS-1010-STYUKYMD
           MOVE    WRK-IN-YMD          TO  SYS-1010-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM
                   UNTIL       (WRK-ERRCD  NOT =   SPACE)  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
               IF      SYS-1010-KBNCD(1:1) NOT =   "1"
      *            ドクター以外
                   ADD     1                   TO  IDX
                   IF      IDX                 >   100
                       MOVE    "10"                TO  WRK-ERRCD
                   ELSE
                       MOVE    SYS-1010-KBNCD(1:5) TO  PHYSRES-DRCD
                                                                 (IDX)
      *                氏名
                       MOVE    SYS-1010-NAME       TO  PHYSRES-NAME
                                                                 (IDX)
                       MOVE    SYS-1010-KANANAME   TO  PHYSRES-KANANAME
                                                                 (IDX)
      *                医籍登録番号
                       MOVE    SYS-1010-ISEKINUM   TO  PHYSRES-ISEKINUM
                                                                 (IDX)
      *                麻薬ライセンス
                       MOVE    SYS-1010-MAYAKULICENSE
                                               TO  PHYSRES-MAYAKULICENSE
                                                                 (IDX)
      *                専門科コード
                       PERFORM VARYING    IDY      FROM    1   BY  1
                               UNTIL      IDY      >   5
                           MOVE    SYS-1010-SEMONKACD-X (IDY)
                                               TO  PHYSRES-SEMONKACD-X
                                                              (IDX IDY)
                       END-PERFORM
                   END-IF
               END-IF
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX                 =   ZERO
               MOVE    "11"            TO  WRK-ERRCD
           END-IF
           .
       2004-SYOCD-LIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-PTXMLMAKE-SEC               SECTION.
      *
           IF      FLG-SYORI           =   2
                                       OR  3
      *        ドクター・職員一覧
               PERFORM 3002-PHYSICIAN-MAKE-SEC
           ELSE
      *        診療科一覧
               PERFORM 3001-DEPARTMENT-MAKE-SEC
           END-IF
      *
           .
       300-PTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理（診療科一覧）
      *****************************************************************
       3001-DEPARTMENT-MAKE-SEC               SECTION.
      *
      *    エラーメッセージ編集
           IF      WRK-ERRCD           =   SPACE
               MOVE    WRK-KEICD           TO  WRK-ERRCD
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               MOVE    "処理終了"          TO  DEPTRES-API-RESULT-MSG
           ELSE
      *        エラー・警告メッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD           TO  DEPTRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  DEPTRES-API-RESULT-MSG
           END-IF
      *
           IF      APILST5-BODY     NOT =   LOW-VALUE
               DISPLAY "DEPTRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_departmentres"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  DEPTRES-MODE
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-DEPARTMENTRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_departmentres"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-DEPARTMENTRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               DISPLAY "XMLWRITE OK      = " MCP-RC
               CONTINUE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLCLOSE"          TO  MCP-FUNC
           MOVE    "xml_departmentres"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-DEPARTMENTRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               DISPLAY "XMLCLOSE OK   = " MCP-RC
               MOVE    DEPTRES-OBJECT      TO  APILST5-BODY 
               MOVE    "application/xml"   TO  APILST5-CONTENT-TYPE
           ELSE
               DISPLAY "XMLCLOSE failure = " MCP-RC
               DISPLAY "patient res xml write failure "
           END-IF
      *
           .
       3001-DEPARTMENT-MAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理（ドクター一覧）
      *****************************************************************
       3002-PHYSICIAN-MAKE-SEC               SECTION.
      *
           MOVE   "Patient Info"       TO  PHYSRES-RESKEY
      *    日付・時間出力編集
           MOVE    DEPTRES-INFORMATION-DATE
                                       TO  PHYSRES-INFORMATION-DATE
           MOVE    DEPTRES-INFORMATION-TIME
                                       TO  PHYSRES-INFORMATION-TIME
           MOVE    ZERO                TO  PHYSRES-API-RESULT
      *    基準日 
           MOVE    WRK-HEN-DATE        TO  PHYSRES-BASE-DATE
      *    エラーメッセージ編集
           IF      WRK-ERRCD           =   SPACE
               MOVE    WRK-KEICD           TO  WRK-ERRCD
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               MOVE    "処理終了"          TO  PHYSRES-API-RESULT-MSG
           ELSE
      *        エラー・警告メッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD           TO  PHYSRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  PHYSRES-API-RESULT-MSG
           END-IF
      *
           IF      APILST5-BODY     NOT =   LOW-VALUE
               DISPLAY "PHYSRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_physicianres"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  PHYSRES-MODE
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-PHYSICIANRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_physicianres"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-PHYSICIANRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               DISPLAY "XMLWRITE OK      = " MCP-RC
               CONTINUE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLCLOSE"          TO  MCP-FUNC
           MOVE    "xml_physicianres"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-PHYSICIANRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               DISPLAY "XMLCLOSE OK   = " MCP-RC
               MOVE    PHYSRES-OBJECT      TO  APILST5-BODY 
               MOVE    "application/xml"   TO  APILST5-CONTENT-TYPE
           ELSE
               DISPLAY "XMLCLOSE failure = " MCP-RC
               DISPLAY "patient res xml write failure "
           END-IF
      *
           .
       3002-PHYSICIAN-MAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE           TO  WRK-HEN-DATE
           ELSE
               INITIALIZE                  WRK-HEN-DATE
               MOVE    WRK-SYY             TO  WRK-HEN-YY
               MOVE    WRK-SMM             TO  WRK-HEN-MM
               MOVE    WRK-SDD             TO  WRK-HEN-DD
               MOVE    "-"                 TO  WRK-HEN-H1
               MOVE    "-"                 TO  WRK-HEN-H2
               IF      WRK-SYMD            =   "99999999"
                   MOVE    "12"                TO  WRK-HEN-MM
                   MOVE    "31"                TO  WRK-HEN-DD
               END-IF
           END-IF
      *
           INITIALIZE                  WRK-HEN-TIME
           MOVE    WRK-THH             TO  WRK-HEN-THH
           MOVE    WRK-TMM             TO  WRK-HEN-TMM
           MOVE    WRK-TSS             TO  WRK-HEN-TSS
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           .
       801-DAYHEN01-EXT.
           EXIT.
      *****************************************************************
      *    日付変換編集処理
      *****************************************************************
       802-DAYHEN02-SEC                SECTION.
      *
           INITIALIZE                  WRK-SYMD
           MOVE    WRK-HEN-YY          TO  WRK-SYY
           MOVE    WRK-HEN-MM          TO  WRK-SMM
           MOVE    WRK-HEN-DD          TO  WRK-SDD
           IF      WRK-SYMD            =   "99991231"
               MOVE    "99"                TO  WRK-SMM
               MOVE    "99"                TO  WRK-SDD
           END-IF
      *
           INITIALIZE                  WRK-THMS
           MOVE    WRK-HEN-THH         TO  WRK-THH
           MOVE    WRK-HEN-TMM         TO  WRK-TMM
           MOVE    WRK-HEN-TSS         TO  WRK-TSS
           .
       802-DAYHEN02-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    WRK-ERRCD
           WHEN    "10"
               MOVE    "対象が１００件以上となります。"
                                               TO  WRK-ERRMSG
           WHEN    "11"
               MOVE    "対象がありません。"
                                               TO  WRK-ERRMSG
      *共通エラー
           WHEN    "89"
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
                           "処理を終了して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    "システム項目が設定できません"
                                               TO  WRK-ERRMSG
               END-EVALUATE
           WHEN    "90"
               MOVE    "他端末使用中"          TO  WRK-ERRMSG
           WHEN    "91"
               MOVE    "処理区分未設定"        TO  WRK-ERRMSG
           WHEN    "97"
               MOVE    "送信内容に誤りがあります。"
                                               TO  WRK-ERRMSG
           WHEN    "98"
               MOVE    "送信内容の読込ができませんでした"
                                               TO  WRK-ERRMSG
           WHEN    "99"
               MOVE    "ユーザＩＤ未登録。"
                                               TO  WRK-ERRMSG
           END-EVALUATE
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理読込処理
      *****************************************************************
       900-SYSKANRI-READ-SEC            SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC         TO  SYSKANRI-REC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
               INITIALIZE                      SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
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
