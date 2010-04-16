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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCHCN02SUB.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : 退院証明書用入院取得サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/06/08    NACL−小豆沢  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-多々納  07/05/09  グループ診療対応
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-NYUINACT                PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-MATCH                   PIC 9(01).
      *    添字領域
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
           03  IDX1                        PIC 9(04).
           03  IDX2                        PIC 9(04).
           03  IDX3                        PIC 9(04).
           03  IDX4                        PIC 9(04).
           03  IDX5                        PIC 9(04).
           03  IDX6                        PIC 9(04).
           03  IDX7                        PIC 9(04).
           03  NYUIN-IDX                   PIC 9(04).
           03  ST-IDX                      PIC 9(04).
           03  ED-IDX                      PIC 9(04).
      *
      *    入院日置換え用
           03  WRK-DDX                     PIC X(02).
           03  WRK-DD9          REDEFINES  WRK-DDX
                                           PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-GAIHAKU-SRYCD           PIC X(09).
           03  WRK-MEISAISU                PIC 9(03).
           03  WRK-KSNKBN                  PIC 9(03).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    入院会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院診療行為マスター
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    点数マスタ−
           COPY    "CPTENSU.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者入院履歴マスタ−
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    通算日数取得サブ
           COPY "CPORCSTUSAN.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    退院証明書用入院取得パラメタ
           COPY    "CPORCHCN02SUB.INC".
      *
       PROCEDURE                   DIVISION    USING
            ORCHCN02SUB-AREA
            SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  LNK-ORCHCN02SUB-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-ORCHCN02SUB-RC
      *
      *    パラメータチェック
           PERFORM 100-PRM-CHK-SEC
           IF    ( LNK-ORCHCN02SUB-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    主処理
           IF      LNK-ORCHCN02SUB-RC      =    ZERO
               PERFORM 200-MAIN-SEC
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    パラメータチェック
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    算定年月チェック
           IF    ( LNK-ORCHCN02SUB-HOSPNUM   =   ZERO  )   OR
                 ( LNK-ORCHCN02SUB-PTID      =   SPACE )   OR
                 ( LNK-ORCHCN02SUB-NYUINYMD  =   SPACE )   OR
                 ( LNK-ORCHCN02SUB-TAIINYMD  =   SPACE )
               MOVE    1               TO  LNK-ORCHCN02SUB-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    患者マスター検索
           INITIALIZE                       PTINF-REC
           MOVE    LNK-ORCHCN02SUB-HOSPNUM  TO  PTINF-HOSPNUM
           MOVE    LNK-ORCHCN02SUB-PTID     TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    1               TO  LNK-ORCHCN02SUB-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主取得
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
      *入院履歴から自院歴の日数取得（入院歴作成機能による履歴分の
      *                              日数取得）
           INITIALIZE                           PTNYUINRRK-REC
           MOVE    LNK-ORCHCN02SUB-HOSPNUM  TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-ORCHCN02SUB-PTID     TO  PTNYUINRRK-PTID
           MOVE    LNK-ORCHCN02SUB-NYUINYMD TO  PTNYUINRRK-NYUINYMD
           MOVE    "tbl_ptnyuinrrk"         TO  MCP-TABLE
           MOVE    "key48"                  TO  MCP-PATHNAME
           PERFORM 910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK          =   ZERO
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key48"             TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
               IF    FLG-PTNYUINRRK        =   ZERO
                   ADD   PTNYUINRRK-TAISYONISSU
                                       TO   LNK-ORCHCN02SUB-NISSU(1)
               END-IF
           END-IF
      *    患者入院履歴クローズ
           MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
           MOVE    "key48"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *入院料の取得
           INITIALIZE                           NYUINACCT-REC
           MOVE    LNK-ORCHCN02SUB-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-ORCHCN02SUB-PTID     TO  NACCT-PTID
           MOVE    LNK-ORCHCN02SUB-NYUINYMD(1:6)
                                            TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"          TO  MCP-TABLE
           MOVE    "key34"                  TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key34"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
      *
                   IF   (LNK-ORCHCN02SUB-NYUINYMD(1:6)  <= NACCT-SRYYM)
                                         AND
                        (LNK-ORCHCN02SUB-TAIINYMD(1:6)  >= NACCT-SRYYM)
      *                該当入院行為テーブルの読み込み
                       PERFORM    210-NYUINACT-GET-SEC
      *                入院基本料単位の入院日数カウント
                       PERFORM    210-NYUIN-COUNT-SEC
                   END-IF
      *
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key34"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    入院会計クローズ
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key34"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *
      *選定対象日数の取得
           INITIALIZE                       ORCSTUSANAREA
           MOVE    LNK-ORCHCN02SUB-HOSPNUM  TO  STUSAN-HOSPNUM
           MOVE    LNK-ORCHCN02SUB-PTID     TO  STUSAN-PTID
           MOVE    LNK-ORCHCN02SUB-TAIINYMD TO  STUSAN-KJNYMD
           CALL    "ORCSTUSAN"     USING        ORCSTUSANAREA
                                                SPA-AREA
           MOVE    STUSAN-NISSU05           TO  LNK-ORCHCN02SUB-TKIKAN
      *
      *
      *   結果表示
      *     PERFORM    VARYING   IDX1   FROM  1  BY  1
      *                UNTIL     (IDX1   >     5)
      *          DISPLAY  "IDX1 = " IDX1
      *          DISPLAY  "LNK-ORCHCN02SUB-SYUBETUCD = "
      *                    LNK-ORCHCN02SUB-SYUBETUCD(IDX1)
      *          DISPLAY  "LNK-ORCHCN02SUB-SYUBETUMEI = "
      *                    LNK-ORCHCN02SUB-SYUBETUMEI(IDX1)
      *          DISPLAY  "LNK-ORCHCN02SUB-NISSU = "
      *                    LNK-ORCHCN02SUB-NISSU(IDX1)
      *     END-PERFORM
            DISPLAY  "LNK-ORCHCN02SUB-TKIKAN = " LNK-ORCHCN02SUB-TKIKAN
            DISPLAY  "STUSAN-RC = " STUSAN-RC
      *
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料単位の日数カウント
      *****************************************************************
       210-NYUIN-COUNT-SEC            SECTION.
      *
           MOVE       ZERO      TO    FLG-MATCH
      *
           PERFORM    VARYING   NYUIN-IDX   FROM   1   BY   1
             UNTIL   (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX) = SPACE) OR
                     (FLG-MATCH        =   1 )     OR
                     (NYUIN-IDX    >   10)
      *        点数マスタより入院料名称の取得（ワークとの比較用）
               INITIALIZE    TNS-TENSU-REC
               MOVE   NSRY-SRYCD(1)   TO    TNS-SRYCD
               MOVE   NACCT-SRYYM     TO    TNS-YUKOSTYMD(1:6)
                                            TNS-YUKOEDYMD(1:6)
               MOVE   "01"            TO    TNS-YUKOSTYMD(7:2)
                                            TNS-YUKOEDYMD(7:2)
               PERFORM    910-TENSU-READ-SEC
               EVALUATE   NSRY-SRYCD(1)
                   WHEN   "111111111"
                      MOVE   "有床診療所入院基本料１"  TO   TNS-NAME
                   WHEN   "222222222"
                      MOVE   "有床診療所入院基本料２"  TO   TNS-NAME
                   WHEN   "333333333"
                      MOVE   "有床診療所入院基本料３"  TO   TNS-NAME
               END-EVALUATE
               IF    (NACCT-SRYYM      >=   "200604")    AND
                     (NSRY-SRYCD(1)    =    "190097010"  OR
                                            "190097110"  OR
                                            "190119510"  OR
                                            "190119610")
                   MOVE   "111111111"     TO    NSRY-SRYCD(1)
                   MOVE   "有床診療所入院基本料１"  TO   TNS-NAME
               END-IF
               IF    (NACCT-SRYYM      >=   "200604")    AND
                     (NSRY-SRYCD(1)    =    "190097210"  OR
                                            "190097310"  OR
                                            "190119710"  OR
                                            "190119810")
                   MOVE   "222222222"     TO    NSRY-SRYCD(1)
                   MOVE   "有床診療所入院基本料２"  TO   TNS-NAME
               END-IF
               IF    (NACCT-SRYYM      >=   "201004")    AND
                     (NSRY-SRYCD(1)    =    "190135110"  OR
                                            "190135210"  OR
                                            "190135310")
                   MOVE   "333333333"     TO    NSRY-SRYCD(1)
                   MOVE   "有床診療所入院基本料３"  TO   TNS-NAME
               END-IF
      *
               IF   (NSRY-SRYCD(1)  =
                          LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX))  AND
                    (TNS-NAME   = 
                          LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX))
                    MOVE   1      TO    FLG-MATCH
                    IF   NACCT-SRYYM  =  LNK-ORCHCN02SUB-NYUINYMD(1:6)
                         MOVE   LNK-ORCHCN02SUB-NYUINYMD(7:2) 
                                         TO   WRK-DDX
                         MOVE   WRK-DD9  TO   ST-IDX
                    ELSE
                         MOVE    1       TO   ST-IDX
                    END-IF
                    IF   NACCT-SRYYM  =  LNK-ORCHCN02SUB-TAIINYMD(1:6)
                         MOVE   LNK-ORCHCN02SUB-TAIINYMD(7:2) 
                                         TO   WRK-DDX
                         MOVE   WRK-DD9  TO   ED-IDX
                    ELSE
                         MOVE   31       TO   ED-IDX
                    END-IF
                    PERFORM   VARYING   IDX1   FROM  ST-IDX  BY  1
                      UNTIL   IDX1      >   ED-IDX
                          IF   NACCT-DAY(IDX1)    NOT =  ZERO
                               ADD   1     TO
                                       LNK-ORCHCN02SUB-NISSU(NYUIN-IDX)
                          END-IF
                    END-PERFORM
               END-IF
           END-PERFORM
      *
      *
           IF   (FLG-MATCH     =     ZERO)   AND
                (NYUIN-IDX     <=       10)
               MOVE  NSRY-SRYCD(1)   TO
                                 LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
      *        点数マスタより入院料名称の取得
               INITIALIZE    TNS-TENSU-REC
               MOVE   LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                                            TO  TNS-SRYCD
               MOVE   NACCT-SRYYM     TO    TNS-YUKOSTYMD(1:6)
                                            TNS-YUKOEDYMD(1:6)
               MOVE   "01"            TO    TNS-YUKOSTYMD(7:2)
                                            TNS-YUKOEDYMD(7:2)
               PERFORM    910-TENSU-READ-SEC
               MOVE     TNS-NAME      TO
                             LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               IF    (NACCT-SRYYM      >=   "200604")    AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190097010"  OR
                                            "190097110"  OR
                                            "190119510"  OR
                                            "190119610"  OR
                                            "111111111")
                   MOVE   "111111111"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "有床診療所入院基本料１"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
               IF    (NACCT-SRYYM      >=   "200604")    AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190097210"  OR
                                            "190097310"  OR
                                            "190119710"  OR
                                            "190119810"  OR
                                            "222222222")
                   MOVE   "222222222"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "有床診療所入院基本料２"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
               IF    (NACCT-SRYYM      >=   "201004")    AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190135110"  OR
                                            "190135210"  OR
                                            "190135310"  OR
                                            "333333333")
                   MOVE   "333333333"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "有床診療所入院基本料３"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
      *
               IF   NACCT-SRYYM  =  LNK-ORCHCN02SUB-NYUINYMD(1:6)
                    MOVE   LNK-ORCHCN02SUB-NYUINYMD(7:2) 
                                    TO   WRK-DDX
                    MOVE   WRK-DD9  TO   ST-IDX
               ELSE
                    MOVE    1       TO   ST-IDX
               END-IF
               IF   NACCT-SRYYM  =  LNK-ORCHCN02SUB-TAIINYMD(1:6)
                    MOVE   LNK-ORCHCN02SUB-TAIINYMD(7:2) 
                                    TO   WRK-DDX
                    MOVE   WRK-DD9  TO   ED-IDX
               ELSE
                    MOVE   31       TO   ED-IDX
               END-IF
               PERFORM   VARYING   IDX1   FROM  ST-IDX  BY  1
                 UNTIL   IDX1      >   ED-IDX
                     IF   NACCT-DAY(IDX1)    NOT =  ZERO
                          ADD   1     TO
                                  LNK-ORCHCN02SUB-NISSU(NYUIN-IDX)
                     END-IF
               END-PERFORM
           END-IF
      *
           .
       210-NYUIN-COUNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為取得
      *****************************************************************
       210-NYUINACT-GET-SEC            SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-NYUINACT-SELECT-SEC
           IF      FLG-NYUINACT        =   ZERO
               MOVE    "tbl_nyuinact"  TO  MCP-TABLE
               MOVE    "key9"          TO  MCP-PATHNAME
               PERFORM   910-NYUINACT-READ-SEC
           END-IF
      *
      *    入院行為クローズ
           MOVE     "tbl_nyuinact"     TO  MCP-TABLE
           MOVE     "key9"             TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       210-NYUINACT-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTINF-REC
                   MOVE    ZERO        TO  FLG-PTINF
               ELSE
                   INITIALIZE              PTINF-REC
                   MOVE    1           TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター検索
      *****************************************************************
       910-NYUINACCT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター読込
      *****************************************************************
       910-NYUINACCT-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  NYUINACCT-REC
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                     NYUINACCT-REC
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為マスター検索
      *****************************************************************
       910-NYUINACT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACT
           ELSE
               MOVE    1                  TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為マスター読込
      *****************************************************************
       910-NYUINACT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACT-REC
               MOVE    ZERO                TO  FLG-NYUINACT
           ELSE
               INITIALIZE                      NYUINACT-REC
               MOVE    1                   TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴マスター検索
      *****************************************************************
       910-PTNYUINRRK-SELECT-SEC        SECTION.
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-PTNYUINRRK
           ELSE
               MOVE    1                  TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴マスター読込
      *****************************************************************
       910-PTNYUINRRK-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTNYUINRRK-REC
               MOVE    ZERO                TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                      PTNYUINRRK-REC
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込（主キー）
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   MOVE    1                   TO  FLG-TENSU
                   INITIALIZE                  TNS-TENSU-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
