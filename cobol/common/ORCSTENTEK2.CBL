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
       PROGRAM-ID.                 ORCSTENTEK2.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院会計照会
      *  コンポーネント名  : 入院用点滴加算抽出処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/03/27    NACL−小豆沢  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *
      *平成２０年４月改定対応
      *  04.02.00    NACL-小豆沢  08/03/26  無菌製剤処理加算を基本項目に変更
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
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
           03  FLG-END             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-NYUINACCT       PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-ZAINUM-MATCH    PIC 9(01).
           03  FLG-TENTEK-SET      PIC 9(01).
           03  FLG-CHUJOU-SET      PIC 9(01).
           03  FLG-SEIMITSU-SET    PIC 9(01).
           03  FLG-TAISYOUGAI      PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-MEISAISU        PIC 9(01).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    診療行為マスター
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    入院診療会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   算定履歴更新サブ
           COPY    "CPORCSSANTEI.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    点滴加算抽出パラメタ領域
           COPY    "CPORCSTENTEK2.INC".
      *
       PROCEDURE                   DIVISION    USING
           ORCSTENTEK2AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  LNK-ORCSTEN2-OUT-AREA
      *
      *    パラメータチェック
           PERFORM 100-PRM-CHK-SEC
           IF    ( LNK-ORCSTEN2-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    主処理
           IF      LNK-ORCSTEN2-RC       =    ZERO
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
           IF    ( LNK-ORCSTEN2-SRYYM  =   SPACE )
               MOVE    1               TO  LNK-ORCSTEN2-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    患者マスター検索
           INITIALIZE                      PTINF-REC
           MOVE    LNK-ORCSTEN2-HOSPNUM    TO  PTINF-HOSPNUM
           MOVE    LNK-ORCSTEN2-PTID       TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    2               TO  LNK-ORCSTEN2-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    保険組合せチェック
           IF     LNK-ORCSTEN2-HKNCOMBI    =   ZERO
               MOVE    3               TO  LNK-ORCSTEN2-RC
               GO  TO  100-PRM-CHK-EXT
           ELSE

               INITIALIZE                          HKNCOMBI-REC
               MOVE    LNK-ORCSTEN2-HOSPNUM    TO  COMB-HOSPNUM
               MOVE    LNK-ORCSTEN2-PTID       TO  COMB-PTID
               MOVE    LNK-ORCSTEN2-HKNCOMBI   TO  COMB-HKNCOMBINUM
               MOVE    "tbl_hkncombi"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-SELECT-SEC
               IF      FLG-HKNCOMBI            NOT =   ZERO
                   MOVE    3               TO  LNK-ORCSTEN2-RC
               ELSE
                   MOVE    "tbl_hkncombi"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 910-HKNCOMBI-READ-SEC
                   IF   FLG-HKNCOMBI           NOT =   ZERO
                        MOVE    3              TO  LNK-ORCSTEN2-RC
                   END-IF
               END-IF
           END-IF
      *
      *    保険組合せクローズ
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           IF  LNK-ORCSTEN2-RC      NOT =    ZERO
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    入院中のチェック（入院会計の保険組合せを検索）
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-ORCSTEN2-HOSPNUM    TO  NACCT-HOSPNUM
           MOVE    LNK-ORCSTEN2-PTID       TO  NACCT-PTID
           MOVE    LNK-ORCSTEN2-SRYYM      TO  NACCT-SRYYM
           MOVE    "5"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           NOT =   ZERO
               MOVE    4                   TO  LNK-ORCSTEN2-RC
           ELSE
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT          NOT =   ZERO
                    MOVE    4              TO  LNK-ORCSTEN2-RC
               END-IF
           END-IF
      *
      *    入院会計クローズ
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    主取得
      *****************************************************************
      *
       200-MAIN-SEC            SECTION.
           IF     LNK-ORCSTEN2-SRYYM    <=    "200803"
               PERFORM    210-CHUSYA1-SEC
           ELSE
               PERFORM    220-CHUSYA2-SEC
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    注射（点滴・中心静脈）の取り込み（平成２０年３月まで）
      *****************************************************************
       210-CHUSYA1-SEC      SECTION.
      *
      *    点滴注射の取り込み
           INITIALIZE                          SRYACCT-REC
           MOVE    LNK-ORCSTEN2-HOSPNUM    TO  ACCT-HOSPNUM
           MOVE    "1"                     TO  ACCT-NYUGAIKBN
           MOVE    LNK-ORCSTEN2-PTID       TO  ACCT-PTID
           MOVE    LNK-ORCSTEN2-SRYYM      TO  ACCT-SRYYM
           MOVE    "33"                    TO  ACCT-SRYKBN
           MOVE    "tbl_sryacct"           TO  MCP-TABLE
           MOVE    "key14"                 TO  MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT             =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT    =   1)
                 IF   (ACCT-ZAITEN     NOT =    ZERO)    AND
                      (ACCT-ZAIKAISU   NOT =    ZERO)    AND
                      (ACCT-MEISAISU   =        1   )    AND
                      (ACCT-JIHOKBN    =       SPACE)    AND
                      (ACCT-HKNCOMBI   =   LNK-ORCSTEN2-HKNCOMBI)  AND
                      (ACCT-SRYCDTOTAL =   130003810)    AND
                      (FLG-TAISYOUGAI  =   ZERO)
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF   ACCT-DAY(1 IDX1)    NOT =   ZERO
                               MOVE  1     TO   FLG-TENTEK-SET
                               ADD   1     TO 
                                    LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                               MOVE  LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                                        TO  WRK-MEISAISU
                               ADD   ACCT-ZAITEN     TO 
                                    LNK-ORCSTEN2-TENTEK-ZAITEN(IDX1)
                               ADD   ACCT-SRYCDTOTAL     TO 
                                    LNK-ORCSTEN2-TENTEK-SRYCDTOTAL(IDX1)
                               MOVE   "130003810"    TO
                                    LNK-ORCSTEN2-TENTEK-SRYCD
                                                  (IDX1 WRK-MEISAISU)
                               PERFORM   300-ZAINUM-SET-SEC
                           END-IF
                       END-PERFORM
                 END-IF
                 MOVE     "tbl_sryacct"      TO  MCP-TABLE
                 MOVE     "key14"            TO  MCP-PATHNAME
                 PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *    診療会計クローズ
           MOVE     "tbl_sryacct"    TO  MCP-TABLE
           MOVE     "key14"          TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *     
      *    点滴注射（乳幼児）＋点滴注射（乳幼児）加算の取り込み
           INITIALIZE                          SRYACCT-REC
           MOVE    LNK-ORCSTEN2-HOSPNUM    TO  ACCT-HOSPNUM
           MOVE    "1"                     TO  ACCT-NYUGAIKBN
           MOVE    LNK-ORCSTEN2-PTID       TO  ACCT-PTID
           MOVE    LNK-ORCSTEN2-SRYYM      TO  ACCT-SRYYM
           MOVE    "33"                    TO  ACCT-SRYKBN
           MOVE    "tbl_sryacct"           TO  MCP-TABLE
           MOVE    "key14"                 TO  MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT             =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT    =   1)
                 IF   (ACCT-ZAITEN     NOT =    ZERO)    AND
                      (ACCT-ZAIKAISU   NOT =    ZERO)    AND
                      (ACCT-MEISAISU   =        2   )    AND
                      (ACCT-JIHOKBN    =       SPACE)    AND
                      (ACCT-HKNCOMBI   =   LNK-ORCSTEN2-HKNCOMBI)  AND
                      (ACCT-SRYCDTOTAL =   260013180)    AND
                      (FLG-TAISYOUGAI  =   ZERO)
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF   ACCT-DAY(1 IDX1)    NOT =   ZERO
                               MOVE  1     TO   FLG-TENTEK-SET
                               ADD   ACCT-ZAITEN     TO 
                                    LNK-ORCSTEN2-TENTEK-ZAITEN(IDX1)
                               ADD   ACCT-SRYCDTOTAL     TO 
                                    LNK-ORCSTEN2-TENTEK-SRYCDTOTAL(IDX1)
                               ADD   1     TO 
                                    LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                               MOVE  LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                                        TO  WRK-MEISAISU
                               MOVE   "130003710"    TO
                                    LNK-ORCSTEN2-TENTEK-SRYCD
                                                  (IDX1 WRK-MEISAISU)
                               ADD   1     TO 
                                    LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                               MOVE  LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                                        TO  WRK-MEISAISU
                               MOVE   "130009470"    TO
                                    LNK-ORCSTEN2-TENTEK-SRYCD
                                                  (IDX1 WRK-MEISAISU)
                               PERFORM   300-ZAINUM-SET-SEC
                           END-IF
                       END-PERFORM
                 END-IF
                 MOVE     "tbl_sryacct"      TO  MCP-TABLE
                 MOVE     "key14"            TO  MCP-PATHNAME
                 PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *    診療会計クローズ
           MOVE     "tbl_sryacct"    TO  MCP-TABLE
           MOVE     "key14"          TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    中心静脈注射の取り込み
           INITIALIZE                          SRYACCT-REC
           MOVE    LNK-ORCSTEN2-HOSPNUM    TO  ACCT-HOSPNUM
           MOVE    "1"                     TO  ACCT-NYUGAIKBN
           MOVE    LNK-ORCSTEN2-PTID       TO  ACCT-PTID
           MOVE    LNK-ORCSTEN2-SRYYM      TO  ACCT-SRYYM
           MOVE    "33"                    TO  ACCT-SRYKBN
           MOVE    "tbl_sryacct"           TO  MCP-TABLE
           MOVE    "key14"                 TO  MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT             =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT    =   1)
                 IF   (ACCT-ZAITEN     NOT =    ZERO)    AND
                      (ACCT-ZAIKAISU   NOT =    ZERO)    AND
                      (ACCT-MEISAISU   =        1   )    AND
                      (ACCT-JIHOKBN    =       SPACE)    AND
                      (ACCT-HKNCOMBI   =   LNK-ORCSTEN2-HKNCOMBI)  AND
                      (ACCT-SRYCDTOTAL =   130004410)    AND
                      (FLG-TAISYOUGAI  =   ZERO)
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF   ACCT-DAY(1 IDX1)    NOT =   ZERO
                               MOVE  1     TO   FLG-CHUJOU-SET
                               ADD   1     TO 
                                    LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                               MOVE  LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                                        TO  WRK-MEISAISU
                               ADD   ACCT-ZAITEN     TO 
                                    LNK-ORCSTEN2-CHUJOU-ZAITEN(IDX1)
                               ADD   ACCT-SRYCDTOTAL     TO 
                                    LNK-ORCSTEN2-CHUJOU-SRYCDTOTAL(IDX1)
                               MOVE   "130004410"    TO
                                    LNK-ORCSTEN2-CHUJOU-SRYCD
                                                  (IDX1 WRK-MEISAISU)
                               PERFORM   300-ZAINUM-SET-SEC
                           END-IF
                       END-PERFORM
                 END-IF
                 MOVE     "tbl_sryacct"      TO  MCP-TABLE
                 MOVE     "key14"            TO  MCP-PATHNAME
                 PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *    診療会計クローズ
           MOVE     "tbl_sryacct"    TO  MCP-TABLE
           MOVE     "key14"          TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    中心静脈注射＋中心静脈注射（乳幼児）加算の取り込み
           INITIALIZE                          SRYACCT-REC
           MOVE    LNK-ORCSTEN2-HOSPNUM    TO  ACCT-HOSPNUM
           MOVE    "1"                     TO  ACCT-NYUGAIKBN
           MOVE    LNK-ORCSTEN2-PTID       TO  ACCT-PTID
           MOVE    LNK-ORCSTEN2-SRYYM      TO  ACCT-SRYYM
           MOVE    "33"                    TO  ACCT-SRYKBN
           MOVE    "tbl_sryacct"           TO  MCP-TABLE
           MOVE    "key14"                 TO  MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT             =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT    =   1)
                 IF   (ACCT-ZAITEN     NOT =    ZERO)    AND
                      (ACCT-ZAIKAISU   NOT =    ZERO)    AND
                      (ACCT-MEISAISU   =        2   )    AND
                      (ACCT-JIHOKBN    =       SPACE)    AND
                      (ACCT-HKNCOMBI   =   LNK-ORCSTEN2-HKNCOMBI)  AND
                      (ACCT-SRYCDTOTAL =   260015580)    AND
                      (FLG-TAISYOUGAI  =   ZERO)
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF   ACCT-DAY(1 IDX1)    NOT =   ZERO
                               MOVE  1     TO   FLG-CHUJOU-SET
                               ADD   ACCT-ZAITEN     TO 
                                    LNK-ORCSTEN2-CHUJOU-ZAITEN(IDX1)
                               ADD   ACCT-SRYCDTOTAL     TO 
                                    LNK-ORCSTEN2-CHUJOU-SRYCDTOTAL(IDX1)
                               ADD   1     TO 
                                    LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                               MOVE  LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                                        TO  WRK-MEISAISU
                               MOVE   "130004410"    TO
                                    LNK-ORCSTEN2-CHUJOU-SRYCD
                                                  (IDX1 WRK-MEISAISU)
                               ADD   1     TO 
                                    LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                               MOVE  LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                                        TO  WRK-MEISAISU
                               MOVE   "130011170"    TO
                                    LNK-ORCSTEN2-CHUJOU-SRYCD
                                                  (IDX1 WRK-MEISAISU)
                               PERFORM   300-ZAINUM-SET-SEC
                           END-IF
                       END-PERFORM
                 END-IF
                 MOVE     "tbl_sryacct"      TO  MCP-TABLE
                 MOVE     "key14"            TO  MCP-PATHNAME
                 PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *    診療会計クローズ
           MOVE     "tbl_sryacct"    TO  MCP-TABLE
           MOVE     "key14"          TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    点滴注射（無菌製剤処理）加算の取り込み
           INITIALIZE                          SRYACCT-REC
           MOVE    LNK-ORCSTEN2-HOSPNUM    TO  ACCT-HOSPNUM
           MOVE    "1"                     TO  ACCT-NYUGAIKBN
           MOVE    LNK-ORCSTEN2-PTID       TO  ACCT-PTID
           MOVE    LNK-ORCSTEN2-SRYYM      TO  ACCT-SRYYM
           MOVE    "33"                    TO  ACCT-SRYKBN
           MOVE    "tbl_sryacct"           TO  MCP-TABLE
           MOVE    "key14"                 TO  MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT             =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT    =   1)
                 IF   (ACCT-ZAITEN     NOT =    ZERO)    AND
                      (ACCT-ZAIKAISU   NOT =    ZERO)    AND
                      (ACCT-MEISAISU   =        1   )    AND
                      (ACCT-JIHOKBN    =       SPACE)    AND
                      (ACCT-HKNCOMBI   =   LNK-ORCSTEN2-HKNCOMBI)  AND
                      (ACCT-SRYCDTOTAL =   130008270)    AND
                      (FLG-TAISYOUGAI  =   ZERO)
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF   ACCT-DAY(1 IDX1)    NOT =   ZERO
                               MOVE  1     TO   FLG-TENTEK-SET
                               ADD   1     TO 
                                    LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                               MOVE  LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                                        TO  WRK-MEISAISU
                               ADD   ACCT-ZAITEN     TO 
                                    LNK-ORCSTEN2-TENTEK-ZAITEN(IDX1)
                               ADD   ACCT-SRYCDTOTAL     TO 
                                    LNK-ORCSTEN2-TENTEK-SRYCDTOTAL(IDX1)
                               MOVE   "130008270"    TO
                                    LNK-ORCSTEN2-TENTEK-SRYCD
                                                  (IDX1 WRK-MEISAISU)
                               PERFORM   300-ZAINUM-SET-SEC
                           END-IF
                       END-PERFORM
                 END-IF
                 MOVE     "tbl_sryacct"      TO  MCP-TABLE
                 MOVE     "key14"            TO  MCP-PATHNAME
                 PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *    診療会計クローズ
           MOVE     "tbl_sryacct"    TO  MCP-TABLE
           MOVE     "key14"          TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    中心静脈注射（無菌製剤処理）加算の取り込み
           INITIALIZE                          SRYACCT-REC
           MOVE    LNK-ORCSTEN2-HOSPNUM    TO  ACCT-HOSPNUM
           MOVE    "1"                     TO  ACCT-NYUGAIKBN
           MOVE    LNK-ORCSTEN2-PTID       TO  ACCT-PTID
           MOVE    LNK-ORCSTEN2-SRYYM      TO  ACCT-SRYYM
           MOVE    "33"                    TO  ACCT-SRYKBN
           MOVE    "tbl_sryacct"           TO  MCP-TABLE
           MOVE    "key14"                 TO  MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT             =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT    =   1)
                 IF   (ACCT-ZAITEN     NOT =    ZERO)    AND
                      (ACCT-ZAIKAISU   NOT =    ZERO)    AND
                      (ACCT-MEISAISU   =        1   )    AND
                      (ACCT-JIHOKBN    =       SPACE)    AND
                      (ACCT-HKNCOMBI   =   LNK-ORCSTEN2-HKNCOMBI)  AND
                      (ACCT-SRYCDTOTAL =   130004570)    AND
                      (FLG-TAISYOUGAI  =   ZERO)
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF   ACCT-DAY(1 IDX1)    NOT =   ZERO
                               MOVE  1     TO   FLG-CHUJOU-SET
                               ADD   1     TO 
                                    LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                               MOVE  LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                                        TO  WRK-MEISAISU
                               ADD   ACCT-ZAITEN     TO 
                                    LNK-ORCSTEN2-CHUJOU-ZAITEN(IDX1)
                               ADD   ACCT-SRYCDTOTAL     TO 
                                    LNK-ORCSTEN2-CHUJOU-SRYCDTOTAL(IDX1)
                               MOVE   "130004570"    TO
                                    LNK-ORCSTEN2-CHUJOU-SRYCD
                                                  (IDX1 WRK-MEISAISU)
                               PERFORM   300-ZAINUM-SET-SEC
                           END-IF
                       END-PERFORM
                 END-IF
                 MOVE     "tbl_sryacct"      TO  MCP-TABLE
                 MOVE     "key14"            TO  MCP-PATHNAME
                 PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *    診療会計クローズ
           MOVE     "tbl_sryacct"    TO  MCP-TABLE
           MOVE     "key14"          TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    点滴注射（無菌製剤処理（細胞毒性を有する））加算の取り込み
           INITIALIZE                          SRYACCT-REC
           MOVE    LNK-ORCSTEN2-HOSPNUM    TO  ACCT-HOSPNUM
           MOVE    "1"                     TO  ACCT-NYUGAIKBN
           MOVE    LNK-ORCSTEN2-PTID       TO  ACCT-PTID
           MOVE    LNK-ORCSTEN2-SRYYM      TO  ACCT-SRYYM
           MOVE    "33"                    TO  ACCT-SRYKBN
           MOVE    "tbl_sryacct"           TO  MCP-TABLE
           MOVE    "key14"                 TO  MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT             =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT    =   1)
                 IF   (ACCT-ZAITEN     NOT =    ZERO)    AND
                      (ACCT-ZAIKAISU   NOT =    ZERO)    AND
                      (ACCT-MEISAISU   =        1   )    AND
                      (ACCT-JIHOKBN    =       SPACE)    AND
                      (ACCT-HKNCOMBI   =   LNK-ORCSTEN2-HKNCOMBI)  AND
                      (ACCT-SRYCDTOTAL =   130011070)    AND
                      (FLG-TAISYOUGAI  =   ZERO)
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF   ACCT-DAY(1 IDX1)    NOT =   ZERO
                               MOVE  1     TO   FLG-TENTEK-SET
                               ADD   1     TO 
                                    LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                               MOVE  LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                                        TO  WRK-MEISAISU
                               ADD   ACCT-ZAITEN     TO 
                                    LNK-ORCSTEN2-TENTEK-ZAITEN(IDX1)
                               ADD   ACCT-SRYCDTOTAL     TO 
                                    LNK-ORCSTEN2-TENTEK-SRYCDTOTAL(IDX1)
                               MOVE   "130011070"    TO
                                    LNK-ORCSTEN2-TENTEK-SRYCD
                                                  (IDX1 WRK-MEISAISU)
                               PERFORM   300-ZAINUM-SET-SEC
                           END-IF
                       END-PERFORM
                 END-IF
                 MOVE     "tbl_sryacct"      TO  MCP-TABLE
                 MOVE     "key14"            TO  MCP-PATHNAME
                 PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *    診療会計クローズ
           MOVE     "tbl_sryacct"    TO  MCP-TABLE
           MOVE     "key14"          TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    通則加算−生物学的製剤注射の取り込み
           INITIALIZE                          SRYACCT-REC
           MOVE    LNK-ORCSTEN2-HOSPNUM    TO  ACCT-HOSPNUM
           MOVE    "1"                     TO  ACCT-NYUGAIKBN
           MOVE    LNK-ORCSTEN2-PTID       TO  ACCT-PTID
           MOVE    LNK-ORCSTEN2-SRYYM      TO  ACCT-SRYYM
           MOVE    "33"                    TO  ACCT-SRYKBN
           MOVE    "tbl_sryacct"           TO  MCP-TABLE
           MOVE    "key14"                 TO  MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT             =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT    =   1)
                 IF   (ACCT-ZAITEN     NOT =    ZERO)    AND
                      (ACCT-ZAIKAISU   NOT =    ZERO)    AND
                      (ACCT-MEISAISU   =        1   )    AND
                      (ACCT-JIHOKBN    =       SPACE)    AND
                      (ACCT-HKNCOMBI   =   LNK-ORCSTEN2-HKNCOMBI)  AND
                      (ACCT-SRYCDTOTAL =   130000110)    AND
                      (FLG-TAISYOUGAI  =   ZERO)
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF   ACCT-DAY(1 IDX1)    NOT =   ZERO
      *                      点滴注射
                             IF  LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                                              NOT =   ZERO
                               MOVE  1     TO   FLG-TENTEK-SET
                               ADD   1     TO
                                    LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                               MOVE  LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                                        TO  WRK-MEISAISU
                               ADD   ACCT-ZAITEN     TO 
                                    LNK-ORCSTEN2-TENTEK-ZAITEN(IDX1)
                               ADD   ACCT-SRYCDTOTAL     TO 
                                    LNK-ORCSTEN2-TENTEK-SRYCDTOTAL(IDX1)
                               MOVE   "130000110"    TO
                                    LNK-ORCSTEN2-TENTEK-SRYCD
                                                  (IDX1 WRK-MEISAISU)
                               PERFORM   300-ZAINUM-SET-SEC
                             END-IF
      *                      中心静脈注射
                             IF  LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                                              NOT =   ZERO
                               MOVE  1     TO   FLG-CHUJOU-SET
                               ADD   1     TO 
                                    LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                               MOVE  LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                                        TO  WRK-MEISAISU
                               ADD   ACCT-ZAITEN     TO 
                                    LNK-ORCSTEN2-CHUJOU-ZAITEN(IDX1)
                               ADD   ACCT-SRYCDTOTAL     TO 
                                    LNK-ORCSTEN2-CHUJOU-SRYCDTOTAL(IDX1)
                               MOVE   "130000110"    TO
                                    LNK-ORCSTEN2-CHUJOU-SRYCD
                                                  (IDX1 WRK-MEISAISU)
                               PERFORM   300-ZAINUM-SET-SEC
                             END-IF
                           END-IF
                       END-PERFORM
                 END-IF
                 MOVE     "tbl_sryacct"      TO  MCP-TABLE
                 MOVE     "key14"            TO  MCP-PATHNAME
                 PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *    診療会計クローズ
           MOVE     "tbl_sryacct"    TO  MCP-TABLE
           MOVE     "key14"          TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    通則加算−精密持続点滴注射の取り込み
           INITIALIZE                          SRYACCT-REC
           MOVE    LNK-ORCSTEN2-HOSPNUM    TO  ACCT-HOSPNUM
           MOVE    "1"                     TO  ACCT-NYUGAIKBN
           MOVE    LNK-ORCSTEN2-PTID       TO  ACCT-PTID
           MOVE    LNK-ORCSTEN2-SRYYM      TO  ACCT-SRYYM
           MOVE    "33"                    TO  ACCT-SRYKBN
           MOVE    "tbl_sryacct"           TO  MCP-TABLE
           MOVE    "key14"                 TO  MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT             =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT    =   1)
                 IF   (ACCT-ZAITEN     NOT =    ZERO)    AND
                      (ACCT-ZAIKAISU   NOT =    ZERO)    AND
                      (ACCT-MEISAISU   =        1   )    AND
                      (ACCT-JIHOKBN    =       SPACE)    AND
                      (ACCT-HKNCOMBI   =   LNK-ORCSTEN2-HKNCOMBI)  AND
                      (ACCT-SRYCDTOTAL =   130000210)    AND
                      (FLG-TAISYOUGAI  =   ZERO)
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF   ACCT-DAY(1 IDX1)    NOT =   ZERO
      *                      点滴注射
                             MOVE    ZERO      TO   FLG-SEIMITSU-SET
                             IF  LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                                              NOT =   ZERO
                               MOVE  1     TO   FLG-SEIMITSU-SET
                               MOVE  1     TO   FLG-TENTEK-SET
                               ADD   1     TO 
                                    LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                               MOVE  LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                                        TO  WRK-MEISAISU
                               ADD   ACCT-ZAITEN     TO 
                                    LNK-ORCSTEN2-TENTEK-ZAITEN(IDX1)
                               ADD   ACCT-SRYCDTOTAL     TO 
                                    LNK-ORCSTEN2-TENTEK-SRYCDTOTAL(IDX1)
                               MOVE   "130000210"    TO
                                    LNK-ORCSTEN2-TENTEK-SRYCD
                                                  (IDX1 WRK-MEISAISU)
                               PERFORM   300-ZAINUM-SET-SEC
                             END-IF
      *                      中心静脈注射
                             IF  LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                                              NOT =   ZERO
                               MOVE  1     TO   FLG-SEIMITSU-SET
                               MOVE  1     TO   FLG-CHUJOU-SET
                               ADD   1     TO 
                                    LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                               MOVE  LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                                        TO  WRK-MEISAISU
                               ADD   ACCT-ZAITEN     TO 
                                    LNK-ORCSTEN2-CHUJOU-ZAITEN(IDX1)
                               ADD   ACCT-SRYCDTOTAL     TO 
                                    LNK-ORCSTEN2-CHUJOU-SRYCDTOTAL(IDX1)
                               MOVE   "130000210"    TO
                                    LNK-ORCSTEN2-CHUJOU-SRYCD
                                                  (IDX1 WRK-MEISAISU)
                               PERFORM   300-ZAINUM-SET-SEC
                             END-IF
                             IF      FLG-SEIMITSU-SET    =    ZERO
                                 MOVE   5     TO    LNK-ORCSTEN2-RC
                             END-IF
                           END-IF
                       END-PERFORM
                 END-IF
                 MOVE     "tbl_sryacct"      TO  MCP-TABLE
                 MOVE     "key14"            TO  MCP-PATHNAME
                 PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *    診療会計クローズ
           MOVE     "tbl_sryacct"    TO  MCP-TABLE
           MOVE     "key14"          TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    通則加算−麻薬注射の取り込み
           INITIALIZE                          SRYACCT-REC
           MOVE    LNK-ORCSTEN2-HOSPNUM    TO  ACCT-HOSPNUM
           MOVE    "1"                     TO  ACCT-NYUGAIKBN
           MOVE    LNK-ORCSTEN2-PTID       TO  ACCT-PTID
           MOVE    LNK-ORCSTEN2-SRYYM      TO  ACCT-SRYYM
           MOVE    "33"                    TO  ACCT-SRYKBN
           MOVE    "tbl_sryacct"           TO  MCP-TABLE
           MOVE    "key14"                 TO  MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT             =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT    =   1)
                 IF   (ACCT-ZAITEN     NOT =    ZERO)    AND
                      (ACCT-ZAIKAISU   NOT =    ZERO)    AND
                      (ACCT-MEISAISU   =        1   )    AND
                      (ACCT-JIHOKBN    =       SPACE)    AND
                      (ACCT-HKNCOMBI   =   LNK-ORCSTEN2-HKNCOMBI)  AND
                      (ACCT-SRYCDTOTAL =   130000310)    AND
                      (FLG-TAISYOUGAI  =   ZERO)
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF   ACCT-DAY(1 IDX1)    NOT =   ZERO
      *                      点滴注射
                             IF  LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                                              NOT =   ZERO
                               MOVE  1     TO   FLG-TENTEK-SET
                               ADD   1     TO 
                                    LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                               MOVE  LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                                        TO  WRK-MEISAISU
                               ADD   ACCT-ZAITEN     TO 
                                    LNK-ORCSTEN2-TENTEK-ZAITEN(IDX1)
                               ADD   ACCT-SRYCDTOTAL     TO 
                                    LNK-ORCSTEN2-TENTEK-SRYCDTOTAL(IDX1)
                               MOVE   "130000310"    TO
                                    LNK-ORCSTEN2-TENTEK-SRYCD
                                                  (IDX1 WRK-MEISAISU)
                               PERFORM   300-ZAINUM-SET-SEC
                             END-IF
      *                      中心静脈注射
                             IF  LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                                              NOT =   ZERO
                               MOVE  1     TO   FLG-CHUJOU-SET
                               ADD   1     TO 
                                    LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                               MOVE  LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                                        TO  WRK-MEISAISU
                               ADD   ACCT-ZAITEN     TO 
                                    LNK-ORCSTEN2-CHUJOU-ZAITEN(IDX1)
                               ADD   ACCT-SRYCDTOTAL     TO 
                                    LNK-ORCSTEN2-CHUJOU-SRYCDTOTAL(IDX1)
                               MOVE   "130000310"    TO
                                    LNK-ORCSTEN2-CHUJOU-SRYCD
                                                  (IDX1 WRK-MEISAISU)
                               PERFORM   300-ZAINUM-SET-SEC
                             END-IF
                           END-IF
                       END-PERFORM
                 END-IF
                 MOVE     "tbl_sryacct"      TO  MCP-TABLE
                 MOVE     "key14"            TO  MCP-PATHNAME
                 PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *    診療会計クローズ
           MOVE     "tbl_sryacct"    TO  MCP-TABLE
           MOVE     "key14"          TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *    
           IF      FLG-TENTEK-SET     =    1
               MOVE    ZERO        TO      LNK-ORCSTEN2-TENTEK-ERRCD
           ELSE
               MOVE    1           TO      LNK-ORCSTEN2-TENTEK-ERRCD
           END-IF
      *    
           IF      FLG-CHUJOU-SET     =    1
               MOVE    ZERO        TO      LNK-ORCSTEN2-CHUJOU-ERRCD
           ELSE
               MOVE    1           TO      LNK-ORCSTEN2-CHUJOU-ERRCD
           END-IF
      *
           IF    LNK-ORCSTEN2-RC      =    5
               INITIALIZE                  LNK-ORCSTEN2-OUT-AREA
               MOVE    5           TO      LNK-ORCSTEN2-RC
           END-IF
           .
       210-CHUSYA1-EXT.
           EXIT.
      *
      *****************************************************************
      *    注射（点滴・中心静脈）の取り込み（平成２０年４月以降）
      *****************************************************************
       220-CHUSYA2-SEC      SECTION.
      *
      *    点滴注射の取り込み
           INITIALIZE                          SRYACCT-REC
           MOVE    LNK-ORCSTEN2-HOSPNUM    TO  ACCT-HOSPNUM
           MOVE    "1"                     TO  ACCT-NYUGAIKBN
           MOVE    LNK-ORCSTEN2-PTID       TO  ACCT-PTID
           MOVE    LNK-ORCSTEN2-SRYYM      TO  ACCT-SRYYM
           MOVE    "33"                    TO  ACCT-SRYKBN
           MOVE    "tbl_sryacct"           TO  MCP-TABLE
           MOVE    "key14"                 TO  MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT             =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT    =   1)
                 IF   (ACCT-ZAITEN     NOT =    ZERO)    AND
                      (ACCT-ZAIKAISU   NOT =    ZERO)    AND
                      (ACCT-MEISAISU   =        1   )    AND
                      (ACCT-JIHOKBN    =       SPACE)    AND
                      (ACCT-HKNCOMBI   =   LNK-ORCSTEN2-HKNCOMBI)  AND
                      (ACCT-SRYCDTOTAL =   130003810)    AND
                      (FLG-TAISYOUGAI  =   ZERO)
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF   ACCT-DAY(1 IDX1)    NOT =   ZERO
                               MOVE  1     TO   FLG-TENTEK-SET
                               ADD   1     TO 
                                    LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                               MOVE  LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                                        TO  WRK-MEISAISU
                               ADD   ACCT-ZAITEN     TO 
                                    LNK-ORCSTEN2-TENTEK-ZAITEN(IDX1)
                               ADD   ACCT-SRYCDTOTAL     TO 
                                    LNK-ORCSTEN2-TENTEK-SRYCDTOTAL(IDX1)
                               MOVE   "130003810"    TO
                                    LNK-ORCSTEN2-TENTEK-SRYCD
                                                  (IDX1 WRK-MEISAISU)
                               PERFORM   300-ZAINUM-SET-SEC
                           END-IF
                       END-PERFORM
                 END-IF
                 MOVE     "tbl_sryacct"      TO  MCP-TABLE
                 MOVE     "key14"            TO  MCP-PATHNAME
                 PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *    診療会計クローズ
           MOVE     "tbl_sryacct"    TO  MCP-TABLE
           MOVE     "key14"          TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *     
      *    点滴注射（乳幼児）＋点滴注射（乳幼児）加算の取り込み
           INITIALIZE                          SRYACCT-REC
           MOVE    LNK-ORCSTEN2-HOSPNUM    TO  ACCT-HOSPNUM
           MOVE    "1"                     TO  ACCT-NYUGAIKBN
           MOVE    LNK-ORCSTEN2-PTID       TO  ACCT-PTID
           MOVE    LNK-ORCSTEN2-SRYYM      TO  ACCT-SRYYM
           MOVE    "33"                    TO  ACCT-SRYKBN
           MOVE    "tbl_sryacct"           TO  MCP-TABLE
           MOVE    "key14"                 TO  MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT             =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT    =   1)
                 IF   (ACCT-ZAITEN     NOT =    ZERO)    AND
                      (ACCT-ZAIKAISU   NOT =    ZERO)    AND
                      (ACCT-MEISAISU   =        2   )    AND
                      (ACCT-JIHOKBN    =       SPACE)    AND
                      (ACCT-HKNCOMBI   =   LNK-ORCSTEN2-HKNCOMBI)  AND
                      (ACCT-SRYCDTOTAL =   260013180)    AND
                      (FLG-TAISYOUGAI  =   ZERO)
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF   ACCT-DAY(1 IDX1)    NOT =   ZERO
                               MOVE  1     TO   FLG-TENTEK-SET
                               ADD   ACCT-ZAITEN     TO 
                                    LNK-ORCSTEN2-TENTEK-ZAITEN(IDX1)
                               ADD   ACCT-SRYCDTOTAL     TO 
                                    LNK-ORCSTEN2-TENTEK-SRYCDTOTAL(IDX1)
                               ADD   1     TO 
                                    LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                               MOVE  LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                                        TO  WRK-MEISAISU
                               MOVE   "130003710"    TO
                                    LNK-ORCSTEN2-TENTEK-SRYCD
                                                  (IDX1 WRK-MEISAISU)
                               ADD   1     TO 
                                    LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                               MOVE  LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                                        TO  WRK-MEISAISU
                               MOVE   "130009470"    TO
                                    LNK-ORCSTEN2-TENTEK-SRYCD
                                                  (IDX1 WRK-MEISAISU)
                               PERFORM   300-ZAINUM-SET-SEC
                           END-IF
                       END-PERFORM
                 END-IF
                 MOVE     "tbl_sryacct"      TO  MCP-TABLE
                 MOVE     "key14"            TO  MCP-PATHNAME
                 PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *    診療会計クローズ
           MOVE     "tbl_sryacct"    TO  MCP-TABLE
           MOVE     "key14"          TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    中心静脈注射の取り込み
           INITIALIZE                          SRYACCT-REC
           MOVE    LNK-ORCSTEN2-HOSPNUM    TO  ACCT-HOSPNUM
           MOVE    "1"                     TO  ACCT-NYUGAIKBN
           MOVE    LNK-ORCSTEN2-PTID       TO  ACCT-PTID
           MOVE    LNK-ORCSTEN2-SRYYM      TO  ACCT-SRYYM
           MOVE    "33"                    TO  ACCT-SRYKBN
           MOVE    "tbl_sryacct"           TO  MCP-TABLE
           MOVE    "key14"                 TO  MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT             =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT    =   1)
                 IF   (ACCT-ZAITEN     NOT =    ZERO)    AND
                      (ACCT-ZAIKAISU   NOT =    ZERO)    AND
                      (ACCT-MEISAISU   =        1   )    AND
                      (ACCT-JIHOKBN    =       SPACE)    AND
                      (ACCT-HKNCOMBI   =   LNK-ORCSTEN2-HKNCOMBI)  AND
                      (ACCT-SRYCDTOTAL =   130004410)    AND
                      (FLG-TAISYOUGAI  =   ZERO)
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF   ACCT-DAY(1 IDX1)    NOT =   ZERO
                               MOVE  1     TO   FLG-CHUJOU-SET
                               ADD   1     TO 
                                    LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                               MOVE  LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                                        TO  WRK-MEISAISU
                               ADD   ACCT-ZAITEN     TO 
                                    LNK-ORCSTEN2-CHUJOU-ZAITEN(IDX1)
                               ADD   ACCT-SRYCDTOTAL     TO 
                                    LNK-ORCSTEN2-CHUJOU-SRYCDTOTAL(IDX1)
                               MOVE   "130004410"    TO
                                    LNK-ORCSTEN2-CHUJOU-SRYCD
                                                  (IDX1 WRK-MEISAISU)
                               PERFORM   300-ZAINUM-SET-SEC
                           END-IF
                       END-PERFORM
                 END-IF
                 MOVE     "tbl_sryacct"      TO  MCP-TABLE
                 MOVE     "key14"            TO  MCP-PATHNAME
                 PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *    診療会計クローズ
           MOVE     "tbl_sryacct"    TO  MCP-TABLE
           MOVE     "key14"          TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    中心静脈注射＋中心静脈注射（乳幼児）加算の取り込み
           INITIALIZE                          SRYACCT-REC
           MOVE    LNK-ORCSTEN2-HOSPNUM    TO  ACCT-HOSPNUM
           MOVE    "1"                     TO  ACCT-NYUGAIKBN
           MOVE    LNK-ORCSTEN2-PTID       TO  ACCT-PTID
           MOVE    LNK-ORCSTEN2-SRYYM      TO  ACCT-SRYYM
           MOVE    "33"                    TO  ACCT-SRYKBN
           MOVE    "tbl_sryacct"           TO  MCP-TABLE
           MOVE    "key14"                 TO  MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT             =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT    =   1)
                 IF   (ACCT-ZAITEN     NOT =    ZERO)    AND
                      (ACCT-ZAIKAISU   NOT =    ZERO)    AND
                      (ACCT-MEISAISU   =        2   )    AND
                      (ACCT-JIHOKBN    =       SPACE)    AND
                      (ACCT-HKNCOMBI   =   LNK-ORCSTEN2-HKNCOMBI)  AND
                      (ACCT-SRYCDTOTAL =   260015580)    AND
                      (FLG-TAISYOUGAI  =   ZERO)
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF   ACCT-DAY(1 IDX1)    NOT =   ZERO
                               MOVE  1     TO   FLG-CHUJOU-SET
                               ADD   ACCT-ZAITEN     TO 
                                    LNK-ORCSTEN2-CHUJOU-ZAITEN(IDX1)
                               ADD   ACCT-SRYCDTOTAL     TO 
                                    LNK-ORCSTEN2-CHUJOU-SRYCDTOTAL(IDX1)
                               ADD   1     TO 
                                    LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                               MOVE  LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                                        TO  WRK-MEISAISU
                               MOVE   "130004410"    TO
                                    LNK-ORCSTEN2-CHUJOU-SRYCD
                                                  (IDX1 WRK-MEISAISU)
                               ADD   1     TO 
                                    LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                               MOVE  LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                                        TO  WRK-MEISAISU
                               MOVE   "130011170"    TO
                                    LNK-ORCSTEN2-CHUJOU-SRYCD
                                                  (IDX1 WRK-MEISAISU)
                               PERFORM   300-ZAINUM-SET-SEC
                           END-IF
                       END-PERFORM
                 END-IF
                 MOVE     "tbl_sryacct"      TO  MCP-TABLE
                 MOVE     "key14"            TO  MCP-PATHNAME
                 PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *    診療会計クローズ
           MOVE     "tbl_sryacct"    TO  MCP-TABLE
           MOVE     "key14"          TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    通則加算−生物学的製剤注射の取り込み
           INITIALIZE                          SRYACCT-REC
           MOVE    LNK-ORCSTEN2-HOSPNUM    TO  ACCT-HOSPNUM
           MOVE    "1"                     TO  ACCT-NYUGAIKBN
           MOVE    LNK-ORCSTEN2-PTID       TO  ACCT-PTID
           MOVE    LNK-ORCSTEN2-SRYYM      TO  ACCT-SRYYM
           MOVE    "33"                    TO  ACCT-SRYKBN
           MOVE    "tbl_sryacct"           TO  MCP-TABLE
           MOVE    "key14"                 TO  MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT             =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT    =   1)
                 IF   (ACCT-ZAITEN     NOT =    ZERO)    AND
                      (ACCT-ZAIKAISU   NOT =    ZERO)    AND
                      (ACCT-MEISAISU   =        1   )    AND
                      (ACCT-JIHOKBN    =       SPACE)    AND
                      (ACCT-HKNCOMBI   =   LNK-ORCSTEN2-HKNCOMBI)  AND
                      (ACCT-SRYCDTOTAL =   130000110)    AND
                      (FLG-TAISYOUGAI  =   ZERO)
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF   ACCT-DAY(1 IDX1)    NOT =   ZERO
      *                      点滴注射
                             IF  LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                                              NOT =   ZERO
                               MOVE  1     TO   FLG-TENTEK-SET
                               ADD   1     TO
                                    LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                               MOVE  LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                                        TO  WRK-MEISAISU
                               ADD   ACCT-ZAITEN     TO 
                                    LNK-ORCSTEN2-TENTEK-ZAITEN(IDX1)
                               ADD   ACCT-SRYCDTOTAL     TO 
                                    LNK-ORCSTEN2-TENTEK-SRYCDTOTAL(IDX1)
                               MOVE   "130000110"    TO
                                    LNK-ORCSTEN2-TENTEK-SRYCD
                                                  (IDX1 WRK-MEISAISU)
                               PERFORM   300-ZAINUM-SET-SEC
                             END-IF
      *                      中心静脈注射
                             IF  LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                                              NOT =   ZERO
                               MOVE  1     TO   FLG-CHUJOU-SET
                               ADD   1     TO 
                                    LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                               MOVE  LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                                        TO  WRK-MEISAISU
                               ADD   ACCT-ZAITEN     TO 
                                    LNK-ORCSTEN2-CHUJOU-ZAITEN(IDX1)
                               ADD   ACCT-SRYCDTOTAL     TO 
                                    LNK-ORCSTEN2-CHUJOU-SRYCDTOTAL(IDX1)
                               MOVE   "130000110"    TO
                                    LNK-ORCSTEN2-CHUJOU-SRYCD
                                                  (IDX1 WRK-MEISAISU)
                               PERFORM   300-ZAINUM-SET-SEC
                             END-IF
                           END-IF
                       END-PERFORM
                 END-IF
                 MOVE     "tbl_sryacct"      TO  MCP-TABLE
                 MOVE     "key14"            TO  MCP-PATHNAME
                 PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *    診療会計クローズ
           MOVE     "tbl_sryacct"    TO  MCP-TABLE
           MOVE     "key14"          TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    通則加算−精密持続点滴注射の取り込み
           INITIALIZE                          SRYACCT-REC
           MOVE    LNK-ORCSTEN2-HOSPNUM    TO  ACCT-HOSPNUM
           MOVE    "1"                     TO  ACCT-NYUGAIKBN
           MOVE    LNK-ORCSTEN2-PTID       TO  ACCT-PTID
           MOVE    LNK-ORCSTEN2-SRYYM      TO  ACCT-SRYYM
           MOVE    "33"                    TO  ACCT-SRYKBN
           MOVE    "tbl_sryacct"           TO  MCP-TABLE
           MOVE    "key14"                 TO  MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT             =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT    =   1)
                 IF   (ACCT-ZAITEN     NOT =    ZERO)    AND
                      (ACCT-ZAIKAISU   NOT =    ZERO)    AND
                      (ACCT-MEISAISU   =        1   )    AND
                      (ACCT-JIHOKBN    =       SPACE)    AND
                      (ACCT-HKNCOMBI   =   LNK-ORCSTEN2-HKNCOMBI)  AND
                      (ACCT-SRYCDTOTAL =   130000210)    AND
                      (FLG-TAISYOUGAI  =   ZERO)
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF   ACCT-DAY(1 IDX1)    NOT =   ZERO
      *                      点滴注射
                             MOVE    ZERO      TO   FLG-SEIMITSU-SET
                             IF  LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                                              NOT =   ZERO
                               MOVE  1     TO   FLG-SEIMITSU-SET
                               MOVE  1     TO   FLG-TENTEK-SET
                               ADD   1     TO 
                                    LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                               MOVE  LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                                        TO  WRK-MEISAISU
                               ADD   ACCT-ZAITEN     TO 
                                    LNK-ORCSTEN2-TENTEK-ZAITEN(IDX1)
                               ADD   ACCT-SRYCDTOTAL     TO 
                                    LNK-ORCSTEN2-TENTEK-SRYCDTOTAL(IDX1)
                               MOVE   "130000210"    TO
                                    LNK-ORCSTEN2-TENTEK-SRYCD
                                                  (IDX1 WRK-MEISAISU)
                               PERFORM   300-ZAINUM-SET-SEC
                             END-IF
      *                      中心静脈注射
                             IF  LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                                              NOT =   ZERO
                               MOVE  1     TO   FLG-SEIMITSU-SET
                               MOVE  1     TO   FLG-CHUJOU-SET
                               ADD   1     TO 
                                    LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                               MOVE  LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                                        TO  WRK-MEISAISU
                               ADD   ACCT-ZAITEN     TO 
                                    LNK-ORCSTEN2-CHUJOU-ZAITEN(IDX1)
                               ADD   ACCT-SRYCDTOTAL     TO 
                                    LNK-ORCSTEN2-CHUJOU-SRYCDTOTAL(IDX1)
                               MOVE   "130000210"    TO
                                    LNK-ORCSTEN2-CHUJOU-SRYCD
                                                  (IDX1 WRK-MEISAISU)
                               PERFORM   300-ZAINUM-SET-SEC
                             END-IF
                             IF      FLG-SEIMITSU-SET    =    ZERO
                                 MOVE   5     TO    LNK-ORCSTEN2-RC
                             END-IF
                           END-IF
                       END-PERFORM
                 END-IF
                 MOVE     "tbl_sryacct"      TO  MCP-TABLE
                 MOVE     "key14"            TO  MCP-PATHNAME
                 PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *    診療会計クローズ
           MOVE     "tbl_sryacct"    TO  MCP-TABLE
           MOVE     "key14"          TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    通則加算−麻薬注射の取り込み
           INITIALIZE                          SRYACCT-REC
           MOVE    LNK-ORCSTEN2-HOSPNUM    TO  ACCT-HOSPNUM
           MOVE    "1"                     TO  ACCT-NYUGAIKBN
           MOVE    LNK-ORCSTEN2-PTID       TO  ACCT-PTID
           MOVE    LNK-ORCSTEN2-SRYYM      TO  ACCT-SRYYM
           MOVE    "33"                    TO  ACCT-SRYKBN
           MOVE    "tbl_sryacct"           TO  MCP-TABLE
           MOVE    "key14"                 TO  MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT             =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT    =   1)
                 IF   (ACCT-ZAITEN     NOT =    ZERO)    AND
                      (ACCT-ZAIKAISU   NOT =    ZERO)    AND
                      (ACCT-MEISAISU   =        1   )    AND
                      (ACCT-JIHOKBN    =       SPACE)    AND
                      (ACCT-HKNCOMBI   =   LNK-ORCSTEN2-HKNCOMBI)  AND
                      (ACCT-SRYCDTOTAL =   130000310)    AND
                      (FLG-TAISYOUGAI  =   ZERO)
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF   ACCT-DAY(1 IDX1)    NOT =   ZERO
      *                      点滴注射
                             IF  LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                                              NOT =   ZERO
                               MOVE  1     TO   FLG-TENTEK-SET
                               ADD   1     TO 
                                    LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                               MOVE  LNK-ORCSTEN2-TENTEK-MEISAISU(IDX1)
                                        TO  WRK-MEISAISU
                               ADD   ACCT-ZAITEN     TO 
                                    LNK-ORCSTEN2-TENTEK-ZAITEN(IDX1)
                               ADD   ACCT-SRYCDTOTAL     TO 
                                    LNK-ORCSTEN2-TENTEK-SRYCDTOTAL(IDX1)
                               MOVE   "130000310"    TO
                                    LNK-ORCSTEN2-TENTEK-SRYCD
                                                  (IDX1 WRK-MEISAISU)
                               PERFORM   300-ZAINUM-SET-SEC
                             END-IF
      *                      中心静脈注射
                             IF  LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                                              NOT =   ZERO
                               MOVE  1     TO   FLG-CHUJOU-SET
                               ADD   1     TO 
                                    LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                               MOVE  LNK-ORCSTEN2-CHUJOU-MEISAISU(IDX1)
                                        TO  WRK-MEISAISU
                               ADD   ACCT-ZAITEN     TO 
                                    LNK-ORCSTEN2-CHUJOU-ZAITEN(IDX1)
                               ADD   ACCT-SRYCDTOTAL     TO 
                                    LNK-ORCSTEN2-CHUJOU-SRYCDTOTAL(IDX1)
                               MOVE   "130000310"    TO
                                    LNK-ORCSTEN2-CHUJOU-SRYCD
                                                  (IDX1 WRK-MEISAISU)
                               PERFORM   300-ZAINUM-SET-SEC
                             END-IF
                           END-IF
                       END-PERFORM
                 END-IF
                 MOVE     "tbl_sryacct"      TO  MCP-TABLE
                 MOVE     "key14"            TO  MCP-PATHNAME
                 PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *    診療会計クローズ
           MOVE     "tbl_sryacct"    TO  MCP-TABLE
           MOVE     "key14"          TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *    
           IF      FLG-TENTEK-SET     =    1
               MOVE    ZERO        TO      LNK-ORCSTEN2-TENTEK-ERRCD
           ELSE
               MOVE    1           TO      LNK-ORCSTEN2-TENTEK-ERRCD
           END-IF
      *    
           IF      FLG-CHUJOU-SET     =    1
               MOVE    ZERO        TO      LNK-ORCSTEN2-CHUJOU-ERRCD
           ELSE
               MOVE    1           TO      LNK-ORCSTEN2-CHUJOU-ERRCD
           END-IF
      *
           IF    LNK-ORCSTEN2-RC      =    5
               INITIALIZE                  LNK-ORCSTEN2-OUT-AREA
               MOVE    5           TO      LNK-ORCSTEN2-RC
           END-IF
           .
       220-CHUSYA2-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象剤番号のセット
      *****************************************************************
       300-ZAINUM-SET-SEC         SECTION.
      *
           MOVE       ZERO        TO   FLG-ZAINUM-MATCH
           PERFORM    VARYING   IDX2    FROM   1   BY  1
             UNTIL    (IDX2     >    20)
                 IF   ACCT-ZAINUM    =   LNK-ORCSTEN2-ZAINUM(IDX2)
                     MOVE    1    TO   FLG-ZAINUM-MATCH
                 END-IF
           END-PERFORM
      *
           IF    FLG-ZAINUM-MATCH   =  ZERO
               ADD    1      TO    IDX3
               MOVE   ACCT-ZAINUM  TO  LNK-ORCSTEN2-ZAINUM(IDX3)
           END-IF
      *
           .
       300-ZAINUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
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
           PERFORM   900-CLOSE-SEC
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
           PERFORM   900-DBSELECT-SEC
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
           PERFORM   900-DBFETCH-SEC
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
      *    診療会計マスター検索
      *****************************************************************
       910-SRYACCT-SELECT-SEC        SECTION.
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-SRYACCT
           ELSE
               MOVE    1                  TO  FLG-SRYACCT
           END-IF
      *
           .
       910-SRYACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       910-SRYACCT-READ-SEC        SECTION.
      *
           MOVE      ZERO        TO    FLG-TAISYOUGAI
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  SRYACCT-REC
               MOVE    ZERO               TO  FLG-SRYACCT
               PERFORM    VARYING   IDX   FROM   1   BY   1
                 UNTIL    IDX   >   31
                      IF   ACCT-DAY(1 IDX)    >=    2
                          MOVE   1        TO     FLG-TAISYOUGAI
                      END-IF
               END-PERFORM
           ELSE
               INITIALIZE                     SRYACCT-REC
               MOVE    1                  TO  FLG-SRYACCT
           END-IF
      *
           .
       910-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター検索
      *****************************************************************
       910-SRYACT-SELECT-SEC        SECTION.
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       910-SRYACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       910-SRYACT-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       910-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター検索
      *****************************************************************
       910-HKNCOMBI-SELECT-SEC        SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-HKNCOMBI
           ELSE
               MOVE    1                  TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               MOVE    ZERO                TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                      HKNCOMBI-REC
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *      
