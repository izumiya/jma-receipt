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
       PROGRAM-ID.                 ORCSNYUINDAY.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : 入院料未算定日取得
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/12/13    NACL−小豆沢  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *平静２０年４月改定対応
      *  04.02.00    NACL-小豆沢  08/03/05  短期滞在手術基本料３の入院追加
      *  04.02.00    NACL-小豆沢  08/06/02  救急搬送診療料の算定日を判定追加
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
           03  FLG-SRYACCT                 PIC 9(01).
           03  FLG-SRYACT                  PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-TANKI2                  PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
      *    添字領域
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
           03  IDX1                        PIC 9(04).
           03  IDX-ST                      PIC 9(02).
           03  IDX-ED                      PIC 9(02).
      *
      *    コンスタント値
       01  CONSTANT-AREA.
      *    短期滞在手術基本料２
           03   CONS-TANKI2                PIC X(09) VALUE "190076810".
      *    短期滞在手術基本料２（生活療養）
           03   CONS-TANKI2-SEIKATSU       PIC X(09) VALUE "190125310".
      *    短期滞在手術基本料３
           03   CONS-TANKI3                PIC X(09) VALUE "190130410".
      *    短期滞在手術基本料３（生活療養）
           03   CONS-TANKI3-SEIKATSU       PIC X(09) VALUE "190130510".
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-GAIHAKU-SRYCD           PIC X(09).
      *
      *    同日再入院日の翌日取得用
           03  WRK-DOUJITSU-YMD            PIC X(08).
           03  WRK-DOUJITSU-YMDR    REDEFINES   WRK-DOUJITSU-YMD.
               05  WRK-DOUJITSU-YYMM       PIC 9(06).
               05  WRK-DOUJITSU-DD         PIC 9(02).
      *
      *    同日再入院日の翌日取得用
           03  WRK-YOKUJITSU-YMD          PIC 9(08).
           03  WRK-YOKUJITSU-YMDR   REDEFINES   WRK-YOKUJITSU-YMD.
               05  WRK-YOKUJITSU-YYMM      PIC 9(06).
               05  WRK-YOKUJITSU-DD        PIC 9(02).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    入院診療会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院診療行為マスター
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為マスター
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    患者入院履歴マスタ−
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
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
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "CPORCSNYUINDAY.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSNYUINDAY-AREA
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
           INITIALIZE                  LNK-NYUINDAY-HENKYAKU-AREA
           MOVE    ZERO            TO  LNK-NYUINDAY-RC
      *
      *    パラメータチェック
           PERFORM 100-PRM-CHK-SEC
           IF    ( LNK-NYUINDAY-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    主処理
           IF      LNK-NYUINDAY-RC      =    ZERO
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
           IF     (LNK-NYUINDAY-SANTEIYM  =   SPACE)      OR
                  (LNK-NYUINDAY-SANTEIYM  <=   "200603")
               MOVE    1                TO  LNK-NYUINDAY-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    患者マスター検索
           INITIALIZE                       PTINF-REC
           MOVE    LNK-NYUINDAY-HOSPNUM   TO  PTINF-HOSPNUM
           MOVE    LNK-NYUINDAY-PTID      TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF              NOT =   ZERO
               MOVE    2                  TO  LNK-NYUINDAY-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    入院中のチェック（入院会計マスター検索）
           INITIALIZE                       NYUINACCT-REC
           MOVE    LNK-NYUINDAY-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-NYUINDAY-PTID      TO  NACCT-PTID
           MOVE    LNK-NYUINDAY-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "5"                    TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"        TO  MCP-TABLE
           MOVE    "key17"                TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT          NOT =   ZERO
               MOVE    3                  TO  LNK-NYUINDAY-RC
           ELSE
               MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE    "key17"            TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT         NOT =   ZERO
                    MOVE    3             TO  LNK-NYUINDAY-RC
               END-IF
           END-IF
      *
      *    入院会計クローズ
           MOVE    "tbl_nyuinacct"        TO  MCP-TABLE
           MOVE    "key17"                TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主取得
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
      *    入院履歴より短期滞在手術基本料２の算定有無を特定
           INITIALIZE                     PTNYUINRRK-REC
           MOVE    LNK-NYUINDAY-HOSPNUM   TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-NYUINDAY-PTID      TO  PTNYUINRRK-PTID
           MOVE    "3"                    TO  PTNYUINRRK-TOKUTEI-KBN
           MOVE    "21"                   TO  PTNYUINRRK-TOKUTEI-NYUIN
           MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
           MOVE    "key49"                TO  MCP-PATHNAME
           PERFORM 910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK         =   ZERO
               MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
               MOVE    "key49"            TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
               PERFORM   UNTIL   (FLG-PTNYUINRRK   =   1)
                 IF (PTNYUINRRK-TENNYUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                   ((PTNYUINRRK-TENSTUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                    (PTNYUINRRK-TENSTUYMD(1:6) = "999999"))
                      MOVE     ZERO       TO     IDX-ST
                                                 IDX-ED
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          <  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                            =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  1                          TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          >  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF      PTNYUINRRK-TENSTUYMD(1:6) = "999999"
                           MOVE   99     TO    IDX-ED
                      END-IF
                      PERFORM    VARYING   IDX   FROM   1   BY   1
                               UNTIL     IDX   >   31
                         IF    (IDX-ST    <=    IDX)   AND
                               (IDX-ED    >=    IDX)
                            MOVE  "1"  TO  LNK-NYUINDAY-TANKI2-KBN(IDX)
                         END-IF
                      END-PERFORM
                 END-IF
                 MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
                 MOVE    "key49"              TO  MCP-PATHNAME
                 PERFORM 910-PTNYUINRRK-READ-SEC
               END-PERFORM
           END-IF
      *
      *    患者入院履歴クローズ
           MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
           MOVE    "key49"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    入院履歴より短期滞在手術基本料２（生活療養）の算定有無を特定
           INITIALIZE                     PTNYUINRRK-REC
           MOVE    LNK-NYUINDAY-HOSPNUM   TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-NYUINDAY-PTID      TO  PTNYUINRRK-PTID
           MOVE    "3"                    TO  PTNYUINRRK-TOKUTEI-KBN
           MOVE    "50"                   TO  PTNYUINRRK-TOKUTEI-NYUIN
           MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
           MOVE    "key49"                TO  MCP-PATHNAME
           PERFORM 910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK         =   ZERO
               MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
               MOVE    "key49"            TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
               PERFORM   UNTIL   (FLG-PTNYUINRRK   =   1)
                 IF (PTNYUINRRK-TENNYUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                   ((PTNYUINRRK-TENSTUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                    (PTNYUINRRK-TENSTUYMD(1:6) = "999999"))
                      MOVE     ZERO       TO     IDX-ST
                                                 IDX-ED
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          <  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                            =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  1                          TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          >  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF      PTNYUINRRK-TENSTUYMD(1:6) = "999999"
                           MOVE   99     TO    IDX-ED
                      END-IF
                      PERFORM    VARYING   IDX   FROM   1   BY   1
                               UNTIL     IDX   >   31
                         IF    (IDX-ST    <=    IDX)   AND
                               (IDX-ED    >=    IDX)
                            MOVE  "2"  TO  LNK-NYUINDAY-TANKI2-KBN(IDX)
                         END-IF
                      END-PERFORM
                 END-IF
                 MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
                 MOVE    "key49"              TO  MCP-PATHNAME
                 PERFORM 910-PTNYUINRRK-READ-SEC
               END-PERFORM
           END-IF
      *
      *    患者入院履歴クローズ
           MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
           MOVE    "key49"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    入院履歴より短期滞在手術基本料３の算定有無を特定
           INITIALIZE                     PTNYUINRRK-REC
           MOVE    LNK-NYUINDAY-HOSPNUM   TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-NYUINDAY-PTID      TO  PTNYUINRRK-PTID
           MOVE    "3"                    TO  PTNYUINRRK-TOKUTEI-KBN
           MOVE    "52"                   TO  PTNYUINRRK-TOKUTEI-NYUIN
           MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
           MOVE    "key49"                TO  MCP-PATHNAME
           PERFORM 910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK         =   ZERO
               MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
               MOVE    "key49"            TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
               PERFORM   UNTIL   (FLG-PTNYUINRRK   =   1)
                 IF (PTNYUINRRK-TENNYUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                   ((PTNYUINRRK-TENSTUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                    (PTNYUINRRK-TENSTUYMD(1:6) = "999999"))
                      MOVE     ZERO       TO     IDX-ST
                                                 IDX-ED
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          <  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                            =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  1                          TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          >  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF      PTNYUINRRK-TENSTUYMD(1:6) = "999999"
                           MOVE   99     TO    IDX-ED
                      END-IF
                      PERFORM    VARYING   IDX   FROM   1   BY   1
                               UNTIL     IDX   >   31
                         IF    (IDX-ST    <=    IDX)   AND
                               (IDX-ED    >=    IDX)
                            MOVE  "1"  TO  LNK-NYUINDAY-TANKI2-KBN(IDX)
                         END-IF
                      END-PERFORM
                 END-IF
                 MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
                 MOVE    "key49"              TO  MCP-PATHNAME
                 PERFORM 910-PTNYUINRRK-READ-SEC
               END-PERFORM
           END-IF
      *
      *    患者入院履歴クローズ
           MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
           MOVE    "key49"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    入院履歴より短期滞在手術基本料３（生活療養）の算定有無を特定
           INITIALIZE                     PTNYUINRRK-REC
           MOVE    LNK-NYUINDAY-HOSPNUM   TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-NYUINDAY-PTID      TO  PTNYUINRRK-PTID
           MOVE    "3"                    TO  PTNYUINRRK-TOKUTEI-KBN
           MOVE    "53"                   TO  PTNYUINRRK-TOKUTEI-NYUIN
           MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
           MOVE    "key49"                TO  MCP-PATHNAME
           PERFORM 910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK         =   ZERO
               MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
               MOVE    "key49"            TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
               PERFORM   UNTIL   (FLG-PTNYUINRRK   =   1)
                 IF (PTNYUINRRK-TENNYUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                   ((PTNYUINRRK-TENSTUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                    (PTNYUINRRK-TENSTUYMD(1:6) = "999999"))
                      MOVE     ZERO       TO     IDX-ST
                                                 IDX-ED
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          <  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                            =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  1                          TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          >  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF      PTNYUINRRK-TENSTUYMD(1:6) = "999999"
                           MOVE   99     TO    IDX-ED
                      END-IF
                      PERFORM    VARYING   IDX   FROM   1   BY   1
                               UNTIL     IDX   >   31
                         IF    (IDX-ST    <=    IDX)   AND
                               (IDX-ED    >=    IDX)
                            MOVE  "2"  TO  LNK-NYUINDAY-TANKI2-KBN(IDX)
                         END-IF
                      END-PERFORM
                 END-IF
                 MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
                 MOVE    "key49"              TO  MCP-PATHNAME
                 PERFORM 910-PTNYUINRRK-READ-SEC
               END-PERFORM
           END-IF
      *
      *    患者入院履歴クローズ
           MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
           MOVE    "key49"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    短期滞在手術基本料２（生活療養含む）又は短期滞在手術基本料３（生活療養含む）
      *    に該当する入院料を判定し、入院会計で算定済みの場合は返却値をクリアする
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-NYUINDAY-HOSPNUM    TO  NACCT-HOSPNUM
           MOVE    LNK-NYUINDAY-PTID       TO  NACCT-PTID
           MOVE    LNK-NYUINDAY-SANTEIYM   TO  NACCT-SRYYM
           MOVE    "1"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   IF    NACCT-SRYCDTOTAL     =    99999916
                       CONTINUE
                   ELSE
                       PERFORM    210-NYUINACT-GET-SEC
                       IF   NSRY-SRYCD(1)   =  CONS-TANKI2     OR
                                               CONS-TANKI2-SEIKATSU OR
                                               CONS-TANKI3     OR
                                               CONS-TANKI3-SEIKATSU
                          PERFORM    VARYING   IDX   FROM   1   BY   1
                                  UNTIL     IDX   >   31
                            IF     NACCT-DAY(IDX)  NOT =  ZERO
                               MOVE  SPACE TO LNK-NYUINDAY-TANKI2-KBN
                                                                 (IDX)
                            END-IF
                          END-PERFORM
                       END-IF
                   END-IF
                   MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
                   MOVE    "key17"              TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *
      *    入院会計クローズ
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    入院区分のセット
           PERFORM    VARYING   IDX   FROM   1   BY   1
                 UNTIL     IDX   >   31
               IF   LNK-NYUINDAY-TANKI2-KBN(IDX)  NOT =   SPACE
                   MOVE   "1"     TO   LNK-NYUINDAY-KBN(IDX)
               END-IF
           END-PERFORM
      *
      *    入院中の救急搬送診療料（入院の診療行為）算定日を
      *    食事算定可能とする
           INITIALIZE                          SRYACT-REC
           MOVE    LNK-NYUINDAY-HOSPNUM    TO  SRY-HOSPNUM
           MOVE    "1"                     TO  SRY-NYUGAIKBN
           MOVE    LNK-NYUINDAY-PTID       TO  SRY-PTID
           MOVE    LNK-NYUINDAY-SANTEIYM   TO  SRY-SRYYM
           MOVE    "114003010"             TO  SRY-SRYCD(1)
                                               SRY-SRYCD(2)
                                               SRY-SRYCD(3)
                                               SRY-SRYCD(4)
                                               SRY-SRYCD(5)
           MOVE    "tbl_sryact"            TO  MCP-TABLE
           MOVE    "key7"                  TO  MCP-PATHNAME
           PERFORM 910-SRYACT-SELECT-SEC
           IF      FLG-SRYACT             =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM   910-SRYACT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACT  =   1)
                   INITIALIZE                  SRYACCT-REC
                   MOVE    SRY-HOSPNUM     TO  ACCT-HOSPNUM
                   MOVE    SRY-NYUGAIKBN   TO  ACCT-NYUGAIKBN
                   MOVE    SRY-PTID        TO  ACCT-PTID
                   MOVE    SRY-SRYKA       TO  ACCT-SRYKA
                   MOVE    SRY-SRYYM       TO  ACCT-SRYYM
                   MOVE    SRY-ZAINUM      TO  ACCT-ZAINUM
                   MOVE    "tbl_sryacct"   TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
                   PERFORM   910-SRYACCT-SELECT-SEC
                   MOVE    "tbl_sryacct"   TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
                   PERFORM   910-SRYACCT-READ-SEC
                   IF    FLG-SRYACCT       =   0
                       PERFORM    VARYING   IDX   FROM   1   BY   1
                         UNTIL     IDX   >   31
                           IF   ACCT-DAY(1 IDX)   NOT =   ZERO
                             MOVE   "1"    TO   LNK-NYUINDAY-KBN(IDX)
                                                LNK-NYUINDAY-KYUKYU-KBN
                                                                  (IDX)
                           END-IF
                       END-PERFORM
                   END-IF
      *
      *            診療会計クローズ
                   MOVE    "tbl_sryacct"   TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
                   PERFORM   900-CLOSE-SEC
      *
                   MOVE    "tbl_sryact"    TO  MCP-TABLE
                   MOVE    "key7"          TO  MCP-PATHNAME
                   PERFORM 910-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
      *    診療行為クローズ
           MOVE    "tbl_sryact"            TO  MCP-TABLE
           MOVE    "key7"                  TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    入院中の救急搬送診療料（外来の診療行為）算定日を
      *    食事算定可能とする
           INITIALIZE                          SRYACT-REC
           MOVE    LNK-NYUINDAY-HOSPNUM    TO  SRY-HOSPNUM
           MOVE    "2"                     TO  SRY-NYUGAIKBN
           MOVE    LNK-NYUINDAY-PTID       TO  SRY-PTID
           MOVE    LNK-NYUINDAY-SANTEIYM   TO  SRY-SRYYM
           MOVE    "114003010"             TO  SRY-SRYCD(1)
                                               SRY-SRYCD(2)
                                               SRY-SRYCD(3)
                                               SRY-SRYCD(4)
                                               SRY-SRYCD(5)
           MOVE    "tbl_sryact"            TO  MCP-TABLE
           MOVE    "key7"                  TO  MCP-PATHNAME
           PERFORM 910-SRYACT-SELECT-SEC
           IF      FLG-SRYACT             =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM   910-SRYACT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACT  =   1)
                   INITIALIZE                  SRYACCT-REC
                   MOVE    SRY-HOSPNUM     TO  ACCT-HOSPNUM
                   MOVE    SRY-NYUGAIKBN   TO  ACCT-NYUGAIKBN
                   MOVE    SRY-PTID        TO  ACCT-PTID
                   MOVE    SRY-SRYKA       TO  ACCT-SRYKA
                   MOVE    SRY-SRYYM       TO  ACCT-SRYYM
                   MOVE    SRY-ZAINUM      TO  ACCT-ZAINUM
                   MOVE    "tbl_sryacct"   TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
                   PERFORM   910-SRYACCT-SELECT-SEC
                   MOVE    "tbl_sryacct"   TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
                   PERFORM   910-SRYACCT-READ-SEC
                   IF    FLG-SRYACCT       =   0
                       PERFORM    VARYING   IDX   FROM   1   BY   1
                         UNTIL     IDX   >   31
                           IF   ACCT-DAY(1 IDX)   NOT =   ZERO
                             MOVE   "1"    TO   LNK-NYUINDAY-KBN(IDX)
                                                LNK-NYUINDAY-KYUKYU-KBN
                                                                  (IDX)
                           END-IF
                       END-PERFORM
                   END-IF
      *
      *            診療会計クローズ
                   MOVE    "tbl_sryacct"   TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
                   PERFORM   900-CLOSE-SEC
      *
                   MOVE    "tbl_sryact"    TO  MCP-TABLE
                   MOVE    "key7"          TO  MCP-PATHNAME
                   PERFORM 910-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
      *    診療行為クローズ
           MOVE    "tbl_sryact"            TO  MCP-TABLE
           MOVE    "key7"                  TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    入院履歴より同日再入院を特定
           INITIALIZE                     PTNYUINRRK-REC
           MOVE    LNK-NYUINDAY-HOSPNUM   TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-NYUINDAY-PTID      TO  PTNYUINRRK-PTID
           MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
           MOVE    "key33"                TO  MCP-PATHNAME
           PERFORM 910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK         =   ZERO
               MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
               MOVE    "key33"            TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
               PERFORM   UNTIL   (FLG-PTNYUINRRK   =   1)
                 IF (PTNYUINRRK-TENNYUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                      MOVE     ZERO       TO     IDX-ST
                      IF  PTNYUINRRK-DOUJITSUKBN   =   "1"
      *
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
      *
                          MOVE  "1"       TO  LNK-NYUINDAY-DOUJITSU-KBN
                                                               (IDX-ST)
      *
      *                   同日再入院日（入院分）の食事開始時間区分
                          MOVE  PTNYUINRRK-TIMEKBN-IN
                                     TO  LNK-NYUINDAY-DOUJITSU-SKJKBN
                                                               (IDX-ST)
      *
      *                   同日再入院日（退院分）の食事終了時間区分
                          EVALUATE   PTNYUINRRK-TIMEKBN-IN
                              WHEN   "0"
                                  MOVE  "3"   TO
                                         LNK-NYUINDAY-DOUJITSU-T-SKJKBN
                                                               (IDX-ST)
                              WHEN   "1"
                                  MOVE  "0"   TO
                                         LNK-NYUINDAY-DOUJITSU-T-SKJKBN
                                                               (IDX-ST)
                              WHEN   "2"
                                  MOVE  "1"   TO
                                         LNK-NYUINDAY-DOUJITSU-T-SKJKBN
                                                               (IDX-ST)
                              WHEN   "3"
                                  MOVE  "2"   TO
                                         LNK-NYUINDAY-DOUJITSU-T-SKJKBN
                                                               (IDX-ST)
                          END-EVALUATE
      *
      *
      *                   同日再入院日の保険組合せを入院会計から
      *                   再取得する
                          MOVE    PTNYUINRRK-TENNYUYMD
                                           TO  WRK-DOUJITSU-YMD
                          INITIALIZE           NYUINACCT-REC
                           MOVE    LNK-NYUINDAY-HOSPNUM
                                           TO  NACCT-HOSPNUM
                           MOVE    LNK-NYUINDAY-PTID
                                           TO  NACCT-PTID
                           MOVE    WRK-DOUJITSU-YYMM
                                           TO  NACCT-SRYYM
                           MOVE    "5"     TO  NACCT-ZAISKBKBN
                           MOVE    "tbl_nyuinacct"  TO  MCP-TABLE
                           MOVE    "key17"          TO  MCP-PATHNAME
                           PERFORM 910-NYUINACCT-SELECT-SEC
                           IF      FLG-NYUINACCT   =   ZERO
                               MOVE    "tbl_nyuinacct"
                                                    TO  MCP-TABLE
                               MOVE    "key17"      TO  MCP-PATHNAME
                               PERFORM 910-NYUINACCT-READ-SEC
                               IF   FLG-NYUINACCT    =   ZERO
                                     MOVE   NACCT-DAY (WRK-DOUJITSU-DD)
                                                 TO  
                                        LNK-NYUINDAY-DOUJITSU-T-HKNCOMBI
                                                           (IDX-ST)
                               END-IF
                           END-IF
      *
      *                    入院会計クローズ
                           MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
                           MOVE    "key17"            TO  MCP-PATHNAME
                           PERFORM   900-CLOSE-SEC
      *
      *                    同日退院分の保険組合せ保険種類を取得する
      *                    （１：社保　２：国保　３：後期高齢　４：労災
      *                      ５：自賠　６：公害　７：自費　８：治験）
                           IF  LNK-NYUINDAY-DOUJITSU-T-HKNCOMBI (IDX-ST)
                                                      NOT = ZERO
                               PERFORM   210-HKNSYURUI-GET1-SEC
                           END-IF
      *
      *                   同日再入院日（入院分）の保険組合せ番号
                          MOVE  PTNYUINRRK-HKNCOMBINUM
                                     TO  LNK-NYUINDAY-DOUJITSU-HKNCOMBI
                                                               (IDX-ST)
      *
      *                   同日再入院日翌日の保険組合せを入院会計から
      *                   再取得する
                          MOVE    ZERO    TO  WRK-YOKUJITSU-YMD
                          INITIALIZE          LNK-DAY6-AREA
                          MOVE    "61"    TO  LNK-DAY6-IRAI
                          MOVE    PTNYUINRRK-TENNYUYMD
                                          TO  LNK-DAY6-KIJUN
                          MOVE    "1"     TO  LNK-DAY6-ZOGENPTN
                          MOVE    1       TO  LNK-DAY6-ZOGEN
                          CALL    "ORCSDAY"   USING     STS-AREA-DAY
                                                        LNK-DAY6-AREA
                          MOVE    LNK-DAY6-KEISAN
                                          TO  WRK-YOKUJITSU-YMD
      *                   入院会計（保険組合せの剤を検索）
                          INITIALIZE           NYUINACCT-REC
                           MOVE    LNK-NYUINDAY-HOSPNUM
                                           TO  NACCT-HOSPNUM
                           MOVE    LNK-NYUINDAY-PTID
                                           TO  NACCT-PTID
                           MOVE    WRK-YOKUJITSU-YYMM
                                           TO  NACCT-SRYYM
                           MOVE    "5"     TO  NACCT-ZAISKBKBN
                           MOVE    "tbl_nyuinacct"  TO  MCP-TABLE
                           MOVE    "key17"          TO  MCP-PATHNAME
                           PERFORM 910-NYUINACCT-SELECT-SEC
                           IF      FLG-NYUINACCT   =   ZERO
                               MOVE    "tbl_nyuinacct"
                                                    TO  MCP-TABLE
                               MOVE    "key17"      TO  MCP-PATHNAME
                               PERFORM 910-NYUINACCT-READ-SEC
                               IF   FLG-NYUINACCT    =   ZERO
                                     MOVE   NACCT-DAY (WRK-YOKUJITSU-DD)
                                                 TO  
                                         LNK-NYUINDAY-DOUJITSU-HKNCOMBI
                                                           (IDX-ST)
                               END-IF
                           END-IF
      *
      *                    入院会計クローズ
                           MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
                           MOVE    "key17"            TO  MCP-PATHNAME
                           PERFORM   900-CLOSE-SEC
      *
      *                    同日再入院分の保険組合せ保険種類を取得する
      *                    （１：社保　２：国保　３：後期高齢　４：労災
      *                      ５：自賠　６：公害　７：自費　８：治験）
                           IF  LNK-NYUINDAY-DOUJITSU-HKNCOMBI (IDX-ST)
                                                      NOT = ZERO
                               PERFORM   210-HKNSYURUI-GET2-SEC
                           END-IF
      *
      *                    保険変動区分をセット
                           IF  (LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX-ST)
                                                              =  "1" OR
                                                                 "2" OR
                                                                 "3")
                                               AND
                               (LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX-ST)
                                                               = "1" OR
                                                                 "2" OR
                                                                 "3")
                              MOVE  "1"   TO
                                 LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX-ST)
                           END-IF
                           IF  (LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX-ST)
                                                              =  "1" OR
                                                                 "2" OR
                                                                 "3")
                                               AND
                               (LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX-ST)
                                                               = "4" OR
                                                                 "5" OR
                                                                 "6" OR
                                                                 "7" OR
                                                                 "8")
                              MOVE  "2"   TO
                                 LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX-ST)
                           END-IF
                           IF  (LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX-ST)
                                                              =  "4" OR
                                                                 "5" OR
                                                                 "6" OR
                                                                 "7" OR
                                                                 "8")
                                               AND
                               (LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX-ST)
                                                               = "1" OR
                                                                 "2" OR
                                                                 "3")
                              MOVE  "3"   TO
                                 LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX-ST)
                           END-IF
                           IF  (LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX-ST)
                                                              =  "4" OR
                                                                 "5" OR
                                                                 "6" OR
                                                                 "7" OR
                                                                 "8")
                                               AND
                               (LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX-ST)
                                                               = "4" OR
                                                                 "5" OR
                                                                 "6" OR
                                                                 "7" OR
                                                                 "8")
                              MOVE  "4"   TO
                                 LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX-ST)
                           END-IF
      *
                      END-IF
                 END-IF
                 MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
                 MOVE    "key33"              TO  MCP-PATHNAME
                 PERFORM 910-PTNYUINRRK-READ-SEC
               END-PERFORM
           END-IF
      *
      *    患者入院履歴クローズ
           MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
           MOVE    "key33"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      **     PERFORM   VARYING  IDX    FROM  1  BY  1 
      **       UNTIL   IDX   >   31
      **       DISPLAY "IDX = " IDX
      **       DISPLAY "DOUJITSU = " LNK-NYUINDAY-DOUJITSU-KBN(IDX)
      **       DISPLAY "HKNHEN   = " LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX)
      **       DISPLAY "TAI-SKJ  = " LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX)
      **       DISPLAY "TAI-HKN  = " LNK-NYUINDAY-DOUJITSU-T-HKNCOMBI(IDX)
      **       DISPLAY "TAI-HKNSYU=" LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX)
      **       DISPLAY "NYU-SKJ  = " LNK-NYUINDAY-DOUJITSU-SKJKBN(IDX)
      **       DISPLAY "NYU-HKN  = " LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX)
      **       DISPLAY "NYU-HKNSYU=" LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX)
      **     END-PERFORM
      **     DISPLAY   "WRK-YOKUJITSU-YMD = " WRK-YOKUJITSU-YMD
      *
      *     IF       LNK-NYUINDAY-TBL   =    SPACE
      *         MOVE     4              TO   LNK-NYUINDAY-RC
      *     END-IF
           .
       200-MAIN-EXT.
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
           PERFORM   900-CLOSE-SEC
      *
           .
       210-NYUINACT-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    同日再入院（退院分）の保険種類取得
      *****************************************************************
       210-HKNSYURUI-GET1-SEC            SECTION.
      *
           MOVE    "1"      TO   LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX-ST)
      *
           INITIALIZE                          HKNCOMBI-REC
           MOVE    LNK-NYUINDAY-HOSPNUM    TO  COMB-HOSPNUM
           MOVE    LNK-NYUINDAY-PTID       TO  COMB-PTID
           MOVE    LNK-NYUINDAY-DOUJITSU-T-HKNCOMBI (IDX-ST)
                                           TO  COMB-HKNCOMBINUM
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-HKNCOMBI-SELECT-SEC
           IF      FLG-HKNCOMBI            =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   910-HKNCOMBI-READ-SEC
               IF   FLG-HKNCOMBI           =   ZERO
      *            国保
                   IF   COMB-HKNNUM        =   "060"  OR
                                               "067"  OR
                                               "068"  OR
                                               "069"
                       MOVE    "2"         TO
                                 LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX-ST)
                   END-IF
      *            後期高齢者
                   IF   COMB-HKNNUM        =   "039"  OR
                                               "040"
                       MOVE    "3"         TO
                                 LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX-ST)
                   END-IF
      *            労災保険
                   IF   COMB-HKNNUM        =   "971"
                       MOVE    "4"         TO
                                 LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX-ST)
                   END-IF
      *            自賠責保険
                   IF   COMB-HKNNUM        =   "973"
                       MOVE    "5"         TO
                                 LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX-ST)
                   END-IF
      *            公害保険
                   IF   COMB-HKNNUM        =   "975"
                       MOVE    "6"         TO
                                 LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX-ST)
                   END-IF
      *            自費
                   IF   COMB-HKNNUM(1:2)   =   "98"
                       MOVE    "7"         TO
                                 LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX-ST)
                   END-IF
      *            治験
                   IF   COMB-HKNNUM(1:2)   =   "90"  OR
                                               "91"
                       MOVE    "8"         TO
                                 LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX-ST)
                   END-IF
               END-IF
      *        保険組合せクローズ
               MOVE    "tbl_hkncombi"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               PERFORM   900-CLOSE-SEC
           ELSE
      *        保険組合せクローズ
               MOVE    "tbl_hkncombi"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               PERFORM   900-CLOSE-SEC
           END-IF
      *
           .
       210-HKNSYURUI-GET1-EXT.
           EXIT.
      *
      *****************************************************************
      *    同日再入院（入院分）の保険種類取得
      *****************************************************************
       210-HKNSYURUI-GET2-SEC            SECTION.
      *
           MOVE    "1"      TO   LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX-ST)
      *
           INITIALIZE                          HKNCOMBI-REC
           MOVE    LNK-NYUINDAY-HOSPNUM    TO  COMB-HOSPNUM
           MOVE    LNK-NYUINDAY-PTID       TO  COMB-PTID
           MOVE    LNK-NYUINDAY-DOUJITSU-HKNCOMBI (IDX-ST)
                                           TO  COMB-HKNCOMBINUM
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-HKNCOMBI-SELECT-SEC
           IF      FLG-HKNCOMBI            =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   910-HKNCOMBI-READ-SEC
               IF   FLG-HKNCOMBI           =   ZERO
      *            国保
                   IF   COMB-HKNNUM        =   "060"  OR
                                               "067"  OR
                                               "068"  OR
                                               "069"
                       MOVE    "2"         TO
                                   LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX-ST)
                   END-IF
      *            後期高齢者
                   IF   COMB-HKNNUM        =   "039"  OR
                                               "040"
                       MOVE    "3"         TO
                                   LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX-ST)
                   END-IF
      *            労災保険
                   IF   COMB-HKNNUM        =   "971"
                       MOVE    "4"         TO
                                   LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX-ST)
                   END-IF
      *            自賠責保険
                   IF   COMB-HKNNUM        =   "973"
                       MOVE    "5"         TO
                                   LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX-ST)
                   END-IF
      *            公害保険
                   IF   COMB-HKNNUM        =   "975"
                       MOVE    "6"         TO
                                   LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX-ST)
                   END-IF
      *            自費
                   IF   COMB-HKNNUM(1:2)   =   "98"
                       MOVE    "7"         TO
                                   LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX-ST)
                   END-IF
      *            治験
                   IF   COMB-HKNNUM(1:2)   =   "90"  OR
                                               "91"
                       MOVE    "8"         TO
                                   LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX-ST)
                   END-IF
               END-IF
      *        保険組合せクローズ
               MOVE    "tbl_hkncombi"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               PERFORM   900-CLOSE-SEC
           ELSE
      *        保険組合せクローズ
               MOVE    "tbl_hkncombi"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               PERFORM   900-CLOSE-SEC
           END-IF
      *
           .
       210-HKNSYURUI-GET2-EXT.
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
      *    患者入院履歴マスター検索
      *****************************************************************
       910-PTNYUINRRK-SELECT-SEC        SECTION.
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
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
           PERFORM   900-DBFETCH-SEC
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
      *    入院診療行為マスター検索
      *****************************************************************
       910-NYUINACT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
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
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACT-REC
               MOVE    ZERO                TO  FLG-NYUINACT
           ELSE
               INITIALIZE                      NYUINACT-REC
               MOVE    1                   TO  FLG-NYUINACT
           END-IF
           .
       910-NYUINACT-READ-EXT.
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
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  SRYACCT-REC
               MOVE    ZERO               TO  FLG-SRYACCT
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
               MOVE    ZERO               TO  FLG-SRYACT
           ELSE
               MOVE    1                  TO  FLG-SRYACT
           END-IF
      *
           .
       910-SRYACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       910-SRYACT-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
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
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
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
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
      *
       900-CLOSE-EXT.
           EXIT.
