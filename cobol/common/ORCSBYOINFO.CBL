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
       PROGRAM-ID.             ORCSBYOINFO.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 病名一覧返却サブ
      *  管理者            : 
      *  11/08/16    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTBYOMEI        PIC 9(01).
           03  FLG-BYOMEI          PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
       01  WRK-AREA.
           03  WRK-YM              PIC 9(06).
      *                             
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
      *    病名システム予約コードテーブル
           COPY    "CMBYOMEICD.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    病名
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *****************************************************************
      *    連絡領域
      ****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSBYOINFO.INC".
           COPY    "COMMON-SPA".
      *****************************************************************
      *
       PROCEDURE                   DIVISION    USING
                                   ORCSBYOINFOAREA
                                   SPA-AREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           DISPLAY "SBYOINFO-RC=" SBYOINFO-RC
            .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *     
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           DISPLAY "SRYYM =" SBYOINFO-SRYYM
           DISPLAY "PTNUM =" SBYOINFO-PTNUM
      *
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
      *
           IF      SBYOINFO-PTID   =   ZERO
               INITIALIZE                  ORCSPTNUMAREA
               MOVE    SBYOINFO-PTNUM  TO  SPTNUM-PTNUM
               CALL    "ORCSPTNUM"     USING   ORCSPTNUMAREA
                                               SPA-AREA
               MOVE    SPTNUM-PTID     TO  SBYOINFO-PTID
           END-IF
           .
       100-INIT-EXT.
      *
           EXIT
           .
      *     
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      * 
           INITIALIZE                  PTBYOMEI-REC
           MOVE    SPA-HOSPNUM     TO  PTBYO-HOSPNUM
           MOVE    SBYOINFO-PTID   TO  PTBYO-PTID
           MOVE    SBYOINFO-SRYYM  TO  PTBYO-SRYYMD (1:6)
           MOVE    "31"            TO  PTBYO-SRYYMD (7:2)
           MOVE    "%%"            TO  PTBYO-SRYKA
           MOVE    "tbl_ptbyomei"  TO  WRK-MCP-TABLE
           MOVE    "key37"         TO  WRK-MCP-PATHNAME
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM         UNTIL ( IDX             >=  200 )
                           OR    ( FLG-PTBYOMEI    =   1   )
               PERFORM 2001-PTBYOMEI-HENSYU-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      FLG-PTBYOMEI        =   ZERO
               MOVE    9                   TO  SBYOINFO-RC
           END-IF
           .
       200-MAIN-EXT.
           EXIT
           .
      *     
      *****************************************************************
      *    患者病名編集処理
      *****************************************************************
       2001-PTBYOMEI-HENSYU-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
      *
      *    対象の病名かチェック
           IF      PTBYO-SRYYMD (1:6)  =   SBYOINFO-SRYYM
               CONTINUE
           ELSE
               IF      PTBYO-TENKIKBN       =   SPACE
                   CONTINUE
               ELSE 
                   MOVE    PTBYO-TENKIYMD(1:6)
                                               TO  WRK-YM
                   IF      WRK-YM              >=  SBYOINFO-SRYYM
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-IF
           END-IF    
      *
      *!レセプト表示は判定しない
      *    レセプト表示フラグ
      **** IF      PTBYO-REZEFLG       =   "1"
      *        MOVE    1                   TO  FLG-CHK
      *    END-IF
      *
      *    レセプト表示月数
      *    IF      PTBYO-REZEMM        >   ZERO
      *        INITIALIZE                      STS-AREA-DAY
      *        INITIALIZE                      LNK-DAY6-AREA
      *        MOVE    "61"                TO  LNK-DAY6-IRAI
      *        MOVE    PTBYO-SRYYMD        TO  LNK-DAY6-KIJUN 
      *        MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
      *        COMPUTE LNK-DAY6-ZOGEN  =   PTBYO-REZEMM    -   1    
      *        CALL    "ORCSDAY"           USING   STS-AREA-DAY
      *                                            LNK-DAY6-AREA
      *        MOVE    LNK-DAY6-KEISAN (1:6)
      *                                    TO  WRK-YM
      *        IF      WRK-YM              >=  SBYOINFO-SRYYM
      *            CONTINUE
      *        ELSE
      *            MOVE    1                   TO  FLG-CHK
      *        END-IF
      *****END-IF
      *!
      *
           IF      FLG-CHK             =   ZERO
               ADD     1                   TO  IDX
      *
               MOVE    PTBYO-SRYKA     TO  SBYOINFO-SRYKA      (IDX)
               MOVE    PTBYO-SRYYMD    TO  SBYOINFO-SRYYMD     (IDX)
               MOVE    PTBYO-BYOMEIINPUTCD-G
                                       TO  SBYOINFO-BYOMEIINPUTCD-G
                                                               (IDX)
               MOVE    PTBYO-BYOMEICD-G
                                       TO  SBYOINFO-BYOMEICD-G (IDX)
               MOVE    PTBYO-BYOMEI    TO  SBYOINFO-BYOMEI     (IDX)
               MOVE    PTBYO-CHARTBYOMEI
                                       TO  SBYOINFO-CHARTBYOMEI(IDX)
               MOVE    PTBYO-HOSOKU-COMMENT
                                       TO  SBYOINFO-HOSOKU-COMMENT
                                                               (IDX)
               MOVE    PTBYO-HOSOKUCD-G
                                       TO  SBYOINFO-HOSOKUCD-G (IDX)
               MOVE    PTBYO-TENKIYMD  TO  SBYOINFO-TENKIYMD   (IDX)
               MOVE    PTBYO-TENKIKBN  TO  SBYOINFO-TENKIKBN   (IDX)
               MOVE    PTBYO-UTAGAIFLG TO  SBYOINFO-UTAGAIFLG  (IDX)
               MOVE    PTBYO-SYUBYOFLG TO  SBYOINFO-SYUBYOFLG  (IDX)
               MOVE    PTBYO-MANSEIKBN TO  SBYOINFO-MANSEIKBN  (IDX)
               MOVE    PTBYO-NYUGAIKBN TO  SBYOINFO-NYUGAIKBN  (IDX)
               MOVE    PTBYO-HKNCOMBI  TO  SBYOINFO-HKNCOMBI   (IDX)
               MOVE    PTBYO-REZEFLG   TO  SBYOINFO-REZEFLG    (IDX)
               MOVE    PTBYO-REZEMM    TO  SBYOINFO-REZEMM     (IDX)
               MOVE    PTBYO-HKNBYOMEIFLG
                                       TO  SBYOINFO-HKNBYOMEIFLG
                                                               (IDX)
               MOVE    PTBYO-BYOMEIHENFLG
                                       TO  SBYOINFO-BYOMEIHENFLG
                                                               (IDX)
      *??
               DISPLAY "IDX=" IDX " SRYKA=" SBYOINFO-SRYKA      (IDX)
                       " SRYYMD=" SBYOINFO-SRYYMD     (IDX)
                       " BYOMEI=" SBYOINFO-BYOMEI     (IDX)
      *??
      *
               PERFORM VARYING IDY     FROM    1   BY  1
                       UNTIL   IDY     >       21
                       OR      SBYOINFO-BYOMEICD (IDX IDY)
                                       =   SPACE
                   INITIALIZE                      BYOMEI-REC
                   MOVE    SBYOINFO-BYOMEICD (IDX IDY)
                                               TO  BYO-BYOMEICD
                   IF  (   BYO-BYOMEICD (1:3)    =   "ZZZ"    )
                   AND ( ( BYO-BYOMEICD (4:1)    <   "1" ) OR 
                         ( BYO-BYOMEICD (4:1)    =   "C" )    )
      *                予約病名コード検索
                       SET     TBL-BYOMEICD-IDX  TO  1
                       SEARCH  TBL-BYOMEICD-OCC
                                           VARYING TBL-BYOMEICD-IDX
                           AT  END
                               MOVE    "1"             TO     
                                               SBYOINFO-FLG (IDX IDY)
                           WHEN    BYO-BYOMEICD    =
                                               TBL-YYK-BYOMEICD
                                                      (TBL-BYOMEICD-IDX)
                               MOVE    TBL-YYK-BYOMEI (TBL-BYOMEICD-IDX)
                                                   TO
                                           SBYOINFO-BYOMEI-T (IDX IDY)
                               MOVE    ZERO        TO  FLG-BYOMEI
                       END-SEARCH
                   ELSE
      *                病名マスタ検索
                       IF      BYO-BYOMEICD    =   "0000999"
                           MOVE    SPACE           TO
                                           SBYOINFO-BYOMEI-T (IDX IDY)
                       ELSE 
                           PERFORM 900-BYOMEI-READ-SEC
                           IF      FLG-BYOMEI      =   ZERO
                               MOVE    BYO-BYOMEI      TO
                                           SBYOINFO-BYOMEI-T (IDX IDY)
                               IF      BYO-HAISIYMD    =   "99999999"
                                                       OR  SPACE
                                   CONTINUE
                               ELSE
                                   MOVE    "2"             TO  
                                               SBYOINFO-FLG (IDX IDY)
                               END-IF
                           ELSE
                               MOVE    "1"             TO     
                                               SBYOINFO-FLG (IDX IDY)
                           END-IF
                       END-IF
                   END-IF
      *??
               DISPLAY "IDY=" IDY
                  " BYOMEICD=" SBYOINFO-BYOMEICD (IDX IDY)
                  " FLG="          SBYOINFO-FLG (IDX IDY)
                  " BYOMEI="   SBYOINFO-BYOMEI-T (IDX IDY)
      *??
               END-PERFORM
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-PTBYO-READ-SEC
      * 
           .
       2001-PTBYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスタ読込
      *****************************************************************
       900-PTBYO-READ-SEC              SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC
               MOVE    ZERO            TO  FLG-PTBYOMEI
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           .
       900-PTBYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名マスタ読込
      *****************************************************************
       900-BYOMEI-READ-SEC         SECTION.
      *
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_byomei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-BYOMEI
                   MOVE    MCPDATA-REC     TO  BYOMEI-REC
               ELSE
                   MOVE    1               TO  FLG-BYOMEI
                   INITIALIZE                  BYOMEI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-BYOMEI
               INITIALIZE                  BYOMEI-REC
           END-IF
      *
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-BYOMEI-READ-EXT.
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
