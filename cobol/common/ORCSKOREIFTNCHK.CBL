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
       PROGRAM-ID.             ORCSKOREIFTNCHK.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 高齢者の一部負担金額記載に関するチェック
      *  管理者            : 
      *  作成日付    作業者        記述
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.02.01    NACL-門脇    06/08/10  高齢者上限額経過措置対応
      *  03.03.01    NACL-門脇    06/09/29  Ｈ１８年１０月改正に伴う一部
      *                                     負担金編集対応
      *  03.03.02    NACL-門脇    06/10/10  老齢福祉年金についての対応
      *  03.03.03    NACL-門脇    06/11/02  減額・免除・支払猶予対応
      *  03.04.01    NACL-門脇    06/11/20  高額４回目以降対応
      *  03.04.02    NACL-門脇    07/03/14  保険欄一部負担金記載修正
      *  03.04.03    NACL-門脇    07/04/01  現物給付対応
      *  03.04.04    NACL-門脇    07/06/13  現物給付高額４回目公費併用時対応
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  03.05.02    NACL-門脇    07/09/03  減額減免全国公費対応
      *  03.05.03    NACL-門脇    07/12/03  北海道特定疾患（道単独事業）対応
      *  04.02.01    NACL-門脇    08/04/01  平成２０年４月改正対応
      *  04.02.02    NACL-門脇    08/05/01  後期高齢者月途中保険者変更対応
      *
      *  04.03.01    NACL-門脇    09/01/05  平成２１年１月改正対応
      *  04.03.02    NACL-門脇    09/02/26  長崎県地方公費対応
      *
      *  04.04.01    NACL-門脇    09/04/13  平成２１年５月改正対応
      *  04.04.02    NACL-門脇    09/06/01  平成２１年５月改正対応
      *                                     （東京都１５１）
      *  04.04.03    NACL-門脇    09/09/07  愛知県単独５１対応
      *
      *  04.05.01    NACL-門脇    11/01/14  保険別請求チェック表の本体取込み
      *****************************************************************
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
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTINF             PIC 9(01).
           03  FLG-HKNCOMBI          PIC 9(01).
           03  FLG-SYUNOU            PIC 9(01).
           03  FLG-ZENKOH-ARI        PIC 9(01).
           03  FLG-PTHKNINF          PIC 9(01).
           03  FLG-TOKUREI-HKNJYA    PIC 9(01).
           03  FLG-PTKOHINF          PIC 9(01).
           03  FLG-CYOKINOMI         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                  PIC 9(04).
           03  IDX2                  PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-HKNCOMBINUM-TBL.
             04  WRK-HKNCOMBINUM-OCC    OCCURS  50.
               05  WRK-HKNCOMBINUM   PIC 9(04).
               05  WRK-KYURATE       PIC 9(03).
               05  WRK-ZENKOH-ARI    PIC 9(01).
           03  WRK-FTNKEI            PIC 9(09).
           03  WRK-KINGAKU           PIC 9(09).
           03  WRK-KINGAKU2          PIC 9(09).
           03  WRK-FTN-KOH-XXX       PIC 9(09).
           03  WRK-FTN-KOH-YYY       PIC 9(09).
           03  WRK-GENGAKU-RITUX     PIC 9(03).
      *
           03  WRK-DOTANDOKU-TBL.
             04  WRK-DOTANDOKU-OCC      OCCURS   4.
               05  WRK-DOTANDOKU     PIC 9(01).
      *
           03  WRK-CYOKI-HKNCOMBINUM PIC 9(04).
           03  WRK-KOHID-CYOKI       PIC 9(10).
      *
      *    共通領域
           COPY    "MCPAREA".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "MCPDATA.INC".
      ***  COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSKOREIFTNCHK.INC".
      *
grpsys     COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           ORCSKOREIFTNCHKAREA
grpsys     SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           MOVE    ZERO                TO  LNK-KOREIFTNCHK-RC
           INITIALIZE                      LNK-KOREIFTNCHK-OUT-AREA
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    パラメータチェック
           PERFORM   100-PRM-CHK-SEC
      *
           IF    LNK-KOREIFTNCHK-RC    =   ZERO
               PERFORM   200-MAIN-SEC
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    パラメータチェック
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    患者マスター検索
           INITIALIZE                          PTINF-REC
grpsys     MOVE    LNK-KOREIFTNCHK-HOSPNUM TO  PTINF-HOSPNUM
           MOVE    LNK-KOREIFTNCHK-PTID    TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    10              TO  LNK-KOREIFTNCHK-RC
           END-IF
      *
           IF    LNK-KOREIFTNCHK-HKNNUM    = SPACE
               MOVE    10              TO  LNK-KOREIFTNCHK-RC
           END-IF
      *
           IF    LNK-KOREIFTNCHK-HKNID     =  ZERO
               MOVE    10              TO  LNK-KOREIFTNCHK-RC
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
           PERFORM    250-PTHKNINF-KENSAKU-SEC
      *
           MOVE    ZERO                    TO  IDX1
      *    対象の保険組合せ番号取得
           INITIALIZE                          HKNCOMBI-REC
grpsys     MOVE    LNK-KOREIFTNCHK-HOSPNUM TO  COMB-HOSPNUM
           MOVE    LNK-KOREIFTNCHK-PTID    TO  COMB-PTID
           MOVE    LNK-KOREIFTNCHK-HKNID   TO  COMB-HKNID
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key5"                  TO  MCP-PATHNAME
           PERFORM 900-HKNCOMBI-SELECT-SEC
           IF      FLG-HKNCOMBI            NOT =   ZERO
               MOVE    01                  TO  LNK-KOREIFTNCHK-RC
               GO  TO  200-MAIN-EXT
           ELSE
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
               PERFORM
                    UNTIL   (IDX1             >=    50 )   OR
                            (FLG-HKNCOMBI  NOT =   ZERO)
      *
                   ADD     1     TO     IDX1
                   MOVE    COMB-HKNCOMBINUM  TO
                                     WRK-HKNCOMBINUM(IDX1)
      ***
                   INITIALIZE        WRK-DOTANDOKU-TBL
      *
                   IF  (LNK-KOREIFTNCHK-PREFNUM   = "01"    )
                   AND (LNK-KOREIFTNCHK-SRYYM    >= "200510")
                   AND (LNK-KOREIFTNCHK-SRYYM    <  "200810")
      *
                     PERFORM    VARYING   IDX2    FROM   1   BY   1
                                 UNTIL    IDX2      >    4
      *
                      IF  COMB-KOHHKNNUM(IDX2) = "051"
                        INITIALIZE                       PTKOHINF-REC
                        MOVE LNK-KOREIFTNCHK-HOSPNUM TO  PTKOH-HOSPNUM
                        MOVE LNK-KOREIFTNCHK-PTID    TO  PTKOH-PTID
                        MOVE COMB-KOHID(IDX2)        TO  PTKOH-KOHID
                        MOVE "tbl_ptkohinf"          TO  MCP-TABLE
                        MOVE "key8"                  TO  MCP-PATHNAME
                        PERFORM 900-PTKOHINF-SELECT-SEC
                        IF      FLG-PTKOHINF         =       ZERO
                            MOVE    "tbl_ptkohinf"   TO  MCP-TABLE
                            MOVE    "key8"           TO  MCP-PATHNAME
                            PERFORM 910-PTKOHINF-READ-SEC
                            IF      FLG-PTKOHINF     =       ZERO
                              IF (PTKOH-JKYSNUM(1:2) >= "91"
                                                 AND <= "95")
                              OR (PTKOH-JKYSNUM(1:2) >= "71"
                                                 AND <= "88")
                                  MOVE   1   TO  WRK-DOTANDOKU(IDX2)
                              END-IF
                            END-IF
                        END-IF
                        MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                        MOVE    "key8"               TO  MCP-PATHNAME
                        PERFORM  920-DB-CLOSE-SEC
                      END-IF
      *
                     END-PERFORM
      *
                   END-IF
      ***
                   IF   COMB-KOH1HKNNUM =  "027"
                     IF ((COMB-KOH2HKNNUM(1:1)  =  "0" OR "9") AND
                         (COMB-KOH2HKNNUM(1:2) NOT = "99"    ) AND
                         (WRK-DOTANDOKU(2)         =  ZERO   ))
                     OR ((COMB-KOH3HKNNUM(1:1)  =  "0" OR "9") AND
                         (COMB-KOH3HKNNUM(1:2) NOT = "99"    ) AND
                         (WRK-DOTANDOKU(3)         =  ZERO   ))
                     OR ((COMB-KOH4HKNNUM(1:1)  =  "0" OR "9") AND
                         (COMB-KOH4HKNNUM(1:2) NOT = "99"    ) AND
                         (WRK-DOTANDOKU(4)         =  ZERO   ))
                        MOVE     1    TO   WRK-ZENKOH-ARI(IDX1)
                     END-IF
                   ELSE
                     IF ((COMB-KOH1HKNNUM(1:1)  =  "0" OR "9") AND
                         (COMB-KOH1HKNNUM(1:2) NOT = "99"    ) AND
                         (WRK-DOTANDOKU(1)         =  ZERO   ))
                     OR ((COMB-KOH2HKNNUM(1:1)  =  "0" OR "9") AND
                         (COMB-KOH2HKNNUM(1:2) NOT = "99"    ) AND
                         (WRK-DOTANDOKU(2)         =  ZERO   ))
                     OR ((COMB-KOH3HKNNUM(1:1)  =  "0" OR "9") AND
                         (COMB-KOH3HKNNUM(1:2) NOT = "99"    ) AND
                         (WRK-DOTANDOKU(3)         =  ZERO   ))
                     OR ((COMB-KOH4HKNNUM(1:1)  =  "0" OR "9") AND
                         (COMB-KOH4HKNNUM(1:2) NOT = "99"    ) AND
                         (WRK-DOTANDOKU(4)         =  ZERO   ))
                     OR ((LNK-KOREIFTNCHK-PREFNUM  = "13"    ) AND
                         (COMB-KOH1HKNNUM          = "151"   ) AND
                         (LNK-KOREIFTNCHK-SRYYM   >= "200905"))
                     OR ((LNK-KOREIFTNCHK-PREFNUM  = "23"    ) AND
                         (COMB-KOH1HKNNUM          = "191"   ) AND
                         (LNK-KOREIFTNCHK-SRYYM   >= "200910"))
                        MOVE     1    TO   WRK-ZENKOH-ARI(IDX1)
                     END-IF
                   END-IF
                   PERFORM   210-KYURATE-HENSYU-SEC
      *
                   MOVE   "tbl_hkncombi"   TO  MCP-TABLE
                   MOVE   "key5"           TO  MCP-PATHNAME
                   PERFORM 910-HKNCOMBI-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE     "tbl_hkncombi"   TO  MCP-TABLE
           MOVE     "key5"           TO  MCP-PATHNAME
           PERFORM   920-DB-CLOSE-SEC
      *
      *    収納マスタから一部負担額チェック等
           MOVE       ZERO      TO    FLG-CYOKINOMI
           MOVE       ZERO      TO    WRK-CYOKI-HKNCOMBINUM
           IF      LNK-KOREIFTNCHK-NYUGAIKBN  =  "1"
               PERFORM       220-NYUIN-SYUKEI-SEC
           ELSE
               IF  LNK-KOREIFTNCHK-RECEKA     =  SPACE
                   PERFORM   230-GAIRAI-SYUKEI-SEC
               END-IF
           END-IF
      *
           MOVE       ZERO      TO    WRK-KOHID-CYOKI
           IF     LNK-KOREIFTNCHK-RC         =   ZERO
             IF   LNK-KOREIFTNCHK-CYOKINOMI  =   ZERO
                CONTINUE
             ELSE
                IF    FLG-CYOKINOMI              =    1
                AND   WRK-CYOKI-HKNCOMBINUM  NOT =   ZERO
                  INITIALIZE                       HKNCOMBI-REC
                  MOVE LNK-KOREIFTNCHK-HOSPNUM TO  COMB-HOSPNUM
                  MOVE LNK-KOREIFTNCHK-PTID    TO  COMB-PTID
                  MOVE WRK-CYOKI-HKNCOMBINUM   TO  COMB-HKNCOMBINUM
                  MOVE "tbl_hkncombi"          TO  MCP-TABLE
                  MOVE "key"                   TO  MCP-PATHNAME
                  PERFORM 900-HKNCOMBI-SELECT-SEC
                  IF   FLG-HKNCOMBI            =   ZERO
                    MOVE "tbl_hkncombi"        TO  MCP-TABLE
                    MOVE "key"                 TO  MCP-PATHNAME
                    PERFORM 910-HKNCOMBI-READ-SEC
                    IF FLG-HKNCOMBI            =   ZERO
                       MOVE   COMB-KOH1ID      TO  WRK-KOHID-CYOKI
                    END-IF
                  END-IF
                  MOVE  "tbl_hkncombi"         TO  MCP-TABLE
                  MOVE  "key"                  TO  MCP-PATHNAME
                  PERFORM   920-DB-CLOSE-SEC
      *
                  IF  LNK-KOREIFTNCHK-KOHID-CYOKI  NOT = ZERO
                  AND LNK-KOREIFTNCHK-KOHID-CYOKI   =
                                              WRK-KOHID-CYOKI
                    CONTINUE
                  ELSE
                    INITIALIZE     LNK-KOREIFTNCHK-OUT-AREA
                  END-IF
                ELSE
                    INITIALIZE     LNK-KOREIFTNCHK-OUT-AREA
                END-IF
             END-IF
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      ***************************************************************
      *    患者保険情報検索
      ***************************************************************
       250-PTHKNINF-KENSAKU-SEC           SECTION.
      *
           MOVE       ZERO               TO    FLG-TOKUREI-HKNJYA
      *
           INITIALIZE                          PTHKNINF-REC
grpsys     MOVE   LNK-KOREIFTNCHK-HOSPNUM
                                         TO    PTHKN-HOSPNUM
           MOVE   LNK-KOREIFTNCHK-PTID   TO    PTHKN-PTID
           MOVE   LNK-KOREIFTNCHK-HKNID  TO    PTHKN-HKNID
           MOVE   PTHKNINF-REC           TO    MCPDATA-REC
           MOVE   "DBSELECT"             TO    MCP-FUNC
           MOVE   "tbl_pthkninf"         TO    MCP-TABLE
           MOVE   "key"                  TO    MCP-PATHNAME
           CALL   "ORCDBMAIN"            USING MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC            =     ZERO
               MOVE    "tbl_pthkninf"    TO    MCP-TABLE
               MOVE    "key"             TO    MCP-PATHNAME
               PERFORM  900-PTHKNINF-READ-SEC
               IF     FLG-PTHKNINF   =     ZERO
                  IF   PTHKN-HKNJANUM  = "67330753"
                      MOVE    1          TO    FLG-TOKUREI-HKNJYA
                  END-IF
               END-IF
           END-IF
      *
           MOVE     "tbl_pthkninf"       TO    MCP-TABLE
           MOVE     "key"                TO    MCP-PATHNAME
           PERFORM   920-DB-CLOSE-SEC
      *
           .
      *
       250-PTHKNINF-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主保険の負担割合編集
      *****************************************************************
       210-KYURATE-HENSYU-SEC            SECTION.
      *
           EVALUATE  LNK-KOREIFTNCHK-NENREIKBN
             WHEN     "1"
              IF  COMB-KOH1-HKNKOHSBTKBN   =   "3"
                EVALUATE   COMB-KOH1PAYKBN
                   WHEN   "01"
                       MOVE   10     TO   WRK-KYURATE(IDX1)
                   WHEN   "02"
                       MOVE   20     TO   WRK-KYURATE(IDX1)
                   WHEN   "03"
                       MOVE   30     TO   WRK-KYURATE(IDX1)
                END-EVALUATE
              ELSE
               IF      LNK-KOREIFTNCHK-NYUGAIKBN  =  "1"
                EVALUATE   COMB-HOJOKBN
                   WHEN   "1"
                   WHEN   "5"
                   WHEN   "9"
                       MOVE   10     TO   WRK-KYURATE(IDX1)
                   WHEN   "2"
                   WHEN   "6"
                   WHEN   "8"
                       MOVE   20     TO   WRK-KYURATE(IDX1)
                   WHEN   "3"
                   WHEN   "7"
                       MOVE   30     TO   WRK-KYURATE(IDX1)
                END-EVALUATE
               ELSE
                EVALUATE   COMB-HOJOKBN
                   WHEN   "1"
                   WHEN   "4"
                   WHEN   "9"
                       MOVE   10     TO   WRK-KYURATE(IDX1)
                   WHEN   "2"
                   WHEN   "5"
                   WHEN   "8"
                       MOVE   20     TO   WRK-KYURATE(IDX1)
                   WHEN   "3"
                   WHEN   "6"
                   WHEN   "7"
                       MOVE   30     TO   WRK-KYURATE(IDX1)
                END-EVALUATE
               END-IF
              END-IF
             WHEN     "2"
              IF      LNK-KOREIFTNCHK-NYUGAIKBN  =  "1"
               EVALUATE    COMB-HKNNUM
                 WHEN    "060"
                    IF ((LNK-KOREIFTNCHK-SRYYM   <=  "200803") AND
                        (LNK-KOREIFTNCHK-AGE          <   3  ))
                    OR ((LNK-KOREIFTNCHK-SRYYM   >=  "200804") AND
                        (LNK-KOREIFTNCHK-SYUGAKUKBN   =   1  ))
                       IF    COMB-HOJOKBN = "0" OR "1" OR "4" OR "5"
                          EVALUATE   COMB-HOJOKBN
                            WHEN   "0"
                            WHEN   "4"
                              MOVE   ZERO    TO   WRK-KYURATE(IDX1)
                            WHEN   "1"
                            WHEN   "5"
                              MOVE   10      TO   WRK-KYURATE(IDX1)
                          END-EVALUATE
                       ELSE
                          MOVE       20      TO   WRK-KYURATE(IDX1)
                       END-IF
                    ELSE
                       EVALUATE   COMB-HOJOKBN
                         WHEN   "0"
                         WHEN   "4"
                            MOVE   ZERO      TO   WRK-KYURATE(IDX1)
                         WHEN   "1"
                         WHEN   "5"
                            MOVE   10        TO   WRK-KYURATE(IDX1)
                         WHEN   "2"
                         WHEN   "6"
                            MOVE   20        TO   WRK-KYURATE(IDX1)
                         WHEN   "3"
                            MOVE   30        TO   WRK-KYURATE(IDX1)
                       END-EVALUATE
                    END-IF
                 WHEN    OTHER
                    IF ((LNK-KOREIFTNCHK-SRYYM   <=  "200803") AND
                        (LNK-KOREIFTNCHK-AGE          <   3  ))
                    OR ((LNK-KOREIFTNCHK-SRYYM   >=  "200804") AND
                        (LNK-KOREIFTNCHK-SYUGAKUKBN   =   1  ))
                           MOVE     20       TO   WRK-KYURATE(IDX1)
                    ELSE
                       IF   COMB-HOJOKBN  =  SPACE
                         IF   FLG-TOKUREI-HKNJYA  =   1
                           MOVE     20       TO   WRK-KYURATE(IDX1)
                         ELSE
                           MOVE     30       TO   WRK-KYURATE(IDX1)
                         END-IF
                       END-IF
                    END-IF
               END-EVALUATE
              ELSE
               EVALUATE    COMB-HKNNUM
                 WHEN    "060"
                    IF ((LNK-KOREIFTNCHK-SRYYM   <=  "200803") AND
                        (LNK-KOREIFTNCHK-AGE          <   3  ))
                    OR ((LNK-KOREIFTNCHK-SRYYM   >=  "200804") AND
                        (LNK-KOREIFTNCHK-SYUGAKUKBN   =   1  ))
                       IF    COMB-HOJOKBN = "0" OR "1" OR "4"
                          EVALUATE   COMB-HOJOKBN
                            WHEN   "0"
                              MOVE   ZERO    TO   WRK-KYURATE(IDX1)
                            WHEN   "1"
                            WHEN   "4"
                              MOVE   10      TO   WRK-KYURATE(IDX1)
                          END-EVALUATE
                       ELSE
                          MOVE       20      TO   WRK-KYURATE(IDX1)
                       END-IF
                    ELSE
                       EVALUATE   COMB-HOJOKBN
                         WHEN   "0"
                            MOVE   ZERO      TO   WRK-KYURATE(IDX1)
                         WHEN   "1"
                         WHEN   "4"
                            MOVE   10        TO   WRK-KYURATE(IDX1)
                         WHEN   "2"
                         WHEN   "5"
                            MOVE   20        TO   WRK-KYURATE(IDX1)
                         WHEN   "3"
                         WHEN   "6"
                            MOVE   30        TO   WRK-KYURATE(IDX1)
                       END-EVALUATE
                    END-IF
                 WHEN    OTHER
                    IF ((LNK-KOREIFTNCHK-SRYYM   <=  "200803") AND
                        (LNK-KOREIFTNCHK-AGE          <   3  ))
                    OR ((LNK-KOREIFTNCHK-SRYYM   >=  "200804") AND
                        (LNK-KOREIFTNCHK-SYUGAKUKBN   =   1  ))
                           MOVE     20       TO   WRK-KYURATE(IDX1)
                    ELSE
                       IF   COMB-HOJOKBN  =  SPACE
                         IF   FLG-TOKUREI-HKNJYA  =   1
                           MOVE     20       TO   WRK-KYURATE(IDX1)
                         ELSE
                           MOVE     30       TO   WRK-KYURATE(IDX1)
                         END-IF
                       END-IF
                    END-IF
               END-EVALUATE
              END-IF
           END-EVALUATE
      *
           .
       210-KYURATE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスタから一部負担額チェック等（入院）
      *****************************************************************
       220-NYUIN-SYUKEI-SEC            SECTION.
      *
      *    全国公費を含む保険組合わせの収納があるかチェック
           PERFORM   VARYING   IDX1   FROM  1  BY  1
                 UNTIL   (IDX1                  >    50 )   OR
                         (WRK-HKNCOMBINUM(IDX1) =   ZERO)
      *
              IF   WRK-ZENKOH-ARI(IDX1)  =  1
                 INITIALIZE                            SYUNOU-REC
grpsys           MOVE    LNK-KOREIFTNCHK-HOSPNUM   TO  SYU-HOSPNUM
                 MOVE    LNK-KOREIFTNCHK-NYUGAIKBN TO  SYU-NYUGAIKBN
                 MOVE    LNK-KOREIFTNCHK-PTID      TO  SYU-PTID
                 MOVE    LNK-KOREIFTNCHK-SRYYM     TO  SYU-SRYYMD(1:6)
                 MOVE    "__"                      TO  SYU-SRYYMD(7:2)
                 MOVE    WRK-HKNCOMBINUM(IDX1)     TO  SYU-HKNCOMBINUM
                 MOVE    "tbl_syunou"              TO  MCP-TABLE
                 MOVE    "key19"                   TO  MCP-PATHNAME
                 PERFORM 900-SYUNOU-SELECT-SEC
                 IF      FLG-SYUNOU            NOT =   ZERO
                     MOVE    01                TO  LNK-KOREIFTNCHK-RC
                     GO  TO  220-NYUIN-SYUKEI-EXT
                 ELSE
                     MOVE   "tbl_syunou"           TO  MCP-TABLE
                     MOVE   "key19"                TO  MCP-PATHNAME
                     PERFORM 910-SYUNOU-READ-SEC
                     IF    FLG-SYUNOU   =    ZERO
                         MOVE    1    TO    FLG-ZENKOH-ARI
                         MOVE   50    TO    IDX1
                     END-IF
                 END-IF
      *
                 MOVE     "tbl_syunou"             TO  MCP-TABLE
                 MOVE     "key19"                  TO  MCP-PATHNAME
                 PERFORM   920-DB-CLOSE-SEC
      *
              END-IF
      *
           END-PERFORM
      *
      *    一部負担額チェック
           PERFORM   VARYING   IDX1   FROM  1  BY  1
                 UNTIL   (IDX1                  >    50 )   OR
                         (WRK-HKNCOMBINUM(IDX1) =   ZERO)
      *
              INITIALIZE                            SYUNOU-REC
grpsys        MOVE    LNK-KOREIFTNCHK-HOSPNUM   TO  SYU-HOSPNUM
              MOVE    LNK-KOREIFTNCHK-NYUGAIKBN TO  SYU-NYUGAIKBN
              MOVE    LNK-KOREIFTNCHK-PTID      TO  SYU-PTID
              MOVE    LNK-KOREIFTNCHK-SRYYM     TO  SYU-SRYYMD(1:6)
              MOVE    "__"                      TO  SYU-SRYYMD(7:2)
              MOVE    WRK-HKNCOMBINUM(IDX1)     TO  SYU-HKNCOMBINUM
              MOVE    "tbl_syunou"              TO  MCP-TABLE
              MOVE    "key19"                   TO  MCP-PATHNAME
              PERFORM 900-SYUNOU-SELECT-SEC
              IF      FLG-SYUNOU            NOT =   ZERO
                  MOVE    01                    TO  LNK-KOREIFTNCHK-RC
                  GO  TO  220-NYUIN-SYUKEI-EXT
              ELSE
                  MOVE   "tbl_syunou"           TO  MCP-TABLE
                  MOVE   "key19"                TO  MCP-PATHNAME
                  PERFORM 910-SYUNOU-READ-SEC
                  PERFORM  UNTIL
                             (FLG-SYUNOU        =    1)
      *
                   IF  ( SYU-KOH-HKNNUM(1)  =  "972" OR "974" )
                   AND ( SYU-KOH-HKNNUM(2)  =  SPACE          )
                   AND ( SYU-KOH-HKNNUM(3)  =  SPACE          )
                   AND ( SYU-KOH-HKNNUM(4)  =  SPACE          )
                      IF    FLG-CYOKINOMI   =  ZERO
                         MOVE     1     TO    FLG-CYOKINOMI
                         MOVE   WRK-HKNCOMBINUM(IDX1)
                                        TO    WRK-CYOKI-HKNCOMBINUM
                      END-IF
                   ELSE
                         MOVE     9     TO    FLG-CYOKINOMI
                         MOVE   ZERO    TO    WRK-CYOKI-HKNCOMBINUM
                   END-IF
      *
                   EVALUATE   LNK-KOREIFTNCHK-NENREIKBN
                     WHEN       "1"
                       PERFORM     2201-NYUIN-KOREI-SYUKEI-SEC
                     WHEN       "2"
                       PERFORM     2202-NYUGAI-WAKODO-SYUKEI-SEC
                   END-EVALUATE
      *
                   MOVE   "tbl_syunou"          TO  MCP-TABLE
                   MOVE   "key19"               TO  MCP-PATHNAME
                   PERFORM 910-SYUNOU-READ-SEC
                  END-PERFORM
              END-IF
      *
              MOVE     "tbl_syunou"             TO  MCP-TABLE
              MOVE     "key19"                  TO  MCP-PATHNAME
              PERFORM   920-DB-CLOSE-SEC
      *
           END-PERFORM
      *
           .
       220-NYUIN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    一部負担額集計（入院）（高齢者）
      *****************************************************************
       2201-NYUIN-KOREI-SYUKEI-SEC            SECTION.
      *
           IF  ( SYU-KOH-HKNNUM(1)  =  SPACE  OR "027" )
           AND ( SYU-KOH-HKNNUM(2)  =  SPACE           )
           AND ( SYU-KOH-HKNNUM(3)  =  SPACE           )
           AND ( SYU-KOH-HKNNUM(4)  =  SPACE           )
             IF   SYU-KOH-HKNNUM(1)  =  "027"
               ADD  SYU-KOH1COMPFTN  TO  LNK-KOREIFTNCHK-FTNGAKU
             ELSE
               ADD  SYU-SYUCOMPFTN   TO  LNK-KOREIFTNCHK-FTNGAKU
             END-IF
           ELSE
             ADD    SYU-KOH-FTN      TO  LNK-KOREIFTNCHK-KOHFTNGAKU
      *
             COMPUTE  WRK-FTNKEI = SYU-TOTAL-HKNTEN
                                 * 10
                                 * WRK-KYURATE(IDX1)
                                 / 100
             COMPUTE  WRK-FTNKEI = WRK-FTNKEI +  5
             COMPUTE  WRK-FTNKEI = WRK-FTNKEI / 10
             COMPUTE  WRK-FTNKEI = WRK-FTNKEI * 10
      *
             IF   ( SYU-KOH-HKNNUM(1)   =   "012" OR "025" )
             OR   ( SYU-KOH-HKNNUM(2)   =   "012" OR "025" )
             OR   ( SYU-KOH-HKNNUM(3)   =   "012" OR "025" )
             OR   ( SYU-KOH-HKNNUM(4)   =   "012" OR "025" )
               IF  LNK-KOREIFTNCHK-TOKU75 = 1 OR 2
                  MOVE    7500   TO   WRK-KINGAKU
               ELSE
                  MOVE    15000  TO   WRK-KINGAKU
               END-IF
               PERFORM  260-ICHIBUFTN-KOH-SET-SEC
             ELSE
               IF   ( SYU-KOH-HKNNUM(1) =   "972" )
               OR   ( SYU-KOH-HKNNUM(2) =   "972" )
               OR   ( SYU-KOH-HKNNUM(3) =   "972" )
               OR   ( SYU-KOH-HKNNUM(4) =   "972" )
                 IF  LNK-KOREIFTNCHK-TOKU75 = 1 OR 2
                    MOVE  5000   TO   WRK-KINGAKU
                 ELSE
                    MOVE  10000  TO   WRK-KINGAKU
                 END-IF
                 PERFORM  260-ICHIBUFTN-KOH-SET-SEC
               ELSE
                 IF  ( SYU-KOH-HKNNUM(1) = "974" )
                 OR  ( SYU-KOH-HKNNUM(2) = "974" )
                 OR  ( SYU-KOH-HKNNUM(3) = "974" )
                 OR  ( SYU-KOH-HKNNUM(4) = "974" )
                   IF  LNK-KOREIFTNCHK-TOKU75 = 1 OR 2
                      MOVE  10000  TO   WRK-KINGAKU
                   ELSE
                      MOVE  20000  TO   WRK-KINGAKU
                   END-IF
                   PERFORM  260-ICHIBUFTN-KOH-SET-SEC
                 ELSE
                   IF    FLG-ZENKOH-ARI  =  ZERO
                     PERFORM  22011-NYUIN-KOREI-HEN1-SEC
                   ELSE
                     IF ((SYU-KOH-HKNNUM(1)     NOT = "027") AND
                         (SYU-KOH-HKNNUM(1)(1:1)    = "0"  ))
                     OR ( SYU-KOH-HKNNUM(2)(1:1)    = "0"   )
                     OR ( SYU-KOH-HKNNUM(3)(1:1)    = "0"   )
                     OR ( SYU-KOH-HKNNUM(4)(1:1)    = "0"   )
                     OR ((LNK-KOREIFTNCHK-PREFNUM  = "13"  ) AND
                         (SYU-KOH-HKNNUM(1)        = "151" ) AND
                         (LNK-KOREIFTNCHK-SRYYM >= "200905"))
                     OR ((LNK-KOREIFTNCHK-PREFNUM  = "23"  ) AND
                         (SYU-KOH-HKNNUM(1)        = "191" ) AND
                         (LNK-KOREIFTNCHK-SRYYM >= "200910"))
      ***              PERFORM   22012-NYUIN-KOREI-HEN2-SEC
                       CONTINUE
                     ELSE
      ***              PERFORM   22011-NYUIN-KOREI-HEN1-SEC
                       CONTINUE
                     END-IF
                   END-IF
                 END-IF
               END-IF
             END-IF
           END-IF
      *
           .
       2201-NYUIN-KOREI-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    一部負担額編集１（入院）（高齢者）
      *****************************************************************
       22011-NYUIN-KOREI-HEN1-SEC               SECTION.
      *
           IF  ((LNK-KOREIFTNCHK-PREFNUM   = "42" ) AND
                (SYU-KOH-HKNNUM(1)         = "186"))
                IF  LNK-KOREIFTNCHK-SRYYM   >=  "200610"
                   IF  LNK-KOREIFTNCHK-TOKU75 = 1 OR 2
                      MOVE    22200  TO   WRK-KINGAKU
                   ELSE
                      MOVE    44400  TO   WRK-KINGAKU
                   END-IF
                   PERFORM    270-ICHIBUFTN-GEN-SEC
                   PERFORM    260-ICHIBUFTN-KOH-SET-SEC
                ELSE
                   MOVE       40200  TO   WRK-KINGAKU
                   PERFORM    270-ICHIBUFTN-GEN-SEC
                   PERFORM    260-ICHIBUFTN-KOH-SET-SEC
                END-IF
           ELSE
            IF   LNK-KOREIFTNCHK-TEISYOTOKU   NOT =  SPACE
              EVALUATE   LNK-KOREIFTNCHK-TEISYOTOKU
                WHEN  "1"
                WHEN  "4"
                   IF  LNK-KOREIFTNCHK-TOKU75 = 1 OR 2
                      MOVE   7500      TO   WRK-KINGAKU
                   ELSE
                      MOVE   15000     TO   WRK-KINGAKU
                   END-IF
                   PERFORM   270-ICHIBUFTN-GEN-SEC
                   PERFORM   260-ICHIBUFTN-KOH-SET-SEC
                WHEN  "2"
                WHEN  "3"
                   IF  LNK-KOREIFTNCHK-TOKU75 = 1 OR 2
                      MOVE   12300     TO   WRK-KINGAKU
                   ELSE
                      MOVE   24600     TO   WRK-KINGAKU
                   END-IF
                   PERFORM   270-ICHIBUFTN-GEN-SEC
                   PERFORM   260-ICHIBUFTN-KOH-SET-SEC
              END-EVALUATE
            ELSE
              IF   (  WRK-KYURATE(IDX1)       =  20 OR 30   )
              AND  (  LNK-KOREIFTNCHK-KEIKASOCHI  =  SPACE  )
              AND  (  LNK-KOREIFTNCHK-KOHYONKAIME =  ZERO   )
                IF  LNK-KOREIFTNCHK-SRYYM   >=  "200610"
                  IF  LNK-KOREIFTNCHK-TOKU75 = 1 OR 2
                   COMPUTE LNK-KOREIFTNCHK-CHIHOUGAKU =
                           LNK-KOREIFTNCHK-CHIHOUGAKU +
                           (SYU-TOTAL-HKNTEN * 10)
                   IF   LNK-KOREIFTNCHK-CHIHOUGAKU  >  133500
                     COMPUTE WRK-KINGAKU2 =
                     (( LNK-KOREIFTNCHK-CHIHOUGAKU - 133500 ) * 0.01 )
                                                              + 0.5
                     COMPUTE WRK-KINGAKU  =  40050 + WRK-KINGAKU2
                   ELSE
                     MOVE    40050  TO   WRK-KINGAKU
                   END-IF
                  ELSE
                   COMPUTE LNK-KOREIFTNCHK-CHIHOUGAKU =
                           LNK-KOREIFTNCHK-CHIHOUGAKU +
                           (SYU-TOTAL-HKNTEN * 10)
                   IF   LNK-KOREIFTNCHK-CHIHOUGAKU  >  267000
                     COMPUTE WRK-KINGAKU2 =
                     (( LNK-KOREIFTNCHK-CHIHOUGAKU - 267000 ) * 0.01 )
                                                              + 0.5
                     COMPUTE WRK-KINGAKU  =  80100 + WRK-KINGAKU2
                   ELSE
                     MOVE    80100  TO   WRK-KINGAKU
                   END-IF
                  END-IF
                  PERFORM   270-ICHIBUFTN-GEN-SEC
                  PERFORM   260-ICHIBUFTN-KOH-SET-SEC
                ELSE
                  COMPUTE LNK-KOREIFTNCHK-CHIHOUGAKU =
                          LNK-KOREIFTNCHK-CHIHOUGAKU +
                          (SYU-TOTAL-HKNTEN * 10)
                  IF   LNK-KOREIFTNCHK-CHIHOUGAKU  >  361500
                    COMPUTE WRK-KINGAKU2 =
                    (( LNK-KOREIFTNCHK-CHIHOUGAKU - 361500 ) * 0.01 )
                                                             + 0.5
                    COMPUTE  WRK-KINGAKU =  72300 + WRK-KINGAKU2
                  ELSE
                    MOVE    72300  TO   WRK-KINGAKU
                  END-IF
                  PERFORM   270-ICHIBUFTN-GEN-SEC
                  PERFORM   260-ICHIBUFTN-KOH-SET-SEC
                END-IF
              ELSE
                IF  LNK-KOREIFTNCHK-SRYYM   >=  "200610"
                   IF  LNK-KOREIFTNCHK-TOKU75 = 1 OR 2
                      MOVE    22200  TO   WRK-KINGAKU
                   ELSE
                      MOVE    44400  TO   WRK-KINGAKU
                   END-IF
                   PERFORM    270-ICHIBUFTN-GEN-SEC
                   PERFORM    260-ICHIBUFTN-KOH-SET-SEC
                ELSE
                   MOVE       40200  TO   WRK-KINGAKU
                   PERFORM    270-ICHIBUFTN-GEN-SEC
                   PERFORM    260-ICHIBUFTN-KOH-SET-SEC
                END-IF
              END-IF
            END-IF
           END-IF
      *
           .
       22011-NYUIN-KOREI-HEN1-EXT.
           EXIT.
      *
      *****************************************************************
      *    一部負担額編集２（入院）（高齢者）
      *****************************************************************
       22012-NYUIN-KOREI-HEN2-SEC               SECTION.
      *
           IF  LNK-KOREIFTNCHK-SRYYM   >=  "200610"
              IF  LNK-KOREIFTNCHK-TOKU75 = 1 OR 2
                 MOVE    22200  TO   WRK-KINGAKU
              ELSE
                 MOVE    44400  TO   WRK-KINGAKU
              END-IF
              PERFORM    260-ICHIBUFTN-KOH-SET-SEC
           ELSE
              MOVE       40200  TO   WRK-KINGAKU
              PERFORM    260-ICHIBUFTN-KOH-SET-SEC
           END-IF
      *
           .
       22012-NYUIN-KOREI-HEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    一部負担額集計（入外）（若人）
      *****************************************************************
       2202-NYUGAI-WAKODO-SYUKEI-SEC            SECTION.
      *
           IF  ( SYU-KOH-HKNNUM(1)  =  "972" OR "974" )
           AND ( SYU-KOH-HKNNUM(2)  =  SPACE          )
           AND ( SYU-KOH-HKNNUM(3)  =  SPACE          )
           AND ( SYU-KOH-HKNNUM(4)  =  SPACE          )
      *
               COMPUTE  WRK-FTNKEI = SYU-TOTAL-HKNTEN
                                   * 10
                                   * WRK-KYURATE(IDX1)
                                   / 100
               COMPUTE  WRK-FTNKEI = WRK-FTNKEI +  5
               COMPUTE  WRK-FTNKEI = WRK-FTNKEI / 10
               COMPUTE  WRK-FTNKEI = WRK-FTNKEI * 10
      *
               IF   LNK-KOREIFTNCHK-NYUGAIKBN  =  "1"
                 CONTINUE
               ELSE
                 COMPUTE  WRK-FTNKEI = WRK-FTNKEI
                                     + SYU-GRP-SGKMONEY
               END-IF
      *
               EVALUATE  SYU-KOH-HKNNUM(1)
                 WHEN      "972"
                   IF  LNK-KOREIFTNCHK-TOKU75 = 3
                      MOVE  5000   TO   WRK-KINGAKU
                   ELSE
                      MOVE  10000  TO   WRK-KINGAKU
                   END-IF
                   PERFORM  260-ICHIBUFTN-KOH-SET-SEC
                 WHEN      "974"
                   IF  LNK-KOREIFTNCHK-TOKU75 = 3
                      MOVE  10000  TO   WRK-KINGAKU
                   ELSE
                      MOVE  20000  TO   WRK-KINGAKU
                   END-IF
                   PERFORM  260-ICHIBUFTN-KOH-SET-SEC
               END-EVALUATE
      *
           END-IF
      *
           .
       2202-NYUGAI-WAKODO-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスタから一部負担額チェック等（外来）
      *****************************************************************
       230-GAIRAI-SYUKEI-SEC            SECTION.
      *
      *    全国公費を含む保険組合わせの収納があるかチェック
           PERFORM   VARYING   IDX1   FROM  1  BY  1
                 UNTIL   (IDX1                  >    50 )   OR
                         (WRK-HKNCOMBINUM(IDX1) =   ZERO)
      *
              IF   WRK-ZENKOH-ARI(IDX1)  =  1
                 INITIALIZE                            SYUNOU-REC
grpsys           MOVE    LNK-KOREIFTNCHK-HOSPNUM   TO  SYU-HOSPNUM
                 MOVE    LNK-KOREIFTNCHK-NYUGAIKBN TO  SYU-NYUGAIKBN
                 MOVE    LNK-KOREIFTNCHK-PTID      TO  SYU-PTID
                 MOVE    LNK-KOREIFTNCHK-SRYYM     TO  SYU-SRYYMD(1:6)
                 MOVE    "__"                      TO  SYU-SRYYMD(7:2)
                 MOVE    WRK-HKNCOMBINUM(IDX1)     TO  SYU-HKNCOMBINUM
                 MOVE    "tbl_syunou"              TO  MCP-TABLE
                 MOVE    "key41"                   TO  MCP-PATHNAME
                 PERFORM 900-SYUNOU-SELECT-SEC
                 IF      FLG-SYUNOU            NOT =   ZERO
                     MOVE    01                TO  LNK-KOREIFTNCHK-RC
                     GO  TO  230-GAIRAI-SYUKEI-EXT
                 ELSE
                     MOVE   "tbl_syunou"           TO  MCP-TABLE
                     MOVE   "key41"                TO  MCP-PATHNAME
                     PERFORM 910-SYUNOU-READ-SEC
                     IF    FLG-SYUNOU   =    ZERO
                         MOVE    1    TO    FLG-ZENKOH-ARI
                         MOVE   50    TO    IDX1
                     END-IF
                 END-IF
      *
                 MOVE     "tbl_syunou"             TO  MCP-TABLE
                 MOVE     "key41"                  TO  MCP-PATHNAME
                 PERFORM   920-DB-CLOSE-SEC
      *
              END-IF
      *
           END-PERFORM
      *
      *    一部負担額チェック
           PERFORM   VARYING   IDX1   FROM  1  BY  1
                 UNTIL   (IDX1                  >    50 )   OR
                         (WRK-HKNCOMBINUM(IDX1) =   ZERO)
      *
              INITIALIZE                            SYUNOU-REC
grpsys        MOVE    LNK-KOREIFTNCHK-HOSPNUM   TO  SYU-HOSPNUM
              MOVE    LNK-KOREIFTNCHK-NYUGAIKBN TO  SYU-NYUGAIKBN
              MOVE    LNK-KOREIFTNCHK-PTID      TO  SYU-PTID
              MOVE    LNK-KOREIFTNCHK-SRYYM     TO  SYU-SRYYMD(1:6)
              MOVE    "__"                      TO  SYU-SRYYMD(7:2)
              MOVE    WRK-HKNCOMBINUM(IDX1)     TO  SYU-HKNCOMBINUM
              MOVE   "tbl_syunou"               TO  MCP-TABLE
              MOVE   "key41"                    TO  MCP-PATHNAME
              PERFORM 900-SYUNOU-SELECT-SEC
              IF      FLG-SYUNOU            NOT =   ZERO
                  MOVE    01                    TO  LNK-KOREIFTNCHK-RC
                  GO  TO  230-GAIRAI-SYUKEI-EXT
              ELSE
                  MOVE   "tbl_syunou"           TO  MCP-TABLE
                  MOVE   "key41"                TO  MCP-PATHNAME
                  PERFORM 910-SYUNOU-READ-SEC
                  PERFORM  UNTIL
                             (FLG-SYUNOU        =    1)
      *
                   IF  ( SYU-KOH-HKNNUM(1)  =  "972" OR "974" )
                   AND ( SYU-KOH-HKNNUM(2)  =  SPACE          )
                   AND ( SYU-KOH-HKNNUM(3)  =  SPACE          )
                   AND ( SYU-KOH-HKNNUM(4)  =  SPACE          )
                      IF    FLG-CYOKINOMI   =  ZERO
                         MOVE     1     TO    FLG-CYOKINOMI
                         MOVE   WRK-HKNCOMBINUM(IDX1)
                                        TO    WRK-CYOKI-HKNCOMBINUM
                      END-IF
                   ELSE
                         MOVE     9     TO    FLG-CYOKINOMI
                         MOVE   ZERO    TO    WRK-CYOKI-HKNCOMBINUM
                   END-IF
      *
                   EVALUATE   LNK-KOREIFTNCHK-NENREIKBN
                     WHEN       "1"
                       PERFORM     2301-GAIRAI-KOREI-SYUKEI-SEC
                     WHEN       "2"
                       PERFORM     2202-NYUGAI-WAKODO-SYUKEI-SEC
                   END-EVALUATE
      *
                   MOVE   "tbl_syunou"          TO  MCP-TABLE
                   MOVE   "key41"               TO  MCP-PATHNAME
                   PERFORM 910-SYUNOU-READ-SEC
                  END-PERFORM
              END-IF
      *
              MOVE     "tbl_syunou"             TO  MCP-TABLE
              MOVE     "key41"                  TO  MCP-PATHNAME
              PERFORM   920-DB-CLOSE-SEC
      *
           END-PERFORM
      *
           .
       230-GAIRAI-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    一部負担額集計（外来）（高齢者）
      *****************************************************************
       2301-GAIRAI-KOREI-SYUKEI-SEC            SECTION.
      *
           IF  ( SYU-KOH-HKNNUM(1)  =  SPACE  OR "027" )
           AND ( SYU-KOH-HKNNUM(2)  =  SPACE           )
           AND ( SYU-KOH-HKNNUM(3)  =  SPACE           )
           AND ( SYU-KOH-HKNNUM(4)  =  SPACE           )
             IF   SYU-KOH-HKNNUM(1)  =  "027"
               ADD  SYU-KOH1COMPFTN  TO  LNK-KOREIFTNCHK-FTNGAKU
             ELSE
               ADD  SYU-SYUCOMPFTN   TO  LNK-KOREIFTNCHK-FTNGAKU
             END-IF
           ELSE
             ADD    SYU-KOH-FTN      TO  LNK-KOREIFTNCHK-KOHFTNGAKU
      *
             COMPUTE  WRK-FTNKEI = SYU-TOTAL-HKNTEN
                                 * 10
                                 * WRK-KYURATE(IDX1)
                                 / 100
             COMPUTE  WRK-FTNKEI = WRK-FTNKEI +  5
             COMPUTE  WRK-FTNKEI = WRK-FTNKEI / 10
             COMPUTE  WRK-FTNKEI = WRK-FTNKEI * 10
      *
             COMPUTE  WRK-FTNKEI = WRK-FTNKEI
                                 + SYU-GRP-SGKMONEY
      *
             IF   ( SYU-KOH-HKNNUM(1)   =   "012" OR "025" )
             OR   ( SYU-KOH-HKNNUM(2)   =   "012" OR "025" )
             OR   ( SYU-KOH-HKNNUM(3)   =   "012" OR "025" )
             OR   ( SYU-KOH-HKNNUM(4)   =   "012" OR "025" )
               IF  LNK-KOREIFTNCHK-TOKU75 = 1 OR 2
                  MOVE    4000     TO   WRK-KINGAKU
               ELSE
                  MOVE    8000     TO   WRK-KINGAKU
               END-IF
               PERFORM    260-ICHIBUFTN-KOH-SET-SEC
             ELSE
               IF   ( SYU-KOH-HKNNUM(1) =   "972" )
               OR   ( SYU-KOH-HKNNUM(2) =   "972" )
               OR   ( SYU-KOH-HKNNUM(3) =   "972" )
               OR   ( SYU-KOH-HKNNUM(4) =   "972" )
                 IF   LNK-KOREIFTNCHK-TEISYOTOKU  NOT =  SPACE
                   IF  LNK-KOREIFTNCHK-TOKU75 = 1 OR 2
                      MOVE  4000     TO   WRK-KINGAKU
                   ELSE
                      MOVE  8000     TO   WRK-KINGAKU
                   END-IF
                   PERFORM  260-ICHIBUFTN-KOH-SET-SEC
                 ELSE
                   IF  LNK-KOREIFTNCHK-TOKU75 = 1 OR 2
                      MOVE  5000     TO   WRK-KINGAKU
                   ELSE
                      MOVE  10000    TO   WRK-KINGAKU
                   END-IF
                   PERFORM  260-ICHIBUFTN-KOH-SET-SEC
                 END-IF
               ELSE
                 IF    FLG-ZENKOH-ARI  =  ZERO
                   PERFORM  23011-GAIRAI-KOREI-HEN1-SEC
                 ELSE
                   IF  ((SYU-KOH-HKNNUM(1)     NOT = "027") AND
                        (SYU-KOH-HKNNUM(1)(1:1)    = "0"  ))
                   OR  ( SYU-KOH-HKNNUM(2)(1:1)    = "0"   )
                   OR  ( SYU-KOH-HKNNUM(3)(1:1)    = "0"   )
                   OR  ( SYU-KOH-HKNNUM(4)(1:1)    = "0"   )
                   OR  ((LNK-KOREIFTNCHK-PREFNUM  = "13"  ) AND
                        (SYU-KOH-HKNNUM(1)        = "151" ) AND
                        (LNK-KOREIFTNCHK-SRYYM >= "200905"))
                   OR  ((LNK-KOREIFTNCHK-PREFNUM  = "23"  ) AND
                        (SYU-KOH-HKNNUM(1)        = "191" ) AND
                        (LNK-KOREIFTNCHK-SRYYM >= "200910"))
      ***            IF  LNK-KOREIFTNCHK-TOKU75 = 1 OR 2
      ***               MOVE   6000      TO   WRK-KINGAKU
      ***            ELSE
      ***               MOVE   12000     TO   WRK-KINGAKU
      ***            END-IF
      ***            PERFORM   260-ICHIBUFTN-KOH-SET-SEC
                     CONTINUE
                   ELSE
                     IF   ( WRK-KYURATE(IDX1) =  20 OR 30 )
                     AND  ((SYU-KOH-HKNNUM(1) = "974"    ) OR
                           (SYU-KOH-HKNNUM(2) = "974"    ) OR
                           (SYU-KOH-HKNNUM(3) = "974"    ) OR
                           (SYU-KOH-HKNNUM(4) = "974"    ))
                       IF   LNK-KOREIFTNCHK-KEIKASOCHI =  SPACE
                         IF  LNK-KOREIFTNCHK-TOKU75 = 1 OR 2
                            MOVE  10000  TO   WRK-KINGAKU
                         ELSE
                            MOVE  20000  TO   WRK-KINGAKU
                         END-IF
                         PERFORM  260-ICHIBUFTN-KOH-SET-SEC
                       ELSE
                         MOVE     12000  TO   WRK-KINGAKU
                         PERFORM  260-ICHIBUFTN-KOH-SET-SEC
                       END-IF
                     ELSE
      ***              PERFORM    23011-GAIRAI-KOREI-HEN1-SEC
                       CONTINUE
                     END-IF
                   END-IF
                 END-IF
               END-IF
             END-IF
           END-IF
      *
           .
       2301-GAIRAI-KOREI-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    一部負担額編集１（外来）（高齢者）
      *****************************************************************
       23011-GAIRAI-KOREI-HEN1-SEC               SECTION.
      *
           IF  ((LNK-KOREIFTNCHK-PREFNUM   = "42" ) AND
                (SYU-KOH-HKNNUM(1)         = "186"))
                   IF  LNK-KOREIFTNCHK-TOKU75 = 1 OR 2
                      MOVE   6000    TO   WRK-KINGAKU
                   ELSE
                      MOVE   12000   TO   WRK-KINGAKU
                   END-IF
                   PERFORM   270-ICHIBUFTN-GEN-SEC
                   PERFORM   260-ICHIBUFTN-KOH-SET-SEC
           ELSE
            IF   LNK-KOREIFTNCHK-TEISYOTOKU   NOT =  SPACE
                   IF  LNK-KOREIFTNCHK-TOKU75 = 1 OR 2
                      MOVE   4000    TO   WRK-KINGAKU
                   ELSE
                      MOVE   8000    TO   WRK-KINGAKU
                   END-IF
                   PERFORM   270-ICHIBUFTN-GEN-SEC
                   PERFORM   260-ICHIBUFTN-KOH-SET-SEC
            ELSE
             IF   (  WRK-KYURATE(IDX1)          =  20 OR 30  )
             AND  (  LNK-KOREIFTNCHK-KEIKASOCHI =  SPACE     )
               IF   LNK-KOREIFTNCHK-SRYYM  >=  "200610"
                   IF  LNK-KOREIFTNCHK-TOKU75 = 1 OR 2
                      MOVE   22200   TO   WRK-KINGAKU
                   ELSE
                      MOVE   44400   TO   WRK-KINGAKU
                   END-IF
                   PERFORM   270-ICHIBUFTN-GEN-SEC
                   PERFORM   260-ICHIBUFTN-KOH-SET-SEC
               ELSE
                   MOVE      40200   TO   WRK-KINGAKU
                   PERFORM   270-ICHIBUFTN-GEN-SEC
                   PERFORM   260-ICHIBUFTN-KOH-SET-SEC
               END-IF
             ELSE
                   IF  LNK-KOREIFTNCHK-TOKU75 = 1 OR 2
                      MOVE   6000    TO   WRK-KINGAKU
                   ELSE
                      MOVE   12000   TO   WRK-KINGAKU
                   END-IF
                   PERFORM   270-ICHIBUFTN-GEN-SEC
                   PERFORM   260-ICHIBUFTN-KOH-SET-SEC
             END-IF
            END-IF
           END-IF
      *
           .
       23011-GAIRAI-KOREI-HEN1-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険欄の一部負担金額セット（公費含む場合）
      *****************************************************************
       260-ICHIBUFTN-KOH-SET-SEC               SECTION.
      *
           IF  WRK-FTN-KOH-XXX   >   WRK-KINGAKU
              MOVE  WRK-KINGAKU      TO   WRK-FTN-KOH-XXX
           END-IF
           COMPUTE  WRK-FTN-KOH-YYY  =  WRK-KINGAKU - WRK-FTN-KOH-XXX
           IF      WRK-FTNKEI    >   WRK-FTN-KOH-YYY
              ADD   WRK-FTN-KOH-YYY  TO  LNK-KOREIFTNCHK-ICHIBUGAKU
                                         WRK-FTN-KOH-XXX
           ELSE
              ADD   WRK-FTNKEI       TO  LNK-KOREIFTNCHK-ICHIBUGAKU
                                         WRK-FTN-KOH-XXX
           END-IF
      *
           .
       260-ICHIBUFTN-KOH-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    金額の減額
      *****************************************************************
       270-ICHIBUFTN-GEN-SEC               SECTION.
      *
           IF   LNK-KOREIFTNCHK-GENGAKUKBN   NOT =   ZERO
      *
               COMPUTE  WRK-FTNKEI = SYU-TOTAL-HKNTEN
                                   * 10
                                   * WRK-KYURATE(IDX1)
                                   / 100
               EVALUATE   LNK-KOREIFTNCHK-GENGAKUKBN
                 WHEN        1
                   IF  LNK-KOREIFTNCHK-GENGAKU-RITU = 100
                       MOVE   ZERO     TO   WRK-FTNKEI
                   ELSE
                       COMPUTE  WRK-GENGAKU-RITUX =
                                100 - LNK-KOREIFTNCHK-GENGAKU-RITU
                       IF    WRK-FTNKEI  NOT = ZERO
                          COMPUTE  WRK-FTNKEI         =
                                   WRK-FTNKEI         *
                                   WRK-GENGAKU-RITUX  /
                                   100
                       END-IF
                   END-IF
                 WHEN        2
                   CONTINUE
                 WHEN        3
                 WHEN        4
                       MOVE   ZERO     TO   WRK-FTNKEI
               END-EVALUATE
               COMPUTE  WRK-FTNKEI = WRK-FTNKEI +  5
               COMPUTE  WRK-FTNKEI = WRK-FTNKEI / 10
               COMPUTE  WRK-FTNKEI = WRK-FTNKEI * 10
      *
               EVALUATE   LNK-KOREIFTNCHK-GENGAKUKBN
                 WHEN        1
                   IF  LNK-KOREIFTNCHK-GENGAKU-RITU = 100
                       MOVE   ZERO     TO   WRK-KINGAKU
                   ELSE
                       COMPUTE  WRK-GENGAKU-RITUX =
                                100 - LNK-KOREIFTNCHK-GENGAKU-RITU
                       IF    WRK-KINGAKU  NOT = ZERO
                         COMPUTE  WRK-KINGAKU         =
                                 (WRK-KINGAKU         *
                                  WRK-GENGAKU-RITUX   /
                                  100) + 0.5
                       END-IF
                   END-IF
                 WHEN        2
                   CONTINUE
                 WHEN        3
                 WHEN        4
                       MOVE   ZERO     TO   WRK-KINGAKU
               END-EVALUATE
      *
           END-IF
      *
           .
       270-ICHIBUFTN-GEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_ptinf"          TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
           IF      MCP-RC               =   ZERO
               MOVE    "DBFETCH"        TO  MCP-FUNC
               MOVE    "tbl_ptinf"      TO  MCP-TABLE
               MOVE    "key"            TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"      USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
               IF      MCP-RC           =   ZERO
                   MOVE    MCPDATA-REC  TO  PTINF-REC
                   MOVE    ZERO         TO  FLG-PTINF
               ELSE
                   INITIALIZE               PTINF-REC
                   MOVE    1            TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                   PTINF-REC
               MOVE    1                TO  FLG-PTINF
           END-IF
      *
           MOVE     "tbl_ptinf"         TO  MCP-TABLE
           MOVE     "key"               TO  MCP-PATHNAME
           PERFORM   920-DB-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター検索
      *****************************************************************
       900-HKNCOMBI-SELECT-SEC        SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-HKNCOMBI
           ELSE
               MOVE    1                  TO  FLG-HKNCOMBI
           END-IF
      *
           .
       900-HKNCOMBI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスター検索
      *****************************************************************
       900-SYUNOU-SELECT-SEC        SECTION.
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-SYUNOU
           ELSE
               MOVE    1                  TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       910-SYUNOU-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       910-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報読込
      *****************************************************************
       900-PTHKNINF-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
               MOVE    ZERO            TO  FLG-PTHKNINF
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           .
       900-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報検索
      *****************************************************************
       900-PTKOHINF-SELECT-SEC        SECTION.
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-PTKOHINF
           ELSE
               MOVE    1                  TO  FLG-PTKOHINF
           END-IF
      *
           .
       900-PTKOHINF-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報読込
      *****************************************************************
       910-PTKOHINF-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
               MOVE    ZERO            TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           .
       910-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       920-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DB-CLOSE-EXT.
           EXIT.
