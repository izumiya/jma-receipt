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
       PROGRAM-ID.             ORCSFTNJGNCHK.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 患者自己負担金レセプト記載判定
      *  管理者            : 
      *  作成日付    作業者        記述
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    04/11/15  公費月上限額取得修正
      *  01.00.02    NACL-門脇    04/11/30  入院対応
      *  01.00.03    NACL-門脇    05/06/01  長期高額療養レセ記載判定追加
      *  01.00.04    NACL-門脇    05/08/12  滋賀県地方公費対応
      *  01.00.05    NACL-門脇    05/10/04  島根県地方公費対応
      *  03.02.01    NACL-門脇    06/09/05  長期９７４対応
      *  03.03.01    NACL-門脇    06/11/02  減額・免除・支払猶予対応
      *  03.04.01    NACL-門脇    07/02/14  福島県飯館村対応
      *  03.04.02    NACL-門脇    07/03/22  埼玉県地方公費対応
      *                                     （川口・春日部市）
      *  03.04.03    NACL-門脇    07/06/04  長崎市地方公費対応
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  03.05.02    NACL-門脇    07/08/01  所得情報取扱い機能強化対応
      *  03.05.03    NACL-門脇    07/08/01  月途中の所得情報変更対応
      *  04.01.01    NACL-門脇    08/01/18  埼玉県川島町地方公費対応
      *  04.01.02    NACL-門脇    08/01/31  滋賀県４４１追加に伴う修正
      *  04.02.01    NACL-門脇    08/03/19  生保の７９併用時の対応
      *  04.02.02    NACL-門脇    08/04/24  埼玉県志木市地方公費対応
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
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-CHECK           PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-GENMEN          PIC 9(01).
           03  FLG-DAYCHK          PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  SYOTOKU-IDX         PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-HKNMST-JGNGAK  PIC 9(06).
           03  WRK-HKNMST-JGNGAK2 PIC 9(06).
           03  WRK-KOHFTN         PIC 9(10).
           03  WRK-SEIKYU         PIC 9(10).
           03  WRK-SKJ-FTN        PIC 9(10).
           03  WRK-PREFNUM        PIC 9(02).
      *
           03  WRK-TEISYOTOKU     PIC X(01).
           03  WRK-RECEJGNDSPKBN  PIC X(01).
      *
           03  WRK-HKNCOMBINUM-TBL.
               05  WRK-HKNCOMBINUM   PIC 9(04)  OCCURS 50.
      *
      *    共通領域
           COPY    "MCPAREA".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    システム管理
           COPY    "CPSK1001.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    公費月上限額取得サブ
           COPY    "CPORCSJGNGET.INC".
      *
      *    保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    患者所得情報編集サブ
           COPY    "CPORCSSYOTOKU.INC".
      *
           COPY    "MCPDATA.INC".
      ***  COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSFTNJGNCHK.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           ORCSFTNJGNCHKAREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           MOVE    ZERO                TO  LNK-FTNJGNCHK-RC
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    パラメータチェック
           PERFORM   100-PRM-CHK-SEC
           IF       (LNK-FTNJGNCHK-RC   NOT =   ZERO)
               GO   TO   000-PROC-EXT
           END-IF
      *
           IF    LNK-FTNJGNCHK-RC      =   ZERO
               PERFORM   200-MAIN-SEC
           END-IF
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
           INITIALIZE                      PTINF-REC
           MOVE    LNK-FTNJGNCHK-HOSPNUM   TO  PTINF-HOSPNUM
           MOVE    LNK-FTNJGNCHK-PTID      TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    10              TO  LNK-FTNJGNCHK-RC
           END-IF
      *
           IF    LNK-FTNJGNCHK-KOHNUM   = SPACE
               MOVE    10              TO  LNK-FTNJGNCHK-RC
           END-IF
      *
           IF    LNK-FTNJGNCHK-KOHID    =  ZERO
               MOVE    10              TO  LNK-FTNJGNCHK-RC
           END-IF
      *
      *    県番号取得
           INITIALIZE                      SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    LNK-FTNJGNCHK-HOSPNUM
                                       TO  SYS-1001-HOSPNUM
           MOVE    LNK-FTNJGNCHK-SRYYM TO  SYS-1001-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1001-STYUKYMD(7:2)
           MOVE    LNK-FTNJGNCHK-SRYYM TO  SYS-1001-EDYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1001-EDYUKYMD(7:2)
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC               =  ZERO
               MOVE     "tbl_syskanri" TO  MCP-TABLE
               MOVE     "key10"        TO  MCP-PATHNAME
               PERFORM   SYSKANRI-READ-SEC
               MOVE      SYS-1001-PREFNUM
                                       TO  WRK-PREFNUM
           END-IF
      *    システム管理クローズ
           MOVE     "tbl_syskanri"     TO    MCP-TABLE
           MOVE     "key10"            TO    MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
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
      *    保険番号マスタからレセプト表示区分と負担上限額の取得
           PERFORM    210-HKNNUM-GET-SEC
      *
           IF   FLG-HKNNUM     =    ZERO
             IF   (HKN-RECEJGNDSPKBN    =   "1"           )
             OR   (HKN-HKNNUM           =   "972" OR "974"
                                                  OR "012")
             OR   (WRK-RECEJGNDSPKBN    =   "1"           )
      *        患者公費負担マスタから負担上限額の取得
               PERFORM    210-PTKOHFTN-GET-SEC
      *        保険の上限額と収納の負担額を比較判定
               PERFORM    210-HANTEI-SEC
             END-IF
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスタからレセプト表示区分と負担上限額の取得
      *****************************************************************
       210-HKNNUM-GET-SEC       SECTION.
      *
           MOVE        ZERO      TO     FLG-CHECK
           MOVE        ZERO      TO     FLG-OK
           MOVE        SPACE     TO     WRK-TEISYOTOKU
           MOVE        SPACE     TO     WRK-RECEJGNDSPKBN
      *
           IF  (WRK-PREFNUM           =  32            ) AND
               (LNK-FTNJGNCHK-KOHNUM  = "191" OR "291"
                                     OR "190" OR "290" ) AND
               (LNK-FTNJGNCHK-SRYYM  >= "200510"       )
      *
               IF   LNK-FTNJGNCHK-KOHNUM  =  "191"
                     PERFORM    2101-SYOTOKU-HENSYU-SEC
               END-IF
               MOVE     "1"      TO     WRK-RECEJGNDSPKBN
      *
           END-IF
      *
           IF  (WRK-PREFNUM           =  11            ) AND
               (LNK-FTNJGNCHK-KOHNUM  = "483"          ) AND
               (LNK-FTNJGNCHK-SRYYM  >= "200804"       )
      *
               PERFORM    2101-SYOTOKU-HENSYU-SEC
      *
           END-IF
      *
           INITIALIZE                           HKNNUM-REC
           MOVE    LNK-FTNJGNCHK-HOSPNUM  TO    HKN-HOSPNUM
           MOVE    LNK-FTNJGNCHK-KOHNUM   TO    HKN-HKNNUM
           MOVE   "tbl_hknnum"            TO    MCP-TABLE
           MOVE   "key2"                  TO    MCP-PATHNAME
           PERFORM  910-HKNNUM-SELECT-SEC
           IF      FLG-HKNNUM           =       ZERO
               MOVE    "tbl_hknnum"       TO    MCP-TABLE
               MOVE    "key2"             TO    MCP-PATHNAME
               PERFORM  910-HKNNUM-READ-SEC
               PERFORM  UNTIL
                   ( FLG-HKNNUM          =   1   )   OR
                   ((HKN-TEKSTYMD(1:6)
                                  <=   LNK-FTNJGNCHK-SRYYM) AND
                    (LNK-FTNJGNCHK-SRYYM
                                  <=   HKN-TEKEDYMD(1:6)))
                      MOVE   "tbl_hknnum" TO    MCP-TABLE
                      MOVE   "key2"       TO    MCP-PATHNAME
                      PERFORM 910-HKNNUM-READ-SEC
               END-PERFORM
           END-IF
      *
           IF   FLG-HKNNUM     =    ZERO
             IF    LNK-FTNJGNCHK-NYUGAIKBN     =   "1"
               EVALUATE   WRK-TEISYOTOKU
                 WHEN     SPACE
                   MOVE   HKN-HON-NYUMJGNGAK     TO  WRK-HKNMST-JGNGAK
                   IF  (WRK-PREFNUM           =  25            ) AND
                       (LNK-FTNJGNCHK-KOHNUM  = "140" OR "241" OR
                                                "243" OR "244" OR
                                                "245" OR "441" ) AND
                       (LNK-FTNJGNCHK-SRYYM  >= "200508"       )
                      MOVE   HKN-HON-NYUDJGNGAK  TO  WRK-HKNMST-JGNGAK
                   END-IF
                 WHEN     "1"
                 WHEN     "4"
                   MOVE   HKN-TNK-NYUMJGNGAK     TO  WRK-HKNMST-JGNGAK
                 WHEN     "2"
                 WHEN     "3"
                   MOVE   HKN-TSY-NYUMJGNGAK     TO  WRK-HKNMST-JGNGAK
               END-EVALUATE
             ELSE
               EVALUATE   WRK-TEISYOTOKU
                 WHEN     SPACE
                   IF    HKN-HON-GAIMNAIJGNGAK   NOT =    ZERO
                      MOVE HKN-HON-GAIMNAIJGNGAK TO WRK-HKNMST-JGNGAK
                   END-IF
                   IF    HKN-HON-GAIMGAIJGNGAK   NOT =    ZERO
                      MOVE HKN-HON-GAIMGAIJGNGAK TO WRK-HKNMST-JGNGAK
                   END-IF
                   IF  (WRK-PREFNUM              =  42     )
                   AND (LNK-FTNJGNCHK-KOHNUM     = "180"   )
                   AND (LNK-FTNJGNCHK-SRYYM     >= "200704")
                      MOVE HKN-HON-GAIDJGNGAK    TO WRK-HKNMST-JGNGAK2
                   END-IF
                 WHEN     "1"
                 WHEN     "4"
                   IF    HKN-TNK-GAIMNAIJGNGAK   NOT =    ZERO
                      MOVE HKN-TNK-GAIMNAIJGNGAK TO WRK-HKNMST-JGNGAK
                   END-IF
                   IF    HKN-TNK-GAIMGAIJGNGAK   NOT =    ZERO
                      MOVE HKN-TNK-GAIMGAIJGNGAK TO WRK-HKNMST-JGNGAK
                   END-IF
                 WHEN     "2"
                   IF    HKN-TSY-GAIMNAIJGNGAK   NOT =    ZERO
                      MOVE HKN-TSY-GAIMNAIJGNGAK TO WRK-HKNMST-JGNGAK
                   END-IF
                   IF    HKN-TSY-GAIMGAIJGNGAK   NOT =    ZERO
                      MOVE HKN-TSY-GAIMGAIJGNGAK TO WRK-HKNMST-JGNGAK
                   END-IF
               END-EVALUATE
             END-IF
             IF   (WRK-PREFNUM             =  11                     )
             AND  (LNK-FTNJGNCHK-KOHNUM    = "481" OR "482" OR "483" )
             AND  (LNK-FTNJGNCHK-NYUGAIKBN = "1"                     )
             AND  (LNK-FTNJGNCHK-SRYYM    >= "200804"                )
                      MOVE     999999            TO WRK-HKNMST-JGNGAK
             END-IF
           END-IF
      *
      *    保険番号クローズ
           MOVE     "tbl_hknnum" TO    MCP-TABLE
           MOVE     "key2"       TO    MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       210-HKNNUM-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得・低年金情報編集
      *****************************************************************
       2101-SYOTOKU-HENSYU-SEC       SECTION.
      *
           MOVE       1       TO      SYOTOKU-IDX
      *
           IF   LNK-FTNJGNCHK-SRYYM   >=  "200604"
      *
              INITIALIZE                         PTKOHINF-REC
              MOVE    LNK-FTNJGNCHK-HOSPNUM  TO  PTKOH-HOSPNUM
              MOVE    LNK-FTNJGNCHK-PTID     TO  PTKOH-PTID
              MOVE    LNK-FTNJGNCHK-KOHID    TO  PTKOH-KOHID
              MOVE    PTKOHINF-REC           TO  MCPDATA-REC
              MOVE    "DBSELECT"             TO  MCP-FUNC
              MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
              MOVE    "key8"                 TO  MCP-PATHNAME
              CALL    "ORCDBMAIN"            USING  MCPAREA
                                                    MCPDATA-REC
                                                    SPA-AREA
              IF      MCP-RC   =    ZERO
                MOVE    "tbl_ptkohinf"   TO  MCP-TABLE
                MOVE    "key8"           TO  MCP-PATHNAME
                PERFORM  910-PTKOHINF-READ-SEC
                IF       FLG-PTKOHINF    =   ZERO
                  IF   PTKOH-TEKSTYMD(1:6)  >=  LNK-FTNJGNCHK-SRYYM
                      MOVE   PTKOH-TEKSTYMD(7:2)  TO   SYOTOKU-IDX
                  END-IF
                END-IF
              END-IF
              MOVE     "tbl_ptkohinf"    TO   MCP-TABLE
              MOVE     "key8"            TO   MCP-PATHNAME
              PERFORM   910-DB-CLOSE-SEC
      *
           END-IF
      *
           INITIALIZE                       ORCSSYOTOKUAREA
           MOVE  LNK-FTNJGNCHK-HOSPNUM  TO  LNK-SYOTOKU-HOSPNUM
           MOVE  LNK-FTNJGNCHK-PTID     TO  LNK-SYOTOKU-PTID
           MOVE  LNK-FTNJGNCHK-SRYYM    TO  LNK-SYOTOKU-SRYYM
      *
           CALL    "ORCSSYOTOKU"   USING  ORCSSYOTOKUAREA
                                          SPA-AREA
      *
           IF    LNK-SYOTOKU-RC    =   ZERO
                 MOVE   LNK-SYOTOKU-CHIKBN(SYOTOKU-IDX)
                                        TO  WRK-TEISYOTOKU
           END-IF
      *
           .
       2101-SYOTOKU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費負担マスタから負担上限額の取得
      *****************************************************************
       210-PTKOHFTN-GET-SEC       SECTION.
      *
           IF ((WRK-PREFNUM                  =  13            )  AND
               (LNK-FTNJGNCHK-KOHNUM         = "151" OR "451" )  AND
               (LNK-FTNJGNCHK-SRYYM         >= "200310"))
      *
           OR ((WRK-PREFNUM                  =  28     )  AND
               (LNK-FTNJGNCHK-KOHNUM         = "186"   )  AND
               (LNK-FTNJGNCHK-SRYYM         >= "200310"))
      *
           OR ((WRK-PREFNUM                  =  07     )  AND
               (LNK-FTNJGNCHK-KOHNUM         = "281"   )  AND
               (LNK-FTNJGNCHK-SRYYM         >= "200702"))
      *
      *         公費月上限額取得
                INITIALIZE                    LNK-ORCSJGNGET-REC
                MOVE  LNK-FTNJGNCHK-HOSPNUM
                                          TO  LNK-ORCSJGNGET-HOSPNUM
                MOVE  LNK-FTNJGNCHK-NYUGAIKBN
                                          TO  LNK-ORCSJGNGET-NYUGAIKBN
                MOVE  LNK-FTNJGNCHK-PTID  TO  LNK-ORCSJGNGET-PTID
                MOVE  LNK-FTNJGNCHK-KOHNUM
                                          TO  LNK-ORCSJGNGET-KOHHKNNUM
                MOVE  LNK-FTNJGNCHK-KOHID TO  LNK-ORCSJGNGET-KOHID
                MOVE  LNK-FTNJGNCHK-SRYYM TO  LNK-ORCSJGNGET-SRYYM
      *
                CALL    "ORCSJGNGET"      USING
                                          LNK-ORCSJGNGET-REC
                                          SPA-AREA
      *
                IF    LNK-ORCSJGNGET-RC   =   ZERO
                 IF    LNK-FTNJGNCHK-NYUGAIKBN     =   "1"
                   MOVE    LNK-ORCSJGNGET-NYUJGNGAK
                                          TO  WRK-HKNMST-JGNGAK
                 ELSE
                   MOVE    LNK-ORCSJGNGET-GAIJGNGAK
                                          TO  WRK-HKNMST-JGNGAK
                 END-IF
                END-IF
      *
           END-IF
           .
       210-PTKOHFTN-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険の上限額と収納の負担額を比較判定
      *****************************************************************
       210-HANTEI-SEC            SECTION.
      *
      *    収納マスタ読み込み対象の保険組合せ番号取得
           INITIALIZE                          HKNCOMBI-REC
           MOVE    LNK-FTNJGNCHK-HOSPNUM   TO  COMB-HOSPNUM
           MOVE    LNK-FTNJGNCHK-PTID      TO  COMB-PTID
           MOVE    LNK-FTNJGNCHK-KOHID     TO  COMB-KOHID(1)
           MOVE    LNK-FTNJGNCHK-KOHID     TO  COMB-KOHID(2)
           MOVE    LNK-FTNJGNCHK-KOHID     TO  COMB-KOHID(3)
           MOVE    LNK-FTNJGNCHK-KOHID     TO  COMB-KOHID(4)
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           PERFORM 910-HKNCOMBI-SELECT-SEC
           IF      FLG-HKNCOMBI            NOT =   ZERO
               MOVE    01                  TO  LNK-FTNJGNCHK-RC
               GO  TO  210-HANTEI-EXT
           ELSE
               MOVE   "tbl_hkncombi"       TO  MCP-TABLE
               MOVE   "key4"               TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
      *
               MOVE      ZERO      TO      IDX
               PERFORM
                 UNTIL   (IDX              >=    50 )
                   OR    (FLG-HKNCOMBI  NOT =   ZERO)
      *
                 IF    COMB-DELKBN          =   SPACE
                 AND   COMB-TEKSTYMD(1:6)  <=   LNK-FTNJGNCHK-SRYYM
                 AND   LNK-FTNJGNCHK-SRYYM <=   COMB-TEKEDYMD(1:6)
      *
                   ADD     1       TO      IDX
                   MOVE    COMB-HKNCOMBINUM  TO
                                        WRK-HKNCOMBINUM(IDX)
      *
                   IF    WRK-RECEJGNDSPKBN    =   "1"
                   AND   FLG-CHECK            =   ZERO
      *
                     IF   COMB-HKNNUM     NOT =  SPACE
                       IF   COMB-KOH1HKNNUM   =  "027"
                          IF  COMB-KOH1PAYKBN  =  "01"
                             CONTINUE
                          ELSE
                             MOVE    1    TO   FLG-CHECK
                          END-IF
                       ELSE
                          EVALUATE  LNK-FTNJGNCHK-NYUGAIKBN
                            WHEN       "1"
                              IF  COMB-HOJOKBN  =  "1" OR "5" OR "9"
                                 CONTINUE
                              ELSE
                                 MOVE    1    TO   FLG-CHECK
                              END-IF
                            WHEN       "2"
                              IF  COMB-HOJOKBN  =  "1" OR "4" OR "9"
                                 CONTINUE
                              ELSE
                                 MOVE    1    TO   FLG-CHECK
                              END-IF
                          END-EVALUATE
                       END-IF
      *
                       IF    FLG-CHECK                =   ZERO
                       AND   LNK-FTNJGNCHK-KOHNUM     =  "191"
                       AND   LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
                       AND   WRK-TEISYOTOKU           =   SPACE
                       AND   FLG-OK                   =   ZERO
                       AND   LNK-FTNJGNCHK-SRYYM      <  "200610"
      *
                         MOVE    1           TO  FLG-OK
      *
                         INITIALIZE              ORCSAGECHKAREA
                         MOVE    LNK-FTNJGNCHK-HOSPNUM
                                             TO  AGECHK-HOSPNUM
                         MOVE    LNK-FTNJGNCHK-PTID
                                             TO  AGECHK-PTID
                         MOVE    LNK-FTNJGNCHK-SRYYM
                                             TO  AGECHK-SRYYMD(1:6)
                         MOVE    "01"        TO  AGECHK-SRYYMD(7:2)
                         MOVE    PTINF-BIRTHDAY
                                             TO  AGECHK-BIRTHDAY
                         CALL    "ORCSAGECHK"    USING
                                                 ORCSAGECHKAREA
                                                 SPA-AREA
                         IF      AGECHK-RC       =   ZERO
                           IF  ( AGECHK-NENREI   >=  70      )
                           OR  ( AGECHK-NENREI   >=  65  AND
                                 COMB-KOH1HKNNUM  = "027"    )
                              MOVE    01      TO  LNK-FTNJGNCHK-RC
                              MOVE     1      TO  FLG-CHECK
                           END-IF
                         END-IF
                       END-IF
                     END-IF
      *
                   END-IF
      *
                 END-IF
      *
                 MOVE   "tbl_hkncombi"      TO  MCP-TABLE
                 MOVE   "key4"              TO  MCP-PATHNAME
                 PERFORM 910-HKNCOMBI-READ-SEC
      *
               END-PERFORM
           END-IF
      *    保険組合せクローズ
           MOVE     "tbl_hkncombi"      TO  MCP-TABLE
           MOVE     "key4"              TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *    収納マスタから該当公費の患者負担額チェック
           IF   FLG-CHECK  =  ZERO
              PERFORM      2101-SYUKEI-SEC
           END-IF
      *
           .
       210-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    該当公費の患者負担額集計
      *****************************************************************
       2101-SYUKEI-SEC            SECTION.
      *
           INITIALIZE                          SYUNOU-REC
           MOVE    LNK-FTNJGNCHK-HOSPNUM   TO  SYU-HOSPNUM
           MOVE    LNK-FTNJGNCHK-PTID      TO  SYU-PTID
           MOVE    LNK-FTNJGNCHK-NYUGAIKBN TO  SYU-NYUGAIKBN
           MOVE    LNK-FTNJGNCHK-SRYYM     TO  SYU-SRYYMD(1:6)
           MOVE    "__"                    TO  SYU-SRYYMD(7:2)
           MOVE    LNK-FTNJGNCHK-KOHNUM    TO  SYU-KOH-HKNNUM(1)
           MOVE    LNK-FTNJGNCHK-KOHNUM    TO  SYU-KOH-HKNNUM(2)
           MOVE    LNK-FTNJGNCHK-KOHNUM    TO  SYU-KOH-HKNNUM(3)
           MOVE    LNK-FTNJGNCHK-KOHNUM    TO  SYU-KOH-HKNNUM(4)
           IF      LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
              MOVE    "tbl_syunou"         TO  MCP-TABLE
              MOVE    "key33"              TO  MCP-PATHNAME
           ELSE
              MOVE    "tbl_syunou"         TO  MCP-TABLE
              MOVE    "key25"              TO  MCP-PATHNAME
           END-IF
           PERFORM 910-SYUNOU-SELECT-SEC
           IF      FLG-SYUNOU              NOT =   ZERO
               MOVE    01                  TO  LNK-FTNJGNCHK-RC
               GO  TO  2101-SYUKEI-EXT
           ELSE
               IF      LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
                  MOVE    "tbl_syunou"     TO  MCP-TABLE
                  MOVE    "key33"          TO  MCP-PATHNAME
               ELSE
                  MOVE    "tbl_syunou"     TO  MCP-TABLE
                  MOVE    "key25"          TO  MCP-PATHNAME
               END-IF
               PERFORM 910-SYUNOU-READ-SEC
               IF   FLG-SYUNOU             =   ZERO
      *            患者負担金の集計
                   MOVE      ZERO          TO      WRK-KOHFTN
                   MOVE      ZERO          TO      WRK-SEIKYU
                   MOVE      ZERO          TO      FLG-GENMEN
                   MOVE      ZERO          TO      FLG-DAYCHK
                   MOVE      ZERO          TO      WRK-SKJ-FTN
                   PERFORM   UNTIL    FLG-SYUNOU  =   1
                       MOVE     ZERO       TO      FLG-ARI
                       PERFORM   VARYING   IDX1   FROM  1  BY  1
                             UNTIL  (IDX1                  >  50 ) OR
                                    (WRK-HKNCOMBINUM(IDX1) = ZERO) OR
                                    (FLG-ARI               =   1 )
                          IF   SYU-HKNCOMBINUM  =  WRK-HKNCOMBINUM(IDX1)
                             MOVE    1    TO    FLG-ARI
                             EVALUATE   LNK-FTNJGNCHK-KOHNUM
                               WHEN   SYU-KOH-HKNNUM(1)
                                   ADD    SYU-KOH-HKNFTNMONEY(1)
                                                       TO   WRK-KOHFTN
                               WHEN   SYU-KOH-HKNNUM(2)
                                   ADD    SYU-KOH-HKNFTNMONEY(2)
                                                       TO   WRK-KOHFTN
                               WHEN   SYU-KOH-HKNNUM(3)
                                   ADD    SYU-KOH-HKNFTNMONEY(3)
                                                       TO   WRK-KOHFTN
                               WHEN   SYU-KOH-HKNNUM(4)
                                   ADD    SYU-KOH-HKNFTNMONEY(4)
                                                       TO   WRK-KOHFTN
                             END-EVALUATE
                             ADD    SYU-TOTAL-HKNTEN   TO   WRK-SEIKYU
                             IF     SYU-YKZKENNUM  NOT = ZERO
                                MOVE    1    TO   FLG-GENMEN
                             END-IF
                             IF  (WRK-PREFNUM              =  42     )
                             AND (LNK-FTNJGNCHK-NYUGAIKBN  = "2"     )
                             AND (LNK-FTNJGNCHK-KOHNUM     = "180"   )
                             AND (LNK-FTNJGNCHK-SRYYM     >= "200704")
                               EVALUATE   LNK-FTNJGNCHK-KOHNUM
                                 WHEN   SYU-KOH-HKNNUM(1)
                                   IF   SYU-KOH-HKNFTNMONEY(1)
                                                >= WRK-HKNMST-JGNGAK2
                                        MOVE   1    TO   FLG-DAYCHK
                                   END-IF
                                 WHEN   SYU-KOH-HKNNUM(2)
                                   IF   SYU-KOH-HKNFTNMONEY(2)
                                                >= WRK-HKNMST-JGNGAK2
                                        MOVE   1    TO   FLG-DAYCHK
                                   END-IF
                                 WHEN   SYU-KOH-HKNNUM(3)
                                   IF   SYU-KOH-HKNFTNMONEY(3)
                                                >= WRK-HKNMST-JGNGAK2
                                        MOVE   1    TO   FLG-DAYCHK
                                   END-IF
                                 WHEN   SYU-KOH-HKNNUM(4)
                                   IF   SYU-KOH-HKNFTNMONEY(4)
                                                >= WRK-HKNMST-JGNGAK2
                                        MOVE   1    TO   FLG-DAYCHK
                                   END-IF
                               END-EVALUATE
                             END-IF
                             IF  (LNK-FTNJGNCHK-NYUGAIKBN  = "1"     )
                             AND (LNK-FTNJGNCHK-KOHNUM     = "012"   )
                             AND (LNK-FTNJGNCHK-SRYYM     >= "200804")
                               IF  (SYU-SYUHKNNUM     =   SPACE)
                               AND (SYU-KOH-HKNNUM(1) =  "079" )
                               AND (SYU-KOH-HKNNUM(2) =  "012" )
                                 ADD  SYU-SKJ-FTN-RECE(1)
                                                   TO  WRK-SKJ-FTN
                               END-IF
                             END-IF
                          END-IF
                       END-PERFORM
                       IF      LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
                          MOVE    "tbl_syunou"     TO  MCP-TABLE
                          MOVE    "key33"          TO  MCP-PATHNAME
                       ELSE
                          MOVE    "tbl_syunou"     TO  MCP-TABLE
                          MOVE    "key25"          TO  MCP-PATHNAME
                       END-IF
                       PERFORM  910-SYUNOU-READ-SEC
                   END-PERFORM
      *
                   IF  (WRK-PREFNUM              =  42     )
                   AND (LNK-FTNJGNCHK-NYUGAIKBN  = "2"     )
                   AND (LNK-FTNJGNCHK-KOHNUM     = "180"   )
                   AND (LNK-FTNJGNCHK-SRYYM     >= "200704")
                     IF   ( WRK-HKNMST-JGNGAK   >   WRK-KOHFTN )
                     AND  ( FLG-DAYCHK          =   ZERO       )
                           MOVE   01        TO  LNK-FTNJGNCHK-RC
                     END-IF
                   ELSE
                     IF ((WRK-PREFNUM            =  11     ) AND
                         (LNK-FTNJGNCHK-KOHNUM   =
                           "181" OR "182" OR "183" OR "281") AND
                         (LNK-FTNJGNCHK-SRYYM   >= "200704"))
                     OR ((WRK-PREFNUM            =  11     ) AND
                         (LNK-FTNJGNCHK-KOHNUM   = "381"   ) AND
                         (LNK-FTNJGNCHK-SRYYM   >= "200801"))
                     OR ((WRK-PREFNUM            =  11     ) AND
                         (LNK-FTNJGNCHK-KOHNUM   = "481" OR
                                                   "482"   ) AND
                         (LNK-FTNJGNCHK-NYUGAIKBN = "2"    ) AND
                         (LNK-FTNJGNCHK-SRYYM   >= "200804"))
                     OR ((WRK-PREFNUM            =  11     ) AND
                         (LNK-FTNJGNCHK-KOHNUM   = "483"   ) AND
                         (WRK-TEISYOTOKU     NOT =  SPACE  ) AND
                         (LNK-FTNJGNCHK-NYUGAIKBN = "2"    ) AND
                         (LNK-FTNJGNCHK-SRYYM   >= "200804"))
                       IF   WRK-KOHFTN   NOT = ZERO
                               MOVE   01        TO  LNK-FTNJGNCHK-RC
                       END-IF
                     ELSE
                       EVALUATE   LNK-FTNJGNCHK-KOHNUM
                         WHEN       "012"
                           IF   WRK-SEIKYU            =   ZERO
                               MOVE   01        TO  LNK-FTNJGNCHK-RC
                           END-IF
                           IF   WRK-SKJ-FTN  >=  14880
                               MOVE   01        TO  LNK-FTNJGNCHK-RC
                           END-IF
                         WHEN       "972"
                         WHEN       "974"
                           IF   FLG-GENMEN   =   1
                             CONTINUE
                           ELSE
                             IF   WRK-HKNMST-JGNGAK   >   WRK-KOHFTN
                               MOVE   01        TO  LNK-FTNJGNCHK-RC
                             END-IF
                           END-IF
                         WHEN       OTHER
                           IF     WRK-HKNMST-JGNGAK   >   WRK-KOHFTN
                               MOVE   01        TO  LNK-FTNJGNCHK-RC
                           END-IF
                       END-EVALUATE
                     END-IF
                   END-IF
               END-IF
           END-IF
      *    収納クローズ
           IF      LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
               MOVE    "tbl_syunou"     TO  MCP-TABLE
               MOVE    "key33"          TO  MCP-PATHNAME
           ELSE
               MOVE    "tbl_syunou"     TO  MCP-TABLE
               MOVE    "key25"          TO  MCP-PATHNAME
           END-IF
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       2101-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスター読込
      *****************************************************************
       SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYS-1001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       SYSKANRI-READ-EXT.
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
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター検索
      *****************************************************************
       910-HKNCOMBI-SELECT-SEC        SECTION.
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
       910-HKNCOMBI-SELECT-EXT.
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
       910-SYUNOU-SELECT-SEC        SECTION.
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
       910-SYUNOU-SELECT-EXT.
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
      *    保険番号マスター検索
      *****************************************************************
       910-HKNNUM-SELECT-SEC        SECTION.
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       910-HKNNUM-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスター読込
      *****************************************************************
       910-HKNNUM-READ-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       910-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報読込
      *****************************************************************
       910-PTKOHINF-READ-SEC           SECTION.
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
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
