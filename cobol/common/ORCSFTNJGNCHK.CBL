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
      *  01.00.03    NACL-門脇    05/08/12  滋賀県地方公費対応
      *  01.00.04    NACL-門脇    05/10/25  島根県地方公費対応
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
           03  FLG-TSYRRK          PIC 9(01).
           03  FLG-TNKRRK          PIC 9(01).
           03  FLG-CHECK           PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-ARI             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-HKNMST-JGNGAK  PIC 9(06).
           03  WRK-KOHFTN         PIC 9(10).
           03  WRK-PREFNUM        PIC 9(02).
      *
           03  WRK-TEISYOTOKU-YMD.
               05  WRK-TEISYOTOKU-YM PIC 9(06).
               05  WRK-TEISYOTOKU-DD PIC 9(02).
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
      *    低所得
       01  TSYRRK-REC.
           COPY    "CPTSYRRK.INC".
      *
      *    年金
       01  TNKRRK-REC.
           COPY    "CPTNKRRK.INC".
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
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSFTNJGNCHK.INC".
      *
       PROCEDURE                    DIVISION    USING
           ORCSFTNJGNCHKAREA.
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
           MOVE    LNK-FTNJGNCHK-HOSPID    TO  PTINF-HOSPID
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
           MOVE    LNK-FTNJGNCHK-SRYYM TO  ORC-DBYMD(1:6)
           MOVE    "01"                TO  ORC-DBYMD(7:2)
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"    USING    MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC               =  ZERO
               MOVE     "SYSKANRI-KEY" TO  ORC-DBPATH
               PERFORM   SYSKANRI-READ-SEC
               MOVE      SYS-1001-PREFNUM
                                       TO  WRK-PREFNUM
           END-IF
      *    システム管理クローズ
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
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
             IF    HKN-RECEJGNDSPKBN    =   "1"
             OR    WRK-RECEJGNDSPKBN    =   "1"
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
           INITIALIZE                           HKNNUM-REC
           MOVE    LNK-FTNJGNCHK-HOSPID TO      HKN-HOSPID
           MOVE    LNK-FTNJGNCHK-KOHNUM TO      HKN-HKNNUM
           MOVE    "HKNNUM-KEY2"        TO      ORC-DBPATH
           PERFORM  910-HKNNUM-SELECT-SEC
           IF      FLG-HKNNUM           =       ZERO
               MOVE    "HKNNUM-KEY2"    TO      ORC-DBPATH
               PERFORM  910-HKNNUM-READ-SEC
               PERFORM  UNTIL
                   ( FLG-HKNNUM          =   1   )   OR
                   ((HKN-TEKSTYMD(1:6)
                                  <=   LNK-FTNJGNCHK-SRYYM) AND
                    (LNK-FTNJGNCHK-SRYYM
                                  <=   HKN-TEKEDYMD(1:6)))
                      PERFORM   910-HKNNUM-READ-SEC
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
                                                "245"          ) AND
                       (LNK-FTNJGNCHK-SRYYM  >= "200508"       )
                      MOVE   HKN-HON-NYUDJGNGAK  TO  WRK-HKNMST-JGNGAK
                   END-IF
                 WHEN     "1"
                   MOVE   HKN-TNK-NYUMJGNGAK     TO  WRK-HKNMST-JGNGAK
                 WHEN     "2"
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
                 WHEN     "1"
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
           END-IF
      *
      *    保険番号クローズ
           MOVE    "HKNNUM-KEY2"             TO  ORC-DBPATH
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
           MOVE    LNK-FTNJGNCHK-SRYYM   TO  WRK-TEISYOTOKU-YM
           MOVE    01                    TO  WRK-TEISYOTOKU-DD
      *
      *    年金履歴より
           INITIALIZE                        TNKRRK-REC
           MOVE    LNK-FTNJGNCHK-HOSPID  TO  TNKRRK-HOSPID
           MOVE    LNK-FTNJGNCHK-PTID    TO  TNKRRK-PTID
           MOVE    TNKRRK-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"            TO  MCP-FUNC
           MOVE    "TNKRRK-KEY3"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"           USING     MCPAREA
                                                   ORCMCPAREA
                                                   MCPDATA-REC
           IF      MCP-RC              =   ZERO
              MOVE    "TNKRRK-KEY3"      TO  ORC-DBPATH
              PERFORM 910-TNKRRK-READ-SEC
              PERFORM  UNTIL
                   ( FLG-TNKRRK          =   1                 ) OR
                   ( TNKRRK-STYMD       <=  WRK-TEISYOTOKU-YMD AND
                     WRK-TEISYOTOKU-YMD <=  TNKRRK-EDYMD       )
      *
                 MOVE    "TNKRRK-KEY3"   TO  ORC-DBPATH
                 PERFORM 910-TNKRRK-READ-SEC
      *
              END-PERFORM
           END-IF
      *
           MOVE      "TNKRRK-KEY3"       TO  ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
      *
      *    低所得より
           INITIALIZE                        TSYRRK-REC
           MOVE    LNK-FTNJGNCHK-HOSPID  TO  TSYRRK-HOSPID
           MOVE    LNK-FTNJGNCHK-PTID    TO  TSYRRK-PTID
           MOVE    TSYRRK-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"            TO  MCP-FUNC
           MOVE    "TSYRRK-KEY3"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"           USING     MCPAREA
                                                   ORCMCPAREA
                                                   MCPDATA-REC
           IF      MCP-RC              =   ZERO
              MOVE    "TSYRRK-KEY3"      TO  ORC-DBPATH
              PERFORM 910-TSYRRK-READ-SEC
              PERFORM  UNTIL
                  (  FLG-TSYRRK          =   1                 ) OR
                  (  TSYRRK-NINSTYMD    <=  WRK-TEISYOTOKU-YMD AND
                     WRK-TEISYOTOKU-YMD <=  TSYRRK-NINEDYMD    )
      *
                 MOVE    "TSYRRK-KEY3"   TO  ORC-DBPATH
                 PERFORM 910-TSYRRK-READ-SEC
      *
              END-PERFORM
           END-IF
      *
           MOVE      "TSYRRK-KEY3"       TO  ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
      *
           EVALUATE    FLG-TSYRRK    ALSO     FLG-TNKRRK
               WHEN    1             ALSO     1
                     MOVE     SPACE   TO       WRK-TEISYOTOKU
               WHEN    0             ALSO     1
                     MOVE     "2"     TO       WRK-TEISYOTOKU
               WHEN    0             ALSO     0
               WHEN    1             ALSO     0
                     MOVE     "1"     TO       WRK-TEISYOTOKU
           END-EVALUATE
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
               (LNK-FTNJGNCHK-SRYYM         >= "200310")           )
      *
           OR ((WRK-PREFNUM                  =  28     )  AND
               (LNK-FTNJGNCHK-KOHNUM         = "186"   )  AND
               (LNK-FTNJGNCHK-SRYYM         >= "200310"))
      *
      *         公費月上限額取得
                INITIALIZE                    LNK-ORCSJGNGET-REC
                MOVE  LNK-FTNJGNCHK-HOSPID
                                          TO  LNK-ORCSJGNGET-HOSPID
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
           MOVE    LNK-FTNJGNCHK-HOSPID    TO  COMB-HOSPID
           MOVE    LNK-FTNJGNCHK-PTID      TO  COMB-PTID
           MOVE    LNK-FTNJGNCHK-KOHID     TO  COMB-KOHID(1)
           MOVE    LNK-FTNJGNCHK-KOHID     TO  COMB-KOHID(2)
           MOVE    LNK-FTNJGNCHK-KOHID     TO  COMB-KOHID(3)
           MOVE    LNK-FTNJGNCHK-KOHID     TO  COMB-KOHID(4)
           MOVE    "HKNCOMBI-KEY4"         TO  ORC-DBPATH
           PERFORM 910-HKNCOMBI-SELECT-SEC
           IF      FLG-HKNCOMBI            NOT =   ZERO
               MOVE    01                  TO  LNK-FTNJGNCHK-RC
               GO  TO  210-HANTEI-EXT
           ELSE
               MOVE    "HKNCOMBI-KEY4"      TO  ORC-DBPATH
               PERFORM 910-HKNCOMBI-READ-SEC
      *
               MOVE      ZERO      TO      IDX
               PERFORM
                 UNTIL   (IDX              >=    50 )
                   OR    (FLG-HKNCOMBI  NOT =   ZERO)
      *
                 IF    COMB-DELKBN          =   SPACE
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
      *
                         MOVE    1           TO  FLG-OK
      *
                         INITIALIZE              ORCSAGECHKAREA
                         MOVE    LNK-FTNJGNCHK-HOSPID
                                             TO  AGECHK-HOSPID
                         MOVE    LNK-FTNJGNCHK-PTID
                                             TO  AGECHK-PTID
                         MOVE    LNK-FTNJGNCHK-SRYYM
                                             TO  AGECHK-SRYYMD(1:6)
                         MOVE    "01"        TO  AGECHK-SRYYMD(7:2)
                         MOVE    PTINF-BIRTHDAY
                                             TO  AGECHK-BIRTHDAY
                         CALL    "ORCSAGECHK"    USING
                                                 ORCSAGECHKAREA
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
                 MOVE    "HKNCOMBI-KEY4"      TO  ORC-DBPATH
                 PERFORM 910-HKNCOMBI-READ-SEC
      *
               END-PERFORM
           END-IF
      *    保険組合せクローズ
           MOVE    "HKNCOMBI-KEY4"    TO  ORC-DBPATH
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
           MOVE    LNK-FTNJGNCHK-HOSPID    TO  SYU-HOSPID
           MOVE    LNK-FTNJGNCHK-PTID      TO  SYU-PTID
           MOVE    LNK-FTNJGNCHK-NYUGAIKBN TO  SYU-NYUGAIKBN
           MOVE    LNK-FTNJGNCHK-SRYYM     TO  SYU-SRYYMD(1:6)
           MOVE    "__"                    TO  SYU-SRYYMD(7:2)
           MOVE    LNK-FTNJGNCHK-KOHNUM    TO  SYU-KOH-HKNNUM(1)
           MOVE    LNK-FTNJGNCHK-KOHNUM    TO  SYU-KOH-HKNNUM(2)
           MOVE    LNK-FTNJGNCHK-KOHNUM    TO  SYU-KOH-HKNNUM(3)
           MOVE    LNK-FTNJGNCHK-KOHNUM    TO  SYU-KOH-HKNNUM(4)
           IF      LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
              MOVE    "SYUNOU-KEY33"       TO  ORC-DBPATH
           ELSE
              MOVE    "SYUNOU-KEY25"       TO  ORC-DBPATH
           END-IF
           PERFORM 910-SYUNOU-SELECT-SEC
           IF      FLG-SYUNOU              NOT =   ZERO
               MOVE    01                  TO  LNK-FTNJGNCHK-RC
               GO  TO  2101-SYUKEI-EXT
           ELSE
               IF      LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
                  MOVE    "SYUNOU-KEY33"   TO  ORC-DBPATH
               ELSE
                  MOVE    "SYUNOU-KEY25"   TO  ORC-DBPATH
               END-IF
               PERFORM 910-SYUNOU-READ-SEC
               IF   FLG-SYUNOU             =   ZERO
      *            患者負担金の集計
                   MOVE      ZERO          TO      WRK-KOHFTN
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
                          END-IF
                       END-PERFORM
                       IF      LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
                          MOVE  "SYUNOU-KEY33"  TO  ORC-DBPATH
                       ELSE
                          MOVE  "SYUNOU-KEY25"  TO  ORC-DBPATH
                       END-IF
                       PERFORM  910-SYUNOU-READ-SEC
                   END-PERFORM
                   IF   WRK-HKNMST-JGNGAK       >   WRK-KOHFTN
                        MOVE   01               TO  LNK-FTNJGNCHK-RC
                   END-IF
               END-IF
           END-IF
      *    収納クローズ
           IF      LNK-FTNJGNCHK-NYUGAIKBN  =  "1"
               MOVE  "SYUNOU-KEY33"  TO  ORC-DBPATH
           ELSE
               MOVE  "SYUNOU-KEY25"  TO  ORC-DBPATH
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
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
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
           MOVE    "PTINF-KEY"          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC               =   ZERO
               MOVE    "DBFETCH"        TO  MCP-FUNC
               MOVE    "PTINF-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"      USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
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
           MOVE    "PTINF-KEY"          TO  ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得マスタ読み込み
      *****************************************************************
       910-TSYRRK-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TSYRRK-REC
               MOVE    ZERO            TO  FLG-TSYRRK
           ELSE
               INITIALIZE                  TSYRRK-REC
               MOVE    1               TO  FLG-TSYRRK
           END-IF
      *
           .
       910-TSYRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    年金マスタ読み込み
      *****************************************************************
       910-TNKRRK-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TNKRRK-REC
               MOVE    ZERO            TO  FLG-TNKRRK
           ELSE
               INITIALIZE                  TNKRRK-REC
               MOVE    1               TO  FLG-TNKRRK
           END-IF
      *
           .
       910-TNKRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター検索
      *****************************************************************
       910-HKNCOMBI-SELECT-SEC        SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
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
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
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
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
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
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
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
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
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
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
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
      *    ＤＢクローズ処理
      *****************************************************************
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
