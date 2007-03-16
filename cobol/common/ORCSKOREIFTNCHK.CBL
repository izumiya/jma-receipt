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
           03  FLG-KEISAN            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                  PIC 9(04).
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
           03  WRK-FTN-KOH-XX2       PIC 9(09).
           03  WRK-FTN-KOH-YYY       PIC 9(09).
           03  WRK-FTN-KOH-YY2       PIC 9(09).
           03  WRK-GENGAKU-RITUX     PIC 9(03).
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
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
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
           COPY    "CPORCSKOREIFTNCHK.INC".
      *
       PROCEDURE                    DIVISION    USING
           ORCSKOREIFTNCHKAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           MOVE    ZERO                TO  LNK-KOREIFTNCHK-RC
                                           LNK-KOREIFTNCHK-ICHIBUGAKU
                                           LNK-KOREIFTNCHK-CHIHOUGAKU
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
           MOVE    LNK-KOREIFTNCHK-HOSPID  TO  PTINF-HOSPID
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
           MOVE    ZERO                    TO  IDX1
      *    対象の保険組合せ番号取得
           INITIALIZE                          HKNCOMBI-REC
           MOVE    LNK-KOREIFTNCHK-HOSPID  TO  COMB-HOSPID
           MOVE    LNK-KOREIFTNCHK-PTID    TO  COMB-PTID
           MOVE    LNK-KOREIFTNCHK-HKNID   TO  COMB-HKNID
           MOVE    "HKNCOMBI-KEY5"         TO  ORC-DBPATH
           PERFORM 900-HKNCOMBI-SELECT-SEC
           IF      FLG-HKNCOMBI            NOT =   ZERO
               MOVE    01                  TO  LNK-KOREIFTNCHK-RC
               GO  TO  200-MAIN-EXT
           ELSE
               MOVE    "HKNCOMBI-KEY5"     TO  ORC-DBPATH
               PERFORM 910-HKNCOMBI-READ-SEC
               PERFORM
                    UNTIL   (IDX1             >=    50 )   OR
                            (FLG-HKNCOMBI  NOT =   ZERO)
      ****         IF  ( COMB-KOH1HKNNUM  =  SPACE  OR "027" )
      ****         AND ( COMB-KOH2HKNNUM  =  SPACE           )
      ****         AND ( COMB-KOH3HKNNUM  =  SPACE           )
      ****         AND ( COMB-KOH4HKNNUM  =  SPACE           )
      ****            CONTINUE
      ****         ELSE
                      ADD     1     TO     IDX1
                      MOVE    COMB-HKNCOMBINUM  TO
                                        WRK-HKNCOMBINUM(IDX1)
                      IF   COMB-KOH1HKNNUM =  "027"
                        IF ((COMB-KOH2HKNNUM(1:1)  =  "0" OR "9") AND
                            (COMB-KOH2HKNNUM(1:2) NOT = "99"    ))
                        OR ((COMB-KOH3HKNNUM(1:1)  =  "0" OR "9") AND
                            (COMB-KOH3HKNNUM(1:2) NOT = "99"    ))
                        OR ((COMB-KOH4HKNNUM(1:1)  =  "0" OR "9") AND
                            (COMB-KOH4HKNNUM(1:2) NOT = "99"    ))
                           MOVE     1    TO   WRK-ZENKOH-ARI(IDX1)
                        END-IF
                      ELSE
                        IF ((COMB-KOH1HKNNUM(1:1)  =  "0" OR "9") AND
                            (COMB-KOH1HKNNUM(1:2) NOT = "99"    ))
                        OR ((COMB-KOH2HKNNUM(1:1)  =  "0" OR "9") AND
                            (COMB-KOH2HKNNUM(1:2) NOT = "99"    ))
                        OR ((COMB-KOH3HKNNUM(1:1)  =  "0" OR "9") AND
                            (COMB-KOH3HKNNUM(1:2) NOT = "99"    ))
                        OR ((COMB-KOH4HKNNUM(1:1)  =  "0" OR "9") AND
                            (COMB-KOH4HKNNUM(1:2) NOT = "99"    ))
                           MOVE     1    TO   WRK-ZENKOH-ARI(IDX1)
                        END-IF
                      END-IF
                      PERFORM   210-KYURATE-HENSYU-SEC
      ****         END-IF
                   MOVE    "HKNCOMBI-KEY5"   TO  ORC-DBPATH
                   PERFORM 910-HKNCOMBI-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "HKNCOMBI-KEY5"    TO  ORC-DBPATH
           PERFORM   920-DB-CLOSE-SEC
      *
      *    収納マスタから一部負担額チェック等
           IF      LNK-KOREIFTNCHK-NYUGAIKBN  =  "1"
               PERFORM   220-SYUKEI-SEC
           ELSE
               IF  LNK-KOREIFTNCHK-RECEKA     =  SPACE
                   PERFORM   230-SYUKEI-SEC
      *********ELSE
      *********    PERFORM   240-SYUKEI-SEC
               END-IF
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    主保険の負担割合編集
      *****************************************************************
       210-KYURATE-HENSYU-SEC            SECTION.
      *
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
      *
           .
       210-KYURATE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスタから一部負担額チェック等（入院）
      *****************************************************************
       220-SYUKEI-SEC            SECTION.
      *
      *    全国公費を含む保険組合わせの収納があるかチェック
           PERFORM   VARYING   IDX1   FROM  1  BY  1
                 UNTIL   (IDX1                  >    50 )   OR
                         (WRK-HKNCOMBINUM(IDX1) =   ZERO)
      *
              IF   WRK-ZENKOH-ARI(IDX1)  =  1
                 INITIALIZE                            SYUNOU-REC
                 MOVE    LNK-KOREIFTNCHK-HOSPID    TO  SYU-HOSPID
                 MOVE    LNK-KOREIFTNCHK-NYUGAIKBN TO  SYU-NYUGAIKBN
                 MOVE    LNK-KOREIFTNCHK-PTID      TO  SYU-PTID
                 MOVE    LNK-KOREIFTNCHK-SRYYM     TO  SYU-SRYYMD(1:6)
                 MOVE    "__"                      TO  SYU-SRYYMD(7:2)
                 MOVE    WRK-HKNCOMBINUM(IDX1)     TO  SYU-HKNCOMBINUM
                 MOVE    "SYUNOU-KEY19"            TO  ORC-DBPATH
                 PERFORM 900-SYUNOU-SELECT-SEC
                 IF      FLG-SYUNOU            NOT =   ZERO
                     MOVE    01                TO  LNK-KOREIFTNCHK-RC
                     GO  TO  220-SYUKEI-EXT
                 ELSE
                     MOVE    "SYUNOU-KEY19"    TO  ORC-DBPATH
                     PERFORM 910-SYUNOU-READ-SEC
                     IF    FLG-SYUNOU   =    ZERO
                         MOVE    1    TO    FLG-ZENKOH-ARI
                         MOVE   50    TO    IDX1
                     END-IF
                 END-IF
      *
                 MOVE    "SYUNOU-KEY19"    TO  ORC-DBPATH
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
              MOVE    LNK-KOREIFTNCHK-HOSPID    TO  SYU-HOSPID
              MOVE    LNK-KOREIFTNCHK-NYUGAIKBN TO  SYU-NYUGAIKBN
              MOVE    LNK-KOREIFTNCHK-PTID      TO  SYU-PTID
              MOVE    LNK-KOREIFTNCHK-SRYYM     TO  SYU-SRYYMD(1:6)
              MOVE    "__"                      TO  SYU-SRYYMD(7:2)
              MOVE    WRK-HKNCOMBINUM(IDX1)     TO  SYU-HKNCOMBINUM
              MOVE    "SYUNOU-KEY19"            TO  ORC-DBPATH
              PERFORM 900-SYUNOU-SELECT-SEC
              IF      FLG-SYUNOU            NOT =   ZERO
                  MOVE    01                    TO  LNK-KOREIFTNCHK-RC
                  GO  TO  220-SYUKEI-EXT
              ELSE
                  MOVE    "SYUNOU-KEY19"        TO  ORC-DBPATH
                  PERFORM 910-SYUNOU-READ-SEC
                  PERFORM  UNTIL
                             (FLG-SYUNOU        =    1)
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
                    ADD    SYU-KOH-FTN   TO  LNK-KOREIFTNCHK-KOHFTNGAKU
      *
                    COMPUTE  WRK-FTNKEI = SYU-TOTAL-HKNTEN
                                        * 10
                                        * WRK-KYURATE(IDX1)
                                        / 100
                    COMPUTE  WRK-FTNKEI = WRK-FTNKEI +  5
                    COMPUTE  WRK-FTNKEI = WRK-FTNKEI / 10
                    COMPUTE  WRK-FTNKEI = WRK-FTNKEI * 10
      *
                    IF   ( SYU-KOH-HKNNUM(1)   =   "012" )
                    OR   ( SYU-KOH-HKNNUM(2)   =   "012" )
                    OR   ( SYU-KOH-HKNNUM(3)   =   "012" )
                    OR   ( SYU-KOH-HKNNUM(4)   =   "012" )
                        IF   LNK-KOREIFTNCHK-ZENKOHARI =  SPACE
                          MOVE  1      TO   FLG-KEISAN
                        ELSE
                          MOVE  ZERO   TO   FLG-KEISAN
                        END-IF
                        MOVE    15000  TO   WRK-KINGAKU
                        PERFORM   2201-ICHIBUFTN-KOH-SET-SEC
                    ELSE
                      IF   ( SYU-KOH-HKNNUM(1) =   "972" )
                      OR   ( SYU-KOH-HKNNUM(2) =   "972" )
                      OR   ( SYU-KOH-HKNNUM(3) =   "972" )
                      OR   ( SYU-KOH-HKNNUM(4) =   "972" )
                        IF   LNK-KOREIFTNCHK-ZENKOHARI =  SPACE
                          MOVE  1      TO   FLG-KEISAN
                        ELSE
                          MOVE  ZERO   TO   FLG-KEISAN
                        END-IF
                        MOVE    10000  TO   WRK-KINGAKU
                        PERFORM   2201-ICHIBUFTN-KOH-SET-SEC
                      ELSE
                        IF   (  LNK-KOREIFTNCHK-TEISYOTOKU
                                                   NOT =  SPACE  )
                        AND  (  FLG-ZENKOH-ARI         =  ZERO   )
                          IF   LNK-KOREIFTNCHK-ZENKOHARI =  SPACE
                            MOVE    ZERO    TO   FLG-KEISAN
                          ELSE
                            MOVE    1       TO   FLG-KEISAN
                          END-IF
                          PERFORM 2202-ICHIBUFTN-KOH-HEN1-SEC
                        ELSE
                          IF   (  WRK-KYURATE(IDX1)  =  20 OR 30   )
                          AND  (  FLG-ZENKOH-ARI         =  ZERO   )
                          AND  (  LNK-KOREIFTNCHK-KEIKASOCHI
                                                         =  SPACE  )
                          AND  (  LNK-KOREIFTNCHK-KOHYONKAIME
                                                         =  SPACE  )
                           IF   LNK-KOREIFTNCHK-ZENKOHARI =  SPACE
                             MOVE   ZERO    TO   FLG-KEISAN
                           ELSE
                             MOVE   1       TO   FLG-KEISAN
                           END-IF
                           PERFORM 2203-ICHIBUFTN-KOH-HEN2-SEC
                          ELSE
                           IF  (SYU-KOH-HKNNUM(1) = "974")
                           OR  (SYU-KOH-HKNNUM(2) = "974")
                           OR  (SYU-KOH-HKNNUM(3) = "974")
                           OR  (SYU-KOH-HKNNUM(4) = "974")
                               IF   LNK-KOREIFTNCHK-ZENKOHARI =  SPACE
                                 MOVE     1      TO   FLG-KEISAN
                               ELSE
                                 MOVE     ZERO   TO   FLG-KEISAN
                               END-IF
                               MOVE       20000  TO   WRK-KINGAKU
                               PERFORM   2201-ICHIBUFTN-KOH-SET-SEC
                           ELSE
                            IF     FLG-ZENKOH-ARI            =  ZERO
                               IF   LNK-KOREIFTNCHK-ZENKOHARI =  SPACE
                                 MOVE    ZERO    TO   FLG-KEISAN
                               ELSE
                                 MOVE    1       TO   FLG-KEISAN
                               END-IF
                               PERFORM 2204-ICHIBUFTN-KOH-HEN3-SEC
                            ELSE
                             IF ((SYU-KOH-HKNNUM(1)     NOT = "027") AND
                                 (SYU-KOH-HKNNUM(1)(1:1)    = "0"  ))
                             OR ( SYU-KOH-HKNNUM(2)(1:1)    = "0"   )
                             OR ( SYU-KOH-HKNNUM(3)(1:1)    = "0"   )
                             OR ( SYU-KOH-HKNNUM(4)(1:1)    = "0"   )
                               IF   LNK-KOREIFTNCHK-ZENKOHARI =  SPACE
                                 MOVE    1       TO   FLG-KEISAN
                               ELSE
                                 MOVE    ZERO    TO   FLG-KEISAN
                               END-IF
                               PERFORM 2204-ICHIBUFTN-KOH-HEN3-SEC
                             ELSE
                               IF LNK-KOREIFTNCHK-TEISYOTOKU NOT = SPACE
                                 IF   LNK-KOREIFTNCHK-ZENKOHARI =  SPACE
                                   MOVE  ZERO    TO   FLG-KEISAN
                                 ELSE
                                   MOVE  1       TO   FLG-KEISAN
                                 END-IF
                                 PERFORM 2202-ICHIBUFTN-KOH-HEN1-SEC
                               ELSE
                                 IF   (WRK-KYURATE(IDX1)  =  20 OR 30)
                                 AND  (LNK-KOREIFTNCHK-KEIKASOCHI
                                                             =  SPACE)
                                 AND  (LNK-KOREIFTNCHK-KOHYONKAIME
                                                             =  SPACE)
                                   IF LNK-KOREIFTNCHK-ZENKOHARI = SPACE
                                     MOVE    ZERO    TO   FLG-KEISAN
                                   ELSE
                                     MOVE    1       TO   FLG-KEISAN
                                   END-IF
                                   PERFORM 2203-ICHIBUFTN-KOH-HEN2-SEC
                                 ELSE
                                   IF LNK-KOREIFTNCHK-ZENKOHARI = SPACE
                                     MOVE    ZERO    TO   FLG-KEISAN
                                   ELSE
                                     MOVE    1       TO   FLG-KEISAN
                                   END-IF
                                   PERFORM 2204-ICHIBUFTN-KOH-HEN3-SEC
                                 END-IF
                               END-IF
                             END-IF
                            END-IF
                           END-IF
                          END-IF
                        END-IF
                      END-IF
                    END-IF
                   END-IF
      *
                   MOVE   "SYUNOU-KEY19"   TO  ORC-DBPATH
                   PERFORM 910-SYUNOU-READ-SEC
                  END-PERFORM
              END-IF
      *
              MOVE    "SYUNOU-KEY19"    TO  ORC-DBPATH
              PERFORM   920-DB-CLOSE-SEC
      *
           END-PERFORM
      *
           .
       220-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスタから一部負担額チェック等（外来）
      *****************************************************************
       230-SYUKEI-SEC            SECTION.
      *
      *    全国公費を含む保険組合わせの収納があるかチェック
           PERFORM   VARYING   IDX1   FROM  1  BY  1
                 UNTIL   (IDX1                  >    50 )   OR
                         (WRK-HKNCOMBINUM(IDX1) =   ZERO)
      *
              IF   WRK-ZENKOH-ARI(IDX1)  =  1
                 INITIALIZE                            SYUNOU-REC
                 MOVE    LNK-KOREIFTNCHK-HOSPID    TO  SYU-HOSPID
                 MOVE    LNK-KOREIFTNCHK-NYUGAIKBN TO  SYU-NYUGAIKBN
                 MOVE    LNK-KOREIFTNCHK-PTID      TO  SYU-PTID
                 MOVE    LNK-KOREIFTNCHK-SRYYM     TO  SYU-SRYYMD(1:6)
                 MOVE    "__"                      TO  SYU-SRYYMD(7:2)
                 MOVE    WRK-HKNCOMBINUM(IDX1)     TO  SYU-HKNCOMBINUM
                 MOVE    "SYUNOU-KEY41"            TO  ORC-DBPATH
                 PERFORM 900-SYUNOU-SELECT-SEC
                 IF      FLG-SYUNOU            NOT =   ZERO
                     MOVE    01                TO  LNK-KOREIFTNCHK-RC
                     GO  TO  230-SYUKEI-EXT
                 ELSE
                     MOVE    "SYUNOU-KEY41"    TO  ORC-DBPATH
                     PERFORM 910-SYUNOU-READ-SEC
                     IF    FLG-SYUNOU   =    ZERO
                         MOVE    1    TO    FLG-ZENKOH-ARI
                         MOVE   50    TO    IDX1
                     END-IF
                 END-IF
      *
                 MOVE    "SYUNOU-KEY41"    TO  ORC-DBPATH
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
              MOVE    LNK-KOREIFTNCHK-HOSPID    TO  SYU-HOSPID
              MOVE    LNK-KOREIFTNCHK-NYUGAIKBN TO  SYU-NYUGAIKBN
              MOVE    LNK-KOREIFTNCHK-PTID      TO  SYU-PTID
              MOVE    LNK-KOREIFTNCHK-SRYYM     TO  SYU-SRYYMD(1:6)
              MOVE    "__"                      TO  SYU-SRYYMD(7:2)
              MOVE    WRK-HKNCOMBINUM(IDX1)     TO  SYU-HKNCOMBINUM
              MOVE    "SYUNOU-KEY41"            TO  ORC-DBPATH
              PERFORM 900-SYUNOU-SELECT-SEC
              IF      FLG-SYUNOU            NOT =   ZERO
                  MOVE    01                    TO  LNK-KOREIFTNCHK-RC
                  GO  TO  230-SYUKEI-EXT
              ELSE
                  MOVE    "SYUNOU-KEY41"        TO  ORC-DBPATH
                  PERFORM 910-SYUNOU-READ-SEC
                  PERFORM  UNTIL
                             (FLG-SYUNOU        =    1)
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
                    ADD    SYU-KOH-FTN   TO  LNK-KOREIFTNCHK-KOHFTNGAKU
      *
                    COMPUTE  WRK-FTNKEI = SYU-TOTAL-HKNTEN
                                        * 10
                                        * WRK-KYURATE(IDX1)
                                        / 100
                    COMPUTE  WRK-FTNKEI = WRK-FTNKEI +  5
                    COMPUTE  WRK-FTNKEI = WRK-FTNKEI / 10
                    COMPUTE  WRK-FTNKEI = WRK-FTNKEI * 10
      *
                    IF   ( SYU-KOH-HKNNUM(1)   =   "012" )
                    OR   ( SYU-KOH-HKNNUM(2)   =   "012" )
                    OR   ( SYU-KOH-HKNNUM(3)   =   "012" )
                    OR   ( SYU-KOH-HKNNUM(4)   =   "012" )
                        IF   LNK-KOREIFTNCHK-ZENKOHARI =  SPACE
                          MOVE     1        TO   FLG-KEISAN
                        ELSE
                          MOVE     ZERO     TO   FLG-KEISAN
                        END-IF
                        MOVE       8000     TO   WRK-KINGAKU
                        PERFORM   2201-ICHIBUFTN-KOH-SET-SEC
                    ELSE
                      IF   ( SYU-KOH-HKNNUM(1) =   "972" )
                      OR   ( SYU-KOH-HKNNUM(2) =   "972" )
                      OR   ( SYU-KOH-HKNNUM(3) =   "972" )
                      OR   ( SYU-KOH-HKNNUM(4) =   "972" )
                        IF   LNK-KOREIFTNCHK-ZENKOHARI =  SPACE
                          MOVE     1        TO   FLG-KEISAN
                        ELSE
                          MOVE     ZERO     TO   FLG-KEISAN
                        END-IF
                        IF      LNK-KOREIFTNCHK-TEISYOTOKU
                                                   NOT =  SPACE
                          MOVE     8000     TO   WRK-KINGAKU
                          PERFORM   2201-ICHIBUFTN-KOH-SET-SEC
                        ELSE
                          MOVE    10000     TO   WRK-KINGAKU
                          PERFORM   2201-ICHIBUFTN-KOH-SET-SEC
                        END-IF
                      ELSE
                        IF   (  LNK-KOREIFTNCHK-TEISYOTOKU
                                                   NOT =  SPACE  )
                        AND  (  FLG-ZENKOH-ARI         =  ZERO   )
                             IF   LNK-KOREIFTNCHK-ZENKOHARI =  SPACE
                               MOVE  ZERO       TO   FLG-KEISAN
                             ELSE
                               MOVE  1          TO   FLG-KEISAN
                             END-IF
                             MOVE    8000       TO   WRK-KINGAKU
                             PERFORM   2201-ICHIBUFTN-KOH-SET-SEC
                        ELSE
                          IF   (  WRK-KYURATE(IDX1)   =  20 OR 30  )
                          AND  (  FLG-ZENKOH-ARI         =  ZERO   )
                          AND  (  LNK-KOREIFTNCHK-KEIKASOCHI
                                                         =  SPACE  )
                           IF   LNK-KOREIFTNCHK-ZENKOHARI =  SPACE
                             MOVE    ZERO       TO   FLG-KEISAN
                           ELSE
                             MOVE    1          TO   FLG-KEISAN
                           END-IF
                           IF   LNK-KOREIFTNCHK-SRYYM  >=  "200610"
                             MOVE       44400   TO   WRK-KINGAKU
                             PERFORM   2201-ICHIBUFTN-KOH-SET-SEC
                           ELSE
                             MOVE       40200   TO   WRK-KINGAKU
                             PERFORM   2201-ICHIBUFTN-KOH-SET-SEC
                           END-IF
                          ELSE
                           IF     FLG-ZENKOH-ARI            =  ZERO
                               IF   LNK-KOREIFTNCHK-ZENKOHARI =  SPACE
                                 MOVE    ZERO   TO   FLG-KEISAN
                               ELSE
                                 MOVE    1      TO   FLG-KEISAN
                               END-IF
                               MOVE   12000     TO   WRK-KINGAKU
                               PERFORM   2201-ICHIBUFTN-KOH-SET-SEC
                           ELSE
                            IF  ((SYU-KOH-HKNNUM(1)     NOT = "027") AND
                                 (SYU-KOH-HKNNUM(1)(1:1)    = "0"  ))
                            OR  ( SYU-KOH-HKNNUM(2)(1:1)    = "0"   )
                            OR  ( SYU-KOH-HKNNUM(3)(1:1)    = "0"   )
                            OR  ( SYU-KOH-HKNNUM(4)(1:1)    = "0"   )
                               IF   LNK-KOREIFTNCHK-ZENKOHARI =  SPACE
                                 MOVE    1      TO   FLG-KEISAN
                               ELSE
                                 MOVE    ZERO   TO   FLG-KEISAN
                               END-IF
                               MOVE   12000     TO   WRK-KINGAKU
                               PERFORM   2201-ICHIBUFTN-KOH-SET-SEC
                            ELSE
                             IF   ( WRK-KYURATE(IDX1) =  20 OR 30 )
                             AND  ((SYU-KOH-HKNNUM(1) = "974"    ) OR
                                   (SYU-KOH-HKNNUM(2) = "974"    ) OR
                                   (SYU-KOH-HKNNUM(3) = "974"    ) OR
                                   (SYU-KOH-HKNNUM(4) = "974"    ))
                               IF   LNK-KOREIFTNCHK-ZENKOHARI =  SPACE
                                 MOVE    1      TO   FLG-KEISAN
                               ELSE
                                 MOVE    ZERO   TO   FLG-KEISAN
                               END-IF
                               MOVE   20000     TO   WRK-KINGAKU
                               PERFORM   2201-ICHIBUFTN-KOH-SET-SEC
                             ELSE
                               IF LNK-KOREIFTNCHK-TEISYOTOKU NOT = SPACE
                                 IF   LNK-KOREIFTNCHK-ZENKOHARI =  SPACE
                                   MOVE  ZERO   TO   FLG-KEISAN
                                 ELSE
                                   MOVE  1      TO   FLG-KEISAN
                                 END-IF
                                 MOVE  8000     TO   WRK-KINGAKU
                                 PERFORM     2201-ICHIBUFTN-KOH-SET-SEC
                               ELSE
                                IF  (WRK-KYURATE(IDX1)   =  20 OR 30   )
                                AND (LNK-KOREIFTNCHK-KEIKASOCHI = SPACE)
                                 IF   LNK-KOREIFTNCHK-ZENKOHARI =  SPACE
                                   MOVE   ZERO  TO   FLG-KEISAN
                                 ELSE
                                   MOVE   1     TO   FLG-KEISAN
                                 END-IF
                                 IF   LNK-KOREIFTNCHK-SRYYM  >= "200610"
                                   MOVE 44400   TO   WRK-KINGAKU
                                   PERFORM   2201-ICHIBUFTN-KOH-SET-SEC
                                 ELSE
                                   MOVE 40200   TO   WRK-KINGAKU
                                   PERFORM   2201-ICHIBUFTN-KOH-SET-SEC
                                 END-IF
                                ELSE
                                 IF   LNK-KOREIFTNCHK-ZENKOHARI =  SPACE
                                   MOVE   ZERO  TO   FLG-KEISAN
                                 ELSE
                                   MOVE   1     TO   FLG-KEISAN
                                 END-IF
                                 MOVE   12000   TO   WRK-KINGAKU
                                 PERFORM     2201-ICHIBUFTN-KOH-SET-SEC
                                END-IF
                               END-IF
                             END-IF
                            END-IF
                           END-IF
                          END-IF
                        END-IF
                      END-IF
                    END-IF
                   END-IF
      *
                   MOVE   "SYUNOU-KEY41"   TO  ORC-DBPATH
                   PERFORM 910-SYUNOU-READ-SEC
                  END-PERFORM
              END-IF
      *
              MOVE    "SYUNOU-KEY41"    TO  ORC-DBPATH
              PERFORM   920-DB-CLOSE-SEC
      *
           END-PERFORM
      *
           .
       230-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険欄の一部負担金額セット（公費含む場合）
      *****************************************************************
       2201-ICHIBUFTN-KOH-SET-SEC               SECTION.
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
                                  WRK-KINGAKU         *
                                  WRK-GENGAKU-RITUX   /
                                  100
                       END-IF
                   END-IF
                 WHEN        2
                   CONTINUE
                 WHEN        3
                 WHEN        4
                       MOVE   ZERO     TO   WRK-KINGAKU
               END-EVALUATE
               COMPUTE  WRK-KINGAKU  =  WRK-KINGAKU   +    5
               COMPUTE  WRK-KINGAKU  =  WRK-KINGAKU   /   10
               COMPUTE  WRK-KINGAKU  =  WRK-KINGAKU   *   10
      *
           END-IF
      *
           IF  FLG-KEISAN  =  1
             IF  WRK-FTN-KOH-XX2   >   WRK-KINGAKU
                MOVE  WRK-KINGAKU      TO   WRK-FTN-KOH-XX2
             END-IF
             COMPUTE  WRK-FTN-KOH-YY2  =  WRK-KINGAKU - WRK-FTN-KOH-XX2
             IF      WRK-FTNKEI    >   WRK-FTN-KOH-YY2
                ADD   WRK-FTN-KOH-YY2  TO  LNK-KOREIFTNCHK-ICHIBUGAKU2
                                           WRK-FTN-KOH-XX2
             ELSE
                ADD   WRK-FTNKEI       TO  LNK-KOREIFTNCHK-ICHIBUGAKU2
                                           WRK-FTN-KOH-XX2
             END-IF
           ELSE
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
           END-IF
      *
           .
       2201-ICHIBUFTN-KOH-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険欄の一部負担金額編集１
      *****************************************************************
       2202-ICHIBUFTN-KOH-HEN1-SEC               SECTION.
      *
           EVALUATE   LNK-KOREIFTNCHK-TEISYOTOKU
             WHEN  "1"
             WHEN  "4"
                MOVE    15000     TO   WRK-KINGAKU
                PERFORM   2201-ICHIBUFTN-KOH-SET-SEC
             WHEN  "2"
             WHEN  "3"
                MOVE    24600     TO   WRK-KINGAKU
                PERFORM   2201-ICHIBUFTN-KOH-SET-SEC
           END-EVALUATE
      *
           .
       2202-ICHIBUFTN-KOH-HEN1-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険欄の一部負担金額編集２
      *****************************************************************
       2203-ICHIBUFTN-KOH-HEN2-SEC               SECTION.
      *
           IF  LNK-KOREIFTNCHK-SRYYM   >=  "200610"
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
              PERFORM   2201-ICHIBUFTN-KOH-SET-SEC
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
              PERFORM   2201-ICHIBUFTN-KOH-SET-SEC
           END-IF
      *
           .
       2203-ICHIBUFTN-KOH-HEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険欄の一部負担金額編集３
      *****************************************************************
       2204-ICHIBUFTN-KOH-HEN3-SEC               SECTION.
      *
           IF  LNK-KOREIFTNCHK-SRYYM   >=  "200610"
              MOVE       44400  TO   WRK-KINGAKU
              PERFORM   2201-ICHIBUFTN-KOH-SET-SEC
           ELSE
              MOVE       40200  TO   WRK-KINGAKU
              PERFORM   2201-ICHIBUFTN-KOH-SET-SEC
           END-IF
      *
           .
       2204-ICHIBUFTN-KOH-HEN3-EXT.
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
       900-HKNCOMBI-SELECT-EXT.
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
       900-SYUNOU-SELECT-SEC        SECTION.
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
       900-SYUNOU-SELECT-EXT.
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
      *    ＤＢクローズ処理
      *****************************************************************
       920-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
      *
       920-DB-CLOSE-EXT.
           EXIT.
