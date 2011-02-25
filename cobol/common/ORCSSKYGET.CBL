      *******************************************************************
      * Project code name "ORCA"
      * Æü°åÉ¸½à¥ì¥»¥×¥È¥½¥Õ¥È¡ÊJMA standard receipt software¡Ë
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
       PROGRAM-ID.             ORCSSKYGET.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : 
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ÊÝ¸±¼ÔÉéÃ´Åù¤ÎÀÁµá³Û¼è¤ê¹þ¤ß¥µ¥Ö
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
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
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-KOHSKY                PIC 9(01).
           03  FLG-SYUNOU                PIC 9(01).
           03  FLG-SYUKEI                PIC 9(01).
           03  FLG-JIHIRANSYUKEI         PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX1                      PIC 9(03).
           03  IDX2                      PIC 9(01).
           03  IDX3                      PIC 9(02).
      *
      *    ¥ï¡¼¥¯ÎÎ°è
       01  WRK-AREA.
           03  WRK-KEISAN                PIC S9(09).
           03  WRK-KEISAN2               PIC S9(09).
      *
           03  WRK-TAIHI-AREA.
             05  WRK-TAIHI-PTID          PIC 9(10).
             05  WRK-TAIHI-HKNCOMBINUM   PIC X(04).
      *
           03  WRK-TAIHI-AREA2.
             05  WRK-TAIHI-DAY           PIC 9(01)   OCCURS   31.
      *
      *    ¶¦ÄÌÎÎ°è
           COPY    "MCPAREA".
      *
      *    ¸øÈñÀÁµá
           COPY    "CPKOHSKY.INC".
      *
      *    ¼ýÇ¼
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSSKYGET.INC".
      *
           COPY    "CPRCF010.INC".
      *
           COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           ORCSSKYGETAREA
           RECE10-REC
           SPA-AREA
           .
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           MOVE    ZERO    TO    LNK-SKYGET-RC
           INITIALIZE            LNK-SKYGET-OUT-AREA
           INITIALIZE            LNK-SKYGET-OUT2-AREA
      *
           INITIALIZE            FLG-AREA
           INITIALIZE            IDX-AREA
           INITIALIZE            WRK-AREA
      *
      *    ¥Ñ¥é¥á¥¿¥Á¥§¥Ã¥¯
           PERFORM   100-PRM-CHK-SEC
      *
           IF    LNK-SKYGET-RC    =   ZERO
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
      *    ¥Ñ¥é¥á¥¿¥Á¥§¥Ã¥¯
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
           EVALUATE    LNK-SKYGET-KBN
      *      ¡ã¼ÒÊÝ¡¦¹ñÊÝ¡¦¸å´ü¹âÎð¤ÎÀÁµá´ÉÍý¥Ç¡¼¥¿Ã±°Ì¤ÎÊÔ½¸¡ä
             WHEN      "1"
               IF      LNK-SKYGET-SRYYM  <  201004
                      MOVE    1     TO   LNK-SKYGET-RC
               ELSE
                 IF    RECE10-REC        =  SPACE
                      MOVE    2     TO   LNK-SKYGET-RC
                 ELSE
                   IF  RECE10-SRYYM      <  201004
                      MOVE    3     TO   LNK-SKYGET-RC
                   END-IF
                 END-IF
               END-IF
      *      ¡ã¼ÒÊÝ¡¦¹ñÊÝ¡¦¸å´ü¹âÎð¤Î¼«ÈñÊ¬°Ê²¼¤ÎÊÔ½¸¡ä
      *      ¡ãÆÃÊÌÎÅÍÜÈñ¡¢¼«ÈñÊÝ¸±¡¢¼£¸³¡¢Ï«ºÒ¡¢¼«ÇåÀÕ¡¢¸ø³²¤ÎÊÔ½¸¡ä
             WHEN      "2"
               IF      LNK-SKYGET-SRYYM  <  201004
                      MOVE    4     TO   LNK-SKYGET-RC
               END-IF
             WHEN      OTHER
                      MOVE    5     TO   LNK-SKYGET-RC
           END-EVALUATE
      *
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç¼èÆÀ
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
           EVALUATE    LNK-SKYGET-KBN
             WHEN      "1"
               PERFORM    2001-HENSYU1-SEC
             WHEN      "2"
               PERFORM    2002-HENSYU2-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¶èÊ¬¡É£±¡É¤ÎÊÔ½¸
      *****************************************************************
       2001-HENSYU1-SEC            SECTION.
      *
           MOVE   RECE10-SKYIRYO-INF   TO   LNK-SKYGET-IRYO-INF
           MOVE   RECE10-SKYSKJ-INF    TO   LNK-SKYGET-SKJ-INF
      *
           IF    (LNK-SKYGET-IRYO-TOTALTEN   NOT = ZERO)
           OR    (LNK-SKYGET-SKJ-RYOYOHI     NOT = ZERO)
             PERFORM   VARYING   IDX1   FROM  1  BY  1
                        UNTIL    IDX1                >  4
               IF   RECE10-KOHNUM(IDX1)  NOT = SPACE
                 MOVE  RECE10-KOHNUM(IDX1)
                            TO  LNK-SKYGET-KOH-HKNNUM(IDX1)
                 MOVE  RECE10-JNISSU(IDX1 + 1)
                            TO  LNK-SKYGET-KOH-NISSU(IDX1)
                 MOVE  RECE10-TOTALTEN(IDX1 + 1)
                            TO  LNK-SKYGET-KOH-TOTALTEN(IDX1)
                 MOVE  RECE10-FTNMONEY(IDX1 + 1)
                            TO  LNK-SKYGET-KOH-FTNMONEY(IDX1)
                 MOVE  RECE10-SHOKUJINISSU(IDX1 + 1)
                            TO  LNK-SKYGET-KOH-KAISU(IDX1)
                 MOVE  RECE10-SHOKUJIRYOYOHI(IDX1 + 1)
                            TO  LNK-SKYGET-KOH-RYOYOHI(IDX1)
                 MOVE  RECE10-SHOKUJIFTN(IDX1 + 1)
                            TO  LNK-SKYGET-KOH-SHOKUJIFTN(IDX1)
               END-IF
             END-PERFORM
      *
             INITIALIZE                          KOHSKY-REC
             MOVE     RECE10-KEY             TO  KOHSKY-KEY
             PERFORM  900-KOHSKY-READ-SEC
             IF       FLG-KOHSKY  =   ZERO
               PERFORM   VARYING   IDX1   FROM  1  BY  1
                          UNTIL    IDX1                >  4
                IF  (KOHSKY-KOHNUM(IDX1) NOT = SPACE                   )
                AND (KOHSKY-KOHNUM(IDX1) NOT = LNK-SKYGET-KOH-HKNNUM(1))
                AND (KOHSKY-KOHNUM(IDX1) NOT = LNK-SKYGET-KOH-HKNNUM(2))
                AND (KOHSKY-KOHNUM(IDX1) NOT = LNK-SKYGET-KOH-HKNNUM(3))
                AND (KOHSKY-KOHNUM(IDX1) NOT = LNK-SKYGET-KOH-HKNNUM(4))
                 MOVE  KOHSKY-KOHNUM(IDX1)
                            TO  LNK-SKYGET-KOHETC-HKNNUM(IDX1)
                 MOVE  KOHSKY-JNISSU(IDX1 + 1)
                            TO  LNK-SKYGET-KOHETC-NISSU(IDX1)
                 MOVE  KOHSKY-TOTALTEN(IDX1 + 1)
                            TO  LNK-SKYGET-KOHETC-TOTALTEN(IDX1)
                 MOVE  KOHSKY-FTNMONEY(IDX1 + 1)
                            TO  LNK-SKYGET-KOHETC-FTNMONEY(IDX1)
                 MOVE  KOHSKY-SHOKUJINISSU(IDX1 + 1)
                            TO  LNK-SKYGET-KOHETC-KAISU(IDX1)
                 MOVE  KOHSKY-SHOKUJIRYOYOHI(IDX1 + 1)
                            TO  LNK-SKYGET-KOHETC-RYOYOHI(IDX1)
                 MOVE  KOHSKY-SHOKUJIFTN(IDX1 + 1)
                            TO  LNK-SKYGET-KOHETC-SHOKUJIFTN(IDX1)
                END-IF
               END-PERFORM
             END-IF
           ELSE
             MOVE    11     TO  LNK-SKYGET-RC
           END-IF
      *
           .
       2001-HENSYU1-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¶èÊ¬¡É£²¡É¤ÎÊÔ½¸
      *****************************************************************
       2002-HENSYU2-SEC            SECTION.
      *
      *    Æþ±¡¡Ê²¾¼ýÇ¼¡Ë
           MOVE    ZERO                  TO   FLG-SYUKEI
           INITIALIZE                         WRK-TAIHI-AREA
           INITIALIZE                         WRK-TAIHI-AREA2
           INITIALIZE                         SYUNOU-REC
           MOVE    SPA-HOSPNUM           TO   SYU-HOSPNUM
           MOVE    "1"                   TO   SYU-NYUGAIKBN
           MOVE    LNK-SKYGET-SRYYM      TO   SYU-SRYYMD(1:6)
           MOVE    "__"                  TO   SYU-SRYYMD(7:2)
           MOVE    SYUNOU-REC            TO   MCPDATA-REC
           MOVE    "tbl_syunou"          TO   MCP-TABLE
           MOVE    "key76"               TO   MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC   =   ZERO
             MOVE    "tbl_syunou"        TO   MCP-TABLE
             MOVE    "key76"             TO   MCP-PATHNAME
             PERFORM 900-SYUNOU-READ-SEC
             PERFORM    UNTIL   (FLG-SYUNOU   =   1)
               IF  SYU-DENPJTIKBN    NOT =  "3"
                 MOVE      1             TO   IDX2
                 PERFORM   20021-KINGAKU-HENSYU-SEC
               END-IF
               MOVE    "tbl_syunou"      TO   MCP-TABLE
               MOVE    "key76"           TO   MCP-PATHNAME
               PERFORM 900-SYUNOU-READ-SEC
             END-PERFORM
           END-IF
           MOVE    "tbl_syunou"          TO   MCP-TABLE
           MOVE    "key76"               TO   MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    ³°Íè
           MOVE    ZERO                  TO   FLG-SYUKEI
           INITIALIZE                         WRK-TAIHI-AREA
           INITIALIZE                         WRK-TAIHI-AREA2
           INITIALIZE                         SYUNOU-REC
           MOVE    SPA-HOSPNUM           TO   SYU-HOSPNUM
           MOVE    "2"                   TO   SYU-NYUGAIKBN
           MOVE    LNK-SKYGET-SRYYM      TO   SYU-SRYYMD(1:6)
           MOVE    "__"                  TO   SYU-SRYYMD(7:2)
           MOVE    SYUNOU-REC            TO   MCPDATA-REC
           MOVE    "tbl_syunou"          TO   MCP-TABLE
           MOVE    "key75"               TO   MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC   =   ZERO
             MOVE    "tbl_syunou"        TO   MCP-TABLE
             MOVE    "key75"             TO   MCP-PATHNAME
             PERFORM 900-SYUNOU-READ-SEC
             PERFORM    UNTIL   (FLG-SYUNOU   =   1)
               IF  SYU-DENPJTIKBN    NOT =  "3"
                 MOVE      2             TO   IDX2
                 PERFORM   20021-KINGAKU-HENSYU-SEC
               END-IF
               MOVE    "tbl_syunou"      TO   MCP-TABLE
               MOVE    "key75"           TO   MCP-PATHNAME
               PERFORM 900-SYUNOU-READ-SEC
             END-PERFORM
           END-IF
           MOVE    "tbl_syunou"          TO   MCP-TABLE
           MOVE    "key75"               TO   MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    Æþ±¡¡ÊËÜ¼ýÇ¼¡Ë
           MOVE    ZERO                  TO   FLG-SYUKEI
           INITIALIZE                         WRK-TAIHI-AREA
           INITIALIZE                         WRK-TAIHI-AREA2
           INITIALIZE                         SYUNOU-REC
           MOVE    SPA-HOSPNUM           TO   SYU-HOSPNUM
           MOVE    "1"                   TO   SYU-NYUGAIKBN
           MOVE    LNK-SKYGET-SRYYM      TO   SYU-SRYYMD(1:6)
           MOVE    "__"                  TO   SYU-SRYYMD(7:2)
           MOVE    SYUNOU-REC            TO   MCPDATA-REC
           MOVE    "tbl_syunou"          TO   MCP-TABLE
           MOVE    "key75"               TO   MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC   =   ZERO
             MOVE    "tbl_syunou"        TO   MCP-TABLE
             MOVE    "key75"             TO   MCP-PATHNAME
             PERFORM 900-SYUNOU-READ-SEC
             PERFORM    UNTIL   (FLG-SYUNOU   =   1)
               IF  SYU-DENPJTIKBN    NOT =  "3"
                 MOVE      3             TO   IDX2
                 PERFORM   20021-KINGAKU-HENSYU-SEC
               END-IF
               MOVE    "tbl_syunou"      TO   MCP-TABLE
               MOVE    "key75"           TO   MCP-PATHNAME
               PERFORM 900-SYUNOU-READ-SEC
             END-PERFORM
           END-IF
           MOVE    "tbl_syunou"          TO   MCP-TABLE
           MOVE    "key75"               TO   MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2002-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¶èÊ¬¡É£²¡É¡Ê¶â³ÛÊÔ½¸¡Ë
      *****************************************************************
       20021-KINGAKU-HENSYU-SEC            SECTION.
      *
      *    ¥¤¥ó¥Ç¥Ã¥¯¥¹ÊÔ½¸
           PERFORM   200211-INDEX-HENSYU-SEC
      *
           IF  (WRK-TAIHI-PTID         =  SYU-PTID       )
           AND (WRK-TAIHI-HKNCOMBINUM  =  SYU-HKNCOMBINUM)
             CONTINUE
           ELSE
             MOVE   1                TO    FLG-SYUKEI
             INITIALIZE                    WRK-TAIHI-AREA
             MOVE   SYU-PTID         TO    WRK-TAIHI-PTID
             MOVE   SYU-HKNCOMBINUM  TO    WRK-TAIHI-HKNCOMBINUM
             INITIALIZE                    WRK-TAIHI-AREA2
           END-IF
      *
           MOVE     ZERO     TO    FLG-JIHIRANSYUKEI
           IF  (SYU-TOTAL-HKNTEN      =  ZERO)
           AND (SYU-TOTAL-MONEY   NOT =  ZERO)
           AND (IDX1                  =   5  )
           AND (SYU-JYO-COMPFTN   NOT =  ZERO)
           AND (LNK-SKYGET-SRYYM  >=   201104)
             MOVE    1       TO    FLG-JIHIRANSYUKEI
           END-IF
      *
           IF   (IDX1  =   4  OR  5  OR  6)
           AND  (IDX2  =   1  OR  2       )
      *
             IF    FLG-SYUKEI   =   1
               IF  SYU-TOTAL-HKNTEN    NOT =  ZERO
               OR  SYU-RYOYOHI-SKJ     NOT =  ZERO
               OR  SYU-RYOYOHI-LIFE    NOT =  ZERO
               OR  FLG-JIHIRANSYUKEI       =   1
                 COMPUTE  LNK-SKYGET-KENSU(IDX1 IDX2) =
                          LNK-SKYGET-KENSU(IDX1 IDX2) +
                          1
                 MOVE     ZERO    TO    FLG-SYUKEI
               END-IF
             END-IF
      *
             IF    SYU-TOTAL-HKNTEN    NOT =  ZERO
             OR    FLG-JIHIRANSYUKEI       =   1
               EVALUATE  SYU-NYUGAIKBN
                 WHEN      "1"
                   PERFORM   VARYING   IDX3   FROM  1  BY  1
                              UNTIL    IDX3    >   31
                     IF  (SYU-DAY(IDX3)        =  "1" OR "2")
                     AND (WRK-TAIHI-DAY(IDX3)  =  ZERO      )
                       COMPUTE  LNK-SKYGET-IRYO2-NISSU(IDX1 IDX2) =
                                LNK-SKYGET-IRYO2-NISSU(IDX1 IDX2) +
                                1
                       MOVE       1     TO    WRK-TAIHI-DAY(IDX3)
                     END-IF
                   END-PERFORM
                 WHEN      "2"
                   MOVE   SYU-SRYYMD(7:2)   TO   IDX3
                   IF     WRK-TAIHI-DAY(IDX3)  =  ZERO
                       COMPUTE  LNK-SKYGET-IRYO2-NISSU(IDX1 IDX2) =
                                LNK-SKYGET-IRYO2-NISSU(IDX1 IDX2) +
                                1
                       MOVE       1     TO    WRK-TAIHI-DAY(IDX3)
                   END-IF
               END-EVALUATE
             END-IF
             IF    SYU-TOTAL-HKNTEN    NOT =  ZERO
               COMPUTE    LNK-SKYGET-IRYO2-TOTALTEN(IDX1 IDX2) =
                          LNK-SKYGET-IRYO2-TOTALTEN(IDX1 IDX2) +
                          SYU-TOTAL-HKNTEN
               IF    IDX1  =   4  OR  5
                 COMPUTE  LNK-SKYGET-IRYO2-ICHIBUFTN(IDX1 IDX2) =
                          LNK-SKYGET-IRYO2-ICHIBUFTN(IDX1 IDX2) +
                          SYU-HKNTEKMONEY
               ELSE
                 COMPUTE  LNK-SKYGET-IRYO2-HOKENJA(IDX1 IDX2) =
                          LNK-SKYGET-IRYO2-HOKENJA(IDX1 IDX2) +
                          SYU-HKNTEKMONEY
               END-IF
             ELSE
               IF    FLG-JIHIRANSYUKEI   =   1
                 COMPUTE    LNK-SKYGET-IRYO2-TOTALTEN(IDX1 IDX2) =
                            LNK-SKYGET-IRYO2-TOTALTEN(IDX1 IDX2) +
                            SYU-TOTAL-MONEY
                 COMPUTE    LNK-SKYGET-IRYO2-ICHIBUFTN(IDX1 IDX2) =
                            LNK-SKYGET-IRYO2-ICHIBUFTN(IDX1 IDX2) +
                            SYU-JYO-COMPFTN
               END-IF
             END-IF
      *
             COMPUTE    LNK-SKYGET-SKJ2-KAISU(IDX1 IDX2) =
                        LNK-SKYGET-SKJ2-KAISU(IDX1 IDX2) +
                        SYU-SYUSKJ-NISSU
             COMPUTE    LNK-SKYGET-SKJ2-RYOYOHI(IDX1 IDX2) =
                        LNK-SKYGET-SKJ2-RYOYOHI(IDX1 IDX2) +
                        SYU-RYOYOHI-SKJ                    +
                        SYU-RYOYOHI-LIFE
             IF    IDX1  =   4  OR  5
               COMPUTE  LNK-SKYGET-SKJ2-ICHIBUFTN(IDX1 IDX2) =
                        LNK-SKYGET-SKJ2-ICHIBUFTN(IDX1 IDX2) +
                        SYU-SKYMONEY-SKJ-KEI                 +
                        SYU-SKYMONEY-LIFE-KEI
             ELSE
               COMPUTE  LNK-SKYGET-SKJ2-HOKENJA(IDX1 IDX2) =
                        LNK-SKYGET-SKJ2-HOKENJA(IDX1 IDX2) +
                        SYU-SKYMONEY-SKJ-KEI               +
                        SYU-SKYMONEY-LIFE-KEI
             END-IF
      *
           END-IF
      *
           IF   (IDX1  =   7  OR  8  OR  9)
           AND  (IDX2  =   1  OR  2       )
      *
             COMPUTE    WRK-KEISAN                 =
                        SYU-SYUSKJ-RYOYOHI         +
                        SYU-RMSAGAKU               +
                        SYU-SKYMONEY-SKJ-JIHI-KEI  +
                        SYU-SKYMONEY-LIFE-JIHI-KEI +
                        SYU-TOTAL-TGMONEY          +
                        SYU-TOTAL-TGMONEY-TAX      +
                        SYU-JIHI-TOTAL             +
                        SYU-JIHI-TOTAL-TAX
             COMPUTE    WRK-KEISAN2           =
                        SYU-RSIJIBAI-SKYMONEY -
                        WRK-KEISAN
      *
             IF    FLG-SYUKEI   =   1
               IF  WRK-KEISAN2         NOT =  ZERO
               OR  SYU-SYUSKJ-RYOYOHI  NOT =  ZERO
                 COMPUTE  LNK-SKYGET-KENSU(IDX1 IDX2) =
                          LNK-SKYGET-KENSU(IDX1 IDX2) +
                          1
                 MOVE     ZERO    TO    FLG-SYUKEI
               END-IF
             END-IF
      *
             IF    WRK-KEISAN2  NOT =  ZERO
               EVALUATE  SYU-NYUGAIKBN
                 WHEN      "1"
                   PERFORM   VARYING   IDX3   FROM  1  BY  1
                              UNTIL    IDX3    >   31
                     IF  (SYU-DAY(IDX3)        =  "1" OR "2")
                     AND (WRK-TAIHI-DAY(IDX3)  =  ZERO      )
                       COMPUTE  LNK-SKYGET-IRYO2-NISSU(IDX1 IDX2) =
                                LNK-SKYGET-IRYO2-NISSU(IDX1 IDX2) +
                                1
                       MOVE       1     TO    WRK-TAIHI-DAY(IDX3)
                     END-IF
                   END-PERFORM
                 WHEN      "2"
                   MOVE   SYU-SRYYMD(7:2)   TO   IDX3
                   IF     WRK-TAIHI-DAY(IDX3)  =  ZERO
                       COMPUTE  LNK-SKYGET-IRYO2-NISSU(IDX1 IDX2) =
                                LNK-SKYGET-IRYO2-NISSU(IDX1 IDX2) +
                                1
                       MOVE       1     TO    WRK-TAIHI-DAY(IDX3)
                   END-IF
               END-EVALUATE
             END-IF
             COMPUTE    LNK-SKYGET-IRYO2-TOTALTEN(IDX1 IDX2) =
                        LNK-SKYGET-IRYO2-TOTALTEN(IDX1 IDX2) +
                        SYU-TOTAL-HKNTEN
             IF    SYU-PTFTNRATE    =  100
               COMPUTE  LNK-SKYGET-IRYO2-ICHIBUFTN(IDX1 IDX2) =
                        LNK-SKYGET-IRYO2-ICHIBUFTN(IDX1 IDX2) +
                        WRK-KEISAN2
             ELSE
               COMPUTE  LNK-SKYGET-IRYO2-HOKENJA(IDX1 IDX2) =
                        LNK-SKYGET-IRYO2-HOKENJA(IDX1 IDX2) +
                        WRK-KEISAN2
             END-IF
      *
             COMPUTE    LNK-SKYGET-SKJ2-KAISU(IDX1 IDX2) =
                        LNK-SKYGET-SKJ2-KAISU(IDX1 IDX2) +
                        SYU-SYUSKJ-NISSU
             COMPUTE    LNK-SKYGET-SKJ2-RYOYOHI(IDX1 IDX2) =
                        LNK-SKYGET-SKJ2-RYOYOHI(IDX1 IDX2) +
                        SYU-SYUSKJ-RYOYOHI
             IF    SYU-PTFTNRATE    =  100
               COMPUTE  LNK-SKYGET-SKJ2-ICHIBUFTN(IDX1 IDX2) =
                        LNK-SKYGET-SKJ2-ICHIBUFTN(IDX1 IDX2) +
                        SYU-SYUSKJ-RYOYOHI
             ELSE
               COMPUTE  LNK-SKYGET-SKJ2-HOKENJA(IDX1 IDX2) =
                        LNK-SKYGET-SKJ2-HOKENJA(IDX1 IDX2) +
                        SYU-SYUSKJ-RYOYOHI
             END-IF
      *
           END-IF
      *
      ***
      *
           COMPUTE      LNK-SKYGET-JIHI(IDX1 IDX2) =
                        LNK-SKYGET-JIHI(IDX1 IDX2) +
                        SYU-RMSAGAKU               +
                        SYU-SKYMONEY-SKJ-JIHI-KEI  +
                        SYU-SKYMONEY-LIFE-JIHI-KEI +
                        SYU-TOTAL-TGMONEY          +
                        SYU-TOTAL-TGMONEY-TAX      +
                        SYU-JIHI-TOTAL             +
                        SYU-JIHI-TOTAL-TAX
      *
           IF    FLG-JIHIRANSYUKEI   =   1
             COMPUTE    LNK-SKYGET-JIHI(IDX1 IDX2) =
                        LNK-SKYGET-JIHI(IDX1 IDX2) -
                        SYU-JYO-COMPFTN
           END-IF
      *
           COMPUTE      WRK-KEISAN2        =
                        SYU-DISCOUNT-MONEY *
                        -1
           COMPUTE      LNK-SKYGET-DISCOUNT-MONEY(IDX1 IDX2) =
                        LNK-SKYGET-DISCOUNT-MONEY(IDX1 IDX2) +
                        WRK-KEISAN2
      *
           COMPUTE      LNK-SKYGET-CHOSEI (IDX1 IDX2) =
                        LNK-SKYGET-CHOSEI (IDX1 IDX2) +
                        SYU-CHOSEI
      *
           COMPUTE      LNK-SKYGET-CHOSEI1(IDX1 IDX2) =
                        LNK-SKYGET-CHOSEI1(IDX1 IDX2) +
                        SYU-CHOSEI1
      *
           COMPUTE      LNK-SKYGET-CHOSEI2(IDX1 IDX2) =
                        LNK-SKYGET-CHOSEI2(IDX1 IDX2) +
                        SYU-CHOSEI2
      *
           IF    IDX1  =   7  OR  8  OR  9
             IF   SYU-PTFTNRATE   =  100
               COMPUTE  WRK-KEISAN            =
                        SYU-RSIJIBAI-SKYMONEY -
                        SYU-DISCOUNT-MONEY    +
                        SYU-CHOSEI
             ELSE
               COMPUTE  WRK-KEISAN                 =
                        SYU-RMSAGAKU               +
                        SYU-SKYMONEY-SKJ-JIHI-KEI  +
                        SYU-SKYMONEY-LIFE-JIHI-KEI +
                        SYU-TOTAL-TGMONEY          +
                        SYU-TOTAL-TGMONEY-TAX      +
                        SYU-JIHI-TOTAL             +
                        SYU-JIHI-TOTAL-TAX         -
                        SYU-DISCOUNT-MONEY         +
                        SYU-CHOSEI
             END-IF
           ELSE
             COMPUTE    WRK-KEISAN                 =
                        SYU-HKNTEKMONEY            +
                        SYU-KOH-FTN                +
                        SYU-SKYMONEY-SKJ-KEI       +
                        SYU-SKYMONEY-LIFE-KEI      +
                        SYU-RMSAGAKU               +
                        SYU-SKYMONEY-SKJ-JIHI-KEI  +
                        SYU-SKYMONEY-LIFE-JIHI-KEI +
                        SYU-TOTAL-TGMONEY          +
                        SYU-TOTAL-TGMONEY-TAX      +
                        SYU-JIHI-TOTAL             +
                        SYU-JIHI-TOTAL-TAX         -
                        SYU-DISCOUNT-MONEY         +
                        SYU-CHOSEI
           END-IF
           COMPUTE      WRK-KEISAN2  =
                        SYU-SKYMONEY -
                        WRK-KEISAN
           COMPUTE      LNK-SKYGET-HASU(IDX1 IDX2) =
                        LNK-SKYGET-HASU(IDX1 IDX2) +
                        WRK-KEISAN2
      *
           .
       20021-KINGAKU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¶èÊ¬¡É£²¡É¡Ê¥¤¥ó¥Ç¥Ã¥¯¥¹ÊÔ½¸¡Ë
      *****************************************************************
       200211-INDEX-HENSYU-SEC            SECTION.
      *
           MOVE      ZERO            TO    IDX1
      *    ¼«ÈñÊÝ¸±
           IF    SYU-SYUHKNNUM(1:2)   =   "98"
              MOVE     5             TO    IDX1
           END-IF
      *    ¼£¸³
           IF    SYU-SYUHKNNUM(1:2)   =   "90" OR "91"
              MOVE     6             TO    IDX1
           END-IF
      *    Ï«ºÒ
           IF    SYU-SYUHKNNUM        =   "971"
              MOVE     7             TO    IDX1
           END-IF
      *    ¼«ÇåÀÕ
           IF    SYU-SYUHKNNUM        =   "973"
              MOVE     8             TO    IDX1
           END-IF
      *    ¸ø³²
           IF    SYU-SYUHKNNUM        =   "975"
              MOVE     9             TO    IDX1
           END-IF
      *    ¹ñÊÝ
           IF    SYU-SYUHKNNUM   =  "060" OR "067"
              MOVE     2             TO    IDX1
           END-IF
      *    ¸å´ü¹âÎð
           IF    SYU-SYUHKNNUM   =  "039"
              MOVE     3             TO    IDX1
           END-IF
      *    ÆÃÊÌÎÅÍÜÈñ
           IF    SYU-SYUHKNNUM   =  "068" OR "069" OR "040"
              MOVE     4             TO    IDX1
           END-IF
      *    ¼ÒÊÝ
           IF    IDX1   =   ZERO
              MOVE     1             TO    IDX1
           END-IF
      *
           .
       200211-INDEX-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ýÇ¼ÆÉ¹þ
      *****************************************************************
       900-SYUNOU-READ-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸øÈñÀÁµá¸¡º÷
      *****************************************************************
       900-KOHSKY-READ-SEC        SECTION.
      *
           MOVE    KOHSKY-REC           TO  MCPDATA-REC
           MOVE    "tbl_kohsky"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           MOVE    "DBSELECT"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_kohsky"     TO  MCP-TABLE
               MOVE    "key"            TO  MCP-PATHNAME
               MOVE    "DBFETCH"        TO  MCP-FUNC
               CALL    "ORCDBMAIN"      USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
               IF      MCP-RC           =   ZERO
                   MOVE    MCPDATA-REC  TO  KOHSKY-REC
                   MOVE    ZERO         TO  FLG-KOHSKY
               ELSE
                   INITIALIZE               KOHSKY-REC
                   MOVE    1            TO  FLG-KOHSKY
               END-IF
           ELSE
               INITIALIZE                   KOHSKY-REC
               MOVE    1                TO  FLG-KOHSKY
           END-IF
           MOVE    "tbl_kohsky"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       900-KOHSKY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â£Ó£Å£Ì£Å£Ã£Ô½èÍý
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
      *
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â£Æ£Å£Ô£Ã£È½èÍý
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
      *    ¥Æ¡¼¥Ö¥ë¥¯¥í¡¼¥º½èÍý
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
      *    ¥Æ¡¼¥Ö¥ë¥¢¥¯¥»¥¹½èÍý
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
