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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCGQSUB02.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ´µ¼Ô¾È²ñ
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ´µ¼Ô¾È²ñ¥µ¥Ö¡Ê¸¡º÷£Ó£Ñ£ÌºîÀ®¡Ë
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  03/06/18    NACL¡ÝÂÀÅÄ    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      * 01.00.01     NACL-ÂÀÅÄ    04/06/21  ºÇ½ª¼õ¿ÇÆü¸¡º÷¾ò·ï½¤Àµ
      * 03.05.00     NACL-ÂÀÅÄ    07.05.23  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-JYOKEN                          PIC 9(01).
           03  FLG-JYOKEN-AND                      PIC 9(01).
           03  FLG-JYOKEN-OR                       PIC 9(01).
           03  FLG-PTBYOMEI-SQL                    PIC 9(01).
           03  FLG-TBLNAME                         PIC 9(01).
           03  FLG-RECECOM-SQL                     PIC 9(01).
           03  FLG-HKNSELPTN                       PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX0                                PIC 9(05).
           03  IDX1                                PIC 9(05).
           03  IDX2                                PIC 9(05).
           03  IDXA                                PIC 9(05).
           03  IDX-LEN                             PIC 9(05).
           03  IDX-KMK1                            PIC 9(05).
           03  IDX-KMK2                            PIC 9(05).
           03  IDX-KMK3                            PIC 9(05).
           03  IDX-SRT1                            PIC 9(05).
           03  IDX-SRT2                            PIC 9(05).
           03  IDX-SRT3                            PIC 9(05).
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-JYOKEN                          PIC 9(05).
           03  CNT-STR                             PIC 9(05).
           03  CNT-RECECOM                         PIC 9(05).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-KIGO-LEN                        PIC 9(05).
           03  WRK-NUM-LEN                         PIC 9(05).
           03  WRK-STR                             PIC X(20000).
           03  WRK-STR-LEN                         PIC 9(05).
           03  WRK-SQL-TBL-NAME                    PIC X(30).
           03  WRK-STSRYYMD                        PIC X(08).
           03  WRK-EDSRYYMD                        PIC X(08).
           03  WRK-STBIRTHDAY                      PIC X(08).
           03  WRK-EDBIRTHDAY                      PIC X(08).
           03  WRK-STLASTYMD                       PIC X(08).
           03  WRK-EDLASTYMD                       PIC X(08).
           03  WRK-STTENKIYMD                      PIC X(08).
           03  WRK-EDTENKIYMD                      PIC X(08).
           03  WRK-STTSYYMD                        PIC X(08).
           03  WRK-STTSYYMD-FROM                   PIC X(08).
           03  WRK-STTSYYMD-TO                     PIC X(08).
           03  WRK-EDTSYYMD                        PIC X(08).
           03  WRK-SQL                             PIC X(20000).
           03  WRK-SQL-PNT                         PIC 9(06).
           03  WRK-PNT                             PIC 9(06).
           03  WRK-SQL-ORDER                       PIC X(20000).
           03  WRK-SQL-ORDER-PNT                   PIC 9(06).
           03  WRK-NAME-SEQ                        PIC 9(01).
           03  WRK-KOHNUM                          PIC X(04).
           03  WRK-PAYKBN                          PIC X(02).
           03  WRK-YMD                             PIC X(08).
           03  WRK-RECECOMMENT                     PIC X(50).
           03  WRK-ITEM-G.
               05  WRK-ITEM                        PIC X(50)
                                                   OCCURS  4.
           03  WRK-KOHTBLID                        PIC X(02).
      *
      *    ¸ÇÄêÃÍÎÎ°è
       01  CONST-AREA.
           03  CONST-ITEM-AREA.
               05  CONST-ITEM-MAX          PIC 9(03) VALUE 6.
               05  CONST-ITEM-VAL.
                   07  CONST-HOSPNUM-TBL   PIC X(01) VALUE "A".
                   07  CONST-HOSPNUM       PIC X(20) VALUE "HOSPNUM".
                   07  CONST-PTID-TBL      PIC X(01) VALUE "A".
                   07  CONST-PTID          PIC X(20) VALUE "PTID".
                   07  CONST-PTNUM-TBL     PIC X(01) VALUE "H".
                   07  CONST-PTNUM         PIC X(20) VALUE "PTNUM".
                   07  CONST-KANANAME-TBL  PIC X(01) VALUE "A".
                   07  CONST-KANANAME      PIC X(20) VALUE "KANANAME".
                   07  CONST-SEX-TBL       PIC X(01) VALUE "A".
                   07  CONST-SEX           PIC X(20) VALUE "SEX".
                   07  CONST-BIRTHDAY-TBL  PIC X(01) VALUE "A".
                   07  CONST-BIRTHDAY      PIC X(20) VALUE "BIRTHDAY".
               05  CONST-ITEM-R    REDEFINES   CONST-ITEM-VAL.
                   07  CONST-ITEM-OCC      OCCURS    6.
                       09  CONST-ITEM-TBL  PIC X(01).
                       09  CONST-ITEM      PIC X(20).
           03  CONST-TABLE-AREA.
               05  CONST-TBL-A     PIC X(20) VALUE "TBL_PTINF       A".
               05  CONST-TBL-B     PIC X(20) VALUE "TBL_SRYKARRK    B".
               05  CONST-TBL-C     PIC X(20) VALUE "TBL_PTBYOMEI    C".
               05  CONST-TBL-D     PIC X(20) VALUE "TBL_PTHKNINF    D".
               05  CONST-TBL-E     PIC X(20) VALUE "TBL_PTKOHINF    E".
               05  CONST-TBL-F     PIC X(20) VALUE "TBL_RRKSRH      F".
               05  CONST-TBL-GT    PIC X(20) VALUE "TBL_SRHACT      G".
               05  CONST-TBL-H     PIC X(20) VALUE "TBL_PTNUM       H".
               05  CONST-TBL-I     PIC X(20) VALUE "TMPV_Q001       D".
               05  CONST-TBL-J1    PIC X(20) VALUE "TBL_TSYRRK      J".
               05  CONST-TBL-J2    PIC X(20) VALUE "TBL_TNKRRK      J".
               05  CONST-TBL-K     PIC X(20) VALUE "TBL_RECECOM     K".
               05  CONST-TBL-L     PIC X(20) VALUE "TBL_SEIKYU_MAIN L".
               05  CONST-TBL-M     PIC X(20) VALUE "TBL_SEIKYU_ETC  M".
           03  CONST-KMK-ITEM-AREA.
               05  FILLER          PIC X(12)   VALUE "PTNUM".
               05  FILLER          PIC X(12)   VALUE "KANANAME".
               05  FILLER          PIC X(12)   VALUE "SEX".
               05  FILLER          PIC X(12)   VALUE "BIRTHDAY".
               05  FILLER          PIC X(12)   VALUE "JKYSNUM".
           03  CONST-KMK-ITEM-R    REDEFINES   CONST-KMK-ITEM-AREA.
               05  CONST-KMK-ITEM  PIC X(12)   OCCURS  5.
      *
           03  CONST-ORDER-AREA.
               05  FILLER          PIC X(5)    VALUE "ASC".
               05  FILLER          PIC X(5)    VALUE "DESC".
           03  CONST-ORDER-R       REDEFINES   CONST-ORDER-AREA.
               05  CONST-ORDER     PIC X(5)    OCCURS  2.
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *    Ê¸»úÎóÁàºî¥µ¥Ö
           COPY    "CPORCSSTRING.INC".
      *
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "COMMON-SPA".
           COPY    "Q01SCR-SPA".
           COPY    "CPQSUB02.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           SPA-Q01FREE
           QSUB02-LNK.
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE              QSUB02-OT-AREA
                                   FLG-AREA
                                   IDX-AREA
                                   CNT-AREA
                                   WRK-AREA
      *
           PERFORM 100-JYOKEN-EDIT-SEC
      *
           MOVE    WRK-SQL         TO  QSUB02-SQL
           MOVE    WRK-SQL-ORDER   TO  QSUB02-SQL-ORDER
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    ¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       100-JYOKEN-EDIT-SEC          SECTION.
      *
           INITIALIZE                  WRK-SQL
                                       WRK-SQL-ORDER
      *
           MOVE    1               TO  WRK-SQL-PNT
                                       WRK-SQL-ORDER-PNT
      *
           IF    ( SPA-Q01-HKNKOH-JKN  =   " " )
            AND  ( SPA-Q01-GTBL (5)    =   "1" )
            AND  ( SPA-Q01-GTBL (6)    =   "1" )
               PERFORM 1001-JYOKEN-EDIT-SEC
           ELSE
               PERFORM 1002-JYOKEN-EDIT-SEC
           END-IF
      *
           STRING  ") q_reference"
           INTO    WRK-SQL
           WITH    POINTER   WRK-SQL-PNT
           END-STRING
      *
           PERFORM 1001-GROUP-EDIT-SEC
      *
           PERFORM 1001-ORDER-EDIT-SEC
      *
           MOVE    SPACE               TO  WRK-SQL (WRK-SQL-PNT:)
      *
           .
       100-JYOKEN-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-JYOKEN-EDIT-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-TBLNAME
           MOVE    ZERO            TO  FLG-HKNSELPTN
           MOVE    SPACE           TO  WRK-KOHTBLID
           PERFORM 10011-JYOKEN-INIT-SEC
      *
           STRING  " FROM ("       DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER   WRK-SQL-PNT
           END-STRING
      *
           MOVE    WRK-SQL-PNT     TO  WRK-PNT
      *
           MOVE    "1"             TO  SPA-Q01-GTBL (5)
           MOVE    SPACE           TO  SPA-Q01-GTBL (6)
      *
           MOVE    "@@"            TO  WRK-KOHTBLID
           PERFORM 1001-JYOKEN-INIT-SEC
      *
           PERFORM 1001-FROM-EDIT-SEC
      *
           PERFORM 1001-WHERE-EDIT-SEC
      *
           STRING  "UNION "        DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER   WRK-SQL-PNT
           END-STRING
      *
           INITIALIZE              FLG-AREA
                                   CNT-AREA
      *
           MOVE    SPACE           TO  SPA-Q01-GTBL (5)
           MOVE    "1"             TO  SPA-Q01-GTBL (6)
      *
           MOVE    "@@"            TO  WRK-KOHTBLID
           PERFORM 1001-JYOKEN-INIT-SEC
      *
           PERFORM 1001-FROM-EDIT-SEC
      *
           PERFORM 1001-WHERE-EDIT-SEC
      *
           MOVE    "1"             TO  SPA-Q01-GTBL (5)
           MOVE    "1"             TO  SPA-Q01-GTBL (6)
      *
           .
       1001-JYOKEN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1002-JYOKEN-EDIT-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-TBLNAME
           MOVE    ZERO            TO  FLG-HKNSELPTN
           MOVE    SPACE           TO  WRK-KOHTBLID
           PERFORM 10011-JYOKEN-INIT-SEC
      *
           STRING  " FROM ("
           INTO    WRK-SQL
           WITH    POINTER   WRK-SQL-PNT
           END-STRING
      *
           MOVE    WRK-SQL-PNT     TO  WRK-PNT
      *
           MOVE    "@@"            TO  WRK-KOHTBLID
           IF    ( SPA-Q01-GTBL (6)  =   "1" )
               IF    ( SPA-Q01-GTBL (8)  =   "1" )
                   IF    ( SPA-Q01-KOHNUM-JKN      =   "1" )
                    AND  ( SPA-Q01-KOHNUM-G    NOT =   SPACE )
                       PERFORM VARYING IDX2    FROM    1   BY  1
                               UNTIL ( IDX2    >   5 )
                                OR   ( WRK-KOHTBLID    NOT =   "@@" )
      *
                           IF    (SPA-Q01-KOHNUM (IDX2)  NOT =   SPACE )
                               MOVE    SPACE   TO  WRK-KOHTBLID
                               MOVE    IDX2    TO  WRK-NAME-SEQ
                               STRING  "D"           DELIMITED  BY  SIZE
                                       WRK-NAME-SEQ  DELIMITED  BY  SIZE
                               INTO    WRK-KOHTBLID
                               END-STRING
                           END-IF
      *
                       END-PERFORM
                   ELSE
                       MOVE    "D"         TO  WRK-KOHTBLID
                   END-IF
               ELSE
                   IF    ( SPA-Q01-KOHNUM-JKN      =   "1" )
                    AND  ( SPA-Q01-KOHNUM-G    NOT =   SPACE )
                       PERFORM VARYING IDX2    FROM    1   BY  1
                               UNTIL ( IDX2    >   5 )
                                OR   ( WRK-KOHTBLID    NOT =   "@@" )
      *
                           IF    (SPA-Q01-KOHNUM (IDX2)  NOT =   SPACE )
                               MOVE    SPACE   TO  WRK-KOHTBLID
                               MOVE    IDX2    TO  WRK-NAME-SEQ
                               STRING  "E"           DELIMITED  BY  SIZE
                                       WRK-NAME-SEQ  DELIMITED  BY  SIZE
                               INTO    WRK-KOHTBLID
                               END-STRING
                           END-IF
      *
                       END-PERFORM
                   ELSE
                       MOVE    "E"         TO  WRK-KOHTBLID
                   END-IF
               END-IF
           END-IF
      *
           PERFORM 1001-JYOKEN-INIT-SEC
      *
           PERFORM 1001-FROM-EDIT-SEC
      *
           PERFORM 1001-WHERE-EDIT-SEC
      *
           .
       1002-JYOKEN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¸¡º÷¾ò·ïÊÔ½¸½é´ü½èÍý
      *****************************************************************
       1001-JYOKEN-INIT-SEC            SECTION.
      *
           MOVE    1               TO  FLG-TBLNAME
           PERFORM 10011-JYOKEN-INIT-SEC
      *
           STRING  " FROM TBL_PTINF A,TBL_PTNUM H "
                                       DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER   WRK-SQL-PNT
           END-STRING
      *
           IF      ( SPA-Q01-STSRYYMD  =   SPACE )
               MOVE    ALL "0"     TO  WRK-STSRYYMD
           ELSE
               MOVE    SPA-Q01-STSRYYMD
                                   TO  WRK-STSRYYMD
           END-IF
      *
           IF      ( SPA-Q01-EDSRYYMD  =   SPACE )
               MOVE    ALL "9"     TO  WRK-EDSRYYMD
           ELSE
               MOVE    SPA-Q01-EDSRYYMD
                                   TO  WRK-EDSRYYMD
           END-IF
      *
           IF      ( SPA-Q01-STBIRTHDAY
                                       =   SPACE )
               MOVE    ALL "0"     TO  WRK-STBIRTHDAY
           ELSE
               MOVE    SPA-Q01-STBIRTHDAY
                                   TO  WRK-STBIRTHDAY
           END-IF
      *
           IF      ( SPA-Q01-EDBIRTHDAY
                                       =   SPACE )
               MOVE    ALL "9"     TO  WRK-EDBIRTHDAY
           ELSE
               MOVE    SPA-Q01-EDBIRTHDAY
                                   TO  WRK-EDBIRTHDAY
           END-IF
      *
           IF      ( SPA-Q01-STLASTYMD =   SPACE )
               MOVE    ALL "0"     TO  WRK-STLASTYMD
           ELSE
               MOVE    SPA-Q01-STLASTYMD
                                   TO  WRK-STLASTYMD
           END-IF
      *
           IF      ( SPA-Q01-EDLASTYMD =   SPACE )
               MOVE    ALL "9"     TO  WRK-EDLASTYMD
           ELSE
               MOVE    SPA-Q01-EDLASTYMD
                                   TO  WRK-EDLASTYMD
           END-IF
      *
           IF      ( SPA-Q01-STTENKIYMD =   SPACE )
               MOVE    ALL "0"     TO  WRK-STTENKIYMD
           ELSE
               MOVE    SPA-Q01-STTENKIYMD
                                   TO  WRK-STTENKIYMD
           END-IF
      *
           IF      ( SPA-Q01-EDTENKIYMD =   SPACE )
               MOVE    ALL "9"     TO  WRK-EDTENKIYMD
           ELSE
               MOVE    SPA-Q01-EDTENKIYMD
                                   TO  WRK-EDTENKIYMD
           END-IF
      *
           EVALUATE    SPA-Q01-STTSYSKBT
           WHEN    "2"
               CONTINUE
           WHEN    "3"
               CONTINUE
           WHEN    OTHER
               EVALUATE    TRUE
               WHEN  ( SPA-Q01-STTSYYM     NOT =   SPACE )
                   MOVE    SPA-Q01-STTSYYM TO  WRK-STTSYYMD
                   IF    ( SPA-Q01-STTSYSKBT   =   "1" )
                       MOVE    "01"        TO  WRK-STTSYYMD(7:2)
                   END-IF
               WHEN  ( SPA-Q01-STSRYYMD    NOT =   SPACE )
      *            MOVE    SPA-Q01-STSRYYMD TO WRK-STTSYYMD
      *            MOVE    "01"            TO  WRK-STTSYYMD(7:2)
                   MOVE    SPA-Q01-STSRYYMD TO WRK-STTSYYMD
               WHEN    OTHER
                   MOVE    ALL "0"         TO  WRK-STTSYYMD
               END-EVALUATE
      *
               EVALUATE    TRUE
               WHEN  ( SPA-Q01-EDTSYYM     NOT =   SPACE )
                   IF    ( SPA-Q01-STTSYSKBT   =   "1" )
                       MOVE    SPA-Q01-EDTSYYM TO  WRK-YMD
                       PERFORM 10011-GET-GETUMATU-SEC
                       MOVE    LNK-DAY7-KOYOMI TO  WRK-EDTSYYMD
                   ELSE
                       MOVE    SPA-Q01-EDTSYYM TO  WRK-EDTSYYMD
                   END-IF
               WHEN  ( SPA-Q01-EDSRYYMD    NOT =   SPACE )
      *            MOVE    SPA-Q01-EDSRYYMD TO WRK-YMD
      *            PERFORM 10011-GET-GETUMATU-SEC
      *            MOVE    LNK-DAY7-KOYOMI TO  WRK-EDTSYYMD
                   MOVE    SPA-Q01-EDSRYYMD TO WRK-EDTSYYMD
               WHEN    OTHER
                   MOVE    ALL "9"         TO  WRK-EDTSYYMD
               END-EVALUATE
      *
           END-EVALUATE
      *
           .
       1001-JYOKEN-INIT-EXT.
           EXIT.
      *****************************************************************
      *    ¸¡º÷¾ò·ïÊÔ½¸½é´ü½èÍý
      *****************************************************************
       10011-JYOKEN-INIT-SEC           SECTION.
      *
           IF    ( FLG-TBLNAME     =   ZERO )
      *
               STRING    "INSERT   INTO  TBL_PTSRH ("
                                              DELIMITED    BY SIZE
                         "HOSPNUM,"           DELIMITED    BY SIZE
                         "TERMID,"            DELIMITED    BY SIZE
                         "PTID,"              DELIMITED    BY SIZE
                         "PTNUM,"             DELIMITED    BY SIZE
                         "KANANAME,"          DELIMITED    BY SIZE
                         "SEX,"               DELIMITED    BY SIZE
                         "BIRTHDAY,"          DELIMITED    BY SIZE
                         "DATAKBN,"           DELIMITED    BY SIZE
                         "CREYMD,"            DELIMITED    BY SIZE
                         "UPYMD,"             DELIMITED    BY SIZE
                         "UPHMS,"             DELIMITED    BY SIZE
                         "JKYSNUM"            DELIMITED    BY SIZE
                         ")"                  DELIMITED    BY SIZE
                         "SELECT "            DELIMITED    BY SIZE
                         "HOSPNUM,'"          DELIMITED    BY SIZE
                         SPA-TERMID           DELIMITED    BY SPACE
                         "',PTID,"            DELIMITED    BY SIZE
                         "PTNUM,"             DELIMITED    BY SIZE
                         "KANANAME,"          DELIMITED    BY SIZE
                         "SEX,"               DELIMITED    BY SIZE
                         "BIRTHDAY,"          DELIMITED    BY SIZE
                         "'1',"               DELIMITED    BY SIZE
                         "'"                  DELIMITED    BY SIZE
                         QSUB02-YMD           DELIMITED    BY SPACE
                         "',"                 DELIMITED    BY SIZE
                         "'"                  DELIMITED    BY SIZE
                         QSUB02-YMD           DELIMITED    BY SPACE
                         "',"                 DELIMITED    BY SIZE
                         "'"                  DELIMITED    BY SIZE
                         QSUB02-HMS           DELIMITED    BY SPACE
                         "',"                 DELIMITED    BY SIZE
               INTO    WRK-SQL
               WITH    POINTER   WRK-SQL-PNT
               END-STRING
           ELSE
               STRING    "SELECT "            DELIMITED    BY SIZE
                         "A.HOSPNUM,"         DELIMITED    BY SIZE
                         "A.PTID,"            DELIMITED    BY SIZE
                         "H.PTNUM,"           DELIMITED    BY SIZE
                         "A.KANANAME,"        DELIMITED    BY SIZE
                         "A.SEX,"             DELIMITED    BY SIZE
                         "A.BIRTHDAY,"        DELIMITED    BY SIZE
               INTO    WRK-SQL
               WITH    POINTER   WRK-SQL-PNT
               END-STRING
           END-IF
      *
           EVALUATE    WRK-KOHTBLID
           WHEN    SPACE
               STRING  "SUBSTR(MAX(JKYSNUM),17,20) AS JKYSNUM"
                                                       DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER   WRK-SQL-PNT
               END-STRING
           WHEN    "@@"
               STRING  "CAST('0' AS TEXT) AS JKYSNUM"
                                                       DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER   WRK-SQL-PNT
               END-STRING
           WHEN    OTHER
               STRING  "CAST("              DELIMITED BY SIZE
                       WRK-KOHTBLID          DELIMITED BY SPACE
                       ".TEKSTYMD||SUBSTR("  DELIMITED BY SIZE
                       WRK-KOHTBLID          DELIMITED BY SPACE
                       ".FTNJANUM||'00000000',1,8)||"
                                             DELIMITED BY SIZE
                       WRK-KOHTBLID          DELIMITED BY SPACE
                       ".JKYSNUM AS TEXT)AS JKYSNUM"
                                             DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER   WRK-SQL-PNT
               END-STRING
           END-EVALUATE
      *
           .
       10011-JYOKEN-INIT-EXT.
           EXIT.
      *****************************************************************
      *    Á°·î¼èÆÀ½èÍý
      *****************************************************************
       10011-GET-ZENGETU-SEC           SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                  TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
      *
           .
       10011-GET-ZENGETU-EXT.
           EXIT.
      *****************************************************************
      *    ·îËö¼èÆÀ½èÍý
      *****************************************************************
       10011-GET-GETUMATU-SEC          SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-YMD (1 : 6)     TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
      *
           .
       10011-GET-GETUMATU-EXT.
           EXIT.
      *****************************************************************
      *    FROM¶çÊÔ½¸½èÍý
      *****************************************************************
       1001-FROM-EDIT-SEC              SECTION.
      *
      *    Äã½êÆÀ
           IF  ( SPA-Q01-GTBL (10) =   "1" )
               PERFORM 1001-TSY-JYOKEN-EDIT-SEC
           END-IF
      *
      *    ºÇ½ª¼õ¿ÇÆü
           IF  ( SPA-Q01-GTBL (3)  =   "1" )
               STRING  ",(SELECT HOSPNUM,PTID FROM "
                                                   DELIMITED  BY  SIZE
                       "TBL_SRYKARRK "             DELIMITED  BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
      *
               IF    ( SPA-Q01-SRYKA   NOT =   SPACE )
                   STRING  "WHERE SRYKA ='"        DELIMITED  BY  SIZE
                           SPA-Q01-SRYKA           DELIMITED  BY  SPACE
                           "' "                    DELIMITED  BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
      *
               STRING  "GROUP BY HOSPNUM,PTID  "   DELIMITED  BY  SIZE
                       "HAVING MAX(LASTYMD) BETWEEN '"
                                                   DELIMITED  BY  SIZE
                       WRK-STLASTYMD               DELIMITED  BY  SPACE
                       "' AND '"                   DELIMITED  BY  SIZE
                       WRK-EDLASTYMD               DELIMITED  BY  SPACE
                       "') B"                      DELIMITED  BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
      *    ÉÂÌ¾¥³¡¼¥É
           IF  ( SPA-Q01-GTBL (4)  =   "1" )
               PERFORM 1001-BYOMEI-JYOKEN-EDIT-SEC
           END-IF
      *
           IF      ((  SPA-Q01-GTBL (5)    =   "1" )
                OR  (  SPA-Q01-GTBL (6)    =   "1" ))
            AND    (   SPA-Q01-GTBL (8)    =   "1" )
               PERFORM 1001-TMPVQ001-JYOKEN-EDIT-SEC
      *
           ELSE
      *        ÊÝ¸±µ­¹æ¡¦ÈÖ¹æ
               IF   ( SPA-Q01-GTBL (5)  =   "1" )
                   MOVE    1               TO  FLG-HKNSELPTN
                   STRING  ",(SELECT AA.HOSPNUM,AA.PTID,AA.DELKBN"
                                                   DELIMITED  BY  SIZE
                           ",BB.HKNJANUM,BB.HONKZKKBN,BB.HKNNUM"
                                                   DELIMITED  BY  SIZE
                           ",AA.HKNID,BB.KIGO,BB.NUM,AA.HKNCOMBINUM "
                                                   DELIMITED  BY  SIZE
                           "FROM TBL_HKNCOMBI AA"
                                                   DELIMITED  BY  SIZE
                           " LEFT JOIN TBL_PTHKNINF BB ON "
                                                   DELIMITED  BY  SIZE
                           "(AA.HOSPNUM=BB.HOSPNUM AND AA.PTID=BB.PTID"
                                                   DELIMITED  BY  SIZE
                           " AND AA.HKNID = BB.HKNID)) D"
                                                   DELIMITED  BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
      *
      *        ¸øÈñÊÝ¸±µ­¹æÈÖ¹æ¡¦ÉéÃ´¼ÔÈÖ¹æ
               IF   ( SPA-Q01-GTBL (6)  =   "1" )
                   PERFORM 1001-PTKOHINF-JYOKEN-EDIT-SEC
               END-IF
           END-IF
      *
      *    ¿Ç»¡Ç¯·îÆü
           IF   ( SPA-Q01-GTBL (8)  =   "1" )
               IF    ( SPA-Q01-GTBL (5)  =   "1" )
                OR   ( SPA-Q01-GTBL (6)  =   "1" )
                OR   ( SPA-Q01-GTBL (9)  =   "1" )
                   CONTINUE
               ELSE
                   STRING  ",TBL_RRKSRH F"         DELIMITED  BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
           END-IF
      *
      *    ¿ÇÎÅ¹Ô°Ù¥³¡¼¥É
           IF   ( SPA-Q01-GTBL (9)  =   "1" )
               PERFORM 1001-SRYACT-JYOKEN-EDIT-SEC
           END-IF
      *
      *    ¥ì¥»¥×¥È¥³¥á¥ó¥È
           IF   ( SPA-Q01-GTBL (11)  =   "1" )
               PERFORM 1001-RECECOM-JYOKEN-EDIT-SEC
           END-IF
      *
      *    ÀÁµá´ÉÍý¥Æ¡¼¥Ö¥ë
           IF   ( SPA-Q01-GTBL (12)  =   "1" )
               PERFORM 1001-SEIKYU-MAIN-JYOKEN-EDIT-SEC
           END-IF
      *
           .
       1001-FROM-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    WHERE¶çÊÔ½¸½èÍý
      *****************************************************************
       1001-WHERE-EDIT-SEC             SECTION.
      *
           STRING  " WHERE A.HOSPNUM="          DELIMITED  BY  SIZE
                   SPA-HOSPNUM                  DELIMITED  BY  SIZE
                   " AND "                      DELIMITED  BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
      *    Äã½êÆÀ
           IF  ( SPA-Q01-GTBL (10)  =   "1" )
               MOVE    FLG-JYOKEN      TO  FLG-JYOKEN-AND
               PERFORM     1001-TSY-SQL-EDIT-SEC
               MOVE    1               TO  FLG-JYOKEN
           END-IF
      *
      *    ¥«¥Ê»áÌ¾¡¦´Á»ú»áÌ¾
           IF  ( SPA-Q01-GTBL (2)  =   "1" )
               MOVE    FLG-JYOKEN      TO  FLG-JYOKEN-AND
               PERFORM     1001-PTINF-SQL-EDIT2-SEC
               MOVE    1               TO  FLG-JYOKEN
           END-IF
      *
      *    ºÇ½ª¼õ¿ÇÆü
           IF  ( SPA-Q01-GTBL (3)  =   "1" )
               MOVE    ZERO            TO  FLG-JYOKEN-AND
               PERFORM     1001-SRYKARRK-SQL-EDIT-SEC
               MOVE    1               TO  FLG-JYOKEN
           END-IF
      *
      *    ÉÂÌ¾¥³¡¼¥É
           IF  ( SPA-Q01-GTBL (4)  =   "1" )
               IF    ( SPA-Q01-BYOMEI-JKN  =   "1" )
                 AND ( SPA-Q01-BYOMEI-G    NOT =   SPACE )
                   PERFORM 1001-PTBYOMEI-SQL-AND-EDIT-SEC
               ELSE
                   MOVE    ZERO            TO  FLG-JYOKEN-AND
                   PERFORM 1001-PTBYOMEI-SQL-OR-EDIT-SEC
                   MOVE    1               TO  FLG-JYOKEN
               END-IF
           END-IF
      *
      *    ÊÝ¸±µ­¹æ¡¦ÈÖ¹æ¡¦¸øÈñÊÝ¸±µ­¹æÈÖ¹æ¡¦ÉéÃ´¼ÔÈÖ¹æ
           IF      ((  SPA-Q01-GTBL (5)    =   "1" )
                OR  (  SPA-Q01-GTBL (6)    =   "1" ))
               IF  (   SPA-Q01-GTBL (8)    =   "1" )
                   IF    ( SPA-Q01-KOHNUM-JKN      =   "1" )
                    AND  ( SPA-Q01-KOHNUM-G    NOT =   SPACE )
                       PERFORM 1001-TMPVQ001-SQL-AND-EDIT-SEC
                   ELSE
                       MOVE    ZERO        TO  FLG-JYOKEN-AND
                       PERFORM 1001-TMPVQ001-SQL-OR-EDIT-SEC
                       MOVE    1           TO  FLG-JYOKEN
                   END-IF
               ELSE
                   IF   ( SPA-Q01-GTBL (5)  =   "1" )
                       MOVE    ZERO        TO  FLG-JYOKEN-AND
                       PERFORM 1001-PTHKNINF-SQL-EDIT-SEC
                       MOVE    1           TO  FLG-JYOKEN
                   END-IF
      *
                   IF   ( SPA-Q01-GTBL (6)  =   "1" )
                       IF    ( SPA-Q01-KOHNUM-JKN      =   "1" )
                        AND  ( SPA-Q01-KOHNUM-G    NOT =   SPACE )
                           PERFORM 1001-PTKOHINF-SQL-AND-EDIT-SEC
                       ELSE
                           MOVE    ZERO        TO  FLG-JYOKEN-AND
                           PERFORM 1001-PTKOHINF-SQL-OR-EDIT-SEC
                           MOVE    1           TO  FLG-JYOKEN
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    À¸Ç¯·îÆü¡¦À­ÊÌ¡¦½»½ê¡¦¾õÂÖ£±¡¦¾õÂÖ£²¡¦¾õÂÖ£³¡¦¥³¥á¥ó¥È¡¦
      *    ÅÅÏÃÈÖ¹æ¡¦¸ºÌÈ»öÍ³
           IF   ( SPA-Q01-GTBL (7)  =   "1" )
               MOVE    FLG-JYOKEN      TO  FLG-JYOKEN-AND
               PERFORM     1001-PTINF-SQL-EDIT3-SEC
               MOVE    1               TO  FLG-JYOKEN
           END-IF
      *
      *    ¥Æ¥¹¥È´µ¼Ô¶èÊ¬
           IF    ( SPA-Q01-CHKTSTPT    =   "F" )
               MOVE    FLG-JYOKEN      TO  FLG-JYOKEN-AND
               PERFORM     1001-PTINF-SQL-EDIT4-SEC
               MOVE    1               TO  FLG-JYOKEN
           END-IF
      *
      *    ¿Ç»¡Ç¯·îÆü
           IF   ( SPA-Q01-GTBL (8)  =   "1" )
               IF    ( SPA-Q01-GTBL (5)  =   "1" )
                OR   ( SPA-Q01-GTBL (6)  =   "1" )
                OR   ( SPA-Q01-GTBL (9)  =   "1" )
                   CONTINUE
               ELSE
                   MOVE    ZERO            TO  FLG-JYOKEN-AND
                   PERFORM     1001-JYURRK-SQL-EDIT-SEC
                   MOVE    1               TO  FLG-JYOKEN
               END-IF
           END-IF
      *
      *    ¿ÇÎÅ¶èÊ¬¡¦¿ÇÎÅ¹Ô°Ù¥³¡¼¥É
           IF   ( SPA-Q01-GTBL (9)  =   "1" )
               IF    ( SPA-Q01-SRYACT-JKN  =   "1" )
                AND  ( SPA-Q01-SRYCD-G NOT =   SPACE )
                   PERFORM     1001-SRYACT-SQL-AND-EDIT-SEC
               ELSE
                   MOVE    ZERO            TO  FLG-JYOKEN-AND
                   PERFORM     1001-SRYACT-SQL-OR-EDIT-SEC
                   MOVE    1               TO  FLG-JYOKEN
               END-IF
           END-IF
      *
      *    ¥ì¥»¥×¥È¥³¥á¥ó¥È
           IF  ( SPA-Q01-GTBL (11)     =   "1" )
               MOVE    FLG-JYOKEN      TO  FLG-JYOKEN-AND
               PERFORM     1001-RECECOM-SQL-EDIT-SEC
               MOVE    1               TO  FLG-JYOKEN
           END-IF
      *
      *    ÆÃµ­»ö¹à
           IF  ( SPA-Q01-GTBL (12)     =   "1" )
               MOVE    FLG-JYOKEN      TO  FLG-JYOKEN-AND
               PERFORM     1001-TOKKI-SQL-EDIT-SEC
               MOVE    1               TO  FLG-JYOKEN
           END-IF
      *
      *    ´µ¼ÔÈÖ¹æ
           MOVE    ZERO            TO  FLG-JYOKEN-AND
           PERFORM 1001-PTNUM-SQL-EDIT-SEC
           MOVE    1               TO  FLG-JYOKEN
      *
           .
       1001-WHERE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    Äã½êÆÀ¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-TSY-JYOKEN-EDIT-SEC        SECTION.
      *
           EVALUATE    SPA-Q01-TSY
           WHEN    "1"
               STRING  ",TBL_TNKRRK J "            DELIMITED  BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           WHEN    "2"
               STRING  ",TBL_TSYRRK J "            DELIMITED  BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           WHEN    "3"
               STRING  ",(SELECT HOSPNUM,PTID,STYMD,EDYMD"
                                                   DELIMITED  BY  SIZE
                       " FROM TBL_TNKRRK UNION ALL SELECT HOSPNUM,PTID,"
                                                   DELIMITED  BY  SIZE
                       "NINSTYMD AS STYMD,NINEDYMD AS EDYMD"
                                                   DELIMITED  BY  SIZE
                       " FROM TBL_TSYRRK) J "
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-EVALUATE
      *
           .
       1001-TSY-JYOKEN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¾È²ñ¥Ó¥å¡¼¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-TMPVQ001-JYOKEN-EDIT-SEC   SECTION.
      *
           IF    ( SPA-Q01-KOHNUM-JKN      =   "1" )
            AND  ( SPA-Q01-KOHNUM-G    NOT =   SPACE )
               MOVE    2               TO  FLG-HKNSELPTN
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   5 )
      *
                   IF    ( SPA-Q01-KOHNUM (IDX2)   NOT =   SPACE )
                       MOVE    IDX2    TO  WRK-NAME-SEQ
                       STRING  ",TMPV_Q001 D"      DELIMITED  BY  SIZE
                               WRK-NAME-SEQ        DELIMITED  BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   END-IF
      *
               END-PERFORM
           ELSE
               MOVE    1               TO  FLG-HKNSELPTN
               STRING  ",TMPV_Q001 D"          DELIMITED  BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           .
       1001-TMPVQ001-JYOKEN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼Ô¸øÈñ¾ðÊó¥Æ¡¼¥Ö¥ë¸¡º÷¾ðÊóÊÔ½¸½èÍý
      *****************************************************************
       1001-PTKOHINF-JYOKEN-EDIT-SEC   SECTION.
      *
           IF    ( SPA-Q01-KOHNUM-JKN      =   "1" )
            AND  ( SPA-Q01-KOHNUM-G    NOT =   SPACE )
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   5 )
      *
                   IF    (SPA-Q01-KOHNUM (IDX2)    NOT =   SPACE )
                       MOVE    IDX2    TO  WRK-NAME-SEQ
                       STRING  ",TBL_PTKOHINF E"   DELIMITED  BY  SIZE
                               WRK-NAME-SEQ        DELIMITED  BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   END-IF
      *
               END-PERFORM
           ELSE
               STRING  ",TBL_PTKOHINF E"   DELIMITED  BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           .
      *
           .
       1001-PTKOHINF-JYOKEN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÌ¾¥Æ¡¼¥Ö¥ë¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-BYOMEI-JYOKEN-EDIT-SEC     SECTION.
      *
           IF    ( SPA-Q01-BYOMEI-JKN      =   "1" )
             AND ( SPA-Q01-BYOMEI-G    NOT =   SPACE )
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   5 )
      *
                   IF    (SPA-Q01-BYOMEI (IDX2)   NOT =   SPACE )
                       MOVE    IDX2    TO  WRK-NAME-SEQ
                       STRING  ",TBL_PTBYOMEI C"   DELIMITED  BY  SIZE
                               WRK-NAME-SEQ        DELIMITED  BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   END-IF
      *
               END-PERFORM
           ELSE
               STRING  ",TBL_PTBYOMEI C"       DELIMITED  BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           .
       1001-BYOMEI-JYOKEN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¥Æ¡¼¥Ö¥ë¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-SRYACT-JYOKEN-EDIT-SEC     SECTION.
      *
           IF    ( SPA-Q01-SRYACT-JKN  =   "1" )
            AND  ( SPA-Q01-SRYCD-G NOT =   SPACE )
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   5 )
      *
                   IF    (SPA-Q01-SRYCD (IDX2) NOT =   SPACE )
                       IF    ( SPA-Q01-GTBL(8) =   SPACE )
                           MOVE    IDX2    TO  WRK-NAME-SEQ
                           STRING  ",TBL_SRYSRH G" DELIMITED  BY  SIZE
                                   WRK-NAME-SEQ    DELIMITED  BY  SIZE
                           INTO    WRK-SQL
                           WITH    POINTER WRK-SQL-PNT
                           END-STRING
                       ELSE
                           MOVE    IDX2    TO  WRK-NAME-SEQ
                           STRING  ",TBL_SRYSRH G"  DELIMITED  BY  SIZE
                                   WRK-NAME-SEQ    DELIMITED  BY  SIZE
                           INTO    WRK-SQL
                           WITH    POINTER WRK-SQL-PNT
                           END-STRING
                       END-IF
                   END-IF
      *
               END-PERFORM
           ELSE
               IF    ( SPA-Q01-GTBL(8) =   SPACE )
                   STRING  ",TBL_SRYSRH G"         DELIMITED  BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               ELSE
                   STRING  ",TBL_SRYSRH G"          DELIMITED  BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
           END-IF
      *
           .
       1001-SRYACT-JYOKEN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¥ì¥»¥×¥È¥³¥á¥ó¥È¥Æ¡¼¥Ö¥ë¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-RECECOM-JYOKEN-EDIT-SEC     SECTION.
      *
           STRING  ",TBL_RECECOM K"         DELIMITED  BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
           .
       1001-RECECOM-JYOKEN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÀÁµá´ÉÍý¥Æ¡¼¥Ö¥ë¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-SEIKYU-MAIN-JYOKEN-EDIT-SEC     SECTION.
      *
           STRING  ",TBL_SEIKYU_MAIN L"     DELIMITED  BY  SIZE
                   " LEFT JOIN TBL_SEIKYU_ETC M USING"
                                            DELIMITED  BY  SIZE
                   " (HOSPNUM, SRYYM, NYUGAIKBN, PTID, RECEKA"
                                            DELIMITED  BY  SIZE
                   ",TEISYUTUSAKI, RECESYUBETU, HKNJANUM, HOJOKBN_KEY"
                                            DELIMITED  BY  SIZE
                   ",TEKSTYMD)"
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
           .
       1001-SEIKYU-MAIN-JYOKEN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¥ì¥»¥×¥È¥³¥á¥ó¥È¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-RECECOM-SQL-EDIT-SEC       SECTION.
      *
           MOVE    "K"             TO  WRK-SQL-TBL-NAME
           PERFORM 900-SQL-JYOKEN-EDIT1-SEC
      *
           STRING  "(K.SRYYM >= '"         DELIMITED   BY  SIZE
                   WRK-STSRYYMD(1:6)       DELIMITED   BY  SPACE
                   "' AND K.SRYYM <= '"    DELIMITED   BY  SIZE
                   WRK-EDSRYYMD(1:6)       DELIMITED   BY  SPACE
                   "')"                    DELIMITED   BY  SIZE
           INTO        WRK-SQL
           WITH POINTER    WRK-SQL-PNT
           END-STRING
      *
      *
           IF    ( SPA-Q01-NYUGAIKBN   NOT =   SPACE )
               PERFORM 10011-NYUGAIKBN-SQL-EDIT-SEC
           END-IF
      *
           IF    ( SPA-Q01-SRYKA   NOT =   SPACE )
               MOVE    1           TO  FLG-RECECOM-SQL
               PERFORM 10011-SRYKA-SQL-EDIT-SEC
           END-IF
      *
           IF    ( SPA-Q01-GTBL (5)    =   "1" )
      *
               MOVE    1                   TO  CNT-JYOKEN
      *
               PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
               IF    ( FLG-HKNSELPTN           =   2 )
                   STRING  WRK-SQL-TBL-NAME        DELIMITED BY SPACE
                           ".HKNCOMBI in (0"  DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL ( IDX1    >   5 )
      *
                       IF    (SPA-Q01-KOHNUM (IDX1)    NOT =   SPACE )
                           MOVE    IDX1    TO  WRK-NAME-SEQ
                           STRING  ",D"            DELIMITED  BY  SIZE
                                   WRK-NAME-SEQ    DELIMITED  BY  SIZE
                                   ".HKNCOMBINUM"  DELIMITED  BY  SIZE
                           INTO    WRK-SQL
                           WITH    POINTER WRK-SQL-PNT
                           END-STRING
                       END-IF
                   END-PERFORM
                   STRING  ")"     DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               ELSE
                   STRING  WRK-SQL-TBL-NAME        DELIMITED BY SPACE
                           ".HKNCOMBI = D.HKNCOMBINUM"
                                                   DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
      *
               PERFORM 900-SQL-JYOKEN-EDIT3-SEC
           END-IF
      *
           MOVE    ZERO    TO      CNT-JYOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   3 )
      *
                MOVE    SPACE     TO  WRK-ITEM-G
      *
                MOVE   SPA-Q01-RECECOM (IDX1)
                                  TO   WRK-RECECOMMENT
      *
               IF    ( WRK-RECECOMMENT
                                   NOT =   SPACE )
                   MOVE    "gsub"          TO  ORCSSTR-COMMAND
                   MOVE    WRK-RECECOMMENT
                                           TO  ORCSSTR-STRING1
                   MOVE    "¡¡"            TO  ORCSSTR-STRING2
                   MOVE    "**"            TO  ORCSSTR-STRING3
                   CALL    "orcsstring" USING  ORCSSTRINGAREA
                   MOVE    ORCSSTR-STRING1 TO  WRK-RECECOMMENT
               END-IF
               INSPECT WRK-RECECOMMENT REPLACING ALL "**"  BY  "  "
               UNSTRING    WRK-RECECOMMENT    DELIMITED  BY  ALL " "
                                   INTO       WRK-ITEM(1)
                                              WRK-ITEM(2)
                                              WRK-ITEM(3)
                                              WRK-ITEM(4)
               END-UNSTRING
      *
               IF  ( WRK-ITEM-G        NOT =   SPACE)
      *
                   COMPUTE CNT-JYOKEN  =   CNT-JYOKEN  +   1
      *
                   PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
                   STRING  "("                 DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
      *
                   MOVE    ZERO            TO  CNT-RECECOM
                   PERFORM VARYING IDX2    FROM    1   BY  1
                           UNTIL ( IDX2    >   4 )
                       IF    ( WRK-ITEM (IDX2)     NOT =   SPACE )
                           COMPUTE CNT-RECECOM =   CNT-RECECOM +   1
                           IF    ( CNT-RECECOM =   1 )
                               CONTINUE
                           ELSE
                               STRING  "AND"   DELIMITED   BY  SIZE
                               INTO    WRK-SQL
                               WITH    POINTER WRK-SQL-PNT
                               END-STRING
                           END-IF
                           STRING  "("             DELIMITED   BY  SIZE
                               WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                               ".COMMENT "         DELIMITED   BY  SIZE
                               "LIKE '%"           DELIMITED   BY  SIZE
                               WRK-ITEM(IDX2)      DELIMITED   BY  SPACE
                               "%')"               DELIMITED   BY  SIZE
                           INTO    WRK-SQL
                           WITH    POINTER WRK-SQL-PNT
                           END-STRING
                       END-IF
                   END-PERFORM
      *
                   STRING  ")"                 DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
           END-PERFORM
      *
           PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
           PERFORM 900-SQL-JYOKEN-EDIT4-SEC
      *
           .
       1001-RECECOM-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÆÃµ­»ö¹à¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-TOKKI-SQL-EDIT-SEC       SECTION.
      *
           MOVE    "L"         TO    WRK-SQL-TBL-NAME
           PERFORM 900-SQL-JYOKEN-EDIT1-SEC
      *
           STRING  "(L.SRYYM >= '"         DELIMITED   BY  SIZE
                   WRK-STSRYYMD(1:6)       DELIMITED   BY  SPACE
                   "' AND L.SRYYM <= '"    DELIMITED   BY  SIZE
                   WRK-EDSRYYMD(1:6)       DELIMITED   BY  SPACE
                   "')"                    DELIMITED   BY  SIZE
           INTO        WRK-SQL
           WITH POINTER    WRK-SQL-PNT
           END-STRING
      *
      *
           IF    ( SPA-Q01-NYUGAIKBN   NOT =   SPACE )
               PERFORM 10011-NYUGAIKBN-SQL-EDIT-SEC
           END-IF
      *
           IF    ( SPA-Q01-GTBL (5)    =   "1" )
      *
               MOVE    1                   TO  CNT-JYOKEN
      *
               PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
               IF    ( FLG-HKNSELPTN           =   2 )
                   STRING  "L.HKNID in (null"  DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL ( IDX1    >   5 )
      *
                       IF    (SPA-Q01-KOHNUM (IDX1)    NOT =   SPACE )
                           MOVE    IDX1    TO  WRK-NAME-SEQ
                           STRING  ",D"            DELIMITED  BY  SIZE
                                   WRK-NAME-SEQ    DELIMITED  BY  SIZE
                                   ".HKNID"  DELIMITED  BY  SIZE
                           INTO    WRK-SQL
                           WITH    POINTER WRK-SQL-PNT
                           END-STRING
                       END-IF
                   END-PERFORM
                   STRING  ")"     DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               ELSE
                   STRING  "L.HKNID in (null, D.HKNID)"
                                       DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
      *
               PERFORM 900-SQL-JYOKEN-EDIT3-SEC
           END-IF
      *
           IF    ( SPA-Q01-TOKKICD     NOT =   SPACE )
            OR   ( SPA-Q01-TOKKINAME   NOT =   SPACE )
      *
               MOVE    ZERO                TO  CNT-JYOKEN
      *
               IF    ( SPA-Q01-TOKKICD NOT =   SPACE )
      *
                   COMPUTE CNT-JYOKEN  =   CNT-JYOKEN  +   1
      *
                   PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
                   STRING  "'"                 DELIMITED BY SIZE
                     SPA-Q01-TOKKICD           DELIMITED BY SIZE
                     "'"                       DELIMITED BY SIZE
                     "in(L.TOKKI1,L.TOKKI2,L.TOKKI3,L.TOKKI4,L.TOKKI5)"
                                               DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
      *
               IF    ( SPA-Q01-TOKKINAME   NOT =   SPACE )
      *
                   COMPUTE CNT-JYOKEN  =   CNT-JYOKEN  +   1
      *
                   PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
                   STRING  "'"                 DELIMITED BY SIZE
                       SPA-Q01-TOKKINAME       DELIMITED BY SPACE
                       "'"                     DELIMITED BY SIZE
                       "in(L.TOKKINM1,L.TOKKINM2,L.TOKKINM3"
                                               DELIMITED BY SIZE
                       ",L.TOKKINM4,L.TOKKINM5)"
                                               DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
      *
               PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
           END-IF
      *
           IF    ( SPA-Q01-SAIGAIKBN   =   "1" OR "2" )
      *
               MOVE    1           TO  CNT-JYOKEN
               PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
               EVALUATE   SPA-Q01-SAIGAIKBN
               WHEN    "1"
                   STRING  "M.ETC_YKZFTN1"      DELIMITED BY SIZE
                       " in( 6,7,8)"            DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               WHEN    "2"
                   STRING  "M.ETC_YKZFTN1"      DELIMITED BY SIZE
                       " not in( 0, 6,7,8)"     DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-EVALUATE
      *
               PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
           END-IF
      *
           PERFORM 900-SQL-JYOKEN-EDIT4-SEC
      *
           .
       1001-TOKKI-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    Äã½êÆÀ¼Ô¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-TSY-SQL-EDIT-SEC           SECTION.
      *
           IF    ( SPA-Q01-TSY     =   "2" )
               PERFORM 10011-TSY-SQL-EDIT-SEC
           ELSE
               PERFORM 10011-TNK-SQL-EDIT-SEC
           END-IF
      *
           .
       1001-TSY-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    Äã½êÆÀ¼Ô£±(£ï£ò£²)¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-TNK-SQL-EDIT-SEC          SECTION.
      *
           MOVE    "J"             TO  WRK-SQL-TBL-NAME
           PERFORM 900-SQL-JYOKEN-EDIT1-SEC
      *
           EVALUATE    SPA-Q01-STTSYSKBT
           WHEN    "1"
           WHEN    "4"
               STRING  "(J.STYMD <= '"         DELIMITED   BY  SIZE
                       WRK-EDTSYYMD            DELIMITED   BY  SPACE
                       "' AND J.EDYMD >= '"    DELIMITED   BY  SIZE
                       WRK-STTSYYMD            DELIMITED   BY  SPACE
                       "')"                    DELIMITED   BY  SIZE
               INTO        WRK-SQL
               WITH POINTER    WRK-SQL-PNT
               END-STRING
           WHEN    "2"
               STRING  "(J.STYMD LIKE '"       DELIMITED   BY  SIZE
                       SPA-Q01-STTSYYM         DELIMITED   BY  SPACE
                       "%')"                   DELIMITED   BY  SIZE
               INTO        WRK-SQL
               WITH POINTER WRK-SQL-PNT
               END-STRING
           WHEN    "3"
               STRING  "(J.EDYMD LIKE '"       DELIMITED   BY  SIZE
                       SPA-Q01-STTSYYM         DELIMITED   BY  SPACE
                       "%')"                   DELIMITED   BY  SIZE
               INTO        WRK-SQL
               WITH POINTER WRK-SQL-PNT
               END-STRING
           END-EVALUATE
      *
           PERFORM 900-SQL-JYOKEN-EDIT4-SEC
      *
           .
       10011-TNK-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    Äã½êÆÀ¼Ô£²¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-TSY-SQL-EDIT-SEC          SECTION.
      *
           MOVE    "J"             TO  WRK-SQL-TBL-NAME
           PERFORM 900-SQL-JYOKEN-EDIT1-SEC
      *
           EVALUATE    SPA-Q01-STTSYSKBT
           WHEN    "1"
           WHEN    "4"
               STRING  "(J.NINSTYMD <= '"      DELIMITED   BY  SIZE
                       WRK-EDTSYYMD            DELIMITED   BY  SPACE
                       "' AND J.NINEDYMD >= '" DELIMITED   BY  SIZE
                       WRK-STTSYYMD            DELIMITED   BY  SPACE
                       "')"                    DELIMITED   BY  SIZE
               INTO        WRK-SQL
               WITH POINTER    WRK-SQL-PNT
               END-STRING
           WHEN    "2"
               STRING  "(J.NINSTYMD LIKE '"    DELIMITED   BY  SIZE
                       SPA-Q01-STTSYYM         DELIMITED   BY  SPACE
                       "%')"                   DELIMITED   BY  SIZE
               INTO        WRK-SQL
               WITH POINTER WRK-SQL-PNT
               END-STRING
           WHEN    "3"
               STRING  "(J.NINEDYMD LIKE '"    DELIMITED   BY  SIZE
                       SPA-Q01-STTSYYM         DELIMITED   BY  SPACE
                       "%')"                   DELIMITED   BY  SIZE
               INTO        WRK-SQL
               WITH POINTER WRK-SQL-PNT
               END-STRING
           END-EVALUATE
      *
           PERFORM 900-SQL-JYOKEN-EDIT4-SEC
      *
           .
       10011-TSY-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼Ô¥Þ¥¹¥¿¸¡º÷¾ò·ïÊÔ½¸½èÍý£²
      *****************************************************************
       1001-PTINF-SQL-EDIT2-SEC        SECTION.
      *
      *    ´Á»ú»áÌ¾¸¡º÷¾ò·ïÊÔ½¸½èÍý
           PERFORM 10011-NAME-SELECT-EDIT-SEC
      *
           .
       1001-PTINF-SQL-EDIT2-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼Ô¥Þ¥¹¥¿¸¡º÷¾ò·ïÊÔ½¸½èÍý£³
      *****************************************************************
       1001-PTINF-SQL-EDIT3-SEC        SECTION.
      *
      *    À¸Ç¯·îÆü¸¡º÷¾ò·ïÊÔ½¸½èÍý
           PERFORM 10011-BIRTH-SELECT-EDIT-SEC
      *
      *    ÃÂÀ¸·î¸¡º÷¾ò·ïÊÔ½¸½èÍý
           PERFORM 10011-BIRTHMON-SELECT-EDIT-SEC
      *
      *    À­ÊÌ¸¡º÷¾ò·ïÊÔ½¸½èÍý
           PERFORM 10011-SEX-SELECT-EDIT-SEC
      *
      *    ½»½ê¸¡º÷¾ò·ïÊÔ½¸½èÍý
           PERFORM 10011-HOME-ADRS-SELECT-EDIT-SEC
      *
      *    ¾õÂÖ£±¸¡º÷¾ò·ïÊÔ½¸½èÍý
           PERFORM 10011-CONDITION1-SEL-EDIT-SEC
      *
      *    ¾õÂÖ£²¸¡º÷¾ò·ïÊÔ½¸½èÍý
           PERFORM 10011-CONDITION2-SEL-EDIT-SEC
      *
      *    ¾õÂÖ£³¸¡º÷¾ò·ïÊÔ½¸½èÍý
           PERFORM 10011-CONDITION3-SEL-EDIT-SEC
      *
      *    ¸ºÌÈ»öÍ³¸¡º÷¾ò·ïÊÔ½¸½èÍý
           PERFORM 10011-GENMEN-SEL-EDIT-SEC
      *
      *    ¥³¥á¥ó¥È¸¡º÷¾ò·ïÊÔ½¸½èÍý
           IF    ( SPA-Q01-SELCOMMENT  =   "1" OR "2" )
               PERFORM 10011-COMMENT-SELECT-EDIT-SEC
           END-IF
      *
      *    ÅÅÏÃÈÖ¹æ¸¡º÷¾ò·ïÊÔ½¸½èÍý
           PERFORM 10011-TEL-SELECT-EDIT-SEC
      *
      *    »àË´¶èÊ¬¸¡º÷¾ò·ïÊÔ½¸½èÍý
           PERFORM 10011-DEATH-SELECT-EDIT-SEC
      *
           .
       1001-PTINF-SQL-EDIT3-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼Ô¥Þ¥¹¥¿¸¡º÷¾ò·ïÊÔ½¸½èÍý£´
      *****************************************************************
       1001-PTINF-SQL-EDIT4-SEC        SECTION.
      *
          MOVE     1               TO  CNT-JYOKEN
      *
          PERFORM  900-SQL-JYOKEN-EDIT2-SEC
      *
          STRING   "(A.TSTPTNUMKBN <> '1')"    DELIMITED   BY  SIZE
          INTO     WRK-SQL
          WITH     POINTER     WRK-SQL-PNT
          END-STRING
      *
          PERFORM  900-SQL-JYOKEN-EDIT3-SEC
           .
       1001-PTINF-SQL-EDIT4-EXT.
           EXIT.
      *****************************************************************
      *    ¿ÇÎÅ²ÊÍúÎò¥Þ¥¹¥¿¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-SRYKARRK-SQL-EDIT-SEC      SECTION.
      *
           MOVE    "B"             TO  WRK-SQL-TBL-NAME
           PERFORM 900-SQL-JYOKEN-EDIT8-SEC
      *
           PERFORM 900-SQL-JYOKEN-EDIT4-SEC
      *
      *
           .
       1001-SRYKARRK-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼ÔÉÂÌ¾¥Þ¥¹¥¿¸¡º÷¾ò·ïÊÔ½¸½èÍý¡ÊOR¡Ë
      *****************************************************************
       1001-PTBYOMEI-SQL-OR-EDIT-SEC       SECTION.
      *
           MOVE    "C"             TO  WRK-SQL-TBL-NAME
           PERFORM 900-SQL-JYOKEN-EDIT1-SEC
      *
           STRING "(((C.SRYYMD <= '"
                  WRK-EDSRYYMD
                  "') AND (C.TENKIYMD >= '"
                  WRK-STSRYYMD
                  "')) OR "
                  "((C.SRYYMD <= '"
                  WRK-EDSRYYMD
                  "') AND (C.TENKIYMD = '        '"
                  "))) AND (DLTFLG = ' ') AND "
           DELIMITED   BY  SIZE
           INTO        WRK-SQL
           WITH POINTER WRK-SQL-PNT
           END-STRING
      *
           IF    ( SPA-Q01-NYUGAIKBN   NOT =   SPACE )
               MOVE    1           TO  FLG-PTBYOMEI-SQL
               PERFORM 10011-NYUGAIKBN-SQL-EDIT-SEC
           END-IF
      *
           IF    ( SPA-Q01-SRYKA   NOT =   SPACE )
               PERFORM 10011-SRYKA-SQL-EDIT-SEC
           END-IF
      *
           IF    ( SPA-Q01-UTAGAIKBN   NOT =   SPACE )
               PERFORM 10011-UTAGAIFLG-SQL-EDIT-SEC
           END-IF
      *
           IF    ( SPA-Q01-KYUSEIKBN   NOT =   SPACE )
               PERFORM 10012-UTAGAIFLG-SQL-EDIT-SEC
           END-IF
      *
           IF    ( SPA-Q01-TENKIKBN   NOT =   SPACE )
               PERFORM 10011-TENKIKBN-SQL-EDIT-SEC
           END-IF
      *
           IF    ( SPA-Q01-SYUBYOKBN   NOT =   SPACE )
               PERFORM 10012-SYUBYOFLG-SQL-EDIT-SEC
           END-IF
      *
           MOVE    ZERO    TO      CNT-JYOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   5 )
      *
               IF  ( SPA-Q01-BYOMEI (IDX1)   NOT =   SPACE )
                   COMPUTE CNT-JYOKEN  =   CNT-JYOKEN  +   1
      *
                   PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
      *
      *            ÉÂÌ¾¤Î¹àÌÜÄ¹¤òµá¤á¤ë
                   MOVE    1               TO  WRK-STR-LEN
                   MOVE    SPACE           TO  WRK-STR
                   STRING  SPA-Q01-BYOMEI (IDX1)
                                               DELIMITED   BY  SIZE
                   INTO    WRK-STR
                   WITH POINTER WRK-STR-LEN
                   END-STRING
                   COMPUTE WRK-STR-LEN =   WRK-STR-LEN -   1
      *
      *            ÉÂÌ¾¤Î¼ÂºÝ¤ÎÊ¸»úÎóÄ¹¤òµá¤á¤ë
                   MOVE    SPA-Q01-BYOMEI (IDX1)
                                               TO  WRK-STR
                   PERFORM 900-STR-LEN-GET-SEC
                   MOVE    WRK-STR-LEN         TO  WRK-NUM-LEN
      *
                   STRING
                       "(C.BYOMEI LIKE '"      DELIMITED   BY  SIZE
                       SPA-Q01-BYOMEI (IDX1)   DELIMITED   BY  SPACE
                       "%')"                   DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
      *
           END-PERFORM
      *
           PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
           PERFORM 900-SQL-JYOKEN-EDIT4-SEC
      *
           .
       1001-PTBYOMEI-SQL-OR-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼ÔÉÂÌ¾¥Þ¥¹¥¿¸¡º÷¾ò·ïÊÔ½¸½èÍý¡ÊAND¡Ë
      *****************************************************************
       1001-PTBYOMEI-SQL-AND-EDIT-SEC      SECTION.
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
               UNTIL   (   IDXA    >   5 )
      *
               IF    (SPA-Q01-BYOMEI (IDXA)    NOT =   SPACE )
      *
                   MOVE    ZERO            TO  FLG-JYOKEN-AND
      *
                   MOVE    IDXA            TO  WRK-NAME-SEQ
                   MOVE    "C"             TO  WRK-SQL-TBL-NAME
                   MOVE    WRK-NAME-SEQ    TO  WRK-SQL-TBL-NAME (2 : 1)
      *
                   PERFORM 900-SQL-JYOKEN-EDIT1-SEC
      *
                   STRING "((("                    DELIMITED BY SIZE
                          WRK-SQL-TBL-NAME         DELIMITED BY SPACE
                          ".SRYYMD <= '"           DELIMITED BY SIZE
                          WRK-EDSRYYMD             DELIMITED BY SIZE
                          "') AND ("               DELIMITED BY SIZE
                          WRK-SQL-TBL-NAME         DELIMITED BY SPACE
                          ".TENKIYMD >= '"         DELIMITED BY SIZE
                          WRK-STSRYYMD             DELIMITED BY SIZE
                          "')) OR "                DELIMITED BY SIZE
                          "(("                     DELIMITED BY SIZE
                          WRK-SQL-TBL-NAME         DELIMITED BY SPACE
                          ".SRYYMD <= '"           DELIMITED BY SIZE
                          WRK-EDSRYYMD             DELIMITED BY SIZE
                          "') AND ("               DELIMITED BY SIZE
                          WRK-SQL-TBL-NAME         DELIMITED BY SPACE
                          ".TENKIYMD = '        '" DELIMITED BY SIZE
                          "))) AND ("              DELIMITED BY SIZE
                          WRK-SQL-TBL-NAME         DELIMITED BY SPACE
                          ".DLTFLG = ' ') AND "    DELIMITED BY SIZE
                   INTO        WRK-SQL
                   WITH POINTER WRK-SQL-PNT
                   END-STRING
      *
                   IF    ( SPA-Q01-NYUGAIKBN   NOT =   SPACE )
                       MOVE    1           TO  FLG-PTBYOMEI-SQL
                       PERFORM 10011-NYUGAIKBN-SQL-EDIT-SEC
                   END-IF
      *
                   IF    ( SPA-Q01-SRYKA   NOT =   SPACE )
                       PERFORM 10011-SRYKA-SQL-EDIT-SEC
                   END-IF
      *
                   IF    ( SPA-Q01-UTAGAIKBN   NOT =   SPACE )
                       PERFORM 10011-UTAGAIFLG-SQL-EDIT-SEC
                   END-IF
      *
                   IF    ( SPA-Q01-KYUSEIKBN   NOT =   SPACE )
                       PERFORM 10012-UTAGAIFLG-SQL-EDIT-SEC
                   END-IF
      *
                   IF    ( SPA-Q01-TENKIKBN    NOT =   SPACE )
                       PERFORM 10011-TENKIKBN-SQL-EDIT-SEC
                   END-IF
      *
                   IF    ( SPA-Q01-SYUBYOKBN   NOT =   SPACE )
                       PERFORM 10012-SYUBYOFLG-SQL-EDIT-SEC
                   END-IF
      *
                   MOVE    1               TO  CNT-JYOKEN
      *
                   PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
      *
      *            ÉÂÌ¾¤Î¹àÌÜÄ¹¤òµá¤á¤ë
                   MOVE    1               TO  WRK-STR-LEN
                   MOVE    SPACE           TO  WRK-STR
                   STRING  SPA-Q01-BYOMEI (IDXA)
                                               DELIMITED   BY  SIZE
                   INTO    WRK-STR
                   WITH POINTER WRK-STR-LEN
                   END-STRING
      *
                   COMPUTE WRK-STR-LEN =   WRK-STR-LEN -   1
      *
      *            ÉÂÌ¾¤Î¼ÂºÝ¤ÎÊ¸»úÎóÄ¹¤òµá¤á¤ë
                   MOVE    SPA-Q01-BYOMEI (IDXA)
                                           TO  WRK-STR
                   PERFORM 900-STR-LEN-GET-SEC
                   MOVE    WRK-STR-LEN     TO  WRK-NUM-LEN
      *
                   STRING
                       "("                     DELIMITED   BY  SIZE
                       WRK-SQL-TBL-NAME        DELIMITED   BY  SPACE
                       ".BYOMEI LIKE '"        DELIMITED   BY  SIZE
                       SPA-Q01-BYOMEI (IDXA)   DELIMITED   BY  SPACE 
                       "%')"                   DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
      *
                   PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
                   PERFORM 900-SQL-JYOKEN-EDIT4-SEC
      *
                   MOVE    1               TO  FLG-JYOKEN
      *
               END-IF
      *
           END-PERFORM
      *
           .
       1001-PTBYOMEI-SQL-AND-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¾È²ñ¥Ó¥å¡¼¸¡º÷¾ò·ïÊÔ½¸½èÍý(AND)
      *****************************************************************
       1001-TMPVQ001-SQL-AND-EDIT-SEC  SECTION.
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   5 )
      *
               IF    (SPA-Q01-KOHNUM (IDXA) NOT =   SPACE )
                   MOVE    ZERO            TO  FLG-JYOKEN-AND
      *
                   MOVE    IDXA            TO  WRK-NAME-SEQ
                   MOVE    "D"             TO  WRK-SQL-TBL-NAME
                   MOVE    WRK-NAME-SEQ    TO  WRK-SQL-TBL-NAME (2 : 1)
      *
                   PERFORM 900-SQL-JYOKEN-EDIT1-SEC
      *
                   STRING "(("             DELIMITED   BY  SIZE
                      WRK-SQL-TBL-NAME     DELIMITED   BY  SPACE
                      ".SRYYMD <= '"       DELIMITED   BY  SIZE
                      WRK-EDSRYYMD         DELIMITED   BY  SIZE
                      "') AND ("           DELIMITED   BY  SIZE
                      WRK-SQL-TBL-NAME     DELIMITED   BY  SPACE
                      ".SRYYMD >= '"       DELIMITED   BY  SIZE
                      WRK-STSRYYMD         DELIMITED   BY  SIZE
                      "')) AND"            DELIMITED   BY  SIZE
                   INTO        WRK-SQL
                   WITH POINTER WRK-SQL-PNT
                   END-STRING
      *
                   IF    ( SPA-Q01-NYUGAIKBN   NOT =   SPACE )
                       PERFORM 10011-NYUGAIKBN-SQL-EDIT-SEC
                   END-IF
      *
                   IF    ( SPA-Q01-SRYKA   NOT =   SPACE )
                       PERFORM 10011-SRYKA-SQL-EDIT-SEC
                   END-IF
      *
                   IF   ( SPA-Q01-GTBL (6)  =   "1" )
      *                ´µ¼Ô¸øÈñ¾ðÊó£Ó£Ñ£ÌÊÔ½¸½èÍý
                       MOVE    SPA-Q01-KOHNUM (IDXA)
                                           TO  WRK-KOHNUM
                       MOVE    SPA-Q01-PAYKBN (IDXA)
                                           TO  WRK-PAYKBN
                       PERFORM 10012-PTKOHINF-KOHNUM-EDIT-SEC
                   END-IF
      *
                   PERFORM 900-SQL-JYOKEN-EDIT4-SEC
      *
                   MOVE    1               TO  FLG-JYOKEN
      *
               END-IF
      *
           END-PERFORM
      *
      *    ¸øÈñÉéÃ´¼ÔÈÖ¹æ£Ó£Ñ£ÌÊÔ½¸½èÍý
           IF    ( SPA-Q01-FTNJANUM-G      NOT =   SPACE )
               MOVE    ZERO            TO  FLG-JYOKEN-OR
      *
               PERFORM 900-SQL-JYOKEN-EDIT5-SEC
      *
               PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   5 )
      *
                   IF    (SPA-Q01-KOHNUM (IDXA) NOT =   SPACE )
                       MOVE    ZERO    TO  FLG-JYOKEN-AND
      *
                       MOVE    IDXA        TO  WRK-NAME-SEQ
                       MOVE    "D"         TO  WRK-SQL-TBL-NAME
                       MOVE    WRK-NAME-SEQ
                                           TO  WRK-SQL-TBL-NAME (2 : 1)
      *
                       PERFORM 900-SQL-JYOKEN-EDIT6-SEC
                       PERFORM 10011-PTKOHINF-FTNNUM-EDIT-SEC
                       PERFORM 900-SQL-JYOKEN-EDIT4-SEC
                   END-IF
      *
               END-PERFORM
      *
               PERFORM 900-SQL-JYOKEN-EDIT4-SEC
      *
               MOVE    1               TO  FLG-JYOKEN
           END-IF
      *
      *    ´µ¼ÔÊÝ¸±¾ðÊó£Ó£Ñ£ÌÊÔ½¸½èÍý
           IF   ( SPA-Q01-GTBL (5)  =   "1" )
      *
               MOVE    ZERO            TO  FLG-JYOKEN-OR
      *
               PERFORM 900-SQL-JYOKEN-EDIT5-SEC
      *
               PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   5 )
      *
                   IF    (SPA-Q01-KOHNUM (IDXA) NOT =   SPACE )
                       MOVE    ZERO        TO  FLG-JYOKEN-AND
      *
                       MOVE    IDXA        TO  WRK-NAME-SEQ
                       MOVE    "D"         TO  WRK-SQL-TBL-NAME
                       MOVE    WRK-NAME-SEQ
                                           TO  WRK-SQL-TBL-NAME (2 : 1)
      *
                       PERFORM 900-SQL-JYOKEN-EDIT6-SEC
                       PERFORM 10011-PTHKNINF-SQL-EDIT-SEC
                       PERFORM 900-SQL-JYOKEN-EDIT4-SEC
      *
                   END-IF
      *
               END-PERFORM
      *
               PERFORM 900-SQL-JYOKEN-EDIT4-SEC
      *
               MOVE    1               TO  FLG-JYOKEN
      *
           END-IF
      *
           .
       1001-TMPVQ001-SQL-AND-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¾È²ñ¥Ó¥å¡¼¸¡º÷¾ò·ïÊÔ½¸½èÍý(OR)
      *****************************************************************
       1001-TMPVQ001-SQL-OR-EDIT-SEC   SECTION.
      *
           MOVE    "D"                 TO  WRK-SQL-TBL-NAME
      *
           PERFORM 900-SQL-JYOKEN-EDIT1-SEC
      *
           STRING "((D.SRYYMD <= '"
                      WRK-EDSRYYMD
                      "') AND (D.SRYYMD >= '"
                      WRK-STSRYYMD
                      "')) AND"
           DELIMITED   BY  SIZE
           INTO        WRK-SQL
           WITH POINTER WRK-SQL-PNT
           END-STRING
      *
           IF    ( SPA-Q01-NYUGAIKBN   NOT =   SPACE )
                   PERFORM 10011-NYUGAIKBN-SQL-EDIT-SEC
           END-IF
      *
           IF    ( SPA-Q01-SRYKA   NOT =   SPACE )
               PERFORM 10011-SRYKA-SQL-EDIT-SEC
           END-IF
      *
           IF   ( SPA-Q01-GTBL (5)  =   "1" )
      *        ´µ¼ÔÊÝ¸±¾ðÊó£Ó£Ñ£ÌÊÔ½¸½èÍý
               PERFORM 10011-PTHKNINF-SQL-EDIT-SEC
           END-IF
      *
           IF   ( SPA-Q01-GTBL (6)  =   "1" )
      *        ´µ¼Ô¸øÈñ¾ðÊó£Ó£Ñ£ÌÊÔ½¸½èÍý
               PERFORM 10011-PTKOHINF-KOHNUM-EDIT-SEC
               PERFORM 10011-PTKOHINF-FTNNUM-EDIT-SEC
           END-IF
      *
      *
           PERFORM 900-SQL-JYOKEN-EDIT4-SEC
      *
           .
       1001-TMPVQ001-SQL-OR-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÊÝ¸±¾ðÊó£Ó£Ñ£ÌÊÔ½¸½èÍý
      *****************************************************************
       10011-PTHKNINF-SQL-EDIT-SEC     SECTION.
      *
           MOVE    ZERO    TO      CNT-JYOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   5 )
      *
               IF  ( SPA-Q01-HKNJANUM (IDX1)
                                           NOT =   SPACE )
                   COMPUTE CNT-JYOKEN  =   CNT-JYOKEN  +   1
      *
                   PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
      *
      *            ÊÝ¸±¼ÔÈÖ¹æ¤Î¹àÌÜÄ¹¤òµá¤á¤ë
                   MOVE    1               TO  WRK-STR-LEN
                   MOVE    SPACE           TO  WRK-STR
                   STRING  SPA-Q01-HKNJANUM (IDX1) 
                                           DELIMITED   BY  SIZE
                   INTO    WRK-STR
                   WITH POINTER WRK-STR-LEN
                   END-STRING
                   COMPUTE WRK-STR-LEN =   WRK-STR-LEN -   1
      *
      *            ÊÝ¸±¼ÔÈÖ¹æ¤Î¼ÂºÝ¤ÎÊ¸»úÎóÄ¹¤òµá¤á¤ë
                   MOVE    SPA-Q01-HKNJANUM (IDX1) TO  WRK-STR
                   PERFORM 900-STR-LEN-GET-SEC
                   MOVE    WRK-STR-LEN             TO  WRK-KIGO-LEN
      *
                   STRING  "("             DELIMITED   BY  SIZE
                       WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                       ".HKNJANUM ='"      DELIMITED   BY  SIZE
                       SPA-Q01-HKNJANUM (IDX1)(1: WRK-KIGO-LEN)
                                           DELIMITED   BY  SIZE
                       "')"                DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
           END-PERFORM
      *
           PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
           IF      ( SPA-Q01-HONKZKKBN NOT =   SPACE )
      *
               MOVE    1                   TO  CNT-JYOKEN
      *
               PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
               STRING  "("
                       WRK-SQL-TBL-NAME        DELIMITED   BY  SPACE
                       ".HONKZKKBN = '"        DELIMITED   BY  SIZE
                       SPA-Q01-HONKZKKBN       DELIMITED   BY  SIZE
                       "')"                    DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
      *
               PERFORM 900-SQL-JYOKEN-EDIT3-SEC
           END-IF
      *
      *
           MOVE    ZERO    TO      CNT-JYOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   5 )
      *
               IF  ( SPA-Q01-HKNNUM (IDX1) NOT =   SPACE )
                   COMPUTE CNT-JYOKEN  =   CNT-JYOKEN  +   1
      *
                   PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
                   EVALUATE    SPA-Q01-HKNNUM (IDX1)
                   WHEN    "SYA"
                       STRING  "("             DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                           ".HKNNUM in ('001','002','003','004'"
                                               DELIMITED   BY  SIZE
                           ",'006','007','031','032','033','034','063'"
                                               DELIMITED   BY  SIZE
                           ",'072','073','074','075','009'"
                                               DELIMITED   BY  SIZE
                           ")"                 DELIMITED   BY  SIZE
                           " or ("             DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                           ".HKNNUM is null"   DELIMITED   BY  SIZE
                           "))"                DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   WHEN    "KOK"
                       STRING  "("             DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                           ".HKNNUM in ('060','067','068','069')"
                                               DELIMITED   BY  SIZE
                           ")"                 DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   WHEN    "KOU"
                       STRING  "("             DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                           ".HKNNUM in ('039','040')"
                                               DELIMITED   BY  SIZE
                           ")"                 DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   WHEN    OTHER
                       STRING  "("             DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                           ".HKNNUM ='"        DELIMITED   BY  SIZE
                           SPA-Q01-HKNNUM (IDX1)
                                               DELIMITED   BY  SIZE
                           "')"                DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
		   END-EVALUATE
               END-IF
           END-PERFORM
      *
           PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
           MOVE    ZERO    TO      CNT-JYOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   5 )
      *
               IF      ( SPA-Q01-KIGO (IDX1)   NOT =   SPACE )
                OR     ( SPA-Q01-NUM  (IDX1)   NOT =   SPACE )
                   COMPUTE CNT-JYOKEN  =   CNT-JYOKEN  +   1
      *
                   PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
                   STRING  "("           DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
      *
                   IF  ( SPA-Q01-KIGO (IDX1)   NOT =   SPACE )
      *                µ­¹æ¤Î¹àÌÜÄ¹¤òµá¤á¤ë
                       MOVE    1               TO  WRK-STR-LEN
                       MOVE    SPACE           TO  WRK-STR
                       STRING  SPA-Q01-KIGO (IDX1) DELIMITED   BY  SIZE
                       INTO    WRK-STR
                       WITH POINTER WRK-STR-LEN
                       END-STRING
                       COMPUTE WRK-STR-LEN =   WRK-STR-LEN -   1
      *
      *                µ­¹æ¤Î¼ÂºÝ¤ÎÊ¸»úÎóÄ¹¤òµá¤á¤ë
                       MOVE    SPA-Q01-KIGO (IDX1) TO  WRK-STR
                       PERFORM 900-STR-LEN-GET-SEC
                       MOVE    WRK-STR-LEN         TO  WRK-KIGO-LEN
      *
                       STRING  "("                 DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME        DELIMITED   BY  SPACE
                           ".KIGO LIKE '"          DELIMITED   BY  SIZE
                           SPA-Q01-KIGO (IDX1)(1: WRK-KIGO-LEN)
                                                   DELIMITED   BY  SIZE
                           "%')"                   DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                       IF  ( SPA-Q01-NUM  (IDX1)   NOT =   SPACE )
                           STRING  "AND"           DELIMITED   BY  SIZE
                           INTO    WRK-SQL
                           WITH    POINTER WRK-SQL-PNT
                           END-STRING
                       END-IF
                   END-IF
      *
                   IF  ( SPA-Q01-NUM  (IDX1)   NOT =   SPACE )
      *                ÈÖ¹æ¤Î¹àÌÜÄ¹¤òµá¤á¤ë
                       MOVE    1               TO  WRK-STR-LEN
                       MOVE    SPACE           TO  WRK-STR
                       STRING  SPA-Q01-KIGO (IDX1) DELIMITED   BY  SIZE
                       INTO    WRK-STR
                       WITH POINTER WRK-STR-LEN
                       END-STRING
                       COMPUTE WRK-STR-LEN =   WRK-STR-LEN -   1
      *
      *                ÈÖ¹æ¤Î¼ÂºÝ¤ÎÊ¸»úÎóÄ¹¤òµá¤á¤ë
                       MOVE    SPA-Q01-NUM (IDX1) TO  WRK-STR
                       PERFORM 900-STR-LEN-GET-SEC
                       MOVE    WRK-STR-LEN         TO  WRK-NUM-LEN
      *
                       STRING  "("                 DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME        DELIMITED   BY  SPACE
                               ".NUM LIKE '"       DELIMITED   BY  SIZE
                               SPA-Q01-NUM  (IDX1)(1: WRK-NUM-LEN)
                                                   DELIMITED   BY  SIZE
                               "%')"               DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   END-IF
      *
                   STRING  ")"                     DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
      *
               END-IF
      *
           END-PERFORM
      *
           PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
           .
       10011-PTHKNINF-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼Ô¸øÈñ¥Þ¥¹¥¿¡Ê¸øÈñ¤Î¼ïÎà¡Ë¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *    ¡ÊOR¾ò·ïÍÑ¡Ë
      *****************************************************************
       10011-PTKOHINF-KOHNUM-EDIT-SEC  SECTION.
      *
           MOVE    ZERO    TO      CNT-JYOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   5 )
      *
               IF  ( SPA-Q01-KOHNUM (IDX1) NOT =   SPACE )
                   COMPUTE CNT-JYOKEN  =   CNT-JYOKEN  +   1
      *
                   PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
      *            ÊÝ¸±ÈÖ¹æ¤Î¹àÌÜÄ¹¤òµá¤á¤ë
                   MOVE    1               TO  WRK-STR-LEN
                   MOVE    SPACE           TO  WRK-STR
                   STRING  SPA-Q01-KOHNUM (IDX1) 
                                           DELIMITED   BY  SIZE
                   INTO    WRK-STR
                   WITH POINTER WRK-STR-LEN
                   END-STRING
                   COMPUTE WRK-STR-LEN =   WRK-STR-LEN -   1
      *
      *            ÊÝ¸±ÈÖ¹æ¤Î¼ÂºÝ¤ÎÊ¸»úÎóÄ¹¤òµá¤á¤ë
                   MOVE    SPA-Q01-KOHNUM (IDX1)   TO  WRK-STR
                   PERFORM 900-STR-LEN-GET-SEC
                   MOVE    WRK-STR-LEN             TO  WRK-KIGO-LEN
      *
                   IF    ( SPA-Q01-KOHNUM (IDX1)   =   "027" )
                    AND  ( SPA-Q01-PAYKBN (IDX1)   =   "01" 
                                                    OR "02"
                                                    OR "03")
                       STRING  "(("            DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                           ".KOHNUM ='"        DELIMITED   BY  SIZE
                           SPA-Q01-KOHNUM (IDX1)(1: WRK-KIGO-LEN)
                                               DELIMITED   BY  SIZE
                           "')AND("            DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                           ".PAYKBN ='"        DELIMITED   BY  SIZE
                           SPA-Q01-PAYKBN (IDX1)
                                               DELIMITED   BY  SIZE
                           "'))"               DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   ELSE
                       STRING  "("             DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                           ".KOHNUM ='"        DELIMITED   BY  SIZE
                           SPA-Q01-KOHNUM (IDX1)(1: WRK-KIGO-LEN)
                                               DELIMITED   BY  SIZE
                           "')"                DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   END-IF
               END-IF
           END-PERFORM
      *
           PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
           .
       10011-PTKOHINF-KOHNUM-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼Ô¸øÈñ¥Þ¥¹¥¿¡Ê¸øÈñ¤Î¼ïÎà¡Ë¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *    ¡ÊAND¾ò·ïÍÑ¡Ë
      *****************************************************************
       10012-PTKOHINF-KOHNUM-EDIT-SEC  SECTION.
      *
           MOVE    1               TO  CNT-JYOKEN
      *
           PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
      *    ÊÝ¸±ÈÖ¹æ¤Î¹àÌÜÄ¹¤òµá¤á¤ë
           MOVE    1               TO  WRK-STR-LEN
           MOVE    SPACE           TO  WRK-STR
           STRING  WRK-KOHNUM
                                           DELIMITED   BY  SIZE
           INTO    WRK-STR
           WITH POINTER WRK-STR-LEN
           END-STRING
           COMPUTE WRK-STR-LEN =   WRK-STR-LEN -   1
      *
      *    ÊÝ¸±ÈÖ¹æ¤Î¼ÂºÝ¤ÎÊ¸»úÎóÄ¹¤òµá¤á¤ë
           MOVE    WRK-KOHNUM      TO  WRK-STR
           PERFORM 900-STR-LEN-GET-SEC
           MOVE    WRK-STR-LEN             TO  WRK-KIGO-LEN
      *
           IF    ( WRK-KOHNUM          =   "027" )
            AND  ( WRK-PAYKBN          =   "01" OR "02" OR "03" )
               STRING  "(("            DELIMITED   BY  SIZE
                   WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                   ".KOHNUM ='"        DELIMITED   BY  SIZE
                   WRK-KOHNUM (1: WRK-KIGO-LEN)
                                       DELIMITED   BY  SIZE
                   "')AND("            DELIMITED   BY  SIZE
                   WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                   ".PAYKBN ='"        DELIMITED   BY  SIZE
                   WRK-PAYKBN          DELIMITED   BY  SIZE
                   "'))"               DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           ELSE
               STRING  "("             DELIMITED   BY  SIZE
                   WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                   ".KOHNUM ='"        DELIMITED   BY  SIZE
                   WRK-KOHNUM (1: WRK-KIGO-LEN)
                                       DELIMITED   BY  SIZE
                   "')"                DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
           .
       10012-PTKOHINF-KOHNUM-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼Ô¸øÈñ¥Þ¥¹¥¿¡Ê¸øÈñ¤Î¼ïÎà¡Ë¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-PTKOHINF-FTNNUM-EDIT-SEC  SECTION.
      *
           MOVE    ZERO    TO      CNT-JYOKEN
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   5 )
      *
               IF  ( SPA-Q01-FTNJANUM  (IDX1)  NOT =   SPACE )
                   COMPUTE CNT-JYOKEN  =   CNT-JYOKEN  +   1
      *
                   PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
      *            ÉéÃ´¼ÔÈÖ¹æ¤Î¹àÌÜÄ¹¤òµá¤á¤ë
                   MOVE    1               TO  WRK-STR-LEN
                   MOVE    SPACE           TO  WRK-STR
                   STRING  SPA-Q01-FTNJANUM (IDX1)
                                               DELIMITED   BY  SIZE
                   INTO    WRK-STR
                   WITH POINTER WRK-STR-LEN
                   END-STRING
                   COMPUTE WRK-STR-LEN =   WRK-STR-LEN -   1
      *
      *            ÉéÃ´¼ÔÈÖ¹æ¤Î¼ÂºÝ¤ÎÊ¸»úÎóÄ¹¤òµá¤á¤ë
                   MOVE    SPA-Q01-FTNJANUM (IDX1) TO  WRK-STR
                   PERFORM 900-STR-LEN-GET-SEC
                   MOVE    WRK-STR-LEN         TO  WRK-NUM-LEN
      *
                   STRING  "("                 DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                           ".FTNJANUM LIKE '"
                                               DELIMITED   BY  SIZE
                           SPA-Q01-FTNJANUM  (IDX1)(1: WRK-NUM-LEN)
                                               DELIMITED   BY  SIZE
                           "%')"               DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
      *
               END-IF
      *
           END-PERFORM
      *
           PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
      *
           .
       10011-PTKOHINF-FTNNUM-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼ÔÊÝ¸±¥Þ¥¹¥¿¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-PTHKNINF-SQL-EDIT-SEC      SECTION.
      *
           MOVE    "D"             TO  WRK-SQL-TBL-NAME
      *
           PERFORM 900-SQL-JYOKEN-EDIT1-SEC
      *
      *    ÊÝ¸±¾ðÊó£Ó£Ñ£ÌÊÔ½¸½èÍý
           PERFORM 10011-PTHKNINF-SQL-EDIT-SEC
      *
           STRING "AND (D.DELKBN = ' ')  "
           DELIMITED   BY  SIZE
           INTO        WRK-SQL
           WITH POINTER WRK-SQL-PNT
           END-STRING
      *
           PERFORM 900-SQL-JYOKEN-EDIT4-SEC
      *
           .
       1001-PTHKNINF-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼Ô¸øÈñ¥Þ¥¹¥¿¸¡º÷¾ò·ïÊÔ½¸½èÍý(OR)
      *****************************************************************
       1001-PTKOHINF-SQL-OR-EDIT-SEC   SECTION.
      *
           MOVE    "E"                 TO  WRK-SQL-TBL-NAME
      *
           PERFORM 900-SQL-JYOKEN-EDIT1-SEC
      *
      *    ¸øÈñ¾ðÊó£Ó£Ñ£ÌÊÔ½¸½èÍý
           PERFORM 10011-PTKOHINF-KOHNUM-EDIT-SEC
           PERFORM 10011-PTKOHINF-FTNNUM-EDIT-SEC
      *
           STRING "AND (E.SAKJOKBN = ' ')  "
           DELIMITED   BY  SIZE
           INTO        WRK-SQL
           WITH POINTER WRK-SQL-PNT
           END-STRING
      *
           PERFORM 900-SQL-JYOKEN-EDIT4-SEC
      *
           .
       1001-PTKOHINF-SQL-OR-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼Ô¸øÈñ¥Þ¥¹¥¿¸¡º÷¾ò·ïÊÔ½¸½èÍý(AND)
      *****************************************************************
       1001-PTKOHINF-SQL-AND-EDIT-SEC   SECTION.
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   5 )
      *
               IF    (SPA-Q01-KOHNUM (IDXA) NOT =   SPACE )
                   MOVE    ZERO            TO  FLG-JYOKEN-AND
      *
                   MOVE    IDXA            TO  WRK-NAME-SEQ
                   MOVE    "E"             TO  WRK-SQL-TBL-NAME
                   MOVE    WRK-NAME-SEQ    TO  WRK-SQL-TBL-NAME (2 : 1)
      *
                   PERFORM 900-SQL-JYOKEN-EDIT1-SEC
      *
      *            ´µ¼Ô¸øÈñ¾ðÊó£Ó£Ñ£ÌÊÔ½¸½èÍý
                   MOVE    SPA-Q01-KOHNUM (IDXA)
                                           TO  WRK-KOHNUM
                   MOVE    SPA-Q01-PAYKBN (IDXA)
                                           TO  WRK-PAYKBN
                   PERFORM 10012-PTKOHINF-KOHNUM-EDIT-SEC
      *
                   PERFORM 900-SQL-JYOKEN-EDIT4-SEC
      *
                   MOVE    1               TO  FLG-JYOKEN
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-Q01-FTNJANUM-G      NOT =   SPACE )
               MOVE    ZERO                TO  FLG-JYOKEN-OR
      *
               PERFORM 900-SQL-JYOKEN-EDIT5-SEC
      *
               PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   5 )
      *
                   IF    (SPA-Q01-KOHNUM (IDXA) NOT =   SPACE )
      *
                       MOVE    ZERO        TO  FLG-JYOKEN-AND
      *
                       MOVE    IDXA        TO  WRK-NAME-SEQ
                       MOVE    "E"         TO  WRK-SQL-TBL-NAME
                       MOVE    WRK-NAME-SEQ
                                           TO  WRK-SQL-TBL-NAME (2 : 1)
      *
                       PERFORM 900-SQL-JYOKEN-EDIT6-SEC
                       PERFORM 10011-PTKOHINF-FTNNUM-EDIT-SEC
                       PERFORM 900-SQL-JYOKEN-EDIT4-SEC
      *
                   END-IF
      *
               END-PERFORM
      *
               PERFORM 900-SQL-JYOKEN-EDIT4-SEC
      *
               MOVE    1               TO  FLG-JYOKEN
           END-IF
      *
           PERFORM 900-SQL-JYOKEN-EDIT5-SEC
      *
           MOVE    ZERO                TO  FLG-JYOKEN-AND
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   5 )
      *
               IF    (SPA-Q01-KOHNUM (IDXA) NOT =   SPACE )
      *
                   MOVE    IDXA        TO  WRK-NAME-SEQ
                   MOVE    "E"         TO  WRK-SQL-TBL-NAME
                   MOVE    WRK-NAME-SEQ
                                       TO  WRK-SQL-TBL-NAME (2 : 1)
      *
                   MOVE    1           TO  CNT-JYOKEN
                   PERFORM 900-SQL-JYOKEN-EDIT2-SEC
                   STRING  "("                 DELIMITED   BY  SIZE
                           WRK-SQL-TBL-NAME    DELIMITED   BY  SPACE
                           ".SAKJOKBN = ' ')"  DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH POINTER WRK-SQL-PNT
                   END-STRING
      *
                   PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
               END-IF
      *
           END-PERFORM
      *
           PERFORM 900-SQL-JYOKEN-EDIT4-SEC
      *
           .
       1001-PTKOHINF-SQL-AND-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¼õ¿ÇÍúÎò¥Þ¥¹¥¿¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-JYURRK-SQL-EDIT-SEC        SECTION.
      *
      *
           MOVE    "F"             TO  WRK-SQL-TBL-NAME
           PERFORM 900-SQL-JYOKEN-EDIT1-SEC
      *
           IF    ( SPA-Q01-NYUGAIKBN   NOT =   SPACE )
               PERFORM 10011-NYUGAIKBN-SQL-EDIT-SEC
           END-IF
      *
           IF    ( SPA-Q01-SRYKA   NOT =   SPACE )
               PERFORM 10011-SRYKA-SQL-EDIT-SEC
           END-IF
      *
           MOVE    1                   TO  CNT-JYOKEN
      *
           PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
      *    ºÇ½ª¼õ¿ÇÆü¸¡º÷¾ò·ïÊÔ½¸
           STRING  "(F.SRYYMD >= '"            DELIMITED   BY  SIZE
                   WRK-STSRYYMD                DELIMITED   BY  SIZE
                   "') AND (F.SRYYMD <= '"     DELIMITED   BY  SIZE
                   WRK-EDSRYYMD                DELIMITED   BY  SIZE
                   "')"                        DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER     WRK-SQL-PNT
           END-STRING
      *
           PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
           PERFORM 900-SQL-JYOKEN-EDIT4-SEC
      *
           .
       1001-JYURRK-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¸¡º÷¾ò·ïÊÔ½¸½èÍý¡ÊOR¡Ë
      *****************************************************************
       1001-SRYACT-SQL-OR-EDIT-SEC     SECTION.
      *
           MOVE    "G"             TO  WRK-SQL-TBL-NAME
           PERFORM 900-SQL-JYOKEN-EDIT1-SEC
      *
           IF    ( SPA-Q01-GTBL(8) =   SPACE )
               CONTINUE
           ELSE
               STRING "((G.SRYYMD <= '"
                      WRK-EDSRYYMD
                      "') AND (G.SRYYMD >= '"
                      WRK-STSRYYMD
                      "')) AND"
               DELIMITED   BY  SIZE
               INTO        WRK-SQL
               WITH POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           IF    ( SPA-Q01-NYUGAIKBN   NOT =   SPACE )
               PERFORM 10011-NYUGAIKBN-SQL-EDIT-SEC
           END-IF
      *
           IF    ( SPA-Q01-SRYKA   NOT =   SPACE )
               PERFORM 10011-SRYKA-SQL-EDIT-SEC
           END-IF
      *
           IF    ( SPA-Q01-SRYKBN  NOT =   SPACE )
               PERFORM 10011-SRYKBN-SQL-EDIT-SEC
           END-IF
      *
           MOVE    ZERO    TO      CNT-JYOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   5 )
      *
               IF      ( SPA-Q01-SRYCD     (IDX1)  NOT =   SPACE )
                   COMPUTE CNT-JYOKEN  =   CNT-JYOKEN  +   1
      *
                   PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
                   STRING  "(G.SRYCD1 = '"         DELIMITED   BY  SIZE
                           SPA-Q01-SRYCD (IDX1)    DELIMITED   BY  SIZE
                           "') OR"                 DELIMITED   BY  SIZE
                           "(G.SRYCD2 = '"         DELIMITED   BY  SIZE
                           SPA-Q01-SRYCD (IDX1)    DELIMITED   BY  SIZE
                           "') OR"                 DELIMITED   BY  SIZE
                           "(G.SRYCD3 = '"         DELIMITED   BY  SIZE
                           SPA-Q01-SRYCD (IDX1)    DELIMITED   BY  SIZE
                           "') OR"                 DELIMITED   BY  SIZE
                           "(G.SRYCD4 = '"         DELIMITED   BY  SIZE
                           SPA-Q01-SRYCD (IDX1)    DELIMITED   BY  SIZE
                           "') OR"                 DELIMITED   BY  SIZE
                           "(G.SRYCD5 = '"         DELIMITED   BY  SIZE
                           SPA-Q01-SRYCD (IDX1)    DELIMITED   BY  SIZE
                           "')"                    DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
      *
               END-IF
      *
           END-PERFORM
      *
           PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
           PERFORM 900-SQL-JYOKEN-EDIT4-SEC
      *
           .
       1001-SRYACT-SQL-OR-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¸¡º÷¾ò·ïÊÔ½¸½èÍý¡ÊAND¡Ë
      *****************************************************************
       1001-SRYACT-SQL-AND-EDIT-SEC    SECTION.
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
               UNTIL   (   IDXA    >   5 )
      *
               IF      ( SPA-Q01-SRYCD     (IDXA)  NOT =   SPACE )
      *
                   MOVE    ZERO            TO  FLG-JYOKEN-AND
      *
                   MOVE    IDXA            TO  WRK-NAME-SEQ
                   MOVE    "G"             TO  WRK-SQL-TBL-NAME
                   MOVE    WRK-NAME-SEQ    TO  WRK-SQL-TBL-NAME (2 : 1)
                   PERFORM 900-SQL-JYOKEN-EDIT1-SEC
      *
                   IF    ( SPA-Q01-GTBL(8) =   SPACE )
                       CONTINUE
                   ELSE
                       STRING "(("                 DELIMITED BY SIZE
                              WRK-SQL-TBL-NAME     DELIMITED BY SPACE
                              ".SRYYMD <= '"       DELIMITED BY SIZE
                              WRK-EDSRYYMD         DELIMITED BY SIZE
                              "') AND ("           DELIMITED BY SIZE
                              WRK-SQL-TBL-NAME     DELIMITED BY SPACE
                              ".SRYYMD >= '"       DELIMITED BY SIZE
                              WRK-STSRYYMD         DELIMITED BY SIZE
                              "')) AND"            DELIMITED BY SIZE
                       INTO   WRK-SQL
                       WITH POINTER WRK-SQL-PNT
                       END-STRING
                   END-IF
      *
                   IF    ( SPA-Q01-NYUGAIKBN   NOT =   SPACE )
                       PERFORM 10011-NYUGAIKBN-SQL-EDIT-SEC
                   END-IF
      *
                   IF    ( SPA-Q01-SRYKA   NOT =   SPACE )
                       PERFORM 10011-SRYKA-SQL-EDIT-SEC
                   END-IF
      *
                   IF    ( SPA-Q01-SRYKBN  NOT =   SPACE )
                       PERFORM 10011-SRYKBN-SQL-EDIT-SEC
                   END-IF
      *
                   MOVE    1               TO      CNT-JYOKEN
      *
                   PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
                   STRING  "("                     DELIMITED BY SIZE
                           WRK-SQL-TBL-NAME        DELIMITED BY SPACE
                           ".SRYCD1 = '"           DELIMITED BY SIZE
                           SPA-Q01-SRYCD (IDXA)    DELIMITED BY SIZE
                           "') OR"                 DELIMITED BY SIZE
                           "("                     DELIMITED BY SIZE
                           WRK-SQL-TBL-NAME        DELIMITED BY SPACE
                           ".SRYCD2 = '"           DELIMITED BY SIZE
                           SPA-Q01-SRYCD (IDXA)    DELIMITED BY SIZE
                           "') OR"                 DELIMITED BY SIZE
                           "("                     DELIMITED BY SIZE
                           WRK-SQL-TBL-NAME        DELIMITED BY SPACE
                           ".SRYCD3 = '"           DELIMITED BY SIZE
                           SPA-Q01-SRYCD (IDXA)    DELIMITED BY SIZE
                           "') OR"                 DELIMITED BY SIZE
                           "("                     DELIMITED BY SIZE
                           WRK-SQL-TBL-NAME        DELIMITED BY SPACE
                           ".SRYCD4 = '"           DELIMITED BY SIZE
                           SPA-Q01-SRYCD (IDXA)    DELIMITED BY SIZE
                           "') OR"                 DELIMITED BY SIZE
                           "("                     DELIMITED BY SIZE
                           WRK-SQL-TBL-NAME        DELIMITED BY SPACE
                           ".SRYCD5 = '"           DELIMITED BY SIZE
                           SPA-Q01-SRYCD (IDXA)    DELIMITED BY SIZE
                           "')"                    DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
      *
                   PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
                   PERFORM 900-SQL-JYOKEN-EDIT4-SEC
      *
                   MOVE    1               TO  FLG-JYOKEN
      *
               END-IF
      *
           END-PERFORM
      *
           .
       1001-SRYACT-SQL-AND-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼ÔÈÖ¹æ¥Þ¥¹¥¿¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       1001-PTNUM-SQL-EDIT-SEC         SECTION.
      *
           IF  ( FLG-JYOKEN        =   1 )
               STRING " AND "      DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER     WRK-SQL-PNT
               END-STRING
           END-IF
      *
           STRING  "((H.HOSPNUM="              DELIMITED   BY  SIZE
                   SPA-HOSPNUM                 DELIMITED   BY  SIZE
                   ")AND("                     DELIMITED   BY  SIZE
                   "H.PTID=A.PTID::bigint) "   DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER     WRK-SQL-PNT
           END-STRING
      *
           IF  ( SPA-Q01-GTBL (1)  =   "1" )
      *
               IF    ( SPA-Q01-STPTNUM     NOT =   SPACE )
                   STRING  "AND (H.PTNUM>='"   DELIMITED  BY  SIZE
                           SPA-Q01-STPTNUM     DELIMITED  BY  SIZE
                           "')"                DELIMITED  BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
      *
               IF    ( SPA-Q01-EDPTNUM     NOT =   SPACE )
                   STRING  "AND (H.PTNUM<='"   DELIMITED  BY  SIZE
                           SPA-Q01-EDPTNUM     DELIMITED  BY  SIZE
                           "')"                DELIMITED  BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
      *
           END-IF
      *
           STRING  ")"         DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER     WRK-SQL-PNT
           END-STRING
      *
           .
       1001-PTNUM-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ´Á»ú»áÌ¾¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-NAME-SELECT-EDIT-SEC           SECTION.
      *
           MOVE    ZERO    TO      CNT-JYOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   5 )
               IF  ( SPA-Q01-NAME (IDX1)   NOT =   SPACE)
      *
                   COMPUTE CNT-JYOKEN  =   CNT-JYOKEN  +   1
      *
                   PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
      *            ¥«¥Ê»áÌ¾¤Î¹àÌÜÄ¹¤òµá¤á¤ë
                   MOVE    1               TO  WRK-STR-LEN
                   MOVE    SPACE           TO  WRK-STR
                   STRING  SPA-Q01-NAME (IDX1)   DELIMITED   BY  SIZE
                   INTO    WRK-STR
                   WITH POINTER WRK-STR-LEN
                   END-STRING
                   COMPUTE WRK-STR-LEN =   WRK-STR-LEN -   1
      *
      *            ¥«¥Ê»áÌ¾¤Î¼ÂºÝ¤ÎÊ¸»úÎóÄ¹¤òµá¤á¤ë
                   MOVE    SPA-Q01-NAME (IDX1)   TO  WRK-STR
                   PERFORM 900-STR-LEN-GET-SEC
      *
                   STRING  "(A.NAME LIKE '"        DELIMITED   BY  SIZE
                           SPA-Q01-NAME (IDX1)(1: WRK-STR-LEN) 
                                                   DELIMITED   BY  SIZE
                           "%' OR A.KANANAME LIKE '"
                           SPA-Q01-NAME (IDX1)(1: WRK-STR-LEN) 
                                                   DELIMITED   BY  SIZE
                           "%')"                   DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
      *
               END-IF
           END-PERFORM
      *
           PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
           .
       10011-NAME-SELECT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    À¸Ç¯·îÆü¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-BIRTH-SELECT-EDIT-SEC          SECTION.
      *
           IF      ( SPA-Q01-STBIRTHDAY    =   SPACE )
            AND    ( SPA-Q01-EDBIRTHDAY    =   SPACE )
               GO  TO  10011-BIRTH-SELECT-EDIT-EXT
           END-IF
      *
           MOVE    1                   TO  CNT-JYOKEN
      *
           PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
           STRING  "(BIRTHDAY >= '"        DELIMITED   BY  SIZE
                   WRK-STBIRTHDAY          DELIMITED   BY  SIZE
                   "') AND "               DELIMITED   BY  SIZE
                   "(BIRTHDAY  <= '"       DELIMITED   BY  SIZE
                   WRK-EDBIRTHDAY          DELIMITED   BY  SIZE
                   "')"                    DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
           PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
      *
           .
       10011-BIRTH-SELECT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÃÂÀ¸·î¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-BIRTHMON-SELECT-EDIT-SEC          SECTION.
      *
           IF      ( SPA-Q01-BIRTHMON      =   "00" )
               GO  TO  10011-BIRTHMON-SELECT-EDIT-EXT
           END-IF
      *
           MOVE    1                   TO  CNT-JYOKEN
      *
           PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
           STRING  "(SUBSTR(BIRTHDAY,5,2) = '" DELIMITED   BY  SIZE
                   SPA-Q01-BIRTHMON            DELIMITED   BY  SIZE
                   "')"                        DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
           PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
      *
           .
       10011-BIRTHMON-SELECT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    À­ÊÌ¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-SEX-SELECT-EDIT-SEC            SECTION.
      *
           IF      ( SPA-Q01-SEX       =   SPACE )
               GO  TO  10011-SEX-SELECT-EDIT-EXT
           END-IF
      *
           MOVE    1                   TO  CNT-JYOKEN
      *
           PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
           STRING  "(SEX = '"       DELIMITED   BY  SIZE
                   SPA-Q01-SEX      DELIMITED   BY  SIZE
                   "')"             DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
           PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
           .
       10011-SEX-SELECT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ½»½ê¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-HOME-ADRS-SELECT-EDIT-SEC      SECTION.
      *
           MOVE    ZERO    TO      CNT-JYOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   5 )
               IF  ( SPA-Q01-JYUSHO (IDX1) NOT =   SPACE)
      *
                   COMPUTE CNT-JYOKEN  =   CNT-JYOKEN  +   1
      *
                   PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
      *            ½»½ê¤Î¹àÌÜÄ¹¤òµá¤á¤ë
                   MOVE    1               TO  WRK-STR-LEN
                   MOVE    SPACE           TO  WRK-STR
                   STRING  SPA-Q01-JYUSHO (IDX1)   DELIMITED   BY  SIZE
                   INTO    WRK-STR
                   WITH POINTER WRK-STR-LEN
                   END-STRING
                   COMPUTE WRK-STR-LEN =   WRK-STR-LEN -   1
      *
      *            ½»½ê¤Î¼ÂºÝ¤ÎÊ¸»úÎóÄ¹¤òµá¤á¤ë
                   MOVE    SPA-Q01-JYUSHO (IDX1)   TO  WRK-STR
                   PERFORM 900-STR-LEN-GET-SEC
      *
                   STRING  "(HOME_ADRS || HOME_BANTI LIKE '"
                                                   DELIMITED   BY  SIZE
                           SPA-Q01-JYUSHO (IDX1)(1: WRK-STR-LEN) 
                                                   DELIMITED   BY  SIZE
                           "%')"                   DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
      *
                END-IF
           END-PERFORM
      *
           PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
           .
       10011-HOME-ADRS-SELECT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¾õÂÖ£±¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-CONDITION1-SEL-EDIT-SEC        SECTION.
      *
           MOVE    ZERO    TO      CNT-JYOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   3 )
               IF  ( SPA-Q01-CONDITION1 (IDX1) NOT =   SPACE)
      *
                   COMPUTE CNT-JYOKEN  =   CNT-JYOKEN  +   1
      *
                   PERFORM 900-SQL-JYOKEN-EDIT7-SEC
      *
                   IF    ( CNT-JYOKEN  =   1 )
                       STRING  "CONDITION_1 IN ("   DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   END-IF
      *
                   STRING  "'"                  DELIMITED   BY  SIZE
                       SPA-Q01-CONDITION1 (IDX1)
                                                DELIMITED   BY  SIZE
                       "'"                      DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
      *
                END-IF
           END-PERFORM
      *
           PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
           .
       10011-CONDITION1-SEL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¾õÂÖ£²¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-CONDITION2-SEL-EDIT-SEC        SECTION.
      *
           MOVE    ZERO    TO      CNT-JYOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   3 )
               IF  ( SPA-Q01-CONDITION2 (IDX1) NOT =   SPACE)
      *
                   COMPUTE CNT-JYOKEN  =   CNT-JYOKEN  +   1
      *
                   PERFORM 900-SQL-JYOKEN-EDIT7-SEC
      *
                   IF    ( CNT-JYOKEN  =   1 )
                       STRING  "CONDITION_2 IN ("   DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   END-IF
      *
                   STRING  "'"                  DELIMITED   BY  SIZE
                       SPA-Q01-CONDITION2 (IDX1)
                                                DELIMITED   BY  SIZE
                       "'"                      DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
      *
                END-IF
           END-PERFORM
      *
           PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
           .
       10011-CONDITION2-SEL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¾õÂÖ£³¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-CONDITION3-SEL-EDIT-SEC        SECTION.
      *
           MOVE    ZERO    TO      CNT-JYOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   3 )
               IF  ( SPA-Q01-CONDITION3 (IDX1) NOT =   SPACE)
      *
                   COMPUTE CNT-JYOKEN  =   CNT-JYOKEN  +   1
      *
                   PERFORM 900-SQL-JYOKEN-EDIT7-SEC
      *
                   IF    ( CNT-JYOKEN  =   1 )
                       STRING  "CONDITION_3 IN ("   DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   END-IF
      *
                   STRING  "'"                  DELIMITED   BY  SIZE
                       SPA-Q01-CONDITION3 (IDX1)
                                                DELIMITED   BY  SIZE
                       "'"                      DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
      *
                END-IF
           END-PERFORM
      *
           PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
           .
       10011-CONDITION3-SEL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¸ºÌÈ»öÍ³¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-GENMEN-SEL-EDIT-SEC       SECTION.
      *
           MOVE    ZERO    TO      CNT-JYOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   3 )
               IF  ( SPA-Q01-GENMEN (IDX1) NOT =   SPACE)
      *
                   COMPUTE CNT-JYOKEN  =   CNT-JYOKEN  +   1
      *
                   PERFORM 900-SQL-JYOKEN-EDIT7-SEC
      *
                   IF    ( CNT-JYOKEN  =   1 )
                       STRING  "DISCOUNT IN ("   DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   END-IF
      *
                   STRING  "'"                  DELIMITED   BY  SIZE
                       SPA-Q01-GENMEN (IDX1)
                                                DELIMITED   BY  SIZE
                       "'"                      DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
      *
                END-IF
           END-PERFORM
      *
           PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
           .
       10011-GENMEN-SEL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¥³¥á¥ó¥È¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-COMMENT-SELECT-EDIT-SEC        SECTION.
      *
           MOVE    ZERO    TO      CNT-JYOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   5 )
               IF  ( SPA-Q01-COMMENT (IDX1) NOT =   SPACE)
      *
                   COMPUTE CNT-JYOKEN  =   CNT-JYOKEN  +   1
      *
                   PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
      *            ¥³¥á¥ó¥È¤Î¹àÌÜÄ¹¤òµá¤á¤ë
                   MOVE    1               TO  WRK-STR-LEN
                   MOVE    SPACE           TO  WRK-STR
                   STRING  SPA-Q01-COMMENT (IDX1)  DELIMITED   BY  SIZE
                   INTO    WRK-STR
                   WITH POINTER WRK-STR-LEN
                   END-STRING
                   COMPUTE WRK-STR-LEN =   WRK-STR-LEN -   1
      *
      *            ¥³¥á¥ó¥È¤Î¼ÂºÝ¤ÎÊ¸»úÎóÄ¹¤òµá¤á¤ë
                   MOVE    SPA-Q01-COMMENT (IDX1)  TO  WRK-STR
                   PERFORM 900-STR-LEN-GET-SEC
      *
                   IF    ( SPA-Q01-SELCOMMENT      =   "1" )
                       STRING  "(COMMENT_1 LIKE '" DELIMITED   BY  SIZE
                           SPA-Q01-COMMENT (IDX1)(1: WRK-STR-LEN) 
                                                   DELIMITED   BY  SIZE
                           "%')OR"                 DELIMITED   BY  SIZE
                           " (COMMENT_2 LIKE '"    DELIMITED   BY  SIZE
                           SPA-Q01-COMMENT (IDX1)(1: WRK-STR-LEN) 
                                                   DELIMITED   BY  SIZE
                           "%')"                   DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   ELSE
                       STRING  "(TABOO_1 LIKE '" DELIMITED   BY  SIZE
                           SPA-Q01-COMMENT (IDX1)(1: WRK-STR-LEN) 
                                                   DELIMITED   BY  SIZE
                           "%')OR"                 DELIMITED   BY  SIZE
                           "(TABOO_2 LIKE '" DELIMITED   BY  SIZE
                           SPA-Q01-COMMENT (IDX1)(1: WRK-STR-LEN) 
                                                   DELIMITED   BY  SIZE
                           "%')OR"                 DELIMITED   BY  SIZE
                           "(ALLERGY_1 LIKE '" DELIMITED   BY  SIZE
                           SPA-Q01-COMMENT (IDX1)(1: WRK-STR-LEN) 
                                                   DELIMITED   BY  SIZE
                           "%')OR"                 DELIMITED   BY  SIZE
                           "(ALLERGY_2 LIKE '" DELIMITED   BY  SIZE
                           SPA-Q01-COMMENT (IDX1)(1: WRK-STR-LEN) 
                                                   DELIMITED   BY  SIZE
                           "%')OR"                 DELIMITED   BY  SIZE
                           "(KANSENSYO_1 LIKE '" DELIMITED   BY  SIZE
                           SPA-Q01-COMMENT (IDX1)(1: WRK-STR-LEN) 
                                                   DELIMITED   BY  SIZE
                           "%')OR"                 DELIMITED   BY  SIZE
                           "(KANSENSYO_2 LIKE '" DELIMITED   BY  SIZE
                           SPA-Q01-COMMENT (IDX1)(1: WRK-STR-LEN) 
                                                   DELIMITED   BY  SIZE
                           "%')"                   DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
                   END-IF
      *
                END-IF
           END-PERFORM
      *
           PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
      *
           .
       10011-COMMENT-SELECT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÅÅÏÃÈÖ¹æ¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-TEL-SELECT-EDIT-SEC        SECTION.
      *
           MOVE    ZERO    TO      CNT-JYOKEN
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL   (   IDX1    >   5 )
               IF  ( SPA-Q01-TEL (IDX1) NOT =   SPACE)
      *
      *            ÅÅÏÃÈÖ¹æ¤Î¹àÌÜÄ¹¤òµá¤á¤ë
                   MOVE    1               TO  WRK-STR-LEN
                   MOVE    SPACE           TO  WRK-STR
                   STRING  SPA-Q01-TEL (IDX1)  DELIMITED   BY  SIZE
                   INTO    WRK-STR
                   WITH POINTER WRK-STR-LEN
                   END-STRING
                   COMPUTE WRK-STR-LEN =   WRK-STR-LEN -   1
      *
      *            ÅÅÏÃÈÖ¹æ¤Î¼ÂºÝ¤ÎÊ¸»úÎóÄ¹¤òµá¤á¤ë
                   MOVE    SPA-Q01-TEL (IDX1)  TO  WRK-STR
                   PERFORM 900-STR-LEN-GET-SEC
      *
                   INSPECT SPA-Q01-TEL (IDX1)  TALLYING
                           CNT-STR         FOR ALL "%"
      *
                   IF    ( SPA-Q01-TEL-TO-HOME1    =   1 )
      *
                       COMPUTE CNT-JYOKEN  =   CNT-JYOKEN  +   1
      *
                       PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
                       STRING  "(HOME_TEL1 LIKE '" DELIMITED   BY  SIZE
                           SPA-Q01-TEL (IDX1)(1: WRK-STR-LEN) 
                                                   DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
      *
                       IF    ( CNT-STR         =   ZERO )
                           STRING  "%')"           DELIMITED   BY  SIZE
                           INTO    WRK-SQL
                           WITH    POINTER WRK-SQL-PNT
                           END-STRING
                       ELSE
                           STRING  "')"            DELIMITED   BY  SIZE
                           INTO    WRK-SQL
                           WITH    POINTER WRK-SQL-PNT
                           END-STRING
                       END-IF
                   END-IF
      *
                   IF    ( SPA-Q01-TEL-TO-HOME2    =   1 )
      *
                       COMPUTE CNT-JYOKEN  =   CNT-JYOKEN  +   1
      *
                       PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
                       STRING  "(HOME_TEL2 LIKE '" DELIMITED   BY  SIZE
                           SPA-Q01-TEL (IDX1)(1: WRK-STR-LEN) 
                                                   DELIMITED   BY  SIZE
                       INTO    WRK-SQL
                       WITH    POINTER WRK-SQL-PNT
                       END-STRING
      *
                       IF    ( CNT-STR         =   ZERO )
                           STRING  "%')"           DELIMITED   BY  SIZE
                           INTO    WRK-SQL
                           WITH    POINTER WRK-SQL-PNT
                           END-STRING
                       ELSE
                           STRING  "')"            DELIMITED   BY  SIZE
                           INTO    WRK-SQL
                           WITH    POINTER WRK-SQL-PNT
                           END-STRING
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
      *
           .
       10011-TEL-SELECT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    »àË´¶èÊ¬¸¡º÷¾ò·ïÊÔ½¸½èÍý
      *****************************************************************
       10011-DEATH-SELECT-EDIT-SEC         SECTION.
      *
      *
           IF      ( SPA-Q01-DEATH         =   SPACE )
               GO  TO  10011-DEATH-SELECT-EDIT-EXT
           END-IF
      *
           MOVE    1                   TO  CNT-JYOKEN
      *
           PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
           IF    ( SPA-Q01-DEATH    =  "0" )
               STRING  "(DEATHKBN   <>  '1')"   DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           ELSE
               STRING  "(DEATHKBN   =  '1')"   DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
      *
           .
       10011-DEATH-SELECT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¿ÇÎÅ²ÊÊÔ½¸½èÍý
      *****************************************************************
       10011-SRYKA-SQL-EDIT-SEC            SECTION.
      *
           MOVE    1                   TO  CNT-JYOKEN
      *
           PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
           IF    ( FLG-RECECOM-SQL     =   1 )
               MOVE    ZERO            TO  FLG-RECECOM-SQL
               STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                       ".SRYKA IN ('00','" DELIMITED BY SIZE
                       SPA-Q01-SRYKA       DELIMITED BY SIZE
                       "')"                DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           ELSE
               STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                       ".SRYKA ='"         DELIMITED BY SIZE
                       SPA-Q01-SRYKA       DELIMITED BY SIZE
                       "'"                 DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
           .
       10011-SRYKA-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ¿ÇÎÅ¶èÊ¬ÊÔ½¸½èÍý
      *****************************************************************
       10011-SRYKBN-SQL-EDIT-SEC           SECTION.
      *
           MOVE    1                   TO  CNT-JYOKEN
      *
           PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
           EVALUATE    SPA-Q01-SRYKBN
           WHEN    "20"
           WHEN    "30"
               IF   ( SPA-Q01-SRYSYUKBN    NOT =   SPACE )
                   STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                           ".SRYKBN ='"        DELIMITED BY SIZE
                           SPA-Q01-SRYSYUKBN   DELIMITED BY SIZE
                           "'"                 DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               ELSE
                   STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                           ".SRYKBN LIKE'"     DELIMITED BY SIZE
                           SPA-Q01-SRYKBN(1:1) DELIMITED BY SIZE
                           "%'"                DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
           WHEN    "90"
               IF   ( SPA-Q01-SRYSYUKBN    NOT =   SPACE )
                   STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                           ".SRYKBN ='"        DELIMITED BY SIZE
                           SPA-Q01-SRYSYUKBN   DELIMITED BY SIZE
                           "'"                 DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               ELSE
                   STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                           ".SRYKBN in ('90','92')"
                                               DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
           WHEN    "80"
                   STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                           ".SRYKBN='80' AND " DELIMITED BY SIZE
                           WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                           ".SRYSYUKBN NOT IN ('830','840')"
                                               DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
           WHEN    "83"
                   STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                           ".SRYKBN='80' AND " DELIMITED BY SIZE
                           WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                           ".SRYSYUKBN='830'"  DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
           WHEN    "84"
                   STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                           ".SRYKBN='80' AND " DELIMITED BY SIZE
                           WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                           ".SRYSYUKBN='840'"  DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
           WHEN    OTHER
                   STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                           ".SRYKBN ='"        DELIMITED BY SIZE
                           SPA-Q01-SRYKBN      DELIMITED BY SIZE
                           "'"                 DELIMITED BY SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
           END-EVALUATE
      *
           PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
           .
       10011-SRYKBN-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    Æþ³°¶èÊ¬ÊÔ½¸½èÍý
      *****************************************************************
       10011-NYUGAIKBN-SQL-EDIT-SEC    SECTION.
      *
           MOVE    1                   TO  CNT-JYOKEN
      *
           PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
           IF    ( FLG-PTBYOMEI-SQL    =   1 )
      *
               MOVE    ZERO            TO  FLG-PTBYOMEI-SQL
      *
               STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                       ".NYUGAIKBN IN (' ','"
                                           DELIMITED BY SIZE
                       SPA-Q01-NYUGAIKBN   DELIMITED BY SIZE
                       "')"                DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           ELSE
               STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                       ".NYUGAIKBN ='"     DELIMITED BY SIZE
                       SPA-Q01-NYUGAIKBN   DELIMITED BY SIZE
                       "'"                 DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
           .
       10011-NYUGAIKBN-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÌ¾µ¿¤¤¥Õ¥é¥°ÊÔ½¸½èÍý
      *****************************************************************
       10011-UTAGAIFLG-SQL-EDIT-SEC    SECTION.
      *
           MOVE    1                   TO  CNT-JYOKEN
      *
           PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
           IF    ( SPA-Q01-UTAGAIKBN   =   "1" )
               STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                       ".UTAGAIFLG IN ('1','3')"
                                           DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           ELSE
               STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                       ".UTAGAIFLG NOT IN ('1','3')"
                                           DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
           .
       10011-UTAGAIFLG-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÌ¾µ¿¤¤¥Õ¥é¥°ÊÔ½¸½èÍý
      *****************************************************************
       10012-UTAGAIFLG-SQL-EDIT-SEC    SECTION.
      *
           MOVE    1                   TO  CNT-JYOKEN
      *
           PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
           IF    ( SPA-Q01-KYUSEIKBN   =   "1" )
               STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                       ".UTAGAIFLG IN ('2','3')"
                                           DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           ELSE
               STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                       ".UTAGAIFLG NOT IN ('2','3')"
                                           DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
           .
       10012-UTAGAIFLG-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÌ¾¼çÉÂÌ¾¥Õ¥é¥°ÊÔ½¸½èÍý
      *****************************************************************
       10012-SYUBYOFLG-SQL-EDIT-SEC    SECTION.
      *
           MOVE    1                   TO  CNT-JYOKEN
      *
           PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
           IF    ( SPA-Q01-SYUBYOKBN   =   "0" )
               STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                   ".SYUBYOFLG = ' '"      DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           ELSE
               STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                   ".SYUBYOFLG = '1'"      DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
           .
       10012-SYUBYOFLG-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂÌ¾Å¾µ¢¶èÊ¬ÊÔ½¸½èÍý
      *****************************************************************
       10011-TENKIKBN-SQL-EDIT-SEC     SECTION.
      *
           MOVE    1                   TO  CNT-JYOKEN
      *
           PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
           EVALUATE    SPA-Q01-TENKIKBN
           WHEN    "0"
      *
               STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                       ".TENKIKBN = ' '"   DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
      *
           WHEN    "9"
      *
               STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                       ".TENKIKBN <> ' '"  DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
      *
           WHEN    OTHER
      *
               STRING  WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                       ".TENKIKBN = '"     DELIMITED BY SIZE
                       SPA-Q01-TENKIKBN    DELIMITED BY SIZE
                       "'"                 DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
      *
           END-EVALUATE
      *
           PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
           IF    ( SPA-Q01-TENKIKBN    =   "1"
                                        OR "2"
                                        OR "3"
                                        OR "8"
                                        OR "9"
                                               )
      *
               PERFORM 900-SQL-JYOKEN-EDIT2-SEC
      *
               STRING  "("                 DELIMITED BY SIZE
                       WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                       ".TENKIYMD >= '"    DELIMITED BY SIZE
                       WRK-STTENKIYMD      DELIMITED BY SIZE
                       "') AND ("          DELIMITED BY SIZE
                       WRK-SQL-TBL-NAME    DELIMITED BY SPACE
                       ".TENKIYMD  <= '"   DELIMITED BY SIZE
                       WRK-EDTENKIYMD      DELIMITED BY SIZE
                       "')"                DELIMITED BY SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
      *
               PERFORM 900-SQL-JYOKEN-EDIT3-SEC
      *
           END-IF
      *
           .
       10011-TENKIKBN-SQL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    GROUP BYÊÔ½¸½èÍý
      *****************************************************************
       1001-GROUP-EDIT-SEC                 SECTION.
      *
           STRING  " GROUP BY "        DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER WRK-SQL-PNT
           END-STRING
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-ITEM-MAX )
      *
               IF    ( IDX0    >   1 )
                   STRING  ","         DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER   WRK-SQL-PNT
                   END-STRING
               ELSE
                   CONTINUE
               END-IF
      *
               STRING  CONST-ITEM     (IDX0)   DELIMITED   BY  SPACE
               INTO    WRK-SQL
               WITH    POINTER   WRK-SQL-PNT
               END-STRING
      *
           END-PERFORM
      *
           .
       1001-GROUP-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ÊÂ¤ÙÂØ¤¨½çÊÔ½¸½èÍý
      *****************************************************************
       1001-ORDER-EDIT-SEC                 SECTION.
      *
           MOVE    SPA-Q01-KMK (1)     TO  IDX-KMK1
           MOVE    SPA-Q01-KMK (2)     TO  IDX-KMK2
           MOVE    SPA-Q01-KMK (3)     TO  IDX-KMK3
      *
           MOVE    SPA-Q01-NARABE (1)  TO  IDX-SRT1
           MOVE    SPA-Q01-NARABE (2)  TO  IDX-SRT2
           MOVE    SPA-Q01-NARABE (3)  TO  IDX-SRT3
      *
           STRING  " ORDER BY "                DELIMITED   BY  SIZE
                   CONST-KMK-ITEM (IDX-KMK1)   DELIMITED   BY  SPACE
                   " "                         DELIMITED   BY  SIZE
                   CONST-ORDER (IDX-SRT1)
                                               DELIMITED   BY  SPACE
                   ","                         DELIMITED   BY  SIZE
                   CONST-KMK-ITEM (IDX-KMK2)   DELIMITED   BY  SPACE
                   " "                         DELIMITED   BY  SIZE
                   CONST-ORDER (IDX-SRT2)
                                               DELIMITED   BY  SPACE
                   ","                         DELIMITED   BY  SIZE
                   CONST-KMK-ITEM (IDX-KMK3)   DELIMITED   BY  SPACE
                   " "                         DELIMITED   BY  SIZE
                   CONST-ORDER (IDX-SRT3)
                                               DELIMITED   BY  SPACE
           INTO    WRK-SQL-ORDER
           WITH    POINTER WRK-SQL-ORDER-PNT
           END-STRING
      *
           IF    ( QSUB02-LIMITCONTROL     =   1 )
      *
               STRING  " LIMIT "                   DELIMITED   BY  SIZE
                       QSUB02-LIMIT                DELIMITED   BY  SIZE
                       " OFFSET "                  DELIMITED   BY  SIZE
                       QSUB02-OFFSET               DELIMITED   BY  SIZE
               INTO    WRK-SQL-ORDER
               WITH    POINTER WRK-SQL-ORDER-PNT
               END-STRING
           END-IF
      *
           .
      *
       1001-ORDER-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    £Ó£Ñ£Ì¾ò·ïÊÔ½¸½èÍý£±
      *****************************************************************
       900-SQL-JYOKEN-EDIT1-SEC            SECTION.
      *
           IF  ( FLG-JYOKEN        =   1 )
               STRING " AND "      DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER     WRK-SQL-PNT
               END-STRING
           END-IF
      *
           STRING  "(("                        DELIMITED   BY  SIZE
                   WRK-SQL-TBL-NAME            DELIMITED   BY  SPACE
                   ".HOSPNUM="                 DELIMITED   BY  SIZE
                   SPA-HOSPNUM                 DELIMITED   BY  SIZE
                   ")AND("                     DELIMITED   BY  SIZE
                   WRK-SQL-TBL-NAME            DELIMITED   BY  SPACE
                   ".PTID=A.PTID) "            DELIMITED   BY  SIZE
                   "AND "                      DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER     WRK-SQL-PNT
           END-STRING
      *
           .
       900-SQL-JYOKEN-EDIT1-EXT.
           EXIT.
      *****************************************************************
      *    £Ó£Ñ£Ì¾ò·ïÊÔ½¸½èÍý£²
      *****************************************************************
       900-SQL-JYOKEN-EDIT2-SEC            SECTION.
      *
           IF  ( CNT-JYOKEN    =   1 )
               IF  (   FLG-JYOKEN-AND  =   "1" )
                   STRING  " AND "         DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
      *
               STRING  "("                 DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           ELSE
               STRING  "OR "               DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           .
       900-SQL-JYOKEN-EDIT2-EXT.
           EXIT.
      *****************************************************************
      *    £Ó£Ñ£Ì¾ò·ïÊÔ½¸½èÍý£³
      *****************************************************************
       900-SQL-JYOKEN-EDIT3-SEC            SECTION.
      *
           IF  ( CNT-JYOKEN    >   0 )
               STRING  ")"      DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
               MOVE    1           TO  FLG-JYOKEN-AND
           END-IF
      *
           .
       900-SQL-JYOKEN-EDIT3-EXT.
           EXIT.
      *****************************************************************
      *    £Ó£Ñ£Ì¾ò·ïÊÔ½¸½èÍý£´
      *****************************************************************
       900-SQL-JYOKEN-EDIT4-SEC            SECTION.
      *
           STRING  ")"         DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER     WRK-SQL-PNT
           END-STRING
      *
           .
       900-SQL-JYOKEN-EDIT4-EXT.
           EXIT.
      *****************************************************************
      *    £Ó£Ñ£Ì¾ò·ïÊÔ½¸½èÍý£µ
      *****************************************************************
       900-SQL-JYOKEN-EDIT5-SEC            SECTION.
      *
           IF  ( FLG-JYOKEN        =   1 )
               STRING " AND "      DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER     WRK-SQL-PNT
               END-STRING
           END-IF
      *
           STRING  "("             DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER         WRK-SQL-PNT
           END-STRING
      *
           .
       900-SQL-JYOKEN-EDIT5-EXT.
           EXIT.
      *****************************************************************
      *    £Ó£Ñ£Ì¾ò·ïÊÔ½¸½èÍý£¶
      *****************************************************************
       900-SQL-JYOKEN-EDIT6-SEC            SECTION.
      *
           IF  ( FLG-JYOKEN-OR     =   1 )
               STRING " OR"        DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER     WRK-SQL-PNT
               END-STRING
           ELSE
               MOVE    1           TO  FLG-JYOKEN-OR
           END-IF
      *
           STRING  "("             DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER     WRK-SQL-PNT
           END-STRING
      *
           .
       900-SQL-JYOKEN-EDIT6-EXT.
           EXIT.
      *****************************************************************
      *    £Ó£Ñ£Ì¾ò·ïÊÔ½¸½èÍý£·
      *****************************************************************
       900-SQL-JYOKEN-EDIT7-SEC            SECTION.
      *
           IF  ( CNT-JYOKEN    =   1 )
               IF  (   FLG-JYOKEN-AND  =   "1" )
                   STRING  " AND "         DELIMITED   BY  SIZE
                   INTO    WRK-SQL
                   WITH    POINTER WRK-SQL-PNT
                   END-STRING
               END-IF
           ELSE
               STRING  ","                 DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER WRK-SQL-PNT
               END-STRING
           END-IF
      *
           .
       900-SQL-JYOKEN-EDIT7-EXT.
           EXIT.
      *****************************************************************
      *    £Ó£Ñ£Ì¾ò·ïÊÔ½¸½èÍý£¸
      *****************************************************************
       900-SQL-JYOKEN-EDIT8-SEC            SECTION.
      *
           IF  ( FLG-JYOKEN        =   1 )
               STRING " AND "      DELIMITED   BY  SIZE
               INTO    WRK-SQL
               WITH    POINTER     WRK-SQL-PNT
               END-STRING
           END-IF
      *
           STRING  "(("                        DELIMITED   BY  SIZE
                   WRK-SQL-TBL-NAME            DELIMITED   BY  SPACE
                   ".HOSPNUM="                 DELIMITED   BY  SIZE
                   SPA-HOSPNUM                 DELIMITED   BY  SIZE
                   ")AND("                     DELIMITED   BY  SIZE
                   WRK-SQL-TBL-NAME            DELIMITED   BY  SPACE
                   ".PTID=A.PTID) "            DELIMITED   BY  SIZE
           INTO    WRK-SQL
           WITH    POINTER     WRK-SQL-PNT
           END-STRING
      *
           .
       900-SQL-JYOKEN-EDIT8-EXT.
           EXIT.
      *****************************************************************
      *    Ê¸»úÎóÄ¹¼èÆÀ½èÍý
      *****************************************************************
       900-STR-LEN-GET-SEC                 SECTION.
      *
           PERFORM VARYING IDX-LEN FROM    WRK-STR-LEN BY  -1
               UNTIL   ( IDX-LEN   <   1)
                OR     ( WRK-STR (IDX-LEN : 1) NOT =   SPACE)
      *
               CONTINUE
           END-PERFORM
      *
           MOVE    IDX-LEN     TO  WRK-STR-LEN
      *
           .
       900-STR-LEN-GET-EXT.
           EXIT.
