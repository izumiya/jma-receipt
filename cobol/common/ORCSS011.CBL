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
       PROGRAM-ID.                     ORCSS011.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¶¦ÄÌ
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ¤È°åÎÅ¶èÊ¬¡¢£Á£Ä£ÌÅÀ¿ôÂÅÅöÀ­¥Á¥§¥Ã¥¯
      *  ´ÉÍý¼Ô            :
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  06/06/20    NACL¡ÝÂÀÅÄ    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  03.05.00    NACL-ÂÀÅÄ    07/05/28  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *
       WORKING-STORAGE                 SECTION.
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-RYOCOM              PIC 9(01).
           03  FLG-NYUINACCT           PIC 9(01).
           03  FLG-NYUINACT            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-RYOYO               PIC 9(01).
           03  FLG-TARGET              PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX-SRY                 PIC 9(05).
           03  IDX-RYO                 PIC 9(05).
           03  IDX-RYOYOCD             PIC 9(05).
           03  IDX-DAY                 PIC 9(05).
           03  IDX-MED                 PIC 9(05).
           03  IDXA                    PIC 9(05).
           03  IDXM                    PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-KJNYMD              PIC X(08).
           03  WRK-SRYCD.
               05  WRK-SRYCD9          PIC 9(09).
           03  WRK-MEDKBN              PIC 9(01).
           03  WRK-RYOYOCD-G.
               05  WRK-RYOYOCD         PIC X(09)
                                       OCCURS  6.
           03  WRK-LIFECD-G.
               05  WRK-LIFECD          PIC X(09)
                                       OCCURS  6.
      *
       01  TNACCT-AREA.
         02    TNACCT-RYOYOCD          PIC X(09)
                                       OCCURS  10.
         02    TNACCT-OCC              OCCURS  10.
           COPY    "CPNYUINACCT.INC"   REPLACING
                                       //NACCT-//  BY  //TNACCT-//.
      *
       01  TNACCT2-AREA.
         02    TNACCT2-RYOYOCD         PIC X(09)
                                       OCCURS  10.
         02    TNACCT2-OCC             OCCURS  10.
           COPY    "CPNYUINACCT.INC"   REPLACING
                                       //NACCT-//  BY  //TNACCT2-//.
      *
       01  TLIFE-AREA.
         02    TLIFE-RYOYOCD           PIC X(09)
                                       OCCURS  10.
         02    TLIFE-OCC               OCCURS  10.
           COPY    "CPNYUINACCT.INC"   REPLACING
                                       //NACCT-//  BY  //TLIFE-//.
      *
       01  TLIFE2-AREA.
         02    TLIFE2-RYOYOCD          PIC X(09)
                                       OCCURS  10.
         02    TLIFE2-OCC              OCCURS  10.
           COPY    "CPNYUINACCT.INC"   REPLACING
                                       //NACCT-//  BY  //TLIFE2-//.
      *
       01  WNACCT-AREA.
         02    WNACCT-RYOYOCD          PIC X(09)
                                       OCCURS  10.
         02    WNACCT-OCC              OCCURS  10.
           COPY    "CPNYUINACCT.INC"   REPLACING
                                       //NACCT-//  BY  //WNACCT-//.
      *
       01  CONST-AREA.
           03  CONST-H2204             PIC X(06)   VALUE "201004".
           03  CONST-ADL-TTLZAINUM     PIC 9(04)   VALUE 3000.
           03  CONST-NSRYACT-MAX       PIC 9(05)   VALUE 5.
           03  CONST-H2204-MATRIX-G.
               05   CONST-H2204-MATRIX-VAL.
                    07                 PIC 9(02)   VALUE 9.
                    07                 PIC 9(02)   VALUE 8.
                    07                 PIC 9(02)   VALUE 7.
                    07                 PIC 9(02)   VALUE 6.
                    07                 PIC 9(02)   VALUE 5.
                    07                 PIC 9(02)   VALUE 4.
                    07                 PIC 9(02)   VALUE 3.
                    07                 PIC 9(02)   VALUE 2.
                    07                 PIC 9(02)   VALUE 1.
               05   CONST-H2204-MATRIX-R
                           REDEFINES   CONST-H2204-MATRIX-VAL.
                    07  CONST-H2204-MATRIX-OCC     OCCURS 3.
                        09   CONST-H2204-MATRIX
                                       PIC 9(02)   OCCURS 3.
      *
           COPY    "CPRYOYOCD.INC".
      *
           COPY    "CPMEDKBN.INC".
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1001.INC".
           COPY    "CPSK5000.INC".
           COPY    "CPSK5015.INC".
      *
           COPY    "CPTENSU.INC".
      *
       01  RYOCOM-REC.
           COPY    "CPRYOCOM.INC".
      *
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑÎÎ°è
      *****************************************************************
      *
      *    ¥Þ¥·¥óÆüÉÕ¼èÆÀ¥µ¥Ö
           COPY    "CPORCSMCNDATE.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      ****************************************************************
       LINKAGE                         SECTION.
           COPY    "CPORCSS011.INC".
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       PROCEDURE                       DIVISION
           USING
           SS011-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END         =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           PERFORM 990-EXIT-PROGRAM-SEC
      *
           .
      *****************************************************************
      *    ½é´ü¡¡½èÍý
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  IDX-AREA
                                       FLG-AREA
                                       WRK-AREA
                                       TNACCT-AREA
                                       TNACCT2-AREA
                                       TLIFE-AREA
                                       TLIFE2-AREA
      *
           PERFORM 800-MCNDATE-SEC
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           MOVE    SS011-SRYYM     TO  WRK-KJNYMD
           MOVE    "01"            TO  WRK-KJNYMD(7:2)
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1001"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    WRK-KJNYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1001-REC
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5000"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    WRK-KJNYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5000-REC
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5015"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    WRK-KJNYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5015-REC
           IF    (  SS011-SRYYM    >=  CONST-H2204 )
               MOVE    "1"         TO  SYS-5015-ADLINPUTKBN
           END-IF
      *
           PERFORM 1001-INIT-TNACCT-SEC
      *
           IF    ( SS011-DAYFLG        =   ZERO )
               PERFORM 1001-EDIT-RYOYO-DAY-SEC
           END-IF
      *
           IF    ( SS011-DAYFLG        =   ZERO )
               PERFORM 1001-EDIT-MEDADL-SEC
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡²ñ·×ÂàÈòÎÎ°è½é´ü½èÍý
      *****************************************************************
       1001-INIT-TNACCT-SEC            SECTION.
      *
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    SS011-PTID          TO  NACCT-PTID
           MOVE    "00"                TO  NACCT-SRYKA
           MOVE    "90"                TO  NACCT-SRYKBN
           MOVE    SS011-SRYYM         TO  NACCT-SRYYM
           MOVE    "1"                 TO  NACCT-ZAISKBKBN
           MOVE    1                   TO  NACCT-SURYOUTOTAL

           MOVE    1                   TO  NACCT-MEISAISU
           MOVE    SMCNDATE-YMD        TO  NACCT-CREYMD
                                           NACCT-UPYMD
           MOVE    SMCNDATE-HMS        TO  NACCT-UPHMS
      *
           PERFORM VARYING IDX-RYO FROM    1   BY  1
                   UNTIL ( IDX-RYO >   10 )
      *
               MOVE    NYUINACCT-REC   TO  TNACCT-OCC (IDX-RYO)
                                           TNACCT2-OCC(IDX-RYO)
                                           TLIFE-OCC  (IDX-RYO)
                                           TLIFE2-OCC (IDX-RYO)
      *
           END-PERFORM
      *
           IF    ( SYS-1001-HOSPSBT    =   1 )
               MOVE  CONST-RYOYOCD(01) TO  TNACCT-RYOYOCD (01)
               MOVE  CONST-RYOYOCD(02) TO  TNACCT-RYOYOCD (02)
               MOVE  CONST-RYOYOCD(03) TO  TNACCT-RYOYOCD (03)
               MOVE  CONST-RYOYOCD(04) TO  TNACCT-RYOYOCD (04)
               MOVE  CONST-RYOYOCD(05) TO  TNACCT-RYOYOCD (05)
               MOVE  CONST-RYOYOCD(10) TO  TNACCT-RYOYOCD (10)
      *
               MOVE  CONST-RYOYOCD(11) TO  TLIFE-RYOYOCD (01)
               MOVE  CONST-RYOYOCD(12) TO  TLIFE-RYOYOCD (02)
               MOVE  CONST-RYOYOCD(13) TO  TLIFE-RYOYOCD (03)
               MOVE  CONST-RYOYOCD(14) TO  TLIFE-RYOYOCD (04)
               MOVE  CONST-RYOYOCD(15) TO  TLIFE-RYOYOCD (05)
               MOVE  CONST-RYOYOCD(20) TO  TLIFE-RYOYOCD (10)
      *
               IF    (  SS011-SRYYM        >=  CONST-H2204 )
                   MOVE  CONST-RYOYOCD(06) TO  TNACCT-RYOYOCD (06)
                   MOVE  CONST-RYOYOCD(07) TO  TNACCT-RYOYOCD (07)
                   MOVE  CONST-RYOYOCD(08) TO  TNACCT-RYOYOCD (08)
                   MOVE  CONST-RYOYOCD(09) TO  TNACCT-RYOYOCD (09)
      *
                   MOVE  CONST-RYOYOCD(16) TO  TLIFE-RYOYOCD (06)
                   MOVE  CONST-RYOYOCD(17) TO  TLIFE-RYOYOCD (07)
                   MOVE  CONST-RYOYOCD(18) TO  TLIFE-RYOYOCD (08)
                   MOVE  CONST-RYOYOCD(19) TO  TLIFE-RYOYOCD (09)
      *
                   MOVE  CONST-RYOYOCD(21) TO  TNACCT2-RYOYOCD(01)
                   MOVE  CONST-RYOYOCD(22) TO  TNACCT2-RYOYOCD(02)
                   MOVE  CONST-RYOYOCD(23) TO  TNACCT2-RYOYOCD(03)
                   MOVE  CONST-RYOYOCD(24) TO  TNACCT2-RYOYOCD(04)
                   MOVE  CONST-RYOYOCD(25) TO  TNACCT2-RYOYOCD(05)
                   MOVE  CONST-RYOYOCD(26) TO  TNACCT2-RYOYOCD(06)
                   MOVE  CONST-RYOYOCD(27) TO  TNACCT2-RYOYOCD(07)
                   MOVE  CONST-RYOYOCD(28) TO  TNACCT2-RYOYOCD(08)
                   MOVE  CONST-RYOYOCD(29) TO  TNACCT2-RYOYOCD(09)
      *
                   MOVE  CONST-RYOYOCD(30) TO  TLIFE2-RYOYOCD(01)
                   MOVE  CONST-RYOYOCD(31) TO  TLIFE2-RYOYOCD(02)
                   MOVE  CONST-RYOYOCD(32) TO  TLIFE2-RYOYOCD(03)
                   MOVE  CONST-RYOYOCD(33) TO  TLIFE2-RYOYOCD(04)
                   MOVE  CONST-RYOYOCD(34) TO  TLIFE2-RYOYOCD(05)
                   MOVE  CONST-RYOYOCD(35) TO  TLIFE2-RYOYOCD(06)
                   MOVE  CONST-RYOYOCD(36) TO  TLIFE2-RYOYOCD(07)
                   MOVE  CONST-RYOYOCD(37) TO  TLIFE2-RYOYOCD(08)
                   MOVE  CONST-RYOYOCD(38) TO  TLIFE2-RYOYOCD(09)
               END-IF
           ELSE
                   MOVE  CONST-RYOYOCD(39) TO  TNACCT-RYOYOCD(01)
                   MOVE  CONST-RYOYOCD(40) TO  TNACCT-RYOYOCD(02)
                   MOVE  CONST-RYOYOCD(41) TO  TNACCT-RYOYOCD(03)
                   MOVE  CONST-RYOYOCD(42) TO  TNACCT-RYOYOCD(04)
                   MOVE  CONST-RYOYOCD(43) TO  TNACCT-RYOYOCD(05)
                   MOVE  CONST-RYOYOCD(44) TO  TNACCT-RYOYOCD(10)
      *
                   MOVE  CONST-RYOYOCD(45) TO  TLIFE-RYOYOCD(01)
                   MOVE  CONST-RYOYOCD(46) TO  TLIFE-RYOYOCD(02)
                   MOVE  CONST-RYOYOCD(47) TO  TLIFE-RYOYOCD(03)
                   MOVE  CONST-RYOYOCD(48) TO  TLIFE-RYOYOCD(04)
                   MOVE  CONST-RYOYOCD(49) TO  TLIFE-RYOYOCD(05)
                   MOVE  CONST-RYOYOCD(50) TO  TLIFE-RYOYOCD(10)
           END-IF
      *
           PERFORM VARYING IDX-RYO FROM    1   BY  1
                   UNTIL ( IDX-RYO >   10 )
      *
               IF    ( TNACCT-RYOYOCD (IDX-RYO)    NOT =   SPACE )
                   MOVE    TNACCT-RYOYOCD (IDX-RYO)
                                       TO  WRK-SRYCD9
                   MOVE    WRK-SRYCD9  TO  TNACCT-SRYCDTOTAL (IDX-RYO)
               END-IF
      *
               IF    ( TNACCT2-RYOYOCD (IDX-RYO)   NOT =   SPACE )
                   MOVE    TNACCT2-RYOYOCD (IDX-RYO)
                                       TO  WRK-SRYCD9
                   MOVE    WRK-SRYCD9  TO  TNACCT2-SRYCDTOTAL (IDX-RYO)
               END-IF
      *
               IF    ( TLIFE-RYOYOCD (IDX-RYO)    NOT =   SPACE )
                   MOVE    TLIFE-RYOYOCD   (IDX-RYO)
                                       TO  WRK-SRYCD9
                   MOVE    WRK-SRYCD9  TO  TLIFE-SRYCDTOTAL (IDX-RYO)
               END-IF
      *
               IF    ( TLIFE2-RYOYOCD (IDX-RYO)    NOT =   SPACE )
                   MOVE    TLIFE2-RYOYOCD   (IDX-RYO)
                                       TO  WRK-SRYCD9
                   MOVE    WRK-SRYCD9  TO  TLIFE2-SRYCDTOTAL (IDX-RYO)
               END-IF
      *
           END-PERFORM
      *
           .
       1001-INIT-TNACCT-EXT.
           EXIT.
      *****************************************************************
      *    ÎÅÍÜ·¿Æþ±¡´ðËÜÎÁ»»Äê´ü´ÖÊÔ½¸½èÍý
      *****************************************************************
       1001-EDIT-RYOYO-DAY-SEC         SECTION.
      *
           INITIALIZE                  SS011-DAY-G
      *
           PERFORM 900-NYUINACCT-KEY36-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               PERFORM 10011-EDIT-RYOYO-DAY-SEC
      *
               PERFORM 900-NYUINACCT-KEY36-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key36"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       1001-EDIT-RYOYO-DAY-EXT.
           EXIT.
      *****************************************************************
      *    ÎÅÍÜ·¿Æþ±¡´ðËÜÎÁ»»Äê´ü´ÖÊÔ½¸½èÍý
      *****************************************************************
       10011-EDIT-RYOYO-DAY-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-RYOYO
      *
           PERFORM 900-NYUINACT-KEY9-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACT    NOT =   ZERO )
                    OR   ( FLG-RYOYO       NOT =   ZERO )
      *
               PERFORM VARYING IDX-SRY FROM    1   BY  1
                       UNTIL ( IDX-SRY >   CONST-NSRYACT-MAX )
                        OR   ( FLG-RYOYO   NOT =   ZERO )
      *
                   PERFORM 100111-RYOYOCD-CHK-SEC
                   IF    ( IDX-RYOYOCD NOT =   ZERO )
      *
                       MOVE    1   TO  FLG-RYOYO
      *
                       PERFORM VARYING IDX-DAY FROM    1   BY  1
                           UNTIL ( IDX-DAY >   31 )
      *
                           IF    ( NACCT-DAY (IDX-DAY) >   ZERO )
                               MOVE    CONST-RYOYOCDMSGID  (IDX-RYOYOCD)
                                           TO  SS011-DAY (IDX-DAY)
                               MOVE    CONST-RYOYOLIFE     (IDX-RYOYOCD)
                                           TO  SS011-LIFE (IDX-DAY)
                               MOVE    CONST-RYOYOKHN      (IDX-RYOYOCD)
                                           TO  SS011-KHN  (IDX-DAY)
                           END-IF
      *
                       END-PERFORM
                   END-IF
      *
               END-PERFORM
      *
               PERFORM 900-NYUINACT-KEY9-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       10011-EDIT-RYOYO-DAY-EXT.
           EXIT.
      *****************************************************************
      *    ÎÅÍÜ·¿Æþ±¡´ðËÜÎÁ¿ÇÎÅ¹Ô°Ù¥³¡¼¥É¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       100111-RYOYOCD-CHK-SEC          SECTION.
      *
           MOVE    ZERO            TO  IDX-RYOYOCD
           PERFORM VARYING IDX-RYO FROM    1   BY  1
                   UNTIL ( IDX-RYO >   CONST-RYOYOCD-MAX )
                    OR   ( IDX-RYOYOCD NOT =   ZERO )
      *
               IF    ( NSRY-SRYCD (IDX-SRY)    =   CONST-RYOYOCD
                                                              (IDX-RYO))
      *
                   MOVE    IDX-RYO TO   IDX-RYOYOCD
      *
               END-IF
      *
           END-PERFORM
      *
           .
       100111-RYOYOCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    °åÎÅ¶èÊ¬£Á£Ä£ÌÅÀ¿ôÊÔ½¸½èÍý
      *****************************************************************
       1001-EDIT-MEDADL-SEC         SECTION.
      *
           PERFORM 900-RYOCOM-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-RYOCOM   NOT =  ZERO )
      *
               IF    ( RYOCOM-ZAISKBKBN    =   "1" )
                   PERFORM 10011-EDIT-ADLTEN-SEC
               END-IF
      *
               IF    ( RYOCOM-ZAISKBKBN    =   "2" )
                   PERFORM 10011-EDIT-MEDKBN-SEC
               END-IF
      *
               PERFORM 900-RYOCOM-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ryocom"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   ZERO )
      *
               IF    ( SS011-DAY (IDX-DAY) >   ZERO )
                   IF    ( SS011-MEDKBN (IDX-DAY)  =   ZERO )
                       MOVE    1   TO  SS011-MEDKBN (IDX-DAY)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       1001-EDIT-MEDADL-EXT.
           EXIT.
      *****************************************************************
      *    °åÎÅ¶èÊ¬ÊÔ½¸½èÍý
      *****************************************************************
       10011-EDIT-MEDKBN-SEC         SECTION.
      *
           MOVE    ZERO            TO  WRK-MEDKBN
      *
           PERFORM VARYING IDX-MED FROM    1   BY  1
                   UNTIL ( IDX-MED >   MEDH20-MAX )
      *
               IF    ( RYOCOM-SRYCD
                                   =   MEDH20-CDL    (IDX-MED)
                                    OR MEDH20-SRYCDL (IDX-MED))
                   MOVE    MEDH20-MEDKBN (IDX-MED)
                                   TO  WRK-MEDKBN
                   MOVE    MEDH20-MAX  TO  IDX-MED
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
      *
               MOVE    ZERO        TO  FLG-TARGET
      *
               IF    ( SS011-DAY (IDX-DAY) >   ZERO )
                   IF    ( RYOCOM-MONTH    >   ZERO )
                       MOVE    1   TO  FLG-TARGET
                   ELSE
                       IF    ( RYOCOM-DAY (IDX-DAY)
                                           >   ZERO )
                           MOVE    1   TO  FLG-TARGET
                       END-IF
                   END-IF
               END-IF
      *
               IF    ( FLG-TARGET  =   1 )
                   IF    ( SS011-MEDKBN (IDX-DAY)  <   WRK-MEDKBN )
                       MOVE    WRK-MEDKBN  TO  SS011-MEDKBN(IDX-DAY)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       10011-EDIT-MEDKBN-EXT.
           EXIT.
      *****************************************************************
      *    £Á£Ä£ÌÅÀ¿ôÊÔ½¸½èÍý
      *****************************************************************
       10011-EDIT-ADLTEN-SEC         SECTION.
      *
           MOVE    ZERO        TO  FLG-TARGET
      *
           IF    ( SYS-5015-ADLINPUTKBN
                                       =   SPACE OR "1" )
               IF    ( RYOCOM-ZAINUM   =   CONST-ADL-TTLZAINUM )
                   CONTINUE
               ELSE
                   MOVE    1   TO  FLG-TARGET
               END-IF
           ELSE
               IF    ( RYOCOM-ZAINUM   =   CONST-ADL-TTLZAINUM )
                   MOVE    1   TO  FLG-TARGET
               END-IF
           END-IF
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
      *
               IF    ( SS011-DAY (IDX-DAY) >   ZERO )
      *
                   IF    ( RYOCOM-MONTH    >   ZERO )
                       COMPUTE SS011-ADLTEN (IDX-DAY)
                           =   SS011-ADLTEN (IDX-DAY)
                           +   RYOCOM-MONTH
                   ELSE
                       COMPUTE SS011-ADLTEN (IDX-DAY)
                           =   SS011-ADLTEN (IDX-DAY)
                           +   RYOCOM-DAY   (IDX-DAY)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       10011-EDIT-ADLTEN-EXT.
           EXIT.
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM 2001-CHK-RYOYO-SEC
      *
           IF    ( SS011-NYUUPDFLG     =   1 )
               PERFORM 2001-UPD-RYOYO-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       2001-CHK-RYOYO-SEC              SECTION.
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
      *
               DISPLAY IDX-DAY ":SS011-DAY [" SS011-DAY (IDX-DAY)   "]"
      *
               IF    ( SS011-DAY (IDX-DAY) >   ZERO )
      *
                   MOVE    SPACE       TO  WRK-RYOYOCD-G
                   MOVE    ZERO        TO  IDX-RYO
      *
                   IF    ( SS011-DAY (IDX-DAY) =   10 )
                               MOVE    10      TO  IDX-RYO
                   ELSE
                       IF    ( SS011-SRYYM    >=   CONST-H2204 )
                        AND  ( SPA-HOSPSBT     =   1 )
                           MOVE    SS011-MEDKBN (IDX-DAY)
                                               TO  IDXM
                           EVALUATE    TRUE
                           WHEN  ( SS011-ADLTEN (IDX-DAY)  >=  23 )
                               MOVE    3       TO  IDXA
                           WHEN  ( SS011-ADLTEN (IDX-DAY)  >=  11 )
                               MOVE    2       TO  IDXA
                           WHEN    OTHER
                               MOVE    1       TO  IDXA
                           END-EVALUATE
                           MOVE    CONST-H2204-MATRIX (IDXM IDXA)
                                               TO  IDX-RYO
                       ELSE
                           EVALUATE    SS011-MEDKBN (IDX-DAY)
                           WHEN    3
                                   MOVE    1   TO  IDX-RYO
                           WHEN    2
                               IF    ( SS011-ADLTEN (IDX-DAY)
                                              >=   11 )
                                   MOVE    2   TO  IDX-RYO
                               ELSE
                                   MOVE    3   TO  IDX-RYO
                               END-IF
                           WHEN    OTHER
                               IF    ( SS011-ADLTEN (IDX-DAY)
                                              >=   23 )
                                   MOVE    4   TO  IDX-RYO
                               ELSE
                                   MOVE    5   TO  IDX-RYO
                               END-IF
                           END-EVALUATE
                       END-IF
                   END-IF
      *
                   IF    ( SS011-KHN  (IDX-DAY)    =   1 )
                       IF    ( SS011-LIFE (IDX-DAY)    =   ZERO )
                           MOVE    IDX-RYO TO  SS011-DAY2 (IDX-DAY)
                           MOVE    TNACCT-RYOYOCD (IDX-RYO)
                                       TO  SS011-SRYCD(IDX-DAY)
                           MOVE    1   TO  TNACCT-DAY (IDX-RYO IDX-DAY)
                           COMPUTE TNACCT-ZAIKAISU (IDX-RYO)
                               =   TNACCT-ZAIKAISU (IDX-RYO)
                               +   1
                       ELSE
                           MOVE    IDX-RYO TO  SS011-DAY2 (IDX-DAY)
                           MOVE    TLIFE-RYOYOCD  (IDX-RYO)
                                       TO  SS011-SRYCD(IDX-DAY)
                           MOVE    1   TO  TLIFE-DAY (IDX-RYO IDX-DAY)
                           COMPUTE TLIFE-ZAIKAISU (IDX-RYO)
                               =   TLIFE-ZAIKAISU (IDX-RYO)
                               +   1
                       END-IF
      *
                   ELSE
                       IF    ( SS011-LIFE (IDX-DAY)    =   ZERO )
                           MOVE    IDX-RYO TO  SS011-DAY2 (IDX-DAY)
                           MOVE    TNACCT2-RYOYOCD (IDX-RYO)
                                       TO  SS011-SRYCD(IDX-DAY)
                           MOVE    1   TO  TNACCT2-DAY (IDX-RYO IDX-DAY)
                           COMPUTE TNACCT2-ZAIKAISU (IDX-RYO)
                               =   TNACCT2-ZAIKAISU (IDX-RYO)
                               +   1
                       ELSE
                           MOVE    IDX-RYO TO  SS011-DAY2 (IDX-DAY)
                           MOVE    TLIFE2-RYOYOCD  (IDX-RYO)
                                       TO  SS011-SRYCD(IDX-DAY)
                           MOVE    1   TO  TLIFE2-DAY (IDX-RYO IDX-DAY)
                           COMPUTE TLIFE2-ZAIKAISU (IDX-RYO)
                               =   TLIFE2-ZAIKAISU (IDX-RYO)
                               +   1
                       END-IF
      *
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       2001-CHK-RYOYO-EXT.
           EXIT.
      *****************************************************************
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁÆþ±¡¿ÇÎÅ²ñ·×¡¦¿ÇÎÅ¹Ô°ÙÊÔ½¸½èÍý
      *****************************************************************
       2001-UPD-RYOYO-SEC         SECTION.
      *
           PERFORM 20011-DEL-RYOYO-SEC
      *
           PERFORM 20011-INS-RYOYO-SEC
      *
           .
       2001-UPD-RYOYO-EXT.
           EXIT.
      *****************************************************************
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁÆþ±¡¿ÇÎÅ²ñ·×¡¦¿ÇÎÅ¹Ô°Ùºï½ü½èÍý
      *****************************************************************
       20011-DEL-RYOYO-SEC         SECTION.
      *
           PERFORM 900-NYUINACCT-KEY36-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               MOVE    ZERO           TO   FLG-RYOYO
      *
               PERFORM 200111-DEL-NYUINACT-SEC
               IF    ( FLG-RYOYO      NOT  =    ZERO )
                   MOVE    NYUINACCT-REC   TO  MCPDATA-REC
                   MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBDELETE-SEC
                   IF    ( MCP-RC      NOT =   ZERO )
                       MOVE    2           TO  SS011-RC
                   END-IF
               END-IF
      *
               PERFORM 900-NYUINACCT-KEY36-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key36"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-DEL-RYOYO-EXT.
           EXIT.
      *****************************************************************
      *    ÎÅÍÜ·¿Æþ±¡´ðËÜÎÁÆþ±¡¿ÇÎÅ¹Ô°Ùºï½ü½èÍý
      *****************************************************************
       200111-DEL-NYUINACT-SEC         SECTION.
      *
           PERFORM 900-NYUINACT-KEY9-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACT    NOT =   ZERO )
                    OR   ( FLG-RYOYO       NOT =   ZERO )
      *
               PERFORM VARYING IDX-SRY FROM    1   BY  1
                       UNTIL ( IDX-SRY >   CONST-NSRYACT-MAX )
                        OR   ( FLG-RYOYO   NOT =   ZERO )
      *
                   PERFORM 100111-RYOYOCD-CHK-SEC
                   IF    ( IDX-RYOYOCD NOT =   ZERO )
      *
                       MOVE    1               TO  FLG-RYOYO
      *
                       MOVE    NYUINACT-REC    TO  MCPDATA-REC
                       MOVE    "tbl_nyuinact"  TO  MCP-TABLE
                       MOVE    "del11"         TO  MCP-PATHNAME
                       PERFORM 910-DBDELETE-SEC
                       IF    ( MCP-RC      NOT =   ZERO )
                           MOVE    1           TO  SS011-RC
                           PERFORM 990-EXIT-PROGRAM-SEC
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
               PERFORM 900-NYUINACT-KEY9-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       200111-DEL-NYUINACT-EXT.
           EXIT.
      *****************************************************************
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁÆþ±¡¿ÇÎÅ²ñ·×¡¦¿ÇÎÅ¹Ô°ÙÄÉ²Ã½èÍý
      *****************************************************************
       20011-INS-RYOYO-SEC                 SECTION.
      *
           MOVE    TNACCT-AREA         TO  WNACCT-AREA
           PERFORM 20012-INS-RYOYO-SEC
      *
           MOVE    TNACCT2-AREA        TO  WNACCT-AREA
           PERFORM 20012-INS-RYOYO-SEC
      *
           MOVE    TLIFE-AREA          TO  WNACCT-AREA
           PERFORM 20012-INS-RYOYO-SEC
      *
           MOVE    TLIFE2-AREA         TO  WNACCT-AREA
           PERFORM 20012-INS-RYOYO-SEC
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    5               TO  SS011-RC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       20011-INS-RYOYO-EXT.
           EXIT.
      *****************************************************************
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁÆþ±¡¿ÇÎÅ²ñ·×¡¦¿ÇÎÅ¹Ô°ÙÄÉ²Ã½èÍý
      *****************************************************************
       20012-INS-RYOYO-SEC                 SECTION.
      *
           PERFORM VARYING  IDX-RYO    FROM    1   BY  1
                   UNTIL  ( IDX-RYO    >   10 )
      *
               MOVE    ZERO            TO  FLG-TARGET
      *
               IF    ( IDX-RYO         =   10 )
                   IF    ( WNACCT-ZAIKAISU (IDX-RYO)   >   ZERO )
                       MOVE    1       TO  FLG-TARGET
                   END-IF
               ELSE
                   IF    ( SYS-5000-RYOYODSPKBN    =    "1" )
                       IF    ( WNACCT-ZAIKAISU (1) >    ZERO )
                        OR   ( WNACCT-ZAIKAISU (2) >    ZERO )
                        OR   ( WNACCT-ZAIKAISU (3) >    ZERO )
                        OR   ( WNACCT-ZAIKAISU (4) >    ZERO )
                        OR   ( WNACCT-ZAIKAISU (5) >    ZERO )
                        OR   ( WNACCT-ZAIKAISU (6) >    ZERO )
                        OR   ( WNACCT-ZAIKAISU (7) >    ZERO )
                        OR   ( WNACCT-ZAIKAISU (8) >    ZERO )
                        OR   ( WNACCT-ZAIKAISU (9) >    ZERO )
                           MOVE    1       TO  FLG-TARGET
                       END-IF
                   ELSE
                       IF    ( WNACCT-ZAIKAISU (IDX-RYO)   >   ZERO )
                           MOVE    1       TO  FLG-TARGET
                       END-IF
                   END-IF
               END-IF
      *
               IF    ( WNACCT-RYOYOCD (IDX-RYO)    =   SPACE )
                           MOVE    ZERO    TO  FLG-TARGET
               END-IF
      *
               DISPLAY IDX-RYO ":" WNACCT-RYOYOCD (IDX-RYO)
                        "[" FLG-TARGET "]"
      *
               IF    ( FLG-TARGET  =   1 )
                   MOVE    WNACCT-OCC (IDX-RYO)
                                           TO  NYUINACCT-REC
                   MOVE    WNACCT-RYOYOCD (IDX-RYO)
                                           TO  WRK-SRYCD
                   PERFORM 200111-INS-NACCT-SEC
               END-IF
           END-PERFORM
      *
           .
       20012-INS-RYOYO-EXT.
           EXIT.
      *****************************************************************
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁÆþ±¡¿ÇÎÅ²ñ·×¡¦¿ÇÎÅ¹Ô°ÙÄÉ²Ã½èÍý
      *****************************************************************
       200111-INS-NACCT-SEC            SECTION.
      *
           PERFORM 900-TENSU-KEY-SEL-SEC
      *
           COMPUTE PTINF-MAXZAINUM
               =   PTINF-MAXZAINUM +   1
      *
           MOVE    TNS-TEN         TO  NACCT-ZAITEN
                                       NACCT-SYUTEN1
           MOVE    PTINF-MAXZAINUM TO  NACCT-ZAINUM
           MOVE    NYUINACCT-REC   TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    3           TO  SS011-RC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           INITIALIZE                  NYUINACT-REC
           MOVE    NACCT-HOSPNUM   TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID      TO  NSRY-PTID
           MOVE    NACCT-SRYKA     TO  NSRY-SRYKA
           MOVE    NACCT-SRYYM     TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM    TO  NSRY-ZAINUM
           MOVE    1               TO  NSRY-RENNUM
           MOVE    "900"           TO  NSRY-SRYSYUKBN
           MOVE    NACCT-SRYKBN    TO  NSRY-SRYKBN
           MOVE    WRK-SRYCD       TO  NSRY-SRYCD (1)
           MOVE    NACCT-CREYMD    TO  NSRY-CREYMD
           MOVE    NACCT-UPYMD     TO  NSRY-UPYMD
           MOVE    NACCT-UPHMS     TO  NSRY-UPHMS
           MOVE    NYUINACT-REC    TO  MCPDATA-REC
           MOVE    "tbl_nyuinact"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    4           TO  SS011-RC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       200111-INS-NACCT-EXT.
           EXIT.
      *****************************************************************
      *    ½ªÎ»½èÍý
      *****************************************************************
       300-END-SEC                    SECTION.
      *
      *    PERFORM 800-DISPLAY-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    DISPLAY½èÍý
      *****************************************************************
       800-DISPLAY-SEC                 SECTION.
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
      *
             IF  ( SS011-DAY (IDX-DAY)     >   ZERO )
      *
               MOVE    SS011-SRYCD (IDX-DAY)
                                       TO  WRK-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
      *
               DISPLAY IDX-DAY
                       ":LIFE["    SS011-LIFE  (IDX-DAY)
                       "]DAY["     SS011-DAY   (IDX-DAY)
                       "]DAY2["    SS011-DAY2  (IDX-DAY)
                       "]ADLTEN["  SS011-ADLTEN(IDX-DAY)
                       "]MEDKBN["  SS011-MEDKBN(IDX-DAY)
                       "]SRYCD["   SS011-SRYCD (IDX-DAY)
                       "]NAME[" TNS-NAME "]"
      *
             END-IF
      *
           END-PERFORM
      *
           .
       800-DISPLAY-EXT.
           EXIT.
      *****************************************************************
      *    ¥Þ¥·¥óÆüÉÕ¼èÆÀ¥µ¥Ö
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¸¡º÷½èÍý
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC  SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡²ñ·×¸¡º÷½èÍý
      *****************************************************************
       900-NYUINACCT-KEY36-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SS011-PTID          TO  NACCT-PTID
           MOVE    SS011-SRYYM         TO  NACCT-SRYYM
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key36"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  NYUINACCT-REC
           ELSE
               INITIALIZE              NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       900-NYUINACCT-KEY36-SEL-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡²ñ·×FETCH½èÍý
      *****************************************************************
       900-NYUINACCT-KEY36-FET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key36"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  NYUINACCT-REC
           ELSE
               INITIALIZE              NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
      *
           .
       900-NYUINACCT-KEY36-FET-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¸¡º÷½èÍý
      *****************************************************************
       900-NYUINACT-KEY9-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACT
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACT-REC
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1               TO  FLG-NYUINACT
           END-IF
      *
           .
       900-NYUINACT-KEY9-SEL-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°ÙFETCH½èÍý
      *****************************************************************
       900-NYUINACT-KEY9-FET-SEC       SECTION.
      *
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACT-REC
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1               TO  FLG-NYUINACT
           END-IF
      *
           .
       900-NYUINACT-KEY9-FET-EXT.
           EXIT.
      *****************************************************************
      *    ÎÅÍÜ¥ì¥»¥×¥È¥³¥á¥ó¥È¥Þ¥¹¥¿¸¡º÷½èÍý
      *****************************************************************
       900-RYOCOM-KEY2-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-RYOCOM
      *
           INITIALIZE                  RYOCOM-REC
           MOVE    SPA-HOSPNUM     TO  RYOCOM-HOSPNUM
           MOVE    "1"             TO  RYOCOM-NYUGAIKBN
           MOVE    SS011-PTID      TO  RYOCOM-PTID
           MOVE    SS011-SRYYM     TO  RYOCOM-SRYYM
           MOVE    RYOCOM-REC      TO  MCPDATA-REC
           MOVE    "tbl_ryocom"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  RYOCOM-REC
           ELSE
               MOVE    1           TO  FLG-RYOCOM
               INITIALIZE              RYOCOM-REC
           END-IF
      *
           .
       900-RYOCOM-KEY2-SEL-EXT.
            EXIT.
      *****************************************************************
      *    ÎÅÍÜ¥ì¥»¥×¥È¥³¥á¥ó¥È¥Þ¥¹¥¿FETCH½èÍý
      *****************************************************************
       900-RYOCOM-KEY2-FET-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-RYOCOM
      *
           MOVE    "tbl_ryocom"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  RYOCOM-REC
           ELSE
               MOVE    1           TO  FLG-RYOCOM
               INITIALIZE              RYOCOM-REC
           END-IF
      *
           .
       900-RYOCOM-KEY2-FET-EXT.
            EXIT.
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¸¡º÷½èÍý
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           INITIALIZE                  TNS-TENSU-REC
      *
           MOVE    WRK-SRYCD       TO  TNS-SRYCD
           MOVE    WRK-KJNYMD      TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
              INITIALIZE               TNS-TENSU-REC
              MOVE    1            TO  FLG-TENSU
           END-IF
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿¥¯¥í¡¼¥º
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSU-KEY-SEL-EXT.
            EXIT.
      *****************************************************************
      *    ´µ¼Ô¾ðÊó¸¡º÷½èÍý
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SS011-PTID          TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â¸¡º÷½èÍý¡ÊFHETCH¤â¹Ô¤¦)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â¸¡º÷½èÍý¡ÊFHETCH¤Ï¹Ô¤ï¤Ê¤¤)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£ÂÆÉ¤ß¹þ¤ß½èÍý
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â£Ä£Å£Ì£Å£Ô£Å½èÍý
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â£É£Î£Ó£Å£Ò£Ô½èÍý
      *****************************************************************
       910-DBINSERT-SEC                 SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â£Õ£Ð£Ä£Á£Ô£Å½èÍý
      *****************************************************************
       910-DBUPDATE-SEC                 SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    EXIT PROGRAM
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           DISPLAY "SS011-RC = "   SS011-RC
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
