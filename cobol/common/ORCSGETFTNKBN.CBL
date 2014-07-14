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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSGETFTNKBN.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¥ì¥»¥×¥È¡Ê¾ðÊóÏ¢·È¡Ë
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ÉéÃ´¶èÊ¬¼èÆÀ
      *  ´ÉÍý¼Ô            :
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  14/ 7/ 7    NACL¡Ý°ËÆ£    ¿·µ¬ºîÀ®
      *****************************************************************
      *
      *  Ê¿À®£²£°Ç¯£´·î²þÀµ°Ê¹ß¤ÎÉéÃ´¶èÊ¬¤òÊÖµÑ
      *  Ê¿À®£²£°Ç¯£³·î¤Þ¤Ç¤Ï¶õÇò¤òÊÖµÑ
      *
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *****************************************************************
      *
       DATA                        DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
       01  FLG-AREA.
           03  FLG-HKNCOMBI            PIC 9(01).
      *
       01  INDEX-AREA.
           03  IDX                     PIC 9(03).
           03  IDY                     PIC 9(03).
      *
       01  WRK-AREA.
           03  WRK-FTNKBN-AREA.
               05  WRK-FTNKBN          PIC X(01).
               05  WRK-KOHNUM          PIC 9(01)
                                           OCCURS  4.
      *
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
       LINKAGE                    SECTION.
           COPY    "CPORCSGETFTNKBN.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSGETFTNKBNAREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE  WRK-AREA
      *
           PERFORM 100-FTNKBN-HENSYU-SEC
           MOVE    WRK-FTNKBN          TO  ORCSGETFK-FTNKBN
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    ÉéÃ´¶èÊ¬ÊÔ½¸½èÍý
      *****************************************************************
       100-FTNKBN-HENSYU-SEC       SECTION.
      *
      *    ¼çÊÝ¸±¡Ü£³¼ï¸øÈñ¤Î¤È¤­³ºÅö¤ÎÉéÃ´¶èÊ¬¤¬¤Ê¤¤¤Î¤Ç
      *    ¡Ü£²¼ï¸øÈñ¤Î¤È¤­¤ÈÆ±¤¸ÉéÃ´¶èÊ¬¤òÊÔ½¸¤¹¤ë
      *
           INITIALIZE  WRK-FTNKBN-AREA
      *
      *    ÊÝ¸±ÁÈ¹ç¤»ÆÉ¹þ¤ß
           INITIALIZE  HKNCOMBI-REC
           MOVE    ORCSGETFK-HOSPNUM   TO  COMB-HOSPNUM
           MOVE    ORCSGETFK-PTID      TO  COMB-PTID
           MOVE    ORCSGETFK-HKNCOMBINUM
                                       TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           PERFORM 900-HKNCOMBI-READ-SEC
           IF      FLG-HKNCOMBI        =   1
               DISPLAY "HKNCONBI READ ERROR"
               GO  TO  100-FTNKBN-HENSYU-EXT
           END-IF
      *
      *    Ê¿À®£²£°Ç¯£´·î²þÀµ°Ê¹ß¤ÎÉéÃ´¶èÊ¬ÀßÄê
           IF      ORCSGETFK-SRYYM     >=  "200804"
               PERFORM 200-FTNKBN-HENSYU-200804-SEC
           END-IF
      *
      *???
      **   IF      WRK-FTNKBN          =   SPACE
      **       DISPLAY "FTNKBN ERR ##"
      **   END-IF
      *???
           .
       100-FTNKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ê¿À®£²£°Ç¯£´·î²þÀµ°Ê¹ß¤ÎÉéÃ´¶èÊ¬ÀßÄê½èÍý
      *****************************************************************
       200-FTNKBN-HENSYU-200804-SEC    SECTION.
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4
               IF      COMB-KOHHKNNUM (IDX)
                                       NOT =   SPACE
                   MOVE    1                   TO  WRK-KOHNUM (IDX)
               END-IF
           END-PERFORM
      *
      *???
      **     DISPLAY "RECE02(1)=" RECE02-KOHNUM (1) "(2)="
      **                          RECE02-KOHNUM (2)
      **             "(3)=" RECE02-KOHNUM (3) "##"
      **     DISPLAY "(1)=" WRK-KOHNUM (1) "(2)=" WRK-KOHNUM (2)
      **             "(3)=" WRK-KOHNUM (3) "##"
      *???
           IF      COMB-HKNNUM     NOT =   SPACE
               EVALUATE    WRK-KOHNUM (1)
                               ALSO    WRK-KOHNUM (2)
                                           ALSO    WRK-KOHNUM (3)
                                                   ALSO    WRK-KOHNUM(4)
      *            £±¼Ô
                   WHEN    0   ALSO    0   ALSO    0   ALSO    0
                       MOVE    "1"            TO  WRK-FTNKBN
      *            £²¼Ô
                   WHEN    1   ALSO    0   ALSO    0   ALSO    0
                       MOVE    "2"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    1   ALSO    0   ALSO    0
                       MOVE    "3"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    0   ALSO    1   ALSO    0
                       MOVE    "E"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    0   ALSO    0   ALSO    1
                       MOVE    "G"            TO  WRK-FTNKBN
      *            £³¼Ô
                   WHEN    1   ALSO    1   ALSO    0   ALSO    0
                       MOVE    "4"            TO  WRK-FTNKBN
                   WHEN    1   ALSO    0   ALSO    1   ALSO    0
                       MOVE    "M"            TO  WRK-FTNKBN
                   WHEN    1   ALSO    0   ALSO    0   ALSO    1
                       MOVE    "N"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    1   ALSO    1   ALSO    0
                       MOVE    "O"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    1   ALSO    0   ALSO    1
                       MOVE    "P"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    0   ALSO    1   ALSO    1
                       MOVE    "Q"            TO  WRK-FTNKBN
      *            £´¼Ô
                   WHEN    1   ALSO    1   ALSO    1   ALSO    0
                       MOVE    "V"            TO  WRK-FTNKBN
                   WHEN    1   ALSO    1   ALSO    0   ALSO    1
                       MOVE    "W"            TO  WRK-FTNKBN
                   WHEN    1   ALSO    0   ALSO    1   ALSO    1
                       MOVE    "X"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    1   ALSO    1   ALSO    1
                       MOVE    "Y"            TO  WRK-FTNKBN
      *            £µ¼Ô
                   WHEN    1   ALSO    1   ALSO    1   ALSO    1
                       MOVE    "9"            TO  WRK-FTNKBN
               END-EVALUATE
           ELSE
               EVALUATE    WRK-KOHNUM (1)
                               ALSO    WRK-KOHNUM (2)
                                           ALSO    WRK-KOHNUM (3)
                                                   ALSO    WRK-KOHNUM(4)
      *            £±¼Ô
                   WHEN    1   ALSO    0   ALSO    0   ALSO    0
                       MOVE    "5"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    1   ALSO    0   ALSO    0
                       MOVE    "6"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    0   ALSO    1   ALSO    0
                       MOVE    "B"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    0   ALSO    0   ALSO    1
                       MOVE    "C"            TO  WRK-FTNKBN
      *            £²¼Ô
                   WHEN    1   ALSO    1   ALSO    0   ALSO    0
                       MOVE    "7"            TO  WRK-FTNKBN
                   WHEN    1   ALSO    0   ALSO    1   ALSO    0
                       MOVE    "H"            TO  WRK-FTNKBN
                   WHEN    1   ALSO    0   ALSO    0   ALSO    1
                       MOVE    "I"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    1   ALSO    1   ALSO    0
                       MOVE    "J"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    1   ALSO    0   ALSO    1
                       MOVE    "K"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    0   ALSO    1   ALSO    1
                       MOVE    "L"            TO  WRK-FTNKBN
      *            £³¼Ô
                   WHEN    1   ALSO    1   ALSO    1   ALSO    0
                       MOVE    "R"            TO  WRK-FTNKBN
                   WHEN    1   ALSO    1   ALSO    0   ALSO    1
                       MOVE    "S"            TO  WRK-FTNKBN
                   WHEN    1   ALSO    0   ALSO    1   ALSO    1
                       MOVE    "T"            TO  WRK-FTNKBN
                   WHEN    0   ALSO    1   ALSO    1   ALSO    1
                       MOVE    "U"            TO  WRK-FTNKBN
      *            £´¼Ô
                   WHEN    1   ALSO    1   ALSO    1   ALSO    1
                       MOVE    "Z"            TO  WRK-FTNKBN
               END-EVALUATE
           END-IF
      *???
      **     IF      WRK-FTNKBN          =   SPACE
      **         DISPLAY "FTNKBN ERR ##"
      **     END-IF
      *???
           .
       200-FTNKBN-HENSYU-200804-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       900-HKNCOMBI-READ-SEC       SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNCOMBI
                   MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   INITIALIZE  HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE  HKNCOMBI-REC
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â£Ó£Å£Ì£Å£Ã£Ô½èÍý
      *****************************************************************
       900-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â£Æ£Å£Ô£Ã£È½èÍý
      *****************************************************************
       900-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
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
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¥¢¥¯¥»¥¹½èÍý
      *****************************************************************
       900-ORCDBMAIN-SEC           SECTION.
      *
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-ORCDBMAIN-EXT.
           EXIT.
      *
