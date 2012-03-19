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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCHCN02SUB.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : Æþ±¡
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : Âà±¡¾ÚÌÀ½ñÍÑÆþ±¡¼èÆÀ¥µ¥Ö
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  06/06/08    NACL¡Ý¾®Æ¦Âô  ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  03.05.00    NACL-Â¿¡¹Ç¼  07/05/09  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
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
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-NYUINACT                PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-MATCH                   PIC 9(01).
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
           03  IDX1                        PIC 9(04).
           03  IDX2                        PIC 9(04).
           03  IDX3                        PIC 9(04).
           03  IDX4                        PIC 9(04).
           03  IDX5                        PIC 9(04).
           03  IDX6                        PIC 9(04).
           03  IDX7                        PIC 9(04).
           03  NYUIN-IDX                   PIC 9(04).
           03  ST-IDX                      PIC 9(04).
           03  ED-IDX                      PIC 9(04).
      *
      *    Æþ±¡ÆüÃÖ´¹¤¨ÍÑ
           03  WRK-DDX                     PIC X(02).
           03  WRK-DD9          REDEFINES  WRK-DDX
                                           PIC 9(02).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
      *
           03  WRK-GAIHAKU-SRYCD           PIC X(09).
           03  WRK-MEISAISU                PIC 9(03).
           03  WRK-KSNKBN                  PIC 9(03).
      *
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡Ý
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿¡Ý
           COPY    "CPTENSU.INC".
      *
      *    ´µ¼Ô¥Þ¥¹¥¿¡Ý
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ´µ¼ÔÆþ±¡ÍúÎò¥Þ¥¹¥¿¡Ý
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *   ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ÄÌ»»Æü¿ô¼èÆÀ¥µ¥Ö
           COPY "CPORCSTUSAN.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *    Âà±¡¾ÚÌÀ½ñÍÑÆþ±¡¼èÆÀ¥Ñ¥é¥á¥¿
           COPY    "CPORCHCN02SUB.INC".
      *
       PROCEDURE                   DIVISION    USING
            ORCHCN02SUB-AREA
            SPA-AREA
           .
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  LNK-ORCHCN02SUB-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-ORCHCN02SUB-RC
      *
      *    ¥Ñ¥é¥á¡¼¥¿¥Á¥§¥Ã¥¯
           PERFORM 100-PRM-CHK-SEC
           IF    ( LNK-ORCHCN02SUB-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    ¼ç½èÍý
           IF      LNK-ORCHCN02SUB-RC      =    ZERO
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
      *    ¥Ñ¥é¥á¡¼¥¿¥Á¥§¥Ã¥¯
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    »»ÄêÇ¯·î¥Á¥§¥Ã¥¯
           IF    ( LNK-ORCHCN02SUB-HOSPNUM   =   ZERO  )   OR
                 ( LNK-ORCHCN02SUB-PTID      =   SPACE )   OR
                 ( LNK-ORCHCN02SUB-NYUINYMD  =   SPACE )   OR
                 ( LNK-ORCHCN02SUB-TAIINYMD  =   SPACE )
               MOVE    1               TO  LNK-ORCHCN02SUB-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼¸¡º÷
           INITIALIZE                       PTINF-REC
           MOVE    LNK-ORCHCN02SUB-HOSPNUM  TO  PTINF-HOSPNUM
           MOVE    LNK-ORCHCN02SUB-PTID     TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    1               TO  LNK-ORCHCN02SUB-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
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
      *Æþ±¡ÍúÎò¤«¤é¼«±¡Îò¤ÎÆü¿ô¼èÆÀ¡ÊÆþ±¡ÎòºîÀ®µ¡Ç½¤Ë¤è¤ëÍúÎòÊ¬¤Î
      *                              Æü¿ô¼èÆÀ¡Ë
           INITIALIZE                           PTNYUINRRK-REC
           MOVE    LNK-ORCHCN02SUB-HOSPNUM  TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-ORCHCN02SUB-PTID     TO  PTNYUINRRK-PTID
           MOVE    LNK-ORCHCN02SUB-NYUINYMD TO  PTNYUINRRK-NYUINYMD
           MOVE    "tbl_ptnyuinrrk"         TO  MCP-TABLE
           MOVE    "key48"                  TO  MCP-PATHNAME
           PERFORM 910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK          =   ZERO
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key48"             TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
               IF    FLG-PTNYUINRRK        =   ZERO
                   ADD   PTNYUINRRK-TAISYONISSU
                                       TO   LNK-ORCHCN02SUB-NISSU(1)
               END-IF
           END-IF
      *    ´µ¼ÔÆþ±¡ÍúÎò¥¯¥í¡¼¥º
           MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
           MOVE    "key48"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *Æþ±¡ÎÁ¤Î¼èÆÀ
           INITIALIZE                           NYUINACCT-REC
           MOVE    LNK-ORCHCN02SUB-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-ORCHCN02SUB-PTID     TO  NACCT-PTID
           MOVE    LNK-ORCHCN02SUB-NYUINYMD(1:6)
                                            TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"          TO  MCP-TABLE
           MOVE    "key34"                  TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key34"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
      *
                   IF   (LNK-ORCHCN02SUB-NYUINYMD(1:6)  <= NACCT-SRYYM)
                                         AND
                        (LNK-ORCHCN02SUB-TAIINYMD(1:6)  >= NACCT-SRYYM)
      *                ³ºÅöÆþ±¡¹Ô°Ù¥Æ¡¼¥Ö¥ë¤ÎÆÉ¤ß¹þ¤ß
                       PERFORM    210-NYUINACT-GET-SEC
      *                Æþ±¡´ðËÜÎÁÃ±°Ì¤ÎÆþ±¡Æü¿ô¥«¥¦¥ó¥È
                       PERFORM    210-NYUIN-COUNT-SEC
                   END-IF
      *
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key34"                 TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key34"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *
      *ÁªÄêÂÐ¾ÝÆü¿ô¤Î¼èÆÀ
           INITIALIZE                       ORCSTUSANAREA
           MOVE    LNK-ORCHCN02SUB-HOSPNUM  TO  STUSAN-HOSPNUM
           MOVE    LNK-ORCHCN02SUB-PTID     TO  STUSAN-PTID
           MOVE    LNK-ORCHCN02SUB-TAIINYMD TO  STUSAN-KJNYMD
           CALL    "ORCSTUSAN"     USING        ORCSTUSANAREA
                                                SPA-AREA
           MOVE    STUSAN-NISSU05           TO  LNK-ORCHCN02SUB-TKIKAN
      *
      *
      *   ·ë²ÌÉ½¼¨
      *     PERFORM    VARYING   IDX1   FROM  1  BY  1
      *                UNTIL     (IDX1   >     5)
      *          DISPLAY  "IDX1 = " IDX1
      *          DISPLAY  "LNK-ORCHCN02SUB-SYUBETUCD = "
      *                    LNK-ORCHCN02SUB-SYUBETUCD(IDX1)
      *          DISPLAY  "LNK-ORCHCN02SUB-SYUBETUMEI = "
      *                    LNK-ORCHCN02SUB-SYUBETUMEI(IDX1)
      *          DISPLAY  "LNK-ORCHCN02SUB-NISSU = "
      *                    LNK-ORCHCN02SUB-NISSU(IDX1)
      *     END-PERFORM
            DISPLAY  "LNK-ORCHCN02SUB-TKIKAN = " LNK-ORCHCN02SUB-TKIKAN
            DISPLAY  "STUSAN-RC = " STUSAN-RC
      *
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡ÎÁÃ±°Ì¤ÎÆü¿ô¥«¥¦¥ó¥È
      *****************************************************************
       210-NYUIN-COUNT-SEC            SECTION.
      *
           MOVE       ZERO      TO    FLG-MATCH
      *
           PERFORM    VARYING   NYUIN-IDX   FROM   1   BY   1
             UNTIL   (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX) = SPACE) OR
                     (FLG-MATCH        =   1 )     OR
                     (NYUIN-IDX    >   10)
      *        ÅÀ¿ô¥Þ¥¹¥¿¤è¤êÆþ±¡ÎÁÌ¾¾Î¤Î¼èÆÀ¡Ê¥ï¡¼¥¯¤È¤ÎÈæ³ÓÍÑ¡Ë
               INITIALIZE    TNS-TENSU-REC
               MOVE   NSRY-SRYCD(1)   TO    TNS-SRYCD
               MOVE   NACCT-SRYYM     TO    TNS-YUKOSTYMD(1:6)
                                            TNS-YUKOEDYMD(1:6)
               MOVE   "01"            TO    TNS-YUKOSTYMD(7:2)
                                            TNS-YUKOEDYMD(7:2)
               PERFORM    910-TENSU-READ-SEC
               EVALUATE   NSRY-SRYCD(1)
                   WHEN   "111111111"
                      MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±"  TO   TNS-NAME
                   WHEN   "222222222"
                      MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²"  TO   TNS-NAME
                   WHEN   "333333333"
                      MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³"  TO   TNS-NAME
                   WHEN   "111111119"
                      MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                                                       TO   TNS-NAME
                   WHEN   "222222229"
                      MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                                                       TO   TNS-NAME
                   WHEN   "333333339"
                      MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                                                       TO   TNS-NAME
                   WHEN   "111111199"
                      MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±"
                                                       TO   TNS-NAME
                   WHEN   "222222299"
                      MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²"
                                                       TO   TNS-NAME
                   WHEN   "333333399"
                      MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³"
                                                       TO   TNS-NAME
                   WHEN   "111111999"
               MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                                                       TO   TNS-NAME
                   WHEN   "222222999"
               MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                                                       TO   TNS-NAME
                   WHEN   "333333999"
               MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                                                       TO   TNS-NAME
               END-EVALUATE
               IF    (NACCT-SRYYM      >=   "200604")    AND
                     (NSRY-SRYCD(1)    =    "190097010"  OR
                                            "190097110"  OR
                                            "190119510"  OR
                                            "190119610")
                   MOVE   "111111111"     TO    NSRY-SRYCD(1)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±"  TO   TNS-NAME
               END-IF
               IF    (NACCT-SRYYM      >=   "200604")    AND
                     (NSRY-SRYCD(1)    =    "190097210"  OR
                                            "190097310"  OR
                                            "190119710"  OR
                                            "190119810")
                   MOVE   "222222222"     TO    NSRY-SRYCD(1)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²"  TO   TNS-NAME
               END-IF
               IF    (NACCT-SRYYM      >=   "201004")    AND
                     (NSRY-SRYCD(1)    =    "190135110"  OR
                                            "190135210"  OR
                                            "190135310")
                   MOVE   "333333333"     TO    NSRY-SRYCD(1)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³"  TO   TNS-NAME
               END-IF
               IF    (NACCT-SRYYM      >=   "201204")    AND
                     (NSRY-SRYCD(1)    =    "190708610"  OR
                                            "190708710"  OR
                                            "190708810")
                   MOVE   "111111119"     TO    NSRY-SRYCD(1)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                                          TO   TNS-NAME
               END-IF
               IF    (NACCT-SRYYM      >=   "201204")    AND
                     (NSRY-SRYCD(1)    =    "190708910"  OR
                                            "190709010"  OR
                                            "190709110")
                   MOVE   "222222229"     TO    NSRY-SRYCD(1)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                                          TO   TNS-NAME
               END-IF
               IF    (NACCT-SRYYM      >=   "201204")    AND
                     (NSRY-SRYCD(1)    =    "190709210"  OR
                                            "190709310"  OR
                                            "190709410")
                   MOVE   "333333339"     TO    NSRY-SRYCD(1)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                                          TO   TNS-NAME
               END-IF
               IF    (NACCT-SRYYM      >=   "201204")    AND
                     (NSRY-SRYCD(1)    =    "190153410"  OR
                                            "190153610"  OR
                                            "190153810")
                   MOVE   "111111199"     TO    NSRY-SRYCD(1)
                   MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±"
                                          TO   TNS-NAME
               END-IF
               IF    (NACCT-SRYYM      >=   "201204")    AND
                     (NSRY-SRYCD(1)    =    "190154010"  OR
                                            "190154210"  OR
                                            "190154410")
                   MOVE   "222222299"     TO    NSRY-SRYCD(1)
                   MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²"
                                          TO   TNS-NAME
               END-IF
               IF    (NACCT-SRYYM      >=   "201204")    AND
                     (NSRY-SRYCD(1)    =    "190154610"  OR
                                            "190154810"  OR
                                            "190155010")
                   MOVE   "333333399"     TO    NSRY-SRYCD(1)
                   MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³"
                                          TO   TNS-NAME
               END-IF
               IF    (NACCT-SRYYM      >=   "201204")    AND
                     (NSRY-SRYCD(1)    =    "190153510"  OR
                                            "190153710"  OR
                                            "190153910")
                   MOVE   "111111999"     TO    NSRY-SRYCD(1)
               MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                                          TO   TNS-NAME
               END-IF
               IF    (NACCT-SRYYM      >=   "201204")    AND
                     (NSRY-SRYCD(1)    =    "190154110"  OR
                                            "190154310"  OR
                                            "190154510")
                   MOVE   "222222999"     TO    NSRY-SRYCD(1)
               MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                                          TO   TNS-NAME
               END-IF
               IF    (NACCT-SRYYM      >=   "201204")    AND
                     (NSRY-SRYCD(1)    =    "190154710"  OR
                                            "190154910"  OR
                                            "190155110")
                   MOVE   "333333999"     TO    NSRY-SRYCD(1)
               MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                                          TO   TNS-NAME
               END-IF
      *
               IF   (NSRY-SRYCD(1)  =
                          LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX))  AND
                    (TNS-NAME   = 
                          LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX))
                    MOVE   1      TO    FLG-MATCH
                    IF   NACCT-SRYYM  =  LNK-ORCHCN02SUB-NYUINYMD(1:6)
                         MOVE   LNK-ORCHCN02SUB-NYUINYMD(7:2) 
                                         TO   WRK-DDX
                         MOVE   WRK-DD9  TO   ST-IDX
                    ELSE
                         MOVE    1       TO   ST-IDX
                    END-IF
                    IF   NACCT-SRYYM  =  LNK-ORCHCN02SUB-TAIINYMD(1:6)
                         MOVE   LNK-ORCHCN02SUB-TAIINYMD(7:2) 
                                         TO   WRK-DDX
                         MOVE   WRK-DD9  TO   ED-IDX
                    ELSE
                         MOVE   31       TO   ED-IDX
                    END-IF
                    PERFORM   VARYING   IDX1   FROM  ST-IDX  BY  1
                      UNTIL   IDX1      >   ED-IDX
                          IF   NACCT-DAY(IDX1)    NOT =  ZERO
                               ADD   1     TO
                                       LNK-ORCHCN02SUB-NISSU(NYUIN-IDX)
                          END-IF
                    END-PERFORM
               END-IF
           END-PERFORM
      *
      *
           IF   (FLG-MATCH     =     ZERO)   AND
                (NYUIN-IDX     <=       10)
               MOVE  NSRY-SRYCD(1)   TO
                                 LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
      *        ÅÀ¿ô¥Þ¥¹¥¿¤è¤êÆþ±¡ÎÁÌ¾¾Î¤Î¼èÆÀ
               INITIALIZE    TNS-TENSU-REC
               MOVE   LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                                            TO  TNS-SRYCD
               MOVE   NACCT-SRYYM     TO    TNS-YUKOSTYMD(1:6)
                                            TNS-YUKOEDYMD(1:6)
               MOVE   "01"            TO    TNS-YUKOSTYMD(7:2)
                                            TNS-YUKOEDYMD(7:2)
               PERFORM    910-TENSU-READ-SEC
               MOVE     TNS-NAME      TO
                             LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               IF    (NACCT-SRYYM      >=   "200604")    AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190097010"  OR
                                            "190097110"  OR
                                            "190119510"  OR
                                            "190119610"  OR
                                            "111111111")
                   MOVE   "111111111"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
               IF    (NACCT-SRYYM      >=   "200604")    AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190097210"  OR
                                            "190097310"  OR
                                            "190119710"  OR
                                            "190119810"  OR
                                            "222222222")
                   MOVE   "222222222"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
               IF    (NACCT-SRYYM      >=   "201004")    AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190135110"  OR
                                            "190135210"  OR
                                            "190135310"  OR
                                            "333333333")
                   MOVE   "333333333"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
      *
               IF    (NACCT-SRYYM      >=   "201204")    AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190708610"  OR
                                            "190708710"  OR
                                            "190708810"  OR
                                            "111111119")
                   MOVE   "111111119"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
      *
               IF    (NACCT-SRYYM      >=   "201204")    AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190708910"  OR
                                            "190709010"  OR
                                            "190709110"  OR
                                            "222222229")
                   MOVE   "222222229"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
      *
               IF    (NACCT-SRYYM      >=   "201204")    AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190709210"  OR
                                            "190709310"  OR
                                            "190709410"  OR
                                            "333333339")
                   MOVE   "333333339"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
      *
               IF    (NACCT-SRYYM      >=   "201204")    AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190153410"  OR
                                            "190153610"  OR
                                            "190153810"  OR
                                            "111111199")
                   MOVE   "111111199"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
      *
               IF    (NACCT-SRYYM      >=   "201204")    AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190154010"  OR
                                            "190154210"  OR
                                            "190154410"  OR
                                            "222222299")
                   MOVE   "222222299"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
      *
               IF    (NACCT-SRYYM      >=   "201204")    AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190154610"  OR
                                            "190154810"  OR
                                            "190155010"  OR
                                            "333333399")
                   MOVE   "333333399"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
                   MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³"  TO
                               LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
      *
               IF    (NACCT-SRYYM      >=   "201204")    AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190153510"  OR
                                            "190153710"  OR
                                            "190153910"  OR
                                            "111111999")
                   MOVE   "111111999"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
               MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                       TO      LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
      *
               IF    (NACCT-SRYYM      >=   "201204")    AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190154110"  OR
                                            "190154310"  OR
                                            "190154510"  OR
                                            "222222999")
                   MOVE   "222222999"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
               MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£²¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                       TO      LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
      *
               IF    (NACCT-SRYYM      >=   "201204")    AND
                     (LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)  =
                                            "190154710"  OR
                                            "190154910"  OR
                                            "190155110"  OR
                                            "333333999")
                   MOVE   "333333999"     TO
                               LNK-ORCHCN02SUB-SYUBETUCD(NYUIN-IDX)
               MOVE   "ÎÅÍÜÉÂ¾²¡¦Í­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£³¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë"
                       TO      LNK-ORCHCN02SUB-SYUBETUMEI(NYUIN-IDX)
               END-IF
      *
               IF   NACCT-SRYYM  =  LNK-ORCHCN02SUB-NYUINYMD(1:6)
                    MOVE   LNK-ORCHCN02SUB-NYUINYMD(7:2) 
                                    TO   WRK-DDX
                    MOVE   WRK-DD9  TO   ST-IDX
               ELSE
                    MOVE    1       TO   ST-IDX
               END-IF
               IF   NACCT-SRYYM  =  LNK-ORCHCN02SUB-TAIINYMD(1:6)
                    MOVE   LNK-ORCHCN02SUB-TAIINYMD(7:2) 
                                    TO   WRK-DDX
                    MOVE   WRK-DD9  TO   ED-IDX
               ELSE
                    MOVE   31       TO   ED-IDX
               END-IF
               PERFORM   VARYING   IDX1   FROM  ST-IDX  BY  1
                 UNTIL   IDX1      >   ED-IDX
                     IF   NACCT-DAY(IDX1)    NOT =  ZERO
                          ADD   1     TO
                                  LNK-ORCHCN02SUB-NISSU(NYUIN-IDX)
                     END-IF
               END-PERFORM
           END-IF
      *
           .
       210-NYUIN-COUNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¼èÆÀ
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
      *    Æþ±¡¹Ô°Ù¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinact"     TO  MCP-TABLE
           MOVE     "key9"             TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       210-NYUINACT-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
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
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-NYUINACCT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-NYUINACCT-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-NYUINACT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-NYUINACT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACT-REC
               MOVE    ZERO                TO  FLG-NYUINACT
           ELSE
               INITIALIZE                      NYUINACT-REC
               MOVE    1                   TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼ÔÆþ±¡ÍúÎò¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-PTNYUINRRK-SELECT-SEC        SECTION.
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
      *    ´µ¼ÔÆþ±¡ÍúÎò¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-PTNYUINRRK-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
      *    ÅÀ¿ô¥Þ¥¹¥¿¡¼ÆÉ¹þ¡Ê¼ç¥­¡¼¡Ë
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   MOVE    1                   TO  FLG-TENSU
                   INITIALIZE                  TNS-TENSU-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
