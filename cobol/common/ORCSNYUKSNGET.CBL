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
       PROGRAM-ID.                 ORCSNYUKSNGET.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : Æþ±¡
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : Æþ±¡ÎÁ²Ã»»¼èÆÀ¥µ¥Ö
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  13/10/31    NACL¡Ý¾®Æ¦Âô  ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
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
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-NYUKSNCHK               PIC 9(01).
           03  FLG-NYUKSN                  PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                         PIC 9(05).
           03  IDX7                        PIC 9(05).
           03  IDX8                        PIC 9(05).
           03  IDX9                        PIC 9(05).
           03  IDX-KSN                     PIC 9(05).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
      *
      *    °åÎÅµ¡´ØÀßÄê¤ÎÆþ±¡ÎÁ²Ã»»¥Á¥§¥Ã¥¯ÍÑ
           03  WRK-NYUKHNKSN-SRYCD         PIC X(09)  OCCURS 20.
           03  WRK-KHN-NYUKSNKBN           PIC X(03).
           03  WRK-NYUKSNKBN               PIC X(03).
           03  WRK-BTU-NYUKSNKBN           PIC X(03).
           03  WRK-BTU-CDKBN-KBNNUM        PIC 9(03).
           03  WRK-BTU-CDKBN-KBNNUM-EDA    PIC 9(02).
           03  WRK-KSN-NYUKSNKBN           PIC X(03).
           03  WRK-KSN-CDKBN-KBNNUM        PIC 9(03).
           03  WRK-KSN-CDKBN-KBNNUM-EDA    PIC 9(02).
      *
           03  WRK-TOKUBETSU-NYUIN         PIC X(01).
      *
      *    ¥×¥ì¥¤¥ë¡¼¥à¡¢ÊÝ°é»Î²Ã»»²Ã»»ÂàÈòÎÎ°è
           03  WRK-BTU-HIKSISUKBN          PIC X(01).
           03  WRK-BTU-KNGTEK-KBN          PIC X(01).
           03  WRK-BTU-DRTEK-KBN           PIC X(01).
      *
      *    ´µ¼ÔÇ¯Îð»»½ÐÍÑ¥¨¥ê¥¢
           03  WRK-NENREI-AREA.
              05  WRK-NENREI               PIC 9(03).
              05  WRK-YMDS1.
                  07  WRK-YYS1             PIC 9(04).
                  07  WRK-MMS1             PIC 9(02).
                  07  WRK-DDS1             PIC 9(02).
              05  WRK-YMDS2.
                  07  WRK-YYS2             PIC 9(04).
                  07  WRK-MMS2             PIC 9(02).
                  07  WRK-DDS2             PIC 9(02).
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY    "CPSYSKANRI.INC".
      *    ¡Ê°åÎÅµ¡´Ø¾ðÊó¡Ë
           COPY    "CPSK5000.INC".
      *    ¡ÊÉÂÅï¾ðÊó¡Ë
           COPY    "CPSK5001.INC".
      *    ¡ÊÉÂ¼¼¾ðÊó¡Ë
           COPY    "CPSK5002.INC".
      *
      *    ´µ¼Ô¥Þ¥¹¥¿
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿¡Ý
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    Æþ±¡ÎÁ²Ã»»¥Á¥§¥Ã¥¯
       01  NYUKSNCHK-REC.
           COPY    "CPNYUKSNCHK.INC".
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
      *    ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    Æþ±¡ÎÁ²Ã»»¼èÆÀ¥Ñ¥é¥á¥¿
           COPY    "CPORCSNYUKSNGET.INC".
      *
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSNYUKSNGET-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      LNK-NYUKSNGET-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-NYUKSNGET-RC
      *
           PERFORM    100-NYUKSN-GET-SEC
           .
      *
       000-PROC-EXT.
           EXIT   PROGRAM
           .
      *
      *****************************************************************
      *    Æþ±¡ÎÁ²Ã»»¤Î¼è¤ê¹þ¤ß½èÍý
      *****************************************************************
       100-NYUKSN-GET-SEC            SECTION.
      *
           MOVE   ZERO             TO   IDX8
      *
      *°åÎÅµ¡´Ø²Ã»»¾ðÊó
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5000"          TO  SYS-KANRICD
           MOVE    LNK-NYUKSNGET-SANTEIYM
                                   TO  SYS-STYUKYMD(1:6)
           MOVE   "01"             TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF   MCP-RC             =    ZERO
               MOVE    MCPDATA-REC TO   SYS-5000-REC
      *        Æþ±¡ÎÁ²Ã»»
               PERFORM   VARYING   IDX7   FROM   1  BY  1
                 UNTIL  (SYS-5000-KSNCD(IDX7)    =   SPACE)  OR
                        (IDX7                    >   20)
                    IF   SYS-5000-KSNKBN(IDX7)   =   "2"
                             ADD    1                TO  IDX8
                             MOVE   SYS-5000-KSNCD(IDX7)  TO
                                             WRK-NYUKHNKSN-SRYCD(IDX8)
                    END-IF
               END-PERFORM
      *        ÃÏ°è²Ã»»
               IF   SYS-5000-CHIIKIKSN    NOT =   SPACE
                    ADD    1        TO   IDX8
                    MOVE   SYS-5000-CHIIKIKSN   TO
                                             WRK-NYUKHNKSN-SRYCD(IDX8)
               END-IF
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *ÉÂÅï²Ã»»¾ðÊó
           PERFORM     4801-BTUSEL-SEC
      *
           IF   FLG-SYSKANRI           =    ZERO
      *
      *        ´Ç¸îÊä½õ²Ã»»
               IF    SYS-5001-BTU-KNGHJOKSN   NOT =   SPACE
                   ADD      1          TO     IDX8
                   EVALUATE     SYS-5001-BTU-KNGHJOKSN
                       WHEN     "01"
                           MOVE  "190103070"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "02"
                           MOVE  "190103470"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "03"
                           MOVE  "190103770"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "04"
                           MOVE  "190103970"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "05"
                           MOVE  "190104070"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
      *                Ê¿À®£±£¸Ç¯£´·î²þÀµÂÐ±þ
                       WHEN     "06"
      *                    ´Ç¸îÊä½õ²Ã»»£±
                           MOVE  "190103770"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "07"
      *                    ´Ç¸îÊä½õ²Ã»»£±¡Ê£±£³ÂÐ£±Æþ±¡´ðËÜÎÁ¡Ë
                           MOVE  "190103870"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "08"
      *                    ´Ç¸îÊä½õ²Ã»»£²
                           MOVE  "190103970"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "09"
      *                    ´Ç¸îÊä½õ²Ã»»£³
                           MOVE  "190104070"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                   END-EVALUATE
               END-IF
      *
      *        Ìë´Ö¶ÐÌ³Åù´Ç¸î²Ã»»
               IF    SYS-5001-BTU-YAKANKNGKSN   NOT =   SPACE
                   ADD      1          TO     IDX8
                   EVALUATE     SYS-5001-BTU-YAKANKNGKSN
                       WHEN     "01"
                           MOVE  "190109870"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "02"
                           MOVE  "190076070"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "03"
                           MOVE  "190076170"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "04"
                           MOVE  "190076370"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                       WHEN     "05"
                           MOVE  "190076470"   TO
                                              WRK-NYUKHNKSN-SRYCD(IDX8)
                   END-EVALUATE
               END-IF
      *
      *        ¾®»ùÆþ±¡°åÎÅ´ÉÍýÎÁ²Ã»»¡ÊÊÝ°é»Î£±¿Í°Ê¾å¡Ë
               MOVE  SPACE       TO   WRK-BTU-HIKSISUKBN
               IF    LNK-NYUKSNGET-NYUIN-SRYCD =   "190129310"  OR
                                                   "190111110"  OR
                                                   "190111210"  OR
                                                   "190075810"  OR
                                                   "190140710"
                   IF    SYS-5001-BTU-HIKSISUKBN   =   "2"
                       MOVE   "2"    TO   WRK-BTU-HIKSISUKBN
                   END-IF
               END-IF
      *
      *      ÉÂÅïÆþ±¡ÎÁ²Ã»»
      *
      *      Ê¿À®£²£°Ç¯£³·î¤Þ¤Ç¤Î²Ã»»¥»¥Ã¥È½èÍý
             IF     LNK-NYUKSNGET-SANTEIYM     <=     "200803"
               PERFORM   VARYING   IDX7   FROM   1  BY  1
                 UNTIL  (IDX7                    >   14)
                   EVALUATE   IDX7
                       WHEN   3
                       WHEN   4
                       WHEN   5
                           CONTINUE
      *                »ùÆ¸»×½Õ´üÀº¿À²ÊÆþ±¡°åÎÅ´ÉÍý²Ã»»¡Ê£²£°ºÍÌ¤Ëþ¡Ë
                       WHEN   6
                           IF      SYS-5001-KSNKBN(IDX7)   =   "2"
                               PERFORM   4801-PT-AGE-SEC
                               IF      WRK-NENREI          <      20
                                   ADD    1                TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                               WRK-NYUKHNKSN-SRYCD(IDX8)
                               END-IF
                           END-IF
      *                ´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»
                       WHEN   13
                           MOVE  SPACE       TO   WRK-BTU-KNGTEK-KBN
                           IF    SYS-5001-KNGTEK-KBN   =   "2"
                               MOVE   "2"    TO   WRK-BTU-KNGTEK-KBN
                           END-IF
      *                Ê£¿ô°å»ÕµÚ¤Ó´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»
                       WHEN   14
                           MOVE  SPACE       TO   WRK-BTU-DRTEK-KBN
                           IF    SYS-5001-DRTEK-KBN   =   "2"
                               MOVE   "2"    TO   WRK-BTU-DRTEK-KBN
                           END-IF
                       WHEN   OTHER
                           IF     SYS-5001-KSNKBN(IDX7)    =   "2"
                               ADD    1                TO  IDX8
                               MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                           END-IF
                   END-EVALUATE
               END-PERFORM
             END-IF
      *
      *      Ê¿À®£²£°Ç¯£´·î¤«¤éÊ¿À®£²£²Ç¯£³·î¤Þ¤Ç¤Î²Ã»»¥»¥Ã¥È½èÍý
             IF    (LNK-NYUKSNGET-SANTEIYM  >=   "200804")    AND
                   (LNK-NYUKSNGET-SANTEIYM  <=   "201003")
                 PERFORM   VARYING   IDX7   FROM   1  BY  1
                   UNTIL  (IDX7      >   14)
                     IF   SYS-5001-KSNKBN(IDX7)   =   "2"
                         EVALUATE    SYS-5001-KSNCD(IDX7)
      *                      »ùÆ¸»×½Õ´üÀº¿À²ÊÆþ±¡°åÎÅ´ÉÍý²Ã»»¡Ê£²£°ºÍÌ¤Ëþ¡Ë
                             WHEN    "190110170"
                                PERFORM   4801-PT-AGE-SEC
                                IF      WRK-NENREI   <      20
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      ½Å¾É´µ¼Ô²óÉüÉÂÅï²Ã»»¡Ê²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÆþ±¡ÎÁ£±¡Ë
                             WHEN    "190129870"
                                IF  LNK-NYUKSNGET-NYUIN-SRYCD
                                                     = "190129410"  OR
                                                       "190129510"  
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      °å»ÕÇÛÃÖÅ¬¹ç²Ã»»¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¡Ë
      *                      ´Ç¸îÇÛÃÖ²Ã»»£±¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¡Ë
      *                      ´Ç¸îÇÛÃÖ²Ã»»£²¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¡Ë
      *                      Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¡Ë
      *                      Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¡Ë
                             WHEN    "190117070"
                             WHEN    "190126570"
                             WHEN    "190109670"
                             WHEN    "190126670"
                             WHEN    "190126770"
                                IF  LNK-NYUKSNGET-NYUIN-SRYCD
                                                         = "190097010"
                                                        OR "190097110"
                                                        OR "190119510"
                                                        OR "190119610"
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¡Ë
                             WHEN    "190126470"
                                IF  LNK-NYUKSNGET-NYUIN-SRYCD
                                                         = "190097010"
                                                        OR "190097110"
                                                        OR "190119510"
                                                        OR "190119610"
                                                        OR "190097210"
                                                        OR "190097310"
                                                        OR "190119710"
                                                        OR "190119810"
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
                             WHEN   OTHER
                                ADD    1                TO  IDX8
                                MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                         END-EVALUATE
                     END-IF
                 END-PERFORM
             END-IF
      *
      *      Ê¿À®£²£²Ç¯£´·î¤«¤éÊ¿À®£²£´Ç¯£³·î¤Þ¤Ç¤Î²Ã»»¥»¥Ã¥È½èÍý
             IF    (LNK-NYUKSNGET-SANTEIYM    >=     "201004")    AND
                   (LNK-NYUKSNGET-SANTEIYM    <=     "201203")
      *         ¡Ê¥·¥¹¥Æ¥à´ÉÍý¤ÎÉÂÅïÀßÄê¸«Ä¾¤·¤Ë¤è¤ë¤â¤Î¡Ë
               PERFORM   VARYING   IDX7   FROM   1  BY  1
                 UNTIL  (IDX7      >   14)
                     IF   SYS-5001-KSNKBN(IDX7)   =   "2"
                         EVALUATE    SYS-5001-KSNCD(IDX7)
      *                      »ùÆ¸»×½Õ´üÀº¿À²ÊÆþ±¡°åÎÅ´ÉÍý²Ã»»¡Ê£²£°ºÍÌ¤Ëþ¡Ë
                             WHEN    "190110170"
                                PERFORM   4801-PT-AGE-SEC
                                IF      WRK-NENREI   <      20
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      ½Å¾É´µ¼Ô²óÉüÉÂÅï²Ã»»¡Ê²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÆþ±¡ÎÁ£±¡Ë
                             WHEN    "190129870"
                                IF  LNK-NYUKSNGET-NYUIN-SRYCD
                                                       = "190129410" OR
                                                         "190129510" OR
                                                         "190141610" OR
                                                         "190141710"
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
      *                            ´ð½àÌ¤Å¬¹ç¤Î¾ì¹ç¤Ï½Å¾É´µ¼Ô²óÉüÉÂÅï²Ã»»¡Ê´ð½àÌ¤Å¬¹ç¡Ë
      *                            ¤ËÃÖ¤­´¹¤¨¤ë
                                   IF  LNK-NYUKSNGET-NYUIN-SRYCD
                                                       = "190129410" OR
                                                         "190129510"
                                     MOVE   "190142370"  TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                   END-IF
                                END-IF
      *                      °å»ÕÇÛÃÖ²Ã»»£±¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¡Ë
      *                      °å»ÕÇÛÃÖ²Ã»»£²¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¡Ë
      *                      ´Ç¸îÇÛÃÖ²Ã»»£±¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¡Ë
      *                      ´Ç¸îÇÛÃÖ²Ã»»£²¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¡Ë
      *                      Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¡Ë
      *                      Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¡Ë
                             WHEN    "190135570"
                             WHEN    "190117070"
                             WHEN    "190109670"
                             WHEN    "190126570"
                             WHEN    "190126770"
                             WHEN    "190126670"
                                IF  LNK-NYUKSNGET-NYUIN-SRYCD
                                                         = "190097010"
                                                        OR "190119510"
                                                        OR "190119610"
                                                        OR "190097210"
                                                        OR "190119710"
                                                        OR "190119810"
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¤È£³¡Ë
                             WHEN    "190126470"
                                IF  LNK-NYUKSNGET-NYUIN-SRYCD
                                                         = "190097010"
                                                        OR "190119510"
                                                        OR "190119610"
                                                        OR "190097210"
                                                        OR "190119710"
                                                        OR "190119810"
                                                        OR "190135110"
                                                        OR "190135210"
                                                        OR "190135310"
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»¡Ê£±£°ÂÐ£±Æþ±¡´ðËÜÎÁ¡Ë
                             WHEN    "190131570"
      *                            °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»
                                   IF  LNK-NYUKSNGET-NYUIN-SRYCD
                                                         = "190077410"
                                     ADD    1            TO  IDX8
                                     MOVE   "190131570"  TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                   END-IF
      *                            °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»¡ÊÆÃÄêµ¡Ç½ÉÂ±¡¡Ë
                                   IF  LNK-NYUKSNGET-NYUIN-SRYCD
                                                         = "190085910"
                                     ADD    1            TO  IDX8
                                     MOVE   "190134870"  TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                   END-IF
      *                            °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»¡ÊÀìÌçÉÂ±¡¡Ë
                                   IF  LNK-NYUKSNGET-NYUIN-SRYCD
                                                         = "190090210"
                                     ADD    1            TO  IDX8
                                     MOVE   "190134970"  TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                   END-IF
      *                      µÙÆü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»¡Ê²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ¡Ë
                             WHEN    "190140870"
                                IF  LNK-NYUKSNGET-NYUIN-SRYCD
                                                       = "190141610" OR
                                                         "190141710" OR
                                                         "190141810" OR
                                                         "190141910"
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
                             WHEN   OTHER
                                ADD    1                TO  IDX8
                                MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                         END-EVALUATE
                     END-IF
               END-PERFORM
             END-IF
      *
      *      Ê¿À®£²£´Ç¯£´·î¤«¤é¤Î²Ã»»¥»¥Ã¥È½èÍý
             IF     LNK-NYUKSNGET-SANTEIYM   >=     "201204"
      *         ¡Ê¥·¥¹¥Æ¥à´ÉÍý¤ÎÉÂÅïÀßÄê¸«Ä¾¤·¤Ë¤è¤ë¤â¤Î¡Ë
               PERFORM   VARYING   IDX7   FROM   1  BY  1
                 UNTIL  (IDX7      >   14)
                     IF   SYS-5001-KSNKBN(IDX7)   =   "2"
                         EVALUATE    SYS-5001-KSNCD(IDX7)
      *                      °å»ÕÇÛÃÖ²Ã»»£±¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¡Ë
      *                      °å»ÕÇÛÃÖ²Ã»»£²¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¡Ë
      *                      ´Ç¸îÇÛÃÖ²Ã»»£±¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¡Ë
      *                      ´Ç¸îÇÛÃÖ²Ã»»£²¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¡Ë
      *                      Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¡Ë
      *                      Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¡Ë
                             WHEN    "190135570"
                             WHEN    "190117070"
                             WHEN    "190109670"
                             WHEN    "190126570"
                             WHEN    "190126770"
                             WHEN    "190126670"
                                IF  LNK-NYUKSNGET-NYUIN-SRYCD
                                                         = "190097010"
                                                        OR "190119510"
                                                        OR "190119610"
                                                        OR "190097210"
                                                        OR "190119710"
                                                        OR "190119810"
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»¡ÊÍ­¾²¿ÇÎÅ½êÆþ±¡´ðËÜÎÁ£±¤È£²¤È£³¡Ë
                             WHEN    "190126470"
                                IF  LNK-NYUKSNGET-NYUIN-SRYCD
                                                         = "190097010"
                                                        OR "190119510"
                                                        OR "190119610"
                                                        OR "190097210"
                                                        OR "190119710"
                                                        OR "190119810"
                                                        OR "190135110"
                                                        OR "190135210"
                                                        OR "190135310"
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»¡Ê£±£³ÂÐ£±Æþ±¡´ðËÜÎÁ¡Ë
                             WHEN    "190131570"
      *                            °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»
                                   IF  LNK-NYUKSNGET-NYUIN-SRYCD
                                                         = "190077510"
                                     ADD    1            TO  IDX8
                                     MOVE   "190131570"  TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                   END-IF
      *                            °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»¡ÊÀìÌçÉÂ±¡¡Ë
                                   IF  LNK-NYUKSNGET-NYUIN-SRYCD
                                                         = "190090310"
                                     ADD    1            TO  IDX8
                                     MOVE   "190134970"  TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                   END-IF
      *                            °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»¡ÊÆÃÄê°ìÈÌÉÂÅïÆþ±¡ÎÁ£±¡Ë
                                   IF  LNK-NYUKSNGET-NYUIN-SRYCD
                                                         = "190151910"
                                     ADD    1            TO  IDX8
                                     MOVE   "190152570"  TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                   END-IF
      *                      µÙÆü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÄó¶¡ÂÎÀ©²Ã»»¡Ê²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ¡Ë
                             WHEN    "190140870"
                                IF  LNK-NYUKSNGET-NYUIN-SRYCD
                                                       = "190141610" OR
                                                         "190141710" OR
                                                         "190141810" OR
                                                         "190141910" OR
                                                         "190150110" OR
                                                         "190150210"
                                   ADD    1            TO  IDX8
                                   MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      ´Ç¸îÉ¬Í×ÅÙ²Ã»»£±¡Ê£±£°ÂÐ£±Æþ±¡´ðËÜÎÁ¡Ë
                             WHEN    "190143070"
      *                         ´Ç¸îÉ¬Í×ÅÙ²Ã»»£±¡Ê°ìÈÌÉÂÅïÆþ±¡´ðËÜÎÁ¡Ë
                                IF  LNK-NYUKSNGET-NYUIN-SRYCD
                                                       =  "190077410"
                                   ADD    1            TO  IDX8
                                   MOVE   "190143070"  TO
                                             WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                         ´Ç¸îÉ¬Í×ÅÙ²Ã»»£±¡ÊÆÃÄêµ¡Ç½ÉÂ±¡¡Ë
                                IF  LNK-NYUKSNGET-NYUIN-SRYCD
                                                       =  "190085910"
                                   ADD    1            TO  IDX8
                                   MOVE   "190143970"  TO
                                             WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                         ´Ç¸îÉ¬Í×ÅÙ²Ã»»£±¡ÊÀìÌçÉÂ±¡¡Ë
                                IF  LNK-NYUKSNGET-NYUIN-SRYCD
                                                       =  "190090210"
                                   ADD    1            TO  IDX8
                                   MOVE   "190144570"  TO
                                             WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                      ´Ç¸îÉ¬Í×ÅÙ²Ã»»£²¡Ê£±£°ÂÐ£±Æþ±¡´ðËÜÎÁ¡Ë
                             WHEN    "190143170"
      *                         ´Ç¸îÉ¬Í×ÅÙ²Ã»»£²¡Ê°ìÈÌÉÂÅïÆþ±¡´ðËÜÎÁ¡Ë
                                IF  LNK-NYUKSNGET-NYUIN-SRYCD
                                                       =  "190077410"
                                   ADD    1            TO  IDX8
                                   MOVE   "190143170"  TO
                                             WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                         ´Ç¸îÉ¬Í×ÅÙ²Ã»»£²¡ÊÆÃÄêµ¡Ç½ÉÂ±¡¡Ë
                                IF  LNK-NYUKSNGET-NYUIN-SRYCD
                                                       =  "190085910"
                                   ADD    1            TO  IDX8
                                   MOVE   "190144070"  TO
                                             WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
      *                         ´Ç¸îÉ¬Í×ÅÙ²Ã»»£²¡ÊÀìÌçÉÂ±¡¡Ë
                                IF  LNK-NYUKSNGET-NYUIN-SRYCD
                                                       =   "190090210"
                                   ADD    1            TO  IDX8
                                   MOVE   "190144670"  TO
                                             WRK-NYUKHNKSN-SRYCD(IDX8)
                                END-IF
                             WHEN   OTHER
                                ADD    1                TO  IDX8
                                MOVE   SYS-5001-KSNCD(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                         END-EVALUATE
                     END-IF
               END-PERFORM
             END-IF
      *
      *      Æþ±¡ÎÁ²Ã»»¡Ê¥æ¡¼¥¶¡¼ÀßÄê¡Ë
             PERFORM   VARYING   IDX7   FROM   1  BY  1
               UNTIL  (IDX7      >   10)
                   IF   SYS-5001-KSNCD2(IDX7)  NOT =   SPACE
                       ADD   1     TO    IDX8
                       MOVE   SYS-5001-KSNCD2(IDX7) TO
                                            WRK-NYUKHNKSN-SRYCD(IDX8)
                   END-IF
             END-PERFORM
      *
           END-IF
      *
      *    ÉÂ¼¼Æþ±¡ÎÁ²Ã»»
           PERFORM     4801-BRMSEL-SEC
      *
           IF   FLG-SYSKANRI       =    ZERO
      *
               PERFORM   VARYING   IDX7   FROM   1  BY  1
                 UNTIL    (IDX7                    >   12)
                     IF   SYS-5002-KSNKBN(IDX7)   =   "2"
                          ADD    1                TO  IDX8
                          MOVE   SYS-5002-KSNCD(IDX7)  TO
                                             WRK-NYUKHNKSN-SRYCD(IDX8)
                     END-IF
               END-PERFORM
      *
           END-IF
      *
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN >   3 )
               IF    ( LNK-NYUKSNGET-I01-KSN-SRYCD (IDX-KSN)
                                       NOT =   SPACE )
                    ADD     1       TO     IDX8
                    MOVE    LNK-NYUKSNGET-I01-KSN-SRYCD (IDX-KSN)
                                    TO     WRK-NYUKHNKSN-SRYCD(IDX8)
               END-IF
           END-PERFORM
      *
      *    Æþ±¡ÎÁ¤ÈÆþ±¡ÎÁ²Ã»»¤ÎÊ»»»Äê¥Á¥§¥Ã¥¯
           MOVE    ZERO            TO     IDX9
           PERFORM    VARYING    IDX8   FROM   1   BY   1
                      UNTIL      (WRK-NYUKHNKSN-SRYCD(IDX8)  =  SPACE)
                         OR      (IDX8   >     20)
                         PERFORM   4801-NYUKSN-CHK-SEC
                         IF     FLG-NYUKSN       =   ZERO   
                             ADD    1                TO  IDX9
                             MOVE   WRK-NYUKHNKSN-SRYCD(IDX8)  TO
                                           LNK-NYUKSNGET-KSNSRYCD(IDX9)
      *                      êóáì´µ¼Ô´ÉÍý²Ã»»¤Î¥Õ¥é¥°¥»¥Ã¥È
                             IF   WRK-NYUKHNKSN-SRYCD(IDX8)  =
                                                         "190116990"
                                  MOVE   "1"    TO
                                       LNK-NYUKSNGET-NYUKSNKBN(IDX9)
                             END-IF
      *                      µßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»¡ÊÉÂ±¡¡Ë¤Î¥Õ¥é¥°¥»¥Ã¥È
                             IF   WRK-NYUKHNKSN-SRYCD(IDX8)  =
                                             "190134270"
                                  IF    LNK-NYUKSNGET-FLG-IPPAN2  =  1
                                     MOVE   "2"    TO
                                         LNK-NYUKSNGET-NYUKSNKBN(IDX9)
                                  END-IF
                                  IF    LNK-NYUKSNGET-FLG-IPPAN2  =  2
                                     MOVE   "3"    TO
                                         LNK-NYUKSNGET-NYUKSNKBN(IDX9)
                                  END-IF
                             END-IF
      *                      µßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»¡Ê¿ÇÎÅ½ê¡Ë¤Î¥Õ¥é¥°¥»¥Ã¥È
                             IF   WRK-NYUKHNKSN-SRYCD(IDX8)  =
                                             "190135670"
                                  IF    LNK-NYUKSNGET-FLG-IPPAN2  =  3
                                     MOVE   "4"    TO
                                         LNK-NYUKSNGET-NYUKSNKBN(IDX9)
                                  END-IF
                             END-IF
                         END-IF
           END-PERFORM
      *
      *    ¾®»ùÆþ±¡°åÎÅ´ÉÍýÎÁ²Ã»»¡ÊÊÝ°é»Î£±¿Í°Ê¾å¡Ë
           IF    (WRK-BTU-HIKSISUKBN   =   "2")  AND
                 (IDX9    <   20 )
               ADD    1            TO  IDX9
               MOVE  "190111370"   TO
                                   LNK-NYUKSNGET-KSNSRYCD(IDX9)
           END-IF
      *
           IF     LNK-NYUKSNGET-SANTEIYM  <=   "200803"
      *      ´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»
             IF    (WRK-BTU-KNGTEK-KBN   =   "2")  AND
                   (IDX9    <   20 )
                 ADD    1            TO  IDX9
                 MOVE  "190109670"   TO
                                   LNK-NYUKSNGET-KSNSRYCD(IDX9)
             END-IF          
      *
      *      Ê£¿ô°å»ÕµÚ¤Ó´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»
             IF    (WRK-BTU-DRTEK-KBN   =   "2")  AND
                   (IDX9    <   20 )
                 ADD    1            TO  IDX9
                 MOVE  "190117070"   TO
                                   LNK-NYUKSNGET-KSNSRYCD(IDX9)
             END-IF
           END-IF 
      *
      *    ÊÖµÑÎÎ°è¤Ø¤Î¥»¥Ã¥È
           MOVE     WRK-TOKUBETSU-NYUIN
                                     TO   LNK-NYUKSNGET-TOKUBETSU-NYUIN
      *
      *    Éüµ¢¥³¡¼¥É
           IF    WRK-NYUKHNKSN-SRYCD(1)   =   SPACE
                MOVE    1            TO   LNK-NYUKSNGET-RC
           ELSE
                MOVE    ZERO         TO   LNK-NYUKSNGET-RC
           END-IF
      *
      *    ÊÖµÑ¾ðÊó¤ÎÉ½¼¨¡Ê¥Æ¥¹¥ÈÍÑ¡Ë
      **     DISPLAY    "===========   ORCSNYUKSNGET HENKYAKU ====="
      **     DISPLAY    "WRK-NENREI = " WRK-NENREI
      **     DISPLAY    "LNK-NYUKSNGET-TOKUBETSU-NYUIN = "
      **                 LNK-NYUKSNGET-TOKUBETSU-NYUIN
      **     PERFORM     VARYING    IDX    FROM   1   BY   1
      **       UNTIL    (IDX    >   20)   OR
      **                (LNK-NYUKSNGET-KSNSRYCD(IDX)  =  SPACE)
      **         DISPLAY "IDX = " IDX
      **         MOVE    LNK-NYUKSNGET-HOSPNUM
      **                                 TO  TNS-HOSPNUM
      **         MOVE    WRK-NYUKHNKSN-SRYCD(IDX)
      **                                 TO  TNS-SRYCD
      **         MOVE    LNK-NYUKSNGET-SANTEIYM
      **                                 TO  TNS-YUKOSTYMD(1:6)
      **         MOVE    "01"            TO  TNS-YUKOSTYMD(7:2)
      **         MOVE    TNS-YUKOSTYMD   TO  TNS-YUKOEDYMD
      **         MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
      **         MOVE    "tbl_tensu"     TO  MCP-TABLE
      **         MOVE    "key"           TO  MCP-PATHNAME
      **         PERFORM 910-DBSELECT-SEC
      **         MOVE    "tbl_tensu"     TO  MCP-TABLE
      **         MOVE    "key"           TO  MCP-PATHNAME
      **         PERFORM 910-DBFETCH-SEC
      **         IF    ( MCP-RC          =   ZERO )
      **            MOVE    MCPDATA-REC TO  TNS-TENSU-REC
      **            MOVE    ZERO        TO  FLG-TENSU
      **         ELSE
      **            MOVE    1           TO  FLG-TENSU
      **         END-IF
      ***
      **         MOVE    "tbl_tensu"     TO  MCP-TABLE
      **         MOVE    "key"           TO  MCP-PATHNAME
      **         PERFORM 990-DBCLOSE-SEC
      **         DISPLAY "KSN-SRYCD = " WRK-NYUKHNKSN-SRYCD(IDX) 
      **                 " NAME = " TNS-NAME
      **     END-PERFORM
      *
      **     DISPLAY  "LNK-NYUKSNGET-RC = " LNK-NYUKSNGET-RC
           .
      *
       100-NYUKSN-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡ÎÁ²Ã»»¤Î»»Äê²Ä¡¦ÉÔ²Ä¥Á¥§¥Ã¥¯
      *****************************************************************
       4801-NYUKSN-CHK-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-NYUKSN
      *
      **     EVALUATE   WRK-NYUKHNKSN-SRYCD(IDX8)
      *        ¿ÇÎÅÏ¿´ÉÍýÂÎÀ©²Ã»»
      **         WHEN   "190100370"
      **             IF   SPA-GMN-I01-SYORIKBN-CD   =  "07" OR "08"
      **                  MOVE   1        TO     FLG-NYUKSN
      **             END-IF
      *            ¼«±¡¤Ç¤Î·ÑÂ³Æþ±¡¥Á¥§¥Ã¥¯
      **     END-EVALUATE
      *
      *    Æþ±¡ÎÁ²Ã»»¥Á¥§¥Ã¥¯¥Æ¡¼¥Ö¥ë¤Ë¤è¤ëÈ½Äê
           INITIALIZE  NYUKSNCHK-REC
      *
           PERFORM   48011-TENSU-SEL-SEC
           MOVE    WRK-BTU-NYUKSNKBN
                                   TO  NYUKSNCHK-NYUINKBN
           PERFORM   48012-TENSU-SEL-SEC
           IF      TNS-NYUKHNKSNKBN    NOT =   ZERO
               MOVE    WRK-KSN-NYUKSNKBN
                                       TO  NYUKSNCHK-KSNKBN
               MOVE    LNK-NYUKSNGET-SANTEIYM
                                       TO  NYUKSNCHK-YUKOSTYMD(1:6)
               MOVE    "01"            TO  NYUKSNCHK-YUKOSTYMD(7:2)
               MOVE    NYUKSNCHK-YUKOSTYMD
                                       TO  NYUKSNCHK-YUKOEDYMD
      *
               MOVE    "tbl_nyuksnchk" TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               MOVE    NYUKSNCHK-REC   TO  MCPDATA-REC
               PERFORM 910-DBSELECT-SEC
               MOVE    "tbl_nyuksnchk" TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               MOVE    NYUKSNCHK-REC   TO  MCPDATA-REC
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  NYUKSNCHK-REC
                   IF    ( NYUKSNCHK-CHKKBN    =   "1" OR "2" )
                       CONTINUE
                   ELSE
                       MOVE   1        TO     FLG-NYUKSN
                   END-IF
               ELSE
                   MOVE   1        TO     FLG-NYUKSN
               END-IF
      *
      *
               MOVE    "tbl_nyuksnchk" TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
           END-IF
      *
      *    Ãí²Ã»»¤ÎÉ½ÍÑÈÖ¹æ¤¬Æ±°ì¤Ç¤¢¤ë¤³¤È¤ÎÈ½Äê
           IF   WRK-KSN-CDKBN-KBNNUM    NOT =    ZERO
              IF  (WRK-BTU-CDKBN-KBNNUM  = WRK-KSN-CDKBN-KBNNUM)  AND
                  (WRK-BTU-CDKBN-KBNNUM-EDA = WRK-KSN-CDKBN-KBNNUM-EDA)
                  CONTINUE
              ELSE
                  MOVE    1     TO    FLG-NYUKSN
              END-IF
           END-IF
      *
      *    Ê¿À®£²£´Ç¯²þÀµ¤Ç¿·Àß¤µ¤ì¤¿Æþ±¡ÎÁ²Ã»»¤Î¥Á¥§¥Ã¥¯ÄÉ²Ã
      *    °Ê²¼¤ÎÆþ±¡ÎÁ²Ã»»¤Ë¤Ä¤¤¤Æ¤ÏÅÀ¿ô¥Þ¥¹¥¿¤ËÆþ±¡´ðËÜÎÁ²Ã»»¶èÊ¬¤¬¥»¥Ã¥È
      *    ¤µ¤ì¤Æ¤¤¤Ê¤¤°Ù¡¢Æþ±¡ÎÁ²Ã»»¥Á¥§¥Ã¥¯¥Æ¡¼¥Ö¥ë¤Ë¤è¤ëÈ½Äê¤¬¤Ç¤­¤Ê¤¤
      *    ¤³¤È¤«¤é¡¢ÆÃÄêÆþ±¡ÎÁ»»Äê»þ¤Ï»»ÄêÉÔ²Ä¤È¤¹¤ë»ÃÄêÂÐ½è¤òÄÉ²Ã¤¹¤ë
      *    ¤³¤È¤È¤¹¤ë¡£
      *    ¡¦Ìë´Ö£µ£°ÂÐ£±µÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»
      *    ¡¦Ìë´Ö£±£°£°ÂÐ£±µÞÀ­´ü´Ç¸îÊä½õÂÎÀ©²Ã»»
      *    ¡¦´Ç¸î¿¦°÷Ìë´ÖÇÛÃÖ²Ã»»
           IF    WRK-NYUKHNKSN-SRYCD(IDX8)   =    "190145970"  OR
                                                  "190146070"  OR
                                                  "190146170"
               IF   ((WRK-BTU-CDKBN-KBNNUM   >=   300)     AND
                     (WRK-BTU-CDKBN-KBNNUM   <=   399))    OR
                    ( WRK-BTU-CDKBN-KBNNUM   =    400 )
                  MOVE    1     TO    FLG-NYUKSN
               END-IF
           END-IF
      *
           .
      *
       4801-NYUKSN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¸¡º÷½èÍý¡ÊÆþ±¡ÎÁ¡Ë
      *****************************************************************
       48011-TENSU-SEL-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-NYUKSNKBN
           MOVE    ZERO            TO  WRK-BTU-CDKBN-KBNNUM
           MOVE    ZERO            TO  WRK-BTU-CDKBN-KBNNUM-EDA
           MOVE    SPACE           TO  WRK-TOKUBETSU-NYUIN
      *
           MOVE    ZERO            TO  FLG-TENSU
           INITIALIZE                  TNS-TENSU-REC
           MOVE    LNK-NYUKSNGET-HOSPNUM
                                   TO  TNS-HOSPNUM
           MOVE    LNK-NYUKSNGET-NYUIN-SRYCD
                                   TO  TNS-SRYCD
           MOVE    LNK-NYUKSNGET-SANTEIYM
                                   TO  TNS-YUKOSTYMD(1:6)
           MOVE    "01"            TO  TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD   TO  TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
               MOVE    ZERO        TO  FLG-TENSU
           ELSE
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF    ( FLG-TENSU       NOT =   ZERO )
               GO  TO  48011-TENSU-SEL-EXT
           END-IF
      *
           MOVE    TNS-NYUKHNKSNKBN
                                   TO  WRK-BTU-NYUKSNKBN
           MOVE    TNS-CDKBN-KBNNUM TO WRK-BTU-CDKBN-KBNNUM
           MOVE    TNS-CDKBN-KBNNUM-EDA TO WRK-BTU-CDKBN-KBNNUM-EDA
      *    ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ¤Î¤È¤­¡¢
      *    É½ÍÑÈÖ¹æ¤ÎÃÖ¤­´¹¤¨
           IF     (TNS-CDKBN-KBNNUM  = 108)   AND
                  (TNS-CDKBN-KOUBAN  =  10)   AND
                  (TNS-YUKOSTYMD    >=  "20120401")
               MOVE   109           TO WRK-BTU-CDKBN-KBNNUM
               MOVE   ZERO          TO WRK-BTU-CDKBN-KBNNUM-EDA
           END-IF
      *
      *    ÆÃÊÌÆþ±¡ÎÁ¤ÎÈ½Äê
           EVALUATE   TNS-NYUKHNKBN
               WHEN   73
               WHEN   74
               WHEN   75
               WHEN   76
               WHEN   77
               WHEN   78
               WHEN   79
               WHEN   83
               WHEN   84
                   MOVE    "1"     TO  WRK-TOKUBETSU-NYUIN
           END-EVALUATE
           IF  TNS-SRYCD      =    "190121810"
                   MOVE    "1"     TO  WRK-TOKUBETSU-NYUIN
           END-IF
           IF  TNS-SRYCD      =    "190799510"
                   MOVE    "1"     TO  WRK-TOKUBETSU-NYUIN
           END-IF
           .
      *
       48011-TENSU-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¸¡º÷½èÍý¡ÊÆþ±¡ÎÁ²Ã»»¡Ë
      *****************************************************************
       48012-TENSU-SEL-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-NYUKSNKBN
           MOVE    ZERO            TO  WRK-KSN-CDKBN-KBNNUM
           MOVE    ZERO            TO  WRK-KSN-CDKBN-KBNNUM-EDA
      *
           MOVE    ZERO            TO  FLG-TENSU
           INITIALIZE                  TNS-TENSU-REC
      *
           MOVE    LNK-NYUKSNGET-HOSPNUM
                                   TO  TNS-HOSPNUM
      *
      *    ÆÃÊÌÆþ±¡ÎÁ»»Äê»þ¤ËÍÄ»ù²Ã»»¤ÈÆýÍÄ»ù²Ã»»¤Î²Ã»»¥³¡¼¥ÉÃÖ¤­´¹¤¨
           IF      WRK-TOKUBETSU-NYUIN   =   "1"
               EVALUATE   WRK-NYUKHNKSN-SRYCD(IDX8)
                   WHEN   "190100470"
                       MOVE  "190100570"  TO  WRK-NYUKHNKSN-SRYCD(IDX8)
                   WHEN   "190100770"
                       MOVE  "190100870"  TO  WRK-NYUKHNKSN-SRYCD(IDX8)
               END-EVALUATE
           END-IF
      *
           MOVE    WRK-NYUKHNKSN-SRYCD(IDX8)
                                   TO  TNS-SRYCD
           MOVE    LNK-NYUKSNGET-SANTEIYM
                                   TO  TNS-YUKOSTYMD(1:6)
           MOVE    "01"            TO  TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD   TO  TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
               MOVE    ZERO        TO  FLG-TENSU
           ELSE
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF    ( FLG-TENSU       NOT =   ZERO )
               GO  TO  48012-TENSU-SEL-EXT
           END-IF
      *
           MOVE    TNS-NYUKHNKSNKBN
                                   TO  WRK-KSN-NYUKSNKBN
      *
      *    Æþ±¡ÎÁ¡Ê£Á£±£°£°ÈÖÂæ¡Ë¡¢ÆÃÄêÆþ±¡ÎÁ¡Ê£Á£³£°£°ÈÖÂæ¡Ë¤Ë´Þ¤Þ¤ì¤ë
      *    Ãí²Ã»»¤Î¤ß¤¬È½ÄêÂÐ¾Ý¤È¤Ê¤ë
           IF      (TNS-CDKBN-KBN       =   "A")     AND
                   (TNS-CDKBN-KBNNUM   >=   100)     AND
                   (TNS-CDKBN-KBNNUM   <=   199)
               MOVE    TNS-CDKBN-KBNNUM TO  WRK-KSN-CDKBN-KBNNUM
               MOVE    TNS-CDKBN-KBNNUM-EDA
                                        TO  WRK-KSN-CDKBN-KBNNUM-EDA
           END-IF
      *
           IF      (TNS-CDKBN-KBN       =   "A")     AND
                   (TNS-CDKBN-KBNNUM   >=   300)     AND
                   (TNS-CDKBN-KBNNUM   <=   399)
               MOVE    TNS-CDKBN-KBNNUM TO  WRK-KSN-CDKBN-KBNNUM
               MOVE    TNS-CDKBN-KBNNUM-EDA
                                        TO  WRK-KSN-CDKBN-KBNNUM-EDA
           END-IF
           .
      *
       48012-TENSU-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡´µ¼Ô¤ÎÇ¯Îð»»½Ð
      *****************************************************************
       4801-PT-AGE-SEC         SECTION.
      *
           INITIALIZE                      PTINF-REC
           MOVE    LNK-NYUKSNGET-HOSPNUM
                                       TO  PTINF-HOSPNUM
           MOVE    LNK-NYUKSNGET-PTID  TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO  )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTINF           =   1
               MOVE    ZERO            TO  WRK-NENREI
           ELSE
               MOVE   PTINF-BIRTHDAY   TO  WRK-YMDS1
               MOVE   LNK-NYUKSNGET-SANTEIYM
                                       TO  WRK-YMDS2(1:6)
               MOVE   "01"             TO  WRK-YMDS2(7:2)
               MOVE   ZERO             TO  WRK-NENREI
               COMPUTE  WRK-NENREI     =   WRK-YYS2
                                       -   WRK-YYS1
               IF   WRK-YMDS1(5:4)     >   WRK-YMDS2(5:4)
                    ADD   -1           TO  WRK-NENREI
               END-IF
           END-IF
      *
           .
      *
       4801-PT-AGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÉÂÅï¾ðÊó¸¡º÷½èÍý
      *****************************************************************
       4801-BTUSEL-SEC             SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    LNK-NYUKSNGET-HOSPNUM
                                       TO  SYS-HOSPNUM
           MOVE    "5001"              TO  SYS-KANRICD
           MOVE    LNK-NYUKSNGET-BTUNUM
                                       TO  SYS-KBNCD
           MOVE    LNK-NYUKSNGET-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-5001-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYS-5001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       4801-BTUSEL-EXT.
           EXIT.
      *****************************************************************
      *    ÉÂ¼¼¾ðÊó¸¡º÷½èÍý
      *****************************************************************
       4801-BRMSEL-SEC             SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    LNK-NYUKSNGET-HOSPNUM
                                       TO  SYS-HOSPNUM
           MOVE    "5002"              TO  SYS-KANRICD
           MOVE    LNK-NYUKSNGET-BRMNUM
                                       TO  SYS-KBNCD
           MOVE    LNK-NYUKSNGET-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-5002-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYS-5002-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       4801-BRMSEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¸¡º÷½èÍý
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£ÂÆÉ¤ß¹þ¤ß½èÍý
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¥¯¥í¡¼¥¹½èÍý
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
