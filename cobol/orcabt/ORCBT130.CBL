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
       PROGRAM-ID.             ORCBT130.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : Æü¼¡Åý·×
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : Æü¼¡Åý·×£Ã£Ó£Ö¥Ç¡¼¥¿ºîÀ®
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  05/03/31    NACL¡ÝÆ£¸¶    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  03.05.01    NACL-Æ£¸¶    07/04/19  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *  03.05.02    NACL-Æ£¸¶    07/09/19  ¥¯¥é¥¤¥¢¥ó¥ÈÊÝÂ¸ÂÐ±þ
      *
      *  04.05.01    NACL-Æ£¸¶    10/02/12  ³ÈÄ¥´Á»úÂÐ±þ
      *  04.05.02    NACL-Æ£¸¶    12/06/22  ½èÍý·ë²Ì²èÌÌ¤Ø¤Î·ï¿ôÉ½¼¨
      *  04.05.03    NACL-Æ£¸¶    12/07/02  ½»½êÍó¤ËÈÖÃÏÊý½ñ¤òÄÉ²Ã
      *
      *  04.06.01    NACL-Æ£¸¶    10/06/21  ÆüËÜ¸ì²½¡Ê¸«½Ð¤·¡ËÂÐ±þ
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    Æü¼¡Åý·×¥Ç¡¼¥¿¡Ê´ðËÜ¾ðÊó¡Ë
           SELECT  TOUKEI04-FILE   ASSIGN  TOUKEI04PARA
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   FILE    STATUS  IS  STS-TOUKEI04.
      *
      *    £Ã£Ó£Ö¥Ç¡¼¥¿¡Ê¿ÇÎÅ¾ðÊó¡Ë 
           SELECT  TOUKEI05-FILE   ASSIGN  TOUKEI05PARA
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-TOUKEI05.
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
           SELECT  TOUKEIERR-FILE  ASSIGN  TOUKEIERR
                                   FILE    STATUS  IS  STS-TOUKEIERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    Æü¼¡Åý·×¥Ç¡¼¥¿¡Ê´ðËÜ¾ðÊó¡Ë
       FD  TOUKEI04-FILE.
       01  TOUKEI04-REC. 
           COPY    "CPTCF004.INC".
      *
      *    £Ã£Ó£Ö¥Ç¡¼¥¿¡Ê¿ÇÎÅ¾ðÊó¡Ë
       FD  TOUKEI05-FILE.
       01  TOUKEI05-REC            PIC X(3000). 
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
       FD  TOUKEIERR-FILE.
       01  TOUKEIERR-REC           PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
grpsys*
grpsys COPY    "COMMON-SPA".
      *
      *    Æü¼¡Åý·×¥Ç¡¼¥¿¡Ê´ðËÜ¾ðÊó¡Ë
      *
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01PARA//
                                   BY         //TOUKEI04PARA//.
      *
      *    £Ã£Ó£Ö¥Ç¡¼¥¿¡Ê¿ÇÎÅ¾ðÊó¡Ë
       01  TOUKEI05PARA.
           03  FILLER                  PIC  X(05) VALUE "/tmp/".
           03  TOUKEI05PARA-HOSPNUM    PIC  9(02).
           03  TOUKEI05PARA-FILE-ID    PIC  X(12) VALUE "toukei11.dat".
      *
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //TOUKEIERR//.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  STS-AREA.
           03  STS-TOUKEI04                    PIC X(02).
           03  STS-TOUKEI05                    PIC X(02).
           03  STS-TOUKEIERR                   PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-TOUKEI04                    PIC 9(06).
           03  CNT-TOUKEI05                    PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX                             PIC 9(03). 
           03  IDY                             PIC 9(03). 
           03  IDZ                             PIC 9(03).
           03  IDW                             PIC 9(03).
           03  IDX1                            PIC 9(03). 
           03  IDX2                            PIC 9(03). 
           03  IDX3                            PIC 9(03). 
      *
      *    °ì»þÎÎ°è
      *
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-TERMID             PIC X(32).
               05  WRK-PARA-KOJINKBN           PIC X(01).
               05  WRK-PARA-JOBID              PIC 9(07).
               05  WRK-PARA-SHELLID            PIC X(08).
               05  WRK-PARA-HOSPNUM            PIC 9(02).
               05  WRK-PARA-FILENM1            PIC X(150).
               05  WRK-PARA-FILE1              PIC X(20).
               05  WRK-PARA-FILENM1-FOLDER     PIC X(150).
      *
           03  WRK-TOUKEIERR           PIC X(200). 
           03  WRK-RENNUM              PIC 9(03).
      *
      *     ±¦¤º¤áÊÔ½¸ÍÑ     
           03  WRK-HENKAN              PIC X(20).
           03  WRK-LENGTH              PIC 9(02).
           03  WRK-HENSYU              PIC X(20).
      *    ½ÐÎÏ¥Ç¡¼¥¿ÊÔ½¸ÍÑ
           03  WRK-REC                 PIC X(3000).
           03  WRK-REC-LENGTH          PIC 9(04).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA            PIC X(400).
               05  WRK-NUM             PIC ZZZZZZZZZ9.999. 
               05  WRK-NAGASA          PIC 9(03).
               05  WRK-ST              PIC 9(02). 
               05  WRK-SYOSUU          PIC 9(01).
               05  WRK-END             PIC 9(01).
      *
      *    ¥Ø¥Ã¥À¡¼¾ðÊó
       01  CSV-HEAD-01.
           03  FILLER              PIC X(12)   VALUE   "°åÎÅµ¡´Ø£É£Ä".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "°åÎÅµ¡´Ø¥³¡¼¥É".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "¿ÇÎÅÇ¯·îÆü".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "¥ì¥³¡¼¥É¼±ÊÌ".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(18)   VALUE
               "¥ì¥»¥×¥È¼ïÊÌ¥³¡¼¥É".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "Æþ³°¶èÊ¬".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "¿ÇÎÅ²Ê¥³¡¼¥É".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(18)   VALUE
               "¥ì¥»ÅÅ¿ÇÎÅ²Ê¥³¡¼¥É".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "ÊÝ¸±ÁÈ¹ç¤»ÈÖ¹æ".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "´µ¼ÔÈÖ¹æ".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "´µ¼Ô£É£Ä".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "¥«¥Ê»áÌ¾".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "´Á»ú»áÌ¾".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "À­ÊÌ".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "À¸Ç¯·îÆü".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "´ðËÜÎÁ¶èÊ¬".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "»þ´Ö³°¶èÊ¬".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "±ý¿Ç¶èÊ¬".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "¾Ò²ð¶èÊ¬".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "Æþ³°Ãæ³°Íè¼±ÊÌ".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "°å»Õ¥³¡¼¥É".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "ÆþÂà±¡¶èÊ¬".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "ÉÂÅïÈÖ¹æ".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "ÉÂ¼¼ÈÖ¹æ".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "³°Çñ¶èÊ¬".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "¿©»ö¶èÊ¬".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "Âà±¡»öÍ³".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(16)   VALUE
               "Á°Æü¿ÇÎÅ²Ê¥³¡¼¥É".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(22)   VALUE
               "Á°Æü¥ì¥»ÅÅ¿ÇÎÅ²Ê¥³¡¼¥É".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "Á°ÆüÉÂÅïÈÖ¹æ".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "Á°ÆüÉÂ¼¼ÈÖ¹æ".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "ÅÁÉ¼ÈÖ¹æ".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "ÅÁÉ¼¾õÂÖ¶èÊ¬".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "ÅÁÉ¼ºîÀ®¶èÊ¬".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "ÊÝ¸±ÅÀ¿ô".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "ÊÝ¸±ÉéÃ´¶â³Û".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "ÊÝ¸±Å¬ÍÑ³°¶â³Û".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "ÊÝ¸±Å¬ÍÑ³°ÀÇ³Û".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "¼«Èñ¶â³Û".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "¼«ÈñÀÇ³Û".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "¿©»öÉéÃ´¶â³Û".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "¾ÃÈñÀÇ³Û".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "¼¼ÎÁº¹³Û".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "¼¼ÎÁº¹³ÛÀÇ³Û".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "ÀÁµá¶â³Û".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "ÊÝ¸±ÈÖ¹æ".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "ÊÝ¸±£É£Ä".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "ÊÝ¸±¼ÔÈÖ¹æ".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "ËÜ¿Í²ÈÂ²¶èÊ¬".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "Êä½õ¶èÊ¬".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "·ÑÂ³¶èÊ¬".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "¸øÈñÈÖ¹æ£±".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "¸øÈñ£É£Ä£±".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "»ÙÊ§¶èÊ¬£±".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "¸øÈñÈÖ¹æ£²".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "¸øÈñ£É£Ä£²".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "»ÙÊ§¶èÊ¬£²".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "¸øÈñÈÖ¹æ£³".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "¸øÈñ£É£Ä£³".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "»ÙÊ§¶èÊ¬£³".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "¸øÈñÈÖ¹æ£´".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "¸øÈñ£É£Ä£´".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "»ÙÊ§¶èÊ¬£´".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "Ï«ºÒÊÝ¸±¶èÊ¬".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "¼«ÇåÀÕÀÁµá¶èÊ¬".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "Í¹ÊØÈÖ¹æ".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(18)   VALUE
               "ÃÏÊý¸ø¶¦ÃÄÂÎ¥³¡¼¥É".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "½»½ê".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "ÅÅÏÃÈÖ¹æ".
      *  
       01  WRK-CONS-AREA.
      *    ¥³¥ó¥Þ
           03  WRK-KUGIRI              PIC X(01)   VALUE   ",".
      *    ²þ¹Ô¥³¡¼¥É
           03  WRK-KAIGYO              PIC X(01)   VALUE   X"0D".
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥¸¥ç¥Ö´ÉÍý¥Þ¥¹¥¿
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑÎÎ°è
      *****************************************************************
      *    ¿ô»úÊÑ´¹ÎÎ°è
           COPY    "CPORCSNUM.INC".
      *
      *   ¥¸¥ç¥Ö´ÉÍý£Ä£ÂÀ©¸æ¥µ¥Ö
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ¥¯¥é¥¤¥¢¥ó¥ÈÊÝÂ¸£Ä£ÂÀ©¸æ¥µ¥Ö
           COPY    "CPORCSFILESV.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(1000).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1 
      *
           PERFORM 300-TERM-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    ½é´ü¡¡½èÍý
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       FLG-AREA
                                       SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA-TERMID
                                               WRK-PARA-KOJINKBN
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               TOUKEIERR
                                               WRK-PARA-FILENM1
                                               WRK-PARA-FILE1
                                               WRK-PARA-FILENM1-FOLDER
           END-UNSTRING 
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ¥¹¥Æ¥Ã¥×´ÉÍý³«»Ï½èÍý
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBT130"      TO  JOB-PGID
           MOVE    "£Ã£Ó£Ö¥Ç¡¼¥¿ºîÀ®"
                                   TO  JOB-SHELLMSG
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           MOVE    "TOKEI110"          TO  TOUKEI04PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  TOUKEI04PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  TOUKEI04PARA-HOSPNUM
      *
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  TOUKEI05PARA-HOSPNUM
      *
           OPEN    INPUT               TOUKEI04-FILE
                   OUTPUT              TOUKEI05-FILE
      *
           PERFORM 900-TOUKEI04-READ-SEC   
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    £Ã£Ó£Ö¡Ê£È£Å£Á£Ä¥ì¥³¡¼¥É¡ËÊÔ½¸½èÍý
           PERFORM 2002-CSV-HEAD-HENSYU-SEC
      *     
      *    £Ã£Ó£Ö¥Õ¥¡¥¤¥ë¡Ê¿ÇÎÅ¥Ç¡¼¥¿¡Ë½èÍý
           PERFORM 2001-FILE-HENSYU-SEC
      *
           PERFORM 900-TOUKEI04-READ-SEC   
           .
       200-MAIN-EXT.
           EXIT. 
      *
      *****************************************************************
      *    £Ã£Ó£Ö¥Õ¥¡¥¤¥ë¡Ê¿ÇÎÅ¥Ç¡¼¥¿¡Ë½èÍý
      *****************************************************************
       2001-FILE-HENSYU-SEC         SECTION.
      *
      *    ´ðËÜ¾ðÊó¼èÆÀ
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *    °åÎÅµ¡´Ø£É£Ä      
           MOVE    TOUKEI04-HOSPID     TO  WRK-DATA
           MOVE    24                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    °åÎÅµ¡´Ø¥³¡¼¥É
           IF      WRK-PARA-KOJINKBN   =   "1"
               MOVE    TOUKEI04-HOSPCD     TO  WRK-DATA
               MOVE    7                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    ¿ÇÎÅÇ¯·î
           MOVE    TOUKEI04-SRYYMD     TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    ¥ì¥³¡¼¥É¼±ÊÌ
           MOVE    TOUKEI04-RECSKB     TO  WRK-NUM
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC  
      *    ¥ì¥»¥×¥È¼ïÊÌ
           MOVE    TOUKEI04-RECESYUBETU
                                       TO  WRK-DATA
           MOVE    4                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    Æþ³°¶èÊ¬
           MOVE    TOUKEI04-NYUGAIKBN  TO  WRK-NUM
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    ¿ÇÎÅ²Ê
           MOVE    TOUKEI04-SRYKA      TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    ¥ì¥»ÅÅ¿ÇÎÅ²Ê
           MOVE    TOUKEI04-RECEKA     TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    ÊÝ¸±ÁÈ¹ç¤»
           MOVE    TOUKEI04-HKNCOMBINUM
                                       TO  WRK-NUM
           MOVE    4                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *
      *    ´µ¼Ô¾ðÊó
      *  
      *    ´µ¼ÔÈÖ¹æ
           MOVE    TOUKEI04-PTNUM      TO  WRK-DATA
           MOVE    20                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    ´µ¼Ô£É£Ä
           MOVE    TOUKEI04-PTID       TO  WRK-NUM
           MOVE    10                  TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC  
      *    ¥«¥Ê»áÌ¾
           IF      WRK-PARA-KOJINKBN   =   "1"
               MOVE    TOUKEI04-KANANAME   TO  WRK-DATA
               MOVE    50                  TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    »áÌ¾
           IF      WRK-PARA-KOJINKBN   =   "1"
               MOVE    TOUKEI04-NAME       TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    À­ÊÌ
           MOVE    TOUKEI04-SEX        TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    À¸Ç¯·îÆü
           MOVE    TOUKEI04-BIRTHDAY   TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ³°Íè¾ðÊó
      *
      *    ´ðËÜÎÁ¶èÊ¬
           MOVE    TOUKEI04-KIHONKBN   TO  WRK-NUM
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    »þ´Ö³°¶èÊ¬
           MOVE    TOUKEI04-TIMEKBN    TO  WRK-NUM
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    ±ý¿Ç¶èÊ¬
           MOVE    TOUKEI04-OSINKBN    TO  WRK-NUM
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    ¾Ò²ð¶èÊ¬
           MOVE    TOUKEI04-SHOUKAIKBN TO  WRK-NUM
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    Æþ±¡Ãæ³°Íè¼±ÊÌ
           MOVE    TOUKEI04-NYUHAISKB  TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    °å»Õ¥³¡¼¥É
           MOVE    TOUKEI04-DRCD       TO  WRK-DATA
           MOVE    5                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *
      *    Æþ±¡¾ðÊó
      *
      *    ÆþÂà±¡¶èÊ¬¡Ê£±¡§ÅöÆüÆþ±¡¡¡£²¡§ÅöÆüÂà±¡¡¡£³¡§Æþ±¡Ãæ¡Ë
           MOVE    TOUKEI04-NYUIN-KBN  TO  WRK-NUM
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    ÉÂÅïÈÖ¹æ
           MOVE    TOUKEI04-BTUNUM     TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    ÉÂ¼¼ÈÖ¹æ
           MOVE    TOUKEI04-BRMNUM     TO  WRK-DATA
           MOVE    6                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    ³°Çñ¶èÊ¬
           MOVE    TOUKEI04-GAIHAKU-KBN
                                       TO  WRK-NUM
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    ¿©»ö¶èÊ¬
           MOVE    TOUKEI04-SKJ-KBN    TO  WRK-NUM
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    Âà±¡»öÍ³
           MOVE    TOUKEI04-TAIINCD    TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    Á°Æü¿ÇÎÅ²Ê
           MOVE    TOUKEI04-MAE-SRYKA  TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    Á°Æü¥ì¥»ÅÅ¿ÇÎÅ²Ê
           MOVE    TOUKEI04-MAE-RECEKA TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    Á°ÆüÉÂÅïÈÖ¹æ
           MOVE    TOUKEI04-MAE-BTUNUM TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    Á°ÆüÉÂ¼¼ÈÖ¹æ
           MOVE    TOUKEI04-MAE-BRMNUM
                                       TO  WRK-DATA
           MOVE    6                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *   
      *    ¼ýÇ¼¾ðÊó¾ðÊó
      *
      *    ÅÁÉ¼ÈÖ¹æ
           MOVE    TOUKEI04-DENPNUM    TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    ÅÁÉ¼¾õÂÖ¶èÊ¬
           MOVE    TOUKEI04-DENPJTIKBN TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    ÅÁÉ¼ºîÀ®¶èÊ¬
           MOVE    TOUKEI04-CREATEKBN  TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    ÊÝ¸±ÅÀ¿ô
           MOVE    TOUKEI04-HKNTEN     TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    ÊÝ¸±ÉéÃ´¶â³Û
           MOVE    TOUKEI04-HKNMONEY   TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    ÊÝ¸±Å¬ÍÑ³°¶â³Û
           MOVE    TOUKEI04-TGMONEY    TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    ÊÝ¸±Å¬ÍÑ³°¶âÀÇ³Û
           MOVE    TOUKEI04-TGMONEY-TAX
                                       TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    ¼«Èñ¶â³Û
           MOVE    TOUKEI04-JIHI       TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    ¼«ÈñÀÇ³Û
           MOVE    TOUKEI04-JIHI-TAX   TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    ¿©»öÉéÃ´¶â³Û
           MOVE    TOUKEI04-SKJ        TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    ¾ÃÈñÀÇ³Û
           MOVE    TOUKEI04-SKJ-TAX    TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    ¼¼ÎÁº¹³Û
           MOVE    TOUKEI04-RMSAGAKU   TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    ¼¼ÎÁº¹³ÛÀÇ³Û
           MOVE    TOUKEI04-RMSAGAKU-TAX
                                       TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    ÀÁµá¶â³Û
           MOVE    TOUKEI04-SKYMONEY   TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *
      *ÊÝ¸±¾ðÊó
      * 
      *    ÊÝ¸±ÈÖ¹æ
           MOVE    TOUKEI04-HKNNUM     TO  WRK-DATA
           MOVE    3                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    ÊÝ¸±£É£Ä
           MOVE    TOUKEI04-HKNID      TO  WRK-NUM
           MOVE    10                  TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    ÊÝ¸±¼ÔÈÖ¹æ
           MOVE    TOUKEI04-HKNJANUM   TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    ËÜ¿Í²ÈÂ²¶èÊ¬
           MOVE    TOUKEI04-HONKZKKBN  TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    Êä½õ¶èÊ¬
           MOVE    TOUKEI04-HOJOKBN    TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    ·ÑÂ³¶èÊ¬
           MOVE    TOUKEI04-CONTKBN    TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    
      *¸øÈñ¾ðÊó
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       4
      *        ¸øÈñ¡Ý¸øÈñÈÖ¹æ
               MOVE    TOUKEI04-KOH-HKNNUM (IDX1)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC  
      *        ¸øÈñ¡Ý¸øÈñ£É£Ä
               MOVE    TOUKEI04-KOH-ID     (IDX1)
                                           TO  WRK-NUM
               MOVE    10                  TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
      *        ¸øÈñ¡Ý»ÙÊ§¶èÊ¬
               MOVE    TOUKEI04-KOH-PAYKBN  (IDX1)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
           END-PERFORM
      *
      *Ï«ºÒ¾ðÊó
      *    
      *    Ï«ºÒ¡ÝÏ«ºÒÊÝ¸±¶èÊ¬
           MOVE    TOUKEI04-RSIHKNKBN      TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    Ï«ºÒ¡Ý¼«ÇåÀÕÀÁµá¶èÊ¬
           MOVE    TOUKEI04-JIBAISEIKBN    TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *´µ¼Ô´ðËÜ¾ðÊó
      *
      *    Í¹ÊØÈÖ¹æ
           MOVE    TOUKEI04-HOME-POST      TO  WRK-DATA
           MOVE    7                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    ÃÏÊý¸ø¶¦ÃÄÂÎ¥³¡¼¥É
           MOVE    TOUKEI04-LPUBCD         TO  WRK-DATA
           MOVE    5                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    ½»½ê
           IF      WRK-PARA-KOJINKBN   =   "1"
               MOVE    TOUKEI04-HOME-ADRS      TO  WRK-DATA
               MOVE    400                     TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    ÅÅÏÃÈÖ¹æ
           IF      WRK-PARA-KOJINKBN   =   "1"
               MOVE    TOUKEI04-HOME-TEL       TO  WRK-DATA
               MOVE    15                      TO  WRK-NAGASA
           END-IF
           MOVE    1                       TO  WRK-END 
           PERFORM 5004-MOJI-HENSYU-SEC
      * 
           PERFORM 5001-FILE-WRITE-SEC
           .
       2001-FILE-SYORI-EXT.
           EXIT. 
      *
      *****************************************************************
      *    £Ã£Ó£Ö¡Ê£È£Å£Á£Ä¥ì¥³¡¼¥É¡ËÊÔ½¸½èÍý
      *****************************************************************
       2002-CSV-HEAD-HENSYU-SEC                SECTION.
      *
           IF      CNT-TOUKEI05        >   ZERO
               GO  TO  2002-CSV-HEAD-HENSYU-EXT
           END-IF
      *
           INITIALIZE                  WRK-REC
                                       WRK-REC-LENGTH
      *
           STRING  CSV-HEAD-01         DELIMITED  BY  SIZE
                   WRK-KAIGYO          DELIMITED  BY  SIZE
                   INTO                WRK-REC
           END-STRING
      *
           MOVE    WRK-REC             TO  TOUKEI05-REC  
           WRITE   TOUKEI05-REC 
           .
       2002-CSV-HEAD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë½ÐÎÏ½èÍý
      *****************************************************************
       5001-FILE-WRITE-SEC        SECTION.
      *
      *    ²þ¹Ô¥³¡¼¥ÉÊÔ½¸ 
           STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                   WRK-KAIGYO                DELIMITED  BY  SIZE
                   INTO        WRK-REC
           END-STRING        
      *
           MOVE    WRK-REC             TO  TOUKEI05-REC  
           WRITE   TOUKEI05-REC 
           ADD     1                   TO  CNT-TOUKEI05
           .
       5001-FILE-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ±¦¤º¤áÊÔ½¸½èÍý
      *****************************************************************
       5003-DATA-RIGHT-HENSYU-SEC          SECTION.
      *
           MOVE    SPACE          TO  WRK-HENSYU
      *              
           IF      WRK-HENKAN     =   SPACE
               CONTINUE
           ELSE    
               PERFORM VARYING IDX FROM    WRK-LENGTH  BY  -1
                       UNTIL   IDX <       1
                   IF      WRK-HENKAN (IDX:1)  NOT =   SPACE
                       COMPUTE IDZ     =   WRK-LENGTH  -   IDX +   1
                       MOVE    WRK-HENKAN (1:IDX)
                                           TO  WRK-HENSYU (IDZ:)
                       MOVE    WRK-HENSYU  TO  WRK-HENKAN                            
                       MOVE    1           TO  IDX
                   END-IF
               END-PERFORM
           END-IF    
      *
           .
       5003-DATA-RIGHT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½ÐÎÏ¥ì¥³¡¼¥É½èÍý¡ÊÊ¸»ú¡Ë
      *****************************************************************
       5004-MOJI-HENSYU-SEC          SECTION.
      *
           IF      WRK-DATA        =   SPACE
               CONTINUE
           ELSE                   
               PERFORM VARYING IDW FROM    WRK-NAGASA  BY  -1
                       UNTIL   IDW <       1
                   IF      WRK-DATA (IDW:1)    NOT =   SPACE
                       IF      WRK-REC-LENGTH      =   ZERO
                           MOVE    WRK-DATA (1:IDW)
                                               TO  WRK-REC
                       ELSE
                           STRING  WRK-REC(1:WRK-REC-LENGTH)
                                               DELIMITED   BY  SIZE
                                   WRK-DATA(1:IDW)
                                               DELIMITED   BY  SIZE
                                   INTO        WRK-REC
                           END-STRING
                       END-IF
                       ADD     IDW                 TO  WRK-REC-LENGTH        
                       MOVE    1                   TO  IDW
                   END-IF
               END-PERFORM
           END-IF
      *     
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING        
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF    
      *
           INITIALIZE                      WRK-HENSYU-AREA
           .
       5004-MOJI-HENSYU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ½ÐÎÏ¥ì¥³¡¼¥É½èÍý¡Ê¿ô»ú¡ËZZZZZZZZZ9.999¤ÎÊÔ½¸
      *****************************************************************
       5005-NUM-HENSYU-SEC          SECTION.
      *
           IF      WRK-NAGASA          =   ZERO
               CONTINUE
           ELSE                   
      *        ³«»Ï°ÌÃÖ
               COMPUTE WRK-ST  =   11      -   WRK-NAGASA
               IF      WRK-SYOSUU          >   ZERO
                   COMPUTE WRK-ST  =   WRK-ST  +   WRK-SYOSUU
                                               +   1
               END-IF                                
      *     
               PERFORM VARYING IDW FROM    WRK-ST    BY  1
                       UNTIL   IDW >       10
                   IF      WRK-NUM (IDW:1)   NOT =   SPACE
                        COMPUTE IDZ     =    11  +   WRK-SYOSUU
                                                 -   IDW       
                        IF      WRK-SYOSUU       >   ZERO
                            ADD     1                TO  IDZ
                        END-IF     
                        STRING  WRK-REC(1:WRK-REC-LENGTH)
                                                 DELIMITED  BY  SIZE
                               WRK-NUM(IDW:IDZ)  DELIMITED  BY  SIZE
                               INTO        WRK-REC
                       END-STRING
                       COMPUTE WRK-REC-LENGTH    =   WRK-REC-LENGTH   +
                                                     IDZ                  
                       MOVE    10                TO  IDW
                   END-IF
               END-PERFORM
           END-IF    
      *     
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING        
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF    
      *
           INITIALIZE                      WRK-HENSYU-AREA
           .
       5005-NUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥¨¥é¡¼½ÐÎÏ½èÍý
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   TOUKEIERR-FILE
           IF      STS-TOUKEIERR         =   ZERO
               CLOSE   TOUKEIERR-FILE
           ELSE
               OPEN    OUTPUT              TOUKEIERR-FILE
      *
               MOVE    WRK-TOUKEIERR         TO  TOUKEIERR-REC
               WRITE   TOUKEIERR-REC
               CLOSE   TOUKEIERR-FILE
      *         
      *        ¥¸¥ç¥Ö´ÉÍý³«»Ï½èÍý
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-TOUKEIERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½ªÎ»½èÍý
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   TOUKEI04-FILE
                   TOUKEI05-FILE
      *
           IF      CNT-TOUKEI04           =   ZERO  
               MOVE    "½èÍýÂÐ¾Ý¤Î¥Ç¡¼¥¿¤¬¤¢¤ê¤Þ¤»¤ó¤Ç¤·¤¿"
                                           TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
      *        ¥Õ¥¡¥¤¥ëÊÝÂ¸¾ðÊó¹¹¿·
               PERFORM 3001-FILE-INFO-INSERT-SEC
           END-IF
      *
      *    ¥¹¥Æ¥Ã¥×´ÉÍý½ªÎ»½èÍý
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           COMPUTE JOB-UPDCNT  =   JOB-UPDCNT      +
                                   CNT-TOUKEI05
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *     
           DISPLAY "TOUKEI04  CNT-IN :" CNT-TOUKEI04
           DISPLAY "TOUKEI05  CNT-OUT:" CNT-TOUKEI05
           DISPLAY "*** ORCBT130 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ëÊÝÂ¸¾ðÊó¹¹¿·½èÍý
      *****************************************************************
       3001-FILE-INFO-INSERT-SEC             SECTION.
      *
           MOVE    ZERO            TO  WRK-RENNUM
      *
           INITIALIZE                  ORCSFILESVAREA
           MOVE    "I"             TO  SFILESV-MODE
           MOVE    WRK-PARA-SHELLID
                                   TO  SFILESV-TBL-KEY
      *
           IF    ( WRK-PARA-FILE1      =   "6"             )
      *****AND   ( CNT-TOUKEI04        >   ZERO            )
               ADD     1               TO  WRK-RENNUM
      *
               MOVE    "toukei2.sh"    TO  SFILESV-SHELLID  (WRK-RENNUM)
               MOVE    1               TO  SFILESV-SHORI-RENNUM
                                                            (WRK-RENNUM)
               MOVE    WRK-RENNUM      TO  SFILESV-RENNUM   (WRK-RENNUM)
      *********MOVE    WRK-PARA-SRYYM  TO  SFILESV-SRYYM    (WRK-RENNUM)
               MOVE    "/tmp/"         TO  SFILESV-FOR-FOLDER
                                                            (WRK-RENNUM)
               MOVE    WRK-PARA-FILENM1
                                       TO  SFILESV-FOR-DATA (WRK-RENNUM)
      *********MOVE    WRK-PARA-FILENM1-FOLDER
      *********                        TO  SFILESV-TO-FOLDER
      *********                                             (WRK-RENNUM)
               MOVE    WRK-PARA-FILENM1
                                       TO  SFILESV-TO-DATA  (WRK-RENNUM)
               MOVE    CNT-TOUKEI05    TO  SFILESV-CNT-ALL  (WRK-RENNUM)
               MOVE    "Æü¼¡Åý·×¥Ç¡¼¥¿"
                                       TO  SFILESV-TITLE    (WRK-RENNUM)
               MOVE    "1"             TO  SFILESV-DATA-TYPE(WRK-RENNUM) 
           END-IF
      *
           IF      WRK-RENNUM          >   ZERO   
               CALL    "ORCSFILESV"    USING
                                           ORCSFILESVAREA
                                           SPA-AREA
           END-IF
           .
       3001-FILE-INFO-INSERT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    Æü¼¡Åý·×¥Ç¡¼¥¿¡Ê´ðËÜ¾ðÊó¡ËÆÉ¹þ
      *****************************************************************
       900-TOUKEI04-READ-SEC             SECTION.
      *
           READ    TOUKEI04-FILE
               AT  END
                   MOVE    1         TO  FLG-END
               NOT AT  END
                   ADD     1         TO  CNT-TOUKEI04
           END-READ
           .
       900-TOUKEI04-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥¸¥ç¥Ö´ÉÍý£Ä£ÂÀ©¸æ½èÍý
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¥¢¥¯¥»¥¹½èÍý
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *      
      *****************************************************************
      *    £Ä£Â¥ª¡¼¥×¥ó½èÍý
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
