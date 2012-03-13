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
       PROGRAM-ID.                 ORCSROUNYUIN.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : Æþ±¡
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : Ï«ºÒÆþ±¡²ñ·×ºîÀ®¥µ¥Ö
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  03/07/11    NACL¡Ý¾®Æ¦Âô  ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01    NACL-ÌçÏÆ    04/02/27  Ì¤¼Â»Ü¸º»»·îÅÓÃæÊÑ¹¹ÂÐ±þ
      *  01.00.02    NACL-¾®Æ¦Âô  04/03/11  Í­¾²¿ÇÎÅ½ê£±·²Æþ±¡´ðËÜÎÁ£±
      *                                     ¡Ê°å»ÕÇÛÃÖÅ¬¹ç¡Ë¤ÎÂÐ±þ
      *  01.00.03    NACL-ÌçÏÆ    04/03/17  Â¾°åÎÅµ¡´Ø¼õ¿Ç¤ÎÈ½ÄêÍÑ¥³¡¼¥É
      *                                     ¡ÊÆþ±¡´ðËÜÎÁ²Ã»»¶èÊ¬¡ËÄÉ²Ã
      *  01.00.04    NACL-ÌçÏÆ    04/03/29  êóáì´µ¼Ô´ÉÍý²Ã»»¤ÎÂÐ±þ
      *  01.00.05    NACL-¾®Æ¦Âô  04/09/10  Ï«ºÒ¾è¿ô¤ò¥µ¥Ö¥ë¡¼¥Á¥ó¤«¤é¤Î
      *                                     È½Äê¤ËÊÑ¹¹
      *  01.00.05    NACL-¾®Æ¦Âô  05/09/16  °Ê²¼¤ÎÆÃÄêÆþ±¡ÎÁ¤Î³°Çñ·×»»¤ò½¤Àµ
      *                                     ¿ÇÎÅ½êÏ·¿Í°åÎÅ´ÉÍýÎÁ
      *                                     Ï·¿ÍÀ­Ç§ÃÎ¾É¼À´µ¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
      *                                     Ï·¿ÍÀ­Ç§ÃÎ¾É¼À´µ¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
      *  01.00.06    NACL-ÌçÏÆ    06/01/20  É¸·çÂÐ±þ
      *  01.00.07    NACL-¾®Æ¦Âô  06/03/22  ÆÃÄêÆþ±¡ÎÁÄÉ²ÃÊ¬¡ÊÇ¾Â´Ãæ¥±¥¢¥æ¥Ë¡Ë
      *                                     ³°ÇñÃÖ¤­´¹¤¨½èÍý
      *  02.09.01    NACL-ÌçÏÆ    06/04/13  £²£°£°£¶Ç¯£´·î²þÀµÂÐ±þ
      *  02.09.02    NACL-ÌçÏÆ    06/05/17  Î¬¾ÎÊÔ½¸(190123250Åù)
      *  02.09.03    NACL-ÌçÏÆ    06/05/23  ³°Çñ»þ¤ÎÆþ±¡´ðËÜÎÁ¼èÆÀ½¤Àµ
      *  02.09.04    NACL-ÌçÏÆ    06/06/02  êóáì´µ¼Ô´ÉÍý²Ã»»ÊÔ½¸ÂÐ±þ
      *  02.09.05    NACL-ÌçÏÆ    06/06/05  Ê£¿ô°å»ÕµÚ¤Ó´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»¤È
      *                                     ´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»¤ÎÊÔ½¸ÂÐ±þ
      *  02.09.06    NACL-ÌçÏÆ    06/06/23  Ç§ÃÎµ¡Ç½¾ã³²²Ã»»¤ÎÊÔ½¸ÂÐ±þ
      *  03.01.01    NACL-ÌçÏÆ    06/06/30  ÎÅÍÜÉÂÅï¤ÎÂ¾°åÎÅµ¡´Ø¼õ¿ÇÂÐ±þ
      *  03.01.02    NACL-¾®Æ¦Âô  06.07.04  Æþ±¡ÎÁ¤Ë´Þ¤á¤ë²Ã»»¤¬¥³¥á¥ó¥È
      *                                     ¤ÈÆ±»þÆþÎÏ¤µ¤ì¤¿¾ì¹ç¤ÎÂÐ±þ
      *  03.04.01    NACL-ÌçÏÆ    06/12/01  ¼«ÇåÀÕ·òÊÝ½àµòÂÐ±þ
      *  03.04.02    NACL-ÌçÏÆ    07/03/12  Êñ³ç¿ÇÎÅÂÐ±þ
      *  03.05.00    NACL-ÌçÏÆ    07/05/08  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *  04.01.01    NACL-ÌçÏÆ    07/10/25  ¸øÌ³ºÒ³²ÂÐ±þ
      *  04.01.02    NACL-ÌçÏÆ    07/12/19  Æþ±¡ÎÁÌ¤»»ÄêÆüÂÐ±þ
      *  04.01.03    NACL-ÌçÏÆ    08/02/04  É¸·ç³°Çñ·×»»½¤Àµ
      *  04.02.01    NACL-ÌçÏÆ    08/02/29  Ê¿À®£²£°Ç¯£´·î²þÀµÂÐ±þ
      *  04.02.02    NACL-¾®Æ¦Âô  08/04/30  Í­¾²¿ÇÎÅ½ê¤Î²Ã»»¤òÏ«ºÒÆþ±¡²Ã»»¤Ë
      *                                     ´Þ¤á¤ë¡ÊÆþ±¡²ñ·×¤è¤ê¡Ë
      *                                     ¡¦Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»
      *                                     ¡¦°å»ÕÇÛÃÖÅ¬¹ç²Ã»»
      *                                     ¡¦´Ç¸îÇÛÃÖ²Ã»»£±
      *                                     ¡¦´Ç¸îÇÛÃÖ²Ã»»£²
      *                                     ¡¦Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±
      *                                     ¡¦Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²
      *                                     ÆÃÄêµ¡Ç½ÉÂ±¡¡ÝÀº¿ÀÉÂÅï¤Î²Ã»»¤òÏ«ºÒÆþ±¡²Ã»»¤Ë
      *                                     ´Þ¤á¤ë¡Ê¿ÇÎÅ²ñ·×¤è¤ê¡Ë
      *                                     ¡¦½ÅÅÙÇ§ÃÎ¾É²Ã»»¡ÊÆÃÄêµ¡Ç½ÉÂ±¡Àº¿ÀÉÂÅï¡Ë
      *                                     ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£±¤Î²Ã»»¤ò
      *                                     Ï«ºÒÆþ±¡²Ã»»¤Ë´Þ¤á¤ë¡ÊÆþ±¡²ñ·×¤è¤ê¡Ë
      *                                     ¡¦½Å¾É´µ¼Ô²óÉüÉÂÅï²Ã»»
      *  04.02.03    NACL-ÌçÏÆ    08/09/24  Æþ±¡Î¬¾ÎÊÔ½¸ÄÉ²Ã
      *  04.02.04    NACL-ÌçÏÆ    08/10/07  Æþ±¡´ðËÜÎÁÍ­¸ú´ü´Ö¥Á¥§¥Ã¥¯
      *  04.04.01    NACL-ÌçÏÆ    09/09/04  ¾®Æþ´ÉÎ¬¾Î½¤Àµ
      *  04.04.02    NACL-ÌçÏÆ    10/05/07  Æþ±¡ÎÁºÞÅÀ¥Á¥§¥Ã¥¯
      *  04.04.03    NACL-ÌçÏÆ    10/06/01  Â¾°åÎÅµ¡´Ø¼õ¿ÇÂÐ±þ
      *  04.04.04    NACL-ÌçÏÆ    10/08/09  Â¾°å¼õ¿Ç¤Î¥Á¥§¥Ã¥¯
      *
      *  04.05.01    NACL-ÌçÏÆ    10/01/18  ¼£¸³ÂÐ±þ
      *  04.05.02    NACL-ÌçÏÆ    11/08/16  Ï«ºÒÉÂ°áÂßÍ¿ÎÁÂÐ±þ
      *  04.05.03    NACL-ÌçÏÆ    12/03/13  ²þÀµÂÐ±þ¡Ê³°Çñ¶èÊ¬£¶¡¤£·¤ÎÂÐ±þ¡Ë
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
           03  FLG-NYUINACCT       PIC 9(01).
           03  FLG-NYUINACT        PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-RJN             PIC 9(01).
           03  FLG-ROUSAI          PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-RYAKU           PIC 9(01).
           03  FLG-NYURYAKUSHO     PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-KSN             PIC 9(01).
           03  FLG-KENPOJYUNKYO    PIC 9(01).
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  ID1                 PIC 9(04).
           03  KSN-IDX             PIC 9(04).
           03  GENSAN-IDX          PIC 9(04).
           03  GENSAN-IDX2         PIC 9(04).
           03  IDV                 PIC 9(02).
           03  SRYACT-IDX          PIC 9(04).
      *
      *    ¥³¥ó¥¹¥¿¥ó¥ÈÃÍ
       01  CONSTANT-AREA.
      *    Æþ±¡¿ÇÎÅ·×²èÌ¤¼Â»Ü¸º»»¡ÊÊ¿À®£±£¸Ç¯£³·î¤Þ¤Ç¡Ë
           03  CONS-KEIKAKU-GENSAN1        PIC X(09) VALUE "190080470".
           03  CONS-KEIKAKU-GENSAN2        PIC X(09) VALUE "190799670".
      *    ±¡Æâ´¶À÷ËÉ»ßÂÐºöÌ¤¼Â»Ü¸º»»¡ÊÊ¿À®£±£¸Ç¯£³·î¤Þ¤Ç¡Ë
           03  CONS-INNAI-GENSAN1          PIC X(09) VALUE "190080570".
           03  CONS-INNAI-GENSAN2          PIC X(09) VALUE "190799770".
      *    °åÎÅ°ÂÁ´´ÉÍýÂÎÀ©Ì¤À°È÷¸º»»¡ÊÊ¿À®£±£¸Ç¯£³·î¤Þ¤Ç¡Ë
           03  CONS-ANZEN-GENSAN1          PIC X(09) VALUE "190111690".
           03  CONS-ANZEN-GENSAN2          PIC X(09) VALUE "190809090".
      *    êóáìÂÐºöÌ¤¼Â»Ü¸º»»¡ÊÊ¿À®£±£¸Ç¯£³·î¤Þ¤Ç¡Ë
           03  CONS-JYOKU-GENSAN1          PIC X(09) VALUE "190111790".
           03  CONS-JYOKU-GENSAN2          PIC X(09) VALUE "190809190".
      *    Æü¾ïÀ¸³è¾ã³²²Ã»»¡ÊÊ¿À®£±£¸Ç¯£¶·î¤Þ¤Ç¡Ë
           03  CONS-NICHIJOU-KSN1          PIC X(09) VALUE "190109170".
           03  CONS-NICHIJOU-KSN2          PIC X(09) VALUE "190807570".
      *    Ç§ÃÎ¾É²Ã»»¡ÊÊ¿À®£±£¸Ç¯£¶·î¤Þ¤Ç¡Ë
           03  CONS-CHIHOU-KSN1            PIC X(09) VALUE "190109270".
           03  CONS-CHIHOU-KSN2            PIC X(09) VALUE "190808670".
      *    êóáì´µ¼Ô´ÉÍý²Ã»»
           03  CONS-JYOKU-KSN1             PIC X(09) VALUE "190116990".
           03  CONS-JYOKU-KSN2             PIC X(09) VALUE "190813390".
      *    Ê£¿ô°å»ÕµÚ¤Ó´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»¡ÊÊ¿À®£²£°Ç¯£³·î¤Þ¤Ç¡Ë
           03  CONS-KANGO-KSN1             PIC X(09) VALUE "190117070".
      *    ´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»¡ÊÊ¿À®£²£°Ç¯£³·î¤Þ¤Ç¡Ë
           03  CONS-KANGO-KSN2             PIC X(09) VALUE "190109670".
      *    Ç§ÃÎµ¡Ç½¾ã³²²Ã»»¡ÊÎÅÍÜÉÂÅï¡Ë¡ÊÊ¿À®£²£°Ç¯£³·î¤Þ¤Ç¡Ë
           03  CONS-NINSYO-KSN1            PIC X(09) VALUE "190121970".
      *    Ç§ÃÎµ¡Ç½¾ã³²²Ã»»¡ÊÍ­¾²¿ÇÎÅ½ê¡Ë¡ÊÊ¿À®£²£°Ç¯£³·î¤Þ¤Ç¡Ë
           03  CONS-NINSYO-KSN2            PIC X(09) VALUE "190122670".
      *
      *--Ê¿À®£²£°Ç¯£´·î°Ê¹ß¤ËÂÐ½è¤òÄÉ²Ã¤·¤¿²Ã»»------------------------
      *    Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»
           03  CONS-200804-KSN1            PIC X(09) VALUE "190126470".
      *    °å»ÕÇÛÃÖÅ¬¹ç²Ã»»¡Ê£²£²Ç¯£´·î°Ê¹ß¡¢°å»ÕÇÛÃÖ²Ã»»£²¡Ë
           03  CONS-200804-KSN2            PIC X(09) VALUE "190117070".
      *    ´Ç¸îÇÛÃÖ²Ã»»£±¡Ê£²£²Ç¯£´·î°Ê¹ß¡¢´Ç¸îÇÛÃÖ²Ã»»£²¡Ë
           03  CONS-200804-KSN3            PIC X(09) VALUE "190126570".
      *    ´Ç¸îÇÛÃÖ²Ã»»£²¡Ê£²£²Ç¯£´·î°Ê¹ß¡¢´Ç¸îÇÛÃÖ²Ã»»£±¡Ë
           03  CONS-200804-KSN4            PIC X(09) VALUE "190109670".
      *    Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±¡Ê£²£²Ç¯£´·î°Ê¹ß¡¢Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²¡Ë
           03  CONS-200804-KSN5            PIC X(09) VALUE "190126670".
      *    Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²¡Ê£²£²Ç¯£´·î°Ê¹ß¡¢Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±¡Ë
           03  CONS-200804-KSN6            PIC X(09) VALUE "190126770".
      *    ½ÅÅÙÇ§ÃÎ¾É²Ã»»¡ÊÆÃÄêµ¡Ç½ÉÂ±¡Àº¿ÀÉÂÅï¡Ë
           03  CONS-200804-KSN7            PIC X(09) VALUE "190119210".
      *    êóáìÉ¾²Á¼Â»Ü²Ã»»¡ÊÎÅÍÜÉÂÅï¡Ë
           03  CONS-200804-KSN8            PIC X(09) VALUE "190121970".
      *    êóáìÉ¾²Á¼Â»Ü²Ã»»¡ÊÍ­¾²¿ÇÎÅ½ê¡Ë
           03  CONS-200804-KSN9            PIC X(09) VALUE "190122670".
      *    ½Å¾É´µ¼Ô²óÉüÉÂÅï²Ã»»
           03  CONS-200804-KSN10           PIC X(09) VALUE "190129870".
      *
      *--Ê¿À®£²£²Ç¯£´·î°Ê¹ß¤ËÂÐ½è¤òÄÉ²Ã¤·¤¿²Ã»»------------------------
      *    ½Å¾É»ù¡Ê¼Ô¡Ë¼õÆþÏ¢·È²Ã»»
           03  CONS-201004-KSN11           PIC X(09) VALUE "190135070".
      *    °å»ÕÇÛÃÖ²Ã»»£±
           03  CONS-201004-KSN12           PIC X(09) VALUE "190135570".
      *    °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»
           03  CONS-201004-KSN13           PIC X(09) VALUE "190131570".
      *    µßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»
           03  CONS-201004-KSN14           PIC X(09) VALUE "190134270".
      *    ½ÅÅÙÇ§ÃÎ¾É²Ã»»¡ÊÀº¿ÀÉÂÅïÆþ±¡´ðËÜÎÁ¡Ë
           03  CONS-201004-KSN15           PIC X(09) VALUE "190118570".
      *    °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»¡ÊÆÃÄêµ¡Ç½ÉÂ±¡¡Ë
           03  CONS-201004-KSN16           PIC X(09) VALUE "190134870".
      *    °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»¡ÊÀìÌçÉÂ±¡¡Ë
           03  CONS-201004-KSN17           PIC X(09) VALUE "190134970".
      *    Í­¾²¿ÇÎÅ½ê°ìÈÌÉÂ¾²½é´ü²Ã»»
           03  CONS-201004-KSN18           PIC X(09) VALUE "190135470".
      *    µßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»
           03  CONS-201004-KSN19           PIC X(09) VALUE "190135670".
      *----------------------------------------------------------------
      *
      *    Ï«ºÒ¼£ÎÅ·×²è²Ã»»
           03  CONS-ROSAICHIRYO            PIC X(09) VALUE "101910050".
      *    ÉÂ°áÂßÍ¿ÎÁ
           03  CONS-BYOUI                  PIC X(09) VALUE "101910060".
      *    ÆÃÊÌ´Æ»ëÎÁ
           03  CONS-TOKUKANSI              PIC X(09) VALUE "101920010".
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
      *
           03  WRK-GAIHAKU-SRYCD           PIC X(09).
           03  WRK-MEISAISU                PIC 9(03).
           03  WRK-RYAKU-SRYCD             PIC X(09).
           03  WRK-RYAKUSHO                PIC X(14).
           03  WRK-SRYKBN                  PIC X(02).
           03  WRK-KSNKBN                  PIC 9(03).
           03  WRK-ZAISKBKBN               PIC X(01).
           03  WRK-SANTEIED-DDX            PIC X(02).
           03  WRK-SANTEIED-DD9            REDEFINES  WRK-SANTEIED-DDX
                                           PIC 9(02).
      *
           03  WRK-SYS5000-COUNT           PIC 9(02).
      *
           03  WRK-GENSAN2-MIJISI          PIC 9(01).
           03  WRK-GENSAN3-MIJISI          PIC 9(01).
           03  WRK-GENSAN4-MIJISI          PIC 9(01).
      *
           03  WRK-SHOKIKSN-TAIHI          PIC X(09).
           03  WRK-HYOKETU-TAIHI           PIC X(09).
           03  WRK-HYOKETU-TEN             PIC 9(09)V99.
           03  WRK-ZAITEN-HYOKETU          PIC 9(08).
      *
           03  WRK-NYUINRYO-ERR            PIC 9(01).
           03  WRK-NYUINRYO-ERR2           PIC 9(01).
           03  WRK-TAIJYUSIN-ERR           PIC 9(01).
      *
           03  WRK-KHNTEN                  PIC 9(08). 
           03  WRK-KSNTEN                  PIC S9(08).
           03  WRK-HYOTEN                  PIC 9(09)V99.
           03  WRK-HIKAKU-ZAITEN           PIC 9(08).
      *
      *    Ï«ºÒÆþ±¡²ñ·×¥ï¡¼¥¯
           03  WRK-ROUSAI-TABLE.
               05  WRK-ROUSAI-TBL          OCCURS  31.
                   07  WRK-NYUINKHN        PIC X(09).
                   07  WRK-SHOKIKSN        PIC X(09).
                   07  WRK-HYOKETU         PIC X(09).
                   07  WRK-GAIHAKU         PIC X(09).
                   07  WRK-NYUKHNKSNKBN    PIC 9(03).
      *
      *    Æþ±¡ÎÁ²Ã»»ÂàÈòÎÎ°è
           03  WRK-NYUKSN-TABLE.
               05  WRK-NYUKSN-TBL          OCCURS  20.
                   07  WRK-NYUKSN          PIC X(09).
                   07  WRK-NYUKSN-ZAITEN   PIC 9(08).
                   07  WRK-NYUKSN-DAY      PIC 9(03)  OCCURS 31.
      *
      *    Ì¤¼Â»Ü¸º»»ÂàÈòÍÑ¥Õ¥é¥°¥Æ¡¼¥Ö¥ë
           03  WRK-GENSAN-TABLE.
               05  WRK-GENSAN-INF          OCCURS  31.
                   07  WRK-GENSAN-INNAI    PIC X(01).
                   07  WRK-GENSAN-IRYO     PIC X(01).
                   07  WRK-GENSAN-JYOKU    PIC X(01).
      *
      *    ±¡Æâ´¶À÷ËÉ»ßÂÐºöÌ¤¼Â»Ü¸º»»¥ï¡¼¥¯
           03  WRK-GENSAN2                 PIC X(09).
      *    °åÎÅ°ÂÁ´´ÉÍýÂÎÀ©Ì¤À°È÷¸º»»¥ï¡¼¥¯
           03  WRK-GENSAN3                 PIC X(09).
      *    êóáìÂÐºöÌ¤¼Â»Ü¸º»»¥ï¡¼¥¯
           03  WRK-GENSAN4                 PIC X(09).
      *
      *    Æþ±¡¿ÇÎÅ·×²èÌ¤¼Â»Ü¸º»»¥ï¡¼¥¯
           03  WRK-KEIKAKU-GENSAN1         PIC 9(03) OCCURS 31.
           03  WRK-KEIKAKU-GENSAN2         PIC 9(03) OCCURS 31.
      *    ±¡Æâ´¶À÷ËÉ»ßÂÐºöÌ¤¼Â»Ü¸º»»¥ï¡¼¥¯
           03  WRK-INNAI-GENSAN1           PIC 9(03) OCCURS 31.
           03  WRK-INNAI-GENSAN2           PIC 9(03) OCCURS 31.
      *    °åÎÅ°ÂÁ´´ÉÍýÂÎÀ©Ì¤À°È÷¸º»»¥ï¡¼¥¯
           03  WRK-ANZEN-GENSAN1           PIC 9(03) OCCURS 31.
           03  WRK-ANZEN-GENSAN2           PIC 9(03) OCCURS 31.
      *    êóáìÂÐºöÌ¤¼Â»Ü¸º»»¥ï¡¼¥¯
           03  WRK-JYOKU-GENSAN1           PIC 9(03) OCCURS 31.
           03  WRK-JYOKU-GENSAN2           PIC 9(03) OCCURS 31.
      *
      *    Æü¾ïÀ¸³è¾ã³²²Ã»»ÂàÈò
           03  WRK-NICHIJOU-KSN1           PIC 9(03) OCCURS 31.
           03  WRK-NICHIJOU-KSN2           PIC 9(03) OCCURS 31.
      *
      *    ÃÔÊò²Ã»»ÂàÈò
           03  WRK-CHIHOU-KSN1             PIC 9(03) OCCURS 31.
           03  WRK-CHIHOU-KSN2             PIC 9(03) OCCURS 31.
      *
      *    êóáì´µ¼Ô´ÉÍý²Ã»»
           03  WRK-JYOKU-KSN1              PIC 9(03) OCCURS 31.
           03  WRK-JYOKU-KSN2              PIC 9(03) OCCURS 31.
      *
      *    Ê£¿ô°å»ÕµÚ¤Ó´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»
           03  WRK-KANGO-KSN1              PIC 9(03) OCCURS 31.
      *    ´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»
           03  WRK-KANGO-KSN2              PIC 9(03) OCCURS 31.
      *
      *    Ç§ÃÎµ¡Ç½¾ã³²²Ã»»¡ÊÎÅÍÜÉÂÅï¡Ë
           03  WRK-NINSYO-KSN1             PIC 9(03) OCCURS 31.
      *    Ç§ÃÎµ¡Ç½¾ã³²²Ã»»¡ÊÍ­¾²¿ÇÎÅ½ê¡Ë
           03  WRK-NINSYO-KSN2             PIC 9(03) OCCURS 31.
      *
      *--Ê¿À®£²£°Ç¯£´·î°Ê¹ß¤ËÂÐ½è¤òÄÉ²Ã¤·¤¿²Ã»»¤ÎÂàÈòÎÎ°è--------------
           03  WRK-200804-KSN1             PIC 9(03) OCCURS 31.
           03  WRK-200804-KSN2             PIC 9(03) OCCURS 31.
           03  WRK-200804-KSN3             PIC 9(03) OCCURS 31.
           03  WRK-200804-KSN4             PIC 9(03) OCCURS 31.
           03  WRK-200804-KSN5             PIC 9(03) OCCURS 31.
           03  WRK-200804-KSN6             PIC 9(03) OCCURS 31.
           03  WRK-200804-KSN7             PIC 9(03) OCCURS 31.
           03  WRK-200804-KSN8             PIC 9(03) OCCURS 31.
           03  WRK-200804-KSN9             PIC 9(03) OCCURS 31.
           03  WRK-200804-KSN10            PIC 9(03) OCCURS 31.
      *--Ê¿À®£²£²Ç¯£´·î°Ê¹ß¤Ë¿·Àß¤µ¤ì¤¿²Ã»»¤ÎÂàÈòÎÎ°è--------------
           03  WRK-201004-KSN11            PIC 9(03) OCCURS 31.
           03  WRK-201004-KSN12            PIC 9(03) OCCURS 31.
           03  WRK-201004-KSN13            PIC 9(03) OCCURS 31.
           03  WRK-201004-KSN14            PIC 9(03) OCCURS 31.
           03  WRK-201004-KSN15            PIC 9(03) OCCURS 31.
           03  WRK-201004-KSN16            PIC 9(03) OCCURS 31.
           03  WRK-201004-KSN17            PIC 9(03) OCCURS 31.
           03  WRK-201004-KSN18            PIC 9(03) OCCURS 31.
           03  WRK-201004-KSN19            PIC 9(03) OCCURS 31.
      *----------------------------------------------------------------
      *
      *    ¿ÇÎÅ¥³¡¼¥É·×½¸·×ÍÑ
           03  WRK-SRYCD-X                 PIC X(09).
           03  WRK-SRYCD-9    REDEFINES    WRK-SRYCD-X
                                           PIC 9(09).
      *    Æþ±¡ÎÁÌ¤»»Äê¶èÊ¬
           03  WRK-NYUINDAY-TBL.
               05  WRK-NYUINDAY-KBN        PIC X(01) OCCURS  31.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK5000.INC".
      *
      *    Æþ±¡¿ÇÎÅ²ñ·×¥Þ¥¹¥¿¡Ý
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    ¿ÇÎÅ²ñ·×
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    ¿ÇÎÅ¹Ô°Ù
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿¡Ý
           COPY    "CPTENSU.INC".
      *
      *    ´µ¼Ô¥Þ¥¹¥¿¡Ý
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ÊÝ¸±ÁÈ¤ß¹ç¤ï¤»
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    Æþ±¡Î¬¾Î
       01  NYURYAKUSHO-REC.
           COPY    "CPNYUINRYAKUSHO.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
      ***  COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *   ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ÊÝ¸±»»ÄêÍÑÇ¯Îð¡¦³ä¹ç·×»»¥µ¥Ö
           COPY    "CPORCSAGECHK.INC".
      *
      *    ÄÌ»»Æü¿ô¼èÆÀ¥µ¥Ö¡ÊÏ«ºÒÊÝ¸±ÍÑ¡Ë
           COPY    "CPORCSRSITUSAN.INC".
      *
      *    Æþ±¡ÎÁÌ¤»»ÄêÆü¼èÆÀ¥µ¥Ö
           COPY    "CPORCSNYUINDAY.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
        LINKAGE                     SECTION.
           COPY    "MCPAREA".
      *    Ï«ºÒÆþ±¡²ñ·×ºîÀ®ÍÑ¥Ñ¥é¥á¥¿
           COPY    "CPORCSROUNYUIN.INC".
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           ORCSROUNYUIN-AREA
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
           INITIALIZE                  LNK-SROUNYUIN-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-SROUNYUIN-RC
      *
      *    ¥Ñ¥é¥á¡¼¥¿¥Á¥§¥Ã¥¯
           PERFORM 100-PRM-CHK-SEC
           IF    ( LNK-SROUNYUIN-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    ¼ç½èÍý
           IF      LNK-SROUNYUIN-RC      =    ZERO
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
           IF    ( LNK-SROUNYUIN-SANTEIYM    =   SPACE )
               MOVE    1               TO  LNK-SROUNYUIN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼¸¡º÷
           INITIALIZE                      PTINF-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO  PTINF-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID      TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    2               TO  LNK-SROUNYUIN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    Ï«ºÒÆþ±¡Ãæ¤Î¥Á¥§¥Ã¥¯¡ÊÆþ±¡²ñ·×¥Þ¥¹¥¿¡¼¸¡º÷¡Ë
           INITIALIZE                      NYUINACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID      TO  NACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "5"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           NOT =   ZERO
               MOVE    3               TO  LNK-SROUNYUIN-RC
           ELSE
               MOVE   "tbl_nyuinacct"      TO  MCP-TABLE
               MOVE   "key17"              TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT          NOT =   ZERO
                    MOVE    3               TO  LNK-SROUNYUIN-RC
               ELSE
      *            Ï«ºÒÊÝ¸±¤ÎÈ½Äê
                   MOVE      ZERO              TO  FLG-ROUSAI
                   PERFORM   VARYING   IDX1   FROM  1   BY  1
                             UNTIL    IDX1   >   31
                        IF   LNK-SROUNYUIN-HKNCOMBI  =
                                               NACCT-DAY(IDX1)
                             MOVE    1         TO  FLG-ROUSAI
                             MOVE   "1"        TO  
                                          LNK-SROUNYUIN-NYUIN-FLG(IDX1)
                        END-IF
                   END-PERFORM
      **>          IF   FLG-ROUSAI             =   ZERO
      **>               MOVE    3              TO  LNK-SROUNYUIN-RC
      **>          END-IF
               END-IF
           END-IF
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinacct"      TO  MCP-TABLE
           MOVE     "key17"              TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           IF   LNK-SROUNYUIN-RC    NOT =   ZERO
                   GO  TO  100-PRM-CHK-EXT
           END-IF
      *
           PERFORM   110-RJN-TAISYO-CHK-SEC
      *
           IF   LNK-SROUNYUIN-RC    NOT =   ZERO
                   GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    Æþ±¡ÎÁÌ¤»»ÄêÆü¼èÆÀ
           INITIALIZE                          ORCSNYUINDAY-AREA
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO  LNK-NYUINDAY-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID      TO  LNK-NYUINDAY-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"          USING   ORCSNYUINDAY-AREA
                                                   SPA-AREA
           IF      LNK-NYUINDAY-RC     =   ZERO
              PERFORM    VARYING   IDX1    FROM   1   BY   1
                          UNTIL    IDX1     >    31
                IF   LNK-SROUNYUIN-NYUIN-FLG(IDX1) = "1"
                   MOVE  LNK-NYUINDAY-KBN(IDX1)
                                           TO  WRK-NYUINDAY-KBN(IDX1)
                END-IF
              END-PERFORM
           END-IF
      *
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ï·¿ÍÂÐ¾Ý´µ¼Ô¥Á¥§¥Ã¥¯Åù
      *****************************************************************
       110-RJN-TAISYO-CHK-SEC         SECTION.
      *
           MOVE    ZERO                 TO   FLG-RJN
      *
           MOVE    ZERO                 TO   FLG-HKNCOMBI
           INITIALIZE                        HKNCOMBI-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM
                                        TO   COMB-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID   TO   COMB-PTID
           MOVE    LNK-SROUNYUIN-HKNCOMBI
                                        TO   COMB-HKNCOMBINUM
           MOVE    "tbl_hkncombi"       TO   MCP-TABLE
           MOVE    "key"                TO   MCP-PATHNAME
           PERFORM 910-HKNCOMBI-SELECT-SEC
           IF      FLG-HKNCOMBI         =    ZERO
               MOVE     "tbl_hkncombi"  TO   MCP-TABLE
               MOVE     "key"           TO   MCP-PATHNAME
               PERFORM   910-HKNCOMBI-READ-SEC
           END-IF
           MOVE     "tbl_hkncombi"      TO   MCP-TABLE
           MOVE     "key"               TO   MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *    Æþ±¡´µ¼Ô¤ÎÇ¯Îð·×»»
           INITIALIZE                       ORCSAGECHKAREA
           MOVE    LNK-SROUNYUIN-HOSPNUM
                                        TO  AGECHK-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID   TO  AGECHK-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM
                                        TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"                 TO  AGECHK-SRYYMD(7:2)
           MOVE    PTINF-BIRTHDAY       TO  AGECHK-BIRTHDAY
           MOVE    LNK-SROUNYUIN-HKNCOMBI
                                        TO  AGECHK-HKNCOMBINUM
           MOVE    "1"                  TO  AGECHK-NYUGAIKBN
           CALL    "ORCSAGECHK"         USING
                                            ORCSAGECHKAREA
                                            SPA-AREA
           IF      AGECHK-RC            =   ZERO
               CONTINUE
           ELSE
               INITIALIZE    ORCSAGECHKAREA
           END-IF
      *
      *    Ï·¿ÍÂÐ¾Ý´µ¼ÔÈ½Äê
           IF    ((FLG-HKNCOMBI        =   ZERO      ) AND
                  ((COMB-KOH1HKNNUM    =   "027"    ) OR
                   (COMB-HKNNUM        =   "039"    )))
           OR    ( AGECHK-NENREI      >=    75        )
           OR    ((AGECHK-NENREI      >=    70       ) AND
                  (PTINF-BIRTHDAY     <=   "19320930"))
               MOVE    1               TO  FLG-RJN
           END-IF
      *
      *    ·òÊÝ½àµòÈ½Äê
           MOVE      ZERO       TO    FLG-KENPOJYUNKYO
           IF      AGECHK-HKNKBN =   4  OR  5
              MOVE      1       TO    FLG-KENPOJYUNKYO
           END-IF
      *
           IF      FLG-HKNCOMBI    NOT =   ZERO
                  MOVE    4     TO    LNK-SROUNYUIN-RC
           END-IF
      *
           .
       110-RJN-TAISYO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç¼èÆÀ
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
           IF      LNK-SROUNYUIN-SANTEIYM   <=   "200803"
               PERFORM 210-ROUNYUIN-SET1-SEC
           ELSE
               IF      LNK-SROUNYUIN-SANTEIYM   <=   "201003"
                   PERFORM 210-ROUNYUIN-SET2-SEC
               ELSE
                   PERFORM 210-ROUNYUIN-SET3-SEC
               END-IF
           END-IF
      *
           IF  LNK-SROUNYUIN-RC  =  ZERO
             IF   WRK-TAIJYUSIN-ERR =  1
                MOVE     9     TO    LNK-SROUNYUIN-RC
             END-IF
           END-IF
           IF  LNK-SROUNYUIN-RC  =  ZERO
             IF   WRK-NYUINRYO-ERR  =  1
                MOVE     7     TO    LNK-SROUNYUIN-RC
             END-IF
           END-IF
           IF  LNK-SROUNYUIN-RC  =  ZERO
             IF   WRK-NYUINRYO-ERR2 =  1
                MOVE     8     TO    LNK-SROUNYUIN-RC
             END-IF
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ï«ºÒÆþ±¡¥µ¥Ö¤ÎÊÖµÑÃÍ¥»¥Ã¥È¡ÊÊ¿À®£²£°Ç¯£³·î¤Þ¤Ç¡Ë
      *****************************************************************
       210-ROUNYUIN-SET1-SEC            SECTION.
      *
      *Ï«ºÒ¾è¿ô¤Î£²½µ´Ö°ÊÆâ¤ÎºÇ½ªÆü¤ò¼èÆÀ
           INITIALIZE                      ORCSRSITUSANAREA
           MOVE    LNK-SROUNYUIN-HOSPNUM
                                       TO  RSITUSAN-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID
                                       TO  RSITUSAN-PTID
           MOVE    LNK-SROUNYUIN-HKNCOMBI
                                       TO  RSITUSAN-HKNCOMBI
           MOVE    LNK-SROUNYUIN-SANTEIYM
                                       TO  RSITUSAN-SRYYM
           CALL    "ORCSRSITUSAN"      USING
                                           ORCSRSITUSANAREA
                                           SPA-AREA
           IF    ( RSITUSAN-RC          =   ZERO )
               CONTINUE
           ELSE
               INITIALIZE  ORCSRSITUSANAREA
           END-IF
           PERFORM   VARYING   IDX1   FROM 1  BY  1
                     UNTIL     IDX1   >    31
               MOVE  RSITUSAN-DAY(IDX1) TO LNK-SROUNYUIN-JOUSU-KBN(IDX1)
           END-PERFORM
      *Ï«ºÒÆþ±¡ÎÁ¤Î¼èÆÀ
           MOVE    ZERO                    TO  FLG-OK
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID      TO  NACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "1"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   MOVE      ZERO     TO      FLG-RYAKU
                                              WRK-KSNKBN
                   MOVE      SPACE    TO      WRK-RYAKUSHO
                                              WRK-SRYKBN
                                              WRK-SHOKIKSN-TAIHI
                                              WRK-HYOKETU-TAIHI
                   IF    NACCT-SRYCDTOTAL     =    99999916
                       CONTINUE
                   ELSE
      *                ³ºÅöÆþ±¡¹Ô°Ù¥Æ¡¼¥Ö¥ë¤ÎÆÉ¤ß¹þ¤ß
                       PERFORM    210-NYUINACT-GET-SEC
      *                Æþ±¡´ðËÜÎÁ¤È½é´ü²Ã»»¡ÊÄ¹´ü¸º»»¡Ë¤ÎÂàÈò
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF    (LNK-SROUNYUIN-NYUIN-FLG(IDX1) = "1")
                           AND   (NACCT-DAY(IDX1)    NOT =  ZERO) 
                               MOVE    NSRY-SRYCD(1)   TO  
                                                   WRK-NYUINKHN(IDX1)
      *
                               IF   FLG-RYAKU   =   ZERO
      *
                                  IF   NSRY-SRYCD(1)  NOT =   SPACE
                                      INITIALIZE    TNS-TENSU-REC
                                      MOVE NSRY-SRYCD(1)
                                                TO  TNS-SRYCD
                                      MOVE LNK-SROUNYUIN-HOSPNUM
                                                TO  TNS-HOSPNUM
                                      MOVE LNK-SROUNYUIN-SANTEIYM
                                                TO  TNS-YUKOSTYMD(1:6)
                                      MOVE "01" TO  TNS-YUKOSTYMD(7:2)
                                      MOVE LNK-SROUNYUIN-SANTEIYM
                                                TO  TNS-YUKOEDYMD(1:6)
                                      MOVE "01" TO  TNS-YUKOEDYMD(7:2)
                                      PERFORM 910-TENSU-READ-SEC
                                      IF  FLG-TENSU  NOT = ZERO
                                        MOVE  1 TO  WRK-NYUINRYO-ERR
                                      END-IF
                                  END-IF
      *
                                  PERFORM  VARYING  IDX3  FROM  2  BY  1
                                            UNTIL   IDX3   >    3
                                    IF   NSRY-SRYCD(IDX3)  NOT =   SPACE
                                      INITIALIZE    TNS-TENSU-REC
                                      MOVE NSRY-SRYCD(IDX3)
                                                TO  TNS-SRYCD
                                      MOVE LNK-SROUNYUIN-HOSPNUM
                                                TO  TNS-HOSPNUM
                                      MOVE LNK-SROUNYUIN-SANTEIYM
                                                TO  TNS-YUKOSTYMD(1:6)
                                      MOVE "01" TO  TNS-YUKOSTYMD(7:2)
                                      MOVE LNK-SROUNYUIN-SANTEIYM
                                                TO  TNS-YUKOEDYMD(1:6)
                                      MOVE "01" TO  TNS-YUKOEDYMD(7:2)
                                      PERFORM 910-TENSU-READ-SEC
                                      EVALUATE  TNS-TENSIKIBETU
                                         WHEN     3
                                         WHEN     8
                                             MOVE   NSRY-SRYCD(IDX3)  TO
                                                      WRK-SHOKIKSN-TAIHI
                                         WHEN     6
                                             MOVE   NSRY-SRYCD(IDX3)  TO
                                                      WRK-HYOKETU-TAIHI
                                      END-EVALUATE
                                    END-IF
                                  END-PERFORM
      *
                                  MOVE    NACCT-SRYKBN
                                                TO  WRK-SRYKBN
      *                           ³°Çñ¥³¡¼¥ÉÊÔ½¸ÍÑ¤ËÆþ±¡´ðËÜÎÁ²Ã»»¶èÊ¬
      *                           ¤ÎÂàÈò
                                  PERFORM   210-GAIHAKU-MAE-SEC
      *
                                  MOVE   NSRY-SRYCD(1)
                                                TO  WRK-RYAKU-SRYCD
                                  MOVE   NACCT-ZAISKBKBN
                                                TO  WRK-ZAISKBKBN
                                  PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                                  MOVE    1     TO    FLG-RYAKU
                               END-IF
      *
                               IF    WRK-SHOKIKSN-TAIHI  NOT = SPACE
                                  MOVE  WRK-SHOKIKSN-TAIHI
                                               TO  WRK-SHOKIKSN(IDX1)
                               END-IF
                               IF    WRK-HYOKETU-TAIHI   NOT = SPACE
                                  MOVE  WRK-HYOKETU-TAIHI
                                               TO  WRK-HYOKETU(IDX1)
                               END-IF
      *
                               MOVE  WRK-SRYKBN
                                         TO  LNK-SROUNYUIN-SRYKBN(IDX1)
                               MOVE  WRK-KSNKBN
                                         TO  WRK-NYUKHNKSNKBN(IDX1)
      *
                               IF    WRK-RYAKUSHO   NOT =  SPACE
                                  MOVE  WRK-RYAKUSHO
                                      TO  LNK-SROUNYUIN-RYAKUSYO(IDX1)
                               END-IF
      *
                               MOVE    1    TO   FLG-OK
                           END-IF
                       END-PERFORM
                   END-IF
                   MOVE   "tbl_nyuinacct"   TO  MCP-TABLE
                   MOVE   "key17"           TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
      ***      IF   FLG-OK     =   ZERO
      ***          MOVE     6      TO      LNK-SROUNYUIN-RC
      ***      END-IF
           ELSE
               MOVE     6      TO      LNK-SROUNYUIN-RC
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinacct"   TO  MCP-TABLE
           MOVE     "key17"           TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           IF   LNK-SROUNYUIN-RC    NOT =   ZERO
                   GO   TO    200-MAIN-EXT
           END-IF
      *
      *Ï«ºÒÆþ±¡¤ÎÏ«ºÒ¾è¿ô¡Ê´ü´Ö²Ã»»¡Ë¤Ë´Þ¤á¤ëÃí²Ã»»¤Î¼èÆÀ
           PERFORM    210-CHUKSN-GET-SEC
      *
      *Ï«ºÒÆþ±¡»þ¤Î³°Çñ¼èÆÀ
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID      TO  NACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "2"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
      *            ³°Çñ¶èÊ¬¤ÎÂàÈò
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
      ****             IF    (WRK-NYUINKHN(IDX1) NOT = SPACE) 
                       IF     NACCT-DAY(IDX1)    NOT =  ZERO
                              PERFORM     210-GAIHAKU-SET-SEC
                       END-IF
                   END-PERFORM
                   MOVE   "tbl_nyuinacct"  TO  MCP-TABLE
                   MOVE   "key17"          TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinacct"        TO  MCP-TABLE
           MOVE     "key17"                TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *·îÆâ¤ËÊ£¿ô¤ÎÆþ±¡´ðËÜ¾ðÊó¡Ê£µ£°£°£°¡Ë¤¬¤Ê¤¤¤«¸¡º÷
           PERFORM   210-SYS-5000-CHK-SEC
      *
      *°åÎÅµ¡´Ø¸º»»¾ðÊó¤Î¼èÆÀ¡Ê¥·¥¹¥Æ¥à´ÉÍý¥Æ¡¼¥Ö¥ë¤è¤ê¡Ë
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-SROUNYUIN-HOSPNUM
                                       TO  SYS-HOSPNUM
           MOVE    LNK-SROUNYUIN-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    LNK-SROUNYUIN-SANTEIYM
                                       TO  SYS-EDYUKYMD(1:6)
           MOVE    "01"                TO  SYS-EDYUKYMD(7:2)
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               PERFORM  VARYING   GENSAN-IDX  FROM  1  BY  1
                        UNTIL     GENSAN-IDX   >   4
                    IF    SYS-5000-GENKBN(GENSAN-IDX)   =   "2"
                        EVALUATE  GENSAN-IDX
                            WHEN  2
                                MOVE  "190080570"  TO  WRK-GENSAN2
      **                          IF   FLG-RJN    =    ZERO
      **                              MOVE  "190080570"  TO  WRK-GENSAN2
      **                          ELSE
      **                              MOVE  "190799770"  TO  WRK-GENSAN2
      **                          END-IF
                                PERFORM VARYING GENSAN-IDX2 FROM 1 BY 1
                                    UNTIL     GENSAN-IDX2   >   31
                                  MOVE   "1"  TO
                                          WRK-GENSAN-INNAI(GENSAN-IDX2)
                                END-PERFORM
                            WHEN  3
                                MOVE  "190111690"  TO  WRK-GENSAN3
      **                          IF   FLG-RJN    =    ZERO
      **                              MOVE  "190111690"  TO  WRK-GENSAN3
      **                          ELSE
      **                              MOVE  "190809090"  TO  WRK-GENSAN3
      **                          END-IF
                                PERFORM VARYING GENSAN-IDX2 FROM 1 BY 1
                                    UNTIL     GENSAN-IDX2   >   31
                                  MOVE   "1"  TO
                                          WRK-GENSAN-IRYO(GENSAN-IDX2)
                                END-PERFORM
                            WHEN  4
                                MOVE  "190111790"  TO  WRK-GENSAN4
      **                          IF   FLG-RJN    =    ZERO
      **                              MOVE  "190111790"  TO  WRK-GENSAN4
      **                          ELSE
      **                              MOVE  "190809190"  TO  WRK-GENSAN4
      **                          END-IF
                                PERFORM VARYING GENSAN-IDX2 FROM 1 BY 1
                                    UNTIL     GENSAN-IDX2   >   31
                                  MOVE   "1"  TO
                                          WRK-GENSAN-JYOKU(GENSAN-IDX2)
                                END-PERFORM
                        END-EVALUATE
                    END-IF
               END-PERFORM
           END-IF
      *    ¥·¥¹¥Æ¥à´ÉÍý¥¯¥í¡¼¥º
           MOVE     "tbl_syskanri"      TO  MCP-TABLE
           MOVE     "key10"             TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           IF        WRK-SYS5000-COUNT    =     ZERO
                 CONTINUE
           ELSE
                 PERFORM  210-SYS-5000-GET-SEC
           END-IF
      *
      *
      *Ï«ºÒÆþ±¡»þ¤ÎÆþ±¡ÎÁ²Ã»»¼èÆÀ¡ÊÆþ±¡²ñ·×¤è¤ê¡Ë
           MOVE    ZERO                    TO  KSN-IDX
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID      TO  NACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "6"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   MOVE      ZERO     TO      FLG-RYAKU
                   MOVE      ZERO     TO      FLG-KSN
      *            Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF    ((WRK-NYUINKHN(IDX1) NOT = SPACE) OR
                              (WRK-NYUINDAY-KBN(IDX1) =  "1" ))
                       AND   ( LNK-SROUNYUIN-GAIHAKU(IDX1) =
                                   ZERO OR 4 OR 5 OR 6 OR 7   )
                       AND   ( NACCT-DAY(IDX1)    NOT =  ZERO )
      *
                         IF  (NACCT-SRYCDTOTAL(6:9) = CONS-KANGO-KSN1
                                                   OR CONS-KANGO-KSN2)
                         AND (LNK-SROUNYUIN-SANTEIYM  >= "200604"    )
                          IF    (WRK-NYUINKHN(IDX1)      NOT = SPACE)
                          AND   (LNK-SROUNYUIN-GAIHAKU(IDX1) =  ZERO)
                           EVALUATE   NACCT-SRYCDTOTAL(6:9)
                             WHEN  CONS-KANGO-KSN1
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-KANGO-KSN1(IDX1)
                             WHEN  CONS-KANGO-KSN2
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-KANGO-KSN2(IDX1)
                           END-EVALUATE
      *
                           IF   FLG-RYAKU   =   ZERO
                              MOVE   NACCT-SRYCDTOTAL(6:9)
                                            TO  WRK-RYAKU-SRYCD
                              MOVE   NACCT-ZAISKBKBN
                                            TO  WRK-ZAISKBKBN
                              PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                              MOVE    1     TO    FLG-RYAKU
                           END-IF
                          END-IF
                         ELSE
                           IF   FLG-KSN   =  ZERO
                               ADD     1    TO    KSN-IDX
                               MOVE    1    TO    FLG-KSN
                           END-IF
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                        TO  WRK-NYUKSN(KSN-IDX)
                           MOVE   NACCT-ZAITEN  
                                        TO  WRK-NYUKSN-ZAITEN(KSN-IDX)
                           MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-NYUKSN-DAY(KSN-IDX IDX1)
      *
                           IF   FLG-RYAKU   =   ZERO
                              MOVE   NACCT-SRYCDTOTAL(6:9)
                                            TO  WRK-RYAKU-SRYCD
                              MOVE   NACCT-ZAISKBKBN
                                            TO  WRK-ZAISKBKBN
                              PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                              MOVE    1     TO    FLG-RYAKU
                           END-IF
                         END-IF
      *
                       END-IF
                   END-PERFORM
                   MOVE   "tbl_nyuinacct"   TO  MCP-TABLE
                   MOVE   "key17"           TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE     "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *
      *Ï«ºÒÆþ±¡»þ¤ÎÆþ±¡ÎÁ²Ã»»¼èÆÀ¡Ê¿ÇÎÅ²ñ·×¤è¤ê¡Ë
           INITIALIZE                           SRYACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO   ACCT-HOSPNUM
           MOVE    "1"                     TO   ACCT-NYUGAIKBN
           MOVE    LNK-SROUNYUIN-PTID      TO   ACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO   ACCT-SRYYM
           MOVE    LNK-SROUNYUIN-HKNCOMBI  TO   ACCT-HKNCOMBI
           MOVE    "tbl_sryacct"           TO   MCP-TABLE
           MOVE    "key22"                 TO   MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT           =   ZERO
               MOVE     "tbl_sryacct"      TO   MCP-TABLE
               MOVE     "key22"            TO   MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT    =   1)
                 MOVE      ZERO     TO      FLG-RYAKU
                 IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                      (ACCT-ZAIKAISU  NOT =    ZERO)    AND
                      (ACCT-JIHOKBN       =   SPACE)
                   IF   ACCT-SRYKBN  =  "90" OR "92"
      *
                     INITIALIZE                         SRYACT-REC
                     MOVE    ACCT-HOSPNUM       TO      SRY-HOSPNUM
                     MOVE    ACCT-NYUGAIKBN     TO      SRY-NYUGAIKBN
                     MOVE    ACCT-PTID          TO      SRY-PTID
                     MOVE    ACCT-SRYYM         TO      SRY-SRYYM
                     MOVE    ACCT-ZAINUM        TO      SRY-ZAINUM
                     MOVE    SRYACT-REC         TO      MCPDATA-REC
                     MOVE    "DBSELECT"         TO      MCP-FUNC
                     MOVE    "tbl_sryact"       TO      MCP-TABLE
                     MOVE    "key9"             TO      MCP-PATHNAME
                     CALL    "ORCDBMAIN"        USING   MCPAREA
                                                        MCPDATA-REC
                                                        SPA-AREA
                     IF      MCP-RC               =       ZERO
                         MOVE    "tbl_sryact"   TO      MCP-TABLE
                         MOVE    "key9"         TO      MCP-PATHNAME
                         PERFORM  910-SRYACT-READ-SEC
                         IF   FLG-SRYACT    =    0
                            IF (SRY-SRYCD(1)  =  CONS-KEIKAKU-GENSAN1
                                              OR CONS-KEIKAKU-GENSAN2
                                              OR CONS-INNAI-GENSAN1
                                              OR CONS-INNAI-GENSAN2
                                              OR CONS-ANZEN-GENSAN1
                                              OR CONS-ANZEN-GENSAN2
                                              OR CONS-JYOKU-GENSAN1
                                              OR CONS-JYOKU-GENSAN2
                                              OR CONS-NICHIJOU-KSN1
                                              OR CONS-NICHIJOU-KSN2
                                              OR CONS-CHIHOU-KSN1
                                              OR CONS-CHIHOU-KSN2  )
                           OR ((SRY-SRYCD(1)  =  CONS-JYOKU-KSN1
                                               OR  CONS-JYOKU-KSN2) AND
                              (LNK-SROUNYUIN-SANTEIYM  < "200604"  ))
                           OR ((SRY-SRYCD(1)    =  CONS-KANGO-KSN1
                                               OR  CONS-KANGO-KSN2) AND
                              (LNK-SROUNYUIN-SANTEIYM >= "200604"  ))
                           OR ((SRY-SRYCD(1)    = CONS-NINSYO-KSN1
                                               OR CONS-NINSYO-KSN2) AND
                              (LNK-SROUNYUIN-SANTEIYM >= "200607"  ))
      *                                        
                                MOVE  SRY-SRYCD(1)   TO
                                               ACCT-SRYCDTOTAL(6:9)
                            END-IF
                         END-IF
                     END-IF
      *              ¿ÇÎÅ¹Ô°Ù¥¯¥í¡¼¥º
                     MOVE     "tbl_sryact"    TO    MCP-TABLE
                     MOVE     "key9"          TO    MCP-PATHNAME
                     PERFORM   910-DB-CLOSE-SEC
      *
                     IF ( ACCT-SRYCDTOTAL(6:9)  =
                                            CONS-KEIKAKU-GENSAN1
                                        OR  CONS-KEIKAKU-GENSAN2
                                        OR  CONS-INNAI-GENSAN1
                                        OR  CONS-INNAI-GENSAN2
                                        OR  CONS-ANZEN-GENSAN1
                                        OR  CONS-ANZEN-GENSAN2
                                        OR  CONS-JYOKU-GENSAN1
                                        OR  CONS-JYOKU-GENSAN2
                                        OR  CONS-NICHIJOU-KSN1
                                        OR  CONS-NICHIJOU-KSN2
                                        OR  CONS-CHIHOU-KSN1
                                        OR  CONS-CHIHOU-KSN2       )
                     OR ((ACCT-SRYCDTOTAL(6:9)  =  CONS-JYOKU-KSN1
                                               OR  CONS-JYOKU-KSN2) AND
                         (LNK-SROUNYUIN-SANTEIYM  < "200604"      ))
                     OR ((ACCT-SRYCDTOTAL(6:9)  =  CONS-KANGO-KSN1
                                               OR  CONS-KANGO-KSN2) AND
                         (LNK-SROUNYUIN-SANTEIYM >= "200604"      ))
                     OR ((ACCT-SRYCDTOTAL(6:9)  = CONS-NINSYO-KSN1
                                               OR CONS-NINSYO-KSN2) AND
                         (LNK-SROUNYUIN-SANTEIYM >= "200607"      ))
      *                Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF    (WRK-NYUINKHN(IDX1) NOT = SPACE)
                           AND   (LNK-SROUNYUIN-GAIHAKU(IDX1)  
                                                        =  ZERO)
                           AND   (ACCT-DAY(1 IDX1)   NOT =  ZERO)
                              EVALUATE   ACCT-SRYCDTOTAL(6:9)
      *                         Æþ±¡¿ÇÎÅ·×²èÌ¤¼Â»Ü¸º»»
                                WHEN  CONS-KEIKAKU-GENSAN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-KEIKAKU-GENSAN1(IDX1)
                                WHEN  CONS-KEIKAKU-GENSAN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-KEIKAKU-GENSAN2(IDX1)
      *                         ±¡Æâ´¶À÷ËÉ»ßÂÐºöÌ¤¼Â»Ü¸º»»
                                WHEN  CONS-INNAI-GENSAN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-INNAI-GENSAN1(IDX1)
                                WHEN  CONS-INNAI-GENSAN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-INNAI-GENSAN2(IDX1)
      *                         °åÎÅ°ÂÁ´´ÉÍýÂÎÀ©Ì¤À°È÷¸º»»
                                WHEN  CONS-ANZEN-GENSAN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ANZEN-GENSAN1(IDX1)
                                WHEN  CONS-ANZEN-GENSAN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ANZEN-GENSAN2(IDX1)
      *                         êóáìÂÐºöÌ¤¼Â»Ü¸º»»
                                WHEN  CONS-JYOKU-GENSAN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-JYOKU-GENSAN1(IDX1)
                                WHEN  CONS-JYOKU-GENSAN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-JYOKU-GENSAN2(IDX1)
      *                         Æü¾ïÀ¸³è¾ã³²²Ã»»
                                WHEN  CONS-NICHIJOU-KSN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-NICHIJOU-KSN1(IDX1)
                                WHEN  CONS-NICHIJOU-KSN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-NICHIJOU-KSN2(IDX1)
      *                         ÃÔÊò²Ã»»
                                WHEN  CONS-CHIHOU-KSN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-CHIHOU-KSN1(IDX1)
                                WHEN  CONS-CHIHOU-KSN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-CHIHOU-KSN2(IDX1)
      *                         êóáì´µ¼Ô´ÉÍý²Ã»»
                                WHEN  CONS-JYOKU-KSN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-JYOKU-KSN1(IDX1)
                                WHEN  CONS-JYOKU-KSN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-JYOKU-KSN2(IDX1)
      *                         Ê£¿ô°å»ÕµÚ¤Ó´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»
                                WHEN  CONS-KANGO-KSN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-KANGO-KSN1(IDX1)
      *                         ´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»
                                WHEN  CONS-KANGO-KSN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-KANGO-KSN2(IDX1)
      *                         Ç§ÃÎµ¡Ç½¾ã³²²Ã»»¡ÊÎÅÍÜÉÂÅï¡Ë
                                WHEN  CONS-NINSYO-KSN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-NINSYO-KSN1(IDX1)
      *                         Ç§ÃÎµ¡Ç½¾ã³²²Ã»»¡ÊÍ­¾²¿ÇÎÅ½ê¡Ë
                                WHEN  CONS-NINSYO-KSN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-NINSYO-KSN2(IDX1)
                              END-EVALUATE
      *
                              IF   FLG-RYAKU   =   ZERO
                                 MOVE   ACCT-SRYCDTOTAL(6:9)
                                               TO  WRK-RYAKU-SRYCD
                                 MOVE   "6"    TO  WRK-ZAISKBKBN
                                 PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                                 MOVE    1     TO    FLG-RYAKU
                              END-IF
      *
                           END-IF
                       END-PERFORM
                     ELSE
                       IF  (ACCT-SRYCDTOTAL(6:9)  =  CONS-ROSAICHIRYO
                                                 OR  CONS-BYOUI
                                                 OR  CONS-TOKUKANSI)
                       OR  (ACCT-ZAIKBN           =    2           )
      *                  Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                        IF  ACCT-ZAIKBN  =  2
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                                    UNTIL     IDX1   >   31
                             IF    ((WRK-NYUINKHN(IDX1) NOT = SPACE) OR
                                    (WRK-NYUINDAY-KBN(IDX1) =  "1" ))
                             AND   ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
                               COMPUTE LNK-SROUNYUIN-GENTEN(IDX1)
                                    =  LNK-SROUNYUIN-GENTEN(IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(1 IDX1))
                             END-IF
                         END-PERFORM
                        ELSE
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                                    UNTIL     IDX1   >   31
      **>                    IF    ((WRK-NYUINKHN(IDX1) NOT = SPACE) OR
      **>                           (WRK-NYUINDAY-KBN(IDX1) =  "1" ))
      **>                    AND   ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
                             IF    ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCTETC
                                                             (IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCTETC
                                                             (IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(1 IDX1))
      *
                               IF   FLG-RYAKU   =   ZERO
                                  MOVE   ACCT-SRYCDTOTAL(6:9)
                                                TO  WRK-RYAKU-SRYCD
                                  MOVE   "6"    TO  WRK-ZAISKBKBN
                                  PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                                  MOVE    1     TO    FLG-RYAKU
                               END-IF
      *
                             END-IF
                         END-PERFORM
                        END-IF
                       ELSE
      *                  Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                                    UNTIL     IDX1   >   31
                             IF    ((WRK-NYUINKHN(IDX1) NOT = SPACE) OR
                                    (WRK-NYUINDAY-KBN(IDX1) =  "1" ))
                             AND   ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCT(IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCT(IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(1 IDX1))
      *
                               IF   FLG-RYAKU   =   ZERO
                                  PERFORM   210-SRYACT-HENSYU-SEC
      *
                                  MOVE    1     TO    FLG-RYAKU
                               END-IF
      *
                             END-IF
                         END-PERFORM
                       END-IF
                     END-IF
                   END-IF
                 END-IF
                 MOVE     "tbl_sryacct"         TO    MCP-TABLE
                 MOVE     "key22"               TO    MCP-PATHNAME
                 PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *    ¿ÇÎÅ²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_sryacct"         TO    MCP-TABLE
           MOVE     "key22"               TO    MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *
      *    ¼èÆÀ¤·¤¿Ï«ºÒÆþ±¡¾ðÊó¤«¤éÆüÃ±°Ì¤ÎÆþ±¡²ñ·×¾ðÊó¤òºÆ¹½ÃÛ
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               MOVE     ZERO                TO  WRK-MEISAISU
      *        Æþ±¡´ðËÜÎÁ
               IF         WRK-NYUINKHN(IDX1)  NOT =        SPACE
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-NYUINKHN(IDX1) TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "1"           TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ³°Çñ
               IF        (WRK-NYUINKHN(IDX1)  NOT =        SPACE)
               AND       (WRK-GAIHAKU(IDX1)   NOT =        SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GAIHAKU(IDX1)  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "2"           TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        Æü¾ïÀ¸³è¾ã³²²Ã»»
               IF         WRK-NICHIJOU-KSN1(IDX1)   NOT =   ZERO
                   ADD      1                 TO  WRK-MEISAISU
                   MOVE   CONS-NICHIJOU-KSN1  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"            TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-NICHIJOU-KSN2(IDX1)   NOT =   ZERO
                   ADD      1                 TO  WRK-MEISAISU
                   MOVE   CONS-NICHIJOU-KSN2  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"            TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ÃÔÊò²Ã»»
               IF         WRK-CHIHOU-KSN1(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-CHIHOU-KSN1     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-CHIHOU-KSN2(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-CHIHOU-KSN2     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        Ê£¿ô°å»ÕµÚ¤Ó´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»
               IF         WRK-KANGO-KSN1(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-KANGO-KSN1      TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»
               IF         WRK-KANGO-KSN2(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-KANGO-KSN2      TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        Ç§ÃÎµ¡Ç½¾ã³²²Ã»»¡ÊÎÅÍÜÉÂÅï¡Ë
               IF         WRK-NINSYO-KSN1(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-NINSYO-KSN1     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        Ç§ÃÎµ¡Ç½¾ã³²²Ã»»¡ÊÍ­¾²¿ÇÎÅ½ê¡Ë
               IF         WRK-NINSYO-KSN2(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-NINSYO-KSN2     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        êóáì´µ¼Ô´ÉÍý²Ã»»
               IF         WRK-JYOKU-KSN1(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-JYOKU-KSN1      TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-JYOKU-KSN2(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-JYOKU-KSN2      TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        Æþ±¡¿ÇÎÅ·×²èÌ¤¼Â»Ü¸º»»
               IF         WRK-KEIKAKU-GENSAN1(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-KEIKAKU-GENSAN1 TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-KEIKAKU-GENSAN2(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-KEIKAKU-GENSAN2 TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ±¡Æâ´¶À÷ËÉ»ßÂÐºöÌ¤¼Â»Ü¸º»»
               IF         WRK-INNAI-GENSAN1(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-INNAI-GENSAN1   TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-INNAI-GENSAN2(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-INNAI-GENSAN2   TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        °åÎÅ°ÂÁ´´ÉÍýÂÎÀ©Ì¤À°È÷¸º»»
               IF         WRK-ANZEN-GENSAN1(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-ANZEN-GENSAN1   TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-ANZEN-GENSAN2(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-ANZEN-GENSAN2   TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        êóáìÂÐºöÌ¤¼Â»Ü¸º»»
               IF         WRK-JYOKU-GENSAN1(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-JYOKU-GENSAN1   TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-JYOKU-GENSAN2(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-JYOKU-GENSAN2   TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ¸º»»¾ðÊó¡Ê¥·¥¹¥Æ¥à´ÉÍý¡Ë
               IF      (WRK-NYUINKHN(IDX1)   NOT =         SPACE)  AND
                       (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-GENSAN-INNAI(IDX1)   NOT =     SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GENSAN2       TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"          TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                   MOVE     1               TO  WRK-GENSAN2-MIJISI
               END-IF
               IF      (WRK-NYUINKHN(IDX1)   NOT =         SPACE)  AND
                       (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-GENSAN-IRYO(IDX1)   NOT =      SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GENSAN3       TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"          TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                   MOVE     1               TO  WRK-GENSAN3-MIJISI
               END-IF
               IF      (WRK-NYUINKHN(IDX1)   NOT =         SPACE)  AND
                       (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-GENSAN-JYOKU(IDX1)   NOT =     SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GENSAN4       TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"          TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                   MOVE     1               TO  WRK-GENSAN4-MIJISI
               END-IF
      *        Ï«ºÒ¾è¿ô¡ÊÆÃÄêÆþ±¡ÎÁ¤Î¾ì¹ç¤ÏÏ«ºÒ¾è¿ô¤Ê¤·¡Ë
               IF       LNK-SROUNYUIN-SRYKBN(IDX1)    =    "90"
                 IF     FLG-KENPOJYUNKYO  =  1
                   CONTINUE
                 ELSE
                   EVALUATE  LNK-SROUNYUIN-JOUSU-KBN(IDX1)
                       WHEN  "1"
                           ADD      1           TO  WRK-MEISAISU
                           MOVE   "101900010"   TO  LNK-SROUNYUIN-SRYCD
                                               (IDX1 WRK-MEISAISU)
                           MOVE    "4"      TO  LNK-SROUNYUIN-SRYCD-SK
                                               (IDX1 WRK-MEISAISU)
                       WHEN  "2"
                           ADD      1           TO  WRK-MEISAISU
                           MOVE   "101900020"   TO  LNK-SROUNYUIN-SRYCD
                                               (IDX1 WRK-MEISAISU)
                           MOVE    "4"      TO  LNK-SROUNYUIN-SRYCD-SK
                                               (IDX1 WRK-MEISAISU)
                   END-EVALUATE
                 END-IF
               END-IF
      *        ½é´ü²Ã»»¡ÊÄ¹´ü¸º»»¡Ë
               IF      (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-SHOKIKSN(IDX1)   NOT =         SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-SHOKIKSN(IDX1) TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "5"           TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        É¸·ç
               IF       WRK-HYOKETU(IDX1)    NOT =         SPACE
                 IF    (WRK-GAIHAKU(IDX1)        =
                                     SPACE      OR
                                    "190108790" OR  "190108890" OR
                                    "190109090"                 )
                 AND   (WRK-NYUINKHN(IDX1)   NOT =       SPACE  )
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-HYOKETU(IDX1)  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "6"           TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                 ELSE
                   MOVE   SPACE             TO  WRK-HYOKETU(IDX1)
                 END-IF
               END-IF
      *        Æþ±¡ÎÁ²Ã»»¡ÊÆþ±¡²ñ·×¡Ë
      *         PERFORM   VARYING   KSN-IDX   FROM  1  BY  1
      *                   UNTIL    (WRK-NYUKSN(KSN-IDX)  =  SPACE)  OR
      *                            (KSN-IDX             >  20)
      *            IF    WRK-NYUKSN-DAY(KSN-IDX IDX1)  =  ZERO
      *                CONTINUE
      *            ELSE
      *                ADD     1           TO  WRK-MEISAISU
      *                MOVE WRK-NYUKSN(KSN-IDX)
      *                                    TO  LNK-SROUNYUIN-SRYCD
      *                                   (IDX1 WRK-MEISAISU)
      *            END-IF
      *         END-PERFORM
           END-PERFORM
      *
      *    ºÆ¹½ÃÛ¤·¤¿Ï«ºÒÆþ±¡²ñ·×¾ðÊó¤«¤éÅÀ¿ô¤Î»»Äê
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
      *
               MOVE     ZERO       TO       WRK-HYOKETU-TEN
                                            WRK-ZAITEN-HYOKETU
               IF   WRK-HYOKETU(IDX1)   =   SPACE
      *          ÅÀ¿ô¥Þ¥¹¥¿¤ÎÆÉ¤ß¹þ¤ß
                 PERFORM   VARYING   IDX2    FROM   1  BY   1
                      UNTIL (LNK-SROUNYUIN-SRYCD(IDX1 IDX2) = SPACE)
                         OR (IDX2    >      20)
                     PERFORM    210-TENSU-KEISAN-SEC
                 END-PERFORM
               ELSE
      *          ÅÀ¿ô¥Þ¥¹¥¿¤ÎÆÉ¤ß¹þ¤ß
                 INITIALIZE                          TNS-TENSU-REC
                 MOVE    WRK-HYOKETU(IDX1)       TO  TNS-SRYCD
                 MOVE    LNK-SROUNYUIN-HOSPNUM   TO  TNS-HOSPNUM
                 MOVE    LNK-SROUNYUIN-SANTEIYM  TO  TNS-YUKOSTYMD(1:6)
                 MOVE    "01"                    TO  TNS-YUKOSTYMD(7:2)
                 MOVE    LNK-SROUNYUIN-SANTEIYM  TO  TNS-YUKOEDYMD(1:6)
                 MOVE    "01"                    TO  TNS-YUKOEDYMD(7:2)
                 PERFORM 910-TENSU-READ-SEC
                 MOVE    TNS-TEN                 TO  WRK-HYOKETU-TEN
      *
                 PERFORM   VARYING   IDX2    FROM   1  BY   1
                      UNTIL (LNK-SROUNYUIN-SRYCD(IDX1 IDX2) = SPACE)
                         OR (IDX2    >      20)
                     PERFORM    210-TENSU-KEISAN-HYOKETU-SEC
                 END-PERFORM
               END-IF
      *
               PERFORM   VARYING   KSN-IDX   FROM   1  BY   1
                         UNTIL (WRK-NYUKSN(KSN-IDX)  =  SPACE)  OR
                               (KSN-IDX    >      20)
                  IF    WRK-NYUKSN-DAY(KSN-IDX IDX1)  =  ZERO
                      CONTINUE
                  ELSE
                      ADD  WRK-NYUKSN-ZAITEN(KSN-IDX)
                                 TO  LNK-SROUNYUIN-ZAITEN-NACCT(IDX1)
                  END-IF
               END-PERFORM
      *
      ***      IF    LNK-SROUNYUIN-NYUIN-FLG(IDX1)      =   "1"
      ***      AND   LNK-SROUNYUIN-ZAITEN(IDX1)         =  ZERO
      ***         MOVE     SPACE   TO   LNK-SROUNYUIN-NYUIN-FLG(IDX1)
      ***      END-IF
           END-PERFORM
      *
      *
           IF  WRK-GENSAN2-MIJISI  =  1
                 MOVE    WRK-GENSAN2       TO  WRK-RYAKU-SRYCD
                 MOVE    "6"               TO  WRK-ZAISKBKBN
                 PERFORM   210-RYAKUSYO-HENSYU-SEC
           END-IF
      *
           IF  WRK-GENSAN3-MIJISI  =  1
                 MOVE    WRK-GENSAN3       TO  WRK-RYAKU-SRYCD
                 MOVE    "6"               TO  WRK-ZAISKBKBN
                 PERFORM   210-RYAKUSYO-HENSYU-SEC
           END-IF
      *
           IF  WRK-GENSAN4-MIJISI  =  1
                 MOVE    WRK-GENSAN4       TO  WRK-RYAKU-SRYCD
                 MOVE    "6"               TO  WRK-ZAISKBKBN
                 PERFORM   210-RYAKUSYO-HENSYU-SEC
           END-IF
      *
           .
       210-ROUNYUIN-SET1-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ï«ºÒÆþ±¡¥µ¥Ö¤ÎÊÖµÑÃÍ¥»¥Ã¥È¡ÊÊ¿À®£²£°Ç¯£´·î°Ê¹ß¡Ë
      *****************************************************************
       210-ROUNYUIN-SET2-SEC            SECTION.
      *
      *Ï«ºÒ¾è¿ô¤Î£²½µ´Ö°ÊÆâ¤ÎºÇ½ªÆü¤ò¼èÆÀ
           INITIALIZE                      ORCSRSITUSANAREA
           MOVE    LNK-SROUNYUIN-HOSPNUM
                                       TO  RSITUSAN-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID
                                       TO  RSITUSAN-PTID
           MOVE    LNK-SROUNYUIN-HKNCOMBI
                                       TO  RSITUSAN-HKNCOMBI
           MOVE    LNK-SROUNYUIN-SANTEIYM
                                       TO  RSITUSAN-SRYYM
           CALL    "ORCSRSITUSAN"      USING
                                           ORCSRSITUSANAREA
                                           SPA-AREA
           IF    ( RSITUSAN-RC          =   ZERO )
               CONTINUE
           ELSE
               INITIALIZE  ORCSRSITUSANAREA
           END-IF
           PERFORM   VARYING   IDX1   FROM 1  BY  1
                     UNTIL     IDX1   >    31
               MOVE  RSITUSAN-DAY(IDX1) TO LNK-SROUNYUIN-JOUSU-KBN(IDX1)
           END-PERFORM
      *
      *Ï«ºÒÆþ±¡ÎÁ¤Î¼èÆÀ
           MOVE    ZERO                    TO  FLG-OK
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID      TO  NACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "1"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   MOVE      ZERO     TO      FLG-RYAKU
                                              WRK-KSNKBN
                   MOVE      SPACE    TO      WRK-RYAKUSHO
                                              WRK-SRYKBN
                                              WRK-SHOKIKSN-TAIHI
                                              WRK-HYOKETU-TAIHI
                   IF    NACCT-SRYCDTOTAL     =    99999916
                       CONTINUE
                   ELSE
      *                ³ºÅöÆþ±¡¹Ô°Ù¥Æ¡¼¥Ö¥ë¤ÎÆÉ¤ß¹þ¤ß
                       PERFORM    210-NYUINACT-GET-SEC
      *                Æþ±¡´ðËÜÎÁ¤È½é´ü²Ã»»¡ÊÄ¹´ü¸º»»¡Ë¤ÎÂàÈò
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF    (LNK-SROUNYUIN-NYUIN-FLG(IDX1) = "1")
                           AND   (NACCT-DAY(IDX1)    NOT =  ZERO) 
                               MOVE    NSRY-SRYCD(1)   TO  
                                                   WRK-NYUINKHN(IDX1)
      *
                               IF   FLG-RYAKU   =   ZERO
      *
                                  IF   NSRY-SRYCD(1)  NOT =   SPACE
                                      INITIALIZE    TNS-TENSU-REC
                                      MOVE NSRY-SRYCD(1)
                                                TO  TNS-SRYCD
                                      MOVE LNK-SROUNYUIN-HOSPNUM
                                                TO  TNS-HOSPNUM
                                      MOVE LNK-SROUNYUIN-SANTEIYM
                                                TO  TNS-YUKOSTYMD(1:6)
                                      MOVE "01" TO  TNS-YUKOSTYMD(7:2)
                                      MOVE LNK-SROUNYUIN-SANTEIYM
                                                TO  TNS-YUKOEDYMD(1:6)
                                      MOVE "01" TO  TNS-YUKOEDYMD(7:2)
                                      PERFORM 910-TENSU-READ-SEC
                                      IF  FLG-TENSU  NOT = ZERO
                                        MOVE  1 TO  WRK-NYUINRYO-ERR
                                      END-IF
                                  END-IF
      *
                                  PERFORM  VARYING  IDX3  FROM  2  BY  1
                                            UNTIL   IDX3   >    3
                                    IF   NSRY-SRYCD(IDX3)  NOT =   SPACE
                                      INITIALIZE    TNS-TENSU-REC
                                      MOVE NSRY-SRYCD(IDX3)
                                                TO  TNS-SRYCD
                                      MOVE LNK-SROUNYUIN-HOSPNUM
                                                TO  TNS-HOSPNUM
                                      MOVE LNK-SROUNYUIN-SANTEIYM
                                                TO  TNS-YUKOSTYMD(1:6)
                                      MOVE "01" TO  TNS-YUKOSTYMD(7:2)
                                      MOVE LNK-SROUNYUIN-SANTEIYM
                                                TO  TNS-YUKOEDYMD(1:6)
                                      MOVE "01" TO  TNS-YUKOEDYMD(7:2)
                                      PERFORM 910-TENSU-READ-SEC
                                      EVALUATE  TNS-TENSIKIBETU
                                         WHEN     3
                                         WHEN     8
                                             MOVE   NSRY-SRYCD(IDX3)  TO
                                                      WRK-SHOKIKSN-TAIHI
                                         WHEN     6
                                             MOVE   NSRY-SRYCD(IDX3)  TO
                                                      WRK-HYOKETU-TAIHI
                                      END-EVALUATE
                                    END-IF
                                  END-PERFORM
      *
                                  MOVE    NACCT-SRYKBN
                                                TO  WRK-SRYKBN
      *                           ³°Çñ¥³¡¼¥ÉÊÔ½¸ÍÑ¤ËÆþ±¡´ðËÜÎÁ²Ã»»¶èÊ¬
      *                           ¤ÎÂàÈò
                                  PERFORM   210-GAIHAKU-MAE-SEC
      *
                                  MOVE   NSRY-SRYCD(1)
                                                TO  WRK-RYAKU-SRYCD
                                  MOVE   NACCT-ZAISKBKBN
                                                TO  WRK-ZAISKBKBN
                                  PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                                  MOVE    1     TO    FLG-RYAKU
                               END-IF
      *
                               IF    WRK-SHOKIKSN-TAIHI  NOT = SPACE
                                  MOVE  WRK-SHOKIKSN-TAIHI
                                               TO  WRK-SHOKIKSN(IDX1)
                               END-IF
                               IF    WRK-HYOKETU-TAIHI   NOT = SPACE
                                  MOVE  WRK-HYOKETU-TAIHI
                                               TO  WRK-HYOKETU(IDX1)
                               END-IF
      *
                               MOVE  WRK-SRYKBN
                                         TO  LNK-SROUNYUIN-SRYKBN(IDX1)
                               MOVE  WRK-KSNKBN
                                         TO  WRK-NYUKHNKSNKBN(IDX1)
      *
                               IF    WRK-RYAKUSHO   NOT =  SPACE
                                  MOVE  WRK-RYAKUSHO
                                      TO  LNK-SROUNYUIN-RYAKUSYO(IDX1)
                               END-IF
      *
                               MOVE    1    TO   FLG-OK
                           END-IF
                       END-PERFORM
                   END-IF
                   MOVE   "tbl_nyuinacct"   TO  MCP-TABLE
                   MOVE   "key17"           TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
      ***      IF   FLG-OK     =   ZERO
      ***          MOVE     6      TO      LNK-SROUNYUIN-RC
      ***      END-IF
           ELSE
               MOVE     6      TO      LNK-SROUNYUIN-RC
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinacct"   TO  MCP-TABLE
           MOVE     "key17"           TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           IF   LNK-SROUNYUIN-RC    NOT =   ZERO
                   GO   TO    200-MAIN-EXT
           END-IF
      *
      *Ï«ºÒÆþ±¡¤ÎÏ«ºÒ¾è¿ô¡Ê´ü´Ö²Ã»»¡Ë¤Ë´Þ¤á¤ëÃí²Ã»»¤Î¼èÆÀ
           PERFORM    210-CHUKSN-GET-SEC
      *
      *Ï«ºÒÆþ±¡»þ¤Î³°Çñ¼èÆÀ
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID      TO  NACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "2"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
      *            ³°Çñ¶èÊ¬¤ÎÂàÈò
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
      ****             IF    (WRK-NYUINKHN(IDX1) NOT = SPACE) 
                       IF     NACCT-DAY(IDX1)    NOT =  ZERO
                              PERFORM     210-GAIHAKU-SET-SEC
                       END-IF
                   END-PERFORM
                   MOVE   "tbl_nyuinacct"  TO  MCP-TABLE
                   MOVE   "key17"          TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinacct"        TO  MCP-TABLE
           MOVE     "key17"                TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *Ï«ºÒÆþ±¡»þ¤ÎÆþ±¡ÎÁ²Ã»»¼èÆÀ¡ÊÆþ±¡²ñ·×¤è¤ê¡Ë
           MOVE    ZERO                    TO  KSN-IDX
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID      TO  NACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "6"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   MOVE      ZERO     TO      FLG-RYAKU
                   MOVE      ZERO     TO      FLG-KSN
      *            Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF    ((WRK-NYUINKHN(IDX1) NOT = SPACE) OR
                              (WRK-NYUINDAY-KBN(IDX1) =  "1" ))
                       AND   ( LNK-SROUNYUIN-GAIHAKU(IDX1) =
                                   ZERO OR 4 OR 5 OR 6 OR 7   )
                       AND   ( NACCT-DAY(IDX1)    NOT =  ZERO )
      *
      *                  KSN1¡§Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»
      *                  KSN2¡§°å»ÕÇÛÃÖÅ¬¹ç²Ã»»
      *                  KSN3¡§´Ç¸îÇÛÃÖ²Ã»»£±
      *                  KSN4¡§´Ç¸îÇÛÃÖ²Ã»»£²
      *                  KSN5¡§Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±
      *                  KSN6¡§Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²
      *                  KSN10¡§½Å¾É´µ¼Ô²óÉüÉÂÅï²Ã»»
                         IF (NACCT-SRYCDTOTAL(6:9) = CONS-200804-KSN1
                                                  OR CONS-200804-KSN2
                                                  OR CONS-200804-KSN3
                                                  OR CONS-200804-KSN4
                                                  OR CONS-200804-KSN5
                                                  OR CONS-200804-KSN6
                                                  OR CONS-200804-KSN10)
                          IF  (WRK-NYUINKHN(IDX1)      NOT = SPACE)
                          AND (LNK-SROUNYUIN-GAIHAKU(IDX1) =
                                       ZERO OR 4 OR 5 OR 6 OR 7   )
                           EVALUATE   NACCT-SRYCDTOTAL(6:9)
                             WHEN  CONS-200804-KSN1
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN1(IDX1)
                             WHEN  CONS-200804-KSN2
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN2(IDX1)
                             WHEN  CONS-200804-KSN3
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN3(IDX1)
                             WHEN  CONS-200804-KSN4
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN4(IDX1)
                             WHEN  CONS-200804-KSN5
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN5(IDX1)
                             WHEN  CONS-200804-KSN6
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN6(IDX1)
                             WHEN  CONS-200804-KSN10
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN10(IDX1)
                           END-EVALUATE
      *
                           IF   FLG-RYAKU   =   ZERO
                              MOVE   NACCT-SRYCDTOTAL(6:9)
                                            TO  WRK-RYAKU-SRYCD
                              MOVE   NACCT-ZAISKBKBN
                                            TO  WRK-ZAISKBKBN
                              PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                              MOVE    1     TO    FLG-RYAKU
                           END-IF
                          END-IF
                         ELSE
                           IF   FLG-KSN   =  ZERO
                               ADD     1    TO    KSN-IDX
                               MOVE    1    TO    FLG-KSN
                           END-IF
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                        TO  WRK-NYUKSN(KSN-IDX)
                           MOVE   NACCT-ZAITEN  
                                        TO  WRK-NYUKSN-ZAITEN(KSN-IDX)
                           MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-NYUKSN-DAY(KSN-IDX IDX1)
      *
                           IF   FLG-RYAKU   =   ZERO
                              MOVE   NACCT-SRYCDTOTAL(6:9)
                                            TO  WRK-RYAKU-SRYCD
                              MOVE   NACCT-ZAISKBKBN
                                            TO  WRK-ZAISKBKBN
                              PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                              MOVE    1     TO    FLG-RYAKU
                           END-IF
                         END-IF
      *
                       END-IF
                   END-PERFORM
                   MOVE   "tbl_nyuinacct"   TO  MCP-TABLE
                   MOVE   "key17"           TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE     "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *
      *Ï«ºÒÆþ±¡»þ¤ÎÆþ±¡ÎÁ²Ã»»¼èÆÀ¡Ê¿ÇÎÅ²ñ·×¤è¤ê¡Ë
           INITIALIZE                           SRYACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO   ACCT-HOSPNUM
           MOVE    "1"                     TO   ACCT-NYUGAIKBN
           MOVE    LNK-SROUNYUIN-PTID      TO   ACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO   ACCT-SRYYM
           MOVE    LNK-SROUNYUIN-HKNCOMBI  TO   ACCT-HKNCOMBI
           MOVE    "tbl_sryacct"           TO   MCP-TABLE
           MOVE    "key22"                 TO   MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT           =   ZERO
               MOVE     "tbl_sryacct"      TO   MCP-TABLE
               MOVE     "key22"            TO   MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT    =   1)
                 MOVE      ZERO     TO      FLG-RYAKU
                 IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                      (ACCT-ZAIKAISU  NOT =    ZERO)    AND
                      (ACCT-JIHOKBN       =   SPACE)
                   IF   ACCT-SRYKBN  =  "90" OR "92"
      *
                     INITIALIZE                         SRYACT-REC
                     MOVE    ACCT-HOSPNUM       TO      SRY-HOSPNUM
                     MOVE    ACCT-NYUGAIKBN     TO      SRY-NYUGAIKBN
                     MOVE    ACCT-PTID          TO      SRY-PTID
                     MOVE    ACCT-SRYYM         TO      SRY-SRYYM
                     MOVE    ACCT-ZAINUM        TO      SRY-ZAINUM
                     MOVE    SRYACT-REC         TO      MCPDATA-REC
                     MOVE    "DBSELECT"         TO      MCP-FUNC
                     MOVE    "tbl_sryact"       TO      MCP-TABLE
                     MOVE    "key9"             TO      MCP-PATHNAME
                     CALL    "ORCDBMAIN"        USING   MCPAREA
                                                        MCPDATA-REC
                                                        SPA-AREA
                     IF      MCP-RC               =       ZERO
                         MOVE    "tbl_sryact"   TO      MCP-TABLE
                         MOVE    "key9"         TO      MCP-PATHNAME
                         PERFORM  910-SRYACT-READ-SEC
                         IF   FLG-SRYACT    =    0
      *                     KSN7¡§½ÅÅÙÇ§ÃÎ¾É²Ã»»
      *                     KSN8¡§êóáìÉ¾²Á¼Â»Ü²Ã»»¡ÊÎÅÍÜÉÂÅï¡Ë
      *                     KSN9¡§êóáìÉ¾²Á¼Â»Ü²Ã»»¡ÊÍ­¾²¿ÇÎÅ½ê¡Ë
                            IF (SRY-SRYCD(1)  =  CONS-200804-KSN7
                                              OR CONS-200804-KSN8
                                              OR CONS-200804-KSN9 )
      *                                        
                                MOVE  SRY-SRYCD(1)   TO
                                               ACCT-SRYCDTOTAL(6:9)
                            END-IF
                         END-IF
                     END-IF
      *              ¿ÇÎÅ¹Ô°Ù¥¯¥í¡¼¥º
                     MOVE     "tbl_sryact"    TO    MCP-TABLE
                     MOVE     "key9"          TO    MCP-PATHNAME
                     PERFORM   910-DB-CLOSE-SEC
      *
                     IF ( ACCT-SRYCDTOTAL(6:9)  =
                                            CONS-200804-KSN7
                                        OR  CONS-200804-KSN8
                                        OR  CONS-200804-KSN9 )
      *                Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF    (WRK-NYUINKHN(IDX1) NOT = SPACE)
                           AND   (LNK-SROUNYUIN-GAIHAKU(IDX1) =
                                     ZERO OR 4 OR 5 OR 6 OR 7   )
                           AND   (ACCT-DAY(1 IDX1)   NOT =  ZERO)
                              EVALUATE   ACCT-SRYCDTOTAL(6:9)
      *                         ½ÅÅÙÇ§ÃÎ¾É²Ã»»
                                WHEN  CONS-200804-KSN7
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN7(IDX1)
      *                         êóáìÉ¾²Á¼Â»Ü²Ã»»¡ÊÎÅÍÜÉÂÅï¡Ë
                                WHEN  CONS-200804-KSN8
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN8(IDX1)
      *                         êóáìÉ¾²Á¼Â»Ü²Ã»»¡ÊÍ­¾²¿ÇÎÅ½ê¡Ë
                                WHEN  CONS-200804-KSN9
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN9(IDX1)
                              END-EVALUATE
      *
                              IF   FLG-RYAKU   =   ZERO
                                 MOVE   ACCT-SRYCDTOTAL(6:9)
                                               TO  WRK-RYAKU-SRYCD
                                 MOVE   "6"    TO  WRK-ZAISKBKBN
                                 PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                                 MOVE    1     TO    FLG-RYAKU
                              END-IF
      *
                           END-IF
                       END-PERFORM
                     ELSE
                       IF  (ACCT-SRYCDTOTAL(6:9)  =  CONS-ROSAICHIRYO
                                                 OR  CONS-BYOUI
                                                 OR  CONS-TOKUKANSI)
                       OR  (ACCT-ZAIKBN           =    2           )
      *                  Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                        IF  ACCT-ZAIKBN  =  2
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                                    UNTIL     IDX1   >   31
                             IF    ((WRK-NYUINKHN(IDX1) NOT = SPACE) OR
                                    (WRK-NYUINDAY-KBN(IDX1) =  "1" ))
                             AND   ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
                               COMPUTE LNK-SROUNYUIN-GENTEN(IDX1)
                                    =  LNK-SROUNYUIN-GENTEN(IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(1 IDX1))
                             END-IF
                         END-PERFORM
                        ELSE
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                                    UNTIL     IDX1   >   31
      **>                    IF    ((WRK-NYUINKHN(IDX1) NOT = SPACE) OR
      **>                           (WRK-NYUINDAY-KBN(IDX1) =  "1" ))
      **>                    AND   ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
                             IF    ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCTETC
                                                             (IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCTETC
                                                             (IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(1 IDX1))
      *
                               IF   FLG-RYAKU   =   ZERO
                                  MOVE   ACCT-SRYCDTOTAL(6:9)
                                                TO  WRK-RYAKU-SRYCD
                                  MOVE   "6"    TO  WRK-ZAISKBKBN
                                  PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                                  MOVE    1     TO    FLG-RYAKU
                               END-IF
      *
                             END-IF
                         END-PERFORM
                        END-IF
                       ELSE
      *                  Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                                    UNTIL     IDX1   >   31
                             IF    ((WRK-NYUINKHN(IDX1) NOT = SPACE) OR
                                    (WRK-NYUINDAY-KBN(IDX1) =  "1" ))
                             AND   ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCT(IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCT(IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(1 IDX1))
      *
                               IF   FLG-RYAKU   =   ZERO
                                  PERFORM   210-SRYACT-HENSYU-SEC
      *
                                  MOVE    1     TO    FLG-RYAKU
                               END-IF
      *
                             END-IF
                         END-PERFORM
                       END-IF
                     END-IF
                   END-IF
                 END-IF
                 MOVE     "tbl_sryacct"         TO    MCP-TABLE
                 MOVE     "key22"               TO    MCP-PATHNAME
                 PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *    ¿ÇÎÅ²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_sryacct"         TO    MCP-TABLE
           MOVE     "key22"               TO    MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *
      *    ¼èÆÀ¤·¤¿Ï«ºÒÆþ±¡¾ðÊó¤«¤éÆüÃ±°Ì¤ÎÆþ±¡²ñ·×¾ðÊó¤òºÆ¹½ÃÛ
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               MOVE     ZERO                TO  WRK-MEISAISU
      *        Æþ±¡´ðËÜÎÁ
               IF         WRK-NYUINKHN(IDX1)  NOT =        SPACE
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-NYUINKHN(IDX1) TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "1"           TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ³°Çñ
               IF        (WRK-NYUINKHN(IDX1)  NOT =        SPACE)
               AND       (WRK-GAIHAKU(IDX1)   NOT =        SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GAIHAKU(IDX1)  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "2"           TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»
               IF         WRK-200804-KSN1(IDX1)   NOT =   ZERO
                   ADD      1                 TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN1  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"            TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        °å»ÕÇÛÃÖÅ¬¹ç²Ã»»
               IF         WRK-200804-KSN2(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN2     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ´Ç¸îÇÛÃÖ²Ã»»£±
               IF         WRK-200804-KSN3(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN3     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ´Ç¸îÇÛÃÖ²Ã»»£²
               IF         WRK-200804-KSN4(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN4     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±
               IF         WRK-200804-KSN5(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN5     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²
               IF         WRK-200804-KSN6(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN6     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ½ÅÅÙÇ§ÃÎ¾É²Ã»»¡ÊÆÃÄêµ¡Ç½ÉÂ±¡Àº¿ÀÉÂÅï¡Ë
               IF         WRK-200804-KSN7(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN7     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        êóáìÉ¾²Á¼Â»Ü²Ã»»¡ÊÎÅÍÜÉÂÅï¡Ë
               IF         WRK-200804-KSN8(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN8     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        êóáìÉ¾²Á¼Â»Ü²Ã»»¡ÊÍ­¾²¿ÇÎÅ½ê¡Ë
               IF         WRK-200804-KSN9(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN9     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ½Å¾É´µ¼Ô²óÉüÉÂÅï²Ã»»
               IF         WRK-200804-KSN10(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN10    TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *
               IF      (WRK-NYUINKHN(IDX1)   NOT =         SPACE)  AND
                       (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-GENSAN-IRYO(IDX1)   NOT =      SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GENSAN3       TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"          TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                   MOVE     1               TO  WRK-GENSAN3-MIJISI
               END-IF
               IF      (WRK-NYUINKHN(IDX1)   NOT =         SPACE)  AND
                       (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-GENSAN-JYOKU(IDX1)   NOT =     SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GENSAN4       TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"          TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                   MOVE     1               TO  WRK-GENSAN4-MIJISI
               END-IF
      *        Ï«ºÒ¾è¿ô¡ÊÆÃÄêÆþ±¡ÎÁ¤Î¾ì¹ç¤ÏÏ«ºÒ¾è¿ô¤Ê¤·¡Ë
               IF       LNK-SROUNYUIN-SRYKBN(IDX1)    =    "90"
                 IF     FLG-KENPOJYUNKYO  =  1
                   CONTINUE
                 ELSE
                   EVALUATE  LNK-SROUNYUIN-JOUSU-KBN(IDX1)
                       WHEN  "1"
                           ADD      1           TO  WRK-MEISAISU
                           MOVE   "101900010"   TO  LNK-SROUNYUIN-SRYCD
                                               (IDX1 WRK-MEISAISU)
                           MOVE    "4"      TO  LNK-SROUNYUIN-SRYCD-SK
                                               (IDX1 WRK-MEISAISU)
                       WHEN  "2"
                           ADD      1           TO  WRK-MEISAISU
                           MOVE   "101900020"   TO  LNK-SROUNYUIN-SRYCD
                                               (IDX1 WRK-MEISAISU)
                           MOVE    "4"      TO  LNK-SROUNYUIN-SRYCD-SK
                                               (IDX1 WRK-MEISAISU)
                   END-EVALUATE
                 END-IF
               END-IF
      *        ½é´ü²Ã»»¡ÊÄ¹´ü¸º»»¡Ë
               IF      (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-SHOKIKSN(IDX1)   NOT =         SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-SHOKIKSN(IDX1) TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "5"           TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        É¸·ç
               IF       WRK-HYOKETU(IDX1)    NOT =         SPACE
                 IF    (WRK-GAIHAKU(IDX1)        =
                                     SPACE      OR
                                    "190108790" OR  "190108890" OR
                                    "190109090"                 )
                 AND   (WRK-NYUINKHN(IDX1)   NOT =       SPACE  )
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-HYOKETU(IDX1)  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "6"           TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                 ELSE
                   MOVE   SPACE             TO  WRK-HYOKETU(IDX1)
                 END-IF
               END-IF
      *        Æþ±¡ÎÁ²Ã»»¡ÊÆþ±¡²ñ·×¡Ë
      *         PERFORM   VARYING   KSN-IDX   FROM  1  BY  1
      *                   UNTIL    (WRK-NYUKSN(KSN-IDX)  =  SPACE)  OR
      *                            (KSN-IDX             >  20)
      *            IF    WRK-NYUKSN-DAY(KSN-IDX IDX1)  =  ZERO
      *                CONTINUE
      *            ELSE
      *                ADD     1           TO  WRK-MEISAISU
      *                MOVE WRK-NYUKSN(KSN-IDX)
      *                                    TO  LNK-SROUNYUIN-SRYCD
      *                                   (IDX1 WRK-MEISAISU)
      *            END-IF
      *         END-PERFORM
           END-PERFORM
      *
      *    ºÆ¹½ÃÛ¤·¤¿Ï«ºÒÆþ±¡²ñ·×¾ðÊó¤«¤éÅÀ¿ô¤Î»»Äê
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
      *
               MOVE     ZERO       TO       WRK-HYOKETU-TEN
                                            WRK-ZAITEN-HYOKETU
               IF   WRK-HYOKETU(IDX1)   =   SPACE
      *          ÅÀ¿ô¥Þ¥¹¥¿¤ÎÆÉ¤ß¹þ¤ß
                 PERFORM   VARYING   IDX2    FROM   1  BY   1
                      UNTIL (LNK-SROUNYUIN-SRYCD(IDX1 IDX2) = SPACE)
                         OR (IDX2    >      20)
                     PERFORM    210-TENSU-KEISAN-SEC
                 END-PERFORM
               ELSE
      *          ÅÀ¿ô¥Þ¥¹¥¿¤ÎÆÉ¤ß¹þ¤ß
                 INITIALIZE                          TNS-TENSU-REC
                 MOVE    WRK-HYOKETU(IDX1)       TO  TNS-SRYCD
                 MOVE    LNK-SROUNYUIN-HOSPNUM   TO  TNS-HOSPNUM
                 MOVE    LNK-SROUNYUIN-SANTEIYM  TO  TNS-YUKOSTYMD(1:6)
                 MOVE    "01"                    TO  TNS-YUKOSTYMD(7:2)
                 MOVE    LNK-SROUNYUIN-SANTEIYM  TO  TNS-YUKOEDYMD(1:6)
                 MOVE    "01"                    TO  TNS-YUKOEDYMD(7:2)
                 PERFORM 910-TENSU-READ-SEC
                 MOVE    TNS-TEN                 TO  WRK-HYOKETU-TEN
      *
                 PERFORM   VARYING   IDX2    FROM   1  BY   1
                      UNTIL (LNK-SROUNYUIN-SRYCD(IDX1 IDX2) = SPACE)
                         OR (IDX2    >      20)
                     PERFORM    210-TENSU-KEISAN-HYOKETU-SEC
                 END-PERFORM
               END-IF
      *
               PERFORM   VARYING   KSN-IDX   FROM   1  BY   1
                         UNTIL (WRK-NYUKSN(KSN-IDX)  =  SPACE)  OR
                               (KSN-IDX    >      20)
                  IF    WRK-NYUKSN-DAY(KSN-IDX IDX1)  =  ZERO
                      CONTINUE
                  ELSE
                      ADD  WRK-NYUKSN-ZAITEN(KSN-IDX)
                                 TO  LNK-SROUNYUIN-ZAITEN-NACCT(IDX1)
                  END-IF
               END-PERFORM
      *
           END-PERFORM
      *
      *
           .
       210-ROUNYUIN-SET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ï«ºÒÆþ±¡¥µ¥Ö¤ÎÊÖµÑÃÍ¥»¥Ã¥È¡ÊÊ¿À®£²£²Ç¯£´·î°Ê¹ß¡Ë
      *****************************************************************
       210-ROUNYUIN-SET3-SEC            SECTION.
      *
      *Ï«ºÒ¾è¿ô¤Î£²½µ´Ö°ÊÆâ¤ÎºÇ½ªÆü¤ò¼èÆÀ
           INITIALIZE                      ORCSRSITUSANAREA
           MOVE    LNK-SROUNYUIN-HOSPNUM
                                       TO  RSITUSAN-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID
                                       TO  RSITUSAN-PTID
           MOVE    LNK-SROUNYUIN-HKNCOMBI
                                       TO  RSITUSAN-HKNCOMBI
           MOVE    LNK-SROUNYUIN-SANTEIYM
                                       TO  RSITUSAN-SRYYM
           CALL    "ORCSRSITUSAN"      USING
                                           ORCSRSITUSANAREA
                                           SPA-AREA
           IF    ( RSITUSAN-RC          =   ZERO )
               CONTINUE
           ELSE
               INITIALIZE  ORCSRSITUSANAREA
           END-IF
           PERFORM   VARYING   IDX1   FROM 1  BY  1
                     UNTIL     IDX1   >    31
               MOVE  RSITUSAN-DAY(IDX1) TO LNK-SROUNYUIN-JOUSU-KBN(IDX1)
           END-PERFORM
      *
      *Ï«ºÒÆþ±¡ÎÁ¤Î¼èÆÀ
           MOVE    ZERO                    TO  FLG-OK
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID      TO  NACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "1"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   MOVE      ZERO     TO      FLG-RYAKU
                                              WRK-KSNKBN
                   MOVE      SPACE    TO      WRK-RYAKUSHO
                                              WRK-SRYKBN
                                              WRK-SHOKIKSN-TAIHI
                                              WRK-HYOKETU-TAIHI
                   MOVE      ZERO     TO      WRK-KHNTEN
                                              WRK-KSNTEN
                                              WRK-HYOTEN
                   IF    NACCT-SRYCDTOTAL     =    99999916
                       CONTINUE
                   ELSE
      *                ³ºÅöÆþ±¡¹Ô°Ù¥Æ¡¼¥Ö¥ë¤ÎÆÉ¤ß¹þ¤ß
                       PERFORM    210-NYUINACT-GET-SEC
      *                Æþ±¡´ðËÜÎÁ¤È½é´ü²Ã»»¡ÊÄ¹´ü¸º»»¡Ë¤ÎÂàÈò
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF    (LNK-SROUNYUIN-NYUIN-FLG(IDX1) = "1")
                           AND   (NACCT-DAY(IDX1)    NOT =  ZERO) 
                               MOVE    NSRY-SRYCD(1)   TO  
                                                   WRK-NYUINKHN(IDX1)
      *
                               IF   FLG-RYAKU   =   ZERO
      *
                                  IF   NSRY-SRYCD(1)  NOT =   SPACE
                                      INITIALIZE    TNS-TENSU-REC
                                      MOVE NSRY-SRYCD(1)
                                                TO  TNS-SRYCD
                                      MOVE LNK-SROUNYUIN-HOSPNUM
                                                TO  TNS-HOSPNUM
                                      MOVE LNK-SROUNYUIN-SANTEIYM
                                                TO  TNS-YUKOSTYMD(1:6)
                                      MOVE "01" TO  TNS-YUKOSTYMD(7:2)
                                      MOVE LNK-SROUNYUIN-SANTEIYM
                                                TO  TNS-YUKOEDYMD(1:6)
                                      MOVE "01" TO  TNS-YUKOEDYMD(7:2)
                                      PERFORM 910-TENSU-READ-SEC
                                      IF  FLG-TENSU  NOT = ZERO
                                        MOVE  1 TO  WRK-NYUINRYO-ERR
                                      ELSE
                                        MOVE TNS-TEN
                                                TO  WRK-KHNTEN
                                      END-IF
                                  END-IF
      *
                                  PERFORM  VARYING  IDX3  FROM  2  BY  1
                                            UNTIL   IDX3   >    3
                                    IF   NSRY-SRYCD(IDX3)  NOT =   SPACE
                                      INITIALIZE    TNS-TENSU-REC
                                      MOVE NSRY-SRYCD(IDX3)
                                                TO  TNS-SRYCD
                                      MOVE LNK-SROUNYUIN-HOSPNUM
                                                TO  TNS-HOSPNUM
                                      MOVE LNK-SROUNYUIN-SANTEIYM
                                                TO  TNS-YUKOSTYMD(1:6)
                                      MOVE "01" TO  TNS-YUKOSTYMD(7:2)
                                      MOVE LNK-SROUNYUIN-SANTEIYM
                                                TO  TNS-YUKOEDYMD(1:6)
                                      MOVE "01" TO  TNS-YUKOEDYMD(7:2)
                                      PERFORM 910-TENSU-READ-SEC
                                      EVALUATE  TNS-TENSIKIBETU
                                         WHEN     3
                                             MOVE   NSRY-SRYCD(IDX3)  TO
                                                      WRK-SHOKIKSN-TAIHI
                                             MOVE     TNS-TEN         TO
                                                      WRK-KSNTEN
                                         WHEN     8
                                             MOVE   NSRY-SRYCD(IDX3)  TO
                                                      WRK-SHOKIKSN-TAIHI
                                             COMPUTE  WRK-KSNTEN =
                                                      TNS-TEN    *  -1
                                         WHEN     6
                                             MOVE   NSRY-SRYCD(IDX3)  TO
                                                      WRK-HYOKETU-TAIHI
                                             MOVE     TNS-TEN         TO
                                                      WRK-HYOTEN
                                      END-EVALUATE
                                    END-IF
                                  END-PERFORM
      *
                                  MOVE    NACCT-SRYKBN
                                                TO  WRK-SRYKBN
      *                           ³°Çñ¥³¡¼¥ÉÊÔ½¸ÍÑ¤ËÆþ±¡´ðËÜÎÁ²Ã»»¶èÊ¬
      *                           ¤ÎÂàÈò
                                  PERFORM   210-GAIHAKU-MAE-SEC
      *
                                  MOVE   NSRY-SRYCD(1)
                                                TO  WRK-RYAKU-SRYCD
                                  MOVE   NACCT-ZAISKBKBN
                                                TO  WRK-ZAISKBKBN
                                  PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                                  MOVE    1     TO    FLG-RYAKU
      *
      *                           Æþ±¡ÎÁºÞÅÀ¥Á¥§¥Ã¥¯
                                  COMPUTE  WRK-HIKAKU-ZAITEN =
                                           WRK-KHNTEN + WRK-KSNTEN
                                  IF  WRK-HYOTEN  NOT =  ZERO
                                    COMPUTE WRK-HIKAKU-ZAITEN =
                                          ( WRK-HIKAKU-ZAITEN -
                                           (WRK-HIKAKU-ZAITEN *
                                            (WRK-HYOTEN / 100)) )
                                                              + 0.5
                                  END-IF
                                  IF  NACCT-ZAITEN  NOT =  ZERO
                                    IF  NACCT-ZAITEN  =
                                                    WRK-HIKAKU-ZAITEN
                                      CONTINUE
                                    ELSE
                                      MOVE   1  TO  WRK-NYUINRYO-ERR2
                                    END-IF
                                  END-IF
      *
                               END-IF
      *
                               IF    WRK-SHOKIKSN-TAIHI  NOT = SPACE
                                  MOVE  WRK-SHOKIKSN-TAIHI
                                               TO  WRK-SHOKIKSN(IDX1)
                               END-IF
                               IF    WRK-HYOKETU-TAIHI   NOT = SPACE
                                  MOVE  WRK-HYOKETU-TAIHI
                                               TO  WRK-HYOKETU(IDX1)
                               END-IF
      *
                               MOVE  WRK-SRYKBN
                                         TO  LNK-SROUNYUIN-SRYKBN(IDX1)
                               MOVE  WRK-KSNKBN
                                         TO  WRK-NYUKHNKSNKBN(IDX1)
      *
                               IF    WRK-RYAKUSHO   NOT =  SPACE
                                  MOVE  WRK-RYAKUSHO
                                      TO  LNK-SROUNYUIN-RYAKUSYO(IDX1)
                               END-IF
      *
                               MOVE    1    TO   FLG-OK
                           END-IF
                       END-PERFORM
                   END-IF
                   MOVE   "tbl_nyuinacct"   TO  MCP-TABLE
                   MOVE   "key17"           TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
      ***      IF   FLG-OK     =   ZERO
      ***          MOVE     6      TO      LNK-SROUNYUIN-RC
      ***      END-IF
           ELSE
               MOVE     6      TO      LNK-SROUNYUIN-RC
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinacct"   TO  MCP-TABLE
           MOVE     "key17"           TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           IF   LNK-SROUNYUIN-RC    NOT =   ZERO
                   GO   TO    200-MAIN-EXT
           END-IF
      *
      *Ï«ºÒÆþ±¡¤ÎÏ«ºÒ¾è¿ô¡Ê´ü´Ö²Ã»»¡Ë¤Ë´Þ¤á¤ëÃí²Ã»»¤Î¼èÆÀ
           PERFORM    210-CHUKSN-GET-SEC
      *
      *Ï«ºÒÆþ±¡»þ¤Î³°Çñ¼èÆÀ
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID      TO  NACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "2"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
      *            ³°Çñ¶èÊ¬¤ÎÂàÈò
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
      ****             IF    (WRK-NYUINKHN(IDX1) NOT = SPACE) 
                       IF     NACCT-DAY(IDX1)    NOT =  ZERO
                              PERFORM     210-GAIHAKU-SET-SEC
                       END-IF
                   END-PERFORM
                   MOVE   "tbl_nyuinacct"  TO  MCP-TABLE
                   MOVE   "key17"          TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinacct"        TO  MCP-TABLE
           MOVE     "key17"                TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *Ï«ºÒÆþ±¡»þ¤ÎÆþ±¡ÎÁ²Ã»»¼èÆÀ¡ÊÆþ±¡²ñ·×¤è¤ê¡Ë
           MOVE    ZERO                    TO  KSN-IDX
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID      TO  NACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "6"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   MOVE      ZERO     TO      FLG-RYAKU
                   MOVE      ZERO     TO      FLG-KSN
      *            Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF    ((WRK-NYUINKHN(IDX1) NOT = SPACE) OR
                              (WRK-NYUINDAY-KBN(IDX1) =  "1" ))
                       AND   ( LNK-SROUNYUIN-GAIHAKU(IDX1) =
                                   ZERO OR 4 OR 5 OR 6 OR 7   )
                       AND   ( NACCT-DAY(IDX1)    NOT =  ZERO )
      *
      *                  KSN1¡§Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»
      *                  KSN12:°å»ÕÇÛÃÖ²Ã»»£±
      *                  KSN2¡§°å»ÕÇÛÃÖ²Ã»»£²
      *                  KSN4¡§´Ç¸îÇÛÃÖ²Ã»»£±
      *                  KSN3¡§´Ç¸îÇÛÃÖ²Ã»»£²
      *                  KSN6¡§Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±
      *                  KSN5¡§Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²
      *                  KSN13:°ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»
      *                  KSN16:°ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»¡ÊÆÃÄêµ¡Ç½ÉÂ±¡¡Ë
      *                  KSN17:°ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»¡ÊÀìÌçÉÂ±¡¡Ë
      *                  KSN14:µßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»
      *                  KSN18:Í­¾²¿ÇÎÅ½ê°ìÈÌÉÂ¾²½é´ü²Ã»»
      *                  KSN19:µßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»
                         IF (NACCT-SRYCDTOTAL(6:9) = CONS-200804-KSN1
                                                  OR CONS-201004-KSN12
                                                  OR CONS-200804-KSN2
                                                  OR CONS-200804-KSN4
                                                  OR CONS-200804-KSN3
                                                  OR CONS-200804-KSN6
                                                  OR CONS-200804-KSN5
                                                  OR CONS-201004-KSN13
                                                  OR CONS-201004-KSN16
                                                  OR CONS-201004-KSN17)
      ***                                         OR CONS-201004-KSN14
      ***                                         OR CONS-201004-KSN18
      ***                                         OR CONS-201004-KSN19)
                          IF  (WRK-NYUINKHN(IDX1)      NOT = SPACE)
                          AND (LNK-SROUNYUIN-GAIHAKU(IDX1) = 
                                       ZERO OR 4 OR 5 OR 6 OR 7   )
                           EVALUATE   NACCT-SRYCDTOTAL(6:9)
                             WHEN  CONS-200804-KSN1
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN1(IDX1)
                             WHEN  CONS-201004-KSN12
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201004-KSN12(IDX1)
                             WHEN  CONS-200804-KSN2
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN2(IDX1)
                             WHEN  CONS-200804-KSN4
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN4(IDX1)
                             WHEN  CONS-200804-KSN3
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN3(IDX1)
                             WHEN  CONS-200804-KSN6
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN6(IDX1)
                             WHEN  CONS-200804-KSN5
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN5(IDX1)
                             WHEN  CONS-201004-KSN13
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201004-KSN13(IDX1)
                             WHEN  CONS-201004-KSN16
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201004-KSN16(IDX1)
                             WHEN  CONS-201004-KSN17
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201004-KSN17(IDX1)
      *                      µßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»
      ***                    WHEN  CONS-201004-KSN14
      ***                         MOVE   NACCT-DAY(IDX1)
      ***                               TO  WRK-201004-KSN14(IDX1)
      *                      Í­¾²¿ÇÎÅ½ê°ìÈÌÉÂ¾²½é´ü²Ã»»
      ***                    WHEN  CONS-201004-KSN18
      ***                         MOVE   NACCT-DAY(IDX1)
      ***                               TO  WRK-201004-KSN18(IDX1)
      *                      µßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»
      ***                    WHEN  CONS-201004-KSN19
      ***                         MOVE   NACCT-DAY(IDX1)
      ***                               TO  WRK-201004-KSN19(IDX1)
                           END-EVALUATE
      *
                           IF   FLG-RYAKU   =   ZERO
                              MOVE   NACCT-SRYCDTOTAL(6:9)
                                            TO  WRK-RYAKU-SRYCD
                              MOVE   NACCT-ZAISKBKBN
                                            TO  WRK-ZAISKBKBN
                              PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                              MOVE    1     TO    FLG-RYAKU
                           END-IF
                          END-IF
                         ELSE
                           IF   FLG-KSN   =  ZERO
                               ADD     1    TO    KSN-IDX
                               MOVE    1    TO    FLG-KSN
                           END-IF
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                        TO  WRK-NYUKSN(KSN-IDX)
                           MOVE   NACCT-ZAITEN  
                                        TO  WRK-NYUKSN-ZAITEN(KSN-IDX)
                           MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-NYUKSN-DAY(KSN-IDX IDX1)
      *
                           IF   FLG-RYAKU   =   ZERO
                              MOVE   NACCT-SRYCDTOTAL(6:9)
                                            TO  WRK-RYAKU-SRYCD
                              MOVE   NACCT-ZAISKBKBN
                                            TO  WRK-ZAISKBKBN
                              PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                              MOVE    1     TO    FLG-RYAKU
                           END-IF
                         END-IF
      *
                       END-IF
                   END-PERFORM
                   MOVE   "tbl_nyuinacct"   TO  MCP-TABLE
                   MOVE   "key17"           TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE     "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *
      *Ï«ºÒÆþ±¡»þ¤ÎÆþ±¡ÎÁ²Ã»»¼èÆÀ¡Ê¿ÇÎÅ²ñ·×¤è¤ê¡Ë
           INITIALIZE                           SRYACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO   ACCT-HOSPNUM
           MOVE    "1"                     TO   ACCT-NYUGAIKBN
           MOVE    LNK-SROUNYUIN-PTID      TO   ACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO   ACCT-SRYYM
           MOVE    LNK-SROUNYUIN-HKNCOMBI  TO   ACCT-HKNCOMBI
           MOVE    "tbl_sryacct"           TO   MCP-TABLE
           MOVE    "key22"                 TO   MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT           =   ZERO
               MOVE     "tbl_sryacct"      TO   MCP-TABLE
               MOVE     "key22"            TO   MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT    =   1)
                 MOVE      ZERO     TO      FLG-RYAKU
                 IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                      (ACCT-ZAIKAISU  NOT =    ZERO)    AND
                      (ACCT-JIHOKBN       =   SPACE)
                   IF   ACCT-SRYKBN  =  "90" OR "92"
      *
                     INITIALIZE                         SRYACT-REC
                     MOVE    ACCT-HOSPNUM       TO      SRY-HOSPNUM
                     MOVE    ACCT-NYUGAIKBN     TO      SRY-NYUGAIKBN
                     MOVE    ACCT-PTID          TO      SRY-PTID
                     MOVE    ACCT-SRYYM         TO      SRY-SRYYM
                     MOVE    ACCT-ZAINUM        TO      SRY-ZAINUM
                     MOVE    SRYACT-REC         TO      MCPDATA-REC
                     MOVE    "DBSELECT"         TO      MCP-FUNC
                     MOVE    "tbl_sryact"       TO      MCP-TABLE
                     MOVE    "key9"             TO      MCP-PATHNAME
                     CALL    "ORCDBMAIN"        USING   MCPAREA
                                                        MCPDATA-REC
                                                        SPA-AREA
                     IF      MCP-RC               =       ZERO
                         MOVE    "tbl_sryact"   TO      MCP-TABLE
                         MOVE    "key9"         TO      MCP-PATHNAME
                         PERFORM  910-SRYACT-READ-SEC
                         IF   FLG-SRYACT    =    0
      *                     KSN7¡§½ÅÅÙÇ§ÃÎ¾É²Ã»»
      *                     KSN8¡§êóáìÉ¾²Á¼Â»Ü²Ã»»¡ÊÎÅÍÜÉÂÅï¡Ë
      *                     KSN9¡§êóáìÉ¾²Á¼Â»Ü²Ã»»¡ÊÍ­¾²¿ÇÎÅ½ê¡Ë
      *                     KSN11:½Å¾É»ù¡Ê¼Ô¡Ë¼õÆþÏ¢·È²Ã»»¡Ê¾ã³²¼Ô»ÜÀßÅùÆþ±¡´ðËÜÎÁ¡Ë
      *                     KSN15:½ÅÅÙÇ§ÃÎ¾É²Ã»»¡ÊÀº¿ÀÉÂÅïÆþ±¡´ðËÜÎÁ¡Ë
      *                     ¡Ý¡Ý°Ê²¼¡¢Æþ±¡²ñ·×¤È¿ÇÎÅ²ñ·×¤«¤é¼èÆÀ¤¹¤ë²Ã»»¡Ý¡Ý
      *                     KSN1¡§Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»
      *                     KSN12:°å»ÕÇÛÃÖ²Ã»»£±
      *                     KSN2¡§°å»ÕÇÛÃÖ²Ã»»£²
      *                     KSN4¡§´Ç¸îÇÛÃÖ²Ã»»£±
      *                     KSN3¡§´Ç¸îÇÛÃÖ²Ã»»£²
      *                     KSN6¡§Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±
      *                     KSN5¡§Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²
      *                     KSN13:°ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»
      *                     KSN16:°ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»¡ÊÆÃÄêµ¡Ç½ÉÂ±¡¡Ë
      *                     KSN17:°ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»¡ÊÀìÌçÉÂ±¡¡Ë
      *                     KSN14:µßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»
      *                     KSN18:Í­¾²¿ÇÎÅ½ê°ìÈÌÉÂ¾²½é´ü²Ã»»
      *                     KSN19:µßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»
                            IF (SRY-SRYCD(1)  =  CONS-200804-KSN7
                                              OR CONS-200804-KSN8
                                              OR CONS-200804-KSN9
                                              OR CONS-201004-KSN11
                                              OR CONS-201004-KSN13
                                              OR CONS-201004-KSN15
                                              OR CONS-201004-KSN16
                                              OR CONS-201004-KSN17
                                              OR CONS-200804-KSN1
                                              OR CONS-201004-KSN12
                                              OR CONS-200804-KSN2
                                              OR CONS-200804-KSN4
                                              OR CONS-200804-KSN3
                                              OR CONS-200804-KSN6
                                              OR CONS-200804-KSN5 )
      ***                                     OR CONS-201004-KSN14
      ***                                     OR CONS-201004-KSN18
      ***                                     OR CONS-201004-KSN19)
      *                                        
                                MOVE  SRY-SRYCD(1)   TO
                                               ACCT-SRYCDTOTAL(6:9)
                            END-IF
                         END-IF
                     END-IF
      *              ¿ÇÎÅ¹Ô°Ù¥¯¥í¡¼¥º
                     MOVE     "tbl_sryact"    TO    MCP-TABLE
                     MOVE     "key9"          TO    MCP-PATHNAME
                     PERFORM   910-DB-CLOSE-SEC
      *
                     IF ( ACCT-SRYCDTOTAL(6:9)  =
                                            CONS-200804-KSN7
                                        OR  CONS-200804-KSN8
                                        OR  CONS-200804-KSN9
                                        OR  CONS-201004-KSN11
                                        OR  CONS-201004-KSN13
                                        OR  CONS-201004-KSN15
                                        OR  CONS-201004-KSN16
                                        OR  CONS-201004-KSN17
                                        OR  CONS-200804-KSN1
                                        OR  CONS-201004-KSN12
                                        OR  CONS-200804-KSN2
                                        OR  CONS-200804-KSN4
                                        OR  CONS-200804-KSN3
                                        OR  CONS-200804-KSN6
                                        OR  CONS-200804-KSN5  )
      ***                               OR  CONS-201004-KSN14
      ***                               OR  CONS-201004-KSN18
      ***                               OR  CONS-201004-KSN19 )
      *                Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF    (WRK-NYUINKHN(IDX1) NOT = SPACE)
                           AND   (LNK-SROUNYUIN-GAIHAKU(IDX1) =
                                     ZERO OR 4 OR 5 OR 6 OR 7   )
                           AND   (ACCT-DAY(1 IDX1)   NOT =  ZERO)
                              EVALUATE   ACCT-SRYCDTOTAL(6:9)
      *                         ½ÅÅÙÇ§ÃÎ¾É²Ã»»
                                WHEN  CONS-200804-KSN7
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN7(IDX1)
      *                         êóáìÉ¾²Á¼Â»Ü²Ã»»¡ÊÎÅÍÜÉÂÅï¡Ë
                                WHEN  CONS-200804-KSN8
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN8(IDX1)
      *                         êóáìÉ¾²Á¼Â»Ü²Ã»»¡ÊÍ­¾²¿ÇÎÅ½ê¡Ë
                                WHEN  CONS-200804-KSN9
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN9(IDX1)
      *                         ½Å¾É»ù¡Ê¼Ô¡Ë¼õÆþÏ¢·È²Ã»»¡Ê¾ã³²¼Ô»ÜÀßÅùÆþ±¡´ðËÜÎÁ¡Ë
                                WHEN  CONS-201004-KSN11
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201004-KSN11(IDX1)
      *                         °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»
                                WHEN  CONS-201004-KSN13
                                  IF  ACCT-DAY(1 IDX1)   NOT = ZERO
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201004-KSN13(IDX1)
                                  END-IF
      *                         ½ÅÅÙÇ§ÃÎ¾É²Ã»»¡ÊÀº¿ÀÉÂÅïÆþ±¡´ðËÜÎÁ¡Ë
                                WHEN  CONS-201004-KSN15
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201004-KSN15(IDX1)
      *                         °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»¡ÊÆÃÄêµ¡Ç½ÉÂ±¡¡Ë
                                WHEN  CONS-201004-KSN16
                                  IF  ACCT-DAY(1 IDX1)   NOT = ZERO
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201004-KSN16(IDX1)
                                  END-IF
      *                         °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»¡ÊÀìÌçÉÂ±¡¡Ë
                                WHEN  CONS-201004-KSN17
                                  IF  ACCT-DAY(1 IDX1)   NOT = ZERO
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201004-KSN17(IDX1)
                                  END-IF
      *                         Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»
                                WHEN  CONS-200804-KSN1
                                  IF  ACCT-DAY(1 IDX1)   NOT = ZERO
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN1(IDX1)
                                  END-IF
      *                         °å»ÕÇÛÃÖ²Ã»»£±
                                WHEN  CONS-201004-KSN12
                                  IF  ACCT-DAY(1 IDX1)   NOT = ZERO
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201004-KSN12(IDX1)
                                  END-IF
      *                         °å»ÕÇÛÃÖ²Ã»»£²
                                WHEN  CONS-200804-KSN2
                                  IF  ACCT-DAY(1 IDX1)   NOT = ZERO
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN2(IDX1)
                                  END-IF
      *                         ´Ç¸îÇÛÃÖ²Ã»»£±
                                WHEN  CONS-200804-KSN4
                                  IF  ACCT-DAY(1 IDX1)   NOT = ZERO
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN4(IDX1)
                                  END-IF
      *                         ´Ç¸îÇÛÃÖ²Ã»»£²
                                WHEN  CONS-200804-KSN3
                                  IF  ACCT-DAY(1 IDX1)   NOT = ZERO
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN3(IDX1)
                                  END-IF
      *                         Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±
                                WHEN  CONS-200804-KSN6
                                  IF  ACCT-DAY(1 IDX1)   NOT = ZERO
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN6(IDX1)
                                  END-IF
      *                         Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²
                                WHEN  CONS-200804-KSN5
                                  IF  ACCT-DAY(1 IDX1)   NOT = ZERO
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN5(IDX1)
                                  END-IF
      *                         µßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»
      ***                       WHEN  CONS-201004-KSN14
      ***                         IF  ACCT-DAY(1 IDX1)   NOT = ZERO
      ***                             MOVE   ACCT-DAY(1 IDX1)
      ***                               TO  WRK-201004-KSN14(IDX1)
      ***                         END-IF
      *                         Í­¾²¿ÇÎÅ½ê°ìÈÌÉÂ¾²½é´ü²Ã»»
      ***                       WHEN  CONS-201004-KSN18
      ***                         IF  ACCT-DAY(1 IDX1)   NOT = ZERO
      ***                             MOVE   ACCT-DAY(1 IDX1)
      ***                               TO  WRK-201004-KSN18(IDX1)
      ***                         END-IF
      *                         µßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»
      ***                       WHEN  CONS-201004-KSN19
      ***                         IF  ACCT-DAY(1 IDX1)   NOT = ZERO
      ***                             MOVE   ACCT-DAY(1 IDX1)
      ***                               TO  WRK-201004-KSN19(IDX1)
      ***                         END-IF
                              END-EVALUATE
      *
                              IF   FLG-RYAKU   =   ZERO
                                 MOVE   ACCT-SRYCDTOTAL(6:9)
                                               TO  WRK-RYAKU-SRYCD
                                 MOVE   "6"    TO  WRK-ZAISKBKBN
                                 PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                                 MOVE    1     TO    FLG-RYAKU
                              END-IF
      *
                           END-IF
                       END-PERFORM
                     ELSE
                       IF  (ACCT-SRYCDTOTAL(6:9)  =  CONS-ROSAICHIRYO
                                                 OR  CONS-BYOUI
                                                 OR  CONS-TOKUKANSI)
                       OR  (ACCT-ZAIKBN           =    2           )
      *                  Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                        IF  ACCT-ZAIKBN  =  2
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                                    UNTIL     IDX1   >   31
                             IF    ((WRK-NYUINKHN(IDX1) NOT = SPACE) OR
                                    (WRK-NYUINDAY-KBN(IDX1) =  "1" ))
                             AND   ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
                               COMPUTE LNK-SROUNYUIN-GENTEN(IDX1)
                                    =  LNK-SROUNYUIN-GENTEN(IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(1 IDX1))
                             END-IF
                         END-PERFORM
                        ELSE
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                                    UNTIL     IDX1   >   31
      **>                    IF    ((WRK-NYUINKHN(IDX1) NOT = SPACE) OR
      **>                           (WRK-NYUINDAY-KBN(IDX1) =  "1" ))
      **>                    AND   ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
                             IF    ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCTETC
                                                             (IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCTETC
                                                             (IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(1 IDX1))
      *
                               IF   FLG-RYAKU   =   ZERO
                                  MOVE   ACCT-SRYCDTOTAL(6:9)
                                                TO  WRK-RYAKU-SRYCD
                                  MOVE   "6"    TO  WRK-ZAISKBKBN
                                  PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                                  MOVE    1     TO    FLG-RYAKU
                               END-IF
      *
                             END-IF
                         END-PERFORM
                        END-IF
                       ELSE
      *                  Æþ±¡ÎÁ²Ã»»¤ÎÂàÈò
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                                    UNTIL     IDX1   >   31
                             IF    ((WRK-NYUINKHN(IDX1) NOT = SPACE) OR
                                    (WRK-NYUINDAY-KBN(IDX1) =  "1" ))
                             AND   ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCT(IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCT(IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(1 IDX1))
      *
                               IF   FLG-RYAKU   =   ZERO
                                  PERFORM   210-SRYACT-HENSYU-SEC
      *
                                  MOVE    1     TO    FLG-RYAKU
                               END-IF
      *
                             END-IF
                         END-PERFORM
                       END-IF
                     END-IF
                   END-IF
                 END-IF
                 MOVE     "tbl_sryacct"         TO    MCP-TABLE
                 MOVE     "key22"               TO    MCP-PATHNAME
                 PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *    ¿ÇÎÅ²ñ·×¥¯¥í¡¼¥º
           MOVE     "tbl_sryacct"         TO    MCP-TABLE
           MOVE     "key22"               TO    MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *
      *    ¼èÆÀ¤·¤¿Ï«ºÒÆþ±¡¾ðÊó¤«¤éÆüÃ±°Ì¤ÎÆþ±¡²ñ·×¾ðÊó¤òºÆ¹½ÃÛ
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               MOVE     ZERO                TO  WRK-MEISAISU
      *        Æþ±¡´ðËÜÎÁ
               IF         WRK-NYUINKHN(IDX1)  NOT =        SPACE
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-NYUINKHN(IDX1) TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "1"           TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ³°Çñ
               IF        (WRK-NYUINKHN(IDX1)  NOT =        SPACE)
               AND       (WRK-GAIHAKU(IDX1)   NOT =        SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GAIHAKU(IDX1)  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "2"           TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        Ìë´Ö¶ÛµÞÂÎÀ©³ÎÊÝ²Ã»»
               IF         WRK-200804-KSN1(IDX1)   NOT =   ZERO
                   ADD      1                 TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN1  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"            TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        °å»ÕÇÛÃÖ²Ã»»£±
               IF         WRK-201004-KSN12(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201004-KSN12    TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        °å»ÕÇÛÃÖ²Ã»»£²
               IF         WRK-200804-KSN2(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN2     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ´Ç¸îÇÛÃÖ²Ã»»£±
               IF         WRK-200804-KSN4(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN4     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ´Ç¸îÇÛÃÖ²Ã»»£²
               IF         WRK-200804-KSN3(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN3     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£±
               IF         WRK-200804-KSN6(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN6     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        Ìë´Ö´Ç¸îÇÛÃÖ²Ã»»£²
               IF         WRK-200804-KSN5(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN5     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ½ÅÅÙÇ§ÃÎ¾É²Ã»»¡ÊÆÃÄêµ¡Ç½ÉÂ±¡Àº¿ÀÉÂÅï¡Ë
               IF         WRK-200804-KSN7(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN7     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        êóáìÉ¾²Á¼Â»Ü²Ã»»¡ÊÎÅÍÜÉÂÅï¡Ë
               IF         WRK-200804-KSN8(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN8     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        êóáìÉ¾²Á¼Â»Ü²Ã»»¡ÊÍ­¾²¿ÇÎÅ½ê¡Ë
               IF         WRK-200804-KSN9(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN9     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ½Å¾É»ù¡Ê¼Ô¡Ë¼õÆþÏ¢·È²Ã»»
               IF         WRK-201004-KSN11(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201004-KSN11    TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»
               IF         WRK-201004-KSN13(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201004-KSN13    TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        µßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»
      ***      IF         WRK-201004-KSN14(IDX1)      NOT =   ZERO
      ***          ADD      1                  TO  WRK-MEISAISU
      ***          MOVE   CONS-201004-KSN14    TO  LNK-SROUNYUIN-SRYCD
      ***                                  (IDX1 WRK-MEISAISU)
      ***          MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
      ***                                  (IDX1 WRK-MEISAISU)
      ***      END-IF
      *        ½ÅÅÙÇ§ÃÎ¾É²Ã»»¡ÊÀº¿ÀÉÂÅïÆþ±¡´ðËÜÎÁ¡Ë
               IF         WRK-201004-KSN15(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201004-KSN15    TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»¡ÊÆÃÄêµ¡Ç½ÉÂ±¡¡Ë
               IF         WRK-201004-KSN16(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201004-KSN16    TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        °ìÈÌÉÂÅï´Ç¸îÉ¬Í×ÅÙÉ¾²Á²Ã»»¡ÊÀìÌçÉÂ±¡¡Ë
               IF         WRK-201004-KSN17(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201004-KSN17    TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        Í­¾²¿ÇÎÅ½ê°ìÈÌÉÂ¾²½é´ü²Ã»»
      ***      IF         WRK-201004-KSN18(IDX1)      NOT =   ZERO
      ***          ADD      1                  TO  WRK-MEISAISU
      ***          MOVE   CONS-201004-KSN18    TO  LNK-SROUNYUIN-SRYCD
      ***                                  (IDX1 WRK-MEISAISU)
      ***          MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
      ***                                  (IDX1 WRK-MEISAISU)
      ***      END-IF
      *        µßµÞ¡¦ºßÂðÅù»Ù±çÎÅÍÜÉÂ¾²½é´ü²Ã»»
      ***      IF         WRK-201004-KSN19(IDX1)      NOT =   ZERO
      ***          ADD      1                  TO  WRK-MEISAISU
      ***          MOVE   CONS-201004-KSN19    TO  LNK-SROUNYUIN-SRYCD
      ***                                  (IDX1 WRK-MEISAISU)
      ***          MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
      ***                                  (IDX1 WRK-MEISAISU)
      ***      END-IF
      *
               IF      (WRK-NYUINKHN(IDX1)   NOT =         SPACE)  AND
                       (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-GENSAN-IRYO(IDX1)   NOT =      SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GENSAN3       TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"          TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                   MOVE     1               TO  WRK-GENSAN3-MIJISI
               END-IF
               IF      (WRK-NYUINKHN(IDX1)   NOT =         SPACE)  AND
                       (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-GENSAN-JYOKU(IDX1)   NOT =     SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GENSAN4       TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"          TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                   MOVE     1               TO  WRK-GENSAN4-MIJISI
               END-IF
      *        Ï«ºÒ¾è¿ô¡ÊÆÃÄêÆþ±¡ÎÁ¤Î¾ì¹ç¤ÏÏ«ºÒ¾è¿ô¤Ê¤·¡Ë
               IF       LNK-SROUNYUIN-SRYKBN(IDX1)    =    "90"
                 IF     FLG-KENPOJYUNKYO  =  1
                   CONTINUE
                 ELSE
                   EVALUATE  LNK-SROUNYUIN-JOUSU-KBN(IDX1)
                       WHEN  "1"
                           ADD      1           TO  WRK-MEISAISU
                           MOVE   "101900010"   TO  LNK-SROUNYUIN-SRYCD
                                               (IDX1 WRK-MEISAISU)
                           MOVE    "4"      TO  LNK-SROUNYUIN-SRYCD-SK
                                               (IDX1 WRK-MEISAISU)
                       WHEN  "2"
                           ADD      1           TO  WRK-MEISAISU
                           MOVE   "101900020"   TO  LNK-SROUNYUIN-SRYCD
                                               (IDX1 WRK-MEISAISU)
                           MOVE    "4"      TO  LNK-SROUNYUIN-SRYCD-SK
                                               (IDX1 WRK-MEISAISU)
                   END-EVALUATE
                 END-IF
               END-IF
      *        ½é´ü²Ã»»¡ÊÄ¹´ü¸º»»¡Ë
               IF      (WRK-GAIHAKU(IDX1)        =
                                     SPACE      OR
                                    "190142190" OR  "190142090" OR
                                    "190142290" OR  "190141590" OR
                                    "190153190" OR  "190153090" OR
                                    "190153390" OR  "190153290" )
               AND     (WRK-SHOKIKSN(IDX1)   NOT =         SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-SHOKIKSN(IDX1) TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "5"           TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        É¸·ç
               IF       WRK-HYOKETU(IDX1)    NOT =         SPACE
                 IF    (WRK-GAIHAKU(IDX1)        =
                                     SPACE      OR
                                    "190142190" OR  "190142090" OR
                                    "190142290" OR  "190141590" OR
                                    "190108790" OR  "190108890" OR
                                    "190109090" OR
                                    "190153190" OR  "190153090" OR
                                    "190153390" OR  "190153290" )
                 AND   (WRK-NYUINKHN(IDX1)   NOT =       SPACE  )
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-HYOKETU(IDX1)  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "6"           TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                 ELSE
                   MOVE   SPACE             TO  WRK-HYOKETU(IDX1)
                 END-IF
               END-IF
      *        Æþ±¡ÎÁ²Ã»»¡ÊÆþ±¡²ñ·×¡Ë
      *         PERFORM   VARYING   KSN-IDX   FROM  1  BY  1
      *                   UNTIL    (WRK-NYUKSN(KSN-IDX)  =  SPACE)  OR
      *                            (KSN-IDX             >  20)
      *            IF    WRK-NYUKSN-DAY(KSN-IDX IDX1)  =  ZERO
      *                CONTINUE
      *            ELSE
      *                ADD     1           TO  WRK-MEISAISU
      *                MOVE WRK-NYUKSN(KSN-IDX)
      *                                    TO  LNK-SROUNYUIN-SRYCD
      *                                   (IDX1 WRK-MEISAISU)
      *            END-IF
      *         END-PERFORM
           END-PERFORM
      *
      *    ºÆ¹½ÃÛ¤·¤¿Ï«ºÒÆþ±¡²ñ·×¾ðÊó¤«¤éÅÀ¿ô¤Î»»Äê
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
      *
               MOVE     ZERO       TO       WRK-HYOKETU-TEN
                                            WRK-ZAITEN-HYOKETU
               IF   WRK-HYOKETU(IDX1)   =   SPACE
      *          ÅÀ¿ô¥Þ¥¹¥¿¤ÎÆÉ¤ß¹þ¤ß
                 PERFORM   VARYING   IDX2    FROM   1  BY   1
                      UNTIL (LNK-SROUNYUIN-SRYCD(IDX1 IDX2) = SPACE)
                         OR (IDX2    >      20)
                     PERFORM    210-TENSU-KEISAN-SEC
                 END-PERFORM
               ELSE
      *          ÅÀ¿ô¥Þ¥¹¥¿¤ÎÆÉ¤ß¹þ¤ß
                 INITIALIZE                          TNS-TENSU-REC
                 MOVE    WRK-HYOKETU(IDX1)       TO  TNS-SRYCD
                 MOVE    LNK-SROUNYUIN-HOSPNUM   TO  TNS-HOSPNUM
                 MOVE    LNK-SROUNYUIN-SANTEIYM  TO  TNS-YUKOSTYMD(1:6)
                 MOVE    "01"                    TO  TNS-YUKOSTYMD(7:2)
                 MOVE    LNK-SROUNYUIN-SANTEIYM  TO  TNS-YUKOEDYMD(1:6)
                 MOVE    "01"                    TO  TNS-YUKOEDYMD(7:2)
                 PERFORM 910-TENSU-READ-SEC
                 MOVE    TNS-TEN                 TO  WRK-HYOKETU-TEN
      *
                 PERFORM   VARYING   IDX2    FROM   1  BY   1
                      UNTIL (LNK-SROUNYUIN-SRYCD(IDX1 IDX2) = SPACE)
                         OR (IDX2    >      20)
                     PERFORM    210-TENSU-KEISAN-HYOKETU-SEC
                 END-PERFORM
               END-IF
      *
               PERFORM   VARYING   KSN-IDX   FROM   1  BY   1
                         UNTIL (WRK-NYUKSN(KSN-IDX)  =  SPACE)  OR
                               (KSN-IDX    >      20)
                  IF    WRK-NYUKSN-DAY(KSN-IDX IDX1)  =  ZERO
                      CONTINUE
                  ELSE
                      ADD  WRK-NYUKSN-ZAITEN(KSN-IDX)
                                 TO  LNK-SROUNYUIN-ZAITEN-NACCT(IDX1)
                  END-IF
               END-PERFORM
      *
           END-PERFORM
      *
      *
           .
       210-ROUNYUIN-SET3-EXT.
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
               MOVE     "tbl_nyuinact" TO  MCP-TABLE
               MOVE     "key9"         TO  MCP-PATHNAME
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
      *    ³°ÇñÁ°½èÍý
      *****************************************************************
       210-GAIHAKU-MAE-SEC            SECTION.
      *
      *    ³°Çñ¥³¡¼¥ÉÊÔ½¸ÍÑ¤ËÆþ±¡´ðËÜÎÁ²Ã»»¶èÊ¬¤È¿ÇÎÅ¶èÊ¬¤ÎÂàÈò
           INITIALIZE                          TNS-TENSU-REC
           MOVE    NSRY-SRYCD(1)           TO  TNS-SRYCD
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO  TNS-HOSPNUM
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  TNS-YUKOSTYMD(1:6)
           MOVE    "01"                    TO  TNS-YUKOSTYMD(7:2)
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  TNS-YUKOEDYMD(1:6)
           MOVE    "01"                    TO  TNS-YUKOEDYMD(7:2)
           PERFORM 910-TENSU-READ-SEC
           MOVE    TNS-NYUKHNKSNKBN        TO  WRK-KSNKBN
      *
           .
       210-GAIHAKU-MAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ³°Çñ¿ÇÎÅ¥³¡¼¥É¼èÆÀ
      *****************************************************************
       210-GAIHAKU-SET-SEC            SECTION.
      *
           MOVE   NACCT-DAY(IDX1)     TO    LNK-SROUNYUIN-GAIHAKU(IDX1)
      *
           EVALUATE   LNK-SROUNYUIN-SRYKBN(IDX1)
              WHEN     "90"
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     1
                      MOVE   "190107290"   TO  WRK-GAIHAKU(IDX1)
                    WHEN     2
                      MOVE   "190107390"   TO  WRK-GAIHAKU(IDX1)
                    WHEN     4
                    WHEN     5
                    WHEN     6
                    WHEN     7
                      PERFORM   2101-GAIHAKU-TAKIKAN-SEC
                 END-EVALUATE
              WHEN     "92"
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     1
                      MOVE   "190107490"   TO  WRK-GAIHAKU(IDX1)
                    WHEN     2
                      MOVE   "190107590"   TO  WRK-GAIHAKU(IDX1)
                    WHEN     4
                    WHEN     5
                    WHEN     6
                    WHEN     7
                      PERFORM   2101-GAIHAKU-TAKIKAN-SEC
                 END-EVALUATE
           END-EVALUATE
      *
           IF       WRK-GAIHAKU(IDX1)   =    SPACE
                 MOVE   ZERO    TO    LNK-SROUNYUIN-GAIHAKU(IDX1)
           END-IF
      *
           .
       210-GAIHAKU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ³°Çñ¿ÇÎÅ¥³¡¼¥É¼èÆÀ
      *****************************************************************
       2101-GAIHAKU-TAKIKAN-SEC           SECTION.
      *
           EVALUATE   WRK-NYUKHNKSNKBN(IDX1)
      *
               WHEN   099  THRU  100
                 IF   LNK-SROUNYUIN-SANTEIYM   >=   "201004"
                   EVALUATE   NACCT-DAY(IDX1)
                    WHEN     4
                      MOVE   "190108890"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     5
                      MOVE   "190142190"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     6
                    WHEN     7
                      MOVE      1           TO   WRK-TAIJYUSIN-ERR
                   END-EVALUATE
                 END-IF
               WHEN   201  THRU  219
               WHEN   221  THRU  222
               WHEN   224
               WHEN   227  THRU  229
               WHEN   230  THRU  239
               WHEN   241  THRU  249
               WHEN   250  THRU  255
                 IF    WRK-NYUKHNKSNKBN(IDX1) = 221 OR 232
                   EVALUATE   NACCT-DAY(IDX1)
                    WHEN     4
                      MOVE   "190108790"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     5
                      MOVE   "190142090"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     6
                      MOVE   "190153190"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     7
                      MOVE   "190153390"    TO   WRK-GAIHAKU(IDX1)
                   END-EVALUATE
                 ELSE
                   IF  WRK-NYUKHNKSNKBN(IDX1) =
                           229 OR 238 OR 215 OR 216 OR 217 OR 239
                     EVALUATE   NACCT-DAY(IDX1)
                       WHEN     4
                         MOVE   "190108790" TO   WRK-GAIHAKU(IDX1)
                       WHEN     5
                         MOVE   "190142090" TO   WRK-GAIHAKU(IDX1)
                       WHEN     6
                         MOVE   "190153190" TO   WRK-GAIHAKU(IDX1)
                       WHEN     7
                         MOVE      1        TO   WRK-TAIJYUSIN-ERR
                     END-EVALUATE
                   ELSE
                     EVALUATE   NACCT-DAY(IDX1)
                       WHEN     4
                         MOVE   "190108790" TO   WRK-GAIHAKU(IDX1)
                       WHEN     5
                         MOVE   "190142090" TO   WRK-GAIHAKU(IDX1)
                       WHEN     6
                       WHEN     7
                         MOVE      1        TO   WRK-TAIJYUSIN-ERR
                     END-EVALUATE
                   END-IF
                 END-IF
               WHEN   013  THRU  021
               WHEN   101  THRU  109
               WHEN   147  THRU  152
               WHEN   173  THRU  190
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     4
                      MOVE   "190108890"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     5
                      MOVE   "190142190"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     6
                    WHEN     7
                      MOVE      1           TO   WRK-TAIJYUSIN-ERR
                 END-EVALUATE
               WHEN   075  THRU  080
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     4
                      MOVE   "190108990"    TO   WRK-GAIHAKU(IDX1)
                 END-EVALUATE
               WHEN   085  THRU  086
               WHEN   132  THRU  133
               WHEN   153  THRU  158
                 IF    WRK-NYUKHNKSNKBN(IDX1) =
                           153 OR 154 OR 155 OR 156 OR 157 OR 158
                   EVALUATE   NACCT-DAY(IDX1)
                    WHEN     4
                      MOVE   "190109090"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     5
                      MOVE   "190142290"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     6
                      MOVE      1           TO   WRK-TAIJYUSIN-ERR
                    WHEN     7
                      MOVE   "190153290"    TO   WRK-GAIHAKU(IDX1)
                   END-EVALUATE
                 ELSE
                   EVALUATE   NACCT-DAY(IDX1)
                    WHEN     4
                      MOVE   "190109090"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     5
                      MOVE   "190142290"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     6
                    WHEN     7
                      MOVE      1           TO   WRK-TAIJYUSIN-ERR
                   END-EVALUATE
                 END-IF
               WHEN   226
               WHEN   240
                 IF        LNK-SROUNYUIN-SANTEIYM   >=  "201004"
                         MOVE    1    TO    WRK-TAIJYUSIN-ERR
                 END-IF
               WHEN   031
               WHEN   033
               WHEN   034
               WHEN   035
               WHEN   038
               WHEN   170
               WHEN   191
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     4
                    WHEN     6
                      IF   LNK-SROUNYUIN-SANTEIYM   >=  "201004"
                         MOVE    1          TO   WRK-TAIJYUSIN-ERR
                      END-IF
                    WHEN     5
                      MOVE   "190141590"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     7
                      MOVE   "190153090"    TO   WRK-GAIHAKU(IDX1)
                 END-EVALUATE
               WHEN   022
               WHEN   023
               WHEN   024
               WHEN   025
               WHEN   026
               WHEN   029
               WHEN   136
               WHEN   168
               WHEN   169
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     4
                    WHEN     6
                      IF   LNK-SROUNYUIN-SANTEIYM   >=  "201004"
                         MOVE    1          TO   WRK-TAIJYUSIN-ERR
                      END-IF
                    WHEN     5
                      MOVE   "190141590"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     7
                      MOVE   "190153090"    TO   WRK-GAIHAKU(IDX1)
                 END-EVALUATE
               WHEN   081
               WHEN   083
               WHEN   172
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     4
                    WHEN     6
                      IF   LNK-SROUNYUIN-SANTEIYM   >=  "201004"
                         MOVE    1          TO   WRK-TAIJYUSIN-ERR
                      END-IF
                    WHEN     5
                      MOVE   "190141590"    TO   WRK-GAIHAKU(IDX1)
                    WHEN     7
                      MOVE   "190153090"    TO   WRK-GAIHAKU(IDX1)
                 END-EVALUATE
               WHEN   OTHER
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     4
                    WHEN     6
                    WHEN     7
                      IF   LNK-SROUNYUIN-SANTEIYM   >=  "201004"
                         MOVE    1          TO   WRK-TAIJYUSIN-ERR
                      END-IF
                    WHEN     5
                      MOVE   "190141590"    TO   WRK-GAIHAKU(IDX1)
                 END-EVALUATE
      *
           END-EVALUATE
      *
           .
       2101-GAIHAKU-TAKIKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô·×»»½èÍý
      *****************************************************************
       210-TENSU-KEISAN-SEC            SECTION.
      *
           INITIALIZE                      TNS-TENSU-REC
           IF    (LNK-SROUNYUIN-SRYCD-SK(IDX1 IDX2) =  "1"  )
           AND   (WRK-GAIHAKU(IDX1)           NOT   =  SPACE)
               MOVE    SPACE    TO   WRK-GAIHAKU-SRYCD
               IF    LNK-SROUNYUIN-SANTEIYM   <=  "201203"
                  PERFORM    2101-GAIHAKU-NYUINKHN-SET-SEC
               END-IF
               IF    WRK-GAIHAKU-SRYCD    NOT =   SPACE
                  MOVE    WRK-GAIHAKU-SRYCD   TO  TNS-SRYCD
               ELSE
                  MOVE    LNK-SROUNYUIN-SRYCD(IDX1 IDX2)
                                              TO  TNS-SRYCD
               END-IF
           ELSE
                  MOVE    LNK-SROUNYUIN-SRYCD(IDX1 IDX2)
                                              TO  TNS-SRYCD
           END-IF
           MOVE   LNK-SROUNYUIN-HOSPNUM       TO  TNS-HOSPNUM
           MOVE   LNK-SROUNYUIN-SANTEIYM      TO  TNS-YUKOSTYMD(1:6)
           MOVE   "01"                        TO  TNS-YUKOSTYMD(7:2)
           MOVE   LNK-SROUNYUIN-SANTEIYM      TO  TNS-YUKOEDYMD(1:6)
           MOVE   "01"                        TO  TNS-YUKOEDYMD(7:2)
           PERFORM 910-TENSU-READ-SEC
      *
      *    ÅÀ¿ô»»Äê
           EVALUATE    TNS-TENSIKIBETU
      *        ÅÀ¿ô¡Ê¥×¥é¥¹¡Ë
               WHEN    3
                   ADD   TNS-TEN     TO  LNK-SROUNYUIN-ZAITEN(IDX1)
      *        ¡ó²Ã»»
               WHEN    5
                   COMPUTE    LNK-SROUNYUIN-ZAITEN(IDX1)  =
                              LNK-SROUNYUIN-ZAITEN(IDX1)  +
                            ((LNK-SROUNYUIN-ZAITEN(IDX1)  *
                              (TNS-TEN   /  100))  + 0.5)
      *        ¡ó¸º»»
               WHEN    6
                   COMPUTE    LNK-SROUNYUIN-ZAITEN(IDX1)  =
                            ( LNK-SROUNYUIN-ZAITEN(IDX1)  -
                             (LNK-SROUNYUIN-ZAITEN(IDX1)  *
                             (TNS-TEN   /  100)))        +  0.5
      *        ÅÀ¿ô¡Ê¥Þ¥¤¥Ê¥¹¡Ë
               WHEN    8
                   COMPUTE  LNK-SROUNYUIN-ZAITEN(IDX1)  =
                            LNK-SROUNYUIN-ZAITEN(IDX1)  -  TNS-TEN
      *        WHEN  OTHER
      *            ADD   TNS-TEN     TO  LNK-SROUNYUIN-ZAITEN(IDX1)
           END-EVALUATE
      *
      *    ¿ÇÎÅ¥³¡¼¥É·×
           MOVE    TNS-SRYCD          TO  WRK-SRYCD-X
           COMPUTE LNK-SROUNYUIN-SRYCDTOTAL(IDX1)  =
                   LNK-SROUNYUIN-SRYCDTOTAL(IDX1)  +   WRK-SRYCD-9
           ADD      1          TO  LNK-SROUNYUIN-MEISAISU(IDX1)
      *
           .
       210-TENSU-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô·×»»½èÍý¡ÊÉ¸·ç¡Ë
      *****************************************************************
       210-TENSU-KEISAN-HYOKETU-SEC            SECTION.
      *
           IF   LNK-SROUNYUIN-SRYCD-SK(IDX1 IDX2)       =   "5"
              MOVE  LNK-SROUNYUIN-ZAITEN(IDX1)  TO  WRK-ZAITEN-HYOKETU
              MOVE       ZERO      TO     LNK-SROUNYUIN-ZAITEN(IDX1)
           END-IF
      *
           IF   LNK-SROUNYUIN-SRYCD-SK(IDX1 IDX2)   NOT =   "6"
              PERFORM    210-TENSU-KEISAN-SEC
           END-IF
      *
           IF    ((LNK-SROUNYUIN-SRYCD-SK(IDX1 IDX2) =  "1"  ) AND
                  (WRK-GAIHAKU(IDX1)                 =  SPACE))
           OR    ( LNK-SROUNYUIN-SRYCD-SK(IDX1 IDX2) =  "2"   )
      *      É¸·ç¡§¡ó¸º»»
             COMPUTE    LNK-SROUNYUIN-ZAITEN(IDX1)  =
                      ( LNK-SROUNYUIN-ZAITEN(IDX1)  -
                       (LNK-SROUNYUIN-ZAITEN(IDX1)  *
                       (WRK-HYOKETU-TEN   /  100))) +  0.5
      *      ¿ÇÎÅ¥³¡¼¥É·×
             MOVE    WRK-HYOKETU(IDX1)   TO  WRK-SRYCD-X
             COMPUTE LNK-SROUNYUIN-SRYCDTOTAL(IDX1)  =
                     LNK-SROUNYUIN-SRYCDTOTAL(IDX1)  +   WRK-SRYCD-9
             ADD      1          TO  LNK-SROUNYUIN-MEISAISU(IDX1)
           END-IF
      *
           IF      LNK-SROUNYUIN-SRYCD-SK(IDX1 IDX2)  =   "5"
             IF    LNK-SROUNYUIN-ZAITEN(IDX1)   >=   ZERO
      *        É¸·ç¡§¡ó¸º»»
               COMPUTE    LNK-SROUNYUIN-ZAITEN(IDX1)  =
                        ( LNK-SROUNYUIN-ZAITEN(IDX1)  -
                         (LNK-SROUNYUIN-ZAITEN(IDX1)  *
                         (WRK-HYOKETU-TEN   /  100))) +  0.5
             ELSE
               COMPUTE    LNK-SROUNYUIN-ZAITEN(IDX1)  =
                          LNK-SROUNYUIN-ZAITEN(IDX1)  *
                          -1
               COMPUTE    LNK-SROUNYUIN-ZAITEN(IDX1)  =
                        ( LNK-SROUNYUIN-ZAITEN(IDX1)  -
                         (LNK-SROUNYUIN-ZAITEN(IDX1)  *
                         (WRK-HYOKETU-TEN   /  100))) +  0.5
               COMPUTE    LNK-SROUNYUIN-ZAITEN(IDX1)  =
                          LNK-SROUNYUIN-ZAITEN(IDX1)  *
                          -1
             END-IF
      *
             COMPUTE LNK-SROUNYUIN-ZAITEN(IDX1)  =
                     LNK-SROUNYUIN-ZAITEN(IDX1)  +  WRK-ZAITEN-HYOKETU
           END-IF
      *
           .
       210-TENSU-KEISAN-HYOKETU-EXT.
           EXIT.
      *
      **************************************************************
      *    ³°Çñ»þ¤ÎÆþ±¡´ðËÜÎÁ¼èÆÀ
      **************************************************************
       2101-GAIHAKU-NYUINKHN-SET-SEC           SECTION.
      *
           EVALUATE  LNK-SROUNYUIN-SRYCD(IDX1 IDX2)
               WHEN  "190078210"
                     MOVE  "190077410"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078310"
                     MOVE  "190077510"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078410"
                     MOVE  "190077610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078510"
                     MOVE  "190077710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078610"
                     MOVE  "190077810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078710"
                     MOVE  "190077910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078810"
                     MOVE  "190078010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078910"
                     MOVE  "190078110"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190082410"
                     MOVE  "190081710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082510"
                     MOVE  "190081810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082610"
                     MOVE  "190081910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082710"
                     MOVE  "190082010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082810"
                     MOVE  "190082110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082910"
                     MOVE  "190082210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190083010"
                     MOVE  "190082310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190084310"
                     MOVE  "190083610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084410"
                     MOVE  "190083710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084510"
                     MOVE  "190083810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084610"
                     MOVE  "190083910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084710"
                     MOVE  "190084010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084810"
                     MOVE  "190084110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084910"
                     MOVE  "190084210"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190087410"
                     MOVE  "190085910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087510"
                     MOVE  "190086010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087610"
                     MOVE  "190086110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087710"
                     MOVE  "190086210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087810"
                     MOVE  "190086310"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087910"
                     MOVE  "190086410"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088010"
                     MOVE  "190086510"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088110"
                     MOVE  "190086610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088210"
                     MOVE  "190086710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088310"
                     MOVE  "190086810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088410"
                     MOVE  "190086910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088510"
                     MOVE  "190087010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088610"
                     MOVE  "190087110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088710"
                     MOVE  "190087210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088810"
                     MOVE  "190087310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190094710"
                     MOVE  "190093710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190094810"
                     MOVE  "190093810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190094910"
                     MOVE  "190093910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190095010"
                     MOVE  "190094010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190095110"
                     MOVE  "190094110"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190797410"
                     MOVE  "190796610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797510"
                     MOVE  "190796710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797610"
                     MOVE  "190796810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797710"
                     MOVE  "190796910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797810"
                     MOVE  "190797010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797910"
                     MOVE  "190797110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190798010"
                     MOVE  "190797210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190798110"
                     MOVE  "190797310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190803510"
                     MOVE  "190802810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803610"
                     MOVE  "190802910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803710"
                     MOVE  "190803010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803810"
                     MOVE  "190803110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803910"
                     MOVE  "190803210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190804010"
                     MOVE  "190803310"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190804110"
                     MOVE  "190803410"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190091210"
                     MOVE  "190090210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190091310"
                     MOVE  "190090310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190109670"
                     MOVE  "190097010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190808870"
                     MOVE  "190805710"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190117070"
                     MOVE  "190097010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190813270"
                     MOVE  "190805710"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190117670"
                     MOVE  "190097010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190813870"
                     MOVE  "190805710"      TO   WRK-GAIHAKU-SRYCD
      *        ¿ÇÎÅ½êÏ·¿Í°åÎÅ´ÉÍýÎÁ
               WHEN  "190774210"
                     MOVE  "190774110"      TO   WRK-GAIHAKU-SRYCD
      *        Ï·¿ÍÀ­Ç§ÃÎ¾É¼À´µ¼£ÎÅÉÂÅïÆþ±¡ÎÁ£±
               WHEN  "190740010"
                     MOVE  "190739910"      TO   WRK-GAIHAKU-SRYCD
      *        Ï·¿ÍÀ­Ç§ÃÎ¾É¼À´µ¼£ÎÅÉÂÅïÆþ±¡ÎÁ£²
               WHEN  "190813510"
                     MOVE  "190813410"      TO   WRK-GAIHAKU-SRYCD
           END-EVALUATE
      *
           IF   (LNK-SROUNYUIN-SANTEIYM  >= "200604"   )
           AND  (WRK-GAIHAKU-SRYCD        = "190774110" OR
                                            "190739910" OR
                                            "190813410")
               MOVE    SPACE    TO    WRK-GAIHAKU-SRYCD
           END-IF
      *
           .
       2101-GAIHAKU-NYUINKHN-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    Î¬¾Î¤ÎÊÔ½¸
      **************************************************************
       210-RYAKUSYO-HENSYU-SEC           SECTION.
      *
           IF   LNK-SROUNYUIN-RYAKUKBN   =   "1"
      *
              MOVE    SPACE         TO   NYURYAKUSHO-REC
              INITIALIZE                 NYURYAKUSHO-REC
      *
              MOVE WRK-RYAKU-SRYCD        TO NYURYAKUSHO-SRYCD
      ***     MOVE LNK-SROUNYUIN-HOSPNUM  TO NYURYAKUSHO-HOSPNUM
              MOVE LNK-SROUNYUIN-SANTEIYM TO NYURYAKUSHO-YUKOSTYMD(1:6)
              MOVE "01"                   TO NYURYAKUSHO-YUKOSTYMD(7:2)
              MOVE LNK-SROUNYUIN-SANTEIYM TO NYURYAKUSHO-YUKOEDYMD(1:6)
              MOVE "01"                   TO NYURYAKUSHO-YUKOEDYMD(7:2)
              PERFORM  910-NYURYAKUSHO-READ-SEC
      *
              IF       FLG-NYURYAKUSHO    =    ZERO
      *
                EVALUATE   WRK-ZAISKBKBN
      *
                 WHEN     1
      *
                  IF   WRK-SRYKBN  =  "90"
      *
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                          UNTIL   ID1                          > 20
                            OR    LNK-SROUNYUIN-RYAKUSYO2(ID1) = SPACE
                            OR    FLG-ARI                      = 1 
                       IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)
                                           =  NYURYAKUSHO-RYAKUSHO
                           MOVE  1   TO   FLG-ARI
                       END-IF
                    END-PERFORM  
                    IF  FLG-ARI  =  ZERO  
                           MOVE   NYURYAKUSHO-RYAKUSHO
                                     TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)
                    END-IF
      *
                    IF  LNK-SROUNYUIN-SANTEIYM  >= "200604"
                     CONTINUE
                    ELSE
                     IF  WRK-RYAKU-SRYCD  =  "190109670" OR "190808870"
      *
                       MOVE     ZERO     TO    FLG-ARI
                       PERFORM  VARYING  ID1    FROM    1   BY  1
                           UNTIL   ID1                          > 20
                             OR    LNK-SROUNYUIN-RYAKUSYO2(ID1) = SPACE
                             OR    FLG-ARI                      = 1 
                         IF LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:4) = "Í­´Ç"
                              MOVE  1   TO   FLG-ARI
                         END-IF
                       END-PERFORM  
                       IF  FLG-ARI  =  ZERO  
                            MOVE  "Í­´Ç"
                                TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:4)
                       END-IF
      *
                     END-IF
                     IF  WRK-RYAKU-SRYCD  =  "190117070" OR "190813270"
      *
                       MOVE     ZERO     TO    FLG-ARI
                       PERFORM  VARYING  ID1    FROM    1   BY  1
                           UNTIL   ID1                          > 20
                             OR    LNK-SROUNYUIN-RYAKUSYO2(ID1) = SPACE
                             OR    FLG-ARI                      = 1 
                         IF LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:4) = "Í­°å"
                              MOVE  1   TO   FLG-ARI
                         END-IF
                       END-PERFORM  
                       IF  FLG-ARI  =  ZERO  
                            MOVE  "Í­°å"
                                TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:4)
                       END-IF
      *
                     END-IF
                     IF  WRK-RYAKU-SRYCD  =  "190117670" OR "190813870"
      *
                       MOVE     ZERO     TO    FLG-ARI
                       PERFORM  VARYING  ID1    FROM    1   BY  1
                           UNTIL   ID1                          > 20
                             OR    LNK-SROUNYUIN-RYAKUSYO2(ID1) = SPACE
                             OR    FLG-ARI                      = 1 
                         IF LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:6) = "Í­´Ç°å"
                              MOVE  1   TO   FLG-ARI
                         END-IF
                       END-PERFORM  
                       IF  FLG-ARI  =  ZERO  
                            MOVE  "Í­´Ç°å"
                                TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:6)
                       END-IF
      *
                     END-IF
                    END-IF
      *
                  ELSE
      *
                    MOVE    NYURYAKUSHO-RYAKUSHO   TO  WRK-RYAKUSHO
      *
                  END-IF
      *
                 WHEN   6
      *
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                          UNTIL   ID1                          > 20
                            OR    LNK-SROUNYUIN-RYAKUSYO2(ID1) = SPACE
                            OR    FLG-ARI                      = 1 
                       IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)
                                           =  NYURYAKUSHO-RYAKUSHO
                           MOVE  1   TO   FLG-ARI
                       END-IF
                    END-PERFORM  
                    IF  FLG-ARI  =  ZERO  
                           MOVE   NYURYAKUSHO-RYAKUSHO
                                     TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)
                    END-IF
      *
                END-EVALUATE
              ELSE
                IF   ( LNK-SROUNYUIN-SANTEIYM  >= "200604"    )
                AND  ( WRK-RYAKU-SRYCD          = "190123250" OR
                                                  "190123350" OR
                                                  "190123450" OR
                                                  "190123550" )
      *
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                          UNTIL   ID1                          > 20
                            OR    LNK-SROUNYUIN-RYAKUSYO2(ID1) = SPACE
                            OR    FLG-ARI                      = 1
                      EVALUATE   WRK-RYAKU-SRYCD
                        WHEN     "190123250"
                         IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:8)
                                                         = "°ìÈÌ·Ð²á"
                             MOVE  1   TO   FLG-ARI
                         END-IF
                        WHEN     "190123350"
                         IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:8)
                                                         = "·ë³Ë·Ð²á"
                             MOVE  1   TO   FLG-ARI
                         END-IF
                        WHEN     "190123450"
                         IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:8)
                                                         = "Àº¿À·Ð²á"
                             MOVE  1   TO   FLG-ARI
                         END-IF
                        WHEN     "190123550"
                         IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:8)
                                                         = "¾ã³²·Ð²á"
                             MOVE  1   TO   FLG-ARI
                         END-IF
                      END-EVALUATE
                    END-PERFORM
                    IF  FLG-ARI  =  ZERO
                      EVALUATE   WRK-RYAKU-SRYCD
                        WHEN     "190123250"
                           MOVE   "°ìÈÌ·Ð²á"
                                 TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:8)
                        WHEN     "190123350"
                           MOVE   "·ë³Ë·Ð²á"
                                 TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:8)
                        WHEN     "190123450"
                           MOVE   "Àº¿À·Ð²á"
                                 TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:8)
                        WHEN     "190123550"
                           MOVE   "¾ã³²·Ð²á"
                                 TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:8)
                      END-EVALUATE
                    END-IF
      *
                END-IF
                IF   ( LNK-SROUNYUIN-SANTEIYM  >= "200807"    )
                AND  ( WRK-RYAKU-SRYCD          = "190130810" OR
                                                  "190131010" OR
                                                  "190131110" )
      *
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                          UNTIL   ID1                          > 20
                            OR    LNK-SROUNYUIN-RYAKUSYO2(ID1) = SPACE
                            OR    FLG-ARI                      = 1
                      EVALUATE   WRK-RYAKU-SRYCD
                        WHEN     "190130810"
                         IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:8)
                                                         = "°ìÈÌ£±£°"
                             MOVE  1   TO   FLG-ARI
                         END-IF
                        WHEN     "190131010"
                         IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:6)
                                                         = "·ë£±£°"
                             MOVE  1   TO   FLG-ARI
                         END-IF
                        WHEN     "190131110"
                         IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:6)
                                                         = "Àì£±£°"
                             MOVE  1   TO   FLG-ARI
                         END-IF
                      END-EVALUATE
                    END-PERFORM
                    IF  FLG-ARI  =  ZERO
                      EVALUATE   WRK-RYAKU-SRYCD
                        WHEN     "190130810"
                           MOVE   "°ìÈÌ£±£°"
                                 TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:8)
                        WHEN     "190131010"
                           MOVE   "·ë£±£°"
                                 TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:6)
                        WHEN     "190131110"
                           MOVE   "Àì£±£°"
                                 TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:6)
                      END-EVALUATE
                    END-IF
      *
                END-IF
              END-IF
      *
           END-IF
      *
           .
       210-RYAKUSYO-HENSYU-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¤ÎÆÉ¤ß¹þ¤ß½èÍý
      **************************************************************
       210-SRYACT-HENSYU-SEC           SECTION.
      *
           IF   LNK-SROUNYUIN-RYAKUKBN   =   "1"
      *
              INITIALIZE                           SRYACT-REC
              MOVE    ACCT-HOSPNUM         TO      SRY-HOSPNUM
              MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
              MOVE    ACCT-PTID            TO      SRY-PTID
              MOVE    ACCT-SRYYM           TO      SRY-SRYYM
              MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
              MOVE    SRYACT-REC           TO      MCPDATA-REC
              MOVE    "DBSELECT"           TO      MCP-FUNC
              MOVE    "tbl_sryact"         TO      MCP-TABLE
              MOVE    "key9"               TO      MCP-PATHNAME
              CALL    "ORCDBMAIN"          USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
              IF      MCP-RC               =       ZERO
                  MOVE    "tbl_sryact"     TO      MCP-TABLE
                  MOVE    "key9"           TO      MCP-PATHNAME
                  PERFORM  910-SRYACT-READ-SEC
                  PERFORM  UNTIL  FLG-SRYACT    =    1
                      PERFORM   VARYING   SRYACT-IDX  FROM  1   BY   1
                                UNTIL    (SRYACT-IDX    >   5)  OR
                                         (SRY-SRYCD(SRYACT-IDX)
                                                            =   SPACE)
      *
                          MOVE    SRY-SRYCD(SRYACT-IDX)
                                                  TO  WRK-RYAKU-SRYCD
                          MOVE    "6"             TO  WRK-ZAISKBKBN
      *
                          PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                      END-PERFORM
                      MOVE     "tbl_sryact"       TO  MCP-TABLE
                      MOVE     "key9"             TO  MCP-PATHNAME
                      PERFORM   910-SRYACT-READ-SEC
                  END-PERFORM
              END-IF
      *
              MOVE     "tbl_sryact"               TO  MCP-TABLE
              MOVE     "key9"                     TO  MCP-PATHNAME
              PERFORM   910-DB-CLOSE-SEC
      *
           END-IF
      *
           .
       210-SRYACT-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    ·îÆâ¤ËÊ£¿ô¤Î°åÎÅ´ü´Ö¾ðÊó¤¬¤Ê¤¤¤«¸¡º÷¡Ê°åÎÅµ¡´Ø¸º»»¤Î³ÎÇ§¡Ë  *
      ******************************************************************
       210-SYS-5000-CHK-SEC                 SECTION.
      *
           MOVE    ZERO                TO  WRK-SYS5000-COUNT
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-SROUNYUIN-HOSPNUM
                                       TO  SYS-HOSPNUM
           MOVE    LNK-SROUNYUIN-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "__"                TO  SYS-STYUKYMD(7:2)
           MOVE    LNK-SROUNYUIN-SANTEIYM
                                       TO  SYS-EDYUKYMD(1:6)
           MOVE    "__"                TO  SYS-EDYUKYMD(7:2)
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key11"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                   ADD     1     TO        WRK-SYS5000-COUNT
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key11"         TO  MCP-PATHNAME
                   PERFORM  910-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE     "tbl_syskanri"     TO  MCP-TABLE
           MOVE     "key11"            TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
      *
       210-SYS-5000-CHK-EXT.
           EXIT.
      *
      ******************************************************************
      *    ·îÆâ¤Î°åÎÅ´ü´Ö¾ðÊó¼èÆÀ¡Ê°åÎÅµ¡´Ø¸º»»¾ðÊó¡Ë                  *
      ******************************************************************
       210-SYS-5000-GET-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-SROUNYUIN-HOSPNUM
                                       TO  SYS-HOSPNUM
           MOVE    LNK-SROUNYUIN-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "__"                TO  SYS-STYUKYMD(7:2)
           MOVE    LNK-SROUNYUIN-SANTEIYM
                                       TO  SYS-EDYUKYMD(1:6)
           MOVE    "__"                TO  SYS-EDYUKYMD(7:2)
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key11"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                 MOVE   SYS-5000-STYUKYMD(7:2)  TO  IDV
                 PERFORM  VARYING   GENSAN-IDX  FROM  1  BY  1
                            UNTIL     GENSAN-IDX   >   4
                    IF    SYS-5000-GENKBN(GENSAN-IDX)   =   "2"
                        EVALUATE  GENSAN-IDX
                            WHEN  2
                              MOVE  "190080570"  TO  WRK-GENSAN2
      **                        IF   FLG-RJN    =    ZERO
      **                            MOVE  "190080570"  TO  WRK-GENSAN2
      **                        ELSE
      **                            MOVE  "190799770"  TO  WRK-GENSAN2
      **                        END-IF
                              PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                                MOVE   "1"  TO
                                        WRK-GENSAN-INNAI(GENSAN-IDX2)
                              END-PERFORM
                            WHEN  3
                              MOVE  "190111690"  TO  WRK-GENSAN3
      **                        IF   FLG-RJN    =    ZERO
      **                            MOVE  "190111690"  TO  WRK-GENSAN3
      **                        ELSE
      **                            MOVE  "190809090"  TO  WRK-GENSAN3
      **                        END-IF
                              PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                                MOVE   "1"  TO
                                        WRK-GENSAN-IRYO(GENSAN-IDX2)
                              END-PERFORM
                            WHEN  4
                              MOVE  "190111790"  TO  WRK-GENSAN4
      **                        IF   FLG-RJN    =    ZERO
      **                            MOVE  "190111790"  TO  WRK-GENSAN4
      **                        ELSE
      **                            MOVE  "190809190"  TO  WRK-GENSAN4
      **                        END-IF
                              PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                                MOVE   "1"  TO
                                        WRK-GENSAN-JYOKU(GENSAN-IDX2)
                              END-PERFORM
                        END-EVALUATE
                    ELSE
                        EVALUATE  GENSAN-IDX
                            WHEN  2
                              PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                                MOVE   SPACE  TO
                                        WRK-GENSAN-INNAI(GENSAN-IDX2)
                              END-PERFORM
                            WHEN  3
                              PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                                MOVE   SPACE  TO
                                        WRK-GENSAN-IRYO(GENSAN-IDX2)
                              END-PERFORM
                            WHEN  4
                              PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                                MOVE   SPACE  TO
                                        WRK-GENSAN-JYOKU(GENSAN-IDX2)
                              END-PERFORM
                        END-EVALUATE
                    END-IF
                 END-PERFORM
                 MOVE    "tbl_syskanri"       TO  MCP-TABLE
                 MOVE    "key11"              TO  MCP-PATHNAME
                 PERFORM  910-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE     "tbl_syskanri"            TO  MCP-TABLE
           MOVE     "key11"                   TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
      *
       210-SYS-5000-GET-EXT.
           EXIT.
      *
      ******************************************************************
      *    Ï«ºÒÆþ±¡¤ÎÏ«ºÒ¾è¿ô¡Ê´ü´Ö²Ã»»¡Ë¤Ë´Þ¤á¤ëÃí²Ã»»¤Î¼èÆÀ          *
      ******************************************************************
       210-CHUKSN-GET-SEC                 SECTION.
      *
      *ÅÀ¿ô¥Þ¥¹¥¿¤è¤êÃí²Ã»»¤ÎÂÐ¾Ý¿ÇÎÅ¥³¡¼¥É¤ò¼èÆÀ
      *
           MOVE   ZERO                     TO  IDX1
           INITIALIZE                          TNS-TENSU-REC
           MOVE   LNK-SROUNYUIN-HOSPNUM    TO  TNS-HOSPNUM
           MOVE   LNK-SROUNYUIN-SANTEIYM   TO  TNS-YUKOSTYMD(1:6)
           MOVE   "01"                     TO  TNS-YUKOSTYMD(7:2)
           MOVE   LNK-SROUNYUIN-SANTEIYM   TO  TNS-YUKOEDYMD(1:6)
           MOVE  "01"                      TO  TNS-YUKOEDYMD(7:2)
           MOVE  "tbl_tensu"               TO  MCP-TABLE
           MOVE  "key30"                   TO  MCP-PATHNAME
           PERFORM 910-TENSU-SELECT-SEC
           IF      FLG-TENSU               =   ZERO
               MOVE     "tbl_tensu"        TO  MCP-TABLE
               MOVE     "key30"            TO  MCP-PATHNAME
               PERFORM   910-TENSU-FETCH-SEC
               PERFORM   UNTIL   (FLG-TENSU  =   1)   OR
                                 (IDX1       >=  20)
                 IF   TNS-SRYCD  =  CONS-201004-KSN14
                                 OR CONS-201004-KSN18
                                 OR CONS-201004-KSN19
                   CONTINUE
                 ELSE
                   ADD     1        TO  IDX1
                   MOVE  TNS-SRYCD  TO  LNK-SROUNYUIN-CHUKSNSRYCD(IDX1)
                 END-IF
                 MOVE   "tbl_tensu"      TO  MCP-TABLE
                 MOVE   "key30"          TO  MCP-PATHNAME
                 PERFORM 910-TENSU-FETCH-SEC
               END-PERFORM
           END-IF
      *    ÅÀ¿ô¥Þ¥¹¥¿¥¯¥í¡¼¥º
           MOVE     "tbl_tensu"       TO  MCP-TABLE
           MOVE     "key30"           TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
      *
       210-CHUKSN-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-5000-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                      SYS-5000-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-READ-EXT.
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE  MCPDATA-REC   TO  PTINF-REC
                   MOVE  ZERO          TO  FLG-PTINF
               ELSE
                   INITIALIZE              PTINF-REC
                   MOVE  1             TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE     "tbl_ptinf"        TO  MCP-TABLE
           MOVE     "key"              TO  MCP-PATHNAME
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
      *    ¿ÇÎÅ²ñ·×¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-SRYACCT-SELECT-SEC        SECTION.
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-SRYACCT
           ELSE
               MOVE    1                  TO  FLG-SRYACCT
           END-IF
      *
           .
       910-SRYACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ñ·×¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-SRYACCT-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  SRYACCT-REC
               MOVE    ZERO               TO  FLG-SRYACCT
           ELSE
               INITIALIZE                     SRYACCT-REC
               MOVE    1                  TO  FLG-SRYACCT
           END-IF
      *
           .
       910-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°ÙÆÉ¹þ
      *****************************************************************
       910-SRYACT-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       910-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¡¼¸¡º÷
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
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               MOVE    ZERO                TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                      HKNCOMBI-REC
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¡¼ÆÉ¹þ¡Ê¼ç¥­¡¼¡Ë
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
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
           MOVE     "tbl_tensu"            TO  MCP-TABLE
           MOVE     "key"                  TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *     
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-TENSU-SELECT-SEC        SECTION.
      *
           MOVE    TNS-TENSU-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING  MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
           IF      MCP-RC                 =   ZERO
               MOVE    ZERO               TO  FLG-TENSU
           ELSE
               MOVE    1                  TO  FLG-TENSU
           END-IF
      *
           .
       910-TENSU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-TENSU-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"              TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING  MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
           IF      MCP-RC                 =   ZERO
               MOVE    MCPDATA-REC        TO  TNS-TENSU-REC
               MOVE    ZERO               TO  FLG-TENSU
           ELSE
               INITIALIZE                     TNS-TENSU-REC
               MOVE    1                  TO  FLG-TENSU
           END-IF
      *
           .
       910-TENSU-FETCH-EXT.
           EXIT.
      *     
      *****************************************************************
      *    Æþ±¡Î¬¾Î¥Þ¥¹¥¿¡¼ÆÉ¹þ¡Ê¼ç¥­¡¼¡Ë
      *****************************************************************
       910-NYURYAKUSHO-READ-SEC        SECTION.
      *
           MOVE    NYURYAKUSHO-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_nyuinryakusho" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_nyuinryakusho" TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  NYURYAKUSHO-REC
                   MOVE    ZERO                TO  FLG-NYURYAKUSHO
               ELSE
      *            µßÌ¿µßµÞÆþ±¡ÎÁ£±¡Ê£´Æü°Ê¾å£·Æü°ÊÆâ¡Ë
      *            µßÌ¿µßµÞÆþ±¡ÎÁ£²¡Ê£´Æü°Ê¾å£·Æü°ÊÆâ¡Ë
      *            ¤ÎÆþ±¡Î¬¾Î¥Æ¡¼¥Ö¥ëÂÐ±þ
                   IF      NYURYAKUSHO-SRYCD   =  "190128610"  OR
                                                  "190128710"
                       MOVE   "20080401"  TO  NYURYAKUSHO-YUKOSTYMD
                       MOVE   "99999999"  TO  NYURYAKUSHO-YUKOEDYMD
                       EVALUATE  NYURYAKUSHO-SRYCD
                          WHEN   "190128610"
                              MOVE  "µßÌ¿£±" TO NYURYAKUSHO-RYAKUSHO
                          WHEN   "190128710"
                              MOVE  "µßÌ¿£²" TO NYURYAKUSHO-RYAKUSHO
                       END-EVALUATE
                       MOVE   ZERO          TO  FLG-NYURYAKUSHO
                   ELSE
                       MOVE    1            TO  FLG-NYURYAKUSHO
                       INITIALIZE               NYURYAKUSHO-REC
                   END-IF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-NYURYAKUSHO
               INITIALIZE                  NYURYAKUSHO-REC
           END-IF
      *
           MOVE     "tbl_nyuinryakusho"    TO  MCP-TABLE
           MOVE     "key"                  TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       910-NYURYAKUSHO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¥¯¥í¡¼¥º½èÍý
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
