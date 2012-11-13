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
       PROGRAM-ID.             ORCR1010.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¥ì¥»¥×¥È
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¸ø³²¥ì¥»¥×¥È¡ÊÆþ±¡³°¡Ë
      *                      ¿ÇÎÅ¶èÊ¬ÊÌÅÀ¿ô½¸·×
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  07/11/16    NACL¡ÝÆ£¸¶    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  04.02.01    NACL-ÌçÏÆ    08/03/24  Ê¿À®£²£°Ç¯£´·î²þÀµÂÐ±þ
      *                                     ¡ÊÆÃµ­»ö¹àÄÉ²ÃÂÐ±þ¡§£²£°¡Ë
      *                                     ¡ÊÇ¾·ì´É¥ê¥Ï¡Ê£³¡Ë¥³¡¼¥ÉÄÉ²Ã¡Ë
      *                                     ¡Ê²ðÃ£¸£°úÈ¯¾ÉÆüµ­ºÜÇÑ»ß¡Ë
      *                                     ¡Ê½é¿Ç¡ÊÌë´Ö¡¦ÁáÄ«Åù¡Ë²Ã»»ÄÉ²Ã¡Ë
      *                                     ¡Ê¿ÇÎÅ¶èÊ¬£¶£´¡§ÉÂÍý¿ÇÃÇÂÐ±þ¡Ë
      *                                     ¡ÊºßÂð¥³¡¼¥ÉÄÉ²Ã¡Ë
      *                                     ¡ÊºÞ»Ù±çµ­ºÜÂÐ¾Ý¥³¡¼¥É½¤Àµ¡Ë
      *                                     ¡Ê¾®»ùÆÃÄê¼À´µ¥«¥¦¥ó¥»¥ê¥ó¥°ÎÁ
      *                                       ¤Î¼«Æ°µ­ºÜÂÐ±þ¡Ë
      *                                     ¡Êºß»ÙÉÂ¼«Æ°µ­ºÜÂÐ±þ¡Ë
      *  04.02.02    NACL-ÌçÏÆ    08/06/03  ¥Í¥Ö¥é¥¤¥¶¡¼²Ã»»ÊÔ½¸½¤Àµ
      *  04.02.03    NACL-ÌçÏÆ    08/06/20  ºßÂðÊÔ½¸½¤Àµ
      *  04.02.04    NACL-ÌçÏÆ    08/11/06  Èé²¼¶ÚÆù¡¢ÀÅÌ®Ãí¼Í¤Îµ­ºÜÊÑ¹¹ÂÐ±þ
      *
      *  04.03.01    NACL-ÌçÏÆ    08/07/08  ¿Ç¶è£±£´¤Î¥³¥á¥ó¥È¤Î¤ß¤ÎºÞ
      *                                     ÊÔ½¸ÂÐ±þ
      *  04.03.02    NACL-ÌçÏÆ    08/07/14  ²¾¥ì¥»ºîÀ®¡Ê±¡³°½èÊý´Þ¤à¡ËÂÐ±þ
      *                                     ÅêÌôºÞÅÀ£°¡Ê±¡³°Åù¡Ëµ­ºÜ
      *  04.03.03    NACL-ÌçÏÆ    08/12/01  ºßÂð¡Ê±¡³°Ê¬¡ËÊÔ½¸½¤Àµ
      *  04.03.04    NACL-ÌçÏÆ    08/12/16  Ê¿À®£²£±Ç¯£±·î²þÀµÂÐ±þ
      *  04.03.05    NACL-ÌçÏÆ    09/01/16  ½èÊýÍó¶â³ÛÊÔ½¸½¤Àµ
      *
      *  04.04.01    NACL-ÌçÏÆ    08/12/25  Êñ³çÂÐ±þ
      *  04.04.02    NACL-ÌçÏÆ    09/02/04  ¸¡ºº¤Î¥³¥á¥ó¥È¼«Æ°µ­ºÜÂÐ±þ
      *  04.04.03    NACL-ÌçÏÆ    09/04/13  Ê¿À®£²£±Ç¯£µ·î²þÀµÂÐ±þ
      *  04.04.04    NACL-ÌçÏÆ    09/12/04  ºÞ¤Þ¤È¤á¹àÌÜÃÍ¤Î·×»»½¤Àµ
      *  04.04.05    NACL-ÌçÏÆ    09/12/09  ½ÐÍè¹â»»Äê¡ÊºÞ¡Ë¤ÎÂÐ±þ
      *  04.04.06    NACL-ÌçÏÆ    10/01/13  ºÞ¤Þ¤È¤á¹àÌÜÃÍ¤Î·×»»½¤Àµ
      *  04.04.07    NACL-ÌçÏÆ    10/01/29  ÆâÉþÌôºÞÄþ¸ºÊÔ½¸ÂÐ±þ
      *  04.04.08    NACL-ÌçÏÆ    10/03/12  ÆâÉþÌôºÞÄþ¸ºÊÔ½¸ÂÐ±þ
      *  04.04.09    NACL-ÌçÏÆ    10/03/23  Æ±ÆüÅÅÏÃºÆ¿Ç¤Î²ó¿ôµ­ºÜÂÐ±þ
      *  04.04.10    NACL-ÌçÏÆ    10/03/25  ²þÀµÂÐ±þ
      *                                     ¡ÊÃÏ°è°åÎÅ¹×¸¥²Ã»»µ­ºÜÊÔ½¸¡Ë
      *                                     ¡ÊÌÀºÙ½ñÈ¯¹ÔÂÎÀ©Åù²Ã»»µ­ºÜÊÔ½¸¡Ë
      *  04.04.11    NACL-ÌçÏÆ    10/03/25  ²þÀµÂÐ±þ
      *                                     ¡ÊÍ½Ìó¥³¡¼¥ÉÆÉ¤ßÈô¤Ð¤·ÊÔ½¸¡Ë
      *  04.04.12    NACL-ÌçÏÆ    10/03/29  ²þÀµÂÐ±þ
      *                                     ¡Ê²èÁü¿ÇÃÇ£²²óÌÜ°Ê¹ß¸º»»ÊÔ½¸¡Ë
      *                                     ¡Ê¹³°­À­¼ðáçºÞ½èÊý´ÉÍý²Ã»»ÊÔ½¸¡Ë
      *                                     ¡Ê¥³¡¼¥ÉÄÉ²Ã¡Ë
      *  04.04.13    NACL-ÌçÏÆ    10/04/06  ²þÀµÂÐ±þ
      *                                     ¡Ê¥³¥á¥ó¥È¼«Æ°µ­ºÜ¡Ë
      *  04.04.14    NACL-ÌçÏÆ    10/12/10  ºÞ¤Þ¤È¤á¹àÌÜÃÍ¤Î·×»»½¤Àµ
      *
      *  04.05.01    NACL-Æ£¸¶    09/07/14  ¾®¿ôÅÀ°Ê²¼£µ·åÂÐ±þ
      *  04.05.02    NACL-ÌçÏÆ    09/08/17  ¾®¿ôÅÀ°Ê²¼£µ·åÂÐ±þ
      *  04.05.03    NACL-ÌçÏÆ    09/09/24  ¥ì¥»¥×¥Èµ­ºÜ¤·¤Ê¤¤¥³¥á¥ó¥ÈÂÐ±þ
      *  04.05.04    NACL-ÌçÏÆ    09/11/19  ÌôºÞ»»Äê¤Ê¤·»þ¤ÎÅ¦Í×ÍóÆ±°ìºÞ
      *                                     ¤Þ¤È¤áµ­ºÜÂÐ±þ
      *  04.05.05    NACL-ÌçÏÆ    10/01/22  ¼£¸³ÂÐ±þ
      *  04.05.06    NACL-ÌçÏÆ    11/06/21  ²èÁü¿ÇÃÇ¡¦ÅÅ»Ò²èÁü´ÉÍý²Ã»»¤Î
      *                                     É½ÍÑÈÖ¹æÊÔ½¸
      *  04.05.07    NACL-ÌçÏÆ    11/12/16  ´ïºà¤Î¶â³Û¤ÈÃ±°Ì¤ÎÂÐ±þ
      *  04.05.08    NACL-ÌçÏÆ    12/02/13  ²ó¿ôÍóÊÔ½¸½¤Àµ¡Ê½èÊý¡¢ËãÆÇ¡Ë
      *  04.05.09    NACL-ÌçÏÆ    12/04/02  Ê¿À®£²£´Ç¯£´·î²þÀµ
      *                                     ¼ÂÆü¿ô¥«¥¦¥ó¥ÈÂÐ±þ
      *                                     ¡ÖºÆ¿Ç¡×¤ÎÂÐ±þ
      *                                     ¡ÖºßÂð¡×¤ÎÂÐ±þ
      *                                     ¡Ö½èÃÖ¡×¤ÎÂÐ±þ
      *                                     ¡Ö²èÁü¿ÇÃÇ¡×¤ÎÂÐ±þ
      *                                     ¡Ö¤½¤ÎÂ¾¡×¤ÎÂÐ±þ
      *                                     ¥³¥á¥ó¥Èº¹¹þ
      *  04.05.10    NACL-ÌçÏÆ    12/05/23  ÊÝ¸±Íó¡Ý°ìÉôÉéÃ´¶âÌ¤µ­ºÜÂÐ±þ
      *  04.05.11    NACL-ÌçÏÆ    12/08/10  £¸£°¤½¤ÎÂ¾Íó¤ÎÅÀ¿ô½¸·×½¤Àµ
      *  04.05.12    NACL-ÌçÏÆ    12/09/19  ÊÝ¸±Íó¡Ý°ìÉôÉéÃ´¶âÍó¶â³ÛÃÖ´¹ÂÐ±þ
      *  04.05.13    NACL-ÌçÏÆ    12/11/08  ¸øÈñÍó¡Ý°ìÉôÉéÃ´¶âÍó¶â³ÛÃÖ´¹ÂÐ±þ
      *
      *  04.06.01    NACL-ÌçÏÆ    10/07/02  ¾®»ùÆÃÄê¼À´µ¥«¥¦¥ó¥»¥ê¥ó¥°ÎÁ¤Î
      *                                     ¥ì¥»¥×¥È¥³¥á¥ó¥È¼«Æ°µ­ºÜ¡Ê½ÅÊ£»þ¡Ë
      *                                     ÂÐ±þ
      *  04.06.02    NACL-ÌçÏÆ    12/10/15  ¿ÇÎÅ¹Ô°ÙÆþÎÏ¤·¤¿¥³¥á¥ó¥È¤Î¤ß¤ÎºÞ¤Î
      *                                     ¥ì¥»¥×¥Èµ­ºÜÂÐ±þ
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE100-FILE     ASSIGN  RECE100PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE100-KEY
                                   FILE    STATUS  IS  STS-RECE100.
           SELECT  RECE120-FILE     ASSIGN  RECE120PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE120-KEY
                                   FILE    STATUS  IS  STS-RECE120.
           SELECT  RECE130-FILE     ASSIGN  RECE130PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE130-KEY
                                   FILE    STATUS  IS  STS-RECE130.
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE100-FILE.
       01  RECE100-REC. 
           COPY    "CPRCF100.INC".
      *
       FD  RECE120-FILE.
       01  RECE120-REC. 
           COPY    "CPRCF120.INC".
      *
       FD  RECE130-FILE.
       01  RECE130-REC. 
           COPY    "CPRCF130.INC".
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *     ¥ì¥»¥×¥ÈÌÀºÙ¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE100//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE120//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE130//.
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  STS-AREA.
           03  STS-RECE100         PIC X(02).
           03  STS-RECE120         PIC X(02).
           03  STS-RECE130         PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-SRYACT                              PIC 9(01).
           03  FLG-TENSU                               PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-ZAITAKU                             PIC 9(01).
           03  FLG-OSINKBN                             PIC 9(01).
           03  FLG-RECE100                             PIC 9(01).
           03  FLG-KANJI                               PIC 9(01).
           03  FLG-SP                                  PIC 9(01). 
           03  FLG-PTCOM                               PIC 9(01).
           03  FLG-ZANHAIKI                            PIC 9(01).
           03  FLG-FORMAL-KISAI                        PIC 9(01).
           03  FLG-RECE130                             PIC 9(01).
           03  FLG-REWRITE                             PIC 9(01).
           03  FLG-SHOSAISIN                           PIC 9(01).       
           03  FLG-SHOSAISIN-GEN                       PIC 9(01).
           03  FLG-SET                                 PIC 9(01).
           03  FLG-HOUSIJI-COM                         PIC 9(01).
           03  FLG-OSIN-COM                            PIC 9(01).
           03  FLG-ZAIHOU-COM                          PIC 9(01).
           03  FLG-SYOUNI-COM                          PIC 9(01).
           03  FLG-HOUTEN-COM                          PIC 9(01).
           03  FLG-SYOBYO-COM                          PIC 9(01).
           03  FLG-RYOYO-COM                           PIC 9(01).
           03  FLG-SYODOU-COM                          PIC 9(01).
           03  FLG-SAIDOU-COM                          PIC 9(01).
           03  FLG-NIKOCHIN-COM                        PIC 9(01).
           03  FLG-ZENSOKU-COM                         PIC 9(01).
           03  FLG-KENSA-COM                           PIC 9(01).
           03  FLG-RINYAK                              PIC 9(01).
           03  FLG-WRITE05                             PIC 9(01).
           03  FLG-WRITE05X                            PIC 9(01).
           03  FLG-TAHOKEN                             PIC 9(01).
           03  FLG-NEBU                                PIC 9(01).
           03  FLG-HENSYU-NG                           PIC 9(01).
      *
      *    ´ðËÜÅÀ¿ô¤ÎÈóÉ½¼¨È½ÄêÍÑ¥Õ¥é¥°
       01  FLG-KHNTEN-AREA.
           03  FLG-KHNTEN2                             PIC 9(01).
           03  FLG-KHNTEN3                             PIC 9(01).
           03  FLG-KHNTEN4                             PIC 9(01).
           03  FLG-KHNTEN5                             PIC 9(01).
           03  FLG-KHNTEN6                             PIC 9(01).
           03  FLG-KHNTEN17                            PIC 9(01).
           03  FLG-KHNTEN20                            PIC 9(01).
           03  FLG-KHNTEN21                            PIC 9(01).
      *
      *    ºÆ¿ÇÎÁ´ðËÜÅÀ¿ô²Ã»»¥Õ¥é¥°
       01  FLG-SAISIN-AREA.
           03  FLG-NYUYOJI                             PIC 9(01).
           03  FLG-YOJI                                PIC 9(01).
           03  FLG-YOJI-ARI                            PIC 9(01).
           03  FLG-SAISIN-ARI                          PIC 9(01).
           03  FLG-KOKENKSN-ARI                        PIC 9(01).
           03  FLG-MESAIKSN-ARI                        PIC 9(01).
           03  FLG-JIKANTAI1-ARI                       PIC 9(01).
           03  FLG-JIKANTAI3-ARI                       PIC 9(01).
      *    ÆýÍÄ»ù²Ã»»¥«¥¦¥ó¥È
       01  CNT-SAISIN-AREA.
           03  CNT-NYUYOJI                             PIC 9(02).
           03  CNT-NYUYOJI2                            PIC 9(02).
           03  CNT-KOKENKSN                            PIC 9(02).
           03  CNT-MESAIKSN                            PIC 9(02).
           03  CNT-JIKANTAI1                           PIC 9(02).
           03  CNT-JIKANTAI3                           PIC 9(02).
           03  CNT-SAISIN-DOUJITU                      PIC 9(02).
           03  CNT-SAISIN-ETC                          PIC 9(02).
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-IN-CANCEL                           PIC 9(06).
           03  CNT-IN-COMMIT                           PIC 9(06).
           03  CNT-IN                                  PIC 9(06).
           03  CNT-OUT1                                PIC 9(06).
           03  CNT-OUT2                                PIC 9(06).
           03  CNT-OUT3                                PIC 9(06).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                                     PIC 9(03).
           03  IDX2                                    PIC 9(03).
           03  IDX3                                    PIC 9(03).
           03  IDX4                                    PIC 9(03).
           03  IDXA                                    PIC 9(01).
           03  IDXB                                    PIC 9(03).
           03  IDXX                                    PIC 9(02).
           03  IDY                                     PIC 9(03).
           03  IDW                                     PIC 9(03).
           03  IDW2                                    PIC 9(03).
           03  IDW3                                    PIC 9(03).
           03  IDW4                                    PIC 9(03).
           03  IDW5                                    PIC 9(03).
           03  IDW6                                    PIC 9(03).
           03  IDV                                     PIC 9(03).
           03  IDU                                     PIC 9(03).
           03  NUM-IDX                                 PIC 9(01).
           03  ACCT-IDX                                PIC 9(02).       
           03  SYSKAN-IDX2                             PIC 9(03).
           03  FUKA-IDX                                PIC 9(03).
      *
      *    ¥Ñ¥¹Ì¾ÎÎ°è
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
      *    ºÞ¾ðÊóÂàÈòÎÎ°è
       01  WK-ZAIKAISU                                 PIC S9(07).
       01  WK-ZAITEN                                   PIC S9(08).
       01  WK-SYUTEN                                   PIC S9(08).
       01  WK-KIZAITEN                                 PIC S9(08).
       01  WK-YKZTEN                                   PIC S9(08).
      *
      *    ÅÀ¿ô¥Æ¡¼¥Ö¥ë¸¡º÷ÍÑÂÐ¾ÝÆüÎÎ°è
       01  WK-TAIHI-AREA.
           03  WK-TAIHI-DAY                            PIC 9(02).
           03  WK-TAIHI-FLG                            PIC 9(01).
           03  WK-TAIHI-SRYYMD.
               05  WK-TAIHI-SRYYM                      PIC 9(06).
               05  WK-TAIHI-SRYD                       PIC 9(02).
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿¾ðÊóÂàÈòÎÎ°è
       01  WRK-TNS-TAIHI-AREA.
           03  WRK-TNS-TAIHI-TABLE                     OCCURS  5.
               05  WRK-TNS-CHUKSNTUBAN                 PIC X(01).
               05  WRK-TNS-DATAKBN                     PIC 9(01).
               05  WRK-TNS-TIMEKSNKBN                  PIC 9(01).
               05  WRK-TNS-TEN                         PIC 9(09)V99.    
               05  WRK-TNS-GAITENTTLKBN                PIC 9(03).
               05  WRK-TNS-OSINKBN                     PIC 9(01).
               05  WRK-TNS-KNSJISKBN                   PIC 9(01).
               05  WRK-TNS-SRYCD                       PIC X(09).
      *
      *    ¼ÂÆü¿ô¥«¥¦¥ó¥ÈÍÑÎÎ°è                                         
       01  TBL-JNISSU.
           03  TBL-JNISSU-DAY                          PIC X(01)
                                                       OCCURS   31.
      *    ¼ÂÆü¿ô¸º¥«¥¦¥ó¥ÈÍÑÎÎ°è                                         
       01  TBL-JNISSU-GEN.
           03  TBL-JNISSU-DAY-GEN                      PIC X(01)
                                                       OCCURS   31.
      *    »ÜÀß´ð½à¾ðÊóÂàÈòÍÑÎÎ°è
       01  TBL-SYS1006-T.
           03  TBL-SYS1006-OCC                         OCCURS  100.
               05  TBL-SYS10062                        PIC X(610).
               05  TBL-SYS10067                        PIC X(610).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                      PIC X(06).
               05  WRK-PARA-TERMID                     PIC X(16).
               05  WRK-PARA-SYSYMD                     PIC X(08).
               05  WRK-PARA-JOBID                      PIC 9(07).
               05  WRK-PARA-SHELLID                    PIC X(08).
           03  WRK-PARA-PRTKBN                         PIC X(01).
           03  WRK-PARA-HOSPNUM                        PIC 9(02).
      *
           03  WRK-SYS-SRYYMD                          PIC X(08).
      *
           03  WRK-ZENKAKU                             PIC X(80).
           03  WRK-NUM.
               05  WRK-NUM-X                           PIC X(10).
               05  FILLER                              PIC X(70).
           03  WRK-NUM-9                               PIC 9(10). 
           03  WRK-STIDX                               PIC 9(02).
           03  WRK-EDIDX                               PIC 9(02).
      *
           03  WRK-RECEERR                             PIC X(200).
      *
           03  WRK-TANICD                              PIC 9(03).
           03  WRK-TANIMOJISU                          PIC 9(01).
           03  WRK-TANINAME                            PIC X(24).
           03  WRK-SRYSURYO                            PIC 9(05)V9(05).
           03  WRK-SRYSURYO2                           PIC 9(05).
           03  WRK-SRYSURYO3                           PIC 9(05).
           03  WRK-KZI058-TEN                          PIC 9(09)V99.
           03  WRK-KZI058-TANICD                       PIC 9(03).
           03  WRK-KZI058-TANIMOJISU                   PIC 9(01).
           03  WRK-KZI058-TANINAME                     PIC X(24).
           03  WRK-RENNUM                              PIC 9(02).
           03  WRK-RENNUM2                             PIC 9(02).
      *
           03  WRK-SRYCD                               PIC 9(09).
      *
           03  WRK-SISETU613                           PIC 9(01).
           03  WRK-SISETU695                           PIC 9(01).
           03  WRK-SISETU3055                          PIC 9(01).
           03  WRK-SISETU3056                          PIC 9(01).
           03  WRK-SISETU3057                          PIC 9(01).
           03  WRK-SISETU3058                          PIC 9(01).
           03  WRK-SISETU3168                          PIC 9(01).
           03  WRK-SISETU3169                          PIC 9(01).
      *
           03  WRK-FILMTEN                             PIC 9(07)V9(02).
           03  WRK-FILMTEN2                            PIC 9(07)V9(02).
      *
      *
      *    ¿ô»úÈ¾³ÑÁ´³ÑÊÑ´¹ÍÑ     
           03  WRK-CHAR-IDX                    PIC 9(02).
           03  WRK-CHAR-HENKAN-AREA.
               05  WRK-CHAR-X                  PIC X(20).
               05  WRK-CHAR-N                  PIC X(40).
               05  WRK-CHAR-CNT                PIC 9(02).
               05  WRK-CHAR-N-CNT              PIC 9(02).
      *     
      *    Ã±²ÁÊÔ½¸ÍÑ     
           03  WRK-TANKA                       PIC 9(09)V9(02).
           03  WRK-TANKA-R  REDEFINES  WRK-TANKA.
               05  WRK-TANKA-S1                PIC 9(09).
               05  WRK-TANKA-S2                PIC 9(02).
           03  WRK-TANKA-X.
               05  WRK-TANKA-Z1                PIC ZZZZZZZZ9.
               05  WRK-TANKA-Z2                PIC X(01).
               05  WRK-TANKA-Z3                PIC ZZ.
      *
      *
           03  WRK-SRYCDTOTAL9                 PIC 9(14).
           03  WRK-SRYCDTOTALX   REDEFINES     WRK-SRYCDTOTAL9
                                               PIC X(14).
           03  WRK-ZAINUM                      PIC 9(08).
      *
      ***  03  WRK-FUKA-KINGAKU                PIC 9(09).
           03  WRK-FUKA-KINGAKU                PIC 9(13)V9(05).
           03  WRK-FUKA-VALUE                  PIC 9(01).
      *
           03  WRK-TOTALTEN1                   PIC 9(08).
           03  WRK-TOTALTEN2                   PIC 9(08).
      *
           03  WRK-SURYOU-COUNT                PIC 9(09).
      *
      *---------------------------------------------------------------
      *  ²ó¿ô¸º»»ÍÑÂàÈòÎÎ°è
      *
      *    ÆÃÄê¼À´µ½èÊý´ÉÍý²Ã»»¡Ê½èÊýÎÁ¡Ë
           03  WRK-TOKUTEI-SKAN                        PIC 9(03).
      *    ½èÊýÎÁ¡ÊÆýÍÄ»ù¡Ë²Ã»»
           03  WRK-NYUYOUJI-SKAN                       PIC 9(03).
      *    ÆÃÄê¼À´µ½èÊý´ÉÍý²Ã»»¡Ê½èÊý¤»¤óÎÁ¡Ë
           03  WRK-TOKUTEI-SKAN2                       PIC 9(03).
      *    ½èÊý¤»¤ó¡ÊÆýÍÄ»ù¡Ë²Ã»»
           03  WRK-NYUYOUJI-SHOKAN                     PIC 9(03).
      *    ½èÊýÎÁ¡ÊËã¡¦¸þ¡¦ÆÇ¡¦³Ð¡Ë
           03  WRK-MADOKU-SHOHO                        PIC 9(03).
      *    Ä¹´üÅêÌô²Ã»»¡Ê½èÊýÎÁ¡Ë
           03  WRK-CYOKI-SKAN                          PIC 9(03).
      *    Ä¹´üÅêÌô²Ã»»¡Ê½èÊý¤»¤óÎÁ¡Ë
           03  WRK-CYOKI-SKAN2                         PIC 9(03).
      *    ¹³°­À­¼ðáçºÞ½èÊý´ÉÍý²Ã»»¡Ê½èÊýÎÁ¡Ë
           03  WRK-AKUSEI-SKAN                         PIC 9(03).
      *    ¹³°­À­¼ðáçºÞ½èÊý´ÉÍý²Ã»»¡Ê½èÊý¤»¤óÎÁ¡Ë
           03  WRK-AKUSEI-SKAN2                        PIC 9(03).
      *    °ìÈÌÌ¾½èÊý²Ã»»¡Ê½èÊý¤»¤óÎÁ¡Ë
           03  WRK-IPPAN-SKAN2                         PIC 9(03).
      *---------------------------------------------------------------
      *
      *    ¸ÇÄêÃÍ
       01  CONST-AREA.
      *    ¥Õ¥ê¡¼¥³¥á¥ó¥È
           03  CONS-COMMENT    PIC X(09)   VALUE   "810000001".
      *    ·ÑÂ³´ÉÍý²Ã»»
           03  CONS-KEIZOKU1   PIC X(09)   VALUE   "112007170".
           03  CONS-KEIZOKU2   PIC X(09)   VALUE   "112702270".
      *    ½èÊý¤»¤óÎÁ
           03  CONS-SHOHOSEN1 PIC X(09)   VALUE    "120002710".
           03  CONS-SHOHOSEN2 PIC X(09)   VALUE    "120002810".
           03  CONS-SHOHOSEN3 PIC X(09)   VALUE    "120002910".
           03  CONS-SHOHOSEN4 PIC X(09)   VALUE    "120003010".
      *    ½èÊý¤»¤ó¡ÊÆýÍÄ»ù¡Ë²Ã»»
           03  CONS-SHOHOKSN1  PIC X(09)   VALUE   "120002470".
           03  CONS-SHOHOKSN2  PIC X(09)   VALUE   "120002570".
      *    Ä¹´üÅêÌô²Ã»»¡Ê½èÊý¤»¤óÎÁ¡Ë
           03  CONS-SHOHOKSN3  PIC X(09)   VALUE   "120003270".
      *    ¹³°­À­¼ðáçºÞ½èÊý´ÉÍý²Ã»»¡Ê½èÊý¤»¤óÎÁ¡Ë
           03  CONS-SHOHOKSN4  PIC X(09)   VALUE   "120003470".
      *    °ìÈÌÌ¾½èÊý²Ã»»¡Ê½èÊý¤»¤óÎÁ¡Ë
           03  CONS-SHOHOKSN5  PIC X(09)   VALUE   "120003570".
      *    ÌôºÞÎÁÄþ¸º¡Ê£¹£°¡¿£±£°£°¡Ë¡ÊÆâÉþÌô¡Ë
           03  CONS-YAKTEIGEN  PIC X(09)   VALUE   "630010002".
      *    Ê´ºÕ¡Ê¥³¥á¥ó¥È¡Ë
           03  CONS-FUNSAI-COMMENT PIC X(09) VALUE "099209901".
      *    ºÎ·ì¡Ê£Â¡Ý£Á¡Ë
           03  CONS-SAIKETSU1      PIC X(09) VALUE "160101210".
      *    ºÎ·ì¡Ê£Â¡Ý£Ö¡Ë
           03  CONS-SAIKETSU2      PIC X(09) VALUE "160095710".
      *    ºÎ·ì¡Ê£Â¡Ý£Ã¡Ë
           03  CONS-SAIKETSU3      PIC X(09) VALUE "160095810".
      *
      *    ÉÂ¾²¿ôÊÔ½¸¥Õ¥é¥°ÊÔ½¸¤Î°Ù¤Î¥³¡¼¥É¡ÊÆÃÄê¼À´µÎÅÍÜ»ØÆ³ÎÁÅù¡Ë
           03  CONS-SIDORYO1       PIC X(09) VALUE "113001810".
           03  CONS-SIDORYO2       PIC X(09) VALUE "113001910".
           03  CONS-SIDORYO3       PIC X(09) VALUE "113002010".
           03  CONS-SIDORYO4       PIC X(09) VALUE "113700610".
           03  CONS-SIDORYO5       PIC X(09) VALUE "113700710".
           03  CONS-SIDORYO6       PIC X(09) VALUE "113700810".
      *
      *    ºß»Ù±ç¥Õ¥é¥°ÊÔ½¸¤Î°Ù¤Î¥³¡¼¥É
           03  CONS-ZAISIEN1       PIC X(09) VALUE "114011570".
           03  CONS-ZAISIEN2       PIC X(09) VALUE "114011670".
           03  CONS-ZAISIEN3       PIC X(09) VALUE "114011770".
      *
      *    ºÆ¿Ç¡ÊÆýÍÄ»ù¡Ë²Ã»»
           03  CONS-NYUYOJI        PIC X(09) VALUE "112000970".
      *    ºÆ¿Ç¡ÊÆýÍÄ»ù¡Ë¡Ê»þ´Ö³°Åù¡Ë²Ã»»
           03  CONS-NYUYOJI-JIKAN  PIC X(09) VALUE "112006970".
      *    ºÆ¿Ç¡ÊÍÄ»ù¡Ë²Ã»»
           03  CONS-YOJI           PIC X(09) VALUE "112001070".
      *    ºÆ¿Ç¡ÊÍÄ»ù¡Ë¡Ê»þ´Ö³°Åù¡Ë²Ã»»
           03  CONS-YOJI-JIKAN     PIC X(09) VALUE "112007070".
      *
      *    ³°Íè¿ÇÎÅÎÁ¡ÊÆýÍÄ»ù¡Ë²Ã»»
           03  CONS-GNYUYOJI       PIC X(09) VALUE "112006270".
      *    ³°Íè¿ÇÎÅÎÁ¡ÊÆýÍÄ»ù¡Ë¡Ê»þ´Ö³°Åù¡Ë²Ã»»
           03  CONS-GNYUYOJI-JIKAN PIC X(09) VALUE "112007270".
      *    ³°Íè¿ÇÎÅÎÁ¡ÊÍÄ»ù¡Ë²Ã»»
           03  CONS-GYOJI          PIC X(09) VALUE "112006370".
      *    ³°Íè¿ÇÎÅÎÁ¡ÊÍÄ»ù¡Ë¡Ê»þ´Ö³°Åù¡Ë²Ã»»
           03  CONS-GYOJI-JIKAN    PIC X(09) VALUE "112007370".
      *
      *    »þ´Ö³°ÂÐ±þ²Ã»»£±
           03  CONS-JIKANTAI1      PIC X(09) VALUE "112016070".
      *    ÃÏ°è°åÎÅ¹×¸¥²Ã»»
           03  CONS-KOKENKSN       PIC X(09) VALUE "112015670".
      *    »þ´Ö³°ÂÐ±þ²Ã»»£³
           03  CONS-JIKANTAI3      PIC X(09) VALUE "112016170".
      *    ÌÀºÙ½ñÈ¯¹ÔÂÎÀ©Åù²Ã»»
           03  CONS-MESAIKSN       PIC X(09) VALUE "112015770".
      *
      *    ¥Õ¥£¥ë¥àÎÁ¡ÊÆýÍÄ»ù¡Ë²Ã»»
           03  CONS-FILM-YOUJI-KASAN  
                                   PIC X(09) VALUE "799990070".
      *    »ÀÁÇÊäÀµÎ¨
           03  CONS-SANSO          PIC X(09) VALUE "770020070".
      *    ¹âµ¤°µ»ÀÁÇ²Ã»»
           03  CONS-SANSO2         PIC X(09) VALUE "770030070".
      *    »ÄÎÌÇÑ´þ
           03  CONS-ZANHAIKI       PIC X(09) VALUE "099309901".
      *
      *
      *    ËýÀ­¤È¤¦ÄË¼À´µ
           03  CONS-MANSEI           PIC X(09)  VALUE  "113006510".
      *    ½ýÉÂ¼êÅö¶â°Õ¸«½ñ¸òÉÕÎÁ
           03  CONS-SYOBYO-COMMENT   PIC X(09)  VALUE  "180000710".
      *    ½ýÉÂ¼êÅö¶â°Õ¸«½ñ¸òÉÕÇ¯·îÆü
           03  CONS-SYOBYO2-COMMENT  PIC X(09)  VALUE  "840000011".
      *    ¡ÊÁêÂ³¡Ë½ýÉÂ¼êÅö¶â°Õ¸«½ñ¸òÉÕÇ¯·îÆü
           03  CONS-SYOBYO3-COMMENT  PIC X(09)  VALUE  "840000012".
      *    ÎÅÍÜÈñÆ±°Õ½ñ¸òÉÕÎÁ
           03  CONS-RYOYO-COMMENT    PIC X(09)  VALUE  "113004310".
      *    ÎÅÍÜÈñÆ±°Õ½ñ¸òÉÕÇ¯·îÆü
           03  CONS-RYOYO2-COMMENT   PIC X(09)  VALUE  "840000078".
      *    Ë¬Ìä´Ç¸î»Ø¼¨ÎÁ
           03  CONS-HOUSIJI-COMMENT  PIC X(09)  VALUE  "114008010".
      *    Ë¬Ìä´Ç¸î»Ø¼¨ÎÁ»»ÄêÆü
           03  CONS-HOUSIJI2-COMMENT PIC X(09)  VALUE  "840000073".
      *    ¼ê½ÑÆü¥³¥á¥ó¥È
           03  CONS-SHUJUTU-COMMENT  PIC X(09)  VALUE  "840000042".
      *    Æ¬ÉôÃ±½ã£Ã£Ô»£±Æ
           03  CONS-CTMRI-COMMENT1   PIC X(09)  VALUE  "170011710".
      *    ¶í´´Ã±½ã£Ã£Ô»£±Æ
           03  CONS-CTMRI-COMMENT2   PIC X(09)  VALUE  "170011810".
      *    »Í»èÃ±½ã£Ã£Ô»£±Æ
           03  CONS-CTMRI-COMMENT3   PIC X(09)  VALUE  "170011910".
      *    Ç¾Áå£Ã£ÔÂ¤±Æ
           03  CONS-CTMRI-COMMENT4   PIC X(09)  VALUE  "170012110".
      *    ðôÉôÃ±½ã£Ã£Ô»£±Æ
           03  CONS-CTMRI-COMMENT5   PIC X(09)  VALUE  "170015550".
      *    Æ¬ÉôÃ±½ã£Í£Ò£É»£±Æ
           03  CONS-CTMRI-COMMENT6   PIC X(09)  VALUE  "170015210".
      *    ¶í´´Ã±½ã£Í£Ò£É»£±Æ
           03  CONS-CTMRI-COMMENT7   PIC X(09)  VALUE  "170020110".
      *    »Í»èÃ±½ã£Í£Ò£É»£±Æ
           03  CONS-CTMRI-COMMENT8   PIC X(09)  VALUE  "170022110".
      *    £Ã£Ô¡¢£Í£Ò£É¡ÊÆ¬Éô¡Ë¡Ê£²²óÌÜ°Ê¹ß¡Ë
           03  CONS-CTMRI-COMMENT9   PIC X(09)  VALUE  "170022290".
      *    £Ã£Ô¡¢£Í£Ò£É¡Ê¶í´´¡Ë¡Ê£²²óÌÜ°Ê¹ß¡Ë
           03  CONS-CTMRI-COMMENT10  PIC X(09)  VALUE  "170022390".
      *    £Ã£Ô¡¢£Í£Ò£É¡Ê»Í»è¡Ë¡Ê£²²óÌÜ°Ê¹ß¡Ë
           03  CONS-CTMRI-COMMENT11  PIC X(09)  VALUE  "170022490".
      *    Æ¬ÉôÆÃ¼ì£Ã£Ô»£±Æ
           03  CONS-CTMRI-COMMENT12  PIC X(09)  VALUE  "170023010".
      *    ¶í´´ÆÃ¼ì£Ã£Ô»£±Æ
           03  CONS-CTMRI-COMMENT13  PIC X(09)  VALUE  "170023110".
      *    »Í»èÆÃ¼ì£Ã£Ô»£±Æ
           03  CONS-CTMRI-COMMENT14  PIC X(09)  VALUE  "170023210".
      *    Æ¬ÉôÆÃ¼ì£Í£Ò£É»£±Æ
           03  CONS-CTMRI-COMMENT15  PIC X(09)  VALUE  "170023310".
      *    ¶í´´ÆÃ¼ì£Í£Ò£É»£±Æ
           03  CONS-CTMRI-COMMENT16  PIC X(09)  VALUE  "170023410".
      *    »Í»èÆÃ¼ì£Í£Ò£É»£±Æ
           03  CONS-CTMRI-COMMENT17  PIC X(09)  VALUE  "170023510".
      *    £Ã£Ô»£±Æ¡Ê£±£¶ÎóÌ¤Ëþ¥Þ¥ë¥Á¥¹¥é¥¤¥¹·¿µ¡´ï¡Ë
           03  CONS-CTMRI-COMMENT18  PIC X(09)  VALUE  "170028610".
      *    £Ã£Ô»£±Æ¡Ê£¶£´Îó°Ê¾å¥Þ¥ë¥Á¥¹¥é¥¤¥¹·¿µ¡´ï¡Ë
           03  CONS-CTMRI-COMMENT19  PIC X(09)  VALUE  "170033410".
      *    £Í£Ò£É»£±Æ¡Ê£³¥Æ¥¹¥é°Ê¾å¤Îµ¡´ï¡Ë
           03  CONS-CTMRI-COMMENT20  PIC X(09)  VALUE  "170033510".
      *    ºßÂð·ì±ÕÆ©ÀÏÉÑ²ó»ØÆ³´ÉÍýÎÁ
           03  CONS-HINKAI-COMMENT   PIC X(09)  VALUE  "114009410".
      *    ½é¿Ç¡ÊÆ±ÆüÊ£¿ô¿ÇÎÅ²Ê¡Ë
           03  CONS-SYODOU-COMMENT   PIC X(09)  VALUE  "111011810".
      *    Ê£½é»»Äê¿ÇÎÅ²Ê¡§
           03  CONS-SYODOU2-COMMENT  PIC X(09)  VALUE  "830000024".
      *    ¥Ë¥³¥Á¥ó°ÍÂ¸¾É´ÉÍýÎÁ¡Ê½é²ó¡Ë
           03  CONS-NIKOCHIN-COMMENT PIC X(09)  VALUE  "113008310".
      *    ¥Ë¥³¥Á¥ó°ÍÂ¸¾É´ÉÍýÎÁ¡Ê£²²óÌÜ¤«¤é£´²óÌÜ¤Þ¤Ç¡Ë
           03  CONS-NIKOCHIN2-COMMENT PIC X(09) VALUE  "113008410".
      *    ¥Ë¥³¥Á¥ó°ÍÂ¸¾É´ÉÍýÎÁ¡Ê£µ²óÌÜ¡Ë
           03  CONS-NIKOCHIN3-COMMENT PIC X(09) VALUE  "113008510".
      *    ½é²ó»»Äê¡¡¡¡·î¡¡Æü
           03  CONS-NIKOCHIN4-COMMENT PIC X(09) VALUE  "840000097".
      *    ½ÅÅÙÓÃÂ©´µ¼Ô¼£ÎÅ´ÉÍý²Ã»»¡Ê£±·îÌÜ¡Ë
           03  CONS-ZENSOKU-COMMENT  PIC X(09)  VALUE  "113008070".
      *    ½ÅÅÙÓÃÂ©´µ¼Ô¼£ÎÅ´ÉÍý²Ã»»¡Ê£²·îÌÜ°Ê¹ß£¶·îÌÜ¤Þ¤Ç¡Ë
           03  CONS-ZENSOKU2-COMMENT PIC X(09)  VALUE  "113008170".
      *    Âè£±²óÌÜ¼£ÎÅ´ÉÍý¡¡¡¡·î¡¡Æü
           03  CONS-ZENSOKU3-COMMENT PIC X(09)  VALUE  "840000098".
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CONS-SINDAI1-COMMENT  PIC X(09)  VALUE  "180027410".
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë
           03  CONS-SINDAI2-COMMENT  PIC X(09)  VALUE  "180027510".
      **   ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê£±£²£°ÆüÄ¶¡Ë
           03  CONS-SINDAI3-COMMENT  PIC X(09)  VALUE  "180029110".
      **   ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê£±£²£°ÆüÄ¶¡Ë
           03  CONS-SINDAI4-COMMENT  PIC X(09)  VALUE  "180029210".
      **   ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£±¡Ë
           03  CONS-SINDAI5-COMMENT  PIC X(09)  VALUE  "180029910".
      **   ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£²¡Ë
           03  CONS-SINDAI6-COMMENT  PIC X(09)  VALUE  "180030010".
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CONS-KOKYUKI1-COMMENT PIC X(09)  VALUE  "180028010".
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë
           03  CONS-KOKYUKI2-COMMENT PIC X(09)  VALUE  "180028110".
      **   ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê£¸£°ÆüÄ¶¡Ë
           03  CONS-KOKYUKI3-COMMENT PIC X(09)  VALUE  "180029710".
      **   ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê£¸£°ÆüÄ¶¡Ë
           03  CONS-KOKYUKI4-COMMENT PIC X(09)  VALUE  "180029810".
      **   ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£±¡Ë
           03  CONS-KOKYUKI5-COMMENT PIC X(09)  VALUE  "180030510".
      **   ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£²¡Ë
           03  CONS-KOKYUKI6-COMMENT PIC X(09)  VALUE  "180030610".
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CONS-NOUKE1-COMMENT   PIC X(09)  VALUE  "180027610".
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë
           03  CONS-NOUKE2-COMMENT   PIC X(09)  VALUE  "180027710".
      **   Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê£±£´£°ÆüÄ¶¡Ë
           03  CONS-NOUKE3-COMMENT   PIC X(09)  VALUE  "180029310".
      **   Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê£±£´£°ÆüÄ¶¡Ë
           03  CONS-NOUKE4-COMMENT   PIC X(09)  VALUE  "180029410".
      **   Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£±¡Ë
           03  CONS-NOUKE5-COMMENT   PIC X(09)  VALUE  "180030110".
      **   Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£²¡Ë
           03  CONS-NOUKE6-COMMENT   PIC X(09)  VALUE  "180030210".
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë
           03  CONS-NOUKE7-COMMENT   PIC X(09)  VALUE  "180030810".
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÇÑÍÑ¾É¸õ·²¡Ë
           03  CONS-NOUKE8-COMMENT   PIC X(09)  VALUE  "180032410".
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÇÑÍÑ¾É¸õ·²¡Ë
           03  CONS-NOUKE9-COMMENT   PIC X(09)  VALUE  "180032510".
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÇÑÍÑ¾É¸õ·²¡Ë
           03  CONS-NOUKE10-COMMENT  PIC X(09)  VALUE  "180032610".
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡¦ÇÑÍÑ¾É¸õ·²°Ê³°¡Ë
           03  CONS-NOUKE11-COMMENT  PIC X(09)  VALUE  "180033910".
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡¦ÇÑÍÑ¾É¸õ·²¡Ë
           03  CONS-NOUKE12-COMMENT  PIC X(09)  VALUE  "180034010".
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡¦ÇÑÍÑ¾É¸õ·²°Ê³°¡Ë
           03  CONS-NOUKE13-COMMENT  PIC X(09)  VALUE  "180034110".
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡¦ÇÑÍÑ¾É¸õ·²¡Ë
           03  CONS-NOUKE14-COMMENT  PIC X(09)  VALUE  "180034210".
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡¦ÇÑÍÑ¾É¸õ·²°Ê³°¡Ë
           03  CONS-NOUKE15-COMMENT  PIC X(09)  VALUE  "180034310".
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡¦ÇÑÍÑ¾É¸õ·²¡Ë
           03  CONS-NOUKE16-COMMENT  PIC X(09)  VALUE  "180034410".
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CONS-UNDOKI1-COMMENT  PIC X(09)  VALUE  "180027810".
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë
           03  CONS-UNDOKI2-COMMENT  PIC X(09)  VALUE  "180027910".
      **   ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê£±£²£°ÆüÄ¶¡Ë
           03  CONS-UNDOKI3-COMMENT  PIC X(09)  VALUE  "180029510".
      **   ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê£±£²£°ÆüÄ¶¡Ë
           03  CONS-UNDOKI4-COMMENT  PIC X(09)  VALUE  "180029610".
      **   ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£±¡Ë
           03  CONS-UNDOKI5-COMMENT  PIC X(09)  VALUE  "180030310".
      **   ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£²¡Ë
           03  CONS-UNDOKI6-COMMENT  PIC X(09)  VALUE  "180030410".
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CONS-UNDOKI7-COMMENT  PIC X(09)  VALUE  "180032710".
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë
           03  CONS-UNDOKI8-COMMENT  PIC X(09)  VALUE  "180034510".
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë
           03  CONS-UNDOKI9-COMMENT  PIC X(09)  VALUE  "180034610".
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë
           03  CONS-UNDOKI10-COMMENT PIC X(09)  VALUE  "180034710".
      *    ÆñÉÂ´µ¼Ô¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ
           03  CONS-NANBYO-COMMENT   PIC X(09)  VALUE  "180017910".
      *    ¾ã³²»ù¡Ê¼Ô¡Ë¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£¶ºÐÌ¤Ëþ¡Ë
           03  CONS-SYOGAI1-COMMENT  PIC X(09)  VALUE  "180028210".
      *    ¾ã³²»ù¡Ê¼Ô¡Ë¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£¶ºÐ°Ê¾å£±£¸ºÐÌ¤Ëþ¡Ë
           03  CONS-SYOGAI2-COMMENT  PIC X(09)  VALUE  "180028310".
      *    ¾ã³²»ù¡Ê¼Ô¡Ë¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±£¸ºÐ°Ê¾å¡Ë
           03  CONS-SYOGAI3-COMMENT  PIC X(09)  VALUE  "180028410".
      *    ÀÝ¿©µ¡Ç½ÎÅË¡
           03  CONS-SESSYOKU-COMMENT PIC X(09)  VALUE  "180016610".
      *    ¤¬¤ó´µ¼Ô¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ
           03  CONS-GANKNJ-COMMENT   PIC X(09)  VALUE  "180033110".
      *    ²ðÃ£¸£°ú
           03  CONS-KAITATU-COMMENT  PIC X(09)  VALUE  "140048010".
      *    ±ý¿Ç
           03  CONS-OSIN-COMMENT     PIC X(09)  VALUE  "114000110".
      *    ÆÃÊÌ±ý¿Ç
           03  CONS-OSIN2-COMMENT    PIC X(09)  VALUE  "114001610".
      *    ±ý¿ÇÆü¥³¥á¥ó¥È
           03  CONS-OSIN3-COMMENT    PIC X(09)  VALUE  "840000039".
      *    ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ
           03  CONS-ZAIHOU-COMMENT   PIC X(09)  VALUE  "114001110".
      *    Ë¬Ìä¿ÇÎÅÆü¥³¥á¥ó¥È
           03  CONS-ZAIHOU2-COMMENT  PIC X(09)  VALUE  "840000066".
      *    ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ¡Êµï½»·Ï¡Ë
           03  CONS-ZAIHOU3-COMMENT  PIC X(09)  VALUE  "114012910".
      *    ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ¡ÊÆ±°ì·úÊªµï½»¼Ô¡Ë¡ÊÆÃÄê»ÜÀßÅùÆþµï¼Ô¡Ë
           03  CONS-ZAIHOU4-COMMENT  PIC X(09)  VALUE  "114018010".
      *
      *    ºßÂð»þ°å³ØÁí¹ç´ÉÍýÎÁ£±¡Ê±¡³°½èÊý¤»¤ó¡Ë
           03  CONS-ZAIIKAN1-COMMENT PIC X(09)  VALUE  "114012210".
      *    ºßÂð»þ°å³ØÁí¹ç´ÉÍýÎÁ£±¡Ê±¡Æâ½èÊý¤»¤ó¡Ë
           03  CONS-ZAIIKAN2-COMMENT PIC X(09)  VALUE  "114012310".
      *    ºßÂð»þ°å³ØÁí¹ç´ÉÍýÎÁ£²¡Ê±¡Æâ½èÊý¤»¤ó¡Ë
           03  CONS-ZAIIKAN3-COMMENT PIC X(09)  VALUE  "114012410".
      *    ºßÂð»þ°å³ØÁí¹ç´ÉÍýÎÁ£²¡Ê±¡³°½èÊý¤»¤ó¡Ë
           03  CONS-ZAIIKAN4-COMMENT PIC X(09)  VALUE  "114007510".
      *    ÆÃÄê»ÜÀßÆþµï»þÅù°å³ØÁí¹ç´ÉÍýÎÁ£±¡Ê½èÊý¤»¤ó¤ò¸òÉÕ¡Ë
           03  CONS-ZAIIKAN5-COMMENT PIC X(09)  VALUE  "114013010".
      *    ÆÃÄê»ÜÀßÆþµï»þÅù°å³ØÁí¹ç´ÉÍýÎÁ£±¡Ê½èÊý¤»¤ó¤ò¸òÉÕ¤·¤Ê¤¤¡Ë
           03  CONS-ZAIIKAN6-COMMENT PIC X(09)  VALUE  "114013110".
      *    ÆÃÄê»ÜÀßÆþµï»þÅù°å³ØÁí¹ç´ÉÍýÎÁ£²¡Ê½èÊý¤»¤ó¤ò¸òÉÕ¡Ë
           03  CONS-ZAIIKAN7-COMMENT PIC X(09)  VALUE  "114013210".
      *    ÆÃÄê»ÜÀßÆþµï»þÅù°å³ØÁí¹ç´ÉÍýÎÁ£²¡Ê½èÊý¤»¤ó¤ò¸òÉÕ¤·¤Ê¤¤¡Ë
           03  CONS-ZAIIKAN8-COMMENT PIC X(09)  VALUE  "114013310".
      *    ºß°åÁí´É¡Êµ¡Ç½¶¯²½¤·¤¿ºß»Ù¿ÇÅù¡Ë¡ÊÉÂ¾²¤¢¤ê¡Ë¡Ê½èÊý¤»¤ó¡Ë
           03  CONS-ZAIIKAN9-COMMENT  PIC X(09) VALUE  "114018710".
      *    ºß°åÁí´É¡Êµ¡Ç½¶¯²½¤·¤¿ºß»Ù¿ÇÅù¡Ë¡ÊÉÂ¾²¤¢¤ê¡Ë¡Ê½èÊý¤»¤ó¤Ê¤·¡Ë
           03  CONS-ZAIIKAN10-COMMENT PIC X(09) VALUE  "114018810".
      *    ºß°åÁí´É¡Êµ¡Ç½¶¯²½¤·¤¿ºß»Ù¿ÇÅù¡Ë¡ÊÉÂ¾²¤Ê¤·¡Ë¡Ê½èÊý¤»¤ó¡Ë
           03  CONS-ZAIIKAN11-COMMENT PIC X(09) VALUE  "114018910".
      *    ºß°åÁí´É¡Êµ¡Ç½¶¯²½¤·¤¿ºß»Ù¿ÇÅù¡Ë¡ÊÉÂ¾²¤Ê¤·¡Ë¡Ê½èÊý¤»¤ó¤Ê¤·¡Ë
           03  CONS-ZAIIKAN12-COMMENT PIC X(09) VALUE  "114019010".
      *    ÆÃ°åÁí´É¡Êµ¡Ç½¶¯²½¤·¤¿ºß»Ù¿ÇÅù¡Ë¡ÊÉÂ¾²¤¢¤ê¡Ë¡Ê½èÊý¤»¤ó¡Ë
           03  CONS-ZAIIKAN13-COMMENT PIC X(09) VALUE  "114019110".
      *    ÆÃ°åÁí´É¡Êµ¡Ç½¶¯²½¤·¤¿ºß»Ù¿ÇÅù¡Ë¡ÊÉÂ¾²¤¢¤ê¡Ë¡Ê½èÊý¤»¤ó¤Ê¤·¡Ë
           03  CONS-ZAIIKAN14-COMMENT PIC X(09) VALUE  "114019210".
      *    ÆÃ°åÁí´É¡Êµ¡Ç½¶¯²½¤·¤¿ºß»Ù¿ÇÅù¡Ë¡ÊÉÂ¾²¤Ê¤·¡Ë¡Ê½èÊý¤»¤ó¡Ë
           03  CONS-ZAIIKAN15-COMMENT PIC X(09) VALUE  "114019310".
      *    ÆÃ°åÁí´É¡Êµ¡Ç½¶¯²½¤·¤¿ºß»Ù¿ÇÅù¡Ë¡ÊÉÂ¾²¤Ê¤·¡Ë¡Ê½èÊý¤»¤ó¤Ê¤·¡Ë
           03  CONS-ZAIIKAN16-COMMENT PIC X(09) VALUE  "114019410".
      *
      *    ¾®»ùÆÃÄê¼À´µ¥«¥¦¥ó¥»¥ê¥ó¥°ÎÁ¡Ê£±²óÌÜ¡Ë
           03  CONS-SYOUNI1-COMMENT  PIC X(09)  VALUE  "113000810".
      *    ¾®»ùÆÃÄê¼À´µ¥«¥¦¥ó¥»¥ê¥ó¥°ÎÁ¡Ê£²²óÌÜ¡Ë
           03  CONS-SYOUNI2-COMMENT  PIC X(09)  VALUE  "113009910".
      *    ¾®»ùÆÃÄêÂè£±²óÌÜ¥«¥¦¥ó¥»¥ê¥ó¥°¥³¥á¥ó¥È
           03  CONS-SYOUNI3-COMMENT  PIC X(09)  VALUE  "840000104".
      *
      *    £Â£Î£ÐÀºÌ©
           03  CONS-KENSA1-COMMENT   PIC X(09)  VALUE  "160162350".
      *    £Î£Ô¡Ý£ð£ò£ï£Â£Î£ÐÀºÌ©
           03  CONS-KENSA2-COMMENT   PIC X(09)  VALUE  "160181250".
      *    £È£Á£Î£ÐÀºÌ©
           03  CONS-KENSA3-COMMENT   PIC X(09)  VALUE  "160116310".
      *
      *    ¿´Â¡¥Ú¡¼¥¹¥á¡¼¥«¡¼»ØÆ³´ÉÍýÎÁ¡Ê±ó³Ö¥â¥Ë¥¿¥ê¥ó¥°¡Ë
           03  CONS-SINPESU-COMMENT  PIC X(09)  VALUE  "113001510".
      *    ½¸ÃÄ±ÉÍÜ¿©»ö»ØÆ³ÎÁ
           03  CONS-SYUEIYO-COMMENT  PIC X(09)  VALUE  "113003410".
      *    ¿Í¹©¿ÕÂ¡¡Ê¤½¤ÎÂ¾¡Ë
           03  CONS-JINZOU1-COMMENT  PIC X(09)  VALUE  "140007710".
      *    ¿Í¹©¿ÕÂ¡¡ÊËýÀ­°Ý»ýÆ©ÀÏ¡Ë¡Ê£´»þ´ÖÌ¤Ëþ¡Ë
           03  CONS-JINZOU2-COMMENT  PIC X(09)  VALUE  "140036710".
      *    ¿Í¹©¿ÕÂ¡¡ÊËýÀ­°Ý»ýÆ©ÀÏ¡Ë¡Ê£´»þ´Ö°Ê¾å£µ»þ´ÖÌ¤Ëþ¡Ë
           03  CONS-JINZOU3-COMMENT  PIC X(09)  VALUE  "140051010".
      *    ¿Í¹©¿ÕÂ¡¡ÊËýÀ­°Ý»ýÆ©ÀÏ¡Ë¡Ê£µ»þ´Ö°Ê¾å¡Ë
           03  CONS-JINZOU4-COMMENT  PIC X(09)  VALUE  "140051110".
      *    Ï¢Â³·È¹Ô¼°Ê¢Ëì¤«¤óÎ®
           03  CONS-JINZOU5-COMMENT  PIC X(09)  VALUE  "140008510".
      *    »ýÂ³´Ë½ù¼°·ì±ÕßÉ²á
           03  CONS-JINZOU6-COMMENT  PIC X(09)  VALUE  "140029850".
      *    ·ìÞù¸ò´¹ÎÅË¡
           03  CONS-JINZOU7-COMMENT  PIC X(09)  VALUE  "140008210".
      *    ·ìµåÀ®Ê¬½üµîÎÅË¡
           03  CONS-JINZOU8-COMMENT  PIC X(09)  VALUE  "140039210".
      *    ¿Í¹©¿ÕÂ¡¡ÊËýÀ­°Ý»ýÆ©ÀÏßÉ²á¡Ë¡ÊÊ£»¨¡Ë
           03  CONS-JINZOU9-COMMENT  PIC X(09)  VALUE  "140052810".
      *    ¶É½ê±¢°µÊÄº¿½èÃÖ¡Ê¤½¤ÎÂ¾¡Ë
           03  CONS-KYOKUIN1-COMMENT PIC X(09)  VALUE  "140052410".
      *    ¶É½ê±¢°µÊÄº¿½èÃÖ¡ÊÈïÊ¤ºàÅ½ÉÕ¡Ë¡Ê£±£°£°£ã£í£²Ì¤Ëþ¡Ë
           03  CONS-KYOKUIN2-COMMENT PIC X(09)  VALUE  "140051810".
      *    ¶É½ê±¢°µÊÄº¿½èÃÖ¡ÊÈïÊ¤ºàÅ½ÉÕ¡Ë¡Ê£±£°£°£ã£í£²¡Á£²£°£°£ã£í£²Ì¤Ëþ¡Ë
           03  CONS-KYOKUIN3-COMMENT PIC X(09)  VALUE  "140051910".
      *    ¶É½ê±¢°µÊÄº¿½èÃÖ¡ÊÈïÊ¤ºàÅ½ÉÕ¡Ë¡Ê£²£°£°£ã£í£²°Ê¾å¡Ë
           03  CONS-KYOKUIN4-COMMENT PIC X(09)  VALUE  "140052010".
      *    ¶É½ê±¢°µÊÄº¿½èÃÖ½é²ó²Ã»»¡Ê£±£°£°£ã£í£²Ì¤Ëþ¡Ë
           03  CONS-KYOKUIN5-COMMENT PIC X(09)  VALUE  "140052170".
      *    ¶É½ê±¢°µÊÄº¿½èÃÖ½é²ó²Ã»»¡Ê£±£°£°£ã£í£²°Ê¾å£²£°£°£ã£í£²Ì¤Ëþ¡Ë
           03  CONS-KYOKUIN6-COMMENT PIC X(09)  VALUE  "140052270".
      *    ¶É½ê±¢°µÊÄº¿½èÃÖ½é²ó²Ã»»¡Ê£²£°£°£ã£í£²°Ê¾å¡Ë
           03  CONS-KYOKUIN7-COMMENT PIC X(09)  VALUE  "140052370".
      *    Ç§ÃÎÎÅË¡¡¦Ç§ÃÎ¹ÔÆ°ÎÅË¡
           03  CONS-NINRYO-COMMENT   PIC X(09)  VALUE  "180033210".
      *    Ç§ÃÎÎÅË¡¡¦Ç§ÃÎ¹ÔÆ°ÎÅË¡¡ÊÀº¿À²ÊµßµÞÂÎÀ©¤ÎÀº¿ÀÊÝ·ò»ØÄê°å¡Ë
           03  CONS-NINRYO2-COMMENT  PIC X(09)  VALUE  "180035910".
      *
      *    Ç§ÃÎ¾ÉÀìÌç¿ÇÃÇ´ÉÍýÎÁ£²
           03  CONS-NINSEN-COMMENT   PIC X(09)  VALUE  "113014610".
      *    Âà±¡»þ¶¦Æ±»ØÆ³ÎÁ£±¡Ê£±°Ê³°¡Ë
           03  CONS-TAKYO-COMMENT    PIC X(09)  VALUE  "113008710".
      *    Âà±¡»þ¶¦Æ±»ØÆ³ÎÁÆÃÊÌ´ÉÍý»ØÆ³²Ã»»
           03  CONS-TAKYOKSN-COMMENT PIC X(09)  VALUE  "113014370".
      *    ³°Íè¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó¿ÇÎÅÎÁ£±
           03  CONS-GAIRIHA1-COMMENT PIC X(09)  VALUE  "113013910".
      *    ³°Íè¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó¿ÇÎÅÎÁ£²
           03  CONS-GAIRIHA2-COMMENT PIC X(09)  VALUE  "113014010".
      *    ³°ÍèÊü¼ÍÀþ¾È¼Í¿ÇÎÅÎÁ
           03  CONS-GAIHOU-COMMENT   PIC X(09)  VALUE  "113014110".
      *    ²ð¸î¿¦°÷ÅùÓ½áâµÛ°úÅù»Ø¼¨ÎÁ
           03  CONS-KAISYOKU-COMMENT PIC X(09)  VALUE  "114020910".
      *
      *    ³°Íè¿×Â®¸¡ÂÎ¸¡ºº²Ã»»
           03  CONS-GAIJINKEN        PIC X(09)  VALUE  "160177770".
      *
      *    Èé²¼¶ÚÆùÃí¼Í
           03  CONS-HIKAKIN          PIC X(09)  VALUE  "130000510".
      *    À¸Êª³ØÅªÀ½ºÞÃí¼Í
           03  CONS-SEIBUTU          PIC X(09)  VALUE  "130000110".
      *    ËãÌôÃí¼Í
           03  CONS-MAYAKU           PIC X(09)  VALUE  "130000310".
      *    ÀÅÌ®ÆâÃí¼Í
           03  CONS-JYOMYAKU         PIC X(09)  VALUE  "130003510".
      *    ÀÅÌ®ÆâÃí¼Í¡ÊÆýÍÄ»ù¡Ë²Ã»»
           03  CONS-JYOMYAKUKASAN    PIC X(09)  VALUE  "130003670".
      *
      *    ³°Íè´ÉÍý²Ã»»
           03  CONS-GAIKAN1          PIC X(09)  VALUE  "112011010".
           03  CONS-GAIKAN2          PIC X(09)  VALUE  "112705110".
           03  CONS-GAIKAN3          PIC X(09)  VALUE  "112705310".
      *
      *    ºÆ¿ÇÎÁ¡ÊÆ±°ìÆü£²²ÊÌÜ¡Ë
           03  CONS-DOUJITU-SAI      PIC X(09)  VALUE  "112015810".
      *    ÅÅÏÃÅùºÆ¿Ç¡ÊÆ±°ìÆü£²²ÊÌÜ¡Ë
           03  CONS-DOUJITU-DEN      PIC X(09)  VALUE  "112015950".
      *    ³°Íè¿ÇÎÅÎÁ¡ÊÆ±°ìÆü£²²ÊÌÜ¡Ë
           03  CONS-DOUJITU-GAI      PIC X(09)  VALUE  "112016210".
      *
      *    Ê£ºÆ»»Äê¿ÇÎÅ²Ê¡§
           03  CONS-DOUJITU-COMMENT  PIC X(09)  VALUE  "830000051".
      *
      *    ºÞ¤Î¿ÇÎÅÆâÍÆ¥Á¥§¥Ã¥¯¥Æ¡¼¥Ö¥ë
       01  TBL-ZAICHECK-AREA.
      *    ¡Ê¥µ¡Ë
           03  FILLER          PIC X(09)   VALUE    "820000046".
      *    ¡Ê¸¡Áª¡Ë
           03  FILLER          PIC X(09)   VALUE    "820000053".
      *    ¡Ê¥ê¥ÏÁª¡Ë
           03  FILLER          PIC X(09)   VALUE    "820000054".
      *    ¡ÊÀºÁª¡Ë
           03  FILLER          PIC X(09)   VALUE    "820000055".
      *
       01  TBL-ZAICHECK-R      REDEFINES   TBL-ZAICHECK-AREA.
           03  TBL-ZAICHECK    PIC X(09)   OCCURS    4.
      *
       01  TBL-ZAICHECK-MAX    PIC 9(02)   VALUE     4.
      *
      *    »ÀÁÇ 
           COPY    "CMSANSOTBL.INC".
      *
      *   ÆüËÜ¸ì¿ô»ú
       01  TBL-SUJI-AREA.
           03  FILLER          PIC X(20)   VALUE
              "£±£²£³£´£µ£¶£·£¸£¹£°".
       01  TBL-SUJI-R          REDEFINES   TBL-SUJI-AREA.
           03  TBL-SUJI        PIC X(02)   OCCURS   10.
       01  TBL-SUJI-AREA-X.
           03  FILLER          PIC X(10)   VALUE
              "1234567890".
       01  TBL-SUJI-X-R        REDEFINES   TBL-SUJI-AREA-X.
           03  TBL-SUJI-X      PIC X(01)   OCCURS   10.
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯
           COPY    "CPTTNS.INC".
      *
       01  WRK-RECE100-REC. 
           COPY    "CPRCF100.INC"  REPLACING  //RECE100//
                                   BY         //WRK-RECE100//.
      *
       01  WRK-RECE100G-REC. 
           COPY    "CPRCF100.INC"  REPLACING  //RECE100//
                                   BY         //WRK-RECE100G//.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥¸¥ç¥Ö´ÉÍý¥Þ¥¹¥¿
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    ¿ÇÎÅ²ñ·×
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    ¿ÇÎÅ¹Ô°Ù
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    ÅÀ¿ô
           COPY    "CPTENSU.INC".
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1006.INC".
      *
      *    ´µ¼Ô¥³¥á¥ó¥È
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑÎÎ°è
      *****************************************************************
      *
      *    È¾³Ñ¥Á¥§¥Ã¥¯¥µ¥Ö
           COPY    "CPORCSKANACHK.INC".
      *
      *   ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    ´µ¼ÔÈÖ¹æÊÑ´¹¥µ¥Ö
           COPY    "CPORCSPTID.INC".
      *    ¿ôÃÍÊÑ´¹¥µ¥Ö
           COPY    "CPORCSNUM.INC".
      *   ¥¸¥ç¥Ö´ÉÍý£Ä£ÂÀ©¸æ¥µ¥Ö
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
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
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               STOP    RUN
           ELSE
               EXIT    PROGRAM
           END-IF 
           .
      *****************************************************************
      *    ½é´ü¡¡½èÍý
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
                                       WRK-AREA
                                       TTNS-AREA
                                       FLG-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-PRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    "RECE900"           TO  RECE100PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE100PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE100PARA-HOSPNUM
           MOVE    "RECE920"           TO  RECE120PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE120PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE120PARA-HOSPNUM
           MOVE    "RECE930"           TO  RECE130PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE130PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE130PARA-HOSPNUM
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        £Ä£Â¥ª¡¼¥×¥ó½èÍý
               PERFORM 100-DBOPEN-SEC
      *
      *        ¥¹¥Æ¥Ã¥×´ÉÍý³«»Ï½èÍý
               MOVE    "STS"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    "ORCR1010"          TO  JOB-PGID
               MOVE    "¿ÇÎÅ¶èÊ¬ÊÌÅÀ¿ô½¸·×"
                                           TO  JOB-SHELLMSG
               PERFORM 900-CALL-ORCSJOB-SEC
      *
      *        ½èÍýÃæ»ßÀßÄê½èÍý
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       »ÜÀß´ð½à¾ðÊó
              MOVE     ZERO     TO     WRK-SISETU613
              MOVE     ZERO     TO     WRK-SISETU695
              MOVE     ZERO     TO     WRK-SISETU3055
              MOVE     ZERO     TO     WRK-SISETU3056
              MOVE     ZERO     TO     WRK-SISETU3057
              MOVE     ZERO     TO     WRK-SISETU3058
              MOVE     ZERO     TO     WRK-SISETU3168
              MOVE     ZERO     TO     WRK-SISETU3169
              PERFORM  VARYING  IDXA   FROM   1   BY  1
                        UNTIL   IDXA     >    7
               IF   IDXA  =  1 OR 3 OR 4 OR 5 OR 6
                 CONTINUE
               ELSE
                 INITIALIZE                   SYSKANRI-REC
                 MOVE    "1006"           TO  SYS-KANRICD
                 MOVE    IDXA             TO  SYS-KBNCD
                 MOVE    WRK-PARA-SRYYM   TO  SYS-STYUKYMD (1:6)
                 MOVE    "01"             TO  SYS-STYUKYMD (7:2)
                 MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
                 MOVE    "tbl_syskanri"   TO  WRK-MCP-TABLE
                 MOVE    "key10"          TO  WRK-MCP-PATHNAME
                 PERFORM 960-KANRIMST-READ-SEC
                 IF      FLG-SYSKANRI     =   ZERO
                   MOVE     MCPDATA-REC   TO  SYS-1006-REC
                   EVALUATE  IDXA
                     WHEN     2
                       MOVE  SYS-1006-SSTKIJUN(113)  TO  WRK-SISETU613
                       MOVE  SYS-1006-SSTKIJUN(195)  TO  WRK-SISETU695
                     WHEN     7
                       MOVE  SYS-1006-SSTKIJUN(55)   TO  WRK-SISETU3055
                       MOVE  SYS-1006-SSTKIJUN(56)   TO  WRK-SISETU3056
                       MOVE  SYS-1006-SSTKIJUN(57)   TO  WRK-SISETU3057
                       MOVE  SYS-1006-SSTKIJUN(58)   TO  WRK-SISETU3058
                       MOVE  SYS-1006-SSTKIJUN(168)  TO  WRK-SISETU3168
                       MOVE  SYS-1006-SSTKIJUN(169)  TO  WRK-SISETU3169
                   END-EVALUATE
                 END-IF
               END-IF
              END-PERFORM
           ELSE
              MOVE     SPACE              TO  TBL-SYS1006-T
              INITIALIZE                      TBL-SYS1006-T
      *
              PERFORM  VARYING  IDXA   FROM   1   BY  1
                        UNTIL   IDXA     >    7
               IF   IDXA  =  1 OR 3 OR 4 OR 5 OR 6
                 CONTINUE
               ELSE
                 MOVE     ZERO            TO  SYSKAN-IDX2
                 INITIALIZE                   SYSKANRI-REC
                 MOVE    "1006"           TO  SYS-KANRICD
                 MOVE    IDXA             TO  SYS-KBNCD
grpsys           MOVE    SPA-HOSPNUM      TO  SYS-HOSPNUM
                 MOVE    SYSKANRI-REC     TO  MCPDATA-REC
                 MOVE    "tbl_syskanri"      TO  MCP-TABLE
                 MOVE    "key8"              TO  MCP-PATHNAME
grpsys           PERFORM 900-DBSELECT-SEC
                 IF      MCP-RC              =   ZERO
                     MOVE    "tbl_syskanri"      TO  MCP-TABLE
                     MOVE    "key8"              TO  MCP-PATHNAME
                     PERFORM  970-KANRIMST-READ-SEC
                     PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                               OR    (SYSKAN-IDX2  >=  100)
      *
                       MOVE    MCPDATA-REC   TO  SYS-1006-REC
                       EVALUATE  IDXA
                         WHEN     2
                           ADD     1     TO  SYSKAN-IDX2
                           MOVE    SYS-1006-REC
                                         TO  TBL-SYS10062(SYSKAN-IDX2)
                         WHEN     7
                           ADD     1     TO  SYSKAN-IDX2
                           MOVE    SYS-1006-REC
                                         TO  TBL-SYS10067(SYSKAN-IDX2)
                       END-EVALUATE
      *
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key8"              TO  MCP-PATHNAME
                       PERFORM 970-KANRIMST-READ-SEC
      *
                     END-PERFORM
                 END-IF
                 MOVE    "tbl_syskanri"      TO  MCP-TABLE
                 MOVE    "key8"              TO  MCP-PATHNAME
                 PERFORM 900-CLOSE-SEC
               END-IF
              END-PERFORM
      *
           END-IF
      *
          .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           OPEN    OUTPUT              RECE130-FILE
           CLOSE                       RECE130-FILE
      *
           OPEN    I-O                 RECE100-FILE
           OPEN    OUTPUT              RECE120-FILE
           OPEN    I-O                 RECE130-FILE
      *
           PERFORM    RECE100-READ-SEC
      *
           PERFORM                     UNTIL   FLG-RECE100 =   1
                                       OR      FLG-END     =   1
      *        ÊÔ½¸½èÍý
               PERFORM     210-HENSYU-SEC
      *
               PERFORM     RECE100-READ-SEC
      *
               IF      CNT-IN-COMMIT   =   300
                   MOVE  ZERO              TO  CNT-IN-COMMIT
                   PERFORM 900-DBCOMMIT-SEC
                   PERFORM 900-DBSTART-SEC
               END-IF
      *
      *        ½èÍýÃæ»ßÀßÄê½èÍý
               IF      CNT-IN-CANCEL   =   50
                   MOVE    ZERO            TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-PERFORM
      *
           CLOSE   RECE100-FILE
                   RECE120-FILE
                   RECE130-FILE
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ÊÔ½¸½èÍý
      *****************************************************************
       210-HENSYU-SEC                   SECTION.
      *
           INITIALIZE                   WRK-RECE100G-REC
      *
           INITIALIZE                   TBL-JNISSU                      
           INITIALIZE                   TBL-JNISSU-GEN
      *
           INITIALIZE                   FLG-KHNTEN-AREA
           INITIALIZE                   FLG-SAISIN-AREA
           INITIALIZE                   CNT-SAISIN-AREA
      *
           MOVE    88888880     TO      WRK-ZAINUM
      *
           MOVE    ZERO         TO      WRK-SURYOU-COUNT
      *
           PERFORM VARYING IDX    FROM    1   BY   1
                   UNTIL ( IDX    >       20    )
                   OR    ( RECE100-SRYKA(IDX)
                                  =       SPACE )
               PERFORM 2104-RECE120-HENSYU-SEC
           END-PERFORM
           PERFORM     2105-GENTEN-HENSYU-SEC
           PERFORM     2106-JNISSU-SET-SEC                                   
           PERFORM     2107-KHNTEN-CLEAR-SEC
           PERFORM     2110-NYUYOJI-COMENTFLG-SET-SEC
      *
           PERFORM     RECE100-REWRITE-SEC
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       2104-RECE120-HENSYU-SEC                SECTION.
      *
           INITIALIZE                      SRYACCT-REC
grpsys     MOVE    RECE100-HOSPNUM     TO  ACCT-HOSPNUM
           MOVE    RECE100-NYUGAIKBN   TO  ACCT-NYUGAIKBN
           MOVE    RECE100-PTID        TO  ACCT-PTID
           MOVE    RECE100-SRYKA(IDX)  TO  ACCT-SRYKA
           MOVE    RECE100-SRYYM       TO  ACCT-SRYYM
           MOVE    RECE100-HKNCOMBI    TO  ACCT-HKNCOMBI
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO   MCP-TABLE
               MOVE    "key24"             TO   MCP-PATHNAME
               PERFORM  900-SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
      *
                   MOVE   ZERO           TO  FLG-RINYAK
                   IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                        (ACCT-ZAIKAISU  NOT =    ZERO)    AND
                        (ACCT-JIHOKBN       =   SPACE)
                               MOVE  ZERO  TO   FLG-SHOSAISIN           
                               MOVE  ZERO  TO   FLG-SHOSAISIN-GEN
                               MOVE  ZERO  TO   WRK-SRYSURYO
                               MOVE  ZERO  TO   WRK-SRYSURYO2
                               MOVE  ZERO  TO   WRK-SRYSURYO3
                               MOVE  ZERO  TO   WRK-TANICD
                               MOVE  ZERO  TO   WRK-TANIMOJISU
                               MOVE  SPACE TO   WRK-TANINAME
                               MOVE  ZERO  TO   FLG-HOUSIJI-COM
                                                FLG-HOUTEN-COM
                                                FLG-SYOBYO-COM
                                                FLG-RYOYO-COM
                                                FLG-SYODOU-COM
                                                FLG-SAIDOU-COM
                                                FLG-NIKOCHIN-COM
                                                FLG-ZENSOKU-COM
                                                FLG-OSIN-COM
                                                FLG-ZAIHOU-COM
                                                FLG-TAHOKEN
                                                FLG-SYOUNI-COM
                                                FLG-KENSA-COM
      *
                               MOVE  ZERO  TO   WRK-FILMTEN
                               MOVE  ZERO  TO   FLG-NEBU
      *
                               MOVE  ZERO  TO
                                           WRK-KZI058-TEN
                                           WRK-KZI058-TANICD
                                           WRK-KZI058-TANIMOJISU
                               MOVE  SPACE TO
                                           WRK-KZI058-TANINAME
      *
                               MOVE  ZERO  TO   FLG-WRITE05
                               MOVE  ZERO  TO   FLG-HENSYU-NG
                               PERFORM 21043-RECE120-TENSOU-SEC
                               PERFORM 21044-RECE130-HENSYU-SEC
                               IF    FLG-WRITE05  =   1
                                 PERFORM RECE120-WRITE-SEC
                               END-IF
                   ELSE
                     IF  (ACCT-ZAIKAISU  NOT =    ZERO)  AND
                         (ACCT-JIHOKBN       =   SPACE)
                               IF ( ACCT-SRYKBN  NOT =
                                    "24" AND "90" AND "92" AND "93" AND
                                    "97" AND "98"                 )
                               OR ( ACCT-ZAIKBN  =   1            )
                                 IF  (ACCT-SRYCDTOTAL(1:12)  =
                                                    "000000999901")
                                 AND (ACCT-MEISAISU      =   1    )
                                      CONTINUE
                                 ELSE
                                      MOVE  ZERO  TO   FLG-SHOSAISIN    
                                      MOVE  ZERO  TO   FLG-SHOSAISIN-GEN
                                      MOVE  ZERO  TO   WRK-SRYSURYO
                                      MOVE  ZERO  TO   WRK-SRYSURYO2
                                      MOVE  ZERO  TO   WRK-SRYSURYO3
                                      MOVE  ZERO  TO   WRK-TANICD
                                      MOVE  ZERO  TO   WRK-TANIMOJISU
                                      MOVE  SPACE TO   WRK-TANINAME
                                      MOVE  ZERO  TO   FLG-HOUSIJI-COM
                                                       FLG-HOUTEN-COM
                                                       FLG-SYOBYO-COM
                                                       FLG-RYOYO-COM
                                                       FLG-SYODOU-COM
                                                       FLG-SAIDOU-COM
                                                       FLG-NIKOCHIN-COM
                                                       FLG-ZENSOKU-COM
                                                       FLG-OSIN-COM
                                                       FLG-ZAIHOU-COM
                                                       FLG-TAHOKEN
                                                       FLG-SYOUNI-COM
                                                       FLG-KENSA-COM
      *
                                      MOVE  ZERO  TO   WRK-FILMTEN
                                      MOVE  ZERO  TO   FLG-NEBU
      *
                                      MOVE  ZERO  TO
                                                  WRK-KZI058-TEN
                                                  WRK-KZI058-TANICD
                                                  WRK-KZI058-TANIMOJISU
                                      MOVE  SPACE TO
                                                  WRK-KZI058-TANINAME
      *
                                      MOVE  ZERO  TO   FLG-WRITE05
                                      MOVE  ZERO  TO   FLG-HENSYU-NG
                                      IF    ACCT-SRYKBN  =
                                            "60" OR "64" OR "80"
                                        PERFORM 21046-SINRYO-CHECK-SEC
                                      END-IF
                                      PERFORM   21043-RECE120-TENSOU-SEC
                                      PERFORM   21044-RECE130-HENSYU-SEC
                                      IF    FLG-WRITE05  =   1
                                        PERFORM RECE120-WRITE-SEC
                                      END-IF
                                 END-IF
                               END-IF
                     END-IF
                   END-IF
                   MOVE    "tbl_sryacct"       TO   MCP-TABLE
                   MOVE    "key24"             TO   MCP-PATHNAME
                   PERFORM   900-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryacct"       TO   MCP-TABLE
           MOVE    "key24"             TO   MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2104-RECE120-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¾ðÊó¥Õ¥¡¥¤¥ë¹àÌÜÅ¾Á÷½èÍý
      *****************************************************************
       21043-RECE120-TENSOU-SEC                SECTION.
      *
           MOVE    SPACE               TO  RECE120-REC
           INITIALIZE                      RECE120-REC
           MOVE    RECE100-KEY         TO  RECE120-KEY1
           MOVE    ACCT-SRYKBN         TO  RECE120-SRYKBN
           MOVE    ACCT-ZAINUM         TO  RECE120-ZAINUM
           MOVE    ACCT-SRYCDTOTAL     TO  WRK-SRYCDTOTAL9
           IF      WRK-SRYCDTOTALX(6:9)
                                       =   CONS-GAIKAN1   OR
                                           CONS-GAIKAN2   OR
                                           CONS-GAIKAN3
               ADD     1               TO  WRK-ZAINUM
               MOVE    WRK-ZAINUM      TO  RECE120-ZAINUM
           END-IF
           MOVE    RECE100-PTNUM       TO  RECE120-PTNUM
      *
           MOVE    ACCT-SRYKA           TO  RECE120-SRYKA
           MOVE    ACCT-HKNCOMBI        TO  RECE120-HKNCOMBI
           MOVE    ACCT-JIHOKBN         TO  RECE120-JIHOKBN
           MOVE    ACCT-ZAITEN          TO  RECE120-ZAITEN
           MOVE    ACCT-SRYCDTOTAL      TO  RECE120-SRYCDTOTAL
           MOVE    ACCT-SURYOUTOTAL     TO  RECE120-SURYOUTOTAL
           MOVE    ACCT-MEISAISU        TO  RECE120-MEISAISU
           MOVE    ACCT-ZAIKAISU        TO  RECE120-ZAIKAISU
           MOVE    ACCT-SYUTEN1         TO  RECE120-SYUTEN1
           MOVE    ACCT-SYUTEN2         TO  RECE120-SYUTEN2
           MOVE    ACCT-YKZTEN          TO  RECE120-YKZTEN
           MOVE    ACCT-KIZAITEN        TO  RECE120-KIZAITEN
           IF      ACCT-SRYKBN    =  "70"
             COMPUTE  RECE120-SYUTEN1   =   RECE120-SYUTEN1
                                        -   RECE120-SYUTEN2
             COMPUTE  RECE120-KIZAITEN  =   RECE120-KIZAITEN
                                        +   RECE120-SYUTEN2
           END-IF
           MOVE    ACCT-DRCD            TO  RECE120-DRCD
           MOVE    ACCT-ZAIREQFLG       TO  RECE120-ZAIREQFLG
           MOVE    ACCT-TEIGENRATE      TO  RECE120-TEIGENRATE
           MOVE        ZERO             TO  WK-TAIHI-DAY
           MOVE        ZERO             TO  WK-TAIHI-FLG
           PERFORM     VARYING   IDX3   FROM    1    BY   1
                       UNTIL     IDX3   >       4
               PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >       31
                   MOVE    ACCT-DAY(IDX3 IDX2)  TO
                                            RECE120-KAISU (IDX3 IDX2)
                   IF     (WK-TAIHI-FLG            =   ZERO)   AND
                          (ACCT-DAY(1 IDX2)    NOT =   ZERO)
                       MOVE     IDX2    TO  WK-TAIHI-DAY
                       MOVE     1       TO  WK-TAIHI-FLG
                   END-IF    
               END-PERFORM
           END-PERFORM
      *
           IF    FLG-RINYAK    NOT =    ZERO
                MOVE    FLG-RINYAK      TO  RECE120-SAFLG
           END-IF
      *
           .
       21043-RECE120-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Å¬ÍÑÍó¾ðÊó¥Õ¥¡¥¤¥ë¹àÌÜÅ¾Á÷½èÍý
      *****************************************************************
       21044-RECE130-HENSYU-SEC               SECTION.
      *
           INITIALIZE                      SRYACT-REC
grpsys     MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
                   MOVE   ZERO   TO  FLG-ZANHAIKI
                                     FLG-FORMAL-KISAI
                   MOVE   ZERO   TO  WRK-RENNUM
                                     WRK-RENNUM2
                   MOVE   ZERO   TO  FLG-WRITE05X
                   PERFORM   210441-RECE130-TENSOU-SEC
                   PERFORM   210442-RECE100-HENSYU-SEC
                   IF  FLG-WRITE05X  =  1
                      PERFORM  RECE130-WRITE-SEC
                      MOVE  1    TO  FLG-WRITE05
                   END-IF
                   IF  FLG-ZANHAIKI  =  1
                      PERFORM  RECE130-REWRITE-SEC
                   END-IF
                   IF  FLG-FORMAL-KISAI  =  1
                      PERFORM  RECE130-REWRITE2-SEC
                   END-IF
                   MOVE    "tbl_sryact"         TO  MCP-TABLE
                   MOVE    "key2"               TO  MCP-PATHNAME
                   PERFORM   900-SRYACT-READ-SEC
               END-PERFORM
           ELSE
               DISPLAY   "SRYACT SELECT ERR" 
           END-IF
      *
           MOVE    "tbl_sryact"         TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF     WRK-FILMTEN  NOT =  ZERO
                COMPUTE    RECE120-SYUTEN2  =  WRK-FILMTEN + 0.5
           END-IF
      *
           .
       21044-RECE130-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       210441-RECE130-TENSOU-SEC                SECTION.
      *
           INITIALIZE                   WRK-TNS-TAIHI-AREA
      *
           MOVE    SPACE                TO  RECE130-REC
           INITIALIZE                       RECE130-REC
           MOVE    RECE100-KEY          TO  RECE130-KEY1
           MOVE    SRY-ZAINUM           TO  RECE130-ZAINUM
           IF      RECE120-ZAINUM       >       88888880
               MOVE   RECE120-ZAINUM    TO  RECE130-ZAINUM
           END-IF
           MOVE    SRY-RENNUM           TO  RECE130-RENNUM
           MOVE    RECE100-PTNUM        TO  RECE130-PTNUM
      *
           MOVE    SRY-SRYKA            TO  RECE130-SRYKA
           MOVE    SRY-SRYKBN           TO  RECE130-SRYKBN
           MOVE    SRY-SRYSYUKBN        TO  RECE130-SRYSYUKBN
           MOVE    SRY-JIHIMONEYTOTAL   TO  RECE130-JIHIMONEY
           MOVE    ZERO                 TO  FLG-OSINKBN
                                                IDX4
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                       UNTIL    (IDX2      >    5)    OR
                                (SRY-SRYCD(IDX2)  =   SPACE)
      *
             PERFORM 900-TENSU-TBL-SEC
      **     ÅÀ¿ô¥Þ¥¹¥¿¾ðÊóÂàÈò
             MOVE    TNS-CHUKSNTUBAN    TO  WRK-TNS-CHUKSNTUBAN(IDX2)
             MOVE    TNS-DATAKBN        TO  WRK-TNS-DATAKBN(IDX2)
             MOVE    TNS-TIMEKSNKBN     TO  WRK-TNS-TIMEKSNKBN(IDX2)
             MOVE    TNS-TEN            TO  WRK-TNS-TEN(IDX2)
             MOVE    TNS-GAITENTTLKBN   TO  WRK-TNS-GAITENTTLKBN(IDX2)
             MOVE    TNS-OSINKBN        TO  WRK-TNS-OSINKBN(IDX2)
             MOVE    TNS-KNSJISKBN      TO  WRK-TNS-KNSJISKBN(IDX2)
             MOVE    TNS-SRYCD          TO  WRK-TNS-SRYCD(IDX2)
      *
             IF     ( SRY-SRYCD(IDX2)(1:1)  =  "S"   )
                  COMPUTE   RECE120-MEISAISU  =  RECE120-MEISAISU
                                             -  1
             END-IF
      *
             IF       SRY-SRYCD(IDX2)(1:5)  =
                      "09960" OR "09970" OR "09980"
                  MOVE    1     TO    FLG-HENSYU-NG
             END-IF
      *
             IF     ( SRY-SRYCD(IDX2)(1:7)  =  "0999901"    )
             OR     ( SRY-SRYCD(IDX2)(1:3)  =  "001"        )
             OR     ((SRY-SRYCD(IDX2)(1:3)  =  "059") AND
                     (TNS-TEN               =  ZERO )       )
             OR     ( SRY-SRYCD(IDX2)(1:7)  =  "0992000"    )
             OR     ( SRY-SRYCD(IDX2)(1:4)  =  "0085"
                                            OR "0086"       )
             OR     ((ACCT-SRYKBN = "40" OR "50" OR "54" OR
                                    "60" OR "64" OR "70" OR
                                    "80"                   ) AND
                     (ACCT-ZAITEN       NOT =  ZERO        ) AND
                     (ACCT-ZAIKAISU     NOT =  ZERO        ) AND
                     (ACCT-JIHOKBN          =  SPACE       ) AND
                     (ACCT-YKZTEN           =  ZERO        ) AND
                     (SRY-SRYCD(IDX2)(1:1)  =  "6"         ))
             OR     ( FLG-HENSYU-NG     NOT =  ZERO         )
             OR     ( SRY-SRYCD(IDX2)       =
                      "099209902" OR "099209903" OR
                      "099209904" OR "099209905" OR
                      "099209906" OR "099209907" OR
                      "099209908" OR
                      "099999903" OR "099999904" OR
                      "099999905" OR "099999906" OR
                      "099999907" OR "099999908" OR
                      "099999909" OR "099999910" OR
                      "099999921" OR
                      "099830101"                           )
                  COMPUTE   RECE120-MEISAISU    =  RECE120-MEISAISU
                                               -  1
                  MOVE      SRY-SRYCD(IDX2)   TO  WRK-SRYCD
                  COMPUTE   RECE120-SRYCDTOTAL  =  RECE120-SRYCDTOTAL
                                               -  WRK-SRYCD
                  IF ((ACCT-SRYKBN = "40" OR "50" OR "54" OR
                                     "60" OR "64" OR "70" OR
                                     "80"                   ) AND
                      (ACCT-ZAITEN       NOT =  ZERO        ) AND
                      (ACCT-ZAIKAISU     NOT =  ZERO        ) AND
                      (ACCT-JIHOKBN          =  SPACE       ) AND
                      (ACCT-YKZTEN           =  ZERO        ) AND
                      (SRY-SRYCD(IDX2)(1:1)  =  "6"         ))
                     COMPUTE  RECE120-SURYOUTOTAL =  RECE120-SURYOUTOTAL
                                                  -  SRY-SRYSURYO(IDX2)
                  ELSE
                     COMPUTE  RECE120-SURYOUTOTAL =  RECE120-SURYOUTOTAL
                                                  -  1
                  END-IF
             END-IF
      *
             IF      SRY-SRYCD(IDX2)(1:1)     =   "0" OR  "1" OR
                                                  "6" OR  "7" OR
                                                  "8"
      *
              IF     ( SRY-SRYCD(IDX2)(1:7)  =  "0999901"    )
              OR     ( SRY-SRYCD(IDX2)(1:3)  =  "001"        )
              OR     ((SRY-SRYCD(IDX2)(1:3)  =  "059") AND
                      (TNS-TEN               =  ZERO )       )
              OR     ( SRY-SRYCD(IDX2)(1:7)  =  "0992000"    )
              OR     ( SRY-SRYCD(IDX2)(1:4)  =  "0085"
                                             OR "0086"       )
              OR     ((ACCT-SRYKBN = "40" OR "50" OR "54" OR
                                     "60" OR "64" OR "70" OR
                                     "80"                   ) AND
                      (ACCT-ZAITEN       NOT =  ZERO        ) AND
                      (ACCT-ZAIKAISU     NOT =  ZERO        ) AND
                      (ACCT-JIHOKBN          =  SPACE       ) AND
                      (ACCT-YKZTEN           =  ZERO        ) AND
                      (SRY-SRYCD(IDX2)(1:1)  =  "6"         ))
              OR     ( FLG-HENSYU-NG     NOT =  ZERO         )
              OR     ( SRY-SRYCD(IDX2)       =
                       "099209902" OR "099209903" OR
                       "099209904" OR "099209905" OR
                       "099209906" OR "099209907" OR
                       "099209908" OR
                       "099999903" OR "099999904" OR
                       "099999905" OR "099999906" OR
                       "099999907" OR "099999908" OR
                       "099999909" OR "099999910" OR
                       "099999921" OR
                       "099830101"                           )
      *
               CONTINUE
      *
              ELSE
      *
               ADD     1                  TO    IDX4
      *
               MOVE    SRY-SRYCD(IDX2)    TO    RECE130-SRYCD(IDX4)
      *
               IF     SRY-SRYCD(IDX2)      =    CONS-GAIJINKEN
                  MOVE   "1"              TO    RECE100-KENTAIKASANFLG
                  MOVE   99               TO    RECE120-KNSJISGRPKBN
               END-IF
               IF     RECE120-KNSJISGRPKBN  =    ZERO
                 IF    TNS-KNSJISGRPKBN = 1 OR 2 OR 3 OR 4 OR 5 OR 6
                  MOVE  TNS-KNSJISGRPKBN  TO    RECE120-KNSJISGRPKBN
                 END-IF
               END-IF
      *
               IF    ( SRY-SRYKBN         =     "60" OR "64" )
               AND   ( TNS-KNSJISKBN      =      2           )
                   MOVE    TNS-KNSJISKBN        TO  RECE120-KNSJISKBN
               END-IF
      *
               IF    ( SRY-SRYKBN   =  "14" OR "21" OR "22" OR "23" )
               AND   ( RECE120-ZAITEN    =     ZERO                 )
               AND   ( RECE120-SAFLG     =     SPACE                )
               AND   ( ACCT-ZAIKBN   NOT =     1                    )
                  IF   SRY-SRYCD(IDX2)   =     "820000046"
                     MOVE     "1"         TO    RECE120-SAFLG
                  END-IF
               END-IF
      *
               IF    ( SRY-SRYKBN   =  "21" OR "22" OR "23" )
               AND   ( RECE120-ZAITEN     =     ZERO        )
               AND   ( ACCT-ZAIKBN   NOT =     1            )
                  IF     RECE120-SAFLG   NOT =  SPACE
                        MOVE   SPACE      TO    RECE120-TOYAKUFLG
                  ELSE
                    IF  (SRY-SRYCD(IDX2)(1:1)  =  "8"  )
                    OR  (SRY-SRYCD(IDX2)(1:3)  =  "008")
                        IF   SRY-SRYSYUKBN  =  "212" OR "292" OR
                                               "222" OR "232"
                          IF   RECE120-TOYAKUFLG = SPACE
                            MOVE   "2"    TO    RECE120-TOYAKUFLG
                          END-IF
                        END-IF
                    ELSE
                        MOVE    "1"       TO    RECE120-TOYAKUFLG
                    END-IF
                  END-IF
               END-IF
      *
               IF    ( SRY-SRYKBN        =     "14" )
               AND   ( RECE120-ZAITEN    =     ZERO )
               AND   ( ACCT-ZAIKBN   NOT =     1    )
                  IF     RECE120-SAFLG   NOT =  SPACE
                        MOVE   SPACE      TO    RECE120-TOYAKUFLG
                  ELSE
                    IF  (SRY-SRYCD(IDX2)(1:1)  =  "8"  )
                    OR  (SRY-SRYCD(IDX2)(1:3)  =  "008")
                        IF   SRY-SRYSYUKBN  =  "148" OR "149"
                          IF   RECE120-TOYAKUFLG = SPACE
                            MOVE   "2"    TO    RECE120-TOYAKUFLG
                          END-IF
                        END-IF
                    ELSE
                        MOVE    "1"       TO    RECE120-TOYAKUFLG
                    END-IF
                  END-IF
               END-IF
      *
               IF     SRY-SRYCD(IDX2)  IS  NUMERIC
      *          IF  SRY-SRYCD(IDX2)(1:3) NOT = "001"
                   MOVE    SRY-SRYCD(IDX2)   TO  WRK-SRYCD
      *
                   MOVE     ZERO    TO    WRK-FUKA-KINGAKU
                   PERFORM  VARYING  FUKA-IDX  FROM  1  BY  1
                             UNTIL   FUKA-IDX   >    9
                     MOVE   SRY-SRYCD(IDX2)(FUKA-IDX:1)
                                             TO  WRK-FUKA-VALUE
                     IF  WRK-FUKA-VALUE  =  ZERO
                       CONTINUE
                     ELSE
                       COMPUTE  RECE120-RUIKEITOTAL  =
                                RECE120-RUIKEITOTAL  +
                              ( WRK-FUKA-VALUE    * WRK-SRYCD)
                       COMPUTE  WRK-FUKA-KINGAKU    =
                                WRK-FUKA-KINGAKU    +
                              ((WRK-FUKA-VALUE^4) * FUKA-IDX
                                                  * SRY-SRYSURYO(IDX2))
                     END-IF
                   END-PERFORM
                   COMPUTE  RECE120-RUIKEITOTAL  =
                            RECE120-RUIKEITOTAL  +
                            WRK-FUKA-KINGAKU
      *
                   MOVE     ZERO    TO    WRK-FUKA-KINGAKU
                   PERFORM  VARYING  FUKA-IDX  FROM  1  BY  1
                             UNTIL   FUKA-IDX   >   10
                     MOVE   SRY-SRYSURYO(IDX2)(FUKA-IDX:1)
                                             TO  WRK-FUKA-VALUE
                     IF  WRK-FUKA-VALUE  =  ZERO
                       CONTINUE
                     ELSE
                       COMPUTE  WRK-FUKA-KINGAKU  =
                                WRK-FUKA-KINGAKU  +
                              ((WRK-FUKA-VALUE^4) * FUKA-IDX)
                     END-IF
                   END-PERFORM
                   COMPUTE  RECE120-RUIKEITOTAL  =
                            RECE120-RUIKEITOTAL  +
                            WRK-FUKA-KINGAKU
      *          END-IF
               END-IF
      *
               IF      SRY-SRYKBN = "40" OR "50" OR "54" OR "60" OR "64"
                  IF   SRY-SRYCD(IDX2)(1:1)  =  "7"
                  AND  TNS-DATAKBN           =  "3"
                         COMPUTE WRK-FILMTEN2   =
                                (TNS-TEN  * SRY-SRYSURYO(IDX2) / 10)
                         IF   TNS-JGNTEN  NOT =  ZERO
                         AND  TNS-JGNTEN   <  WRK-FILMTEN2
                             COMPUTE  WRK-FILMTEN  =
                                      WRK-FILMTEN  +
                                      TNS-JGNTEN
                         ELSE
                             COMPUTE  WRK-FILMTEN  =
                                      WRK-FILMTEN  +
                                      WRK-FILMTEN2
                         END-IF
                  END-IF
               END-IF
      *
               MOVE    SRY-SRYSURYO(IDX2) TO    RECE130-SRYSURYO(IDX4)
               MOVE    SRY-SRYKAISU(IDX2) TO    RECE130-SRYKAISU(IDX4)
               MOVE    SRY-MEISKYFLG(IDX2) TO   RECE130-MEISKYFLG(IDX4)
               MOVE    SRY-AUTOKBN(IDX2)  TO    RECE130-AUTOKBN(IDX4)
               MOVE    SRY-INPUTNUM(IDX2) TO    RECE130-INPUTNUM(IDX4)
      *
      **¿ÇÎÅ¼ÂÆü¿ô¤Î¥«¥¦¥ó¥È¤ò¹Ô¤¨¤ëºÞ¤Ç¤¢¤ë¤«¤ÎÈ½Äê
               IF  ((ACCT-ZAITEN    NOT =    ZERO)    AND               
                    (ACCT-ZAIKAISU  NOT =    ZERO)    AND               
                    (ACCT-SRYKBN        =  "11"  OR  "12"  OR           
                                           "13"  OR  "14"     ))        
               OR  ((ACCT-ZAITEN        =    ZERO)    AND               
                    (ACCT-ZAIKAISU  NOT =    ZERO)    AND               
                    (ACCT-SRYKBN        =  "11"  OR  "12"  OR           
                                           "99"               ))        
      *
                       IF ( (TNS-JITUDAY    =     2          ) AND      
                            (TNS-DAYCNT     =     1   OR   2 )    )     
                       OR (  SRY-SRYCD(IDX2)  =  "112700850"            
                                              OR "112003350"            
                                              OR "099409901"            
                                              OR "099999902"            
                                              OR "099110001"            
                                              OR "099120001"            
      *                              Æ±ÆüºÆ¿Ç
                                              OR "112010150"
                                              OR "112010250"
                                              OR "112010350"
                                              OR "112010450"
                                              OR "112010550"
                                              OR "112704450"
                                              OR "112704550"
                                              OR "112704650"
                                              OR "112704750"
                                              OR "112010650"
                                              OR "112010750"
                                              OR "112010850"
                                              OR "112010950"
                                              OR "112012850"
                                              OR "112704850"
                                              OR "112704950"
                                              OR "112705050"
                                              OR "112706850"
                                              OR "112008850"
                                              OR "112008950"
                                              OR "112009050"
                                              OR "112009150"
                                              OR "112012550"
                                              OR "112703450"
                                              OR "112703550"
                                              OR "112703650"
                                              OR "112706550"
                                              OR "112008350"
                                              OR "112008450"
                                              OR "112008550"
                                              OR "112008650"
                                              OR "112008750"
                                              OR "112703050"
                                              OR "112703150"
                                              OR "112703250"
                                              OR "112703350"
      *
                                              OR "112011710"
                                              OR "112011810"
                                              OR "112011910"
                                              OR "112012010"
                                              OR "112012210"
                                              OR "112705850"
                                              OR "112705950"
                                              OR "112706050"
      *
                                              OR "111011810"
                                              OR "113013910"
                                              OR "113014010"
                                              OR "113014110"
                                              OR "112015810"
                                              OR "112015950"
                                              OR "112016210"      )
      *
                             MOVE    1      TO    FLG-SHOSAISIN         
      *
                       END-IF                                           
      *
                       IF    SRY-SRYCD(IDX2)  =  "099409903"
                             MOVE    1      TO    FLG-SHOSAISIN-GEN
                       END-IF
      *
               END-IF                                                   
      *
               IF      TNS-SRYKBN         =     "80"
                   IF  TNS-GAITENTTLKBN   =     130
                       MOVE   "13"        TO    RECE120-SRYKBN
                                                RECE130-SRYKBN
                   END-IF
               END-IF
      *
               PERFORM  2104411-RECE130-TENSOU-COMCNT-HEN-SEC
      *
               IF  SRY-SRYCD(IDX2)        =     CONS-ZANHAIKI           
                IF   ACCT-ZAIKBN  =  1
                  MOVE    "1"             TO  RECE130-RECEPTFLG(IDX4)
                  MOVE    ZERO            TO  RECE130-SRYSURYO(IDX4)
                ELSE
                  COMPUTE WRK-SRYSURYO2   =     WRK-SRYSURYO            
                                          +     0.99999                   
                  COMPUTE RECE130-SRYSURYO(IDX4)                         
                                          =     WRK-SRYSURYO2           
                                          -     WRK-SRYSURYO            
                  IF  RECE130-SRYSURYO(IDX4)  NOT = ZERO                 
                    MOVE  WRK-TANICD      TO    RECE130-TANICD(IDX4)     
                    MOVE  WRK-TANIMOJISU  TO    RECE130-TANIMOJISU(IDX4) 
                    MOVE  WRK-TANINAME    TO    RECE130-TANINAME(IDX4)   
                    IF    RECE130-RENNUM  >  1                           
                    AND   IDX4           =  1                           
                      MOVE    1           TO    FLG-ZANHAIKI            
                      COMPUTE  WRK-RENNUM  =  RECE130-RENNUM  -  1       
                      MOVE WRK-SRYSURYO2  TO  WRK-SRYSURYO3             
                    ELSE                                                
                      MOVE WRK-SRYSURYO2  TO  RECE130-SRYSURYO(IDX4 - 1) 
                    END-IF
                  ELSE
                    MOVE  "1"             TO  RECE130-RECEPTFLG(IDX4)
                  END-IF                                                
                END-IF
               ELSE                                                     
                  MOVE    SRY-SRYSURYO(IDX2)                            
                                          TO    WRK-SRYSURYO            
                  IF   TNS-TANICD  NOT = SPACE AND "000"
                    MOVE  TNS-TANICD      TO    RECE130-TANICD(IDX4)
                                                WRK-TANICD
                  END-IF
                  MOVE    TNS-TANIMOJISU  TO    RECE130-TANIMOJISU(IDX4)
                                                WRK-TANIMOJISU          
                  MOVE    TNS-TANINAME    TO    RECE130-TANINAME(IDX4)
                                                WRK-TANINAME            
               END-IF                                                   
      *********IF      TNS-TENSIKIBETU    =     1   OR   3
      *********    MOVE  TNS-TENSIKIBETU  TO   RECE130-TENSIKIBETU(IDX4)
      *********    MOVE  TNS-TEN          TO    RECE130-TEN(IDX4)
      *********END-IF
               MOVE  TNS-TENSIKIBETU  TO   RECE130-TENSIKIBETU(IDX4)
               MOVE  TNS-TEN          TO   RECE130-TEN(IDX4)
               IF      RECE100-NYUGAIKBN   =     "1"
                   MOVE  TNS-NYUTENTTLKBN TO    RECE130-TENTTLKBN(IDX4)
               ELSE
                   MOVE  TNS-GAITENTTLKBN TO    RECE130-TENTTLKBN(IDX4)
               END-IF
               IF     (SRY-SRYCD    (IDX2)(1:1)     =   "8"  )
               OR     (SRY-SRYCD    (IDX2)(1:3)     =   "008")
                  MOVE ZERO               TO    RECE130-YUKOKETA(IDX4)
               ELSE
                  MOVE TNS-YUKOKETA       TO    RECE130-YUKOKETA(IDX4)
               END-IF
               MOVE    TNS-DATAKBN        TO    RECE130-DATAKBN (IDX4)
               MOVE    TNS-HOUKNSKBN      TO    RECE130-HOUKNSKBN(IDX4)
               MOVE    TNS-KNSJISKBN      TO    RECE130-KNSJISKBN(IDX4)
               MOVE    TNS-CDKBN-KBN      TO    RECE130-CDKBN-KBN(IDX4)
               MOVE    TNS-CDKBN-SYO      TO    RECE130-CDKBN-SYO(IDX4)
               MOVE    TNS-CDKBN-BU       TO    RECE130-CDKBN-BU(IDX4)
               MOVE    TNS-CDKBN-KBNNUM   
                                      TO RECE130-CDKBN-KBNNUM(IDX4)
               MOVE    TNS-CDKBN-KBNNUM-EDA
                                      TO RECE130-CDKBN-KBNNUM-EDA(IDX4)
               MOVE    TNS-CDKBN-KOUBAN
                                      TO RECE130-CDKBN-KOUBAN(IDX4)
               MOVE    TNS-KOUHYOJYUNNUM  
                                      TO RECE130-KOUHYOJYUNNUM(IDX4)
               MOVE    TNS-SRYSYUKBN  TO RECE130-SRYSYUKBN-X  (IDX4)
               MOVE    TNS-KOKUJISKBKBN1  
                                      TO RECE130-KOKUJISKBKBN1(IDX4)
               MOVE    TNS-KZMCOMPSIKIBETU
                                      TO RECE130-KZMCOMPSIKIBETU(IDX4)
               MOVE    TNS-CHUKSNCD   TO RECE130-CHUKSNCD       (IDX4)
               MOVE    TNS-CHUKSNTUBAN
                                      TO RECE130-CHUKSNTUBAN    (IDX4)
               MOVE    TNS-KENTAICOMMENT
                                      TO RECE130-KENTAICOMMENT  (IDX4)
               MOVE    TNS-KNSJISGRPKBN
                                      TO RECE130-KNSJISGRPKBN   (IDX4)
               MOVE    TNS-TOKUKIZAISBT1
                                      TO RECE130-TOKUKIZAISBT1  (IDX4)
               MOVE    TNS-SANSOKBN   TO RECE130-SANSOKBN       (IDX4)
      *
               IF    (TNS-SRYCD       =  "170022290"  OR
                                         "170022390"  OR
                                         "170022490")
               AND   (RECE100-SRYYM  <=  "201003"   )
                      MOVE   202      TO  RECE130-CDKBN-KBNNUM(IDX4)
                      MOVE   99       TO  RECE130-CDKBN-KOUBAN(IDX4)
               END-IF
               IF     TNS-SRYCD       =  "170016010"  OR
                                         "170020270"  OR  "170024470" OR
                                         "170024910"  OR  "170025010" OR
                                         "170025210"  OR  "170025310" OR
                                         "170025410"  OR  "170025510" OR
                                         "170025610"  OR  "170025710" OR
                                         "170025810"  OR  "170025910" OR
                                         "170026010"  OR  "170026110" OR
                                         "170026210"  OR  "170026310" OR
                                         "170026810"  OR
                                         "170000210"  OR  "170016910" OR
                                         "170017010"  OR  "170026710" OR
                                         "170028810"
                      MOVE   203      TO  RECE130-CDKBN-KBNNUM(IDX4)
                      MOVE   99       TO  RECE130-CDKBN-KOUBAN(IDX4)
               END-IF
      *
               IF    (TNS-SRYCD       =  "170022290")
               AND   (RECE100-SRYYM  >=  "201004"   )
                      MOVE   "1"      TO  RECE120-GAZOGEN
               END-IF
      *
               IF    ((SRY-SRYCD    (IDX2)(1:1)     =   "8"  )  OR
                      (SRY-SRYCD    (IDX2)(1:3)     =   "008"))
               AND   ( SRY-INPUTNUM (IDX2)      NOT =   ZERO  )
grpsys             MOVE    SRY-HOSPNUM          TO  PTCOM-HOSPNUM
                   MOVE    SRY-PTID             TO  PTCOM-PTID
                   MOVE    SRY-ZAINUM           TO  PTCOM-ZAINUM
                   MOVE    SRY-SRYCD    (IDX2)  TO  PTCOM-SRYCD
                   MOVE    SRY-INPUTNUM (IDX2)  TO  PTCOM-RENNUM
                   PERFORM 950-PTCOM-READ-SEC
                   MOVE    PTCOM-INPUTCOMENT    TO  RECE130-NAME (IDX4)
                                                    RECE130-INPUTCOMENT
                                                                (IDX4)
      *            
                   IF      SRY-SRYCD (IDX2)(1:1)
                                                =   "8"
                       IF      PTCOM-INPUTCHI1     =   SPACE
                           INITIALIZE                  ORCSKANACHKAREA
                           MOVE    "2"             TO  KANACHK-SYORI 
                           MOVE    TNS-NAME        TO  KANACHK-MAE-INPUT
                           CALL    "ORCSKANACHK"   USING
                                                       ORCSKANACHKAREA
                           IF    ( KANACHK-RC      =   ZERO )
                           AND   ( KANACHK-MAX     >   ZERO )
                               COMPUTE IDXX    =   KANACHK-MAX +   1
                               MOVE    PTCOM-INPUTCOMENT (IDXX:)
                                                       TO
                                           RECE130-INPUTCOMENT   (IDX4)
                           ELSE
                               MOVE    PTCOM-INPUTCOMENT
                                                       TO
                                           RECE130-INPUTCOMENT   (IDX4)
                           END-IF
                       ELSE
                           INITIALIZE              ORCSKANACHKAREA
                           MOVE    "2"             TO  KANACHK-SYORI 
                           STRING  PTCOM-INPUTCHI1 DELIMITED  BY  SPACE
                                   PTCOM-INPUTCHI2 DELIMITED  BY  SPACE
                                   PTCOM-INPUTCHI3 DELIMITED  BY  SPACE
                                   PTCOM-INPUTCHI4 DELIMITED  BY  SPACE
                                   INTO   KANACHK-MAE-INPUT
                           END-STRING
                           CALL    "ORCSKANACHK"   USING
                                                       ORCSKANACHKAREA
                           IF      KANACHK-RC      =   ZERO
                               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                                   TO
                                           RECE130-INPUTCOMENT   (IDX4)
                           END-IF
                       END-IF
                   END-IF
      *
      ***          IF    RECE120-COMCDFLG   NOT =   "2"
      ***             MOVE   "1"           TO    RECE120-COMCDFLG
      ***          END-IF
               ELSE
                   MOVE    TNS-NAME        TO    RECE130-NAME    (IDX4)
                                                 RECE130-INPUTCOMENT
                                                                 (IDX4)
                   MOVE    TNS-YUKOKETA    TO    RECE130-YUKOKETA(IDX4)
               END-IF
      *
               IF          SRY-SRYKBN       =  "21"
                    IF   SRY-SRYCD(IDX2)  =  "820000047" OR 
                                             "630010002"
                        MOVE     "2"       TO    RECE120-COMCDFLG
                    END-IF
               END-IF
               IF    (SRY-SRYCD    (IDX2)(1:1)     =   "8"  )  OR
                     (SRY-SRYCD    (IDX2)(1:3)     =   "008")
                    IF    RECE120-COMCDFLG   NOT =   "2"
                        MOVE     "1"       TO    RECE120-COMCDFLG
                    END-IF
               END-IF
      *
               IF    RECE120-COMCDFLG       =   "2"
                 IF  SRY-SRYCD(IDX2)  =  "820000047" OR
                                         "630010002"
                   CONTINUE
                 ELSE
                   ADD   999               TO  WRK-SURYOU-COUNT
                   ADD   WRK-SURYOU-COUNT  TO  RECE120-SURYOUTOTAL
                 END-IF
               END-IF
      *
               IF          SRY-SRYKBN       =     "31" OR "32"
                 IF        SRY-SRYCD(IDX2)  =    CONS-HIKAKIN
      ***                                  OR    CONS-SEIBUTU
      ***                                  OR    CONS-MAYAKU
                                           OR    CONS-JYOMYAKU
      ***                                  OR    CONS-JYOMYAKUKASAN
                   MOVE    SPACE           TO    RECE130-NAME(IDX4)
                 END-IF
               END-IF
               IF  ( SRY-SRYSYUKBN         =  "334"       )
               AND ((SRY-SRYCD(IDX2)       =  "810000001") OR
                    (SRY-SRYCD(IDX2)(1:3)  =  "008"      ))
               AND ( FLG-HOUTEN-COM        =   ZERO       )
                    PERFORM  VARYING  IDV  FROM  1  BY  2
                            UNTIL  (IDV            >   200 )
                             OR    (FLG-HOUTEN-COM =     1 )
                      IF   RECE130-NAME(IDX4)(IDV:2)   =   "Ë¬"
                         MOVE   SPACE  TO    RECE120-FLG-HOUTEN-COMMENT
                         MOVE    1     TO    FLG-HOUTEN-COM
                      END-IF
                    END-PERFORM
               END-IF
      *
               IF  ( SRY-SRYKBN     =  "60" OR "64" )
               AND ((SRY-SRYCD(IDX2)(1:1)  =  "8"  ) OR
                    (SRY-SRYCD(IDX2)(1:3)  =  "008"))
               AND ( FLG-KENSA-COM  =   ZERO        )
                 IF  SRY-SRYCD(IDX2) = "840000085" OR "840000087"
                    CONTINUE
                 ELSE
                    PERFORM  VARYING  IDV  FROM  1  BY  2
                              UNTIL  (IDV            >   200)
                               OR    (FLG-KENSA-COM  =    1 )
                      IF  RECE130-NAME(IDX4)(IDV:2)   =  "Æü"
                        MOVE   SPACE  TO  RECE120-FLG-KENSA-COMMENT
                        MOVE     1    TO  FLG-KENSA-COM
                      END-IF
                    END-PERFORM
                 END-IF
               END-IF
      *
               IF  SRY-SRYCD(IDX2)(1:3)     =   "058"
                 MOVE  TNS-FORMALYUKOKETA TO  RECE130-YUKOKETA(IDX4)
                 MOVE  TNS-FORMALNAME     TO  RECE130-NAME(IDX4)
      *
                 MOVE     TNS-TEN         TO  WRK-KZI058-TEN
                 IF   TNS-TANICD  NOT = SPACE AND "000"
                    MOVE  TNS-TANICD      TO  WRK-KZI058-TANICD
                    MOVE  TNS-TANIMOJISU  TO  WRK-KZI058-TANIMOJISU
                    MOVE  TNS-TANINAME    TO  WRK-KZI058-TANINAME
                 END-IF
                 MOVE     ZERO            TO  RECE130-TEN(IDX4)
                 MOVE     ZERO            TO  RECE130-TANICD(IDX4)
                 MOVE     ZERO            TO  RECE130-TANIMOJISU(IDX4)
                 MOVE     SPACE           TO  RECE130-TANINAME(IDX4)
               END-IF
               IF  (SRY-SRYCD(IDX2)(1:1)    =   "7")
               AND (SRY-MEISKYFLG(IDX2)     =   "1")
                 IF   (SRY-SRYKBN  =   "70")
                 AND  (TNS-DATAKBN =   "3" )
                    IF    RECE130-RENNUM  >  1
                    AND   IDX4            =  1
                      MOVE    1      TO  FLG-FORMAL-KISAI
                      COMPUTE  WRK-RENNUM2 =  RECE130-RENNUM  -  1
                    ELSE
                      MOVE  "1"      TO  RECE130-RECEPTFLG(IDX4 - 1)
                    END-IF
                 ELSE
                    MOVE  "1"        TO  RECE130-RECEHENFLG(IDX4)
                    IF    WRK-KZI058-TEN  NOT = ZERO
                       MOVE   WRK-KZI058-TEN
                                     TO  RECE130-TEN(IDX4)
                    END-IF
                    IF   (RECE130-TANICD(IDX4)   = ZERO)
                    AND  (WRK-KZI058-TANICD  NOT = ZERO)
                       MOVE   WRK-KZI058-TANICD
                                     TO  RECE130-TANICD(IDX4)
                       MOVE   WRK-KZI058-TANIMOJISU
                                     TO  RECE130-TANIMOJISU(IDX4)
                       MOVE   WRK-KZI058-TANINAME
                                     TO  RECE130-TANINAME(IDX4)
                    END-IF
                 END-IF
               END-IF
      *
               IF  (SRY-SRYKBN         =  "99"       )
               AND (SRY-RENNUM         =   1         )
               AND (IDX2               =   1         )
               AND (SRY-SRYCD(IDX2)    =  "099999902")
                    MOVE      1         TO  FLG-TAHOKEN
               END-IF
               IF  (FLG-TAHOKEN           =   1              )
               AND (SRY-RENNUM            =   1              )
               AND (IDX2                  =   2              )
               AND (SRY-SRYCD(IDX2)(1:4)  =  "0082"          )
               AND (TNS-NAME(1:14)        =  "ÊÝ¸±Ì¾¾ÎÊÑ¹¹¡§")
                  MOVE     "1"          TO  RECE130-RECEPTFLG(IDX4)
      *
                  MOVE      ZERO        TO  IDW3
                  PERFORM   VARYING   IDW   FROM   200   BY  -1
                             UNTIL    IDW     <    15
                       IF   TNS-NAME(IDW:1)  NOT =   SPACE
                           COMPUTE IDW6       =  (IDW / 2) - 7
                           MOVE    IDW        TO  IDW3
                           MOVE    1          TO  IDW
                       END-IF
                  END-PERFORM
                  IF     IDW3    NOT =  ZERO
                    MOVE  SPACE TO RECE130-NAME(IDX4 - 1)
                    COMPUTE   IDW4  =  IDW3  -  14
                    MOVE  TNS-NAME(15:IDW4)
                                TO RECE130-NAME(IDX4 - 1)(1:IDW4)
                    COMPUTE   IDW5  =  IDW4  +   1
                    MOVE  "¤Ë¤Æ¿Ç»¡ÎÁ»»ÄêºÑ"
                                TO RECE130-NAME(IDX4 - 1)(IDW5:16)
                    COMPUTE   RECE130-YUKOKETA(IDX4 - 1) =
                              RECE130-YUKOKETA(IDX4 - 1) -
                              3                         +
                              IDW6
                  END-IF
               END-IF
      *
               IF  RECE130-YUKOKETA(IDX4)  =  ZERO
                   PERFORM   VARYING   IDW   FROM   200   BY  -1
                           UNTIL   IDW   <   1
                       IF   RECE130-NAME(IDX4)(IDW:1)  NOT =   SPACE
                           COMPUTE RECE130-YUKOKETA(IDX4)  =
                                   IDW     /   2
                           MOVE    1          TO  IDW
                       END-IF
                   END-PERFORM
               END-IF
      *
      *        ÆÃÄê´ïºà¡Ê²èÁü¿ÇÃÇ¤Î¥Õ¥£¥ë¥à°Ê³°¡Ë¤Î¤È¤­Ã±²Á¤òÊÔ½¸
               IF      SRY-SRYCD (IDX2) (1:1)  =   "7"
      ***          IF   (( SRY-SRYKBN               =   "70" )  AND
      ***                ( TNS-DATAKBN              =   "3"  ))
                   IF   (  TNS-DATAKBN              =   "3"   )
                   OR   (  SRY-SRYCD (IDX2)         =
                                    CONS-FILM-YOUJI-KASAN  OR  
                                    CONS-SANSO             OR
                                    CONS-SANSO2               )
                       CONTINUE
                   ELSE
                     IF      TNS-TEN         =   ZERO
                       CONTINUE
                     ELSE
                       PERFORM 2104412-RECE130-TENSOU-TANKA-HEN-SEC
                       COMPUTE IDW =   RECE130-YUKOKETA(IDX4)  *  2 
                                                   +   3                    
                       MOVE    WRK-CHAR-N (1:WRK-CHAR-N-CNT)
                                                   TO
                                               RECE130-NAME(IDX4)(IDW:)   
                       COMPUTE RECE130-YUKOKETA(IDX4)
                                              =   RECE130-YUKOKETA(IDX4)
                                                   +   1   
                                                   +
                                                  (WRK-CHAR-N-CNT  /  2)
                     END-IF
                   END-IF
               END-IF        
      *----------------------------------------------------------------
      *        ºßÂðÉôÊ¬¤Î¥ì¥»¥×¥ÈÊÔ½¸ÍÑ¤Ë±ý¿Ç¡ÊÆÃÊÌ±ý¿Ç¡Ë¡¦Ìë´Ö¡¦
      *        ¿¼Ìë¡¦¶ÛµÞ¤ÎºÞÈ½Äê¤òÄÉ²Ã
      *----------------------------------------------------------------
               IF    SRY-SRYKBN     =  "14"
      *
                 IF (SRY-SRYCD(IDX2) =  CONS-ZAIHOU-COMMENT )
                 OR (SRY-SRYCD(IDX2) =  CONS-ZAIHOU3-COMMENT)
                 OR (SRY-SRYCD(IDX2) =  CONS-ZAIHOU4-COMMENT)
                    MOVE        9         TO    FLG-OSINKBN
                 END-IF
      *
                 EVALUATE    SRY-SRYCD(IDX2)
      *±ý¿Ç¡ÊÆÃÊÌ±ý¿Ç¡Ë
                   WHEN    "114000110"
                   WHEN    "114001610"
                   WHEN    "114000970"
                   WHEN    "114002470"
                   WHEN    "114002970"
                   IF  FLG-OSINKBN        NOT = 2  AND  3  AND  9
                       MOVE     1         TO    FLG-OSINKBN
                   END-IF
      *Ìë´Ö
                   WHEN    "114000470"
                   WHEN    "114001970"
                   WHEN    "114011670"
                   WHEN    "114011970"
                   WHEN    "114017570"
                   WHEN    "114017870"
                   IF  FLG-OSINKBN        NOT = 9
                       MOVE     2         TO    FLG-OSINKBN
                   END-IF
      *¿¼Ìë¡¦¶ÛµÞ
                   WHEN    "114000370"
                   WHEN    "114000570"
                   WHEN    "114001870"
                   WHEN    "114002070"
                   WHEN    "114011570"
                   WHEN    "114011770"
                   WHEN    "114011870"
                   WHEN    "114012070"
                   WHEN    "114017470"
                   WHEN    "114017770"
                   WHEN    "114017670"
                   WHEN    "114017970"
                   IF  FLG-OSINKBN        NOT = 9
                       MOVE     3         TO    FLG-OSINKBN
                   END-IF
                 END-EVALUATE
               END-IF
      *
               MOVE    ACCT-ZAIKBN  TO  RECE120-ZAIKBN
      *
               IF      ACCT-ZAIKBN       =     1
                 MOVE   ZERO        TO  RECE130-SRYSURYO(IDX4)
                 MOVE   ZERO        TO  RECE130-TANICD(IDX4)
                 MOVE   ZERO        TO  RECE130-TANIMOJISU(IDX4)
                 MOVE   SPACE       TO  RECE130-TANINAME(IDX4)
                 IF  (SRY-SRYCD(IDX2)(1:1) = "6" OR "7")
                 OR  (SRY-SRYCD(IDX2)(1:3) = "059"     )
                 OR  (SRY-SRYCD(IDX2)(1:2) = "63"      )
                     MOVE  "1"          TO  RECE120-COMCDFLG
                     MOVE  "810000001"  TO  RECE130-SRYCD(IDX4)
                 END-IF
               END-IF
               IF      ACCT-ZAIKBN       =     2
                 MOVE   ZERO        TO  RECE130-SRYSURYO(IDX4)
                 MOVE   ZERO        TO  RECE130-TANICD(IDX4)
                 MOVE   ZERO        TO  RECE130-TANIMOJISU(IDX4)
                 MOVE   SPACE       TO  RECE130-TANINAME(IDX4)
               END-IF
      *
              END-IF
             END-IF  
           END-PERFORM
      *
           IF    IDX4    NOT =   ZERO
               MOVE    1     TO    FLG-WRITE05X
           END-IF
      *
           .
       210441-RECE130-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È¥³¥á¥ó¥È¥Õ¥é¥°ÊÔ½¸½èÍý
      *****************************************************************
       2104411-RECE130-TENSOU-COMCNT-HEN-SEC               SECTION.
      *
           IF      RECE120-SRYKBN      =     "11"
      *       ½é¿ÇÎÁ»»Äê²Ê¡§
              IF ( SRY-SRYCD(IDX2)    =     "830000020"         )
              OR ((SRY-SRYCD(IDX2)    =     CONS-SYODOU-COMMENT) AND
                  (RECE100-SRYYM     >=     "201204"           ))
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-SYOSANTEIKA-COMMENT
              END-IF
      *       ½é¿Ç¡ÊÆ±ÆüÊ£¿ô¿ÇÎÅ²Ê¡Ë
              IF   SRY-SRYCD(IDX2)    =     CONS-SYODOU-COMMENT
              AND  FLG-SYODOU-COM     =     ZERO
                   MOVE   "1"   TO    RECE120-FLG-SYODOU-COMMENT
                   MOVE   SRY-SRYKA   TO    RECE120-SYODOU-SRYKA(1)
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-SYODOU2-COMMENT
                   MOVE  SPACE  TO    RECE120-FLG-SYODOU-COMMENT
                   MOVE  SPACE  TO    RECE120-SYODOU-SRYKA(1)
                   MOVE    1    TO    FLG-SYODOU-COM
              END-IF
           END-IF
      *
           IF      RECE120-SRYKBN      =     "12"
      *       ºÆ¿ÇÎÁ»»Äê²Ê¡§
              IF   SRY-SRYCD(IDX2)    =     "830000021"
                                      OR    CONS-DOUJITU-SAI
                                      OR    CONS-DOUJITU-DEN
                                      OR    CONS-DOUJITU-GAI
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-SAISANTEIKA-COMMENT
              END-IF
      *       ÅÅÏÃºÆ¿Ç
              IF  (SRY-SRYCD(IDX2)    =     "112009750"   OR
                                            "112009850"   OR
                                            "112009950"   OR
                                            "112010050"   OR
                                            "112012750"   OR
                                            "112704150"   OR
                                            "112704250"   OR
                                            "112704350"   OR
                                            "112706750"   OR
                                            "112010650"   OR
                                            "112010750"   OR
                                            "112010850"   OR
                                            "112010950"   OR
                                            "112012850"   OR
                                            "112704850"   OR
                                            "112704950"   OR
                                            "112705050"   OR
                                            "112706850"   OR
                                            "112007950"   OR
                                            "112008050"   OR
                                            "112008150"   OR
                                            "112008250"   OR
                                            "112012450"   OR
                                            "112702750"   OR
                                            "112702850"   OR
                                            "112702950"   OR
                                            "112706450"   OR
                                            "112008850"   OR
                                            "112008950"   OR
                                            "112009050"   OR
                                            "112009150"   OR
                                            "112012550"   OR
                                            "112703450"   OR
                                            "112703550"   OR
                                            "112703650"   OR
                                            "112706550"     )
              AND (RECE100-SRYYM      <     "201004"        )
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-DENWASAI-COMMENT
              END-IF
              IF  (SRY-SRYCD(IDX2)    =     "112007950"     )
              AND (RECE100-SRYYM      >=    "201004"        )
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-DENWASAI-COMMENT
              END-IF
      *       Æ±ÆüºÆ¿Ç
              IF  (SRY-SRYCD(IDX2)    =     "112010150"   OR
                                            "112010250"   OR
                                            "112010350"   OR
                                            "112010450"   OR
                                            "112010550"   OR
                                            "112704450"   OR
                                            "112704550"   OR
                                            "112704650"   OR
                                            "112704750"   OR
                                            "112010650"   OR
                                            "112010750"   OR
                                            "112010850"   OR
                                            "112010950"   OR
                                            "112012850"   OR
                                            "112704850"   OR
                                            "112704950"   OR
                                            "112705050"   OR
                                            "112706850"   OR
                                            "112008850"   OR
                                            "112008950"   OR
                                            "112009050"   OR
                                            "112009150"   OR
                                            "112012550"   OR
                                            "112703450"   OR
                                            "112703550"   OR
                                            "112703650"   OR
                                            "112706550"   OR
                                            "112008350"   OR
                                            "112008450"   OR
                                            "112008550"   OR
                                            "112008650"   OR
                                            "112008750"   OR
                                            "112703050"   OR
                                            "112703150"   OR
                                            "112703250"   OR
                                            "112703350"   OR
      *
                                            "112011710"   OR
                                            "112705850"     )
              AND (RECE100-SRYYM      <     "201004"        )
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-DOUJITUSAI-COMMENT
              END-IF
              IF  (SRY-SRYCD(IDX2)    =     "112008350"   OR
                                            "112011710"     )
              AND (RECE100-SRYYM      >=    "201004"        )
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-DOUJITUSAI-COMMENT
              END-IF
      *       Æ±ÆüÅÅÏÃºÆ¿Ç
              IF  (SRY-SRYCD(IDX2)    =     "112008850"     )
              AND (RECE100-SRYYM      >=    "201004"        )
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-DOUDENSAI-COMMENT
              END-IF
      *
      *       ºÆ¿Ç¡ÊÆ±°ìÆü£²²ÊÌÜ¡Ë
              IF  (SRY-SRYCD(IDX2)    =     CONS-DOUJITU-SAI
                                      OR    CONS-DOUJITU-DEN
                                      OR    CONS-DOUJITU-GAI)
              AND (FLG-SAIDOU-COM     =     ZERO            )
                 MOVE   "1"        TO   RECE120-FLG-SAIDOU-COMMENT
                 MOVE   SRY-SRYKA  TO   RECE120-SAIDOU-SRYKA(1)
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-DOUJITU-COMMENT
                 MOVE   SPACE      TO   RECE120-FLG-SAIDOU-COMMENT
                 MOVE   SPACE      TO   RECE120-SAIDOU-SRYKA(1)
                 MOVE    1         TO   FLG-SAIDOU-COM
              END-IF
           END-IF
      *
           IF      RECE120-SRYKBN      =     "40"
      *       ²ðÃ£¸£°ú¡ÊÈ¯¾ÉÆüµ­ºÜ¡Ë
              IF   RECE100-SRYYM      >=    "200604"
              AND  RECE100-SRYYM      <=    "200803"
                IF   SRY-SRYCD(IDX2)  =     CONS-KAITATU-COMMENT
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-KAITATU-COMMENT
                END-IF
              END-IF
      *
              IF   RECE100-SRYYM     >=    "201004"
      *         ¿Í¹©¿ÕÂ¡Åù
                IF   SRY-SRYCD(IDX2)  =     CONS-JINZOU1-COMMENT
                                        OR  CONS-JINZOU2-COMMENT
                                        OR  CONS-JINZOU3-COMMENT
                                        OR  CONS-JINZOU4-COMMENT
                                        OR  CONS-JINZOU5-COMMENT
                                        OR  CONS-JINZOU6-COMMENT
                                        OR  CONS-JINZOU7-COMMENT
                                        OR  CONS-JINZOU8-COMMENT
                                        OR  CONS-JINZOU9-COMMENT
                        MOVE   "1"    TO    RECE120-FLG-JINZOU-COMMENT
                END-IF
      *         ¶É½ê±¢°µÊÄº¿½èÃÖ
                IF   RECE100-SRYYM     >=    "201204"
                  IF   SRY-SRYCD(IDX2)  =     CONS-KYOKUIN5-COMMENT
                                          OR  CONS-KYOKUIN6-COMMENT
                                          OR  CONS-KYOKUIN7-COMMENT
                        MOVE   "1"    TO    RECE120-FLG-KYOKUIN-COMMENT
                  END-IF
                ELSE
                  IF   SRY-SRYCD(IDX2)  =     CONS-KYOKUIN1-COMMENT
                                          OR  CONS-KYOKUIN2-COMMENT
                                          OR  CONS-KYOKUIN3-COMMENT
                                          OR  CONS-KYOKUIN4-COMMENT
                        MOVE   "1"    TO    RECE120-FLG-KYOKUIN-COMMENT
                  END-IF
                END-IF
              END-IF
           END-IF
      *
           IF      RECE120-SRYKBN      =     "80"
      *       ¥ê¥Ï¥Ó¥ê
              IF   SRY-SRYCD(IDX2)    =     "180706910"   OR
                                            "180022210"   OR
                                            "180706810"   OR
                                            "180022110"   OR
                                            "180707110"   OR
                                            "180022410"   OR
                                            "180707010"   OR
                                            "180022310"   OR
                                            "180707310"   OR
                                            "180022610"   OR
                                            "180707210"   OR
                                            "180022510"   OR
                                            "180707510"   OR
                                            "180022810"   OR
                                            "180707410"   OR
                                            "180022710"   OR
                                            "180708210"   OR
                                            "180023710"   OR
                                            "180708410"   OR
                                            "180023910"   OR
                                            "180708110"   OR
                                            "180023610"   OR
                                            "180708310"   OR
                                            "180023810"   OR
                                            "180024910"   OR
                                            "180024810"   OR
                                            "180025110"   OR
                                            "180025010"   OR
                                            "180025510"   OR
                                            "180025410"
      ****              MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-RIHABIRI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-SINDAI1-COMMENT  OR
                                            CONS-SINDAI2-COMMENT  OR
                                            CONS-SINDAI3-COMMENT  OR
                                            CONS-SINDAI4-COMMENT  OR
                                            CONS-KOKYUKI1-COMMENT OR
                                            CONS-KOKYUKI2-COMMENT OR
                                            CONS-KOKYUKI3-COMMENT OR
                                            CONS-KOKYUKI4-COMMENT OR
                                            CONS-NOUKE1-COMMENT   OR
                                            CONS-NOUKE2-COMMENT   OR
                                            CONS-NOUKE3-COMMENT   OR
                                            CONS-NOUKE4-COMMENT   OR
                                            CONS-NOUKE7-COMMENT   OR
                                            CONS-NOUKE8-COMMENT   OR
                                            CONS-NOUKE9-COMMENT   OR
                                            CONS-NOUKE10-COMMENT  OR
                                            CONS-NOUKE11-COMMENT  OR
                                            CONS-NOUKE12-COMMENT  OR
                                            CONS-NOUKE13-COMMENT  OR
                                            CONS-NOUKE14-COMMENT  OR
                                            CONS-NOUKE15-COMMENT  OR
                                            CONS-NOUKE16-COMMENT  OR
                                            CONS-UNDOKI1-COMMENT  OR
                                            CONS-UNDOKI2-COMMENT  OR
                                            CONS-UNDOKI3-COMMENT  OR
                                            CONS-UNDOKI4-COMMENT  OR
                                            CONS-UNDOKI7-COMMENT  OR
                                            CONS-UNDOKI8-COMMENT  OR
                                            CONS-UNDOKI9-COMMENT  OR
                                            CONS-UNDOKI10-COMMENT OR
                                            CONS-SYOGAI1-COMMENT  OR
                                            CONS-SYOGAI2-COMMENT  OR
                                            CONS-SYOGAI3-COMMENT  OR
                                            CONS-GANKNJ-COMMENT
      ****              MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-RIHABIRI-COMMENT
              END-IF
              EVALUATE      SRY-SRYCD(IDX2)
                WHEN     CONS-SINDAI5-COMMENT
                        MOVE   "2"    TO    RECE120-RIHABIRI-COMMENT
                WHEN     CONS-SINDAI6-COMMENT
                        MOVE   "3"    TO    RECE120-RIHABIRI-COMMENT
                WHEN     CONS-KOKYUKI5-COMMENT
                        MOVE   "4"    TO    RECE120-RIHABIRI-COMMENT
                WHEN     CONS-KOKYUKI6-COMMENT
                        MOVE   "5"    TO    RECE120-RIHABIRI-COMMENT
                WHEN     CONS-NOUKE5-COMMENT
                        MOVE   "6"    TO    RECE120-RIHABIRI-COMMENT
                WHEN     CONS-NOUKE6-COMMENT
                        MOVE   "7"    TO    RECE120-RIHABIRI-COMMENT
                WHEN     CONS-UNDOKI5-COMMENT
                        MOVE   "8"    TO    RECE120-RIHABIRI-COMMENT
                WHEN     CONS-UNDOKI6-COMMENT
                        MOVE   "9"    TO    RECE120-RIHABIRI-COMMENT
              END-EVALUATE
      *       ¥ê¥Ï¥Ó¥ê¡ÊÈ¯¾ÉÆüµ­ºÜ¡Ë
              IF   RECE100-SRYYM      >=    "200604"
                IF   SRY-SRYCD(IDX2)  =     CONS-SINDAI1-COMMENT
                                      OR    CONS-SINDAI2-COMMENT
                                      OR    CONS-SINDAI3-COMMENT
                                      OR    CONS-SINDAI4-COMMENT
                                      OR    CONS-SINDAI5-COMMENT
                                      OR    CONS-SINDAI6-COMMENT
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-SINDAI-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-KOKYUKI1-COMMENT
                                      OR    CONS-KOKYUKI2-COMMENT
                                      OR    CONS-KOKYUKI3-COMMENT
                                      OR    CONS-KOKYUKI4-COMMENT
                                      OR    CONS-KOKYUKI5-COMMENT
                                      OR    CONS-KOKYUKI6-COMMENT
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-KOKYUKI-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-NOUKE1-COMMENT
                                      OR    CONS-NOUKE2-COMMENT
                                      OR    CONS-NOUKE3-COMMENT
                                      OR    CONS-NOUKE4-COMMENT
                                      OR    CONS-NOUKE5-COMMENT
                                      OR    CONS-NOUKE6-COMMENT
                                      OR    CONS-NOUKE7-COMMENT
                                      OR    CONS-NOUKE8-COMMENT
                                      OR    CONS-NOUKE9-COMMENT
                                      OR    CONS-NOUKE10-COMMENT
                                      OR    CONS-NOUKE11-COMMENT
                                      OR    CONS-NOUKE12-COMMENT
                                      OR    CONS-NOUKE13-COMMENT
                                      OR    CONS-NOUKE14-COMMENT
                                      OR    CONS-NOUKE15-COMMENT
                                      OR    CONS-NOUKE16-COMMENT
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-NOUKE-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-UNDOKI1-COMMENT
                                      OR    CONS-UNDOKI2-COMMENT
                                      OR    CONS-UNDOKI3-COMMENT
                                      OR    CONS-UNDOKI4-COMMENT
                                      OR    CONS-UNDOKI5-COMMENT
                                      OR    CONS-UNDOKI6-COMMENT
                                      OR    CONS-UNDOKI7-COMMENT
                                      OR    CONS-UNDOKI8-COMMENT
                                      OR    CONS-UNDOKI9-COMMENT
                                      OR    CONS-UNDOKI10-COMMENT
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-UNDOKI-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-NANBYO-COMMENT
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-NANBYO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-SYOGAI1-COMMENT
                                      OR    CONS-SYOGAI2-COMMENT
                                      OR    CONS-SYOGAI3-COMMENT
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-SYOGAI-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-SESSYOKU-COMMENT
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-SESSYOKU-COMMENT
                END-IF
              END-IF
      *
              IF   RECE100-SRYYM     >=    "201004"
      *         Ç§ÃÎÎÅË¡¡¦Ç§ÃÎ¹ÔÆ°ÎÅË¡
                IF   SRY-SRYCD(IDX2)  =     CONS-NINRYO-COMMENT
                        MOVE   "1"    TO    RECE120-FLG-NINRYO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-NINRYO2-COMMENT
                        MOVE   "2"    TO    RECE120-FLG-NINRYO-COMMENT
                END-IF
              END-IF
           END-IF
      *
           IF      RECE120-SRYKBN      =     "13"
              IF   SRY-SRYCD(IDX2)    =     CONS-MANSEI
                   MOVE   "1"   TO    RECE120-FLG-SHIDOU-COMMENT
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     CONS-SYOBYO-COMMENT
              AND  FLG-SYOBYO-COM     =     ZERO
                   MOVE   "1"   TO    RECE120-FLG-SYOBYO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-SYOBYO2-COMMENT
                                      OR    CONS-SYOBYO3-COMMENT
                   MOVE  SPACE  TO    RECE120-FLG-SYOBYO-COMMENT
                   MOVE    1    TO    FLG-SYOBYO-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     CONS-RYOYO-COMMENT
              AND  FLG-RYOYO-COM      =     ZERO
                   MOVE   "1"   TO    RECE120-FLG-RYOYO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-RYOYO2-COMMENT
                   MOVE  SPACE  TO    RECE120-FLG-RYOYO-COMMENT
                   MOVE    1    TO    FLG-RYOYO-COM
              END-IF
      *
              IF  (SRY-SRYCD(IDX2)    =     CONS-NIKOCHIN-COMMENT
                                      OR    CONS-NIKOCHIN2-COMMENT
                                      OR    CONS-NIKOCHIN3-COMMENT)
              AND (FLG-NIKOCHIN-COM   =     ZERO                  )
                   MOVE   "1"   TO    RECE120-FLG-NIKOCHIN-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-NIKOCHIN4-COMMENT
                   MOVE  SPACE  TO    RECE120-FLG-NIKOCHIN-COMMENT
                   MOVE    1    TO    FLG-NIKOCHIN-COM
              END-IF
      *
              IF  (SRY-SRYCD(IDX2)    =     CONS-ZENSOKU-COMMENT
                                      OR    CONS-ZENSOKU2-COMMENT)
              AND (FLG-ZENSOKU-COM    =     ZERO                 )
                   MOVE   "1"   TO    RECE120-FLG-ZENSOKU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-ZENSOKU3-COMMENT
                   MOVE  SPACE  TO    RECE120-FLG-ZENSOKU-COMMENT
                   MOVE    1    TO    FLG-ZENSOKU-COM
              END-IF
      *
              IF   RECE100-SRYYM    >=     "200804"
                IF   (SRY-SRYCD(IDX2)   =     CONS-SYOUNI1-COMMENT)
                AND  (FLG-SYOUNI-COM    =     ZERO                )
                   MOVE   "1"   TO    RECE120-FLG-SYOUNI-COMMENT
                   MOVE   "1"   TO    RECE100-SYOUNIFLG
                END-IF
                IF   (SRY-SRYCD(IDX2)   =     CONS-SYOUNI2-COMMENT)
                AND  (FLG-SYOUNI-COM    =     ZERO                )
                   MOVE   "2"   TO    RECE120-FLG-SYOUNI-COMMENT
                END-IF
                IF    SRY-SRYCD(IDX2)   =     CONS-SYOUNI3-COMMENT
                   MOVE  SPACE  TO    RECE120-FLG-SYOUNI-COMMENT
                   MOVE    1    TO    FLG-SYOUNI-COM
                END-IF
              END-IF
      *
              IF   RECE100-SRYYM     >=    "201004"
      *         ¿´Â¡¥Ú¡¼¥¹¥á¡¼¥«¡¼»ØÆ³´ÉÍýÎÁ¡Ê±ó³Ö¥â¥Ë¥¿¥ê¥ó¥°¡Ë
                IF   SRY-SRYCD(IDX2)  =     CONS-SINPESU-COMMENT
                        MOVE   "1"    TO    RECE120-FLG-SINPESU-COMMENT
                END-IF
      *         ½¸ÃÄ±ÉÍÜ¿©»ö»ØÆ³ÎÁ
                IF   SRY-SRYCD(IDX2)  =     CONS-SYUEIYO-COMMENT
                        MOVE   "1"    TO    RECE120-FLG-SYUEIYO-COMMENT
                END-IF
              END-IF
      *
              IF   RECE100-SRYYM     >=    "201204"
                IF   SRY-SRYCD(IDX2)  =     CONS-NINSEN-COMMENT
                        MOVE   "1"    TO    RECE120-FLG-NINSEN-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-TAKYO-COMMENT
                        MOVE   "1"    TO    RECE100-COMCNTFLG
                        MOVE   "1"    TO    RECE120-FLG-TAKYO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-TAKYOKSN-COMMENT
                        MOVE   "1"    TO    RECE120-FLG-TAKYOKSN-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-GAIRIHA1-COMMENT
                                      OR    CONS-GAIRIHA2-COMMENT
                                      OR    CONS-GAIHOU-COMMENT
                        MOVE   "1"    TO    RECE120-FLG-GAIRIHA-COMMENT
                END-IF
              END-IF
           END-IF
      *
           IF      RECE120-SRYKBN      =     "14"
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUSIJI-COMMENT
              AND  FLG-HOUSIJI-COM    =     ZERO
                   MOVE   "1"   TO    RECE120-FLG-HOUSIJI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUSIJI2-COMMENT
                   MOVE  SPACE  TO    RECE120-FLG-HOUSIJI-COMMENT
                   MOVE    1    TO    FLG-HOUSIJI-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     CONS-HINKAI-COMMENT
                   MOVE   "1"   TO    RECE120-FLG-HINKAI-COMMENT
              END-IF
      *
              IF  (SRY-SRYCD(IDX2)    =     CONS-OSIN-COMMENT
                                      OR    CONS-OSIN2-COMMENT)
              AND (FLG-OSIN-COM       =     ZERO              )
                   MOVE   "1"   TO    RECE120-FLG-OSIN-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-OSIN3-COMMENT
                   MOVE  SPACE  TO    RECE120-FLG-OSIN-COMMENT
                   MOVE    1    TO    FLG-OSIN-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     CONS-ZAIIKAN1-COMMENT
                                      OR    CONS-ZAIIKAN2-COMMENT
                                      OR    CONS-ZAIIKAN3-COMMENT
                                      OR    CONS-ZAIIKAN4-COMMENT
                                      OR    CONS-ZAIIKAN5-COMMENT
                                      OR    CONS-ZAIIKAN6-COMMENT
                                      OR    CONS-ZAIIKAN7-COMMENT
                                      OR    CONS-ZAIIKAN8-COMMENT
                                      OR    CONS-ZAIIKAN9-COMMENT
                                      OR    CONS-ZAIIKAN10-COMMENT
                                      OR    CONS-ZAIIKAN11-COMMENT
                                      OR    CONS-ZAIIKAN12-COMMENT
                                      OR    CONS-ZAIIKAN13-COMMENT
                                      OR    CONS-ZAIIKAN14-COMMENT
                                      OR    CONS-ZAIIKAN15-COMMENT
                                      OR    CONS-ZAIIKAN16-COMMENT
                                      OR    CONS-ZAIHOU-COMMENT
                                      OR    CONS-ZAIHOU3-COMMENT
                                      OR    CONS-ZAIHOU4-COMMENT
                   MOVE   "1"   TO    RECE100-COMCNTFLG
                   MOVE   "2"   TO    RECE120-FLG-OSIN-COMMENT
              END-IF
      *
              IF   (SRY-SRYCD(IDX2)   =     CONS-ZAIHOU-COMMENT
                                      OR    CONS-ZAIHOU3-COMMENT
                                      OR    CONS-ZAIHOU4-COMMENT)
              AND  (FLG-ZAIHOU-COM    =     ZERO                )
                   MOVE   "1"   TO    RECE120-FLG-ZAIHOU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-ZAIHOU2-COMMENT
                   MOVE  SPACE  TO    RECE120-FLG-ZAIHOU-COMMENT
                   MOVE    1    TO    FLG-ZAIHOU-COM
              END-IF
      *
              IF   RECE100-SRYYM    >=     "201204"
                IF   SRY-SRYCD(IDX2) =  CONS-OSIN-COMMENT
                                    OR  CONS-OSIN2-COMMENT
                   IF    (WRK-SISETU3168  =  1)
                   OR    (WRK-SISETU3055  =  1)
                   OR    (WRK-SISETU3056  =  1)
                       MOVE   "1"   TO    RECE100-COMCNTFLG
                       MOVE   "1"   TO    RECE120-FLG-ZAISIEN-COMMENT
                   END-IF
                   IF  RECE120-FLG-ZAISIEN-COMMENT = SPACE
                     IF  (WRK-SISETU3169  =  1)
                     OR  (WRK-SISETU3057  =  1)
                     OR  (WRK-SISETU3058  =  1)
                       MOVE   "1"   TO    RECE100-COMCNTFLG
                       MOVE   "2"   TO    RECE120-FLG-ZAISIEN-COMMENT
                     END-IF
                   END-IF
                END-IF
              ELSE
                IF   WRK-SISETU613   =  1
                  IF   RECE100-SRYYM   >=  "200604"
                  AND  RECE100-SRYYM   <=  "200803"
                      IF SRY-SRYCD(IDX2) =  CONS-ZAIHOU-COMMENT OR
                                            CONS-ZAISIEN1       OR
                                            CONS-ZAISIEN2       OR
                                            CONS-ZAISIEN3
                        MOVE   "1"   TO    RECE100-COMCNTFLG
                        MOVE   "1"   TO    RECE120-FLG-ZAISIEN-COMMENT
                      END-IF
                  END-IF
                  IF   RECE100-SRYYM   >=  "200804"
                      IF SRY-SRYCD(IDX2) =  CONS-ZAISIEN1       OR
                                            CONS-ZAISIEN2       OR
                                            CONS-ZAISIEN3
                        MOVE   "1"   TO    RECE100-COMCNTFLG
                        MOVE   "1"   TO    RECE120-FLG-ZAISIEN-COMMENT
                      END-IF
                  END-IF
                ELSE
                  IF   WRK-SISETU695   =  1
                    IF   RECE100-SRYYM   >=  "200804"
                      IF SRY-SRYCD(IDX2) =  CONS-ZAISIEN1       OR
                                            CONS-ZAISIEN2       OR
                                            CONS-ZAISIEN3
                        MOVE   "1"   TO    RECE100-COMCNTFLG
                        MOVE   "2"   TO    RECE120-FLG-ZAISIEN-COMMENT
                      END-IF
                    END-IF
                  END-IF
                END-IF
              END-IF
      *
              IF   RECE100-SRYYM    >=     "201204"
                IF   SRY-SRYCD(IDX2) =  CONS-KAISYOKU-COMMENT
                   MOVE   "1"   TO    RECE120-FLG-KAISYOKU-COMMENT
                END-IF
              END-IF
           END-IF
      *
           IF      RECE120-SRYKBN      =     "33"
              IF   SRY-SRYSYUKBN      =     "334"
              AND  FLG-HOUTEN-COM     =     ZERO
                   MOVE   "1"   TO    RECE120-FLG-HOUTEN-COMMENT
              END-IF
           END-IF
      *
           IF      RECE120-SRYKBN      =     "50"
              IF   RECE120-SYUTEN1     =     ZERO
                   MOVE   "1"   TO    RECE120-FLG-SHUJUTU-COMMENT
              END-IF
              IF   SRY-SRYSYUKBN      =     "510"
                   MOVE   "1"   TO    RECE120-FLG-SHUJUTU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-SHUJUTU-COMMENT
                   MOVE   "1"   TO    RECE120-FLG-SHUJUTU-COMMENT
              END-IF
           END-IF
      *
           IF   RECE100-SRYYM     >=    "201004"
            IF     RECE120-SRYKBN     =     "54"
              IF   RECE120-SYUTEN1    =     ZERO
                   MOVE   "1"   TO    RECE120-FLG-MASUI-COMMENT
              END-IF
            END-IF
           END-IF
      *
           IF      RECE120-SRYKBN      =     "70"
              IF  (SRY-SRYCD(IDX2)    =     CONS-CTMRI-COMMENT1  OR
                                            CONS-CTMRI-COMMENT2  OR
                                            CONS-CTMRI-COMMENT3  OR
                                            CONS-CTMRI-COMMENT4  OR
                                            CONS-CTMRI-COMMENT5  OR
                                            CONS-CTMRI-COMMENT6  OR
                                            CONS-CTMRI-COMMENT7  OR
                                            CONS-CTMRI-COMMENT8  OR
                                            CONS-CTMRI-COMMENT9  OR
                                            CONS-CTMRI-COMMENT10 OR
                                            CONS-CTMRI-COMMENT11 OR
                                            CONS-CTMRI-COMMENT12 OR
                                            CONS-CTMRI-COMMENT13 OR
                                            CONS-CTMRI-COMMENT14 OR
                                            CONS-CTMRI-COMMENT15 OR
                                            CONS-CTMRI-COMMENT16 OR
                                            CONS-CTMRI-COMMENT17)
              AND (RECE100-SRYYM      <     "201004"            )
                   MOVE   "1"   TO    RECE120-FLG-CTMRI-COMMENT
              END-IF
              IF  (SRY-SRYCD(IDX2)    =     CONS-CTMRI-COMMENT1  OR
                                            CONS-CTMRI-COMMENT2  OR
                                            CONS-CTMRI-COMMENT4  OR
                                            CONS-CTMRI-COMMENT6  OR
                                            CONS-CTMRI-COMMENT7  OR
                                            CONS-CTMRI-COMMENT18 OR
                                            CONS-CTMRI-COMMENT19 OR
                                            CONS-CTMRI-COMMENT20)
              AND (RECE100-SRYYM      >=    "201004"            )
                   MOVE   "1"   TO    RECE120-FLG-CTMRI-COMMENT
              END-IF
           END-IF
      *
           IF      RECE120-SRYKBN      =     "60" OR "64"
              IF   (SRY-SRYCD(IDX2)   =     CONS-KENSA1-COMMENT
                                      OR    CONS-KENSA2-COMMENT
                                      OR    CONS-KENSA3-COMMENT)
              AND  (FLG-KENSA-COM     =     ZERO               )
                   MOVE   "1"   TO    RECE120-FLG-KENSA-COMMENT
              END-IF
           END-IF
      *
           IF      RECE120-SRYKBN   =  "14" OR "40" OR "50" OR "54"
                                    OR "60" OR "70" OR "80" OR "64"
              SET     TBL-SANSO    TO  1
              SEARCH      TBL-SANSO-OCC   VARYING   TBL-SANSO
                   AT  END
                       MOVE    ZERO            TO  FLG-SANSO-OK
                   WHEN    SRY-SRYCD (IDX2)    =   TBL-SANSO-SRYCD
                                                        (TBL-SANSO)
                       MOVE    1               TO  FLG-SANSO-OK
              END-SEARCH
              IF      FLG-SANSO-OK        =    1
                   MOVE   "1"   TO    RECE120-FLG-SANSO-COMMENT
              END-IF
           END-IF
      *
           .
       2104411-RECE130-TENSOU-COMCNT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    »ÀÁÇ¤ÎÃ±²ÁÊÔ½¸½èÍý
      *****************************************************************
       2104412-RECE130-TENSOU-TANKA-HEN-SEC     SECTION.
      *
      ***  IF      TNS-TEN         =   ZERO
      ***      GO  TO  2104412-RECE130-TENSOU-TANKA-HEN-EXT  
      ***  END-IF
      *
           MOVE    SPACE           TO  WRK-TANKA-X
      * 
      *    ¿ô»úÉ½¼¨ÍÑÊÔ½¸¡ÊÈ¾³Ñ¡Ë
           MOVE    TNS-TEN         TO  WRK-TANKA
           MOVE    WRK-TANKA-S1    TO  WRK-TANKA-Z1
           MOVE    ZERO            TO  FLG-SP
           PERFORM   VARYING   IDY   FROM    2   BY  -1
                     UNTIL     IDY           =   ZERO
             IF  WRK-TANKA-S2(IDY:1) NOT =   ZERO
                 MOVE    1               TO  FLG-SP
             END-IF
             IF  FLG-SP                  =   1
                 MOVE    WRK-TANKA-S2(IDY:1)
                                         TO  WRK-TANKA-Z3(IDY:1)
             END-IF
           END-PERFORM
           IF  WRK-TANKA-Z3          NOT =   SPACE
               MOVE    "."               TO  WRK-TANKA-Z2
           END-IF
      *        
           INITIALIZE                      WRK-CHAR-HENKAN-AREA
           MOVE    WRK-TANKA-X         TO  WRK-CHAR-X
           MOVE    12                  TO  WRK-CHAR-CNT
           MOVE    1                   TO  WRK-CHAR-IDX
           MOVE    ZERO                TO  WRK-CHAR-N-CNT
      *     
           PERFORM VARYING     IDW     FROM    1   BY  1
                   UNTIL       IDW     >   WRK-CHAR-CNT
               IF      WRK-CHAR-X (IDW:1)  =   SPACE
                   CONTINUE
               ELSE        
                   IF    WRK-CHAR-X (IDW:1)  IS  NUMERIC
                       IF      WRK-CHAR-X (IDW:1)  =   "0"
                           MOVE    TBL-SUJI (10)
                                       TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                       ELSE
                           MOVE    WRK-CHAR-X (IDW:1)  TO  NUM-IDX
                           MOVE    TBL-SUJI (NUM-IDX)
                                       TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                       END-IF                    
                   ELSE
                       IF    WRK-CHAR-X (IDW:1)  =   "."
                           MOVE    "¡¥"   TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                       ELSE    
                           MOVE    "  "   TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                       END-IF
                   END-IF    
                   ADD     2           TO  WRK-CHAR-IDX
                                           WRK-CHAR-N-CNT
               END-IF    
           END-PERFORM
      *
           IF      WRK-CHAR-N-CNT      >   ZERO
                   MOVE    "±ß"        TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                   ADD     2           TO  WRK-CHAR-N-CNT
           END-IF                             
      *
           .
       2104412-RECE130-TENSOU-TANKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷£²¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       210442-RECE100-HENSYU-SEC               SECTION.
      *
      *    ÆÃÄê¼À´µ½èÊý´ÉÍý²Ã»»¤Î»»Äê²ó¿ô¸º»»ÍÑÎÎ°è¤Î½é´ü²½
           MOVE   ZERO              TO   WRK-TOKUTEI-SKAN
           MOVE   ZERO              TO   WRK-TOKUTEI-SKAN2
      *    ½èÊý¤ÎÆýÍÄ»ù²Ã»»¤Î»»Äê²ó¿ô¸º»»ÍÑÎÎ°è¤Î½é´ü²½
           MOVE   ZERO              TO   WRK-NYUYOUJI-SKAN
      *    ½èÊý¤»¤ó(ÆýÍÄ»ù²Ã»»)¤Î»»Äê²ó¿ô¸º»»ÍÑÎÎ°è¤Î½é´ü²½
           MOVE   ZERO              TO   WRK-NYUYOUJI-SHOKAN
      *    ½èÊýÎÁ¡ÊËã¡¦¸þ¡¦ÆÇ¡¦³Ð¡Ë¤Î»»Äê²ó¿ô¸º»»ÍÑÎÎ°è¤Î½é´ü²½
           MOVE   ZERO              TO   WRK-MADOKU-SHOHO
      *    Ä¹´üÅêÌô²Ã»»¤Î»»Äê²ó¿ô¸º»»ÍÑÎÎ°è¤Î½é´ü²½
           MOVE   ZERO              TO   WRK-CYOKI-SKAN
           MOVE   ZERO              TO   WRK-CYOKI-SKAN2
      *    ¹³°­À­¼ðáçºÞ½èÊý´ÉÍý²Ã»»¤Î»»Äê²ó¿ô¸º»»ÍÑÎÎ°è¤Î½é´ü²½
           MOVE   ZERO              TO   WRK-AKUSEI-SKAN
                                         WRK-AKUSEI-SKAN2
      *    °ìÈÌÌ¾½èÊý²Ã»»¤Î»»Äê²ó¿ô¸º»»ÍÑÎÎ°è¤Î½é´ü²½
           MOVE   ZERO              TO   WRK-IPPAN-SKAN2
      *
      *
      *    ¸øÈñ¤ÎÊ¬Î¥É½¼¨ÍÑ¥¤¥ó¥Ç¥Ã¥¯¥¹¤Î³ÎÄê
           IF   FLG-SHOSAISIN        =       1                      
                PERFORM    VARYING   ACCT-IDX   FROM   1    BY   1  
                           UNTIL    ACCT-IDX   >      31            
                  IF  ACCT-DAY(1 ACCT-IDX)   NOT =    ZERO          
                      MOVE    "1"       TO                      
                                        TBL-JNISSU-DAY(ACCT-IDX)      
                  END-IF                                            
                END-PERFORM                                         
           END-IF
           IF   FLG-SHOSAISIN-GEN    =       1
                PERFORM    VARYING   ACCT-IDX   FROM   1    BY   1
                           UNTIL    ACCT-IDX   >      31
                  IF  ACCT-DAY(1 ACCT-IDX)   NOT =    ZERO
                      MOVE    "1"       TO
                                    TBL-JNISSU-DAY-GEN(ACCT-IDX)
                  END-IF
                END-PERFORM
           END-IF
      *
           PERFORM   VARYING  IDX3   FROM   1   BY   1
                     UNTIL    (IDX3         >     5)        OR
                              (SRY-SRYCD(IDX3)   =   SPACE)
               IF    SRY-RENNUM     =     1
                   IF  IDX3         =     1
                       MOVE  ACCT-ZAIKAISU     TO   WK-ZAIKAISU
                       MOVE  ACCT-ZAITEN       TO   WK-ZAITEN
                       IF    SRY-SRYKBN  =  "31" OR "32" OR "33" OR
                                            "40" OR "50" OR "54" OR
                                            "60" OR "70" OR "80" OR
                                            "64"
                           MOVE   ACCT-SYUTEN1   TO   WK-SYUTEN
                           COMPUTE  WK-YKZTEN  =  ACCT-KIZAITEN
                                               +  ACCT-YKZTEN
                           IF  SRY-SRYKBN  =  "70"
                             COMPUTE  WK-SYUTEN  =  WK-SYUTEN
                                                 -  ACCT-SYUTEN2
                             COMPUTE  WK-YKZTEN  =  WK-YKZTEN
                                                 +  ACCT-SYUTEN2
                           END-IF
                       END-IF
                       IF    SRY-SRYKBN     =  "14"
                           MOVE    ACCT-SYUTEN1   TO   WK-SYUTEN
                           MOVE    ACCT-KIZAITEN  TO   WK-KIZAITEN
                           MOVE    ACCT-YKZTEN    TO   WK-YKZTEN
                       END-IF
                   ELSE
                       IF ( SRY-SRYKBN   =  "21"  OR  "22"  OR  "23" OR
                                            "31"  OR  "32"  OR  "33" OR
                                            "40"  OR  "50"  OR  "54" OR
                                            "60"  OR  "70"  OR  "80" OR
                                            "13"  OR  "14"  OR  "64" )
                       OR ((ACCT-ZAIKBN  =   2  ) AND
                           (SRY-SRYKBN   =  "12"))
                           MOVE  ZERO          TO   WK-ZAIKAISU
                           MOVE  ZERO          TO   WK-ZAITEN
                           MOVE  ZERO          TO   WK-SYUTEN
                           MOVE  ZERO          TO   WK-KIZAITEN
                           MOVE  ZERO          TO   WK-YKZTEN
      ***                  MOVE  ZERO          TO   FLG-OSINKBN
                       END-IF
                   END-IF
               ELSE
                     MOVE    ZERO     TO    WK-ZAIKAISU
                     MOVE    ZERO     TO    WK-ZAITEN
                     MOVE    ZERO     TO    WK-SYUTEN
                     MOVE    ZERO     TO    WK-KIZAITEN
                     MOVE    ZERO     TO    WK-YKZTEN
      ***            MOVE    ZERO     TO    FLG-OSINKBN
               END-IF
               IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                    (ACCT-ZAIKAISU  NOT =    ZERO)
                     PERFORM   2104421-RECE-LEFT-SET-SEC
               END-IF
               IF  ( SRY-SRYKBN   =   "21"   OR   "22"   OR   "23"   OR
                                      "31"   OR   "32"   OR   "33"   OR
                                      "50"   OR   "54"   OR   "60"   OR
                                      "70"   OR   "64"     )
               OR  ((ACCT-ZAIKBN  =    2                  ) AND
                    (SRY-SRYKBN   =   "12" OR "13" OR "14"))
                     MOVE    6      TO    IDX3
               END-IF
           END-PERFORM
      *
      *    ½èÊý¤Î»»Äê²ó¿ô¤«¤éÆÃÄê¼À´µ½èÊý´ÉÍý²Ã»»¤Î¸º»»
           COMPUTE    RECE100-KAISU(21)   =    RECE100-KAISU(21)  -
                                              WRK-TOKUTEI-SKAN
      *
      *    ½èÊý¤Î»»Äê²ó¿ô¤«¤é½èÊý(ÆýÍÄ»ù)²Ã»»¤Î¸º»»
           COMPUTE    RECE100-KAISU(21)   =    RECE100-KAISU(21)  -
                                              WRK-NYUYOUJI-SKAN           
      *
      *    ½èÊý¤Î»»Äê²ó¿ô¤«¤éÄ¹´üÅêÌô²Ã»»¡Ê½èÊýÎÁ)¤Î¸º»»
           COMPUTE    RECE100-KAISU(21)   =    RECE100-KAISU(21)  -
                                              WRK-CYOKI-SKAN     
      *
      *    ½èÊý¤Î»»Äê²ó¿ô¤«¤é¹³°­À­¼ðáçºÞ½èÊý´ÉÍý²Ã»»¡Ê½èÊýÎÁ¡Ë¤Î¸º»»
           COMPUTE    RECE100-KAISU(21)   =    RECE100-KAISU(21)  -
                                              WRK-AKUSEI-SKAN
      *
      *    ½èÊý¤Î»»Äê²ó¿ô¤«¤éÆÃÄê¼À´µ½èÊý´ÉÍý²Ã»»(½èÊýÎÁ)¤Î¸º»»
           COMPUTE    RECE100-KAISU(36)   =    RECE100-KAISU(36)  -
                                              WRK-TOKUTEI-SKAN2           
      *
      *    ½èÊý¤»¤ó¤Î»»Äê²ó¿ô¤«¤é½èÊý¤»¤ó(ÆýÍÄ»ù)²Ã»»¤Î¸º»»
           COMPUTE    RECE100-KAISU(36)   =    RECE100-KAISU(36)  -
                                              WRK-NYUYOUJI-SHOKAN           
      *
      *    ½èÊý¤»¤ó¤Î»»Äê²ó¿ô¤«¤éÄ¹´üÅêÌô²Ã»»(½èÊý¤»¤óÎÁ)¤Î¸º»»
           COMPUTE    RECE100-KAISU(36)   =    RECE100-KAISU(36)  -
                                              WRK-CYOKI-SKAN2    
      *
      *    ½èÊý¤»¤ó¤Î»»Äê²ó¿ô¤«¤é¹³°­À­¼ðáçºÞ½èÊý´ÉÍý²Ã»»(½èÊý¤»¤óÎÁ)¤Î¸º»»
           COMPUTE    RECE100-KAISU(36)   =    RECE100-KAISU(36)  -
                                              WRK-AKUSEI-SKAN2
      *
      *    ½èÊý¤»¤ó¤Î»»Äê²ó¿ô¤«¤é°ìÈÌÌ¾½èÊý²Ã»»(½èÊý¤»¤óÎÁ)¤Î¸º»»
           COMPUTE    RECE100-KAISU(36)   =    RECE100-KAISU(36)  -
                                              WRK-IPPAN-SKAN2
      *
      *    ËãÆÇ¤Î»»Äê²ó¿ô¤«¤é½èÊýÎÁ¡ÊËã¡¦¸þ¡¦ÆÇ¡¦³Ð¡Ë¤Î¸º»»
           COMPUTE    RECE100-KAISU(22)   =    RECE100-KAISU(22)  -
                                              WRK-MADOKU-SHOHO           
      *
           .                                           
      *
       210442-RECE100-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷£²¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       2104421-RECE-LEFT-SET-SEC               SECTION.
      *
           EVALUATE    SRY-SRYKBN
      *    ½é¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "11"
                   IF   WRK-TNS-CHUKSNTUBAN(IDX3)   =   "0"
                        ADD   WK-ZAIKAISU  TO   RECE100-KAISU(1)
                   END-IF
      *
      *************£±¡§»þ´Ö³°¡¡£²¡§¿¼Ìë¡¡£³¡§µÙÆü¡¡£´¡§»þ´Ö³°¡¦¿¼Ìë
      *************£µ¡§»þ´Ö³°¡¦µÙÆü¡¡£¶¡§µÙÆü¡¦¿¼Ìë
      *************£·¡§»þ´Ö³°¡¦µÙÆü¡¦¿¼Ìë
                   IF   WRK-TNS-DATAKBN(IDX3)  =   2
                     EVALUATE    WRK-TNS-TIMEKSNKBN(IDX3)
                         WHEN    1
                         WHEN    5
                         WHEN    6
                             EVALUATE   RECE100-SHOSHINTIME
                               WHEN   SPACE
                                  MOVE  "1"     TO   RECE100-SHOSHINTIME
                               WHEN   "1"
                                  CONTINUE
                               WHEN   "2"
                                  MOVE  "4"     TO   RECE100-SHOSHINTIME
                               WHEN   "3"
                                  MOVE  "5"     TO   RECE100-SHOSHINTIME
                               WHEN   "4"
                                  CONTINUE
                               WHEN   "5"
                                  CONTINUE
                               WHEN   "6"
                                  MOVE  "7"     TO   RECE100-SHOSHINTIME
                               WHEN   "7"
                                  CONTINUE
                             END-EVALUATE
                         WHEN    4
                             EVALUATE   RECE100-SHOSHINTIME
                               WHEN   SPACE
                                  MOVE  "2"     TO   RECE100-SHOSHINTIME
                               WHEN   "1"
                                  MOVE  "4"     TO   RECE100-SHOSHINTIME
                               WHEN   "2"
                                  CONTINUE
                               WHEN   "3"
                                  MOVE  "6"     TO   RECE100-SHOSHINTIME
                               WHEN   "4"
                                  CONTINUE
                               WHEN   "5"
                                  MOVE  "7"     TO   RECE100-SHOSHINTIME
                               WHEN   "6"
                                  CONTINUE
                               WHEN   "7"
                                  CONTINUE
                             END-EVALUATE
                         WHEN    3
                             EVALUATE   RECE100-SHOSHINTIME
                               WHEN   SPACE
                                  MOVE  "3"     TO   RECE100-SHOSHINTIME
                               WHEN   "1"
                                  MOVE  "5"     TO   RECE100-SHOSHINTIME
                               WHEN   "2"
                                  MOVE  "6"     TO   RECE100-SHOSHINTIME
                               WHEN   "3"
                                  CONTINUE
                               WHEN   "4"
                                  MOVE  "7"     TO   RECE100-SHOSHINTIME
                               WHEN   "5"
                                  CONTINUE
                               WHEN   "6"
                                  CONTINUE
                               WHEN   "7"
                                  CONTINUE
                             END-EVALUATE
                     END-EVALUATE
                   END-IF
      *
                   COMPUTE    RECE100-TENSU(1 1)  =
                              RECE100-TENSU(1 1)  +
                             (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
      *    ºÆ¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸¡Ê½¸·×ÊýË¡ÊÑ¹¹¸å¡Ë
               WHEN    "12"
                 EVALUATE  WRK-TNS-GAITENTTLKBN(IDX3)
      *      ¡Ê´ðËÜ¼êµ»¡Ë
                   WHEN  120
                       IF ( SRY-SRYCD(IDX3)      =   CONS-KEIZOKU1  OR
                                                     CONS-KEIZOKU2 )
                       OR ( WRK-TNS-CHUKSNTUBAN(IDX3)  NOT = "0"   )
                           CONTINUE
                       ELSE
                           ADD   WK-ZAIKAISU    TO   RECE100-KAISU(2)
                           IF    SRY-SRYCD(IDX3)   =   CONS-DOUJITU-SAI
                                                  OR   CONS-DOUJITU-DEN
                                                  OR   CONS-DOUJITU-GAI
                             ADD  WK-ZAIKAISU   TO   CNT-SAISIN-DOUJITU
                           ELSE
                             ADD  WK-ZAIKAISU   TO   CNT-SAISIN-ETC
                           END-IF
                       END-IF
      **
                       IF   FLG-NYUYOJI          =   ZERO
                       AND  SRY-SRYCD(IDX3)      =   CONS-NYUYOJI
                            MOVE     1       TO    FLG-NYUYOJI
                       END-IF
                       IF   FLG-NYUYOJI          =   2
                       AND  SRY-SRYCD(IDX3)      =   CONS-NYUYOJI
                            MOVE     9       TO    FLG-NYUYOJI
                       END-IF
                       IF   FLG-NYUYOJI          =   ZERO
                       AND  SRY-SRYCD(IDX3)      =   CONS-NYUYOJI-JIKAN
                            MOVE     2       TO    FLG-NYUYOJI
                       END-IF
                       IF   FLG-NYUYOJI          =   1
                       AND  SRY-SRYCD(IDX3)      =   CONS-NYUYOJI-JIKAN
                            MOVE     9       TO    FLG-NYUYOJI
                       END-IF
      *
                       IF   FLG-YOJI          =   ZERO
                       AND  SRY-SRYCD(IDX3)      =   CONS-YOJI
                            MOVE     1       TO    FLG-YOJI
                       END-IF
                       IF   FLG-YOJI          =   2
                       AND  SRY-SRYCD(IDX3)      =   CONS-YOJI
                            MOVE     9       TO    FLG-YOJI
                       END-IF
                       IF   FLG-YOJI          =   ZERO
                       AND  SRY-SRYCD(IDX3)      =   CONS-YOJI-JIKAN
                            MOVE     2       TO    FLG-YOJI
                       END-IF
                       IF   FLG-YOJI          =   1
                       AND  SRY-SRYCD(IDX3)      =   CONS-YOJI-JIKAN
                            MOVE     9       TO    FLG-YOJI
                       END-IF
      **
                       IF   FLG-NYUYOJI          =   ZERO
                       AND  SRY-SRYCD(IDX3)      =   CONS-GNYUYOJI
                            MOVE     1       TO    FLG-NYUYOJI
                       END-IF
                       IF   FLG-NYUYOJI          =   2
                       AND  SRY-SRYCD(IDX3)      =   CONS-GNYUYOJI
                            MOVE     9       TO    FLG-NYUYOJI
                       END-IF
                       IF   FLG-NYUYOJI          =   ZERO
                       AND  SRY-SRYCD(IDX3)      =   CONS-GNYUYOJI-JIKAN
                            MOVE     2       TO    FLG-NYUYOJI
                       END-IF
                       IF   FLG-NYUYOJI          =   1
                       AND  SRY-SRYCD(IDX3)      =   CONS-GNYUYOJI-JIKAN
                            MOVE     9       TO    FLG-NYUYOJI
                       END-IF
      *
                       IF   FLG-YOJI          =   ZERO
                       AND  SRY-SRYCD(IDX3)      =   CONS-GYOJI
                            MOVE     1       TO    FLG-YOJI
                       END-IF
                       IF   FLG-YOJI          =   2
                       AND  SRY-SRYCD(IDX3)      =   CONS-GYOJI
                            MOVE     9       TO    FLG-YOJI
                       END-IF
                       IF   FLG-YOJI          =   ZERO
                       AND  SRY-SRYCD(IDX3)      =   CONS-GYOJI-JIKAN
                            MOVE     2       TO    FLG-YOJI
                       END-IF
                       IF   FLG-YOJI          =   1
                       AND  SRY-SRYCD(IDX3)      =   CONS-GYOJI-JIKAN
                            MOVE     9       TO    FLG-YOJI
                       END-IF
      **
                       IF  SRY-SRYCD(IDX3)  =  CONS-NYUYOJI  OR
                                               CONS-GNYUYOJI
                            COMPUTE  CNT-NYUYOJI  =  CNT-NYUYOJI
                                                  +  WK-ZAIKAISU
                            COMPUTE  CNT-NYUYOJI2 =  CNT-NYUYOJI2
                                                  +  WK-ZAIKAISU
                       END-IF
                       IF   RECE100-SRYYM  >= "201004"
                         IF  SRY-SRYCD(IDX3)  =  CONS-JIKANTAI1
                            COMPUTE  CNT-JIKANTAI1 = CNT-JIKANTAI1
                                                   + WK-ZAIKAISU
                         END-IF
                         IF  SRY-SRYCD(IDX3)  =  CONS-KOKENKSN
                            COMPUTE  CNT-KOKENKSN =  CNT-KOKENKSN
                                                  +  WK-ZAIKAISU
                         END-IF
                         IF  SRY-SRYCD(IDX3)  =  CONS-JIKANTAI3
                            COMPUTE  CNT-JIKANTAI3 = CNT-JIKANTAI3
                                                   + WK-ZAIKAISU
                         END-IF
                         IF  SRY-SRYCD(IDX3)  =  CONS-MESAIKSN
                            COMPUTE  CNT-MESAIKSN =  CNT-MESAIKSN
                                                  +  WK-ZAIKAISU
                         END-IF
                       END-IF
      **
                       IF ( SRY-SRYCD(IDX3)      =   CONS-KEIZOKU1  OR
                                                     CONS-KEIZOKU2 )
                       OR ( FLG-NYUYOJI          =   9             )
                       OR ( FLG-YOJI             =   9             )
                            MOVE   ZERO       TO    RECE100-KHNTEN(2)
                            MOVE   1          TO    FLG-KHNTEN2
                       ELSE
                         IF  SRY-SRYCD(IDX3)  =  CONS-NYUYOJI        OR
                                                 CONS-NYUYOJI-JIKAN  OR
                                                 CONS-YOJI           OR
                                                 CONS-YOJI-JIKAN     OR
                                                 CONS-GNYUYOJI       OR
                                                 CONS-GNYUYOJI-JIKAN OR
                                                 CONS-GYOJI          OR
                                                 CONS-GYOJI-JIKAN    
                            IF   FLG-YOJI-ARI  =  ZERO
                              ADD  WRK-TNS-TEN(IDX3) TO
                                                     RECE100-KHNTEN(2)
                              MOVE     1       TO    FLG-YOJI-ARI
                            END-IF
                         ELSE
                          IF  (RECE100-SRYYM   >=  "201004"       )
                          AND (SRY-SRYCD(IDX3)  =  CONS-KOKENKSN  OR
                                                   CONS-MESAIKSN  OR
                                                   CONS-JIKANTAI1 OR
                                                   CONS-JIKANTAI3 )
                            EVALUATE  SRY-SRYCD(IDX3)
                              WHEN     CONS-JIKANTAI1
                                IF   FLG-JIKANTAI1-ARI  =  ZERO
                                  ADD  WRK-TNS-TEN(IDX3)
                                                   TO  RECE100-KHNTEN(2)
                                  MOVE     1       TO  FLG-JIKANTAI1-ARI
                                END-IF
                              WHEN     CONS-KOKENKSN
                                IF   FLG-KOKENKSN-ARI   =  ZERO
                                  ADD  WRK-TNS-TEN(IDX3)
                                                   TO  RECE100-KHNTEN(2)
                                  MOVE     1       TO  FLG-KOKENKSN-ARI
                                END-IF
                              WHEN     CONS-JIKANTAI3
                                IF   FLG-JIKANTAI3-ARI  =  ZERO
                                  ADD  WRK-TNS-TEN(IDX3)
                                                   TO  RECE100-KHNTEN(2)
                                  MOVE     1       TO  FLG-JIKANTAI3-ARI
                                END-IF
                              WHEN     CONS-MESAIKSN
                                IF   FLG-MESAIKSN-ARI   =  ZERO
                                  ADD  WRK-TNS-TEN(IDX3)
                                                   TO  RECE100-KHNTEN(2)
                                  MOVE     1       TO  FLG-MESAIKSN-ARI
                                END-IF
                            END-EVALUATE
                          ELSE
                            IF   FLG-SAISIN-ARI   =  ZERO
                              ADD  WRK-TNS-TEN(IDX3) TO
                                                     RECE100-KHNTEN(2)
                              MOVE     1       TO    FLG-SAISIN-ARI
                            END-IF
                          END-IF
                         END-IF
                       END-IF
      **
                       COMPUTE  RECE100-TENSU(2 1) =
                                RECE100-TENSU(2 1) +
                               (WRK-TNS-TEN(IDX3)  *   ACCT-ZAIKAISU)
      *      ¡Ê³°Íè´ÉÍý²Ã»»¡Ë
                   WHEN  122
                        IF    RECE100-KHNTEN(3)   =   ZERO
                           MOVE  WRK-TNS-TEN(IDX3) TO  RECE100-KHNTEN(3)
                        ELSE
                           IF RECE100-KHNTEN(3) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO  RECE100-KHNTEN(3)
                              MOVE   1    TO  FLG-KHNTEN3
                           END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE100-KAISU(3)
                        COMPUTE RECE100-TENSU(3 1) =
                                RECE100-TENSU(3 1) +
                               (WRK-TNS-TEN(IDX3)  *   ACCT-ZAIKAISU)
      *      ¡Ê»þ´Ö³°¡Ë
                   WHEN  123
                        IF    RECE100-KHNTEN(4)   =   ZERO
                           MOVE  WRK-TNS-TEN(IDX3)    TO
                                              RECE100-KHNTEN(4)
                        ELSE
                          IF RECE100-KHNTEN(4) NOT = WRK-TNS-TEN(IDX3)
                             MOVE   ZERO  TO  RECE100-KHNTEN(4)
                             MOVE   1     TO  FLG-KHNTEN4
                          END-IF
                        END-IF
                        ADD   WK-ZAIKAISU TO  RECE100-KAISU(4)
                        IF SRY-SRYCD(IDX3) = "112015170" OR
                                             "112015270" OR
                                             "112015370" OR
                                             "112015470" OR
                                             "112014470" OR
                                             "112014570" OR
                                             "112014670" OR
                                             "112014770" OR
                                             "112014870" OR
                                             "112014970" OR
                                             "112015070" OR
                                             "112014170" OR
                                             "112014270" OR
                                             "112014370"
                           COMPUTE  CNT-NYUYOJI2 = CNT-NYUYOJI2
                                                 + WK-ZAIKAISU
                        END-IF
                        COMPUTE RECE100-TENSU(4 1) =
                                RECE100-TENSU(4 1) +
                               (WRK-TNS-TEN(IDX3)  *   ACCT-ZAIKAISU)
      *      ¡ÊµÙÆü¡Ë
                   WHEN  124
                        IF    RECE100-KHNTEN(5)   =   ZERO
                           MOVE  WRK-TNS-TEN(IDX3)    TO
                                              RECE100-KHNTEN(5)
                        ELSE
                          IF RECE100-KHNTEN(5) NOT = WRK-TNS-TEN(IDX3)
                             MOVE   ZERO  TO  RECE100-KHNTEN(5)
                             MOVE   1     TO  FLG-KHNTEN5
                          END-IF
                        END-IF
                        ADD   WK-ZAIKAISU TO  RECE100-KAISU(5)
                        IF SRY-SRYCD(IDX3) = "112015170" OR
                                             "112015270" OR
                                             "112015370" OR
                                             "112015470" OR
                                             "112014470" OR
                                             "112014570" OR
                                             "112014670" OR
                                             "112014770" OR
                                             "112014870" OR
                                             "112014970" OR
                                             "112015070" OR
                                             "112014170" OR
                                             "112014270" OR
                                             "112014370"
                           COMPUTE  CNT-NYUYOJI2 = CNT-NYUYOJI2
                                                 + WK-ZAIKAISU
                        END-IF
                        COMPUTE RECE100-TENSU(5 1) =
                                RECE100-TENSU(5 1) +
                               (WRK-TNS-TEN(IDX3)  *   ACCT-ZAIKAISU)
      *      ¡Ê¿¼Ìë¡Ë
                   WHEN  125
                        IF    RECE100-KHNTEN(6)   =   ZERO
                           MOVE  WRK-TNS-TEN(IDX3)    TO
                                              RECE100-KHNTEN(6)
                        ELSE
                          IF RECE100-KHNTEN(6) NOT = WRK-TNS-TEN(IDX3)
                             MOVE   ZERO  TO  RECE100-KHNTEN(6)
                             MOVE   1     TO  FLG-KHNTEN6
                          END-IF
                        END-IF
                        ADD   WK-ZAIKAISU TO  RECE100-KAISU(6)
                        IF SRY-SRYCD(IDX3) = "112015170" OR
                                             "112015270" OR
                                             "112015370" OR
                                             "112015470" OR
                                             "112014470" OR
                                             "112014570" OR
                                             "112014670" OR
                                             "112014770" OR
                                             "112014870" OR
                                             "112014970" OR
                                             "112015070" OR
                                             "112014170" OR
                                             "112014270" OR
                                             "112014370"
                           COMPUTE  CNT-NYUYOJI2 = CNT-NYUYOJI2
                                                 + WK-ZAIKAISU
                        END-IF
                        COMPUTE RECE100-TENSU(6 1) =
                                RECE100-TENSU(6 1) +
                               (WRK-TNS-TEN(IDX3)  *   ACCT-ZAIKAISU)
      *      ¡Ê¤½¤ÎÂ¾¡Ë
                   WHEN  OTHER 
                        IF   ACCT-ZAIKBN     =  2
                           MOVE   ZERO       TO    RECE100-KHNTEN(2)
                           MOVE   1          TO    FLG-KHNTEN2
                           COMPUTE  WRK-RECE100G-TENSU(2 1) =
                                    WRK-RECE100G-TENSU(2 1) +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                        END-IF
                 END-EVALUATE
      *    »ØÆ³¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "13"
                 IF  ACCT-ZAIKBN   =    2
                  COMPUTE  WRK-RECE100G-TENSU(9 1)  =
                           WRK-RECE100G-TENSU(9 1)  +
                          (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                  EVALUATE   SRY-SRYCD(IDX3)
      *            ¸ø³²¼À´µÁêÃÌÎÁ
                   WHEN     "102130010"
                      ADD   WK-ZAIKAISU TO  RECE100-KAISU(7)
                      COMPUTE    RECE100-TENSU(7 2)  =
                                 RECE100-TENSU(7 2)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                      IF   FLG-NEBU  =  ZERO
                         MOVE     1    TO    FLG-NEBU
                      END-IF
      *            ¸ø³²³°ÍèÎÅÍÜ»ØÆ³ÎÁ
                   WHEN     "102130020"
      ***             ADD   WK-ZAIKAISU TO  RECE100-KAISU(8)
                      COMPUTE    RECE100-TENSU(8 2)  =
                                 RECE100-TENSU(8 2)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                      IF   FLG-NEBU  =  ZERO
                         MOVE     2    TO    FLG-NEBU
                      END-IF
      *            ¥Í¥Ö¥é¥¤¥¶¡¼²Ã»»
                   WHEN     "102400010"
                      EVALUATE  FLG-NEBU
                        WHEN       1
                          COMPUTE  RECE100-TENSU(8 2)  =
                                   RECE100-TENSU(8 2)  +
                                  (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                          COMPUTE  RECE100-TENSU(7 2) =
                                   RECE100-TENSU(7 2) -
                                  (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                        WHEN       2
                          CONTINUE
                        WHEN       3
                          COMPUTE  RECE100-TENSU(8 2)  =
                                   RECE100-TENSU(8 2)  +
                                  (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                          COMPUTE  RECE100-TENSU(9 1) =
                                   RECE100-TENSU(9 1) -
                                  (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                      END-EVALUATE
                   WHEN      OTHER
                      ADD   WK-ZAIKAISU TO  RECE100-KAISU(9)
                      COMPUTE    RECE100-TENSU(9 1)  =
                                 RECE100-TENSU(9 1)  +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
                      IF   FLG-NEBU  =  ZERO
                         MOVE     3    TO    FLG-NEBU
                      END-IF
                  END-EVALUATE
                 END-IF
      *    ºßÂð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "14"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE  WRK-RECE100G-TENSU(14 1)  =
                            WRK-RECE100G-TENSU(14 1)  +
                           (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   MOVE      ZERO            TO   FLG-ZAITAKU
      *            ±ý¿Ç¡¦ÆÃÊÌ±ý¿Ç¡Ê»þ´Ö²Ã»»¡Ë
                   IF    FLG-OSINKBN         =   1
                       MOVE         1        TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE100-KAISU(10)
                       COMPUTE   RECE100-TENSU(10 1)  =
                                 RECE100-TENSU(10 1)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
      *                ¥Í¥Ö¥é¥¤¥¶¡¼²Ã»»
                       IF    SRY-SRYCD(IDX3)  =  "102400010"
                         COMPUTE    RECE100-TENSU(15 2)  =
                                    RECE100-TENSU(15 2)  +
                                   (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                         COMPUTE    RECE100-TENSU(10 1) =
                                    RECE100-TENSU(10 1) -
                                   (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                       END-IF
                   END-IF
      *            Ìë´Ö
                   IF    FLG-OSINKBN         =    2
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE100-KAISU(11)
                       COMPUTE   RECE100-TENSU(11 1)  =
                                 RECE100-TENSU(11 1)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
      *                ¥Í¥Ö¥é¥¤¥¶¡¼²Ã»»
                       IF    SRY-SRYCD(IDX3)  =  "102400010"
                         COMPUTE    RECE100-TENSU(15 2)  =
                                    RECE100-TENSU(15 2)  +
                                   (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                         COMPUTE    RECE100-TENSU(11 1) =
                                    RECE100-TENSU(11 1) -
                                   (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                       END-IF
                   END-IF
      *            ¿¼Ìë¡¦¶ÛµÞ
                   IF    FLG-OSINKBN         =    3
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE100-KAISU(12)
                       COMPUTE   RECE100-TENSU(12 1)  =
                                 RECE100-TENSU(12 1)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
      *                ¥Í¥Ö¥é¥¤¥¶¡¼²Ã»»
                       IF    SRY-SRYCD(IDX3)  =  "102400010"
                         COMPUTE    RECE100-TENSU(15 2)  =
                                    RECE100-TENSU(15 2)  +
                                   (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                         COMPUTE    RECE100-TENSU(12 1) =
                                    RECE100-TENSU(12 1) -
                                   (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                       END-IF
                   END-IF
      *            ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅ
                   IF (( WRK-TNS-OSINKBN(IDX3)   =     1          )  AND
                       ((SRY-SRYCD(IDX3)     NOT =    "114000110")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114001610")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114000970")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114002470")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114002970")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114000470")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114001970")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114011670")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114011970")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114000370")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114001570")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114001870")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114002070")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114011570")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114011770")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114011870")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114012070")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114017470")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114017570")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114017670")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114017770")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114017870")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114017970")))
                   OR (  FLG-OSINKBN             =     9           )
      ***          OR (  SRY-SRYCD(IDX3) =    CONS-ZAIHOU-COMMENT  )
      ***          OR (  SRY-SRYCD(IDX3) =    CONS-ZAIHOU3-COMMENT )
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE100-KAISU(13)
                       COMPUTE   RECE100-TENSU(13 1)  =
                                 RECE100-TENSU(13 1)  +
                                (WK-SYUTEN     *   WK-ZAIKAISU)
      *                ¥Í¥Ö¥é¥¤¥¶¡¼²Ã»»
                       IF    SRY-SRYCD(IDX3)  =  "102400010"
                         COMPUTE    RECE100-TENSU(15 2)  =
                                    RECE100-TENSU(15 2)  +
                                   (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                         COMPUTE    RECE100-TENSU(13 1) =
                                    RECE100-TENSU(13 1) -
                                   (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                       END-IF
                   END-IF
      *            ¤½¤ÎÂ¾
                   IF    FLG-ZAITAKU    =   ZERO
                       COMPUTE   RECE100-TENSU(14 1)  =
                                 RECE100-TENSU(14 1)  +
                                (WK-SYUTEN     *   WK-ZAIKAISU)
      *                ¥Í¥Ö¥é¥¤¥¶¡¼²Ã»»
                       IF    SRY-SRYCD(IDX3)  =  "102400010"
                         COMPUTE    RECE100-TENSU(15 2)  =
                                    RECE100-TENSU(15 2)  +
                                   (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                         COMPUTE    RECE100-TENSU(14 1) =
                                    RECE100-TENSU(14 1) -
                                   (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                       END-IF
                   END-IF
      *            ÌôºÞ
                   COMPUTE   RECE100-TENSU(15 2)  =
                             RECE100-TENSU(15 2)  +
                            (WK-KIZAITEN *   WK-ZAIKAISU)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                 END-IF
      *    ÅêÌô¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF   SRY-SRYCD(IDX3)  =    CONS-YAKTEIGEN
                           COMPUTE   RECE100-TENSU(16 2)  =
                                     RECE100-TENSU(16 2)  -
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                           ADD   WK-ZAIKAISU  TO   RECE100-KAISU(16)
                           COMPUTE   RECE100-TENSU(16 2)  =
                                     RECE100-TENSU(16 2)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)  =    CONS-FUNSAI-COMMENT
                           ADD   WK-ZAIKAISU  TO   RECE100-KAISU(16)
                           COMPUTE   RECE100-TENSU(16 2)  =
                                     RECE100-TENSU(16 2)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                       IF    RECE100-KHNTEN(17)   =   ZERO
                          MOVE  WRK-TNS-TEN(IDX3) TO RECE100-KHNTEN(17)
                       ELSE
                          IF   RECE100-KHNTEN(17)
                                          NOT = WRK-TNS-TEN(IDX3)
                               MOVE   ZERO    TO    RECE100-KHNTEN(17)
                               MOVE   1       TO    FLG-KHNTEN17
                          END-IF
                       END-IF
                       ADD   WK-ZAIKAISU  TO   RECE100-KAISU(17)
                       COMPUTE   RECE100-TENSU(17 1)  =
                                 RECE100-TENSU(17 1)  +
                                (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                     END-IF
                   END-IF
               WHEN    "22"
                   ADD    WK-ZAIKAISU    TO   RECE100-KAISU(18)
                   COMPUTE   RECE100-TENSU(18 2)  =
                             RECE100-TENSU(18 2)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
               WHEN    "23"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                       ADD    WK-ZAIKAISU    TO   RECE100-KAISU(19)
                       COMPUTE   RECE100-TENSU(19 2)  =
                                 RECE100-TENSU(19 2)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   ELSE
                       IF    RECE100-KHNTEN(20)   =   ZERO
                          MOVE  WRK-TNS-TEN(IDX3) TO  RECE100-KHNTEN(20)
                       ELSE
                          IF   RECE100-KHNTEN(20)
                                              NOT = WRK-TNS-TEN(IDX3)
                               MOVE   ZERO    TO    RECE100-KHNTEN(20)
                               MOVE   1       TO    FLG-KHNTEN20
                          END-IF
                       END-IF
                       ADD    WK-ZAIKAISU    TO   RECE100-KAISU(20)
                       COMPUTE   RECE100-TENSU(20 1)  =
                                 RECE100-TENSU(20 1)  +
                                (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                   END-IF
      *    ½èÊý¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "25"
                   IF SRY-SRYCD(IDX3)      =    "120002270"
                      COMPUTE  WRK-TOKUTEI-SKAN  =  WRK-TOKUTEI-SKAN +
                                                    WK-ZAIKAISU 
                   END-IF
                   IF SRY-SRYCD(IDX3)      =    "120002170"
                      COMPUTE  WRK-NYUYOUJI-SKAN  =  WRK-NYUYOUJI-SKAN +
                                                     WK-ZAIKAISU 
                   END-IF
                   IF SRY-SRYCD(IDX3)      =    "120003170"             
                      COMPUTE  WRK-CYOKI-SKAN  =  WRK-CYOKI-SKAN +      
                                                  WK-ZAIKAISU           
                   END-IF                                               
                   IF SRY-SRYCD(IDX3)      =    "120003370"
                      COMPUTE  WRK-AKUSEI-SKAN =  WRK-AKUSEI-SKAN +
                                                  WK-ZAIKAISU
                   END-IF
      ***          IF    RECE100-KHNTEN(21)   =   ZERO
      ***             MOVE  WRK-TNS-TEN(IDX3) TO  RECE100-KHNTEN(21)
      ***          ELSE
      ***             IF   RECE100-KHNTEN(21) NOT = WRK-TNS-TEN(IDX3)
      ***             AND  WRK-TNS-TEN(IDX3)  NOT = ZERO
      ***                  MOVE   ZERO    TO    RECE100-KHNTEN(21)
      ***                  MOVE   1       TO    FLG-KHNTEN21
      ***             END-IF
      ***          END-IF
                   IF    WK-ZAITEN   NOT =  ZERO
                     IF    RECE100-KHNTEN(21)     = ZERO
                      MOVE  WK-ZAITEN     TO   RECE100-KHNTEN(21)
                     ELSE
                      IF   RECE100-KHNTEN(21) NOT = WK-ZAITEN
                            MOVE   ZERO   TO   RECE100-KHNTEN(21)
                            MOVE   1      TO   FLG-KHNTEN21
                      END-IF
                     END-IF
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE100-KAISU(21)
                   END-IF
                   COMPUTE   RECE100-TENSU(21 1)  =
                             RECE100-TENSU(21 1)  +
                            (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
      *    ËãÆÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "26"
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE100-KAISU(22)
                   END-IF
                   IF SRY-SRYCD(IDX3)      =    "120001310"
                    COMPUTE WRK-MADOKU-SHOHO = WRK-MADOKU-SHOHO +
                                               WK-ZAIKAISU 
                   END-IF
                   COMPUTE   RECE100-TENSU(22 1)  =
                             RECE100-TENSU(22 1)  +
                            (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
      *    Ä´´ð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "27"
      ***          IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
      ***          OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
      ***          OR  (SRY-SRYCD(IDX3)        = "099999903")
      ***             CONTINUE
      ***          ELSE
      ***             ADD    WK-ZAIKAISU     TO    RECE100-KAISU(23)
      ***          END-IF
                   COMPUTE   RECE100-TENSU(23 1)  =
                             RECE100-TENSU(23 1)  +
                            (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
      *    Èé²¼¶ÚÆùÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "31"
                   ADD    WK-ZAIKAISU     TO   RECE100-KAISU(24)
                   COMPUTE   RECE100-TENSU(24 1)  =
                             RECE100-TENSU(24 1)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE100-TENSU(27 2)  =
                             RECE100-TENSU(27 2)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ÀÅÌ®ÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "32"
                   ADD    WK-ZAIKAISU     TO   RECE100-KAISU(25)
                   COMPUTE   RECE100-TENSU(25 1)  =
                             RECE100-TENSU(25 1)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE100-TENSU(27 2)  =
                             RECE100-TENSU(27 2)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾Ãí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "33"
                   ADD    WK-ZAIKAISU     TO   RECE100-KAISU(26)
                   COMPUTE   RECE100-TENSU(26 1)  =
                             RECE100-TENSU(26 1)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE100-TENSU(27 2)  =
                             RECE100-TENSU(27 2)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ½èÃÖ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE100-KAISU(28)
                   END-IF
                   COMPUTE   RECE100-TENSU(28 1)  =
                             RECE100-TENSU(28 1)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE100-TENSU(29 2)  =
                             RECE100-TENSU(29 2)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *
      *            ¥Í¥Ö¥é¥¤¥¶¡¼²Ã»»
                   IF    SRY-SRYCD(IDX3)  =  "102400010"
                      COMPUTE    RECE100-TENSU(29 2)  =
                                 RECE100-TENSU(29 2)  +
                                (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                      COMPUTE    RECE100-TENSU(28 1)  =
                                 RECE100-TENSU(28 1)  -
                                (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                   END-IF
      *    ¼ê½Ñ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE100-KAISU(30)
                   END-IF
                   COMPUTE   RECE100-TENSU(30 1)  =
                             RECE100-TENSU(30 1)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE100-TENSU(31 2)  =
                             RECE100-TENSU(31 2)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¸¡ºº¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "60"
               WHEN    "64"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE100-KAISU(32)
                   END-IF
                   COMPUTE   RECE100-TENSU(32 1)  =
                             RECE100-TENSU(32 1)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    WRK-TNS-KNSJISKBN(IDX3)    =   2
                         COMPUTE   RECE100-KAISU(32)  =
                                   RECE100-KAISU(32)  -
                                   WK-ZAIKAISU
                   END-IF
                   IF    WRK-TNS-SRYCD(IDX3)  =   CONS-SAIKETSU1  OR
                                                  CONS-SAIKETSU2  OR
                                                  CONS-SAIKETSU3
                         IF   RECE100-KAISU(32)  NOT =  ZERO
                              COMPUTE   RECE100-KAISU(32)  =
                                        RECE100-KAISU(32)  -
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE100-TENSU(33 2)  =
                             RECE100-TENSU(33 2)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ²èÁü¿ÇÃÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE100-KAISU(34)
                   END-IF
                   COMPUTE   RECE100-TENSU(34 1)  =
                             RECE100-TENSU(34 1)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE100-TENSU(35 2)  =
                             RECE100-TENSU(35 2)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "80"
                   IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN2
                      COMPUTE  WRK-TOKUTEI-SKAN2 =  WRK-TOKUTEI-SKAN2 +
                                                    ACCT-ZAIKAISU
                   END-IF
                   IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN1
                    COMPUTE WRK-NYUYOUJI-SHOKAN = WRK-NYUYOUJI-SHOKAN +
                                                  ACCT-ZAIKAISU
                   END-IF
                   IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN3          
                      COMPUTE  WRK-CYOKI-SKAN2  =   WRK-CYOKI-SKAN2   + 
                                                    ACCT-ZAIKAISU
                   END-IF                                               
                   IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN4
                      COMPUTE  WRK-AKUSEI-SKAN2 =   WRK-AKUSEI-SKAN2  +
                                                    ACCT-ZAIKAISU
                   END-IF
                   IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN5
                      COMPUTE  WRK-IPPAN-SKAN2  =   WRK-IPPAN-SKAN2   +
                                                    ACCT-ZAIKAISU
                   END-IF
                   IF SRY-SRYCD(IDX3)      =    CONS-SHOHOSEN1 OR
                                                CONS-SHOHOSEN2 OR
                                                CONS-SHOHOSEN3 OR
                                                CONS-SHOHOSEN4 OR
                                                CONS-SHOHOKSN1 OR
                                                CONS-SHOHOKSN2 OR
                                                CONS-SHOHOKSN3 OR
                                                CONS-SHOHOKSN4 OR
                                                CONS-SHOHOKSN5
                           ADD  ACCT-ZAIKAISU  TO  RECE100-KAISU(36)
                   END-IF
                   COMPUTE   RECE100-TENSU(37 1)  =
                             RECE100-TENSU(37 1)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF SRY-SRYCD(IDX3)      =    CONS-SHOHOSEN1 OR
                                                CONS-SHOHOSEN2 OR
                                                CONS-SHOHOSEN3 OR
                                                CONS-SHOHOSEN4 OR
                                                CONS-SHOHOKSN1 OR
                                                CONS-SHOHOKSN2 OR
                                                CONS-SHOHOKSN3 OR
                                                CONS-SHOHOKSN4 OR
                                                CONS-SHOHOKSN5
                       COMPUTE   RECE100-TENSU(36 1)  =
                                 RECE100-TENSU(36 1)  +
                                (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                       COMPUTE   RECE100-TENSU(37 1)  =
                                 RECE100-TENSU(37 1)  -
                                (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                   ELSE
                     IF    WRK-TNS-GAITENTTLKBN(IDX3)   =    130
                       COMPUTE   RECE100-TENSU(9 1)   =
                                 RECE100-TENSU(9 1)   +
                                (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                       COMPUTE   RECE100-TENSU(37 1)  =
                                 RECE100-TENSU(37 1)  -
                                (WRK-TNS-TEN(IDX3)  *  ACCT-ZAIKAISU)
                     END-IF
                   END-IF
                   COMPUTE   RECE100-TENSU(38 2)  =
                             RECE100-TENSU(38 2)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
           END-EVALUATE
           .
      *
       2104421-RECE-LEFT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºÞ¤Î¿ÇÎÅÆâÍÆ¥Á¥§¥Ã¥¯
      *****************************************************************
       21046-SINRYO-CHECK-SEC                 SECTION.
      *
      ***  PERFORM   VARYING   IDU   FROM   1   BY   1
           PERFORM   VARYING   IDU   FROM   2   BY   1
                      UNTIL  ( IDU   >   TBL-ZAICHECK-MAX )
                       OR    ( FLG-RINYAK  NOT =  ZERO    )
      *
              IF  ( IDU  =  1                                  )
              OR  ((IDU  =  2) AND (ACCT-SRYKBN = "60" OR "64"))
              OR  ((IDU  =  3) AND (ACCT-SRYKBN = "80"        ))
              OR  ((IDU  =  4) AND (ACCT-SRYKBN = "80"        ))
      *
                 INITIALIZE                   SRYACT-REC
grpsys           MOVE  RECE100-HOSPNUM     TO  SRY-HOSPNUM
                 MOVE  RECE100-NYUGAIKBN   TO  SRY-NYUGAIKBN
                 MOVE  RECE100-PTID        TO  SRY-PTID
                 MOVE  RECE100-SRYYM       TO  SRY-SRYYM
                 MOVE  ACCT-ZAINUM        TO  SRY-ZAINUM
                 MOVE  TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(1)
                 MOVE  TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(2)
                 MOVE  TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(3)
                 MOVE  TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(4)
                 MOVE  TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(5)
                 MOVE  SRYACT-REC         TO  MCPDATA-REC
                 MOVE    "tbl_sryact"         TO  MCP-TABLE
                 MOVE    "key8"               TO  MCP-PATHNAME
grpsys           PERFORM 900-DBSELECT-SEC
                 IF      MCP-RC               =       ZERO
                     MOVE    "tbl_sryact"         TO  MCP-TABLE
                     MOVE    "key8"               TO  MCP-PATHNAME
                     PERFORM   900-SRYACT-READ-SEC
                     IF     FLG-SRYACT   =  ZERO
                         EVALUATE   IDU
                           WHEN      1
                             MOVE   1   TO  FLG-RINYAK
                           WHEN      2
                           WHEN      3
                           WHEN      4
                             MOVE   2   TO  FLG-RINYAK
                         END-EVALUATE
                     END-IF
                 END-IF
                 MOVE    "tbl_sryact"         TO  MCP-TABLE
                 MOVE    "key8"               TO  MCP-PATHNAME
                 PERFORM   900-CLOSE-SEC
      *
              END-IF
      *
           END-PERFORM
      *
           .
       21046-SINRYO-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³çÅÀ¿ô¤Î¼£¸³¸ºÅÀÊ¬ÊÔ½¸
      *****************************************************************
       2105-GENTEN-HENSYU-SEC      SECTION.
      *
           MOVE     ZERO    TO    WRK-TOTALTEN1
           MOVE     ZERO    TO    WRK-TOTALTEN2
           PERFORM VARYING ACCT-IDX    FROM    1   BY  1
                   UNTIL   ACCT-IDX    >       38
               ADD    RECE100-TENSU(ACCT-IDX 1)  TO  WRK-TOTALTEN1
               ADD    RECE100-TENSU(ACCT-IDX 2)  TO  WRK-TOTALTEN2
           END-PERFORM
           COMPUTE   WRK-TOTALTEN1  =  WRK-TOTALTEN1
                                    -  WRK-RECE100G-TENSU(2 1)
                                    -  WRK-RECE100G-TENSU(9 1)
                                    -  WRK-RECE100G-TENSU(14 1)
      *
      *    ºÆ¿Ç¤Î¥ì¥»ÅÀ¿ô¤ÎÊÔ½¸¡Ê¸ºÅÀ¡Ë
           IF   RECE100-TENSU(2 1)   >=  WRK-RECE100G-TENSU(2 1)
             COMPUTE  RECE100-TENSU(2 1)  =
                      RECE100-TENSU(2 1)  - WRK-RECE100G-TENSU(2 1)
           ELSE
             MOVE     ZERO    TO   RECE100-TENSU(2 1)
           END-IF
      *
      *    »ØÆ³¤Î¥ì¥»ÅÀ¿ô¤ÎÊÔ½¸¡Ê¸ºÅÀ¡Ë
           IF   RECE100-TENSU(9 1)   >=  WRK-RECE100G-TENSU(9 1)
             COMPUTE  RECE100-TENSU(9 1)  =
                      RECE100-TENSU(9 1)  - WRK-RECE100G-TENSU(9 1)
           ELSE
             MOVE     ZERO    TO   RECE100-TENSU(9 1)
           END-IF
      *
      *    ºßÂð¤Î¥ì¥»ÅÀ¿ô¤ÎÊÔ½¸¡Ê¸ºÅÀ¡Ë
           IF   RECE100-TENSU(14 1)  >=  WRK-RECE100G-TENSU(14 1)
             COMPUTE  RECE100-TENSU(14 1) =
                      RECE100-TENSU(14 1) - WRK-RECE100G-TENSU(14 1)
           ELSE
             MOVE     ZERO    TO   RECE100-TENSU(14 1)
           END-IF
      *
           .
       2105-GENTEN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ÂÆü¿ôÊÔ½¸½èÍý
      *****************************************************************
       2106-JNISSU-SET-SEC                    SECTION.                       
      *
           PERFORM VARYING ACCT-IDX    FROM    1   BY  1
                   UNTIL   ACCT-IDX    >       31
      *        ¼ÂÆü¿ô¤«¤é¸º¤¹¤ë
               IF    ( TBL-JNISSU-DAY     (ACCT-IDX)   NOT =   SPACE )
               AND   ( TBL-JNISSU-DAY-GEN (ACCT-IDX)   NOT =   SPACE )
                    MOVE  SPACE        TO  TBL-JNISSU-DAY (ACCT-IDX)
               END-IF
      *
               IF    TBL-JNISSU-DAY (ACCT-IDX)
                                       NOT =  SPACE
      *            ¼ÂÆü¿ôÊÔ½¸
                   ADD     1           TO  RECE100-JNISSU
      *            ¼ÂÆü¿ô¥Æ¡¼¥Ö¥ëÊÔ½¸
                   MOVE    "1"         TO  RECE100-JNISSUTBL(ACCT-IDX)
               END-IF
           END-PERFORM
      *
      *    ¹ç·×ÅÀ¿ô½¸·×
      ***  PERFORM VARYING ACCT-IDX    FROM    1   BY  1
      ***          UNTIL   ACCT-IDX    >       38
      ***      ADD    RECE100-TENSU(ACCT-IDX 1)  TO
      ***                                    RECE100-TENSU(41 1)
      ***      ADD    RECE100-TENSU(ACCT-IDX 2)  TO
      ***                                    RECE100-TENSU(41 2)
      ***  END-PERFORM
      *
           MOVE       WRK-TOTALTEN1     TO    RECE100-TENSU(41 1)
           MOVE       WRK-TOTALTEN2     TO    RECE100-TENSU(41 2)
      *
      *    ¹ç·×¡Ê£±£µ±ß¡ß¼êµ»ÅÀ¡Ë
           COMPUTE    RECE100-TOTALTEN(1)  =  RECE100-TENSU(41 1)
                                           *  15
      *    ¹ç·×¡Ê£±£°±ß¡ßÌôºÞÅùÅÀ¡Ë
           COMPUTE    RECE100-TOTALTEN(2)  =  RECE100-TENSU(41 2)
                                           *  10
      *    Áí¹ç·×
           COMPUTE    RECE100-TOTALTEN(3)  =  RECE100-TOTALTEN(1)
                                           +  RECE100-TOTALTEN(2)
           .
       2106-JNISSU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î´ðËÜÅÀ¿ôÉôÊ¬¤Î¥¯¥ê¥¢½èÍý¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       2107-KHNTEN-CLEAR-SEC               SECTION.
      *
           IF        FLG-KHNTEN2        =    1
               MOVE    ZERO             TO   RECE100-KHNTEN(2)
           END-IF
           IF        FLG-KHNTEN3        =    1
               MOVE    ZERO             TO   RECE100-KHNTEN(3)
           END-IF
           IF        FLG-KHNTEN4        =    1
               MOVE    ZERO             TO   RECE100-KHNTEN(4)
           END-IF
           IF        FLG-KHNTEN5        =    1
               MOVE    ZERO             TO   RECE100-KHNTEN(5)
           END-IF
           IF        FLG-KHNTEN6        =    1
               MOVE    ZERO             TO   RECE100-KHNTEN(6)
           END-IF
           IF        FLG-KHNTEN17        =    1
               MOVE    ZERO             TO   RECE100-KHNTEN(17)
           END-IF
           IF        FLG-KHNTEN20        =    1
               MOVE    ZERO             TO   RECE100-KHNTEN(20)
           END-IF
           IF        FLG-KHNTEN21        =    1
               MOVE    ZERO             TO   RECE100-KHNTEN(21)
           END-IF
      *
           .
       2107-KHNTEN-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆýÍÄ»ù¡¦ÍÄ»ù¤Î¥³¥á¥ó¥È¥Õ¥é¥°¥»¥Ã¥È
      *****************************************************************
       2110-NYUYOJI-COMENTFLG-SET-SEC               SECTION.
      *
             IF    (CNT-SAISIN-ETC      NOT =  ZERO)
             AND   (CNT-SAISIN-DOUJITU  NOT =  ZERO)
                     MOVE   ZERO         TO   RECE100-KHNTEN(2)
             END-IF
             IF    (CNT-NYUYOJI      NOT =  ZERO       )
      ***    AND   (RECE100-KAISU(2) NOT =  CNT-NYUYOJI)
             AND   (CNT-SAISIN-ETC   NOT =  CNT-NYUYOJI)
                     MOVE   ZERO         TO   RECE100-KHNTEN(2)
             END-IF
             IF     RECE100-NYUYOJIFLG      =  SPACE
               IF    (CNT-NYUYOJI2     NOT =  ZERO        )
      ***      AND   (RECE100-KAISU(2) NOT =  CNT-NYUYOJI2)
               AND   (CNT-SAISIN-ETC   NOT =  CNT-NYUYOJI2)
                     MOVE     "1"        TO   RECE100-NYUYOJIFLG
               END-IF
             END-IF
             IF   RECE100-SRYYM  >= "201004"
               IF    (CNT-JIKANTAI1    NOT =  ZERO          )
      ***      AND   (RECE100-KAISU(2) NOT =  CNT-JIKANTAI1)
               AND   (CNT-SAISIN-ETC   NOT =  CNT-JIKANTAI1)
                     MOVE   ZERO         TO   RECE100-KHNTEN(2)
               END-IF
               IF    (CNT-KOKENKSN     NOT =  ZERO        )
      ***      AND   (RECE100-KAISU(2) NOT =  CNT-KOKENKSN)
               AND   (CNT-SAISIN-ETC   NOT =  CNT-KOKENKSN)
                     MOVE   ZERO         TO   RECE100-KHNTEN(2)
               END-IF
               IF    (CNT-JIKANTAI3    NOT =  ZERO          )
      ***      AND   (RECE100-KAISU(2) NOT =  CNT-JIKANTAI3)
               AND   (CNT-SAISIN-ETC   NOT =  CNT-JIKANTAI3)
                     MOVE   ZERO         TO   RECE100-KHNTEN(2)
               END-IF
               IF    (CNT-MESAIKSN     NOT =  ZERO        )
      ***      AND   (RECE100-KAISU(2) NOT =  CNT-MESAIKSN)
               AND   (CNT-SAISIN-ETC   NOT =  CNT-MESAIKSN)
                     MOVE   ZERO         TO   RECE100-KHNTEN(2)
               END-IF
             END-IF
      *
           .
       2110-NYUYOJI-COMENTFLG-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    Á´³Ñ¢ª¿ôÃÍÈ¾³ÑÊÑ´¹½èÍý
      *****************************************************************
       800-NUM-ZENKAKU-HENKAN-SEC    SECTION.
      *
           MOVE    ZERO                TO  WRK-STIDX
                                           WRK-EDIDX 
           MOVE    SPACE               TO  WRK-NUM
           MOVE    ZERO                TO  WRK-NUM-9
      *
      *    ¶õÇò¤Î¤È¤­¤Ï¥¼¥í 
           IF      WRK-ZENKAKU          =       SPACE
               MOVE    ZERO                TO  WRK-NUM
               MOVE    10                  TO  IDY   
           ELSE
      *
      *        Ê¸»ú¤Î³«»Ï·å
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX         >   80
                       OR      WRK-STIDX   >   ZERO
                   IF      WRK-ZENKAKU (IDX:1)  NOT =   SPACE
                       MOVE    IDX                 TO  WRK-STIDX
                   END-IF     
               END-PERFORM
      *        Ê¸»ú¤Î½ªÎ»·å
               PERFORM VARYING IDX FROM    80  BY  -1
                       UNTIL   IDX         <   1
                       OR      WRK-EDIDX   >   ZERO
                   IF      WRK-ZENKAKU (IDX:1)  NOT =   SPACE
                       MOVE    IDX                 TO  WRK-EDIDX
                   END-IF     
               END-PERFORM
      *
      *        Á´³Ñ¿ô»ú¤Î¤È¤­È¾³Ñ¿ô»ú¤ËÊÑ´¹¡Ê¿ô»ú°Ê³°¤Î¤È¤­¤Ï¥¼¥í¡Ë
               IF      WRK-ZENKAKU (WRK-STIDX:1)   >=  X"A1"
                   MOVE    ZERO                TO  IDX3
                   PERFORM VARYING     IDX     FROM    WRK-STIDX  BY  2
                           UNTIL       IDX     >       WRK-EDIDX
                       IF      WRK-ZENKAKU (IDX:2)  >= "£°"  AND
                                                   <=  "£¹"
                           ADD     1               TO  IDX3
                           EVALUATE    WRK-ZENKAKU (IDX:2)
                               WHEN    "£°"
                                   MOVE    "0"     TO  WRK-NUM (IDX3:1)
                               WHEN    "£±"
                                   MOVE    "1"     TO  WRK-NUM (IDX3:1)
                               WHEN    "£²"
                                   MOVE    "2"     TO  WRK-NUM (IDX3:1)
                               WHEN    "£³"
                                   MOVE    "3"     TO  WRK-NUM (IDX3:1)
                               WHEN    "£´"
                                   MOVE    "4"     TO  WRK-NUM (IDX3:1)
                               WHEN    "£µ"
                                   MOVE    "5"     TO  WRK-NUM (IDX3:1)
                               WHEN    "£¶"
                                   MOVE    "6"     TO  WRK-NUM (IDX3:1)
                               WHEN    "£·"
                                   MOVE    "7"     TO  WRK-NUM (IDX3:1)
                               WHEN    "£¸"
                                   MOVE    "8"     TO  WRK-NUM (IDX3:1)
                               WHEN    "£¹"
                                   MOVE    "9"     TO  WRK-NUM (IDX3:1)
                           END-EVALUATE
      *****************ELSE
      *****************    MOVE    ZERO            TO  WRK-NUM
      *****************    MOVE    WRK-EDIDX       TO  IDX
      *****************    MOVE    10              TO  IDX3
                       END-IF
                   END-PERFORM
                   MOVE    IDX3                TO  IDY     
               ELSE
      *        È¾³Ñ¿ô»ú¤ËÊÑ´¹¡Ê¿ô»ú°Ê³°¤Î¤È¤­¤Ï¥¼¥í¡Ë
                   INSPECT WRK-ZENKAKU(WRK-STIDX:)  REPLACING   ALL " "
                                                                BY  "0"
                   IF      WRK-ZENKAKU(WRK-STIDX:)     NOT NUMERIC
                       PERFORM VARYING IDX     FROM    WRK-STIDX  BY  1
                               UNTIL   IDX     >       WRK-EDIDX
                           IF      WRK-ZENKAKU (IDX:1) >=  "0" AND
                                                       <=  "9"
                               ADD     1           TO  IDX3
                               MOVE    WRK-ZENKAKU (IDX:1)
                                                   TO  WRK-NUM (IDX3:1)
                           END-IF
                       END-PERFORM    
                       MOVE    IDX3                TO  IDY     
                   ELSE
                       COMPUTE IDY     =   WRK-EDIDX   -
                                           WRK-STIDX   +   1
                       MOVE    WRK-ZENKAKU (WRK-STIDX:IDY) TO  WRK-NUM
                   END-IF     
               END-IF
           END-IF
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-NUM-X           TO  SNUM-INX
           CALL    "ORCSNUM"           USING   ORCSNUMAREA
           IF      SNUM-RC             =   ZERO
               MOVE    SNUM-OUTNUM         TO  WRK-NUM-9
           ELSE
               MOVE    ZERO                TO  WRK-NUM-9 
           END-IF
      *
           .
       800-NUM-ZENKAKU-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Æ¡¼¥Ö¥ë¤Î¸¡º÷
      *****************************************************************
       900-TENSU-TBL-SEC               SECTION.
      *
           MOVE    ZERO                 TO  FLG-TENSU
           MOVE    SRY-SRYCD(IDX2)      TO  TTNS-CHK-SRYCD
           MOVE    RECE130-SRYYM         TO  WK-TAIHI-SRYYM
           MOVE    WK-TAIHI-DAY         TO  WK-TAIHI-SRYD
           MOVE    WK-TAIHI-SRYYMD      TO  TTNS-CHK-YMD
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯¥Á¥§¥Ã¥¯
           PERFORM 9001-TTNS-CHK-SEC
      *
      *    ÂàÈò¥ï¡¼¥¯¤ËÂ¸ºß¤·¤Ê¤¤¾ì¹ç¤Ï£Ä£Â¤ò¸¡º÷
           IF    ( TTNS-FLG            =   ZERO )
               INITIALIZE                           TNS-TENSU-REC
grpsys         MOVE    SPA-HOSPNUM          TO  TNS-HOSPNUM
               MOVE    SRY-SRYCD(IDX2)      TO  TNS-SRYCD
               MOVE    RECE130-SRYYM         TO  WK-TAIHI-SRYYM
               MOVE    WK-TAIHI-DAY         TO  WK-TAIHI-SRYD
               MOVE    WK-TAIHI-SRYYMD      TO  TNS-YUKOSTYMD
                                                    TNS-YUKOEDYMD
               MOVE    TNS-TENSU-REC        TO  MCPDATA-REC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_tensu"     TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM  910-TENSU-READ-SEC
                   IF       FLG-TENSU       =       1
                       DISPLAY   "TENSU FETCH ERR"  
                   ELSE
      *                ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯¤ËÊÔ½¸¤¹¤ë
                       PERFORM 9001-TTNS-HEN-SEC
                   END-IF
               ELSE
                   DISPLAY   "TENSU SELECT ERR" 
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1               TO  FLG-TENSU
               END-IF
      *
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *
           .
       900-TENSU-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       9001-TTNS-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  TTNS-FLG
           INITIALIZE                      TNS-TENSU-REC
      *
           PERFORM VARYING TTNS-IDX    FROM    1   BY  1
                   UNTIL ( TTNS-IDX    >   TTNS-MAX )
                    OR   ( TTNS-FLG    =   1 )
               IF    ( TTNS-KEY-SRYCD (TTNS-IDX)
                                       =   TTNS-CHK-SRYCD )
                 AND ( TTNS-KEY-YUKOSTYMD (TTNS-IDX)
                                      <=   TTNS-CHK-YMD )
                 AND ( TTNS-KEY-YUKOEDYMD (TTNS-IDX)
                                      >=   TTNS-CHK-YMD )
                   MOVE    1           TO  TTNS-FLG
                   MOVE    TTNS-DAT (TTNS-IDX)
                                       TO  TNS-TENSU-REC
               END-IF
           END-PERFORM
      *
           .
       9001-TTNS-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿ÂàÈò¥ï¡¼¥¯ÊÔ½¸½èÍý
      *****************************************************************
       9001-TTNS-HEN-SEC               SECTION.
      *
           IF      TNS-TANIMOJISU    =   ZERO
           AND     TNS-TANINAME  NOT =   SPACE
               PERFORM   VARYING   IDW2   FROM   24   BY  -1
                          UNTIL    IDW2    <      1
                  IF   TNS-TANINAME(IDW2:1)  NOT =   SPACE
                      COMPUTE  TNS-TANIMOJISU   =   IDW2   /   2
                      MOVE    1     TO    IDW2
                  END-IF
               END-PERFORM
           END-IF
      *
           IF    ( TTNS-CONST-MAX  >   TTNS-MAX )
               COMPUTE TTNS-MAX    =   TTNS-MAX    +   1
               MOVE    TNS-SRYCD       TO  TTNS-KEY-SRYCD
                                                   (TTNS-MAX)
               MOVE    TNS-YUKOSTYMD   TO  TTNS-KEY-YUKOSTYMD
                                                   (TTNS-MAX)
               MOVE    TNS-YUKOEDYMD   TO  TTNS-KEY-YUKOEDYMD
                                                   (TTNS-MAX)
               MOVE    TNS-TENSU-REC   TO  TTNS-DAT
                                                   (TTNS-MAX)
           END-IF
      *
           .
       9001-TTNS-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ôÆÉ¹þ
      *****************************************************************
       910-TENSU-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               MOVE    ZERO            TO  FLG-TENSU
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ñ·×ÆÉ¹þ
      *****************************************************************
       900-SRYACCT-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               IF        ACCT-SRYKBN        =    "95"   OR  "96"
                   GO  TO  900-SRYACCT-READ-SEC
               ELSE
                   MOVE    ZERO            TO  FLG-SRYACCT
               END-IF
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°ÙÆÉ¹þ
      *****************************************************************
       900-SRYACT-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       960-KANRIMST-READ-SEC         SECTION.
      * 
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       960-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       970-KANRIMST-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       970-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¥³¥á¥ó¥È¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       950-PTCOM-READ-SEC           SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTCOM-REC
                   MOVE    ZERO                TO  FLG-PTCOM
               ELSE
                   MOVE    1                   TO  FLG-PTCOM
                   INITIALIZE                  PTCOM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTCOM
               INITIALIZE                  PTCOM-REC
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       950-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ãæ´Ö¥Õ¥¡¥¤¥ë£Ò£Å£Á£Ä½èÍý
      *****************************************************************
       RECE100-READ-SEC            SECTION.
      *
           READ    RECE100-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-RECE100
               NOT  AT  END
                   ADD     1     TO    CNT-IN
                   ADD     1     TO    CNT-IN-COMMIT
                                       CNT-IN-CANCEL
      *
                   DISPLAY "RECE100 PTNUM=" RECE100-PTNUM     
      *
                   IF      WRK-PARA-SRYYM     =  ZERO
      *                 °åÎÅµ¡´ØÊÔ½¸
                       MOVE  RECE100-SRYYM  TO  WRK-SYS-SRYYMD(1:6)
                       MOVE  "01"           TO  WRK-SYS-SRYYMD(7:2)
                       PERFORM 900-SYSKANRI-HENSYU-SEC
                   END-IF
           END-READ
           .
       RECE100-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¿ÇÎÅÇ¯·î¤ÎÂÐ¾Ý¤È¤Ê¤ë¥·¥¹´ÉÍý¤ò¼èÆÀ¡Ê¸ÄÊÌ½èÍý¤Î¤ß
      ****************************************************************
       900-SYSKANRI-HENSYU-SEC         SECTION.
      *
      *    »ÜÀß´ð½à¾ðÊó
           MOVE    ZERO                TO  FLG-SET
      *
           PERFORM  VARYING  SYSKAN-IDX2  FROM  1   BY  1
                     UNTIL ( SYSKAN-IDX2               >  100  )
                      OR   ( TBL-SYS10062(SYSKAN-IDX2) = SPACE )
                      OR   ( FLG-SET                   =   1   )
               MOVE    TBL-SYS10062(SYSKAN-IDX2)  TO  SYS-1006-REC
               IF   ( SYS-1006-STYUKYMD  <=  WRK-SYS-SRYYMD    )
               AND  ( WRK-SYS-SRYYMD     <=  SYS-1006-EDYUKYMD )
                   MOVE     1                  TO  FLG-SET
               END-IF
           END-PERFORM
           IF      FLG-SET    =   ZERO
               MOVE  ZERO                    TO  WRK-SISETU613
               MOVE  ZERO                    TO  WRK-SISETU695
           ELSE
               MOVE  SYS-1006-SSTKIJUN(113)  TO  WRK-SISETU613
               MOVE  SYS-1006-SSTKIJUN(195)  TO  WRK-SISETU695
           END-IF
      *
           MOVE    ZERO                TO  FLG-SET
      *
           PERFORM  VARYING  SYSKAN-IDX2  FROM  1   BY  1
                     UNTIL ( SYSKAN-IDX2               >  100  )
                      OR   ( TBL-SYS10067(SYSKAN-IDX2) = SPACE )
                      OR   ( FLG-SET                   =   1   )
               MOVE    TBL-SYS10067(SYSKAN-IDX2)  TO  SYS-1006-REC
               IF   ( SYS-1006-STYUKYMD  <=  WRK-SYS-SRYYMD    )
               AND  ( WRK-SYS-SRYYMD     <=  SYS-1006-EDYUKYMD )
                   MOVE     1                  TO  FLG-SET
               END-IF
           END-PERFORM
           IF      FLG-SET    =   ZERO
               MOVE  ZERO                    TO  WRK-SISETU3055
               MOVE  ZERO                    TO  WRK-SISETU3056
               MOVE  ZERO                    TO  WRK-SISETU3057
               MOVE  ZERO                    TO  WRK-SISETU3058
               MOVE  ZERO                    TO  WRK-SISETU3168
               MOVE  ZERO                    TO  WRK-SISETU3169
           ELSE
               MOVE  SYS-1006-SSTKIJUN(55)   TO  WRK-SISETU3055
               MOVE  SYS-1006-SSTKIJUN(56)   TO  WRK-SISETU3056
               MOVE  SYS-1006-SSTKIJUN(57)   TO  WRK-SISETU3057
               MOVE  SYS-1006-SSTKIJUN(58)   TO  WRK-SISETU3058
               MOVE  SYS-1006-SSTKIJUN(168)  TO  WRK-SISETU3168
               MOVE  SYS-1006-SSTKIJUN(169)  TO  WRK-SISETU3169
           END-IF
      *
           .
       900-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿¹¹¿·½èÍý¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       RECE100-REWRITE-SEC                SECTION.
      *
           REWRITE   RECE100-REC
      *
           ADD     1                   TO  CNT-OUT3
           .
       RECE100-REWRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿½ÐÎÏ½èÍý¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       RECE120-WRITE-SEC                SECTION.
      *
           WRITE   RECE120-REC
      *
           ADD     1                   TO  CNT-OUT1
           .
       RECE120-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿½ÐÎÏ½èÍý¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       RECE130-WRITE-SEC                SECTION.
      *
           WRITE   RECE130-REC
      *
           ADD     1                   TO  CNT-OUT2
      *
           .
       RECE130-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿½ÐÎÏ½èÍý¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       RECE130-REWRITE-SEC              SECTION.                        
      *
              MOVE    ZERO                 TO  FLG-REWRITE
      *
              INITIALIZE                       RECE130-REC
              MOVE    RECE120-KEY1         TO  RECE130-KEY1
              MOVE    RECE120-ZAINUM       TO  RECE130-ZAINUM
              START   RECE130-FILE
                    KEY IS    >=   RECE130-KEY
                    INVALID
                        MOVE    1         TO  FLG-RECE130
                    NOT INVALID
                        MOVE    ZERO      TO  FLG-RECE130
                        READ    RECE130-FILE   NEXT
                          AT  END
                            MOVE    1     TO  FLG-RECE130
                        END-READ
              END-START
              PERFORM   UNTIL
                  (RECE120-KEY1         NOT =  RECE130-KEY1  ) OR
                  (RECE120-ZAINUM       NOT =  RECE130-ZAINUM) OR
                  (FLG-RECE130              =      1         ) OR
                  (FLG-REWRITE              =      1         )
                   IF    RECE130-RENNUM   =   WRK-RENNUM
                      MOVE    1              TO  FLG-REWRITE
                      PERFORM  VARYING  IDXB  FROM  5   BY  -1
                                UNTIL   IDXB   <    1
                        IF  RECE130-SRYCD(IDXB)  NOT = SPACE
                          MOVE WRK-SRYSURYO3 TO RECE130-SRYSURYO(IDXB)
                          MOVE 1             TO IDXB
                        END-IF
                      END-PERFORM
                      REWRITE   RECE130-REC
                   END-IF
                   READ    RECE130-FILE   NEXT
                     AT  END
                       MOVE    1     TO  FLG-RECE130
                   END-READ
              END-PERFORM
           .
       RECE130-REWRITE-EXT.
           EXIT.                                                        
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿½ÐÎÏ½èÍý¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       RECE130-REWRITE2-SEC              SECTION.                        
      *
              MOVE    ZERO                 TO  FLG-REWRITE
      *
              INITIALIZE                       RECE130-REC
              MOVE    RECE120-KEY1         TO  RECE130-KEY1
              MOVE    RECE120-ZAINUM       TO  RECE130-ZAINUM
              START   RECE130-FILE
                    KEY IS    >=   RECE130-KEY
                    INVALID
                        MOVE    1         TO  FLG-RECE130
                    NOT INVALID
                        MOVE    ZERO      TO  FLG-RECE130
                        READ    RECE130-FILE   NEXT
                          AT  END
                            MOVE    1     TO  FLG-RECE130
                        END-READ
              END-START
              PERFORM   UNTIL
                  (RECE120-KEY1         NOT =  RECE130-KEY1  ) OR
                  (RECE120-ZAINUM       NOT =  RECE130-ZAINUM) OR
                  (FLG-RECE130              =      1         ) OR
                  (FLG-REWRITE              =      1         )
                   IF    RECE130-RENNUM   =   WRK-RENNUM2
                      MOVE    1              TO  FLG-REWRITE
                      PERFORM  VARYING  IDXB  FROM  5   BY  -1
                                UNTIL   IDXB   <    1
                        IF  RECE130-SRYCD(IDXB)  NOT = SPACE
                          MOVE   "1"  TO   RECE130-RECEPTFLG(IDXB)
                          MOVE    1   TO   IDXB
                        END-IF
                      END-PERFORM
                      REWRITE   RECE130-REC
                   END-IF
                   READ    RECE130-FILE   NEXT
                     AT  END
                       MOVE    1     TO  FLG-RECE130
                   END-READ
              END-PERFORM
           .
       RECE130-REWRITE2-EXT.
           EXIT.                                                        
      *
      *****************************************************************
      *    ¥¨¥é¡¼½ÐÎÏ½èÍý
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *         
             IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ¥¸¥ç¥Ö´ÉÍý³«»Ï½èÍý
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
             END-IF
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½èÍýÃæ»ß½èÍý
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               CONTINUE
           ELSE
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
               IF    ( SJOBKANRI-RETURN
                                       =   ZERO )
               AND   ( JOB-STOPFLG     =   1    )
                   MOVE    "½èÍý¤¬Ãæ»ß¤µ¤ì¤Þ¤·¤¿"
                                           TO  WRK-RECEERR
      *
                   OPEN    INPUT   RECEERR-FILE
                   IF      STS-RECEERR         =   ZERO
                       CLOSE   RECEERR-FILE
                   ELSE
                       OPEN    OUTPUT              RECEERR-FILE
      *    
                       MOVE    WRK-RECEERR         TO  RECEERR-REC
                       WRITE   RECEERR-REC
                       CLOSE   RECEERR-FILE
      *         
      *                ¥¸¥ç¥Ö´ÉÍý½ªÎ»½èÍý
                       MOVE    "CAN"           TO  SJOBKANRI-MODE
                       INITIALIZE                  JOBKANRI-REC
                       MOVE    WRK-RECEERR     TO  JOB-YOBI
                       MOVE    "8888"          TO  JOB-ERRCD
                       MOVE    2               TO  JOB-STOPFLG
grpsys                 PERFORM 900-CALL-ORCSJOB-SEC
                   END-IF
      *                             
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½ªÎ»½èÍý
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *    ¥¹¥Æ¥Ã¥×´ÉÍý½ªÎ»½èÍý
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE100  CNT-IN  :" CNT-IN 
           DISPLAY "RECE120  CNT-OUT :" CNT-OUT1  
           DISPLAY "RECE130  CNT-OUT :" CNT-OUT2  
           DISPLAY "*** ORCR1010 END ***"
           .
       300-TERM-EXT.
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
      *    £Ä£Â£Ó£Å£Ì£Å£Ã£Ô½èÍý
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
      *
      *****************************************************************
      *    £Ä£Â¡¡£Ã£Ï£Í£Í£É£Ô½èÍý
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *      
      *****************************************************************
      *    £Ä£Â¡¡£Ó£Ô£Á£Ò£Ô½èÍý
      *****************************************************************
       900-DBSTART-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSTART-EXT.
           EXIT.
      *
