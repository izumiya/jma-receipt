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
       PROGRAM-ID.             ORCR0420.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¥ì¥»¥×¥È¡ÊÏ«ºÒ¡¦¼«ÇåÀÕ¡Ë
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ·î¼¡¶ÈÌ³¡¡Ï«ºÒ¼«ÇåÀÕÌÀºÙ½ñ¥Õ¥¡¥¤¥ëºîÀ®
      *                      ¿ÇÎÅ¶èÊ¬ÊÌÅÀ¿ô½¸·×¡ÊÏ«ºÒ¡¦¼«ÇåÀÕ¡Ë
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  02/06/18    MCC¡ÝÌçÏÆ¡¡   ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01    NACL-ÌçÏÆ    02.09.12  ¼«ÇåÀÕÂÐ±þ¡¡¡¡¡¡¡¡
      *  01.00.02    NACL-Æ£¸¶    02.09.20  ¸¡ºº¤ÎÊÔ½¸¤Î¤¿¤á¤Ë²ó¿ô¤ò
      *                                     Á´¤ÆÊÔ½¸¤¹¤ë
      *  01.00.03    NACL-ÌçÏÆ    02.10.02  Å¦Í×½ÐÎÏ¹àÌÜÊÑ¹¹¤Î°Ù¤ÎÊÔ½¸
      *  01.00.04    NACL-ÌçÏÆ    02.11.06  ÌóÂ«½èÊý»þ¤ÎÅÀ¿ôÍóÉ½¼¨½¤Àµ
      *  01.00.05    NACL-ÌçÏÆ    02.11.21  ¼«ÇåÀÕ¡Ê¥Ý¥ê¥Í¥Ã¥¯Åù¤ÎÂÐ±þ¡Ë
      *  01.00.06    NACL-ÌçÏÆ    02.11.26  ¸¡¿Ç¥³¡¼¥É¤òÊÔ½¸¤Ç¤­¤ë¤è¤¦½¤Àµ
      *  01.00.07    NACL-Æ£¸¶    02.12.02  »ÀÁÇ¤Î»þÃ±²Á¤òÉ½¼¨¤¹¤ë¤è¤¦¤ËÄÉ²Ã
      *  01.00.08    NACL-Æ£¸¶    03.01.22  ¥Ç¡¼¥¿¶èÊ¬¤ÎÄÉ²Ã
      *  01.00.09    NACL-ÌçÏÆ    03.02.06  ¥³¥á¥ó¥È¥«¥¦¥ó¥È¥Õ¥é¥°¤ÎÊÔ½¸ÄÉ²Ã
      *  01.00.10    NACL-ÌçÏÆ    03.03.10  ¿ÇÎÅ²Ê£±£°¤«¤é£²£°¤Ø
      *  01.00.11    NACL-Æ£¸¶    03.04.09  ÆÃÄê´ïºà¡Ê²èÁü¿ÇÃÇ¤Î¥Õ¥£¥ë¥à°Ê³°¡Ë
      *                                     ¤ÎÃ±²Á¤òÉ½¼¨¤¹¤ë¤è¤¦¤Ë½¤Àµ
      *  01.00.12    NACL-ÌçÏÆ    03.04.14  ¼«ÇåÀÕ¤Î¾®¿ôÅÀ°Ê²¼¶â³Û·×»»
      *                                     »Í¼Î¸ÞÆþ¤ËÊÑ¹¹
      *  01.00.13    NACL-ÌçÏÆ    03.05.08  Ì¾¾ÎÍ­¸ú·å¤Î¥»¥Ã¥È
      *  01.00.14    NACL-ÌçÏÆ    03.05.13  ¼«ÇåÀÕ¡§¹øÉô¸ÇÄêÂÓ²Ã»»ÂÐ±þ
      *  01.00.15    NACL-ÌçÏÆ    03.05.20  ¼«ÇåÀÕ¡§ÎÅÍÜÃ´Åö¼êÅö¤ÆÂÐ±þ
      *  01.00.16    NACL-ÌçÏÆ    03.05.22  »ÄÎÌÇÑ´þ¤Î¿ôÎÌ¥»¥Ã¥È
      *  01.00.17    NACL-ÌçÏÆ    03.06.03  ³°Íè¿ÇÎÅÎÁÂÐ±þ
      *                                     ¡ÊÊ¿À®£±£µÇ¯£¶·î°Ê¹ß¡Ë
      *  01.00.18    NACL-ÌçÏÆ    03.09.24  ¥³¥á¥ó¥ÈÊÔ½¸¥Õ¥é¥°¤Î¥»¥Ã¥È
      *  01.00.19    NACL-ÌçÏÆ    03.09.29  Êñ³çÂÐ¾Ý¸¡ºº¾ðÊó¤Î¥»¥Ã¥È
      *                                     ¸¡ººÅù¼Â»ÜÈ½ÃÇ¾ðÊó¤Î¥»¥Ã¥È
      *                                     ³Æ¼ïÈÖ¹æÅù¾ðÊó¤Î¥»¥Ã¥È
      *  01.00.20    NACL-ÌçÏÆ    03.10.06  ÅÀ¿ô¥Þ¥¹¥¿¤òÆÉ¤ß¹þ¤Þ¤Ê¤¤¤è¤¦
      *                                     ½¤Àµ
      *  01.00.21    NACL-ÌçÏÆ    04.01.06  ¿ÇÎÅ¹Ô°Ù¥³¡¼¥É¤Î£±·åÌÜ¤ÎÈ½Äê
      *                                     ½¤Àµ
      *  01.00.22    NACL-ÌçÏÆ    04.01.21  Ï«ºÒ¤ÎÆÃÄê´ïºà(09593)ÂÐ±þ
      *  01.00.23    NACL-ÌçÏÆ    04.02.05  ¿ÇÎÅ¥³¡¼¥É¡ö¿ôÎÌ¤ÎÎß·×ÊÔ½¸
      *  £²£°£°£´Ç¯£´·î²þÀµÂÐ±þ
      *  01.01.01    NACL-ÌçÏÆ    04.04.01  Ä¹´üÅêÌô²Ã»»¡Ê½èÊýÎÁ¡¢½èÊý¤»¤ó
      *                                     ÎÁ¡ËÄÉ²ÃÂÐ±þ
      *  01.01.02    NACL-ÌçÏÆ    04.04.01  ¥ê¥Ï¸À¸ìÄ°³ÐÎÅË¡¡Ê£³¡ËÄÉ²ÃÂÐ±þ
      *  01.01.03    NACL-ÌçÏÆ    04.04.05  ºßÂð·ì±ÕÆ©ÀÏÉÑ²ó»ØÆ³´ÉÍýÎÁ¤Î
      *                                     ¥³¥á¥ó¥ÈÂÐ±þ
      *  01.01.04    NACL-ÌçÏÆ    04.04.10  ºßÂð´µ¼ÔË¬ÌäÅÀÅ©Ãí¼Í´ÉÍý»ØÆ³ÎÁ
      *                                     »»Äê»þ¤ÎÅÀÅ©Ãí¼Í¤Î¥³¥á¥ó¥ÈÂÐ±þ
      *  01.01.05    NACL-ÌçÏÆ    04.04.26  ¥æ¡¼¥¶¡¼¥³¥á¥ó¥ÈÄÉ²ÃÂÐ±þ
      *  01.01.06    NACL-¾®Æ¦Âô  04.06.08  £Ã£Ô¡¦£Í£Ò£É¡Ê£²²óÌÜ°Ê¹ß¡Ë¤Î
      *                                     Å¦Í×Íóµ­ºÜ½ç½¤Àµ
      *  01.01.07    NACL-ÌçÏÆ    04.06.11  Èé²¼¶ÚÆùÃí¼Í¤Ç¡¢ÌôºÞÅÀ¡¦´ïºàÅÀ
      *                                     ¤¬Ìµ¤¤¤Î¾ì¹ç¤ÎÅ¦Í×Íóµ­ºÜÂÐ±þ
      *  01.01.08    NACL-ÌçÏÆ    04.07.28  ÅêÌôÎÁ¤Î´ïºàÅÀ¤Î½¸·×½¤Àµ
      *  01.01.09    NACL-ÌçÏÆ    04.07.28  µÙ¶È¾ÚÌÀ¥Õ¥é¥°¡¢µÙ¶È´ü´Ö½ªÎ»·î
      *                                     Æü¤ÎÊÔ½¸
      *  01.01.10    NACL-ÌçÏÆ    04.10.15  ¼«ÇåÀÕÆÃÄê´ïºàÅù¾ÃÈñÀÇÂÐ±þ
      *  Ãæ»ß½èÍýÄÉ²Ã
      *  01.02.01    NACL-Æ£¸¶    04/10/13  Ãæ»ß½èÍý¤ÎÄÉ²Ã
      *  01.02.02    NACL-ÌçÏÆ    04/12/17  ¶»Éô¸ÇÄêÂÓ²Ã»»¡¢ÁÏ³°¸ÇÄê´ï
      *                                     »ÈÍÑ²Ã»»¤òÌôºÞÅù¤Ø
      *  01.02.03    NACL-ÌçÏÆ    05/01/18  »ÀÁÇ·×»»¼°¥³¥á¥ó¥ÈÂÐ±þ
      *  01.02.04    NACL-ÌçÏÆ    05/01/26  ¥·¥¹¥Æ¥à´ÉÍý£´£°£°£±¤Ë´Ø¤¹¤ë½¤Àµ
      *  01.02.05    NACL-ÌçÏÆ    05/01/27  ¹ø¡¦¶»Éô¸ÇÄêÂÓ²Ã»»¡¢ÁÏ³°¸ÇÄê´ï
      *                                     »ÈÍÑ²Ã»»¤Îµ»½Ñ¡¦ÌôºÞÅù¤ÎÁªÂòÂÐ±þ
      *  01.02.06    NACL-ÌçÏÆ    05/01/28  Ê¸½ñÎÁ¤Î¥³¥á¥ó¥ÈÂÐ±þ
      *  01.02.07    NACL-ÌçÏÆ    05/02/28  ¡Ê¥µ¡Ëµ­ºÜÂÐ±þ
      *  01.02.08    NACL-ÌçÏÆ    05/03/03  ÆÃµ­»ö¹à¤ÎÊÔ½¸½¤Àµ
      *  01.02.09    NACL-ÌçÏÆ    05/04/20  ÆâÉþÌôºÞÄþ¸ºÊÔ½¸ÂÐ±þ
      *  01.02.10    NACL-ÌçÏÆ    05/05/19  ÅÀ¿ô¥Þ¥¹¥¿¤ÎÃ±°ÌÊ¸»ú¿ô¤¬¥¼¥í
      *                                     ¤Î¾ì¹ç¤ÎÊÔ½¸
      *  01.02.11    NACL-¾®Æ¦Âô  05/10/28  ÆÃµ­»ö¹à¡¡Àè¿Ê¡¢À©Ä¶¡¡ÂÐ±þ
      *  01.02.12    NACL-ÌçÏÆ    05/11/01  ¡Ê¸¡Áª¡ËÅù¥³¥á¥ó¥ÈÂÐ±þ
      *  01.02.13    NACL-ÌçÏÆ    05/11/11  ¥³¥á¥ó¥È¤¬¤¢¤ë¾ì¹ç¤ÎÊÔ½¸½¤Àµ
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE41-FILE     ASSIGN  RECE41PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE41-KEY
                                   FILE    STATUS  IS  STS-RECE41.
           SELECT  RECE44-FILE     ASSIGN  RECE44PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE44-KEY
                                   FILE    STATUS  IS  STS-RECE44.
           SELECT  RECE444-FILE    ASSIGN  RECE44PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE444-KEY
                                   FILE    STATUS  IS  STS-RECE44.
           SELECT  RECE4A-FILE     ASSIGN  RECE4APARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE4A-KEY
                                   FILE    STATUS  IS  STS-RECE4A.
           SELECT  RECE4A4-FILE    ASSIGN  RECE4APARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE4A4-KEY
                                   FILE    STATUS  IS  STS-RECE4A.
           SELECT  RECE45-FILE     ASSIGN  RECE45PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE45-KEY
                                   FILE    STATUS  IS  STS-RECE45.
           SELECT  RECE455-FILE    ASSIGN  RECE45PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE455-KEY
                                   FILE    STATUS  IS  STS-RECE45.
           SELECT  RECE4B-FILE     ASSIGN  RECE4BPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE4B-KEY
                                   FILE    STATUS  IS  STS-RECE4B.
           SELECT  RECE4B5-FILE    ASSIGN  RECE4BPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE4B5-KEY
                                   FILE    STATUS  IS  STS-RECE4B.
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE41-FILE.
       01  RECE41-REC. 
           COPY    "CPRCF041.INC".
      *
       FD  RECE44-FILE.
       01  RECE44-REC. 
           COPY    "CPRCF044.INC".
      *
       FD  RECE444-FILE.
       01  RECE444-REC. 
           COPY    "CPRCF044.INC"  REPLACING  //RECE44//
                                   BY         //RECE444//.
      *
       FD  RECE4A-FILE.
       01  RECE4A-REC. 
           COPY    "CPRCF044.INC"  REPLACING  //RECE44//
                                   BY         //RECE4A//.
      *
       FD  RECE4A4-FILE.
       01  RECE4A4-REC. 
           COPY    "CPRCF044.INC"  REPLACING  //RECE44//
                                   BY         //RECE4A4//.
      *
       FD  RECE45-FILE.
       01  RECE45-REC. 
           COPY    "CPRCF045.INC".
      *
       FD  RECE455-FILE.
       01  RECE455-REC. 
           COPY    "CPRCF045.INC"  REPLACING  //RECE45//
                                   BY         //RECE455//.
      *
       FD  RECE4B-FILE.
       01  RECE4B-REC. 
           COPY    "CPRCF045.INC"  REPLACING  //RECE45//
                                   BY         //RECE4B//.
      *
       FD  RECE4B5-FILE.
       01  RECE4B5-REC. 
           COPY    "CPRCF045.INC"  REPLACING  //RECE45//
                                   BY         //RECE4B5//.
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *     ¥ì¥»¥×¥ÈÌÀºÙ¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE41//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE44//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE4A//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE45//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE4B//.
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  STS-AREA.
           03  STS-RECE41                              PIC X(02).
           03  STS-RECE44                              PIC X(02).
           03  STS-RECE45                              PIC X(02).
           03  STS-RECE4A                              PIC X(02).
           03  STS-RECE4B                              PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-SRYACT                              PIC 9(01).
           03  FLG-TENSU                               PIC 9(01).
           03  FLG-ZAITAKU                             PIC 9(01).
           03  FLG-OSINKBN                             PIC 9(01).
           03  FLG-PTCOM                               PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-DAY                                 PIC 9(01).
           03  FLG-SP                                  PIC 9(01). 
           03  FLG-ZANHAIKI                            PIC 9(01).
           03  FLG-RECE45                              PIC 9(01).
           03  FLG-REWRITE                             PIC 9(01).
           03  FLG-SET                                 PIC 9(01).
           03  FLG-HOUSIJI-COM                         PIC 9(01).
           03  FLG-HOUTEN-COM                          PIC 9(01).
           03  FLG-TAIGAI                              PIC 9(01).
           03  FLG-RINYAK                              PIC 9(01).
      *
      *    ´ðËÜÅÀ¿ô¤ÎÈóÉ½¼¨È½ÄêÍÑ¥Õ¥é¥°
       01  FLG-KHNTEN-AREA.
           03  FLG-KHNTEN2                             PIC 9(01).
           03  FLG-KHNTEN3                             PIC 9(01).
           03  FLG-KHNTEN4                             PIC 9(01).
           03  FLG-KHNTEN5                             PIC 9(01).
           03  FLG-KHNTEN6                             PIC 9(01).
           03  FLG-KHNTEN15                            PIC 9(01).
           03  FLG-KHNTEN18                            PIC 9(01).
           03  FLG-KHNTEN19                            PIC 9(01).
      *
      *    ´ðËÜÅÀ¿ô¤ÎÈóÉ½¼¨È½ÄêÍÑ¥Õ¥é¥°¡Ê¼«ÇåÀÕÍÑ¡Ë
       01  FLG-KHNTEN-JI-AREA.                                          
           03  FLG-KHNTEN10-JI                         PIC 9(01).       
           03  FLG-KHNTEN13-JI                         PIC 9(01).       
           03  FLG-KHNTEN14-JI                         PIC 9(01).       
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
           03  CNT-IN-COMMIT                           PIC 9(06).
           03  CNT-IN-CANCEL                           PIC 9(06).
           03  CNT-OUT1                                PIC 9(06).
           03  CNT-OUT2                                PIC 9(06).
           03  CNT-OUT3                                PIC 9(06).
           03  CNT-OUT4                                PIC 9(06).
           03  CNT-OUT5                                PIC 9(06).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                                     PIC 9(03).
           03  IDX2                                    PIC 9(03).
           03  IDX3                                    PIC 9(03).
           03  IDX4                                    PIC 9(03).
           03  IDX5                                    PIC 9(03).
           03  ID1                                     PIC 9(03). 
           03  IDY                                     PIC 9(03).
           03  IDW                                     PIC 9(03).
           03  IDV                                     PIC 9(03).
           03  IDU                                     PIC 9(03).
           03  NUM-IDX                                 PIC 9(01).
           03  SYSKAN-IDX                              PIC 9(03).
      *
      *    ¥Ñ¥¹Ì¾ÎÎ°è
       01  WK-PATH                                     PIC X(20).
      *
      *    ºÞ¾ðÊóÂàÈòÎÎ°è
       01  WK-ZAIKAISU                                 PIC S9(07).
       01  WK-ZAITEN                                   PIC S9(08).
       01  WK-SYUTEN                                   PIC S9(08).
       01  WK-KIZAITEN                                 PIC S9(08).
       01  WK-YKZTEN                                   PIC S9(08).
      *
      *    °åÎÅµ¡´Ø¡Ê´ðËÜ¾ðÊó¡ËÂàÈòÍÑÎÎ°è
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC                         OCCURS  10.
               05  TBL-SYS1001                         PIC X(610).
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
               05  WRK-TNS-DATAKBN                     PIC 9(01).
               05  WRK-TNS-TIMEKSNKBN                  PIC 9(01).
               05  WRK-TNS-TEN                         PIC 9(09)V99.    
               05  WRK-TNS-GAITENTTLKBN                PIC 9(03).
               05  WRK-TNS-OSINKBN                     PIC 9(01).
               05  WRK-TNS-KNSJISKBN                   PIC 9(01).
               05  WRK-TNS-SRYCD                       PIC X(09).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-PATH                                PIC X(64).
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                      PIC X(06).
               05  WRK-PARA-TERMID                     PIC X(16).
               05  WRK-PARA-SYSYMD                     PIC X(08).
               05  WRK-PARA-JOBID                      PIC 9(07).
               05  WRK-PARA-SHELLID                    PIC X(08).
      *
           03  WRK-SYS-SRYYMD                          PIC X(08).
      *
           03  WRK-RECEERR                             PIC X(200).
      *
           03  WRK-KEISAN                              PIC 9(09).
           03  WRK-TEN                                 PIC 9(09).
           03  WRK-MEISAI-CNT                          PIC 9(04).
           03  WRK-SRYCDTOTAL                          PIC 9(14).
           03  WRK-SURYOUTOTAL                         PIC 9(07)V9(03).
           03  WRK-KSNRATE                             PIC 9V99.    
           03  WRK-SRYCD                               PIC 9(09).   
           03  WRK-YOUBUKASAN                          PIC 9(07).
           03  WRK-KYOBUKASAN                          PIC 9(07).
           03  WRK-SOGAIKASAN                          PIC 9(07).
           03  WRK-RYOYOTEATE                          PIC 9(07).
           03  WRK-RYOYOTEATE2                         PIC 9(07).
      *
           03  WRK-KYUGYOFLG                           PIC X(01).
      *
           03  WRK-DAY                                 PIC 9(02).
      *
           03  WRK-ZAIKAISU-XXX                        PIC 9(03).
      *
           03  WRK-TANICD                              PIC 9(03).
           03  WRK-TANIMOJISU                          PIC 9(01).
           03  WRK-TANINAME                            PIC X(24).
           03  WRK-SRYSURYO                            PIC 9(05)V9(03).
           03  WRK-SRYSURYO2                           PIC 9(05).
           03  WRK-SRYSURYO3                           PIC 9(05).
           03  WRK-RENNUM                              PIC 9(02).
      *
           03  WRK-TEN-KZI                             PIC 9(09).
      *
      *    ¾ÃÈñÀÇÃ¼¿ô¶èÊ¬
           03  WRK-TAX1001-KBN                         PIC X(01).
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
           03  WRK-TANKA-9                     PIC 9(09)V9(02).
           03  WRK-TANKA-R  REDEFINES  WRK-TANKA-9.
               05  WRK-TANKA-S1                PIC 9(09).
               05  WRK-TANKA-S2                PIC 9(02).
           03  WRK-TANKA-X.
               05  WRK-TANKA-Z1                PIC ZZZZZZZZ9.
               05  WRK-TANKA-Z2                PIC X(01).
               05  WRK-TANKA-Z3                PIC ZZ.
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
      *---------------------------------------------------------------
      *
      *    ¸ÇÄêÃÍ
       01  CONST-AREA.
      *    ·ÑÂ³´ÉÍý²Ã»»
           03  CONS-KEIZOKU1   PIC X(09)     VALUE "112007170".
           03  CONS-KEIZOKU2   PIC X(09)     VALUE "112702270".
      *    ³°Íè¿ÇÎÅÎÁ¡Ê£±²óÌÜ¡Ë
           03  CONS-GAIRAIRYO1 PIC X(09)     VALUE "112011310".
           03  CONS-GAIRAIRYO2 PIC X(09)     VALUE "112705510".
      *    ½èÊý¤»¤óÎÁ
           03  CONS-SHOHOSEN1 PIC X(09)      VALUE "120002710".
           03  CONS-SHOHOSEN2 PIC X(09)      VALUE "120002810".
           03  CONS-SHOHOSEN3 PIC X(09)      VALUE "120002910".
           03  CONS-SHOHOSEN4 PIC X(09)      VALUE "120003010".
      *    ½èÊý¤»¤ó¡ÊÆýÍÄ»ù¡Ë²Ã»»
           03  CONS-SHOHOKSN1  PIC X(09)     VALUE "120002470".
           03  CONS-SHOHOKSN2  PIC X(09)     VALUE "120002570".
      *    Ä¹´üÅêÌô²Ã»»¡Ê½èÊý¤»¤óÎÁ¡Ë
           03  CONS-SHOHOKSN3  PIC X(09)     VALUE "120003270".
      *    ÌôºÞÎÁÄþ¸º¡Ê£¹£°¡¿£±£°£°¡Ë¡ÊÆâÉþÌô¡Ë
           03  CONS-YAKTEIGEN  PIC X(09)     VALUE "630010002".
      *    Ê´ºÕ¡Ê¥³¥á¥ó¥È¡Ë
           03  CONS-FUNSAI-COMMENT PIC X(09) VALUE "099209901".
      *    ºÎ·ì¡Ê£Â¡Ý£Á¡Ë
           03  CONS-SAIKETSU1      PIC X(09) VALUE "160101210".
      *    ºÎ·ì¡Ê£Â¡Ý£Ö¡Ë
           03  CONS-SAIKETSU2      PIC X(09) VALUE "160095710".
      *    ºÎ·ì¡Ê£Â¡Ý£Ã¡Ë
           03  CONS-SAIKETSU3      PIC X(09) VALUE "160095810".
      *    ÆÃµ­»ö¹à
           03  CONS-TOKKI1         PIC X(14) VALUE "00000099990101".
           03  CONS-TOKKI2         PIC X(14) VALUE "00000099990102".
           03  CONS-TOKKI3         PIC X(14) VALUE "00000099990103".
           03  CONS-TOKKI4         PIC X(14) VALUE "00000099990104".
           03  CONS-TOKKI5         PIC X(14) VALUE "00000099990105".
           03  CONS-TOKKI7         PIC X(14) VALUE "00000099990107".
           03  CONS-TOKKI8         PIC X(14) VALUE "00000099990108".
           03  CONS-TOKKI9         PIC X(14) VALUE "00000099990109".
           03  CONS-TOKKI10        PIC X(14) VALUE "00000099990110".
           03  CONS-TOKKI11        PIC X(14) VALUE "00000099990111".
           03  CONS-TOKKI12        PIC X(14) VALUE "00000099990112".
           03  CONS-TOKKI13        PIC X(14) VALUE "00000099990113".
           03  CONS-TOKKI14        PIC X(14) VALUE "00000099990114".
      *    ÆýÍÄ»ù²Ã»»
           03  CONS-NYUKSN1        PIC X(09) VALUE "111000370".         
           03  CONS-NYUKSN2        PIC X(09) VALUE "111011470".
      *    ºÆ¿Ç¡Ê£³·îÄ¶¡Ë¡Ê£µ²óÄ¶¡Ë¤Ê¤É
      *    ¡Ê£´£¶£µ¤Ë´Ø·¸¤¢¤ê¡Ë
           03  CONS-SAISHIN-A1     PIC X(09) VALUE "101120060".
      **   03  CONS-SAISHIN-A2     PIC X(09) VALUE "112001210".
      **   03  CONS-SAISHIN-A3     PIC X(09) VALUE "112701110".
      **   03  CONS-SAISHIN-A4     PIC X(09) VALUE "112000970".
      **   03  CONS-SAISHIN-A5     PIC X(09) VALUE "112001070".
      **   03  CONS-SAISHIN-A6     PIC X(09) VALUE "112006970".
      **   03  CONS-SAISHIN-A7     PIC X(09) VALUE "112007070".
      **   03  CONS-SAISHIN-A8     PIC X(09) VALUE "112001310".
      **   03  CONS-SAISHIN-A9     PIC X(09) VALUE "112701210".
      **   03  CONS-SAISHIN-A10    PIC X(09) VALUE "112001110".
      **   03  CONS-SAISHIN-A11    PIC X(09) VALUE "112701010".
      **   03  CONS-SAISHIN-A12    PIC X(09) VALUE "112001410".
      **   03  CONS-SAISHIN-A13    PIC X(09) VALUE "112701310".
      *    ³°Íè´ÉÍý²Ã»»¡Ê£³·îÄ¶¡Ë¡Ê£µ²óÄ¶¡Ë
      *    ¡Ê£´£¶£°¤Ë´Ø·¸¤¢¤ê¡Ë
           03  CONS-GAIRAI         PIC X(09) VALUE "101120090".
      *    Ï«ºÒ¼«ÇåÀÕ´Ø·¸
           03  CONS-ROUSAI1    PIC X(09)   VALUE   "101110010".
           03  CONS-ROUSAI2    PIC X(09)   VALUE   "101110020".
           03  CONS-ROUSAI3    PIC X(09)   VALUE   "101110030".
           03  CONS-ROUSAI4    PIC X(09)   VALUE   "101120030".
           03  CONS-ROUSAI5    PIC X(09)   VALUE   "101120040".
           03  CONS-ROUSAI6    PIC X(09)   VALUE   "101120050".
           03  CONS-ROUSAI7    PIC X(09)   VALUE   "101120060".
           03  CONS-ROUSAI8    PIC X(09)   VALUE   "101130010".
           03  CONS-ROUSAI9    PIC X(09)   VALUE   "101130020".
           03  CONS-ROUSAI10   PIC X(09)   VALUE   "101130030".
           03  CONS-ROUSAI11   PIC X(09)   VALUE   "101130040".
           03  CONS-ROUSAI12   PIC X(09)   VALUE   "101130050".
           03  CONS-ROUSAI13   PIC X(09)   VALUE   "101130060".
           03  CONS-ROUSAI14   PIC X(09)   VALUE   "101130070".
           03  CONS-ROUSAI15   PIC X(09)   VALUE   "101130080".
           03  CONS-ROUSAI16   PIC X(09)   VALUE   "101130090".
           03  CONS-ROUSAI17   PIC X(09)   VALUE   "101130100".
      *
           03  CONS-ROUCOM1    PIC X(09)   VALUE   "840800001".
           03  CONS-ROUCOM2    PIC X(09)   VALUE   "840800002". 
      *
      *    ¹øÉô¸ÇÄêÂÓ²Ã»»
           03  CONS-YOUBUKASAN PIC X(09)   VALUE   "140037490".
      *    ¶»Éô¸ÇÄêÂÓ²Ã»»
           03  CONS-KYOBUKASAN PIC X(09)   VALUE   "140040110".
      *    ÁÏ³°¸ÇÄê´ï»ÈÍÑ²Ã»»
           03  CONS-SOGAIKASAN PIC X(09)   VALUE   "150266970".
      *    ÎÅÍÜÃ´Åö¼êÅö¤Æ
           03  CONS-RYOYOTEATE PIC X(09)   VALUE   "199000610".
      *
      *
      *    ¥Õ¥£¥ë¥àÎÁ¡ÊÆýÍÄ»ù¡Ë²Ã»»
           03  CONS-FILM-YOUJI-KASAN  
                               PIC X(09)   VALUE   "799990070".
      *    »ÀÁÇÊäÀµÎ¨
           03  CONS-SANSO      PIC X(09)   VALUE   "770020070".
      *    ¹âµ¤°µ»ÀÁÇ²Ã»»
           03  CONS-SANSO2     PIC X(09)   VALUE   "770030070".
      *    »ÄÎÌÇÑ´þ
           03  CONS-ZANHAIKI   PIC X(09)   VALUE   "099309901".
      *
      *
      *    ËýÀ­¤È¤¦ÄË¼À´µ
           03  CONS-MANSEI           PIC X(09)  VALUE  "113006510".
      *    ½ýÉÂ¼êÅö¶â°Õ¸«½ñ¸òÉÕÎÁ
           03  CONS-SYOBYO-COMMENT   PIC X(09)  VALUE  "180000710".
      *    ÎÅÍÜÈñÆ±°Õ½ñ¸òÉÕÎÁ
           03  CONS-RYOYO-COMMENT    PIC X(09)  VALUE  "113004310".
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
      *    ºßÂð·ì±ÕÆ©ÀÏÉÑ²ó»ØÆ³´ÉÍýÎÁ
           03  CONS-HINKAI-COMMENT   PIC X(09)  VALUE  "114009410".
      *
      *    Èé²¼¶ÚÆùÃí¼Í
           03  CONS-HIKAKIN          PIC X(09)  VALUE  "130000510".
      *    À¸Êª³ØÅªÀ½ºÞÃí¼Í
           03  CONS-SEIBUTU          PIC X(09)  VALUE  "130000110".
      *    ËãÌôÃí¼Í
           03  CONS-MAYAKU           PIC X(09)  VALUE  "130000310".
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
      *
      *
           COPY    "MCPAREA".
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
      *    ´µ¼Ô¥³¥á¥ó¥È
       01  PTCOM-REC.
       COPY    "CPPTCOM.INC".
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY    "CPSYSKANRI.INC".
      *    °åÎÅµ¡´Ø¾ðÊó¡Ý´ðËÜ
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK4003.INC".
      *
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑÎÎ°è
      *****************************************************************
      *
      *   ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    ´µ¼ÔÈÖ¹æÊÑ´¹¥µ¥Ö
           COPY    "CPORCSPTID.INC".
      *    ¿ôÃÍÊÑ´¹¥µ¥Ö
           COPY    "CPORCSNUM.INC".
      *    ¥Á¥§¥Ã¥¯¥Ç¥£¥¸¥Ã¥È¡Ê¥â¥¸¥å¥é¥¹£±£±ÍÑ¡Ë»»½Ð¥µ¥Ö
           COPY    "CPORCCHKDGT11.INC".
      *   ¥¸¥ç¥Ö´ÉÍý£Ä£ÂÀ©¸æ¥µ¥Ö
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
      *     
           COPY    "CPORCMCP.INC".  
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
      *
           MOVE    COMMAND-PARAM       TO  WRK-PARA
      *
           MOVE    "RECE410"           TO  RECE41PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE41PARA-TERMID
      *
           MOVE    "RECE440"           TO  RECE44PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE44PARA-TERMID
           MOVE    "RECE450"           TO  RECE45PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE45PARA-TERMID
      *
           MOVE    "RECE441"           TO  RECE4APARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE4APARA-TERMID
           MOVE    "RECE451"           TO  RECE4BPARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE4BPARA-TERMID
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECEERR-TERMID
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ¥¹¥Æ¥Ã¥×´ÉÍý³«»Ï½èÍý
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "ORCR0420"      TO  JOB-PGID
           MOVE    "¿ÇÎÅ¶èÊ¬ÊÌÅÀ¿ô½¸·×¡ÊÏ«ºÒ¡¦¼«ÇåÀÕ¡Ë"                 
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *    ½èÍýÃæ»ßÀßÄê½èÍý
           PERFORM 500-CANCEL-HENSYU-SEC
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       °åÎÅµ¡´Ø¾ðÊó¡Ý´ðËÜ
              INITIALIZE                      SYS-1001-REC
              MOVE    "1001"              TO  SYS-1001-KANRICD
              MOVE    "*"                 TO  SYS-1001-KBNCD
              MOVE    WRK-PARA-SRYYM      TO  ORC-DBYMD (1:6)
              MOVE    "01"                TO  ORC-DBYMD (7:2)
              MOVE    SYS-1001-REC        TO  MCPDATA-REC
              MOVE    "SYSKANRI-KEY"      TO  WRK-PATH
              PERFORM 800-SYSKANRI-READ-SEC
              IF      FLG-SYSKANRI        =   ZERO
                  MOVE    SYSKANRI-REC    TO  SYS-1001-REC
                  MOVE    SYS-1001-TAX-HASU-KBN
                                          TO  WRK-TAX1001-KBN
              ELSE
                  MOVE    "°åÎÅµ¡´Ø¾ðÊó¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£"
                                              TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1                   TO  FLG-END
              END-IF
           ELSE
              MOVE     ZERO               TO  SYSKAN-IDX
              MOVE     SPACE              TO  TBL-SYS1001-T
              INITIALIZE                      TBL-SYS1001-T
      *
              INITIALIZE                      SYS-1001-REC
              MOVE    "1001"              TO  SYS-1001-KANRICD
              MOVE    "*"                 TO  SYS-1001-KBNCD
              MOVE    SYS-1001-REC        TO  MCPDATA-REC
              MOVE    "DBSELECT"          TO  MCP-FUNC
              MOVE    "SYSKANRI-KEY8"     TO  ORC-DBPATH
              CALL    "ORCMCPSUB"             USING
                                              MCPAREA
                                              ORCMCPAREA
                                              MCPDATA-REC
              IF      MCP-RC              =   ZERO
                  MOVE    "SYSKANRI-KEY8" TO  ORC-DBPATH
                  PERFORM 810-SYSKANRI-READ-SEC
                  PERFORM  UNTIL   FLG-SYSKANRI  =    1
      *
                    MOVE    SYSKANRI-REC  TO  SYS-1001-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1001-REC  TO  TBL-SYS1001(SYSKAN-IDX)
      *
                    MOVE    "SYSKANRI-KEY8" TO  ORC-DBPATH
                    PERFORM 810-SYSKANRI-READ-SEC
      *
                  END-PERFORM
              ELSE
                  MOVE    "°åÎÅµ¡´Ø¾ðÊó¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£"
                                          TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1               TO  FLG-END
              END-IF
      *
              MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
              MOVE    "SYSKANRI-KEY8"     TO  ORC-DBPATH
              CALL    "ORCMCPSUB"          USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           END-IF
      *                             
      *    ¥ì¥»¥×¥È½èÍý
           OPEN    OUTPUT              RECE444-FILE
           CLOSE                       RECE444-FILE
      *
           OPEN    OUTPUT              RECE455-FILE
           CLOSE                       RECE455-FILE
      *
           OPEN    OUTPUT              RECE4A4-FILE
           CLOSE                       RECE4A4-FILE
      *
           OPEN    OUTPUT              RECE4B5-FILE
           CLOSE                       RECE4B5-FILE
      *
           OPEN    OUTPUT              RECE44-FILE
      ***                              RECE45-FILE
           OPEN    I-O                 RECE45-FILE
      *
           OPEN    OUTPUT              RECE4A-FILE
                                       RECE4B-FILE
      
      *
           OPEN    I-O                 RECE41-FILE
      *
           PERFORM     900-RECE41-READ-SEC
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
           PERFORM     210-HENSYU-SEC
      *      
           PERFORM     900-RECE41-READ-SEC
           IF      CNT-IN-COMMIT   =   300
               MOVE  ZERO          TO      CNT-IN-COMMIT
               PERFORM 900-DBCOMMIT-SEC
               PERFORM 900-DBSTART-SEC
           END-IF
      *
      *    ½èÍýÃæ»ßÀßÄê½èÍý
           IF      CNT-IN-CANCEL       =   50
               MOVE    ZERO                TO  CNT-IN-CANCEL
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ÊÔ½¸½èÍý
      *****************************************************************
       210-HENSYU-SEC                   SECTION.
      *
           IF      RECE41-HKNKBN   NOT  =   "3"
      *
              INITIALIZE                   FLG-KHNTEN-AREA
              INITIALIZE                   FLG-KHNTEN-JI-AREA           
      *
              PERFORM    VARYING    IDX    FROM   1   BY   1
                         UNTIL     (IDX      >   20)  OR
                                   (RECE41-SRYKA(IDX)   =   SPACE)
                  PERFORM     2101-RECE44-HENSYU-SEC
              END-PERFORM
      *
              IF      RECE41-HKNKBN   NOT  =   "4"                      
                  PERFORM     2102-KHNTEN-CLEAR-SEC
              ELSE                                                      
                  PERFORM     2103-KHNTEN-JI-CLEAR-SEC                       
              END-IF                                                    
      *
           ELSE
      *
              PERFORM    VARYING    IDX    FROM   1   BY   1
                         UNTIL     (IDX      >   20)  OR
                                   (RECE41-SRYKA(IDX)   =   SPACE)
                  PERFORM     2104-RECE44-HENSYU2-SEC
              END-PERFORM
      *
           END-IF
      *
           PERFORM     2105-RECE41-REWRITE-SEC
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®
      *****************************************************************
       2101-RECE44-HENSYU-SEC                SECTION.
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE41-HOSPID        TO      ACCT-HOSPID
           MOVE    RECE41-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE41-PTID          TO      ACCT-PTID
           MOVE    RECE41-SRYKA(IDX)    TO      ACCT-SRYKA
           MOVE    RECE41-SRYYM         TO      ACCT-SRYYM
           MOVE    RECE41-HKNCOMBI      TO      ACCT-HKNCOMBI
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "SRYACCT-KEY24"      TO      ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "SRYACCT-KEY24"  TO      ORC-DBPATH
               PERFORM  800-SRYACCT-READ-SEC
               PERFORM  UNTIL     FLG-SRYACCT   =   1 
      *
                   MOVE   ZERO          TO  FLG-RINYAK
      *
                   IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                        (ACCT-ZAIKAISU  NOT =    ZERO)
                               MOVE    ZERO    TO      WRK-TEN
                                                       WRK-MEISAI-CNT
                                                       WRK-SRYCDTOTAL
                                                       WRK-SURYOUTOTAL
                                                       WRK-YOUBUKASAN
                                                       WRK-KYOBUKASAN
                                                       WRK-SOGAIKASAN
                                                       WRK-RYOYOTEATE
      *
                               MOVE    ZERO    TO      WRK-SRYSURYO
                                                       WRK-SRYSURYO2
                                                       WRK-SRYSURYO3
                                                       WRK-TANICD
                                                       WRK-TANIMOJISU
                               MOVE    SPACE   TO      WRK-TANINAME
                               MOVE    ZERO    TO      FLG-HOUSIJI-COM
                                                       FLG-HOUTEN-COM
      *
                               MOVE    SPACE   TO      WRK-KYUGYOFLG
      *
                               PERFORM   21011-RECE44-TENSOU-SEC
                               PERFORM   21012-RECE45-HENSYU-SEC
                               PERFORM   21013-RECE44-WRITE-SEC
                   ELSE
                             IF   ACCT-ZAIKAISU  NOT =    ZERO
                               IF   ACCT-SRYKBN  =  "11"  OR  "12" 
                                                          OR  "99" 
                                 IF  ACCT-SRYCDTOTAL = CONS-TOKKI1  OR
                                                       CONS-TOKKI2  OR
                                                       CONS-TOKKI3  OR
                                                       CONS-TOKKI4  OR
                                                       CONS-TOKKI5  OR
                                                       CONS-TOKKI7  OR
                                                       CONS-TOKKI8  OR
                                                       CONS-TOKKI9  OR
                                                       CONS-TOKKI10 OR
                                                       CONS-TOKKI11 OR
                                                       CONS-TOKKI12 OR
                                                       CONS-TOKKI13 OR
                                                       CONS-TOKKI14
                                      CONTINUE
                                 ELSE
                                      MOVE  ZERO  TO  WRK-TEN
                                                      WRK-MEISAI-CNT
                                                      WRK-SRYCDTOTAL
                                                      WRK-SURYOUTOTAL
                                                      WRK-YOUBUKASAN
                                                      WRK-KYOBUKASAN
                                                      WRK-SOGAIKASAN
                                                      WRK-RYOYOTEATE
      *
                                      MOVE  ZERO  TO  WRK-SRYSURYO
                                                      WRK-SRYSURYO2
                                                      WRK-SRYSURYO3
                                                      WRK-TANICD
                                                      WRK-TANIMOJISU
                                      MOVE  SPACE TO  WRK-TANINAME
                                      MOVE  ZERO  TO  FLG-HOUSIJI-COM
                                                      FLG-HOUTEN-COM
      *
                                      MOVE  SPACE TO  WRK-KYUGYOFLG
      *
                                      PERFORM   21011-RECE44-TENSOU-SEC
                                      PERFORM   21012-RECE45-HENSYU-SEC
                                      PERFORM   21013-RECE44-WRITE-SEC
                                 END-IF
                               ELSE
                                 PERFORM   21014-SINRYO-CHECK-SEC
                                 IF   FLG-RINYAK  NOT =  ZERO
                                      MOVE  ZERO  TO  WRK-TEN
                                                      WRK-MEISAI-CNT
                                                      WRK-SRYCDTOTAL
                                                      WRK-SURYOUTOTAL
                                                      WRK-YOUBUKASAN
                                                      WRK-KYOBUKASAN
                                                      WRK-SOGAIKASAN
                                                      WRK-RYOYOTEATE
      *
                                      MOVE  ZERO  TO  WRK-SRYSURYO
                                                      WRK-SRYSURYO2
                                                      WRK-SRYSURYO3
                                                      WRK-TANICD
                                                      WRK-TANIMOJISU
                                      MOVE  SPACE TO  WRK-TANINAME
                                      MOVE  ZERO  TO  FLG-HOUSIJI-COM
                                                      FLG-HOUTEN-COM
      *
                                      MOVE  SPACE TO  WRK-KYUGYOFLG
      *
                                      PERFORM   21011-RECE44-TENSOU-SEC
                                      PERFORM   21012-RECE45-HENSYU-SEC
                                      PERFORM   21013-RECE44-WRITE-SEC
                                 END-IF
                               END-IF
                             END-IF
                   END-IF
      *
                   MOVE     "SRYACCT-KEY24"  TO   ORC-DBPATH
                   PERFORM   800-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE   "SRYACCT-KEY24"     TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
           .
       2101-RECE44-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷
      *****************************************************************
       21011-RECE44-TENSOU-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE44-REC
           INITIALIZE                           RECE44-REC
           MOVE    RECE41-HOSPID        TO      RECE44-HOSPID
           MOVE    RECE41-SRYYM         TO      RECE44-SRYYM
           MOVE    RECE41-NYUGAIKBN     TO      RECE44-NYUGAIKBN
           MOVE    RECE41-PTID          TO      RECE44-PTID
           MOVE    RECE41-RECEKA        TO      RECE44-RECEKA
           MOVE    RECE41-HKNID         TO      RECE44-HKNID
           MOVE    RECE41-TEKSTYMD      TO      RECE44-TEKSTYMD
           MOVE    RECE41-PTNUM         TO      RECE44-PTNUM 
           MOVE    ACCT-ZAINUM          TO      RECE44-ZAINUM
           MOVE    ACCT-SRYKBN          TO      RECE44-SRYKBN
           MOVE    ACCT-SRYKA           TO      RECE44-SRYKA
           MOVE    ACCT-HKNCOMBI        TO      RECE44-HKNCOMBI
           MOVE    ACCT-JIHOKBN         TO      RECE44-JIHOKBN
           MOVE    ACCT-ZAITEN          TO      RECE44-ZAITEN
           MOVE    ACCT-SRYCDTOTAL      TO      RECE44-SRYCDTOTAL
           MOVE    ACCT-SURYOUTOTAL     TO      RECE44-SURYOUTOTAL
           MOVE    ACCT-MEISAISU        TO      RECE44-MEISAISU
           IF      RECE41-HKNKBN   NOT  =   "3"
              MOVE    ACCT-ZAIKAISU     TO      RECE44-ZAIKAISU
           ELSE
              MOVE    WRK-ZAIKAISU-XXX  TO      RECE44-ZAIKAISU
           END-IF
           MOVE    ACCT-SYUTEN1         TO      RECE44-SYUTEN1
           MOVE    ACCT-SYUTEN2         TO      RECE44-SYUTEN2
           MOVE    ACCT-YKZTEN          TO      RECE44-YKZTEN
           MOVE    ACCT-KIZAITEN        TO      RECE44-KIZAITEN
           IF   RECE41-HKNKBN     =  "4"
              IF  ACCT-SRYKBN     =  "70"
                  COMPUTE  RECE44-SYUTEN1   =  RECE44-SYUTEN1
                                            -  RECE44-SYUTEN2
                  COMPUTE  RECE44-KIZAITEN  =  RECE44-KIZAITEN
                                            +  RECE44-SYUTEN2
              END-IF
           END-IF
           MOVE    ACCT-DRCD            TO      RECE44-DRCD
           MOVE    ACCT-ZAIREQFLG       TO      RECE44-ZAIREQFLG
           MOVE    ACCT-TEIGENRATE      TO      RECE44-TEIGENRATE
      *
           IF      RECE41-HKNKBN   NOT  =   "3"
               MOVE        ZERO             TO      WK-TAIHI-DAY
               MOVE        ZERO             TO      WK-TAIHI-FLG
               PERFORM     VARYING   IDX3   FROM    1    BY   1
                           UNTIL     IDX3   >       4
                   PERFORM     VARYING   IDX2   FROM    1    BY   1
                               UNTIL     IDX2   >       31
                       MOVE    ACCT-DAY(IDX3 IDX2)  TO
                                            RECE44-KAISU (IDX3 IDX2)
                       IF     (WK-TAIHI-FLG            =   ZERO)   AND
                              (ACCT-DAY(1 IDX2)    NOT =   ZERO)
                           MOVE     IDX2    TO      WK-TAIHI-DAY
                           MOVE     1       TO      WK-TAIHI-FLG
                       END-IF    
                   END-PERFORM
               END-PERFORM
           ELSE
               PERFORM     VARYING   IDX3   FROM    1    BY   1
                           UNTIL     IDX3   >       4
                  MOVE    ACCT-DAY(IDX3 WRK-DAY)  
                                       TO    RECE44-KAISU(IDX3 WRK-DAY)
              END-PERFORM                         
              MOVE    WRK-DAY          TO    WK-TAIHI-DAY
           END-IF
      *
           IF    FLG-RINYAK   NOT =    ZERO
                MOVE    FLG-RINYAK     TO    RECE44-SAFLG
           END-IF
      *
           .
       21011-RECE44-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®
      *****************************************************************
       21012-RECE45-HENSYU-SEC               SECTION.
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPID          TO      SRY-HOSPID
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
           MOVE    ACCT-SRYKA           TO      SRY-SRYKA
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "SRYACT-KEY2"        TO      ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "SRYACT-KEY2"    TO      ORC-DBPATH
               PERFORM  800-SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
                   MOVE   ZERO   TO  FLG-ZANHAIKI
                   MOVE   ZERO   TO  WRK-RENNUM
                   PERFORM   210121-RECE45-TENSOU-SEC
                   PERFORM   210122-RECE41-HENSYU-SEC
                   PERFORM   210123-RECE45-WRITE-SEC
                   IF  FLG-ZANHAIKI  =  1
                      PERFORM  210124-RECE45-REWRITE-SEC
                   END-IF
                   MOVE     "SRYACT-KEY2"    TO   ORC-DBPATH
                   PERFORM   800-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE   "SRYACT-KEY2"      TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
           .
       21012-RECE45-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷
      *****************************************************************
       210121-RECE45-TENSOU-SEC                SECTION.
      *
           INITIALIZE                   WRK-TNS-TAIHI-AREA
      *
           MOVE    SPACE                TO      RECE45-REC
           INITIALIZE                           RECE45-REC
           MOVE    RECE41-HOSPID        TO      RECE45-HOSPID
           MOVE    RECE41-SRYYM         TO      RECE45-SRYYM
           MOVE    RECE41-NYUGAIKBN     TO      RECE45-NYUGAIKBN
           MOVE    RECE41-PTID          TO      RECE45-PTID
           MOVE    RECE41-RECEKA        TO      RECE45-RECEKA
           MOVE    RECE41-HKNID         TO      RECE45-HKNID
           MOVE    RECE41-TEKSTYMD      TO      RECE45-TEKSTYMD
           MOVE    RECE41-PTNUM         TO      RECE45-PTNUM
           MOVE    SRY-ZAINUM           TO      RECE45-ZAINUM
           MOVE    SRY-RENNUM           TO      RECE45-RENNUM
           MOVE    SRY-SRYKA            TO      RECE45-SRYKA
           MOVE    SRY-SRYKBN           TO      RECE45-SRYKBN
           MOVE    SRY-SRYSYUKBN        TO      RECE45-SRYSYUKBN
           MOVE    SRY-JIHIMONEYTOTAL   TO      RECE45-JIHIMONEY
      * 
           MOVE    ZERO                 TO      FLG-OSINKBN
                                                IDX4
      *
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                       UNTIL    (IDX2      >    5)    OR
                                (SRY-SRYCD(IDX2)  =   SPACE)
      *
      *
             PERFORM    800-TENSU-TBL-SEC
      **     ÅÀ¿ô¥Þ¥¹¥¿¾ðÊóÂàÈò
             MOVE    TNS-DATAKBN        TO  WRK-TNS-DATAKBN(IDX2)
             MOVE    TNS-TIMEKSNKBN     TO  WRK-TNS-TIMEKSNKBN(IDX2)
      *
             IF      RECE41-HKNKBN        =  "4"
             AND     ACCT-SRYKBN          =  "80"
             AND     SRY-SRYCD(IDX2)(1:5) =  "09593"
                 PERFORM   2101213-JIBAI-KIZAI-TENSU-HENSYU-SEC
                 MOVE   WRK-TEN-KZI     TO  WRK-TNS-TEN(IDX2)
             ELSE
                MOVE    TNS-TEN         TO  WRK-TNS-TEN(IDX2)
             END-IF
      *
             MOVE    TNS-GAITENTTLKBN   TO  WRK-TNS-GAITENTTLKBN(IDX2)
             MOVE    TNS-OSINKBN        TO  WRK-TNS-OSINKBN(IDX2)
             MOVE    TNS-KNSJISKBN      TO  WRK-TNS-KNSJISKBN(IDX2)
             MOVE    TNS-SRYCD          TO  WRK-TNS-SRYCD(IDX2)
      *
             IF     ( SRY-SRYCD(IDX2)(1:1)  =  "S"   )
                  COMPUTE   RECE44-MEISAISU  =  RECE44-MEISAISU
                                             -  1
             END-IF
      *
             IF     ( SRY-SRYCD(IDX2)  =  CONS-TOKKI1(6:9)
                                      OR  CONS-TOKKI2(6:9)
                                      OR  CONS-TOKKI3(6:9)
                                      OR  CONS-TOKKI4(6:9)
                                      OR  CONS-TOKKI5(6:9)
                                      OR  CONS-TOKKI7(6:9)
                                      OR  CONS-TOKKI8(6:9)
                                      OR  CONS-TOKKI9(6:9)
                                      OR  CONS-TOKKI10(6:9)
                                      OR  CONS-TOKKI11(6:9)
                                      OR  CONS-TOKKI12(6:9)
                                      OR  CONS-TOKKI13(6:9)
                                      OR  CONS-TOKKI14(6:9) )
             OR     ( SRY-SRYCD(IDX2)(1:3)  =  "001"        )
             OR     ((SRY-SRYCD(IDX2)(1:3)  =  "059") AND
                     (TNS-TEN               =  ZERO )       )
                  COMPUTE   RECE44-MEISAISU    =  RECE44-MEISAISU
                                               -  1
                  MOVE      SRY-SRYCD(IDX2)   TO  WRK-SRYCD
                  COMPUTE   RECE44-SRYCDTOTAL  =  RECE44-SRYCDTOTAL
                                               -  WRK-SRYCD
                  COMPUTE   RECE44-SURYOUTOTAL =  RECE44-SURYOUTOTAL
                                               -  1
             END-IF
      *
             IF      SRY-SRYCD(IDX2)(1:1)     =   "0" OR  "1" OR
                                                  "6" OR  "7" OR
                                                  "8"
      *
              IF     ( SRY-SRYCD(IDX2)  =  CONS-TOKKI1(6:9)
                                       OR  CONS-TOKKI2(6:9)
                                       OR  CONS-TOKKI3(6:9)
                                       OR  CONS-TOKKI4(6:9)
                                       OR  CONS-TOKKI5(6:9)
                                       OR  CONS-TOKKI7(6:9)
                                       OR  CONS-TOKKI8(6:9)
                                       OR  CONS-TOKKI9(6:9)
                                       OR  CONS-TOKKI10(6:9)
                                       OR  CONS-TOKKI11(6:9)
                                       OR  CONS-TOKKI12(6:9)
                                       OR  CONS-TOKKI13(6:9)
                                       OR  CONS-TOKKI14(6:9) )
              OR     ( SRY-SRYCD(IDX2)(1:3)  =  "001"        )
              OR     ((SRY-SRYCD(IDX2)(1:3)  =  "059") AND
                      (TNS-TEN               =  ZERO )       )
      *
               CONTINUE
      *
              ELSE
      *
               ADD     1                  TO    IDX4
               MOVE    SRY-SRYCD(IDX2)    TO    RECE45-SRYCD(IDX4)
      *
               IF      SRY-SRYCD(IDX2)    =     CONS-ROUSAI10
                        MOVE   "1"        TO    RECE41-KYUGYOFLG
                                                WRK-KYUGYOFLG
               END-IF
      *
               IF      SRY-SRYKBN         =     "12"
                   IF   SRY-SRYCD(IDX2)   =     CONS-KEIZOKU1    OR
                                                CONS-KEIZOKU2
                        MOVE   "1"      TO    RECE41-KEIZOKU-PRTKBN
                   END-IF
                   IF   SRY-SRYCD(IDX2)   =     CONS-GAIRAIRYO1  OR
                                                CONS-GAIRAIRYO2
                        MOVE   "2"      TO    RECE41-KEIZOKU-PRTKBN
                   END-IF
                   IF   SRY-SRYCD(IDX2)   =     CONS-SAISHIN-A1   
      **                                  OR    CONS-SAISHIN-A2   
      **                                  OR    CONS-SAISHIN-A3   
      **                                  OR    CONS-SAISHIN-A4   
      **                                  OR    CONS-SAISHIN-A5   
      **                                  OR    CONS-SAISHIN-A6   
      **                                  OR    CONS-SAISHIN-A7   
      **                                  OR    CONS-SAISHIN-A8   
      **                                  OR    CONS-SAISHIN-A9   
      **                                  OR    CONS-SAISHIN-A10  
      **                                  OR    CONS-SAISHIN-A11  
      **                                  OR    CONS-SAISHIN-A12  
      **                                  OR    CONS-SAISHIN-A13  
                        MOVE   "1"      TO    RECE41-SAISHIN-PRTKBN2
                   END-IF
                   IF   SRY-SRYCD(IDX2)   =     CONS-GAIRAI 
                        MOVE   "1"      TO    RECE41-GAIRAIKSN-PRTKBN
                   END-IF
               END-IF
      *
               IF     SRY-SRYCD(IDX2)  IS  NUMERIC
                 IF  SRY-SRYCD(IDX2)(1:3) NOT = "001"
                   MOVE    SRY-SRYCD(IDX2)  TO  WRK-SRYCD
                   COMPUTE  RECE44-RUIKEITOTAL  =
                            RECE44-RUIKEITOTAL  +
                           (WRK-SRYCD  *  SRY-SRYSURYO(IDX2))
                 END-IF
               END-IF
      *
               MOVE    SRY-SRYSURYO(IDX2) TO    RECE45-SRYSURYO(IDX4)
               MOVE    SRY-SRYKAISU(IDX2) TO    RECE45-SRYKAISU(IDX4)
               MOVE    SRY-MEISKYFLG(IDX2) TO   RECE45-MEISKYFLG(IDX4)
               MOVE    SRY-AUTOKBN(IDX2)  TO    RECE45-AUTOKBN(IDX4)
               MOVE    SRY-INPUTNUM(IDX2) TO    RECE45-INPUTNUM(IDX4)
      **
               IF   RECE41-HKNKBN    NOT  =  "4"                        
                  IF      TNS-SRYKBN         =     "80"
                      IF  TNS-GAITENTTLKBN   =     130
                          MOVE   "13"        TO    RECE44-SRYKBN
                                                   RECE45-SRYKBN
                      END-IF
                  END-IF
               ELSE                                                     
                  IF      SRY-SRYCD(IDX2)    =     CONS-YOUBUKASAN
                   IF   RECE41-KOTEIKBN  =  SPACE  OR  "2"
                    COMPUTE  WRK-YOUBUKASAN   =  WRK-YOUBUKASAN
                                              +  TNS-TEN
                    COMPUTE  RECE44-SYUTEN1   =  RECE44-SYUTEN1
                                              -  TNS-TEN
                    COMPUTE  RECE44-KIZAITEN  =  RECE44-KIZAITEN
                                              +  TNS-TEN
                   END-IF
                  END-IF
                  IF      SRY-SRYCD(IDX2)    =     CONS-KYOBUKASAN
                   IF   RECE41-KOTEIKBN  =  SPACE  OR  "2"
                    COMPUTE  WRK-KYOBUKASAN   =  WRK-KYOBUKASAN
                                              +  TNS-TEN
                    COMPUTE  RECE44-SYUTEN1   =  RECE44-SYUTEN1
                                              -  TNS-TEN
                    COMPUTE  RECE44-KIZAITEN  =  RECE44-KIZAITEN
                                              +  TNS-TEN
                   END-IF
                  END-IF
                  IF      SRY-SRYCD(IDX2)    =     CONS-SOGAIKASAN
                   IF   RECE41-KOTEIKBN  =  SPACE  OR  "2"
                    COMPUTE  WRK-SOGAIKASAN   =  WRK-SOGAIKASAN
                                              +  TNS-TEN
                    COMPUTE  RECE44-SYUTEN1   =  RECE44-SYUTEN1
                                              -  TNS-TEN
                    COMPUTE  RECE44-KIZAITEN  =  RECE44-KIZAITEN
                                              +  TNS-TEN
                   END-IF
                  END-IF
      *           ¼«ÇåÀÕ¡¦ÎÅÍÜÃ´Åö¼êÅöÍÑÆÃÎã½èÍý
      *           ¡Ê¼«ÇåÀÕ¤Î¾ì¹ç¡¢£·ÅÀ¤ò£¸£´±ß¤ËÆÉ¤ßÂØ¤¨¤ë¡Ë
      *           ¡ÊÎÅÍÜÃ´Åö¼êÅö¡§£±·î£±²ó¤Î»»Äê¡Ë
                  IF      SRY-SRYCD(IDX2)    =     CONS-RYOYOTEATE
                    MOVE     84     TO     WRK-RYOYOTEATE2
                    COMPUTE  WRK-RYOYOTEATE   =  WRK-RYOYOTEATE
                                              +  WRK-RYOYOTEATE2
                  END-IF
      *
                  IF ( ACCT-SRYKBN          =  "80"               ) AND
                     ( SRY-SRYCD(IDX2)(1:5) =  "09591" OR "09592" )
                                MOVE   "95"   TO    RECE44-SRYKBN       
                                                    RECE45-SRYKBN       
                  ELSE
                   IF ( ACCT-SRYKBN          =  "80"              ) AND 
                      ( SRY-SRYCD(IDX2)(1:5) =  "09593"           )     
                                MOVE   "40"   TO    RECE44-SRYKBN       
                                                    RECE45-SRYKBN       
                   ELSE                                                 
                     IF      TNS-SRYKBN         =   "80"                
                       IF   SRY-SRYCD(IDX2)   =     CONS-SHOHOSEN1 OR   
                                                    CONS-SHOHOSEN2 OR   
                                                    CONS-SHOHOSEN3 OR   
                                                    CONS-SHOHOSEN4 OR   
                                                    CONS-SHOHOKSN1 OR   
                                                    CONS-SHOHOKSN2 OR   
                                                    CONS-SHOHOKSN3
                              CONTINUE                                  
                       ELSE                                             
                              IF    TNS-GAITENTTLKBN   =  130           
                                  MOVE   "13"   TO  RECE44-SRYKBN       
                                                    RECE45-SRYKBN       
                              ELSE                                      
                                  MOVE   "40"   TO  RECE44-SRYKBN       
                                                    RECE45-SRYKBN       
                              END-IF                                    
                       END-IF                                           
                     END-IF
                   END-IF                                               
                  END-IF                                                
      *
                  IF  ( ACCT-SRYKBN       =  "80"  )
                  AND ( RECE44-SAFLG  NOT =  SPACE )
                            MOVE   "40"   TO  RECE44-SRYKBN
                                              RECE45-SRYKBN
                  END-IF
      *
               END-IF                                                   
      *
               PERFORM  2101211-RECE45-TENSOU-COMCNT-HEN-SEC
      *
               IF  SRY-SRYCD(IDX2)        =     CONS-ZANHAIKI           
                  COMPUTE WRK-SRYSURYO2   =     WRK-SRYSURYO            
                                          +     0.999                   
                  COMPUTE RECE45-SRYSURYO(IDX4)                         
                                          =     WRK-SRYSURYO2           
                                          -     WRK-SRYSURYO            
                  IF  RECE45-SRYSURYO(IDX4)  NOT = ZERO                 
                    MOVE  WRK-TANICD      TO    RECE45-TANICD(IDX4)     
                    MOVE  WRK-TANIMOJISU  TO    RECE45-TANIMOJISU(IDX4) 
                    MOVE  WRK-TANINAME    TO    RECE45-TANINAME(IDX4)   
                    IF    RECE45-RENNUM  >  1                           
                    AND   IDX4           =  1                           
                      MOVE    1           TO    FLG-ZANHAIKI            
                      COMPUTE  WRK-RENNUM  =  RECE45-RENNUM  -  1       
                      MOVE WRK-SRYSURYO2  TO  WRK-SRYSURYO3             
                    ELSE                                                
                      MOVE WRK-SRYSURYO2  TO  RECE45-SRYSURYO(IDX4 - 1) 
                    END-IF
                  END-IF                                                
               ELSE                                                     
                  IF      TNS-TANIMOJISU    =   ZERO
                  AND     TNS-TANINAME  NOT =   SPACE
                     PERFORM   VARYING   IDW   FROM   24   BY  -1
                                UNTIL    IDW    <      1
                        IF   TNS-TANINAME(IDW:1)  NOT =   SPACE
                            COMPUTE  TNS-TANIMOJISU   =   IDW   /   2
                            MOVE    1     TO    IDW
                        END-IF
                     END-PERFORM
                  END-IF
                  MOVE    SRY-SRYSURYO(IDX2)                            
                                          TO    WRK-SRYSURYO            
                  MOVE    TNS-TANICD      TO    RECE45-TANICD(IDX4)
                                                WRK-TANICD              
                  MOVE    TNS-TANIMOJISU  TO    RECE45-TANIMOJISU(IDX4)
                                                WRK-TANIMOJISU          
                  MOVE    TNS-TANINAME    TO    RECE45-TANINAME(IDX4)
                                                WRK-TANINAME            
               END-IF                                                   
               IF ( ACCT-SRYKBN          =  "80"              ) AND     
                  ( SRY-SRYCD(IDX2)(1:5) =  "09593"           )         
                    MOVE  1               TO  RECE45-TENSIKIBETU(IDX4)  
      ***>          MOVE  TNS-TEN         TO  RECE45-TEN(IDX4)          
                    MOVE  WRK-TNS-TEN(IDX2)  TO  RECE45-TEN(IDX4)
               ELSE                                                     
                 IF   RECE41-HKNKBN     =  "4"
                 AND  SRY-SRYCD(IDX2)   =  CONS-RYOYOTEATE
                    MOVE  1               TO  RECE45-TENSIKIBETU(IDX4)  
                    MOVE  WRK-RYOYOTEATE2 TO  RECE45-TEN(IDX4)
                 ELSE
                    MOVE  TNS-TENSIKIBETU TO  RECE45-TENSIKIBETU(IDX4)
                    MOVE  TNS-TEN         TO  RECE45-TEN(IDX4)
                 END-IF
               END-IF                                                   
               IF      RECE41-NYUGAIKBN   =     "1"
                   MOVE  TNS-NYUTENTTLKBN TO    RECE45-TENTTLKBN(IDX4)
               ELSE
                   MOVE  TNS-GAITENTTLKBN TO    RECE45-TENTTLKBN(IDX4)
               END-IF
               IF     (SRY-SRYCD    (IDX2)(1:1)     =   "8"  )
               OR     (SRY-SRYCD    (IDX2)(1:3)     =   "008")
                  MOVE ZERO               TO    RECE45-YUKOKETA(IDX4)
               ELSE
                  MOVE TNS-YUKOKETA       TO    RECE45-YUKOKETA(IDX4)
               END-IF
               MOVE    TNS-DATAKBN        TO    RECE45-DATAKBN (IDX4)
               MOVE    TNS-HOUKNSKBN      TO    RECE45-HOUKNSKBN(IDX4)
               MOVE    TNS-KNSJISKBN      TO    RECE45-KNSJISKBN(IDX4)
               MOVE    TNS-CDKBN-KBN      TO    RECE45-CDKBN-KBN(IDX4)
               MOVE    TNS-CDKBN-SYO      TO    RECE45-CDKBN-SYO(IDX4)
               MOVE    TNS-CDKBN-BU       TO    RECE45-CDKBN-BU(IDX4)
               MOVE    TNS-CDKBN-KBNNUM   
                                      TO RECE45-CDKBN-KBNNUM(IDX4)
               MOVE    TNS-CDKBN-KBNNUM-EDA
                                      TO RECE45-CDKBN-KBNNUM-EDA(IDX4)
               MOVE    TNS-CDKBN-KOUBAN
                                      TO RECE45-CDKBN-KOUBAN(IDX4)
               MOVE    TNS-KOUHYOJYUNNUM  
                                      TO RECE45-KOUHYOJYUNNUM(IDX4)
      *
               IF     TNS-SRYCD       =  "170022290"  OR
                                         "170022390"  OR
                                         "170022490"
                      MOVE   202      TO  RECE45-CDKBN-KBNNUM(IDX4)
                      MOVE   99       TO  RECE45-CDKBN-KOUBAN(IDX4)
               END-IF
      *
               IF    ((SRY-SRYCD    (IDX2)(1:1)     =   "8"  )  OR
                      (SRY-SRYCD    (IDX2)(1:3)     =   "008"))
               AND   ( SRY-INPUTNUM (IDX2)      NOT =   ZERO  )
                   MOVE    SRY-HOSPID           TO  PTCOM-HOSPID
                   MOVE    SRY-PTID             TO  PTCOM-PTID
                   MOVE    SRY-ZAINUM           TO  PTCOM-ZAINUM
                   MOVE    SRY-SRYCD (IDX2)     TO  PTCOM-SRYCD
                   MOVE    SRY-INPUTNUM(IDX2)   TO  PTCOM-RENNUM
                   PERFORM 800-PTCOM-READ-SEC
                   MOVE    PTCOM-INPUTCOMENT   TO    RECE45-NAME(IDX4)
      *
      ***          IF    RECE44-COMCDFLG   NOT =   "3"
      ***             MOVE   "1"           TO    RECE44-COMCDFLG
      ***          END-IF
      *
                   IF      WRK-KYUGYOFLG   =   "1"
                   AND     SRY-SRYCD(IDX2) = CONS-ROUCOM1
                        MOVE   PTCOM-INPUTCHI3(1:2)
                                            TO  RECE41-KYUGYO-END(1:2)
                        MOVE   PTCOM-INPUTCHI4(1:2)
                                            TO  RECE41-KYUGYO-END(3:2)
                        MOVE   SPACE        TO  WRK-KYUGYOFLG
                   END-IF
      *
               ELSE
                   MOVE    TNS-NAME            TO    RECE45-NAME(IDX4)
      *
      *            IF   RECE41-HKNKBN   =  "4"  
      *            AND  RECE41-SRYYM   >=  "200302"
      *                 IF   SRY-SRYCD(IDX2)  =  "101120040"  OR
      *                                          "101120050"  OR
      *                                          "101120060"
      *                    MOVE   "£¶"   TO    RECE45-NAME(IDX4)(9:2)
      *                 END-IF
      *                 IF   SRY-SRYCD(IDX2)  =  "101120070"  OR
      *                                          "101120080"  OR
      *                                          "101120090"
      *                    MOVE   "£¶"   TO    RECE45-NAME(IDX4)(31:2)
      *                 END-IF
      *            END-IF
      *
               END-IF
      *
               IF          SRY-SRYKBN       =  "21"
                    IF   SRY-SRYCD(IDX2)  =  "820000047" OR 
                                             "630010002"
                        MOVE     "3"       TO    RECE44-COMCDFLG
                    END-IF
               END-IF
               IF    (SRY-SRYCD    (IDX2)(1:1)     =   "8"  )  OR
                     (SRY-SRYCD    (IDX2)(1:3)     =   "008")
                    IF    RECE44-COMCDFLG   NOT =   "3"
                        MOVE     "1"       TO    RECE44-COMCDFLG
                    END-IF
               END-IF
      *
               IF          SRY-SRYCD(IDX2)  =    CONS-HIKAKIN
                                           OR    CONS-SEIBUTU
                                           OR    CONS-MAYAKU
                   MOVE    SPACE           TO    RECE45-NAME(IDX4)
               END-IF
               IF  ( SRY-SRYSYUKBN         =  "334"       )
               AND ((SRY-SRYCD(IDX2)       =  "810000001") OR
                    (SRY-SRYCD(IDX2)(1:3)  =  "008"      ))
               AND ( FLG-HOUTEN-COM        =   ZERO       )
                    PERFORM  VARYING  IDV  FROM  1  BY  2
                            UNTIL  (IDV            >   200 )
                             OR    (FLG-HOUTEN-COM =     1 )
                      IF   RECE45-NAME(IDX4)(IDV:2)   =   "Ë¬"
                         MOVE   SPACE  TO    RECE44-FLG-HOUTEN-COMMENT
                         MOVE    1     TO    FLG-HOUTEN-COM
                      END-IF
                    END-PERFORM
               END-IF
      *
               IF  RECE45-YUKOKETA(IDX4)  =  ZERO
                   PERFORM   VARYING   IDW   FROM   200   BY  -1
                           UNTIL   IDW   <   1
                       IF   RECE45-NAME(IDX4)(IDW:1)  NOT =   SPACE
                           COMPUTE RECE45-YUKOKETA(IDX4)  =
                                   IDW     /   2
                           MOVE    1          TO  IDW
                       END-IF
                   END-PERFORM
               END-IF
      *
               IF (  SRY-SRYCD(IDX2) = CONS-ROUSAI1  OR
                                       CONS-ROUSAI2  OR
                                       CONS-ROUSAI3  OR
                                       CONS-ROUSAI4  OR
                                       CONS-ROUSAI5  OR
                                       CONS-ROUSAI6  OR
                                       CONS-ROUSAI7  OR
                                       CONS-ROUSAI8  OR
                                       CONS-ROUSAI9  OR
                                       CONS-ROUSAI10 OR
                                       CONS-ROUSAI11 OR
                                       CONS-ROUSAI12 OR
                                       CONS-ROUSAI13 OR
                                       CONS-ROUSAI14 OR
                                       CONS-ROUSAI15 OR
                                       CONS-ROUSAI16 OR
                                       CONS-ROUSAI17   )                
               OR (( ACCT-SRYKBN          =  "80"     )   AND           
                   ( SRY-SRYCD(IDX2)(1:5) =  "09593"  ))                
      *
      *           IF     TNS-TENSIKIBETU   =   1
                     ADD      1          TO  WRK-MEISAI-CNT
                     COMPUTE  WRK-TEN    =   WRK-TEN   +
                                           ( TNS-TEN   /  10 ) 
      *
                     ADD      1          TO  WRK-SURYOUTOTAL
                     MOVE  SRY-SRYCD(IDX2) 
                                         TO  WRK-SRYCD
                     ADD   WRK-SRYCD     TO  WRK-SRYCDTOTAL
      *           END-IF
      *
                  MOVE     "2"     TO    RECE45-SIKFLG(IDX4)
      *
               ELSE
      *
                  IF   RECE41-HKNKBN     =  "4"
                  AND  SRY-SRYCD(IDX2)   =  CONS-RYOYOTEATE
      *
                     ADD      1          TO  WRK-MEISAI-CNT
                     COMPUTE  WRK-TEN    =   WRK-TEN   +
                                             TNS-TEN
      *
                     ADD      1          TO  WRK-SURYOUTOTAL
                     MOVE  SRY-SRYCD(IDX2) 
                                         TO  WRK-SRYCD
                     ADD   WRK-SRYCD     TO  WRK-SRYCDTOTAL
      *
                     MOVE     "2"        TO  RECE45-SIKFLG(IDX4)
      *
                  ELSE
      *
                      IF  ( SRY-SRYCD(IDX2) = CONS-ROUCOM1  OR
                                              CONS-ROUCOM2      )
                      OR  ( SRY-SRYCD(1)    = CONS-ROUSAI2  OR
                                              CONS-ROUSAI3      )
                      OR (( ACCT-SRYKBN     = "11"              ) AND
                          ( SRY-SRYCD(IDX2)(1:1)   =   "8"      ) AND
                          ( SRY-SRYCD(IDX2) NOT = "830000020"   ))
                      OR (( ACCT-SRYKBN     = "11"              ) AND
                          ( SRY-SRYCD(IDX2)(1:3)   =   "008"    ))
                      OR (( ACCT-SRYKBN     = "12"              ) AND
                          ( SRY-SRYCD(IDX2)(1:1)   =   "8"      ) AND
                          ( SRY-SRYCD(IDX2) NOT = "830000021"   ))
                      OR (( ACCT-SRYKBN     = "12"              ) AND
                          ( SRY-SRYCD(IDX2)(1:3)   =   "008"    ))
                      OR (( ACCT-SRYKBN     = "13"              ) AND
                          ( SRY-SRYCD(1)    =  CONS-ROUSAI8  OR
                                               CONS-ROUSAI9  OR
                                               CONS-ROUSAI10 OR
                                               CONS-ROUSAI11 OR
                                               CONS-ROUSAI12 OR
                                               CONS-ROUSAI13 OR
                                               CONS-ROUSAI14 OR
                                               CONS-ROUSAI15 OR
                                               CONS-ROUSAI16 OR
                                               CONS-ROUSAI17    ) AND
                          ( SRY-SRYCD(IDX2)(1:3)   =   "008"    ))
                      OR (( ACCT-SRYKBN     = "13"              ) AND
                          ( SRY-SRYCD(1)    =  CONS-ROUSAI8  OR
                                               CONS-ROUSAI9  OR
                                               CONS-ROUSAI10 OR
                                               CONS-ROUSAI11 OR
                                               CONS-ROUSAI12 OR
                                               CONS-ROUSAI13 OR
                                               CONS-ROUSAI14 OR
                                               CONS-ROUSAI15 OR
                                               CONS-ROUSAI16 OR
                                               CONS-ROUSAI17    ) AND
                          ( SRY-SRYCD(IDX2)(1:1)   =   "8"      ))
      *
                         ADD    1      TO    WRK-MEISAI-CNT             
                         MOVE  "2"     TO    RECE45-SIKFLG(IDX4)
      *
                         ADD    1      TO    WRK-SURYOUTOTAL
                         MOVE  SRY-SRYCD(IDX2) 
                                       TO    WRK-SRYCD
                         ADD   WRK-SRYCD 
                                       TO    WRK-SRYCDTOTAL
                         IF  ( RECE44-COMCDFLG       =   "1"   )
                         AND ((SRY-SRYCD(IDX2)(1:1)  =   "8"  ) OR
                              (SRY-SRYCD(IDX2)(1:3)  =   "008"))
                            MOVE  "2"  TO    RECE44-COMCDFLG
                         END-IF
                      ELSE
                         MOVE  "1"     TO    RECE45-SIKFLG(IDX4)
                      END-IF
      *
                  END-IF
      *
               END-IF
      *
      *        ÆÃÄê´ïºà¡Ê²èÁü¿ÇÃÇ¤Î¥Õ¥£¥ë¥à°Ê³°¡Ë¤Î¤È¤­Ã±²Á¤òÊÔ½¸
               IF      SRY-SRYCD (IDX2) (1:1)  =   "7"
                   IF   (( SRY-SRYKBN               =   "70" )  AND
                         ( TNS-DATAKBN              =   "3"  ))
                   OR   (  SRY-SRYCD (IDX2)         =
                                    CONS-FILM-YOUJI-KASAN  OR  
                                    CONS-SANSO             OR
                                    CONS-SANSO2               )
                       CONTINUE
                   ELSE
      **               SET     TBL-SANSO           TO  1
      **               SEARCH      TBL-SANSO-OCC   VARYING   TBL-SANSO
      **                   AT  END
      **                       MOVE    ZERO            TO  FLG-SANSO-OK
      **                   WHEN    SRY-SRYCD (IDX2)    =   TBL-SANSO-SRYCD
      **                                                        (TBL-SANSO)
      **                       MOVE    1               TO  FLG-SANSO-OK
      **               END-SEARCH
      **               IF      FLG-SANSO-OK        =   "1"
                     IF      TNS-TEN         =   ZERO
                       CONTINUE
                     ELSE
                       PERFORM 2101212-RECE45-TENSOU-TANKA-HEN-SEC
                       COMPUTE IDW =   RECE45-YUKOKETA(IDX4)  *  2 
                                                   +   3                    
                       MOVE    WRK-CHAR-N (1:WRK-CHAR-N-CNT)
                                                   TO
                                               RECE45-NAME(IDX4)(IDW:)   
                       COMPUTE RECE45-YUKOKETA(IDX4)
                                               =   RECE45-YUKOKETA(IDX4)
                                                   +   1   
                                                   +
                                                  (WRK-CHAR-N-CNT  /  2)
                     END-IF
                   END-IF
               END-IF        
      *       
      *----------------------------------------------------------------
      *        ºßÂðÉôÊ¬¤Î¥ì¥»¥×¥ÈÊÔ½¸ÍÑ¤Ë±ý¿Ç¡ÊÆÃÊÌ±ý¿Ç¡Ë¡¦Ìë´Ö¡¦
      *        ¿¼Ìë¡¦¶ÛµÞ¤ÎºÞÈ½Äê¤òÄÉ²Ã
      *----------------------------------------------------------------
               IF    SRY-SRYKBN     =  "14"
                 EVALUATE    SRY-SRYCD(IDX2)
      *±ý¿Ç¡ÊÆÃÊÌ±ý¿Ç¡Ë
                   WHEN    "114000110"
                   WHEN    "114001610"
                   WHEN    "114000970"
                   IF  FLG-OSINKBN        NOT = 2  AND 3
                       MOVE     1         TO    FLG-OSINKBN
                   END-IF
      *Ìë´Ö
                   WHEN    "114000470"
                   WHEN    "114001970"
                       MOVE     2         TO    FLG-OSINKBN
      *¿¼Ìë¡¦¶ÛµÞ
                   WHEN    "114000370"
                   WHEN    "114000570"
                   WHEN    "114001870"
                   WHEN    "114002070"
                       MOVE     3         TO    FLG-OSINKBN
                 END-EVALUATE
               END-IF
              END-IF
             END-IF  
           END-PERFORM
      *
           .
       210121-RECE45-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È¥³¥á¥ó¥È¥Õ¥é¥°ÊÔ½¸½èÍý
      *****************************************************************
       2101211-RECE45-TENSOU-COMCNT-HEN-SEC               SECTION.
      *
           IF      RECE44-SRYKBN      =     "11"
      *       ½é¿ÇÎÁ»»Äê²Ê¡§
              IF   SRY-SRYCD(IDX2)    =     "830000020"
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-SYOSANTEIKA-COMMENT
              END-IF
           END-IF
      *
           IF      RECE44-SRYKBN      =     "12"
      *       ºÆ¿ÇÎÁ»»Äê²Ê¡§
              IF   SRY-SRYCD(IDX2)    =     "830000021"
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-SAISANTEIKA-COMMENT
              END-IF
      *       Æ±ÆüºÆ¿Ç
              IF   SRY-SRYCD(IDX2)    =     "112011710"   OR
                                            "112705850"   
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-DOUJITUSAI-COMMENT
              END-IF
           END-IF
      *
           IF     (RECE41-HKNKBN    NOT  =  "4"    AND
                   RECE44-SRYKBN         =  "80"       )
           OR     (RECE41-HKNKBN         =  "4"    AND
                   RECE44-SRYKBN         =  "40"       )
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
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-RIHABIRI-COMMENT
              END-IF
           END-IF
      *
      *
           IF      RECE44-SRYKBN      =     "13"
              IF   SRY-SRYCD(IDX2)    =     CONS-MANSEI
                   MOVE   "1"   TO    RECE44-FLG-SHIDOU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-SYOBYO-COMMENT
                   MOVE   "1"   TO    RECE44-FLG-SYOBYO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-RYOYO-COMMENT
                   MOVE   "1"   TO    RECE44-FLG-RYOYO-COMMENT
              END-IF
           END-IF
      *
           IF      RECE44-SRYKBN      =     "14"
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUSIJI-COMMENT
              AND  FLG-HOUSIJI-COM    =     ZERO
                   MOVE   "1"   TO    RECE44-FLG-HOUSIJI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUSIJI2-COMMENT
                   MOVE  SPACE  TO    RECE44-FLG-HOUSIJI-COMMENT
                   MOVE    1    TO    FLG-HOUSIJI-COM
              END-IF
           END-IF
           IF      RECE44-SRYKBN      =     "14"
              IF   SRY-SRYCD(IDX2)    =     CONS-HINKAI-COMMENT
                   MOVE   "1"   TO    RECE44-FLG-HINKAI-COMMENT
              END-IF
           END-IF
      *
           IF      RECE44-SRYKBN      =     "33"
              IF   SRY-SRYSYUKBN      =     "334"
              AND  FLG-HOUTEN-COM     =     ZERO
                   MOVE   "1"   TO    RECE44-FLG-HOUTEN-COMMENT
              END-IF
           END-IF
      *
           IF      RECE44-SRYKBN      =     "50"
              IF   RECE44-SYUTEN1     =     ZERO
                   MOVE   "1"   TO    RECE44-FLG-SHUJUTU-COMMENT
              END-IF
              IF   SRY-SRYSYUKBN      =     "510"
                   MOVE   "1"   TO    RECE44-FLG-SHUJUTU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-SHUJUTU-COMMENT
                   MOVE   "1"   TO    RECE44-FLG-SHUJUTU-COMMENT
              END-IF
           END-IF
      *
           IF      RECE44-SRYKBN      =     "70"
              IF   SRY-SRYCD(IDX2)    =     CONS-CTMRI-COMMENT1  OR
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
                                            CONS-CTMRI-COMMENT17
                   MOVE   "1"   TO    RECE44-FLG-CTMRI-COMMENT
              END-IF
           END-IF
      *
           IF      RECE44-SRYKBN   =  "14" OR "40" OR "50" OR "54"
                                           OR "60" OR "70" OR "80"
              SET     TBL-SANSO    TO      1
              SEARCH      TBL-SANSO-OCC   VARYING   TBL-SANSO
                   AT  END
                       MOVE    ZERO            TO  FLG-SANSO-OK
                   WHEN    SRY-SRYCD (IDX2)    =   TBL-SANSO-SRYCD
                                                        (TBL-SANSO)
                       MOVE    1               TO  FLG-SANSO-OK
              END-SEARCH
              IF      FLG-SANSO-OK        =    1
                   MOVE   "1"   TO    RECE44-FLG-SANSO-COMMENT
              END-IF
           END-IF
      *
           .
       2101211-RECE45-TENSOU-COMCNT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    »ÀÁÇ¤ÎÃ±²ÁÊÔ½¸½èÍý
      *****************************************************************
       2101212-RECE45-TENSOU-TANKA-HEN-SEC     SECTION.
      *
      ***  IF      TNS-TEN         =   ZERO
      ***      GO  TO  2101212-RECE45-TENSOU-TANKA-HEN-EXT  
      ***  END-IF
      *
           MOVE    SPACE           TO  WRK-TANKA-X
      * 
      *    ¿ô»úÉ½¼¨ÍÑÊÔ½¸¡ÊÈ¾³Ñ¡Ë
           MOVE    TNS-TEN         TO  WRK-TANKA-9
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
       2101212-RECE45-TENSOU-TANKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼«ÇåÀÕÆÃÄê´ïºàÅÀ¿ôÊÔ½¸½èÍý
      *****************************************************************
       2101213-JIBAI-KIZAI-TENSU-HENSYU-SEC     SECTION.
      *
           MOVE     ZERO                   TO    WRK-TEN-KZI
           MOVE     ZERO                   TO    FLG-TAIGAI
      *
           IF   RECE41-TAXKBN-KZI   =   "2"
      *
      *       ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡Ê£´£°£°£³¡Ë¤Ë¤Æ¾ÃÈñÀÇÂÐ¾Ý³°¥³¡¼¥É¤«È½ÊÌ
              INITIALIZE                         SYS-4003-REC
              MOVE    "4003"               TO    SYS-4003-KANRICD
              MOVE    "*"                  TO    SYS-4003-KBNCD
              MOVE    RECE41-SRYYM         TO    ORC-DBYMD (1:6)
              MOVE    "01"                 TO    ORC-DBYMD (7:2)
              MOVE    SYS-4003-REC         TO    MCPDATA-REC
              MOVE    "SYSKANRI-KEY"       TO    WRK-PATH
              PERFORM 800-SYSKANRI-READ-SEC
              IF      FLG-SYSKANRI        =   ZERO
                  MOVE    SYSKANRI-REC     TO    SYS-4003-REC
                  PERFORM   VARYING   IDX5    FROM   1   BY   1
                             UNTIL   (IDX5                 >    10 )
                               OR    (SYS-4003-SRYCD(IDX5) =  SPACE)
                               OR    (FLG-TAIGAI           =     1 )
                      IF   SYS-4003-SRYCD(IDX5)  =  SRY-SRYCD(IDX2)
                          MOVE     1       TO    FLG-TAIGAI
                      END-IF
                  END-PERFORM
              END-IF
      *
      *       ÆÃÄê´ïºàÅù¾ÃÈñÀÇ·×»»
              IF   FLG-TAIGAI  =  ZERO
                   IF    WRK-TAX1001-KBN  =  "1"
                      COMPUTE  WRK-TEN-KZI  =
                              (TNS-TEN  *  1.05)  +  0.5
                   ELSE
                      COMPUTE  WRK-TEN-KZI  =
                              (TNS-TEN  *  1.05)
                   END-IF
              ELSE
                   MOVE     TNS-TEN       TO     WRK-TEN-KZI
              END-IF
           ELSE
                   MOVE     TNS-TEN       TO     WRK-TEN-KZI
           END-IF
      *
           .
       2101213-JIBAI-KIZAI-TENSU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷
      *****************************************************************
       210122-RECE41-HENSYU-SEC               SECTION.
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
      *
           PERFORM   VARYING  IDX3   FROM   1   BY   1
                     UNTIL    (IDX3         >     5)        OR
                              (SRY-SRYCD(IDX3)   =   SPACE)
               IF    SRY-RENNUM     =     1
                   IF  IDX3         =     1
                       IF      RECE41-HKNKBN   NOT  =   "3"
                          MOVE  ACCT-ZAIKAISU     TO   WK-ZAIKAISU
                       ELSE
                          MOVE  WRK-ZAIKAISU-XXX  TO   WK-ZAIKAISU
                       END-IF 
                       MOVE  ACCT-ZAITEN       TO   WK-ZAITEN
                       IF   RECE41-HKNKBN    NOT  =  "4"                
                         IF ( ACCT-SRYKBN          =  "80"    ) AND   
                            ( SRY-SRYCD(IDX3)(1:5) =  "09593" )       
                                  MOVE   ACCT-SYUTEN1   TO   WK-SYUTEN  
                                  COMPUTE   WK-YKZTEN =                 
                                                      ACCT-KIZAITEN  +  
                                                      ACCT-YKZTEN       
                         ELSE                                         
                           IF    SRY-SRYKBN     =  "40"  OR  "50"  OR
                                                   "70"  OR  "80"  OR
                                                   "54"  OR  "60"
                                  MOVE   ACCT-SYUTEN1   TO   WK-SYUTEN
                                  COMPUTE   WK-YKZTEN =
                                                      ACCT-KIZAITEN  +
                                                      ACCT-YKZTEN
                           END-IF
                         END-IF
                       ELSE                                             
                         IF ( ACCT-SRYKBN          =  "80"    )   AND
                            ( SRY-SRYCD(IDX3)(1:5) =  "09591" OR 
                                                      "09592" )
                               CONTINUE
                         ELSE
                           IF ( ACCT-SRYKBN          =  "80"    ) AND   
                              ( SRY-SRYCD(IDX3)(1:5) =  "09593" )       
                                  MOVE   ACCT-SYUTEN1   TO   WK-SYUTEN  
                                  COMPUTE   WK-YKZTEN =                 
                                                      ACCT-KIZAITEN  +  
                                                      ACCT-YKZTEN       
                           ELSE                                         
                             IF    SRY-SRYKBN   =  "30"  OR  "31"  OR     
                                                   "32"  OR  "33"  OR     
                                                   "40"  OR  "50"  OR     
                                                   "54"  OR  "60"  OR     
                                                   "70"  OR  "80"         
                                  MOVE   ACCT-SYUTEN1   TO   WK-SYUTEN  
                                  COMPUTE   WK-YKZTEN =                 
                                                      ACCT-KIZAITEN  +  
                                                      ACCT-YKZTEN       
                                  IF  SRY-SRYKBN  =  "70"
                                     COMPUTE  WK-SYUTEN  =  WK-SYUTEN
                                                         -  ACCT-SYUTEN2
                                     COMPUTE  WK-YKZTEN  =  WK-YKZTEN
                                                         +  ACCT-SYUTEN2
                                  END-IF
                                  IF  WRK-YOUBUKASAN  NOT =  ZERO
                                   COMPUTE  WK-SYUTEN  = WK-SYUTEN
                                                       - WRK-YOUBUKASAN
                                   COMPUTE  WK-YKZTEN  = WK-YKZTEN
                                                       + WRK-YOUBUKASAN
                                  END-IF
                                  IF  WRK-KYOBUKASAN  NOT =  ZERO
                                   COMPUTE  WK-SYUTEN  = WK-SYUTEN
                                                       - WRK-KYOBUKASAN
                                   COMPUTE  WK-YKZTEN  = WK-YKZTEN
                                                       + WRK-KYOBUKASAN
                                  END-IF
                                  IF  WRK-SOGAIKASAN  NOT =  ZERO
                                   COMPUTE  WK-SYUTEN  = WK-SYUTEN
                                                       - WRK-SOGAIKASAN
                                   COMPUTE  WK-YKZTEN  = WK-YKZTEN
                                                       + WRK-SOGAIKASAN
                                  END-IF
                             END-IF
                           END-IF                                       
                         END-IF
                       END-IF                                           
                       IF    SRY-SRYKBN     =  "14"
                           MOVE    ACCT-SYUTEN1   TO   WK-SYUTEN
                           MOVE    ACCT-KIZAITEN  TO   WK-KIZAITEN
                           MOVE    ACCT-YKZTEN    TO   WK-YKZTEN
                           IF   RECE41-HKNKBN   =  "4"
                             IF  WRK-YOUBUKASAN  NOT =  ZERO
                                COMPUTE  WK-SYUTEN   =  WK-SYUTEN
                                                     -  WRK-YOUBUKASAN
                                COMPUTE  WK-KIZAITEN =  WK-KIZAITEN
                                                     +  WRK-YOUBUKASAN
                             END-IF
                             IF  WRK-KYOBUKASAN  NOT =  ZERO
                                COMPUTE  WK-SYUTEN   =  WK-SYUTEN
                                                     -  WRK-KYOBUKASAN
                                COMPUTE  WK-KIZAITEN =  WK-KIZAITEN
                                                     +  WRK-KYOBUKASAN
                             END-IF
                             IF  WRK-SOGAIKASAN  NOT =  ZERO
                                COMPUTE  WK-SYUTEN   =  WK-SYUTEN
                                                     -  WRK-SOGAIKASAN
                                COMPUTE  WK-KIZAITEN =  WK-KIZAITEN
                                                     +  WRK-SOGAIKASAN
                             END-IF
                           END-IF
                       END-IF
                   ELSE
                       IF   SRY-SRYKBN   =  "21"  OR  "22"  OR  "23" OR
                                            "31"  OR  "32"  OR  "33" OR
                                            "40"  OR  "50"  OR  "54" OR
                                            "60"  OR  "70"  OR  "80" OR
                                            "13"  OR  "14"
                           MOVE  ZERO          TO   WK-ZAIKAISU
                           MOVE  ZERO          TO   WK-ZAITEN
                           MOVE  ZERO          TO   WK-SYUTEN
                           MOVE  ZERO          TO   WK-KIZAITEN
                           MOVE  ZERO          TO   WK-YKZTEN
                           MOVE  ZERO          TO   FLG-OSINKBN
                       END-IF
                   END-IF
               ELSE
                     MOVE    ZERO     TO    WK-ZAIKAISU
                     MOVE    ZERO     TO    WK-ZAITEN
                     MOVE    ZERO     TO    WK-SYUTEN
                     MOVE    ZERO     TO    WK-KIZAITEN
                     MOVE    ZERO     TO    WK-YKZTEN
                     MOVE    ZERO     TO    FLG-OSINKBN
               END-IF
               IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                    (ACCT-ZAIKAISU  NOT =    ZERO)
                  IF      RECE41-HKNKBN   NOT  =   "3"
                     IF   RECE41-HKNKBN   NOT  =   "4"
                        PERFORM   2101221-RECE-LEFT-SET-SEC
                     ELSE
                        PERFORM   2101223-RECE-LEFT-SET3-SEC
                     END-IF
                  ELSE
                        PERFORM   2101222-RECE-LEFT-SET2-SEC
                  END-IF
               END-IF
               IF    SRY-SRYKBN   =  "21"   OR   "22"   OR   "23"   OR
                                     "31"   OR   "32"   OR   "33"   OR
                                     "40"   OR   "50"   OR   "54"   OR
                                     "60"   OR   "70"   OR   "14"
                     MOVE    6      TO    IDX3
               END-IF
           END-PERFORM
      *
           IF      RECE41-HKNKBN   NOT  =   "3"
      *
              IF   RECE41-HKNKBN   NOT  =   "4"                         
      *
      *         ½èÊý¤Î»»Äê²ó¿ô¤«¤éÆÃÄê¼À´µ½èÊý´ÉÍý²Ã»»¤Î¸º»»
                COMPUTE    RECE41-KAISU(19)   =    RECE41-KAISU(19)  -
                                                   WRK-TOKUTEI-SKAN
      *
      *         ½èÊý¤Î»»Äê²ó¿ô¤«¤é½èÊý(ÆýÍÄ»ù)²Ã»»¤Î¸º»»
                COMPUTE    RECE41-KAISU(19)   =    RECE41-KAISU(19)  -
                                                   WRK-NYUYOUJI-SKAN           
      *
      *         ½èÊý¤Î»»Äê²ó¿ô¤«¤éÄ¹´üÅêÌô²Ã»»¡Ê½èÊýÎÁ)¤Î¸º»»
                COMPUTE    RECE41-KAISU(19)   =    RECE41-KAISU(19)  -  
                                                   WRK-CYOKI-SKAN       
      *
      *         ½èÊý¤Î»»Äê²ó¿ô¤«¤éÆÃÄê¼À´µ½èÊý´ÉÍý²Ã»»(½èÊýÎÁ)¤Î¸º»»
                COMPUTE    RECE41-KAISU(33)   =    RECE41-KAISU(33)  -
                                                   WRK-TOKUTEI-SKAN2           
      *
      *         ½èÊý¤»¤ó¤Î»»Äê²ó¿ô¤«¤é½èÊý¤»¤ó(ÆýÍÄ»ù)²Ã»»¤Î¸º»»
                COMPUTE    RECE41-KAISU(33)   =    RECE41-KAISU(33)  -
                                                   WRK-NYUYOUJI-SHOKAN      
      *
      *         ½èÊý¤»¤ó¤Î»»Äê²ó¿ô¤«¤éÄ¹´üÅêÌô²Ã»»(½èÊý¤»¤óÎÁ)¤Î¸º»»
                COMPUTE    RECE41-KAISU(33)   =    RECE41-KAISU(33)  -  
                                                   WRK-CYOKI-SKAN2      
      *
      *         ËãÆÇ¤Î»»Äê²ó¿ô¤«¤é½èÊýÎÁ¡ÊËã¡¦¸þ¡¦ÆÇ¡¦³Ð¡Ë¤Î¸º»»
                COMPUTE    RECE41-KAISU(20)   =    RECE41-KAISU(20)  -
                                                   WRK-MADOKU-SHOHO  
      *
              ELSE                                                      
      *
      *         ½èÊý¤Î»»Äê²ó¿ô¤«¤éÆÃÄê¼À´µ½èÊý´ÉÍý²Ã»»¤Î¸º»»
                COMPUTE    RECE41-KAISU(14)   =    RECE41-KAISU(14)  -  
                                                   WRK-TOKUTEI-SKAN     
      *
      *         ½èÊý¤Î»»Äê²ó¿ô¤«¤é½èÊý(ÆýÍÄ»ù)²Ã»»¤Î¸º»»
                COMPUTE    RECE41-KAISU(14)   =    RECE41-KAISU(14)  -  
                                                   WRK-NYUYOUJI-SKAN     
      *
      *         ½èÊý¤Î»»Äê²ó¿ô¤«¤éÄ¹´üÅêÌô²Ã»»¡Ê½èÊýÎÁ)¤Î¸º»»
                COMPUTE    RECE41-KAISU(14)   =    RECE41-KAISU(14)  -  
                                                   WRK-CYOKI-SKAN       
      *
      *         ½èÊý¤Î»»Äê²ó¿ô¤«¤éÆÃÄê¼À´µ½èÊý´ÉÍý²Ã»»(½èÊýÎÁ)¤Î¸º»»
                COMPUTE    RECE41-KAISU(33)   =    RECE41-KAISU(33)  -  
                                                   WRK-TOKUTEI-SKAN2    
      *
      *         ½èÊý¤»¤ó¤Î»»Äê²ó¿ô¤«¤é½èÊý¤»¤ó(ÆýÍÄ»ù)²Ã»»¤Î¸º»»
                COMPUTE    RECE41-KAISU(33)   =    RECE41-KAISU(33)  -  
                                                   WRK-NYUYOUJI-SHOKAN  
      *
      *         ½èÊý¤»¤ó¤Î»»Äê²ó¿ô¤«¤éÄ¹´üÅêÌô²Ã»»(½èÊý¤»¤óÎÁ)¤Î¸º»»
                COMPUTE    RECE41-KAISU(33)   =    RECE41-KAISU(33)  -  
                                                   WRK-CYOKI-SKAN2      
      *
      *         ËãÆÇ¤Î»»Äê²ó¿ô¤«¤é½èÊýÎÁ¡ÊËã¡¦¸þ¡¦ÆÇ¡¦³Ð¡Ë¤Î¸º»»
                COMPUTE    RECE41-KAISU(15)   =    RECE41-KAISU(15)  -  
                                                   WRK-MADOKU-SHOHO     
      *
              END-IF                                                    
      *
           ELSE
      *
      *         ½èÊý¤Î»»Äê²ó¿ô¤«¤éÆÃÄê¼À´µ½èÊý´ÉÍý²Ã»»¤Î¸º»»
                COMPUTE    RECE41-KAISU(12)   =    RECE41-KAISU(12)  -
                                                   WRK-TOKUTEI-SKAN
      *
      *         ½èÊý¤Î»»Äê²ó¿ô¤«¤é½èÊý(ÆýÍÄ»ù)²Ã»»¤Î¸º»»
                COMPUTE    RECE41-KAISU(12)   =    RECE41-KAISU(12)  -
                                                   WRK-NYUYOUJI-SKAN           
      *
      *         ½èÊý¤Î»»Äê²ó¿ô¤«¤éÄ¹´üÅêÌô²Ã»»¡Ê½èÊýÎÁ)¤Î¸º»»
                COMPUTE    RECE41-KAISU(12)   =    RECE41-KAISU(12)  -  
                                                   WRK-CYOKI-SKAN       
      *
      *         ½èÊý¤Î»»Äê²ó¿ô¤«¤éÆÃÄê¼À´µ½èÊý´ÉÍý²Ã»»(½èÊýÎÁ)¤Î¸º»»
                COMPUTE    RECE41-KAISU(30)   =    RECE41-KAISU(30)  -
                                                   WRK-TOKUTEI-SKAN2           
      *
      *         ½èÊý¤»¤ó¤Î»»Äê²ó¿ô¤«¤é½èÊý¤»¤ó(ÆýÍÄ»ù)²Ã»»¤Î¸º»»
                COMPUTE    RECE41-KAISU(30)   =    RECE41-KAISU(30)  -
                                                   WRK-NYUYOUJI-SHOKAN      
      *
      *         ½èÊý¤»¤ó¤Î»»Äê²ó¿ô¤«¤éÄ¹´üÅêÌô²Ã»»(½èÊý¤»¤óÎÁ)¤Î¸º»»
                COMPUTE    RECE41-KAISU(30)   =    RECE41-KAISU(30)  -  
                                                   WRK-CYOKI-SKAN2      
      *
      *         ËãÆÇ¤Î»»Äê²ó¿ô¤«¤é½èÊýÎÁ¡ÊËã¡¦¸þ¡¦ÆÇ¡¦³Ð¡Ë¤Î¸º»»
                COMPUTE    RECE41-KAISU(13)   =    RECE41-KAISU(13)  -
                                                   WRK-MADOKU-SHOHO  
      *
           END-IF         
      *
           .                                           
      *
       210122-RECE41-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷
      *****************************************************************
       2101221-RECE-LEFT-SET-SEC               SECTION.
      *
           EVALUATE    SRY-SRYKBN
      *    ½é¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "11"
                   IF   WRK-TNS-DATAKBN(IDX3)  =   2
                     EVALUATE    WRK-TNS-TIMEKSNKBN(IDX3)
                         WHEN    1
                         WHEN    5
                             MOVE  "1"     TO   RECE41-SHOSHINTIME
                         WHEN    3
                             MOVE  "2"     TO   RECE41-SHOSHINTIME
                         WHEN    4
                             MOVE  "3"     TO   RECE41-SHOSHINTIME
                     END-EVALUATE
                     COMPUTE    RECE41-TENSU(1)  =
                                RECE41-TENSU(1)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF
                   IF   SRY-SRYCD(IDX3) = CONS-ROUSAI1    
                        COMPUTE    RECE41-KINGAKU2(1)  =
                                   RECE41-KINGAKU2(1)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF  
                   IF   SRY-SRYCD(IDX3) = CONS-ROUSAI2 OR
                                          CONS-ROUSAI3    
                        COMPUTE    RECE41-KINGAKU2(4)  =
                                   RECE41-KINGAKU2(4)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF  
      *    ºÆ¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸¡Ê½¸·×ÊýË¡ÊÑ¹¹¸å¡Ë
               WHEN    "12"
                 EVALUATE  WRK-TNS-GAITENTTLKBN(IDX3)
      *      ¡Ê´ðËÜ¼êµ»¡Ë
                   WHEN  120
                     IF   SRY-SRYCD(IDX3) =  CONS-ROUSAI5  OR
                                             CONS-ROUSAI6  OR
                                             CONS-ROUSAI7  
                         ADD   WK-ZAIKAISU      TO   RECE41-KAISU2(2)
                         COMPUTE    RECE41-KINGAKU2(2)  =
                                    RECE41-KINGAKU2(2)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     ELSE
                         IF    RECE41-KHNTEN(2)   =   ZERO
                            MOVE  WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(2)
                         ELSE
                            IF RECE41-KHNTEN(2) NOT = WRK-TNS-TEN(IDX3)
                               MOVE   ZERO    TO  RECE41-KHNTEN(2)
                               MOVE   1    TO  FLG-KHNTEN2
                            END-IF
                         END-IF
                         ADD    WK-ZAIKAISU   TO   RECE41-KAISU(2)
                         COMPUTE    RECE41-TENSU(2)  =
                                    RECE41-TENSU(2)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     END-IF
      *
      *      ¡Ê³°Íè´ÉÍý²Ã»»¡Ë
                   WHEN  122
                        IF    RECE41-KHNTEN(3)   =   ZERO
                           MOVE  WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(3)
                        ELSE
                           IF RECE41-KHNTEN(3) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO  RECE41-KHNTEN(3)
                              MOVE   1    TO  FLG-KHNTEN3
                           END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(3)
                        COMPUTE    RECE41-TENSU(3)  =
                                   RECE41-TENSU(3)  +
                                  (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      ¡Ê»þ´Ö³°¡Ë
                   WHEN  123
                        IF    RECE41-KHNTEN(4)   =   ZERO
                           MOVE  WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(4)
                        ELSE
                           IF RECE41-KHNTEN(4) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO
                                            RECE41-KHNTEN(4)
                              MOVE   1    TO  FLG-KHNTEN4
                           END-IF
                        END-IF
                        ADD   WK-ZAIKAISU  TO
                                            RECE41-KAISU(4)
                        COMPUTE RECE41-TENSU(4) =
                                RECE41-TENSU(4) +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      ¡ÊµÙÆü¡Ë
                   WHEN  124
                        IF    RECE41-KHNTEN(5)   =   ZERO
                           MOVE  WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(5)
                        ELSE
                           IF RECE41-KHNTEN(5) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO
                                            RECE41-KHNTEN(5)
                              MOVE   1     TO   FLG-KHNTEN5
                           END-IF
                        END-IF
                        ADD   WK-ZAIKAISU   TO
                                              RECE41-KAISU(5)
                        COMPUTE  RECE41-TENSU(5) =
                                 RECE41-TENSU(5) +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *       ¡Ê¿¼Ìë¡Ë
                   WHEN  125
                        IF    RECE41-KHNTEN(6)   =   ZERO
                           MOVE  WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(6)
                        ELSE
                           IF RECE41-KHNTEN(6) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO
                                            RECE41-KHNTEN(6)
                              MOVE   1       TO   FLG-KHNTEN6
                           END-IF
                        END-IF
                        ADD   WK-ZAIKAISU  TO
                                            RECE41-KAISU(6)
                        COMPUTE  RECE41-TENSU(6) =
                                 RECE41-TENSU(6) +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *       ¡Ê¤½¤ÎÂ¾¡Ë
                   WHEN  OTHER 
                        IF   SRY-SRYCD(IDX3) =  CONS-ROUSAI4  
                           ADD   WK-ZAIKAISU   TO   RECE41-KAISU2(3)
                           COMPUTE    RECE41-KINGAKU2(3)  =
                                      RECE41-KINGAKU2(3)  +
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        END-IF
                 END-EVALUATE
      *    »ØÆ³¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "13"
                   IF   SRY-SRYCD(IDX3) =   CONS-ROUSAI8  OR
                                            CONS-ROUSAI9  OR
                                            CONS-ROUSAI10 OR
                                            CONS-ROUSAI11 OR
                                            CONS-ROUSAI12 OR
                                            CONS-ROUSAI13 OR
                                            CONS-ROUSAI14 OR
                                            CONS-ROUSAI15 OR
                                            CONS-ROUSAI16 OR
                                            CONS-ROUSAI17 
                         COMPUTE    RECE41-KINGAKU2(4)  =
                                    RECE41-KINGAKU2(4)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   ELSE
                         COMPUTE    RECE41-TENSU(7)  =
                                    RECE41-TENSU(7)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF                   
      *
      *    ºßÂð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "14"
                   MOVE      ZERO            TO   FLG-ZAITAKU
      *            ±ý¿Ç¡¦ÆÃÊÌ±ý¿Ç¡Ê»þ´Ö²Ã»»¡Ë
                   IF    FLG-OSINKBN         =   1
                       MOVE         1        TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(8)
                       COMPUTE   RECE41-TENSU(8)  =
                                 RECE41-TENSU(8)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            Ìë´Ö
                   IF    FLG-OSINKBN         =    2
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(9)
                       COMPUTE   RECE41-TENSU(9)  =
                                 RECE41-TENSU(9)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            ¿¼Ìë¡¦¶ÛµÞ
                   IF    FLG-OSINKBN         =    3
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(10)
                       COMPUTE   RECE41-TENSU(10)  =
                                 RECE41-TENSU(10)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅ
                   IF  ( WRK-TNS-OSINKBN(IDX3) =     1          )  AND
                       ((SRY-SRYCD(IDX3)   NOT =    "114000110")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001610")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000370")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002070"))
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(11)
                       COMPUTE   RECE41-TENSU(11)  =
                                 RECE41-TENSU(11)  +
                                (WK-SYUTEN         * ACCT-ZAIKAISU)
                   END-IF
      *            ¤½¤ÎÂ¾
                   IF    FLG-ZAITAKU    =   ZERO
                       COMPUTE   RECE41-TENSU(12)  =
                                 RECE41-TENSU(12)  +
                                (WK-SYUTEN     *   WK-ZAIKAISU)
                   END-IF
      *            ÌôºÞ
                   COMPUTE   RECE41-TENSU(13)  =
                             RECE41-TENSU(13)  +
                             (WK-KIZAITEN *   WK-ZAIKAISU)  +
                             (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ÅêÌô¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                     IF   SRY-SRYCD(IDX3)         =    CONS-YAKTEIGEN
                       COMPUTE   RECE41-TENSU(14)  =
                                 RECE41-TENSU(14)  -
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                       ADD   WK-ZAIKAISU       TO   RECE41-KAISU(14)
                       COMPUTE   RECE41-TENSU(14)  =
                                 RECE41-TENSU(14)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)   =    CONS-FUNSAI-COMMENT
                       ADD   WK-ZAIKAISU       TO   RECE41-KAISU(14)
                       COMPUTE   RECE41-TENSU(14)  =
                                 RECE41-TENSU(14)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE   
                       IF    RECE41-KHNTEN(15)   =   ZERO
                          MOVE  WRK-TNS-TEN(IDX3)  TO  RECE41-KHNTEN(15)
                       ELSE
                          IF   RECE41-KHNTEN(15) NOT = WRK-TNS-TEN(IDX3)
                               MOVE   ZERO    TO    RECE41-KHNTEN(15)
                               MOVE   1       TO    FLG-KHNTEN15
                          END-IF
                       END-IF
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(15)
                       COMPUTE   RECE41-TENSU(15)  =
                                 RECE41-TENSU(15)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     END-IF   
                   END-IF
               WHEN    "22"
                   ADD    WK-ZAIKAISU    TO   RECE41-KAISU(16)
                   COMPUTE   RECE41-TENSU(16)  =
                             RECE41-TENSU(16)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
               WHEN    "23"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(17)
                       COMPUTE   RECE41-TENSU(17)  =
                                 RECE41-TENSU(17)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   ELSE
                       IF    RECE41-KHNTEN(18)   =   ZERO
                          MOVE  WRK-TNS-TEN(IDX3)  TO  RECE41-KHNTEN(18)
                       ELSE
                          IF   RECE41-KHNTEN(18) NOT = WRK-TNS-TEN(IDX3)
                               MOVE   ZERO    TO    RECE41-KHNTEN(18)
                               MOVE   1       TO    FLG-KHNTEN18
                          END-IF
                       END-IF
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(18)
                       COMPUTE   RECE41-TENSU(18)  =
                                 RECE41-TENSU(18)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF
      *    ½èÊý¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "25"
                   IF SRY-SRYCD(IDX3)   =    "120002270"
                      COMPUTE  WRK-TOKUTEI-SKAN  = WRK-TOKUTEI-SKAN +
                                                   WK-ZAIKAISU 
                   END-IF
                   IF SRY-SRYCD(IDX3)   =    "120002170"
                      COMPUTE  WRK-NYUYOUJI-SKAN = WRK-NYUYOUJI-SKAN +
                                                   WK-ZAIKAISU 
                   END-IF
                   IF SRY-SRYCD(IDX3)   =    "120003170"                
                      COMPUTE  WRK-CYOKI-SKAN  =  WRK-CYOKI-SKAN +      
                                                  WK-ZAIKAISU           
                   END-IF                                               
                   IF    RECE41-KHNTEN(19)   =   ZERO
                      MOVE  WRK-TNS-TEN(IDX3)  TO  RECE41-KHNTEN(19)
                   ELSE
                      IF   RECE41-KHNTEN(19) NOT = WRK-TNS-TEN(IDX3)
                           MOVE   ZERO    TO    RECE41-KHNTEN(19)
                           MOVE   1       TO    FLG-KHNTEN19
                      END-IF
                   END-IF
                   ADD    WK-ZAIKAISU     TO    RECE41-KAISU(19)
                   COMPUTE   RECE41-TENSU(19)  =
                             RECE41-TENSU(19)  +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *    ËãÆÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "26"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(20)
                   IF SRY-SRYCD(IDX3)      =    "120001310"
                    COMPUTE WRK-MADOKU-SHOHO = WRK-MADOKU-SHOHO +
                                               WK-ZAIKAISU 
                   END-IF
                   COMPUTE   RECE41-TENSU(20)  =
                             RECE41-TENSU(20)  +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *    Ä´´ð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "27"
                   COMPUTE   RECE41-TENSU(21)  =
                             RECE41-TENSU(21)  +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *    Èé²¼¶ÚÆùÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "31"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(22)
                   COMPUTE   RECE41-TENSU(22)  =
                             RECE41-TENSU(22)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ÀÅÌ®ÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "32"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(23)
                   COMPUTE   RECE41-TENSU(23)  =
                             RECE41-TENSU(23)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾Ãí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "33"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(24)
                   COMPUTE   RECE41-TENSU(24)  =
                             RECE41-TENSU(24)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ½èÃÖ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(25)
                   END-IF
                   COMPUTE   RECE41-TENSU(25)  =
                             RECE41-TENSU(25)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(26)  =
                             RECE41-TENSU(26)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¼ê½Ñ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(27)
                   END-IF
                   COMPUTE   RECE41-TENSU(27)  =
                             RECE41-TENSU(27)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(28)  =
                             RECE41-TENSU(28)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¸¡ºº¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "60"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(29)
                   END-IF
                   COMPUTE   RECE41-TENSU(29)  =
                             RECE41-TENSU(29)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    WRK-TNS-KNSJISKBN(IDX3)    =   2
                         COMPUTE   RECE41-KAISU(29)  =
                                   RECE41-KAISU(29)  -  
                                   WK-ZAIKAISU
                   END-IF
                   IF    WRK-TNS-SRYCD(IDX3)  =   CONS-SAIKETSU1  OR
                                                  CONS-SAIKETSU2  OR
                                                  CONS-SAIKETSU3
                         IF   RECE41-KAISU(29)  NOT =  ZERO
                              COMPUTE   RECE41-KAISU(29)  =
                                        RECE41-KAISU(29)  -   
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE41-TENSU(30)  =
                             RECE41-TENSU(30)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ²èÁü¿ÇÃÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(31)
                   END-IF
                   COMPUTE   RECE41-TENSU(31)  =
                             RECE41-TENSU(31)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(32)  =
                             RECE41-TENSU(32)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "80"
                   IF ( ACCT-SRYKBN          =  "80"    )  AND         
                      ( SRY-SRYCD(IDX3)(1:5) =  "09593" )              
                       COMPUTE    RECE41-KINGAKU2(4)  =                 
                                  RECE41-KINGAKU2(4)  +                 
                                 (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)  
                   ELSE               
                    IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN2
                     COMPUTE  WRK-TOKUTEI-SKAN2 =  WRK-TOKUTEI-SKAN2 +
                                                   WK-ZAIKAISU 
                    END-IF
                    IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN1
                     COMPUTE WRK-NYUYOUJI-SHOKAN = WRK-NYUYOUJI-SHOKAN +
                                                  WK-ZAIKAISU 
                    END-IF
                    IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN3         
                     COMPUTE  WRK-CYOKI-SKAN2  =   WRK-CYOKI-SKAN2   +  
                                                   WK-ZAIKAISU          
                    END-IF                                              
                    IF   SRY-SRYCD(IDX3)   =  CONS-SHOHOSEN1 OR
                                              CONS-SHOHOSEN2 OR
                                              CONS-SHOHOSEN3 OR
                                              CONS-SHOHOSEN4 OR
                                              CONS-SHOHOKSN1 OR
                                              CONS-SHOHOKSN2 OR
                                              CONS-SHOHOKSN3            
                         ADD    WK-ZAIKAISU  TO  RECE41-KAISU(33)
                         COMPUTE   RECE41-TENSU(33)  =
                                   RECE41-TENSU(33)  +
                                  (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                    ELSE
                         IF    WRK-TNS-GAITENTTLKBN(IDX3)   =    130
                           COMPUTE   RECE41-TENSU(7)  =
                                     RECE41-TENSU(7)  +
                                    (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                         ELSE  
                           COMPUTE   RECE41-TENSU(34)  =
                                     RECE41-TENSU(34)  +
                                    (WK-SYUTEN   *   ACCT-ZAIKAISU)
                         END-IF
                    END-IF
                    COMPUTE   RECE41-TENSU(35)  =
                              RECE41-TENSU(35)  +
                             (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
           END-EVALUATE
      *
           .
      *
       2101221-RECE-LEFT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷
      *****************************************************************
       2101222-RECE-LEFT-SET2-SEC               SECTION.
      *
           EVALUATE    SRY-SRYKBN
      *    ½é¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "11"
                   IF   WRK-TNS-DATAKBN(IDX3)  =   2
                     EVALUATE    WRK-TNS-TIMEKSNKBN(IDX3)
                         WHEN    1
                         WHEN    5
                             MOVE  "1"     TO   RECE41-SHOSHINTIME
                         WHEN    3
                             MOVE  "2"     TO   RECE41-SHOSHINTIME
                         WHEN    4
                             MOVE  "3"     TO   RECE41-SHOSHINTIME
                     END-EVALUATE
                     ADD   WK-ZAIKAISU     TO   RECE41-KAISU(1)
                     COMPUTE    RECE41-TENSU(1)  =
                                RECE41-TENSU(1)  +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                   END-IF
                   IF   SRY-SRYCD(IDX3) = CONS-ROUSAI1    
                        COMPUTE    RECE41-KINGAKU2(1)  =
                                   RECE41-KINGAKU2(1)  +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                        MOVE    RECE41-TEKSTYMD    
                                           TO   RECE41-SYOSAIYMD2(1)
                   END-IF  
      *    ºÆ¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸¡Ê½¸·×ÊýË¡ÊÑ¹¹¸å¡Ë
               WHEN    "12"
                 EVALUATE  WRK-TNS-GAITENTTLKBN(IDX3)
      *      ¡Ê´ðËÜ¼êµ»¡Ë
                   WHEN  120
                     IF   SRY-SRYCD(IDX3) =  CONS-ROUSAI5  OR
                                             CONS-ROUSAI6  OR
                                             CONS-ROUSAI7  
                         COMPUTE    RECE41-KINGAKU2(2)  =
                                    RECE41-KINGAKU2(2)  +
                                 (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                         MOVE    RECE41-TEKSTYMD    
                                           TO   RECE41-SYOSAIYMD2(2)
                     ELSE
                         IF      RECE41-KEIZOKU-PRTKBN     =   "2"
                            ADD   WK-ZAIKAISU  TO
                                               RECE41-KAISU(3)
                            COMPUTE    RECE41-TENSU(3)  =
                                       RECE41-TENSU(3)  +
                                 (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                         ELSE
                            ADD   WK-ZAIKAISU  TO
                                               RECE41-KAISU(2)
                            COMPUTE    RECE41-TENSU(2)  =
                                       RECE41-TENSU(2)  +
                                 (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                         END-IF
                     END-IF
      *      ¡Ê»þ´Ö³°¡Ë
                   WHEN  123
                     IF      RECE41-KEIZOKU-PRTKBN     =   "2"
                        MOVE  "1"     TO   RECE41-SAISHINTIME 
                        ADD   WK-ZAIKAISU  TO
                                            RECE41-KAISU(4)
                        COMPUTE RECE41-TENSU(4) =
                                RECE41-TENSU(4) +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                     ELSE
                        MOVE  "1"     TO   RECE41-SAISHINTIME 
                        ADD   WK-ZAIKAISU  TO
                                            RECE41-KAISU(2)
                        COMPUTE RECE41-TENSU(2) =
                                RECE41-TENSU(2) +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                     END-IF
      *      ¡ÊµÙÆü¡Ë
                   WHEN  124
                     IF      RECE41-KEIZOKU-PRTKBN     =   "2"
                        MOVE  "2"     TO   RECE41-SAISHINTIME 
                        ADD   WK-ZAIKAISU  TO
                                            RECE41-KAISU(4)
                        COMPUTE RECE41-TENSU(4) =
                                RECE41-TENSU(4) +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                     ELSE
                        MOVE  "2"     TO   RECE41-SAISHINTIME 
                        ADD   WK-ZAIKAISU  TO
                                            RECE41-KAISU(2)
                        COMPUTE RECE41-TENSU(2) =
                                RECE41-TENSU(2) +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                     END-IF
      *      ¡Ê¿¼Ìë¡Ë
                   WHEN  125
                     IF      RECE41-KEIZOKU-PRTKBN     =   "2"
                        MOVE  "3"     TO   RECE41-SAISHINTIME 
                        ADD   WK-ZAIKAISU  TO
                                            RECE41-KAISU(4)
                        COMPUTE RECE41-TENSU(4) =
                                RECE41-TENSU(4) +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                     ELSE
                        MOVE  "3"     TO   RECE41-SAISHINTIME 
                        ADD   WK-ZAIKAISU  TO
                                            RECE41-KAISU(2)
                        COMPUTE RECE41-TENSU(2) =
                                RECE41-TENSU(2) +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                     END-IF
                 END-EVALUATE
      *    »ØÆ³¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "13"
                         COMPUTE    RECE41-TENSU(5)  =
                                    RECE41-TENSU(5)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ÅêÌô¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                     IF   SRY-SRYCD(IDX3)         =    CONS-YAKTEIGEN
                       COMPUTE   RECE41-TENSU(7)  =
                                 RECE41-TENSU(7)  -
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                       ADD   WK-ZAIKAISU       TO   RECE41-KAISU(7)
                       COMPUTE   RECE41-TENSU(7)  =
                                 RECE41-TENSU(7)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)   =    CONS-FUNSAI-COMMENT
                       ADD   WK-ZAIKAISU       TO   RECE41-KAISU(7)
                       COMPUTE   RECE41-TENSU(7)  =
                                 RECE41-TENSU(7)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE   
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(8)
                       COMPUTE   RECE41-TENSU(8)  =
                                 RECE41-TENSU(8)  +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                     END-IF   
                   END-IF
               WHEN    "22"
                   ADD    WK-ZAIKAISU    TO   RECE41-KAISU(9)
                   COMPUTE   RECE41-TENSU(9)  =
                             RECE41-TENSU(9)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
               WHEN    "23"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(10)
                       COMPUTE   RECE41-TENSU(10)  =
                                 RECE41-TENSU(10)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   ELSE
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(11)
                       COMPUTE   RECE41-TENSU(11)  =
                                 RECE41-TENSU(11)  +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                   END-IF
      *    ½èÊý¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "25"
                   IF SRY-SRYCD(IDX3)   =    "120002270"
                      COMPUTE  WRK-TOKUTEI-SKAN  = WRK-TOKUTEI-SKAN +
                                                   WK-ZAIKAISU 
                   END-IF
                   IF SRY-SRYCD(IDX3)   =    "120002170"
                      COMPUTE  WRK-NYUYOUJI-SKAN = WRK-NYUYOUJI-SKAN +
                                                   WK-ZAIKAISU 
                   END-IF
                   IF SRY-SRYCD(IDX3)   =    "120003170"                
                      COMPUTE  WRK-CYOKI-SKAN  =  WRK-CYOKI-SKAN +      
                                                  WK-ZAIKAISU           
                   END-IF                                               
                   ADD    WK-ZAIKAISU     TO    RECE41-KAISU(12)
                   COMPUTE   RECE41-TENSU(12)  =
                             RECE41-TENSU(12)  +
                            (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
      *    ËãÆÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "26"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(13)
                   IF SRY-SRYCD(IDX3)      =    "120001310"
                    COMPUTE WRK-MADOKU-SHOHO = WRK-MADOKU-SHOHO +
                                               WK-ZAIKAISU 
                   END-IF
                   COMPUTE   RECE41-TENSU(13)  =
                             RECE41-TENSU(13)  +
                            (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
      *    Ä´´ð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "27"
                   COMPUTE   RECE41-TENSU(14)  =
                             RECE41-TENSU(14)  +
                            (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
      *    Èé²¼¶ÚÆùÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "31"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(15)
                   COMPUTE   RECE41-TENSU(15)  =
                             RECE41-TENSU(15)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ÀÅÌ®ÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "32"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(16)
                   COMPUTE   RECE41-TENSU(16)  =
                             RECE41-TENSU(16)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾Ãí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "33"
                   COMPUTE   RECE41-TENSU(17)  =
                             RECE41-TENSU(17)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ½èÃÖ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(18)
                   END-IF
                   COMPUTE   RECE41-TENSU(18)  =
                             RECE41-TENSU(18)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(20)  =
                             RECE41-TENSU(20)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¼ê½Ñ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(21)
                   END-IF
                   COMPUTE   RECE41-TENSU(21)  =
                             RECE41-TENSU(21)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(23)  =
                             RECE41-TENSU(23)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¸¡ºº¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "60"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(24)
                   END-IF
                   COMPUTE   RECE41-TENSU(24)  =
                             RECE41-TENSU(24)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    WRK-TNS-KNSJISKBN(IDX3)    =   2
                         COMPUTE   RECE41-KAISU(24)  =
                                   RECE41-KAISU(24)  -  
                                   WK-ZAIKAISU
                   END-IF
                   IF    WRK-TNS-SRYCD(IDX3)  =   CONS-SAIKETSU1  OR
                                                  CONS-SAIKETSU2  OR
                                                  CONS-SAIKETSU3
                         IF   RECE41-KAISU(24)  NOT =  ZERO
                              COMPUTE   RECE41-KAISU(24)  =
                                        RECE41-KAISU(24)  -   
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE41-TENSU(26)  =
                             RECE41-TENSU(26)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ²èÁü¿ÇÃÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(27)
                   END-IF
                   COMPUTE   RECE41-TENSU(27)  =
                             RECE41-TENSU(27)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(29)  =
                             RECE41-TENSU(29)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "80"
                   IF ( ACCT-SRYKBN          =  "80"    )  AND         
                      ( SRY-SRYCD(IDX3)(1:5) =  "09593" )              
                       CONTINUE
                   ELSE               
                    IF   SRY-SRYCD(IDX3)   =      CONS-SHOHOSEN1 OR
                                                  CONS-SHOHOSEN2 OR
                                                  CONS-SHOHOSEN3 OR
                                                  CONS-SHOHOSEN4 OR
                                                  CONS-SHOHOKSN1 OR
                                                  CONS-SHOHOKSN2 OR
                                                  CONS-SHOHOKSN3        
                         COMPUTE   RECE41-TENSU(30)  =
                                   RECE41-TENSU(30)  +
                                  (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                    ELSE
                         IF    WRK-TNS-GAITENTTLKBN(IDX3)   =    130
                           COMPUTE   RECE41-TENSU(5)  =
                                     RECE41-TENSU(5)  +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                         ELSE  
                           COMPUTE   RECE41-TENSU(31)  =
                                     RECE41-TENSU(31)  +
                                    (WK-SYUTEN   *   WRK-ZAIKAISU-XXX)
                         END-IF
                    END-IF
                    COMPUTE   RECE41-TENSU(32)  =
                              RECE41-TENSU(32)  +
                             (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
           END-EVALUATE
      *
           .
      *
       2101222-RECE-LEFT-SET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷
      *****************************************************************
       2101223-RECE-LEFT-SET3-SEC               SECTION.
      *
           EVALUATE    SRY-SRYKBN
      *    ½é¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "11"
                   IF   WRK-TNS-DATAKBN(IDX3)  =   2
                     EVALUATE    WRK-TNS-TIMEKSNKBN(IDX3)
                         WHEN    1
                         WHEN    5
                             MOVE  "1"     TO   RECE41-SHOSHINTIME
                         WHEN    3
                             MOVE  "2"     TO   RECE41-SHOSHINTIME
                         WHEN    4
                             MOVE  "3"     TO   RECE41-SHOSHINTIME
                     END-EVALUATE
                     COMPUTE    RECE41-TENSU(1)  =
                                RECE41-TENSU(1)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     IF  SRY-SRYCD(IDX3)  =  CONS-NYUKSN1  OR
                                             CONS-NYUKSN2
                             MOVE  "1"     TO   RECE41-SHOSHIN-NYU
                     END-IF
                   END-IF
                   IF   SRY-SRYCD(IDX3) = CONS-ROUSAI1    
                        COMPUTE    RECE41-KINGAKU2(1)  =
                                   RECE41-KINGAKU2(1)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF  
                   IF   SRY-SRYCD(IDX3) = CONS-ROUSAI2 OR
                                          CONS-ROUSAI3    
                        COMPUTE    RECE41-KINGAKU2(4)  =
                                   RECE41-KINGAKU2(4)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF  
      *    ºÆ¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸¡Ê½¸·×ÊýË¡ÊÑ¹¹¸å¡Ë
               WHEN    "12"
                 EVALUATE  WRK-TNS-GAITENTTLKBN(IDX3)
      *      ¡Ê´ðËÜ¼êµ»¡Ë
                   WHEN  120
                     IF   SRY-SRYCD(IDX3) =  CONS-ROUSAI5  OR
                                             CONS-ROUSAI6  OR
                                             CONS-ROUSAI7  
                         ADD   WK-ZAIKAISU      TO   RECE41-KAISU2(2)
                         COMPUTE    RECE41-KINGAKU2(2)  =
                                    RECE41-KINGAKU2(2)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     ELSE
                         COMPUTE    RECE41-TENSU(7)  =
                                    RECE41-TENSU(7)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     END-IF
      *
      *      ¡Ê³°Íè´ÉÍý²Ã»»¡Ë
                   WHEN  122
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(2)
                        COMPUTE    RECE41-TENSU(2)  =
                                   RECE41-TENSU(2)  +
                                  (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      ¡Ê»þ´Ö³°¡Ë
                   WHEN  123
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(3)
                        COMPUTE RECE41-TENSU(3) =
                                RECE41-TENSU(3) +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      ¡ÊµÙÆü¡Ë
                   WHEN  124
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(4)
                        COMPUTE  RECE41-TENSU(4) =
                                 RECE41-TENSU(4) +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *       ¡Ê¿¼Ìë¡Ë
                   WHEN  125
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(5)
                        COMPUTE  RECE41-TENSU(5) =
                                 RECE41-TENSU(5) +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *       ¡Ê¤½¤ÎÂ¾¡Ë
                   WHEN  OTHER 
                        IF   SRY-SRYCD(IDX3) =  CONS-ROUSAI4  
                           ADD   WK-ZAIKAISU TO   RECE41-KAISU2(3)
                           COMPUTE    RECE41-KINGAKU2(3)  =
                                      RECE41-KINGAKU2(3)  +
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        END-IF
                 END-EVALUATE
      *    »ØÆ³¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "13"
                   IF   SRY-SRYCD(IDX3) =   CONS-ROUSAI8  OR
                                            CONS-ROUSAI9  OR
                                            CONS-ROUSAI10 OR
                                            CONS-ROUSAI11 OR
                                            CONS-ROUSAI12 OR
                                            CONS-ROUSAI13 OR
                                            CONS-ROUSAI14 OR
                                            CONS-ROUSAI15 OR
                                            CONS-ROUSAI16 OR
                                            CONS-ROUSAI17 
                         COMPUTE    RECE41-KINGAKU2(6)  =
                                    RECE41-KINGAKU2(6)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   ELSE
                         COMPUTE    RECE41-TENSU(6)  =
                                    RECE41-TENSU(6)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF                   
      *
      *    ºßÂð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "14"
                   MOVE      ZERO            TO   FLG-ZAITAKU
      *            ±ý¿Ç¡¦ÆÃÊÌ±ý¿Ç¡Ê»þ´Ö²Ã»»¡Ë
                   IF    FLG-OSINKBN         =   1
                       MOVE         1        TO   FLG-ZAITAKU
                       COMPUTE   RECE41-TENSU(7)  =
                                 RECE41-TENSU(7)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            Ìë´Ö
                   IF    FLG-OSINKBN         =    2
                       MOVE   1              TO   FLG-ZAITAKU
                       COMPUTE   RECE41-TENSU(7)  =
                                 RECE41-TENSU(7)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            ¿¼Ìë¡¦¶ÛµÞ
                   IF    FLG-OSINKBN         =    3
                       MOVE   1              TO   FLG-ZAITAKU
                       COMPUTE   RECE41-TENSU(7)  =
                                 RECE41-TENSU(7)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅ
                   IF  ( WRK-TNS-OSINKBN(IDX3) =     1          )  AND
                       ((SRY-SRYCD(IDX3)   NOT =    "114000110")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001610")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000370")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002070"))
                       MOVE   1              TO   FLG-ZAITAKU
                       COMPUTE   RECE41-TENSU(7)  =
                                 RECE41-TENSU(7)  +
                                (WK-SYUTEN         * ACCT-ZAIKAISU)
                   END-IF
      *            ¤½¤ÎÂ¾
                   IF    FLG-ZAITAKU    =   ZERO
                       COMPUTE   RECE41-TENSU(7)  =
                                 RECE41-TENSU(7)  +
                                (WK-SYUTEN     *   WK-ZAIKAISU)
                   END-IF
      *            ÌôºÞ
                   COMPUTE   RECE41-TENSU-YAK(7)  =
                             RECE41-TENSU-YAK(7)  +
                             (WK-KIZAITEN *   WK-ZAIKAISU)  +
                             (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ÅêÌô¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                     IF   SRY-SRYCD(IDX3)         =    CONS-YAKTEIGEN
                       COMPUTE   RECE41-TENSU-YAK(9)  =
                                 RECE41-TENSU-YAK(9)  -
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                       ADD   WK-ZAIKAISU       TO   RECE41-KAISU(9)
                       COMPUTE   RECE41-TENSU-YAK(9)  =
                                 RECE41-TENSU-YAK(9)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)   =    CONS-FUNSAI-COMMENT
                       ADD   WK-ZAIKAISU       TO   RECE41-KAISU(9)
                       COMPUTE   RECE41-TENSU-YAK(9)  =
                                 RECE41-TENSU-YAK(9)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE   
                       IF    RECE41-KHNTEN(10)   =   ZERO
                          MOVE  WRK-TNS-TEN(IDX3)  TO  RECE41-KHNTEN(10)
                       ELSE
                          IF   RECE41-KHNTEN(10) NOT = WRK-TNS-TEN(IDX3)
                               MOVE   ZERO    TO    RECE41-KHNTEN(10)
                               MOVE   1       TO    FLG-KHNTEN10-JI
                          END-IF
                       END-IF
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(10)
                       COMPUTE   RECE41-TENSU(10)  =
                                 RECE41-TENSU(10)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     END-IF   
                   END-IF
               WHEN    "22"
                   ADD    WK-ZAIKAISU    TO   RECE41-KAISU(11)
                   COMPUTE   RECE41-TENSU-YAK(11)  =
                             RECE41-TENSU-YAK(11)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
               WHEN    "23"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(12)
                       COMPUTE   RECE41-TENSU-YAK(12)  =
                                 RECE41-TENSU-YAK(12)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   ELSE
                       IF    RECE41-KHNTEN(13)   =   ZERO
                          MOVE  WRK-TNS-TEN(IDX3)  TO  RECE41-KHNTEN(13)
                       ELSE
                          IF   RECE41-KHNTEN(13) NOT = WRK-TNS-TEN(IDX3)
                               MOVE   ZERO    TO    RECE41-KHNTEN(13)
                               MOVE   1       TO    FLG-KHNTEN13-JI
                          END-IF
                       END-IF
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(13)
                       COMPUTE   RECE41-TENSU(13)  =
                                 RECE41-TENSU(13)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF
      *    ½èÊý¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "25"
                   IF SRY-SRYCD(IDX3)   =    "120002270"
                      COMPUTE  WRK-TOKUTEI-SKAN  = WRK-TOKUTEI-SKAN +
                                                   WK-ZAIKAISU 
                   END-IF
                   IF SRY-SRYCD(IDX3)   =    "120002170"
                      COMPUTE  WRK-NYUYOUJI-SKAN = WRK-NYUYOUJI-SKAN +
                                                   WK-ZAIKAISU 
                   END-IF
                   IF SRY-SRYCD(IDX3)   =    "120003170"                
                      COMPUTE  WRK-CYOKI-SKAN  =  WRK-CYOKI-SKAN +      
                                                  WK-ZAIKAISU           
                   END-IF                                               
                   IF    RECE41-KHNTEN(14)   =   ZERO
                      MOVE  WRK-TNS-TEN(IDX3)  TO  RECE41-KHNTEN(14)
                   ELSE
                      IF   RECE41-KHNTEN(14) NOT = WRK-TNS-TEN(IDX3)
                           MOVE   ZERO    TO    RECE41-KHNTEN(14)
                           MOVE   1       TO    FLG-KHNTEN14-JI
                      END-IF
                   END-IF
                   ADD    WK-ZAIKAISU     TO    RECE41-KAISU(14)
                   COMPUTE   RECE41-TENSU(14)  =
                             RECE41-TENSU(14)  +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *    ËãÆÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "26"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(15)
                   IF SRY-SRYCD(IDX3)      =    "120001310"
                    COMPUTE WRK-MADOKU-SHOHO = WRK-MADOKU-SHOHO +
                                               WK-ZAIKAISU 
                   END-IF
                   COMPUTE   RECE41-TENSU(15)  =
                             RECE41-TENSU(15)  +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *    Ä´´ð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "27"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(16)
                   COMPUTE   RECE41-TENSU(16)  =
                             RECE41-TENSU(16)  +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *    Èé²¼¶ÚÆùÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "31"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(19)
                   COMPUTE   RECE41-TENSU(19)  =
                             RECE41-TENSU(19)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU-YAK(22)  =
                             RECE41-TENSU-YAK(22)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ÀÅÌ®ÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "32"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(20)
                   COMPUTE   RECE41-TENSU(20)  =
                             RECE41-TENSU(20)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU-YAK(22)  =
                             RECE41-TENSU-YAK(22)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾Ãí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "33"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(21)
                   COMPUTE   RECE41-TENSU(21)  =
                             RECE41-TENSU(21)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU-YAK(22)  =
                             RECE41-TENSU-YAK(22)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ½èÃÖ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(25)
                   END-IF
                   COMPUTE   RECE41-TENSU(25)  =
                             RECE41-TENSU(25)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU-YAK(26)  =
                             RECE41-TENSU-YAK(26)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¼ê½Ñ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(27)
                   END-IF
                   COMPUTE   RECE41-TENSU(27)  =
                             RECE41-TENSU(27)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU-YAK(28)  =
                             RECE41-TENSU-YAK(28)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¸¡ºº¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "60"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(29)
                   END-IF
                   COMPUTE   RECE41-TENSU(29)  =
                             RECE41-TENSU(29)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    WRK-TNS-KNSJISKBN(IDX3)    =   2
                         COMPUTE   RECE41-KAISU(29)  =
                                   RECE41-KAISU(29)  -  
                                   WK-ZAIKAISU
                   END-IF
                   IF    WRK-TNS-SRYCD(IDX3)  =   CONS-SAIKETSU1  OR
                                                  CONS-SAIKETSU2  OR
                                                  CONS-SAIKETSU3
                         IF   RECE41-KAISU(29)  NOT =  ZERO
                              COMPUTE   RECE41-KAISU(29)  =
                                        RECE41-KAISU(29)  -   
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE41-TENSU-YAK(30)  =
                             RECE41-TENSU-YAK(30)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ²èÁü¿ÇÃÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(31)
                   END-IF
                   COMPUTE   RECE41-TENSU(31)  =
                             RECE41-TENSU(31)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU-YAK(32)  =
                             RECE41-TENSU-YAK(32)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "80"
                   IF ( ACCT-SRYKBN          =  "80"    )   AND
                      ( SRY-SRYCD(IDX3)(1:5) =  "09591" OR 
                                                "09592" )
                        CONTINUE
                   ELSE
                    IF ( ACCT-SRYKBN          =  "80"    )  AND         
                       ( SRY-SRYCD(IDX3)(1:5) =  "09593" )              
                       COMPUTE    RECE41-KINGAKU2(6)  =                 
                                  RECE41-KINGAKU2(6)  +                 
                                 (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)  
                    ELSE                                                
                     IF  SRY-SRYCD(IDX3)      =    CONS-RYOYOTEATE
                       COMPUTE    RECE41-KINGAKU2(6)  =                 
                                  RECE41-KINGAKU2(6)  +                 
                                 (WRK-RYOYOTEATE  *   ACCT-ZAIKAISU)    
                     ELSE
                      IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN2
                       COMPUTE  WRK-TOKUTEI-SKAN2 =  WRK-TOKUTEI-SKAN2
                                                  +  WK-ZAIKAISU 
                      END-IF
                      IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN1
                       COMPUTE  WRK-NYUYOUJI-SHOKAN = 
                                                    WRK-NYUYOUJI-SHOKAN
                                                  + WK-ZAIKAISU 
                      END-IF
                      IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN3       
                       COMPUTE  WRK-CYOKI-SKAN2  =  WRK-CYOKI-SKAN2     
                                                 +  WK-ZAIKAISU         
                      END-IF                                            
                      IF   SRY-SRYCD(IDX3)    =    CONS-SHOHOSEN1 OR
                                                   CONS-SHOHOSEN2 OR
                                                   CONS-SHOHOSEN3 OR
                                                   CONS-SHOHOSEN4 OR
                                                   CONS-SHOHOKSN1 OR
                                                   CONS-SHOHOKSN2 OR
                                                   CONS-SHOHOKSN3       
                          ADD    WK-ZAIKAISU  TO  RECE41-KAISU(33)
                          COMPUTE   RECE41-TENSU(33)  =
                                    RECE41-TENSU(33)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                      ELSE
                          IF    WRK-TNS-GAITENTTLKBN(IDX3)   =    130
                            COMPUTE   RECE41-TENSU(6)  =
                                      RECE41-TENSU(6)  +
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                          ELSE  
                            IF    WK-SYUTEN       NOT =   ZERO
                                ADD  WK-ZAIKAISU  TO  RECE41-KAISU(25)
                            END-IF
                            COMPUTE   RECE41-TENSU(25)  =
                                      RECE41-TENSU(25)  +
                                     (WK-SYUTEN   *   ACCT-ZAIKAISU)
                          END-IF
                      END-IF
                      COMPUTE   RECE41-TENSU-YAK(26)  =
                                RECE41-TENSU-YAK(26)  +
                               (WK-YKZTEN   *   WK-ZAIKAISU)
                     END-IF
                    END-IF                                              
                   END-IF
           END-EVALUATE
      *
           .
      *
       2101223-RECE-LEFT-SET3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿½ÐÎÏ½èÍý
      *****************************************************************
       210123-RECE45-WRITE-SEC                SECTION.
      *
           MOVE    RECE45-REC          TO   RECE4B-REC      
      *
           WRITE   RECE45-REC
           ADD     1                   TO   CNT-OUT2
      *
           WRITE   RECE4B-REC
           ADD     1                   TO   CNT-OUT5
      *
           .
       210123-RECE45-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿½ÐÎÏ½èÍý
      *****************************************************************
       210124-RECE45-REWRITE-SEC              SECTION.                        
      *
           MOVE    ZERO                TO  FLG-REWRITE
      *
           INITIALIZE                      RECE45-REC
           MOVE    RECE44-HOSPID       TO  RECE45-HOSPID
           MOVE    RECE44-SRYYM        TO  RECE45-SRYYM
           MOVE    RECE44-NYUGAIKBN    TO  RECE45-NYUGAIKBN
           MOVE    RECE44-PTID         TO  RECE45-PTID
           MOVE    RECE44-RECEKA       TO  RECE45-RECEKA
           MOVE    RECE44-HKNID        TO  RECE45-HKNID
           MOVE    RECE44-TEKSTYMD     TO  RECE45-TEKSTYMD
           MOVE    RECE44-ZAINUM       TO  RECE45-ZAINUM
           START   RECE45-FILE
                 KEY IS    >=   RECE45-KEY
                 INVALID
                     MOVE    1         TO  FLG-RECE45
                 NOT INVALID
                     MOVE    ZERO      TO  FLG-RECE45
                     READ    RECE45-FILE   NEXT
                       AT  END
                         MOVE    1     TO  FLG-RECE45
                     END-READ
           END-START
           PERFORM   UNTIL
               (RECE44-HOSPID       NOT =  RECE45-HOSPID)      OR
               (RECE44-SRYYM        NOT =  RECE45-SRYYM)       OR
               (RECE44-NYUGAIKBN    NOT =  RECE45-NYUGAIKBN)   OR
               (RECE44-PTID         NOT =  RECE45-PTID)        OR
               (RECE44-RECEKA       NOT =  RECE45-RECEKA)      OR
               (RECE44-HKNID        NOT =  RECE45-HKNID)       OR
               (RECE44-TEKSTYMD     NOT =  RECE45-TEKSTYMD)    OR
               (RECE44-ZAINUM       NOT =  RECE45-ZAINUM)      OR
               (FLG-RECE45                =      1        )    OR
               (FLG-REWRITE               =      1        )
                IF    RECE45-RENNUM   =   WRK-RENNUM
                   MOVE    1              TO  FLG-REWRITE
                   MOVE    WRK-SRYSURYO3  TO  RECE45-SRYSURYO(5)
                   REWRITE   RECE45-REC
                END-IF
                READ    RECE45-FILE   NEXT
                  AT  END
                    MOVE    1     TO  FLG-RECE45
                END-READ
           END-PERFORM
      *
           .
       210124-RECE45-REWRITE-EXT.
           EXIT.                                                       
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿½ÐÎÏ½èÍý
      *****************************************************************
       21013-RECE44-WRITE-SEC                SECTION.
      *
           MOVE    RECE44-REC          TO  RECE4A-REC
      *
           COMPUTE  RECE44-ZAITEN      =  RECE44-ZAITEN     -  
                                          WRK-TEN
           COMPUTE  RECE44-MEISAISU    =  RECE44-MEISAISU   -
                                          WRK-MEISAI-CNT
           COMPUTE  RECE44-SYUTEN1     =  RECE44-SYUTEN1    -
                                          WRK-TEN
           COMPUTE  RECE44-SRYCDTOTAL  =  RECE44-SRYCDTOTAL -
                                          WRK-SRYCDTOTAL
           COMPUTE  RECE44-SURYOUTOTAL =  RECE44-SURYOUTOTAL -
                                          WRK-SURYOUTOTAL     
      *
           MOVE     WRK-TEN         TO  RECE4A-ZAITEN
           MOVE     WRK-MEISAI-CNT  TO  RECE4A-MEISAISU
           MOVE     WRK-TEN         TO  RECE4A-SYUTEN1  
           MOVE     WRK-SRYCDTOTAL  TO  RECE4A-SRYCDTOTAL
           MOVE     WRK-SURYOUTOTAL TO  RECE4A-SURYOUTOTAL
      *
           WRITE   RECE44-REC
           ADD     1                TO  CNT-OUT1
      *
           WRITE   RECE4A-REC
           ADD     1                TO  CNT-OUT4
      *
           .
       21013-RECE44-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºÞ¤Î¿ÇÎÅÆâÍÆ¥Á¥§¥Ã¥¯
      *****************************************************************
       21014-SINRYO-CHECK-SEC                 SECTION.
      *
           PERFORM   VARYING   IDU   FROM   1   BY   1
                      UNTIL  ( IDU   >   TBL-ZAICHECK-MAX )
                       OR    ( FLG-RINYAK  NOT =  ZERO    )
      *
              IF  ( IDU  =  1                          )
              OR  ((IDU  =  2) AND (ACCT-SRYKBN = "60"))
              OR  ((IDU  =  3) AND (ACCT-SRYKBN = "80"))
              OR  ((IDU  =  4) AND (ACCT-SRYKBN = "80"))
      *
                 INITIALIZE                  SRYACT-REC
                 MOVE RECE41-HOSPID      TO  SRY-HOSPID
                 MOVE RECE41-NYUGAIKBN   TO  SRY-NYUGAIKBN
                 MOVE RECE41-PTID        TO  SRY-PTID
                 MOVE RECE41-SRYYM       TO  SRY-SRYYM
                 MOVE ACCT-ZAINUM        TO  SRY-ZAINUM
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(1)
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(2)
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(3)
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(4)
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(5)
                 MOVE SRYACT-REC         TO  MCPDATA-REC
                 MOVE "DBSELECT"         TO  MCP-FUNC
                 MOVE "SRYACT-KEY8"      TO  ORC-DBPATH
                 CALL "ORCMCPSUB"        USING  MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
                 IF      MCP-RC        =  ZERO
                     MOVE  "SRYACT-KEY8" TO  ORC-DBPATH 
                     PERFORM   800-SRYACT-READ-SEC
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
                 MOVE    "SRYACT-KEY8" TO  WK-PATH
                 PERFORM   CSR-CLOSE-SEC
      *
              END-IF
      *
           END-PERFORM
      *
           .
       21014-SINRYO-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î´ðËÜÅÀ¿ôÉôÊ¬¤Î¥¯¥ê¥¢½èÍý
      *****************************************************************
       2102-KHNTEN-CLEAR-SEC               SECTION.
      *
           IF        FLG-KHNTEN2        =    1
               MOVE    ZERO             TO   RECE41-KHNTEN(2)
           END-IF
           IF        FLG-KHNTEN3        =    1
               MOVE    ZERO             TO   RECE41-KHNTEN(3)
           END-IF
           IF        FLG-KHNTEN4        =    1
               MOVE    ZERO             TO   RECE41-KHNTEN(4)
           END-IF
           IF        FLG-KHNTEN5        =    1
               MOVE    ZERO             TO   RECE41-KHNTEN(5)
           END-IF
           IF        FLG-KHNTEN6        =    1
               MOVE    ZERO             TO   RECE41-KHNTEN(6)
           END-IF
           IF        FLG-KHNTEN15        =    1
               MOVE    ZERO             TO   RECE41-KHNTEN(15)
           END-IF
           IF        FLG-KHNTEN18        =    1
               MOVE    ZERO             TO   RECE41-KHNTEN(18)
           END-IF
           IF        FLG-KHNTEN19        =    1
               MOVE    ZERO             TO   RECE41-KHNTEN(19)
           END-IF
           .
       2102-KHNTEN-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î´ðËÜÅÀ¿ôÉôÊ¬¤Î¥¯¥ê¥¢½èÍý¡Ê¼«ÇåÀÕ¡Ë
      *****************************************************************
       2103-KHNTEN-JI-CLEAR-SEC               SECTION.
      *
           IF        FLG-KHNTEN10-JI    =    1
               MOVE    ZERO             TO   RECE41-KHNTEN(10)
           END-IF
           IF        FLG-KHNTEN13-JI    =    1
               MOVE    ZERO             TO   RECE41-KHNTEN(13)
           END-IF
           IF        FLG-KHNTEN14-JI    =    1
               MOVE    ZERO             TO   RECE41-KHNTEN(14)
           END-IF
           .
       2103-KHNTEN-JI-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®
      *****************************************************************
       2104-RECE44-HENSYU2-SEC                SECTION.
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE41-HOSPID        TO      ACCT-HOSPID
           MOVE    RECE41-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE41-PTID          TO      ACCT-PTID
           MOVE    RECE41-SRYKA(IDX)    TO      ACCT-SRYKA
           MOVE    RECE41-SRYYM         TO      ACCT-SRYYM
           MOVE    RECE41-HKNCOMBI      TO      ACCT-HKNCOMBI
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "SRYACCT-KEY24"      TO      ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "SRYACCT-KEY24"  TO      ORC-DBPATH
               PERFORM  800-SRYACCT-READ-SEC
               PERFORM  UNTIL     FLG-SRYACCT   =   1 
      *
                   MOVE   ZERO          TO  FLG-RINYAK
      *
                   MOVE   RECE41-TEKSTYMD(7:2)   TO   WRK-DAY
                   IF  ACCT-DAY(1 WRK-DAY)   NOT =    ZERO
                        MOVE    1       TO   FLG-DAY
                   ELSE
                        MOVE    ZERO    TO   FLG-DAY  
                   END-IF
      *
                   IF      FLG-DAY         =   1
                           IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                                (ACCT-ZAIKAISU  NOT =    ZERO)
                               MOVE  ACCT-DAY(1 WRK-DAY)  
                                               TO    WRK-ZAIKAISU-XXX
                               MOVE    ZERO    TO    WRK-TEN
                                                     WRK-MEISAI-CNT
                                                     WRK-SRYCDTOTAL
                                                     WRK-SURYOUTOTAL
                                                     WRK-YOUBUKASAN
                                                     WRK-KYOBUKASAN
                                                     WRK-SOGAIKASAN
                                                     WRK-RYOYOTEATE
      *
                               MOVE    ZERO    TO    WRK-SRYSURYO
                                                     WRK-SRYSURYO2
                                                     WRK-SRYSURYO3
                                                     WRK-TANICD
                                                     WRK-TANIMOJISU
                               MOVE    SPACE   TO    WRK-TANINAME
                               MOVE    ZERO    TO    FLG-HOUSIJI-COM
                                                     FLG-HOUTEN-COM
      *
                               MOVE    SPACE   TO    WRK-KYUGYOFLG
      *
                               PERFORM   21011-RECE44-TENSOU-SEC
                               PERFORM   21012-RECE45-HENSYU-SEC
                               PERFORM   21013-RECE44-WRITE-SEC
                           ELSE
                             IF   ACCT-ZAIKAISU  NOT =    ZERO
                               IF   ACCT-SRYKBN  =  "11"  OR  "12" 
                                                          OR  "99" 
                                 IF  ACCT-SRYCDTOTAL = CONS-TOKKI1  OR
                                                       CONS-TOKKI2  OR
                                                       CONS-TOKKI3  OR
                                                       CONS-TOKKI4  OR
                                                       CONS-TOKKI5  OR
                                                       CONS-TOKKI7  OR
                                                       CONS-TOKKI8  OR
                                                       CONS-TOKKI9  OR
                                                       CONS-TOKKI10 OR
                                                       CONS-TOKKI11 OR
                                                       CONS-TOKKI12 OR
                                                       CONS-TOKKI13 OR
                                                       CONS-TOKKI14
                                      CONTINUE
                                 ELSE
                                      MOVE  ACCT-DAY(1 WRK-DAY)  
                                                  TO  WRK-ZAIKAISU-XXX
                                      MOVE  ZERO  TO  WRK-TEN
                                                      WRK-MEISAI-CNT
                                                      WRK-SRYCDTOTAL
                                                      WRK-SURYOUTOTAL
                                                      WRK-YOUBUKASAN
                                                      WRK-KYOBUKASAN
                                                      WRK-SOGAIKASAN
                                                      WRK-RYOYOTEATE
      *
                                      MOVE  ZERO  TO  WRK-SRYSURYO
                                                      WRK-SRYSURYO2
                                                      WRK-SRYSURYO3
                                                      WRK-TANICD
                                                      WRK-TANIMOJISU
                                      MOVE  SPACE TO  WRK-TANINAME
                                      MOVE  ZERO  TO  FLG-HOUSIJI-COM
                                                      FLG-HOUTEN-COM
      *
                                      MOVE  SPACE TO  WRK-KYUGYOFLG
      *
                                      PERFORM   21011-RECE44-TENSOU-SEC
                                      PERFORM   21012-RECE45-HENSYU-SEC
                                      PERFORM   21013-RECE44-WRITE-SEC
                                 END-IF
                               ELSE
                                 PERFORM   21014-SINRYO-CHECK-SEC
                                 IF   FLG-RINYAK  NOT =  ZERO
                                      MOVE  ACCT-DAY(1 WRK-DAY)  
                                                  TO  WRK-ZAIKAISU-XXX
                                      MOVE  ZERO  TO  WRK-TEN
                                                      WRK-MEISAI-CNT
                                                      WRK-SRYCDTOTAL
                                                      WRK-SURYOUTOTAL
                                                      WRK-YOUBUKASAN
                                                      WRK-KYOBUKASAN
                                                      WRK-SOGAIKASAN
                                                      WRK-RYOYOTEATE
      *
                                      MOVE  ZERO  TO  WRK-SRYSURYO
                                                      WRK-SRYSURYO2
                                                      WRK-SRYSURYO3
                                                      WRK-TANICD
                                                      WRK-TANIMOJISU
                                      MOVE  SPACE TO  WRK-TANINAME
                                      MOVE  ZERO  TO  FLG-HOUSIJI-COM
                                                      FLG-HOUTEN-COM
      *
                                      MOVE  SPACE TO  WRK-KYUGYOFLG
      *
                                      PERFORM   21011-RECE44-TENSOU-SEC
                                      PERFORM   21012-RECE45-HENSYU-SEC
                                      PERFORM   21013-RECE44-WRITE-SEC
                                 END-IF
                               END-IF
                             END-IF
                           END-IF
                   END-IF
                   MOVE     "SRYACCT-KEY24"  TO   ORC-DBPATH
                   PERFORM   800-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE   "SRYACCT-KEY24"     TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
           .
       2104-RECE44-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿¹¹¿·½èÍý
      *****************************************************************
       2105-RECE41-REWRITE-SEC                SECTION.
      *
           IF   RECE41-HKNKBN     =   "4"                               
      *
               COMPUTE   RECE41-TENSU(8)      =  RECE41-TENSU(1)        
                                              +  RECE41-TENSU(2)        
                                              +  RECE41-TENSU(3)        
                                              +  RECE41-TENSU(4)        
                                              +  RECE41-TENSU(5)        
                                              +  RECE41-TENSU(6)        
                                              +  RECE41-TENSU(7)        
      *
               COMPUTE   RECE41-TENSU-YAK(8)  =  RECE41-TENSU-YAK(7)    
      *
               COMPUTE   RECE41-TENSU(17)     =  RECE41-TENSU(10)       
                                              +  RECE41-TENSU(13)       
                                              +  RECE41-TENSU(14)       
                                              +  RECE41-TENSU(15)       
                                              +  RECE41-TENSU(16)       
      *
               COMPUTE   RECE41-TENSU-YAK(18) =  RECE41-TENSU-YAK(9)    
                                              +  RECE41-TENSU-YAK(11)   
                                              +  RECE41-TENSU-YAK(12)   
      *
               COMPUTE   RECE41-TENSU(23)     =  RECE41-TENSU(19)       
                                              +  RECE41-TENSU(20)       
                                              +  RECE41-TENSU(21)       
      *
               COMPUTE   RECE41-TENSU-YAK(24) =  RECE41-TENSU-YAK(22)   
      *
               COMPUTE   WRK-KSNRATE          =  RECE41-KSNRATE / 100   
      *
      ***      PERFORM    VARYING    ID1    FROM    1   BY   1          
      ***                 UNTIL      ID1    >     31                    
      *              
      ***         ADD   RECE41-SYUTEN-JI(ID1)  TO  RECE41-TOTALTEN
      ***         ADD   RECE41-YKZTEN-JI(ID1)  TO  RECE41-TOTALTEN-YAK
      *
      ***         COMPUTE   WRK-KEISAN        =  RECE41-SYUTEN-JI(ID1)  
      ***                                     *  RECE41-TANKA           
      ***         COMPUTE   RECE41-TOTALKIN   =  RECE41-TOTALKIN  +
      ***                              ( WRK-KEISAN  *  WRK-KSNRATE )   
      *
      ***         COMPUTE   WRK-KEISAN        =  RECE41-YKZTEN-JI(ID1)  
      ***                                     *  RECE41-TANKA           
      ***         COMPUTE   RECE41-TOTALKIN-YAK =  RECE41-TOTALKIN-YAK
      ***                                       +  WRK-KEISAN           
      *
      ***      END-PERFORM                                              
      *
               PERFORM    VARYING    ID1    FROM    1   BY   1          
                       UNTIL      ID1    >     31                    
      *              
                  ADD   RECE41-SYUTEN-JI(ID1)  TO  RECE41-TOTALTEN
                  ADD   RECE41-YKZTEN-JI(ID1)  TO  RECE41-TOTALTEN-YAK
      *
               END-PERFORM
      *
               COMPUTE   RECE41-TOTALKIN   =  
                  (RECE41-TOTALTEN  *  RECE41-TANKA  *  WRK-KSNRATE) 
                                                  +  0.5
      *
               COMPUTE   RECE41-TOTALKIN-YAK =  
                  (RECE41-TOTALTEN-YAK  *  RECE41-TANKA)  
                                                  +  0.5
      *
               COMPUTE   RECE41-KINGAKU2(5)   =  RECE41-KINGAKU2(1)     
                                              +  RECE41-KINGAKU2(2)     
                                              +  RECE41-KINGAKU2(3)     
                                              +  RECE41-KINGAKU2(4)     
      *
      ***      COMPUTE   RECE41-TOTALKIN2     =  RECE41-KINGAKU2(5)     
      ***                                     *  WRK-KSNRATE            
      *
               COMPUTE   RECE41-TOTALKIN2     =  
                  (RECE41-KINGAKU2(5)   *  WRK-KSNRATE)  +  0.5         
      *
               COMPUTE   RECE41-TOTALKIN2-ETC =  RECE41-KINGAKU2(6)     
                                              +  RECE41-SINDAN-KIN      
                                              +  RECE41-MEISAI-KIN      
      * 
           END-IF                                                       
      *
           REWRITE   RECE41-REC
      *
           ADD     1                   TO  CNT-OUT3
      *
           .
       2105-RECE41-REWRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½ªÎ»½èÍý
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE41-FILE
                   RECE44-FILE
                   RECE45-FILE
                   RECE4A-FILE
                   RECE4B-FILE
      *
      *    ¥¹¥Æ¥Ã¥×´ÉÍý½ªÎ»½èÍý
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "RECE41-IN  :" CNT-IN   
           DISPLAY "RECE41-MOD :" CNT-OUT3  
           DISPLAY "RECE44-OUT :" CNT-OUT1 
           DISPLAY "RECE45-OUT :" CNT-OUT2  
           DISPLAY "RECE4A-OUT :" CNT-OUT4 
           DISPLAY "RECE4B-OUT :" CNT-OUT5  
           
           DISPLAY "*** ORCR0420 END ***"
      * 
           .
       300-TERM-EXT.
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
      *        ¥¸¥ç¥Ö´ÉÍý³«»Ï½èÍý
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½èÍýÃæ»ß½èÍý
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
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
      *            ¥¸¥ç¥Ö´ÉÍý½ªÎ»½èÍý
                   MOVE    "CAN"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
                   MOVE    WRK-PARA-SHELLID
                                           TO  JOB-SHELLID
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "8888"          TO  JOB-ERRCD
                   MOVE    2               TO  JOB-STOPFLG
                   CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA  
                                           JOBKANRI-REC
               END-IF
      *                             
               MOVE    1                   TO  FLG-END
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´ÉÍý¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-PATH            TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    WRK-PATH            TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    WRK-PATH            TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´ÉÍý¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       810-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       810-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ñ·×ÆÉ¹þ
      *****************************************************************
       800-SRYACCT-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       800-SRYACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°ÙÆÉ¹þ
      *****************************************************************
       800-SRYACT-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       800-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Æ¡¼¥Ö¥ë¤Î¸¡º÷
      *****************************************************************
       800-TENSU-TBL-SEC               SECTION.
      *
           INITIALIZE                           TNS-TENSU-REC
           MOVE    SRY-SRYCD(IDX2)      TO      TNS-SRYCD
           MOVE    TNS-TENSU-REC        TO      MCPDATA-REC
           MOVE    RECE45-SRYYM         TO      WK-TAIHI-SRYYM
           MOVE    WK-TAIHI-DAY         TO      WK-TAIHI-SRYD
           MOVE    WK-TAIHI-SRYYMD      TO      ORC-DBYMD
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "TENSU-KEY"          TO      ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "TENSU-KEY"      TO      ORC-DBPATH
               PERFORM  810-TENSU-READ-SEC
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           MOVE   "TENSU-KEY"      TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
           .
       800-TENSU-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ôÆÉ¹þ
      *****************************************************************
       810-TENSU-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               MOVE    ZERO            TO  FLG-TENSU
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           .
       810-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¥³¥á¥ó¥È¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       800-PTCOM-READ-SEC           SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTCOM-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "PTCOM-KEY"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "PTCOM-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       800-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ãæ´Ö¥Õ¥¡¥¤¥ë£Ò£Å£Á£Ä½èÍý
      *****************************************************************
       900-RECE41-READ-SEC            SECTION.
      *
           READ    RECE41-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-END
               NOT  AT  END
                   ADD     1     TO    CNT-IN
                   ADD     1     TO    CNT-IN-COMMIT
                                       CNT-IN-CANCEL
      *
                   DISPLAY "RECE41  PTNUM=" RECE41-PTNUM
      *
                   PERFORM 910-SYSKANRI-HENSYU-SEC
      *
           END-READ
           .
       900-RECE41-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¿ÇÎÅÇ¯·î¤ÎÂÐ¾Ý¤È¤Ê¤ë¥·¥¹´ÉÍý¤ò¼èÆÀ¡Ê¸ÄÊÌ½èÍý¤Î¤ß
      ****************************************************************
       910-SYSKANRI-HENSYU-SEC         SECTION.
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
             CONTINUE
           ELSE
      *      °åÎÅµ¡´Ø£É£ÄÊÔ½¸
             MOVE  ZERO           TO  FLG-SET
             MOVE  RECE41-SRYYM   TO  WRK-SYS-SRYYMD(1:6)
             MOVE  "01"           TO  WRK-SYS-SRYYMD(7:2)
             PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                   UNTIL  ( SYSKAN-IDX              >  10   )
                    OR    ( TBL-SYS1001(SYSKAN-IDX) = SPACE )
                    OR    ( FLG-SET                 =   1   )
      *
                MOVE  TBL-SYS1001(SYSKAN-IDX)  TO  SYS-1001-REC
                IF   ( SYS-1001-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                AND  ( WRK-SYS-SRYYMD     <=  SYS-1001-EDYUKYMD )
                   MOVE     1       TO    FLG-SET
                   MOVE    SYS-1001-TAX-HASU-KBN
                                    TO    WRK-TAX1001-KBN
                END-IF
      *
             END-PERFORM
             IF     FLG-SET  =  ZERO
      *
               STRING "°åÎÅµ¡´Ø¾ðÊó¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£¿ÇÎÅÇ¯·î¡á"
                                          DELIMITED  BY  SIZE 
                       RECE41-SRYYM       DELIMITED  BY  SIZE
                                      INTO    WRK-RECEERR
               END-STRING
      *
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1               TO  FLG-END
               GO  TO  910-SYSKANRI-HENSYU-EXT
             END-IF
      *
           END-IF
      *
           .
      *
       910-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¡¡¥ª¡¼¥×¥ó½èÍý
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¡¡¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥«¡¼¥½¥ë¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       CSR-CLOSE-SEC            SECTION.
      *
           INITIALIZE                  MCPAREA
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    WK-PATH             TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           .
       CSR-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¡¡£Ã£Ï£Í£Í£É£Ô½èÍý
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           CALL    "MCPSUB"          USING
                                        MCPAREA
                                        MCPDATA-REC.
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
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           .
       900-DBSTART-EXT.
           EXIT.
      *
