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
       PROGRAM-ID.             ORCR0660.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¥ì¥»¥×¥È¡ÊÆþ±¡¡Ë
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¥ì¥»¥×¥È¥³¥á¥ó¥ÈÊÔ½¸
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  02/12/13    NaCl-ÌçÏÆ     ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01    NACL-ÌçÏÆ    03.03.31  ÌôºÞ´ÉÍý»ØÆ³ÎÁ¤Î»»ÄêÆüÊÔ½¸
      *  01.00.02    NACL-ÌçÏÆ    03.04.04  Æþ±¡±ÉÍÜ¿©»ö»ØÆ³ÎÁ¤Î»»ÄêÆüÊÔ½¸
      *                                     Âà±¡Á°Ë¬Ìä»ØÆ³ÎÁ¤Î»»ÄêÆüÊÔ½¸
      *                                     Àº¿À²ÊÂà±¡Á°Ë¬Ìä»ØÆ³ÎÁ¤Î»»ÄêÆüÊÔ½¸
      *                                     ½ýÉÂ¼êÅö¶â°Õ¸«½ñ¸òÉÕÎÁ¤Î¸òÉÕÆüÊÔ½¸
      *                                     ÎÅÍÜÈñÆ±°Õ½ñ¸òÉÕÎÁ¤Î¸òÉÕÆüÊÔ½¸
      *  01.00.03    NACL-ÌçÏÆ    03.04.08  Âà±¡»þÉþÌô»ØÆ³²Ã»»»»Äê»þ¤Î
      *                                     Á°·îÌôºÞ´ÉÍý»ØÆ³ÎÁ¤Î»»ÄêÆüÊÔ½¸
      *  01.00.04    NACL-ÌçÏÆ    03.04.24  Í¢·ì¤Ï¥³¥á¥ó¥ÈÊÔ½¸ÂÐ¾Ý³°¤È¤¹¤ë
      *  01.00.05    NACL-ÌçÏÆ    03.05.28  ¼õ¿ÇÍúÎò¥­¡¼¤ÎÊÑ¹¹
      *  01.00.06    NACL-ÌçÏÆ    03.06.19  Ê£¿ô¿ÇÎÅ²Ê¼õ¿Ç¤ÎÊÔ½¸½¤Àµ
      *  01.00.07    NACL-ÌçÏÆ    03.08.20  ¸øÈñ¼õ¿ÇÆü¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *  01.00.08    NACL-ÌçÏÆ    03.09.24  ¥³¥á¥ó¥ÈÊÔ½¸½¤Àµ
      *  01.00.09    NACL-ÌçÏÆ    03.09.29  ¥³¥á¥ó¥ÈÊÔ½¸½¤Àµ
      *  01.00.10    NACL-ÌçÏÆ    03.12.15  ¼«Æ°ÊÔ½¸¥³¥á¥ó¥È¤Î¸øÈñÊ¬Î¥¶èÊ¬
      *                                     ¥»¥Ã¥È
      *  01.00.11    NACL-ÌçÏÆ    04.01.14  ¿ÇÎÅ¹Ô°Ù¤ÎÊñ³çÊ¬¤Ï½ü¤¯
      *                                     SRYACCT5¢ª31
      *  £²£°£°£´Ç¯£´·î²þÀµÂÐ±þ
      *  01.01.01    NACL-ÌçÏÆ    04.04.01  ¥ê¥Ï¸À¸ìÄ°³ÐÎÅË¡¡Ê£³¡ËÄÉ²ÃÂÐ±þ
      *  01.01.02    NACL-ÌçÏÆ    04.04.05  ºßÂð·ì±ÕÆ©ÀÏÉÑ²ó»ØÆ³´ÉÍýÎÁ¤Î
      *                                     ¥³¥á¥ó¥ÈÂÐ±þ
      *  01.01.03    NACL-ÌçÏÆ    04.04.15  Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆü¤Î¥³¥á¥ó¥È
      *                                     ÂÐ±þ
      *  01.01.04    NACL-ÌçÏÆ    04.06.07  ¥³¥Ô¡¼¶ç£°£¶£µÊÑ¹¹¤ËÈ¼¤¦½¤Àµ
      *  01.01.05    NACL-ÌçÏÆ    04.06.08  ÁªÄê³°Çñ¤Î¥³¥á¥ó¥ÈÂÐ±þ
      *  Ãæ»ß½èÍýÄÉ²Ã
      *  01.02.01    NACL-Æ£¸¶    04/10/07  Ãæ»ß½èÍý¤ÎÄÉ²Ã
      *  01.02.02    NACL-ÌçÏÆ    04/12/09  Ç¾·ì´É¼À´µµÞÀ­È¯¾É½ªÎ»ÆüÂÐ±þ
      *  01.02.03    NACL-ÌçÏÆ    05/01/18  »ÀÁÇ·×»»¼°¥³¥á¥ó¥ÈÂÐ±þ
      *
      *  02.06.01    NACL-ÌçÏÆ    05/05/10  Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÉÂÌ¾µ­ºÜÂÐ±þ
      *  02.06.02    NACL-ÃÝÅÄ    05/05/30  realtime preview ½èÍýÄÉ²Ã
      *  02.06.03    NACL-ÌçÏÆ    05/08/22  ¥ê¥Ï¥Ó¥ê¼Â»Ü²ó¿ôº¹¤·¹þ¤ß½¤ÀµÂÐ±þ
      *  02.06.04    NACL-ÌçÏÆ    05/08/31  ºÞÈÖ¹æÊÔ½¸½¤Àµ
      *  02.06.05    NACL-ÌçÏÆ    05/11/01  ¡Ê¸¡Áª¡ËÅù¥³¥á¥ó¥ÈÂÐ±þ
      *  02.06.06    NACL-ÌçÏÆ    05/11/15  ½ýÉÂ¼êÅö¶â¡¢ÎÅÍÜÈñÆ±°Õ½ñ¤Î¼«Æ°
      *                                     µ­ºÜÊÔ½¸½¤Àµ
      *  02.06.07    NACL-ÌçÏÆ    05/12/20  º¹¤·¹þ¤ß¥³¥á¥ó¥È¤ÎÊÝ¸±ÁÈ¹ç¤»ÊÔ½¸
      *  02.09.01    NACL-ÌçÏÆ    06/04/13  £²£°£°£¶Ç¯£´·î²þÀµÂÐ±þ
      *  02.09.02    NACL-ÌçÏÆ    06/04/28  ¥ê¥Ï¸¡º÷¾ò·ï½¤Àµ
      *  02.09.03    NACL-ÌçÏÆ    06/05/01  ¥ê¥ÏÈ¯¾ÉÆüÅö·îÊ¬µ­ºÜ½¤Àµ
      *  02.09.04    NACL-ÌçÏÆ    06/05/30  ¥ê¥ÏÈ¯¾ÉÆüµ­ºÜ½¤Àµ
      *  03.01.01    NACL-ÌçÏÆ    06/06/22  ±ý¿Ç¡¢ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ¤Î
      *                                     ¥³¥á¥ó¥È¼«Æ°µ­ºÜÂÐ±þ
      *  03.01.02    NACL-ÌçÏÆ    06/07/10  ºß»Ù±ç¥³¥á¥ó¥È¼«Æ°µ­ºÜÂÐ±þ
      *  03.02.01    NACL-ÌçÏÆ    06/08/01  ¥ê¥ÏÈ¯¾ÉÆüµ­ºÜ½¤Àµ
      *  03.03.01    NACL-ÌçÏÆ    06/10/19  ÎÅÍÜÉÂÅï¥ì¥»¥³¥á¥ó¥Èµ­ºÜÂÐ±þ
      *  03.04.01    NACL-ÌçÏÆ    06/12/21  ¥ì¥»¥×¥ÈÅÅ»»ÂÐ¾Ý³°¥Õ¥é¥°ÊÔ½¸
      *                                     ¡Ê¿ÇÎÅ¶èÊ¬£¹£°¡¢£¹£²¡Ë
      *  03.04.02    NACL-ÌçÏÆ    07/01/12  ¥ê¥Ï¥Ó¥êÈ¯¾É½ªÎ»ÆüÂÐ±þ
      *  03.04.03    NACL-ÌçÏÆ    07/01/15  ¥ê¥Ï¥Ó¥ê¼ê½ÑÆü¡¦µÞÀ­Áý°­Æüµ­ºÜÂÐ±þ
      *  03.04.04    NACL-ÌçÏÆ    07/02/19  ËÌ³¤Æ»£µ£±¡Ü£´£±ÂÐ±þ
      *  03.04.05    NACL-ÌçÏÆ    07/03/05  »ÀÁÇ·×»»¼°½¤Àµ
      *  03.04.06    NACL-ÌçÏÆ    07/04/13  ¥ê¥Ï¥Ó¥êÂÐ±þ
      *  03.05.01    NACL-¾®Æ¦Âô  07/06/04  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *  03.05.02    NACL-ÌçÏÆ    07/08/07  »»ÄêÍúÎò¤Ë´Ø¤ï¤ëµ­ºÜ½¤Àµ
      *  04.01.01    NACL-ÌçÏÆ    07/11/26  ÆÃÄê´ïºà¾¦ÉÊÌ¾µ­ºÜÂÐ±þ
      *  04.01.02    NACL-ÌçÏÆ    07/11/30  Âà±¡»þÉþÌô»ØÆ³²Ã»»»»ÄêÆüµ­ºÜÂÐ±þ
      *  04.01.03    NACL-ÌçÏÆ    08/01/15  ¸ø³²ÂÐ±þ
      *  04.02.01    NACL-ÌçÏÆ    08/03/24  Ê¿À®£²£°Ç¯£´·î²þÀµÂÐ±þ
      *                                     ¡ÊÌôºÞ´ÉÍý»ØÆ³ÎÁ¥³¡¼¥ÉÄÉ²Ã¡Ë
      *                                     ¡Ê¿ÇÎÅ¶èÊ¬£¶£´¡§ÉÂÍý¿ÇÃÇÂÐ±þ¡Ë
      *                                     ¡Êºß»ÙÉÂ¼«Æ°µ­ºÜÂÐ±þ¡Ë
      *  04.02.02    NACL-¾®Æ¦Âô  08/04/08  °åÎÅ¶èÊ¬¥³¥á¥ó¥È²þÄêÂÐ±þ
      *  04.02.03    NACL-ÌçÏÆ    08/05/07  Âà±¡»þÉþÌô»ØÆ³²Ã»»»»ÄêÆüµ­ºÜ½¤Àµ
      *
      *  04.03.01    NACL-ÌçÏÆ    08/08/19  Æþ±¡ÎÁÌ¤»»Äê»þ¤Î¥³¥á¥ó¥ÈÂÐ±þ
      *  04.03.02    NACL-ÌçÏÆ    08/11/04  ¥ê¥Ï¥Ó¥êÈ¯¾ÉÆüÇ¯·îÆüÂÐ±þ
      *  04.03.03    NACL-ÌçÏÆ    09/01/05  Ê¿À®£²£±Ç¯£±·î²þÀµÂÐ±þ
      *  04.03.04    NACL-ÌçÏÆ    09/03/10  ¾ã³²¤Î¥³¥á¥ó¥Èµ­ºÜ½¤Àµ
      *
      *  04.04.01    NACL-ÌçÏÆ    09/02/04  ¸¡ºº¤Î¥³¥á¥ó¥È¼«Æ°µ­ºÜÂÐ±þ
      *  04.04.02    NACL-ÌçÏÆ    09/03/11  ÆÃÊÌ¤Ê´Ø·¸¤Ë¤¢¤ëÂ¾±¡Îò¤ò½é²ó
      *                                     Æþ±¡Æü¤È¤·¤Æµ¯»»¤·¥ì¥»¥×¥Èµ­
      *                                     ºÜ¤ò¹Ô¤Ê¤¦ÂÐ±þ
      *  04.04.03    NACL-ÌçÏÆ    09/06/22  Àº¿À²Ê³ÖÎ¥¼¼´ÉÍý²Ã»»»»ÄêÆü
      *                                     ¥ì¥»ÅÅÍÑ¥³¥á¥ó¥ÈÊÔ½¸
      *  04.04.04    NACL-ÌçÏÆ    10/03/03  ÎÅÍÜ¥ì¥»¥³¥á¥ó¥ÈÊÔ½¸½¤Àµ
      *  04.04.05    NACL-ÌçÏÆ    10/03/23  ²þÀµÂÐ±þ¡Ê³°Çñ¶èÊ¬£µ¤ÎÂÐ±þ¡Ë
      *  04.04.06    NACL-ÌçÏÆ    10/03/29  ²þÀµÂÐ±þ
      *                                     ¡Ê¥³¡¼¥ÉÄÉ²Ã¡Ë
      *  04.04.07    NACL-ÌçÏÆ    10/04/06  ²þÀµÂÐ±þ
      *                                     ¡Ê¥³¥á¥ó¥È¼«Æ°µ­ºÜ¡Ë
      *  04.04.08    NACL-ÌçÏÆ    10/04/23  ºßÂð·ì±ÕÆ©ÀÏÉÑ²ó»ØÆ³´ÉÍýÎÁ¤Î
      *                                     ¥³¥á¥ó¥È¼«Æ°µ­ºÜ½¤Àµ
      *  04.04.09    NACL-ÌçÏÆ    10/05/10  Æþ±¡´ü´Ö¥³¥á¥ó¥È½¤Àµ
      *  04.04.10    NACL-ÌçÏÆ    10/06/01  Â¾°åÎÅµ¡´Ø¼õ¿ÇÂÐ±þ
      *
      *  04.05.01    NACL-Æ£¸¶    09/07/14  ¾®¿ôÅÀ°Ê²¼£µ·åÂÐ±þ
      *  04.05.02    NACL-ÌçÏÆ    09/11/25  ¥ê¥Ï¥Ó¥ê¥³¥á¥ó¥ÈÂÐ±þ
      *  04.05.03    NACL-ÌçÏÆ    09/11/30  ¥ì¥»ÅÅ¥Õ¥©¡¼¥Þ¥Ã¥È¥³¥á¥ó¥È³ÈÄ¥
      *                                     ÂÐ±þ¡Ê¥Õ¥¡¥¤¥ëÊÔ½¸¡Ë
      *  04.05.04    NACL-ÌçÏÆ    11/08/08  ³°Çñ¡ÊÂ¾°å¼õ¿Ç¡Ë¤ÎÆüÉÕ¥³¥á¥ó¥È
      *                                     ¤Îº¹¤·¹þ¤ß¤ò»ß¤á¤ë
      *  04.05.05    NACL-ÌçÏÆ    11/11/30  ²èÁü¿ÇÃÇ¥Õ¥¤¥ë¥àÊ¬²è¿ôÂÐ±þ
      *  04.05.06    NACL-ÌçÏÆ    12/04/02  Ê¿À®£²£´Ç¯£´·î²þÀµ
      *                                     ¥³¥á¥ó¥Èº¹¹þ
      *                                     ³°ÇñÆü¥³¥á¥ó¥È¤òµ­ºÜ¤·¤Ê¤¤
      *                                     190153090
      *                                     190153190
      *                                     190153290
      *                                     190153390
      *                                     ÎÅÍÜÉÂÅï¥³¥á¥ó¥ÈÌ¾¾ÎÊÔ½¸
      *                                     820000160
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
           SELECT  RECE62-FILE     ASSIGN  RECE62PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE62-KEY
                                   FILE    STATUS  IS  STS-RECE62.
      *
           SELECT  RECE64-FILE     ASSIGN  RECE64PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE64-KEY
                                   FILE    STATUS  IS  STS-RECE64.
      *
           SELECT  RECE65-FILE     ASSIGN  RECE65PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE65-KEY
                                   FILE    STATUS  IS  STS-RECE65.
      *
           SELECT  RECE641-FILE    ASSIGN  RECE641PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE641-KEY
                                   FILE    STATUS  IS  STS-RECE641.
           SELECT  RECE6441-FILE   ASSIGN  RECE641PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE6441-KEY
                                   FILE    STATUS  IS  STS-RECE641.
      *
           SELECT  RECE651-FILE    ASSIGN  RECE651PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE651-KEY
                                   FILE    STATUS  IS  STS-RECE651.
           SELECT  RECE6551-FILE   ASSIGN  RECE651PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE6551-KEY
                                   FILE    STATUS  IS  STS-RECE651.
      *¥ï¡¼¥¯
           SELECT  RECE649-FILE    ASSIGN  RECE649PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE649-KEY
                                   FILE    STATUS  IS  STS-RECE649.
           SELECT  RECE6449-FILE   ASSIGN  RECE649PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE6449-KEY
                                   FILE    STATUS  IS  STS-RECE649.
      *
           SELECT  RECE659-FILE    ASSIGN  RECE659PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE659-KEY
                                   FILE    STATUS  IS  STS-RECE659.
           SELECT  RECE6559-FILE   ASSIGN  RECE659PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE6559-KEY
                                   FILE    STATUS  IS  STS-RECE659.
      *
           SELECT  RECE655-FILE    ASSIGN  RECE655PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE655-KEY
                                   FILE    STATUS  IS  STS-RECE655.
           SELECT  RECE6555-FILE   ASSIGN  RECE655PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE6555-KEY
                                   FILE    STATUS  IS  STS-RECE655.
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    ¥ì¥»¥×¥È£°£²¥Ç¡¼¥¿
       FD  RECE62-FILE.
       01  RECE62-REC.
           COPY    "CPRCF062.INC".
      *
      *    ¥ì¥»¥×¥È£°£´¥Ç¡¼¥¿
       FD  RECE64-FILE.
       01  RECE64-REC.
           COPY    "CPRCF064.INC".
      *
      *    ¥ì¥»¥×¥È£°£µ¥Ç¡¼¥¿
       FD  RECE65-FILE.
       01  RECE65-REC.
           COPY    "CPRCF065.INC".
      *
       FD  RECE641-FILE.
       01  RECE641-REC. 
           COPY    "CPRCF064.INC"  REPLACING   //RECE64//
                                   BY          //RECE641//.
      *
       FD  RECE651-FILE.
       01  RECE651-REC. 
           COPY    "CPRCF065.INC"  REPLACING   //RECE65//
                                   BY          //RECE651//.
      *
       FD  RECE6441-FILE.
       01  RECE6441-REC. 
           COPY    "CPRCF064.INC"  REPLACING   //RECE64//
                                   BY          //RECE6441//.
      *
       FD  RECE6551-FILE.
       01  RECE6551-REC. 
           COPY    "CPRCF065.INC"  REPLACING   //RECE65//
                                   BY          //RECE6551//.
      *¥ï¡¼¥¯
       FD  RECE649-FILE.
       01  RECE649-REC. 
           COPY    "CPRCF064.INC"  REPLACING   //RECE64//
                                   BY          //RECE649//.
      *
       FD  RECE659-FILE.
       01  RECE659-REC. 
           COPY    "CPRCF065.INC"  REPLACING   //RECE65//
                                   BY          //RECE659//.
      *
       FD  RECE6449-FILE.
       01  RECE6449-REC. 
           COPY    "CPRCF064.INC"  REPLACING   //RECE64//
                                   BY          //RECE6449//.
      *
       FD  RECE6559-FILE.
       01  RECE6559-REC. 
           COPY    "CPRCF065.INC"  REPLACING   //RECE65//
                                   BY          //RECE6559//.
      *
       FD  RECE655-FILE.
       01  RECE655-REC. 
           COPY    "CPRCF0655.INC".
      *
       FD  RECE6555-FILE.
       01  RECE6555-REC. 
           COPY    "CPRCF0655.INC" REPLACING   //RECE655//
                                   BY          //RECE6555//.
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
                                   BY         //RECE62//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE64//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE65//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE641//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE651//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE649//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE659//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE655//.
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPERRFL.INC"
                                   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  STS-AREA.
           03  STS-RECE62          PIC X(02).
           03  STS-RECE64          PIC X(02).
           03  STS-RECE641         PIC X(02).
           03  STS-RECE65          PIC X(02).
           03  STS-RECE651         PIC X(02).
           03  STS-RECE649         PIC X(02).
           03  STS-RECE659         PIC X(02).
           03  STS-RECE655         PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-RECE62          PIC 9(01).
           03  FLG-RECE64          PIC 9(01).
           03  FLG-RECE641         PIC 9(01).
           03  FLG-RECE649         PIC 9(01).
           03  FLG-RECE65          PIC 9(01).
           03  FLG-RECE659         PIC 9(01).
           03  FLG-RECE651         PIC 9(01).
           03  FLG-YAKUKAN-COMMENT PIC 9(01).
           03  FLG-HINKAI-COMMENT  PIC 9(01).
           03  FLG-KYUSEI-COMMENT  PIC 9(01).
           03  FLG-KYUSEI-COMMENT2 PIC 9(01).
           03  FLG-COMMENT         PIC 9(02).
           03  FLG-WRKRECE65       PIC 9(01).
           03  FLG-FIRST           PIC 9(01).
           03  FLG-NYU-RENZOKU     PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-KYUSEI-OK       PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-RIHA            PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-RYOCOM          PIC 9(01).
           03  FLG-RYOCOM2         PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-COMOKIKAE       PIC 9(01).
           03  FLG-COMOKIKAE2      PIC 9(01).
           03  FLG-HENSYUKIN       PIC 9(01).
           03  FLG-HANTEI          PIC 9(01).
           03  FLG-COMARI          PIC 9(01).
           03  FLG-COMARI2         PIC 9(01).
           03  FLG-SANTEIPLUS      PIC 9(01).
           03  FLG-SETCOM          PIC 9(01).
           03  FLG-MEISKY-OKIKAE   PIC 9(01).
           03  FLG-KAISIEN         PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-NG              PIC 9(01).
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-RECE62          PIC 9(06).
           03  CNT-RECE64          PIC 9(06).
           03  CNT-RECE65          PIC 9(06).
           03  CNT-RECE649         PIC 9(06).
           03  CNT-RECE659         PIC 9(06).
           03  CNT-RECE655         PIC 9(06).
           03  CNT-RECE64-RE       PIC 9(06).
           03  CNT-RECE65-RE       PIC 9(06).
           03  CNT-RIHA            PIC 9(06).
           03  CNT-IN-COMMIT       PIC 9(06).
           03  CNT-IN-CANCEL       PIC 9(06).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(02).
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(04).
           03  IDX6                PIC 9(04).
           03  IDX12               PIC 9(04).
           03  IDX13               PIC 9(04).
           03  IDX14               PIC 9(04).
           03  IDX15               PIC 9(01).
           03  IDX16               PIC 9(02).
           03  IDX17               PIC 9(02).
           03  IDX18               PIC 9(02).
           03  IDX19               PIC 9(02).
           03  IDX20               PIC 9(04).
           03  IDX21               PIC 9(01).
           03  DAY-IDX             PIC 9(04).
           03  DAY-IDX2            PIC 9(02).
           03  IDY                 PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX-RECE65          PIC 9(04).
           03  IDV                 PIC 9(04).
           03  IDW                 PIC 9(04).
           03  RYO-IDX             PIC 9(03).
           03  RYO-IDX2            PIC 9(03).
           03  RYO-IDX3            PIC 9(03).
           03  IDA                 PIC 9(04).
           03  IDB                 PIC 9(04).
           03  IDC1                PIC 9(02).
           03  IDC2                PIC 9(02).
           03  IDC3                PIC 9(02).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM              PIC  X(06).
               05  WRK-PARA-TERMID             PIC  X(16).
               05  WRK-PARA-SYSYMD             PIC  X(08).
               05  WRK-PARA-JOBID              PIC  9(07).
               05  WRK-PARA-SHELLID            PIC  X(08).
           03  WRK-PARA-HOSPNUM                PIC  9(02).
      *
           03  WRK-JYUNBAN         PIC 9(04).
           03  WRK-GYOSUU          PIC 9(04).
           03  WRK-GYOSUU2         PIC 9(04).
           03  WRK-RENNUM          PIC 9(02).
           03  WRK-CNT-MEISAI      PIC 9(04).
           03  WRK-KETA            PIC 9(04).
           03  WRK-KETA-TAIHI      PIC 9(04).
           03  WRK-KETA2           PIC 9(04).
           03  WRK-KOHBUNRI        PIC 9(01).
           03  WRK-KOHBUNRI-JIDO   PIC 9(01).
           03  WRK-HKNCOMBI-JIDO   PIC 9(04).
           03  WRK-RECEERR         PIC X(200).
      *
           03  WRK-FSANTEIYMD      PIC 9(08).
           03  WRK-AFTER2          PIC 9(08).
           03  WRK-AFTER180        PIC 9(08).
           03  WRK-TOSEKIYMD       PIC 9(08).
           03  WRK-KYUSEIYMD       PIC 9(08).
           03  WRK-HINKAIYMD       PIC 9(08).
           03  WRK-DAY             PIC 9(02).
      *
           03  WRK-SRYKBN          PIC X(02).
           03  WRK-SRYCD           PIC X(09).
           03  WRK-SRYCD2          PIC X(09).
           03  WRK-ZAINUM          PIC 9(08).
      *
           03  WRK-HENSYU-KAISU    PIC X(02).
           03  WRK-HENSYU-KAISU2   PIC X(04).
      *
           03  TBL-SANTEI-DAY.
               05  TB-SANTEI-DAY   PIC 9(02)   OCCURS  31.
      *
      *    ³ºÅö¿ÇÎÅ·îÊ¬
           03  TBL-RIHASANTEI-DAY.
               05  TB-RIHASANTEI-DAY PIC 9(01) OCCURS  31.
      *
      *    Ä¾¶áÊ¬
           03  WRK-RIHASANTEI-YMD.
               05  WRK-RIHASANTEI-YM PIC 9(06).
               05  WRK-RIHASANTEI-DD PIC 9(02).
      *
      *    Ä¾¶áÊ¬ÂàÈòÍÑ
           03  WRK-RIHASANTEI-YMD2   PIC X(08).
      *
      *    Á°·î°ÊÁ°¤ÎÄ¾¶á½ªÎ»Æü
           03  WRK-RIHASANTEI-END    PIC X(08).
      *
      *    £²£°£°£¶Ç¯£´·î£±Æü¤Ë»»ÄêÆü¤¬¤¢¤ë¤«
           03  WRK-RIHASANTEI41-FLG  PIC 9(01).
      *
           03  TBL-NYU-DAY.
               05  TBL-NYU-DAY-OCC             OCCURS  31.
                 07  TBL-NYU-DAY-ST  PIC 9(02).
                 07  TBL-NYU-DAY-ED  PIC 9(02).
                 07  TBL-NYU-DAY-CD  PIC X(09).
      *
           03  WRK-ZENYYMM           PIC 9(06).
      *
           03  WRK-TUKI-YAKUKAN      PIC 9(02).
           03  WRK-DAY-YAKUKAN       PIC 9(02).
      *
           03  WRK-KEISAN            PIC 9(09).
           03  WRK-KEISAN2           PIC 9(03).
           03  WRK-KEISAN3           PIC 9(09).
           03  WRK-KEISAN4           PIC 9(09).
           03  WRK-KEISAN-TAIHI      PIC X(09).
      *
           03  WRK-KYUSEI-NYUGAIKBN  PIC X(01).
           03  WRK-KYUSEI-SRYKA      PIC X(02).
           03  WRK-KYUSEI-ZAINUM     PIC 9(08).
      *
           03  WRK-HENYMD            PIC X(08).
           03  WRK-TAIHI-XX          PIC X(02).
      *
           03  WRK-HENSYU-KINGAKU    PIC X(09).
           03  WRK-HENSYU-KINGAKU2   PIC X(18).
           03  WRK-HENSYU-KINGAKU3   PIC X(18).
      *
           03  WRK-RIHAIGAKU-SRYCD           PIC X(09).
           03  WRK-RIHAIGAKU-SRYCD2          PIC X(09).
           03  WRK-RIHAIGAKU-FSANTEIYMD      PIC X(08).
           03  WRK-RIHAIGAKU-SANTEI-AREA.
               05  WRK-RIHAIGAKU-SANTEI-FLG  PIC X(01).
               05  WRK-RIHAIGAKU-SANTEI-DAY  PIC 9(03)
                                             OCCURS 31.
      *
           03  WRK-YAKUKAN-SRYCD     PIC X(09).
      *
           03  WRK-HEN-RENNUM                PIC 9(02).
           03  WRK-HEN-SHORI-RENNUM          PIC 9(01).
           03  WRK-HEN-SEQNO                 PIC 9(02).
      *
           03  WRK-HEN-RENNUM-X              PIC 9(02).
           03  WRK-HEN-SHORI-RENNUM-X        PIC 9(01).
           03  WRK-HEN-SEQNO-X               PIC 9(02).
      *
           03  WRK-HEN-RENNUM-J              PIC 9(02).
           03  WRK-HEN-SHORI-RENNUM-J        PIC 9(01).
           03  WRK-HEN-SEQNO-J               PIC 9(02).
      *
           03  WRK-HEN-RENNUM-J2             PIC 9(02).
           03  WRK-HEN-SHORI-RENNUM-J2       PIC 9(01).
           03  WRK-HEN-SEQNO-J2              PIC 9(02).
      *
           03  WRK-KAISIEN-FSANTEIYMD        PIC X(08).
           03  WRK-KAISIEN-YMD               PIC X(08).
           03  WRK-KAISIEN-CHECKYMD          PIC X(08).
           03  WRK-KAISIEN-RRKNUM            PIC 9(03).
           03  WRK-KAISIEN-RRKNUM-X          PIC 9(03).
           03  WRK-KAISIEN-RRKNUM-Y          PIC 9(03).
      *
           03  WRK-BUNKAKUKETA               PIC 9(02).
           03  WRK-BUNKAKU-N                 PIC X(10).
           03  WRK-SRYKAISU-X                PIC ZZ9.
           03  WRK-CHAR-HENKAN-AREA.
               05  WRK-CHAR-X                PIC X(03).
               05  WRK-CHAR-N                PIC X(06).
               05  WRK-CHAR-N-CNT            PIC 9(01).
               05  WRK-CHAR-IDX              PIC 9(01).
      *
       01  WRK-RECE64-REC. 
           COPY    "CPRCF064.INC"  REPLACING   //RECE64//
                                   BY          //WRK-RECE64//.
      *
       01  WRK-RECE65-REC. 
           COPY    "CPRCF065.INC"  REPLACING   //RECE65//
                                   BY          //WRK-RECE65//.
      *
       01  WRK-RECE65XX-REC.
         02 WRK-RECE65XX-TBL       OCCURS 50.
           COPY    "CPRCF065.INC"  REPLACING   //RECE65//
                                   BY          //WRK-RECE65XX//.
      *
       01  WRK-RECE65XX2-REC.
         02 WRK-RECE65XX2-TBL      OCCURS 300.
           COPY    "CPRCF065.INC"  REPLACING   //RECE65//
                                   BY          //WRK-RECE65XX2//.
      *
       01  WRK-FREE-AREA.
           03  WRK-FREEINF                     OCCURS 49.         
               05  WK-FREE-YUKOKETA          PIC  9(03).
               05  WK-FREE-NAME              PIC  X(200).
      *
       01  WRK-FREE-AREA2.                                              
           03  WRK-FREEINF2                    OCCURS 50.               
               05  WK-FREE-YUKOKETA2         PIC  9(03).                
               05  WK-FREE-NAME2             PIC  X(200).               
               05  WK-FREE-MEISKYFLG2        PIC  X(01).
               05  WK-FREE-SETCOMATFLG2      PIC  9(03).
      *
       01  WRK-FREE-AREA3.                                              
           03  WRK-FREEINF3.
               05  WK-FREE-NAME3             PIC  X(1000).              
               05  WK-FREE-NAME3-TAIHI       PIC  X(1000).              
      *
       01  WRK-JIDO-SETCOM-AREA.                                              
           03  WRK-JIDO-SETCOMATFLG          PIC  9(03).
           03  WRK-JIDO-SETCOMFLG            PIC  X(01).
      *
      *    ÆüÉÕÁ´³ÑÊÑ´¹ÎÎ°è
       01  TBL-DAY-AREA.
           03  FILLER              PIC X(04)  VALUE "¡¡£±".
           03  FILLER              PIC X(04)  VALUE "¡¡£²".
           03  FILLER              PIC X(04)  VALUE "¡¡£³".
           03  FILLER              PIC X(04)  VALUE "¡¡£´".
           03  FILLER              PIC X(04)  VALUE "¡¡£µ".
           03  FILLER              PIC X(04)  VALUE "¡¡£¶".
           03  FILLER              PIC X(04)  VALUE "¡¡£·".
           03  FILLER              PIC X(04)  VALUE "¡¡£¸".
           03  FILLER              PIC X(04)  VALUE "¡¡£¹".
           03  FILLER              PIC X(04)  VALUE "£±£°".
           03  FILLER              PIC X(04)  VALUE "£±£±".
           03  FILLER              PIC X(04)  VALUE "£±£²".
           03  FILLER              PIC X(04)  VALUE "£±£³".
           03  FILLER              PIC X(04)  VALUE "£±£´".
           03  FILLER              PIC X(04)  VALUE "£±£µ".
           03  FILLER              PIC X(04)  VALUE "£±£¶".
           03  FILLER              PIC X(04)  VALUE "£±£·".
           03  FILLER              PIC X(04)  VALUE "£±£¸".
           03  FILLER              PIC X(04)  VALUE "£±£¹".
           03  FILLER              PIC X(04)  VALUE "£²£°".
           03  FILLER              PIC X(04)  VALUE "£²£±".
           03  FILLER              PIC X(04)  VALUE "£²£²".
           03  FILLER              PIC X(04)  VALUE "£²£³".
           03  FILLER              PIC X(04)  VALUE "£²£´".
           03  FILLER              PIC X(04)  VALUE "£²£µ".
           03  FILLER              PIC X(04)  VALUE "£²£¶".
           03  FILLER              PIC X(04)  VALUE "£²£·".
           03  FILLER              PIC X(04)  VALUE "£²£¸".
           03  FILLER              PIC X(04)  VALUE "£²£¹".
           03  FILLER              PIC X(04)  VALUE "£³£°".
           03  FILLER              PIC X(04)  VALUE "£³£±".
       01  TBL-DAY-AREA-R          REDEFINES   TBL-DAY-AREA.
           03  TBL-DAY             PIC X(04)   OCCURS   31.
      *
       01  MAX-MEISAISU            PIC 9(07).
      *
      *    ¸ÇÄêÃÍ
       01  CONST-AREA.
      *    ÌôºÞ´ÉÍý»ØÆ³ÎÁ
           03  CONS-YAKUKAN-COMMENT PIC X(09)  VALUE "120001610".
      *    »»ÄêÆü¥³¥á¥ó¥È
           03  CONS-SANTEI-COMMENT1  PIC X(09) VALUE "840000058".
           03  CONS-SANTEI-COMMENT2  PIC X(09) VALUE "840000065".
      *    ºßÂð·ì±ÕÆ©ÀÏ»ØÆ³´ÉÍýÎÁ
           03  CONS-TOSEKI          PIC X(09)  VALUE "114009310".
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆü
           03  CONS-KYUSEI          PIC X(09)  VALUE "099800101".
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾É½ªÎ»Æü
           03  CONS-KYUSEI-END      PIC X(09)  VALUE "099800102".
      *    ¿´Âç¥ê¥Ï³«»ÏÆü
           03  CONS-SINDAI          PIC X(09) VALUE "099800111".
      *    ¸ÆµÛ´ï¥ê¥Ï³«»ÏÆü
           03  CONS-KOKYUKI         PIC X(09) VALUE "099800141".
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï³«»ÏÆü
           03  CONS-NOUKE           PIC X(09) VALUE "099800121".
      *    ±¿Æ°´ï¥ê¥Ï³«»ÏÆü
           03  CONS-UNDOKI          PIC X(09) VALUE "099800131".
      *    ÆñÉÂ´µ¼Ô¥ê¥Ï³«»ÏÆü
           03  CONS-NANBYO          PIC X(09) VALUE "099800161".
      *    ¾ã³²»ù¡Ê¼Ô¡Ë¥ê¥Ï³«»ÏÆü
           03  CONS-SYOGAI          PIC X(09) VALUE "099800171".
      *    ÀÝ¿©ÎÅË¡³«»ÏÆü
           03  CONS-SESSYOKU        PIC X(09) VALUE "099800151".
      *
      *    ¿´Âç¥ê¥Ï½ªÎ»Æü
           03  CONS-SINDAI-E        PIC X(09) VALUE "099800112".
      *    ¸ÆµÛ´ï¥ê¥Ï½ªÎ»Æü
           03  CONS-KOKYUKI-E       PIC X(09) VALUE "099800142".
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï½ªÎ»Æü
           03  CONS-NOUKE-E         PIC X(09) VALUE "099800122".
      *    ±¿Æ°´ï¥ê¥Ï½ªÎ»Æü
           03  CONS-UNDOKI-E        PIC X(09) VALUE "099800132".
      *    ÆñÉÂ´µ¼Ô¥ê¥Ï½ªÎ»Æü
           03  CONS-NANBYO-E        PIC X(09) VALUE "099800162".
      *    ¾ã³²»ù¡Ê¼Ô¡Ë¥ê¥Ï½ªÎ»Æü
           03  CONS-SYOGAI-E        PIC X(09) VALUE "099800172".
      *    ÀÝ¿©ÎÅË¡½ªÎ»Æü
           03  CONS-SESSYOKU-E      PIC X(09) VALUE "099800152".
      *
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£±¡Ë
           03  CONS-SINDAI-IGAKU1   PIC X(09) VALUE "180029910".
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£²¡Ë
           03  CONS-SINDAI-IGAKU2   PIC X(09) VALUE "180030010".
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£±¡Ë
           03  CONS-KOKYUKI-IGAKU1  PIC X(09) VALUE "180030510".
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£²¡Ë
           03  CONS-KOKYUKI-IGAKU2  PIC X(09) VALUE "180030610".
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£±¡Ë
           03  CONS-NOUKE-IGAKU1    PIC X(09) VALUE "180030110".
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£²¡Ë
           03  CONS-NOUKE-IGAKU2    PIC X(09) VALUE "180030210".
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£±¡Ë
           03  CONS-UNDOKI-IGAKU1   PIC X(09) VALUE "180030310".
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó°å³Ø´ÉÍýÎÁ¡Ê£²¡Ë
           03  CONS-UNDOKI-IGAKU2   PIC X(09) VALUE "180030410".
      *
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¼Â»Ü
           03  CONS-SINDAI-DUMMY    PIC X(09) VALUE "099800311".
      *    ¸ÆµÛ´ï¥ê¥Ï¼Â»Ü
           03  CONS-KOKYUKI-DUMMY   PIC X(09) VALUE "099800341".
      *    Ç¾·ì´É¼À´µ¥ê¥Ï¼Â»Ü
           03  CONS-NOUKE-DUMMY     PIC X(09) VALUE "099800321".
      *    ±¿Æ°´ï¥ê¥Ï¼Â»Ü
           03  CONS-UNDOKI-DUMMY    PIC X(09) VALUE "099800331".
      *
      *    ²ð¸î»Ù±çÏ¢·È»ØÆ³ÎÁ
           03  CONS-KAISIEN         PIC X(09) VALUE "113011710".
      *
      *    ÌôºÞ´ÉÍý»ØÆ³ÎÁ¥Æ¡¼¥Ö¥ë¡Ê£²£°£°£¸Ç¯£´·î¿ÇÎÅÊ¬¡Á¡Ë
       01  TBL-YAKUKAN-AREA.
           03  FILLER               PIC X(09) VALUE "113010710".
           03  FILLER               PIC X(09) VALUE "113010810".
           03  FILLER               PIC X(09) VALUE "120001610".
      *
       01  TBL-YAKUKAN-R       REDEFINES   TBL-YAKUKAN-AREA.
            03  TBL-YAKUKAN    PIC X(09)   OCCURS   3.
      *
       01  TBL-YAKUKAN-MAX     PIC 9(02)   VALUE    3.
      *
      *
       01  TBL-HASSYO-INF.
           03  TBL-HASSYO-KAITATU-FLG           PIC 9(01).
           03  TBL-HASSYO-SINDAI-FLG            PIC 9(01).
           03  TBL-HASSYO-KOKYUKI-FLG           PIC 9(01).
           03  TBL-HASSYO-NOUKE-FLG             PIC 9(01).
           03  TBL-HASSYO-UNDOKI-FLG            PIC 9(01).
           03  TBL-HASSYO-NANBYO-FLG            PIC 9(01).
           03  TBL-HASSYO-SYOGAI-FLG            PIC 9(01).
           03  TBL-HASSYO-SESSYOKU-FLG          PIC 9(01).
           03  TBL-HASSYO-INF2                  OCCURS  5.
               05  TBL-HASSYO-KAITATU-COMMENT   PIC 9(01).
               05  TBL-HASSYO-SINDAI-COMMENT    PIC 9(01).
               05  TBL-HASSYO-KOKYUKI-COMMENT   PIC 9(01).
               05  TBL-HASSYO-NOUKE-COMMENT     PIC 9(01).
               05  TBL-HASSYO-UNDOKI-COMMENT    PIC 9(01).
               05  TBL-HASSYO-NANBYO-COMMENT    PIC 9(01).
               05  TBL-HASSYO-SYOGAI-COMMENT    PIC 9(01).
               05  TBL-HASSYO-SESSYOKU-COMMENT  PIC 9(01).
               05  TBL-HASSYO-HKNCOMBI          PIC 9(04).
      *
       01  TBL-ZAISIEN-INF.
           03  TBL-ZAISIEN-INF2                 OCCURS  5.
               05  TBL-ZAISIEN-COMMENT          PIC 9(01).
               05  TBL-ZAISIEN-HKNCOMBI         PIC 9(04).
      *
      *    »ÀÁÇ¡Ê¶â³Û¡¦¿ôÎÌ¡Ë¾ðÊó
       01  TBL-SANSO-INF.
           03  TBL-SANSO-INF2       OCCURS    10.
               05  TBL-SANSO-TEN           PIC 9(09)V9(02).
               05  TBL-SANSO-SRYSURYO      PIC 9(05)V9(05).
           03  TBL-SANSO-INF3.
               05  TBL-KASAN-SRYSURYO      PIC 9(05)V9(05).
      *
      *    ÎÅÍÜ¥ì¥»¥³¥á¥ó¥È¾ðÊó
       01  TBL-RYOCOM-INF.
           03  TBL-RYOCOM-INF2                 OCCURS  31.
               05  TBL-RYOCOM-NYUINKBN         PIC  X(01).
               05  TBL-RYOCOM-AREA.
                 06  TBL-RYOCOM-MEISAISU       PIC  9(03).
                 06  TBL-RYOCOM-SRYCDTOTAL     PIC  9(14).
                 06  TBL-RYOCOM-INF3           OCCURS  30.
                   07  TBL-RYOCOM-IRYO-SRYCD   PIC  X(09).
                 06  TBL-RYOCOM-ADL-SRYCD      PIC  X(09).
                 06  TBL-RYOCOM-ADL-TEN        PIC  9(02).
                 06  TBL-RYOCOM-ADL-TEN-MOJI   PIC  X(04).
                 06  TBL-RYOCOM-INF4           OCCURS   4.
                   07  TBL-RYOCOM-ADL-SRYCD-H22      PIC  X(09).
                   07  TBL-RYOCOM-ADL-TEN-H22        PIC  9(02).
                   07  TBL-RYOCOM-ADL-TEN-MOJI-H22   PIC  X(04).
      *
      *    ÎÅÍÜ¥ì¥»¥³¥á¥ó¥È¾ðÊó¡Ê³ºÅöÊ¬¤Î¤Þ¤È¤á¡Ë
       01  TBL-RYOCOM2-INF.
           03  TBL-RYOCOM2-INF2                OCCURS  31.
               05  TBL-RYOCOM2-AREA.
                 06  TBL-RYOCOM2-MEISAISU      PIC  9(03).
                 06  TBL-RYOCOM2-SRYCDTOTAL    PIC  9(14).
                 06  TBL-RYOCOM2-INF3          OCCURS  30.
                   07  TBL-RYOCOM2-IRYO-SRYCD  PIC  X(09).
                 06  TBL-RYOCOM2-ADL-SRYCD     PIC  X(09).
                 06  TBL-RYOCOM2-ADL-TEN       PIC  9(02).
                 06  TBL-RYOCOM2-ADL-TEN-MOJI  PIC  X(04).
                 06  TBL-RYOCOM2-INF4          OCCURS   4.
                   07  TBL-RYOCOM2-ADL-SRYCD-H22     PIC  X(09).
                   07  TBL-RYOCOM2-ADL-TEN-H22       PIC  9(02).
                   07  TBL-RYOCOM2-ADL-TEN-MOJI-H22  PIC  X(04).
               05  TBL-RYOCOM2-INF4            OCCURS  31.
                 06  TBL-RYOCOM2-DAY           PIC  9(01).
      *
      *    ¥Ñ¥¹Ì¾ÎÎ°è
       01  WRK-PATH                        PIC X(64).
      *
      *    »ÀÁÇ 
           COPY    "CMSANSOTBL.INC".
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥¸¥ç¥Ö´ÉÍý¥Þ¥¹¥¿
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY    "CPSYSKANRI.INC".
      *
      *    ¿ÇÎÅ²ÊÌÜ¾ðÊó
           COPY    "CPSK1005.INC".
      *
      *    ¿ÇÎÅ²ñ·×
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    ¿ÇÎÅ¹Ô°Ù
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    »»ÄêÍúÎò
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    ÊÝ¸±ÁÈ¤ß¹ç¤ï¤»
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ´µ¼Ô¥³¥á¥ó¥È
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    ÅÀ¿ô
           COPY    "CPTENSU.INC".
      *
      *    »»ÄêÍúÎòÉÕ²Ã
       01  SANTEIPLUS-REC.
           COPY    "CPSANTEIPLUS.INC".
      *
      *    Æþ±¡ÍúÎò
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑÎÎ°è
      *****************************************************************
      *   ¥¸¥ç¥Ö´ÉÍý£Ä£ÂÀ©¸æ¥µ¥Ö
           COPY    "CPORCSJOBKANRI.INC".
      *     
      *    ¿ôÃÍÊÑ´¹¥µ¥Ö
           COPY    "CPORCSNUM.INC".
      *
      *    ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    Á´³Ñ¥Á¥§¥Ã¥¯¥Ñ¥é¥á¥¿
           COPY    "CPORCSKANACHK.INC".
      *
      *    ÎÅÍÜ¥ì¥»¥³¥á¥ó¥È¼èÆÀ¥µ¥Ö
           COPY    "CPORCSRYOCOM.INC".
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
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL  FLG-END  =   1
      *
           PERFORM 300-END-SEC
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
      *
      *    ¥Õ¥¡¥¤¥ë£Ï£Ð£Å£Î½èÍý
      *
           PERFORM 110-FILE-OPEN-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        £Ä£Â¥ª¡¼¥×¥ó½èÍý
               PERFORM 120-DBOPEN-SEC
      *
      *        ¥¹¥Æ¥Ã¥×´ÉÍý³«»Ï½èÍý
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0660"      TO  JOB-PGID
               MOVE    "¥³¥á¥ó¥ÈÊÔ½¸"  TO  JOB-SHELLMSG
               PERFORM   900-CALL-ORCSJOB-SEC
      *        ½èÍýÃæ»ßÀßÄê½èÍý
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           PERFORM 990-RECE62-READ-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë£Ï£Ð£Å£Î½èÍý
      *****************************************************************
       110-FILE-OPEN-SEC          SECTION.
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    "RECE620"           TO  RECE62PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE62PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE62PARA-HOSPNUM
      *
           MOVE    "RECE64W"           TO  RECE64PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE64PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE64PARA-HOSPNUM
      *
           MOVE    "RECE650"           TO  RECE65PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE65PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE65PARA-HOSPNUM
      *
           MOVE    "RECE641"           TO  RECE641PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE641PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE641PARA-HOSPNUM
      *
           MOVE    "RECE651"           TO  RECE651PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE651PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE651PARA-HOSPNUM
      *
           MOVE    "RECE64Y"           TO  RECE649PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE649PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE649PARA-HOSPNUM
      *
           MOVE    "RECE65Y"           TO  RECE659PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE659PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE659PARA-HOSPNUM
      *
           MOVE    "RECE655"           TO  RECE655PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE655PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE655PARA-HOSPNUM
      *
           OPEN    INPUT               RECE62-FILE
      *
           OPEN    INPUT               RECE64-FILE
      *
           OPEN    INPUT               RECE65-FILE
      *
           OPEN    OUTPUT              RECE6441-FILE
           CLOSE                       RECE6441-FILE
      *
           OPEN    OUTPUT              RECE6551-FILE
           CLOSE                       RECE6551-FILE
      *
           OPEN    OUTPUT              RECE6449-FILE
           CLOSE                       RECE6449-FILE
      *
           OPEN    OUTPUT              RECE6559-FILE
           CLOSE                       RECE6559-FILE
      *
           OPEN    OUTPUT              RECE6555-FILE
           CLOSE                       RECE6555-FILE
      *
           OPEN    I-O                 RECE641-FILE
           OPEN    I-O                 RECE649-FILE
      *
           OPEN    I-O                 RECE651-FILE
           OPEN    I-O                 RECE659-FILE
      *
           OPEN    OUTPUT              RECE655-FILE
      *
           .
       110-FILE-OPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                      TBL-HASSYO-INF
           INITIALIZE                      TBL-ZAISIEN-INF
           INITIALIZE                      TBL-RYOCOM-INF
           INITIALIZE                      WRK-JIDO-SETCOM-AREA
      *
      *    ÎÅÍÜ¥ì¥»¥³¥á¥ó¥È¼èÆÀ
      *
           MOVE      ZERO     TO      FLG-RYOCOM
      *
           IF    RECE62-SRYYM   >=  "200610"
             INITIALIZE                   ORCSRYOCOM-AREA
             MOVE    RECE62-HOSPNUM   TO  LNK-RYOCOM-HOSPNUM
             MOVE    RECE62-PTID      TO  LNK-RYOCOM-PTID
             MOVE    RECE62-SRYYM     TO  LNK-RYOCOM-SANTEIYM
             CALL    "ORCSRYOCOM"         USING
                                          ORCSRYOCOM-AREA
                                          SPA-AREA
             IF      LNK-RYOCOM-RC    =   ZERO
               MOVE       1           TO  FLG-RYOCOM
               PERFORM  VARYING  RYO-IDX  FROM  1  BY  1
                         UNTIL   RYO-IDX   >   31
                 MOVE  LNK-RYOCOM-NYUINKBN(RYO-IDX)
                                 TO  TBL-RYOCOM-NYUINKBN(RYO-IDX)
                 MOVE  LNK-RYOCOM-MEISAISU(RYO-IDX)
                                 TO  TBL-RYOCOM-MEISAISU(RYO-IDX)
                 MOVE  LNK-RYOCOM-SRYCDTOTAL(RYO-IDX)
                                 TO  TBL-RYOCOM-SRYCDTOTAL(RYO-IDX)
                 PERFORM  VARYING  RYO-IDX2  FROM  1  BY  1
                           UNTIL   RYO-IDX2   >   30
                   MOVE  LNK-RYOCOM-IRYO-SRYCD(RYO-IDX RYO-IDX2)
                           TO  TBL-RYOCOM-IRYO-SRYCD(RYO-IDX RYO-IDX2)
                 END-PERFORM
                 MOVE  LNK-RYOCOM-ADL-SRYCD(RYO-IDX)
                                 TO  TBL-RYOCOM-ADL-SRYCD(RYO-IDX)
                 MOVE  LNK-RYOCOM-ADL-TEN(RYO-IDX)
                                 TO  TBL-RYOCOM-ADL-TEN(RYO-IDX)
                 MOVE  LNK-RYOCOM-ADL-TEN-MOJI(RYO-IDX)
                                 TO  TBL-RYOCOM-ADL-TEN-MOJI(RYO-IDX)
                 PERFORM  VARYING  RYO-IDX2  FROM  1  BY  1
                           UNTIL   RYO-IDX2   >    4
                   MOVE  LNK-RYOCOM-ADL-SRYCD-H22(RYO-IDX RYO-IDX2)
                      TO  TBL-RYOCOM-ADL-SRYCD-H22(RYO-IDX RYO-IDX2)
                   MOVE  LNK-RYOCOM-ADL-TEN-H22(RYO-IDX RYO-IDX2)
                      TO  TBL-RYOCOM-ADL-TEN-H22(RYO-IDX RYO-IDX2)
                   MOVE  LNK-RYOCOM-ADL-TEN-MOJI-H22(RYO-IDX RYO-IDX2)
                      TO  TBL-RYOCOM-ADL-TEN-MOJI-H22(RYO-IDX RYO-IDX2)
                 END-PERFORM
               END-PERFORM
             END-IF
           END-IF
      *
           MOVE  RECE62-KOHBUNRI-MIN   TO  WRK-KOHBUNRI-JIDO
           MOVE  ZERO                  TO  WRK-HKNCOMBI-JIDO
      *
           IF   RECE62-COMCNTFLG   =   "1"
      *
             INITIALIZE                      RECE64-REC
             MOVE    RECE62-HOSPNUM      TO  RECE64-HOSPNUM
             MOVE    RECE62-SRYYM        TO  RECE64-SRYYM
             MOVE    RECE62-NYUGAIKBN    TO  RECE64-NYUGAIKBN
             MOVE    RECE62-PTID         TO  RECE64-PTID
             MOVE    RECE62-RECEKA       TO  RECE64-RECEKA
             MOVE    RECE62-TEISYUTUSAKI TO  RECE64-TEISYUTUSAKI
             MOVE    RECE62-RECESYUBETU  TO  RECE64-RECESYUBETU
             MOVE    RECE62-HKNJANUM-KEY TO  RECE64-HKNJANUM
             MOVE    RECE62-HOJOKBN-KEY  TO  RECE64-HOJOKBN
             MOVE    RECE62-TEKSTYMD     TO  RECE64-TEKSTYMD
             MOVE    ZERO                TO  RECE64-ZAINUM
      *
             START   RECE64-FILE
                 KEY IS    >=   RECE64-KEY
                 INVALID
                     MOVE    1           TO  FLG-RECE64
                 NOT INVALID
                     PERFORM 990-RECE64-NEXT-SEC
             END-START
             PERFORM
               UNTIL  ( FLG-RECE64           =   1                   )
               OR     ( RECE62-HOSPNUM   NOT =   RECE64-HOSPNUM      )
               OR     ( RECE62-SRYYM     NOT =   RECE64-SRYYM        )
               OR     ( RECE62-NYUGAIKBN NOT =   RECE64-NYUGAIKBN    )
               OR     ( RECE62-PTID      NOT =   RECE64-PTID         )
               OR     ( RECE62-RECEKA    NOT =   RECE64-RECEKA       )
               OR     ( RECE62-TEISYUTUSAKI
                                         NOT =   RECE64-TEISYUTUSAKI )
               OR     ( RECE62-RECESYUBETU
                                         NOT =   RECE64-RECESYUBETU  )
               OR     ( RECE62-HKNJANUM-KEY
                                         NOT =   RECE64-HKNJANUM     )
               OR     ( RECE62-HOJOKBN-KEY
                                         NOT =   RECE64-HOJOKBN      )
               OR     ( RECE62-TEKSTYMD  NOT =   RECE64-TEKSTYMD     )
      *
               EVALUATE      RECE64-SRYKBN
                   WHEN      "14"
      *            ¡Êºß»Ù±ç¤Ë´Ø¤¹¤ë¿ÇÎÅ¤Î¥«¥¦¥ó¥È¡Ë
                      IF  RECE64-FLG-ZAISIEN-COMMENT   =  "1" OR "2"
                        MOVE    RECE64-REC    TO   WRK-RECE64-REC
                        COMPUTE WRK-KOHBUNRI
                                   =    RECE64-KOHBUNRI  +  1
                        EVALUATE  RECE64-FLG-ZAISIEN-COMMENT
                         WHEN      "1"
                          MOVE 1 TO TBL-ZAISIEN-COMMENT(WRK-KOHBUNRI)
                         WHEN      "2"
                          MOVE 2 TO TBL-ZAISIEN-COMMENT(WRK-KOHBUNRI)
                        END-EVALUATE
                        MOVE  RECE64-HKNCOMBI  TO
                                     TBL-ZAISIEN-HKNCOMBI(WRK-KOHBUNRI)
                      END-IF
                   WHEN      "40"
      *            ¡Ê²ðÃ£¸£°ú¤¢¤ê¡Ë
                      IF  RECE64-KAITATU-COMMENT    =  "1"
                        MOVE    1    TO    TBL-HASSYO-KAITATU-FLG
                        COMPUTE  WRK-KOHBUNRI
                                   =    RECE64-KOHBUNRI  +  1
                        MOVE    1    TO
                               TBL-HASSYO-KAITATU-COMMENT(WRK-KOHBUNRI)
                        MOVE   RECE64-HKNCOMBI  TO
                                      TBL-HASSYO-HKNCOMBI(WRK-KOHBUNRI)
                      END-IF
                   WHEN      "80"
      *            ¡Ê¥ê¥Ï¥Ó¥ê¤¢¤ê¡Ë
                      IF  RECE64-SINDAI-COMMENT     =  "1"
                        MOVE    1    TO    TBL-HASSYO-SINDAI-FLG
                        COMPUTE  WRK-KOHBUNRI
                                   =    RECE64-KOHBUNRI  +  1
                        MOVE    1    TO
                               TBL-HASSYO-SINDAI-COMMENT(WRK-KOHBUNRI)
                        MOVE   RECE64-HKNCOMBI  TO
                                     TBL-HASSYO-HKNCOMBI(WRK-KOHBUNRI)
                      END-IF
                      IF  RECE64-KOKYUKI-COMMENT    =  "1"
                        MOVE    1    TO    TBL-HASSYO-KOKYUKI-FLG
                        COMPUTE  WRK-KOHBUNRI
                                   =    RECE64-KOHBUNRI  +  1
                        MOVE    1    TO
                               TBL-HASSYO-KOKYUKI-COMMENT(WRK-KOHBUNRI)
                        MOVE   RECE64-HKNCOMBI  TO
                                      TBL-HASSYO-HKNCOMBI(WRK-KOHBUNRI)
                      END-IF
                      IF  RECE64-NOUKE-COMMENT      =  "1"
                        MOVE    1    TO    TBL-HASSYO-NOUKE-FLG
                        COMPUTE  WRK-KOHBUNRI
                                   =    RECE64-KOHBUNRI  +  1
                        MOVE    1    TO
                               TBL-HASSYO-NOUKE-COMMENT(WRK-KOHBUNRI)
                        MOVE   RECE64-HKNCOMBI  TO
                                    TBL-HASSYO-HKNCOMBI(WRK-KOHBUNRI)
                      END-IF
                      IF  RECE64-UNDOKI-COMMENT     =  "1"
                        MOVE    1    TO    TBL-HASSYO-UNDOKI-FLG
                        COMPUTE  WRK-KOHBUNRI
                                   =    RECE64-KOHBUNRI  +  1
                        MOVE    1    TO
                               TBL-HASSYO-UNDOKI-COMMENT(WRK-KOHBUNRI)
                        MOVE   RECE64-HKNCOMBI  TO
                                     TBL-HASSYO-HKNCOMBI(WRK-KOHBUNRI)
                      END-IF
                      IF  RECE64-NANBYO-COMMENT     =  "1"
                        MOVE    1    TO    TBL-HASSYO-NANBYO-FLG
                        COMPUTE  WRK-KOHBUNRI
                                   =    RECE64-KOHBUNRI  +  1
                        MOVE    1    TO
                               TBL-HASSYO-NANBYO-COMMENT(WRK-KOHBUNRI)
                        MOVE   RECE64-HKNCOMBI  TO
                                     TBL-HASSYO-HKNCOMBI(WRK-KOHBUNRI)
                      END-IF
                      IF  RECE64-SYOGAI-COMMENT     =  "1"
                        MOVE    1    TO    TBL-HASSYO-SYOGAI-FLG
                        COMPUTE  WRK-KOHBUNRI
                                   =    RECE64-KOHBUNRI  +  1
                        MOVE    1    TO
                               TBL-HASSYO-SYOGAI-COMMENT(WRK-KOHBUNRI)
                        MOVE   RECE64-HKNCOMBI  TO
                                     TBL-HASSYO-HKNCOMBI(WRK-KOHBUNRI)
                      END-IF
                      IF  RECE64-SESSYOKU-COMMENT   =  "1"
                        MOVE    1    TO    TBL-HASSYO-SESSYOKU-FLG
                        COMPUTE  WRK-KOHBUNRI
                                   =    RECE64-KOHBUNRI  +  1
                        MOVE    1    TO
                               TBL-HASSYO-SESSYOKU-COMMENT(WRK-KOHBUNRI)
                        MOVE   RECE64-HKNCOMBI  TO
                                       TBL-HASSYO-HKNCOMBI(WRK-KOHBUNRI)
                      END-IF
               END-EVALUATE
      *
               PERFORM 990-RECE64-NEXT-SEC
      *
             END-PERFORM
      *
           END-IF
      *
      *    ¥Ñ¥é¥á¡¼¥¿£°£´¥Õ¥¡¥¤¥ë¤è¤êÊÔ½¸
           INITIALIZE                      RECE64-REC
           MOVE    RECE62-HOSPNUM      TO  RECE64-HOSPNUM
           MOVE    RECE62-SRYYM        TO  RECE64-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE64-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE64-PTID
           MOVE    RECE62-RECEKA       TO  RECE64-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE64-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE64-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE64-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE64-HOJOKBN
           MOVE    RECE62-TEKSTYMD     TO  RECE64-TEKSTYMD
           MOVE    ZERO                TO  RECE64-ZAINUM
      *
           START   RECE64-FILE
               KEY IS    >=   RECE64-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE64
               NOT INVALID
                   PERFORM 990-RECE64-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE64           =   1                   )
               OR     ( RECE62-HOSPNUM   NOT =   RECE64-HOSPNUM      )
               OR     ( RECE62-SRYYM     NOT =   RECE64-SRYYM        )
               OR     ( RECE62-NYUGAIKBN NOT =   RECE64-NYUGAIKBN    )
               OR     ( RECE62-PTID      NOT =   RECE64-PTID         )
               OR     ( RECE62-RECEKA    NOT =   RECE64-RECEKA       )
               OR     ( RECE62-TEISYUTUSAKI
                                         NOT =   RECE64-TEISYUTUSAKI )
               OR     ( RECE62-RECESYUBETU
                                         NOT =   RECE64-RECESYUBETU  )
               OR     ( RECE62-HKNJANUM-KEY
                                         NOT =   RECE64-HKNJANUM     )
               OR     ( RECE62-HOJOKBN-KEY
                                         NOT =   RECE64-HOJOKBN      )
               OR     ( RECE62-TEKSTYMD  NOT =   RECE64-TEKSTYMD     )
      *
               INITIALIZE    WRK-JIDO-SETCOM-AREA
               MOVE    ZERO   TO   FLG-KAISIEN
      *
               IF   WRK-HKNCOMBI-JIDO   =   ZERO
                 IF    WRK-KOHBUNRI-JIDO   =   RECE64-KOHBUNRI
                    IF  (RECE64-SRYKBN    NOT =  "95"  AND  "96")
      ***           AND (RECE64-ZAITEN    NOT =   ZERO)
      ***           AND (RECE64-ZAIKAISU  NOT =   ZERO)
                       MOVE   RECE64-HKNCOMBI   TO   WRK-HKNCOMBI-JIDO
                    END-IF
                 END-IF
               END-IF
      *
               EVALUATE      RECE64-SRYKBN
                   WHEN      "11"
                      MOVE   ZERO   TO  FLG-COMMENT
      *            ¡Ê½é¿Ç¡ÊÆ±ÆüÊ£¿ô¿ÇÎÅ²Ê¡Ë»»Äê²Ê¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SYODOU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE64-TENSOU-SEC
                      END-IF
                   WHEN      "13"
                      MOVE   ZERO   TO  FLG-COMMENT
      *            ¡ÊÌôºÞ´ÉÍý»ØÆ³ÎÁ»»ÄêÆü¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-YAKUKANSIDO-HANTEI-SEC
                      END-IF
      *            ¡ÊÆþ±¡±ÉÍÜ¿©»ö»ØÆ³ÎÁ»»ÄêÆü¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-NYUEIYOSIDO-HANTEI-SEC
                      END-IF
      *            ¡ÊÂà±¡Á°Ë¬Ìä»ØÆ³ÎÁ»»ÄêÆü¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-HOUMONSIDO-HANTEI-SEC
                      END-IF
      *            ¡Ê½ýÉÂ¼êÅö¶â°Õ¸«½ñ¸òÉÕÎÁ¸òÉÕÆü¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SYOBYO-HANTEI-SEC
                      END-IF
      *            ¡ÊÎÅÍÜÈñÆ±°Õ½ñ¸òÉÕÎÁ¸òÉÕÆü¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-RYOYO-HANTEI-SEC
                      END-IF
      *            ¡ÊÂà±¡»þÉþÌô»ØÆ³²Ã»»»»Äê»þ¤Î
      *              Á°·îÌôºÞ´ÉÍý»ØÆ³ÎÁ»»ÄêÆü¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-TAIIN-HANTEI-SEC
                      END-IF
      *            ¡Ê²ð¸î»Ù±çÏ¢·È»ØÆ³ÎÁ¤Î»»ÄêÆü¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KAISIEN-HANTEI-SEC
                      END-IF
      *            ¡Ê½¸ÃÄ±ÉÍÜ¿©»ö»ØÆ³ÎÁ¤Î»»ÄêÆü¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SYUEIYO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE64-TENSOU-SEC
                      END-IF
                   WHEN      "14"
                      MOVE   ZERO   TO  FLG-COMMENT
      *            ¡ÊË¬Ìä´Ç¸î»Ø¼¨ÎÁ»»ÄêÆü¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-HOUSIJI-HANTEI-SEC
                      END-IF
      *            ¡ÊºßÂð·ì±ÕÆ©ÀÏÉÑ²ó»ØÆ³´ÉÍýÎÁ»»Äê»þ¤Î
      *              ºßÂð·ì±ÕÆ©ÀÏ»ØÆ³´ÉÍýÎÁ»»ÄêÆü¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-HINKAI-HANTEI-SEC
                      END-IF
      *            ¡Ê±ý¿Ç¡¦ÆÃÊÌ±ý¿Ç»»ÄêÆü¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-OSIN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE64-TENSOU-SEC
                      END-IF
                   WHEN      "40"
                      MOVE   ZERO   TO  FLG-COMMENT
      *            ¡Ê¿Í¹©¿ÕÂ¡Åù¤Î»»ÄêÆü¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-JINZOUETC-HANTEI-SEC
                      END-IF
      *            ¡Ê¶É½ê±¢°µÊÄº¿½èÃÖ¤Î»»ÄêÆü¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KYOKUIN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE64-TENSOU-SEC
                      END-IF
                   WHEN      "50"
      *            ¡Ê¼ê½ÑÆü¤Î¥»¥Ã¥È¡Ë
                      PERFORM    2001-SHUJUTSU-HANTEI-SEC
                   WHEN      "54"
      *            ¡ÊËã¿ìÆü¤Î¥»¥Ã¥È¡Ë
                      PERFORM    2001-MASUI-HANTEI-SEC
                   WHEN      "60"
                   WHEN      "64"
                      MOVE   ZERO   TO  FLG-COMMENT
      *            ¡Ê¡Ê¸¡Áª¡ËÆü¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KENSEN-HANTEI-SEC
                      END-IF
      *            ¡Ê¼Â»ÜÆü¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KENSA-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE64-TENSOU-SEC
                      END-IF
                   WHEN      "70"
      *            ¡Ê£Ã£Ô¡¦£Í£Ò£É¼Â»ÜÆü¤Î¥»¥Ã¥È¡Ë
                      PERFORM    2001-CTMRI-HANTEI-SEC
                   WHEN      "80"
                      MOVE   ZERO   TO  FLG-COMMENT
      *            ¡ÊÀº¿À²ÊÂà±¡Á°Ë¬Ìä»ØÆ³ÎÁ»»ÄêÆü¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-HOUMONSIDO-SEISIN-HANTEI-SEC
                      END-IF
      *            ¡Ê¥ê¥Ï¥Ó¥ê¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                        IF    RECE64-RIHABIRI-COMMENT  NOT = SPACE
                          IF  RECE64-RIHABIRI-COMMENT      =  "1"
                            MOVE      99   TO   FLG-COMMENT
                            PERFORM   2001-RIHA-COMMENT-SEC
                          ELSE
                            MOVE      99   TO   FLG-COMMENT
                            PERFORM   2001-RIHAIGAKU-COMMENT-SEC
                          END-IF
                        END-IF
                      END-IF
      *            ¡Ê¡Ê¥ê¥ÏÁª¡ËÆü¤Î¥»¥Ã¥È¡Ë
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM        2001-RIHASEN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                             PERFORM   2099-RECE64-TENSOU-SEC
                      END-IF
                   WHEN      "90"                                       
                   WHEN      "92"
                      EVALUATE    RECE64-ZAISKBKBN
                        WHEN   "2"
                          PERFORM   2001-NYU-COMMENT-SEC
                        WHEN   "6"
                          MOVE   ZERO   TO  FLG-COMMENT
      *                  ¡ÊÀº¿À²Ê³ÖÎ¥ÉÂ¼¼´ÉÍý²Ã»»¤Î»»ÄêÆü¤Î¥»¥Ã¥È¡Ë
                          IF   FLG-COMMENT  =  ZERO
                            PERFORM    2001-KAKURI-HANTEI-SEC
                          END-IF
      *                  ¡Ê±ÉÍÜ¥µ¥Ý¡¼¥È¥Á¡¼¥à²Ã»»¤Î»»ÄêÆü¤Î¥»¥Ã¥È¡Ë
                          IF   FLG-COMMENT  =  ZERO
                            PERFORM    2001-EISAPO-HANTEI-SEC
                          END-IF
      *                  ¡Ê¸ÆµÛ¥±¥¢¥Á¡¼¥à²Ã»»¤Î»»ÄêÆü¤Î¥»¥Ã¥È¡Ë
                          IF   FLG-COMMENT  =  ZERO
                            PERFORM    2001-KOKKEA-HANTEI-SEC
                          END-IF
      *                  ¡Ê½Å¾É¼Ô²Ã»»¡ÊÀº¿ÀÎÅÍÜÉÂÅïÆþ±¡ÎÁ¡Ë¤Î»»ÄêÆü¤Î¥»¥Ã¥È¡Ë
                          IF   FLG-COMMENT  =  ZERO
                            PERFORM    2001-JYUSYO-HANTEI-SEC
                          END-IF
                          IF   FLG-COMMENT  =  ZERO
                            PERFORM    2099-RECE64-TENSOU-SEC
                          END-IF
                        WHEN   OTHER
                          PERFORM   2099-RECE64-TENSOU-SEC
                      END-EVALUATE
                   WHEN   OTHER
                      PERFORM       2099-RECE64-TENSOU-SEC
               END-EVALUATE
      *
               PERFORM 990-RECE64-NEXT-SEC
      *
           END-PERFORM
      *
           INITIALIZE    WRK-JIDO-SETCOM-AREA
      *
      *    Â¾ÊÝ¸±¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ¤ß¤Î¥³¥á¥ó¥È
      *
           IF   RECE62-NYUINRYOFLG   NOT =   SPACE
      *
              IF    RECE62-RECESYUBETU  =  8888
                  CONTINUE
              ELSE
                  PERFORM   2001-NYUINRYO-COMMENT-SEC
              END-IF
      *
           END-IF
      *
      *    ¸øÈñ¼õ¿ÇÆü¤Î¥³¥á¥ó¥È
      *
           IF   RECE62-KOHJNISSUTBLINF    =   ALL " "
              CONTINUE
           ELSE
              PERFORM   2001-KOHJYUSIN-COMMENT-SEC
           END-IF
      *
      *    Îà»öÏ·¿ÍÁë¸ýÄ§¼ý³Û¤Î¥³¥á¥ó¥È
      *
           IF   RECE62-KOH-SAIKEIFLG    =   1
              PERFORM   2001-KOHSAIKEI-COMMENT-SEC
           END-IF
      *
      *    È¯¾ÉÆü¤Î¥³¥á¥ó¥È
           IF   RECE62-SRYYM      >=    "200604"
               PERFORM   2001-HASSYO-HANTEI-SEC
           ELSE
               MOVE      ZERO            TO   FLG-COMOKIKAE
               MOVE      CONS-KYUSEI     TO   WRK-SRYCD
               PERFORM   2001-KYUSEIHASSYO-HANTEI-SEC
           END-IF
      *
      *    ºß»Ù±ç¥³¥á¥ó¥È
           PERFORM   VARYING   IDX    FROM   1   BY   1
                      UNTIL    IDX      >    5
              IF     TBL-ZAISIEN-COMMENT(IDX)   NOT =  ZERO
                  PERFORM    2001-ZAISIEN-COMMENT-SEC
              END-IF
           END-PERFORM
      *
      *    ¾ã³²¤Î¥³¥á¥ó¥È¡Ê£·£µºÐÅþÃ£·î¤ÎÆÃÎã¡Ë
      *
           IF   RECE62-RJNGAIKOHFTN(3)  =   1
              PERFORM   2001-KOUREI-TOKUREI-COMMENT-SEC
           END-IF
      *
      *    ÆÃÊÌ¤Î¥³¥á¥ó¥È¡ÊÆÃÊÌ¤Ê´Ø·¸¤Ë¤¢¤ëÂ¾±¡¡Ë
      *
           IF   RECE62-TAINFLG          =  "1"
              PERFORM   2001-TAIN-TOKUBETU-COMMENT-SEC
           END-IF
      *
      *****º¹¹þÊÔ½¸£²**********************
           PERFORM     250-SASHIKOMI2-SEC
      *
      *****´Ø·¸¥³¥á¥ó¥ÈÊÔ½¸****************
           PERFORM     260-SETCOM-HENSYU-SEC
      *
           PERFORM 990-RECE62-READ-SEC
      *
           IF  CNT-IN-COMMIT   =    300
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
      *    Â¾ÊÝ¸±¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ¤ß¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-NYUINRYO-COMMENT-SEC          SECTION.
      *
           INITIALIZE                      RECE649-REC
           MOVE    RECE62-HOSPNUM      TO  RECE649-HOSPNUM
           MOVE    RECE62-SRYYM        TO  RECE649-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE649-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE649-PTID
           MOVE    RECE62-RECEKA       TO  RECE649-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE649-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE649-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE649-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE649-HOJOKBN
           MOVE    RECE62-TEKSTYMD     TO  RECE649-TEKSTYMD
           MOVE    99999999            TO  RECE649-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE649-KOHBUNRI
           MOVE    "90"                TO  RECE649-SRYKBN
           MOVE    WRK-HKNCOMBI-JIDO   TO  RECE649-HKNCOMBI
      *
           MOVE    1                   TO  RECE649-MEISAISU
      *
           WRITE   RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           INITIALIZE                      RECE659-REC
           MOVE    RECE62-HOSPNUM      TO  RECE659-HOSPNUM
           MOVE    RECE62-SRYYM        TO  RECE659-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE659-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE659-PTID
           MOVE    RECE62-RECEKA       TO  RECE659-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE659-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE659-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE659-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE659-HOJOKBN
           MOVE    RECE62-TEKSTYMD     TO  RECE659-TEKSTYMD
           MOVE    99999999            TO  RECE659-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE659-KOHBUNRI
           MOVE    "90"                TO  RECE659-SRYKBN
      *    ¥³¥á¥ó¥È¤ÎÁÞÆþ
           MOVE    SPACE               TO  WRK-RECE65XX-REC
           INITIALIZE                      WRK-RECE65XX-REC
           MOVE    "810000001"         TO  WRK-RECE65XX-SRYCD(1 1)
           MOVE     ZERO               TO  WRK-RECE65XX-SRYSURYO(1 1)
           EVALUATE   RECE62-NYUINRYOFLG
             WHEN     "1"
               MOVE     10             TO  WRK-RECE65XX-YUKOKETA(1 1)
               MOVE    "¼«Èñ¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ" 
                                       TO  WRK-RECE65XX-NAME(1 1)
             WHEN     "2"
               MOVE     10             TO  WRK-RECE65XX-YUKOKETA(1 1)
               MOVE    "Ï«ºÒ¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ" 
                                       TO  WRK-RECE65XX-NAME(1 1)
             WHEN     "3"
               MOVE     11             TO  WRK-RECE65XX-YUKOKETA(1 1)
               MOVE    "¼«ÇåÀÕ¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ" 
                                       TO  WRK-RECE65XX-NAME(1 1)
             WHEN     "4"
               MOVE     10             TO  WRK-RECE65XX-YUKOKETA(1 1)
               MOVE    "¹ñÊÝ¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ" 
                                       TO  WRK-RECE65XX-NAME(1 1)
             WHEN     "5"
               MOVE     10             TO  WRK-RECE65XX-YUKOKETA(1 1)
               MOVE    "¼ÒÊÝ¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ" 
                                       TO  WRK-RECE65XX-NAME(1 1)
             WHEN     "6"
               MOVE     11             TO  WRK-RECE65XX-YUKOKETA(1 1)
               MOVE    "Â¾ÊÝ¸±¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ" 
                                       TO  WRK-RECE65XX-NAME(1 1)
             WHEN     "7"
               MOVE     10             TO  WRK-RECE65XX-YUKOKETA(1 1)
               MOVE    "¸ø³²¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ" 
                                       TO  WRK-RECE65XX-NAME(1 1)
             WHEN     "8"
               MOVE     10             TO  WRK-RECE65XX-YUKOKETA(1 1)
               MOVE    "¸åÊÝ¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ" 
                                       TO  WRK-RECE65XX-NAME(1 1)
           END-EVALUATE
      *
           MOVE    WRK-RECE65XX-SRYACT-TBL(1 1)
                                       TO  RECE659-SRYACT-TBL(1)
           WRITE   RECE659-REC
           ADD     1                   TO  CNT-RECE659
      *
           .
       2001-NYUINRYO-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºß»Ù±ç¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-ZAISIEN-COMMENT-SEC          SECTION.
      *
           INITIALIZE                      RECE649-REC
           MOVE    RECE62-HOSPNUM      TO  RECE649-HOSPNUM
           MOVE    RECE62-SRYYM        TO  RECE649-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE649-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE649-PTID
           MOVE    RECE62-RECEKA       TO  RECE649-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE649-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE649-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE649-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE649-HOJOKBN
           MOVE    RECE62-TEKSTYMD     TO  RECE649-TEKSTYMD
           COMPUTE WRK-ZAINUM    =  99999964  +  IDX
           MOVE    WRK-ZAINUM          TO  RECE649-ZAINUM
           COMPUTE WRK-KOHBUNRI  =  IDX  -  1
           MOVE    WRK-KOHBUNRI        TO  RECE649-KOHBUNRI
           MOVE    WRK-RECE64-SRYKBN   TO  RECE649-SRYKBN
           MOVE    TBL-ZAISIEN-HKNCOMBI(IDX)
                                       TO  RECE649-HKNCOMBI
      *
           MOVE    1                   TO  RECE649-MEISAISU
      *
           WRITE   RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           INITIALIZE                      RECE659-REC
           MOVE    RECE62-HOSPNUM      TO  RECE659-HOSPNUM
           MOVE    RECE62-SRYYM        TO  RECE659-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE659-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE659-PTID
           MOVE    RECE62-RECEKA       TO  RECE659-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE659-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE659-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE659-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE659-HOJOKBN
           MOVE    RECE62-TEKSTYMD     TO  RECE659-TEKSTYMD
           COMPUTE WRK-ZAINUM    =  99999964  +  IDX
           MOVE    WRK-ZAINUM          TO  RECE659-ZAINUM
           COMPUTE WRK-KOHBUNRI  =  IDX  -  1
           MOVE    WRK-KOHBUNRI        TO  RECE659-KOHBUNRI
           MOVE    WRK-RECE64-SRYKBN   TO  RECE659-SRYKBN
      *    ºß»Ù±ç¥³¥á¥ó¥È¤ÎÁÞÆþ
           MOVE    SPACE               TO  WRK-RECE65XX-REC
           INITIALIZE                      WRK-RECE65XX-REC
           MOVE    "810000001"         TO  WRK-RECE65XX-SRYCD(1 1)
           MOVE     ZERO               TO  WRK-RECE65XX-SRYSURYO(1 1)
           MOVE     3                  TO  WRK-RECE65XX-YUKOKETA(1 1)
           EVALUATE  TBL-ZAISIEN-COMMENT(IDX)
             WHEN      1
               MOVE    "ºß»Ù±ç"        TO  WRK-RECE65XX-NAME(1 1)
             WHEN      2
               MOVE    "ºß»ÙÉÂ"        TO  WRK-RECE65XX-NAME(1 1)
           END-EVALUATE
      *
           MOVE    WRK-RECE65XX-SRYACT-TBL(1 1)
                                       TO  RECE659-SRYACT-TBL(1)
           WRITE   RECE659-REC
           ADD     1                   TO  CNT-RECE659
      *
           .
       2001-ZAISIEN-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¾ã³²¤Î¥³¥á¥ó¥È¡Ê£·£µºÐÅþÃ£·î¤ÎÆÃÎã¡Ë
      *****************************************************************
       2001-KOUREI-TOKUREI-COMMENT-SEC          SECTION.
      *
           INITIALIZE                      RECE649-REC
           MOVE    RECE62-HOSPNUM      TO  RECE649-HOSPNUM
           MOVE    RECE62-SRYYM        TO  RECE649-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE649-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE649-PTID
           MOVE    RECE62-RECEKA       TO  RECE649-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE649-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE649-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE649-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE649-HOJOKBN
           MOVE    RECE62-TEKSTYMD     TO  RECE649-TEKSTYMD
           MOVE    99999998            TO  RECE649-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE649-KOHBUNRI
           MOVE    "99"                TO  RECE649-SRYKBN
           MOVE    WRK-HKNCOMBI-JIDO   TO  RECE649-HKNCOMBI
      *
           MOVE    1                   TO  RECE649-MEISAISU
      *
           WRITE   RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           INITIALIZE                      RECE659-REC
           MOVE    RECE62-HOSPNUM      TO  RECE659-HOSPNUM
           MOVE    RECE62-SRYYM        TO  RECE659-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE659-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE659-PTID
           MOVE    RECE62-RECEKA       TO  RECE659-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE659-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE659-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE659-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE659-HOJOKBN
           MOVE    RECE62-TEKSTYMD     TO  RECE659-TEKSTYMD
           MOVE    99999998            TO  RECE659-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE659-KOHBUNRI
           MOVE    "99"                TO  RECE659-SRYKBN
      *    ¥³¥á¥ó¥È¤ÎÁÞÆþ
           MOVE    SPACE               TO  WRK-RECE65XX-REC
           INITIALIZE                      WRK-RECE65XX-REC
           MOVE    "820000143"         TO  WRK-RECE65XX-SRYCD(1 1)
           MOVE     ZERO               TO  WRK-RECE65XX-SRYSURYO(1 1)
           MOVE     4                  TO  WRK-RECE65XX-YUKOKETA(1 1)
           MOVE    "¡Ê¾ã³²¡Ë"          TO  WRK-RECE65XX-NAME(1 1)
      *
           MOVE    WRK-RECE65XX-SRYACT-TBL(1 1)
                                       TO  RECE659-SRYACT-TBL(1)
           WRITE   RECE659-REC
           ADD     1                   TO  CNT-RECE659
      *
           .
       2001-KOUREI-TOKUREI-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆÃÊÌ¤Î¥³¥á¥ó¥È¡ÊÆÃÊÌ¤Ê´Ø·¸¤Ë¤¢¤ëÂ¾±¡¡Ë
      *****************************************************************
       2001-TAIN-TOKUBETU-COMMENT-SEC          SECTION.
      *
           INITIALIZE                      RECE649-REC
           MOVE    RECE62-HOSPNUM      TO  RECE649-HOSPNUM
           MOVE    RECE62-SRYYM        TO  RECE649-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE649-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE649-PTID
           MOVE    RECE62-RECEKA       TO  RECE649-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE649-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE649-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE649-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE649-HOJOKBN
           MOVE    RECE62-TEKSTYMD     TO  RECE649-TEKSTYMD
           MOVE    99999990            TO  RECE649-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE649-KOHBUNRI
           MOVE    "99"                TO  RECE649-SRYKBN
           MOVE    WRK-HKNCOMBI-JIDO   TO  RECE649-HKNCOMBI
      *
           MOVE    1                   TO  RECE649-MEISAISU
      *
           WRITE   RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           INITIALIZE                      RECE659-REC
           MOVE    RECE62-HOSPNUM      TO  RECE659-HOSPNUM
           MOVE    RECE62-SRYYM        TO  RECE659-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE659-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE659-PTID
           MOVE    RECE62-RECEKA       TO  RECE659-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE659-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE659-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE659-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE659-HOJOKBN
           MOVE    RECE62-TEKSTYMD     TO  RECE659-TEKSTYMD
           MOVE    99999990            TO  RECE659-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE659-KOHBUNRI
           MOVE    "99"                TO  RECE659-SRYKBN
      *    ¥³¥á¥ó¥È¤ÎÁÞÆþ
           MOVE    SPACE               TO  WRK-RECE65XX-REC
           INITIALIZE                      WRK-RECE65XX-REC
           MOVE    "810000001"         TO  WRK-RECE65XX-SRYCD(1 1)
           MOVE     ZERO               TO  WRK-RECE65XX-SRYSURYO(1 1)
           MOVE     4                  TO  WRK-RECE65XX-YUKOKETA(1 1)
           MOVE    "¡ÊÆÃÊÌ¡Ë"          TO  WRK-RECE65XX-NAME(1 1)
      *
           MOVE    WRK-RECE65XX-SRYACT-TBL(1 1)
                                       TO  RECE659-SRYACT-TBL(1)
           WRITE   RECE659-REC
           ADD     1                   TO  CNT-RECE659
      *
           .
       2001-TAIN-TOKUBETU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸øÈñ¼õ¿ÇÆü¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-KOHJYUSIN-COMMENT-SEC          SECTION.
      *
           INITIALIZE                TBL-NYU-DAY
      *
           MOVE    ZERO          TO  DAY-IDX
           MOVE    ZERO          TO  FLG-NYU-RENZOKU
      *
           PERFORM    VARYING    IDX  FROM   1   BY   1
                      UNTIL     (IDX      >   31)  OR
                                (DAY-IDX  >   31)
                IF   RECE62-KOHJNISSUTBL(IDX)   = SPACE
                      MOVE  ZERO  TO  FLG-NYU-RENZOKU
                ELSE
                      IF    FLG-NYU-RENZOKU  =  ZERO
                         ADD   1     TO  DAY-IDX
                         MOVE  IDX   TO  TBL-NYU-DAY-ST(DAY-IDX)
                      END-IF
                      MOVE  IDX      TO  TBL-NYU-DAY-ED(DAY-IDX)
                      MOVE  1        TO  FLG-NYU-RENZOKU
                END-IF
           END-PERFORM
      *
           PERFORM    VARYING    DAY-IDX  FROM   1   BY   1
                      UNTIL  (DAY-IDX                      >   31)  OR
                             (TBL-NYU-DAY-ST(DAY-IDX)  = ZERO)
                IF    TBL-NYU-DAY-ST(DAY-IDX) 
                                    =   TBL-NYU-DAY-ED(DAY-IDX)
                      MOVE  CONS-SANTEI-COMMENT1
                                    TO  TBL-NYU-DAY-CD(DAY-IDX)
                ELSE
                      MOVE  CONS-SANTEI-COMMENT2
                                    TO  TBL-NYU-DAY-CD(DAY-IDX)
                END-IF
           END-PERFORM
      *
           MOVE       ZERO       TO   IDZ
           INITIALIZE            WRK-FREE-AREA2
      *
           MOVE       5          TO   FLG-COMMENT
      *
           PERFORM  20011-FREE-COMMENT3-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                TO  IDX-RECE65
           MOVE    SPACE               TO  WRK-RECE65XX-REC
           INITIALIZE                      WRK-RECE65XX-REC
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                ADD    1       TO  IDX-RECE65
                MOVE   "810000001"
                               TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                               TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                MOVE   WK-FREE-NAME2(IDX)
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
           END-PERFORM 
      *
      *    ¼õ¿ÇÆü¤ÎÊÔ½¸¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
                INITIALIZE                       RECE659-REC
                MOVE    RECE62-HOSPNUM       TO  RECE659-HOSPNUM
                MOVE    RECE62-SRYYM         TO  RECE659-SRYYM
                MOVE    RECE62-NYUGAIKBN     TO  RECE659-NYUGAIKBN
                MOVE    RECE62-PTID          TO  RECE659-PTID
                MOVE    RECE62-RECEKA        TO  RECE659-RECEKA
                MOVE    RECE62-TEISYUTUSAKI  TO  RECE659-TEISYUTUSAKI
                MOVE    RECE62-RECESYUBETU   TO  RECE659-RECESYUBETU
                MOVE    RECE62-HKNJANUM-KEY  TO  RECE659-HKNJANUM
                MOVE    RECE62-HOJOKBN-KEY   TO  RECE659-HOJOKBN
                MOVE    RECE62-TEKSTYMD      TO  RECE659-TEKSTYMD
                MOVE    99999980             TO  RECE659-ZAINUM
                MOVE    WRK-KOHBUNRI-JIDO    TO  RECE659-KOHBUNRI
                MOVE    "99"                 TO  RECE659-SRYKBN
                ADD     1                    TO  WRK-RENNUM
                MOVE    WRK-RENNUM           TO  RECE659-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                         UNTIL   (IDX       >    5)   OR
                                 (FLG-WRKRECE65   =   1)
                   ADD         1             TO    IDX-RECE65
                   IF  WRK-RECE65XX-SRYCD(IDX-RECE65 1) = SPACE
                       MOVE    1             TO    FLG-WRKRECE65
                   ELSE
                       MOVE  WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
                                        TO   RECE659-SRYACT-TBL(IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF  
                END-PERFORM
                IF   RECE659-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE659-REC
                   ADD     1            TO   CNT-RECE659
                END-IF
           END-PERFORM
      *
      *
           INITIALIZE                      RECE649-REC
           MOVE    RECE62-HOSPNUM      TO  RECE649-HOSPNUM
           MOVE    RECE62-SRYYM        TO  RECE649-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE649-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE649-PTID
           MOVE    RECE62-RECEKA       TO  RECE649-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE649-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE649-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE649-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE649-HOJOKBN
           MOVE    RECE62-TEKSTYMD     TO  RECE649-TEKSTYMD
           MOVE    99999980            TO  RECE649-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE649-KOHBUNRI
           MOVE    "99"                TO  RECE649-SRYKBN
           MOVE    WRK-HKNCOMBI-JIDO   TO  RECE649-HKNCOMBI
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE649-MEISAISU
      *
           WRITE   RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           .
       2001-KOHJYUSIN-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Îà»öÏ·¿ÍÁë¸ýÄ§¼ý³Û¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-KOHSAIKEI-COMMENT-SEC          SECTION.
      *
           INITIALIZE                      RECE649-REC
           MOVE    RECE62-HOSPNUM      TO  RECE649-HOSPNUM
           MOVE    RECE62-SRYYM        TO  RECE649-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE649-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE649-PTID
           MOVE    RECE62-RECEKA       TO  RECE649-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE649-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE649-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE649-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE649-HOJOKBN
           MOVE    RECE62-TEKSTYMD     TO  RECE649-TEKSTYMD
           MOVE    99999997            TO  RECE649-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE649-KOHBUNRI
           MOVE    "99"                TO  RECE649-SRYKBN
           MOVE    WRK-HKNCOMBI-JIDO   TO  RECE649-HKNCOMBI
      *
           MOVE    1                   TO  RECE649-MEISAISU
      *
           WRITE   RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           INITIALIZE                      RECE659-REC
           MOVE    RECE62-HOSPNUM      TO  RECE659-HOSPNUM
           MOVE    RECE62-SRYYM        TO  RECE659-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE659-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE659-PTID
           MOVE    RECE62-RECEKA       TO  RECE659-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE659-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE659-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE659-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE659-HOJOKBN
           MOVE    RECE62-TEKSTYMD     TO  RECE659-TEKSTYMD
           MOVE    99999997            TO  RECE659-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE659-KOHBUNRI
           MOVE    "99"                TO  RECE659-SRYKBN
      *    ¥³¥á¥ó¥È¤ÎÁÞÆþ
           MOVE    SPACE               TO  WRK-RECE65XX-REC
           INITIALIZE                      WRK-RECE65XX-REC
           MOVE    "810000001"         TO  WRK-RECE65XX-SRYCD(1 1)
           MOVE     ZERO               TO  WRK-RECE65XX-SRYSURYO(1 1)
           MOVE     10                 TO  WRK-RECE65XX-YUKOKETA(1 1)
           MOVE    "Îà»öÏ·¿ÍÁë¸ýÄ§¼ý³Û¡¡"
                                       TO  WRK-RECE65XX-NAME(1 1)
      *
           MOVE     ZERO               TO  IDA
           MOVE     ZERO               TO  IDB
           MOVE     ZERO               TO  FLG-HENSYUKIN
           MOVE     RECE62-KOH-SAIKEI  TO  WRK-HENSYU-KINGAKU
           MOVE     SPACE              TO  WRK-HENSYU-KINGAKU2
           MOVE     SPACE              TO  WRK-HENSYU-KINGAKU3
      *
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-HENSYU-KINGAKU  TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      KANACHK-RC      =   ZERO
              MOVE  KANACHK-OUT-INPUT(1:18)
                                       TO  WRK-HENSYU-KINGAKU2
           END-IF
           PERFORM   VARYING   IDA   FROM   1   BY   2
                      UNTIL    IDA    >    18
              IF   WRK-HENSYU-KINGAKU2(IDA:2)  =  "£°"
                    IF  (FLG-HENSYUKIN = ZERO)
                    AND (IDA       NOT =  17 )
                       CONTINUE
                    ELSE
                       MOVE  1   TO  FLG-HENSYUKIN
                       ADD   1   TO  IDB
                       MOVE  WRK-HENSYU-KINGAKU2(IDA:2)  TO
                                 WRK-HENSYU-KINGAKU3(IDB:2)
                       ADD   1   TO  IDB
                    END-IF
              ELSE
                    MOVE  1   TO  FLG-HENSYUKIN
                    ADD   1   TO  IDB
                    MOVE  WRK-HENSYU-KINGAKU2(IDA:2)  TO
                              WRK-HENSYU-KINGAKU3(IDB:2)
                    ADD   1   TO  IDB
              END-IF
           END-PERFORM
      *
           COMPUTE  IDA  =  ( IDB  /  2 )  +  1
           COMPUTE  WRK-RECE65XX-YUKOKETA(1 1)  =
                    WRK-RECE65XX-YUKOKETA(1 1)  +  IDA
      *
           MOVE     WRK-HENSYU-KINGAKU3(1:IDB)
                                   TO  WRK-RECE65XX-NAME(1 1)(21:IDB)
           COMPUTE  IDA  =  21  +  IDB
           MOVE    "±ß"            TO  WRK-RECE65XX-NAME(1 1)(IDA:2)
      *
           MOVE    "1"             TO  WRK-RECE65XX-RECEPTFLG(1 1)
      *
           MOVE    WRK-RECE65XX-SRYACT-TBL(1 1)
                                   TO  RECE659-SRYACT-TBL(1)
           WRITE   RECE659-REC
           ADD     1               TO  CNT-RECE659
      *
           .
       2001-KOHSAIKEI-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    È¯¾ÉÆü¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-HASSYO-HANTEI-SEC          SECTION.
      *
           IF   TBL-HASSYO-KAITATU-FLG   =   1
             MOVE      ZERO            TO   FLG-COMOKIKAE
             MOVE      CONS-KYUSEI     TO   WRK-SRYCD
             PERFORM   2001-KYUSEIHASSYO-HANTEI-SEC
           END-IF
      ***
           IF   TBL-HASSYO-SINDAI-FLG   =   1
             MOVE      ZERO            TO   FLG-COMOKIKAE
             MOVE      CONS-SINDAI     TO   WRK-SRYCD
             MOVE      CONS-SINDAI-E   TO   WRK-SRYCD2
             PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
             IF        FLG-KYUSEI-COMMENT    =      1
                 PERFORM   VARYING   IDX14    FROM   1   BY   1
                            UNTIL    IDX14      >    5
                    IF   TBL-HASSYO-SINDAI-COMMENT(IDX14)   =  1
                        IF  WRK-RIHASANTEI-YMD  NOT = SPACE
                            MOVE   1      TO  FLG-RIHA
                            MOVE   WRK-RIHASANTEI-YMD
                                          TO  WRK-KYUSEIYMD
                            PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
                            PERFORM    20011-HASSYO-COMMENT-SEC
                        END-IF
                        PERFORM   VARYING   IDX16    FROM   1   BY   1
                                   UNTIL    IDX16      >   31
                          IF  TB-RIHASANTEI-DAY(IDX16)  NOT = ZERO
                            MOVE   2      TO  FLG-RIHA
                            MOVE   RECE62-SRYYM
                                          TO  WRK-KYUSEIYMD(1:6)
                            MOVE   IDX16  TO  WRK-KYUSEIYMD(7:2)
                            PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
                            PERFORM    20011-HASSYO-COMMENT-SEC
                          END-IF
                        END-PERFORM
                    END-IF
                 END-PERFORM
             END-IF
           END-IF
      ***
           IF   TBL-HASSYO-KOKYUKI-FLG   =   1
             MOVE      ZERO            TO   FLG-COMOKIKAE
             MOVE      CONS-KOKYUKI    TO   WRK-SRYCD
             MOVE      CONS-KOKYUKI-E  TO   WRK-SRYCD2
             PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
             IF        FLG-KYUSEI-COMMENT    =      1
                 PERFORM   VARYING   IDX14    FROM   1   BY   1
                            UNTIL    IDX14      >    5
                    IF   TBL-HASSYO-KOKYUKI-COMMENT(IDX14)   =  1
                        IF  WRK-RIHASANTEI-YMD  NOT = SPACE
                            MOVE   1      TO  FLG-RIHA
                            MOVE   WRK-RIHASANTEI-YMD
                                          TO  WRK-KYUSEIYMD
                            PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
                            PERFORM    20011-HASSYO-COMMENT-SEC
                        END-IF
                        PERFORM   VARYING   IDX16    FROM   1   BY   1
                                   UNTIL    IDX16      >   31
                          IF  TB-RIHASANTEI-DAY(IDX16)  NOT = ZERO
                            MOVE   2      TO  FLG-RIHA
                            MOVE   RECE62-SRYYM
                                          TO  WRK-KYUSEIYMD(1:6)
                            MOVE   IDX16  TO  WRK-KYUSEIYMD(7:2)
                            PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
                            PERFORM    20011-HASSYO-COMMENT-SEC
                          END-IF
                        END-PERFORM
                    END-IF
                 END-PERFORM
             END-IF
           END-IF
      ***
           IF   TBL-HASSYO-NOUKE-FLG   =   1
             MOVE      1               TO   FLG-COMOKIKAE
             MOVE      CONS-NOUKE      TO   WRK-SRYCD
             MOVE      CONS-NOUKE-E    TO   WRK-SRYCD2
             PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
             IF        FLG-KYUSEI-COMMENT    =      1
                 PERFORM   VARYING   IDX14    FROM   1   BY   1
                            UNTIL    IDX14      >    5
                    IF   TBL-HASSYO-NOUKE-COMMENT(IDX14)   =  1
                        IF  WRK-RIHASANTEI-YMD  NOT = SPACE
                            MOVE   1      TO  FLG-RIHA
                            MOVE   WRK-RIHASANTEI-YMD
                                          TO  WRK-KYUSEIYMD
                            PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
                            PERFORM    20011-HASSYO-COMMENT-SEC
                        END-IF
                        PERFORM   VARYING   IDX16    FROM   1   BY   1
                                   UNTIL    IDX16      >   31
                          IF  TB-RIHASANTEI-DAY(IDX16)  NOT = ZERO
                            MOVE   2      TO  FLG-RIHA
                            MOVE   RECE62-SRYYM
                                          TO  WRK-KYUSEIYMD(1:6)
                            MOVE   IDX16  TO  WRK-KYUSEIYMD(7:2)
                            PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
                            PERFORM    20011-HASSYO-COMMENT-SEC
                          END-IF
                        END-PERFORM
                    END-IF
                 END-PERFORM
             END-IF
           END-IF
      ***
           IF   TBL-HASSYO-UNDOKI-FLG   =   1
             MOVE      1                TO   FLG-COMOKIKAE
             MOVE      CONS-UNDOKI      TO   WRK-SRYCD
             MOVE      CONS-UNDOKI-E    TO   WRK-SRYCD2
             PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
             IF        FLG-KYUSEI-COMMENT    =      1
                 PERFORM   VARYING   IDX14    FROM   1   BY   1
                            UNTIL    IDX14      >    5
                    IF   TBL-HASSYO-UNDOKI-COMMENT(IDX14)   =  1
                        IF  WRK-RIHASANTEI-YMD  NOT = SPACE
                            MOVE   1      TO  FLG-RIHA
                            MOVE   WRK-RIHASANTEI-YMD
                                          TO  WRK-KYUSEIYMD
                            PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
                            PERFORM    20011-HASSYO-COMMENT-SEC
                        END-IF
                        PERFORM   VARYING   IDX16    FROM   1   BY   1
                                   UNTIL    IDX16      >   31
                          IF  TB-RIHASANTEI-DAY(IDX16)  NOT = ZERO
                            MOVE   2      TO  FLG-RIHA
                            MOVE   RECE62-SRYYM
                                          TO  WRK-KYUSEIYMD(1:6)
                            MOVE   IDX16  TO  WRK-KYUSEIYMD(7:2)
                            PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
                            PERFORM    20011-HASSYO-COMMENT-SEC
                          END-IF
                        END-PERFORM
                    END-IF
                 END-PERFORM
             END-IF
           END-IF
      ***
           IF   TBL-HASSYO-NANBYO-FLG   =   1
             MOVE      1                TO   FLG-COMOKIKAE
             MOVE      CONS-NANBYO      TO   WRK-SRYCD
             MOVE      CONS-NANBYO-E    TO   WRK-SRYCD2
             PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
             IF        FLG-KYUSEI-COMMENT    =      1
                 PERFORM   VARYING   IDX14    FROM   1   BY   1
                            UNTIL    IDX14      >    5
                    IF   TBL-HASSYO-NANBYO-COMMENT(IDX14)   =  1
                        IF  WRK-RIHASANTEI-YMD  NOT = SPACE
                            MOVE   1      TO  FLG-RIHA
                            MOVE   WRK-RIHASANTEI-YMD
                                          TO  WRK-KYUSEIYMD
                            PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
                            PERFORM    20011-HASSYO-COMMENT-SEC
                        END-IF
                        PERFORM   VARYING   IDX16    FROM   1   BY   1
                                   UNTIL    IDX16      >   31
                          IF  TB-RIHASANTEI-DAY(IDX16)  NOT = ZERO
                            MOVE   2      TO  FLG-RIHA
                            MOVE   RECE62-SRYYM
                                          TO  WRK-KYUSEIYMD(1:6)
                            MOVE   IDX16  TO  WRK-KYUSEIYMD(7:2)
                            PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
                            PERFORM    20011-HASSYO-COMMENT-SEC
                          END-IF
                        END-PERFORM
                    END-IF
                 END-PERFORM
             END-IF
           END-IF
      ***
           IF   TBL-HASSYO-SYOGAI-FLG   =   1
             MOVE      ZERO             TO   FLG-COMOKIKAE
             MOVE      CONS-SYOGAI      TO   WRK-SRYCD
             MOVE      CONS-SYOGAI-E    TO   WRK-SRYCD2
             PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
             IF        FLG-KYUSEI-COMMENT    =      1
                 PERFORM   VARYING   IDX14    FROM   1   BY   1
                            UNTIL    IDX14      >    5
                    IF   TBL-HASSYO-SYOGAI-COMMENT(IDX14)   =  1
                        IF  WRK-RIHASANTEI-YMD  NOT = SPACE
                            MOVE   1      TO  FLG-RIHA
                            MOVE   WRK-RIHASANTEI-YMD
                                          TO  WRK-KYUSEIYMD
                            PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
                            PERFORM    20011-HASSYO-COMMENT-SEC
                        END-IF
                        PERFORM   VARYING   IDX16    FROM   1   BY   1
                                   UNTIL    IDX16      >   31
                          IF  TB-RIHASANTEI-DAY(IDX16)  NOT = ZERO
                            MOVE   2      TO  FLG-RIHA
                            MOVE   RECE62-SRYYM
                                          TO  WRK-KYUSEIYMD(1:6)
                            MOVE   IDX16  TO  WRK-KYUSEIYMD(7:2)
                            PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
                            PERFORM    20011-HASSYO-COMMENT-SEC
                          END-IF
                        END-PERFORM
                    END-IF
                 END-PERFORM
             END-IF
           END-IF
      ***
           IF   TBL-HASSYO-SESSYOKU-FLG   =   1
             MOVE      ZERO               TO   FLG-COMOKIKAE
             MOVE      CONS-SESSYOKU      TO   WRK-SRYCD
             MOVE      CONS-SESSYOKU-E    TO   WRK-SRYCD2
             PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
             IF        FLG-KYUSEI-COMMENT    =      1
                 PERFORM   VARYING   IDX14    FROM   1   BY   1
                            UNTIL    IDX14      >    5
                    IF   TBL-HASSYO-SESSYOKU-COMMENT(IDX14)   =  1
                        IF  WRK-RIHASANTEI-YMD  NOT = SPACE
                            MOVE   1      TO  FLG-RIHA
                            MOVE   WRK-RIHASANTEI-YMD
                                          TO  WRK-KYUSEIYMD
                            PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
                            PERFORM    20011-HASSYO-COMMENT-SEC
                        END-IF
                        PERFORM   VARYING   IDX16    FROM   1   BY   1
                                   UNTIL    IDX16      >   31
                          IF  TB-RIHASANTEI-DAY(IDX16)  NOT = ZERO
                            MOVE   2      TO  FLG-RIHA
                            MOVE   RECE62-SRYYM
                                          TO  WRK-KYUSEIYMD(1:6)
                            MOVE   IDX16  TO  WRK-KYUSEIYMD(7:2)
                            PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
                            PERFORM    20011-HASSYO-COMMENT-SEC
                          END-IF
                        END-PERFORM
                    END-IF
                 END-PERFORM
             END-IF
           END-IF
      *
           .
       2001-HASSYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆü¤¬¤¢¤ë¤«È½Äê
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆü¤Î£±£¸£°Æü°ÊÆâ¤«È½Äê
      *****************************************************************
       2001-KYUSEIHASSYO-HANTEI-SEC          SECTION.
      *
           MOVE     ZERO     TO     FLG-KYUSEI-COMMENT
           MOVE     ZERO     TO     FLG-KYUSEI-COMMENT2
           MOVE     ZERO     TO     WRK-KYUSEIYMD
      *
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆü
      *
      *    »»ÄêÍúÎò¸¡º÷
           INITIALIZE                  SANTEI-REC
           MOVE    RECE62-HOSPNUM      TO  SANTEI-HOSPNUM
           MOVE    RECE62-PTID         TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      *
                MOVE    "tbl_santei"        TO      MCP-TABLE
                MOVE    "key3"              TO      MCP-PATHNAME
                PERFORM        900-SANTEI-READ-SEC
      *
                PERFORM   UNTIL  ( FLG-SANTEI         =   1 ) OR
                                 ( FLG-KYUSEI-COMMENT =   1 )
      *
                   PERFORM  VARYING   IDX3   FROM  31  BY  -1
                             UNTIL   ( IDX3               <    1 )
                               OR    ( FLG-KYUSEI-COMMENT =    1 )
      *
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
      *
                         MOVE   SANTEI-SRYYM   TO  WRK-FSANTEIYMD(1:6)
                         MOVE   IDX3           TO  WRK-FSANTEIYMD(7:2)
      *
                         INITIALIZE                  STS-AREA-DAY
                         INITIALIZE                  LNK-DAY6-AREA
                         MOVE    "61"            TO  LNK-DAY6-IRAI
                         MOVE    WRK-FSANTEIYMD  TO  LNK-DAY6-KIJUN
                         MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                         MOVE     179            TO  LNK-DAY6-ZOGEN
                         CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                         LNK-DAY6-AREA
                         MOVE    LNK-DAY6-KEISAN TO  WRK-AFTER180
      *
                         IF   WRK-FSANTEIYMD(1:6) <=  RECE62-SRYYM
                         AND  RECE62-SRYYM        <=  WRK-AFTER180(1:6)
                               MOVE    1    TO   FLG-KYUSEI-COMMENT
                               MOVE  WRK-FSANTEIYMD
                                            TO   WRK-KYUSEIYMD
                         END-IF
      *
                     END-IF
      *
                   END-PERFORM
      *
                   MOVE    "tbl_santei"        TO      MCP-TABLE
                   MOVE    "key3"              TO      MCP-PATHNAME
                   PERFORM        900-SANTEI-READ-SEC
      *
                END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_santei"        TO      MCP-TABLE
           MOVE    "key3"              TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾É½ªÎ»Æü
      *
           IF      FLG-KYUSEI-COMMENT    =      1
      *       »»ÄêÍúÎò¸¡º÷
              INITIALIZE                  SANTEI-REC
              MOVE    RECE62-HOSPNUM      TO  SANTEI-HOSPNUM
              MOVE    RECE62-PTID         TO  SANTEI-PTID
              MOVE    CONS-KYUSEI-END     TO  SANTEI-SRYCD
              MOVE    ZERO                TO  SANTEI-NYUGAIKBN
              MOVE    ZERO                TO  SANTEI-SRYKA
              MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
              MOVE    SANTEI-REC          TO  MCPDATA-REC
              MOVE    "tbl_santei"        TO      MCP-TABLE
              MOVE    "key3"              TO      MCP-PATHNAME
              PERFORM   900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
      *
                   MOVE    "tbl_santei"        TO      MCP-TABLE
                   MOVE    "key3"              TO      MCP-PATHNAME
                   PERFORM        900-SANTEI-READ-SEC
      *
                   PERFORM   UNTIL ( FLG-SANTEI          =   1         )
                              OR   ( SANTEI-SRYYM  < WRK-KYUSEIYMD(1:6))
                              OR   ( FLG-KYUSEI-COMMENT2 =   1         )
      *
                      PERFORM  VARYING   IDX3   FROM  31  BY  -1
                             UNTIL ( IDX3                  <    1      )
                              OR   ( FLG-KYUSEI-COMMENT2   =    1      )
      *
                        IF   SANTEI-DAY(IDX3)  NOT =  ZERO
      *
                           MOVE   SANTEI-SRYYM   TO  WRK-FSANTEIYMD(1:6)
                           MOVE   IDX3           TO  WRK-FSANTEIYMD(7:2)
      *
                           IF   WRK-KYUSEIYMD        <  WRK-FSANTEIYMD
                           AND  WRK-FSANTEIYMD(1:6)  <  RECE62-SRYYM
                                  MOVE    1    TO   FLG-KYUSEI-COMMENT2
                           END-IF
      *
                        END-IF
      *
                      END-PERFORM
      *
                      MOVE    "tbl_santei"        TO      MCP-TABLE
                      MOVE    "key3"              TO      MCP-PATHNAME
                      PERFORM        900-SANTEI-READ-SEC
      *
                   END-PERFORM
      *
              END-IF
      *
              MOVE    "tbl_santei"        TO      MCP-TABLE
              MOVE    "key3"              TO      MCP-PATHNAME
              PERFORM   900-CLOSE-SEC
           END-IF
      *
           IF      FLG-KYUSEI-COMMENT    =      1
           AND     FLG-KYUSEI-COMMENT2   =      ZERO
               PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
               IF   RECE62-SRYYM      >=    "200604"
                   PERFORM   VARYING   IDX14    FROM   1   BY   1
                              UNTIL    IDX14      >    5
                      IF   TBL-HASSYO-KAITATU-COMMENT(IDX14)   =  1
                          MOVE       WRK-KYUSEIYMD(7:2)  TO  IDX16
                          PERFORM    20011-HASSYO-COMMENT-SEC
                      END-IF
                   END-PERFORM
               ELSE
                   PERFORM    20011-KYUSEIHASSYO-COMMENT-SEC
               END-IF
           END-IF
      *
           .
       2001-KYUSEIHASSYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆüÅù¤Î¸¡º÷
      *****************************************************************
       20011-KYUSEIHASSYO-KENSAKU-SEC          SECTION.
      *
           MOVE    ZERO                  TO  FLG-KYUSEI-OK
           MOVE    SPACE                 TO  WRK-KYUSEI-NYUGAIKBN
           MOVE    SPACE                 TO  WRK-KYUSEI-SRYKA
           MOVE    ZERO                  TO  WRK-KYUSEI-ZAINUM
           MOVE    ZERO                  TO  FLG-COMOKIKAE2
           MOVE    ZERO                  TO  FLG-COMARI
           MOVE    ZERO                  TO  FLG-COMARI2
      *
           MOVE    ZERO                  TO  IDZ
           INITIALIZE                        WRK-FREE-AREA
      *++++
           PERFORM   VARYING   IDX15    FROM   1   BY   1
                      UNTIL   (IDX15          >    2)
                       OR     (FLG-KYUSEI-OK  =    1)
      *
           INITIALIZE                        SRYACT-REC
           MOVE    RECE62-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    IDX15                 TO  SRY-NYUGAIKBN
           MOVE    RECE62-PTID           TO  SRY-PTID
           MOVE    WRK-KYUSEIYMD(1:6)    TO  SRY-SRYYM
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (1)
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (2)
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (3)
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (4)
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (5)
           MOVE    SRYACT-REC            TO  MCPDATA-REC
           MOVE    "tbl_sryact"          TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"      TO     MCP-TABLE
               MOVE    "key7"            TO     MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               PERFORM  UNTIL   FLG-SRYACT     =   1
                         OR     FLG-KYUSEI-OK  =   1
                 INITIALIZE                     SRYACCT-REC
                 MOVE    SRY-HOSPNUM      TO    ACCT-HOSPNUM
                 MOVE    SRY-NYUGAIKBN    TO    ACCT-NYUGAIKBN
                 MOVE    SRY-PTID         TO    ACCT-PTID
                 MOVE    SRY-SRYKA        TO    ACCT-SRYKA
                 MOVE    SRY-SRYYM        TO    ACCT-SRYYM
                 MOVE    SRY-ZAINUM       TO    ACCT-ZAINUM
                 MOVE    SRYACCT-REC      TO    MCPDATA-REC
                 MOVE    "tbl_sryacct"    TO    MCP-TABLE
                 MOVE    "key31"          TO    MCP-PATHNAME
                 PERFORM   900-DBSELECT-SEC
                 IF     MCP-RC            =  ZERO
                    MOVE    "tbl_sryacct"    TO    MCP-TABLE
                    MOVE    "key31"          TO    MCP-PATHNAME
                    PERFORM   900-DBFETCH-SEC
                    IF      MCP-RC        =       ZERO
                        MOVE    MCPDATA-REC  TO  SRYACCT-REC
      *
                        MOVE    WRK-KYUSEIYMD(7:2)  TO  DAY-IDX2
                        IF   ACCT-DAY(1 DAY-IDX2)  NOT =  ZERO
      *                   ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¤Î¸¡º÷
                          INITIALIZE                 HKNCOMBI-REC
                          MOVE    ACCT-HOSPNUM   TO  COMB-HOSPNUM
                          MOVE    ACCT-PTID      TO  COMB-PTID
                          MOVE    ACCT-HKNCOMBI  TO  COMB-HKNCOMBINUM
                          MOVE    HKNCOMBI-REC   TO  MCPDATA-REC
                          MOVE    "tbl_hkncombi" TO  MCP-TABLE
                          MOVE    "key"          TO  MCP-PATHNAME
                          PERFORM   900-DBSELECT-SEC
                          IF      MCP-RC         =       ZERO
                             MOVE    "tbl_hkncombi" TO  MCP-TABLE
                             MOVE    "key"          TO  MCP-PATHNAME
                             PERFORM   900-DBFETCH-SEC
                             IF      MCP-RC        =       ZERO
                               MOVE   MCPDATA-REC  TO   HKNCOMBI-REC
                               IF   COMB-HKNNUM = "971" OR "973"
                                                        OR "975"
                                   CONTINUE
                               ELSE
                                   MOVE   1   TO  FLG-KYUSEI-OK
                                   MOVE   ACCT-NYUGAIKBN
                                              TO  WRK-KYUSEI-NYUGAIKBN
                                   MOVE   ACCT-SRYKA
                                              TO  WRK-KYUSEI-SRYKA
                                   MOVE   ACCT-ZAINUM
                                              TO  WRK-KYUSEI-ZAINUM
                               END-IF
                             END-IF
                          END-IF
                          INITIALIZE                   MCPAREA
                          MOVE    "tbl_hkncombi" TO  MCP-TABLE
                          MOVE    "key"          TO  MCP-PATHNAME
                          PERFORM   900-CLOSE-SEC
      *
                        END-IF
                    END-IF
                 END-IF
                 INITIALIZE                     MCPAREA
                 MOVE    "tbl_sryacct"    TO    MCP-TABLE
                 MOVE    "key31"          TO    MCP-PATHNAME
                 PERFORM   900-CLOSE-SEC
      *
                 MOVE    "tbl_sryact"          TO      MCP-TABLE
                 MOVE    "key7"                TO      MCP-PATHNAME
                 PERFORM   900-SRYACT-READ-SEC
               END-PERFORM
           END-IF
           INITIALIZE                        MCPAREA
           MOVE    "tbl_sryact"          TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           END-PERFORM
      *++++
           IF      WRK-KYUSEI-ZAINUM    NOT =    ZERO
              INITIALIZE                           SRYACT-REC
              MOVE    RECE62-HOSPNUM         TO    SRY-HOSPNUM
              MOVE    WRK-KYUSEI-NYUGAIKBN   TO    SRY-NYUGAIKBN
              MOVE    RECE62-PTID            TO    SRY-PTID
              MOVE    WRK-KYUSEI-SRYKA       TO    SRY-SRYKA
              MOVE    WRK-KYUSEIYMD(1:6)     TO    SRY-SRYYM
              MOVE    WRK-KYUSEI-ZAINUM      TO    SRY-ZAINUM
              MOVE    SRYACT-REC             TO    MCPDATA-REC
              MOVE    "tbl_sryact"           TO    MCP-TABLE
              MOVE    "key2"                 TO    MCP-PATHNAME
              PERFORM   900-DBSELECT-SEC
              IF     MCP-RC            =  ZERO
                 MOVE    "tbl_sryact"        TO    MCP-TABLE
                 MOVE    "key2"              TO    MCP-PATHNAME
                 PERFORM  900-SRYACT-READ-SEC
                 PERFORM  UNTIL   FLG-SRYACT     =   1
                   PERFORM  VARYING  IDX12  FROM  1  BY  1
                             UNTIL  (IDX12             >    5   )  OR
                                    (SRY-SRYCD(IDX12)  =   SPACE)
                      IF   (SRY-SRYCD(IDX12)(1:1)     =   "8"  )  OR
                           (SRY-SRYCD(IDX12)(1:3)     =   "008")
                       IF   SRY-INPUTNUM(IDX12)   NOT =   ZERO
                         INITIALIZE                      PTCOM-REC
                         MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
                         MOVE    SRY-PTID            TO  PTCOM-PTID
                         MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
                         MOVE    SRY-SRYCD(IDX12)    TO  PTCOM-SRYCD
                         MOVE    SRY-INPUTNUM(IDX12) TO  PTCOM-RENNUM
                         PERFORM 900-PTCOM-READ-SEC
                         ADD     1     TO    IDZ
                         MOVE    PTCOM-INPUTCOMENT
                                                TO  WK-FREE-NAME(IDZ)
                         PERFORM   VARYING   IDX13   FROM   200   BY  -1
                                    UNTIL    IDX13    <    1
                           IF  WK-FREE-NAME(IDZ)(IDX13:1)  NOT =  SPACE
                             COMPUTE  WK-FREE-YUKOKETA(IDZ)  =
                                      IDX13    /    2
                             MOVE     1     TO      IDX13
                           END-IF
                         END-PERFORM
                       ELSE
                         PERFORM      900-TENSU-TBL-SEC
                         ADD     1           TO  IDZ
                         MOVE    TNS-NAME    TO  WK-FREE-NAME(IDZ)
                         PERFORM   VARYING   IDX13   FROM   200   BY  -1
                                    UNTIL    IDX13    <    1
                           IF  WK-FREE-NAME(IDZ)(IDX13:1)  NOT =  SPACE
                             COMPUTE  WK-FREE-YUKOKETA(IDZ)  =
                                      IDX13    /    2
                             MOVE     1     TO      IDX13
                           END-IF
                         END-PERFORM
                       END-IF
                       IF   FLG-COMOKIKAE  =   1
                          EVALUATE  SRY-SRYCD(IDX12)
                            WHEN     "840000042"
                              IF   FLG-COMOKIKAE2 = ZERO
                                 MOVE    1    TO  FLG-COMOKIKAE2
                                 MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                                 MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                                 COMPUTE  IDZ  =  IDZ - 1
                              END-IF
                            WHEN     "840000101"
                              IF   FLG-COMOKIKAE2 = ZERO
                                 MOVE    2    TO  FLG-COMOKIKAE2
                                 MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                                 MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                                 COMPUTE  IDZ  =  IDZ - 1
                              END-IF
                          END-EVALUATE
                       END-IF
                       IF   FLG-COMOKIKAE  =   1
                          IF  SRY-SRYCD(IDX12) = "840000042" OR
                                                 "840000101"
                            MOVE    1    TO   FLG-COMARI2
                          ELSE
                            MOVE    1    TO   FLG-COMARI
                          END-IF
                       ELSE
                            MOVE    1    TO   FLG-COMARI
                       END-IF
                      END-IF
                   END-PERFORM
                   MOVE    "tbl_sryact"      TO    MCP-TABLE
                   MOVE    "key2"            TO    MCP-PATHNAME
                   PERFORM   900-SRYACT-READ-SEC
                 END-PERFORM
              END-IF
              INITIALIZE                     MCPAREA
              MOVE    "tbl_sryact"      TO    MCP-TABLE
              MOVE    "key2"            TO    MCP-PATHNAME
              PERFORM   900-CLOSE-SEC
           END-IF
      *++++
           IF      FLG-COMARI    =    ZERO
              INITIALIZE                      SANTEIPLUS-REC
              MOVE    RECE62-HOSPNUM      TO  SANTEIPLUS-HOSPNUM
              MOVE    RECE62-PTID         TO  SANTEIPLUS-PTID
              MOVE    WRK-KYUSEIYMD(1:6)  TO  SANTEIPLUS-SRYYM
              MOVE    WRK-SRYCD           TO  SANTEIPLUS-SRYCD
              MOVE    ZERO                TO  SANTEIPLUS-NYUGAIKBN
              MOVE    ZERO                TO  SANTEIPLUS-SRYKA
              MOVE    ZERO                TO  SANTEIPLUS-HKNCOMBINUM
              MOVE    WRK-KYUSEIYMD(7:2)  TO  SANTEIPLUS-DAYKEY
              MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
              MOVE    "tbl_santeiplus"    TO  MCP-TABLE
              MOVE    "key2"              TO  MCP-PATHNAME
              PERFORM 900-DBSELECT-SEC
              IF      MCP-RC               =       ZERO
                 MOVE    "tbl_santeiplus"     TO  MCP-TABLE
                 MOVE    "key2"               TO  MCP-PATHNAME
                 PERFORM  900-SANTEIPLUS-READ-SEC
                 PERFORM  UNTIL   FLG-SANTEIPLUS     =   1
                   IF  SANTEIPLUS-COMMENT  NOT =  SPACE
                      ADD     1                 TO  IDZ
                      MOVE  SANTEIPLUS-COMMENT  TO  WK-FREE-NAME(IDZ)
                      PERFORM   VARYING   IDX13   FROM   200   BY  -1
                                 UNTIL    IDX13    <     1
                        IF  WK-FREE-NAME(IDZ)(IDX13:1)  NOT =  SPACE
                          COMPUTE  WK-FREE-YUKOKETA(IDZ)  =
                                   IDX13    /    2
                          MOVE     1     TO      IDX13
                        END-IF
                      END-PERFORM
                   END-IF
                   IF   FLG-COMOKIKAE  =    1
                   AND  FLG-COMARI2    =   ZERO
                      IF   SANTEIPLUS-COMMENT(1:8)  = "¼ê½Ñ¡¡Æü"
                        IF   FLG-COMOKIKAE2 = ZERO
                           MOVE    1    TO  FLG-COMOKIKAE2
                           MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                           MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                           COMPUTE  IDZ  =  IDZ - 1
                        END-IF
                      END-IF
                      IF   SANTEIPLUS-COMMENT(1:12) = "µÞÀ­Áý°­¡¡Æü"
                        IF   FLG-COMOKIKAE2 = ZERO
                           MOVE    2    TO  FLG-COMOKIKAE2
                           MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                           MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                           COMPUTE  IDZ  =  IDZ - 1
                        END-IF
                      END-IF
                   END-IF
                   MOVE    "tbl_santeiplus"     TO  MCP-TABLE
                   MOVE    "key2"               TO  MCP-PATHNAME
                   PERFORM  900-SANTEIPLUS-READ-SEC
                 END-PERFORM
              END-IF
              MOVE    "tbl_santeiplus"     TO  MCP-TABLE
              MOVE    "key2"               TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
           END-IF
      *
           .
       20011-KYUSEIHASSYO-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ç¾·ì´É¼À´µµÞÀ­È¯¾ÉÆü¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       20011-KYUSEIHASSYO-COMMENT-SEC          SECTION.
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO        TO  IDX-RECE65
           MOVE    SPACE       TO  WRK-RECE65XX-REC
           INITIALIZE              WRK-RECE65XX-REC
      *
           ADD     1           TO  IDX-RECE65
           MOVE    "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
           MOVE    ZERO        TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
           MOVE    9           TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
           MOVE    "È¯¾É¡¡¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
           MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
           MOVE    TBL-DAY(DAY-IDX2)
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(7:4)
           MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
           MOVE    TBL-DAY(DAY-IDX2)
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(13:4)
      *
           PERFORM   VARYING    IDX12   FROM    1   BY  1
                      UNTIL   ( IDX12               >   49    )
                       OR     ( WK-FREE-NAME(IDX12) =   SPACE )
                ADD    1       TO  IDX-RECE65
                MOVE   "810000001"
                               TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                MOVE   WK-FREE-YUKOKETA(IDX12)
                               TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                MOVE   WK-FREE-NAME(IDX12)
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
           END-PERFORM
      *
      *    ²Ê¡¦ÆüÉÕ¤ÎÊÔ½¸¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
                INITIALIZE                       RECE659-REC
                MOVE    RECE62-HOSPNUM       TO  RECE659-HOSPNUM
                MOVE    RECE62-SRYYM         TO  RECE659-SRYYM
                MOVE    RECE62-NYUGAIKBN     TO  RECE659-NYUGAIKBN
                MOVE    RECE62-PTID          TO  RECE659-PTID
                MOVE    RECE62-RECEKA        TO  RECE659-RECEKA
                MOVE    RECE62-TEISYUTUSAKI  TO  RECE659-TEISYUTUSAKI
                MOVE    RECE62-RECESYUBETU   TO  RECE659-RECESYUBETU
                MOVE    RECE62-HKNJANUM-KEY  TO  RECE659-HKNJANUM
                MOVE    RECE62-HOJOKBN-KEY   TO  RECE659-HOJOKBN
                MOVE    RECE62-TEKSTYMD      TO  RECE659-TEKSTYMD
                MOVE    99999996             TO  RECE659-ZAINUM
                MOVE    WRK-KOHBUNRI-JIDO    TO  RECE659-KOHBUNRI
                MOVE    "99"                 TO  RECE659-SRYKBN
                ADD     1                    TO  WRK-RENNUM
                MOVE    WRK-RENNUM           TO  RECE659-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                          UNTIL   (IDX             >    5)   OR
                                  (FLG-WRKRECE65   =    1)
                   ADD         1             TO    IDX-RECE65
                   IF  WRK-RECE65XX-SRYCD(IDX-RECE65 1) = SPACE
                       MOVE    1             TO    FLG-WRKRECE65
                   ELSE
                       MOVE  WRK-RECE65XX-SRYACT-TBL (IDX-RECE65 1)
                                        TO   RECE659-SRYACT-TBL (IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
                END-PERFORM
                IF   RECE659-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE659-REC
                   ADD     1            TO   CNT-RECE659
                END-IF
           END-PERFORM
      *
      *
           INITIALIZE                      RECE649-REC
           MOVE    RECE62-HOSPNUM      TO  RECE649-HOSPNUM
           MOVE    RECE62-SRYYM        TO  RECE649-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE649-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE649-PTID
           MOVE    RECE62-RECEKA       TO  RECE649-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE649-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE649-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE649-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE649-HOJOKBN
           MOVE    RECE62-TEKSTYMD     TO  RECE649-TEKSTYMD
           MOVE    99999996            TO  RECE649-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE649-KOHBUNRI
           MOVE    "99"                TO  RECE649-SRYKBN
           MOVE    WRK-HKNCOMBI-JIDO   TO  RECE649-HKNCOMBI
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE649-MEISAISU
      *
           WRITE   RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           .
       20011-KYUSEIHASSYO-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    È¯¾ÉÆü¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       20011-HASSYO-COMMENT-SEC          SECTION.
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO        TO  IDX-RECE65
           MOVE    SPACE       TO  WRK-RECE65XX-REC
           INITIALIZE              WRK-RECE65XX-REC
      *
           ADD     1           TO  IDX-RECE65
           MOVE    "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
           MOVE    ZERO        TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
      *
           EVALUATE      WRK-SRYCD
             WHEN       CONS-KYUSEI
               MOVE    19   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE    "Ç¾·ì´É¼À´µµÞÀ­È¯¾É¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
               MOVE    21   TO  IDC1
               MOVE    27   TO  IDC2
               MOVE    33   TO  IDC3
               PERFORM 200111-HASSYO-COMMENT-HIZUKE-SEC
               COMPUTE WRK-ZAINUM    =  99998000
                                     + (IDX16 + (IDX14 * 31))
               MOVE    "40" TO  WRK-SRYKBN
             WHEN       CONS-SINDAI
               MOVE    23   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE    "¿´Âç·ì´É¼À´µ¥ê¥Ï¡§¼£ÎÅ³«»Ï¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
               MOVE    29   TO  IDC1
               MOVE    35   TO  IDC2
               MOVE    41   TO  IDC3
               PERFORM 200111-HASSYO-COMMENT-HIZUKE-SEC
               IF      FLG-RIHA      =  1
                 COMPUTE WRK-ZAINUM    =  99998200 + IDX14
               ELSE
                 COMPUTE WRK-ZAINUM    =  99998200
                                       + (IDX16 + (IDX14 * 31))
               END-IF
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-NOUKE
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                   MOVE    21  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE    "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    25  TO  IDC1
                   MOVE    31  TO  IDC2
                   MOVE    37  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-HIZUKE-SEC
                 WHEN       1
                   MOVE    21  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE    "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§¼ê½Ñ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    25  TO  IDC1
                   MOVE    31  TO  IDC2
                   MOVE    37  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-HIZUKE-SEC
                 WHEN       2
                   MOVE    23  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE
                       "Ç¾·ì´É¼À´µÅù¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    29  TO  IDC1
                   MOVE    35  TO  IDC2
                   MOVE    41  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-HIZUKE-SEC
               END-EVALUATE
               IF      FLG-RIHA      =  1
                 COMPUTE WRK-ZAINUM    =  99998400 + IDX14
               ELSE
                 COMPUTE WRK-ZAINUM    =  99998400
                                       + (IDX16 + (IDX14 * 31))
               END-IF
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-UNDOKI
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                   MOVE    18  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE    "±¿Æ°´ï¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    19  TO  IDC1
                   MOVE    25  TO  IDC2
                   MOVE    31  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-HIZUKE-SEC
                 WHEN       1
                   MOVE    18  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE    "±¿Æ°´ï¥ê¥Ï¡§¼ê½Ñ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    19  TO  IDC1
                   MOVE    25  TO  IDC2
                   MOVE    31  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-HIZUKE-SEC
                 WHEN       2
                   MOVE    20  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE    "±¿Æ°´ï¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    23  TO  IDC1
                   MOVE    29  TO  IDC2
                   MOVE    35  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-HIZUKE-SEC
               END-EVALUATE
               IF      FLG-RIHA      =  1
                 COMPUTE WRK-ZAINUM    =  99998600 + IDX14
               ELSE
                 COMPUTE WRK-ZAINUM    =  99998600
                                       + (IDX16 + (IDX14 * 31))
               END-IF
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-KOKYUKI
               MOVE    20   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE    "¸ÆµÛ´ï¥ê¥Ï¡§¼£ÎÅ³«»Ï¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
               MOVE    23   TO  IDC1
               MOVE    29   TO  IDC2
               MOVE    35   TO  IDC3
               PERFORM 200111-HASSYO-COMMENT-HIZUKE-SEC
               IF      FLG-RIHA      =  1
                 COMPUTE WRK-ZAINUM    =  99998800 + IDX14
               ELSE
                 COMPUTE WRK-ZAINUM    =  99998800
                                       + (IDX16 + (IDX14 * 31))
               END-IF
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-SESSYOKU
               MOVE    21   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE    "ÀÝ¿©µ¡Ç½ÎÅË¡¡§¼£ÎÅ³«»Ï¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
               MOVE    25   TO  IDC1
               MOVE    31   TO  IDC2
               MOVE    37   TO  IDC3
               PERFORM 200111-HASSYO-COMMENT-HIZUKE-SEC
               IF      FLG-RIHA      =  1
                 COMPUTE WRK-ZAINUM    =  99999000 + IDX14
               ELSE
                 COMPUTE WRK-ZAINUM    =  99999000
                                       + (IDX16 + (IDX14 * 31))
               END-IF
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-NANBYO
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                   MOVE    19  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE    "ÆñÉÂ´µ¼Ô¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    21  TO  IDC1
                   MOVE    27  TO  IDC2
                   MOVE    33  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-HIZUKE-SEC
                 WHEN       1
                   MOVE    19  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE    "ÆñÉÂ´µ¼Ô¥ê¥Ï¡§¼ê½Ñ¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    21  TO  IDC1
                   MOVE    27  TO  IDC2
                   MOVE    33  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-HIZUKE-SEC
                 WHEN       2
                   MOVE    21  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE    "ÆñÉÂ´µ¼Ô¥ê¥Ï¡§µÞÀ­Áý°­¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    25  TO  IDC1
                   MOVE    31  TO  IDC2
                   MOVE    37  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-HIZUKE-SEC
               END-EVALUATE
               IF      FLG-RIHA      =  1
                 COMPUTE WRK-ZAINUM    =  99999200 + IDX14
               ELSE
                 COMPUTE WRK-ZAINUM    =  99999200
                                       + (IDX16 + (IDX14 * 31))
               END-IF
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-SYOGAI
               MOVE    21   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE    "¾ã³²»ù¡Ê¼Ô¡Ë¥ê¥Ï¡§È¯¾É¡¡¡¡¡¡Ç¯¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
               MOVE    25   TO  IDC1
               MOVE    31   TO  IDC2
               MOVE    37   TO  IDC3
               PERFORM 200111-HASSYO-COMMENT-HIZUKE-SEC
               IF      FLG-RIHA      =  1
                 COMPUTE WRK-ZAINUM    =  99999400 + IDX14
               ELSE
                 COMPUTE WRK-ZAINUM    =  99999400
                                       + (IDX16 + (IDX14 * 31))
               END-IF
               MOVE    "80" TO  WRK-SRYKBN
           END-EVALUATE
      *
           PERFORM   VARYING    IDX12   FROM    1   BY  1
                      UNTIL   ( IDX12               >   49    )
                       OR     ( WK-FREE-NAME(IDX12) =   SPACE )
                ADD    1       TO  IDX-RECE65
                MOVE   "810000001"
                               TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                MOVE   WK-FREE-YUKOKETA(IDX12)
                               TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                MOVE   WK-FREE-NAME(IDX12)
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
           END-PERFORM
      *
      *    ²Ê¡¦ÆüÉÕ¤ÎÊÔ½¸¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
                INITIALIZE                       RECE659-REC
                MOVE    RECE62-HOSPNUM       TO  RECE659-HOSPNUM
                MOVE    RECE62-SRYYM         TO  RECE659-SRYYM
                MOVE    RECE62-NYUGAIKBN     TO  RECE659-NYUGAIKBN
                MOVE    RECE62-PTID          TO  RECE659-PTID
                MOVE    RECE62-RECEKA        TO  RECE659-RECEKA
                MOVE    RECE62-TEISYUTUSAKI  TO  RECE659-TEISYUTUSAKI
                MOVE    RECE62-RECESYUBETU   TO  RECE659-RECESYUBETU
                MOVE    RECE62-HKNJANUM-KEY  TO  RECE659-HKNJANUM
                MOVE    RECE62-HOJOKBN-KEY   TO  RECE659-HOJOKBN
                MOVE    RECE62-TEKSTYMD      TO  RECE659-TEKSTYMD
                MOVE    WRK-ZAINUM           TO  RECE659-ZAINUM
                COMPUTE WRK-KOHBUNRI  =  IDX14  -  1
                MOVE    WRK-KOHBUNRI         TO  RECE659-KOHBUNRI
                MOVE    WRK-SRYKBN           TO  RECE659-SRYKBN
                ADD     1                    TO  WRK-RENNUM
                MOVE    WRK-RENNUM           TO  RECE659-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                          UNTIL   (IDX             >    5)   OR
                                  (FLG-WRKRECE65   =    1)
                   ADD         1             TO    IDX-RECE65
                   IF  WRK-RECE65XX-SRYCD(IDX-RECE65 1) = SPACE
                       MOVE    1             TO    FLG-WRKRECE65
                   ELSE
                       MOVE  WRK-RECE65XX-SRYACT-TBL (IDX-RECE65 1)
                                        TO   RECE659-SRYACT-TBL (IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
                END-PERFORM
                IF   RECE659-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE659-REC
                   ADD     1            TO   CNT-RECE659
                END-IF
           END-PERFORM
      *
      *
           INITIALIZE                      RECE649-REC
           MOVE    RECE62-HOSPNUM      TO  RECE649-HOSPNUM
           MOVE    RECE62-SRYYM        TO  RECE649-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE649-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE649-PTID
           MOVE    RECE62-RECEKA       TO  RECE649-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE649-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE649-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE649-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE649-HOJOKBN
           MOVE    RECE62-TEKSTYMD     TO  RECE649-TEKSTYMD
           MOVE    WRK-ZAINUM          TO  RECE649-ZAINUM
           COMPUTE WRK-KOHBUNRI  =  IDX14  -  1
           MOVE    WRK-KOHBUNRI        TO  RECE649-KOHBUNRI
           MOVE    WRK-SRYKBN          TO  RECE649-SRYKBN
           MOVE    TBL-HASSYO-HKNCOMBI(IDX14)
                                       TO  RECE649-HKNCOMBI
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE649-MEISAISU
      *
           WRITE   RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           .
       20011-HASSYO-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    È¯¾ÉÆü¤Î¥³¥á¥ó¥ÈÊÔ½¸¡ÊÇ¯·îÆü¡Ë
      *****************************************************************
       200111-HASSYO-COMMENT-HIZUKE-SEC          SECTION.
      *
           INITIALIZE                         STS-AREA-DAY
           INITIALIZE                         LNK-DAY2-AREA
           MOVE    "21"                   TO  LNK-DAY2-IRAI
           MOVE    WRK-KYUSEIYMD          TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"    USING     STS-AREA-DAY
                                          LNK-DAY2-AREA
           IF      STS-DAY-RC1            =   ZERO
              MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
              INITIALIZE                       ORCSKANACHKAREA
              MOVE    "2"                  TO  KANACHK-SYORI
              MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
              CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
              IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:4)
                       TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(IDC1:4)
              END-IF
      *
              MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
              INITIALIZE                       ORCSKANACHKAREA
              MOVE    "2"                  TO  KANACHK-SYORI
              MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
              CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
              IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:4)
                       TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(IDC2:4)
              END-IF
      *
              MOVE    LNK-DAY2-EDTYMD1-D   TO  WRK-TAIHI-XX
              INITIALIZE                       ORCSKANACHKAREA
              MOVE    "2"                  TO  KANACHK-SYORI
              MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
              CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
              IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:4)
                       TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(IDC3:4)
              END-IF
           END-IF
      *
           .
       200111-HASSYO-COMMENT-HIZUKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ê¥Ï¥Ó¥êÈ¯¾ÉÆü¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       20011-RIHABIRIHASSYO-HANTEI-SEC          SECTION.
      *
           MOVE     ZERO     TO     FLG-KYUSEI-COMMENT2
           MOVE     SPACE    TO     WRK-RIHASANTEI-END
      *
      *    ¥ê¥Ï½ªÎ»Æü
      *
      *    »»ÄêÍúÎò¸¡º÷
           INITIALIZE                  SANTEI-REC
           MOVE    RECE62-HOSPNUM      TO  SANTEI-HOSPNUM
           MOVE    RECE62-PTID         TO  SANTEI-PTID
           MOVE    WRK-SRYCD2          TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      *
                MOVE    "tbl_santei"        TO  MCP-TABLE
                MOVE    "key3"              TO  MCP-PATHNAME
                PERFORM        900-SANTEI-READ-SEC
      *
                PERFORM   UNTIL  ( FLG-SANTEI          =  1 ) OR
                                 ( FLG-KYUSEI-COMMENT2 =  1 )
      *
                 IF    SANTEI-SRYYM  <  RECE62-SRYYM
                   PERFORM   VARYING   IDX3   FROM  31  BY  -1
                              UNTIL  ( IDX3                <   1 )
                                OR   ( FLG-KYUSEI-COMMENT2 =   1 )
      *
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
      *
                        MOVE  SANTEI-SRYYM  TO  WRK-RIHASANTEI-END(1:6)
                        MOVE  IDX3          TO  WRK-RIHASANTEI-END(7:2)
      *
                        MOVE  1             TO  FLG-KYUSEI-COMMENT2
      *
                     END-IF
      *
                   END-PERFORM
                 END-IF
      *
                 MOVE    "tbl_santei"        TO  MCP-TABLE
                 MOVE    "key3"              TO  MCP-PATHNAME
                 PERFORM        900-SANTEI-READ-SEC
      *
                END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      ********
      *
           MOVE     ZERO     TO     FLG-KYUSEI-COMMENT
           MOVE     ZERO     TO     FLG-KYUSEI-COMMENT2
           MOVE     ZERO     TO     WRK-KYUSEIYMD
           INITIALIZE               TBL-RIHASANTEI-DAY
           MOVE     SPACE    TO     WRK-RIHASANTEI-YMD
           MOVE     SPACE    TO     WRK-RIHASANTEI-YMD2
           MOVE     ZERO     TO     WRK-RIHASANTEI41-FLG
      *
      *    ¥ê¥Ï³«»ÏÆü
      *
      *    »»ÄêÍúÎò¸¡º÷
           INITIALIZE                  SANTEI-REC
           MOVE    RECE62-HOSPNUM      TO  SANTEI-HOSPNUM
           MOVE    RECE62-PTID         TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      *
                MOVE    "tbl_santei"        TO  MCP-TABLE
                MOVE    "key3"              TO  MCP-PATHNAME
                PERFORM        900-SANTEI-READ-SEC
      *
                PERFORM   UNTIL  ( FLG-SANTEI          =  1 ) OR
                                 ( FLG-KYUSEI-COMMENT2 =  1 )
      *
                 IF   SANTEI-SRYYM  <=  RECE62-SRYYM
                  IF   SANTEI-SRYYM  =  RECE62-SRYYM
                   PERFORM  VARYING   IDX3   FROM   1  BY  1
                             UNTIL    IDX3    >    31
      *
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
      *
      ****               MOVE   SANTEI-SRYYM   TO  WRK-KYUSEIYMD(1:6)
      ****               MOVE   "00"           TO  WRK-KYUSEIYMD(7:2)
                         MOVE   1     TO  TB-RIHASANTEI-DAY(IDX3)
      *
                         MOVE   1     TO  FLG-KYUSEI-COMMENT
      *
                         IF    (SANTEI-SRYYM = "200604")
                         AND   (IDX3         =  1      )
                            MOVE    1    TO   WRK-RIHASANTEI41-FLG
                         END-IF
                         IF    (SANTEI-SRYYM  NOT = "200604")
                         AND   (IDX3              =  1      )
                            MOVE   1     TO   FLG-KYUSEI-COMMENT2
                         END-IF
      *
                     END-IF
      *
                   END-PERFORM
                  ELSE
                   PERFORM  VARYING   IDX3   FROM  31  BY  -1
                             UNTIL   ( IDX3                <   1 )
                               OR    ( FLG-KYUSEI-COMMENT2 =   1 )
      *
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
      *
      ****               MOVE   SANTEI-SRYYM   TO  WRK-KYUSEIYMD(1:6)
      ****               MOVE   IDX3           TO  WRK-KYUSEIYMD(7:2)
      *
                       MOVE   SANTEI-SRYYM
                                        TO  WRK-RIHASANTEI-YMD2(1:6)
                       MOVE   IDX3      TO  WRK-RIHASANTEI-YMD2(7:2)
      *
                       IF  (WRK-RIHASANTEI-END  = SPACE              )
                       OR  (WRK-RIHASANTEI-END <= WRK-RIHASANTEI-YMD2)
      *
                         MOVE   SANTEI-SRYYM
                                          TO  WRK-RIHASANTEI-YMD(1:6)
                         MOVE   IDX3      TO  WRK-RIHASANTEI-YMD(7:2)
      *
                         MOVE   1         TO  FLG-KYUSEI-COMMENT
      *
                       END-IF
      *
                       MOVE     1         TO  FLG-KYUSEI-COMMENT2
      *
                       IF  (WRK-RIHASANTEI-END  = SPACE              )
                       OR  (WRK-RIHASANTEI-END <= WRK-RIHASANTEI-YMD2)
      *
                         IF    (SANTEI-SRYYM = "200604")
                         AND   (IDX3         =  1      )
                            MOVE   ZERO  TO   FLG-KYUSEI-COMMENT2
                         END-IF
                         IF     WRK-RIHASANTEI41-FLG  =  1
                            MOVE   ZERO  TO   TB-RIHASANTEI-DAY(1)
                         END-IF
      *
                       END-IF
      *
                     END-IF
      *
                   END-PERFORM
                  END-IF
                 END-IF
      *
                 MOVE    "tbl_santei"        TO  MCP-TABLE
                 MOVE    "key3"              TO  MCP-PATHNAME
                 PERFORM        900-SANTEI-READ-SEC
      *
                END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       20011-RIHABIRIHASSYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½é¿Ç¡ÊÆ±ÆüÊ£¿ô¿ÇÎÅ²Ê¡Ë¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-SYODOU-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-SYODOU-COMMENT   =    "1"
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                MOVE      "1"       TO     WRK-JIDO-SETCOMFLG
                MOVE       4        TO     WRK-JIDO-SETCOMATFLG
                MOVE       99       TO     FLG-COMMENT
                PERFORM    20011-SYODOU-COMMENT-SEC
           END-IF
      *
           .
       2001-SYODOU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½é¿Ç¡ÊÆ±ÆüÊ£¿ô¿ÇÎÅ²Ê¡Ë¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       20011-SYODOU-COMMENT-SEC          SECTION.
      *
           MOVE       ZERO       TO   IDZ
           INITIALIZE            WRK-FREE-AREA2
      *
           PERFORM   VARYING   IDX    FROM   1   BY   1
                      UNTIL   (IDX                       >   10  )
                       OR     (RECE64-SYODOU-SRYKA(IDX)  =  SPACE)
      *
             ADD      1                  TO  IDZ
             MOVE    "Ê£½é»»Äê¿ÇÎÅ²Ê¡§"  TO  WK-FREE-NAME2(IDZ)(1:16)
      *      ¿ÇÎÅ²Ê¤Î¼èÆÀ
             INITIALIZE                      SYS-1005-REC
             MOVE    "1005"              TO  SYS-1005-KANRICD
             MOVE    RECE64-SYODOU-SRYKA(IDX)
                                         TO  SYS-1005-KBNCD
             MOVE    RECE62-SRYYM        TO  SYS-1005-STYUKYMD(1:6)
             MOVE    "01"                TO  SYS-1005-STYUKYMD(7:2)
             MOVE    SYS-1005-STYUKYMD   TO  SYS-1005-EDYUKYMD
             MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
             MOVE    SYS-1005-REC        TO  MCPDATA-REC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM   900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM  900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI      =   ZERO
                  MOVE  SYS-1005-SRYKANAME(1:40)
                                         TO  WK-FREE-NAME2(IDZ)(17:40)
               END-IF
             END-IF
      *
             INITIALIZE                        MCPAREA
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
      *
             IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
               MOVE  "2"                   TO  WK-FREE-MEISKYFLG2(IDZ)
               MOVE  WRK-JIDO-SETCOMATFLG  TO  WK-FREE-SETCOMATFLG2(IDZ)
             END-IF
      *
             PERFORM   VARYING   IDW   FROM   200   BY  -1
                        UNTIL    IDW   <   1
                IF   WK-FREE-NAME2(IDZ)(IDW:1)  NOT =  SPACE
                     COMPUTE WK-FREE-YUKOKETA2(IDZ) =  IDW  /   2
                     MOVE    1   TO    IDW
                END-IF
             END-PERFORM
      *
           END-PERFORM
      *
           PERFORM  200111-RECE65-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
      *
           MOVE    RECE64-MEISAISU      TO  MAX-MEISAISU
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
                MOVE   "810000001"
                             TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
                MOVE   WK-FREE-MEISKYFLG2(IDX)
                          TO  WRK-RECE65XX-MEISKYFLG(MAX-MEISAISU 1)
                MOVE   WK-FREE-SETCOMATFLG2(IDX)
                          TO  WRK-RECE65XX-SETCOMATFLG(MAX-MEISAISU 1)
           END-PERFORM
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-SYODOU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÌôºÞ´ÉÍý»ØÆ³ÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-YAKUKANSIDO-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-YAKUKAN-COMMENT   =    "1"
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                MOVE      101      TO     WRK-JIDO-SETCOMATFLG
                MOVE       3       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-YAKUKANSIDO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡±ÉÍÜ¿©»ö»ØÆ³ÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-NYUEIYOSIDO-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-NYUEIYO-COMMENT   =    "1"
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                MOVE      102      TO     WRK-JIDO-SETCOMATFLG
                MOVE       3       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-NYUEIYOSIDO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Âà±¡Á°Ë¬Ìä»ØÆ³ÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-HOUMONSIDO-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-HOUMON-COMMENT   =    "1"
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                MOVE      103      TO     WRK-JIDO-SETCOMATFLG
                MOVE       3       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-HOUMONSIDO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½ýÉÂ¼êÅö¶â°Õ¸«½ñ¸òÉÕÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-SYOBYO-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-SYOBYO-COMMENT   =    "1"
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                MOVE       6       TO     WRK-JIDO-SETCOMATFLG
                MOVE       4       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-SYOBYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÎÅÍÜÈñÆ±°Õ½ñ¸òÉÕÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-RYOYO-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-RYOYO-COMMENT    =    "1"
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                MOVE       7       TO     WRK-JIDO-SETCOMATFLG
                MOVE       4       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-RYOYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Âà±¡»þÉþÌô»ØÆ³²Ã»»¤ò»»Äê»þ¡¢Åö·î¤ËÌôºÞ´ÉÍý»ØÆ³ÎÁ¤¬¤Ê¤¯¡¢Á°·î
      *    ¤ËÌôºÞ´ÉÍý»ØÆ³ÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-TAIIN-HANTEI-SEC          SECTION.
      *
           MOVE      ZERO            TO      WRK-TUKI-YAKUKAN
           MOVE      ZERO            TO      WRK-DAY-YAKUKAN
      *
           IF      RECE64-FLG-TAIIN-COMMENT   =    "1"
      *
             MOVE      ZERO            TO      FLG-YAKUKAN-COMMENT
      *
             IF    RECE62-SRYYM    >=  "200804"
               PERFORM   VARYING   IDX6   FROM    1   BY    1
                          UNTIL  ( IDX6   >   TBL-YAKUKAN-MAX       )
                            OR   ( FLG-YAKUKAN-COMMENT   =    1     )
                  MOVE   TBL-YAKUKAN(IDX6)     TO  WRK-YAKUKAN-SRYCD
                  PERFORM  2001X-TAIIN-HANTEI-TOUGETU-SEC
               END-PERFORM
             ELSE
                  MOVE   CONS-YAKUKAN-COMMENT  TO  WRK-YAKUKAN-SRYCD
                  PERFORM  2001X-TAIIN-HANTEI-TOUGETU-SEC
             END-IF
      *
             IF      FLG-YAKUKAN-COMMENT   =   ZERO
      *
               MOVE      ZERO            TO      WRK-ZENYYMM
      *
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    RECE64-SRYYM        TO  LNK-DAY6-KIJUN(1:6)
               MOVE    "01"                TO  LNK-DAY6-KIJUN(7:2)
               MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
               MOVE     -1                 TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN(1:6)
                                           TO  WRK-ZENYYMM
      *
               IF    RECE62-SRYYM    >=  "200804"
                 PERFORM   VARYING   IDX6   FROM    1   BY    1
                            UNTIL    IDX6   >   TBL-YAKUKAN-MAX
                    MOVE   TBL-YAKUKAN(IDX6)     TO  WRK-YAKUKAN-SRYCD
                    PERFORM  2001Y-TAIIN-HANTEI-ZENGETU-SEC
                 END-PERFORM
               ELSE
                    MOVE   CONS-YAKUKAN-COMMENT  TO  WRK-YAKUKAN-SRYCD
                    PERFORM  2001Y-TAIIN-HANTEI-ZENGETU-SEC
               END-IF
      *
             END-IF
      *
             MOVE       99                TO     FLG-COMMENT
             IF    WRK-DAY-YAKUKAN    NOT   =   ZERO
                MOVE    2                 TO     FLG-HANTEI
                MOVE    WRK-ZENYYMM(5:2)  TO     WRK-TUKI-YAKUKAN
             ELSE
                MOVE    1                 TO     FLG-HANTEI
             END-IF
             PERFORM    20011-TAIIN-COMMENT-SEC
      *
           END-IF
      *
           .
       2001-TAIIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Åö·î¤ËÌôºÞ´ÉÍý»ØÆ³ÎÁ¤¬¤¢¤ë¤«¥Á¥§¥Ã¥¯
      *****************************************************************
       2001X-TAIIN-HANTEI-TOUGETU-SEC          SECTION.
      *
           INITIALIZE                    SRYACT-REC
           MOVE    RECE64-HOSPNUM    TO  SRY-HOSPNUM
           MOVE    RECE64-NYUGAIKBN  TO  SRY-NYUGAIKBN
           MOVE    RECE64-PTID       TO  SRY-PTID
           MOVE    RECE64-SRYYM      TO  SRY-SRYYM
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (1)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (2)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (3)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (4)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (5)
           MOVE    SRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_sryact"      TO  MCP-TABLE
           MOVE    "key7"            TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
      *
           IF      MCP-RC               =       ZERO
             MOVE    "tbl_sryact"    TO  MCP-TABLE
             MOVE    "key7"          TO  MCP-PATHNAME
             PERFORM  900-SRYACT-READ-SEC
             IF      FLG-SRYACT  =  ZERO
                   MOVE    1         TO  FLG-YAKUKAN-COMMENT
             END-IF
           END-IF
      *
           INITIALIZE                    MCPAREA
           MOVE    "tbl_sryact"      TO  MCP-TABLE
           MOVE    "key7"            TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       2001X-TAIIN-HANTEI-TOUGETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Á°·î¤ËÌôºÞ´ÉÍý»ØÆ³ÎÁ¤¬¤¢¤ë¤«¥Á¥§¥Ã¥¯
      *****************************************************************
       2001Y-TAIIN-HANTEI-ZENGETU-SEC          SECTION.
      *
           INITIALIZE                    SRYACT-REC
           MOVE    RECE64-HOSPNUM    TO  SRY-HOSPNUM
           MOVE    RECE64-NYUGAIKBN  TO  SRY-NYUGAIKBN
           MOVE    RECE64-PTID       TO  SRY-PTID
           MOVE    WRK-ZENYYMM       TO  SRY-SRYYM
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (1)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (2)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (3)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (4)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (5)
           MOVE    SRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_sryact"      TO  MCP-TABLE
           MOVE    "key7"            TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
      *
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO   MCP-TABLE
               MOVE    "key7"           TO   MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
      *
               PERFORM  UNTIL   FLG-SRYACT   =  1
      *
                 INITIALIZE                  SRYACCT-REC
                 MOVE    SRY-HOSPNUM    TO   ACCT-HOSPNUM
                 MOVE    SRY-NYUGAIKBN  TO   ACCT-NYUGAIKBN
                 MOVE    SRY-PTID       TO   ACCT-PTID
                 MOVE    SRY-SRYKA      TO   ACCT-SRYKA
                 MOVE    SRY-SRYYM      TO   ACCT-SRYYM
                 MOVE    SRY-ZAINUM     TO   ACCT-ZAINUM
                 MOVE    SRYACCT-REC    TO   MCPDATA-REC
                 MOVE    "tbl_sryacct"  TO   MCP-TABLE
                 MOVE    "key31"        TO   MCP-PATHNAME
                 PERFORM   900-DBSELECT-SEC
      *
                 IF  MCP-RC               =  ZERO
                     MOVE    "tbl_sryacct"  TO  MCP-TABLE
                     MOVE    "key31"        TO  MCP-PATHNAME
                     PERFORM   900-DBFETCH-SEC
                     IF      MCP-RC       =  ZERO
                       MOVE    MCPDATA-REC  TO  SRYACCT-REC
                       PERFORM   VARYING   IDX  FROM   1   BY   1
                                  UNTIL    IDX    >   31
                         IF    ACCT-DAY(1 IDX)  NOT =  ZERO
                            IF    IDX   >   WRK-DAY-YAKUKAN
                                MOVE  IDX   TO  WRK-DAY-YAKUKAN
                            END-IF
                         END-IF
                       END-PERFORM
                     END-IF
                 END-IF
      *
                 INITIALIZE                  MCPAREA
                 MOVE    "tbl_sryacct"  TO   MCP-TABLE
                 MOVE    "key31"        TO   MCP-PATHNAME
                 PERFORM   900-CLOSE-SEC
      *
                 MOVE    "tbl_sryact"   TO   MCP-TABLE
                 MOVE    "key7"         TO   MCP-PATHNAME
                 PERFORM   900-SRYACT-READ-SEC
      *
               END-PERFORM
           END-IF
      *
           INITIALIZE                        MCPAREA
           MOVE    "tbl_sryact"      TO      MCP-TABLE
           MOVE    "key7"            TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       2001Y-TAIIN-HANTEI-ZENGETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ²ð¸î»Ù±çÏ¢·È»ØÆ³ÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-KAISIEN-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-KAISIEN-COMMENT   =    "1"
               MOVE    ZERO                TO  FLG-KAISIEN
               MOVE    SPACE               TO  WRK-KAISIEN-FSANTEIYMD
               INITIALIZE                      SANTEI-REC
               MOVE    RECE62-HOSPNUM      TO  SANTEI-HOSPNUM
               MOVE    RECE62-PTID         TO  SANTEI-PTID
               MOVE    CONS-KAISIEN        TO  SANTEI-SRYCD
               MOVE    ZERO                TO  SANTEI-NYUGAIKBN
               MOVE    ZERO                TO  SANTEI-SRYKA
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM   900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"      TO  MCP-TABLE
                 MOVE    "key3"            TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL  ( FLG-SANTEI   =  1 )
                            OR    ( FLG-KAISIEN  =  1 )
                   IF   SANTEI-SRYYM  <  RECE64-SRYYM
                     PERFORM  VARYING   IDX3   FROM  31  BY  -1
                               UNTIL  ( IDX3        <   1 )
                                OR    ( FLG-KAISIEN =   1 )
                       IF   SANTEI-DAY(IDX3)  NOT =  ZERO
                         MOVE    1    TO  FLG-KAISIEN
                         MOVE   SANTEI-SRYYM
                                      TO  WRK-KAISIEN-FSANTEIYMD(1:6)
                         MOVE   IDX3  TO  WRK-KAISIEN-FSANTEIYMD(7:2)
                       END-IF
                     END-PERFORM
                   END-IF
                   MOVE    "tbl_santei"    TO  MCP-TABLE
                   MOVE    "key3"          TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
               END-IF
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM  900-CLOSE-SEC
      *
               IF   FLG-KAISIEN  =   1
      *           ºÞ¤ÎºÇ½é¤ÎÆü¸¡º÷
                  MOVE       SPACE         TO  WRK-KAISIEN-YMD
                  PERFORM  VARYING   IDX3   FROM  1  BY  1
                            UNTIL    IDX3  >   31
                    IF   RECE64-KAISU(1 IDX3)  NOT =  ZERO
                       MOVE  RECE64-SRYYM  TO  WRK-KAISIEN-YMD(1:6)
                       MOVE  IDX3          TO  WRK-KAISIEN-YMD(7:2)
                       MOVE  31            TO  IDX3
                    END-IF
                  END-PERFORM
      *
                  IF   WRK-KAISIEN-YMD   NOT =  SPACE
                    MOVE  WRK-KAISIEN-YMD     TO  WRK-KAISIEN-CHECKYMD
                    PERFORM  2001-KAISIEN-PTNYUINRRK-CHECK-SEC
                    MOVE  WRK-KAISIEN-RRKNUM  TO  WRK-KAISIEN-RRKNUM-X
      *
                    MOVE  WRK-KAISIEN-FSANTEIYMD
                                              TO  WRK-KAISIEN-CHECKYMD
                    PERFORM  2001-KAISIEN-PTNYUINRRK-CHECK-SEC
                    MOVE  WRK-KAISIEN-RRKNUM  TO  WRK-KAISIEN-RRKNUM-Y
      *
                    IF  (WRK-KAISIEN-RRKNUM-X  NOT = ZERO           )
                    AND (WRK-KAISIEN-RRKNUM-Y  NOT = ZERO           )
                    AND (WRK-KAISIEN-RRKNUM-X = WRK-KAISIEN-RRKNUM-Y)
                       CONTINUE
                    ELSE
                       MOVE   ZERO   TO   FLG-KAISIEN
                    END-IF
                  ELSE
                       MOVE   ZERO   TO   FLG-KAISIEN
                  END-IF
               END-IF
      *
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE      106      TO     WRK-JIDO-SETCOMATFLG
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-KAISIEN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡ÍúÎò¥Á¥§¥Ã¥¯
      *****************************************************************
       2001-KAISIEN-PTNYUINRRK-CHECK-SEC          SECTION.
      *
           MOVE    ZERO                  TO  WRK-KAISIEN-RRKNUM
           INITIALIZE                        PTNYUINRRK-REC
           MOVE    RECE62-HOSPNUM        TO  PTNYUINRRK-HOSPNUM
           MOVE    RECE62-PTID           TO  PTNYUINRRK-PTID
           MOVE    WRK-KAISIEN-CHECKYMD  TO  PTNYUINRRK-NYUINYMD
           MOVE    WRK-KAISIEN-CHECKYMD  TO  PTNYUINRRK-TAIINYMD
           MOVE    PTNYUINRRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"      TO  MCP-TABLE
           MOVE    "key11"               TO  MCP-PATHNAME
           PERFORM  900-DBSELECT-SEC
           IF      MCP-RC   =   ZERO
             MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
             MOVE    "key11"             TO  MCP-PATHNAME
             PERFORM  900-PTNYUINRRK-READ-SEC
             IF    FLG-PTNYUINRRK  =  ZERO
                MOVE  PTNYUINRRK-RRKNUM  TO  WRK-KAISIEN-RRKNUM
             END-IF
           END-IF
           MOVE    "tbl_ptnyuinrrk"      TO  MCP-TABLE
           MOVE    "key11"               TO  MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       2001-KAISIEN-PTNYUINRRK-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½¸ÃÄ±ÉÍÜ¿©»ö»ØÆ³ÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-SYUEIYO-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-SYUEIYO-COMMENT   =    "1"
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE       18      TO     WRK-JIDO-SETCOMATFLG
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-SYUEIYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ë¬Ìä´Ç¸î»Ø¼¨ÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-HOUSIJI-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-HOUSIJI-COMMENT   =    "1"
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE       11      TO     WRK-JIDO-SETCOMATFLG
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-HOUSIJI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºßÂð·ì±ÕÆ©ÀÏÉÑ²ó»ØÆ³´ÉÍýÎÁ¤¬¤¢¤ë¤«È½Äê
      *    ºßÂð·ì±ÕÆ©ÀÏ»ØÆ³´ÉÍýÎÁ»»ÄêÆü¤ÎÎò·î¤Ç£²·î°ÊÆâ¤«È½Äê
      *****************************************************************
       2001-HINKAI-HANTEI-SEC          SECTION.
      *
           IF   RECE64-FLG-HINKAI-COMMENT   =    "1"
      *
             MOVE    ZERO    TO   WRK-DAY
             PERFORM  VARYING   IDX3   FROM  1  BY  1
                       UNTIL    IDX3  >   31
                 IF   RECE64-KAISU(1 IDX3)  NOT =  ZERO
                        MOVE  IDX3  TO  WRK-DAY
                        MOVE  31    TO  IDX3
                 END-IF
             END-PERFORM
      *
             MOVE     ZERO     TO     FLG-HINKAI-COMMENT
             MOVE     ZERO     TO     WRK-TOSEKIYMD
      *
      *      »»ÄêÍúÎò¸¡º÷
             INITIALIZE                  SANTEI-REC
             MOVE    RECE62-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE62-PTID         TO  SANTEI-PTID
             MOVE    CONS-TOSEKI         TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM   900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                    MOVE    "tbl_santei"        TO  MCP-TABLE
                    MOVE    "key3"              TO  MCP-PATHNAME
                    PERFORM        900-SANTEI-READ-SEC
                    PERFORM   UNTIL  ( FLG-SANTEI         =   1 ) OR
                                     ( FLG-HINKAI-COMMENT =   1 )
      *
                      IF   SANTEI-SRYYM  <=  RECE64-SRYYM
      ***              MOVE   SANTEI-SRYYM     TO  WRK-FSANTEIYMD(1:6)
      ***              MOVE   SANTEI-MSANTEID  TO  WRK-FSANTEIYMD(7:2)
                       MOVE   SANTEI-FSANTEIYMD
                                               TO  WRK-FSANTEIYMD
      *
                       INITIALIZE                  STS-AREA-DAY
                       INITIALIZE                  LNK-DAY6-AREA
                       MOVE    "61"            TO  LNK-DAY6-IRAI
                       MOVE    WRK-FSANTEIYMD  TO  LNK-DAY6-KIJUN
                       MOVE    "2"             TO  LNK-DAY6-ZOGENPTN
                       MOVE     2              TO  LNK-DAY6-ZOGEN
                       CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN TO  WRK-AFTER2
      *
                       MOVE    RECE64-SRYYM    TO  WRK-HINKAIYMD(1:6)
                       MOVE    WRK-DAY         TO  WRK-HINKAIYMD(7:2)
      *
                       IF     WRK-DAY      NOT  =  ZERO
                       AND    WRK-FSANTEIYMD   <=  WRK-HINKAIYMD
                         IF ((WRK-HINKAIYMD    <   WRK-AFTER2) AND
                             (RECE64-SRYYM     <= "201003"   ))
                         OR ( RECE64-SRYYM     >= "201004"    )
                             MOVE    1    TO   FLG-HINKAI-COMMENT
                             MOVE  WRK-FSANTEIYMD
                                          TO   WRK-TOSEKIYMD
                         END-IF
                       END-IF
                      END-IF
      * 
                      MOVE    "tbl_santei"     TO  MCP-TABLE
                      MOVE    "key3"           TO  MCP-PATHNAME
                      PERFORM        900-SANTEI-READ-SEC
                    END-PERFORM
             END-IF
      *
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
      *
             IF      FLG-HINKAI-COMMENT    =      1
                 INITIALIZE            WRK-JIDO-SETCOM-AREA
                 MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                 MOVE       12      TO     WRK-JIDO-SETCOMATFLG
                 MOVE       99      TO     FLG-COMMENT
                 PERFORM    20011-HINKAI-COMMENT-SEC
             END-IF
      *
           END-IF
      *
           .
       2001-HINKAI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºßÂð·ì±ÕÆ©ÀÏÉÑ²ó»ØÆ³´ÉÍýÎÁ¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       20011-HINKAI-COMMENT-SEC          SECTION.
      *
             PERFORM   200111-RECE65-TAIHI-SEC
      *
      *      £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
             MOVE    ZERO                 TO  IDX-RECE65
             MOVE    SPACE                TO  WRK-RECE65XX-REC
             INITIALIZE                       WRK-RECE65XX-REC
      *      ¥³¥á¥ó¥È¤ÎÁÞÆþ
             COMPUTE  MAX-MEISAISU        =   RECE64-MEISAISU  +  1
      *
             MOVE    "810000001"
                           TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
             MOVE    ZERO  TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
             MOVE    16    TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE    "½é²ó»ØÆ³´ÉÍý»»ÄêÆü¡¡¡¡¡¡·î¡¡¡¡Æü"
                           TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
             MOVE    WRK-TOSEKIYMD(5:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(21:4)
             MOVE    WRK-TOSEKIYMD(7:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(27:4)
      *
             IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
               MOVE  "2"   TO  WRK-RECE65XX-MEISKYFLG(MAX-MEISAISU 1)
               MOVE  WRK-JIDO-SETCOMATFLG
                           TO  WRK-RECE65XX-SETCOMATFLG(MAX-MEISAISU 1)
             END-IF
      *
             PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-HINKAI-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ±ý¿Ç¡¦ÆÃÊÌ±ý¿Ç¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-OSIN-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-OSIN-COMMENT   =    "1"
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE       6       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-OSIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿Í¹©¿ÕÂ¡Åù¤Î¥³¥á¥ó¥È¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-JINZOUETC-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-JINZOU-COMMENT   =    "1"
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
           END-IF
      *
           .
       2001-JINZOUETC-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¶É½ê±¢°µÊÄº¿½èÃÖ¤Î¥³¥á¥ó¥È¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-KYOKUIN-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-KYOKUIN-COMMENT   =    "1"
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
           END-IF
      *
           .
       2001-KYOKUIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ê½ÑÆü¤Î¥³¥á¥ó¥È¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-SHUJUTSU-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-SHUJUTU-COMMENT   =    SPACE
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                MOVE       1       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
           ELSE
                PERFORM    2099-RECE64-TENSOU-SEC
           END-IF
      *
           .
       2001-SHUJUTSU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ëã¿ìÆü¤Î¥³¥á¥ó¥È¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-MASUI-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-MASUI-COMMENT   =    SPACE
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                MOVE       8       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
           ELSE
                PERFORM    2099-RECE64-TENSOU-SEC
           END-IF
      *
           .
       2001-MASUI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¡Ê¸¡Áª¡Ë¤Î¥³¥á¥ó¥È¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-KENSEN-HANTEI-SEC          SECTION.
      *
           IF     RECE64-SAFLG   =    "2"
               MOVE       99      TO     FLG-COMMENT
               PERFORM    20011-KENSENTOU-COMMENT-SEC
           END-IF
      *
           .
       2001-KENSEN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¡Ê¸¡Áª¡ËÅù¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       20011-KENSENTOU-COMMENT-SEC          SECTION.
      *
           PERFORM   200111-RECE65-TAIHI-SEC
      *
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
      *
           PERFORM
               UNTIL  ( FLG-RECE65           =   1                   )
               OR     ( RECE64-HOSPNUM   NOT =   RECE65-HOSPNUM      )
               OR     ( RECE64-SRYYM     NOT =   RECE65-SRYYM        )
               OR     ( RECE64-NYUGAIKBN NOT =   RECE65-NYUGAIKBN    )
               OR     ( RECE64-PTID      NOT =   RECE65-PTID         )
               OR     ( RECE64-RECEKA    NOT =   RECE65-RECEKA       )
               OR     ( RECE64-TEISYUTUSAKI
                                         NOT =   RECE65-TEISYUTUSAKI )
               OR     ( RECE64-RECESYUBETU
                                         NOT =   RECE65-RECESYUBETU  )
               OR     ( RECE64-HKNJANUM  NOT =   RECE65-HKNJANUM     )
               OR     ( RECE64-HOJOKBN   NOT =   RECE65-HOJOKBN      )
               OR     ( RECE64-TEKSTYMD  NOT =   RECE65-TEKSTYMD     )
               OR     ( RECE64-ZAINUM    NOT =   RECE65-ZAINUM       )
               OR     ( RECE64-ZAIREN    NOT =   RECE65-ZAIREN       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX               >   5     )
                         OR      ( RECE65-SRYCD(IDX) =   SPACE )
                    ADD    1             TO      IDX-RECE65
                    MOVE   RECE65-SRYACT-TBL(IDX)
                             TO   WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
      *++++++
                    IF    IDX-RECE65  =  RECE64-MEISAISU
      *
                      IF  WRK-RECE65XX-SRYCD(IDX-RECE65 1) = "810000001"
                      OR  WRK-RECE65XX-SRYCD(IDX-RECE65 1)(1:3)  = "008"
      *
                        COMPUTE   IDV  =
                         (WRK-RECE65XX-YUKOKETA(IDX-RECE65 1) * 2) + 1
      *
                        IF   WRK-RECE65XX-YUKOKETA(IDX-RECE65 1) <=  36
                          MOVE    "¡¡¡ß"   TO
                           WRK-RECE65XX-INPUTCOMENT(IDX-RECE65 1)(IDV:4)
                        END-IF
                        IF   WRK-RECE65XX-YUKOKETA(IDX-RECE65 1) <=  96
                          MOVE    "¡¡¡ß"   TO
                           WRK-RECE65XX-NAME(IDX-RECE65 1)(IDV:4)
                        END-IF
      *
                        COMPUTE   IDV  =  IDV  +  4
      *
                        MOVE    RECE64-ZAIKAISU   TO   DAY-IDX2
                        PERFORM 20011-KAISU-HENSYU-SEC
                        IF   WRK-RECE65XX-YUKOKETA(IDX-RECE65 1) <=  36
                          MOVE    WRK-HENSYU-KAISU2  TO
                           WRK-RECE65XX-INPUTCOMENT(IDX-RECE65 1)(IDV:4)
                        END-IF
                        IF   WRK-RECE65XX-YUKOKETA(IDX-RECE65 1) <=  96
                          MOVE    WRK-HENSYU-KAISU2  TO
                           WRK-RECE65XX-NAME(IDX-RECE65 1)(IDV:4)
                          COMPUTE WRK-RECE65XX-YUKOKETA(IDX-RECE65 1) =
                                  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1) +
                                  4
                        END-IF
      *
                      END-IF
      *
                    END-IF
      *++++++
               END-PERFORM
      *
               PERFORM 990-RECE65-NEXT-SEC
      *
           END-PERFORM
      *
      *    ¥³¥á¥ó¥È¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
                    INITIALIZE                        RECE659-REC
                    MOVE    WRK-RECE65-KEY       TO   RECE659-KEY
                    ADD     1                    TO   WRK-RENNUM
                    MOVE    WRK-RENNUM           TO   RECE659-RENNUM
                    MOVE    WRK-RECE65-KOHBUNRI  TO   RECE659-KOHBUNRI
                    MOVE    WRK-RECE65-PTNUM     TO   RECE659-PTNUM
                    MOVE    WRK-RECE65-SRYKA     TO   RECE659-SRYKA
                    MOVE    WRK-RECE65-SRYKBN    TO   RECE659-SRYKBN
                    MOVE    WRK-RECE65-SRYSYUKBN TO   RECE659-SRYSYUKBN
                    MOVE    WRK-RECE65-JIHIMONEY TO   RECE659-JIHIMONEY
                    PERFORM  VARYING   IDX   FROM   1   BY   1
                             UNTIL   (IDX       >    5)   OR
                                     (FLG-WRKRECE65   =   1)
                       ADD         1             TO    IDX-RECE65
                       IF  WRK-RECE65XX-SRYCD(IDX-RECE65 1) = SPACE
                           MOVE    1             TO    FLG-WRKRECE65
                       ELSE
                           MOVE  WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
                                            TO   RECE659-SRYACT-TBL(IDX)
                           ADD   1          TO   WRK-CNT-MEISAI
                       END-IF
                    END-PERFORM
                    IF   RECE659-SRYCD(1)  NOT =  SPACE
                       WRITE   RECE659-REC
                       ADD     1            TO   CNT-RECE659
                    END-IF
           END-PERFORM
      *
           MOVE    RECE64-REC        TO      RECE649-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE649-MEISAISU
           WRITE   RECE649-REC
           ADD     1                 TO      CNT-RECE649
      *
           .
       20011-KENSENTOU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ²ó¿ôÊÔ½¸
      *****************************************************************
       20011-KAISU-HENSYU-SEC          SECTION.
      *
           MOVE    SPACE                TO  WRK-HENSYU-KAISU
           MOVE    SPACE                TO  WRK-HENSYU-KAISU2
      *
           MOVE    DAY-IDX2             TO  WRK-HENSYU-KAISU
           IF      WRK-HENSYU-KAISU(1:1) =  "0"
               MOVE      SPACE          TO  WRK-HENSYU-KAISU(1:1)
           END-IF
      *
           INITIALIZE                       ORCSKANACHKAREA
           MOVE    "2"                  TO  KANACHK-SYORI
           MOVE    WRK-HENSYU-KAISU     TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
           IF      KANACHK-RC      =   ZERO
                MOVE    KANACHK-OUT-INPUT(1:4)
                                        TO  WRK-HENSYU-KAISU2
           END-IF
      *
           .
       20011-KAISU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸¡ºº¡Ê£Â£Î£Ð¡¢£Î£Ô¡Ý£ð£ò£ï£Â£Î£Ð¡¢£È£Á£Î£Ð¡Ë¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-KENSA-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-KENSA-COMMENT    =    "1"
               INITIALIZE            WRK-JIDO-SETCOM-AREA
      ***      MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
      ***      MOVE       15      TO     WRK-JIDO-SETCOMATFLG
               MOVE       7       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-KENSA-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ã£Ô¡¦£Í£Ò£É¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-CTMRI-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-CTMRI-COMMENT   =    "1"
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                MOVE       2       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
           ELSE
                PERFORM    2099-RECE64-TENSOU-SEC
           END-IF
      *
           .
       2001-CTMRI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Àº¿À²ÊÂà±¡Á°Ë¬Ìä»ØÆ³ÎÁ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-HOUMONSIDO-SEISIN-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-HOUSEI-COMMENT   =    "1"
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                MOVE      105      TO     WRK-JIDO-SETCOMATFLG
                MOVE       3       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-HOUMONSIDO-SEISIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Âà±¡»þÉþÌô»ØÆ³²Ã»»¤ò»»Äê»þ¤ÎÁ°·îÌôºÞ´ÉÍý»ØÆ³ÎÁ¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       20011-TAIIN-COMMENT-SEC          SECTION.
      *
           MOVE       RECE64-REC       TO  RECE649-REC
           EVALUATE   FLG-HANTEI
             WHEN       1
                ADD     1              TO  RECE649-MEISAISU
             WHEN       2
                ADD     2              TO  RECE649-MEISAISU
           END-EVALUATE
           MOVE   "1"                  TO  RECE649-SETCOMFLG
           WRITE      RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           INITIALIZE                      RECE65-REC
           MOVE    RECE64-HOSPNUM      TO  RECE65-HOSPNUM
           MOVE    RECE64-SRYYM        TO  RECE65-SRYYM
           MOVE    RECE64-NYUGAIKBN    TO  RECE65-NYUGAIKBN
           MOVE    RECE64-PTID         TO  RECE65-PTID
           MOVE    RECE64-RECEKA       TO  RECE65-RECEKA
           MOVE    RECE64-TEISYUTUSAKI TO  RECE65-TEISYUTUSAKI
           MOVE    RECE64-RECESYUBETU  TO  RECE65-RECESYUBETU
           MOVE    RECE64-HKNJANUM     TO  RECE65-HKNJANUM
           MOVE    RECE64-HOJOKBN      TO  RECE65-HOJOKBN
           MOVE    RECE64-TEKSTYMD     TO  RECE65-TEKSTYMD
           MOVE    RECE64-ZAINUM       TO  RECE65-ZAINUM
           MOVE    RECE64-ZAIREN       TO  RECE65-ZAIREN
           START   RECE65-FILE
                 KEY IS    >=   RECE65-KEY
                 INVALID
                     MOVE    1         TO  FLG-RECE65
                 NOT INVALID
                     PERFORM 990-RECE65-NEXT-SEC
           END-START
      *
           MOVE    RECE65-REC           TO  RECE659-REC
           EVALUATE   FLG-HANTEI
             WHEN       1
               MOVE    RECE65-SRYACT-TBL(1) TO  RECE659-SRYACT-TBL(1)
               MOVE    RECE65-SRYACT-TBL(2) TO  RECE659-SRYACT-TBL(3)
               MOVE    RECE65-SRYACT-TBL(3) TO  RECE659-SRYACT-TBL(4)
               MOVE    RECE65-SRYACT-TBL(4) TO  RECE659-SRYACT-TBL(5)
      *
               MOVE    SPACE                TO  RECE659-SRYACT-TBL(2)
               INITIALIZE                       RECE659-SRYACT-TBL(2)
               MOVE    "810000001"          TO  RECE659-SRYCD(2)
               MOVE    ZERO                 TO  RECE659-SRYSURYO(2)
               PERFORM  200111-SANTEI-DAY-SEC
               PERFORM  200111-TAIIN-COMMENT-HENSYU-SEC
               MOVE    "2"                  TO  RECE659-MEISKYFLG(2)
               MOVE     104                 TO  RECE659-SETCOMATFLG(2)
             WHEN       2
               MOVE    RECE65-SRYACT-TBL(1) TO  RECE659-SRYACT-TBL(1)
               MOVE    RECE65-SRYACT-TBL(2) TO  RECE659-SRYACT-TBL(4)
               MOVE    RECE65-SRYACT-TBL(3) TO  RECE659-SRYACT-TBL(5)
      *
               MOVE    SPACE                TO  RECE659-SRYACT-TBL(2)
               INITIALIZE                       RECE659-SRYACT-TBL(2)
               MOVE    "810000001"          TO  RECE659-SRYCD(2)
               MOVE    ZERO                 TO  RECE659-SRYSURYO(2)
               PERFORM  200111-SANTEI-DAY-SEC
               PERFORM  200111-TAIIN-COMMENT-HENSYU-SEC
               MOVE    "2"                  TO  RECE659-MEISKYFLG(2)
               MOVE     104                 TO  RECE659-SETCOMATFLG(2)
      *
               MOVE    SPACE                TO  RECE659-SRYACT-TBL(3)
               INITIALIZE                       RECE659-SRYACT-TBL(3)
               MOVE    "810000001"          TO  RECE659-SRYCD(3)
               MOVE    ZERO                 TO  RECE659-SRYSURYO(3)
               MOVE    16                   TO  RECE659-YUKOKETA(3)
               MOVE    "ÌôºÞ´ÉÍý»ØÆ³ÎÁ»»Äê¡¡¡¡¡¡·î¡¡¡¡Æü"
                                            TO  RECE659-NAME(3)
               MOVE     WRK-TUKI-YAKUKAN    TO  DAY-IDX
               MOVE     TBL-DAY(DAY-IDX)    TO  RECE659-NAME(3)(21:4)
               MOVE     WRK-DAY-YAKUKAN     TO  DAY-IDX
               MOVE     TBL-DAY(DAY-IDX)    TO  RECE659-NAME(3)(27:4)
               MOVE    "2"                  TO  RECE659-MEISKYFLG(3)
               MOVE     104                 TO  RECE659-SETCOMATFLG(3)
           END-EVALUATE
           WRITE    RECE659-REC
           ADD      1                   TO  CNT-RECE659
      *
           .
       20011-TAIIN-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    »»ÄêÆüÊÔ½¸
      *****************************************************************
       200111-TAIIN-COMMENT-HENSYU-SEC          SECTION.
      *
           MOVE     TB-SANTEI-DAY(1)    TO  DAY-IDX
           IF       DAY-IDX  <  10
             MOVE    5                  TO  RECE659-YUKOKETA(2)
             MOVE    "»»Äê¡¡¡¡Æü"       TO  RECE659-NAME(2)(1:10)
             MOVE   TBL-DAY(DAY-IDX)    TO  RECE659-NAME(2)(5:4)
             MOVE   11                  TO  IDX17
           ELSE
             MOVE    6                  TO  RECE659-YUKOKETA(2)
             MOVE    "»»Äê¡¡¡¡¡¡Æü"     TO  RECE659-NAME(2)(1:12)
             MOVE   TBL-DAY(DAY-IDX)    TO  RECE659-NAME(2)(7:4)
             MOVE   13                  TO  IDX17
           END-IF
           IF       TB-SANTEI-DAY(2)  NOT = ZERO
             MOVE   TB-SANTEI-DAY(2)    TO  DAY-IDX
             IF       DAY-IDX  <  10
               ADD   3              TO  RECE659-YUKOKETA(2)
               MOVE  "¡¢¡¡Æü"       TO  RECE659-NAME(2)(IDX17:6)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)(3:2)
                                    TO  RECE659-NAME(2)(IDX18:2)
               COMPUTE  IDX17 = IDX17 + 6
             ELSE
               ADD   4              TO  RECE659-YUKOKETA(2)
               MOVE  "¡¢¡¡¡¡Æü"     TO  RECE659-NAME(2)(IDX17:8)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)
                                    TO  RECE659-NAME(2)(IDX18:4)
               COMPUTE  IDX17 = IDX17 + 8
             END-IF
           END-IF
           IF       TB-SANTEI-DAY(3)  NOT = ZERO
             MOVE   TB-SANTEI-DAY(3)    TO  DAY-IDX
             IF       DAY-IDX  <  10
               ADD   3              TO  RECE659-YUKOKETA(2)
               MOVE  "¡¢¡¡Æü"       TO  RECE659-NAME(2)(IDX17:6)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)(3:2)
                                    TO  RECE659-NAME(2)(IDX18:2)
               COMPUTE  IDX17 = IDX17 + 6
             ELSE
               ADD   4              TO  RECE659-YUKOKETA(2)
               MOVE  "¡¢¡¡¡¡Æü"     TO  RECE659-NAME(2)(IDX17:8)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)
                                    TO  RECE659-NAME(2)(IDX18:4)
               COMPUTE  IDX17 = IDX17 + 8
             END-IF
           END-IF
           IF       TB-SANTEI-DAY(4)  NOT = ZERO
             MOVE   TB-SANTEI-DAY(4)    TO  DAY-IDX
             IF       DAY-IDX  <  10
               ADD   3              TO  RECE659-YUKOKETA(2)
               MOVE  "¡¢¡¡Æü"       TO  RECE659-NAME(2)(IDX17:6)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)(3:2)
                                    TO  RECE659-NAME(2)(IDX18:2)
               COMPUTE  IDX17 = IDX17 + 6
             ELSE
               ADD   4              TO  RECE659-YUKOKETA(2)
               MOVE  "¡¢¡¡¡¡Æü"     TO  RECE659-NAME(2)(IDX17:8)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)
                                    TO  RECE659-NAME(2)(IDX18:4)
               COMPUTE  IDX17 = IDX17 + 8
             END-IF
           END-IF
           IF       TB-SANTEI-DAY(5)  NOT = ZERO
             MOVE   TB-SANTEI-DAY(5)    TO  DAY-IDX
             IF       DAY-IDX  <  10
               ADD   3              TO  RECE659-YUKOKETA(2)
               MOVE  "¡¢¡¡Æü"       TO  RECE659-NAME(2)(IDX17:6)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)(3:2)
                                    TO  RECE659-NAME(2)(IDX18:2)
               COMPUTE  IDX17 = IDX17 + 6
             ELSE
               ADD   4              TO  RECE659-YUKOKETA(2)
               MOVE  "¡¢¡¡¡¡Æü"     TO  RECE659-NAME(2)(IDX17:8)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)
                                    TO  RECE659-NAME(2)(IDX18:4)
               COMPUTE  IDX17 = IDX17 + 8
             END-IF
           END-IF
      *
           .
       200111-TAIIN-COMMENT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ê½ÑÆüÅù¤Î¥³¥á¥ó¥ÈÊÔ½¸¡Êº¹¹þ¡§¾å¡Ë
      *****************************************************************
       20011-SHUJUTSU-ETC-COMMENT-SEC          SECTION.
      *
           PERFORM   200111-SANTEI-DAY-SEC
      *
           MOVE       ZERO       TO   IDZ                               
           INITIALIZE            WRK-FREE-AREA2                         
      *
           PERFORM  200111-FREE-COMMENT2-SEC                              
      *
           PERFORM  200111-RECE65-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                ADD    1     TO  IDX-RECE65
                MOVE   "810000001"
                             TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                MOVE   ZERO  TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
           END-PERFORM 
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-SHUJUTSU-ETC-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ê½ÑÆüÅù¤Î¥³¥á¥ó¥ÈÊÔ½¸£²¡Êº¹¹þ¡§²¼¡Ë
      *****************************************************************
       20011-SHUJUTSU-ETC2-COMMENT-SEC          SECTION.
      *
           PERFORM   200111-SANTEI-DAY-SEC
      *
           MOVE       ZERO       TO   IDZ                               
           INITIALIZE            WRK-FREE-AREA2                         
      *
           PERFORM  200111-FREE-COMMENT2-SEC                              
      *
           PERFORM  200111-RECE65-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
      *
           MOVE    RECE64-MEISAISU      TO  MAX-MEISAISU
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
                MOVE   "810000001"
                              TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO   TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
                MOVE   WK-FREE-MEISKYFLG2(IDX)
                          TO  WRK-RECE65XX-MEISKYFLG(MAX-MEISAISU 1)
                MOVE   WK-FREE-SETCOMATFLG2(IDX)
                          TO  WRK-RECE65XX-SETCOMATFLG(MAX-MEISAISU 1)
           END-PERFORM 
      *
           IF    FLG-KAISIEN  =  1
              COMPUTE  MAX-MEISAISU  =   MAX-MEISAISU  +  1
              MOVE   "810000001"
                            TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
              MOVE   ZERO   TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
              MOVE   12     TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
              MOVE   "½é²ó»»ÄêÆü¡¡¡¡¡¡·î¡¡¡¡Æü"
                            TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
              MOVE   WRK-KAISIEN-FSANTEIYMD(5:2)   TO  DAY-IDX2
              MOVE   TBL-DAY(DAY-IDX2)
                       TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(13:4)
              MOVE   WRK-KAISIEN-FSANTEIYMD(7:2)   TO  DAY-IDX2
              MOVE   TBL-DAY(DAY-IDX2)
                       TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(19:4)
              MOVE   "2"    TO  WRK-RECE65XX-MEISKYFLG(MAX-MEISAISU 1)
              MOVE   106    TO  WRK-RECE65XX-SETCOMATFLG(MAX-MEISAISU 1)
      *
              MOVE   ZERO   TO  FLG-KAISIEN
           END-IF
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-SHUJUTSU-ETC2-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    »»ÄêÆü¤Î³ÎÄê
      *****************************************************************
       200111-SANTEI-DAY-SEC          SECTION.
      *
           INITIALIZE                TBL-SANTEI-DAY
      *
           MOVE    ZERO           TO  DAY-IDX
           PERFORM    VARYING    IDX  FROM   1   BY   1
                      UNTIL     (IDX      >   31)  OR
                                (DAY-IDX  >   31)
                IF    RECE64-KAISU(1 IDX)  NOT =  ZERO
                      ADD   1     TO  DAY-IDX
                      MOVE  IDX   TO  TB-SANTEI-DAY(DAY-IDX)
                END-IF
           END-PERFORM
      *
           .
       200111-SANTEI-DAY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ê½ÑÆüÅù¤ÎÆüÉÕ¤ÎÊÔ½¸
      *****************************************************************
       200111-FREE-COMMENT2-SEC          SECTION.                         
      *
           ADD      1          TO    IDZ
      *
           EVALUATE   FLG-COMMENT
              WHEN    1
                 MOVE     4          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     2          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "¼ê½Ñ"  TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
              WHEN    2
                 MOVE     4          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     2          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "¼Â»Ü"  TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
              WHEN    3
                 MOVE     4          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     2          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "»»Äê"  TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
              WHEN    4
                 MOVE     16         TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     8          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "¸òÉÕ¡¡¡¡Ç¯¡¡¡¡·î"
                                 TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
      *
                 INITIALIZE                      STS-AREA-DAY
                 INITIALIZE                      LNK-DAY2-AREA
                 MOVE    "21"                TO  LNK-DAY2-IRAI
                 MOVE    RECE62-SRYYM        TO  WRK-HENYMD(1:6)
                 MOVE    "01"                TO  WRK-HENYMD(7:2)
                 MOVE    WRK-HENYMD          TO  LNK-DAY2-YMD
                 CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                             LNK-DAY2-AREA
                 IF  STS-DAY-RC1             =   ZERO
                   MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                        TO  WK-FREE-NAME2(IDZ)(5:4)
                   END-IF
      *
                   MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                        TO  WK-FREE-NAME2(IDZ)(11:4)
                   END-IF
                 END-IF
              WHEN    6
                 MOVE     0          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     0          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
              WHEN    7
                 MOVE     8          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     4          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "¸¡ºº¼Â»Ü"
                             TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
              WHEN    8
                 MOVE     4          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     2          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "Ëã¿ì"  TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
           END-EVALUATE
      *
           IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
             MOVE  "2"                   TO  WK-FREE-MEISKYFLG2(IDZ)
             MOVE  WRK-JIDO-SETCOMATFLG  TO  WK-FREE-SETCOMATFLG2(IDZ)
           END-IF
      *
           MOVE      ZERO          TO  FLG-FIRST
      *
           PERFORM   VARYING   IDX    FROM   1   BY   1
                          UNTIL   (IDX                 >    31)
                           OR     (TB-SANTEI-DAY(IDX)  =  ZERO)
      *
                 IF  FLG-FIRST  =  ZERO
                    IF  TB-SANTEI-DAY(IDX)     <  10 
      *
                       MOVE   TB-SANTEI-DAY(IDX)  TO  IDY
      *
                       ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                       MOVE    "¡¡"
                           TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                       ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                       MOVE    TBL-DAY(IDY)(3:2)     
                           TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                       ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                       MOVE    "Æü"
                           TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                    ELSE
      *
                       MOVE   TB-SANTEI-DAY(IDX)  TO  IDY
      *
                       IF   FLG-COMMENT   =   4 OR 6
                         ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                         COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                         MOVE    TBL-DAY(IDY)     
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
                       ELSE
                         ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                         COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                         MOVE    "¡¡"
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                         ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                         COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                         MOVE    TBL-DAY(IDY)     
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
                       END-IF
                       ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                       MOVE    "Æü"
                           TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                    END-IF
                    MOVE      1              TO  FLG-FIRST
                 ELSE
                    IF  TB-SANTEI-DAY(IDX)     <  10 
      *
                       MOVE   TB-SANTEI-DAY(IDX)  TO  IDY
      *
                       IF    WK-FREE-YUKOKETA2(IDZ)  =  25
                          PERFORM  2001111-FREE-COMMENT2-KAIGYO-SEC
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    "¡¢"
                                    TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       ELSE
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "¡¢"
                                    TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                       END-IF
      *
                       IF    WK-FREE-YUKOKETA2(IDZ)  =  24 OR 25
                          PERFORM  2001111-FREE-COMMENT2-KAIGYO-SEC
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    TBL-DAY(IDY)(3:2)     
                                    TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "Æü"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2) 
                       ELSE
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    TBL-DAY(IDY)(3:2)     
                                    TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "Æü"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2) 
                       END-IF
                    ELSE
      *
                       MOVE   TB-SANTEI-DAY(IDX)  TO  IDY
      *
                       IF    WK-FREE-YUKOKETA2(IDZ)  =  25
                          PERFORM  2001111-FREE-COMMENT2-KAIGYO-SEC
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    "¡¢"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       ELSE
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "¡¢"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       END-IF
      *
                       IF    WK-FREE-YUKOKETA2(IDZ)  =  23 OR 24 OR 25
                          PERFORM  2001111-FREE-COMMENT2-KAIGYO-SEC
                          ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    TBL-DAY(IDY)     
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                          MOVE    "Æü"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       ELSE
                          ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    TBL-DAY(IDY)     
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                          MOVE    "Æü"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       END-IF
                    END-IF
                 END-IF
      *
           END-PERFORM
           .
       200111-FREE-COMMENT2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ê½ÑÆüÅù¤ÎÆüÉÕ¤ÎÊÔ½¸¡Ê²þ¹Ô¡Ë
      *****************************************************************
       2001111-FREE-COMMENT2-KAIGYO-SEC          SECTION.                  
      *
           ADD      1              TO  IDZ
      *
           MOVE    WRK-KETA-TAIHI  TO  WRK-KETA
           MOVE    WRK-KETA2       TO  WK-FREE-YUKOKETA2(IDZ)
           MOVE    ALL "¡¡"        TO  WK-FREE-NAME2(IDZ)(1:WRK-KETA)
      *
           IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
             MOVE  "2"                   TO  WK-FREE-MEISKYFLG2(IDZ)
             MOVE  WRK-JIDO-SETCOMATFLG  TO  WK-FREE-SETCOMATFLG2(IDZ)
           END-IF
      *
           .
       2001111-FREE-COMMENT2-KAIGYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸øÈñ¼õ¿ÇÆü¤ÎÆüÉÕ¤ÎÊÔ½¸
      *****************************************************************
       20011-FREE-COMMENT3-SEC          SECTION.                         
      *
           ADD      1          TO    IDZ
      *
           EVALUATE   FLG-COMMENT
              WHEN    5
                 MOVE     3          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     2          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "Ê¡»ã"  TO    WK-FREE-NAME2(IDZ)(1:4)
           END-EVALUATE
      *
           MOVE      ZERO          TO  FLG-FIRST
      *
           PERFORM   VARYING   IDX    FROM   1   BY   1
                          UNTIL   (IDX                  >    31 )
                           OR     (TBL-NYU-DAY-ST(IDX)  =   ZERO)
      *
                 IF  TBL-NYU-DAY-CD(IDX)   =  CONS-SANTEI-COMMENT1
      *
                       MOVE   TBL-NYU-DAY-ST(IDX) TO  IDY
      *
                       IF  FLG-FIRST  =  ZERO
                         ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                         COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                         MOVE    "¡¡"
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                         MOVE  1  TO  FLG-FIRST 
                       ELSE
                         IF    WK-FREE-YUKOKETA2(IDZ)  =  25
                           PERFORM  200111-FREE-COMMENT3-KAIGYO-SEC
                         ELSE
                           ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                           COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                           MOVE    "¡¢"
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                         END-IF
                       END-IF
      *
                       IF  TBL-NYU-DAY-ST(IDX)     <  10 
                          IF    WK-FREE-YUKOKETA2(IDZ)  =
                                                       24 OR 25
                            PERFORM  200111-FREE-COMMENT3-KAIGYO-SEC
                          END-IF
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    TBL-DAY(IDY)(3:2)     
                              TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "Æü"
                              TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       ELSE
                          IF    WK-FREE-YUKOKETA2(IDZ)  =
                                                 23 OR 24 OR 25
                            PERFORM  200111-FREE-COMMENT3-KAIGYO-SEC
                          END-IF
                          ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    TBL-DAY(IDY)     
                              TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                          MOVE    "Æü"
                              TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       END-IF
      *
                 ELSE
      *
                       MOVE   TBL-NYU-DAY-ST(IDX) TO  IDY
                       MOVE   TBL-NYU-DAY-ED(IDX) TO  IDY2
      *
                       IF  FLG-FIRST  =  ZERO
                         ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                         COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                         MOVE    "¡¡"
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                         MOVE  1  TO  FLG-FIRST 
                       ELSE
                         IF    WK-FREE-YUKOKETA2(IDZ)  =  25
                           PERFORM  200111-FREE-COMMENT3-KAIGYO-SEC
                         ELSE
                           ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                           COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                           MOVE    "¡¢"
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                         END-IF
                       END-IF
      *
                       IF  TBL-NYU-DAY-ST(IDX)     <  10 
                          IF  TBL-NYU-DAY-ED(IDX)  <  10
                            IF    WK-FREE-YUKOKETA2(IDZ)  =  
                                       21 OR 22 OR 23 OR 24 OR 25
                              PERFORM  200111-FREE-COMMENT3-KAIGYO-SEC
                            END-IF
                            ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                            COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                            MOVE    TBL-DAY(IDY)(3:2)     
                                TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                            ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                            COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                            MOVE    "Æü¡Á"
                                TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                            ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                            COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                            MOVE    TBL-DAY(IDY2)(3:2)     
                                TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                            ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                            COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                            MOVE    "Æü"
                                TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                          ELSE
                            IF    WK-FREE-YUKOKETA2(IDZ)  =  
                                 20 OR 21 OR 22 OR 23 OR 24 OR 25
                              PERFORM  200111-FREE-COMMENT3-KAIGYO-SEC
                            END-IF
                            ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                            COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                            MOVE    TBL-DAY(IDY)(3:2)     
                                TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                            ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                            COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                            MOVE    "Æü¡Á"
                                TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                            ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                            COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                            MOVE    TBL-DAY(IDY2)
                                TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                            ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                            COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                            MOVE    "Æü"
                                TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                          END-IF
                       ELSE
                          IF    WK-FREE-YUKOKETA2(IDZ)  =  
                               19 OR 20 OR 21 OR 22 OR 23 OR 24 OR 25
                              PERFORM  200111-FREE-COMMENT3-KAIGYO-SEC
                          END-IF
                          ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    TBL-DAY(IDY)     
                              TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                          ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                          MOVE    "Æü¡Á"
                              TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                          ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                          MOVE    TBL-DAY(IDY2)
                              TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                          MOVE    "Æü"
                              TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       END-IF
      *
                 END-IF
      *
           END-PERFORM
           .
       20011-FREE-COMMENT3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸øÈñ¼õ¿ÇÆü¤ÎÆüÉÕ¤ÎÊÔ½¸¡Ê²þ¹Ô¡Ë
      *****************************************************************
       200111-FREE-COMMENT3-KAIGYO-SEC          SECTION.                  
      *
           ADD      1              TO  IDZ
      *
           MOVE    WRK-KETA-TAIHI  TO  WRK-KETA
           MOVE    WRK-KETA2       TO  WK-FREE-YUKOKETA2(IDZ)
           MOVE    ALL "¡¡"        TO  WK-FREE-NAME2(IDZ)(1:4)
      *
           .
       200111-FREE-COMMENT3-KAIGYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ê¥Ï¥Ó¥ê¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-RIHA-COMMENT-SEC          SECTION.
      *
             INITIALIZE            WRK-JIDO-SETCOM-AREA
             MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
             MOVE       16      TO     WRK-JIDO-SETCOMATFLG
      *
             PERFORM   200111-RECE65-TAIHI-SEC
      *
      *      £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
             MOVE    ZERO                 TO  IDX-RECE65
             MOVE    SPACE                TO  WRK-RECE65XX-REC
             INITIALIZE                       WRK-RECE65XX-REC
      *      ¥ê¥Ï¥Ó¥ê¥³¥á¥ó¥È¤ÎÁÞÆþ
             COMPUTE  MAX-MEISAISU        =   RECE64-MEISAISU  +  1
      *
             MOVE    "810000001"
                            TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
             MOVE    ZERO   TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
             MOVE    7      TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE    "¼Â»ÜÆü¿ô¡¡¡¡Æü"
                            TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
             MOVE    ZERO                 TO  CNT-RIHA
             PERFORM  VARYING   IDX  FROM  1  BY  1
                       UNTIL    IDX   >   31
                 IF   RECE64-KAISU(1 IDX)  NOT = ZERO
                        ADD      1        TO  CNT-RIHA
                 END-IF
             END-PERFORM
             IF      CNT-RIHA             NOT =    ZERO
               MOVE    CNT-RIHA             TO  DAY-IDX
               MOVE    TBL-DAY(DAY-IDX)
                           TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(9:4)
             END-IF
      *
             IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
               MOVE  "2"   TO  WRK-RECE65XX-MEISKYFLG(MAX-MEISAISU 1)
               MOVE  WRK-JIDO-SETCOMATFLG
                           TO  WRK-RECE65XX-SETCOMATFLG(MAX-MEISAISU 1)
             END-IF
      *
             PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       2001-RIHA-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ê¥Ï¥Ó¥ê°å³Ø´ÉÍýÎÁ¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-RIHAIGAKU-COMMENT-SEC          SECTION.
      *
           INITIALIZE            WRK-JIDO-SETCOM-AREA
      *
           EVALUATE      RECE64-RIHABIRI-COMMENT
              WHEN           "2"
                  MOVE   CONS-SINDAI-IGAKU1   TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-SINDAI-DUMMY    TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "3"
                  MOVE   CONS-SINDAI-IGAKU2   TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-SINDAI-DUMMY    TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "4"
                  MOVE   CONS-KOKYUKI-IGAKU1  TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-KOKYUKI-DUMMY   TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "5"
                  MOVE   CONS-KOKYUKI-IGAKU2  TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-KOKYUKI-DUMMY   TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "6"
                  MOVE   CONS-NOUKE-IGAKU1    TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-NOUKE-DUMMY     TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "7"
                  MOVE   CONS-NOUKE-IGAKU2    TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-NOUKE-DUMMY     TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "8"
                  MOVE   CONS-UNDOKI-IGAKU1   TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-UNDOKI-DUMMY    TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "9"
                  MOVE   CONS-UNDOKI-IGAKU2   TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-UNDOKI-DUMMY    TO  WRK-RIHAIGAKU-SRYCD2
           END-EVALUATE
      ***
           MOVE          SPACE          TO  WRK-RIHAIGAKU-FSANTEIYMD
      *    °å³Ø´ÉÍý¡§»»ÄêÍúÎò¸¡º÷
           INITIALIZE                       SANTEI-REC
           MOVE    RECE64-HOSPNUM       TO  SANTEI-HOSPNUM
           MOVE    RECE64-PTID          TO  SANTEI-PTID
           MOVE    RECE64-SRYYM         TO  SANTEI-SRYYM
           MOVE    WRK-RIHAIGAKU-SRYCD  TO  SANTEI-SRYCD
           MOVE    ZERO                 TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                 TO  SANTEI-SRYKA
           MOVE    ZERO                 TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC           TO  MCPDATA-REC
           MOVE    "tbl_santei"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             PERFORM  900-SANTEI-READ-SEC
             IF       FLG-SANTEI        =   ZERO
                MOVE  SANTEI-FSANTEIYMD TO  WRK-RIHAIGAKU-FSANTEIYMD
             END-IF
           END-IF
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      ***
           INITIALIZE      WRK-RIHAIGAKU-SANTEI-AREA
      *    ¥ê¥Ï¼Â»Ü¥À¥ß¡¼¡§»»ÄêÍúÎò¸¡º÷
           INITIALIZE                       SANTEI-REC
           MOVE    RECE64-HOSPNUM       TO  SANTEI-HOSPNUM
           MOVE    RECE64-PTID          TO  SANTEI-PTID
           MOVE    RECE64-SRYYM         TO  SANTEI-SRYYM
           MOVE    WRK-RIHAIGAKU-SRYCD2 TO  SANTEI-SRYCD
           MOVE    ZERO                 TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                 TO  SANTEI-SRYKA
           MOVE    ZERO                 TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC           TO  MCPDATA-REC
           MOVE    "tbl_santei"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             PERFORM  900-SANTEI-READ-SEC
             IF       FLG-SANTEI        =   ZERO
               PERFORM  VARYING  IDX  FROM  1  BY  1
                         UNTIL   IDX    >  31
                 IF   SANTEI-DAY(IDX)   NOT =  ZERO
                    MOVE  "1"  TO  WRK-RIHAIGAKU-SANTEI-FLG
                    MOVE  SANTEI-DAY(IDX)
                               TO  WRK-RIHAIGAKU-SANTEI-DAY(IDX)
                 END-IF
               END-PERFORM
             END-IF
           END-IF
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      ***
      *
           PERFORM  200111-RECE65-TAIHI-SEC
      *
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
      *
           COMPUTE  MAX-MEISAISU        =   RECE64-MEISAISU  +  1
      *
           MOVE    "810000001"
                         TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
           MOVE    ZERO  TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
           MOVE    9     TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
           MOVE    "³«»Ï¡¡¡¡¡¡·î¡¡¡¡Æü"
                         TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
           IF      WRK-RIHAIGAKU-FSANTEIYMD  NOT = SPACE
             MOVE    WRK-RIHAIGAKU-FSANTEIYMD(5:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                         TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(7:4)
             MOVE    WRK-RIHAIGAKU-FSANTEIYMD(7:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                         TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(13:4)
           END-IF
      *
           IF      WRK-RIHAIGAKU-SANTEI-FLG  NOT = SPACE
             PERFORM    20011-RIHAIGAKU-DUMMY-COMMENT-SEC
           ELSE
             PERFORM    200111-SRYACTINF-HENSYU-SEC
           END-IF
      *
           .
       2001-RIHAIGAKU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ê¥Ï¥Ó¥ê°å³Ø´ÉÍýÎÁ¤Î¥³¥á¥ó¥ÈÊÔ½¸¡Ê¥ê¥Ï¼Â»ÜÆü¡Ë
      *****************************************************************
       20011-RIHAIGAKU-DUMMY-COMMENT-SEC          SECTION.
      *
           INITIALIZE             TBL-SANTEI-DAY
      *
           MOVE     ZERO      TO   DAY-IDX
           PERFORM  VARYING  IDX  FROM   1   BY   1
                     UNTIL   IDX    >   31
             IF  WRK-RIHAIGAKU-SANTEI-DAY(IDX)  NOT = ZERO
                 ADD    1     TO   DAY-IDX
                 MOVE   IDX   TO   TB-SANTEI-DAY(DAY-IDX)
             END-IF
           END-PERFORM
      *
           MOVE     ZERO      TO   IDZ
           INITIALIZE         WRK-FREE-AREA2
      *
           MOVE     2         TO   FLG-COMMENT
           PERFORM  200111-FREE-COMMENT2-SEC
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
                MOVE   "810000001"
                             TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
           END-PERFORM
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-RIHAIGAKU-DUMMY-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¡Ê¥ê¥ÏÁª¡Ë¤Î¥³¥á¥ó¥È¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2001-RIHASEN-HANTEI-SEC          SECTION.
      *
           IF     RECE64-SAFLG   =    "2"
               MOVE       99      TO     FLG-COMMENT
               PERFORM    20011-KENSENTOU-COMMENT-SEC
           END-IF
      *
           .
       2001-RIHASEN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ³°ÇñÆü¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *    Àº¿À²Ê³ÖÎ¥ÉÂ¼¼´ÉÍý²Ã»»¡ÊÆþ±¡ÎÁ²Ã»»¡Ë¤Î¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       2001-NYU-COMMENT-SEC          SECTION.
      *
           INITIALIZE  WRK-JIDO-SETCOM-AREA
      *
           PERFORM     20011-NYU-DAY-SEC
      *
           PERFORM     200111-RECE65-TAIHI-SEC
      *
           MOVE     ZERO    TO    FLG-NG
           IF   (RECE64-ZAISKBKBN  =  "2"                          )
           AND  (FLG-RECE65        =  ZERO                         )
           AND  (WRK-RECE65-SRYCD(1)  =  "190108890" OR "190142190"
                                      OR "190108790" OR "190142090"
                                      OR "190108990" OR "190109090"
                                      OR "190142290" OR "190141590"
                                      OR "999999982" OR "999999992"
                                      OR "999999983" OR "999999993"
                                      OR "190116890" OR "999999984"
                                      OR "999999994"
                                      OR "190153190" OR "190153390"
                                      OR "190153290" OR "190153090")
              MOVE    1     TO    FLG-NG
           END-IF
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
      *
           MOVE    RECE64-MEISAISU      TO  MAX-MEISAISU
      *
           IF    FLG-NG  =  ZERO
      *      »»ÄêÆü¥³¥á¥ó¥È¤ÎÁÞÆþ
             PERFORM  VARYING      IDX    FROM  1   BY   1
                      UNTIL       (IDX    >     31)      OR
                                  (TBL-NYU-DAY-ST(IDX)  =  ZERO)
               COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
      ***      MOVE    TBL-NYU-DAY-CD(IDX)
               MOVE    "810000001"
                             TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
               MOVE    ZERO  TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
               IF    TBL-NYU-DAY-CD(IDX)  =  CONS-SANTEI-COMMENT1
                 MOVE    3   TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
                 MOVE    "¡¡¡¡Æü"
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
                 MOVE    TBL-NYU-DAY-ST(IDX)
                             TO  DAY-IDX
                 MOVE    TBL-DAY(DAY-IDX)
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(1:4)
               ELSE
                 MOVE    7   TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
                 MOVE    "¡¡¡¡Æü¡Á¡¡¡¡Æü"
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
                 MOVE    TBL-NYU-DAY-ST(IDX)
                             TO  DAY-IDX
                 MOVE    TBL-DAY(DAY-IDX)
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(1:4)
                 MOVE    TBL-NYU-DAY-ED(IDX)
                             TO  DAY-IDX
                 MOVE    TBL-DAY(DAY-IDX)
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(9:4)
               END-IF
               IF   RECE64-ZAITEN   NOT =   ZERO
                 MOVE   "1"  TO  WRK-RECE65XX-RECEDENFLG(MAX-MEISAISU 1)
               END-IF
             END-PERFORM
           END-IF
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           IF     RECE64-ZAISKBKBN             =    "6"
           AND    RECE64-MEISAISU              =     1
             IF  (RECE64-FLG-KAKURI-COMMENT    =    "1"       )
             OR  (RECE64-FLG-EISAPO-COMMENT    =    "1" OR "2")
             OR  (RECE64-FLG-KOKKEA-COMMENT    =    "1"       )
             OR  (RECE64-FLG-JYUSYO-COMMENT    =    "1" OR "2")
               PERFORM    20012-NYUKASAN-HENSYU-SEC
             END-IF
           END-IF
      *
           .
       2001-NYU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ³°ÇñÆü¤Î³ÎÄê
      *    Àº¿À²Ê³ÖÎ¥ÉÂ¼¼´ÉÍý²Ã»»Æü¡ÊÆþ±¡ÎÁ²Ã»»¡Ë¤Î³ÎÄê
      *****************************************************************
       20011-NYU-DAY-SEC          SECTION.
      *
           INITIALIZE                TBL-NYU-DAY
      *
           MOVE    ZERO          TO  DAY-IDX
           MOVE    ZERO          TO  FLG-NYU-RENZOKU
      *
           PERFORM    VARYING    IDX  FROM   1   BY   1
                      UNTIL     (IDX      >   31)  OR
                                (DAY-IDX  >   31)
                IF    RECE64-KAISU(1 IDX)   =  ZERO
                      MOVE  ZERO  TO  FLG-NYU-RENZOKU
                ELSE
                  IF  (RECE64-ZAISKBKBN         =  "2")
                  AND (FLG-RYOCOM               =   1 )
                  AND (TBL-RYOCOM-NYUINKBN(IDX) =  "1")
                  AND (RECE62-SRYYM       >=  "201004")
                      MOVE  ZERO  TO  FLG-NYU-RENZOKU
                  ELSE
                      IF    FLG-NYU-RENZOKU  =  ZERO
                         ADD   1     TO  DAY-IDX
                         MOVE  IDX   TO  TBL-NYU-DAY-ST(DAY-IDX)
                      END-IF
                      MOVE  IDX      TO  TBL-NYU-DAY-ED(DAY-IDX)
                      MOVE  1        TO  FLG-NYU-RENZOKU
                  END-IF
                END-IF
           END-PERFORM
      *
           PERFORM    VARYING    DAY-IDX  FROM   1   BY   1
                      UNTIL  (DAY-IDX                      >   31)  OR
                             (TBL-NYU-DAY-ST(DAY-IDX)  = ZERO)
                IF    TBL-NYU-DAY-ST(DAY-IDX) 
                                    =   TBL-NYU-DAY-ED(DAY-IDX)
                      MOVE  CONS-SANTEI-COMMENT1
                                    TO  TBL-NYU-DAY-CD(DAY-IDX)
                ELSE
                      MOVE  CONS-SANTEI-COMMENT2
                                    TO  TBL-NYU-DAY-CD(DAY-IDX)
                END-IF
           END-PERFORM
      *
           .
       20011-NYU-DAY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¡Ê¥ì¥»ÅÅÍÑ¡Ë
      *****************************************************************
       20012-NYUKASAN-HENSYU-SEC          SECTION.
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
      *
           MOVE    1                    TO  MAX-MEISAISU
      *
           MOVE  "810000001"  TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
           MOVE   ZERO        TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
           IF  (RECE64-FLG-KAKURI-COMMENT    =    "1")
             MOVE   13        TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE  "Àº¿À²Ê³ÖÎ¥¼¼´ÉÍý²Ã»»»»ÄêÆü"
                              TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
           END-IF
           IF  (RECE64-FLG-EISAPO-COMMENT    =    "1")
             MOVE   14        TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE  "±ÉÍÜ¥µ¥Ý¡¼¥È¥Á¡¼¥à²Ã»»»»ÄêÆü"
                              TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
           END-IF
           IF  (RECE64-FLG-EISAPO-COMMENT    =    "2")
             MOVE   20        TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE  "±ÉÍÜ¥µ¥Ý¡¼¥È¥Á¡¼¥à²Ã»»¡Ê»ØÄêÃÏ°è¡Ë»»ÄêÆü"
                              TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
           END-IF
           IF  (RECE64-FLG-KOKKEA-COMMENT    =    "1")
             MOVE   12        TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE  "¸ÆµÛ¥±¥¢¥Á¡¼¥à²Ã»»»»ÄêÆü"
                              TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
           END-IF
           IF  (RECE64-FLG-JYUSYO-COMMENT    =    "2")
             MOVE   9         TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE  "½Å¾É¼Ô²Ã»»£±»»ÄêÆü"
                              TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
           END-IF
           IF  (RECE64-FLG-JYUSYO-COMMENT    =    "1")
            IF  RECE62-SRYYM   >=  "201204"
             MOVE   9         TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE  "½Å¾É¼Ô²Ã»»£²»»ÄêÆü"
                              TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
            ELSE
             MOVE   19        TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE  "½Å¾É¼Ô²Ã»»¡ÊÀº¿ÀÎÅÍÜÉÂÅïÆþ±¡ÎÁ¡Ë»»ÄêÆü"
                              TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
            END-IF
           END-IF
           MOVE  "1"          TO  WRK-RECE65XX-RECEPTFLG(MAX-MEISAISU 1)
      *    »»ÄêÆü¥³¥á¥ó¥È¤ÎÁÞÆþ
           PERFORM  VARYING      IDX    FROM  1   BY   1
                    UNTIL       (IDX    >     31)      OR
                                (TBL-NYU-DAY-ST(IDX)  =  ZERO)
               COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
               MOVE    "810000001"
                             TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
               MOVE    ZERO  TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
               IF    TBL-NYU-DAY-CD(IDX)  =  CONS-SANTEI-COMMENT1
                 MOVE    3   TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
                 MOVE    "¡¡¡¡Æü"
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
                 MOVE    TBL-NYU-DAY-ST(IDX)
                             TO  DAY-IDX
                 MOVE    TBL-DAY(DAY-IDX)
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(1:4)
               ELSE
                 MOVE    7   TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
                 MOVE    "¡¡¡¡Æü¡Á¡¡¡¡Æü"
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
                 MOVE    TBL-NYU-DAY-ST(IDX)
                             TO  DAY-IDX
                 MOVE    TBL-DAY(DAY-IDX)
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(1:4)
                 MOVE    TBL-NYU-DAY-ED(IDX)
                             TO  DAY-IDX
                 MOVE    TBL-DAY(DAY-IDX)
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(9:4)
               END-IF
               MOVE   "1"    TO  WRK-RECE65XX-RECEPTFLG(MAX-MEISAISU 1)
           END-PERFORM
      *
      *    ¥³¥á¥ó¥È¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
               INITIALIZE                        RECE659-REC
               MOVE    RECE64-KEY           TO   RECE659-KEY
               COMPUTE IDX19              =  RECE64-KOHBUNRI + 1
               EVALUATE  RECE64-SRYKBN
                 WHEN      "90"
                   IF  (RECE64-FLG-KAKURI-COMMENT    =    "1")
                     COMPUTE RECE659-ZAINUM =  99994000  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-EISAPO-COMMENT    =    "1")
                     COMPUTE RECE659-ZAINUM =  99994005  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-EISAPO-COMMENT    =    "2")
                     COMPUTE RECE659-ZAINUM =  99994010  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-KOKKEA-COMMENT    =    "1")
                     COMPUTE RECE659-ZAINUM =  99994015  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-JYUSYO-COMMENT    =    "2")
                     COMPUTE RECE659-ZAINUM =  99994020  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-JYUSYO-COMMENT    =    "1")
                     COMPUTE RECE659-ZAINUM =  99994025  +  IDX19
                   END-IF
                   MOVE    "900"            TO   RECE659-SRYSYUKBN
                 WHEN      "92"
                   IF  (RECE64-FLG-KAKURI-COMMENT    =    "1")
                     COMPUTE RECE659-ZAINUM =  99994050  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-EISAPO-COMMENT    =    "1")
                     COMPUTE RECE659-ZAINUM =  99994055  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-EISAPO-COMMENT    =    "2")
                     COMPUTE RECE659-ZAINUM =  99994060  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-KOKKEA-COMMENT    =    "1")
                     COMPUTE RECE659-ZAINUM =  99994065  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-JYUSYO-COMMENT    =    "2")
                     COMPUTE RECE659-ZAINUM =  99994070  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-JYUSYO-COMMENT    =    "1")
                     COMPUTE RECE659-ZAINUM =  99994075  +  IDX19
                   END-IF
                   MOVE    "920"            TO   RECE659-SRYSYUKBN
               END-EVALUATE
               MOVE    ZERO                 TO   RECE659-ZAIREN
               ADD     1                    TO   WRK-RENNUM
               MOVE    WRK-RENNUM           TO   RECE659-RENNUM
               MOVE    RECE64-KOHBUNRI      TO   RECE659-KOHBUNRI
               MOVE    RECE64-PTNUM         TO   RECE659-PTNUM
               MOVE    RECE64-SRYKA         TO   RECE659-SRYKA
               MOVE    RECE64-SRYKBN        TO   RECE659-SRYKBN
               MOVE    ZERO                 TO   RECE659-JIHIMONEY
               PERFORM  VARYING   IDX   FROM   1   BY   1
                         UNTIL   (IDX       >    5)   OR
                                 (FLG-WRKRECE65   =   1)
                   ADD        1             TO    IDX-RECE65
                   IF  WRK-RECE65XX-SRYCD(IDX-RECE65 1) = SPACE
                       MOVE   1             TO    FLG-WRKRECE65
                   ELSE
                       MOVE  WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
                                        TO   RECE659-SRYACT-TBL(IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
               END-PERFORM
               IF   RECE659-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE659-REC
                   ADD     1            TO   CNT-RECE659
               END-IF
           END-PERFORM
      *
           INITIALIZE                      RECE649-REC
           MOVE    RECE62-HOSPNUM      TO  RECE649-HOSPNUM
           MOVE    RECE62-SRYYM        TO  RECE649-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE649-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE649-PTID
           MOVE    RECE62-RECEKA       TO  RECE649-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE649-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE649-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE649-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE649-HOJOKBN
           MOVE    RECE62-TEKSTYMD     TO  RECE649-TEKSTYMD
           COMPUTE IDX19                =  RECE64-KOHBUNRI + 1
           EVALUATE  RECE64-SRYKBN
             WHEN      "90"
               IF  (RECE64-FLG-KAKURI-COMMENT    =    "1")
                 COMPUTE RECE649-ZAINUM =  99994000  +  IDX19
               END-IF
               IF  (RECE64-FLG-EISAPO-COMMENT    =    "1")
                 COMPUTE RECE649-ZAINUM =  99994005  +  IDX19
               END-IF
               IF  (RECE64-FLG-EISAPO-COMMENT    =    "2")
                 COMPUTE RECE649-ZAINUM =  99994010  +  IDX19
               END-IF
               IF  (RECE64-FLG-KOKKEA-COMMENT    =    "1")
                 COMPUTE RECE649-ZAINUM =  99994015  +  IDX19
               END-IF
               IF  (RECE64-FLG-JYUSYO-COMMENT    =    "2")
                 COMPUTE RECE649-ZAINUM =  99994020  +  IDX19
               END-IF
               IF  (RECE64-FLG-JYUSYO-COMMENT    =    "1")
                 COMPUTE RECE649-ZAINUM =  99994025  +  IDX19
               END-IF
             WHEN      "92"
               IF  (RECE64-FLG-KAKURI-COMMENT    =    "1")
                 COMPUTE RECE649-ZAINUM =  99994050  +  IDX19
               END-IF
               IF  (RECE64-FLG-EISAPO-COMMENT    =    "1")
                 COMPUTE RECE649-ZAINUM =  99994055  +  IDX19
               END-IF
               IF  (RECE64-FLG-EISAPO-COMMENT    =    "2")
                 COMPUTE RECE649-ZAINUM =  99994060  +  IDX19
               END-IF
               IF  (RECE64-FLG-KOKKEA-COMMENT    =    "1")
                 COMPUTE RECE649-ZAINUM =  99994065  +  IDX19
               END-IF
               IF  (RECE64-FLG-JYUSYO-COMMENT    =    "2")
                 COMPUTE RECE649-ZAINUM =  99994070  +  IDX19
               END-IF
               IF  (RECE64-FLG-JYUSYO-COMMENT    =    "1")
                 COMPUTE RECE649-ZAINUM =  99994075  +  IDX19
               END-IF
           END-EVALUATE
           MOVE    ZERO                TO  RECE649-ZAIREN
           MOVE    RECE64-KOHBUNRI     TO  RECE649-KOHBUNRI
           MOVE    RECE64-SRYKBN       TO  RECE649-SRYKBN
           MOVE    "6"                 TO  RECE649-ZAISKBKBN
           MOVE    RECE64-PTNUM        TO  RECE649-PTNUM
           MOVE    RECE64-SRYKA        TO  RECE649-SRYKA
           MOVE    RECE64-HKNCOMBI     TO  RECE649-HKNCOMBI
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE649-MEISAISU
      *
           WRITE   RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           .
       20012-NYUKASAN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Àº¿À²Ê³ÖÎ¥ÉÂ¼¼´ÉÍý²Ã»»¡ÊÆþ±¡ÎÁ²Ã»»¡Ë¤Î¥³¥á¥ó¥È¤¬¤¢¤ë¤«È½Äê
      *    ¡Ê¿ÇÎÅ¹Ô°ÙÆþÎÏ¡Ë
      *****************************************************************
       2001-KAKURI-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-KAKURI-COMMENT    =    "1"
               MOVE       99      TO     FLG-COMMENT
               PERFORM   2001-NYU-COMMENT-SEC
           END-IF         
      *
           .
       2001-KAKURI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ±ÉÍÜ¥µ¥Ý¡¼¥È¥Á¡¼¥à²Ã»»¡ÊÆþ±¡ÎÁ²Ã»»¡Ë¤Î¥³¥á¥ó¥È¤¬¤¢¤ë¤«È½Äê
      *    ¡Ê¿ÇÎÅ¹Ô°ÙÆþÎÏ¡Ë
      *****************************************************************
       2001-EISAPO-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-EISAPO-COMMENT    =    "1" OR "2"
               MOVE       99      TO     FLG-COMMENT
               PERFORM   2001-NYU-COMMENT-SEC
           END-IF
      *
           .
       2001-EISAPO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸ÆµÛ¥±¥¢¥Á¡¼¥à²Ã»»¡ÊÆþ±¡ÎÁ²Ã»»¡Ë¤Î¥³¥á¥ó¥È¤¬¤¢¤ë¤«È½Äê
      *    ¡Ê¿ÇÎÅ¹Ô°ÙÆþÎÏ¡Ë
      *****************************************************************
       2001-KOKKEA-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-KOKKEA-COMMENT    =    "1"
               MOVE       99      TO     FLG-COMMENT
               PERFORM   2001-NYU-COMMENT-SEC
           END-IF
      *
           .
       2001-KOKKEA-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½Å¾É¼Ô²Ã»»¡ÊÀº¿ÀÎÅÍÜÉÂÅïÆþ±¡ÎÁ¡Ë¡ÊÆþ±¡ÎÁ²Ã»»¡Ë¤Î¥³¥á¥ó¥È¤¬¤¢¤ë¤«È½Äê
      *    ¡Ê¿ÇÎÅ¹Ô°ÙÆþÎÏ¡Ë
      *****************************************************************
       2001-JYUSYO-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-JYUSYO-COMMENT    =    "1" OR "2"
               MOVE       99      TO     FLG-COMMENT
               PERFORM   2001-NYU-COMMENT-SEC
           END-IF
      *
           .
       2001-JYUSYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£´Å¾Á÷½èÍý
      *****************************************************************
       2099-RECE64-TENSOU-SEC     SECTION.
      *
           MOVE       RECE64-REC    TO   RECE649-REC
           WRITE      RECE649-REC
           ADD        1             TO   CNT-RECE649
           PERFORM    20991-RECE65-TENSOU-SEC
      *
           .
       2099-RECE64-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£µÅ¾Á÷½èÍý
      *****************************************************************
       20991-RECE65-TENSOU-SEC     SECTION.
      *
           INITIALIZE                      RECE65-REC
           MOVE    RECE64-HOSPNUM      TO  RECE65-HOSPNUM
           MOVE    RECE64-SRYYM        TO  RECE65-SRYYM
           MOVE    RECE64-NYUGAIKBN    TO  RECE65-NYUGAIKBN
           MOVE    RECE64-PTID         TO  RECE65-PTID
           MOVE    RECE64-RECEKA       TO  RECE65-RECEKA
           MOVE    RECE64-TEISYUTUSAKI TO  RECE65-TEISYUTUSAKI
           MOVE    RECE64-RECESYUBETU  TO  RECE65-RECESYUBETU
           MOVE    RECE64-HKNJANUM     TO  RECE65-HKNJANUM
           MOVE    RECE64-HOJOKBN      TO  RECE65-HOJOKBN
           MOVE    RECE64-TEKSTYMD     TO  RECE65-TEKSTYMD
           MOVE    RECE64-ZAINUM       TO  RECE65-ZAINUM
           MOVE    RECE64-ZAIREN       TO  RECE65-ZAIREN
           START   RECE65-FILE
               KEY IS    >=   RECE65-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE65
               NOT INVALID
                   PERFORM 990-RECE65-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE65           =   1                   )
               OR     ( RECE64-HOSPNUM   NOT =   RECE65-HOSPNUM      )
               OR     ( RECE64-SRYYM     NOT =   RECE65-SRYYM        )
               OR     ( RECE64-NYUGAIKBN NOT =   RECE65-NYUGAIKBN    )
               OR     ( RECE64-PTID      NOT =   RECE65-PTID         )
               OR     ( RECE64-RECEKA    NOT =   RECE65-RECEKA       )
               OR     ( RECE64-TEISYUTUSAKI
                                         NOT =   RECE65-TEISYUTUSAKI )
               OR     ( RECE64-RECESYUBETU
                                         NOT =   RECE65-RECESYUBETU  )
               OR     ( RECE64-HKNJANUM  NOT =   RECE65-HKNJANUM     )
               OR     ( RECE64-HOJOKBN   NOT =   RECE65-HOJOKBN      )
               OR     ( RECE64-TEKSTYMD  NOT =   RECE65-TEKSTYMD     )
               OR     ( RECE64-ZAINUM    NOT =   RECE65-ZAINUM       )
               OR     ( RECE64-ZAIREN    NOT =   RECE65-ZAIREN       )
      *
      *
               MOVE          RECE65-REC       TO      RECE659-REC
               WRITE         RECE659-REC
               ADD     1                 TO  CNT-RECE659
      *
               PERFORM 990-RECE65-NEXT-SEC
      *
           END-PERFORM
      *
           .
       20991-RECE65-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    £°£µ¡¡ÂàÈò½èÍý
      *****************************************************************
       200111-RECE65-TAIHI-SEC     SECTION.
      *
           INITIALIZE                      RECE65-REC
           MOVE    RECE64-HOSPNUM      TO  RECE65-HOSPNUM
           MOVE    RECE64-SRYYM        TO  RECE65-SRYYM
           MOVE    RECE64-NYUGAIKBN    TO  RECE65-NYUGAIKBN
           MOVE    RECE64-PTID         TO  RECE65-PTID
           MOVE    RECE64-RECEKA       TO  RECE65-RECEKA
           MOVE    RECE64-TEISYUTUSAKI TO  RECE65-TEISYUTUSAKI
           MOVE    RECE64-RECESYUBETU  TO  RECE65-RECESYUBETU
           MOVE    RECE64-HKNJANUM     TO  RECE65-HKNJANUM
           MOVE    RECE64-HOJOKBN      TO  RECE65-HOJOKBN
           MOVE    RECE64-TEKSTYMD     TO  RECE65-TEKSTYMD
           MOVE    RECE64-ZAINUM       TO  RECE65-ZAINUM
           MOVE    RECE64-ZAIREN       TO  RECE65-ZAIREN
           START   RECE65-FILE
                 KEY IS    >=   RECE65-KEY
                 INVALID
                     MOVE    1           TO  FLG-RECE65
                 NOT INVALID
                     PERFORM 990-RECE65-NEXT-SEC
                     MOVE    RECE65-REC  TO  WRK-RECE65-REC
           END-START
      *
           .
       200111-RECE65-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    £°£µ¡¡£Ó£Ò£Ù£Á£Ã£Ô£É£Î£ÆÊÔ½¸½èÍý
      *****************************************************************
       200111-SRYACTINF-HENSYU-SEC     SECTION.
      *
           PERFORM
               UNTIL  ( FLG-RECE65           =   1                   )
               OR     ( RECE64-HOSPNUM   NOT =   RECE65-HOSPNUM      )
               OR     ( RECE64-SRYYM     NOT =   RECE65-SRYYM        )
               OR     ( RECE64-NYUGAIKBN NOT =   RECE65-NYUGAIKBN    )
               OR     ( RECE64-PTID      NOT =   RECE65-PTID         )
               OR     ( RECE64-RECEKA    NOT =   RECE65-RECEKA       )
               OR     ( RECE64-TEISYUTUSAKI
                                         NOT =   RECE65-TEISYUTUSAKI )
               OR     ( RECE64-RECESYUBETU
                                         NOT =   RECE65-RECESYUBETU  )
               OR     ( RECE64-HKNJANUM  NOT =   RECE65-HKNJANUM     )
               OR     ( RECE64-HOJOKBN   NOT =   RECE65-HOJOKBN      )
               OR     ( RECE64-TEKSTYMD  NOT =   RECE65-TEKSTYMD     )
               OR     ( RECE64-ZAINUM    NOT =   RECE65-ZAINUM       )
               OR     ( RECE64-ZAIREN    NOT =   RECE65-ZAIREN       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX               >   5     )
                         OR      ( RECE65-SRYCD(IDX) =   SPACE )
                    ADD    1             TO      IDX-RECE65
                    MOVE   RECE65-SRYACT-TBL(IDX)
                             TO   WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
                    IF   RECE64-ZAITEN   =   ZERO
                       PERFORM   2001111-SRYACTINF-NAME-HENSYU-SEC
                    END-IF
               END-PERFORM
      *
               PERFORM 990-RECE65-NEXT-SEC
      *
           END-PERFORM
      *
      *    ¥³¥á¥ó¥È¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
                    INITIALIZE                        RECE659-REC
                    MOVE    WRK-RECE65-KEY       TO   RECE659-KEY
                    ADD     1                    TO   WRK-RENNUM
                    MOVE    WRK-RENNUM           TO   RECE659-RENNUM
                    MOVE    WRK-RECE65-KOHBUNRI  TO   RECE659-KOHBUNRI
                    MOVE    WRK-RECE65-PTNUM     TO   RECE659-PTNUM
                    MOVE    WRK-RECE65-SRYKA     TO   RECE659-SRYKA
                    MOVE    WRK-RECE65-SRYKBN    TO   RECE659-SRYKBN
                    MOVE    WRK-RECE65-SRYSYUKBN TO   RECE659-SRYSYUKBN
                    MOVE    WRK-RECE65-JIHIMONEY TO   RECE659-JIHIMONEY
                    PERFORM  VARYING   IDX   FROM   1   BY   1
                             UNTIL   (IDX       >    5)   OR
                                     (FLG-WRKRECE65   =   1)
                       ADD         1             TO    IDX-RECE65
                       IF  WRK-RECE65XX-SRYCD(IDX-RECE65 1) = SPACE
                           MOVE    1             TO    FLG-WRKRECE65
                       ELSE
                           MOVE  WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
                                            TO   RECE659-SRYACT-TBL(IDX)
                           ADD   1          TO   WRK-CNT-MEISAI
                       END-IF
                    END-PERFORM
                    IF   RECE659-SRYCD(1)  NOT =  SPACE
                       WRITE   RECE659-REC
                       ADD     1            TO   CNT-RECE659
                    END-IF
           END-PERFORM
      *
           MOVE    RECE64-REC        TO      RECE649-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE649-MEISAISU
           IF   WRK-JIDO-SETCOMFLG   =  "1"
             MOVE  WRK-JIDO-SETCOMFLG
                                     TO      RECE649-SETCOMFLG
           END-IF
           WRITE   RECE649-REC
           ADD     1                 TO      CNT-RECE649
      *
           .
       200111-SRYACTINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    £°£µ¡¡£Ó£Ò£Ù£Á£Ã£Ô£É£Î£Æ¡¡Ì¾¾ÎÊÔ½¸½èÍý
      *****************************************************************
       2001111-SRYACTINF-NAME-HENSYU-SEC     SECTION.
      *
           IF   RECE62-SRYYM      >=    "201004"
             EVALUATE   WRK-RECE65XX-SRYCD(IDX-RECE65 1)
      *
               WHEN   "190111590"
                 MOVE  "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                 MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                 MOVE   8       TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "³°Çñ¡ÊÁªÄêÎÅÍÜ¡Ë"
                                TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
      *
               WHEN   "999999982"
                 MOVE  "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                 MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                 MOVE   28      TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "ÆÃÄêÆþ±¡ÎÁ¸º»»"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                 MOVE  "¡ÊÂ¾°å¼õ¿Ç¡¦Êñ³ç¿ÇÎÅ¹Ô°Ù»»Äê¡Ë¡ÊÁªÄêÎÅÍÜ¡Ë"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:42)
      *
               WHEN   "999999983"
                 MOVE  "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                 MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                 MOVE   33      TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁÅù¸º»»"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:24)
                 MOVE  "¡ÊÂ¾°å¼õ¿Ç¡¦Êñ³ç¿ÇÎÅ¹Ô°Ù»»Äê¡Ë¡ÊÁªÄêÎÅÍÜ¡Ë"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(25:42)
      *
               WHEN   "999999984"
                 MOVE  "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                 MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                 MOVE   37      TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ¸º»»"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:32)
                 MOVE  "¡ÊÂ¾°å¼õ¿Ç¡¦Êñ³ç¿ÇÎÅ¹Ô°Ù»»Äê¡Ë¡ÊÁªÄêÎÅÍÜ¡Ë"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(33:42)
      *
               WHEN   "999999992"
                 MOVE  "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                 MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                 MOVE   29      TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "ÆÃÄêÆþ±¡ÎÁ¸º»»"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                 MOVE  "¡ÊÂ¾°å¼õ¿Ç¡¦Êñ³ç¿ÇÎÅ¹Ô°ÙÌ¤»»Äê¡Ë¡ÊÁªÄêÎÅÍÜ¡Ë"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:44)
      *
               WHEN   "999999993"
                 MOVE  "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                 MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                 MOVE   34      TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁÅù¸º»»"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:24)
                 MOVE  "¡ÊÂ¾°å¼õ¿Ç¡¦Êñ³ç¿ÇÎÅ¹Ô°ÙÌ¤»»Äê¡Ë¡ÊÁªÄêÎÅÍÜ¡Ë"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(25:44)
      *
               WHEN   "999999994"
                 MOVE  "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                 MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                 MOVE   38      TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ¸º»»"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:32)
                 MOVE  "¡ÊÂ¾°å¼õ¿Ç¡¦Êñ³ç¿ÇÎÅ¹Ô°ÙÌ¤»»Äê¡Ë¡ÊÁªÄêÎÅÍÜ¡Ë"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(33:44)
             END-EVALUATE
           ELSE
             EVALUATE   WRK-RECE65XX-SRYCD(IDX-RECE65 1)
      *
               WHEN   "190111590"
                 MOVE  "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                 MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                 MOVE   8       TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "³°Çñ¡ÊÁªÄêÎÅÍÜ¡Ë"
                                TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
      *
               WHEN   "999999982"
                 MOVE  "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                 MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                 MOVE   22      TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "ÆÃÄêÆþ±¡ÎÁ¸º»»"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                 MOVE  "¡ÊÂ¾°åÎÅµ¡´Ø¼õ¿Ç¡Ë¡ÊÁªÄêÎÅÍÜ¡Ë"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:30)
      *
               WHEN   "999999983"
                 MOVE  "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                 MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                 MOVE   26      TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ¸º»»"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:22)
                 MOVE  "¡ÊÂ¾°åÎÅµ¡´Ø¼õ¿Ç¡Ë¡ÊÁªÄêÎÅÍÜ¡Ë"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(23:30)
      *
               WHEN   "999999984"
                 MOVE  "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                 MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                 MOVE   31      TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ¸º»»"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:32)
                 MOVE  "¡ÊÂ¾°åÎÅµ¡´Ø¼õ¿Ç¡Ë¡ÊÁªÄêÎÅÍÜ¡Ë"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(33:30)
             END-EVALUATE
           END-IF
      *
           .
       2001111-SRYACTINF-NAME-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    º¹¹þÊÔ½¸£²
      *****************************************************************
       250-SASHIKOMI2-SEC     SECTION.
      *
      *    ¥Ñ¥é¥á¡¼¥¿£¶£´£¹¥Õ¥¡¥¤¥ë¤è¤êÊÔ½¸
           INITIALIZE                      RECE649-REC
           MOVE    RECE62-HOSPNUM      TO  RECE649-HOSPNUM
           MOVE    RECE62-SRYYM        TO  RECE649-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE649-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE649-PTID
           MOVE    RECE62-RECEKA       TO  RECE649-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE649-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE649-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE649-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE649-HOJOKBN
           MOVE    RECE62-TEKSTYMD     TO  RECE649-TEKSTYMD
           MOVE    ZERO                TO  RECE649-ZAINUM
      *
           START   RECE649-FILE
               KEY IS    >=   RECE649-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE649
               NOT INVALID
                   PERFORM 990-RECE649-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE649          =   1                    )
               OR     ( RECE62-HOSPNUM   NOT =   RECE649-HOSPNUM      )
               OR     ( RECE62-SRYYM     NOT =   RECE649-SRYYM        )
               OR     ( RECE62-NYUGAIKBN NOT =   RECE649-NYUGAIKBN    )
               OR     ( RECE62-PTID      NOT =   RECE649-PTID         )
               OR     ( RECE62-RECEKA    NOT =   RECE649-RECEKA       )
               OR     ( RECE62-TEISYUTUSAKI
                                         NOT =   RECE649-TEISYUTUSAKI )
               OR     ( RECE62-RECESYUBETU
                                         NOT =   RECE649-RECESYUBETU  )
               OR     ( RECE62-HKNJANUM-KEY
                                         NOT =   RECE649-HKNJANUM     )
               OR     ( RECE62-HOJOKBN-KEY
                                         NOT =   RECE649-HOJOKBN      )
               OR     ( RECE62-TEKSTYMD  NOT =   RECE649-TEKSTYMD     )
      *
               EVALUATE  RECE649-SRYKBN
                 WHEN      "90"
                 WHEN      "92"
                    IF    FLG-RYOCOM  =  ZERO
                       PERFORM       2599-RECE649-TENSOU-SEC
                    ELSE
                       EVALUATE  RECE649-ZAISKBKBN
                         WHEN     "1"
                         WHEN     "2"
                           PERFORM   2502-RYOCOM-HANTEI-SEC
                         WHEN   OTHER
                           PERFORM   2599-RECE649-TENSOU-SEC
                       END-EVALUATE
                    END-IF
                 WHEN      OTHER
                    MOVE   ZERO   TO  FLG-COMMENT
      *             ¡Ê»ÀÁÇ·×»»¼°¤Î¥³¥á¥ó¥È¤Î¥»¥Ã¥È¡Ë
                    IF   FLG-COMMENT  =  ZERO
                        PERFORM   2501-SANSO-HANTEI-SEC
                    END-IF
      *             ¡Ê¥Õ¥¤¥ë¥à¤Î¥³¥á¥ó¥È¤Î¥»¥Ã¥È¡Ë
                    IF   FLG-COMMENT  =  ZERO
                        PERFORM   2503-FILM-HANTEI-SEC
                    END-IF
                    IF   FLG-COMMENT  =  ZERO
                        PERFORM   2599-RECE649-TENSOU-SEC
                    END-IF
               END-EVALUATE
      *
               PERFORM 990-RECE649-NEXT-SEC
      *
           END-PERFORM
      *
           .
       250-SASHIKOMI2-EXT.
           EXIT.
      *
      *****************************************************************
      *    »ÀÁÇ¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2501-SANSO-HANTEI-SEC          SECTION.
      *
           IF     RECE649-FLG-SANSO-COMMENT   =    "1"
               MOVE       99      TO     FLG-COMMENT
      *
               PERFORM    25011-SANSO-TAIHI-SEC
      *
               PERFORM    25011-SANSO-COMMENT-SEC
      *
           END-IF
      *
           .
       2501-SANSO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    »ÀÁÇ¤Î¿ôÎÌ¡¦Ã±²ÁÂàÈò
      *****************************************************************
       25011-SANSO-TAIHI-SEC          SECTION.
      *
           MOVE       ZERO    TO     IDX4
           INITIALIZE                TBL-SANSO-INF
      *
           INITIALIZE                       RECE659-REC
           MOVE    RECE649-HOSPNUM      TO  RECE659-HOSPNUM
           MOVE    RECE649-SRYYM        TO  RECE659-SRYYM
           MOVE    RECE649-NYUGAIKBN    TO  RECE659-NYUGAIKBN
           MOVE    RECE649-PTID         TO  RECE659-PTID
           MOVE    RECE649-RECEKA       TO  RECE659-RECEKA
           MOVE    RECE649-TEISYUTUSAKI TO  RECE659-TEISYUTUSAKI
           MOVE    RECE649-RECESYUBETU  TO  RECE659-RECESYUBETU
           MOVE    RECE649-HKNJANUM     TO  RECE659-HKNJANUM
           MOVE    RECE649-HOJOKBN      TO  RECE659-HOJOKBN
           MOVE    RECE649-TEKSTYMD     TO  RECE659-TEKSTYMD
           MOVE    RECE649-ZAINUM       TO  RECE659-ZAINUM
           MOVE    RECE649-ZAIREN       TO  RECE659-ZAIREN
           START   RECE659-FILE
               KEY IS    >=   RECE659-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE659
               NOT INVALID
                   PERFORM 990-RECE659-NEXT-SEC
           END-START
           PERFORM
               UNTIL ( FLG-RECE659           =  1                    )
               OR    ( RECE649-HOSPNUM   NOT =  RECE659-HOSPNUM      )
               OR    ( RECE649-SRYYM     NOT =  RECE659-SRYYM        )
               OR    ( RECE649-NYUGAIKBN NOT =  RECE659-NYUGAIKBN    )
               OR    ( RECE649-PTID      NOT =  RECE659-PTID         )
               OR    ( RECE649-RECEKA    NOT =  RECE659-RECEKA       )
               OR    ( RECE649-TEISYUTUSAKI
                                         NOT =  RECE659-TEISYUTUSAKI )
               OR    ( RECE649-RECESYUBETU
                                         NOT =  RECE659-RECESYUBETU  )
               OR    ( RECE649-HKNJANUM  NOT =  RECE659-HKNJANUM     )
               OR    ( RECE649-HOJOKBN   NOT =  RECE659-HOJOKBN      )
               OR    ( RECE649-TEKSTYMD  NOT =  RECE659-TEKSTYMD     )
               OR    ( RECE649-ZAINUM    NOT =  RECE659-ZAINUM       )
               OR    ( RECE649-ZAIREN    NOT =  RECE659-ZAIREN       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                          UNTIL  ( IDX                >   5     )
                           OR    ( RECE659-SRYCD(IDX) =   SPACE )
      *
                  SET     TBL-SANSO    TO     1
                  SEARCH      TBL-SANSO-OCC   VARYING   TBL-SANSO
                       AT  END
                           MOVE    ZERO    TO   FLG-SANSO-OK
                       WHEN   RECE659-SRYCD(IDX) = TBL-SANSO-SRYCD
                                                       (TBL-SANSO)
                           MOVE    1       TO   FLG-SANSO-OK
                  END-SEARCH
                  IF      FLG-SANSO-OK        =    1
                     ADD      1             TO  IDX4
                     MOVE  RECE659-TEN(IDX) TO  TBL-SANSO-TEN(IDX4)
                     MOVE  RECE659-SRYSURYO(IDX)
                                            TO  TBL-SANSO-SRYSURYO
                                                             (IDX4)
                  END-IF
                  IF    RECE659-SRYCD(IDX) = "770030070"
                     IF   TBL-KASAN-SRYSURYO  =  ZERO
                        MOVE   RECE659-SRYSURYO(IDX)
                                     TO  TBL-KASAN-SRYSURYO
                     ELSE
                        COMPUTE   TBL-KASAN-SRYSURYO  =  
                                  TBL-KASAN-SRYSURYO  *  
                                  RECE659-SRYSURYO(IDX)   
                     END-IF
                  END-IF
      *
               END-PERFORM 
      *
               PERFORM 990-RECE659-NEXT-SEC
      *
           END-PERFORM
      *
           .
       25011-SANSO-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    »ÀÁÇ¤Î¥³¥á¥ó¥ÈÊÔ½¸¡Êº¹¹þ¡§²¼¡Ë
      *****************************************************************
       25011-SANSO-COMMENT-SEC          SECTION.
      *
           MOVE       ZERO       TO   IDZ                               
           INITIALIZE            WRK-FREE-AREA2                         
           INITIALIZE            WRK-FREE-AREA3
      *
           PERFORM  250111-SANSO-FREE-COMMENT-SEC
      *
           PERFORM  250111-RECE659-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£¶£µ£¹¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
           MOVE    ZERO                 TO  WRK-JYUNBAN
                                            WRK-GYOSUU
                                            WRK-GYOSUU2
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
      *
                ADD       1      TO     WRK-GYOSUU
      *
           END-PERFORM 
      *
           PERFORM    250111-SRYACTINF2-HENSYU-SEC
      *
           .
       25011-SANSO-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    »ÀÁÇ·×»»¼°ÊÔ½¸
      *****************************************************************
       250111-SANSO-FREE-COMMENT-SEC          SECTION.
      *
           MOVE     1          TO    WRK-KETA
           MOVE    ZERO        TO    WRK-KEISAN4
      *
           PERFORM   VARYING   IDX    FROM   1   BY   1
                      UNTIL   (IDX                 >    10)
                       OR     (TBL-SANSO-TEN(IDX)  =  ZERO)
      *
             IF   IDX  >  1
                MOVE    "¡Ü"   TO  WK-FREE-NAME3(WRK-KETA:2)
                COMPUTE  WRK-KETA   =  WRK-KETA   +  2
             END-IF
      *
             MOVE    "¡Ê"   TO  WK-FREE-NAME3(WRK-KETA:2)
             COMPUTE  WRK-KETA   =  WRK-KETA   +  2
      *Ã±²Á
             MOVE     TBL-SANSO-TEN(IDX)   TO   WRK-KEISAN
             COMPUTE  WRK-KEISAN2  = (TBL-SANSO-TEN(IDX) -  WRK-KEISAN)
                                   *   1000
      *
             PERFORM  2501111-SANSO-FREE-COMMENT-SUCHI-SEC
      *
             MOVE    "±ß¡ß" TO  WK-FREE-NAME3(WRK-KETA:4)
             COMPUTE  WRK-KETA   =  WRK-KETA   +  4
      *¿ôÎÌ
             MOVE     TBL-SANSO-SRYSURYO(IDX)  TO  WRK-KEISAN
             COMPUTE  WRK-KEISAN2  
                             = (TBL-SANSO-SRYSURYO(IDX) -  WRK-KEISAN)
                             *   1000
      *
             PERFORM  2501111-SANSO-FREE-COMMENT-SUCHI-SEC
      *
             IF    TBL-KASAN-SRYSURYO  =  ZERO
                MOVE    "£Ì¡ß£±¡¥£³¡Ë" TO  WK-FREE-NAME3(WRK-KETA:12)
                COMPUTE  WRK-KETA   =  WRK-KETA   +  12
      *
      ***       COMPUTE  WRK-KEISAN3   =   WRK-KEISAN3  +
      ***         (TBL-SANSO-TEN(IDX) * TBL-SANSO-SRYSURYO(IDX) * 1.3)
                COMPUTE  WRK-KEISAN3   =
                  (TBL-SANSO-TEN(IDX) * TBL-SANSO-SRYSURYO(IDX) * 1.3)
                                                                + 0.5
                COMPUTE  WRK-KEISAN4  = WRK-KEISAN4 + WRK-KEISAN3
             ELSE
                MOVE    "£Ì¡ß£±¡¥£³¡ß" TO  WK-FREE-NAME3(WRK-KETA:12)
                COMPUTE  WRK-KETA   =  WRK-KETA   +  12
      *
                MOVE     TBL-KASAN-SRYSURYO  TO  WRK-KEISAN
                COMPUTE  WRK-KEISAN2  
                             = (TBL-KASAN-SRYSURYO -  WRK-KEISAN)
                             *   1000
      *
                PERFORM  2501111-SANSO-FREE-COMMENT-SUCHI-SEC
      *
                MOVE    "¡Ë" TO  WK-FREE-NAME3(WRK-KETA:2)
                COMPUTE  WRK-KETA   =  WRK-KETA   +  2
      *
      ***       COMPUTE  WRK-KEISAN3   =   WRK-KEISAN3  +
      ***         (TBL-SANSO-TEN(IDX) * TBL-SANSO-SRYSURYO(IDX) * 1.3
      ***                             * TBL-KASAN-SRYSURYO)
                COMPUTE  WRK-KEISAN3   =
                  (TBL-SANSO-TEN(IDX) * TBL-SANSO-SRYSURYO(IDX) * 1.3
                                      * TBL-KASAN-SRYSURYO)     + 0.5
                COMPUTE  WRK-KEISAN4  = WRK-KEISAN4 + WRK-KEISAN3
             END-IF
      *
           END-PERFORM
      *
           IF   IDX  =  2
              MOVE    "¡à£±£°¡á"  TO  WK-FREE-NAME3(WRK-KETA:8)
              COMPUTE  WRK-KETA   =  WRK-KETA   +  8
      *
      ***     COMPUTE  WRK-KEISAN =   WRK-KEISAN3  +  0.5
      ***     COMPUTE  WRK-KEISAN =  (WRK-KEISAN   /  10)  +  0.5
              COMPUTE  WRK-KEISAN =  (WRK-KEISAN4  /  10)  +  0.5
              MOVE     ZERO       TO   WRK-KEISAN2
      *
              PERFORM  2501111-SANSO-FREE-COMMENT-SUCHI-SEC
      *
              MOVE    "ÅÀ"   TO  WK-FREE-NAME3(WRK-KETA:2)
              COMPUTE  WRK-KETA   =  WRK-KETA   +  2
           ELSE
              IF   IDX  >  2
                  MOVE   WK-FREE-NAME3  TO  WK-FREE-NAME3-TAIHI
                  MOVE    "¡Ê"    TO   WK-FREE-NAME3(1:2)
                  MOVE   WK-FREE-NAME3-TAIHI
                                  TO   WK-FREE-NAME3(3:)
                  COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                  MOVE    "¡Ë¡à£±£°¡á"  TO  WK-FREE-NAME3(WRK-KETA:10)
                  COMPUTE  WRK-KETA   =  WRK-KETA   +  10
      *
      ***         COMPUTE  WRK-KEISAN =   WRK-KEISAN3  +  0.5
      ***         COMPUTE  WRK-KEISAN =  (WRK-KEISAN   /  10)  +  0.5
                  COMPUTE  WRK-KEISAN =  (WRK-KEISAN4  /  10)  +  0.5
                  MOVE     ZERO       TO   WRK-KEISAN2
      *
                  PERFORM  2501111-SANSO-FREE-COMMENT-SUCHI-SEC
      *
                  MOVE    "ÅÀ"   TO  WK-FREE-NAME3(WRK-KETA:2)
                  COMPUTE  WRK-KETA   =  WRK-KETA   +  2
              END-IF
           END-IF
      *
           IF     WRK-KETA    >   1
               PERFORM   VARYING   IDX4    FROM   1   BY   50
                          UNTIL   (IDX4     >    1000)
                            OR    (WK-FREE-NAME3(IDX4:1) = SPACE)
                   ADD     1      TO    IDZ
                   MOVE    ZERO   TO    WRK-KETA2
                   PERFORM   VARYING   IDX5    FROM   1   BY   2
                              UNTIL   (IDX5     >    50)
                      IF   WK-FREE-NAME3(IDX4:IDX5) = SPACE
                          MOVE   IDX5   TO   WRK-KETA2
                          MOVE   50     TO   IDX5
                      END-IF
                   END-PERFORM
                   IF   WRK-KETA2 =  ZERO
                        MOVE   25   TO  WK-FREE-YUKOKETA2(IDZ)
                   ELSE
                        COMPUTE   WK-FREE-YUKOKETA2(IDZ)  =
                                       ( WRK-KETA2 - 1 )  /  2
                   END-IF
                   MOVE   WK-FREE-NAME3(IDX4:50)
                                   TO    WK-FREE-NAME2(IDZ)
               END-PERFORM
           END-IF
      *
           .
       250111-SANSO-FREE-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    »ÀÁÇ·×»»¼°¡¦¿ôÃÍÊÔ½¸
      *****************************************************************
       2501111-SANSO-FREE-COMMENT-SUCHI-SEC          SECTION.          
      *
           IF    WRK-KEISAN  NOT  =  ZERO
               PERFORM   VARYING   IDX4    FROM   1   BY   1
                          UNTIL   (IDX4               >    9)
                     IF   WRK-KEISAN(IDX4:1) =   "0"
                        CONTINUE
                     ELSE
                        MOVE  IDX4   TO  IDX5
                        MOVE   9     TO  IDX4
                     END-IF
               END-PERFORM
               MOVE  SPACE             TO  WRK-KEISAN-TAIHI
               MOVE  WRK-KEISAN(IDX5:) TO  WRK-KEISAN-TAIHI
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI
               MOVE  WRK-KEISAN-TAIHI  TO  KANACHK-MAE-INPUT
               CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      KANACHK-RC      =   ZERO
                   MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                               TO  WK-FREE-NAME3(WRK-KETA:KANACHK-MAX)
                   COMPUTE  WRK-KETA   =  WRK-KETA   +  KANACHK-MAX
               END-IF
           END-IF
      *
           IF    WRK-KEISAN2  NOT  =  ZERO
                PERFORM   VARYING   IDX4    FROM   3   BY  -1
                           UNTIL   (IDX4               <    1)
                      IF   WRK-KEISAN2(IDX4:1) =   "0"
                         CONTINUE
                      ELSE
                         MOVE  IDX4   TO  IDX5
                         MOVE   1     TO  IDX4
                      END-IF
                END-PERFORM
                MOVE  SPACE               TO  WRK-KEISAN-TAIHI
                MOVE  WRK-KEISAN2(1:IDX5) TO  WRK-KEISAN-TAIHI
      *
                IF    WRK-KEISAN  NOT  =  ZERO
                   MOVE    "¡¥"     TO  WK-FREE-NAME3(WRK-KETA:2)
                   COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                ELSE
                   MOVE    "£°¡¥"   TO  WK-FREE-NAME3(WRK-KETA:4)
                   COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                END-IF
      *
                INITIALIZE                  ORCSKANACHKAREA
                MOVE    "2"             TO  KANACHK-SYORI
                MOVE  WRK-KEISAN-TAIHI  TO  KANACHK-MAE-INPUT
                CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
                IF      KANACHK-RC      =   ZERO
                    MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                TO  WK-FREE-NAME3(WRK-KETA:KANACHK-MAX)
                    COMPUTE  WRK-KETA   =  WRK-KETA   +  KANACHK-MAX
                END-IF
           END-IF
      *
           IF    WRK-KEISAN    =  ZERO
           AND   WRK-KEISAN2   =  ZERO
               MOVE    "£°"   TO  WK-FREE-NAME3(WRK-KETA:2)
               COMPUTE  WRK-KETA   =  WRK-KETA   +  2
           END-IF
      *
           .
       2501111-SANSO-FREE-COMMENT-SUCHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    £¶£µ£¹¡¡ÂàÈò½èÍý
      *****************************************************************
       250111-RECE659-TAIHI-SEC     SECTION.
      *
           INITIALIZE                       RECE659-REC
           MOVE    RECE649-HOSPNUM      TO  RECE659-HOSPNUM
           MOVE    RECE649-SRYYM        TO  RECE659-SRYYM
           MOVE    RECE649-NYUGAIKBN    TO  RECE659-NYUGAIKBN
           MOVE    RECE649-PTID         TO  RECE659-PTID
           MOVE    RECE649-RECEKA       TO  RECE659-RECEKA
           MOVE    RECE649-TEISYUTUSAKI TO  RECE659-TEISYUTUSAKI
           MOVE    RECE649-RECESYUBETU  TO  RECE659-RECESYUBETU
           MOVE    RECE649-HKNJANUM     TO  RECE659-HKNJANUM
           MOVE    RECE649-HOJOKBN      TO  RECE659-HOJOKBN
           MOVE    RECE649-TEKSTYMD     TO  RECE659-TEKSTYMD
           MOVE    RECE649-ZAINUM       TO  RECE659-ZAINUM
           MOVE    RECE649-ZAIREN       TO  RECE659-ZAIREN
           START   RECE659-FILE
                 KEY IS    >=   RECE659-KEY
                 INVALID
                     MOVE    1          TO  FLG-RECE659
                 NOT INVALID
                     PERFORM 990-RECE659-NEXT-SEC
                     MOVE   RECE659-REC TO  WRK-RECE65-REC
           END-START
      *
           .
       250111-RECE659-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    £¶£µ£¹¡¡£Ó£Ò£Ù£Á£Ã£Ô£É£Î£ÆÊÔ½¸½èÍý
      *****************************************************************
       250111-SRYACTINF2-HENSYU-SEC     SECTION.
      *
           PERFORM
               UNTIL  ( FLG-RECE659           =   1                    )
               OR     ( RECE649-HOSPNUM   NOT =   RECE659-HOSPNUM      )
               OR     ( RECE649-SRYYM     NOT =   RECE659-SRYYM        )
               OR     ( RECE649-NYUGAIKBN NOT =   RECE659-NYUGAIKBN    )
               OR     ( RECE649-PTID      NOT =   RECE659-PTID         )
               OR     ( RECE649-RECEKA    NOT =   RECE659-RECEKA       )
               OR     ( RECE649-TEISYUTUSAKI
                                          NOT =   RECE659-TEISYUTUSAKI )
               OR     ( RECE649-RECESYUBETU
                                          NOT =   RECE659-RECESYUBETU  )
               OR     ( RECE649-HKNJANUM  NOT =   RECE659-HKNJANUM     )
               OR     ( RECE649-HOJOKBN   NOT =   RECE659-HOJOKBN      )
               OR     ( RECE649-TEKSTYMD  NOT =   RECE659-TEKSTYMD     )
               OR     ( RECE649-ZAINUM    NOT =   RECE659-ZAINUM       )
               OR     ( RECE649-ZAIREN    NOT =   RECE659-ZAIREN       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   5     )
                         OR      ( RECE659-SRYCD(IDX) =   SPACE )
                    ADD    1             TO      IDX-RECE65
                    MOVE   RECE659-SRYACT-TBL(IDX)
                             TO   WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
      *
                    IF   RECE659-SRYCD(IDX) = "770020070" OR "770030070"
                         MOVE   IDX-RECE65   TO   WRK-JYUNBAN
                    END-IF
      *
               END-PERFORM
      *
               PERFORM 990-RECE659-NEXT-SEC
      *
           END-PERFORM
      *
           IF     WRK-GYOSUU   NOT =  ZERO
      *
      *      ÊäÀµÎ¨Ëô¤Ï¹âµ¤°µ»ÀÁÇ²Ã»»°Ê¹ß¤ÎÌÀºÙ¤ò¤º¤é¤¹
             IF   WRK-JYUNBAN  NOT =  ZERO
      *
               PERFORM   VARYING   IDX   FROM   IDX-RECE65   BY  -1
                          UNTIL  ( IDX    <    1           )
                           OR    ( IDX    <=   WRK-JYUNBAN )
      *
                   COMPUTE   WRK-GYOSUU2  =  IDX  +  WRK-GYOSUU
                   MOVE   WRK-RECE65XX-SRYACT-TBL(IDX 1)  TO
                          WRK-RECE65XX-SRYACT-TBL(WRK-GYOSUU2 1)
                   INITIALIZE   WRK-RECE65XX-SRYACT-TBL(IDX 1)
      *
               END-PERFORM
      *
             END-IF
      *
      *      ·×»»¼°¤Îº¹¤·¹þ¤ß
             IF   WRK-JYUNBAN  NOT =  ZERO
                 MOVE      WRK-JYUNBAN    TO     MAX-MEISAISU
             ELSE
                 MOVE      IDX-RECE65     TO     MAX-MEISAISU
             END-IF
             PERFORM   VARYING   IDX   FROM    1   BY  1
                        UNTIL  ( IDX                >   50    )
                         OR    ( WK-FREE-NAME2(IDX) =   SPACE )
      *
                ADD    1     TO  MAX-MEISAISU
                MOVE   "810000001"
                             TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
                MOVE   "1"   TO  WRK-RECE65XX-RECEDENFLG(MAX-MEISAISU 1)
      *
             END-PERFORM
      *
           END-IF
      *
      *    ¥³¥á¥ó¥È¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
                    INITIALIZE                        RECE651-REC
                    MOVE    WRK-RECE65-KEY       TO   RECE651-KEY
                    ADD     1                    TO   WRK-RENNUM
                    MOVE    WRK-RENNUM           TO   RECE651-RENNUM
                    MOVE    WRK-RECE65-KOHBUNRI  TO   RECE651-KOHBUNRI
                    MOVE    WRK-RECE65-PTNUM     TO   RECE651-PTNUM
                    MOVE    WRK-RECE65-SRYKA     TO   RECE651-SRYKA
                    MOVE    WRK-RECE65-SRYKBN    TO   RECE651-SRYKBN
                    MOVE    WRK-RECE65-SRYSYUKBN TO   RECE651-SRYSYUKBN
                    MOVE    WRK-RECE65-JIHIMONEY TO   RECE651-JIHIMONEY
                    PERFORM  VARYING   IDX   FROM   1   BY   1
                             UNTIL   (IDX       >    5)   OR
                                     (FLG-WRKRECE65   =   1)
                       ADD         1             TO    IDX-RECE65
                       IF  WRK-RECE65XX-SRYCD(IDX-RECE65 1) = SPACE
                           MOVE    1             TO    FLG-WRKRECE65
                       ELSE
                           MOVE  WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
                                            TO   RECE651-SRYACT-TBL(IDX)
                           ADD   1          TO   WRK-CNT-MEISAI
                       END-IF
                    END-PERFORM
                    IF   RECE651-SRYCD(1)  NOT =  SPACE
                       WRITE   RECE651-REC
                       ADD     1            TO   CNT-RECE65
                    END-IF
           END-PERFORM
      *
           MOVE    RECE649-REC       TO      RECE641-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE641-MEISAISU
           WRITE   RECE641-REC
           ADD     1                 TO      CNT-RECE64
      *
           .
       250111-SRYACTINF2-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÎÅÍÜÉÂÅï¥³¥á¥ó¥È¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2502-RYOCOM-HANTEI-SEC          SECTION.
      *
           MOVE     ZERO     TO     FLG-RYOCOM2
           INITIALIZE               TBL-RYOCOM2-INF
      *
           PERFORM  VARYING  RYO-IDX  FROM  1  BY  1
                     UNTIL   RYO-IDX   >   31
      *
               IF    (RECE649-KAISU(1 RYO-IDX)   NOT =  ZERO)
               AND   (TBL-RYOCOM-NYUINKBN(RYO-IDX)   =  "1" )
      *
                  MOVE     ZERO     TO    FLG-ARI
                  PERFORM  VARYING  RYO-IDX3  FROM  1  BY  1
                            UNTIL  (RYO-IDX3   >   31)
                             OR    (TBL-RYOCOM2-MEISAISU(RYO-IDX3)
                                               = ZERO)
                             OR    (FLG-ARI    =    1)
                   IF   RECE62-SRYYM  >=  "201004"
                     IF  (TBL-RYOCOM2-MEISAISU  (RYO-IDX3) =
                                    TBL-RYOCOM-MEISAISU  (RYO-IDX))
                     AND (TBL-RYOCOM2-SRYCDTOTAL(RYO-IDX3) =
                                    TBL-RYOCOM-SRYCDTOTAL(RYO-IDX))
                     AND (TBL-RYOCOM2-ADL-TEN-H22(RYO-IDX3 1) =
                                 TBL-RYOCOM-ADL-TEN-H22(RYO-IDX 1))
                     AND (TBL-RYOCOM2-ADL-TEN-H22(RYO-IDX3 2) =
                                 TBL-RYOCOM-ADL-TEN-H22(RYO-IDX 2))
                     AND (TBL-RYOCOM2-ADL-TEN-H22(RYO-IDX3 3) =
                                 TBL-RYOCOM-ADL-TEN-H22(RYO-IDX 3))
                     AND (TBL-RYOCOM2-ADL-TEN-H22(RYO-IDX3 4) =
                                 TBL-RYOCOM-ADL-TEN-H22(RYO-IDX 4))
                        MOVE   1   TO  TBL-RYOCOM2-DAY(RYO-IDX3 RYO-IDX)
                        MOVE   1   TO  FLG-ARI
                     END-IF
                   ELSE
                     IF  (TBL-RYOCOM2-MEISAISU  (RYO-IDX3) =
                                    TBL-RYOCOM-MEISAISU  (RYO-IDX))
                     AND (TBL-RYOCOM2-SRYCDTOTAL(RYO-IDX3) =
                                    TBL-RYOCOM-SRYCDTOTAL(RYO-IDX))
                     AND (TBL-RYOCOM2-ADL-TEN   (RYO-IDX3) =
                                    TBL-RYOCOM-ADL-TEN   (RYO-IDX))
                        MOVE   1   TO  TBL-RYOCOM2-DAY(RYO-IDX3 RYO-IDX)
                        MOVE   1   TO  FLG-ARI
                     END-IF
                   END-IF
                  END-PERFORM
                  IF  FLG-ARI  =  ZERO
                     MOVE  TBL-RYOCOM-AREA(RYO-IDX)
                               TO  TBL-RYOCOM2-AREA(RYO-IDX3)
                     MOVE  1   TO  TBL-RYOCOM2-DAY (RYO-IDX3 RYO-IDX)
                  END-IF
      *
                  MOVE      1      TO     FLG-RYOCOM2
      *
               END-IF
      *
           END-PERFORM
      *
           IF  FLG-RYOCOM2   =   ZERO
              PERFORM   2599-RECE649-TENSOU-SEC
           ELSE
            IF   RECE62-SRYYM  >=  "201004"
              PERFORM   25021-RYOCOM-HENSYU-H22-SEC
            ELSE
              PERFORM   25021-RYOCOM-HENSYU-SEC
            END-IF
           END-IF
      *
           .
       2502-RYOCOM-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÎÅÍÜÉÂÅï¥³¥á¥ó¥ÈÊÔ½¸¡ÊÊ¿À®£²£²Ç¯£´·î¿ÇÎÅ¡Á¡Ë
      *****************************************************************
       25021-RYOCOM-HENSYU-H22-SEC          SECTION.
      *
           PERFORM  250111-RECE659-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX2-REC
           INITIALIZE                       WRK-RECE65XX2-REC
      *
           MOVE    RECE649-MEISAISU     TO  MAX-MEISAISU
      *
      *
           PERFORM  VARYING  RYO-IDX3  FROM  1  BY  1
                     UNTIL  (RYO-IDX3   >   31                    )
                      OR    (TBL-RYOCOM2-MEISAISU(RYO-IDX3) = ZERO)
      *
              INITIALIZE                TBL-NYU-DAY
      *
              MOVE    ZERO          TO  DAY-IDX
              MOVE    ZERO          TO  FLG-NYU-RENZOKU
      *
              PERFORM    VARYING    IDX  FROM   1   BY   1
                          UNTIL    (IDX      >   31)  OR
                                   (DAY-IDX  >   31)
                IF    TBL-RYOCOM2-DAY(RYO-IDX3 IDX)   =  ZERO
                      MOVE  ZERO  TO  FLG-NYU-RENZOKU
                ELSE
                      IF    FLG-NYU-RENZOKU  =  ZERO
                         ADD   1     TO  DAY-IDX
                         MOVE  IDX   TO  TBL-NYU-DAY-ST(DAY-IDX)
                      END-IF
                      MOVE  IDX      TO  TBL-NYU-DAY-ED(DAY-IDX)
                      MOVE  1        TO  FLG-NYU-RENZOKU
                END-IF
              END-PERFORM
      *
      ***     PERFORM  VARYING  DAY-IDX  FROM   1   BY   1
      ***               UNTIL  (DAY-IDX                  >   31)
      ***                OR    (TBL-NYU-DAY-ST(DAY-IDX)  = ZERO)
      ***       IF    TBL-NYU-DAY-ST(DAY-IDX) 
      ***                           =   TBL-NYU-DAY-ED(DAY-IDX)
      ***             MOVE  "840000059"
      ***                           TO  TBL-NYU-DAY-CD(DAY-IDX)
      ***       ELSE
      ***             MOVE  "810000001"
      ***                           TO  TBL-NYU-DAY-CD(DAY-IDX)
      ***       END-IF
      ***     END-PERFORM
      *
      *       »»ÄêÆü¥³¥á¥ó¥È¤ÎÁÞÆþ
              PERFORM  VARYING   IDX    FROM  1   BY   1
                        UNTIL   (IDX                  >    31)
                         OR     (TBL-NYU-DAY-ST(IDX)  =  ZERO)
               COMPUTE  MAX-MEISAISU   =   MAX-MEISAISU  +  1
               IF  MAX-MEISAISU <= 300
                 MOVE  "810000001"
                             TO  WRK-RECE65XX2-SRYCD(MAX-MEISAISU 1)
                 MOVE   ZERO TO  WRK-RECE65XX2-SRYSURYO(MAX-MEISAISU 1)
      ***        IF    TBL-NYU-DAY-CD(IDX)  =  "840000059"
      ***          MOVE   16 TO  WRK-RECE65XX2-YUKOKETA(MAX-MEISAISU 1)
      ***          MOVE   "ÎÅÍÜÉÂÅïÆþ±¡´ü´Ö¡¡¡¡¡¡Æü¡Á¡¡¡¡Æü"
      ***                  TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
      ***          MOVE   TBL-NYU-DAY-ST(IDX) TO  DAY-IDX2
      ***          MOVE   TBL-DAY(DAY-IDX2)
      ***                  TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(19:4)
      ***          MOVE   TBL-NYU-DAY-ED(IDX) TO  DAY-IDX2
      ***          MOVE   TBL-DAY(DAY-IDX2)
      ***                  TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(27:4)
      ***        ELSE
                   MOVE   16 TO  WRK-RECE65XX2-YUKOKETA(MAX-MEISAISU 1)
                   EVALUATE RECE62-HOSPSBT
                     WHEN    "1"
                       MOVE   "ÎÅÍÜÉÂÅïÆþ±¡´ü´Ö¡¡¡¡¡¡Æü¡Á¡¡¡¡Æü"
                           TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
                     WHEN    "2"
                       MOVE   "ÎÅÍÜÉÂ¾²Æþ±¡´ü´Ö¡¡¡¡¡¡Æü¡Á¡¡¡¡Æü"
                           TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
                   END-EVALUATE
                   MOVE   TBL-NYU-DAY-ST(IDX) TO  DAY-IDX2
                   MOVE   TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(19:4)
                   MOVE   TBL-NYU-DAY-ED(IDX) TO  DAY-IDX2
                   MOVE   TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(27:4)
      ***        END-IF
                 MOVE  "1" TO  WRK-RECE65XX2-RECEDENFLG(MAX-MEISAISU 1)
               END-IF
              END-PERFORM
      *
              PERFORM  VARYING  IDX  FROM   1   BY   1
                  UNTIL  (IDX                                  >  30  )
                   OR    (TBL-RYOCOM2-IRYO-SRYCD(RYO-IDX3 IDX) = SPACE)
      *
               COMPUTE  MAX-MEISAISU  =  MAX-MEISAISU  +  1
               IF  MAX-MEISAISU <= 300
                   MOVE "810000001"
                              TO WRK-RECE65XX2-SRYCD(MAX-MEISAISU 1)
                   MOVE  ZERO TO WRK-RECE65XX2-SRYSURYO(MAX-MEISAISU 1)
                   MOVE  4    TO WRK-RECE65XX2-YUKOKETA(MAX-MEISAISU 1)
                   PERFORM       250212-RYOCOM-NAME-HENSYU-SEC
                   MOVE "1" TO WRK-RECE65XX2-RECEDENFLG(MAX-MEISAISU 1)
               END-IF
      *
              END-PERFORM
      *
              PERFORM  VARYING  IDX  FROM   1   BY   1
                UNTIL (IDX   >   4)
      *
                IF  TBL-RYOCOM2-ADL-SRYCD-H22(RYO-IDX3 IDX) NOT = SPACE
                  COMPUTE  MAX-MEISAISU  =  MAX-MEISAISU  +  1
                  IF  MAX-MEISAISU <= 300
                    MOVE   "810000001"
                             TO WRK-RECE65XX2-SRYCD(MAX-MEISAISU 1)
                    MOVE   ZERO
                             TO WRK-RECE65XX2-SRYSURYO(MAX-MEISAISU 1)
                    EVALUATE  IDX
                      WHEN     1
                        MOVE 17
                           TO WRK-RECE65XX2-YUKOKETA(MAX-MEISAISU 1)
                        MOVE "£Á£Ä£Ì£á¡¡¥Ù¥Ã¥É¾å¤Î²ÄÆ°À­¡¡¡¡¡¡ÅÀ"
                           TO WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
                        MOVE TBL-RYOCOM2-ADL-TEN-MOJI-H22(RYO-IDX3 IDX)
                           TO WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(29:4)
                      WHEN     2
                        MOVE 11
                           TO WRK-RECE65XX2-YUKOKETA(MAX-MEISAISU 1)
                        MOVE "£Á£Ä£Ì£â¡¡°Ü¾è¡¡¡¡¡¡ÅÀ"
                           TO WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
                        MOVE TBL-RYOCOM2-ADL-TEN-MOJI-H22(RYO-IDX3 IDX)
                           TO WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(17:4)
                      WHEN     3
                        MOVE 11
                           TO WRK-RECE65XX2-YUKOKETA(MAX-MEISAISU 1)
                        MOVE "£Á£Ä£Ì£ã¡¡¿©»ö¡¡¡¡¡¡ÅÀ"
                           TO WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
                        MOVE TBL-RYOCOM2-ADL-TEN-MOJI-H22(RYO-IDX3 IDX)
                           TO WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(17:4)
                      WHEN     4
                        MOVE 15
                           TO WRK-RECE65XX2-YUKOKETA(MAX-MEISAISU 1)
                        MOVE "£Á£Ä£Ì£ä¡¡¥È¥¤¥ì¤Î»ÈÍÑ¡¡¡¡¡¡ÅÀ"
                           TO WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
                        MOVE TBL-RYOCOM2-ADL-TEN-MOJI-H22(RYO-IDX3 IDX)
                           TO WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(25:4)
                    END-EVALUATE
                    MOVE "1" TO WRK-RECE65XX2-RECEDENFLG(MAX-MEISAISU 1)
                  END-IF
                END-IF
      *
              END-PERFORM
      *
           END-PERFORM
      *
      *
           PERFORM    250211-SRYACTINF3-HENSYU-SEC
      *
           .
       25021-RYOCOM-HENSYU-H22-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÎÅÍÜÉÂÅï¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       25021-RYOCOM-HENSYU-SEC          SECTION.
      *
           PERFORM  250111-RECE659-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£°£µ¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX2-REC
           INITIALIZE                       WRK-RECE65XX2-REC
      *
           MOVE    RECE649-MEISAISU     TO  MAX-MEISAISU
      *
      *
           PERFORM  VARYING  RYO-IDX3  FROM  1  BY  1
                     UNTIL  (RYO-IDX3   >   31                    )
                      OR    (TBL-RYOCOM2-MEISAISU(RYO-IDX3) = ZERO)
      *
              INITIALIZE                TBL-NYU-DAY
      *
              MOVE    ZERO          TO  DAY-IDX
              MOVE    ZERO          TO  FLG-NYU-RENZOKU
      *
              PERFORM    VARYING    IDX  FROM   1   BY   1
                          UNTIL    (IDX      >   31)  OR
                                   (DAY-IDX  >   31)
                IF    TBL-RYOCOM2-DAY(RYO-IDX3 IDX)   =  ZERO
                      MOVE  ZERO  TO  FLG-NYU-RENZOKU
                ELSE
                      IF    FLG-NYU-RENZOKU  =  ZERO
                         ADD   1     TO  DAY-IDX
                         MOVE  IDX   TO  TBL-NYU-DAY-ST(DAY-IDX)
                      END-IF
                      MOVE  IDX      TO  TBL-NYU-DAY-ED(DAY-IDX)
                      MOVE  1        TO  FLG-NYU-RENZOKU
                END-IF
              END-PERFORM
      *
              PERFORM  VARYING  DAY-IDX  FROM   1   BY   1
                        UNTIL  (DAY-IDX                  >   31)
                         OR    (TBL-NYU-DAY-ST(DAY-IDX)  = ZERO)
                IF    TBL-NYU-DAY-ST(DAY-IDX) 
                                    =   TBL-NYU-DAY-ED(DAY-IDX)
                      MOVE  "840000059"
                                    TO  TBL-NYU-DAY-CD(DAY-IDX)
                ELSE
                      MOVE  "810000001"
                                    TO  TBL-NYU-DAY-CD(DAY-IDX)
                END-IF
              END-PERFORM
      *
      *       »»ÄêÆü¥³¥á¥ó¥È¤ÎÁÞÆþ
              PERFORM  VARYING   IDX    FROM  1   BY   1
                        UNTIL   (IDX                  >    31)
                         OR     (TBL-NYU-DAY-ST(IDX)  =  ZERO)
               COMPUTE  MAX-MEISAISU   =   MAX-MEISAISU  +  1
               IF  MAX-MEISAISU <= 300
                 MOVE  "810000001"
                             TO  WRK-RECE65XX2-SRYCD(MAX-MEISAISU 1)
                 MOVE   ZERO TO  WRK-RECE65XX2-SRYSURYO(MAX-MEISAISU 1)
                 IF    TBL-NYU-DAY-CD(IDX)  =  "840000059"
                   MOVE   6  TO  WRK-RECE65XX2-YUKOKETA(MAX-MEISAISU 1)
                   MOVE   "¡¡¡¡·î¡¡¡¡Æü"
                           TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
                   MOVE   RECE62-SRYYM(5:2)   TO  DAY-IDX2
                   MOVE   TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(1:4)
                   MOVE   TBL-NYU-DAY-ST(IDX) TO  DAY-IDX2
                   MOVE   TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(7:4)
                 ELSE
                   MOVE   13 TO  WRK-RECE65XX2-YUKOKETA(MAX-MEISAISU 1)
                   MOVE   "¡¡¡¡·î¡¡¡¡Æü¡Á¡¡¡¡·î¡¡¡¡Æü"
                           TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
                   MOVE   RECE62-SRYYM(5:2)   TO  DAY-IDX2
                   MOVE   TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(1:4)
                               WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(15:4)
                   MOVE   TBL-NYU-DAY-ST(IDX) TO  DAY-IDX2
                   MOVE   TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(7:4)
                   MOVE   TBL-NYU-DAY-ED(IDX) TO  DAY-IDX2
                   MOVE   TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(21:4)
                 END-IF
                 MOVE  "1" TO  WRK-RECE65XX2-RECEDENFLG(MAX-MEISAISU 1)
               END-IF
              END-PERFORM
      *
              PERFORM  VARYING  IDX  FROM   1   BY   1
                  UNTIL  (IDX                                  >  30  )
                   OR    (TBL-RYOCOM2-IRYO-SRYCD(RYO-IDX3 IDX) = SPACE)
      *
               COMPUTE  MAX-MEISAISU  =  MAX-MEISAISU  +  1
               IF  MAX-MEISAISU <= 300
                   MOVE "810000001"
                              TO WRK-RECE65XX2-SRYCD(MAX-MEISAISU 1)
                   MOVE  ZERO TO WRK-RECE65XX2-SRYSURYO(MAX-MEISAISU 1)
                   MOVE  4    TO WRK-RECE65XX2-YUKOKETA(MAX-MEISAISU 1)
                   PERFORM       250212-RYOCOM-NAME-HENSYU-SEC
                   MOVE "1" TO WRK-RECE65XX2-RECEDENFLG(MAX-MEISAISU 1)
               END-IF
      *
              END-PERFORM
      *
              IF   TBL-RYOCOM2-ADL-SRYCD(RYO-IDX3)  NOT =  SPACE
               COMPUTE  MAX-MEISAISU  =  MAX-MEISAISU  +  1
               IF  MAX-MEISAISU <= 300
                MOVE   "810000001"
                             TO WRK-RECE65XX2-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO WRK-RECE65XX2-SRYSURYO(MAX-MEISAISU 1)
                MOVE   7     TO WRK-RECE65XX2-YUKOKETA(MAX-MEISAISU 1)
                MOVE   "£Á£Ä£Ì¡¡¡¡¡¡ÅÀ"
                             TO WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
                MOVE   TBL-RYOCOM2-ADL-TEN-MOJI(RYO-IDX3)
                             TO WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(9:4)
                MOVE "1" TO WRK-RECE65XX2-RECEDENFLG(MAX-MEISAISU 1)
               END-IF
              END-IF
      *
           END-PERFORM
      *
      *
           PERFORM    250211-SRYACTINF3-HENSYU-SEC
      *
           .
       25021-RYOCOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    £¶£µ£¹¡¡£Ó£Ò£Ù£Á£Ã£Ô£É£Î£ÆÊÔ½¸½èÍý
      *****************************************************************
       250211-SRYACTINF3-HENSYU-SEC     SECTION.
      *
           PERFORM
               UNTIL  ( FLG-RECE659           =   1                    )
               OR     ( RECE649-HOSPNUM   NOT =   RECE659-HOSPNUM      )
               OR     ( RECE649-SRYYM     NOT =   RECE659-SRYYM        )
               OR     ( RECE649-NYUGAIKBN NOT =   RECE659-NYUGAIKBN    )
               OR     ( RECE649-PTID      NOT =   RECE659-PTID         )
               OR     ( RECE649-RECEKA    NOT =   RECE659-RECEKA       )
               OR     ( RECE649-TEISYUTUSAKI
                                          NOT =   RECE659-TEISYUTUSAKI )
               OR     ( RECE649-RECESYUBETU
                                          NOT =   RECE659-RECESYUBETU  )
               OR     ( RECE649-HKNJANUM  NOT =   RECE659-HKNJANUM     )
               OR     ( RECE649-HOJOKBN   NOT =   RECE659-HOJOKBN      )
               OR     ( RECE649-TEKSTYMD  NOT =   RECE659-TEKSTYMD     )
               OR     ( RECE649-ZAINUM    NOT =   RECE659-ZAINUM       )
               OR     ( RECE649-ZAIREN    NOT =   RECE659-ZAIREN       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                          UNTIL  ( IDX                >   5     )
                           OR    ( RECE659-SRYCD(IDX) =   SPACE )
                    ADD    1    TO    IDX-RECE65
                    MOVE   RECE659-SRYACT-TBL(IDX)
                           TO   WRK-RECE65XX2-SRYACT-TBL(IDX-RECE65 1)
               END-PERFORM
      *
               PERFORM 990-RECE659-NEXT-SEC
      *
           END-PERFORM
      *
      *    ¥³¥á¥ó¥È¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
               INITIALIZE                        RECE651-REC
               MOVE    WRK-RECE65-KEY       TO   RECE651-KEY
               ADD     1                    TO   WRK-RENNUM
               MOVE    WRK-RENNUM           TO   RECE651-RENNUM
               MOVE    WRK-RECE65-KOHBUNRI  TO   RECE651-KOHBUNRI
               MOVE    WRK-RECE65-PTNUM     TO   RECE651-PTNUM
               MOVE    WRK-RECE65-SRYKA     TO   RECE651-SRYKA
               MOVE    WRK-RECE65-SRYKBN    TO   RECE651-SRYKBN
               MOVE    WRK-RECE65-SRYSYUKBN TO   RECE651-SRYSYUKBN
               MOVE    WRK-RECE65-JIHIMONEY TO   RECE651-JIHIMONEY
               PERFORM  VARYING   IDX   FROM   1   BY   1
                         UNTIL   (IDX             >   5)
                          OR     (FLG-WRKRECE65   =   1)
                   ADD        1             TO   IDX-RECE65
                   IF  WRK-RECE65XX2-SRYCD(IDX-RECE65 1) = SPACE
                      MOVE    1             TO    FLG-WRKRECE65
                   ELSE
                      MOVE  WRK-RECE65XX2-SRYACT-TBL(IDX-RECE65 1)
                                       TO   RECE651-SRYACT-TBL(IDX)
                      ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
                   IF   IDX-RECE65   >= 300
                      MOVE    1        TO   FLG-WRKRECE65
                   END-IF
               END-PERFORM
               IF  RECE651-SRYCD(1)  NOT =  SPACE
                  WRITE   RECE651-REC
                  ADD     1            TO   CNT-RECE65
               END-IF
           END-PERFORM
      *
           MOVE    RECE649-REC       TO      RECE641-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE641-MEISAISU
      *
           MOVE    RECE649-MEISAISU  TO      RECE641-RYOTENKISAISU
      *
           WRITE   RECE641-REC
           ADD     1                 TO      CNT-RECE64
      *
           .
       250211-SRYACTINF3-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÎÅÍÜÉÂÅï¥³¥á¥ó¥ÈÌ¾¾ÎÊÔ½¸
      *****************************************************************
       250212-RYOCOM-NAME-HENSYU-SEC     SECTION.
      *
           EVALUATE  TBL-RYOCOM2-IRYO-SRYCD(RYO-IDX3 IDX)
             WHEN      "820000057"
             WHEN      "820000096"
                MOVE "¡Ö£°£±¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000058"
             WHEN      "820000097"
                MOVE "¡Ö£°£²¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000059"
             WHEN      "820000098"
                MOVE "¡Ö£°£³¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000060"
             WHEN      "820000099"
                MOVE "¡Ö£°£´¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000061"
             WHEN      "820000100"
                MOVE "¡Ö£°£µ¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000062"
             WHEN      "820000101"
                MOVE "¡Ö£°£¶¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000063"
             WHEN      "820000102"
                MOVE "¡Ö£°£·¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000064"
             WHEN      "820000103"
                MOVE "¡Ö£°£¸¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000065"
             WHEN      "820000104"
                MOVE "¡Ö£°£¹¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000066"
             WHEN      "820000105"
                MOVE "¡Ö£±£°¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000067"
             WHEN      "820000106"
                MOVE "¡Ö£±£±¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000068"
             WHEN      "820000107"
                MOVE "¡Ö£±£²¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000069"
             WHEN      "820000108"
                MOVE "¡Ö£±£³¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000070"
             WHEN      "820000109"
                MOVE "¡Ö£±£´¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000071"
             WHEN      "820000110"
                MOVE "¡Ö£±£µ¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000072"
             WHEN      "820000111"
                MOVE "¡Ö£±£¶¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000073"
             WHEN      "820000112"
                MOVE "¡Ö£±£·¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000074"
             WHEN      "820000113"
                MOVE "¡Ö£±£¸¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000075"
             WHEN      "820000114"
                MOVE "¡Ö£±£¹¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000076"
             WHEN      "820000115"
                MOVE "¡Ö£²£°¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000077"
             WHEN      "820000116"
                MOVE "¡Ö£²£±¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000078"
             WHEN      "820000117"
                MOVE "¡Ö£²£²¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000079"
             WHEN      "820000118"
                MOVE "¡Ö£²£³¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000080"
             WHEN      "820000119"
                MOVE "¡Ö£²£´¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000081"
             WHEN      "820000120"
                MOVE "¡Ö£²£µ¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000082"
             WHEN      "820000121"
                MOVE "¡Ö£²£¶¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000083"
             WHEN      "820000122"
                MOVE "¡Ö£²£·¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000084"
             WHEN      "820000123"
                MOVE "¡Ö£²£¸¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000085"
             WHEN      "820000124"
                MOVE "¡Ö£²£¹¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000086"
             WHEN      "820000125"
                MOVE "¡Ö£³£°¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000087"
             WHEN      "820000126"
                MOVE "¡Ö£³£±¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000088"
             WHEN      "820000127"
                MOVE "¡Ö£³£²¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000089"
             WHEN      "820000128"
                MOVE "¡Ö£³£³¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000090"
             WHEN      "820000129"
                MOVE "¡Ö£³£´¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000091"
             WHEN      "820000130"
                MOVE "¡Ö£³£µ¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000092"
             WHEN      "820000131"
                MOVE "¡Ö£³£¶¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000093"
             WHEN      "820000132"
                MOVE "¡Ö£³£·¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000158"
             WHEN      "820000160"
                MOVE "¡Ö£¹£±¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000159"
                MOVE "¡Ö£¹£²¡×"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
           END-EVALUATE
      *
           .
       250212-RYOCOM-NAME-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Õ¥¤¥ë¥à¤¬¤¢¤ë¤«È½Äê
      *****************************************************************
       2503-FILM-HANTEI-SEC          SECTION.
      *
           IF     RECE649-FLG-FILM-COMMENT   =    "1"
               MOVE       99      TO     FLG-COMMENT
               PERFORM    25031-FILM-COMMENT-SEC
           END-IF
      *
           .
       2503-FILM-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Õ¥¤¥ë¥à¤Î¥³¥á¥ó¥ÈÊÔ½¸¡Êº¹¹þ¡§²¼¡Ë
      *****************************************************************
       25031-FILM-COMMENT-SEC          SECTION.
      *
           INITIALIZE                WRK-JIDO-SETCOM-AREA
           MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
           MOVE       20      TO     WRK-JIDO-SETCOMATFLG
      *
           PERFORM  250111-RECE659-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£¶£µ£¹¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO       TO     IDX-RECE65
           MOVE    SPACE      TO     WRK-RECE65XX-REC
           INITIALIZE                WRK-RECE65XX-REC
           MOVE    ZERO       TO     WRK-JYUNBAN
      *
           PERFORM  250311-SRYACTINF2-HENSYU-SEC
      *
           .
       25031-FILM-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    £¶£µ£¹¡¡£Ó£Ò£Ù£Á£Ã£Ô£É£Î£ÆÊÔ½¸½èÍý
      *****************************************************************
       250311-SRYACTINF2-HENSYU-SEC     SECTION.
      *
           PERFORM
               UNTIL  ( FLG-RECE659           =   1                    )
               OR     ( RECE649-HOSPNUM   NOT =   RECE659-HOSPNUM      )
               OR     ( RECE649-SRYYM     NOT =   RECE659-SRYYM        )
               OR     ( RECE649-NYUGAIKBN NOT =   RECE659-NYUGAIKBN    )
               OR     ( RECE649-PTID      NOT =   RECE659-PTID         )
               OR     ( RECE649-RECEKA    NOT =   RECE659-RECEKA       )
               OR     ( RECE649-TEISYUTUSAKI
                                          NOT =   RECE659-TEISYUTUSAKI )
               OR     ( RECE649-RECESYUBETU
                                          NOT =   RECE659-RECESYUBETU  )
               OR     ( RECE649-HKNJANUM  NOT =   RECE659-HKNJANUM     )
               OR     ( RECE649-HOJOKBN   NOT =   RECE659-HOJOKBN      )
               OR     ( RECE649-TEKSTYMD  NOT =   RECE659-TEKSTYMD     )
               OR     ( RECE649-ZAINUM    NOT =   RECE659-ZAINUM       )
               OR     ( RECE649-ZAIREN    NOT =   RECE659-ZAIREN       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   5     )
                         OR      ( RECE659-SRYCD(IDX) =   SPACE )
                    ADD    1             TO      IDX-RECE65
                    MOVE   RECE659-SRYACT-TBL(IDX)
                             TO   WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
      *
                    IF   RECE659-SRYCD(IDX) =
                         "700010000" OR "700030000" OR "700050000" OR
                         "700080000" OR "700110000" OR "700140000" OR
                         "700290000" OR "700310000" OR "700330000" OR
                         "700710000" OR "700730000" OR "700750000" OR
                         "700780000" OR "700810000" OR "700840000" OR
                         "700870000" OR "700160000" OR "700180000" OR
                         "700190000" OR "700200000" OR "700210000" OR
                         "700220000" OR "700230000" OR "700240000" OR
                         "700250000" OR "700260000" OR "700270000" OR
                         "700280000" OR "700590000" OR "700600000" OR
                         "700630000" OR "700640000" OR "700650000" OR
                         "700680000" OR "729780000"
                         MOVE   IDX-RECE65   TO   WRK-JYUNBAN
                    END-IF
      *
               END-PERFORM
      *
               PERFORM 990-RECE659-NEXT-SEC
      *
           END-PERFORM
      *
      *    ¥Õ¥£¥ë¥à°Ê¹ß¤ÎÌÀºÙ¤ò¤º¤é¤¹
           IF   WRK-JYUNBAN  NOT =  ZERO
      *
               PERFORM   VARYING   IDX   FROM   IDX-RECE65   BY  -1
                          UNTIL  ( IDX    <    1           )
                           OR    ( IDX    <=   WRK-JYUNBAN )
      *
                   COMPUTE   WRK-GYOSUU2  =  IDX  +  1
                   MOVE   WRK-RECE65XX-SRYACT-TBL(IDX 1)  TO
                          WRK-RECE65XX-SRYACT-TBL(WRK-GYOSUU2 1)
                   INITIALIZE   WRK-RECE65XX-SRYACT-TBL(IDX 1)
      *
               END-PERFORM
      *
      *        Ê¬²è¿ô¤Îº¹¤·¹þ¤ß
               MOVE   WRK-JYUNBAN     TO  MAX-MEISAISU
      *
               ADD    1               TO  MAX-MEISAISU
               MOVE  "810000001"
                              TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
               MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
      *
               MOVE   WRK-RECE65XX-SRYKAISU(WRK-JYUNBAN 1)
                                      TO  WRK-SRYKAISU-X
      *        Á´³ÑÊÑ´¹
               INITIALIZE                     WRK-CHAR-HENKAN-AREA
               MOVE     WRK-SRYKAISU-X  TO    WRK-CHAR-X
               PERFORM  2503111-NUM-ZENKAKU-HENKAN-SEC
               STRING   WRK-CHAR-N      DELIMITED  BY  SPACE
                        "Ê¬²è"          DELIMITED  BY  SIZE
                                        INTO  WRK-BUNKAKU-N
               COMPUTE WRK-BUNKAKUKETA   =   WRK-CHAR-N-CNT  +  4
      *
               COMPUTE WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1) =
                       WRK-BUNKAKUKETA   /   2
               MOVE    WRK-BUNKAKU-N
                           TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
               MOVE    "1" TO  WRK-RECE65XX-RECEPTFLG(MAX-MEISAISU 1)
      *
               IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                 MOVE  "2" TO  WRK-RECE65XX-MEISKYFLG(MAX-MEISAISU 1)
                 MOVE  WRK-JIDO-SETCOMATFLG
                           TO  WRK-RECE65XX-SETCOMATFLG(MAX-MEISAISU 1)
               END-IF
      *
           END-IF
      *
      *    ¥³¥á¥ó¥È¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
                    INITIALIZE                        RECE651-REC
                    MOVE    WRK-RECE65-KEY       TO   RECE651-KEY
                    ADD     1                    TO   WRK-RENNUM
                    MOVE    WRK-RENNUM           TO   RECE651-RENNUM
                    MOVE    WRK-RECE65-KOHBUNRI  TO   RECE651-KOHBUNRI
                    MOVE    WRK-RECE65-PTNUM     TO   RECE651-PTNUM
                    MOVE    WRK-RECE65-SRYKA     TO   RECE651-SRYKA
                    MOVE    WRK-RECE65-SRYKBN    TO   RECE651-SRYKBN
                    MOVE    WRK-RECE65-SRYSYUKBN TO   RECE651-SRYSYUKBN
                    MOVE    WRK-RECE65-JIHIMONEY TO   RECE651-JIHIMONEY
                    PERFORM  VARYING   IDX   FROM   1   BY   1
                             UNTIL   (IDX       >    5)   OR
                                     (FLG-WRKRECE65   =   1)
                       ADD         1             TO    IDX-RECE65
                       IF  WRK-RECE65XX-SRYCD(IDX-RECE65 1) = SPACE
                           MOVE    1             TO    FLG-WRKRECE65
                       ELSE
                           MOVE  WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
                                            TO   RECE651-SRYACT-TBL(IDX)
                           ADD   1          TO   WRK-CNT-MEISAI
                       END-IF  
                    END-PERFORM
                    IF   RECE651-SRYCD(1)  NOT =  SPACE
                       WRITE   RECE651-REC
                       ADD     1            TO   CNT-RECE65
                    END-IF
           END-PERFORM
      *
           MOVE    RECE649-REC          TO   RECE641-REC
           MOVE    WRK-CNT-MEISAI       TO   RECE641-MEISAISU
           IF   WRK-JIDO-SETCOMFLG   =  "1"
             MOVE  WRK-JIDO-SETCOMFLG   TO   RECE641-SETCOMFLG
           END-IF
           WRITE   RECE641-REC
           ADD     1                    TO   CNT-RECE64
      *
           .
       250311-SRYACTINF2-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ôÃÍÈ¾³Ñ¢ªÁ´³ÑÊÑ´¹½èÍý
      *****************************************************************
       2503111-NUM-ZENKAKU-HENKAN-SEC    SECTION.
      *
           MOVE     1       TO   WRK-CHAR-IDX
           MOVE    ZERO     TO   WRK-CHAR-N-CNT
      *
           PERFORM   VARYING   IDX20   FROM    1   BY  1
                      UNTIL    IDX20     >     3
              IF  WRK-CHAR-X(IDX20:1)  IS   NUMERIC
                 MOVE   WRK-CHAR-X(IDX20:1)
                                    TO  IDX21
                 MOVE   TBL-DAY(IDX21)(3:2)
                                    TO  WRK-CHAR-N(WRK-CHAR-IDX:)
                 ADD    2           TO  WRK-CHAR-IDX
                                        WRK-CHAR-N-CNT
              END-IF
           END-PERFORM
      *
           .
       2503111-NUM-ZENKAKU-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£¶£´£¹Å¾Á÷½èÍý
      *****************************************************************
       2599-RECE649-TENSOU-SEC     SECTION.
      *
           MOVE       RECE649-REC    TO   RECE641-REC
           WRITE      RECE641-REC
           ADD        1              TO   CNT-RECE64
           PERFORM    25991-RECE659-TENSOU-SEC
      *
           .
       2599-RECE649-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£¶£µ£¹Å¾Á÷½èÍý
      *****************************************************************
       25991-RECE659-TENSOU-SEC     SECTION.
      *
           PERFORM  250111-RECE659-TAIHI-SEC
      *
      *    £Ò£Å£Ã£Å£¶£µ£¹¥Õ¥¡¥¤¥ë¤ÎÆâÍÆ¤ò¥Æ¡¼¥Ö¥ë¤ËÂàÈò
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
      *
           PERFORM
               UNTIL  ( FLG-RECE659           =   1                    )
               OR     ( RECE649-HOSPNUM   NOT =   RECE659-HOSPNUM      )
               OR     ( RECE649-SRYYM     NOT =   RECE659-SRYYM        )
               OR     ( RECE649-NYUGAIKBN NOT =   RECE659-NYUGAIKBN    )
               OR     ( RECE649-PTID      NOT =   RECE659-PTID         )
               OR     ( RECE649-RECEKA    NOT =   RECE659-RECEKA       )
               OR     ( RECE649-TEISYUTUSAKI
                                          NOT =   RECE659-TEISYUTUSAKI )
               OR     ( RECE649-RECESYUBETU
                                          NOT =   RECE659-RECESYUBETU  )
               OR     ( RECE649-HKNJANUM  NOT =   RECE659-HKNJANUM     )
               OR     ( RECE649-HOJOKBN   NOT =   RECE659-HOJOKBN      )
               OR     ( RECE649-TEKSTYMD  NOT =   RECE659-TEKSTYMD     )
               OR     ( RECE649-ZAINUM    NOT =   RECE659-ZAINUM       )
               OR     ( RECE649-ZAIREN    NOT =   RECE659-ZAIREN       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   5     )
                         OR      ( RECE659-SRYCD(IDX) =   SPACE )
                    ADD    1             TO      IDX-RECE65
                    MOVE   RECE659-SRYACT-TBL(IDX)
                             TO   WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
               END-PERFORM
      *
               PERFORM 990-RECE659-NEXT-SEC
      *
           END-PERFORM
      *
      *    ¥³¥á¥ó¥È¤ò´Þ¤ó¤ÀÆâÍÆ¤ÇÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºÆºîÀ®
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
                    INITIALIZE                        RECE651-REC
                    MOVE    WRK-RECE65-KEY       TO   RECE651-KEY
                    ADD     1                    TO   WRK-RENNUM
                    MOVE    WRK-RENNUM           TO   RECE651-RENNUM
                    MOVE    WRK-RECE65-KOHBUNRI  TO   RECE651-KOHBUNRI
                    MOVE    WRK-RECE65-PTNUM     TO   RECE651-PTNUM
                    MOVE    WRK-RECE65-SRYKA     TO   RECE651-SRYKA
                    MOVE    WRK-RECE65-SRYKBN    TO   RECE651-SRYKBN
                    MOVE    WRK-RECE65-SRYSYUKBN TO   RECE651-SRYSYUKBN
                    MOVE    WRK-RECE65-JIHIMONEY TO   RECE651-JIHIMONEY
                    PERFORM  VARYING   IDX   FROM   1   BY   1
                             UNTIL   (IDX       >    5)   OR
                                     (FLG-WRKRECE65   =   1)
                       ADD         1             TO    IDX-RECE65
                       IF  WRK-RECE65XX-SRYCD(IDX-RECE65 1) = SPACE
                           MOVE    1             TO    FLG-WRKRECE65
                       ELSE
                           MOVE  WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
                                            TO   RECE651-SRYACT-TBL(IDX)
                       END-IF
                    END-PERFORM
                    IF   RECE651-SRYCD(1)  NOT =  SPACE
                       WRITE   RECE651-REC
                       ADD     1            TO   CNT-RECE65
                    END-IF
           END-PERFORM
      *
           .
       25991-RECE659-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´Ø·¸¥³¥á¥ó¥ÈÊÔ½¸
      *****************************************************************
       260-SETCOM-HENSYU-SEC     SECTION.
      *
      *    ¥Ñ¥é¥á¡¼¥¿£¶£´£±¥Õ¥¡¥¤¥ë¤è¤êÊÔ½¸
           INITIALIZE                      RECE641-REC
           MOVE    RECE62-HOSPNUM      TO  RECE641-HOSPNUM
           MOVE    RECE62-SRYYM        TO  RECE641-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE641-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE641-PTID
           MOVE    RECE62-RECEKA       TO  RECE641-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE641-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE641-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE641-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE641-HOJOKBN
           MOVE    RECE62-TEKSTYMD     TO  RECE641-TEKSTYMD
           MOVE    ZERO                TO  RECE641-ZAINUM
      *
           START   RECE641-FILE
               KEY IS    >=   RECE641-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE641
               NOT INVALID
                   PERFORM 990-RECE641-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE641          =   1                    )
               OR     ( RECE62-HOSPNUM   NOT =   RECE641-HOSPNUM      )
               OR     ( RECE62-SRYYM     NOT =   RECE641-SRYYM        )
               OR     ( RECE62-NYUGAIKBN NOT =   RECE641-NYUGAIKBN    )
               OR     ( RECE62-PTID      NOT =   RECE641-PTID         )
               OR     ( RECE62-RECEKA    NOT =   RECE641-RECEKA       )
               OR     ( RECE62-TEISYUTUSAKI
                                         NOT =   RECE641-TEISYUTUSAKI )
               OR     ( RECE62-RECESYUBETU
                                         NOT =   RECE641-RECESYUBETU  )
               OR     ( RECE62-HKNJANUM-KEY
                                         NOT =   RECE641-HKNJANUM     )
               OR     ( RECE62-HOJOKBN-KEY
                                         NOT =   RECE641-HOJOKBN      )
               OR     ( RECE62-TEKSTYMD  NOT =   RECE641-TEKSTYMD     )
      *
               IF     RECE641-SETCOMFLG   =    "1"
                  PERFORM 2601-SETCOM-HENSYU2-SEC
                  IF  RECE641-SRYYM   >=  "200804"
                    CONTINUE
                  ELSE
                    MOVE   SPACE   TO   RECE641-SETCOMFLG
                    REWRITE   RECE641-REC
                    ADD     1      TO   CNT-RECE64-RE
                  END-IF
               END-IF
      *
               PERFORM 990-RECE641-NEXT-SEC
      *
           END-PERFORM
      *
           .
       260-SETCOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´Ø·¸¥³¥á¥ó¥ÈÊÔ½¸£²
      *****************************************************************
       2601-SETCOM-HENSYU2-SEC     SECTION.
      *
           MOVE    ZERO    TO    FLG-SETCOM
      *
           MOVE    ZERO    TO    WRK-HEN-RENNUM-X
                                 WRK-HEN-SHORI-RENNUM-X
                                 WRK-HEN-SEQNO-X
           MOVE    ZERO    TO    WRK-HEN-RENNUM-J
                                 WRK-HEN-SHORI-RENNUM-J
                                 WRK-HEN-SEQNO-J
           MOVE    ZERO    TO    WRK-HEN-RENNUM-J2
                                 WRK-HEN-SHORI-RENNUM-J2
                                 WRK-HEN-SEQNO-J2
      *
           INITIALIZE                       RECE651-REC
           MOVE    RECE641-HOSPNUM      TO  RECE651-HOSPNUM
           MOVE    RECE641-SRYYM        TO  RECE651-SRYYM
           MOVE    RECE641-NYUGAIKBN    TO  RECE651-NYUGAIKBN
           MOVE    RECE641-PTID         TO  RECE651-PTID
           MOVE    RECE641-RECEKA       TO  RECE651-RECEKA
           MOVE    RECE641-TEISYUTUSAKI TO  RECE651-TEISYUTUSAKI
           MOVE    RECE641-RECESYUBETU  TO  RECE651-RECESYUBETU
           MOVE    RECE641-HKNJANUM     TO  RECE651-HKNJANUM
           MOVE    RECE641-HOJOKBN      TO  RECE651-HOJOKBN
           MOVE    RECE641-TEKSTYMD     TO  RECE651-TEKSTYMD
           MOVE    RECE641-ZAINUM       TO  RECE651-ZAINUM
           MOVE    RECE641-ZAIREN       TO  RECE651-ZAIREN
           START   RECE651-FILE
               KEY IS    >=   RECE651-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE651
               NOT INVALID
                   PERFORM 990-RECE651-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE651           =   1                    )
               OR     ( RECE641-HOSPNUM   NOT =   RECE651-HOSPNUM      )
               OR     ( RECE641-SRYYM     NOT =   RECE651-SRYYM        )
               OR     ( RECE641-NYUGAIKBN NOT =   RECE651-NYUGAIKBN    )
               OR     ( RECE641-PTID      NOT =   RECE651-PTID         )
               OR     ( RECE641-RECEKA    NOT =   RECE651-RECEKA       )
               OR     ( RECE641-TEISYUTUSAKI
                                          NOT =   RECE651-TEISYUTUSAKI )
               OR     ( RECE641-RECESYUBETU
                                          NOT =   RECE651-RECESYUBETU  )
               OR     ( RECE641-HKNJANUM  NOT =   RECE651-HKNJANUM     )
               OR     ( RECE641-HOJOKBN   NOT =   RECE651-HOJOKBN      )
               OR     ( RECE641-TEKSTYMD  NOT =   RECE651-TEKSTYMD     )
               OR     ( RECE641-ZAINUM    NOT =   RECE651-ZAINUM       )
               OR     ( RECE641-ZAIREN    NOT =   RECE651-ZAIREN       )
      *
               MOVE      ZERO      TO    FLG-MEISKY-OKIKAE
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   5     )
                         OR      ( RECE651-SRYCD(IDX) =   SPACE )
                 IF   RECE651-MEISKYFLG(IDX)  =  "2"
                   IF      RECE651-SRYYM  >=  "200804"
                    IF     RECE651-SETCOMATFLG(IDX) = ZERO
                      IF   (FLG-SETCOM              =  1   )
                      AND  (RECE651-RECEDENFLG(IDX) = SPACE)
                        ADD    1                 TO  WRK-HEN-SEQNO-X
                        IF   WRK-HEN-SEQNO-X  >  3
                          MOVE   SPACE   TO  RECE651-MEISKYFLG(IDX)
                          MOVE   1       TO  FLG-MEISKY-OKIKAE
                        ELSE
                          MOVE   WRK-HEN-RENNUM-X  TO  WRK-HEN-RENNUM
                          MOVE   WRK-HEN-SHORI-RENNUM-X
                                         TO  WRK-HEN-SHORI-RENNUM
                          MOVE   WRK-HEN-SEQNO-X   TO  WRK-HEN-SEQNO
                          PERFORM  26011-SETCOM-HENSYU3-SEC
                        END-IF
                        IF  (WRK-HEN-RENNUM-X = WRK-HEN-RENNUM-J )
                        AND (WRK-HEN-SHORI-RENNUM-X
                                        = WRK-HEN-SHORI-RENNUM-J )
                          ADD  1                 TO  WRK-HEN-SEQNO-J
                        END-IF
                        IF  (WRK-HEN-RENNUM-X = WRK-HEN-RENNUM-J2)
                        AND (WRK-HEN-SHORI-RENNUM-X
                                        = WRK-HEN-SHORI-RENNUM-J2)
                          ADD  1                 TO  WRK-HEN-SEQNO-J2
                        END-IF
                      END-IF
                    ELSE
                      EVALUATE   RECE651-SETCOMATFLG(IDX)
                        WHEN    1
                        WHEN    3  THRU   16
                        WHEN   18
                        WHEN   20
                        WHEN  101  THRU  106
                          ADD  1                 TO  WRK-HEN-SEQNO-J
                          IF   WRK-HEN-SEQNO-J  >  3
                            MOVE   SPACE   TO  RECE651-MEISKYFLG(IDX)
                            MOVE   1       TO  FLG-MEISKY-OKIKAE
                          ELSE
                            MOVE WRK-HEN-RENNUM-J  TO  WRK-HEN-RENNUM
                            MOVE WRK-HEN-SHORI-RENNUM-J
                                         TO  WRK-HEN-SHORI-RENNUM
                            MOVE WRK-HEN-SEQNO-J   TO  WRK-HEN-SEQNO
                            PERFORM  26011-SETCOM-HENSYU3-SEC
                          END-IF
                          IF  (WRK-HEN-RENNUM-X = WRK-HEN-RENNUM-J )
                          AND (WRK-HEN-SHORI-RENNUM-X
                                          = WRK-HEN-SHORI-RENNUM-J )
                            ADD    1             TO  WRK-HEN-SEQNO-X
                          END-IF
                        WHEN    2
                          ADD  1                 TO  WRK-HEN-SEQNO-J2
                          IF   WRK-HEN-SEQNO-J2 >  3
                            MOVE   SPACE   TO  RECE651-MEISKYFLG(IDX)
                            MOVE   1       TO  FLG-MEISKY-OKIKAE
                          ELSE
                            MOVE WRK-HEN-RENNUM-J2 TO  WRK-HEN-RENNUM
                            MOVE WRK-HEN-SHORI-RENNUM-J2
                                         TO  WRK-HEN-SHORI-RENNUM
                            MOVE WRK-HEN-SEQNO-J2  TO  WRK-HEN-SEQNO
                            PERFORM  26011-SETCOM-HENSYU3-SEC
                          END-IF
                          IF  (WRK-HEN-RENNUM-X = WRK-HEN-RENNUM-J2)
                          AND (WRK-HEN-SHORI-RENNUM-X
                                          = WRK-HEN-SHORI-RENNUM-J2)
                            ADD    1             TO  WRK-HEN-SEQNO-X
                          END-IF
                      END-EVALUATE
                    END-IF
                   ELSE
                    MOVE   SPACE   TO  RECE651-MEISKYFLG(IDX)
                    MOVE   1       TO  FLG-MEISKY-OKIKAE
                   END-IF
                 ELSE
                    IF   (RECE651-SRYCD(IDX)(1:1) = "1" OR "6" OR "7")
                    OR   (RECE651-SRYCD(IDX)(1:3) = "059"            )
                      MOVE   1               TO  FLG-SETCOM
                      MOVE   RECE651-RENNUM  TO  WRK-HEN-RENNUM-X
                      MOVE   IDX             TO  WRK-HEN-SHORI-RENNUM-X
                      MOVE   ZERO            TO  WRK-HEN-SEQNO-X
                    END-IF
      *
                    IF    RECE651-SRYCD(IDX) =
      *1
                          "113000410" OR "113000510" OR "180000110" OR
                          "180000210" OR
      *3
                          "113001210" OR "113001310" OR "113002110" OR
      *4
                          "111011810" OR
      *6
                          "180000710" OR
      *7
                          "113004310" OR
      *11
                          "114008010" OR
      *12
                          "114009410" OR
      *16
                          "180027410" OR "180027510" OR "180028010" OR
                          "180028110" OR "180027610" OR "180027710" OR
                          "180030810" OR "180027810" OR "180027910" OR
                          "180028210" OR "180028310" OR "180028410" OR
                          "180032410" OR "180032510" OR "180032610" OR
                          "180032710" OR "180033110" OR
                          "180033910" OR "180034010" OR "180034110" OR
                          "180034210" OR "180034310" OR "180034410" OR
                          "180034510" OR "180034610" OR "180034710" OR
      *18
                          "113003410" OR
      *20
                          "700010000" OR "700030000" OR "700050000" OR
                          "700080000" OR "700110000" OR "700140000" OR
                          "700290000" OR "700310000" OR "700330000" OR
                          "700710000" OR "700730000" OR "700750000" OR
                          "700780000" OR "700810000" OR "700840000" OR
                          "700870000" OR "700160000" OR "700180000" OR
                          "700190000" OR "700200000" OR "700210000" OR
                          "700220000" OR "700230000" OR "700240000" OR
                          "700250000" OR "700260000" OR "700270000" OR
                          "700280000" OR "700590000" OR "700600000" OR
                          "700630000" OR "700640000" OR "700650000" OR
                          "700680000" OR "729780000" OR
      *101
                          "113010710" OR "113010810" OR "120001610" OR
      *102
                          "113003310" OR
      *103
                          "190030310" OR
      *104
                          "113006270" OR
      *105
                          "180018310" OR
      *106
                          "113011710"
      *
                       MOVE   RECE651-RENNUM  TO  WRK-HEN-RENNUM-J
                       MOVE   IDX       TO  WRK-HEN-SHORI-RENNUM-J
                       MOVE   ZERO            TO  WRK-HEN-SEQNO-J
                    END-IF
      *
                    IF    RECE651-SRYCD(IDX) =
      *2
                          "113000670"
      *
                       MOVE   RECE651-RENNUM  TO  WRK-HEN-RENNUM-J2
                       MOVE   IDX       TO  WRK-HEN-SHORI-RENNUM-J2
                       MOVE   ZERO            TO  WRK-HEN-SEQNO-J2
                    END-IF
                 END-IF
               END-PERFORM
      *
               IF    FLG-MEISKY-OKIKAE  =  1
                  REWRITE   RECE651-REC
                  ADD       1        TO   CNT-RECE65-RE
               END-IF
      *
               PERFORM 990-RECE651-NEXT-SEC
      *
           END-PERFORM
      *
           .
       2601-SETCOM-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´Ø·¸¥³¥á¥ó¥ÈÊÔ½¸£³
      *****************************************************************
       26011-SETCOM-HENSYU3-SEC     SECTION.
      *
           MOVE    SPACE                 TO  RECE655-REC
           INITIALIZE                        RECE655-REC
      *
           MOVE    RECE651-KEY           TO  RECE655-KEY
      *
           MOVE    WRK-HEN-RENNUM        TO  RECE655-RENNUM
           MOVE    WRK-HEN-SHORI-RENNUM  TO  RECE655-SHORI-RENNUM
           MOVE    WRK-HEN-SEQNO         TO  RECE655-SEQNO
      *
           MOVE    RECE651-SRYCD(IDX)    TO  RECE655-SRYCD
           MOVE    RECE651-NAME(IDX)     TO  RECE655-NAME
           MOVE    RECE651-INPUTCOMENT(IDX)
                                         TO  RECE655-INPUTCOMENT
      *
           WRITE   RECE655-REC
           ADD     1                     TO  CNT-RECE655
      *
           .
       26011-SETCOM-HENSYU3-EXT.
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
      *            ¥¸¥ç¥Ö´ÉÍý½ªÎ»½èÍý
                   MOVE    "JBE"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "9999"          TO  JOB-ERRCD
                   PERFORM   900-CALL-ORCSJOB-SEC
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
               PERFORM   900-CALL-ORCSJOB-SEC
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
                       PERFORM   900-CALL-ORCSJOB-SEC
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
      *    ½ªÎ»¡¡¡¡½èÍý
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ¥Õ¥¡¥¤¥ë£Ã£Ì£Ï£Ó£Å½èÍý
           PERFORM 310-FILE-CLOSE-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ¥¹¥Æ¥Ã¥×´ÉÍý½ªÎ»½èÍý
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               PERFORM   900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE62  CNT-IN  :" CNT-RECE62 
           DISPLAY "RECE649 CNT-WK  :" CNT-RECE649
           DISPLAY "RECE659 CNT-WK  :" CNT-RECE659
           DISPLAY "RECE64  CNT-OUT :" CNT-RECE64 
           DISPLAY "RECE65  CNT-OUT :" CNT-RECE65 
           DISPLAY "RECE64  REWRITE :" CNT-RECE64-RE
           DISPLAY "RECE65  REWRITE :" CNT-RECE65-RE
           DISPLAY "RECE655 CNT-OUT :" CNT-RECE655
           DISPLAY "*** ORCR0660 END ***"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë£Ã£Ì£Ï£Ó£Å½èÍý
      *****************************************************************
       310-FILE-CLOSE-SEC              SECTION.
      *
           CLOSE                       RECE62-FILE
      *
           CLOSE                       RECE64-FILE
      *
           CLOSE                       RECE65-FILE
      *
           CLOSE                       RECE641-FILE     
           CLOSE                       RECE651-FILE     
      *
           CLOSE                       RECE649-FILE
           CLOSE                       RECE659-FILE
      *
           CLOSE                       RECE655-FILE
      *
           .
       310-FILE-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È£°£²¥Ç¡¼¥¿½ç¸¡º÷½èÍý
      *****************************************************************
       990-RECE62-READ-SEC           SECTION.
      *
           READ    RECE62-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-END
               NOT AT  END
                   ADD     1           TO  CNT-RECE62
                   ADD     1           TO  CNT-IN-COMMIT
                                           CNT-IN-CANCEL
      *
                   DISPLAY "RECE62  PTNUM=" RECE62-PTNUM
      *
           END-READ
      *
           .
       990-RECE62-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£°£´¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE64-NEXT-SEC           SECTION.
      *
           READ    RECE64-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE64
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE64
           END-READ
      *
           .
       990-RECE64-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£¶£´£¹¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE649-NEXT-SEC           SECTION.
      *
           READ    RECE649-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE649
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE649
           END-READ
      *
           .
       990-RECE649-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£¶£´£±¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE641-NEXT-SEC           SECTION.
      *
           READ    RECE641-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE641
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE641
           END-READ
      *
           .
       990-RECE641-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£°£µ¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE65-NEXT-SEC           SECTION.
      *
           READ    RECE65-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE65
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE65
           END-READ
      *
           .
       990-RECE65-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£¶£µ£¹¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE659-NEXT-SEC           SECTION.
      *
           READ    RECE659-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE659
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE659
           END-READ
      *
           .
       990-RECE659-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    ¥ì¥»¥×¥È£¶£µ£±¥Ç¡¼¥¿ÆÉ¤ß
      ****************************************************************
       990-RECE651-NEXT-SEC           SECTION.
      *
           READ    RECE651-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE651
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE651
           END-READ
      *
           .
       990-RECE651-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°ÙÆÉ¹þ
      *****************************************************************
       900-SRYACT-READ-SEC           SECTION.
      *
           PERFORM   900-DBFETCH-SEC
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
      *    »»ÄêÍúÎòÉÕ²ÃÆÉ¹þ
      *****************************************************************
       900-SANTEIPLUS-READ-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SANTEIPLUS-REC
               MOVE    ZERO            TO  FLG-SANTEIPLUS
           ELSE
               INITIALIZE                  SANTEIPLUS-REC
               MOVE    1               TO  FLG-SANTEIPLUS
           END-IF
      *
           .
       900-SANTEIPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    »»ÄêÎò¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       900-SANTEI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEI-REC
               MOVE    ZERO                TO  FLG-SANTEI
           ELSE
               INITIALIZE                      SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           .
       900-SANTEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡ÍúÎò¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       900-PTNYUINRRK-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTNYUINRRK-REC
               MOVE    ZERO                TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                      PTNYUINRRK-REC
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       900-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¥³¥á¥ó¥È¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       900-PTCOM-READ-SEC           SECTION.
      *
           MOVE     PTCOM-REC          TO      MCPDATA-REC
           MOVE    "tbl_ptcom"         TO      MCP-TABLE
           MOVE    "key"               TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom"       TO      MCP-TABLE
               MOVE    "key"             TO      MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTCOM-REC
                   MOVE    ZERO                TO  FLG-PTCOM
               ELSE
                   MOVE    1                   TO  FLG-PTCOM
                   INITIALIZE                      PTCOM-REC
               END-IF
           ELSE
               MOVE    1                       TO  FLG-PTCOM
               INITIALIZE                          PTCOM-REC
           END-IF
      *
           MOVE    "tbl_ptcom"         TO      MCP-TABLE
           MOVE    "key"               TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       900-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Æ¡¼¥Ö¥ë¤Î¸¡º÷
      *****************************************************************
       900-TENSU-TBL-SEC               SECTION.
      *
           INITIALIZE                       TNS-TENSU-REC
           MOVE    SRY-SRYCD(IDX12) TO      TNS-SRYCD
           MOVE    SRY-SRYYM        TO      TNS-YUKOSTYMD(1:6)
           MOVE    "01"             TO      TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD    TO      TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM      TO      TNS-HOSPNUM
           MOVE    TNS-TENSU-REC    TO      MCPDATA-REC
           MOVE    "tbl_tensu"      TO      MCP-TABLE
           MOVE    "key"            TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_tensu"      TO      MCP-TABLE
               MOVE    "key"            TO      MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
                   MOVE    ZERO            TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1               TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                      TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           INITIALIZE                     MCPAREA
           MOVE    "tbl_tensu"      TO      MCP-TABLE
           MOVE    "key"            TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       900-TENSU-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
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
      *
      *****************************************************************
      *    ¥¸¥ç¥Ö´ÉÍý£Ä£ÂÀ©¸æ½èÍý
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
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
           PERFORM 900-ORCDBMAIN-SEC
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
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¥¢¥¯¥»¥¹½èÍý
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *      
      *****************************************************************
      *    £Ä£Â¡¡¥ª¡¼¥×¥ó½èÍý
      *****************************************************************
       120-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM   900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM   900-ORCDBMAIN-SEC
           .
       120-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¡¡¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM   900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM   900-ORCDBMAIN-SEC
      *
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
           PERFORM   900-ORCDBMAIN-SEC
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
           PERFORM   900-ORCDBMAIN-SEC
           .
       900-DBSTART-EXT.
           EXIT.
