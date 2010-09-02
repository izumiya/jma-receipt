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
       PROGRAM-ID.             ORCR0840.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¥ì¥»¥×¥ÈÆþ±¡¡ÊÏ«ºÒ¡¦¼«ÇåÀÕ¡Ë
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¿ÇÎÅ¶èÊ¬ÊÌÅÀ¿ô½¸·×¡ÊÏ«ºÒ¡¦¼«ÇåÀÕ¡Ë
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  03/06/01    NaCl¡ÝÌçÏÆ    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01   NACL-ÌçÏÆ     04/01/06  ¿ÇÎÅ¹Ô°Ù¥³¡¼¥É¤Î£±·åÌÜ¤ÎÈ½Äê
      *                                     ½¤Àµ
      *  01.00.02    NACL-ÌçÏÆ    04.01.21  Ï«ºÒ¤ÎÆÃÄê´ïºà(09593)ÂÐ±þ
      *  01.00.03    NACL-ÌçÏÆ    04.01.21  Ï«ºÒ¡¦¼«ÇåÀÕ¤ÎÆþ±¡¼¼ÎÁ²Ã»»
      *                                     (09594)ÂÐ±þ
      *  01.00.04    NACL-ÌçÏÆ    04/01/28  ¿ÇÎÅ¶èÊ¬£¹£°¡¢£¹£²¤Î¥³¥á¥ó¥È
      *                                     ¤Î¤ß¤ÎºÞ¤â¥ì¥»µ­ºÜ¤ÎÂÐ¾Ý¤Ë¤¹¤ë
      *  01.00.05    NACL-ÌçÏÆ    04/02/05  ¿ÇÎÅ¥³¡¼¥É¡ö¿ôÎÌ¤ÎÎß·×ÊÔ½¸
      *  01.00.06    NACL-ÌçÏÆ    04/02/06  ¼«ÇåÀÕ¤Î¿©»öÎÅÍÜÈñ¤òÏ«ºÒ
      *                                     ¤Î£±¡¥£²ÇÜ¤Ç·×»»¤¹¤ë
      *  £²£°£°£´Ç¯£´·î²þÀµÂÐ±þ
      *  01.01.01    NACL-ÌçÏÆ    04/04/01  êóáì´µ¼Ô´ÉÍý²Ã»»¤ÎÂÐ±þ
      *  01.01.02    NACL-ÌçÏÆ    04/04/01  ¥ê¥Ï¸À¸ìÄ°³ÐÎÅË¡¡Ê£³¡ËÄÉ²ÃÂÐ±þ
      *  01.01.03    NACL-ÌçÏÆ    04/04/05  ºßÂð·ì±ÕÆ©ÀÏÉÑ²ó»ØÆ³´ÉÍýÎÁ¤Î
      *                                     ¥³¥á¥ó¥ÈÂÐ±þ
      *  01.01.04    NACL-ÌçÏÆ    04/04/26  ¥æ¡¼¥¶¡¼¥³¥á¥ó¥ÈÄÉ²ÃÂÐ±þ
      *  01.01.05    NACL-ÌçÏÆ    04/05/20  ÅÀÅ©¤Î´ïºàÃ±²ÁÊÔ½¸
      *  01.01.06    NACL-¾®Æ¦Âô  04/06/08  £Ã£Ô¡¦£Í£Ò£É¡Ê£²²óÌÜ°Ê¹ß¡Ë¤Î
      *                                     Å¦Í×Íóµ­ºÜ½ç½¤Àµ
      *  01.01.07    NACL-ÌçÏÆ    04/07/28  ÅêÌôÎÁ¤Î´ïºàÅÀ¤Î½¸·×½¤Àµ
      *  01.01.08    NACL-ÌçÏÆ    04/10/04  ÅÀÅ©¤Î¿ÇÎÅ¥³¡¼¥É¡ö¿ôÎÌ¤ÎÎß·×ÊÔ½¸
      *  01.01.09    NACL-ÌçÏÆ    04/10/15  ¼«ÇåÀÕÆÃÄê´ïºàÅù¾ÃÈñÀÇÂÐ±þ
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
      *  01.02.09    NACL-ÌçÏÆ    05/03/03  ¿©»ö¤Ë´Ø¤¹¤ëÊÔ½¸½¤Àµ
      *  01.02.10    NACL-ÌçÏÆ    05/05/16  ÎÅÍÜÃ´Åö¼êÅö¡ÊËÌ³¤Æ»¡ËÂÐ±þ
      *  01.02.11    NACL-ÌçÏÆ    05/05/19  ÅÀ¿ô¥Þ¥¹¥¿¤ÎÃ±°ÌÊ¸»ú¿ô¤¬¥¼¥í
      *                                     ¤Î¾ì¹ç¤ÎÊÔ½¸
      *  01.02.12    NACL-¾®Æ¦Âô  05/05/20  Æþ±¡ÅÀÅ©¼ïÊÌÄÉ²Ã¤ËÈ¼¤¦½¤Àµ
      *  01.02.13    NACL-ÌçÏÆ    05/07/15  ¼«ÇåÀÕ½¾ÍèÍÍ¼°ÂÐ±þ
      *  01.02.14    NACL-ÌçÏÆ    05/08/22  ¥ê¥Ï¥Ó¥ê¼Â»Ü²ó¿ôº¹¤·¹þ¤ß½¤ÀµÂÐ±þ
      *  01.02.15    NACL-Æ£¸¶    05/08/22  ¸¡ÂÎ¸¡ºº¥³¥á¥ó¥È¤ÎÄÉ²Ã
      *  01.02.16    NACL-ÌçÏÆ    05/09/13  Æþ±¡ÅÀÅ©¥³¥á¥ó¥È¤Ë´Ø¤¹¤ë½¤Àµ
      *  01.02.17    NACL-ÌçÏÆ    05/08/29  £°£¹£µ£¹£³¤Î¥³¥á¥ó¥ÈÂÐ±þ
      *  01.02.18    NACL-ÌçÏÆ    05/10/06  ¿Ç¶è£¹£µ£¹£¶ÂÐ±þ
      *  01.02.19    NACL-¾®Æ¦Âô  05/10/28  ÆÃµ­»ö¹à¡¡Àè¿Ê¡¢À©Ä¶¡¡ÂÐ±þ
      *  01.02.20    NACL-ÌçÏÆ    05/11/01  ¡Ê¸¡Áª¡ËÅù¥³¥á¥ó¥ÈÂÐ±þ
      *  01.02.21    NACL-ÌçÏÆ    05/11/11  ¥³¥á¥ó¥È¤¬¤¢¤ë¾ì¹ç¤ÎÊÔ½¸½¤Àµ
      *  01.02.22    NACL-ÌçÏÆ    05/11/15  ½ýÉÂ¼êÅö¶â¡¢ÎÅÍÜÈñÆ±°Õ½ñ¤Î¼«Æ°
      *                                     µ­ºÜ¥Õ¥é¥°ÊÔ½¸½¤Àµ
      *  01.02.23    NACL-ÌçÏÆ    05/12/14  ½é¿Ç»þ´Ö¶èÊ¬ÊÔ½¸½¤Àµ
      *  01.02.24    NACL-ÌçÏÆ    06/01/05  ¥·¥¹´É¡Ê£µ£°£°£°¡Ë¤Î¿©»ö¾ðÊó¼è
      *                                     ÆÀ½¤Àµ
      *  01.02.25    NACL-ÌçÏÆ    06/02/01  £°£¹£¹£²£°£°£°¤Ç»Ï¤Þ¤ëÅêÌôÍÑ
      *                                     ¥³¥á¥ó¥È¤ÎÊÔ½¸ÂÐ±þ
      *  01.02.26    NACL-ÌçÏÆ    06/02/07  ¿©»ö´ðËÜÎÁ¾ðÊó¡Ê¤½¤ÎÂ¾¡ËÊÔ½¸
      *
      *  £²£°£°£¶Ç¯£´·î²þÀµÂÐ±þ
      *  02.09.01    NACL-ÌçÏÆ    06/04/05  Îß·×¥È¡¼¥¿¥ëÊÔ½¸½¤Àµ
      *  02.09.02    NACL-ÌçÏÆ    06/04/13  £²£°£°£¶Ç¯£´·î²þÀµÂÐ±þ
      *  02.09.03    NACL-Æ£¸¶    06/04/14  ¤­¤¶¤ßÃÍ·×»»¼±ÊÌ¤ÎÄÉ²Ã
      *  02.09.04    NACL-ÌçÏÆ    06/04/20  ³°Íè¿×Â®¸¡ÂÎ¸¡ºº²Ã»»ÊÔ½¸
      *  02.09.05    NACL-ÌçÏÆ    06/05/09  Îß·×¥È¡¼¥¿¥ëÊÔ½¸½¤Àµ
      *  02.09.06    NACL-ÌçÏÆ    06/06/02  êóáì´µ¼Ô´ÉÍý²Ã»»ÊÔ½¸ÂÐ±þ
      *  02.09.07    NACL-ÌçÏÆ    06/06/05  Ê£¿ô°å»ÕµÚ¤Ó´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»¤È
      *                                     ´Ç¸îÇÛÃÖÅ¬¹ç²Ã»»¤ÎÊÔ½¸ÂÐ±þ
      *  02.09.08    NACL-ÌçÏÆ    06/06/09  Ï«ºÒ¡Ê²Ã»»¤Ê¤·¡Ë¤òºÞ¤Ë´Þ¤à
      *                                     ¾ì¹ç¤ÎÊÔ½¸½¤Àµ¡Ê¼«ÇåÀÕ¡Ë
      *  03.00.01    NACL-ÌçÏÆ    06/06/22  ±ý¿Ç¡¢ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ¤Î
      *                                     ¥³¥á¥ó¥È¼«Æ°µ­ºÜÂÐ±þ
      *  03.00.02    NACL-ÌçÏÆ    06/06/23  Ç§ÃÎµ¡Ç½¾ã³²²Ã»»¤ÎÊÔ½¸ÂÐ±þ
      *  03.01.01    NACL-¾®Æ¦Âô  06.07.04  Æþ±¡ÎÁ¤Ë´Þ¤á¤ë²Ã»»¤¬¥³¥á¥ó¥È
      *                                     ¤ÈÆ±»þÆþÎÏ¤µ¤ì¤¿¾ì¹ç¤ÎÂÐ±þ
      *  03.01.02    NACL-ÌçÏÆ    06/07/10  ºß»Ù±ç¥³¥á¥ó¥È¼«Æ°µ­ºÜÂÐ±þ
      *  03.01.03    NACL-ÌçÏÆ    06/07/14  ¿ÇÎÅ¶èÊ¬ÃÖ¤­´¹¤¨¥Õ¥é¥°¤ÎÊÔ½¸
      *  03.01.04    NACL-ÌçÏÆ    06/07/19  ¿©»ö¹àÌÜÊÔ½¸½¤Àµ
      *  03.02.01    NACL-ÌçÏÆ    06/08/10  ¹âÎð¼Ô¾å¸Â³Û·Ð²áÁ¼ÃÖÂÐ±þ
      *  03.02.02    NACL-ÌçÏÆ    06/09/08  ¥Õ¥¤¥ë¥à¤ÎÃ±²ÁÊÔ½¸½¤Àµ
      *  03.03.01    NACL-ÌçÏÆ    06/09/27  ÆÃµ­»ö¹à£±£¶Ä¹£²ÂÐ±þ
      *  03.03.02    NACL-ÌçÏÆ    06/10/11  Æþ±¡»þÀ¸³èÎÅÍÜÈñÂÐ±þ
      *  03.03.03    NACL-ÌçÏÆ    06/11/10  ±ý¿Ç¤Î¥³¥á¥ó¥È¼«Æ°µ­ºÜ¾ò·ï½¤Àµ
      *  03.04.01    NACL-ÌçÏÆ    06/12/01  ¼«ÇåÀÕ·òÊÝ½àµòÂÐ±þ
      *  03.04.02    NACL-ÌçÏÆ    07/03/12  Êñ³ç¿ÇÎÅÂÐ±þ
      *  03.04.03    NACL-ÌçÏÆ    07/04/01  ¸½ÊªµëÉÕÂÐ±þ
      *  03.04.04    NACL-ÌçÏÆ    07/04/13  ¥ê¥Ï¥Ó¥êÂÐ±þ
      *  03.04.05    NACL-ÌçÏÆ    07/05/11  Ï«ºÒ¡¦¼«ÇåÀÕÃ±²Á£±£°±ßÌ¤ËþÂÐ±þ
      *  03.04.06    NACL-ÌçÏÆ    07/05/15  ÅÀÅ©¤Îµ­ºÜÊÔ½¸½¤Àµ
      *  03.04.07    NACL-ÌçÏÆ    07/06/07  Æþ±¡¼¼ÎÁ²Ã»»¤Î¥³¥á¥ó¥ÈÂÐ±þ
      *
      *  03.05.01    NACL-ÌçÏÆ    07/06/01  ¥°¥ë¡¼¥×¿ÇÎÅÂÐ±þ
      *  03.05.02    NACL-Æ£¸¶    07/06/01  realtime preview ½èÍýÄÉ²Ã
      *  03.05.03    NACL-ÌçÏÆ    07/06/26  ¼«ÇåÀÕÊ¿À®£±£¹Ç¯£´·î²þÀµÍÍ¼°ÂÐ±þ
      *  03.05.04    NACL-ÌçÏÆ    07/07/17  ¸ÄÊÌ»þ¤Î¥·¥¹¥Æ¥à´ÉÍýÊÔ½¸½¤Àµ
      *  03.05.05    NACL-ÌçÏÆ    07/07/26  ¥ì¥»¤Ëµ­ºÜ¤·¤Ê¤¤¥³¥á¥ó¥ÈÂÐ±þ
      *  03.05.06    NACL-ÌçÏÆ    07/09/11  ÅÀÅ©¼êµ»½¸·×½¤Àµ
      *  03.05.07    NACL-ÌçÏÆ    07/09/18  ²èÁü¿ÇÃÇ¤Î²Ã»»¡Ê¥Ç¡¼¥¿¶èÊ¬¡§¼êµ»¡Ë
      *                                     ¤Îµ­ºÜ½çÊÑ¹¹ÂÐ±þ
      *  04.01.01    NACL-ÌçÏÆ    07/10/05  Æþ±¡ÅÀÅ©¥³¥á¥ó¥ÈÂÐ±þ
      *  04.01.02    NACL-ÌçÏÆ    07/10/24  ¼«ÇåÀÕÆÃÄê´ïºàÅù¾ÃÈñÀÇÂÐ¾Ý³°ÀßÄê
      *                                     ¹àÌÜ¿ô³ÈÄ¥ÂÐ±þ
      *  04.01.03    NACL-ÌçÏÆ    07/11/01  ¸øÌ³ºÒ³²ÂÐ±þ
      *  04.01.04    NACL-ÌçÏÆ    07/11/26  ÆÃÄê´ïºà¾¦ÉÊÌ¾µ­ºÜÂÐ±þ
      *  04.01.05    NACL-ÌçÏÆ    07/11/30  Â¾ÊÝ¸±¤Ë¤Æ¿Ç»¡ÎÁ»»ÄêºÑ¤ÎÌ¾¾ÎÃÖ´¹
      *  04.01.06    NACL-ÌçÏÆ    07/12/19  Æþ±¡ÎÁÌ¤»»ÄêÆüÂÐ±þ
      *  04.01.07    NACL-ÌçÏÆ    08/01/15  ¸ø³²ÂÐ±þ
      *  04.01.08    NACL-ÌçÏÆ    08/02/12  ½èÊýÎÁÅù¥«¥¦¥ó¥È½¤Àµ
      *  04.02.01    NACL-ÌçÏÆ    08/03/10  ¡û¡û¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ¤Î¥Õ¥é¥°ÊÔ½¸½¤Àµ
      *  04.02.02    NACL-ÌçÏÆ    08/03/24  Ê¿À®£²£°Ç¯£´·î²þÀµÂÐ±þ
      *                                     ¡ÊÆÃµ­»ö¹àÄÉ²ÃÂÐ±þ¡§£²£°¡Ë
      *                                     ¡ÊÌôºÞ´ÉÍý»ØÆ³ÎÁ¥³¡¼¥ÉÄÉ²Ã¡Ë
      *                                     ¡ÊÇ¾·ì´É¥ê¥Ï¡Ê£³¡Ë¥³¡¼¥ÉÄÉ²Ã¡Ë
      *                                     ¡Ê²ðÃ£¸£°úÈ¯¾ÉÆüµ­ºÜÇÑ»ß¡Ë
      *                                     ¡Ê½é¿Ç¡ÊÌë´Ö¡¦ÁáÄ«Åù¡Ë²Ã»»ÄÉ²Ã¡Ë
      *                                     ¡Ê¿ÇÎÅ¶èÊ¬£¶£´¡§ÉÂÍý¿ÇÃÇÂÐ±þ¡Ë
      *                                     ¡ÊºÆ½¢Ï«¡§±ßÊÑ¹¹ÂÐ±þ¡Ë
      *                                     ¡Ê¥ê¥Ï¡§ÀìÍÑ¥³¡¼¥ÉÄÉ²Ã¡Ë
      *                                     ¡Êºß»ÙÉÂ¼«Æ°µ­ºÜÂÐ±þ¡Ë
      *  04.02.03    NACL-ÌçÏÆ    08/04/03  ¡û¡û¤Ë¤ÆÆþ±¡ÎÁ»»ÄêºÑ¤Î¥Õ¥é¥°ÊÔ½¸½¤Àµ
      *  04.02.04    NACL-ÌçÏÆ    08/04/30  Æþ±¡ÎÁ¤Ë´Þ¤á¤ë²Ã»»ÂÐ±þ
      *  04.02.05    NACL-ÌçÏÆ    08/06/13  Âà±¡Á°Ë¬Ìä»ØÆ³ÎÁ¡¢Àº¿À²ÊÂà±¡Á°Ë¬Ìä
      *                                     »ØÆ³ÎÁ¤Î¥³¥á¥ó¥ÈÂÐ±þ
      *  04.02.06    NACL-ÌçÏÆ    08/06/20  ºßÂðÊÔ½¸½¤Àµ
      *
      *  04.03.01    NACL-ÌçÏÆ    08/07/08  ¿Ç¶è£±£´¤Î¥³¥á¥ó¥È¤Î¤ß¤ÎºÞ
      *                                     ÊÔ½¸ÂÐ±þ
      *  04.03.02    NACL-ÌçÏÆ    08/07/11  ¿ÇÃÇ½ñÌÀºÙ½ñÎÁ¤ÎÅ¦Í×Íóµ­ºÜÂÐ±þ
      *  04.03.03    NACL-ÌçÏÆ    08/07/14  ²¾¥ì¥»ºîÀ®¡Ê±¡³°½èÊý´Þ¤à¡ËÂÐ±þ
      *                                     ÅêÌôºÞÅÀ£°¡Ê±¡³°Åù¡Ëµ­ºÜ
      *  04.03.04    NACL-ÌçÏÆ    08/08/19  Æþ±¡ÎÁÌ¤»»Äê»þ¤Î¥³¥á¥ó¥ÈÂÐ±þ
      *  04.03.05    NACL-ÌçÏÆ    08/09/25  ¼«ÇåÀÕ£°£¹£µ£¹£³¤Î½¸·×ÀèÂÐ±þ
      *  04.03.06    NACL-ÌçÏÆ    08/12/01  ºßÂð¡Ê±¡³°Ê¬¡ËÊÔ½¸½¤Àµ
      *  04.03.07    NACL-ÌçÏÆ    08/12/04  ²¾¼ýÇ¼¥Á¥§¥Ã¥¯½èÍýÄÉ²Ã
      *  04.03.08    NACL-ÌçÏÆ    08/12/16  Ê¿À®£²£±Ç¯£±·î²þÀµÂÐ±þ
      *
      *  04.04.01    NACL-ÌçÏÆ    08/12/25  Êñ³çÂÐ±þ
      *  04.04.02    NACL-ÌçÏÆ    09/02/04  ¸¡ºº¤Î¥³¥á¥ó¥È¼«Æ°µ­ºÜÂÐ±þ
      *  04.04.03    NACL-ÌçÏÆ    09/04/13  Ê¿À®£²£±Ç¯£µ·î²þÀµÂÐ±þ
      *  04.04.04    NACL-ÌçÏÆ    09/06/23  ÅÀÅ©´ïºà¾¦ÉÊÌ¾µ­ºÜ½¤Àµ
      *  04.04.05    NACL-ÌçÏÆ    09/11/05  ¹âÎð¼ÔÂ¾ÊÝ¸±Æþ±¡ÂÐ±þ
      *  04.04.06    NACL-ÌçÏÆ    09/11/19  ¼«ÇåÀÕ·òÊÝ½àµòÎÅÍÜÃ´Åö¼êÅöÂÐ±þ
      *  04.04.07    NACL-ÌçÏÆ    09/12/04  ºÞ¤Þ¤È¤á¹àÌÜÃÍ¤Î·×»»½¤Àµ
      *  04.04.08    NACL-ÌçÏÆ    09/12/09  ½ÐÍè¹â»»Äê¡ÊºÞ¡Ë¤ÎÂÐ±þ
      *  04.04.09    NACL-ÌçÏÆ    10/01/13  ºÞ¤Þ¤È¤á¹àÌÜÃÍ¤Î·×»»½¤Àµ
      *  04.04.10    NACL-ÌçÏÆ    10/03/23  ²þÀµÂÐ±þ¡Ê³°Çñ¶èÊ¬£µ¤ÎÂÐ±þ¡Ë
      *                                             ¡ÊÏ«ºÒ¾è¿ô¤Ë´Ø¤ï¤ë²Ã»»ÂÐ±þ¡Ë
      *  04.04.11    NACL-ÌçÏÆ    10/03/25  ²þÀµÂÐ±þ
      *                                     ¡ÊÍ½Ìó¥³¡¼¥ÉÆÉ¤ßÈô¤Ð¤·ÊÔ½¸¡Ë
      *  04.04.12    NACL-ÌçÏÆ    10/03/29  ²þÀµÂÐ±þ
      *                                     ¡Ê²èÁü¿ÇÃÇ£²²óÌÜ°Ê¹ß¸º»»ÊÔ½¸¡Ë
      *                                     ¡Ê¥³¡¼¥ÉÄÉ²Ã¡Ë
      *  04.04.13    NACL-ÌçÏÆ    10/03/31  ²þÀµÂÐ±þ
      *                                     ¡ÊðôÉô¸ÇÄêÂÓ²Ã»»¤ÎÂÐ±þ¡Ë
      *  04.04.14    NACL-ÌçÏÆ    10/04/02  ÆÃÊÌÏ«ºÒÉÕÅº´Ç¸î¤ÎÂÐ±þ
      *  04.04.15    NACL-ÌçÏÆ    10/04/06  ²þÀµÂÐ±þ
      *                                     ¡Ê¥³¥á¥ó¥È¼«Æ°µ­ºÜ¡Ë
      *  04.04.16    NACL-ÌçÏÆ    10/05/14  Æþ±¡ÎÁ²Ã»»ºÞÈÖ¹æÊÑ´¹
      *
      *  04.05.01    NACL-Æ£¸¶    09/07/14  ¾®¿ôÅÀ°Ê²¼£µ·åÂÐ±þ
      *  04.05.02    NACL-ÌçÏÆ    09/08/17  ¾®¿ôÅÀ°Ê²¼£µ·åÂÐ±þ
      *  04.05.03    NACL-ÌçÏÆ    09/09/24  ¥ì¥»¥×¥Èµ­ºÜ¤·¤Ê¤¤¥³¥á¥ó¥ÈÂÐ±þ
      *  04.05.04    NACL-ÌçÏÆ    09/11/19  ÌôºÞ»»Äê¤Ê¤·»þ¤ÎÅ¦Í×ÍóÆ±°ìºÞ
      *                                     ¤Þ¤È¤áµ­ºÜÂÐ±þ
      *  04.05.05    NACL-ÌçÏÆ    09/12/04  ¿©»ö¡ÊÆÃÊÌ¿©¡Ë¤ÎÂ¾ÊÝ¸±ÂÐ±þ
      *  04.05.06    NACL-ÌçÏÆ    10/01/07  ¼£¸³ÂÐ±þ
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE82-FILE     ASSIGN  RECE82PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE82-KEY
                                   FILE    STATUS  IS  STS-RECE82.
           SELECT  RECE84-FILE     ASSIGN  RECE84PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE84-KEY
                                   FILE    STATUS  IS  STS-RECE84.
           SELECT  RECE844-FILE    ASSIGN  RECE84PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE844-KEY
                                   FILE    STATUS  IS  STS-RECE84.
           SELECT  RECE8A-FILE     ASSIGN  RECE8APARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE8A-KEY
                                   FILE    STATUS  IS  STS-RECE8A.
           SELECT  RECE8A4-FILE    ASSIGN  RECE8APARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE8A4-KEY
                                   FILE    STATUS  IS  STS-RECE8A.
           SELECT  RECE85-FILE     ASSIGN  RECE85PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE85-KEY
                                   FILE    STATUS  IS  STS-RECE85.
           SELECT  RECE855-FILE    ASSIGN  RECE85PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE855-KEY
                                   FILE    STATUS  IS  STS-RECE85.
           SELECT  RECE8B-FILE     ASSIGN  RECE8BPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE8B-KEY
                                   FILE    STATUS  IS  STS-RECE8B.
           SELECT  RECE8B5-FILE    ASSIGN  RECE8BPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE8B5-KEY
                                   FILE    STATUS  IS  STS-RECE8B.
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE82-FILE.
       01  RECE82-REC. 
           COPY    "CPRCF082.INC".
      *
       FD  RECE84-FILE.
       01  RECE84-REC. 
           COPY    "CPRCF084.INC".
      *
       FD  RECE844-FILE.
       01  RECE844-REC. 
           COPY    "CPRCF084.INC"  REPLACING  //RECE84//
                                   BY         //RECE844//.
      *
       FD  RECE8A-FILE.
       01  RECE8A-REC. 
           COPY    "CPRCF084.INC"  REPLACING  //RECE84//
                                   BY         //RECE8A//.
      *
       FD  RECE8A4-FILE.
       01  RECE8A4-REC. 
           COPY    "CPRCF084.INC"  REPLACING  //RECE84//
                                   BY         //RECE8A4//.
      *
       FD  RECE85-FILE.
       01  RECE85-REC. 
           COPY    "CPRCF085.INC".
      *
       FD  RECE855-FILE.
       01  RECE855-REC. 
           COPY    "CPRCF085.INC"  REPLACING  //RECE85//
                                   BY         //RECE855//.
      *
       FD  RECE8B-FILE.
       01  RECE8B-REC. 
           COPY    "CPRCF085.INC"  REPLACING  //RECE85//
                                   BY         //RECE8B//.
      *
       FD  RECE8B5-FILE.
       01  RECE8B5-REC. 
           COPY    "CPRCF085.INC"  REPLACING  //RECE85//
                                   BY         //RECE8B5//.
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE82//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE84//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE8A//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE85//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE8B//.
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë Ì¾¾ÎÎÎ°è 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  STS-AREA.
           03  STS-RECE82                              PIC X(02).
           03  STS-RECE84                              PIC X(02).
           03  STS-RECE85                              PIC X(02).
           03  STS-RECE8A                              PIC X(02).
           03  STS-RECE8B                              PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-SRYACT                              PIC 9(01).
           03  FLG-TENSU                               PIC 9(01).
           03  FLG-SYUNOU                              PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-SP                                  PIC 9(01).
           03  FLG-PTCOM                               PIC 9(01).
           03  FLG-NYUINACCT                           PIC 9(01).
           03  FLG-NYUINACT                            PIC 9(01).
           03  FLG-ARI                                 PIC 9(01).
           03  FLG-ARI2                                PIC 9(01).
           03  FLG-DAY21                               PIC 9(01).
           03  FLG-DAY24                               PIC 9(01).
           03  FLG-TENTEKI                             PIC 9(01).
           03  FLG-WRITE85                             PIC 9(01).
           03  FLG-WRITE85X                            PIC 9(01).
           03  FLG-SIKIBETU                            PIC 9(01).
           03  FLG-ZANHAIKI                            PIC 9(01).
           03  FLG-FORMAL-KISAI                        PIC 9(01).
           03  FLG-RECE85-ZAN                          PIC 9(01).
           03  FLG-REWRITE                             PIC 9(01).
           03  FLG-OKIKAE                              PIC 9(01).
           03  FLG-SET                                 PIC 9(01).
           03  FLG-HEYA                                PIC 9(01).
           03  FLG-KHN                                 PIC 9(01).
           03  FLG-NYUINRYO                            PIC 9(01).
           03  FLG-HKNCOMBI                            PIC 9(01).
           03  FLG-HOUSIJI-COM                         PIC 9(01).
           03  FLG-OSIN-COM                            PIC 9(01).
           03  FLG-SYOBYO-COM                          PIC 9(01).
           03  FLG-RYOYO-COM                           PIC 9(01).
           03  FLG-SYODOU-COM                          PIC 9(01).
           03  FLG-KENSA-COM                           PIC 9(01).
           03  FLG-TAIGAI                              PIC 9(01).
           03  FLG-RINYAK                              PIC 9(01).
           03  FLG-KHNRYO-SKJ                          PIC 9(01).
           03  FLG-TAISYO                              PIC 9(01).
           03  FLG-TENTEKIMATOME                       PIC 9(01).
           03  FLG-SRYACCTPLUS                         PIC 9(01).
           03  FLG-OSINKBN                             PIC 9(01).
           03  FLG-ZAITAKU                             PIC 9(01).
           03  FLG-TAHOKEN                             PIC 9(01).
           03  FLG-HOUSEI-COM                          PIC 9(01).
           03  FLG-HOUMON-COM                          PIC 9(01).
           03  FLG-SRYKBN-HEN                          PIC 9(01).
           03  FLG-SYUNOU-ARI                          PIC 9(01).
           03  FLG-FORMALNAME                          PIC 9(01).
           03  FLG-JYOGAI                              PIC 9(01).
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
           03  CNT-DAY21                               PIC 9(06).
           03  CNT-DAY24                               PIC 9(06).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                                     PIC 9(03).
           03  IDX2                                    PIC 9(03).
           03  IDX3                                    PIC 9(03).
           03  IDX4                                    PIC 9(03).
           03  IDX5                                    PIC 9(03).
           03  IDX6                                    PIC 9(03).
           03  IDX7                                    PIC 9(03).
           03  IDX8                                    PIC 9(03).
           03  IDX9                                    PIC 9(03).
           03  IDX10                                   PIC 9(03).
           03  IDX11                                   PIC 9(02).
           03  IDX12                                   PIC 9(02).
           03  IDX13                                   PIC 9(02).
           03  IDX14                                   PIC 9(02).
           03  IDXA                                    PIC 9(01).
           03  IDXB                                    PIC 9(03).
           03  IDY                                     PIC 9(03).
           03  IDY2                                    PIC 9(03).
           03  IDY3                                    PIC 9(03).
           03  IDZ                                     PIC 9(03).
           03  IDW                                     PIC 9(03).
           03  IDW3                                    PIC 9(03).
           03  IDW4                                    PIC 9(03).
           03  IDW5                                    PIC 9(03).
           03  IDW6                                    PIC 9(03).
           03  ID1                                     PIC 9(03).
           03  ID2                                     PIC 9(03).
           03  ID3                                     PIC 9(03).
           03  ID4                                     PIC 9(03).
           03  IDU                                     PIC 9(03).
           03  NUM-IDX                                 PIC 9(01).
           03  SYUKEI-IDX                              PIC 9(03).
           03  SYUKEI-IDX2                             PIC 9(03).
           03  SYUKEI-IDX3                             PIC 9(03).
           03  SYUKEI-IDX4                             PIC 9(03).
           03  SYSKAN-IDX                              PIC 9(03).
           03  SYSKAN-IDX2                             PIC 9(03).
           03  IDV                                     PIC 9(02).
           03  IDV2                                    PIC 9(03).
           03  FUKA-IDX                                PIC 9(03).
      *
      *    ´ðËÜÅÀ¿ô¤ÎÈóÉ½¼¨È½ÄêÍÑ¥Õ¥é¥°¡Ê¼«ÇåÀÕÍÑ¡Ë
       01  FLG-KHNTEN-JI-AREA.                                          
           03  FLG-KHNTEN7-JI                          PIC 9(01).       
           03  FLG-KHNTEN8-JI                          PIC 9(01).       
           03  FLG-KHNTEN9-JI                          PIC 9(01).       
      *
      *    ºÞ¾ðÊóÂàÈòÎÎ°è
       01  WK-ZAIKAISU                                 PIC S9(07).
       01  WK-ZAITEN                                   PIC S9(08).
       01  WK-SYUTEN                                   PIC S9(08).
       01  WK-KIZAITEN                                 PIC S9(08).
       01  WK-YKZTEN                                   PIC S9(08).
      *
      *    ¥¨¥é¡¼¥á¥Ã¥»¡¼¥¸ÊÔ½¸ÎÎ°è
       01  ERR-EDIT-AREA.
           03  ERR-PTNUM                               PIC X(20).
           03  ERR-ERRMSG                              PIC X(80).
           03  ERR-HEN-ERRMSG                          PIC X(200).
      *
      *    ÅÀ¿ô¥Æ¡¼¥Ö¥ë¸¡º÷ÍÑÂÐ¾ÝÆüÎÎ°è
       01  WK-TAIHI-AREA.
           03  WK-TAIHI-DAY                            PIC 9(02).
           03  WK-TAIHI-FLG                            PIC 9(01).
           03  WK-TAIHI-SRYYMD.
               05  WK-TAIHI-SRYYM                      PIC 9(06).
               05  WK-TAIHI-SRYD                       PIC X(02).
      *
       01  WRK-DAY-TABLE.
           03  WRK-DAY-T                               OCCURS   31.
               05  WRK-DAY1                            PIC 9(04).
               05  WRK-DAY2                            PIC 9(04).
               05  WRK-DAY4                            PIC 9(04).
               05  WRK-DAY5                            PIC 9(04).
      *
       01  WRK-DAY-TABLE2.
           03  WRK-DAY-T2                              OCCURS   31.
               05  WRK-DAY21                           PIC 9(04).
               05  WRK-DAY24                           PIC 9(04).
      *
       01  WRK-SKJ-TABLE.
           03  WRK-SKJ-T                               OCCURS   31.
               05  WRK-SKJ-ZAITEN                      PIC 9(08).
               05  WRK-SKJ-ZAIKAISU                    PIC 9(04).
               05  WRK-SKJ-SRYCD-T.
                06  WRK-SKJ-SRYCD-OCC                  OCCURS   15.
                 07  WRK-SKJ-SRYCD                     PIC 9(09).
      *
       01  WRK-SKJ-TABLE2.
           03  WRK-SKJ-T2                              OCCURS   31.
               05  WRK-SKJ2-ZAITEN                     PIC 9(08).
               05  WRK-SKJ2-ZAIKAISU                   PIC 9(04).
               05  WRK-SKJ2-MEISAISU                   PIC 9(04).
               05  WRK-SKJ2-SRYCDTOTAL                 PIC 9(14).
               05  WRK-SKJ2-SRYCD-T.
                06  WRK-SKJ2-SRYCD-OCC                 OCCURS   15.
                 07  WRK-SKJ2-SRYCD                    PIC 9(09).
               05  WRK-SKJ2-KAISU-T.
                06  WRK-SKJ2-KAISU-OCC                 OCCURS   31.
                 07  WRK-SKJ2-KAISU                    PIC 9(01).
      *
       01  WRK-SKJ200604-TABLE.
           03  WRK-SKJ200604-T                         OCCURS   31.
               05  WRK-SKJ200604-KBN                   PIC 9(01).
               05  WRK-SKJ200604-SRYCDTOTAL            PIC 9(14).
               05  WRK-SKJ200604-MEISAISU              PIC 9(02).
               05  WRK-SKJ200604-SURYOUTOTAL           PIC 9(02).
               05  WRK-SKJ200604-T2                    OCCURS    3.
                   07  WRK-SKJ200604-DAY               PIC 9(04).
               05  WRK-SKJ200604-T3                    OCCURS    9.
                   07  WRK-SKJ200604-SRYCD             PIC 9(09).
                   07  WRK-SKJ200604-SURYOU            PIC 9(02).
      *
               05  WRK-SKJ200604-SRYCDTOTAL-KAN        PIC 9(14).
               05  WRK-SKJ200604-MEISAISU-KAN          PIC 9(02).
               05  WRK-SKJ200604-SURYOUTOTAL-KAN       PIC 9(02).
               05  WRK-SKJ200604-DAY-KAN               PIC 9(01).
               05  WRK-SKJ200604-T4                    OCCURS    3.
                   07  WRK-SKJ200604-SRYCD-KAN         PIC 9(09).
                   07  WRK-SKJ200604-SURYOU-KAN        PIC 9(02).
      *
       01  WRK-NYULIFE-TABLE.
           03  WRK-NYULIFE-AREA                        OCCURS   31.
               05  WRK-NYULIFE-KBN                     PIC X(01).
      *
       01  WRK-TENTEKI2-TABLE.
      *
           03  WRK-TENTEKI2-HENKYAKUARI                PIC 9(01).
      *
           03  WRK-TENTEKI2-HENKYAKU-AREA.
      *
               05  WRK-TENTEKI2-ZAINUM                 PIC 9(08)
                                                       OCCURS   20.
      *
               05  WRK-TENTEKI2-T-AREA                 OCCURS   31.
                 06  WRK-TENTEKI2-T-MEISAISU           PIC 9(03).
                 06  WRK-TENTEKI2-T-ZAITEN             PIC 9(07).
                 06  WRK-TENTEKI2-T-SRYCDTOTAL         PIC 9(13).
                 06  WRK-TENTEKI2-T-SRYCD-X.
                   07  WRK-TENTEKI2-T-SRYCD            PIC X(09)
                                                       OCCURS    5.
               05  WRK-TENTEKI2-T-ERRCD                PIC 9(01).
      *
               05  WRK-TENTEKI2-C-AREA                 OCCURS   31.
                 06  WRK-TENTEKI2-C-MEISAISU           PIC 9(03).
                 06  WRK-TENTEKI2-C-ZAITEN             PIC 9(07).
                 06  WRK-TENTEKI2-C-SRYCDTOTAL         PIC 9(13).
                 06  WRK-TENTEKI2-C-SRYCD-X.
                   07  WRK-TENTEKI2-C-SRYCD            PIC X(09)
                                                       OCCURS    5.
               05  WRK-TENTEKI2-C-ERRCD                PIC 9(01).
      *
       01  WRK-ORCSTEN-TABLE.
           03  WRK-ORCSTEN-T                           OCCURS  350.
               05  WRK-ORCSTEN-SRYCD                   PIC X(09).
               05  WRK-ORCSTEN-SURYO                   PIC 9(05)V9(05).
               05  WRK-ORCSTEN-COM-MEI                 PIC X(80).
           03  WRK-ORCSTEN-T2                          OCCURS   31.
               05  WRK-ORCSTEN-DAY                     PIC 9(03).
           03  WRK-ORCSTEN-ZAIKAISU                    PIC 9(04).
      *
       01  WRK-CHUKSNSRYCD-TABLE.
           03  WRK-CHUKSNSRYCD                         PIC X(09)
                                                       OCCURS   50.
      *
      *    °åÎÅµ¡´Ø¡ÊÆþ±¡´ðËÜ¡ËÂàÈòÍÑÎÎ°è
       01  TBL-SYS5000-T.
           03  TBL-SYS5000-OCC     OCCURS  100.
               05  TBL-SYS5000                         PIC X(610).
      *    °åÎÅµ¡´Ø¡Ê´ðËÜ¾ðÊó¡ËÂàÈòÍÑÎÎ°è
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC     OCCURS  100.
               05  TBL-SYS1001                         PIC X(610).
      *    »ÜÀß´ð½à¾ðÊóÂàÈòÍÑÎÎ°è
       01  TBL-SYS1006-T.
           03  TBL-SYS1006-OCC                         OCCURS  100.
               05  TBL-SYS10061                        PIC X(610).
               05  TBL-SYS10062                        PIC X(610).
      *
      *    ¼êµ»ÅÀ¡¦ÌôºÞÅùÅÀ¿ôÍÑÎÎ°è
       01  TBL-TENSUG-JI.
           03  TBL-TENSU-JI                            OCCURS   50.
               05  TBL-SYUTEN-JI                       PIC 9(09).
               05  TBL-YKZTEN-JI                       PIC 9(09).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                      PIC X(06).
               05  WRK-PARA-TERMID                     PIC X(16).
               05  WRK-PARA-SYSYMD                     PIC X(08).
               05  WRK-PARA-RECEKBN                    PIC X(01).
               05  WRK-PARA-JOBID                      PIC 9(07).
               05  WRK-PARA-SHELLID                    PIC X(08).
           03  WRK-PARA-JIBAIPRTKBN                    PIC X(01).
           03  WRK-PARA-PRTKBN                         PIC X(01).
           03  WRK-PARA-HOSPNUM                        PIC 9(02).
      *
           03  WRK-SYS-SRYYMD                          PIC X(08).
      *
           03  WRK-SRYYM-5000                          PIC X(06).
           03  WRK-SYS5000-COUNT                       PIC 9(02).
      *
           03  WRK-TEN                                 PIC 9(09).
           03  WRK-MEISAI-CNT                          PIC 9(04).
           03  WRK-SRYCDTOTAL                          PIC 9(14).
           03  WRK-SURYOUTOTAL                         PIC 9(07)V9(05).
           03  WRK-KSNRATE                             PIC 9V99.    
           03  WRK-YOUBUKASAN                          PIC 9(07).
           03  WRK-KYOBUKASAN                          PIC 9(07).
           03  WRK-KEIBUKASAN                          PIC 9(07).
           03  WRK-SOGAIKASAN                          PIC 9(07).
           03  WRK-RYOYOTEATE                          PIC 9(07).
           03  WRK-RYOYOTEATE2                         PIC 9(07).
      *
           03  WRK-HEYA-NINZU                          PIC 9(02).
      *
           03  WRK-SRYCD-X.
               05  WRK-SRYCD                           PIC 9(09).
           03  WRK-ZAINUM                              PIC 9(08).
           03  WRK-ZAINUM-CNT2                         PIC 9(03).
           03  WRK-RENNUM                              PIC 9(02).
           03  WRK-RENNUM2                             PIC 9(02).
           03  WRK-TANICD                              PIC 9(03).
           03  WRK-TANIMOJISU                          PIC 9(01).
           03  WRK-TANINAME                            PIC X(24).
      *
           03  WRK-SRYSURYO                            PIC 9(05)V9(05).
           03  WRK-SRYSURYO2                           PIC 9(05).
           03  WRK-SRYSURYO3                           PIC 9(05).
      *
           03  WRK-RECEERR                             PIC X(200).
      *
           03  WRK-SHOKUJIRYOYO-OCC.
               05  WRK-SHOKUJIRYOYO                    PIC 9(01)
                                                       OCCURS 31.
           03  WRK-TOKUKANRIKASAN-OCC.
               05  WRK-TOKUKANRIKASAN                  PIC 9(01)
                                                       OCCURS 31.
      *
           03  WRK-KEISAN                              PIC 9(09).
           03  WRK-KEISAN2                             PIC 9(09).
      *
           03  WRK-SROUNYUIN-SRYCDTOTAL                PIC 9(14).
           03  WRK-SROUNYUIN-MEISAISU                  PIC 9(07).
           03  WRK-SROUNYUIN-SRYCD                     PIC 9(09).
      *
           03  WRK-TEN-KZI                             PIC 9(09).
           03  WRK-TEN-BUN                             PIC 9(09).
      *
      *    ¼«ÇåÀÕÆÃÄê´ïºàÅù¤ÎÅÀ¿ôÂàÈò
           03  WRK-JIBAIKZI-TNS-TAIHI-TABLE.
               05  WRK-JIBAIKZI-TNS-TAIHI-OCC          OCCURS  5.
                 06  WRK-JIBAIKZI-TNS                  PIC 9(09).
                 06  WRK-JIBAIKZI-SYUKEIKBN            PIC 9(01).
      *
           03  WRK-SYUTEN-JI                           PIC S9(09).
           03  WRK-YKZTEN-JI                           PIC 9(09).
           03  WRK-KINGAKU-JI                          PIC 9(09).
      *
      *    ¾ÃÈñÀÇÃ¼¿ô¶èÊ¬
           03  WRK-TAX1001-KBN                         PIC X(01).
      *
           03  WRK-SISETU613                           PIC 9(01).
           03  WRK-SISETU695                           PIC 9(01).
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
           03  WRK-FUKA-KINGAKU                PIC 9(09).
           03  WRK-FUKA-VALUE                  PIC 9(01).
      *
           03  WRK-ZAITENNASHI                 PIC 9(09).
      *
           03  WRK-TENTEKI2-SRYCD-X.
               05  WRK-TENTEKI2-SRYCD          PIC X(09)
                                               OCCURS  5.
      *
           03  WRK-STR                         PIC X(200).
           03  WRK-LEN                         PIC 9(05).
           03  WRK-SHO                         PIC 9(05).
           03  WRK-AMARI                       PIC 9(05).
      *
      *    ¸ÇÄêÃÍ
       01  CONST-AREA.
      *    ¥Õ¥ê¡¼¥³¥á¥ó¥È
           03  CONS-COMMENT    PIC X(09)   VALUE   "810000001".
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
      *    ÆýÍÄ»ù²Ã»»
           03  CONS-NYUKSN1        PIC X(09) VALUE "111000370".
           03  CONS-NYUKSN2        PIC X(09) VALUE "111011470".
           03  CONS-NYUKSN3        PIC X(09) VALUE "111011970".
           03  CONS-NYUKSN4        PIC X(09) VALUE "111012070".
           03  CONS-NYUKSN5        PIC X(09) VALUE "111012170".
           03  CONS-NYUKSN6        PIC X(09) VALUE "111012270".
           03  CONS-NYUKSN7        PIC X(09) VALUE "111011570".
           03  CONS-NYUKSN8        PIC X(09) VALUE "111011670".
           03  CONS-NYUKSN9        PIC X(09) VALUE "111011770".
      *
      *    Ï«ºÒ¼«ÇåÀÕ´Ø·¸
           03  CONS-ROUSAI1    PIC X(09)   VALUE   "101110010".
           03  CONS-ROUSAI2    PIC X(09)   VALUE   "101110020".
           03  CONS-ROUSAI3    PIC X(09)   VALUE   "101130010".
           03  CONS-ROUSAI4    PIC X(09)   VALUE   "101130020".
           03  CONS-ROUSAI5    PIC X(09)   VALUE   "101130030".
           03  CONS-ROUSAI6    PIC X(09)   VALUE   "101130040".
           03  CONS-ROUSAI7    PIC X(09)   VALUE   "101130050".
           03  CONS-ROUSAI8    PIC X(09)   VALUE   "101130060".
           03  CONS-ROUSAI9    PIC X(09)   VALUE   "101130070".
           03  CONS-ROUSAI10   PIC X(09)   VALUE   "101130080".
           03  CONS-ROUSAI11   PIC X(09)   VALUE   "101130090".
           03  CONS-ROUSAI12   PIC X(09)   VALUE   "101130100".
           03  CONS-ROUSAI13   PIC X(09)   VALUE   "101910010".
           03  CONS-ROUSAI14   PIC X(09)   VALUE   "101910020".
           03  CONS-ROUSAI15   PIC X(09)   VALUE   "101910030".
           03  CONS-ROUSAI16   PIC X(09)   VALUE   "101910040".
           03  CONS-ROUSAI17   PIC X(09)   VALUE   "101910070".
           03  CONS-ROUSAI18   PIC X(09)   VALUE   "101910080".
           03  CONS-ROUSAI19   PIC X(09)   VALUE   "101910090".
           03  CONS-ROUSAI20   PIC X(09)   VALUE   "101910100".
           03  CONS-ROUSAI21   PIC X(09)   VALUE   "101110040".
      *
           03  CONS-ROUCOM1    PIC X(09)   VALUE   "840800001".
           03  CONS-ROUCOM2    PIC X(09)   VALUE   "840800002".
           03  CONS-ROUCOM3    PIC X(09)   VALUE   "820800001".
           03  CONS-ROUCOM4    PIC X(09)   VALUE   "820800002".
           03  CONS-ROUCOM5    PIC X(09)   VALUE   "820800003".
           03  CONS-ROUCOM6    PIC X(09)   VALUE   "820800004".
           03  CONS-ROUCOM7    PIC X(09)   VALUE   "830800001".
      *
      *    ¹øÉô¸ÇÄêÂÓ²Ã»»
           03  CONS-YOUBUKASAN PIC X(09)   VALUE   "140037490".
      *    ¶»Éô¸ÇÄêÂÓ²Ã»»
           03  CONS-KYOBUKASAN PIC X(09)   VALUE   "140040110".
      *    ðôÉô¸ÇÄêÂÓ²Ã»»
           03  CONS-KEIBUKASAN PIC X(09)   VALUE   "140052610".
      *    ÁÏ³°¸ÇÄê´ï»ÈÍÑ²Ã»»
           03  CONS-SOGAIKASAN PIC X(09)   VALUE   "150266970".
      *    ÎÅÍÜÃ´Åö¼êÅö¡ÊÆþ±¡¡Ë
           03  CONS-RYOYOTEATE PIC X(09)   VALUE   "199000510".
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
      *    ÌôºÞ´ÉÍý»ØÆ³ÎÁ
           03  CONS-YAKUKAN-COMMENT1 PIC X(09)  VALUE  "113010710".
           03  CONS-YAKUKAN-COMMENT2 PIC X(09)  VALUE  "113010810".
           03  CONS-YAKUKAN-COMMENT3 PIC X(09)  VALUE  "120001610".
      *    Æþ±¡±ÉÍÜ¿©»ö»ØÆ³ÎÁ
           03  CONS-NYUEIYO-COMMENT  PIC X(09)  VALUE  "113003310".
      *    Âà±¡Á°Ë¬Ìä»ØÆ³ÎÁ
           03  CONS-HOUMON-COMMENT1  PIC X(09)  VALUE  "190030310".
      *    ¡ÊÏ·¡ËÂà±¡Á°Ë¬Ìä»ØÆ³ÎÁ
           03  CONS-HOUMON-COMMENT2  PIC X(09)  VALUE  "190723610".
      *    Âà±¡Á°Ë¬Ìä»ØÆ³ÎÁ»»ÄêÆü
           03  CONS-HOUMON-COMMENT3  PIC X(09)  VALUE  "840000055".
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
      *    Âà±¡»þÉþÌô»ØÆ³²Ã»»
           03  CONS-TAIIN-COMMENT    PIC X(09)  VALUE  "113006270".
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
      *    Àº¿À²ÊÂà±¡Á°Ë¬Ìä»ØÆ³ÎÁ
           03  CONS-HOUSEI-COMMENT   PIC X(09)  VALUE  "180018310".
      *    Àº¿À²ÊÂà±¡Á°Ë¬Ìä»ØÆ³ÎÁ»»ÄêÆü
           03  CONS-HOUSEI-COMMENT2  PIC X(09)  VALUE  "840000079".
      *    Àº¿À²Ê³ÖÎ¥ÉÂ¼¼´ÉÍý²Ã»»
           03  CONS-KAKURI-COMMENT   PIC X(09)  VALUE  "190107070".
      *    Ï«ºÒ¼£ÎÅ·×²è²Ã»»
           03  CONS-CHIRYO-COMMENT   PIC X(09)  VALUE  "101910050".
      *
      *    ÉÂ°áÂßÍ¿ÎÁ
           03  CONS-BYOITAIYO        PIC X(09)  VALUE  "101910060".
      *    ºßÂð·ì±ÕÆ©ÀÏÉÑ²ó»ØÆ³´ÉÍýÎÁ
           03  CONS-HINKAI-COMMENT   PIC X(09)  VALUE  "114009410".
      *    ½é¿Ç¡ÊÆ±ÆüÊ£¿ô¿ÇÎÅ²Ê¡Ë
           03  CONS-SYODOU-COMMENT   PIC X(09)  VALUE  "101110040".
      *    Ê£½é»»Äê¿ÇÎÅ²Ê¡§
           03  CONS-SYODOU2-COMMENT  PIC X(09)  VALUE  "830000024".
      *    ½é¿Ç¡ÊÆ±ÆüÊ£¿ô¿ÇÎÅ²Ê¡Ë
           03  CONS-SYODOU3-COMMENT  PIC X(09)  VALUE  "111011810".
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
      *!   ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CONS-SINDAI7-COMMENT  PIC X(09)  VALUE  "101800080".
      *!   ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë
           03  CONS-SINDAI8-COMMENT  PIC X(09)  VALUE  "101800090".
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
      *!   ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CONS-KOKYUKI7-COMMENT PIC X(09)  VALUE  "101800150".
      *!   ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë
           03  CONS-KOKYUKI8-COMMENT PIC X(09)  VALUE  "101800160".
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
      *!   Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CONS-NOUKE8-COMMENT   PIC X(09)  VALUE  "101800100".
      *!   Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë
           03  CONS-NOUKE9-COMMENT   PIC X(09)  VALUE  "101800110".
      *!   Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë
           03  CONS-NOUKE10-COMMENT  PIC X(09)  VALUE  "101800120".
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÇÑÍÑ¾É¸õ·²¡Ë
           03  CONS-NOUKE11-COMMENT  PIC X(09)  VALUE  "180032410".
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÇÑÍÑ¾É¸õ·²¡Ë
           03  CONS-NOUKE12-COMMENT  PIC X(09)  VALUE  "180032510".
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÇÑÍÑ¾É¸õ·²¡Ë
           03  CONS-NOUKE13-COMMENT  PIC X(09)  VALUE  "180032610".
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
      *!   ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CONS-UNDOKI7-COMMENT  PIC X(09)  VALUE  "101800130".
      *!   ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë
           03  CONS-UNDOKI8-COMMENT  PIC X(09)  VALUE  "101800140".
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CONS-UNDOKI9-COMMENT  PIC X(09)  VALUE  "180032710".
      *!   ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CONS-UNDOKI10-COMMENT PIC X(09)  VALUE  "101800125".
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
      *
      *    £Â£Î£ÐÀºÌ©
           03  CONS-KENSA1-COMMENT   PIC X(09)  VALUE  "160162350".
      *    £Î£Ô¡Ý£ð£ò£ï£Â£Î£ÐÀºÌ©
           03  CONS-KENSA2-COMMENT   PIC X(09)  VALUE  "160181250".
      *    £È£Á£Î£ÐÀºÌ©
           03  CONS-KENSA3-COMMENT   PIC X(09)  VALUE  "160116310".
      *
      *    ²ð¸î»Ù±çÏ¢·È»ØÆ³ÎÁ
           03  CONS-KAISIEN-COMMENT  PIC X(09)  VALUE  "113011710".
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
      *    ¶É½ê±¢°µÊÄº¿½èÃÖ¡Ê¤½¤ÎÂ¾¡Ë
           03  CONS-KYOKUIN1-COMMENT PIC X(09)  VALUE  "140052410".
      *    ¶É½ê±¢°µÊÄº¿½èÃÖ¡ÊÈïÊ¤ºàÅ½ÉÕ¡Ë¡Ê£±£°£°£ã£í£²Ì¤Ëþ¡Ë
           03  CONS-KYOKUIN2-COMMENT PIC X(09)  VALUE  "140051810".
      *    ¶É½ê±¢°µÊÄº¿½èÃÖ¡ÊÈïÊ¤ºàÅ½ÉÕ¡Ë¡Ê£±£°£°£ã£í£²¡Á£²£°£°£ã£í£²Ì¤Ëþ¡Ë
           03  CONS-KYOKUIN3-COMMENT PIC X(09)  VALUE  "140051910".
      *    ¶É½ê±¢°µÊÄº¿½èÃÖ¡ÊÈïÊ¤ºàÅ½ÉÕ¡Ë¡Ê£²£°£°£ã£í£²°Ê¾å¡Ë
           03  CONS-KYOKUIN4-COMMENT PIC X(09)  VALUE  "140052010".
      *    ±ÉÍÜ¥µ¥Ý¡¼¥È¥Á¡¼¥à²Ã»»
           03  CONS-EISAPO-COMMENT   PIC X(09)  VALUE  "190136810".
      *    ¸ÆµÛ¥±¥¢¥Á¡¼¥à²Ã»»
           03  CONS-KOKKEA-COMMENT   PIC X(09)  VALUE  "190137910".
      *    ½Å¾É¼Ô²Ã»»¡ÊÀº¿ÀÎÅÍÜÉÂÅïÆþ±¡ÎÁ¡Ë
           03  CONS-JYUSYO-COMMENT   PIC X(09)  VALUE  "190141370".
      *
      *    ³°Íè¿×Â®¸¡ÂÎ¸¡ºº²Ã»»
           03  CONS-GAIJINKEN        PIC X(09)  VALUE  "160177770".
      *
      *    ºß»Ù±ç¥Õ¥é¥°ÊÔ½¸¤Î°Ù¤Î¥³¡¼¥É
           03  CONS-ZAISIEN1         PIC X(09)  VALUE  "114011570".
           03  CONS-ZAISIEN2         PIC X(09)  VALUE  "114011670".
           03  CONS-ZAISIEN3         PIC X(09)  VALUE  "114011770".
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
       01  WRK-RECE82G-REC. 
           COPY    "CPRCF082.INC"  REPLACING  //RECE82//
                                   BY         //WRK-RECE82G//.
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
      *    ¿ÇÎÅ²ñ·×ÉÕ²Ã¾ðÊó¥Þ¥¹¥¿¡Ý
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
      *    ¿ÇÎÅ¹Ô°Ù
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    Æþ±¡²ñ·×
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    ÅÀ¿ô
           COPY    "CPTENSU.INC".
      *
      *    ¼ýÇ¼
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý
           COPY    "CPSYSKANRI.INC".
      *    °åÎÅµ¡´Ø¾ðÊó¡Ý´ðËÜ
           COPY    "CPSK1001.INC".
           COPY    "CPSK1006.INC".
      *
           COPY    "CPSK200501.INC".
      *
           COPY    "CPSK4003.INC".
      *
           COPY    "CPSK5000.INC".
      *
      *    ´µ¼Ô¥³¥á¥ó¥È
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    ÊÝ¸±ÁÈ¤ß¹ç¤ï¤»
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑÎÎ°è
      *****************************************************************
      *
      *    È¾³Ñ¥Á¥§¥Ã¥¯¥µ¥Ö
      **** COPY    "CPORCSKANACHK.INC".
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
      *    Æþ±¡ÅÀÅ©°ì³ç¹¹¿·¥µ¥Ö
           COPY    "CPORCSTENTEK.INC".
      *    Ï«ºÒÆþ±¡²ñ·×ºîÀ®¥µ¥Ö
           COPY    "CPORCSROUNYUIN.INC".
      *    Æþ±¡»þÀ¸³èÎÅÍÜÈñÉéÃ´³Û¼èÆÀ¥µ¥Ö
           COPY    "CPORCSNYULIFE.INC".
      *    Æþ±¡ÅÀÅ©¼êµ»²Ã»»¤Þ¤È¤á¥µ¥Ö
           COPY    "CPORCSTENTEK2.INC".
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
      *
      *****************************************************************
      *    ½é´ü¡¡½èÍý
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
                                       WRK-AREA
                                       FLG-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-JIBAIPRTKBN
                                               WRK-PARA-PRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    "RECE820"           TO  RECE82PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE82PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE82PARA-HOSPNUM
           MOVE    "RECE840"           TO  RECE84PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE84PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE84PARA-HOSPNUM
           MOVE    "RECE850"           TO  RECE85PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE85PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE85PARA-HOSPNUM
      *
           MOVE    "RECE841"           TO  RECE8APARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE8APARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE8APARA-HOSPNUM
           MOVE    "RECE851"           TO  RECE8BPARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE8BPARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE8BPARA-HOSPNUM
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 100-DBOPEN-SEC
      *
      *        ¥¹¥Æ¥Ã¥×´ÉÍý³«»Ï½èÍý
               MOVE    "STS"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    "ORCR0840"          TO  JOB-PGID
               MOVE    "¿ÇÎÅ¶èÊ¬ÊÌÅÀ¿ô½¸·×¡ÊÏ«ºÒ¡¦¼«ÇåÀÕ¡Ë"
                                           TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
      *        ½èÍýÃæ»ßÀßÄê½èÍý
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF 
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡Ê£±£°£°£±¡Ë¤«¤é¾ÃÈñÀÇÃ¼¿ô¶èÊ¬
      *       ¤Î¼èÆÀ
              MOVE    SPACE               TO  WRK-TAX1001-KBN
      *
              INITIALIZE                      SYSKANRI-REC
              MOVE    "1001"              TO  SYS-KANRICD
              MOVE    "*"                 TO  SYS-KBNCD
              MOVE    WRK-PARA-SRYYM      TO  SYS-STYUKYMD (1:6)
              MOVE    "01"                TO  SYS-STYUKYMD (7:2)
              MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
grpsys        MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              PERFORM 910-SYSKANRI-INV-SEC
              IF      FLG-SYSKANRI        =   ZERO
                MOVE  MCPDATA-REC         TO  SYS-1001-REC
                MOVE  SYS-1001-TAX-HASU-KBN
                                          TO  WRK-TAX1001-KBN
              ELSE
                MOVE  "°åÎÅµ¡´Ø¾ðÊó¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£"
                                          TO  WRK-RECEERR
                PERFORM 500-ERR-HENSYU-SEC
                MOVE    1                 TO  FLG-END
              END-IF
      *
      *       »ÜÀß´ð½à¾ðÊó
              MOVE     ZERO     TO     WRK-SISETU613
              MOVE     ZERO     TO     WRK-SISETU695
              PERFORM  VARYING  IDXA   FROM   1   BY  1
                        UNTIL   IDXA     >    2
                INITIALIZE                      SYSKANRI-REC
                MOVE    "1006"              TO  SYS-KANRICD
                MOVE    IDXA                TO  SYS-KBNCD
                MOVE    WRK-PARA-SRYYM      TO  SYS-STYUKYMD (1:6)
                MOVE    "01"                TO  SYS-STYUKYMD (7:2)
                MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
grpsys          MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
                MOVE    SYSKANRI-REC        TO  MCPDATA-REC
                PERFORM 910-SYSKANRI-INV-SEC
                IF      FLG-SYSKANRI     =   ZERO
                   MOVE   MCPDATA-REC       TO  SYS-1006-REC
                   EVALUATE  IDXA
                     WHEN     1
                       CONTINUE
                     WHEN     2
                       MOVE  SYS-1006-SSTKIJUN(113)
                                            TO  WRK-SISETU613
                       MOVE  SYS-1006-SSTKIJUN(195)
                                            TO  WRK-SISETU695
                   END-EVALUATE
                END-IF
              END-PERFORM
           ELSE
              MOVE     ZERO               TO  SYSKAN-IDX
              MOVE     SPACE              TO  TBL-SYS1001-T
              INITIALIZE                      TBL-SYS1001-T
      *
              INITIALIZE                      SYSKANRI-REC
              MOVE    "1001"              TO  SYS-KANRICD
              MOVE    "*"                 TO  SYS-KBNCD
grpsys        MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
grpsys        PERFORM 900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"  TO  MCP-TABLE
                  MOVE    "key8"          TO  MCP-PATHNAME
                  PERFORM 900-SYSKANRI-READ-SEC
                  PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC   TO  SYS-1001-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1001-REC  TO  TBL-SYS1001(SYSKAN-IDX)
      *
                    MOVE    "tbl_syskanri"    TO  MCP-TABLE
                    MOVE    "key8"            TO  MCP-PATHNAME
                    PERFORM 900-SYSKANRI-READ-SEC
                  END-PERFORM
              ELSE
                  MOVE    "°åÎÅµ¡´Ø¾ðÊó¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£"
                                          TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1               TO  FLG-END
              END-IF
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
      *
              MOVE     SPACE              TO  TBL-SYS1006-T
              INITIALIZE                      TBL-SYS1006-T
      *
              PERFORM  VARYING  IDXA   FROM   1   BY  1
                        UNTIL   IDXA     >    2
                 MOVE     ZERO            TO  SYSKAN-IDX2
                 INITIALIZE                   SYSKANRI-REC
                 MOVE    "1006"           TO  SYS-KANRICD
                 MOVE    IDXA             TO  SYS-KBNCD
grpsys           MOVE    SPA-HOSPNUM      TO  SYS-HOSPNUM
                 MOVE    SYSKANRI-REC     TO  MCPDATA-REC
                 MOVE    "tbl_syskanri"   TO  MCP-TABLE
                 MOVE    "key8"           TO  MCP-PATHNAME
grpsys           PERFORM 900-DBSELECT-SEC
                 IF      MCP-RC           =   ZERO
                     MOVE    "tbl_syskanri"   TO  MCP-TABLE
                     MOVE    "key8"           TO  MCP-PATHNAME
                     PERFORM  900-SYSKANRI-READ-SEC
                     PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                               OR    (SYSKAN-IDX2  >=  100)
      *
                       MOVE   MCPDATA-REC     TO  SYS-1006-REC
                       EVALUATE  IDXA
                         WHEN     1
                           ADD     1      TO  SYSKAN-IDX2
                           MOVE    SYS-1006-REC
                                          TO  TBL-SYS10061(SYSKAN-IDX2)
                         WHEN     2
                           ADD     1      TO  SYSKAN-IDX2
                           MOVE    SYS-1006-REC
                                          TO  TBL-SYS10062(SYSKAN-IDX2)
                       END-EVALUATE
                       MOVE    "tbl_syskanri" TO  MCP-TABLE
                       MOVE    "key8"         TO  MCP-PATHNAME
                       PERFORM  900-SYSKANRI-READ-SEC
                     END-PERFORM
                 END-IF
                 MOVE    "tbl_syskanri"       TO  MCP-TABLE
                 MOVE    "key8"               TO  MCP-PATHNAME
                 PERFORM  900-CLOSE-SEC
              END-PERFORM
           END-IF
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *
             MOVE      WRK-PARA-SRYYM    TO  WRK-SRYYM-5000
             PERFORM   120-SYS-5000-CHK-SEC
      *
      *      ¿©»ö¡¦Ì¤¼Â»Ü¸º»»¾ðÊó¡Ê¥·¥¹¥Æ¥à´ÉÍý¥Æ¡¼¥Ö¥ë¤è¤ê¡Ë
             INITIALIZE                   WRK-SHOKUJIRYOYO-OCC
                                          WRK-TOKUKANRIKASAN-OCC
      *
             INITIALIZE                      SYSKANRI-REC
             MOVE    "5000"              TO  SYS-KANRICD
             MOVE    "*"                 TO  SYS-KBNCD
             MOVE    WRK-PARA-SRYYM      TO  SYS-STYUKYMD (1:6)
             MOVE    "01"                TO  SYS-STYUKYMD (7:2)
             MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
grpsys       MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
             MOVE    SYSKANRI-REC        TO  MCPDATA-REC
             PERFORM 910-SYSKANRI-INV-SEC
             IF      FLG-SYSKANRI        =   ZERO
               MOVE     MCPDATA-REC      TO  SYS-5000-REC
               PERFORM  110-SYSKAN-HENSYU-SEC
               IF        WRK-SYS5000-COUNT    =     ZERO
                  CONTINUE
               ELSE
                  PERFORM  130-SYS-5000-GET-SEC
               END-IF
             ELSE
               MOVE "°åÎÅµ¡´Ø¾ðÊó¡ÝÆþ±¡´ðËÜ¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£"
                                      TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1              TO  FLG-END
             END-IF
      *
           ELSE
      *
             MOVE     ZERO               TO  SYSKAN-IDX
             MOVE     SPACE              TO  TBL-SYS5000-T
             INITIALIZE                      TBL-SYS5000-T
      *
             INITIALIZE                      SYSKANRI-REC
             MOVE    "5000"              TO  SYS-KANRICD
             MOVE    "*"                 TO  SYS-KBNCD
grpsys       MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
             MOVE    SYSKANRI-REC        TO  MCPDATA-REC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key8"              TO  MCP-PATHNAME
grpsys       PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_syskanri"  TO  MCP-TABLE
                 MOVE    "key8"          TO  MCP-PATHNAME
                 PERFORM 900-SYSKANRI-READ-SEC
                 PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                           OR    (SYSKAN-IDX   >=  100)
      *
                   MOVE    MCPDATA-REC  TO  SYS-5000-REC
                   ADD     1            TO  SYSKAN-IDX
                   MOVE   SYS-5000-REC  TO  TBL-SYS5000(SYSKAN-IDX)
      *
                   MOVE    "tbl_syskanri"   TO  MCP-TABLE
                   MOVE    "key8"           TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
                 END-PERFORM
             ELSE
                 MOVE "°åÎÅµ¡´Ø¾ðÊó¡ÝÆþ±¡´ðËÜ¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£"
                                        TO  WRK-RECEERR
                 PERFORM 500-ERR-HENSYU-SEC
                 MOVE    1              TO  FLG-END
             END-IF
             MOVE    "tbl_syskanri"     TO  MCP-TABLE
             MOVE    "key8"             TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
           END-IF
      *
           INITIALIZE                   SYSKANRI-REC
           MOVE    "2005"           TO  SYS-KANRICD
           MOVE    "01"             TO  SYS-KBNCD
           MOVE    WRK-PARA-SRYYM   TO  SYS-STYUKYMD (1:6)
           MOVE    "01"             TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM      TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC     TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI     =   ZERO
                MOVE  MCPDATA-REC   TO  SYS-200501-REC
           ELSE
                INITIALIZE              SYS-200501-REC
           END-IF
           IF      SYS-200501-COMMENT-NYUINRYO-KSIKBN
                                    =   SPACE
               MOVE    "1"   TO  SYS-200501-COMMENT-NYUINRYO-KSIKBN
           END-IF
      *
           OPEN    OUTPUT              RECE844-FILE
           CLOSE                       RECE844-FILE
      *
           OPEN    OUTPUT              RECE855-FILE
           CLOSE                       RECE855-FILE
      *
           OPEN    OUTPUT              RECE8A4-FILE
           CLOSE                       RECE8A4-FILE
      *
           OPEN    OUTPUT              RECE8B5-FILE
           CLOSE                       RECE8B5-FILE
      *
           OPEN    OUTPUT              RECE84-FILE
           OPEN    I-O                 RECE85-FILE
      *
           OPEN    OUTPUT              RECE8A-FILE
                                       RECE8B-FILE
      *
           OPEN    I-O                 RECE82-FILE
      *
           PERFORM    900-RECE82-READ-SEC
      *
           IF  FLG-END =  ZERO
              INITIALIZE  WRK-CHUKSNSRYCD-TABLE
              IF  RECE82-SRYYM  >= "201004"
                 PERFORM   140-NYUINKASAN-JYOGAI-CYUSYUTU-SEC
              END-IF
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥«¥ó¤ÎÊÔ½¸
      *****************************************************************
       110-SYSKAN-HENSYU-SEC                SECTION.
      *
           PERFORM   VARYING   IDX11  FROM  1  BY  1
                      UNTIL    IDX11   >   31
              EVALUATE    SYS-5000-SHOKUJIRYOYOCD
      *         Æþ±¡¿©»öÎÅÍÜ¡Ê£±¡Ë
                WHEN         "197000110"
                    MOVE     1      TO   WRK-SHOKUJIRYOYO(IDX11)
                    EVALUATE  SYS-5000-SHOKUKBN(1)
                      WHEN     "1"
                        MOVE  ZERO  TO   WRK-TOKUKANRIKASAN(IDX11)
                      WHEN     "2"
                        MOVE    1   TO   WRK-TOKUKANRIKASAN(IDX11)
                    END-EVALUATE
      *         Æþ±¡¿©»öÎÅÍÜ¡Ê£²¡Ë
                WHEN         "197000710"
                    MOVE     2      TO   WRK-SHOKUJIRYOYO(IDX11)
              END-EVALUATE
           END-PERFORM
      *
           .
       110-SYSKAN-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    ·îÆâ¤ËÊ£¿ô¤Î°åÎÅ´ü´Ö¾ðÊó¤¬¤Ê¤¤¤«¸¡º÷¡Ê°åÎÅµ¡´Ø¸º»»¤Î³ÎÇ§¡Ë  *
      ******************************************************************
       120-SYS-5000-CHK-SEC                 SECTION.
      *
           MOVE     ZERO               TO  WRK-SYS5000-COUNT
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    WRK-SRYYM-5000      TO  SYS-STYUKYMD (1:6)
           MOVE    "__"                TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key11"         TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                   MOVE    MCPDATA-REC TO  SYS-5000-REC
                   ADD     1     TO        WRK-SYS5000-COUNT
      *
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key11"         TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       120-SYS-5000-CHK-EXT.
           EXIT.
      *
      ******************************************************************
      *    ·îÆâ¤Î°åÎÅ´ü´Ö¾ðÊó¼èÆÀ¡Ê°åÎÅµ¡´Ø¸º»»¾ðÊó¡Ë                  *
      ******************************************************************
       130-SYS-5000-GET-SEC                 SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    WRK-SRYYM-5000      TO  SYS-STYUKYMD (1:6)
           MOVE    "__"                TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key11"         TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                 MOVE   MCPDATA-REC  TO  SYS-5000-REC
                 MOVE   SYS-5000-STYUKYMD(7:2)  TO  IDV
                 PERFORM    VARYING  IDX11  FROM  IDV  BY  1
                             UNTIL   IDX11    >   31
                      EVALUATE    SYS-5000-SHOKUJIRYOYOCD
      *                Æþ±¡¿©»öÎÅÍÜ¡Ê£±¡Ë
                       WHEN         "197000110"
                          MOVE     1      TO   WRK-SHOKUJIRYOYO(IDX11)
                          EVALUATE  SYS-5000-SHOKUKBN(1)
                            WHEN     "1"
                              MOVE  ZERO  TO   WRK-TOKUKANRIKASAN(IDX11)
                            WHEN     "2"
                              MOVE    1   TO   WRK-TOKUKANRIKASAN(IDX11)
                          END-EVALUATE
      *                Æþ±¡¿©»öÎÅÍÜ¡Ê£²¡Ë
                       WHEN         "197000710"
                          MOVE     2      TO   WRK-SHOKUJIRYOYO(IDX11)
                          MOVE    ZERO    TO   WRK-TOKUKANRIKASAN(IDX11)
                      END-EVALUATE
                 END-PERFORM
      *
                 MOVE    "tbl_syskanri"   TO   MCP-TABLE
                 MOVE    "key11"          TO   MCP-PATHNAME
                 PERFORM 900-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_syskanri"         TO   MCP-TABLE
           MOVE    "key11"                TO   MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       130-SYS-5000-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡ÎÁ²Ã»»½ü³°¥³¡¼¥ÉÃê½Ð
      *****************************************************************
       140-NYUINKASAN-JYOGAI-CYUSYUTU-SEC      SECTION.
      *
           MOVE   ZERO               TO  IDX14
      *
           INITIALIZE                    TNS-TENSU-REC
           MOVE   SPA-HOSPNUM        TO  TNS-HOSPNUM
           MOVE   RECE82-SRYYM       TO  TNS-YUKOSTYMD(1:6)
           MOVE   "01"               TO  TNS-YUKOSTYMD(7:2)
           MOVE   RECE82-SRYYM       TO  TNS-YUKOEDYMD(1:6)
           MOVE   "01"               TO  TNS-YUKOEDYMD(7:2)
           MOVE   TNS-TENSU-REC      TO  MCPDATA-REC
           MOVE   "tbl_tensu"        TO  MCP-TABLE
           MOVE   "key30"            TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC            =   ZERO
              MOVE    "tbl_tensu"    TO  MCP-TABLE
              MOVE    "key30"        TO  MCP-PATHNAME
              PERFORM  900-TENSU-READ-SEC
              PERFORM  UNTIL  (FLG-TENSU  =     1)
                        OR    (IDX14     >=    50)
                 IF   TNS-SRYCD  =  "190134270"
                                 OR "190135470"
                                 OR "190135670"
                   CONTINUE
                 ELSE
                   ADD    1          TO  IDX14
                   MOVE   TNS-SRYCD  TO  WRK-CHUKSNSRYCD(IDX14)
                 END-IF
      *
                 MOVE    "tbl_tensu" TO  MCP-TABLE
                 MOVE    "key30"     TO  MCP-PATHNAME
                 PERFORM  900-TENSU-READ-SEC
              END-PERFORM
           END-IF
           MOVE    "tbl_tensu"       TO  MCP-TABLE
           MOVE    "key30"           TO  MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       140-NYUINKASAN-JYOGAI-CYUSYUTU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 210-HENSYU-SEC
      *
           IF   FLG-END = ZERO
             PERFORM 900-RECE82-READ-SEC
      *
             IF  CNT-IN-COMMIT   =   300
                 MOVE  ZERO          TO      CNT-IN-COMMIT
                 PERFORM 900-DBCOMMIT-SEC
                 PERFORM 900-DBSTART-SEC
             END-IF
      *
      *      ½èÍýÃæ»ßÀßÄê½èÍý
             IF      CNT-IN-CANCEL       =   50
                 MOVE    ZERO                TO  CNT-IN-CANCEL
                 PERFORM 500-CANCEL-HENSYU-SEC
             END-IF
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ÊÔ½¸½èÍý
      *****************************************************************
       210-HENSYU-SEC                   SECTION.
      *
           INITIALIZE          WRK-DAY-TABLE
      *
           INITIALIZE          FLG-KHNTEN-JI-AREA
      *
           INITIALIZE          TBL-TENSUG-JI
      *
           INITIALIZE          WRK-SKJ200604-TABLE
      *
           INITIALIZE          WRK-NYULIFE-TABLE
      *
           INITIALIZE          WRK-TENTEKI2-TABLE
      *
           MOVE   ZERO   TO    FLG-HEYA
      *
           MOVE   ZERO   TO    WRK-KINGAKU-JI
      *
           MOVE   ZERO   TO    WRK-ZAITENNASHI
      *
           INITIALIZE          WRK-RECE82G-REC
      *
           PERFORM    290-TENTEKIMATOME-HENSYU-SEC
      *
           PERFORM    220-RECE84-HENSYU-SEC
           PERFORM    230-RECE84-HENSYU2-SEC
           PERFORM    240-RECE84-HENSYU3-SEC
           PERFORM    250-RECE84-HENSYU4-SEC
           PERFORM    260-FTN-SET-SEC
      *
           IF      RECE82-HKNKBN    =   "4"
             PERFORM  270-KHNTEN-JI-CLEAR-SEC
           END-IF                            
      *
           PERFORM    281-GENTEN-HENSYU-SEC
      *
           PERFORM    280-RECE82-REWRITE-SEC
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀÅ©¼êµ»²Ã»»¤Þ¤È¤áÊÔ½¸
      *****************************************************************
       290-TENTEKIMATOME-HENSYU-SEC      SECTION.
      *
           INITIALIZE                  ORCSTENTEK2AREA
           MOVE    RECE82-HOSPNUM  TO  LNK-ORCSTEN2-HOSPNUM
           MOVE    RECE82-PTID     TO  LNK-ORCSTEN2-PTID
           MOVE    RECE82-SRYYM    TO  LNK-ORCSTEN2-SRYYM
           MOVE    RECE82-HKNCOMBI TO  LNK-ORCSTEN2-HKNCOMBI
           CALL    "ORCSTENTEK2"  USING
                                  ORCSTENTEK2AREA
                                  SPA-AREA
           IF      LNK-ORCSTEN2-RC  =   ZERO
             IF   (LNK-ORCSTEN2-TENTEK-ERRCD  = ZERO)
             OR   (LNK-ORCSTEN2-CHUJOU-ERRCD  = ZERO)
               MOVE  1            TO  WRK-TENTEKI2-HENKYAKUARI
               MOVE  LNK-ORCSTEN2-OUT-AREA
                                  TO  WRK-TENTEKI2-HENKYAKU-AREA
             END-IF
           END-IF
      *
           .
       290-TENTEKIMATOME-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       220-RECE84-HENSYU-SEC                SECTION.
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE82-HOSPNUM       TO      ACCT-HOSPNUM
           MOVE    RECE82-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE82-PTID          TO      ACCT-PTID
           MOVE    RECE82-SRYYM         TO      ACCT-SRYYM
           MOVE    RECE82-HKNCOMBI      TO      ACCT-HKNCOMBI
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "tbl_sryacct"        TO      MCP-TABLE
           MOVE    "key22"              TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryacct"    TO      MCP-TABLE
               MOVE    "key22"          TO      MCP-PATHNAME
               PERFORM  900-SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
      *
                MOVE   ZERO          TO  FLG-RINYAK
      *
                IF    ACCT-SRYKBN       =  "95"  OR  "96"
                 CONTINUE
                ELSE
      *
                 MOVE     ZERO    TO    FLG-JYOGAI
                 IF    ACCT-SRYKBN  =  "90" OR "92"
                   PERFORM  VARYING  ID4    FROM   1   BY   1
                             UNTIL  (ID4                  >   50  )
                               OR   (WRK-CHUKSNSRYCD(ID4) =  SPACE)
                               OR   (FLG-JYOGAI           =   1   )
                     IF  ACCT-SRYCDTOTAL(6:9) = WRK-CHUKSNSRYCD(ID4)
                        MOVE     1    TO   FLG-JYOGAI
                     END-IF
                   END-PERFORM
                 END-IF
      *
      *          Ì¤¼Â»Ü¸º»»¡¦Æü¾ïÀ¸³è¾ã³²²Ã»»¡¦ÃÔÊò²Ã»»¡¦
      *          êóáì´µ¼Ô´ÉÍý²Ã»»¤ò½ü¤¯
                 IF ((ACCT-SRYCDTOTAL  =  190080470  OR  190799670
                                      OR  190080570  OR  190799770
                                      OR  190111690  OR  190809090
                                      OR  190111790  OR  190809190
                                      OR  190807570  OR  190808670
                                      OR  190116990  OR  190813390) AND
                     (RECE82-SRYYM     <= "200603"                ))
                 OR ((ACCT-SRYCDTOTAL  =  190109170  OR  190109270) AND
                     (RECE82-SRYYM     <= "200606"                ))
                 OR ((ACCT-SRYCDTOTAL  =  190117070  OR  190109670) AND
                     (RECE82-SRYYM     >= "200604"                ) AND
                     (RECE82-SRYYM     <= "200803"                ))
                 OR ((ACCT-SRYCDTOTAL  =  190121970  OR  190122670) AND
                     (RECE82-SRYYM     >= "200607"                ))
                 OR ((ACCT-SRYCDTOTAL  =  190119210               ) AND
                     (RECE82-SRYYM     >= "200804"                ))
                 OR ( FLG-JYOGAI       =  1                        )
                   CONTINUE
                 ELSE
      *
                   MOVE     ZERO    TO    FLG-TAISYO
                   IF   (ACCT-SRYKBN         =   "80" )
                   AND  (ACCT-ZAITEN         =   ZERO )
                   AND  (ACCT-ZAIKAISU   NOT =   ZERO )
                   AND  (ACCT-JIHOKBN        =   SPACE)
                     PERFORM  2204-SRYACCTPLUS-KENSAKU-SEC
                   END-IF
      *
                   MOVE     ZERO    TO    FLG-TENTEKIMATOME
                   IF    ACCT-SRYKBN  =  "33"
                     PERFORM  2205-TENTEKIMATOME-CHECK-SEC
                   END-IF
      *
                   IF  ((ACCT-ZAITEN    NOT =    ZERO ) AND
                        (ACCT-ZAIKAISU  NOT =    ZERO ) AND
                        (ACCT-JIHOKBN       =    SPACE) AND
                        (FLG-TENTEKIMATOME  =    ZERO ))
                   OR  ( FLG-TAISYO         =      1   )
      *
                       MOVE   ZERO   TO   WRK-TEN
                                          WRK-MEISAI-CNT
                                          WRK-SRYCDTOTAL
                                          WRK-SURYOUTOTAL
                                          WRK-YOUBUKASAN
                                          WRK-KYOBUKASAN
                                          WRK-KEIBUKASAN
                                          WRK-SOGAIKASAN
                                          WRK-RYOYOTEATE
      *
                       MOVE   ZERO   TO   WRK-SRYSURYO
                       MOVE   ZERO   TO   WRK-SRYSURYO2
                       MOVE   ZERO   TO   WRK-SRYSURYO3
                       MOVE   ZERO   TO   WRK-TANICD
                       MOVE   ZERO   TO   WRK-TANIMOJISU
                       MOVE   SPACE  TO   WRK-TANINAME
                       MOVE   ZERO   TO   FLG-HOUSIJI-COM
                                          FLG-SYOBYO-COM
                                          FLG-RYOYO-COM
                                          FLG-SYODOU-COM
                                          FLG-OSIN-COM
                                          FLG-HOUSEI-COM
                                          FLG-HOUMON-COM
                                          FLG-TAHOKEN
                                          FLG-SRYKBN-HEN
                                          FLG-KENSA-COM
      *
                       MOVE   ZERO   TO   FLG-WRITE85
                       MOVE   ZERO   TO   FLG-TENTEKI
                       IF  (ACCT-SRYKBN     = "97"     )
                       AND (ACCT-SRYCDTOTAL = 197000470
                                           OR 197001570
                                           OR 197001210)
                         IF  (RECE82-SRYYM   >= "200604" )
                         AND (ACCT-SRYCDTOTAL = 197000470
                                             OR 197001570)
                           PERFORM   2201-RECE84-TENSOU-SEC
                           PERFORM   2202-RECE85-HENSYU-SEC
                           IF    FLG-WRITE85  =   1
                               PERFORM 900-RECE84-WRITE-SEC
                           END-IF
                         END-IF
                       ELSE
                         PERFORM   2201-RECE84-TENSOU-SEC
                         PERFORM   2202-RECE85-HENSYU-SEC
                         IF    FLG-WRITE85  =   1
                             PERFORM 900-RECE84-WRITE-SEC
                         END-IF
                       END-IF
                   ELSE
                       IF  (ACCT-ZAIKAISU  NOT =    ZERO) AND
                           (ACCT-JIHOKBN       =   SPACE) AND
                           (FLG-TENTEKIMATOME  =    ZERO)
                            IF ( ACCT-SRYKBN  =  "11" OR
                                                 "33" OR "99" OR
                                                 "90" OR "92"  )
                            OR ((RECE82-SRYYM >= "200804"     ) AND
                                (ACCT-SRYKBN  =  "14"         ))
                            OR ((RECE82-SRYYM >= "200804"     ) AND
                                (ACCT-SRYKBN  =  "21" OR "22" OR
                                                 "23"         ) AND
                                (WRK-PARA-PRTKBN  =  "2"      ))
                            OR ( ACCT-ZAIKBN  =   1            )
                               IF  (ACCT-SRYCDTOTAL(1:12)  =
                                                  "000000999901")
                               AND (ACCT-MEISAISU      =   1    )
                                 CONTINUE
                               ELSE
                                 MOVE   ZERO   TO   WRK-TEN
                                                    WRK-MEISAI-CNT
                                                    WRK-SRYCDTOTAL
                                                    WRK-SURYOUTOTAL
                                                    WRK-YOUBUKASAN
                                                    WRK-KYOBUKASAN
                                                    WRK-KEIBUKASAN
                                                    WRK-SOGAIKASAN
                                                    WRK-RYOYOTEATE
      *
                                 MOVE   ZERO   TO   WRK-SRYSURYO
                                 MOVE   ZERO   TO   WRK-SRYSURYO2
                                 MOVE   ZERO   TO   WRK-SRYSURYO3
                                 MOVE   ZERO   TO   WRK-TANICD
                                 MOVE   ZERO   TO   WRK-TANIMOJISU
                                 MOVE   SPACE  TO   WRK-TANINAME
                                 MOVE   ZERO   TO   FLG-HOUSIJI-COM
                                                    FLG-SYOBYO-COM
                                                    FLG-RYOYO-COM
                                                    FLG-SYODOU-COM
                                                    FLG-OSIN-COM
                                                    FLG-HOUSEI-COM
                                                    FLG-HOUMON-COM
                                                    FLG-TAHOKEN
                                                    FLG-SRYKBN-HEN
                                                    FLG-KENSA-COM
      *
                                 MOVE   ZERO   TO   FLG-WRITE85
                                 MOVE   ZERO   TO   FLG-TENTEKI
                                 PERFORM   2201-RECE84-TENSOU-SEC
                                 PERFORM   2202-RECE85-HENSYU-SEC
                                 IF    FLG-WRITE85  =   1
                                    PERFORM 900-RECE84-WRITE-SEC
                                 END-IF
                               END-IF
                            ELSE
                               PERFORM   2203-SINRYO-CHECK-SEC
                               IF   FLG-RINYAK  NOT =  ZERO
                                 MOVE   ZERO   TO   WRK-TEN
                                                    WRK-MEISAI-CNT
                                                    WRK-SRYCDTOTAL
                                                    WRK-SURYOUTOTAL
                                                    WRK-YOUBUKASAN
                                                    WRK-KYOBUKASAN
                                                    WRK-KEIBUKASAN
                                                    WRK-SOGAIKASAN
                                                    WRK-RYOYOTEATE
      *
                                 MOVE   ZERO   TO   WRK-SRYSURYO
                                 MOVE   ZERO   TO   WRK-SRYSURYO2
                                 MOVE   ZERO   TO   WRK-SRYSURYO3
                                 MOVE   ZERO   TO   WRK-TANICD
                                 MOVE   ZERO   TO   WRK-TANIMOJISU
                                 MOVE   SPACE  TO   WRK-TANINAME
                                 MOVE   ZERO   TO   FLG-HOUSIJI-COM
                                                    FLG-SYOBYO-COM
                                                    FLG-RYOYO-COM
                                                    FLG-SYODOU-COM
                                                    FLG-OSIN-COM
                                                    FLG-HOUSEI-COM
                                                    FLG-HOUMON-COM
                                                    FLG-TAHOKEN
                                                    FLG-SRYKBN-HEN
                                                    FLG-KENSA-COM
      *
                                 MOVE   ZERO   TO   FLG-WRITE85
                                 MOVE   ZERO   TO   FLG-TENTEKI
                                 PERFORM   2201-RECE84-TENSOU-SEC
                                 PERFORM   2202-RECE85-HENSYU-SEC
                                 IF    FLG-WRITE85  =   1
                                    PERFORM 900-RECE84-WRITE-SEC
                                 END-IF
                               END-IF
                            END-IF
                       END-IF
                   END-IF
                 END-IF
                END-IF
                MOVE    "tbl_sryacct"    TO      MCP-TABLE
                MOVE    "key22"          TO      MCP-PATHNAME
                PERFORM  900-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryacct"         TO      MCP-TABLE
           MOVE    "key22"               TO      MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       220-RECE84-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       2201-RECE84-TENSOU-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE84-REC
           INITIALIZE                           RECE84-REC
           MOVE    RECE82-HOSPNUM       TO      RECE84-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE84-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE84-PTID
           MOVE    RECE82-RECEKA        TO      RECE84-RECEKA
           MOVE    RECE82-HKNID         TO      RECE84-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE84-TEKSTYMD
           MOVE    ACCT-ZAINUM          TO      RECE84-ZAINUM
           MOVE    ACCT-SRYKBN          TO      RECE84-SRYKBN
           IF      ACCT-SRYKBN = "90" OR "92"
                 MOVE    "6"            TO      RECE84-ZAISKBKBN
           ELSE 
              IF   ACCT-SRYKBN = "97"
      ***        MOVE    "4"            TO      RECE84-ZAISKBKBN
                 MOVE    "9"            TO      RECE84-ZAISKBKBN
              END-IF
           END-IF
           MOVE    RECE82-PTNUM         TO      RECE84-PTNUM
           MOVE    ACCT-SRYKA           TO      RECE84-SRYKA
           MOVE    ACCT-HKNCOMBI        TO      RECE84-HKNCOMBI
           MOVE    ACCT-JIHOKBN         TO      RECE84-JIHOKBN
           MOVE    ACCT-ZAITEN          TO      RECE84-ZAITEN
           MOVE    ACCT-SRYCDTOTAL      TO      RECE84-SRYCDTOTAL
           MOVE    ACCT-SURYOUTOTAL     TO      RECE84-SURYOUTOTAL
           MOVE    ACCT-MEISAISU        TO      RECE84-MEISAISU
           MOVE    ACCT-ZAIKAISU        TO      RECE84-ZAIKAISU
           MOVE    ACCT-SYUTEN1         TO      RECE84-SYUTEN1
           MOVE    ACCT-SYUTEN2         TO      RECE84-SYUTEN2
           MOVE    ACCT-YKZTEN          TO      RECE84-YKZTEN
           MOVE    ACCT-KIZAITEN        TO      RECE84-KIZAITEN
           IF   RECE82-HKNKBN     =  "4"
              IF  ACCT-SRYKBN     =  "70"
                  COMPUTE  RECE84-SYUTEN1   =  RECE84-SYUTEN1
                                            -  RECE84-SYUTEN2
                  COMPUTE  RECE84-KIZAITEN  =  RECE84-KIZAITEN
                                            +  RECE84-SYUTEN2
              END-IF
           END-IF
           MOVE    ACCT-DRCD            TO      RECE84-DRCD
           MOVE    ACCT-ZAIREQFLG       TO      RECE84-ZAIREQFLG
           MOVE    ACCT-TEIGENRATE      TO      RECE84-TEIGENRATE
           MOVE        ZERO             TO      WK-TAIHI-DAY
           MOVE        ZERO             TO      WK-TAIHI-FLG
           PERFORM     VARYING   IDX3   FROM    1    BY   1
                       UNTIL     IDX3   >       4
               PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >       31
                   MOVE    ACCT-DAY(IDX3 IDX2)  TO
                                            RECE84-KAISU (IDX3 IDX2)
                   IF     (WK-TAIHI-FLG            =   ZERO)   AND
                          (ACCT-DAY(1 IDX2)    NOT =   ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF    
               END-PERFORM
           END-PERFORM
      *
           MOVE      ZERO     TO    FLG-OKIKAE
           IF   ( RECE82-AGE       >=  75        )
           OR   ( RECE82-BIRTHDAY  <  "19321001" )
               PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL    ( IDX2        >    31 )
                             OR     ( FLG-OKIKAE  =     1 )
                  IF  ( RECE82-BYOTOSYUBETU(IDX2) =  "02" OR "03" )
                  AND ( RECE84-KAISU(1 IDX2)  NOT =  ZERO         )
                        MOVE   1   TO   FLG-OKIKAE
                  END-IF    
               END-PERFORM
           END-IF
      *
           IF    FLG-RINYAK    NOT =    ZERO
                MOVE    FLG-RINYAK   TO    RECE84-SAFLG
           END-IF
      *
           .
       2201-RECE84-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       2202-RECE85-HENSYU-SEC               SECTION.
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO      SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "tbl_sryact"         TO      MCP-TABLE
           MOVE    "key9"               TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key9"           TO      MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
      *
               MOVE     ZERO    TO    FLG-JYOGAI
               IF    ACCT-SRYKBN  =  "90" OR "92"
                 PERFORM  VARYING  ID4    FROM   1   BY   1
                           UNTIL  (ID4                  >   50  )
                             OR   (WRK-CHUKSNSRYCD(ID4) =  SPACE)
                             OR   (FLG-JYOGAI           =   1   )
                   IF  SRY-SRYCD(1) = WRK-CHUKSNSRYCD(ID4)
                      MOVE     1    TO   FLG-JYOGAI
                   END-IF
                 END-PERFORM
               END-IF
      *
      *        Ï«ºÒÆþ±¡¤Î²Ã»»ÂÐ¾Ý¥³¡¼¥É¤ò¿ÇÎÅ¹Ô°Ù¥Æ¡¼¥Ö¥ë¤ÇÈ½Äê
               IF ((SRY-SRYCD(1)  =  "190080470"  OR  "190799670"
                                 OR  "190080570"  OR  "190799770"
                                 OR  "190111690"  OR  "190809090"
                                 OR  "190111790"  OR  "190809190"
                                 OR  "190807570"  OR  "190808670"
                                 OR  "190116990"  OR  "190813390") AND
                   (RECE82-SRYYM     <= "200603"                 ))
               OR ((SRY-SRYCD(1)  =  "190109170"  OR  "190109270") AND
                   (RECE82-SRYYM     <= "200606"                 ))
               OR ((SRY-SRYCD(1)  =  "190117070"  OR  "190109670") AND
                   (RECE82-SRYYM     >= "200604"                 ) AND
                   (RECE82-SRYYM     <= "200803"                 ))
               OR ((SRY-SRYCD(1)  =  "190121970"  OR  "190122670") AND
                   (RECE82-SRYYM     >= "200607"                 ))
               OR ((SRY-SRYCD(1)  =  "190119210"                 ) AND
                   (RECE82-SRYYM     >= "200804"                 ))
               OR ( FLG-JYOGAI    =   1                           )
                 MOVE   ZERO         TO   FLG-WRITE85
               ELSE
                 PERFORM  UNTIL  FLG-SRYACT    =    1
                   MOVE    ZERO         TO   FLG-ZANHAIKI
                                             FLG-FORMAL-KISAI
                   MOVE    ZERO         TO   WRK-RENNUM
                                             WRK-RENNUM2
                   MOVE    ZERO         TO   FLG-WRITE85X
                   PERFORM   22021-RECE85-TENSOU-SEC
                   IF   FLG-TENTEKI  =  ZERO
                   AND  FLG-WRITE85X =   1
                     PERFORM 22022-RECE82-HENSYU-SEC
                     PERFORM 900-RECE85-WRITE-SEC
                     MOVE  1            TO   FLG-WRITE85
                   END-IF
                   IF   FLG-ZANHAIKI  =  1
                      PERFORM  900-RECE85-REWRITE-SEC
                   END-IF
                   IF  FLG-FORMAL-KISAI  =  1
                      PERFORM  900-RECE85-REWRITE2-SEC
                   END-IF
                   MOVE    "tbl_sryact" TO   MCP-TABLE
                   MOVE    "key9"       TO   MCP-PATHNAME
                   PERFORM  900-SRYACT-READ-SEC
                 END-PERFORM
               END-IF
           END-IF
           MOVE    "tbl_sryact"         TO   MCP-TABLE
           MOVE    "key9"               TO   MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       2202-RECE85-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       22021-RECE85-TENSOU-SEC                SECTION.
      *
           INITIALIZE          WRK-JIBAIKZI-TNS-TAIHI-TABLE
      *
           MOVE    SPACE                TO      RECE85-REC
           INITIALIZE                           RECE85-REC
           MOVE    RECE82-HOSPNUM       TO      RECE85-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE85-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE85-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE85-PTID
           MOVE    RECE82-RECEKA        TO      RECE85-RECEKA
           MOVE    RECE82-HKNID         TO      RECE85-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE85-TEKSTYMD
           MOVE    SRY-ZAINUM           TO      RECE85-ZAINUM
           MOVE    SRY-RENNUM           TO      RECE85-RENNUM
           MOVE    RECE82-PTNUM         TO      RECE85-PTNUM
      *
           MOVE    SRY-SRYKA            TO      RECE85-SRYKA
           MOVE    SRY-SRYKBN           TO      RECE85-SRYKBN
           MOVE    SRY-SRYSYUKBN        TO      RECE85-SRYSYUKBN
           MOVE    SRY-JIHIMONEYTOTAL   TO      RECE85-JIHIMONEY
      *
           MOVE    ZERO                 TO      IDX4
           MOVE    ZERO                 TO      FLG-OSINKBN
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                       UNTIL    (IDX2      >    5)    OR
                                (SRY-SRYCD(IDX2)  =   SPACE)
      *
      *     Ï·¿ÍÀº¿ÀÉÂÅïÅù¤Î¿ÇÎÅ¹Ô°Ù¥³¡¼¥ÉÃÖ¤­´¹¤¨
            IF   RECE82-SRYYM  >= "200604"
              CONTINUE
            ELSE
              IF   FLG-OKIKAE   =   1
                EVALUATE  SRY-SRYCD(IDX2)
      *           ·ì±Õ³ØÅª¸¡ººÈ½ÃÇÎÁ
                  WHEN  "160061810"
                      MOVE  "160703610"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  641800
      *           ¸¡ÂÎ¸¡ºº´ÉÍý²Ã»»¡Ê£±¡Ë
                  WHEN  "160170170"
                      MOVE  "160710070"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  539900
      *           ¸¡ÂÎ¸¡ºº´ÉÍý²Ã»»¡Ê£²¡Ë
                  WHEN  "160161610"
                      MOVE  "160709310"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  547700
      *           À¸²½³ØÅª¸¡ºº¡Ê£±¡ËÈ½ÃÇÎÁ
                  WHEN  "160061910"
                      MOVE  "160703710"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  641800
      *           À¸²½³ØÅª¸¡ºº¡Ê£²¡ËÈ½ÃÇÎÁ
                  WHEN  "160062010"
                      MOVE  "160703810"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  641800
      *           Ç¢¡¦ÊµÊØÅù¸¡ººÈ½ÃÇÎÁ
                  WHEN  "160061710"
                      MOVE  "160703510"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  641800
      *           ÈùÀ¸Êª³ØÅª¸¡ººÈ½ÃÇÎÁ
                  WHEN  "160062210"
                      MOVE  "160704010"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  641800
      *           ÌÈ±Ö³ØÅª¸¡ººÈ½ÃÇÎÁ
                  WHEN  "160062110"
                      MOVE  "160703910"  TO  SRY-SRYCD(IDX2)
                      COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                                  +  641800
                END-EVALUATE
              END-IF
            END-IF
      *
            MOVE    SRY-SRYCD(IDX2)    TO    WRK-SRYCD
            MOVE    RECE85-SRYYM       TO    WK-TAIHI-SRYYM
            MOVE    WK-TAIHI-DAY       TO    WK-TAIHI-SRYD
            PERFORM    900-TENSU-TBL-SEC
      *
            IF     ( SRY-SRYCD(IDX2)(1:1)  =  "S"   )
                  COMPUTE   RECE84-MEISAISU  =  RECE84-MEISAISU
                                             -  1
            END-IF
      *
            IF     ( SRY-SRYCD(IDX2)(1:7)  =  "0999901"    )
            OR     ( SRY-SRYCD(IDX2)(1:3)  =  "001"        )
            OR     ((SRY-SRYCD(IDX2)(1:3)  =  "059") AND
                    (TNS-TEN               =  ZERO )       )
            OR     ( SRY-SRYCD(IDX2)(1:7)  =  "0992000"    )
            OR     ( SRY-SRYCD(IDX2)       =  "099209903"
                                           OR "099209905"
                                           OR "099209906"  )
            OR     ( SRY-SRYCD(IDX2)(1:4)  =  "0085"
                                           OR "0086"       )
            OR     ( SRY-SRYCD(IDX2)       =  "099999903"
                                           OR "099999904"
                                           OR "099999905"
                                           OR "099999906"
                                           OR "099999907"  )
            OR     ((SRY-SRYSYUKBN  = "330" OR "331" OR
                                      "332" OR "350" OR
                                      "352"               ) AND
                    ((SRY-SRYCD(IDX2)(1:1) =   "8"       ) OR
                     (SRY-SRYCD(IDX2)(1:3) =   "008"     )) AND
                    ( ACCT-ZAITEN      NOT =   ZERO       ) AND
                    ( ACCT-SYUTEN1         =   ZERO       ) AND
                    ( ACCT-ZAIKAISU    NOT =   ZERO       ))
            OR     ((ACCT-SRYKBN = "40" OR "50" OR "54" OR
                                   "60" OR "64" OR "70" OR
                                   "80"                   ) AND
                    (ACCT-ZAITEN       NOT =  ZERO        ) AND
                    (ACCT-ZAIKAISU     NOT =  ZERO        ) AND
                    (ACCT-JIHOKBN          =  SPACE       ) AND
                    (ACCT-YKZTEN           =  ZERO        ) AND
                    (SRY-SRYCD(IDX2)(1:1)  =  "6"         ))
                  COMPUTE   RECE84-MEISAISU    =  RECE84-MEISAISU
                                               -  1
                  MOVE      SRY-SRYCD(IDX2)   TO  WRK-SRYCD
                  COMPUTE   RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                               -  WRK-SRYCD
                  IF ((ACCT-SRYKBN = "40" OR "50" OR "54" OR
                                     "60" OR "64" OR "70" OR
                                     "80"                   ) AND
                      (ACCT-ZAITEN       NOT =  ZERO        ) AND
                      (ACCT-ZAIKAISU     NOT =  ZERO        ) AND
                      (ACCT-JIHOKBN          =  SPACE       ) AND
                      (ACCT-YKZTEN           =  ZERO        ) AND
                      (SRY-SRYCD(IDX2)(1:1)  =  "6"         ))
                     COMPUTE   RECE84-SURYOUTOTAL =  RECE84-SURYOUTOTAL
                                                  -  SRY-SRYSURYO(IDX2)
                  ELSE
                     COMPUTE   RECE84-SURYOUTOTAL =  RECE84-SURYOUTOTAL
                                                  -  1
                  END-IF
            END-IF
      *
            IF      SRY-SRYCD(IDX2)(1:1)     =   "0" OR  "1" OR
                                                 "6" OR  "7" OR
                                                 "8"
      *
             IF  (  SRY-SRYSYUKBN            = "330" OR "331"  OR      
                                               "332" OR "350"  OR
                                               "352" )
             AND (( SRY-SRYCD(IDX2)(1:3)     = "059"             ) OR  
                  ( SRY-SRYCD(IDX2)(1:1)     = "7"               ) OR  
                  ( SRY-SRYCD(IDX2)          = CONS-ZANHAIKI     ) OR  
                  ( SRY-SRYCD(IDX2)(1:1)     = "6"               ))    
             AND (  ACCT-ZAITEN          NOT = ZERO  )
      *******AND (  ACCT-SYUTEN1             = ZERO  )
             AND (  ACCT-ZAIKAISU        NOT = ZERO  )
      *
              MOVE    1                  TO    FLG-TENTEKI             
      *
             ELSE                                                      
      *
              IF     ( SRY-SRYCD(IDX2)(1:7)  =  "0999901"    )
              OR     ( SRY-SRYCD(IDX2)(1:3)  =  "001"        )
              OR     ((SRY-SRYCD(IDX2)(1:3)  =  "059") AND
                      (TNS-TEN               =  ZERO )       )
              OR     ( SRY-SRYCD(IDX2)(1:7)  =  "0992000"    )
              OR     ( SRY-SRYCD(IDX2)       =  "099209903"
                                             OR "099209905"
                                             OR "099209906"  )
              OR     ( SRY-SRYCD(IDX2)(1:4)  =  "0085"
                                             OR "0086"       )
              OR     ( SRY-SRYCD(IDX2)       =  "099999903"
                                             OR "099999904"
                                             OR "099999905"
                                             OR "099999906"
                                             OR "099999907"  )
              OR     ((SRY-SRYSYUKBN  = "330" OR "331" OR
                                        "332" OR "350" OR
                                        "352"               ) AND
                      ((SRY-SRYCD(IDX2)(1:1) =   "8"       ) OR
                       (SRY-SRYCD(IDX2)(1:3) =   "008"     )) AND
                      ( ACCT-ZAITEN      NOT =   ZERO       ) AND
                      ( ACCT-SYUTEN1         =   ZERO       ) AND
                      ( ACCT-ZAIKAISU    NOT =   ZERO       ))
              OR     ((ACCT-SRYKBN = "40" OR "50" OR "54" OR
                                     "60" OR "64" OR "70" OR
                                     "80"                   ) AND
                      (ACCT-ZAITEN       NOT =  ZERO        ) AND
                      (ACCT-ZAIKAISU     NOT =  ZERO        ) AND
                      (ACCT-JIHOKBN          =  SPACE       ) AND
                      (ACCT-YKZTEN           =  ZERO        ) AND
                      (SRY-SRYCD(IDX2)(1:1)  =  "6"         ))
      *
               CONTINUE
      *
              ELSE
      *
               ADD     1                  TO    IDX4    
      *
               MOVE    SRY-SRYCD(IDX2)    TO    RECE85-SRYCD(IDX4)
      *
               IF      SRY-SRYCD(IDX2)    =     CONS-BYOITAIYO
                  EVALUATE  RECE82-HOSPSBT
                     WHEN     "1"
                          MOVE     "3"    TO    RECE82-HOSPSBT
                     WHEN     "2"
                          MOVE     "4"    TO    RECE82-HOSPSBT
                  END-EVALUATE
               END-IF
      *
               IF     SRY-SRYCD(IDX2)      =    CONS-GAIJINKEN
                  MOVE   "1"              TO    RECE82-KENTAIKASANFLG
                  MOVE   99               TO    RECE84-KNSJISGRPKBN
               END-IF
               IF     RECE84-KNSJISGRPKBN  =    ZERO
                 IF    TNS-KNSJISGRPKBN = 1 OR 2 OR 3 OR 4 OR 5 OR 6
                  MOVE  TNS-KNSJISGRPKBN  TO    RECE84-KNSJISGRPKBN
                 END-IF
               END-IF
      *
               IF    ( SRY-SRYKBN   =  "14" OR "21" OR "22" OR "23" )
               AND   ( RECE84-ZAITEN     =     ZERO                 )
               AND   ( RECE84-SAFLG      =     SPACE                )
               AND   ( ACCT-ZAIKBN   NOT =     1                    )
                  IF   SRY-SRYCD(IDX2)   =     "820000046"
                     MOVE     "1"         TO    RECE84-SAFLG
                  END-IF
               END-IF
      *
               IF    ( SRY-SRYKBN   =  "21" OR "22" OR "23" )
               AND   ( RECE84-ZAITEN     =     ZERO         )
               AND   ( ACCT-ZAIKBN   NOT =     1            )
                  IF     RECE84-SAFLG   NOT =  SPACE
                        MOVE   SPACE      TO    RECE84-TOYAKUFLG
                  ELSE
                    IF  (SRY-SRYCD(IDX2)(1:1)  =  "8"  )
                    OR  (SRY-SRYCD(IDX2)(1:3)  =  "008")
                      IF   RECE84-TOYAKUFLG = SPACE
                        MOVE    "2"       TO    RECE84-TOYAKUFLG
                      END-IF
                    ELSE
                        MOVE    "1"       TO    RECE84-TOYAKUFLG
                    END-IF
                  END-IF
               END-IF
      *
               IF    ( SRY-SRYKBN        =     "14" )
               AND   ( RECE84-ZAITEN     =     ZERO )
               AND   ( ACCT-ZAIKBN   NOT =     1    )
                  IF     RECE84-SAFLG   NOT =  SPACE
                        MOVE   SPACE      TO    RECE84-TOYAKUFLG
                  ELSE
                    IF  (SRY-SRYCD(IDX2)(1:1)  =  "8"  )
                    OR  (SRY-SRYCD(IDX2)(1:3)  =  "008")
                        IF   SRY-SRYSYUKBN  =  "148" OR "149"
                          EVALUATE  WRK-PARA-PRTKBN
                           WHEN      "1"
                             MOVE   "9"  TO    RECE84-TOYAKUFLG
                           WHEN      "2"
                             IF   RECE84-TOYAKUFLG = SPACE
                               MOVE "2"  TO    RECE84-TOYAKUFLG
                             END-IF
                          END-EVALUATE
                        END-IF
                    ELSE
                        EVALUATE  WRK-PARA-PRTKBN
                          WHEN      "1"
                            MOVE  "9"    TO    RECE84-TOYAKUFLG
                          WHEN      "2"
                            MOVE  "1"    TO    RECE84-TOYAKUFLG
                        END-EVALUATE
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
                       COMPUTE  RECE84-RUIKEITOTAL  =
                                RECE84-RUIKEITOTAL  +
                              ( WRK-FUKA-VALUE    * WRK-SRYCD)
                       COMPUTE  WRK-FUKA-KINGAKU    =
                                WRK-FUKA-KINGAKU    +
                              ((WRK-FUKA-VALUE^4) * FUKA-IDX
                                                  * SRY-SRYSURYO(IDX2))
                     END-IF
                   END-PERFORM
                   COMPUTE  RECE84-RUIKEITOTAL  =
                            RECE84-RUIKEITOTAL  +
                            WRK-FUKA-KINGAKU
      *          END-IF
               END-IF
      *
               MOVE    SRY-SRYSURYO(IDX2) TO    RECE85-SRYSURYO(IDX4)
               MOVE    SRY-SRYKAISU(IDX2) TO    RECE85-SRYKAISU(IDX4)
               MOVE    SRY-MEISKYFLG(IDX2) TO   RECE85-MEISKYFLG(IDX4)
               MOVE    SRY-AUTOKBN(IDX2)  TO    RECE85-AUTOKBN(IDX4)
               MOVE    SRY-INPUTNUM(IDX2) TO    RECE85-INPUTNUM(IDX4)
      *
               IF   RECE82-HKNKBN    NOT  =  "4"                        
                  IF ( SRY-SRYCD(IDX2)  =  CONS-ROUSAI2   OR
                                           CONS-ROUSAI4   OR
                                           CONS-ROUSAI5   OR
                                           CONS-ROUSAI6   OR
                                           CONS-ROUSAI7   OR
                                           CONS-ROUSAI8   OR
                                           CONS-ROUSAI9   OR
                                           CONS-ROUSAI10  OR
                                           CONS-ROUSAI11  OR
                                           CONS-ROUSAI12    )
                  OR ((SRY-SRYCD(IDX2)  =  CONS-ROUSAI3    ) AND
                      (RECE82-SRYYM     <  "200804"        ))
                          MOVE   "80"        TO    RECE84-SRYKBN
                                                   RECE85-SRYKBN
                  ELSE
                    IF      TNS-SRYKBN         =     "80"
                      IF  TNS-NYUTENTTLKBN   =     130
                          MOVE   "13"        TO    RECE84-SRYKBN
                                                   RECE85-SRYKBN
                      END-IF
                    END-IF
                  END-IF
               ELSE                                                     
                  IF      SRY-SRYCD(IDX2)    =     CONS-YOUBUKASAN
                   IF   RECE82-KOTEIKBN  =  SPACE  OR  "2"
                    COMPUTE  WRK-YOUBUKASAN   =  WRK-YOUBUKASAN
                                              +  TNS-TEN
                    COMPUTE  RECE84-SYUTEN1   =  RECE84-SYUTEN1
                                              -  TNS-TEN
                    COMPUTE  RECE84-KIZAITEN  =  RECE84-KIZAITEN
                                              +  TNS-TEN
                   END-IF
                  END-IF
                  IF      SRY-SRYCD(IDX2)    =     CONS-KYOBUKASAN
                   IF   RECE82-KOTEIKBN  =  SPACE  OR  "2"
                    COMPUTE  WRK-KYOBUKASAN   =  WRK-KYOBUKASAN
                                              +  TNS-TEN
                    COMPUTE  RECE84-SYUTEN1   =  RECE84-SYUTEN1
                                              -  TNS-TEN
                    COMPUTE  RECE84-KIZAITEN  =  RECE84-KIZAITEN
                                              +  TNS-TEN
                   END-IF
                  END-IF
                  IF      SRY-SRYCD(IDX2)    =     CONS-KEIBUKASAN
                   IF   RECE82-KOTEIKBN  =  SPACE  OR  "2"
                    COMPUTE  WRK-KEIBUKASAN   =  WRK-KEIBUKASAN
                                              +  TNS-TEN
                    COMPUTE  RECE84-SYUTEN1   =  RECE84-SYUTEN1
                                              -  TNS-TEN
                    COMPUTE  RECE84-KIZAITEN  =  RECE84-KIZAITEN
                                              +  TNS-TEN
                   END-IF
                  END-IF
                  IF      SRY-SRYCD(IDX2)    =     CONS-SOGAIKASAN
                   IF   RECE82-KOTEIKBN  =  SPACE  OR  "2"
                    COMPUTE  WRK-SOGAIKASAN   =  WRK-SOGAIKASAN
                                              +  TNS-TEN
                    COMPUTE  RECE84-SYUTEN1   =  RECE84-SYUTEN1
                                              -  TNS-TEN
                    COMPUTE  RECE84-KIZAITEN  =  RECE84-KIZAITEN
                                              +  TNS-TEN
                   END-IF
                  END-IF
      *           ¼«ÇåÀÕ¡¦ÎÅÍÜÃ´Åö¼êÅöÍÑÆÃÎã½èÍý
      *           ¡Ê¼«ÇåÀÕ¤Î¾ì¹ç¡¢£±£°ÅÀ¤ò£±£²£°±ß¤ËÆÉ¤ßÂØ¤¨¤ë¡Ë
      *           ¡ÊÎÅÍÜÃ´Åö¼êÅö¡§£±·î£±²ó¤Î»»Äê¡Ë
                  IF      SRY-SRYCD(IDX2)    =     CONS-RYOYOTEATE
                    EVALUATE  RECE82-JIBAIKBN
                      WHEN      "1"
                        MOVE     120    TO     WRK-RYOYOTEATE2
                        COMPUTE  WRK-RYOYOTEATE   =
                                 WRK-RYOYOTEATE   +
                                 WRK-RYOYOTEATE2
                      WHEN      "2"
                        COMPUTE  WRK-RYOYOTEATE   =  WRK-RYOYOTEATE
                                                  +  TNS-TEN
                        COMPUTE  RECE84-SYUTEN1   =  RECE84-SYUTEN1
                                                  -  TNS-TEN
                        COMPUTE  RECE84-KIZAITEN  =  RECE84-KIZAITEN
                                                  +  TNS-TEN
                    END-EVALUATE
                  END-IF
      *
                  IF ( ACCT-SRYKBN          =  "80"               ) AND
                     ( SRY-SRYCD(IDX2)(1:5) =  "09591" OR "09592" )
                     EVALUATE  SRY-SRYCD(IDX2)(1:5)
                      WHEN      "09591"
                        IF  RECE82-SINDAN-TUU < 2
                          MOVE   "95"   TO    RECE84-SRYKBN
                                              RECE85-SRYKBN
                        END-IF
                      WHEN      "09592"
                        IF  RECE82-MEISAI-TUU < 2
                          MOVE   "95"   TO    RECE84-SRYKBN
                                              RECE85-SRYKBN
                        END-IF
                     END-EVALUATE
                     MOVE    1          TO    FLG-SRYKBN-HEN
                  ELSE
                   IF ( SRY-SRYCD(IDX2)  =  CONS-ROUSAI2   OR
                                            CONS-ROUSAI4   OR
                                            CONS-ROUSAI5   OR
                                            CONS-ROUSAI6   OR
                                            CONS-ROUSAI7   OR
                                            CONS-ROUSAI8   OR
                                            CONS-ROUSAI9   OR
                                            CONS-ROUSAI10  OR
                                            CONS-ROUSAI11  OR
                                            CONS-ROUSAI12  OR
                                            CONS-ROUSAI13  OR
                                            CONS-ROUSAI14  OR
                                            CONS-ROUSAI15  OR
                                            CONS-ROUSAI16  OR
                                            CONS-ROUSAI17  OR
                                            CONS-ROUSAI18  OR
                                            CONS-ROUSAI19  OR
                                            CONS-ROUSAI20    )
                   OR ( SRY-SRYCD(IDX2)(1:5) = "09593"       )
                   OR ( SRY-SRYCD(IDX2)(1:5) = "09594"       )
                   OR ((SRY-SRYCD(IDX2)  =  CONS-ROUSAI3    ) AND
                       (RECE82-SRYYM     <  "200804"        ))
                   OR ((RECE82-JIBAIKBN  =  "1"             ) AND
                       (SRY-SRYCD(IDX2)  =  CONS-RYOYOTEATE ))
                   OR ( SRY-SRYCD(IDX2)  >= "101910110" AND
                                         <= "101910400"      )
                     IF   SRY-SRYCD(IDX2)(1:5) =  "09593"
                       MOVE  TNS-KANANAMECHGKBN
                                      TO  WRK-JIBAIKZI-SYUKEIKBN(IDX2)
                                          RECE84-JIBAISYUKEIKBN
                       EVALUATE  TNS-KANANAMECHGKBN
                         WHEN      0
                         WHEN      4
                         WHEN      5
                            MOVE   "80"   TO  RECE84-SRYKBN
                                              RECE85-SRYKBN
                         WHEN      1
                            MOVE   "11"   TO  RECE84-SRYKBN
                                              RECE85-SRYKBN
                       END-EVALUATE
                     ELSE
                       MOVE   "80"        TO  RECE84-SRYKBN
                                              RECE85-SRYKBN
                     END-IF
                     MOVE      1          TO  FLG-SRYKBN-HEN
                   ELSE                                                 
                     IF     TNS-SRYKBN         =   "80"
                       IF   TNS-NYUTENTTLKBN   =   130
                            MOVE   "13"   TO  RECE84-SRYKBN
                                              RECE85-SRYKBN
                       END-IF
                     END-IF
                     IF  (WRK-PARA-JIBAIPRTKBN =  "0" )
                     AND (FLG-SRYKBN-HEN       =  ZERO)
                       IF (( RECE84-SRYKBN     =  "80" ) AND
                           ( RECE84-OKIKAEFLG  =  SPACE))
                       OR (  RECE84-OKIKAEFLG  =  "1"   )
                        IF   SRY-SRYCD(IDX2)   =  CONS-RYOYOTEATE
                            CONTINUE
                        ELSE
                            MOVE   "1"    TO  RECE84-OKIKAEFLG
                            MOVE   "40"   TO  RECE84-SRYKBN       
                                              RECE85-SRYKBN       
                        END-IF
                       END-IF
                     END-IF
                     IF (( RECE84-SRYKBN     =  "14" ) AND
                         ( RECE84-OKIKAEFLG  =  SPACE))
                     OR (  RECE84-OKIKAEFLG  =  "2"   )
                            MOVE   "2"    TO  RECE84-OKIKAEFLG
                            MOVE   "80"   TO  RECE84-SRYKBN       
                                              RECE85-SRYKBN       
                     END-IF
                   END-IF                                               
                  END-IF                                                
      *
               END-IF                                                   
      *
               PERFORM  220211-RECE85-TENSOU-COMCNT-HEN-SEC
      *
               IF  SRY-SRYCD(IDX2)        =     CONS-ZANHAIKI           
                IF   ACCT-ZAIKBN  =  1
                  MOVE    "1"             TO  RECE85-RECEPTFLG(IDX4)
                  MOVE    ZERO            TO  RECE85-SRYSURYO(IDX4)
                ELSE
                  COMPUTE WRK-SRYSURYO2   =     WRK-SRYSURYO            
                                          +     0.99999                   
                  COMPUTE RECE85-SRYSURYO(IDX4)                         
                                          =     WRK-SRYSURYO2           
                                          -     WRK-SRYSURYO            
                  IF  RECE85-SRYSURYO(IDX4)  NOT = ZERO                 
                    MOVE  WRK-TANICD      TO    RECE85-TANICD(IDX4)     
                    MOVE  WRK-TANIMOJISU  TO    RECE85-TANIMOJISU(IDX4) 
                    MOVE  WRK-TANINAME    TO    RECE85-TANINAME(IDX4)   
                    IF    RECE85-RENNUM  >  1                           
                    AND   IDX4           =  1                           
                      MOVE    1           TO    FLG-ZANHAIKI            
                      COMPUTE  WRK-RENNUM  =  RECE85-RENNUM  -  1       
                      MOVE WRK-SRYSURYO2  TO  WRK-SRYSURYO3             
                    ELSE                                                
                      MOVE WRK-SRYSURYO2  TO  RECE85-SRYSURYO(IDX4 - 1) 
                    END-IF
                  ELSE
                    MOVE  "1"             TO  RECE85-RECEPTFLG(IDX4)
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
                  MOVE    TNS-TANICD      TO    RECE85-TANICD(IDX4)
                                                WRK-TANICD              
                  MOVE    TNS-TANIMOJISU  TO    RECE85-TANIMOJISU(IDX4)
                                                WRK-TANIMOJISU          
                  MOVE    TNS-TANINAME    TO    RECE85-TANINAME(IDX4)
                                                WRK-TANINAME            
               END-IF                                                   
               IF ( ACCT-SRYKBN          =  "80"              ) AND     
                  ( SRY-SRYCD(IDX2)(1:5) =  "09591" OR "09592"
                                         OR "09593" OR "09594")         
                    MOVE  1               TO  RECE85-TENSIKIBETU(IDX4)  
                    IF     (RECE82-HKNKBN        =  "4"               )
                    AND    (SRY-SRYCD(IDX2)(1:5) =  "09591" OR "09592"
                                                            OR "09593")
                      IF    SRY-SRYCD(IDX2)(1:5) =  "09593"
                        PERFORM   220212-JIBAI-KIZAI-TENSU-HENSYU-SEC
                        MOVE  WRK-TEN-KZI TO  RECE85-TEN(IDX4)
                        MOVE  WRK-TEN-KZI TO  WRK-JIBAIKZI-TNS(IDX2)
                      ELSE
                        PERFORM   220214-JIBAI-BUNSYO-TENSU-HENSYU-SEC
                        MOVE  WRK-TEN-BUN TO  RECE85-TEN(IDX4)
                      END-IF
                    ELSE
                        MOVE  TNS-TEN     TO  RECE85-TEN(IDX4)
                    END-IF
               ELSE                                                     
                 IF   RECE82-HKNKBN     =  "4"
                 AND  RECE82-JIBAIKBN   =  "1"
                 AND  SRY-SRYCD(IDX2)   =  CONS-RYOYOTEATE
                    MOVE  1               TO  RECE85-TENSIKIBETU(IDX4)  
                    MOVE  WRK-RYOYOTEATE2 TO  RECE85-TEN(IDX4)
                 ELSE
                    MOVE  TNS-TENSIKIBETU TO  RECE85-TENSIKIBETU(IDX4)
                    MOVE  TNS-TEN         TO  RECE85-TEN(IDX4)
                 END-IF
               END-IF                                                   
               MOVE    TNS-NYUTENTTLKBN   TO    RECE85-TENTTLKBN(IDX4)
               IF     (SRY-SRYCD    (IDX2)(1:1)     =   "8"  )
               OR     (SRY-SRYCD    (IDX2)(1:3)     =   "008")
                  MOVE ZERO               TO    RECE85-YUKOKETA(IDX4)
               ELSE
                  MOVE TNS-YUKOKETA       TO    RECE85-YUKOKETA(IDX4)
               END-IF
               MOVE    TNS-KENTAICOMMENT  TO    RECE85-KENTAICOMMENT
                                                               (IDX4)
               MOVE    TNS-DATAKBN        TO    RECE85-DATAKBN (IDX4)
               MOVE    TNS-HOUKNSKBN      TO    RECE85-HOUKNSKBN(IDX4)
               MOVE    TNS-KNSJISKBN      TO    RECE85-KNSJISKBN(IDX4)
               MOVE    TNS-CDKBN-KBN      TO    RECE85-CDKBN-KBN(IDX4)
               MOVE    TNS-CDKBN-SYO      TO    RECE85-CDKBN-SYO(IDX4)
               MOVE    TNS-CDKBN-BU       TO    RECE85-CDKBN-BU(IDX4)
               MOVE    TNS-CDKBN-KBNNUM   
                                      TO RECE85-CDKBN-KBNNUM(IDX4)
               MOVE    TNS-CDKBN-KBNNUM-EDA
                                      TO RECE85-CDKBN-KBNNUM-EDA(IDX4)
               MOVE    TNS-CDKBN-KOUBAN
                                      TO RECE85-CDKBN-KOUBAN(IDX4)
               MOVE    TNS-KOUHYOJYUNNUM  
                                      TO RECE85-KOUHYOJYUNNUM(IDX4)
               MOVE    TNS-KZMCOMPSIKIBETU
                                      TO RECE85-KZMCOMPSIKIBETU(IDX4)
               MOVE    TNS-KNSJISGRPKBN
                                      TO RECE85-KNSJISGRPKBN   (IDX4)
      *
               IF    (TNS-SRYCD       =  "170022290"  OR
                                         "170022390"  OR
                                         "170022490")
               AND   (RECE82-SRYYM   <=  "201003"   )
                      MOVE   202      TO  RECE85-CDKBN-KBNNUM(IDX4)
                      MOVE   99       TO  RECE85-CDKBN-KOUBAN(IDX4)
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
                                         "170026810"
                      MOVE   203      TO  RECE85-CDKBN-KBNNUM(IDX4)
                      MOVE   99       TO  RECE85-CDKBN-KOUBAN(IDX4)
               END-IF
      *
               IF    (TNS-SRYCD       =  "170022290")
               AND   (RECE82-SRYYM   >=  "201004"   )
                      MOVE   "1"      TO  RECE84-GAZOGEN
               END-IF
      *
               IF    ((SRY-SRYCD    (IDX2)(1:1)     =   "8"  )  OR
                      (SRY-SRYCD    (IDX2)(1:3)     =   "008"))
               AND   ( SRY-INPUTNUM (IDX2)      NOT =   ZERO  )
                   MOVE    SRY-HOSPNUM          TO  PTCOM-HOSPNUM
                   MOVE    SRY-PTID             TO  PTCOM-PTID
                   MOVE    SRY-ZAINUM           TO  PTCOM-ZAINUM
                   MOVE    SRY-SRYCD (IDX2)     TO  PTCOM-SRYCD
                   MOVE    SRY-INPUTNUM(IDX2)   TO  PTCOM-RENNUM
                   PERFORM 900-PTCOM-READ-SEC
                   MOVE    PTCOM-INPUTCOMENT   TO    RECE85-NAME(IDX4)
      *
      ***          MOVE   "1"                  TO    RECE84-COMCDFLG
               ELSE
                   MOVE    TNS-NAME            TO    RECE85-NAME(IDX4)
               END-IF
      *
               IF    (SRY-SRYCD    (IDX2)(1:1)     =   "8"  )  OR
                     (SRY-SRYCD    (IDX2)(1:3)     =   "008")
                        MOVE     "1"       TO    RECE84-COMCDFLG
               END-IF
      *
               IF  ( SRY-SRYKBN     =  "60" OR "64" )
               AND ((SRY-SRYCD(IDX2)(1:1)  =  "8"  ) OR
                    (SRY-SRYCD(IDX2)(1:3)  =  "008"))
               AND ( FLG-KENSA-COM  =   ZERO        )
                 IF  SRY-SRYCD(IDX2) = "840000085" OR "840000087"
                    CONTINUE
                 ELSE
                    PERFORM  VARYING  IDV2  FROM  1  BY  2
                              UNTIL  (IDV2           >   200)
                               OR    (FLG-KENSA-COM  =    1 )
                      IF  RECE85-NAME(IDX4)(IDV2:2)   =  "Æü"
                        MOVE   SPACE  TO  RECE84-FLG-KENSA-COMMENT
                        MOVE     1    TO  FLG-KENSA-COM
                      END-IF
                    END-PERFORM
                 END-IF
               END-IF
      *
               IF  SRY-SRYCD(IDX2)(1:3)     =   "058"
                 MOVE  TNS-FORMALYUKOKETA TO  RECE85-YUKOKETA(IDX4)
                 MOVE  TNS-FORMALNAME     TO  RECE85-NAME(IDX4)
               END-IF
               IF  (SRY-SRYCD(IDX2)(1:1)    =   "7")
               AND (SRY-MEISKYFLG(IDX2)     =   "1")
                 IF   (SRY-SRYKBN  =   "70")
                 AND  (TNS-DATAKBN =   "3" )
                    IF    RECE85-RENNUM  >  1
                    AND   IDX4           =  1
                      MOVE    1           TO  FLG-FORMAL-KISAI
                      COMPUTE  WRK-RENNUM2 =  RECE85-RENNUM  -  1
                    ELSE
                      MOVE  "1"           TO  RECE85-RECEPTFLG(IDX4 - 1)
                    END-IF
                 ELSE
                    MOVE  "1"             TO  RECE85-RECEHENFLG(IDX4)
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
                  MOVE     "1"          TO  RECE85-RECEPTFLG(IDX4)
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
                    MOVE  SPACE TO RECE85-NAME(IDX4 - 1)
                    COMPUTE   IDW4  =  IDW3  -  14
                    MOVE  TNS-NAME(15:IDW4)
                                TO RECE85-NAME(IDX4 - 1)(1:IDW4)
                    COMPUTE   IDW5  =  IDW4  +   1
                    MOVE  "¤Ë¤Æ¿Ç»¡ÎÁ»»ÄêºÑ"
                                TO RECE85-NAME(IDX4 - 1)(IDW5:16)
                    COMPUTE   RECE85-YUKOKETA(IDX4 - 1) =
                              RECE85-YUKOKETA(IDX4 - 1) -
                              3                         +
                              IDW6
                  END-IF
               END-IF
      *
               IF  RECE85-YUKOKETA(IDX4)  =  ZERO
                   PERFORM   VARYING   IDW   FROM   200   BY  -1
                           UNTIL   IDW   <   1
                       IF   RECE85-NAME(IDX4)(IDW:1)  NOT =   SPACE
                           COMPUTE RECE85-YUKOKETA(IDX4)  =
                                   IDW     /   2
                           MOVE    1          TO  IDW
                       END-IF
                   END-PERFORM
               END-IF
      *
               IF (  SRY-SRYCD(IDX2) = CONS-ROUSAI1  OR
                                       CONS-ROUSAI2  OR
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
                                       CONS-ROUSAI17 OR
                                       CONS-ROUSAI18 OR
                                       CONS-ROUSAI19 OR
                                       CONS-ROUSAI20 OR
                                       CONS-ROUSAI21   )                
               OR (( ACCT-SRYKBN          =  "80"     )   AND           
                   ( SRY-SRYCD(IDX2)(1:5) =  "09591"
                                         OR  "09592"
                                         OR  "09593"
                                         OR  "09594"  ))                
               OR (( SRY-SRYCD(IDX2) = CONS-ROUSAI3   )   AND
                   ( RECE82-SRYYM    < "200804"       ))
               OR (  SRY-SRYCD(IDX2) >= "101910110" AND
                                     <= "101910400"    )
      *
                     ADD      1          TO  WRK-MEISAI-CNT
                     COMPUTE  WRK-TEN    =   WRK-TEN   +
                                           ( TNS-TEN   /  10 ) 
      *
                     ADD      1          TO  WRK-SURYOUTOTAL
                     MOVE  SRY-SRYCD(IDX2) 
                                         TO  WRK-SRYCD
                     ADD   WRK-SRYCD     TO  WRK-SRYCDTOTAL
      *
                     MOVE     "2"        TO    RECE85-SIKFLG(IDX4)
      *
               ELSE
      *
                  IF   RECE82-HKNKBN     =  "4"
                  AND  RECE82-JIBAIKBN   =  "1"
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
                     MOVE     "2"        TO  RECE85-SIKFLG(IDX4)
      *
                  ELSE
      *
                    IF  ((RECE82-HKNKBN      =   "4") AND
                         (RECE82-JIBAIKBN    =   "2"))
                    OR  ((RECE82-HKNKBN      =   "5") AND
                         (RECE82-KOMUKBN     =   "2"))
                      IF (( ACCT-SRYKBN          =  "80"    ) AND
                          ( SRY-SRYCD(1)(1:5)    =  "09591"
                                                 OR "09592"
                                                 OR "09593"
                                                 OR "09594" ) AND
                          ( SRY-SRYCD(IDX2)(1:1) =   "8"    ))
                      OR (( ACCT-SRYKBN          =  "80"    ) AND
                          ( SRY-SRYCD(1)(1:5)    =  "09591"
                                                 OR "09592"
                                                 OR "09593"
                                                 OR "09594" ) AND
                          ( SRY-SRYCD(IDX2)(1:3) =   "008"  ))
      *
                         ADD    1      TO    WRK-MEISAI-CNT             
                         MOVE  "2"     TO    RECE85-SIKFLG(IDX4)
      *
                         ADD    1      TO    WRK-SURYOUTOTAL
                         MOVE  SRY-SRYCD(IDX2) 
                                       TO    WRK-SRYCD
                         ADD   WRK-SRYCD 
                                       TO    WRK-SRYCDTOTAL
                         IF  ( RECE84-COMCDFLG       =   "1"   )
                         AND ((SRY-SRYCD(IDX2)(1:1)  =   "8"  ) OR
                              (SRY-SRYCD(IDX2)(1:3)  =   "008"))
                            MOVE  "2"  TO    RECE84-COMCDFLG
                         END-IF
                      ELSE
                         MOVE  "1"     TO    RECE85-SIKFLG(IDX4)
                      END-IF
                    ELSE
                      IF  ( SRY-SRYCD(IDX2) = CONS-ROUCOM1  OR
                                              CONS-ROUCOM2  OR
                                              CONS-ROUCOM3  OR
                                              CONS-ROUCOM4  OR
                                              CONS-ROUCOM5  OR
                                              CONS-ROUCOM6  OR
                                              CONS-ROUCOM7      )
                      OR  ( SRY-SRYCD(1)    = CONS-ROUSAI2      )
                      OR (( ACCT-SRYKBN     = "11"              ) AND
                          ( SRY-SRYCD(IDX2)(1:1)   =   "8"      ) AND
                          ( SRY-SRYCD(IDX2) NOT = "830000020"   ) AND
                          ( ACCT-ZAIKBN     NOT =  1 AND 2      ))
                      OR (( ACCT-SRYKBN     = "11"              ) AND
                          ( SRY-SRYCD(IDX2)(1:3)   =   "008"    ) AND
                          ( ACCT-ZAIKBN     NOT =  1 AND 2      ))
                      OR (( ACCT-SRYKBN     = "13"              ) AND
                          ( SRY-SRYCD(1)    =  CONS-ROUSAI4  OR
                                               CONS-ROUSAI5  OR
                                               CONS-ROUSAI6  OR
                                               CONS-ROUSAI7  OR
                                               CONS-ROUSAI8  OR
                                               CONS-ROUSAI9  OR
                                               CONS-ROUSAI10 OR
                                               CONS-ROUSAI11 OR
                                               CONS-ROUSAI12    ) AND
                          ( SRY-SRYCD(IDX2)(1:3)   =   "008"    ))
                      OR (( ACCT-SRYKBN     = "13"              ) AND
                          ( SRY-SRYCD(1)    =  CONS-ROUSAI4  OR
                                               CONS-ROUSAI5  OR
                                               CONS-ROUSAI6  OR
                                               CONS-ROUSAI7  OR
                                               CONS-ROUSAI8  OR
                                               CONS-ROUSAI9  OR
                                               CONS-ROUSAI10 OR
                                               CONS-ROUSAI11 OR
                                               CONS-ROUSAI12    ) AND
                          ( SRY-SRYCD(IDX2)(1:1)   =   "8"      ))
                      OR (( ACCT-SRYKBN     = "13"              ) AND
                          ( SRY-SRYCD(1)    =  CONS-ROUSAI3     ) AND
                          ((SRY-SRYCD(IDX2)(1:3)   =   "008") OR
                           (SRY-SRYCD(IDX2)(1:1)   =   "8"  )   ) AND
                          ( RECE82-SRYYM    <  "200804"         ))
                      OR (( ACCT-SRYKBN            =  "80"      ) AND
                          ( SRY-SRYCD(1)(1:5)      =  "09591"
                                                   OR "09592"
                                                   OR "09593"
                                                   OR "09594"   ) AND
                          ( SRY-SRYCD(IDX2)(1:1)   =   "8"      ))
                      OR (( ACCT-SRYKBN            =  "80"      ) AND
                          ( SRY-SRYCD(1)(1:5)      =  "09591"
                                                   OR "09592"
                                                   OR "09593"
                                                   OR "09594"   ) AND
                          ( SRY-SRYCD(IDX2)(1:3)   =   "008"    ))
                      OR (( ACCT-SRYKBN     = "80"              ) AND
                          ( SRY-SRYCD(1)    =  CONS-ROUSAI13  OR
                                               CONS-ROUSAI14  OR
                                               CONS-ROUSAI15  OR
                                               CONS-ROUSAI16  OR
                                               CONS-ROUSAI17  OR
                                               CONS-ROUSAI18  OR
                                               CONS-ROUSAI19  OR
                                               CONS-ROUSAI20    ) AND
                          ( SRY-SRYCD(IDX2)(1:3)   =   "008"    ))
                      OR (( ACCT-SRYKBN     = "80"              ) AND
                          ( SRY-SRYCD(1)    =  CONS-ROUSAI13  OR
                                               CONS-ROUSAI14  OR
                                               CONS-ROUSAI15  OR
                                               CONS-ROUSAI16  OR
                                               CONS-ROUSAI17  OR
                                               CONS-ROUSAI18  OR
                                               CONS-ROUSAI19  OR
                                               CONS-ROUSAI20    ) AND
                          ( SRY-SRYCD(IDX2)(1:1)   =   "8"      ))
                      OR (( ACCT-SRYKBN     = "80"              ) AND
                          ( SRY-SRYCD(1)    >= "101910110" AND
                                            <= "101910400"      ) AND
                          ( SRY-SRYCD(IDX2)(1:3)   =   "008"    ))
                      OR (( ACCT-SRYKBN     = "80"              ) AND
                          ( SRY-SRYCD(1)    >= "101910110" AND
                                            <= "101910400"      ) AND
                          ( SRY-SRYCD(IDX2)(1:1)   =   "8"      ))
      *
                         ADD    1      TO    WRK-MEISAI-CNT             
                         MOVE  "2"     TO    RECE85-SIKFLG(IDX4)
      *
                         ADD    1      TO    WRK-SURYOUTOTAL
                         MOVE  SRY-SRYCD(IDX2) 
                                       TO    WRK-SRYCD
                         ADD   WRK-SRYCD 
                                       TO    WRK-SRYCDTOTAL
                         IF  ( RECE84-COMCDFLG       =   "1"   )
                         AND ((SRY-SRYCD(IDX2)(1:1)  =   "8"  ) OR
                              (SRY-SRYCD(IDX2)(1:3)  =   "008"))
                            MOVE  "2"  TO    RECE84-COMCDFLG
                         END-IF
                      ELSE
                         MOVE  "1"     TO    RECE85-SIKFLG(IDX4)
                      END-IF
                    END-IF
      *
                  END-IF
      *
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
                       PERFORM 220213-RECE85-TENSOU-TANKA-HEN-SEC
                       COMPUTE IDW =   RECE85-YUKOKETA(IDX4)  *  2 
                                                   +   3                    
                       MOVE    WRK-CHAR-N (1:WRK-CHAR-N-CNT)
                                                   TO
                                               RECE85-NAME(IDX4)(IDW:)   
                       COMPUTE RECE85-YUKOKETA(IDX4)
                                               =   RECE85-YUKOKETA(IDX4)
                                                   +   1   
                                                   +
                                                  (WRK-CHAR-N-CNT  /  2)
                     END-IF
                   END-IF
               END-IF
      *
               IF    SRY-SRYKBN     =  "14"
      *
                 IF (SRY-SRYCD(IDX2) =  "114001110")
                 OR (SRY-SRYCD(IDX2) =  "114012910")
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
                   IF  FLG-OSINKBN        NOT = 9
                       MOVE     3         TO    FLG-OSINKBN
                   END-IF
                 END-EVALUATE
               END-IF
      *
               MOVE    ACCT-ZAIKBN  TO  RECE84-ZAIKBN
      *
               IF      ACCT-ZAIKBN       =     1
                 MOVE   ZERO        TO  RECE85-SRYSURYO(IDX4)
                 MOVE   ZERO        TO  RECE85-TANICD(IDX4)
                 MOVE   ZERO        TO  RECE85-TANIMOJISU(IDX4)
                 MOVE   SPACE       TO  RECE85-TANINAME(IDX4)
                 IF  (SRY-SRYCD(IDX2)(1:1) = "6" OR "7")
                 OR  (SRY-SRYCD(IDX2)(1:3) = "059"     )
                 OR  (SRY-SRYCD(IDX2)(1:2) = "63"      )
                     MOVE  "1"          TO  RECE84-COMCDFLG
                     MOVE  "810000001"  TO  RECE85-SRYCD(IDX4)
                 END-IF
               END-IF
               IF      ACCT-ZAIKBN       =     2
                 MOVE   ZERO        TO  RECE85-SRYSURYO(IDX4)
                 MOVE   ZERO        TO  RECE85-TANICD(IDX4)
                 MOVE   ZERO        TO  RECE85-TANIMOJISU(IDX4)
                 MOVE   SPACE       TO  RECE85-TANINAME(IDX4)
               END-IF
      *
              END-IF
             END-IF
            END-IF  
           END-PERFORM
      *
           IF    IDX4    NOT =   ZERO
               MOVE    1     TO    FLG-WRITE85X
           END-IF
      *
           .
       22021-RECE85-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥È¥³¥á¥ó¥È¥Õ¥é¥°ÊÔ½¸½èÍý
      *****************************************************************
       220211-RECE85-TENSOU-COMCNT-HEN-SEC               SECTION.
      *
           IF      RECE84-SRYKBN      =     "11"
      *       ½é¿Ç¡ÊÆ±ÆüÊ£¿ô¿ÇÎÅ²Ê¡Ë
              IF  (SRY-SRYCD(IDX2)    =     CONS-SYODOU-COMMENT
                                      OR    CONS-SYODOU3-COMMENT)
              AND (FLG-SYODOU-COM     =     ZERO                )
                   MOVE   "1"   TO    RECE84-FLG-SYODOU-COMMENT
                   MOVE   SRY-SRYKA   TO    RECE84-SYODOU-SRYKA(1)
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-SYODOU2-COMMENT
                   MOVE  SPACE  TO    RECE84-FLG-SYODOU-COMMENT
                   MOVE  SPACE  TO    RECE84-SYODOU-SRYKA(1)
                   MOVE    1    TO    FLG-SYODOU-COM
              END-IF
           END-IF
      *
           IF      RECE84-SRYKBN      =     "40"
      *       ²ðÃ£¸£°ú¡ÊÈ¯¾ÉÆüµ­ºÜ¡Ë
              IF   RECE82-SRYYM      >=    "200604"
              AND  RECE82-SRYYM      <=    "200803"
                IF   SRY-SRYCD(IDX2)  =     CONS-KAITATU-COMMENT
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-KAITATU-COMMENT
                END-IF
              END-IF
      *
              IF   RECE82-SRYYM      >=    "201004"
      *         ¿Í¹©¿ÕÂ¡Åù
                IF   SRY-SRYCD(IDX2)  =     CONS-JINZOU1-COMMENT
                                        OR  CONS-JINZOU2-COMMENT
                                        OR  CONS-JINZOU3-COMMENT
                                        OR  CONS-JINZOU4-COMMENT
                                        OR  CONS-JINZOU5-COMMENT
                                        OR  CONS-JINZOU6-COMMENT
                                        OR  CONS-JINZOU7-COMMENT
                                        OR  CONS-JINZOU8-COMMENT
                        MOVE   "1"    TO    RECE84-FLG-JINZOU-COMMENT
                END-IF
      *         ¶É½ê±¢°µÊÄº¿½èÃÖ
                IF   SRY-SRYCD(IDX2)  =     CONS-KYOKUIN1-COMMENT
                                        OR  CONS-KYOKUIN2-COMMENT
                                        OR  CONS-KYOKUIN3-COMMENT
                                        OR  CONS-KYOKUIN4-COMMENT
                        MOVE   "1"    TO    RECE84-FLG-KYOKUIN-COMMENT
                END-IF
              END-IF
           END-IF
      *
           IF    ((RECE82-HKNKBN    NOT  =  "4"         ) AND
                  (RECE84-SRYKBN         =  "80"        ))
           OR    ((RECE82-HKNKBN         =  "4"         ) AND
                  (RECE84-SRYKBN         =  "40" OR "80"))
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
      ****              MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-RIHABIRI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-SINDAI1-COMMENT  OR
                                            CONS-SINDAI2-COMMENT  OR
                                            CONS-SINDAI3-COMMENT  OR
                                            CONS-SINDAI4-COMMENT  OR
                                            CONS-SINDAI7-COMMENT  OR
                                            CONS-SINDAI8-COMMENT  OR
                                            CONS-KOKYUKI1-COMMENT OR
                                            CONS-KOKYUKI2-COMMENT OR
                                            CONS-KOKYUKI3-COMMENT OR
                                            CONS-KOKYUKI4-COMMENT OR
                                            CONS-KOKYUKI7-COMMENT OR
                                            CONS-KOKYUKI8-COMMENT OR
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
      ****              MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-RIHABIRI-COMMENT
              END-IF
              EVALUATE      SRY-SRYCD(IDX2)
                WHEN     CONS-SINDAI5-COMMENT
                        MOVE   "2"    TO    RECE84-RIHABIRI-COMMENT
                WHEN     CONS-SINDAI6-COMMENT
                        MOVE   "3"    TO    RECE84-RIHABIRI-COMMENT
                WHEN     CONS-KOKYUKI5-COMMENT
                        MOVE   "4"    TO    RECE84-RIHABIRI-COMMENT
                WHEN     CONS-KOKYUKI6-COMMENT
                        MOVE   "5"    TO    RECE84-RIHABIRI-COMMENT
                WHEN     CONS-NOUKE5-COMMENT
                        MOVE   "6"    TO    RECE84-RIHABIRI-COMMENT
                WHEN     CONS-NOUKE6-COMMENT
                        MOVE   "7"    TO    RECE84-RIHABIRI-COMMENT
                WHEN     CONS-UNDOKI5-COMMENT
                        MOVE   "8"    TO    RECE84-RIHABIRI-COMMENT
                WHEN     CONS-UNDOKI6-COMMENT
                        MOVE   "9"    TO    RECE84-RIHABIRI-COMMENT
              END-EVALUATE
      *       ¥ê¥Ï¥Ó¥ê¡ÊÈ¯¾ÉÆüµ­ºÜ¡Ë
              IF   RECE82-SRYYM      >=    "200604"
                IF   SRY-SRYCD(IDX2)  =     CONS-SINDAI1-COMMENT
                                      OR    CONS-SINDAI2-COMMENT
                                      OR    CONS-SINDAI3-COMMENT
                                      OR    CONS-SINDAI4-COMMENT
                                      OR    CONS-SINDAI5-COMMENT
                                      OR    CONS-SINDAI6-COMMENT
                                      OR    CONS-SINDAI7-COMMENT
                                      OR    CONS-SINDAI8-COMMENT
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-SINDAI-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-KOKYUKI1-COMMENT
                                      OR    CONS-KOKYUKI2-COMMENT
                                      OR    CONS-KOKYUKI3-COMMENT
                                      OR    CONS-KOKYUKI4-COMMENT
                                      OR    CONS-KOKYUKI5-COMMENT
                                      OR    CONS-KOKYUKI6-COMMENT
                                      OR    CONS-KOKYUKI7-COMMENT
                                      OR    CONS-KOKYUKI8-COMMENT
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-KOKYUKI-COMMENT
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
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-NOUKE-COMMENT
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
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-UNDOKI-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-NANBYO-COMMENT
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-NANBYO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-SYOGAI1-COMMENT
                                      OR    CONS-SYOGAI2-COMMENT
                                      OR    CONS-SYOGAI3-COMMENT
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-SYOGAI-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-SESSYOKU-COMMENT
                        MOVE   "1"    TO    RECE82-COMCNTFLG
                        MOVE   "1"    TO    RECE84-SESSYOKU-COMMENT
                END-IF
              END-IF
           END-IF
      *
           IF      RECE84-SRYKBN      =     "13"
              IF   SRY-SRYCD(IDX2)    =     CONS-YAKUKAN-COMMENT1 OR
                                            CONS-YAKUKAN-COMMENT2 OR
                                            CONS-YAKUKAN-COMMENT3
                   MOVE   "1"   TO    RECE84-FLG-YAKUKAN-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-NYUEIYO-COMMENT
                   MOVE   "1"   TO    RECE84-FLG-NYUEIYO-COMMENT
              END-IF
      *
              IF  (SRY-SRYCD(IDX2)    =     CONS-HOUMON-COMMENT1  OR
                                            CONS-HOUMON-COMMENT2)
              AND (FLG-HOUMON-COM     =     ZERO                )
                   MOVE   "1"   TO    RECE84-FLG-HOUMON-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUMON-COMMENT3
                   MOVE  SPACE  TO    RECE84-FLG-HOUMON-COMMENT
                   MOVE    1    TO    FLG-HOUMON-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     CONS-SYOBYO-COMMENT
              AND  FLG-SYOBYO-COM     =     ZERO
                   MOVE   "1"   TO    RECE84-FLG-SYOBYO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-SYOBYO2-COMMENT
                                      OR    CONS-SYOBYO3-COMMENT
                   MOVE  SPACE  TO    RECE84-FLG-SYOBYO-COMMENT
                   MOVE    1    TO    FLG-SYOBYO-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     CONS-RYOYO-COMMENT
              AND  FLG-RYOYO-COM      =     ZERO
                   MOVE   "1"   TO    RECE84-FLG-RYOYO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-RYOYO2-COMMENT
                   MOVE  SPACE  TO    RECE84-FLG-RYOYO-COMMENT
                   MOVE    1    TO    FLG-RYOYO-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     CONS-TAIIN-COMMENT
                   MOVE   "1"   TO    RECE84-FLG-TAIIN-COMMENT
              END-IF
      *
              IF   RECE82-SRYYM      >=    "201004"
      *         ²ð¸î»Ù±çÏ¢·È»ØÆ³ÎÁ
                IF   SRY-SRYCD(IDX2)  =     CONS-KAISIEN-COMMENT
                        MOVE   "1"    TO    RECE84-FLG-KAISIEN-COMMENT
                END-IF
      *         ½¸ÃÄ±ÉÍÜ¿©»ö»ØÆ³ÎÁ
                IF   SRY-SRYCD(IDX2)  =     CONS-SYUEIYO-COMMENT
                        MOVE   "1"    TO    RECE84-FLG-SYUEIYO-COMMENT
                END-IF
              END-IF
           END-IF
      *
           IF     (RECE82-HKNKBN    NOT  =  "4"    AND
                   RECE84-SRYKBN         =  "14"       )
           OR     (RECE82-HKNKBN         =  "4"    AND
                   RECE84-SRYKBN         =  "80"       )
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUSIJI-COMMENT
              AND  FLG-HOUSIJI-COM    =     ZERO
                   MOVE   "1"   TO    RECE84-FLG-HOUSIJI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUSIJI2-COMMENT
                   MOVE  SPACE  TO    RECE84-FLG-HOUSIJI-COMMENT
                   MOVE    1    TO    FLG-HOUSIJI-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     CONS-HINKAI-COMMENT
                   MOVE   "1"   TO    RECE84-FLG-HINKAI-COMMENT
              END-IF
      *
      ***     IF  (SRY-SRYCD(IDX2)    =     CONS-OSIN-COMMENT
      ***                             OR    CONS-OSIN2-COMMENT)
      ***     AND (FLG-OSIN-COM       =     ZERO              )
      ***          MOVE   "1"   TO    RECE84-FLG-OSIN-COMMENT
      ***     END-IF
      ***     IF   SRY-SRYCD(IDX2)    =     CONS-OSIN3-COMMENT
      ***          MOVE  SPACE  TO    RECE84-FLG-OSIN-COMMENT
      ***          MOVE    1    TO    FLG-OSIN-COM
      ***     END-IF
      *
              IF     WRK-SISETU613      =      1
                IF     RECE82-SRYYM    >=     "200604"
                  IF   SRY-SRYCD(IDX2)  =     CONS-ZAISIEN1    OR
                                              CONS-ZAISIEN2    OR
                                              CONS-ZAISIEN3
                       MOVE   "1"   TO    RECE82-COMCNTFLG
                       MOVE   "1"   TO    RECE84-FLG-ZAISIEN-COMMENT
                  END-IF
                END-IF
              ELSE
                IF   WRK-SISETU695      =      1
                  IF   RECE82-SRYYM    >=     "200804"
                    IF SRY-SRYCD(IDX2)  =     CONS-ZAISIEN1    OR
                                              CONS-ZAISIEN2    OR
                                              CONS-ZAISIEN3
                       MOVE   "1"   TO    RECE82-COMCNTFLG
                       MOVE   "2"   TO    RECE84-FLG-ZAISIEN-COMMENT
                    END-IF
                  END-IF
                END-IF
              END-IF
           END-IF
      *
           IF      RECE84-SRYKBN      =     "50"
              IF   RECE84-SYUTEN1     =     ZERO
                   MOVE   "1"   TO    RECE84-FLG-SHUJUTU-COMMENT
              END-IF
              IF   SRY-SRYSYUKBN      =     "510"
                   MOVE   "1"   TO    RECE84-FLG-SHUJUTU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-SHUJUTU-COMMENT
                   MOVE   "1"   TO    RECE84-FLG-SHUJUTU-COMMENT
              END-IF
           END-IF
      *
           IF   RECE82-SRYYM      >=    "201004"
            IF     RECE84-SRYKBN      =     "54"
              IF   RECE84-SYUTEN1     =     ZERO
                   MOVE   "1"   TO    RECE84-FLG-MASUI-COMMENT
              END-IF
            END-IF
           END-IF
      *
           IF      RECE84-SRYKBN      =     "70"
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
              AND (RECE82-SRYYM       <     "201004"            )
                   MOVE   "1"   TO    RECE84-FLG-CTMRI-COMMENT
              END-IF
              IF  (SRY-SRYCD(IDX2)    =     CONS-CTMRI-COMMENT1  OR
                                            CONS-CTMRI-COMMENT2  OR
                                            CONS-CTMRI-COMMENT4  OR
                                            CONS-CTMRI-COMMENT6  OR
                                            CONS-CTMRI-COMMENT7  OR
                                            CONS-CTMRI-COMMENT18)
              AND (RECE82-SRYYM       >=    "201004"            )
                   MOVE   "1"   TO    RECE84-FLG-CTMRI-COMMENT
              END-IF
           END-IF
      *
           IF    ((RECE82-HKNKBN    NOT  =  "4"         ) AND
                  (RECE84-SRYKBN         =  "80"        ))
           OR    ((RECE82-HKNKBN         =  "4"         ) AND
                  (RECE84-SRYKBN         =  "40" OR "80"))
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUSEI-COMMENT
              AND  FLG-HOUSEI-COM     =     ZERO
                   MOVE   "1"   TO    RECE84-FLG-HOUSEI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUSEI-COMMENT2
                   MOVE  SPACE  TO    RECE84-FLG-HOUSEI-COMMENT
                   MOVE    1    TO    FLG-HOUSEI-COM
              END-IF
           END-IF
      *
           IF      RECE84-SRYKBN      =     "90"  OR  "92"
              IF   SRY-SRYCD(IDX2)    =     CONS-KAKURI-COMMENT
                   MOVE   "1"   TO    RECE84-FLG-KAKURI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-CHIRYO-COMMENT
                   MOVE   "1"   TO    RECE84-FLG-CHIRYO-COMMENT
              END-IF
      *
              IF   RECE82-SRYYM      >=    "201004"
      *         ±ÉÍÜ¥µ¥Ý¡¼¥È¥Á¡¼¥à²Ã»»
                IF   SRY-SRYCD(IDX2)  =     CONS-EISAPO-COMMENT
                        MOVE   "1"    TO    RECE84-FLG-EISAPO-COMMENT
                END-IF
      *         ¸ÆµÛ¥±¥¢¥Á¡¼¥à²Ã»»
                IF   SRY-SRYCD(IDX2)  =     CONS-KOKKEA-COMMENT
                        MOVE   "1"    TO    RECE84-FLG-KOKKEA-COMMENT
                END-IF
      *         ½Å¾É¼Ô²Ã»»¡ÊÀº¿ÀÎÅÍÜÉÂÅïÆþ±¡ÎÁ¡Ë
                IF   SRY-SRYCD(IDX2)  =     CONS-JYUSYO-COMMENT
                        MOVE   "1"    TO    RECE84-FLG-JYUSYO-COMMENT
                END-IF
              END-IF
           END-IF
      *
           IF      RECE84-SRYKBN      =     "60" OR "64"
              IF   (SRY-SRYCD(IDX2)   =     CONS-KENSA1-COMMENT
                                      OR    CONS-KENSA2-COMMENT
                                      OR    CONS-KENSA3-COMMENT)
              AND  (FLG-KENSA-COM     =     ZERO               )
                   MOVE   "1"   TO    RECE84-FLG-KENSA-COMMENT
              END-IF
           END-IF
      *
           IF      RECE84-SRYKBN   =  "14" OR "40" OR "50" OR "54"
                                   OR "60" OR "70" OR "80" OR "64"
              SET     TBL-SANSO    TO      1
              SEARCH      TBL-SANSO-OCC   VARYING   TBL-SANSO
                   AT  END
                       MOVE    ZERO            TO  FLG-SANSO-OK
                   WHEN    SRY-SRYCD (IDX2)    =   TBL-SANSO-SRYCD
                                                        (TBL-SANSO)
                       MOVE    1               TO  FLG-SANSO-OK
              END-SEARCH
              IF      FLG-SANSO-OK        =    1
                   MOVE   "1"   TO    RECE84-FLG-SANSO-COMMENT
              END-IF
           END-IF
      *
           .
       220211-RECE85-TENSOU-COMCNT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼«ÇåÀÕÆÃÄê´ïºàÅÀ¿ôÊÔ½¸½èÍý
      *****************************************************************
       220212-JIBAI-KIZAI-TENSU-HENSYU-SEC     SECTION.
      *
           MOVE     ZERO                   TO    WRK-TEN-KZI
           MOVE     ZERO                   TO    FLG-TAIGAI
      *
           IF   RECE82-TAXKBN-KZI   =   "2"
      *
      *       ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡Ê£´£°£°£³¡Ë¤Ë¤Æ¾ÃÈñÀÇÂÐ¾Ý³°¥³¡¼¥É¤«È½ÊÌ
              INITIALIZE                         SYSKANRI-REC
              MOVE    "4003"               TO    SYS-KANRICD
              MOVE    "*"                  TO    SYS-KBNCD
              MOVE    RECE82-SRYYM         TO    SYS-STYUKYMD (1:6)
              MOVE    "01"                 TO    SYS-STYUKYMD (7:2)
              MOVE    SYS-STYUKYMD         TO    SYS-EDYUKYMD
grpsys        MOVE    SPA-HOSPNUM          TO    SYS-HOSPNUM
              MOVE    SYSKANRI-REC         TO    MCPDATA-REC
              PERFORM 910-SYSKANRI-INV-SEC
              IF      FLG-SYSKANRI        =   ZERO
                MOVE      MCPDATA-REC      TO    SYS-4003-REC
                PERFORM   VARYING   IDX10    FROM   1   BY   1
                           UNTIL   (IDX10                 >    50 )
                            OR     (SYS-4003-SRYCD(IDX10) =  SPACE)
                            OR     (FLG-TAIGAI            =     1 )
                  IF   SYS-4003-SRYCD(IDX10)  =  SRY-SRYCD(IDX2)
                      MOVE     1           TO    FLG-TAIGAI
                  END-IF
                END-PERFORM
              END-IF
              IF      TNS-KANANAMECHGKBN  =  ZERO OR 5
                  CONTINUE
              ELSE
                  MOVE     1       TO    FLG-TAIGAI
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
       220212-JIBAI-KIZAI-TENSU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    »ÀÁÇ¤ÎÃ±²ÁÊÔ½¸½èÍý
      *****************************************************************
       220213-RECE85-TENSOU-TANKA-HEN-SEC     SECTION.
      *
           MOVE    SPACE           TO  WRK-TANKA-X
      * 
      *    ¿ô»úÉ½¼¨ÍÑÊÔ½¸¡ÊÈ¾³Ñ¡Ë
           MOVE    TNS-TEN         TO  WRK-TANKA
           MOVE    WRK-TANKA-S1    TO  WRK-TANKA-Z1
           MOVE    ZERO            TO  FLG-SP
           PERFORM   VARYING   IDY3   FROM    2   BY  -1
                     UNTIL     IDY3           =   ZERO
             IF  WRK-TANKA-S2(IDY3:1) NOT =   ZERO
                 MOVE    1               TO  FLG-SP
             END-IF
             IF  FLG-SP                  =   1
                 MOVE    WRK-TANKA-S2(IDY3:1)
                                         TO  WRK-TANKA-Z3(IDY3:1)
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
       220213-RECE85-TENSOU-TANKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼«ÇåÀÕÊ¸½ñÎÁÅÀ¿ôÊÔ½¸½èÍý
      *****************************************************************
       220214-JIBAI-BUNSYO-TENSU-HENSYU-SEC     SECTION.
      *
           MOVE        ZERO          TO    WRK-TEN-BUN
      *
           IF   RECE82-TAXKBN   =   "2"
              IF    WRK-TAX1001-KBN  =  "1"
                  COMPUTE  WRK-TEN-BUN  =
                          (TNS-TEN  *  1.05)  +  0.5
              ELSE
                  COMPUTE  WRK-TEN-BUN  =
                          (TNS-TEN  *  1.05)
              END-IF
           ELSE
              MOVE     TNS-TEN       TO     WRK-TEN-BUN
           END-IF
      *
           .
       220214-JIBAI-BUNSYO-TENSU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷£²¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       22022-RECE82-HENSYU-SEC               SECTION.
      *
           PERFORM   VARYING  IDX3   FROM   1   BY   1
                     UNTIL    (IDX3         >     5)        OR
                              (SRY-SRYCD(IDX3)   =   SPACE)
               IF    SRY-RENNUM     =     1
                   IF  IDX3         =     1
                       MOVE  ACCT-ZAIKAISU     TO   WK-ZAIKAISU
                       MOVE  ACCT-ZAITEN       TO   WK-ZAITEN
                       IF   RECE82-HKNKBN    NOT  =  "4"
                         IF    SRY-SRYKBN   =  "31" OR "32" OR "33" OR
                                               "40" OR "50" OR "54" OR
                                               "60" OR "70" OR "80" OR
                                               "64"
                                MOVE   ACCT-SYUTEN1   TO   WK-SYUTEN
                                COMPUTE   WK-YKZTEN   =
                                                      ACCT-KIZAITEN  +
                                                      ACCT-YKZTEN
                         END-IF
                       ELSE
                         IF    SRY-SRYKBN   =  "31" OR "32" OR "33" OR
                                               "40" OR "50" OR "54" OR
                                               "60" OR "70" OR "80" OR
                                               "64"
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
                                IF  WRK-KEIBUKASAN  NOT =  ZERO
                                   COMPUTE  WK-SYUTEN  = WK-SYUTEN
                                                       - WRK-KEIBUKASAN
                                   COMPUTE  WK-YKZTEN  = WK-YKZTEN
                                                       + WRK-KEIBUKASAN
                                END-IF
                                IF  WRK-SOGAIKASAN  NOT =  ZERO
                                   COMPUTE  WK-SYUTEN  = WK-SYUTEN
                                                       - WRK-SOGAIKASAN
                                   COMPUTE  WK-YKZTEN  = WK-YKZTEN
                                                       + WRK-SOGAIKASAN
                                END-IF
                                IF  WRK-RYOYOTEATE  NOT =  ZERO
                                 IF  RECE82-JIBAIKBN    =  "2"
                                   COMPUTE  WK-SYUTEN  = WK-SYUTEN
                                                       - WRK-RYOYOTEATE
                                   COMPUTE  WK-YKZTEN  = WK-YKZTEN
                                                       + WRK-RYOYOTEATE
                                 END-IF
                                END-IF
                         END-IF
                       END-IF
                       IF    SRY-SRYKBN     =  "14"
                           MOVE    ACCT-SYUTEN1   TO   WK-SYUTEN
                           MOVE    ACCT-KIZAITEN  TO   WK-KIZAITEN
                           MOVE    ACCT-YKZTEN    TO   WK-YKZTEN
                           IF   RECE82-HKNKBN   =  "4"
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
                             IF  WRK-KEIBUKASAN  NOT =  ZERO
                                COMPUTE  WK-SYUTEN   =  WK-SYUTEN
                                                     -  WRK-KEIBUKASAN
                                COMPUTE  WK-KIZAITEN =  WK-KIZAITEN
                                                     +  WRK-KEIBUKASAN
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
                                            "13"  OR  "14"  OR  "64"
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
               IF  ((ACCT-ZAITEN    NOT =    ZERO) AND
                    (ACCT-ZAIKAISU  NOT =    ZERO))
               OR  ( FLG-TAISYO         =     1   )
                  EVALUATE   RECE82-HKNKBN
                    WHEN       "1"
                    WHEN       "2"
                        PERFORM   220221-RECE-LEFT-SET-SEC
                    WHEN       "4"
                        EVALUATE  WRK-PARA-RECEKBN
                          WHEN      "4"
      ********************* ¡Ê¿·ÍÍ¼°¡Ë
                            PERFORM   220222-RECE-LEFT-SET2-SEC
                          WHEN      "5"
      ********************* ¡Ê½¾ÍèÍÍ¼°¡Ë
                            PERFORM   220223-RECE-LEFT-SET3-SEC
                        END-EVALUATE
                    WHEN       "5"
                        PERFORM   220224-RECE-LEFT-SET4-SEC
                  END-EVALUATE
               END-IF
               IF  ( SRY-SRYKBN   =   "21"   OR   "22"   OR   "23"   OR
                                      "31"   OR   "32"   OR   "33"   OR
                                      "40"   OR   "50"   OR   "54"   OR
                                      "60"   OR   "70"   OR   "14"   OR
                                      "64" )
               OR  ((ACCT-ZAIKBN  =    2  ) AND
                    (SRY-SRYKBN   =   "13"))
                     MOVE    6      TO    IDX3
               END-IF
           END-PERFORM
      *
           .                                           
      *
       22022-RECE82-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷£²¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       220221-RECE-LEFT-SET-SEC               SECTION.
      *
           MOVE        SRY-SRYCD(IDX3)     TO   WRK-SRYCD
           MOVE        RECE85-SRYYM        TO   WK-TAIHI-SRYYM
           MOVE        WK-TAIHI-DAY        TO   WK-TAIHI-SRYD
           PERFORM     900-TENSU-TBL-SEC
           EVALUATE    SRY-SRYKBN
      *    ½é¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "11"
                   IF   TNS-DATAKBN  =   2
                     EVALUATE    TNS-TIMEKSNKBN
                         WHEN    1
                         WHEN    5
                         WHEN    6
                             MOVE  "1"     TO   RECE82-SHOSHINTIME
                         WHEN    3
                             MOVE  "2"     TO   RECE82-SHOSHINTIME
                         WHEN    4
                             MOVE  "3"     TO   RECE82-SHOSHINTIME
                     END-EVALUATE
                     COMPUTE    RECE82-TENSU(1)  =
                                RECE82-TENSU(1)  +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF
                   IF   SRY-SRYCD(IDX3) = CONS-ROUSAI1  OR
                                          CONS-ROUSAI21
                        COMPUTE    RECE82-KINGAKU2(1)  =
                                   RECE82-KINGAKU2(1)  +
                                   (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF  
                   IF   SRY-SRYCD(IDX3) = CONS-ROUSAI2
                        COMPUTE    RECE82-KINGAKU2(2)  =
                                   RECE82-KINGAKU2(2)  +
                                   (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF  
      *    »ØÆ³¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "13"
                   IF ( SRY-SRYCD(IDX3) =   CONS-ROUSAI4  OR
                                            CONS-ROUSAI5  OR
                                            CONS-ROUSAI6  OR
                                            CONS-ROUSAI7  OR
                                            CONS-ROUSAI8  OR
                                            CONS-ROUSAI9  OR
                                            CONS-ROUSAI10 OR
                                            CONS-ROUSAI11 OR
                                            CONS-ROUSAI12    )
                   OR ((SRY-SRYCD(IDX3) =   CONS-ROUSAI3    ) AND
                       (RECE82-SRYYM    <   "200804"        ))
                         COMPUTE    RECE82-KINGAKU2(2)  =
                                    RECE82-KINGAKU2(2)  +
                                   (TNS-TEN   *   ACCT-ZAIKAISU)
                   ELSE
                     IF   ACCT-ZAIKBN     =  2
                         COMPUTE    WRK-RECE82G-TENSU(2)  =
                                    WRK-RECE82G-TENSU(2)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                         COMPUTE    RECE82-TENSU(2)  =
                                    RECE82-TENSU(2)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   END-IF                   
      *    ºßÂð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "14"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE    WRK-RECE82G-TENSU(3)  =
                              WRK-RECE82G-TENSU(3)  +
                             (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   COMPUTE    RECE82-TENSU(3)  =
                              RECE82-TENSU(3)  +
                             (WK-ZAITEN   *   WK-ZAIKAISU)
                 END-IF
      *    ÅêÌô¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF   SRY-SRYCD(IDX3)         =    CONS-YAKTEIGEN
                           COMPUTE   RECE82-TENSU(4)  =
                                     RECE82-TENSU(4)  -
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                           ADD   WK-ZAIKAISU   TO   RECE82-KAISU(4)
                           COMPUTE   RECE82-TENSU(4)  =
                                     RECE82-TENSU(4)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)     =    CONS-FUNSAI-COMMENT
                            ADD   WK-ZAIKAISU  TO   RECE82-KAISU(4)
                            COMPUTE   RECE82-TENSU(4)  =
                                      RECE82-TENSU(4)  +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF   
                   END-IF
               WHEN    "22"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(5)
                   COMPUTE   RECE82-TENSU(5)  =
                             RECE82-TENSU(5)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
               WHEN    "23"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(6)
                   COMPUTE   RECE82-TENSU(6)  =
                             RECE82-TENSU(6)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    Ä´ºÞ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "24"
                   IF  (SRY-SRYCD(IDX3)(1:1)   =   "8"  )
                   OR  (SRY-SRYCD(IDX3)(1:3)   =   "008")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(7)
                   END-IF
                   COMPUTE   RECE82-TENSU(7)  =
                             RECE82-TENSU(7)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    ËãÆÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "26"
                   IF  (SRY-SRYCD(IDX3)(1:1)   =   "8"  )
                   OR  (SRY-SRYCD(IDX3)(1:3)   =   "008")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(8)
                   END-IF
                   COMPUTE   RECE82-TENSU(8)  =
                             RECE82-TENSU(8)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    Ä´´ð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "27"
                   COMPUTE   RECE82-TENSU(9)  =
                             RECE82-TENSU(9)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    Èé²¼¶ÚÆùÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "31"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(10)
                   COMPUTE   RECE82-TENSU(10)  =
                             RECE82-TENSU(10)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ÀÅÌ®ÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "32"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(11)
                   COMPUTE   RECE82-TENSU(11)  =
                             RECE82-TENSU(11)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾Ãí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "33"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(12)
                   COMPUTE   RECE82-TENSU(12)  =
                             RECE82-TENSU(12)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ½èÃÖ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(13)
                   END-IF
                   COMPUTE   RECE82-TENSU(13)  =
                             RECE82-TENSU(13)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(14)  =
                             RECE82-TENSU(14)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¼ê½Ñ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(15)
                   END-IF
                   COMPUTE   RECE82-TENSU(15)  =
                             RECE82-TENSU(15)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(16)  =
                             RECE82-TENSU(16)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¸¡ºº¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "60"
               WHEN    "64"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(17)
                   END-IF
                   COMPUTE   RECE82-TENSU(17)  =
                             RECE82-TENSU(17)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    TNS-KNSJISKBN    =   2
                      COMPUTE   RECE82-KAISU(17)  =
                                RECE82-KAISU(17)  -  
                                WK-ZAIKAISU
                   END-IF
                   IF    TNS-SRYCD        =   CONS-SAIKETSU1  OR
                                              CONS-SAIKETSU2  OR
                                              CONS-SAIKETSU3
                         IF   RECE82-KAISU(17)  NOT =  ZERO
                              COMPUTE   RECE82-KAISU(17)  =
                                        RECE82-KAISU(17)  -   
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE82-TENSU(18)  =
                             RECE82-TENSU(18)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ²èÁü¿ÇÃÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(19)
                   END-IF
                   COMPUTE   RECE82-TENSU(19)  =
                             RECE82-TENSU(19)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(20)  =
                             RECE82-TENSU(20)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "80"
                   IF ( SRY-SRYCD(IDX3) =   CONS-ROUSAI13  OR
                                            CONS-ROUSAI14  OR
                                            CONS-ROUSAI15  OR
                                            CONS-ROUSAI16  OR
                                            CONS-ROUSAI17  OR
                                            CONS-ROUSAI18  OR
                                            CONS-ROUSAI19  OR
                                            CONS-ROUSAI20 )
                   OR ((ACCT-SRYKBN          =  "80"     )  AND
                       (SRY-SRYCD(IDX3)(1:5) =  "09594"  ))
                         COMPUTE    RECE82-KINGAKU2(2)  =
                                    RECE82-KINGAKU2(2)  +
                                  (TNS-TEN     *   ACCT-ZAIKAISU)
                         EVALUATE   SRY-SRYCD(IDX3)
                              WHEN   CONS-ROUSAI13
                              WHEN   CONS-ROUSAI14
                                  MOVE    1     TO   WRK-HEYA-NINZU
                              WHEN   CONS-ROUSAI15
                              WHEN   CONS-ROUSAI16
                                  MOVE    2     TO   WRK-HEYA-NINZU
                              WHEN   CONS-ROUSAI17
                              WHEN   CONS-ROUSAI18
                                  MOVE    3     TO   WRK-HEYA-NINZU
                              WHEN   CONS-ROUSAI19
                              WHEN   CONS-ROUSAI20
                                  MOVE    4     TO   WRK-HEYA-NINZU
                         END-EVALUATE
                         EVALUATE   SRY-SRYCD(IDX3)(1:7)
                              WHEN   "0959401"
                              WHEN   "0959402"
                                  MOVE    1     TO   WRK-HEYA-NINZU
                              WHEN   "0959403"
                              WHEN   "0959404"
                                  MOVE    2     TO   WRK-HEYA-NINZU
                              WHEN   "0959407"
                              WHEN   "0959408"
                                  MOVE    3     TO   WRK-HEYA-NINZU
                              WHEN   "0959409"
                              WHEN   "0959410"
                                  MOVE    4     TO   WRK-HEYA-NINZU
                         END-EVALUATE
                         IF    RECE82-HEYA-NINZU   =   ZERO
                         AND   FLG-HEYA            =   ZERO
                           MOVE  WRK-HEYA-NINZU  TO   RECE82-HEYA-NINZU
                           ADD   ACCT-ZAIKAISU   TO   RECE82-HEYA-NISSU
                         ELSE
                           IF  RECE82-HEYA-NINZU   =  WRK-HEYA-NINZU
                           AND FLG-HEYA            =   ZERO
                              ADD  ACCT-ZAIKAISU TO   RECE82-HEYA-NISSU
                           ELSE
                              MOVE  ZERO         TO   RECE82-HEYA-NINZU
                              ADD  ACCT-ZAIKAISU TO   RECE82-HEYA-NISSU
                              MOVE  1            TO   FLG-HEYA
                           END-IF
                         END-IF
                   ELSE
                     IF ((ACCT-SRYKBN          =  "80"   )  AND
                         (SRY-SRYCD(IDX3)(1:5) =  "09593"))
                     OR ( SRY-SRYCD(IDX3) >= "101910110" AND
                                          <= "101910400"  )
                         COMPUTE   RECE82-KINGAKU2(2)  =
                                   RECE82-KINGAKU2(2)  +
                                  (TNS-TEN     *   ACCT-ZAIKAISU)
                         IF   (FLG-TAISYO           =   1     )
                         AND  (SRY-SRYCD(IDX3)(1:5) =  "09593")
                          COMPUTE  WRK-ZAITENNASHI  =
                                   WRK-ZAITENNASHI  +
                                  (TNS-TEN     *   ACCT-ZAIKAISU)
                         END-IF
                     ELSE                
                       IF    TNS-NYUTENTTLKBN   =    130
                         COMPUTE   RECE82-TENSU(2)  =
                                   RECE82-TENSU(2)  +
                                  (TNS-TEN     *   ACCT-ZAIKAISU)
                       ELSE  
                         COMPUTE   RECE82-TENSU(21)  =
                                   RECE82-TENSU(21)  +
                                  (WK-SYUTEN   *   WK-ZAIKAISU)
                       END-IF
                       COMPUTE       RECE82-TENSU(22)  =
                                     RECE82-TENSU(22)  +
                                    (WK-YKZTEN   *   WK-ZAIKAISU)
                     END-IF
                   END-IF
           END-EVALUATE
           .
      *
       220221-RECE-LEFT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷£²¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       220222-RECE-LEFT-SET2-SEC               SECTION.
      *
           MOVE        SRY-SRYCD(IDX3)     TO   WRK-SRYCD
           MOVE        RECE85-SRYYM        TO   WK-TAIHI-SRYYM
           MOVE        WK-TAIHI-DAY        TO   WK-TAIHI-SRYD
           PERFORM     900-TENSU-TBL-SEC
           EVALUATE    SRY-SRYKBN
      *    ½é¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "11"
                   IF   TNS-DATAKBN  =   2
                     EVALUATE    TNS-TIMEKSNKBN
                         WHEN    1
                         WHEN    5
                         WHEN    6
                             MOVE  "1"     TO   RECE82-SHOSHINTIME
                         WHEN    3
                             MOVE  "2"     TO   RECE82-SHOSHINTIME
                         WHEN    4
                             MOVE  "3"     TO   RECE82-SHOSHINTIME
                     END-EVALUATE
                     COMPUTE    RECE82-TENSU(1)  =
                                RECE82-TENSU(1)  +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                     IF  SRY-SRYCD(IDX3)  =  CONS-NYUKSN1  OR
                                             CONS-NYUKSN2  OR
                                             CONS-NYUKSN3  OR
                                             CONS-NYUKSN4  OR
                                             CONS-NYUKSN5  OR
                                             CONS-NYUKSN6  OR
                                             CONS-NYUKSN7  OR
                                             CONS-NYUKSN8  OR
                                             CONS-NYUKSN9
                             MOVE  "1"     TO   RECE82-SHOSHIN-NYU
                     END-IF
                   END-IF
                   IF   SRY-SRYCD(IDX3) = CONS-ROUSAI1  OR
                                          CONS-ROUSAI21
                        COMPUTE    RECE82-KINGAKU2(1)  =
                                   RECE82-KINGAKU2(1)  +
                                   (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF  
                   IF   SRY-SRYCD(IDX3) = CONS-ROUSAI2
                        COMPUTE    RECE82-KINGAKU2(2)  =
                                   RECE82-KINGAKU2(2)  +
                                   (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF  
      *    »ØÆ³¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "13"
                   IF ( SRY-SRYCD(IDX3) =   CONS-ROUSAI4  OR
                                            CONS-ROUSAI5  OR
                                            CONS-ROUSAI6  OR
                                            CONS-ROUSAI7  OR
                                            CONS-ROUSAI8  OR
                                            CONS-ROUSAI9  OR
                                            CONS-ROUSAI10 OR
                                            CONS-ROUSAI11 OR
                                            CONS-ROUSAI12    )
                   OR ((SRY-SRYCD(IDX3) =   CONS-ROUSAI3    ) AND
                       (RECE82-SRYYM    <   "200804"        ))
                         COMPUTE    RECE82-KINGAKU3(3)  =
                                    RECE82-KINGAKU3(3)  +
                                   (TNS-TEN   *   ACCT-ZAIKAISU)
                   ELSE
                     IF   ACCT-ZAIKBN     =  2
                         COMPUTE    WRK-RECE82G-TENSU(2)  =
                                    WRK-RECE82G-TENSU(2)  +
                                   (WK-ZAITEN *   WK-ZAIKAISU)
                     ELSE
                         COMPUTE    RECE82-TENSU(2)  =
                                    RECE82-TENSU(2)  +
                                   (WK-ZAITEN *   WK-ZAIKAISU)
                     END-IF
                   END-IF                   
      *    ºßÂð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "14"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE    WRK-RECE82G-TENSU(22) =
                              WRK-RECE82G-TENSU(22) +
                             (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   EVALUATE  WRK-PARA-JIBAIPRTKBN
                     WHEN      "0"
                       COMPUTE    RECE82-TENSU(22)  =
                                  RECE82-TENSU(22)  +
                                 (WK-SYUTEN     *   WK-ZAIKAISU)
                       COMPUTE    RECE82-TENSU-YAK(22)  =
                                  RECE82-TENSU-YAK(22)  +
                                 (WK-KIZAITEN   *   WK-ZAIKAISU)  +
                                 (WK-YKZTEN     *   WK-ZAIKAISU)
                     WHEN      "1"
                       COMPUTE    RECE82-TENSU(22)  =
                                  RECE82-TENSU(22)  +
                                 (WK-SYUTEN     *   WK-ZAIKAISU)
                       COMPUTE    RECE82-TENSU-YAK(23)  =
                                  RECE82-TENSU-YAK(23)  +
                                 (WK-KIZAITEN   *   WK-ZAIKAISU)  +
                                 (WK-YKZTEN     *   WK-ZAIKAISU)
                   END-EVALUATE
                 END-IF
      *    ÅêÌô¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF   SRY-SRYCD(IDX3)         =    CONS-YAKTEIGEN
                           COMPUTE   RECE82-TENSU-YAK(4)  =
                                     RECE82-TENSU-YAK(4)  -
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                           ADD   WK-ZAIKAISU   TO   RECE82-KAISU(4)
                           COMPUTE   RECE82-TENSU-YAK(4)  =
                                     RECE82-TENSU-YAK(4)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)     =    CONS-FUNSAI-COMMENT
                            ADD   WK-ZAIKAISU  TO   RECE82-KAISU(4)
                            COMPUTE   RECE82-TENSU-YAK(4)  =
                                      RECE82-TENSU-YAK(4)  +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF   
                   END-IF
               WHEN    "22"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(5)
                   COMPUTE   RECE82-TENSU-YAK(5)  =
                             RECE82-TENSU-YAK(5)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
               WHEN    "23"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(6)
                   COMPUTE   RECE82-TENSU-YAK(6)  =
                             RECE82-TENSU-YAK(6)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    Ä´ºÞ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "24"
                   IF    RECE82-KHNTEN(7)   =   ZERO
                         MOVE  TNS-TEN    TO    RECE82-KHNTEN(7)
                   ELSE
                      IF   RECE82-KHNTEN(7) NOT = TNS-TEN
                      AND  TNS-TEN          NOT = ZERO
                           MOVE   ZERO    TO    RECE82-KHNTEN(7)
                           MOVE   1       TO    FLG-KHNTEN7-JI
                      END-IF
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   =   "8"  )
                   OR  (SRY-SRYCD(IDX3)(1:3)   =   "008")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(7)
                   END-IF
                   COMPUTE   RECE82-TENSU(7)  =
                             RECE82-TENSU(7)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    ËãÆÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "26"
                   IF   WRK-PARA-JIBAIPRTKBN =  "0"
                    IF    RECE82-KHNTEN(8)   =   ZERO
                         MOVE  TNS-TEN    TO    RECE82-KHNTEN(8)
                    ELSE
                      IF   RECE82-KHNTEN(8) NOT = TNS-TEN
                      AND  TNS-TEN          NOT = ZERO
                           MOVE   ZERO    TO    RECE82-KHNTEN(8)
                           MOVE   1       TO    FLG-KHNTEN8-JI
                      END-IF
                    END-IF
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   =   "8"  )
                   OR  (SRY-SRYCD(IDX3)(1:3)   =   "008")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(8)
                   END-IF
                   COMPUTE   RECE82-TENSU(8)  =
                             RECE82-TENSU(8)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    Ä´´ð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "27"
                   IF    RECE82-KHNTEN(9)   =   ZERO
                         MOVE  TNS-TEN    TO    RECE82-KHNTEN(9)
                   ELSE
                      IF   RECE82-KHNTEN(9) NOT = TNS-TEN
                      AND  TNS-TEN          NOT = ZERO
                           MOVE   ZERO    TO    RECE82-KHNTEN(9)
                           MOVE   1       TO    FLG-KHNTEN9-JI
                      END-IF
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   =   "8"  )
                   OR  (SRY-SRYCD(IDX3)(1:3)   =   "008")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(9)
                   END-IF
                   COMPUTE   RECE82-TENSU(9)  =
                             RECE82-TENSU(9)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    Èé²¼¶ÚÆùÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "31"
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(12)
                   COMPUTE   RECE82-TENSU(12)  =
                             RECE82-TENSU(12)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(13)  =
                             RECE82-TENSU-YAK(13)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ÀÅÌ®ÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "32"
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(12)
                   COMPUTE   RECE82-TENSU(12)  =
                             RECE82-TENSU(12)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(13)  =
                             RECE82-TENSU-YAK(13)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾Ãí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "33"
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(12)
                   COMPUTE   RECE82-TENSU(12)  =
                             RECE82-TENSU(12)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(13)  =
                             RECE82-TENSU-YAK(13)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ½èÃÖ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(14)
                   END-IF
                   COMPUTE   RECE82-TENSU(14)  =
                             RECE82-TENSU(14)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(15)  =
                             RECE82-TENSU-YAK(15)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¼ê½Ñ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(16)
                   END-IF
                   COMPUTE   RECE82-TENSU(16)  =
                             RECE82-TENSU(16)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(17)  =
                             RECE82-TENSU-YAK(17)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¸¡ºº¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "60"
               WHEN    "64"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(18)
                   END-IF
                   COMPUTE   RECE82-TENSU(18)  =
                             RECE82-TENSU(18)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    TNS-KNSJISKBN    =   2
                      COMPUTE   RECE82-KAISU(18)  =
                                RECE82-KAISU(18)  -  
                                WK-ZAIKAISU
                   END-IF
                   IF    TNS-SRYCD        =   CONS-SAIKETSU1  OR
                                              CONS-SAIKETSU2  OR
                                              CONS-SAIKETSU3
                         IF   RECE82-KAISU(18)  NOT =  ZERO
                              COMPUTE   RECE82-KAISU(18)  =
                                        RECE82-KAISU(18)  -   
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE82-TENSU-YAK(19)  =
                             RECE82-TENSU-YAK(19)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ²èÁü¿ÇÃÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(20)
                   END-IF
                   COMPUTE   RECE82-TENSU(20)  =
                             RECE82-TENSU(20)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU-YAK(21)  =
                             RECE82-TENSU-YAK(21)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "80"
                  IF ( SRY-SRYCD(IDX3) =   CONS-ROUSAI13  OR
                                           CONS-ROUSAI14  OR
                                           CONS-ROUSAI15  OR
                                           CONS-ROUSAI16  OR
                                           CONS-ROUSAI17  OR
                                           CONS-ROUSAI18  OR
                                           CONS-ROUSAI19  OR
                                           CONS-ROUSAI20 )
                  OR ((ACCT-SRYKBN          =  "80"     )  AND
                      (SRY-SRYCD(IDX3)(1:5) =  "09594"  ))
                       COMPUTE    RECE82-KINGAKU3(1)  =
                                  RECE82-KINGAKU3(1)  +
                                 (TNS-TEN   *   ACCT-ZAIKAISU)
                       EVALUATE   SRY-SRYCD(IDX3)
                              WHEN   CONS-ROUSAI13
                              WHEN   CONS-ROUSAI14
                                  MOVE    1     TO   WRK-HEYA-NINZU
                              WHEN   CONS-ROUSAI15
                              WHEN   CONS-ROUSAI16
                                  MOVE    2     TO   WRK-HEYA-NINZU
                              WHEN   CONS-ROUSAI17
                              WHEN   CONS-ROUSAI18
                                  MOVE    3     TO   WRK-HEYA-NINZU
                              WHEN   CONS-ROUSAI19
                              WHEN   CONS-ROUSAI20
                                  MOVE    4     TO   WRK-HEYA-NINZU
                       END-EVALUATE
                       EVALUATE   SRY-SRYCD(IDX3)(1:7)
                              WHEN   "0959401"
                              WHEN   "0959402"
                                  MOVE    1     TO   WRK-HEYA-NINZU
                              WHEN   "0959403"
                              WHEN   "0959404"
                                  MOVE    2     TO   WRK-HEYA-NINZU
                              WHEN   "0959407"
                              WHEN   "0959408"
                                  MOVE    3     TO   WRK-HEYA-NINZU
                              WHEN   "0959409"
                              WHEN   "0959410"
                                  MOVE    4     TO   WRK-HEYA-NINZU
                       END-EVALUATE
                       IF    RECE82-HEYA-NINZU   =   ZERO
                       AND   FLG-HEYA            =   ZERO
                           MOVE  WRK-HEYA-NINZU  TO   RECE82-HEYA-NINZU
                           ADD   ACCT-ZAIKAISU   TO   RECE82-HEYA-NISSU
                       ELSE
                           IF  RECE82-HEYA-NINZU   =  WRK-HEYA-NINZU
                           AND FLG-HEYA            =   ZERO
                              ADD  ACCT-ZAIKAISU TO   RECE82-HEYA-NISSU
                           ELSE
                              MOVE  ZERO         TO   RECE82-HEYA-NINZU
                              ADD  ACCT-ZAIKAISU TO   RECE82-HEYA-NISSU
                              MOVE  1            TO   FLG-HEYA
                           END-IF
                       END-IF
                  ELSE
                   IF ( ACCT-SRYKBN          =  "80"    )   AND
                      ( SRY-SRYCD(IDX3)(1:5) =  "09591" OR 
                                                "09592" )
                       CONTINUE
                   ELSE
                    IF ((ACCT-SRYKBN          =  "80"   )  AND
                        (SRY-SRYCD(IDX3)(1:5) =  "09593"))
                    OR ( SRY-SRYCD(IDX3) >= "101910110" AND
                                         <= "101910400"  )
                     IF  SRY-SRYCD(IDX3)(1:5) =  "09593"
                      EVALUATE  WRK-JIBAIKZI-SYUKEIKBN(IDX3)
                       WHEN       0
                       WHEN       5
                         COMPUTE RECE82-KINGAKU3(3)  =
                                 RECE82-KINGAKU3(3)  +
                             (WRK-JIBAIKZI-TNS(IDX3) * ACCT-ZAIKAISU)
                       WHEN       1
                         COMPUTE RECE82-KINGAKU2(1)  =
                                 RECE82-KINGAKU2(1)  +
                             (WRK-JIBAIKZI-TNS(IDX3) * ACCT-ZAIKAISU)
                       WHEN       4
                         COMPUTE RECE82-KINGAKU2(2)  =
                                 RECE82-KINGAKU2(2)  +
                             (WRK-JIBAIKZI-TNS(IDX3) * ACCT-ZAIKAISU)
                      END-EVALUATE
                     ELSE
                         COMPUTE RECE82-KINGAKU3(3)  =
                                 RECE82-KINGAKU3(3)  +
                             (TNS-TEN     *   ACCT-ZAIKAISU)
                     END-IF
                    ELSE                                                
                     IF  SRY-SRYCD(IDX3)      =    CONS-RYOYOTEATE
                       COMPUTE    RECE82-KINGAKU3(3)  =                 
                                  RECE82-KINGAKU3(3)  +                 
                                 (WRK-RYOYOTEATE  *   ACCT-ZAIKAISU)    
                     ELSE
                      IF    TNS-NYUTENTTLKBN   =    130
                            COMPUTE   RECE82-TENSU(2)  =
                                      RECE82-TENSU(2)  +
                                     (TNS-TEN   *   ACCT-ZAIKAISU)
                      ELSE
                        EVALUATE  WRK-PARA-JIBAIPRTKBN
                          WHEN      "0"
                            IF    WK-SYUTEN       NOT =   ZERO
                               ADD  WK-ZAIKAISU  TO  RECE82-KAISU(14)
                            END-IF
                            COMPUTE   RECE82-TENSU(14)  =
                                      RECE82-TENSU(14)  +
                                     (WK-SYUTEN   *   ACCT-ZAIKAISU)
                          WHEN      "1"
                            COMPUTE   RECE82-TENSU(22)  =
                                      RECE82-TENSU(22)  +
                                     (WK-SYUTEN   *   ACCT-ZAIKAISU)
                        END-EVALUATE
                      END-IF
                      EVALUATE  WRK-PARA-JIBAIPRTKBN
                        WHEN      "0"
                          COMPUTE   RECE82-TENSU-YAK(15)  =
                                    RECE82-TENSU-YAK(15)  +
                                   (WK-YKZTEN   *   WK-ZAIKAISU)
                        WHEN      "1"
                          COMPUTE   RECE82-TENSU-YAK(23)  =
                                    RECE82-TENSU-YAK(23)  +
                                   (WK-YKZTEN   *   WK-ZAIKAISU)
                      END-EVALUATE
                     END-IF
                    END-IF                                              
                   END-IF
                  END-IF
           END-EVALUATE
           .
      *
       220222-RECE-LEFT-SET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷£²¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       220223-RECE-LEFT-SET3-SEC               SECTION.
      *
           MOVE        SRY-SRYCD(IDX3)     TO   WRK-SRYCD
           MOVE        RECE85-SRYYM        TO   WK-TAIHI-SRYYM
           MOVE        WK-TAIHI-DAY        TO   WK-TAIHI-SRYD
           PERFORM     900-TENSU-TBL-SEC
           EVALUATE    SRY-SRYKBN
      *    ½é¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "11"
                 EVALUATE   RECE82-JIBAIKBN
                  WHEN       "1"
                   IF   TNS-DATAKBN  =   2
                     EVALUATE    TNS-TIMEKSNKBN
                         WHEN    1
                         WHEN    5
                         WHEN    6
                             MOVE  "1"     TO   RECE82-SHOSHINTIME
                         WHEN    3
                             MOVE  "2"     TO   RECE82-SHOSHINTIME
                         WHEN    4
                             MOVE  "3"     TO   RECE82-SHOSHINTIME
                     END-EVALUATE
                     COMPUTE    RECE82-TENSU(1)    =
                                RECE82-TENSU(1)    +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                     COMPUTE    TBL-SYUTEN-JI(1)   =
                                TBL-SYUTEN-JI(1)   +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                     IF  SRY-SRYCD(IDX3)  =  CONS-NYUKSN1  OR
                                             CONS-NYUKSN2  OR
                                             CONS-NYUKSN3  OR
                                             CONS-NYUKSN4  OR
                                             CONS-NYUKSN5  OR
                                             CONS-NYUKSN6  OR
                                             CONS-NYUKSN7  OR
                                             CONS-NYUKSN8  OR
                                             CONS-NYUKSN9
                             MOVE  "1"     TO   RECE82-SHOSHIN-NYU
                     END-IF
                   END-IF
                   IF   SRY-SRYCD(IDX3) = CONS-ROUSAI1  OR
                                          CONS-ROUSAI21
     ****               COMPUTE    RECE82-SONOTAKIN-JYU  =
     ****                          RECE82-SONOTAKIN-JYU  +
     ****                          (TNS-TEN         *  ACCT-ZAIKAISU)
                        COMPUTE    WRK-KINGAKU-JI   =
                                   WRK-KINGAKU-JI   +
                                   (TNS-TEN         *  ACCT-ZAIKAISU)
                   END-IF
                   IF   SRY-SRYCD(IDX3) = CONS-ROUSAI2
     ****               COMPUTE    RECE82-SONOTAKIN-JYU  =
     ****                          RECE82-SONOTAKIN-JYU  +
     ****                          (TNS-TEN         *  ACCT-ZAIKAISU)
                        COMPUTE    WRK-KINGAKU-JI   =
                                   WRK-KINGAKU-JI   +
                                   (TNS-TEN         *  ACCT-ZAIKAISU)
                   END-IF  
                  WHEN       "2"
                   IF   TNS-DATAKBN  =   2
                     EVALUATE    TNS-TIMEKSNKBN
                         WHEN    1
                         WHEN    5
                         WHEN    6
                             MOVE  "1"     TO   RECE82-SHOSHINTIME
                         WHEN    3
                             MOVE  "2"     TO   RECE82-SHOSHINTIME
                         WHEN    4
                             MOVE  "3"     TO   RECE82-SHOSHINTIME
                     END-EVALUATE
                     IF  SRY-SRYCD(IDX3)  =  CONS-NYUKSN1  OR
                                             CONS-NYUKSN2  OR
                                             CONS-NYUKSN3  OR
                                             CONS-NYUKSN4  OR
                                             CONS-NYUKSN5  OR
                                             CONS-NYUKSN6  OR
                                             CONS-NYUKSN7  OR
                                             CONS-NYUKSN8  OR
                                             CONS-NYUKSN9
                             MOVE  "1"     TO   RECE82-SHOSHIN-NYU
                     END-IF
                   END-IF
                   COMPUTE    RECE82-TENSU(1)    =
                              RECE82-TENSU(1)    +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                   COMPUTE    TBL-SYUTEN-JI(1)   =
                              TBL-SYUTEN-JI(1)   +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                 END-EVALUATE
      *    »ØÆ³¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "13"
                 EVALUATE   RECE82-JIBAIKBN
                  WHEN       "1"
                   IF ( SRY-SRYCD(IDX3) =   CONS-ROUSAI4  OR
                                            CONS-ROUSAI5  OR
                                            CONS-ROUSAI6  OR
                                            CONS-ROUSAI7  OR
                                            CONS-ROUSAI8  OR
                                            CONS-ROUSAI9  OR
                                            CONS-ROUSAI10 OR
                                            CONS-ROUSAI11 OR
                                            CONS-ROUSAI12    )
                   OR ((SRY-SRYCD(IDX3) =   CONS-ROUSAI3    ) AND
                       (RECE82-SRYYM    <   "200804"        ))
                         COMPUTE    RECE82-SONOTAKIN-JYU  =
                                    RECE82-SONOTAKIN-JYU  +
                                   (TNS-TEN   *   ACCT-ZAIKAISU)
                   ELSE
                     IF   ACCT-ZAIKBN     =  2
                         COMPUTE    WRK-RECE82G-TENSU(2)  =
                                    WRK-RECE82G-TENSU(2)  +
                                   (WK-ZAITEN *   WK-ZAIKAISU)
                     ELSE
                         COMPUTE    RECE82-TENSU(2)  =
                                    RECE82-TENSU(2)  +
                                   (WK-ZAITEN *   WK-ZAIKAISU)
                         COMPUTE    TBL-SYUTEN-JI(2) =
                                    TBL-SYUTEN-JI(2) +
                                   (WK-ZAITEN *   WK-ZAIKAISU)
                     END-IF
                   END-IF                   
                  WHEN       "2"
                     IF   ACCT-ZAIKBN     =  2
                         COMPUTE    WRK-RECE82G-TENSU(2)  =
                                    WRK-RECE82G-TENSU(2)  +
                                   (WK-ZAITEN *   WK-ZAIKAISU)
                     ELSE
                         COMPUTE    RECE82-TENSU(2)  =
                                    RECE82-TENSU(2)  +
                                   (WK-ZAITEN *   WK-ZAIKAISU)
                         COMPUTE    TBL-SYUTEN-JI(2) =
                                    TBL-SYUTEN-JI(2) +
                                   (WK-ZAITEN *   WK-ZAIKAISU)
                     END-IF
                 END-EVALUATE
      *    ºßÂð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "14"
                 IF  ACCT-ZAIKBN   =    2
                   EVALUATE  WRK-PARA-JIBAIPRTKBN
                     WHEN      "0"
                       COMPUTE    WRK-RECE82G-TENSU(28) =
                                  WRK-RECE82G-TENSU(28) +
                                 (WK-ZAITEN     *   WK-ZAIKAISU)
                     WHEN      "1"
                       COMPUTE    WRK-RECE82G-TENSU(27) =
                                  WRK-RECE82G-TENSU(27) +
                                 (WK-ZAITEN     *   WK-ZAIKAISU)
                   END-EVALUATE
                 ELSE
                   EVALUATE  WRK-PARA-JIBAIPRTKBN
                     WHEN      "0"
                       COMPUTE    RECE82-TENSU(28)  =
                                  RECE82-TENSU(28)  +
                                 (WK-SYUTEN     *   WK-ZAIKAISU)
                       COMPUTE    TBL-SYUTEN-JI(28) =
                                  TBL-SYUTEN-JI(28) +
                                 (WK-SYUTEN     *   WK-ZAIKAISU)
                       COMPUTE    RECE82-TENSU(28)  =
                                  RECE82-TENSU(28)  +
                                 (WK-KIZAITEN   *   WK-ZAIKAISU)  +
                                 (WK-YKZTEN     *   WK-ZAIKAISU)
                       COMPUTE    TBL-YKZTEN-JI(28) =
                                  TBL-YKZTEN-JI(28) +
                                 (WK-KIZAITEN   *   WK-ZAIKAISU)  +
                                 (WK-YKZTEN     *   WK-ZAIKAISU)
                     WHEN      "1"
                       COMPUTE    RECE82-TENSU(27)  =
                                  RECE82-TENSU(27)  +
                                 (WK-SYUTEN     *   WK-ZAIKAISU)
                       COMPUTE    TBL-SYUTEN-JI(27) =
                                  TBL-SYUTEN-JI(27) +
                                 (WK-SYUTEN     *   WK-ZAIKAISU)
                       COMPUTE    RECE82-TENSU(28)  =
                                  RECE82-TENSU(28)  +
                                 (WK-KIZAITEN   *   WK-ZAIKAISU)  +
                                 (WK-YKZTEN     *   WK-ZAIKAISU)
                       COMPUTE    TBL-YKZTEN-JI(28) =
                                  TBL-YKZTEN-JI(28) +
                                 (WK-KIZAITEN   *   WK-ZAIKAISU)  +
                                 (WK-YKZTEN     *   WK-ZAIKAISU)
                   END-EVALUATE
                 END-IF
      *    ÅêÌô¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF   SRY-SRYCD(IDX3)         =    CONS-YAKTEIGEN
                           COMPUTE   RECE82-TENSU(4)  =
                                     RECE82-TENSU(4)  -
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                           COMPUTE   TBL-YKZTEN-JI(4) =
                                     TBL-YKZTEN-JI(4) -
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                           ADD   WK-ZAIKAISU   TO   RECE82-KAISU(4)
                           COMPUTE   RECE82-TENSU(4)  =
                                     RECE82-TENSU(4)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                           COMPUTE   TBL-YKZTEN-JI(4) =
                                     TBL-YKZTEN-JI(4) +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)     =    CONS-FUNSAI-COMMENT
                            ADD   WK-ZAIKAISU  TO   RECE82-KAISU(4)
                            COMPUTE   RECE82-TENSU(4)  =
                                      RECE82-TENSU(4)  +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                            COMPUTE   TBL-YKZTEN-JI(4) =
                                      TBL-YKZTEN-JI(4) +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF   
                   END-IF
               WHEN    "22"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(5)
                   COMPUTE   RECE82-TENSU(5)  =
                             RECE82-TENSU(5)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(5) =
                             TBL-YKZTEN-JI(5) +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
               WHEN    "23"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(6)
                   COMPUTE   RECE82-TENSU(6)  =
                             RECE82-TENSU(6)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(6) =
                             TBL-YKZTEN-JI(6) +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    Ä´ºÞ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "24"
                   IF   WRK-PARA-JIBAIPRTKBN =  "0"
                     IF    RECE82-KHNTEN(7)   =   ZERO
                         MOVE  TNS-TEN    TO    RECE82-KHNTEN(7)
                     ELSE
                      IF   RECE82-KHNTEN(7) NOT = TNS-TEN
                      AND  TNS-TEN          NOT = ZERO
                           MOVE   ZERO    TO    RECE82-KHNTEN(7)
                           MOVE   1       TO    FLG-KHNTEN7-JI
                      END-IF
                     END-IF
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   =   "8"  )
                   OR  (SRY-SRYCD(IDX3)(1:3)   =   "008")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(7)
                   END-IF
                   COMPUTE   RECE82-TENSU(7)  =
                             RECE82-TENSU(7)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(7) =
                             TBL-SYUTEN-JI(7) +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    ËãÆÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "26"
                   IF   WRK-PARA-JIBAIPRTKBN =  "0"
                     IF    RECE82-KHNTEN(8)   =   ZERO
                         MOVE  TNS-TEN    TO    RECE82-KHNTEN(8)
                     ELSE
                      IF   RECE82-KHNTEN(8) NOT = TNS-TEN
                      AND  TNS-TEN          NOT = ZERO
                           MOVE   ZERO    TO    RECE82-KHNTEN(8)
                           MOVE   1       TO    FLG-KHNTEN8-JI
                      END-IF
                     END-IF
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   =   "8"  )
                   OR  (SRY-SRYCD(IDX3)(1:3)   =   "008")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(8)
                   END-IF
                   COMPUTE   RECE82-TENSU(8)  =
                             RECE82-TENSU(8)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(8) =
                             TBL-SYUTEN-JI(8) +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    Ä´´ð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "27"
                   IF   WRK-PARA-JIBAIPRTKBN =  "0"
                     IF    RECE82-KHNTEN(9)   =   ZERO
                         MOVE  TNS-TEN    TO    RECE82-KHNTEN(9)
                     ELSE
                      IF   RECE82-KHNTEN(9) NOT = TNS-TEN
                      AND  TNS-TEN          NOT = ZERO
                           MOVE   ZERO    TO    RECE82-KHNTEN(9)
                           MOVE   1       TO    FLG-KHNTEN9-JI
                      END-IF
                     END-IF
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   =   "8"  )
                   OR  (SRY-SRYCD(IDX3)(1:3)   =   "008")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(9)
                   END-IF
                   COMPUTE   RECE82-TENSU(9)  =
                             RECE82-TENSU(9)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(9) =
                             TBL-SYUTEN-JI(9) +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    Èé²¼¶ÚÆùÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "31"
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(11)
                   COMPUTE   RECE82-TENSU(11)   =
                             RECE82-TENSU(11)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(11)  =
                             TBL-SYUTEN-JI(11)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(11)   =
                             RECE82-TENSU(11)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(11)  =
                             TBL-YKZTEN-JI(11)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ÀÅÌ®ÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "32"
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(12)
                   COMPUTE   RECE82-TENSU(12)   =
                             RECE82-TENSU(12)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(12)  =
                             TBL-SYUTEN-JI(12)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(12)   =
                             RECE82-TENSU(12)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(12)  =
                             TBL-YKZTEN-JI(12)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾Ãí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "33"
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(13)
                   COMPUTE   RECE82-TENSU(13)   =
                             RECE82-TENSU(13)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(13)  =
                             TBL-SYUTEN-JI(13)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(13)   =
                             RECE82-TENSU(13)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(13)  =
                             TBL-YKZTEN-JI(13)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ½èÃÖ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(15)
                   END-IF
                   COMPUTE   RECE82-TENSU(15)   =
                             RECE82-TENSU(15)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(15)  =
                             TBL-SYUTEN-JI(15)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(16)   =
                             RECE82-TENSU(16)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(16)  =
                             TBL-YKZTEN-JI(16)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¼ê½Ñ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(18)
                   END-IF
                   COMPUTE   RECE82-TENSU(18)   =
                             RECE82-TENSU(18)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(18)  =
                             TBL-SYUTEN-JI(18)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(19)   =
                             RECE82-TENSU(19)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(19)  =
                             TBL-YKZTEN-JI(19)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¸¡ºº¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "60"
               WHEN    "64"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(21)
                   END-IF
                   COMPUTE   RECE82-TENSU(21)   =
                             RECE82-TENSU(21)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(21)  =
                             TBL-SYUTEN-JI(21)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    TNS-KNSJISKBN    =   2
                      COMPUTE   RECE82-KAISU(21)  =
                                RECE82-KAISU(21)  -  
                                WK-ZAIKAISU
                   END-IF
                   IF    TNS-SRYCD        =   CONS-SAIKETSU1  OR
                                              CONS-SAIKETSU2  OR
                                              CONS-SAIKETSU3
                         IF   RECE82-KAISU(21)  NOT =  ZERO
                              COMPUTE   RECE82-KAISU(21)  =
                                        RECE82-KAISU(21)  -   
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE82-TENSU(22)   =
                             RECE82-TENSU(22)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(22)  =
                             TBL-YKZTEN-JI(22)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ²èÁü¿ÇÃÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(24)
                   END-IF
                   COMPUTE   RECE82-TENSU(24)   =
                             RECE82-TENSU(24)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(24)  =
                             TBL-SYUTEN-JI(24)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(25)   =
                             RECE82-TENSU(25)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(25)  =
                             TBL-YKZTEN-JI(25)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "80"
                  IF ( SRY-SRYCD(IDX3) =   CONS-ROUSAI13  OR
                                           CONS-ROUSAI14  OR
                                           CONS-ROUSAI15  OR
                                           CONS-ROUSAI16  OR
                                           CONS-ROUSAI17  OR
                                           CONS-ROUSAI18  OR
                                           CONS-ROUSAI19  OR
                                           CONS-ROUSAI20 )
                  OR ((ACCT-SRYKBN          =  "80"     )  AND
                      (SRY-SRYCD(IDX3)(1:5) =  "09594"  ))
                       COMPUTE    RECE82-SITURYO-JYU  =
                                  RECE82-SITURYO-JYU  +
                                 (TNS-TEN   *   ACCT-ZAIKAISU)
                       EVALUATE   SRY-SRYCD(IDX3)
                              WHEN   CONS-ROUSAI13
                              WHEN   CONS-ROUSAI14
                                  MOVE    1     TO   WRK-HEYA-NINZU
                              WHEN   CONS-ROUSAI15
                              WHEN   CONS-ROUSAI16
                                  MOVE    2     TO   WRK-HEYA-NINZU
                              WHEN   CONS-ROUSAI17
                              WHEN   CONS-ROUSAI18
                                  MOVE    3     TO   WRK-HEYA-NINZU
                              WHEN   CONS-ROUSAI19
                              WHEN   CONS-ROUSAI20
                                  MOVE    4     TO   WRK-HEYA-NINZU
                       END-EVALUATE
                       EVALUATE   SRY-SRYCD(IDX3)(1:7)
                              WHEN   "0959401"
                              WHEN   "0959402"
                                  MOVE    1     TO   WRK-HEYA-NINZU
                              WHEN   "0959403"
                              WHEN   "0959404"
                                  MOVE    2     TO   WRK-HEYA-NINZU
                              WHEN   "0959407"
                              WHEN   "0959408"
                                  MOVE    3     TO   WRK-HEYA-NINZU
                              WHEN   "0959409"
                              WHEN   "0959410"
                                  MOVE    4     TO   WRK-HEYA-NINZU
                       END-EVALUATE
                       IF    RECE82-HEYA-NINZU   =   ZERO
                       AND   FLG-HEYA            =   ZERO
                           MOVE  WRK-HEYA-NINZU  TO   RECE82-HEYA-NINZU
                           ADD   ACCT-ZAIKAISU   TO   RECE82-HEYA-NISSU
                       ELSE
                           IF  RECE82-HEYA-NINZU   =  WRK-HEYA-NINZU
                           AND FLG-HEYA            =   ZERO
                              ADD  ACCT-ZAIKAISU TO   RECE82-HEYA-NISSU
                           ELSE
                              MOVE  ZERO         TO   RECE82-HEYA-NINZU
                              ADD  ACCT-ZAIKAISU TO   RECE82-HEYA-NISSU
                              MOVE  1            TO   FLG-HEYA
                           END-IF
                       END-IF
                  ELSE
                   IF ( ACCT-SRYKBN          =  "80"    )   AND
                      ( SRY-SRYCD(IDX3)(1:5) =  "09591" OR 
                                                "09592" )
                       CONTINUE
                   ELSE
                    IF ((ACCT-SRYKBN          =  "80"   )  AND
                        (SRY-SRYCD(IDX3)(1:5) =  "09593"))
                    OR ( SRY-SRYCD(IDX3) >= "101910110" AND
                                         <= "101910400"  )
                     IF  SRY-SRYCD(IDX3)(1:5) =  "09593"
                      EVALUATE   RECE82-JIBAIKBN
                       WHEN       "1"
                         EVALUATE  WRK-JIBAIKZI-SYUKEIKBN(IDX3)
                           WHEN       0
                           WHEN       5
                             COMPUTE  RECE82-SONOTAKIN-JYU =
                                      RECE82-SONOTAKIN-JYU +
                               (WRK-JIBAIKZI-TNS(IDX3) * ACCT-ZAIKAISU)
                           WHEN       1
                           WHEN       4
                             COMPUTE  WRK-KINGAKU-JI =
                                      WRK-KINGAKU-JI +
                               (WRK-JIBAIKZI-TNS(IDX3) * ACCT-ZAIKAISU)
                         END-EVALUATE
                       WHEN       "2"
                             COMPUTE  RECE82-SONOTAKIN-JYU =
                                      RECE82-SONOTAKIN-JYU +
                               (WRK-JIBAIKZI-TNS(IDX3) * ACCT-ZAIKAISU)
                      END-EVALUATE
                     ELSE
                             COMPUTE  RECE82-SONOTAKIN-JYU  =
                                      RECE82-SONOTAKIN-JYU  +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                     END-IF
                    ELSE                                                
                     IF  RECE82-JIBAIKBN      =    "1"
                     AND SRY-SRYCD(IDX3)      =    CONS-RYOYOTEATE
                       COMPUTE    RECE82-SONOTAKIN-JYU  =               
                                  RECE82-SONOTAKIN-JYU  +               
                                 (WRK-RYOYOTEATE  *   ACCT-ZAIKAISU)    
                     ELSE
                      IF    TNS-NYUTENTTLKBN   =    130
                            COMPUTE   RECE82-TENSU(2)   =
                                      RECE82-TENSU(2)   +
                                     (TNS-TEN   *   ACCT-ZAIKAISU)
                            COMPUTE   TBL-SYUTEN-JI(2)  =
                                      TBL-SYUTEN-JI(2)  +
                                     (TNS-TEN   *   ACCT-ZAIKAISU)
                      ELSE
                        EVALUATE  WRK-PARA-JIBAIPRTKBN
                          WHEN      "0"
                            IF    WK-SYUTEN       NOT =   ZERO
                               ADD  WK-ZAIKAISU  TO  RECE82-KAISU(15)
                            END-IF
                            COMPUTE   RECE82-TENSU(15)   =
                                      RECE82-TENSU(15)   +
                                     (WK-SYUTEN   *   ACCT-ZAIKAISU)
                            COMPUTE   TBL-SYUTEN-JI(15)  =
                                      TBL-SYUTEN-JI(15)  +
                                     (WK-SYUTEN   *   ACCT-ZAIKAISU)
                          WHEN      "1"
                            COMPUTE   RECE82-TENSU(27)   =
                                      RECE82-TENSU(27)   +
                                     (WK-SYUTEN   *   ACCT-ZAIKAISU)
                            COMPUTE   TBL-SYUTEN-JI(27)  =
                                      TBL-SYUTEN-JI(27)  +
                                     (WK-SYUTEN   *   ACCT-ZAIKAISU)
                        END-EVALUATE
                      END-IF
                      EVALUATE  WRK-PARA-JIBAIPRTKBN
                        WHEN      "0"
                         IF  SRY-SRYCD(IDX3) = CONS-RYOYOTEATE
                          COMPUTE   RECE82-TENSU(28)  =
                                    RECE82-TENSU(28)  +
                                   (WK-YKZTEN   *   WK-ZAIKAISU)
                          COMPUTE   TBL-YKZTEN-JI(28)  =
                                    TBL-YKZTEN-JI(28)  +
                                   (WK-YKZTEN   *   WK-ZAIKAISU)
                         ELSE
                          COMPUTE   RECE82-TENSU(16)  =
                                    RECE82-TENSU(16)  +
                                   (WK-YKZTEN   *   WK-ZAIKAISU)
                          COMPUTE   TBL-YKZTEN-JI(16)  =
                                    TBL-YKZTEN-JI(16)  +
                                   (WK-YKZTEN   *   WK-ZAIKAISU)
                         END-IF
                        WHEN      "1"
                          COMPUTE   RECE82-TENSU(28)  =
                                    RECE82-TENSU(28)  +
                                   (WK-YKZTEN   *   WK-ZAIKAISU)
                          COMPUTE   TBL-YKZTEN-JI(28)  =
                                    TBL-YKZTEN-JI(28)  +
                                   (WK-YKZTEN   *   WK-ZAIKAISU)
                      END-EVALUATE
                     END-IF
                    END-IF                                              
                   END-IF
                  END-IF
           END-EVALUATE
           .
      *
       220223-RECE-LEFT-SET3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷£²¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       220224-RECE-LEFT-SET4-SEC               SECTION.
      *
           MOVE        SRY-SRYCD(IDX3)     TO   WRK-SRYCD
           MOVE        RECE85-SRYYM        TO   WK-TAIHI-SRYYM
           MOVE        WK-TAIHI-DAY        TO   WK-TAIHI-SRYD
           PERFORM     900-TENSU-TBL-SEC
           EVALUATE    SRY-SRYKBN
      *    ½é¿Ç¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "11"
                   IF   SRY-SRYCD(IDX3) = CONS-ROUSAI1  OR
                                          CONS-ROUSAI21 OR
                                          CONS-ROUSAI2
                      CONTINUE
                   ELSE
                      IF   TNS-DATAKBN  =   2
                        EVALUATE    TNS-TIMEKSNKBN
                            WHEN    1
                            WHEN    5
                            WHEN    6
                                MOVE  "1"     TO   RECE82-SHOSHINTIME
                            WHEN    3
                                MOVE  "2"     TO   RECE82-SHOSHINTIME
                            WHEN    4
                                MOVE  "3"     TO   RECE82-SHOSHINTIME
                        END-EVALUATE
                      END-IF
                      IF    RECE82-KOMUKBN  =   "2"
                        IF  TNS-CHUKSNTUBAN  NOT = "0"
                            CONTINUE
                        ELSE
                            ADD   WK-ZAIKAISU  TO   RECE82-KAISU(1)
                        END-IF
                      ELSE
                        IF  TNS-DATAKBN  =   2
                          IF   SRY-SRYCD(IDX3) = "111012370"
                            CONTINUE
                          ELSE
                            ADD   WK-ZAIKAISU  TO   RECE82-KAISU(1)
                          END-IF
                        END-IF
                      END-IF
                      COMPUTE   RECE82-TENSU(1)    =
                                RECE82-TENSU(1)    +
                               (TNS-TEN   *   ACCT-ZAIKAISU)
                   END-IF
      *    »ØÆ³¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "13"
                   IF ( SRY-SRYCD(IDX3) =   CONS-ROUSAI4  OR
                                            CONS-ROUSAI5  OR
                                            CONS-ROUSAI6  OR
                                            CONS-ROUSAI7  OR
                                            CONS-ROUSAI8  OR
                                            CONS-ROUSAI9  OR
                                            CONS-ROUSAI10 OR
                                            CONS-ROUSAI11 OR
                                            CONS-ROUSAI12    )
                   OR ((SRY-SRYCD(IDX3) =   CONS-ROUSAI3    ) AND
                       (RECE82-SRYYM    <   "200804"        ))
                         CONTINUE
                   ELSE
                     IF   ACCT-ZAIKBN     =  2
                         COMPUTE    WRK-RECE82G-TENSU(9)  =
                                    WRK-RECE82G-TENSU(9)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                         COMPUTE    RECE82-TENSU(9)   =
                                    RECE82-TENSU(9)   +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   END-IF
      *    ºßÂð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "14"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE    WRK-RECE82G-TENSU(14) =
                              WRK-RECE82G-TENSU(14) +
                             (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   MOVE      ZERO            TO   FLG-ZAITAKU
      *            ±ý¿Ç¡¦ÆÃÊÌ±ý¿Ç¡Ê»þ´Ö²Ã»»¡Ë
                   IF    FLG-OSINKBN         =   1
                       MOVE         1        TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(10)
                       COMPUTE   RECE82-TENSU(10)   =
                                 RECE82-TENSU(10)   +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            Ìë´Ö
                   IF    FLG-OSINKBN         =    2
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(11)
                       COMPUTE   RECE82-TENSU(11)   =
                                 RECE82-TENSU(11)   +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            ¿¼Ìë¡¦¶ÛµÞ
                   IF    FLG-OSINKBN         =    3
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(12)
                       COMPUTE   RECE82-TENSU(12)   =
                                 RECE82-TENSU(12)   +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅ
                   IF (( TNS-OSINKBN           =     1          )  AND
                       ((SRY-SRYCD(IDX3)   NOT =    "114000110")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001610")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011670")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000370")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002070")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011770")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114012070")))
                   OR (  FLG-OSINKBN           =     9           )
      ***          OR (  SRY-SRYCD(IDX3) =    "114001110"        )
      ***          OR (  SRY-SRYCD(IDX3) =    "114012910"        )
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(13)
                       COMPUTE   RECE82-TENSU(13)   =
                                 RECE82-TENSU(13)   +
                                (WK-SYUTEN         * ACCT-ZAIKAISU)
                   END-IF
      *            ¤½¤ÎÂ¾
                   IF    FLG-ZAITAKU    =   ZERO
                       COMPUTE   RECE82-TENSU(14)   =
                                 RECE82-TENSU(14)   +
                                (WK-SYUTEN     *   WK-ZAIKAISU)
                   END-IF
      *            ÌôºÞ
                   COMPUTE   RECE82-TENSU(15)   =
                             RECE82-TENSU(15)   +
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
                     IF   SRY-SRYCD(IDX3)         =    CONS-YAKTEIGEN
                           COMPUTE   RECE82-TENSU(16)  =
                                     RECE82-TENSU(16)  -
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                           ADD   WK-ZAIKAISU   TO   RECE82-KAISU(16)
                           COMPUTE   RECE82-TENSU(16)  =
                                     RECE82-TENSU(16)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)     =    CONS-FUNSAI-COMMENT
                            ADD   WK-ZAIKAISU  TO   RECE82-KAISU(16)
                            COMPUTE   RECE82-TENSU(16)  =
                                      RECE82-TENSU(16)  +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF   
                   END-IF
               WHEN    "22"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(18)
                   COMPUTE   RECE82-TENSU(18)  =
                             RECE82-TENSU(18)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
               WHEN    "23"
                   ADD    WK-ZAIKAISU    TO   RECE82-KAISU(20)
                   COMPUTE   RECE82-TENSU(20)  =
                             RECE82-TENSU(20)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    Ä´ºÞ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "24"
                   IF  (SRY-SRYCD(IDX3)(1:1)   =   "8"  )
                   OR  (SRY-SRYCD(IDX3)(1:3)   =   "008")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(22)
                   END-IF
                   COMPUTE   RECE82-TENSU(22)  =
                             RECE82-TENSU(22)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    ËãÆÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "26"
                   IF  (SRY-SRYCD(IDX3)(1:1)   =   "8"  )
                   OR  (SRY-SRYCD(IDX3)(1:3)   =   "008")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE82-KAISU(23)
                   END-IF
                   COMPUTE   RECE82-TENSU(23)  =
                             RECE82-TENSU(23)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    Ä´´ð¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "27"
                   COMPUTE   RECE82-TENSU(24)  =
                             RECE82-TENSU(24)  +
                            (TNS-TEN   *   ACCT-ZAIKAISU)
      *    Èé²¼¶ÚÆùÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "31"
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(25)
                   COMPUTE   RECE82-TENSU(25)  =
                             RECE82-TENSU(25)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ÀÅÌ®ÆâÃí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "32"
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(26)
                   COMPUTE   RECE82-TENSU(26)  =
                             RECE82-TENSU(26)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾Ãí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "33"
                   ADD    WK-ZAIKAISU     TO   RECE82-KAISU(27)
                   COMPUTE   RECE82-TENSU(27)  =
                             RECE82-TENSU(27)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    ½èÃÖ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(28)
                   END-IF
                   COMPUTE   RECE82-TENSU(28)   =
                             RECE82-TENSU(28)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(29)   =
                             RECE82-TENSU(29)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¼ê½Ñ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(30)
                   END-IF
                   COMPUTE   RECE82-TENSU(30)   =
                             RECE82-TENSU(30)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(31)   =
                             RECE82-TENSU(31)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¸¡ºº¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "60"
               WHEN    "64"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(32)
                   END-IF
                   COMPUTE   RECE82-TENSU(32)   =
                             RECE82-TENSU(32)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    TNS-KNSJISKBN    =   2
                         COMPUTE   RECE82-KAISU(32)  =
                                   RECE82-KAISU(32)  -
                                   WK-ZAIKAISU
                   END-IF
                   IF    TNS-SRYCD  =   CONS-SAIKETSU1  OR
                                        CONS-SAIKETSU2  OR
                                        CONS-SAIKETSU3
                         IF   RECE82-KAISU(32)  NOT =  ZERO
                              COMPUTE   RECE82-KAISU(32)  =
                                        RECE82-KAISU(32)  -
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE82-TENSU(33)   =
                             RECE82-TENSU(33)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ²èÁü¿ÇÃÇ¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE82-KAISU(34)
                   END-IF
                   COMPUTE   RECE82-TENSU(34)   =
                             RECE82-TENSU(34)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE82-TENSU(35)   =
                             RECE82-TENSU(35)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    ¤½¤ÎÂ¾¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
               WHEN    "80"
                   IF ( SRY-SRYCD(IDX3) =   CONS-ROUSAI13  OR
                                            CONS-ROUSAI14  OR
                                            CONS-ROUSAI15  OR
                                            CONS-ROUSAI16  OR
                                            CONS-ROUSAI17  OR
                                            CONS-ROUSAI18  OR
                                            CONS-ROUSAI19  OR
                                            CONS-ROUSAI20 )
                   OR ((ACCT-SRYKBN          =  "80"     )  AND
                       (SRY-SRYCD(IDX3)(1:5) =  "09594"  ))
                     CONTINUE
                   ELSE
                     IF ((ACCT-SRYKBN          =  "80"   )  AND
                         (SRY-SRYCD(IDX3)(1:5) =  "09593"))
                     OR ( SRY-SRYCD(IDX3) >= "101910110" AND
                                          <= "101910400"  )
                         IF   (FLG-TAISYO           =   1     )
                         AND  (SRY-SRYCD(IDX3)(1:5) =  "09593")
                          COMPUTE  WRK-ZAITENNASHI  =
                                   WRK-ZAITENNASHI  +
                                  (TNS-TEN     *   ACCT-ZAIKAISU)
                         END-IF
                     ELSE                
                       IF    TNS-NYUTENTTLKBN   =    130
                         COMPUTE   RECE82-TENSU(9)  =
                                   RECE82-TENSU(9)  +
                                  (TNS-TEN     *   ACCT-ZAIKAISU)
                       ELSE  
                         COMPUTE   RECE82-TENSU(37)  =
                                   RECE82-TENSU(37)  +
                                  (WK-SYUTEN   *   WK-ZAIKAISU)
                       END-IF
                       COMPUTE       RECE82-TENSU(38)  =
                                     RECE82-TENSU(38)  +
                                    (WK-YKZTEN   *   WK-ZAIKAISU)
                     END-IF
                   END-IF
           END-EVALUATE
           .
      *
       220224-RECE-LEFT-SET4-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºÞ¤Î¿ÇÎÅÆâÍÆ¥Á¥§¥Ã¥¯
      *****************************************************************
       2203-SINRYO-CHECK-SEC                 SECTION.
      *
           PERFORM   VARYING   IDU   FROM   1   BY   1
                      UNTIL  ( IDU   >   TBL-ZAICHECK-MAX )
                       OR    ( FLG-RINYAK  NOT =  ZERO    )
      *
              IF  ( IDU  =  1                                  )
              OR  ((IDU  =  2) AND (ACCT-SRYKBN = "60" OR "64"))
              OR  ((IDU  =  3) AND (ACCT-SRYKBN = "80"        ))
              OR  ((IDU  =  4) AND (ACCT-SRYKBN = "80"        ))
      *
                 INITIALIZE                  SRYACT-REC
                 MOVE RECE82-HOSPNUM     TO  SRY-HOSPNUM
                 MOVE RECE82-NYUGAIKBN   TO  SRY-NYUGAIKBN
                 MOVE RECE82-PTID        TO  SRY-PTID
                 MOVE RECE82-SRYYM       TO  SRY-SRYYM
                 MOVE ACCT-ZAINUM        TO  SRY-ZAINUM
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(1)
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(2)
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(3)
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(4)
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(5)
                 MOVE SRYACT-REC         TO  MCPDATA-REC
                 MOVE "tbl_sryact"       TO  MCP-TABLE
                 MOVE "key8"             TO  MCP-PATHNAME
grpsys           PERFORM 900-DBSELECT-SEC
                 IF      MCP-RC  =  ZERO
                     MOVE    "tbl_sryact"   TO  MCP-TABLE
                     MOVE    "key8"         TO  MCP-PATHNAME
                     PERFORM  900-SRYACT-READ-SEC
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
                 MOVE "tbl_sryact"   TO  MCP-TABLE
                 MOVE "key8"         TO  MCP-PATHNAME
                 PERFORM 900-CLOSE-SEC
      *
              END-IF
      *
           END-PERFORM
      *
           .
       2203-SINRYO-CHECK-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿ÇÎÅ²ñ·×ÉÕ²Ã¾ðÊó¸¡º÷
      **************************************************************
       2204-SRYACCTPLUS-KENSAKU-SEC           SECTION.
      *
           INITIALIZE                      SRYACCTPLUS-REC
           MOVE    ACCT-HOSPNUM       TO   ACCTP-HOSPNUM
           MOVE    ACCT-NYUGAIKBN     TO   ACCTP-NYUGAIKBN
           MOVE    ACCT-PTID          TO   ACCTP-PTID
           MOVE    ACCT-SRYKA         TO   ACCTP-SRYKA
           MOVE    ACCT-SRYYM         TO   ACCTP-SRYYM
           MOVE    ACCT-ZAINUM        TO   ACCTP-ZAINUM
           MOVE    1                  TO   ACCTP-RENNUM
           MOVE    SRYACCTPLUS-REC    TO   MCPDATA-REC
           MOVE    "tbl_sryacctplus"  TO   MCP-TABLE
           MOVE    "key"              TO   MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacctplus"  TO   MCP-TABLE
               MOVE    "key"              TO   MCP-PATHNAME
               PERFORM  900-SRYACCTPLUS-READ-SEC
               IF        FLG-SRYACCTPLUS   =   ZERO
                 IF   ACCTP-RSIKINGAKU  NOT = ZERO
                    MOVE    1   TO   FLG-TAISYO
                 END-IF
               END-IF
           END-IF
           MOVE    "tbl_sryacctplus"  TO   MCP-TABLE
           MOVE    "key"              TO   MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       2204-SRYACCTPLUS-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀÅ©¼êµ»²Ã»»¤Þ¤È¤áÂÐ¾Ý¤«¥Á¥§¥Ã¥¯
      *****************************************************************
       2205-TENTEKIMATOME-CHECK-SEC                 SECTION.
      *
           PERFORM  VARYING  ID4    FROM   1   BY   1
                     UNTIL  (ID4                      >   20 )
                       OR   (WRK-TENTEKI2-ZAINUM(ID4) =  ZERO)
                       OR   (FLG-TENTEKIMATOME        =    1 )
      *
             IF    ACCT-ZAINUM  =  WRK-TENTEKI2-ZAINUM(ID4)
                  MOVE     1    TO   FLG-TENTEKIMATOME
             END-IF
      *
           END-PERFORM
      *
           .
       2205-TENTEKIMATOME-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       230-RECE84-HENSYU2-SEC                SECTION.
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE82-HOSPNUM        TO      NACCT-HOSPNUM
           MOVE    RECE82-NYUGAIKBN      TO      NACCT-NYUGAIKBN
           MOVE    RECE82-PTID           TO      NACCT-PTID
           MOVE    RECE82-SRYYM          TO      NACCT-SRYYM
           MOVE    NYUINACCT-REC         TO      MCPDATA-REC
           MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
grpsys     PERFORM  900-DBSELECT-SEC
           IF      MCP-RC             =       ZERO
               MOVE    "tbl_nyuinacct"   TO      MCP-TABLE
               MOVE    "key7"            TO      MCP-PATHNAME
               PERFORM  900-NYUINACCT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACCT  =    "1")
      *
                 IF     NACCT-ZAISKBKBN    =    "1"  
                   IF     NACCT-SRYCDTOTAL    =    099999916
                       CONTINUE
                   ELSE
                     PERFORM    VARYING   IDY   FROM   1   BY   1
                                      UNTIL    IDY    >    31
                       IF   NACCT-DAY(IDY)   NOT =  ZERO 
                             MOVE   NACCT-DAY(IDY)  TO   WRK-DAY1(IDY) 
                       END-IF
                     END-PERFORM
                   END-IF
                 END-IF
      *
                 IF     NACCT-ZAISKBKBN    =    "2"  
                   PERFORM    VARYING   IDY   FROM   1   BY   1
                                    UNTIL    IDY    >    31
                     IF   NACCT-DAY(IDY)   NOT =  ZERO 
                             MOVE   NACCT-DAY(IDY)  TO   WRK-DAY2(IDY) 
                     END-IF
                   END-PERFORM
                 END-IF
      *
                 IF   ( NACCT-ZAISKBKBN    =    "4"       )
                 AND  ( NACCT-SRYCDTOTAL   =    197000570 )
                   PERFORM    VARYING   IDY   FROM   1   BY   1
                                    UNTIL    IDY    >    31
                     IF   NACCT-DAY(IDY)   NOT =  ZERO 
                             MOVE   NACCT-DAY(IDY)  TO   WRK-DAY4(IDY) 
                     END-IF
                   END-PERFORM
                 END-IF
      *
                 IF     NACCT-ZAISKBKBN    =    "5"  
                   PERFORM    VARYING   IDY   FROM   1   BY   1
                                    UNTIL    IDY    >    31
                     IF   NACCT-DAY(IDY)   NOT =  ZERO 
                             MOVE   NACCT-DAY(IDY)  TO   WRK-DAY5(IDY) 
                     END-IF
                   END-PERFORM
                 END-IF
      *
                 MOVE    "tbl_nyuinacct"   TO      MCP-TABLE
                 MOVE    "key7"            TO      MCP-PATHNAME
                 PERFORM  900-NYUINACCT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_nyuinacct"         TO      MCP-TABLE
           MOVE    "key7"                  TO      MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
      *
           MOVE    ZERO    TO    WRK-ZAINUM-CNT2
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE82-HOSPNUM        TO      NACCT-HOSPNUM
           MOVE    RECE82-NYUGAIKBN      TO      NACCT-NYUGAIKBN
           MOVE    RECE82-PTID           TO      NACCT-PTID
           MOVE    RECE82-SRYYM          TO      NACCT-SRYYM
           MOVE    NYUINACCT-REC         TO      MCPDATA-REC
           MOVE    "tbl_nyuinacct"       TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_nyuinacct"   TO      MCP-TABLE
               MOVE    "key7"            TO      MCP-PATHNAME
               PERFORM  900-NYUINACCT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACCT  =    "1") 
      *
                 IF     NACCT-ZAISKBKBN    =     "4"  OR  "6"  OR "7"
      *
                   MOVE     ZERO    TO    FLG-JYOGAI
                   IF    NACCT-ZAISKBKBN    =  "6"
                     PERFORM  VARYING  ID4    FROM   1   BY   1
                               UNTIL  (ID4                  >   50  )
                                 OR   (WRK-CHUKSNSRYCD(ID4) =  SPACE)
                                 OR   (FLG-JYOGAI           =   1   )
                       IF  NACCT-SRYCDTOTAL(6:9) = WRK-CHUKSNSRYCD(ID4)
                          MOVE     1    TO   FLG-JYOGAI
                       END-IF
                     END-PERFORM
                   END-IF
      *
                   IF ((NACCT-ZAISKBKBN    =  "6"                  ) AND
                       (NACCT-SRYCDTOTAL   = 190117070 OR 190109670) AND
                       (RECE82-SRYYM      >=  "200604"             ))
                   OR ((NACCT-ZAISKBKBN    =  "6"                  ) AND
                       (NACCT-SRYCDTOTAL   = 190126670 OR 190126770
                                          OR 190126570 OR 190126470) AND
                       (RECE82-SRYYM      >=  "200804"             ))
                   OR ((NACCT-ZAISKBKBN    =  "6"                  ) AND
                       (NACCT-SRYCDTOTAL   = 190129870             ) AND
                       (RECE82-SRYYM      >=  "200804"             ) AND
                       (RECE82-SRYYM      <=  "201003"             ))
                   OR ( FLG-JYOGAI         = 1                      )
      *
                      CONTINUE
      *
                   ELSE
      *
                      IF   NACCT-ZAISKBKBN  = "6" OR "7"
                         ADD    1         TO    WRK-ZAINUM-CNT2
                      END-IF
      *
                      INITIALIZE          WRK-DAY-TABLE2
                                          WRK-SKJ-TABLE
                                          WRK-SKJ-TABLE2
                      MOVE     ZERO       TO    CNT-DAY21
                                                CNT-DAY24
                      MOVE     ZERO       TO    FLG-DAY21
                                                FLG-DAY24
      *
                      MOVE     ZERO       TO    WRK-TEN
                                                WRK-MEISAI-CNT
                                                WRK-SRYCDTOTAL
                                                WRK-SURYOUTOTAL
      *
                      PERFORM  VARYING  IDY    FROM    1   BY  1
                                  UNTIL   IDY     >       31
      *
                         IF  RECE82-HKNCOMBI     =  WRK-DAY5(IDY)
                            IF  ((WRK-DAY1(IDY)   NOT  =  ZERO  ) OR
                                 (RECE82-NYUINDAY-KBN(IDY) = "1"))
                            AND ( NACCT-ZAITEN    NOT  =  ZERO   )
                            AND ( NACCT-ZAISKBKBN NOT  =  "4"    )
                            AND ( NACCT-DAY(IDY)  NOT  =  ZERO   )
                               IF  WRK-DAY2(IDY) =  ZERO  OR  4
                                                          OR  5
                                    MOVE   NACCT-DAY(IDY)  
                                           TO   WRK-DAY21(IDY) 
                                    ADD    1   TO   CNT-DAY21
                                    MOVE   1   TO   FLG-DAY21
                               END-IF
                            END-IF
      *                     *¿©»ö
                            IF  ((WRK-DAY1(IDY)   NOT  =  ZERO  ) OR
                                 (RECE82-NYUINDAY-KBN(IDY) = "1"))
                            AND ( NACCT-ZAITEN         =  ZERO   )
                            AND ( NACCT-ZAISKBKBN      =  "4"    )
                            AND ( NACCT-DAY(IDY)  NOT  =  ZERO   )
                            AND ( WRK-SHOKUJIRYOYO(IDY)
                                                  NOT  =  ZERO   )
                               IF  WRK-DAY2(IDY) =  ZERO  OR  4
                                                          OR  5
                                IF   RECE82-SRYYM  >= "200604"
                                 IF  NACCT-SRYCDTOTAL = 099999918
                                                    OR  099999919
                                                    OR  099999920
                                  MOVE   NACCT-DAY(IDY)  
                                            TO   WRK-DAY24(IDY) 
                                  PERFORM      2301-NYUINACCT-SYU-SEC
                                 END-IF
                                ELSE
                                 IF  NACCT-SRYCDTOTAL = 099999913
                                  MOVE   NACCT-DAY(IDY)  
                                            TO   WRK-DAY24(IDY) 
                                  PERFORM      2301-NYUINACCT-SYU-SEC
                                  ADD    1     TO   CNT-DAY24
                                  MOVE   1     TO   FLG-DAY24
                                 END-IF
                                END-IF
                               END-IF
                            END-IF
                            IF     RECE82-SRYYM        >= "200610"
                              IF  ((WRK-DAY1(IDY)   NOT  =  ZERO  ) OR
                                   (RECE82-NYUINDAY-KBN(IDY) = "1"))
                              AND ( NACCT-ZAITEN         =  ZERO   )
                              AND ( NACCT-ZAISKBKBN      =  "4"    )
      ***                     AND ( NACCT-DAY(IDY)  NOT  =  ZERO   )
                              AND ( WRK-SHOKUJIRYOYO(IDY)
                                                    NOT  =  ZERO   )
                              AND ( NACCT-SRYCDTOTAL = 099999918
                                                   OR  099999919
                                                   OR  099999920   )
                              AND ( WRK-DAY2(IDY) =  ZERO  OR  4
                                                           OR  5   )
                                PERFORM   2307-NYUINACCT-SYU-KAN-SEC
                              END-IF
                            END-IF
                         END-IF
      *
                      END-PERFORM
      *
                      IF   FLG-DAY21    =   1
                         PERFORM   2302-RECE84-TENSOU2-SEC
                         PERFORM   2303-RECE85-HENSYU2-SEC
                         PERFORM   900-RECE84-WRITE-SEC
                      END-IF
      *
                      IF   FLG-DAY24    =   1
                       IF   RECE82-SRYYM  >= "200604"
                         CONTINUE
                       ELSE
                         PERFORM    2304-NYUINACCT-HEN-SEC
                         PERFORM  VARYING  SYUKEI-IDX3  FROM  1  BY  1
                            UNTIL  SYUKEI-IDX3           >  31
                             OR    WRK-SKJ2-ZAITEN(SYUKEI-IDX3) = ZERO
      *
                             PERFORM   2305-RECE84-TENSOU2-SKJ-SEC
                             PERFORM   2306-RECE85-HENSYU2-SKJ-SEC
                             PERFORM   900-RECE84-WRITE-SEC
      *
                         END-PERFORM
                       END-IF
                      END-IF
      *
                   END-IF
      *
                 END-IF
      *
                 MOVE    "tbl_nyuinacct"   TO      MCP-TABLE
                 MOVE    "key7"            TO      MCP-PATHNAME
                 PERFORM  900-NYUINACCT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_nyuinacct"         TO      MCP-TABLE
           MOVE    "key7"                  TO      MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    Æþ±¡»þÀ¸³èÎÅÍÜÈñÉéÃ´³Û¼èÆÀ
           IF    RECE82-SRYYM   >=  "200610"
             INITIALIZE                   ORCSNYULIFE-AREA
             MOVE    RECE82-HOSPNUM   TO  LNK-NYULIFE-HOSPNUM
             MOVE    RECE82-PTID      TO  LNK-NYULIFE-PTID
             MOVE    RECE82-SRYYM     TO  LNK-NYULIFE-SANTEIYM
             CALL    "ORCSNYULIFE"        USING
                                          ORCSNYULIFE-AREA
                                          SPA-AREA
             IF      LNK-NYULIFE-RC   =   ZERO
               PERFORM  VARYING  SYUKEI-IDX3  FROM  1  BY  1
                         UNTIL   SYUKEI-IDX3   >   31
                 MOVE  LNK-NYULIFE-KBN(SYUKEI-IDX3)
                                  TO  WRK-NYULIFE-KBN(SYUKEI-IDX3)
               END-PERFORM
             END-IF
           END-IF
      *
      *    £²£°£°£¶Ç¯£´·î°Ê¹ß¤Î¿©»öÎÅÍÜ
           IF   RECE82-SRYYM   >=  "200604"
      *
              PERFORM  VARYING  SYUKEI-IDX3  FROM  1  BY  1
                        UNTIL   SYUKEI-IDX3    >  31
      *
                 IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 1)
                                                    NOT =  ZERO
                      PERFORM  2305-RECE84-TENSOU2-SKJ200604-SEC
                      PERFORM  2306-RECE85-HENSYU2-SKJ200604-SEC
                      PERFORM  900-RECE84-WRITE-SEC
                 END-IF
      *
                 IF   WRK-SKJ200604-DAY-KAN(SYUKEI-IDX3)
                                                    NOT =  ZERO
                 AND  WRK-NYULIFE-KBN(SYUKEI-IDX3)  NOT =  SPACE
                      PERFORM  2308-RECE84-TENSOU2-SKJ200610-KAN-SEC
                      PERFORM  2309-RECE85-HENSYU2-SKJ200610-KAN-SEC
                      PERFORM  900-RECE84-WRITE-SEC
                 END-IF
      *
              END-PERFORM
      *
           END-IF
      *
           .
       230-RECE84-HENSYU2-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿©»öÎÅÍÜÈñ¤Î½¸·×
      **************************************************************
       2301-NYUINACCT-SYU-SEC           SECTION.
      *
           IF   RECE82-SRYYM  >= "200604"
      *
             EVALUATE   WRK-SHOKUJIRYOYO(IDY)
              WHEN  1
                 EVALUATE   WRK-DAY24(IDY)
                   WHEN  1
                     MOVE    1       TO  WRK-SKJ200604-KBN(IDY)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 1)
                     IF    WRK-DAY4(IDY)  NOT =  ZERO
                       ADD   1       TO  WRK-SKJ200604-DAY(IDY 3)
                       MOVE  ZERO    TO  WRK-DAY4(IDY)
                     END-IF
                   WHEN  2
                     MOVE    1       TO  WRK-SKJ200604-KBN(IDY)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 1)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 2)
                     IF    WRK-DAY4(IDY)  NOT =  ZERO
                       ADD   1       TO  WRK-SKJ200604-DAY(IDY 3)
                       MOVE  ZERO    TO  WRK-DAY4(IDY)
                     END-IF
                 END-EVALUATE
              WHEN  2
                     MOVE    2       TO  WRK-SKJ200604-KBN(IDY)
                     ADD     1       TO  WRK-SKJ200604-DAY(IDY 1)
             END-EVALUATE
      *
           ELSE
      *
             MOVE       ZERO                TO   IDY2
      *
             EVALUATE   WRK-DAY24(IDY)
              WHEN  1
                EVALUATE   WRK-SHOKUJIRYOYO(IDY)
                  WHEN  1
      *              Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                     ADD    1             TO   WRK-SKJ-ZAIKAISU(IDY)
                     ADD    1             TO   IDY2
                     MOVE  "197000110"    TO   WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                     MOVE  "197000110"    TO   WRK-SRYCD-X
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    900-TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              Ï«ºÒ¾è¿ô
                     PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
      *              ÆÃÊÌ´ÉÍý²Ã»»
                     IF    WRK-TOKUKANRIKASAN(IDY)  =  1
                        ADD    1             TO   IDY2
                        MOVE  "197000270"    TO  WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                        MOVE  "197000270"    TO   WRK-SRYCD-X
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    900-TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 Ï«ºÒ¾è¿ô
                        PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
      *              ¿©Æ²²Ã»»
                     IF    WRK-DAY4(IDY)  NOT =  ZERO
                        ADD    1             TO   IDY2
                        MOVE  "197000570"    TO  WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                        MOVE  "197000570"    TO   WRK-SRYCD-X
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    900-TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 Ï«ºÒ¾è¿ô
                        PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
                  WHEN  2
      *              Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£²¡Ë
                     ADD    1             TO   WRK-SKJ-ZAIKAISU(IDY)
                     ADD    1             TO   IDY2
                     MOVE  "197000710"    TO   WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                     MOVE  "197000710"    TO   WRK-SRYCD-X
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    900-TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              Ï«ºÒ¾è¿ô
                     PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
                END-EVALUATE
              WHEN  2
      *              Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                     ADD    1             TO   WRK-SKJ-ZAIKAISU(IDY)
                     ADD    1             TO   IDY2
                     MOVE  "197000110"    TO   WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                     MOVE  "197000110"    TO   WRK-SRYCD-X
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    900-TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              Ï«ºÒ¾è¿ô
                     PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
      *              ÆÃÊÌ´ÉÍý²Ã»»
                     IF    WRK-TOKUKANRIKASAN(IDY)  =  1
                        ADD    1             TO   IDY2
                        MOVE  "197000270"    TO  WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                        MOVE  "197000270"    TO   WRK-SRYCD-X
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    900-TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 Ï«ºÒ¾è¿ô
                        PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
      *              ÆÃÊÌ¿©²Ã»»
                     ADD    1             TO   IDY2
                     MOVE  "197000470"    TO   WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                     MOVE  "197000470"    TO   WRK-SRYCD-X
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    900-TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              Ï«ºÒ¾è¿ô
                     PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
      *              ¿©Æ²²Ã»»
                     IF    WRK-DAY4(IDY)  NOT =  ZERO
                        ADD    1             TO   IDY2
                        MOVE  "197000570"    TO  WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                        MOVE  "197000570"    TO   WRK-SRYCD-X
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    900-TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 Ï«ºÒ¾è¿ô
                        PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
              WHEN  3
      *              Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                     ADD    1             TO   WRK-SKJ-ZAIKAISU(IDY)
                     ADD    1             TO   IDY2
                     MOVE  "197000110"    TO   WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                     MOVE  "197000110"    TO   WRK-SRYCD-X
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    900-TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              Ï«ºÒ¾è¿ô
                     PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
      *              ÆÃÊÌ´ÉÍý²Ã»»
                     IF    WRK-TOKUKANRIKASAN(IDY)  =  1
                        ADD    1             TO   IDY2
                        MOVE  "197000270"    TO  WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                        MOVE  "197000270"    TO   WRK-SRYCD-X
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    900-TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 Ï«ºÒ¾è¿ô
                        PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
      *              ¿©Æ²²Ã»»
                     IF    WRK-DAY4(IDY)  NOT =  ZERO
                        ADD    1             TO   IDY2
                        MOVE  "197000570"    TO  WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                        MOVE  "197000570"    TO   WRK-SRYCD-X
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    900-TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 Ï«ºÒ¾è¿ô
                        PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
      *              Ê£¿ô¸¥Î©²Ã»»
                     ADD    1             TO   IDY2
                     MOVE  "197000670"    TO   WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                     MOVE  "197000670"    TO   WRK-SRYCD-X
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    900-TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              Ï«ºÒ¾è¿ô
                     PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
              WHEN  4
      *              Æþ±¡»þ¿©»öÎÅÍÜ¡Ê£±¡Ë
                     ADD    1             TO   WRK-SKJ-ZAIKAISU(IDY)
                     ADD    1             TO   IDY2
                     MOVE  "197000110"    TO   WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                     MOVE  "197000110"    TO   WRK-SRYCD-X
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    900-TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              Ï«ºÒ¾è¿ô
                     PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
      *              ÆÃÊÌ´ÉÍý²Ã»»
                     IF    WRK-TOKUKANRIKASAN(IDY)  =  1
                        ADD    1             TO   IDY2
                        MOVE  "197000270"    TO  WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                        MOVE  "197000270"    TO   WRK-SRYCD-X
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    900-TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 Ï«ºÒ¾è¿ô
                        PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
      *              ÆÃÊÌ¿©²Ã»»
                     ADD    1             TO   IDY2
                     MOVE  "197000470"    TO   WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                     MOVE  "197000470"    TO   WRK-SRYCD-X
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    900-TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              Ï«ºÒ¾è¿ô
                     PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
      *              ¿©Æ²²Ã»»
                     IF    WRK-DAY4(IDY)  NOT =  ZERO
                        ADD    1             TO   IDY2
                        MOVE  "197000570"    TO  WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                        MOVE  "197000570"    TO   WRK-SRYCD-X
                        MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                        MOVE   "01"          TO   WK-TAIHI-SRYD
                        PERFORM    900-TENSU-TBL-SEC
                        IF       FLG-TENSU      =    ZERO
                           PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                        END-IF
      *                 Ï«ºÒ¾è¿ô
                        PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
                     END-IF
      *              Ê£¿ô¸¥Î©²Ã»»
                     ADD    1             TO   IDY2
                     MOVE  "197000670"    TO   WRK-SKJ-SRYCD-OCC
                                                             (IDY IDY2)
                     MOVE  "197000670"    TO   WRK-SRYCD-X
                     MOVE   RECE82-SRYYM  TO   WK-TAIHI-SRYYM
                     MOVE   "01"          TO   WK-TAIHI-SRYD
                     PERFORM    900-TENSU-TBL-SEC
                     IF       FLG-TENSU      =    ZERO
                        PERFORM  23011-NYUINACCT-SYU-KEISAN-SEC
                     END-IF
      *              Ï«ºÒ¾è¿ô
                     PERFORM     23012-NYUINACCT-SYU-JYOSU-SEC
             END-EVALUATE
      *
           END-IF
      *
           .
       2301-NYUINACCT-SYU-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿©»öÎÅÍÜÈñ¤Î·×»»
      **************************************************************
       23011-NYUINACCT-SYU-KEISAN-SEC           SECTION.
      *
           COMPUTE  WRK-KEISAN   =   TNS-TEN     *  1.2
           COMPUTE  WRK-KEISAN   =   WRK-KEISAN  +   5
           COMPUTE  WRK-KEISAN   =   WRK-KEISAN  /  10
           COMPUTE  WRK-KEISAN   =   WRK-KEISAN  *  10
           IF      RECE82-HKNKBN    =   "4"
           AND     RECE82-SKJKBN    =   "2"
               COMPUTE  WRK-KEISAN   =   WRK-KEISAN  *  1.2
           END-IF
           ADD      WRK-KEISAN  TO   WRK-SKJ-ZAITEN(IDY)
      *
           .
       23011-NYUINACCT-SYU-KEISAN-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿©»öÎÅÍÜÈñ¤ÎÏ«ºÒ¾è¿ô¥»¥Ã¥È
      **************************************************************
       23012-NYUINACCT-SYU-JYOSU-SEC           SECTION.
      *
           ADD    1             TO   IDY2
           MOVE  "101970010"    TO   WRK-SKJ-SRYCD-OCC (IDY IDY2)
           IF      RECE82-HKNKBN    =   "4"
           AND     RECE82-SKJKBN    =   "2"
      *       ¼«ÇåÀÕ¡Ê£±¡¥£²ÇÜ¡Ë
              ADD    1             TO   IDY2
      *       MOVE  "101970010"    TO   WRK-SKJ-SRYCD-OCC(IDY IDY2)
              MOVE  "099609901"    TO   WRK-SKJ-SRYCD-OCC(IDY IDY2)
           END-IF
      *
           .
       23012-NYUINACCT-SYU-JYOSU-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿©»öÎÅÍÜÈñ¤Î½¸·×¡Ê´Ä¶­¡Ë
      **************************************************************
       2307-NYUINACCT-SYU-KAN-SEC           SECTION.
      *
           EVALUATE   WRK-SHOKUJIRYOYO(IDY)
             WHEN   1
                MOVE    1       TO  WRK-SKJ200604-KBN(IDY)
                MOVE    1       TO  WRK-SKJ200604-DAY-KAN(IDY)
             WHEN   2
                MOVE    2       TO  WRK-SKJ200604-KBN(IDY)
                MOVE    1       TO  WRK-SKJ200604-DAY-KAN(IDY)
           END-EVALUATE
      *
           .
       2307-NYUINACCT-SYU-KAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       2302-RECE84-TENSOU2-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE84-REC
           INITIALIZE                           RECE84-REC
           MOVE    RECE82-HOSPNUM       TO      RECE84-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE84-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE84-PTID
           MOVE    RECE82-RECEKA        TO      RECE84-RECEKA
           MOVE    RECE82-HKNID         TO      RECE84-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE84-TEKSTYMD
      *
           EVALUATE  NACCT-ZAISKBKBN
            WHEN    "6"
            WHEN    "7"
             COMPUTE WRK-ZAINUM  = 99993000  +  WRK-ZAINUM-CNT2
             MOVE    WRK-ZAINUM         TO      RECE84-ZAINUM
            WHEN    OTHER
             MOVE    NACCT-ZAINUM       TO      RECE84-ZAINUM
           END-EVALUATE
      *
           MOVE    NACCT-SRYKBN         TO      RECE84-SRYKBN
           MOVE    NACCT-ZAISKBKBN      TO      RECE84-ZAISKBKBN
           IF      NACCT-ZAISKBKBN      =       "7"
      ****    IF   RECE82-HKNKBN        =       "4"
      ****      MOVE     "40"           TO      RECE84-SRYKBN
      ****    ELSE
                MOVE     "80"           TO      RECE84-SRYKBN
      ****    END-IF
              MOVE      SPACE           TO      RECE84-ZAISKBKBN
           END-IF
           MOVE    RECE82-PTNUM         TO      RECE84-PTNUM
      *
           MOVE    NACCT-SRYKA          TO      RECE84-SRYKA
           MOVE    RECE82-HKNCOMBI      TO      RECE84-HKNCOMBI
           MOVE    SPACE                TO      RECE84-JIHOKBN
           MOVE    NACCT-ZAITEN         TO      RECE84-ZAITEN
           MOVE    NACCT-SRYCDTOTAL     TO      RECE84-SRYCDTOTAL
           IF      NACCT-ZAISKBKBN      =       "7"
                MOVE    199000510       TO      RECE84-SRYCDTOTAL
           END-IF
           MOVE    NACCT-SURYOUTOTAL    TO      RECE84-SURYOUTOTAL
           MOVE    NACCT-MEISAISU       TO      RECE84-MEISAISU
           MOVE    CNT-DAY21            TO      RECE84-ZAIKAISU
           MOVE    NACCT-SYUTEN1        TO      RECE84-SYUTEN1
           MOVE    NACCT-SYUTEN2        TO      RECE84-SYUTEN2
           MOVE    NACCT-YKZTEN         TO      RECE84-YKZTEN
           MOVE    NACCT-KIZAITEN       TO      RECE84-KIZAITEN
           IF    RECE82-HKNKBN    =  "4"
           AND   RECE82-JIBAIKBN  =  "2"
           AND   NACCT-ZAISKBKBN  =  "7"
             MOVE  NACCT-SYUTEN1        TO      RECE84-KIZAITEN
             MOVE  ZERO                 TO      RECE84-SYUTEN1
           END-IF
           MOVE    NACCT-DRCD           TO      RECE84-DRCD
           MOVE    SPACE                TO      RECE84-ZAIREQFLG
           MOVE    ZERO                 TO      RECE84-TEIGENRATE
           MOVE        ZERO             TO      WK-TAIHI-DAY
           MOVE        ZERO             TO      WK-TAIHI-FLG
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >       31
                   MOVE    WRK-DAY21(IDX2)  TO
                                            RECE84-KAISU (1 IDX2)
                   IF     (WK-TAIHI-FLG           =   ZERO)   AND
                          (WRK-DAY21(IDX2)    NOT =   ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF    
           END-PERFORM
           .
       2302-RECE84-TENSOU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       2303-RECE85-HENSYU2-SEC               SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM      TO   NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN    TO   NSRY-NYUGAIKBN
           MOVE    NACCT-PTID         TO   NSRY-PTID
           MOVE    NACCT-SRYYM        TO   NSRY-SRYYM
           MOVE    NACCT-ZAINUM       TO   NSRY-ZAINUM
           MOVE    NYUINACT-REC       TO   MCPDATA-REC
           MOVE    "tbl_nyuinact"     TO   MCP-TABLE
           MOVE    "key9"             TO   MCP-PATHNAME
grpsys     PERFORM  900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_nyuinact"     TO   MCP-TABLE
               MOVE    "key9"             TO   MCP-PATHNAME
               PERFORM  900-NYUINACT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACT  =    "1")
                   PERFORM   23031-RECE85-TENSOU2-SEC
                   IF      NACCT-ZAISKBKBN      =       "7"
                      PERFORM   23032-RECE82-HENSYU2-SEC
                   END-IF
                   PERFORM   900-RECE85-WRITE-SEC
      *
                   MOVE    "tbl_nyuinact" TO   MCP-TABLE
                   MOVE    "key9"         TO   MCP-PATHNAME
                   PERFORM  900-NYUINACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_nyuinact"         TO   MCP-TABLE
           MOVE    "key9"                 TO   MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       2303-RECE85-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       23031-RECE85-TENSOU2-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE85-REC
           INITIALIZE                           RECE85-REC
           MOVE    RECE82-HOSPNUM       TO      RECE85-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE85-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE85-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE85-PTID
           MOVE    RECE82-RECEKA        TO      RECE85-RECEKA
           MOVE    RECE82-HKNID         TO      RECE85-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE85-TEKSTYMD
      *
           EVALUATE  NACCT-ZAISKBKBN
            WHEN    "6"
            WHEN    "7"
             COMPUTE WRK-ZAINUM  = 99993000  +  WRK-ZAINUM-CNT2
             MOVE    WRK-ZAINUM         TO      RECE85-ZAINUM
            WHEN    OTHER
             MOVE    NSRY-ZAINUM        TO      RECE85-ZAINUM
           END-EVALUATE
      *
           MOVE    NSRY-RENNUM          TO      RECE85-RENNUM
           MOVE    RECE82-PTNUM         TO      RECE85-PTNUM
      *
           MOVE    NSRY-SRYKA           TO      RECE85-SRYKA
           MOVE    NSRY-SRYKBN          TO      RECE85-SRYKBN
           MOVE    NSRY-SRYSYUKBN       TO      RECE85-SRYSYUKBN
           IF      NACCT-ZAISKBKBN      =       "7"
      ****    IF    RECE82-HKNKBN       =       "4"
      ****       MOVE     "40"          TO      RECE85-SRYKBN
      ****    ELSE
                 MOVE     "80"          TO      RECE85-SRYKBN
      ****    END-IF
              MOVE       "800"          TO      RECE85-SRYSYUKBN
           END-IF
           MOVE    NSRY-JIHIMONEYTOTAL  TO      RECE85-JIHIMONEY
      *
           MOVE    ZERO                 TO      IDX4
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                       UNTIL    (IDX2      >    5)    OR
                                (NSRY-SRYCD(IDX2)  =   SPACE)
             IF      NSRY-SRYCD(IDX2)(1:1)    =   "0"  OR  "1" OR  
                                                  "8"
               ADD     1                   TO  IDX4    
               MOVE    NSRY-SRYCD(IDX2)    TO  RECE85-SRYCD(IDX4)
               IF      NACCT-ZAISKBKBN      =       "7"
                  MOVE     "199000510"     TO  RECE85-SRYCD(IDX4)
               END-IF
               MOVE    NSRY-SRYSURYO(IDX2) TO  RECE85-SRYSURYO(IDX4)
               MOVE    NSRY-SRYKAISU(IDX2) TO  RECE85-SRYKAISU(IDX4)
               MOVE    SPACE               TO  RECE85-MEISKYFLG(IDX4)
               MOVE    NSRY-AUTOKBN(IDX2)  TO  RECE85-AUTOKBN(IDX4)
               MOVE    NSRY-INPUTNUM(IDX2) TO  RECE85-INPUTNUM(IDX4)
               MOVE    NSRY-SRYCD(IDX2)    TO  WRK-SRYCD
               IF      NACCT-ZAISKBKBN      =       "7"
                  MOVE     199000510       TO  WRK-SRYCD
               END-IF
               MOVE    RECE85-SRYYM        TO  WK-TAIHI-SRYYM
               MOVE    WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
               PERFORM    900-TENSU-TBL-SEC
               MOVE    TNS-TANICD          TO  RECE85-TANICD(IDX4)
               MOVE    TNS-TANIMOJISU      TO  RECE85-TANIMOJISU(IDX4)
               MOVE    TNS-TANINAME        TO  RECE85-TANINAME(IDX4)
               MOVE    TNS-TENSIKIBETU     TO  RECE85-TENSIKIBETU(IDX4)
               MOVE    TNS-TEN             TO  RECE85-TEN(IDX4)
               IF    RECE82-HKNKBN    =  "4"
               AND   RECE82-JIBAIKBN  =  "1"
               AND   NACCT-ZAISKBKBN  =  "7"
                   MOVE  1                 TO  RECE85-TENSIKIBETU(IDX4)  
                   MOVE  120               TO  RECE85-TEN(IDX4)
               END-IF
               MOVE    TNS-NYUTENTTLKBN   TO    RECE85-TENTTLKBN(IDX4)
               MOVE    TNS-YUKOKETA       TO    RECE85-YUKOKETA(IDX4)
               MOVE    TNS-DATAKBN        TO    RECE85-DATAKBN (IDX4)
               MOVE    TNS-HOUKNSKBN      TO    RECE85-HOUKNSKBN(IDX4)
               MOVE    TNS-KNSJISKBN      TO    RECE85-KNSJISKBN(IDX4)
               MOVE    TNS-CDKBN-KBN      TO    RECE85-CDKBN-KBN(IDX4)
               MOVE    TNS-CDKBN-SYO      TO    RECE85-CDKBN-SYO(IDX4)
               MOVE    TNS-CDKBN-BU       TO    RECE85-CDKBN-BU(IDX4)
               MOVE    TNS-CDKBN-KBNNUM   
                                      TO RECE85-CDKBN-KBNNUM(IDX4)
               MOVE    TNS-CDKBN-KBNNUM-EDA
                                      TO RECE85-CDKBN-KBNNUM-EDA(IDX4)
               MOVE    TNS-CDKBN-KOUBAN
                                      TO RECE85-CDKBN-KOUBAN(IDX4)
               MOVE    TNS-KOUHYOJYUNNUM  
                                      TO RECE85-KOUHYOJYUNNUM(IDX4)
               IF    ((NSRY-SRYCD    (IDX2)(1:1)     =   "8"  )  OR
                      (NSRY-SRYCD    (IDX2)(1:3)     =   "008"))
               AND   ( NSRY-INPUTNUM (IDX2)      NOT =   ZERO  )
                   MOVE    NSRY-HOSPNUM          TO  PTCOM-HOSPNUM
                   MOVE    NSRY-PTID             TO  PTCOM-PTID
                   MOVE    NSRY-ZAINUM           TO  PTCOM-ZAINUM
                   MOVE    NSRY-SRYCD (IDX2)     TO  PTCOM-SRYCD
                   MOVE    NSRY-INPUTNUM(IDX2)   TO  PTCOM-RENNUM
                   PERFORM 900-PTCOM-READ-SEC
                   MOVE    PTCOM-INPUTCOMENT     TO  RECE85-NAME(IDX4)
               ELSE
                   MOVE    TNS-NAME              TO  RECE85-NAME(IDX4)
               END-IF
               IF    RECE82-HKNKBN    =  "4"
               AND   RECE82-JIBAIKBN  =  "1"
               AND   NACCT-ZAISKBKBN  =  "7"
      *
                 MOVE     1                 TO    WRK-MEISAI-CNT
                 MOVE     TNS-TEN           TO    WRK-TEN
                 MOVE     1                 TO    WRK-SURYOUTOTAL
                 MOVE     199000510         TO    WRK-SRYCDTOTAL
      *
                 MOVE     "2"               TO    RECE85-SIKFLG(IDX4)
               ELSE
                 MOVE     "1"               TO    RECE85-SIKFLG(IDX4)
               END-IF
      *
               MOVE    NSRY-SRYCD(IDX2)  TO  WRK-SRYCD
               IF      NACCT-ZAISKBKBN      =       "7"
                  MOVE     199000510     TO  WRK-SRYCD
               END-IF
               MOVE     ZERO    TO    WRK-FUKA-KINGAKU
               PERFORM  VARYING  FUKA-IDX  FROM  1  BY  1
                         UNTIL   FUKA-IDX   >    9
                 MOVE   WRK-SRYCD(FUKA-IDX:1)
                                         TO  WRK-FUKA-VALUE
                 IF  WRK-FUKA-VALUE  =  ZERO
                   CONTINUE
                 ELSE
                   COMPUTE  RECE84-RUIKEITOTAL  =
                            RECE84-RUIKEITOTAL  +
                          ( WRK-FUKA-VALUE    * WRK-SRYCD)
                   COMPUTE  WRK-FUKA-KINGAKU    =
                            WRK-FUKA-KINGAKU    +
                          ((WRK-FUKA-VALUE^4) * FUKA-IDX
                                              * NSRY-SRYSURYO(IDX2))
                 END-IF
               END-PERFORM
               COMPUTE  RECE84-RUIKEITOTAL  =
                        RECE84-RUIKEITOTAL  +
                        WRK-FUKA-KINGAKU
      *
             END-IF  
           END-PERFORM
      *
           .
       23031-RECE85-TENSOU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡ÊÎÅÍÜÃ´Åö¼êÅö¡Ë
      *****************************************************************
       23032-RECE82-HENSYU2-SEC                SECTION.
      *
      *    ¤½¤ÎÂ¾¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
           EVALUATE   RECE82-HKNKBN
             WHEN      "1"
             WHEN      "2"
                COMPUTE    RECE82-TENSU(21)  =
                           RECE82-TENSU(21)  +
                          (NACCT-ZAITEN   *   CNT-DAY21)
             WHEN      "4"
                EVALUATE  WRK-PARA-RECEKBN
                  WHEN      "4"
      **************¡Ê¿·ÍÍ¼°¡Ë
                     COMPUTE    RECE82-KINGAKU3(3)  =
                                RECE82-KINGAKU3(3)  +
                               (120            *   CNT-DAY21)
                  WHEN      "5"
      **************¡Ê½¾ÍèÍÍ¼°¡Ë
                   EVALUATE  RECE82-JIBAIKBN
                    WHEN   "1"
                     COMPUTE    RECE82-SONOTAKIN-JYU  =
                                RECE82-SONOTAKIN-JYU  +
                               (120            *   CNT-DAY21)
                    WHEN   "2"
                     COMPUTE   RECE82-TENSU(28)  =
                               RECE82-TENSU(28)  +
                               (NACCT-ZAITEN   *   CNT-DAY21)
                     COMPUTE   TBL-YKZTEN-JI(28)  =
                               TBL-YKZTEN-JI(28)  +
                               (NACCT-ZAITEN   *   CNT-DAY21)
                   END-EVALUATE
                END-EVALUATE
             WHEN      "5"
                COMPUTE    RECE82-TENSU(37)  =
                           RECE82-TENSU(37)  +
                          (NACCT-ZAITEN   *   CNT-DAY21)
           END-EVALUATE
      *
           .
       23032-RECE82-HENSYU2-EXT.
           EXIT.
      *
      **************************************************************
      *    ¿©»öÎÅÍÜÈñ¤ÎÊÔ½¸
      **************************************************************
       2304-NYUINACCT-HEN-SEC           SECTION.
      *
           PERFORM    VARYING  SYUKEI-IDX     FROM    1   BY  1
                  UNTIL  SYUKEI-IDX            >  31
      *
             IF    WRK-SKJ-ZAITEN(SYUKEI-IDX)  NOT =  ZERO
      *
               MOVE     ZERO     TO    FLG-ARI
               PERFORM  VARYING  SYUKEI-IDX2    FROM    1   BY  1
                      UNTIL  SYUKEI-IDX2           > 31
                       OR    WRK-SKJ2-ZAITEN(SYUKEI-IDX2) = ZERO
                       OR    FLG-ARI               = 1
                  IF  ( WRK-SKJ2-ZAITEN   (SYUKEI-IDX2)
                                     =  WRK-SKJ-ZAITEN   (SYUKEI-IDX) )
                  AND ( WRK-SKJ2-SRYCD-T  (SYUKEI-IDX2)
                                     =  WRK-SKJ-SRYCD-T  (SYUKEI-IDX) )
                     ADD   WRK-SKJ-ZAIKAISU(SYUKEI-IDX)  TO  
                                       WRK-SKJ2-ZAIKAISU (SYUKEI-IDX2)
                     MOVE  1  TO  WRK-SKJ2-KAISU(SYUKEI-IDX2 SYUKEI-IDX)
                     MOVE  1  TO   FLG-ARI
                  END-IF
               END-PERFORM  
               IF  FLG-ARI  =  ZERO  
                     MOVE  WRK-SKJ-ZAITEN   (SYUKEI-IDX)  TO  
                                       WRK-SKJ2-ZAITEN   (SYUKEI-IDX2)
                     ADD   WRK-SKJ-ZAIKAISU (SYUKEI-IDX)  TO  
                                       WRK-SKJ2-ZAIKAISU (SYUKEI-IDX2)
                     MOVE  WRK-SKJ-SRYCD-T  (SYUKEI-IDX)  TO  
                                       WRK-SKJ2-SRYCD-T  (SYUKEI-IDX2)
                     PERFORM  VARYING  SYUKEI-IDX3  FROM  1  BY  1
                        UNTIL ( SYUKEI-IDX3     >   15 )
                         OR   ( WRK-SKJ-SRYCD(SYUKEI-IDX SYUKEI-IDX3) 
                                                               =  ZERO)
      *
                        ADD   1  TO  WRK-SKJ2-MEISAISU(SYUKEI-IDX2)
                        ADD   WRK-SKJ-SRYCD(SYUKEI-IDX SYUKEI-IDX3)
                                 TO  WRK-SKJ2-SRYCDTOTAL(SYUKEI-IDX2)
      *
                     END-PERFORM
                     MOVE  1  TO  WRK-SKJ2-KAISU(SYUKEI-IDX2 SYUKEI-IDX)
               END-IF
      *
             END-IF
      *
           END-PERFORM
      *
           .
      *
       2304-NYUINACCT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       2305-RECE84-TENSOU2-SKJ-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE84-REC
           INITIALIZE                           RECE84-REC
           MOVE    RECE82-HOSPNUM       TO      RECE84-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE84-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE84-PTID
           MOVE    RECE82-RECEKA        TO      RECE84-RECEKA
           MOVE    RECE82-HKNID         TO      RECE84-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE84-TEKSTYMD
           COMPUTE WRK-ZAINUM    =  99999920  +  SYUKEI-IDX3
           MOVE    WRK-ZAINUM           TO      RECE84-ZAINUM
           MOVE    NACCT-SRYKBN         TO      RECE84-SRYKBN
           MOVE    NACCT-ZAISKBKBN      TO      RECE84-ZAISKBKBN
           MOVE    RECE82-PTNUM         TO      RECE84-PTNUM
      *
           MOVE    NACCT-SRYKA          TO      RECE84-SRYKA
           MOVE    RECE82-HKNCOMBI      TO      RECE84-HKNCOMBI
           MOVE    SPACE                TO      RECE84-JIHOKBN
           MOVE    WRK-SKJ2-ZAITEN(SYUKEI-IDX3) 
                                        TO      RECE84-ZAITEN
           MOVE    WRK-SKJ2-SRYCDTOTAL(SYUKEI-IDX3)
                                        TO      RECE84-SRYCDTOTAL
           MOVE    WRK-SKJ2-MEISAISU(SYUKEI-IDX3)
                                        TO      RECE84-SURYOUTOTAL
           MOVE    WRK-SKJ2-MEISAISU(SYUKEI-IDX3)
                                        TO      RECE84-MEISAISU
           MOVE    WRK-SKJ2-ZAIKAISU(SYUKEI-IDX3)
                                        TO      RECE84-ZAIKAISU
           MOVE    WRK-SKJ2-ZAITEN(SYUKEI-IDX3)
                                        TO      RECE84-SYUTEN1
           MOVE    ZERO                 TO      RECE84-RUIKEITOTAL
           MOVE    ZERO                 TO      RECE84-SYUTEN2
           MOVE    ZERO                 TO      RECE84-YKZTEN
           MOVE    ZERO                 TO      RECE84-KIZAITEN
           MOVE    NACCT-DRCD           TO      RECE84-DRCD
           MOVE    SPACE                TO      RECE84-ZAIREQFLG
           MOVE    ZERO                 TO      RECE84-TEIGENRATE
           MOVE        ZERO             TO      WK-TAIHI-DAY
           MOVE        ZERO             TO      WK-TAIHI-FLG
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >       31
                   MOVE    WRK-SKJ2-KAISU(SYUKEI-IDX3 IDX2)             
                                        TO   RECE84-KAISU (1 IDX2)
                   IF     (WK-TAIHI-FLG           =   ZERO)
                   AND    (WRK-SKJ2-KAISU(SYUKEI-IDX3 IDX2)             
                                                     NOT =   ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF    
           END-PERFORM
           .
       2305-RECE84-TENSOU2-SKJ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       2305-RECE84-TENSOU2-SKJ200604-SEC                SECTION.
      *
           ADD      1   TO   WRK-SKJ200604-MEISAISU(SYUKEI-IDX3)
           IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 2)  NOT =  ZERO
              ADD   1   TO   WRK-SKJ200604-MEISAISU(SYUKEI-IDX3)
           END-IF
           IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 3)  NOT =  ZERO
              ADD   1   TO   WRK-SKJ200604-MEISAISU(SYUKEI-IDX3)
           END-IF
           IF  ((RECE82-HKNKBN      =   "4") AND
                (RECE82-JIBAIKBN    =   "2"))
           OR  ((RECE82-HKNKBN      =   "5") AND
                (RECE82-KOMUKBN     =   "2"))
             CONTINUE
           ELSE
             IF      RECE82-HKNKBN    =   "4"
             AND     RECE82-SKJKBN    =   "2"
                COMPUTE  WRK-SKJ200604-MEISAISU(SYUKEI-IDX3) =
                         WRK-SKJ200604-MEISAISU(SYUKEI-IDX3) * 3
             ELSE
                COMPUTE  WRK-SKJ200604-MEISAISU(SYUKEI-IDX3) =
                         WRK-SKJ200604-MEISAISU(SYUKEI-IDX3) * 2
             END-IF
           END-IF
      ***
           ADD  WRK-SKJ200604-DAY(SYUKEI-IDX3 1)
                     TO   WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
           ADD  WRK-SKJ200604-DAY(SYUKEI-IDX3 2)
                     TO   WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
           ADD  WRK-SKJ200604-DAY(SYUKEI-IDX3 3)
                     TO   WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
      ***
           MOVE       ZERO     TO     IDX12
           IF   WRK-NYULIFE-KBN(SYUKEI-IDX3)  NOT =  SPACE
            EVALUATE   WRK-SKJ200604-KBN(SYUKEI-IDX3)
      *
            WHEN      1
      *
             ADD  197001310 TO WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
             ADD  1         TO IDX12
             MOVE 197001310 TO WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
             MOVE WRK-SKJ200604-DAY(SYUKEI-IDX3 1)
                            TO WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
             IF  ((RECE82-HKNKBN      =   "4") AND
                  (RECE82-JIBAIKBN    =   "2"))
             OR  ((RECE82-HKNKBN      =   "5") AND
                  (RECE82-KOMUKBN     =   "2"))
              CONTINUE
             ELSE
              IF      RECE82-HKNKBN    =   "4"
              AND     RECE82-SKJKBN    =   "2"
                ADD   101970010
                           TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                ADD   1    TO  IDX12
                MOVE  101970010
                           TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                ADD   099609901
                           TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                ADD   1    TO  IDX12
                MOVE  099609901
                           TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
              ELSE
                ADD   101970010
                           TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                ADD   1    TO  IDX12
                MOVE  101970010
                           TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
              END-IF
             END-IF
      *
             IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 2)  NOT =  ZERO
               ADD  197001570 TO WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
               ADD  1         TO IDX12
               MOVE 197001570 TO WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
               MOVE WRK-SKJ200604-DAY(SYUKEI-IDX3 2)
                            TO WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
               IF  ((RECE82-HKNKBN      =   "4") AND
                    (RECE82-JIBAIKBN    =   "2"))
               OR  ((RECE82-HKNKBN      =   "5") AND
                    (RECE82-KOMUKBN     =   "2"))
                CONTINUE
               ELSE
                IF    RECE82-HKNKBN    =   "4"
                AND   RECE82-SKJKBN    =   "2"
                   ADD   101970010
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  101970010
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                   ADD   099609901
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  099609901
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                ELSE
                   ADD   101970010
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  101970010
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                END-IF
               END-IF
             END-IF
      *
             IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 3)  NOT =  ZERO
               ADD  197001670 TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
               ADD  1         TO  IDX12
               MOVE 197001670 TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
               MOVE WRK-SKJ200604-DAY(SYUKEI-IDX3 3)
                            TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
               IF  ((RECE82-HKNKBN      =   "4") AND
                    (RECE82-JIBAIKBN    =   "2"))
               OR  ((RECE82-HKNKBN      =   "5") AND
                    (RECE82-KOMUKBN     =   "2"))
                CONTINUE
               ELSE
                IF    RECE82-HKNKBN    =   "4"
                AND   RECE82-SKJKBN    =   "2"
                   ADD   101970010
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  101970010
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                   ADD   099609901
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  099609901
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                ELSE
                   ADD   101970010
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  101970010
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                END-IF
               END-IF
             END-IF
      *
            WHEN      2
      *
             ADD  197001710 TO WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
             ADD  1         TO IDX12
             MOVE 197001710 TO WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
             MOVE WRK-SKJ200604-DAY(SYUKEI-IDX3 1)
                            TO WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
             IF  ((RECE82-HKNKBN      =   "4") AND
                  (RECE82-JIBAIKBN    =   "2"))
             OR  ((RECE82-HKNKBN      =   "5") AND
                  (RECE82-KOMUKBN     =   "2"))
              CONTINUE
             ELSE
              IF      RECE82-HKNKBN    =   "4"
              AND     RECE82-SKJKBN    =   "2"
                ADD   101970010
                           TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                ADD   1    TO  IDX12
                MOVE  101970010
                           TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                ADD   099609901
                           TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                ADD   1    TO  IDX12
                MOVE  099609901
                           TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
              ELSE
                ADD   101970010
                           TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                ADD   1    TO  IDX12
                MOVE  101970010
                           TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
              END-IF
             END-IF
      *
            END-EVALUATE
           ELSE
            EVALUATE   WRK-SKJ200604-KBN(SYUKEI-IDX3)
      *
            WHEN      1
      *
             ADD  197000110 TO WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
             ADD  1         TO IDX12
             MOVE 197000110 TO WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
             MOVE WRK-SKJ200604-DAY(SYUKEI-IDX3 1)
                            TO WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
             IF  ((RECE82-HKNKBN      =   "4") AND
                  (RECE82-JIBAIKBN    =   "2"))
             OR  ((RECE82-HKNKBN      =   "5") AND
                  (RECE82-KOMUKBN     =   "2"))
              CONTINUE
             ELSE
              IF      RECE82-HKNKBN    =   "4"
              AND     RECE82-SKJKBN    =   "2"
                ADD   101970010
                           TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                ADD   1    TO  IDX12
                MOVE  101970010
                           TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                ADD   099609901
                           TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                ADD   1    TO  IDX12
                MOVE  099609901
                           TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
              ELSE
                ADD   101970010
                           TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                ADD   1    TO  IDX12
                MOVE  101970010
                           TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
              END-IF
             END-IF
      *
             IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 2)  NOT =  ZERO
               ADD  197000470 TO WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
               ADD  1         TO IDX12
               MOVE 197000470 TO WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
               MOVE WRK-SKJ200604-DAY(SYUKEI-IDX3 2)
                            TO WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
               IF  ((RECE82-HKNKBN      =   "4") AND
                    (RECE82-JIBAIKBN    =   "2"))
               OR  ((RECE82-HKNKBN      =   "5") AND
                    (RECE82-KOMUKBN     =   "2"))
                CONTINUE
               ELSE
                IF    RECE82-HKNKBN    =   "4"
                AND   RECE82-SKJKBN    =   "2"
                   ADD   101970010
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  101970010
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                   ADD   099609901
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  099609901
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                ELSE
                   ADD   101970010
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  101970010
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                END-IF
               END-IF
             END-IF
      *
             IF   WRK-SKJ200604-DAY(SYUKEI-IDX3 3)  NOT =  ZERO
               ADD  197000570 TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
               ADD  1         TO  IDX12
               MOVE 197000570 TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
               MOVE WRK-SKJ200604-DAY(SYUKEI-IDX3 3)
                            TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
               IF  ((RECE82-HKNKBN      =   "4") AND
                    (RECE82-JIBAIKBN    =   "2"))
               OR  ((RECE82-HKNKBN      =   "5") AND
                    (RECE82-KOMUKBN     =   "2"))
                CONTINUE
               ELSE
                IF    RECE82-HKNKBN    =   "4"
                AND   RECE82-SKJKBN    =   "2"
                   ADD   101970010
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  101970010
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                   ADD   099609901
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  099609901
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                ELSE
                   ADD   101970010
                            TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                   ADD   1  TO  IDX12
                   MOVE  101970010
                            TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                   MOVE  1  TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                   ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                END-IF
               END-IF
             END-IF
      *
            WHEN      2
      *
             ADD  197000710 TO WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
             ADD  1         TO IDX12
             MOVE 197000710 TO WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
             MOVE WRK-SKJ200604-DAY(SYUKEI-IDX3 1)
                            TO WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
             IF  ((RECE82-HKNKBN      =   "4") AND
                  (RECE82-JIBAIKBN    =   "2"))
             OR  ((RECE82-HKNKBN      =   "5") AND
                  (RECE82-KOMUKBN     =   "2"))
              CONTINUE
             ELSE
              IF      RECE82-HKNKBN    =   "4"
              AND     RECE82-SKJKBN    =   "2"
                ADD   101970010
                           TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                ADD   1    TO  IDX12
                MOVE  101970010
                           TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                ADD   099609901
                           TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                ADD   1    TO  IDX12
                MOVE  099609901
                           TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
              ELSE
                ADD   101970010
                           TO  WRK-SKJ200604-SRYCDTOTAL(SYUKEI-IDX3)
                ADD   1    TO  IDX12
                MOVE  101970010
                           TO  WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX12)
                MOVE  1    TO  WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX12)
                ADD   1    TO  WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
              END-IF
             END-IF
      *
            END-EVALUATE
           END-IF
      ***
           MOVE    SPACE                TO  RECE84-REC
           INITIALIZE                       RECE84-REC
           MOVE    RECE82-HOSPNUM       TO  RECE84-HOSPNUM
           MOVE    RECE82-SRYYM         TO  RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO  RECE84-NYUGAIKBN
           MOVE    RECE82-PTID          TO  RECE84-PTID
           MOVE    RECE82-RECEKA        TO  RECE84-RECEKA
           MOVE    RECE82-HKNID         TO  RECE84-HKNID
           MOVE    RECE82-TEKSTYMD      TO  RECE84-TEKSTYMD
           IF   WRK-NYULIFE-KBN(SYUKEI-IDX3)  NOT =  SPACE
      ***       COMPUTE WRK-ZAINUM  =  99999731  +  SYUKEI-IDX3
                COMPUTE WRK-ZAINUM  =  99996100  +  SYUKEI-IDX3
                MOVE    WRK-ZAINUM      TO  RECE84-ZAINUM
           ELSE
      ***       COMPUTE WRK-ZAINUM  =  99999700  +  SYUKEI-IDX3
                COMPUTE WRK-ZAINUM  =  99996000  +  SYUKEI-IDX3
                MOVE    WRK-ZAINUM      TO  RECE84-ZAINUM
           END-IF
      ***  COMPUTE WRK-ZAINUM    =  99999920  +  SYUKEI-IDX3
      ***  MOVE    WRK-ZAINUM           TO  RECE84-ZAINUM
           MOVE    "97"                 TO  RECE84-SRYKBN
           MOVE    "4"                  TO  RECE84-ZAISKBKBN
           MOVE    RECE82-PTNUM         TO  RECE84-PTNUM
      *
           MOVE    SPACE                TO  RECE84-SRYKA
           MOVE    RECE82-HKNCOMBI      TO  RECE84-HKNCOMBI
           MOVE    SPACE                TO  RECE84-JIHOKBN
      *
           MOVE    WRK-SKJ200604-SRYCDTOTAL (SYUKEI-IDX3)
                                        TO  RECE84-SRYCDTOTAL
           MOVE    WRK-SKJ200604-SURYOUTOTAL(SYUKEI-IDX3)
                                        TO  RECE84-SURYOUTOTAL
           MOVE    WRK-SKJ200604-MEISAISU   (SYUKEI-IDX3)
                                        TO  RECE84-MEISAISU
           MOVE    1                    TO  RECE84-ZAIKAISU
      *
           MOVE    ZERO                 TO  RECE84-RUIKEITOTAL
           MOVE    ZERO                 TO  RECE84-SYUTEN2
           MOVE    ZERO                 TO  RECE84-YKZTEN
           MOVE    ZERO                 TO  RECE84-KIZAITEN
           MOVE    SPACE                TO  RECE84-DRCD
           MOVE    SPACE                TO  RECE84-ZAIREQFLG
           MOVE    ZERO                 TO  RECE84-TEIGENRATE
           MOVE    1                    TO  RECE84-KAISU(1 SYUKEI-IDX3)
           MOVE    SYUKEI-IDX3          TO  WK-TAIHI-DAY
      *
           .
       2305-RECE84-TENSOU2-SKJ200604-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê´Ä¶­¡Ë¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       2308-RECE84-TENSOU2-SKJ200610-KAN-SEC                SECTION.
      *
           ADD     1    TO   WRK-SKJ200604-MEISAISU-KAN(SYUKEI-IDX3)
           IF  ((RECE82-HKNKBN      =   "4") AND
                (RECE82-JIBAIKBN    =   "2"))
           OR  ((RECE82-HKNKBN      =   "5") AND
                (RECE82-KOMUKBN     =   "2"))
             CONTINUE
           ELSE
             IF      RECE82-HKNKBN    =   "4"
             AND     RECE82-SKJKBN    =   "2"
                COMPUTE  WRK-SKJ200604-MEISAISU-KAN(SYUKEI-IDX3) =
                         WRK-SKJ200604-MEISAISU-KAN(SYUKEI-IDX3) * 3
             ELSE
                COMPUTE  WRK-SKJ200604-MEISAISU-KAN(SYUKEI-IDX3) =
                         WRK-SKJ200604-MEISAISU-KAN(SYUKEI-IDX3) * 2
             END-IF
           END-IF
      ***
           ADD     1    TO   WRK-SKJ200604-SURYOUTOTAL-KAN(SYUKEI-IDX3)
      ***
           MOVE       ZERO     TO     IDX12
           EVALUATE   WRK-SKJ200604-KBN(SYUKEI-IDX3)
            WHEN      1
             ADD  197001410
                        TO WRK-SKJ200604-SRYCDTOTAL-KAN(SYUKEI-IDX3)
             ADD  1     TO IDX12
             MOVE 197001410
                        TO WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX12)
             MOVE 1     TO WRK-SKJ200604-SURYOU-KAN(SYUKEI-IDX3 IDX12)
             IF  ((RECE82-HKNKBN      =   "4") AND
                  (RECE82-JIBAIKBN    =   "2"))
             OR  ((RECE82-HKNKBN      =   "5") AND
                  (RECE82-KOMUKBN     =   "2"))
              CONTINUE
             ELSE
              IF      RECE82-HKNKBN    =   "4"
              AND     RECE82-SKJKBN    =   "2"
                ADD   101970010
                         TO  WRK-SKJ200604-SRYCDTOTAL-KAN(SYUKEI-IDX3)
                ADD   1  TO  IDX12
                MOVE  101970010
                         TO  WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX12)
                MOVE  1  TO  WRK-SKJ200604-SURYOU-KAN(SYUKEI-IDX3 IDX12)
                ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL-KAN(SYUKEI-IDX3)
                ADD   099609901
                         TO  WRK-SKJ200604-SRYCDTOTAL-KAN(SYUKEI-IDX3)
                ADD   1  TO  IDX12
                MOVE  099609901
                         TO  WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX12)
                MOVE  1  TO  WRK-SKJ200604-SURYOU-KAN(SYUKEI-IDX3 IDX12)
                ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL-KAN(SYUKEI-IDX3)
              ELSE
                ADD   101970010
                         TO  WRK-SKJ200604-SRYCDTOTAL-KAN(SYUKEI-IDX3)
                ADD   1  TO  IDX12
                MOVE  101970010
                         TO  WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX12)
                MOVE  1  TO  WRK-SKJ200604-SURYOU-KAN(SYUKEI-IDX3 IDX12)
                ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL-KAN(SYUKEI-IDX3)
              END-IF
             END-IF
            WHEN      2
             ADD  197001810
                        TO WRK-SKJ200604-SRYCDTOTAL-KAN(SYUKEI-IDX3)
             ADD  1     TO IDX12
             MOVE 197001810
                        TO WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX12)
             MOVE 1     TO WRK-SKJ200604-SURYOU-KAN(SYUKEI-IDX3 IDX12)
             IF  ((RECE82-HKNKBN      =   "4") AND
                  (RECE82-JIBAIKBN    =   "2"))
             OR  ((RECE82-HKNKBN      =   "5") AND
                  (RECE82-KOMUKBN     =   "2"))
              CONTINUE
             ELSE
              IF      RECE82-HKNKBN    =   "4"
              AND     RECE82-SKJKBN    =   "2"
                ADD   101970010
                         TO  WRK-SKJ200604-SRYCDTOTAL-KAN(SYUKEI-IDX3)
                ADD   1  TO  IDX12
                MOVE  101970010
                         TO  WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX12)
                MOVE  1  TO  WRK-SKJ200604-SURYOU-KAN(SYUKEI-IDX3 IDX12)
                ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL-KAN(SYUKEI-IDX3)
                ADD   099609901
                         TO  WRK-SKJ200604-SRYCDTOTAL-KAN(SYUKEI-IDX3)
                ADD   1  TO  IDX12
                MOVE  099609901
                         TO  WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX12)
                MOVE  1  TO  WRK-SKJ200604-SURYOU-KAN(SYUKEI-IDX3 IDX12)
                ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL-KAN(SYUKEI-IDX3)
              ELSE
                ADD   101970010
                         TO  WRK-SKJ200604-SRYCDTOTAL-KAN(SYUKEI-IDX3)
                ADD   1  TO  IDX12
                MOVE  101970010
                         TO  WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX12)
                MOVE  1  TO  WRK-SKJ200604-SURYOU-KAN(SYUKEI-IDX3 IDX12)
                ADD   1  TO  WRK-SKJ200604-SURYOUTOTAL-KAN(SYUKEI-IDX3)
              END-IF
             END-IF
           END-EVALUATE
      ***
           MOVE    SPACE                TO  RECE84-REC
           INITIALIZE                       RECE84-REC
           MOVE    RECE82-HOSPNUM       TO  RECE84-HOSPNUM
           MOVE    RECE82-SRYYM         TO  RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO  RECE84-NYUGAIKBN
           MOVE    RECE82-PTID          TO  RECE84-PTID
           MOVE    RECE82-RECEKA        TO  RECE84-RECEKA
           MOVE    RECE82-HKNID         TO  RECE84-HKNID
           MOVE    RECE82-TEKSTYMD      TO  RECE84-TEKSTYMD
      ***  COMPUTE WRK-ZAINUM  =  99999762  +  SYUKEI-IDX3
           COMPUTE WRK-ZAINUM  =  99996300  +  SYUKEI-IDX3
           MOVE    WRK-ZAINUM           TO  RECE84-ZAINUM
           MOVE    "97"                 TO  RECE84-SRYKBN
           MOVE    "4"                  TO  RECE84-ZAISKBKBN
           MOVE    RECE82-PTNUM         TO  RECE84-PTNUM
      *
           MOVE    SPACE                TO  RECE84-SRYKA
           MOVE    RECE82-HKNCOMBI      TO  RECE84-HKNCOMBI
           MOVE    SPACE                TO  RECE84-JIHOKBN
      *
           MOVE    WRK-SKJ200604-SRYCDTOTAL-KAN (SYUKEI-IDX3)
                                        TO  RECE84-SRYCDTOTAL
           MOVE    WRK-SKJ200604-SURYOUTOTAL-KAN(SYUKEI-IDX3)
                                        TO  RECE84-SURYOUTOTAL
           MOVE    WRK-SKJ200604-MEISAISU-KAN   (SYUKEI-IDX3)
                                        TO  RECE84-MEISAISU
           MOVE    1                    TO  RECE84-ZAIKAISU
      *
           MOVE    ZERO                 TO  RECE84-RUIKEITOTAL
           MOVE    ZERO                 TO  RECE84-SYUTEN2
           MOVE    ZERO                 TO  RECE84-YKZTEN
           MOVE    ZERO                 TO  RECE84-KIZAITEN
           MOVE    SPACE                TO  RECE84-DRCD
           MOVE    SPACE                TO  RECE84-ZAIREQFLG
           MOVE    ZERO                 TO  RECE84-TEIGENRATE
           MOVE    1                    TO  RECE84-KAISU(1 SYUKEI-IDX3)
           MOVE    SYUKEI-IDX3          TO  WK-TAIHI-DAY
      *
           .
       2308-RECE84-TENSOU2-SKJ200610-KAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       2306-RECE85-HENSYU2-SKJ-SEC               SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM      TO   NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN    TO   NSRY-NYUGAIKBN
           MOVE    NACCT-PTID         TO   NSRY-PTID
           MOVE    NACCT-SRYYM        TO   NSRY-SRYYM
           MOVE    NACCT-ZAINUM       TO   NSRY-ZAINUM
           MOVE    NYUINACT-REC       TO   MCPDATA-REC
           MOVE    "tbl_nyuinact"     TO   MCP-TABLE
           MOVE    "key9"             TO   MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_nyuinact"     TO   MCP-TABLE
               MOVE    "key9"             TO   MCP-PATHNAME
               PERFORM  900-NYUINACT-READ-SEC
           ELSE
               MOVE     1               TO      FLG-NYUINACT
           END-IF
           IF  FLG-NYUINACT   =   ZERO
               MOVE      1      TO     IDX7
               MOVE      1      TO     IDX8
               MOVE      5      TO     IDX9
               PERFORM   23061-RECE85-TENSOU2-SKJ-SEC
               PERFORM   900-RECE85-WRITE-SEC
               IF   WRK-SKJ2-SRYCD(SYUKEI-IDX3 6)  NOT =   ZERO
                  MOVE      2      TO     IDX7
                  MOVE      6      TO     IDX8
                  MOVE     10      TO     IDX9
                  PERFORM   23061-RECE85-TENSOU2-SKJ-SEC
                  PERFORM   900-RECE85-WRITE-SEC
               END-IF
               IF   WRK-SKJ2-SRYCD(SYUKEI-IDX3 11)  NOT =   ZERO
                  MOVE      3      TO     IDX7
                  MOVE     11      TO     IDX8
                  MOVE     15      TO     IDX9
                  PERFORM   23061-RECE85-TENSOU2-SKJ-SEC
                  PERFORM   900-RECE85-WRITE-SEC
               END-IF
           END-IF
      *
           MOVE    "tbl_nyuinact"     TO   MCP-TABLE
           MOVE    "key9"             TO   MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       2306-RECE85-HENSYU2-SKJ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       23061-RECE85-TENSOU2-SKJ-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE85-REC
           INITIALIZE                           RECE85-REC
           MOVE    RECE82-HOSPNUM       TO      RECE85-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE85-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE85-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE85-PTID
           MOVE    RECE82-RECEKA        TO      RECE85-RECEKA
           MOVE    RECE82-HKNID      TO      RECE85-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE85-TEKSTYMD
           COMPUTE WRK-ZAINUM    =  99999920  +  SYUKEI-IDX3
           MOVE    WRK-ZAINUM           TO      RECE85-ZAINUM
           MOVE    IDX7                 TO      RECE85-RENNUM
           MOVE    RECE82-PTNUM         TO      RECE85-PTNUM
      *
           MOVE    NSRY-SRYKA           TO      RECE85-SRYKA
           MOVE    NSRY-SRYKBN          TO      RECE85-SRYKBN
           MOVE    NSRY-SRYSYUKBN       TO      RECE85-SRYSYUKBN
           MOVE    NSRY-JIHIMONEYTOTAL  TO      RECE85-JIHIMONEY
      *
           MOVE    ZERO                 TO      IDX4
           PERFORM     VARYING   IDX2   FROM    IDX8    BY   1
                  UNTIL  (IDX2      >    IDX9)    
                    OR   (WRK-SKJ2-SRYCD(SYUKEI-IDX3 IDX2)  =   ZERO)
      *
                ADD     1                    TO    IDX4  
                MOVE    WRK-SKJ2-SRYCD(SYUKEI-IDX3 IDX2)
                                             TO  RECE85-SRYCD(IDX4)
                MOVE    1                    TO  RECE85-SRYSURYO(IDX4)
                MOVE    ZERO                 TO  RECE85-SRYKAISU(IDX4)
                MOVE    SPACE                TO  RECE85-MEISKYFLG(IDX4)
                MOVE    SPACE                TO  RECE85-AUTOKBN(IDX4)
                MOVE    ZERO                 TO  RECE85-INPUTNUM(IDX4)
                MOVE    WRK-SKJ2-SRYCD(SYUKEI-IDX3 IDX2)
                                             TO  WRK-SRYCD
                MOVE    RECE85-SRYYM         TO  WK-TAIHI-SRYYM
                MOVE    WK-TAIHI-DAY         TO  WK-TAIHI-SRYD
                PERFORM    900-TENSU-TBL-SEC
                MOVE    TNS-TANICD          TO  RECE85-TANICD(IDX4)
                MOVE    TNS-TANIMOJISU      TO  RECE85-TANIMOJISU(IDX4)
                MOVE    TNS-TANINAME        TO  RECE85-TANINAME(IDX4)
                MOVE    TNS-TENSIKIBETU     TO  RECE85-TENSIKIBETU(IDX4)
                MOVE    TNS-TEN             TO  RECE85-TEN(IDX4)
                MOVE    TNS-NYUTENTTLKBN    TO  RECE85-TENTTLKBN(IDX4)
                MOVE    TNS-YUKOKETA        TO  RECE85-YUKOKETA(IDX4)
                MOVE    TNS-DATAKBN         TO  RECE85-DATAKBN (IDX4)
                MOVE    TNS-HOUKNSKBN       TO  RECE85-HOUKNSKBN(IDX4)
                MOVE    TNS-KNSJISKBN       TO  RECE85-KNSJISKBN(IDX4)
                MOVE    TNS-CDKBN-KBN       TO  RECE85-CDKBN-KBN(IDX4)
                MOVE    TNS-CDKBN-SYO       TO  RECE85-CDKBN-SYO(IDX4)
                MOVE    TNS-CDKBN-BU        TO  RECE85-CDKBN-BU(IDX4)
                MOVE    TNS-CDKBN-KBNNUM  
                                      TO  RECE85-CDKBN-KBNNUM(IDX4)
                MOVE    TNS-CDKBN-KBNNUM-EDA
                                      TO  RECE85-CDKBN-KBNNUM-EDA(IDX4)
                MOVE    TNS-CDKBN-KOUBAN
                                      TO  RECE85-CDKBN-KOUBAN(IDX4)
                MOVE    TNS-KOUHYOJYUNNUM  
                                      TO  RECE85-KOUHYOJYUNNUM(IDX4)
                MOVE    TNS-NAME            TO  RECE85-NAME(IDX4)
                MOVE     "1"                TO  RECE85-SIKFLG(IDX4)
      *
           END-PERFORM
      *
           .
       23061-RECE85-TENSOU2-SKJ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       2306-RECE85-HENSYU2-SKJ200604-SEC               SECTION.
      *
           MOVE         1      TO     IDX7
           MOVE         1      TO     IDX8
           MOVE         5      TO     IDX9
           PERFORM   23061-RECE85-TENSOU2-SKJ200604-SEC
           PERFORM   900-RECE85-WRITE-SEC
      *
           IF   WRK-SKJ200604-MEISAISU(SYUKEI-IDX3)  >  5
              MOVE      2      TO     IDX7
              MOVE      6      TO     IDX8
              MOVE      9      TO     IDX9
              PERFORM   23061-RECE85-TENSOU2-SKJ200604-SEC
              PERFORM   900-RECE85-WRITE-SEC
           END-IF
      *
           .
       2306-RECE85-HENSYU2-SKJ200604-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       23061-RECE85-TENSOU2-SKJ200604-SEC                SECTION.
      *
           MOVE    SPACE                TO  RECE85-REC
           INITIALIZE                       RECE85-REC
           MOVE    RECE82-HOSPNUM       TO  RECE85-HOSPNUM
           MOVE    RECE82-SRYYM         TO  RECE85-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO  RECE85-NYUGAIKBN
           MOVE    RECE82-PTID          TO  RECE85-PTID
           MOVE    RECE82-RECEKA        TO  RECE85-RECEKA
           MOVE    RECE82-HKNID         TO  RECE85-HKNID
           MOVE    RECE82-TEKSTYMD      TO  RECE85-TEKSTYMD
           IF   WRK-NYULIFE-KBN(SYUKEI-IDX3)  NOT =  SPACE
      ***       COMPUTE WRK-ZAINUM  =  99999731  +  SYUKEI-IDX3
                COMPUTE WRK-ZAINUM  =  99996100  +  SYUKEI-IDX3
                MOVE    WRK-ZAINUM      TO  RECE85-ZAINUM
           ELSE
      ***       COMPUTE WRK-ZAINUM  =  99999700  +  SYUKEI-IDX3
                COMPUTE WRK-ZAINUM  =  99996000  +  SYUKEI-IDX3
                MOVE    WRK-ZAINUM      TO  RECE85-ZAINUM
           END-IF
      ***  COMPUTE WRK-ZAINUM    =  99999920  +  SYUKEI-IDX3
      ***  MOVE    WRK-ZAINUM           TO  RECE85-ZAINUM
           MOVE    IDX7                 TO  RECE85-RENNUM
           MOVE    RECE82-PTNUM         TO  RECE85-PTNUM
      *
           MOVE    SPACE                TO  RECE85-SRYKA
           MOVE    "97"                 TO  RECE85-SRYKBN
           MOVE    "970"                TO  RECE85-SRYSYUKBN
           MOVE    ZERO                 TO  RECE85-JIHIMONEY
      *
           MOVE    ZERO                 TO  IDX4
           PERFORM  VARYING  IDX2   FROM   IDX8   BY   1
                     UNTIL  (IDX2     >    IDX9)
                       OR   (WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX2)
                                                            =   ZERO)
      *
             ADD     1                   TO  IDX4
             MOVE    WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX2)
                                         TO  RECE85-SRYCD(IDX4)
             MOVE    WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX2)
                                         TO  RECE85-SRYSURYO(IDX4)
             MOVE    ZERO                TO  RECE85-SRYKAISU(IDX4)
             MOVE    SPACE               TO  RECE85-MEISKYFLG(IDX4)
             MOVE    SPACE               TO  RECE85-AUTOKBN(IDX4)
             MOVE    ZERO                TO  RECE85-INPUTNUM(IDX4)
             MOVE    RECE85-SRYCD(IDX4)  TO  WRK-SRYCD
             MOVE    RECE85-SRYYM        TO  WK-TAIHI-SRYYM
             MOVE    WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
             PERFORM    900-TENSU-TBL-SEC
             MOVE    TNS-TANICD          TO  RECE85-TANICD(IDX4)
             MOVE    TNS-TANIMOJISU      TO  RECE85-TANIMOJISU(IDX4)
             MOVE    TNS-TANINAME        TO  RECE85-TANINAME(IDX4)
             MOVE    TNS-TENSIKIBETU     TO  RECE85-TENSIKIBETU(IDX4)
             MOVE    TNS-TEN             TO  RECE85-TEN(IDX4)
             MOVE    TNS-NYUTENTTLKBN    TO  RECE85-TENTTLKBN(IDX4)
             MOVE    TNS-YUKOKETA        TO  RECE85-YUKOKETA(IDX4)
             MOVE    TNS-DATAKBN         TO  RECE85-DATAKBN (IDX4)
             MOVE    TNS-HOUKNSKBN       TO  RECE85-HOUKNSKBN(IDX4)
             MOVE    TNS-KNSJISKBN       TO  RECE85-KNSJISKBN(IDX4)
             MOVE    TNS-CDKBN-KBN       TO  RECE85-CDKBN-KBN(IDX4)
             MOVE    TNS-CDKBN-SYO       TO  RECE85-CDKBN-SYO(IDX4)
             MOVE    TNS-CDKBN-BU        TO  RECE85-CDKBN-BU(IDX4)
             MOVE    TNS-CDKBN-KBNNUM    TO  RECE85-CDKBN-KBNNUM(IDX4)
             MOVE    TNS-CDKBN-KBNNUM-EDA
                                     TO  RECE85-CDKBN-KBNNUM-EDA(IDX4)
             MOVE    TNS-CDKBN-KOUBAN    TO  RECE85-CDKBN-KOUBAN(IDX4)
             MOVE    TNS-KOUHYOJYUNNUM   TO  RECE85-KOUHYOJYUNNUM(IDX4)
             MOVE    TNS-NAME            TO  RECE85-NAME(IDX4)
             MOVE     "1"                TO  RECE85-SIKFLG(IDX4)
      *
             IF    WRK-SKJ200604-SRYCD(SYUKEI-IDX3 IDX2)  =
                            101970010   OR   099609901
               CONTINUE
             ELSE
              IF  ((RECE82-HKNKBN      =   "4") AND
                   (RECE82-JIBAIKBN    =   "2"))
              OR  ((RECE82-HKNKBN      =   "5") AND
                   (RECE82-KOMUKBN     =   "2"))
                  COMPUTE RECE84-ZAITEN =
                          RECE84-ZAITEN +
                  (TNS-TEN * WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX2))
                  COMPUTE RECE84-SYUTEN1 =
                          RECE84-SYUTEN1 +
                  (TNS-TEN * WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX2))
              ELSE
               IF    RECE82-HKNKBN    =   "4"
               AND   RECE82-SKJKBN    =   "2"
                  COMPUTE  WRK-KEISAN   =   TNS-TEN     *  1.2
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  +   5
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  /  10
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  *  10
      *
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  *  1.2
      *
                  COMPUTE RECE84-ZAITEN =
                          RECE84-ZAITEN +
                  (WRK-KEISAN * WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX2))
                  COMPUTE RECE84-SYUTEN1 =
                          RECE84-SYUTEN1 +
                  (WRK-KEISAN * WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX2))
               ELSE
                  COMPUTE  WRK-KEISAN   =   TNS-TEN     *  1.2
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  +   5
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  /  10
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  *  10
      *
                  COMPUTE RECE84-ZAITEN =
                          RECE84-ZAITEN +
                  (WRK-KEISAN * WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX2))
                  COMPUTE RECE84-SYUTEN1 =
                          RECE84-SYUTEN1 +
                  (WRK-KEISAN * WRK-SKJ200604-SURYOU(SYUKEI-IDX3 IDX2))
               END-IF
              END-IF
             END-IF
      *
           END-PERFORM
      *
           .
       23061-RECE85-TENSOU2-SKJ200604-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê´Ä¶­¡Ë¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       2309-RECE85-HENSYU2-SKJ200610-KAN-SEC               SECTION.
      *
           MOVE         1      TO     IDX7
           MOVE         1      TO     IDX8
           MOVE         3      TO     IDX9
           PERFORM   23091-RECE85-TENSOU2-SKJ200610-KAN-SEC
           PERFORM   900-RECE85-WRITE-SEC
      *
           .
       2309-RECE85-HENSYU2-SKJ200610-KAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê´Ä¶­¡Ë¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       23091-RECE85-TENSOU2-SKJ200610-KAN-SEC                SECTION.
      *
           MOVE    SPACE                TO  RECE85-REC
           INITIALIZE                       RECE85-REC
           MOVE    RECE82-HOSPNUM       TO  RECE85-HOSPNUM
           MOVE    RECE82-SRYYM         TO  RECE85-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO  RECE85-NYUGAIKBN
           MOVE    RECE82-PTID          TO  RECE85-PTID
           MOVE    RECE82-RECEKA        TO  RECE85-RECEKA
           MOVE    RECE82-HKNID         TO  RECE85-HKNID
           MOVE    RECE82-TEKSTYMD      TO  RECE85-TEKSTYMD
      ***  COMPUTE WRK-ZAINUM   =  99999762  +  SYUKEI-IDX3
           COMPUTE WRK-ZAINUM   =  99996300  +  SYUKEI-IDX3
           MOVE    WRK-ZAINUM           TO  RECE85-ZAINUM
           MOVE    IDX7                 TO  RECE85-RENNUM
           MOVE    RECE82-PTNUM         TO  RECE85-PTNUM
      *
           MOVE    SPACE                TO  RECE85-SRYKA
           MOVE    "97"                 TO  RECE85-SRYKBN
           MOVE    "970"                TO  RECE85-SRYSYUKBN
           MOVE    ZERO                 TO  RECE85-JIHIMONEY
      *
           MOVE    ZERO                 TO  IDX4
           PERFORM  VARYING  IDX2   FROM   IDX8   BY   1
                     UNTIL  (IDX2     >    IDX9)
                       OR   (WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX2)
                                                            =   ZERO)
      *
             ADD     1                   TO  IDX4
             MOVE    WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX2)
                                         TO  RECE85-SRYCD(IDX4)
             MOVE    WRK-SKJ200604-SURYOU-KAN(SYUKEI-IDX3 IDX2)
                                         TO  RECE85-SRYSURYO(IDX4)
             MOVE    ZERO                TO  RECE85-SRYKAISU(IDX4)
             MOVE    SPACE               TO  RECE85-MEISKYFLG(IDX4)
             MOVE    SPACE               TO  RECE85-AUTOKBN(IDX4)
             MOVE    ZERO                TO  RECE85-INPUTNUM(IDX4)
             MOVE    RECE85-SRYCD(IDX4)  TO  WRK-SRYCD
             MOVE    RECE85-SRYYM        TO  WK-TAIHI-SRYYM
             MOVE    WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
             PERFORM    900-TENSU-TBL-SEC
             MOVE    TNS-TANICD          TO  RECE85-TANICD(IDX4)
             MOVE    TNS-TANIMOJISU      TO  RECE85-TANIMOJISU(IDX4)
             MOVE    TNS-TANINAME        TO  RECE85-TANINAME(IDX4)
             MOVE    TNS-TENSIKIBETU     TO  RECE85-TENSIKIBETU(IDX4)
             MOVE    TNS-TEN             TO  RECE85-TEN(IDX4)
             MOVE    TNS-NYUTENTTLKBN    TO  RECE85-TENTTLKBN(IDX4)
             MOVE    TNS-YUKOKETA        TO  RECE85-YUKOKETA(IDX4)
             MOVE    TNS-DATAKBN         TO  RECE85-DATAKBN (IDX4)
             MOVE    TNS-HOUKNSKBN       TO  RECE85-HOUKNSKBN(IDX4)
             MOVE    TNS-KNSJISKBN       TO  RECE85-KNSJISKBN(IDX4)
             MOVE    TNS-CDKBN-KBN       TO  RECE85-CDKBN-KBN(IDX4)
             MOVE    TNS-CDKBN-SYO       TO  RECE85-CDKBN-SYO(IDX4)
             MOVE    TNS-CDKBN-BU        TO  RECE85-CDKBN-BU(IDX4)
             MOVE    TNS-CDKBN-KBNNUM    TO  RECE85-CDKBN-KBNNUM(IDX4)
             MOVE    TNS-CDKBN-KBNNUM-EDA
                                     TO  RECE85-CDKBN-KBNNUM-EDA(IDX4)
             MOVE    TNS-CDKBN-KOUBAN    TO  RECE85-CDKBN-KOUBAN(IDX4)
             MOVE    TNS-KOUHYOJYUNNUM   TO  RECE85-KOUHYOJYUNNUM(IDX4)
             MOVE    TNS-NAME            TO  RECE85-NAME(IDX4)
             MOVE     "1"                TO  RECE85-SIKFLG(IDX4)
      *
             IF    WRK-SKJ200604-SRYCD-KAN(SYUKEI-IDX3 IDX2)  =
                            101970010   OR   099609901
               CONTINUE
             ELSE
              IF  ((RECE82-HKNKBN      =   "4") AND
                   (RECE82-JIBAIKBN    =   "2"))
              OR  ((RECE82-HKNKBN      =   "5") AND
                   (RECE82-KOMUKBN     =   "2"))
                  COMPUTE RECE84-ZAITEN =
                          RECE84-ZAITEN +
                  (TNS-TEN * WRK-SKJ200604-SURYOU-KAN
                                               (SYUKEI-IDX3 IDX2))
                  COMPUTE RECE84-SYUTEN1 =
                          RECE84-SYUTEN1 +
                  (TNS-TEN * WRK-SKJ200604-SURYOU-KAN
                                               (SYUKEI-IDX3 IDX2))
              ELSE
               IF    RECE82-HKNKBN    =   "4"
               AND   RECE82-SKJKBN    =   "2"
                  COMPUTE  WRK-KEISAN   =   TNS-TEN     *  1.2
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  +   5
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  /  10
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  *  10
      *
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  *  1.2
      *
                  COMPUTE RECE84-ZAITEN =
                          RECE84-ZAITEN +
                  (WRK-KEISAN * WRK-SKJ200604-SURYOU-KAN
                                               (SYUKEI-IDX3 IDX2))
                  COMPUTE RECE84-SYUTEN1 =
                          RECE84-SYUTEN1 +
                  (WRK-KEISAN * WRK-SKJ200604-SURYOU-KAN
                                               (SYUKEI-IDX3 IDX2))
               ELSE
                  COMPUTE  WRK-KEISAN   =   TNS-TEN     *  1.2
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  +   5
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  /  10
                  COMPUTE  WRK-KEISAN   =   WRK-KEISAN  *  10
      *
                  COMPUTE RECE84-ZAITEN =
                          RECE84-ZAITEN +
                  (WRK-KEISAN * WRK-SKJ200604-SURYOU-KAN
                                               (SYUKEI-IDX3 IDX2))
                  COMPUTE RECE84-SYUTEN1 =
                          RECE84-SYUTEN1 +
                  (WRK-KEISAN * WRK-SKJ200604-SURYOU-KAN
                                               (SYUKEI-IDX3 IDX2))
               END-IF
              END-IF
             END-IF
      *
           END-PERFORM
      *
           .
       23091-RECE85-TENSOU2-SKJ200610-KAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       240-RECE84-HENSYU3-SEC                SECTION.
      *
      *    ÅÀÅ©¤Î·×»»
      *
           INITIALIZE                  ORCSTENTEKAREA
           MOVE    "2"                 TO  ORCSTEN-KBN
           MOVE    RECE82-PTID         TO  ORCSTEN-PTID
           MOVE    RECE82-SRYYM        TO  ORCSTEN-SRYYM
           MOVE    RECE82-HKNCOMBI     TO  ORCSTEN-HKNCOMBI
      ***  MOVE    RECE82-HOSPNUM      TO  SPA-HOSPNUM
           CALL    "ORCSTENTEK"        USING
                                       SPA-AREA
                                       ORCSTENTEKAREA
      *
      *    IF      ORCSTEN-ERRCD       =   ZERO
      *
      *           ÌôºÞ
                  PERFORM  VARYING  IDY    FROM    1   BY  1
                              UNTIL   IDY     >       31
      *
                    MOVE   ZERO      TO   FLG-FORMALNAME
                    IF  ORCSTEN-ZAITEN(IDY)  NOT =  ZERO
      *
                       MOVE   1      TO   FLG-SIKIBETU
      *
                       INITIALIZE         WRK-ORCSTEN-TABLE
                       MOVE   ZERO   TO   IDX6
                       PERFORM  VARYING  IDX5    FROM    1   BY  1
                              UNTIL   IDX5     >       50
                                OR    ORCSTEN-SRYCD(IDY IDX5)  = SPACE
                          ADD   1   TO   IDX6
                          MOVE  ORCSTEN-SRYCD(IDY IDX5)  TO  
                                      WRK-ORCSTEN-SRYCD(IDX6)
                          MOVE  ORCSTEN-SURYO(IDY IDX5)  TO  
                                      WRK-ORCSTEN-SURYO(IDX6)
                          IF  ORCSTEN-HAIK-SRYCD(IDY IDX5) NOT = SPACE
                             ADD   1   TO   IDX6
                             MOVE  ORCSTEN-HAIK-SRYCD(IDY IDX5)
                                       TO   WRK-ORCSTEN-SRYCD(IDX6)
                             MOVE  ORCSTEN-HAIK-SURYO(IDY IDX5)
                                       TO   WRK-ORCSTEN-SURYO(IDX6)
                          END-IF 
                          PERFORM  VARYING  IDX13   FROM    1   BY  1
                            UNTIL  IDX13    >      5
                              OR   ORCSTEN-COM-SRYCD(IDY IDX5 IDX13)
                                                              = SPACE
                             ADD   1   TO   IDX6
                             MOVE  "810000001"
                                       TO   WRK-ORCSTEN-SRYCD(IDX6)
                             MOVE  1   TO   WRK-ORCSTEN-SURYO(IDX6)
                             MOVE  ORCSTEN-COM-MEI(IDY IDX5 IDX13)
                                       TO   WRK-ORCSTEN-COM-MEI(IDX6)
                          END-PERFORM
                       END-PERFORM
      *
                       PERFORM   2401-RECE84-TENSOU3-SEC
      *
                       MOVE      350  TO   IDX5
                       PERFORM   2402-RECE85-TENSOU3-SEC
                       PERFORM   2403-RECE82-HENSYU3-SEC
                       PERFORM   900-RECE84-WRITE-SEC
      *
                    END-IF
      *
                  END-PERFORM
      *
      *           ´ïºà
                  PERFORM  VARYING  IDY    FROM    1   BY  1
                              UNTIL   IDY     >       50
                                OR    ORCSTEN-KIZ-ZAITEN(IDY)  =  ZERO
      *
                       MOVE   ZERO   TO   FLG-FORMALNAME
                       MOVE   2      TO   FLG-SIKIBETU
      *
                       INITIALIZE         WRK-ORCSTEN-TABLE
                       MOVE   ZERO   TO   IDX6
                       PERFORM  VARYING  IDX5    FROM    1   BY  1
                              UNTIL   IDX5     >       10
                                OR    ORCSTEN-KIZ-SRYCD(IDY IDX5)
                                                               = SPACE
                          ADD   1   TO   IDX6
                          MOVE  ORCSTEN-KIZ-SRYCD(IDY IDX5)  TO  
                                      WRK-ORCSTEN-SRYCD(IDX6)
                          MOVE  ORCSTEN-KIZ-SURYO(IDY IDX5)  TO  
                                      WRK-ORCSTEN-SURYO(IDX6)
                          PERFORM  VARYING  IDX13   FROM    1   BY  1
                            UNTIL  IDX13    >      5
                              OR   ORCSTEN-KIZ-COM-SRYCD(IDY IDX5 IDX13)
                                                              = SPACE
                             ADD   1   TO   IDX6
                             MOVE  "810000001"
                                       TO   WRK-ORCSTEN-SRYCD(IDX6)
                             MOVE  1   TO   WRK-ORCSTEN-SURYO(IDX6)
                             MOVE  ORCSTEN-KIZ-COM-MEI(IDY IDX5 IDX13)
                                       TO   WRK-ORCSTEN-COM-MEI(IDX6)
                          END-PERFORM
                       END-PERFORM
                       PERFORM  VARYING  IDX5    FROM    1   BY  1
                              UNTIL   IDX5     >       31
                          MOVE  ORCSTEN-KIZ-DAY  (IDY IDX5)  TO  
                                      WRK-ORCSTEN-DAY  (IDX5)
                          ADD   ORCSTEN-KIZ-DAY  (IDY IDX5)  TO
                                      WRK-ORCSTEN-ZAIKAISU
                       END-PERFORM
      *
                       PERFORM   2401-RECE84-TENSOU3-SEC
      *
                       MOVE      60   TO   IDX5
                       PERFORM   2402-RECE85-TENSOU3-SEC
                       PERFORM   2403-RECE82-HENSYU3-SEC
                       PERFORM   900-RECE84-WRITE-SEC
      *
                  END-PERFORM
      *
      *    END-IF
      *
      ***
      *
      *    ÅÀÅ©¼êµ»²Ã»»
      *
           IF   WRK-TENTEKI2-HENKYAKUARI   NOT =  ZERO
      *
               IF   WRK-TENTEKI2-T-ERRCD       =  ZERO
                 PERFORM  VARYING  IDY    FROM    1   BY  1
                           UNTIL   IDY     >     31
                    IF  WRK-TENTEKI2-T-ZAITEN(IDY)  NOT =  ZERO
                       MOVE      3   TO   FLG-SIKIBETU
                       PERFORM   2401X-RECE84-TENSOU3-SEC
                       MOVE      WRK-TENTEKI2-T-SRYCD-X(IDY)
                                     TO   WRK-TENTEKI2-SRYCD-X
                       PERFORM   2402X-RECE85-TENSOU3-SEC
                       PERFORM   2403-RECE82-HENSYU3-SEC
                       PERFORM   900-RECE84-WRITE-SEC
                    END-IF
                 END-PERFORM
               END-IF
      *
               IF   WRK-TENTEKI2-C-ERRCD       =  ZERO
                 PERFORM  VARYING  IDY    FROM    1   BY  1
                           UNTIL   IDY     >     31
                    IF  WRK-TENTEKI2-C-ZAITEN(IDY)  NOT =  ZERO
                       MOVE      4   TO   FLG-SIKIBETU
                       PERFORM   2401X-RECE84-TENSOU3-SEC
                       MOVE      WRK-TENTEKI2-C-SRYCD-X(IDY)
                                     TO   WRK-TENTEKI2-SRYCD-X
                       PERFORM   2402X-RECE85-TENSOU3-SEC
                       PERFORM   2403-RECE82-HENSYU3-SEC
                       PERFORM   900-RECE84-WRITE-SEC
                    END-IF
                 END-PERFORM
               END-IF
      *
           END-IF
      *
           .
       240-RECE84-HENSYU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       2401-RECE84-TENSOU3-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE84-REC
           INITIALIZE                           RECE84-REC
           MOVE    RECE82-HOSPNUM       TO      RECE84-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE84-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE84-PTID
           MOVE    RECE82-RECEKA        TO      RECE84-RECEKA
           MOVE    RECE82-HKNID         TO      RECE84-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE84-TEKSTYMD
           IF    FLG-SIKIBETU  =  1
             COMPUTE WRK-ZAINUM    =  99997000  +  IDY
           ELSE
             COMPUTE WRK-ZAINUM    =  99997400  +  IDY
           END-IF
           MOVE    WRK-ZAINUM           TO      RECE84-ZAINUM
           MOVE    "33"                 TO      RECE84-SRYKBN
           MOVE    SPACE                TO      RECE84-ZAISKBKBN
           MOVE    RECE82-PTNUM         TO      RECE84-PTNUM
      *
           MOVE    "00"                 TO      RECE84-SRYKA
           MOVE    RECE82-HKNCOMBI      TO      RECE84-HKNCOMBI
           MOVE    SPACE                TO      RECE84-JIHOKBN
           IF    FLG-SIKIBETU  =  1
             MOVE  ORCSTEN-ZAITEN(IDY)  TO      RECE84-ZAITEN
           ELSE
             MOVE  ORCSTEN-KIZ-ZAITEN(IDY)  
                                        TO      RECE84-ZAITEN
           END-IF
           MOVE    ZERO                 TO      RECE84-SRYCDTOTAL
           MOVE    ZERO                 TO      RECE84-SURYOUTOTAL
           MOVE    ZERO                 TO      RECE84-MEISAISU
           IF    FLG-SIKIBETU  =  1
             MOVE  1                    TO      RECE84-ZAIKAISU
           ELSE
             MOVE  WRK-ORCSTEN-ZAIKAISU TO      RECE84-ZAIKAISU
           END-IF
           MOVE    ZERO                 TO      RECE84-SYUTEN1
           MOVE    ZERO                 TO      RECE84-SYUTEN2
           IF    FLG-SIKIBETU  =  1
             MOVE  ORCSTEN-ZAITEN(IDY)  TO      RECE84-YKZTEN
             MOVE  ZERO                 TO      RECE84-KIZAITEN
           ELSE
             MOVE  ZERO                 TO      RECE84-YKZTEN
             MOVE  ORCSTEN-KIZ-ZAITEN(IDY)
                                        TO      RECE84-KIZAITEN
           END-IF
           MOVE    SPACE                TO      RECE84-DRCD
           MOVE    SPACE                TO      RECE84-ZAIREQFLG
           MOVE    ZERO                 TO      RECE84-TEIGENRATE
      *
           IF    FLG-SIKIBETU  =  1
             MOVE    1                  TO      RECE84-KAISU (1 IDY)
             MOVE    IDY                TO      WK-TAIHI-DAY
           ELSE
             MOVE        ZERO           TO      WK-TAIHI-DAY
             MOVE        ZERO           TO      WK-TAIHI-FLG
             PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >       31
                   MOVE    WRK-ORCSTEN-DAY(IDX2)  TO
                                            RECE84-KAISU (1 IDX2)
                   IF     (WK-TAIHI-FLG            =   ZERO)   AND
                          (WRK-ORCSTEN-DAY(IDX2)  NOT =   ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF    
             END-PERFORM
           END-IF
           .
       2401-RECE84-TENSOU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       2401X-RECE84-TENSOU3-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE84-REC
           INITIALIZE                           RECE84-REC
           MOVE    RECE82-HOSPNUM       TO      RECE84-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE84-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE84-PTID
           MOVE    RECE82-RECEKA        TO      RECE84-RECEKA
           MOVE    RECE82-HKNID         TO      RECE84-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE84-TEKSTYMD
           IF    FLG-SIKIBETU  =  3
             COMPUTE WRK-ZAINUM    =  99995000  +  IDY
           ELSE
             COMPUTE WRK-ZAINUM    =  99995400  +  IDY
           END-IF
           MOVE    WRK-ZAINUM           TO      RECE84-ZAINUM
           MOVE    "33"                 TO      RECE84-SRYKBN
           MOVE    SPACE                TO      RECE84-ZAISKBKBN
           MOVE    RECE82-PTNUM         TO      RECE84-PTNUM
      *
           MOVE    "00"                 TO      RECE84-SRYKA
           MOVE    RECE82-HKNCOMBI      TO      RECE84-HKNCOMBI
           MOVE    SPACE                TO      RECE84-JIHOKBN
           IF    FLG-SIKIBETU  =  3
             MOVE  WRK-TENTEKI2-T-ZAITEN(IDY)
                                        TO      RECE84-ZAITEN
                                                RECE84-SYUTEN1
             MOVE  WRK-TENTEKI2-T-MEISAISU(IDY)
                                        TO      RECE84-MEISAISU
                                                RECE84-SURYOUTOTAL
             MOVE  WRK-TENTEKI2-T-SRYCDTOTAL(IDY)
                                        TO      RECE84-SRYCDTOTAL
           ELSE
             MOVE  WRK-TENTEKI2-C-ZAITEN(IDY)
                                        TO      RECE84-ZAITEN
                                                RECE84-SYUTEN1
             MOVE  WRK-TENTEKI2-C-MEISAISU(IDY)
                                        TO      RECE84-MEISAISU
                                                RECE84-SURYOUTOTAL
             MOVE  WRK-TENTEKI2-C-SRYCDTOTAL(IDY)
                                        TO      RECE84-SRYCDTOTAL
           END-IF
           MOVE    1                    TO      RECE84-ZAIKAISU
           MOVE    ZERO                 TO      RECE84-SYUTEN2
           MOVE    ZERO                 TO      RECE84-YKZTEN
           MOVE    ZERO                 TO      RECE84-KIZAITEN
           MOVE    SPACE                TO      RECE84-DRCD
           MOVE    SPACE                TO      RECE84-ZAIREQFLG
           MOVE    ZERO                 TO      RECE84-TEIGENRATE
      *
           MOVE    1                    TO      RECE84-KAISU (1 IDY)
           MOVE    IDY                  TO      WK-TAIHI-DAY
           .
       2401X-RECE84-TENSOU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       2402-RECE85-TENSOU3-SEC                SECTION.
      *
           MOVE    1                       TO   IDX2
           MOVE    ZERO                    TO   WRK-RENNUM
           MOVE    ZERO                    TO   WRK-TANICD
           MOVE    ZERO                    TO   WRK-TANIMOJISU
           MOVE    SPACE                   TO   WRK-TANINAME
      *
           PERFORM
                  UNTIL     (IDX2      >   IDX5)    OR
                            (WRK-ORCSTEN-SRYCD(IDX2)  =   SPACE)
      *
              MOVE    SPACE                TO   RECE85-REC
              INITIALIZE                        RECE85-REC
              MOVE    RECE82-HOSPNUM       TO   RECE85-HOSPNUM
              MOVE    RECE82-SRYYM         TO   RECE85-SRYYM
              MOVE    RECE82-NYUGAIKBN     TO   RECE85-NYUGAIKBN
              MOVE    RECE82-PTID          TO   RECE85-PTID
              MOVE    RECE82-RECEKA        TO   RECE85-RECEKA
              MOVE    RECE82-HKNID         TO   RECE85-HKNID
              MOVE    RECE82-TEKSTYMD      TO   RECE85-TEKSTYMD
              IF    FLG-SIKIBETU  =  1
                COMPUTE WRK-ZAINUM    =  99997000  +  IDY
              ELSE
                COMPUTE WRK-ZAINUM    =  99997400  +  IDY
              END-IF
              MOVE    WRK-ZAINUM           TO   RECE85-ZAINUM
              ADD     1                    TO   WRK-RENNUM
              MOVE    WRK-RENNUM           TO   RECE85-RENNUM
              MOVE    RECE82-PTNUM         TO   RECE85-PTNUM
      *
              MOVE    "00"                 TO   RECE85-SRYKA
              MOVE    "33"                 TO   RECE85-SRYKBN
              MOVE    "330"                TO   RECE85-SRYSYUKBN
              MOVE    ZERO                 TO   RECE85-JIHIMONEY
      *
              MOVE    ZERO                 TO      IDX4
              PERFORM
                     UNTIL  (IDX4      >=   5  )    OR
                            (IDX2      >   IDX5)    OR
                            (WRK-ORCSTEN-SRYCD(IDX2)  =   SPACE)
      *
                IF   WRK-ORCSTEN-SRYCD(IDX2)(1:1) =   "0" OR  "6" OR  
                                                      "7" OR  "8"
      *
                  ADD   1                   TO  IDX4    
      *
                  MOVE  WRK-ORCSTEN-SRYCD(IDX2) 
                                            TO  RECE85-SRYCD(IDX4)
                  MOVE  WRK-ORCSTEN-SURYO(IDX2) 
                                            TO  RECE85-SRYSURYO(IDX4)
                  MOVE  ZERO                TO  RECE85-SRYKAISU(IDX4)
                  MOVE  SPACE               TO  RECE85-MEISKYFLG(IDX4)
                  MOVE  SPACE               TO  RECE85-AUTOKBN(IDX4)
                  MOVE  ZERO                TO  RECE85-INPUTNUM(IDX4)
                  MOVE  WRK-ORCSTEN-SRYCD(IDX2) 
                                            TO  WRK-SRYCD
                  MOVE  RECE85-SRYYM        TO  WK-TAIHI-SRYYM
                  MOVE  WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
                  PERFORM    900-TENSU-TBL-SEC
                  IF  WRK-ORCSTEN-SRYCD(IDX2)  =  CONS-ZANHAIKI
                    IF  RECE85-SRYSURYO(IDX4)  NOT = ZERO
                      MOVE   WRK-TANICD     TO  RECE85-TANICD(IDX4)
                      MOVE   WRK-TANIMOJISU TO  RECE85-TANIMOJISU(IDX4)
                      MOVE   WRK-TANINAME   TO  RECE85-TANINAME(IDX4)
                    ELSE
                      MOVE   "1"            TO  RECE85-RECEPTFLG(IDX4)
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
                      MOVE  TNS-TANICD      TO  RECE85-TANICD(IDX4)
                                                WRK-TANICD
                      MOVE  TNS-TANIMOJISU  TO  RECE85-TANIMOJISU(IDX4)
                                                WRK-TANIMOJISU
                      MOVE  TNS-TANINAME    TO  RECE85-TANINAME(IDX4)
                                                WRK-TANINAME
                  END-IF
                  MOVE  TNS-TENSIKIBETU     TO  RECE85-TENSIKIBETU(IDX4)
                  MOVE  TNS-TEN             TO  RECE85-TEN(IDX4)
                  MOVE  TNS-NYUTENTTLKBN    TO  RECE85-TENTTLKBN(IDX4)
                  IF  (WRK-ORCSTEN-SRYCD(IDX2)(1:1)  =   "8"  )
                  OR  (WRK-ORCSTEN-SRYCD(IDX2)(1:3)  =   "008")
                     MOVE   ZERO            TO  RECE85-YUKOKETA(IDX4)
                  ELSE
                     MOVE   TNS-YUKOKETA    TO  RECE85-YUKOKETA(IDX4)
                  END-IF
                  MOVE  TNS-DATAKBN         TO  RECE85-DATAKBN (IDX4)
                  MOVE  TNS-HOUKNSKBN       TO  RECE85-HOUKNSKBN(IDX4)
                  MOVE  TNS-KNSJISKBN       TO  RECE85-KNSJISKBN(IDX4)
                  MOVE  TNS-CDKBN-KBN       TO  RECE85-CDKBN-KBN(IDX4)
                  MOVE  TNS-CDKBN-SYO       TO  RECE85-CDKBN-SYO(IDX4)
                  MOVE  TNS-CDKBN-BU        TO  RECE85-CDKBN-BU(IDX4)
                  MOVE  TNS-CDKBN-KBNNUM    
                                      TO  RECE85-CDKBN-KBNNUM(IDX4)
                  MOVE  TNS-CDKBN-KBNNUM-EDA
                                      TO  RECE85-CDKBN-KBNNUM-EDA(IDX4)
                  MOVE  TNS-CDKBN-KOUBAN
                                      TO  RECE85-CDKBN-KOUBAN(IDX4)
                  MOVE  TNS-KOUHYOJYUNNUM  
                                      TO  RECE85-KOUHYOJYUNNUM(IDX4)
      *
                  MOVE     "1"              TO  RECE85-SIKFLG(IDX4)
      *
                  IF  (WRK-ORCSTEN-SRYCD(IDX2)(1:1)  =   "8"  )
                  OR  (WRK-ORCSTEN-SRYCD(IDX2)(1:3)  =   "008")
                     MOVE   WRK-ORCSTEN-COM-MEI(IDX2)
                                      TO RECE85-NAME(IDX4)
                  ELSE
                     MOVE   TNS-NAME  TO RECE85-NAME(IDX4)
                  END-IF
      *
                  IF   WRK-ORCSTEN-SRYCD(IDX2)(1:3)  =   "058"
                    MOVE  TNS-FORMALYUKOKETA TO  RECE85-YUKOKETA(IDX4)
                    MOVE  TNS-FORMALNAME     TO  RECE85-NAME(IDX4)
                    MOVE  1                  TO  FLG-FORMALNAME
                  END-IF
                  IF  (WRK-ORCSTEN-SRYCD(IDX2)(1:1)  =   "7")
                  AND (FLG-FORMALNAME                =    1 )
                    MOVE  "1"             TO  RECE85-RECEHENFLG(IDX4)
                    MOVE  ZERO            TO  FLG-FORMALNAME
                  END-IF
      *
                  IF   RECE85-YUKOKETA(IDX4)  =  ZERO
                     PERFORM   VARYING   IDW   FROM   200   BY  -1
                                UNTIL    IDW   <   1
                       IF   RECE85-NAME(IDX4)(IDW:1)  NOT =   SPACE
                           COMPUTE RECE85-YUKOKETA(IDX4)  =
                                   IDW     /   2
                           MOVE    1          TO  IDW
                       END-IF
                     END-PERFORM
                  END-IF
      *
                  IF  (WRK-ORCSTEN-SRYCD(IDX2)(1:1)  =   "8"  )
                  OR  (WRK-ORCSTEN-SRYCD(IDX2)(1:3)  =   "008")
                        MOVE     "1"             TO  RECE84-COMCDFLG
                  END-IF
      *
                  MOVE  WRK-ORCSTEN-SRYCD(IDX2)  TO  WRK-SRYCD
                  COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL
                                              +  WRK-SRYCD
                  COMPUTE  RECE84-SURYOUTOTAL =  RECE84-SURYOUTOTAL
                                              +  WRK-ORCSTEN-SURYO(IDX2)
                  COMPUTE  RECE84-MEISAISU    =  RECE84-MEISAISU
                                              +  1
      *
                  MOVE     ZERO    TO    WRK-FUKA-KINGAKU
                  PERFORM  VARYING  FUKA-IDX  FROM  1  BY  1
                            UNTIL   FUKA-IDX   >    9
                    MOVE   WRK-ORCSTEN-SRYCD(IDX2)(FUKA-IDX:1)
                                            TO  WRK-FUKA-VALUE
                    IF  WRK-FUKA-VALUE  =  ZERO
                      CONTINUE
                    ELSE
                      COMPUTE  RECE84-RUIKEITOTAL  =
                               RECE84-RUIKEITOTAL  +
                             ( WRK-FUKA-VALUE    * WRK-SRYCD)
                      COMPUTE  WRK-FUKA-KINGAKU    =
                               WRK-FUKA-KINGAKU    +
                             ((WRK-FUKA-VALUE^4) * FUKA-IDX
                                   * WRK-ORCSTEN-SURYO(IDX2))
                    END-IF
                  END-PERFORM
                  COMPUTE  RECE84-RUIKEITOTAL  =
                           RECE84-RUIKEITOTAL  +
                           WRK-FUKA-KINGAKU
      *
      *           ÆÃÄê´ïºà¤ÎÃ±²ÁÊÔ½¸
                  IF      WRK-ORCSTEN-SRYCD(IDX2)(1:1)  =   "7"
                    IF  WRK-ORCSTEN-SRYCD(IDX2)       =
                                       CONS-FILM-YOUJI-KASAN  OR
                                       CONS-SANSO             OR
                                       CONS-SANSO2              
                      CONTINUE
                    ELSE
                      IF      TNS-TEN         =   ZERO
                        CONTINUE
                      ELSE
                        PERFORM 220213-RECE85-TENSOU-TANKA-HEN-SEC
                        COMPUTE IDW =   RECE85-YUKOKETA(IDX4)  *  2
                                    +   3
                        MOVE    WRK-CHAR-N (1:WRK-CHAR-N-CNT)
                                   TO   RECE85-NAME(IDX4)(IDW:)
                        COMPUTE RECE85-YUKOKETA(IDX4)  =
                                RECE85-YUKOKETA(IDX4)  +
                                  1                    +
                               (WRK-CHAR-N-CNT  /  2)
                      END-IF
                    END-IF
                  END-IF
      *
                END-IF  
      *
                ADD        1                TO  IDX2
      *
              END-PERFORM
      *
              PERFORM   900-RECE85-WRITE-SEC
      *
           END-PERFORM
      *
           .
       2402-RECE85-TENSOU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       2402X-RECE85-TENSOU3-SEC                SECTION.
      *
           MOVE    SPACE                TO   RECE85-REC
           INITIALIZE                        RECE85-REC
           MOVE    RECE82-HOSPNUM       TO   RECE85-HOSPNUM
           MOVE    RECE82-SRYYM         TO   RECE85-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO   RECE85-NYUGAIKBN
           MOVE    RECE82-PTID          TO   RECE85-PTID
           MOVE    RECE82-RECEKA        TO   RECE85-RECEKA
           MOVE    RECE82-HKNID         TO   RECE85-HKNID
           MOVE    RECE82-TEKSTYMD      TO   RECE85-TEKSTYMD
           IF    FLG-SIKIBETU  =  3
             COMPUTE WRK-ZAINUM    =  99995000  +  IDY
           ELSE
             COMPUTE WRK-ZAINUM    =  99995400  +  IDY
           END-IF
           MOVE    WRK-ZAINUM           TO   RECE85-ZAINUM
           MOVE    1                    TO   RECE85-RENNUM
           MOVE    RECE82-PTNUM         TO   RECE85-PTNUM
      *
           MOVE    "00"                 TO   RECE85-SRYKA
           MOVE    "33"                 TO   RECE85-SRYKBN
           IF    FLG-SIKIBETU  =  3
              MOVE    "330"             TO   RECE85-SRYSYUKBN
           ELSE
              MOVE    "340"             TO   RECE85-SRYSYUKBN
           END-IF
           MOVE    ZERO                 TO   RECE85-JIHIMONEY
      *
           MOVE    ZERO                 TO   IDX4
           PERFORM   VARYING   IDX2   FROM    1    BY   1
                      UNTIL   (IDX2                      >     5  )
                        OR    (WRK-TENTEKI2-SRYCD(IDX2)  =   SPACE)
      *
                  ADD   1                   TO  IDX4
      *
                  MOVE  WRK-TENTEKI2-SRYCD(IDX2)
                                            TO  RECE85-SRYCD(IDX4)
                  MOVE  1                   TO  RECE85-SRYSURYO(IDX4)
                  MOVE  ZERO                TO  RECE85-SRYKAISU(IDX4)
                  MOVE  SPACE               TO  RECE85-MEISKYFLG(IDX4)
                  MOVE  SPACE               TO  RECE85-AUTOKBN(IDX4)
                  MOVE  ZERO                TO  RECE85-INPUTNUM(IDX4)
                  MOVE  WRK-TENTEKI2-SRYCD(IDX2)
                                            TO  WRK-SRYCD
                  MOVE  RECE85-SRYYM        TO  WK-TAIHI-SRYYM
                  MOVE  WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
                  PERFORM    900-TENSU-TBL-SEC
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
                  MOVE  TNS-TANICD          TO  RECE85-TANICD(IDX4)
                  MOVE  TNS-TANIMOJISU      TO  RECE85-TANIMOJISU(IDX4)
                  MOVE  TNS-TANINAME        TO  RECE85-TANINAME(IDX4)
                  MOVE  TNS-TENSIKIBETU     TO  RECE85-TENSIKIBETU(IDX4)
                  MOVE  TNS-TEN             TO  RECE85-TEN(IDX4)
                  MOVE  TNS-NYUTENTTLKBN    TO  RECE85-TENTTLKBN(IDX4)
                  MOVE  TNS-YUKOKETA        TO  RECE85-YUKOKETA(IDX4)
                  MOVE  TNS-DATAKBN         TO  RECE85-DATAKBN (IDX4)
                  MOVE  TNS-HOUKNSKBN       TO  RECE85-HOUKNSKBN(IDX4)
                  MOVE  TNS-KNSJISKBN       TO  RECE85-KNSJISKBN(IDX4)
                  MOVE  TNS-CDKBN-KBN       TO  RECE85-CDKBN-KBN(IDX4)
                  MOVE  TNS-CDKBN-SYO       TO  RECE85-CDKBN-SYO(IDX4)
                  MOVE  TNS-CDKBN-BU        TO  RECE85-CDKBN-BU(IDX4)
                  MOVE  TNS-CDKBN-KBNNUM
                                      TO  RECE85-CDKBN-KBNNUM(IDX4)
                  MOVE  TNS-CDKBN-KBNNUM-EDA
                                      TO  RECE85-CDKBN-KBNNUM-EDA(IDX4)
                  MOVE  TNS-CDKBN-KOUBAN
                                      TO  RECE85-CDKBN-KOUBAN(IDX4)
                  MOVE  TNS-KOUHYOJYUNNUM
                                      TO  RECE85-KOUHYOJYUNNUM(IDX4)
                  MOVE  TNS-NAME      TO  RECE85-NAME(IDX4)
                  MOVE     "1"        TO  RECE85-SIKFLG(IDX4)
      *
                  MOVE     ZERO       TO  WRK-FUKA-KINGAKU
                  PERFORM  VARYING  FUKA-IDX  FROM  1  BY  1
                            UNTIL   FUKA-IDX   >    9
                    MOVE   WRK-TENTEKI2-SRYCD(IDX2)(FUKA-IDX:1)
                                            TO  WRK-FUKA-VALUE
                    IF  WRK-FUKA-VALUE  =  ZERO
                      CONTINUE
                    ELSE
                      COMPUTE  RECE84-RUIKEITOTAL  =
                               RECE84-RUIKEITOTAL  +
                             ( WRK-FUKA-VALUE    * WRK-SRYCD)
                      COMPUTE  WRK-FUKA-KINGAKU    =
                               WRK-FUKA-KINGAKU    +
                             ((WRK-FUKA-VALUE^4) * FUKA-IDX
                                                 * 1)
                    END-IF
                  END-PERFORM
                  COMPUTE  RECE84-RUIKEITOTAL  =
                           RECE84-RUIKEITOTAL  +
                           WRK-FUKA-KINGAKU
      *
           END-PERFORM
      *
           IF     IDX4    NOT =  ZERO
              PERFORM   900-RECE85-WRITE-SEC
           END-IF
      *
           .
       2402X-RECE85-TENSOU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷£²¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       2403-RECE82-HENSYU3-SEC               SECTION.
      *
      *    ¤½¤ÎÂ¾Ãí¼Í¤Î¥ì¥»ÅÀ¿ôÍó¤ÎÊÔ½¸
           EVALUATE   RECE82-HKNKBN
            WHEN       "1"
            WHEN       "2"
             ADD   RECE84-ZAIKAISU    TO   RECE82-KAISU(12)
      *
             COMPUTE   RECE82-TENSU(12)  =
                       RECE82-TENSU(12)  +
                       (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
            WHEN       "4"
                EVALUATE  WRK-PARA-RECEKBN
                  WHEN      "4"
      **************¡Ê¿·ÍÍ¼°¡Ë
                    ADD   RECE84-ZAIKAISU    TO   RECE82-KAISU(12)
      *
                    IF    FLG-SIKIBETU  =  3 OR 4
                       COMPUTE   RECE82-TENSU(12)  =
                                 RECE82-TENSU(12)  +
                                 (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
                    ELSE
                       COMPUTE   RECE82-TENSU-YAK(13)  =
                                 RECE82-TENSU-YAK(13)  +
                                 (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
                    END-IF
                  WHEN      "5"
      **************¡Ê½¾ÍèÍÍ¼°¡Ë
                    ADD   RECE84-ZAIKAISU    TO   RECE82-KAISU(13)
      *
                    IF    FLG-SIKIBETU  =  3 OR 4
                       COMPUTE   RECE82-TENSU(13)  =
                                 RECE82-TENSU(13)  +
                                 (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
                       COMPUTE   TBL-SYUTEN-JI(13)  =
                                 TBL-SYUTEN-JI(13)  +
                                 (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
                    ELSE
                       COMPUTE   RECE82-TENSU(13)  =
                                 RECE82-TENSU(13)  +
                                 (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
                       COMPUTE   TBL-YKZTEN-JI(13) =
                                 TBL-YKZTEN-JI(13) +
                                 (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
                    END-IF
                END-EVALUATE
            WHEN       "5"
             ADD   RECE84-ZAIKAISU    TO   RECE82-KAISU(27)
      *
             COMPUTE   RECE82-TENSU(27)  =
                       RECE82-TENSU(27)  +
                       (RECE84-ZAITEN  *  RECE84-ZAIKAISU)
           END-EVALUATE
           .                                           
      *
       2403-RECE82-HENSYU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤ÎºîÀ®¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       250-RECE84-HENSYU4-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-KHN
                                           WRK-SROUNYUIN-SRYCDTOTAL
                                           WRK-SROUNYUIN-MEISAISU
                                           WRK-SROUNYUIN-SRYCD
      *
           INITIALIZE                      ORCSROUNYUIN-AREA
           MOVE    RECE82-HOSPNUM      TO  LNK-SROUNYUIN-HOSPNUM
           MOVE    RECE82-PTID         TO  LNK-SROUNYUIN-PTID
           MOVE    RECE82-SRYYM        TO  LNK-SROUNYUIN-SANTEIYM
           MOVE    RECE82-HKNCOMBI     TO  LNK-SROUNYUIN-HKNCOMBI
           MOVE    SPACE               TO  LNK-SROUNYUIN-RYAKUKBN
      *
           CALL    "ORCSROUNYUIN"      USING   MCPAREA
                                               ORCSROUNYUIN-AREA
                                               SPA-AREA
      *
           IF   LNK-SROUNYUIN-RC   =   ZERO
      *
              PERFORM    VARYING   IDY   FROM   1   BY   1
                          UNTIL    IDY    >    31
      *
                IF   LNK-SROUNYUIN-NYUIN-FLG(IDY)    =  "1"
                AND  RECE82-NYUINDAY-KBN(IDY)        =  SPACE
      *
                   MOVE     ZERO        TO    WRK-RENNUM
      *
                   IF   LNK-SROUNYUIN-GAIHAKU(IDY)  =  ZERO
      *
                     MOVE     1     TO    FLG-KHN
      *
                     PERFORM   2501-RECE84-TENSOU2-KHN-SEC
      *
                     MOVE     1     TO    SYUKEI-IDX
                     MOVE     5     TO    SYUKEI-IDX2
                     IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)  
                                                     NOT = SPACE
                        PERFORM   2502-RECE85-TENSOU2-KHN-SEC
                        PERFORM   900-RECE85-WRITE-SEC
                     END-IF
      *
                     MOVE     6     TO    SYUKEI-IDX
                     MOVE    10     TO    SYUKEI-IDX2
                     IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)  
                                                     NOT = SPACE
                        PERFORM   2502-RECE85-TENSOU2-KHN-SEC
                        PERFORM   900-RECE85-WRITE-SEC
                     END-IF
      *
                     MOVE    11     TO    SYUKEI-IDX
                     MOVE    15     TO    SYUKEI-IDX2
                     IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)  
                                                     NOT = SPACE
                        PERFORM   2502-RECE85-TENSOU2-KHN-SEC
                        PERFORM   900-RECE85-WRITE-SEC
                     END-IF
      *
                     MOVE    16     TO    SYUKEI-IDX
                     MOVE    20     TO    SYUKEI-IDX2
                     IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)  
                                                     NOT = SPACE
                        PERFORM   2502-RECE85-TENSOU2-KHN-SEC
                        PERFORM   900-RECE85-WRITE-SEC
                     END-IF
      *
                     PERFORM   900-RECE84-WRITE-SEC
      *
                   ELSE
      *
                     MOVE     2     TO    FLG-KHN
      *
                     MOVE      LNK-SROUNYUIN-SRYCD(IDY 1)
                                        TO   WRK-SROUNYUIN-SRYCD
                     COMPUTE   WRK-SROUNYUIN-SRYCDTOTAL      =
                               LNK-SROUNYUIN-SRYCDTOTAL(IDY) -
                               WRK-SROUNYUIN-SRYCD
                     COMPUTE   WRK-SROUNYUIN-MEISAISU        =
                               LNK-SROUNYUIN-MEISAISU(IDY)   -
                               1
      *
                     PERFORM   2501-RECE84-TENSOU2-KHN-SEC
      *
                     MOVE     2     TO    SYUKEI-IDX
                     MOVE     6     TO    SYUKEI-IDX2
                     IF   LNK-SROUNYUIN-SRYCD(IDY SYUKEI-IDX)  
                                                     NOT = SPACE
                        PERFORM   2502-RECE85-TENSOU2-KHN-SEC
                        PERFORM   900-RECE85-WRITE-SEC
                     END-IF
      *
                     PERFORM   900-RECE84-WRITE-SEC
      *
                   END-IF
      *
                END-IF
      *
              END-PERFORM
      *
           END-IF
      *
           .
       250-RECE84-HENSYU4-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       2501-RECE84-TENSOU2-KHN-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE84-REC
           INITIALIZE                           RECE84-REC
           MOVE    RECE82-HOSPNUM       TO      RECE84-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE84-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE84-PTID
           MOVE    RECE82-RECEKA        TO      RECE84-RECEKA
           MOVE    RECE82-HKNID         TO      RECE84-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE84-TEKSTYMD
      *
           COMPUTE WRK-ZAINUM  =  99999800  +  IDY
           MOVE    WRK-ZAINUM           TO      RECE84-ZAINUM
      *
           MOVE    LNK-SROUNYUIN-SRYKBN(IDY)
                                        TO      RECE84-SRYKBN
           IF      FLG-KHN  =  1
              MOVE    "1"               TO      RECE84-ZAISKBKBN
           ELSE
              MOVE    "2"               TO      RECE84-ZAISKBKBN
           END-IF
           MOVE    RECE82-PTNUM         TO      RECE84-PTNUM
      *
           MOVE    "00"                 TO      RECE84-SRYKA
           MOVE    RECE82-HKNCOMBI      TO      RECE84-HKNCOMBI
           MOVE    SPACE                TO      RECE84-JIHOKBN
           MOVE    LNK-SROUNYUIN-ZAITEN(IDY)
                                        TO      RECE84-ZAITEN
           IF      FLG-KHN  =  1
              MOVE    LNK-SROUNYUIN-SRYCDTOTAL(IDY)
                                        TO      RECE84-SRYCDTOTAL
              MOVE    LNK-SROUNYUIN-MEISAISU(IDY)
                                        TO      RECE84-SURYOUTOTAL
              MOVE    LNK-SROUNYUIN-MEISAISU(IDY)
                                        TO      RECE84-MEISAISU
           ELSE
              MOVE    WRK-SROUNYUIN-SRYCDTOTAL
                                        TO      RECE84-SRYCDTOTAL
              MOVE    WRK-SROUNYUIN-MEISAISU
                                        TO      RECE84-SURYOUTOTAL
              MOVE    WRK-SROUNYUIN-MEISAISU
                                        TO      RECE84-MEISAISU
           END-IF
           MOVE    1                    TO      RECE84-ZAIKAISU
           MOVE    LNK-SROUNYUIN-ZAITEN(IDY)
                                        TO      RECE84-SYUTEN1
           MOVE    ZERO                 TO      RECE84-RUIKEITOTAL
           MOVE    ZERO                 TO      RECE84-SYUTEN2
           MOVE    ZERO                 TO      RECE84-YKZTEN
           MOVE    ZERO                 TO      RECE84-KIZAITEN
           MOVE    SPACE                TO      RECE84-DRCD
           MOVE    SPACE                TO      RECE84-ZAIREQFLG
           MOVE    ZERO                 TO      RECE84-TEIGENRATE
      *
           MOVE    1                    TO      RECE84-KAISU (1 IDY)
           MOVE    IDY                  TO      WK-TAIHI-DAY
           .
       2501-RECE84-TENSOU2-KHN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       2502-RECE85-TENSOU2-KHN-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE85-REC
           INITIALIZE                           RECE85-REC
           MOVE    RECE82-HOSPNUM       TO      RECE85-HOSPNUM
           MOVE    RECE82-SRYYM         TO      RECE85-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE85-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE85-PTID
           MOVE    RECE82-RECEKA        TO      RECE85-RECEKA
           MOVE    RECE82-HKNID         TO      RECE85-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE85-TEKSTYMD
      *
           COMPUTE WRK-ZAINUM   =   99999800   +   IDY
           MOVE    WRK-ZAINUM           TO      RECE85-ZAINUM
      *
           ADD     1                    TO      WRK-RENNUM
           MOVE    WRK-RENNUM           TO      RECE85-RENNUM
           MOVE    RECE82-PTNUM         TO      RECE85-PTNUM
      *
           MOVE    "00"                 TO      RECE85-SRYKA
           MOVE    LNK-SROUNYUIN-SRYKBN(IDY)
                                        TO      RECE85-SRYKBN
           IF      LNK-SROUNYUIN-SRYKBN(IDY) =  "90"
              MOVE    "900"             TO      RECE85-SRYSYUKBN
           ELSE
              MOVE    "920"             TO      RECE85-SRYSYUKBN
           END-IF 
           MOVE    ZERO                 TO      RECE85-JIHIMONEY
      *
           MOVE    ZERO                 TO      IDX4
           PERFORM     VARYING   IDX2   FROM   SYUKEI-IDX   BY   1
               UNTIL (IDX2      >    SYUKEI-IDX2)
                OR   (LNK-SROUNYUIN-SRYCD(IDY IDX2)  =   SPACE)
      *
               ADD     1                   TO  IDX4    
               MOVE    LNK-SROUNYUIN-SRYCD(IDY IDX2)
                                           TO  RECE85-SRYCD(IDX4)
               MOVE    1                   TO  RECE85-SRYSURYO(IDX4)
               MOVE    ZERO                TO  RECE85-SRYKAISU(IDX4)
               MOVE    SPACE               TO  RECE85-MEISKYFLG(IDX4)
               MOVE    SPACE               TO  RECE85-AUTOKBN(IDX4)
               MOVE    ZERO                TO  RECE85-INPUTNUM(IDX4)
               MOVE    LNK-SROUNYUIN-SRYCD(IDY IDX2)
                                           TO  WRK-SRYCD
               MOVE    RECE85-SRYYM        TO  WK-TAIHI-SRYYM
               MOVE    WK-TAIHI-DAY        TO  WK-TAIHI-SRYD
               PERFORM    900-TENSU-TBL-SEC
               MOVE    TNS-TANICD          TO  RECE85-TANICD(IDX4)
               MOVE    TNS-TANIMOJISU      TO  RECE85-TANIMOJISU(IDX4)
               MOVE    TNS-TANINAME        TO  RECE85-TANINAME(IDX4)
               MOVE    TNS-TENSIKIBETU     TO  RECE85-TENSIKIBETU(IDX4)
               MOVE    TNS-TEN             TO  RECE85-TEN(IDX4)
               MOVE    TNS-NYUTENTTLKBN   TO    RECE85-TENTTLKBN(IDX4)
               MOVE    TNS-YUKOKETA       TO    RECE85-YUKOKETA(IDX4)
               MOVE    TNS-DATAKBN        TO    RECE85-DATAKBN (IDX4)
               MOVE    TNS-HOUKNSKBN      TO    RECE85-HOUKNSKBN(IDX4)
               MOVE    TNS-KNSJISKBN      TO    RECE85-KNSJISKBN(IDX4)
               MOVE    TNS-CDKBN-KBN      TO    RECE85-CDKBN-KBN(IDX4)
               MOVE    TNS-CDKBN-SYO      TO    RECE85-CDKBN-SYO(IDX4)
               MOVE    TNS-CDKBN-BU       TO    RECE85-CDKBN-BU(IDX4)
               MOVE    TNS-CDKBN-KBNNUM   
                                      TO RECE85-CDKBN-KBNNUM(IDX4)
               MOVE    TNS-CDKBN-KBNNUM-EDA
                                      TO RECE85-CDKBN-KBNNUM-EDA(IDX4)
               MOVE    TNS-CDKBN-KOUBAN
                                      TO RECE85-CDKBN-KOUBAN(IDX4)
               MOVE    TNS-KOUHYOJYUNNUM  
                                      TO RECE85-KOUHYOJYUNNUM(IDX4)
               MOVE    TNS-NAME           TO    RECE85-NAME(IDX4)
               MOVE     "1"               TO    RECE85-SIKFLG(IDX4)
      *
           END-PERFORM
      *
           .
       2502-RECE85-TENSOU2-KHN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Ø¤ÎÉéÃ´¶â¥»¥Ã¥È¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       260-FTN-SET-SEC                SECTION.
      *
           MOVE    ZERO                 TO      FLG-KHNRYO-SKJ
      *
           MOVE    ZERO                 TO      FLG-NYUINRYO
      *
           MOVE    ZERO                 TO      FLG-SYUNOU-ARI
      *
           INITIALIZE                           SYUNOU-REC
           MOVE    RECE82-HOSPNUM       TO      SYU-HOSPNUM
           MOVE    RECE82-NYUGAIKBN     TO      SYU-NYUGAIKBN
           MOVE    RECE82-PTID          TO      SYU-PTID
           MOVE    RECE82-SRYYM         TO      SYU-SRYYMD(1:6)
           MOVE    "__"                 TO      SYU-SRYYMD(7:2)
           MOVE    RECE82-HKNCOMBI      TO      SYU-HKNCOMBINUM
           MOVE    SYUNOU-REC           TO      MCPDATA-REC
           MOVE    "tbl_syunou"         TO      MCP-TABLE
           MOVE    "key19"              TO      MCP-PATHNAME
grpsys     PERFORM  900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_syunou"     TO      MCP-TABLE
               MOVE    "key19"          TO      MCP-PATHNAME
               PERFORM  900-SYUNOU-READ-SEC
               PERFORM  UNTIL
                  (FLG-SYUNOU             =    1)
      *
                   IF  SYU-DENPJTIKBN    NOT =  "3"
                       PERFORM   2601-RECE82-FTN-HENSYU-SEC
                       MOVE   1   TO   FLG-SYUNOU-ARI
                   END-IF
      *
                   MOVE    "tbl_syunou" TO      MCP-TABLE
                   MOVE    "key19"      TO      MCP-PATHNAME
                   PERFORM  900-SYUNOU-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_syunou"         TO      MCP-TABLE
           MOVE    "key19"              TO      MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
      *    ¿©»öÎ¬¾ÎÊÔ½¸
      *
           IF   RECE82-SRYYM  >= "200604"
           AND  RECE82-SRYYM  <  "200610"
                PERFORM    2602-SKJ-RAYKU-HENSYU-SEC
           END-IF
      *
           IF   FLG-SYUNOU-ARI  =  ZERO
              DISPLAY "<SYUNOU NG STOP>"
              MOVE    "²¾¼ýÇ¼¥Ç¡¼¥¿¤¬¤¢¤ê¤Þ¤»¤ó"  TO  ERR-ERRMSG
              PERFORM 600-ERRMSG-EDIT-SEC
              MOVE    ERR-HEN-ERRMSG    TO  WRK-RECEERR
              PERFORM 500-ERR-HENSYU-SEC
              MOVE    1                 TO  FLG-END
           END-IF
      *
           .
       260-FTN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î¹àÌÜÅ¾Á÷¡ÝÉéÃ´¶â¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       2601-RECE82-FTN-HENSYU-SEC               SECTION.
      *
           IF   RECE82-NYUINRYOFLG       =   "1"
           AND  FLG-NYUINRYO             =   ZERO
               PERFORM         26011-NYUINRYOFLG-SET-SEC
           END-IF
      *
           IF    RECE82-SRYYM  >= "200610"
      *
             PERFORM  VARYING  ID1    FROM    1   BY  1
                       UNTIL   ID1      >    10
      *
               IF  SYU-SHOKUJI(ID1)   =   ZERO
                   CONTINUE
               ELSE
                 IF   (RECE82-HKNKBN         = "4")
                 AND  (WRK-PARA-RECEKBN      = "5")
                 AND  (WRK-PARA-JIBAIPRTKBN  = "1")
                   EVALUATE    ID1
                     WHEN       1
                     WHEN       2
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(1)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO(1)
                      EVALUATE  ID1
                        WHEN     1
                          MOVE  "£±"   TO  RECE82-SHOKUJISYUBETU(1)
                        WHEN     2
                          MOVE  "£²"   TO  RECE82-SHOKUJISYUBETU(1)
                      END-EVALUATE
                     WHEN       3
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(2)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO(2)
                      MOVE  "ÆÃÊÌ" TO  RECE82-SHOKUJISYUBETU(2)
                     WHEN       4
                     WHEN       8
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(3)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO(3)
                      MOVE  "¿©Æ²" TO  RECE82-SHOKUJISYUBETU(3)
                     WHEN       9
                     WHEN      10
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(4)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO(4)
                      MOVE  "´Ä¶­" TO  RECE82-SHOKUJISYUBETU(4)
                     WHEN       5
                     WHEN       6
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU-SEI(1)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO-SEI(1)
                      EVALUATE  ID1
                        WHEN     5
                          MOVE  "£±"   TO  RECE82-SHOKUJISYUBETU-KHNSEI
                        WHEN     6
                          MOVE  "£²"   TO  RECE82-SHOKUJISYUBETU-KHNSEI
                      END-EVALUATE
                     WHEN       7
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU-SEI(2)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO-SEI(2)
                   END-EVALUATE
                 ELSE
                   EVALUATE    ID1
                     WHEN       1
                     WHEN       5
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(1)
                      IF   FLG-KHNRYO-SKJ   =   ZERO
                        IF   RECE82-SHOKUJIKHNRYO(1)     =  ZERO
                          MOVE  SYU-SHOKUJI(ID1)
                                     TO  RECE82-SHOKUJIKHNRYO(1)
                        ELSE
                          IF RECE82-SHOKUJIKHNRYO(1) NOT =
                                                SYU-SHOKUJI(ID1)
                            MOVE   ZERO  TO  RECE82-SHOKUJIKHNRYO(1)
                            MOVE   1     TO  FLG-KHNRYO-SKJ
                          END-IF
                        END-IF
                      END-IF
                      MOVE  "£±"   TO  RECE82-SHOKUJISYUBETU(1)
                     WHEN       2
                     WHEN       6
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(1)
                      IF   FLG-KHNRYO-SKJ   =   ZERO
                        IF   RECE82-SHOKUJIKHNRYO(1)     =  ZERO
                          MOVE  SYU-SHOKUJI(ID1)
                                     TO  RECE82-SHOKUJIKHNRYO(1)
                        ELSE
                          IF RECE82-SHOKUJIKHNRYO(1) NOT =
                                                SYU-SHOKUJI(ID1)
                            MOVE   ZERO  TO  RECE82-SHOKUJIKHNRYO(1)
                            MOVE   1     TO  FLG-KHNRYO-SKJ
                          END-IF
                        END-IF
                      END-IF
                      MOVE  "£²"   TO  RECE82-SHOKUJISYUBETU(1)
                     WHEN       3
                     WHEN       7
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(2)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO(2)
                      MOVE  "ÆÃÊÌ" TO  RECE82-SHOKUJISYUBETU(2)
                     WHEN       4
                     WHEN       8
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(3)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO(3)
                      MOVE  "¿©Æ²" TO  RECE82-SHOKUJISYUBETU(3)
                     WHEN       9
                     WHEN      10
                      ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(4)
                      MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO(4)
                      MOVE  "´Ä¶­" TO  RECE82-SHOKUJISYUBETU(4)
                   END-EVALUATE
                 END-IF
               END-IF
      *
             END-PERFORM
      *
           ELSE
      *
            IF   RECE82-SRYYM  >= "200604"
      *
             PERFORM  VARYING  ID1    FROM    1   BY  1
                       UNTIL   ID1      >     4
      *
               IF  SYU-SHOKUJI(ID1)   =   ZERO
                   CONTINUE
               ELSE
                   ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU(ID1)
                   MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO(ID1)
               END-IF
      *
             END-PERFORM
      *
            ELSE
      *
             PERFORM  VARYING  ID1    FROM    1   BY  1
                       UNTIL   ID1      >     5
      *
               IF  SYU-SHOKUJI(ID1)   =   ZERO
                 CONTINUE
               ELSE
                 MOVE     ZERO     TO    FLG-ARI
                 PERFORM  VARYING  ID2    FROM    1   BY  1
                           UNTIL   ID2                       > 2
                             OR    RECE82-SHOKUJIKHNRYO(ID2) = ZERO
                             OR    FLG-ARI                   = 1
                     IF  SYU-SHOKUJI(ID1) = RECE82-SHOKUJIKHNRYO(ID2)
                           ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                          RECE82-SHOKUJIKHNNISSU(ID2)
                           MOVE  1   TO   FLG-ARI
                     END-IF
                 END-PERFORM
                 IF  FLG-ARI  =  ZERO
                 AND ID2     <=   2
                     ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                          RECE82-SHOKUJIKHNNISSU(ID2)
                     MOVE  SYU-SHOKUJI(ID1)        TO
                                          RECE82-SHOKUJIKHNRYO(ID2)
                 END-IF
      *
                 IF  FLG-ARI  =  ZERO
                 AND ID2      >   2
                   MOVE     ZERO     TO    FLG-ARI2
                   PERFORM  VARYING  ID4    FROM    1   BY  1
                             UNTIL ID4                           > 3
                               OR  RECE82-SHOKUJIKHNRYO-ETC(ID4) = ZERO
                               OR  FLG-ARI2                      = 1
                       IF  SYU-SHOKUJI(ID1)   =
                                       RECE82-SHOKUJIKHNRYO-ETC(ID4)
                             ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU-ETC(ID4)
                             MOVE  1   TO   FLG-ARI2
                       END-IF
                   END-PERFORM
                   IF  FLG-ARI2  =  ZERO
                   AND ID4      <=   3
                       ADD   SYU-SHOKUJI-NISSU(ID1)  TO
                                       RECE82-SHOKUJIKHNNISSU-ETC(ID4)
                       MOVE  SYU-SHOKUJI(ID1)        TO
                                       RECE82-SHOKUJIKHNRYO-ETC(ID4)
                   END-IF
                 END-IF
               END-IF
      *
             END-PERFORM
      *
             IF      SYU-SHOKUJI(6)  NOT =  ZERO
                   ADD   SYU-SHOKUJI-NISSU(6)  TO
                                       RECE82-SHOKUJIKHNNISSU(3)
                   MOVE  SYU-SHOKUJI(6)        TO
                                       RECE82-SHOKUJIKHNRYO(3)
             END-IF
             IF      SYU-SHOKUJI(7)  NOT =  ZERO
                   ADD   SYU-SHOKUJI-NISSU(7)  TO
                                       RECE82-SHOKUJIKHNNISSU(4)
                   MOVE  SYU-SHOKUJI(7)        TO
                                       RECE82-SHOKUJIKHNRYO(4)
             END-IF
      *
            END-IF
      *
           END-IF
      *
           IF      SYU-SYUSKJ-RYOYOHI  NOT =  ZERO
                   ADD   SYU-SYUSKJ-NISSU     TO  
                                       RECE82-SHOKUJITOTALDAY
                   ADD   SYU-SYUSKJ-RYOYOHI   TO
                                       RECE82-SHOKUJITOTALKIN
           END-IF
      *
           .
       2601-RECE82-FTN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Â¾ÊÝ¸±¤Ë¤ÆÆþ±¡ÎÁ¤ò»»Äê¤·¤Æ¤¤¤ë¾ì¹ç¤ÎÊÝ¸±·èÄê
      *****************************************************************
       26011-NYUINRYOFLG-SET-SEC               SECTION.
      *
           MOVE     SPACE    TO    RECE82-NYUINRYOFLG
      *
           PERFORM  VARYING  IDY    FROM    1   BY  1
                            UNTIL   IDY     >       31
      *
             IF      SYU-DAY(IDY)    =    "2"
      *
                IF         WRK-DAY5(IDY)      NOT =  ZERO
                AND        WRK-DAY1(IDY)      NOT =  ZERO
                   PERFORM   260111-NYUINRYOFLG-HENSYU-SEC
                ELSE
                   IF      WRK-DAY5(IDY)              NOT =  ZERO
                   AND     RECE82-NYUINDAY-KBN(IDY)   NOT =  "1"
                     EVALUATE  SYS-200501-COMMENT-NYUINRYO-KSIKBN
                       WHEN     "0"
                          MOVE   SPACE    TO   RECE82-NYUINRYOFLG
                       WHEN     "1"
                          MOVE    "1"     TO   RECE82-NYUINRYOFLG
                       WHEN     "2"
                          MOVE    "6"     TO   RECE82-NYUINRYOFLG
                       WHEN     "3"
                          PERFORM   260111-NYUINRYOFLG-HENSYU-SEC
                     END-EVALUATE
                   END-IF
                   IF      WRK-DAY5(IDY)              NOT =  ZERO
                   AND     RECE82-NYUINDAY-KBN(IDY)       =  "1"
                   AND     RECE82-SRYYM                  >=  "200904"
                          PERFORM   260111-NYUINRYOFLG-HENSYU-SEC
                   END-IF
                END-IF
      *
                MOVE      1       TO     FLG-NYUINRYO
                MOVE     31       TO     IDY
      *
             END-IF
      *
           END-PERFORM
      *
           .
       26011-NYUINRYOFLG-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÊÔ½¸
      *****************************************************************
       260111-NYUINRYOFLG-HENSYU-SEC               SECTION.
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    RECE82-HOSPNUM  TO  COMB-HOSPNUM
           MOVE    RECE82-PTID     TO  COMB-PTID
           MOVE    WRK-DAY5(IDY)   TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
           PERFORM 900-HKNCOMBI-INV-SEC
           IF      FLG-HKNCOMBI    =   ZERO
              IF   COMB-HKNNUM(1:2)  =  "98"
              OR   COMB-HKNNUM       =  "971"
              OR   COMB-HKNNUM       =  "973"
              OR   COMB-HKNNUM       =  "975"
                IF   COMB-HKNNUM(1:2)  =  "98"
                   MOVE    "1"   TO   RECE82-NYUINRYOFLG
                END-IF
                IF   COMB-HKNNUM       =  "971"
                   IF   RECE82-HKNNUM  =  "971"
                     CONTINUE
                   ELSE
                     MOVE   "2"  TO   RECE82-NYUINRYOFLG
                   END-IF
                END-IF
                IF   COMB-HKNNUM       =  "973"
                   IF   RECE82-HKNNUM  =  "973"
                     CONTINUE
                   ELSE
                     MOVE   "3"  TO   RECE82-NYUINRYOFLG
                   END-IF
                END-IF
                IF   COMB-HKNNUM       =  "975"
                   MOVE    "7"   TO   RECE82-NYUINRYOFLG
                END-IF
              ELSE
                EVALUATE  COMB-HKNNUM
                  WHEN     "060"
                  WHEN     "067"
                  WHEN     "068"
                  WHEN     "069"
                    MOVE   "4"   TO   RECE82-NYUINRYOFLG
                  WHEN     "039"
                  WHEN     "040"
                    MOVE   "8"   TO   RECE82-NYUINRYOFLG
                  WHEN      OTHER
                    MOVE   "5"   TO   RECE82-NYUINRYOFLG
                    IF   COMB-HKNNUM(1:2)  =  "90" OR "91"
                     MOVE SPACE  TO   RECE82-NYUINRYOFLG
                    END-IF
                END-EVALUATE
              END-IF
           END-IF
      *
           .
       260111-NYUINRYOFLG-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿©»öÎ¬¾ÎÊÔ½¸
      *****************************************************************
       2602-SKJ-RAYKU-HENSYU-SEC               SECTION.
      *
           PERFORM  VARYING  ID1    FROM    1   BY  1
                     UNTIL   ID1      >     4
      *
               IF  RECE82-SHOKUJIKHNNISSU(ID1)   =   ZERO
                   CONTINUE
               ELSE
                   EVALUATE   ID1
                     WHEN      1
                       MOVE   "£±"    TO   RECE82-SHOKUJISYUBETU(1)
                     WHEN      2
                       MOVE   "£²"    TO   RECE82-SHOKUJISYUBETU(1)
                     WHEN      3
                       MOVE   "ÆÃÊÌ"  TO   RECE82-SHOKUJISYUBETU(2)
                     WHEN      4
                       MOVE   "¿©Æ²"  TO   RECE82-SHOKUJISYUBETU(3)
                   END-EVALUATE
               END-IF
      *
           END-PERFORM
      *
           IF    RECE82-SHOKUJIKHNNISSU(1)      =  ZERO
           AND   RECE82-SHOKUJIKHNNISSU(2)  NOT =  ZERO
               MOVE    RECE82-SHOKUJIKHNNISSU(2)
                              TO  RECE82-SHOKUJIKHNNISSU(1)
               MOVE    RECE82-SHOKUJIKHNRYO(2)
                              TO  RECE82-SHOKUJIKHNRYO(1)
      *
               MOVE    ZERO   TO  RECE82-SHOKUJIKHNNISSU(2)
                                  RECE82-SHOKUJIKHNRYO(2)
           END-IF
      *
           IF      RECE82-SHOKUJIKHNNISSU(2)      =  ZERO
             IF    RECE82-SHOKUJIKHNNISSU(3)  NOT =  ZERO
               MOVE    RECE82-SHOKUJIKHNNISSU(3)
                              TO  RECE82-SHOKUJIKHNNISSU(2)
               MOVE    RECE82-SHOKUJIKHNRYO(3)
                              TO  RECE82-SHOKUJIKHNRYO(2)
               MOVE    ZERO   TO  RECE82-SHOKUJIKHNNISSU(3)
                                  RECE82-SHOKUJIKHNRYO(3)
             END-IF
             IF    RECE82-SHOKUJIKHNNISSU(4)  NOT =  ZERO
               MOVE    RECE82-SHOKUJIKHNNISSU(4)
                              TO  RECE82-SHOKUJIKHNNISSU(3)
               MOVE    RECE82-SHOKUJIKHNRYO(4)
                              TO  RECE82-SHOKUJIKHNRYO(3)
               MOVE    ZERO   TO  RECE82-SHOKUJIKHNNISSU(4)
                                  RECE82-SHOKUJIKHNRYO(4)
             END-IF
           END-IF
      *
           .
       2602-SKJ-RAYKU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ì¥»¥×¥ÈÃæ´Ö¥Õ¥¡¥¤¥ë¤Î´ðËÜÅÀ¿ôÉôÊ¬¤Î¥¯¥ê¥¢½èÍý¡Ê¼«ÇåÀÕ¡Ë
      *****************************************************************
       270-KHNTEN-JI-CLEAR-SEC               SECTION.
      *
           IF        FLG-KHNTEN7-JI    =    1
               MOVE    ZERO             TO   RECE82-KHNTEN(7)
           END-IF
           IF        FLG-KHNTEN8-JI    =    1
               MOVE    ZERO             TO   RECE82-KHNTEN(8)
           END-IF
           IF        FLG-KHNTEN9-JI    =    1
               MOVE    ZERO             TO   RECE82-KHNTEN(9)
           END-IF
           .
       270-KHNTEN-JI-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    Êñ³çÅÀ¿ô¤Î¼£¸³¸ºÅÀÊ¬ÊÔ½¸
      *****************************************************************
       281-GENTEN-HENSYU-SEC      SECTION.
      *
           IF   RECE82-TENSU(2)   >=  WRK-RECE82G-TENSU(2)
             COMPUTE  RECE82-TENSU(2)   =
                      RECE82-TENSU(2)   -  WRK-RECE82G-TENSU(2)
           ELSE
             MOVE     ZERO    TO   RECE82-TENSU(2)
           END-IF
           IF   RECE82-TENSU(3)   >=  WRK-RECE82G-TENSU(3)
             COMPUTE  RECE82-TENSU(3)   =
                      RECE82-TENSU(3)   -  WRK-RECE82G-TENSU(3)
           ELSE
             MOVE     ZERO    TO   RECE82-TENSU(3)
           END-IF
           IF   RECE82-TENSU(9)   >=  WRK-RECE82G-TENSU(9)
             COMPUTE  RECE82-TENSU(9)   =
                      RECE82-TENSU(9)   -  WRK-RECE82G-TENSU(9)
           ELSE
             MOVE     ZERO    TO   RECE82-TENSU(9)
           END-IF
           IF   RECE82-TENSU(14)  >=  WRK-RECE82G-TENSU(14)
             COMPUTE  RECE82-TENSU(14)  =
                      RECE82-TENSU(14)  -  WRK-RECE82G-TENSU(14)
           ELSE
             MOVE     ZERO    TO   RECE82-TENSU(14)
           END-IF
           IF   RECE82-TENSU(22)  >=  WRK-RECE82G-TENSU(22)
             COMPUTE  RECE82-TENSU(22)  =
                      RECE82-TENSU(22)  -  WRK-RECE82G-TENSU(22)
           ELSE
             MOVE     ZERO    TO   RECE82-TENSU(22)
           END-IF
           IF   RECE82-TENSU(27)  >=  WRK-RECE82G-TENSU(27)
             COMPUTE  RECE82-TENSU(27)  =
                      RECE82-TENSU(27)  -  WRK-RECE82G-TENSU(27)
           ELSE
             MOVE     ZERO    TO   RECE82-TENSU(27)
           END-IF
           IF   RECE82-TENSU(28)  >=  WRK-RECE82G-TENSU(28)
             COMPUTE  RECE82-TENSU(28)  =
                      RECE82-TENSU(28)  -  WRK-RECE82G-TENSU(28)
           ELSE
             MOVE     ZERO    TO   RECE82-TENSU(28)
           END-IF
      *
           .
       281-GENTEN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿¹¹¿·½èÍý¡Ê£Æ£°£°£²¡Ë
      *****************************************************************
       280-RECE82-REWRITE-SEC                SECTION.
      *
           IF   RECE82-HKNKBN     NOT  =   "4"
             COMPUTE  RECE82-KINGAKU2-SYO  =  RECE82-KINGAKU2-SYO
                                           +  WRK-ZAITENNASHI
           END-IF
      *
           IF   RECE82-HKNKBN     =   "4"                               
      *
             EVALUATE  WRK-PARA-RECEKBN
              WHEN      "4"
      ********¡Ê¿·ÍÍ¼°¡Ë
      *
               COMPUTE   RECE82-TENSU(3)      =  RECE82-TENSU(1)        
                                              +  RECE82-TENSU(2)        
      *
               COMPUTE   RECE82-TENSU(10)     =  RECE82-TENSU(7)       
                                              +  RECE82-TENSU(8)       
                                              +  RECE82-TENSU(9)       
      *
               COMPUTE   RECE82-TENSU-YAK(11) =  RECE82-TENSU-YAK(4)    
                                              +  RECE82-TENSU-YAK(5)   
                                              +  RECE82-TENSU-YAK(6)   
      *
               MOVE      ZERO              TO    RECE82-KINGAKU2-SYO
                                                 RECE82-TOTALKIN2
                                                 RECE82-KINGAKU3-SYO
      *
               COMPUTE   RECE82-KINGAKU2-SYO  =  RECE82-KINGAKU2(1)
                                              +  RECE82-KINGAKU2(2)
                                              +  RECE82-KINGAKU2(3)
                                              +  RECE82-KINGAKU2(4)
                                              +  RECE82-KINGAKU2(5)
      *
               COMPUTE   WRK-KSNRATE  =  RECE82-KSNRATE  /  100
               COMPUTE   RECE82-TOTALKIN2     =
                        (RECE82-KINGAKU2-SYO  *  WRK-KSNRATE)  +  0.5
      *
               COMPUTE   RECE82-KINGAKU3-SYO  =  RECE82-KINGAKU3(1)
                                              +  RECE82-KINGAKU3(2)
                                              +  RECE82-KINGAKU3(3)
                                              +  RECE82-KINGAKU3(4)
                                              +  RECE82-KINGAKU3(5)
      *
               COMPUTE   RECE82-TOTALKIN-ETC      =
                         RECE82-KINGAKU3-SYO      +
                         RECE82-SHOKUJITOTALKIN   +
                         RECE82-SINDAN-KIN        +
                         RECE82-MEISAI-KIN
      * 
              WHEN      "5"
      ********¡Ê½¾ÍèÍÍ¼°¡Ë
      *
               MOVE      ZERO              TO    RECE82-TOTALTEN
                                                 RECE82-TOTALTEN-YAK
                                                 RECE82-KINGAKU2-SYO
                                                 RECE82-KINGAKU3-SYO
                                                 RECE82-TOTALKIN
                                                 RECE82-TOTALKIN-YAK
                                                 RECE82-TOTALKIN2
                                                 RECE82-TOTALKIN-ETC
      *
               COMPUTE   RECE82-TENSU(3)      =  RECE82-TENSU(1)
                                              +  RECE82-TENSU(2)
      *
               COMPUTE   RECE82-TENSU(10)     =  RECE82-TENSU(4)
                                              +  RECE82-TENSU(5)
                                              +  RECE82-TENSU(6)
                                              +  RECE82-TENSU(7)
                                              +  RECE82-TENSU(8)
                                              +  RECE82-TENSU(9)
      *
               COMPUTE   RECE82-TENSU(14)     =  RECE82-TENSU(11)
                                              +  RECE82-TENSU(12)
                                              +  RECE82-TENSU(13)
      *
               COMPUTE   RECE82-TENSU(17)     =  RECE82-TENSU(15)
                                              +  RECE82-TENSU(16)
      *
               COMPUTE   RECE82-TENSU(20)     =  RECE82-TENSU(18)
                                              +  RECE82-TENSU(19)
      *
               COMPUTE   RECE82-TENSU(23)     =  RECE82-TENSU(21)
                                              +  RECE82-TENSU(22)
      *
               COMPUTE   RECE82-TENSU(26)     =  RECE82-TENSU(24)
                                              +  RECE82-TENSU(25)
      *
               COMPUTE   RECE82-TENSU(29)     =  RECE82-TENSU(27)
                                              +  RECE82-TENSU(28)
      *
      ***      COMPUTE   RECE82-TOTALTEN      =  RECE82-NYUTOTALTEN
      ***                                     +  RECE82-TENSU(3)
      ***                                     +  RECE82-TENSU(10)
      ***                                     +  RECE82-TENSU(14)
      ***                                     +  RECE82-TENSU(17)
      ***                                     +  RECE82-TENSU(20)
      ***                                     +  RECE82-TENSU(23)
      ***                                     +  RECE82-TENSU(26)
      ***                                     +  RECE82-TENSU(29)
      *
               MOVE     ZERO              TO   WRK-SYUTEN-JI
               MOVE     ZERO              TO   WRK-YKZTEN-JI
               PERFORM    VARYING    ID3    FROM    1   BY   1
                           UNTIL     ID3     >     31
                  COMPUTE  WRK-SYUTEN-JI  =  WRK-SYUTEN-JI
                                          +  RECE82-SYUTEN-JI(ID3)
                  COMPUTE  WRK-YKZTEN-JI  =  WRK-YKZTEN-JI
                                          +  RECE82-YKZTEN-JI(ID3)
               END-PERFORM
               COMPUTE   RECE82-TOTALTEN  =  RECE82-NYUTOTALTEN
                                          +  WRK-SYUTEN-JI
                                          +  WRK-YKZTEN-JI
      *
               IF   TBL-SYUTEN-JI(2)   >=  WRK-RECE82G-TENSU(2)
                 COMPUTE  TBL-SYUTEN-JI(2)  =
                          TBL-SYUTEN-JI(2)  -  WRK-RECE82G-TENSU(2)
               ELSE
                 MOVE     ZERO    TO   TBL-SYUTEN-JI(2)
               END-IF
               IF   TBL-SYUTEN-JI(27)  >=  WRK-RECE82G-TENSU(27)
                 COMPUTE  TBL-SYUTEN-JI(27) =
                          TBL-SYUTEN-JI(27) -  WRK-RECE82G-TENSU(27)
               ELSE
                 MOVE     ZERO    TO   TBL-SYUTEN-JI(27)
               END-IF
               IF   TBL-SYUTEN-JI(28)  >=  WRK-RECE82G-TENSU(28)
                 COMPUTE  TBL-SYUTEN-JI(28) =
                          TBL-SYUTEN-JI(28) -  WRK-RECE82G-TENSU(28)
               ELSE
                 MOVE     ZERO    TO   TBL-SYUTEN-JI(28)
               END-IF
      *
               EVALUATE   RECE82-JIBAIKBN
      *
                WHEN       "1"
      *
                  COMPUTE   WRK-KSNRATE  =  RECE82-KSNRATE  /  100
      *
                  COMPUTE  RECE82-TOTALKIN-JYU   =
                           RECE82-TOTALKIN-JYU   +
                     ((WRK-SYUTEN-JI +  RECE82-NYUTOTALTEN)
                                     *  RECE82-TANKA  *  WRK-KSNRATE)
                                                      +  0.5
                  COMPUTE  RECE82-TOTALKIN-JYU   =
                           RECE82-TOTALKIN-JYU   +
                     (WRK-YKZTEN-JI  *  RECE82-TANKA) +  0.5
      *
                  COMPUTE  RECE82-SONOTAKIN-JYU  =
                           RECE82-SONOTAKIN-JYU  +
                     (WRK-KINGAKU-JI *  WRK-KSNRATE)  +  0.5
      ***
                  PERFORM    VARYING    ID3    FROM    1   BY   1
                              UNTIL     ID3     >     29
                    IF   ID3  =   3  OR  10  OR  14  OR  17  OR
                                 20  OR  23  OR  26  OR  29
                       CONTINUE
                    ELSE
                       COMPUTE  RECE82-KINGAKU-JYU(ID3) =
                                RECE82-KINGAKU-JYU(ID3) +
                       (TBL-SYUTEN-JI(ID3) * RECE82-TANKA * WRK-KSNRATE)
                                                          + 0.5
                       COMPUTE  RECE82-KINGAKU-JYU(ID3) =
                                RECE82-KINGAKU-JYU(ID3) +
                       (TBL-YKZTEN-JI(ID3) * RECE82-TANKA)
                                                          + 0.5
                    END-IF
                  END-PERFORM
      ***
                  PERFORM    VARYING    ID3    FROM    1   BY   1
                              UNTIL     ID3     >     10
      *
                    COMPUTE  RECE82-NYUKHNKIN-JYU(ID3) =
                             RECE82-NYUKHNKIN-JYU(ID3) +
                    (RECE82-NYUKHNKEI(ID3) * RECE82-TANKA * WRK-KSNRATE)
                                                          + 0.5
                    COMPUTE  RECE82-NYUTOTALKIN-JYU    =
                             RECE82-NYUTOTALKIN-JYU    +
                    (RECE82-NYUKHNKEI(ID3) * RECE82-TANKA * WRK-KSNRATE)
                                                          + 0.5
      *
                    COMPUTE  RECE82-NYUTOKKIN-JYU(ID3) =
                             RECE82-NYUTOKKIN-JYU(ID3) +
                    (RECE82-NYUTOKKEI(ID3) * RECE82-TANKA * WRK-KSNRATE)
                                                          + 0.5
                    COMPUTE  RECE82-NYUTOTALKIN-JYU    =
                             RECE82-NYUTOTALKIN-JYU    +
                    (RECE82-NYUTOKKEI(ID3) * RECE82-TANKA * WRK-KSNRATE)
                                                          + 0.5
      *
                  END-PERFORM
      *
                  MOVE   RECE82-NYUKHNKEI2  TO  WRK-KEISAN2
                  COMPUTE  RECE82-NYUKHNKIN2-JYU  =
                           RECE82-NYUKHNKIN2-JYU  +
                           (WRK-KEISAN2 * RECE82-TANKA * WRK-KSNRATE)
                                                       + 0.5
                  MOVE   RECE82-NYUTOKKEI2  TO  WRK-KEISAN2
                  COMPUTE  RECE82-NYUTOKKIN2-JYU  =
                           RECE82-NYUTOKKIN2-JYU  +
                           (WRK-KEISAN2 * RECE82-TANKA * WRK-KSNRATE)
                                                       + 0.5
                  IF     RECE82-NYUKHNKEI2  <   ZERO
                      COMPUTE  RECE82-NYUKHNKIN2-JYU   =
                               RECE82-NYUKHNKIN2-JYU   *  -1
                  END-IF
                  IF     RECE82-NYUTOKKEI2  <   ZERO
                      COMPUTE  RECE82-NYUTOKKIN2-JYU   =
                               RECE82-NYUTOKKIN2-JYU   *  -1
                  END-IF
      *
                  COMPUTE  RECE82-NYUTOTALKIN-JYU    =
                           RECE82-NYUTOTALKIN-JYU    +
                           RECE82-NYUKHNKIN2-JYU
                  COMPUTE  RECE82-NYUTOTALKIN-JYU    =
                           RECE82-NYUTOTALKIN-JYU    +
                           RECE82-NYUTOKKIN2-JYU
      *
                WHEN       "2"
      *
                  COMPUTE  RECE82-TOTALKIN-JYU   =
                           RECE82-TOTALKIN-JYU   +
                     ((WRK-SYUTEN-JI +  RECE82-NYUTOTALTEN)
                                     *  RECE82-SYUTENTANKA )
                  COMPUTE  RECE82-TOTALKIN-JYU   =
                           RECE82-TOTALKIN-JYU   +
                     ( WRK-YKZTEN-JI *  RECE82-YKZTENTANKA )
      ***
                  PERFORM    VARYING    ID3    FROM    1   BY   1
                              UNTIL     ID3     >     29
                    IF   ID3  =   3  OR  10  OR  14  OR  17  OR
                                 20  OR  23  OR  26  OR  29
                       CONTINUE
                    ELSE
                       COMPUTE  RECE82-KINGAKU-JYU(ID3) =
                                RECE82-KINGAKU-JYU(ID3) +
                         (TBL-SYUTEN-JI(ID3) * RECE82-SYUTENTANKA)
                       COMPUTE  RECE82-KINGAKU-JYU(ID3) =
                                RECE82-KINGAKU-JYU(ID3) +
                         (TBL-YKZTEN-JI(ID3) * RECE82-YKZTENTANKA)
                    END-IF
                  END-PERFORM
      ***
                  PERFORM    VARYING    ID3    FROM    1   BY   1
                              UNTIL     ID3     >     10
      *
                     COMPUTE  RECE82-NYUKHNKIN-JYU(ID3) =
                              RECE82-NYUKHNKIN-JYU(ID3) +
                       (RECE82-NYUKHNKEI(ID3) * RECE82-SYUTENTANKA)
                     COMPUTE  RECE82-NYUTOTALKIN-JYU    =
                              RECE82-NYUTOTALKIN-JYU    +
                       (RECE82-NYUKHNKEI(ID3) * RECE82-SYUTENTANKA)
      *
                     COMPUTE  RECE82-NYUTOKKIN-JYU(ID3) =
                              RECE82-NYUTOKKIN-JYU(ID3) +
                       (RECE82-NYUTOKKEI(ID3) * RECE82-SYUTENTANKA)
                     COMPUTE  RECE82-NYUTOTALKIN-JYU    =
                              RECE82-NYUTOTALKIN-JYU    +
                       (RECE82-NYUTOKKEI(ID3) * RECE82-SYUTENTANKA)
      *
                  END-PERFORM
      *
                  MOVE   RECE82-NYUKHNKEI2  TO  WRK-KEISAN2
                  COMPUTE  RECE82-NYUKHNKIN2-JYU  =
                           RECE82-NYUKHNKIN2-JYU  +
                           (WRK-KEISAN2 * RECE82-SYUTENTANKA)
                  MOVE   RECE82-NYUTOKKEI2  TO  WRK-KEISAN2
                  COMPUTE  RECE82-NYUTOKKIN2-JYU  =
                           RECE82-NYUTOKKIN2-JYU  +
                           (WRK-KEISAN2 * RECE82-SYUTENTANKA)
                  IF     RECE82-NYUKHNKEI2  <   ZERO
                      COMPUTE  RECE82-NYUKHNKIN2-JYU   =
                               RECE82-NYUKHNKIN2-JYU   *  -1
                  END-IF
                  IF     RECE82-NYUTOKKEI2  <   ZERO
                      COMPUTE  RECE82-NYUTOKKIN2-JYU   =
                               RECE82-NYUTOKKIN2-JYU   *  -1
                  END-IF
      *
                  COMPUTE  RECE82-NYUTOTALKIN-JYU    =
                           RECE82-NYUTOTALKIN-JYU    +
                           RECE82-NYUKHNKIN2-JYU
                  COMPUTE  RECE82-NYUTOTALKIN-JYU    =
                           RECE82-NYUTOTALKIN-JYU    +
                           RECE82-NYUTOKKIN2-JYU
      *
               END-EVALUATE
      *
               COMPUTE   RECE82-KINGAKU-JYU(3)  =
                         RECE82-KINGAKU-JYU(1)  +
                         RECE82-KINGAKU-JYU(2)   
      *
               COMPUTE   RECE82-KINGAKU-JYU(10)  =
                         RECE82-KINGAKU-JYU(4)   +
                         RECE82-KINGAKU-JYU(5)   +
                         RECE82-KINGAKU-JYU(6)   +
                         RECE82-KINGAKU-JYU(7)   +
                         RECE82-KINGAKU-JYU(8)   +
                         RECE82-KINGAKU-JYU(9)    
      *
               COMPUTE   RECE82-KINGAKU-JYU(14)  =
                         RECE82-KINGAKU-JYU(11)  +
                         RECE82-KINGAKU-JYU(12)  +
                         RECE82-KINGAKU-JYU(13)   
      *
               COMPUTE   RECE82-KINGAKU-JYU(17)  =
                         RECE82-KINGAKU-JYU(15)  +
                         RECE82-KINGAKU-JYU(16)   
      *
               COMPUTE   RECE82-KINGAKU-JYU(20)  =
                         RECE82-KINGAKU-JYU(18)  +
                         RECE82-KINGAKU-JYU(19)   
      *
               COMPUTE   RECE82-KINGAKU-JYU(23)  =
                         RECE82-KINGAKU-JYU(21)  +
                         RECE82-KINGAKU-JYU(22)   
      *
               COMPUTE   RECE82-KINGAKU-JYU(26)  =
                         RECE82-KINGAKU-JYU(24)  +
                         RECE82-KINGAKU-JYU(25)   
      *
               COMPUTE   RECE82-KINGAKU-JYU(29)  =
                         RECE82-KINGAKU-JYU(27)  +
                         RECE82-KINGAKU-JYU(28)   
      *
             END-EVALUATE
      *
           END-IF                                                       
      *
           REWRITE   RECE82-REC
      *
           ADD     1                   TO  CNT-OUT3
      *
           .
       280-RECE82-REWRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿½ÐÎÏ½èÍý¡Ê£Æ£°£°£´¡Ë
      *****************************************************************
       900-RECE84-WRITE-SEC                SECTION.
      *
           MOVE     RECE84-REC      TO  RECE8A-REC
      *
           COMPUTE  RECE84-ZAITEN      =  RECE84-ZAITEN     -  
                                          WRK-TEN
           COMPUTE  RECE84-MEISAISU    =  RECE84-MEISAISU   -
                                          WRK-MEISAI-CNT
           COMPUTE  RECE84-SYUTEN1     =  RECE84-SYUTEN1    -
                                          WRK-TEN
           COMPUTE  RECE84-SRYCDTOTAL  =  RECE84-SRYCDTOTAL -
                                          WRK-SRYCDTOTAL
           COMPUTE  RECE84-SURYOUTOTAL =  RECE84-SURYOUTOTAL -
                                          WRK-SURYOUTOTAL     
      *
           MOVE     WRK-TEN         TO  RECE8A-ZAITEN
           MOVE     WRK-MEISAI-CNT  TO  RECE8A-MEISAISU
           MOVE     WRK-TEN         TO  RECE8A-SYUTEN1  
           MOVE     WRK-SRYCDTOTAL  TO  RECE8A-SRYCDTOTAL
           MOVE     WRK-SURYOUTOTAL TO  RECE8A-SURYOUTOTAL
      *
           WRITE   RECE84-REC
           ADD     1                TO  CNT-OUT1
      *
           WRITE   RECE8A-REC
           ADD     1                TO  CNT-OUT4
      *
           .
       900-RECE84-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿½ÐÎÏ½èÍý¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       900-RECE85-WRITE-SEC                SECTION.
      *
           MOVE    RECE85-REC          TO   RECE8B-REC      
      *
           WRITE   RECE85-REC
           ADD     1                   TO   CNT-OUT2
      *
           WRITE   RECE8B-REC
           ADD     1                   TO   CNT-OUT5
      *
           .
       900-RECE85-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿½ÐÎÏ½èÍý¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       900-RECE85-REWRITE-SEC              SECTION.                        
      *
           MOVE    ZERO                TO  FLG-REWRITE
      *
           INITIALIZE                      RECE85-REC
           MOVE    RECE84-HOSPNUM      TO  RECE85-HOSPNUM
           MOVE    RECE84-SRYYM        TO  RECE85-SRYYM
           MOVE    RECE84-NYUGAIKBN    TO  RECE85-NYUGAIKBN
           MOVE    RECE84-PTID         TO  RECE85-PTID
           MOVE    RECE84-RECEKA       TO  RECE85-RECEKA
           MOVE    RECE84-HKNID     TO  RECE85-HKNID
           MOVE    RECE84-TEKSTYMD     TO  RECE85-TEKSTYMD
           MOVE    RECE84-ZAINUM       TO  RECE85-ZAINUM
           START   RECE85-FILE
                    KEY IS    >=   RECE85-KEY
                    INVALID
                        MOVE    1         TO  FLG-RECE85-ZAN
                    NOT INVALID
                        MOVE    ZERO      TO  FLG-RECE85-ZAN
                        READ    RECE85-FILE   NEXT
                          AT  END
                            MOVE    1     TO  FLG-RECE85-ZAN
                        END-READ
           END-START
           PERFORM   UNTIL
               (RECE84-HOSPNUM      NOT =  RECE85-HOSPNUM)     OR
               (RECE84-SRYYM        NOT =  RECE85-SRYYM)       OR
               (RECE84-NYUGAIKBN    NOT =  RECE85-NYUGAIKBN)   OR
               (RECE84-PTID         NOT =  RECE85-PTID)        OR
               (RECE84-RECEKA       NOT =  RECE85-RECEKA)      OR
               (RECE84-HKNID        NOT =  RECE85-HKNID)    OR
               (RECE84-TEKSTYMD     NOT =  RECE85-TEKSTYMD)    OR
               (RECE84-ZAINUM       NOT =  RECE85-ZAINUM)      OR
               (FLG-RECE85-ZAN            =      1        )    OR
               (FLG-REWRITE               =      1        )
                   IF    RECE85-RENNUM   =   WRK-RENNUM
                      MOVE    1              TO  FLG-REWRITE
                      PERFORM  VARYING  IDXB  FROM  5   BY  -1
                                UNTIL   IDXB   <    1
                        IF  RECE85-SRYCD(IDXB)  NOT = SPACE
                          MOVE WRK-SRYSURYO3 TO RECE85-SRYSURYO(IDXB)
                          MOVE 1             TO IDXB
                        END-IF
                      END-PERFORM
                      REWRITE   RECE85-REC
                   END-IF
                   READ    RECE85-FILE   NEXT
                     AT  END
                       MOVE    1     TO  FLG-RECE85-ZAN
                   END-READ
           END-PERFORM
      *
           .
       900-RECE85-REWRITE-EXT.
           EXIT.                                                       
      *
      *****************************************************************
      *    ¥Ç¡¼¥¿½ÐÎÏ½èÍý¡Ê£Æ£°£°£µ¡Ë
      *****************************************************************
       900-RECE85-REWRITE2-SEC              SECTION.                        
      *
           MOVE    ZERO                TO  FLG-REWRITE
      *
           INITIALIZE                      RECE85-REC
           MOVE    RECE84-HOSPNUM      TO  RECE85-HOSPNUM
           MOVE    RECE84-SRYYM        TO  RECE85-SRYYM
           MOVE    RECE84-NYUGAIKBN    TO  RECE85-NYUGAIKBN
           MOVE    RECE84-PTID         TO  RECE85-PTID
           MOVE    RECE84-RECEKA       TO  RECE85-RECEKA
           MOVE    RECE84-HKNID     TO  RECE85-HKNID
           MOVE    RECE84-TEKSTYMD     TO  RECE85-TEKSTYMD
           MOVE    RECE84-ZAINUM       TO  RECE85-ZAINUM
           START   RECE85-FILE
                    KEY IS    >=   RECE85-KEY
                    INVALID
                        MOVE    1         TO  FLG-RECE85-ZAN
                    NOT INVALID
                        MOVE    ZERO      TO  FLG-RECE85-ZAN
                        READ    RECE85-FILE   NEXT
                          AT  END
                            MOVE    1     TO  FLG-RECE85-ZAN
                        END-READ
           END-START
           PERFORM   UNTIL
               (RECE84-HOSPNUM      NOT =  RECE85-HOSPNUM)     OR
               (RECE84-SRYYM        NOT =  RECE85-SRYYM)       OR
               (RECE84-NYUGAIKBN    NOT =  RECE85-NYUGAIKBN)   OR
               (RECE84-PTID         NOT =  RECE85-PTID)        OR
               (RECE84-RECEKA       NOT =  RECE85-RECEKA)      OR
               (RECE84-HKNID        NOT =  RECE85-HKNID)    OR
               (RECE84-TEKSTYMD     NOT =  RECE85-TEKSTYMD)    OR
               (RECE84-ZAINUM       NOT =  RECE85-ZAINUM)      OR
               (FLG-RECE85-ZAN            =      1        )    OR
               (FLG-REWRITE               =      1        )
                   IF    RECE85-RENNUM   =   WRK-RENNUM2
                      MOVE    1              TO  FLG-REWRITE
                      PERFORM  VARYING  IDXB  FROM  5   BY  -1
                                UNTIL   IDXB   <    1
                        IF  RECE85-SRYCD(IDXB)  NOT = SPACE
                          MOVE   "1"  TO   RECE85-RECEPTFLG(IDXB)
                          MOVE    1   TO   IDXB
                        END-IF
                      END-PERFORM
                      REWRITE   RECE85-REC
                   END-IF
                   READ    RECE85-FILE   NEXT
                     AT  END
                       MOVE    1     TO  FLG-RECE85-ZAN
                   END-READ
           END-PERFORM
      *
           .
       900-RECE85-REWRITE2-EXT.
           EXIT.                                                       
      *
      *****************************************************************
      *    ½ªÎ»½èÍý
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE82-FILE
                   RECE84-FILE
                   RECE85-FILE
                   RECE8A-FILE
                   RECE8B-FILE
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ¥¹¥Æ¥Ã¥×´ÉÍý½ªÎ»½èÍý
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE82-IN  :" CNT-IN   
           DISPLAY "RECE82-MOD :" CNT-OUT3  
           DISPLAY "RECE84-OUT :" CNT-OUT1 
           DISPLAY "RECE85-OUT :" CNT-OUT2  
           DISPLAY "RECE8A-OUT :" CNT-OUT4 
           DISPLAY "RECE8B-OUT :" CNT-OUT5  
           DISPLAY "*** ORCR0840 END ***"
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
               IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *            ¥¸¥ç¥Ö´ÉÍý½ªÎ»½èÍý
                   MOVE    "JBE"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "9999"          TO  JOB-ERRCD
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
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
      *    ¥¨¥é¡¼¥á¥Ã¥»¡¼¥¸ÊÔ½¸½èÍý
      *****************************************************************
       600-ERRMSG-EDIT-SEC             SECTION.
      *
           MOVE    SPACE               TO  ERR-HEN-ERRMSG
      *
           MOVE    RECE82-PTNUM        TO  ERR-PTNUM
      *
           MOVE    ZERO                TO  WRK-SHO
                                           WRK-AMARI
           MOVE    1                   TO  WRK-LEN
           MOVE    SPACE               TO  WRK-STR
           STRING  ERR-PTNUM           DELIMITED   BY  SPACE
           INTO    WRK-STR
           WITH  POINTER WRK-LEN
           END-STRING
      *
           COMPUTE WRK-LEN =   WRK-LEN -   1
      *
           DIVIDE  2   INTO    WRK-LEN GIVING  WRK-SHO
           REMAINDER WRK-AMARI
           END-DIVIDE
      *
           IF    ( WRK-AMARI   =   ZERO )
              STRING  ERR-ERRMSG          DELIMITED   BY  SPACE
                      "¡Î´µ¼ÔÈÖ¹æ¡§"      DELIMITED   BY  SIZE
                      ERR-PTNUM           DELIMITED   BY  SPACE
                      "¡Ï"                DELIMITED   BY  SIZE
              INTO    ERR-HEN-ERRMSG
              END-STRING
           ELSE
              STRING  ERR-ERRMSG          DELIMITED   BY  SPACE
                      "¡Î´µ¼ÔÈÖ¹æ:"       DELIMITED   BY  SIZE
                      ERR-PTNUM           DELIMITED   BY  SPACE
                      "¡Ï"                DELIMITED   BY  SIZE
              INTO    ERR-HEN-ERRMSG
              END-STRING
           END-IF
      *
           .
       600-ERRMSG-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ãæ´Ö¥Õ¥¡¥¤¥ë£Ò£Å£Á£Ä½èÍý
      *****************************************************************
       900-RECE82-READ-SEC            SECTION.
      *
           READ    RECE82-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-END
               NOT  AT  END
                   ADD     1     TO    CNT-IN
                   ADD     1     TO    CNT-IN-COMMIT
                                       CNT-IN-CANCEL
      *
                   DISPLAY "RECE82 "  " PTNUM=" RECE82-PTNUM
                                      " (PTID=" RECE82-PTID ")"
      *
                   PERFORM 910-SYSKANRI-HENSYU-SEC
      *
           END-READ
           .
       900-RECE82-READ-EXT.
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
      *        ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿¡Ê£±£°£°£±¡Ë¤«¤é¾ÃÈñÀÇÃ¼¿ô¶èÊ¬
      *        ¤Î¼èÆÀ
               MOVE    SPACE      TO  WRK-TAX1001-KBN
      *
               MOVE  ZERO         TO  FLG-SET
               MOVE  RECE82-SRYYM TO  WRK-SYS-SRYYMD(1:6)
               MOVE  "01"         TO  WRK-SYS-SRYYMD(7:2)
               PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                    UNTIL  ( SYSKAN-IDX              >  100  )
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
                  STRING
                  "°åÎÅµ¡´Ø¾ðÊó¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£¿ÇÎÅÇ¯·î¡á"
                                               DELIMITED  BY  SIZE 
                       RECE82-SRYYM            DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
                  END-STRING
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1               TO  FLG-END
                  GO  TO  910-SYSKANRI-HENSYU-EXT
               END-IF
      *
      *        »ÜÀß´ð½à¾ðÊó
               MOVE    ZERO         TO  FLG-SET
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
               MOVE      RECE82-SRYYM   TO  WRK-SRYYM-5000
               PERFORM   120-SYS-5000-CHK-SEC
      *
      *        ¿©»ö¡¦Ì¤¼Â»Ü¸º»»¾ðÊó¡Ê¥·¥¹¥Æ¥à´ÉÍý¥Æ¡¼¥Ö¥ë¤è¤ê¡Ë
               INITIALIZE             WRK-SHOKUJIRYOYO-OCC
                                      WRK-TOKUKANRIKASAN-OCC
      *
               MOVE  ZERO         TO  FLG-SET
               MOVE  RECE82-SRYYM TO  WRK-SYS-SRYYMD(1:6)
               MOVE  "01"         TO  WRK-SYS-SRYYMD(7:2)
               PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                    UNTIL  ( SYSKAN-IDX              >  100  )
                     OR    ( TBL-SYS5000(SYSKAN-IDX) = SPACE )
                     OR    ( FLG-SET                 =   1   )
      *
                  MOVE  TBL-SYS5000(SYSKAN-IDX)  TO  SYS-5000-REC
                  IF   ( SYS-5000-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                  AND  ( WRK-SYS-SRYYMD     <=  SYS-5000-EDYUKYMD )
                     MOVE     1       TO    FLG-SET
                     PERFORM  110-SYSKAN-HENSYU-SEC
                     IF        WRK-SYS5000-COUNT    =     ZERO
                        CONTINUE
                     ELSE
                        PERFORM  130-SYS-5000-GET-SEC
                     END-IF
                  END-IF
      *
               END-PERFORM
               IF     FLG-SET  =  ZERO
                  STRING
              "°åÎÅµ¡´Ø¾ðÊó¡ÝÆþ±¡´ðËÜ¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿¡£¿ÇÎÅÇ¯·î¡á"
                                               DELIMITED  BY  SIZE 
                       RECE82-SRYYM            DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
                  END-STRING
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
      *    ¿ÇÎÅ²ñ·×ÆÉ¹þ
      *****************************************************************
       900-SRYACCT-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SRYACCT
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
               INITIALIZE                      SRYACCT-REC
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
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°ÙÆÉ¹þ
      *****************************************************************
       900-SRYACCTPLUS-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCTPLUS-REC
               MOVE    ZERO                TO  FLG-SRYACCTPLUS
           ELSE
               INITIALIZE                      SRYACCTPLUS-REC
               MOVE    1                   TO  FLG-SRYACCTPLUS
           END-IF
      *
           .
       900-SRYACCTPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       900-NYUINACCT-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-NYUINACCT
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
           ELSE
               MOVE    1                   TO  FLG-NYUINACCT
               INITIALIZE                      NYUINACCT-REC
           END-IF
      *
           .
       900-NYUINACCT-READ-EXT.
           EXIT.
      *
      **************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼ÆÉ¹þ
      **************************************************************
       900-NYUINACT-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-NYUINACT
               MOVE    MCPDATA-REC         TO  NYUINACT-REC
           ELSE
               MOVE    1                   TO  FLG-NYUINACT
               INITIALIZE                      NYUINACT-REC
           END-IF
      *
           .
       900-NYUINACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       900-TENSU-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO          TO    FLG-TENSU
               MOVE    MCPDATA-REC   TO    TNS-TENSU-REC
           ELSE
               MOVE    1             TO    FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Æ¡¼¥Ö¥ë¤Î¸¡º÷
      *****************************************************************
       900-TENSU-TBL-SEC               SECTION.
      *
           INITIALIZE                           TNS-TENSU-REC
grpsys     MOVE    SPA-HOSPNUM          TO      TNS-HOSPNUM
           MOVE    WRK-SRYCD            TO      TNS-SRYCD
           MOVE    WK-TAIHI-SRYYMD      TO      TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC        TO      MCPDATA-REC
           MOVE    "tbl_tensu"          TO      MCP-TABLE
           MOVE    "key"                TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_tensu"      TO      MCP-TABLE
               MOVE    "key"            TO      MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE   MCPDATA-REC   TO      TNS-TENSU-REC
                   MOVE   ZERO          TO      FLG-TENSU
               ELSE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE    1            TO      FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                       TNS-TENSU-REC
               MOVE    1                TO      FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"          TO      MCP-TABLE
           MOVE    "key"                TO      MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-TENSU-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ýÇ¼ÆÉ¹þ
      *****************************************************************
       900-SYUNOU-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYUNOU
               MOVE    MCPDATA-REC         TO  SYUNOU-REC
           ELSE
               MOVE    1                   TO  FLG-SYUNOU
               INITIALIZE                      SYUNOU-REC
           END-IF
      *
           .
       900-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       900-HKNCOMBI-INV-SEC         SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-HKNCOMBI
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   IF      COMB-DELKBN         =   "1"
                       MOVE    1           TO  FLG-HKNCOMBI
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-HKNCOMBI
                   INITIALIZE                  HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                      HKNCOMBI-REC
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-HKNCOMBI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¥³¥á¥ó¥È¥Þ¥¹¥¿ÆÉ¤ß¹þ¤ß
      *****************************************************************
       900-PTCOM-READ-SEC           SECTION.
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
                   MOVE    MCPDATA-REC     TO  PTCOM-REC
                   MOVE    ZERO            TO  FLG-PTCOM
               ELSE
                   MOVE    1               TO  FLG-PTCOM
                   INITIALIZE                  PTCOM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTCOM
               INITIALIZE                      PTCOM-REC
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTCOM-READ-EXT.
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
      *    ¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       900-CLOSE-SEC                  SECTION.
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
